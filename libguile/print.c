/* Copyright (C) 1995-2004, 2006, 2008-2014 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */



#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <errno.h>
#include <iconv.h>
#include <stdio.h>
#include <assert.h>

#include <uniconv.h>
#include <unictype.h>
#include <c-strcase.h>

#include "libguile/_scm.h"
#include "libguile/chars.h"
#include "libguile/continuations.h"
#include "libguile/smob.h"
#include "libguile/control.h"
#include "libguile/eval.h"
#include "libguile/macros.h"
#include "libguile/procprop.h"
#include "libguile/read.h"
#include "libguile/weaks.h"
#include "libguile/programs.h"
#include "libguile/alist.h"
#include "libguile/struct.h"
#include "libguile/ports.h"
#include "libguile/ports-internal.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/srfi-13.h"
#include "libguile/strports.h"
#include "libguile/vectors.h"
#include "libguile/numbers.h"
#include "libguile/vm.h"

#include "libguile/validate.h"
#include "libguile/print.h"

#include "libguile/private-options.h"



/* Character printers.  */

#define PORT_CONVERSION_HANDLER(port)		\
  SCM_PTAB_ENTRY (port)->ilseq_handler

static size_t display_string (const void *, int, size_t, SCM,
			      scm_t_string_failed_conversion_handler);

static int display_character (scm_t_wchar, SCM,
			      scm_t_string_failed_conversion_handler);

static void write_character (scm_t_wchar, SCM, int);

static void write_character_escaped (scm_t_wchar, int, SCM);



/* {Names of immediate symbols}
 * 
 * This table must agree with the declarations in scm.h: {Immediate Symbols}.
 */

/* This table must agree with the list of flags in tags.h.  */
static const char *iflagnames[] =
{
  "#f",
  "#nil",  /* Elisp nil value. Should print from elisp as symbol `nil'. */
  "#<XXX UNUSED LISP FALSE -- DO NOT USE -- SHOULD NEVER BE SEEN XXX>",
  "()",
  "#t",
  "#<XXX UNUSED BOOLEAN 0 -- DO NOT USE -- SHOULD NEVER BE SEEN XXX>",
  "#<XXX UNUSED BOOLEAN 1 -- DO NOT USE -- SHOULD NEVER BE SEEN XXX>",
  "#<XXX UNUSED BOOLEAN 2 -- DO NOT USE -- SHOULD NEVER BE SEEN XXX>",
  "#<unspecified>",
  "#<undefined>",
  "#<eof>",

  /* Unbound slot marker for GOOPS.  For internal use in GOOPS only.  */
  "#<unbound>",
};

SCM_SYMBOL (sym_reader, "reader");

scm_t_option scm_print_opts[] = {
  { SCM_OPTION_SCM, "highlight-prefix", (scm_t_bits)SCM_BOOL_F_BITS,
    "The string to print before highlighted values." },
  { SCM_OPTION_SCM, "highlight-suffix", (scm_t_bits)SCM_BOOL_F_BITS,
    "The string to print after highlighted values." },
  { SCM_OPTION_SCM, "quote-keywordish-symbols", (scm_t_bits)SCM_BOOL_F_BITS,
    "How to print symbols that have a colon as their first or last character. "
    "The value '#f' does not quote the colons; '#t' quotes them; "
    "'reader' quotes them when the reader option 'keywords' is not '#f'." },
  { SCM_OPTION_BOOLEAN, "escape-newlines", 1,
    "Render newlines as \\n when printing using `write'." },
  { SCM_OPTION_BOOLEAN, "r7rs-symbols", 0,
    "Escape symbols using R7RS |...| symbol notation." },
  { SCM_OPTION_BOOLEAN, "datum-labels", 0,
    "Print cyclic data using SRFI-38 datum label notation." },
  { 0 },
};

SCM_DEFINE (scm_print_options, "print-options-interface", 0, 1, 0, 
            (SCM setting),
	    "Option interface for the print options. Instead of using\n"
	    "this procedure directly, use the procedures\n"
	    "@code{print-enable}, @code{print-disable}, @code{print-set!}\n"
	    "and @code{print-options}.")
#define FUNC_NAME s_scm_print_options
{
  SCM ans = scm_options (setting,
			 scm_print_opts,
			 FUNC_NAME);
  return ans;
}
#undef FUNC_NAME


/* {Printing of Scheme Objects}
 */

/* Detection of circular references.
 *
 * Before Guile 2.0.10, references to ancestor objects were kept in a
 * stack, leading to O(depth * N) time complexity.
 *
 * Guile now supports datum label notation as specified in SRFI-38 and
 * R7RS.  We still maintain the ref stack for backward compatibility,
 * but we no longer search it.  Instead, we use a hash table and other
 * data structures in scm_internal_print_state.
 *
 * Printing SRFI-38 datum label notation requires at least two passes,
 * because we cannot know which datum label definitions need to be
 * printed until we've traversed the entire structure.
 *
 * The reference implementation of SRFI-38 does an initial traversal to
 * determine which datum label definitions are needed, and then begins
 * printing.  We don't do that in Guile because of the need to support
 * custom data structures (e.g. SRFI-9 records) with their custom
 * printers.  We'd need such data structures to provide custom
 * pre-traversers as well.
 *
 * Instead, Guile prints compound structures to a private string port,
 * while recording the port byte positions where datum label definitions
 * might be needed.  After we've printed the entire structure to the
 * private string port, we copy it to the actual output port with any
 * needed datum label definitions inserted.
 *
 * Three types of insertions might be needed.  In the simple case, we
 * need only insert "#1=" before a shared datum, but things get more
 * complicated when the shared datum is a pair that is also the CDR
 * of another pair.  Consider the circular list (1 2 3 4 3 4 ...)
 * which will ultimately be written as (1 2 . #1=(3 4 . #1#)).  Before
 * post-processing, the string is (1 2 3 4 . #1#), so we need to insert
 * ". #1=(" in one place, and ")" in another.
 *
 * There's one additional complication.  Print states are part of
 * Guile's public API, and can be accessed by custom printers that are
 * used in the middle of a larger 'write' or 'display'.  It is therefore
 * possible to use an existing print state with a port other than the
 * private string port.  For example, a custom printer might choose to
 * use the existing print state while printing to its own string port,
 * and then later process that string.  In these cases, it is not
 * possible for us to record the byte positions where datum label
 * definitions might later be needed.
 *
 * To handle this case, we support another mechanism: wherever a datum
 * label definition might be needed, we print out a special marker
 * including seldom-used control characters and the 'id'.  The
 * presumption is that this string will later be printed to the private
 * string port.  If needed, we scan for these special markers later and
 * either remove them or change them to the proper SRFI-38 notation.
 *
 * Since this alternate mechanism is not completely reliable, we keep
 * track of which 'id's (if any) were written using these special
 * markers.  If no special markers were printed, then we avoid the scan
 * entirely.
 */

/* The values of these are important, because they are used to
   sort post-insertions at the same byte position.  We need
   CDR_OPEN insertions to come before OTHER insertions.  */
#define POST_INSERT_MODE_CDR_OPEN   1
#define POST_INSERT_MODE_CDR_CLOSE  2
#define POST_INSERT_MODE_OTHER      3

#define SCM_INTERNAL_PRINT_STATE_LAYOUT "srpwpwpwpwpwuwuwuwuwuw"
typedef struct scm_internal_print_state
{
  SCM handle;			/* Struct handle */
  SCM port;                     /* String port which will later have
                                   datum label definitions inserted.  */
  SCM object_ids;               /* Hash table mapping objects to ids
                                   (integers), for all objects that
                                   should be referenced by datum label
                                   if seen.  */
  SCM id_positions;             /* Simple vector indexed by id, which
                                   contains one of three things:
                                    * an integer (byte offset into 'port')
                                      where the datum label definition
                                      should be inserted.
                                    * a pair of two integers (byte
                                      offsets into 'port'), used when
                                      the associated object is a pair
                                      that was first seen as the CDR of
                                      another pair.  In such cases,
                                      inserting a datum label definition
                                      involves changing e.g. (a b c) to
                                      (a . #1=(b c)), so ". #1=(" must
                                      be inserted in one position, and
                                      ")" must be inserted in another
                                      position.  The byte offsets of
                                      these two positions are stored
                                      as a pair.
                                    * #t means that the datum was not
                                      written to 'port', and therefore
                                      markers were inserted in the output
                                      directly.  */
  SCM id_label_nums;            /* Simple vector indexed by id, which
                                   initially contains #f but is later
                                   assigned a datum label (integer)
                                   when the first datum label reference
                                   is needed.  */
  SCM needed_ids;               /* List of ids that require datum label
                                   definitions to be inserted.  */
  unsigned long write_shared_p;    /* 0 means a normal 'write' or 'display'
                                   where datum labels are only used to
                                   prevent infinite output, i.e. they
                                   are used to reference ancestors only.
                                   1 means 'write-shared' (R7RS) or
                                   'write/ss' (SRFI-38).  */
  unsigned long next_num;          /* The datum label that will be assigned
                                   to the next datum label reference
                                   that has not already been assigned a
                                   number.  */
  unsigned long next_id;        /* The id that will be assigned to the
                                   next object that could potentially be
                                   assigned a datum label.  */
  unsigned long markers_p;      /* 1 if any markers were inserted.  */
  unsigned long num_allocated_ids; /* The current size of the 'id_positions'
                                    and 'id_label_nums' vectors.  */
} scm_internal_print_state;

static SCM scm_internal_print_state_vtable;

static SCM internal_print_state_table;
static scm_i_pthread_mutex_t internal_print_state_table_mutex =
  SCM_I_PTHREAD_MUTEX_INITIALIZER;


#define PUSH_REF(pstate, obj)			\
do						\
{						\
  PSTATE_STACK_SET (pstate, pstate->top, obj);	\
  pstate->top++;				\
  if (pstate->top == pstate->ceiling)		\
    grow_ref_stack (pstate);			\
} while(0)

#define ENTER_NESTED_DATA(the_port, pstate, ipstate, obj, label) \
do								\
{								\
  unsigned long id = ipstate->next_id;                          \
  SCM s_id = SCM_I_MAKINUM (id);                                \
  SCM obj_id = scm_hashq_create_handle_x (ipstate->object_ids,  \
                                          (obj), s_id);         \
  if (!scm_is_eq (SCM_CDR (obj_id), s_id))                      \
    goto label;                                                 \
  if (pstate->fancyp                                            \
      && pstate->top - pstate->list_offset >= pstate->level)	\
    {                                                           \
      scm_hashq_remove_x (ipstate->object_ids, (obj));          \
      scm_putc ('#', port);					\
      return;                                                   \
    }                                                           \
  ipstate->next_id++;                                           \
  if (id == ipstate->num_allocated_ids)                         \
    grow_id_vects (ipstate);                                    \
  SCM_SIMPLE_VECTOR_SET (ipstate->id_label_nums, id,            \
                         SCM_BOOL_F);                           \
  if (scm_is_eq (the_port, ipstate->port))                      \
    SCM_SIMPLE_VECTOR_SET (ipstate->id_positions, id,           \
                           scm_ftell (the_port));               \
  else if (scm_is_true (ipstate->port))                         \
    {                                                           \
      SCM_SIMPLE_VECTOR_SET (ipstate->id_positions, id,         \
                             SCM_BOOL_T);                       \
      ipstate->markers_p = 1;                                   \
      scm_putc (0xE, the_port);                                 \
      scm_putc (POST_INSERT_MODE_OTHER, the_port);              \
      scm_uintprint (id, 10, the_port);                         \
      scm_putc (0xF, the_port);                                 \
    }                                                           \
  PUSH_REF(pstate, obj);					\
} while(0)

#define EXIT_NESTED_DATA(pstate, ipstate)			\
do								\
{								\
  --pstate->top;						\
  if (!ipstate->write_shared_p)                                 \
    scm_hashq_remove_x (ipstate->object_ids,                    \
                        PSTATE_STACK_REF (pstate, pstate->top));\
  PSTATE_STACK_SET (pstate, pstate->top, SCM_UNDEFINED);	\
}								\
while (0)

SCM scm_print_state_vtable = SCM_BOOL_F;
static SCM print_state_pool = SCM_EOL;
scm_i_pthread_mutex_t print_state_mutex = SCM_I_PTHREAD_MUTEX_INITIALIZER;

#ifdef GUILE_DEBUG /* Used for debugging purposes */

SCM_DEFINE (scm_current_pstate, "current-pstate", 0, 0, 0, 
           (),
	    "Return the current-pstate -- the car of the\n"
	    "@code{print_state_pool}.  @code{current-pstate} is only\n"
	    "included in @code{--enable-guile-debug} builds.")
#define FUNC_NAME s_scm_current_pstate
{
  if (!scm_is_null (print_state_pool))
    return SCM_CAR (print_state_pool);
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

#endif

#define PSTATE_SIZE 50L

static SCM
make_print_state (void)
{
  SCM internal_print_state
    = scm_make_struct (scm_internal_print_state_vtable, SCM_INUM0, SCM_EOL);
  scm_internal_print_state *ipstate
    = (scm_internal_print_state *) SCM_STRUCT_DATA (internal_print_state);
  SCM print_state
    = scm_make_struct (scm_print_state_vtable, SCM_INUM0, SCM_EOL);
  scm_print_state *pstate = SCM_PRINT_STATE (print_state);
  pstate->ref_vect = scm_c_make_vector (PSTATE_SIZE, SCM_UNDEFINED);
  pstate->ceiling = SCM_SIMPLE_VECTOR_LENGTH (pstate->ref_vect);
  pstate->highlight_objects = scm_list_1 (internal_print_state);
  ipstate->port = SCM_BOOL_F;
  ipstate->num_allocated_ids = 64;
  ipstate->write_shared_p = 0;
  ipstate->next_id = 0;
  ipstate->next_num = 1;
  ipstate->markers_p = 0;
  ipstate->object_ids = scm_c_make_hash_table (ipstate->num_allocated_ids);
  ipstate->id_positions = scm_c_make_vector (ipstate->num_allocated_ids,
                                             SCM_BOOL_F);
  ipstate->id_label_nums = scm_c_make_vector (ipstate->num_allocated_ids,
                                              SCM_BOOL_F);
  ipstate->needed_ids = SCM_EOL;

  scm_i_pthread_mutex_lock (&internal_print_state_table_mutex);
  scm_hashq_set_x (internal_print_state_table, print_state,
                   internal_print_state);
  scm_i_pthread_mutex_unlock (&internal_print_state_table_mutex);

  return print_state;
}

SCM
scm_make_print_state ()
{
  SCM answer = SCM_BOOL_F;

  /* First try to allocate a print state from the pool */
  scm_i_pthread_mutex_lock (&print_state_mutex);
  if (!scm_is_null (print_state_pool))
    {
      answer = SCM_CAR (print_state_pool);
      print_state_pool = SCM_CDR (print_state_pool);
    }
  scm_i_pthread_mutex_unlock (&print_state_mutex);
  
  return scm_is_false (answer) ? make_print_state () : answer;
}

void
scm_free_print_state (SCM print_state)
{
  SCM handle;
  scm_print_state *pstate = SCM_PRINT_STATE (print_state);
  /* Cleanup before returning print state to pool.
   * It is better to do it here.  Doing it in scm_prin1
   * would cost more since that function is called much more
   * often.
   */
  pstate->fancyp = 0;
  pstate->revealed = 0;
  pstate->highlight_objects = SCM_EOL;
  /* XXX FIXME clear internal print state.  */
  scm_i_pthread_mutex_lock (&print_state_mutex);
  handle = scm_cons (print_state, print_state_pool);
  print_state_pool = handle;
  scm_i_pthread_mutex_unlock (&print_state_mutex);
}

SCM
scm_i_port_with_print_state (SCM port, SCM print_state)
{
  if (SCM_UNBNDP (print_state))
    {
      if (SCM_PORT_WITH_PS_P (port))
	return port;
      else
	print_state = scm_make_print_state ();
      /* port does not need to be coerced since it doesn't have ps */
    }
  else
    port = SCM_COERCE_OUTPORT (port);
  SCM_RETURN_NEWSMOB (scm_tc16_port_with_ps,
		      SCM_UNPACK (scm_cons (port, print_state)));
}

static scm_internal_print_state *
get_internal_print_state (scm_print_state *pstate)
{
  SCM obj;

  if (scm_is_pair (pstate->highlight_objects)
      && ((obj = SCM_CAR (pstate->highlight_objects)), SCM_STRUCTP (obj))
      && scm_is_eq (SCM_STRUCT_VTABLE (obj),
                    scm_internal_print_state_vtable))
    ;
  else
    {
      scm_i_pthread_mutex_lock (&internal_print_state_table_mutex);
      obj = scm_hashq_ref (internal_print_state_table, pstate->handle,
                           SCM_BOOL_F);
      scm_i_pthread_mutex_unlock (&internal_print_state_table_mutex);
      pstate->highlight_objects
        = scm_cons (obj, scm_delq (obj, pstate->highlight_objects));
    }
  return (scm_internal_print_state *) SCM_STRUCT_DATA (obj);
}

static void
grow_simple_vector (SCM *vec_p, unsigned long new_size)
{
  SCM old_vect = *vec_p;
  SCM new_vect = scm_c_make_vector (new_size, SCM_UNDEFINED);
  unsigned long old_size = SCM_SIMPLE_VECTOR_LENGTH (old_vect);
  unsigned long i;

  for (i = 0; i != old_size; ++i)
    SCM_SIMPLE_VECTOR_SET (new_vect, i, SCM_SIMPLE_VECTOR_REF (old_vect, i));

  *vec_p = new_vect;
}

static void
grow_ref_stack (scm_print_state *pstate)
{
  grow_simple_vector (&pstate->ref_vect, (pstate->ceiling *= 2));
}

static void
grow_id_vects (scm_internal_print_state *ipstate)
{
  unsigned long new_size = (ipstate->num_allocated_ids *= 2);
  grow_simple_vector (&ipstate->id_positions,  new_size);
  grow_simple_vector (&ipstate->id_label_nums, new_size);
}

#define PSTATE_STACK_REF(p,i)   SCM_SIMPLE_VECTOR_REF((p)->ref_vect, (i))
#define PSTATE_STACK_SET(p,i,v) SCM_SIMPLE_VECTOR_SET((p)->ref_vect, (i), (v))

static void
legacy_print_circref (SCM port, SCM ref, scm_print_state *pstate)
{
  register long i;
  long self = pstate->top - 1;
  i = pstate->top - 1;
  if (scm_is_pair (PSTATE_STACK_REF (pstate, i)))
    {
      while (i > 0)
	{
	  if (!scm_is_pair (PSTATE_STACK_REF (pstate, i-1))
	      || !scm_is_eq (SCM_CDR (PSTATE_STACK_REF (pstate, i-1)), 
			     SCM_CDR (PSTATE_STACK_REF (pstate, i))))
	    break;
	  --i;
	}
      self = i;
    }
  for (i = pstate->top - 1; 1; --i)
    if (scm_is_eq (PSTATE_STACK_REF(pstate, i), ref))
      break;
  scm_putc ('#', port);
  scm_intprint (i - self, 10, port);
  scm_putc ('#', port);
}

static void
print_circref (SCM port, SCM ref, scm_print_state *pstate,
               scm_internal_print_state *ipstate)
{
  SCM id, num;
  unsigned long cnum;

  if (scm_is_false (ipstate->port))
    legacy_print_circref (port, ref, pstate);
  else
    {
      id = scm_hashq_ref (ipstate->object_ids, ref, SCM_BOOL_F);
      num = scm_vector_ref (ipstate->id_label_nums, id);
      if (scm_is_false (num))
        {
          cnum = ipstate->next_num++;
          num = scm_from_ulong (cnum);
          scm_vector_set_x (ipstate->id_label_nums, id, num);
          ipstate->needed_ids = scm_cons (id, ipstate->needed_ids);
        }
      else
        cnum = scm_to_ulong (num);
      scm_putc ('#', port);
      scm_uintprint (cnum, 10, port);
      scm_putc ('#', port);
    }
}

/* Print the name of a symbol. */

static int
quote_keywordish_symbols (void)
{
  SCM option = SCM_PRINT_KEYWORD_STYLE;

  if (scm_is_false (option))
    return 0;
  if (scm_is_eq (option, sym_reader))
    return scm_is_true (SCM_PACK (SCM_KEYWORD_STYLE));
  return 1;
}

#define INITIAL_IDENTIFIER_MASK                                      \
  (UC_CATEGORY_MASK_Lu | UC_CATEGORY_MASK_Ll | UC_CATEGORY_MASK_Lt   \
   | UC_CATEGORY_MASK_Lm | UC_CATEGORY_MASK_Lo | UC_CATEGORY_MASK_Mn \
   | UC_CATEGORY_MASK_Nl | UC_CATEGORY_MASK_No | UC_CATEGORY_MASK_Pd \
   | UC_CATEGORY_MASK_Pc | UC_CATEGORY_MASK_Po | UC_CATEGORY_MASK_Sc \
   | UC_CATEGORY_MASK_Sm | UC_CATEGORY_MASK_Sk | UC_CATEGORY_MASK_So \
   | UC_CATEGORY_MASK_Co)

#define SUBSEQUENT_IDENTIFIER_MASK                                      \
  (INITIAL_IDENTIFIER_MASK                                              \
   | UC_CATEGORY_MASK_Nd | UC_CATEGORY_MASK_Mc | UC_CATEGORY_MASK_Me)

static int
symbol_has_extended_read_syntax (SCM sym)
{
  size_t pos, len = scm_i_symbol_length (sym);
  scm_t_wchar c;

  /* The empty symbol.  */
  if (len == 0)
    return 1;

  c = scm_i_symbol_ref (sym, 0);

  /* Single dot; conflicts with dotted-pair notation.  */
  if (len == 1 && c == '.')
    return 1;

  /* Other initial-character constraints.  */
  if (c == '\'' || c == '`' || c == ',' || c == '"' || c == ';' || c == '#')
    return 1;

  /* R7RS allows neither '|' nor '\' in bare symbols.  */
  if ((c == '|' || c == '\\') && SCM_PRINT_R7RS_SYMBOLS_P)
    return 1;
  
  /* Keywords can be identified by trailing colons too.  */
  if (c == ':' || scm_i_symbol_ref (sym, len - 1) == ':')
    return quote_keywordish_symbols ();
  
  /* Number-ish symbols.  */
  if (scm_is_true (scm_i_string_to_number (scm_symbol_to_string (sym), 10)))
    return 1;
  
  /* Other disallowed first characters.  */
  if (!uc_is_general_category_withtable (c, INITIAL_IDENTIFIER_MASK))
    return 1;

  /* Otherwise, any character that's in the identifier category mask is
     fine to pass through as-is, provided it's not one of the ASCII
     delimiters like `;'.  */
  for (pos = 1; pos < len; pos++)
    {
      c = scm_i_symbol_ref (sym, pos);
      if (!uc_is_general_category_withtable (c, SUBSEQUENT_IDENTIFIER_MASK))
        return 1;
      else if (c == '"' || c == ';' || c == '#')
        return 1;
      else if ((c == '|' || c == '\\') && SCM_PRINT_R7RS_SYMBOLS_P)
        /* R7RS allows neither '|' nor '\' in bare symbols.  */
        return 1;
    }

  return 0;
}

static void
print_normal_symbol (SCM sym, SCM port)
{
  scm_display (scm_symbol_to_string (sym), port);
}

static void
print_extended_symbol (SCM sym, SCM port)
{
  size_t pos, len;
  scm_t_string_failed_conversion_handler strategy;

  len = scm_i_symbol_length (sym);
  strategy = PORT_CONVERSION_HANDLER (port);

  scm_lfwrite ("#{", 2, port);

  for (pos = 0; pos < len; pos++)
    {
      scm_t_wchar c = scm_i_symbol_ref (sym, pos);
      
      if (uc_is_general_category_withtable (c,
                                            SUBSEQUENT_IDENTIFIER_MASK
                                            | UC_CATEGORY_MASK_Zs))
        {
          if (!display_character (c, port, strategy)
              || (c == '\\' && !display_character (c, port, strategy)))
            scm_encoding_error ("print_extended_symbol", errno,
                                "cannot convert to output locale",
                                port, SCM_MAKE_CHAR (c));
        }
      else
        {
          scm_lfwrite ("\\x", 2, port);
          scm_intprint (c, 16, port);
          scm_putc (';', port);
        }
    }

  scm_lfwrite ("}#", 2, port);
}

static void
print_r7rs_extended_symbol (SCM sym, SCM port)
{
  size_t pos, len;
  scm_t_string_failed_conversion_handler strategy;

  len = scm_i_symbol_length (sym);
  strategy = PORT_CONVERSION_HANDLER (port);

  scm_putc ('|', port);

  for (pos = 0; pos < len; pos++)
    {
      scm_t_wchar c = scm_i_symbol_ref (sym, pos);

      switch (c)
        {
        case '\a': scm_lfwrite ("\\a", 2, port); break;
        case '\b': scm_lfwrite ("\\b", 2, port); break;
        case '\t': scm_lfwrite ("\\t", 2, port); break;
        case '\n': scm_lfwrite ("\\n", 2, port); break;
        case '\r': scm_lfwrite ("\\r", 2, port); break;
        case '|':  scm_lfwrite ("\\|", 2, port); break;
        case '\\': scm_lfwrite ("\\x5c;", 5, port); break;
        default:
          if (uc_is_general_category_withtable (c,
                                                UC_CATEGORY_MASK_L
                                                | UC_CATEGORY_MASK_M
                                                | UC_CATEGORY_MASK_N
                                                | UC_CATEGORY_MASK_P
                                                | UC_CATEGORY_MASK_S)
              || (c == ' '))
            {
              if (!display_character (c, port, strategy))
                scm_encoding_error ("print_r7rs_extended_symbol", errno,
                                    "cannot convert to output locale",
                                    port, SCM_MAKE_CHAR (c));
            }
          else
            {
              scm_lfwrite ("\\x", 2, port);
              scm_intprint (c, 16, port);
              scm_putc (';', port);
            }
          break;
        }
    }

  scm_putc ('|', port);
}

/* FIXME: allow R6RS hex escapes instead of #{...}# or |...|.  */
void
scm_i_print_symbol_name (SCM sym, SCM port)
{
  if (!symbol_has_extended_read_syntax (sym))
    print_normal_symbol (sym, port);
  else if (SCM_PRINT_R7RS_SYMBOLS_P)
    print_r7rs_extended_symbol (sym, port);
  else
    print_extended_symbol (sym, port);
}

void
scm_print_symbol_name (const char *str, size_t len, SCM port)
{
  SCM symbol = scm_from_locale_symboln (str, len);
  scm_i_print_symbol_name (symbol, port);
}

/* Print generally.  Handles both write and display according to PSTATE.
 */
SCM_GPROC(s_write, "write", 1, 1, 0, scm_write, g_write);
SCM_GPROC(s_display, "display", 1, 1, 0, scm_display, g_display);

static void iprin1 (SCM exp, SCM port, scm_print_state *pstate,
                    scm_internal_print_state *ipstate);

static void
scm_i_iprlist (char *hdr, SCM exp, int tlr, SCM port, scm_print_state *pstate,
               scm_internal_print_state *ipstate);

/* Print a character as an octal or hex escape.  */
#define PRINT_CHAR_ESCAPE(i, port)              \
  do                                            \
    {                                           \
      if (!SCM_R6RS_ESCAPES_P)                  \
        scm_intprint (i, 8, port);              \
      else                                      \
        {                                       \
          scm_puts ("x", port);                 \
          scm_intprint (i, 16, port);           \
        }                                       \
    }                                           \
  while (0)

static SCM
compare_post_inserts (SCM a, SCM b)
{
  if (scm_is_true (scm_num_eq_p (SCM_CAR (a), SCM_CAR (b))))
    /* If the positions are the same, we sort by the post-insert mode,
       so that CDR_OPEN post-inserts are placed before OTHER post
       inserts at the same position.  */
    return scm_less_p (SCM_CADR (a), SCM_CADR (b));
  else
    return scm_less_p (SCM_CAR (a), SCM_CAR (b));
}

static SCM compare_post_inserts_proc;

/* Search for a marker in STR starting at position START, with an id
   that was printed as a marker (as recorded in ipstate->id_positions).
   If one is found, put the position in *POS_P, the position after the
   marker in *END_P, the mode in *MODE_P, the id in *ID_P, and return 1.
   If no marker is found, return 0.  */
static int
find_next_marker (SCM str, SCM start, scm_internal_print_state *ipstate,
                  SCM *pos_p, SCM *end_p, int *mode_p, SCM *id_p)
{
  SCM pos, last;

  for (; scm_is_true (pos = scm_string_index (str, SCM_MAKE_CHAR (0xE),
                                              start, SCM_UNDEFINED))
         && scm_is_true (last = scm_string_index (str, SCM_MAKE_CHAR (0xF),
                                                  pos, SCM_UNDEFINED));
       start = scm_sum (pos, SCM_INUM1))
    {
      size_t i = scm_to_size_t (pos);
      size_t clast = scm_to_size_t (last);
      scm_t_wchar mode_char;
      size_t id = 0;

      if (clast < i + 3)
        continue;
      i++;
      mode_char = SCM_CHAR (scm_c_string_ref (str, i++));
      switch (mode_char)
        {
        case POST_INSERT_MODE_CDR_OPEN:
        case POST_INSERT_MODE_CDR_CLOSE:
        case POST_INSERT_MODE_OTHER:
          for (; i < clast; i++)
            {
              scm_t_wchar digit = SCM_CHAR (scm_c_string_ref (str, i));
              if (digit < '0' || digit > '9'
                  || ((((size_t) -1) - (digit - '0')) / 10) <= id)
                break;
              id = 10 * id + (digit - '0');
            }
          if (i < clast
              || id >= ipstate->next_id
              || !scm_is_eq (SCM_BOOL_T,
                             scm_c_vector_ref (ipstate->id_positions, id)))
            continue;
          *pos_p = pos;
          *end_p = scm_sum (last, SCM_INUM1);
          *mode_p = mode_char;
          *id_p = scm_from_size_t (id);
          return 1;
        default:
          continue;
        }
    }
  return 0;
}

static void
print_post_insert (SCM port, int mode, unsigned long num)
{
  switch (mode)
    {
    case POST_INSERT_MODE_CDR_OPEN:
      scm_puts (". #", port);
      scm_uintprint (num, 10, port);
      scm_puts ("=(", port);
      break;
    case POST_INSERT_MODE_CDR_CLOSE:
      scm_putc (')', port);
      break;
    case POST_INSERT_MODE_OTHER:
      scm_putc ('#', port);
      scm_uintprint (num, 10, port);
      scm_putc ('=', port);
      break;
    default:
      abort ();
    }
}

static void
scm_i_iprin1 (SCM exp, SCM port, scm_print_state *pstate,
              scm_internal_print_state *ipstate);

static void
scm_i_wrapped_iprin1 (SCM exp, SCM port, scm_print_state *pstate,
                      scm_internal_print_state *ipstate)
{
  SCM tport;

  tport = scm_open_output_string ();
  scm_i_set_port_encoding_x (tport, "UTF-8");

  ipstate->port = tport;
  scm_i_iprin1 (exp, tport, pstate, ipstate);
  ipstate->port = SCM_BOOL_F;

  if (scm_is_null (ipstate->needed_ids)
      && !ipstate->markers_p)
    scm_display (scm_get_output_string (tport), port);
  else
    {
      SCM ids, post_inserts, last_pos;

      post_inserts = SCM_EOL;
      for (ids = ipstate->needed_ids;
           scm_is_pair (ids);
           ids = SCM_CDR (ids))
        {
          SCM id = SCM_CAR (ids);
          SCM num = scm_vector_ref (ipstate->id_label_nums, id);
          SCM pos = scm_vector_ref (ipstate->id_positions, id);

          if (scm_is_eq (pos, SCM_BOOL_T))
            ;
          else if (scm_is_pair (pos))
            post_inserts = scm_cons2
              (scm_cons2 (SCM_CAR (pos),
                          SCM_I_MAKINUM (POST_INSERT_MODE_CDR_OPEN),
                          num),
               scm_cons2 (SCM_CDR (pos),
                          SCM_I_MAKINUM (POST_INSERT_MODE_CDR_CLOSE),
                          num),
               post_inserts);
          else
            post_inserts = scm_cons
              (scm_cons2 (pos,
                          SCM_I_MAKINUM (POST_INSERT_MODE_OTHER),
                          num),
               post_inserts);
        }

      /* Sort by position */
      post_inserts = scm_sort_x (post_inserts, compare_post_inserts_proc);

      last_pos = SCM_INUM0;
      for (;;)
        {
          SCM elt, pos, info, str;
          unsigned long num;
          int mode;

          if (scm_is_pair (post_inserts))
            {
              elt  = SCM_CAR (post_inserts);
              pos  = SCM_CAR (elt);
              info = SCM_CDR (elt);
              mode = scm_to_int (SCM_CAR (info));
              num  = scm_to_ulong (SCM_CDR (info));
            }
          else
            pos = SCM_UNDEFINED;

          str = scm_i_strport_to_string (tport, last_pos, pos);
          if (ipstate->markers_p)
            {
              SCM start = SCM_INUM0;
              SCM pos, end, id, num;
              int mode;

              for (start = SCM_INUM0;
                   find_next_marker (str, start, ipstate,
                                     &pos, &end, &mode, &id);
                   start = end)
                {
                  scm_display (scm_substring (str, start, pos),
                               port);
                  num = scm_vector_ref (ipstate->id_label_nums, id);
                  if (scm_is_true (num))
                    print_post_insert (port, mode, scm_to_ulong (num));
                }
              scm_display (scm_substring (str, start, SCM_UNDEFINED),
                           port);
            }
          else
            scm_display (str, port);

          if (scm_is_eq (pos, SCM_UNDEFINED))
            break;

          print_post_insert (port, mode, num);

          last_pos = pos;
          post_inserts = SCM_CDR (post_inserts);
        }
    }

  ipstate->next_id = 0;
  ipstate->next_num = 1;
  ipstate->markers_p = 0;
  scm_hash_clear_x (ipstate->object_ids);
  ipstate->needed_ids = SCM_EOL;

  scm_close_port (tport);
}
  
static void
scm_i_iprin1 (SCM exp, SCM port, scm_print_state *pstate,
              scm_internal_print_state *ipstate)
{
  if (scm_is_false (ipstate->port)
      && SCM_NIMP (exp) && scm_is_false (scm_string_p (exp))
      && (ipstate->write_shared_p || SCM_PRINT_DATUM_LABELS_P))
    scm_i_wrapped_iprin1 (exp, port, pstate, ipstate);
  else if (pstate->fancyp
      && scm_is_true (scm_memq (exp, pstate->highlight_objects)))
    {
      scm_display (SCM_PRINT_HIGHLIGHT_PREFIX, port);
      iprin1 (exp, port, pstate, ipstate);
      scm_display (SCM_PRINT_HIGHLIGHT_SUFFIX, port);
    }
  else
    iprin1 (exp, port, pstate, ipstate);
}

void
scm_iprin1 (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_i_iprin1 (exp, port, pstate, get_internal_print_state (pstate));
}

static void
iprin1 (SCM exp, SCM port, scm_print_state *pstate,
        scm_internal_print_state *ipstate)
{
  switch (SCM_ITAG3 (exp))
    {
    case scm_tc3_tc7_1:
    case scm_tc3_tc7_2:
      /* These tc3 tags should never occur in an immediate value.  They are
       * only used in cell types of non-immediates, i. e. the value returned
       * by SCM_CELL_TYPE (exp) can use these tags.
       */
      scm_ipruk ("immediate", exp, port);
      break;
    case scm_tc3_int_1:
    case scm_tc3_int_2:
      scm_intprint (SCM_I_INUM (exp), 10, port);
      break;
    case scm_tc3_imm24:
      if (SCM_CHARP (exp))
	{
	  if (SCM_WRITINGP (pstate))
	    write_character (SCM_CHAR (exp), port, 0);
	  else
	    {
	      if (!display_character (SCM_CHAR (exp), port,
				      PORT_CONVERSION_HANDLER (port)))
		scm_encoding_error (__func__, errno,
				    "cannot convert to output locale",
				    port, exp);
	    }
	}
      else if (SCM_IFLAGP (exp)
	       && ((size_t) SCM_IFLAGNUM (exp) < (sizeof iflagnames / sizeof (char *))))
        {
          scm_puts (iflagnames [SCM_IFLAGNUM (exp)], port);
        }
      else
	{
	  /* unknown immediate value */
	  scm_ipruk ("immediate", exp, port);
	}
      break;
    case scm_tc3_cons:
      switch (SCM_TYP7 (exp))
	{
	case scm_tcs_struct:
	  {
	    ENTER_NESTED_DATA (port, pstate, ipstate, exp, circref);
	    if (SCM_OBJ_CLASS_FLAGS (exp) & SCM_CLASSF_GOOPS)
	      {
		SCM pwps, print = pstate->writingp ? g_write : g_display;
		if (SCM_UNPACK (print) == 0)
		  goto print_struct;
		pwps = scm_i_port_with_print_state (port, pstate->handle);
		pstate->revealed = 1;
		scm_call_generic_2 (print, exp, pwps);
	      }
	    else
	      {
	      print_struct:
		scm_print_struct (exp, port, pstate);
	      }
	    EXIT_NESTED_DATA (pstate, ipstate);
	  }
	  break;
	case scm_tcs_cons_imcar:
	case scm_tcs_cons_nimcar:
	  ENTER_NESTED_DATA (port, pstate, ipstate, exp, circref);
	  scm_i_iprlist ("(", exp, ')', port, pstate, ipstate);
	  EXIT_NESTED_DATA (pstate, ipstate);
	  break;
	circref:
	  print_circref (port, exp, pstate, ipstate);
	  break;
	case scm_tc7_number:
          switch SCM_TYP16 (exp) {
          case scm_tc16_big:
            scm_bigprint (exp, port, pstate);
            break;
          case scm_tc16_real:
            scm_print_real (exp, port, pstate);
            break;
          case scm_tc16_complex:
            scm_print_complex (exp, port, pstate);
            break;
          case scm_tc16_fraction:
            scm_i_print_fraction (exp, port, pstate);
            break;
          }
	  break;
        case scm_tc7_string:
          if (SCM_WRITINGP (pstate))
            {
              size_t len, i;

              display_character ('"', port, iconveh_question_mark);
              len = scm_i_string_length (exp);
              for (i = 0; i < len; ++i)
		write_character (scm_i_string_ref (exp, i), port, 1);

              display_character ('"', port, iconveh_question_mark);
              scm_remember_upto_here_1 (exp);
            }
          else
	    {
	      size_t len, printed;

	      len = scm_i_string_length (exp);
	      printed = display_string (scm_i_string_data (exp),
					scm_i_is_narrow_string (exp),
					len, port,
					PORT_CONVERSION_HANDLER (port));
	      if (SCM_UNLIKELY (printed < len))
		scm_encoding_error (__func__, errno,
				    "cannot convert to output locale",
				    port, scm_c_string_ref (exp, printed));
	    }

          scm_remember_upto_here_1 (exp);
          break;
	case scm_tc7_symbol:
	  if (scm_i_symbol_is_interned (exp))
	    {
	      scm_i_print_symbol_name (exp, port);
	      scm_remember_upto_here_1 (exp);
	    }
	  else
	    {
	      scm_puts ("#<uninterned-symbol ", port);
	      scm_i_print_symbol_name (exp, port);
	      scm_putc (' ', port);
	      scm_uintprint (SCM_UNPACK (exp), 16, port);
	      scm_putc ('>', port);
	    }
	  break;
	case scm_tc7_variable:
	  scm_i_variable_print (exp, port, pstate);
	  break;
	case scm_tc7_program:
	  scm_i_program_print (exp, port, pstate);
	  break;
	case scm_tc7_pointer:
	  scm_i_pointer_print (exp, port, pstate);
	  break;
	case scm_tc7_hashtable:
	  scm_i_hashtable_print (exp, port, pstate);
	  break;
	case scm_tc7_fluid:
	  scm_i_fluid_print (exp, port, pstate);
	  break;
	case scm_tc7_dynamic_state:
	  scm_i_dynamic_state_print (exp, port, pstate);
	  break;
	case scm_tc7_frame:
	  scm_i_frame_print (exp, port, pstate);
	  break;
	case scm_tc7_objcode:
	  scm_i_objcode_print (exp, port, pstate);
	  break;
	case scm_tc7_vm:
	  scm_i_vm_print (exp, port, pstate);
	  break;
	case scm_tc7_vm_cont:
	  scm_i_vm_cont_print (exp, port, pstate);
	  break;
	case scm_tc7_prompt:
	  scm_i_prompt_print (exp, port, pstate);
	  break;
	case scm_tc7_with_fluids:
	  scm_i_with_fluids_print (exp, port, pstate);
	  break;
	case scm_tc7_array:
	  ENTER_NESTED_DATA (port, pstate, ipstate, exp, circref);
          scm_i_print_array (exp, port, pstate);
          EXIT_NESTED_DATA (pstate, ipstate);
          break;
	case scm_tc7_bytevector:
	  scm_i_print_bytevector (exp, port, pstate);
	  break;
	case scm_tc7_bitvector:
	  scm_i_print_bitvector (exp, port, pstate);
	  break;
	case scm_tc7_wvect:
	  ENTER_NESTED_DATA (port, pstate, ipstate, exp, circref);
	  if (SCM_IS_WHVEC (exp))
	    scm_puts ("#wh(", port);
	  else
	    scm_puts ("#w(", port);
	  goto common_vector_printer;
	case scm_tc7_vector:
	  ENTER_NESTED_DATA (port, pstate, ipstate, exp, circref);
	  scm_puts ("#(", port);
	common_vector_printer:
	  {
	    register long i;
	    long last = SCM_SIMPLE_VECTOR_LENGTH (exp) - 1;
	    int cutp = 0;
	    if (pstate->fancyp
		&& SCM_SIMPLE_VECTOR_LENGTH (exp) > pstate->length)
	      {
		last = pstate->length - 1;
		cutp = 1;
	      }
	    if (SCM_I_WVECTP (exp))
	      {
		/* Elements of weak vectors may not be accessed via the
		   `SIMPLE_VECTOR_REF ()' macro.  */
		for (i = 0; i < last; ++i)
		  {
		    scm_i_iprin1 (scm_c_weak_vector_ref (exp, i),
				  port, pstate, ipstate);
		    scm_putc (' ', port);
		  }
	      }
	    else
	      {
		for (i = 0; i < last; ++i)
		  {
                    scm_i_iprin1 (SCM_SIMPLE_VECTOR_REF (exp, i),
                                  port, pstate, ipstate);
		    scm_putc (' ', port);
		  }
	      }

	    if (i == last)
	      {
		/* CHECK_INTS; */
                scm_i_iprin1 (SCM_I_WVECTP (exp)
			      ? scm_c_weak_vector_ref (exp, i)
			      : SCM_SIMPLE_VECTOR_REF (exp, i),
			      port, pstate, ipstate);
	      }
	    if (cutp)
	      scm_puts (" ...", port);
	    scm_putc (')', port);
	  }
	  EXIT_NESTED_DATA (pstate, ipstate);
	  break;
	case scm_tc7_port:
	  {
	    register long i = SCM_PTOBNUM (exp);
	    if (i < scm_numptob
		&& scm_ptobs[i].print
		&& (scm_ptobs[i].print) (exp, port, pstate))
	      break;
	    goto punk;
	  }
	case scm_tc7_smob:
	  ENTER_NESTED_DATA (port, pstate, ipstate, exp, circref);
	  SCM_SMOB_DESCRIPTOR (exp).print (exp, port, pstate);
	  EXIT_NESTED_DATA (pstate, ipstate);
	  break;
	default:
          /* case scm_tcs_closures: */
	punk:
	  scm_ipruk ("type", exp, port);
	}
    }
}

/* Print states are necessary for circular reference safe printing.
 * They are also expensive to allocate.  Therefore print states are
 * kept in a pool so that they can be reused.
 */

/* The PORT argument can also be a print-state/port pair, which will
 * then be used instead of allocating a new print state.  This is
 * useful for continuing a chain of print calls from Scheme.  */

static void
scm_i_prin1 (SCM exp, SCM port, int writingp, int sharedp)
{
  SCM handle = SCM_BOOL_F; /* Will GC protect the handle whilst unlinked */
  SCM pstate_scm;
  scm_print_state *pstate;
  scm_internal_print_state *ipstate;
  int old_writingp, old_sharedp;

  /* If PORT is a print-state/port pair, use that.  Else create a new
     print-state. */

  if (SCM_PORT_WITH_PS_P (port))
    {
      pstate_scm = SCM_PORT_WITH_PS_PS (port);
      port = SCM_PORT_WITH_PS_PORT (port);
    }
  else
    {
      /* First try to allocate a print state from the pool */
      scm_i_pthread_mutex_lock (&print_state_mutex);
      if (!scm_is_null (print_state_pool))
	{
	  handle = print_state_pool;
	  print_state_pool = SCM_CDR (print_state_pool);
	}
      scm_i_pthread_mutex_unlock (&print_state_mutex);
      if (scm_is_false (handle))
	handle = scm_list_1 (make_print_state ());
      pstate_scm = SCM_CAR (handle);
    }

  pstate = SCM_PRINT_STATE (pstate_scm);
  ipstate = get_internal_print_state (pstate);
  if (scm_is_true (handle))
    {
      ipstate->next_id = 0;
      ipstate->next_num = 1;
      ipstate->write_shared_p = 0;
      ipstate->markers_p = 0;
      scm_hash_clear_x (ipstate->object_ids);
      ipstate->needed_ids = SCM_EOL;
    }
  old_writingp = pstate->writingp;
  old_sharedp = ipstate->write_shared_p;
  pstate->writingp = writingp;
  if (sharedp)
    ipstate->write_shared_p = 1;
  scm_i_iprin1 (exp, port, pstate, ipstate);
  pstate->writingp = old_writingp;
  ipstate->write_shared_p = old_sharedp;

  /* XXX FIXME: if this was 'write-shared', but we were previously
     in non-shared mode, then we should restore ipstate->object_ids
     to its previous value. */

  /* Return print state to pool if it has been created above and
     hasn't escaped to Scheme. */

  if (scm_is_true (handle) && !pstate->revealed)
    {
      scm_i_pthread_mutex_lock (&print_state_mutex);
      SCM_SETCDR (handle, print_state_pool);
      print_state_pool = handle;
      scm_i_pthread_mutex_unlock (&print_state_mutex);
    }
}

void
scm_prin1 (SCM exp, SCM port, int writingp)
{
  scm_i_prin1 (exp, port, writingp, 0);
}

/* Convert codepoint CH to UTF-8 and store the result in UTF8.  Return
   the number of bytes of the UTF-8-encoded string.  */
static size_t
codepoint_to_utf8 (scm_t_wchar ch, scm_t_uint8 utf8[4])
{
  size_t len;
  scm_t_uint32 codepoint;

  codepoint = (scm_t_uint32) ch;

  if (codepoint <= 0x7f)
    {
      len = 1;
      utf8[0] = (scm_t_uint8) codepoint;
    }
  else if (codepoint <= 0x7ffUL)
    {
      len = 2;
      utf8[0] = 0xc0 | (codepoint >> 6);
      utf8[1] = 0x80 | (codepoint & 0x3f);
    }
  else if (codepoint <= 0xffffUL)
    {
      len = 3;
      utf8[0] = 0xe0 | (codepoint >> 12);
      utf8[1] = 0x80 | ((codepoint >> 6) & 0x3f);
      utf8[2] = 0x80 | (codepoint & 0x3f);
    }
  else
    {
      len = 4;
      utf8[0] = 0xf0 | (codepoint >> 18);
      utf8[1] = 0x80 | ((codepoint >> 12) & 0x3f);
      utf8[2] = 0x80 | ((codepoint >> 6) & 0x3f);
      utf8[3] = 0x80 | (codepoint & 0x3f);
    }

  return len;
}

#define STR_REF(s, x)				\
  (narrow_p					\
   ? (scm_t_wchar) ((unsigned char *) (s))[x]	\
   : ((scm_t_wchar *) (s))[x])

/* Write STR to PORT as UTF-8.  STR is a LEN-codepoint string; it is
   narrow if NARROW_P is true, wide otherwise.  Return LEN.  */
static size_t
display_string_as_utf8 (const void *str, int narrow_p, size_t len,
			SCM port)
{
  size_t printed = 0;

  while (len > printed)
    {
      size_t utf8_len, i;
      char *input, utf8_buf[256];

      /* Convert STR to UTF-8.  */
      for (i = printed, utf8_len = 0, input = utf8_buf;
	   i < len && utf8_len + 4 < sizeof (utf8_buf);
	   i++)
	{
	  utf8_len += codepoint_to_utf8 (STR_REF (str, i),
					 (scm_t_uint8 *) input);
	  input = utf8_buf + utf8_len;
	}

      /* INPUT was successfully converted, entirely; print the
	 result.  */
      scm_lfwrite (utf8_buf, utf8_len, port);
      printed += i - printed;
    }

  assert (printed == len);

  return len;
}

/* Convert STR through PORT's output conversion descriptor and write the
   output to PORT.  Return the number of codepoints written.  */
static size_t
display_string_using_iconv (const void *str, int narrow_p, size_t len,
			    SCM port,
			    scm_t_string_failed_conversion_handler strategy)
{
  size_t printed;
  scm_t_iconv_descriptors *id;
  scm_t_port_internal *pti = SCM_PORT_GET_INTERNAL (port);

  id = scm_i_port_iconv_descriptors (port, SCM_PORT_WRITE);

  if (SCM_UNLIKELY (pti->at_stream_start_for_bom_write && len > 0))
    {
      scm_t_port *pt = SCM_PTAB_ENTRY (port);

      /* Record that we're no longer at stream start.  */
      pti->at_stream_start_for_bom_write = 0;
      if (pt->rw_random)
        pti->at_stream_start_for_bom_read = 0;

      /* Write a BOM if appropriate.  */
      if (SCM_UNLIKELY (c_strcasecmp(pt->encoding, "UTF-16") == 0
                        || c_strcasecmp(pt->encoding, "UTF-32") == 0))
        display_character (SCM_UNICODE_BOM, port, iconveh_error);
    }

  printed = 0;

  while (len > printed)
    {
      size_t done, utf8_len, input_left, output_left, i;
      size_t codepoints_read, output_len;
      char *input, *output;
      char utf8_buf[256], encoded_output[256];
      size_t offsets[256];

      /* Convert STR to UTF-8.  */
      for (i = printed, utf8_len = 0, input = utf8_buf;
	   i < len && utf8_len + 4 < sizeof (utf8_buf);
	   i++)
	{
	  offsets[utf8_len] = i;
	  utf8_len += codepoint_to_utf8 (STR_REF (str, i),
					 (scm_t_uint8 *) input);
	  input = utf8_buf + utf8_len;
	}

      input = utf8_buf;
      input_left = utf8_len;

      output = encoded_output;
      output_left = sizeof (encoded_output);

      done = iconv (id->output_cd, &input, &input_left,
		    &output, &output_left);

      output_len = sizeof (encoded_output) - output_left;

      if (SCM_UNLIKELY (done == (size_t) -1))
	{
          int errno_save = errno;

	  /* Reset the `iconv' state.  */
	  iconv (id->output_cd, NULL, NULL, NULL, NULL);

	  /* Print the OUTPUT_LEN bytes successfully converted.  */
	  scm_lfwrite (encoded_output, output_len, port);

	  /* See how many input codepoints these OUTPUT_LEN bytes
	     corresponds to.  */
	  codepoints_read = offsets[input - utf8_buf] - printed;
	  printed += codepoints_read;

	  if (errno_save == EILSEQ &&
	      strategy != SCM_FAILED_CONVERSION_ERROR)
	    {
	      /* Conversion failed somewhere in INPUT and we want to
		 escape or substitute the offending input character.  */

	      if (strategy == SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE)
		{
		  scm_t_wchar ch;

		  /* Find CH, the offending codepoint, and escape it.  */
		  ch = STR_REF (str, offsets[input - utf8_buf]);
		  write_character_escaped (ch, 1, port);
		}
	      else
		/* STRATEGY is `SCM_FAILED_CONVERSION_QUESTION_MARK'.  */
		display_string ("?", 1, 1, port, strategy);

	      printed++;
	    }
	  else
	    /* Something bad happened that we can't handle: bail out.  */
	    break;
	}
      else
	{
	  /* INPUT was successfully converted, entirely; print the
	     result.  */
	  scm_lfwrite (encoded_output, output_len, port);
	  codepoints_read = i - printed;
	  printed += codepoints_read;
	}
    }

  return printed;
}

#undef STR_REF

/* Display the LEN codepoints in STR to PORT according to STRATEGY;
   return the number of codepoints successfully displayed.  If NARROW_P,
   then STR is interpreted as a sequence of `char', denoting a Latin-1
   string; otherwise it's interpreted as a sequence of
   `scm_t_wchar'.  */
static size_t
display_string (const void *str, int narrow_p,
		size_t len, SCM port,
		scm_t_string_failed_conversion_handler strategy)

{
  scm_t_port_internal *pti;

  pti = SCM_PORT_GET_INTERNAL (port);

  if (pti->encoding_mode == SCM_PORT_ENCODING_MODE_UTF8)
    return display_string_as_utf8 (str, narrow_p, len, port);
  else
    return display_string_using_iconv (str, narrow_p, len,
				       port, strategy);
}

/* Attempt to display CH to PORT according to STRATEGY.  Return non-zero
   if CH was successfully displayed, zero otherwise (e.g., if it was not
   representable in PORT's encoding.)  */
static int
display_character (scm_t_wchar ch, SCM port,
		   scm_t_string_failed_conversion_handler strategy)
{
  return display_string (&ch, 0, 1, port, strategy) == 1;
}

/* Attempt to pretty-print CH, a combining character, to PORT.  Return
   zero upon failure, non-zero otherwise.  The idea is to print CH above
   a dotted circle to make it more visible.  */
static int
write_combining_character (scm_t_wchar ch, SCM port)
{
  scm_t_wchar str[2];

  str[0] = SCM_CODEPOINT_DOTTED_CIRCLE;
  str[1] = ch;

  return display_string (str, 0, 2, port, iconveh_error) == 2;
}

/* Write CH to PORT in its escaped form, using the string escape syntax
   if STRING_ESCAPES_P is non-zero.  */
static void
write_character_escaped (scm_t_wchar ch, int string_escapes_p, SCM port)
{
  if (string_escapes_p)
    {
      /* Represent CH using the in-string escape syntax.  */

      static const char hex[] = "0123456789abcdef";
      static const char escapes[7] = "abtnvfr";
      char buf[9];

      if (ch >= 0x07 && ch <= 0x0D && ch != 0x0A)
	{
	  /* Use special escapes for some C0 controls.  */
	  buf[0] = '\\';
	  buf[1] = escapes[ch - 0x07];
	  scm_lfwrite (buf, 2, port);
	}
      else if (!SCM_R6RS_ESCAPES_P)
	{
	  if (ch <= 0xFF)
	    {
	      buf[0] = '\\';
	      buf[1] = 'x';
	      buf[2] = hex[ch / 16];
	      buf[3] = hex[ch % 16];
	      scm_lfwrite (buf, 4, port);
	    }
	  else if (ch <= 0xFFFF)
	    {
	      buf[0] = '\\';
	      buf[1] = 'u';
	      buf[2] = hex[(ch & 0xF000) >> 12];
	      buf[3] = hex[(ch & 0xF00) >> 8];
	      buf[4] = hex[(ch & 0xF0) >> 4];
	      buf[5] = hex[(ch & 0xF)];
	      scm_lfwrite (buf, 6, port);
	    }
	  else if (ch > 0xFFFF)
	    {
	      buf[0] = '\\';
	      buf[1] = 'U';
	      buf[2] = hex[(ch & 0xF00000) >> 20];
	      buf[3] = hex[(ch & 0xF0000) >> 16];
	      buf[4] = hex[(ch & 0xF000) >> 12];
	      buf[5] = hex[(ch & 0xF00) >> 8];
	      buf[6] = hex[(ch & 0xF0) >> 4];
	      buf[7] = hex[(ch & 0xF)];
	      scm_lfwrite (buf, 8, port);
	    }
	}
      else
	{
	  /* Print an R6RS variable-length hex escape: "\xNNNN;".  */
	  scm_t_wchar ch2 = ch;

	  int i = 8;
	  buf[i] = ';';
	  i --;
	  if (ch == 0)
	    buf[i--] = '0';
	  else
	    while (ch2 > 0)
	      {
		buf[i] = hex[ch2 & 0xF];
		ch2 >>= 4;
		i --;
	      }
	  buf[i] = 'x';
	  i --;
	  buf[i] = '\\';
	  scm_lfwrite (buf + i, 9 - i, port);
	}
    }
  else
    {
      /* Represent CH using the character escape syntax.  */
      const char *name;

      name = scm_i_charname (SCM_MAKE_CHAR (ch));
      if (name != NULL)
	scm_puts (name, port);
      else
	PRINT_CHAR_ESCAPE (ch, port);
    }
}

/* Write CH to PORT, escaping it if it's non-graphic or not
   representable in PORT's encoding.  If STRING_ESCAPES_P is true and CH
   needs to be escaped, it is escaped using the in-string escape syntax;
   otherwise the character escape syntax is used.  */
static void
write_character (scm_t_wchar ch, SCM port, int string_escapes_p)
{
  int printed = 0;
  scm_t_string_failed_conversion_handler strategy;

  strategy = PORT_CONVERSION_HANDLER (port);

  if (string_escapes_p)
    {
      /* Check if CH deserves special treatment.  */
      if (ch == '"' || ch == '\\')
	{
	  display_character ('\\', port, iconveh_question_mark);
	  display_character (ch, port, strategy);
	  printed = 1;
	}
      else if (ch == '\n' && SCM_PRINT_ESCAPE_NEWLINES_P)
        {
	  display_character ('\\', port, iconveh_question_mark);
	  display_character ('n', port, strategy);
	  printed = 1;
        }
      else if (ch == ' ' || ch == '\n')
	{
	  display_character (ch, port, strategy);
	  printed = 1;
	}
    }
  else
    {
      display_string ("#\\", 1, 2, port, iconveh_question_mark);

      if (uc_combining_class (ch) != UC_CCC_NR)
	/* Character is a combining character, so attempt to
	   pretty-print it.  */
	printed = write_combining_character (ch, port);
    }

  if (!printed
      && uc_is_general_category_withtable (ch,
					   UC_CATEGORY_MASK_L |
					   UC_CATEGORY_MASK_M |
					   UC_CATEGORY_MASK_N |
					   UC_CATEGORY_MASK_P |
					   UC_CATEGORY_MASK_S))
    /* CH is graphic; attempt to display it.  */
    printed = display_character (ch, port, iconveh_error);

  if (!printed)
    /* CH isn't graphic or cannot be represented in PORT's encoding.  */
    write_character_escaped (ch, string_escapes_p, port);
}

/* Display STR to PORT from START inclusive to END exclusive.  */
void
scm_i_display_substring (SCM str, size_t start, size_t end, SCM port)
{
  int narrow_p;
  const char *buf;
  size_t len, printed;

  buf = scm_i_string_data (str);
  len = end - start;
  narrow_p = scm_i_is_narrow_string (str);
  buf += start * (narrow_p ? sizeof (char) : sizeof (scm_t_wchar));

  printed = display_string (buf, narrow_p, end - start, port,
			    PORT_CONVERSION_HANDLER (port));

  if (SCM_UNLIKELY (printed < len))
    scm_encoding_error (__func__, errno,
			"cannot convert to output locale",
			port, scm_c_string_ref (str, printed + start));
}


/* Print an integer.
 */

void 
scm_intprint (scm_t_intmax n, int radix, SCM port)
{
  char num_buf[SCM_INTBUFLEN];
  scm_lfwrite (num_buf, scm_iint2str (n, radix, num_buf), port);
}

void 
scm_uintprint (scm_t_uintmax n, int radix, SCM port)
{
  char num_buf[SCM_INTBUFLEN];
  scm_lfwrite (num_buf, scm_iuint2str (n, radix, num_buf), port);
}

/* Print an object of unrecognized type.
 */

void 
scm_ipruk (char *hdr, SCM ptr, SCM port)
{
  scm_puts ("#<unknown-", port);
  scm_puts (hdr, port);
  if (1) /* (scm_in_heap_p (ptr)) */ /* FIXME */
    {
      scm_puts (" (0x", port);
      scm_uintprint (SCM_CELL_WORD_0 (ptr), 16, port);
      scm_puts (" . 0x", port);
      scm_uintprint (SCM_CELL_WORD_1 (ptr), 16, port);
      scm_puts (") @", port);
    }
  scm_puts (" 0x", port);
  scm_uintprint (SCM_UNPACK (ptr), 16, port);
  scm_putc ('>', port);
}


/* Print a list.
 */
static void
scm_i_iprlist (char *hdr, SCM exp, int tlr, SCM port, scm_print_state *pstate,
               scm_internal_print_state *ipstate)
{
  SCM cdr_ids = SCM_EOL;
  SCM cdrs = SCM_EOL;
  long floor = pstate->top - 2;
  long n = pstate->length;

  scm_puts (hdr, port);
  
  scm_i_iprin1 (SCM_CAR (exp), port, pstate, ipstate);
  exp = SCM_CDR (exp); --n;
  for (; scm_is_pair (exp); exp = SCM_CDR (exp))
    {
      unsigned long id = ipstate->next_id;
      SCM s_id = SCM_I_MAKINUM (id);
      SCM obj_id = scm_hashq_create_handle_x (ipstate->object_ids, exp, s_id);
      if (!scm_is_eq (SCM_CDR (obj_id), s_id))
        goto circref;

      if (!ipstate->write_shared_p)
        cdrs = scm_cons (exp, cdrs);

      if (pstate->fancyp)
        {
          if (n == 0)
            {
              scm_puts (" ...", port);
              goto skip_tail;
            }
          else
            --n;
        }
      PUSH_REF(pstate, exp);
      ++pstate->list_offset;
      scm_putc (' ', port);

      /* CHECK_INTS; */
      ipstate->next_id++;
      if (id == ipstate->num_allocated_ids)
        grow_id_vects (ipstate);
      SCM_SIMPLE_VECTOR_SET (ipstate->id_label_nums, id,
                             SCM_BOOL_F);
      cdr_ids = scm_cons (s_id, cdr_ids);
      if (scm_is_eq (port, ipstate->port))
        SCM_SIMPLE_VECTOR_SET (ipstate->id_positions, id,
                               scm_list_1 (scm_ftell (port)));
      else if (scm_is_true (ipstate->port))
        {
          SCM_SIMPLE_VECTOR_SET (ipstate->id_positions, id,
                                 SCM_BOOL_T);
          ipstate->markers_p = 1;
          scm_putc (0xE, port);
          scm_putc (POST_INSERT_MODE_CDR_OPEN, port);
          scm_uintprint (id, 10, port);
          scm_putc (0xF, port);
        }

      scm_i_iprin1 (SCM_CAR (exp), port, pstate, ipstate);
    }

  if (!SCM_NULL_OR_NIL_P (exp))
    {
      scm_puts (" . ", port);
      scm_i_iprin1 (exp, port, pstate, ipstate);
    }
skip_tail:
  pstate->list_offset -= pstate->top - floor - 2;
  goto end;

circref:
  pstate->list_offset -= pstate->top - floor - 2;
  scm_puts (" . ", port);
  print_circref (port, exp, pstate, ipstate);

end:
  if (!ipstate->write_shared_p)
    for (; scm_is_pair (cdrs); cdrs = SCM_CDR (cdrs))
      scm_hashq_remove_x (ipstate->object_ids, SCM_CAR (cdrs));

  if (scm_is_eq (port, ipstate->port))
    {
      SCM pos = scm_ftell (port);
      for (; scm_is_pair (cdr_ids); cdr_ids = SCM_CDR (cdr_ids))
        SCM_SETCDR (scm_vector_ref (ipstate->id_positions, SCM_CAR (cdr_ids)),
                    pos);
    }
  else if (scm_is_true (ipstate->port))
    {
      for (; scm_is_pair (cdr_ids); cdr_ids = SCM_CDR (cdr_ids))
        {
          scm_putc (0xE, port);
          scm_putc (POST_INSERT_MODE_CDR_CLOSE, port);
          scm_uintprint (scm_to_ulong (SCM_CAR (cdr_ids)), 10, port);
          scm_putc (0xF, port);
        }
    }

  scm_putc (tlr, port);
  /* XXX FIXME black-hole the popped refs */
  pstate->top = floor + 2;
  return;
}

void
scm_iprlist (char *hdr, SCM exp, int tlr, SCM port, scm_print_state *pstate)
{
  scm_i_iprlist (hdr, exp, tlr, port, pstate,
                 get_internal_print_state (pstate));
}



int
scm_valid_oport_value_p	(SCM val)
{
  return (SCM_OPOUTPORTP (val)
          || (SCM_PORT_WITH_PS_P (val)
              && SCM_OPOUTPORTP (SCM_PORT_WITH_PS_PORT (val))));
}

SCM_DEFINE (scm_write_shared, "write-shared", 1, 1, 0,
            (SCM obj, SCM port),
	    "Write OBJ to PORT with shared structure represented using datum labels.")
#define FUNC_NAME s_scm_write_shared
{
  if (SCM_UNBNDP (port))
    port = scm_current_output_port ();
  else
    SCM_VALIDATE_OPORT_VALUE (2, port);

  scm_i_prin1 (obj, port, 1, 1);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* SCM_GPROC(s_write, "write", 1, 1, 0, scm_write, g_write); */

SCM 
scm_write (SCM obj, SCM port)
{
  if (SCM_UNBNDP (port))
    port = scm_current_output_port ();

  SCM_ASSERT (scm_valid_oport_value_p (port), port, SCM_ARG2, s_write);

  scm_i_prin1 (obj, port, 1, 0);
  return SCM_UNSPECIFIED;
}


/* SCM_GPROC(s_display, "display", 1, 1, 0, scm_display, g_display); */

SCM 
scm_display (SCM obj, SCM port)
{
  if (SCM_UNBNDP (port))
    port = scm_current_output_port ();

  SCM_ASSERT (scm_valid_oport_value_p (port), port, SCM_ARG2, s_display);

  scm_i_prin1 (obj, port, 0, 0);
  return SCM_UNSPECIFIED;
}


SCM_DEFINE (scm_simple_format, "simple-format", 2, 0, 1,
            (SCM destination, SCM message, SCM args),
	    "Write @var{message} to @var{destination}, defaulting to\n"
	    "the current output port.\n"
	    "@var{message} can contain @code{~A} (was @code{%s}) and\n"
	    "@code{~S} (was @code{%S}) escapes.  When printed,\n"
	    "the escapes are replaced with corresponding members of\n"
	    "@var{args}:\n"
	    "@code{~A} formats using @code{display} and @code{~S} formats\n"
	    "using @code{write}.\n"
	    "If @var{destination} is @code{#t}, then use the current output\n"
	    "port, if @var{destination} is @code{#f}, then return a string\n"
	    "containing the formatted text. Does not add a trailing newline.")
#define FUNC_NAME s_scm_simple_format
{
  SCM port, answer = SCM_UNSPECIFIED;
  int fReturnString = 0;
  int writingp;
  size_t start, p, end;

  if (scm_is_eq (destination, SCM_BOOL_T))
    {
      destination = port = scm_current_output_port ();
      SCM_VALIDATE_OPORT_VALUE (1, destination);
    }
  else if (scm_is_false (destination))
    {
      fReturnString = 1;
      port = scm_mkstrport (SCM_INUM0, SCM_BOOL_F,
			    SCM_OPN | SCM_WRTNG,
			    FUNC_NAME);
      destination = port;
    }
  else
    {
      SCM_VALIDATE_OPORT_VALUE (1, destination);
      port = SCM_COERCE_OUTPORT (destination);
    }
  SCM_VALIDATE_STRING (2, message);
  SCM_VALIDATE_REST_ARGUMENT (args);

  p = 0;
  start = 0;
  end = scm_i_string_length (message);
  for (p = start; p != end; ++p)
    if (scm_i_string_ref (message, p) == '~')
      {
	if (++p == end)
	  break;

	switch (scm_i_string_ref (message, p)) 
	  {
	  case 'A': case 'a':
	    writingp = 0;
	    break;
	  case 'S': case 's':
	    writingp = 1;
	    break;
	  case '~':
	    scm_lfwrite_substr (message, start, p, port);
	    start = p + 1;
	    continue;
	  case '%':
	    scm_lfwrite_substr (message, start, p - 1, port);
	    scm_newline (port);
	    start = p + 1;
	    continue;
	  default:
	    SCM_MISC_ERROR ("FORMAT: Unsupported format option ~~~A - use (ice-9 format) instead",
			    scm_list_1 (SCM_MAKE_CHAR (scm_i_string_ref (message, p))));
	    
	  }


	if (!scm_is_pair (args))
	  SCM_MISC_ERROR ("FORMAT: Missing argument for ~~~A",
			  scm_list_1 (SCM_MAKE_CHAR (scm_i_string_ref (message, p))));
			  		
	scm_lfwrite_substr (message, start, p - 1, port);
	/* we pass destination here */
	scm_prin1 (SCM_CAR (args), destination, writingp);
	args = SCM_CDR (args);
	start = p + 1;
      }

  scm_lfwrite_substr (message, start, p, port);
  if (!scm_is_eq (args, SCM_EOL))
    SCM_MISC_ERROR ("FORMAT: ~A superfluous arguments",
		    scm_list_1 (scm_length (args)));

  if (fReturnString)
    answer = scm_strport_to_string (destination);

  return scm_return_first (answer, message);
}
#undef FUNC_NAME


SCM_DEFINE (scm_newline, "newline", 0, 1, 0, 
            (SCM port),
	    "Send a newline to @var{port}.\n"
	    "If @var{port} is omitted, send to the current output port.")
#define FUNC_NAME s_scm_newline
{
  if (SCM_UNBNDP (port))
    port = scm_current_output_port ();

  SCM_VALIDATE_OPORT_VALUE (1, port);

  scm_putc ('\n', SCM_COERCE_OUTPORT (port));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_write_char, "write-char", 1, 1, 0,
            (SCM chr, SCM port),
	    "Send character @var{chr} to @var{port}.")
#define FUNC_NAME s_scm_write_char
{
  if (SCM_UNBNDP (port))
    port = scm_current_output_port ();

  SCM_VALIDATE_CHAR (1, chr);
  SCM_VALIDATE_OPORT_VALUE (2, port);

  port = SCM_COERCE_OUTPORT (port);
  if (!display_character (SCM_CHAR (chr), port,
			  PORT_CONVERSION_HANDLER (port)))
    scm_encoding_error (__func__, errno,
			"cannot convert to output locale",
			port, chr);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/* Call back to Scheme code to do the printing of special objects
 * (like structs).  SCM_PRINTER_APPLY applies PROC to EXP and a smob
 * containing PORT and PSTATE.  This object can be used as the port for
 * display/write etc to continue the current print chain.  The REVEALED
 * field of PSTATE is set to true to indicate that the print state has
 * escaped to Scheme and thus has to be freed by the GC.
 */

scm_t_bits scm_tc16_port_with_ps;

/* Print exactly as the port itself would */

static int
port_with_ps_print (SCM obj, SCM port, scm_print_state *pstate)
{
  obj = SCM_PORT_WITH_PS_PORT (obj);
  return scm_ptobs[SCM_PTOBNUM (obj)].print (obj, port, pstate);
}

SCM
scm_printer_apply (SCM proc, SCM exp, SCM port, scm_print_state *pstate)
{
  pstate->revealed = 1;
  return scm_call_2 (proc, exp,
		     scm_i_port_with_print_state (port, pstate->handle));
}

SCM_DEFINE (scm_port_with_print_state, "port-with-print-state", 1, 1, 0, 
            (SCM port, SCM pstate),
	    "Create a new port which behaves like @var{port}, but with an\n"
	    "included print state @var{pstate}.  @var{pstate} is optional.\n"
	    "If @var{pstate} isn't supplied and @var{port} already has\n"
	    "a print state, the old print state is reused.")
#define FUNC_NAME s_scm_port_with_print_state
{
  SCM_VALIDATE_OPORT_VALUE (1, port);
  if (!SCM_UNBNDP (pstate))
    SCM_VALIDATE_PRINTSTATE (2, pstate);
  return scm_i_port_with_print_state (port, pstate);
}
#undef FUNC_NAME

SCM_DEFINE (scm_get_print_state, "get-print-state", 1, 0, 0, 
            (SCM port),
	    "Return the print state of the port @var{port}. If @var{port}\n"
	    "has no associated print state, @code{#f} is returned.")
#define FUNC_NAME s_scm_get_print_state
{
  if (SCM_PORT_WITH_PS_P (port))
    return SCM_PORT_WITH_PS_PS (port);
  if (SCM_OUTPUT_PORT_P (port))
    return SCM_BOOL_F;
  SCM_WRONG_TYPE_ARG (1, port);
}
#undef FUNC_NAME



void
scm_init_print ()
{
  SCM ps_type, ips_type;

  scm_gc_register_root (&print_state_pool);

  scm_gc_register_root (&scm_print_state_vtable);
  ps_type = scm_make_vtable (scm_from_latin1_string (SCM_PRINT_STATE_LAYOUT),
                          SCM_BOOL_F);
  scm_set_struct_vtable_name_x (ps_type, scm_from_latin1_symbol ("print-state"));
  scm_print_state_vtable = ps_type;

  scm_gc_register_root (&scm_internal_print_state_vtable);
  ips_type = scm_make_vtable
    (scm_from_latin1_string (SCM_INTERNAL_PRINT_STATE_LAYOUT), SCM_BOOL_F);
  scm_set_struct_vtable_name_x
    (ips_type, scm_from_latin1_symbol ("internal-print-state"));
  scm_internal_print_state_vtable = ips_type;

  internal_print_state_table = scm_make_weak_key_hash_table (SCM_UNDEFINED);

  /* Don't want to bind a wrapper class in GOOPS, so pass 0 as arg1. */
  scm_tc16_port_with_ps = scm_make_smob_type (0, 0);
  scm_set_smob_print (scm_tc16_port_with_ps, port_with_ps_print);

  compare_post_inserts_proc = scm_c_make_gsubr
    ("compare-post-inserts", 2, 0, 0, compare_post_inserts);

#include "libguile/print.x"

  scm_init_opts (scm_print_options, scm_print_opts);
  scm_print_opts[SCM_PRINT_HIGHLIGHT_PREFIX_I].val =
    SCM_UNPACK (scm_from_locale_string ("{"));
  scm_print_opts[SCM_PRINT_HIGHLIGHT_SUFFIX_I].val =
    SCM_UNPACK (scm_from_locale_string ("}"));
  scm_print_opts[SCM_PRINT_KEYWORD_STYLE_I].val = SCM_UNPACK (sym_reader);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
