/* Copyright 1995-2002,2006,2008-2012,2018,2020
     Free Software Foundation, Inc.

   This file is part of Guile.

   Guile is free software: you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Guile is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
   License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with Guile.  If not, see
   <https://www.gnu.org/licenses/>.  */




#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <errno.h>

#include "alist.h"
#include "async.h"
#include "debug.h"
#include "gc.h"
#include "gsubr.h"
#include "hash.h"
#include "hashtab.h"
#include "keywords.h"
#include "list.h"
#include "modules.h"
#include "numbers.h"
#include "pairs.h"
#include "ports.h"
#include "private-options.h"
#include "smob.h"
#include "symbols.h"
#include "weak-table.h"

#include "srcprop.h"



/* {Source Properties}
 *
 * Properties of source list expressions.
 * Three of these have special meaning:
 *
 * filename    The name of the source file.
 * line	       The source code line number.
 * column      The source code column number.
 *
 * Most properties above can be set by the reader.
 *
 */

SCM_GLOBAL_SYMBOL (scm_sym_filename, "filename");
SCM_GLOBAL_SYMBOL (scm_sym_line, "line");
SCM_GLOBAL_SYMBOL (scm_sym_column, "column");

static SCM scm_source_whash;


/*
 *  Source properties are stored as double cells with the
 *  following layout:
  
 * car = tag | col (untagged)
 * cbr = line
 * ccr = filename
 * cdr = alist
 */

static scm_t_bits tc16_srcprops;

#define SRCPROPSP(p) (SCM_SMOB_PREDICATE (tc16_srcprops, (p)))
#define SRCPROPCOL(p) (scm_from_int (SCM_SMOB_FLAGS (p)))
#define SRCPROPLINE(p) (SCM_SMOB_OBJECT_1 (p))
#define SRCPROPFNAME(p) (SCM_SMOB_OBJECT_2 (p))
#define SRCPROPALIST(p) (SCM_SMOB_OBJECT_3 (p))
#define SETSRCPROPCOL(p, c) (SCM_SET_SMOB_FLAGS (p, scm_to_int (c)))
#define SETSRCPROPLINE(p, l) (SCM_SET_SMOB_OBJECT_1 (p, l))
#define SETSRCPROPFNAME(p, x) (SCM_SET_SMOB_OBJECT_2 (p, x))
#define SETSRCPROPALIST(p, x) (SCM_SET_SMOB_OBJECT_3 (p, x))


static SCM scm_srcprops_to_alist (SCM obj);



static int
supports_source_props (SCM obj)
{
  return SCM_NIMP (obj) && !scm_is_symbol (obj) && !scm_is_keyword (obj);
}


static int
srcprops_print (SCM obj, SCM port, scm_print_state *pstate)
{
  int writingp = SCM_WRITINGP (pstate);
  scm_puts ("#<srcprops ", port);
  SCM_SET_WRITINGP (pstate, 1);
  scm_iprin1 (scm_srcprops_to_alist (obj), port, pstate);
  SCM_SET_WRITINGP (pstate, writingp);
  scm_putc ('>', port);
  return 1;
}


SCM
scm_i_make_srcprops (SCM line, SCM col, SCM filename, SCM alist)
{
  SCM_RETURN_NEWSMOB3 (tc16_srcprops | (scm_to_int (col) << 16),
                       SCM_UNPACK (line),
                       SCM_UNPACK (filename),
                       SCM_UNPACK (alist));
}

static SCM
scm_srcprops_to_alist (SCM obj)
{
  SCM alist = SRCPROPALIST (obj);
  if (scm_is_true (SRCPROPFNAME (obj)))
    alist = scm_acons (scm_sym_filename, SRCPROPFNAME (obj), alist);
  alist = scm_acons (scm_sym_column, SRCPROPCOL (obj), alist);
  alist = scm_acons (scm_sym_line, SRCPROPLINE (obj), alist);
  return alist;
}

SCM_DEFINE (scm_supports_source_properties_p, "supports-source-properties?", 1, 0, 0,
            (SCM obj),
            "Return #t if @var{obj} supports adding source properties,\n"
            "otherwise return #f.")
#define FUNC_NAME s_scm_supports_source_properties_p
{
  return scm_from_bool (supports_source_props (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_source_properties, "source-properties", 1, 0, 0, 
            (SCM obj),
	    "Return the source property association list of @var{obj}.")
#define FUNC_NAME s_scm_source_properties
{
  if (SCM_IMP (obj))
    return SCM_EOL;
  else
    {
      SCM p = scm_weak_table_refq (scm_source_whash, obj, SCM_EOL); 

      if (SRCPROPSP (p))
        return scm_srcprops_to_alist (p);
      else
        /* list from set-source-properties!, or SCM_EOL for not found */
        return p;
    }
}
#undef FUNC_NAME

#define SCM_VALIDATE_NIM(pos, scm) \
  SCM_MAKE_VALIDATE_MSG (pos, scm, NIMP, "non-immediate")

/* Perhaps this procedure should look through an alist
   and try to make a srcprops-object...? */
SCM_DEFINE (scm_set_source_properties_x, "set-source-properties!", 2, 0, 0,
            (SCM obj, SCM alist),
	    "Install the association list @var{alist} as the source property\n"
	    "list for @var{obj}.")
#define FUNC_NAME s_scm_set_source_properties_x
{
  SCM_VALIDATE_NIM (1, obj);

  scm_weak_table_putq_x (scm_source_whash, obj, alist);

  return alist;
}
#undef FUNC_NAME

int
scm_i_has_source_properties (SCM obj)
#define FUNC_NAME "%set-source-properties"
{
  if (SCM_IMP (obj))
    return 0;
  else
    return scm_is_true (scm_weak_table_refq (scm_source_whash, obj, SCM_BOOL_F));
}
#undef FUNC_NAME
  

void
scm_i_set_source_properties_x (SCM obj, SCM line, SCM col, SCM fname)
#define FUNC_NAME "%set-source-properties"
{
  SCM_VALIDATE_NIM (1, obj);

  scm_weak_table_putq_x (scm_source_whash, obj,
                         scm_i_make_srcprops (line, col, fname, SCM_EOL));
}
#undef FUNC_NAME

SCM_DEFINE (scm_source_property, "source-property", 2, 0, 0,
            (SCM obj, SCM key),
	    "Return the source property specified by @var{key} from\n"
	    "@var{obj}'s source property list.")
#define FUNC_NAME s_scm_source_property
{
  SCM p;

  if (SCM_IMP (obj))
    return SCM_BOOL_F;

  p = scm_weak_table_refq (scm_source_whash, obj, SCM_EOL);

  if (!SRCPROPSP (p))
    goto alist;
  if (scm_is_eq (scm_sym_line, key))
    return SRCPROPLINE (p);
  else if (scm_is_eq (scm_sym_column, key))
    return SRCPROPCOL (p);
  else if (scm_is_eq (scm_sym_filename, key))
    return SRCPROPFNAME (p);
  else
    {
      p = SRCPROPALIST (p);
    alist:
      p = scm_assoc (key, p);
      return (scm_is_pair (p) ? SCM_CDR (p) : SCM_BOOL_F);
    }
}
#undef FUNC_NAME

static scm_i_pthread_mutex_t source_mutex = SCM_I_PTHREAD_MUTEX_INITIALIZER;

SCM_DEFINE (scm_set_source_property_x, "set-source-property!", 3, 0, 0,
            (SCM obj, SCM key, SCM datum),
	    "Set the source property of object @var{obj}, which is specified by\n"
	    "@var{key} to @var{datum}.  Normally, the key will be a symbol.")
#define FUNC_NAME s_scm_set_source_property_x
{
  SCM p;
  SCM_VALIDATE_NIM (1, obj);

  scm_i_pthread_mutex_lock (&source_mutex);
  p = scm_weak_table_refq (scm_source_whash, obj, SCM_EOL);

  if (scm_is_eq (scm_sym_line, key))
    {
      if (SRCPROPSP (p))
	SETSRCPROPLINE (p, datum);
      else
	scm_weak_table_putq_x (scm_source_whash, obj,
                               scm_i_make_srcprops (datum, SCM_INUM0,
                                                    SCM_BOOL_F, p));
    }
  else if (scm_is_eq (scm_sym_column, key))
    {
      if (SRCPROPSP (p))
	SETSRCPROPCOL (p, datum);
      else
	scm_weak_table_putq_x (scm_source_whash, obj,
                               scm_i_make_srcprops (SCM_INUM0, datum,
                                                    SCM_BOOL_F, p));
    }
  else if (scm_is_eq (scm_sym_filename, key))
    {
      if (SRCPROPSP (p))
	SETSRCPROPFNAME (p, datum);
      else
	scm_weak_table_putq_x (scm_source_whash, obj,
                               scm_i_make_srcprops (SCM_INUM0, SCM_INUM0,
                                                    datum, p));
    }
  else
    {
      if (SRCPROPSP (p))
	SETSRCPROPALIST (p, scm_acons (key, datum, SRCPROPALIST (p)));
      else
	scm_weak_table_putq_x (scm_source_whash, obj,
                               scm_acons (key, datum, p));
    }
  scm_i_pthread_mutex_unlock (&source_mutex);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_cons_source, "cons-source", 3, 0, 0, 
            (SCM xorig, SCM x, SCM y),
	    "Create and return a new pair whose car and cdr are @var{x} and @var{y}.\n"
	    "Any source properties associated with @var{xorig} are also associated\n"
	    "with the new pair.")
#define FUNC_NAME s_scm_cons_source
{
  SCM p, z;
  z = scm_cons (x, y);
  /* Copy source properties possibly associated with xorig. */
  p = scm_weak_table_refq (scm_source_whash, xorig, SCM_BOOL_F);
  if (scm_is_true (p))
    scm_weak_table_putq_x (scm_source_whash, z, p);
  return z;
}
#undef FUNC_NAME


void
scm_init_srcprop ()
{
  tc16_srcprops = scm_make_smob_type ("srcprops", 0);
  scm_set_smob_print (tc16_srcprops, srcprops_print);

  scm_source_whash = scm_c_make_weak_table (0, SCM_WEAK_TABLE_KIND_KEY);
  scm_c_define ("source-whash", scm_source_whash);

#include "srcprop.x"
}

