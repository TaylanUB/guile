/* Copyright 2017-2018,2021
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

#include "alist.h"
#include "eval.h"
#include "gsubr.h"
#include "keywords.h"
#include "modules.h"
#include "pairs.h"
#include "ports.h"
#include "srcprop.h"
#include "threads.h"
#include "variable.h"
#include "vectors.h"

#include "syntax.h"




/* The source field was added to syntax objects in Guile 3.0.6.  However
   there can be older syntax objects present in compiled files that
   don't have the source field.  If a syntax object has a source field,
   its tag will have HAS_SOURCE_WORD_FLAG set.  */
#define HAS_SOURCE_WORD_FLAG 0x100

enum
{
  TAG_WORD,
  EXPR_WORD,
  WRAP_WORD,
  MODULE_WORD,
  SOURCE_WORD,
  WORD_COUNT
};

static int
scm_is_syntax (SCM x)
{
  return SCM_HAS_TYP7 (x, scm_tc7_syntax);
}

#define SCM_VALIDATE_SYNTAX(pos, scm) \
  SCM_I_MAKE_VALIDATE_MSG2 (pos, scm, scm_is_syntax, "syntax object")

SCM_DEFINE (scm_syntax_p, "syntax?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if the argument @var{obj} is a syntax object,\n"
            "else @code{#f}.")
#define FUNC_NAME s_scm_syntax_p
{
  return scm_from_bool (scm_is_syntax (obj));
}
#undef FUNC_NAME

static SCM
sourcev_to_props (SCM v)
{
  SCM props = scm_acons (scm_sym_line, scm_c_vector_ref (v, 1),
                         scm_acons (scm_sym_column, scm_c_vector_ref (v, 2),
                                    SCM_EOL));
  if (scm_is_true (scm_c_vector_ref (v, 0)))
    props = scm_acons (scm_sym_filename, scm_c_vector_ref (v, 0), props);
  return props;
}

static SCM
props_to_sourcev (SCM props)
{
  SCM v = scm_c_make_vector (3, SCM_BOOL_F);
  scm_c_vector_set_x (v, 0, scm_assq_ref (props, scm_sym_filename));
  scm_c_vector_set_x (v, 1, scm_assq_ref (props, scm_sym_line));
  scm_c_vector_set_x (v, 2, scm_assq_ref (props, scm_sym_column));
  return v;
}

SCM_DEFINE (scm_make_syntax, "make-syntax", 3, 1, 0,
	    (SCM exp, SCM wrap, SCM module, SCM source),
	    "Make a new syntax object.")
#define FUNC_NAME s_scm_make_syntax
{
  if (SCM_UNBNDP (source))
    source = scm_source_properties (exp);
  if (scm_is_pair (source))
    source = props_to_sourcev (source);
  if (!scm_is_vector (source))
    source = SCM_BOOL_F;

  SCM ret = scm_words (scm_tc7_syntax | HAS_SOURCE_WORD_FLAG, WORD_COUNT);
  SCM_SET_CELL_OBJECT (ret, EXPR_WORD, exp);
  SCM_SET_CELL_OBJECT (ret, WRAP_WORD, wrap);
  SCM_SET_CELL_OBJECT (ret, MODULE_WORD, module);
  SCM_SET_CELL_OBJECT (ret, SOURCE_WORD, source);

  return ret;
}
#undef FUNC_NAME

SCM_DEFINE (scm_syntax_expression, "syntax-expression", 1, 0, 0,
	    (SCM obj),
	    "Return the expression contained in the syntax object @var{obj}.")
#define FUNC_NAME s_scm_syntax_expression
{
  SCM_VALIDATE_SYNTAX (1, obj);
  return SCM_CELL_OBJECT (obj, EXPR_WORD);
}
#undef FUNC_NAME

SCM_DEFINE (scm_syntax_wrap, "syntax-wrap", 1, 0, 0,
	    (SCM obj),
	    "Return the wrap contained in the syntax object @var{obj}.")
#define FUNC_NAME s_scm_syntax_wrap
{
  SCM_VALIDATE_SYNTAX (1, obj);
  return SCM_CELL_OBJECT (obj, WRAP_WORD);
}
#undef FUNC_NAME

SCM_DEFINE (scm_syntax_module, "syntax-module", 1, 0, 0,
	    (SCM obj),
	    "Return the module info contained in the syntax object @var{obj}.")
#define FUNC_NAME s_scm_syntax_module
{
  SCM_VALIDATE_SYNTAX (1, obj);
  return SCM_CELL_OBJECT (obj, MODULE_WORD);
}
#undef FUNC_NAME

SCM_DEFINE (scm_syntax_source, "syntax-source", 1, 0, 0,
	    (SCM obj),
	    "Return the source properties for syntax object @var{obj}, as\n"
            "an alist possibly containing the keys @code{filename},\n"
            "@code{line}, and @code{column}.  Return @code{#f} if no\n"
            "source properties are available.")
#define FUNC_NAME s_scm_syntax_source
{
  SCM_VALIDATE_SYNTAX (1, obj);
  if (!(SCM_CELL_WORD (obj, TAG_WORD) & HAS_SOURCE_WORD_FLAG))
    return SCM_BOOL_F;
  SCM src = SCM_CELL_OBJECT (obj, SOURCE_WORD);
  if (scm_is_vector (src))
    src = sourcev_to_props (src);
  return src;
}
#undef FUNC_NAME

SCM_DEFINE (scm_syntax_sourcev, "syntax-sourcev", 1, 0, 0,
	    (SCM obj),
	    "Return the source location information for syntax object\n"
            "@var{obj}, as a vector of @code{#(@var{filename} @var{line}\n"
            "@var{column})}, or @code{#f} if no source properties are\n"
            "available.")
#define FUNC_NAME s_scm_syntax_sourcev
{
  SCM_VALIDATE_SYNTAX (1, obj);
  if (!(SCM_CELL_WORD (obj, TAG_WORD) & HAS_SOURCE_WORD_FLAG))
    return SCM_BOOL_F;
  SCM src = SCM_CELL_OBJECT (obj, SOURCE_WORD);
  if (scm_is_null (src) || scm_is_pair (src))
    src = props_to_sourcev (src);
  return src;
}
#undef FUNC_NAME

static SCM print_syntax_var;

static void
init_print_syntax_var (void)
{
  print_syntax_var =
    scm_c_private_variable ("system syntax", "print-syntax");
}

void
scm_i_syntax_print (SCM obj, SCM port, scm_print_state *pstate)
{
  static scm_i_pthread_once_t once = SCM_I_PTHREAD_ONCE_INIT;
  scm_i_pthread_once (&once, init_print_syntax_var);
  scm_call_2 (scm_variable_ref (print_syntax_var), obj, port);
}

void
scm_init_syntax ()
{
#include "syntax.x"
}

