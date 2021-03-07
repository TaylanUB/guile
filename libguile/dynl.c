/* dynl.c - dynamic linking

   Copyright 1990-2003,2008-2011,2017-2018,2021
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


/* "dynl.c" dynamically link&load object files.
   Author: Aubrey Jaffer
   Modified for libguile by Marius Vollmer */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#ifdef __MINGW32__
#include "posix-w32.h"
#define dlopen(_nam,_flg) (dlopen_w32((_nam),(_flg)))
#define dlsym(_hndl,_nam) (dlsym_w32((_hndl),(_nam)))
#define dlclose(_hndl)    (dlclose_w32((_hndl)))
#define dlerror()         (dlerror_w32())
#else
#include <dlfcn.h>
#endif

#include "boolean.h"
#include "deprecation.h"
#include "eval.h"
#include "extensions.h"
#include "foreign.h"
#include "gsubr.h"
#include "list.h"
#include "modules.h"
#include "numbers.h"
#include "strings.h"
#include "threads.h"
#include "variable.h"
#include "version.h"

#include "dynl.h"


static SCM
dlerror_string (const char *fallback)
{
  const char *message = dlerror ();
  if (message)
    return scm_from_locale_string (message);
  return scm_from_utf8_string ("Unknown error");
}

SCM_DEFINE_STATIC (scm_dlopen, "dlopen", 2, 0, 0, (SCM name, SCM flags), "")
#define FUNC_NAME s_scm_dlopen
{
  void *handle;
  int c_flags = scm_to_int (flags);

  if (scm_is_false (name))
    handle = dlopen (NULL, c_flags);
  else
    {
      char *c_name = scm_to_locale_string (name);
      handle = dlopen (c_name, c_flags);
      free (c_name);
    }

  if (!handle) {
    SCM message = dlerror_string ("Unknown error while opening module");
    SCM_MISC_ERROR ("file ~S, message ~S", scm_list_2 (name, message));
  }

  return scm_from_pointer (handle, NULL);
}
#undef FUNC_NAME

SCM_DEFINE_STATIC (scm_dlclose, "dlclose", 1, 0, 0, (SCM obj), "")
#define FUNC_NAME s_scm_dlclose
{
  void *handle = scm_to_pointer (obj);

  if (dlclose (handle) != 0) {
    SCM message = dlerror_string ("Unknown error");
    SCM_MISC_ERROR ("Error closing module: ~S", scm_list_1 (message));
  }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE_STATIC (scm_dlsym, "dlsym", 2, 0, 0, (SCM obj, SCM name), "")
#define FUNC_NAME s_scm_dlsym
{
  void *handle = scm_to_pointer (obj);
  char *c_name = scm_to_utf8_string (name);

  void *sym = dlsym (handle, c_name);
  free (c_name);

  if (!sym) {
    SCM message = dlerror_string ("Unknown error");
    SCM_MISC_ERROR ("Error resolving ~S: ~S", scm_list_2 (name, message));
  }

  return scm_from_pointer (sym, NULL);
}
#undef FUNC_NAME

#define DEFINE_LAZY_VAR(c_name, mod_name, sym_name)                     \
  static SCM c_name##_var;                                              \
  static void init_##c_name##_var (void)                                \
  {                                                                     \
    c_name##_var = scm_c_public_lookup (mod_name, sym_name);            \
  }                                                                     \
  static SCM c_name (void)                                              \
  {                                                                     \
    static scm_i_pthread_once_t once = SCM_I_PTHREAD_ONCE_INIT;         \
    scm_i_pthread_once (&once, init_##c_name##_var);                    \
    return scm_variable_ref (c_name##_var);                             \
  }

DEFINE_LAZY_VAR (load_foreign_library,
                 "system foreign-library", "load-foreign-library");
DEFINE_LAZY_VAR (foreign_library_p,
                 "system foreign-library", "foreign-library?");
DEFINE_LAZY_VAR (foreign_library_pointer,
                 "system foreign-library", "foreign-library-pointer");

SCM
scm_dynamic_link (SCM filename)
{
  return scm_call_1 (load_foreign_library (), filename);
}

SCM
scm_dynamic_object_p (SCM obj)
{
  return scm_call_1 (foreign_library_p (), obj);
}

SCM
scm_dynamic_pointer (SCM name, SCM obj)
{
  return scm_call_2 (foreign_library_pointer (), obj, name);
}

SCM
scm_dynamic_func (SCM name, SCM obj)
{
  return scm_dynamic_pointer (name, obj);
}

SCM
scm_dynamic_call (SCM name, SCM obj)
{
  SCM pointer = scm_dynamic_pointer (name, obj);
  void (*f)(void) = SCM_POINTER_VALUE (pointer);
  f();
  return SCM_UNSPECIFIED;
}

static void
scm_init_system_foreign_library (void *unused)
{
  scm_c_define ("RTLD_LAZY", scm_from_int (RTLD_LAZY));
  scm_c_define ("RTLD_NOW", scm_from_int (RTLD_NOW));
  scm_c_define ("RTLD_GLOBAL", scm_from_int (RTLD_GLOBAL));
  scm_c_define ("RTLD_LOCAL", scm_from_int (RTLD_LOCAL));

#include "dynl.x"
}

void
scm_init_dynamic_linking ()
{
  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_system_foreign_library",
                            scm_init_system_foreign_library,
			    NULL);

  // FIXME: Deprecate all of these, once (system foreign-library) has
  // had enough time in the world.
  scm_c_define_gsubr
    ("dynamic-link", 0, 1, 0, (scm_t_subr) scm_dynamic_link);
  scm_c_define_gsubr
    ("dynamic-object?", 1, 0, 0, (scm_t_subr) scm_dynamic_object_p);
  scm_c_define_gsubr
    ("dynamic-func", 2, 0, 0, (scm_t_subr) scm_dynamic_func);
  scm_c_define_gsubr
    ("dynamic-pointer", 2, 0, 0, (scm_t_subr) scm_dynamic_pointer);
  scm_c_define_gsubr
    ("dynamic-call", 2, 0, 0, (scm_t_subr) scm_dynamic_call);
}
