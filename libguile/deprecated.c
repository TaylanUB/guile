/* Copyright 2003-2004,2006,2008-2018,2020
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

#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define SCM_BUILDING_DEPRECATED_CODE

#include "bitvectors.h"
#include "deprecation.h"
#include "gc.h"
#include "strings.h"

#include "deprecated.h"

#if (SCM_ENABLE_DEPRECATED == 1)



#ifndef MAXPATHLEN
#define MAXPATHLEN 80
#endif /* ndef MAXPATHLEN */
#ifndef X_OK
#define X_OK 1
#endif /* ndef X_OK */

char *
scm_find_executable (const char *name)
{
  char tbuf[MAXPATHLEN];
  int i = 0, c;
  FILE *f;

  scm_c_issue_deprecation_warning ("scm_find_executable is deprecated.");

  /* fprintf(stderr, "s_f_e checking access %s ->%d\n", name, access(name, X_OK)); fflush(stderr); */
  if (access (name, X_OK))
    return 0L;
  f = fopen (name, "r");
  if (!f)
    return 0L;
  if ((fgetc (f) == '#') && (fgetc (f) == '!'))
    {
      while (1)
	switch (c = fgetc (f))
	  {
	  case /*WHITE_SPACES */ ' ':
	  case '\t':
	  case '\r':
	  case '\f':
	  case EOF:
	    tbuf[i] = 0;
	    fclose (f);
	    return strdup (tbuf);
	  default:
	    tbuf[i++] = c;
	    break;
	  }
    }
  fclose (f);
  return strdup (name);
}




SCM
scm_istr2bve (SCM str)
{
  scm_t_array_handle handle;
  size_t len = scm_i_string_length (str);
  SCM vec = scm_c_make_bitvector (len, SCM_UNDEFINED);
  SCM res = vec;

  uint32_t mask;
  size_t k, j;
  const char *c_str;
  uint32_t *data;

  scm_c_issue_deprecation_warning
    ("scm_istr2bve is deprecated.  "
     "Read from a string instead, prefixed with `#*'.");

  data = scm_bitvector_writable_elements (vec, &handle, NULL, NULL, NULL);
  c_str = scm_i_string_chars (str);

  for (k = 0; k < (len + 31) / 32; k++)
    {
      data[k] = 0L;
      j = len - k * 32;
      if (j > 32)
	j = 32;
      for (mask = 1L; j--; mask <<= 1)
	switch (*c_str++)
	  {
	  case '0':
	    break;
	  case '1':
	    data[k] |= mask;
	    break;
	  default:
	    res = SCM_BOOL_F;
	    goto exit;
	  }
    }
  
 exit:
  scm_array_handle_release (&handle);
  scm_remember_upto_here_1 (str);
  return res;
}




void
scm_i_init_deprecated ()
{
#include "deprecated.x"
}

#endif /* SCM_ENABLE_DEPRECATD == 1 */
