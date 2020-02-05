#ifndef SCM_SRFI_4_H
#define SCM_SRFI_4_H
/* srfi-4.c --- Homogeneous numeric vector datatypes.

   Copyright 2001,2004,2006,2008-2011,2014,2018,2021
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


#include "libguile/array-handle.h"

SCM_API SCM scm_make_srfi_4_vector (SCM type, SCM len, SCM fill);

/* Specific procedures.
 */

#define SCM_SRFI4_DECL(tag, type)                                       \
  SCM_API SCM scm_##tag##vector_p (SCM obj);                            \
  SCM_API SCM scm_make_##tag##vector (SCM n, SCM fill);                 \
  SCM_API SCM scm_take_##tag##vector (type *data, size_t n);            \
  SCM_API SCM scm_##tag##vector (SCM l);                                \
  SCM_API SCM scm_##tag##vector_length (SCM uvec);                      \
  SCM_API SCM scm_##tag##vector_ref (SCM uvec, SCM index);              \
  SCM_API SCM scm_##tag##vector_set_x (SCM uvec, SCM index, SCM value); \
  SCM_API SCM scm_##tag##vector_to_list (SCM uvec);                     \
  SCM_API SCM scm_list_to_##tag##vector (SCM l);                        \
  SCM_API SCM scm_any_to_##tag##vector (SCM obj);                       \
  SCM_API const type *scm_array_handle_##tag##_elements (scm_t_array_handle *h); \
  SCM_API type *scm_array_handle_##tag##_writable_elements (scm_t_array_handle *h); \
  SCM_API const type *scm_##tag##vector_elements (SCM uvec, scm_t_array_handle *h, size_t *lenp, ssize_t *incp); \
  SCM_API type *scm_##tag##vector_writable_elements (SCM uvec, scm_t_array_handle *h, size_t *lenp, ssize_t *incp);

SCM_SRFI4_DECL (u8, uint8_t)
SCM_SRFI4_DECL (s8, int8_t)
SCM_SRFI4_DECL (u16, uint16_t)
SCM_SRFI4_DECL (s16, int16_t)
SCM_SRFI4_DECL (u32, uint32_t)
SCM_SRFI4_DECL (s32, int32_t)
SCM_SRFI4_DECL (u64, uint64_t)
SCM_SRFI4_DECL (s64, int64_t)
SCM_SRFI4_DECL (f32, float)
SCM_SRFI4_DECL (f64, double)
SCM_SRFI4_DECL (c32, float)
SCM_SRFI4_DECL (c64, double)

#undef SCM_SRFI4_DECL

SCM_INTERNAL void scm_init_srfi_4 (void);

#endif /* SCM_SRFI_4_H */
