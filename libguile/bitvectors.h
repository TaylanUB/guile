#ifndef SCM_BITVECTORS_H
#define SCM_BITVECTORS_H

/* Copyright 1995-1997,1999-2001,2004,2006,2008-2009,2014,2018,2020
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



/* Bitvectors. Exciting stuff, maybe!
 */


/** Bit vectors */

SCM_API SCM scm_list_to_bitvector (SCM list);
SCM_API SCM scm_bitvector_to_list (SCM vec);

SCM_API SCM scm_bitvector_position (SCM v, SCM item, SCM start);

SCM_API int scm_is_bitvector (SCM obj);
SCM_API SCM scm_c_make_bitvector (size_t len, SCM fill);
SCM_API size_t scm_c_bitvector_length (SCM vec);
SCM_API size_t scm_c_bitvector_count (SCM v);
SCM_API int scm_c_bitvector_bit_is_set (SCM vec, size_t idx);
SCM_API int scm_c_bitvector_bit_is_clear (SCM vec, size_t idx);
SCM_API void scm_c_bitvector_set_bit_x (SCM vec, size_t idx);
SCM_API void scm_c_bitvector_clear_bit_x (SCM vec, size_t idx);
SCM_API void scm_c_bitvector_set_bits_x (SCM v, SCM bits);
SCM_API void scm_c_bitvector_clear_bits_x (SCM v, SCM bits);
SCM_API void scm_c_bitvector_set_all_bits_x (SCM vec);
SCM_API void scm_c_bitvector_clear_all_bits_x (SCM vec);
SCM_API void scm_c_bitvector_flip_all_bits_x (SCM vec);
SCM_API size_t scm_c_bitvector_count_bits (SCM v, SCM bits);

SCM_API const uint32_t *scm_array_handle_bit_elements (scm_t_array_handle *h);
SCM_API uint32_t *scm_array_handle_bit_writable_elements (scm_t_array_handle *h);
SCM_API size_t scm_array_handle_bit_elements_offset (scm_t_array_handle *h);
SCM_API const uint32_t *scm_bitvector_elements (SCM vec,
						    scm_t_array_handle *h,
						    size_t *offp,
						    size_t *lenp,
						    ssize_t *incp);
SCM_API uint32_t *scm_bitvector_writable_elements (SCM vec, 
						       scm_t_array_handle *h,
						       size_t *offp,
						       size_t *lenp,
						       ssize_t *incp);

SCM_INTERNAL uint32_t *scm_i_bitvector_bits (SCM vec);
SCM_INTERNAL int scm_i_is_mutable_bitvector (SCM vec);
SCM_INTERNAL int scm_i_print_bitvector (SCM vec, SCM port, scm_print_state *pstate);
SCM_INTERNAL SCM scm_i_bitvector_equal_p (SCM vec1, SCM vec2);
SCM_INTERNAL void scm_init_bitvectors (void);

#endif  /* SCM_BITVECTORS_H */
