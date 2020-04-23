/* Copyright 1995-1998,2000-2006,2009-2014,2018,2020
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
#  include <config.h>
#endif

#include <string.h>

#include "array-handle.h"
#include "arrays.h"
#include "boolean.h"
#include "deprecation.h"
#include "generalized-vectors.h"
#include "gsubr.h"
#include "list.h"
#include "numbers.h"
#include "pairs.h"
#include "ports.h"
#include "srfi-4.h"

#include "bitvectors.h"


#define SCM_F_BITVECTOR_IMMUTABLE (0x80)

/* To do in Guile 3.1.x:
    - Allocate bits inline with bitvector, starting from &SCM_CELL_WORD_2.
    - Use uintptr_t for bitvector component instead of uint32_t.
    - Remove deprecated support for bitvector-ref et al on arrays.
    - Replace primitives that operator on bitvectors but don't have
      bitvector- prefix.
    - Add Scheme compiler support for bitvector primitives.  */
#define IS_BITVECTOR(obj)         SCM_HAS_TYP7  ((obj), scm_tc7_bitvector)
#define IS_MUTABLE_BITVECTOR(x)                                 \
  (SCM_NIMP (x) &&                                              \
   ((SCM_CELL_TYPE (x) & (0x7f | SCM_F_BITVECTOR_IMMUTABLE))    \
    == scm_tc7_bitvector))
#define BITVECTOR_LENGTH(obj)   ((size_t)SCM_CELL_WORD_1(obj))
#define BITVECTOR_BITS(obj)     ((uint32_t *)SCM_CELL_WORD_2(obj))

#define VALIDATE_BITVECTOR(_pos, _obj)                                  \
  SCM_ASSERT_TYPE (IS_BITVECTOR (_obj), (_obj), (_pos), FUNC_NAME,      \
                   "bitvector")
#define VALIDATE_MUTABLE_BITVECTOR(_pos, _obj)                          \
  SCM_ASSERT_TYPE (IS_MUTABLE_BITVECTOR (_obj), (_obj), (_pos),         \
                   FUNC_NAME, "mutable bitvector")

uint32_t *
scm_i_bitvector_bits (SCM vec)
{
  if (!IS_BITVECTOR (vec))
    abort ();
  return BITVECTOR_BITS (vec);
}

int
scm_i_is_mutable_bitvector (SCM vec)
{
  return IS_MUTABLE_BITVECTOR (vec);
}

int
scm_i_print_bitvector (SCM vec, SCM port, scm_print_state *pstate)
{
  size_t bit_len = BITVECTOR_LENGTH (vec);
  size_t word_len = (bit_len+31)/32;
  uint32_t *bits = BITVECTOR_BITS (vec);
  size_t i, j;

  scm_puts ("#*", port);
  for (i = 0; i < word_len; i++, bit_len -= 32)
    {
      uint32_t mask = 1;
      for (j = 0; j < 32 && j < bit_len; j++, mask <<= 1)
	scm_putc ((bits[i] & mask)? '1' : '0', port);
    }
    
  return 1;
}

SCM
scm_i_bitvector_equal_p (SCM vec1, SCM vec2)
{
  size_t bit_len = BITVECTOR_LENGTH (vec1);
  size_t word_len = (bit_len + 31) / 32;
  uint32_t last_mask =  ((uint32_t)-1) >> (32*word_len - bit_len);
  uint32_t *bits1 = BITVECTOR_BITS (vec1);
  uint32_t *bits2 = BITVECTOR_BITS (vec2);

  /* compare lengths */
  if (BITVECTOR_LENGTH (vec2) != bit_len)
    return SCM_BOOL_F;
  /* avoid underflow in word_len-1 below. */
  if (bit_len == 0)
    return SCM_BOOL_T;
  /* compare full words */
  if (memcmp (bits1, bits2, sizeof (uint32_t) * (word_len-1)))
    return SCM_BOOL_F;
  /* compare partial last words */
  if ((bits1[word_len-1] & last_mask) != (bits2[word_len-1] & last_mask))
    return SCM_BOOL_F;
  return SCM_BOOL_T;
}

int
scm_is_bitvector (SCM vec)
{
  return IS_BITVECTOR (vec);
}

SCM_DEFINE_STATIC (bitvector_p, "bitvector?", 1, 0, 0,
                   (SCM obj),
                   "Return @code{#t} when @var{obj} is a bitvector, else\n"
                   "return @code{#f}.")
#define FUNC_NAME s_bitvector_p
{
  return scm_from_bool (scm_is_bitvector (obj));
}
#undef FUNC_NAME

SCM
scm_c_make_bitvector (size_t len, SCM fill)
{
  size_t word_len = (len + 31) / 32;
  uint32_t *bits;
  SCM res;

  bits = scm_gc_malloc_pointerless (sizeof (uint32_t) * word_len,
				    "bitvector");
  res = scm_double_cell (scm_tc7_bitvector, len, (scm_t_bits)bits, 0);

  if (SCM_UNBNDP (fill) || !scm_is_true (fill))
    scm_c_bitvector_clear_all_bits_x (res);
  else
    scm_c_bitvector_set_all_bits_x (res);
      
  return res;
}

SCM_DEFINE_STATIC (make_bitvector, "make-bitvector", 1, 1, 0,
                   (SCM len, SCM fill),
                   "Create a new bitvector of length @var{len} and\n"
                   "optionally initialize all elements to @var{fill}.")
#define FUNC_NAME s_make_bitvector
{
  return scm_c_make_bitvector (scm_to_size_t (len), fill);
}
#undef FUNC_NAME

SCM_DEFINE_STATIC (bitvector, "bitvector", 0, 0, 1,
                   (SCM bits),
                   "Create a new bitvector with the arguments as elements.")
#define FUNC_NAME s_bitvector
{
  return scm_list_to_bitvector (bits);
}
#undef FUNC_NAME

size_t
scm_c_bitvector_length (SCM vec)
{
  if (!IS_BITVECTOR (vec))
    scm_wrong_type_arg_msg (NULL, 0, vec, "bitvector");
  return BITVECTOR_LENGTH (vec);
}

SCM_DEFINE_STATIC (bitvector_length, "bitvector-length", 1, 0, 0,
                   (SCM vec),
                   "Return the length of the bitvector @var{vec}.")
#define FUNC_NAME s_bitvector_length
{
  return scm_from_size_t (scm_c_bitvector_length (vec));
}
#undef FUNC_NAME

const uint32_t *
scm_array_handle_bit_elements (scm_t_array_handle *h)
{
  if (h->element_type != SCM_ARRAY_ELEMENT_TYPE_BIT)
    scm_wrong_type_arg_msg (NULL, 0, h->array, "bit array");
  return ((const uint32_t *) h->elements) + h->base/32;
}

uint32_t *
scm_array_handle_bit_writable_elements (scm_t_array_handle *h)
{
  if (h->writable_elements != h->elements)
    scm_wrong_type_arg_msg (NULL, 0, h->array, "mutable bit array");
  return (uint32_t *) scm_array_handle_bit_elements (h);
}

size_t
scm_array_handle_bit_elements_offset (scm_t_array_handle *h)
{
  return h->base % 32;
}

const uint32_t *
scm_bitvector_elements (SCM vec,
			scm_t_array_handle *h,
			size_t *offp,
			size_t *lenp,
			ssize_t *incp)
{
  scm_array_get_handle (vec, h);
  if (1 != scm_array_handle_rank (h))
    {
      scm_array_handle_release (h);
      scm_wrong_type_arg_msg (NULL, 0, vec, "rank 1 bit array");
    }
  if (offp)
    {
      scm_t_array_dim *dim = scm_array_handle_dims (h);
      *offp = scm_array_handle_bit_elements_offset (h);
      *lenp = dim->ubnd - dim->lbnd + 1;
      *incp = dim->inc;
    }
  return scm_array_handle_bit_elements (h);
}


uint32_t *
scm_bitvector_writable_elements (SCM vec,
				 scm_t_array_handle *h,
				 size_t *offp,
				 size_t *lenp,
				 ssize_t *incp)
{
  const uint32_t *ret = scm_bitvector_elements (vec, h, offp, lenp, incp);

  if (h->writable_elements != h->elements)
    scm_wrong_type_arg_msg (NULL, 0, h->array, "mutable bit array");

  return (uint32_t *) ret;
}

int
scm_c_bitvector_bit_is_set (SCM vec, size_t idx)
#define FUNC_NAME "bitvector-bit-set?"
{
  VALIDATE_BITVECTOR (1, vec);
  if (idx >= BITVECTOR_LENGTH (vec))
    SCM_OUT_OF_RANGE (2, scm_from_size_t (idx));

  const uint32_t *bits = BITVECTOR_BITS (vec);
  return (bits[idx/32] & (1L << (idx%32))) ? 1 : 0;
}
#undef FUNC_NAME

int
scm_c_bitvector_bit_is_clear (SCM vec, size_t idx)
{
  return !scm_c_bitvector_bit_is_set (vec, idx);
}

SCM_DEFINE_STATIC (scm_bitvector_bit_set_p, "bitvector-bit-set?", 2, 0, 0,
                   (SCM vec, SCM idx),
                   "Return @code{#t} if the bit at index @var{idx} of the \n"
                   "bitvector @var{vec} is set, or @code{#f} otherwise.")
#define FUNC_NAME s_scm_bitvector_bit_set_p
{
  return scm_from_bool (scm_c_bitvector_bit_is_set (vec, scm_to_size_t (idx)));
}
#undef FUNC_NAME

SCM_DEFINE_STATIC (scm_bitvector_bit_clear_p, "bitvector-bit-clear?", 2, 0, 0,
                   (SCM vec, SCM idx),
                   "Return @code{#t} if the bit at index @var{idx} of the \n"
                   "bitvector @var{vec} is clear (unset), or @code{#f} otherwise.")
#define FUNC_NAME s_scm_bitvector_bit_clear_p
{
  return scm_from_bool
    (scm_c_bitvector_bit_is_clear (vec, scm_to_size_t (idx)));
}
#undef FUNC_NAME

void
scm_c_bitvector_set_bit_x (SCM vec, size_t idx)
#define FUNC_NAME "bitvector-set-bit!"
{
  VALIDATE_MUTABLE_BITVECTOR (1, vec);
  if (idx >= BITVECTOR_LENGTH (vec))
    SCM_OUT_OF_RANGE (2, scm_from_size_t (idx));

  uint32_t *bits = BITVECTOR_BITS (vec);
  uint32_t mask = 1L << (idx%32);
  bits[idx/32] |= mask;
}
#undef FUNC_NAME

void
scm_c_bitvector_clear_bit_x (SCM vec, size_t idx)
#define FUNC_NAME "bitvector-clear-bit!"
{
  VALIDATE_MUTABLE_BITVECTOR (1, vec);
  if (idx >= BITVECTOR_LENGTH (vec))
    SCM_OUT_OF_RANGE (2, scm_from_size_t (idx));

  uint32_t *bits = BITVECTOR_BITS (vec);
  uint32_t mask = 1L << (idx%32);
  bits[idx/32] &= ~mask;
}
#undef FUNC_NAME

SCM_DEFINE_STATIC (scm_bitvector_set_bit_x, "bitvector-set-bit!", 2, 0, 0,
                   (SCM vec, SCM idx),
                   "Set the element at index @var{idx} of the bitvector\n"
                   "@var{vec}.")
#define FUNC_NAME s_scm_bitvector_set_bit_x
{
  scm_c_bitvector_set_bit_x (vec, scm_to_size_t (idx));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE_STATIC (scm_bitvector_clear_bit_x, "bitvector-clear-bit!", 2, 0, 0,
                   (SCM vec, SCM idx),
                   "Clear the element at index @var{idx} of the bitvector\n"
                   "@var{vec}.")
#define FUNC_NAME s_scm_bitvector_set_bit_x
{
  scm_c_bitvector_clear_bit_x (vec, scm_to_size_t (idx));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_c_bitvector_set_all_bits_x (SCM bv)
#define FUNC_NAME "bitvector-set-all-bits!"
{
  VALIDATE_MUTABLE_BITVECTOR (1, bv);
  size_t len = BITVECTOR_LENGTH (bv);

  if (len > 0)
    {
      uint32_t *bits = BITVECTOR_BITS (bv);
      size_t word_len = (len + 31) / 32;
      uint32_t last_mask =  ((uint32_t)-1) >> (32*word_len - len);

      memset (bits, 0xFF, sizeof(uint32_t)*(word_len-1));
      bits[word_len-1] |= last_mask;
    }
}
#undef FUNC_NAME

void
scm_c_bitvector_clear_all_bits_x (SCM bv)
#define FUNC_NAME "bitvector-clear-all-bits!"
{
  VALIDATE_MUTABLE_BITVECTOR (1, bv);
  size_t len = BITVECTOR_LENGTH (bv);

  if (len > 0)
    {
      uint32_t *bits = BITVECTOR_BITS (bv);
      size_t word_len = (len + 31) / 32;
      uint32_t last_mask =  ((uint32_t)-1) >> (32*word_len - len);

      memset (bits, 0x00, sizeof(uint32_t)*(word_len-1));
      bits[word_len-1] &= ~last_mask;
    }
}
#undef FUNC_NAME

SCM_DEFINE_STATIC (scm_bitvector_set_all_bits_x,
                   "bitvector-set-all-bits!", 1, 0, 0, (SCM vec),
                   "Set all elements of the bitvector @var{vec}.")
{
  scm_c_bitvector_set_all_bits_x (vec);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE_STATIC (scm_bitvector_clear_all_bits_x,
                   "bitvector-clear-all-bits!", 1, 0, 0, (SCM vec),
                   "Clear all elements of the bitvector @var{vec}.")
{
  scm_c_bitvector_clear_all_bits_x (vec);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (scm_list_to_bitvector, "list->bitvector", 1, 0, 0,
	    (SCM list),
	    "Return a new bitvector initialized with the elements\n"
	    "of @var{list}.")
#define FUNC_NAME s_scm_list_to_bitvector
{
  size_t bit_len = scm_to_size_t (scm_length (list));
  SCM vec = scm_c_make_bitvector (bit_len, SCM_UNDEFINED);
  size_t word_len = (bit_len+31)/32;
  uint32_t *bits = BITVECTOR_BITS (vec);
  size_t i, j;

  for (i = 0; i < word_len && scm_is_pair (list); i++, bit_len -= 32)
    {
      uint32_t mask = 1;
      bits[i] = 0;
      for (j = 0; j < 32 && j < bit_len;
	   j++, mask <<= 1, list = SCM_CDR (list))
	if (scm_is_true (SCM_CAR (list)))
	  bits[i] |= mask;
    }

  return vec;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bitvector_to_list, "bitvector->list", 1, 0, 0,
	    (SCM vec),
	    "Return a new list initialized with the elements\n"
	    "of the bitvector @var{vec}.")
#define FUNC_NAME s_scm_bitvector_to_list
{
  SCM res = SCM_EOL;

  if (IS_BITVECTOR (vec))
    {
      const uint32_t *bits = BITVECTOR_BITS (vec);
      size_t len = BITVECTOR_LENGTH (vec);
      size_t word_len = (len + 31) / 32;

      for (size_t i = 0; i < word_len; i++, len -= 32)
	{
	  uint32_t mask = 1;
	  for (size_t j = 0; j < 32 && j < len; j++, mask <<= 1)
	    res = scm_cons ((bits[i] & mask)? SCM_BOOL_T : SCM_BOOL_F, res);
	}
    }
  else
    {
      scm_t_array_handle handle;
      size_t off, len;
      ssize_t inc;

      scm_bitvector_elements (vec, &handle, &off, &len, &inc);

      scm_c_issue_deprecation_warning
        ("Using bitvector->list on arrays is deprecated.  "
         "Use array->list instead.");

      for (size_t i = 0; i < len; i++)
	res = scm_cons (scm_array_handle_ref (&handle, i*inc), res);

      scm_array_handle_release (&handle);
  
    }

  return scm_reverse_x (res, SCM_EOL);
}
#undef FUNC_NAME

/* From mmix-arith.w by Knuth.

  Here's a fun way to count the number of bits in a tetrabyte.

  [This classical trick is called the ``Gillies--Miller method for
  sideways addition'' in {\sl The Preparation of Programs for an
  Electronic Digital Computer\/} by Wilkes, Wheeler, and Gill, second
  edition (Reading, Mass.:\ Addison--Wesley, 1957), 191--193. Some of
  the tricks used here were suggested by Balbir Singh, Peter
  Rossmanith, and Stefan Schwoon.]
*/

static size_t
count_ones (uint32_t x)
{
  x=x-((x>>1)&0x55555555);
  x=(x&0x33333333)+((x>>2)&0x33333333);
  x=(x+(x>>4))&0x0f0f0f0f;
  x=x+(x>>8);
  return (x+(x>>16)) & 0xff;
}

size_t
scm_c_bitvector_count (SCM bitvector)
#define FUNC_NAME "bitvector-count"
{
  VALIDATE_BITVECTOR (1, bitvector);

  size_t len = BITVECTOR_LENGTH (bitvector);

  if (len == 0)
    return 0;

  const uint32_t *bits = BITVECTOR_BITS (bitvector);
  size_t count = 0;

  size_t word_len = (len + 31) / 32;
  size_t i;
  for (i = 0; i < word_len-1; i++)
    count += count_ones (bits[i]);

  uint32_t last_mask =  ((uint32_t)-1) >> (32*word_len - len);
  count += count_ones (bits[i] & last_mask);

  return count;
}
#undef FUNC_NAME

SCM_DEFINE_STATIC (scm_bitvector_count, "bitvector-count", 1, 0, 0,
                   (SCM bitvector),
                   "Return the number of set bits in @var{bitvector}.")
#define FUNC_NAME s_scm_bitvector_count
{
  return scm_from_size_t (scm_c_bitvector_count (bitvector));
}
#undef FUNC_NAME

/* returns 32 for x == 0. 
*/
static size_t
find_first_one (uint32_t x)
{
  size_t pos = 0;
  /* do a binary search in x. */
  if ((x & 0xFFFF) == 0)
    x >>= 16, pos += 16;
  if ((x & 0xFF) == 0)
    x >>= 8, pos += 8;
  if ((x & 0xF) == 0)
    x >>= 4, pos += 4;
  if ((x & 0x3) == 0)
    x >>= 2, pos += 2;
  if ((x & 0x1) == 0)
    pos += 1;
  return pos;
}

SCM_DEFINE (scm_bitvector_position, "bitvector-position", 2, 1, 0,
            (SCM v, SCM bit, SCM start),
	    "Return the index of the first occurrence of @var{bit} in bit\n"
	    "vector @var{v}, starting from @var{start} (or zero if not given)\n."
	    "If there is no @var{bit} entry between @var{start} and the end of\n"
	    "@var{v}, then return @code{#f}.  For example,\n"
	    "\n"
	    "@example\n"
	    "(bitvector-position #*000101 #t)  @result{} 3\n"
	    "(bitvector-position #*0001111 #f 3) @result{} #f\n"
	    "@end example")
#define FUNC_NAME s_scm_bitvector_position
{
  VALIDATE_BITVECTOR (1, v);

  size_t len = BITVECTOR_LENGTH (v);
  int c_bit = scm_to_bool (bit);
  size_t first_bit =
    SCM_UNBNDP (start) ? 0 : scm_to_unsigned_integer (start, 0, len);
  
  if (first_bit == len)
    return SCM_BOOL_F;

  const uint32_t *bits = BITVECTOR_BITS (v);
  size_t word_len = (len + 31) / 32;
  uint32_t last_mask =  ((uint32_t)-1) >> (32*word_len - len);
  size_t first_word = first_bit / 32;
  uint32_t first_mask =
    ((uint32_t)-1) << (first_bit - 32*first_word);
      
  for (size_t i = first_word; i < word_len; i++)
    {
      uint32_t w = c_bit ? bits[i] : ~bits[i];
      if (i == first_word)
        w &= first_mask;
      if (i == word_len-1)
        w &= last_mask;
      if (w)
        return scm_from_size_t (32*i + find_first_one (w));
    }

  return SCM_BOOL_F;
}
#undef FUNC_NAME

void
scm_c_bitvector_set_bits_x (SCM v, SCM bits)
#define FUNC_NAME "bitvector-set-bits!"
{
  VALIDATE_MUTABLE_BITVECTOR (1, v);
  VALIDATE_BITVECTOR (2, bits);
  size_t v_len = BITVECTOR_LENGTH (v);
  uint32_t *v_bits = BITVECTOR_BITS (v);
  size_t kv_len = BITVECTOR_LENGTH (bits);
  const uint32_t *kv_bits = BITVECTOR_BITS (bits);

  if (v_len < kv_len)
    scm_misc_error (NULL,
                    "selection bitvector longer than target bitvector",
                    SCM_EOL);
      
  if (kv_len > 0)
    {
      size_t word_len = (kv_len + 31) / 32;
      uint32_t last_mask = ((uint32_t)-1) >> (32*word_len - kv_len);
      size_t i;
      for (i = 0; i < word_len-1; i++)
        v_bits[i] |= kv_bits[i];
      v_bits[i] |= kv_bits[i] & last_mask;
    }
}
#undef FUNC_NAME

void
scm_c_bitvector_clear_bits_x (SCM v, SCM bits)
#define FUNC_NAME "bitvector-clear-bits!"
{
  VALIDATE_MUTABLE_BITVECTOR (1, v);
  VALIDATE_BITVECTOR (2, bits);
  size_t v_len = BITVECTOR_LENGTH (v);
  uint32_t *v_bits = BITVECTOR_BITS (v);
  size_t kv_len = BITVECTOR_LENGTH (bits);
  const uint32_t *kv_bits = BITVECTOR_BITS (bits);

  if (v_len < kv_len)
    scm_misc_error (NULL,
                    "selection bitvector longer than target bitvector",
                    SCM_EOL);
      
  if (kv_len > 0)
    {
      size_t word_len = (kv_len + 31) / 32;
      uint32_t last_mask = ((uint32_t)-1) >> (32*word_len - kv_len);
      size_t i;

      for (i = 0; i < word_len-1; i++)
        v_bits[i] &= ~kv_bits[i];
      v_bits[i] &= ~(kv_bits[i] & last_mask);
    }
}
#undef FUNC_NAME

SCM_DEFINE_STATIC (scm_bitvector_set_bits_x, "bitvector-set-bits!", 2, 0, 0,
                   (SCM v, SCM bits),
                   "Update the bitvector @var{v} in place by performing a\n"
                   "logical OR of its bits with those of @var{bits}.\n"
                   "For example:\n"
                   "\n"
                   "@example\n"
                   "(define bv (bitvector-copy #*11000010))\n"
                   "(bitvector-set-bits! bv #*10010001)\n"
                   "bv\n"
                   "@result{} #*11010011\n"
                   "@end example")
#define FUNC_NAME s_scm_bitvector_set_bits_x
{
  scm_c_bitvector_set_bits_x (v, bits);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE_STATIC (scm_bitvector_clear_bits_x, "bitvector-clear-bits!", 2, 0, 0,
                   (SCM v, SCM bits),
                   "Update the bitvector @var{v} in place by performing a\n"
                   "logical AND of its bits with the complement of those of\n"
                   "@var{bits}.  For example:\n"
                   "\n"
                   "@example\n"
                   "(define bv (bitvector-copy #*11000010))\n"
                   "(bitvector-clear-bits! bv #*10010001)\n"
                   "bv\n"
                   "@result{} #*01000010\n"
                   "@end example")
#define FUNC_NAME s_scm_bitvector_clear_bits_x
{
  scm_c_bitvector_clear_bits_x (v, bits);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

size_t
scm_c_bitvector_count_bits (SCM bv, SCM bits)
#define FUNC_NAME "bitvector-count-bits"
{
  VALIDATE_BITVECTOR (1, bv);
  VALIDATE_BITVECTOR (2, bits);

  size_t v_len = BITVECTOR_LENGTH (bv);
  const uint32_t *v_bits = BITVECTOR_BITS (bv);
  size_t kv_len = BITVECTOR_LENGTH (bits);
  const uint32_t *kv_bits = BITVECTOR_BITS (bits);

  if (v_len < kv_len)
    SCM_MISC_ERROR ("selection bitvector longer than target bitvector",
                    SCM_EOL);

  size_t i, word_len = (kv_len + 31) / 32;
  uint32_t last_mask = ((uint32_t)-1) >> (32*word_len - kv_len);

  size_t count = 0;
  for (i = 0; i < word_len-1; i++)
    count += count_ones (v_bits[i] & kv_bits[i]);
  count += count_ones (v_bits[i] & kv_bits[i] & last_mask);

  return count;
}
#undef FUNC_NAME

SCM_DEFINE_STATIC (scm_bitvector_count_bits, "bitvector-count-bits", 2, 0, 0,
                   (SCM v, SCM kv),
                   "Return a count of how many entries in bit vector @var{v}\n"
                   "are set, with @var{kv} selecting the entries to consider.\n"
                   "\n"
                   "For example,\n"
                   "\n"
                   "@example\n"
                   "(bitvector-count-bits #*01110111 #*11001101) @result{} 3\n"
                   "@end example")
#define FUNC_NAME s_scm_bitvector_count_bits
{
  return scm_from_size_t (scm_c_bitvector_count_bits (v, kv));
}
#undef FUNC_NAME

void
scm_c_bitvector_flip_all_bits_x (SCM v)
#define FUNC_NAME "bitvector-flip-all-bits!"
{
  VALIDATE_MUTABLE_BITVECTOR (1, v);

  size_t len = BITVECTOR_LENGTH (v);
  uint32_t *bits = BITVECTOR_BITS (v);
  size_t word_len = (len + 31) / 32;
  uint32_t last_mask = ((uint32_t)-1) >> (32*word_len - len);
  size_t i;

  for (i = 0; i < word_len-1; i++)
    bits[i] = ~bits[i];
  bits[i] = bits[i] ^ last_mask;
}
#undef FUNC_NAME

SCM_DEFINE_STATIC (scm_bitvector_flip_all_bits_x,
                   "bitvector-flip-all-bits!", 1, 0, 0, (SCM v),
                   "Modify the bit vector @var{v} in place by setting all\n"
                   "clear bits and clearing all set bits.")
#define FUNC_NAME s_scm_bitvector_flip_all_bits_x
{
  scm_c_bitvector_flip_all_bits_x (v);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_VECTOR_IMPLEMENTATION (SCM_ARRAY_ELEMENT_TYPE_BIT, make_bitvector)

void
scm_init_bitvectors ()
{
#include "bitvectors.x"
}
