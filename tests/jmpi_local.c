#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  jit_reloc_t r = jit_jmp (j);
  jit_reti (j, 0);
  jit_pointer_t addr = jit_address (j);
  jit_reti (j, 1);
  jit_patch_here (j, r);
  jit_jmpi (j, addr);
  jit_reti (j, 2);

  int (*f)(void) = jit_end(j, NULL);

  ASSERT(f() == 1);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
