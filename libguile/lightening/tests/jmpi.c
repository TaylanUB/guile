#include "test.h"

void *tail;

static void *target;

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  jit_enter_jit_abi(j, 0, 0, 0);
  jit_movi(j, JIT_R0, 42);
  jit_jmpi(j, target);
  // Unreachable.
  jit_breakpoint(j);
  int (*f)(void) = jit_end(j, NULL);
  ASSERT(f() == 42);
}

// Make the tail-call target via a separate main_helper because probably the new
// arena will be allocated farther away, forcing nonlocal jumps.
static void
make_target(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 0, 0, 0);
  // Tail call target assumes tail caller called enter_jit_abi with compatible
  // parameters.
  target = jit_address(j);
  jit_leave_jit_abi(j, 0, 0, align);
  jit_retr(j, JIT_R0);
  jit_end(j, NULL);

  main_helper(0, NULL, run_test);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, make_target);
}
