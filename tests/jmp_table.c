#include "test.h"

#define NTARGETS ((size_t) 4)

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 0, 0, 0);
  jit_load_args_1(j, jit_operand_gpr (JIT_OPERAND_ABI_POINTER, JIT_R0));

  jit_reloc_t default_target = jit_bgei_u(j, JIT_R0, NTARGETS);

  // FIXME: need ldxr with word stride, then can eliminate lshi.
  jit_lshi(j, JIT_R0, JIT_R0, sizeof(intptr_t) == 4 ? 2 : 3);
  jit_reloc_t table = jit_mov_addr(j, JIT_R1);
  jit_ldxr(j, JIT_R1, JIT_R1, JIT_R0);
  jit_jmpr(j, JIT_R1);

  jit_begin_data (j, (NTARGETS + 1) * sizeof(intptr_t));
  jit_align(j, sizeof(intptr_t));
  jit_patch_here(j, table);
  jit_reloc_t targets[NTARGETS];
  jit_reloc_t tails[NTARGETS];
  for (size_t i = 0; i < NTARGETS; i++) {
    targets[i] = jit_emit_addr(j);
  }
  jit_end_data (j);

  for (size_t i = 0; i < NTARGETS; i++) {
    jit_patch_here(j, targets[i]);
    jit_movi(j, JIT_R0, i * i);
    tails[i] = jit_jmp(j);
  }

  jit_patch_here(j, default_target);
  jit_movi(j, JIT_R0, 42);
  for (int i = 0; i < NTARGETS; i++) {
    jit_patch_here(j, tails[i]);
  }
  jit_leave_jit_abi(j, 0, 0, align);
  jit_retr(j, JIT_R0);

  jit_word_t (*f)(jit_word_t) = jit_end(j, NULL);

  for (int i = -2; i < ((int) NTARGETS) + 2; i++) {
    if (i < 0) {
      ASSERT(f(i) == 42);
    } else if (i < NTARGETS) {
      ASSERT(f(i) == i * i);
    } else {
      ASSERT(f(i) == 42);
    }
  }
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
