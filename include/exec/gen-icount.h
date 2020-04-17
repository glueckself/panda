#ifndef GEN_ICOUNT_H
#define GEN_ICOUNT_H

#include "qemu/timer.h"

/* Helpers for instruction counting code generation.  */

static TCGOp *icount_start_insn;
static TCGLabel *icount_label;


static inline void gen_io_start(void)
{
    TCGv_i32 tmp = tcg_const_i32(1);
    tcg_gen_st_i32(tmp, cpu_env,
                   offsetof(ArchCPU, parent_obj.can_do_io) -
                   offsetof(ArchCPU, env));
    tcg_temp_free_i32(tmp);
}

/*
 * cpu->can_do_io is cleared automatically at the beginning of
 * each translation block.  The cost is minimal and only paid
 * for -icount, plus it would be very easy to forget doing it
 * in the translator.  Therefore, backends only need to call
 * gen_io_start.
 */
static inline void gen_io_end(void)
{
    TCGv_i32 tmp = tcg_const_i32(0);
    tcg_gen_st_i32(tmp, cpu_env,
                   offsetof(ArchCPU, parent_obj.can_do_io) -
                   offsetof(ArchCPU, env));
    tcg_temp_free_i32(tmp);
}


static inline void gen_tb_start(TranslationBlock *tb)
{
    TCGv_i32 count, flag, imm;

    tcg_ctx->exitreq_label = gen_new_label();
    flag = tcg_temp_new_i32();
    tcg_gen_ld_i32(flag, cpu_env,
                   offsetof(CPUState, tcg_exit_req) - offsetof(ArchCPU, env));
    tcg_gen_brcondi_i32(TCG_COND_NE, flag, 0, tcg_ctx->exitreq_label);
    tcg_temp_free_i32(flag);

    if (!(tb_cflags(tb) & CF_USE_ICOUNT)) {
        return;
    }

    icount_label = gen_new_label();
    count = tcg_temp_local_new_i32();
    tcg_gen_ld_i32(count, cpu_env,
                   offsetof(ArchCPU, neg.icount_decr.u32) -
                   offsetof(ArchCPU, env));


    imm = tcg_temp_new_i32();
    /* We emit a movi with a dummy immediate argument. Keep the insn index
     * of the movi so that we later (when we know the actual insn count)
     * can update the immediate argument with the actual insn count.  */

    tcg_gen_movi_i32(imm, 0xdeadbeef);
    icount_start_insn = tcg_last_op();

    tcg_gen_sub_i32(count, count, imm);
    tcg_temp_free_i32(imm);



    tcg_gen_brcondi_i32(TCG_COND_LT, count, 0, icount_label);
    tcg_gen_st16_i32(count, cpu_env,
                         offsetof(ArchCPU, neg.icount_decr.u16.low) -
                         offsetof(ArchCPU, env));
        gen_io_end();

    tcg_temp_free_i32(count);
}

static inline void gen_tb_end(TranslationBlock *tb, int num_insns)
{
    gen_set_label(tcg_ctx->exitreq_label);
    tcg_gen_exit_tb(tb, TB_EXIT_REQUESTED);

    if (tb_cflags(tb) & CF_USE_ICOUNT) {
        /* Update the num_insn immediate parameter now that we know
         * the actual insn count.  */
        tcg_set_insn_param(icount_start_insn, 1, num_insns);
        gen_set_label(icount_label);
        tcg_gen_exit_tb(tb, TB_EXIT_ICOUNT_EXPIRED);
    }


    //TODO: panda: i guess this is not needed anymore. ... Terminate the linked list.  */
   // tcg_ctx.gen_op_buf[tcg_ctx.gen_op_buf[0].prev].next = 0;
}

// Record and replay
static inline void gen_op_update_rr_icount(void)
{
    TCGv_i64 count;

    count = tcg_temp_new_i64();

    tcg_gen_ld_i64(count, cpu_env, -offsetof(ArchCPU, env) + offsetof(CPUState, rr_guest_instr_count));
    tcg_gen_addi_i64(count, count, 1);
    tcg_gen_st_i64(count, cpu_env, -offsetof(ArchCPU, env) + offsetof(CPUState, rr_guest_instr_count));

    tcg_temp_free_i64(count);
}

static inline void gen_op_update_panda_pc(uint64_t new_pc)
{
    TCGv_i64 tmp_pc = tcg_const_i64(new_pc);
    tcg_gen_st_i64(tmp_pc, cpu_env, -offsetof(ArchCPU, env) + offsetof(CPUState, panda_guest_pc));
    tcg_temp_free_i64(tmp_pc);
}

#endif



