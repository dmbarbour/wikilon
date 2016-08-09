{
  global: 

    wikrt_env_create;
    wikrt_env_destroy;
    wikrt_env_sync;
    wikrt_env_gc;

    wikrt_cx_create;
    wikrt_cx_relax;
    wikrt_cx_reset;
    wikrt_cx_destroy;
    wikrt_cx_env;
    wikrt_cx_gc;

    wikrt_api_ver;
    wikrt_valid_token;

    wikrt_error;
    wikrt_set_error;

    wikrt_refltype;

    wikrt_copy; 
    wikrt_drop;
    wikrt_trash;
    wikrt_move;
    wikrt_copy_move;

    wikrt_stow;
    wikrt_load;

    wikrt_wswap;
    wikrt_zswap;
    wikrt_assocl;
    wikrt_assocr;

    wikrt_sum_wswap;
    wikrt_sum_zswap;
    wikrt_sum_assocl;
    wikrt_sum_assocr;
    wikrt_sum_distrib;
    wikrt_sum_factor;

    wikrt_accel_swap;
    wikrt_accel_sum_swap;
    wikrt_accel_wrzw;
    wikrt_accel_wzlw;

    wikrt_intro_unit;
    wikrt_elim_unit;
    wikrt_intro_unit_r;
    wikrt_elim_unit_r;
    
    wikrt_wrap_sum;
    wikrt_unwrap_sum;

    wikrt_intro_i32;
    wikrt_intro_i64;
    wikrt_peek_i32;
    wikrt_peek_i64;
    wikrt_intro_istr;
    wikrt_peek_istr;

    wikrt_intro_binary;
    wikrt_read_binary;
    wikrt_anno_binary;

    wikrt_intro_text;
    wikrt_read_text;
    wikrt_anno_text;

    wikrt_wrap_seal;
    wikrt_unwrap_seal;

    wikrt_text_to_block;
    wikrt_block_to_text;

    wikrt_apply;
    wikrt_step_eval;

    wikrt_quote;
    wikrt_block_aff;
    wikrt_block_rel;
    wikrt_block_lazy;
    wikrt_block_fork;
    wikrt_quote;
    wikrt_compose;

    wikrt_int_add;
    wikrt_int_mul;
    wikrt_int_neg;
    wikrt_int_div;
    wikrt_int_cmp;

    wikrt_trace_enable;
    wikrt_trace_write;
    wikrt_trace_read;
    wikrt_peek_mem_stats;

    wikrt_valid_key;

    wikrt_txn_create;
    wikrt_txn_read;
    wikrt_txn_write;
    wikrt_txn_abort;
    wikrt_txn_commit;
    wikrt_txn_durable;

  local: *;
};
