{
  global: 

    wikrt_env_create;
    wikrt_env_destroy;
    wikrt_env_sync;

    wikrt_cx_create;
    wikrt_cx_destroy;
    wikrt_cx_env;

    wikrt_abcd_operators;
    wikrt_abcd_expansion;
    wikrt_strerr;
    wikrt_valid_token;
    wikrt_api_ver;

    wikrt_copy;
    wikrt_copy_remote;
    wikrt_drop;
    wikrt_stow;

    wikrt_wswap;
    wikrt_zswap;
    wikrt_assocl;
    wikrt_assocr;
    wikrt_swap;

    wikrt_sum_wswap;
    wikrt_sum_zswap;
    wikrt_sum_assocl;
    wikrt_sum_assocr;
    wikrt_sum_swap;
    wikrt_sum_distrib;
    wikrt_sum_factor;

    wikrt_intro_unit;
    wikrt_elim_unit;
    wikrt_intro_unit_r;
    wikrt_elim_unit_r;
    
    wikrt_wrap_sum;
    wikrt_unwrap_sum;
    wikrt_unwrap_match_sum;

    wikrt_intro_i32;
    wikrt_intro_i64;
    wikrt_peek_i32;
    wikrt_peek_i64;
    wikrt_intro_istr;
    wikrt_peek_istr;

    wikrt_intro_binary;
    wikrt_read_binary;
    wikrt_intro_text;
    wikrt_read_text;
    wikrt_text_to_block;
    wikrt_block_to_text;

    wikrt_wrap_seal;
    wikrt_unwrap_seal;

    wikrt_apply;
    wikrt_step_eval;
    wikrt_quote;
    wikrt_block_aff;
    wikrt_block_rel;
    wikrt_block_par;
    wikrt_quote;
    wikrt_compose;

    wikrt_int_add;
    wikrt_int_mul;
    wikrt_int_neg;
    wikrt_int_div;
    wikrt_int_cmp;

    wikrt_valid_key;

    wikrt_txn_create;
    wikrt_txn_read;
    wikrt_txn_write;
    wikrt_txn_abort;
    wikrt_txn_commit;
    wikrt_txn_durable;

  local: *;
};
