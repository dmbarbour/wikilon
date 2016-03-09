{
  global: 

    wikrt_env_create;
    wikrt_env_destroy;
    wikrt_env_sync;

    wikrt_cx_create;
    wikrt_cx_fork;
    wikrt_cx_destroy;
    wikrt_cx_env;

    wikrt_abcd_operators;
    wikrt_abcd_expansion;
    wikrt_strerr;

    wikrt_valid_token;

    wikrt_peek_type;
    wikrt_match_type;
    wikrt_peek_typestr;
    wikrt_match_typestr;

    wikrt_copy;
    wikrt_drop;
    wikrt_move;
    wikrt_copy_move;
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
    wikrt_sum_distrib;
    wikrt_sum_factor;

    wikrt_alloc_unit;
    wikrt_split_unit;
    
    wikrt_alloc_sum;
    wikrt_split_sum;

    wikrt_alloc_i32;
    wikrt_alloc_i64;
    wikrt_peek_i32;
    wikrt_peek_i64;
    wikrt_alloc_istr;
    wikrt_peek_istr;

    wikrt_alloc_binary;
    wikrt_read_binary;
    
    wikrt_alloc_text;
    wikrt_read_text;

    wikrt_alloc_block;

    wikrt_alloc_seal;
    wikrt_split_seal;

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
