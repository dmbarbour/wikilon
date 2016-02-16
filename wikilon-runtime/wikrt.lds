{
  global: 

    wikrt_env_create;
    wikrt_env_destroy;
    wikrt_env_sync;

    wikrt_cx_create;
    wikrt_cx_destroy;
    wikrt_cx_reset;
    wikrt_cx_env;

    wikrt_abcd_operators;
    wikrt_abcd_expansion;
    wikrt_strerr;

    wikrt_valid_token;

    wikrt_alloc_stream;
    wikrt_addend_stream;
    wikrt_end_stream;
    wikrt_awaiting_stream;

    wikrt_alloc_binary;
    wikrt_read_binary;
    
    wikrt_alloc_text;
    wikrt_alloc_text_len;
    wikrt_read_text;

    wikrt_alloc_block;
    wikrt_alloc_block_len;

    wikrt_alloc_i32;
    wikrt_alloc_i64;
    wikrt_peek_i32;
    wikrt_peek_i64;

    wikrt_alloc_istr;
    wikrt_peek_istr;
    wikrt_peek_isize;

    wikrt_alloc_prod;
    wikrt_split_prod;
    wikrt_alloc_sum;
    wikrt_split_sum;

    wikrt_alloc_seal;
    wikrt_split_seal;

    wikrt_peek_type;

    wikrt_copy;
    wikrt_drop;
    wikrt_move;
    wikrt_stow;

    wikrt_alloc_eval;
    wikrt_step_eval;
    wikrt_token_stop;
    wikrt_quote;
    wikrt_compose;

    wikrt_iadd;
    wikrt_imul;
    wikrt_ineg;
    wikrt_idiv;
    wikrt_icmp;

    wikrt_valid_key;

    wikrt_txn_create;
    wikrt_txn_swap;
    wikrt_txn_abort;
    wikrt_txn_commit;
    wikrt_txn_durable;

  local: *;
};
