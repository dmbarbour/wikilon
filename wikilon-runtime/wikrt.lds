{
  global: 

    wikrt_env_create;
    wikrt_env_incref;
    wikrt_env_destroy;
    wikrt_env_sync;

    wikrt_cx_create;
    wikrt_cx_incref;
    wikrt_cx_destroy;
    wikrt_cx_env;

    wikrt_copy;
    wikrt_drop;
    wikrt_stow;

    wikrt_alloc_i32;
    wikrt_alloc_i64;
    wikrt_alloc_istr;
    wikrt_peek_i32;
    wikrt_peek_i64;
    wikrt_peek_istr;
    wikrt_peek_isize;

    wikrt_iadd;
    wikrt_imul;
    wikrt_ineg;
    wikrt_idiv;
    wikrt_icmp;

    wikrt_alloc_prod;
    wikrt_take_prod;

    wikrt_alloc_sum;
    wikrt_take_sum;
    
    wikrt_alloc_array;
    wikrt_alloc_binary;
    wikrt_alloc_text;
    wikrt_alloc_sealed;

    wikrt_alloc_block;
    wikrt_quote;
    wikrt_compose;

    wikrt_compose;
    wikrt_affine;
    wikrt_relevant;
    wikrt_parallel;
    wikrt_iadd;
    wikrt_imul;
    wikrt_ineg;
    wikrt_idiv;
    wirkt_icmp;

    wikrt_peek_type;

    wikrt_pop_prod;
    wikrt_pop_stack;
    wikrt_pop_sum;
    wikrt_pop_list;
    wikrt_pop_binary;
    wikrt_pop_text;
    wikrt_pop_seal;

    wikrt_text_valid;
    wikrt_token_valid;
    wikrt_abc_valid;
    wikrt_key_valid;

    wikrt_txn_begin;
    wikrt_txn_read;
    wikrt_txn_write;
    wikrt_txn_swap;
    wikrt_txn_durable;
    wikrt_txn_abort;
    wikrt_txn_commit;

  local: *;
};
