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

    wikrt_pop_prod;
    wikrt_pop_stack;
    wikrt_pop_sum;
    wikrt_pop_list;
    wikrt_pop_binary;
    wikrt_pop_text;
    wikrt_pop_seal;

    wikrt_alloc_unit;
    wikrt_alloc_prod;
    wikrt_alloc_stack;
    wikrt_alloc_sum;
    wikrt_alloc_list;
    wikrt_alloc_binary;
    wikrt_alloc_text;
    wikrt_alloc_int;
    wikrt_alloc_i32;
    wikrt_alloc_i64;
    wikrt_alloc_istr;
    wikrt_alloc_seal;

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
