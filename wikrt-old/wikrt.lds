{
  global: 

    wikrt_api_ver;

    wikrt_env_create;
    wikrt_env_destroy;
    wikrt_env_threadpool;

    wikrt_cx_create;
    wikrt_cx_destroy;
    wikrt_cx_reset;
    wikrt_cx_env;

    wikrt_abc_list;
    wikrt_tok_valid;

    wikrt_block_open;
    wikrt_block_close;
    wikrt_apply;
    wikrt_bind;
    wikrt_copy; 
    wikrt_drop;

    wikrt_abc;
    wikrt_abc_l;

    wikrt_text;
    wikrt_text_l;
    wikrt_nat;
    
    wikrt_swap;
    wikrt_inline;

    wikrt_trash;
    wikrt_cx_gc;


    wikrt_stow;
    wikrt_load;
    wikrt_db_open;
    wikrt_db_copy;
    wikrt_db_sync;
    wikrt_db_gc;
    wikrt_key_read;
    wikrt_key_write;
    wikrt_txn_create;
    wikrt_txn_abort;
    wikrt_txn_commit;
    wikrt_txn_durable;




    wikrt_error;
    wikrt_set_error;

    wikrt_wrap_seal;
    wikrt_unwrap_seal;

    wikrt_step_eval;
    wikrt_set_step_effort;

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
    wikrt_peek_type;
    wikrt_peek_size;

  local: *;
};
