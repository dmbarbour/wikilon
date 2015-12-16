{
  global: 

    wikrt_env_create;
    wikrt_env_refct;
    wikrt_env_destroy;

    wikrt_cx_create;
    wikrt_cx_refct;
    wikrt_cx_destroy;
    wikrt_cx_env;

    wikrt_copy;
    wikrt_drop;

    wikrt_txn_begin;
    wikrt_db_read;
    wikrt_db_write;
    wikrt_db_swap;
    wikrt_txn_durable;
    wikrt_txn_abort;
    wikrt_txn_commit;
  local: *;
};
