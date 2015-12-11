{
  global: 
    wikrt_strerror;

    wikrt_env_create;
    wikrt_env_destroy;

    wikrt_cx_create;
    wikrt_cx_destroy;
    wikrt_cx_env;

    wikrt_txn_begin;
    wikrt_db_read;
    wikrt_db_write;
    wikrt_db_swap;
    wikrt_txn_mark_durable;
    wikrt_txn_abort;
    wikrt_txn_commit;
    wikrt_hello;
  local: *;
};
