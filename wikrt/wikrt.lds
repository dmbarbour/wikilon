{
  global: 

    wikrt_api_ver;

    wikrt_env_create;
    wikrt_env_destroy;
    wikrt_env_threadpool;

    wikrt_cx_create;
    wikrt_cx_reset;
    wikrt_cx_copy;
    wikrt_cx_freeze;
    wikrt_cx_destroy;
    wikrt_cx_env;
    wikrt_cx_gc;
    wikrt_cx_mem_stats;

    wikrt_write;
    wikrt_read;
    wikrt_is_empty;
    wikrt_clear;
    wikrt_move;
    wikrt_copy;
    wikrt_stream_iter;

    wikrt_load_def;
    wikrt_save_def;
    wikrt_load_rsc;
    wikrt_save_rsc;
    wikrt_hash;

    wikrt_commit;
    
    wikrt_parse_code;
    wikrt_eval;
    wikrt_eval_cmd;
    wikrt_eval_data;
    wikrt_stream_to_binary;
    wikrt_extract_binary;
    wikrt_set_effort;

    wikrt_debug_trace;
    wikrt_load_prelude;

    wikrt_db_open;
    wikrt_db_sync;
    wikrt_db_gc;
    wikrt_db_copy;

  local: *;
};
