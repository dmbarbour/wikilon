{
  global: 

    wikrt_api_ver;
    wikrt_prelude;

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

    wikrt_write;
    wikrt_read;
    wikrt_is_empty;
    wikrt_clear;
    wikrt_move;
    wikrt_copy;

    wikrt_load_def;
    wikrt_save_def;
    wikrt_load_rsc;
    wikrt_save_rsc;
    wikrt_hash;

    wikrt_commit;
    
    wikrt_parse_check;
    wikrt_parse;
    wikrt_eval;
    wikrt_eval_parallel;
    wikrt_eval_stream;
    wikrt_eval_data;
    wikrt_eval_binary;
    wikrt_reify_binary;
    wikrt_set_effort;

    wikrt_debug_trace;
    wikrt_debug_breakpoint;
    wikrt_debug_eval_step;
    wikrt_prof_stack;
    wikrt_prof_heap;

    wikrt_db_open;
    wikrt_db_sync;
    wikrt_db_gc;
    wikrt_db_copy;

  local: *;
};
