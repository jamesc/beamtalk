
### 7a actor sync read: c count / GenServer.call

Beamtalk `actorCall:c:`:

```
63.0 calls/op to 51 distinct functions
    6.00  erlang:erase/1
    3.00  erlang:put/2
    3.00  atomics:add/3
    2.00  erlang:monotonic_time/1
    2.00  erlang:function_exported/3
    2.00  atomics:get/2
    1.00  bt@vs_elixir_bench@vb_bench:-class_actorCall:c:/3-repeat/2-0-/4
    1.00  persistent_term:get/2
    1.00  persistent_term:get/1
    1.00  gen_server:try_handle_call/4
    1.00  gen_server:reply/2
    1.00  gen_server:loop/5
    1.00  gen_server:loop/4
    1.00  gen_server:handle_msg/3
    1.00  gen_server:decode_msg/6
    1.00  gen_server:decode_msg/4
    1.00  gen_server:cancel_timer/1
    1.00  gen_server:call/2
    1.00  gen:reply/2
    1.00  gen:do_call/4
    1.00  gen:call/4
    1.00  gen:call/3
    1.00  ets:lookup/2
    1.00  erlang:send/3
    1.00  erlang:monitor/3
    1.00  erlang:demonitor/2
    1.00  erlang:atom_to_binary/2
    1.00  bt@vs_elixir_bench@vb_counter:safe_dispatch/4
    1.00  bt@vs_elixir_bench@vb_counter:safe_dispatch/3
    1.00  bt@vs_elixir_bench@vb_counter:handle_call/3
    1.00  bt@vs_elixir_bench@vb_counter:dispatch/4
    1.00  beamtalk_trace_store:record_dispatch/6
    1.00  beamtalk_trace_store:add_sample/4
    1.00  beamtalk_telemetry:dispatch_span/5
    1.00  beamtalk_message_dispatch:send/3
    1.00  beamtalk_class_registry:is_class_name/1
    1.00  beamtalk_actor_state:uninstall/1
    1.00  beamtalk_actor_state:install/1
    1.00  beamtalk_actor:uninstall_call_from/0
    1.00  beamtalk_actor:sync_send_pid/4
    1.00  beamtalk_actor:sync_send_known/4
    1.00  beamtalk_actor:sync_call/5
    1.00  beamtalk_actor:restore_propagated_ctx/1
    1.00  beamtalk_actor:notify_state_change/2
    1.00  beamtalk_actor:mailbox_continuation_check/2
    1.00  beamtalk_actor:install_call_from/1
    1.00  beamtalk_actor:get_propagated_ctx/0
    1.00  beamtalk_actor:encode_reply_for_tagged/3
    1.00  beamtalk_actor:clear_pending_actions/0
    1.00  beamtalk_actor:apply_pending_actions/1
    1.00  beamtalk_actor:-sync_call/5-fun-0-/5
```

Elixir `gen_call`:

```
19.0 calls/op to 19 distinct functions
    1.00  Elixir.VeBench:gen_call_loop/4
    1.00  gen_server:try_handle_call/4
    1.00  gen_server:reply/2
    1.00  gen_server:loop/5
    1.00  gen_server:loop/4
    1.00  gen_server:handle_msg/3
    1.00  gen_server:decode_msg/6
    1.00  gen_server:decode_msg/4
    1.00  gen_server:cancel_timer/1
    1.00  gen:reply/2
    1.00  gen:do_call/4
    1.00  gen:call/4
    1.00  erlang:send/3
    1.00  erlang:monitor/3
    1.00  erlang:demonitor/2
    1.00  Elixir.VeBench.Counter:handle_call/3
    1.00  Elixir.GenServer:whereis/1
    1.00  Elixir.GenServer:call/3
    1.00  Elixir.GenServer:call/2
```

### 7c actor cast: c increment! / GenServer.cast

Beamtalk `actorCast:c:`:

```
55.0 calls/op to 45 distinct functions
    6.00  erlang:erase/1
    3.00  atomics:add/3
    2.00  atomics:get/2
    2.00  erlang:monotonic_time/1
    2.00  erlang:function_exported/3
    1.00  erlang:put/2
    1.00  persistent_term:get/2
    1.00  persistent_term:get/1
    1.00  gen_server:loop/5
    1.00  gen_server:loop/4
    1.00  gen_server:handle_msg/3
    1.00  gen_server:decode_msg/6
    1.00  gen_server:decode_msg/4
    1.00  gen_server:cancel_timer/1
    1.00  ets:lookup/2
    1.00  erlang:atom_to_binary/2
    1.00  bt@vs_elixir_bench@vb_counter:safe_dispatch/4
    1.00  bt@vs_elixir_bench@vb_counter:safe_dispatch/3
    1.00  bt@vs_elixir_bench@vb_counter:dispatch/4
    1.00  bt@vs_elixir_bench@vb_bench:-class_actorCast:c:/3-repeat/2-0-/4
    1.00  beamtalk_trace_store:record_dispatch/6
    1.00  beamtalk_trace_store:add_sample/4
    1.00  beamtalk_telemetry:dispatch_span/5
    1.00  beamtalk_class_registry:is_class_name/1
    1.00  beamtalk_actor_state:uninstall/1
    1.00  beamtalk_actor_state:install/1
    1.00  beamtalk_actor:restore_propagated_ctx/1
    1.00  beamtalk_actor:notify_state_change/2
    1.00  beamtalk_actor:mailbox_continuation_check/2
    1.00  beamtalk_actor:clear_pending_actions/0
    1.00  beamtalk_actor:apply_pending_actions/1
    1.00  gen_server:try_handle_cast/3
    1.00  gen_server:handle_common_reply/5
    1.00  gen_server:do_send/2
    1.00  gen_server:do_cast/2
    1.00  gen_server:cast_msg/1
    1.00  gen_server:cast/2
    1.00  ets:member/2
    1.00  erlang:send/2
    1.00  bt@vs_elixir_bench@vb_counter:handle_cast/2
    1.00  beamtalk_object_watch:is_watched/1
    1.00  beamtalk_message_dispatch:cast/3
    1.00  beamtalk_actor:cast_send_pid/4
    1.00  beamtalk_actor:cast_send_known/4
    1.00  beamtalk_actor:-cast_send_pid/4-fun-0-/3
```

Elixir `gen_cast`:

```
14.0 calls/op to 14 distinct functions
    1.00  gen_server:loop/5
    1.00  gen_server:loop/4
    1.00  gen_server:handle_msg/3
    1.00  gen_server:decode_msg/6
    1.00  gen_server:decode_msg/4
    1.00  gen_server:cancel_timer/1
    1.00  Elixir.VeBench:cast_loop/3
    1.00  gen_server:try_handle_cast/3
    1.00  gen_server:handle_common_reply/5
    1.00  erlang:send/2
    1.00  Elixir.VeBench.Counter:handle_cast/2
    1.00  Elixir.GenServer:do_send/2
    1.00  Elixir.GenServer:cast_msg/1
    1.00  Elixir.GenServer:cast/2
```
