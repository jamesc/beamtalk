
### 0 loop overhead (subtracted)

Beamtalk `baseline:`:

```
1.0 calls/op to 1 distinct functions
    1.00  bt@vs_elixir_bench@vb_bench:-class_baseline:/2-repeat/2-0-/3
```

Elixir `baseline`:

```
1.0 calls/op to 1 distinct functions
    1.00  Elixir.VeBench:baseline_loop/4
```

### 0b timesRepeat: whose block assigns nothing

Beamtalk `loopNoAssign:`:

```
20.0 calls/op to 13 distinct functions
    2.00  beamtalk_primitive:send/3
    2.00  beamtalk_primitive:module_for_value/2
    2.00  beamtalk_primitive:module_for_value/1
    2.00  beamtalk_primitive:dispatch_via_module/3
    2.00  beamtalk_message_dispatch:send/3
    2.00  beamtalk_message_dispatch:is_actor/1
    2.00  beamtalk_message_dispatch:classify_receiver/1
    1.00  bt@stdlib@integer:timesRepeat:/2
    1.00  bt@stdlib@integer:dispatch/3
    1.00  bt@vs_elixir_bench@vb_bench:-class_loopNoAssign:/2-anonymous-0-/0
    1.00  bt@stdlib@true:ifTrue:ifFalse:/3
    1.00  bt@stdlib@true:dispatch/3
    1.00  bt@stdlib@integer:-timesRepeat:/2-anonymous-1-/2
```

Elixir `baseline`:

```
1.0 calls/op to 1 distinct functions
    1.00  Elixir.VeBench:baseline_loop/4
```

### 1a getter on a sealed Value: p x / p.x

Beamtalk `getterSealed:p:`:

```
22.0 calls/op to 22 distinct functions
    1.00  bt@vs_elixir_bench@vb_bench:-class_getterSealed:p:/3-repeat/2-0-/4
    1.00  persistent_term:get/2
    1.00  maps:find/2
    1.00  ets:lookup/2
    1.00  erlang:module_loaded/1
    1.00  erlang:function_exported/3
    1.00  bt@vs_elixir_bench@vb_point:x/1
    1.00  bt@vs_elixir_bench@vb_point:dispatch/3
    1.00  beamtalk_tagged_map:class_of/1
    1.00  beamtalk_primitive:value_type_send/5
    1.00  beamtalk_primitive:tagged_map_dispatch/1
    1.00  beamtalk_primitive:tagged_class_dispatch/1
    1.00  beamtalk_primitive:send_map/3
    1.00  beamtalk_primitive:send/3
    1.00  beamtalk_primitive:resolved_module_loaded/2
    1.00  beamtalk_primitive:is_ivar_method/1
    1.00  beamtalk_primitive:-tagged_class_dispatch/1-fun-0-/2
    1.00  beamtalk_message_dispatch:send/3
    1.00  beamtalk_message_dispatch:is_actor/1
    1.00  beamtalk_message_dispatch:classify_receiver/1
    1.00  beamtalk_class_derived_cache:fetch/3
    1.00  atomics:get/2
```

Elixir `getter_dot`:

```
1.0 calls/op to 1 distinct functions
    1.00  Elixir.VeBench:getter_dot_loop/4
```

### 1b getter on an open Value: p x / p.x

Beamtalk `getterOpen:p:`:

```
22.0 calls/op to 22 distinct functions
    1.00  bt@vs_elixir_bench@vb_bench:-class_getterOpen:p:/3-repeat/2-0-/4
    1.00  persistent_term:get/2
    1.00  maps:find/2
    1.00  ets:lookup/2
    1.00  erlang:module_loaded/1
    1.00  erlang:function_exported/3
    1.00  bt@vs_elixir_bench@vb_open_point:x/1
    1.00  bt@vs_elixir_bench@vb_open_point:dispatch/3
    1.00  beamtalk_tagged_map:class_of/1
    1.00  beamtalk_primitive:value_type_send/5
    1.00  beamtalk_primitive:tagged_map_dispatch/1
    1.00  beamtalk_primitive:tagged_class_dispatch/1
    1.00  beamtalk_primitive:send_map/3
    1.00  beamtalk_primitive:send/3
    1.00  beamtalk_primitive:resolved_module_loaded/2
    1.00  beamtalk_primitive:is_ivar_method/1
    1.00  beamtalk_primitive:-tagged_class_dispatch/1-fun-0-/2
    1.00  beamtalk_message_dispatch:send/3
    1.00  beamtalk_message_dispatch:is_actor/1
    1.00  beamtalk_message_dispatch:classify_receiver/1
    1.00  beamtalk_class_derived_cache:fetch/3
    1.00  atomics:get/2
```

Elixir `getter_dot`:

```
1.0 calls/op to 1 distinct functions
    1.00  Elixir.VeBench:getter_dot_loop/4
```

### 1c method on a sealed Value: p sum / Point.sum(p)

Beamtalk `sumSealed:p:`:

```
22.0 calls/op to 22 distinct functions
    1.00  bt@vs_elixir_bench@vb_bench:-class_sumSealed:p:/3-repeat/2-0-/4
    1.00  persistent_term:get/2
    1.00  maps:find/2
    1.00  ets:lookup/2
    1.00  erlang:module_loaded/1
    1.00  erlang:function_exported/3
    1.00  bt@vs_elixir_bench@vb_point:sum/1
    1.00  bt@vs_elixir_bench@vb_point:dispatch/3
    1.00  beamtalk_tagged_map:class_of/1
    1.00  beamtalk_primitive:value_type_send/5
    1.00  beamtalk_primitive:tagged_map_dispatch/1
    1.00  beamtalk_primitive:tagged_class_dispatch/1
    1.00  beamtalk_primitive:send_map/3
    1.00  beamtalk_primitive:send/3
    1.00  beamtalk_primitive:resolved_module_loaded/2
    1.00  beamtalk_primitive:is_ivar_method/1
    1.00  beamtalk_primitive:-tagged_class_dispatch/1-fun-0-/2
    1.00  beamtalk_message_dispatch:send/3
    1.00  beamtalk_message_dispatch:is_actor/1
    1.00  beamtalk_message_dispatch:classify_receiver/1
    1.00  beamtalk_class_derived_cache:fetch/3
    1.00  atomics:get/2
```

Elixir `point_sum`:

```
2.0 calls/op to 2 distinct functions
    1.00  Elixir.VeBench:point_sum_loop/4
    1.00  Elixir.VeBench.Point:sum/1
```

### 2a functional update: p withX: 3 / %{p | x: 3}

Beamtalk `withX:p:`:

```
23.0 calls/op to 23 distinct functions
    1.00  bt@vs_elixir_bench@vb_bench:-class_withX:p:/3-repeat/2-0-/4
    1.00  persistent_term:get/2
    1.00  maps:put/3
    1.00  maps:find/2
    1.00  ets:lookup/2
    1.00  erlang:module_loaded/1
    1.00  erlang:function_exported/3
    1.00  bt@vs_elixir_bench@vb_point:withX:/2
    1.00  bt@vs_elixir_bench@vb_point:dispatch/3
    1.00  beamtalk_tagged_map:class_of/1
    1.00  beamtalk_primitive:value_type_send/5
    1.00  beamtalk_primitive:tagged_map_dispatch/1
    1.00  beamtalk_primitive:tagged_class_dispatch/1
    1.00  beamtalk_primitive:send_map/3
    1.00  beamtalk_primitive:send/3
    1.00  beamtalk_primitive:resolved_module_loaded/2
    1.00  beamtalk_primitive:is_ivar_method/1
    1.00  beamtalk_primitive:-tagged_class_dispatch/1-fun-0-/2
    1.00  beamtalk_message_dispatch:send/3
    1.00  beamtalk_message_dispatch:is_actor/1
    1.00  beamtalk_message_dispatch:classify_receiver/1
    1.00  beamtalk_class_derived_cache:fetch/3
    1.00  atomics:get/2
```

Elixir `with_x`:

```
1.0 calls/op to 1 distinct functions
    1.00  Elixir.VeBench:with_x_loop/4
```

### 2b construct: VbPoint x: a y: 4 / %Point{x: a, y: 4}

Beamtalk `construct:a:`:

```
102.0 calls/op to 74 distinct functions
    7.00  erlang:put/2
    7.00  erlang:erase/1
    5.00  beamtalk_object_class:restore_dict_key/2
    3.00  ets:lookup/2
    3.00  erlang:list_to_atom/1
    3.00  erlang:atom_to_list/1
    3.00  erlang:++/2
    2.00  ets:info/1
    2.00  erlang:whereis/1
    2.00  beamtalk_class_registry:class_object_tag/1
    2.00  beamtalk_class_dispatch:is_test_execution_selector/1
    1.00  bt@vs_elixir_bench@vb_bench:-class_construct:a:/3-repeat/2-0-/4
    1.00  gen_server:try_handle_call/4
    1.00  gen_server:reply/2
    1.00  gen_server:loop/5
    1.00  gen_server:loop/4
    1.00  gen_server:handle_msg/3
    1.00  gen_server:decode_msg/6
    1.00  gen_server:decode_msg/4
    1.00  gen_server:cancel_timer/1
    1.00  gen_server:call/3
    1.00  gen:reply/2
    1.00  gen:do_call/4
    1.00  gen:call/4
    1.00  ets:lookup_element/4
    1.00  ets:insert/2
    1.00  ets:info/2
    1.00  erlang:send/3
    1.00  erlang:monitor/3
    1.00  erlang:demonitor/2
    1.00  erlang:binary_to_atom/2
    1.00  erlang:atom_to_binary/2
    1.00  bt@vs_elixir_bench@vb_point:class_x:y:/3
    1.00  beamtalk_object_class:seed_session_context_from/1
    1.00  beamtalk_object_class:handle_call/3
    1.00  beamtalk_object_class:dispatch_class_method/5
    1.00  beamtalk_object_class:class_send/3
    1.00  beamtalk_object_class:caller_pid/1
    1.00  beamtalk_object_class:adopt_entry_group_leader/1
    1.00  beamtalk_object_class:-seed_session_context_from/1-fun-0-/5
    1.00  beamtalk_object_class:-adopt_entry_group_leader/1-fun-2-/0
    1.00  beamtalk_extensions:lookup/2
    1.00  beamtalk_ets_heir:maybe_set_heir/1
    1.00  beamtalk_ets_heir:heir_pid/0
    1.00  beamtalk_dispatch:check_extension/2
    1.00  beamtalk_class_vars:uninstall/1
    1.00  beamtalk_class_vars:key_for_tag/1
    1.00  beamtalk_class_vars:install/2
    1.00  beamtalk_class_vars:assert_absent/1
    1.00  beamtalk_class_registry:whereis_class/1
    1.00  beamtalk_class_registry:registry_name/1
    1.00  beamtalk_class_registry:record_class_state_snapshot/2
    1.00  beamtalk_class_registry:ensure_class_state_table/0
    1.00  beamtalk_class_registry:class_name_for_pid/1
    1.00  beamtalk_class_metadata:row/1
    1.00  beamtalk_class_metadata:lookup_direct_class_method/2
    1.00  beamtalk_class_metadata:lookup_class_method_fun/2
    1.00  beamtalk_class_metadata:has_runtime_class_methods/1
    1.00  beamtalk_class_metadata:field/2
    1.00  beamtalk_class_dispatch:run_with_class_vars/3
    1.00  beamtalk_class_dispatch:receiver_tag/1
    1.00  beamtalk_class_dispatch:lookup_direct_call/3
    1.00  beamtalk_class_dispatch:local_session_context/0
    1.00  beamtalk_class_dispatch:invoke_class_method/7
    1.00  beamtalk_class_dispatch:handle_class_method_call/6
    1.00  beamtalk_class_dispatch:class_send_with_recovery/3
    1.00  beamtalk_class_dispatch:class_send_plain/3
    1.00  beamtalk_class_dispatch:class_send_dispatch/3
    1.00  beamtalk_class_dispatch:class_send/3
    1.00  beamtalk_class_dispatch:class_method_fun_name/1
    1.00  beamtalk_class_dispatch:apply_compiled_class_method/6
    1.00  beamtalk_class_dispatch:apply_class_method_in_context/6
    1.00  beamtalk_class_dispatch:-invoke_class_method/7-fun-0-/6
    1.00  beamtalk_class_dispatch:-class_send_plain/3-fun-4-/3
```

Elixir `construct`:

```
1.0 calls/op to 1 distinct functions
    1.00  Elixir.VeBench:construct_loop/4
```

### 3a `class sealed` method, sealed class / Mod.fun(arg)

Beamtalk `classSealedMethod:`:

```
2.0 calls/op to 2 distinct functions
    1.00  bt@vs_elixir_bench@vb_bench:-class_classSealedMethod:/2-repeat/2-0-/3
    1.00  bt@vs_elixir_bench@vb_sealed_math:class_double:/2
```

Elixir `remote_call`:

```
2.0 calls/op to 2 distinct functions
    1.00  Elixir.VeBench:remote_call_loop/4
    1.00  Elixir.VeBench.Math:double/1
```

### 3b class method, sealed class / Mod.fun(arg)

Beamtalk `sealedClassMethod:`:

```
102.0 calls/op to 74 distinct functions
    7.00  erlang:put/2
    7.00  erlang:erase/1
    5.00  beamtalk_object_class:restore_dict_key/2
    3.00  ets:lookup/2
    3.00  erlang:list_to_atom/1
    3.00  erlang:atom_to_list/1
    3.00  erlang:++/2
    2.00  ets:info/1
    2.00  erlang:whereis/1
    2.00  beamtalk_class_registry:class_object_tag/1
    2.00  beamtalk_class_dispatch:is_test_execution_selector/1
    1.00  bt@vs_elixir_bench@vb_bench:-class_sealedClassMethod:/2-repeat/2-0-/3
    1.00  gen_server:try_handle_call/4
    1.00  gen_server:reply/2
    1.00  gen_server:loop/5
    1.00  gen_server:loop/4
    1.00  gen_server:handle_msg/3
    1.00  gen_server:decode_msg/6
    1.00  gen_server:decode_msg/4
    1.00  gen_server:cancel_timer/1
    1.00  gen_server:call/3
    1.00  gen:reply/2
    1.00  gen:do_call/4
    1.00  gen:call/4
    1.00  ets:lookup_element/4
    1.00  ets:insert/2
    1.00  ets:info/2
    1.00  erlang:send/3
    1.00  erlang:monitor/3
    1.00  erlang:demonitor/2
    1.00  erlang:binary_to_atom/2
    1.00  erlang:atom_to_binary/2
    1.00  bt@vs_elixir_bench@vb_math:class_double:/2
    1.00  beamtalk_object_class:seed_session_context_from/1
    1.00  beamtalk_object_class:handle_call/3
    1.00  beamtalk_object_class:dispatch_class_method/5
    1.00  beamtalk_object_class:class_send/3
    1.00  beamtalk_object_class:caller_pid/1
    1.00  beamtalk_object_class:adopt_entry_group_leader/1
    1.00  beamtalk_object_class:-seed_session_context_from/1-fun-0-/5
    1.00  beamtalk_object_class:-adopt_entry_group_leader/1-fun-2-/0
    1.00  beamtalk_extensions:lookup/2
    1.00  beamtalk_ets_heir:maybe_set_heir/1
    1.00  beamtalk_ets_heir:heir_pid/0
    1.00  beamtalk_dispatch:check_extension/2
    1.00  beamtalk_class_vars:uninstall/1
    1.00  beamtalk_class_vars:key_for_tag/1
    1.00  beamtalk_class_vars:install/2
    1.00  beamtalk_class_vars:assert_absent/1
    1.00  beamtalk_class_registry:whereis_class/1
    1.00  beamtalk_class_registry:registry_name/1
    1.00  beamtalk_class_registry:record_class_state_snapshot/2
    1.00  beamtalk_class_registry:ensure_class_state_table/0
    1.00  beamtalk_class_registry:class_name_for_pid/1
    1.00  beamtalk_class_metadata:row/1
    1.00  beamtalk_class_metadata:lookup_direct_class_method/2
    1.00  beamtalk_class_metadata:lookup_class_method_fun/2
    1.00  beamtalk_class_metadata:has_runtime_class_methods/1
    1.00  beamtalk_class_metadata:field/2
    1.00  beamtalk_class_dispatch:run_with_class_vars/3
    1.00  beamtalk_class_dispatch:receiver_tag/1
    1.00  beamtalk_class_dispatch:lookup_direct_call/3
    1.00  beamtalk_class_dispatch:local_session_context/0
    1.00  beamtalk_class_dispatch:invoke_class_method/7
    1.00  beamtalk_class_dispatch:handle_class_method_call/6
    1.00  beamtalk_class_dispatch:class_send_with_recovery/3
    1.00  beamtalk_class_dispatch:class_send_plain/3
    1.00  beamtalk_class_dispatch:class_send_dispatch/3
    1.00  beamtalk_class_dispatch:class_send/3
    1.00  beamtalk_class_dispatch:class_method_fun_name/1
    1.00  beamtalk_class_dispatch:apply_compiled_class_method/6
    1.00  beamtalk_class_dispatch:apply_class_method_in_context/6
    1.00  beamtalk_class_dispatch:-invoke_class_method/7-fun-0-/6
    1.00  beamtalk_class_dispatch:-class_send_plain/3-fun-4-/3
```

Elixir `remote_call`:

```
2.0 calls/op to 2 distinct functions
    1.00  Elixir.VeBench:remote_call_loop/4
    1.00  Elixir.VeBench.Math:double/1
```

### 3c class method, open class / Mod.fun(arg)

Beamtalk `openClassMethod:`:

```
102.0 calls/op to 74 distinct functions
    7.00  erlang:put/2
    7.00  erlang:erase/1
    5.00  beamtalk_object_class:restore_dict_key/2
    3.00  ets:lookup/2
    3.00  erlang:list_to_atom/1
    3.00  erlang:atom_to_list/1
    3.00  erlang:++/2
    2.00  ets:info/1
    2.00  erlang:whereis/1
    2.00  beamtalk_class_registry:class_object_tag/1
    2.00  beamtalk_class_dispatch:is_test_execution_selector/1
    1.00  bt@vs_elixir_bench@vb_bench:-class_openClassMethod:/2-repeat/2-0-/3
    1.00  gen_server:try_handle_call/4
    1.00  gen_server:reply/2
    1.00  gen_server:loop/5
    1.00  gen_server:loop/4
    1.00  gen_server:handle_msg/3
    1.00  gen_server:decode_msg/6
    1.00  gen_server:decode_msg/4
    1.00  gen_server:cancel_timer/1
    1.00  gen_server:call/3
    1.00  gen:reply/2
    1.00  gen:do_call/4
    1.00  gen:call/4
    1.00  ets:lookup_element/4
    1.00  ets:insert/2
    1.00  ets:info/2
    1.00  erlang:send/3
    1.00  erlang:monitor/3
    1.00  erlang:demonitor/2
    1.00  erlang:binary_to_atom/2
    1.00  erlang:atom_to_binary/2
    1.00  bt@vs_elixir_bench@vb_open_math:class_double:/2
    1.00  beamtalk_object_class:seed_session_context_from/1
    1.00  beamtalk_object_class:handle_call/3
    1.00  beamtalk_object_class:dispatch_class_method/5
    1.00  beamtalk_object_class:class_send/3
    1.00  beamtalk_object_class:caller_pid/1
    1.00  beamtalk_object_class:adopt_entry_group_leader/1
    1.00  beamtalk_object_class:-seed_session_context_from/1-fun-0-/5
    1.00  beamtalk_object_class:-adopt_entry_group_leader/1-fun-2-/0
    1.00  beamtalk_extensions:lookup/2
    1.00  beamtalk_ets_heir:maybe_set_heir/1
    1.00  beamtalk_ets_heir:heir_pid/0
    1.00  beamtalk_dispatch:check_extension/2
    1.00  beamtalk_class_vars:uninstall/1
    1.00  beamtalk_class_vars:key_for_tag/1
    1.00  beamtalk_class_vars:install/2
    1.00  beamtalk_class_vars:assert_absent/1
    1.00  beamtalk_class_registry:whereis_class/1
    1.00  beamtalk_class_registry:registry_name/1
    1.00  beamtalk_class_registry:record_class_state_snapshot/2
    1.00  beamtalk_class_registry:ensure_class_state_table/0
    1.00  beamtalk_class_registry:class_name_for_pid/1
    1.00  beamtalk_class_metadata:row/1
    1.00  beamtalk_class_metadata:lookup_direct_class_method/2
    1.00  beamtalk_class_metadata:lookup_class_method_fun/2
    1.00  beamtalk_class_metadata:has_runtime_class_methods/1
    1.00  beamtalk_class_metadata:field/2
    1.00  beamtalk_class_dispatch:run_with_class_vars/3
    1.00  beamtalk_class_dispatch:receiver_tag/1
    1.00  beamtalk_class_dispatch:lookup_direct_call/3
    1.00  beamtalk_class_dispatch:local_session_context/0
    1.00  beamtalk_class_dispatch:invoke_class_method/7
    1.00  beamtalk_class_dispatch:handle_class_method_call/6
    1.00  beamtalk_class_dispatch:class_send_with_recovery/3
    1.00  beamtalk_class_dispatch:class_send_plain/3
    1.00  beamtalk_class_dispatch:class_send_dispatch/3
    1.00  beamtalk_class_dispatch:class_send/3
    1.00  beamtalk_class_dispatch:class_method_fun_name/1
    1.00  beamtalk_class_dispatch:apply_compiled_class_method/6
    1.00  beamtalk_class_dispatch:apply_class_method_in_context/6
    1.00  beamtalk_class_dispatch:-invoke_class_method/7-fun-0-/6
    1.00  beamtalk_class_dispatch:-class_send_plain/3-fun-4-/3
```

Elixir `remote_call`:

```
2.0 calls/op to 2 distinct functions
    1.00  Elixir.VeBench:remote_call_loop/4
    1.00  Elixir.VeBench.Math:double/1
```

### 4a untyped receiver: o x / o.x

Beamtalk `dynGetter:o:`:

```
22.0 calls/op to 22 distinct functions
    1.00  bt@vs_elixir_bench@vb_bench:-class_dynGetter:o:/3-repeat/2-0-/4
    1.00  persistent_term:get/2
    1.00  maps:find/2
    1.00  ets:lookup/2
    1.00  erlang:module_loaded/1
    1.00  erlang:function_exported/3
    1.00  bt@vs_elixir_bench@vb_point:x/1
    1.00  bt@vs_elixir_bench@vb_point:dispatch/3
    1.00  beamtalk_tagged_map:class_of/1
    1.00  beamtalk_primitive:value_type_send/5
    1.00  beamtalk_primitive:tagged_map_dispatch/1
    1.00  beamtalk_primitive:tagged_class_dispatch/1
    1.00  beamtalk_primitive:send_map/3
    1.00  beamtalk_primitive:send/3
    1.00  beamtalk_primitive:resolved_module_loaded/2
    1.00  beamtalk_primitive:is_ivar_method/1
    1.00  beamtalk_primitive:-tagged_class_dispatch/1-fun-0-/2
    1.00  beamtalk_message_dispatch:send/3
    1.00  beamtalk_message_dispatch:is_actor/1
    1.00  beamtalk_message_dispatch:classify_receiver/1
    1.00  beamtalk_class_derived_cache:fetch/3
    1.00  atomics:get/2
```

Elixir `getter_dot`:

```
1.0 calls/op to 1 distinct functions
    1.00  Elixir.VeBench:getter_dot_loop/4
```

### 4b untyped receiver: o area / protocol

Beamtalk `dynArea:o:`:

```
22.0 calls/op to 22 distinct functions
    1.00  bt@vs_elixir_bench@vb_bench:-class_dynArea:o:/3-repeat/2-0-/4
    1.00  persistent_term:get/2
    1.00  maps:find/2
    1.00  ets:lookup/2
    1.00  erlang:module_loaded/1
    1.00  erlang:function_exported/3
    1.00  bt@vs_elixir_bench@vb_square:dispatch/3
    1.00  bt@vs_elixir_bench@vb_square:area/1
    1.00  beamtalk_tagged_map:class_of/1
    1.00  beamtalk_primitive:value_type_send/5
    1.00  beamtalk_primitive:tagged_map_dispatch/1
    1.00  beamtalk_primitive:tagged_class_dispatch/1
    1.00  beamtalk_primitive:send_map/3
    1.00  beamtalk_primitive:send/3
    1.00  beamtalk_primitive:resolved_module_loaded/2
    1.00  beamtalk_primitive:is_ivar_method/1
    1.00  beamtalk_primitive:-tagged_class_dispatch/1-fun-0-/2
    1.00  beamtalk_message_dispatch:send/3
    1.00  beamtalk_message_dispatch:is_actor/1
    1.00  beamtalk_message_dispatch:classify_receiver/1
    1.00  beamtalk_class_derived_cache:fetch/3
    1.00  atomics:get/2
```

Elixir `protocol`:

```
5.0 calls/op to 5 distinct functions
    1.00  Elixir.VeBench:protocol_loop/4
    1.00  Elixir.VeBench.Area.VeBench.Square:area/1
    1.00  Elixir.VeBench.Area:impl_for!/1
    1.00  Elixir.VeBench.Area:impl_for/1
    1.00  Elixir.VeBench.Area:area/1
```

### 4c typed open Value: s area / Square.area(s)

Beamtalk `typedArea:s:`:

```
22.0 calls/op to 22 distinct functions
    1.00  bt@vs_elixir_bench@vb_bench:-class_typedArea:s:/3-repeat/2-0-/4
    1.00  persistent_term:get/2
    1.00  maps:find/2
    1.00  ets:lookup/2
    1.00  erlang:module_loaded/1
    1.00  erlang:function_exported/3
    1.00  bt@vs_elixir_bench@vb_square:dispatch/3
    1.00  bt@vs_elixir_bench@vb_square:area/1
    1.00  beamtalk_tagged_map:class_of/1
    1.00  beamtalk_primitive:value_type_send/5
    1.00  beamtalk_primitive:tagged_map_dispatch/1
    1.00  beamtalk_primitive:tagged_class_dispatch/1
    1.00  beamtalk_primitive:send_map/3
    1.00  beamtalk_primitive:send/3
    1.00  beamtalk_primitive:resolved_module_loaded/2
    1.00  beamtalk_primitive:is_ivar_method/1
    1.00  beamtalk_primitive:-tagged_class_dispatch/1-fun-0-/2
    1.00  beamtalk_message_dispatch:send/3
    1.00  beamtalk_message_dispatch:is_actor/1
    1.00  beamtalk_message_dispatch:classify_receiver/1
    1.00  beamtalk_class_derived_cache:fetch/3
    1.00  atomics:get/2
```

Elixir `square_area`:

```
2.0 calls/op to 2 distinct functions
    1.00  Elixir.VeBench:square_area_loop/4
    1.00  Elixir.VeBench.Square:area/1
```

### 5a inherited method: s base / defdelegate

Beamtalk `inherited:s:`:

```
25.0 calls/op to 24 distinct functions
    2.00  ets:lookup/2
    1.00  bt@vs_elixir_bench@vb_bench:-class_inherited:s:/3-repeat/2-0-/4
    1.00  persistent_term:get/2
    1.00  maps:find/2
    1.00  erlang:module_loaded/1
    1.00  erlang:function_exported/3
    1.00  bt@vs_elixir_bench@vb_sub:dispatch/3
    1.00  bt@vs_elixir_bench@vb_base:dispatch/3
    1.00  bt@vs_elixir_bench@vb_base:base/1
    1.00  beamtalk_tagged_map:class_of/1
    1.00  beamtalk_primitive:value_type_send/5
    1.00  beamtalk_primitive:tagged_map_dispatch/1
    1.00  beamtalk_primitive:tagged_class_dispatch/1
    1.00  beamtalk_primitive:send_map/3
    1.00  beamtalk_primitive:send/3
    1.00  beamtalk_primitive:resolved_module_loaded/2
    1.00  beamtalk_primitive:is_ivar_method/1
    1.00  beamtalk_primitive:-tagged_class_dispatch/1-fun-0-/2
    1.00  beamtalk_message_dispatch:send/3
    1.00  beamtalk_message_dispatch:is_actor/1
    1.00  beamtalk_message_dispatch:classify_receiver/1
    1.00  beamtalk_extensions:lookup/2
    1.00  beamtalk_class_derived_cache:fetch/3
    1.00  atomics:get/2
```

Elixir `delegate`:

```
3.0 calls/op to 3 distinct functions
    1.00  Elixir.VeBench:delegate_loop/4
    1.00  Elixir.VeBench.Sub:base/1
    1.00  Elixir.VeBench.Base:base/1
```

### 5b self-send to inherited: s viaSelf / local call

Beamtalk `inheritedViaSelf:s:`:

```
46.0 calls/op to 25 distinct functions
    3.00  ets:lookup/2
    2.00  persistent_term:get/2
    2.00  maps:find/2
    2.00  erlang:module_loaded/1
    2.00  erlang:function_exported/3
    2.00  bt@vs_elixir_bench@vb_sub:dispatch/3
    2.00  beamtalk_tagged_map:class_of/1
    2.00  beamtalk_primitive:value_type_send/5
    2.00  beamtalk_primitive:tagged_map_dispatch/1
    2.00  beamtalk_primitive:tagged_class_dispatch/1
    2.00  beamtalk_primitive:send_map/3
    2.00  beamtalk_primitive:send/3
    2.00  beamtalk_primitive:resolved_module_loaded/2
    2.00  beamtalk_primitive:is_ivar_method/1
    2.00  beamtalk_primitive:-tagged_class_dispatch/1-fun-0-/2
    2.00  beamtalk_message_dispatch:send/3
    2.00  beamtalk_message_dispatch:is_actor/1
    2.00  beamtalk_message_dispatch:classify_receiver/1
    2.00  beamtalk_class_derived_cache:fetch/3
    2.00  atomics:get/2
    1.00  bt@vs_elixir_bench@vb_bench:-class_inheritedViaSelf:s:/3-repeat/2-0-/4
    1.00  bt@vs_elixir_bench@vb_sub:viaSelf/1
    1.00  bt@vs_elixir_bench@vb_base:dispatch/3
    1.00  bt@vs_elixir_bench@vb_base:base/1
    1.00  beamtalk_extensions:lookup/2
```

Elixir `delegate_via_self`:

```
4.0 calls/op to 4 distinct functions
    1.00  Elixir.VeBench:delegate_via_self_loop/4
    1.00  Elixir.VeBench.Sub:via_self/1
    1.00  Elixir.VeBench.Sub:base/1
    1.00  Elixir.VeBench.Base:base/1
```

### 5c runtime-resolved method: s hasField: #v / Map.has_key?

Beamtalk `hasField:s:`:

```
22.0 calls/op to 22 distinct functions
    1.00  ets:lookup/2
    1.00  bt@vs_elixir_bench@vb_bench:-class_hasField:s:/3-repeat/2-0-/4
    1.00  persistent_term:get/2
    1.00  maps:find/2
    1.00  erlang:module_loaded/1
    1.00  erlang:function_exported/3
    1.00  bt@vs_elixir_bench@vb_sub:dispatch/3
    1.00  beamtalk_tagged_map:class_of/1
    1.00  beamtalk_reflection:has_field/2
    1.00  beamtalk_primitive:value_type_send/5
    1.00  beamtalk_primitive:tagged_map_dispatch/1
    1.00  beamtalk_primitive:tagged_class_dispatch/1
    1.00  beamtalk_primitive:send_map/3
    1.00  beamtalk_primitive:send/3
    1.00  beamtalk_primitive:resolved_module_loaded/2
    1.00  beamtalk_primitive:is_ivar_method/1
    1.00  beamtalk_primitive:-tagged_class_dispatch/1-fun-0-/2
    1.00  beamtalk_message_dispatch:send/3
    1.00  beamtalk_message_dispatch:is_actor/1
    1.00  beamtalk_message_dispatch:classify_receiver/1
    1.00  beamtalk_class_derived_cache:fetch/3
    1.00  atomics:get/2
```

Elixir `has_key`:

```
1.0 calls/op to 1 distinct functions
    1.00  Elixir.VeBench:has_key_loop/4
```

### 6a block value: x / fun.(x)

Beamtalk `blockValue:blk:`:

```
2.0 calls/op to 2 distinct functions
    1.00  bt@vs_elixir_bench@vb_bench:-class_blockValue:blk:/3-repeat/2-0-/4
    1.00  bt@vs_elixir_bench@vb_bench:-class_adder/1-anonymous-0-/1
```

Elixir `fun_call`:

```
2.0 calls/op to 2 distinct functions
    1.00  Elixir.VeBench:fun_call_loop/4
    1.00  Elixir.VeBench:-adder/0-fun-0-/1
```

### 6b do: summing 100 elements / Enum.reduce

Beamtalk `doSum:list:`:

```
202.0 calls/op to 4 distinct functions
  100.00  lists:foldl_1/3
  100.00  bt@vs_elixir_bench@vb_bench:-class_doSum:list:/3-anonymous-1-/2
    1.00  bt@vs_elixir_bench@vb_bench:-class_doSum:list:/3-repeat/2-0-/4
    1.00  lists:foldl/3
```

Elixir `enum_reduce_sum`:

```
203.0 calls/op to 4 distinct functions
  101.00  Elixir.Enum:-reduce/3-lists^foldl/2-0-/3
  100.00  Elixir.VeBench:-enum_reduce_sum_loop/4-fun-0-/2
    1.00  Elixir.VeBench:enum_reduce_sum_loop/4
    1.00  Elixir.Enum:reduce/3
```

### 6c do: over 100, no assignment / Enum.each

Beamtalk `doNoAssign:list:`:

```
203.0 calls/op to 4 distinct functions
  101.00  lists:foreach_1/2
  100.00  bt@vs_elixir_bench@vb_bench:-class_doNoAssign:list:/3-anonymous-1-/1
    1.00  bt@vs_elixir_bench@vb_bench:-class_doNoAssign:list:/3-repeat/2-0-/4
    1.00  lists:foreach/2
```

Elixir `enum_each`:

```
203.0 calls/op to 4 distinct functions
  101.00  Elixir.Enum:-each/2-lists^foreach/1-0-/2
  100.00  Elixir.VeBench:-enum_each_loop/4-fun-0-/1
    1.00  Elixir.VeBench:enum_each_loop/4
    1.00  Elixir.Enum:each/2
```

### 6d collect: 100 elements / Enum.map

Beamtalk `collect:list:`:

```
202.0 calls/op to 4 distinct functions
  100.00  lists:map_1/2
  100.00  bt@vs_elixir_bench@vb_bench:-class_collect:list:/3-anonymous-1-/1
    1.00  bt@vs_elixir_bench@vb_bench:-class_collect:list:/3-repeat/2-0-/4
    1.00  lists:map/2
```

Elixir `enum_map`:

```
203.0 calls/op to 4 distinct functions
  101.00  Elixir.Enum:-map/2-lists^map/1-1-/2
  100.00  Elixir.VeBench:-enum_map_loop/4-fun-0-/1
    1.00  Elixir.VeBench:enum_map_loop/4
    1.00  Elixir.Enum:map/2
```

### 6e inject:into: 100 elements / Enum.reduce

Beamtalk `inject:list:`:

```
202.0 calls/op to 4 distinct functions
  100.00  lists:foldl_1/3
  100.00  bt@vs_elixir_bench@vb_bench:-class_inject:list:/3-anonymous-1-/2
    1.00  bt@vs_elixir_bench@vb_bench:-class_inject:list:/3-repeat/2-0-/4
    1.00  lists:foldl/3
```

Elixir `enum_reduce_sum`:

```
203.0 calls/op to 4 distinct functions
  101.00  Elixir.Enum:-reduce/3-lists^foldl/2-0-/3
  100.00  Elixir.VeBench:-enum_reduce_sum_loop/4-fun-0-/2
    1.00  Elixir.VeBench:enum_reduce_sum_loop/4
    1.00  Elixir.Enum:reduce/3
```

### 7a actor sync read: c count / GenServer.call

Beamtalk `actorCall:c:`:

```
140.0 calls/op to 98 distinct functions
   11.00  erlang:erase/1
    4.00  erlang:atom_to_binary/2
    4.00  beamtalk_actor:-strip_local_temps/1-lc$^0/1-0-/1
    3.00  telemetry:do_execute/4
    3.00  ets:lookup/2
    3.00  erlang:function_exported/3
    3.00  beamtalk_actor:is_local_temp_key/1
    3.00  atomics:add/3
    2.00  telemetry_handler_table:list_for_event/1
    2.00  telemetry_handler_table:default_impl/0
    2.00  telemetry_ets:list_for_event/2
    2.00  telemetry:execute/3
    2.00  persistent_term:get/2
    2.00  persistent_term:get/1
    2.00  maps:find/2
    2.00  erlang:put/2
    2.00  erlang:monotonic_time/0
    2.00  erlang:++/2
    2.00  beamtalk_trace_store:is_enabled/0
    2.00  beamtalk_telemetry:with_span_context/2
    2.00  beamtalk_class_registry:is_class_name/1
    2.00  beamtalk_actor:get_trace_context/0
    2.00  atomics:get/2
    1.99  erlang:make_ref/0
    1.00  bt@vs_elixir_bench@vb_bench:-class_actorCall:c:/3-repeat/2-0-/4
    1.00  maps:keys/1
    1.00  logger:get_process_metadata/0
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
    1.00  ets:member/2
    1.00  ets:match/2
    1.00  ets:insert/2
    1.00  erlang:system_time/0
    1.00  erlang:send/3
    1.00  erlang:monitor/3
    1.00  erlang:is_process_alive/1
    1.00  erlang:demonitor/2
    1.00  erlang:convert_time_unit/3
    1.00  bt@vs_elixir_bench@vb_counter:safe_dispatch/4
    1.00  bt@vs_elixir_bench@vb_counter:safe_dispatch/3
    1.00  bt@vs_elixir_bench@vb_counter:handle_call/3
    1.00  bt@vs_elixir_bench@vb_counter:dispatch/4
    1.00  bt@vs_elixir_bench@vb_counter:class_name/0
    1.00  binary:part/3
    1.00  beamtalk_trace_store:record_dispatch/6
    1.00  beamtalk_trace_store:handle_dispatch_stop/4
    1.00  beamtalk_trace_store:ensure_counter_slot/2
    1.00  beamtalk_trace_store:do_record_dispatch/6
    1.00  beamtalk_trace_store:cas_min/3
    1.00  beamtalk_trace_store:cas_max/3
    1.00  beamtalk_telemetry:run_traced_span/3
    1.00  beamtalk_telemetry:run_span/3
    1.00  beamtalk_telemetry:maybe_span/3
    1.00  beamtalk_telemetry:emit_stop/3
    1.00  beamtalk_tagged_map:class_of/1
    1.00  beamtalk_tagged_map:class_name_from_mod/2
    1.00  beamtalk_pid:is_alive/1
    1.00  beamtalk_object_watch:is_watched/1
    1.00  beamtalk_message_dispatch:with_known_class/3
    1.00  beamtalk_message_dispatch:send/3
    1.00  beamtalk_message_dispatch:is_actor_ref/1
    1.00  beamtalk_message_dispatch:is_actor/1
    1.00  beamtalk_message_dispatch:classify_receiver/1
    1.00  beamtalk_message_dispatch:-send/3-fun-0-/3
    1.00  beamtalk_class_registry:is_class_object/1
    1.00  beamtalk_actor:wire_sync_call/5
    1.00  beamtalk_actor:sync_send_remote/3
    1.00  beamtalk_actor:sync_send/3
    1.00  beamtalk_actor:strip_local_temps/1
    1.00  beamtalk_actor:stash_known_class/2
    1.00  beamtalk_actor:restore_propagated_ctx/1
    1.00  beamtalk_actor:notify_state_change/2
    1.00  beamtalk_actor:make_self/1
    1.00  beamtalk_actor:mailbox_continuation_check/2
    1.00  beamtalk_actor:lookup_class/1
    1.00  beamtalk_actor:local_call/3
    1.00  beamtalk_actor:get_sync_propagated_ctx/0
    1.00  beamtalk_actor:get_propagated_ctx/0
    1.00  beamtalk_actor:get_otel_ctx/0
    1.00  beamtalk_actor:encode_reply_for_tagged/3
    1.00  beamtalk_actor:continuation_target/2
    1.00  beamtalk_actor:clear_trace_context/0
    1.00  beamtalk_actor:clear_pending_actions/0
    1.00  beamtalk_actor:clear_known_class/0
    1.00  beamtalk_actor:check_call_stack/2
    1.00  beamtalk_actor:apply_pending_actions/1
    1.00  beamtalk_actor:-sync_send_remote/3-fun-0-/5
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

### 7b actor state update: c increment / GenServer.call

Beamtalk `actorUpdate:c:`:

```
140.0 calls/op to 98 distinct functions
   11.00  erlang:erase/1
    4.00  erlang:atom_to_binary/2
    4.00  beamtalk_actor:-strip_local_temps/1-lc$^0/1-0-/1
    3.00  telemetry:do_execute/4
    3.00  ets:lookup/2
    3.00  erlang:function_exported/3
    3.00  beamtalk_actor:is_local_temp_key/1
    3.00  atomics:add/3
    2.00  telemetry_handler_table:list_for_event/1
    2.00  telemetry_handler_table:default_impl/0
    2.00  telemetry_ets:list_for_event/2
    2.00  telemetry:execute/3
    2.00  persistent_term:get/2
    2.00  persistent_term:get/1
    2.00  maps:find/2
    2.00  erlang:put/2
    2.00  erlang:monotonic_time/0
    2.00  erlang:++/2
    2.00  beamtalk_trace_store:is_enabled/0
    2.00  beamtalk_telemetry:with_span_context/2
    2.00  beamtalk_class_registry:is_class_name/1
    2.00  beamtalk_actor:get_trace_context/0
    2.00  atomics:get/2
    1.99  erlang:make_ref/0
    1.00  bt@vs_elixir_bench@vb_bench:-class_actorUpdate:c:/3-repeat/2-0-/4
    1.00  maps:keys/1
    1.00  logger:get_process_metadata/0
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
    1.00  ets:member/2
    1.00  ets:match/2
    1.00  ets:insert/2
    1.00  erlang:system_time/0
    1.00  erlang:send/3
    1.00  erlang:monitor/3
    1.00  erlang:is_process_alive/1
    1.00  erlang:demonitor/2
    1.00  erlang:convert_time_unit/3
    1.00  bt@vs_elixir_bench@vb_counter:safe_dispatch/4
    1.00  bt@vs_elixir_bench@vb_counter:safe_dispatch/3
    1.00  bt@vs_elixir_bench@vb_counter:handle_call/3
    1.00  bt@vs_elixir_bench@vb_counter:dispatch/4
    1.00  bt@vs_elixir_bench@vb_counter:class_name/0
    1.00  binary:part/3
    1.00  beamtalk_trace_store:record_dispatch/6
    1.00  beamtalk_trace_store:handle_dispatch_stop/4
    1.00  beamtalk_trace_store:ensure_counter_slot/2
    1.00  beamtalk_trace_store:do_record_dispatch/6
    1.00  beamtalk_trace_store:cas_min/3
    1.00  beamtalk_trace_store:cas_max/3
    1.00  beamtalk_telemetry:run_traced_span/3
    1.00  beamtalk_telemetry:run_span/3
    1.00  beamtalk_telemetry:maybe_span/3
    1.00  beamtalk_telemetry:emit_stop/3
    1.00  beamtalk_tagged_map:class_of/1
    1.00  beamtalk_tagged_map:class_name_from_mod/2
    1.00  beamtalk_pid:is_alive/1
    1.00  beamtalk_object_watch:is_watched/1
    1.00  beamtalk_message_dispatch:with_known_class/3
    1.00  beamtalk_message_dispatch:send/3
    1.00  beamtalk_message_dispatch:is_actor_ref/1
    1.00  beamtalk_message_dispatch:is_actor/1
    1.00  beamtalk_message_dispatch:classify_receiver/1
    1.00  beamtalk_message_dispatch:-send/3-fun-0-/3
    1.00  beamtalk_class_registry:is_class_object/1
    1.00  beamtalk_actor:wire_sync_call/5
    1.00  beamtalk_actor:sync_send_remote/3
    1.00  beamtalk_actor:sync_send/3
    1.00  beamtalk_actor:strip_local_temps/1
    1.00  beamtalk_actor:stash_known_class/2
    1.00  beamtalk_actor:restore_propagated_ctx/1
    1.00  beamtalk_actor:notify_state_change/2
    1.00  beamtalk_actor:make_self/1
    1.00  beamtalk_actor:mailbox_continuation_check/2
    1.00  beamtalk_actor:lookup_class/1
    1.00  beamtalk_actor:local_call/3
    1.00  beamtalk_actor:get_sync_propagated_ctx/0
    1.00  beamtalk_actor:get_propagated_ctx/0
    1.00  beamtalk_actor:get_otel_ctx/0
    1.00  beamtalk_actor:encode_reply_for_tagged/3
    1.00  beamtalk_actor:continuation_target/2
    1.00  beamtalk_actor:clear_trace_context/0
    1.00  beamtalk_actor:clear_pending_actions/0
    1.00  beamtalk_actor:clear_known_class/0
    1.00  beamtalk_actor:check_call_stack/2
    1.00  beamtalk_actor:apply_pending_actions/1
    1.00  beamtalk_actor:-sync_send_remote/3-fun-0-/5
```

Elixir `gen_update`:

```
19.0 calls/op to 19 distinct functions
    1.00  Elixir.VeBench:gen_update_loop/4
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
131.0 calls/op to 89 distinct functions
   12.01  erlang:erase/1
    4.00  erlang:atom_to_binary/2
    4.00  beamtalk_actor:-strip_local_temps/1-lc$^0/1-0-/1
    3.00  telemetry:do_execute/4
    3.00  ets:lookup/2
    3.00  erlang:function_exported/3
    3.00  beamtalk_actor:is_local_temp_key/1
    3.00  atomics:add/3
    2.00  telemetry_handler_table:list_for_event/1
    2.00  telemetry_handler_table:default_impl/0
    2.00  telemetry_ets:list_for_event/2
    2.00  telemetry:execute/3
    2.00  persistent_term:get/2
    2.00  persistent_term:get/1
    2.00  maps:find/2
    2.00  erlang:monotonic_time/0
    2.00  erlang:++/2
    2.00  beamtalk_trace_store:is_enabled/0
    2.00  beamtalk_telemetry:with_span_context/2
    2.00  beamtalk_class_registry:is_class_name/1
    2.00  beamtalk_actor:get_trace_context/0
    2.00  atomics:get/2
    1.94  erlang:make_ref/0
    1.00  erlang:put/2
    1.00  maps:keys/1
    1.00  logger:get_process_metadata/0
    1.00  gen_server:loop/5
    1.00  gen_server:loop/4
    1.00  gen_server:handle_msg/3
    1.00  gen_server:decode_msg/6
    1.00  gen_server:decode_msg/4
    1.00  gen_server:cancel_timer/1
    1.00  ets:member/2
    1.00  ets:match/2
    1.00  ets:insert/2
    1.00  erlang:system_time/0
    1.00  erlang:is_process_alive/1
    1.00  erlang:convert_time_unit/3
    1.00  bt@vs_elixir_bench@vb_counter:safe_dispatch/4
    1.00  bt@vs_elixir_bench@vb_counter:safe_dispatch/3
    1.00  bt@vs_elixir_bench@vb_counter:dispatch/4
    1.00  bt@vs_elixir_bench@vb_counter:class_name/0
    1.00  bt@vs_elixir_bench@vb_bench:-class_actorCast:c:/3-repeat/2-0-/4
    1.00  binary:part/3
    1.00  beamtalk_trace_store:record_dispatch/6
    1.00  beamtalk_trace_store:handle_dispatch_stop/4
    1.00  beamtalk_trace_store:ensure_counter_slot/2
    1.00  beamtalk_trace_store:do_record_dispatch/6
    1.00  beamtalk_trace_store:cas_min/3
    1.00  beamtalk_trace_store:cas_max/3
    1.00  beamtalk_telemetry:run_traced_span/3
    1.00  beamtalk_telemetry:run_span/3
    1.00  beamtalk_telemetry:maybe_span/3
    1.00  beamtalk_telemetry:emit_stop/3
    1.00  beamtalk_tagged_map:class_of/1
    1.00  beamtalk_tagged_map:class_name_from_mod/2
    1.00  beamtalk_pid:is_alive/1
    1.00  beamtalk_object_watch:is_watched/1
    1.00  beamtalk_message_dispatch:with_known_class/3
    1.00  beamtalk_message_dispatch:is_actor_ref/1
    1.00  beamtalk_message_dispatch:is_actor/1
    1.00  beamtalk_message_dispatch:classify_receiver/1
    1.00  beamtalk_class_registry:is_class_object/1
    1.00  beamtalk_actor:strip_local_temps/1
    1.00  beamtalk_actor:stash_known_class/2
    1.00  beamtalk_actor:restore_propagated_ctx/1
    1.00  beamtalk_actor:notify_state_change/2
    1.00  beamtalk_actor:make_self/1
    1.00  beamtalk_actor:mailbox_continuation_check/2
    1.00  beamtalk_actor:lookup_class/1
    1.00  beamtalk_actor:get_propagated_ctx/0
    1.00  beamtalk_actor:get_otel_ctx/0
    1.00  beamtalk_actor:continuation_target/2
    1.00  beamtalk_actor:clear_trace_context/0
    1.00  beamtalk_actor:clear_pending_actions/0
    1.00  beamtalk_actor:clear_known_class/0
    1.00  beamtalk_actor:apply_pending_actions/1
    1.00  gen_server:try_handle_cast/3
    1.00  gen_server:handle_common_reply/5
    1.00  gen_server:do_send/2
    1.00  gen_server:do_cast/2
    1.00  gen_server:cast_msg/1
    1.00  gen_server:cast/2
    1.00  erlang:send/2
    1.00  bt@vs_elixir_bench@vb_counter:handle_cast/2
    1.00  beamtalk_message_dispatch:cast/3
    1.00  beamtalk_message_dispatch:-cast/3-fun-0-/3
    1.00  beamtalk_actor:cast_send/3
    1.00  beamtalk_actor:-cast_send/3-fun-0-/4
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

### 7d actor method doing self hasField: / GenServer.call

Beamtalk `actorProbe:c:`:

```
270.0 calls/op to 153 distinct functions
   13.00  ets:lookup/2
   11.00  erlang:erase/1
    6.00  erlang:function_exported/3
    5.00  ets:lookup_element/4
    5.00  ets:info/1
    5.00  erlang:++/2
    5.00  beamtalk_class_registry:class_name_for_pid/1
    5.00  beamtalk_class_metadata:field/2
    4.00  maps:find/2
    4.00  erlang:atom_to_binary/2
    4.00  beamtalk_actor:-strip_local_temps/1-lc$^0/1-0-/1
    3.00  telemetry:do_execute/4
    3.00  persistent_term:get/2
    3.00  erlang:whereis/1
    3.00  erlang:list_to_atom/1
    3.00  erlang:atom_to_list/1
    3.00  beamtalk_object_class:reflection_class_name/1
    3.00  beamtalk_object_class:published_compiled_method/3
    3.00  beamtalk_object_class:published_class_name/1
    3.00  beamtalk_object_class:local_compiled_method/3
    3.00  beamtalk_object_class:compiled_method_safe/3
    3.00  beamtalk_class_registry:whereis_class/1
    3.00  beamtalk_class_registry:registry_name/1
    3.00  beamtalk_class_metadata:lookup_method_record/3
    3.00  beamtalk_class_metadata:lookup_instance_methods/1
    3.00  beamtalk_actor:is_local_temp_key/1
    3.00  atomics:get/2
    3.00  atomics:add/3
    2.00  telemetry_handler_table:list_for_event/1
    2.00  telemetry_handler_table:default_impl/0
    2.00  telemetry_ets:list_for_event/2
    2.00  telemetry:execute/3
    2.00  persistent_term:get/1
    2.00  maps:from_list/1
    2.00  ets:whereis/1
    2.00  ets:tab2list/1
    2.00  ets:match_object/2
    2.00  erlang:put/2
    2.00  erlang:monotonic_time/0
    2.00  bt@vs_elixir_bench@vb_counter:class_name/0
    2.00  beamtalk_xref:gen_snapshot/0
    2.00  beamtalk_xref:-method_info/3-lc$^1/1-0-/3
    2.00  beamtalk_trace_store:is_enabled/0
    2.00  beamtalk_telemetry:with_span_context/2
    2.00  beamtalk_tagged_map:class_of/1
    2.00  beamtalk_tagged_map:class_name_from_mod/2
    2.00  beamtalk_object_class:superclass_safe/1
    2.00  beamtalk_method_resolver:superclass_pid/1
    2.00  beamtalk_method_resolver:method_step/2
    2.00  beamtalk_method_resolver:-resolve_with_hierarchy/2-fun-0-/3
    2.00  beamtalk_hierarchy:walk_ancestors/4
    2.00  beamtalk_class_registry:is_class_name/1
    2.00  beamtalk_class_metadata:lookup_superclass/1
    2.00  beamtalk_actor:get_trace_context/0
    2.00  erlang:make_ref/0
    1.00  bt@vs_elixir_bench@vb_bench:-class_actorProbe:c:/3-repeat/2-0-/4
    1.00  maps:keys/1
    1.00  logger:get_process_metadata/0
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
    1.00  ets:member/2
    1.00  ets:match/2
    1.00  ets:insert/2
    1.00  erlang:system_time/0
    1.00  erlang:send/3
    1.00  erlang:monitor/3
    1.00  erlang:module_loaded/1
    1.00  erlang:is_process_alive/1
    1.00  erlang:demonitor/2
    1.00  erlang:convert_time_unit/3
    1.00  bt@vs_elixir_bench@vb_counter:safe_dispatch/4
    1.00  bt@vs_elixir_bench@vb_counter:safe_dispatch/3
    1.00  bt@vs_elixir_bench@vb_counter:handle_call/3
    1.00  bt@vs_elixir_bench@vb_counter:dispatch/4
    1.00  binary:part/3
    1.00  beamtalk_xref:read_stable_loop/2
    1.00  beamtalk_xref:read_stable/1
    1.00  beamtalk_xref:method_origin/3
    1.00  beamtalk_xref:method_info/3
    1.00  beamtalk_xref:is_live_gen/3
    1.00  beamtalk_xref:-method_info/3-fun-0-/3
    1.00  beamtalk_trace_store:record_dispatch/6
    1.00  beamtalk_trace_store:handle_dispatch_stop/4
    1.00  beamtalk_trace_store:ensure_counter_slot/2
    1.00  beamtalk_trace_store:do_record_dispatch/6
    1.00  beamtalk_trace_store:cas_min/3
    1.00  beamtalk_trace_store:cas_max/3
    1.00  beamtalk_telemetry:run_traced_span/3
    1.00  beamtalk_telemetry:run_span/3
    1.00  beamtalk_telemetry:maybe_span/3
    1.00  beamtalk_telemetry:emit_stop/3
    1.00  beamtalk_reflection:has_field/2
    1.00  beamtalk_primitive:value_type_send/5
    1.00  beamtalk_primitive:tagged_map_dispatch/1
    1.00  beamtalk_primitive:tagged_class_dispatch/1
    1.00  beamtalk_primitive:send_map/3
    1.00  beamtalk_primitive:send/3
    1.00  beamtalk_primitive:runtime_instance_method/2
    1.00  beamtalk_primitive:resolved_module_loaded/2
    1.00  beamtalk_primitive:method_fun_from_resolved/1
    1.00  beamtalk_primitive:is_ivar_method/1
    1.00  beamtalk_primitive:-tagged_class_dispatch/1-fun-0-/2
    1.00  beamtalk_pid:is_alive/1
    1.00  beamtalk_object_watch:is_watched/1
    1.00  beamtalk_object_ops:try_dispatch/3
    1.00  beamtalk_object_ops:has_method/1
    1.00  beamtalk_object_ops:dispatch/4
    1.00  beamtalk_object_class:method/2
    1.00  beamtalk_object_class:compiled_method_map/7
    1.00  beamtalk_method_resolver:resolve_with_hierarchy/2
    1.00  beamtalk_method_resolver:resolve/2
    1.00  beamtalk_message_dispatch:with_known_class/3
    1.00  beamtalk_message_dispatch:send/3
    1.00  beamtalk_message_dispatch:is_actor_ref/1
    1.00  beamtalk_message_dispatch:is_actor/1
    1.00  beamtalk_message_dispatch:classify_receiver/1
    1.00  beamtalk_message_dispatch:-send/3-fun-0-/3
    1.00  beamtalk_hierarchy:walk_ancestors/3
    1.00  beamtalk_class_registry:is_class_object/1
    1.00  beamtalk_class_derived_cache:fetch/3
    1.00  beamtalk_actor:wire_sync_call/5
    1.00  beamtalk_actor:sync_send_remote/3
    1.00  beamtalk_actor:sync_send/3
    1.00  beamtalk_actor:strip_local_temps/1
    1.00  beamtalk_actor:stash_known_class/2
    1.00  beamtalk_actor:restore_propagated_ctx/1
    1.00  beamtalk_actor:notify_state_change/2
    1.00  beamtalk_actor:make_self/1
    1.00  beamtalk_actor:mailbox_continuation_check/2
    1.00  beamtalk_actor:lookup_class/1
    1.00  beamtalk_actor:local_call/3
    1.00  beamtalk_actor:get_sync_propagated_ctx/0
    1.00  beamtalk_actor:get_propagated_ctx/0
    1.00  beamtalk_actor:get_otel_ctx/0
    1.00  beamtalk_actor:encode_reply_for_tagged/3
    1.00  beamtalk_actor:continuation_target/2
    1.00  beamtalk_actor:clear_trace_context/0
    1.00  beamtalk_actor:clear_pending_actions/0
    1.00  beamtalk_actor:clear_known_class/0
    1.00  beamtalk_actor:check_call_stack/2
    1.00  beamtalk_actor:apply_pending_actions/1
    1.00  beamtalk_actor:-sync_send_remote/3-fun-0-/5
```

Elixir `gen_probe`:

```
19.0 calls/op to 19 distinct functions
    1.00  Elixir.VeBench:gen_probe_loop/4
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

### 8a on: Error do: without a raise / try-rescue

Beamtalk `onDoNoRaise:`:

```
27.0 calls/op to 27 distinct functions
    1.00  bt@vs_elixir_bench@vb_bench:-class_onDoNoRaise:/2-repeat/2-0-/3
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
    1.00  erlang:whereis/1
    1.00  erlang:send/3
    1.00  erlang:monitor/3
    1.00  erlang:list_to_atom/1
    1.00  erlang:demonitor/2
    1.00  erlang:atom_to_list/1
    1.00  erlang:++/2
    1.00  bt@vs_elixir_bench@vb_bench:-class_onDoNoRaise:/2-anonymous-1-/1
    1.00  beamtalk_object_class:module_name/1
    1.00  beamtalk_object_class:handle_call/3
    1.00  beamtalk_class_vars:snapshot/0
    1.00  beamtalk_class_registry:whereis_class/1
    1.00  beamtalk_class_registry:registry_name/1
```

Elixir `try_no_raise`:

```
1.0 calls/op to 1 distinct functions
    1.00  Elixir.VeBench:try_no_raise_loop/4
```

### 8b on: Error do: with a raise / raise-rescue

Beamtalk `onDoRaise:`:

```
1615.0 calls/op to 215 distinct functions
  129.00  ets:do_foldl/4
  128.00  lists:foldl_1/3
  128.00  lists:foldl/3
  128.00  ets:next_lookup/2
  128.00  beamtalk_class_registry:-module_to_class_map/0-fun-0-/2
  128.00  beamtalk_class_metadata:-foldl_modules/2-fun-0-/3
   96.00  lists:do_flatten/2
   66.00  string:split_1/6
   22.00  string:-to_upper/1-lc$^0/1-0-/1
   22.00  proplists:get_value/3
   17.00  io_lib_format:count_small/2
   17.00  io_lib_format:build_limited/5
   17.00  erlang:++/2
   17.00  beamtalk_module_name:-snake_to_class/1-lc$^0/1-0-/1
   16.00  proplists:get_value/2
   16.00  lists:reverse/1
   14.00  io_lib_format:iolist_to_chars/1
   14.00  erlang:atom_to_list/1
   12.00  string:is_empty/1
   12.00  lists:sublist_2/2
   11.00  string:to_upper_char/1
   11.00  string:to_upper/1
   11.00  beamtalk_module_name:to_upper_chars/1
   11.00  beamtalk_module_name:capitalize/1
   10.00  lists:nthtail_1/2
    9.00  lists:reverse/2
    9.00  beamtalk_stack_frame:-wrap/1-lc$^0/1-0-/2
    8.00  maps:find/2
    8.00  logger_backend:do_apply_filters/4
    8.00  lists:flatten/1
    8.00  io_lib_format:collect/2
    8.00  io_lib_format:build_small/1
    8.00  beamtalk_stack_frame:wrap_frame/2
    8.00  beamtalk_stack_frame:resolve_class_name/2
    7.00  ets:lookup/2
    7.00  erlang:put/2
    7.00  erlang:erase/1
    6.00  unicode_util:cp/1
    6.00  unicode:characters_to_list/2
    6.00  unicode:characters_to_list/1
    6.00  string:split/3
    6.00  erlang:list_to_existing_atom/1
    6.00  erlang:list_to_binary/1
    6.00  erlang:list_to_atom/1
    6.00  beamtalk_stack_frame:module_to_class_heuristic/1
    6.00  beamtalk_module_name:snake_to_class/1
    5.00  unicode_util:gc/1
    5.00  string:prefix_1/2
    5.00  logger_filters:on_match/2
    5.00  logger_config:table_key/1
    5.00  lists:prefix/2
    5.00  beamtalk_object_class:restore_dict_key/2
    5.00  beamtalk_class_registry:inherits_from/2
    4.00  logger_filters:filter_domain/4
    4.00  logger_filters:domain/2
    4.00  logger:add_default_metadata/2
    4.00  ets:lookup_element/4
    4.00  beamtalk_class_metadata:field/2
    3.00  persistent_term:get/2
    3.00  maps:merge/2
    3.00  logger_filters:is_prefix/3
    3.00  logger_config:less_or_equal_level/2
    3.00  logger_config:get/2
    3.00  logger_backend:call_handlers/3
    3.00  logger_backend:apply_filters/4
    3.00  logger:default/1
    3.00  io_lib_format:string/6
    3.00  io_lib_format:precision/2
    3.00  io_lib_format:pad_char/2
    3.00  io_lib_format:modifiers/3
    3.00  io_lib_format:field_width/3
    3.00  io_lib_format:field_width/2
    3.00  io_lib_format:field_value/2
    3.00  io_lib_format:control_small/7
    3.00  io_lib_format:collect_cseq/2
    3.00  io_lib_format:collect_cc/2
    3.00  ets:info/1
    3.00  erlang:whereis/1
    3.00  beamtalk_class_metadata:lookup_superclass/1
    2.00  logger_config:get/3
    2.00  lists:suffix/2
    2.00  lists:sublist/2
    2.00  lists:nthtail/2
    2.00  gen_server:try_handle_call/4
    2.00  gen_server:reply/2
    2.00  gen_server:loop/5
    2.00  gen_server:loop/4
    2.00  gen_server:handle_msg/3
    2.00  gen_server:decode_msg/6
    2.00  gen_server:decode_msg/4
    2.00  gen_server:cancel_timer/1
    2.00  gen:reply/2
    2.00  gen:do_call/4
    2.00  gen:call/4
    2.00  ets:whereis/1
    2.00  ets:safe_fixtable/2
    2.00  erlang:send/3
    2.00  erlang:monitor/3
    2.00  erlang:error/1
    2.00  erlang:demonitor/2
    2.00  beamtalk_object_class:handle_call/3
    2.00  beamtalk_exception_handler:strip_class_suffix/1
    2.00  beamtalk_exception_handler:matches_class/2
    2.00  beamtalk_exception_handler:is_exception_class/1
    2.00  beamtalk_error:format_reason/2
    2.00  beamtalk_class_registry:whereis_class/1
    2.00  beamtalk_class_registry:registry_name/1
    2.00  beamtalk_class_registry:class_object_tag/1
    2.00  beamtalk_class_metadata:row/1
    2.00  beamtalk_class_dispatch:is_test_execution_selector/1
    1.00  bt@vs_elixir_bench@vb_bench:-class_onDoRaise:/2-repeat/2-0-/3
    1.00  os:system_time/1
    1.00  logger_filters:remote_gl/2
    1.00  logger_filters:filter_remote_gl/2
    1.00  logger_config:allow/2
    1.00  logger_backend:log_allowed/3
    1.00  logger:timestamp/0
    1.00  logger:tid/0
    1.00  logger:proc_meta/0
    1.00  logger:macro_log/5
    1.00  logger:log_allowed/4
    1.00  logger:get_process_metadata/0
    1.00  logger:do_log_allowed/5
    1.00  logger:deatomize/1
    1.00  logger:allow/2
    1.00  logger:add_default_metadata/1
    1.00  lists:member/2
    1.00  lists:keyfind/3
    1.00  io_lib_format:sub/2
    1.00  io_lib_format:scan/2
    1.00  io_lib_format:limit_field/2
    1.00  io_lib_format:iolist_to_chars/3
    1.00  io_lib_format:get_option/3
    1.00  io_lib_format:fwrite/2
    1.00  io_lib_format:decr_pc/2
    1.00  io_lib_format:count_small/1
    1.00  io_lib_format:control_limited/11
    1.00  io_lib_format:build/2
    1.00  io_lib_format:build/1
    1.00  io_lib:format/2
    1.00  gen_server:call/3
    1.00  gen_server:call/2
    1.00  gen:call/3
    1.00  ets:soft_whereis/1
    1.00  ets:insert/2
    1.00  ets:info/2
    1.00  ets:foldl/3
    1.00  ets:first_lookup/1
    1.00  erlang:iolist_to_binary/1
    1.00  erlang:group_leader/0
    1.00  erlang:binary_to_list/1
    1.00  erlang:binary_to_atom/2
    1.00  erlang:atom_to_binary/2
    1.00  bt@vs_elixir_bench@vb_bench:-class_onDoRaise:/2-anonymous-2-/1
    1.00  bt@vs_elixir_bench@vb_bench:-class_onDoRaise:/2-anonymous-1-/0
    1.00  bt@stdlib@exception:class_signal:/2
    1.00  beamtalk_stack_frame:wrap/1
    1.00  beamtalk_object_class:seed_session_context_from/1
    1.00  beamtalk_object_class:module_name/1
    1.00  beamtalk_object_class:dispatch_class_method/5
    1.00  beamtalk_object_class:class_send/3
    1.00  beamtalk_object_class:caller_pid/1
    1.00  beamtalk_object_class:adopt_entry_group_leader/1
    1.00  beamtalk_object_class:-seed_session_context_from/1-fun-0-/5
    1.00  beamtalk_object_class:-adopt_entry_group_leader/1-fun-2-/0
    1.00  beamtalk_hierarchy:walk_ancestors/4
    1.00  beamtalk_hierarchy:walk_ancestors/3
    1.00  beamtalk_extensions:lookup/2
    1.00  beamtalk_exception_handler:wrap/1
    1.00  beamtalk_exception_handler:signal_message/2
    1.00  beamtalk_exception_handler:reraise/1
    1.00  beamtalk_exception_handler:matches_class_name/2
    1.00  beamtalk_exception_handler:ensure_wrapped/3
    1.00  beamtalk_exception_handler:ensure_wrapped/1
    1.00  beamtalk_exception_handler:class_to_kind/1
    1.00  beamtalk_exception_handler:class_signal_message/2
    1.00  beamtalk_ets_heir:maybe_set_heir/1
    1.00  beamtalk_ets_heir:heir_pid/0
    1.00  beamtalk_error:raise/1
    1.00  beamtalk_dispatch:check_extension/2
    1.00  beamtalk_class_vars:uninstall/1
    1.00  beamtalk_class_vars:snapshot/0
    1.00  beamtalk_class_vars:restore/1
    1.00  beamtalk_class_vars:key_for_tag/1
    1.00  beamtalk_class_vars:install/2
    1.00  beamtalk_class_vars:assert_absent/1
    1.00  beamtalk_class_registry:record_class_state_snapshot/2
    1.00  beamtalk_class_registry:module_to_class_map/0
    1.00  beamtalk_class_registry:ensure_class_state_table/0
    1.00  beamtalk_class_registry:class_name_for_pid/1
    1.00  beamtalk_class_metadata:lookup_methods/1
    1.00  beamtalk_class_metadata:lookup_direct_class_method/2
    1.00  beamtalk_class_metadata:lookup_class_method_fun/2
    1.00  beamtalk_class_metadata:has_runtime_class_methods/1
    1.00  beamtalk_class_metadata:foldl_modules/2
    1.00  beamtalk_class_dispatch:walk_class_method_chain/3
    1.00  beamtalk_class_dispatch:unwrap_class_call/1
    1.00  beamtalk_class_dispatch:superclass_from_ets/1
    1.00  beamtalk_class_dispatch:run_with_class_vars/3
    1.00  beamtalk_class_dispatch:receiver_tag/1
    1.00  beamtalk_class_dispatch:lookup_direct_call/3
    1.00  beamtalk_class_dispatch:local_session_context/0
    1.00  beamtalk_class_dispatch:invoke_class_method/7
    1.00  beamtalk_class_dispatch:handle_class_method_call/6
    1.00  beamtalk_class_dispatch:find_class_method_in_chain/2
    1.00  beamtalk_class_dispatch:class_send_with_recovery/3
    1.00  beamtalk_class_dispatch:class_send_plain/3
    1.00  beamtalk_class_dispatch:class_send_dispatch/3
    1.00  beamtalk_class_dispatch:class_send/3
    1.00  beamtalk_class_dispatch:class_method_fun_name/1
    1.00  beamtalk_class_dispatch:apply_compiled_class_method/6
    1.00  beamtalk_class_dispatch:apply_class_method_in_context/6
    1.00  beamtalk_class_dispatch:-walk_class_method_chain/3-fun-0-/3
    1.00  beamtalk_class_dispatch:-invoke_class_method/7-fun-0-/6
    1.00  beamtalk_class_dispatch:-class_send_plain/3-fun-4-/3
```

Elixir `try_raise`:

```
11.0 calls/op to 9 distinct functions
    2.00  Elixir.RuntimeError:exception/1
    2.00  Elixir.Enum:-reduce/3-lists^foldl/2-0-/3
    1.00  Elixir.VeBench:try_raise_loop/4
    1.00  erlang:error/3
    1.00  Elixir.RuntimeError:__struct__/1
    1.00  Elixir.RuntimeError:-__struct__/1-fun-0-/2
    1.00  Elixir.Kernel:validate_struct!/3
    1.00  Elixir.Kernel:struct!/2
    1.00  Elixir.Enum:reduce/3
```

### 9a (Result ok: x) value / {:ok, x} match

Beamtalk `resultOk:`:

```
152.0 calls/op to 109 distinct functions
   10.00  lists:prefix/2
    7.00  erlang:put/2
    7.00  erlang:erase/1
    5.00  beamtalk_object_class:restore_dict_key/2
    4.00  ets:lookup/2
    4.00  erlang:atom_to_list/1
    3.00  erlang:list_to_atom/1
    3.00  erlang:++/2
    2.00  ets:info/1
    2.00  erlang:whereis/1
    2.00  beamtalk_primitive:send/3
    2.00  beamtalk_message_dispatch:send/3
    2.00  beamtalk_message_dispatch:is_actor/1
    2.00  beamtalk_message_dispatch:classify_receiver/1
    2.00  beamtalk_class_registry:class_object_tag/1
    2.00  beamtalk_class_dispatch:is_test_execution_selector/1
    1.00  gen_server:loop/5
    1.00  gen_server:loop/4
    1.00  gen_server:handle_msg/3
    1.00  gen_server:decode_msg/6
    1.00  gen_server:decode_msg/4
    1.00  gen_server:cancel_timer/1
    1.00  ets:info/2
    1.00  bt@vs_elixir_bench@vb_bench:-class_resultOk:/2-repeat/2-0-/3
    1.00  persistent_term:get/2
    1.00  maps:find/2
    1.00  gen_server:try_handle_call/4
    1.00  gen_server:reply/2
    1.00  gen_server:call/3
    1.00  gen:reply/2
    1.00  gen:do_call/4
    1.00  gen:call/4
    1.00  ets:lookup_element/4
    1.00  ets:insert/2
    1.00  erlang:send/3
    1.00  erlang:monitor/3
    1.00  erlang:module_loaded/1
    1.00  erlang:function_exported/3
    1.00  erlang:demonitor/2
    1.00  erlang:binary_to_atom/2
    1.00  erlang:atom_to_binary/2
    1.00  bt@stdlib@true:ifTrue:ifFalse:/3
    1.00  bt@stdlib@true:dispatch/3
    1.00  bt@stdlib@result:value/1
    1.00  bt@stdlib@result:dispatch/3
    1.00  bt@stdlib@result:class_ok:/2
    1.00  bt@stdlib@result:-value/1-anonymous-1-/1
    1.00  beamtalk_tagged_map:class_of/1
    1.00  beamtalk_result:ok:/1
    1.00  beamtalk_result:ok/1
    1.00  beamtalk_result:from_tagged_tuple/1
    1.00  beamtalk_primitive:tagged_map_dispatch/1
    1.00  beamtalk_primitive:tagged_class_dispatch/1
    1.00  beamtalk_primitive:send_map/3
    1.00  beamtalk_primitive:resolved_module_loaded/2
    1.00  beamtalk_primitive:module_for_value/2
    1.00  beamtalk_primitive:module_for_value/1
    1.00  beamtalk_primitive:dispatch_via_module/3
    1.00  beamtalk_primitive:-tagged_class_dispatch/1-fun-0-/2
    1.00  beamtalk_object_class:seed_session_context_from/1
    1.00  beamtalk_object_class:handle_call/3
    1.00  beamtalk_object_class:dispatch_class_method/5
    1.00  beamtalk_object_class:class_send/3
    1.00  beamtalk_object_class:caller_pid/1
    1.00  beamtalk_object_class:adopt_entry_group_leader/1
    1.00  beamtalk_object_class:-seed_session_context_from/1-fun-0-/5
    1.00  beamtalk_object_class:-adopt_entry_group_leader/1-fun-2-/0
    1.00  beamtalk_extensions:lookup/2
    1.00  beamtalk_ets_heir:maybe_set_heir/1
    1.00  beamtalk_ets_heir:heir_pid/0
    1.00  beamtalk_erlang_proxy:returns_raw_results/1
    1.00  beamtalk_erlang_proxy:native_call/4
    1.00  beamtalk_erlang_proxy:is_beamtalk_module/1
    1.00  beamtalk_erlang_proxy:coerce_result_no_charlist/1
    1.00  beamtalk_erlang_proxy:coerce_ffi_result/2
    1.00  beamtalk_erlang_proxy:apply_with_coercion/5
    1.00  beamtalk_dispatch:check_extension/2
    1.00  beamtalk_class_vars:uninstall/1
    1.00  beamtalk_class_vars:snapshot/0
    1.00  beamtalk_class_vars:key_for_tag/1
    1.00  beamtalk_class_vars:install/2
    1.00  beamtalk_class_vars:assert_absent/1
    1.00  beamtalk_class_registry:whereis_class/1
    1.00  beamtalk_class_registry:registry_name/1
    1.00  beamtalk_class_registry:record_class_state_snapshot/2
    1.00  beamtalk_class_registry:ensure_class_state_table/0
    1.00  beamtalk_class_registry:class_name_for_pid/1
    1.00  beamtalk_class_metadata:row/1
    1.00  beamtalk_class_metadata:lookup_direct_class_method/2
    1.00  beamtalk_class_metadata:lookup_class_method_fun/2
    1.00  beamtalk_class_metadata:has_runtime_class_methods/1
    1.00  beamtalk_class_metadata:field/2
    1.00  beamtalk_class_dispatch:run_with_class_vars/3
    1.00  beamtalk_class_dispatch:receiver_tag/1
    1.00  beamtalk_class_dispatch:lookup_direct_call/3
    1.00  beamtalk_class_dispatch:local_session_context/0
    1.00  beamtalk_class_dispatch:invoke_class_method/7
    1.00  beamtalk_class_dispatch:handle_class_method_call/6
    1.00  beamtalk_class_dispatch:class_send_with_recovery/3
    1.00  beamtalk_class_dispatch:class_send_plain/3
    1.00  beamtalk_class_dispatch:class_send_dispatch/3
    1.00  beamtalk_class_dispatch:class_send/3
    1.00  beamtalk_class_dispatch:class_method_fun_name/1
    1.00  beamtalk_class_dispatch:apply_compiled_class_method/6
    1.00  beamtalk_class_dispatch:apply_class_method_in_context/6
    1.00  beamtalk_class_dispatch:-invoke_class_method/7-fun-0-/6
    1.00  beamtalk_class_dispatch:-class_send_plain/3-fun-4-/3
    1.00  beamtalk_class_derived_cache:fetch/3
    1.00  atomics:get/2
```

Elixir `ok_match`:

```
2.0 calls/op to 2 distinct functions
    1.00  Elixir.VeBench:ok_match_loop/4
    1.00  Elixir.VeBench.Helpers:ok/1
```

### 9b r isOk / match?({:ok, _}, r)

Beamtalk `resultIsOk:r:`:

```
19.0 calls/op to 19 distinct functions
    1.00  bt@vs_elixir_bench@vb_bench:-class_resultIsOk:r:/3-repeat/2-0-/4
    1.00  persistent_term:get/2
    1.00  maps:find/2
    1.00  ets:lookup/2
    1.00  erlang:module_loaded/1
    1.00  bt@stdlib@result:isOk/1
    1.00  bt@stdlib@result:dispatch/3
    1.00  beamtalk_tagged_map:class_of/1
    1.00  beamtalk_primitive:tagged_map_dispatch/1
    1.00  beamtalk_primitive:tagged_class_dispatch/1
    1.00  beamtalk_primitive:send_map/3
    1.00  beamtalk_primitive:send/3
    1.00  beamtalk_primitive:resolved_module_loaded/2
    1.00  beamtalk_primitive:-tagged_class_dispatch/1-fun-0-/2
    1.00  beamtalk_message_dispatch:send/3
    1.00  beamtalk_message_dispatch:is_actor/1
    1.00  beamtalk_message_dispatch:classify_receiver/1
    1.00  beamtalk_class_derived_cache:fetch/3
    1.00  atomics:get/2
```

Elixir `ok_is_ok`:

```
1.0 calls/op to 1 distinct functions
    1.00  Elixir.VeBench:ok_is_ok_loop/4
```

### 10a Dictionary at:put: / Map.put

Beamtalk `dictPut:d:`:

```
14.0 calls/op to 13 distinct functions
    2.00  maps:find/2
    1.00  bt@vs_elixir_bench@vb_bench:-class_dictPut:d:/3-repeat/2-0-/4
    1.00  maps:put/3
    1.00  bt@stdlib@dictionary:dispatch/3
    1.00  bt@stdlib@dictionary:at:put:/3
    1.00  beamtalk_tagged_map:class_of/1
    1.00  beamtalk_tagged_map:class_name_from_mod/2
    1.00  beamtalk_primitive:tagged_map_dispatch/1
    1.00  beamtalk_primitive:send_map/3
    1.00  beamtalk_primitive:send/3
    1.00  beamtalk_message_dispatch:send/3
    1.00  beamtalk_message_dispatch:is_actor/1
    1.00  beamtalk_message_dispatch:classify_receiver/1
```

Elixir `map_put`:

```
2.0 calls/op to 2 distinct functions
    1.00  Elixir.VeBench:map_put_loop/4
    1.00  maps:put/3
```

### 10b Dictionary at: / Map.get

Beamtalk `dictAt:d:`:

```
13.0 calls/op to 12 distinct functions
    2.00  maps:find/2
    1.00  bt@vs_elixir_bench@vb_bench:-class_dictAt:d:/3-repeat/2-0-/4
    1.00  bt@stdlib@dictionary:dispatch/3
    1.00  bt@stdlib@dictionary:at:/2
    1.00  beamtalk_tagged_map:class_of/1
    1.00  beamtalk_tagged_map:class_name_from_mod/2
    1.00  beamtalk_primitive:tagged_map_dispatch/1
    1.00  beamtalk_primitive:send_map/3
    1.00  beamtalk_primitive:send/3
    1.00  beamtalk_message_dispatch:send/3
    1.00  beamtalk_message_dispatch:is_actor/1
    1.00  beamtalk_message_dispatch:classify_receiver/1
```

Elixir `map_get`:

```
3.0 calls/op to 3 distinct functions
    1.00  Elixir.VeBench:map_get_loop/4
    1.00  Elixir.Map:get/3
    1.00  Elixir.Map:get/2
```

### 10c List add: (100 elements) / l ++ [x]

Beamtalk `listAdd:list:`:

```
11.0 calls/op to 11 distinct functions
    1.00  bt@vs_elixir_bench@vb_bench:-class_listAdd:list:/3-repeat/2-0-/4
    1.00  erlang:++/2
    1.00  bt@stdlib@list:dispatch/3
    1.00  bt@stdlib@list:add:/2
    1.00  beamtalk_primitive:send/3
    1.00  beamtalk_primitive:module_for_value/2
    1.00  beamtalk_primitive:module_for_value/1
    1.00  beamtalk_primitive:dispatch_via_module/3
    1.00  beamtalk_message_dispatch:send/3
    1.00  beamtalk_message_dispatch:is_actor/1
    1.00  beamtalk_message_dispatch:classify_receiver/1
```

Elixir `list_append`:

```
2.0 calls/op to 2 distinct functions
    1.00  Elixir.VeBench:list_append_loop/4
    1.00  erlang:++/2
```

### 10d List includes: (100 elements) / Enum.member?

Beamtalk `listIncludes:list:`:

```
12.0 calls/op to 12 distinct functions
    1.00  bt@vs_elixir_bench@vb_bench:-class_listIncludes:list:/3-repeat/2-0-/4
    1.00  lists:member/2
    1.00  bt@stdlib@list:includes:/2
    1.00  bt@stdlib@list:dispatch/3
    1.00  beamtalk_primitive:send/3
    1.00  beamtalk_primitive:module_for_value/2
    1.00  beamtalk_primitive:module_for_value/1
    1.00  beamtalk_primitive:dispatch_via_module/3
    1.00  beamtalk_message_dispatch:send/3
    1.00  beamtalk_message_dispatch:is_actor/1
    1.00  beamtalk_message_dispatch:classify_receiver/1
    1.00  beamtalk_equality:member/2
```

Elixir `list_member`:

```
4.0 calls/op to 4 distinct functions
    1.00  Elixir.VeBench:list_member_loop/4
    1.00  lists:member/2
    1.00  Elixir.Enum:member?/2
    1.00  Elixir.Enum:__in__/2
```

### 10e String ++ / <>

Beamtalk `stringConcat:s:`:

```
3.0 calls/op to 3 distinct functions
    1.00  bt@vs_elixir_bench@vb_bench:-class_stringConcat:s:/3-repeat/2-0-/4
    1.00  erlang:iolist_to_binary/1
    1.00  erlang:binary_to_list/1
```

Elixir `binary_concat`:

```
1.0 calls/op to 1 distinct functions
    1.00  Elixir.VeBench:binary_concat_loop/4
```

### 11a 1 to: 100 do: summing / recursive sum

Beamtalk `sumToDo:`:

```
102.0 calls/op to 2 distinct functions
  101.00  bt@vs_elixir_bench@vb_bench:-class_sumToDo:/2-loop/2-1-/2
    1.00  bt@vs_elixir_bench@vb_bench:-class_sumToDo:/2-repeat/2-0-/3
```

Elixir `sum_recursive`:

```
102.0 calls/op to 2 distinct functions
  101.00  Elixir.VeBench:sum/3
    1.00  Elixir.VeBench:sum_recursive_loop/4
```

### 11b a + 1, a :: Integer / a + 1

Beamtalk `add:a:`:

```
1.0 calls/op to 1 distinct functions
    1.00  bt@vs_elixir_bench@vb_bench:-class_add:a:/3-repeat/2-0-/4
```

Elixir `add`:

```
1.0 calls/op to 1 distinct functions
    1.00  Elixir.VeBench:add_loop/4
```

### 11c a + 1, a untyped / a + 1

Beamtalk `addUntyped:a:`:

```
1.0 calls/op to 1 distinct functions
    1.00  bt@vs_elixir_bench@vb_bench:-class_addUntyped:a:/3-repeat/2-0-/4
```

Elixir `add`:

```
1.0 calls/op to 1 distinct functions
    1.00  Elixir.VeBench:add_loop/4
```

### 11d a > 0 ifTrue: [1] ifFalse: [2] / if

Beamtalk `ifTrueIfFalse:a:`:

```
11.0 calls/op to 11 distinct functions
    1.00  bt@vs_elixir_bench@vb_bench:-class_ifTrueIfFalse:a:/3-repeat/2-0-/4
    1.00  bt@vs_elixir_bench@vb_bench:-class_ifTrueIfFalse:a:/3-anonymous-2-/0
    1.00  bt@stdlib@true:ifTrue:ifFalse:/3
    1.00  bt@stdlib@true:dispatch/3
    1.00  beamtalk_primitive:send/3
    1.00  beamtalk_primitive:module_for_value/2
    1.00  beamtalk_primitive:module_for_value/1
    1.00  beamtalk_primitive:dispatch_via_module/3
    1.00  beamtalk_message_dispatch:send/3
    1.00  beamtalk_message_dispatch:is_actor/1
    1.00  beamtalk_message_dispatch:classify_receiver/1
```

Elixir `if_else`:

```
1.0 calls/op to 1 distinct functions
    1.00  Elixir.VeBench:if_else_loop/4
```

### 12a p class / p.__struct__

Beamtalk `classOf:p:`:

```
20.0 calls/op to 17 distinct functions
    2.00  erlang:list_to_atom/1
    2.00  erlang:atom_to_list/1
    2.00  erlang:++/2
    1.00  bt@vs_elixir_bench@vb_bench:-class_classOf:p:/3-repeat/2-0-/4
    1.00  maps:find/2
    1.00  ets:lookup_element/4
    1.00  erlang:whereis/1
    1.00  beamtalk_tagged_map:class_of/2
    1.00  beamtalk_primitive:class_of_object_inner/1
    1.00  beamtalk_primitive:class_of_object_by_name/1
    1.00  beamtalk_primitive:class_of_object/1
    1.00  beamtalk_primitive:class_of/1
    1.00  beamtalk_class_registry:whereis_class/1
    1.00  beamtalk_class_registry:registry_name/1
    1.00  beamtalk_class_registry:class_object_tag/1
    1.00  beamtalk_class_metadata:lookup_module/1
    1.00  beamtalk_class_metadata:field/2
```

Elixir `struct_of`:

```
1.0 calls/op to 1 distinct functions
    1.00  Elixir.VeBench:struct_of_loop/4
```

### 12b p isKindOf: VbPoint / is_struct(p, Point)

Beamtalk `isKindOf:p:`:

```
266.0 calls/op to 127 distinct functions
   16.00  ets:lookup/2
    9.00  ets:lookup_element/4
    9.00  beamtalk_class_metadata:field/2
    6.00  lists:member/2
    6.00  ets:info/1
    6.00  erlang:list_to_atom/1
    6.00  erlang:atom_to_list/1
    6.00  erlang:++/2
    6.00  beamtalk_class_registry:class_name_for_pid/1
    5.00  erlang:put/2
    5.00  erlang:erase/1
    5.00  beamtalk_object_class:restore_dict_key/2
    5.00  beamtalk_hierarchy:walk_ancestors/4
    5.00  beamtalk_extensions:lookup/2
    4.00  erlang:function_exported/3
    4.00  beamtalk_class_metadata:row/1
    4.00  beamtalk_class_metadata:lookup_superclass/1
    4.00  beamtalk_class_metadata:lookup_module/1
    4.00  beamtalk_class_dispatch:superclass_from_ets/1
    3.00  erlang:whereis/1
    3.00  erlang:module_loaded/1
    3.00  beamtalk_object_class:module_name_safe/1
    3.00  beamtalk_message_dispatch:send/3
    3.00  beamtalk_message_dispatch:is_actor/1
    3.00  beamtalk_message_dispatch:classify_receiver/1
    3.00  beamtalk_class_registry:whereis_class/1
    3.00  beamtalk_class_registry:registry_name/1
    3.00  beamtalk_class_registry:class_object_tag/1
    3.00  beamtalk_class_metadata:lookup_methods/1
    3.00  beamtalk_class_dispatch:-walk_class_method_chain/3-fun-0-/3
    2.00  maps:find/2
    2.00  gen_server:try_handle_call/4
    2.00  gen_server:reply/2
    2.00  gen_server:loop/5
    2.00  gen_server:loop/4
    2.00  gen_server:handle_msg/3
    2.00  gen_server:decode_msg/6
    2.00  gen_server:decode_msg/4
    2.00  gen_server:cancel_timer/1
    2.00  gen:reply/2
    2.00  gen:do_call/4
    2.00  gen:call/4
    2.00  erlang:send/3
    2.00  erlang:monitor/3
    2.00  erlang:demonitor/2
    2.00  code:ensure_loaded/1
    2.00  beamtalk_primitive:send/3
    2.00  beamtalk_object_class:handle_call/3
    2.00  beamtalk_hierarchy:walk_ancestors/3
    2.00  beamtalk_dispatch:check_extension/2
    2.00  beamtalk_class_registry:is_class_name/1
    1.00  bt@vs_elixir_bench@vb_bench:-class_isKindOf:p:/3-repeat/2-0-/4
    1.00  persistent_term:get/2
    1.00  gen_server:call/3
    1.00  gen_server:call/2
    1.00  gen:call/3
    1.00  ets:member/2
    1.00  erlang:atom_to_binary/2
    1.00  bt@vs_elixir_bench@vb_point:dispatch/3
    1.00  bt@stdlib@value:dispatch/3
    1.00  bt@stdlib@true:ifTrue:ifFalse:/3
    1.00  bt@stdlib@true:dispatch/3
    1.00  bt@stdlib@object:isKindOf:/2
    1.00  bt@stdlib@object:dispatch/3
    1.00  bt@stdlib@class:has_method/1
    1.00  bt@stdlib@class:dispatch/4
    1.00  bt@stdlib@class:dispatch/3
    1.00  bt@stdlib@behaviour:includesBehaviour:/2
    1.00  bt@stdlib@behaviour:has_method/1
    1.00  bt@stdlib@behaviour:dispatch/3
    1.00  bt@stdlib@behaviour:-includesBehaviour:/2-anonymous-1-/0
    1.00  binary:part/3
    1.00  beamtalk_tagged_map:class_of/2
    1.00  beamtalk_tagged_map:class_of/1
    1.00  beamtalk_primitive:value_type_send/5
    1.00  beamtalk_primitive:tagged_map_dispatch/1
    1.00  beamtalk_primitive:tagged_class_dispatch/1
    1.00  beamtalk_primitive:send_map/3
    1.00  beamtalk_primitive:resolved_module_loaded/2
    1.00  beamtalk_primitive:module_for_value/2
    1.00  beamtalk_primitive:module_for_value/1
    1.00  beamtalk_primitive:is_ivar_method/1
    1.00  beamtalk_primitive:dispatch_via_module/3
    1.00  beamtalk_primitive:class_of_object_inner/1
    1.00  beamtalk_primitive:class_of_object_by_name/1
    1.00  beamtalk_primitive:class_of_object/1
    1.00  beamtalk_primitive:class_of/1
    1.00  beamtalk_primitive:-tagged_class_dispatch/1-fun-0-/2
    1.00  beamtalk_object_class:seed_session_context_from/1
    1.00  beamtalk_object_class:reflection_class_name/1
    1.00  beamtalk_object_class:probe_local_method/4
    1.00  beamtalk_object_class:module_name/1
    1.00  beamtalk_object_class:local_method_defined/2
    1.00  beamtalk_object_class:has_method_local/2
    1.00  beamtalk_object_class:dispatch_class_method/5
    1.00  beamtalk_object_class:class_send/3
    1.00  beamtalk_object_class:class_name_safe/1
    1.00  beamtalk_object_class:caller_pid/1
    1.00  beamtalk_object_class:adopt_entry_group_leader/1
    1.00  beamtalk_object_class:-seed_session_context_from/1-fun-0-/5
    1.00  beamtalk_object_class:-has_method_local/2-fun-0-/1
    1.00  beamtalk_object_class:-adopt_entry_group_leader/1-fun-2-/0
    1.00  beamtalk_extensions:has/2
    1.00  beamtalk_dispatch:lookup_in_class_chain/5
    1.00  beamtalk_dispatch:lookup/5
    1.00  beamtalk_dispatch:is_supervisor_instance/1
    1.00  beamtalk_dispatch:invoke_step/6
    1.00  beamtalk_dispatch:invoke_method/6
    1.00  beamtalk_dispatch:class_chain_step/6
    1.00  beamtalk_dispatch:-lookup_in_class_chain/5-fun-0-/6
    1.00  beamtalk_class_registry:is_class_object/1
    1.00  beamtalk_class_metadata:lookup_instance_methods/1
    1.00  beamtalk_class_metadata:lookup_direct_class_method/2
    1.00  beamtalk_class_dispatch:walk_class_method_chain/3
    1.00  beamtalk_class_dispatch:try_class_chain_fallthrough/3
    1.00  beamtalk_class_dispatch:lookup_direct_call/3
    1.00  beamtalk_class_dispatch:local_session_context/0
    1.00  beamtalk_class_dispatch:is_test_execution_selector/1
    1.00  beamtalk_class_dispatch:handle_class_method_call/6
    1.00  beamtalk_class_dispatch:find_class_method_in_chain/2
    1.00  beamtalk_class_dispatch:class_send_with_recovery/3
    1.00  beamtalk_class_dispatch:class_send_plain/3
    1.00  beamtalk_class_dispatch:class_send_dispatch/3
    1.00  beamtalk_class_dispatch:class_send/3
    1.00  beamtalk_class_dispatch:-class_send_plain/3-fun-4-/3
    1.00  beamtalk_class_derived_cache:fetch/3
    1.00  atomics:get/2
```

Elixir `is_struct_check`:

```
1.0 calls/op to 1 distinct functions
    1.00  Elixir.VeBench:is_struct_check_loop/4
```
