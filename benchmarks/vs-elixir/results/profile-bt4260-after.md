
### 2a functional update: p withX: 3 / %{p | x: 3}

Beamtalk `withX:p:`:

```
4.0 calls/op to 4 distinct functions
    1.00  bt@vs_elixir_bench@vb_bench:-class_withX:p:/3-repeat/2-0-/4
    1.00  maps:put/3
    1.00  bt@vs_elixir_bench@vb_point:withX:/2
    1.00  bt@vs_elixir_bench@vb_point:dispatch/3
```

Elixir `with_x`:

```
1.0 calls/op to 1 distinct functions
    1.00  Elixir.VeBench:with_x_loop/4
```

### 12c s isKindOf: String / is_binary(s)

Beamtalk `isKindOfBuiltin:s:`:

```
21.0 calls/op to 17 distinct functions
    2.00  persistent_term:get/2
    2.00  ets:lookup/2
    2.00  erlang:is_process_alive/1
    2.00  atomics:get/2
    1.00  bt@vs_elixir_bench@vb_bench:-class_isKindOfBuiltin:s:/3-repeat/2-0-/4
    1.00  erlang:whereis/1
    1.00  bt@stdlib@string:dispatch/3
    1.00  bt@stdlib@object:isKindOf:/2
    1.00  bt@stdlib@object:dispatch/3
    1.00  beamtalk_primitive:class_of/1
    1.00  beamtalk_dispatch:inherited_target/2
    1.00  beamtalk_class_derived_cache:fetch/3
    1.00  beamtalk_class_derived_cache:fetch/2
    1.00  beamtalk_behaviour_intrinsics:receiver_class_side/1
    1.00  beamtalk_behaviour_intrinsics:objectIsKindOf/2
    1.00  beamtalk_behaviour_intrinsics:kind_of_pids_alive/1
    1.00  beamtalk_behaviour_intrinsics:instance_is_kind_of/3
```

Elixir `is_binary_check`:

```
1.0 calls/op to 1 distinct functions
    1.00  Elixir.VeBench:is_binary_check_loop/4
```
