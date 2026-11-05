
### 12c s isKindOf: String / is_binary(s)

Beamtalk `isKindOfBuiltin:s:`:

```
27.0 calls/op to 19 distinct functions
    5.00  ets:lookup/2
    4.00  beamtalk_extensions:lookup/2
    2.00  erlang:is_process_alive/1
    1.00  bt@vs_elixir_bench@vb_bench:-class_isKindOfBuiltin:s:/3-repeat/2-0-/4
    1.00  persistent_term:get/2
    1.00  erlang:whereis/1
    1.00  bt@stdlib@value:dispatch/3
    1.00  bt@stdlib@string:dispatch/3
    1.00  bt@stdlib@object:isKindOf:/2
    1.00  bt@stdlib@object:dispatch/3
    1.00  bt@stdlib@collection:dispatch/3
    1.00  bt@stdlib@binary:dispatch/3
    1.00  beamtalk_primitive:class_of/1
    1.00  beamtalk_class_derived_cache:fetch/3
    1.00  beamtalk_behaviour_intrinsics:receiver_class_side/1
    1.00  beamtalk_behaviour_intrinsics:objectIsKindOf/2
    1.00  beamtalk_behaviour_intrinsics:kind_of_pids_alive/1
    1.00  beamtalk_behaviour_intrinsics:instance_is_kind_of/3
    1.00  atomics:get/2
```

Elixir `is_binary_check`:

```
1.0 calls/op to 1 distinct functions
    1.00  Elixir.VeBench:is_binary_check_loop/4
```
