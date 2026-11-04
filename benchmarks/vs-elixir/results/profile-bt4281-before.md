### 2c construct via new: VbPoint new: #{#x => a, #y => 4} / struct!(Point, %{x: a, y: 4})

Beamtalk `constructMap:a:`:

```
95.0 calls/op to 35 distinct functions
   12.00  ets:lookup_element/4
   12.00  beamtalk_class_metadata:field/2
    8.00  erlang:++/2
    6.00  erlang:--/2
    5.00  erlang:function_exported/3
    4.00  beamtalk_class_metadata:read_meta_detailed/1
    4.00  beamtalk_class_metadata:read_meta/1
    4.00  beamtalk_class_metadata:lookup_superclass/1
    4.00  beamtalk_class_metadata:lookup_module/1
    4.00  beamtalk_class_metadata:lookup_instance_variables/1
    4.00  beamtalk_class_instantiation:own_declared_fields/1
    4.00  beamtalk_class_instantiation:declared_fields_walk/3
    2.00  ets:lookup/2
    1.00  bt@vs_elixir_bench@vb_bench:-class_constructMap:a:/3-repeat/2-0-/4
    1.00  maps:merge/2
    1.00  maps:keys/1
    1.00  lists:sort/1
    1.00  lists:member/2
    1.00  erlang:whereis/1
    1.00  bt@vs_elixir_bench@vb_point:new/1
    1.00  bt@vs_elixir_bench@vb_point:new/0
    1.00  bt@vs_elixir_bench@vb_point:__beamtalk_meta/0
    1.00  bt@stdlib@value:__beamtalk_meta/0
    1.00  bt@stdlib@proto_object:__beamtalk_meta/0
    1.00  bt@stdlib@object:__beamtalk_meta/0
    1.00  beamtalk_object_class:class_send/3
    1.00  beamtalk_class_registry:class_name_for_pid/1
    1.00  beamtalk_class_metadata:row/1
    1.00  beamtalk_class_metadata:lookup_direct_constructor/2
    1.00  beamtalk_class_instantiation:declared_fields/2
    1.00  beamtalk_class_instantiation:check_init_args/5
    1.00  beamtalk_class_dispatch:lookup_direct_constructor_module/3
    1.00  beamtalk_class_dispatch:lookup_direct_constructor/3
    1.00  beamtalk_class_dispatch:class_send_plain/3
    1.00  beamtalk_class_dispatch:class_send/3
```

Elixir `construct_map`:

```
18.0 calls/op to 13 distinct functions
    3.00  maps:fold_1/4
    2.00  maps:try_next/2
    2.00  Elixir.VeBench.Point:-__struct__/1-fun-0-/2
    2.00  Elixir.Enum:-reduce/3-fun-0-/4
    1.00  Elixir.VeBench:construct_map_loop/4
    1.00  maps:next/1
    1.00  maps:iterator/2
    1.00  maps:iterator/1
    1.00  maps:fold/3
    1.00  Elixir.VeBench.Point:__struct__/1
    1.00  Elixir.Kernel:validate_struct!/3
    1.00  Elixir.Kernel:struct!/2
    1.00  Elixir.Enum:reduce/3
```
