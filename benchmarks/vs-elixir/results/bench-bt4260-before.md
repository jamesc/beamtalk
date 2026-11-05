# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 04:01:47 up 33 min,  0 user,  load average: 2.92, 3.81, 3.65
# rounds 3
# load average after: 04:02:03 up 33 min,  0 user,  load average: 2.71, 3.72, 3.62

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 0b | timesRepeat: whose block assigns nothing | 44.7 | 3.01 | 2.00 | 0.05 | 0.00 | 0.00 | - | - | - | 3.01 | 2.00 | send(N, 'timesRepeat:', [fun]); Integer>>timesRepeat: dispatches ifTrue:ifFalse: per step |
| 1a | getter on a sealed Value: p x / p.x | 8.40 | 3.00 | 0.00 | 2.96 | 0.00 | 0.00 | 2.8 | - | - | 3.00 | 0.00 | tag-guarded direct VbPoint:dispatch/3 (BT-4069); mismatch falls back to send/3 |
| 1b | getter on an open Value: p x / p.x | 8.50 | 3.00 | 0.00 | 3.30 | 0.00 | 0.00 | 2.6 | - | - | 3.00 | 0.00 | send(P, x, []) -> classify, tagged-map class lookup (ETS + cache), Mod:dispatch/3 (open receiver stays dynamic) |
| 1c | method on a sealed Value: p sum / Point.sum(p) | 9.50 | 3.00 | 0.00 | 4.65 | 2.00 | 0.00 | 2.0 | 1.5 | - | 1.00 | 0.00 | send(P, sum, []), same path as 1a |
| 2a | functional update: p withX: 3 / %{p \| x: 3} | 39.5 | 4.01 | 6.00 | 48.8 | 0.00 | 6.00 | 0.8 | - | 1.0 | 4.01 | 0.00 | send(P, 'withX:', [3]), same path as 1a + maps:put |
| 2b | construct: VbPoint x: a y: 4 / %Point{x: a, y: 4} | 118 | 3.04 | 6.00 | 93.8 | 0.05 | 6.00 | 1.3 | - | 1.0 | 2.99 | 0.00 | whereis_class + class_send: gen_server:call into the VbPoint class process |
| 2c | construct via new: VbPoint new: #{#x => a, #y => 4} / struct!(Point, %{x: a, y: 4}) | 983 | 37.1 | 130 | 169 | 26.0 | 36.0 | 5.8 | 1.4 | 3.6 | 11.1 | 94.0 | class_send('new:') -> direct constructor: VbPoint:new/1 in the caller -> check_init_args/5, declared fields from one {declared_fields, C} derived-cache hit (BT-4281) |
| 3a | `class sealed` method, sealed class / Mod.fun(arg) | 21.4 | 3.00 | 0.00 | 2.60 | 2.00 | 0.00 | 8.3 | 1.5 | - | 1.00 | 0.00 | guarded direct call: one persistent_term:get of the {Mod, Fun, Tag} static call key, whose value is ClassRef, then Mod:'class_double:'(ClassRef, 21) (BT-4316) |
| 3b | class method, sealed class / Mod.fun(arg) | 20.2 | 3.00 | 0.00 | 3.02 | 2.00 | 0.00 | 6.7 | 1.5 | - | 1.00 | 0.00 | guarded direct call, as 3a: `class sealed` plays no part (ADR 0142, BT-4301) |
| 3c | class method, open class / Mod.fun(arg) | 18.4 | 3.00 | 0.00 | 3.35 | 2.00 | 0.00 | 5.5 | 1.5 | - | 1.00 | 0.00 | guarded direct call, as 3a: open class (ADR 0142, BT-4301) |
| 3d | class method, class in a variable / mod.fun(arg) | 413 | 21.1 | 42.0 | 17.1 | 2.00 | 0.00 | 24.2 | 10.5 | - | 19.1 | 42.0 | send(C, 'double:', [21]) -> class_send: runs in the caller (ADR 0142, BT-4300) |
| 3e | inherited class method, open stateless subclass / Mod.fun(arg) | 678 | 28.1 | 36.0 | 2.47 | 2.00 | 0.00 | 275.1 | 14.0 | - | 26.1 | 36.0 | whereis_class + class_send: inherited method resolved by the owner memo, runs in the caller (BT-4300) |
| 3f | class method, class with classState: / Mod.fun(arg) | 8691 | 201 | 576 | 3.99 | 2.00 | 0.00 | 2178.6 | 100.3 | - | 199 | 576 | whereis_class + class_send: gen_server:call; stays there under ADR 0142 (class state) |
| 4a | untyped receiver: o x / o.x | 478 | 25.1 | 28.0 | 2.72 | 0.00 | 0.00 | 175.8 | - | - | 25.1 | 28.0 | send(O, x, []) -> classify, tagged-map class lookup (ETS + cache), Mod:dispatch/3 (untyped receiver stays dynamic) |
| 4b | untyped receiver: o area / protocol | 458 | 25.1 | 28.0 | 19.2 | 7.00 | 0.00 | 23.8 | 3.6 | - | 18.1 | 28.0 | send(O, area, []), same path as 4a |
| 4c | typed open Value: s area / Square.area(s) | 9.19 | 3.00 | 0.00 | 3.61 | 2.00 | 0.00 | 2.5 | 1.5 | - | 1.00 | 0.00 | send(S, area, []), same path as 4a (open class stays dynamic) |
| 5a | inherited method: s base / defdelegate | 200 | 7.02 | 3.00 | 4.29 | 3.00 | 0.00 | 46.7 | 2.3 | - | 4.02 | 3.00 | send(S, base, []): VbSub:dispatch/3 misses, extensions lookup, VbBase:dispatch/3 |
| 5b | self-send to inherited: s viaSelf / local call | 203 | 9.02 | 3.00 | 6.15 | 4.00 | 0.00 | 33.0 | 2.3 | - | 5.02 | 3.00 | send(S, viaSelf, []), then `self base` is a second full send |
| 5c | runtime-resolved method: s hasField: #v / Map.has_key? | 467 | 25.1 | 28.0 | 5.53 | 0.00 | 0.00 | 84.5 | - | - | 25.1 | 28.0 | send(S, 'hasField:', [v]) -> beamtalk_reflection:has_field/2 |
| 6a | block value: x / fun.(x) | 7.98 | 2.00 | 0.00 | 2.60 | 2.00 | 0.00 | 3.1 | 1.0 | - | 0.00 | 0.00 | is_function(Blk, 1) guard, then apply Blk(41) |
| 6b | do: summing 100 elements / Enum.reduce | 3858 | 302 | 404 | 266 | 303 | 0.00 | 14.5 | 1.0 | - | -0.69 | 404 | lists:foldl over a fun that maps:get/maps:put `sum` in a state map; + with coercion try |
| 6c | do: over 100, no assignment / Enum.each | 792 | 303 | 0.00 | 806 | 303 | 0.00 | 1.0 | 1.0 | - | 0.00 | 0.00 | is_list guard, lists:foreach(fun) |
| 6d | collect: 100 elements / Enum.map | 2885 | 403 | 200 | 3208 | 404 | 200 | 0.9 | 1.0 | 1.0 | -1.01 | 0.00 | is_list guard, lists:map(fun); * with is_number guard |
| 6e | inject:into: 100 elements / Enum.reduce | 715 | 302 | 0.00 | 340 | 303 | 0.00 | 2.1 | 1.0 | - | -1.00 | 0.00 | is_list guard, lists:foldl(fun); + with coercion try |
| 7a | actor sync read: c count / GenServer.call | 2440 | 89.2 | 146 | 1655 | 37.1 | 56.0 | 1.5 | 2.4 | 2.6 | 52.1 | 90.0 | send(C, count, []) -> beamtalk_actor:sync_send, gen_server:call + telemetry/trace/ctx hooks both sides |
| 7b | actor state update: c increment / GenServer.call | 2602 | 92.3 | 152 | 1485 | 37.1 | 56.0 | 1.8 | 2.5 | 2.7 | 55.3 | 96.0 | as 7a |
| 7c | actor cast: c increment! / GenServer.cast | 1226 | 68.6 | 77.0 | 511 | 18.8 | 12.0 | 2.4 | 3.7 | 6.4 | 49.9 | 65.0 | beamtalk_message_dispatch:cast -> gen_server:cast + the same hooks |
| 7d | actor method doing self hasField: / GenServer.call | 2964 | 142 | 223 | 1412 | 37.1 | 56.0 | 2.1 | 3.8 | 4.0 | 105 | 167 | as 7a; in the actor `self hasField:` misses the module and walks the runtime method resolver |
| 7e | inherited actor method doing self increment / delegated GenServer.call | 3053 | 127 | 203 | 1387 | 42.1 | 56.0 | 2.2 | 3.0 | 3.6 | 85.4 | 147 | as 7a; the subclass's dispatch/4 default arm takes the memoised inherited_target/2 verdict to the owner (BT-4280) |
| 7f | actor method doing self increment / GenServer.call | 2652 | 97.4 | 159 | 1516 | 41.1 | 56.0 | 1.7 | 2.4 | 2.8 | 56.3 | 103 | as 7a; the self-send is a static call inside the class module |
| 7g | actor method reading two getters of a typed sealed Value / GenServer.call | 2413 | 97.3 | 164 | 1298 | 37.0 | 56.0 | 1.9 | 2.6 | 2.9 | 60.2 | 108 | as 7a; each `p x` is a tag + __is_slot/1-guarded maps:get with no cell bracket, the `+` a guarded BIF with none either (BT-4279) |
| 8a | on: Error do: without a raise / try-rescue | 51.7 | 5.00 | 0.00 | 2.81 | 0.00 | 0.00 | 18.4 | - | - | 5.00 | 0.00 | `Error` class ref built per entry (whereis_class + module_name gen_server:call), class_vars snapshot, try |
| 8b | on: Error do: with a raise / raise-rescue | 1877 | 89.3 | 141 | 419 | 13.2 | 36.0 | 4.5 | 6.8 | 3.9 | 76.1 | 105 | as 8a + `Error signal:` raised in the caller behind its caller-safe raise key (one persistent_term read, BT-4270), wrapped and class-matched; frames built only on read |
| 9a | (Result ok: x) value / {:ok, x} match | 1218 | 34.2 | 39.0 | 53.0 | 2.02 | 3.00 | 23.0 | 17.0 | 13.0 | 32.2 | 36.0 | class_send(Result, 'ok:') gen_server:call; `value` as a block-or-send case |
| 9b | r isOk / match?({:ok, _}, r) | 8.92 | 3.00 | 0.00 | 0.26 | 0.00 | 0.00 | - | - | - | 3.00 | 0.00 | tag-guarded direct Result:dispatch/3 (BT-4069); mismatch falls back to send/3 |
| 10a | Dictionary at:put: / Map.put | 149 | 4.04 | 10.0 | 36.5 | 1.01 | 6.00 | 4.1 | 4.0 | 1.7 | 3.04 | 4.00 | send(D, 'at:put:', ...) -> Dictionary:dispatch/3 -> maps:put |
| 10b | Dictionary at: / Map.get | 13.9 | 3.00 | 0.00 | 5.49 | 3.00 | 0.00 | 2.5 | 1.0 | - | -0.00 | 0.00 | send(D, 'at:', ...) -> Dictionary:dispatch/3 -> maps:get |
| 10c | List add: (100 elements) / l ++ [x] | 746 | 7.68 | 202 | 828 | 4.64 | 200 | 0.9 | 1.7 | 1.0 | 3.04 | 2.00 | send(L, 'add:', [7]) -> List primitive (++) |
| 10d | List includes: (100 elements) / Enum.member? | 85.9 | 9.00 | 0.00 | 85.1 | 8.00 | 0.00 | 1.0 | 1.1 | - | 1.00 | 0.00 | send(L, 'includes:', [50]) -> List primitive (lists:member) |
| 10e | String ++ / <> | 262 | 9.07 | 11.0 | 207 | 0.05 | 8.00 | 1.3 | - | 1.4 | 9.02 | 3.00 | is_list guard, iolist_to_binary([binary_to_list(S), binary_to_list("!")]) |
| 11a | 1 to: 100 do: summing / recursive sum | 351 | 102 | 7.00 | 107 | 102 | 0.00 | 3.3 | 1.0 | - | 0.03 | 7.00 | letrec loop; + with is_number guard and coercion try |
| 11b | a + 1, a :: Integer / a + 1 | 0.45 | 0.00 | 0.00 | 0.44 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | erlang:'+'(A, 1) |
| 11c | a + 1, a untyped / a + 1 | 5.94 | 0.00 | 0.00 | 0.18 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | is_number guard, erlang:'+' |
| 11d | a > 0 ifTrue: [1] ifFalse: [2] / if | 0.35 | 0.00 | 0.00 | 0.59 | 0.00 | 0.00 | 0.6 | - | - | 0.00 | 0.00 | send(Bool, 'ifTrue:ifFalse:', [fun, fun]) -> True:dispatch/3 |
| 12a | p class / p.__struct__ | 149 | 1.04 | 5.00 | 3.25 | 0.00 | 0.00 | 45.9 | - | - | 1.04 | 5.00 | typed receiver (BT-4127): is_map + tag guard, erlang:whereis + class-object tuple with the pinned module; untyped: beamtalk_primitive:class_of_object derived-cache hit |
| 12b | p isKindOf: VbPoint / is_struct(p, Point) | 1462 | 39.2 | 58.0 | 1.72 | 0.00 | 0.00 | 852.3 | - | - | 39.2 | 58.0 | class ref built per call (whereis_class + module_name_safe) + send(P, 'isKindOf:') -> direct objectIsKindOf/2 call (no FFI proxy, BT-4410): one memoised {is_kind_of, C, B} lookup (BT-4221) |
| 12c | s isKindOf: String / is_binary(s) | 1287 | 35.2 | 50.0 | 2.16 | 0.00 | 0.00 | 595.0 | - | - | 35.2 | 50.0 | as 12b, but the String literal bakes its module (whereis only, no module_name_safe, BT-4411) and the send walks String/Binary/Collection/Value/Object dispatch (an extensions lookup per level) to Object>>isKindOf: |
| 13a | FFI call, integer result / apply | 109 | 14.0 | 4.00 | 8.07 | 1.00 | 0.00 | 13.5 | 14.0 | - | 13.0 | 4.00 | direct_call/4 ('erlang', 'phash2', [A], 'stock'): mode precomputed (BT-4153), integer fast path |
| 13b | FFI call, binary result, caller-typed / apply | 176 | 14.0 | 7.00 | 26.8 | 1.00 | 3.00 | 6.6 | 14.0 | 2.3 | 13.0 | 4.00 | direct_call/4 ('erlang', 'integer_to_binary', [A], 'stock'): binary fast path |
| 13c | FFI call, {ok, V} result / apply | 455 | 24.1 | 21.0 | 20.6 | 1.00 | 3.00 | 22.1 | 24.0 | 7.0 | 23.1 | 18.0 | direct_call/4 ('maps', 'find', [K, D], 'stock'): mode precomputed, {ok, V} still wraps to Result |
| 13d | FFI call, ok-atom result / apply | 173 | 13.0 | 7.00 | 2.60 | 2.00 | 0.00 | 66.6 | 6.5 | - | 11.0 | 7.00 | direct_call/4 ('vs_bench', 'ffi_ok', [], 'stock'): mode precomputed, ok still wraps to Result |
| 14a | typed Tuple at: / elem(t, 0) | 9.93 | 3.00 | 0.00 | 0.43 | 0.00 | 0.00 | - | - | - | 3.00 | 0.00 | is_tuple-guarded direct Tuple:dispatch/3 (BT-4294); 'at:' reads an in-range element inline (BT-4400) |
| 14b | class reference: last := VbPoint / Point | 450 | 11.1 | 16.0 | 0.03 | 0.00 | 0.00 | - | - | - | 11.1 | 16.0 | class reference per use: whereis of the compile-time registered name + module_name_safe (BT-4295) |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 0b | `baseline` | 0.05 | 0.00 | 0.00 |
| 1a | `getter_dot` | 2.96 | 0.00 | 0.00 |
| 1a | `getter_fun` | 3.55 | 2.00 | 0.00 |
| 1a | `getter_match` | 1.28 | 0.00 | 0.00 |
| 1b | `getter_dot` | 3.30 | 0.00 | 0.00 |
| 1c | `point_sum` | 4.65 | 2.00 | 0.00 |
| 2a | `with_x` | 48.8 | 0.00 | 6.00 |
| 2b | `construct` | 93.8 | 0.05 | 6.00 |
| 2c | `construct_map` | 169 | 26.0 | 36.0 |
| 2c | `construct` | 81.1 | 0.03 | 6.00 |
| 3a | `remote_call` | 2.60 | 2.00 | 0.00 |
| 3b | `remote_call` | 3.02 | 2.00 | 0.00 |
| 3c | `remote_call` | 3.35 | 2.00 | 0.00 |
| 3d | `dyn_remote_call` | 17.1 | 2.00 | 0.00 |
| 3d | `remote_call` | 2.78 | 2.00 | 0.00 |
| 3e | `remote_call` | 2.47 | 2.00 | 0.00 |
| 3f | `remote_call` | 3.99 | 2.00 | 0.00 |
| 4a | `getter_dot` | 2.72 | 0.00 | 0.00 |
| 4b | `protocol` | 19.2 | 7.00 | 0.00 |
| 4b | `apply3` | 19.2 | 2.00 | 0.00 |
| 4b | `dyn_module` | 18.4 | 2.00 | 0.00 |
| 4c | `square_area` | 3.61 | 2.00 | 0.00 |
| 4c | `protocol` | 20.3 | 7.00 | 0.00 |
| 5a | `delegate` | 4.29 | 3.00 | 0.00 |
| 5b | `delegate_via_self` | 6.15 | 4.00 | 0.00 |
| 5c | `has_key` | 5.53 | 0.00 | 0.00 |
| 6a | `fun_call` | 2.60 | 2.00 | 0.00 |
| 6b | `enum_reduce_sum` | 266 | 303 | 0.00 |
| 6b | `lists_foldl` | 302 | 302 | 0.00 |
| 6c | `enum_each` | 806 | 303 | 0.00 |
| 6d | `enum_map` | 3208 | 404 | 200 |
| 6e | `enum_reduce_sum` | 340 | 303 | 0.00 |
| 6e | `lists_foldl` | 304 | 302 | 0.00 |
| 7a | `gen_call` | 1655 | 37.1 | 56.0 |
| 7b | `gen_update` | 1485 | 37.1 | 56.0 |
| 7c | `gen_cast` | 511 | 18.8 | 12.0 |
| 7d | `gen_probe` | 1412 | 37.1 | 56.0 |
| 7e | `gen_via_self` | 1387 | 42.1 | 56.0 |
| 7f | `gen_via_self` | 1516 | 41.1 | 56.0 |
| 7g | `gen_sum_of` | 1298 | 37.0 | 56.0 |
| 8a | `try_no_raise` | 2.81 | 0.00 | 0.00 |
| 8b | `try_raise` | 419 | 13.2 | 36.0 |
| 9a | `ok_match` | 53.0 | 2.02 | 3.00 |
| 9b | `ok_is_ok` | 0.26 | 0.00 | 0.00 |
| 10a | `map_put` | 36.5 | 1.01 | 6.00 |
| 10b | `map_get` | 5.49 | 3.00 | 0.00 |
| 10c | `list_append` | 828 | 4.64 | 200 |
| 10d | `list_member` | 85.1 | 8.00 | 0.00 |
| 10e | `binary_concat` | 207 | 0.05 | 8.00 |
| 11a | `sum_recursive` | 107 | 102 | 0.00 |
| 11a | `sum_reduce_range` | 616 | 252 | 0.00 |
| 11b | `add` | 0.44 | 0.00 | 0.00 |
| 11c | `add` | 0.18 | 0.00 | 0.00 |
| 11d | `if_else` | 0.59 | 0.00 | 0.00 |
| 12a | `struct_of` | 3.25 | 0.00 | 0.00 |
| 12b | `is_struct_check` | 1.72 | 0.00 | 0.00 |
| 12c | `is_binary_check` | 2.16 | 0.00 | 0.00 |
| 13a | `apply_phash2` | 8.07 | 1.00 | 0.00 |
| 13a | `direct_phash2` | 8.17 | 1.00 | 0.00 |
| 13b | `apply_integer_to_binary` | 26.8 | 1.00 | 3.00 |
| 13b | `direct_integer_to_binary` | 29.4 | 1.00 | 3.00 |
| 13c | `apply_maps_find` | 20.6 | 1.00 | 3.00 |
| 13c | `direct_maps_find` | 29.0 | 1.00 | 3.00 |
| 13d | `apply_ffi_ok` | 2.60 | 2.00 | 0.00 |
| 14a | `tuple_elem` | 0.43 | 0.00 | 0.00 |
| 14b | `module_ref` | 0.03 | 0.00 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.02 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.10 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0b | bt | loopNoAssign: | 45.7 | 4.01 | 2.00 | 4.01 | 2.00 |
| 0b | ex | baseline | 1.16 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1a | bt | getterSealed:p: | 9.41 | 4.00 | 0.00 | 4.00 | 0.00 |
| 1a | ex | getter_dot | 4.07 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1a | ex | getter_fun | 4.65 | 3.00 | 0.00 | 3.00 | 0.00 |
| 1a | ex | getter_match | 2.38 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1b | bt | getterOpen:p: | 9.51 | 4.00 | 0.00 | 4.00 | 0.00 |
| 1b | ex | getter_dot | 4.40 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1c | bt | sumSealed:p: | 10.5 | 4.00 | 0.00 | 4.00 | 0.00 |
| 1c | ex | point_sum | 5.75 | 3.00 | 0.00 | 3.00 | 0.00 |
| 2a | bt | withX:p: | 40.5 | 5.01 | 6.00 | 5.01 | 6.00 |
| 2a | ex | with_x | 49.9 | 1.00 | 6.00 | 1.00 | 6.00 |
| 2b | bt | construct:a: | 119 | 4.04 | 6.00 | 4.04 | 6.00 |
| 2b | ex | construct | 94.9 | 1.05 | 6.00 | 1.05 | 6.00 |
| 2c | bt | constructMap:a: | 984 | 38.1 | 130 | 38.1 | 130 |
| 2c | ex | construct | 82.2 | 1.03 | 6.00 | 1.03 | 6.00 |
| 2c | ex | construct_map | 170 | 27.0 | 36.0 | 27.0 | 36.0 |
| 3a | bt | classSealedMethod: | 22.4 | 4.00 | 0.00 | 4.00 | 0.00 |
| 3a | ex | remote_call | 3.70 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3b | bt | sealedClassMethod: | 21.2 | 4.00 | 0.00 | 4.00 | 0.00 |
| 3b | ex | remote_call | 4.12 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3c | bt | openClassMethod: | 19.4 | 4.00 | 0.00 | 4.00 | 0.00 |
| 3c | ex | remote_call | 4.46 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3d | bt | dynClassMethod:c: | 414 | 22.1 | 42.0 | 22.1 | 42.0 |
| 3d | ex | dyn_remote_call | 18.2 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3d | ex | remote_call | 3.88 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3e | bt | inheritedClassMethod: | 679 | 29.1 | 36.0 | 29.1 | 36.0 |
| 3e | ex | remote_call | 3.57 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3f | bt | statefulClassMethod: | 8692 | 202 | 576 | 91.3 | 276 |
| 3f | ex | remote_call | 5.09 | 3.00 | 0.00 | 3.00 | 0.00 |
| 4a | bt | dynGetter:o: | 479 | 26.1 | 28.0 | 26.1 | 28.0 |
| 4a | ex | getter_dot | 3.82 | 1.00 | 0.00 | 1.00 | 0.00 |
| 4b | bt | dynArea:o: | 459 | 26.1 | 28.0 | 26.1 | 28.0 |
| 4b | ex | apply3 | 20.3 | 3.00 | 0.00 | 3.00 | 0.00 |
| 4b | ex | dyn_module | 19.5 | 3.00 | 0.00 | 3.00 | 0.00 |
| 4b | ex | protocol | 20.3 | 8.00 | 0.00 | 8.00 | 0.00 |
| 4c | bt | typedArea:s: | 10.2 | 4.00 | 0.00 | 4.00 | 0.00 |
| 4c | ex | protocol | 21.4 | 8.00 | 0.00 | 8.00 | 0.00 |
| 4c | ex | square_area | 4.71 | 3.00 | 0.00 | 3.00 | 0.00 |
| 5a | bt | inherited:s: | 201 | 8.02 | 3.00 | 8.02 | 3.00 |
| 5a | ex | delegate | 5.40 | 4.00 | 0.00 | 4.00 | 0.00 |
| 5b | bt | inheritedViaSelf:s: | 204 | 10.0 | 3.00 | 10.0 | 3.00 |
| 5b | ex | delegate_via_self | 7.25 | 5.00 | 0.00 | 5.00 | 0.00 |
| 5c | bt | hasField:s: | 468 | 26.1 | 28.0 | 26.1 | 28.0 |
| 5c | ex | has_key | 6.64 | 1.00 | 0.00 | 1.00 | 0.00 |
| 6a | bt | blockValue:blk: | 9.00 | 3.00 | 0.00 | 3.00 | 0.00 |
| 6a | ex | fun_call | 3.71 | 3.00 | 0.00 | 3.00 | 0.00 |
| 6b | bt | doSum:list: | 3859 | 303 | 404 | 303 | 404 |
| 6b | ex | enum_reduce_sum | 268 | 304 | 0.00 | 304 | 0.00 |
| 6b | ex | lists_foldl | 303 | 303 | 0.00 | 303 | 0.00 |
| 6c | bt | doNoAssign:list: | 793 | 304 | 0.00 | 304 | 0.00 |
| 6c | ex | enum_each | 807 | 304 | 0.00 | 304 | 0.00 |
| 6d | bt | collect:list: | 2886 | 404 | 200 | 404 | 200 |
| 6d | ex | enum_map | 3209 | 405 | 200 | 405 | 200 |
| 6e | bt | inject:list: | 716 | 303 | 0.00 | 303 | 0.00 |
| 6e | ex | enum_reduce_sum | 341 | 304 | 0.00 | 304 | 0.00 |
| 6e | ex | lists_foldl | 305 | 303 | 0.00 | 303 | 0.00 |
| 7a | bt | actorCall:c: | 2441 | 90.2 | 146 | 44.1 | 83.0 |
| 7a | ex | gen_call | 1656 | 38.1 | 56.0 | 17.1 | 33.0 |
| 7b | bt | actorUpdate:c: | 2603 | 93.3 | 152 | 44.1 | 83.0 |
| 7b | ex | gen_update | 1486 | 38.1 | 56.0 | 17.1 | 33.0 |
| 7c | bt | actorCast:c: | 1227 | 69.6 | 77.0 | 34.0 | 38.0 |
| 7c | ex | gen_cast | 512 | 19.8 | 12.0 | 7.02 | 3.00 |
| 7d | bt | actorProbe:c: | 2965 | 143 | 223 | 44.1 | 83.0 |
| 7d | ex | gen_probe | 1413 | 38.1 | 56.0 | 17.1 | 33.0 |
| 7e | bt | actorViaSelfSub:c: | 3054 | 128 | 203 | 44.1 | 83.0 |
| 7e | ex | gen_via_self | 1388 | 43.1 | 56.0 | 17.0 | 33.0 |
| 7f | bt | actorViaSelf:c: | 2653 | 98.4 | 159 | 44.1 | 83.0 |
| 7f | ex | gen_via_self | 1517 | 42.1 | 56.0 | 17.0 | 33.0 |
| 7g | bt | actorSumOf:c: | 2414 | 98.3 | 164 | 44.2 | 85.1 |
| 7g | ex | gen_sum_of | 1299 | 38.0 | 56.0 | 17.0 | 33.0 |
| 8a | bt | onDoNoRaise: | 52.7 | 6.00 | 0.00 | 6.00 | 0.00 |
| 8a | ex | try_no_raise | 3.91 | 1.00 | 0.00 | 1.00 | 0.00 |
| 8b | bt | onDoRaise: | 1878 | 90.3 | 141 | 90.3 | 141 |
| 8b | ex | try_raise | 420 | 14.2 | 36.0 | 14.2 | 36.0 |
| 9a | bt | resultOk: | 1219 | 35.2 | 39.0 | 35.2 | 39.0 |
| 9a | ex | ok_match | 54.1 | 3.02 | 3.00 | 3.02 | 3.00 |
| 9b | bt | resultIsOk:r: | 9.94 | 4.00 | 0.00 | 4.00 | 0.00 |
| 9b | ex | ok_is_ok | 1.36 | 1.00 | 0.00 | 1.00 | 0.00 |
| 10a | bt | dictPut:d: | 150 | 5.04 | 10.0 | 5.04 | 10.0 |
| 10a | ex | map_put | 37.6 | 2.01 | 6.00 | 2.01 | 6.00 |
| 10b | bt | dictAt:d: | 15.0 | 4.00 | 0.00 | 4.00 | 0.00 |
| 10b | ex | map_get | 6.60 | 4.00 | 0.00 | 4.00 | 0.00 |
| 10c | bt | listAdd:list: | 747 | 8.68 | 202 | 8.68 | 202 |
| 10c | ex | list_append | 829 | 5.64 | 200 | 5.64 | 200 |
| 10d | bt | listIncludes:list: | 86.9 | 10.0 | 0.00 | 10.0 | 0.00 |
| 10d | ex | list_member | 86.2 | 9.00 | 0.00 | 9.00 | 0.00 |
| 10e | bt | stringConcat:s: | 263 | 10.1 | 11.0 | 10.1 | 11.0 |
| 10e | ex | binary_concat | 208 | 1.05 | 8.00 | 1.05 | 8.00 |
| 11a | bt | sumToDo: | 352 | 103 | 7.00 | 103 | 7.00 |
| 11a | ex | sum_recursive | 108 | 103 | 0.00 | 103 | 0.00 |
| 11a | ex | sum_reduce_range | 617 | 253 | 0.00 | 253 | 0.00 |
| 11b | bt | add:a: | 1.46 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11b | ex | add | 1.55 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11c | bt | addUntyped:a: | 6.95 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11c | ex | add | 1.29 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11d | bt | ifTrueIfFalse:a: | 1.37 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11d | ex | if_else | 1.70 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12a | bt | classOf:p: | 150 | 2.04 | 5.00 | 2.04 | 5.00 |
| 12a | ex | struct_of | 4.36 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12b | bt | isKindOf:p: | 1463 | 40.2 | 58.0 | 40.2 | 58.0 |
| 12b | ex | is_struct_check | 2.82 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12c | bt | isKindOfBuiltin:s: | 1288 | 36.2 | 50.0 | 36.2 | 50.0 |
| 12c | ex | is_binary_check | 3.27 | 1.00 | 0.00 | 1.00 | 0.00 |
| 13a | bt | ffiInt:a: | 110 | 15.0 | 4.00 | 15.0 | 4.00 |
| 13a | ex | apply_phash2 | 9.17 | 2.00 | 0.00 | 2.00 | 0.00 |
| 13a | ex | direct_phash2 | 9.28 | 2.00 | 0.00 | 2.00 | 0.00 |
| 13b | bt | ffiBinary:a: | 177 | 15.0 | 7.00 | 15.0 | 7.00 |
| 13b | ex | apply_integer_to_binary | 27.9 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13b | ex | direct_integer_to_binary | 30.5 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13c | bt | ffiOkTuple:d: | 456 | 25.1 | 21.0 | 25.1 | 21.0 |
| 13c | ex | apply_maps_find | 21.7 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13c | ex | direct_maps_find | 30.1 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13d | bt | ffiOk: | 174 | 14.0 | 7.00 | 14.0 | 7.00 |
| 13d | ex | apply_ffi_ok | 3.71 | 3.00 | 0.00 | 3.00 | 0.00 |
| 14a | bt | tupleAt:t: | 10.9 | 4.00 | 0.00 | 4.00 | 0.00 |
| 14a | ex | tuple_elem | 1.54 | 1.00 | 0.00 | 1.00 | 0.00 |
| 14b | bt | classReference: | 451 | 12.1 | 16.0 | 12.1 | 16.0 |
| 14b | ex | module_ref | 1.14 | 1.00 | 0.00 | 1.00 | 0.00 |
