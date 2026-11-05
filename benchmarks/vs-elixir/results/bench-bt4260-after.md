# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 05:50:38 up  2:22,  0 user,  load average: 2.00, 5.18, 9.50
# rounds 3
# load average after: 05:50:53 up  2:22,  0 user,  load average: 2.89, 5.23, 9.45

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 0b | timesRepeat: whose block assigns nothing | 42.6 | 3.01 | 2.00 | -0.15 | 0.00 | 0.00 | - | - | - | 3.01 | 2.00 | send(N, 'timesRepeat:', [fun]); Integer>>timesRepeat: dispatches ifTrue:ifFalse: per step |
| 1a | getter on a sealed Value: p x / p.x | 13.3 | 3.00 | 0.00 | 2.61 | 0.00 | 0.00 | 5.1 | - | - | 3.00 | 0.00 | tag-guarded direct VbPoint:dispatch/3 (BT-4069); mismatch falls back to send/3 |
| 1b | getter on an open Value: p x / p.x | 13.2 | 3.00 | 0.00 | 3.07 | 0.00 | 0.00 | 4.3 | - | - | 3.00 | 0.00 | send(P, x, []) -> classify, tagged-map class lookup (ETS + cache), Mod:dispatch/3 (open receiver stays dynamic) |
| 1c | method on a sealed Value: p sum / Point.sum(p) | 17.2 | 3.00 | 0.00 | 3.50 | 2.00 | 0.00 | 4.9 | 1.5 | - | 1.00 | 0.00 | send(P, sum, []), same path as 1a |
| 2a | functional update: p withX: 3 / %{p \| x: 3} | 76.6 | 4.02 | 6.00 | 46.2 | 0.00 | 6.00 | 1.7 | - | 1.0 | 4.02 | 0.00 | send(P, 'withX:', [3]), same path as 1a + maps:put |
| 2b | construct: VbPoint x: a y: 4 / %Point{x: a, y: 4} | 111 | 3.03 | 6.00 | 84.1 | 0.04 | 6.00 | 1.3 | - | 1.0 | 2.99 | 0.00 | whereis_class + class_send: gen_server:call into the VbPoint class process |
| 2c | construct via new: VbPoint new: #{#x => a, #y => 4} / struct!(Point, %{x: a, y: 4}) | 997 | 37.1 | 130 | 143 | 26.0 | 36.0 | 7.0 | 1.4 | 3.6 | 11.1 | 94.0 | class_send('new:') -> direct constructor: VbPoint:new/1 in the caller -> check_init_args/5, declared fields from one {declared_fields, C} derived-cache hit (BT-4281) |
| 3a | `class sealed` method, sealed class / Mod.fun(arg) | 19.6 | 3.00 | 0.00 | 2.83 | 2.00 | 0.00 | 6.9 | 1.5 | - | 1.00 | 0.00 | guarded direct call: one persistent_term:get of the {Mod, Fun, Tag} static call key, whose value is ClassRef, then Mod:'class_double:'(ClassRef, 21) (BT-4316) |
| 3b | class method, sealed class / Mod.fun(arg) | 19.8 | 3.00 | 0.00 | 2.53 | 2.00 | 0.00 | 7.8 | 1.5 | - | 1.00 | 0.00 | guarded direct call, as 3a: `class sealed` plays no part (ADR 0142, BT-4301) |
| 3c | class method, open class / Mod.fun(arg) | 19.6 | 3.00 | 0.00 | 2.82 | 2.00 | 0.00 | 7.0 | 1.5 | - | 1.00 | 0.00 | guarded direct call, as 3a: open class (ADR 0142, BT-4301) |
| 3d | class method, class in a variable / mod.fun(arg) | 393 | 21.1 | 42.0 | 17.0 | 2.00 | 0.00 | 23.2 | 10.5 | - | 19.1 | 42.0 | send(C, 'double:', [21]) -> class_send: runs in the caller (ADR 0142, BT-4300) |
| 3e | inherited class method, open stateless subclass / Mod.fun(arg) | 676 | 28.1 | 36.0 | 3.15 | 2.00 | 0.00 | 214.6 | 14.0 | - | 26.1 | 36.0 | whereis_class + class_send: inherited method resolved by the owner memo, runs in the caller (BT-4300) |
| 3f | class method, class with classState: / Mod.fun(arg) | 8164 | 201 | 574 | 3.22 | 2.00 | 0.00 | 2532.3 | 100.3 | - | 199 | 574 | whereis_class + class_send: gen_server:call; stays there under ADR 0142 (class state) |
| 4a | untyped receiver: o x / o.x | 433 | 25.0 | 28.0 | 2.76 | 0.00 | 0.00 | 156.8 | - | - | 25.0 | 28.0 | send(O, x, []) -> classify, tagged-map class lookup (ETS + cache), Mod:dispatch/3 (untyped receiver stays dynamic) |
| 4b | untyped receiver: o area / protocol | 474 | 25.1 | 28.0 | 19.6 | 7.00 | 0.00 | 24.1 | 3.6 | - | 18.1 | 28.0 | send(O, area, []), same path as 4a |
| 4c | typed open Value: s area / Square.area(s) | 14.5 | 3.00 | 0.00 | 3.80 | 2.00 | 0.00 | 3.8 | 1.5 | - | 1.00 | 0.00 | send(S, area, []), same path as 4a (open class stays dynamic) |
| 5a | inherited method: s base / defdelegate | 207 | 7.02 | 3.00 | 5.23 | 3.00 | 0.00 | 39.6 | 2.3 | - | 4.02 | 3.00 | send(S, base, []): VbSub:dispatch/3 misses, extensions lookup, VbBase:dispatch/3 |
| 5b | self-send to inherited: s viaSelf / local call | 184 | 9.02 | 3.00 | 5.41 | 4.00 | 0.00 | 34.1 | 2.3 | - | 5.01 | 3.00 | send(S, viaSelf, []), then `self base` is a second full send |
| 5c | runtime-resolved method: s hasField: #v / Map.has_key? | 476 | 25.1 | 28.0 | 5.98 | 0.01 | 0.00 | 79.5 | - | - | 25.0 | 28.0 | send(S, 'hasField:', [v]) -> beamtalk_reflection:has_field/2 |
| 6a | block value: x / fun.(x) | 8.85 | 2.00 | 0.00 | 2.65 | 2.00 | 0.00 | 3.3 | 1.0 | - | 0.00 | 0.00 | is_function(Blk, 1) guard, then apply Blk(41) |
| 6b | do: summing 100 elements / Enum.reduce | 3548 | 302 | 404 | 306 | 303 | 0.00 | 11.6 | 1.0 | - | -0.70 | 404 | lists:foldl over a fun that maps:get/maps:put `sum` in a state map; + with coercion try |
| 6c | do: over 100, no assignment / Enum.each | 867 | 303 | 0.00 | 853 | 303 | 0.00 | 1.0 | 1.0 | - | 0.00 | 0.00 | is_list guard, lists:foreach(fun) |
| 6d | collect: 100 elements / Enum.map | 2274 | 403 | 200 | 2677 | 404 | 200 | 0.8 | 1.0 | 1.0 | -0.98 | 0.00 | is_list guard, lists:map(fun); * with is_number guard |
| 6e | inject:into: 100 elements / Enum.reduce | 689 | 302 | 0.00 | 307 | 303 | 0.00 | 2.2 | 1.0 | - | -1.00 | 0.00 | is_list guard, lists:foldl(fun); + with coercion try |
| 7a | actor sync read: c count / GenServer.call | 2536 | 89.2 | 146 | 1680 | 37.1 | 56.0 | 1.5 | 2.4 | 2.6 | 52.1 | 90.0 | send(C, count, []) -> beamtalk_actor:sync_send, gen_server:call + telemetry/trace/ctx hooks both sides |
| 7b | actor state update: c increment / GenServer.call | 2954 | 92.4 | 152 | 1560 | 37.1 | 56.0 | 1.9 | 2.5 | 2.7 | 55.3 | 96.0 | as 7a |
| 7c | actor cast: c increment! / GenServer.cast | 1178 | 68.5 | 73.0 | 411 | 18.5 | 12.0 | 2.9 | 3.7 | 6.1 | 50.0 | 61.0 | beamtalk_message_dispatch:cast -> gen_server:cast + the same hooks |
| 7d | actor method doing self hasField: / GenServer.call | 3134 | 142 | 223 | 1633 | 37.1 | 56.0 | 1.9 | 3.8 | 4.0 | 105 | 167 | as 7a; in the actor `self hasField:` misses the module and walks the runtime method resolver |
| 7e | inherited actor method doing self increment / delegated GenServer.call | 3145 | 125 | 203 | 1700 | 42.1 | 56.0 | 1.8 | 3.0 | 3.6 | 83.2 | 147 | as 7a; the subclass's dispatch/4 default arm takes the memoised inherited_target/2 verdict to the owner (BT-4280) |
| 7f | actor method doing self increment / GenServer.call | 3049 | 97.5 | 159 | 2142 | 41.2 | 56.0 | 1.4 | 2.4 | 2.8 | 56.3 | 103 | as 7a; the self-send is a static call inside the class module |
| 7g | actor method reading two getters of a typed sealed Value / GenServer.call | 2443 | 97.3 | 162 | 1441 | 37.1 | 56.0 | 1.7 | 2.6 | 2.9 | 60.2 | 106 | as 7a; each `p x` is a tag + __is_slot/1-guarded maps:get with no cell bracket, the `+` a guarded BIF with none either (BT-4279) |
| 8a | on: Error do: without a raise / try-rescue | 57.8 | 5.00 | 0.00 | 2.56 | 0.00 | 0.00 | 22.6 | - | - | 5.00 | 0.00 | `Error` class ref built per entry (whereis_class + module_name gen_server:call), class_vars snapshot, try |
| 8b | on: Error do: with a raise / raise-rescue | 1711 | 89.3 | 141 | 425 | 13.2 | 36.0 | 4.0 | 6.8 | 3.9 | 76.1 | 105 | as 8a + `Error signal:` raised in the caller behind its caller-safe raise key (one persistent_term read, BT-4270), wrapped and class-matched; frames built only on read |
| 9a | (Result ok: x) value / {:ok, x} match | 1054 | 34.2 | 39.0 | 50.8 | 2.02 | 3.00 | 20.7 | 17.0 | 13.0 | 32.2 | 36.0 | class_send(Result, 'ok:') gen_server:call; `value` as a block-or-send case |
| 9b | r isOk / match?({:ok, _}, r) | 9.82 | 3.00 | 0.00 | 0.33 | 0.00 | 0.00 | - | - | - | 3.00 | 0.00 | tag-guarded direct Result:dispatch/3 (BT-4069); mismatch falls back to send/3 |
| 10a | Dictionary at:put: / Map.put | 122 | 4.03 | 10.0 | 35.7 | 1.01 | 6.00 | 3.4 | 4.0 | 1.7 | 3.02 | 4.00 | send(D, 'at:put:', ...) -> Dictionary:dispatch/3 -> maps:put |
| 10b | Dictionary at: / Map.get | 15.1 | 3.00 | 0.00 | 5.06 | 3.00 | 0.00 | 3.0 | 1.0 | - | -0.00 | 0.00 | send(D, 'at:', ...) -> Dictionary:dispatch/3 -> maps:get |
| 10c | List add: (100 elements) / l ++ [x] | 864 | 6.86 | 202 | 677 | 4.52 | 200 | 1.3 | 1.5 | 1.0 | 2.33 | 2.00 | send(L, 'add:', [7]) -> List primitive (++) |
| 10d | List includes: (100 elements) / Enum.member? | 102 | 9.00 | 0.00 | 86.9 | 8.00 | 0.00 | 1.2 | 1.1 | - | 1.00 | 0.00 | send(L, 'includes:', [50]) -> List primitive (lists:member) |
| 10e | String ++ / <> | 249 | 9.06 | 11.0 | 208 | 0.04 | 8.00 | 1.2 | - | 1.4 | 9.02 | 3.00 | is_list guard, iolist_to_binary([binary_to_list(S), binary_to_list("!")]) |
| 11a | 1 to: 100 do: summing / recursive sum | 285 | 102 | 7.00 | 111 | 102 | 0.00 | 2.6 | 1.0 | - | 0.03 | 7.00 | letrec loop; + with is_number guard and coercion try |
| 11b | a + 1, a :: Integer / a + 1 | 0.61 | 0.00 | 0.00 | 0.20 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | erlang:'+'(A, 1) |
| 11c | a + 1, a untyped / a + 1 | 6.39 | 0.00 | 0.00 | 0.30 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | is_number guard, erlang:'+' |
| 11d | a > 0 ifTrue: [1] ifFalse: [2] / if | 0.53 | 0.00 | 0.00 | 0.35 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | send(Bool, 'ifTrue:ifFalse:', [fun, fun]) -> True:dispatch/3 |
| 12a | p class / p.__struct__ | 155 | 1.04 | 5.00 | 3.65 | 0.00 | 0.00 | 42.3 | - | - | 1.04 | 5.00 | typed receiver (BT-4127): is_map + tag guard, erlang:whereis + class-object tuple with the pinned module; untyped: beamtalk_primitive:class_of_object derived-cache hit |
| 12b | p isKindOf: VbPoint / is_struct(p, Point) | 1213 | 39.1 | 74.0 | 1.07 | 0.00 | 0.00 | 1129.5 | - | - | 39.1 | 74.0 | class ref built per call (whereis_class + module_name_safe) + send(P, 'isKindOf:') -> direct objectIsKindOf/2 call (no FFI proxy, BT-4410): one memoised {is_kind_of, C, B} lookup (BT-4221) |
| 12c | s isKindOf: String / is_binary(s) | 694 | 27.1 | 60.0 | 2.71 | 0.00 | 0.00 | 255.5 | - | - | 27.1 | 60.0 | as 12b, but the String literal bakes its module (whereis only, no module_name_safe, BT-4411) and the send walks String/Binary/Collection/Value/Object dispatch (an extensions lookup per level) to Object>>isKindOf: |
| 13a | FFI call, integer result / apply | 95.4 | 14.0 | 4.00 | 8.40 | 1.00 | 0.00 | 11.4 | 14.0 | - | 13.0 | 4.00 | direct_call/4 ('erlang', 'phash2', [A], 'stock'): mode precomputed (BT-4153), integer fast path |
| 13b | FFI call, binary result, caller-typed / apply | 144 | 14.0 | 7.00 | 21.6 | 1.00 | 3.00 | 6.7 | 14.0 | 2.3 | 13.0 | 4.00 | direct_call/4 ('erlang', 'integer_to_binary', [A], 'stock'): binary fast path |
| 13c | FFI call, {ok, V} result / apply | 429 | 24.1 | 21.0 | 21.2 | 1.00 | 3.00 | 20.3 | 24.0 | 7.0 | 23.1 | 18.0 | direct_call/4 ('maps', 'find', [K, D], 'stock'): mode precomputed, {ok, V} still wraps to Result |
| 13d | FFI call, ok-atom result / apply | 141 | 13.0 | 7.00 | 2.59 | 2.00 | 0.00 | 54.6 | 6.5 | - | 11.0 | 7.00 | direct_call/4 ('vs_bench', 'ffi_ok', [], 'stock'): mode precomputed, ok still wraps to Result |
| 14a | typed Tuple at: / elem(t, 0) | 14.2 | 3.00 | 0.00 | 0.17 | 0.00 | 0.00 | - | - | - | 3.00 | 0.00 | is_tuple-guarded direct Tuple:dispatch/3 (BT-4294); 'at:' reads an in-range element inline (BT-4400) |
| 14b | class reference: last := VbPoint / Point | 535 | 11.1 | 16.0 | -0.12 | 0.00 | 0.00 | - | - | - | 11.1 | 16.0 | class reference per use: whereis of the compile-time registered name + module_name_safe (BT-4295) |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 0b | `baseline` | -0.15 | 0.00 | 0.00 |
| 1a | `getter_dot` | 2.61 | 0.00 | 0.00 |
| 1a | `getter_fun` | 3.60 | 2.00 | 0.00 |
| 1a | `getter_match` | 1.34 | 0.00 | 0.00 |
| 1b | `getter_dot` | 3.07 | 0.00 | 0.00 |
| 1c | `point_sum` | 3.50 | 2.00 | 0.00 |
| 2a | `with_x` | 46.2 | 0.00 | 6.00 |
| 2b | `construct` | 84.1 | 0.04 | 6.00 |
| 2c | `construct_map` | 143 | 26.0 | 36.0 |
| 2c | `construct` | 84.5 | 0.04 | 6.00 |
| 3a | `remote_call` | 2.83 | 2.00 | 0.00 |
| 3b | `remote_call` | 2.53 | 2.00 | 0.00 |
| 3c | `remote_call` | 2.82 | 2.00 | 0.00 |
| 3d | `dyn_remote_call` | 17.0 | 2.00 | 0.00 |
| 3d | `remote_call` | 3.04 | 2.00 | 0.00 |
| 3e | `remote_call` | 3.15 | 2.00 | 0.00 |
| 3f | `remote_call` | 3.22 | 2.00 | 0.00 |
| 4a | `getter_dot` | 2.76 | 0.00 | 0.00 |
| 4b | `protocol` | 19.6 | 7.00 | 0.00 |
| 4b | `apply3` | 16.9 | 2.00 | 0.00 |
| 4b | `dyn_module` | 17.1 | 2.00 | 0.00 |
| 4c | `square_area` | 3.80 | 2.00 | 0.00 |
| 4c | `protocol` | 19.2 | 7.00 | 0.00 |
| 5a | `delegate` | 5.23 | 3.00 | 0.00 |
| 5b | `delegate_via_self` | 5.41 | 4.00 | 0.00 |
| 5c | `has_key` | 5.98 | 0.01 | 0.00 |
| 6a | `fun_call` | 2.65 | 2.00 | 0.00 |
| 6b | `enum_reduce_sum` | 306 | 303 | 0.00 |
| 6b | `lists_foldl` | 314 | 302 | 0.00 |
| 6c | `enum_each` | 853 | 303 | 0.00 |
| 6d | `enum_map` | 2677 | 404 | 200 |
| 6e | `enum_reduce_sum` | 307 | 303 | 0.00 |
| 6e | `lists_foldl` | 308 | 302 | 0.00 |
| 7a | `gen_call` | 1680 | 37.1 | 56.0 |
| 7b | `gen_update` | 1560 | 37.1 | 56.0 |
| 7c | `gen_cast` | 411 | 18.5 | 12.0 |
| 7d | `gen_probe` | 1633 | 37.1 | 56.0 |
| 7e | `gen_via_self` | 1700 | 42.1 | 56.0 |
| 7f | `gen_via_self` | 2142 | 41.2 | 56.0 |
| 7g | `gen_sum_of` | 1441 | 37.1 | 56.0 |
| 8a | `try_no_raise` | 2.56 | 0.00 | 0.00 |
| 8b | `try_raise` | 425 | 13.2 | 36.0 |
| 9a | `ok_match` | 50.8 | 2.02 | 3.00 |
| 9b | `ok_is_ok` | 0.33 | 0.00 | 0.00 |
| 10a | `map_put` | 35.7 | 1.01 | 6.00 |
| 10b | `map_get` | 5.06 | 3.00 | 0.00 |
| 10c | `list_append` | 677 | 4.52 | 200 |
| 10d | `list_member` | 86.9 | 8.00 | 0.00 |
| 10e | `binary_concat` | 208 | 0.04 | 8.00 |
| 11a | `sum_recursive` | 111 | 102 | 0.00 |
| 11a | `sum_reduce_range` | 618 | 252 | 0.00 |
| 11b | `add` | 0.20 | 0.00 | 0.00 |
| 11c | `add` | 0.30 | 0.00 | 0.00 |
| 11d | `if_else` | 0.35 | 0.00 | 0.00 |
| 12a | `struct_of` | 3.65 | 0.00 | 0.00 |
| 12b | `is_struct_check` | 1.07 | 0.00 | 0.00 |
| 12c | `is_binary_check` | 2.71 | 0.00 | 0.00 |
| 13a | `apply_phash2` | 8.40 | 1.00 | 0.00 |
| 13a | `direct_phash2` | 9.13 | 1.00 | 0.00 |
| 13b | `apply_integer_to_binary` | 21.6 | 1.00 | 3.00 |
| 13b | `direct_integer_to_binary` | 23.7 | 1.00 | 3.00 |
| 13c | `apply_maps_find` | 21.2 | 1.00 | 3.00 |
| 13c | `direct_maps_find` | 18.8 | 1.00 | 3.00 |
| 13d | `apply_ffi_ok` | 2.59 | 2.00 | 0.00 |
| 14a | `tuple_elem` | 0.17 | 0.00 | 0.00 |
| 14b | `module_ref` | -0.12 | 0.00 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 0.99 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.17 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0b | bt | loopNoAssign: | 43.6 | 4.01 | 2.00 | 4.01 | 2.00 |
| 0b | ex | baseline | 1.02 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1a | bt | getterSealed:p: | 14.3 | 4.00 | 0.00 | 4.00 | 0.00 |
| 1a | ex | getter_dot | 3.78 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1a | ex | getter_fun | 4.77 | 3.00 | 0.00 | 3.00 | 0.00 |
| 1a | ex | getter_match | 2.50 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1b | bt | getterOpen:p: | 14.2 | 4.00 | 0.00 | 4.00 | 0.00 |
| 1b | ex | getter_dot | 4.24 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1c | bt | sumSealed:p: | 18.2 | 4.00 | 0.00 | 4.00 | 0.00 |
| 1c | ex | point_sum | 4.67 | 3.00 | 0.00 | 3.00 | 0.00 |
| 2a | bt | withX:p: | 77.6 | 5.02 | 6.00 | 5.02 | 6.00 |
| 2a | ex | with_x | 47.4 | 1.00 | 6.00 | 1.00 | 6.00 |
| 2b | bt | construct:a: | 112 | 4.03 | 6.00 | 4.03 | 6.00 |
| 2b | ex | construct | 85.2 | 1.04 | 6.00 | 1.04 | 6.00 |
| 2c | bt | constructMap:a: | 998 | 38.1 | 130 | 38.1 | 130 |
| 2c | ex | construct | 85.7 | 1.04 | 6.00 | 1.04 | 6.00 |
| 2c | ex | construct_map | 144 | 27.0 | 36.0 | 27.0 | 36.0 |
| 3a | bt | classSealedMethod: | 20.5 | 4.00 | 0.00 | 4.00 | 0.00 |
| 3a | ex | remote_call | 4.00 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3b | bt | sealedClassMethod: | 20.8 | 4.00 | 0.00 | 4.00 | 0.00 |
| 3b | ex | remote_call | 3.70 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3c | bt | openClassMethod: | 20.6 | 4.00 | 0.00 | 4.00 | 0.00 |
| 3c | ex | remote_call | 3.99 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3d | bt | dynClassMethod:c: | 394 | 22.1 | 42.0 | 22.1 | 42.0 |
| 3d | ex | dyn_remote_call | 18.1 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3d | ex | remote_call | 4.21 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3e | bt | inheritedClassMethod: | 677 | 29.1 | 36.0 | 29.1 | 36.0 |
| 3e | ex | remote_call | 4.32 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3f | bt | statefulClassMethod: | 8165 | 202 | 574 | 91.3 | 276 |
| 3f | ex | remote_call | 4.39 | 3.00 | 0.00 | 3.00 | 0.00 |
| 4a | bt | dynGetter:o: | 434 | 26.0 | 28.0 | 26.0 | 28.0 |
| 4a | ex | getter_dot | 3.93 | 1.00 | 0.00 | 1.00 | 0.00 |
| 4b | bt | dynArea:o: | 475 | 26.1 | 28.0 | 26.1 | 28.0 |
| 4b | ex | apply3 | 18.0 | 3.00 | 0.00 | 3.00 | 0.00 |
| 4b | ex | dyn_module | 18.2 | 3.00 | 0.00 | 3.00 | 0.00 |
| 4b | ex | protocol | 20.8 | 8.00 | 0.00 | 8.00 | 0.00 |
| 4c | bt | typedArea:s: | 15.5 | 4.00 | 0.00 | 4.00 | 0.00 |
| 4c | ex | protocol | 20.3 | 8.00 | 0.00 | 8.00 | 0.00 |
| 4c | ex | square_area | 4.96 | 3.00 | 0.00 | 3.00 | 0.00 |
| 5a | bt | inherited:s: | 208 | 8.02 | 3.00 | 8.02 | 3.00 |
| 5a | ex | delegate | 6.39 | 4.00 | 0.00 | 4.00 | 0.00 |
| 5b | bt | inheritedViaSelf:s: | 185 | 10.0 | 3.00 | 10.0 | 3.00 |
| 5b | ex | delegate_via_self | 6.58 | 5.00 | 0.00 | 5.00 | 0.00 |
| 5c | bt | hasField:s: | 477 | 26.1 | 28.0 | 26.1 | 28.0 |
| 5c | ex | has_key | 7.15 | 1.01 | 0.00 | 1.00 | 0.00 |
| 6a | bt | blockValue:blk: | 9.85 | 3.00 | 0.00 | 3.00 | 0.00 |
| 6a | ex | fun_call | 3.81 | 3.00 | 0.00 | 3.00 | 0.00 |
| 6b | bt | doSum:list: | 3549 | 303 | 404 | 303 | 404 |
| 6b | ex | enum_reduce_sum | 307 | 304 | 0.00 | 304 | 0.00 |
| 6b | ex | lists_foldl | 316 | 303 | 0.00 | 303 | 0.00 |
| 6c | bt | doNoAssign:list: | 868 | 304 | 0.00 | 304 | 0.00 |
| 6c | ex | enum_each | 854 | 304 | 0.00 | 304 | 0.00 |
| 6d | bt | collect:list: | 2275 | 404 | 200 | 404 | 200 |
| 6d | ex | enum_map | 2678 | 405 | 200 | 405 | 200 |
| 6e | bt | inject:list: | 690 | 303 | 0.00 | 303 | 0.00 |
| 6e | ex | enum_reduce_sum | 308 | 304 | 0.00 | 304 | 0.00 |
| 6e | ex | lists_foldl | 309 | 303 | 0.00 | 303 | 0.00 |
| 7a | bt | actorCall:c: | 2537 | 90.2 | 146 | 44.1 | 83.0 |
| 7a | ex | gen_call | 1681 | 38.1 | 56.0 | 17.1 | 33.0 |
| 7b | bt | actorUpdate:c: | 2955 | 93.4 | 152 | 44.1 | 83.0 |
| 7b | ex | gen_update | 1561 | 38.1 | 56.0 | 17.1 | 33.0 |
| 7c | bt | actorCast:c: | 1179 | 69.5 | 73.0 | 34.0 | 38.0 |
| 7c | ex | gen_cast | 412 | 19.5 | 12.0 | 7.02 | 3.00 |
| 7d | bt | actorProbe:c: | 3135 | 143 | 223 | 44.1 | 83.0 |
| 7d | ex | gen_probe | 1634 | 38.1 | 56.0 | 17.1 | 33.0 |
| 7e | bt | actorViaSelfSub:c: | 3146 | 126 | 203 | 44.1 | 83.0 |
| 7e | ex | gen_via_self | 1701 | 43.1 | 56.0 | 17.1 | 33.0 |
| 7f | bt | actorViaSelf:c: | 3050 | 98.5 | 159 | 44.1 | 83.0 |
| 7f | ex | gen_via_self | 2143 | 42.2 | 56.0 | 17.1 | 33.0 |
| 7g | bt | actorSumOf:c: | 2444 | 98.3 | 162 | 44.2 | 85.1 |
| 7g | ex | gen_sum_of | 1442 | 38.1 | 56.0 | 17.1 | 33.0 |
| 8a | bt | onDoNoRaise: | 58.8 | 6.00 | 0.00 | 6.00 | 0.00 |
| 8a | ex | try_no_raise | 3.72 | 1.00 | 0.00 | 1.00 | 0.00 |
| 8b | bt | onDoRaise: | 1712 | 90.3 | 141 | 90.3 | 141 |
| 8b | ex | try_raise | 426 | 14.2 | 36.0 | 14.2 | 36.0 |
| 9a | bt | resultOk: | 1055 | 35.2 | 39.0 | 35.2 | 39.0 |
| 9a | ex | ok_match | 52.0 | 3.02 | 3.00 | 3.02 | 3.00 |
| 9b | bt | resultIsOk:r: | 10.8 | 4.00 | 0.00 | 4.00 | 0.00 |
| 9b | ex | ok_is_ok | 1.49 | 1.00 | 0.00 | 1.00 | 0.00 |
| 10a | bt | dictPut:d: | 123 | 5.03 | 10.0 | 5.03 | 10.0 |
| 10a | ex | map_put | 36.9 | 2.01 | 6.00 | 2.01 | 6.00 |
| 10b | bt | dictAt:d: | 16.1 | 4.00 | 0.00 | 4.00 | 0.00 |
| 10b | ex | map_get | 6.23 | 4.00 | 0.00 | 4.00 | 0.00 |
| 10c | bt | listAdd:list: | 865 | 7.86 | 202 | 7.86 | 202 |
| 10c | ex | list_append | 678 | 5.52 | 200 | 5.52 | 200 |
| 10d | bt | listIncludes:list: | 103 | 10.0 | 0.00 | 10.0 | 0.00 |
| 10d | ex | list_member | 88.0 | 9.00 | 0.00 | 9.00 | 0.00 |
| 10e | bt | stringConcat:s: | 250 | 10.1 | 11.0 | 10.1 | 11.0 |
| 10e | ex | binary_concat | 209 | 1.04 | 8.00 | 1.04 | 8.00 |
| 11a | bt | sumToDo: | 286 | 103 | 7.00 | 103 | 7.00 |
| 11a | ex | sum_recursive | 112 | 103 | 0.00 | 103 | 0.00 |
| 11a | ex | sum_reduce_range | 619 | 253 | 0.00 | 253 | 0.00 |
| 11b | bt | add:a: | 1.61 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11b | ex | add | 1.36 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11c | bt | addUntyped:a: | 7.39 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11c | ex | add | 1.46 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11d | bt | ifTrueIfFalse:a: | 1.52 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11d | ex | if_else | 1.52 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12a | bt | classOf:p: | 156 | 2.04 | 5.00 | 2.04 | 5.00 |
| 12a | ex | struct_of | 4.82 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12b | bt | isKindOf:p: | 1214 | 40.1 | 74.0 | 40.1 | 74.0 |
| 12b | ex | is_struct_check | 2.24 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12c | bt | isKindOfBuiltin:s: | 695 | 28.1 | 60.0 | 28.1 | 60.0 |
| 12c | ex | is_binary_check | 3.88 | 1.00 | 0.00 | 1.00 | 0.00 |
| 13a | bt | ffiInt:a: | 96.4 | 15.0 | 4.00 | 15.0 | 4.00 |
| 13a | ex | apply_phash2 | 9.57 | 2.00 | 0.00 | 2.00 | 0.00 |
| 13a | ex | direct_phash2 | 10.3 | 2.00 | 0.00 | 2.00 | 0.00 |
| 13b | bt | ffiBinary:a: | 145 | 15.0 | 7.00 | 15.0 | 7.00 |
| 13b | ex | apply_integer_to_binary | 22.7 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13b | ex | direct_integer_to_binary | 24.9 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13c | bt | ffiOkTuple:d: | 430 | 25.1 | 21.0 | 25.1 | 21.0 |
| 13c | ex | apply_maps_find | 22.3 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13c | ex | direct_maps_find | 20.0 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13d | bt | ffiOk: | 142 | 14.0 | 7.00 | 14.0 | 7.00 |
| 13d | ex | apply_ffi_ok | 3.75 | 3.00 | 0.00 | 3.00 | 0.00 |
| 14a | bt | tupleAt:t: | 15.2 | 4.00 | 0.00 | 4.00 | 0.00 |
| 14a | ex | tuple_elem | 1.34 | 1.00 | 0.00 | 1.00 | 0.00 |
| 14b | bt | classReference: | 536 | 12.1 | 16.0 | 12.1 | 16.0 |
| 14b | ex | module_ref | 1.05 | 1.00 | 0.00 | 1.00 | 0.00 |
