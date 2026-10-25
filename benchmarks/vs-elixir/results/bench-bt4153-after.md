# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 18
# load average: 17:27  up 28 days,  3:17, 3 users, load averages: 26.39 65.09 64.61
# rounds 7
# load average after: 17:28  up 28 days,  3:18, 3 users, load averages: 18.05 59.35 62.55

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 0b | timesRepeat: whose block assigns nothing | 101 | 26.1 | 10.0 | -0.01 | 0.00 | 0.00 | - | - | - | 26.1 | 10.0 | send(N, 'timesRepeat:', [fun]); Integer>>timesRepeat: dispatches ifTrue:ifFalse: per step |
| 1a | getter on a sealed Value: p x / p.x | 3.65 | 3.00 | 0.00 | 1.61 | 0.00 | 0.00 | 2.3 | - | - | 3.00 | 0.00 | tag-guarded direct VbPoint:dispatch/3 (BT-4069); mismatch falls back to send/3 |
| 1b | getter on an open Value: p x / p.x | 166 | 29.1 | 28.0 | 1.63 | 0.00 | 0.00 | 102.1 | - | - | 29.1 | 28.0 | send(P, x, []) -> classify, tagged-map class lookup (ETS + cache), Mod:dispatch/3 (open receiver stays dynamic) |
| 1c | method on a sealed Value: p sum / Point.sum(p) | 5.11 | 3.00 | 0.00 | 1.75 | 2.00 | 0.00 | 2.9 | 1.5 | - | 1.00 | 0.00 | send(P, sum, []), same path as 1a |
| 2a | functional update: p withX: 3 / %{p \| x: 3} | 19.6 | 4.01 | 6.00 | 32.2 | 0.00 | 6.00 | 0.6 | - | 1.0 | 4.01 | 0.00 | send(P, 'withX:', [3]), same path as 1a + maps:put |
| 2b | construct: VbPoint x: a y: 4 / %Point{x: a, y: 4} | 39.2 | 2.05 | 6.00 | 31.9 | 0.06 | 6.00 | 1.2 | - | 1.0 | 1.99 | 0.00 | whereis_class + class_send: gen_server:call into the VbPoint class process |
| 3a | `class sealed` method, sealed class / Mod.fun(arg) | 1.53 | 2.00 | 0.00 | 1.48 | 2.00 | 0.00 | 1.0 | 1.0 | - | 0.00 | 0.00 | direct call Mod:'class_double:'(nil, 21) |
| 3b | class method, sealed class / Mod.fun(arg) | 2015 | 144 | 340 | 1.50 | 2.00 | 0.00 | 1343.3 | 71.8 | - | 142 | 340 | whereis_class + class_send: not `class sealed`, so gen_server:call into the class process |
| 3c | class method, open class / Mod.fun(arg) | 2176 | 144 | 373 | 1.49 | 2.00 | 0.00 | 1461.4 | 72.1 | - | 142 | 373 | whereis_class + class_send: gen_server:call into the class process |
| 4a | untyped receiver: o x / o.x | 166 | 29.1 | 28.0 | 1.54 | 0.00 | 0.00 | 107.8 | - | - | 29.1 | 28.0 | send(O, x, []) -> classify, tagged-map class lookup (ETS + cache), Mod:dispatch/3 (untyped receiver stays dynamic) |
| 4b | untyped receiver: o area / protocol | 164 | 29.1 | 28.0 | 9.85 | 7.00 | 0.00 | 16.7 | 4.2 | - | 22.1 | 28.0 | send(O, area, []), same path as 4a |
| 4c | typed open Value: s area / Square.area(s) | 165 | 29.1 | 28.0 | 1.87 | 2.00 | 0.00 | 88.5 | 14.5 | - | 27.1 | 28.0 | send(S, area, []), same path as 4a (open class stays dynamic) |
| 5a | inherited method: s base / defdelegate | 199 | 33.1 | 31.0 | 1.95 | 3.00 | 0.00 | 101.7 | 11.0 | - | 30.1 | 31.0 | send(S, base, []): VbSub:dispatch/3 misses, extensions lookup, VbBase:dispatch/3 |
| 5b | self-send to inherited: s viaSelf / local call | 459 | 61.3 | 59.0 | 2.27 | 4.00 | 0.00 | 201.8 | 15.3 | - | 57.3 | 59.0 | send(S, viaSelf, []), then `self base` is a second full send |
| 5c | runtime-resolved method: s hasField: #v / Map.has_key? | 168 | 29.1 | 28.0 | 2.31 | 0.00 | 0.00 | 72.7 | - | - | 29.1 | 28.0 | send(S, 'hasField:', [v]) -> beamtalk_reflection:has_field/2 |
| 6a | block value: x / fun.(x) | 1.75 | 2.00 | 0.00 | 1.43 | 2.00 | 0.00 | 1.2 | 1.0 | - | 0.00 | 0.00 | is_function(Blk, 1) guard, then apply Blk(41) |
| 6b | do: summing 100 elements / Enum.reduce | 2444 | 304 | 404 | 220 | 303 | 0.00 | 11.1 | 1.0 | - | 0.66 | 404 | lists:foldl over a fun that maps:get/maps:put `sum` in a state map; + with coercion try |
| 6c | do: over 100, no assignment / Enum.each | 320 | 303 | 0.00 | 300 | 303 | 0.00 | 1.1 | 1.0 | - | 0.00 | 0.00 | is_list guard, lists:foreach(fun) |
| 6d | collect: 100 elements / Enum.map | 1534 | 403 | 200 | 1695 | 404 | 200 | 0.9 | 1.0 | 1.0 | -1.00 | 0.00 | is_list guard, lists:map(fun); * with is_number guard |
| 6e | inject:into: 100 elements / Enum.reduce | 214 | 302 | 0.00 | 233 | 303 | 0.00 | 0.9 | 1.0 | - | -1.00 | 0.00 | is_list guard, lists:foldl(fun); + with coercion try |
| 7a | actor sync read: c count / GenServer.call | 3498 | 222 | 308 | 760 | 37.1 | 56.0 | 4.6 | 6.0 | 5.5 | 185 | 252 | send(C, count, []) -> beamtalk_actor:sync_send, gen_server:call + telemetry/trace/ctx hooks both sides |
| 7b | actor state update: c increment / GenServer.call | 3181 | 222 | 314 | 927 | 37.2 | 56.0 | 3.4 | 6.0 | 5.6 | 185 | 258 | as 7a |
| 7c | actor cast: c increment! / GenServer.cast | 2660 | 203 | 242 | 133 | 18.5 | 12.0 | 19.9 | 11.0 | 20.2 | 184 | 230 | beamtalk_message_dispatch:cast -> gen_server:cast + the same hooks |
| 7d | actor method doing self hasField: / GenServer.call | 3323 | 278 | 385 | 899 | 37.2 | 56.0 | 3.7 | 7.5 | 6.9 | 241 | 329 | as 7a; in the actor `self hasField:` misses the module and walks the runtime method resolver |
| 8a | on: Error do: without a raise / try-rescue | 222 | 21.1 | 21.0 | 1.49 | 0.00 | 0.00 | 148.9 | - | - | 21.1 | 21.0 | `Error` class ref built per entry (whereis_class + module_name gen_server:call), class_vars snapshot, try |
| 8b | on: Error do: with a raise / raise-rescue | 66598 | 2763 | 22701 | 172 | 13.1 | 36.0 | 387.9 | 210.9 | 630.6 | 2750 | 22665 | as 8a + `Error signal:` via the Error class process, which logs an error, rewraps, rebuilds a stack |
| 9a | (Result ok: x) value / {:ok, x} match | 287 | 50.1 | 46.0 | 20.5 | 2.01 | 3.00 | 14.0 | 24.9 | 15.3 | 48.1 | 43.0 | class_send(Result, 'ok:') gen_server:call; `value` as a block-or-send case |
| 9b | r isOk / match?({:ok, _}, r) | 4.65 | 3.00 | 0.00 | 0.16 | 0.00 | 0.00 | - | - | - | 3.00 | 0.00 | tag-guarded direct Result:dispatch/3 (BT-4069); mismatch falls back to send/3 |
| 10a | Dictionary at:put: / Map.put | 90.9 | 18.0 | 10.0 | 38.6 | 1.03 | 6.00 | 2.4 | 17.6 | 1.7 | 17.0 | 4.00 | send(D, 'at:put:', ...) -> Dictionary:dispatch/3 -> maps:put |
| 10b | Dictionary at: / Map.get | 20.8 | 17.0 | 0.00 | 2.21 | 3.00 | 0.00 | 9.4 | 5.7 | - | 14.0 | 0.00 | send(D, 'at:', ...) -> Dictionary:dispatch/3 -> maps:get |
| 10c | List add: (100 elements) / l ++ [x] | 328 | 17.7 | 202 | 325 | 4.70 | 200 | 1.0 | 3.8 | 1.0 | 13.0 | 2.00 | send(L, 'add:', [7]) -> List primitive (++) |
| 10d | List includes: (100 elements) / Enum.member? | 36.5 | 19.0 | 0.00 | 26.2 | 8.00 | 0.00 | 1.4 | 2.4 | - | 11.0 | 0.00 | send(L, 'includes:', [50]) -> List primitive (lists:member) |
| 10e | String ++ / <> | 81.0 | 9.06 | 28.0 | 84.4 | 0.04 | 8.00 | 1.0 | - | 3.5 | 9.03 | 20.0 | is_list guard, iolist_to_binary([binary_to_list(S), binary_to_list("!")]) |
| 11a | 1 to: 100 do: summing / recursive sum | 151 | 102 | 7.00 | 57.9 | 102 | 0.00 | 2.6 | 1.0 | - | 0.03 | 7.00 | letrec loop; + with is_number guard and coercion try |
| 11b | a + 1, a :: Integer / a + 1 | 0.11 | 0.00 | 0.00 | -0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | erlang:'+'(A, 1) |
| 11c | a + 1, a untyped / a + 1 | 1.38 | 0.00 | 0.00 | 0.21 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | is_number guard, erlang:'+' |
| 11d | a > 0 ifTrue: [1] ifFalse: [2] / if | 38.3 | 14.0 | 4.00 | 0.23 | 0.00 | 0.00 | - | - | - | 14.0 | 4.00 | send(Bool, 'ifTrue:ifFalse:', [fun, fun]) -> True:dispatch/3 |
| 12a | p class / p.__struct__ | 107 | 15.0 | 23.0 | 1.41 | 0.00 | 0.00 | 75.4 | - | - | 15.0 | 23.0 | beamtalk_primitive:class_of_object: derived-cache hit (counter + ETS read + pid liveness) |
| 12b | p isKindOf: VbPoint / is_struct(p, Point) | 943 | 74.3 | 108 | 1.00 | 0.00 | 0.00 | 943.0 | - | - | 74.3 | 108 | class ref built per call (gen_server:call) + send(P, 'isKindOf:') -> includesBehaviour: via class processes |
| 13a | FFI call, integer result / apply | 29.5 | 7.01 | 2.00 | 4.55 | 1.00 | 0.00 | 6.5 | 7.0 | - | 6.01 | 2.00 | direct_call/4 ('erlang', 'phash2', [A], 'stock'): mode precomputed (BT-4153), integer fast path |
| 13b | FFI call, binary result, caller-typed / apply | 46.2 | 7.02 | 5.00 | 9.84 | 1.00 | 3.00 | 4.7 | 7.0 | 1.7 | 6.02 | 2.00 | direct_call/4 ('erlang', 'integer_to_binary', [A], 'stock'): binary fast path |
| 13c | FFI call, {ok, V} result / apply | 126 | 13.1 | 17.0 | 7.01 | 1.00 | 3.00 | 18.0 | 13.1 | 5.7 | 12.1 | 14.0 | direct_call/4 ('maps', 'find', [K, D], 'stock'): mode precomputed, {ok, V} still wraps to Result |
| 13d | FFI call, ok-atom result / apply | 55.3 | 10.0 | 7.00 | 1.61 | 2.00 | 0.00 | 34.5 | 5.0 | - | 8.03 | 7.00 | direct_call/4 ('vs_bench', 'ffi_ok', [], 'stock'): mode precomputed, ok still wraps to Result |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 0b | `baseline` | -0.01 | 0.00 | 0.00 |
| 1a | `getter_dot` | 1.61 | 0.00 | 0.00 |
| 1a | `getter_fun` | 1.69 | 2.00 | 0.00 |
| 1a | `getter_match` | 0.95 | 0.00 | 0.00 |
| 1b | `getter_dot` | 1.63 | 0.00 | 0.00 |
| 1c | `point_sum` | 1.75 | 2.00 | 0.00 |
| 2a | `with_x` | 32.2 | 0.00 | 6.00 |
| 2b | `construct` | 31.9 | 0.06 | 6.00 |
| 3a | `remote_call` | 1.48 | 2.00 | 0.00 |
| 3b | `remote_call` | 1.50 | 2.00 | 0.00 |
| 3c | `remote_call` | 1.49 | 2.00 | 0.00 |
| 4a | `getter_dot` | 1.54 | 0.00 | 0.00 |
| 4b | `protocol` | 9.85 | 7.00 | 0.00 |
| 4b | `apply3` | 6.57 | 2.00 | 0.00 |
| 4b | `dyn_module` | 6.50 | 2.00 | 0.00 |
| 4c | `square_area` | 1.87 | 2.00 | 0.00 |
| 4c | `protocol` | 9.83 | 7.00 | 0.00 |
| 5a | `delegate` | 1.95 | 3.00 | 0.00 |
| 5b | `delegate_via_self` | 2.27 | 4.00 | 0.00 |
| 5c | `has_key` | 2.31 | 0.00 | 0.00 |
| 6a | `fun_call` | 1.43 | 2.00 | 0.00 |
| 6b | `enum_reduce_sum` | 220 | 303 | 0.00 |
| 6b | `lists_foldl` | 270 | 302 | 0.00 |
| 6c | `enum_each` | 300 | 303 | 0.00 |
| 6d | `enum_map` | 1695 | 404 | 200 |
| 6e | `enum_reduce_sum` | 233 | 303 | 0.00 |
| 6e | `lists_foldl` | 200 | 302 | 0.00 |
| 7a | `gen_call` | 760 | 37.1 | 56.0 |
| 7b | `gen_update` | 927 | 37.2 | 56.0 |
| 7c | `gen_cast` | 133 | 18.5 | 12.0 |
| 7d | `gen_probe` | 899 | 37.2 | 56.0 |
| 8a | `try_no_raise` | 1.49 | 0.00 | 0.00 |
| 8b | `try_raise` | 172 | 13.1 | 36.0 |
| 9a | `ok_match` | 20.5 | 2.01 | 3.00 |
| 9b | `ok_is_ok` | 0.16 | 0.00 | 0.00 |
| 10a | `map_put` | 38.6 | 1.03 | 6.00 |
| 10b | `map_get` | 2.21 | 3.00 | 0.00 |
| 10c | `list_append` | 325 | 4.70 | 200 |
| 10d | `list_member` | 26.2 | 8.00 | 0.00 |
| 10e | `binary_concat` | 84.4 | 0.04 | 8.00 |
| 11a | `sum_recursive` | 57.9 | 102 | 0.00 |
| 11a | `sum_reduce_range` | 193 | 252 | 0.00 |
| 11b | `add` | -0.00 | 0.00 | 0.00 |
| 11c | `add` | 0.21 | 0.00 | 0.00 |
| 11d | `if_else` | 0.23 | 0.00 | 0.00 |
| 12a | `struct_of` | 1.41 | 0.00 | 0.00 |
| 12b | `is_struct_check` | 1.00 | 0.00 | 0.00 |
| 13a | `apply_phash2` | 4.55 | 1.00 | 0.00 |
| 13a | `direct_phash2` | 4.46 | 1.00 | 0.00 |
| 13b | `apply_integer_to_binary` | 9.84 | 1.00 | 3.00 |
| 13b | `direct_integer_to_binary` | 10.1 | 1.00 | 3.00 |
| 13c | `apply_maps_find` | 7.01 | 1.00 | 3.00 |
| 13c | `direct_maps_find` | 6.77 | 1.00 | 3.00 |
| 13d | `apply_ffi_ok` | 1.61 | 2.00 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 0.66 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 0.74 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0b | bt | loopNoAssign: | 102 | 27.1 | 10.0 | 27.1 | 10.0 |
| 0b | ex | baseline | 0.73 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1a | bt | getterSealed:p: | 4.31 | 4.00 | 0.00 | 4.00 | 0.00 |
| 1a | ex | getter_dot | 2.35 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1a | ex | getter_fun | 2.43 | 3.00 | 0.00 | 3.00 | 0.00 |
| 1a | ex | getter_match | 1.69 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1b | bt | getterOpen:p: | 167 | 30.1 | 28.0 | 30.1 | 28.0 |
| 1b | ex | getter_dot | 2.37 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1c | bt | sumSealed:p: | 5.77 | 4.00 | 0.00 | 4.00 | 0.00 |
| 1c | ex | point_sum | 2.49 | 3.00 | 0.00 | 3.00 | 0.00 |
| 2a | bt | withX:p: | 20.2 | 5.01 | 6.00 | 5.01 | 6.00 |
| 2a | ex | with_x | 33.0 | 1.00 | 6.00 | 1.00 | 6.00 |
| 2b | bt | construct:a: | 39.9 | 3.05 | 6.00 | 3.05 | 6.00 |
| 2b | ex | construct | 32.7 | 1.06 | 6.00 | 1.06 | 6.00 |
| 3a | bt | classSealedMethod: | 2.19 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3a | ex | remote_call | 2.22 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3b | bt | sealedClassMethod: | 2016 | 145 | 340 | 43.3 | 100 |
| 3b | ex | remote_call | 2.24 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3c | bt | openClassMethod: | 2177 | 145 | 373 | 43.1 | 101 |
| 3c | ex | remote_call | 2.23 | 3.00 | 0.00 | 3.00 | 0.00 |
| 4a | bt | dynGetter:o: | 166 | 30.1 | 28.0 | 30.1 | 28.0 |
| 4a | ex | getter_dot | 2.28 | 1.00 | 0.00 | 1.00 | 0.00 |
| 4b | bt | dynArea:o: | 165 | 30.1 | 28.0 | 30.1 | 28.0 |
| 4b | ex | apply3 | 7.31 | 3.00 | 0.00 | 3.00 | 0.00 |
| 4b | ex | dyn_module | 7.24 | 3.00 | 0.00 | 3.00 | 0.00 |
| 4b | ex | protocol | 10.6 | 8.00 | 0.00 | 8.00 | 0.00 |
| 4c | bt | typedArea:s: | 166 | 30.1 | 28.0 | 30.1 | 28.0 |
| 4c | ex | protocol | 10.6 | 8.00 | 0.00 | 8.00 | 0.00 |
| 4c | ex | square_area | 2.60 | 3.00 | 0.00 | 3.00 | 0.00 |
| 5a | bt | inherited:s: | 199 | 34.1 | 31.0 | 34.1 | 31.0 |
| 5a | ex | delegate | 2.69 | 4.00 | 0.00 | 4.00 | 0.00 |
| 5b | bt | inheritedViaSelf:s: | 459 | 62.3 | 59.0 | 62.3 | 59.0 |
| 5b | ex | delegate_via_self | 3.01 | 5.00 | 0.00 | 5.00 | 0.00 |
| 5c | bt | hasField:s: | 169 | 30.1 | 28.0 | 30.1 | 28.0 |
| 5c | ex | has_key | 3.05 | 1.00 | 0.00 | 1.00 | 0.00 |
| 6a | bt | blockValue:blk: | 2.41 | 3.00 | 0.00 | 3.00 | 0.00 |
| 6a | ex | fun_call | 2.17 | 3.00 | 0.00 | 3.00 | 0.00 |
| 6b | bt | doSum:list: | 2444 | 305 | 404 | 305 | 404 |
| 6b | ex | enum_reduce_sum | 221 | 304 | 0.00 | 304 | 0.00 |
| 6b | ex | lists_foldl | 271 | 303 | 0.00 | 303 | 0.00 |
| 6c | bt | doNoAssign:list: | 320 | 304 | 0.00 | 304 | 0.00 |
| 6c | ex | enum_each | 301 | 304 | 0.00 | 304 | 0.00 |
| 6d | bt | collect:list: | 1535 | 404 | 200 | 404 | 200 |
| 6d | ex | enum_map | 1696 | 405 | 200 | 405 | 200 |
| 6e | bt | inject:list: | 214 | 303 | 0.00 | 303 | 0.00 |
| 6e | ex | enum_reduce_sum | 233 | 304 | 0.00 | 304 | 0.00 |
| 6e | ex | lists_foldl | 201 | 303 | 0.00 | 303 | 0.00 |
| 7a | bt | actorCall:c: | 3498 | 223 | 308 | 144 | 224 |
| 7a | ex | gen_call | 761 | 38.1 | 56.0 | 17.0 | 33.0 |
| 7b | bt | actorUpdate:c: | 3182 | 223 | 314 | 144 | 224 |
| 7b | ex | gen_update | 928 | 38.2 | 56.0 | 17.2 | 33.0 |
| 7c | bt | actorCast:c: | 2660 | 204 | 242 | 127 | 178 |
| 7c | ex | gen_cast | 134 | 19.5 | 12.0 | 7.02 | 3.00 |
| 7d | bt | actorProbe:c: | 3324 | 279 | 385 | 144 | 224 |
| 7d | ex | gen_probe | 899 | 38.2 | 56.0 | 17.2 | 33.0 |
| 8a | bt | onDoNoRaise: | 223 | 22.1 | 21.0 | 22.1 | 21.0 |
| 8a | ex | try_no_raise | 2.23 | 1.00 | 0.00 | 1.00 | 0.00 |
| 8b | bt | onDoRaise: | 66598 | 2764 | 22701 | 2218 | 21257 |
| 8b | ex | try_raise | 172 | 14.1 | 36.0 | 14.1 | 36.0 |
| 9a | bt | resultOk: | 288 | 51.1 | 46.0 | 51.1 | 46.0 |
| 9a | ex | ok_match | 21.2 | 3.01 | 3.00 | 3.01 | 3.00 |
| 9b | bt | resultIsOk:r: | 5.31 | 4.00 | 0.00 | 4.00 | 0.00 |
| 9b | ex | ok_is_ok | 0.90 | 1.00 | 0.00 | 1.00 | 0.00 |
| 10a | bt | dictPut:d: | 91.6 | 19.0 | 10.0 | 19.0 | 10.0 |
| 10a | ex | map_put | 39.3 | 2.03 | 6.00 | 2.03 | 6.00 |
| 10b | bt | dictAt:d: | 21.5 | 18.0 | 0.00 | 18.0 | 0.00 |
| 10b | ex | map_get | 2.95 | 4.00 | 0.00 | 4.00 | 0.00 |
| 10c | bt | listAdd:list: | 328 | 18.7 | 202 | 18.7 | 202 |
| 10c | ex | list_append | 326 | 5.70 | 200 | 5.70 | 200 |
| 10d | bt | listIncludes:list: | 37.1 | 20.0 | 0.00 | 20.0 | 0.00 |
| 10d | ex | list_member | 26.9 | 9.00 | 0.00 | 9.00 | 0.00 |
| 10e | bt | stringConcat:s: | 81.7 | 10.1 | 28.0 | 10.1 | 28.0 |
| 10e | ex | binary_concat | 85.1 | 1.04 | 8.00 | 1.04 | 8.00 |
| 11a | bt | sumToDo: | 152 | 103 | 7.00 | 103 | 7.00 |
| 11a | ex | sum_recursive | 58.7 | 103 | 0.00 | 103 | 0.00 |
| 11a | ex | sum_reduce_range | 193 | 253 | 0.00 | 253 | 0.00 |
| 11b | bt | add:a: | 0.77 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11b | ex | add | 0.74 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11c | bt | addUntyped:a: | 2.04 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11c | ex | add | 0.95 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11d | bt | ifTrueIfFalse:a: | 38.9 | 15.0 | 4.00 | 15.0 | 4.00 |
| 11d | ex | if_else | 0.97 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12a | bt | classOf:p: | 107 | 16.0 | 23.0 | 16.0 | 23.0 |
| 12a | ex | struct_of | 2.15 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12b | bt | isKindOf:p: | 943 | 75.3 | 108 | 75.3 | 108 |
| 12b | ex | is_struct_check | 1.74 | 1.00 | 0.00 | 1.00 | 0.00 |
| 13a | bt | ffiInt:a: | 30.1 | 8.01 | 2.00 | 8.01 | 2.00 |
| 13a | ex | apply_phash2 | 5.29 | 2.00 | 0.00 | 2.00 | 0.00 |
| 13a | ex | direct_phash2 | 5.20 | 2.00 | 0.00 | 2.00 | 0.00 |
| 13b | bt | ffiBinary:a: | 46.9 | 8.02 | 5.00 | 8.02 | 5.00 |
| 13b | ex | apply_integer_to_binary | 10.6 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13b | ex | direct_integer_to_binary | 10.8 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13c | bt | ffiOkTuple:d: | 127 | 14.1 | 17.0 | 14.1 | 17.0 |
| 13c | ex | apply_maps_find | 7.75 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13c | ex | direct_maps_find | 7.51 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13d | bt | ffiOk: | 56.0 | 11.0 | 7.00 | 11.0 | 7.00 |
| 13d | ex | apply_ffi_ok | 2.35 | 3.00 | 0.00 | 3.00 | 0.00 |
