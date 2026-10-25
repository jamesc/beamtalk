# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 18
# load average: 12:28  up 27 days, 22:18, 5 users, load averages: 17.75 34.89 37.06
# rounds 7
# load average after: 12:29  up 27 days, 22:19, 5 users, load averages: 12.43 31.96 35.92

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 0b | timesRepeat: whose block assigns nothing | 102 | 26.1 | 10.0 | -0.01 | 0.00 | 0.00 | - | - | - | 26.1 | 10.0 | send(N, 'timesRepeat:', [fun]); Integer>>timesRepeat: dispatches ifTrue:ifFalse: per step |
| 1a | getter on a sealed Value: p x / p.x | 3.61 | 3.00 | 0.00 | 1.63 | 0.00 | 0.00 | 2.2 | - | - | 3.00 | 0.00 | tag-guarded direct VbPoint:dispatch/3 (BT-4069); mismatch falls back to send/3 |
| 1b | getter on an open Value: p x / p.x | 169 | 29.1 | 28.0 | 1.44 | 0.00 | 0.00 | 116.9 | - | - | 29.1 | 28.0 | send(P, x, []) -> classify, tagged-map class lookup (ETS + cache), Mod:dispatch/3 (open receiver stays dynamic) |
| 1c | method on a sealed Value: p sum / Point.sum(p) | 4.93 | 3.00 | 0.00 | 1.70 | 2.00 | 0.00 | 2.9 | 1.5 | - | 1.00 | 0.00 | send(P, sum, []), same path as 1a |
| 2a | functional update: p withX: 3 / %{p \| x: 3} | 20.0 | 4.01 | 6.00 | 32.7 | 0.00 | 6.00 | 0.6 | - | 1.0 | 4.01 | 0.00 | send(P, 'withX:', [3]), same path as 1a + maps:put |
| 2b | construct: VbPoint x: a y: 4 / %Point{x: a, y: 4} | 39.5 | 2.05 | 6.00 | 32.3 | 0.05 | 6.00 | 1.2 | - | 1.0 | 2.00 | 0.00 | whereis_class + class_send: gen_server:call into the VbPoint class process |
| 3a | `class sealed` method, sealed class / Mod.fun(arg) | 1.30 | 2.00 | 0.00 | 1.55 | 2.00 | 0.00 | 0.8 | 1.0 | - | 0.00 | 0.00 | direct call Mod:'class_double:'(nil, 21) |
| 3b | class method, sealed class / Mod.fun(arg) | 1965 | 144 | 340 | 1.34 | 2.00 | 0.00 | 1467.7 | 71.8 | - | 142 | 340 | whereis_class + class_send: not `class sealed`, so gen_server:call into the class process |
| 3c | class method, open class / Mod.fun(arg) | 2087 | 144 | 373 | 1.48 | 2.00 | 0.00 | 1408.6 | 72.1 | - | 142 | 373 | whereis_class + class_send: gen_server:call into the class process |
| 4a | untyped receiver: o x / o.x | 167 | 29.1 | 28.0 | 1.54 | 0.00 | 0.00 | 108.7 | - | - | 29.1 | 28.0 | send(O, x, []) -> classify, tagged-map class lookup (ETS + cache), Mod:dispatch/3 (untyped receiver stays dynamic) |
| 4b | untyped receiver: o area / protocol | 173 | 29.1 | 28.0 | 9.34 | 7.00 | 0.00 | 18.5 | 4.2 | - | 22.1 | 28.0 | send(O, area, []), same path as 4a |
| 4c | typed open Value: s area / Square.area(s) | 170 | 29.1 | 28.0 | 1.75 | 2.00 | 0.00 | 97.4 | 14.5 | - | 27.1 | 28.0 | send(S, area, []), same path as 4a (open class stays dynamic) |
| 5a | inherited method: s base / defdelegate | 207 | 33.1 | 31.0 | 1.97 | 3.00 | 0.00 | 104.7 | 11.0 | - | 30.1 | 31.0 | send(S, base, []): VbSub:dispatch/3 misses, extensions lookup, VbBase:dispatch/3 |
| 5b | self-send to inherited: s viaSelf / local call | 456 | 61.3 | 59.0 | 2.22 | 4.00 | 0.00 | 205.8 | 15.3 | - | 57.3 | 59.0 | send(S, viaSelf, []), then `self base` is a second full send |
| 5c | runtime-resolved method: s hasField: #v / Map.has_key? | 180 | 29.1 | 28.0 | 2.29 | 0.00 | 0.00 | 78.4 | - | - | 29.1 | 28.0 | send(S, 'hasField:', [v]) -> beamtalk_reflection:has_field/2 |
| 6a | block value: x / fun.(x) | 1.56 | 2.00 | 0.00 | 1.37 | 2.00 | 0.00 | 1.1 | 1.0 | - | 0.00 | 0.00 | is_function(Blk, 1) guard, then apply Blk(41) |
| 6b | do: summing 100 elements / Enum.reduce | 2422 | 304 | 404 | 239 | 303 | 0.00 | 10.1 | 1.0 | - | 0.71 | 404 | lists:foldl over a fun that maps:get/maps:put `sum` in a state map; + with coercion try |
| 6c | do: over 100, no assignment / Enum.each | 323 | 303 | 0.00 | 329 | 303 | 0.00 | 1.0 | 1.0 | - | 0.00 | 0.00 | is_list guard, lists:foreach(fun) |
| 6d | collect: 100 elements / Enum.map | 1582 | 403 | 200 | 1781 | 404 | 200 | 0.9 | 1.0 | 1.0 | -0.99 | 0.00 | is_list guard, lists:map(fun); * with is_number guard |
| 6e | inject:into: 100 elements / Enum.reduce | 217 | 302 | 0.00 | 272 | 303 | 0.00 | 0.8 | 1.0 | - | -1.00 | 0.00 | is_list guard, lists:foldl(fun); + with coercion try |
| 7a | actor sync read: c count / GenServer.call | 2976 | 222 | 308 | 893 | 37.2 | 56.0 | 3.3 | 6.0 | 5.5 | 185 | 252 | send(C, count, []) -> beamtalk_actor:sync_send, gen_server:call + telemetry/trace/ctx hooks both sides |
| 7b | actor state update: c increment / GenServer.call | 3155 | 222 | 314 | 897 | 37.2 | 56.0 | 3.5 | 6.0 | 5.6 | 185 | 258 | as 7a |
| 7c | actor cast: c increment! / GenServer.cast | 2530 | 203 | 242 | 139 | 18.3 | 12.0 | 18.2 | 11.1 | 20.2 | 184 | 230 | beamtalk_message_dispatch:cast -> gen_server:cast + the same hooks |
| 7d | actor method doing self hasField: / GenServer.call | 3345 | 278 | 385 | 750 | 37.1 | 56.0 | 4.5 | 7.5 | 6.9 | 241 | 329 | as 7a; in the actor `self hasField:` misses the module and walks the runtime method resolver |
| 8a | on: Error do: without a raise / try-rescue | 238 | 21.2 | 21.0 | 1.47 | 0.00 | 0.00 | 161.6 | - | - | 21.2 | 21.0 | `Error` class ref built per entry (whereis_class + module_name gen_server:call), class_vars snapshot, try |
| 8b | on: Error do: with a raise / raise-rescue | 65075 | 2693 | 22615 | 173 | 13.2 | 36.0 | 376.1 | 204.4 | 628.2 | 2680 | 22579 | as 8a + `Error signal:` via the Error class process, which logs an error, rewraps, rebuilds a stack |
| 9a | (Result ok: x) value / {:ok, x} match | 447 | 68.3 | 76.0 | 20.4 | 2.02 | 3.00 | 21.9 | 33.8 | 25.3 | 66.3 | 73.0 | class_send(Result, 'ok:') gen_server:call; `value` as a block-or-send case |
| 9b | r isOk / match?({:ok, _}, r) | 4.54 | 3.00 | 0.00 | 0.15 | 0.00 | 0.00 | - | - | - | 3.00 | 0.00 | tag-guarded direct Result:dispatch/3 (BT-4069); mismatch falls back to send/3 |
| 10a | Dictionary at:put: / Map.put | 96.9 | 18.1 | 10.0 | 40.6 | 1.06 | 6.00 | 2.4 | 17.1 | 1.7 | 17.0 | 4.00 | send(D, 'at:put:', ...) -> Dictionary:dispatch/3 -> maps:put |
| 10b | Dictionary at: / Map.get | 20.7 | 17.0 | 0.00 | 2.19 | 3.00 | 0.00 | 9.4 | 5.7 | - | 14.0 | 0.00 | send(D, 'at:', ...) -> Dictionary:dispatch/3 -> maps:get |
| 10c | List add: (100 elements) / l ++ [x] | 348 | 17.7 | 202 | 327 | 4.73 | 200 | 1.1 | 3.7 | 1.0 | 13.0 | 2.00 | send(L, 'add:', [7]) -> List primitive (++) |
| 10d | List includes: (100 elements) / Enum.member? | 38.6 | 19.0 | 0.00 | 29.5 | 8.00 | 0.00 | 1.3 | 2.4 | - | 11.0 | 0.00 | send(L, 'includes:', [50]) -> List primitive (lists:member) |
| 10e | String ++ / <> | 94.2 | 9.09 | 28.0 | 93.7 | 0.04 | 8.00 | 1.0 | - | 3.5 | 9.05 | 20.0 | is_list guard, iolist_to_binary([binary_to_list(S), binary_to_list("!")]) |
| 11a | 1 to: 100 do: summing / recursive sum | 172 | 102 | 7.00 | 77.5 | 102 | 0.00 | 2.2 | 1.0 | - | 0.04 | 7.00 | letrec loop; + with is_number guard and coercion try |
| 11b | a + 1, a :: Integer / a + 1 | 0.00 | 0.00 | 0.00 | 0.21 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | erlang:'+'(A, 1) |
| 11c | a + 1, a untyped / a + 1 | 1.29 | 0.00 | 0.00 | 0.22 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | is_number guard, erlang:'+' |
| 11d | a > 0 ifTrue: [1] ifFalse: [2] / if | 44.4 | 14.0 | 4.00 | 0.22 | 0.00 | 0.00 | - | - | - | 14.0 | 4.00 | send(Bool, 'ifTrue:ifFalse:', [fun, fun]) -> True:dispatch/3 |
| 12a | p class / p.__struct__ | 115 | 15.0 | 23.0 | 1.50 | 0.00 | 0.00 | 76.8 | - | - | 15.0 | 23.0 | beamtalk_primitive:class_of_object: derived-cache hit (counter + ETS read + pid liveness) |
| 12b | p isKindOf: VbPoint / is_struct(p, Point) | 1202 | 92.8 | 166 | 0.98 | 0.00 | 0.00 | 1226.4 | - | - | 92.8 | 166 | class ref built per call (gen_server:call) + send(P, 'isKindOf:') -> includesBehaviour: via class processes |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 0b | `baseline` | -0.01 | 0.00 | 0.00 |
| 1a | `getter_dot` | 1.63 | 0.00 | 0.00 |
| 1a | `getter_fun` | 1.60 | 2.00 | 0.00 |
| 1a | `getter_match` | 1.01 | 0.00 | 0.00 |
| 1b | `getter_dot` | 1.44 | 0.00 | 0.00 |
| 1c | `point_sum` | 1.70 | 2.00 | 0.00 |
| 2a | `with_x` | 32.7 | 0.00 | 6.00 |
| 2b | `construct` | 32.3 | 0.05 | 6.00 |
| 3a | `remote_call` | 1.55 | 2.00 | 0.00 |
| 3b | `remote_call` | 1.34 | 2.00 | 0.00 |
| 3c | `remote_call` | 1.48 | 2.00 | 0.00 |
| 4a | `getter_dot` | 1.54 | 0.00 | 0.00 |
| 4b | `protocol` | 9.34 | 7.00 | 0.00 |
| 4b | `apply3` | 6.49 | 2.00 | 0.00 |
| 4b | `dyn_module` | 6.44 | 2.00 | 0.00 |
| 4c | `square_area` | 1.75 | 2.00 | 0.00 |
| 4c | `protocol` | 9.57 | 7.00 | 0.00 |
| 5a | `delegate` | 1.97 | 3.00 | 0.00 |
| 5b | `delegate_via_self` | 2.22 | 4.00 | 0.00 |
| 5c | `has_key` | 2.29 | 0.00 | 0.00 |
| 6a | `fun_call` | 1.37 | 2.00 | 0.00 |
| 6b | `enum_reduce_sum` | 239 | 303 | 0.00 |
| 6b | `lists_foldl` | 270 | 302 | 0.00 |
| 6c | `enum_each` | 329 | 303 | 0.00 |
| 6d | `enum_map` | 1781 | 404 | 200 |
| 6e | `enum_reduce_sum` | 272 | 303 | 0.00 |
| 6e | `lists_foldl` | 281 | 302 | 0.00 |
| 7a | `gen_call` | 893 | 37.2 | 56.0 |
| 7b | `gen_update` | 897 | 37.2 | 56.0 |
| 7c | `gen_cast` | 139 | 18.3 | 12.0 |
| 7d | `gen_probe` | 750 | 37.1 | 56.0 |
| 8a | `try_no_raise` | 1.47 | 0.00 | 0.00 |
| 8b | `try_raise` | 173 | 13.2 | 36.0 |
| 9a | `ok_match` | 20.4 | 2.02 | 3.00 |
| 9b | `ok_is_ok` | 0.15 | 0.00 | 0.00 |
| 10a | `map_put` | 40.6 | 1.06 | 6.00 |
| 10b | `map_get` | 2.19 | 3.00 | 0.00 |
| 10c | `list_append` | 327 | 4.73 | 200 |
| 10d | `list_member` | 29.5 | 8.00 | 0.00 |
| 10e | `binary_concat` | 93.7 | 0.04 | 8.00 |
| 11a | `sum_recursive` | 77.5 | 102 | 0.00 |
| 11a | `sum_reduce_range` | 193 | 252 | 0.00 |
| 11b | `add` | 0.21 | 0.00 | 0.00 |
| 11c | `add` | 0.22 | 0.00 | 0.00 |
| 11d | `if_else` | 0.22 | 0.00 | 0.00 |
| 12a | `struct_of` | 1.50 | 0.00 | 0.00 |
| 12b | `is_struct_check` | 0.98 | 0.00 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 0.75 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 0.75 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0b | bt | loopNoAssign: | 102 | 27.1 | 10.0 | 27.1 | 10.0 |
| 0b | ex | baseline | 0.74 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1a | bt | getterSealed:p: | 4.35 | 4.00 | 0.00 | 4.00 | 0.00 |
| 1a | ex | getter_dot | 2.38 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1a | ex | getter_fun | 2.35 | 3.00 | 0.00 | 3.00 | 0.00 |
| 1a | ex | getter_match | 1.76 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1b | bt | getterOpen:p: | 170 | 30.1 | 28.0 | 30.1 | 28.0 |
| 1b | ex | getter_dot | 2.20 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1c | bt | sumSealed:p: | 5.67 | 4.00 | 0.00 | 4.00 | 0.00 |
| 1c | ex | point_sum | 2.46 | 3.00 | 0.00 | 3.00 | 0.00 |
| 2a | bt | withX:p: | 20.8 | 5.01 | 6.00 | 5.01 | 6.00 |
| 2a | ex | with_x | 33.4 | 1.00 | 6.00 | 1.00 | 6.00 |
| 2b | bt | construct:a: | 40.2 | 3.05 | 6.00 | 3.05 | 6.00 |
| 2b | ex | construct | 33.0 | 1.05 | 6.00 | 1.05 | 6.00 |
| 3a | bt | classSealedMethod: | 2.05 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3a | ex | remote_call | 2.30 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3b | bt | sealedClassMethod: | 1965 | 145 | 340 | 43.3 | 100 |
| 3b | ex | remote_call | 2.09 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3c | bt | openClassMethod: | 2088 | 145 | 373 | 43.1 | 101 |
| 3c | ex | remote_call | 2.23 | 3.00 | 0.00 | 3.00 | 0.00 |
| 4a | bt | dynGetter:o: | 168 | 30.1 | 28.0 | 30.1 | 28.0 |
| 4a | ex | getter_dot | 2.29 | 1.00 | 0.00 | 1.00 | 0.00 |
| 4b | bt | dynArea:o: | 174 | 30.1 | 28.0 | 30.1 | 28.0 |
| 4b | ex | apply3 | 7.25 | 3.00 | 0.00 | 3.00 | 0.00 |
| 4b | ex | dyn_module | 7.19 | 3.00 | 0.00 | 3.00 | 0.00 |
| 4b | ex | protocol | 10.1 | 8.00 | 0.00 | 8.00 | 0.00 |
| 4c | bt | typedArea:s: | 171 | 30.1 | 28.0 | 30.1 | 28.0 |
| 4c | ex | protocol | 10.3 | 8.00 | 0.00 | 8.00 | 0.00 |
| 4c | ex | square_area | 2.50 | 3.00 | 0.00 | 3.00 | 0.00 |
| 5a | bt | inherited:s: | 207 | 34.1 | 31.0 | 34.1 | 31.0 |
| 5a | ex | delegate | 2.73 | 4.00 | 0.00 | 4.00 | 0.00 |
| 5b | bt | inheritedViaSelf:s: | 457 | 62.3 | 59.0 | 62.3 | 59.0 |
| 5b | ex | delegate_via_self | 2.97 | 5.00 | 0.00 | 5.00 | 0.00 |
| 5c | bt | hasField:s: | 180 | 30.1 | 28.0 | 30.1 | 28.0 |
| 5c | ex | has_key | 3.04 | 1.00 | 0.00 | 1.00 | 0.00 |
| 6a | bt | blockValue:blk: | 2.31 | 3.00 | 0.00 | 3.00 | 0.00 |
| 6a | ex | fun_call | 2.12 | 3.00 | 0.00 | 3.00 | 0.00 |
| 6b | bt | doSum:list: | 2423 | 305 | 404 | 305 | 404 |
| 6b | ex | enum_reduce_sum | 240 | 304 | 0.00 | 304 | 0.00 |
| 6b | ex | lists_foldl | 271 | 303 | 0.00 | 303 | 0.00 |
| 6c | bt | doNoAssign:list: | 324 | 304 | 0.00 | 304 | 0.00 |
| 6c | ex | enum_each | 329 | 304 | 0.00 | 304 | 0.00 |
| 6d | bt | collect:list: | 1583 | 404 | 200 | 404 | 200 |
| 6d | ex | enum_map | 1781 | 405 | 200 | 405 | 200 |
| 6e | bt | inject:list: | 218 | 303 | 0.00 | 303 | 0.00 |
| 6e | ex | enum_reduce_sum | 273 | 304 | 0.00 | 304 | 0.00 |
| 6e | ex | lists_foldl | 282 | 303 | 0.00 | 303 | 0.00 |
| 7a | bt | actorCall:c: | 2977 | 223 | 308 | 144 | 224 |
| 7a | ex | gen_call | 894 | 38.2 | 56.0 | 17.2 | 33.0 |
| 7b | bt | actorUpdate:c: | 3156 | 223 | 314 | 144 | 224 |
| 7b | ex | gen_update | 898 | 38.2 | 56.0 | 17.0 | 33.0 |
| 7c | bt | actorCast:c: | 2531 | 204 | 242 | 127 | 178 |
| 7c | ex | gen_cast | 140 | 19.3 | 12.0 | 7.02 | 3.00 |
| 7d | bt | actorProbe:c: | 3346 | 279 | 385 | 144 | 224 |
| 7d | ex | gen_probe | 751 | 38.1 | 56.0 | 17.0 | 33.0 |
| 8a | bt | onDoNoRaise: | 239 | 22.2 | 21.0 | 22.2 | 21.0 |
| 8a | ex | try_no_raise | 2.22 | 1.00 | 0.00 | 1.00 | 0.00 |
| 8b | bt | onDoRaise: | 65076 | 2694 | 22615 | 2149 | 21167 |
| 8b | ex | try_raise | 174 | 14.2 | 36.0 | 14.2 | 36.0 |
| 9a | bt | resultOk: | 448 | 69.3 | 76.0 | 69.3 | 76.0 |
| 9a | ex | ok_match | 21.1 | 3.02 | 3.00 | 3.02 | 3.00 |
| 9b | bt | resultIsOk:r: | 5.29 | 4.00 | 0.00 | 4.00 | 0.00 |
| 9b | ex | ok_is_ok | 0.90 | 1.00 | 0.00 | 1.00 | 0.00 |
| 10a | bt | dictPut:d: | 97.6 | 19.1 | 10.0 | 19.1 | 10.0 |
| 10a | ex | map_put | 41.3 | 2.06 | 6.00 | 2.06 | 6.00 |
| 10b | bt | dictAt:d: | 21.4 | 18.0 | 0.00 | 18.0 | 0.00 |
| 10b | ex | map_get | 2.94 | 4.00 | 0.00 | 4.00 | 0.00 |
| 10c | bt | listAdd:list: | 348 | 18.7 | 202 | 18.7 | 202 |
| 10c | ex | list_append | 327 | 5.73 | 200 | 5.73 | 200 |
| 10d | bt | listIncludes:list: | 39.3 | 20.0 | 0.00 | 20.0 | 0.00 |
| 10d | ex | list_member | 30.3 | 9.00 | 0.00 | 9.00 | 0.00 |
| 10e | bt | stringConcat:s: | 94.9 | 10.1 | 28.0 | 10.1 | 28.0 |
| 10e | ex | binary_concat | 94.5 | 1.04 | 8.00 | 1.04 | 8.00 |
| 11a | bt | sumToDo: | 172 | 103 | 7.00 | 103 | 7.00 |
| 11a | ex | sum_recursive | 78.3 | 103 | 0.00 | 103 | 0.00 |
| 11a | ex | sum_reduce_range | 194 | 253 | 0.00 | 253 | 0.00 |
| 11b | bt | add:a: | 0.75 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11b | ex | add | 0.96 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11c | bt | addUntyped:a: | 2.04 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11c | ex | add | 0.97 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11d | bt | ifTrueIfFalse:a: | 45.1 | 15.0 | 4.00 | 15.0 | 4.00 |
| 11d | ex | if_else | 0.97 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12a | bt | classOf:p: | 116 | 16.0 | 23.0 | 16.0 | 23.0 |
| 12a | ex | struct_of | 2.25 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12b | bt | isKindOf:p: | 1202 | 93.8 | 166 | 93.8 | 166 |
| 12b | ex | is_struct_check | 1.73 | 1.00 | 0.00 | 1.00 | 0.00 |
