# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 18
# load average: 22:11  up 24 days,  7:01, 4 users, load averages: 26.67 41.76 38.73
# rounds 9
# load average after: 22:12  up 24 days,  7:02, 4 users, load averages: 19.58 36.78 37.08

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 0b | timesRepeat: whose block assigns nothing | 95.6 | 26.0 | 10.0 | -0.04 | 0.00 | 0.00 | - | - | - | 26.0 | 10.0 | send(N, 'timesRepeat:', [fun]); Integer>>timesRepeat: dispatches ifTrue:ifFalse: per step |
| 1a | getter on a sealed Value: p x / p.x | 158 | 29.0 | 28.0 | 1.68 | 0.00 | 0.00 | 94.1 | - | - | 29.0 | 28.0 | send(P, x, []) -> classify, tagged-map class lookup (ETS + cache), Mod:dispatch/3 |
| 1b | getter on an open Value: p x / p.x | 152 | 29.0 | 28.0 | 1.47 | 0.00 | 0.00 | 103.4 | - | - | 29.0 | 28.0 | send(P, x, []), same path as 1a |
| 1c | method on a sealed Value: p sum / Point.sum(p) | 165 | 29.0 | 28.0 | 1.27 | 2.00 | 0.00 | 130.1 | 14.5 | - | 27.0 | 28.0 | send(P, sum, []), same path as 1a |
| 2a | functional update: p withX: 3 / %{p \| x: 3} | 325 | 30.2 | 34.0 | 31.8 | 0.00 | 6.00 | 10.2 | - | 5.7 | 30.2 | 28.0 | send(P, 'withX:', [3]), same path as 1a + maps:put |
| 2b | construct: VbPoint x: a y: 4 / %Point{x: a, y: 4} | 2790 | 147 | 480 | 32.3 | 0.03 | 6.00 | 86.4 | - | 80.0 | 147 | 474 | whereis_class + class_send: gen_server:call into the VbPoint class process |
| 3a | `class sealed` method, sealed class / Mod.fun(arg) | 0.94 | 2.00 | 0.00 | 0.83 | 2.00 | 0.00 | 1.1 | 1.0 | - | 0.00 | 0.00 | direct call Mod:'class_double:'(nil, 21) |
| 3b | class method, sealed class / Mod.fun(arg) | 2293 | 145 | 444 | 2.89 | 2.00 | 0.00 | 792.6 | 72.4 | - | 143 | 444 | whereis_class + class_send: not `class sealed`, so gen_server:call into the class process |
| 3c | class method, open class / Mod.fun(arg) | 2642 | 146 | 480 | 2.83 | 2.00 | 0.00 | 932.2 | 73.1 | - | 144 | 480 | whereis_class + class_send: gen_server:call into the class process |
| 4a | untyped receiver: o x / o.x | 169 | 29.0 | 28.0 | 1.53 | 0.00 | 0.00 | 110.7 | - | - | 29.0 | 28.0 | send(O, x, []), same path as 1a |
| 4b | untyped receiver: o area / protocol | 165 | 29.0 | 28.0 | 9.06 | 7.00 | 0.00 | 18.2 | 4.1 | - | 22.0 | 28.0 | send(O, area, []), same path as 1a |
| 4c | typed open Value: s area / Square.area(s) | 160 | 29.0 | 28.0 | 1.38 | 2.00 | 0.00 | 116.6 | 14.5 | - | 27.0 | 28.0 | send(S, area, []), same path as 1a |
| 5a | inherited method: s base / defdelegate | 198 | 33.1 | 31.0 | 1.55 | 3.00 | 0.00 | 127.1 | 11.0 | - | 30.1 | 31.0 | send(S, base, []): VbSub:dispatch/3 misses, extensions lookup, VbBase:dispatch/3 |
| 5b | self-send to inherited: s viaSelf / local call | 457 | 61.2 | 59.0 | 1.95 | 4.00 | 0.00 | 234.7 | 15.3 | - | 57.2 | 59.0 | send(S, viaSelf, []), then `self base` is a second full send |
| 5c | runtime-resolved method: s hasField: #v / Map.has_key? | 166 | 29.1 | 28.0 | 12.2 | 0.00 | 0.00 | 13.6 | - | - | 29.0 | 28.0 | send(S, 'hasField:', [v]) -> beamtalk_reflection:has_field/2 |
| 6a | block value: x / fun.(x) | 1.09 | 2.00 | 0.00 | 0.71 | 2.00 | 0.00 | 1.5 | 1.0 | - | 0.00 | 0.00 | is_function(Blk, 1) guard, then apply Blk(41) |
| 6b | do: summing 100 elements / Enum.reduce | 2348 | 303 | 404 | 278 | 303 | 0.00 | 8.4 | 1.0 | - | 0.07 | 404 | lists:foldl over a fun that maps:get/maps:put `sum` in a state map; + with coercion try |
| 6c | do: over 100, no assignment / Enum.each | 342 | 303 | 0.00 | 314 | 303 | 0.00 | 1.1 | 1.0 | - | 0.00 | 0.00 | is_list guard, lists:foreach(fun) |
| 6d | collect: 100 elements / Enum.map | 1345 | 403 | 200 | 1544 | 404 | 200 | 0.9 | 1.0 | 1.0 | -1.00 | 0.00 | is_list guard, lists:map(fun); * with is_number guard |
| 6e | inject:into: 100 elements / Enum.reduce | 230 | 302 | 0.00 | 261 | 303 | 0.00 | 0.9 | 1.0 | - | -1.00 | 0.00 | is_list guard, lists:foldl(fun); + with coercion try |
| 7a | actor sync read: c count / GenServer.call | 3315 | 211 | 312 | 945 | 37.2 | 56.0 | 3.5 | 5.7 | 5.6 | 174 | 256 | send(C, count, []) -> beamtalk_actor:sync_send, gen_server:call + telemetry/trace/ctx hooks both sides |
| 7b | actor state update: c increment / GenServer.call | 3528 | 211 | 318 | 965 | 37.2 | 56.0 | 3.7 | 5.7 | 5.7 | 174 | 262 | as 7a |
| 7c | actor cast: c increment! / GenServer.cast | 2309 | 194 | 242 | 135 | 18.5 | 12.0 | 17.1 | 10.4 | 20.2 | 175 | 230 | beamtalk_message_dispatch:cast -> gen_server:cast + the same hooks |
| 7d | actor method doing self hasField: / GenServer.call | 23170 | 849 | 3265 | 934 | 37.2 | 56.0 | 24.8 | 22.8 | 58.3 | 812 | 3209 | as 7a; in the actor `self hasField:` misses the module and walks the runtime method resolver |
| 8a | on: Error do: without a raise / try-rescue | 991 | 48.3 | 101 | 2.61 | 0.00 | 0.00 | 379.8 | - | - | 48.3 | 101 | `Error` class ref built per entry (whereis_class + module_name gen_server:call), class_vars snapshot, try |
| 8b | on: Error do: with a raise / raise-rescue | 61053 | 2658 | 22383 | 170 | 13.1 | 36.0 | 358.2 | 202.8 | 621.7 | 2645 | 22347 | as 8a + `Error signal:` via the Error class process, which logs an error, rewraps, rebuilds a stack |
| 9a | (Result ok: x) value / {:ok, x} match | 3138 | 216 | 762 | 18.4 | 2.01 | 3.00 | 171.0 | 107.2 | 254.0 | 214 | 759 | class_send(Result, 'ok:') gen_server:call; `value` as a block-or-send case |
| 9b | r isOk / match?({:ok, _}, r) | 181 | 25.1 | 27.0 | 0.17 | 0.00 | 0.00 | - | - | - | 25.1 | 27.0 | send(R, isOk, []), same path as 1a |
| 10a | Dictionary at:put: / Map.put | 89.2 | 18.0 | 10.0 | 41.1 | 1.03 | 6.00 | 2.2 | 17.6 | 1.7 | 17.0 | 4.00 | send(D, 'at:put:', ...) -> Dictionary:dispatch/3 -> maps:put |
| 10b | Dictionary at: / Map.get | 20.2 | 17.0 | 0.00 | 1.56 | 3.00 | 0.00 | 12.9 | 5.7 | - | 14.0 | 0.00 | send(D, 'at:', ...) -> Dictionary:dispatch/3 -> maps:get |
| 10c | List add: (100 elements) / l ++ [x] | 337 | 17.7 | 202 | 316 | 4.69 | 200 | 1.1 | 3.8 | 1.0 | 13.0 | 2.00 | send(L, 'add:', [7]) -> List primitive (++) |
| 10d | List includes: (100 elements) / Enum.member? | 39.5 | 19.0 | 0.00 | 29.7 | 8.00 | 0.00 | 1.3 | 2.4 | - | 11.0 | 0.00 | send(L, 'includes:', [50]) -> List primitive (lists:member) |
| 10e | String ++ / <> | 81.4 | 9.06 | 28.0 | 91.8 | 0.04 | 8.00 | 0.9 | - | 3.5 | 9.03 | 20.0 | is_list guard, iolist_to_binary([binary_to_list(S), binary_to_list("!")]) |
| 11a | 1 to: 100 do: summing / recursive sum | 154 | 102 | 7.00 | 66.3 | 102 | 0.00 | 2.3 | 1.0 | - | 0.03 | 7.00 | letrec loop; + with is_number guard and coercion try |
| 11b | a + 1, a :: Integer / a + 1 | 0.23 | 0.00 | 0.00 | 0.01 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | erlang:'+'(A, 1) |
| 11c | a + 1, a untyped / a + 1 | 1.31 | 0.00 | 0.00 | 0.24 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | is_number guard, erlang:'+' |
| 11d | a > 0 ifTrue: [1] ifFalse: [2] / if | 43.1 | 14.0 | 4.00 | 0.24 | 0.00 | 0.00 | - | - | - | 14.0 | 4.00 | send(Bool, 'ifTrue:ifFalse:', [fun, fun]) -> True:dispatch/3 |
| 12a | p class / p.__struct__ | 323 | 26.2 | 83.0 | 1.48 | 0.00 | 0.00 | 217.6 | - | - | 26.2 | 83.0 | beamtalk_primitive:class_of_object: ETS lookups + list_to_atom per call |
| 12b | p isKindOf: VbPoint / is_struct(p, Point) | 6814 | 415 | 1358 | 11.1 | 0.00 | 0.00 | 611.2 | - | - | 415 | 1358 | class ref built per call (gen_server:call) + send(P, 'isKindOf:') -> includesBehaviour: via class processes |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 0b | `baseline` | -0.04 | 0.00 | 0.00 |
| 1a | `getter_dot` | 1.68 | 0.00 | 0.00 |
| 1a | `getter_fun` | 1.26 | 2.00 | 0.00 |
| 1a | `getter_match` | 1.00 | 0.00 | 0.00 |
| 1b | `getter_dot` | 1.47 | 0.00 | 0.00 |
| 1c | `point_sum` | 1.27 | 2.00 | 0.00 |
| 2a | `with_x` | 31.8 | 0.00 | 6.00 |
| 2b | `construct` | 32.3 | 0.03 | 6.00 |
| 3a | `remote_call` | 0.83 | 2.00 | 0.00 |
| 3b | `remote_call` | 2.89 | 2.00 | 0.00 |
| 3c | `remote_call` | 2.83 | 2.00 | 0.00 |
| 4a | `getter_dot` | 1.53 | 0.00 | 0.00 |
| 4b | `protocol` | 9.06 | 7.00 | 0.00 |
| 4b | `apply3` | 6.29 | 2.00 | 0.00 |
| 4b | `dyn_module` | 7.70 | 2.00 | 0.00 |
| 4c | `square_area` | 1.38 | 2.00 | 0.00 |
| 4c | `protocol` | 8.79 | 7.00 | 0.00 |
| 5a | `delegate` | 1.55 | 3.00 | 0.00 |
| 5b | `delegate_via_self` | 1.95 | 4.00 | 0.00 |
| 5c | `has_key` | 12.2 | 0.00 | 0.00 |
| 6a | `fun_call` | 0.71 | 2.00 | 0.00 |
| 6b | `enum_reduce_sum` | 278 | 303 | 0.00 |
| 6b | `lists_foldl` | 296 | 302 | 0.00 |
| 6c | `enum_each` | 314 | 303 | 0.00 |
| 6d | `enum_map` | 1544 | 404 | 200 |
| 6e | `enum_reduce_sum` | 261 | 303 | 0.00 |
| 6e | `lists_foldl` | 281 | 302 | 0.00 |
| 7a | `gen_call` | 945 | 37.2 | 56.0 |
| 7b | `gen_update` | 965 | 37.2 | 56.0 |
| 7c | `gen_cast` | 135 | 18.5 | 12.0 |
| 7d | `gen_probe` | 934 | 37.2 | 56.0 |
| 8a | `try_no_raise` | 2.61 | 0.00 | 0.00 |
| 8b | `try_raise` | 170 | 13.1 | 36.0 |
| 9a | `ok_match` | 18.4 | 2.01 | 3.00 |
| 9b | `ok_is_ok` | 0.17 | 0.00 | 0.00 |
| 10a | `map_put` | 41.1 | 1.03 | 6.00 |
| 10b | `map_get` | 1.56 | 3.00 | 0.00 |
| 10c | `list_append` | 316 | 4.69 | 200 |
| 10d | `list_member` | 29.7 | 8.00 | 0.00 |
| 10e | `binary_concat` | 91.8 | 0.04 | 8.00 |
| 11a | `sum_recursive` | 66.3 | 102 | 0.00 |
| 11a | `sum_reduce_range` | 185 | 252 | 0.00 |
| 11b | `add` | 0.01 | 0.00 | 0.00 |
| 11c | `add` | 0.24 | 0.00 | 0.00 |
| 11d | `if_else` | 0.24 | 0.00 | 0.00 |
| 12a | `struct_of` | 1.48 | 0.00 | 0.00 |
| 12b | `is_struct_check` | 11.1 | 0.00 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.86 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.83 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0b | bt | loopNoAssign: | 97.5 | 27.0 | 10.0 | 27.0 | 10.0 |
| 0b | ex | baseline | 1.79 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1a | bt | getterSealed:p: | 160 | 30.0 | 28.0 | 30.0 | 28.0 |
| 1a | ex | getter_dot | 3.51 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1a | ex | getter_fun | 3.09 | 3.00 | 0.00 | 3.00 | 0.00 |
| 1a | ex | getter_match | 2.83 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1b | bt | getterOpen:p: | 154 | 30.0 | 28.0 | 30.0 | 28.0 |
| 1b | ex | getter_dot | 3.30 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1c | bt | sumSealed:p: | 167 | 30.0 | 28.0 | 30.0 | 28.0 |
| 1c | ex | point_sum | 3.10 | 3.00 | 0.00 | 3.00 | 0.00 |
| 2a | bt | withX:p: | 327 | 31.2 | 34.0 | 31.2 | 34.0 |
| 2a | ex | with_x | 33.6 | 1.00 | 6.00 | 1.00 | 6.00 |
| 2b | bt | construct:a: | 2792 | 148 | 480 | 46.1 | 238 |
| 2b | ex | construct | 34.1 | 1.03 | 6.00 | 1.03 | 6.00 |
| 3a | bt | classSealedMethod: | 2.80 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3a | ex | remote_call | 2.66 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3b | bt | sealedClassMethod: | 2295 | 146 | 444 | 45.6 | 214 |
| 3b | ex | remote_call | 4.72 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3c | bt | openClassMethod: | 2644 | 147 | 480 | 45.9 | 222 |
| 3c | ex | remote_call | 4.66 | 3.00 | 0.00 | 3.00 | 0.00 |
| 4a | bt | dynGetter:o: | 171 | 30.0 | 28.0 | 30.0 | 28.0 |
| 4a | ex | getter_dot | 3.36 | 1.00 | 0.00 | 1.00 | 0.00 |
| 4b | bt | dynArea:o: | 166 | 30.0 | 28.0 | 30.0 | 28.0 |
| 4b | ex | apply3 | 8.11 | 3.00 | 0.00 | 3.00 | 0.00 |
| 4b | ex | dyn_module | 9.53 | 3.00 | 0.00 | 3.00 | 0.00 |
| 4b | ex | protocol | 10.9 | 8.00 | 0.00 | 8.00 | 0.00 |
| 4c | bt | typedArea:s: | 162 | 30.0 | 28.0 | 30.0 | 28.0 |
| 4c | ex | protocol | 10.6 | 8.00 | 0.00 | 8.00 | 0.00 |
| 4c | ex | square_area | 3.21 | 3.00 | 0.00 | 3.00 | 0.00 |
| 5a | bt | inherited:s: | 199 | 34.1 | 31.0 | 34.1 | 31.0 |
| 5a | ex | delegate | 3.38 | 4.00 | 0.00 | 4.00 | 0.00 |
| 5b | bt | inheritedViaSelf:s: | 459 | 62.2 | 59.0 | 62.2 | 59.0 |
| 5b | ex | delegate_via_self | 3.78 | 5.00 | 0.00 | 5.00 | 0.00 |
| 5c | bt | hasField:s: | 168 | 30.1 | 28.0 | 30.1 | 28.0 |
| 5c | ex | has_key | 14.0 | 1.00 | 0.00 | 1.00 | 0.00 |
| 6a | bt | blockValue:blk: | 2.94 | 3.00 | 0.00 | 3.00 | 0.00 |
| 6a | ex | fun_call | 2.54 | 3.00 | 0.00 | 3.00 | 0.00 |
| 6b | bt | doSum:list: | 2350 | 304 | 404 | 304 | 404 |
| 6b | ex | enum_reduce_sum | 280 | 304 | 0.00 | 304 | 0.00 |
| 6b | ex | lists_foldl | 298 | 303 | 0.00 | 303 | 0.00 |
| 6c | bt | doNoAssign:list: | 344 | 304 | 0.00 | 304 | 0.00 |
| 6c | ex | enum_each | 316 | 304 | 0.00 | 304 | 0.00 |
| 6d | bt | collect:list: | 1347 | 404 | 200 | 404 | 200 |
| 6d | ex | enum_map | 1546 | 405 | 200 | 405 | 200 |
| 6e | bt | inject:list: | 232 | 303 | 0.00 | 303 | 0.00 |
| 6e | ex | enum_reduce_sum | 263 | 304 | 0.00 | 304 | 0.00 |
| 6e | ex | lists_foldl | 283 | 303 | 0.00 | 303 | 0.00 |
| 7a | bt | actorCall:c: | 3317 | 212 | 312 | 137 | 227 |
| 7a | ex | gen_call | 947 | 38.2 | 56.0 | 17.2 | 33.0 |
| 7b | bt | actorUpdate:c: | 3530 | 212 | 318 | 137 | 227 |
| 7b | ex | gen_update | 967 | 38.2 | 56.0 | 17.2 | 33.0 |
| 7c | bt | actorCast:c: | 2311 | 195 | 242 | 123 | 181 |
| 7c | ex | gen_cast | 137 | 19.5 | 12.0 | 7.02 | 3.00 |
| 7d | bt | actorProbe:c: | 23172 | 850 | 3265 | 137 | 227 |
| 7d | ex | gen_probe | 936 | 38.2 | 56.0 | 17.2 | 33.0 |
| 8a | bt | onDoNoRaise: | 993 | 49.3 | 101 | 28.3 | 78.0 |
| 8a | ex | try_no_raise | 4.44 | 1.00 | 0.00 | 1.00 | 0.00 |
| 8b | bt | onDoRaise: | 61054 | 2659 | 22383 | 2098 | 20935 |
| 8b | ex | try_raise | 172 | 14.1 | 36.0 | 14.1 | 36.0 |
| 9a | bt | resultOk: | 3140 | 217 | 762 | 88.3 | 485 |
| 9a | ex | ok_match | 20.2 | 3.01 | 3.00 | 3.01 | 3.00 |
| 9b | bt | resultIsOk:r: | 183 | 26.1 | 27.0 | 26.1 | 27.0 |
| 9b | ex | ok_is_ok | 2.00 | 1.00 | 0.00 | 1.00 | 0.00 |
| 10a | bt | dictPut:d: | 91.1 | 19.0 | 10.0 | 19.0 | 10.0 |
| 10a | ex | map_put | 43.0 | 2.03 | 6.00 | 2.03 | 6.00 |
| 10b | bt | dictAt:d: | 22.0 | 18.0 | 0.00 | 18.0 | 0.00 |
| 10b | ex | map_get | 3.39 | 4.00 | 0.00 | 4.00 | 0.00 |
| 10c | bt | listAdd:list: | 338 | 18.7 | 202 | 18.7 | 202 |
| 10c | ex | list_append | 318 | 5.69 | 200 | 5.69 | 200 |
| 10d | bt | listIncludes:list: | 41.3 | 20.0 | 0.00 | 20.0 | 0.00 |
| 10d | ex | list_member | 31.5 | 9.00 | 0.00 | 9.00 | 0.00 |
| 10e | bt | stringConcat:s: | 83.2 | 10.1 | 28.0 | 10.1 | 28.0 |
| 10e | ex | binary_concat | 93.6 | 1.04 | 8.00 | 1.04 | 8.00 |
| 11a | bt | sumToDo: | 156 | 103 | 7.00 | 103 | 7.00 |
| 11a | ex | sum_recursive | 68.1 | 103 | 0.00 | 103 | 0.00 |
| 11a | ex | sum_reduce_range | 187 | 253 | 0.00 | 253 | 0.00 |
| 11b | bt | add:a: | 2.08 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11b | ex | add | 1.84 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11c | bt | addUntyped:a: | 3.17 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11c | ex | add | 2.07 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11d | bt | ifTrueIfFalse:a: | 44.9 | 15.0 | 4.00 | 15.0 | 4.00 |
| 11d | ex | if_else | 2.07 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12a | bt | classOf:p: | 325 | 27.2 | 83.0 | 27.2 | 83.0 |
| 12a | ex | struct_of | 3.31 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12b | bt | isKindOf:p: | 6816 | 416 | 1358 | 274 | 954 |
| 12b | ex | is_struct_check | 13.0 | 1.00 | 0.00 | 1.00 | 0.00 |
