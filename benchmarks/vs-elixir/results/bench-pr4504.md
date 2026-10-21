# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 18
# load average: 22:06  up 24 days,  6:56, 4 users, load averages: 58.89 62.22 42.50
# rounds 7
# load average after: 22:06  up 24 days,  6:56, 4 users, load averages: 33.59 55.34 40.88

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 0b | timesRepeat: whose block assigns nothing | 93.0 | 26.0 | 10.0 | 0.01 | 0.00 | 0.00 | - | - | - | 26.0 | 10.0 | send(N, 'timesRepeat:', [fun]); Integer>>timesRepeat: dispatches ifTrue:ifFalse: per step |
| 1a | getter on a sealed Value: p x / p.x | 156 | 29.0 | 28.0 | 1.37 | 0.00 | 0.00 | 114.3 | - | - | 29.0 | 28.0 | send(P, x, []) -> classify, tagged-map class lookup (ETS + cache), Mod:dispatch/3 |
| 1b | getter on an open Value: p x / p.x | 155 | 29.0 | 28.0 | 1.40 | 0.00 | 0.00 | 110.7 | - | - | 29.0 | 28.0 | send(P, x, []), same path as 1a |
| 1c | method on a sealed Value: p sum / Point.sum(p) | 186 | 29.0 | 28.0 | 1.23 | 2.00 | 0.00 | 151.9 | 14.5 | - | 27.0 | 28.0 | send(P, sum, []), same path as 1a |
| 2a | functional update: p withX: 3 / %{p \| x: 3} | 372 | 30.2 | 34.0 | 32.2 | 0.00 | 6.00 | 11.6 | - | 5.7 | 30.2 | 28.0 | send(P, 'withX:', [3]), same path as 1a + maps:put |
| 2b | construct: VbPoint x: a y: 4 / %Point{x: a, y: 4} | 1897 | 143 | 371 | 33.3 | 0.03 | 6.00 | 57.0 | - | 61.8 | 143 | 365 | whereis_class + class_send: gen_server:call into the VbPoint class process |
| 3a | `class sealed` method, sealed class / Mod.fun(arg) | 0.86 | 2.00 | 0.00 | 0.85 | 2.00 | 0.00 | 1.0 | 1.0 | - | 0.00 | 0.00 | direct call Mod:'class_double:'(nil, 21) |
| 3b | class method, sealed class / Mod.fun(arg) | 2715 | 144 | 337 | 2.67 | 2.00 | 0.00 | 1016.5 | 72.0 | - | 142 | 337 | whereis_class + class_send: not `class sealed`, so gen_server:call into the class process |
| 3c | class method, open class / Mod.fun(arg) | 2561 | 144 | 366 | 2.87 | 2.00 | 0.00 | 893.3 | 72.1 | - | 142 | 366 | whereis_class + class_send: gen_server:call into the class process |
| 4a | untyped receiver: o x / o.x | 179 | 29.0 | 28.0 | 1.30 | 0.00 | 0.00 | 137.0 | - | - | 29.0 | 28.0 | send(O, x, []), same path as 1a |
| 4b | untyped receiver: o area / protocol | 160 | 29.0 | 28.0 | 8.94 | 7.00 | 0.00 | 17.8 | 4.1 | - | 22.0 | 28.0 | send(O, area, []), same path as 1a |
| 4c | typed open Value: s area / Square.area(s) | 158 | 29.0 | 28.0 | 1.28 | 2.00 | 0.00 | 123.1 | 14.5 | - | 27.0 | 28.0 | send(S, area, []), same path as 1a |
| 5a | inherited method: s base / defdelegate | 192 | 33.1 | 31.0 | 1.48 | 3.00 | 0.00 | 129.7 | 11.0 | - | 30.1 | 31.0 | send(S, base, []): VbSub:dispatch/3 misses, extensions lookup, VbBase:dispatch/3 |
| 5b | self-send to inherited: s viaSelf / local call | 439 | 61.2 | 59.0 | 1.72 | 4.00 | 0.00 | 254.8 | 15.3 | - | 57.2 | 59.0 | send(S, viaSelf, []), then `self base` is a second full send |
| 5c | runtime-resolved method: s hasField: #v / Map.has_key? | 177 | 29.1 | 28.0 | 11.9 | 0.00 | 0.00 | 14.9 | - | - | 29.0 | 28.0 | send(S, 'hasField:', [v]) -> beamtalk_reflection:has_field/2 |
| 6a | block value: x / fun.(x) | 1.11 | 2.00 | 0.00 | 0.94 | 2.00 | 0.00 | 1.2 | 1.0 | - | 0.00 | 0.00 | is_function(Blk, 1) guard, then apply Blk(41) |
| 6b | do: summing 100 elements / Enum.reduce | 2318 | 303 | 404 | 280 | 303 | 0.00 | 8.3 | 1.0 | - | 0.07 | 404 | lists:foldl over a fun that maps:get/maps:put `sum` in a state map; + with coercion try |
| 6c | do: over 100, no assignment / Enum.each | 334 | 303 | 0.00 | 327 | 303 | 0.00 | 1.0 | 1.0 | - | 0.00 | 0.00 | is_list guard, lists:foreach(fun) |
| 6d | collect: 100 elements / Enum.map | 1334 | 403 | 200 | 1467 | 404 | 200 | 0.9 | 1.0 | 1.0 | -1.00 | 0.00 | is_list guard, lists:map(fun); * with is_number guard |
| 6e | inject:into: 100 elements / Enum.reduce | 215 | 302 | 0.00 | 265 | 303 | 0.00 | 0.8 | 1.0 | - | -1.00 | 0.00 | is_list guard, lists:foldl(fun); + with coercion try |
| 7a | actor sync read: c count / GenServer.call | 3378 | 209 | 316 | 894 | 37.2 | 56.0 | 3.8 | 5.6 | 5.6 | 172 | 260 | send(C, count, []) -> beamtalk_actor:sync_send, gen_server:call + telemetry/trace/ctx hooks both sides |
| 7b | actor state update: c increment / GenServer.call | 3230 | 209 | 322 | 904 | 37.2 | 56.0 | 3.6 | 5.6 | 5.7 | 172 | 266 | as 7a |
| 7c | actor cast: c increment! / GenServer.cast | 2283 | 192 | 246 | 134 | 18.5 | 12.0 | 17.1 | 10.4 | 20.5 | 173 | 234 | beamtalk_message_dispatch:cast -> gen_server:cast + the same hooks |
| 7d | actor method doing self hasField: / GenServer.call | 21549 | 819 | 2760 | 906 | 37.2 | 56.0 | 23.8 | 22.0 | 49.3 | 781 | 2704 | as 7a; in the actor `self hasField:` misses the module and walks the runtime method resolver |
| 8a | on: Error do: without a raise / try-rescue | 231 | 21.1 | 21.0 | 2.59 | 0.00 | 0.00 | 89.3 | - | - | 21.1 | 21.0 | `Error` class ref built per entry (whereis_class + module_name gen_server:call), class_vars snapshot, try |
| 8b | on: Error do: with a raise / raise-rescue | 66767 | 2711 | 22267 | 174 | 13.1 | 36.0 | 383.9 | 206.9 | 618.5 | 2698 | 22231 | as 8a + `Error signal:` via the Error class process, which logs an error, rewraps, rebuilds a stack |
| 9a | (Result ok: x) value / {:ok, x} match | 438 | 68.2 | 76.0 | 18.3 | 2.01 | 3.00 | 24.0 | 33.9 | 25.3 | 66.2 | 73.0 | class_send(Result, 'ok:') gen_server:call; `value` as a block-or-send case |
| 9b | r isOk / match?({:ok, _}, r) | 176 | 25.1 | 27.0 | 0.19 | 0.00 | 0.00 | - | - | - | 25.1 | 27.0 | send(R, isOk, []), same path as 1a |
| 10a | Dictionary at:put: / Map.put | 88.6 | 18.0 | 10.0 | 38.7 | 1.03 | 6.00 | 2.3 | 17.6 | 1.7 | 17.0 | 4.00 | send(D, 'at:put:', ...) -> Dictionary:dispatch/3 -> maps:put |
| 10b | Dictionary at: / Map.get | 19.9 | 17.0 | 0.00 | 1.74 | 3.00 | 0.00 | 11.4 | 5.7 | - | 14.0 | 0.00 | send(D, 'at:', ...) -> Dictionary:dispatch/3 -> maps:get |
| 10c | List add: (100 elements) / l ++ [x] | 336 | 17.7 | 202 | 311 | 4.69 | 200 | 1.1 | 3.8 | 1.0 | 13.0 | 2.00 | send(L, 'add:', [7]) -> List primitive (++) |
| 10d | List includes: (100 elements) / Enum.member? | 39.6 | 19.0 | 0.00 | 29.5 | 8.00 | 0.00 | 1.3 | 2.4 | - | 11.0 | 0.00 | send(L, 'includes:', [50]) -> List primitive (lists:member) |
| 10e | String ++ / <> | 81.0 | 9.06 | 28.0 | 91.0 | 0.04 | 8.00 | 0.9 | - | 3.5 | 9.03 | 20.0 | is_list guard, iolist_to_binary([binary_to_list(S), binary_to_list("!")]) |
| 11a | 1 to: 100 do: summing / recursive sum | 148 | 102 | 7.00 | 59.3 | 102 | 0.00 | 2.5 | 1.0 | - | 0.03 | 7.00 | letrec loop; + with is_number guard and coercion try |
| 11b | a + 1, a :: Integer / a + 1 | 0.31 | 0.00 | 0.00 | -0.84 | 0.00 | 0.00 | -0.4 | - | - | 0.00 | 0.00 | erlang:'+'(A, 1) |
| 11c | a + 1, a untyped / a + 1 | 1.41 | 0.00 | 0.00 | 0.23 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | is_number guard, erlang:'+' |
| 11d | a > 0 ifTrue: [1] ifFalse: [2] / if | 37.1 | 14.0 | 4.00 | 0.26 | 0.00 | 0.00 | - | - | - | 14.0 | 4.00 | send(Bool, 'ifTrue:ifFalse:', [fun, fun]) -> True:dispatch/3 |
| 12a | p class / p.__struct__ | 421 | 25.2 | 44.0 | 1.28 | 0.00 | 0.00 | 329.0 | - | - | 25.2 | 44.0 | beamtalk_primitive:class_of_object: ETS lookups + list_to_atom per call |
| 12b | p isKindOf: VbPoint / is_struct(p, Point) | 862 | 118 | 194 | 10.8 | 0.00 | 0.00 | 79.8 | - | - | 118 | 194 | class ref built per call (gen_server:call) + send(P, 'isKindOf:') -> includesBehaviour: via class processes |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 0b | `baseline` | 0.01 | 0.00 | 0.00 |
| 1a | `getter_dot` | 1.37 | 0.00 | 0.00 |
| 1a | `getter_fun` | 1.07 | 2.00 | 0.00 |
| 1a | `getter_match` | 0.87 | 0.00 | 0.00 |
| 1b | `getter_dot` | 1.40 | 0.00 | 0.00 |
| 1c | `point_sum` | 1.23 | 2.00 | 0.00 |
| 2a | `with_x` | 32.2 | 0.00 | 6.00 |
| 2b | `construct` | 33.3 | 0.03 | 6.00 |
| 3a | `remote_call` | 0.85 | 2.00 | 0.00 |
| 3b | `remote_call` | 2.67 | 2.00 | 0.00 |
| 3c | `remote_call` | 2.87 | 2.00 | 0.00 |
| 4a | `getter_dot` | 1.30 | 0.00 | 0.00 |
| 4b | `protocol` | 8.94 | 7.00 | 0.00 |
| 4b | `apply3` | 6.00 | 2.00 | 0.00 |
| 4b | `dyn_module` | 6.03 | 2.00 | 0.00 |
| 4c | `square_area` | 1.28 | 2.00 | 0.00 |
| 4c | `protocol` | 8.80 | 7.00 | 0.00 |
| 5a | `delegate` | 1.48 | 3.00 | 0.00 |
| 5b | `delegate_via_self` | 1.72 | 4.00 | 0.00 |
| 5c | `has_key` | 11.9 | 0.00 | 0.00 |
| 6a | `fun_call` | 0.94 | 2.00 | 0.00 |
| 6b | `enum_reduce_sum` | 280 | 303 | 0.00 |
| 6b | `lists_foldl` | 255 | 302 | 0.00 |
| 6c | `enum_each` | 327 | 303 | 0.00 |
| 6d | `enum_map` | 1467 | 404 | 200 |
| 6e | `enum_reduce_sum` | 265 | 303 | 0.00 |
| 6e | `lists_foldl` | 251 | 302 | 0.00 |
| 7a | `gen_call` | 894 | 37.2 | 56.0 |
| 7b | `gen_update` | 904 | 37.2 | 56.0 |
| 7c | `gen_cast` | 134 | 18.5 | 12.0 |
| 7d | `gen_probe` | 906 | 37.2 | 56.0 |
| 8a | `try_no_raise` | 2.59 | 0.00 | 0.00 |
| 8b | `try_raise` | 174 | 13.1 | 36.0 |
| 9a | `ok_match` | 18.3 | 2.01 | 3.00 |
| 9b | `ok_is_ok` | 0.19 | 0.00 | 0.00 |
| 10a | `map_put` | 38.7 | 1.03 | 6.00 |
| 10b | `map_get` | 1.74 | 3.00 | 0.00 |
| 10c | `list_append` | 311 | 4.69 | 200 |
| 10d | `list_member` | 29.5 | 8.00 | 0.00 |
| 10e | `binary_concat` | 91.0 | 0.04 | 8.00 |
| 11a | `sum_recursive` | 59.3 | 102 | 0.00 |
| 11a | `sum_reduce_range` | 190 | 252 | 0.00 |
| 11b | `add` | -0.84 | 0.00 | 0.00 |
| 11c | `add` | 0.23 | 0.00 | 0.00 |
| 11d | `if_else` | 0.26 | 0.00 | 0.00 |
| 12a | `struct_of` | 1.28 | 0.00 | 0.00 |
| 12b | `is_struct_check` | 10.8 | 0.00 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.74 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.82 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0b | bt | loopNoAssign: | 94.8 | 27.0 | 10.0 | 27.0 | 10.0 |
| 0b | ex | baseline | 1.83 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1a | bt | getterSealed:p: | 158 | 30.0 | 28.0 | 30.0 | 28.0 |
| 1a | ex | getter_dot | 3.19 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1a | ex | getter_fun | 2.89 | 3.00 | 0.00 | 3.00 | 0.00 |
| 1a | ex | getter_match | 2.69 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1b | bt | getterOpen:p: | 156 | 30.0 | 28.0 | 30.0 | 28.0 |
| 1b | ex | getter_dot | 3.22 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1c | bt | sumSealed:p: | 188 | 30.0 | 28.0 | 30.0 | 28.0 |
| 1c | ex | point_sum | 3.05 | 3.00 | 0.00 | 3.00 | 0.00 |
| 2a | bt | withX:p: | 374 | 31.2 | 34.0 | 31.2 | 34.0 |
| 2a | ex | with_x | 34.0 | 1.00 | 6.00 | 1.00 | 6.00 |
| 2b | bt | construct:a: | 1899 | 144 | 371 | 43.1 | 121 |
| 2b | ex | construct | 35.1 | 1.03 | 6.00 | 1.03 | 6.00 |
| 3a | bt | classSealedMethod: | 2.60 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3a | ex | remote_call | 2.67 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3b | bt | sealedClassMethod: | 2717 | 145 | 337 | 43.5 | 99.0 |
| 3b | ex | remote_call | 4.49 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3c | bt | openClassMethod: | 2563 | 145 | 366 | 43.3 | 100 |
| 3c | ex | remote_call | 4.69 | 3.00 | 0.00 | 3.00 | 0.00 |
| 4a | bt | dynGetter:o: | 180 | 30.0 | 28.0 | 30.0 | 28.0 |
| 4a | ex | getter_dot | 3.12 | 1.00 | 0.00 | 1.00 | 0.00 |
| 4b | bt | dynArea:o: | 161 | 30.0 | 28.0 | 30.0 | 28.0 |
| 4b | ex | apply3 | 7.82 | 3.00 | 0.00 | 3.00 | 0.00 |
| 4b | ex | dyn_module | 7.84 | 3.00 | 0.00 | 3.00 | 0.00 |
| 4b | ex | protocol | 10.8 | 8.00 | 0.00 | 8.00 | 0.00 |
| 4c | bt | typedArea:s: | 159 | 30.0 | 28.0 | 30.0 | 28.0 |
| 4c | ex | protocol | 10.6 | 8.00 | 0.00 | 8.00 | 0.00 |
| 4c | ex | square_area | 3.10 | 3.00 | 0.00 | 3.00 | 0.00 |
| 5a | bt | inherited:s: | 194 | 34.1 | 31.0 | 34.1 | 31.0 |
| 5a | ex | delegate | 3.30 | 4.00 | 0.00 | 4.00 | 0.00 |
| 5b | bt | inheritedViaSelf:s: | 441 | 62.2 | 59.0 | 62.2 | 59.0 |
| 5b | ex | delegate_via_self | 3.54 | 5.00 | 0.00 | 5.00 | 0.00 |
| 5c | bt | hasField:s: | 179 | 30.1 | 28.0 | 30.1 | 28.0 |
| 5c | ex | has_key | 13.7 | 1.00 | 0.00 | 1.00 | 0.00 |
| 6a | bt | blockValue:blk: | 2.85 | 3.00 | 0.00 | 3.00 | 0.00 |
| 6a | ex | fun_call | 2.76 | 3.00 | 0.00 | 3.00 | 0.00 |
| 6b | bt | doSum:list: | 2320 | 304 | 404 | 304 | 404 |
| 6b | ex | enum_reduce_sum | 282 | 304 | 0.00 | 304 | 0.00 |
| 6b | ex | lists_foldl | 257 | 303 | 0.00 | 303 | 0.00 |
| 6c | bt | doNoAssign:list: | 336 | 304 | 0.00 | 304 | 0.00 |
| 6c | ex | enum_each | 329 | 304 | 0.00 | 304 | 0.00 |
| 6d | bt | collect:list: | 1335 | 404 | 200 | 404 | 200 |
| 6d | ex | enum_map | 1469 | 405 | 200 | 405 | 200 |
| 6e | bt | inject:list: | 217 | 303 | 0.00 | 303 | 0.00 |
| 6e | ex | enum_reduce_sum | 266 | 304 | 0.00 | 304 | 0.00 |
| 6e | ex | lists_foldl | 253 | 303 | 0.00 | 303 | 0.00 |
| 7a | bt | actorCall:c: | 3380 | 210 | 316 | 135 | 227 |
| 7a | ex | gen_call | 896 | 38.2 | 56.0 | 17.2 | 33.0 |
| 7b | bt | actorUpdate:c: | 3231 | 210 | 322 | 135 | 227 |
| 7b | ex | gen_update | 905 | 38.2 | 56.0 | 17.2 | 33.0 |
| 7c | bt | actorCast:c: | 2285 | 193 | 246 | 121 | 181 |
| 7c | ex | gen_cast | 136 | 19.5 | 12.0 | 7.02 | 3.00 |
| 7d | bt | actorProbe:c: | 21551 | 820 | 2760 | 135 | 227 |
| 7d | ex | gen_probe | 908 | 38.2 | 56.0 | 17.2 | 33.0 |
| 8a | bt | onDoNoRaise: | 233 | 22.1 | 21.0 | 22.1 | 21.0 |
| 8a | ex | try_no_raise | 4.41 | 1.00 | 0.00 | 1.00 | 0.00 |
| 8b | bt | onDoRaise: | 66769 | 2712 | 22267 | 2166 | 20824 |
| 8b | ex | try_raise | 176 | 14.1 | 36.0 | 14.1 | 36.0 |
| 9a | bt | resultOk: | 440 | 69.2 | 76.0 | 69.2 | 76.0 |
| 9a | ex | ok_match | 20.1 | 3.01 | 3.00 | 3.01 | 3.00 |
| 9b | bt | resultIsOk:r: | 178 | 26.1 | 27.0 | 26.1 | 27.0 |
| 9b | ex | ok_is_ok | 2.01 | 1.00 | 0.00 | 1.00 | 0.00 |
| 10a | bt | dictPut:d: | 90.3 | 19.0 | 10.0 | 19.0 | 10.0 |
| 10a | ex | map_put | 40.6 | 2.03 | 6.00 | 2.03 | 6.00 |
| 10b | bt | dictAt:d: | 21.6 | 18.0 | 0.00 | 18.0 | 0.00 |
| 10b | ex | map_get | 3.56 | 4.00 | 0.00 | 4.00 | 0.00 |
| 10c | bt | listAdd:list: | 338 | 18.7 | 202 | 18.7 | 202 |
| 10c | ex | list_append | 313 | 5.69 | 200 | 5.69 | 200 |
| 10d | bt | listIncludes:list: | 41.4 | 20.0 | 0.00 | 20.0 | 0.00 |
| 10d | ex | list_member | 31.4 | 9.00 | 0.00 | 9.00 | 0.00 |
| 10e | bt | stringConcat:s: | 82.7 | 10.1 | 28.0 | 10.1 | 28.0 |
| 10e | ex | binary_concat | 92.9 | 1.04 | 8.00 | 1.04 | 8.00 |
| 11a | bt | sumToDo: | 150 | 103 | 7.00 | 103 | 7.00 |
| 11a | ex | sum_recursive | 61.1 | 103 | 0.00 | 103 | 0.00 |
| 11a | ex | sum_reduce_range | 192 | 253 | 0.00 | 253 | 0.00 |
| 11b | bt | add:a: | 2.05 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11b | ex | add | 0.98 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11c | bt | addUntyped:a: | 3.15 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11c | ex | add | 2.05 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11d | bt | ifTrueIfFalse:a: | 38.8 | 15.0 | 4.00 | 15.0 | 4.00 |
| 11d | ex | if_else | 2.07 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12a | bt | classOf:p: | 423 | 26.2 | 44.0 | 26.2 | 44.0 |
| 12a | ex | struct_of | 3.10 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12b | bt | isKindOf:p: | 864 | 119 | 194 | 119 | 194 |
| 12b | ex | is_struct_check | 12.6 | 1.00 | 0.00 | 1.00 | 0.00 |
