# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 18
# load average: 22:07  up 24 days,  6:57, 4 users, load averages: 26.65 52.32 40.13
# rounds 7
# load average after: 22:08  up 24 days,  6:57, 4 users, load averages: 19.69 46.68 38.67

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 0b | timesRepeat: whose block assigns nothing | 90.2 | 26.0 | 10.0 | 0.03 | 0.00 | 0.00 | - | - | - | 26.0 | 10.0 | send(N, 'timesRepeat:', [fun]); Integer>>timesRepeat: dispatches ifTrue:ifFalse: per step |
| 1a | getter on a sealed Value: p x / p.x | 162 | 29.0 | 28.0 | 1.47 | 0.00 | 0.00 | 109.7 | - | - | 29.0 | 28.0 | send(P, x, []) -> classify, tagged-map class lookup (ETS + cache), Mod:dispatch/3 |
| 1b | getter on an open Value: p x / p.x | 161 | 29.0 | 28.0 | 1.50 | 0.00 | 0.00 | 107.1 | - | - | 29.0 | 28.0 | send(P, x, []), same path as 1a |
| 1c | method on a sealed Value: p sum / Point.sum(p) | 171 | 29.0 | 28.0 | 1.31 | 2.00 | 0.00 | 130.7 | 14.5 | - | 27.0 | 28.0 | send(P, sum, []), same path as 1a |
| 2a | functional update: p withX: 3 / %{p \| x: 3} | 333 | 30.2 | 34.0 | 32.1 | 0.00 | 6.00 | 10.4 | - | 5.7 | 30.2 | 28.0 | send(P, 'withX:', [3]), same path as 1a + maps:put |
| 2b | construct: VbPoint x: a y: 4 / %Point{x: a, y: 4} | 2926 | 148 | 494 | 31.5 | 0.03 | 6.00 | 92.8 | - | 82.4 | 148 | 488 | whereis_class + class_send: gen_server:call into the VbPoint class process |
| 3a | `class sealed` method, sealed class / Mod.fun(arg) | 1.02 | 2.00 | 0.00 | 1.19 | 2.00 | 0.00 | 0.9 | 1.0 | - | 0.00 | 0.00 | direct call Mod:'class_double:'(nil, 21) |
| 3b | class method, sealed class / Mod.fun(arg) | 2621 | 146 | 452 | 2.77 | 2.00 | 0.00 | 945.9 | 73.1 | - | 144 | 452 | whereis_class + class_send: not `class sealed`, so gen_server:call into the class process |
| 3c | class method, open class / Mod.fun(arg) | 2861 | 146 | 492 | 2.05 | 2.00 | 0.00 | 1393.3 | 73.2 | - | 144 | 492 | whereis_class + class_send: gen_server:call into the class process |
| 4a | untyped receiver: o x / o.x | 170 | 29.0 | 28.0 | 1.26 | 0.00 | 0.00 | 134.6 | - | - | 29.0 | 28.0 | send(O, x, []), same path as 1a |
| 4b | untyped receiver: o area / protocol | 172 | 29.0 | 28.0 | 8.29 | 7.00 | 0.00 | 20.7 | 4.1 | - | 22.0 | 28.0 | send(O, area, []), same path as 1a |
| 4c | typed open Value: s area / Square.area(s) | 165 | 29.0 | 28.0 | 1.41 | 2.00 | 0.00 | 116.8 | 14.5 | - | 27.0 | 28.0 | send(S, area, []), same path as 1a |
| 5a | inherited method: s base / defdelegate | 182 | 33.1 | 31.0 | 1.56 | 3.00 | 0.00 | 116.5 | 11.0 | - | 30.1 | 31.0 | send(S, base, []): VbSub:dispatch/3 misses, extensions lookup, VbBase:dispatch/3 |
| 5b | self-send to inherited: s viaSelf / local call | 459 | 61.2 | 59.0 | 1.61 | 4.00 | 0.00 | 285.2 | 15.3 | - | 57.2 | 59.0 | send(S, viaSelf, []), then `self base` is a second full send |
| 5c | runtime-resolved method: s hasField: #v / Map.has_key? | 170 | 29.1 | 28.0 | 12.6 | 0.00 | 0.00 | 13.4 | - | - | 29.0 | 28.0 | send(S, 'hasField:', [v]) -> beamtalk_reflection:has_field/2 |
| 6a | block value: x / fun.(x) | 0.99 | 2.00 | 0.00 | 0.46 | 2.00 | 0.00 | - | 1.0 | - | 0.00 | 0.00 | is_function(Blk, 1) guard, then apply Blk(41) |
| 6b | do: summing 100 elements / Enum.reduce | 2416 | 303 | 404 | 274 | 303 | 0.00 | 8.8 | 1.0 | - | 0.07 | 404 | lists:foldl over a fun that maps:get/maps:put `sum` in a state map; + with coercion try |
| 6c | do: over 100, no assignment / Enum.each | 333 | 303 | 0.00 | 330 | 303 | 0.00 | 1.0 | 1.0 | - | 0.00 | 0.00 | is_list guard, lists:foreach(fun) |
| 6d | collect: 100 elements / Enum.map | 1665 | 403 | 200 | 1706 | 404 | 200 | 1.0 | 1.0 | 1.0 | -1.00 | 0.00 | is_list guard, lists:map(fun); * with is_number guard |
| 6e | inject:into: 100 elements / Enum.reduce | 227 | 302 | 0.00 | 245 | 303 | 0.00 | 0.9 | 1.0 | - | -1.00 | 0.00 | is_list guard, lists:foldl(fun); + with coercion try |
| 7a | actor sync read: c count / GenServer.call | 3622 | 209 | 310 | 914 | 37.2 | 56.0 | 4.0 | 5.6 | 5.5 | 172 | 254 | send(C, count, []) -> beamtalk_actor:sync_send, gen_server:call + telemetry/trace/ctx hooks both sides |
| 7b | actor state update: c increment / GenServer.call | 3859 | 210 | 316 | 921 | 37.2 | 56.0 | 4.2 | 5.6 | 5.6 | 172 | 260 | as 7a |
| 7c | actor cast: c increment! / GenServer.cast | 2315 | 192 | 242 | 142 | 18.5 | 12.0 | 16.3 | 10.4 | 20.2 | 173 | 230 | beamtalk_message_dispatch:cast -> gen_server:cast + the same hooks |
| 7d | actor method doing self hasField: / GenServer.call | 4039 | 265 | 430 | 919 | 37.2 | 56.0 | 4.4 | 7.1 | 7.7 | 227 | 374 | as 7a; in the actor `self hasField:` misses the module and walks the runtime method resolver |
| 8a | on: Error do: without a raise / try-rescue | 1066 | 48.3 | 101 | 2.37 | 0.00 | 0.00 | 448.9 | - | - | 48.3 | 101 | `Error` class ref built per entry (whereis_class + module_name gen_server:call), class_vars snapshot, try |
| 8b | on: Error do: with a raise / raise-rescue | 63479 | 2705 | 22426 | 171 | 13.1 | 36.0 | 371.8 | 206.5 | 622.9 | 2692 | 22390 | as 8a + `Error signal:` via the Error class process, which logs an error, rewraps, rebuilds a stack |
| 9a | (Result ok: x) value / {:ok, x} match | 3566 | 218 | 772 | 19.4 | 2.01 | 3.00 | 183.4 | 108.1 | 257.3 | 216 | 769 | class_send(Result, 'ok:') gen_server:call; `value` as a block-or-send case |
| 9b | r isOk / match?({:ok, _}, r) | 190 | 25.1 | 27.0 | 0.23 | 0.00 | 0.00 | - | - | - | 25.1 | 27.0 | send(R, isOk, []), same path as 1a |
| 10a | Dictionary at:put: / Map.put | 91.1 | 18.0 | 10.0 | 39.8 | 1.03 | 6.00 | 2.3 | 17.6 | 1.7 | 17.0 | 4.00 | send(D, 'at:put:', ...) -> Dictionary:dispatch/3 -> maps:put |
| 10b | Dictionary at: / Map.get | 19.8 | 17.0 | 0.00 | 1.59 | 3.00 | 0.00 | 12.5 | 5.7 | - | 14.0 | 0.00 | send(D, 'at:', ...) -> Dictionary:dispatch/3 -> maps:get |
| 10c | List add: (100 elements) / l ++ [x] | 351 | 17.7 | 202 | 311 | 4.70 | 200 | 1.1 | 3.8 | 1.0 | 13.0 | 2.00 | send(L, 'add:', [7]) -> List primitive (++) |
| 10d | List includes: (100 elements) / Enum.member? | 39.7 | 19.0 | 0.00 | 28.2 | 8.00 | 0.00 | 1.4 | 2.4 | - | 11.0 | 0.00 | send(L, 'includes:', [50]) -> List primitive (lists:member) |
| 10e | String ++ / <> | 86.2 | 9.06 | 28.0 | 92.4 | 0.04 | 8.00 | 0.9 | - | 3.5 | 9.03 | 20.0 | is_list guard, iolist_to_binary([binary_to_list(S), binary_to_list("!")]) |
| 11a | 1 to: 100 do: summing / recursive sum | 161 | 102 | 7.00 | 58.4 | 102 | 0.00 | 2.8 | 1.0 | - | 0.03 | 7.00 | letrec loop; + with is_number guard and coercion try |
| 11b | a + 1, a :: Integer / a + 1 | 0.07 | 0.00 | 0.00 | 0.09 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | erlang:'+'(A, 1) |
| 11c | a + 1, a untyped / a + 1 | 1.36 | 0.00 | 0.00 | 0.09 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | is_number guard, erlang:'+' |
| 11d | a > 0 ifTrue: [1] ifFalse: [2] / if | 38.4 | 14.0 | 4.00 | 0.33 | 0.00 | 0.00 | - | - | - | 14.0 | 4.00 | send(Bool, 'ifTrue:ifFalse:', [fun, fun]) -> True:dispatch/3 |
| 12a | p class / p.__struct__ | 308 | 26.2 | 83.0 | 1.60 | 0.00 | 0.00 | 192.8 | - | - | 26.2 | 83.0 | beamtalk_primitive:class_of_object: ETS lookups + list_to_atom per call |
| 12b | p isKindOf: VbPoint / is_struct(p, Point) | 6656 | 412 | 1368 | 8.67 | 0.00 | 0.00 | 767.6 | - | - | 412 | 1368 | class ref built per call (gen_server:call) + send(P, 'isKindOf:') -> includesBehaviour: via class processes |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 0b | `baseline` | 0.03 | 0.00 | 0.00 |
| 1a | `getter_dot` | 1.47 | 0.00 | 0.00 |
| 1a | `getter_fun` | 1.16 | 2.00 | 0.00 |
| 1a | `getter_match` | 1.00 | 0.00 | 0.00 |
| 1b | `getter_dot` | 1.50 | 0.00 | 0.00 |
| 1c | `point_sum` | 1.31 | 2.00 | 0.00 |
| 2a | `with_x` | 32.1 | 0.00 | 6.00 |
| 2b | `construct` | 31.5 | 0.03 | 6.00 |
| 3a | `remote_call` | 1.19 | 2.00 | 0.00 |
| 3b | `remote_call` | 2.77 | 2.00 | 0.00 |
| 3c | `remote_call` | 2.05 | 2.00 | 0.00 |
| 4a | `getter_dot` | 1.26 | 0.00 | 0.00 |
| 4b | `protocol` | 8.29 | 7.00 | 0.00 |
| 4b | `apply3` | 5.98 | 2.00 | 0.00 |
| 4b | `dyn_module` | 6.18 | 2.00 | 0.00 |
| 4c | `square_area` | 1.41 | 2.00 | 0.00 |
| 4c | `protocol` | 8.64 | 7.00 | 0.00 |
| 5a | `delegate` | 1.56 | 3.00 | 0.00 |
| 5b | `delegate_via_self` | 1.61 | 4.00 | 0.00 |
| 5c | `has_key` | 12.6 | 0.00 | 0.00 |
| 6a | `fun_call` | 0.46 | 2.00 | 0.00 |
| 6b | `enum_reduce_sum` | 274 | 303 | 0.00 |
| 6b | `lists_foldl` | 293 | 302 | 0.00 |
| 6c | `enum_each` | 330 | 303 | 0.00 |
| 6d | `enum_map` | 1706 | 404 | 200 |
| 6e | `enum_reduce_sum` | 245 | 303 | 0.00 |
| 6e | `lists_foldl` | 207 | 302 | 0.00 |
| 7a | `gen_call` | 914 | 37.2 | 56.0 |
| 7b | `gen_update` | 921 | 37.2 | 56.0 |
| 7c | `gen_cast` | 142 | 18.5 | 12.0 |
| 7d | `gen_probe` | 919 | 37.2 | 56.0 |
| 8a | `try_no_raise` | 2.37 | 0.00 | 0.00 |
| 8b | `try_raise` | 171 | 13.1 | 36.0 |
| 9a | `ok_match` | 19.4 | 2.01 | 3.00 |
| 9b | `ok_is_ok` | 0.23 | 0.00 | 0.00 |
| 10a | `map_put` | 39.8 | 1.03 | 6.00 |
| 10b | `map_get` | 1.59 | 3.00 | 0.00 |
| 10c | `list_append` | 311 | 4.70 | 200 |
| 10d | `list_member` | 28.2 | 8.00 | 0.00 |
| 10e | `binary_concat` | 92.4 | 0.04 | 8.00 |
| 11a | `sum_recursive` | 58.4 | 102 | 0.00 |
| 11a | `sum_reduce_range` | 191 | 252 | 0.00 |
| 11b | `add` | 0.09 | 0.00 | 0.00 |
| 11c | `add` | 0.09 | 0.00 | 0.00 |
| 11d | `if_else` | 0.33 | 0.00 | 0.00 |
| 12a | `struct_of` | 1.60 | 0.00 | 0.00 |
| 12b | `is_struct_check` | 8.67 | 0.00 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.77 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.75 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0b | bt | loopNoAssign: | 91.9 | 27.0 | 10.0 | 27.0 | 10.0 |
| 0b | ex | baseline | 1.78 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1a | bt | getterSealed:p: | 164 | 30.0 | 28.0 | 30.0 | 28.0 |
| 1a | ex | getter_dot | 3.22 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1a | ex | getter_fun | 2.91 | 3.00 | 0.00 | 3.00 | 0.00 |
| 1a | ex | getter_match | 2.75 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1b | bt | getterOpen:p: | 163 | 30.0 | 28.0 | 30.0 | 28.0 |
| 1b | ex | getter_dot | 3.25 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1c | bt | sumSealed:p: | 173 | 30.0 | 28.0 | 30.0 | 28.0 |
| 1c | ex | point_sum | 3.06 | 3.00 | 0.00 | 3.00 | 0.00 |
| 2a | bt | withX:p: | 334 | 31.2 | 34.0 | 31.2 | 34.0 |
| 2a | ex | with_x | 33.9 | 1.00 | 6.00 | 1.00 | 6.00 |
| 2b | bt | construct:a: | 2927 | 149 | 494 | 46.1 | 238 |
| 2b | ex | construct | 33.3 | 1.03 | 6.00 | 1.03 | 6.00 |
| 3a | bt | classSealedMethod: | 2.79 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3a | ex | remote_call | 2.94 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3b | bt | sealedClassMethod: | 2623 | 147 | 452 | 45.6 | 214 |
| 3b | ex | remote_call | 4.52 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3c | bt | openClassMethod: | 2863 | 147 | 492 | 45.9 | 222 |
| 3c | ex | remote_call | 3.80 | 3.00 | 0.00 | 3.00 | 0.00 |
| 4a | bt | dynGetter:o: | 172 | 30.0 | 28.0 | 30.0 | 28.0 |
| 4a | ex | getter_dot | 3.01 | 1.00 | 0.00 | 1.00 | 0.00 |
| 4b | bt | dynArea:o: | 174 | 30.0 | 28.0 | 30.0 | 28.0 |
| 4b | ex | apply3 | 7.73 | 3.00 | 0.00 | 3.00 | 0.00 |
| 4b | ex | dyn_module | 7.93 | 3.00 | 0.00 | 3.00 | 0.00 |
| 4b | ex | protocol | 10.0 | 8.00 | 0.00 | 8.00 | 0.00 |
| 4c | bt | typedArea:s: | 167 | 30.0 | 28.0 | 30.0 | 28.0 |
| 4c | ex | protocol | 10.4 | 8.00 | 0.00 | 8.00 | 0.00 |
| 4c | ex | square_area | 3.16 | 3.00 | 0.00 | 3.00 | 0.00 |
| 5a | bt | inherited:s: | 184 | 34.1 | 31.0 | 34.1 | 31.0 |
| 5a | ex | delegate | 3.31 | 4.00 | 0.00 | 4.00 | 0.00 |
| 5b | bt | inheritedViaSelf:s: | 461 | 62.2 | 59.0 | 62.2 | 59.0 |
| 5b | ex | delegate_via_self | 3.36 | 5.00 | 0.00 | 5.00 | 0.00 |
| 5c | bt | hasField:s: | 171 | 30.1 | 28.0 | 30.1 | 28.0 |
| 5c | ex | has_key | 14.4 | 1.00 | 0.00 | 1.00 | 0.00 |
| 6a | bt | blockValue:blk: | 2.77 | 3.00 | 0.00 | 3.00 | 0.00 |
| 6a | ex | fun_call | 2.21 | 3.00 | 0.00 | 3.00 | 0.00 |
| 6b | bt | doSum:list: | 2418 | 304 | 404 | 304 | 404 |
| 6b | ex | enum_reduce_sum | 276 | 304 | 0.00 | 304 | 0.00 |
| 6b | ex | lists_foldl | 295 | 303 | 0.00 | 303 | 0.00 |
| 6c | bt | doNoAssign:list: | 335 | 304 | 0.00 | 304 | 0.00 |
| 6c | ex | enum_each | 332 | 304 | 0.00 | 304 | 0.00 |
| 6d | bt | collect:list: | 1667 | 404 | 200 | 404 | 200 |
| 6d | ex | enum_map | 1708 | 405 | 200 | 405 | 200 |
| 6e | bt | inject:list: | 229 | 303 | 0.00 | 303 | 0.00 |
| 6e | ex | enum_reduce_sum | 246 | 304 | 0.00 | 304 | 0.00 |
| 6e | ex | lists_foldl | 209 | 303 | 0.00 | 303 | 0.00 |
| 7a | bt | actorCall:c: | 3624 | 210 | 310 | 135 | 227 |
| 7a | ex | gen_call | 916 | 38.2 | 56.0 | 17.2 | 33.0 |
| 7b | bt | actorUpdate:c: | 3861 | 211 | 316 | 135 | 227 |
| 7b | ex | gen_update | 923 | 38.2 | 56.0 | 17.2 | 33.0 |
| 7c | bt | actorCast:c: | 2317 | 193 | 242 | 121 | 181 |
| 7c | ex | gen_cast | 144 | 19.5 | 12.0 | 7.02 | 3.00 |
| 7d | bt | actorProbe:c: | 4041 | 266 | 430 | 135 | 227 |
| 7d | ex | gen_probe | 920 | 38.2 | 56.0 | 17.2 | 33.0 |
| 8a | bt | onDoNoRaise: | 1068 | 49.3 | 101 | 28.3 | 78.0 |
| 8a | ex | try_no_raise | 4.12 | 1.00 | 0.00 | 1.00 | 0.00 |
| 8b | bt | onDoRaise: | 63481 | 2706 | 22426 | 2140 | 20964 |
| 8b | ex | try_raise | 173 | 14.1 | 36.0 | 14.1 | 36.0 |
| 9a | bt | resultOk: | 3568 | 219 | 772 | 88.3 | 485 |
| 9a | ex | ok_match | 21.2 | 3.01 | 3.00 | 3.01 | 3.00 |
| 9b | bt | resultIsOk:r: | 192 | 26.1 | 27.0 | 26.1 | 27.0 |
| 9b | ex | ok_is_ok | 1.98 | 1.00 | 0.00 | 1.00 | 0.00 |
| 10a | bt | dictPut:d: | 92.9 | 19.0 | 10.0 | 19.0 | 10.0 |
| 10a | ex | map_put | 41.5 | 2.03 | 6.00 | 2.03 | 6.00 |
| 10b | bt | dictAt:d: | 21.6 | 18.0 | 0.00 | 18.0 | 0.00 |
| 10b | ex | map_get | 3.33 | 4.00 | 0.00 | 4.00 | 0.00 |
| 10c | bt | listAdd:list: | 352 | 18.7 | 202 | 18.7 | 202 |
| 10c | ex | list_append | 313 | 5.70 | 200 | 5.70 | 200 |
| 10d | bt | listIncludes:list: | 41.5 | 20.0 | 0.00 | 20.0 | 0.00 |
| 10d | ex | list_member | 29.9 | 9.00 | 0.00 | 9.00 | 0.00 |
| 10e | bt | stringConcat:s: | 87.9 | 10.1 | 28.0 | 10.1 | 28.0 |
| 10e | ex | binary_concat | 94.2 | 1.04 | 8.00 | 1.04 | 8.00 |
| 11a | bt | sumToDo: | 163 | 103 | 7.00 | 103 | 7.00 |
| 11a | ex | sum_recursive | 60.1 | 103 | 0.00 | 103 | 0.00 |
| 11a | ex | sum_reduce_range | 193 | 253 | 0.00 | 253 | 0.00 |
| 11b | bt | add:a: | 1.84 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11b | ex | add | 1.84 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11c | bt | addUntyped:a: | 3.14 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11c | ex | add | 1.84 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11d | bt | ifTrueIfFalse:a: | 40.2 | 15.0 | 4.00 | 15.0 | 4.00 |
| 11d | ex | if_else | 2.08 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12a | bt | classOf:p: | 310 | 27.2 | 83.0 | 27.2 | 83.0 |
| 12a | ex | struct_of | 3.35 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12b | bt | isKindOf:p: | 6658 | 413 | 1368 | 274 | 954 |
| 12b | ex | is_struct_check | 10.4 | 1.00 | 0.00 | 1.00 | 0.00 |
