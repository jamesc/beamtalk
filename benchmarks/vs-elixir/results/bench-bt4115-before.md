# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 05:35:55 up 2 min,  0 user,  load average: 1.02, 0.34, 0.12
# rounds 7
# load average after: 05:36:53 up 3 min,  0 user,  load average: 1.28, 0.54, 0.21

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 7a | actor sync read: c count / GenServer.call | 10459 | 226 | 311 | 2564 | 37.1 | 56.0 | 4.1 | 6.1 | 5.6 | 189 | 255 | send(C, count, []) -> beamtalk_actor:sync_send, gen_server:call + telemetry/trace/ctx hooks both sides |
| 7b | actor state update: c increment / GenServer.call | 12710 | 226 | 317 | 2535 | 37.1 | 56.0 | 5.0 | 6.1 | 5.7 | 189 | 261 | as 7a |
| 7c | actor cast: c increment! / GenServer.cast | 8466 | 199 | 239 | 450 | 18.6 | 12.0 | 18.8 | 10.7 | 19.9 | 180 | 227 | beamtalk_message_dispatch:cast -> gen_server:cast + the same hooks |
| 7d | actor method doing self hasField: / GenServer.call | 14898 | 283 | 388 | 2509 | 37.1 | 56.0 | 5.9 | 7.6 | 6.9 | 245 | 332 | as 7a; in the actor `self hasField:` misses the module and walks the runtime method resolver |
| 8a | on: Error do: without a raise / try-rescue | 1052 | 21.1 | 21.0 | 3.27 | 0.00 | 0.00 | 322.0 | - | - | 21.1 | 21.0 | `Error` class ref built per entry (whereis_class + module_name gen_server:call), class_vars snapshot, try |
| 8b | on: Error do: with a raise / raise-rescue | 182558 | 2425 | 22970 | 775 | 13.1 | 36.0 | 235.7 | 184.9 | 638.0 | 2411 | 22934 | as 8a + `Error signal:` via the Error class process, which logs an error, rewraps, rebuilds a stack |
| 12a | p class / p.__struct__ | 412 | 15.0 | 23.0 | 3.20 | 0.00 | 0.00 | 128.8 | - | - | 15.0 | 23.0 | beamtalk_primitive:class_of_object: derived-cache hit (counter + ETS read + pid liveness) |
| 12b | p isKindOf: VbPoint / is_struct(p, Point) | 2185 | 74.1 | 108 | 2.82 | 0.00 | 0.00 | 775.6 | - | - | 74.1 | 108 | class ref built per call (whereis_class + module_name_safe) + send(P, 'isKindOf:') -> objectIsKindOf/2: one memoised {is_kind_of, C, B} lookup (BT-4221) |
| 12c | s isKindOf: String / is_binary(s) | 2953 | 93.1 | 111 | 3.51 | 0.00 | 0.00 | 841.7 | - | - | 93.1 | 111 | as 12b, but the send walks String/Binary/Collection/Value/Object dispatch (an extensions lookup per level) to Object>>isKindOf: |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 7a | `gen_call` | 2564 | 37.1 | 56.0 |
| 7b | `gen_update` | 2535 | 37.1 | 56.0 |
| 7c | `gen_cast` | 450 | 18.6 | 12.0 |
| 7d | `gen_probe` | 2509 | 37.1 | 56.0 |
| 8a | `try_no_raise` | 3.27 | 0.00 | 0.00 |
| 8b | `try_raise` | 775 | 13.1 | 36.0 |
| 12a | `struct_of` | 3.20 | 0.00 | 0.00 |
| 12b | `is_struct_check` | 2.82 | 0.00 | 0.00 |
| 12c | `is_binary_check` | 3.51 | 0.00 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 3.07 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 2.78 | 1.00 | 0.00 | 1.00 | 0.00 |
| 7a | bt | actorCall:c: | 10462 | 227 | 311 | 144 | 224 |
| 7a | ex | gen_call | 2567 | 38.1 | 56.0 | 17.1 | 33.0 |
| 7b | bt | actorUpdate:c: | 12713 | 227 | 317 | 143 | 224 |
| 7b | ex | gen_update | 2538 | 38.1 | 56.0 | 17.1 | 33.0 |
| 7c | bt | actorCast:c: | 8469 | 200 | 239 | 124 | 175 |
| 7c | ex | gen_cast | 452 | 19.6 | 12.0 | 7.02 | 3.00 |
| 7d | bt | actorProbe:c: | 14901 | 284 | 388 | 144 | 224 |
| 7d | ex | gen_probe | 2512 | 38.1 | 56.0 | 17.1 | 33.0 |
| 8a | bt | onDoNoRaise: | 1055 | 22.1 | 21.0 | 22.1 | 21.0 |
| 8a | ex | try_no_raise | 6.05 | 1.00 | 0.00 | 1.00 | 0.00 |
| 8b | bt | onDoRaise: | 182561 | 2426 | 22970 | 1890 | 21510 |
| 8b | ex | try_raise | 777 | 14.1 | 36.0 | 14.1 | 36.0 |
| 12a | bt | classOf:p: | 415 | 16.0 | 23.0 | 16.0 | 23.0 |
| 12a | ex | struct_of | 5.98 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12b | bt | isKindOf:p: | 2188 | 75.1 | 108 | 75.1 | 108 |
| 12b | ex | is_struct_check | 5.60 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12c | bt | isKindOfBuiltin:s: | 2956 | 94.1 | 111 | 94.1 | 111 |
| 12c | ex | is_binary_check | 6.29 | 1.00 | 0.00 | 1.00 | 0.00 |
