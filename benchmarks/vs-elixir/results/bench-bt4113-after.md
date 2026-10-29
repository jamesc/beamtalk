# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 05:38:49 up 5 min,  0 user,  load average: 1.26, 0.81, 0.35
# rounds 7
# load average after: 05:39:47 up 6 min,  0 user,  load average: 1.44, 0.93, 0.42

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 7a | actor sync read: c count / GenServer.call | 5324 | 140 | 149 | 2227 | 37.1 | 56.0 | 2.4 | 3.8 | 2.7 | 103 | 93.0 | send(C, count, []) -> beamtalk_actor:sync_send, gen_server:call + telemetry/trace/ctx hooks both sides |
| 7b | actor state update: c increment / GenServer.call | 4080 | 140 | 155 | 2664 | 37.1 | 56.0 | 1.5 | 3.8 | 2.8 | 103 | 99.0 | as 7a |
| 7c | actor cast: c increment! / GenServer.cast | 1828 | 110 | 76.0 | 460 | 18.6 | 12.0 | 4.0 | 5.9 | 6.3 | 91.3 | 64.0 | beamtalk_message_dispatch:cast -> gen_server:cast + the same hooks |
| 7d | actor method doing self hasField: / GenServer.call | 4843 | 196 | 226 | 2208 | 37.1 | 56.0 | 2.2 | 5.3 | 4.0 | 159 | 170 | as 7a; in the actor `self hasField:` misses the module and walks the runtime method resolver |
| 8a | on: Error do: without a raise / try-rescue | 1159 | 21.1 | 21.0 | 2.97 | 0.00 | 0.00 | 390.2 | - | - | 21.1 | 21.0 | `Error` class ref built per entry (whereis_class + module_name gen_server:call), class_vars snapshot, try |
| 8b | on: Error do: with a raise / raise-rescue | 260542 | 2473 | 22972 | 592 | 13.1 | 36.0 | 439.9 | 188.7 | 638.1 | 2460 | 22936 | as 8a + `Error signal:` via the Error class process, which logs an error, rewraps, rebuilds a stack |
| 12a | p class / p.__struct__ | 434 | 15.0 | 23.0 | 2.89 | 0.00 | 0.00 | 150.5 | - | - | 15.0 | 23.0 | beamtalk_primitive:class_of_object: derived-cache hit (counter + ETS read + pid liveness) |
| 12b | p isKindOf: VbPoint / is_struct(p, Point) | 2499 | 74.1 | 108 | 2.48 | 0.00 | 0.00 | 1006.7 | - | - | 74.1 | 108 | class ref built per call (whereis_class + module_name_safe) + send(P, 'isKindOf:') -> objectIsKindOf/2: one memoised {is_kind_of, C, B} lookup (BT-4221) |
| 12c | s isKindOf: String / is_binary(s) | 3256 | 93.1 | 111 | 4.19 | 0.00 | 0.00 | 778.0 | - | - | 93.1 | 111 | as 12b, but the send walks String/Binary/Collection/Value/Object dispatch (an extensions lookup per level) to Object>>isKindOf: |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 7a | `gen_call` | 2227 | 37.1 | 56.0 |
| 7b | `gen_update` | 2664 | 37.1 | 56.0 |
| 7c | `gen_cast` | 460 | 18.6 | 12.0 |
| 7d | `gen_probe` | 2208 | 37.1 | 56.0 |
| 8a | `try_no_raise` | 2.97 | 0.00 | 0.00 |
| 8b | `try_raise` | 592 | 13.1 | 36.0 |
| 12a | `struct_of` | 2.89 | 0.00 | 0.00 |
| 12b | `is_struct_check` | 2.48 | 0.00 | 0.00 |
| 12c | `is_binary_check` | 4.19 | 0.00 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 2.42 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 2.32 | 1.00 | 0.00 | 1.00 | 0.00 |
| 7a | bt | actorCall:c: | 5326 | 141 | 149 | 79.1 | 83.0 |
| 7a | ex | gen_call | 2229 | 38.1 | 56.0 | 17.1 | 33.0 |
| 7b | bt | actorUpdate:c: | 4083 | 141 | 155 | 79.1 | 83.0 |
| 7b | ex | gen_update | 2667 | 38.1 | 56.0 | 17.1 | 33.0 |
| 7c | bt | actorCast:c: | 1830 | 111 | 76.0 | 62.0 | 38.0 |
| 7c | ex | gen_cast | 462 | 19.6 | 12.0 | 7.02 | 3.00 |
| 7d | bt | actorProbe:c: | 4845 | 197 | 226 | 79.1 | 83.0 |
| 7d | ex | gen_probe | 2211 | 38.1 | 56.0 | 17.0 | 33.0 |
| 8a | bt | onDoNoRaise: | 1162 | 22.1 | 21.0 | 22.1 | 21.0 |
| 8a | ex | try_no_raise | 5.29 | 1.00 | 0.00 | 1.00 | 0.00 |
| 8b | bt | onDoRaise: | 260544 | 2474 | 22972 | 1931 | 21514 |
| 8b | ex | try_raise | 595 | 14.1 | 36.0 | 14.1 | 36.0 |
| 12a | bt | classOf:p: | 437 | 16.0 | 23.0 | 16.0 | 23.0 |
| 12a | ex | struct_of | 5.21 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12b | bt | isKindOf:p: | 2502 | 75.1 | 108 | 75.1 | 108 |
| 12b | ex | is_struct_check | 4.80 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12c | bt | isKindOfBuiltin:s: | 3258 | 94.1 | 111 | 94.1 | 111 |
| 12c | ex | is_binary_check | 6.50 | 1.00 | 0.00 | 1.00 | 0.00 |
