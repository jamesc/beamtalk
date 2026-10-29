# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 05:38:11 up 4 min,  0 user,  load average: 1.44, 0.77, 0.32
# rounds 7
# load average after: 05:38:42 up 4 min,  0 user,  load average: 1.40, 0.83, 0.35

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 7a | actor sync read: c count / GenServer.call | 12401 | 226 | 315 | 2335 | 37.1 | 56.0 | 5.3 | 6.1 | 5.6 | 189 | 259 | send(C, count, []) -> beamtalk_actor:sync_send, gen_server:call + telemetry/trace/ctx hooks both sides |
| 7b | actor state update: c increment / GenServer.call | 13616 | 226 | 321 | 2686 | 37.1 | 56.0 | 5.1 | 6.1 | 5.7 | 189 | 265 | as 7a |
| 7c | actor cast: c increment! / GenServer.cast | 8794 | 200 | 240 | 486 | 18.7 | 12.0 | 18.1 | 10.7 | 20.0 | 181 | 228 | beamtalk_message_dispatch:cast -> gen_server:cast + the same hooks |
| 7d | actor method doing self hasField: / GenServer.call | 13274 | 282 | 392 | 2240 | 37.1 | 56.0 | 5.9 | 7.6 | 7.0 | 245 | 336 | as 7a; in the actor `self hasField:` misses the module and walks the runtime method resolver |
| 8a | on: Error do: without a raise / try-rescue | 1106 | 21.1 | 21.0 | 2.48 | 0.00 | 0.00 | 446.1 | - | - | 21.1 | 21.0 | `Error` class ref built per entry (whereis_class + module_name gen_server:call), class_vars snapshot, try |
| 8b | on: Error do: with a raise / raise-rescue | 4964 | 123 | 171 | 609 | 13.1 | 36.0 | 8.2 | 9.4 | 4.7 | 109 | 135 | as 8a + `Error signal:` via the Error class process, which logs an error, rewraps, rebuilds a stack |
| 12a | p class / p.__struct__ | 447 | 15.0 | 23.0 | 2.59 | 0.00 | 0.00 | 172.3 | - | - | 15.0 | 23.0 | beamtalk_primitive:class_of_object: derived-cache hit (counter + ETS read + pid liveness) |
| 12b | p isKindOf: VbPoint / is_struct(p, Point) | 2676 | 52.2 | 67.0 | 3.40 | 0.00 | 0.00 | 787.1 | - | - | 52.2 | 67.0 | class ref built per call (whereis_class + module_name_safe) + send(P, 'isKindOf:') -> objectIsKindOf/2: one memoised {is_kind_of, C, B} lookup (BT-4221) |
| 12c | s isKindOf: String / is_binary(s) | 3364 | 71.2 | 70.0 | 3.82 | 0.00 | 0.00 | 880.1 | - | - | 71.2 | 70.0 | as 12b, but the send walks String/Binary/Collection/Value/Object dispatch (an extensions lookup per level) to Object>>isKindOf: |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 7a | `gen_call` | 2335 | 37.1 | 56.0 |
| 7b | `gen_update` | 2686 | 37.1 | 56.0 |
| 7c | `gen_cast` | 486 | 18.7 | 12.0 |
| 7d | `gen_probe` | 2240 | 37.1 | 56.0 |
| 8a | `try_no_raise` | 2.48 | 0.00 | 0.00 |
| 8b | `try_raise` | 609 | 13.1 | 36.0 |
| 12a | `struct_of` | 2.59 | 0.00 | 0.00 |
| 12b | `is_struct_check` | 3.40 | 0.00 | 0.00 |
| 12c | `is_binary_check` | 3.82 | 0.00 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 2.54 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 2.85 | 1.00 | 0.00 | 1.00 | 0.00 |
| 7a | bt | actorCall:c: | 12404 | 227 | 315 | 143 | 224 |
| 7a | ex | gen_call | 2338 | 38.1 | 56.0 | 17.1 | 33.0 |
| 7b | bt | actorUpdate:c: | 13619 | 227 | 321 | 143 | 224 |
| 7b | ex | gen_update | 2689 | 38.1 | 56.0 | 17.1 | 33.0 |
| 7c | bt | actorCast:c: | 8797 | 201 | 240 | 125 | 176 |
| 7c | ex | gen_cast | 488 | 19.7 | 12.0 | 7.02 | 3.00 |
| 7d | bt | actorProbe:c: | 13276 | 283 | 392 | 144 | 224 |
| 7d | ex | gen_probe | 2243 | 38.1 | 56.0 | 17.1 | 33.0 |
| 8a | bt | onDoNoRaise: | 1109 | 22.1 | 21.0 | 22.1 | 21.0 |
| 8a | ex | try_no_raise | 5.33 | 1.00 | 0.00 | 1.00 | 0.00 |
| 8b | bt | onDoRaise: | 4967 | 124 | 171 | 124 | 171 |
| 8b | ex | try_raise | 611 | 14.1 | 36.0 | 14.1 | 36.0 |
| 12a | bt | classOf:p: | 449 | 16.0 | 23.0 | 16.0 | 23.0 |
| 12a | ex | struct_of | 5.44 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12b | bt | isKindOf:p: | 2678 | 53.2 | 67.0 | 53.2 | 67.0 |
| 12b | ex | is_struct_check | 6.25 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12c | bt | isKindOfBuiltin:s: | 3366 | 72.2 | 70.0 | 72.2 | 70.0 |
| 12c | ex | is_binary_check | 6.67 | 1.00 | 0.00 | 1.00 | 0.00 |
