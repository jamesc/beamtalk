# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 05:37:00 up 3 min,  0 user,  load average: 1.24, 0.56, 0.22
# rounds 7
# load average after: 05:38:03 up 4 min,  0 user,  load average: 1.52, 0.76, 0.31

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 7a | actor sync read: c count / GenServer.call | 11360 | 226 | 311 | 2239 | 37.1 | 56.0 | 5.1 | 6.1 | 5.5 | 189 | 255 | send(C, count, []) -> beamtalk_actor:sync_send, gen_server:call + telemetry/trace/ctx hooks both sides |
| 7b | actor state update: c increment / GenServer.call | 11511 | 226 | 317 | 2684 | 37.1 | 56.0 | 4.3 | 6.1 | 5.7 | 189 | 261 | as 7a |
| 7c | actor cast: c increment! / GenServer.cast | 7562 | 201 | 241 | 482 | 18.8 | 12.0 | 15.7 | 10.7 | 20.1 | 182 | 229 | beamtalk_message_dispatch:cast -> gen_server:cast + the same hooks |
| 7d | actor method doing self hasField: / GenServer.call | 16562 | 282 | 388 | 2309 | 37.1 | 56.0 | 7.2 | 7.6 | 6.9 | 245 | 332 | as 7a; in the actor `self hasField:` misses the module and walks the runtime method resolver |
| 8a | on: Error do: without a raise / try-rescue | 1179 | 21.1 | 21.0 | 2.99 | 0.00 | 0.00 | 394.6 | - | - | 21.1 | 21.0 | `Error` class ref built per entry (whereis_class + module_name gen_server:call), class_vars snapshot, try |
| 8b | on: Error do: with a raise / raise-rescue | 231130 | 2467 | 22977 | 620 | 13.1 | 36.0 | 373.1 | 188.2 | 638.3 | 2454 | 22941 | as 8a + `Error signal:` via the Error class process, which logs an error, rewraps, rebuilds a stack |
| 12a | p class / p.__struct__ | 404 | 15.0 | 23.0 | 2.88 | 0.00 | 0.00 | 140.2 | - | - | 15.0 | 23.0 | beamtalk_primitive:class_of_object: derived-cache hit (counter + ETS read + pid liveness) |
| 12b | p isKindOf: VbPoint / is_struct(p, Point) | 2265 | 52.2 | 67.0 | 2.12 | 0.00 | 0.00 | 1067.7 | - | - | 52.2 | 67.0 | class ref built per call (whereis_class + module_name_safe) + send(P, 'isKindOf:') -> objectIsKindOf/2: one memoised {is_kind_of, C, B} lookup (BT-4221) |
| 12c | s isKindOf: String / is_binary(s) | 2823 | 71.2 | 70.0 | 3.55 | 0.00 | 0.00 | 794.1 | - | - | 71.2 | 70.0 | as 12b, but the send walks String/Binary/Collection/Value/Object dispatch (an extensions lookup per level) to Object>>isKindOf: |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 7a | `gen_call` | 2239 | 37.1 | 56.0 |
| 7b | `gen_update` | 2684 | 37.1 | 56.0 |
| 7c | `gen_cast` | 482 | 18.8 | 12.0 |
| 7d | `gen_probe` | 2309 | 37.1 | 56.0 |
| 8a | `try_no_raise` | 2.99 | 0.00 | 0.00 |
| 8b | `try_raise` | 620 | 13.1 | 36.0 |
| 12a | `struct_of` | 2.88 | 0.00 | 0.00 |
| 12b | `is_struct_check` | 2.12 | 0.00 | 0.00 |
| 12c | `is_binary_check` | 3.55 | 0.00 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 2.25 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 2.45 | 1.00 | 0.00 | 1.00 | 0.00 |
| 7a | bt | actorCall:c: | 11362 | 227 | 311 | 143 | 224 |
| 7a | ex | gen_call | 2241 | 38.1 | 56.0 | 17.0 | 33.0 |
| 7b | bt | actorUpdate:c: | 11513 | 227 | 317 | 143 | 224 |
| 7b | ex | gen_update | 2686 | 38.1 | 56.0 | 17.0 | 33.0 |
| 7c | bt | actorCast:c: | 7564 | 202 | 241 | 126 | 177 |
| 7c | ex | gen_cast | 485 | 19.8 | 12.0 | 7.02 | 3.00 |
| 7d | bt | actorProbe:c: | 16564 | 283 | 388 | 143 | 224 |
| 7d | ex | gen_probe | 2312 | 38.1 | 56.0 | 17.1 | 33.0 |
| 8a | bt | onDoNoRaise: | 1181 | 22.1 | 21.0 | 22.1 | 21.0 |
| 8a | ex | try_no_raise | 5.44 | 1.00 | 0.00 | 1.00 | 0.00 |
| 8b | bt | onDoRaise: | 231132 | 2468 | 22977 | 1932 | 21517 |
| 8b | ex | try_raise | 622 | 14.1 | 36.0 | 14.1 | 36.0 |
| 12a | bt | classOf:p: | 407 | 16.0 | 23.0 | 16.0 | 23.0 |
| 12a | ex | struct_of | 5.33 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12b | bt | isKindOf:p: | 2267 | 53.2 | 67.0 | 53.2 | 67.0 |
| 12b | ex | is_struct_check | 4.57 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12c | bt | isKindOfBuiltin:s: | 2825 | 72.2 | 70.0 | 72.2 | 70.0 |
| 12c | ex | is_binary_check | 6.00 | 1.00 | 0.00 | 1.00 | 0.00 |
