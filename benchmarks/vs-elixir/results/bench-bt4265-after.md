# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 13:12:08 up  1:27,  0 user,  load average: 6.37, 7.82, 9.37
# rounds 7
# load average after: 13:12:13 up  1:27,  0 user,  load average: 6.10, 7.74, 9.33

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 7a | actor sync read: c count / GenServer.call | 2397 | 89.1 | 142 | 1475 | 37.0 | 56.0 | 1.6 | 2.4 | 2.5 | 52.1 | 86.0 | send(C, count, []) -> beamtalk_actor:sync_send, gen_server:call + telemetry/trace/ctx hooks both sides |
| 7b | actor state update: c increment / GenServer.call | 2536 | 92.1 | 148 | 1521 | 37.1 | 56.0 | 1.7 | 2.5 | 2.6 | 55.1 | 92.0 | as 7a |
| 7c | actor cast: c increment! / GenServer.cast | 1342 | 68.0 | 73.0 | 426 | 18.4 | 12.0 | 3.1 | 3.7 | 6.1 | 49.6 | 61.0 | beamtalk_message_dispatch:cast -> gen_server:cast + the same hooks |
| 7d | actor method doing self hasField: / GenServer.call | 3150 | 142 | 219 | 1408 | 37.0 | 56.0 | 2.2 | 3.8 | 3.9 | 105 | 163 | as 7a; in the actor `self hasField:` misses the module and walks the runtime method resolver |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 7a | `gen_call` | 1475 | 37.0 | 56.0 |
| 7b | `gen_update` | 1521 | 37.1 | 56.0 |
| 7c | `gen_cast` | 426 | 18.4 | 12.0 |
| 7d | `gen_probe` | 1408 | 37.0 | 56.0 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.20 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.24 | 1.00 | 0.00 | 1.00 | 0.00 |
| 7a | bt | actorCall:c: | 2399 | 90.1 | 142 | 44.1 | 83.0 |
| 7a | ex | gen_call | 1476 | 38.0 | 56.0 | 17.0 | 33.0 |
| 7b | bt | actorUpdate:c: | 2537 | 93.1 | 148 | 44.1 | 83.0 |
| 7b | ex | gen_update | 1522 | 38.1 | 56.0 | 17.0 | 33.0 |
| 7c | bt | actorCast:c: | 1343 | 69.0 | 73.0 | 34.0 | 38.0 |
| 7c | ex | gen_cast | 428 | 19.4 | 12.0 | 7.02 | 3.00 |
| 7d | bt | actorProbe:c: | 3152 | 143 | 219 | 44.1 | 83.0 |
| 7d | ex | gen_probe | 1409 | 38.0 | 56.0 | 17.0 | 33.0 |
