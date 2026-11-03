# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 12:04:07 up 19 min,  0 user,  load average: 4.62, 3.82, 2.32
# rounds 3
# load average after: 12:04:10 up 19 min,  0 user,  load average: 4.62, 3.82, 2.32

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 7a | actor sync read: c count / GenServer.call | 2669 | 140 | 149 | 1746 | 37.1 | 56.0 | 1.5 | 3.8 | 2.7 | 103 | 93.0 | send(C, count, []) -> beamtalk_actor:sync_send, gen_server:call + telemetry/trace/ctx hooks both sides |
| 7b | actor state update: c increment / GenServer.call | 2752 | 140 | 155 | 1556 | 37.1 | 56.0 | 1.8 | 3.8 | 2.8 | 103 | 99.0 | as 7a |
| 7c | actor cast: c increment! / GenServer.cast | 1281 | 110 | 80.0 | 369 | 18.5 | 12.0 | 3.5 | 5.9 | 6.7 | 91.0 | 68.0 | beamtalk_message_dispatch:cast -> gen_server:cast + the same hooks |
| 7d | actor method doing self hasField: / GenServer.call | 3360 | 193 | 226 | 1768 | 37.1 | 56.0 | 1.9 | 5.2 | 4.0 | 156 | 170 | as 7a; in the actor `self hasField:` misses the module and walks the runtime method resolver |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 7a | `gen_call` | 1746 | 37.1 | 56.0 |
| 7b | `gen_update` | 1556 | 37.1 | 56.0 |
| 7c | `gen_cast` | 369 | 18.5 | 12.0 |
| 7d | `gen_probe` | 1768 | 37.1 | 56.0 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.23 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.11 | 1.00 | 0.00 | 1.00 | 0.00 |
| 7a | bt | actorCall:c: | 2670 | 141 | 149 | 79.1 | 83.0 |
| 7a | ex | gen_call | 1747 | 38.1 | 56.0 | 17.1 | 33.0 |
| 7b | bt | actorUpdate:c: | 2753 | 141 | 155 | 79.1 | 83.0 |
| 7b | ex | gen_update | 1557 | 38.1 | 56.0 | 17.1 | 33.0 |
| 7c | bt | actorCast:c: | 1283 | 111 | 80.0 | 62.0 | 38.0 |
| 7c | ex | gen_cast | 370 | 19.5 | 12.0 | 7.02 | 3.00 |
| 7d | bt | actorProbe:c: | 3361 | 194 | 226 | 79.1 | 83.0 |
| 7d | ex | gen_probe | 1769 | 38.1 | 56.0 | 17.1 | 33.0 |
