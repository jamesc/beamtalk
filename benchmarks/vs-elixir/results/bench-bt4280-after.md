# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 02:36:25 up 51 min,  0 user,  load average: 0.78, 1.38, 2.55
# rounds 7
# load average after: 02:36:31 up 51 min,  0 user,  load average: 0.82, 1.37, 2.53

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 7a | actor sync read: c count / GenServer.call | 2760 | 89.3 | 146 | 1788 | 37.1 | 56.0 | 1.5 | 2.4 | 2.6 | 52.2 | 90.0 | send(C, count, []) -> beamtalk_actor:sync_send, gen_server:call + telemetry/trace/ctx hooks both sides |
| 7b | actor state update: c increment / GenServer.call | 2927 | 92.3 | 152 | 1800 | 37.1 | 56.0 | 1.6 | 2.5 | 2.7 | 55.2 | 96.0 | as 7a |
| 7e | inherited actor method doing self increment / delegated GenServer.call | 3337 | 127 | 203 | 1524 | 42.1 | 56.0 | 2.2 | 3.0 | 3.6 | 85.2 | 147 | as 7a; the subclass's dispatch/4 default arm takes the memoised inherited_target/2 verdict to the owner (BT-4280) |
| 7f | actor method doing self increment / GenServer.call | 2846 | 97.5 | 159 | 1568 | 41.1 | 56.0 | 1.8 | 2.4 | 2.8 | 56.4 | 103 | as 7a; the self-send is a static call inside the class module |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 7a | `gen_call` | 1788 | 37.1 | 56.0 |
| 7b | `gen_update` | 1800 | 37.1 | 56.0 |
| 7e | `gen_via_self` | 1524 | 42.1 | 56.0 |
| 7f | `gen_via_self` | 1568 | 41.1 | 56.0 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.54 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.50 | 1.00 | 0.00 | 1.00 | 0.00 |
| 7a | bt | actorCall:c: | 2761 | 90.3 | 146 | 44.1 | 83.0 |
| 7a | ex | gen_call | 1789 | 38.1 | 56.0 | 17.1 | 33.0 |
| 7b | bt | actorUpdate:c: | 2928 | 93.3 | 152 | 44.1 | 83.0 |
| 7b | ex | gen_update | 1801 | 38.1 | 56.0 | 17.1 | 33.0 |
| 7e | bt | actorViaSelfSub:c: | 3339 | 128 | 203 | 44.1 | 83.0 |
| 7e | ex | gen_via_self | 1525 | 43.1 | 56.0 | 17.0 | 33.0 |
| 7f | bt | actorViaSelf:c: | 2848 | 98.5 | 159 | 44.1 | 83.0 |
| 7f | ex | gen_via_self | 1570 | 42.1 | 56.0 | 17.0 | 33.0 |
