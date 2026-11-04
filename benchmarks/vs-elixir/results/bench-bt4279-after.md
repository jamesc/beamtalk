# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 13:48:40 up  1:09,  0 user,  load average: 2.68, 2.84, 2.28
# rounds 7
# load average after: 13:48:46 up  1:09,  0 user,  load average: 2.42, 2.78, 2.27

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 7a | actor sync read: c count / GenServer.call | 3453 | 89.3 | 142 | 1990 | 37.1 | 56.0 | 1.7 | 2.4 | 2.5 | 52.2 | 86.0 | send(C, count, []) -> beamtalk_actor:sync_send, gen_server:call + telemetry/trace/ctx hooks both sides |
| 7b | actor state update: c increment / GenServer.call | 3584 | 92.2 | 148 | 1974 | 37.1 | 56.0 | 1.8 | 2.5 | 2.6 | 55.1 | 92.0 | as 7a |
| 7g | actor method reading two getters of a typed sealed Value / GenServer.call | 3505 | 97.3 | 158 | 2040 | 37.1 | 56.0 | 1.7 | 2.6 | 2.8 | 60.3 | 102 | as 7a; each `p x` is a tag + __is_slot/1-guarded maps:get with no cell bracket, the `+` a guarded BIF with none either (BT-4279) |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 7a | `gen_call` | 1990 | 37.1 | 56.0 |
| 7b | `gen_update` | 1974 | 37.1 | 56.0 |
| 7g | `gen_sum_of` | 2040 | 37.1 | 56.0 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.48 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.51 | 1.00 | 0.00 | 1.00 | 0.00 |
| 7a | bt | actorCall:c: | 3455 | 90.3 | 142 | 44.1 | 83.0 |
| 7a | ex | gen_call | 1991 | 38.1 | 56.0 | 17.0 | 33.0 |
| 7b | bt | actorUpdate:c: | 3585 | 93.2 | 148 | 44.1 | 83.0 |
| 7b | ex | gen_update | 1976 | 38.1 | 56.0 | 17.1 | 33.0 |
| 7g | bt | actorSumOf:c: | 3507 | 98.3 | 158 | 44.2 | 85.1 |
| 7g | ex | gen_sum_of | 2041 | 38.1 | 56.0 | 17.0 | 33.0 |
