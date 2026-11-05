# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 14:12:24 up  2:28,  0 user,  load average: 5.21, 8.29, 8.44
# rounds 9
# load average after: 14:12:25 up  2:28,  0 user,  load average: 5.21, 8.29, 8.44

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 12a | p class / p.__struct__ | 142 | 1.04 | 5.00 | 3.51 | 0.00 | 0.00 | 40.4 | - | - | 1.04 | 5.00 | typed receiver (BT-4127): is_map + tag guard, erlang:whereis + class-object tuple with the pinned module; untyped: beamtalk_primitive:class_of_object derived-cache hit |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 12a | `struct_of` | 3.51 | 0.00 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.21 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.22 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12a | bt | classOf:p: | 143 | 2.04 | 5.00 | 2.03 | 5.00 |
| 12a | ex | struct_of | 4.73 | 1.00 | 0.00 | 1.00 | 0.00 |
