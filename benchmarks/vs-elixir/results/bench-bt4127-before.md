# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 13:37:22 up  1:53,  0 user,  load average: 4.69, 6.11, 5.83
# rounds 9
# load average after: 13:37:24 up  1:53,  0 user,  load average: 4.69, 6.11, 5.83

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 12a | p class / p.__struct__ | 292 | 15.0 | 23.0 | 3.62 | 0.00 | 0.00 | 80.5 | - | - | 15.0 | 23.0 | beamtalk_primitive:class_of_object: derived-cache hit (counter + ETS read + pid liveness) |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 12a | `struct_of` | 3.62 | 0.00 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.25 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.23 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12a | bt | classOf:p: | 293 | 16.0 | 23.0 | 16.0 | 23.0 |
| 12a | ex | struct_of | 4.84 | 1.00 | 0.00 | 1.00 | 0.00 |
