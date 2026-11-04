# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 03:23:34 up  1:38,  0 user,  load average: 13.44, 14.89, 16.09
# rounds 9
# load average after: 03:23:38 up  1:39,  0 user,  load average: 13.56, 14.89, 16.08

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 2b | construct: VbPoint x: a y: 4 / %Point{x: a, y: 4} | 424 | 7.05 | 11.0 | 149 | 0.03 | 6.00 | 2.8 | - | 1.8 | 7.03 | 5.00 | whereis_class + class_send: gen_server:call into the VbPoint class process |
| 2c | construct via new: VbPoint new: #{#x => a, #y => 4} / struct!(Point, %{x: a, y: 4}) | 1711 | 37.1 | 130 | 216 | 26.0 | 36.0 | 7.9 | 1.4 | 3.6 | 11.1 | 94.0 | class_send('new:') -> direct constructor: VbPoint:new/1 in the caller -> check_init_args/5, declared fields from one {declared_fields, C} derived-cache hit (BT-4281) |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 2b | `construct` | 149 | 0.03 | 6.00 |
| 2c | `construct_map` | 216 | 26.0 | 36.0 |
| 2c | `construct` | 159 | 0.03 | 6.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 3.62 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 2.72 | 1.00 | 0.00 | 1.00 | 0.00 |
| 2b | bt | construct:a: | 427 | 8.05 | 11.0 | 8.05 | 11.0 |
| 2b | ex | construct | 151 | 1.03 | 6.00 | 1.03 | 6.00 |
| 2c | bt | constructMap:a: | 1715 | 38.1 | 130 | 38.1 | 130 |
| 2c | ex | construct | 161 | 1.03 | 6.00 | 1.03 | 6.00 |
| 2c | ex | construct_map | 219 | 27.0 | 36.0 | 27.0 | 36.0 |
