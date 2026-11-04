# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 03:22:27 up  1:37,  0 user,  load average: 13.24, 15.33, 16.31
# rounds 9
# load average after: 03:22:37 up  1:38,  0 user,  load average: 12.99, 15.20, 16.26

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 2b | construct: VbPoint x: a y: 4 / %Point{x: a, y: 4} | 407 | 7.06 | 11.0 | 168 | 0.03 | 6.00 | 2.4 | - | 1.8 | 7.03 | 5.00 | whereis_class + class_send: gen_server:call into the VbPoint class process |
| 2c | construct via new: VbPoint new: #{#x => a, #y => 4} / struct!(Point, %{x: a, y: 4}) | 7023 | 146 | 175 | 228 | 26.0 | 36.0 | 30.7 | 5.6 | 4.9 | 120 | 139 | class_send new: -> generated VbPoint new/1 -> check_init_args/5 against the declared-field set ({declared_fields, C} derived-cache hit, BT-4281) |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 2b | `construct` | 168 | 0.03 | 6.00 |
| 2c | `construct_map` | 228 | 26.0 | 36.0 |
| 2c | `construct` | 150 | 0.03 | 6.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 3.24 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 2.72 | 1.00 | 0.00 | 1.00 | 0.00 |
| 2b | bt | construct:a: | 411 | 8.06 | 11.0 | 8.05 | 11.0 |
| 2b | ex | construct | 170 | 1.03 | 6.00 | 1.03 | 6.00 |
| 2c | bt | constructMap:a: | 7026 | 147 | 175 | 147 | 175 |
| 2c | ex | construct | 152 | 1.03 | 6.00 | 1.03 | 6.00 |
| 2c | ex | construct_map | 231 | 27.0 | 36.0 | 27.0 | 36.0 |
