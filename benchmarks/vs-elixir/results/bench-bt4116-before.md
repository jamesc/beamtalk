# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 11:58:03 up  1:04,  0 user,  load average: 3.01, 5.73, 4.96
# rounds 9
# load average after: 11:58:04 up  1:05,  0 user,  load average: 3.01, 5.73, 4.96

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 6b | do: summing 100 elements / Enum.reduce | 4106 | 302 | 404 | 282 | 303 | 0.00 | 14.5 | 1.0 | - | -0.70 | 404 | lists:foldl over a fun that maps:get/maps:put `sum` in a state map; + with coercion try |
| 6c | do: over 100, no assignment / Enum.each | 850 | 303 | 0.00 | 847 | 303 | 0.00 | 1.0 | 1.0 | - | 0.00 | 0.00 | is_list guard, lists:foreach(fun) |
| 6e | inject:into: 100 elements / Enum.reduce | 691 | 302 | 0.00 | 284 | 303 | 0.00 | 2.4 | 1.0 | - | -1.00 | 0.00 | is_list guard, lists:foldl(fun); + with coercion try |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 6b | `enum_reduce_sum` | 282 | 303 | 0.00 |
| 6b | `lists_foldl` | 297 | 302 | 0.00 |
| 6c | `enum_each` | 847 | 303 | 0.00 |
| 6e | `enum_reduce_sum` | 284 | 303 | 0.00 |
| 6e | `lists_foldl` | 309 | 302 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.28 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.14 | 1.00 | 0.00 | 1.00 | 0.00 |
| 6b | bt | doSum:list: | 4107 | 303 | 404 | 303 | 404 |
| 6b | ex | enum_reduce_sum | 283 | 304 | 0.00 | 304 | 0.00 |
| 6b | ex | lists_foldl | 298 | 303 | 0.00 | 303 | 0.00 |
| 6c | bt | doNoAssign:list: | 851 | 304 | 0.00 | 304 | 0.00 |
| 6c | ex | enum_each | 848 | 304 | 0.00 | 304 | 0.00 |
| 6e | bt | inject:list: | 693 | 303 | 0.00 | 303 | 0.00 |
| 6e | ex | enum_reduce_sum | 285 | 304 | 0.00 | 304 | 0.00 |
| 6e | ex | lists_foldl | 310 | 303 | 0.00 | 303 | 0.00 |
