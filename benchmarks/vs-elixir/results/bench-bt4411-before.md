# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 14:36:01 up  1:49,  0 user,  load average: 2.11, 4.50, 4.93
# rounds 7
# load average after: 14:36:09 up  1:49,  0 user,  load average: 2.02, 4.44, 4.91

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 12b | p isKindOf: VbPoint / is_struct(p, Point) | 1407 | 46.0 | 62.0 | 1.83 | 0.00 | 0.00 | 768.0 | - | - | 46.0 | 62.0 | class ref built per call (whereis_class + module_name_safe) + send(P, 'isKindOf:') -> objectIsKindOf/2: one memoised {is_kind_of, C, B} lookup (BT-4221) |
| 12c | s isKindOf: String / is_binary(s) | 2377 | 52.1 | 65.0 | 3.48 | 0.00 | 0.00 | 683.0 | - | - | 52.1 | 65.0 | as 12b, but the send walks String/Binary/Collection/Value/Object dispatch (an extensions lookup per level) to Object>>isKindOf: |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 12b | `is_struct_check` | 1.83 | 0.00 | 0.00 |
| 12c | `is_binary_check` | 3.48 | 0.00 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.50 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.39 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12b | bt | isKindOf:p: | 1408 | 47.0 | 62.0 | 47.0 | 62.0 |
| 12b | ex | is_struct_check | 3.22 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12c | bt | isKindOfBuiltin:s: | 2378 | 53.1 | 65.0 | 53.1 | 65.0 |
| 12c | ex | is_binary_check | 4.87 | 1.00 | 0.00 | 1.00 | 0.00 |
