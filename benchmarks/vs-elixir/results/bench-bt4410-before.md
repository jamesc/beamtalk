# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 13:09:38 up 22 min,  0 user,  load average: 2.02, 2.26, 2.01
# rounds 7
# load average after: 13:09:46 up 22 min,  0 user,  load average: 1.86, 2.22, 1.99

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 12b | p isKindOf: VbPoint / is_struct(p, Point) | 1385 | 46.0 | 62.0 | 3.35 | 0.00 | 0.00 | 413.7 | - | - | 46.0 | 62.0 | class ref built per call (whereis_class + module_name_safe) + send(P, 'isKindOf:') -> objectIsKindOf/2: one memoised {is_kind_of, C, B} lookup (BT-4221) |
| 12c | s isKindOf: String / is_binary(s) | 2423 | 52.1 | 65.0 | 2.80 | 0.00 | 0.00 | 866.0 | - | - | 52.1 | 65.0 | as 12b, but the send walks String/Binary/Collection/Value/Object dispatch (an extensions lookup per level) to Object>>isKindOf: |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 12b | `is_struct_check` | 3.35 | 0.00 | 0.00 |
| 12c | `is_binary_check` | 2.80 | 0.00 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.55 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.67 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12b | bt | isKindOf:p: | 1387 | 47.0 | 62.0 | 47.0 | 62.0 |
| 12b | ex | is_struct_check | 5.02 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12c | bt | isKindOfBuiltin:s: | 2425 | 53.1 | 65.0 | 53.1 | 65.0 |
| 12c | ex | is_binary_check | 4.47 | 1.00 | 0.00 | 1.00 | 0.00 |
