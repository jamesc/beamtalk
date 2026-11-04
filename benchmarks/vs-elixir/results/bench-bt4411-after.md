# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 14:43:41 up  1:56,  0 user,  load average: 1.89, 2.44, 3.72
# rounds 7
# load average after: 14:43:45 up  1:56,  0 user,  load average: 1.90, 2.43, 3.71

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 12b | p isKindOf: VbPoint / is_struct(p, Point) | 1320 | 46.0 | 62.0 | 2.79 | 0.00 | 0.00 | 473.0 | - | - | 46.0 | 62.0 | class ref built per call (whereis_class + module_name_safe) + send(P, 'isKindOf:') -> objectIsKindOf/2: one memoised {is_kind_of, C, B} lookup (BT-4221) |
| 12c | s isKindOf: String / is_binary(s) | 1229 | 42.0 | 54.0 | 2.94 | 0.00 | 0.00 | 418.5 | - | - | 42.0 | 54.0 | as 12b, but the String literal bakes its module (whereis only, no module_name_safe, BT-4411) and the send walks String/Binary/Collection/Value/Object dispatch (an extensions lookup per level) to Object>>isKindOf: |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 12b | `is_struct_check` | 2.79 | 0.00 | 0.00 |
| 12c | `is_binary_check` | 2.94 | 0.00 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 2.13 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.58 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12b | bt | isKindOf:p: | 1322 | 47.0 | 62.0 | 47.0 | 62.0 |
| 12b | ex | is_struct_check | 4.37 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12c | bt | isKindOfBuiltin:s: | 1231 | 43.0 | 54.0 | 43.0 | 54.0 |
| 12c | ex | is_binary_check | 4.52 | 1.00 | 0.00 | 1.00 | 0.00 |
