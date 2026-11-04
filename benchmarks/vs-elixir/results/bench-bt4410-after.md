# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 13:11:32 up 24 min,  0 user,  load average: 1.49, 2.11, 1.99
# rounds 7
# load average after: 13:11:37 up 24 min,  0 user,  load average: 1.53, 2.11, 1.99

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 12b | p isKindOf: VbPoint / is_struct(p, Point) | 2146 | 39.2 | 58.0 | 2.48 | 0.00 | 0.00 | 865.2 | - | - | 39.2 | 58.0 | class ref built per call (whereis_class + module_name_safe) + send(P, 'isKindOf:') -> direct objectIsKindOf/2 call (no FFI proxy, BT-4410): one memoised {is_kind_of, C, B} lookup (BT-4221) |
| 12c | s isKindOf: String / is_binary(s) | 1532 | 45.0 | 61.0 | 2.79 | 0.00 | 0.00 | 548.8 | - | - | 45.0 | 61.0 | as 12b, but the send walks String/Binary/Collection/Value/Object dispatch (an extensions lookup per level) to Object>>isKindOf: |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 12b | `is_struct_check` | 2.48 | 0.00 | 0.00 |
| 12c | `is_binary_check` | 2.79 | 0.00 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.83 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.92 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12b | bt | isKindOf:p: | 2148 | 40.2 | 58.0 | 40.2 | 58.0 |
| 12b | ex | is_struct_check | 4.40 | 1.00 | 0.00 | 1.00 | 0.00 |
| 12c | bt | isKindOfBuiltin:s: | 1534 | 46.0 | 61.0 | 46.0 | 61.0 |
| 12c | ex | is_binary_check | 4.72 | 1.00 | 0.00 | 1.00 | 0.00 |
