# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 18
# load average: 16:42  up 28 days,  2:32, 3 users, load averages: 27.98 38.44 29.22
# rounds 7
# load average after: 16:42  up 28 days,  2:32, 3 users, load averages: 27.98 38.44 29.22

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 13a | FFI call, integer result / apply | 111 | 16.1 | 14.0 | 5.04 | 1.00 | 0.00 | 22.1 | 16.1 | - | 15.1 | 14.0 | call 'beamtalk_erlang_proxy':'direct_call'('erlang', 'phash2', [A]) |
| 13b | FFI call, binary result, caller-typed / apply | 184 | 16.1 | 17.0 | 11.8 | 1.00 | 3.00 | 15.6 | 16.1 | 5.7 | 15.1 | 14.0 | call 'beamtalk_erlang_proxy':'direct_call'('erlang', 'integer_to_binary', [A]) |
| 13c | FFI call, {ok, V} result / apply | 251 | 19.2 | 25.0 | 8.81 | 1.00 | 3.00 | 28.5 | 19.2 | 8.3 | 18.2 | 22.0 | call 'beamtalk_erlang_proxy':'direct_call'('maps', 'find', [K, D]) |
| 13d | FFI call, ok-atom result / apply | 216 | 19.2 | 23.0 | 1.60 | 2.00 | 0.00 | 135.2 | 9.6 | - | 17.2 | 23.0 | call 'beamtalk_erlang_proxy':'direct_call'('vs_bench', 'ffi_ok', []) |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 13a | `apply_phash2` | 5.04 | 1.00 | 0.00 |
| 13a | `direct_phash2` | 4.66 | 1.00 | 0.00 |
| 13b | `apply_integer_to_binary` | 11.8 | 1.00 | 3.00 |
| 13b | `direct_integer_to_binary` | 12.2 | 1.00 | 3.00 |
| 13c | `apply_maps_find` | 8.81 | 1.00 | 3.00 |
| 13c | `direct_maps_find` | 9.10 | 1.00 | 3.00 |
| 13d | `apply_ffi_ok` | 1.60 | 2.00 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 0.80 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 0.88 | 1.00 | 0.00 | 1.00 | 0.00 |
| 13a | bt | ffiInt:a: | 112 | 17.1 | 14.0 | 17.1 | 14.0 |
| 13a | ex | apply_phash2 | 5.92 | 2.00 | 0.00 | 2.00 | 0.00 |
| 13a | ex | direct_phash2 | 5.54 | 2.00 | 0.00 | 2.00 | 0.00 |
| 13b | bt | ffiBinary:a: | 184 | 17.1 | 17.0 | 17.1 | 17.0 |
| 13b | ex | apply_integer_to_binary | 12.7 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13b | ex | direct_integer_to_binary | 13.1 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13c | bt | ffiOkTuple:d: | 252 | 20.2 | 25.0 | 20.2 | 25.0 |
| 13c | ex | apply_maps_find | 9.69 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13c | ex | direct_maps_find | 9.98 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13d | bt | ffiOk: | 217 | 20.2 | 23.0 | 20.2 | 23.0 |
| 13d | ex | apply_ffi_ok | 2.48 | 3.00 | 0.00 | 3.00 | 0.00 |
