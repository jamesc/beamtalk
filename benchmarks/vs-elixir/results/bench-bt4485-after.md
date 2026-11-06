# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 02:19:07 up  1:57,  0 user,  load average: 3.81, 4.91, 5.17
# rounds 7
# load average after: 02:19:10 up  1:57,  0 user,  load average: 3.67, 4.86, 5.15

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 13a | FFI call, integer result / apply | 13.4 | 1.00 | 0.00 | 8.59 | 1.00 | 0.00 | 1.6 | 1.0 | - | 0.00 | 0.00 | direct erlang:phash2(A) in a try (BT-4485), behind an is_number/is_atom/is_pid argument guard: the spec proves the Integer result needs no coercion; errors go to ffi_call_raised/8 |
| 13b | FFI call, binary result, caller-typed / apply | 210 | 14.0 | 7.00 | 34.4 | 1.00 | 3.00 | 6.1 | 14.0 | 2.3 | 13.0 | 4.00 | direct_call/4 ('erlang', 'integer_to_binary', [A], 'stock'): binary fast path |
| 13c | FFI call, {ok, V} result / apply | 485 | 24.1 | 21.0 | 26.4 | 1.00 | 3.00 | 18.4 | 24.1 | 7.0 | 23.1 | 18.0 | direct_call/4 ('maps', 'find', [K, D], 'stock'): mode precomputed, {ok, V} still wraps to Result |
| 13d | FFI call, ok-atom result / apply | 187 | 13.0 | 7.00 | 2.89 | 2.00 | 0.00 | 64.7 | 6.5 | - | 11.0 | 7.00 | direct_call/4 ('vs_bench', 'ffi_ok', [], 'stock'): mode precomputed, ok still wraps to Result |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 13a | `apply_phash2` | 8.59 | 1.00 | 0.00 |
| 13a | `direct_phash2` | 9.11 | 1.00 | 0.00 |
| 13b | `apply_integer_to_binary` | 34.4 | 1.00 | 3.00 |
| 13b | `direct_integer_to_binary` | 33.9 | 1.00 | 3.00 |
| 13c | `apply_maps_find` | 26.4 | 1.00 | 3.00 |
| 13c | `direct_maps_find` | 23.4 | 1.00 | 3.00 |
| 13d | `apply_ffi_ok` | 2.89 | 2.00 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.12 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.19 | 1.00 | 0.00 | 1.00 | 0.00 |
| 13a | bt | ffiInt:a: | 14.5 | 2.00 | 0.00 | 2.00 | 0.00 |
| 13a | ex | apply_phash2 | 9.78 | 2.00 | 0.00 | 2.00 | 0.00 |
| 13a | ex | direct_phash2 | 10.3 | 2.00 | 0.00 | 2.00 | 0.00 |
| 13b | bt | ffiBinary:a: | 211 | 15.0 | 7.00 | 15.0 | 7.00 |
| 13b | ex | apply_integer_to_binary | 35.6 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13b | ex | direct_integer_to_binary | 35.1 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13c | bt | ffiOkTuple:d: | 486 | 25.1 | 21.0 | 25.1 | 21.0 |
| 13c | ex | apply_maps_find | 27.5 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13c | ex | direct_maps_find | 24.6 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13d | bt | ffiOk: | 188 | 14.0 | 7.00 | 14.0 | 7.00 |
| 13d | ex | apply_ffi_ok | 4.08 | 3.00 | 0.00 | 3.00 | 0.00 |
