# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 00:30:41 up 9 min,  0 user,  load average: 5.68, 5.03, 2.71
# rounds 7
# load average after: 00:30:44 up 9 min,  0 user,  load average: 5.68, 5.03, 2.71

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 13a | FFI call, integer result / apply | 134 | 14.0 | 4.00 | 8.35 | 1.00 | 0.00 | 16.1 | 14.0 | - | 13.0 | 4.00 | direct_call/4 ('erlang', 'phash2', [A], 'stock'): mode precomputed (BT-4153), integer fast path |
| 13b | FFI call, binary result, caller-typed / apply | 206 | 14.0 | 7.00 | 31.3 | 1.00 | 3.00 | 6.6 | 14.0 | 2.3 | 13.0 | 4.00 | direct_call/4 ('erlang', 'integer_to_binary', [A], 'stock'): binary fast path |
| 13c | FFI call, {ok, V} result / apply | 519 | 24.1 | 21.0 | 28.2 | 1.00 | 3.00 | 18.4 | 24.1 | 7.0 | 23.1 | 18.0 | direct_call/4 ('maps', 'find', [K, D], 'stock'): mode precomputed, {ok, V} still wraps to Result |
| 13d | FFI call, ok-atom result / apply | 200 | 13.0 | 7.00 | 3.09 | 2.00 | 0.00 | 64.9 | 6.5 | - | 11.0 | 7.00 | direct_call/4 ('vs_bench', 'ffi_ok', [], 'stock'): mode precomputed, ok still wraps to Result |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 13a | `apply_phash2` | 8.35 | 1.00 | 0.00 |
| 13a | `direct_phash2` | 8.96 | 1.00 | 0.00 |
| 13b | `apply_integer_to_binary` | 31.3 | 1.00 | 3.00 |
| 13b | `direct_integer_to_binary` | 29.2 | 1.00 | 3.00 |
| 13c | `apply_maps_find` | 28.2 | 1.00 | 3.00 |
| 13c | `direct_maps_find` | 25.4 | 1.00 | 3.00 |
| 13d | `apply_ffi_ok` | 3.09 | 2.00 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.10 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.11 | 1.00 | 0.00 | 1.00 | 0.00 |
| 13a | bt | ffiInt:a: | 135 | 15.0 | 4.00 | 15.0 | 4.00 |
| 13a | ex | apply_phash2 | 9.46 | 2.00 | 0.00 | 2.00 | 0.00 |
| 13a | ex | direct_phash2 | 10.1 | 2.00 | 0.00 | 2.00 | 0.00 |
| 13b | bt | ffiBinary:a: | 207 | 15.0 | 7.00 | 15.0 | 7.00 |
| 13b | ex | apply_integer_to_binary | 32.4 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13b | ex | direct_integer_to_binary | 30.4 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13c | bt | ffiOkTuple:d: | 521 | 25.1 | 21.0 | 25.1 | 21.0 |
| 13c | ex | apply_maps_find | 29.3 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13c | ex | direct_maps_find | 26.5 | 2.00 | 3.00 | 2.00 | 3.00 |
| 13d | bt | ffiOk: | 201 | 14.0 | 7.00 | 14.0 | 7.00 |
| 13d | ex | apply_ffi_ok | 4.19 | 3.00 | 0.00 | 3.00 | 0.00 |
