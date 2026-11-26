# BT-5261: comparison guards test the `String` tag, not `is_binary`

The before / after numbers for BT-5261. The inlined comparison guard took
`is_binary` as its `String` test, which no `String` passes since BT-4170
(a `String` is the tagged `{beamtalk_string, Bin}` term). The fix makes two
changes:

- The unknown-receiver guard is now `is_number` alone. A raw `Binary`
  dispatches and raises `does_not_understand`, and a `String` reaches
  `String>><`, as it already did.
- A declared `String` receiver is tested with the `String` tag. Its arm tests
  the right operand the BT-5213 way (bare BIF for a `String`, else a
  frame-free `type_error`).

Row 11p (`a < b`, `a :: String`, `b :: String`) is new in this change. It
measures the declared-`String` path, which used to dispatch.

## How it was measured

- Like for like: both sides are repo dev builds (`target/debug/beamtalk`,
  whose sysroot supplies the runtime and stdlib), on the same machine, with
  the new bench row on both sides. **base** is `main` at `8088582`;
  **after** is this branch.
- Machine: a 4-vCPU cloud container (x86_64, no pinning), OTP 28
  (erts 16.4), Elixir 1.20.1, 4 schedulers.
- `BEAMTALK=<compiler> benchmarks/vs-elixir/run.sh bench 7 11d,11e,11h,11i,11k,11m,11n,11o,11p`,
  three runs per side, alternating base / after (7 interleaved rounds per
  run). Each number is the median of its rounds, net of the side's loop
  baseline.

## Summary (BT ns, net; reductions / words per op)

| Row | base ns (1 / 2 / 3) | after ns (1 / 2 / 3) | base red / words | after red / words |
|---|---|---|---|---|
| 11d `a > 0` | 8.15 / 7.69 / 7.91 | 7.99 / 7.61 / 8.02 | 0 / 0 | 0 / 0 |
| 11e `a < 10` | 8.58 / 7.90 / 7.98 | 7.41 / 7.43 / 7.02 | 0 / 0 | 0 / 0 |
| 11h `5 < b` | 8.96 / 7.65 / 7.97 | 7.73 / 7.98 / 7.64 | 0 / 0 | 0 / 0 |
| 11i `a < b` untyped | 9.28 / 7.78 / 7.18 | 8.04 / 7.96 / 7.51 | 0 / 0 | 0 / 0 |
| 11k `5 + b` (reference) | 9.01 / 8.69 / 8.80 | 9.30 / 8.80 / 8.44 | 0 / 0 | 0 / 0 |
| 11m `"m" < b` | 15.5 / 14.7 / 14.9 | 15.0 / 15.7 / 15.6 | 0 / 0 | 0 / 0 |
| 11n `a < b` Strings | 117 / 90.7 / 92.8 | 110 / 108 / 99.5 | 12 / 2 | 12 / 2 |
| 11o `#m < b` | 110 / 107 / 107 | 114 / 112 / 101 | 9 / 2 | 9 / 2 |
| **11p `a :: String < b :: String`** | **106 / 98.1 / 96.7** | **8.01 / 7.97 / 7.73** | **12 / 2** | **0 / 0** |

## Reading

- **11p** is the row this change targets. A declared `String` comparison
  used to fail its `is_binary` guard and dispatch to `String>><`
  (~100 ns, 12 reductions, 2 words). It now runs inline: the `String` tag on
  the receiver, the `String` tag on the argument, then the bare BIF
  (~8 ns, 0 / 0). That is the cost of 11i, the untyped number row.
- **11i** (the gate row) loses its `is_binary` arm. Reductions and words are
  unchanged and ns is inside the base spread. A number takes the same first
  arm as before.
- **11n** (the gate row) dispatched before and still does. A tagged `String`
  failed `is_binary` too, so the code path is the same, at 12 / 2 on both
  sides. Its ns varies run to run on both sides (90.7–117 base,
  99.5–110 after), as it did in the BT-5213 runs (97.9–106).
- The other rows compile to the same code. Reductions and words are
  identical, and ns is within noise.

## Raw tables

Each run's full `run.sh` output, BT and Elixir columns included, is below.

### base, run 1

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 11d | a > 0 ifTrue: [1] ifFalse: [2] / if | 8.15 | 0.00 | 0.00 | 0.79 | 0.00 | 0.00 | 10.3 | - | - | 0.00 | 0.00 | send(Bool, 'ifTrue:ifFalse:', [fun, fun]) -> True:dispatch/3 |
| 11e | a < 10, a :: Integer / a < 10 | 8.58 | 0.00 | 0.00 | 0.94 | 0.00 | 0.00 | 9.1 | - | - | 0.00 | 0.00 | is_number guard (declared Integer, BT-4823), erlang:'<' |
| 11h | 5 < b, b untyped / 5 < b | 8.96 | 0.00 | 0.00 | 0.80 | 0.00 | 0.00 | 11.2 | - | - | 0.00 | 0.00 | case is_number(B): erlang:'<'(5, B); else send_number_comparison(B, 'lessThanFromNumber:', [5], '<') (ADR 0116 addendum, BT-5210; the call arm gives the site a frame) |
| 11i | a < b, both untyped / a < b | 9.28 | 0.00 | 0.00 | 2.21 | 0.00 | 0.00 | 4.2 | - | - | 0.00 | 0.00 | is_number(A) -> is_number(B) ? erlang:'<'(A, B) : send_number_comparison/4; else send(A, '<', [B]) (BT-5210; no is_binary arm since BT-5261) |
| 11k | 5 + b, b untyped / 5 + b | 9.01 | 0.00 | 0.00 | 0.59 | 0.00 | 0.00 | 15.2 | - | - | 0.00 | 0.00 | erlang:'+'(5, B) in a try; on badarith an is_number(B) test then send_number_coercion/4 (the arithmetic reference the comparison addendum matches) |
| 11m | "m" < b, b untyped / "m" < b | 15.5 | 0.00 | 0.00 | 17.3 | 0.00 | 0.00 | 0.9 | - | - | 0.00 | 0.00 | case is_record(B, beamtalk_string, 2): erlang:'<'("m", B); else erlang:error({beamtalk_ordering_type_error, "m", '<', B}) (BT-5213; frame-free) |
| 11n | a < b, both untyped Strings / a < b | 117 | 12.0 | 2.00 | 3.01 | 0.00 | 0.00 | 39.0 | - | - | 12.0 | 2.00 | is_number(A) false for a tagged String: send(A, '<', [B]) -> String>>< body: is_record(B, beamtalk_string, 2) ? erlang:'<' : erlang:error({beamtalk_ordering_type_error, ...}) (BT-5213) |
| 11p | a < b, a :: String, b :: String / a < b | 106 | 12.0 | 2.00 | 0.91 | 0.00 | 0.00 | 117.0 | - | - | 12.0 | 2.00 | is_record(A, beamtalk_string, 2) -> is_record(B, beamtalk_string, 2) ? erlang:'<'(A, B) : erlang:error({beamtalk_ordering_type_error, ...}); else send(A, '<', [B]) (BT-5261; inline, was a dispatch to String>><) |
| 11o | #m < b, b untyped / :m < b | 110 | 9.01 | 2.00 | 4.15 | 0.00 | 0.00 | 26.5 | - | - | 9.01 | 2.00 | send(A, '<', [B]) -> Symbol>>< body: is_atom(B) and not nil/true/false ? erlang:'<' : erlang:error({beamtalk_ordering_type_error, ...}) (BT-5213) |


### base, run 2

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 11d | a > 0 ifTrue: [1] ifFalse: [2] / if | 7.69 | 0.00 | 0.00 | 0.75 | 0.00 | 0.00 | 10.3 | - | - | 0.00 | 0.00 | send(Bool, 'ifTrue:ifFalse:', [fun, fun]) -> True:dispatch/3 |
| 11e | a < 10, a :: Integer / a < 10 | 7.90 | 0.00 | 0.00 | 0.69 | 0.00 | 0.00 | 11.5 | - | - | 0.00 | 0.00 | is_number guard (declared Integer, BT-4823), erlang:'<' |
| 11h | 5 < b, b untyped / 5 < b | 7.65 | 0.00 | 0.00 | 0.59 | 0.00 | 0.00 | 12.9 | - | - | 0.00 | 0.00 | case is_number(B): erlang:'<'(5, B); else send_number_comparison(B, 'lessThanFromNumber:', [5], '<') (ADR 0116 addendum, BT-5210; the call arm gives the site a frame) |
| 11i | a < b, both untyped / a < b | 7.78 | 0.00 | 0.00 | 1.15 | 0.00 | 0.00 | 6.8 | - | - | 0.00 | 0.00 | is_number(A) -> is_number(B) ? erlang:'<'(A, B) : send_number_comparison/4; else send(A, '<', [B]) (BT-5210; no is_binary arm since BT-5261) |
| 11k | 5 + b, b untyped / 5 + b | 8.69 | 0.00 | 0.00 | 0.53 | 0.00 | 0.00 | 16.3 | - | - | 0.00 | 0.00 | erlang:'+'(5, B) in a try; on badarith an is_number(B) test then send_number_coercion/4 (the arithmetic reference the comparison addendum matches) |
| 11m | "m" < b, b untyped / "m" < b | 14.7 | 0.00 | 0.00 | 11.7 | 0.00 | 0.00 | 1.3 | - | - | 0.00 | 0.00 | case is_record(B, beamtalk_string, 2): erlang:'<'("m", B); else erlang:error({beamtalk_ordering_type_error, "m", '<', B}) (BT-5213; frame-free) |
| 11n | a < b, both untyped Strings / a < b | 90.7 | 12.0 | 2.00 | 0.93 | 0.00 | 0.00 | 97.4 | - | - | 12.0 | 2.00 | is_number(A) false for a tagged String: send(A, '<', [B]) -> String>>< body: is_record(B, beamtalk_string, 2) ? erlang:'<' : erlang:error({beamtalk_ordering_type_error, ...}) (BT-5213) |
| 11p | a < b, a :: String, b :: String / a < b | 98.1 | 12.0 | 2.00 | 0.96 | 0.00 | 0.00 | 102.5 | - | - | 12.0 | 2.00 | is_record(A, beamtalk_string, 2) -> is_record(B, beamtalk_string, 2) ? erlang:'<'(A, B) : erlang:error({beamtalk_ordering_type_error, ...}); else send(A, '<', [B]) (BT-5261; inline, was a dispatch to String>><) |
| 11o | #m < b, b untyped / :m < b | 107 | 9.01 | 2.00 | 3.39 | 0.00 | 0.00 | 31.5 | - | - | 9.01 | 2.00 | send(A, '<', [B]) -> Symbol>>< body: is_atom(B) and not nil/true/false ? erlang:'<' : erlang:error({beamtalk_ordering_type_error, ...}) (BT-5213) |


### base, run 3

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 11d | a > 0 ifTrue: [1] ifFalse: [2] / if | 7.91 | 0.00 | 0.00 | 0.79 | 0.00 | 0.00 | 10.0 | - | - | 0.00 | 0.00 | send(Bool, 'ifTrue:ifFalse:', [fun, fun]) -> True:dispatch/3 |
| 11e | a < 10, a :: Integer / a < 10 | 7.98 | 0.00 | 0.00 | 0.90 | 0.00 | 0.00 | 8.9 | - | - | 0.00 | 0.00 | is_number guard (declared Integer, BT-4823), erlang:'<' |
| 11h | 5 < b, b untyped / 5 < b | 7.97 | 0.00 | 0.00 | 0.77 | 0.00 | 0.00 | 10.3 | - | - | 0.00 | 0.00 | case is_number(B): erlang:'<'(5, B); else send_number_comparison(B, 'lessThanFromNumber:', [5], '<') (ADR 0116 addendum, BT-5210; the call arm gives the site a frame) |
| 11i | a < b, both untyped / a < b | 7.18 | 0.00 | 0.00 | 1.43 | 0.00 | 0.00 | 5.0 | - | - | 0.00 | 0.00 | is_number(A) -> is_number(B) ? erlang:'<'(A, B) : send_number_comparison/4; else send(A, '<', [B]) (BT-5210; no is_binary arm since BT-5261) |
| 11k | 5 + b, b untyped / 5 + b | 8.80 | 0.00 | 0.00 | 0.71 | 0.00 | 0.00 | 12.4 | - | - | 0.00 | 0.00 | erlang:'+'(5, B) in a try; on badarith an is_number(B) test then send_number_coercion/4 (the arithmetic reference the comparison addendum matches) |
| 11m | "m" < b, b untyped / "m" < b | 14.9 | 0.00 | 0.00 | 10.7 | 0.00 | 0.00 | 1.4 | - | - | 0.00 | 0.00 | case is_record(B, beamtalk_string, 2): erlang:'<'("m", B); else erlang:error({beamtalk_ordering_type_error, "m", '<', B}) (BT-5213; frame-free) |
| 11n | a < b, both untyped Strings / a < b | 92.8 | 12.0 | 2.00 | 1.64 | 0.00 | 0.00 | 56.5 | - | - | 12.0 | 2.00 | is_number(A) false for a tagged String: send(A, '<', [B]) -> String>>< body: is_record(B, beamtalk_string, 2) ? erlang:'<' : erlang:error({beamtalk_ordering_type_error, ...}) (BT-5213) |
| 11p | a < b, a :: String, b :: String / a < b | 96.7 | 12.0 | 2.00 | 1.09 | 0.00 | 0.00 | 88.4 | - | - | 12.0 | 2.00 | is_record(A, beamtalk_string, 2) -> is_record(B, beamtalk_string, 2) ? erlang:'<'(A, B) : erlang:error({beamtalk_ordering_type_error, ...}); else send(A, '<', [B]) (BT-5261; inline, was a dispatch to String>><) |
| 11o | #m < b, b untyped / :m < b | 107 | 9.01 | 2.00 | 3.70 | 0.00 | 0.00 | 29.0 | - | - | 9.01 | 2.00 | send(A, '<', [B]) -> Symbol>>< body: is_atom(B) and not nil/true/false ? erlang:'<' : erlang:error({beamtalk_ordering_type_error, ...}) (BT-5213) |


### after, run 1

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 11d | a > 0 ifTrue: [1] ifFalse: [2] / if | 7.99 | 0.00 | 0.00 | 0.58 | 0.00 | 0.00 | 13.8 | - | - | 0.00 | 0.00 | send(Bool, 'ifTrue:ifFalse:', [fun, fun]) -> True:dispatch/3 |
| 11e | a < 10, a :: Integer / a < 10 | 7.41 | 0.00 | 0.00 | 0.91 | 0.00 | 0.00 | 8.1 | - | - | 0.00 | 0.00 | is_number guard (declared Integer, BT-4823), erlang:'<' |
| 11h | 5 < b, b untyped / 5 < b | 7.73 | 0.00 | 0.00 | 0.34 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | case is_number(B): erlang:'<'(5, B); else send_number_comparison(B, 'lessThanFromNumber:', [5], '<') (ADR 0116 addendum, BT-5210; the call arm gives the site a frame) |
| 11i | a < b, both untyped / a < b | 8.04 | 0.00 | 0.00 | 1.31 | 0.00 | 0.00 | 6.1 | - | - | 0.00 | 0.00 | is_number(A) -> is_number(B) ? erlang:'<'(A, B) : send_number_comparison/4; else send(A, '<', [B]) (BT-5210; no is_binary arm since BT-5261) |
| 11k | 5 + b, b untyped / 5 + b | 9.30 | 0.00 | 0.00 | 0.31 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | erlang:'+'(5, B) in a try; on badarith an is_number(B) test then send_number_coercion/4 (the arithmetic reference the comparison addendum matches) |
| 11m | "m" < b, b untyped / "m" < b | 15.0 | 0.00 | 0.00 | 11.2 | 0.00 | 0.00 | 1.3 | - | - | 0.00 | 0.00 | case is_record(B, beamtalk_string, 2): erlang:'<'("m", B); else erlang:error({beamtalk_ordering_type_error, "m", '<', B}) (BT-5213; frame-free) |
| 11n | a < b, both untyped Strings / a < b | 110 | 12.0 | 2.00 | 1.30 | 0.00 | 0.00 | 84.7 | - | - | 12.0 | 2.00 | is_number(A) false for a tagged String: send(A, '<', [B]) -> String>>< body: is_record(B, beamtalk_string, 2) ? erlang:'<' : erlang:error({beamtalk_ordering_type_error, ...}) (BT-5213) |
| 11p | a < b, a :: String, b :: String / a < b | 8.01 | 0.00 | 0.00 | 0.77 | 0.00 | 0.00 | 10.4 | - | - | 0.00 | 0.00 | is_record(A, beamtalk_string, 2) -> is_record(B, beamtalk_string, 2) ? erlang:'<'(A, B) : erlang:error({beamtalk_ordering_type_error, ...}); else send(A, '<', [B]) (BT-5261; inline, was a dispatch to String>><) |
| 11o | #m < b, b untyped / :m < b | 114 | 9.01 | 2.00 | 3.66 | 0.00 | 0.00 | 31.1 | - | - | 9.01 | 2.00 | send(A, '<', [B]) -> Symbol>>< body: is_atom(B) and not nil/true/false ? erlang:'<' : erlang:error({beamtalk_ordering_type_error, ...}) (BT-5213) |


### after, run 2

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 11d | a > 0 ifTrue: [1] ifFalse: [2] / if | 7.61 | 0.00 | 0.00 | 0.51 | 0.00 | 0.00 | 14.9 | - | - | 0.00 | 0.00 | send(Bool, 'ifTrue:ifFalse:', [fun, fun]) -> True:dispatch/3 |
| 11e | a < 10, a :: Integer / a < 10 | 7.43 | 0.00 | 0.00 | 0.78 | 0.00 | 0.00 | 9.5 | - | - | 0.00 | 0.00 | is_number guard (declared Integer, BT-4823), erlang:'<' |
| 11h | 5 < b, b untyped / 5 < b | 7.98 | 0.00 | 0.00 | 0.51 | 0.00 | 0.00 | 15.8 | - | - | 0.00 | 0.00 | case is_number(B): erlang:'<'(5, B); else send_number_comparison(B, 'lessThanFromNumber:', [5], '<') (ADR 0116 addendum, BT-5210; the call arm gives the site a frame) |
| 11i | a < b, both untyped / a < b | 7.96 | 0.00 | 0.00 | 1.34 | 0.00 | 0.00 | 6.0 | - | - | 0.00 | 0.00 | is_number(A) -> is_number(B) ? erlang:'<'(A, B) : send_number_comparison/4; else send(A, '<', [B]) (BT-5210; no is_binary arm since BT-5261) |
| 11k | 5 + b, b untyped / 5 + b | 8.80 | 0.00 | 0.00 | 0.20 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | erlang:'+'(5, B) in a try; on badarith an is_number(B) test then send_number_coercion/4 (the arithmetic reference the comparison addendum matches) |
| 11m | "m" < b, b untyped / "m" < b | 15.7 | 0.00 | 0.00 | 12.6 | 0.00 | 0.00 | 1.3 | - | - | 0.00 | 0.00 | case is_record(B, beamtalk_string, 2): erlang:'<'("m", B); else erlang:error({beamtalk_ordering_type_error, "m", '<', B}) (BT-5213; frame-free) |
| 11n | a < b, both untyped Strings / a < b | 108 | 12.0 | 2.00 | 0.94 | 0.00 | 0.00 | 114.6 | - | - | 12.0 | 2.00 | is_number(A) false for a tagged String: send(A, '<', [B]) -> String>>< body: is_record(B, beamtalk_string, 2) ? erlang:'<' : erlang:error({beamtalk_ordering_type_error, ...}) (BT-5213) |
| 11p | a < b, a :: String, b :: String / a < b | 7.97 | 0.00 | 0.00 | 0.90 | 0.00 | 0.00 | 8.9 | - | - | 0.00 | 0.00 | is_record(A, beamtalk_string, 2) -> is_record(B, beamtalk_string, 2) ? erlang:'<'(A, B) : erlang:error({beamtalk_ordering_type_error, ...}); else send(A, '<', [B]) (BT-5261; inline, was a dispatch to String>><) |
| 11o | #m < b, b untyped / :m < b | 112 | 9.01 | 2.00 | 3.60 | 0.00 | 0.00 | 30.9 | - | - | 9.01 | 2.00 | send(A, '<', [B]) -> Symbol>>< body: is_atom(B) and not nil/true/false ? erlang:'<' : erlang:error({beamtalk_ordering_type_error, ...}) (BT-5213) |


### after, run 3

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 11d | a > 0 ifTrue: [1] ifFalse: [2] / if | 8.02 | 0.00 | 0.00 | 0.89 | 0.00 | 0.00 | 9.0 | - | - | 0.00 | 0.00 | send(Bool, 'ifTrue:ifFalse:', [fun, fun]) -> True:dispatch/3 |
| 11e | a < 10, a :: Integer / a < 10 | 7.02 | 0.00 | 0.00 | 0.93 | 0.00 | 0.00 | 7.6 | - | - | 0.00 | 0.00 | is_number guard (declared Integer, BT-4823), erlang:'<' |
| 11h | 5 < b, b untyped / 5 < b | 7.64 | 0.00 | 0.00 | 0.84 | 0.00 | 0.00 | 9.1 | - | - | 0.00 | 0.00 | case is_number(B): erlang:'<'(5, B); else send_number_comparison(B, 'lessThanFromNumber:', [5], '<') (ADR 0116 addendum, BT-5210; the call arm gives the site a frame) |
| 11i | a < b, both untyped / a < b | 7.51 | 0.00 | 0.00 | 1.32 | 0.00 | 0.00 | 5.7 | - | - | 0.00 | 0.00 | is_number(A) -> is_number(B) ? erlang:'<'(A, B) : send_number_comparison/4; else send(A, '<', [B]) (BT-5210; no is_binary arm since BT-5261) |
| 11k | 5 + b, b untyped / 5 + b | 8.44 | 0.00 | 0.00 | 0.60 | 0.00 | 0.00 | 14.0 | - | - | 0.00 | 0.00 | erlang:'+'(5, B) in a try; on badarith an is_number(B) test then send_number_coercion/4 (the arithmetic reference the comparison addendum matches) |
| 11m | "m" < b, b untyped / "m" < b | 15.6 | 0.00 | 0.00 | 11.4 | 0.00 | 0.00 | 1.4 | - | - | 0.00 | 0.00 | case is_record(B, beamtalk_string, 2): erlang:'<'("m", B); else erlang:error({beamtalk_ordering_type_error, "m", '<', B}) (BT-5213; frame-free) |
| 11n | a < b, both untyped Strings / a < b | 99.5 | 12.0 | 2.00 | 1.09 | 0.00 | 0.00 | 91.0 | - | - | 12.0 | 2.00 | is_number(A) false for a tagged String: send(A, '<', [B]) -> String>>< body: is_record(B, beamtalk_string, 2) ? erlang:'<' : erlang:error({beamtalk_ordering_type_error, ...}) (BT-5213) |
| 11p | a < b, a :: String, b :: String / a < b | 7.73 | 0.00 | 0.00 | 1.02 | 0.00 | 0.00 | 7.6 | - | - | 0.00 | 0.00 | is_record(A, beamtalk_string, 2) -> is_record(B, beamtalk_string, 2) ? erlang:'<'(A, B) : erlang:error({beamtalk_ordering_type_error, ...}); else send(A, '<', [B]) (BT-5261; inline, was a dispatch to String>><) |
| 11o | #m < b, b untyped / :m < b | 101 | 9.01 | 2.00 | 3.50 | 0.00 | 0.00 | 28.8 | - | - | 9.01 | 2.00 | send(A, '<', [B]) -> Symbol>>< body: is_atom(B) and not nil/true/false ? erlang:'<' : erlang:error({beamtalk_ordering_type_error, ...}) (BT-5213) |

