# BT-5213: `String` and `Symbol` receivers order only against their own kind

The before / after numbers for BT-5213 (ADR 0116 § Addendum follow-up). A
`String` or `Symbol` on the left of `<`, `<=`, `>`, `>=` now tests its right
operand with its class's routing guard and raises a frame-free
`erlang:error({beamtalk_ordering_type_error, L, Op, R})` for any other kind.
Rows 11m–11o are new in this change: `"m" < b` (a proven `String` receiver,
the inlined test), `a < b` with two untyped Strings (dispatch to the
`String>><` primitive body) and `#m < b` (dispatch to `Symbol>><`).

## How it was measured

- Like for like: both sides are repo dev builds (`target/debug/beamtalk`,
  its sysroot supplying the runtime and stdlib) on the same machine, with the
  new bench rows on both sides. **base** is `main` at `52539ed` (Phase 2c);
  **after** is this branch.
- Machine: a 4-vCPU cloud container (x86_64, no pinning), OTP 28 (erts 16.4),
  Elixir 1.20.1, 4 schedulers.
- `BEAMTALK=<compiler> benchmarks/vs-elixir/run.sh bench 7 11d,11e,11h,11i,11k,11m,11n,11o`,
  three runs per side (7 interleaved rounds each; each number is the median
  of its rounds, net of the side's loop baseline).
- A first version of the change raised through a runtime helper call
  (`beamtalk_message_dispatch:ordering_type_error/3`) instead of
  `erlang:error/1`. Its 11m was 17.6 / 17.6 / 18.0 ns: the call arm gives the
  site a stack frame (ADR 0116 § The failing arm). The frame-free arm below
  removes that cost.

## Summary (BT ns, net; reductions / words per op)

| Row | base ns (1 / 2 / 3) | after ns (1 / 2 / 3) | red / words, both sides |
|---|---|---|---|
| 11d `a > 0` | 7.95 / 8.50 / 8.02 | 7.96 / 8.00 / 7.93 | 0 / 0 |
| 11e `a < 10` | 7.79 / 8.34 / 7.95 | 7.95 / 7.98 / 7.54 | 0 / 0 |
| 11h `5 < b` | 7.89 / 7.55 / 7.99 | 7.69 / 7.99 / 7.85 | 0 / 0 |
| 11i `a < b` untyped | 7.97 / 7.97 / 7.98 | 8.20 / 8.20 / 8.06 | 0 / 0 |
| 11k `5 + b` (reference) | 9.49 / 9.53 / 8.90 | 9.09 / 9.20 / 9.11 | 0 / 0 |
| 11m `"m" < b` | 16.4 / 15.2 / 15.1 | 15.2 / 15.3 / 15.0 | 0 / 0 |
| 11n `a < b` Strings | 104 / 100 / 106 | 102 / 97.9 / 101 | 12 / 2 |
| 11o `#m < b` | 98.7 / 98.3 / 99.5 | 108 / 96.6 / 102 | 9 / 2 |

## After, run 2 (full output)

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 11d | a > 0 ifTrue: [1] ifFalse: [2] / if | 8.00 | 0.00 | 0.00 | 0.92 | 0.00 | 0.00 | 8.7 | - | - | 0.00 | 0.00 | send(Bool, 'ifTrue:ifFalse:', [fun, fun]) -> True:dispatch/3 |
| 11e | a < 10, a :: Integer / a < 10 | 7.98 | 0.00 | 0.00 | 0.79 | 0.00 | 0.00 | 10.1 | - | - | 0.00 | 0.00 | is_number guard (declared Integer, BT-4823), erlang:'<' |
| 11h | 5 < b, b untyped / 5 < b | 7.99 | 0.00 | 0.00 | 0.63 | 0.00 | 0.00 | 12.8 | - | - | 0.00 | 0.00 | case is_number(B): erlang:'<'(5, B); else send_number_comparison(B, 'lessThanFromNumber:', [5], '<') (ADR 0116 addendum, BT-5210; the call arm gives the site a frame) |
| 11i | a < b, both untyped / a < b | 8.20 | 0.00 | 0.00 | 1.10 | 0.00 | 0.00 | 7.5 | - | - | 0.00 | 0.00 | is_number(A) -> is_number(B) ? erlang:'<'(A, B) : send_number_comparison/4; else is_binary(A) ? erlang:'<'(A, B) : send(A, '<', [B]) (BT-5210) |
| 11k | 5 + b, b untyped / 5 + b | 9.20 | 0.00 | 0.00 | 0.65 | 0.00 | 0.00 | 14.2 | - | - | 0.00 | 0.00 | erlang:'+'(5, B) in a try; on badarith an is_number(B) test then send_number_coercion/4 (the arithmetic reference the comparison addendum matches) |
| 11m | "m" < b, b untyped / "m" < b | 15.3 | 0.00 | 0.00 | 11.4 | 0.00 | 0.00 | 1.3 | - | - | 0.00 | 0.00 | case is_record(B, beamtalk_string, 2): erlang:'<'("m", B); else ordering_type_error/3 (BT-5213; the call arm gives the site a frame) |
| 11n | a < b, both untyped Strings / a < b | 97.9 | 12.0 | 2.00 | 1.03 | 0.00 | 0.00 | 94.8 | - | - | 12.0 | 2.00 | is_number(A) / is_binary(A) both false for a tagged String: send(A, '<', [B]) -> String>>< body: is_record(B, beamtalk_string, 2) ? erlang:'<' : ordering_type_error/3 (BT-5213) |
| 11o | #m < b, b untyped / :m < b | 96.6 | 9.01 | 2.00 | 3.42 | 0.00 | 0.00 | 28.3 | - | - | 9.01 | 2.00 | send(A, '<', [B]) -> Symbol>>< body: is_atom(B) and not nil/true/false ? erlang:'<' : ordering_type_error/3 (BT-5213) |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 11d | `if_else` | 0.92 | 0.00 | 0.00 |
| 11e | `less` | 0.79 | 0.00 | 0.00 |
| 11h | `lit_less` | 0.63 | 0.00 | 0.00 |
| 11i | `less_ab` | 1.10 | 0.00 | 0.00 |
| 11k | `lit_add` | 0.65 | 0.00 | 0.00 |
| 11m | `lit_str_less` | 11.4 | 0.00 | 0.00 |
| 11n | `less_ab` | 1.03 | 0.00 | 0.00 |
| 11o | `lit_sym_less` | 3.42 | 0.00 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.45 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.44 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11d | bt | ifTrueIfFalse:a: | 9.45 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11d | ex | if_else | 2.36 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11e | bt | lessTyped:a: | 9.43 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11e | ex | less | 2.23 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11h | bt | litLess:b: | 9.44 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11h | ex | lit_less | 2.07 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11i | bt | untypedLess:a:b: | 9.65 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11i | ex | less_ab | 2.54 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11k | bt | litAdd:b: | 10.7 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11k | ex | lit_add | 2.09 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11m | bt | litStrLess:b: | 16.7 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11m | ex | lit_str_less | 12.8 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11n | bt | untypedStrLess:a:b: | 99.3 | 13.0 | 2.00 | 13.0 | 2.00 |
| 11n | ex | less_ab | 2.48 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11o | bt | litSymLess:b: | 98.1 | 10.0 | 2.00 | 10.0 | 2.00 |
| 11o | ex | lit_sym_less | 4.86 | 1.00 | 0.00 | 1.00 | 0.00 |
