# BT-5176 Phase 0: number-on-the-left comparison baseline

The "before" numbers for the ADR 0116 comparison addendum (BT-5176). Phase 2
changes the codegen for `<`, `<=`, `>`, `>=` so a number on the left tests the
right operand. It compares against this file. It also records rows 11d–11g,
which were in the harness but in no committed results file.

## How it was measured

- Commit `fd0d5d5` (main) plus BT-5207: the new rows only, no codegen or runtime change.
- Compiler: the installed `beamtalk` 0.6.0-edge (`beamtalk --print-sysroot` runtime and stdlib), not a repo dev build.
- Machine: a 4-vCPU cloud container (Intel Xeon @ 2.10 GHz, x86_64, no pinning), OTP 28 (erts 16.4), Elixir 1.20.1, 4 schedulers. Load average 2–4 at start (the first run follows the container's cold build).
- `just bench-vs-elixir bench 7 11d,11e,11f,11g,11h,11i,11j,11k,11l`, run **three times** back to back (7 interleaved rounds each; each number is the median of its 7 rounds, net of the side's loop baseline). Run 2 is reproduced in full below; the three runs' ns are in the summary.
- Reductions and heap words are deterministic (identical across runs, apart from the sub-1 rounding noted below). Time is noisy: rows whose cost is under ~1 ns net sit at the noise floor (negative or sub-ns Elixir values are noise), and one outlier (11l BT, run 1: 6324 ns against 3625–3995) came from a cold first run on a shared machine. Compare phases on reductions and words, and on ns only where the change is several ns or more.

## The rows

`b` is a method argument (the harness passes the same integer 5 for both operands of 11i/11j on the Beamtalk side and `{5, 6}` on the Elixir side), so the compiler cannot prove it numeric. These rows therefore measure the arm the addendum changes.

| Row | Beamtalk | Compiles to today |
|---|---|---|
| 11h | `5 < b` (literal receiver, untyped `b`) | bare `erlang:'<'(5, B)`: no test of `B`, so a non-number `B` is compared in Erlang term order (the bug the addendum fixes) |
| 11i | `a < b`, both untyped | `is_number`/`is_binary` guard on `A`, then `erlang:'<'(A, B)`, else `send/3`; `B` is never tested |
| 11j | `a :: Integer < b`, `b` untyped | `is_number` guard on `A`, `erlang:'<'(A, B)`, else `send/3`; `B` is never tested |
| 11k | `5 + b`, `b` untyped (arithmetic reference) | `erlang:'+'(5, B)` in a try; on `badarith` an `is_number(B)` test, then `send_number_coercion/4` |
| 11l | actor `spin: k`: `self.count := k. [0 < self.count] whileTrue: [self.count := self.count - 1]` | a `StateAcc` letrec, not the hybrid shape (BT-4121): the whole state map is threaded, one `maps:put` per iteration, condition bare `erlang:'<'(0, maps:get(count, StateAcc))`. Five iterations per call. Phase 2 reports before and after on this row. |

## Summary (ns across the three runs)

| Row | Operation | BT ns (run 1 / 2 / 3) | BT red | BT words | EX ns (run 1 / 2 / 3) | EX red | EX words |
|---|---|---|--:|--:|---|--:|--:|
| 11d | a > 0 ifTrue: [1] ifFalse: [2] / if | 9.07 / 9.49 / 7.64 | 0.00 | 0.00 | 1.51 / 0.48 / 0.42 | 0.00 | 0.00 |
| 11e | a < 10, a :: Integer / a < 10 | 8.15 / 7.94 / 8.61 | 0.00 | 0.00 | 0.14 / 0.61 / 1.52 | 0.00 | 0.00 |
| 11f | [a > i] whileTrue:, a :: Integer / recursion | 301 / 268 / 277 | 19.0 | 7.00 | 16.1 / 14.5 / 12.4 | 7.00 | 0.00 |
| 11g | a max: 3, a :: Integer / max(a, 3) | 19.9 / 19.9 / 23.5 | 4.00 | 0.00 | -0.14 / 0.69 / 0.53 | 0.00 | 0.00 |
| 11h | 5 < b, b untyped / 5 < b | 0.53 / 0.41 / 0.82 | 0.00 | 0.00 | 0.16 / 0.38 / 0.26 | 0.00 | 0.00 |
| 11i | a < b, both untyped / a < b | 10.4 / 11.0 / 11.3 | 0.00 | 0.00 | 1.21 / 0.91 / 0.60 | 0.00 | 0.00 |
| 11j | a < b, a :: Integer, b untyped / a < b | 9.48 / 8.70 / 7.92 | 0.00 | 0.00 | 0.74 / 0.85 / 0.50 | 0.00 | 0.00 |
| 11k | 5 + b, b untyped / 5 + b | 9.05 / 10.8 / 9.12 | 0.00 | 0.00 | 0.16 / 0.89 / 0.16 | 0.00 | 0.00 |
| 11l | actor method [0 < self.n] whileTrue: writing a field / GenServer.call | 6324 / 3995 / 3625 | 113 | 189 | 2381 / 2317 / 2053 | 44.1 | 56.0 |
## Run 2 in full

# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 08:44:26 up 4 min,  0 user,  load average: 3.93, 2.62, 1.11
# rounds 7
# load average after: 08:44:32 up 4 min,  0 user,  load average: 3.78, 2.61, 1.12

### Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 11d | a > 0 ifTrue: [1] ifFalse: [2] / if | 9.49 | 0.00 | 0.00 | 0.48 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | send(Bool, 'ifTrue:ifFalse:', [fun, fun]) -> True:dispatch/3 |
| 11e | a < 10, a :: Integer / a < 10 | 7.94 | 0.00 | 0.00 | 0.61 | 0.00 | 0.00 | 13.0 | - | - | 0.00 | 0.00 | is_number guard (declared Integer, BT-4823), erlang:'<' |
| 11f | [a > i] whileTrue:, a :: Integer / recursion | 268 | 19.0 | 7.00 | 14.5 | 7.00 | 0.00 | 18.5 | 2.7 | - | 12.0 | 7.00 | letrec loop; is_number guard on the typed limit (BT-4823), erlang:'>' |
| 11g | a max: 3, a :: Integer / max(a, 3) | 19.9 | 4.00 | 0.00 | 0.69 | 0.00 | 0.00 | 28.7 | - | - | 4.00 | 0.00 | Number>>max: (self guarded: Number is not a proven class), erlang:'>' |
| 11h | 5 < b, b untyped / 5 < b | 0.41 | 0.00 | 0.00 | 0.38 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | bare erlang:'<'(5, B): the proven number receiver gets no guard, so a non-number B is compared in Erlang term order (ADR 0116 addendum, the failing arm) |
| 11i | a < b, both untyped / a < b | 11.0 | 0.00 | 0.00 | 0.91 | 0.00 | 0.00 | 12.1 | - | - | 0.00 | 0.00 | is_number/is_binary guard on A, erlang:'<'(A, B); else send(A, '<', [B]); B is never tested |
| 11j | a < b, a :: Integer, b untyped / a < b | 8.70 | 0.00 | 0.00 | 0.85 | 0.00 | 0.00 | 10.2 | - | - | 0.00 | 0.00 | as 11e, with B untyped: is_number guard on the declared-Integer A, erlang:'<'(A, B); B is never tested |
| 11k | 5 + b, b untyped / 5 + b | 10.8 | 0.00 | 0.00 | 0.89 | 0.00 | 0.00 | 12.1 | - | - | 0.00 | 0.00 | erlang:'+'(5, B) in a try; on badarith an is_number(B) test then send_number_coercion/4 (the arithmetic reference the comparison addendum matches) |
| 11l | actor method [0 < self.n] whileTrue: writing a field / GenServer.call | 3995 | 113 | 189 | 2317 | 44.1 | 56.0 | 1.7 | 2.6 | 3.4 | 68.4 | 133 | as 7a; in the actor the loop is a StateAcc letrec (whole state map threaded, one maps:put per iteration), condition bare erlang:'<'(0, maps:get(count, StateAcc)); the StateAcc shape, not the hybrid one (BT-4121) |

### Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 11d | `if_else` | 0.48 | 0.00 | 0.00 |
| 11e | `less` | 0.61 | 0.00 | 0.00 |
| 11f | `while_to` | 14.5 | 7.00 | 0.00 |
| 11g | `max_of` | 0.69 | 0.00 | 0.00 |
| 11h | `lit_less` | 0.38 | 0.00 | 0.00 |
| 11i | `less_ab` | 0.91 | 0.00 | 0.00 |
| 11j | `less_ab` | 0.85 | 0.00 | 0.00 |
| 11k | `lit_add` | 0.89 | 0.00 | 0.00 |
| 11l | `gen_spin` | 2317 | 44.1 | 56.0 |

### Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.59 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.71 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11d | bt | ifTrueIfFalse:a: | 11.1 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11d | ex | if_else | 2.19 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11e | bt | lessTyped:a: | 9.53 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11e | ex | less | 2.32 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11f | bt | whileTyped:a: | 270 | 20.0 | 7.00 | 20.0 | 7.00 |
| 11f | ex | while_to | 16.2 | 8.00 | 0.00 | 8.00 | 0.00 |
| 11g | bt | maxTyped:a: | 21.5 | 5.00 | 0.00 | 5.00 | 0.00 |
| 11g | ex | max_of | 2.40 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11h | bt | litLess:b: | 2.00 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11h | ex | lit_less | 2.09 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11i | bt | untypedLess:a:b: | 12.6 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11i | ex | less_ab | 2.62 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11j | bt | typedLess:a:b: | 10.3 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11j | ex | less_ab | 2.56 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11k | bt | litAdd:b: | 12.4 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11k | ex | lit_add | 2.60 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11l | bt | actorSpin:c: | 3997 | 114 | 189 | 44.1 | 83.0 |
| 11l | ex | gen_spin | 2318 | 45.1 | 56.0 | 17.1 | 33.0 |
