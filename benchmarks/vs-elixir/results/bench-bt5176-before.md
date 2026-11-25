# BT-5176 Phase 0: number-on-the-left comparison baseline

The "before" numbers for the ADR 0116 comparison addendum (BT-5176). Phase 2
changes the codegen for `<`, `<=`, `>`, `>=` so a number on the left tests the
right operand. It compares against this file. It also records rows 11d–11g,
which were in the harness but in no committed results file.

## How it was measured

- Commit `fd0d5d5` (main) plus BT-5207: the new rows only, no codegen or runtime change.
- Compiler: the installed `beamtalk` 0.6.0-edge+c71a6f9 (2026-11-25), whose sysroot supplies the runtime and stdlib, not a repo dev build. Phase 2 should re-measure with a build of the same commit's codegen, and re-run this file's command on the unchanged base if it uses a different compiler, so the comparison is like for like.
- Machine: a 4-vCPU cloud container (Intel Xeon @ 2.10 GHz, x86_64, no pinning), OTP 28 (erts 16.4), Elixir 1.20.1, 4 schedulers. Load average 2–4 at start (the first run follows the container's cold build).
- `just bench-vs-elixir bench 7 11d,11e,11f,11g,11h,11i,11j,11k,11l`, run **three times** back to back (7 interleaved rounds each; each number is the median of its 7 rounds, net of the side's loop baseline). Run 2 is reproduced in full below; the three runs' ns are in the summary.
- Reductions and heap words are deterministic (identical across runs). Time is noisy: rows whose cost is under ~1 ns net sit at the noise floor (negative or sub-ns Elixir values are noise). Row 11l (an actor call over a gen_server) is noisy on a shared machine: 4.6–6.1 µs across the three runs. Compare phases on reductions and words, and on ns only where the change is several ns or more.

## The rows

`b` is a method argument (the harness passes the integer 5 for both operands of 11i/11j on each side), so the compiler cannot prove it numeric. These rows therefore measure the arm the addendum changes.

| Row | Beamtalk | Compiles to today |
|---|---|---|
| 11h | `5 < b` (literal receiver, untyped `b`) | bare `erlang:'<'(5, B)`: no test of `B`, so a non-number `B` is compared in Erlang term order (the bug the addendum fixes) |
| 11i | `a < b`, both untyped | `is_number`/`is_binary` guard on `A`, then `erlang:'<'(A, B)`, else `send/3`; `B` is never tested |
| 11j | `a :: Integer < b`, `b` untyped | `is_number` guard on `A`, `erlang:'<'(A, B)`, else `send/3`; `B` is never tested |
| 11k | `5 + b`, `b` untyped (arithmetic reference) | `erlang:'+'(5, B)` in a try; on `badarith` an `is_number(B)` test, then `send_number_coercion/4` |
| 11l | actor `spin: k`: `self.count := k. [0 < self.count] whileTrue: [self.count := self.count - 1]` | a `StateAcc` letrec, not the hybrid shape (BT-4121): the whole state map is threaded, one `maps:put` per iteration, condition bare `erlang:'<'(0, maps:get(count, StateAcc))`. Five iterations per call. Phase 2 reports before and after on this row. The Elixir twin threads a one-key map written each pass, so the work is comparable. |

## Summary (ns across the three runs)

| Row | Operation | BT ns (run 1 / 2 / 3) | BT red | BT words | EX ns (run 1 / 2 / 3) | EX red | EX words |
|---|---|---|--:|--:|---|--:|--:|
| 11d | a > 0 ifTrue: [1] ifFalse: [2] / if | 7.45 / 10.1 / 8.67 | 0.00 | 0.00 | 0.89 / 1.66 / 0.47 | 0.00 | 0.00 |
| 11e | a < 10, a :: Integer / a < 10 | 7.59 / 7.73 / 9.78 | 0.00 | 0.00 | 0.70 / 1.41 / 0.89 | 0.00 | 0.00 |
| 11f | [a > i] whileTrue:, a :: Integer / recursion | 285 / 300 / 289 | 19.0 | 7.00 | 13.6 / 16.4 / 16.2 | 7.00 | 0.00 |
| 11g | a max: 3, a :: Integer / max(a, 3) | 21.9 / 24.5 / 25.3 | 4.00 | 0.00 | 0.31 / 0.17 / 0.53 | 0.00 | 0.00 |
| 11h | 5 < b, b untyped / 5 < b | 0.31 / 1.26 / 0.84 | 0.00 | 0.00 | 0.81 / 1.11 / 0.50 | 0.00 | 0.00 |
| 11i | a < b, both untyped / a < b | 10.5 / 14.7 / 13.4 | 0.00 | 0.00 | 1.08 / 1.82 / 2.54 | 0.00 | 0.00 |
| 11j | a < b, a :: Integer, b untyped / a < b | 8.83 / 9.28 / 9.20 | 0.00 | 0.00 | 1.14 / 0.34 / 1.56 | 0.00 | 0.00 |
| 11k | 5 + b, b untyped / 5 + b | 9.10 / 10.9 / 9.46 | 0.00 | 0.00 | -0.00 / 0.03 / 1.17 | 0.00 | 0.00 |
| 11l | actor method [0 < self.n] whileTrue: writing a field / GenServer.call | 6091 / 4597 / 5201 | 112 | 187 | 2245 / 2524 / 2640 | 45.1 | 80.0 |

## Run 2 in full

# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 08:56:16 up 16 min,  0 user,  load average: 0.98, 2.19, 1.82
# rounds 7
# load average after: 08:56:22 up 16 min,  0 user,  load average: 0.98, 2.17, 1.82

### Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 11d | a > 0 ifTrue: [1] ifFalse: [2] / if | 10.1 | 0.00 | 0.00 | 1.66 | 0.00 | 0.00 | 6.1 | - | - | 0.00 | 0.00 | send(Bool, 'ifTrue:ifFalse:', [fun, fun]) -> True:dispatch/3 |
| 11e | a < 10, a :: Integer / a < 10 | 7.73 | 0.00 | 0.00 | 1.41 | 0.00 | 0.00 | 5.5 | - | - | 0.00 | 0.00 | is_number guard (declared Integer, BT-4823), erlang:'<' |
| 11f | [a > i] whileTrue:, a :: Integer / recursion | 300 | 19.0 | 7.00 | 16.4 | 7.00 | 0.00 | 18.3 | 2.7 | - | 12.0 | 7.00 | letrec loop; is_number guard on the typed limit (BT-4823), erlang:'>' |
| 11g | a max: 3, a :: Integer / max(a, 3) | 24.5 | 4.00 | 0.00 | 0.17 | 0.00 | 0.00 | - | - | - | 4.00 | 0.00 | Number>>max: (self guarded: Number is not a proven class), erlang:'>' |
| 11h | 5 < b, b untyped / 5 < b | 1.26 | 0.00 | 0.00 | 1.11 | 0.00 | 0.00 | 1.1 | - | - | 0.00 | 0.00 | bare erlang:'<'(5, B): the proven number receiver gets no guard, so a non-number B is compared in Erlang term order (ADR 0116 addendum, the failing arm) |
| 11i | a < b, both untyped / a < b | 14.7 | 0.00 | 0.00 | 1.82 | 0.00 | 0.00 | 8.1 | - | - | 0.00 | 0.00 | is_number/is_binary guard on A, erlang:'<'(A, B); else send(A, '<', [B]); B is never tested |
| 11j | a < b, a :: Integer, b untyped / a < b | 9.28 | 0.00 | 0.00 | 0.34 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | as 11e, with B untyped: is_number guard on the declared-Integer A, erlang:'<'(A, B); B is never tested |
| 11k | 5 + b, b untyped / 5 + b | 10.9 | 0.00 | 0.00 | 0.03 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | erlang:'+'(5, B) in a try; on badarith an is_number(B) test then send_number_coercion/4 (the arithmetic reference the comparison addendum matches) |
| 11l | actor method [0 < self.n] whileTrue: writing a field / GenServer.call | 4597 | 112 | 187 | 2524 | 45.1 | 80.0 | 1.8 | 2.5 | 2.3 | 67.2 | 107 | as 7a; in the actor the loop is a StateAcc letrec (whole state map threaded, one maps:put per iteration), condition bare erlang:'<'(0, maps:get(count, StateAcc)); the StateAcc shape, not the hybrid one (BT-4121) |

### Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 11d | `if_else` | 1.66 | 0.00 | 0.00 |
| 11e | `less` | 1.41 | 0.00 | 0.00 |
| 11f | `while_to` | 16.4 | 7.00 | 0.00 |
| 11g | `max_of` | 0.17 | 0.00 | 0.00 |
| 11h | `lit_less` | 1.11 | 0.00 | 0.00 |
| 11i | `less_ab` | 1.82 | 0.00 | 0.00 |
| 11j | `less_ab` | 0.34 | 0.00 | 0.00 |
| 11k | `lit_add` | 0.03 | 0.00 | 0.00 |
| 11l | `gen_spin` | 2524 | 45.1 | 80.0 |

### Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.59 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 2.69 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11d | bt | ifTrueIfFalse:a: | 11.7 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11d | ex | if_else | 4.35 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11e | bt | lessTyped:a: | 9.32 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11e | ex | less | 4.09 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11f | bt | whileTyped:a: | 301 | 20.0 | 7.00 | 20.0 | 7.00 |
| 11f | ex | while_to | 19.1 | 8.00 | 0.00 | 8.00 | 0.00 |
| 11g | bt | maxTyped:a: | 26.1 | 5.00 | 0.00 | 5.00 | 0.00 |
| 11g | ex | max_of | 2.86 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11h | bt | litLess:b: | 2.84 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11h | ex | lit_less | 3.80 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11i | bt | untypedLess:a:b: | 16.3 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11i | ex | less_ab | 4.50 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11j | bt | typedLess:a:b: | 10.9 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11j | ex | less_ab | 3.03 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11k | bt | litAdd:b: | 12.5 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11k | ex | lit_add | 2.72 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11l | bt | actorSpin:c: | 4598 | 113 | 187 | 44.1 | 83.0 |
| 11l | ex | gen_spin | 2527 | 46.1 | 80.0 | 17.1 | 33.0 |
