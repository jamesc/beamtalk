# BT-5176 Phase 2b: number-on-the-left comparison, after (BT-5210)

The "after" numbers for the inlined path of the ADR 0116 comparison addendum
(BT-5176, Phase 2b, BT-5210), against the Phase 0 baseline in
`bench-bt5176-before.md`. Phase 2b tests the right operand of an inlined
`<`, `<=`, `>`, `>=` whose left operand may be a number, unless the
compiler proves the right operand numeric, and sends a non-number its
reflected hook (`lessThanFromNumber:` …) through
`send_number_comparison/4`.

## How it was measured

- Like for like, as the baseline asks: both sides are repo dev builds on the
  same machine, interleaved. **base** is `main` at `7e456e5` (Phase 2a), built in
  a separate worktree; **after** is this branch. Each compiler's sysroot
  supplies its own runtime and stdlib (so `Number>>max:` in 11g is compiled
  by the compiler under test).
- Machine: a 4-vCPU cloud container (x86_64, no pinning), OTP 28 (erts 16.4),
  Elixir 1.20.1, 4 schedulers.
- `BEAMTALK=<compiler> just bench-vs-elixir bench 7 11d,11e,11f,11g,11h,11i,11j,11k,11l`,
  run three times per side, alternating base / after (7 interleaved rounds
  each; each number is the median of its 7 rounds, net of the side's loop
  baseline). After-run 2 is reproduced in full below.
- Reductions and heap words are deterministic except 11l's words (an actor
  call; 181–189 across the base runs alone). Rows under ~1 ns net sit at the
  noise floor.

## Generated code

Compared per method (`bt@vs_elixir_bench@vb_bench.core`, temporaries
renamed away):

| Row | Method | base → after |
|---|---|---|
| 11d | `ifTrueIfFalse:a:` (`a > 0`) | identical (right operand a literal: proven) |
| 11e | `lessTyped:a:` (`a < 10`) | identical (proven) |
| 11f | `whileTyped:a:` (`[a > i] whileTrue:`) | the number arm of the receiver guard gains `case is_number(I)` |
| 11g | `maxTyped:a:` | identical; the change is in `Number>>max:` (`self > other`, `other` declared `Number`: tested) |
| 11h | `litLess:b:` (`5 < b`) | bare `erlang:'<'(5, B)` → `case is_number(B)`: BIF / `send_number_comparison(B, 'lessThanFromNumber:', [5], '<')` |
| 11i | `untypedLess:a:b:` | split by kind: `is_number(A)` → `is_number(B)` → BIF / hook; else `is_binary(A)` → BIF / `send/3` |
| 11j | `typedLess:a:b:` | `is_number(A)` true arm gains `is_number(B)` → BIF / hook |
| 11k | `litAdd:b:` | identical (arithmetic reference) |

**11l loop shape** (`VbCounter>>spin:`, `[0 < self.count] whileTrue: [self.count := self.count - 1]`):

- base: a `StateAcc` letrec (already, see the baseline); the condition closure
  answers the bare `erlang:'<'(0, maps:get(count, StateAcc))`.
- after: still a `StateAcc` letrec. `0 < self.count` is now a guarded
  operator (its right operand is not proven), so the condition closure answers
  `{Bool, StateAcc}` and the comparison is a BT-4279 fast-path bracket:
  `is_number(Count)` takes the BIF with no store; otherwise the state is
  stored, the hook sent, the state taken back. The method gains the exit
  store of a method that brackets. No hybrid → `StateAcc` move happens here,
  because the row was already `StateAcc`.

## Summary (BT ns, three runs per side)

| Row | Operation | base ns (1 / 2 / 3) | after ns (1 / 2 / 3) | base red / words | after red / words |
|---|---|---|---|--:|--:|
| 11d | a > 0 ifTrue: [1] ifFalse: [2] / if | 8.65 / 7.67 / 8.25 | 8.00 / 8.82 / 8.21 | 0.00 / 0.00 | 0.00 / 0.00 |
| 11e | a < 10, a :: Integer / a < 10 | 8.39 / 8.31 / 7.53 | 7.72 / 7.69 / 7.97 | 0.00 / 0.00 | 0.00 / 0.00 |
| 11f | [a > i] whileTrue:, a :: Integer / recursion | 267 / 246 / 263 | 254 / 259 / 273 | 19.0 / 7.00 | 19.0 / 7.00 |
| 11g | a max: 3, a :: Integer / max(a, 3) | 19.3 / 23.5 / 19.7 | 17.0 / 17.0 / 17.0 | 4.00 / 0.00 | 4.00 / 0.00 |
| 11h | 5 < b, b untyped / 5 < b | 0.46 / 1.05 / 0.70 | 7.68 / 7.75 / 7.77 | 0.00 / 0.00 | 0.00 / 0.00 |
| 11i | a < b, both untyped / a < b | 10.9 / 10.6 / 11.0 | 7.74 / 7.97 / 7.96 | 0.00 / 0.00 | 0.00 / 0.00 |
| 11j | a < b, a :: Integer, b untyped / a < b | 7.80 / 7.62 / 8.21 | 8.29 / 7.70 / 8.09 | 0.00 / 0.00 | 0.00 / 0.00 |
| 11k | 5 + b, b untyped / 5 + b | 10.2 / 8.83 / 9.41 | 8.87 / 9.38 / 9.54 | 0.00 / 0.00 | 0.00 / 0.00 |
| 11l | actor method [0 < self.n] whileTrue: writing a field / GenServer.call | 4912 / 4171 / 3692 | 3607 / 3667 / 3583 | 112 / 181 | 102 / 187 |

11l's reductions are 113 / 112 / 113 (base) and 102 / 102 / 102 (after);
its words 189 / 181 / 185 and 189 / 187 / 187. Every other row's reductions
and words are the same in all six runs.

## The gate (ADR 0116 § Addendum, Phase 2 acceptance)

| Check | Result |
|---|---|
| 11d, 11e (proven right operand) compile to the same code; reductions, words, ns unchanged | **Pass.** Identical Core Erlang; 0 / 0 both sides; ns 7.5–8.8 on both sides. |
| 11f, 11g, 11i, 11j gain one `is_number` test: reductions and words identical, ns inside the run-to-run spread | **Pass.** 11f 19 / 7 both sides, ns 246–267 → 254–273. 11g 4 / 0, ns 19.3–23.5 → 17.0. 11i 0 / 0, ns 10.6–11.0 → 7.7–8.0. 11j 0 / 0, ns 7.6–8.2 → 7.7–8.3. None is slower outside the spread. |
| The new `5 < b` row (11h) is no slower than the `5 + b` row (11k) | **Pass.** 11h 7.68 / 7.75 / 7.77 ns; 11k 8.87 / 9.38 / 9.54 ns in the same runs. 11h itself goes from the noise floor (bare BIF, 0.5–1.1 ns) to ~7.7 ns: an explicit regression of literal-receiver sites with an unproven right operand. The cost is not the hook (here `b` is a number and the hook never runs) but the stack frame a `case` arm that makes a call gives the whole site, as the addendum predicted (§ The failing arm) and the owner accepted (decision 1). |
| The actor-loop row (11l) reported with its before and after loop shape | Reported above. Both sides are `StateAcc`; after adds the fast-path bracket in the condition. It measured **faster**, not slower: 3583–3667 ns against 3692–4912, and 102 reductions against 112–113. No regression to take to the owner. |

## After-run 2 in full

The "Beamtalk compiles to" column of rows 11f–11l was rewritten after the
run to the right-tested lowering, matching `compiles_to/1` in
`harness/vs_bench.erl` as updated in the same change; the numbers are as
measured.

# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 13:19:35 up  1:25,  0 user,  load average: 2.14, 5.39, 4.90
# rounds 7
# load average after: 13:19:40 up  1:26,  0 user,  load average: 2.04, 5.31, 4.88

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 11d | a > 0 ifTrue: [1] ifFalse: [2] / if | 8.82 | 0.00 | 0.00 | 1.53 | 0.00 | 0.00 | 5.8 | - | - | 0.00 | 0.00 | send(Bool, 'ifTrue:ifFalse:', [fun, fun]) -> True:dispatch/3 |
| 11e | a < 10, a :: Integer / a < 10 | 7.69 | 0.00 | 0.00 | 0.87 | 0.00 | 0.00 | 8.8 | - | - | 0.00 | 0.00 | is_number guard (declared Integer, BT-4823), erlang:'<' |
| 11f | [a > i] whileTrue:, a :: Integer / recursion | 259 | 19.0 | 7.00 | 12.2 | 7.00 | 0.00 | 21.2 | 2.7 | - | 12.0 | 7.00 | letrec loop; is_number guard on the typed limit (BT-4823), its number arm an is_number(I) test (ADR 0116 addendum, BT-5210), erlang:'>'; else send_number_comparison/4 |
| 11g | a max: 3, a :: Integer / max(a, 3) | 17.0 | 4.00 | 0.00 | 0.80 | 0.00 | 0.00 | 21.2 | - | - | 4.00 | 0.00 | Number>>max: (self guarded: Number is not a proven class; the declared other is tested, BT-5210), erlang:'>' |
| 11h | 5 < b, b untyped / 5 < b | 7.75 | 0.00 | 0.00 | 1.03 | 0.00 | 0.00 | 7.5 | - | - | 0.00 | 0.00 | case is_number(B): erlang:'<'(5, B); else send_number_comparison(B, 'lessThanFromNumber:', [5], '<') (ADR 0116 addendum, BT-5210; the call arm gives the site a frame) |
| 11i | a < b, both untyped / a < b | 7.97 | 0.00 | 0.00 | 1.22 | 0.00 | 0.00 | 6.5 | - | - | 0.00 | 0.00 | is_number(A) -> is_number(B) ? erlang:'<'(A, B) : send_number_comparison/4; else is_binary(A) ? erlang:'<'(A, B) : send(A, '<', [B]) (BT-5210) |
| 11j | a < b, a :: Integer, b untyped / a < b | 7.70 | 0.00 | 0.00 | 1.35 | 0.00 | 0.00 | 5.7 | - | - | 0.00 | 0.00 | as 11e, with B untyped: is_number guard on the declared-Integer A, its true arm is_number(B) ? erlang:'<'(A, B) : send_number_comparison/4 (BT-5210) |
| 11k | 5 + b, b untyped / 5 + b | 9.38 | 0.00 | 0.00 | 0.68 | 0.00 | 0.00 | 13.8 | - | - | 0.00 | 0.00 | erlang:'+'(5, B) in a try; on badarith an is_number(B) test then send_number_coercion/4 (the arithmetic reference the comparison addendum matches) |
| 11l | actor method [0 < self.n] whileTrue: writing a field / GenServer.call | 3667 | 102 | 187 | 2210 | 45.1 | 80.0 | 1.7 | 2.3 | 2.3 | 57.3 | 107 | as 7a; in the actor the loop is a StateAcc letrec (whole state map threaded, one maps:put per iteration); the condition 0 < self.count is a guarded operator (BT-5210): a BT-4279 fast path is_number(Count) -> erlang:'<', else store, send_number_comparison/4, take |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 11d | `if_else` | 1.53 | 0.00 | 0.00 |
| 11e | `less` | 0.87 | 0.00 | 0.00 |
| 11f | `while_to` | 12.2 | 7.00 | 0.00 |
| 11g | `max_of` | 0.80 | 0.00 | 0.00 |
| 11h | `lit_less` | 1.03 | 0.00 | 0.00 |
| 11i | `less_ab` | 1.22 | 0.00 | 0.00 |
| 11j | `less_ab` | 1.35 | 0.00 | 0.00 |
| 11k | `lit_add` | 0.68 | 0.00 | 0.00 |
| 11l | `gen_spin` | 2210 | 45.1 | 80.0 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.60 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.31 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11d | bt | ifTrueIfFalse:a: | 10.4 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11d | ex | if_else | 2.84 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11e | bt | lessTyped:a: | 9.28 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11e | ex | less | 2.19 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11f | bt | whileTyped:a: | 261 | 20.0 | 7.00 | 20.0 | 7.00 |
| 11f | ex | while_to | 13.5 | 8.00 | 0.00 | 8.00 | 0.00 |
| 11g | bt | maxTyped:a: | 18.6 | 5.00 | 0.00 | 5.00 | 0.00 |
| 11g | ex | max_of | 2.11 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11h | bt | litLess:b: | 9.35 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11h | ex | lit_less | 2.34 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11i | bt | untypedLess:a:b: | 9.57 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11i | ex | less_ab | 2.53 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11j | bt | typedLess:a:b: | 9.30 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11j | ex | less_ab | 2.66 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11k | bt | litAdd:b: | 11.0 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11k | ex | lit_add | 1.99 | 1.00 | 0.00 | 1.00 | 0.00 |
| 11l | bt | actorSpin:c: | 3669 | 103 | 187 | 44.1 | 83.0 |
| 11l | ex | gen_spin | 2211 | 46.1 | 80.0 | 17.0 | 33.0 |
