# BT-4270: class-literal signal sends raise in the caller behind a run-time key

Rows 8a and 8b, `run.sh bench 7 8a,8b`, dev build, OTP 28, one machine. Row
8b is `[Error signal: "boom"] on: Error do: [...]` 10,000 times. Three builds:

| 8b, net of loop | ns/op | red/op | words/op |
|---|--:|--:|--:|
| main (6f1451b): BT-4115's unguarded inline | 1442 | 86.4 | 151 |
| inline removed: the plain ADR 0142 dynamic path | 7809 | 155 | 348 |
| **this change: inline behind its caller-safe raise key** | 3057 | **89.3** | **141** |

ns figures are noisy on this machine (other jobs ran); reductions and words
are the comparison. Row 8a (no raise) is 5.00 reductions in all three.

The dynamic path's cost is its raise relay: `run_in_caller` catches the raise
inside `Exception class >> signal:`, then `unwrap_class_call`,
`outcome_reply`, `log_relayed_raise` and `reraise` raise it again in the
caller, which captures the stack trace a second time.

With the key, the hit path is one `persistent_term:get/2` and then the
primitive itself (`beamtalk_exception_handler:class_signal_message/2` with
the class name). The words fall below main because `strip_class_suffix/1`
(run by every `on:do:` match) now uses `class_display_name/1`'s BIFs instead
of building and scanning a list.

## Raw output (this change)

# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 19:04:28 up 34 min,  0 user,  load average: 2.59, 1.66, 1.41
# rounds 7
# load average after: 19:04:29 up 34 min,  0 user,  load average: 2.59, 1.66, 1.41

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 8a | on: Error do: without a raise / try-rescue | 71.8 | 5.00 | 0.00 | 3.01 | 0.00 | 0.00 | 23.8 | - | - | 5.00 | 0.00 | `Error` class ref built per entry (whereis_class + module_name gen_server:call), class_vars snapshot, try |
| 8b | on: Error do: with a raise / raise-rescue | 3057 | 89.3 | 141 | 676 | 13.1 | 36.0 | 4.5 | 6.8 | 3.9 | 76.2 | 105 | as 8a + `Error signal:` raised in the caller (class_signal_message/2, BT-4115), wrapped and class-matched; frames built only on read |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 8a | `try_no_raise` | 3.01 | 0.00 | 0.00 |
| 8b | `try_raise` | 676 | 13.1 | 36.0 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 2.08 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 2.03 | 1.00 | 0.00 | 1.00 | 0.00 |
| 8a | bt | onDoNoRaise: | 73.9 | 6.00 | 0.00 | 6.00 | 0.00 |
| 8a | ex | try_no_raise | 5.04 | 1.00 | 0.00 | 1.00 | 0.00 |
| 8b | bt | onDoRaise: | 3060 | 90.3 | 141 | 90.3 | 141 |
| 8b | ex | try_raise | 678 | 14.1 | 36.0 | 14.1 | 36.0 |
