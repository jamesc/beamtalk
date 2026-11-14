# OTP report draft: `erlc +from_core` fails with "Internal consistency check failed" on a boolean-only `case`

**Status:** drafted, not filed. The repository owner files it upstream
(<https://github.com/erlang/otp/issues>); do not file from an agent session.
Tracked in BT-4787; root cause found in BT-4202 (PR #5087).

## Summary

Compiling Core Erlang with `erlc +from_core` crashes the compiler's back end
(reported as `Internal consistency check failed - please report this bug.`,
instruction `{jump,{f,0}}`) for a `case` that has no catch-all clause, when its
scrutinee is itself a `case` with one branch that provably matches neither of
the outer clauses. In larger programs the same family of crash surfaces as
`beam_clean ... undefined_label`.

Adding one catch-all clause to the outer `case` makes the module compile.

## Environment

* OTP 28.5 (`erlang:system_info(otp_release)` = `28`, erts 16.4), Linux x86_64.
* Only OTP 28 was tried. OTP 27 (the other version in `otp-support.toml`) has
  **not** been checked; the filer should try it before reporting.

## Steps to reproduce

```
cp q1.core.txt q1.core && erlc +from_core q1.core
```

`q1.core.txt` is in this directory (11 lines of Core Erlang, no Beamtalk involved; copy it to `q1.core` first, because the repo ignores `*.core`):

```erlang
module 'q1' ['run'/1]
  attributes []
'run'/1 = fun (Self) ->
  case case call 'foo':'bar'() of
         <'true'> when 'true' -> 'a'
         <N> when 'true' -> call 'foo':'send'(1)
       end of
    <'false'> when 'true' -> 'k'
    <'true'> when 'true' -> 'nil'
  end
end
```

## Expected

The module compiles. At worst, the compiler reports an ordinary error or warning
about the unmatched value `'a'`. A `case` without a catch-all is valid Core
Erlang, and failing to match is a runtime `case_clause` error.

## Actual

```
q1:1: function run/1+9:
  Internal consistency check failed - please report this bug.
  Instruction: {jump,{f,0}}
  Error:       throws_exception:
```

(exit status 1, no `.beam` written)

## Workaround and control

`q1_fixed.core.txt` is identical except for one extra clause on the outer `case`:

```erlang
<M> when 'true' -> call 'erlang':'error'({'case_clause', M})
```

after copying it to `q1_fixed.core`, `erlc +from_core q1_fixed.core` succeeds. Binding the inner value to a variable
first does not avoid the crash, and replacing the inner `case` with a constant,
or the scrutinee `call` with a constant, also compiles. That points at the
optimiser seeing that the `'a'` branch can only fail, then leaving a jump to a
label that no longer exists.

## Why it matters to us

Beamtalk emits this shape for `whileTrue:`/`whileFalse:` loops whose condition
tail is an inlined pure conditional. The repository's `case_clause_fallback`
helper (`crates/beamtalk-codegen/src/core_erlang/util.rs`) adds the explicit
catch-all for this reason, so we are not blocked.
