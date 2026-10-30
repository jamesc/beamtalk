# BT-4301: static class sends compile to the guarded direct call (ADR 0142 Phase 2a)

Before and after for the class-side rows. BT-4301 changes codegen: a
literal-receiver class send to a class method the class itself defines, and
whose chain declares no class variables, compiles to

```erlang
let ClassPid = call 'erlang':'whereis'('beamtalk_class_VbOpenMath') in
case call 'beamtalk_class_shadow_flags':'direct_call_ok'('VbOpenMath class', 'VbOpenMath') of
  <'true'> when call 'erlang':'is_pid'(ClassPid) ->
    try call 'bt@vs_elixir_bench@vb_open_math':'class_double:'(
            {'beamtalk_object', 'VbOpenMath class', 'bt@vs_elixir_bench@vb_open_math', ClassPid}, 21)
    of <R> -> R
    catch <C, E, S> -> %% an `undef` of this very function (the method was
                       %% removed) takes the class_send below; anything else
                       %% is re-raised
  <_> when 'true' ->
    call 'beamtalk_object_class':'class_send'(ClassPid, 'double:', [21])
end
```

The `try` costs no reductions; the numbers below include it.

`sealed` and `class sealed` play no part (rows 3a, 3b and 3c compile the same).
The ADR 0129 static direct call of row 3a was unguarded and passed `nil` as
`ClassSelf`; it now carries the same guard and passes the real class
reference. Inherited methods (3e) and dynamic sends (3d) keep BT-4300's
dynamic path.

Measured with the dev build of the compiler (`target/debug/beamtalk`) and the
dev runtime tree. OTP 28 (erts 16.4), Elixir 1.20.1, 4 schedulers.
`run.sh bench N 3a,3b,3c,3d,3e,3f` (before: 3 rounds, after: 5). Reductions
and words are deterministic; time is noisy on this shared machine (load
average 1.0 to 1.9 across the runs), so compare ns only within one run.

## Rows (net of the loop baseline)

| Row | Operation | before red | before words | before ns | after red | after words | after ns |
|---|---|--:|--:|--:|--:|--:|--:|
| 3a | `class sealed` method, sealed class | 2.00 | 0 | 7.04 | 6.03 | 5 | 265 |
| 3b | class method, sealed class, not `class sealed` | 24.3 | 41 | 2361 | 6.03 | 5 | 281 |
| 3c | class method, open class | 24.3 | 42 | 2362 | 6.02 | 5 | 290 |
| 3d | class method, class in a variable (dynamic send) | 28.1 | 42 | 1129 | 28.1 | 42 | 828 |
| 3e | inherited class method, open stateless subclass | 32.2 | 42 | 2493 | 32.1 | 42 | 1505 |
| 3f | class method, class with `classState:` (control) | 214 | 672 | 19386 | 204 | 590 | 16336 |

Rows 3b and 3c meet the ADR 0142 gate (≤ 10 reductions for literal-receiver
sends). Row 3a does not keep its ≤ 3: the guard (`direct_call_ok/2`, one
`persistent_term` read) and the class reference (`erlang:whereis/1` of the
compile-time registered name, plus the 5-word `#beamtalk_object{}` tuple) are
what the old unguarded `Mod:'class_double:'(nil, 21)` skipped. They are the
price of "the guard is the contract" (ADR 0025 amendment) and of retiring the
`nil` `ClassSelf` (BT-4037).

## Where the reductions go

`run.sh profile 3a` (3b and 3c are identical), 5 calls per send, 1 of them the
loop:

```
    1.00  persistent_term:get/2                         } the guard
    1.00  beamtalk_class_shadow_flags:direct_call_ok/2  }
    1.00  erlang:whereis/1                              the class reference's pid
    1.00  bt@vs_elixir_bench@vb_sealed_math:class_double:/2
```

Before BT-4301, 3b and 3c also paid the call site's `whereis_class/1`
(`registry_name/1`'s `atom_to_binary` + `binary_to_atom`, which dominated their
time) and the `beamtalk_object_class:class_send/3` wrapper plus BT-4300's
in-caller hop (the owner memo and the liveness check). The guarded call looks
the pid up by the registered name built at compile time
(`util::class_registered_name`, pinned to `beamtalk_class_registry:registry_name/1`
by the shared fixture `class_registered_name_conformance.json`), the form
BT-4295 proposes for every class reference. A first cut that kept
`whereis_class/1` measured 12.1 reductions and about 690 ns for 3a to 3c.
