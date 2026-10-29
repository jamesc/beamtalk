# BT-4298: class-side dispatch baseline (ADR 0142 Phase 0)

The starting point for epic BT-4297 (ADR 0142: the class process only owns
class state). It records the class-side rows that the epic's children are
measured against. On main every non-`class sealed` class method is a
`gen_server:call` into the class process. BT-4300 runs dynamic class sends to
stateless classes in the caller, and BT-4301 does the same for literal-receiver
sends. ADR 0142's Phase 1 gate is rows 3b and 3c at 10 reductions or fewer,
with no `'$gen_call'` in the profile.

Measured at `f265a0c` (main) plus BT-4298, with the dev build of the compiler
(`target/debug/beamtalk`) and the dev runtime tree. OTP 28 (erts 16.4),
Elixir 1.20.1, 4 schedulers, load average about 2.0. As in the README,
reductions and words are deterministic. Time is noisy on a shared machine.
This host measures the class-process hop at about 15 µs, against the
README's 2.3 µs, so compare ns only within one run.

## Rows

`run.sh bench 5 3a,3b,3c,3d,3e,3f`. Rows 3d, 3e and 3f are new in BT-4298.
3a (a `class sealed` method of a sealed, stateless class, direct-called by
ADR 0129 Phase 0b) is the target. 3b and 3c already existed.

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | +red | +words | Beamtalk compiles to | ADR 0142 |
|---|---|--:|--:|--:|--:|--:|--:|--:|---|---|
| 3a | `class sealed` method, sealed class / Mod.fun(arg) | 8.56 | 2.00 | 0.00 | 3.68 | 2.00 | 0.00 | 0.00 | direct call `Mod:'class_double:'(nil, 21)` | (target) |
| 3b | class method, sealed class / Mod.fun(arg) | 15088 | 203 | 469 | 3.80 | 2.00 | 201 | 469 | whereis_class + class_send, gen_server:call | BT-4300 (dynamic), BT-4301 (static) |
| 3c | class method, open class / Mod.fun(arg) | 21108 | 206 | 518 | 4.37 | 2.00 | 204 | 518 | whereis_class + class_send, gen_server:call | BT-4300, BT-4301 |
| 3d | class method, class in a variable / mod.fun(arg) | 15017 | 208 | 518 | 22.2 | 2.00 | 206 | 518 | `send(C, 'double:', [21])` to class_send, gen_server:call | BT-4300 |
| 3e | inherited class method, open stateless subclass / Mod.fun(arg) | 17030 | 247 | 651 | 3.33 | 2.00 | 245 | 651 | whereis_class + class_send into the subclass's process, inherited lookup there | BT-4300, BT-4301 |
| 3f | class method, class with `classState:` / Mod.fun(arg) | 15094 | 204 | 562 | 4.01 | 2.00 | 202 | 562 | whereis_class + class_send, gen_server:call | unchanged (control: the class has class variables) |

3d's Elixir comparison is `mod.double(21)` with the module in a variable
(22.2 ns, 2 red). The static `Math.double(21)` is 4.04 ns.

About half of each hop's reductions and words are spent in the class process
(the "caller red / caller words" columns of the raw table): 3b 99 of 204 red
in the caller, 3c 99 of 207, 3d 103 of 209, 3e 120 of 248, 3f 99 of 205. The
inherited send (3e) costs about 40 red and 130 words more than an own method
(3c), for the hierarchy walk in the subclass's process.

## Differential fixture

The class-side rows of the differential send fixture are
`stdlib/test/class_dispatch_differential_test.bt`. It found two existing
behaviours that ADR 0142 should not inherit by accident:

- **BT-4307.** A class-side extension on an ancestor is not reached by an
  external send to a subclass. An extension-only selector raises
  `does_not_understand`, and an extension that shadows a compiled ancestor
  method is bypassed. An ADR 0084 runtime fun on an ancestor *is* reached.
  ADR 0142 §2 assumes both are. The fixture pins today's outcome.
- **BT-4308.** `TestCase run:` and `runAll` answer a raw binary although they
  declare `String`.
