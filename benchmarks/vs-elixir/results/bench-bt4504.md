# BT-4504: folded class-side constants of every literal kind

BT-4363 folded a class-literal send of a class-side method whose whole body is
one integer literal into the literal itself, behind a `{constant_value, Value,
Module, Fun, Tag}` static call key. BT-4504 extends the fold to every scalar
literal kind. Rows 3k-3m send `VbConstants protocolError` (`=> 1`),
`VbConstants name` (`=> "protocol error"`) and `VbConstants tag`
(`=> #protocolError`); 3a is the guarded direct call they replace.

All three cost 1 reduction (the `persistent_term:get/2` of the class's static
call key) and 0 words, against 3 for the guarded direct call and 2 for an
Elixir remote call. The string row takes about twice the time of the integer
row: its key in the class's static call map is a tuple holding the tagged
binary `{beamtalk_string, <<"protocol error">>}`, which the map match hashes
and compares. The symbol row sits in between.

Command: `BEAMTALK=$PWD/target/debug/beamtalk benchmarks/vs-elixir/run.sh bench 15 3a,3k,3l,3m`
(the compiler's build profile does not change the emitted code).

## Run
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 03:09:10 up 37 min,  0 user,  load average: 0.77, 1.47, 2.19
# rounds 15
# load average after: 03:09:13 up 37 min,  0 user,  load average: 0.77, 1.47, 2.19

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 3a | `class sealed` method, sealed class / Mod.fun(arg) | 19.8 | 3.00 | 0.00 | 2.57 | 2.00 | 0.00 | 7.7 | 1.5 | - | 1.00 | 0.00 | guarded direct call: one persistent_term:get of the class's static call key (an atom), its {Mod, Fun, Tag} target matched in the map to ClassRef (BT-4316, BT-4442), then Mod:'class_double:'(ClassRef, 21) |
| 3k | integer class-side constant: Errs protocolError / Mod.const() | 19.7 | 1.00 | 0.00 | 2.44 | 2.00 | 0.00 | 8.1 | 0.5 | - | -1.00 | 0.00 | folded constant: one persistent_term:get of the class's static call key, its {constant_value, 1, Mod, Fun, Tag} key matched in the map, then the literal 1 in place (BT-4363) |
| 3l | string class-side constant: Errs name / Mod.const() | 38.1 | 1.00 | 0.00 | 2.84 | 2.00 | 0.00 | 13.4 | 0.5 | - | -1.00 | 0.00 | folded constant, as 3k: the key and the hit arm carry the tagged {'beamtalk_string', <<...>>} term (BT-4504) |
| 3m | symbol class-side constant: Errs tag / Mod.const() | 24.6 | 1.00 | 0.00 | 3.00 | 2.00 | 0.00 | 8.2 | 0.5 | - | -1.00 | 0.00 | folded constant, as 3k: the key and the hit arm carry the atom (BT-4504) |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 3a | `remote_call` | 2.57 | 2.00 | 0.00 |
| 3k | `remote_const_int` | 2.44 | 2.00 | 0.00 |
| 3k | `module_attr_int` | 0.02 | 0.00 | 0.00 |
| 3l | `remote_const_string` | 2.84 | 2.00 | 0.00 |
| 3l | `module_attr_string` | 0.12 | 0.00 | 0.00 |
| 3m | `remote_const_atom` | 3.00 | 2.00 | 0.00 |
| 3m | `module_attr_atom` | 0.24 | 0.00 | 0.00 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 2.17 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 2.08 | 1.00 | 0.00 | 1.00 | 0.00 |
| 3a | bt | classSealedMethod: | 21.9 | 4.00 | 0.00 | 4.00 | 0.00 |
| 3a | ex | remote_call | 4.64 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3k | bt | intConstant: | 21.8 | 2.00 | 0.00 | 2.00 | 0.00 |
| 3k | ex | module_attr_int | 2.10 | 1.00 | 0.00 | 1.00 | 0.00 |
| 3k | ex | remote_const_int | 4.52 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3l | bt | stringConstant: | 40.2 | 2.00 | 0.00 | 2.00 | 0.00 |
| 3l | ex | module_attr_string | 2.20 | 1.00 | 0.00 | 1.00 | 0.00 |
| 3l | ex | remote_const_string | 4.91 | 3.00 | 0.00 | 3.00 | 0.00 |
| 3m | bt | symbolConstant: | 26.7 | 2.00 | 0.00 | 2.00 | 0.00 |
| 3m | ex | module_attr_atom | 2.32 | 1.00 | 0.00 | 1.00 | 0.00 |
| 3m | ex | remote_const_atom | 5.08 | 3.00 | 0.00 | 3.00 | 0.00 |
