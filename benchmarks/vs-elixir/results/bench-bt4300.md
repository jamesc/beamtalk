# BT-4300: dynamic class sends run stateless class methods in the caller (ADR 0142 Phase 1b)

Before and after for the class-side rows of the BT-4298 baseline
(`bench-bt4298-baseline.md`). BT-4300 changes `beamtalk_class_dispatch:class_send/3`:
a class-side send to a class whose chain declares no class variables, and that
nothing shadows, runs the class method in the caller through the
`{class_method_owner, ClassPid, Selector}` memo, instead of a `gen_server:call`
into the class process. A literal-receiver send that is not an ADR 0129 direct
call still compiles to `whereis_class` + `class_send`, so rows 3b, 3c and 3e go
through the same path as the dynamic send (3d).

Measured with the dev build of the compiler (`target/debug/beamtalk`) and the
dev runtime tree. OTP 28 (erts 16.4), Elixir 1.20.1, 4 schedulers.
`run.sh bench 5 3a,3b,3c,3d,3e,3f` (before: 3 rounds). Reductions and words are
deterministic; time is noisy on this shared machine (load average 1.4 to 6.6
across the runs), so compare ns only within one run.

## Rows (net of the loop baseline)

| Row | Operation | before red | before words | before ns | after red | after words | after ns |
|---|---|--:|--:|--:|--:|--:|--:|
| 3a | `class sealed` method, sealed class (ADR 0129 direct call) | 2.00 | 0 | 8.03 | 2.00 | 0 | 8.75 |
| 3b | class method, sealed class, not `class sealed` | 203 | 547 | 12822 | 24.2 | 41 | 1578 |
| 3c | class method, open class | 204 | 600 | 13680 | 24.2 | 42 | 1672 |
| 3d | class method, class in a variable (dynamic send) | 212 | 600 | 19143 | 28.1 | 42 | 638 |
| 3e | inherited class method, open stateless subclass | 247 | 789 | 14706 | 31.3 | 42 | 2257 |
| 3f | class method, class with `classState:` (control: keeps its process) | 203 | 648 | 14109 | 214 | 666 | 13946 |

No `'$gen_call'` (no `gen_server` function at all) appears in the profile of
3b, 3c, 3d or 3e any more. 3f still makes its `gen_server:call` and pays about
11 reductions more than before, for the in-caller check that declines it (the
memo read and the liveness check).

## Where the remaining reductions go

`run.sh profile`, 3c (18 calls per send, 1 of them the loop):

```
    2.00  persistent_term:get/2            memo generation, direct_call_ok/2's flag
    1.00  ets:lookup/2                     the owner memo
    1.00  atomics:get/2                    memo generation counter
    1.00  erlang:is_process_alive/1
    1.00  beamtalk_class_derived_cache:fetch/2
    1.00  beamtalk_class_shadow_flags:direct_call_ok/2
    1.00  beamtalk_class_dispatch:class_send/3
    1.00  beamtalk_class_dispatch:class_send_plain/3
    1.00  beamtalk_class_dispatch:apply_compiled_class_method/7
    1.00  bt@vs_elixir_bench@vb_open_math:class_double:/2
    1.00  beamtalk_object_class:class_send/3             } the call site's
    1.00  beamtalk_class_registry:whereis_class/1        } receiver lookup
    1.00  beamtalk_class_registry:registry_name/1        } (codegen, BT-4301)
    1.00  erlang:atom_to_binary/2                        }
    1.00  erlang:binary_to_atom/2                        }
    1.00  erlang:whereis/1                               }
```

The in-caller hop itself, from `beamtalk_class_dispatch:class_send/3` to the
method, is 11 calls: the guard, the memo read and the call. ADR 0142's Phase 1
gate (3b and 3c at 10 reductions or fewer) is not met by this change alone:

- 3b, 3c and 3e also pay the literal call site's `whereis_class` plus the
  `beamtalk_object_class:class_send/3` wrapper (6 calls, and the
  `binary_to_atom/2` of the registered name dominates their time). BT-4301
  compiles these sends to the guarded direct call, which removes both.
- 3d also pays `beamtalk_message_dispatch:send/3`'s receiver classification
  (`classify_receiver`, `is_class_object`, two `is_class_name`, about 8
  calls), which is not part of the class-send path.
