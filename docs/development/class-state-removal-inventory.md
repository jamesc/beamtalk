<!--
Copyright 2026 James Casey
SPDX-License-Identifier: Apache-2.0
-->

# ADR 0153 Phase 0: `classState:` removal inventory

> **One-off document.** It is the work list for BT-5148 to BT-5163 and nothing keeps it in step with the code. It was written against `main` at `b464155` (2026-11-24), file lists refreshed after merging BT-5141. **The final deletion issue (BT-5163, "mark ADR 0153 Implemented") deletes this file.** If the counts below disagree with a fresh `grep`, trust the `grep` and fix the list in the issue that finds out.
>
> Spec: [ADR 0153](../ADR/0153-remove-class-variables.md) § Implementation Phase 0. Issue: BT-5145, epic BT-5144. This change adds documents only; it edits no source, test or fixture.

## 1. Summary

| What | Count | Disposition |
|---|--:|---|
| `stdlib/test/fixtures` files that declare `classState:` | 91 | 63 delete, 28 rewrite |
| `tests/repl-protocol` files that declare `classState:` | 18 (15 fixtures, 3 cases) | see § 3 |
| `test-package-compiler/cases` that declare `classState:` | 10 cases, 12 snapshot files (plus 1 mention-only case, 1 snapshot) | see § 4 |
| Benchmark / perf `.bt` files that declare `classState:` | 5 | see § 5 |
| `stdlib/src`, `examples/`, 13 downstream packages, `beamtalk-uat` declarations | 0 | none to migrate |
| Rust files that mention class variables | 208 | 10 delete, 134 edit, 63 comments only, 1 keep |
| Erlang files (`.erl`, `.hrl`) that mention class variables | 63 (27 test or test-support) | 7 delete, 49 edit, 7 comments only |
| `ClassBuilder` `classVars:` / `addClassState:default:` users | listed in § 7 | see § 7 |
| Downstream hits | `performLocally:` in `beamtalk-exdura` (1 source file, 8 test files) | **report before Phase 2 starts** (§ 9) |

The ADR counted 118 declaring files in `stdlib/test`, `tests/repl-protocol` and `test-package-compiler/cases` (90 + 18 + 10). The tree has 119: 91 fixtures here, not 90. It counted "145 Rust and 60 Erlang files". `grep -liE 'class[ _-]?var(iable)?s?|classState'` gives **208 and 63** on this commit, and none of the patterns tried on `f1fcd5e` (the ADR's commit) gives 145 or 60 either, so the ADR's pattern is unknown. The tables below use the pattern in § 8, listed file by file, so the later issues do not depend on a count.

**Owners.** Every row names the issue that owns it.

| Issue | Scope |
|---|---|
| BT-5148 | `stdlib/test` fixtures and tests |
| BT-5149 | `tests/repl-protocol`, `test-package-compiler` |
| BT-5150 | benchmarks and `runtime/perf` |
| BT-5151 | class-variable fuzz harness, agreement corpus, nightly job |
| BT-5152 | reject `classState:` / `late classState:` / class-side `self.x` (parser, AST, unparse, errors on every surface) |
| BT-5153 | codegen stops emitting class-variable code, `ThreadedIr` catch entry and verifier |
| BT-5154 | analysis: lints, `ClassInfo`, `runs_class_method_in_caller`, language service, compiler port |
| BT-5155 | stdlib class-variable surface (`classVars:`, `classVarNames`, `performLocally:`, class-side slot paths) |
| BT-5157 | docs, ADR annotations, `AGENTS.md`, `CHANGELOG.md` |
| BT-5158 | runtime: shadow flags, class metadata, descendant-flag erasure |
| BT-5159 | runtime: `protect/1` and snapshot/restore at catch boundaries, `lint-class-var-catchers` |
| BT-5160 | runtime: the class-variable home (`invoke_class_method/7`, `class_state` field, `get/set_class_var`) |
| BT-5161 | runtime funs run in the caller (guard split) |
| BT-5162 | runtime: `with_snapshot/2`, snapshot rows, `performLocally:` intrinsic |
| BT-5163 | delete `beamtalk_class_vars*`, ABI check for old modules, mark ADR Implemented |

## 2. Rewrite guidance

Every fixture marked **rewrite** below names its replacement. These rules apply to all of them.

| Use of the class variable | Replacement |
|---|---|
| A literal constant (`classState: items = #(1, 2)`, `label = #base`, `workers = 1`) | A class method that answers the literal: `class items => #(1, 2)`. A list or dictionary literal is one constant-pool term (§ 10, check 1). Not an array literal `#[...]`, which is rebuilt per call |
| A counter observed by the test (`hits`, `runs`, `count`) | The method's return value, if the test can observe it there. Otherwise `AtomicCounter` |
| A shared mutable value (`cfg`, `current`) | A named Actor (`spawnAs:` in `setUp`, stopped in `tearDown`) |
| A cache | `Ets` owned by the test process, or an Actor's `state:` |
| A field-write probe in a threading matrix (`self.n :=` in the class column) | None. Class methods have no state to write. Delete the cell. The local-write cells stay |

**Three traps, which each fixture rewritten to use these must respect:**

1. **`AtomicCounter` and named `Ets` tables are node-global names.** A fixture rewritten to use one needs a name unique to its test (a per-test `Symbol`, or a name built from a unique integer), or parallel test runs collide. Test classes run concurrently unless the class overrides `class serial -> Boolean => true`, which `TestCase` documents for tests that touch global ETS tables or registered names (`stdlib/src/test_case.bt:80-90`).
2. **An `Ets` table created inside a class method belongs to the calling process and dies with it.** A class method now runs in the caller, so a table made there is gone when the test process ends and invisible to other processes. Create it in the test's `setUp`, or in an Actor.
3. **An `AtomicCounter` belongs to its class process and must be deleted in `tearDown`.** `AtomicCounter new: #name` is process-bound, so the table is owned by `AtomicCounter`'s class process, not the caller. It lives until `delete` or until the node stops. A test that does not delete it leaks a counter into every later test in the run.

## 3. `stdlib/test`

### 3.1 Fixtures that declare `classState:` (91)

*delete* = the fixture pinned class-variable semantics (the note says what). *rewrite* = the class variable only reached another scenario (the note says the replacement; see § 2). "Used by" is every file under `stdlib/test`, `tests/`, `crates/` and `runtime/` that names one of the fixture's classes; `fixtures/x` is another fixture. Tests that become empty after the fixture goes are deleted with it (§ 3.2).

| Fixture | Class | Disposition | Used by | What it pinned / replacement |
|---|---|---|---|---|
| `bt4290runner.bt` | `Bt4290Runner` | delete | `fixtures/bt4290target.bt`, `fixtures/bt4290relay.bt` | `pinned = 0` only forces the class into class-process execution (ADR 0142). BT-4290 (removal waits on a blocked class process) and BT-4262 (`removeFromSystem` from a class method raises `dispatch_error`) pin class-process behaviour that class methods no longer have. OPEN: BT-5148 decides whether the guard still needs a test through a process-bound selector (`new` override) or the rows go. The bt4252 pair keeps its stateless siblings (`bt4252class_protocol_*_base`) and drops the stateful child rows |
| `cv_own_only_forgot.bt` | `CvOwnOnlyForgot` | delete | `cv_own_only_test.bt` | BT-3801: class variables are own-only (`undeclared_class_variable` on an inherited method) |
| `mutation_corpus_class_method.bt` | `MutationCorpusClassMethod` | rewrite | `metamorphic_threading_test.bt`, `value_type_mutation_matrix_test.bt`, `tests/repl-protocol/cases/metamorphic_threading.btscript` | BT-2345/BT-2346 mutation matrix, class-method column. `runs` is the field-write mode, which has no class-side equivalent. Drop field-write cells, keep the local-write cells; the metamorphic pairs lose their class-field arm |
| `class_state_sup_app.bt` | `ClassStateSupApp` | rewrite | `class_state_supervisor_test.bt` | BT-3407/BT-3962: `cfg` set by a class method and read by the supervisor config. Hold the config in a named Actor (`spawnAs:` in `setUp`, stopped in `tearDown`); `class_state_sup_worker` stays |
| `inherited_child.bt` | `InheritedChild` | rewrite | `class_reflection_test.bt`, `inherited_class_method_test.bt`, `system_navigation_fields_test.bt` | BT-2007 inherited class-method self-dispatch. `counter` makes the methods stateful; make them stateless (return values). system_navigation_fields_test.bt and class_reflection_test.bt use these classes: drop their class-side slot rows |
| `sso_class_var_sub.bt` | `SsoClassVarSub` | delete | `self_send_override_test.bt` | BT-3690/BT-3691: class-variable writes across self-sends, arms, loops and folds |
| `late_class_var_read_actor.bt` | `LateClassVarReadActor` | delete | `fixtures/late_class_var_assigned_actor.bt`, `late_field_read_test.bt` | ADR 0124 §4i: class-side `hasField:`/`clearField:` and `late classState:`. The instance `late state:` halves of the tests stay |
| `sso_blocks_sub.bt` | `SsoBlocksSub` | delete | `self_send_override_blocks_test.bt` | ADR 0142 §3 blocks passed to a stateful class (at-home write, `class_state_unreachable`). The template-method rows of self_send_override*_test.bt move to the class-variable-free fixtures (`sso_no_vars_base`, `sso_nested_base`) |
| `bt3947inline_block_loop_class.bt` | `Bt3947InlineBlockLoopClass` | rewrite | `bt3947inline_block_loop_test.bt` | Class-method column of a local-threading matrix. `classState:` is the field-write probe or a constant. Drop the cells that touch `self.<var>`; replace a constant (`items = #(1, 2)`, `f = 0`) with a class method that answers the literal |
| `bt4310unsequenced_class.bt` | `Bt4310UnsequencedClass` | rewrite | `bt4003residuals_test.bt` | Class-method column of a local-threading matrix. `classState:` is the field-write probe or a constant. Drop the cells that touch `self.<var>`; replace a constant (`items = #(1, 2)`, `f = 0`) with a class method that answers the literal |
| `adr0131local_rebind_class.bt` | `Adr0131LocalRebindClass` | rewrite | `adr0131local_rebind_test.bt` | ADR 0131 class-method column. `n` is the field-write probe (`self.n :=`, `#[self.n, t]`). Drop `classState: n` and the cells that read or write it; keep the local-only cells. The `.tsv` probe tables and the Rust matrices that count cells change with it |
| `sso_arm_export_open.bt` | `SsoArmExportOpen` | delete | `fixtures/sso_arm_export_sealed.bt`, `self_send_arm_export_test.bt` | BT-3690/BT-3691: class-variable writes across self-sends, arms, loops and folds |
| `cvs_open.bt` | `CvsOpen` | delete | `class_var_semantics_open_test.bt`, `fixtures/class_var_semantics_matrix_test.bt` | ADR 0130 Phase 1 class-variable semantics matrix (open/sealed, override, initialiser) |
| `cv_own_only_base.bt` | `CvOwnOnlyBase` | delete | `fixtures/cv_own_only_forgot.bt`, `fixtures/cv_own_only_redeclared.bt`, `cv_own_only_test.bt` | BT-3801: class variables are own-only (`undeclared_class_variable` on an inherited method) |
| `sso_stale_base.bt` | `SsoStaleBase` | delete | `fixtures/sso_stale_sub.bt` | BT-3690/BT-3691: class-variable writes across self-sends, arms, loops and folds |
| `sso_class_var_base.bt` | `SsoClassVarBase` | delete | `fixtures/sso_class_var_sub.bt` | BT-3690/BT-3691: class-variable writes across self-sends, arms, loops and folds |
| `foldl_class_var_regression.bt` | `FoldlClassVarRegression` | delete | `foldl_class_var_regression_test.bt` | BT-3140/3150/3151/3168/3169: class-variable writes inside loop and fold bodies |
| `bt4003residuals_class.bt` | `Bt4003ResidualsClass` | rewrite | `bt4003residuals_test.bt` | Class-method column of a local-threading matrix. `classState:` is the field-write probe or a constant. Drop the cells that touch `self.<var>`; replace a constant (`items = #(1, 2)`, `f = 0`) with a class method that answers the literal |
| `cvs_sealed.bt` | `CvsSealed` | delete | `class_var_semantics_sealed_test.bt`, `fixtures/class_var_semantics_matrix_test.bt` | ADR 0130 Phase 1 class-variable semantics matrix (open/sealed, override, initialiser) |
| `class_side_self_collection_probe.bt` | `ClassSideSelfCollectionProbe` | delete | `class_side_self_collection_send_test.bt` | BT-3935: `self do:` in a class method re-enters the class process (`dispatch_error`); with class methods in the caller the scenario cannot occur |
| `class_state_actor.bt` | `ClassStateActor` | delete | `class_state_test.bt` | BT-927: declaration, initial value, class-side getter/setter, persistence across calls |
| `class_method_conditional_local_and_class_var.bt` | `ClassMethodConditionalLocalAndClassVar` | delete | `class_method_conditional_local_and_class_var_test.bt` | BT-3159: conditional arm assigns a local from a class-variable-mutating self-send |
| `class_reflection_cascade_own_name_actor.bt` | `ClassReflectionCascadeOwnNameActor` | delete | `fixtures/class_reflection_own_name_sub_actor.bt`, `class_reflection_cascade_own_name_test.bt` | BT-4423: class-side reflection sends as cascade / own-name receiver inside a class method |
| `bt4290target.bt` | `Bt4290Target` | delete | `bt4290remove_while_waiting_test.bt` | `pinned = 0` only forces the class into class-process execution (ADR 0142). BT-4290 (removal waits on a blocked class process) and BT-4262 (`removeFromSystem` from a class method raises `dispatch_error`) pin class-process behaviour that class methods no longer have. OPEN: BT-5148 decides whether the guard still needs a test through a process-bound selector (`new` override) or the rows go. The bt4252 pair keeps its stateless siblings (`bt4252class_protocol_*_base`) and drops the stateful child rows |
| `class_var_point.bt` | `ClassVarPoint` | delete | `instance_class_var_access_value_type_test.bt` | BT-412/BT-413: class variable shared by instances, read via `self class` |
| `adr0131exc_producer_class.bt` | `Adr0131ExcProducerClass` | rewrite | `adr0131exc_producer_test.bt` | ADR 0131 class-method column. `n` is the field-write probe (`self.n :=`, `#[self.n, t]`). Drop `classState: n` and the cells that read or write it; keep the local-only cells. The `.tsv` probe tables and the Rust matrices that count cells change with it |
| `cvs_driver.bt` | `CvsDriver` | delete | `fixtures/cvs_open.bt`, `fixtures/cvs_sealed.bt`, `fixtures/cvs_override_base.bt` | ADR 0130 Phase 1 class-variable semantics matrix (open/sealed, override, initialiser) |
| `adr0131loop_matrix_class.bt` | `Adr0131LoopMatrixClass` | rewrite | `adr0131loop_matrix_test.bt` | ADR 0131 class-method column. `n` is the field-write probe (`self.n :=`, `#[self.n, t]`). Drop `classState: n` and the cells that read or write it; keep the local-only cells. The `.tsv` probe tables and the Rust matrices that count cells change with it |
| `guarded_diff_class_stateful.bt` | `GuardedDiffClassStateful` | delete | `class_dispatch_differential_test.bt`, `fixtures/guarded_diff_class_forgot.bt` | ADR 0142 rules 1-2: a class with class variables keeps its class process / is never direct-called. The stateless sibling rows in the owning tests stay; delete the stateful rows (class_method_in_caller_test, direct_dispatch_test, class_dispatch_differential_test, own_class_literal_send_test). `then_test.bt` also uses `InCallerStateful`: re-point it at a stateless class |
| `adr0131do_producer_class.bt` | `Adr0131DoProducerClass` | rewrite | `adr0131do_producer_test.bt` | ADR 0131 class-method column. `n` is the field-write probe (`self.n :=`, `#[self.n, t]`). Drop `classState: n` and the cells that read or write it; keep the local-only cells. The `.tsv` probe tables and the Rust matrices that count cells change with it |
| `cvs_init_override_base.bt` | `CvsInitOverrideBase` | delete | `fixtures/cvs_init_override_sub.bt` | ADR 0130 Phase 1 class-variable semantics matrix (open/sealed, override, initialiser) |
| `bt3912match_class.bt` | `Bt3912MatchClass` | rewrite | `bt3912match_test.bt` | Class-method column of a local-threading matrix. `classState:` is the field-write probe or a constant. Drop the cells that touch `self.<var>`; replace a constant (`items = #(1, 2)`, `f = 0`) with a class method that answers the literal |
| `class_reflection_own_name_sub_actor.bt` | `ClassReflectionOwnNameSubActor` | delete | `class_reflection_cascade_own_name_test.bt` | BT-4423: class-side reflection sends as cascade / own-name receiver inside a class method |
| `cascade_class_var_rebind_receiver_position.bt` | `CascadeClassVarRebindReceiverPosition` | delete | `cascade_class_var_rebind_receiver_position_test.bt` | BT-3412: cascade ClassVars-rebind scoping bug |
| `bt4262remove_guard.bt` | `Bt4262RemoveGuard` | delete | `class_side_behaviour_protocol_test.bt` | `pinned = 0` only forces the class into class-process execution (ADR 0142). BT-4290 (removal waits on a blocked class process) and BT-4262 (`removeFromSystem` from a class method raises `dispatch_error`) pin class-process behaviour that class methods no longer have. OPEN: BT-5148 decides whether the guard still needs a test through a process-bound selector (`new` override) or the rows go. The bt4252 pair keeps its stateless siblings (`bt4252class_protocol_*_base`) and drops the stateful child rows |
| `field_at_class_var_outside_actor.bt` | `FieldAtClassVarOutsideActor` | delete | `class_field_at_test.bt` | BT-4148/BT-4421: class-side `fieldAt:`/`fieldAt:put:` (ADR 0153 Decided Q7: now does_not_understand; Phase 2 pins that) |
| `cv_own_only_redeclared.bt` | `CvOwnOnlyRedeclared` | delete | `cv_own_only_test.bt` | BT-3801: class variables are own-only (`undeclared_class_variable` on an inherited method) |
| `adr0131field_value_class.bt` | `Adr0131FieldValueClass` | delete | `adr0131field_value_test.bt` | BT-3795: a loop/fold inside the value of a class-variable write (`self.n := <here>`). The class column has nothing left; the actor column (`adr0131field_value_actor`) stays |
| `late_class_var_assigned_actor.bt` | `LateClassVarAssignedActor` | delete | `late_field_read_test.bt` | ADR 0124 §4i: class-side `hasField:`/`clearField:` and `late classState:`. The instance `late state:` halves of the tests stay |
| `mutation_corpus_actor_class_method.bt` | `MutationCorpusActorClassMethod` | rewrite | `value_type_mutation_matrix_test.bt` | BT-2345/BT-2346 mutation matrix, class-method column. `runs` is the field-write mode, which has no class-side equivalent. Drop field-write cells, keep the local-write cells; the metamorphic pairs lose their class-field arm |
| `wrapper_outer_local_threading_helper.bt` | `WrapperOuterLocalThreadingHelper` | rewrite | `wrapper_local_loop_in_class_method_fold_test.bt` | Class-method column of a local-threading matrix. `classState:` is the field-write probe or a constant. Drop the cells that touch `self.<var>`; replace a constant (`items = #(1, 2)`, `f = 0`) with a class method that answers the literal |
| `inherited_parent.bt` | `InheritedParent` | rewrite | `class_reflection_test.bt`, `fixtures/inherited_child.bt`, `inherited_class_method_test.bt`, `system_navigation_fields_test.bt` | BT-2007 inherited class-method self-dispatch. `counter` makes the methods stateful; make them stateless (return values). system_navigation_fields_test.bt and class_reflection_test.bt use these classes: drop their class-side slot rows |
| `sso_arm_export_sealed.bt` | `SsoArmExportSealed` | delete | `fixtures/sso_arm_export_open.bt`, `self_send_arm_export_test.bt` | BT-3690/BT-3691: class-variable writes across self-sends, arms, loops and folds |
| `has_field_probe_child.bt` | `HasFieldProbeChild` | delete | `direct_dispatch_test.bt` | ADR 0142 rules 1-2: a class with class variables keeps its class process / is never direct-called. The stateless sibling rows in the owning tests stay; delete the stateful rows (class_method_in_caller_test, direct_dispatch_test, class_dispatch_differential_test, own_class_literal_send_test). `then_test.bt` also uses `InCallerStateful`: re-point it at a stateless class |
| `shadow_relay_child.bt` | `ShadowRelayChild` | delete | `class_var_nlr_shadow_test.bt`, `fixtures/shadow_relay_probe.bt`, `fixtures/shadow_relay_parent.bt` | ADR 0110 / BT-3032: class-variable writes survive a non-local-return relay (shadow key) |
| `bt3962class_call_sup.bt` | `Bt3962ClassCallSup` | rewrite | `supervisor_instances_test.bt` | `workers = 1` read by `childSpecs`: a literal constant, so `class childCount => 1` |
| `shadow_cross_class_owner.bt` | `ShadowCrossClassOwner` | delete | `class_var_nlr_shadow_test.bt` | ADR 0110 / BT-3032: class-variable writes survive a non-local-return relay (shadow key) |
| `sso_scope_sealed.bt` | `SsoScopeSealed` | delete | `self_send_override_blocks_test.bt` | ADR 0142 §3 blocks passed to a stateful class (at-home write, `class_state_unreachable`). The template-method rows of self_send_override*_test.bt move to the class-variable-free fixtures (`sso_no_vars_base`, `sso_nested_base`) |
| `cascade_class_var_rebind.bt` | `CascadeClassVarRebind` | delete | `cascade_class_var_rebind_test.bt` | BT-3412: cascade ClassVars-rebind scoping bug |
| `adr0131block_arity_class.bt` | `Adr0131BlockArityClass` | rewrite | `adr0131block_arity_test.bt` | ADR 0131 class-method column. `n` is the field-write probe (`self.n :=`, `#[self.n, t]`). Drop `classState: n` and the cells that read or write it; keep the local-only cells. The `.tsv` probe tables and the Rust matrices that count cells change with it |
| `bt4252class_protocol_actor_child.bt` | `Bt4252ClassProtocolActorChild` | delete | `fixtures/bt4252class_protocol_actor_base.bt`, `class_side_behaviour_protocol_test.bt` | `pinned = 0` only forces the class into class-process execution (ADR 0142). BT-4290 (removal waits on a blocked class process) and BT-4262 (`removeFromSystem` from a class method raises `dispatch_error`) pin class-process behaviour that class methods no longer have. OPEN: BT-5148 decides whether the guard still needs a test through a process-bound selector (`new` override) or the rows go. The bt4252 pair keeps its stateless siblings (`bt4252class_protocol_*_base`) and drops the stateful child rows |
| `adr0131cond_producer_class.bt` | `Adr0131CondProducerClass` | rewrite | `adr0131cond_producer_test.bt` | ADR 0131 class-method column. `n` is the field-write probe (`self.n :=`, `#[self.n, t]`). Drop `classState: n` and the cells that read or write it; keep the local-only cells. The `.tsv` probe tables and the Rust matrices that count cells change with it |
| `result_try_do_tier2fixture.bt` | `ResultTryDoTier2Fixture` | rewrite | `result_try_do_tier2test.bt` | BT-3750 Rust<->Erlang `Result tryDo:` conformance. Drop the class-variable restore assertion (`protect/1`); keep the Tier 1 / Tier 2 Result and local-state rules |
| `sso_scope_open.bt` | `SsoScopeOpen` | delete | `self_send_override_blocks_test.bt` | ADR 0142 §3 blocks passed to a stateful class (at-home write, `class_state_unreachable`). The template-method rows of self_send_override*_test.bt move to the class-variable-free fixtures (`sso_no_vars_base`, `sso_nested_base`) |
| `sso_no_vars_sub.bt` | `SsoNoVarsSub` | delete | `self_send_override_test.bt` | ADR 0142 §3 blocks passed to a stateful class (at-home write, `class_state_unreachable`). The template-method rows of self_send_override*_test.bt move to the class-variable-free fixtures (`sso_no_vars_base`, `sso_nested_base`) |
| `field_at_class_var_inside_actor.bt` | `FieldAtClassVarInsideActor` | delete | `class_field_at_inside_test.bt` | BT-4148/BT-4421: class-side `fieldAt:`/`fieldAt:put:` (ADR 0153 Decided Q7: now does_not_understand; Phase 2 pins that) |
| `class_var_counter.bt` | `ClassVarCounter` | delete | `class_variables_test.bt`, `crates/beamtalk-core/src/semantic_analysis/type_checker/tests/basic_inference.rs` | BT-412/BT-413: class variable shared by instances, read via `self class` |
| `in_caller_stateful.bt` | `InCallerStateful` | delete | `class_method_in_caller_test.bt`, `fixtures/in_caller_stateful_sub.bt`, `then_test.bt` | ADR 0142 rules 1-2: a class with class variables keeps its class process / is never direct-called. The stateless sibling rows in the owning tests stay; delete the stateful rows (class_method_in_caller_test, direct_dispatch_test, class_dispatch_differential_test, own_class_literal_send_test). `then_test.bt` also uses `InCallerStateful`: re-point it at a stateless class |
| `adr0131exc_matrix_class.bt` | `Adr0131ExcMatrixClass` | rewrite | `adr0131exc_matrix_test.bt` | ADR 0131 class-method column. `n` is the field-write probe (`self.n :=`, `#[self.n, t]`). Drop `classState: n` and the cells that read or write it; keep the local-only cells. The `.tsv` probe tables and the Rust matrices that count cells change with it |
| `adr0131eval_block_class.bt` | `Adr0131EvalBlockClass` | rewrite | `adr0131eval_block_test.bt` | ADR 0131 class-method column. `n` is the field-write probe (`self.n :=`, `#[self.n, t]`). Drop `classState: n` and the cells that read or write it; keep the local-only cells. The `.tsv` probe tables and the Rust matrices that count cells change with it |
| `adr0131cond_matrix_class.bt` | `Adr0131CondMatrixClass` | rewrite | `adr0131cond_matrix_test.bt` | ADR 0131 class-method column. `n` is the field-write probe (`self.n :=`, `#[self.n, t]`). Drop `classState: n` and the cells that read or write it; keep the local-only cells. The `.tsv` probe tables and the Rust matrices that count cells change with it |
| `sso_nested_sub.bt` | `SsoNestedSub` | delete | `fixtures/sso_nested_base.bt`, `self_send_override_test.bt` | ADR 0142 §3 blocks passed to a stateful class (at-home write, `class_state_unreachable`). The template-method rows of self_send_override*_test.bt move to the class-variable-free fixtures (`sso_no_vars_base`, `sso_nested_base`) |
| `class_method_nlr_class_vars.bt` | `ClassMethodNlrClassVars` | rewrite | `class_method_non_local_return_test.bt` | BT-1202 `^` inside blocks in class methods. `flag` only observes whether execution continued: answer the observation as the method result, or use an `AtomicCounter` with a name unique to the test, deleted in `tearDown` |
| `collection_driver.bt` | `CollectionDriver` | rewrite | `class_var_nlr_shadow_test.bt`, `fixtures/class_driven_collection.bt`, `fixtures/nested_driven_collection.bt`, `fixtures/self_driven_collection.bt`, `fixtures/shadow_cross_class_owner.bt`, `fixtures/collection_probe.bt`, `class_driven_collection_test.bt` | class_driven_collection_test.bt (BT-3022 `^` through `do:` crossing back out of a class-driven Collection) is not about class variables. Drop `runs`, observe through return values. The repl-protocol class_var_nlr_shadow case and fixtures were deleted in BT-5149 |
| `fold_class_var_mutation.bt` | `FoldClassVarMutation` | delete | `fold_class_var_mutation_test.bt` | BT-3140/3150/3151/3168/3169: class-variable writes inside loop and fold bodies |
| `sso_plain_base.bt` | `SsoPlainBase` | delete | `fixtures/sso_plain_sub.bt`, `fixtures/sso_plain_sealed.bt`, `self_send_plain_reply_test.bt` | BT-3690/BT-3691: class-variable writes across self-sends, arms, loops and folds |
| `loop_class_var_mutation.bt` | `LoopClassVarMutation` | delete | `loop_class_var_mutation_test.bt` | BT-3140/3150/3151/3168/3169: class-variable writes inside loop and fold bodies |
| `loop_nlr_shadow_driver.bt` | `LoopNlrShadowDriver` | delete | `fixtures/loop_nlr_shadow_probe.bt`, `loop_nlr_shadow_test.bt` | ADR 0110 / BT-3032: class-variable writes survive a non-local-return relay (shadow key) |
| `bt3911arm_frames_class.bt` | `Bt3911ArmFramesClass` | rewrite | `bt3911arm_frames_test.bt` | Class-method column of a local-threading matrix. `classState:` is the field-write probe or a constant. Drop the cells that touch `self.<var>`; replace a constant (`items = #(1, 2)`, `f = 0`) with a class method that answers the literal |
| `sso_blocks_base.bt` | `SsoBlocksBase` | delete | `fixtures/sso_blocks_sub.bt`, `self_send_override_blocks_test.bt` | ADR 0142 §3 blocks passed to a stateful class (at-home write, `class_state_unreachable`). The template-method rows of self_send_override*_test.bt move to the class-variable-free fixtures (`sso_no_vars_base`, `sso_nested_base`) |
| `bt4290relay.bt` | `Bt4290Relay` | delete | `fixtures/bt4290target.bt` | `pinned = 0` only forces the class into class-process execution (ADR 0142). BT-4290 (removal waits on a blocked class process) and BT-4262 (`removeFromSystem` from a class method raises `dispatch_error`) pin class-process behaviour that class methods no longer have. OPEN: BT-5148 decides whether the guard still needs a test through a process-bound selector (`new` override) or the rows go. The bt4252 pair keeps its stateless siblings (`bt4252class_protocol_*_base`) and drops the stateful child rows |
| `bt4317own_literal_stateful.bt` | `Bt4317OwnLiteralStateful` | delete | `fixtures/bt4317own_literal_forgot.bt`, `fixtures/bt4317own_literal_stateful_child.bt`, `own_class_literal_send_test.bt` | ADR 0142 rules 1-2: a class with class variables keeps its class process / is never direct-called. The stateless sibling rows in the owning tests stay; delete the stateful rows (class_method_in_caller_test, direct_dispatch_test, class_dispatch_differential_test, own_class_literal_send_test). `then_test.bt` also uses `InCallerStateful`: re-point it at a stateless class |
| `paren_stmt_class_methods.bt` | `ParenStmtClassMethods` | delete | `paren_statement_walkers_test.bt` | BT-5031 class-method statement walker: every cell writes `self.count`. The actor and value-type walkers stay |
| `class_var_snapshot.bt` | `ClassVarSnapshot` | delete | `class_var_snapshot_test.bt`, `crates/beamtalk-codegen/src/core_erlang/threaded_ir/emit.rs`, `crates/beamtalk-codegen/src/core_erlang/threaded_ir/verify.rs`, `crates/beamtalk-codegen/src/core_erlang/threaded_ir/ir.rs`, `crates/beamtalk-codegen/src/core_erlang/threaded_ir/tests/catch_boundary.rs`, `crates/beamtalk-codegen/src/core_erlang/control_flow/exception_handling.rs` | BT-4722 / BT-1937: class-variable capture order and sub-expression self-send mutation |
| `direct_dispatch_stateful.bt` | `DirectDispatchStateful` | delete | `error_method_test.bt`, `direct_dispatch_test.bt` | ADR 0142 rules 1-2: a class with class variables keeps its class process / is never direct-called. The stateless sibling rows in the owning tests stay; delete the stateful rows (class_method_in_caller_test, direct_dispatch_test, class_dispatch_differential_test, own_class_literal_send_test). `then_test.bt` also uses `InCallerStateful`: re-point it at a stateless class |
| `class_method_protected_locals.bt` | `ClassMethodProtectedLocals` | rewrite | `class_method_protected_locals_test.bt` | Class-method column of a local-threading matrix. `classState:` is the field-write probe or a constant. Drop the cells that touch `self.<var>`; replace a constant (`items = #(1, 2)`, `f = 0`) with a class method that answers the literal |
| `class_var_sub_expr.bt` | `ClassVarSubExpr` | delete | `class_var_sub_expr_test.bt` | BT-4722 / BT-1937: class-variable capture order and sub-expression self-send mutation |
| `bt4252class_protocol_object_child.bt` | `Bt4252ClassProtocolObjectChild` | delete | `fixtures/bt4252class_protocol_object_base.bt`, `class_side_behaviour_protocol_test.bt` | `pinned = 0` only forces the class into class-process execution (ADR 0142). BT-4290 (removal waits on a blocked class process) and BT-4262 (`removeFromSystem` from a class method raises `dispatch_error`) pin class-process behaviour that class methods no longer have. OPEN: BT-5148 decides whether the guard still needs a test through a process-bound selector (`new` override) or the rows go. The bt4252 pair keeps its stateless siblings (`bt4252class_protocol_*_base`) and drops the stateful child rows |
| `collection_probe.bt` | `CollectionProbe` | rewrite | `class_var_nlr_shadow_test.bt`, `class_driven_collection_test.bt` | class_driven_collection_test.bt (BT-3022 `^` through `do:` crossing back out of a class-driven Collection) is not about class variables. Drop `runs`, observe through return values. The repl-protocol class_var_nlr_shadow case and fixtures were deleted in BT-5149 |
| `cvs_override_base.bt` | `CvsOverrideBase` | delete | `fixtures/cvs_override_sub.bt` | ADR 0130 Phase 1 class-variable semantics matrix (open/sealed, override, initialiser) |
| `has_clear_class_var_outside_actor.bt` | `HasClearClassVarOutsideActor` | delete | `has_clear_field_test.bt` | ADR 0124 §4i: class-side `hasField:`/`clearField:` and `late classState:`. The instance `late state:` halves of the tests stay |
| `bt4317own_literal_stateful_child.bt` | `Bt4317OwnLiteralStatefulChild` | delete | `own_class_literal_send_test.bt` | ADR 0142 rules 1-2: a class with class variables keeps its class process / is never direct-called. The stateless sibling rows in the owning tests stay; delete the stateful rows (class_method_in_caller_test, direct_dispatch_test, class_dispatch_differential_test, own_class_literal_send_test). `then_test.bt` also uses `InCallerStateful`: re-point it at a stateless class |
| `has_clear_class_var_actor.bt` | `HasClearClassVarActor` | delete | `fixtures/has_clear_class_var_outside_actor.bt`, `has_clear_field_test.bt` | ADR 0124 §4i: class-side `hasField:`/`clearField:` and `late classState:`. The instance `late state:` halves of the tests stay |
| `shadow_relay_parent.bt` | `ShadowRelayParent` | delete | `class_var_nlr_shadow_test.bt`, `fixtures/shadow_relay_child.bt` | ADR 0110 / BT-3032: class-variable writes survive a non-local-return relay (shadow key) |
| `bt4014class_side_operand_producer.bt` | `Bt4014ClassSideOperandProducer` | rewrite | `bt4014class_side_operand_producer_test.bt` | Class-method column of a local-threading matrix. `classState:` is the field-write probe or a constant. Drop the cells that touch `self.<var>`; replace a constant (`items = #(1, 2)`, `f = 0`) with a class method that answers the literal |
| `cvs_init_open.bt` | `CvsInitOpen` | delete | `class_var_semantics_open_test.bt` | ADR 0130 Phase 1 class-variable semantics matrix (open/sealed, override, initialiser) |
| `lookup_field_write_class.bt` | `LookupFieldWriteClass` | delete | `lookup_field_write_test.bt` | BT-3922: a Dictionary lookup handler writing a class variable in a class method; the test has no other column |
| `class_method_conditional_self_call.bt` | `ClassMethodConditionalSelfCall` | rewrite | `class_var_nlr_shadow_test.bt`, `class_method_conditional_self_call_test.bt` | BT-1550 conditional self-call + `^` (`unbound_var ClassVars{N}`). Drop the mutating row; keep the value rows |
| `cvs_init_sealed.bt` | `CvsInitSealed` | delete | `class_var_semantics_sealed_test.bt` | ADR 0130 Phase 1 class-variable semantics matrix (open/sealed, override, initialiser) |
| `instance_access_counter.bt` | `InstanceAccessCounter` | delete | `instance_class_var_access_test.bt` | BT-412/BT-413: class variable shared by instances, read via `self class` |
| `sso_plain_sealed.bt` | `SsoPlainSealed` | delete | `fixtures/sso_plain_base.bt`, `self_send_plain_reply_test.bt` | BT-3690/BT-3691: class-variable writes across self-sends, arms, loops and folds |

Fixtures that subclass a declaring fixture but have no `classState:` line themselves, and go with their parent: `bt4317own_literal_forgot`, `cvs_init_override_sub`, `cvs_override_sub`, `guarded_diff_class_forgot`, `in_caller_stateful_sub`, `sso_plain_sub`, `sso_stale_sub`. `field_at_no_class_vars_holder`, `sso_no_vars_base` and `direct_dispatch_stateless` mention `classState:` only in comments; they stay and have the comment reworded.

**The one open design question in this list.** Six fixtures (`bt4290*`, `bt4262remove_guard`, `bt4252class_protocol_*_child`) use `pinned = 0` only to force a class into class-process execution. BT-4290 (a removal that waits on a class process blocked in a class method raises `timeout`) and BT-4262 (`removeFromSystem` from a class method raises `dispatch_error`) are guards for a situation that, after ADR 0153, only a *process-bound* selector can create. BT-5148 decides: delete them, or reach the same state through a process-bound class method such as a `new` override. ADR 0153 does not say.

### 3.2 Tests

| Test | Disposition | What |
|---|---|---|
| `class_state_test.bt` | delete | BT-927 class-state declaration, initialisation, getter/setter, persistence |
| `class_variables_test.bt` | delete | BT-412 class variables persist across instance creation. Also named in `crates/beamtalk-core/.../type_checker/tests/basic_inference.rs` (`ClassVarCounter`): BT-5154 edits that test |
| `instance_class_var_access_test.bt`, `instance_class_var_access_value_type_test.bt` | delete | BT-413 instances read class variables through `self class` |
| `cv_own_only_test.bt` | delete | BT-3801 own-only class variables, `undeclared_class_variable` |
| `class_var_semantics_open_test.bt`, `class_var_semantics_sealed_test.bt`, `class_var_semantics_override_test.bt` (+ `fixtures/class_var_semantics_matrix_test.bt`) | delete | ADR 0130 Phase 1 semantics matrix, open and sealed |
| `class_var_snapshot_test.bt` | delete | BT-4722 creation-time class-variable capture order |
| `class_var_sub_expr_test.bt` | delete | BT-1937 sub-expression self-send mutation |
| `class_var_nlr_shadow_test.bt`, `loop_nlr_shadow_test.bt` | delete | ADR 0110 shadow key across non-local-return relay. `loop_nlr_shadow_test.bt` is BT-3136, "one end-to-end test" of a counted loop + foreign `^` + class-variable mutation: the class-variable piece cannot survive; BT-5148 decides whether the actor-field loop + `^` piece is already covered elsewhere |
| `cascade_class_var_rebind_test.bt`, `cascade_class_var_rebind_receiver_position_test.bt` | delete | BT-3412 cascade ClassVars rebind |
| `fold_class_var_mutation_test.bt`, `loop_class_var_mutation_test.bt`, `foldl_class_var_regression_test.bt` | delete | class-variable writes in loop/fold bodies |
| `class_method_conditional_local_and_class_var_test.bt` | delete | BT-3159 |
| `self_send_arm_export_test.bt`, `self_send_plain_reply_test.bt` | delete | BT-3690/3691 commit semantics of class-variable writes |
| `self_send_override_test.bt`, `self_send_override_blocks_test.bt` | edit | keep the template-method override rows (BT-3666) on stateless fixtures; delete rows that read class variables, blocks passed to a stateful class and `class_state_unreachable` |
| `class_field_at_test.bt`, `class_field_at_inside_test.bt` | delete | BT-4148/BT-4421 class-side `fieldAt:`. Replace with one test that pins does_not_understand (ADR 0153 Decided Q7, owned by BT-5152) |
| `class_reflection_cascade_own_name_test.bt` | delete | BT-4423 |
| `late_field_read_test.bt`, `has_clear_field_test.bt` | edit | keep the instance `late state:` rows; delete the `late classState:` rows |
| `lookup_field_write_test.bt` | delete | BT-3922 class column only |
| `class_side_self_collection_send_test.bt` | delete | BT-3935 class-process re-entry |
| `class_method_in_caller_test.bt` | edit | keep the stateless rows. The "class with class variables keeps its class process" rows go. Also uses `performLocally:` |
| `direct_dispatch_test.bt` | edit | keep stateless rows, delete the stateful pair (`DirectDispatchStateful`, `HasFieldProbeChild`) |
| `class_dispatch_differential_test.bt` | edit | delete the `GuardedDiffClassStateful` rows; uses `performLocally:` today |
| `own_class_literal_send_test.bt` | edit | keep the stateless chain and the third shape; delete the class-variable chain (BT-4317) |
| `then_test.bt` | edit | re-point `InCallerStateful` at a stateless class |
| `class_side_behaviour_protocol_test.bt` | edit | BT-4252 keep the stateless rows; BT-4262 `testRemoveFromSystemInClassProcessIsRefused` goes (see OPEN on the fixtures) |
| `bt4290remove_while_waiting_test.bt` | delete or rewrite (OPEN, BT-5148) | see fixtures `bt4290*` |
| `class_state_supervisor_test.bt` | rewrite | named Actor holds the config |
| `supervisor_instances_test.bt` | edit | `Bt3962ClassCallSup` becomes a literal class method |
| `inherited_class_method_test.bt`, `class_reflection_test.bt`, `system_navigation_fields_test.bt` | edit | stateless parent/child; drop class-side slot rows (BT-5155 removes the class-side slot branches in `system_navigation.bt`) |
| `result_try_do_tier2test.bt` | edit | drop the class-variable restore assertion |
| `class_method_non_local_return_test.bt` | edit | BT-1202: observe without class state |
| `class_method_conditional_self_call_test.bt` | edit | drop the mutating row |
| `class_driven_collection_test.bt` | edit | BT-3022 without `runs` |
| `metamorphic_threading_test.bt`, `value_type_mutation_matrix_test.bt`, `bt3385dead_assignment_test.bt` | edit | drop class-field write cells; `tests/repl-protocol/cases/metamorphic_threading.btscript` follows. |
| `adr0131local_rebind_test.bt`, `adr0131cond_producer_test.bt`, `adr0131cond_matrix_test.bt`, `adr0131loop_matrix_test.bt`, `adr0131exc_producer_test.bt`, `adr0131exc_matrix_test.bt`, `adr0131do_producer_test.bt`, `adr0131eval_block_test.bt`, `adr0131block_arity_test.bt` | edit | drop class-column cells that touch `self.n` |
| `adr0131field_value_test.bt` | edit | drop the class column |
| `bt3911arm_frames_test.bt`, `bt3912match_test.bt`, `bt3947inline_block_loop_test.bt`, `bt4003residuals_test.bt`, `bt4014class_side_operand_producer_test.bt`, `class_method_protected_locals_test.bt`, `wrapper_local_loop_in_class_method_fold_test.bt` | edit | same, per fixture |
| `paren_statement_walkers_test.bt` | edit | drop the "class method" half |
| `bt3380actor_list_op_selector_test.bt` | edit | references `CollectionProbe`; re-check after the fixture rewrite |
| `error_method_test.bt` | edit (check) | mentions `DirectDispatchStateful`; likely a stale name-only reference |
| `guarded_static_call_test.bt` | edit | calls `Erlang beamtalk_class_shadow_flags set_class_vars:` (lines 89-112) to flip the class-variable flag. Rows go with the flag (BT-5158) |
| `perform_locally_test.bt`, `fixtures/local_call_helper.bt` | delete | `performLocally:` (BT-5155 removes the intrinsic, BT-5162 the runtime path) |
| `class_builder_class_method_test.bt`, `class_builder_incremental_test.bt` | edit | use `classVars:` / `addClassState:default:`; see ClassBuilder section |
| `field_kinds_test.bt`, `late_slot_test.bt` | edit | mention `classState:` in comments/rows for the five compile-time errors (ADR 0124): drop the `classState:` halves |
| `class_var_semantics_override_test.bt`, `fixtures/class_var_semantics_matrix_test.bt`, `fixtures/cvs_override_sub.bt`, `fixtures/cvs_init_override_sub.bt`, `fixtures/sso_stale_sub.bt`, `fixtures/sso_plain_sub.bt`, `fixtures/guarded_diff_class_forgot.bt`, `fixtures/bt4317own_literal_forgot.bt`, `fixtures/in_caller_stateful_sub.bt` | delete | fixtures that subclass a declaring fixture and have no `classState:` line of their own |

`stdlib/test` files other than the above that mention `classState:` only in prose: `field_kinds_test.bt`, `late_slot_test.bt`, `inherited_class_method_test.bt`, `direct_dispatch_test.bt`, `system_navigation_fields_test.bt`, `self_send_override_test.bt`, `late_field_read_test.bt`, `loop_nlr_shadow_test.bt`, `has_clear_field_test.bt`, `guarded_static_call_test.bt`, `class_state_test.bt`. All are in the table above.

## 4. `tests/repl-protocol`

Declares `classState:` in 15 fixtures and 3 cases (`constant_class_method_key`, `late_slots`, `guarded_static_call_key`; `late_slots` declares three).

| File | Disposition | Pinned / replacement |
|---|---|---|
| `cases/class_var_single_home.btscript` and its fixtures `class_var_single_home_{counter,escaping_write,ids,loud,probechild,reload_v1,reload_v2,tally,tally_main}.bt` | delete (10 files) | ADR 0130 single-home semantics: nesting, escaping write, reload keeps the home |
| `cases/class_var_nlr_shadow.btscript`, `fixtures/class_var_nlr_shadow_driver.bt`, `fixtures/class_var_nlr_shadow_probe.bt` (no declaration of its own) | delete (3) | ADR 0110 shadow write across `^` relay, REPL column of `class_var_nlr_shadow_test.bt` (deleted by BT-5148) |
| `cases/storage_family_threading.btscript` and fixtures `storage_family_class_method{,_ensure_direct_write,_foldl_direct_write}.bt` | edit; delete the 3 fixtures and the class-method column | storage-family threading in a class method (field write is the class variable) |
| `cases/class_methods_run_in_caller.btscript`, `fixtures/class_methods_run_in_caller_counter.bt` | edit; delete the fixture and its stateful rows | ADR 0142: a class with class variables keeps its class process. The stateless rows stay |
| `cases/workspace_native_api.btscript` (lines 751-775), `fixtures/field_fixture.bt` | edit | `SystemNavigation fieldReadersOf:/fieldWritersOf: #spawnCount in: FieldFixture` on a class-side field. Delete the class-side rows and the `spawnCount` field and its two class methods; the instance rows stay |
| `cases/late_slots.btscript` (lines 55, 227, 282) | edit | three inline `late classState: current :: Integer` classes. Delete the class-side cases; instance `late state:` stays |
| `cases/constant_class_method_key.btscript` (line 121), `cases/guarded_static_call_key.btscript` (line 94) | rewrite | the inline `classState: count :: Integer = 0` is the stateful *contrast* to a constant class method. Drop the contrast row |
| `cases/perform_locally.btscript`, `fixtures/perform_locally_fixture.bt` | delete | `performLocally:` (BT-5155 / BT-5162) |
| `cases/class_builder_capstone.btscript` (line 78, 126), `cases/class_builder_class_methods.btscript` (line 23) | edit | `classVars:` / `addClassState:default:` (§ 7) |
| `cases/metamorphic_threading.btscript` | edit | follows `mutation_corpus_class_method` (§ 3.1) |

Owner: BT-5149.

## 5. `test-package-compiler/cases`

Each case is `cases/<name>/main.bt` and its `tests/snapshots/compiler_tests__<name>_*.snap`. Snapshots are deleted with the case, or regenerated (`cargo insta`) when the case is rewritten.

| Case | Snapshots | Disposition | Pinned / replacement |
|---|--:|---|---|
| `class_var_single_mutation`, `class_var_sequential_mutations`, `class_var_mutation_before_block_relay`, `class_var_multi_field_mutations` | 1 each | delete | ADR 0111 Phase A3 threading-mode matrix: the class-variable mutation cells (byte-parity anchors for the Rust/Erlang agreement) |
| `self_send_explicit_class_reference`, `self_send_open_class_side` | 1 each | delete | BT-3666 class-side self-send rebinds `ClassVars` |
| `self_send_class_var_less_base` | 1 | delete | declares nothing (the text "classState:" is in its comment); pins that a base with no class variables still returns `class_var_result` when a late-bound self-send rebinds `ClassVars` |
| `class_methods` | 1 | rewrite | class-method codegen with an `instanceCount` class variable. Drop the variable, keep the class-method shapes |
| `foldl_value_type_and_class_method`, `letrec_direct_params_value_and_class` | 2 each | rewrite | ADR 0111 A3 matrix cells for the ClassMethod context: drop `total` / `spawnCount`, keep the value-type cell and the locals-only class-method cell |
| `nlr_matching_token_class_method` | 1 | rewrite | NLR matching-token shape in a class method; `total` is the observed write: observe through the result |

Owner: BT-5149.

## 6. Benchmarks and `runtime/perf`

Recorded result files under `benchmarks/vs-elixir/results/` are history and are not edited (7 of them mention `classState:`).

| File | Disposition | What |
|---|---|---|
| vb_stateful_math.bt (deleted by BT-5150) (`calls :: Integer = 0`) | delete | the class behind row **3f** (`VbStatefulMath`, `vb_bench.bt:178` `statefulClassMethod:`) |
| vb_late_state_math.bt (deleted by BT-5150) | delete | no declaration: the harness publishes "declares class variables" with `beamtalk_class_metadata:put_class_variables/2` (`vs_bench.erl:351-355`). Rows **3g** and **3h** |
| `benchmarks/vs-elixir/beamtalk/src/vb_bench.bt` (`lateStateMathClass`, `statefulClassMethod:`, `dynClassMethod:c:` and the comment at line 185) | edit | drop `lateStateMathClass` and `statefulClassMethod:`. Keep `dynClassMethod:c:` (rows 3d and 3j use it) |
| `benchmarks/vs-elixir/harness/vs_bench.erl` (rows 3f, 3g, 3h; `compiles_to("3f"|"3g"|"3h")`; `put_class_variables` / `chain_has_class_variables` calls at 351-355; `late_state_math_class` at 262) | edit | delete rows 3f-3h. Row 3d (class in a variable) and 3i/3j (class-side extension shadow) stay, but their Elixir / BT comparison text that names `classState:` changes. Rows 3i/3j (class-side extension shadow) belong to ADR 0152 / BT-5141: leave them to it |
| `benchmarks/vs-elixir/README.md` | edit | mentions `classState:` |
| vd_stateful_math.bt (deleted by BT-5150) (`calls :: Integer = 0`) | delete | the class behind row **c4** (`classStateful:`) |
| `benchmarks/dispatch-overhead/harness/vd_bench.erl:51` (row c4) and `benchmarks/dispatch-overhead/README.md` | edit | delete row c4 and its row-table line |
| ssb_class_var_class.bt (deleted by BT-5150) (`a`, `b`, `c`) | delete | `SsbClassVarClass`, driven from `ssb_main.bt:44` |
| ssb_class_var_threaded_loop_class.bt (deleted by BT-5150) (`a`, `b`, `c`) | delete | `SsbClassVarThreadedLoopClass`, `ssb_main.bt:48` |
| `runtime/perf/self_send_bench/src/ssb_on_do_raise_class.bt` (`hits = 0`) | rewrite | `SsbOnDoRaiseClass`, `ssb_main.bt:59`, measures a raising `on:do:` in a class method. The class variable only keeps the class in class-process mode. Its Object twin `ssb_on_do_raise_object.bt` is the control row; after the rewrite the two are the same shape, so delete the class variant or turn it into a stateless class-method row |
| `runtime/perf/beamtalk_perf_tests.erl:1173-1201` | check | runs `SsbMain run`; confirm the row-name assertions after the three classes change |

Owner: BT-5150. The baseline ([§ 12](#12-baseline-benchmark)) is recorded **before** any of these change; BT-5150 is blocked on it.

## 7. `ClassBuilder`, `classVars:`, `addClassState:default:`

The two setters go (`stdlib/src/class_builder.bt:134` `classVars:`, `:307` `addClassState:default:`, and the `classState` field behind them). `ClassBuilder` itself, `fields:`, `methods:`, `classMethods:`, ADR 0084 funs, `class_builder_source.rs` and `register/1` stay.

**Tests and docs that call them:**

| File | Use |
|---|---|
| `stdlib/test/class_builder_class_method_test.bt:30, 85, 99, 111, 117` | `classVars: #{#total => 0}` / `#{#n => 10}` seeds the value a `classMethods:` block reads and writes. Rewrite: the blocks answer a literal, or delete the class-variable rows (the ADR 0084 funs stay) |
| `stdlib/test/class_builder_incremental_test.bt:6, 8, 34, 60, 65` | `addClassState: #total default: 0` and the "BT-873 dropped writes" test. Delete the class-state rows; keep `addMethod:body:` / `addClassMethod:body:` |
| `class_var_nlr_shadow_test.bt:126` (deleted by BT-5148) | `classVars: #{#runs => 0}`; goes with the test |
| `tests/repl-protocol/cases/class_builder_capstone.btscript:78, 126` | `classVars:` / `addClassState:default:` |
| `tests/repl-protocol/cases/class_builder_class_methods.btscript:23` | "classState seeded via classVars:, mutation threaded" |
| `docs/beamtalk-language-features.md:1377-1429` | § on class variables with ClassBuilder (`classVars: #{ #opened => 0 }`, `addClassState: #total default: 10`) |
| `stdlib/src/class_builder.bt` doc comments (lines 103, 125-134, 298-307) | the `classMethods:` example `[:self \| self.total := self.total + 1]` (line 103) and the two setters' docs |
| `crates/beamtalk-codegen/src/core_erlang/class_builder_source.rs:329-395, 954-976` | recognises `classVars:` / `addClassState:default:` cascade keys as class-variable names; its tests (lines 954-976) |
| `crates/beamtalk-codegen/src/core_erlang/tests/dispatch.rs:4318, 4509` | cascades with `classVars: #{ #runs => 0 }` |
| `crates/beamtalk-codegen/src/core_erlang/tests/gen_server/class_var_capture_catch.rs:426`, `class_var_not_inherited.rs:118, 159` | go with those test files (delete) |
| `crates/beamtalk-compiler-port/src/tests/expression_assignment.rs:375` | cascade with `classVars: #{ #opened => 0 }` |
| `crates/beamtalk-core/.../class_hierarchy/generated_builtins.rs:1312, 1325` | generated: regenerate after the stdlib change |
| `crates/beamtalk-codegen/src/core_erlang/generator/{accessors.rs:163, context.rs:368}`, `class_method_codegen.rs:172` | comments about the builder's `classVars:` keys |

**Class variables used inside a `classMethods:` block** (the case ADR 0153 § 5 names): only the first two rows above, plus the `class_builder.bt:103` doc example. No `examples/` or downstream file does this.

The ADR's migration row `builder classVars: #{#x => 1}` → `builder addClassMethod: #x body: [:self | 1]` is the replacement for all of them. Owners: BT-5155 (stdlib), BT-5148 / BT-5149 (tests), BT-5153 (`class_builder_source.rs`), BT-5157 (docs).

## 8. Rust and Erlang files that mention class variables

Pattern (case-insensitive), run over `crates/**/*.rs` and `runtime/**/*.{erl,hrl}`:

```text
class[ _-]?var(iable)?s?|classState
```

208 Rust files and 63 Erlang files match. "Hits" is the matching line count. Dispositions:

- **delete**: the file exists only for class variables.
- **edit**: code changes (an arm, a field, a parameter, a test row goes).
- **edit (comments only)**: only comments or doc text mention it; the text is reworded or removed with the code it describes. A final `grep` gate in BT-5163 catches what is missed.
- **keep**: the match means something else.

The disposition is a first reading from the pattern, the file's role and a read of the matching lines of the small files. It is not a per-line audit; the owner reads the file when it takes it. The owner column is the issue that does the work. Where one file needs edits from two issues, the later one's share is named in § 8.3.

### 8.1 Rust (208)

| File | Hits | Disposition | Owner |
|---|--:|---|---|
| `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/generated_builtins.rs` | 143 | edit | BT-5154 |
| `crates/beamtalk-codegen/src/core_erlang/class_var_access.rs` | 116 | delete | BT-5153 |
| `crates/beamtalk-core/src/semantic_analysis/tests/class_state_abroad.rs` | 65 | delete | BT-5154 |
| `crates/beamtalk-codegen/src/core_erlang/tests/gen_server/class_var_capture_catch.rs` | 50 | delete | BT-5153 |
| `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/tests.rs` | 47 | edit | BT-5154 |
| `crates/beamtalk-codegen/src/core_erlang/tests/gen_server/class_var_access.rs` | 47 | delete | BT-5153 |
| `crates/beamtalk-core/src/semantic_analysis/validators/class_state_abroad.rs` | 45 | delete | BT-5154 |
| `crates/beamtalk-core/src/semantic_analysis/validators/class_process_ffi_validators.rs` | 43 | edit | BT-5154 |
| `crates/beamtalk-codegen/src/core_erlang/tests/dispatch.rs` | 40 | edit | BT-5153 |
| `crates/beamtalk-core/src/semantic_analysis/validators/class_validators.rs` | 37 | edit | BT-5154 |
| `crates/beamtalk-codegen/src/core_erlang/class_var_keys.rs` | 36 | delete | BT-5153 |
| `crates/beamtalk-core/src/source_analysis/parser/declarations.rs` | 35 | edit | BT-5152 |
| `crates/beamtalk-cli/src/commands/release/upgrade.rs` | 31 | edit | BT-5163 |
| `crates/beamtalk-core/src/semantic_analysis/validators/reserved_name_validators.rs` | 30 | edit | BT-5154 |
| `crates/beamtalk-core/src/semantic_analysis/block_facts/mutation.rs` | 29 | edit | BT-5153 |
| `crates/beamtalk-core/src/source_analysis/parser/tests/class_tests.rs` | 28 | edit | BT-5152 |
| `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/hierarchy_queries.rs` | 28 | edit | BT-5154 |
| `crates/beamtalk-codegen/src/core_erlang/intrinsics/object.rs` | 26 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/control_flow/exception_handling.rs` | 25 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/gen_server/class_registration.rs` | 24 | edit | BT-5153 |
| `crates/beamtalk-cli/tests/cli/cli_class_var_agreement.rs` | 24 | delete | BT-5151 |
| `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/class_info.rs` | 23 | edit | BT-5154 |
| `crates/beamtalk-codegen/src/core_erlang/class_builder_source.rs` | 23 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/class_method_codegen.rs` | 22 | edit | BT-5153 |
| `crates/beamtalk-core/src/semantic_analysis/tests/local_threading.rs` | 21 | edit | BT-5153 |
| `crates/beamtalk-language-service/src/queries/diagnostic_provider.rs` | 19 | edit | BT-5154 |
| `crates/beamtalk-core/src/semantic_analysis/block_facts/tests/mutation.rs` | 19 | edit | BT-5153 |
| `crates/beamtalk-codegen/tests/class_var_agreement.rs` | 19 | delete | BT-5151 |
| `crates/beamtalk-codegen/src/core_erlang/expr_shape.rs` | 19 | edit | BT-5153 |
| `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/mod.rs` | 18 | edit | BT-5154 |
| `crates/beamtalk-codegen/src/core_erlang/threaded_ir/verify.rs` | 18 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/gen_server/late_field_read.rs` | 18 | edit | BT-5153 |
| `crates/beamtalk-cli/src/commands/build_stdlib/generators.rs` | 18 | edit | BT-5154 |
| `crates/beamtalk-lint/src/actor_state_abroad.rs` | 17 | edit | BT-5154 |
| `crates/beamtalk-codegen/src/core_erlang/class_registry.rs` | 16 | edit | BT-5153 |
| `crates/beamtalk-core/src/source_analysis/parser/tests/expression_tests.rs` | 15 | edit | BT-5152 |
| `crates/beamtalk-codegen/src/core_erlang/generator/mod.rs` | 15 | edit | BT-5153 |
| `crates/beamtalk-core/src/test_helpers/class_var_program.rs` | 14 | delete | BT-5151 |
| `crates/beamtalk-core/src/semantic_analysis/facts.rs` | 14 | edit | BT-5154 |
| `crates/beamtalk-codegen/src/core_erlang/threaded_ir/ir.rs` | 14 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/racing_operand_corpus.rs` | 14 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/gen_server/class_var_not_inherited.rs` | 14 | delete | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/generator/accessors.rs` | 14 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/expressions.rs` | 14 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/sequencing.rs` | 13 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/generator/context.rs` | 13 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/threaded_ir/tests/catch_boundary.rs` | 12 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/gen_server/state_threading_loops.rs` | 12 | edit | BT-5153 |
| `crates/beamtalk-core/src/test_helpers.rs` | 11 | edit | BT-5151 |
| `crates/beamtalk-core/src/compilation/diagnostics_policy.rs` | 11 | edit | BT-5154 |
| `crates/beamtalk-codegen/src/core_erlang/tests/control_flow/exception_handling.rs` | 11 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/class_meta.rs` | 11 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/blocks.rs` | 11 | edit | BT-5153 |
| `crates/beamtalk-cli/src/commands/build_stdlib/metadata.rs` | 11 | edit | BT-5154 |
| `crates/beamtalk-core/src/source_analysis/method_category.rs` | 10 | edit | BT-5152 |
| `crates/beamtalk-codegen/src/core_erlang/threaded_prelude.rs` | 10 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/control_flow/list_and_loop_value_threading.rs` | 10 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/class_side_operand_producer_corpus.rs` | 10 | edit | BT-5153 |
| `crates/beamtalk-core/src/semantic_analysis/validators/late_slot_validators.rs` | 9 | edit | BT-5154 |
| `crates/beamtalk-core/src/semantic_analysis/tests/adr0123_shape_version_and_migrations.rs` | 9 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/expressions.rs` | 9 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/operand_order_check.rs` | 9 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/generator/body_state.rs` | 9 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/expressions/tests.rs` | 9 | edit | BT-5153 |
| `crates/beamtalk-core/src/ast/expression.rs` | 8 | edit | BT-5152 |
| `crates/beamtalk-core/src/ast/class.rs` | 8 | edit | BT-5152 |
| `crates/beamtalk-codegen/src/core_erlang/tests/gen_server/protocol_and_xref_metadata.rs` | 8 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/primitives/behaviour.rs` | 8 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/field_types.rs` | 8 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/dispatch_codegen/tests.rs` | 8 | edit | BT-5153 |
| `crates/beamtalk-core/src/source_analysis/parser/mod.rs` | 7 | edit | BT-5152 |
| `crates/beamtalk-core/src/semantic_analysis/validators/local_threading/walker.rs` | 7 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/threaded_ir/emit.rs` | 7 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/while_condition_class_method.rs` | 7 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/runtime_export_contract_tests.rs` | 7 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/gen_server/native_facade.rs` | 7 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/control_flow/plan.rs` | 7 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/control_flow/conditionals.rs` | 7 | edit (comments only) | BT-5153 |
| `crates/beamtalk-core/src/semantic_analysis/block_facts/sequencing.rs` | 6 | edit (comments only) | BT-5153 |
| `crates/beamtalk-core/src/ast/normalize.rs` | 6 | edit | BT-5152 |
| `crates/beamtalk-compiler-port/src/decode.rs` | 6 | edit | BT-5154 |
| `crates/beamtalk-codegen/src/core_erlang/tests/gen_server/tier2_nested_blocks.rs` | 6 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/gen_server/native_facade.rs` | 6 | edit | BT-5153 |
| `crates/beamtalk-language-service/src/queries/references_provider.rs` | 5 | edit | BT-5154 |
| `crates/beamtalk-core/src/unparse/declarations.rs` | 5 | edit | BT-5152 |
| `crates/beamtalk-core/src/source_analysis/summary.rs` | 5 | edit | BT-5152 |
| `crates/beamtalk-core/src/semantic_analysis/tests/bt5026_state_defaults_and_protocol_bodies.rs` | 5 | edit | BT-5153 |
| `crates/beamtalk-compiler-port/src/tests/dispatch_and_hierarchy_meta.rs` | 5 | edit | BT-5154 |
| `crates/beamtalk-codegen/src/core_erlang/tests/while_condition_carrier_corpus.rs` | 5 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/local_producer_agreement.rs` | 5 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/gen_server/mod.rs` | 5 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/expect_directive_statement.rs` | 5 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/control_flow/family_detector_differential.rs` | 5 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/control_flow/conditionals.rs` | 5 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/actor_state_access.rs` | 5 | edit | BT-5153 |
| `crates/beamtalk-language-service/src/identifier_lookup.rs` | 4 | edit | BT-5154 |
| `crates/beamtalk-core/src/semantic_analysis/type_checker/tests/bt4235_value_type_trailing_field_assignment.rs` | 4 | edit | BT-5154 |
| `crates/beamtalk-core/src/semantic_analysis/tests/bt5025_cascade_later_messages.rs` | 4 | edit | BT-5153 |
| `crates/beamtalk-core/src/semantic_analysis/scope.rs` | 4 | edit | BT-5154 |
| `crates/beamtalk-core/src/semantic_analysis/mod.rs` | 4 | edit | BT-5154 |
| `crates/beamtalk-codegen/src/core_erlang/tests/loop_body_conditional_receiver_corpus.rs` | 4 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/ffi_direct_call.rs` | 4 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/closed_fun_state_version.rs` | 4 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/dispatch_codegen/class_send.rs` | 4 | edit (comments only) | BT-5153 |
| `crates/beamtalk-cli/src/commands/util.rs` | 4 | edit | BT-5149 |
| `crates/beamtalk-cli/src/commands/build_stdlib/tests/generators.rs` | 4 | edit | BT-5154 |
| `crates/beamtalk-language-service/src/tests/goto_definition_navigation.rs` | 3 | edit | BT-5154 |
| `crates/beamtalk-core/src/test_helpers/statement_probes.rs` | 3 | edit | BT-5151 |
| `crates/beamtalk-core/src/semantic_analysis/validators/native_validators.rs` | 3 | edit | BT-5154 |
| `crates/beamtalk-core/src/semantic_analysis/validators/mod.rs` | 3 | edit | BT-5154 |
| `crates/beamtalk-core/src/semantic_analysis/type_checker/tests/bt4043_metaclass_slot_assignment.rs` | 3 | edit | BT-5154 |
| `crates/beamtalk-core/src/semantic_analysis/name_resolver.rs` | 3 | edit (comments only) | BT-5154 |
| `crates/beamtalk-core/src/semantic_analysis/block_facts/outer_writes.rs` | 3 | edit (comments only) | BT-5153 |
| `crates/beamtalk-core/src/semantic_analysis/block_facts/escape.rs` | 3 | edit | BT-5153 |
| `crates/beamtalk-core/src/ast_walker.rs` | 3 | edit (comments only) | BT-5152 |
| `crates/beamtalk-codegen/src/core_erlang/tests/threaded_locals_agreement.rs` | 3 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/primitives.rs` | 3 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/match_scrutinee_state_corpus.rs` | 3 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/field_assignment_value_position.rs` | 3 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/constant_class_methods.rs` | 3 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/mod.rs` | 3 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/driver.rs` | 3 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/dispatch_codegen/super_send.rs` | 3 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/dispatch_codegen/self_dispatch.rs` | 3 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/dispatch_codegen/ffi.rs` | 3 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/dispatch_codegen/class_reference.rs` | 3 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/control_flow/list_ops/fold_driver.rs` | 3 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/control_flow/inline_control_flow.rs` | 3 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/control_flow/analysis.rs` | 3 | edit (comments only) | BT-5153 |
| `crates/beamtalk-language-service/src/queries/return_value_fix_provider.rs` | 2 | edit (comments only) | BT-5154 |
| `crates/beamtalk-language-service/src/queries/folding_range_provider.rs` | 2 | edit (comments only) | BT-5154 |
| `crates/beamtalk-core/src/source_analysis/parser/tests/traits_tests.rs` | 2 | edit | BT-5152 |
| `crates/beamtalk-core/src/source_analysis/parser/protocols.rs` | 2 | edit (comments only) | BT-5152 |
| `crates/beamtalk-core/src/semantic_analysis/validators/local_threading/escapes.rs` | 2 | edit (comments only) | BT-5153 |
| `crates/beamtalk-core/src/semantic_analysis/type_checker/validation/construction.rs` | 2 | keep | - |
| `crates/beamtalk-core/src/semantic_analysis/type_checker/tests/self_type.rs` | 2 | edit | BT-5154 |
| `crates/beamtalk-core/src/semantic_analysis/type_checker/tests/basic_inference.rs` | 2 | edit | BT-5154 |
| `crates/beamtalk-core/src/semantic_analysis/trait_expansion/tests.rs` | 2 | edit | BT-5154 |
| `crates/beamtalk-core/src/semantic_analysis/alias_expansion.rs` | 2 | edit | BT-5154 |
| `crates/beamtalk-core/src/ast/mod.rs` | 2 | edit (comments only) | BT-5152 |
| `crates/beamtalk-codegen/src/core_erlang/value_type_codegen/conditionals.rs` | 2 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/util.rs` | 2 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/residual_dynamic_sends.rs` | 2 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/paren_transparency.rs` | 2 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/nlr.rs` | 2 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/guarded_op_loops.rs` | 2 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/ffi_delegate_direct_call.rs` | 2 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/ensure_relay_corpus.rs` | 2 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/nlr.rs` | 2 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/control_flow/list_ops/tests.rs` | 2 | edit | BT-5153 |
| `crates/beamtalk-cli/tests/cli/cli_run.rs` | 2 | edit | BT-5149 |
| `crates/beamtalk-cli/src/commands/build_stdlib/orchestrate.rs` | 2 | edit | BT-5154 |
| `crates/beamtalk-repl/tests/repl_codegen_smoke.rs` | 1 | edit | BT-5149 |
| `crates/beamtalk-lint/src/lib.rs` | 1 | edit (comments only) | BT-5154 |
| `crates/beamtalk-lint/src/dead_block_assignment.rs` | 1 | edit (comments only) | BT-5154 |
| `crates/beamtalk-language-service/src/tests/rename.rs` | 1 | edit | BT-5154 |
| `crates/beamtalk-language-service/src/rename.rs` | 1 | edit (comments only) | BT-5154 |
| `crates/beamtalk-language-service/src/queries/selector_rename_query.rs` | 1 | edit (comments only) | BT-5154 |
| `crates/beamtalk-language-service/src/queries/hover_provider.rs` | 1 | edit (comments only) | BT-5154 |
| `crates/beamtalk-language-service/src/queries/field_accesses_query.rs` | 1 | edit (comments only) | BT-5154 |
| `crates/beamtalk-language-service/src/lib.rs` | 1 | edit (comments only) | BT-5154 |
| `crates/beamtalk-core/src/unparse/tests/round_trip_and_idempotency.rs` | 1 | edit | BT-5152 |
| `crates/beamtalk-core/src/unparse/tests/canonical_source.rs` | 1 | edit | BT-5152 |
| `crates/beamtalk-core/src/unparse/comments.rs` | 1 | edit (comments only) | BT-5152 |
| `crates/beamtalk-core/src/source_analysis/parser/property_tests.rs` | 1 | edit | BT-5152 |
| `crates/beamtalk-core/src/source_analysis/parser/methods.rs` | 1 | edit (comments only) | BT-5152 |
| `crates/beamtalk-core/src/semantic_analysis/validators/local_threading/mod.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-core/src/semantic_analysis/type_checker/tests/bt4001_class_side_super.rs` | 1 | edit (comments only) | BT-5154 |
| `crates/beamtalk-core/src/semantic_analysis/type_checker/native_type_registry.rs` | 1 | edit (comments only) | BT-5154 |
| `crates/beamtalk-core/src/semantic_analysis/type_checker/inference/mod.rs` | 1 | edit | BT-5154 |
| `crates/beamtalk-core/src/semantic_analysis/module_validator.rs` | 1 | edit (comments only) | BT-5154 |
| `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/builtins.rs` | 1 | edit | BT-5154 |
| `crates/beamtalk-core/src/semantic_analysis/block_facts/mod.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-core/src/semantic_analysis/analyser.rs` | 1 | edit (comments only) | BT-5154 |
| `crates/beamtalk-core/src/ast/well_known.rs` | 1 | edit (comments only) | BT-5152 |
| `crates/beamtalk-core/src/ast/pipeline.rs` | 1 | edit (comments only) | BT-5152 |
| `crates/beamtalk-compiler-port/src/tests/expression_assignment.rs` | 1 | edit (comments only) | BT-5154 |
| `crates/beamtalk-codegen/src/core_erlang/value_type_codegen/body.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/threading_analysis.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/threaded_producers.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/threaded_ir/tests/mod.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/threaded_expr.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/mod.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/inline_block_local_in_loop_corpus.rs` | 1 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/gen_server/direct_class_self.rs` | 1 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/control_flow/mod.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/closure_self_send_state_guard_corpus.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/tests/arm_local_slot_verifier_corpus.rs` | 1 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/spec_codegen.rs` | 1 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/primitives/intrinsic_bodies.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/patterns/match_lowering.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/operators.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/intrinsics/mod.rs` | 1 | edit | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/intrinsics/logger.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/gen_server/state.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/error.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/ensure_nlr_relay.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/dispatch_codegen/spawn.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/dispatch_codegen/field_assignment.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/control_flow/plan/tests.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/control_flow/mod.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/control_flow/local_assign.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/control_flow/family_slots.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/control_flow/exception_bodies.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-codegen/src/core_erlang/control_flow/conditional_producers.rs` | 1 | edit (comments only) | BT-5153 |
| `crates/beamtalk-cli/tests/cli/main.rs` | 1 | edit | BT-5149 |
| `crates/beamtalk-cli/src/commands/release/assembly.rs` | 1 | edit (comments only) | BT-5163 |
| `crates/beamtalk-cli/src/commands/build_stdlib/tests/mod.rs` | 1 | edit | BT-5154 |

`type_checker/validation/construction.rs` is **keep**: `aClassVar new` there is a local variable that holds a class, not a class variable.

### 8.2 Erlang (63)

| File | Hits | Disposition | Owner |
|---|--:|---|---|
| `runtime/apps/beamtalk_runtime/test/beamtalk_class_vars_tests.erl` | 226 | delete | BT-5163 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_object_class.erl` | 105 | edit | BT-5160 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_class_vars.erl` | 90 | delete | BT-5163 |
| `runtime/apps/beamtalk_runtime/test/beamtalk_class_dispatch_tests.erl` | 75 | edit | BT-5160 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_class_metadata.erl` | 73 | edit | BT-5158 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_class_dispatch.erl` | 72 | edit | BT-5160 |
| `runtime/apps/beamtalk_runtime/test/beamtalk_object_class_tests.erl` | 65 | edit | BT-5160 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_class_shadow_flags.erl` | 53 | edit | BT-5158 |
| `runtime/apps/beamtalk_runtime/test/beamtalk_class_in_caller_tests.erl` | 38 | edit | BT-5158 |
| `runtime/apps/beamtalk_runtime/test/beamtalk_class_var_abi_tests.erl` | 35 | delete | BT-5163 |
| `runtime/apps/beamtalk_runtime/test/beamtalk_erlang_proxy_tests.erl` | 34 | edit | BT-5159 |
| `runtime/apps/beamtalk_runtime/test/beamtalk_class_direct_call_ok_tests.erl` | 29 | edit | BT-5158 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_class_var_errors.erl` | 26 | delete | BT-5163 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_class_var_abi.erl` | 23 | delete | BT-5163 |
| `runtime/apps/beamtalk_runtime/test/beamtalk_class_registry_tests.erl` | 21 | edit | BT-5160 |
| `runtime/apps/beamtalk_test_support/src/beamtalk_class_vars_test_support.erl` | 20 | delete | BT-5163 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_class_registry.erl` | 19 | edit | BT-5160 |
| `runtime/apps/beamtalk_workspace/test/beamtalk_repl_eval_tests.erl` | 17 | edit | BT-5159 |
| `runtime/apps/beamtalk_stdlib/test/beamtalk_result_tests.erl` | 17 | edit | BT-5159 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_behaviour_intrinsics.erl` | 17 | edit | BT-5160 |
| `runtime/apps/beamtalk_runtime/test/beamtalk_class_dispatch_test_helper.erl` | 16 | edit | BT-5160 |
| `runtime/apps/beamtalk_runtime/test/beamtalk_class_shadow_flags_tests.erl` | 15 | edit | BT-5158 |
| `runtime/apps/beamtalk_runtime/test/beamtalk_class_metadata_tests.erl` | 15 | edit | BT-5158 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_erlang_proxy.erl` | 14 | edit | BT-5159 |
| `runtime/apps/beamtalk_runtime/test/beamtalk_supervisor_tests.erl` | 11 | edit | BT-5159 |
| `runtime/apps/beamtalk_runtime/include/beamtalk_class_vars_keys.hrl` | 10 | delete | BT-5163 |
| `runtime/apps/beamtalk_runtime/test/beamtalk_behaviour_intrinsics_tests.erl` | 9 | edit | BT-5160 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_release_shapes.erl` | 9 | edit | BT-5163 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_class_publish.erl` | 9 | edit | BT-5160 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_class_builder.erl` | 8 | edit | BT-5160 |
| `runtime/apps/beamtalk_stdlib/src/beamtalk_result.erl` | 5 | edit | BT-5159 |
| `runtime/apps/beamtalk_runtime/test/beamtalk_native_api_tests.erl` | 4 | edit | BT-5160 |
| `runtime/apps/beamtalk_runtime/test/beamtalk_class_derived_cache_tests.erl` | 4 | edit | BT-5158 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_telemetry.erl` | 4 | edit | BT-5159 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_runtime_api.erl` | 4 | edit | BT-5160 |
| `runtime/apps/beamtalk_workspace/test/beamtalk_repl_loader_abi_tests.erl` | 3 | edit | BT-5159 |
| `runtime/apps/beamtalk_workspace/src/beamtalk_repl_eval.erl` | 3 | edit | BT-5159 |
| `runtime/apps/beamtalk_stdlib/src/beamtalk_file.erl` | 3 | edit | BT-5159 |
| `runtime/apps/beamtalk_runtime/test/beamtalk_release_shapes_tests.erl` | 3 | edit | BT-5163 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_supervisor_init_config.erl` | 3 | edit | BT-5162 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_supervisor.erl` | 3 | edit | BT-5159 |
| `runtime/apps/beamtalk_workspace/test/beamtalk_test_native_facade.erl` | 2 | edit | BT-5159 |
| `runtime/apps/beamtalk_test_support/src/beamtalk_test_assert.erl` | 2 | edit | BT-5159 |
| `runtime/apps/beamtalk_stdlib/src/beamtalk_test_case.erl` | 2 | edit | BT-5159 |
| `runtime/apps/beamtalk_runtime/test/beamtalk_class_instantiation_tests.erl` | 2 | edit (comments only) | BT-5160 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_supervisor_class_call.erl` | 2 | edit | BT-5162 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_native_api.erl` | 2 | edit | BT-5160 |
| `runtime/apps/beamtalk_compiler/test/beamtalk_compiler_server_tests.erl` | 2 | edit | BT-5163 |
| `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_sup.erl` | 1 | edit | BT-5159 |
| `runtime/apps/beamtalk_stdlib/src/beamtalk_ets.erl` | 1 | edit (comments only) | BT-5159 |
| `runtime/apps/beamtalk_runtime/test/beamtalk_test_native_counter_facade.erl` | 1 | edit | BT-5159 |
| `runtime/apps/beamtalk_runtime/test/beamtalk_object_printer_tests.erl` | 1 | edit (comments only) | BT-5160 |
| `runtime/apps/beamtalk_runtime/test/beamtalk_class_sup_tests.erl` | 1 | edit (comments only) | BT-5160 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_runtime_app.erl` | 1 | edit (comments only) | BT-5160 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_dispatch.erl` | 1 | edit | BT-5160 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_class_monitor.erl` | 1 | edit (comments only) | BT-5158 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_class_dispatch_self.erl` | 1 | edit | BT-5160 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_class_derived_cache.erl` | 1 | edit | BT-5158 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_call_failure.erl` | 1 | edit | BT-5160 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_actor_state.erl` | 1 | edit | BT-5160 |
| `runtime/apps/beamtalk_runtime/src/beamtalk_actor.erl` | 1 | edit | BT-5160 |
| `runtime/apps/beamtalk_runtime/include/beamtalk.hrl` | 1 | edit (comments only) | BT-5160 |

### 8.3 Files that need two issues, and ordering notes

- `beamtalk_class_dispatch.erl`, `beamtalk_class_shadow_flags.erl`: BT-5141 (ADR 0152) is editing both now. BT-5158 and BT-5160 start after it merges (ADR 0153 § Implementation). BT-5161 (guard split) touches the same two files and must follow BT-5158.
- `beamtalk_object_class.erl`: BT-5160 (`get/set_class_var`, `class_state` field), BT-5162 (`local_call/3` loses `with_snapshot/2`; `local_call/3` itself stays, the migration runner calls it at `beamtalk_shape_migration.erl:306`).
- `beamtalk_class_registry.erl`: BT-5160 (class-state field rows) and BT-5162 (the class-state snapshot rows).
- `beamtalk_supervisor.erl`: line 391 `protect/1` is BT-5159; the comment at 2110 follows.
- `crates/beamtalk-codegen/src/core_erlang/class_meta.rs:454` emits `class_var_abi` into every module's `__beamtalk_meta/0`; BT-5153 deletes the producer (`class_var_keys.rs`), BT-5163 owns what replaces the ABI check (§ 11, finding F3).

### 8.4 Not Rust or Erlang

| Path | What | Owner |
|---|---|---|
| `docs/beamtalk-language-features.md`, `beamtalk-native-erlang.md`, `beamtalk-packages.md`, `beamtalk-principles.md`, `beamtalk-syntax-rationale.md`, `beamtalk-tooling.md`, `stdlib-implementation-status.md` | prose | BT-5157 |
| `docs/development/{erlang-guidelines,class-var-catcher-audit,actor-state-cell,benchmarks,environment-variables,debugging,surface-parity,testing-strategy}.md`, `docs/agents/expanded.md`, `AGENTS.md`, `CHANGELOG.md` | prose, rules | BT-5157 (`class-var-catcher-audit.md` is deleted by BT-5159) |
| `docs/ADR/*.md` (49 files mention class variables) | annotate 0013, 0019, 0084, 0109, 0111, 0123, 0124, 0127, 0129, 0130, 0133, 0137, 0142 as the ADR lists | BT-5157 |
| `.github/workflows/fuzz.yml` (job `class-var-corpus`), `fuzz-triage.yml`, `.github/claude-fuzz-triage-prompt.md`, `.github/scripts/repro-class-var.sh`, `ci-platforms.yml` | nightly job and triage path; `ci-platforms.yml` is not in the ADR's list: read it | BT-5151 |
| `Justfile` (`test-class-var-corpus`, `lint-class-var-catchers`), `scripts/ci/lint-class-var-catchers.escript`, `scripts/ci/fixtures/lint-class-var-catchers/*`, `scripts/ci/lint-workaround-comments.sh` | recipes and lint | BT-5151 (corpus), BT-5159 (catcher lint) |
| `editors/vscode/syntaxes/beamtalk.tmLanguage.json`, `editors/github-chrome/test/tokenize.test.js` | highlight `classState:` as a keyword | BT-5152 decides: the parser keeps the arm to emit the error, so the highlighter can too |
| `stdlib/src/{behaviour,class_builder,system_navigation,process_dictionary,proto_object,block,telemetry}.bt` | `classVarNames`, `allClassVarNames`, `performLocally:`, class-side slot branches, doc comments | BT-5155 |

## 9. Downstream search

`classState:`, `performLocally:`, `classVarNames` and `classVars:` over the 13 packages named in the ADR (agents, announcements, buccaneer, catalina, hpack, http, linear, mint, oauth, yaml, symphony, exdura, liquid) and `beamtalk-uat`, plus `collections`, `registry`, `watcher` (all `jamesc/*` repositories). Run with `gh api -X GET search/code -f q='<term> user:jamesc'` on 2026-11-24, which searches default branches only and can lag a push: **re-run it immediately before Phase 2, as the ADR says.**

| Term | Declarations | Other hits |
|---|---|---|
| `classState:` | **0** in any `src/` | `AGENTS.md` of 15 repositories (the shared sentence "Class variables shared by all instances are declared with `classState:`"); `beamtalk-buccaneer` `src/exchange.bt` and its engine spike `spike_exchange.bt` (doc comment: "a class method with no classState: runs in the caller's process") |
| `classVarNames`, `classVars:` | 0 | none |
| `performLocally:` | 0 declarations | **`beamtalk-exdura`: `src/workflow/workflow_runner.bt` (doc comment recommending `performLocally:` "to avoid class object contention") and 8 tests: `test/workflow/{replay_proof,versioning,child_workflow,workflow_engine,workflow_options}_test.bt`, `test/event_store/snapshot_test.bt`, `test/timer/durable_timer_test.bt`** (all `WorkflowRunner performLocally: #await:timeout: withArguments: #(semName, 10000)`) |
| `beamtalk-uat` | 0 for all four | none |

**Action before Phase 2 (report on BT-5145; BT-5155 and BT-5162 must not land before this is fixed).** `beamtalk-exdura` calls `performLocally:` in 8 test files and recommends it in a doc comment. ADR 0153 Decided Q3 deletes it. The migration row is `WorkflowRunner perform: #await:timeout: withArguments: #(...)` (runs in the caller for every selector that is not process-bound). If `await:timeout:` is not process-bound (the doc comment calls it a class-side wait; its definition was not read), that is a mechanical edit in `beamtalk-exdura`. That repository needs its own issue; it is not created here because no issue tracker for it was part of this task.

The 15 `AGENTS.md` files carry the class-variable sentence; they are guidance text, not code.

## 10. Checks

### Check 1: a literal dictionary, list or tuple in a method body is one constant-pool term

**Holds for dictionary and list literals; there is no tuple literal.** Compiled with the repo's compiler (`target/debug/beamtalk build`) and disassembled with `beam_disasm:file/1`:

```beamtalk
Object subclass: Consts
  class reasons -> Dictionary(Integer, String) =>
    #{200 => "OK", 404 => "Not Found", 500 => "Internal Server Error"}
  class primes -> List(Integer) => #(2, 3, 5, 7, 11)
  class pair -> Tuple => #{#a => 1, #b => #(1, 2, 3)}
  class arr -> Array(Integer) => #[1, 2, 3]
```

| Method | Core Erlang | BEAM |
|---|---|---|
| `reasons` | `~{ 200 => {'beamtalk_string', #{...}#}, 404 => ..., 500 => ... }~` | `{move,{literal,#{200 => {beamtalk_string,<<"OK">>}, 404 => {beamtalk_string,<<"Not Found">>}, 500 => {beamtalk_string,<<"Internal Server Error">>}}},{x,0}}` then return: **one literal** |
| `primes` | `[2, 3, 5, 7, 11]` | `{move,{literal,[2,3,5,7,11]},{x,0}}`: **one literal** |
| `pair` (nested map and list) | `~{ 'a' => 1, 'b' => [1, 2, 3] }~` | `{move,{literal,#{b => [1,2,3],a => 1}},{x,0}}`: **one literal** |
| `arr` (`#[...]`) | `call 'beamtalk_array':'from_list'([1, 2, 3])` | the literal list **plus a call**: rebuilt on every call, as the ADR says |

A string element is `{beamtalk_string, <<...>>}`, which is a literal too. Beamtalk has no tuple *literal* expression: `docs/beamtalk-language-features.md` only produces a `Tuple` from FFI results and `spawnMonitored`, so the ADR's "tuple" cannot appear in a method body; it is moot. An expression with a non-literal element (a variable, a send) is **not** folded; this was not measured here, only the all-literal case. The full `.core` and `.beam` stay in the scratchpad; they are not checked in.

### Check 2: nothing sets a class variable through `set_class_var/3` outside its own definition and `beamtalk_runtime_api`

**Holds.** `grep -rnE 'set_class_var' runtime crates scripts stdlib`: the definition is `beamtalk_object_class.erl:316-323` (`set_class_var/3`, handled at `:1215` as `{set_class_var, Name, Value}`), the re-export is `beamtalk_runtime_api.erl:54, 278-280`. The remaining hits are comments in `beamtalk_class_vars.erl:82` and `beamtalk_object_class.erl:1681, 1721`, and **EUnit calls that go through the gen_server message** `gen_server:call(Pid, {set_class_var, ...})` in `beamtalk_object_class_tests.erl:1819, 1834, 1874, 1879` and the direct `beamtalk_object_class:set_class_var/3` calls in the same file (`:2005, :2016`). No `runtime/apps/*/src` caller, no `crates/` caller, no `stdlib/` caller. (`set_class_vars/2` in `beamtalk_class_shadow_flags` and `beamtalk_class_metadata:put_class_variables/2` are different functions; `stdlib/test/guarded_static_call_test.bt:89-112` calls `Erlang beamtalk_class_shadow_flags set_class_vars:` and is listed in § 3.2.)

### Check 3: the build stamp forces a recompile of dependency BEAMs and workspace-persisted BEAMs

**Holds for the CLI paths. Does not by itself protect a runtime that loads an old BEAM; see F3.**

- **Project build** (`beamtalk build`, `run`, `test`): `provenance_requires_rebuild` (`crates/beamtalk-cli/src/commands/build/mod.rs:327-329, 757-780`) forces a full rebuild when the stamp's `beamtalk_version` or OTP version differs, and the stamp is written last (`:839-845`). The key is `BEAMTALK_VERSION` verbatim, `-dev+<sha>` included, plus `BEAMTALK_DIRTY_TAG` for an uncommitted tree (`build_stamp.rs:40-56`).
- **Dependency BEAMs**: `deps/mod.rs:594-602` treats a dependency whose stamp is missing, unreadable or from another toolchain as stale and rebuilds it; `deps/path.rs:491-497` writes the dependency stamp last. Pre-stamp dependencies are rebuilt once.
- **Workspace-persisted BEAMs**: the workspace stores class *source* (`beamtalk_workspace_meta:set_class_source/2`) and recompiles it with the running compiler, so those are never old BEAMs. What it loads from disk is the project's `_build` and `_build/deps/` through `activate_dependency_modules/1` (`beamtalk_workspace_bootstrap.erl:121-128` → `beamtalk_module_activation:activate_dependencies/2`). That path does **not** read a stamp. It is covered because `beamtalk repl` first runs the project build (`crates/beamtalk-cli/src/commands/repl/mod.rs:121`), which is stamp-gated. A workspace started some other way, over an unbuilt-since-upgrade `_build/deps/`, is covered only by the runtime's `class_var_abi` load gate. Whether the detached `beamtalk workspace` start also builds first was **not** traced.

## 11. Findings that need a decision or a note in the owning issue

- **F1. `beamtalk-lint/src/actor_state_abroad.rs` reuses the class-variable analysis.** It imports `ClassVarAccesses` / `class_var_accesses` from the `class-state-abroad` validator and says so: "`class_var_accesses` is generic over the name set it is given: here the actor's mutable instance fields, not class variables" (`actor_state_abroad.rs:55-56, 101, 459`). BT-5154 deletes `validators/class_state_abroad.rs`; deleting it as written breaks the actor lint. Per the repo's "no duplicate implementations" rule, extract the generic name-set walker into a shared leaf module below both before deleting the validator. Rename it: it is not about class variables any more.
- **F2. Rows 3g and 3h of the vs-elixir benchmark do not declare `classState:`.** They fake it with `put_class_variables/2` (`vs_bench.erl:351-355`), so `grep classState:` misses them. They go with BT-5158 (the function is deleted).
- **F3. The `class_var_abi` gate will not refuse a module built before ADR 0153.** `ABI_VERSION` is 3 (`class_var_keys.rs:55`) and `__beamtalk_meta/0` carries it. (Inferred from reading the gate; not run.) After Phase 3 deletes `beamtalk_class_vars`, an old module still passes the gate (its ABI equals the runtime's) and then fails with `undef` at its first `on:do:` (`beamtalk_class_vars:snapshot/0`). The ADR's mitigation ("its check moves to the remaining module-load check") only works if the expected value changes: BT-5153 or BT-5163 must bump `ABI_VERSION` (to 4) in the same change that stops emitting snapshots, or the loader must reject a module that carries `class_var_abi` at all. `release/upgrade.rs:52-56, 380-386` and `release/assembly.rs:1065` read the field too (`--upgrade-from` compares it), and the test native facades (`beamtalk_test_native_facade.erl:35`, `beamtalk_test_native_counter_facade.erl:21`, `beamtalk_repl_loader_abi_tests.erl`, `beamtalk_release_shapes_tests.erl:59-61, 141`) hard-code it.
- **F4. Six fixtures pin class-process-only behaviour** (`bt4290*`, `bt4262`, `bt4252*child`): decide between "delete" and "reach it through a process-bound selector" in BT-5148 (§ 3.1).
- **F5. `performLocally:` has a downstream user** (§ 9).
- **F6. `late_slots.btscript` and `perform_locally.btscript` are REPL-protocol cases.** They change REPL-visible behaviour; AGENTS.md says to confirm REPL output changes with the owner. ADR 0153 is that decision for these; no further confirmation is needed, noted so nobody stops on it.
- **F7. The ADR's file counts (145 / 60) cannot be reproduced** (§ 1). Use the tables.

### `lint-class-var-catchers`, and the `protect/1` call-site list

`just lint-class-var-catchers` on this commit: *"self-test: 4 fixture(s) behave as expected"* and *"Class-variable catcher audit: 89 catching site(s) carry a bt-catcher-audit marker."*

Call sites of the class-variable API, from `grep -rnE 'beamtalk_class_vars:(snapshot|restore|with_snapshot|protect)\(' runtime/apps/*/src` (comments excluded):

| Call | Site |
|---|---|
| `protect/1` | `beamtalk_result.erl:228`, `:285`; `beamtalk_supervisor.erl:391`; `beamtalk_telemetry.erl:317`; `beamtalk_test_case.erl:76`; `beamtalk_test_assert.erl:48`; `beamtalk_repl_eval.erl:842` |
| `snapshot/0`, `restore/1` pair | `beamtalk_erlang_proxy.erl:436` (snapshot read by the call site), `:511` (snapshot), `:520` (restore) |
| `with_snapshot/2` | `beamtalk_supervisor_class_call.erl:63`; `beamtalk_supervisor_init_config.erl:135`; `beamtalk_object_class.erl:474` (`local_call/3`) |

This **confirms** the ADR's Phase 3 list exactly: the 7 `protect/1` sites (`result` ×2, `supervisor`, `telemetry`, `test_case`, `test_assert`, `repl_eval`), the proxy pair, and the three `with_snapshot/2` sites (BT-5162). Nothing is missing and nothing extra is called. The lint's own fixtures call the API too (`scripts/ci/fixtures/lint-class-var-catchers/clean.erl:12, 20, 28, 37`) and go with the lint. Ownership: `protect/1` and the proxy pair are BT-5159; `with_snapshot/2` is BT-5162.


## 12. Baseline benchmark

Recorded on `b464155` before anything is removed, in [`benchmarks/vs-elixir/results/bench-bt5145-baseline.md`](../../benchmarks/vs-elixir/results/bench-bt5145-baseline.md) and summarised in [`benchmarks.md`](benchmarks.md). It holds the vs-elixir rows 3a-3j, 8a, 8b, the dispatch-overhead rows e1, e2, c1-c5, the `self_send_bench` `on:do:` rows, and a direct timing of `beamtalk_result:tryDo/1` (no benchmark row exists for `Result tryDo:`). The "after" run belongs to BT-5164 and the ADR's Phase 3 gate.

Headline numbers (dev build, 4-core container, noisy time, deterministic reductions):

| | before |
|---|---|
| 3a-3c, a class send that is a guarded direct call | 3.0 reductions, 0 words, 20-26 ns |
| 3d, class in a variable | 24.1 reductions, 42 words |
| 3f, class with `classState:` | 140 reductions, 167 words (goes to the class process) |
| 8a, `on: Error do:` without a raise | 5.0 reductions, 67.7 ns (Elixir `try`: 0 reductions, 4.3 ns) |
| 8b, `on: Error do:` with a raise | 99.1 reductions, 167 words, 2210 ns |
| `beamtalk_result:tryDo/1`, block answers | 23.0 ns (a bare `try`: 8.4 ns) |
