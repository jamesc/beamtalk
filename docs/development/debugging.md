# Debugging Workflow

Step-by-step debugging for common failures in the Beamtalk compiler and runtime.

## Compiler Crashes

```bash
# 1. Enable panic backtraces
RUST_BACKTRACE=1 beamtalk build failing.bt

# 2. Identify which phase failed
# Lexer error:     "unexpected character at line X, column Y"
# Parser error:    "expected X, found Y"
# Codegen error:   "failed to generate code for ..."

# 3. Create minimal repro case
echo "minimal failing code" > test.bt
beamtalk build test.bt

# 4. Add debug output in relevant layer
# For parser: add dbg!(&ast) in crates/beamtalk-core/src/source_analysis/parser/mod.rs
# For codegen: add dbg!(&expr) in crates/beamtalk-codegen/src/core_erlang/
```

## Runtime Errors

```bash
# 1. Inspect generated Core Erlang
cat build/module_name.core | less

# Look for:
# - Function definitions ('functionName'/Arity)
# - Pattern matches (case ... of)
# - Error calls (call 'erlang':'error')
# - `-|` source annotations on function heads and message sends
#   (`( Expr -| [Line, {'file', "path.bt"}] )` — BT-3127), which carry
#   the .bt file/line into the BEAM Line chunk and runtime stack traces

# 2. Test generated BEAM in Erlang shell
# Single-file build outputs to ./build with bt@ prefix
erl -pa build
1> 'bt@module_name':function_name(Args).

# 3. Enable Erlang debug traces
2> dbg:tracer().
3> dbg:p(all, c).
4> dbg:tpl('bt@module_name', '_', []).
5> 'bt@module_name':function_name(Args).
```

Stack traces from compiled `.bt` code (both raw Erlang `erlang:get_stacktrace/0`-style
frames and Beamtalk's `StackFrame` objects, see `e stackTrace` in
`stdlib/src/StackFrame.bt`) report the original `.bt` file and line number for
function heads and message-send call sites — not the compiled module's own
name or line 1 — as long as the compilation unit had a source path (any
`.bt` file built via `beamtalk build`/`beamtalk test`; REPL-evaluated code
with no backing file falls back to a bare line number, no file).

## Test Failures

```bash
# 1. Run single test with output
cargo test test_name -- --nocapture

# 2. Check what the test expects
# - Snapshot test: see tests/snapshots/*.snap
# - REPL-protocol test: see tests/repl-protocol/cases/*.btscript
# - Unit test: read test source

# 3. Update snapshots if intentional
cargo test test_name
# Review changes in git diff
cargo insta accept

# 4. Run all tests in module
cargo test --test module_name
```

## REPL-Protocol Test Failures

```bash
# 1. Check REPL daemon logs
just test-repl-protocol 2>&1 | tee repl-protocol.log
grep "ERROR\|Warning\|failed" repl-protocol.log

# 2. Test fixture manually
cd tests/repl-protocol/fixtures
../../target/debug/beamtalk build counter.bt
cat build/counter.core

# 3. Run REPL interactively
beamtalk repl
> :load tests/repl-protocol/fixtures/counter.bt
> Counter spawn
> c increment

# 4. Check expected output in test file
cat tests/repl-protocol/cases/actors.btscript
# Look for // => expected output comments
```

## Codegen Debugging

```bash
# 1. Generate and inspect Core Erlang
beamtalk build failing.bt
cat build/failing.core

# 2. Look for suspicious patterns:
# - Missing State/Self parameters
# - Unbound variables (StateX, State1, etc.)
# - Wrong function arities
# - Call to undefined functions

# 3. Compare with working example
beamtalk build examples/counter.bt
diff build/counter.core build/failing.core

# 4. Add codegen debug output
# Edit crates/beamtalk-codegen/src/core_erlang/expressions.rs
dbg!(&expr);
// Rebuild and check output
```

### core_lint (BT-3115)

Every scoping bug in the list above — unbound `StateX`, missing `State`/`Self`,
wrong arities — is a Core Erlang well-formedness violation that OTP's
`core_lint` pass catches during `beamtalk build`, with a message naming the
offending variable and function/arity directly:

```
build/failing.core: unbound variable 'State' in myMethod/2
```

`core_lint` runs **unconditionally** as part of `compile:forms`/`compile:file`'s
`from_core` pipeline (every Beamtalk compile path takes this route) —
independent of the `clint`/`clint0`/`no_lint` options, which only affect lint
re-runs when compiling from Erlang *source*, a path Beamtalk never takes. So
this check was always on; what changed in BT-3115 was making the failure
readable everywhere it can surface:

- **Port backend** (`beamtalk_build_worker`/`beamtalk_compiler_server`, the
  default): formats `compile:forms`' raw error term via
  `beamtalk_compile_diagnostics:format_errors/1`, which calls the same
  `sys_messages:format_messages/4` OTP itself uses internally.
- **Escript backend** (`BEAMTALK_COMPILER=escript`, `compile.escript`):
  formats the same way and prints to this process's actual stderr — a prior
  version relied on `compile:file`'s `report_errors`/`report_warnings`
  options, which print via the compiling process's default group leader;
  that lands on **stdout**, not stderr, so the Rust CLI's stdout parser
  (which only recognises the `beamtalk-compile-*` protocol markers) silently
  dropped the message rather than showing it.

If you see a bare `internal error` or a build failure with no readable cause,
run with `BEAMTALK_COMPILER=escript` and compare — the two backends are
expected to produce the same wording for the same malformed input.

### ThreadedIr verifier (ADR 0111, BT-3129-BT-3165, BT-3164, BT-3166-BT-3170)

State threading — actor/instance `State`, class-var `ClassVars`, value-type
`Self`, loop-local threading, and non-local-return (NLR) relay — used to be
coordinated only by scattered `debug_assert!`s at each emission site, each
independently re-deriving the same invariants. `crates/beamtalk-codegen/src/core_erlang/threaded_ir.rs`
replaces that with a small mid-level IR (`ThreadedIr`/`ThreadedStmt`) that IS
the `Document` emission for every construct family this table covers,
including exception handling's `on:do:`/`ensure:` (BT-3165, the last
holdout), and a single `verify()` pass per construct/method that checks it
before `render()` turns it into the `Document` the caller emits. A
violation is a `threaded_ir::VerifyError`, reported through the
shared `report_threaded_ir_verify_errors` helper (`control_flow/mod.rs`,
`pub(super)`) — a `debug_assert!` in debug/CI builds (hard failure, `just
verify-threaded-ir` runs the whole `stdlib/test/*.bt` +
`stdlib/bootstrap-test/*.btscript` corpus through this path specifically to
catch it) and an `internal:` error diagnostic in release builds (compile
still succeeds; the diagnostic is the only signal). This is diagnosis
*earlier and more precisely attributed* than `core_lint` above — most of
these invariants would otherwise surface, if at all, as a `core_lint`
unbound-variable or badarg error one layer further from the Beamtalk source
that caused it. See ADR 0111 §The verifier / §Verifier honesty for what a
verifier can and cannot catch.

If a `VerifyError` fires, the variant tells you which invariant broke and
where to start reading:

| Variant | Means | Look at |
|---|---|---|
| `UnboundVersion` | A versioned var (e.g. `State2`) was referenced with no producing `Bind` in its frame or an ancestor frame on the frame stack. | Whatever emission path built the `ThreadedIr` fragment around the failing construct — it referenced a version it never bound. Live against real per-arm/per-method IR everywhere, including `exception_handling.rs`'s `on:do:`/`ensure:` arms as of BT-3165. |
| `NonLinearVersion` | Within one `FrameId`, a version was produced by more than one `Bind`, or consumed as the source of more than one successor — frame-scoped SSA-like linearity broken. | The generator for that frame; likely a duplicate `Bind` or a version reused across two branch arms that should have gotten distinct `FrameId`s. Live everywhere `UnboundVersion` is. |
| `ThreadingModeUnpackMismatch` | An optimized `ThreadingMode` (a mode chosen specifically because it needs no `StateAcc` unpack) contains an unpack `Bind` anyway. | `while_loops.rs` / `counted_loops.rs`'s mode-selection logic — `ThreadingPlan::generate_unpack_at_iteration_start`'s `if !use_direct_params && !use_hybrid_params` guard (`control_flow/mod.rs`) is what makes this invariant hold structurally; BT-3154 deleted the per-call-site `check_loop_unpack_invariant`/`verify_loop_unpack_invariant` wrapper that used to check it explicitly, since `verify()`'s general `ThreadingModeUnpackMismatch` check was redundant with that guard. |
| `ShadowWriteMissing` | A class-var `Bind` at a shadow-write-eligible point (per the enclosing `Threaded`/`ConditionalLoop` nodes' `shadow_write_eligible` stack — ADR 0111 Addendum 9, not `FrameId` as of BT-3167) inside a method whose body can relay a foreign NLR (an `NlrCatch` with `boundary: ClassMethod { has_class_vars: true }`) lacks `shadow_write: true` — the ADR 0110 contract. | `expressions.rs`'s class-var assignment emission path (BT-3148, real `Bind` producer) and `gen_server/methods.rs`'s method-body backfill (`verify_body_with_opaque_version_gaps`) — a future change dropped the shadow write ADR 0110's fix depends on, or added a new class-var mutation site without it. As of BT-3164, `verify_body_with_opaque_version_gaps` backfills both `State`- and `ClassVars`-prefix gaps (`backfill_opaque_version_gap`), and `gen_server/methods.rs::lower_class_method_body` promotes a class method's own last-statement `self.classVar := value` to a real `Bind` — the shape that first lets this variant see a real class-var `Bind` jointly with a real class-method `NlrCatch` over the method's actual emitted IR, not just the isolated synthetic-marker fixture `construct_and_verify_class_var_bind` has always checked. |
| `TupleAccUnpackModeMismatch` | A `ThreadedStmt::TupleAccUnpack` node (flat positional-unpack accumulator) appeared outside a `ThreadingMode::TupleAcc` body. | `list_ops/*.rs` / `dict_ops.rs`'s `ThreadingPlan::generate_tuple_unpack_docs` — the tuple-shaped sibling of `ThreadingModeUnpackMismatch`. |
| `EarlyExitGateSlotMismatch` | A `TupleAccUnpack` node's own `gate_slots` disagrees with its enclosing `ThreadingMode::TupleAcc`'s `gate_slots` — the unpack would read threaded-local values from the wrong tuple positions (well-formed Core Erlang, silently *wrong values*, not a `core_lint` failure). | The list-op family's slot count in `list_ops/*.rs` (`do:`: 0; `collect:`/`select:`/boolean-predicate ops: 1; `takeWhile:`/`dropWhile:`/`detect:`-family: 2). Live since BT-3147 — `mode_gate_slots` (from `ListOpKind::gate_slots`, a canonical per-op table) and `node_gate_slots` (from each call site's own `index_offset - 1`) are genuinely independent sources now. |
| `TupleAccInValueTypeContext` | `TupleAcc` mode was selected in a `ValueType` context, which has no actor `State` to reference — regression-pinning, `#[cfg(test)]`-only (unreachable today via `select_tuple_acc`'s own early-return; no production constructor). | `control_flow/mod.rs`'s `select_tuple_acc` guard ordering. |
| `NestedStateAccFallbackUnderDirectParams` | A nested list-op that itself needs a `StateAcc`-map fallback appeared under an enclosing `DirectParams` loop, which has no `StateAcc` map for the inner `{value, StateAcc}` result to unpack into. Regression-pinning, `#[cfg(test)]`-only (unreachable today via `select_direct_params`'s own guard; no production constructor). | `control_flow/mod.rs`'s `select_direct_params`'s `!effects.has_non_tuple_safe_list_op` guard. |
| `StateEffectEscapesExpression` | A `ThreadedValue` (ADR 0118) whose prelude carries a versioned `Bind` for `prefix` was `close()`d — rendered as nested `let`s around its value — in a context that cannot thread that prefix (`CloseContext::Opaque`), so the state effect the expression performed (a nested actor self-send's `NewState`, say) is scoped away and lost to everything after it: the "silent drop" class of bug as a verifier finding. | The consumer that called `close()` — it should *splice* the prelude into its own frame's IR instead (`stmts.extend(tv.prelude)`, as every `lower_body_exprs_with_reply` arm does since BT-3415), or, at a genuine boundary (a Tier 1 closure body, an FFI argument, a block passed to a class method, spec/doc codegen), surface a user-facing diagnostic built from this error. Constructed only by `ThreadedValue::close`; every ADR 0118 phase (1a-4, plus 5a/5b/6's `ClassVars` consolidation) has landed, so every expression-position consumer now splices its prelude — `close()` itself still has no production caller as of this writing (`expression_doc` deliberately stays a plain forwarder per ADR 0118 §Decision 5: it is reached only by genuinely un-migrated, self-contained-`Document` boundaries, not by any position this table's matrix rows cover). Wiring `close()`'s `StateEffectEscapesExpression` into a user-facing diagnostic at one of those genuine boundaries (`check_no_unsafe_class_method_self_sends`) is tracked separately as a follow-up (BT-3430), not part of this migration. |

`RoutingMismatch` (BT-3135's structural replacement for the two
`gen_server/methods.rs` routing `debug_assert!`s) was itself deleted by
BT-3148 (ADR 0111 Addendum 4): `classify_body_expr`'s upfront classification
is now the *only* computation deciding whether a construct routes through
the shared Actor `threaded_expr.rs` emitter (`emit_actor_threaded_last_stmts`/
`emit_actor_threaded_assign_rhs_stmts`, which never decline) — there is no
second, independently-computed recheck left to disagree with the first, so
the mismatch this variant caught is unrepresentable by construction.

**`ClassVars` threading through loop/fold bodies (BT-3155 epic: BT-3166-BT-3170).**
`ShadowWriteMissing`'s frame model above (BT-3167) is what let two more
`ThreadedIr` node kinds start producing real class-var `Bind`s instead of
rejecting them at compile time: `ConditionalLoop` (BT-3168, `whileTrue:`/
`timesRepeat:`/`to:do:`/`to:by:do:` — `Letrec` bodies) now threads
`ClassVars` as an extra fun parameter through the loop's own recursive tail
call, gated by the same `shadow_write_eligible` the loop node carries; the
`Foldl*` accumulator (BT-3169, `do:`/`collect:`/`select:`/`inject:into:`/
list-op and dict-op bodies) becomes a `{ClassVars, StateAcc}` 2-tuple
whenever a body threads `ClassVars`, so `EarlyExitGateSlotMismatch`'s
`gate_slots` count stays untouched by `ClassVars`'s presence — modeled as
an orthogonal bool, not an extra gate slot (see ADR 0111 Addendum 9,
Question 6). Neither migration added a new `VerifyError` variant — both
route through the existing `UnboundVersion`/`NonLinearVersion`/
`ShadowWriteMissing` checks against the now-real `Bind`s these node kinds
produce, the same "no net-new detection, just a wider real-IR surface"
pattern BT-3164's class-method-body pipeline established. The one shape
still rejected at compile time is a loop/fold body whose only mutation is
the class-var write/self-send itself, with no other local mutation to
trigger state threading in the first place — see `docs/beamtalk-language-features.md`'s
"Passing Blocks Through Class Methods" section for the user-facing version
of that boundary, and ADR 0111 Addendum 9 for the full six-question design
this migration implements.

**The `whileTrue:`/`whileFalse:` condition as real IR (ADR 0118 phase 3,
BT-3419).** `ConditionalLoop` no longer treats its condition as an opaque,
outside-the-frame `Document` (the pre-BT-3419 `continue_header` field): it
now carries `condition: Vec<ThreadedStmt>` (the condition block's own
prelude — typically a self-send producer's `Bind`, or a plain local-var
rebind) and `condition_value: ValueRef` (the condition's pure final boolean),
verified in the SAME frame `body` is — `verify()`'s `UnboundVersion`/
`NonLinearVersion` checks apply to the condition's `Bind`s unchanged, no new
`VerifyError` variant. `render_conditional_loop` emits `condition`'s prelude
inside the loop's own `fun`, directly ahead of the `case`, so a self-send's
`State` advance is available to both the continuing recursive call and the
exit arm — closing the two `#[should_panic]` regressions BT-3414 pinned for
a self-send (or an inline-threaded `and:`) inside a `whileTrue:`/
`whileFalse:` condition block. The remaining opaque half, `continue_arm`
(the case-clause pattern text, e.g. `"<'true'> when 'true' -> "`), is sound
opacity in the same sense `exit_arm`'s pattern half is — see the variant's
own doc comment.

`just verify-threaded-ir` (wired into `just ci`) compiles the full
`stdlib/test/*.bt` + `stdlib/bootstrap-test/*.btscript` corpus in a debug
build so any of these panics the build instead of only degrading to a
diagnostic — the fastest way to reproduce a verifier failure locally is to
narrow that corpus down: `just test-stdlib <file>` / `just test-bunit
<file>` against the specific fixture, then `dbg!` the `ThreadedIr` fragment
at the failing construct's emission site.

**Emission-input coverage, as of ADR 0118 (BT-3424 close-out).** `ThreadedIr` started
(BT-3129-BT-3144) as a verification-only side channel: a fixture built and
checked alongside `Document` emission that happened separately, directly
from AST + generator state (ADR 0111's own Addendum, "delivered vs.
designed"). BT-3145 (`while_loops.rs`'s `generate_while_loop_direct`) was
the first real emission-input call site; BT-3146 (`conditionals.rs`),
BT-3147 (`list_ops/*.rs`/`dict_ops.rs`), BT-3148 (`gen_server/methods.rs`
Actor method bodies, class-var `Bind`s, `NlrCatch`), BT-3149
(`expressions.rs`'s `generate_block_stateful`, the Tier 2 stateful-block-body
threading for list-op/message-send block arguments), and BT-3165
(`exception_handling.rs`'s `on:do:`/`ensure:` mutation-threading generators,
ADR 0111 Addendum 5's E1-E7 per-shape table) each promoted another construct
family the same way: the `ThreadedStmt`(s) built for the construct's own
mutation sequence — real `Bind`/`Threaded`/`NlrCatch`/`Return`/
`TupleAccUnpack`/`ConditionalLoop` nodes, not a hand-fixture — ARE what
`verify()` checks and `render()` emits, byte-identical to the pre-migration
hand-rolled `Document` by construction. Verification is per-construct (one
`verify()` call per branch arm / loop / stateful-block body / exception-body
arm) except for gen_server Actor method bodies and class-method bodies,
where BT-3148's `lower_body_exprs_with_reply` and BT-3164's
`lower_class_method_body` (both + `verify_body_with_opaque_version_gaps`)
already verify the WHOLE method body in one call — the "method-level
verify()" shape ADR 0111's close-out aimed at. As of ADR 0111's own close-out
(BT-3165), a state effect *nested inside expression position* — a self-send
as a binary-op operand, a conditional leaking its `{Result, State}` tuple
into a keyword-send argument, and the other rows
`stdlib/test/actor_self_send_position_matrix_test.bt` pins — still wasn't
verified as such: generalizing the single-call-per-method shape to those
positions would have meant hoisting each nested construct's whole real
`Vec<ThreadedStmt>` fragment up to the enclosing body instead of rendering
to a `Document` at its own boundary, a rewrite estimated at the scale of
ADR 0018's rejected Alternative 3 (a full typed Core Erlang IR) and
deliberately not attempted at the time.

ADR 0118 (BT-3415-BT-3424) closed that gap without needing a rewrite at
that scale: rather than hoisting a nested construct's fragment up to the
enclosing body, every state-effecting expression form becomes a *producer*
of a small `ThreadedValue { prelude, value }` (see this file's
`StateEffectEscapesExpression` row and ADR 0118 itself for the full design),
and the existing sequencing rule splices that prelude into whichever
frame's own per-construct `verify()` call already covers it — the same
`UnboundVersion`/`NonLinearVersion` checks this table describes, now also
firing for a `Bind` nested inside expression position, not just one at
statement-top-level. Every consumer this table's construct families cover
(conditional/exception arms, stateful-block bodies, loop bodies and the
loop **condition**, inline-threaded control flow used as a value) is
migrated as of phase 4 (BT-3420); the remaining gap is the genuinely
un-migrated, self-contained-`Document` boundaries `close()`'s own row above
describes, which are a different (and much smaller) case than the
expression-position coverage this paragraph used to call out of scope.

**`exception_handling.rs`'s `on:do:`/`ensure:` (BT-3165, closing the gap
BT-3149's close-out found).** `generate_exception_body_with_threading_inner`
now builds each arm's E1-E7 shapes (field assignment and local-var
assignment reuse `conditionals.rs`'s `lower_field_assignment_bind`/
`lower_local_var_assignment_bind` directly — the same `Bind` decomposition,
same mint order) as real `ThreadedStmt`s, wraps them via `conditionals.rs`'s
`verify_and_render_branch_arm`, `verify()`s, and `render()`s — one call per
`with_branch_context` arm (the try body; `on:do:`'s handler body;
`ensure:`'s success-cleanup and error-cleanup bodies, the latter compiled
twice). The one file-specific wrinkle: this body loop's legacy separator
convention (a literal space between *source-level* statements, unlike
`conditionals.rs`'s no-separator arms) is reproduced by pushing that space
as its own `ThreadedStmt::Statement` at each source-statement boundary,
not by routing the flat per-shape sequence through
`render_loop_body_statements` (which separates every raw `ThreadedStmt`
entry — that would inject spurious spaces inside any shape spanning more
than one entry, e.g. a field assignment's Statement+Bind pair).
`control_flow/mod.rs`'s `check_branch_frame_linearity` and
`threaded_ir.rs`'s `verify_branch_frame_linearity` — the scalar-synthesis
scaffolding these two call sites were the last production users of — are
deleted; `NonLinearVersion`/`UnboundVersion` are live checks for `on:do:`/
`ensure:` arms now, same as everywhere else in this table.

**The class-method body pipeline (BT-3164, closing the gap BT-3148 left
open).** `gen_server/methods.rs`'s class-method body generator
(`generate_class_method_body`, pre-dating `BodyExprKind`/
`classify_body_expr` entirely) is now `lower_class_method_body`, returning
a real `Vec<ThreadedStmt>` instead of a hand-rolled `Document` — mirroring
BT-3148's own `is_last`-only precedent for the Actor pipeline's
`BodyExprKind::FieldAssignment`: only a class method's own direct
`self.classVar := value` in the body's *last* position is promoted to a
real `Bind` (`lower_class_method_last_class_var_bind`); every other
position, and any class-var rebind hidden inside the shared
`emit_class_var_result_unwrap` helper (the class-method analogue of the
Actor pipeline's `generate_self_dispatch_open`), stays an opaque
`Statement`. Both class-method NLR call sites
(`generate_class_method_functions`, `generate_class_method_fun_from_block`
— the latter migrated off the now-deleted `wrap_class_method_body_with_nlr_catch`
Document-wrap by this issue) mint the token before lowering and prepend a
real `NlrCatch`, then verify the whole body in one
`verify_and_render_body_stmts` call — the same "method-level `verify()`"
shape BT-3148 established for Actor bodies, now also covering class
methods. This is what first lets `ShadowWriteMissing` see a real
class-var `Bind` jointly with a real class-method `NlrCatch` (the ADR 0110
joint-visibility gap ADR 0111 Addendum 6 left open); the pre-existing
isolated, synthetic-marker check `construct_and_verify_class_var_bind`
still runs too, since it is the only one of the two that fires for a
method with no literal `^` at all (the ADR 0110 `CollectionDriver
countedRun:over:` repro shape). The other 5 `wrap_body_with_nlr_catch`-family
call sites this issue's task list named were audited: `generate_class_method_fun_from_block`
(above) shares the same pipeline and was migrated; the 3 Actor-flavored
call sites (`actor_codegen.rs`, `gen_server/dispatch.rs`,
`gen_server/extensions.rs`) don't carry the `ClassVars`
`ShadowWriteMissing` gap this issue closes and are tracked separately
(BT-3171); `gen_server/extensions.rs`'s value-type NLR site
(`wrap_value_type_body_with_nlr_catch`) is a structurally different,
already-inline mechanism, not applicable.

## Runtime/REPL Debugging

```bash
# 1. Check if modules loaded
beamtalk repl
> Beamtalk allClasses
> Beamtalk classNamed: #Counter

# 2. Enable CLI diagnostics for the REPL
RUST_LOG=beamtalk=debug beamtalk repl

# 3. Run the node in the foreground (no daemonization)
beamtalk repl --foreground

# 4. Check actor state
> c := Counter spawn
> c class
> c respondsTo: #increment

# 5. Inspect Erlang process state
# In separate terminal:
erl -name debug@127.0.0.1 -setcookie beamtalk
(debug@127.0.0.1)1> nodes().
(debug@127.0.0.1)2> observer:start().
# Find beamtalk_repl process, inspect state
```

## Performance Debugging

```bash
# 1. Profile compilation
time beamtalk build large_file.bt

# 2. Profile runtime (connect Erlang shell to running node)
# Start REPL in one terminal, then in another:
erl -remsh beamtalk@localhost -name profiler@localhost
1> timer:tc(fun() -> 'bt@counter':spawn() end).
{TimeInMicroseconds, Result}

# 3. Check memory usage (Observer in Erlang shell)
1> observer:start().
% Use the Memory tab to see allocation by process

# 4. Flame graphs (advanced)
# Enable Erlang profiling
erl -pa build
1> fprof:apply(Module, Function, Args).
2> fprof:profile().
3> fprof:analyse().
```

## Codegen Diagnostics (BT-1343)

The compiler can emit detailed diagnostics about code generation decisions.
These are off by default (too noisy for normal use) and gated behind environment variables.

### Environment Variables

| Variable | Effect |
|---|---|
| `BEAMTALK_CODEGEN_DIAGNOSTICS=1` | Enable all codegen diagnostics (info-level hints) |
| `BEAMTALK_WARN_STATEACC=1` | Promote StateAcc fallback diagnostics to warning level (requires `BEAMTALK_CODEGEN_DIAGNOSTICS=1`) |

```bash
# See all codegen decisions
BEAMTALK_CODEGEN_DIAGNOSTICS=1 beamtalk build myfile.bt

# Highlight StateAcc fallbacks as warnings
BEAMTALK_CODEGEN_DIAGNOSTICS=1 BEAMTALK_WARN_STATEACC=1 beamtalk build myfile.bt
```

### Diagnostic Categories

**1. Block calling convention chosen**

Reports which optimization mode was selected for each stateful loop:
- `direct-params` — pure locals, no field mutations (BT-1275)
- `tuple-acc` — local mutations in foldl list ops (BT-1276)
- `hybrid` — locals + field reads/mutations as direct params (BT-1326)
- `StateAcc` — fallback map-based threading

Example: `Loop at line 42: using direct-params (3 locals, 0 field mutations)`

**2. StateAcc fallback reason**

When falling back to StateAcc, includes the specific reason:
- `self-send in loop body`
- `nested list op with cross-scope mutation`
- `tier-2 value call on threaded local`
- `inline conditional writing to threaded local`
- `condition has state effects`
- `control-flow sub-expression with mutations`

Example: `Loop at line 15: StateAcc fallback — self-send in loop body`

**3. Non-local return (^) in block**

Emitted when `^` inside a block generates throw/catch, which can prevent BEAM JIT
from optimizing the enclosing function.

Example: `Non-local return at line 15: compiled via throw/catch, may inhibit JIT optimization`

**4. Synchronous self-send in loop**

Flags deadlock risk when a loop body sends a message to `self`.

Example: `Self-send 'self bar' inside loop at line 30: synchronous call to own mailbox, potential deadlock`

**5. Dynamic dispatch fallback**

When a message send can't be statically resolved and uses runtime dispatch
via `beamtalk_message_dispatch:send/3`.

Example: `Send 'foo:' at line 23: dynamic dispatch (receiver type unknown)`

**6. Large extracted arity**

Informational when a loop extracts >8 parameters as direct fun arguments.

Example: `Loop at line 10: 14 extracted params`

## When All Else Fails

1. **Simplify** — Remove code until it works, then add back
2. **Compare** — Find similar working code, diff against it
3. **Ask** — Share error + what you tried, get fresh eyes
4. **Rubber duck** — Explain the problem out loud to yourself
5. **Sleep** — Come back tomorrow with fresh perspective
