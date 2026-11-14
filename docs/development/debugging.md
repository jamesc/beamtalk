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
`stdlib/src/stack_frame.bt`) report the original `.bt` file and line number for
function heads and message-send call sites — not the compiled module's own
name or line 1 — as long as the compilation unit had a source path (any
`.bt` file built via `beamtalk build`/`beamtalk test`; REPL-evaluated code
with no backing file falls back to a bare line number, no file).

## Reading Beamtalk Stack Traces

Test failures, `Exception>>stackTrace` and JSON log stacktraces all go
through one renderer, `beamtalk_stack_frame:format_frame/1` (ADR 0139: hide
what the compiler made up, show what a person named), so the same frame
prints the same way everywhere. A bare `{Module, Function, Arity}` is
rendered by `beamtalk_stack_frame:render_mfa/1` in the same notation;
`beamtalk_stack_frame:raw_mfa/1` is the one raw `module:function/arity`
formatter, used for the machine-readable `mfa` log field and process stats.

A default test failure (3 frames, see below):

```text
FAIL testParse
  Expected 3, got 4
  at Parser>>readToken (parser.bt:42)
  at [] in Parser>>parse: (parser.bt:31)
  at ParserTest>>testParse (parser_test.bt:12)
```

### Frame format

| Frame | Rendered as |
|---|---|
| Instance method | `Counter>>increment (counter.bt:12)` |
| Class-side method | `Counter class>>new (counter.bt:3)` (no `class_` prefix) |
| Block | `[] in Counter>>increment (counter.bt:14)` |
| Erlang code (any module that is not a Beamtalk class) | `lists:foldl/3 (lists.erl:2471)` |
| A `bt@...` module the class table cannot resolve | `bt@user@Hpack>>class_encode:headers:table: (hpack.bt:40)` |

- **Order is innermost-first.** The first line is where the error was raised;
  each following line is its caller; the last is the outermost caller (for a
  test failure, the test method).
- **Class** is resolved through the runtime class table
  (`beamtalk_class_registry`); nothing is guessed from the module name. A
  `bt@...` module whose class is not loaded shows the raw module name (and
  its raw function name). A class name is package-qualified (`json@Parser`)
  only when two loaded class modules share the name. A same-named module with no registered class counts too, since the class table holds one row per name and cannot see it.
- **Erlang frames** are written `module:function/arity` as recorded, never
  as `Module>>function`. Test failure output lists only Beamtalk frames.
- **`[] in`** marks a compiler-lifted block (a closure body or loop body)
  inside the named method, so `[] in Parser>>parse:` means "inside a block
  in `parse:`", not a separate method.
- **File** comes from the frame itself, falling back to the module's
  `beamtalk_source` attribute (via `beamtalk_reflection:source_file_from_module/1`).
  Stdlib frames show the file basename only. Other absolute paths are shown
  relative to the current directory (basename if outside it).
- **Line** is the `.bt` line of the function head or message-send call site.
  Block and loop call sites carry real lines too (BT-4356).
- **File with no line**, e.g. `ParserTest>>testParse (parser_test.bt)` (the
  synthetic test-method line below is always like this): the file is
  known but the frame has no usable line (unknown or 0). The frame still
  identifies the class and selector; just don't expect a line to jump to.
  Frames with neither file nor line print as `Class>>selector`.

### Test failure traces

Test failure output filters out runtime, OTP and test-runner frames and keeps
only Beamtalk user and stdlib frames, then applies these rules:

- **Default cap of 3 frames.** If more than 3 frames remain, the output keeps
  the 2 innermost frames (the error site) plus the outermost frame (the test
  method), so the caller's file and line are never truncated away. Frames in
  between are dropped. With 3 or fewer, all are shown.
- **`beamtalk test --full-traces`** disables the cap and prints the complete
  filtered trace. Filtering of internal frames still applies.
- **Repeats are collapsed.** Adjacent identical lines are merged, with a
  trailing `[xN]` after the whole line, e.g. a recursive send whose frames
  render identically:

  ```text
    at Tree>>walk: (tree.bt:14) [x3]
    at TreeTest>>testWalk (tree_test.bt:8)
  ```

- **Synthetic test-method line.** The VM stacktrace depth is limited, so the
  test method's own frame can be missing. When it is not among the rendered
  frames, a final `at Class>>testMethod (file)` line is appended so you always
  see where the failing test lives. This line has no line number (the file
  comes from the `beamtalk_source` fallback).
- **Source snippet at error site.** The innermost frame with a readable
  source file and line is followed by a snippet: the failing line marked `>`
  plus one line of context on either side. Snippet lines are context, not
  frames — they never start with ` at `, so `frames[i]` still pairs with
  the i-th `at` line. Missing, unreadable or out-of-range sources produce
  no snippet.
- **setUp failures carry no frames**, so there is no `--full-traces` detail
  for them.

The `beamtalk test` runner also builds a structured `frames` list in each
failed `TestResult` JSON (`class`, `selector`, `block`, `file`, `line`,
`repeat`), index-aligned with the `at` lines and built from the same parts as
the text. Nothing consumes the `frames` key yet; the text output is the
supported interface.

### `Exception>>stackTrace`

```beamtalk
[1 / 0] on: Exception do: [:e | e stackTrace first printString]
```

`printString` on a `StackFrame` uses the same renderer, so it shows the same
`Class>>selector (file:line)` text as test failures. `stackTrace`
returns every frame the VM recorded; Beamtalk neither caps nor collapses it
(the VM's own stacktrace depth limit still applies). REPL-evaluated
code with no backing file shows no file in its frames.

### Reading a crash report (BT-4820)

An actor that crashes (a failed `initialize`, or an error in a `!` cast,
`handleInfo:` or a continuation; ADR 0134) is reported by OTP: a
`gen_server` terminate report, a `proc_lib` crash report, and a supervisor
`child_terminated` report. `beamtalk_crash_report_filter` (a logger primary
filter installed when the runtime starts) shows Beamtalk names in their text:
`Counter>>handleInfo:` instead of `bt@pkg@counter:'handleInfo:'/2`. It
translates stacktrace frames, `initial_call` and the supervisor offender's
`mfargs`. State, last message, mailbox and dictionary are printed as terms,
and a report it does not recognise is printed unchanged.

The report term itself is unchanged, so the JSON formatter and
`ws_log_handler` still carry the raw `module:function/arity`. To see raw names
in text, `logger:remove_primary_filter(beamtalk_crash_report_filter)`. If the
filter hits an unexpected report shape it logs one warning
(`crash_report_translation_failed`), prints that report untranslated, and
stays installed.

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

### ThreadedIr verifier (ADR 0111, BT-3129-BT-3165, BT-3164, BT-3166-BT-3170, BT-3447)

State threading — actor/instance `State`, value-type
`Self`, loop-local threading, and non-local-return (NLR) relay — used to be
coordinated only by scattered `debug_assert!`s at each emission site, each
independently re-deriving the same invariants. `crates/beamtalk-codegen/src/core_erlang/threaded_ir/`
replaces that with a small mid-level IR (`ThreadedIr`/`ThreadedStmt`) that IS
the `Document` emission for every construct family this table covers —
conditionals, exception handling's `on:do:`/`ensure:`, Actor and
class-method bodies, Tier 2 stateful-block bodies, the list-op/dict-op
tuple-accumulator unpack, ADR 0118's expression-position preludes, and (as
of ADR 0111 Addendum 15/16, epic BT-3447) `Letrec` (`whileTrue:`/
`whileFalse:`/`timesRepeat:`/`to:do:`/`to:by:do:`) loop bodies as a single
`ThreadedStmt::ConditionalLoop` node and `Foldl*` (list-op/dict-op fold)
bodies as a single merged `Threaded` node — no construct family is left on
the pre-ADR-0111 AST-directed path — and a single `verify()` pass per construct/method
that checks it
before `render()` turns it into the `Document` the caller emits. A
violation is a `threaded_ir::VerifyError`, reported through the
shared `report_threaded_ir_verify_errors` helper (`control_flow/mod.rs`,
`pub(super)`) — a `debug_assert!` in debug/CI builds (hard failure, `just
verify-threaded-ir` runs the whole `stdlib/test/*.bt` +
`stdlib/bootstrap-test/*.btscript` corpus through this path specifically to
catch it) and an `internal:` **warning** in release builds (category
`InternalVerifier`; the compile still succeeds, and it never fails a build,
even under `--warnings-as-errors`; ADR 0111 Addendum 17, BT-3724). The CLI
build path prints it with the file's other diagnostics; the compiler port
(REPL/MCP load and eval) appends it to the response `warnings`. Only these
diagnostics are forwarded out of codegen; other codegen warnings are still
dropped. To test the release path without a release build, use
`CodegenOptions::with_injected_verifier_violation()` (a hidden test hook) or
assert `verify_errors_to_diagnostic` directly. This is diagnosis
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
| `TupleAccUnpackModeMismatch` | A `ThreadedStmt::TupleAccUnpack` node (flat positional-unpack accumulator) appeared outside a `ThreadingMode::TupleAcc` body. | `list_ops/*.rs` / `dict_ops.rs`, via `control_flow::body::generate_foldl_loop_body` (ADR 0111 Addendum 15) — the tuple-shaped sibling of `ThreadingModeUnpackMismatch`. |
| `EarlyExitGateSlotMismatch` | A `TupleAccUnpack` node's own `gate_slots` disagrees with its enclosing `ThreadingMode::TupleAcc`'s `gate_slots` — the unpack would read threaded-local values from the wrong tuple positions (well-formed Core Erlang, silently *wrong values*, not a `core_lint` failure). | The list-op family's slot count in `list_ops/*.rs` (`do:`: 0; `collect:`/`select:`/boolean-predicate ops: 1; `takeWhile:`/`dropWhile:`/`detect:`-family: 2). Live since BT-3147 — `mode_gate_slots` (from `ListOpKind::gate_slots`, a canonical per-op table) and `node_gate_slots` (from each call site's own `index_offset - 1`) are genuinely independent sources now. |
| `TupleAccWithFamily` | A `TupleAcc` fold plan also threads a family (`Self`, …); the tuple accumulator carries locals only (BT-4116, replaces the retired `TupleAccInValueTypeContext`). | `control_flow/plan.rs`'s `select_tuple_acc` field-write guard. |
| `FoldAccShapeMismatch` | A `TupleAcc` fold's accumulator statement is a bare value where the mode unpacks a tuple, or the reverse (`FoldAccShape`, BT-4116). | `ThreadingPlan::fold_carry` / `acc_repack_doc`. |
| `NestedStateAccFallbackUnderDirectParams` | A nested list-op that itself needs a `StateAcc`-map fallback appeared under an enclosing `DirectParams` loop, which has no `StateAcc` map for the inner `{value, StateAcc}` result to unpack into. Regression-pinning, `#[cfg(test)]`-only (unreachable today via `select_direct_params`'s own guard; no production constructor). | `control_flow/mod.rs`'s `select_direct_params`'s `!effects.has_non_tuple_safe_list_op` guard. |
| `StateEffectEscapesExpression` | A `ThreadedValue` (ADR 0118) whose prelude carries a versioned `Bind` for `prefix` was `close()`d — rendered as nested `let`s around its value — in a context that cannot thread that prefix (`CloseContext::Opaque`), so the state effect the expression performed (a nested actor self-send's `NewState`, say) is scoped away and lost to everything after it: the "silent drop" class of bug as a verifier finding. | The consumer that called `close()` — it should *splice* the prelude into its own frame's IR instead (`stmts.extend(tv.prelude)`, as every `lower_body_exprs_with_reply` arm does since BT-3415), or, at a genuine boundary (a Tier 1 closure body, an FFI argument, a block passed to a class method, spec/doc codegen), surface a user-facing diagnostic built from this error. Constructed only by `ThreadedValue::close`; every ADR 0118 phase (1a-4, plus 5a/5b/6's `ClassVars` consolidation) has landed, so every expression-position consumer now splices its prelude — `close()` itself still has no production caller as of this writing (`expression_doc` deliberately stays a plain forwarder per ADR 0118 §Decision 5: it is reached only by genuinely un-migrated, self-contained-`Document` boundaries, not by any position this table's matrix rows cover). Since BT-3802 the same finding (via the shared `opaque_escapes` rule) has one production reporter: `generate_cascade`, the closed rendering of a non-`self` cascade, reports a versioned `Bind` in its hoisted operand prelude (the BT-3791 `State1` rebind trapped in a cascade operand) — a cascade whose operands need a prelude is a producer that `threaded_expression` splices, so reaching the closed form with one means a consumer failed to splice; `cascade_operand_state_corpus.rs` proves it over operand position x consumer shape. BT-3803 adds the second: `lower_field_write` reports a `Closure::Closed` actor/value-type field write (it mints a `State`/`Self` version `Bind` inside a self-contained document), because a parenthesized field assignment in value position (`Transcript show: (self.n := 5)`) is a producer `threaded_expression` splices via `lower_field_assignment_bind`; `field_assignment_value_position.rs` proves it over value position x enclosing shape. BT-3809 adds the third: `capture_argument_list_doc` reports a versioned `Bind` in a closed argument list's operand prelude (a self-send statement in a `do:`/`collect:`/`inject:into:` body whose operand rebinds `State`: the dispatch read the pre-operand state and a sibling operand read an unbound `StateAcc1`); a self-send statement is a producer that `sequenced_self_dispatch` sequences (operand preludes spliced, then dispatch `Statement` + `Bind`), shared by `threaded_expression` and the fold body; `loop_body_self_send_operand_corpus.rs` proves it over self-send shape x loop/fold body shape. Wiring `close()`'s `StateEffectEscapesExpression` into a user-facing diagnostic at one of those genuine boundaries (`check_no_unsafe_class_method_self_sends`) is tracked separately as a follow-up (BT-3430), not part of this migration. |
| `RacingOperands` | A list cell or map literal of the finished Core Erlang holds two or more operands that can run code (a `call`/`apply`/`primop` — not a variable, literal, `fun`, or a pure projection such as `erlang:element`/`maps:get` over variables). erlc evaluates a list cell's tail before its head and a map literal's last pair first, so those operands run out of source order (BT-4296). | The codegen site that built the operand list: route its operands through `thread_subexprs` / `thread_args` (or `thread_default_values` for a default map) so every operand but the last is bound to a temp in source order. Reported by `operand_order_check` from `generate_module_with_warnings` in debug builds only (a `debug_assert!`; release builds do not run the scan); the `snippet` field is the start of the offending construct in the module text. `tests/racing_operand_corpus.rs` proves it fires on a reintroduced in-place builder (`LocalRebindFault::OperandsInPlace`). |
| `CatchWithoutClassVarRestore` | A compiled `on:do:`'s catch boundary (`ThreadedStmt::OnDoCatch`, ADR 0130 §4) is missing one of its two halves: the node has no entry `let Snap = beamtalk_class_vars:snapshot() in` before its `try` (`NoSnapshotEntry`), the non-NLR clause does not begin with `beamtalk_class_vars:restore(Snap)` (or restores some other snapshot, or there is no non-NLR clause), or the two `$bt_nlr` pass-through clauses (the actor 4-tuple and the 3-tuple) are not both ordered before it. The carried `CatchRestoreDefect` names which. A wrong restore is wrong-value Core Erlang, not an unbound variable: an error keeps the writes made inside the protected region, or a `^` throw discards writes it must keep. **What it guards:** the node renders `let Snap = ... in try ... of ... catch ...` as one unit, and its one builder verifies it as a one-element IR and renders it at once, so the check cannot fire from today's builder. It exists for a second `OnDoCatch` constructor (or an edit to the builder) that drops a half; the `catch_boundary.rs` tests build such nodes directly. | `exception_handling.rs`'s `on_do_catch_boundary`, the single builder of every compiled `on:do:` catch boundary (class-side, instance-side, value-type, direct-called and the generic `onDo` fallback alike), called by `generate_on_do`, `generate_on_do_with_mutations` and `generate_on_do_tier1_try` with only the protected expression, the `of` variable and the handler. Fix the boundary in `on_do_catch_boundary` or the node's lowering (`render_on_do_catch`), never at a call site. `ensure:` is not a boundary and builds no such node. |
| `ActorStateInClassMethod` | A class-method scope (`ScopeKind::ClassMethod`, from `CoreErlangGenerator::in_class_method`) opens a `StateAcc` loop whose fallback reason is `NoThreadedLocals` (it carries nothing, so its seed can only be the ambient actor `State` — unbound in a class method, BT-3694), or produces a method-level `State` version or a `SelfVt` version anywhere (`ClassMethodDefect::FamilyVersion`; `State` inside a threading frame is the `StateAcc` map's version, which carries threaded locals and is legitimate). A class method threads no family (ADR 0130 §3), so a class-method `StateAcc` loop may only carry threaded locals, seeded from a fresh `maps:new()`. Per scope kind: `ClassMethod` forbids both families and the locals-less loop; `ValueType` (a value-type instance method, `CodeGenContext::ValueType`; ADR 0131 §3) forbids a method-level `State` version, since a value type has no `State` parameter either (o3/o12/s2 in value-type methods), and a locals-less `StateAcc` loop or fold that carries no `SelfVt` family (BT-3999: `[self done] whileTrue: [nil]` seeded from that unbound `State`), and allows its own `SelfVt`; `Instance` (actor/REPL) is not constrained by this check (`eligible_families` owns that, ADR 0122). BT-3725. BT-4089 adds the third defect (`ClassMethodDefect::TupleAccRepackFromAmbientState`): a tuple-acc fold's post-fold repack seeds from the ambient actor `State` in a scope whose frame has none — reported by the repack lowering itself (`ThreadingPlan::append_repack_stateacc_doc`), which is AST-directed document emission outside the IR (the `UnbracketedUserCodeSend` shape), not by the structural walk. | The mode-selection predicate that routed the construct into a `StateAcc` shape in a class method (e.g. `condition_has_state_effects` in `control_flow/plan.rs`), or the family-threading code that produced the `State`/`SelfVt` `Bind`. Fix it there, never by guarding at the loop's call site. Every production lowering site verifies through a scope-aware entry point (`CoreErlangGenerator::verify_threaded_ir`, or `verify_body_with_opaque_version_gaps` / `verify_simple_bind` given `threaded_scope()`) rather than bare `verify()`, which is `Instance`-scoped and enforces nothing here. For the repack defect, the seed choice in `append_repack_stateacc_doc` (it must read `packs_from_fresh_map`, the one predicate shared with `generate_pack_prefix` and `generate_exit_stateacc`). |
| `ThreadedLocalDropped` | ADR 0131 §4: a `ConstructTuple` whose `threads` lists `local` is not followed, in the same frame's statement list, by a `LocalRebind` for `local` before the list ends or before anything other than a family `Bind` (a `Direct` step of the `State`/`SelfVt` family: the extraction that reads the carrier first; its value is an opaque `Doc`, so which carrier it reads is not checked) or a `DiscardLocals` at a `MethodBody` frame — the construct's write to `local` is lost (o4, o7, o8, s2, s9). Also reported for a `LocalRebind` its carrier's construct does not list (or with no open construct for that carrier): the §1 transitive-closure gap, where a frame rebinds `t` while the construct carrying it out omits `t`. Checked over every statement list, so at every nesting level. A `Statement` between the tuple and its rebinds counts as a carrier read (the verifier cannot see inside it). | The producer's prelude (`build_local_threading_prelude`: tuple, family binds, one rebind per threaded local, contiguous), or the consumer that spliced something between them or dropped the rebinds. A construct in last position of a loop body or branch arm must still rebind; only a `MethodBody` frame may `DiscardLocals`. If the construct's `threads` is short, the fix is in `threaded_locals_of`'s closure, not the rebind. |
| `LocalRebindModeMismatch` | ADR 0131 §4: a `LocalRebind`'s lowered shape (`RebindLowering::shape()`: `Let`, `LoopParam`, `MapPut`) is not the cell ADR 0131 §2's table (`RebindShape::for_frame`) gives its enclosing frame's mode and the local's membership in that frame's `threads`: a plain `let` for a member of a `StateAcc` frame (the write never reaches the map the frame threads out), a `Put` for a non-member (a stray `__local__` key, persisted into an actor's `gen_server` `State`, the BT-2717 class), a `Gensym` chain entry for a non-parameter. The enclosing frame is read off the node that encloses the rebind (`RebindFrame::of` over `MethodBody`/`BranchArm`/`Threaded`/`ConditionalLoop`, each recording its own `threads`); a slice with no frame node is the implicit non-REPL method root. | The producer's `lower` callback passed to `build_local_rebind`, which must answer the shape it is asked for, or the loop/arm node whose `threads` does not list a local it threads. |
| `LocalRebindFrameMismatch` | ADR 0131 §2: a `LocalRebind`'s recorded `frame` is not the frame of the node that actually encloses it: it was lowered against one frame and spliced into another. | The consumer that spliced the prelude into a different frame than the one it passed to the producer (`RebindFrame`). |
| `HeldRebindNotIdentity` | BT-3835: a `LocalRebind` lowered to `RebindLowering::MapHeld` (a `MapPut` cell with its `Put` elided, because `maps:put(K, maps:get(K, M), M)` is `M`) whose elision is not provably the identity: its read is not a `Key` read of its own key through `state_first == map`, `map` is not a `State` version of the enclosing frame, or a `State` step of that frame earlier in the same statement list has moved the map past `map`. The elided write would then have been a real one, so the local's new value never reaches the map the frame threads out (`erlc`-clean, silently wrong). | `RebindLowering::held_in_frame_map` (`threaded_ir/ir.rs`), the one rule, and the `current` map `build_family_local_threading_prelude` tracks across a prelude's rebinds; a hand-built `MapHeld` must satisfy the same condition. |
| `LocalReadAfterSiblingRebind` | ADR 0131 §1a: a sibling of a sequenced send reads outer local `local` in place (a plain-variable child left for the parent to compile, so after every sibling's prelude) while a later sibling's prelude holds a top-level `LocalRebind` for `local` — the read sees the post-rebind value, against source order (`t + ([t := t + 1. 1] on: …)` must be `0 + 1`). Reported by `verify_sibling_reads`, called from `sequence_children`. Fires on a real compile under fault injection over the producer-position corpus (`local_rebind_verifier_corpus.rs`, `sibling_snapshot.rs`), as well as on hand-built IR (`threaded_ir/tests/local_rebind.rs`). | `sequence_children`'s §1a exemption test (`snapshot_before_sibling_rebind`): a plain variable that a later sibling threads through any construct reachable inside it (`reachable_threaded_locals`: core's `expression_threaded_writes` plus `threaded_locals_of`) must be snapshot to a `Tmp` before that sibling's prelude. If the set disagrees with the rebinds the producer emitted, fix the set there. |
| `RebindNotVisible` | BT-4263 (ADR 0131 §2): a producer handed its enclosing frame a prelude whose last top-level `LocalRebind` of `local` binds `value_var`, but the generator's variable scope at the hand-off resolves `local` to another variable (or none): the producer bound the rebind inside a scope it then popped, so every later read of the local sees the pre-construct value (`{[:y \| t := y] value: 5, t}` answered `{5, 0}`, `erlc`-clean). Reported by `verify_rebinds_visible` (via `report_rebinds_visible`) at the producer funnels: `inline_control_flow_producer_with` (conditionals, evaluated blocks, lookups, `match:`), `local_threading_producer` (loops and folds) and `exception_producer_value`. Fires on a real compile under `LocalRebindFault::EvaluatedBlockRebindsScoped` (`tests/evaluated_block_arg_local_producer.rs`) and on hand-built IR (`threaded_ir/tests/local_rebind.rs`). | The producer that pushes a scope of its own around the construct (`generate_evaluated_block_producer` binds the block's parameters in one): after popping it, bind each `LocalRebind`'s `local` to its `value_var` again in the scope the frame keeps. |
| `ConstructFamilyDropped` | BT-3821: a `ConstructTuple` declares (`families`, read off its arms' version counters, never off the extraction that follows) that its arms or body advanced the `State`/`SelfVt` family, but no `Direct` `Bind` of that family follows it in the same frame's statement list before anything else does. The family's rebind lives inside the construct's tuple and dies with the carrier: every later statement reads the pre-construct version, so the write is silently lost (`erlc`-clean). Checked over every statement list, at every nesting level, whatever the scope kind. | The producer's prelude: `vt_conditional_value_producer` / `lower_vt_conditional` (value-type conditional whose arms write a field) builds the tuple (`build_family_construct_tuple`), then `vt_conditional_family_binds` — those two must stay adjacent. Shown to fire on a real compile by `LocalRebindFault::DropFamilyBind` over the field-writing-arm positions x statement shapes (`value_type_conditional_family_verifier_corpus.rs`). |
| `StaleConstructSeed` | BT-4206: a `ConstructTuple`'s `scratch_base` (the `State` version its arms' seeded scratch map starts from, recorded because the seed lives inside the tuple's opaque `doc`) is older than a `State` version an earlier statement of the same frame's list already bound. The seed was read before a step that precedes it — a `match:` scrutinee's or a lookup receiver's/key's prelude spliced ahead of the tuple — so every arm threads from the stale `State` and the step's field writes are lost when the construct's `StateAcc` becomes the next `State` (BT-4196). Checked over every statement list, at every nesting level. | `producer_scratch_seed` (`conditionals.rs`) mints the seed and its `base`; `generate_match_producer` and `generate_lookup_tuple` call it after their operands compile, and `with_producer_head` records `base` on the tuple. Shown to fire on a real compile by `LocalRebindFault::SeedBeforeScrutinee` over scrutinee x arm x position (`match_scrutinee_state_corpus.rs`). |
| `StalePrestore` | BT-4612: a store-only `SyncedCall`'s `reads` (the `State` version its store holds, recorded because the store is a rendered name, a `Gensym`) is older than a `State` version an earlier statement of the same frame's list already bound. The `^`-closure pre-store is built and verified on its own, mid-frame, so it is verified after the version its frame has bound by then (`verify_in_scope_after`), which stands for that earlier statement. The store was read before a step that precedes it (BT-4532's first cut read the runtime dispatch's store before `sequence_call` threaded its operands), so the cell misses the step's field writes and a `^` thrown from a closure the call runs carries them out of the method lost. Checked over every statement list, at every nesting level, the same shape as `StaleConstructSeed`. It guards future builders and cannot fire on shipped code today: the builder takes the store's read and the frame's bound version from the same `frame_state_version()` read. | `nlr_closure_prestored_call` (`synced_call.rs`) reads the store where it is emitted (`nlr_prestore_read`), after the call's operands are threaded. Shown to fire on a real compile by `FAULT_PRESTORE_READS_BEFORE_OPERANDS` (the store read before a conditional's receiver is sequenced) over call x position x prefix (`nlr_closure_call_corpus.rs`): every row whose receiver steps `State` reports, no other does. |
| `ArmLocalPutNotCarried` | ADR 0133 Phase 3 (BT-3911), `ConstructFamilyDropped` extended to the `Local` family: a `BranchArm`'s body `Put`s the scratch-map slot of a local (its `__local__` key) that the arm's `threads` does not list. Every arm of every non-REPL frame is a `BranchArm` carrying the outer locals its construct threads; a local bound in the arm is a plain `let` (`local_has_scratch_slot`), because the construct's result tuple never hands it back. A write the frame does not carry is dead at best and at worst the sign of an arm lowered against a scratch map its frame does not have: a conditional nested in a `whileTrue:` condition closure wrote its block-local into an unbound `StateAcc` (BT-3891). Checked over the straight-line statements of every `BranchArm`. | `lower_local_var_assignment_bind` (`push_local_slot_put`): the `Put` after an assignment is gated by the registered frame's `threads`; the arm's frame is registered by `generate_conditional_arm` (producer arms) and `generate_plain_arm` / `generate_exception_body_with_threading` (every other arm), and a `whileTrue:` condition closure registers its own frame (`ThreadingPlan::condition_rebind_frame`). Shown to fire on a real compile by `LocalRebindFault::PutUncarriedLocal` over every arm-bearing construct x frame x method context (`arm_local_slot_verifier_corpus.rs`). |
| `BindFrameMismatch` | BT-3811: a `Bind` whose `target` was minted in a different `FrameId` than the frame node (or the caller's frame, for a spliced prelude) that encloses it; the per-frame linearity and frame-flow checks would then be answered against the wrong frame (a nested hybrid-field rebind threaded ahead with a hardcoded `FrameId::ROOT` inside a loop-body frame). Checked by `verify_bind_frames`. | The lowering that minted the `Bind` against one frame and spliced it into another; pass the enclosing frame's id. `hybrid_field_rebind_corpus.rs` proves it over position x frame. |
| `PrecompiledSubexprReadMoreThanOnce` | BT-3830: a child an enclosing sequencing pass compiled ahead (`precompiled_subexprs`) was read back more than once by its parent's compile, so its prelude ran once but its value was substituted into a second position and a state-effecting child dispatches twice. A never-read entry is the converse and is a hard `CodeGenError::Internal` in `finish_precompiled_scope`. | `sequencing.rs`: the parent that compiles a pre-sequenced operand must consume it exactly once. |
| `CatchFilterNotClosed` | BT-3718: an `OnDoCatch` opens its exception-class filter but does not close it with the handler arm, the `FilterMiss` re-raise and the exhaustive wildcard clause; `erlc` rejects an `on:do:` nested in another protected region with `ambiguous_catch_try_state`. | `exception_handling.rs`'s `on_do_catch_boundary`, the single builder of the catch boundary. |
| `FoldFamilyNotCarried` | BT-3827: a fold lambda body (`do:`/`collect:`/`inject:into:`) advances the value-type `Self` family (a top-level `Bind` chain in its own frame) but does not end in the `Return` that carries the chain's last version out through the accumulator. The lambda's `Self{N}` rebind dies with the closure, so every later statement reads the pre-fold `Self` and the write is silently lost (`erlc`-clean) — the shape the fold lowering used to reject. Checked by `verify_fold_family_carried` on every fold body, whatever the plan says. | `CoreErlangGenerator::carry_fold_families` (`control_flow/body.rs`), called by `generate_foldl_loop_body` for every `Foldl*` body: it rebases the first `Self` `Bind` onto the lambda's incoming `Self` and turns the body's tail into `Return({acc}, Self{N})`. The emitters wrap/unwrap the accumulator with `FoldFamilySlots` (`family_slots.rs`). Shown to fire on a real compile by `LocalRebindFault::DropFoldFamilySlot` over the fold positions x statement shapes (`value_type_fold_family_corpus.rs`), which also runs a generated corpus of fold bodies against an oracle. |
| `FoldLocalsNotCarried` | BT-3863 (ADR 0131 §4): a fold frame that threads outer locals does not end its lambda body in the accumulator its mode carries them in — a `ThreadedStmt::FoldAccumulator` whose `FoldCarry` is `Locals(threads)` (in order) for a tuple accumulator, or `StateMap` for a `StateAcc` one. The next iteration then reads its locals out of whatever the body ended with (BT-3839: a nested loop's raw value in place of the repacked tuple), and every write is silently lost (`erlc`-clean). Checked by `verify_fold_locals_carried` on every fold body, before `carry_fold_families` wraps the tail. It checks the accumulator's shape, not which versions it reads: only the `do:` tail can end in a raw value, and a write lost inside an opaque statement before a correctly shaped tail is the loop-body producer-agreement check's to catch (run under the same fault for the other fold kinds in `fold_locals_carried_corpus.rs`). | Every `BodyKind` epilogue in `control_flow/body.rs` builds its accumulator through `fold_accumulator`, tagging what it carries; only a fold that threads nothing may end in `FoldCarry::Value`. Shown to fire on a real compile by `LocalRebindFault::SplitFoldStatementGate` (the fold body's statement classifier misses its producers, as before BT-3839) over every `do:`-kind fold ending in a loop or fold (`fold_locals_carried_corpus.rs`). |
| `NlrThrowWithoutSlot` | ADR 0041 §State-Carrying NLR: a `ThreadedStmt::NlrThrow` whose `slot` is empty, in **every** scope. It would render `{'$bt_nlr', Tok, Val, {}}`, an empty tuple no catch unpacks. | A throw built without the state its boundary hands back (the scope's own single state). |
| `ReplReturnWithoutBoundary` | BT-4288 / BT-4312: a REPL `^` run in place (`beamtalk-core`'s `block_facts::repl_eval_returns`: at the root, in a conditional arm, an evaluated or protected block, a loop or fold body; not the input's whole last statement) lowered with no NLR boundary to throw to, so it compiles to its bare value: a loop runs on (`#(1, 2) do: [:x | ^x]` at the REPL answered the loop's value; `[n < 3] whileTrue: [^10]` never ended), and so do the input's later statements (`x := 1. ^7. x := 2` answered 2), `erlc`-clean. Invariant, in every scope: a `^` that must end its scope from inside a construct's `fun` or before later statements is a throw to the scope's boundary. Reported by the `Expression::Return` lowering. | Methods mint their token whenever a block holds a `^` (`body_has_block_nlr`); the REPL eval mints its own for exactly the `repl_eval_returns` of the input (`repl_nlr.rs`: `enter_repl_eval_boundary`, and `wrap_repl_eval_body`, which wraps the eval body in a `ThreadedStmt::NlrCatch` with `NlrBoundary::Repl`). Shown to fire on a real compile by `repl_nlr::boundary_fault::without_boundary` over the generated REPL corpus (`beamtalk-repl/tests/repl_loop_body_return.rs`). |
| `EnsureCleanupLostOnNlr` | BT-4312: an `ensure:`'s catch (`ThreadedStmt::EnsureCatch`) re-raises a `^` of its own scope after a cleanup that can write the state the throw carries, so the boundary answers the state at the `^` and drops the cleanup's writes (`EnsureRelayDefect::NoRelay`); or its relay re-throws a state other than the cleanup's result (`RethrowsStaleState`); or, where the state lives in the actor-state cell, it re-throws a state rebuilt from the `ensure:` site's bindings instead of the carried one under the cell (`RebuildsCarriedStateAtSite`: a merged Tier 2 fun's captured locals re-flushed as bound at the `ensure:`). The cleanup can write the carried state when it is inlined and advances the carried family; when it is applied as a closure (an operand that is not a block literal, `EnsureCleanup::Closure`) in an actor method, whose `^` stored `State` to the cell; and wherever the state is the cell. Repros: `w := 0. [^5] ensure: [w := 9]` at the REPL left `w` at 0; an actor method's `[^1] ensure: [self.n := 9]`, and `c := [self.n := 9]. [^1] ensure: c`, left `n` unchanged. Invariant, in every scope: an `ensure:` is not a boundary, so its cleanup's writes reach whichever boundary the in-flight NLR or exception reaches. | Every compiled `ensure:` catch is built by `ensure_nlr_relay.rs`'s `ensure_catch`: the `$bt_nlr` clause for the scope's token (`scope_nlr_token`) runs the cleanup from the carried state (`NlrSlot::carry`) and resumes the throw with its result (`StateAcc{N}`/`Self{N}`, or `refresh(Self, Carried)` where the state is the cell). Shown to fire on a real compile by `ensure_nlr_relay::fault` (`RelayFault::NoRelay`, `RelayFault::RebuildAtSite`) over the generated REPL corpus (`beamtalk-repl/tests/repl_loop_body_return.rs`) and the scope x position x shape corpus (`beamtalk-codegen`'s `tests/ensure_relay_corpus.rs`, whose scopes include the REPL). A `^` foreign to the `ensure:` (no token of its scope) owes no relay: a closure cleanup writes the cell in place, which its method commits (`ActorNlrCommitBypassesCell`). So `Block>>ensure:` reached with `perform:` is a plain catch since BT-4536; BT-4429's relay there (`NlrCarry::Foreign`, `refresh_carried/2`) is gone. |
| `EnsureForeignCleanupNotStored` | ADR 0146 R1 (BT-4534): where the actor-state cell is live (an actor instance method's threaded body, `EnsureCatch::cell_live`), an inlined `ensure:` cleanup that advances `StateAcc` has no clause for a foreign `$bt_nlr` throw (`ForeignStoreDefect::NoClause`), or that clause stores nothing to the cell (`NoStore`) or a `StateAcc` version other than the cleanup's final one (`StoresStaleVersion`), or (BT-4598) it shares the relay's cleanup with no relay of the scope's own throw that runs it from the cell (`SharedWithoutCellRelay`). A foreign `^` (one of another method of the same actor, from a block it passed in) then takes the error path, which runs the cleanup with write-through off, so the cleanup's field writes die with the re-raise. Repro: a method calls `self run: [^3]`, where `run:` does `[blk value] ensure: [self.n := 40]`; an outer cleanup in the owner read the old `n`. Invariant, for every scope: a `^` is never an error for its owner, so every unwinding exit of a scope that threads actor `State` leaves the cell holding that scope's final version. A class method running in its caller (ADR 0142), a value type and the REPL have no actor `State` to store, and in a closure the state already is the cell, so `cell_live` is false there. | `ensure_with_mutations_catch` (`control_flow/exception_handling.rs`) builds the `EnsureForeignNlr` clause when `cell_boundary` holds and the cleanup advances `StateAcc`: the cleanup compiled from the cell (`ensure_cleanup_from_cell`), its final `StateAcc` stored through `cell_put_unwound_call` (`beamtalk_actor_state:store_unwound/2`, which tells the BT-4535 test-build commit check the cell is now ahead of the throw), the term re-raised unchanged. Where the scope's own throw has a relay seeded from the cell (`cell_store`), the clause shares that relay's compiled cleanup (`ForeignCleanupBody::SharedWithRelay`, BT-4598): one `$bt_nlr` clause runs it once and branches on the token, so a nested cleanup compiles 3^depth times, not 4^depth (`ensure_relay_corpus_nested_cleanup_grows_as_three_to_the_depth`). Shown to fire on a real compile by `ensure_nlr_relay::fault` (`RelayFault::ForeignOnErrorPath`) over the "who throws" axis of `beamtalk-codegen`'s `tests/ensure_relay_corpus.rs`. |
| `ActorNlrCommitBypassesCell` | ADR 0146 (BT-4535): an actor method's NLR boundary (`ThreadedStmt::NlrCatch` with `NlrBoundary::ActorReply`) commits the state the throw carries, `{'reply', Value, State}` (`NlrArmShape::CarriedReply`), instead of the actor's cell, `{'reply', Value, beamtalk_actor_state:commit(Self, State)}` (`NlrArmShape::Reply`). The copy misses whatever was written to the cell after the `^` started (an `ensure:` cleanup of another method it unwound through, ADR 0146 R1) and, for a block the actor created and another actor ran, is the block's creation-time capture. Repro (BT-4459): `blk := [^1]. self.n := 5. peer call: blk` left `n` at 0. Invariant, in every scope kind: a `^` that reaches an actor method's boundary commits the cell, the one place the actor's fields live; `commit/2` answers the carried state only away from home (an away `super_send/5`), decided by `at_home/1`. | `nlr_arm_result` (`nlr.rs`) is the one place every boundary arm is built, from `NlrBoundary::arm_shape`, which is what the verifier checks. Shown to fire on a real compile by `nlr::fault` (`NlrFault::CommitCarried`) in `beamtalk-codegen`'s `tests/nlr.rs`. The runtime half is the test-build check in `beamtalk_actor_state:commit/2` (`BEAMTALK_CHECK_NLR_COMMIT`, exported by the `Justfile`): a `^` thrown at home must reach its boundary carrying fields equal to the cell, else `actor_nlr_stale_cell`; that is a lowering that let a `^` start without storing the method's state, to fix in the `ThreadedIr` node that owns the scope. A throw is excused when a cleanup may have put the cell ahead of it (`note_unwound/1`, from an `ensure:` foreign clause's `store_unwound/2` and the `Block>>ensure:` fallback) or when it was thrown away from home, recognised by process identity (`rethrow_from_away/1` where another process relays it back, or leaving an away `super_send/5`); the note is keyed on the carried state and spent by that throw's commit (BT-4536). The `Block>>ensure:` fallback keeps a performed cleanup's write only through this commit: `tests/ensure_relay_corpus.rs` shows `CommitCarried` fails a method whose `^` crosses one. |
| `NlrThrowWithoutCellStore` | ADR 0146 (BT-4535): a `ThreadedStmt::NlrThrow` where the actor-state cell is live (`cell_live`, an actor instance method's threaded body) throws without first storing the state it carries to the cell (`cell_store`). The method's NLR boundary commits the cell, and a cleanup the throw runs reads it, so the cell must be current whenever a `^` starts. Repro: `flag ifTrue: [^(self.count := self.count + 10)]` (BT-3495's arm lowering, also BT-3374's `^self foo`) threw the written `State` with no store, so the boundary committed the count from before the write (`NilTypeMatchSelfMutationActorTest`, found by the BUnit run of the `commit/2` test-build check). | `CoreErlangGenerator::nlr_throw_stmt` (`nlr.rs`) builds every arm `^` as an `NlrThrow` node from `nlr_throw_stores_cell`, the same decision the generic `Return` lowering's text throw uses. Shown to fire on a real compile by `nlr::fault` (`NlrFault::ThrowWithoutCellStore`) over the arm-return corpus in `beamtalk-codegen`'s `tests/nlr.rs`, which also checks every own-token throw in the corpus stores its state first. |
| `ConditionFamilyNotCarried` | BT-4002: a `ConditionalLoop`'s condition advanced a family (`condition_families`, read off the `Self` version counter around the condition's lowering) but no `Bind` in the condition slot hands that family's version back into the loop's frame from one of the loop's `produces` seeds. The condition is a closure called once per iteration, so a `Self{N}` it binds dies with it. Every iteration restarts from the pre-condition `Self`: the write is lost and the loop can run forever (`[self.n := self.n + 1. self.n < 3] whileTrue: [k := k + 1]` in a value-type or `TestCase` method), `erlc`-clean. Applies to every loop in every scope. BT-4015 extends it to a `Local` family: every outer local (in the REPL, binding) the condition writes (`loop_condition_written_locals`) rides the `StateAcc` map, so the condition slot must hold the typed `ThreadedStmt::ConditionHandBack` that rebinds `StateAcc` to the map the closure answered. Without it the write dies with the closure: the loop never ends, or a body that reads the local reads the closure's own variable (`erlc`: unbound variable). | `generate_while_loop_with_mutations` (`while_loops.rs`): the plan must thread the family (`letrec_loop_threads_value_self(condition, body)`), `CondFun` must answer `{Bool, StateAcc, SelfN}` (`generate_stateful_while_condition`'s `carry_self`), and the `element(3, CondPair)` `Bind` must sit in the node's `condition`. Shown to fire on a real compile by `LocalRebindFault::BodyOnlyLoopFamilies` over the field-writing-condition corpus (`while_condition_carrier_corpus.rs`). For a `Local` family: `while_condition_has_carried_effects` must route the condition through the carried lowering (a written local is a carried effect), so `generate_while_loop_with_mutations` emits the `ConditionHandBack`. Shown to fire on a real compile by `LocalRebindFault::ConditionWritesNotCarried` over the REPL rows (`while_condition_binding_corpus.rs`). |
| `ConditionWriteNotInMap` | BT-4584: a carried `whileTrue:`/`whileFalse:` condition hands its `StateAcc` map back to the loop (`ConditionHandBack`, so `ConditionFamilyNotCarried` is satisfied), but a statement of it that writes one of the condition's `Local` families (`name := ...` or a destructuring `#(a, b) := ...`, filtered by `loop_condition_written_locals`) has no top-level `Bind` with a `Put` of that local's scratch key inside its span. The name was bound by a plain `let` that dies with the closure, so the body, the next iteration and the code after the loop read the old value (`[x := x + 1. #(w, b) := #(w + x, 0). x < 3] whileTrue: [nil]` at the REPL left `w` at 5, not 11), `erlc`-clean. Applies to every carried condition in every scope kind. | `generate_stateful_while_condition` (`while_loops.rs`): each local-write statement must lower through the frame's `Put` (`lower_local_var_assignment_bind`, or `lower_destructure_bind` with `LocalWrite::Slot` for a destructuring). The sites come from `condition_local_write_sites`; the check is `verify_condition_writes_in_map`. Shown to fire on a real compile by `LocalRebindFault::ConditionDestructurePlain` over the REPL and compiled destructuring rows (`while_condition_binding_corpus.rs`). |
| `ConstructCarrierInPlace` | BT-3949: `ProducerSiteNotThreaded` for a producer site whose construct writes no outer local but still compiles to a `{Value, State}` carrier because it threads a family (an actor fold whose block self-sends or writes a field). No `ConstructTuple` inside the site's span means the construct was compiled in place inside its consumer's document (a map-literal value, an array element, a keyword argument), so the consumer reads the carrier tuple as the value (`Tuple` where `List` was expected) and the family's next version is dropped (`erlc`-clean). Checked by `verify_producer_sites_threaded` at every frame that registers producer sites: the method root, Tier 2 block bodies, loop and fold bodies, and (since BT-3949) every conditional, `match:` and handler arm. | The frame's producer-site registration: a producer's arm registers every arm-frame site (`generate_conditional_arm`, `with_body_producer_sites`), a plain arm registers the same arm-frame sites (`generate_plain_arm`, BT-4460), and an actor `match:` arm is threaded and lowered as a plain arm frame when `match_arm_has_producer_site` says so (`match_lowering.rs`). A value-type `match:` arm has no `Self` merge, so a field-writing fold at one of those sites is rejected as `ValueSelfFieldAssignmentInMatchArm` (`vt_match_arm_producer_field_write`). Shown to fire on a real compile by `LocalRebindFault::PlainArmOpaqueProducers` over carrier construct x operand position x plain-arm kind (`arm_operand_carrier_corpus.rs`). |
| `ConstructLocalsNotRebound` | BT-4460 / BT-4462: a construct whose `threaded_locals_of` set is non-empty was compiled in place by an arm or fold body — an opaque `element(2, ...)` extraction of its state carrier followed by a `ConstructLocalsInPlace` marker — instead of lowered as a producer (a `ConstructTuple` and one `LocalRebind` per local). Its writes ride in the carrier's scratch keys (an actor's arm-local key leaks into the actor state, and a loop in a nested `on:do:`/`ensure:` body crashed with `invalid argument`), and no frame rule rebinds the locals. Until BT-4462 the frame reread each local out of the carrier (`push_control_flow_threaded_var_rereads` / `rebind_threaded_vars_from_state`, the legacy write-backs); those are deleted and the marker renders nothing, so this finding is all that stands between such a shape and a dropped write. Reported wherever a `ConstructLocalsInPlace` carries a local; no frame legitimately emits one. | The frame's producer-site registration: since BT-4460 a plain arm registers every arm-frame site, as a producer's arm does, so a nested conditional, `ifNil:`, `and:`/`or:`, `match:` or `on:do:`/`ensure:` writing an arm-local lowers through `arm_statement_is_producer` → `threaded_expression`; core's `arm_frame_conditional_producer` agrees. Shown to fire on a real compile by `LocalRebindFault::ArmLoopOrFoldSitesOnly` (the pre-BT-4460 plain-arm rule) over the generated grid's fixtures (`plain_arm_nested_construct_corpus.rs`, `stdlib/test/bt4460plain_arm_nested_grid_test.bt`) and by `LocalRebindFault::EvaluatedBlockNotAProducer` (`evaluated_block_grid_corpus.rs`). |
| `ReplCachedBindingStale` | BT-4461: a REPL frame other than the root (a conditional arm, an `on:do:`/`ensure:` block, an evaluated block, a loop or fold body) holds the bindings it writes in Core variables (`m := 1` in an arm binds `m`), and a construct at one of its producer sites writes such a binding but its `ConstructTuple` does not thread it. The REPL producer adopted the bindings map whole (`element(2, CF)`) and no `LocalRebind` read the binding back out of it, so the frame's later reads answered the value from before the construct (`c ifTrue: [m := 1. c ifTrue: [m := 5] ifFalse: [nil]. m] ifFalse: [0]` answered 1), and a loop that was the construct read the frame's variable instead of its own `StateAcc` and never ended. Checked by `verify_repl_producer_sites_threaded` over each REPL frame's sites (`repl_frame_producer_sites`). | The REPL producers: `lower_repl_construct_tuple`, a REPL `match:` and `conditional_tuple_to_threaded_value` all end in `repl_adoption_prelude`, which threads every binding `repl_cached_bindings_written_by` names (hidden while a loop compiles, so it reads its own map) and rebinds each from the adopted map (`MapHeld`). Shown to fire on a real compile by `LocalRebindFault::ReplCachedBindingsNotRethreaded` over the generated REPL grid (`repl_cached_binding_corpus.rs`, `tests/repl-protocol/cases/bt4461_repl_arm_cached_binding_grid.btscript`). |
| `UnthreadedEvaluatedBlock` | BT-3982: an actor evaluated literal block (`[...] value`, `[:y | ...] value: x`) that threads `State` reached the legacy inline lowering (`generate_block_value_inline_with_mutations`), whose value is a raw `{Value, State}` tuple, at a site no body sequencer consumes as a Tier 2 statement (`generate_tier2_value_call_doc`). The tuple becomes the value (`{[:y | self.count := y] value: 5, self.count}` answered `{{5, Counter(count: 5)}, 0}`) and the block's `State` is dropped, `erlc`-clean. Reported where the actor producer applies (`threads_actor_evaluated_block_state`). | The producer gate (`actor_evaluated_block_producer_parts`, read by `inline_control_flow_needs_threading`, `evaluated_block_is_producer` and the statement classifiers) or a consumer that compiled the send with plain `expression_doc` instead of `threaded_expression`. Shown to fire on a real compile by `LocalRebindFault::EvaluatedBlockNotAProducer` over the repro table (`tests/evaluated_block_producer.rs`). |
| `TupleAccStateReadNotEntryState` | BT-4178: a tuple-accumulator fold body resolved the actor-state map (a field read, a `^`'s carried state, a closure capture, a nested loop's seed) to a name other than the fold's entry state, or to the lambda's own accumulator parameter. The lambda binds only a tuple of locals, so the loop context's `StateAcc` is unbound there (an `erlc` failure in `collect:`-family folds) or, if the accumulator were named `StateAcc`, the tuple itself (`badmap` at run time). | `generate_foldl_loop_body` (`control_flow/body.rs`), the one lowering of every fold body: it captures the entry state in `LoopMode::direct_params_outer_state_var` for a tuple-acc body (as a direct-params loop does, BT-3562) and checks the names `current_field_read_state_var` logged. A fold's accumulator parameter must be a fresh name (`AccSt`), never `StateAcc`. |
| `StaleReadInBracketedLoop` | BT-4180 (ADR 0137 §4b): inside a stateless loop whose checkout bracketed guarded operators (or anything lowered inside one), an actor-state read source (`site`: a field read, a bare field name, a `^`'s carried state, an actor closure's capture, a plain closure's scratch seed or exception outer state, a nested loop plan's initial state) was a threaded `State` variable or a hybrid pre-extracted parameter instead of the cell. The slow paths wrote the cell past it, so the read answers the actor as of loop entry. | A read site that resolves the state map itself instead of asking `actor_state_read_source` (`synced_call.rs`; `tests/stale_read_sites.rs` guards the accessors), or a lowering that seeds from the entry `State` inside a bracket: make the source the cell in the lowering that owns the scope (as `with_actor_closure` does for a closure capture). `FAULT_ENTRY_STATE_READS` reproduces it. |
| `NlrThrowInHybridLoop` | BT-4523 (ADR 0146 P0): a `^` was lowered in the body or condition of a hybrid-params loop (or in a loop nested in one that handed its uses outward). A hybrid loop holds each field it writes in a fun parameter (`_NField`), so the `State` in scope is the loop-entry map, and the `^` stored and carried that map: `1 to: 5 do: [:i \| self.n := i. k := i. i == 3 ifTrue: [^k]]` answered 3 and left `n` at 0, `erlc`-clean. Invariant, for every loop mode: a `^` in a loop body stores and carries a state that includes every field written so far (`StateAcc` threads the whole `State`; direct-params and tuple-accumulator bodies write no field). The `^`'s state read is recorded on the innermost loop marker (`actor_state_read_source`, `StateReadSite::NlrThrowState`); each hybrid lowering checks its marker (`checkout_hybrid_loop`) and reports with its `ConditionalLoop`'s findings. Shown to fire on a real compile by `FAULT_HYBRID_ADMITS_NLR` over the generated corpus (`hybrid_loop_nlr_corpus.rs`). | Mode selection: `ThreadingPlan::select_hybrid_params` must exclude a body or condition that holds a `^` (`BodyEffects::has_nlr`, from `beamtalk_core::semantic_analysis::facts::block_has_nlr`); such a loop threads `StateAcc` (`StateAccFallbackReason::NonLocalReturn`). |
| `UnbracketedUserCodeSend` | ADR 0137 Phase 2a (BT-4020), a call that may run user code in this process (a send to an unknown or in-process receiver, a non-inlined `value`, a send with a non-literal block argument, an FFI call, an unmarked primitive, the dispatching branch of an operator guard, a self-send that may reach `dispatch/4`'s default arm) in an actor instance method was compiled outside every `SyncedCall` bracket, so the cell the closures and aliased self-sends read holds an earlier `State` than the method's. `region` names the kind of region the lowering was in (`method-root`, `arm`, `loop`, `fold`, `tier2-body`, `other`): the `ThreadedIr` node that owns it is the one to fix. Reported by the generator at the send emitter (`CoreErlangGenerator::note_send`), not by walking the IR, because the sends in an unbracketed region are exactly the ones that are not in the IR. | A lowering that compiles its operands or statements with `expression_doc` instead of `threaded_expression`/`thread_ahead` (so no producer brackets them), or whose mode cannot thread `State` (direct-params and hybrid loops keep fields in parameters and have no whole `State` to store; the plan must select a `State`-threading mode when the body holds a bracketed call, `BodyEffects::body_has_synced_sends`). The classification is `beamtalk_core::state_threading_selectors::send_user_code` plus `synced_call.rs`'s self-send and operator rules. Pinned by `tests/synced_calls.rs`. |
| `UnbracketedNlrClosure` | BT-4524 (ADR 0146 P0): a block literal holding a `^` was compiled as an actor closure (`with_actor_closure`, at method level) for a call that stores nothing to the cell first: the send was classified `UserCodeCall::No` outside every bracket window (`note_send` records its `^`-holding block operands; a conditional's non-Boolean fallback send records its arms itself, `note_nlr_blocks`, since a mutation-path conditional never reaches `note_send`), and no covering store applies (a conditional fallback's pre-store or bracket, `on:do:`'s pre-region entry store). The closure's `^` carries the cell (`view`), older than the method's `State`, so earlier field writes are lost (`self.n := 6. [^2] ensure: [nil]` left `n` at 0; so did closure `timesRepeat:`/`to:do:`/`whileTrue:`, `[^5] value`, a non-inlined conditional, a nil test (`r := x ifNil: [^8]`) and a non-Boolean receiver's `ifTrue:`), `erlc`-clean. Invariant: a call that may run a block holding a `^` of the current scope stores the threaded state first. An operand the lowering inlines is never a finding, and neither is a block literal operand of a send classified `SendUserCode::Never` (`[^1] yourself`, `x fieldAt: #y put: [^1]`), which runs none of its operands (BT-4606), or of a send to a `no_user_code` primitive (`is_no_user_code_send`), which runs no user code whatever its arguments (BT-4633; both via `send_runs_no_operand`). A store covers only the block literals it stores for (`with_nlr_closure_cover`), never a closure of another send nested in an operand (BT-4532). BT-4532 (ADR 0146) also reports, at the send, an unbracketed construct that runs as a closure an operand that is not a block literal (a block variable or parameter, any expression: `SendUserCode::ClosureOperand`), since the value may hold a `^` of the method. Shown to fire on a real compile by `FAULT_NLR_CLOSURES_UNSTORED` over the generated corpus (`nlr_closure_call_corpus.rs`). | The call's classification or emitter: `classify_loop`/`classify_inlined` (`synced_call.rs`) bracket a closure call whose block holds a `^` (`block_returns_from_scope`), and `classify_send` brackets every `ClosureOperand` send; a call no bracket covers stores with `nlr_closure_prestore`/`nlr_closure_prestored_call`, a store-only `SyncedCall` (`boolean_fallback_send`; the runtime dispatch has none, BT-4612: outside every window it sees a block literal only as an operand of a send that runs none of its operands, a `Never` send (`p spawnWith: [^1]`) or a `no_user_code` primitive); a pure-inline conditional declines when an arm holds a bracketed call (`pure_blocks_hold_bracketed_call`). |

**Synced calls (ADR 0137 Phase 2a, BT-4020).** Every call that may run user code in an actor instance method is bracketed (`ThreadedStmt::SyncedCall`: store the threaded `State` to `'$bt_actor_state'`, make the call, reload it as the next `State` version), and the `UnbracketedUserCodeSend` check fails the compile (a debug-build panic) when one is missing. There is no switch (the Phase 2a flag was removed with Phase 2b, BT-4009). A call to a stdlib primitive whose pragma is marked `no_user_code` (`@primitive "size" no_user_code`, ADR 0137 §4) is not bracketed when the receiver's inferred class is the marking class; `build-stdlib` rejects the mark on a method with a parameter that is not a primitive value type. The invariant probe (`tests/synced_call_sites_corpus.rs`) compiles generated programs in a test-only count mode that records each gap's codegen site (`BT4020_SITES_DUMP=1` prints them).

`RoutingMismatch` (BT-3135's structural replacement for the two
`gen_server/methods.rs` routing `debug_assert!`s) was itself deleted by
BT-3148 (ADR 0111 Addendum 4): `classify_body_expr`'s upfront classification
is now the *only* computation deciding whether a construct routes through
the shared Actor `threaded_expr.rs` emitter (`emit_actor_threaded_last_stmts`/
`emit_actor_threaded_assign_rhs_stmts`, which never decline) — there is no
second, independently-computed recheck left to disagree with the first, so
the mismatch this variant caught is unrepresentable by construction.

**ADR 0122 close-out: one detector, one list, one helper (BT-3519).** ADR
0122 unified the six hand-written sites that each separately answered "does
this body mutate storage family X, and how do I carry it out" —
`while_loops.rs`/`counted_loops.rs`, `value_type_codegen.rs`'s loop and
conditional sites, `conditionals.rs`'s Actor conditional,
`exception_handling.rs`'s `on:do:`/`ensure:`, and `plan.rs`'s Foldl
accumulator — into three shared pieces, all in `control_flow/`:

- **One type, `ThreadedFamilies`** (`analysis.rs`) — an ordered
  `Vec<VersionPrefix>` in canonical slot order (`State`, then `SelfVt`;
  ADR 0122 Decision 2; the `ClassVars` family was removed by ADR 0130), constructed only via
  `ThreadedFamilies::from_matches` so no call site can hand-assemble one out
  of order.
- **One emission helper, `family_slots.rs`** — `append_family_slots`/
  `append_baseline_family_slots` close a construct's result tuple with one
  trailing slot per family; `extract_family_slots` unpacks them back out
  afterwards. Every site in the table above routes its own trailing-slot
  tuple through this pair now, instead of a hand-rolled per-site
  `Option<usize>`/bool-pair shape.
- **The capability-declaration pattern** — a site names which families it
  can carry as data, not as a hand-written `if` per family: `eligible_families`
  answers "which families exist in this generator context right now" once,
  centrally, from `CodeGenContext`/`in_class_method()` (never re-derived per
  call site — this is exactly the "ask about the ones in front of you"
  mistake ADR 0122's own "Why the gaps keep happening" section names as the
  root cause of BT-3489 and BT-3506); `match_lowering.rs`'s
  `MATCH_ARM_FAMILIES` (`[State]`, `SelfVt` excluded) is the
  clearest example of a site declaring a narrower capability than
  `eligible_families` would allow, with the exclusion checked against the
  shared detector rather than hand-matched. A family a site does not declare
  is *meant* to be rejected through the construct's own existing diagnostic
  (`FieldAssignmentInUnsupportedBlock`,
  `ValueSelfFieldAssignmentInMatchArm`) — detection and carry-capability stay
  deliberately separate questions (ADR 0122 Decision 1). That pairing is
  verified present for the loop sites; it is **not yet wired up for one
  shape found auditing this close-out**, a `SelfVt` mutation nested inside
  a value-type conditional's own `ifTrue:`/`ifFalse:` block, missed by the
  narrow detector with no rejection to catch the miss, so the mutation is
  silently dropped instead (the audit's `ClassVars` half no longer applies:
  ADR 0130 removed the family). Tracked as
  [BT-3522](https://linear.app/beamtalk/issue/BT-3522).

What ADR 0122 did **not** collapse into one recursive walk, on purpose: each
site's own "does THIS body's TOP-LEVEL statements mutate family X" predicate
(`CoreErlangGenerator::loop_body_threads_value_self`,
`ThreadingPlan::threads_value_self`, `exception_handling.rs`'s
`block_top_level_mutates_family`) stays narrower than the fully recursive
`body_threaded_families` (`analysis.rs`) the ADR's Decision 1 describes as
the eventual end state. The narrow predicates are the *actual carrying
formula* — the recursive one is a validation/differential-test tool
(`tests/control_flow/family_detector_differential.rs` runs both over the
whole corpus and asserts agreement everywhere the old formula fires),
deliberately not wired into live emission: a construct can only thread a
mutation its own tail-call/tuple shape has a slot for, and that slot only
ever comes from a BARE top-level statement (a mutation nested inside the
body's own conditional/loop keeps its own, more narrowly-scoped version
chain). Widening a live
site's own detector to the recursive walk would change which programs
compile and how — exactly the kind of diff `just core-diff` (below) exists
to catch — so it stayed out of scope; `body_threaded_families` carries
`#[allow(dead_code)]` in production builds for the same reason. This is a
documented, differential-tested design decision, not leftover duplication:
AGENTS.md's no-duplicate-implementations rule is satisfied by the
differential test enforcing the invariant, not by deleting one of the two
formulas.

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
the first real emission-input call site — since deleted (ADR 0111
Addendum 13); loops (both `Letrec` — while/counted loop bodies onto
`ConditionalLoop` — and `Foldl*` — list-op/dict-op fold bodies onto a
merged `Threaded` node — since fully migrated, Addendum 15). BT-3146 (`conditionals.rs`),
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
`render_branch_arm`, `verify()`s, and `render()`s — one call per
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

**The class-method body pipeline (BT-3164, simplified by ADR 0130).** A
class method's body is still lowered to a real `Vec<ThreadedStmt>` and
verified in one method-level `verify()` call together with its `NlrCatch`,
but class variables no longer appear in it: ADR 0130 deleted the `ClassVars`
family, the shadow-write flag on `Bind`, `ShadowWriteMissing`, the
`class_var_result` unwrap helpers and the isolated shadow-write check, so a
class-variable access is an ordinary call into `beamtalk_class_vars` (a
`put`/`get` in the class process) and needs no `Bind`. The one class-variable
node left is the catch boundary: `ThreadedStmt::OnDoCatch` (see
`CatchWithoutClassVarRestore` above).

**Corpus `.core` diff harness (BT-3509, ADR 0122 Phase 0).** The verifier
above catches a *malformed* `ThreadedIr` graph — an unbound version, a
a slot-count mismatch — via `debug_assert!` under
`just verify-threaded-ir`. It does **not** catch a *well-formed* graph that
renders to different Core Erlang than before: a changed tuple shape or slot
order still passes every `VerifyError` check while silently changing
generated code. ADR 0122's `ThreadedFamilies` unification (unifying how
`State`/`SelfVt` (and, before ADR 0130, `ClassVars`) thread through loops, conditionals,
`on:do:`/`ensure:`, and Foldl bodies) is exactly the kind of refactor where
that gap matters: every migration in that epic is meant to be
byte-identical except its own documented, reviewed exception. `just
core-diff` (see `docs/development/testing-strategy.md` § Corpus `.core`
diff harness) is the tool that proves it — it compiles the same
`stdlib/test/*.bt` + `stdlib/bootstrap-test/*.btscript` corpus this
verifier runs over, from two different checkouts, and diffs the generated
`.core` byte-for-byte. Run it before and after a `ThreadedIr` emission
change touching this section's call sites; an empty diff is the strongest
evidence a refactor changed nothing observable, and a non-empty diff should
show exactly the change you intended and nothing else.

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

### Profiling Beamtalk code with tprof/eprof (BT-4106)

`tprof` and `eprof` work in a Beamtalk node. Both profilers call
`M:module_info/1` on every loaded module during analysis, so every
Beamtalk-compiled module exports `module_info/0,1` (BT-4067) — without
them `tprof` answers `badarg` and `eprof` crashes with
`undef bt@...:module_info/1`. Pinned by
`runtime/apps/beamtalk_stdlib/test/beamtalk_otp_profiler_tests.erl`.

```erlang
%% Ad-hoc: time one Beamtalk dispatch call (MFA form profiles just it).
{Class, {call_time, Entries}} = tprof:profile(
    'bt@stdlib@integer', dispatch, ['class', [], 5],
    #{type => call_time, report => return}).

%% Whole-expression: trace everything the fun calls; use start_profiling/
%% stop_profiling/analyze to also print the per-function table.
{ok, Result} = eprof:profile(
    [self()], fun() -> 'bt@stdlib@integer':dispatch('class', [], 5) end).
```

Notes:

- Beamtalk functions show up under their compiled module names
  (`bt@package@module`), e.g. `bt@stdlib@integer:dispatch/3` — value-type
  methods additionally appear as `methodName/1` entries on the same module.
- A class method executes in its class's `gen_server` process, not the
  caller's, so `eprof:profile([self()], Fun)` attributes the call to the
  caller and the method body to the class process. To capture the method
  body, add the class process to the rootset (or use `tprof`'s MFA form on
  the method's module directly, as above).
- `observer:start()` works for live memory/process inspection from the same
  shell.

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
from optimizing the enclosing function. A method whose only block `^`s are
guard-style early returns in inlined conditionals (`x ifTrue: [^y]` at the top
level, within the limits in the language guide's § Blocks (Closures)) has none:
codegen compiles them as direct returns (BT-4415,
`core_erlang/guard_returns.rs`).

Example: `Non-local return at line 15: compiled via throw/catch, may inhibit JIT optimization`

**4. Synchronous self-send in loop**

Flags deadlock risk when a loop body sends a message to `self`.

Example: `Self-send 'self bar' inside loop at line 30: synchronous call to own mailbox, potential deadlock`

**5. Dynamic dispatch fallback**

When a message send can't be statically resolved and uses runtime dispatch
via `beamtalk_message_dispatch:send/3`. This is the dynamic-site report
(BT-4292): each hint names the receiver's static type (class, kind and
provenance, or `Dynamic`/union/negation) and the reason the send stayed
dynamic. The word after `reason:` is a stable token for counting sites
(`untyped`, `builtin`, `supervisor`, `abstract`, `root`, `actor`,
`ambiguous`, `exception`, `binary-selector`, `perform`, …). A built-in
receiver whose row of the shared routing fixture
(`runtime/apps/beamtalk_runtime/test/fixtures/tagged_map_routing_conformance.json`)
has a single route (`List`, `Dictionary`, `Integer`, …) is no longer a dynamic
site (BT-4294), and neither is a `String` or `Pid` send of a selector outside
the row's special selectors (BT-4390); for the built-ins and special selectors
that stay dynamic the rest names the row's reason. The reason is the gate of
the same decision the guarded direct call is emitted from (`typed_send_target`
in `dispatch_codegen.rs`), so the report cannot disagree with the generated
code.

Examples:

```text
Send 'foo:' at line 23: dynamic dispatch (receiver: Dynamic (unannotated parameter) [dynamic]; reason: untyped)
Send 'whenResolved:' at line 12: dynamic dispatch (receiver: Pid [Object, declared]; reason: builtin — Pid: the dynamic path answers 'whenResolved:' outside bt@stdlib@pid)
Send 'start' at line 40: dynamic dispatch (receiver: AppSupervisor [Object, declared]; reason: supervisor — AppSupervisor instances are Supervisor handles (beamtalk_supervisor tuples))
```

Count a project's dynamic sites by reason with:

```bash
BEAMTALK_CODEGEN_DIAGNOSTICS=1 beamtalk build --force 2>&1 \
  | grep -o 'reason: [a-z-]*' | sort | uniq -c
```

**6. Large extracted arity**

Informational when a loop extracts >8 parameters as direct fun arguments.

Example: `Loop at line 10: 14 extracted params`

## When All Else Fails

1. **Simplify** — Remove code until it works, then add back
2. **Compare** — Find similar working code, diff against it
3. **Ask** — Share error + what you tried, get fresh eyes
4. **Rubber duck** — Explain the problem out loud to yourself
5. **Sleep** — Come back tomorrow with fresh perspective
