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
# For codegen: add dbg!(&expr) in crates/beamtalk-core/src/codegen/core_erlang/
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
# Edit crates/beamtalk-core/src/codegen/core_erlang/expressions.rs
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

### ThreadedIr verifier (ADR 0111, BT-3129-BT-3136)

State threading — actor/instance `State`, class-var `ClassVars`, value-type
`Self`, loop-local threading, and non-local-return (NLR) relay — used to be
coordinated only by scattered `debug_assert!`s at each emission site, each
independently re-deriving the same invariants. `crates/beamtalk-core/src/codegen/core_erlang/threaded_ir.rs`
replaces that with a small mid-level IR (`ThreadedIr`/`ThreadedStmt`) built
alongside the real `Document` emission at every state-threading call site,
and a single `verify()` pass that checks it. A violation is a
`threaded_ir::VerifyError`, reported through the shared
`report_threaded_ir_verify_errors` helper (`control_flow/mod.rs`,
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
| `UnboundVersion` | A versioned var (e.g. `State2`) was referenced with no producing `Bind` in its frame or an ancestor frame on the frame stack. | Whatever emission path built the `ThreadedIr` fragment around the failing construct — it referenced a version it never bound. |
| `NonLinearVersion` | Within one `FrameId`, a version was produced by more than one `Bind`, or consumed as the source of more than one successor — frame-scoped SSA-like linearity broken. | The generator for that frame; likely a duplicate `Bind` or a version reused across two branch arms that should have gotten distinct `FrameId`s. |
| `ThreadingModeUnpackMismatch` | An optimized `ThreadingMode` (a mode chosen specifically because it needs no `StateAcc` unpack) contains an unpack `Bind` anyway. | `while_loops.rs` / `counted_loops.rs`'s mode-selection logic (`control_flow/mod.rs`'s `verify_loop_unpack_invariant`) — this is the structural replacement for the four loop "unpack should emit no code" `debug_assert!`s BT-3132 deleted. |
| `ShadowWriteMissing` | A class-var `Bind` at frame depth 0 (method top frame) inside a method whose body can relay a foreign NLR (an `NlrCatch` with `boundary: ClassMethod { has_class_vars: true }`) lacks `shadow_write: true` — the ADR 0110 contract. | `expressions.rs`'s class-var assignment emission path — a future change dropped the shadow write ADR 0110's fix depends on, or added a new class-var mutation site without it. |
| `TupleAccUnpackModeMismatch` | A `ThreadedStmt::TupleAccUnpack` node (flat positional-unpack accumulator) appeared outside a `ThreadingMode::TupleAcc` body. | `list_ops/*.rs` / `dict_ops.rs`'s `ThreadingPlan::generate_tuple_unpack_docs` — the tuple-shaped sibling of `ThreadingModeUnpackMismatch`. |
| `EarlyExitGateSlotMismatch` | A `TupleAccUnpack` node's own `gate_slots` disagrees with its enclosing `ThreadingMode::TupleAcc`'s `gate_slots` — the unpack would read threaded-local values from the wrong tuple positions (well-formed Core Erlang, silently *wrong values*, not a `core_lint` failure). | The list-op family's slot count in `list_ops/*.rs` (`do:`: 0; `collect:`/`select:`/boolean-predicate ops: 1; `takeWhile:`/`dropWhile:`/`detect:`-family: 2). |
| `TupleAccInValueTypeContext` | `TupleAcc` mode was selected in a `ValueType` context, which has no actor `State` to reference — regression-pinning (unreachable today via `select_tuple_acc`'s own early-return). | `control_flow/mod.rs`'s `select_tuple_acc` guard ordering. |
| `NestedStateAccFallbackUnderDirectParams` | A nested list-op that itself needs a `StateAcc`-map fallback appeared under an enclosing `DirectParams` loop, which has no `StateAcc` map for the inner `{value, StateAcc}` result to unpack into. | `control_flow/mod.rs`'s `select_direct_params`'s `!effects.has_non_tuple_safe_list_op` guard. |
| `RoutingMismatch` | `gen_server/methods.rs`'s upfront `classify_body_expr` classification (`BodyExprKind::LocalAssignControlFlow` / `ControlFlowWithMutations`) committed a construct to the shared Actor `threaded_expr.rs` emitter, but that emitter's own downstream recheck declined and fell through to the generic path instead — the structural replacement for the two `gen_server/methods.rs` routing `debug_assert!`s BT-3135 deleted. | `gen_server/methods.rs`'s `classify_body_expr` vs. `threaded_expr.rs`'s `control_flow_has_mutations`. |

`just verify-threaded-ir` (wired into `just ci`) compiles the full
`stdlib/test/*.bt` + `stdlib/bootstrap-test/*.btscript` corpus in a debug
build so any of these panics the build instead of only degrading to a
diagnostic — the fastest way to reproduce a verifier failure locally is to
narrow that corpus down: `just test-stdlib <file>` / `just test-bunit
<file>` against the specific fixture, then `dbg!` the `ThreadedIr` fragment
at the failing construct's emission site.

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
