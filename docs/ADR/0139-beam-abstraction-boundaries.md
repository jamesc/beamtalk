# ADR 0139: BEAM Abstraction Boundaries — What Leaks and What Doesn't

## Status
Accepted (2026-10-26). Amended 2026-10-26 (post-acceptance lead review): raw-mode flag renamed, frame semantics and printString rule decided, tuple scope clamped, `error_info` mechanism specified, crash-report hook and 0028 verdict scoped into Phase 1 — see "Amendment (2026-10-26)" below.

Progress (2026-11-14): Phase 1 is partly built by adjacent work: a shared frame renderer (`beamtalk_stack_frame:format_frame/1`; BT-4353/BT-4357) serves test failures and `StackFrame>>printString`, sends are line-annotated (BT-3127, extended by BT-4356), and Erlang reasons are classified by `beamtalk_exception_handler:wrap_raw/2`. Not built: `--raw-mfa`, the crash-report logger hook, selector recovery for actor frames, rules for class-side and non-Beamtalk frames, removal of raw MFA from error messages and FFI diagnostics, the ADR 0028 verdict. The Phase 3 binaries row is settled by epic BT-4170 (done 2026-10-28; ADR 0140, which proposed flipping it, was rejected). All seven cited issues are done. Implementation is planned as epic BT-4814 (see Implementation Tracking).

Amended 2026-11-14 (after review): the rule is restated as "hide what the compiler made up; show what a person named", the five blocking questions from the review are decided, and the plan is cut to one phase with two spikes — see "Amendment (2026-11-14)" below. Where the amendment and older text disagree, the amendment wins.

## Amendment (2026-10-26): post-acceptance review fixes

A lead review pass after acceptance found eight gaps: an under-specified
frame shape, a raw-mode flag that collides with btscript, two unresolved ADR
tensions (0028, 0094), a missing hybrid alternative, an overbroad tuple rule,
and two Phase 1 scope holes (crash reports, the 0028 verdict). This amendment
is additive and clarifying except for the flag rename, which replaces the
original name everywhere below. No original verdict is withdrawn.

### Decision

1. **The raw-mode flag is `--raw-mfa`, not `--verbose`.** `-v/--verbose`
   is already the global logging-verbosity flag on every `beamtalk`
   subcommand, so the raw-mode flag is named `--raw-mfa` throughout this ADR:
   the one flag that renders raw MFA on any user-facing surface, alongside
   machine MFA in the JSON formatter and explicit tooling APIs.
2. **Frames reconstruct, Elixir-style.** Inlined `dispatch/4` erases the
   send frame; the translator does not preserve synthetic send frames.
   Traces show callee frames only, with each MFA translated to
   `Class>>selector (file.bt:line)` — the Elixir shape this ADR adopts as
   its closest reference. No per-call-site codegen cost; inlining stays
   free to optimize.
3. **The `error_info` mechanism is specified.** EEP-54 carries no
   module→source map, so the translator resolves display names from
   line-annotated codegen maps plus the ADR 0119 module registry. ADR 0119
   is a named dependency of Phase 1, not background reading.
4. **`printString` renders display names.** The ADR 0094
   (printString=Debug) tension resolves against an exemption: `printString`
   is the largest user-facing surface (inspectors, logs, test failures),
   and raw MFA there would leak the whole policy through one hole. Raw MFA
   stays one flag (`--raw-mfa`) away.
5. **The tuple rule covers public surfaces.** "Handwritten `.bt` never sees
   a 2-tuple" means public API surfaces: stdlib signatures, FFI returns,
   user-visible values. Codegen-internal ok/error traffic is unaffected.
6. **Rejected alternative: lightly-translated MFA in default output.**
   Elixir shows `Foo.bar/1` — an MFA that reads cleanly — so a hybrid
   could show our MFAs with only cosmetic translation. Rejected: our
   compiled names (`bt@package:'class_sel'/2`) are unreadable
   un-translated, so the hybrid keeps BEAM names in the default surface
   while still paying the two-mode maintenance cost. Full translation plus
   `--raw-mfa` dominates it.
7. **The crash-report logger hook is Phase 1 scope.** SASL-style crash
   reports render through the translator (display names by default, raw
   under `--raw-mfa`); Phase 1 owns the logger hook that does it.
8. **The ADR 0028 verdict is Phase 1 acceptance.** Whether the interop
   strategy needs amending is decided by the Phase 1 epic, which records
   the verdict back here; this ADR does not pre-decide it.

### Consequences

- The two-mode maintenance cost is unchanged in kind; the rename only
  removes the collision with the global `--verbose` flag.
- The Phase 1 epic must name all of: the translator, line-annotated
  codegen, 0119-registry resolution, the crash-report logger hook, the
  0028 verdict, and its suites (per the Implementation section's test
  rule).

## Review (2026-11-14): findings

*Items 1–8 and 10 are decided by the "Amendment (2026-11-14)" that follows;
item 9 becomes its spike 2. Item 11's evidence and prior-art notes stand as
recorded.*

A three-pass review (facts against the code at 2026-11-14, reasoning,
adversarial) found the one-line policy sound and the baseline stale. Stale
or wrong statements of fact were corrected in place and are marked
*(Corrected 2026-11-14)*. No verdict was changed. The items below are
design gaps; they need the owner's decision, recorded as an amendment,
before a Phase 1 epic is filed.

### Must be decided before planning

1. **Actor frames have no selector.** Actor methods compile to clauses
   inside `dispatch/4`, so an MFA cannot yield `Class>>selector`; frames
   read `Counter>>dispatch`. Amendment 2 ("each MFA translated to
   `Class>>selector`") is not implementable as written. Choose: a
   function per actor method, an emitted line→selector table, or accept
   `Class>>dispatch (file:line)`.
2. **Amendment 3 names the wrong registry.** ADR 0119's registry is a
   Rust compile-time class→module structure; the runtime cannot consult
   it. Runtime frames resolve through the class table
   (`beamtalk_class_registry:class_name_for_module/1`) with a lossy
   heuristic fallback. Decide capture-time vs render-time resolution, and
   the rendering for renamed, unloaded and remote-node modules and for
   same-named classes in different packages (ADR 0070).
3. **Frame taxonomy is undecided.** The shipped renderer already answers
   some cases against the rule: class-side methods print the `class_`
   prefix and, on a registry miss, the raw module
   (`bt@user@Hpack>>class_encode:headers:table:`, pinned by a test);
   Erlang frames are dressed as selectors (`lists>>foldl_1`). Decide the
   exact string for: class-side method, block, generated callback
   (`init/1`, `handle_call/3`), runtime `beamtalk_*` module, OTP or
   dependency module, user `native/*.erl`.
4. **`--raw-mfa` is not a mechanism yet.** Where it is passed (CLI flag,
   REPL command, session setting) is unstated. The workspace node is
   shared by the REPL, LSP, MCP and the LiveView IDE, so a node-wide
   switch changes output for every client; a release has no command line.
   Messages that embed MFA are composed at raise time and structured test
   frames already drop it, so a render-time flag cannot recover it.
   Amendment 4 would make `printString` depend on a flag. The likely
   shape is: always carry both forms in structured data and let each
   client choose. Its relation to `test --full-traces` is undefined.
5. **The tuple verdict contradicts implemented ADRs.** ADR 0076 owns
   conversion in the runtime proxy, return values only, and explicitly
   excludes outbound arguments and messages. ADR 0136 added tuple
   literals for handwritten FFI code. `handleInfo:` receives raw tuples.
   ADR 0028 §2 says values are not converted at boundaries. Scope the
   rule to ok/error-tagged returns, say what "both directions" means,
   and state here which of 0028, 0076 and 0136 are amended — not in an
   epic (amendment 8), and not in Phase 1, since the conflict is with
   Phases 2–3.

### Should be fixed

6. **The examples are not acceptance examples.** BT-4094 was a codegen
   bug diagnosed from the raw module name; no rendering policy turns its
   `undef` into "connection refused". `gen_tcp:connect/2` exists, so
   "expected (host, port), got 2 args" is self-contradictory; today's
   output is already selector-shaped. Replace with real before/after
   output from a live site.
7. **The FFI row misapplies the rule.** `gen_tcp:connect/3` is the
   foreign library's own name and its arity identifies the function; it
   is not an accident of Beamtalk compilation. The rule that decides the
   hard cases is narrower: hide identifiers the Beamtalk compiler
   synthesised (`bt@…` atoms, mangled function names, arities inflated by
   self and state); show what the user or a foreign library named
   (Erlang modules, registered names, ETS tables, node names).
8. **Phases 2–4 have little defined work left.** Typed FFI is ADR 0075
   (implemented); `native/` is ADR 0072; binaries are BT-4170; the app
   template (`beamtalk new --app`) and supervisor defaults exist. Reduce
   each to a concrete gap list or to "recorded, owned by X". The `stop`
   vocabulary pass is an API break, against "no API breaks intended".
9. **The crash-report hook needs its own spike.** Crash reports are
   mostly terms, not frames; shapes vary across OTP versions; a logger
   filter that crashes is silently removed.
10. **Missing alternatives and consequences.** Not considered: show both
    forms always (`Class>>sel (called m:f/a)`, which the actor messages
    already do), translate at the viewer, a reversible name mangling.
    "Thin membrane" is rejected by slogan yet is what Phase 1 ships.
    Unlisted costs: no reverse lookup from display name to MFA for
    `dbg`/`recon`; hidden names can hide compiler bugs; nothing enforces
    the rule in CI.
11. **Evidence and prior art.** Of the seven cited issues only BT-4022 is
    a diagnostics-policy case; five come from one epic (the Catalina
    port). Gleam does not hide supervision or translate stack traces, so
    it is a weak precedent for "purity". The Implementation section names
    no affected components, per-phase estimates, test suites or
    validation step.

## Amendment (2026-11-14): the rule that decides, and one phase

The review above found the policy sound but too broad to decide hard cases,
and five gaps blocking a plan. This amendment restates the rule, decides
the gaps, and cuts the work to what follows from the rule. It replaces
amendment items 2, 3 and 4 of 2026-10-26, narrows the Tuples and FFI
verdicts, and replaces the four-phase plan.

### The rule

**Hide what the compiler made up; show what a person named.**

- *Compiler-made, hidden:* `bt@package@module` atoms, mangled function
  names (the `class_` prefix, `dispatch/4`, `safe_dispatch`, `-fun-0-`),
  arities inflated by self and state.
- *Person-named, shown:* classes and selectors, Erlang modules and
  functions written by someone (including a package's own `native/*.erl`),
  registered names, ETS tables, node names, tuples and atoms in user data,
  and every lifecycle concept.

"Names hidden, lifecycle visible" remains the summary; this is the test to
apply when a case is unclear.

### Decisions

1. **Actor frames get their selector from a line→selector table.** Actor
   methods are clauses of `dispatch/4`, so the MFA alone cannot name the
   method. The compiler emits, per actor-style module, a sorted table of
   `{StartLine, Selector}` as a generated function returning a constant
   (not a module attribute, which is copied on every read). The renderer
   finds the frame's line in it. Codegen and state threading are
   unchanged; value classes already have one function per method and need
   no table. A frame with no line falls back to `Class>>dispatch`.
   Measured cost (BT-4817, spike 1): nothing on the dispatch path (the
   table is read only when a frame is rendered); one binary search per
   rendered actor frame, about 0.85 µs per lookup on a 24-entry table
   (including the benchmark loop); the stdlib `.beam` total moves from
   3,320,508 to 3,320,868 bytes. Only the 8 of 125 stdlib modules that
   are actors carry the table, +1,608 bytes across them; the largest,
   `bt@stdlib@actor`, grows from 118,984 to 119,492 bytes. Not
   measured: the fraction of actor frames in the existing test traces
   that have no line and so fall back to `Class>>dispatch`, and the
   actor-dispatch benchmarks (no code on that path changed).
2. **Names resolve through the runtime class table**, not the ADR 0119
   registry (which is compile-time only and stays the resolver for
   compiler diagnostics). When a module cannot be resolved the frame
   shows the raw module; the renderer never guesses a class name. A class
   name is package-qualified only when two loaded classes share it.
   Frames are resolved on the node that raised them.
3. **Frame rendering.**

   | Frame | Rendered as |
   |---|---|
   | Instance method | `Counter>>increment (counter.bt:12)` |
   | Class-side method | `Counter class>>new (counter.bt:3)` |
   | Block | `[] in Counter>>increment (counter.bt:14)` |
   | Erlang code in a Beamtalk package (`native/*.erl`, the project's or a dependency's) | `beamtalk_http_client:request/4 (beamtalk_http_client.erl:88)` — shown; a person named it |
   | The frame that raised, when it is other Erlang code (OTP, a hex dependency) | `gen_tcp:connect/3` — shown, so the trace says where it failed |
   | Other OTP and hex-dependency frames (`gen_server`, `proc_lib`, `lists` internals) | hidden by default |
   | Generated callbacks and runtime internals (`init/1`, `handle_call/3`, `safe_dispatch`, `beamtalk_*`) | hidden by default |

   Erlang frames are written `module:function/arity`: that is the name
   their author gave them, and the arity identifies the function. They
   are never dressed as `lists>>foldl_1`. Hidden frames are collapsed,
   not deleted: the full list is in the structured data (decision 4).
   How a module is known to belong to a Beamtalk package comes from the
   package's module list (ADR 0072); spike 1 confirms the lookup.
4. **Raw output is chosen per client, not by a node-wide switch.** Every
   structured frame and error carries both forms: display name and raw
   MFA, plus the complete unfiltered frame list. Text renderers print the
   display form. Raw is selected where the output is consumed:
   `--raw-mfa` on `beamtalk run` and `beamtalk test` (where it also
   un-hides frames, subsuming `--full-traces`), a per-session REPL
   toggle, and fields in JSON logs, LSP and MCP payloads. `printString`
   never depends on a flag; `StackFrame` answers its raw MFA through an
   accessor. One exception to hiding: an undefined-function error on a
   `bt@…` module always carries the raw MFA in its details, because that
   usually means a compiler bug.
5. **Tuples are data, not a leak.** "Handwritten `.bt` never sees a
   2-tuple" and "both directions" are withdrawn. The verdict is what ADR
   0076 and ADR 0121 already do: `ok`/`error`-tagged returns from FFI
   calls become `Result`. Outbound arguments, messages to `handleInfo:`
   and tuple literals (ADR 0136) are untouched. No ADR is amended, so
   the "ADR 0028 verdict" item of 2026-10-26 is closed: none is needed.
6. **FFI errors keep the foreign name.** A failed `(Erlang gen_tcp)
   connect: …` names `gen_tcp:connect/3`. The work is only to remove
   `bt@…` and mangled names from the messages that embed them
   (`beamtalk_exception_handler`, `beamtalk_actor`,
   `beamtalk_erlang_proxy`, the Rust FFI diagnostics). Messages may show
   both forms — `Bad argument in Ticker>>bump: (called lists:reverse/1)`
   — which the actor messages already do.
7. **Rows with no work here.** Binaries: decided by BT-4170; the
   `byteAt:`/`at:` indexing question is a separate issue. Typed FFI: ADR
   0075. `native/` membership: ADR 0072. Lifecycle, distribution,
   releases: ADRs 0059, 0125, 0126, 0135 — visible by policy, nothing
   planned. The `stop` family naming is an API break and gets its own
   issue, not this ADR.
8. **The rule is enforced.** A CI check fails when a golden file or a
   user-facing message template contains `bt@` or a mangled function
   name, outside raw-mode fixtures.

### Examples (real output, replacing the Decision section's)

```text
// Undefined function, class-side method in a dependency
Before: Undefined function: 'bt@catalina':'class_start:'/2
After:  Undefined function: Catalina class>>start:
        (details: bt@catalina:'class_start:'/2)

// Class-side block frame, registry miss
Before: at bt@user@Hpack>>class_encode:headers:table: [block] (hpack.bt:40)
After:  at [] in Hpack class>>encode:headers:table: (hpack.bt:40)

// Actor method frame
Before: at Ticker>>dispatch (src/ticker.bt)
After:  at Ticker>>tick: (src/ticker.bt:14)

// Erlang frames
Before: at lists>>foldl_1 (lists.erl:2471)
        at gen_server>>try_handle_call (gen_server.erl:2470)
After:  at lists:foldl/3 (lists.erl:2471)     // only if it raised
        (gen_server frame hidden)
```

### Plan (replaces Phases 1–4)

One epic, "Names and error rendering", preceded by two spikes:

- **Spike 1 — actor frame selectors.** Emit the table for one actor
  class; measure the stdlib size change, lookup time, and how often a
  frame has no line. Confirm the package-module lookup for decision 3.
- **Spike 2 — crash reports.** Crash reports are mostly terms, their
  shape varies across supported OTP versions, and a logger filter that
  crashes is silently removed. Decide what is translated and the
  failure behaviour before committing to the hook.

Then, in order:

1. One renderer: fold `beamtalk_json_formatter:format_stack_frame/1` and
   the three `format_mfa/1` copies into `beamtalk_stack_frame`; drop the
   heuristic class-name fallback; apply the frame table above.
   Components: `beamtalk_runtime`, `beamtalk_workspace`,
   `beamtalk_stdlib` (test output).
2. Line→selector table in codegen (`beamtalk-codegen`, actor/gen_server
   generator) and its use in the renderer.
3. Messages: remove compiler-made names from the runtime and compiler
   message sites listed in decision 6 (`beamtalk_runtime`;
   `beamtalk-core` type checker and validators).
4. Both forms in structured data; `--raw-mfa` on `run`/`test`; REPL
   toggle; JSON/LSP/MCP fields (`beamtalk-cli`, `beamtalk_workspace`,
   `beamtalk-lsp`, `beamtalk-mcp`).
5. Crash-report hook, as scoped by spike 2.
6. The CI check of decision 8.

Suites: EUnit for the renderer and frame table; codegen snapshots for
the table; re-blessed goldens for every changed message; a btscript E2E
for an actor error and for the REPL raw toggle. Effort: M–L, plus the
two spikes (S each).

### Consequences

- A trace mixes two notations (`Class>>selector` and `module:function/arity`).
  Accepted: the second names things that exist and can be looked up.
- Default traces are shorter; a failure deep inside OTP shows only the
  raising frame until raw mode is on.
- The earlier "never `m:f/a` on a user-facing surface" claim is replaced
  by "never a compiler-made name".
- Carrying both forms makes structured frames larger; nothing is
  recomputed and no flag changes what a value prints.

## Context

Beamtalk runs on BEAM and embraces it (principles: BEAM-native, hot reload is
core, actors map to natural BEAM concepts). But *which parts* of BEAM a
Beamtalk developer sees was never decided — it accreted. The result is
inconsistent leakage:

- **Names leak.** `Undefined function: bt@catalina:'class_start:'/2`
  (BT-4094), `bt@stdlib@stack_frame:module_info/1` in an eprof crash
  (BT-4106), `{module}:{function}/{arity}` in FFI type errors
  (`crates/beamtalk-core/src/semantic_analysis/type_checker/inference/send/ffi.rs`).
  A shared frame renderer exists (`beamtalk_stack_frame:format_frame/1`,
  used by test failures and `StackFrame>>printString`), but it is not the
  only one: the JSON formatter has its own `format_stack_frame/1`, three
  `format_mfa/1` copies exist (`beamtalk_json_formatter`,
  `beamtalk_ws_log_handler`, `beamtalk_repl_ops_watch`), and error
  *messages* still embed raw MFA (`beamtalk_exception_handler`,
  `beamtalk_actor`, `beamtalk_erlang_proxy`, the Rust FFI diagnostics).
  *(Corrected 2026-11-14: the original said no central translator existed.)*
- **Error rendering is half-built.** `#beamtalk_error{}` records +
  `format/1`, the exception handler's wrap/reraise (which already classifies
  `undef`, `badarg`, `badarith`, `function_clause`, `noproc`), ADR 0015
  (REPL error objects), ADR 0134 (crash semantics) exist. Sends carry line
  annotations (BT-3127) and frames render through `beamtalk_stack_frame`.
  EEP-54 `error_info` is consumed only for binary construction (ADR 0143).
  Missing: selector names for actor frames (actor methods are clauses of
  `dispatch/4`, so frames read `Counter>>dispatch`), rules for class-side
  and non-Beamtalk frames, crash-report rendering, and a raw mode.
  *(Corrected 2026-11-14: the original said there was no line-annotated
  codegen, no stacktrace translation and no `error_info` work.)*
- **Tuples leak.** `{#error, {#invalid_utf8, 0}}` (since fixed: `Binary>>asString`
  answers a Result), `{ok, nil}`, `unwrap` at
  every FFI boundary, the `stopResult`-seam pattern (ADR 0101). ADR 0076
  converts ok/error tuples at the boundary in one direction; handwritten
  `.bt` still converts by hand.
- **Bytes leaked** (`byteSize` vs `size`, sniffed dispatch) until epic
  BT-4170 (done 2026-10-28). Residue: 0-based `byteAt:` next to 1-based
  `at:`, `asStringUnchecked`, `fromIolist:`.
- **The FFI membrane is raw.** `(Erlang telemetry) execute: ...` with
  arity-in-the-error, `Unknown Erlang module` warnings (BT-4022 fixed the
  known-modules half), `native/*.erl` escape hatches, second-class
  `declare native:` (BT-4144).
- **Lifecycle is OTP transliterated.** Supervision, links, restarts,
  `terminate:`, `handleInfo:`, `continueWith:` — faithful, but no on-ramp
  and inconsistent naming (`stop` / `stopWith:` / `stopWith:timeout:` on Actor
  vs `stop` / `stopWithin:` on supervisors).

### Constraints

- BEAM-native is a core principle: the VM must stay reachable, not hidden
  behind an opaque wall (operability, observer/recon/dbg, hot reload).
- Lifecycle leakage is accepted policy: "you know you're on the BEAM" — but
  with sensible out-of-box defaults so casual users don't pay the OTP
  learning curve up front.
- Interactive-first: whatever the policy, the REPL must demonstrate it.

## Decision

**Names hidden, lifecycle visible.** One line draws the boundary: *names are
accidents of compilation; lifecycle is essence of the model.* Per-area
verdicts *(the Display names, Errors, Tuples and FFI rows are narrowed by
the Amendment of 2026-11-14, whose examples replace the ones below)*:

| Area | Verdict | Rule |
|------|---------|------|
| Display names | **HIDE** | Every user-facing surface (errors, stacktraces, `describe`, FFI diagnostics, test failures) renders `Class>>selector`, never `bt@package:'class_sel'/2`. Raw MFA available via `--raw-mfa`/tooling flag only. One shared translator, no ad-hoc prefix stripping. |
| Errors + stacktraces | **HIDE mechanics** | Frames render as `Class>>selector (file.bt:line)`; Erlang reasons translate to `#beamtalk_error{}` with Smalltalk-readable messages. Line-annotated codegen + `error_info`-style rendering. *(Amended: callee frames only, reconstructed Elixir-style — no synthetic send frames; names resolve via codegen maps + the ADR 0119 registry. `printString` renders display names. See Amendment.)* |
| Tuples / Result | **CONVERT at boundary** | Codegen owns the conversion both directions; handwritten `.bt` never sees a 2-tuple. Extends ADR 0076's direction to a rule. *(Amended: public API surfaces only; codegen-internal traffic unaffected. See Amendment.)* |
| Binaries | **BYTES for Binary, GRAPHEMES for String — decided in BT-4170** | Epic BT-4170 (Remove Binary UTF-8 sniffing, done 2026-10-28; ADR 0140, which proposed flipping it, rejected) owns this row: class = construction type, no sniffed dispatch — `Binary>>size` bytes, `String>>size` graphemes, `Binary>>at:` 1-based bytes, `Binary>>do:` yields Integers, `byteSize` deleted, native text boundaries construct String, `asString` returns a real Result. Open: `Binary>>byteAt:` (0-based) vs `at:` (1-based). This ADR adds no new verdict here; it cites the split as the policy's worked example (representation is essence, like lifecycle — decided explicitly, not leaked accidentally). |
| FFI membrane | **TYPED + Beamtalk-shaped** | FFI calls get typed specs; failures name the selector and expectation (`telemetry execute: expected 3 args`), never arities and module atoms. `native/` is a first-class package member, not a trapdoor. |
| Process lifecycle | **KEEP, with OOB defaults** | Supervision, links, restarts, `terminate:` stay visible with Beamtalk spelling — plus one template supervised app and defaults (restart intensity, shutdown timeouts) so the common case needs no OTP knowledge. |
| Distribution / releases / hot reload | **KEEP (OTP-native)** | Per ADR 0125/0126 direction; Beamtalk spelling where it exists, OTP concepts undisguised. |

```beamtalk
// Before:
Catalina start: #{}                // when unreachable
// => error: bt@catalina:'class_start:'/2 -> undef

// After:
Catalina start: #{}
// => Error: Catalina>>start: failed: connection refused
//    (raw MFA under --raw-mfa)
```

REPL:

```beamtalk
> (Erlang gen_tcp) connect: "x" with: 1
=> Error: gen_tcp connect: expected (host, port), got 2 args
   // Beamtalk-shaped; no m:f/a

> (Erlang mt19937) seed: 42   // unknown module, typo'd
=> Unknown FFI module 'mt19937' — did you mean…?  // spans + suggestions, not atoms
```

## Prior Art

| Language | Posture | What we take |
|----------|---------|--------------|
| **Elixir** | Hides `Elixir.` prefix everywhere; `** (UndefinedFunctionError)` with `Foo.bar/1`; `error_info` pretty-printing | Closest reference: names hidden, VM reachable, errors structured. We adopt the shape. |
| **Gleam** | Hides almost everything (`gleam/otp` wraps supervision/actors in supervised, typed APIs; raw `gleam/erlang/process` remains for the few) | The purity precedent — and the caution: Gleam chose it deliberately for its audience. Rejected here: it would hide the lifecycle this ADR keeps visible, and fight the BEAM-native principle. |
| **Erlang** | Fully raw (`undef`, `badarg`, `{M,F,A}`) | The floor. Rejected as user-facing posture; kept as the `--raw-mfa` substrate. |
| **Pharo** | No VM concept at all (image model) | Proof that hiding works when the VM is truly an implementation detail — inapplicable while we embrace OTP interop. |
| **Python** | C extensions must translate failures to Python exceptions (return NULL + set); a raw segfault is the equivalent leak | The membrane precedent outside BEAM: translation at the boundary is the norm, raw passthrough is a bug. Supports the FFI-membrane and tuple-conversion verdicts. |

## User Impact

- **Newcomer**: errors speak their language from day one; lifecycle defaults mean the first actor program needs no OTP. Discovery: `--raw-mfa` teaches the mapping on demand. Cost: two output modes to learn ("which error is real?") — mitigated by making display output the default everywhere and documenting `--raw-mfa` once.
- **Smalltalk developer**: message-passing stays pure at every surface they touch; supervision is the one new concept, and it arrives with a template, not a manual. Cost: supervision/links stay unconcealed by design — the one pill to swallow.
- **Erlang/BEAM developer**: nothing is taken away — raw MFA, observer, recon all remain one flag away; typed FFI makes Beamtalk→Erlang calls safer than raw `apply`. Cost: display names hide the info they want most — mitigated by `--raw-mfa` plus machine MFA in the JSON formatter.
- **Operator**: sensible OOB defaults (restart intensity, shutdowns) with full observability underneath; log output keeps machine-parseable MFA via the JSON formatter. Cost: log scrapers migrate per phase (see Migration Path).
- **Tooling developer**: one translator + one error taxonomy to consume instead of N ad-hoc formats; spans on every diagnostic. Cost: someone must own the translator; unfunded until the Phase 1 epic is filed.

## Steelman Analysis

### Rejected: thin membrane (errors only)

- 🧑‍💻 **Newcomer**: "Fix my confusion with the smallest possible change."
- 🎩 **Smalltalk purist**: "Minimal departure risk — don't touch what works."
- ⚙️ **BEAM veteran**: "Errors are the only real pain; leave FFI and binaries alone."
- 🏭 **Operator**: "Least migration risk of any option."
- 🎨 **Language designer**: "Do one thing well; revisit the rest with data."

Genuine — and it fails on coherence: fixing errors while leaving
`(Erlang m) f:` raw and tuples manual preserves exactly the inconsistency
this ADR exists to end. Half a membrane is no membrane.

### Rejected: full Smalltalk purity (hide everything)

- 🧑‍💻 **Newcomer**: "I never learn OTP at all."
- 🎩 **Smalltalk purist**: "True Smalltalk — the VM disappears entirely."
- ⚙️ **BEAM veteran**: *(cannot genuinely advocate hiding the substrate they
  debug production with — telling in itself)*
- 🏭 **Operator**: "Fewer concepts to learn to operate the system."
- 🎨 **Language designer**: "Most elegant, most coherent — one model, no seams."

Genuine for everyone except the cohort that operates the system — which is
why it loses. It also contradicts the principles doc (actors map to natural
BEAM concepts) and both distribution/releases ADRs.

### Tension points

- Newcomers split hide-more/hide-less; Smalltalkers prefer purity but accept
  visible lifecycle with defaults.
- BEAM veterans and operators firmly want lifecycle visible — debuggability
  beats elegance in production.
- The names/lifecycle line resolves the tension: accidents hidden, essence
  visible.

## Alternatives Considered

### Thin membrane (errors only)
MFA translation + stacktrace rendering, nothing else. Rejected: leaves the
inconsistency in place; FFI/tuples/binaries stay raw.

### Full Smalltalk purity
Auto-supervision, no visible processes/monitors/links. Rejected: fights the
BEAM-native principle, both OTP ADRs, and operability.

### Do nothing
Keep accreting per-issue fixes (BT-4094, BT-4106, BT-4022 each patched one
leak). Rejected: seven issues of evidence (BT-4094, 4106, 4022, 4144, 4052,
4046, 4047) show the whack-a-mole cost; without a policy every fix
re-litigates hide-vs-keep.

## Consequences

### Positive

- A durable rule for every future leak report: names hidden, lifecycle
  visible — no re-litigation.
- Day-one experience transforms (errors) while power users lose nothing
  (`--raw-mfa`, observer, recon intact).
- FFI becomes approachable: typed specs + named errors instead of arities
  and atoms.

### Negative

- Display changes break snapshot tests, log scrapers, and any docs showing
  raw output — a migration sweep with each phase.
- Line-annotated codegen + translator + typed FFI is real multi-epic work;
  this ADR commits to it without funding it.
- Two output modes (`--raw-mfa`) to maintain forever.

### Neutral

- OTP lifecycle surface unchanged in shape, only in defaults and naming
  consistency.
- No cost on the success path beyond the line annotations already
  emitted; translation runs at render time. (`class_name_for_module/1` is
  a full ETS scan per call, which matters for the crash-report hook.)

## Implementation

*(Superseded by the plan in the Amendment of 2026-11-14: one epic and two
spikes. The original four phases are kept below as the record.)*

Phased, each phase its own epic (no code in this ADR):

1. **Names + error rendering** (highest value): central MFA→display
   translator; `Class>>selector (file.bt:line)` frames via line-annotated
   codegen; Erlang-reason → `#beamtalk_error{}` translation;
   `--raw-mfa` raw mode. Consumes BT-4094/4106-class issues.
   *(Amended: also owns the crash-report logger hook, 0119-registry
   resolution, and the ADR 0028 verdict. See Amendment.)*
2. **FFI membrane**: typed FFI specs with named errors; `native/` as
   first-class package member; `declare native:` parity (BT-4144).
3. **Tuples + binaries**: codegen-owned boundary conversion; the
   Binary/String audit was done in Epic BT-4170 (2026-10-28), leaving only
   the `byteAt:` / `at:` indexing question and conversion-door docs.
4. **Lifecycle defaults**: template supervised app; audited OOB defaults;
   vocabulary pass (`stop` family naming).

Effort: L–XL total, sliced per phase. Each phase lands green independently,
and each phase epic must name its suites: unit tests for the translator/rules,
corpus/golden updates for changed output, and an E2E btscript where the phase
touches the REPL or CLI surface (Phase 1 and 2 do).

## Migration Path

- Snapshot/golden tests asserting raw `bt@…` output get re-blessed to
  display names per phase.
- Log scrapers move to the JSON formatter (keeps machine MFA) or
  `--raw-mfa`.
- Downstream packages: display-only changes; no API breaks intended. Any
  phase that breaks an API states it in its own epic.

## Implementation Tracking

**Epic:** [BT-4814](https://linear.app/beamtalk/issue/BT-4814) — Names and error rendering: hide compiler-made names
**Status:** Planned (2026-11-14)

| Phase | Issue | Title | Size | Blocked by |
|-------|-------|-------|------|------------|
| 1 Foundation | BT-4815 | One frame renderer with the frame table (class-side, block and Erlang frames) | M | — |
| 2 Core | BT-4817 | Actor frames name their selector: line→selector table (carries spike 1 measurements) | M | BT-4815 |
| 2 Core | BT-4818 | Frame visibility: package native Erlang and the raising frame shown, OTP and generated frames hidden | M | BT-4815 |
| 2 Core | BT-4816 | Remove compiler-made names from error messages | S | — |
| 2 Core | BT-4819 | Structured frames carry both forms and the full frame list | M | BT-4815, BT-4818 |
| 3 Integration | BT-4821 | Per-client raw output: `--raw-mfa` on run/test, REPL session toggle | M | BT-4819 |
| 3 Integration | BT-4820 | Crash reports through the frame renderer (carries spike 2 scope decision) | M | BT-4815 |
| 4 Validation | BT-4822 | CI check, end-to-end btscript, docs, ADR to Implemented | M | BT-4816, BT-4817, BT-4818, BT-4820, BT-4821 |

The two spikes named in the amendment's plan are folded into BT-4817 and
BT-4820 as acceptance criteria: their measurements and scope decision are
recorded back in this ADR.

## References

- Related ADRs: ADR 0006 (dispatch), ADR 0015 (REPL errors), ADR 0016
  (stdlib naming), ADR 0028 (interop strategy), ADR 0060 (Result hybrid),
  ADR 0076 (tuple→Result), ADR 0094 (printString=Debug), ADR 0101 (native
  objects), ADR 0119 (module registry), ADR 0121 (ok/error specs), ADR 0125
  (releases), ADR 0126 (distribution), ADR 0134 (crash semantics); also
  bearing on the verdicts: ADR 0072 (user Erlang sources), ADR 0075 (FFI
  type definitions), ADR 0136 (tuple literals), ADR 0140 (rejected),
  ADR 0143 (binary construction, `error_info`)
- Related issues: BT-4094 (naming), BT-4106 (stackframe), BT-4022 (FFI),
  BT-4144 (native), BT-4052 (linked exits), BT-4046/47 (supervision),
  BT-4170 (epic: Binary/String split — owns the binaries row)
- Documentation: `docs/beamtalk-principles.md` (§9 Seamless BEAM
  Ecosystem Integration, §10 Fault Tolerance via OTP, interactive-first);
  `docs/development/debugging.md` (the shipped frame renderer)
- Related work: BT-4353 / BT-4357 (shared frame renderer, test stacktraces)
