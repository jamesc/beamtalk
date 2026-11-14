# ADR 0139: BEAM Abstraction Boundaries — What Leaks and What Doesn't

## Status
Accepted (2026-10-26). Amended 2026-10-26 (post-acceptance lead review): raw-mode flag renamed, frame semantics and printString rule decided, tuple scope clamped, `error_info` mechanism specified, crash-report hook and 0028 verdict scoped into Phase 1 — see "Amendment (2026-10-26)" below.

Progress (2026-11-14): none of the four phase epics has been filed. Phase 1 is partly built by adjacent work: a shared frame renderer (`beamtalk_stack_frame:format_frame/1`; BT-4353/BT-4357) serves test failures and `StackFrame>>printString`, sends are line-annotated (BT-3127, extended by BT-4356), and Erlang reasons are classified by `beamtalk_exception_handler:wrap_raw/2`. Not built: `--raw-mfa`, the crash-report logger hook, selector recovery for actor frames, rules for class-side and non-Beamtalk frames, removal of raw MFA from error messages and FFI diagnostics, the ADR 0028 verdict. The Phase 3 binaries row is settled by epic BT-4170 (done 2026-10-28; ADR 0140, which proposed flipping it, was rejected). All seven cited issues are done.

**Not ready to plan from — see "Review (2026-11-14)" below.** That review corrected stale statements in place and lists the questions that must be answered before a Phase 1 epic is filed.

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

## Review (2026-11-14): not ready to plan from

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
verdicts:

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
