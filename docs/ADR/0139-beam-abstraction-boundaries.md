# ADR 0139: BEAM Abstraction Boundaries — What Leaks and What Doesn't

## Status
Accepted (2026-10-26). Amended 2026-10-26 (post-acceptance lead review): raw-mode flag renamed, frame semantics and printString rule decided, tuple scope clamped, `error_info` mechanism specified, crash-report hook and 0028 verdict scoped into Phase 1 — see "Amendment (2026-10-26)" below.

## Amendment (2026-10-26): post-acceptance review fixes

A lead review pass after acceptance found eight gaps: an under-specified
frame shape, a raw-mode flag that collides with btscript, two unresolved ADR
tensions (0028, 0094), a missing hybrid alternative, an overbroad tuple rule,
and two Phase 1 scope holes (crash reports, the 0028 verdict). This amendment
is additive and clarifying except for the flag rename, which replaces the
original name everywhere below. No original verdict is withdrawn.

### Decision

1. **The raw-mode flag is `--raw-mfa`, not `--verbose`.** btscript already
   owns `--verbose`, so every `--verbose` in this ADR reads `--raw-mfa`:
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
  removes the btscript collision.
- The Phase 1 epic must name all of: the translator, line-annotated
  codegen, 0119-registry resolution, the crash-report logger hook, the
  0028 verdict, and its suites (per the Implementation section's test
  rule).

## Context

Beamtalk runs on BEAM and embraces it (principles: BEAM-native, hot reload is
core, actors map to natural BEAM concepts). But *which parts* of BEAM a
Beamtalk developer sees was never decided — it accreted. The result is
inconsistent leakage:

- **Names leak.** `Undefined function: bt@catalina:'class_start:'/2`
  (BT-4094), `bt@stdlib@stack_frame:module_info/1` in an eprof crash
  (BT-4106), `{module}:{function}/{arity}` in FFI type errors
  (`inference/send/ffi.rs`). There is no central MFA→display translator
  (only `beamtalk_json_formatter`'s log-only `format_mfa`).
- **Error rendering is half-built.** `#beamtalk_error{}` records +
  `format/1`, the exception handler's wrap/reraise, ADR 0015 (REPL error
  objects), ADR 0134 (crash semantics) exist — but there is no `error_info`
  integration, no line-annotated codegen, no stacktrace translation. (The
  repo's only "EEP" work is EEP-48 doc chunks — since removed by ADR 0033 —
  and EEP-59 `-doc` attributes; no EEP-based error work exists.)
- **Tuples leak.** `{#error, {#invalid_utf8, 0}}`, `{ok, nil}`, `unwrap` at
  every FFI boundary, the `stopResult`-seam pattern (ADR 0101). ADR 0076
  converts ok/error tuples at the boundary in one direction; handwritten
  `.bt` still converts by hand.
- **Bytes leak.** `byteAt:` vs `at:`, `byteSize` vs `size`,
  `asStringUnchecked`, byte offsets in errors, `fromIolist:`. The default
  path doesn't commit to graphemes.
- **The FFI membrane is raw.** `(Erlang telemetry) execute: ...` with
  arity-in-the-error, `Unknown Erlang module` warnings (BT-4022 fixed the
  known-modules half), `native/*.erl` escape hatches, second-class
  `declare native:` (BT-4144).
- **Lifecycle is OTP transliterated.** Supervision, links, restarts,
  `terminate:`, `handleInfo:`, `continueWith:` — faithful, but no on-ramp
  and inconsistent naming (`stop:` vs `stopWithin:` vs `stopWith:timeout:`).

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
| Display names | **HIDE** | Every user-facing surface (errors, stacktraces, `describe`, FFI diagnostics, test failures) renders `Class>>selector`, never `bt@package:'class_sel'/2`. Raw MFA available via `--verbose`/tooling flag only. One shared translator, no ad-hoc prefix stripping. |
| Errors + stacktraces | **HIDE mechanics** | Frames render as `Class>>selector (file.bt:line)`; Erlang reasons translate to `#beamtalk_error{}` with Smalltalk-readable messages. Line-annotated codegen + `error_info`-style rendering. *(Amended: callee frames only, reconstructed Elixir-style — no synthetic send frames; names resolve via codegen maps + the ADR 0119 registry. `printString` renders display names. See Amendment.)* |
| Tuples / Result | **CONVERT at boundary** | Codegen owns the conversion both directions; handwritten `.bt` never sees a 2-tuple. Extends ADR 0076's direction to a rule. *(Amended: public API surfaces only; codegen-internal traffic unaffected. See Amendment.)* |
| Binaries | **BYTES for Binary, GRAPHEMES for String — decided in BT-4170** | Epic BT-4170 (Remove Binary UTF-8 sniffing, in progress) owns this row: class = construction type, no sniffed dispatch — `Binary>>size` bytes, `String>>size` graphemes, `Binary>>at:` 1-based bytes, `Binary>>do:` yields Integers, `byteSize` deleted, native text boundaries construct String, `asString` returns a real Result. This ADR adds no new verdict here; it cites the split as the policy's worked example (representation is essence, like lifecycle — decided explicitly, not leaked accidentally). |
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
//    (raw MFA under --verbose)
```

REPL:

```beamtalk
> (Erlang gen_tcp) connect: "x" with: 1
=> Error: gen_tcp connect: expected (host, port), got 2 args
   "Beamtalk-shaped; no m:f/a"

> (Erlang mt19937) seed: 42   "unknown module, typo'd"
=> Unknown FFI module 'mt19937' — did you mean…?  "spans + suggestions, not atoms"
```

## Prior Art

| Language | Posture | What we take |
|----------|---------|--------------|
| **Elixir** | Hides `Elixir.` prefix everywhere; `** (UndefinedFunctionError)` with `Foo.bar/1`; `error_info` pretty-printing | Closest reference: names hidden, VM reachable, errors structured. We adopt the shape. |
| **Gleam** | Hides almost everything (`gleam/otp` wraps supervision/actors in supervised, typed APIs; raw `gleam/erlang/process` remains for the few) | The purity precedent — and the caution: Gleam chose it deliberately for its audience. Rejected here: it would hide the lifecycle this ADR keeps visible, and fight the BEAM-native principle. |
| **Erlang** | Fully raw (`undef`, `badarg`, `{M,F,A}`) | The floor. Rejected as user-facing posture; kept as the `--verbose` substrate. |
| **Pharo** | No VM concept at all (image model) | Proof that hiding works when the VM is truly an implementation detail — inapplicable while we embrace OTP interop. |
| **Python** | C extensions must translate failures to Python exceptions (return NULL + set); a raw segfault is the equivalent leak | The membrane precedent outside BEAM: translation at the boundary is the norm, raw passthrough is a bug. Supports the FFI-membrane and tuple-conversion verdicts. |

## User Impact

- **Newcomer**: errors speak their language from day one; lifecycle defaults mean the first actor program needs no OTP. Discovery: `--verbose` teaches the mapping on demand. Cost: two output modes to learn ("which error is real?") — mitigated by making display output the default everywhere and documenting `--verbose` once.
- **Smalltalk developer**: message-passing stays pure at every surface they touch; supervision is the one new concept, and it arrives with a template, not a manual. Cost: supervision/links stay unconcealed by design — the one pill to swallow.
- **Erlang/BEAM developer**: nothing is taken away — raw MFA, observer, recon all remain one flag away; typed FFI makes Beamtalk→Erlang calls safer than raw `apply`. Cost: display names hide the info they want most — mitigated by `--verbose` plus machine MFA in the JSON formatter.
- **Operator**: sensible OOB defaults (restart intensity, shutdowns) with full observability underneath; log output keeps machine-parseable MFA via the JSON formatter. Cost: log scrapers migrate per phase (see Migration Path).
- **Tooling developer**: one translator + one error taxonomy to consume instead of N ad-hoc formats; spans on every diagnostic. Cost: someone must build and own the translator — Phase 1 funds it.

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
  (`--verbose`, observer, recon intact).
- FFI becomes approachable: typed specs + named errors instead of arities
  and atoms.

### Negative

- Display changes break snapshot tests, log scrapers, and any docs showing
  raw output — a migration sweep with each phase.
- Line-annotated codegen + translator + typed FFI is real multi-epic work;
  this ADR commits to it without funding it.
- Two output modes (`--verbose`) to maintain forever.

### Neutral

- OTP lifecycle surface unchanged in shape, only in defaults and naming
  consistency.
- No runtime cost: translation happens at diagnostic-render time.

## Implementation

Phased, each phase its own epic (no code in this ADR):

1. **Names + error rendering** (highest value): central MFA→display
   translator; `Class>>selector (file.bt:line)` frames via line-annotated
   codegen; Erlang-reason → `#beamtalk_error{}` translation;
   `--verbose` raw mode. Consumes BT-4094/4106-class issues.
   *(Amended: also owns the crash-report logger hook, 0119-registry
   resolution, and the ADR 0028 verdict; `--verbose` reads `--raw-mfa`.
   See Amendment.)*
2. **FFI membrane**: typed FFI specs with named errors; `native/` as
   first-class package member; `declare native:` parity (BT-4144).
3. **Tuples + binaries**: codegen-owned boundary conversion; the
   Binary/String audit itself lives in Epic BT-4170 (BT-4168/69/71/72/73) —
   this phase tracks its completion for the `bin at: 1` drift resolution
   and conversion-door docs rather than re-doing it.
4. **Lifecycle defaults**: template supervised app; audited OOB defaults;
   vocabulary pass (`stop:` family naming).

Effort: L–XL total, sliced per phase. Each phase lands green independently,
and each phase epic must name its suites: unit tests for the translator/rules,
corpus/golden updates for changed output, and an E2E btscript where the phase
touches the REPL or CLI surface (Phase 1 and 2 do).

## Migration Path

- Snapshot/golden tests asserting raw `bt@…` output get re-blessed to
  display names per phase.
- Log scrapers move to the JSON formatter (keeps machine MFA) or
  `--verbose`.
- Downstream packages: display-only changes; no API breaks intended. Any
  phase that breaks an API states it in its own epic.

## References

- Related ADRs: ADR 0006 (dispatch), ADR 0015 (REPL errors), ADR 0016
  (stdlib naming), ADR 0028 (interop strategy), ADR 0060 (Result hybrid),
  ADR 0076 (tuple→Result), ADR 0094 (printString=Debug), ADR 0101 (native
  objects), ADR 0119 (module registry), ADR 0121 (ok/error specs), ADR 0125
  (releases), ADR 0126 (distribution), ADR 0134 (crash semantics)
- Related issues: BT-4094 (naming), BT-4106 (stackframe), BT-4022 (FFI),
  BT-4144 (native), BT-4052 (linked exits), BT-4046/47 (supervision),
  BT-4170 (epic: Binary/String split — owns the binaries row)
- Documentation: `docs/beamtalk-principles.md` (BEAM-native,
  interactive-first)
