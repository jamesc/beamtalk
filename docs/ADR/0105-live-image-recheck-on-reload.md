# ADR 0105: Live Image Re-Checking on Hot Reload

## Status
Proposed (2026-07-05)

## Implementation Tracking

**Epic:** [BT-2775](https://linear.app/beamtalk/issue/BT-2775)
**Issues:**

| Phase | Issue | Title | Size | Blocked by |
|---|---|---|---|---|
| 0 | [BT-2776](https://linear.app/beamtalk/issue/BT-2776) | Walking skeleton — one method → one caller → one LSP diagnostic (proves the 3 assumptions) | M | – |
| 1 | [BT-2777](https://linear.app/beamtalk/issue/BT-2777) | Per-selector signature-generation store + capture plumbing | M | BT-2776 |
| 1 | [BT-2778](https://linear.app/beamtalk/issue/BT-2778) | Re-check orchestration (xref lookup + receiver filter + batched port re-check + findings) | M | BT-2777 |
| 1 | [BT-2779](https://linear.app/beamtalk/issue/BT-2779) | Publish on all surfaces + clearing-by-replacement semantics | M | BT-2778 |
| 2 | [BT-2780](https://linear.app/beamtalk/issue/BT-2780) | Shape-change re-check (`state:`/`field:`), integrated with ADR 0104 `spawnWith:`/accessor checking | M | BT-2778, BT-2779 |
| 2 | [BT-2781](https://linear.app/beamtalk/issue/BT-2781) | Fan-out benchmarks + xref receiver-type-key decision | S | BT-2778 |
| 3 | [BT-2782](https://linear.app/beamtalk/issue/BT-2782) | Pre-save advisory + `:recheck image` command | S | BT-2778 |
| 4 | [BT-2783](https://linear.app/beamtalk/issue/BT-2783) | E2E killer-demo test + docs + status flip | S | BT-2779, BT-2780, BT-2782 |

Deferred / recorded follow-ups: xref receiver-type-key extension (BT-2781 decides), transitive re-check refinement, proxy meta-dependent tracking, single-method check API. Enabling ADRs (all landed): 0087 (xref), 0082 (edit/save), 0100 (severity), 0022 (compiler port), 0104 (actor/`spawnWith:` checking).

**Status:** Planned

## Context

### Problem statement

Beamtalk's premise is code that changes while it runs — method-level edit &
save (ADR 0082), live patching as a message send (principle 11), hot reload as
core (principle 2). But the type checker only sees code at compile time, one
compilation unit at a time. When a method's signature changes *live*, every
existing caller in the image is now potentially stale — and today nothing
tells anyone until the stale call misbehaves at runtime.

This is also the ecosystem's open problem, and Beamtalk's clearest
opportunity (`docs/internal/positioning.md`, Seed 3): Gleam's FAQ concedes
upgrades cannot be type-checked ("the usual Erlang amount of safety");
Elixir's inference has no live image to re-check; Dialyzer is post-hoc by
construction. **Nobody can demo: change a running method's signature and watch
the stale callers light up.** The infrastructure to do it is unusually cheap
for Beamtalk, because the hard part already shipped:

- **ADR 0087 (Implemented):** `beamtalk_xref` maintains a **selector-keyed**
  call-sites index in the runtime (`{Selector, Site}` bag; sites carry owner,
  method, line, `recv_kind` — *not* a static receiver class), kept correct
  across hot reload and ClassBuilder lifecycle hooks. That is *most of* the
  dependency graph incremental re-checking needs — see Mechanism step 2 for
  what it is not.
- **Principle 12:** the compiler is the language service — the same
  `beamtalk-core` that compiled the edit can re-check its dependents (via the
  compiler port, ADR 0022 — a separate OS process, so round-trips must batch).
- **ADR 0100:** the severity ladder for "how certain is the checker" already
  exists; reload-induced findings slot into it (see Severity — including one
  place where this ADR must *not* silently escalate).

One honest caveat up front: **hot-patching currently *discards* type
metadata.** `put_method/4` deliberately clears the patched selector's
signature and return type (`beamtalk_object_class.erl` — "return types are
cleared for hot-patched methods; the compiler treats them as dynamic",
confirmed by ADR 0050), and `__beamtalk_meta/0` is stale after live patches by
design. So the interface delta this ADR needs is **new plumbing captured at
patch time**, not a read of existing state — see Mechanism step 1.

### What "stale" means

A live redefinition can invalidate callers three ways:

1. **Signature change** — return type or parameter types changed:
   `getValue -> Integer` becomes `getValue -> String`; caller does
   `self getValue + 1`.
2. **Removal** — the selector no longer exists on the class; callers are now
   DNU-at-runtime.
3. **Shape change** — `state:`/`field:` slots added, removed, or retyped;
   `spawnWith:` sites and field accessors are affected (with ADR 0104's
   checking).

### Constraints

- **Advisory, never blocking** (ADR 0100). A reload is an *action that already
  happened* — diagnostics inform, they cannot veto. (Whether a future strict
  mode may pre-veto a reload is explicitly out of scope.)
- **Bounded latency.** Re-checking runs at workspace-interaction speed;
  transitive re-checking must be cut off, not exhaustive.
- **Open world.** Reflective sends (`perform:`) and DNU handlers are invisible
  to xref; findings are graded accordingly.

## Decision

On every live definition change (method save, class rebuild, module reload),
run an **incremental re-check of known dependents** and publish the findings
as live diagnostics on all surfaces (LSP, workspace UI, REPL notification).

### Mechanism

1. **Signature diff — captured at patch time, not read back.** The compiler
   already computes the new method's signature when it compiles the edit; the
   *compile response* must carry it, and the workspace must record it per
   selector **before** `put_method/4` installs the patch (which clears type
   metadata — see Context caveat). The previous generation's signature is
   whatever was recorded at the *last* patch (or the original
   `__beamtalk_meta` for never-patched methods). Capturing at every patch is
   what keeps diffs working across repeated edits to the same method —
   read-back from class state cannot, because the pre-patch type is wiped on
   install. This is **new plumbing** through the compile-request/response
   protocol and a small new signature-generation store in the workspace.
2. **Dependent lookup — selector query, then receiver filter.**
   `beamtalk_xref` is keyed by **selector only** (ADR 0087's schema has no
   receiver-class component; `recv_kind` is self/super/ffi/other). So the
   lookup is: query sites for the changed selector, then **filter candidates
   by inferred receiver type** during re-check — a site calling `size` on a
   `List` is not a dependent of `Counter>>size`. For common selectors this
   filter is what keeps fan-out bounded; a **numeric cap on re-checked callers
   per reload** (with a "N more not checked" note) is part of the design, and
   extending the xref schema with a receiver-type key is the recorded
   follow-up if fan-out proves hot (see Alternatives).
3. **Re-check at file granularity, batched.** The checker's entry points are
   whole-`Module` (`check_module` / `infer_types`); there is no
   single-method check today. The unit of re-check is therefore the caller's
   **enclosing compilation unit**, batched into **one compiler-port request**
   per reload (the port is a separate OS process, ADR 0022 — ~2ms overhead
   plus compile time per request; per-caller round-trips would not meet the
   latency constraint). Sources come from the **live image**, never disk:
   ADR 0082 makes in-memory `method_source` authoritative until
   `Workspace flush`, and a flagged caller may itself carry unflushed edits.
   A single-method check API is a possible later optimisation, not a
   prerequisite. One level of fan-out only — with an honest caveat: the
   cutoff is exact for callers with *declared* signatures, but a caller whose
   *inferred* return type silently shifts (it delegates to the changed
   method, no annotation) can propagate breakage to *its* callers unchecked.
   That gap is **accepted** in this ADR; comparing re-inferred signatures at
   the cutoff is the recorded refinement.
4. **Publish.** Findings appear as diagnostics attributed to the caller's
   source location, tagged as reload-induced, on every surface
   (surface-parity applies). **Clearing is replacement, not just agreement**:
   every re-check of a caller **replaces all of that caller's reload-induced
   findings with the new result — clean or different** — so findings can never
   refer to a stale interface generation. This one rule covers the two
   otherwise-missed paths: (a) *reload-fixes-reload* — reload B restores or
   adapts the callee, the dependent re-check runs again (Mechanism steps 1–3
   fire on every reload), and the caller's finding clears without anyone
   editing the caller; (b) *supersession* — back-to-back reloads of the same
   method re-check the caller against generation B and replace any
   generation-A finding rather than accumulating contradictory diagnostics.
   Findings also clear on `Workspace changes revert:` (ADR 0082) and on
   workspace restart (session state, never persisted). The Phase 0 napkin
   must validate the supersession case explicitly. Site-level `@expect`
   markers keep their ADR 0100 Rule 3 precedence — an expected DNU stays
   silent even when reload-induced.

### The demo (REPL/workspace session)

```
bt> :load counter.bt
bt> counter := Counter spawn
bt> counter getCount + 1          // => 1

// ... in the editor: change `getCount -> Integer` to `getCount -> String`,
//     save (method-level hot reload, ADR 0082) ...

⚠ reload check: Counter>>getCount signature changed (Integer → String);
   2 callers re-checked, 1 stale
   Dashboard>>refresh (dashboard.bt:14): `self counter getCount + 1`
     — `+ 1` expects a number, `getCount` now returns String
```

Only genuinely-affected callers surface. `StatsView>>render` was the second
re-checked caller — it passes `getCount` to `Transcript show:`, which expects
a `String`, so it re-checks *clean* against the new signature and is silently
suppressed (the header's "2 re-checked, 1 stale" is the only trace of it).

### Error example (removal)

```
ℹ reload check: Counter>>reset was removed; 1 caller remains
   AdminPanel>>onClick (admin.bt:9): `counter reset` will raise
   does_not_understand at runtime
   (Hint severity per ADR 0100 Rule 1 — single closed receiver)
```

### Severity

ADR 0100 governs — and it must not be silently escalated. Its Rule 1 caps an
unresolved selector on a single closed-complete receiver at **`Hint`** (its
steelman explicitly rejects `Warning` there to avoid converting quiet hints
into warnings overnight). Accordingly:

- **Removed-selector findings** on a single statically-known receiver are
  `Hint` — same as any other unresolved selector under Rule 1. Reload adds
  attribution ("removed by the reload of X"), not severity.
- **Signature-mismatch findings** (`+ 1` on a now-`String` return) are type
  mismatches, not unresolved selectors — they take whatever severity the
  ordinary type checker gives that mismatch. Reload changes *when* they're
  discovered, not *how severe* they are.
- Anything reached through `Dynamic`, `perform:`, or DNU-overriding receivers
  is silent, as always. Site-level `@expect` (Rule 3) wins over reload
  attribution.
- If experience shows reload-induced findings deserve more prominence than
  their static equivalents, that is an explicit ADR 0100 Rule 3 carve-out to
  propose *there* (with the argument that post-reload certainty differs from
  static certainty) — not a default this ADR smuggles in.

Reload-induced findings never block the reload (it already happened) and
never fail a build (they live in the workspace session, not the artifact).

## Prior Art

| System | Approach | Take / leave |
|---|---|---|
| **Erlang/OTP** | Hot reload with `code_change/3`; type-blind | The substrate; we add the missing feedback loop on top of unchanged mechanics. |
| **Gleam** | FAQ: upgrades cannot be type-checked | The conceded gap this ADR fills. |
| **TypeScript LSP / watch mode** | Incremental re-check on file change with a dependency graph | The playbook (principle 12) — but tsc re-checks *files against files*; Beamtalk re-checks *the running image against its own next generation*. |
| **Smalltalk (Pharo)** | Live redefinition, no static re-check; browser marks broken senders only syntactically | The liveness model; we add typed feedback Pharo never had. |
| **Rust/hot-patching research (e.g. Erlang's relups, dynamic software update literature)** | Update-safety proofs, typestate for upgrades | Sound update-checking is research-grade; deliberately out of scope — we inform, not verify. |

## User Impact

- **Newcomer:** hot reload stops being scary — the system says what the edit
  broke, immediately, with locations. This is the feature that teaches why
  liveness + types is the pitch.
- **Smalltalk developer:** the Pharo "senders of" reflex becomes automatic —
  every save effectively runs a typed senders-check.
- **Erlang/BEAM developer:** the answer to "what does this deploy break?"
  before the crash report, using the same reload they already do.
- **Operator:** workspace-session feature; production reload paths unchanged.
  (A future CI mode — "re-check against a running staging image" — is noted,
  not designed.)
- **Tooling developer:** diagnostics flow through existing LSP publishing;
  new work is the reload→re-check trigger and the reload-induced tag.

## Steelman Analysis

- ⚙️ **BEAM veteran, for doing nothing:** "OTP release handling has managed
  untyped upgrades for decades; discipline works." — Countered: at OTP shops,
  that discipline is senior engineers reading diffs. The check automates
  exactly that reading, and stays advisory.
- 🎨 **Language designer, for sound update-checking:** "Inform-only lets a
  stale caller run; verify the upgrade or don't ship types." — Acknowledged:
  sound dynamic-software-update is research-grade and would reintroduce the
  Gleam trade (safety by forbidding liveness). Against the positioning
  ("types serve a live system, advisory"), inform-only is the consistent
  choice.
- 🧑‍💻 **Newcomer, for blocking reloads on errors:** "If it's wrong, stop
  me!" — Partially accommodated: a *pre-save* check in the editor (LSP already
  sees the edit before the reload) can warn before installing; the post-reload
  image check remains non-blocking. The pre-save path is Phase 3.

### Tension point
One-level vs transitive re-checking. Transitive is more complete but unbounded
in a big image; one-level is fast and catches the direct damage. Start
one-level; measure; ADR 0087's benchmarking precedent (BT-2299) applies.

## Alternatives Considered

### Whole-image re-check on every reload
Simple and complete; unbounded latency as images grow. Rejected as the
default; may become an explicit command (`:recheck image`).

### Per-caller (single-method) re-check
The ideal granularity — but no single-method check API exists (the checker's
entry points are whole-`Module`). Building one is real infrastructure.
**Adopted middle ground instead:** re-check the affected caller's whole
enclosing compilation unit, batched into one port request (Mechanism step 3).
Per-file is coarser than per-method but bounded, and reuses `check_module`
unchanged; the single-method API is a later optimisation if file-granularity
latency disappoints.

### Extend xref with a receiver-type key
Would make dependent lookup precise instead of selector-wide (Mechanism step
2's filter). Not chosen now — it changes ADR 0087's shipped schema and
generation lifecycle for a cost that only matters if common-selector fan-out
proves hot in practice. Recorded as the follow-up, with the per-reload caller
cap as the interim guard.

### Static-only (no runtime trigger)
Let the LSP re-check open files on edit, ignore the live image. Rejected: the
whole point is the *image* — callers in files nobody has open are exactly the
ones people forget.

### Sound upgrade verification
Prove reload safety before install. Rejected (see steelman): research-grade,
anti-liveness, and off-positioning.

## Consequences

### Positive
- The thirty-second demo no BEAM language can give: live signature change →
  stale callers flagged with locations, from the running image.
- Builds on shipped infrastructure (xref, compiler port, severity policy, LSP
  publishing) — but honestly: the **new** code is real, not glue. Signature
  capture at patch time (because hot-patch *clears* type metadata),
  receiver-type filtering over selector-keyed xref results, and batched
  whole-file re-checks are all net-new plumbing (see Mechanism). What's
  reused is the checker itself, the index, and the diagnostic channels.
- Would make ADR 0104's actor typing durable under liveness (actor interface
  changes re-check senders) — noting 0104 is itself Proposed, so this is a
  benefit contingent on an unbuilt dependency.

### Negative
- The workspace runtime must call back into the Rust compiler for re-checks —
  a new runtime→compiler interaction path with failure modes (compiler
  unavailable, version skew between image and sources) that need explicit
  handling.
- xref covers *compiled* call sites; workspace-defined bindings/snippets need
  their own dependent tracking (session bindings layer, ADR 0081).
- **Proxy-routed calls are invisible to the dependent lookup.** A call through
  a forwarding proxy (`counter withTimeout: 5000` then `getCount` — ADR 0104
  §4) reaches `Counter` via the proxy's DNU forwarding, so xref records the
  site under the proxy path, not `Counter>>getCount`; a signature change to
  `Counter` can produce a false "no stale callers" for proxy-wrapped usage —
  exactly the "files nobody has open" case this ADR exists for. Fix path:
  either track proxies as meta-dependents of the wrapped class's interface in
  xref, or (interim) accept and document the gap. **The Phase 0 spike (BT-2776)
  established the miss scope, and it is worse than "under the proxy path": a
  proxy-routed send records _no_ site at all (absent from `senders_of`
  entirely), so the false "no stale callers" is guaranteed for proxy-wrapped
  usage — an explicit decision is required in the core phases.**
- Interface deltas require previous-generation metadata retention — small but
  new state in the class registry.
- Findings can be stale-about-staleness: a caller flagged after reload A may
  be fixed by reload B; clearing logic must be reliable or trust erodes.

### Neutral
- Production/artifact builds unchanged; this is a workspace-session feature.
- Adds no new severity levels — slots into ADR 0100.

## Implementation

1. **Phase 0 (napkin, ~M — this is the risky part, not an ~S):** one case
   end-to-end, proving the three assumptions the reviewers flagged: (a)
   signature captured from the **compile response at patch time** (before
   `put_method/4` clears type metadata) and retained across generations; (b)
   selector-keyed xref lookup + receiver-type filter finds the right caller;
   (c) a batched whole-file re-check through the compiler port returns a
   diagnostic within interactive latency. One method, one caller, one LSP
   diagnostic — reading the caller's **live** `method_source`, not disk.
2. **Phase 1 (~M):** generalise: per-selector signature store with generation
   handling (incl. repeated edits to the same method); removed-selector and
   signature-change findings at ADR 0100 severities; caller cap + "N more not
   checked" note; surface-parity plumbing (LSP + workspace UI + REPL notice);
   clearing on re-edit / `Workspace changes revert:` / restart.
3. **Phase 2 (~M):** shape changes (`state:`/`field:`) integrated with ADR
   0104's `spawnWith:`/accessor checking (once 0104 lands); benchmarks per
   the BT-2299 precedent — including the common-selector fan-out case that
   decides whether the xref receiver-key extension is needed.
4. **Phase 3 (~S):** pre-save advisory in the editor (check before install);
   explicit `:recheck image` command (surface-parity entry required).

**Affected:** workspace runtime (`beamtalk_workspace`, reload/ClassBuilder
hooks), `beamtalk_xref` queries, language service (`beamtalk-core`), LSP/REPL
surfaces. **Not:** codegen output, production reload mechanics, the artifact
build.

## References
- Seed: `docs/internal/positioning.md` (Seed 3 — "the killer demo")
- Enabling ADRs: ADR 0087 (xref index, Implemented — the dependency graph),
  ADR 0082 (method-level edit/save — the trigger), ADR 0100 (severity),
  ADR 0104 (actor interface checking), ADR 0081 (session bindings)
- Principles: 2 (Hot Reload is Core), 11 (Live Patching is a Message Send),
  12 (Compiler is the Language Service)
- External: [Gleam FAQ on hot code reloading](https://gleam.run/frequently-asked-questions/)
