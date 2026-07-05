# ADR 0105: Live Image Re-Checking on Hot Reload

## Status
Proposed (2026-07-05)

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

- **ADR 0087 (Implemented):** `beamtalk_xref` maintains a selector→call-sites
  index in the runtime, kept correct across hot reload (purge + new-generation
  install) and ClassBuilder lifecycle hooks. That *is* the dependency graph
  incremental re-checking needs.
- **Principle 12:** the compiler is the language service — the same
  `beamtalk-core` that compiled the edit can re-check its dependents.
- **ADR 0100:** the severity ladder for "how certain is the checker" already
  exists; reload-induced findings slot straight into it.

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

1. **Signature diff.** When the workspace compiler (`beamtalk_repl_compiler` /
   ClassBuilder path) installs a redefinition, compute the *interface delta*
   against the previous generation: per selector — added / removed /
   signature-changed (using the same method metadata codegen already emits in
   `__beamtalk_meta`).
2. **Dependent lookup.** For each removed or signature-changed selector, query
   `beamtalk_xref` (ADR 0087) for its call sites: `(module, selector, arity) →
   [caller methods]`.
3. **Targeted re-check.** Re-run the type checker on exactly those caller
   methods against the *new* interface. One level only — a caller whose own
   interface is unchanged by the finding does not trigger further fan-out
   (its diagnostic is enough; its callers' relationship to *it* didn't
   change).
4. **Publish.** Findings appear as diagnostics attributed to the caller's
   source location, tagged as reload-induced, on every surface
   (surface-parity applies). They clear automatically when either side is
   edited to agree.

### The demo (REPL/workspace session)

```
bt> :load counter.bt
bt> counter := Counter spawn
bt> counter getCount + 1          // => 1

// ... in the editor: change `getCount -> Integer` to `getCount -> String`,
//     save (method-level hot reload, ADR 0082) ...

⚠ reload check: 2 callers of Counter>>getCount are now stale
   Dashboard>>refresh (dashboard.bt:14): `self counter getCount + 1`
     — `+ 1` expects a number, `getCount` now returns String
   StatsView>>render (stats.bt:31): argument to `Transcript show:` — OK? no:
     expects String — this caller is fine and is not reported
```

Only genuinely-affected callers surface; agreeing callers re-check clean and
stay silent.

### Error example (removal)

```
⚠ reload check: Counter>>reset was removed; 1 caller remains
   AdminPanel>>onClick (admin.bt:9): `counter reset` will raise
   does_not_understand at runtime
```

### Severity

Per ADR 0100's ladder: signature-mismatch and removed-selector findings on
statically-known receivers are `Warning`; anything reached through `Dynamic`,
`perform:`, or DNU-overriding receivers is silent or `Lint`. Reload-induced
findings never block the reload (it already happened) and never fail a build
(they live in the workspace session, not the artifact).

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
- Reuses shipped infrastructure (xref, language-service compiler, severity
  policy, LSP publishing) — the novel code is a diff + a trigger + a targeted
  re-check loop.
- Makes ADR 0104's actor typing durable under liveness (actor interface
  changes re-check senders).

### Negative
- The workspace runtime must call back into the Rust compiler for re-checks —
  a new runtime→compiler interaction path with failure modes (compiler
  unavailable, version skew between image and sources) that need explicit
  handling.
- xref covers *compiled* call sites; workspace-defined bindings/snippets need
  their own dependent tracking (session bindings layer, ADR 0081).
- Interface deltas require previous-generation metadata retention — small but
  new state in the class registry.
- Findings can be stale-about-staleness: a caller flagged after reload A may
  be fixed by reload B; clearing logic must be reliable or trust erodes.

### Neutral
- Production/artifact builds unchanged; this is a workspace-session feature.
- Adds no new severity levels — slots into ADR 0100.

## Implementation

1. **Phase 0 (napkin, ~S):** hard-code one case end-to-end — method signature
   change via workspace save → xref lookup → re-check one caller → one LSP
   diagnostic. Proves the runtime→compiler round-trip, which is the risky
   part.
2. **Phase 1 (~M):** interface-delta computation from `__beamtalk_meta`
   generations; removed-selector and signature-change findings; surface-parity
   plumbing (LSP + workspace UI + REPL notice).
3. **Phase 2 (~M):** shape changes (`state:`/`field:`) integrated with ADR
   0104's `spawnWith:`/accessor checking; clearing logic; benchmarks per the
   BT-2299 precedent.
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
