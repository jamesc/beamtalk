# ADR 0100: Open-World Diagnostic Policy for Unresolved Selectors and Classes

## Status
Accepted (2026-07-10; proposed 2026-06-27)

**Implementation status (2026-07-10):** all three rules are implemented.
Rule 1 and Rule 3 landed via BT-2793. Rule 2 landed via BT-2796 (WS2:
`KnowledgeScope` completeness contract + parse-error guard), BT-2795 (WS1:
project-wide extension visibility), and BT-2794 (pre-WS3 dependency guard) —
see the Rule 2 section for how "removal" of the cross-file-parent
suppression was realised. WS3 (cross-package extension metadata, ADR 0070
amendment) remains future work; until it lands, packages that declare
dependencies get no unresolved-selector hints in project-complete builds
(the Rule 1 third downgrade, implemented as the BT-2794 guard).

## Context

### Problem statement

Beamtalk is a dynamic, open-world language: any object may implement
`doesNotUnderstand:`, selectors can be sent reflectively via `perform:`, classes
can be extended at runtime (ADR 0066), and methods can be hot-reloaded while the
system runs. In such a world a *statically unresolved* message send is **not**
proof of a bug — the receiver may understand the message at runtime through a
mechanism the checker cannot see.

At the same time, the type checker *can* often prove that a send will fail (a
concrete, closed receiver type whose whole method surface is known and does not
include the selector). Reporting nothing in that case wastes a real signal —
typos, renamed methods, and wrong receivers are exactly the errors a Smalltalker
expects the tooling to catch.

The tension is therefore not "should we report unresolved sends?" but **"with
what severity, given how complete the checker's knowledge is?"** That question
has never been written down. The behaviour exists in code, was assembled
incrementally, and includes at least one workaround (suppressing diagnostics
whenever a receiver has a cross-file parent) that is really a stand-in for "my
static knowledge is incomplete here." The project-scoped compilation epic
(BT-2251) is about to make that knowledge *complete* for the intra-project case,
which forces the question: when the checker becomes more certain, does it become
more severe? This ADR fixes the policy so that WS1/WS2 (project scope) and a
future strict mode have a principled rule to build against, rather than each
escalating severity ad hoc.

### Current state (as of this ADR)

Severity and category infrastructure already exists
(`crates/beamtalk-core/src/source_analysis/parser/mod.rs`):

- **Severities:** `Error` (blocks compile), `Warning`, `Lint` (shown by
  `beamtalk lint`), `Hint` (informational note).
- **Categories** (`DiagnosticCategory`): `Dnu`, `Type`, `UnresolvedClass`,
  `UnresolvedFfi`, `ArityMismatch`, `Deprecation`, … — these let policy and
  suppression target a class of diagnostic rather than a message string.

The de-facto behaviour for an unresolved *selector*
(`type_checker/validation.rs`, `type_checker/inference.rs`):

| Receiver situation | Today |
|---|---|
| Unresolved selector on a **known, concrete** class | `Hint` (`Dnu`) |
| Receiver is **Dynamic / unknown** | **Silent** (no diagnostic) |
| **Union** where *no* member responds and there is no uncertainty | `Warning` |
| Union, some members respond or membership is uncertain | `Hint` |
| Class declares a `doesNotUnderstand:` override | Silent |
| Class has a **cross-file parent** | Silent *(incompleteness workaround)* |

Unresolved *classes* and *FFI* targets emit `Warning`
(`UnresolvedClass` / `UnresolvedFfi`). `@expect dnu` / `@expect type` suppress at
a site (ADR 0077). `--warnings-as-errors` promotes `Warning`/`Hint` to `Error`,
but **excludes** `UnresolvedClass`, `UnresolvedFfi`, `ArityMismatch`, and
`Deprecation` for gradual migration; `Dnu` is **not** excluded, so today
`--warnings-as-errors` already makes unresolved selectors fatal.

### Constraints

- **The language guarantees the escape hatches** — `doesNotUnderstand:`,
  `perform:`, runtime extensions, hot reload. A default that rejects code which
  these make valid would break the language's contract (ADR 0066, ADR 0024).
- **Reload and live development** (ADR 0024 static-first, live-augmented): a file
  may not resolve in isolation yet resolve fine against the running image. The
  static layer must not be more fatal than the live reality.
- **Whatever the policy, it must be expressible per-category**, because the right
  answer differs for "selector on a closed receiver" vs. "imported class does not
  exist."

## Decision

**Severity is a function of how complete the checker's knowledge is, and
escalation to a build-failing error is always opt-in — never the default.**

Concretely, three rules:

### Rule 1 — Knowledge gates severity (the "completeness ladder")

**Implemented (BT-2793):** the classifier lives in
`crates/beamtalk-core/src/semantic_analysis/receiver_knowledge.rs`
(`ReceiverKnowledge` / `classify_receiver`), consulted from the four sites
that previously re-derived the decision independently:
`type_checker/validation.rs::check_class_side_send` and
`check_instance_selector`, and `protocol_registry.rs::check_conformance_to_protocol`
and `check_class_side_conformance`. This was a pure refactor of *where* the
decision lives — behaviour was unchanged at the time; Rule 2 (below)
subsequently made the `has_cross_file_parent` check fire only where
knowledge is genuinely incomplete.

Diagnose an unresolved selector only as severely as the checker's certainty
warrants. Absence of evidence is not evidence of absence in an open world.

| Checker's knowledge of the receiver | Default severity |
|---|---|
| **Unknown / `Dynamic`** — cannot enumerate the method surface | **Silent** |
| **Open** — known class but with a `doesNotUnderstand:` handler, or an open extension point the checker can't close | **Silent** |
| **Closed & complete** — concrete known class; full method surface known across the project *and* dependency interfaces; no DNU handler; not `Dynamic` | **`Hint`** (`Dnu`) |
| **Provably failing union** — every member is closed and *none* responds, no uncertainty | **`Warning`** (`Dnu`) |
| **Unresolved class / FFI target** — the *name* resolves to nothing | **`Warning`** (`UnresolvedClass` / `UnresolvedFfi`) |

The default ceiling for a *send* the checker believes will fail is **`Hint`** —
or `Warning` only when failure is *provable* (the union case). This mirrors
Dialyzer's success-typing stance: complain only when a call can be *shown* to
fail, stay quiet otherwise.

**Classification is conservative — when in doubt, classify *down*.** A receiver is
`ClosedComplete` only when the checker can enumerate its *entire* method surface
with certainty. The following force a downgrade to `Open` (→ silent), so they are
never misreported:

- **Any `Dynamic` in a union** (`Integer | Dynamic`) downgrades the *whole* union
  to `Open`. The checker does not emit per-arm hints on a mixed union — a partial
  "hint on the `Integer` arm, silence on the `Dynamic` arm" is more confusing than
  useful, and the `Dynamic` arm could be the one that flows at runtime.
- **A receiver that is also the target of `perform:` / reflective dispatch** in
  scope is `Open` for the reflectively-sent selectors — a typed receiver does not
  become provably-closed just because it has a static type.
- **A class whose method surface depends on data not yet loaded** — e.g. a
  dependency type whose *extension* metadata has not been loaded (pre-WS3, see
  ADR 0070 amendment). The checker must treat "I have the class but maybe not all
  its extensions" as `Open`, not `ClosedComplete` (see Rule 2 sequencing).
  Note the scope this implies: because *any* class — including stdlib `String`,
  `Integer`, `Array` — can be extended by a dependency, until WS3 loads
  cross-package extension metadata the only receivers that reach `ClosedComplete`
  are classes the checker can confirm are dependency-extension-free (in practice,
  classes local to a package with no dependencies). So WS1/WS2 alone deliver the
  `Hint` precision benefit only for local, dependency-free types; the full benefit
  for stdlib and imported types waits on WS3. This is intentional, not a gap.

Because severity hinges entirely on this classification, the
`Dynamic`/`Open`/`ClosedComplete` decision MUST be made in **one shared place** so
`validation.rs` and `inference.rs` cannot disagree about the same call site
(see Implementation).

### Rule 2 — Completeness improves accuracy, it does not raise severity

**Implemented (BT-2796 + BT-2795 + BT-2794).** How "removing the
suppression" was realised, per the design note
(`docs/plans/2026-07-10-bt-2795-bt-2796-project-scoped-knowledge.md`):
suppression was replaced by *resolution*, not by deleting the check. WS2
(BT-2796) introduced the `KnowledgeScope` completeness contract
(`ModuleOnly` / `ProjectComplete`, claimed only by orchestrators that walk
the whole project) plus a per-receiver parse-error guard
(`surface_incomplete`). WS1 (BT-2795) made project-wide extensions visible
on every surface (CLI build Pass 1, lint's package walk, the LSP
`ProjectIndex`). With intra-project parents and extensions injected,
`has_cross_file_parent` no longer fires in project-complete builds except
for genuinely-unresolved parents — where staying `Open` is the correct
conservative answer (the class already carries an `UnresolvedClass`
warning). `ModuleOnly` contexts (REPL, isolated single files,
budget-exhausted LSP preloads) keep the pre-Rule-2 behaviour automatically.
BT-2794 added the pre-WS3 dependency guard from the sequencing bullet below:
a package that declares dependencies has **no** `ClosedComplete` receivers
until WS3, because a dependency can extend any class — including `Object`,
which every receiver's chain reaches. (This supersedes the design note's
tentative "package-local classes are dependency-extension-free" refinement:
it is false for *inherited* extensions on shared roots.)

When project-scoped compilation (BT-2251 WS1/WS2) gives the checker complete
intra-project knowledge, it **replaces the incompleteness workarounds with real
resolution** — but the *default severity is unchanged*. Specifically:

- The "**cross-file parent → silent**" suppression is **removed**. It was a proxy
  for "I can't see the parent's methods." Once the project hierarchy is assembled
  (WS2) and project-wide extensions are registered (WS1), the checker *can* see
  them, so it resolves the send for real instead of suppressing.
- A same-project cross-file extension (`String >> shoutLouder` in another file)
  that today produces a **false `Hint`** will simply **resolve** and produce
  **nothing**. The number of `Hint`s goes *down*, not up.
- A genuinely-unresolved selector on a now-fully-known closed receiver stays a
  **`Hint`**. Knowing more makes the Hint *trustworthy*; it does not make it an
  error.

This is the crux: **project scope buys precision, not strictness.** Strictness is
a separate, explicit choice (Rule 3).

**Sequencing guard (important).** The incompleteness workarounds may be removed
*only* once the knowledge that made them necessary actually exists — removing them
early turns every previously-suppressed site into a fresh false positive. The
binding order:

1. The **cross-file-parent** suppression stays until project-wide hierarchy
   assembly (WS2) is verified complete.
2. A receiver whose class may carry **cross-package extensions** stays classified
   `Open` (Rule 1) until cross-package extension metadata is loaded (WS3 / the
   ADR 0070 amendment). Until then, a fully-known *intra-project* surface is **not**
   sufficient to call such a receiver `ClosedComplete`, or the checker would emit a
   `Hint` for a dependency-contributed method that genuinely exists.

In other words: **suppression is removed per-receiver only when the checker can
prove its surface is complete for *that* receiver**, not globally when "WS1 has
landed." A feature flag gates the removal until the corresponding workstream is
verified, so a partially-landed epic never regresses diagnostics.

### Rule 3 — Escalation is opt-in and per-category

**Implemented (BT-2793):** the `[diagnostics]` table in `beamtalk.toml`,
parsed in `crates/beamtalk-cli/src/commands/manifest.rs`
(`DiagnosticsTable` / `DiagnosticSeverityOverride`) and applied in
`crates/beamtalk-cli/src/beam_compiler.rs`
(`apply_diagnostics_table`, ahead of the `--warnings-as-errors` promotion
pass — precedence steps 2 and 3 below). `apply_diagnostics_table` enforces a
**severity floor**: it never touches a diagnostic that already carries
`Severity::Error` (e.g. `ActorNew`, `Inheritance`, `EmptyBody` — hard
structural errors, not Rule 1 completeness-ladder output), so the table can
only move the soft diagnostics this rule is about, never silence a
guaranteed compile error. See the
[Package Management guide](../beamtalk-packages.md) (`[diagnostics]` Section)
for the user-facing schema, the severity floor, and examples.

Promotion of soft diagnostics to build-failing `Error`s is never the default. It
is requested explicitly:

- `--warnings-as-errors` keeps its current meaning (promote `Warning`/`Hint` to
  `Error`), and keeps excluding the gradual-migration categories
  (`UnresolvedClass`, `UnresolvedFfi`, `ArityMismatch`, `Deprecation`).
- `@expect dnu` / `@expect type` (ADR 0077) remain the site-level *down*-grade
  (acknowledge-and-silence) for the irreducibly-dynamic call.
- A future per-category strictness table in `beamtalk.toml` (e.g. treat `Dnu` as
  `Error` in a mature application package) is the intended home for project-level
  escalation. This ADR reserves the policy seam but does not define the schema —
  that is a follow-up once project scope lands and real false-positive rates are
  known.

**Precedence (so the sources never silently conflict).** A `Dnu` diagnostic's
final disposition is resolved most-specific-wins, in this order:

1. **Site-level `@expect dnu` / `@expect type`** (ADR 0077) — always wins; an
   explicit acknowledgement at the call site silences it regardless of any global
   setting. (A site that resolves after WS1/WS2 makes its `@expect` *stale*; the
   existing stale-directive warning — BT-1412,
   `queries/diagnostic_provider.rs` — already flags an `@expect` with no matching
   diagnostic, so improved resolution self-reports the now-dead annotation with no
   new mechanism.)
2. **Per-category project table** (`beamtalk.toml`, future) — sets the category's
   base severity for the package (`ignore` / `hint` / `warn` / `error`).
3. **Rule 1 default** — the completeness-ladder severity, when nothing above
   applies.

`--warnings-as-errors` is a **final promotion pass**, not a competing base
severity: after 1–3 resolve a diagnostic to `Warning`/`Hint`, the flag promotes it
to `Error` (minus the gradual-migration exclusions). So a `dnu = "ignore"` table
entry removes the diagnostic before the flag ever sees it, while `dnu = "warn"`
plus `--warnings-as-errors` yields an error (the `beamtalk.toml` keys here are
illustrative — the actual schema is deferred, per Rule 3). This keeps the flag's
meaning ("treat what remains as fatal") intact and orthogonal to the base policy.

### What a user sees

REPL — a typo on a closed receiver is a soft, navigable hint, and the expression
still runs (the runtime raises the real DNU if reached):

```beamtalk
"hello" reverssed
// hint: 'String' does not appear to understand #reverssed (did you mean #reversed?)
// => the expression still compiles and runs; runtime raises doesNotUnderstand: if evaluated
```

Dynamic receiver — silence, because the checker cannot know:

```beamtalk
x := someApi fetchThing.   // x : Dynamic
x wobble                   // no diagnostic — open world, may be understood at runtime
```

Provable failure on a closed union — a `Warning`, the strongest soft signal:

```beamtalk
n := flag ifTrue: [1] ifFalse: [2.0].   // n : Integer | Float
n frobnicate
// warning: no member of 'Integer | Float' understands #frobnicate
```

Opt-in strict build — the same hint, now fatal, by explicit request:

```text
$ beamtalk build --warnings-as-errors
error: 'String' does not appear to understand #reverssed   (Dnu, promoted)
```

## Prior Art

- **Smalltalk (Pharo/Squeak):** no static selector checking at all — an unknown
  send becomes `doesNotUnderstand:` *at runtime*, often a teachable moment (the
  debugger offers to create the method). Pure open world. Beamtalk keeps the
  open-world *default* but adds a static **hint** layer Pharo lacks, without
  making it fatal.
- **Erlang Dialyzer (success typings):** the guiding analog. Dialyzer never warns
  unless it can *prove* a call cannot succeed — it tolerates anything it cannot
  disprove. Rule 1's "Warning only on provable failure, Hint otherwise, silent
  when unknown" is success-typing applied to message sends.
- **Elixir (`mix xref` / compiler warnings):** unresolved remote calls are
  *warnings*, not errors, and dynamic dispatch (`apply/3`) is exempt — the same
  "soft by default, dynamic is invisible" posture.
- **Gleam:** the opposite pole — sound static types, closed world, unresolved =
  hard compile error. Deliberately *rejected* as a default here: it is incoherent
  with `doesNotUnderstand:`, runtime extensions, and hot reload. It is, however,
  what Rule 3's opt-in strict mode approximates for teams who want it.
- **TypeScript:** gradual typing with **opt-in strictness flags**
  (`noImplicitAny`, `strict`). Directly inspires Rule 3 — the language ships
  permissive, and severity is dialled up per-project by explicit configuration,
  not by the checker getting cleverer.

What we adopt: Dialyzer's provability bar (Rule 1) and TypeScript's opt-in
escalation (Rule 3). What we reject: Gleam's closed-world fatality as a default.

## User Impact

- **Newcomer (from Python/JS/Ruby):** gets a gentle, navigable hint on typos
  ("did you mean #reversed?") without the build breaking — matches the
  exploratory feel they expect, and the runtime still raises a clear DNU if they
  run it. They are never blocked by the checker disagreeing with valid dynamic
  code they copied from a tutorial.
- **Smalltalk developer:** the open-world default is *familiar and correct* —
  `doesNotUnderstand:` is sacred, and the checker never pretends a send is
  illegal just because it can't see the implementor. The added static hint is a
  bonus over Pharo, not a constraint.
- **Erlang/BEAM developer:** the success-typing posture is exactly Dialyzer's,
  which they already trust; "warn only when provably wrong" reads as correct and
  low-noise. Opt-in `--warnings-as-errors` fits CI gating habits.
- **Production operator:** no behavioural change to running code — diagnostics are
  a build/edit-time concern. Hot reload and live patches are never blocked by a
  stale static view, because the static layer is non-fatal by default and the
  live image is authoritative (ADR 0024).
- **Tooling developer (LSP/MCP):** a stable `(severity, category)` contract per
  diagnostic, with `Dnu` clearly separated, makes it trivial to render hints
  distinctly from errors and to offer "create method" / "did you mean" actions.

### Discoverability

The `did you mean` note rides the existing `hint`/`notes` fields on `Diagnostic`,
so editors and the REPL surface the suggestion without new infrastructure. The
`@expect` directive (ADR 0077) is the discoverable, in-source way to mark a send
as intentionally dynamic.

## Steelman Analysis

### Option B — Escalate to Warning when statically complete

- 🧑‍💻 **Newcomer:** "A yellow squiggle is more visible than a grey hint — I'd
  notice my typo sooner."
- 🎩 **Smalltalk purist:** "If you've *proven* the receiver is closed and lacks
  the method, that's as real as a union failure — treat it the same."
- ⚙️ **BEAM veteran:** "Dialyzer surfaces provable failures as warnings, not
  notes; a complete closed receiver is provable, so Warning is the honest level."
- 🏭 **Operator:** "Warnings show up in CI summaries; hints get lost."
- 🎨 **Language designer:** "Severity tracking certainty argues that *maximum*
  certainty (closed + complete) deserves the higher soft level."

This is the strongest competitor, and the tension is real — honestly, a complete
closed receiver is *exactly as provable* as the union case that already warns. The
distinction is **not** provability (claiming otherwise would be inconsistent: both
rest on "every candidate class is closed and none responds"). The distinction is
**blast radius and frequency**:

- The "no union member responds" case is **rare** — it requires the user to have
  written branching control flow whose arms disagree, then send a selector none of
  them has. When it fires it is almost always a genuine bug, so `Warning` is
  low-noise.
- A bare send to a single concrete receiver is the **most common shape in the
  language**. Making it `Warning`-by-default the instant WS1/WS2 land would convert
  a large body of today's quiet hints into warnings overnight — the precise
  "project scope feels like a regression" outcome Rule 2 promises to avoid.

So the union `Warning` is retained as an existing, *narrow* aggressive case, and
the common single-receiver case stays `Hint` — with escalation available
per-category (Rule 3) for teams that want union-level strictness everywhere. If
post-WS1 false-positive rates prove low, raising the closed-complete ceiling is a
one-line change to that table, not a re-decision of this ADR.

### Option C — Hard error when statically complete (Gleam-style)

- 🧑‍💻 **Newcomer:** "Other languages I know just tell me it's wrong — no
  ambiguity."
- 🎩 **Smalltalk purist:** (cannot honestly steelman — fatal static rejection
  contradicts `doesNotUnderstand:`; the strongest they'd offer is "for a *sealed*
  Value class with no DNU handler, it truly cannot be understood.")
- ⚙️ **BEAM veteran:** "A guaranteed-crash call is worth failing the build over."
- 🏭 **Operator:** "I want CI red on provable bugs."
- 🎨 **Language designer:** "Soundness where soundness is achievable."

Rejected as a default: it breaks the language's open-world guarantee and the
live-augmented model (a file that fails static checking in isolation may resolve
against the running image). The legitimate piece of this — sealed `Value` classes
with no DNU handler are a genuinely closed sub-world — is precisely the kind of
narrow rule Rule 3's opt-in table can encode later, not a global default.

### Tension points

- **Hint vs Warning for the closed-complete case** is the one place reasonable
  people disagree (Option B). Resolved in favour of Hint to keep Rule 2's
  "project scope buys precision, not strictness" promise, with the escalation lever
  explicitly preserved.
- **BEAM veterans/operators lean stricter; Smalltalkers/newcomers lean softer.**
  Rule 3 (opt-in per-category escalation) is the release valve that lets both
  camps get what they want without a global default that hurts the other.

## Alternatives Considered

### Promote unresolved selectors to Warning/Error by default
Covered as Options B/C above — rejected as defaults, preserved as opt-in (Rule 3).

### Keep the cross-file-parent suppression permanently
Rejected. It is an incompleteness workaround, not a policy: it silences *real*
typos whenever a class happens to have an out-of-file parent. Project scope (WS2)
removes the incompleteness, so the workaround should go with it (Rule 2), not
calcify into the contract.

### Route `Dnu` to the `Lint` channel instead of `Hint`
A genuine option: emit unresolved-selector diagnostics at `Severity::Lint`
(invisible in a normal `beamtalk build`, surfaced only by `beamtalk lint`) rather
than `Hint` (always shown as an informational note). Lint gives a *stronger*
opt-in separation than Hint — the default build is completely silent on
unresolved sends. Rejected as the default because the typo-catching value of the
hint is highest *in the editor, inline, as you type* (LSP surfaces `Hint`s;
`Lint` implies a separate command run), and Smalltalkers expect the tool to point
at a suspicious send immediately, not on a deferred lint pass. `Lint` remains
available as a per-category choice under Rule 3 for teams who want unresolved
sends out of the default build entirely.

### One global strictness flag only (no categories)
Rejected. `UnresolvedClass`/`UnresolvedFfi`/`ArityMismatch` already need
independent migration treatment (they are excluded from `--warnings-as-errors`
today). A category-aware policy is already a de-facto requirement; this ADR names
it rather than inventing it.

## Consequences

### Positive
- A written, principled rule the project-scope work (WS1/WS2) and any future
  strict mode build against, instead of escalating severity ad hoc.
- Fewer false positives after WS1/WS2: same-project cross-file extensions resolve
  for real, so today's false `Dnu` hints disappear (Rule 2).
- The open-world contract (`doesNotUnderstand:`, `perform:`, extensions, hot
  reload) is never violated by a default — preserving ADR 0024 and ADR 0066.
- Clear `(severity, category)` semantics for LSP/MCP rendering and `@expect`.

### Negative
- A real typo on a closed receiver stays a *hint* by default — quieter than some
  users (Option B/C steelman) would prefer. Mitigated by `did you mean` notes and
  opt-in escalation.
- Defers the per-category strictness schema to a follow-up, so "make `Dnu` fatal
  for this package" is not yet expressible beyond the global flag.

### Neutral
- No change to runtime behaviour or generated code — purely a build/edit-time
  diagnostic policy.
- Largely codifies and rationalises existing behaviour; the only near-term code
  change is removing the cross-file-parent suppression once WS2 lands.

## Implementation

This ADR is policy; the code changes ride the BT-2251 workstreams.

- **Semantic analysis** (`crates/beamtalk-core/src/semantic_analysis/`):
  **done (BT-2793).** The severity decision is centralised behind the
  completeness ladder (Rule 1) in `receiver_knowledge.rs` — an explicit
  "receiver knowledge" notion (`Dynamic` / `Open` / `ClosedComplete`,
  `classify_receiver`) that `type_checker/validation.rs` and
  `protocol_registry.rs` consult instead of each re-deriving the
  suppression/severity decision. This was a pure refactor of *where* the
  decision lives — `has_cross_file_parent` still forces `Open`, so behaviour
  is unchanged pending Rule 2.
- **Remove the cross-file-parent suppression** when project-wide hierarchy (WS2)
  and project-wide extension registration (WS1) are in place — resolution
  replaces suppression (Rule 2). **Done** (BT-2796, BT-2795, BT-2794); see the
  Rule 2 section for the realised mechanism (`KnowledgeScope`, per-receiver
  parse-error guard, pre-WS3 dependency guard).
- **Escalation** (`crates/beamtalk-cli/src/beam_compiler.rs`,
  `crates/beamtalk-cli/src/commands/manifest.rs`): **done (BT-2793).** The
  `beamtalk.toml` `[diagnostics]` per-category table (Rule 3) is implemented;
  `--warnings-as-errors` itself is unchanged (still promotes `Warning`/`Hint`
  to `Error`, excluding the gradual-migration categories unless the table
  explicitly overrides one of them).
- **No runtime/codegen changes.**
- **Coordination with ADR 0101 (`native:` for stateless Objects):** the
  receiver-knowledge scan must count `self delegate` `native:` methods as
  *declared methods*. A `native:` Object is `ClosedComplete`
  (no extension hook → a DNU hint on an unknown selector is correct), but its
  delegated methods have only a `self delegate` sentinel body, so a naive
  "has a real body?" check would treat them as missing and emit false `Dnu` hints. `@primitive` classes (including
  the reflection facades) remain `Open`. See ADR 0101 § Migration Path.

## Migration Path

This ADR mostly codifies existing behaviour, so most code needs no change. Two
transitions warrant care:

- **Teams already running `--warnings-as-errors`** will see *new* build failures
  when WS1/WS2 land — not because their code changed, but because improved
  resolution surfaces previously-*suppressed* true positives (e.g. a real typo on a
  class that happened to have a cross-file parent). This is a genuine, if benign,
  tightening. Mitigation: WS1/WS2 ship behind the Rule 2 sequencing flag, and the
  workstream that flips it should call out the expected new diagnostics in its
  changelog so CI breakage is anticipated, not mysterious. Affected sites are fixed
  by correcting the selector or adding `@expect dnu` where the send is intentionally
  dynamic.
- **Stale `@expect` directives**: a site that becomes resolvable after WS1/WS2 has a
  now-unnecessary `@expect`; the existing stale-directive warning (BT-1412,
  `queries/diagnostic_provider.rs`) flags these automatically, so cleanup is
  mechanical and self-surfacing.

## References
- Related issues: BT-2251 (epic: project-scoped compilation & type-checking;
  WS5 is this policy), BT-2250, BT-2228; BT-2793 (Rule 1 + Rule 3,
  implemented), BT-2794 (Rule 2, blocked on BT-2795/BT-2796)
- Related ADRs: ADR 0024 (static-first, live-augmented tooling), ADR 0066 (open
  class extension methods), ADR 0077 (type coverage visibility / `@expect`),
  ADR 0070 (package namespaces & dependencies — cross-package interface)
- Prior art: Erlang Dialyzer success typings; TypeScript `strict` flags; Elixir
  `mix xref`; Gleam (closed-world contrast)
