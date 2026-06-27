# ADR 0100: Open-World Diagnostic Policy for Unresolved Selectors and Classes

## Status
Proposed (2026-06-27)

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

### Rule 2 — Completeness improves accuracy, it does not raise severity

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

### Rule 3 — Escalation is opt-in and per-category

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

This is the strongest competitor and the tension is real: a *complete closed*
receiver is arguably as provable as the union case that already warns. It was not
chosen as the default because (a) a single concrete receiver still has the
`perform:`/extension/hot-reload escape that a frozen union analysis less commonly
hits in practice, (b) raising the floor the moment project-scope lands would make
WS1/WS2 *feel* like a regression ("why did my hints become warnings?"), violating
Rule 2's promise that precision ≠ strictness, and (c) anyone who wants this can
get it per-category via Rule 3. If post-WS1 false-positive rates prove low, the
default ceiling for the closed-complete case can be revisited — the policy seam
(Rule 3) is where that lever lives.

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

- **Semantic analysis** (`crates/beamtalk-core/src/semantic_analysis/type_checker/`):
  centralise the severity decision behind the completeness ladder (Rule 1) so
  `validation.rs` / `inference.rs` consult one place rather than scattered
  `Diagnostic::hint` / `::warning` calls. Introduce an explicit
  "receiver knowledge" notion (`Dynamic` / `Open` / `ClosedComplete`) that the
  send-checker switches on.
- **Remove the cross-file-parent suppression** when project-wide hierarchy (WS2)
  and project-wide extension registration (WS1) are in place — resolution
  replaces suppression (Rule 2).
- **Escalation** (`crates/beamtalk-cli/src/beam_compiler.rs`): no change to
  `--warnings-as-errors` now; the `beamtalk.toml` per-category table (Rule 3) is a
  separate, later issue.
- **No runtime/codegen changes.**

## References
- Related issues: BT-2251 (epic: project-scoped compilation & type-checking;
  WS5 is this policy), BT-2250, BT-2228
- Related ADRs: ADR 0024 (static-first, live-augmented tooling), ADR 0066 (open
  class extension methods), ADR 0077 (type coverage visibility / `@expect`),
  ADR 0070 (package namespaces & dependencies — cross-package interface)
- Prior art: Erlang Dialyzer success typings; TypeScript `strict` flags; Elixir
  `mix xref`; Gleam (closed-world contrast)
