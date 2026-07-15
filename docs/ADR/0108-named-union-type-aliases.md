# ADR 0108: Named Union Type Aliases (`type` Declarations)

## Status
Proposed (2026-07-15)

## Context

### Problem statement

Beamtalk can express anonymous structural unions in any type position —
`Integer | String`, `String | Nil`, and (since BT-2627) closed singleton
unions like `#temporary | #transient | #permanent` — but has **no way to
name one and reuse it**. Every signature that accepts a restart policy must
spell out the full union:

```beamtalk
restart: policy :: #temporary | #transient | #permanent => ...
defaultPolicy -> #temporary | #transient | #permanent => #transient
escalate: from :: #temporary | #transient | #permanent
       to: to :: #temporary | #transient | #permanent => ...
```

This is not hypothetical — it is the *actual* supervision-policy idiom in
the stdlib docs today (`docs/beamtalk-language-features.md` §Union Types
uses exactly this example, and `class supervisionPolicy` returns exactly
this atom set). BT-2618 (tightening over-broad `Symbol` stdlib annotations
to singleton unions) is blocked in practice on this verbosity: repeating a
three-member union across many signatures is error-prone (miss one member
in one place and two "identical" annotations silently diverge), and there
is no single declaration site to hang documentation on or check
exhaustiveness against.

### Current state

- **Anonymous unions are mature and load-bearing.**
  `TypeAnnotation::Union`/`Singleton` (`ast/expression.rs`), parsed by
  `parse_type_annotation`/`parse_single_type_annotation`
  (`source_analysis/parser/declarations.rs`), resolved by
  `resolve_type_annotation` (`type_resolver.rs:106`) into
  `InferredType::Union`/`Known { class_name: "#foo" }` (the
  singleton-as-Symbol convention). The whole set-theoretic algebra of
  ADR 0102 (`intersect`/`difference`, `\`/`&` syntax, `Negation`) operates
  on this structural representation.
- **Exhaustiveness is structural and site-local.** `match:` warns
  (advisory, ADR 0102 §4) and `matchExhaustive:` errors (opt-in, ADR 0106)
  when the scrutinee's *inferred* type is a closed union of `#symbol`
  singletons (`is_closed_singleton_union`) or a closed leaf-class/`Nil`
  union (ADR 0107). "Closed" is recomputed from the structural type at
  every `match:` site; there is no nominal closedness concept anywhere in
  the checker.
- **No alias mechanism exists.** `type` is not a token; no
  `TypeAnnotation` variant names another annotation. The single
  acknowledgment in the codebase is a comment in `type_resolver.rs:95-98`:
  *"A future evolution may thread [a class hierarchy] in for type-alias
  resolution; no current caller needs it."*
- **Codegen is already erased.** Unions and singletons are compile-time
  constructs; singletons are plain Erlang atoms at runtime, and `-spec`
  generation (`spec_codegen.rs`) emits `union(...)` of `atom` literals.
  A `::` annotation inserts no runtime check (confirmed empirically in
  ADR 0107).

### Constraints

- **Gradual typing, warnings-not-errors**
  (`docs/beamtalk-principles.md` Non-Goals, ADR 0025, ADR 0100): a named
  union must not create any new build-failing diagnostic that isn't
  explicitly opted into. The existing advisory/`matchExhaustive:` split is
  the ceiling.
- **Type erasure** (ADR 0068): a `RestartStrategy` value at runtime *is*
  the atom `temporary` — indistinguishable from any other Symbol. No
  design may assume a runtime tag.
- **Set-theoretic algebra compatibility** (ADR 0102): narrowing must keep
  working through a named union. The false branch of
  `policy =:= #temporary` must still be computable — whatever
  `RestartStrategy \ #temporary` means, it must reduce through the
  existing `difference` operator.
- **Compiler is the language service** (principle #12): hover,
  completions, and diagnostics must be able to render the name, not just
  the expansion — otherwise the feature adds a level of indirection with
  no tooling payoff.
- **Hot reload** (ADR 0105): an alias redefined in a live image changes
  the meaning of every annotation referencing it; the re-check story must
  be stated, not assumed.

## Decision

Add **transparent type aliases**, declared with a top-level `type`
declaration:

```beamtalk
/// How a supervised child restarts after exit.
type RestartStrategy = #temporary | #transient | #permanent
```

An alias names an existing `TypeAnnotation` — nothing more. At
annotation-resolution time (`resolve_type_annotation`), a reference to
`RestartStrategy` expands to the structural union it names, so every
downstream system — narrowing, the `\`/`&` algebra, advisory and asserted
exhaustiveness, spec generation — operates on the *exact same*
`InferredType::Union` shape it handles today, unchanged. The alias name is
retained as **display metadata** so hover and diagnostics can render
`RestartStrategy` rather than (or alongside) the expansion.

This is the TypeScript literal-union-alias model and the Erlang
`-type restart() :: temporary | transient | permanent.` model: aliases are
about *naming*, not about *identity*. There is no new kind of type.

### Semantics

- **Transparent (structural), not nominal.** `RestartStrategy` and
  `#temporary | #transient | #permanent` are the same type. A value of
  one is a value of the other; two aliases with the same expansion are
  interchangeable. Assignability, narrowing, and exhaustiveness are
  computed on the expansion.
- **Any annotation can be named**, not just singleton unions:

  ```beamtalk
  type Port = Integer
  type JsonKey = String | Symbol
  type Timeout = Integer | #infinity
  type PublicTag = Symbol \ (#reserved | #internal)
  type JsonValue = Nil | Boolean | Integer | Float | String | List | Dictionary
  ```

  Singleton-union enums are the motivating case, but restricting the RHS
  would be artificial — the declaration names a `TypeAnnotation`, and all
  of these are `TypeAnnotation`s. (Parametric aliases —
  `type Option(T) = T | Nil` — are deliberately deferred; see
  Consequences.)
- **Namespace:** aliases share the single class/protocol namespace
  (ADR 0068 established that protocols and classes share a namespace and
  collisions are structural errors; aliases extend it). Declaring
  `type Printable = ...` when a protocol `Printable` exists is a compile
  error, and vice versa. Note this is *new bidirectional plumbing*, not
  free reuse: the existing collision check
  (`protocol_registry.rs:296-334`) is one-directional
  (protocol-name vs. `hierarchy.has_class`); alias registration needs
  checks against both classes and protocols, and both of those need a
  check against aliases. Registration order for a **batch compile**:
  classes, then protocols (today's order — `ProtocolRegistry::
  register_module` already runs after class-hierarchy extraction), then
  aliases last, so an alias colliding with either a class or a protocol
  always has a fully-formed hierarchy/registry to check against and
  never races a same-file same-name definition of the other two kinds.
  This ordering guarantee does **not** hold for a **live REPL/hot-reload
  session**, where a user can declare `type Foo = ...` and later, in the
  same or a different session turn, redefine `Foo` as a class
  (`Foo subclass: Bar`) — arriving in the opposite order the batch
  sequence assumes. Class and protocol registration must therefore
  *also* query the alias table for collisions in the live path (not just
  alias registration querying classes/protocols), symmetric with the
  batch case — otherwise a live `class Foo` can silently shadow an
  existing `type Foo` alias with no diagnostic. This is a live-session
  requirement on class/protocol registration, not just a new check
  inside alias registration — flagged explicitly for the implementing
  issue so it isn't discovered as a live-only collision bug.
- **Aliases are exported, like classes and protocols.** This is not
  optional: the ADR's flagship consumers are *stdlib* annotations —
  BT-2618's `RestartStrategy` and BT-2827's
  `Json parse: -> Result(JsonValue, JsonError)` are only useful if
  *user* code can see, write, and get hover for `JsonValue`. A
  package-local alias would let the stdlib abbreviate its own internals
  while leaving every public signature as unreadable as today.
  - **Mechanism — the protocol precedent, not the class one.** Aliases
    erase and produce no BEAM module, so ADR 0070's module-keyed
    collision/qualification machinery (`collision_checker.rs`,
    `pkg@Name`) cannot see them. But protocols have the same property
    and already solved it: protocol metadata is extracted from defining
    modules and seeded into the consumer's registry
    (`ProtocolRegistry::extract_protocol_infos` /
    `add_pre_loaded`, BT-2006), with collisions diagnosed at seeding
    time and current-module definitions winning. Alias export follows
    this model exactly: alias declarations are carried in the package's
    compiled metadata and seeded into the consumer's alias table.
  - **ADR 0071's `internal` modifier applies** (`internal type Foo =
    ...`): an internal alias is package-private — usable in `internal`
    signatures, not exported. An exported (public) signature referencing
    an internal alias is a compile error (the consumer could never name
    or resolve it) — the same leakage rule 0071 applies to internal
    classes in public positions.
  - **The leakage check must run on the expansion, not just the
    written name.** Because aliases are transparent, a *public* alias
    can still leak an internal type through substitution —
    `public type Pub = InternalAlias | String` never writes
    `InternalAlias` in a public *signature*, but `Pub`'s expansion
    contains whatever `InternalAlias` expands to, exposing it to
    consumers who can never resolve that name. The declaration-time
    check for a public alias must therefore walk its fully-expanded
    `TypeAnnotation` and reject any internal class, protocol, or alias
    reachable through it — not just check the alias's own
    `internal`/public flag — mirroring how the alias's own expansion is
    already computed eagerly at declaration time (see No recursion,
    below).
  - **Cross-package collisions** (two dependencies each exporting
    `type Id = ...`) are diagnosed at seeding time like pre-loaded
    protocol collisions. `pkg@Name` qualification for aliases is
    deferred: unlike a class collision, an alias collision always has a
    zero-cost workaround — write the expansion, or re-declare a local
    alias under a different name — because the alias *is* its expansion.
- **Single-letter names are reserved — a declaration error.** ADR 0068's
  implicit method-local type parameters make any *single uppercase
  letter* in type position that is not a known class a generic parameter
  (`is_generic_type_param`, `types.rs:1290`; the inference guard at
  `infer_method_local_params` keys on `!hierarchy.has_class(name)`,
  which an alias would not satisfy). A `type T = ...` alias would
  therefore silently shadow — or be silently shadowed by — every generic
  method's `T`/`E`/`R`, depending on resolution order, with no namespace
  check able to see the collision (implicit params are per-method and
  never registered anywhere). The clean resolution is disjointness:
  `type T = ...` is a declaration error ("single-letter type names are
  reserved for type parameters"), making alias names and type-parameter
  names non-overlapping by construction. Resolution order in
  `resolve_type_annotation` is then: `subst` (type params) → alias table
  → nominal class — and the two interesting lookups can never claim the
  same name. No corresponding change is needed in
  `infer_method_local_params`: its `is_generic_type_param` gate
  (`types.rs:1290`) already restricts inference to single-letter
  uppercase names, so a multi-letter alias used as a bare parameter
  annotation can never enter that path regardless — the single-letter
  ban makes the two namespaces disjoint for free, at every call site,
  with no additional guard to add or maintain. Symmetrically, a bare
  unbound single letter on an alias *RHS* (`type Timeout = Integer | T`)
  is a declaration error
  ("unbound type parameter `T`; parametric aliases are not yet
  supported") rather than a phantom class named `T`.
- **No recursion.** An alias may reference other aliases; a reference
  cycle (`type A = B`, `type B = A | Integer`) — including
  self-reference — is a structural compile error at declaration time.
  This is a real divergence from Erlang, whose `-type json() :: null |
  boolean() | number() | binary() | [json()] | #{binary() => json()}.`
  is legal: recursion is incompatible with *eager* expansion
  (a self-referencing alias has no finite `InferredType` tree), and
  supporting it would mean lazy/deferred alias resolution throughout the
  checker — a different, much larger design. Concretely for the stdlib:
  `type JsonValue = Nil | Boolean | Integer | Float | String | List |
  Dictionary` is expressible (unparameterized containers — which is all
  the element-type precision erasure-checked containers give at a
  dynamic boundary anyway), while the fully-recursive
  `List(JsonValue)` form is not, v1. Flagged as future work alongside
  parametric aliases — both point at the same lazy-substitution
  machinery. Because hot reload can invalidate the
  batch-time check (declare `type A = Integer` and `type B = A`, both
  legal, then live-redefine `type A = B` — a cycle the one-shot
  declaration check already cleared), cycle detection re-runs over the
  alias dependency graph on every live alias (re)definition, and
  expansion itself carries a visited-set guard so a cycle that slips
  through is a diagnostic, never a hang. A rejected redefinition (cycle
  detected, or any other declaration-time error) leaves the alias table
  unchanged — the previous binding, if any, stays in effect, so a failed
  live edit cannot leave dependent annotations resolving against a
  missing alias.
- **Doc comments attach.** `///` above a `type` declaration flows to
  hover and `:help`, giving the member set a single documentation site —
  one of the two main motivations.

### Exhaustiveness — inherited, not new

Because the alias resolves to a closed singleton union, both existing
checks apply with **zero new checker machinery**:

```beamtalk
type Direction = #north | #south | #east | #west

heading :: Direction := readHeading

heading match: [
  #north -> 0;
  #south -> 180;
  #east  -> 90
]
// ⚠ Warning: non-exhaustive match: `#west` is not handled
//   (residual type: `#west`)

heading matchExhaustive: [
  #north -> 0;
  #south -> 180;
  #east  -> 90
]
// ⛔ Error: non-exhaustive matchExhaustive: `#west` is not handled
//   (residual type: `#west`)
```

Adding a member to the alias declaration makes every
`matchExhaustive:` over it non-exhaustive *in one edit* — the single
biggest ergonomic win, and the reason BT-2628 was filed. The severity
policy is exactly ADR 0102 §4 / ADR 0106's, untouched.

### Narrowing and the set-theoretic algebra

Expansion happens before the algebra runs, so ADR 0102's operators need no
changes:

```beamtalk
policy :: RestartStrategy := readPolicy
policy =:= #temporary ifTrue: [ ... ] ifFalse: [
  // policy :: #transient | #permanent  — difference computed on the expansion
]
```

**Display through normalisation — scoped honestly.** The alias name is
shown when the type at hand is *structurally identical* to the alias
expansion (hover on `policy` at its declaration shows
`RestartStrategy (#temporary | #transient | #permanent)`). Residuals are
**not** guaranteed to carry the name: ADR 0102's
`difference`/`intersect`/`union_of` are normalising functions that build
fresh `Union`/`Negation` values, flattening and deduplicating members —
nothing in that algebra propagates a display name, and on dedup between
two aliases sharing a member there is no principled owner. So the false
branch above hovers as the structural `#transient | #permanent`, v1.
Propagating an `(from RestartStrategy)` breadcrumb through the operators
is a compatible later enhancement, not a v1 commitment — promising it
would mean re-plumbing every 0102 operator for a cosmetic gain.

### Surface syntax: `type Name = ...`

Two candidate spellings were seriously considered (see Steelman Analysis
for the full argument):

```beamtalk
// Chosen — dedicated declaration form
type RestartStrategy = #temporary | #transient | #permanent

// Rejected — keyword-message style, following `Protocol define:`
Type define: RestartStrategy as: #temporary | #transient | #permanent
```

The `type` form is chosen because:

1. **An alias has no message surface.** Classes and protocols are
   runtime-reflective objects — `Counter methods`, `Printable
   requiredMethods` — so declaring them with a message-send-shaped form
   (`subclass:`, `define:`) is an honest affordance. A transparent alias
   *vanishes at resolution*; it is never a receiver, and dressing it as a
   message send to a pseudo-class `Type` (which is not a class) would be a
   false affordance.
2. **Every reference language spells aliases as a dedicated form.**
   Erlang `-type restart() :: ... .`, Gleam `type X = ...` /
   `pub type X { ... }`, TypeScript `type X = ...`, Elixir `@type`. A
   newcomer from any of them guesses this syntax cold.
3. **One-liner ergonomics are the point.** The feature exists to make
   naming a union *cheap*; a three-keyword message form works against the
   grain of its own motivation.
4. **Parsing is contained.** `type` becomes a contextual keyword only in
   declaration position (start of a top-level statement, followed by an
   uppercase identifier and `=`). It remains a legal identifier
   everywhere else — no reserved word is added, and no existing code can
   break. Precedent: `=>`, `::`, `->`, and `^` already establish that
   Beamtalk uses non-message syntax where declarations, not messages, are
   being written (`docs/beamtalk-syntax-rationale.md` §Class Definition:
   Message Send → Syntax makes exactly this argument for `subclass:`
   itself being parsed syntax).

### REPL session

```
> type Direction = #north | #south | #east | #west
=> Direction
> d :: Direction := #north
=> #north
> d matchExhaustive: [#north -> 0; #south -> 180; #east -> 90]
⛔ Error: non-exhaustive matchExhaustive: `#west` is not handled
   (residual type: `#west`)
> d matchExhaustive: [#north -> 0; #south -> 180; #east -> 90; #west -> 270]
=> 0
```

**"What does this alias map to?" — `:help <Alias>`.** The existing
`:help <Name>` command (which already renders class and protocol docs) is
extended to alias names, showing the declaration, expansion, and doc
comment:

```
> :help RestartStrategy
type RestartStrategy = #temporary | #transient | #permanent

  How a supervised child restarts after exit.

Declared in: stdlib/src/Supervisor.bt:12
```

For an alias with no doc comment, the block below the declaration line
is omitted entirely — just `type Name = ...` followed by the
`Declared in:` line — matching how `:help` already renders an
undocumented class or method (declaration only, no blank comment
section).

A Haskell-style `:t <expr>` (type-of-expression) command is **not** part
of this ADR: `:t` is already taken in the Beamtalk REPL as the alias for
`:test`, and ADR 0102 explicitly deferred a `:type`-style command as its
own surface-parity decision (`docs/development/surface-parity.md`) —
that deferral stands. Alias *introspection* (`:help`) is in scope here
because the declaration is the feature; expression-level type queries
remain the separate, broader decision.

### Error examples

```beamtalk
// Unknown alias — the existing unresolved-class diagnostic (ADR 0100)
restart: policy :: RestartStrateg => ...
// ⛔ Error: unknown type `RestartStrateg` (did you mean `RestartStrategy`?)

// Collision with an existing class/protocol name
type String = Symbol
// ⛔ Error: `String` is already defined as a class

// Reference cycle
type Ab = Bc | Integer
type Bc = Ab | Symbol
// ⛔ Error: type alias cycle: `Ab` → `Bc` → `Ab`

// Single-letter name — reserved for type parameters (ADR 0068)
type T = Integer | Nil
// ⛔ Error: single-letter type names are reserved for type parameters;
//    choose a longer name (e.g. `type OptionalInt = Integer | Nil`)

// Unbound type variable on the RHS — parametric aliases not yet supported
type Timeout = Integer | T
// ⛔ Error: unbound type parameter `T` in type alias `Timeout`;
//    parametric aliases (`type Name(T) = ...`) are not yet supported

// Value outside the named union — existing union-membership diagnostics,
// now phrased with the alias name
p :: RestartStrategy := #premanent
// ⚠ Warning: `#premanent` is not a member of `RestartStrategy`
//   (#temporary | #transient | #permanent) — did you mean `#permanent`?
```

## Prior Art

| Language | Approach | What we take / leave |
|---|---|---|
| **Erlang/Elixir typespecs** (`-type` / `@type`) | Transparent aliases over structural types; `-type restart() :: temporary \| transient \| permanent.` is *literally this feature* for the same underlying atom sets. Dialyzer expands aliases before analysis. | **Adopt wholesale as the semantic model** — Beamtalk singletons are Erlang atoms, and this is how the BEAM ecosystem has always named atom sets. Confirms transparent-alias is the BEAM-native answer. We also gain what typespecs have: a named `-type` can be emitted in generated specs for FFI readability. |
| **TypeScript** (literal-union type aliases) | `type Direction = 'n' \| 's' \| 'e' \| 'w'` — transparent, structural, erased; tooling displays the alias name; exhaustiveness via narrowing + `satisfies never`. | **Adopt** the alias-with-display-name model and the composition with an opt-in exhaustiveness assertion (Beamtalk already has `matchExhaustive:`, our analogue of `satisfies never`, ADR 0106). TS proves at scale that named literal unions cover the enum use case without nominal enums. |
| **Rust / Swift enums** | Nominal, closed sum types with distinct identity; exhaustive `match` by default; variants can carry payloads. | **Leave.** Their model requires runtime identity (discriminant tags) and default-strict exhaustiveness — both incompatible with type erasure (the value *is* the atom) and with gradual, advisory-by-default checking (ADR 0100). Payload-carrying variants are already served by `sealed Value subclass:` + constructor patterns (ADR 0060/0107). |
| **Gleam** | Nominal custom types (`type Season { Spring Summer }`) — closed, exhaustive by construction; also has transparent `type` aliases for naming. | **Split the difference knowingly:** Gleam's *custom types* are the nominal road we decline (its exhaustiveness comes from nominal closedness; ours comes from the ADR 0102 structural algebra). Gleam's *aliases* — `type Headers = List(#(String, String))` — are exactly this ADR. |
| **Smalltalk / Newspeak** | No static types, no aliases; atom-set idioms are documented in comments and method names. | No prior art to preserve — like ADR 0102, this is purely additive edit-time tooling with zero runtime footprint. |

## User Impact

- **Newcomer:** `type RestartStrategy = ...` reads identically to
  TypeScript/Gleam; the follow-on behaviours (hover shows the name,
  `matchExhaustive:` names the missing member) are discoverable through
  normal use. One new top-level form to learn, spelled the way every
  mainstream language spells it.
- **Smalltalk developer:** zero runtime change — no new objects, no new
  messages, `#temporary` is still just a Symbol. The alias is opt-in
  annotation vocabulary; un-annotated code never sees it. The one genuine
  departure (a non-message top-level form) is confined to type-land,
  which is already the acknowledged pragmatic-departure zone (`::`,
  `->`, `\`, `&`).
- **Erlang/BEAM developer:** this is `-type` for Beamtalk — the most
  familiar feature in the ADR. Generated specs can emit the named
  `-type`, making the FFI boundary *more* idiomatic, not less.
- **Production operator:** no codegen, runtime, or hot-reload *mechanism*
  change; compile/edit-time only. One new hot-reload consideration:
  redefining an alias in a live image re-checks annotations that
  reference it (same ADR 0105 machinery that already handles class-shape
  changes — see Implementation).
- **Tooling/LSP developer:** the alias registry is one more name table in
  the compiler-as-language-service; hover/completions gain a
  higher-signal display name; go-to-definition on a type annotation gets
  a real target. The display-name provenance is the only genuinely new
  rendering machinery.

## Steelman Analysis

### Chosen: transparent alias, `type Name = ...` syntax

- 🧑‍💻 **Newcomer:** "I'd write this syntax without reading a single doc —
  it's TypeScript's and Gleam's spelling for the same feature."
- 🎩 **Smalltalk purist:** "Of the shapes on offer, this is the most
  honest: it doesn't pretend an alias is an object. A fake message send
  to a fake `Type` class would be the *less* Smalltalk-true option."
- ⚙️ **BEAM veteran:** "It is `-type`, semantically and in generated
  specs. Atom sets with names are how OTP has documented its own APIs for
  thirty years."
- 🏭 **Operator:** "Compile-time only; generated code identical; nothing
  new to observe in prod."
- 🎨 **Language designer:** "One expansion step at resolution time buys
  reuse of narrowing, the 0102 algebra, both exhaustiveness checks, and
  spec codegen — the highest behaviour-to-machinery ratio available."

### Rejected A: nominal enum (distinct identity, opaque)

- 🎨 **Language designer:** "A real closed type is the principled answer:
  `RestartStrategy` *means* something the structural union doesn't —
  intent. Error messages and hover get the name with no provenance
  bolt-on, and exhaustiveness is definitionally sound rather than
  inference-dependent."
- 🧑‍💻 **Newcomer (from Rust/Swift):** "Enums are the feature I asked for
  by name."
- **Why not chosen:** nominal identity fights every adjacent system.
  Type erasure means the value is the atom `temporary` — two nominal
  enums sharing a member (`type A = #x | #y`, `type B = #x | #z`) cannot
  be told apart at runtime, so nominal identity would be a compile-time
  fiction with FFI holes (what does an Erlang function returning
  `temporary` produce?). The ADR 0102 algebra must break the nominal
  wrapper open the moment narrowing runs (`RestartStrategy \ #temporary`
  has no answer *except* the structural residual), so opacity cannot
  survive first contact with the feature's own motivating use case.
  Assignability needs new bidirectional rules (`#temporary` ↔
  `RestartStrategy`). Effort L–XL, and nearly all of the delta over the
  alias buys behaviour the structural machinery already provides. The one
  real advantage — the name in diagnostics — is recovered by display
  provenance at a fraction of the cost.

### Rejected B: keyword-message declaration syntax (`Type define: X as: ...`)

- 🎩 **Smalltalk purist:** "One grammar shape for every declaration —
  `subclass:`, `Protocol define:`, and this. No new keyword, and it's
  discoverable by analogy with what's already in the language."
- **Why not chosen:** the analogy is misleading, not supporting. Classes
  and protocols are runtime-reflective *objects*; `Protocol define:`
  declares something you can later send `requiredMethods` to. An alias is
  not an object, never a receiver, and erases at resolution — a
  message-send costume on it is a false affordance, and `Type` would be a
  pseudo-receiver that is not a class (unlike `Protocol`, which names a
  real registry surface). It is also 3× the tokens for a feature whose
  entire purpose is making names cheap. The purist's uniformity argument
  is real but is outweighed by honesty-of-form — the same reasoning the
  syntax-rationale doc already applies to `subclass:` (parsed syntax that
  merely *looks* like a send).

### Rejected C: do nothing (keep anonymous unions only)

- 🎩 **Smalltalk purist:** "Every type-system addition needs to earn its
  place; Smalltalk needs none of this."
- 🏭 **Operator:** "The zero-risk option; nothing can regress."
- **Why not chosen:** the status quo has a concrete, filed cost: BT-2618
  is blocked on annotation verbosity, the stdlib's own supervision API
  documents a three-member union with no single source of truth, and
  divergent copies of "the same" union are a real failure mode (miss one
  member in one signature and exhaustiveness silently means different
  things at different sites). The feature is small (M) precisely because
  it reuses everything; doing nothing saves less than it costs.

### Tension points

- Newcomers from Rust/Swift genuinely want Option A's *word* (`enum`) and
  closed-by-default checking; BEAM veterans and operators are firmly
  against nominal identity that the runtime cannot honour. The alias +
  `matchExhaustive:` combination gives the Rust newcomer the *behaviour*
  they want (one-edit propagation to every match site) without the
  identity the BEAM cannot provide — behaviour over branding.
- Purists split between B (uniform grammar) and C (nothing); neither
  prefers the chosen form, but B's own steelman collapses on inspection
  (the uniformity it claims is with constructs that are objects, which
  this isn't), and C concedes the filed problem. The chosen option is
  every *other* cohort's first choice.

## Alternatives Considered

See Steelman Analysis above — nominal enums (A), keyword-message
declaration syntax (B), and status quo (C) were all considered and
rejected in favour of transparent aliases with a `type` declaration, for
the reasons given there. Two further variants were examined and dropped
briefly:

### Hybrid: transparent assignability, nominal diagnostics/exhaustiveness

The genuine midpoint between the alias and the nominal enum: keep
structural assignability and narrowing (so FFI and the ADR 0102 algebra
are untouched), but track alias identity as a first-class fact for
diagnostics, hover, and exhaustiveness — closer to how Elm or OCaml
render aliases, and a road that could later support `Meters`/`Feet`
branding as an opt-in. This deserves naming because the chosen design
already contains its seed: display-name provenance *is*
nominal-for-display. The distinction is degree — the hybrid makes the
name definitional (every operator must preserve and reason about it;
exhaustiveness keyed on "the alias's member set" rather than the
structural expansion), where the chosen design makes it cosmetic
(operators stay name-blind; v1 display is scoped to
structurally-identical types). Not chosen because the definitional
version re-opens exactly the costs of Rejected A at the operator level —
every `difference`/`intersect`/`union_of` call must decide what happens
to the name, dedup between overlapping aliases needs an ownership rule,
and exhaustiveness gains nothing (`matchExhaustive:` over the expansion
is already the full guarantee; a name-keyed check would be the same
computation with extra bookkeeping). If branded types are ever wanted,
they should be their own ADR with this section as the starting point —
the alias grammar and registry built here are forward-compatible with
that extension.

### Class-shaped declaration (`sealed Union subclass: RestartStrategy`)

Modelling the enum as a class-family (one class per member, or a sealed
class with singleton instances) — how a pure Smalltalk would do it.
Rejected: the values must remain plain atoms for FFI (`supervisionPolicy`
returns `#permanent` today and Erlang supervisors consume the atom);
wrapping them in instances breaks every existing call site and the FFI
boundary. Payload-carrying sum types are already served by
`sealed Value subclass:` + constructor patterns (ADR 0060/0107) — that
lane exists and is not this feature.

### New declaration operator (`RestartStrategy ::= ...`)

A dedicated top-level `::=` binding form. Rejected: invents a novel token
for no expressive gain over `type ... =`, with no precedent in any
reference language a user might arrive from.

## Consequences

### Positive

- Closed atom sets get a single declaration site: named, documented
  (`///`), and reusable across signatures — directly unblocks BT-2618 and
  BT-2827-style API typing.
- `matchExhaustive:` over a named union tracks the declaration: adding a
  member breaks every asserted match in one edit. Advisory `match:`
  warnings likewise. Zero new exhaustiveness machinery.
- Narrowing, `\`/`&`, spec generation, and hover all work by
  construction, because resolution produces the structural types they
  already consume.
- Generated Dialyzer specs can emit a named `-type` per alias, making the
  FFI boundary more idiomatic for Erlang/Elixir consumers — **for alias
  RHSs with a faithful Erlang spec form** (unions of singletons, classes,
  generics). A `\`/`&` alias RHS has no exact spec form (ADR 0102;
  `spec_codegen.rs:86-95` already widens `Difference` to its base and
  `Intersection` to its left operand), so its `-type` is emitted widened,
  with the exclusion lost — same over-approximation the anonymous form
  already has today, but worth knowing before pointing Erlang tooling at
  a named `public_tag()`.
- Aliases generalise past enums for free (`type Timeout = Integer |
  #infinity`), because the RHS is any `TypeAnnotation`.

### Negative

- A new top-level declaration form — the first that is not
  keyword-message-shaped — is one more thing to teach, and a real (if
  bounded) departure for purists.
- Transparent semantics mean two aliases with the same expansion are
  interchangeable; users wanting `Meters`/`Feet`-style *branded* types
  will find this feature deliberately does not provide them (and the ADR
  should be cited when that request arrives).
- Display-name provenance is genuinely new machinery and is this
  design's real cost centre, not a bolt-on: `InferredType`'s
  `TypeProvenance` today carries spans only (`Declared|Inferred|
  Substituted(Span)`), no name — the alias name needs new storage, and
  the Decision section deliberately scopes v1 display to
  structurally-identical types because the ADR 0102 operators do not
  propagate names. Done lazily this degrades to "expansion everywhere",
  which would gut the readability motivation — it is the
  implementation-quality risk to watch.
- Alias redefinition on hot reload must re-check dependent annotations —
  and this is **new plumbing, not reuse**. ADR 0107 learned exactly this
  lesson (its BT-2856 resolution): ADR 0105's re-check graph is purely
  selector-keyed or state/field-shape-keyed, and a new dependency kind
  (there, "sites that tested this class for leafness"; here,
  "annotations that reference this alias") needs its own trigger.
  0107's `trigger_leaf_change/1` had no lookup key and swept every live
  class; an alias trigger is better placed — the alias *name* is a
  natural key for an alias-name → dependent-annotation-site edge — but
  that edge must be recorded at resolution time, which is new work.
  Without it, a live image can hold `matchExhaustive:` proofs computed
  against a stale member set.

### Neutral

- No runtime, codegen-semantics, or REPL-output change; erasure is
  total. The named `-type` emission is spec cosmetics only.
- **Parametric aliases (`type Option(T) = T | Nil`) and recursive
  aliases (`type JsonValue = ... | List(JsonValue)`) are deliberately
  deferred**, not rejected: parametric aliases require substitution
  machinery at alias expansion (a second instantiation path beside
  ADR 0068's class generics), and recursive aliases require lazy
  resolution (see Semantics — eager expansion cannot represent them).
  They are one deferral, not two: both need the same shift from
  eager-expand-at-resolution to substitute-on-demand, so a future ADR
  should take them together. The declaration grammar deliberately leaves
  room (`type Name(...) = ...` is a parse error today, reserved).
- Runtime reflection of aliases (`Beamtalk aliases`, alias objects) is a
  non-goal for v1: aliases live in the compiler-as-language-service
  (hover, `:help`, completions cover discoverability). If reflection
  demand materialises, an alias registry in `__beamtalk_meta`-style
  metadata is a compatible extension.
- **Sendability (ADR 0103) sees the expansion.** An alias in a
  message-type position reduces to its RHS before any sendability
  classification runs, so aliases add no new case to 0103's class-kind
  keying. One pre-existing open edge is made more *reachable* (not
  created) by alias generality: 0103 has no stated rule for
  `Negation`/`Intersection` types, and `type Msg = Symbol \ #internal`
  makes such a type easier to put in a send position — flagged here for
  ADR 0103's implementation rather than resolved by this one.

## Implementation

To be broken into an epic via `/plan-adr` once Accepted. Expected shape:

- **Lexer/Parser:** contextual `type` keyword at top-level declaration
  position (`type` + uppercase identifier + `=` + type annotation);
  produces a new `TypeAliasDefinition` AST node (name, annotation, doc
  comment, span) alongside `ClassDefinition`/`ProtocolDefinition` in
  `ast/class.rs` + `source_analysis/parser/declarations.rs`. `type`
  stays a valid identifier everywhere else.
- **Unparse:** round-trip the declaration
  (`unparse/mod.rs`).
- **Semantic analysis:** alias table threaded into
  `resolve_type_annotation` (`type_resolver.rs` — the hook its own
  comment anticipates). Resolution order: `subst` (type params) → alias
  table → nominal class; disjoint by construction given the
  single-letter ban. Declaration-time checks: single-letter name error,
  unbound-type-variable-on-RHS error, cycle detection (re-run on live
  redefinition; expansion carries a visited-set guard). Bidirectional
  namespace collision checks against classes *and* protocols (new
  plumbing — the existing `protocol_registry.rs` check is
  one-directional). `infer_method_local_params`
  (`inference.rs:5497-5506`) needs **no** change — its existing
  `is_generic_type_param` gate is single-letter-only, so multi-letter
  alias names are already excluded from that path by construction (see
  Semantics). Provenance:
  resolved `InferredType` carries the alias name for display (extend
  `TypeProvenance` or add a display-name field — design detail for the
  implementing issue); v1 display scope is structurally-identical types
  only, per the Decision section.
- **Diagnostics/hover:** render alias name with expansion on hover
  (`hover_provider.rs`); membership/exhaustiveness diagnostics name the
  alias where the annotation was written, concrete members in residuals.
- **Exhaustiveness:** no changes —
  `check_singleton_match_exhaustiveness` / `matchExhaustive:` operate on
  the expansion by construction. Regression tests pin that an
  alias-annotated scrutinee behaves identically to the spelled-out union.
- **Export/packaging:** alias declarations carried in the package's
  compiled metadata and seeded into consuming compilations' alias
  tables, mirroring `ProtocolRegistry::extract_protocol_infos` /
  `add_pre_loaded` (BT-2006): current-module wins, cross-package
  collisions diagnosed at seeding time. `internal type` support plus the
  leakage check — run on the *expanded* `TypeAnnotation` of every public
  alias and public signature, not just the written name, so a public
  alias cannot smuggle an internal class/protocol/alias through
  transparent substitution (see Semantics).
- **Hot reload (live-session namespace ordering):** class and protocol
  registration in the live/REPL path also query the alias table for
  collisions, symmetric with alias registration querying classes and
  protocols — the batch registration order (classes → protocols →
  aliases) does not bound the order live redefinitions can arrive in
  (see Semantics).
- **Codegen:** optional named `-type` emission per alias in
  `spec_codegen.rs`; annotation sites reference it. No other codegen
  change.
- **Hot reload:** a new alias-name → dependent-annotation-site re-check
  trigger. ADR 0105's existing graphs (selector-keyed xref,
  state/field-shape) do not cover this dependency kind — same situation
  ADR 0107 hit for leaf-status changes (BT-2856). Unlike 0107's
  `trigger_leaf_change/1` (no lookup key, sweeps all live classes), the
  alias name is a natural key: record alias-name → annotation-site edges
  at resolution time and re-check only dependents. Cycle detection
  re-runs on every live alias redefinition (see Semantics).
- **System Browser / LiveView IDE (ADR 0096, ADR 0046):** aliases need no
  new creation UI — a `type` declaration is ordinary top-level source
  text, created through the existing Editor pane (`load-source`) exactly
  like a class or protocol definition. Display is a new sibling category,
  not a new sidebar: `browse-classes` (ADR 0096) gets a sibling
  `browse-type-aliases` op returning `{value, [AliasRow]}` rows
  (`name`, `expansion`, `doc`, `source_file`, `internal`), and the System
  Browser tree gains "Type Aliases" alongside its existing Classes/
  Protocols panes; the VS Code Workspace Explorer sidebar (ADR 0046)
  gains a matching "Type Aliases (N)" section beside its "Classes (N
  loaded)" section. Both are read-only listings — Observer-role-safe
  (ADR 0091 Decision 4: no user code triggered), same as `browse-classes`.
  Update `docs/development/surface-parity.md`'s browse-op table with the
  new op alongside `browse-classes`/`browse-protocols`.
- **LSP/REPL parity:** completions for alias names in type position.
  **Go-to-definition works by construction**: `TypeAliasDefinition` is a
  real AST node with a span, and every use site resolves through the
  alias table, so definition/references queries have a concrete target —
  go-to-definition on `RestartStrategy` in any annotation position jumps
  to its `type` declaration, and find-references enumerates annotation
  sites (the same resolution-time alias-name → annotation-site edges the
  hot-reload trigger records; find-references coverage is scoped to
  files compiled in the current build graph — uncompiled or
  never-opened files contribute no edges until compiled). A silent
  partial result is a real hazard here — a user trusting an empty or
  short reference list could delete an alias that is in fact used by an
  uncompiled file. The LSP implementation must surface the
  incompleteness rather than let a partial list read as exhaustive
  (e.g. a `window/showMessage` warning when the workspace has
  uncompiled files, mirroring how other incremental-analysis LSPs flag
  partial results); the exact mechanism is an implementing-issue design
  detail, but silence is not an acceptable default. Hover on the
  alias name shows the expansion. REPL: `:help <Alias>` renders declaration + expansion + doc
  comment (see REPL session above); `type` declarations accepted in REPL
  input; no `:t`/`:type` expression-type command (deferred per ADR 0102 —
  and `:t` is already the `:test` alias). Update
  `docs/development/surface-parity.md` (`:help` alias support is a
  cross-surface operation: CLI REPL, MCP, LSP hover must agree).
- **Docs:** `docs/beamtalk-language-features.md` (new §Type Aliases under
  the type-system material), `docs/beamtalk-syntax-rationale.md` (the
  `type`-form-vs-message-form argument, recorded).
- **Stdlib follow-through:** BT-2618 retargets its tightened annotations
  at named aliases (`type RestartStrategy`, etc.) instead of repeated
  anonymous unions, and BT-2827's typed JSON/YAML/HTTP APIs get
  `type JsonValue` (unparameterized-container form — see the recursion
  note under Semantics) as the exported vocabulary for
  `Json parse: -> Result(JsonValue, JsonError)`-style signatures. Both
  exercise the export path, making stdlib the first consumer of
  alias-metadata seeding.

## References

- Related issues: BT-2628 (this ADR's driver), BT-2618 (stdlib annotation
  tightening — primary consumer), BT-2627 (singleton annotations in type
  position — shipped prerequisite), BT-2624 (singleton-in-union checking),
  BT-2827 (typed JSON/YAML APIs — blocked on this)
- Related ADRs: ADR 0068 (unions/singletons/generics; the namespace and
  `TypeAnnotation` machinery this extends), ADR 0102 (set-theoretic
  algebra and advisory exhaustiveness the alias expands into), ADR 0106
  (`matchExhaustive:` — the assertion this composes with), ADR 0107
  (leaf-type exhaustiveness + the hot-reload staleness precedent),
  ADR 0025/0100 (gradual typing / diagnostic severity policy), ADR 0053
  (`::` annotation syntax), ADR 0060 (Result — the payload-carrying
  sum-type lane this deliberately does not duplicate), ADR 0105 (live
  re-check machinery the alias trigger extends with a new dependency
  kind), ADR 0070/0071 (package namespaces and visibility — alias
  export follows the protocol-metadata seeding model, and `internal`
  applies to aliases), ADR 0103 (sendability — sees the alias
  expansion; `Negation`/`Intersection` edge flagged there), ADR 0096
  (System Browser browse-op precedent `browse-type-aliases` follows),
  ADR 0046 (VS Code Workspace Explorer sidebar — matching "Type
  Aliases" section)
- Documentation: `docs/beamtalk-language-features.md` §Union Types,
  §Difference and Intersection Types; `docs/beamtalk-principles.md`
  (Non-Goals — mandatory static typing);
  `docs/beamtalk-syntax-rationale.md` §Class Definition: Message Send →
  Syntax
- External: [Erlang `-type` declarations](https://www.erlang.org/doc/system/typespec.html);
  [TypeScript type aliases + literal unions](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#type-aliases);
  [Gleam type aliases](https://tour.gleam.run/data-types/type-aliases/)
