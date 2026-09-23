# ADR 0127: Traits — Protocols with Provided Methods, Flattened into Users

## Status
Accepted (2026-09-23)

Closes the question ADR 0005 left open as Q9 ("Mixins/traits: deferred to a
future ADR") and BT-274 (cancelled 2026-04-01 as "not pressing"; reopened by
BT-3526 with the corpus evidence below).

The decision in one paragraph: a `Protocol define:` body may now contain
*provided* methods (a signature with `=>` and a body) beside its *required*
signatures (no `=>`). There is no separate keyword; in this ADR a **trait**
is simply a protocol with at least one provided method. A class receives a
protocol's provided methods by naming it in a `uses:` line at the top of its
body. Composition is
**flattened at compile time**: the using class is compiled exactly as if the
provided methods were written in its own body, so `ThreadedIr`, the class-kind
calling convention, `super`, sealed self-send optimisation, xref, and hot
reload all see ordinary methods. A class's own method beats a trait's, and a
trait's beats an inherited one **only when the using class says so** with an
`overriding:` clause (§3a), so no provision silently replaces an inherited
method in a closed-world build, from either side of a release. Two traits providing the same
selector is a compile error unless the class defines it or excludes one. A
protocol's type is its required ∪ provided selectors, so the structural
conformance rule of ADR 0068 is unchanged for protocols without provisions,
and `uses:` never becomes a nominal `implements:`.

**Decisions taken after review** (each records the alternative and where it
is written up):

1. ~~**Now, or inheritance first?**~~ **Decided (2026-09-23): traits now.**
   `Magnitude` and an abstract enumerable class would remove most of the
   measured duplication, but they model *capabilities* as *kinds*
   (§Context, "Kinds versus capabilities"). The stdlib is not refactored
   into an inheritance-based interim first; Option H is rejected.
2. ~~**A protocol's type: required only, or required ∪ provided?**~~ **Decided (2026-09-23):
   required ∪ provided** (§8). The required-only alternative (Option B′)
   borrows Java 8's rule, which works only because Java is nominal: every
   implementer inherits the defaults. Under structural conformance a class
   can conform without `uses:` and so without the provisions, and a
   required-only type would let it through to code that then sends
   `max:` and fails with `does_not_understand` at run time. Required ∪
   provided reports that case as a conformance warning instead. Pony is
   the precedent: its structural `interface` may carry default bodies,
   includes them in the type, and delivers them only to types that declare
   `is I`.
3. ~~**One keyword or two.**~~ **Decided (2026-09-23): one keyword.**
   `Protocol define:` may carry method bodies; there is no `Trait define:`.
   With decision 2 settled, a second keyword would only signal whether a
   file has bodies, and adding a protocol's first body would force a
   keyword change for no semantic gain.
4. ~~**v1 scope.**~~ **Decided (2026-09-23)** (§Implementation, "v1
   scope"). v1 ships everything the semantics need plus live patching and
   the users index, because principle 11 ("Live patching is a message
   send") and principle 8 ("Reflection as Primitive") make them part of the
   feature, and the in-image reload fan-out needs the users index to find
   its targets. Deferred past v1: `aliasing:`, a protocol using another
   protocol (§Status 8), class-side provisions,
   browse grouping by trait, protocol-wide rename, and LSP hover
   provenance.

Decided after the second review (2026-09-23):

5. **`overriding:` exempts only `printString` and `displayString`** (§3a).
   `equals:` and `hash` need an explicit `overriding:`, because silently
   replacing a value's equality changes how its instances behave in sets
   and dictionaries.
6. **`uses:` of a protocol with no provisions is a hint** (§1), not a
   warning, so a protocol gaining or losing its last body never fails a
   `--warnings-as-errors` build.
7. **`Comparable` requires `<`**, not a three-way `compare:` (§1, §9): it is
   Pharo's `Magnitude` contract, every intended user already has it, and a
   three-way protocol can be added separately later.
8. **A protocol `uses:`-ing another protocol is post-v1** (Phase 7). No
   stdlib protocol needs it, and cutting it removes transitive expansion,
   cycle detection, protocol-internal precedence and same-origin diamonds
   from v1. `extending:` still composes types.
9. **The stdlib-protocol patch refusal** reads "Cannot recompile 'max:' on
   stdlib protocol 'Comparable': built-in protocols are read-only in the
   workspace", a literal mirror of the existing stdlib-class message
   (`stdlib_method_read_only_error/2`, `beamtalk_repl_eval.erl`; §11).

All decisions are resolved; accepted 2026-09-23.

## Context

### Problem statement

Beamtalk has three ways to share behaviour, and none of them can put an
*implemented* method on two unrelated classes:

| Mechanism | Shares | Cannot |
|---|---|---|
| Single inheritance (ADR 0005, 0006) | Bodies, down one chain | Cross the class-kind wall (ADR 0067): an `Actor` can never inherit from `Collection`, which is a `Value` |
| Structural protocols (ADR 0068) | A *contract* (selector set) | Carry a body — "Protocol bodies use class-body style — method signatures without `=>` implementations" |
| Extension methods (ADR 0066) | A body, onto **one** named class | Be reused: `Comparable+DateTime.bt` and `Comparable+Duration.bt` are two copies; extensions "cannot add instance variables" and cannot *require* anything of the target |

The gap is the one Schärli et al. identified in Smalltalk-80 twenty years
ago: with single inheritance, behaviour that cuts across the hierarchy is
either duplicated or hoisted into an over-general superclass. Beamtalk's
class kinds make the wall higher than Smalltalk's, because the natural home
for shared collection behaviour (`Collection`) is a `Value`, and an `Actor`
can never be one.

**Evidence from the stdlib** (`stdlib/src/*.bt` at the time of writing):

*Ordering.* Four classes outside the `Number` tower hand-write the four
ordering operators — each operator backed by its own FFI call or
`@primitive`, so replacing three of them with derived bodies would be a
small performance trade, not free — and none of them gets `between:and:`,
`min:` or `max:`, which exist only on `Number` and its subclasses. The
missing derived methods, not the four operators, are the reuse gap:

| Class | `<` `>` `<=` `>=` | `between:and:` / `min:` / `max:` |
|---|---|---|
| `DateTime` (`date_time.bt:313-341`) | 4 × `(Erlang beamtalk_datetime) lt:/gt:/lte:/gte:` | none |
| `Duration` (`duration.bt:225-253`) | 4 × `(Erlang beamtalk_duration) …` | none |
| `Uuid` (`uuid.bt:140-149`) | 4 × `(Erlang beamtalk_uuid) …` | none |
| `String` (`string.bt:96-123`) | 4 × `@primitive` | none |
| `Character` (`character.bt:61-85`) | 4 × `@primitive` | inherited — `Character` is an `Integer` subclass (`character.bt:16`) |
| `Integer` / `Float` | 4 × `@primitive` each | `min:`/`max:` **duplicated verbatim** in both (`integer.bt:294-306`, `float.bt:177-188`) — fixable today by moving them up to `Number`, no traits needed; `between:and:` on `Number` only |

*Enumeration.* Two `Value` classes that are not collections re-implement the
`Collection` "shared protocol" by hand because *is-a `Collection`* would be
the wrong statement:

| Selector | `Collection` (`collection.bt`) | `SupervisionTree` (`supervision_tree.bt`) | `ChangeLog` (`change_log.bt`) |
|---|---|---|---|
| `size` | abstract | `:66` | `:63` |
| `isEmpty` | `:68` `self size =:= 0` | `:74` | `:71` `self size =:= 0` |
| `notEmpty` / `isNotEmpty` | `:77` | — | `:81` `self isEmpty not` |
| `do:` | abstract | `:84` | `:93` |
| `select:` | `:269` | `:94` | `:105` |
| `detect:` / `detect:ifNone:` | `:304` / `:327` | `:104` / `:119` | — |
| `collect:` | `:162` | `:128` | — |

`String` (`string.bt:414-423`) also re-implements `isEmpty` / `isNotEmpty`
in terms of `length` rather than inheriting them.

**Most of this stdlib duplication *is* fixable by single inheritance**, and
the ADR should be judged knowing that. `DateTime`, `Duration`, `Uuid` and
`Number` all subclass `Value` directly, so a Pharo-style abstract
`Magnitude` Value class above them would remove the ordering copies for
everything except `String`. `SupervisionTree` and `ChangeLog` also subclass
`Value` directly, so an abstract enumerable Value class (with `Collection`
under it) would remove the enumeration copies. §Alternatives treats this
seriously as Option H, and rejects it on modelling grounds, not on counts.

**Kinds versus capabilities.** Even where inheritance *could* remove the
duplication, it would be modelling the wrong thing. A superclass says what
a class *is*: `Integer` is a `Number`, `Array` is a `Collection`. "Can be
ordered" and "can be enumerated" are not kinds; they are capabilities, and
their natural names are adjectives (`Comparable`, `Enumerable`), not nouns.
Putting `DateTime`, `Duration` and `Uuid` under a `Magnitude` superclass
would spend each class's only superclass slot on a capability, and would
claim a kinship between a timestamp, a length of time and an identifier
that exists only because they share `<`. Smalltalk-80's `Magnitude` is a
class because the language had nowhere else to put shared behaviour, not
because ordering is a kind. Traits give capabilities their own home, and
leave the hierarchy to say what things are.

What inheritance cannot fix at all is the rest of the reason for this ADR:

1. **A class that needs two concerns.** `String` is a `Collection` (via
   `Binary`) *and* is ordered. Under inheritance it can have `Magnitude` or
   `Collection` behaviour, not both; today it re-implements the ordering half
   and `isEmpty`/`isNotEmpty` by hand.
2. **The class-kind wall.** Every shared-behaviour base in the stdlib is a
   `Value`. An `Actor` that wants `do:`/`select:`/`detect:` over what it
   holds (a registry, a pool, a subscription set) can never inherit them.
   The stdlib has no such actor today. The evidence for this case is
   structural, not a count, and it is weaker than it looks: block-taking
   methods that call each other through actor self-sends are broken today
   (BT-3580, §9), and blocks passed to an actor run in the actor's process
   (language guide § Checked boundaries; ADR 0103). Traits whose provisions do not pass blocks through self-sends
   (`Comparable`, `Counting`, snapshot-based `Enumerable`, §9) are unaffected.
3. **A superclass slot already taken.** User classes subclass `TestCase`,
   `Error`, `Supervisor` or an application base actor. Their superclass is
   spent on framework integration, so shared behaviour has nowhere to go
   except copies or extension methods.

The ADR 0037 steelman for "Option C: Traits / Protocol" made the same
argument — "Traits separate the *what* (Enumerable protocol) from the *how*
(implementation per type)" — and rejected it only because protocols did not
yet exist. They do now.

### Current state

**Class definition** (parser doc, `parser/declarations.rs:216-233`;
ADR 0067): `[internal|abstract|sealed|typed]* Superclass subclass: Name`, an
optional header tail (`(T, E)`, `native: mod`, `handleScope:`,
`shapeVersion:`), then an indented body of `state:` / `field:` /
`classState:` declarations and methods. The body loop ends at the first line
that is none of those — an unknown keyword such as `uses: Foo` today
**silently ends the class body** (`declarations.rs:808-809`), which this ADR
must turn into a targeted diagnostic.

**Protocols** (ADR 0068): `Protocol define: Name`, optional `(T)` and
`extending:`, body of signatures. The parser tells a signature from a method
by the absence of `=>`. Conformance is structural, tiered
(`ClassHierarchy` walk → REPL method table → DNU override), and cached in
`beamtalk_protocol_registry`. ADR 0068 Alternative D rejected a nominal
`implements:` clause; ADR 0025 Alternative C rejected the same thing. A
class and a protocol share one namespace.

**Extension methods** (ADR 0066): `Class >> sel => body`. A same-file
"self-extension" is **folded into the host class's AST** before codegen
(`gen_server/extensions.rs:8-11`, `compiler-port/handlers/compile.rs:109-142`
`merge_method`) — that is flattening, and it is the mechanism this ADR
generalises. A foreign extension is a fun in the ETS table
`beamtalk_extensions`, consulted before the hierarchy walk at every level —
that is "shared code + dispatch", the alternative this ADR rejects for
traits (§Alternatives).

**Method representation**. One class per file, one BEAM module per class
(ADR 0040, `module_validator.rs:130-131`); the module name derives from the
sole top-level definition (ADR 0119). Inherited methods are never copied:
actors walk the class registry at run time and value types call the
superclass module statically. A method body is **not class-neutral**: it
bakes in the lexical class name (`super`, error hints), the lexical module
(actor self-sends go to `<module>:safe_dispatch`,
`dispatch_codegen.rs:2019-2028`), the class kind (`{reply, R, State}` clause
vs. plain function), and per-class facts (`late` slots, sealed selectors,
class-var names). `ClassInfo.methods` is a `Vec<MethodInfo>` whose entries
already carry `defined_in` (`class_hierarchy/class_info.rs:50-80`), and
`ClassHierarchy::register_extensions` already appends foreign methods to it.

**Reflection**. `beamtalk_xref_methods` rows carry
`provenance := class_body | extension | class_builder | put_method`
(ADR 0087); `browse-protocols` maps `extension` to a synthetic "extensions"
category. `Behaviour methods` returns local selectors only (ADR 0032).
`renameSelector:to:` (ADR 0114) rewrites self/super sites within the class's
subclass closure. `removeSelector:` (ADR 0112) removes a class's own method.

### Constraints

- **Single dispatch, single inheritance** (ADR 0005 Q8, decided). Traits must
  not introduce a second lookup path or a linearisation order.
- **Structural conformance stays** (ADR 0068). `uses:` may not become the
  `implements:` that ADR 0068/0025 rejected.
- **Three class kinds** (ADR 0067) with three storage families (`State` map,
  `Self` map, none) and two calling conventions. Anything a trait method
  does with state must compile correctly in all three.
- **State threading lowers through `ThreadedIr`** (ADR 0111, 0118, 0120,
  0122). A trait design that needs a new threading mode is a large change; one
  that produces ordinary methods is not.
- **One top-level definition per file** and module-name derivation from it
  (ADR 0040, 0119).
- **Hot reload per class** (ADR 0050, 0105). Editing a trait must reach every
  user without a restart, and must not trigger a state-shape migration
  (ADR 0123) unless a shape actually changed.
- **Honesty of form** (`beamtalk-syntax-rationale.md` § Type Alias):
  `X define:` is reserved for things that exist as runtime-reflective objects.
  `Protocol define:` already declares one; giving it bodies keeps it one, so
  no new `X define:` form is needed.
- **No duplicate rule implementations** across Rust and Erlang
  (`architecture-principles.md` §6–7). The flattening, precedence and
  conflict rules live in one place; the runtime consumes their *result*.

## Decision

### 1. Protocols may provide methods — `Protocol define:` with bodies

```beamtalk
// stdlib/src/comparable.bt
// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

/// Comparable — a total order derived from one required `<`.
///
/// A using class implements `<`; everything else is provided. A class that
/// already implements every selector below conforms to `Comparable`
/// whether or not it `uses:` it (ADR 0068).
Protocol define: Comparable
  /// Required — strict ordering. The using class must implement this.
  < other :: Self -> Boolean

  // === Derived ordering ===

  > other :: Self -> Boolean => other < self
  <= other :: Self -> Boolean => (other < self) not
  >= other :: Self -> Boolean => (self < other) not

  /// True if the receiver lies in the closed interval `[min, max]`.
  between: min :: Self and: max :: Self -> Boolean =>
    (self >= min) and: [self <= max]

  min: other :: Self -> Self => (self < other) ifTrue: [self] ifFalse: [other]
  max: other :: Self -> Self => (self < other) ifTrue: [other] ifFalse: [self]
```

The body grammar is the protocol body grammar (ADR 0068) plus method bodies:

| Line shape | Meaning |
|---|---|
| `selector :: … -> T` (no `=>`) | **Required** method — the using class, its superclass chain, or another used trait must provide it |
| `selector … => body` | **Provided** method — flattened into every user |
| `class selector …` without `=>` | Class-side **requirement**, as protocols already allow (ADR 0068, BT-1611) |
| `class selector … => body` | Class-side **provision** — post-v1 (Phase 7); in v1 an error, "class-side provided methods are not yet supported" |
| `sealed` / `internal` before the selector | **Rejected in v1** (§13). `sealed` cannot stop the using class from overriding (class wins, §3), and `internal` in a trait has no well-defined package once flattened into a user in another package (ADR 0071) |
| `uses: OtherProtocol …` | Composition — a protocol using another protocol's provisions. **Post-v1** (Phase 7, §Status 8); in v1 a `uses:` line in a protocol body is a "not yet supported" error |
| `// === Name ===` | Category divider (`method_category.rs`), kept with the trait's source |
| `/// doc` | Doc comment, carried onto the flattened method and shown by `browse` |

Generic protocols (`Protocol define: Enumerable(E)`, ADR 0068) may provide
methods too.

**`Self` in a trait.** `Self` as a *parameter* type is an error in class
methods today ("`Self` cannot be used as a parameter type … only valid in
return position", `type_checker/validation.rs:1959`) and legal in protocol
signatures (`beamtalk-language-features.md:1867`). A trait may use `Self` in
**both** positions, and flattening **substitutes the using class's name for
every `Self` in a provision's signature** before the method reaches the type
checker. `Comparable>>max: other :: Self -> Self` becomes
`DateTime>>max: other :: DateTime -> DateTime` in `DateTime`: exactly the
signature a hand-written method would declare. Substituting the return
position too matters: left as `Self`, `max:` flattened into a class with
subclasses would claim to return the receiver's subclass while returning
`other`. The Eiffel unsoundness that motivates the class-method ban (a
subclass narrowing a parameter type) does not arise, because the substituted
type is the using class, fixed at flattening, and subclasses inherit it
unchanged as they inherit any hand-written method. Required signatures keep
`Self` unsubstituted; they are checked like protocol requirements (§5). For a
generic user (`Pair(A, B) uses: Comparable`), `Self` becomes the user with
its own type parameters, `Pair(A, B)`.

**No primitives in provisions.** A provided method may not use `@primitive`
or `@intrinsic`. Provisions are flattened into user classes compiled in
project mode, which cannot reproduce stdlib-mode primitives (the reason the
workspace refuses stdlib patches, `beamtalk_repl_eval.erl:927-938`).

**Which mechanism to use.** A **superclass** says what a class *is* and
shares representation and state. A **protocol with provisions** is a
capability derived from a few required methods and usable across kinds and
hierarchies. An **extension method** adds one behaviour to one existing
class, typically one you don't own. The language guide (Phase 6) states
this rule.

A protocol with provisions still has no header modifiers, no superclass, no
`state:` / `field:` / `classState:` (§7), and no `native:`. It stays one
protocol per file (`comparable.bt`, as `printable.bt` is today) and compiles
to one module by the ADR 0119 rule. There is no new top-level form.

Names are protocol names (`Comparable`, `Enumerable`), not Pharo's
`TComparable`: there is one construct, and it is a type.

**`uses:` of a protocol with no provisions** is a hint, not an error or a
warning: "`Printable` provides no methods, so `uses:` only checks its
requirements here; conformance is structural (ADR 0068)." Its one effect is
to check the requirements at the class definition, the "assert conformance
here" idiom. That is none of the things ADR 0068 rejected `implements:` for:
it is optional, adds no nominal typing, and never breaks when a protocol is
added later. A hint rather than a warning also avoids churn, because a
protocol gaining or losing its last provision would otherwise flip every
user between clean and warning, which fails builds under
`--warnings-as-errors`. When a protocol later gains
its first provided method, the classes that should receive it add
`uses:`; structural conformers are affected as §8 describes.

### 2. Composing traits — `uses:` lines in the class body

```beamtalk
sealed typed Value subclass: DateTime native: beamtalk_datetime
  uses: Comparable
  field: …

  < other :: DateTime -> Boolean =>
    (Erlang beamtalk_datetime) lt: self with: other
  // `>`, `<=`, `>=` stay: each is its own FFI call, and the class body
  // wins over a provision (§3, §9).
  // Gained from Comparable: `between:and:`, `min:`, `max:`.
```

Grammar of a `uses:` line:

```
uses: [package@]ProtocolName[(TypeArgs)] [excluding: #(#sel, …)] [overriding: #(#sel, …)]
```

Post-v1 (Phase 7) adds one more optional clause, `[aliasing: #{#newSel => #traitSel, …}]`.
In v1 the keyword is reserved and rejected with a "not yet supported" error.

`package@` qualifies a protocol from another package, as `json@Parser` does for
a superclass (ADR 0070). Inside a protocol body only `uses:` is reserved,
and only at the start of a line that has no `=>` (a method *named* `uses:`
has a body, as ADR 0071 distinguishes `internal`). `excluding:` and the
other clause keywords appear only after it on the same line, so they stay
usable in ordinary selectors (`system_navigation.bt:1890`). Without the
reservation today `parse_protocol_method_signature_with_doc`
would read `uses: Comparable` as a required keyword signature with a
parameter named `Comparable`, so the protocol-body parser must check for them
first, as it already does for `extending:`. `overriding:` is reserved the
same way.

- One trait per `uses:` line; several traits are several lines. This matches
  `state:` / `field:` (one declaration per line) and keeps the header line —
  whose clause order is already fragile (`beamtalk-language-features.md:350-353`)
  — untouched.
- `uses:` lines come **immediately after the header**, before any slot or
  method. A class reads top-down: what it composes, what it holds, what it
  does. A `uses:` after a slot or method is an error (§13), and so is any
  other unknown keyword line in a class body — the silent-end-of-body
  behaviour goes.
- `excluding:` drops provided selectors from this use (Pharo `-`).
- `overriding:` acknowledges provided selectors that replace a method the
  class inherits from its superclass chain (§3a). It has no Pharo
  equivalent; it plays the role of C#'s `override`.
- `aliasing:` (**post-v1**, §Implementation) adds each trait-provided `#traitSel` to the class *also* under
  `#newSel` (Pharo `@`). The alias always copies the trait's original
  provision, even when `#traitSel` is also excluded; the original name stays
  unless excluded. The
  dictionary direction is Pharo's: new name on the left.
- The argument forms are ordinary literals — a list of symbols and a
  dictionary of symbols — so nothing new is lexed.
- A trait uses another trait with the same line, in the same position (first
  in the body).

The header form (`Value subclass: DateTime uses: Comparable`) was considered
and rejected: it is the Pharo spelling, but Beamtalk's header tail is already
order-sensitive, and exclusion/aliasing clauses do not fit on one line
(§Alternatives).

### 3. Flattening — the defining semantics

A class that uses traits **means exactly the class with the provided methods
written into its body**, after these steps, in order:

1. Collect each used protocol's provisions. (Post-v1, when a protocol may
   `uses:` another, expansion is transitive; a protocol's own provisions
   beat those of protocols it uses, `excluding:` is allowed on its `uses:`
   lines, and `overriding:` there is an error because a protocol has no
   superclass.)
2. Per `uses:` line, apply `excluding:`. (Post-v1, `aliasing:` runs
   first, reading from the trait's *original* provisions, so an alias
   survives the exclusion of the selector it copies:
   `excluding: #(#printString) aliasing: #{#describeString => #printString}`.)
3. Merge the provisions of all used traits into one set, detecting
   conflicts (§4).
4. Drop every trait provision whose selector (same side) the class body
   defines — **the class wins**.
5. Check requirements (§5), and check that every provision replacing an
   inherited method is acknowledged (§3a).
6. Compile the class with the merged methods as if they were its own.

Precedence, highest first: **class body → trait provision → inherited**. A
trait provision therefore *overrides* an inherited method, precisely as a
method written in the body would — but only an acknowledged one (§3a). This
is Schärli's flattening property and Pharo's rule, with Pharo's silent
override removed, and it is the property that makes every later section
cheap:
after step 6 there is nothing trait-shaped left for `ThreadedIr`, dispatch,
`super`, sealed self-sends, xref or hot reload to know about.

**Where it runs.** Flattening is a **new pass in `beamtalk-core` semantic
analysis** (`semantic_analysis/trait_expansion.rs`), with explicit inputs:
the module's AST, the ASTs of every trait it uses (§10a), and `ClassInfo`
for superclasses outside the module. It is *not* an extension of today's
same-file self-extension fold: that fold runs in the compiler port *after*
analysis (`compiler-port/handlers/compile.rs:109-142`) and its
`merge_method` (`compiler-port/decode.rs:596`) is replace-wins, the opposite
of class-wins. The pass runs in two halves:

- **Expansion** (steps 1–4, 6) runs *before* `ClassHierarchy` is built, so
  type inference (ADR 0083), protocol conformance (ADR 0068), sendability
  (ADR 0103), definite assignment (ADR 0124) and every validator see the
  flattened class.
- **The requirement check** (step 5) runs *after* `ClassHierarchy` is built,
  because it needs `resolves_selector` over the superclass chain.

**`defined_in` stays the using class.** Every existing consumer of
`MethodInfo.defined_in` reads it as a class name: inherited type-parameter
substitution (`build_inherited_substitution_map`,
`type_checker/inference/generics.rs:116-190`), method-local type-parameter
inference (`generics.rs:286-302`), internal-visibility package lookup
(`validation.rs:666-673`), DNU-override detection (`has_dnu_override_in`,
`method_resolution.rs:275-287`) and the LSP's `find_defining_class`. Setting
it to a trait name would silently break all five. Instead a new field
`MethodInfo.origin: Option<ProtocolName>` records provenance for reflection,
diagnostics and tooling, and `defined_in` keeps meaning "the class whose
module holds the code", which after flattening is the user.

**Type-parameter substitution and hygiene.** Expansion substitutes the
`uses:` type arguments for the trait's type parameters
(`uses: Enumerable(Worker)` replaces `E` with `Worker`) and `Self` as in §1.
It must be **hygienic**: a trait provision's method-local type variables are
alpha-renamed before substitution, so `Pair(A, B) uses: Enumerable(A)` does
not capture the `A` in `inject: initial :: A into: …` as the class's `A`.

**Name resolution.** Free class and protocol names in a provision are
resolved **in the protocol's package** at expansion time and emitted
package-qualified, and `internal` visibility (ADR 0071) of anything a
provision references is checked against the protocol's package. Without
this, a provision from package `json` flattened into a user would resolve
`Parser` in the user's package (ADR 0070 resolves names lazily at the use
site): `json`'s own internals would become visibility errors, and a
same-named class in the user's package would capture the reference.

**Source locations.** A flattened method keeps the source identity of its
protocol. Today `Span` carries only offsets (`span.rs:35`) and codegen
maps spans to lines through one source text per module (`options.rs:40`),
so a copied provision's spans must be tagged with their file. Diagnostics
whose span lies in a protocol are reported **in the protocol file, once**,
de-duplicated across users, with a note "while flattening into A, B, …";
BEAM line annotations and stack traces for a flattened method point at the
protocol's lines.

**Synthesised methods rank as class body.** A `Value` class's auto-generated
field accessors and `with*:` setters are suppressed today whenever
`class.methods` already has the selector (`value_accessors.rs:63-92`). The
expansion pass takes the synthesised-selector set from the existing shared
module `crates/beamtalk-core/src/synthetic_selectors.rs`, not a copy. A
trait provision must not suppress them: synthesised accessors count as
class body for step 4, so `field: size` beats a trait's `size`.

### 3a. Trait provisions never silently override inherited methods

**The problem.** Under plain Pharo precedence a trait's method beats an
inherited one with no signal. That makes traits a worse fragile base class
than inheritance, from two directions, and in neither does the using
class's own source change:

- **The trait grows.** `Describable` v2 adds a `printString` provision.
  Every user whose superclass defines `printString` silently loses it.
- **The superclass grows.** A superclass v2 adds `summary`, which a trait
  the subclass uses already provides. The subclass silently keeps the
  trait's `summary` and shadows the new superclass method.

**The rule.** After class-wins (§3 step 4), every remaining provision whose
selector the using class **inherits** from its superclass chain, on the
same side, must be acknowledged on its `uses:` line:

```beamtalk
Record subclass: AuditRecord
  uses: Describable overriding: #(#printString)
```

Otherwise it is a **compile error** in the using class, naming both
sides and both fixes:

```
error: Describable provides `printString`, which AuditRecord would otherwise
       inherit from Record.
  hint: to use Describable's version, write
          uses: Describable overriding: #(#printString)
        to keep Record's version, write
          uses: Describable excluding: #(#printString)
```

The error is the point: it turns a silent behaviour change on upgrade into a
build break at the one place that can decide, which is the versioning
argument behind C#'s explicit `override`/`new`.

**Details.**

- **A two-selector allowlist is exempt, not whole root classes.** Only
  `printString` and `displayString` inherited from `Object` or `Value`
  (`object.bt:140, 163`; `value.bt:83`) are exempt: they are cosmetic
  defaults a class is expected to replace. Every other root method needs
  `overriding:` like any inherited method, including `equals:` and `hash`
  (`object.bt:204, 244`): a provision that silently replaced a value's
  structural equality would change how its instances behave in sets and
  dictionaries (decided, §Status 5). So do `isNil`, `ifNil:`, `inspect`,
  `terminate:` and the rest. Exempting whole root classes
  would reopen the motivating case: `Describable` v2 adding `printString`
  would still silently replace `Value>>printString` on every direct `Value`
  user. Scala and Kotlin require explicit `override` even on `toString`;
  the allowlist is this ADR's concession to how often those two are
  replaced. A protocol that provides exactly one of `equals:` and `hash`
  gets a warning.
- **Sealed inherited methods** stay an error whether or not they are
  acknowledged (§13), exactly as for a class-body method.
- **Open world.** When the superclass chain leaves the indexed world
  (ADR 0100), the check cannot know what is inherited, so an unacknowledged
  override there is a hint, not an error.
- **Stale acknowledgements.** An `overriding:` entry that no longer replaces
  anything is a warning. It goes stale in three ways: the superclass drops
  the method, the trait drops the provision, or the class body starts
  defining the selector itself, so class-wins removes the provision before
  §3a runs. The list stays an accurate record of what
  the class knowingly replaces.
- **Diamonds (post-v1).** Once a protocol may use another, one provision
  can reach the class through several `uses:` lines (the same-origin case
  of §4); acknowledging it on any one of those lines is enough, and listing
  it on more than one is allowed and is not stale.
- **Only provisions are checked.** A method written in the class body
  overrides an inherited one silently, as it does today; the class's own
  source is where that decision is visible. This leaves one versioning gap
  §3a does not close: a superclass release that adds a method a subclass
  body already defines changes the subclass silently. That is C#'s
  `new`/`override` case, and closing it needs an opt-in lint in the style of
  TypeScript's `noImplicitOverride`, a separate decision.
- **Same origin is exempt.** If the inherited method and the provision come
  from the same protocol (a superclass started to `uses:` a protocol the
  subclass also uses), and the superclass has not customised the selector,
  they are identical and §3a does not fire. If the superclass customised
  it, §3a applies.
- **Foreign extensions count as inherited.** An ADR 0066 extension on a
  superclass is part of what the class inherits, so a provision replacing
  it needs `overriding:` too.
- **Timing.** The check needs the superclass chain, so it runs in the
  post-`ClassHierarchy` half of the pass, beside the requirement check (§3,
  §5).

### 4. Conflicts and their resolution

Two used traits providing the same selector on the same side is a **compile
error** in the using class, unless one of:

- the class body defines that selector (the class wins, §3 step 4);
- one use excludes it (`excluding:`);
- (post-v1) both provisions are the *same* method reached through two
  paths (a trait used directly and via another trait). In v1 no protocol
  uses another, so any two protocols providing the same selector conflict.
  "Same" means the same protocol and
  selector, not the same content hash, so a reload in progress cannot turn a
  diamond into a conflict. Same origin is not a conflict —
  Schärli's rule, which keeps diamond-shaped trait composition harmless.

An aliased copy is a provision like any other and can conflict.

```beamtalk
Value subclass: Report
  uses: Labelled
  uses: Describable
  // error: `printString` is provided by both Labelled and Describable in Report.
  //   Define `printString` in Report, or exclude one:
  //     uses: Describable excluding: #(#printString)
```

There is no "call both" or `Trait.super` (Java 8's `A.super.m()`). In v1 a
class that wants behaviour from both writes the method itself; its own
definition wins over both provisions, so no exclusion is needed:

```beamtalk
Value subclass: Report
  uses: Labelled
  uses: Describable

  printString -> String => self title ++ " " ++ self summary
```

Post-v1, `aliasing:` lets the class keep one provision under a new name and
call it, instead of rewriting its body:

```beamtalk
Value subclass: Report
  uses: Labelled
  uses: Describable excluding: #(#printString) aliasing: #{#describeString => #printString}

  printString -> String => self describeString ++ " " ++ self summary
```

### 5. Required methods

Every required selector of every used trait (after exclusion) must resolve on
the flattened class: from the class body, from another used trait's
provisions, or from the superclass chain. The check runs in the second
half of the expansion pass, after `ClassHierarchy` has been built from the
flattened classes (§3), so it can use the existing
`ClassHierarchy::resolves_selector`, and ADR 0100's open-world policy applies:
an unresolvable requirement is an **error** when the chain is closed-world
and a **hint** when the superclass lives in an unindexed package or the class
overrides `doesNotUnderstand:`.

```beamtalk
Value subclass: Version
  uses: Comparable
  field: major = 0
// error: Version uses Comparable but does not implement required `<`
//   hint: Comparable requires `< other :: Self -> Boolean`
```

**Provisions are checked once, in the protocol.** Each provision is
type-checked in the protocol file with `self` bounded by the protocol's
required ∪ provided selectors plus `Object`'s. A self-send outside that set
is an error: "`foo` is sent by `max:` but is neither required nor provided
by Comparable; declare it as required." This is Schärli's rule that the
required set covers every self-send, and it means a protocol with no users
is still checked. Per-user checking after flattening then reports only
diagnostics that depend on the user, once each.

**Excluding can break conformance.** `uses: T excluding: #(#x)`, where the
class neither defines nor inherits `x`, leaves the class not conforming to
`T`, the protocol it names. That is a warning: "C uses T but does not
conform to T: it excludes `x` without defining or inheriting it."

**`extending:` a protocol with provisions.** When `Q extending: P` and `P`
has provisions, `P`'s provided selectors are requirements for classes that
`uses: Q`, because `Q`'s type includes them (§8) and `extending:` brings no
bodies.

A required signature is checked like a protocol requirement: selector and
arity are mandatory; declared types are compared by the type checker and
mismatches are warnings, as everywhere else (ADR 0025).

### 6. `self`, `super` and `^` inside trait methods

- `self` is the instance of the using class. Self-sends in a provided method
  resolve against the using class, including its own overrides, because the
  method *is* the using class's method after flattening — for actors it is a
  clause of that class's `dispatch/4`, so the lexical-module self-send goes
  to the right module.
- `super` means the using class's superclass, as it would in a method written
  in the body. Traits have no superclass; `super` in a trait body that is
  never used by a class is simply unreachable code.
- `^` is a non-local return from the flattened method, compiled by the
  existing throw/catch lowering, exactly as in `Collection>>detect:`
  (`collection.bt:304`). The one known gap is not trait-specific: on an
  actor, `^` inside a block that is passed through a self-send to another
  method escapes as a raw error today (BT-3580, §9).
- Sealed-class self-send optimisation (direct calls to `__sealed_<sel>`)
  applies to flattened methods automatically, because it is decided per
  using class, after flattening.

### 7. Traits are stateless — required accessors, not slots

A trait body may not contain `state:`, `field:`, `classState:`, or any
`self.slot` read or write. State a trait needs is expressed as a required
accessor (and, for mutation, a required mutator), which the using class
supplies as a method:

```beamtalk
Protocol define: Counting
  /// Required.
  count -> Integer
  /// Required. Returns the receiver (Actor) or the updated value (Value).
  setCount: n :: Integer -> Self

  increment -> Self => self setCount: self count + 1
  reset -> Self => self setCount: 0
```

Rationale, in order of weight:

1. **Kind-neutral state access.** `self count` compiles correctly in an
   actor (`State` map), a value (`Self` map) or a class-side method
   (`ClassVars` map) because it is a message send, and message sends are
   already threaded by `ThreadedIr` in every context. A `self.count := …`
   in a trait body would need the trait to know which storage family it will
   be flattened into — that is a fourth threading input, and it is exactly
   the "per-kind semantics" ADR 0067 spent a whole ADR separating.
   This covers *state*, not every body shape: a provision that passes a
   block through a self-send to another provision is broken on actors today
   (BT-3580, §9). Kind-neutrality for block-taking provisions depends on
   that fix, or on the trait being written so no block crosses a self-send.
2. **No shape changes, ever.** Since a trait adds no slots, using, editing or
   reloading a trait never changes a class's state shape, so it never
   triggers ADR 0123 migration or `hot_reload_descendants`. Trait reload is a
   pure method-table event.
3. **It is the original model.** Schärli's traits are stateless with required
   accessors; Pharo shipped that for a decade before adding stateful traits,
   and its `TSlot`/`TraitedClass` machinery is the part that keeps breaking.
4. **`Object` subclasses cannot hold state at all** (ADR 0067), yet should be
   able to use class-side traits (§9, post-v1).

**Selectors a trait may not provide.** Some selectors change what the
compiler or runtime does with a class, not just what it answers, so a trait
providing them would change a class's shape or dispatch behind a `uses:`
line. A provision (not a requirement) of any of these is an error (§13):
`initialize`, `terminate:` (the actor lifecycle hook), `migrateFromV<N>:` (ADR 0123's migration chain,
`ast/class.rs:350`), `doesNotUnderstand:args:` (open-world conformance,
ADR 0068/0100), and `supervisionPolicy` / `supervisionSpec`
(`actor_codegen.rs:337-349`). The statelessness check is syntactic
(`self.slot`, slot declarations); reflective writes such as
`fieldAt:put:` are not detected, exactly as they are not in any method today.

Stateful traits are **deferred**, not rejected. If evidence appears (a trait
whose users all declare the same slot with the same default), the extension
is a per-kind `state:`/`field:` declaration inside the trait, flattened into
the user's slot list, and it must then specify its own `ThreadedIr`
implications. Nothing decided here forecloses that.

### 8. The type of a protocol with provisions

A protocol's type — what `conformsTo:`, `:: T` annotations, intersections
and bounds check — is its **required ∪ provided** selectors, instance and
class side. For a protocol with no provisions that is exactly ADR 0068's
rule, unchanged. `Protocol requiredMethods:` keeps answering the
requirements; conformance checks `requiredMethods:` ∪ `providedMethods:`.
Consequences:

- `x :: Comparable` in a type position, `Collection(Object) & Comparable`
  intersections, and `Logger(T :: Comparable)` bounds all work with no new
  type-system concept.
- **Conformance stays structural.** `Integer` implements every `Comparable`
  selector itself, so `Integer conformsTo: #Comparable` is `true` without
  `uses:`. A class that uses `Comparable` conforms because, after flattening,
  it *has* the selectors, not because it declared anything. `uses:` is a
  composition declaration whose effect makes structural conformance hold;
  it is not ADR 0068's rejected `implements:`.
- The type checker sees trait-provided methods through `ClassInfo.methods`
  like any other method, so `dt between: a and: b` infers `Boolean` on
  `DateTime`, `Version max: other` infers `Version` (`Self`), and a call to
  a required-but-unimplemented selector is caught by §5 before inference
  runs.
- **Giving a protocol's *existing* signatures bodies is
  source-compatible** for every consumer: `Printable`'s selector
  set does not change, so every `:: Printable` annotation, `conformsTo:`
  query and structurally-conforming class is unaffected. **Adding a *new*
  provision is not**: it enlarges the protocol's type, so every class that
  conformed structurally without `uses:` stops conforming. That is the same
  break as adding a required selector to a protocol today (ADR 0068), and it
  is the price of the required ∪ provided rule (decided, §Status 2). The
  cost is smaller than "breaking" suggests: ADR 0068 makes conformance
  problems warnings, not errors, so adding a provision never fails a build
  unless the project builds with `--warnings-as-errors`, as the stdlib
  does (`Justfile:584`).
  It adds warnings exactly where a structural conformer reaches code typed
  against the trait, which are the places that would fail at run time if
  that code sent the new selector. This matches TypeScript, where adding a
  member to an interface is a breaking change for structural implementers
  and is reported, not silently accepted.
  Removing a protocol's last provision leaves its users' `uses:` lines as
  no-op warnings (§1), not errors.
- **An override of a provision is checked against it.** Class-wins drops the
  provision in step 4, but its signature is kept for one check: the class
  body's method must be override-compatible with it, by the same rule used
  for overriding an inherited method, and a mismatch is a warning. Without
  this, a class could replace `Enumerable>>select: -> List(E)` with
  `select: -> Set(E)` while the protocol's type still promised `List(E)`.

`extending:` and (post-v1) `uses:` can both appear in a protocol body and
stay distinct. `extending: P` adds P's selectors to this protocol's *type* and
nothing else (ADR 0068). `uses: P` flattens P's *provisions* into this
protocol, which also brings them into its type. A protocol that wants P's
bodies uses it; one that only wants to be a subtype of P extends it.

### 9. Traits across the class kinds

**Value** — `DateTime`, above (§2). The same change applies to `Duration`,
`Uuid` and `String`. Each keeps its primitive `<` and should keep its
primitive `>`/`<=`/`>=` too, since a body method beats a provision (§3) and
each is its own FFI call; what they gain is `between:and:`, `min:` and
`max:`. **`Number` does not use `Comparable`.** It already defines all four
operators and `between:and:` (`number.bt:38-47, 101`), so the trait would
add only `min:`/`max:`, and `Comparable`'s `min: other :: Number -> Number`
would change tie semantics (`3 max: 3.0` answers `3.0` today, and would
answer `3`). The `Integer`/`Float` `min:`/`max:` duplicate is fixed by
moving it up to `Number`, which is plain inheritance.

**Actor** — an actor that wants enumeration over what it holds. The trait
requires a **snapshot** rather than an internal iterator, so every
provision forwards to a primitive `List` operation and no block ever
crosses a self-send:

```beamtalk
// stdlib/src/enumerable.bt
Protocol define: Enumerable(E)
  /// Required — the elements, in the receiver's order, as a List snapshot.
  elements -> List(E)

  size -> Integer => self elements size
  isEmpty -> Boolean => self elements isEmpty
  isNotEmpty -> Boolean => self elements isNotEmpty

  do: block :: Block(E, Object) -> Nil => self elements do: block
  inject: initial :: A into: block :: Block(A, E, A) -> A =>
    self elements inject: initial into: block
  select: block :: Block(E, Boolean) -> List(E) => self elements select: block
  collect: block :: Block(E, R) -> List(R) => self elements collect: block
  detect: block :: Block(E, Boolean) ifNone: noneBlock :: Block(T) -> E | T =>
    self elements detect: block ifNone: noneBlock
  anySatisfy: block :: Block(E, Boolean) -> Boolean => self elements anySatisfy: block
  count: block :: Block(E, Boolean) -> Integer => self elements count: block
```

```beamtalk
typed Actor subclass: WorkerPool
  uses: Enumerable(Worker)
  state: workers :: List(Worker) = #()

  elements -> List(Worker) => self.workers

  add: w :: Worker -> Nil => self.workers := self.workers add: w
  idle -> List(Worker) => self elements select: [:w | w isIdle]
```

The snapshot design is deliberate, and it replaces an earlier draft whose
provisions were written over a required `do:` (`size` as
`self inject: 0 into: …`, `detect:ifNone:` as `self do: [… ^each …]`). That
shape is correct in a `Value` but **broken in an `Actor` today**: a block
passed through an actor self-send loses both `^` (the NLR throw escapes as a
raw `$bt_nlr` error) and captured-local writes (`size` answered `0` for four
elements). The probe and the fix are tracked as **BT-3580**. The snapshot
design also answers two semantic points that a `do:`-based trait gets wrong
on actors:

- **Blocks run in the actor's process.** `pool select: [...]` sends the
  block to the actor, as any block argument to an actor method does
  (language guide § Checked boundaries; ADR 0103). Captured-variable writes in it do not reach the caller, and a
  re-entrant send to `pool` from inside it deadlocks or raises
  `calling_self`. Callers that want caller-side semantics use
  `pool elements select: [...]`, and the trait makes `elements` the
  documented way in.
- **Speed.** Each provision is one primitive `List` call, not an
  `inject:`/`addFirst:`/`reversed` rebuild.

Adoption is per class, not mechanical. `SupervisionTree` and `ChangeLog`
are one-line forwarders to an inner list already, so they become
`elements => self flatNodes` (not `nodes`, which is an FFI enrich pass,
`supervision_tree.bt:40-41`) and `elements => self activeEntries`. But
`ChangeLog select:` deliberately ranges over *all* `entries`, including
orphans (`change_log.bt:36-44, 96-106`), so it keeps its own `select:`
(class wins), and `SupervisionTree do:` answers `self`, not `nil`, so it
keeps its own `do:`. Its return type (`SupervisionTree`, not `Nil`) must be
reconciled in Phase 6 itself, because the §8 override warning would fail
the stdlib build under `--warnings-as-errors`: either change it to `Nil`
after auditing callers, or `uses: Enumerable(SupervisionNode) excluding:
#(#do:)`. **`Collection` does not *use* `Enumerable` in v1**: its
`select:`/`collect:` answer `Self` via `species`, and every enumeration
method it has is already inherited by its subclasses. But Phase 6 adds
`elements -> List(E) => self asList` to `Collection`, so every list, set and
array **conforms** to `Enumerable` structurally, and `x :: Enumerable(E)`
accepts them. Without it the name would promise the Ruby/Elixir
`Enumerable` while no collection satisfied it.

**Object** (class-side provisions are **post-v1**, §Implementation; in v1 an `Object` subclass can use only instance-side provisions, which is rarely useful) — instance methods on an uninstantiable class are pointless, so
`Object subclass:` users compose **class-side** traits:

```beamtalk
Protocol define: Versioned
  /// Required.
  class version -> String

  class banner -> String => self name asString ++ " " ++ self version
  class isVersion: v :: String -> Boolean => self version =:= v
```

```beamtalk
Object subclass: ReportTool
  uses: Versioned

  class version -> String => "1.4.0"
```

`ReportTool banner // => "ReportTool 1.4.0"`. A trait with instance-side provisions
used by an `Object` subclass is not an error — the methods flatten onto a
class that will never be instantiated, exactly as a hand-written instance
method on an `Object` subclass is not an error today.

**Native classes** (`native: mod`, ADR 0101) can use traits; a provided body
is an ordinary method next to the `self delegate` ones.

### 10. Codegen — flatten into the user; the protocol module carries the source

**Using class.** Nothing new. The flattened `ClassDefinition` (with the trait
`MethodDefinition`s merged in, provenance-marked) goes through the existing
actor / value / class-side generators. The only codegen-visible additions
are (a) `methodXref` rows for flattened methods carry
`provenance := protocol, origin := ProtocolName`; (b) `methodSource` for a
flattened method is the **trait file's source slice for that method**, taken
from the trait's span and not unparsed after substitution, so `browse` and
`CompiledMethod source` show the real code and the ChangeLog does not see a
spurious difference; and (c) the user's `__beamtalk_meta` records
`uses => [{ProtocolName, ContentHash}]`, the hash of each used protocol's
source, so the build, ADR 0105's re-check, and ADR 0125's release preflight
can detect a user compiled against a stale trait.

**Protocol module.** A protocol file already compiles to a module that
registers the protocol on load (`generate_protocol_registrations`,
`class_registry.rs:699`). With provisions it still contains **no dispatch
and no callable methods**. Its registration additionally records the
provided signatures, per-method source, docs
and categories, and the runtime `beamtalk_protocol_registry` gains a
provided-selector set and a users index. There is no second registry.

**Carrying trait ASTs to users.** Flattening needs the trait's method ASTs
at the user's compile time, and `__beamtalk_meta` does not carry method
source today (`class_meta.rs`; per-method source lives only in the live class
process via `register_class/0`'s BuilderState, `class_registry.rs:555`).
The carrier is therefore **the trait's `.bt` source**, which every compile
path already has access to:

- **Same package:** the parsed `Module` of the trait file.
- **Dependency package (CLI):** the dependency's source checkout, which
  `beamtalk-cli/src/dependency_classes.rs` already re-parses to build
  cross-package `ClassInfo`; it now also yields trait ASTs.
- **REPL / compiler port:** the workspace's source for the trait file, or,
  for a trait loaded only as a `.beam`, a new exported
  `'__beamtalk_protocol_source'/0` on the protocol module that returns its
  source text. That function is the only new carrier, and it exists only on
  protocol modules that have provisions.

A trait is therefore a compile-time dependency of its users; §10a says how
each build path tracks it.

**Why not a shared trait module + dispatch step** (foreign-extension style):
an actor method's self-sends are compiled against the lexical module
(`<module>:safe_dispatch`, `dispatch_codegen.rs:2019-2028`); compiled once in
a trait module they would target a module with no gen_server and no
`dispatch/4`. The calling convention differs by kind, so a shared module
would need one body per kind per method. Every send on every class would pay
an extra ETS lookup in the chain walk that ADR 0032 just shortened. And
`super`, sealed self-sends, `late` slot guards and error hints all bake in
the lexical class. Flattening makes every one of these a non-issue; the
costs it carries instead (N copies of each method's bytecode, N recompiles
on trait edit) are priced in §Consequences.

### 10a. Build graph and invalidation

Traits are the first cross-file dependency that **embeds code** in its
dependents. Today every compile path treats `.bt` files as independent:
the CLI rebuilds a file only when its own content hash changes
(`commands/build/changes.rs:120-150`), and `build_stdlib` says "files are
independent, no ordering required" (`build_stdlib.rs:132, 196-198`). Each
entry point changes as follows:

| Entry point | Gets trait ASTs from | Invalidates users by |
|---|---|---|
| CLI `beamtalk build` | Same-package parse; dependency source (§10) | A user's cache key becomes its own hash **plus the hashes of its transitive traits** |
| `build_stdlib` | The existing protocol pre-pass (`build_stdlib.rs:968`), extended to keep full ASTs of protocols with provisions and hand them to every file | Stdlib is rebuilt whole; `generated_builtins.rs` is regenerated from the flattened classes |
| Compiler port (REPL, workspace) | Workspace source, or `'__beamtalk_protocol_source'/0` | §11 fan-out, using the users index in the protocol registry |
| LSP / language service | `ProjectIndex`, which gains trait → users edges | Re-analyses open users when a trait file changes |
| MCP lint / `beamtalk check` | Same as the CLI build | Same as the CLI build |

**Load order** is unchanged: protocol modules already load before classes
(`protocol_modules`). A class module never *calls* a protocol module, so a
missing protocol module at run time affects reflection only, never
dispatch.

**Binary-only dependencies.** Outside the REPL, building a user of a
dependency's protocol needs that protocol's source: from the dependency's
source checkout, or from `'__beamtalk_protocol_source'/0` on its compiled
module. A dependency that ships neither cannot have its provisions
flattened, and `uses:` of such a protocol is an error naming the missing
source.

**Packages and releases.** A dependency's trait bodies become part of each
downstream user's compiled code. A patch release of a dependency that
changes a provision has no effect downstream until the downstream package is
rebuilt. The `uses => [{Name, Hash}]` meta entry (§10) lets `beamtalk
build` warn when a dependency's trait hash differs from the one a user was
compiled against, and lets ADR 0125's appup generation see that a user
module changed.

### 11. Hot reload and live patching

- **Stdlib traits are read-only**, like stdlib classes: the workspace
  already refuses to patch a class compiled in stdlib mode
  (`beamtalk_repl_eval.erl:927-938`), and re-expanding a stdlib user would
  recompile `DateTime` or `String`. The existing refusal looks up classes,
  not protocols, so `Comparable >> max: …` at the REPL gets a new message,
  approved to mirror the existing stdlib-class one (§Status 9):
  "Cannot recompile 'max:' on stdlib protocol 'Comparable': built-in
  protocols are read-only in the workspace"
- **Editing a user-package trait file** recompiles the trait and **every
  loaded, source-backed, non-stdlib user**, then reloads each user through
  the existing `update_class` path. Users without source in the workspace
  (dependency or release classes) keep their old flattened code, and the
  reload warns naming them; they pick the change up on their next build
  (§10a). Because no shape can
  change (§7), no `sys:change_code`, no migration, no
  `hot_reload_descendants`. ADR 0105's re-check then runs on the users'
  senders as it does for any method change.
- **A trait edit is all-or-nothing: compile everything, then load.** A
  trait change can be valid on its own and still break one of its users:
  a new provision may conflict with another trait that user has (§4), fail
  that user's `overriding:` check (§3a), or leave a requirement unmet
  (§5). Reloading users one at a time would leave some classes on the new
  trait and some on the old. So every trait edit — a file reload,
  `Describable >> sel => …`, or `Describable removeSelector:` — runs in two stages:
  1. **Compile** the trait and every source-backed user in memory. Nothing
     is loaded yet.
  2. **If every compile succeeds** (warnings allowed), load the trait
     module first, then each user through `update_class`. **If any
     compile fails,** load nothing: the edit is rejected, the trait keeps
     its previous definition in the image, and the error lists every
     failing user with its own diagnostic and fix:

  ```
  error: Describable edit rejected — 1 of 4 users fails to compile.
    AuditRecord: Describable provides `summary`, which AuditRecord would
      otherwise inherit from Record.
      hint: uses: Describable overriding: #(#summary)
            or: uses: Describable excluding: #(#summary)
  ```

  The guarantee covers compilation, which is where every trait-induced
  failure is detected. Loading is per module, so for the few milliseconds
  of stage 2 a process can observe some users on the new code and some on
  the old, as it can today when a class and its subclass are reloaded
  together. Suspending processes to close that window is out of scope. If
  a load in stage 2 fails, the modules already loaded in that edit are
  reloaded from their previous binaries. The same two-stage behaviour
  applies on every surface (REPL, MCP, LSP-driven save), recorded in
  `docs/development/surface-parity.md`.
- **Live-patching a trait method** — `Describable >> summary => …` — is a
  message send (principle 11): it replaces the method in the trait's
  registry entry and re-expands users through the two-stage rule above,
  the same whole-class recompile that `Foo >> bar => …` performs today
  (ADR 0066 §REPL).
- **`Describable removeSelector: #summary`** (ADR 0112) removes the
  provision and re-expands users, also through the two-stage rule; it is
  rejected if a user relied on the removed provision to satisfy another
  trait's requirement. **`DateTime removeSelector: #max:`** where `max:` is
  trait-provided is an error: "`max:` is provided by trait Comparable;
  exclude it with `uses: Comparable excluding: #(#max:)` or remove it from the
  trait". A class's own override of a trait selector can be removed as
  usual, which re-exposes the provision; if the re-exposed provision would
  fail §3a (it replaces an inherited non-allowlisted method the class does
  not acknowledge), the removal is refused with the §3a hint.
- **Renaming or removing a *required* selector on one user** —
  `DateTime renameSelector: #< to: #precedes:` — is refused while any used
  trait requires or sends it, because the sites that send it live in trait
  bodies shared with other users. The hint names the trait.
- **Browse `save-section` on a flattened method** is routed to the trait
  file, not the class file. Saving it in the class file would silently
  create a detached override; the browse op resolves the file from
  `origin`.
- **Defining a protocol with provisions at the REPL** is `Protocol define:`,
  as today; changing a class's `uses:` lines is a class
  redefinition, recompiled whole as today.
- **Workspace flush** (ADR 0113) writes a trait file like a class file.

### 12. Reflection and tooling

| Surface | Addition |
|---|---|
| `Protocol` (`protocol.bt`) | `Protocol providedMethods: #Comparable` and `Protocol usersOf: #Comparable` (answering `List(Behaviour)`, order unspecified; `SystemNavigation usersOf:` shares its implementation) beside the existing `requiredMethods:` and `conformingClasses:`. `usersOf:` answers the classes that `uses:` it; `conformingClasses:` still answers every structural conformer |
| `Behaviour` | `usedProtocols -> List(Symbol)` (directly used, in `uses:` order), `allUsedProtocols` (transitive, including superclasses'), `usesProtocol: #Comparable` — distinct from the existing `protocols`, which answers structural conformance; `methods` still answers local selectors and **includes** flattened ones, because they are local |
| `CompiledMethod` | `origin -> Symbol | Nil` → the protocol name or `nil`; `source` is the trait's text; `respondsTo:`, `canUnderstand:`, `includesSelector:` need no change — the method is there |
| `SystemNavigation` | `usersOf: #Comparable`; `implementorsOf: #between:and:` lists every user (true — each *does* implement it) and each row's `origin` says which trait |
| Browse / categories (grouping **post-v1**; in v1 flattened methods are listed under their protocol's own dividers, unprefixed) | Flattened methods appear in the class under their trait's own `// === ===` dividers, prefixed with the trait name (`Comparable › Derived ordering`), via a new `provenance := protocol` branch beside the existing `extension → "extensions"` one (`protocol_from_source/4`, `beamtalk_repl_ops_browse.erl:2284-2287`) |
| Xref (ADR 0087, 0115) | `beamtalk_xref_methods` rows: `provenance := protocol`, `origin := ProtocolName`. Sender rows inside a flattened body are indexed per user, `recv_type` per ADR 0115, so `sendersOf:` on `<` finds `DateTime`'s copy of `between:and:` — correct, because that copy really sends `<` to a `DateTime` |
| Rename (ADR 0114; protocol-wide rename is **post-v1**; in v1 a class rename also rewrites type arguments in `uses:` lines, e.g. `uses: Enumerable(Worker)`, via `referencesTo:`) | `renameSelector:to:` on a trait-provided selector of a **class** is refused with a hint to rename on the trait; on the **trait** the site closure is the trait body ∪ the union of every user's ADR 0114 closure, one ChangeLog entry. `renameTo:` on a trait adds `uses:` lines to `referencesTo:` (a new `uses_protocol` reference row) and moves the file like a class rename does |
| LSP | Go-to-definition on a flattened method jumps to the trait; completion on a receiver of a using class lists trait methods (they are in `ClassInfo.methods`); hover shows `from Comparable` (hover: post-v1) |
| Extension conflict (ADR 0066) | An extension `DateTime >> max:` on a trait-provided selector is the existing "cannot override a method defined in the class body" error, because after flattening it *is* in the body |

### 13. Diagnostics

All at the `uses:` line or the offending trait line; codes assigned at
implementation in the existing `E`/`W` series.

| Situation | Severity | Message shape |
|---|---|---|
| Two traits provide the same selector | Error | "`sel` is provided by both A and B in C. Define `sel` in C, or exclude one: `uses: B excluding: #(#sel)`" |
| Required selector unresolvable (closed world) | Error | "C uses T but does not implement required `sel`" + the required signature as hint |
| Required selector unresolvable (open world) | Hint | as ADR 0100 |
| `uses:` names a protocol with no provided methods | Hint | "`Printable` provides no methods, so `uses:` only checks its requirements here; conformance is structural (ADR 0068)" (§1, §Status 6) |
| `uses:` names an unknown name | Error | "unknown protocol `T`" + nearest-name hint |
| `uses:` after a slot or method | Error | "`uses:` lines must come before state and method declarations" |
| Unknown keyword line in a class body | Error | "unexpected `foo:` in class body" (replaces the silent end-of-body) |
| `excluding:` names a selector T does not provide | Error | "T does not provide `sel`" |
| `aliasing:` in v1 | Error | "`aliasing:` is not yet supported" (the check itself, "T does not provide `sel`", arrives with Phase 7) |
| `excluding:` a required selector | Error | "`sel` is required by T, not provided; requirements cannot be excluded" |
| `state:`/`field:`/`classState:` or `self.slot` in a protocol | Error | "protocols are stateless — declare `slot -> Type` as a required method" |
| A `uses:` line in a protocol body (v1) | Error | "a protocol using another protocol is not yet supported" (post-v1 it becomes composition, and a cycle is an error: "protocol A uses itself through B") |
| Protocol modifier (`sealed Protocol define:`) | Error | "protocols take no modifiers" |
| `sealed` or `internal` on a provided method | Error | "`sealed`/`internal` are not supported on provided methods in v1" |
| A protocol provides `initialize`, `migrateFromV<N>:`, `doesNotUnderstand:args:`, `supervisionPolicy` or `supervisionSpec` | Error | "a protocol cannot provide `sel`; it changes how the class is built or dispatched. Declare it as a required method instead" |
| A provision would override an inherited `sealed` method | Error | as for a class-body method overriding a sealed method |
| A provision would override an inherited method (outside the kind roots) without `overriding:` | Error | §3a — names the trait, the superclass, and both `overriding:` and `excluding:` fixes |
| The same, where the superclass chain is open-world (ADR 0100) | Hint | §3a |
| An `overriding:` entry that replaces nothing | Warning | "`sel` in `overriding:` does not override an inherited method; remove it" |
| `overriding:` names a selector T does not provide | Error | "T does not provide `sel`" |
| A class `uses:` a trait its superclass already uses, and the superclass has not customised any of its selectors | Hint | "C's superclass already uses T; this `uses:` is redundant" (same origin, §3a) |
| The same, where the superclass customised a selector T provides | Error | §3a — the provision would replace the superclass's customisation; acknowledge with `overriding:` or `excluding:` |
| A provision sends a selector that is neither required nor provided (nor `Object`'s) | Error | "`foo` is sent by `max:` but is neither required nor provided by T; declare it as required" (§5) |
| A provision references a name not resolvable from the protocol's package | Error | reported once, in the protocol file (§3, "Name resolution") |
| `uses: T excluding: #(#x)` leaves the class not conforming to T | Warning | "C uses T but does not conform to T: it excludes `x` without defining or inheriting it" (§5) |
| A protocol provides exactly one of `equals:` and `hash` | Warning | §3a |
| `overriding:` on a protocol's own `uses:` line (post-v1) | Error | "a protocol has no superclass; `overriding:` has no meaning here" (§3 step 1) |
| A provision uses `@primitive` or `@intrinsic` | Error | "provided methods cannot use primitives" (§1) |
| A class-side provision (`class sel … =>`) in v1 | Error | "class-side provided methods are not yet supported" (§1) |
| A class-body override is not override-compatible with the provision it replaces | Warning | §8 |
| Renaming or removing a required selector on a user | Runtime error | §11 |
| A trait edit that makes any source-backed user fail to compile | Runtime error (edit rejected, nothing loaded) | §11 — lists every failing user with its diagnostic |
| `Self` as a parameter type in a class body method | Error | unchanged (`check_no_self_in_params`); the hint mentions traits when the method's class uses one |
| Type mismatch between a required signature and the class's method | Warning | as ADR 0025 protocol/type warnings |
| `removeSelector:` on a trait-provided method of a class | Runtime error | see §11 |

### REPL session

With this `version.bt` loaded (`Comparable` is a stdlib protocol):

```beamtalk
typed Value subclass: Version
  uses: Comparable
  field: major :: Integer = 0
  field: minor :: Integer = 0

  < other :: Version -> Boolean =>
    (self.major < other major) or: [(self.major =:= other major) and: [self.minor < other minor]]
```

```beamtalk
v := Version major: 1 minor: 4
v between: (Version major: 1 minor: 0) and: (Version major: 2 minor: 0)   // => true
v max: (Version major: 1 minor: 9)                       // => Version(major: 1, minor: 9)
Version usedProtocols                                     // => [#Comparable]
Version conformsTo: #Comparable                           // => true
// Integer conforms structurally, without uses:
Integer conformsTo: #Comparable                           // => true
(Version >> #max:) origin                                 // => #Comparable
(Version >> #<) origin                                    // => nil
(Protocol usersOf: #Comparable) size                      // => 5
Comparable >> max: other :: Self -> Self => (self < other) ifTrue: [other] ifFalse: [self]
// => error: Cannot recompile 'max:' on stdlib protocol 'Comparable': built-in protocols are read-only in the workspace
Version removeSelector: #max:
// => error: `max:` is provided by trait Comparable; exclude it with
//    `uses: Comparable excluding: #(#max:)` or remove it from the trait
```

### Single source of truth

- **Flattening, precedence, conflict and requirement rules** live in one
  Rust pass (`semantic_analysis/trait_expansion.rs`, §3), which produces a
  flattened `ClassDefinition`. The compiler port's `merge_method` fold is
  not reused and does not learn about traits. Codegen, the type checker, validators and the
  language service consume its output; none re-derives the rules. The
  runtime never flattens: it receives compiled classes plus provenance data.
- **A protocol's type** (required ∪ provided) is computed once, in Rust,
  and emitted through the existing protocol registration codegen.
  `beamtalk_protocol_registry.erl` stores the result and a users index; it
  never recomputes the rule.
- **The extended protocol registration shape** (provided signatures and
  users) crosses the Rust→Erlang boundary as *data*. It gets a
  conformance fixture under
  `runtime/apps/beamtalk_runtime/test/fixtures/` like the other meta shapes
  (architecture-principles §7 "Keep" disposition), not a hand-mirrored
  record.
- **Method categories** stay source-only (`method_category.rs`); the browse
  layer reads the trait's dividers from the trait's source, so no second
  category representation appears.

## Prior Art

| Language | Mechanism | Composition | Conflicts | State | Taken / rejected |
|---|---|---|---|---|---|
| **Smalltalk-80 / Squeak / Pharo ≤ 6** | Traits (Schärli, Ducasse, Nierstrasz, Black, ECOOP 2003): `Object subclass: #C uses: TA + TB - {#x} @ {#y -> #x}` | Flattened; class wins; `+`/`-`/`@` | Explicit exclusion or alias; unresolved conflict is a method that errors when called | Stateless, required accessors | **Adopted** wholesale: flattening, precedence, exclusion, required/provided, same-origin-is-not-a-conflict (alias post-v1). Adapted: keyword clauses instead of `+ - @` operators; conflict is a *compile* error, not a runtime `traitConflict`; no `T` prefix |
| **Pharo 7+** | Stateful traits (`TraitedClass`, `TSlot`) | as above + slots | as above | Slots | **Deferred** (§7) — kind-specific slots need a threading story |
| **Newspeak** | Mixins: every class declaration denotes a mixin that can be applied to a superclass to create a class | Linearised application creates a class per use | Latest application wins (order) | Yes | **Rejected** (§Alternatives) — order-dependent, one registry class + module per application, extra chain steps per send |
| **Scala** | `trait` with linearisation; `with A with B` | Linearised; `super` chains through traits | Two unrelated traits with the same concrete member are a compile error unless the class overrides; `override` is required on every concrete override, including `toString` | Yes | Rejected as mixins. Adopted: "trait is also a type" (§8), conflicts as compile errors (§4), and explicit override with no root-class exemption beyond §3a's short allowlist |
| **TypeScript** | No traits; a `class` used as a type includes all its members; `override` keyword and `noImplicitOverride` (4.3) | Structural | n/a | n/a | Adopted: a type made of every member, implemented or not, is our required ∪ provided rule (§8), and adding a member is a reported break for structural implementers. `noImplicitOverride` is the same concern as `overriding:` (§3a) |
| **Rust** | `trait` with default methods, `impl T for S` | Static, type-directed; orphan rules | Disambiguation by qualified path | None (associated functions, consts and types only) | Adopted: required vs. default methods as the two body shapes. Rejected: nominal `impl` — ADR 0068 chose structural |
| **Swift** | Protocol extensions with default implementations | Static dispatch for non-requirement extension methods | Ambiguity error | None | Rejected: the static/dynamic dispatch split is a well-known footgun; Beamtalk has one dispatch (ADR 0006) |
| **C# 8** | Default interface methods; `override` / `new` on class members | Class wins over interface defaults; a class member hiding a base member without `new` or `override` warns | Must override | None | Adopted: the versioning stance. C# makes replacing inherited behaviour an explicit act so a base-type release cannot silently change a subclass; `overriding:` (§3a) is that rule applied to trait provisions |
| **Java 8** | Interface default methods | Class wins, **including inherited superclass methods**, which beat interface defaults; `A.super.m()` to pick a default | Two defaults with the same signature must be overridden | None | Adopted: the class body wins. Diverged: here a provision beats an inherited method, but only with `overriding:` (§3a). Rejected: the qualified-super call; the class writes the method (v1) or aliases (post-v1) (§4) |
| **Kotlin** | Interface default methods; `by` delegation | A superclass method and an interface default with the same signature is a compile error ("must override … inherits many implementations"); `super<A>.m()` | Must override | None | Direct precedent for §3a: an overlap between an inherited method and a default is an error until the class says what it wants. `by` delegation is the snapshot `Enumerable` design (§9) in language form |
| **Pony** | `trait` (nominal, default methods) + `interface` (structural, may also carry default bodies) | Only types that declare `is I` receive the bodies; structural conformers do not | Must override | None | The strongest precedent for §8 and decision 2: a structural type that includes defaulted methods, with bodies delivered only by an explicit declaration. Pony has no exclusion/alias; we keep Pharo's |
| **Dart** | `mixin` with `on` constraints, `with` | Linearised | Order decides | Yes | Rejected as mixins. A mixin's abstract members are its required methods; `on` constrains the superclass type so `super` calls type-check |
| **Ruby** | `module` + `include` / `prepend` | Linearised into the ancestor chain | Order decides | Instance vars by convention | Rejected as mixins; `Comparable`/`Enumerable` are the canonical examples and the names we use |
| **Perl Moose / Raku** | Roles: `with 'R' => { -excludes => …, -alias => … }` | Flattened (roles are traits) | Compile-time conflict unless resolved | Attributes allowed | Confirms exclusion/alias as keyword clauses read fine outside Smalltalk |
| **Elixir** | `use M` → `__using__` macro injects code; `defprotocol` for dispatch | Textual injection | Injected and local clauses of the same name/arity merge (with a "clause cannot match" warning) unless the injector marks them `defoverridable` | n/a | `use` is unchecked injection; our flattening is checked injection, and `defoverridable` is our class-wins rule made opt-in. Protocols ≈ our ADR 0068. `Enumerable` is the reference API |
| **Erlang / OTP** | `-behaviour(M)` callbacks; the experimental, since-removed `-extends` module attribute | Contract only | n/a | n/a | Behaviours ≈ protocols; no body sharing — the gap this ADR fills |
| **LFE Flavors** | Mixins (`(defflavor … (:mixins …))`) | Linearised | Order | Yes | Rejected as mixins |
| **Gleam** | None; modules and functions | n/a | n/a | n/a | Not applicable |
| **PHP** | `trait` + `use`, abstract members as requirements | Flattened; class > trait > inherited | `insteadof` / `as` resolve conflicts | Properties allowed | The closest mainstream match to this ADR's model, and it shows the silent override of inherited methods that §3a exists to prevent |
| **Go** | Structural interfaces, deliberately without default methods; embedding for reuse | n/a | n/a | n/a | Supports decision 2's reasoning: in a structural system, defaults that structural conformers never receive are a trap, so Go has none |

**A note on the word "protocol".** In Pharo a *protocol* is a method
category — `accessing`, `printing` — with no required-selector set and no
conformance check. Beamtalk already uses the word both ways: the REPL's
`browse-protocols` op groups methods by category (the Pharo sense), while
`Behaviour protocols` and `Protocol define:` are ADR 0068's structural
types. In this ADR "protocol" always means the ADR 0068 type, and the
rule that a protocol's provided methods are part of its type (§8) comes from
Scala and Pony, not Pharo.

## User Impact

**Newcomer (Python/JS/Ruby).** `uses: Comparable` next to `state:` reads like
Ruby's `include Comparable` and Python's `class V(Comparable)`, and the
required/provided split is the Java-interface-with-defaults they know. The
errors name the fix (`define it in C, or exclude one: …`). They discover it
by `Version usedProtocols`, `CompiledMethod origin`, and (post-v1) `browse` grouping `Comparable › Derived ordering`.
The one surprise is that `Integer conformsTo: #Comparable` is true without
`uses:` — the same structural surprise ADR 0068 already documents.

**Smalltalk developer.** This is Pharo traits with the operators spelled as
keywords and the conflict moved to compile time. Flattening, class-wins,
`super` = superclass, stateless + required accessors, same-origin harmless:
all Schärli. What differs: no `T` prefix, no runtime `traitConflict` method,
and a trait is written as a protocol with bodies. They would look for
`Trait named:` and find `Protocol define:`; for `TFoo` and find `Foo`.

**Erlang/Elixir developer.** A flattened method is an ordinary function in
the class's module — `observer`, `recon`, `dbg` and `Module:module_info()`
show it with no trait indirection, and Erlang callers dispatch to it like any
other. The protocol module is metadata only. `use`-style injection is what they
expect; the difference is that missing requirements are compile errors and
conflicts do not silently merge clauses. Behaviour callbacks map to
required methods. The concern they should raise: a dependency's trait
bodies are compiled into their modules, so a dependency upgrade that
changes a trait needs a rebuild to take effect (§10a).

**Production operator.** No new process, no new ETS lookup in the dispatch
path, no state-shape change on trait reload. The cost is N recompiles when a
trait used by N classes is edited — visible as N `ClassLoaded`
announcements, exactly like N method edits. Bytecode grows by one copy per
user; §Consequences quantifies. In a release, a trait edit changes every
user's module, so the appup lists each of them, and the user meta's trait
hash (§10) is what lets ADR 0125's tooling see why.

**Tooling developer.** `ProtocolDefinition` gains provided methods and
`uses:`, and `ClassDefinition` gains `uses: Vec<ProtocolUse>`; everything downstream sees a
flattened class with `origin` marks. Completion, hover, go-to-definition and
rename each need one branch on `origin`. Static analysis gets *easier*: a
trait method is analysed in each concrete context with concrete types.
The unparser (needed for flush and rename) must round-trip provided methods
in protocol bodies and `uses:` lines.

## Steelman Analysis

### Option A: Stateless flattened traits — `Protocol define:` with bodies + `uses:` lines (chosen)
- 🧑‍💻 **Newcomer**: "`uses: Comparable` is `include Comparable`. Required methods are the Java interface I already know. The error tells me the exact line to add."
- 🎩 **Smalltalk purist**: "This is Schärli's paper with keyword syntax. Flattening keeps the class the unit of meaning; `super` still means the superclass; conflicts are mine to resolve, not the runtime's to guess."
- ⚙️ **BEAM veteran**: "Flattened methods are plain functions in plain modules. No macro magic, no extra hop, and `Module:module_info(exports)` tells the truth."
- 🏭 **Operator**: "Zero dispatch cost and zero shape migrations. A trait edit is N method reloads I can already observe."
- 🎨 **Language designer**: "One construct — a protocol, which may provide bodies — so there is one body grammar, one namespace and one conformance rule. The type-system additions are small and local: `Self` substitution in provisions and an override check against a dropped provision. `overriding:` makes replacing inherited behaviour an explicit act, so a release on either side can't silently make a provision replace inherited behaviour, and errors in a provision are reported once, in the protocol file."

### Option B: Retroactive default bodies (every structural conformer receives them)
Provisions apply to every class that structurally conforms, with no `uses:` line, as Swift protocol extensions do.
- 🧑‍💻 **Newcomer**: "Write `max:` once and every ordered type has it, including ones I didn't write. No declaration to forget."
- 🎩 **Smalltalk purist**: "Duck typing all the way: if it has `<`, it's comparable, and it should get what comparable things get. A `uses:` line is a nominal island in a structural language."
- ⚙️ **BEAM veteran**: "No class-to-protocol build edges, no N recompiles on a protocol edit; the behaviour lives in one place."
- 🏭 **Operator**: "One module to reload when the protocol changes."
- 🎨 **Language designer**: "It is the only design where required ∪ provided and painless evolution both hold: adding a provision breaks nobody, because every conformer receives it."
- *Why rejected*: bodies land on classes that never asked, across packages. `Integer` would receive `Comparable>>max:` and change its tie semantics (§9). The body cannot be flattened into classes compiled before the protocol, so B needs Option E's shared module plus a dispatch step that checks conformance at run time on every miss. And exclusion and `overriding:` have no line to attach to. Swift's version resolves extension methods statically, which is how it avoids that run-time check; Beamtalk sends are dynamic, so it cannot.

### Option B″: Extension methods on a protocol (C#-style)
Allow `Comparable >> max: … => …` as an extension whose receiver is any class conforming to `Comparable`, as C# extension methods on an interface (the design behind LINQ) do.
- 🧑‍💻 **Newcomer**: "It's just an extension method with a protocol as the target. I already know `>>`."
- 🎩 **Smalltalk purist**: "Extensions are the Smalltalk way to add behaviour; widening their target from a class to a protocol is a small step."
- ⚙️ **BEAM veteran**: "One copy in one module, registered like today's foreign extensions."
- 🏭 **Operator**: "Nothing is copied into my classes; nothing recompiles when the protocol changes."
- 🎨 **Language designer**: "This is how a mainstream OO language added LINQ without traits: behaviour attached to an interface type, one definition, one location for errors and go-to-definition."
- *Why rejected*: C# resolves extension methods **statically**, from the declared type of the receiver expression, so there is no run-time cost and no ambiguity. Beamtalk sends are dynamic and the static type is often `Dynamic`, so a protocol extension would have to be found at run time by checking structural conformance on every lookup miss, and two protocols extending the same selector would be ambiguous at run time rather than a compile error. It is Option B with a different spelling.

### Option B′: One keyword, opt-in, required-only type (type rule rejected; single keyword adopted)
`Protocol define:` may carry bodies; a class opts in with `uses:`; the protocol's type is its *requirements* only, so provisions can be added without breaking structural conformers.
- 🧑‍💻 **Newcomer**: "It's exactly Java and Kotlin: the interface lists what I must write, and the defaults are extras."
- 🎩 **Smalltalk purist**: "A protocol is what you must implement; what you are given for free isn't the contract."
- ⚙️ **BEAM veteran**: "A behaviour's contract is its callbacks; what `__using__` injects isn't part of it."
- 🏭 **Operator**: "A dependency adding a default method can't change whether my classes conform."
- 🎨 **Language designer**: "It keeps everything A gets right — opt-in, flattening, class-wins — and makes a protocol evolvable without warnings anywhere."
- *Why the required-only type is rejected* (decided 2026-09-23, §Status 2): Java 8's evolvable-interface rule depends on nominal typing, where every implementer inherits the defaults. Under structural conformance a class with only `<` conforms without `uses:` and never receives `max:`. A required-only type admits it into `clamp: x :: Comparable …`, which then fails with `does_not_understand` at run time; required ∪ provided reports the same case as a conformance warning at the call site. B′'s evolution advantage is also smaller than it looks, since conformance problems are warnings (ADR 0068), so adding a provision breaks a build under A only where the project opts into `--warnings-as-errors`. It also makes provisions nearly useless to generic code, which can only safely send the required selectors.
- *What was adopted* (decided 2026-09-23, §Status 3): the single-keyword half. One `Protocol define:` that may carry bodies, with A's type rule, is Option A as this ADR now specifies it.

### Option C: Newspeak-style mixins (class-in-the-chain)
- 🧑‍💻 **Newcomer**: "Ruby's `include` and Dart's `with` work this way, and I already understand them."
- 🎩 **Smalltalk purist**: "Newspeak's insight that every class body is a mixin is deeper than traits — one concept for inheritance *and* composition."
- ⚙️ **BEAM veteran**: "Each application is a real module, so `super` chains naturally and hot reload is per mixin."
- 🏭 **Operator**: "Nothing is copied; one edit, one reload."
- 🎨 **Language designer**: "Mixins can carry state, which traits here cannot; and linearisation gives a deterministic answer to every conflict without asking the user."
- *Why rejected*: the "deterministic answer" is order-dependence, the thing Schärli's paper was written against. On BEAM each application is a class gen_server plus a module plus one more step in every chain walk (ADR 0032 removed cached tables precisely to keep walks short). Kind-crossing is worse, not better: a mixin carrying `state:` cannot be applied to a `Value`.

### Option H: Inheritance only (`Magnitude`, abstract enumerable Value class)
- 🧑‍💻 **Newcomer**: "One mechanism — classes — and I already know it."
- 🎩 **Smalltalk purist**: "This is what Smalltalk-80 and Pharo actually do for ordering: `Magnitude` has been the home of `<`, `between:and:`, `min:` and `max:` since 1980."
- ⚙️ **BEAM veteran**: "No compiler pass, no new module kind, no N-way recompile on edit."
- 🏭 **Operator**: "Nothing new to observe; hot reload of `Magnitude` is one module."
- 🎨 **Language designer**: "It removes most of the measured duplication with zero surface area. Add traits when the first actor needs enumeration, not before."
- *Why not chosen*: the arguments are correct about the stdlib's line count, but the design is wrong about what the hierarchy means. `Magnitude` and an abstract enumerable class are capabilities dressed as kinds (§Context, "Kinds versus capabilities"), and they spend each class's one superclass slot on them. It also fails `String`, every `Actor`, and every class whose superclass is already spent. Decided against on 2026-09-23 (§Status, decision 1).

### Option D: Extension methods only (`Comparable+DateTime.bt` per class)
- 🧑‍💻 **Newcomer**: "It already works today; I just copy a file."
- 🎩 **Smalltalk purist**: "Class extensions are the Smalltalk way to add behaviour without touching the hierarchy."
- ⚙️ **BEAM veteran**: "One ETS table, no new compiler pass."
- 🏭 **Operator**: "Nothing new to learn or monitor."
- 🎨 **Language designer**: "Composition without a new construct."
- *Why rejected*: extensions attach a body to one named class, so reuse across N classes is N copies. Nothing checks requirements, conflicts between two extension files are undetectable until load, and every extension call pays the ETS probe. The stdlib duplication in §Context is hand-written in class bodies, not extensions, so it is evidence for the problem, not for this option.

### Option E: Shared code + dispatch step (implementation variant of A)
Keep `uses:` opt-in, but compile each provision once, into the protocol's module, and reach it through a dispatch step, as foreign extensions (ADR 0066) are reached today.
- 🧑‍💻 **Newcomer**: "A protocol method lives in one place, so the debugger and stack traces show one place."
- ⚙️ **BEAM veteran**: "One copy of the bytecode; reload the protocol module and every user sees it, like a superclass method today. Foreign extensions already do this, for actors too, with a three-argument fun that threads the actor's state."
- 🏭 **Operator**: "N users, one reload, and no build-graph edges."
- 🎨 **Language designer**: "One definition: errors reported once, one go-to-definition target, trivial live patching, and none of §10a's invalidation machinery."
- *Why rejected*: shared code is possible, since foreign extensions prove it, but each provision needs one compiled body per class kind, every lookup miss pays the registry probe at each hierarchy level (the chain walk ADR 0032 kept short), and a shared body loses what the flattened method knows about its class: `super`, sealed self-sends, `late` guards and class-specific error hints. Block and state threading across that fun boundary is also where BT-1512 and BT-3580 live. The designer's strongest points, errors once and one source location, are kept by §3's source-location rule and §5's check-once rule, so flattening pays only the recompile cost (§Consequences).

### Option F: Stateful traits (Pharo 7 style)
- 🧑‍💻 **Newcomer**: "A `Counting` trait that can't hold `count` feels crippled."
- 🎩 **Smalltalk purist**: "Pharo added slots to traits because required accessors were boilerplate."
- 🎨 **Language designer**: "A trait that owns its state is more cohesive."
- *Why deferred*: state declarations are per-kind (ADR 0067), so a stateful trait is either kind-specific (and cannot cross the wall it exists to cross) or needs a fourth `ThreadedIr` storage input. No stdlib evidence needs it; §7 leaves the door open.

### Option G: Multiple inheritance
- 🧑‍💻 **Newcomer**: "Python does this: `class V(Ordered, Collection)`, with C3 ordering. I'd guess it."
- 🎩 **Smalltalk purist**: "Some Smalltalks experimented with it; the purist case is weak, which is itself telling."
- ⚙️ **BEAM veteran**: "Nothing on the BEAM helps or hinders it; it is a lookup-order question."
- 🏭 **Operator**: "One mechanism for all reuse, instead of classes plus protocols."
- 🎨 **Language designer**: "CLOS/Dylan-style MI with a linearisation is the general solution; traits are a restricted MI."
- *Why rejected*: ADR 0005 Q8 decided single dispatch and single inheritance; MI cannot cross class kinds; the diamond problem is exactly what traits were invented to remove.

### Tension points
- **Keyword count and type rule (A vs. B′).** B′ differed from A in keyword count and in whether provisions are part of the type. The type rule went to A, because under structural conformance B′'s rule trades a compile-time warning for a run-time `does_not_understand`. The keyword count went to B′: once the type rule was settled, a separate `Trait define:` only signalled whether a file had bodies, and changing a file's keyword when its first body is added is friction for no semantic gain (§Status 3).
- **One copy vs. many (A vs. E, B″).** A language designer values one definition with errors reported once. Flattening keeps that for diagnostics (§3, §5) and pays for it in recompiles; the shared-code designs keep it everywhere and pay on every send.
- **Evidence vs. modelling (A vs. H).** The stdlib count is mostly answerable by inheritance, so reviewers who weigh measured duplication alone would prefer H. The decision went to A because the hierarchy should record kinds and traits should record capabilities; H would encode ordering and enumeration as kinds.
- **One reload vs. N (A vs. E).** Operators want E's one reload. Resolved for A: flattened methods keep their class-bound facts and pay nothing per send; the N-recompile cost is accepted and bounded by the all-or-nothing reload (§11, §Consequences).
- **Stateless vs. stateful (A vs. F).** Newcomers and Pharo 7 users want F; the class-kind split makes A the only kind-neutral choice today. Deferred, not closed.
- **Header vs. body `uses:`.** Pharo puts it in the header; Beamtalk's header tail is already order-fragile and cannot hold exclusion/alias clauses. Body lines chosen.

## Alternatives Considered

### Inheritance only — `Magnitude` and an abstract enumerable Value class
Add `abstract typed Value subclass: Magnitude` (Pharo's own home for
`<`/`between:and:`/`min:`/`max:`) above `Number`, `DateTime`, `Duration` and
`Uuid`, and an abstract enumerable Value class above `Collection`,
`SupervisionTree` and `ChangeLog`. No new language surface; it removes most
of the §Context duplication today. Rejected as the *mechanism* because it
cannot serve `String` (ordered *and* a collection), any `Actor`, or any class
whose superclass is already spent (§Context, "What inheritance cannot fix").
It is also rejected as an interim stdlib refactor (decided 2026-09-23): it
would model capabilities as kinds (§Context, "Kinds versus capabilities")
and then have to be unwound when traits land.

### Extension methods on a protocol (C#-style)
`Comparable >> max: … => …` applying to every conformer, as C# extension
methods on an interface do. Rejected because C# resolves them statically
from the receiver's declared type, and Beamtalk's dynamic sends would need
a run-time conformance check on every lookup miss and would turn
cross-protocol ambiguity into a run-time question (§Steelman B″).

### Delegation (Kotlin `by`)
Forward a set of selectors to a member object: `Enumerable` by
`self.workers`. The snapshot `Enumerable` (§9) is this pattern expressed
as a protocol with provisions. A general delegation form would need a
per-kind way to name the delegate (a slot on actors and values, nothing on
`Object` subclasses) and adds no capability that `elements`-style required
methods lack. Not pursued.

### Protocols with default method bodies
`Protocol define: Comparable` gaining `=>` bodies. The **retroactive** form,
where bodies apply to every structural conformer, is rejected: it injects
methods into classes that never opted in (§Steelman B). The **opt-in** form,
B′ (`uses:` required, type = requirements only), is rejected for its type
rule (§Steelman B′, §Status 2). Its single-keyword syntax is adopted
(§Status 3).

### A separate `Trait define:` keyword
The earlier draft of this ADR: traits as their own top-level form beside
`Protocol define:`, with identical body grammar. Rejected (2026-09-23,
§Status 3): with the type rule settled it differs from the chosen design
only in readability, and it makes a protocol file change keyword when it
gains its first body.

### Newspeak / Ruby / Dart / Scala mixins
Linearised application inserting a class per use. Rejected: order-dependent
conflict resolution; a registry gen_server and module per application; one
more step in every chain walk; cannot cross class kinds if it carries state
(§Steelman C).

### Extension methods only
Status quo, one `Comparable+DateTime.bt` per user. Rejected: it is what produced the
duplication in §Context (§Steelman D).

### Shared trait module + dispatch step
Foreign-extension-style ETS lookup of trait funs. Rejected for the lexical
self-send, dual calling convention and per-send cost (§10, §Steelman E).

### Stateful traits
Deferred (§7, §Steelman F).

### Multiple inheritance
Rejected by ADR 0005; see §Steelman G.

### `uses:` as a header clause
`Value subclass: DateTime native: beamtalk_datetime uses: Comparable`. This is
Pharo's spelling and matches `native:`. Rejected because the header tail is
already order-sensitive (the docs warn that reversing `native:` and
`handleScope:` silently drops one), several traits with `excluding:` and
`aliasing:` do not fit on a line, and the rejected-but-precedent ADR 0042
sketch already used the body-line form.

### Pharo's `+ - @` operators
`uses: TA + TB - {#x} @ {#y -> #x}`. Rejected: the class body is declaration
context, not expression context, and `@` is the package separator in
`json@Parser`. Keyword clauses with literal lists/dictionaries need no new
tokens.

### Runtime conflict methods (Pharo's `traitConflict`)
Pharo installs a method that raises on call when a conflict is unresolved.
Rejected: Beamtalk compiles classes; a conflict is known at compile time and
should be an error there, per the diagnostic policy of ADR 0100.

### `Trait.super` / `A.super.m()` for calling an overridden provision
Java 8's escape hatch. Rejected in favour of writing the method in the class
(v1) and `aliasing:` (post-v1) (§4): both keep the flattening property
(nothing trait-shaped survives compilation) and need no new send form.

### Do nothing
The BT-274 cancellation position ("the class hierarchy handles behavior
sharing adequately for current needs"). The stdlib count alone does not
refute it: v1 removes about eight enumeration forwarders from
`SupervisionTree` and `ChangeLog`, and adds `between:and:`, `min:` and
`max:` to four classes that lack them today. The ordering operators stay,
and the `Integer`/`Float` `min:`/`max:` copy is fixed by inheritance.
Rejected because the value is the mechanism, not the line count:
capabilities should not be modelled as kinds, and the cases inheritance
cannot reach at all — two concerns on one class, the actor/value wall, and
a spent superclass slot — are permanent properties of the language.

## Consequences

### Positive
- Cross-kind behavioural reuse with **no runtime cost**: a flattened method
  is indistinguishable from a hand-written one at dispatch time.
- `ThreadedIr`, `super`, `^`, sealed self-send optimisation, `late` guards,
  xref, ADR 0105 re-check, ADR 0114 rename and ADR 0123 migration need **no
  new modes** — they see ordinary methods, and traits can never change a
  shape.
- Conflicts and missing requirements are **compile errors with a named fix**,
  where Pharo gives a runtime error and Elixir gives a warning.
- **No silent override by a provision** (§3a). In a closed-world build,
  neither a new provision nor a new superclass method can make a provision
  replace an inherited method (outside the two allowlisted printing defaults)
  without an error at that class's next build. That removes the
  fragile-base-class problem plain trait precedence adds. It does not close
  the pre-existing case of a class-body method silently overriding a newly
  added superclass method (§3a).
- **One construct.** Protocols gain bodies; there is no new top-level
  keyword, file kind or registry, and ADR 0068's structural rule is
  untouched for protocols without provisions (§8).
- `between:and:`/`min:`/`max:` arrive on `DateTime`, `Duration`, `Uuid` and
  `String`, and `Enumerable` retires most of the `SupervisionTree`/
  `ChangeLog` forwarders. The stdlib line-count saving is small; the value
  is the mechanism, for user code and for classes that need two concerns.
- Protocol → trait conversion that gives existing signatures bodies is
  source-compatible (§8).

### Negative
- **One more clause to write.** A trait provision that replaces an
  inherited, non-root method needs `overriding:` (§3a), and a trait or
  superclass release that introduces such an overlap breaks downstream
  builds until each user decides. That is intended; the cost is the break.
- **Bytecode duplication**: one copy of each provided method per user. For
  `Comparable` (three flattened methods × four stdlib users) and
  `Enumerable` (about ten × two) this is a few KB. A trait used by hundreds of classes would be measurable;
  the mitigation is that provided methods are usually one-liners over
  required ones.
- **N recompiles on trait edit** (§11). Editing a trait recompiles every
  source-backed user. Per-file compiles are fast and already the unit of
  `>>` live patching; the workspace loader must batch them and announce once
  per user.
- **Reflection must say where a method came from** or `implementorsOf:` looks
  wrong. `origin` on `CompiledMethod` and xref rows is mandatory, not
  optional; grouping by trait in browse follows post-v1.
- **No stateful traits** — the `Counting` example needs two required
  accessors the user must write. Deferred with a stated extension path.
- **Adding a provision to a trait is a breaking change** for classes that
  conform to its protocol structurally without `uses:`. They get conformance
  warnings, which fail the build only under `--warnings-as-errors` (§8,
  decided §Status 2).
- **Trait bodies are compile-time ABI across packages** (§10a). A
  dependency's trait change reaches downstream code only on rebuild; the
  meta hash makes staleness visible but does not remove it.
- **Every build path gains a dependency edge** (§10a): the CLI cache key,
  `build_stdlib`'s pre-pass, the compiler port and the LSP index all change.
  This is the largest hidden cost of flattening.
- **Block-taking provisions on actors depend on BT-3580.** Until it is
  fixed, traits whose provisions pass blocks between each other through
  self-sends must not be used by actors; the stdlib `Enumerable` avoids the
  pattern (§9).
- **The unknown-keyword-ends-body behaviour becomes an error.** Any existing
  source relying on a stray keyword line to terminate a class body (none
  known in the repo) would break; this is a fix, not a regression.

### Neutral
- `uses:` is a composition declaration, not a conformance declaration.
  ADR 0068's rejection of `implements:` stands; `conformsTo:` remains
  structural and can be true without `uses:`.
- Protocol modules, with or without provisions, exist only for reflection
  and registration; they contain no callable methods. `beamtalk_dispatch` is unchanged.
- Method categories remain source-only (`method_category.rs`); flattened
  methods take their trait's dividers.
- Extension methods and traits compose: an extension may target a using
  class for any selector the flattened body does not define. A file-level
  `P >> sel =>` extension on a protocol is an error. At the REPL,
  `P >> sel =>` edits the protocol's own source: it adds or replaces a
  provision, is recorded in the ChangeLog as a protocol patch, and is
  flushed to the protocol file (§11).
- **`Comparable`'s required `<` is its most permanent API**, and is decided
  (§Status 7): changing the requirement later would break every user, so a
  three-way comparison, if wanted, arrives as a separate protocol.
- `Comparable` has no equality requirement: `=:=` is hard-lowered and cannot
  be overridden (`date_time.bt:291-296`). Where `<` compares something
  coarser than structure (`DateTime` compares instants, while `=:=` is
  structural), the trait gives a total *preorder*. A three-way `compare:`
  requirement is a possible follow-up.
- `uses: Comparable` does not change `List sort`, which is Erlang term order
  (`primitives/list.rs:92`), or the `@primitive` `Collection max`/`min`.
  Ruby users will expect `sort` to follow `<`; documenting the difference is
  part of Phase 6.
- Stdlib `Comparable` and `Enumerable` take names that user code may already
  use for its own protocols (the language guide's own example is
  `Protocol define: Comparable`, `beamtalk-language-features.md:1866`). See
  §Migration Path.

## Implementation

Sized for `/plan-adr`. Phases 1–3 are the compiler; 4–5 the runtime and live
system; 6 adoption. Phase 0 is a half-day spike that de-risks the one
assumption this ADR could not verify from source.

| Phase | Scope | Components | Size | Depends on |
|---|---|---|---|---|
| 0 | **Spike**: (a) confirm inherited actor method self-send binding (lexical `<module>:safe_dispatch` vs. `__class_mod__`), since §6 rests on self-sends resolving in the *using* class's module; (b) re-run the BT-3580 probe and record whether block-taking actor provisions are safe. Record both in `docs/development/debugging.md` | runtime, codegen (read-only) | S | — |
| 1 | **Syntax**: `ProtocolDefinition` gains `provided_methods` (`ast/class.rs`; a `uses:` line in a protocol body is a "not yet supported" error in v1); `ClassDefinition.uses: Vec<ProtocolUse { protocol, type_args, excluding, overriding, span }>`; `parse_protocol_body` accepts provided methods (a signature followed by `=>`) through the class method parser, and reserves `uses:`/`excluding:`/`overriding:`/`aliasing:` (`aliasing:` parses to a "not yet supported" error until Phase 7); `package@Protocol`; `uses:` in the class-body loop with ordering and unknown-keyword errors; unparse round-trip; no new top-level form, so one-definition-per-file and ADR 0119 naming are unchanged; lexer nothing (contextual keywords) | `beamtalk-core` source_analysis, ast, unparse | M | — |
| 2 | **Semantics**: `protocol_registry.rs` extended with provided signatures; `trait_expansion.rs` implementing §3–§5 as a new pass with explicit inputs (expansion before `ClassHierarchy`, requirement check after; exclusion, conflict, class-wins, same-origin rule, synthesised accessors ranked as class body); `MethodInfo.origin` with `defined_in` left as the using class; `ProtocolInfo` conformance over required ∪ provided; hygienic type-param and `Self` substitution; reserved-selector and override-compatibility checks; the §3a unacknowledged-override check with kind-root exemption and stale-entry warning; statelessness validator (§7); all §13 diagnostics; `typed` check on the flattened class; name resolution in the protocol's package (§3); provisions checked once in the protocol with the self-send rule (§5); `excluding:` conformance warning; `extending:` of a protocol with provisions (§5); same-origin exemption and kind-root allowlist (§3a); diagnostics tagged with the protocol's source identity and de-duplicated across users (§3) | `beamtalk-core` semantic_analysis, type_checker | L | 1 |
| 3 | **Codegen & build graph**: feed the flattened `ClassDefinition` to the existing generators (no change to `merge_method`); `methodXref` provenance/origin; `methodSource` as the trait's source slice; `uses => [{Name, Hash}]` in user meta; protocol module emission extended with provisions (`generate_protocol_registrations`, `'__beamtalk_protocol_source'/0`); every §10a entry point (CLI cache key, `build_stdlib` protocol pre-pass keeping full ASTs and `generated_builtins.rs`, compiler port, `dependency_classes.rs`, `ProjectIndex` edges); conformance fixture for the extended protocol registration shape; codegen maps lines per method source so BEAM line annotations for flattened methods point at the protocol file (§3) | `beamtalk-codegen`, `beamtalk-compiler-port`, `beamtalk-cli`, `beamtalk-language-service`, `build_stdlib` | L | 2 |
| 4 | **Runtime & reflection**: `beamtalk_protocol_registry.erl` gains provided selectors and a users index; `Protocol providedMethods:`/`usersOf:`; `Behaviour usedProtocols/allUsedProtocols/usesProtocol:`; `CompiledMethod origin`; `SystemNavigation usersOf:`; xref `provenance := protocol`; `removeSelector:` guard (§11); `beamtalk_xref_methods` schema bump; surface-parity table rows | runtime, stdlib, `docs/development/surface-parity.md` | M | 3 |
| 5 | **Live system**: trait file reload → two-stage user recompile fan-out in the workspace loader (compile all, load only if all succeed, roll back loaded modules on a load failure; source-backed, non-stdlib users only; stdlib traits read-only); `Describable >> sel => …` and `removeSelector:` live patching on protocols; required-selector rename/remove refusal and `save-section` routing (§11); provided methods in `Protocol define:` at the REPL; ADR 0105 re-check hookup; flush of protocol files (ADR 0113); LSP go-to-definition and completion on `origin`; REPL-protocol tests | workspace, REPL, LSP | L | 4 |
| 6 | **Stdlib adoption** (one issue per trait): `Comparable` on `DateTime`, `Duration`, `Uuid`, `String` (each keeps its primitive operators); `Enumerable(E)` on `SupervisionTree` and `ChangeLog` per §9 (`ChangeLog` keeps its all-entries `select:`, and keeps its public `notEmpty` as the one-line method `notEmpty -> Boolean => self isNotEmpty`; `SupervisionTree` keeps its `do:`); separately, move `Integer`/`Float` `min:`/`max:` up to `Number` (inheritance, not traits); BUnit tests in `stdlib/test/`; `docs/beamtalk-language-features.md` § Traits; close ADR 0005 Q9; add `elements -> List(E) => self asList` to `Collection` so every collection conforms to `Enumerable`; reconcile `SupervisionTree do:`'s return type before adoption (the stdlib builds with `--warnings-as-errors`); list the selectors added per class in the release notes (§Migration Path) | stdlib, docs | M | 5 |
| 7 | **Post-v1** (one issue each, any order): a protocol `uses:`-ing another (transitive expansion, cycle check, protocol-internal precedence, same-origin diamonds, §3 step 1, §4, §3a); `aliasing:` (parser clause, alias-before-exclude semantics, conflict rules, §2/§4); class-side provisions (§9); browse grouping by trait (§12); protocol-wide rename (ADR 0114 `renameSelector:to:` redirection and protocol `renameTo:` with `uses_protocol` reference rows); LSP hover provenance | core, workspace, LSP | M | 6 |

**Tests per phase.**

| Phase | Suites |
|---|---|
| 0 | A BUnit fixture in `stdlib/test/` pinning the observed self-send binding: a subclass override called from an inherited actor method |
| 1 | Parser unit tests and snapshots in `beamtalk-core`, unparse round-trip, and diagnostics for misplaced `uses:` and unknown keyword lines |
| 2 | Semantic-analysis unit tests for each §13 row, both §3a directions (trait grows, superclass grows), class-wins, `Self` substitution, and generic `E` substitution, a type error in a provision shared by two users reported once at the protocol's line, a self-send outside the declared set, and a same-named class in the user's package not capturing a provision's reference |
| 3 | `test-package-compiler` codegen snapshots for one user of each class kind, `just verify-threaded-ir` over the flattened stdlib, and the trait-meta conformance fixture |
| 4 | Runtime EUnit for the extended `beamtalk_protocol_registry`, BUnit reflection tests (`usedProtocols`, `origin`, `usersOf:`), and xref tests for `provenance := protocol` |
| 5 | `tests/repl-protocol/cases/` for trait reload fan-out, a rejected edit that loads nothing and names the failing user, `Describable >> sel` live patching, `removeSelector:` refusal, and rename; plus LSP tests |
| 6 | Existing `just test-stdlib` and `just test-bunit` stay green with the duplicates deleted, plus new BUnit tests for `between:and:`/`min:`/`max:` on each new user |
| 7 | Per feature: protocol-uses-protocol expansion, cycles and same-origin diamonds; parser and semantic tests for `aliasing:`, BUnit for class-side provisions, REPL-protocol tests for rename and browse grouping |

Phase 1 alone is mergeable (a parsed but unexpanded `uses:` is a "not yet
supported" error); phases 2–3 together give a working compiler; phase 4 is
required before the feature is documented as available, because reflection
without `origin` misleads.

**v1 scope (decided 2026-09-23, §Status 4).** Phases 0–6 as listed above
make up v1. Phase 7 is the post-v1 work.

| In v1 | Post-v1 (Phase 7) |
|---|---|
| `Protocol define:` with provided methods; `uses:` with `excluding:` and `overriding:` (§3a is not optional: without it v1 would ship the silent-override problem it exists to prevent) | `aliasing:` (§2); `ChangeLog` keeps `notEmpty` as a one-line method instead |
| Flattening, conflicts, requirements, reserved selectors, statelessness, `Self` and type-parameter substitution | Class-side provisions (§9, `Versioned`) |
| Required ∪ provided type (§8) | Browse grouping by trait (§12) |
| All-or-nothing reload of every user (§11) | Protocol-wide rename across all users (ADR 0114 redirection, protocol `renameTo:`) |
| Live patching: `P >> sel => …` and `P removeSelector:` (principle 11) | LSP hover "from Comparable" polish beyond go-to-definition |
| Users index; `Protocol usersOf:`/`providedMethods:`; `Behaviour usedProtocols` (principle 8, and the reload fan-out's source of targets) | |
| `origin` on `CompiledMethod` and xref rows; refusal to rename or remove a trait-provided method on one class | |
| Stdlib: `Comparable` on `DateTime`, `Duration`, `Uuid`, `String`; `Enumerable` on `SupervisionTree`, `ChangeLog` | |

`aliasing:` returns when a second use appears beyond `ChangeLog notEmpty`.
Every post-v1 item is additive: none changes the meaning of v1 code.

Deferred to follow-up ADRs, explicitly: stateful traits (§7); trait-level
modifiers; `sealed` traits for whole-program optimisation (BT-274's last
criterion — moot while flattening already gives per-class sealing).

## Implementation Tracking

**Epic:** [BT-3586](https://linear.app/beamtalk/issue/BT-3586)
**Status:** Planned

| ADR phase | Issue | Title | Size | Blocked by |
|---|---|---|---|---|
| 1 | [BT-3587](https://linear.app/beamtalk/issue/BT-3587) | Parse provided methods in `Protocol define:` and `uses:` lines in class bodies | M | — |
| 2 | [BT-3588](https://linear.app/beamtalk/issue/BT-3588) | Flattening pass; protocol type is required ∪ provided | M | BT-3587 |
| 2 | [BT-3589](https://linear.app/beamtalk/issue/BT-3589) | Semantic checks: requirements, `overriding:`, provision self-sends, protocol-side rules | M | BT-3588 |
| 0, 3 | [BT-3590](https://linear.app/beamtalk/issue/BT-3590) | Codegen with protocol source identity and package-scoped name resolution | M | BT-3588 |
| 3 | [BT-3591](https://linear.app/beamtalk/issue/BT-3591) | Build graph: protocol → user edges in every compile path | M | BT-3590 |
| 4 | [BT-3592](https://linear.app/beamtalk/issue/BT-3592) | Runtime users index and reflection API | M | BT-3590 |
| 5 | [BT-3593](https://linear.app/beamtalk/issue/BT-3593) | All-or-nothing reload and live patching | M | BT-3591, BT-3592 |
| 6 | [BT-3594](https://linear.app/beamtalk/issue/BT-3594) | Stdlib adoption, Traits docs, end-to-end test | M | BT-3589, BT-3593 |
| 7 | [BT-3595](https://linear.app/beamtalk/issue/BT-3595) | Post-v1 (needs-spec) | L | v1 |

Related: [BT-3580](https://linear.app/beamtalk/issue/BT-3580) (actor self-send block bug; not blocking).

## Migration Path

Additive for language semantics; three notes for adopters:

- **Stray keyword lines** in a class body were silently treated as end-of-body
  and become an error (§13). `just test-stdlib` and the parity projects will
  surface any instance.
- **Giving a protocol provided methods** is an edit to the protocol file:
  add `=>` bodies to existing signatures, with no consumer changes (§8),
  then add `uses:` to the classes that should receive them. Adding *new*
  provisions adds conformance warnings for structural conformers (§8).
- **User extensions that Phase 6 turns into errors.** An extension such as
  `DateTime >> max:` or `String >> between:and:` (plausible today, since
  those methods are missing) becomes the ADR 0066 "cannot override a method
  defined in the class body" error once `Comparable` is flattened in. The
  release notes must list the added selectors per class.
- **New stdlib names.** Stdlib `Comparable` and `Enumerable` collide with any
  user protocol or class of the same name through the existing
  class/protocol name-collision error. Phase 6 must check the known
  application corpora and the docs' own `Protocol define: Comparable`
  example before landing, and the release notes must name both.

## References

- Related issues: BT-3526 (this ADR), BT-3523 (epic), BT-274 (original
  design issue, cancelled; superseded here), BT-3580 (actor self-send block
  NLR and captured-local bug found in review), BT-102 (method combinations —
  a flattened trait method combines like any other), BT-105 (sealing)
- Related ADRs: ADR 0005 (Q8/Q9: single dispatch, composition deferred),
  ADR 0006 (unified dispatch), ADR 0013/0048 (class-side syntax in force),
  ADR 0025 (gradual typing; rejected `implements:`), ADR 0032 (chain walk,
  no flattened tables), ADR 0036 (metaclass tower), ADR 0037 (collection
  hierarchy; Option C), ADR 0040 (one class per file), ADR 0042 (rejected
  `uses: Mutable` sketch), ADR 0050 (`__beamtalk_meta`, class hierarchy),
  ADR 0066 (extension methods; self-extension fold), ADR 0067 (class kinds),
  ADR 0068 (protocols; rejected nominal conformance), ADR 0083 (metaclass
  inference), ADR 0087/0115 (xref), ADR 0100 (open-world diagnostics),
  ADR 0070 (package-qualified names), ADR 0071 (`internal`), ADR 0101
  (`native:`), ADR 0104 (typed actor protocols — trait methods on an actor
  are part of its message interface), ADR 0105 (recheck on reload), ADR 0111/0118/0120/0122
  (`ThreadedIr`), ADR 0112 (`removeSelector:`), ADR 0113 (flush), ADR 0114
  (rename), ADR 0119 (module naming), ADR 0123 (versioned state), ADR 0124
  (`late` slots), ADR 0125 (releases — trait hashes in user meta),
  ADR 0126 (distribution — flattened methods are ordinary methods, so
  remote dispatch needs nothing trait-specific)
- Documentation: `docs/beamtalk-language-features.md` § Structural Protocols,
  § Extension Methods, § Class Modifiers; `docs/beamtalk-syntax-rationale.md`
  § Type Alias Declaration (honesty of form); `docs/beamtalk-principles.md`
  §6, §8, §11; `docs/development/architecture-principles.md` §6–7
- Schärli, Ducasse, Nierstrasz, Black — *Traits: Composable Units of
  Behaviour*, ECOOP 2003. Ducasse, Nierstrasz, Schärli, Wuyts, Black —
  *Traits: A Mechanism for Fine-grained Reuse*, TOPLAS 2006. Bergel, Ducasse,
  Nierstrasz, Wuyts — *Stateful Traits*, 2007. Bracha — *Newspeak Programming
  Language Draft Specification* (mixins). Pony tutorial — *Traits and
  Interfaces*.
