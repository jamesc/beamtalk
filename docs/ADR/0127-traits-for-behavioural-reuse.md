# ADR 0127: Traits — Stateless, Flattened Units of Behaviour

## Status
Proposed (2026-09-22)

Closes the question ADR 0005 left open as Q9 ("Mixins/traits: deferred to a
future ADR") and BT-274 (cancelled 2026-04-01 as "not pressing"; reopened by
BT-3526 with the corpus evidence below).

The decision in one paragraph: a **trait** is a named, stateless bundle of
*required* method signatures and *provided* method bodies, defined with
`Trait define:` in the same body syntax as `Protocol define:` (a signature
without `=>` is a requirement, one with `=>` is a provision). A class composes
traits with `uses:` lines at the top of its body. Composition is
**flattened at compile time**: the using class is compiled exactly as if the
provided methods were written in its own body, so `ThreadedIr`, the class-kind
calling convention, `super`, sealed self-send optimisation, xref, and hot
reload all see ordinary methods. A class's own method beats a trait's, and a
trait's beats an inherited one **only when the using class says so** with an
`overriding:` clause (§3a), so neither a trait release nor a superclass
release can silently replace behaviour. Two traits providing the same
selector is a compile error unless the class defines it or excludes one. Every trait also
*is* a protocol of the same name (its required ∪ provided selectors), so the
structural conformance rule of ADR 0068 is unchanged and `uses:` never
becomes a nominal `implements:`.

**Open decisions for the author** (raised by review; the draft takes the
first position in each and says where the alternative is written up):

1. ~~**Now, or inheritance first?**~~ **Decided (2026-09-23): traits now.**
   `Magnitude` and an abstract enumerable class would remove most of the
   measured duplication, but they model *capabilities* as *kinds*
   (§Context, "Kinds versus capabilities"). The stdlib is not refactored
   into an inheritance-based interim first; Option H is rejected.
2. ~~**The implied protocol's selector set.**~~ **Decided (2026-09-23):
   required ∪ provided** (§8). The required-only alternative (Option B′)
   borrows Java 8's rule, which works only because Java is nominal: every
   implementer inherits the defaults. Under structural conformance a class
   can conform without `uses:` and so without the provisions, and a
   required-only type would let it through to code that then sends
   `max:` and fails with `does_not_understand` at run time. Required ∪
   provided reports that case as a conformance warning instead.
3. **One keyword or two** — `Trait define:` beside `Protocol define:` (draft)
   or one `Protocol define:` that may carry bodies, opted into with `uses:`.
   With decision 2 settled this is a readability question only: the
   semantics are identical either way.
4. **v1 scope** — the draft specifies `aliasing:`, class-side traits, live
   patching of trait methods, and a runtime trait registry. A minimal v1
   (`excluding:` and `overriding:` only, file-reload editing, reflection
   via xref `origin`)
   would cut Phases 4–5 roughly in half (§Implementation).

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
   (ADR 0104). Traits whose provisions do not pass blocks through self-sends
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
  Traits do (Pharo's `Trait named:`; §12 below), so `Trait define:` is honest.
- **No duplicate rule implementations** across Rust and Erlang
  (`architecture-principles.md` §6–7). The flattening, precedence and
  conflict rules live in one place; the runtime consumes their *result*.

## Decision

### 1. Defining a trait — `Trait define:`

```beamtalk
// stdlib/src/comparable.bt
// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

/// Comparable — a total order derived from one required `<`.
///
/// A using class implements `<`; everything else is provided. A class that
/// already implements every selector below conforms to the `Comparable`
/// protocol whether or not it uses the trait (ADR 0068).
Trait define: Comparable
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
| `class selector …` (either shape) | The same, on the class side (ADR 0013 `class` prefix, the syntax in force while ADR 0048 is deferred) |
| `sealed` / `internal` before the selector | **Rejected in v1** (§13). `sealed` cannot stop the using class from overriding (class wins, §3), and `internal` in a trait has no well-defined package once flattened into a user in another package (ADR 0071) |
| `uses: OtherTrait …` | Trait composition (§2) — a trait may use traits |
| `// === Name ===` | Category divider (`method_category.rs`), kept with the trait's source |
| `/// doc` | Doc comment, carried onto the flattened method and shown by `browse` |

A trait may be generic — `Trait define: Enumerable(E)` — with the same
type-parameter syntax as classes and protocols.

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
`Self` unsubstituted; they are checked like protocol requirements (§5).

A trait has no header modifiers, no superclass, no `state:` / `field:` /
`classState:` (§7), and no `native:`. A `.bt` file holds one trait as its
sole top-level definition, named like a class file (`comparable.bt`), and
compiles to one module by the ADR 0119 rule.

Trait names follow protocol naming (`Comparable`, `Enumerable`,
`Printable`), not Pharo's `TComparable` — Beamtalk is "Smalltalk-like, not
Smalltalk-compatible", and the trait *is* its protocol (§8), so the name
should read as a type.

### 2. Composing traits — `uses:` lines in the class body

```beamtalk
sealed typed Value subclass: DateTime native: beamtalk_datetime
  uses: Comparable
  field: …

  < other :: DateTime -> Boolean =>
    (Erlang beamtalk_datetime) lt: self with: other
  // `>`, `<=`, `>=` deleted — provided by Comparable.
  // `between:and:`, `min:`, `max:` gained.
```

Grammar of a `uses:` line:

```
uses: [package@]TraitName[(TypeArgs)] [excluding: #(#sel, …)] [overriding: #(#sel, …)] [aliasing: #{#newSel => #traitSel, …}]
```

`package@` qualifies a trait from another package, as `json@Parser` does for
a superclass (ADR 0070). Inside a trait body, `uses:`, `excluding:` and
`aliasing:` are reserved: today `parse_protocol_method_signature_with_doc`
would read `uses: Comparable` as a required keyword signature with a
parameter named `Comparable`, so the trait-body parser must check for them
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
- `aliasing:` adds each trait-provided `#traitSel` to the class *also* under
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

1. Expand each used trait transitively (a trait's own `uses:` first).
2. Per `uses:` line, apply `aliasing:` first, reading from the trait's
   *original* provisions, then `excluding:`. An alias therefore survives
   the exclusion of the selector it copies, which is what
   `excluding: #(#printString) aliasing: #{#describeString => #printString}`
   (§4) relies on.
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
`MethodInfo.origin: Option<TraitName>` records provenance for reflection,
diagnostics and tooling, and `defined_in` keeps meaning "the class whose
module holds the code", which after flattening is the user.

**Type-parameter substitution and hygiene.** Expansion substitutes the
`uses:` type arguments for the trait's type parameters
(`uses: Enumerable(Worker)` replaces `E` with `Worker`) and `Self` as in §1.
It must be **hygienic**: a trait provision's method-local type variables are
alpha-renamed before substitution, so `Pair(A, B) uses: Enumerable(A)` does
not capture the `A` in `inject: initial :: A into: …` as the class's `A`.

**Synthesised methods rank as class body.** A `Value` class's auto-generated
field accessors and `with*:` setters are suppressed today whenever
`class.methods` already has the selector (`value_accessors.rs:63-92`). A
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

- **Kind roots are exempt.** Methods inherited from `ProtoObject`,
  `Object`, `Value` or `Actor` (and, class side, `Behaviour`, `Class` and
  `Metaclass`) are defaults meant to be replaced, such as `printString`,
  `displayString` and `hash`. Requiring `overriding:` for them would put the
  clause on nearly every use of `Printable`-like traits and teach users to
  write it without reading it.
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
- **Diamonds.** When one provision reaches the class through several
  `uses:` lines (the same-origin case of §4), acknowledging it on any one of
  those lines is enough. Listing it on more than one is allowed and is not
  stale.
- **Only provisions are checked.** A method written in the class body
  overrides an inherited one silently, as it does today; the class's own
  source is where that decision is visible.
- **Timing.** The check needs the superclass chain, so it runs in the
  post-`ClassHierarchy` half of the pass, beside the requirement check (§3,
  §5).

### 4. Conflicts and their resolution

Two used traits providing the same selector on the same side is a **compile
error** in the using class, unless one of:

- the class body defines that selector (the class wins, §3 step 4);
- one use excludes it (`excluding:`);
- both provisions are the *same* method reached through two paths (a trait
  used directly and via another trait). Same origin is not a conflict —
  Schärli's rule, which keeps diamond-shaped trait composition harmless.

An aliased copy is a provision like any other and can conflict.

```beamtalk
Value subclass: Report
  uses: Printable
  uses: Describable
  // error: `printString` is provided by both Printable and Describable in Report.
  //   Define `printString` in Report, or exclude one:
  //     uses: Describable excluding: #(#printString)
```

There is no "call both" or `Trait.super` (Java 8's `A.super.m()`). A class
that wants both bodies keeps one under an alias and calls it:

```beamtalk
Value subclass: Report
  uses: Printable
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
Trait define: Counting
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
   able to use class-side traits (§9).

**Selectors a trait may not provide.** Some selectors change what the
compiler or runtime does with a class, not just what it answers, so a trait
providing them would change a class's shape or dispatch behind a `uses:`
line. A provision (not a requirement) of any of these is an error (§13):
`initialize`, `migrateFromV<N>:` (ADR 0123's migration chain,
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

### 8. A trait is also a protocol

Every trait `T` registers a protocol `T` whose required selectors are `T`'s
**required ∪ provided** selectors, instance and class side. Consequences:

- `x :: Comparable` in a type position, `Collection(Object) & Comparable`
  intersections, and `Logger(T :: Comparable)` bounds all work with no new
  type-system concept. Traits and protocols share the class/protocol
  namespace; `Trait define: Printable` next to `Protocol define: Printable`
  is the existing name-collision error.
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
- **Converting a protocol into a trait by giving *existing* signatures
  bodies is source-compatible** for every consumer: `Printable`'s selector
  set does not change, so every `:: Printable` annotation, `conformsTo:`
  query and structurally-conforming class is unaffected. **Adding a *new*
  provision is not**: it enlarges the implied protocol, so every class that
  conformed structurally without `uses:` stops conforming. That is the same
  break as adding a required selector to a protocol today (ADR 0068), and it
  is the price of the required ∪ provided rule (decided, §Status 2). The
  cost is smaller than "breaking" suggests: ADR 0068 makes conformance
  problems warnings, not errors, so adding a provision never fails a build.
  It adds warnings exactly where a structural conformer reaches code typed
  against the trait, which are the places that would fail at run time if
  that code sent the new selector. This matches TypeScript, where adding a
  member to an interface is a breaking change for structural implementers
  and is reported, not silently accepted.
  The reverse conversion (trait → protocol) breaks users' `uses:` lines and
  is an ordinary deprecation.
- **An override of a provision is checked against it.** Class-wins drops the
  provision in step 4, but its signature is kept for one check: the class
  body's method must be override-compatible with it, by the same rule used
  for overriding an inherited method, and a mismatch is a warning. Without
  this, a class could replace `Enumerable>>select: -> List(E)` with
  `select: -> Set(E)` while the implied protocol still promised `List(E)`.

`extending:` on protocols and `uses:` on traits are the same operation at
different levels (contract composition vs. implementation composition) and
stay separate keywords, because a protocol cannot use a trait (it would
acquire bodies) and a trait using a protocol would only add requirements,
which is what listing them does already.

### 9. All three class kinds can use traits

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
Trait define: Enumerable(E)
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
  idle -> List(Worker) => self select: [:w | w isIdle]
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
  (ADR 0104). Captured-variable writes in it do not reach the caller, and a
  re-entrant send to `pool` from inside it deadlocks or raises
  `calling_self`. Callers that want caller-side semantics use
  `pool elements select: [...]`, and the trait makes `elements` the
  documented way in.
- **Speed.** Each provision is one primitive `List` call, not an
  `inject:`/`addFirst:`/`reversed` rebuild.

Adoption is per class, not mechanical. `SupervisionTree` and `ChangeLog`
are one-line forwarders to an inner list already, so they become
`elements => self nodes` and `elements => self activeEntries`. But
`ChangeLog select:` deliberately ranges over *all* `entries`, including
orphans (`change_log.bt:36-44, 96-106`), so it keeps its own `select:`
(class wins), and `SupervisionTree do:` answers `self`, not `nil`, so it
keeps its own `do:` and the override check (§8) warns until its return type
is reconciled. **`Collection` does not adopt `Enumerable` in v1**: its
`select:`/`collect:` answer `Self` via `species`, and every enumeration
method it has is already inherited by its subclasses.

**Object** — instance methods on an uninstantiable class are pointless, so
`Object subclass:` users compose **class-side** traits:

```beamtalk
Trait define: Versioned
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

### 10. Codegen — flatten into the user; a trait module for reflection

**Using class.** Nothing new. The flattened `ClassDefinition` (with the trait
`MethodDefinition`s merged in, provenance-marked) goes through the existing
actor / value / class-side generators. The only codegen-visible additions
are (a) `methodXref` rows for flattened methods carry
`provenance := trait, origin := TraitName`; (b) `methodSource` for a
flattened method is the **trait file's source slice for that method**, taken
from the trait's span and not unparsed after substitution, so `browse` and
`CompiledMethod source` show the real code and the ChangeLog does not see a
spurious difference; and (c) the user's `__beamtalk_meta` records
`traits => [{TraitName, ContentHash}]`, the hash of each used trait's
source, so the build, ADR 0105's re-check, and ADR 0125's release preflight
can detect a user compiled against a stale trait.

**Trait module.** A trait file compiles to a module that contains **no
dispatch and no callable methods**. It exports `__beamtalk_meta/0`
(ADR 0050) describing the trait — name, type params, `uses`, required and
provided signatures on both sides, per-method source, docs, categories — and
an `on_load` `register_trait/0` that (a) inserts the trait into a new ETS
table `beamtalk_trait_registry` (mirroring `beamtalk_protocol_registry`) and
(b) registers the implied protocol through the **same** codegen path as
`Protocol define:` (`generate_protocol_registrations`,
`class_registry.rs:699`), so the runtime protocol registry does not know
traits exist.

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
  `'__beamtalk_trait_source'/0` on the trait module that returns its source
  text. That function is the only new carrier, and it exists only on trait
  modules.

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
| `build_stdlib` | A trait pre-pass beside the existing protocol pre-pass (`build_stdlib.rs:968`), handing full trait ASTs to every file | Stdlib is rebuilt whole; `generated_builtins.rs` is regenerated from the flattened classes |
| Compiler port (REPL, workspace) | Workspace source, or `'__beamtalk_trait_source'/0` | §11 fan-out, using the users recorded in the trait registry |
| LSP / language service | `ProjectIndex`, which gains trait → users edges | Re-analyses open users when a trait file changes |
| MCP lint / `beamtalk check` | Same as the CLI build | Same as the CLI build |

**Load order.** Trait modules register their implied protocols on load, so
they load before classes, as `protocol_modules` do today. A class module
never *calls* its trait module, so a missing trait module at run time
affects reflection only, never dispatch.

**Packages and releases.** A dependency's trait bodies become part of each
downstream user's compiled code. A patch release of a dependency that
changes a provision has no effect downstream until the downstream package is
rebuilt. The `traits => [{Name, Hash}]` meta entry (§10) lets `beamtalk
build` warn when a dependency's trait hash differs from the one a user was
compiled against, and lets ADR 0125's appup generation see that a user
module changed.

### 11. Hot reload and live patching

- **Stdlib traits are read-only**, like stdlib classes: the workspace
  already refuses to patch a class compiled in stdlib mode
  (`beamtalk_repl_eval.erl:927-938`), and re-expanding a stdlib user would
  recompile `DateTime` or `String`. `Comparable >> max: …` at the REPL is
  refused with that existing error.
- **Editing a user-package trait file** recompiles the trait and **every
  loaded, source-backed, non-stdlib user**, then reloads each user through
  the existing `update_class` path. Users without source in the workspace
  (dependency or release classes) keep their old flattened code, and the
  reload warns naming them; they pick the change up on their next build
  (§10a). Because no shape can
  change (§7), no `sys:change_code`, no migration, no
  `hot_reload_descendants`. ADR 0105's re-check then runs on the users'
  senders as it does for any method change.
- **Live-patching a trait method** — `Comparable >> max: other => …` — is a
  message send (principle 11): it replaces the method in the trait's
  registry entry and re-expands users, the same whole-class recompile that
  `Foo >> bar => …` performs today (ADR 0066 §REPL).
- **`Comparable removeSelector: #max:`** (ADR 0112) removes the provision and
  re-expands users. **`DateTime removeSelector: #max:`** where `max:` is
  trait-provided is an error: "`max:` is provided by trait Comparable;
  exclude it with `uses: Comparable excluding: #(#max:)` or remove it from the
  trait". A class's own override of a trait selector can be removed as
  usual, which re-exposes the provision.
- **Renaming or removing a *required* selector on one user** —
  `DateTime renameSelector: #< to: #precedes:` — is refused while any used
  trait requires or sends it, because the sites that send it live in trait
  bodies shared with other users. The hint names the trait.
- **Browse `save-section` on a flattened method** is routed to the trait
  file, not the class file. Saving it in the class file would silently
  create a detached override; the browse op resolves the file from
  `origin`.
- **Defining a trait at the REPL** — `Trait define: …` — works exactly as
  `Protocol define:` does; changing a class's `uses:` lines is a class
  redefinition, recompiled whole as today.
- **Workspace flush** (ADR 0113) writes a trait file like a class file.

### 12. Reflection and tooling

| Surface | Addition |
|---|---|
| `stdlib/src/trait.bt` — `sealed typed Object subclass: Trait` | `Trait requiredMethods: #Comparable`, `Trait providedMethods: #Comparable`, `Trait usersOf: #Comparable`, `Trait isTrait: #Comparable`, `Trait allTraits` — mirroring `Protocol` (`protocol.bt`) |
| `Behaviour` | `traits` (directly used, in `uses:` order), `allTraits` (transitive, including superclasses'), `usesTrait: #Comparable`; `methods` still answers local selectors and **includes** flattened ones, because they are local |
| `CompiledMethod` | `origin` → the trait name or `nil`; `source` is the trait's text; `respondsTo:`, `canUnderstand:`, `includesSelector:` need no change — the method is there |
| `SystemNavigation` | `usersOf: #Comparable`; `implementorsOf: #between:and:` lists every user (true — each *does* implement it) and each row's `origin` says which trait |
| Browse / categories | Flattened methods appear in the class under their trait's own `// === ===` dividers, prefixed with the trait name (`Comparable › Derived ordering`), via a new `provenance := trait` branch beside the existing `extension → "extensions"` one (`protocol_from_source/4`, `beamtalk_repl_ops_browse.erl:2284-2287`) |
| Xref (ADR 0087, 0115) | `beamtalk_xref_methods` rows: `provenance := trait`, `origin := TraitName`. Sender rows inside a flattened body are indexed per user, `recv_type` per ADR 0115, so `sendersOf:` on `<` finds `DateTime`'s copy of `between:and:` — correct, because that copy really sends `<` to a `DateTime` |
| Rename (ADR 0114) | `renameSelector:to:` on a trait-provided selector of a **class** is refused with a hint to rename on the trait; on the **trait** the site closure is the trait body ∪ the union of every user's ADR 0114 closure, one ChangeLog entry. `renameTo:` on a trait adds `uses:` lines to `referencesTo:` (a new `uses_trait` reference row) and moves the file like a class rename does |
| LSP | Go-to-definition on a flattened method jumps to the trait; completion on a receiver of a using class lists trait methods (they are in `ClassInfo.methods`); hover shows `from Comparable` |
| Extension conflict (ADR 0066) | An extension `DateTime >> max:` on a trait-provided selector is the existing "cannot override a method defined in the class body" error, because after flattening it *is* in the body |

### 13. Diagnostics

All at the `uses:` line or the offending trait line; codes assigned at
implementation in the existing `E`/`W` series.

| Situation | Severity | Message shape |
|---|---|---|
| Two traits provide the same selector | Error | "`sel` is provided by both A and B in C. Define `sel` in C, or exclude one: `uses: B excluding: #(#sel)`" |
| Required selector unresolvable (closed world) | Error | "C uses T but does not implement required `sel`" + the required signature as hint |
| Required selector unresolvable (open world) | Hint | as ADR 0100 |
| `uses:` names a protocol | Error | "`Printable` is a protocol, not a trait — protocols carry no method bodies. Convert it with `Trait define: Printable`, or implement the methods" |
| `uses:` names an unknown name | Error | "unknown trait `T`" + nearest-name hint |
| `uses:` after a slot or method | Error | "`uses:` lines must come before state and method declarations" |
| Unknown keyword line in a class body | Error | "unexpected `foo:` in class body" (replaces the silent end-of-body) |
| `excluding:`/`aliasing:` names a selector T does not provide | Error | "T does not provide `sel`" |
| `excluding:` a required selector | Error | "`sel` is required by T, not provided; requirements cannot be excluded" |
| `state:`/`field:`/`classState:` or `self.slot` in a trait | Error | "traits are stateless — declare `slot -> Type` as a required method" |
| Trait `uses:` cycle | Error | "trait A uses itself through B" |
| Trait modifier (`sealed Trait define:`) | Error | "traits take no modifiers" |
| `sealed` or `internal` on a trait method | Error | "`sealed`/`internal` are not supported on trait methods in v1" |
| A trait provides `initialize`, `migrateFromV<N>:`, `doesNotUnderstand:args:`, `supervisionPolicy` or `supervisionSpec` | Error | "a trait cannot provide `sel`; it changes how the class is built or dispatched. Declare it as a required method instead" |
| A provision would override an inherited `sealed` method | Error | as for a class-body method overriding a sealed method |
| A provision would override an inherited method (outside the kind roots) without `overriding:` | Error | §3a — names the trait, the superclass, and both `overriding:` and `excluding:` fixes |
| The same, where the superclass chain is open-world (ADR 0100) | Hint | §3a |
| An `overriding:` entry that replaces nothing | Warning | "`sel` in `overriding:` does not override an inherited method; remove it" |
| `overriding:` names a selector T does not provide | Error | "T does not provide `sel`" |
| A class `uses:` a trait its superclass already uses | Warning | "C's superclass already uses T; this re-flattens T's methods over the superclass's customisations" |
| A class-body override is not override-compatible with the provision it replaces | Warning | §8 |
| Renaming or removing a required selector on a user | Runtime error | §11 |
| `Self` as a parameter type in a class body method | Error | unchanged (`check_no_self_in_params`); the hint mentions traits when the method's class uses one |
| Type mismatch between a required signature and the class's method | Warning | as ADR 0025 protocol/type warnings |
| `removeSelector:` on a trait-provided method of a class | Runtime error | see §11 |

### REPL session

With `comparable.bt` and this `version.bt` loaded:

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
Version traits                                            // => [Comparable]
Version conformsTo: #Comparable                           // => true
Integer conformsTo: #Comparable                           // => true — structurally, no uses:
(Version >> #max:) origin                                 // => Comparable
(Version >> #<) origin                                    // => nil
Trait usersOf: #Comparable                                // => [Version, DateTime, Duration, Uuid, String]
Comparable >> max: other :: Self -> Self => (self < other) ifTrue: [other] ifFalse: [self]
// => error: Comparable is a stdlib trait and cannot be patched in the workspace
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
- **The implied protocol** is emitted through the existing protocol
  registration codegen, so `beamtalk_protocol_registry.erl` has no
  trait-specific branch and the "required ∪ provided" rule is computed once,
  in Rust.
- **The trait registry's row shape** (`__beamtalk_meta` for a trait) crosses
  the Rust→Erlang boundary as *data*. It gets a conformance fixture under
  `runtime/apps/beamtalk_runtime/test/fixtures/` like the other meta shapes
  (architecture-principles §7 "Keep" disposition), not a hand-mirrored
  record.
- **Method categories** stay source-only (`method_category.rs`); the browse
  layer reads the trait's dividers from the trait's source, so no second
  category representation appears.

## Prior Art

| Language | Mechanism | Composition | Conflicts | State | Taken / rejected |
|---|---|---|---|---|---|
| **Smalltalk-80 / Squeak / Pharo ≤ 6** | Traits (Schärli, Ducasse, Nierstrasz, Black, ECOOP 2003): `Object subclass: #C uses: TA + TB - {#x} @ {#y -> #x}` | Flattened; class wins; `+`/`-`/`@` | Explicit exclusion or alias; unresolved conflict is a method that errors when called | Stateless, required accessors | **Adopted** wholesale: flattening, precedence, exclusion, alias, required/provided, same-origin-is-not-a-conflict. Adapted: keyword clauses instead of `+ - @` operators; conflict is a *compile* error, not a runtime `traitConflict`; no `T` prefix |
| **Pharo 7+** | Stateful traits (`TraitedClass`, `TSlot`) | as above + slots | as above | Slots | **Deferred** (§7) — kind-specific slots need a threading story |
| **Newspeak** | Mixins: `Superclass mixin |> C`; every class body is a mixin | Linearised application creates a class per use | Latest application wins (order) | Yes | **Rejected** (§Alternatives) — order-dependent, one registry class + module per application, extra chain steps per send |
| **Scala** | `trait` with linearisation; `with A with B` | Linearised; `super` chains through traits | Order decides; `abstract override` | Yes | Rejected as mixins; but Scala's "trait is also a type" is our §8 |
| **TypeScript** | No traits; a `class` used as a type includes all its members; `override` keyword and `noImplicitOverride` (4.3) | Structural | n/a | n/a | Adopted: a type made of every member, implemented or not, is our required ∪ provided rule (§8), and adding a member is a reported break for structural implementers. `noImplicitOverride` is the same concern as `overriding:` (§3a) |
| **Rust** | `trait` with default methods, `impl T for S` | Static, type-directed; orphan rules | Disambiguation by qualified path | None (associated fns only) | Adopted: required vs. default methods as the two body shapes. Rejected: nominal `impl` — ADR 0068 chose structural |
| **Swift** | Protocol extensions with default implementations | Static dispatch for non-requirement extension methods | Ambiguity error | None | Rejected: the static/dynamic dispatch split is a well-known footgun; Beamtalk has one dispatch (ADR 0006) |
| **C# 8** | Default interface methods; `override` / `new` on class members | Class wins over interface defaults; a class member hiding a base member without `new` or `override` warns | Must override | None | Adopted: the versioning stance. C# makes replacing inherited behaviour an explicit act so a base-type release cannot silently change a subclass; `overriding:` (§3a) is that rule applied to trait provisions |
| **Java 8 / Kotlin** | Interface default methods | Flattened-ish; class wins, then explicit `A.super.m()` (Java) / `super<A>.m()` (Kotlin) | Must override | None | Adopted: class wins. Rejected: the qualified-super call; we alias instead (§4) |
| **Pony** | `trait` (nominal, default methods) + `interface` (structural) | Nominal `is T` | Must override | None | Closest match to §8: nominal *composition* declaration + structural *type*. Pony has no exclusion/alias; we keep Pharo's |
| **Dart** | `mixin` with `on` constraints, `with` | Linearised | Order decides | Yes | Rejected as mixins; `on` is our required-methods list |
| **Ruby** | `module` + `include` / `prepend` | Linearised into the ancestor chain | Order decides | Instance vars by convention | Rejected as mixins; `Comparable`/`Enumerable` are the canonical examples and the names we use |
| **Perl Moose / Raku** | Roles: `with 'R' => { -excludes => …, -alias => … }` | Flattened (roles are traits) | Compile-time conflict unless resolved | Attributes allowed | Confirms exclusion/alias as keyword clauses read fine outside Smalltalk |
| **Elixir** | `use M` → `__using__` macro injects code; `defprotocol` for dispatch | Textual injection | Injected and local clauses of the same name/arity merge (with a "clause cannot match" warning) unless the injector marks them `defoverridable` | n/a | `use` is unchecked injection; our flattening is checked injection, and `defoverridable` is our class-wins rule made opt-in. Protocols ≈ our ADR 0068. `Enumerable` is the reference API |
| **Erlang / OTP** | `-behaviour(M)` callbacks; `-extends` parse transform | Contract only | n/a | n/a | Behaviours ≈ protocols; no body sharing — the gap this ADR fills |
| **LFE Flavors** | Mixins (`(defflavor … (:mixins …))`) | Linearised | Order | Yes | Rejected as mixins |
| **Gleam** | None; modules and functions | n/a | n/a | n/a | Not applicable |

**A note on the word "protocol".** In Pharo a *protocol* is a method
category — `accessing`, `printing` — with no required-selector set and no
conformance check. Beamtalk already uses the word both ways: the REPL's
`browse-protocols` op groups methods by category (the Pharo sense), while
`Behaviour protocols` and `Protocol define:` are ADR 0068's structural
types. In this ADR "protocol" always means the ADR 0068 type, and the
"trait is also a protocol" rule (§8) comes from Scala and Pony, not Pharo.

## User Impact

**Newcomer (Python/JS/Ruby).** `uses: Comparable` next to `state:` reads like
Ruby's `include Comparable` and Python's `class V(Comparable)`, and the
required/provided split is the Java-interface-with-defaults they know. The
errors name the fix (`define it in C, or exclude one: …`). They discover it
by `Version traits` and by `browse` showing `Comparable › Derived ordering`.
The one surprise is that `Integer conformsTo: #Comparable` is true without
`uses:` — the same structural surprise ADR 0068 already documents.

**Smalltalk developer.** This is Pharo traits with the operators spelled as
keywords and the conflict moved to compile time. Flattening, class-wins,
`super` = superclass, stateless + required accessors, same-origin harmless:
all Schärli. What differs: no `T` prefix, no runtime `traitConflict` method,
and traits are protocols. They would look for `Trait named:` and find
`Trait define:`; for `TFoo` and find `Foo`.

**Erlang/Elixir developer.** A flattened method is an ordinary function in
the class's module — `observer`, `recon`, `dbg` and `Module:module_info()`
show it with no trait indirection, and Erlang callers dispatch to it like any
other. The trait module is metadata only. `use`-style injection is what they
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

**Tooling developer.** The AST gains `TraitDefinition` and
`ClassDefinition.uses: Vec<TraitUse>`; everything downstream sees a
flattened class with `origin` marks. Completion, hover, go-to-definition and
rename each need one branch on `origin`. Static analysis gets *easier*: a
trait method is analysed in each concrete context with concrete types.
The unparser (needed for flush and rename) must round-trip `Trait define:`
and `uses:` lines.

## Steelman Analysis

### Option A: Stateless flattened traits, `Trait define:` + `uses:` lines (chosen)
- 🧑‍💻 **Newcomer**: "`uses: Comparable` is `include Comparable`. Required methods are the Java interface I already know. The error tells me the exact line to add."
- 🎩 **Smalltalk purist**: "This is Schärli's paper with keyword syntax. Flattening keeps the class the unit of meaning; `super` still means the superclass; conflicts are mine to resolve, not the runtime's to guess."
- ⚙️ **BEAM veteran**: "Flattened methods are plain functions in plain modules. No macro magic, no extra hop, and `Module:module_info(exports)` tells the truth."
- 🏭 **Operator**: "Zero dispatch cost and zero shape migrations. A trait edit is N method reloads I can already observe."
- 🎨 **Language designer**: "Trait = protocol + bodies, one body grammar, one namespace, one conformance rule. The design adds a composition declaration and nothing else to the type system."

### Option B: Protocols with default bodies (`Protocol define:` grows `=>`)
- 🧑‍💻 **Newcomer**: "One concept instead of two — Swift and Java do it this way."
- 🎩 **Smalltalk purist**: "Smalltalk never had protocols as types; a trait is the nearest thing, so one keyword should do."
- ⚙️ **BEAM veteran**: "Elixir has one `defprotocol`; two keywords for one idea is Java-brained."
- 🏭 **Operator**: "Fewer file kinds to reason about in a deploy."
- 🎨 **Language designer**: "Since a trait *is* a protocol (§8), the keyword is the only difference. And Java 8 added default methods precisely so interfaces could evolve without breaking implementers — which §8's required ∪ provided rule gives up."
- *The retroactive form is rejected; the opt-in form is not.* Conformance is automatic, so a protocol whose bodies landed on every structural conformer would inject methods into classes across packages that never asked — Swift's retroactive protocol extensions. Any safe form needs an opt-in `uses:` line to attach exclusion and aliasing to.

### Option B′: One keyword, opt-in, required-only type (type rule rejected; keyword open)
`Protocol define:` may carry bodies; a class opts in with `uses:`; the protocol's type is its *requirements* only, so provisions can be added without breaking structural conformers.
- 🧑‍💻 **Newcomer**: "Exactly Java/Kotlin interfaces with defaults. One word."
- 🎩 **Smalltalk purist**: "Smalltalk never had two type-like keywords, and Pharo's 'protocol' was never a type anyway."
- ⚙️ **BEAM veteran**: "One `defprotocol`-like construct, and adding a default is never a breaking change."
- 🏭 **Operator**: "A dependency adding a default method can't break my conformance checks."
- 🎨 **Language designer**: "It keeps everything A gets right — opt-in, flattening, class-wins — and fixes A's one evolvability flaw."
- *Why the required-only type is rejected* (decided 2026-09-23, §Status 2): Java 8's evolvable-interface rule depends on nominal typing, where every implementer inherits the defaults. Under structural conformance a class with only `<` conforms without `uses:` and never receives `max:`. A required-only type admits it into `clamp: x :: Comparable …`, which then fails with `does_not_understand` at run time; required ∪ provided reports the same case as a conformance warning at the call site. B′'s evolution advantage is also smaller than it looks, since conformance problems are warnings (ADR 0068) and adding a provision never breaks a build under A either. It also makes provisions nearly useless to generic code, which can only safely send the required selectors.
- *What remains open*: the single-keyword half of B′. One `Protocol define:` that may carry bodies, with A's type rule, is semantically identical to A; the choice is readability (§Status 3).

### Option C: Newspeak-style mixins (class-in-the-chain)
- 🧑‍💻 **Newcomer**: "Dart and Ruby do it this way; `with` reads fine."
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

### Option E: Shared trait module + dispatch step (implementation variant of A)
- ⚙️ **BEAM veteran**: "One copy of the bytecode; reload the trait module and every user sees it instantly, like a superclass method today."
- 🏭 **Operator**: "N users, one reload."
- 🎨 **Language designer**: "Foreign extensions already do this; reuse it."
- *Why rejected*: actor self-sends compile against the lexical module (§10); a trait module has no gen_server. Two calling conventions means two bodies per method. Every send pays a probe. `super`, sealed self-sends, `late` guards and error hints bake in the lexical class. The "instant reload" advantage is real and is the price paid in §Consequences.

### Option F: Stateful traits (Pharo 7 style)
- 🧑‍💻 **Newcomer**: "A `Counting` trait that can't hold `count` feels crippled."
- 🎩 **Smalltalk purist**: "Pharo added slots to traits because required accessors were boilerplate."
- 🎨 **Language designer**: "A trait that owns its state is more cohesive."
- *Why deferred*: state declarations are per-kind (ADR 0067), so a stateful trait is either kind-specific (and cannot cross the wall it exists to cross) or needs a fourth `ThreadedIr` storage input. No stdlib evidence needs it; §7 leaves the door open.

### Option G: Multiple inheritance
- 🎨 **Language designer**: "CLOS/Dylan-style MI with a linearisation is the general solution."
- *Why rejected*: ADR 0005 Q8 decided single dispatch and single inheritance; MI cannot cross class kinds; the diamond problem is exactly what traits were invented to remove.

### Tension points
- **One keyword and the type rule (A vs. B′).** B′ differed from A in keyword count and in whether provisions are part of the type. The type rule is settled for A, because under structural conformance B′'s rule trades a compile-time warning for a run-time `does_not_understand`. The keyword count remains a readability judgement.
- **Evidence vs. modelling (A vs. H).** The stdlib count is mostly answerable by inheritance, so reviewers who weigh measured duplication alone would prefer H. The decision went to A because the hierarchy should record kinds and traits should record capabilities; H would encode ordering and enumeration as kinds.
- **One concept vs. two (A vs. B).** Language designers and Elixir users lean B; Smalltalkers and the "honesty of form" rule lean A. Resolved for A because the keyword carries information (bodies present or not) and the two are convertible in place.
- **Copy vs. share (A vs. E).** Operators want E's one-reload; the compiler's lexical assumptions make A the only one that works without re-architecting self-sends. Resolved for A; the N-recompile cost is accepted and bounded (§Consequences).
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

### Protocols with default method bodies
`Protocol define: Comparable` gaining `=>` bodies. The **retroactive** form,
where bodies apply to every structural conformer, is rejected: it injects
methods into classes that never opted in (§Steelman B). The **opt-in** form,
B′ (`uses:` required, type = requirements only), is rejected for its type
rule (§Steelman B′, §Status 2). Its single-keyword syntax, with A's type
rule, remains open (§Status 3).

### Newspeak / Ruby / Dart / Scala mixins
Linearised application inserting a class per use. Rejected: order-dependent
conflict resolution; a registry gen_server and module per application; one
more step in every chain walk; cannot cross class kinds if it carries state
(§Steelman C).

### Extension methods only
Status quo, one `Trait+Class.bt` per user. Rejected: it is what produced the
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
Java 8's escape hatch. Rejected in favour of `aliasing:` (§4): it keeps the
flattening property (nothing trait-shaped survives compilation) and needs no
new send form.

### Do nothing
The BT-274 cancellation position ("the class hierarchy handles behavior
sharing adequately for current needs"). The stdlib count alone does not
refute it: eight classes and ~25 hand-written methods a trait would provide
(12 derived ordering operators across four classes, four `min:`/`max:`
copies, ten enumeration and emptiness methods across `SupervisionTree`,
`ChangeLog` and `String`), most of which inheritance could also remove
(see "Inheritance only" above). Rejected because capabilities should not
be modelled as kinds, and because the three cases inheritance cannot reach
— two concerns on one class, the actor/value wall, and a spent superclass
slot — are permanent properties of the language, not of today's corpus.

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
- **No silent override on upgrade** (§3a). Neither a new trait provision
  nor a new superclass method can change a class's behaviour without an
  error at that class's next build, which removes the fragile-base-class
  problem that plain trait precedence makes worse.
- The type system gains nothing new: a trait is a protocol (§8), and
  ADR 0068's structural rule is untouched.
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
  `Comparable` (six methods × five users) and `Enumerable` (~ten × three)
  this is a few KB. A trait used by hundreds of classes would be measurable;
  the mitigation is that provided methods are usually one-liners over
  required ones.
- **N recompiles on trait edit** (§11). Editing a trait recompiles every
  source-backed user. Per-file compiles are fast and already the unit of
  `>>` live patching; the workspace loader must batch them and announce once
  per user.
- **Reflection must say where a method came from** or `implementorsOf:` looks
  wrong. `origin` on `CompiledMethod` and xref rows is mandatory, not
  optional, and the browse layer must group by trait.
- **No stateful traits** — the `Counting` example needs two required
  accessors the user must write. Deferred with a stated extension path.
- **A second `define:` keyword** next to `Protocol define:`. Mitigated by
  identical body grammar and in-place convertibility (open decision 3).
- **Adding a provision to a trait is a breaking change** for classes that
  conform to its protocol structurally without `uses:`. They get conformance
  warnings, not build failures (§8, decided §Status 2).
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
- Trait modules exist only for reflection and registration; they contain no
  callable methods. `beamtalk_dispatch` is unchanged.
- Method categories remain source-only (`method_category.rs`); flattened
  methods take their trait's dividers.
- Extension methods and traits compose: an extension may target a using
  class for any selector the flattened body does not define, and may target
  the trait's *implied protocol* no more than it can target a protocol today
  (it cannot).
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
| 1 | **Syntax**: `TraitDefinition` AST (`ast/class.rs`), `ClassDefinition.uses: Vec<TraitUse { trait, type_args, excluding, aliasing, span }>`; `is_at_trait_definition` in the top-level dispatch (`parser/mod.rs:1406-1429`); `parse_trait_body` sharing `parse_protocol_method_signature_with_doc` for requirements (reserving `uses:`/`excluding:`/`aliasing:`) and the class method parser for provisions; `package@Trait`; `uses:` in the class-body loop with ordering and unknown-keyword errors; unparse round-trip; one-definition-per-file and ADR 0119 module naming for traits; lexer nothing (contextual keywords) | `beamtalk-core` source_analysis, ast, unparse | M | — |
| 2 | **Semantics**: `semantic_analysis/trait_registry.rs` (mirrors `protocol_registry.rs`: registration, name collision, cycle check); `trait_expansion.rs` implementing §3–§5 as a new pass with explicit inputs (expansion before `ClassHierarchy`, requirement check after; exclusion, aliasing, conflict, class-wins, same-origin rule, synthesised accessors ranked as class body); `MethodInfo.origin` with `defined_in` left as the using class; implied-protocol registration into `ProtocolInfo`; hygienic type-param and `Self` substitution; reserved-selector and override-compatibility checks; the §3a unacknowledged-override check with kind-root exemption and stale-entry warning; statelessness validator (§7); all §13 diagnostics; `typed` check on the flattened class | `beamtalk-core` semantic_analysis, type_checker | L | 1 |
| 3 | **Codegen & build graph**: feed the flattened `ClassDefinition` to the existing generators (no change to `merge_method`); `methodXref` provenance/origin; `methodSource` as the trait's source slice; `traits => [{Name, Hash}]` in user meta; trait module emission (`__beamtalk_meta`, `'__beamtalk_trait_source'/0`, `register_trait/0`, implied protocol via `generate_protocol_registrations`) loaded before classes like `protocol_modules`; every §10a entry point (CLI cache key, `build_stdlib` trait pre-pass and `generated_builtins.rs`, compiler port, `dependency_classes.rs`, `ProjectIndex` edges); conformance fixture for the trait meta shape | `beamtalk-codegen`, `beamtalk-compiler-port`, `beamtalk-cli`, `beamtalk-language-service`, `build_stdlib` | L | 2 |
| 4 | **Runtime & reflection**: `beamtalk_trait_registry.erl` (ETS, mirrors protocol registry); `stdlib/src/trait.bt`; `Behaviour traits/allTraits/usesTrait:`; `CompiledMethod origin`; `SystemNavigation usersOf:`; xref `provenance := trait` + browse grouping; `removeSelector:` guard (§11); `beamtalk_xref_methods` schema bump; surface-parity table rows | runtime, stdlib, `docs/development/surface-parity.md` | M | 3 |
| 5 | **Live system**: trait file reload → user recompile fan-out in the workspace loader (source-backed, non-stdlib users only; stdlib traits read-only); `Trait >> sel => …` and `Trait removeSelector:` live patching; required-selector rename/remove refusal and `save-section` routing (§11); `Trait define:` at the REPL; ADR 0105 re-check hookup; ADR 0114 rename (`renameSelector:to:` redirection, trait `renameTo:` with `uses:` reference rows); flush of trait files (ADR 0113); LSP go-to-definition/hover/completion on `origin`; REPL-protocol tests | workspace, REPL, LSP | L | 4 |
| 6 | **Stdlib adoption** (one issue per trait): `Comparable` on `DateTime`, `Duration`, `Uuid`, `String` (each keeps its primitive operators); `Enumerable(E)` on `SupervisionTree` and `ChangeLog` per §9 (`ChangeLog` keeps its all-entries `select:`, and keeps its public `notEmpty` with `uses: Enumerable(ChangeEntry) aliasing: #{#notEmpty => #isNotEmpty}`; `SupervisionTree` keeps its `do:`); separately, move `Integer`/`Float` `min:`/`max:` up to `Number` (inheritance, not traits); BUnit tests in `stdlib/test/`; `docs/beamtalk-language-features.md` § Traits; close ADR 0005 Q9 | stdlib, docs | M | 5 |

**Tests per phase.**

| Phase | Suites |
|---|---|
| 0 | A BUnit fixture in `stdlib/test/` pinning the observed self-send binding: a subclass override called from an inherited actor method |
| 1 | Parser unit tests and snapshots in `beamtalk-core`, unparse round-trip, and diagnostics for misplaced `uses:` and unknown keyword lines |
| 2 | Semantic-analysis unit tests for each §13 row, both §3a directions (trait grows, superclass grows), the same-origin diamond, class-wins, `Self` substitution, and generic `E` substitution |
| 3 | `test-package-compiler` codegen snapshots for one user of each class kind, `just verify-threaded-ir` over the flattened stdlib, and the trait-meta conformance fixture |
| 4 | Runtime EUnit for `beamtalk_trait_registry`, BUnit reflection tests (`traits`, `origin`, `usersOf:`), and xref tests for `provenance := trait` |
| 5 | `tests/repl-protocol/cases/` for trait reload fan-out, `Trait >> sel` live patching, `removeSelector:` refusal, and rename; plus LSP tests |
| 6 | Existing `just test-stdlib` and `just test-bunit` stay green with the duplicates deleted, plus new BUnit tests for `between:and:`/`min:`/`max:` on each new user |

Phase 1 alone is mergeable (a parsed but unexpanded `uses:` is a "not yet
supported" error); phases 2–3 together give a working compiler; phase 4 is
required before the feature is documented as available, because reflection
without `origin` misleads.

**Minimal v1 (open decision 4).** If the author prefers a smaller first
cut, the natural boundary is: Phases 0–3 plus, from Phase 4, only xref
`origin` and `CompiledMethod origin`; `uses:` with `excluding:` and
`overriding:` only (§3a is not optional: without it, a minimal v1 would
ship the silent-override problem it exists to prevent);
trait edits as file reloads; no `aliasing:`, no class-side traits, no
`Trait >>` live patching, no `beamtalk_trait_registry`. `aliasing:` then
waits for a second use beyond `ChangeLog notEmpty`, which a one-line
`notEmpty => self isNotEmpty` covers.

Deferred to follow-up ADRs, explicitly: stateful traits (§7); trait-level
modifiers; `sealed` traits for whole-program optimisation (BT-274's last
criterion — moot while flattening already gives per-class sealing).

## Migration Path

Additive for language semantics; three notes for adopters:

- **Stray keyword lines** in a class body were silently treated as end-of-body
  and become an error (§13). `just test-stdlib` and the parity projects will
  surface any instance.
- **Converting a protocol to a trait** is a file edit (`Protocol define:` →
  `Trait define:`, add bodies to existing signatures) with no consumer
  changes (§8). Adding *new* provisions, or the reverse conversion, is a
  breaking change and follows the normal deprecation path.
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
