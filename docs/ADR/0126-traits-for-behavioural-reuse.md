# ADR 0126: Traits — Stateless, Flattened Units of Behaviour

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
reload all see ordinary methods. A class's own method beats a trait's, a
trait's beats an inherited one, and two traits providing the same selector is
a compile error unless the class defines it or excludes one. Every trait also
*is* a protocol of the same name (its required ∪ provided selectors), so the
structural conformance rule of ADR 0068 is unchanged and `uses:` never
becomes a nominal `implements:`.

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

*Ordering.* Five classes hand-write the four ordering operators, each derived
from one primitive comparison, and none of them gets `between:and:`, `min:`
or `max:`, which exist only on `Number`:

| Class | `<` `>` `<=` `>=` | `between:and:` / `min:` / `max:` |
|---|---|---|
| `DateTime` (`date_time.bt:313-341`) | 4 × `(Erlang beamtalk_datetime) lt:/gt:/lte:/gte:` | none |
| `Duration` (`duration.bt:225-253`) | 4 × `(Erlang beamtalk_duration) …` | none |
| `Uuid` (`uuid.bt:140-149`) | 4 × `(Erlang beamtalk_uuid) …` | none |
| `String` (`string.bt:96-123`) | 4 × `@primitive` | none |
| `Character` (`character.bt:61-85`) | 4 × `@primitive` | none |
| `Integer` / `Float` | 4 × `@primitive` each | `min:`/`max:` **duplicated verbatim** in both (`integer.bt:294-306`, `float.bt:177-188`); `between:and:` on `Number` only |

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

Neither cluster is fixable by inheritance. `DateTime` and `Uuid` are not
numbers. `SupervisionTree` is a snapshot, not a collection, and an actor
that wants `do:`/`select:`/`detect:` over its contents (a registry, a pool)
cannot subclass `Collection` at all. The ADR 0037 steelman for "Option C:
Traits / Protocol" already said so — "Traits separate the *what* (Enumerable
protocol) from the *how* (implementation per type)" — and rejected it only
because protocols did not yet exist. They do now.

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
| `sealed` / `internal` before the selector | Carried onto the flattened method unchanged |
| `uses: OtherTrait …` | Trait composition (§2) — a trait may use traits |
| `// === Name ===` | Category divider (`method_category.rs`), kept with the trait's source |
| `/// doc` | Doc comment, carried onto the flattened method and shown by `browse` |

A trait may be generic — `Trait define: Enumerable(E)` — with the same
type-parameter syntax as classes and protocols. `Self` in a trait signature
denotes the using class.

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
uses: TraitName[(TypeArgs)] [excluding: #(#sel, …)] [aliasing: #{#newSel => #traitSel, …}]
```

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
- `aliasing:` adds each trait-provided `#traitSel` to the class *also* under
  `#newSel` (Pharo `@`). The original stays unless also excluded. The
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
2. Apply `excluding:`, then `aliasing:`, per `uses:` line.
3. Merge the provisions of all used traits into one set, detecting
   conflicts (§4).
4. Drop every trait provision whose selector (same side) the class body
   defines — **the class wins**.
5. Check requirements (§5).
6. Compile the class with the merged methods as if they were its own.

Precedence, highest first: **class body → trait provision → inherited**. A
trait provision therefore *overrides* an inherited method, precisely as a
method written in the body would. This is Schärli's flattening property and
Pharo's rule, and it is the property that makes every later section cheap:
after step 6 there is nothing trait-shaped left for `ThreadedIr`, dispatch,
`super`, sealed self-sends, xref or hot reload to know about.

Flattening is a compile-time transformation of the AST in the Compilation
context (`beamtalk-core` semantic analysis), the same place same-file
self-extensions are folded today. It runs before `ClassHierarchy` is built,
so type inference (ADR 0083), protocol conformance (ADR 0068), sendability
(ADR 0103), definite assignment (ADR 0124) and every validator see the
flattened class. `MethodInfo.defined_in` is set to the **trait name** for
each flattened method, and a new `MethodInfo.origin: Origin::Trait(name)`
distinguishes "from a trait" from "inherited from a superclass" for
reflection and diagnostics.

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
provisions, or from the superclass chain. Resolution uses the existing
`ClassHierarchy::resolves_selector`, so ADR 0100's open-world policy applies:
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
  existing throw/catch lowering. `Enumerable>>detect:` below relies on it
  exactly as `Collection>>detect:` does (`collection.bt:304`).
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

1. **Kind-neutrality for free.** `self count` compiles correctly in an
   actor (`State` map), a value (`Self` map) or a class-side method
   (`ClassVars` map) because it is a message send, and message sends are
   already threaded by `ThreadedIr` in every context. A `self.count := …`
   in a trait body would need the trait to know which storage family it will
   be flattened into — that is a fourth threading input, and it is exactly
   the "per-kind semantics" ADR 0067 spent a whole ADR separating.
2. **No shape changes, ever.** Since a trait adds no slots, using, editing or
   reloading a trait never changes a class's state shape, so it never
   triggers ADR 0123 migration or `hot_reload_descendants`. Trait reload is a
   pure method-table event.
3. **It is the original model.** Schärli's traits are stateless with required
   accessors; Pharo shipped that for a decade before adding stateful traits,
   and its `TSlot`/`TraitedClass` machinery is the part that keeps breaking.
4. **`Object` subclasses cannot hold state at all** (ADR 0067), yet should be
   able to use class-side traits (§9).

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
- **Converting a protocol into a trait is source-compatible for every
  consumer.** When `Printable` grows a default body, its file becomes
  `Trait define: Printable`; every `:: Printable` annotation, `conformsTo:`
  query and structurally-conforming class is unaffected. The reverse
  (trait → protocol) breaks users' `uses:` lines and is an ordinary
  deprecation.

`extending:` on protocols and `uses:` on traits are the same operation at
different levels (contract composition vs. implementation composition) and
stay separate keywords, because a protocol cannot use a trait (it would
acquire bodies) and a trait using a protocol would only add requirements,
which is what listing them does already.

### 9. All three class kinds can use traits

**Value** — `DateTime`, above (§2). The same change applies to `Duration`,
`Uuid`, `String` and `Character`; each keeps its primitive `<` and may keep
its primitive `>`/`<=`/`>=` too, since a body method beats a provision (§3)
— the decision to drop the three derived operators is per class and is a
one-send-per-call trade.

**Actor** — an actor that wants enumeration over what it holds:

```beamtalk
// stdlib/src/enumerable.bt
Trait define: Enumerable(E)
  /// Required — evaluate `block` for every element, in the receiver's order.
  do: block :: Block(E, Object) -> Nil

  size -> Integer => self inject: 0 into: [:n :_each | n + 1]
  isEmpty -> Boolean => self size =:= 0
  isNotEmpty -> Boolean => self isEmpty not

  inject: initial :: A into: block :: Block(A, E, A) -> A =>
    acc := initial
    self do: [:each | acc := block value: acc value: each]
    acc

  select: block :: Block(E, Boolean) -> List(E) =>
    (self inject: #() into: [:acc :each |
      (block value: each) ifTrue: [acc addFirst: each] ifFalse: [acc]
    ]) reversed

  collect: block :: Block(E, R) -> List(R) =>
    (self inject: #() into: [:acc :each | acc addFirst: (block value: each)]) reversed

  detect: block :: Block(E, Boolean) ifNone: noneBlock :: Block(T) -> E | T =>
    self do: [:each | (block value: each) ifTrue: [^each]]
    noneBlock value

  anySatisfy: block :: Block(E, Boolean) -> Boolean =>
    self do: [:each | (block value: each) ifTrue: [^true]]
    false

  count: block :: Block(E, Boolean) -> Integer =>
    self inject: 0 into: [:n :each | (block value: each) ifTrue: [n + 1] ifFalse: [n]]
```

```beamtalk
typed Actor subclass: WorkerPool
  uses: Enumerable(Worker)
  state: workers :: List(Worker) = #()

  do: block :: Block(Worker, Object) -> Nil => self.workers do: block

  add: w :: Worker -> Nil => self.workers := self.workers add: w
  idle -> List(Worker) => self select: [:w | w isIdle]
```

`inject:into:` writes a captured local inside a block — a Tier 2 stateful
block body (ADR 0111) — and `detect:ifNone:` uses `^`. Both lower through
`ThreadedIr` **in the using class's context**: in `WorkerPool` the method is
an actor `dispatch/4` clause with `State` threading; in `SupervisionTree` it
is a value function. The trait author writes it once and never thinks about
which.

`Collection` itself becomes `abstract typed Value subclass: Collection(E)`
`uses: Enumerable(E)`, overriding `select:`/`collect:` to return `Self` via
`species` as it does today (`collection.bt:162-172`); `SupervisionTree` and
`ChangeLog` drop their hand-written copies. This is the ADR 0037 "Option C"
outcome, delivered without touching the class hierarchy.

**Object** — instance methods on an uninstantiable class are pointless, so
`Object subclass:` users compose **class-side** traits:

```beamtalk
Trait define: Versioned
  /// Required.
  class version -> String

  class banner -> String => self name ++ " " ++ self version
  class isNewerThan: other :: String -> Boolean =>
    (Erlang beamtalk_version) compare: self version with: other =:= #gt
```

```beamtalk
Object subclass: Program
  uses: Versioned

  class version -> String => "1.4.0"
```

`Program banner // => "Program 1.4.0"`. A trait with instance-side provisions
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
`provenance := trait, origin := TraitName`, and (b) `methodSource` for a
flattened method is the trait's source text, so `browse` and
`CompiledMethod source` show the real code.

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

**Cross-package use.** Flattening needs the trait's method ASTs at the
user's compile time. Within a compile unit they come from the parsed
`Module`. Across packages the compiler port loads the trait module's
`__beamtalk_meta` and re-parses the embedded per-method source — the same
carrier already used for cross-package `ClassInfo` (ADR 0050) and for
`browse` of loaded classes. A trait is therefore a compile-time dependency
of its users, recorded in the `ProjectIndex` like a superclass is.

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

### 11. Hot reload and live patching

- **Editing a trait file** recompiles the trait and **every loaded user**
  (`Trait usersOf:` from the registry ∪ `ProjectIndex` users), then reloads
  each user through the existing `update_class` path. Because no shape can
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
| Browse / categories | Flattened methods appear in the class under their trait's own `// === ===` dividers, prefixed with the trait name (`Comparable › Derived ordering`), via a new `provenance := trait` branch beside the existing `extension → "extensions"` one (`beamtalk_repl_ops_browse.erl:2245-2259`) |
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
| Trait modifier (`sealed Trait define:`) | Error | "traits take no modifiers; use `sealed` on individual methods" |
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
Version traits                                            // => #(Comparable)
Version conformsTo: #Comparable                           // => true
Integer conformsTo: #Comparable                           // => true — structurally, no uses:
(Version >> #max:) origin                                 // => Comparable
(Version >> #<) origin                                    // => nil
Trait usersOf: #Comparable                                // => #(Version, DateTime, Duration, Uuid, String, Character)
Comparable >> max: other :: Self -> Self => (self < other) ifTrue: [other] ifFalse: [self]
                                                          // => Comparable — six classes re-expanded
Version removeSelector: #max:
// => error: `max:` is provided by trait Comparable; exclude it with
//    `uses: Comparable excluding: #(#max:)` or remove it from the trait
```

### Single source of truth

- **Flattening, precedence, conflict and requirement rules** live in one
  Rust pass (`semantic_analysis/trait_expansion.rs`), which produces a
  flattened `ClassDefinition`. Codegen, the type checker, validators and the
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
| **Rust** | `trait` with default methods, `impl T for S` | Static, type-directed; orphan rules | Disambiguation by qualified path | None (associated fns only) | Adopted: required vs. default methods as the two body shapes. Rejected: nominal `impl` — ADR 0068 chose structural |
| **Swift** | Protocol extensions with default implementations | Static dispatch for non-requirement extension methods | Ambiguity error | None | Rejected: the static/dynamic dispatch split is a well-known footgun; Beamtalk has one dispatch (ADR 0006) |
| **Java 8 / Kotlin** | Interface default methods | Flattened-ish; class wins, then explicit `A.super.m()` | Must override | None | Adopted: class wins. Rejected: `A.super.m()`; we alias instead (§4) |
| **Pony** | `trait` (nominal, default methods) + `interface` (structural) | Nominal `is T` | Must override | None | Closest match to §8: nominal *composition* declaration + structural *type*. Pony has no exclusion/alias; we keep Pharo's |
| **Dart** | `mixin` with `on` constraints, `with` | Linearised | Order decides | Yes | Rejected as mixins; `on` is our required-methods list |
| **Ruby** | `module` + `include` / `prepend` | Linearised into the ancestor chain | Order decides | Instance vars by convention | Rejected as mixins; `Comparable`/`Enumerable` are the canonical examples and the names we use |
| **Perl Moose / Raku** | Roles: `with 'R' => { -excludes => …, -alias => … }` | Flattened (roles are traits) | Compile-time conflict unless resolved | Attributes allowed | Confirms exclusion/alias as keyword clauses read fine outside Smalltalk |
| **Elixir** | `use M` → `__using__` macro injects code; `defprotocol` for dispatch | Textual injection | None (last `def` wins, warning) | n/a | `use` is unchecked injection; our flattening is checked injection. Protocols ≈ our ADR 0068. `Enumerable` is the reference API |
| **Erlang / OTP** | `-behaviour(M)` callbacks; `-extends` parse transform | Contract only | n/a | n/a | Behaviours ≈ protocols; no body sharing — the gap this ADR fills |
| **LFE Flavors** | Mixins (`(defflavor … (:mixins …))`) | Linearised | Order | Yes | Rejected as mixins |
| **Gleam** | None; modules and functions | n/a | n/a | n/a | Not applicable |

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
conflicts are not "last def wins". Behaviour callbacks map to required
methods.

**Production operator.** No new process, no new ETS lookup in the dispatch
path, no state-shape change on trait reload. The cost is N recompiles when a
trait used by N classes is edited — visible as N `ClassLoaded`
announcements, exactly like N method edits. Bytecode grows by one copy per
user; §Consequences quantifies.

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
- 🎨 **Language designer**: "The strongest case: since a trait *is* a protocol (§8), the keyword is the only difference. But conformance is *automatic* — a default body on a protocol would silently land on every class that structurally conforms, with no `uses:` line to hang exclusion or aliasing on, or it would need `uses:` anyway, at which point only the keyword differs. The keyword tells the reader whether bodies are present. Kept separate; convertible in place."

### Option C: Newspeak-style mixins (class-in-the-chain)
- 🧑‍💻 **Newcomer**: "Dart and Ruby do it this way; `with` reads fine."
- 🎩 **Smalltalk purist**: "Newspeak's insight that every class body is a mixin is deeper than traits — one concept for inheritance *and* composition."
- ⚙️ **BEAM veteran**: "Each application is a real module, so `super` chains naturally and hot reload is per mixin."
- 🏭 **Operator**: "Nothing is copied; one edit, one reload."
- 🎨 **Language designer**: "Mixins can carry state, which traits here cannot; and linearisation gives a deterministic answer to every conflict without asking the user."
- *Why rejected*: the "deterministic answer" is order-dependence, the thing Schärli's paper was written against. On BEAM each application is a class gen_server plus a module plus one more step in every chain walk (ADR 0032 removed cached tables precisely to keep walks short). Kind-crossing is worse, not better: a mixin carrying `state:` cannot be applied to a `Value`.

### Option D: Extension methods only (`Comparable+DateTime.bt` per class)
- 🧑‍💻 **Newcomer**: "It already works today; I just copy a file."
- 🎩 **Smalltalk purist**: "Class extensions are the Smalltalk way to add behaviour without touching the hierarchy."
- ⚙️ **BEAM veteran**: "One ETS table, no new compiler pass."
- 🏭 **Operator**: "Nothing new to learn or monitor."
- 🎨 **Language designer**: "Composition without a new construct."
- *Why rejected*: the evidence *is* this option in use. Copies drift (`min:`/`max:` in `Integer` and `Float`), nothing checks requirements, conflicts are undetectable, and every extension call pays the ETS probe.

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
- **One concept vs. two (A vs. B).** Language designers and Elixir users lean B; Smalltalkers and the "honesty of form" rule lean A. Resolved for A because the keyword carries information (bodies present or not) and the two are convertible in place.
- **Copy vs. share (A vs. E).** Operators want E's one-reload; the compiler's lexical assumptions make A the only one that works without re-architecting self-sends. Resolved for A; the N-recompile cost is accepted and bounded (§Consequences).
- **Stateless vs. stateful (A vs. F).** Newcomers and Pharo 7 users want F; the class-kind split makes A the only kind-neutral choice today. Deferred, not closed.
- **Header vs. body `uses:`.** Pharo puts it in the header; Beamtalk's header tail is already order-fragile and cannot hold exclusion/alias clauses. Body lines chosen.

## Alternatives Considered

### Protocols with default method bodies
`Protocol define: Comparable` gaining `=>` bodies. Rejected as a *separate*
keyword decision only (§Steelman B): the semantics are identical to §8, and
a protocol can be converted to a trait in place when it grows a body.

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
The BT-274 cancellation position. Rejected by the corpus: seven classes and
~40 hand-written derived methods in the stdlib alone, plus the structural
impossibility of enumeration on an actor.

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
- The type system gains nothing new: a trait is a protocol (§8), and
  ADR 0068's structural rule is untouched.
- Stdlib shrinks: `Comparable` retires ~20 derived-operator bodies and adds
  `between:and:`/`min:`/`max:` to five classes; `Enumerable` retires the
  `SupervisionTree`/`ChangeLog` copies and delivers ADR 0037's Option C.
- Protocol → trait conversion is source-compatible, so protocols can grow
  defaults when evidence appears rather than up front.

### Negative
- **Bytecode duplication**: one copy of each provided method per user. For
  `Comparable` (six methods × six users) and `Enumerable` (~ten × three) this
  is a few KB. A trait used by hundreds of classes would be measurable; the
  mitigation is that provided methods are usually one-liners over required
  ones.
- **N recompiles on trait edit** (§11). Editing `Enumerable` recompiles
  `Collection` and its whole `uses:` set. Per-file compiles are fast and
  already the unit of `>>` live patching; the workspace loader must batch
  them and announce once per user.
- **Reflection must say where a method came from** or `implementorsOf:` looks
  wrong. `origin` on `CompiledMethod` and xref rows is mandatory, not
  optional, and the browse layer must group by trait.
- **No stateful traits** — the `Counting` example needs two required
  accessors the user must write. Deferred with a stated extension path.
- **A second `define:` keyword** next to `Protocol define:`. Mitigated by
  identical body grammar and in-place convertibility.
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
- `Number` may use `Comparable` (dropping the `Integer`/`Float` `min:`/`max:`
  duplicates) while `Integer`/`Float` keep their `@primitive` operators — the
  class-wins rule applies per class, and inheritance still flows from
  `Number`.

## Implementation

Sized for `/plan-adr`. Phases 1–3 are the compiler; 4–5 the runtime and live
system; 6 adoption. Phase 0 is a half-day spike that de-risks the one
assumption this ADR could not verify from source.

| Phase | Scope | Components | Size | Depends on |
|---|---|---|---|---|
| 0 | **Spike**: confirm inherited actor method self-send binding (lexical `<module>:safe_dispatch` vs. `__class_mod__`), since flattening's correctness argument (§6) rests on self-sends resolving in the *using* class's module. Record the finding in `docs/development/debugging.md` | runtime, codegen (read-only) | S | — |
| 1 | **Syntax**: `TraitDefinition` AST (`ast/class.rs`), `ClassDefinition.uses: Vec<TraitUse { trait, type_args, excluding, aliasing, span }>`; `is_at_trait_definition` in the top-level dispatch (`parser/mod.rs:1406-1429`); `parse_trait_body` sharing `parse_protocol_method_signature_with_doc` for requirements and the class method parser for provisions; `uses:` in the class-body loop with ordering and unknown-keyword errors; unparse round-trip; one-definition-per-file and ADR 0119 module naming for traits; lexer nothing (contextual keywords) | `beamtalk-core` source_analysis, ast, unparse | M | — |
| 2 | **Semantics**: `semantic_analysis/trait_registry.rs` (mirrors `protocol_registry.rs`: registration, name collision, cycle check); `trait_expansion.rs` implementing §3–§5 (expansion, exclusion, aliasing, conflict, class-wins, requirements via `resolves_selector`, same-origin rule); `MethodInfo.origin`; implied-protocol registration into `ProtocolInfo`; type-param substitution and `Self`; statelessness validator (§7); all §13 diagnostics; `typed` check on the flattened class | `beamtalk-core` semantic_analysis, type_checker | L | 1 |
| 3 | **Codegen**: feed the flattened `ClassDefinition` to the existing generators (extend the self-extension `merge_method` fold); `methodXref` provenance/origin; `methodSource` from the trait; trait module emission (`__beamtalk_meta`, `register_trait/0`, implied protocol via `generate_protocol_registrations`); cross-package expansion by re-parsing meta-embedded source in the compiler port; `ProjectIndex` trait→users dependency edges; conformance fixture for the trait meta shape | `beamtalk-codegen`, `beamtalk-compiler-port`, `beamtalk-language-service` | L | 2 |
| 4 | **Runtime & reflection**: `beamtalk_trait_registry.erl` (ETS, mirrors protocol registry); `stdlib/src/trait.bt`; `Behaviour traits/allTraits/usesTrait:`; `CompiledMethod origin`; `SystemNavigation usersOf:`; xref `provenance := trait` + browse grouping; `removeSelector:` guard (§11); `beamtalk_xref_methods` schema bump; surface-parity table rows | runtime, stdlib, `docs/development/surface-parity.md` | M | 3 |
| 5 | **Live system**: trait file reload → user recompile fan-out in the workspace loader; `Trait >> sel => …` and `Trait removeSelector:` live patching; `Trait define:` at the REPL; ADR 0105 re-check hookup; ADR 0114 rename (`renameSelector:to:` redirection, trait `renameTo:` with `uses:` reference rows); flush of trait files (ADR 0113); LSP go-to-definition/hover/completion on `origin`; REPL-protocol tests | workspace, REPL, LSP | L | 4 |
| 6 | **Stdlib adoption** (one issue per trait): `Comparable` on `DateTime`, `Duration`, `Uuid`, `String`, `Character`, `Number` (retire `Integer`/`Float` `min:`/`max:`); `Enumerable(E)` on `Collection`, `SupervisionTree`, `ChangeLog`; BUnit tests in `stdlib/test/`; `docs/beamtalk-language-features.md` § Traits; close ADR 0005 Q9 | stdlib, docs | M | 5 |

Phase 1 alone is mergeable (a parsed but unexpanded `uses:` is a "not yet
supported" error); phases 2–3 together give a working compiler; phase 4 is
required before the feature is documented as available, because reflection
without `origin` misleads.

Deferred to follow-up ADRs, explicitly: stateful traits (§7); trait-level
modifiers; `sealed` traits for whole-program optimisation (BT-274's last
criterion — moot while flattening already gives per-class sealing).

## Migration Path

Additive; no existing source changes meaning. Two notes for adopters:

- **Stray keyword lines** in a class body were silently treated as end-of-body
  and become an error (§13). `just test-stdlib` and the parity projects will
  surface any instance.
- **Converting a protocol to a trait** is a file edit (`Protocol define:` →
  `Trait define:`, add bodies) with no consumer changes (§8). The reverse is
  a breaking change for `uses:` sites and follows the normal deprecation
  path.

## References

- Related issues: BT-3526 (this ADR), BT-3523 (epic), BT-274 (original
  design issue, cancelled; superseded here), BT-102 (method combinations —
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
  ADR 0101 (`native:`), ADR 0105 (recheck on reload), ADR 0111/0118/0120/0122
  (`ThreadedIr`), ADR 0112 (`removeSelector:`), ADR 0113 (flush), ADR 0114
  (rename), ADR 0119 (module naming), ADR 0123 (versioned state), ADR 0124
  (`late` slots)
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
