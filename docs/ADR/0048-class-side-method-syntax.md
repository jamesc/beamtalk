# ADR 0048: Class-Side Method Syntax

## Status

Proposed (2026-03-02)

## Context

### History: `class` modifier introduced before metaclasses

ADR 0013 introduced class-side method definitions using a `class` prefix modifier:

```beamtalk
class new -> Foo => @primitive "new"
class pi -> Float => @primitive "pi"
class uniqueInstance =>
    self.uniqueInstance ifNil: [self.uniqueInstance := super new].
    self.uniqueInstance
```

This syntax was designed as a pragmatic shortcut in a world where Beamtalk did not yet have metaclasses. The `class` modifier told the compiler "register this method on the class's dispatch table rather than the instance table." There was no metaclass object to navigate to, so the modifier was the only available mechanism.

ADR 0036 subsequently introduced the full metaclass tower: every class `Foo` now has a real `Foo class` metaclass object backed by the same class gen_server process (virtual tag dispatch). `Counter class`, `Counter class class`, `Metaclass class class == Metaclass class` — all work. The object model is Smalltalk-complete.

With the metaclass tower in place, the original motivation for the `class` modifier — no metaclass to navigate to — no longer holds.

### The Collision Problem

`class` is simultaneously:
1. **A grammar modifier keyword** — the `class` token at declaration level introduces a class-side method definition.
2. **A valid method name** — the `class` unary message returns the receiver's metaclass (e.g., `42 class` returns `Integer class`; `Counter class` returns the metaclass of `Counter`).

The parser cannot reliably distinguish these two roles. ADR 0047 documented this as Ambiguity 2:

```beamtalk
sealed class -> Metaclass => @primitive "classClass"
```

Is this:
- A **sealed** class-side binary method named `->` with argument `Metaclass`?
- A **sealed** instance method named `class` with return type `Metaclass`?

The parser resolved this as the former — generating `class_->/2`, with `Self` unbound. The intended reading was the latter: an instance method named `class` that returns the receiver's metaclass.

ADR 0047 (Arrow token) fixed Ambiguity 1 (`->` as method selector) using a dedicated `TokenKind::Arrow`. It addressed Ambiguity 2 with a per-case lookahead hack: before consuming `class` as a modifier, peek ahead; if `Arrow Identifier FatArrow` follows, treat `class` as the method selector instead. This lookahead is correct for the one known case, but it is a patched exception to a broken general rule, not a fix to the underlying grammar problem.

The underlying problem is structural: the grammar keyword and the method name are the same token. Any method named `class` on any class must thread through the lookahead special case. Any future method whose name coincides with another modifier keyword (`sealed`, `override`) would create the same class of problem.

### The Smalltalk Comparison

In Smalltalk, there is no `class` modifier keyword. There is only the object model. Class-side methods are defined by navigating to the metaclass via a message send:

```smalltalk
"Pharo / Squeak syntax"
(MyClass class) >> #myMethod    "defines on Foo's metaclass — class side"
MyClass >> #myMethod             "defines on MyClass — instance side"
```

`class` is a unary message that returns the metaclass object. It is not special syntax. Defining a method "on the metaclass" is simply defining an ordinary method on an ordinary object that happens to be a metaclass. The `class` modifier collision is absent because Smalltalk has no modifier syntax at all.

Beamtalk's `class` modifier was a syntactic convenience that collapsed the two-step reflective pattern (`navigate to metaclass, then define method`) into a single inline declaration. Now that Beamtalk has real metaclasses (ADR 0036), the original Smalltalk approach is structurally available.

### Scope

There are approximately 57 class-side method definitions across the stdlib following the current `class methodName` pattern. All would need migration under any of the alternatives below.

## Decision

TBD — see Alternatives.

This ADR is drafted for discussion. The three options each represent a distinct position on the tradeoff between Smalltalk fidelity, syntactic minimalism, and migration simplicity. No option has been selected; the analysis and steelman sections are provided to structure the decision.

## Prior Art

### Pharo and Squeak (Smalltalk)

Pharo's image-based browser presents "instance" and "class" as two tabs on the class
definition pane. In expression form (evaluating in the image), class-side methods are
defined by navigating to the metaclass via a message send:

```smalltalk
"Expression syntax — sends the `class` message to reach the metaclass"
(MyClass class) >> #new      "class side"
MyClass >> #balance          "instance side"
```

`class` is a unary message returning the metaclass — not a modifier keyword. There is
no collision with method names because `class` is only ever a message send.

In text form (Tonel file format), the distinction is carried by a `classSide` attribute
on the method definition header, not by inline syntax:

```tonel
{ #category : 'instance creation' }
MyClass >> balance [
    ^balance
]

{ #category : 'instance creation', #classSide : true }
MyClass >> new [
    ^super new initialize
]
```

The receiver in both Tonel entries is `MyClass >>`; the `#classSide : true` attribute
tells the Tonel loader to install the method on the metaclass rather than the class
itself. This is an attribute approach — closer in spirit to Option B (`meta` modifier)
than to a separate metaclass block — but without any syntax collision risk because the
attribute is in the metadata header, not in the method signature.

### Newspeak

Newspeak has no metaclasses. Classes are nestable first-class objects. Class-side behavior is expressed via nested class definitions:

```newspeak
class Counter [
    class var count = 0.       "class-side state"

    class >> increment [       "class-side method (theoretical syntax)"
        count := count + 1
    ]

    >> value [ ^count ]        "instance-side method"
]
```

In practice Newspeak uses nested class slots and the module instantiation model to provide class-like factories. There is no `class` modifier keyword; instead the module/class distinction provides the same scoping. Not directly applicable to Beamtalk's object model.

### Ruby

Ruby uses `class << self` to open the eigenclass (singleton class) for class-side method definitions:

```ruby
class MyClass
  class << self
    def new_instance
      # ...
    end
  end

  def instance_method
    # ...
  end
end
```

Alternatively, prefixing with `self.`:

```ruby
class MyClass
  def self.new_instance   # class-side
    # ...
  end

  def instance_method     # instance-side
    # ...
  end
end
```

Ruby's `self.` prefix is functionally analogous to Beamtalk's `class` modifier, but it uses `self` (not `class`) and applies at the method level. The `class << self` block groups multiple class-side definitions. Neither form creates an ambiguity with method names because `self.` is parsed as a receiver qualifier, not a single modifier token.

### Kotlin

Kotlin uses `companion object` to group class-side definitions:

```kotlin
class MyClass {
    companion object {
        fun create(): MyClass = MyClass()
        const val DEFAULT = "default"
    }

    fun instanceMethod() { ... }
}
```

`companion object` is an explicit nested object declaration, not a method modifier. It is verbose but unambiguous: methods inside a `companion object` block are class-side; everything outside is instance-side. This is structurally analogous to Option A below.

`static` (available for top-level functions and via `@JvmStatic` in companion objects) is Java-inherited syntax that Kotlin considers second-class.

### Swift

Swift uses `static` (for classes that cannot be overridden) and `class` (for overridable class-side methods):

```swift
class MyClass {
    static func create() -> MyClass { return MyClass() }   // not overridable
    class func factory() -> MyClass { return MyClass() }   // overridable in subclasses
    func instanceMethod() { }
}
```

Notably, Swift uses `class` as a modifier keyword in exactly the same position Beamtalk does — and Swift does have the analogous ambiguity that `class` is both the keyword opening a class declaration and a modifier on methods within a class. Swift resolves this by context: at the declaration level inside a class body, `class func` is always a method modifier; there is no method named `class`. Beamtalk's problem is that `class` is a valid method name (the Smalltalk `class` message), which Swift does not have.

`@staticmethod` (Python), `static` (Java, C#, Kotlin, Swift) — the keyword-based approach is the dominant pattern in mainstream languages. The collision with method names is Beamtalk-specific, arising from the Smalltalk heritage where `class` is a ubiquitous unary message.

## User Impact

The following analysis applies across all three options, noting where the options diverge.

### Newcomer

Newcomers have no existing code to migrate and no preconceptions about which syntax is "correct." All three options are learnable. The question is discoverability:

- **Option A** (separate class-side declaration): The metaclass-side declaration visually separates class-side methods from instance-side methods. A newcomer who asks "how do I add a class-side method?" can find it by reading the class definition structure rather than remembering a modifier keyword. However, Beamtalk does not have block delimiters (`[...]`) for class bodies — class definitions are open-ended, with methods continuing until the next class declaration or EOF. Option A requires deciding what the class-side declaration syntax actually looks like in Beamtalk, and how the parser knows where it ends. This is an open design question; see Alternatives Considered for discussion.
- **Option B** (`meta` modifier): Drop-in replacement for `class` modifier. Newcomers who search for "class-side method" in documentation will find `meta methodName`. The keyword is unambiguous but novel — `meta` is not in mainstream language vocabulary for this concept.
- **Option C** (`+` sigil): Objective-C/Swift convention. Familiar to iOS developers. Short. However, it is opaque to anyone without Objective-C background: `+ new` does not self-document.

### Smalltalk Developer

A Smalltalk developer's strongest expectation is the object-model-first approach: class-side methods are defined by navigating to the metaclass. Option A is the closest match to this mental model — `class Foo class [...]` reads as "open the metaclass side of Foo and define methods." The `class` message is familiar; the block syntax follows from the instance-side pattern.

Options B and C use modifier keywords, which are alien to Smalltalk syntax philosophy. However, a Smalltalk developer will immediately understand what they do.

Neither Option B nor Option C collides with the Smalltalk mental model — `meta` and `+` are not method names in Smalltalk — so neither causes confusion, just mild aesthetic disappointment.

### Erlang/BEAM Developer

An Erlang developer cares about clarity of what compiles to what. Options B and C are more predictable: a method prefixed with `meta` or `+` is straightforwardly a class-side function in the compiled module. Option A's separate declaration block is structurally clear as well, but introduces a question: "are `class Foo [...]` and `class Foo class [...]` compiled into the same module?" (Yes, they are — both compile into `foo.beam`.)

All three options produce identical BEAM output; the choice is purely syntactic.

### Tooling Developer (LSP, IDE)

For any option, the compiler must map method definitions to the correct dispatch table (instance-side vs class-side). The classification must be statically determinable from the AST without lookahead hacks.

- Option A: The dispatch side is determined by which block the method appears in. Clean structural distinction; no ambiguity.
- Option B: The dispatch side is determined by the `meta` modifier token, which is not a valid method name. No ambiguity.
- Option C: The dispatch side is determined by the `+` sigil. No ambiguity.

All three options eliminate the Ambiguity 2 lookahead hack. From a tooling perspective, any option is preferable to the status quo.

### Production Operator

No runtime impact. The class-side/instance-side distinction is purely a compile-time concern — it determines which dispatch table a method is registered in, not how it executes. All three options produce identical BEAM output.

## Steelman Analysis

### Option A: Separate Class Body Declaration

**Best case from each cohort:**

- **Newcomer**: "When I open a class definition and want to add a factory method, the separate class-side declaration tells me exactly where to put it. I don't have to remember a modifier; the structure of the source file reflects the structure of the object model — just like switching tabs in a browser."
- **Smalltalk developer**: "This is the honest representation. In Smalltalk, class-side methods are defined on the metaclass, which is a separate object. A separate declaration for the metaclass side acknowledges that truth directly, rather than treating class-side methods as a tagged variant of instance-side methods. The `class` message in `Foo class` is the same `class` message I send in expressions — one concept, not two."
- **BEAM developer**: "The source structure tells me immediately which functions in `foo.beam` are instance methods and which are class-side. I don't have to mentally filter by modifier. The module has two named sections."
- **Language designer**: "It eliminates the `class` modifier keyword specifically, which is the one that collides with a real method name. `sealed` and `override` are not plausible method names, so their collision risk is negligible and they can remain as modifiers. The object model provides the syntax for the one case that mattered."

**Honest tensions:**

- Classes with both instance and class-side methods require two separate top-level declaration blocks, which splits code that conceptually belongs together (e.g., `new` alongside `initialize`). The Pharo browser solves this with tabs; in a flat file, the split is physical and potentially confusing.
- The `class Foo class [...]` form introduces a question about ordering: can there be more than two blocks? (No — one instance-side, one class-side.) Can they interleave? (No.) This rule must be documented and enforced by the parser.
- The `class` message in `Foo class [...]` is parsed as a message send, not a modifier keyword. This requires the parser to handle class body introductions differently depending on whether they are followed by `class` (navigate to metaclass side) or not. This is more complex than a modifier keyword but less ambiguous.

### Option B: `meta` Modifier Keyword

**Best case from each cohort:**

- **Newcomer**: "`meta` is self-documenting. When I see `meta new -> Foo`, I know immediately this is a method on the class itself, not on instances. I don't need to know what a metaclass is. `meta` reads as 'about the class' and that's good enough."
- **Smalltalk developer**: "I can still write `class new -> Foo => ...` after mentally swapping `class` for `meta`. It's a mechanical translation. The distinction between class-side and instance-side is preserved. I lose nothing semantically."
- **BEAM developer**: "Drop-in replacement for the current `class` modifier. The migration is fully mechanical — a sed-like rename. I lose no expressiveness, and I gain clarity: `meta` is never a method name, so this is unambiguous forever."
- **Language designer**: "Minimal change to the grammar. `meta` is added as a modifier keyword in the lexer. No structural change to the declaration syntax. The parser handles `meta methodSelector` identically to how it previously handled `class methodSelector`. Implementation risk is very low."
- **Operator**: "Zero risk. Nothing changes at runtime. Migration is trivially scriptable."

**Honest tensions:**

- `meta` is not a concept in Smalltalk or mainstream OO vocabulary for this purpose. Every other major language uses `static` or `class` or `self.`; `meta` is a Beamtalk invention. Newcomers from Swift will expect `class func`, not `meta`.
- Using `meta` means the `class` message and the `class` method definition keyword coexist, distinguished only by context (expression position vs. declaration position). The ambiguity is fixed by replacing one keyword — but the broader pattern of modifiers-vs-method-names is still a latent issue if future modifiers are introduced.
- `meta` could reasonably be confused with metaprogramming or metadata in other languages, creating a false connotation.

### Option C: `+` Sigil

**Best case from each cohort:**

- **Newcomer from Objective-C/Swift**: "I already know what `+` means. This is the right choice for this ecosystem. Short, clear, and conventional for developers coming from Apple platforms."
- **Language designer (conciseness)**: "`+` is a single character with zero ambiguity at the declaration level. No keyword to add to the lexer, no collision possible. It is the smallest possible addition to the syntax."
- **BEAM developer**: "`+` is unambiguous from any Erlang/BEAM perspective. No Erlang concept collides with it. The generated code is identical."
- **Tooling developer**: "Sigil-prefixed methods are trivially identifiable in any parser. The AST representation — a leading `+` on the method definition — is more compact than a modifier keyword."

**Honest tensions:**

- `+` is opaque to anyone without Objective-C background. A developer coming from Python, Ruby, Kotlin, or Java has no basis for inferring what `+ new -> Foo =>` means. The sigil approach optimises for conciseness over discoverability.
- `+` as a unary prefix in method definitions could create visual confusion with binary method selectors in expression position. In `x + y`, `+` is a binary message send. In `+ new -> Foo =>`, it is a declaration prefix. The contexts are syntactically distinct but visually similar in a quick scan.
- The full convention (`+` for class-side, implicit `-` for instance-side) is Objective-C-specific. Beamtalk has no `- instanceMethod` prefix for instance-side methods. Adopting only half of the convention (`+` for class-side, nothing for instance-side) is internally consistent but breaks the Objective-C analogy that justifies the sigil in the first place.

### Cross-Cohort Tensions

- Smalltalk developers and language designers favour Option A for purity and forward-looking correctness; they are willing to accept the two-block split as the honest representation of the object model.
- BEAM developers and operators lean toward Option B or Option C for migration simplicity and minimal grammar change; they prefer not to restructure existing class definitions.
- Newcomers are neutral between all three options, with Option B (`meta`) being most self-documenting and Option C least.
- Tooling developers favour any of the three options over the status quo, with a mild preference for structural options (A) over modifier keywords, as the dispatch side is determined by block containment rather than token inspection.

## Alternatives Considered

### Option A: `SuperClass class subclass: Foo class` Declaration

Class-side methods are placed in a second `subclass:` declaration in the same `.bt`
file, using `X class subclass: Y class` to declare the metaclass side alongside the
instance side. Both declarations live in the same file — no file split required.

```beamtalk
// Float.bt — instance side and class side in the same file

Number subclass: Float
    state: value = 0.0

    + other -> Float => @primitive "+"
    sqrt -> Float => @primitive "sqrt"
    abs -> Float => @primitive "abs"

Number class subclass: Float class
    pi -> Float => @primitive "pi"
    nan -> Float => @primitive "nan"
```

The first declaration ends when the parser sees the second `subclass:` header — the
same rule that already terminates any class body. No new terminator syntax is needed.

The `X class subclass: Y class` form uses only existing tokens and the existing
`subclass:` keyword. The one new parser rule: when both the superclass and the class
name in a `subclass:` header carry a trailing `class` message, register the methods
on the metaclass of `Y` rather than on `Y` itself. The metaclass inheritance
(`Float class` inheriting from `Number class`) is made explicit — and it is real.

For a class with both instance and class sides:

```beamtalk
// TranscriptStream.bt

Object subclass: TranscriptStream
    classVar: uniqueInstance = nil

    class -> Metaclass => @primitive "classClass"   // unambiguous: method named `class`
    show: text => self.buffer := self.buffer ++ #(text)

Object class subclass: TranscriptStream class
    new => self error: 'Use uniqueInstance instead'
    uniqueInstance =>
        self.uniqueInstance ifNil: [self.uniqueInstance := super new].
        self.uniqueInstance
```

For a class with only instance-side methods (the common case), no second declaration
is needed — nothing changes.

**In stdlib migration terms:** The 57 class-side method definitions are extracted into
a second `X class subclass: Y class` declaration in the same file. The instance-side
declaration is unchanged.

**Tradeoffs:**
- Reuses `subclass:` entirely — no new syntax form, no new keywords, no new tokens.
- `class` has one meaning throughout: the metaclass-navigation message.
- Eliminates all modifier keyword collision, present and future.
- Makes metaclass inheritance explicit and correct (`Float class` inherits from `Number class`).
- Both sides live in the same file; termination uses the existing open-ended body rule.
- `classVar:` naturally stays in the instance-side declaration (class variables belong
  to the class object, not the metaclass).
- Requires one new parser recognition rule for `X class subclass: Y class` headers.

### Option B: `meta` Modifier Keyword

Replace the `class` modifier with `meta`. Every existing `class methodName` definition becomes `meta methodName`. The grammar and parser change are purely mechanical.

```beamtalk
Object subclass: TranscriptStream
    classVar: uniqueInstance = nil

    // Instance method — fully unambiguous
    class -> Metaclass => @primitive "classClass"
    show: text => self.buffer := self.buffer ++ #(text)

    // Class-side methods — `meta` is never a method name
    meta new => self error: 'Use uniqueInstance instead'
    meta uniqueInstance =>
        self.uniqueInstance ifNil: [self.uniqueInstance := super new].
        self.uniqueInstance
```

Stdlib examples:

```beamtalk
meta new -> Foo => @primitive "new"
meta pi -> Float => @primitive "pi"
```

**In stdlib migration terms:** All 57 occurrences of `class methodName => ...` become `meta methodName => ...`. Fully mechanical. No structural changes.

**Tradeoffs:**
- Minimum viable change. Parser adds `meta` as a modifier keyword; the rest of `declarations.rs` is unchanged.
- `meta` is unambiguous: it is not a valid Smalltalk method name and is unlikely to conflict with any future method name.
- Loses the Smalltalk object-model justification; retains the modifier-keyword approach.
- `meta` connotes metaprogramming/metadata in other ecosystems, which could mislead newcomers.
- Does not resolve the structural grammar concern that modifier keywords and method names share the same namespace — future additions of modifier keywords require the same vigilance about collision.

### Option C: `+` Sigil

Adopt the Objective-C/Swift convention: prefix class-side method definitions with `+`. Instance-side methods carry no prefix (the unmarked default).

```beamtalk
Object subclass: TranscriptStream
    classVar: uniqueInstance = nil

    // Instance method — no prefix, `class` is unambiguously a method name
    class -> Metaclass => @primitive "classClass"
    show: text => self.buffer := self.buffer ++ #(text)

    // Class-side methods — `+` sigil
    + new => self error: 'Use uniqueInstance instead'
    + uniqueInstance =>
        self.uniqueInstance ifNil: [self.uniqueInstance := super new].
        self.uniqueInstance
```

Stdlib examples:

```beamtalk
+ new -> Foo => @primitive "new"
+ pi -> Float => @primitive "pi"
```

**In stdlib migration terms:** All 57 occurrences of `class methodName => ...` become `+ methodName => ...`. Fully mechanical.

**Tradeoffs:**
- Shortest syntax. `+` is a single character.
- Unambiguous at the declaration level: `+` before a method selector is not a binary send in this position.
- Familiar to Objective-C/Swift developers.
- Opaque to everyone else — no self-documentation.
- Breaks with Beamtalk's Smalltalk heritage more sharply than Option B.
- The `+` character appears in many contexts in Beamtalk (binary message send, string/collection operations). At the declaration level it is unambiguous, but visually noisy in mixed files.
- Objective-C uses `-` for instance-side methods alongside `+` for class-side. Beamtalk would only adopt the `+` half of the convention, which makes the analogy incomplete.

## Consequences

The consequences below are conditional on the option selected.

### Option A Consequences

**Positive:**
- Reuses `subclass:` entirely — no new keywords, no new tokens, no new syntax form.
- `class` has one meaning throughout the language: the metaclass-navigation message.
- Eliminates the `class`-modifier collision; `sealed` and `override` remain as modifiers
  on individual methods within either declaration and are unaffected.
- Makes metaclass inheritance explicit and structurally correct.
- Both sides live in the same file; no forced file split.
- `classVar:` placement is naturally resolved: instance-side declaration, as now.

**Negative:**
- Classes with both instance and class-side methods require two `subclass:` declarations
  in the same file, which may obscure the relationship between `new` (class-side) and
  `initialize` (instance-side).
- One new parser recognition rule for `X class subclass: Y class` headers.
- Migration requires extracting class-side methods into a new declaration, not just a
  token rename.

**Neutral:**
- The open-ended body termination rule is unchanged; the second `subclass:` header
  terminates the first declaration exactly as any other `subclass:` would today.

### Option B Consequences

**Positive:**
- Minimum parser change: add `meta` as a modifier keyword, replace `class` modifier handling.
- Migration is fully mechanical: `s/\bclass \b/meta /g` in method definition position.
- `meta` is permanently non-colliding with any plausible method name.
- All existing class definitions retain their single-block structure.

**Negative:**
- The structural grammar concern (modifier keywords and method names share token space) is addressed for this instance but not resolved in general. Future modifier keywords require the same vigilance.
- `meta` is a new vocabulary item that none of the reference languages use in this context.
- Abandons the Smalltalk object-model justification in favour of syntactic convenience.

**Neutral:**
- The `class` modifier is removed from the grammar entirely; any documentation or tooling that references "class prefix for class-side methods" requires updating.
- No structural change to class definition AST nodes: `class_methods` continues to be a separate `Vec<MethodDefinition>` within the single `ClassDefinition`.

### Option C Consequences

**Positive:**
- Shortest migration path; `+` replaces the `class` modifier with one character.
- Unambiguous at the declaration level.
- Familiar to iOS/Apple-ecosystem developers.

**Negative:**
- Least self-documenting option; requires documentation reference to learn what `+` means.
- Breaks more sharply with Smalltalk heritage than Options A or B.
- Adopting only `+` without `-` makes the Objective-C analogy partial.
- `+` as a declaration-level sigil may surprise developers encountering Beamtalk source for the first time.

**Neutral:**
- Parser handles `+` in method-definition position as a class-side indicator rather than a binary selector token.
- No runtime impact.

## Implementation

### Affected Components

| Component | Option A | Option B | Option C |
|---|---|---|---|
| `crates/beamtalk-compiler/src/parser/lexer.rs` | No change | Add `meta` keyword token | No change (or minimal: `+` sigil detection in declaration context) |
| `crates/beamtalk-compiler/src/parser/token.rs` | No change | Add `TokenKind::Meta` | No change |
| `crates/beamtalk-compiler/src/parser/declarations.rs` | New declaration form for `class Foo class [...]` | Replace `class`-modifier handling with `meta`-modifier handling | Replace `class`-modifier handling with `+`-sigil detection |
| `crates/beamtalk-compiler/src/ast.rs` | `ClassDefinition` gains optional `class_side_body`; or two separate `ClassDefinition` nodes per class | No change to AST structure | No change to AST structure |
| `stdlib/src/*.bt` | ~57 definitions extracted into new `class Foo class [...]` blocks | ~57 `class methodName` → `meta methodName` renames | ~57 `class methodName` → `+ methodName` renames |
| `crates/beamtalk-compiler/src/codegen/generated_builtins.rs` | Regenerate after stdlib migration | Regenerate after stdlib migration | Regenerate after stdlib migration |

### Migration Scope

The stdlib contains approximately 57 class-side method definitions distributed across these files (representative, not exhaustive):

- `stdlib/src/Object.bt` — `class new`, `class error:`, etc.
- `stdlib/src/Class.bt` — `sealed class` (the `classClass` collision case), `class name`
- `stdlib/src/Boolean.bt`, `stdlib/src/True.bt`, `stdlib/src/False.bt` — class-side constructors
- `stdlib/src/Integer.bt`, `stdlib/src/Float.bt` — `class pi`, numeric factories
- `stdlib/src/Array.bt`, `stdlib/src/OrderedCollection.bt` — `class new`, `class new:`
- `stdlib/src/TestCase.bt` — `class allTestSelectors`, `class run`

For Options B and C, migration is token-level: replace the modifier/sigil and no structural reorganisation is needed. For Option A, each file with class-side methods gains a new top-level `class Foo class [...]` declaration block.

### Superseding ADR 0013

This ADR supersedes the class-side method syntax specified in ADR 0013 Section 2 ("Class-Side Methods — Syntax: `class` prefix"). ADR 0013's semantic decisions (class-side dispatch, inheritance, `super` in class-side methods) are unaffected — only the surface syntax changes.

### ADR 0047 Relationship

ADR 0047's Ambiguity 2 fix (lookahead in `parse_method_definition` at line 619) was a targeted patch for the `sealed class -> Metaclass =>` case. Whichever option is chosen here, that lookahead patch becomes unnecessary and should be removed:

- Option A: `class` is no longer a modifier keyword; the lookahead is dead code.
- Option B: `class` is replaced by `meta`; the `class`-as-modifier branch is removed entirely.
- Option C: `class` is replaced by `+`; same as Option B.

## Migration Path

This is a breaking change. All existing `class methodName => ...` definitions in `.bt` files require migration.

### For Options B or C (Mechanical)

The migration is a token-level rename and can be performed with a targeted source transformation. Conceptually:

```
// Before (current syntax)
class new -> Foo => @primitive "new"
class pi -> Float => @primitive "pi"
sealed class -> Metaclass => @primitive "classClass"

// After (Option B)
meta new -> Foo => @primitive "new"
meta pi -> Float => @primitive "pi"
sealed class -> Metaclass => @primitive "classClass"   // no change: `class` is now unambiguously the method name

// After (Option C)
+ new -> Foo => @primitive "new"
+ pi -> Float => @primitive "pi"
sealed class -> Metaclass => @primitive "classClass"   // no change: `class` is now unambiguously the method name
```

After renaming, `generated_builtins.rs` must be regenerated via `just build-stdlib`.

### For Option A (Structural)

Each class with class-side methods is split into two declarations. A manual review is required because `classVar:` placement must be decided per-class. The transformation is not purely textual.

Example migration for `TranscriptStream`:

```beamtalk
// Before (current syntax — single block)
Object subclass: TranscriptStream
    classVar: uniqueInstance = nil

    show: text => ...
    class new => self error: 'Use uniqueInstance instead'
    class uniqueInstance =>
        self.uniqueInstance ifNil: [self.uniqueInstance := super new].
        self.uniqueInstance

// After (Option A — two blocks)
Object subclass: TranscriptStream
    classVar: uniqueInstance = nil

    show: text => ...

class TranscriptStream class [
    new => self error: 'Use uniqueInstance instead'
    uniqueInstance =>
        self.uniqueInstance ifNil: [self.uniqueInstance := super new].
        self.uniqueInstance
]
```

### User-Facing Communication

Because this is a syntax breaking change, migration must be coordinated with a compiler version increment. The compiler should emit a clear diagnostic for any source file that still uses the old `class methodName` modifier form after the migration deadline:

```
error: `class` is no longer a method modifier — use `meta methodName` (Option B) or `+ methodName` (Option C)
  --> stdlib/src/Float.bt:12:5
   |
12 |     class pi -> Float => @primitive "pi"
   |     ^^^^^
   |
   = note: see ADR 0048 for migration instructions
```

## References

- Related issues: BT-1003 (stdlib return type annotation audit — discovered Ambiguity 2), BT-1018 (parser ambiguity cleanup)
- Related ADRs: ADR 0013 (class-side methods introduced — this ADR supersedes ADR 0013 Section 2 on class-side method syntax), ADR 0036 (full metaclass tower — makes the original motivation for the `class` modifier obsolete), ADR 0047 (Arrow token disambiguation — fixed Ambiguity 1; documented Ambiguity 2 as a structural grammar problem requiring this ADR)
- Prior art: Pharo by Example, Classes and Metaclasses chapter; Smalltalk-80 Blue Book Chapter 14 (Classes as Objects); Objective-C Programming Language Guide (instance vs. class methods); Swift Language Reference (static and class methods); Kotlin Reference (companion objects)
