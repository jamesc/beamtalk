# ADR 0147: Map-Shape Types for Keyed Dictionaries

## Status
Accepted (2026-11-11)

## Context

### Problem statement

`Dictionary(K, V)` gives every value the same type. Real Beamtalk code often
uses a dictionary as a record instead: each key has its own value type. The
checker only sees `V`, usually `Object` or `Dynamic`, so every read loses its
type and the code narrows by hand.

The clearest case is option maps. `buccaneer` declares:

```beamtalk
/// - `#maxFrameSize` (Integer, default `8000000`) — ...
/// - `#validateTextFrames` (Boolean, default `true`) — reject non-UTF-8 text frames.
/// - `#compress` (Boolean, default `false`) — ...
/// ...
type WebSocketOptions = Dictionary(Symbol, Object)
```

The same shape is then written twice more: as a key list
(`class websocketKeys -> List(Symbol)`) and as a runtime validator
(`class checkWebSocketOptions:`). The reader casts each value back
(`src/pipeline.bt`):

```beamtalk
ws := config websocket
// Types checked in BuccaneerConfig>>checkWebSocketOptions:.
validateText :: Boolean := ws at: #validateTextFrames ifAbsent: [true]
maxFrameSize :: Integer := ws at: #maxFrameSize ifAbsent: [8000000]
```

So the shape is stated three times (doc comment, key list, validator), and
none of the three is checked at compile time. A caller who writes
`#{#maxFramSize => 1}` finds out at startup. These annotated-local casts are
also a large share of the unchecked narrowing casts BT-4719 wants to make
visible.

The second case is FFI. Erlang specs describe keyed maps precisely
(`#{k := T, k2 => T2}`), but `beamtalk_spec_reader:map_type/1` maps every map
type without a `'$beamtalk_class'` tag to bare `Dictionary`. ADR 0075 lists
"typed maps" as future work. `beamtalk-http` shows the cost: it calls
`(Erlang uri_string) parse: url`, whose spec names seven keys with their
types, and then re-checks each value with `isKindOf:`.

### Current state

A survey of literal-key `at:` reads in the stdlib's sibling libraries:

| Repo | Symbol keys | String keys | String keys come from |
|---|---|---|---|
| buccaneer | 89 | 0 | – |
| beamtalk-http | 11 | 0 | – |
| catalina | 16 | 0 | – |
| exdura | 34 | 5 | JSON request bodies |
| symphony | 37 | 17 | YAML config, codex JSON-RPC |
| beamtalk-linear | 0 | 36 | Linear GraphQL responses |
| beamtalk-agents | 0 | 56 | Claude and codex stream-json |

The split is clean. Symbol keys are internal option and config maps, plus
FFI maps. String keys are decoded external data (JSON, YAML). This ADR covers
the first group only; see "String keys" under Alternatives Considered.

Eleven aliases across buccaneer, catalina and beamtalk-http have the form
`type XOptions = Dictionary(Symbol, Object | Dynamic)` with the keys listed
in a doc comment.

What already exists in the compiler:

- **Singleton symbol types.** `#foo` is a type and a subtype of `Symbol`
  (ADR 0108, BT-2627).
- **Map literals lose the pairing.** `infer_map_literal`
  (`type_checker/inference/literals.rs`) joins keys and values separately:
  `#{#a => 1, #b => "x"}` is `Dictionary(#a | #b, Integer | String)`.
- **Map patterns take symbol and string keys.** `MapPatternKey` has
  `Symbol` and `StringLit` (`ast/pattern.rs`).
- **The checker already inspects map literals at call sites** for
  `new:` / `spawnWith:` field checking (`type_checker/validation.rs`).
- **`Dictionary` is immutable** and backed by an Erlang map.
- **`...` is already a token** (`TokenKind::Ellipsis`, used for rest patterns).

### Constraints

- Types are erased (ADR 0025). A shape cannot make untrusted data safe.
- Aliases expand eagerly and cannot be recursive (ADR 0108).
- Assignability is partly string-based today. `is_assignable_to` takes type
  strings, treats a leading `#` as a singleton, and `split_top_level` tracks
  only `()`. The spec reader also hands types to Rust as strings. A shape's
  `#{`, `|` and `,` inside braces break all three unless handled.
- A new `InferredType` variant touches subtyping, unions, the set-theoretic
  operators (ADR 0102), narrowing, display, spec generation and the LSP. The
  cost grows with every operation made shape-aware, so the set of shape-aware
  operations must be small and explicit.

## Decision

Add **shape types**: a structural type for a dictionary with known symbol
keys, each with its own value type. A shape is written like the map literal it
describes, and is usable anywhere a type is: annotations, `type` aliases,
generic arguments, unions.

```beamtalk
type WebSocketOptions = #{
  ?#enabled => Boolean,
  ?#maxFrameSize => Integer,
  ?#maxFragmentedMessageSize => Integer,
  ?#validateTextFrames => Boolean,
  ?#compress => Boolean,
  ?#deflateOptions => Dictionary(Symbol, Object),
  ?#maxInflateRatio => Integer,
  ?#primitiveOpsModule => Object,     // reserved, accepted for Bandit parity
  ?#logProtocolErrors => Object       // reserved, accepted for Bandit parity
}
```

### Syntax

```text
ShapeType  := '#{' [ Field { ',' Field } [ ',' '...' ] | '...' ] '}'
Field      := [ '?' ] Key '=>' Type
Key        := SymbolLiteral
```

- `#key => T` is a **required** key. `?#key => T` is an **optional** key.
- A trailing `...` makes the shape **open**: other keys may be present.
  Without it the shape is **closed**.
- Keys are symbol literals, in any form the language accepts for a symbol.
- `,` and `?` are both binary-selector characters, so `Integer,?#b` lexes as
  one `,?` token today. In a shape the parser splits it; no whitespace is
  required.
- A value type is any type, including another shape, so shapes nest.

Optional is a property of the key, not the value. `?#k => T` means the key
may be missing. `#k => T | Nil` means the key is present and may hold `nil`.

### Semantic model

The model is Elixir's set-theoretic map types, the same source ADR 0102 took
its operators from. A shape denotes a set of maps:

- A required field `#k => T` says the key is present with a value in `T`.
- An optional field `?#k => T` says the key holds a value in `T` or is
  absent. It is Elixir's `if_set(T)`, written on the key.
- A closed shape says every unnamed key is absent. An open shape says
  nothing about unnamed keys.

Subtyping is inclusion between those sets. The rules below are that
inclusion spelled out, not a separate design.

### Subtyping

Dictionaries are immutable, so shapes are covariant in their value types.

**Shape to shape.** `S` is assignable to `T` when all of these hold:

1. Every required key of `T` is required in `S`, with an assignable value type.
2. Every optional key of `T` that `S` has (required or optional) has an
   assignable value type. If `S` lacks it, `S` must be closed.
3. Every key of `S` that `T` does not name is allowed only if `T` is open.
4. If `S` is open, `T` is open.

**Shape to `Dictionary(K, V)`.** A closed shape is assignable when every key
type is assignable to `K` and every value type to `V`. An open shape's unnamed
entries are `Object`-keyed and `Object`-valued, so it is assignable only when
`K` and `V` accept `Object` (or are `Dynamic`). Bare `Dictionary` accepts any
shape.

**`Dictionary` to shape.** Follows the existing gradual rule for generics
(`type_args_match`):

- Bare `Dictionary`, and a `Dictionary` with a `Dynamic` argument, are
  compatible with any shape, silently. `Dictionary new` and
  `Dictionary(Symbol, Dynamic)` therefore keep working where a shape is
  expected.
- A fully typed `Dictionary(K, V)` (for example `Dictionary(Symbol, Object)`)
  is not assignable. An annotated local remains the way to assert it, and it
  is the same unchecked narrowing cast it is today (BT-4719 decides how
  visible that is).

### Map literals infer shapes

A map literal whose keys are all symbol literals infers a closed shape, with
nested literals inferring nested shapes:

```beamtalk
opts := #{#body => "{}", #timeout => 5000}
// #{#body => String, #timeout => Integer}
```

Any other literal (a computed or non-symbol key) infers `Dictionary(K, V)` as
today. Checking a literal against a declared shape is then ordinary
shape-to-shape assignability, including through an unannotated local, with
diagnostics that name the offending key.

This is needed, not optional. Without it `opts := #{...}` followed by passing
`opts` to a shape-typed parameter would warn, because the local would be a
fully typed `Dictionary`.

The `Dictionary(K, V)` view of an inferred shape (see below) is exactly the
type `infer_map_literal` gives today, so sends that are not shape-aware see
no change.

### Shapes in the type algebra (ADR 0102)

- **Union.** `S | T` stays a union of two shapes. Shapes are never merged
  into one wider shape.
- **Intersection, difference, negation.** Stored with ADR 0102's existing
  forms and not simplified fieldwise in this ADR. `Dynamic` behaves as it
  does everywhere else in ADR 0102.

The fieldwise intersection rule follows from the model and is recorded here
for the narrowing work (BT-4709) to implement, since that is its only user.
A key in both sides takes the intersection of the value types and is required
if either side requires it. A key in one side only is kept if the other side
is open; if the other side is closed it is dropped when optional and makes
the result `Never` when required. A key whose value types intersect to
`Never` is dropped if optional on both sides and makes the result `Never`
otherwise. The result is open only if both sides are. `includesKey: #k` in
the true branch is then `S & #{#k => Object, ...}`.

### Diagnostic severity

Shape diagnostics follow ADR 0100: default severity is `Warning`, never a
build-stopping error unless a project opts in.

| Situation | Default |
|---|---|
| `at:` / `at:ifPresent:` / map pattern on a key a closed shape does not name | `Warning` (the read can only fail or never match) |
| `at:ifAbsent:` on a key a closed shape does not name | `Warning` (the block always runs) |
| Literal checked against a shape: missing required key, unknown key in a closed shape, wrong value type | `Warning` |
| Optional key read with `at:`, or unnamed key in an open shape | silent |

### Shape-aware operations

Only these operations use the per-key types. The key argument must have a
singleton symbol type (`#k`). A literal `#k` has one; so does a local bound
to one. A key typed as a union of singletons answers the union of the
per-key results.

| Operation | Required key | Optional key | Key not in shape |
|---|---|---|---|
| `at: #k` | `T` | `T` (raises at run time if absent, as today) | closed: diagnostic; open: `Object` |
| `at: #k ifAbsent: [b]` | `T` | `T \| B` | closed: diagnostic; open: `Object \| B` |
| `at: #k ifPresent: [:v \| ...]` | block gets `T` | block gets `T` | closed: diagnostic; open: block gets `Object` |
| map pattern `#{#k => v}` | `v :: T` | `v :: T` | closed: diagnostic; open: `v :: Object` |
| `at: #k put: v` | shape with `#k => typeof(v)`, required | same | closed or open: shape gains `#k`, required |

`at:put:` is here because real code builds option maps with it
(`options at: #handler put: handler` in `Buccaneer start:handler:`), and a
`Dictionary` result would not be accepted where the shape is expected.

**Union receivers.** A keyed read on a union (`S1 | S2`, `S | Nil`,
`S | Tuple`) answers the union of each member's result, with non-shape
members typed as they are today. The "key not in shape" diagnostic fires only
when every member is a closed shape lacking the key.

**`isKindOf: Dictionary`** narrowing keeps the shape members of a union: a
shape is a `Dictionary`.

**Every other message** treats the shape as `Dictionary(K, V)`, where `K` is
the join of the key types and `V` the join of the value types (`Object` for
both if the shape is open). That includes any keyed message with a key that
is not a singleton symbol, `keys`, `do:`, `collect:`, and, in this ADR,
`removeKey:` and `merge:`, which answer a `Dictionary(K, V)`.

### Runtime

None, and no change to generated code. Shapes are erased like every other
annotation, and a shape in a signature still emits `map()` in the Erlang
`-spec`, as `Dictionary` does today. They describe maps the
program builds itself (options, config) and maps a trusted FFI spec describes.
They are not a way to validate external data: that should still be parsed
into `Value` classes at the boundary, as `beamtalk-linear`'s `Issue fromNode:`
does.

### FFI mapping

The spec reader maps Erlang map types to shapes:

| Erlang field | Beamtalk |
|---|---|
| `k := T` (`map_field_exact`, atom key) | `#k => T` |
| `k => T` (`map_field_assoc`, atom key) | `?#k => T` |
| a non-singleton key field alongside atom keys (`atom() => term()`) | trailing `...` |
| a field keyed by `true`, `false` or `nil` | trailing `...` (those atoms are not symbols in Beamtalk) |
| no singleton keys at all (`#{atom() => integer()}`, `map()`) | `Dictionary`, as today |
| `'$beamtalk_class' := C` | the class `C`, as today (checked first) |

Erlang type syntax has no binary-literal keys, so symbol keys cover every
keyed map a spec can describe. Turning a typed domain field
(`atom() => integer()`) into `...` loses its value type; that is accepted.

Value types are whatever the reader already produces for them, which is often
loose (`unicode:chardata()` is `Dynamic | String | Binary`). A shape does not
make those tighter; it adds the key set and keeps each key's type separate.

This applies to parameters as well as results: an Erlang function that takes
a keyed map now expects a shape. The gradual rule and literal inference above
keep existing callers quiet unless they pass a fully typed `Dictionary`.

FFI specs are trusted exactly as ADR 0075 already trusts them. Emitting
shapes back into Erlang `-spec`s is deferred (see below).

### Worked examples

**Config (buccaneer).** With the alias above:

```beamtalk
ws := config websocket
validateText := ws at: #validateTextFrames ifAbsent: [true]    // Boolean
maxFrameSize := ws at: #maxFrameSize ifAbsent: [8000000]       // Integer
```

The two casts and the comment that excuses them go. With `ConfigOptions`
also written as a shape (`?#websocket => WebSocketOptions`, ...), the call
site is checked too:

```beamtalk
Buccaneer start: #{#handler => h, #websocket => #{#maxFramSize => 1}}
// warning: no key #maxFramSize in WebSocketOptions (did you mean #maxFrameSize?)
```

The runtime validator stays for value-range checks (positive integers) and
for callers that build the map dynamically.

**Options (beamtalk-http).**

```beamtalk
type RequestOptions = #{
  ?#headers => List(List(String)),
  ?#body => String,
  ?#timeout => Integer,
  ?#maxBodyBytes => Integer | #unlimited
}

HTTPClient request: #patch url: u options: #{#body => "{}", #timeout => "5s"}
// warning: #timeout expects Integer, got String
```

**FFI (`uri_string:parse/1`).** The OTP type is:

```erlang
-type uri_map() ::
  #{fragment => unicode:chardata(), host => unicode:chardata(),
    path => unicode:chardata(), port => non_neg_integer() | undefined,
    query => unicode:chardata(), scheme => unicode:chardata(),
    userinfo => unicode:chardata()}.
```

`parse/1` returns `uri_map() | error()`. Today that reads as
`Dictionary | Tuple(Symbol, Symbol, Dynamic)`; with this ADR it is a closed
shape with seven optional keys, or the tuple. In `HTTPClient urlParts:`:

```beamtalk
raw := (Erlang uri_string) parse: url
(raw isKindOf: Dictionary) ifFalse: [^self httpError: "Invalid URL"]
port := raw at: #port ifAbsent: [80]       // Integer | Symbol, was Dynamic
scheme := raw at: #shceme ifAbsent: [nil]  // warning: no key #shceme
```

The gain is the key set and the port type. The text fields stay
`Dynamic | String | Binary` because that is what `unicode:chardata()` maps
to, so the `textOf:` conversions in that method stay.

### REPL

```beamtalk
type Point = #{#x => Integer, #y => Integer}
// => "Point"
p :: Point := #{#x => 1, #y => 2}
// => #{#x => 1, #y => 2}
p at: #x
// => 1
p at: #z
// warning: no key #z in Point
// => ERROR: ... key not found (raised at run time, as today)
q :: Point := #{#x => 1}
// warning: missing required key #y for Point
```

`:help Point` shows the expanded shape, as it does for any alias (ADR 0108).
Diagnostic wording is illustrative and is settled in implementation.

### Single sources of truth

- **Keys** are symbol names. If string keys are added later, shape keys must
  reuse the pattern-key type (`MapPatternKey`: symbol or string literal),
  not a second enum.
- **Type display and alias provenance** go through ADR 0108's display-name
  path, so a diagnostic says `WebSocketOptions`, not the expanded shape.
- **The string form of a type.** Shapes must be parsed from type strings by
  one brace-aware routine shared by `is_assignable_to`, `split_top_level`
  and the FFI type-string reader, not three ad hoc checks.
- **Rust and Erlang** meet at the type string the spec reader writes. A
  shared fixture (Erlang map spec, the string the reader emits, the shape
  Rust parses) keeps the two sides in step; no hand-synchronised table.
- **Options documentation.** Once an alias is a shape, the key list in its
  doc comment and helpers like `websocketKeys` duplicate it. Deriving those
  from the shape is future work (see below), not a reason to keep both.

## Prior Art

| Language | Example | Notes |
|---|---|---|
| Erlang typespecs | `#{name := binary(), age => integer()}` | `:=` required, `=>` optional. Closed unless a general key field is present. Checked by Dialyzer and eqWAlizer. |
| Elixir (set-theoretic, 1.17+) | `%{name: binary(), age: if_set(integer())}`, `%{..., name: binary()}` | Closed by default, leading `...` opens. Optional is `if_set` on the value. |
| Hack | `shape('a' => int, ?'b' => string, ...)` | Closed by default, `...` opens, `?` prefix marks optional. |
| Typed Clojure | `(HMap :mandatory {:a Num} :optional {:b Str} :complete? true)` | Mandatory, optional and absent keys; partial by default. |
| TypeScript | `{a: number; b?: string}` | Open by width subtyping, but fresh literals get excess-property checks. |
| Python | `class M(TypedDict, total=False)`, `NotRequired[int]`, `closed=True` (PEP 728) | Started open; PEP 728 added closed because open could not type `**kwargs` or catch typos. |
| Gleam | – | No shapes. `Dict(k, v)` is homogeneous; records are custom types. |
| Pharo / Squeak | – | No static shapes. Idiom is a real object, or a `Dictionary` checked at run time. |

**Adopted.** Elixir's semantic model: shapes are sets of maps, optional is
"value or absent", closed by default with `...` to open, and shapes take
part in union and intersection. This keeps shapes consistent with ADR 0102,
which adopted the same model for unions and singletons. Hack's surface for
the spelling: literal-like syntax and a `?` prefix. TypeScript's literal-site
checking. Erlang is the FFI wire format only: its `:=` / `=>` fields
translate one-to-one into required and optional keys.

**Rejected.** Erlang's `:=` / `=>` spelling: in a Beamtalk literal `=>` means
"this key is present", so using `=>` for optional would invert it. Elixir's
spelling of optional as `if_set(T)` on the value: the meaning is adopted, but
written on the key it reads better beside `T | Nil`. Open-by-default (TypeScript, early TypedDict): it gives up the
typo check that motivates this ADR, and Python's history shows closed gets
added later anyway.

## User Impact

- **Newcomer.** The type looks like the value. Misspelt option keys become
  compile-time errors with a suggestion instead of a startup failure. The
  `?` prefix and `...` are two small things to learn.
- **Smalltalk developer.** No precedent in Smalltalk. Nothing changes for
  objects: a shape is still a `Dictionary` answering the same messages. The
  guidance stays "data with behaviour is a class".
- **Erlang/Elixir developer.** Map specs they already write arrive typed.
  Beamtalk signatures still emit `map()`, so Dialyzer sees no more than today.
- **Operator.** No runtime change, no new failure mode, no performance cost.
- **Tooling developer.** Key completion after `at: #` on a shape, hover
  showing a key's type and the alias name, go-to-definition on an alias.
  One new `TypeAnnotation` and `InferredType` variant to handle.

## Steelman Analysis

### Rejected: parse into `Value` classes, add no type

- **Smalltalk purist:** "A thing with named, typed slots is an object. Give
  it a class and the checker already knows everything."
- **Operator:** "A generated `fromDictionary:` validates at the boundary.
  This is the only option that checks anything at run time."
- **Language designer:** "No new type form; the nominal system does the work."

Why not: it does nothing for FFI map specs, and it makes every option literal
(`options: #{#timeout => 5000}`) into a constructor call or leaves it
unchecked. The ADR keeps this as the recommended treatment for external data.

### Rejected: inference only, no annotation syntax

- **Newcomer:** "Nothing to learn; literals just get smarter."
- **Language designer:** "Smallest change; no syntax to regret."

Why not: the shape is lost at every method boundary. The buccaneer casts sit
across a boundary (`config websocket`), so they would all stay.

### Rejected: open by default

- **BEAM veteran:** "Maps get extended. Options flow through layers that add
  their own keys."
- **Newcomer from TypeScript:** "This is how object types work."

Why not: the typo check is the main benefit for option maps. Pass-through
maps write `...`, which also documents that they are pass-through.

### Tension points

- Purists and operators prefer Value classes; BEAM developers and newcomers
  prefer shapes. Both stay available and the ADR says when to use which.
- Closed-by-default helps config and costs pass-through maps a `...`.
- JSON and YAML users will ask for string-key shapes. The answer for now is a
  `Value` class at the boundary.

## Alternatives Considered

### Do nothing: keep `Dictionary` and the annotated casts
BT-4719 is already making unchecked narrowing casts visible, and the runtime
validators already catch bad options at startup. This costs nothing in the
checker. Rejected because it leaves each option alias stated three times
(doc comment, key list, validator), gives no call-site feedback on misspelt
keys, and leaves FFI maps as bare `Dictionary` with no way to do better.

### `Boolean?` for optional keys
`#k => Boolean?` reads as "nullable Boolean" to anyone from Kotlin, Swift or
TypeScript, which is a different thing from a missing key.

### String keys (JSON and YAML objects)
`#{"id" => String, ...}` would cover the 114 string-key reads in exdura,
symphony, beamtalk-linear and beamtalk-agents, and map patterns already
accept string keys. Left out: that data is external and the types are erased,
so a shape over it is an unchecked claim that looks like a guarantee. Those
libraries mostly want tag-field narrowing (`msg at: "type"`) and validation,
neither of which a shape provides. The right tool today is a `Value` class
built at the boundary. Revisit together with string singleton types.

### A named `shape` declaration (nominal)
`shape WebSocketOptions ...` as its own declaration kind. Rejected: `type`
aliases (ADR 0108) already name structural types, and FFI shapes are anonymous.

### Shapes only where declared, no literal inference
The first draft kept `infer_map_literal` unchanged and checked a literal
against a shape only when it sat directly in a typed position. Rejected after
review: `opts := #{...}` followed by a call, and `options at: #handler put: h`,
are both common in the libraries being migrated and would warn.

### Per-key types for every update now
Typing `removeKey:` and `merge:` per key in this ADR. Deferred: each adds
checker surface, and the surveyed code does not need them. `at:put:` is in
because it does.

### Emit shapes into Erlang `-spec`s
`#{k := T, k2 => T2}` instead of `map()`. Deferred: it changes generated
attributes, and Dialyzer would then check closed map contracts against
callers the Beamtalk checker accepted through `Dynamic`. It needs its own
measurement of Dialyzer fallout.

## Consequences

### Positive
- Option-map aliases state their shape once, in a form the checker enforces.
- Misspelt and mistyped option keys are caught at the call site.
- A large class of unchecked narrowing casts (BT-4719) disappears.
- Erlang map specs with atom keys arrive typed instead of as bare `Dictionary`.
- No runtime cost and no change to generated code.
- Reads of a key a literal never had (`#{#a => 1} at: #b`) are reported.

### Negative
- A new `InferredType` variant: every exhaustive match on types gains an arm.
- String-keyed data (JSON, YAML) gets nothing from this ADR.
- Runtime validators remain for dynamic callers and value-range checks, so
  the shape and the validator can still drift.
- `removeKey:` and `merge:` answer a `Dictionary` and lose the shape.
- The config boundary still needs one cast: a validator that builds a section
  from a `Dictionary(Symbol, Object)` must assert its shape once. The casts
  move from every reader to that one place; they do not vanish.
- FFI tightening will produce new warnings in existing callers. The number
  is unknown until measured (phase 6 gate).
- Anonymous FFI shapes have no alias name, so hovers and diagnostics must
  truncate long shapes.
- Recursive shapes are impossible while aliases are non-recursive.

### Neutral
- Symbol-keyed map literals infer a shape instead of
  `Dictionary(K, V)`. Sends that are not shape-aware see the same type as
  before.
- Redefining a shape alias re-checks its use sites through ADR 0108's
  existing hot-reload trigger.
- Tightening an FFI return from `Dictionary` to a shape can surface new
  diagnostics in existing code that reads a key the spec does not name.
- Existing `Dictionary(Symbol, Object)` aliases keep working; migration is
  per alias.

## Implementation

| Phase | Work | Components | Size |
|---|---|---|---|
| 1 | `TypeAnnotation::Shape`: parser (including the `,?` split), AST, unparse, formatter | `source_analysis`, `ast`, `unparse` | M |
| 2 | Brace-aware type strings: one shared routine for splitting and parsing the string form; fix the `#`-prefix singleton checks so `#{` is not a singleton | `string_utils.rs`, `type_checker/validation.rs` | M |
| 3 | `InferredType::Shape`: resolution from annotations and aliases, display with truncation, shape-to-shape and shape-to-`Dictionary` assignability, the gradual rule, `Dictionary` view for all other sends | `type_checker` | L |
| 4 | Map literals infer shapes; key-naming diagnostics for shape mismatches | `type_checker/inference/literals.rs`, `validation.rs` | M |
| 5 | Keyed operations: `at:`, `at:ifAbsent:`, `at:ifPresent:` (and `ifPresent:ifAbsent:`), `at:put:`, map-pattern bindings, union receivers, `isKindOf:` narrowing | `type_checker/inference`, `narrowing` | M |
| 6 | FFI: spec reader maps `:=` / `=>` fields to shapes; shared fixture | `beamtalk_spec_reader.erl`, `native_types.rs` | M |
| 7 | LSP key completion and hover; language-features and syntax-rationale docs; surface parity | `beamtalk-language-service`, docs | M |
| 8 | Prove it: migrate buccaneer config aliases, beamtalk-http `RequestOptions` / `ClientOptions`, stdlib `system_navigation.bt` casts | sibling repos, stdlib | M |
| 9 | End-to-end tests (BUnit plus one btscript) | `stdlib/test`, `tests/repl-protocol` | S |

Phase 3 depends on 1 and 2. Phases 4 and 5 depend on 3. Phase 6 depends on 3
and the gates below.

**Wire check (gate before phase 6).** Phases 1 to 5 must carry one real alias
end to end: buccaneer's `WebSocketOptions` as a shape, the casts in
`pipeline.bt` removed, a misspelt key at a `Buccaneer start:` call site
reported, and no new warnings elsewhere in buccaneer beyond the one cast at
the config validator. If that is noisy or awkward, revisit the design before
building FFI mapping and tooling on it.

**FFI gate (within phase 6).** Before the reader change is switched on, count
the new warnings it produces across the stdlib and the sibling libraries. If
most are noise from loose value types or domain keys, narrow the mapping
(for example, results only) before shipping it.

None of this touches state-threading lowering, so `ci:corpus` is not needed.

### Deferred (each needs its own justification)

- Per-key result types for `removeKey:` and `merge:`.
- Fieldwise intersection, and `includesKey:` narrowing of an optional key to
  present (BT-4709), with a warning for `at:` on an un-narrowed optional key.
- Emitting shapes into Erlang `-spec`s.
- Expected-type propagation into list literals (`List(Shape)` elements):
  owned by BT-2949.
- String keys, and tag-field narrowing over a union of shapes
  (`msg at: "type"`), which beamtalk-agents and symphony's codex runner
  would use. Both need string singleton types and a story for validation.
- Deriving key lists, option docs or a validator from a shape.
- Recursive shapes.

## Migration Path

Two things change for existing code without any edit:

- Symbol-keyed map literals infer shapes. A read of a key the literal never
  had now warns. Everything else sees the same type as before.
- FFI maps with atom keys become shapes (phase 6). A read of a key outside
  the spec warns; the fix is to correct the key or annotate the value as
  `Dictionary`.

To migrate an option alias:

1. Replace the `Dictionary(Symbol, Object)` alias with a shape taken from its
   doc comment and its key list. Include every key the validator accepts,
   reserved ones too.
2. Where a validator builds the section from an untyped map, assert the shape
   once there with an annotated local.
3. Delete the annotated-local casts at the readers.
4. Keep the runtime validator for value-range checks and dynamic callers.

Field defaults such as `field: websocket :: WebSocketOptions = Dictionary new`
keep working under the gradual rule.

## References
- Related issues: BT-4725 (this ADR), BT-4719 (narrowing casts), BT-4709 (narrowing gaps), BT-2949 (generic container element checks)
- Related ADRs: ADR 0025 (gradual typing), ADR 0068 (parametric types), ADR 0075 (FFI type definitions), ADR 0100 (open-world diagnostic policy), ADR 0102 (set-theoretic operators), ADR 0108 (type aliases)
- Code: `stdlib/src/dictionary.bt`, `crates/beamtalk-core/src/semantic_analysis/type_checker/inference/literals.rs`, `runtime/apps/beamtalk_compiler/src/beamtalk_spec_reader.erl`
- Motivating code: buccaneer `src/pipeline.bt`, `src/buccaneer_config.bt`; beamtalk-http `src/httpclient.bt`
- Erlang map types: https://www.erlang.org/doc/system/typespec.html#types-and-their-syntax
- Hack shapes: https://docs.hhvm.com/hack/built-in-types/shape
- Python TypedDict: PEP 589, PEP 655, PEP 728
