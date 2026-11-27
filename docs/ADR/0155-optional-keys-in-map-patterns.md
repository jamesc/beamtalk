# ADR 0155: Optional Keys in Map Patterns

## Status
Proposed (2026-11-27)

## Context

### The problem

A map pattern `#{#k => v}` matches a dictionary that holds every listed key.
It ignores extra keys, but a missing key means the pattern does not match: a
`match:` arm falls through, and a destructuring assignment raises `#key_error`.

Real API data rarely has only required keys. The downstream adoption of the
pattern features (epic BT-5221) found the same gap in three packages:

- **beamtalk-linear** (`Issue fromNode:`, `IssueRef fromNode:`): Linear
  GraphQL nodes leave out keys the query did not select or the issue does not
  have. Both constructors are a column of `node at: "x" ifAbsent: [nil]`.
- **beamtalk-agents** (`ClaudeBackend start:config:onEvent:`,
  `handleControl:`, `usageCounters:`): config and stream messages use
  `at:ifAbsent:` with defaults. A map pattern would make every key required
  and change behaviour.
- **beamtalk-oauth** (callback parameters, token-info bodies, ID-token
  claims): a mix of required and optional keys. `iss`, `error`,
  `error_description`, `azp` and `aud` are optional by specification.

So the code keeps `at:ifAbsent:` where one pattern would say what the data
looks like. BT-5344 asks for map patterns with optional keys.

### What exists today

- **Map patterns** (BT-1296) take a symbol or string key and a value that is a
  variable, `_`, a literal, or a type pattern `x :: T` (BT-5284, BT-5294).
  `_ :: T` is documented as nesting too, but in a map value it does not parse
  today: `parse_map_pattern_value` takes `_` as a wildcard before it looks for
  `::`. The implementation fixes this. Parser: `parse_map_pattern` in
  `crates/beamtalk-core/src/source_analysis/parser/patterns.rs`. AST:
  `Pattern::Map { pairs: Vec<MapPatternPair> }` in
  `crates/beamtalk-core/src/ast/pattern.rs`.
- **In a `match:` arm** the pattern lowers to a native Core Erlang map pattern
  `~{K := V}~`. A nested `x :: T` is hoisted to a plain variable and
  class-tested after the shape matches, in source order, before the `when:`
  guard (`hoist_nested_types` / `generate_nested_type_arm` in
  `crates/beamtalk-codegen/src/core_erlang/patterns/type_tests.rs`).
- **In a destructuring assignment** each pair lowers to
  `call 'erlang':'map_get'(K, M)`, which raises on a missing key
  (`destructure.rs`). There `x :: T` is a trusted declared type, not a runtime
  test (BT-5284).
- **Shape types** (ADR 0147) already have optional keys. `?#k => T` in a shape
  type means "the key holds a `T` or is absent". The type checker types a
  map-pattern binding from the shape's value type (`bind_map_pattern_types`,
  BT-5336). The parser already splits a `,?` token into `,` and `?` inside a
  shape.
- **`at: k ifAbsent: [d]`** on a `Dictionary` checks the receiver's class
  tag, allocates a closure for the block and calls
  `bt@stdlib@dictionary:dispatch/3` (see Codegen below).

### Constraints

- An Erlang map pattern cannot say "maybe absent". The lowering needs a
  `maps:get/3` or an `is_map_key` test outside the native pattern.
- `|` already means list cons (`#(h | t)`) and union types
  (`x :: Integer | Nil`). Inside a map pattern the value can be `x :: T`, so a
  `|` after it would be read as part of the type.
- `?`, `,` and `=` are binary-selector characters. `,?` and `=-` each lex as
  one token.
- `=` is not an expression operator. The parser rejects a bare `=` with
  "use `=:=` or `==`". It appears only as the default marker in
  `state: x :: T = 0` and `field: x :: T = nil`.
- A pattern must stay free of side effects. Patterns take no part in
  `ThreadedIr` state threading. A default that could send a message would
  bring them in.

## Decision

### 1. Syntax: `?` before the key, `= literal` after the value

A map-pattern key may be marked optional with a leading `?`, the same marker
ADR 0147 uses for an optional key in a shape type. An optional key may be
followed by `= default`, the same marker as a `state:`/`field:` default.

```text
MapPatternPair := [ '?' ] Key '=>' Value [ '=' Default ]     // '= Default' only after '?'
Key            := SymbolLiteral | StringLiteral
Default        := Number | '-' Number | String | Symbol | Character
                | 'nil' | 'true' | 'false' | '#()' | '#{}'
```

In the REPL:

```text
> #{?#port => port = 80, #host => host} := #{#host => "example.com"}
=> #{#host => "example.com"}
> port
=> 80
> #{?#port => p2} := #{#host => "example.com"}
=> #{#host => "example.com"}
> p2
=> nil
```

```beamtalk
// absent -> nil
#{#id => id, ?#priority => priority} := node

// absent -> the default
#{?"command" => command = "claude", ?"readTimeoutMs" => timeout = 5000} := config

// in a match: arm, with a nested type test
node match: [
  #{"id" => id, ?"priority" => p :: Integer} -> {id, p};
  _ -> nil
]
```

Rules:

- **Optional means absent, not nil.** `?#k => v` binds `nil` when the key is
  missing. When the key is present it binds the value, even if the value is
  `nil`. A default replaces only a missing key, as `at:ifAbsent:` does. This
  is the same distinction ADR 0147 draws between `?#k => T` and
  `#k => T | Nil`.
- **The value of an optional key** is a variable, `x :: T` or `_ :: T`.
  A literal value (`?#type => "blocks"`) is a compile error: "absent or equal
  to `"blocks"`" is rarely meant, and a reader would misread it. A bare `_`
  (`?#k => _`) tests nothing, so it is a warning: "an optional key bound to
  `_` has no effect; remove the entry".
- **A default is a scalar constant**: a number (negative too), string, symbol,
  character, `nil`, `true`, `false`, the empty list `#()` or the empty map
  `#{}`. Together these cover every default in the evidence. Each one is a
  Core Erlang constant (`[]`, `~{}~`, an atom, a number, a tagged string
  tuple), so the pattern stays side-effect free and `maps:get/3` evaluates
  nothing. Arrays (`#[]` builds through `beamtalk_array:from_list`) and
  collections with contents are left out. A default that is not
  a constant is a compile error at the default:
  `the default of an optional key must be a literal; bind the key without a
  default and use ifNil: for a computed value`. A default on `_ :: T`, or on a
  key without `?`, is a compile error.
- **Order.** Required and optional pairs may be mixed in any order. A key that
  appears both required and optional (`#{#k => a, ?#k => b}`) is a compile
  error. Two required pairs on the same key keep today's behaviour.
- **Top level only.** An optional key is allowed only in a map pattern that is
  a whole `match:` arm pattern or a whole destructuring target. In a map nested
  inside a tuple, list or `Result ok:` pattern it is a compile error for now:
  `optional keys are supported only in a top-level map pattern`. A nested map
  has no variable for `maps:get/3` to read. Lifting this needs an alias
  variable for the nested map, which is a separate change.
- **The scrutinee must be a `Dictionary`.** A pattern that has an optional key
  first checks that the value is an untagged map, the same test
  `_ :: Dictionary` uses (`wrap_untagged_map_test`). Value objects, arrays and
  other tagged maps are Erlang maps at run time. In a `match:` arm a failed
  check falls through to the next arm. In a destructure it raises
  `#pattern_mismatch`. Without this check,
  `#{?#k => v}` would match every one of them and bind `nil`. (A pattern with
  only required keys can already match a tagged map that holds those keys.
  That is a separate, pre-existing gap. This ADR leaves it unchanged.)
- **Lexing.** `?#k`, `?"k"`, `, ?#k` and `,?#k` all parse: as in a shape type,
  the parser splits a `,?` token. `= -1` parses; `=-1` lexes `=-` as one
  token, and the parser splits it the same way. `fmt` always prints `?` with
  no space before the key, and ` = ` with a space on each side.

### 2. Types

An optional binding has the type a required binding would have at that key,
joined with `Nil` when there is no default, or with the default's type `D`
when there is one. ADR 0147 gives `at: #k ifAbsent: [b]` the same rule:
`T | B`. Today a map-pattern binding is typed only when the scrutinee is a
shape (symbol keys, BT-5336). Otherwise it stays untyped (`Dynamic`), and
optional keys do not change that. String keys, which every downstream example
uses, therefore type through the `x :: T` columns only.

| Scrutinee | `?#k => v` | `?#k => v = d` | `?#k => v :: T` | `?#k => v :: T = d` |
|---|---|---|---|---|
| shape, `#k => S` (required) | `S` | `S` | `S & T` | `S & T` |
| shape, `?#k => S` (optional) | `S \| Nil` | `S \| D` | `(S & T) \| Nil` | `(S & T) \| D` |
| shape, `#k` not named, closed | `Nil` + warning | `D` + warning | `Nil` + warning | `D` + warning |
| shape, `#k` not named, open | `Object \| Nil` | `Object \| D` | `T \| Nil` | `T` |
| not a shape (`Dictionary(K, V)`, `Dynamic`, string keys) | `Dynamic` | `Dynamic` | `T \| Nil` | `T` |

The `:: T` columns describe a `match:` arm, where `x :: T` is a test. In a
destructure it is the declared type of the local (see §3).

`D` is the default's type. `S & T` is the intersection BT-5336 already
computes for a nested type pattern. An unknown, `Object` or disjoint position
gives the bare `T`. Two further rules:

- **A default must fit a type test.** In `?#k => n :: Integer = 0` the default
  is checked against `Integer` at compile time. If it does not fit
  (`= "none"`), the compiler warns `default "none" is not an Integer`. A
  default can fit `T` without fitting the shape's `S` (`S = Integer`,
  `T = Number`, `= 0.5`), hence `(S & T) | D`.
- **An absent key in a closed shape** is always absent, so the pattern binds
  the default or `nil`. The compiler warns `no key #k in <shape>; it is always
  absent`, as ADR 0147 warns for `at:ifAbsent:` on such a key. A `?` on a
  required key is silent.

**Narrowing with a nested `x :: T`** (BT-5294, BT-5328, BT-5332) follows from
"optional means absent":

| Data | `?#k => n :: Integer` in a `match:` arm | in a `:=` destructure |
|---|---|---|
| key absent | matches, `n = nil` | binds `n = nil` |
| key absent, `= 0` | matches, `n = 0` | binds `n = 0` |
| `#k => 5` | matches, `n = 5` | binds `n = 5` |
| `#k => "x"` | **no match**, next arm | binds `n = "x"` (annotation is trusted, BT-5284) |
| `#k => nil` | **no match** (`nil` is not an `Integer`) | binds `n = nil` |

In a `match:` arm the type test runs only when the key is present. A present
value of the wrong class falls through to the next arm. The test runs in source
order with the other nested type tests, after the shape match and before the
`when:` guard. The guard sees every binding. Because of the untagged-map check,
a pattern whose keys are all optional matches any `Dictionary` and nothing
else. Exhaustiveness is unchanged: a map pattern never makes a `match:`
exhaustive over a non-map type.

**JSON `null`.** A present `nil` fails `?#k => x :: T`, because `nil` is not a
`T` and a type pattern names one class (`x :: String | Nil` is not a type
pattern). An API that sends explicit nulls is read with `?#k => x`, without the
test, or the arm matches `nil` first. Union type patterns would cover this
case. They are a separate feature, and this ADR keeps "optional means absent"
rather than special-casing `nil`.

### 3. Destructuring

Optional keys work the same in `#{...} := m` as in a `match:` arm, except that
`x :: T` stays a trusted annotation (BT-5284).

- **An optional key never raises.** Absent binds the default, or `nil`.
- **A required key still raises `#key_error` when it is missing.** No change.
  A required key in a destructure is an assertion about the data. Turning a
  missing key into `nil` without a `?` would hide bugs. The `?` makes the
  choice visible at the key.
- A non-map right-hand side raises as it does today.
- `x :: T` without a default declares the local as `T | Nil`, because absent
  binds `nil`. With a default it declares `T`, and the default is checked
  against `T` like any assignment.

The parser sends a destructure through the pattern grammar when it sees a `?`,
`,?` or `=` inside the leading `#{`. The test is the same lookahead
`leading_collection_is_typed_destructure` already uses for `::`. With `=` in
the lookahead, `#{#id => id = 0} := m` reaches the pattern parser and gets the
specific "only an optional key can have a default" error, not the generic
"'=' is not an operator".

### 4. Codegen

Required pairs stay in the native map pattern. In a destructure, each optional
pair becomes one `maps:get/3` with the constant default (`'nil'` when there is
none). In a `match:` arm, an optional pair with a nested type test becomes one
`maps:find/2`, so the test runs only on a present value and the key is looked
up once. No new runtime function is needed.

An arm that has an optional pair always leaves the native single-`case` fast
path. Its bindings come from `let`s after the clause matches, so a `when:`
guard in clause position could not see them. Such an arm goes through
`generate_nested_type_arm`, the path nested type tests already take, with the
guard run last. The predicate that selects that path (`pattern_has_nested_type`)
also checks for an optional pair.

**Destructure.** `#{?"request" => request = #{}, ?"request_id" => requestId} := msg`

```erlang
let _Map@1 = _msg@0 in
let Request@ = call 'maps':'get'(<"request">, _Map@1, ~{}~) in
let RequestId@ = call 'maps':'get'(<"request_id">, _Map@1, 'nil') in
...
```

Compare a required key today:
`let Id@ = call 'erlang':'map_get'(<"id">, _Map@17) in ...`.
(`<"k">` stands for the tagged string key
`{'beamtalk_string', #{...}#}`, which `map_pattern_key_doc` already emits.
A symbol key is a plain atom.)

**`match:` arm with a nested type test.**
`#{"id" => id, ?"priority" => p :: Integer} -> {id, p}; _ -> nil`

```erlang
case _Match@7 of
  <~{ <"id"> := Id@ }~> when 'true' ->
    case <UNTAGGED-MAP TEST on _Match@7> of           % wrap_untagged_map_test
      <'true'> when 'true' ->
        let <_OptOk@20, P@> =
          case call 'maps':'find'(<"priority">, _Match@7) of
            <{'ok', _V@21}> when 'true' ->
              <call 'erlang':'is_integer'(_V@21), _V@21>     % the existing type test, as a boolean
            <'error'> when 'true' -> <'true', 'nil'>         % or the default
          end in
        case _OptOk@20 of
          <'true'>  when 'true' -> {Id@, P@}
          <'false'> when 'true' -> apply '_Fallthrough@14'/0 ()
        end
      <'false'> when 'true' -> apply '_Fallthrough@14'/0 ()
    end
  <_NoMatch@8> when 'true' -> apply '_Fallthrough@14'/0 ()
end
```

The type test in the `ok` branch is the test that
`dispatch_type_pattern_strategy` already builds for a hoisted type pattern. It
is built with `'true'` as the success continuation and `'false'` as the
failure, so every class strategy works: BIF guards, the `String` tag, the class
tag, and `isKindOf:` for classes with subclasses. The arm body appears once.
Without a type test the pair is
`let P@ = call 'maps':'get'(<"priority">, _Match@7, 'nil')`.

**Cost compared with `at:ifAbsent:`.** Today `node at: "priority" ifAbsent:
[nil]` compiles to an `is_map` test, a `maps:get('$beamtalk_class', …)` tag
lookup, an `is_map_key('__class_mod__', …)` test, a closure allocation for the
block, and a remote call to `bt@stdlib@dictionary:dispatch/3`. That call
dispatches `at:ifAbsent:` and applies the closure when the key is missing. The
pattern lowering is one `maps:get/3` BIF per key, plus one `is_map_key` per key
with a type test, which uses one `maps:find/2` instead. The untagged-map check
runs once per pattern. The pattern allocates nothing and makes no remote call.
`at:ifAbsent:` repeats its class-tag check for every key.

The state-threading machinery is not involved. Defaults are constants and
patterns contain no sends, so no `ThreadedIr` node changes. The implementation
PR still carries `ci:corpus`, because it touches `match:` arm lowering.

### 5. Formatter and LSP

- **AST.** `MapPatternPair` gains `optional: Option<Span>` (the `?`) and
  `default: Option<(MapPatternDefault, Span)>`. `MapPatternDefault` is a
  `Literal`, `Nil`, `True`, `False`, `EmptyList` or `EmptyMap`, so constness
  holds by construction and codegen renders it as a leaf. It contains no
  names, so the pattern walkers need only carry the new fields.
- **`fmt` / unparse** prints `?` directly before the key and ` = <default>`
  after the value. The BT-5299 wrapping rule is unchanged: a map pattern that
  does not fit goes one pair per line, with a trailing comma, and each optional
  pair stays whole on its line. Formatting is idempotent, and a `,?` in the
  source comes out as `, ?`.
- **LSP.** Hover on a binding shows the type from the table above
  (`String | Nil`). Go-to-definition and references treat the variable as a
  binding site, as today. The default is not a binding. Semantic tokens mark
  `?` and `=` as operators. Diagnostics carry the spans of the `?` and the
  default.

### Examples from the downstream packages

**beamtalk-linear, `IssueRef fromNode:`** (`src/issue_ref.bt`). Today:

```beamtalk
class fromNode: node :: Dictionary -> IssueRef =>
  self
    new: #{
      #id => (node at: "id" ifAbsent: [nil]),
      #identifier => (node at: "identifier" ifAbsent: [nil]),
      #state => ((node at: "state" ifAbsent: [
        #{}
      ]) at: "name" ifAbsent: [nil])
    }
```

With optional keys:

```beamtalk
class fromNode: node :: Dictionary -> IssueRef =>
  #{?"id" => id, ?"identifier" => identifier, ?"state" => stateNode = #{}} := node
  #{?"name" => state} := stateNode
  self new: #{#id => id, #identifier => identifier, #state => state}
```

A `state` that is present but `null` still raises at the second destructure,
just as `nil at: "name" ifAbsent:` raises today.

**beamtalk-linear, `Issue fromNode:`** (`src/issue.bt`): twelve
`at:ifAbsent:` lookups become one pattern that lists the node's keys:

```beamtalk
class fromNode: node :: Dictionary -> Issue =>
  #{
    ?"id" => id,
    ?"identifier" => identifier,
    ?"title" => title,
    ?"description" => description,
    ?"priority" => rawPriority,
    ?"state" => stateNode = #{},
    ?"branchName" => branchName,
    ?"url" => url,
    ?"labels" => labelsObj,
    ?"inverseRelations" => relationsObj,
    ?"createdAt" => createdAt,
    ?"updatedAt" => updatedAt
  } := node
  #{?"name" => state} := stateNode
  priority := rawPriority match: [n :: Integer -> n; _ -> nil]
  self
    new: #{
      #id => id,
      #identifier => identifier,
      #title => title,
      #description => description,
      #priority => priority,
      #state => state,
      #branchName => branchName,
      #url => url,
      #labels => (self labelsFromNode: labelsObj),
      #blockedBy => (self blockersFromNode: relationsObj),
      #createdAt => createdAt,
      #updatedAt => updatedAt
    }
```

`priority` keeps its `match:`. In a destructure `?"priority" => p :: Integer`
is an unchecked annotation, and the original code drops a non-integer
priority. A `match:` arm *would* test it, but the method has one shape, so a
second statement is clearer than a one-arm match.

**beamtalk-agents, `ClaudeBackend start:config:onEvent:`**
(`src/claude_backend.bt`). The defaults are the reason the adoption could not
use a pattern. Now they sit next to their keys:

```beamtalk
class start: workspacePath :: String config: config :: Dictionary onEvent: callback :: Block(Dictionary, Object) | Nil -> ClaudeBackend =>
  #{
    ?"command" => command = "claude",
    ?"permissionMode" => permissionMode = "acceptEdits",
    ?"allowedTools" => allowedTools = #(),
    ?"disallowedTools" => disallowedTools = #(),
    ?"readTimeoutMs" => readTimeoutMs = 5000,
    ?"resumeSessionId" => resumeSessionId
  } := config
  self
    spawnWith: #{
      #workspacePath => workspacePath,
      #command => command,
      #permissionMode => permissionMode,
      #allowedTools => allowedTools,
      #disallowedTools => disallowedTools,
      #readTimeoutMs => readTimeoutMs,
      #onEvent => callback,
      #resumeSessionId => resumeSessionId
    }
```

**beamtalk-agents, `handleControl:` and `usageCounters:`:**

```beamtalk
handleControl: msg :: Dictionary -> Result(Dictionary, AgentError) | Nil =>
  #{?"request" => request = #{}, ?"request_id" => requestId} := msg
  #{?"subtype" => subtype} := request
  // ... unchanged

class usageCounters: usage :: Dictionary -> Dictionary =>
  #{?"input_tokens" => input = 0, ?"output_tokens" => output = 0, ?"total_tokens" => total} := usage
  #{"input" => input, "output" => output, "total" => (total ifNil: [input + output])}
```

`total_tokens` has a computed default, `input + output`. Defaults are
constants, so it binds `nil` and `ifNil:` computes the sum. That differs from
`at:ifAbsent:` only when the key is present and holds `nil`. Then the sum is
used instead of `nil`, which is the better answer for a counter.

**beamtalk-oauth, the authorization callback** (`src/oauth_flows.bt`). Six
lookups are spread over forty lines of one method. They become one statement
at the top that says which parameters a callback may carry:

```beamtalk
#{
  ?"state" => state = "",
  ?"code" => code = "",
  ?"error" => providerError,
  ?"error_description" => errorDescription = "",
  ?"iss" => iss
} := parsed
```

`(parsed includesKey: "error") not` becomes `providerError isNil`. Query
parameters are never `nil`, so the two are the same.

**beamtalk-oauth, token-info bodies** (`src/token_revocation.bt`). A `match:`
arm whose keys are all optional matches any `Dictionary`, so it replaces the
`isKindOf: Dictionary` check as well as the lookup:

```beamtalk
class isInactive: status :: Integer body: body :: Object -> Boolean =>
  status =:= 401 ifTrue: [^true]
  status =:= 400 ifFalse: [^false]
  body match: [
    #{?"error" => code} -> code isNil or: [code =:= "invalid_token"];
    _ -> false
  ]

class isForClient: clientId :: String body: body :: Dictionary -> Boolean =>
  #{?"azp" => azp, ?"aud" => aud, ?"application" => app} := body
  azp notNil ifTrue: [^azp =:= clientId]
  // ... the aud / application checks, unchanged
```

**beamtalk-oauth, ID-token claims.** `aud := claims at: "aud" ifAbsent: [nil]`
(twice in `oauth_flows.bt`) becomes `#{?"aud" => aud} := claims`. The
`TokenResponse` shape in `tokenSetFrom:obtainedAt:` already has every key,
because the shim fills missing parts with `nil`. That site stays as it is.

### Error examples

```beamtalk
#{?#type => "blocks"} := r
// error: an optional key must bind a variable or a type pattern, not a literal
//   hint: match the key as required (#type => "blocks"), or bind it and compare

#{?"timeout" => t = defaults timeout} := config
// error: the default of an optional key must be a literal

{#ok, #{?#k => v}} := pair
// error: optional keys are supported only in a top-level map pattern
//   hint: bind the key without a default and use `t ifNil: [defaults timeout]`

#{#id => id = 0} := node
// error: only an optional key (`?#id`) can have a default

#{?#n => n :: Integer = "none"} := m
// warning: default "none" is not an Integer

opts :: #{#a => Integer} := #{#a => 1}
#{?#b => b} := opts
// warning: no key #b in #{#a => Integer}; it is always absent
```

## Prior Art

- **Erlang.** Map patterns (`#{k := V}`) require the key. Absence is handled
  outside the pattern with `maps:get/3`, `maps:find/2`, or the guard BIFs
  `is_map_key/2` and `map_get/2`. The lowering above uses exactly these. We
  add the pattern-level spelling Erlang lacks.
- **Elixir.** `%{"a" => a}` requires the key too. Code falls back to
  `Map.get(m, "b", default)`. Elixir's set-theoretic types write an optional
  key as `if_set(T)`, the model ADR 0147 adopted as `?#k => T`. We reuse that
  marker in the pattern, so a shape and the pattern that reads it look alike.
- **Gleam.** No map patterns. `dict.get` returns a `Result`, so absence is
  explicit at every lookup.
- **Clojure.** Map destructuring binds an absent key to `nil` and takes
  defaults in `:or`: `{:keys [a b] :or {b 0}}`. This is the closest semantic
  match: absent → nil, default only when absent. We put the default next to
  its key instead of in a separate clause.
- **JavaScript / TypeScript.** `const {a, b = 0} = obj`: destructuring with
  `= default`, applied when the property is `undefined`. This is where `=` as
  the default marker is most familiar.
- **Python** (`match` mapping patterns `{"a": a}`) requires the key, like
  Erlang. Optional keys use `.get()` outside the pattern.
- **Smalltalk** has no patterns. `at:ifAbsent:` is the idiom, and it stays
  available.

## User Impact

- **Newcomer.** `= default` reads like JavaScript and Python defaults. `?`
  for "maybe there" is familiar from TypeScript's `b?: T` and nullable types.
  The error for a computed default says what to write instead.
- **Smalltalk developer.** `at:ifAbsent:` still works and still allows a
  computed block. The pattern is a declarative alternative for the common case
  of reading a record. Message sends stay out of patterns.
- **Erlang/Elixir developer.** The lowering is `maps:get/3` and
  `is_map_key/2`, the code they would write by hand, and visible in the
  generated `.core`.
- **Production operator.** No new runtime path, no closure per lookup, and
  stack traces point at `maps:get` / `map_get` like other map code.
- **Tooling developer.** Two optional fields on `MapPatternPair`. The default
  is an ordinary constant `Expression`, so walkers need no new cases.

## Steelman Analysis

### `"b"? => b` (postfix marker on the key)
- 🧑‍💻 **Newcomer**: "Reads like TypeScript's `b?: T`."
- 🎨 **Language designer**: "The `?` sits next to the key it qualifies."
- **Why not:** shape types already put `?` first (`?#k => T`). Two orders for
  one idea is worse than either order. `"b"?=> b` also lexes `?=>` as one
  binary selector.

### `"b" => b | 0` (default after `|`)
- 🧑‍💻 **Newcomer**: "Like `||` in JS or Ruby."
- **Why not:** `b :: Integer | 0` is a union type with the literal type `0`,
  and `#(h | t)` is cons. It is ambiguous wherever a type pattern appears.

### `"b" => (b default: 0)` or `"b" => b ifAbsent: 0`
- 🎩 **Smalltalk purist**: "It is a keyword message, the Smalltalk way."
- **Why not:** a pattern is not an expression. Something that looks like a
  send but is not evaluated as one misleads. It also nests a keyword parse
  inside the pattern grammar.

### Default only, no `?` (`"b" => b = 0`, absent → default, else required)
- 🎨 **Language designer**: "One marker instead of two."
- **Why not:** it gives no way to say "optional, absent is nil" without
  `= nil` on every key, and that case is the most common one in the evidence
  (Linear). `?` alone covers it. `?` plus `= d` covers the rest.

### Computed defaults (`?"total" => t = input + output`)
- ⚙️ **BEAM veteran**: "`case maps:find` with an expression in the `error`
  branch costs nothing extra."
- 🧑‍💻 **Newcomer**: "Why can't I write the default I need?"
- **Why not now:** a default that can send makes patterns effectful. A `match:`
  arm's default would then need state threading inside an actor method, and a
  name in a default looks like a binder in pattern position. The one computed
  default in the evidence (`input + output`) reads fine as `ifNil:`. Deferred.
  Constants can be widened to pure expressions later without breaking code.

### Absent required key binds `nil` in destructuring (no `?` needed there)
- 🧑‍💻 **Newcomer**: "Destructuring should just work like `at:ifAbsent: [nil]`."
- 🏭 **Operator**: "Fewer crashes from optional API fields."
- **Why not:** it silently changes every existing destructure, and a required
  key is how the code says "this must be here". A `#key_error` at the
  destructure is better than a `nil` three calls later.

### Tension

Newcomers would trade `?` for less typing, as JS does with plain defaults.
BEAM veterans and operators want absence to be explicit, so it is visible in
the code and the generated Core. The decision keeps it explicit with a
one-character marker.

## Alternatives Considered

### Status quo: keep `at:ifAbsent:`
It needs no change, and `at:ifAbsent:` already allows computed defaults. It was
rejected because the evidence is three packages in which adopters wanted a
pattern and could not use one. A column of lookups hides which keys a record
has and costs a closure and a dispatch per key.

### A library helper instead of syntax
For example `node valuesAt: #("id", "title") ifAbsent: nil`, answering a list.
It needs no grammar change. It was rejected because positional results lose
the key-to-name pairing, defaults would be per call rather than per key, and
nothing reaches the type checker or a `match:` arm.

### One marker for the whole pattern (`#?{ ... }`, every key optional)
Most evidence patterns are entirely optional (Linear 12 of 12 keys, the oauth
callback 5 of 5, the agents config 6 of 6). It was rejected because claims,
token-info bodies and shapes mix required and optional keys, so a per-key
marker is needed anyway. A whole-pattern form could be added later as sugar.

See the Steelman Analysis above for the syntax alternatives (postfix `?`,
`| default`, keyword-style default, default-only) and the semantic ones
(computed defaults, implicit `nil` in destructuring). Two lowerings were also
considered:

- **`maps:get/3` followed by an `is_map_key` gate.** Two lookups per
  type-tested key. The chosen `maps:find/2`, returning a `<Ok, Value>` value
  list, does one lookup and also keeps one body.
- **A guard** `when not is_map_key(K, M) orelse is_integer(map_get(K, M))`.
  It is guard-safe only for BIF-tested classes, not for tagged or
  subclass-aware classes. It could be a later optimization for the BIF cases.

## Consequences

### Positive
- API-shaped data reads as one pattern that names its keys, which is what the
  BT-5221 adopters wanted.
- Pattern syntax matches shape-type syntax: `#{?#k => v}` reads a value of type
  `#{?#k => T}`.
- The lowering is cheaper than `at:ifAbsent:`: no closure, no dispatch.
- An absent key and a present `nil` stay distinct, as in shape types.

### Negative
- Defaults are constants only. A computed default needs `ifNil:`, which also
  fires on a present `nil`.
- One more marker in the pattern grammar, and one more destructure lookahead
  trigger.
- In a destructure, `?#k => x :: T` does not test the class (BT-5284 holds), so
  "wrong type" is caught only in a `match:` arm. This is already true of
  required keys.

- Optional keys are top-level only, and a present `nil` fails `?#k => x :: T`.
  Both are deliberate limits, with follow-ups noted above: nested optional maps,
  and union type patterns.
- `= default` takes the obvious spelling for a future alias pattern
  (`p = #{...}`). An alias would need another spelling, such as `as`.

### Neutral
- Nested map or tuple patterns inside a map value are still not supported
  (separate gap). The Linear `state { name }` example uses two destructures.
- A future `...rest` in map patterns would leave optional keys out of the
  rest map.
- `at:ifAbsent:` is unchanged and remains the tool for computed defaults and
  dynamic keys.

## Implementation

One PR (size M), labelled `ci:corpus`:

1. **AST + parser** (`ast/pattern.rs`, `parser/patterns.rs`,
   `parser/mod.rs`): `optional` / `default` on `MapPatternPair`; the `?` and
   `,?` / `=-` splitting; default validation; the destructure lookahead
   trigger on `?` and `=`; the top-level-only and mixed-duplicate errors; and
   the `_ :: T` map-value parse fix.
2. **Semantic analysis / type checker** (`type_checker/shape_keyed.rs`,
   `inference/flow.rs`, `pattern_bindings.rs`): the binding types in §2, the
   default-vs-type-test warning, and the always-absent warning.
3. **Codegen** (`patterns/match_lowering.rs`, `type_tests.rs`,
   `destructure.rs`): optional pairs left out of the native pattern; arms with
   optional pairs routed through `generate_nested_type_arm`; the untagged-map
   check; `maps:get/3`, and `maps:find/2` with a boolean type test.
4. **fmt / unparse** (`unparse/expressions.rs`) and **LSP** hover/semantic
   tokens.
5. **Tests**: key present; key absent; key absent with a default; present but
   the wrong type under a nested `x :: T` (no match in `match:`); destructuring
   (an optional key never raises, a required key still raises `#key_error`);
   parser errors; fmt round-trip. BUnit in `stdlib/test/`, plus parser,
   codegen-snapshot and type-checker unit tests.
6. **Docs**: `docs/beamtalk-language-features.md` § Pattern Matching (the
   Dict/Map row, a subsection, and the "Patterns in Real Code" note that map
   patterns require every key).

Downstream (after the PR merges and edge is republished): rewrite at least one
site per package in beamtalk-linear, beamtalk-agents and beamtalk-oauth, one PR
per repo.

## Migration Path

None. The change is additive. Every existing map pattern means what it meant
before, and `?` and `=` were not valid in a map pattern until now.

## References
- Related issues: BT-5344 (this), BT-5221 (adoption epic), BT-5225, BT-5226
  (evidence), BT-1296 (map patterns), BT-5284 (annotated destructuring),
  BT-5294 / BT-5328 / BT-5332 / BT-5336 (nested type patterns), BT-5299 (fmt
  wrapping of map patterns)
- Related ADRs: ADR 0147 (map-shape types: `?#k => T`), ADR 0107 (nil and type
  patterns), ADR 0089 (typed document leaves for codegen)
- Documentation: `docs/beamtalk-language-features.md` § Pattern Matching
