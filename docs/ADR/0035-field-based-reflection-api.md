# ADR 0035: Rename Instance Variable Reflection API from instVar to field

## Status
Implemented (2026-02-24)

## Context

Beamtalk's reflection API currently uses Smalltalk-80 naming conventions for instance variable access:

```beamtalk
c instVarNames await              // => #(#value)
c instVarAt: #value await         // => 0
c instVarAt: #value put: 42 await // => 42
```

These names are inherited from Smalltalk-80, but Beamtalk's underlying implementation is fundamentally different:

| Aspect | Smalltalk-80 | Beamtalk |
|--------|-------------|----------|
| Storage | Fixed-size array | Erlang map (`#{}`) |
| Access | By integer index (1-based) | By atom key |
| Adding a variable | Recompile class + rebuild all instances | `maps:put` — zero disruption |
| `instVarAt:` argument | Integer | Symbol/atom |

The name `instVarAt:` is actively misleading — it implies positional/indexed access (Pharo's `instVarAt:` takes an integer and uses VM primitive 73), but Beamtalk performs keyed map lookup. In Pharo, adding an instance variable to a class triggers a full recompilation cascade: the class is rebuilt, all subclasses are rebuilt, and every existing instance is migrated to a new layout. None of this applies to Beamtalk's map-backed state.

Positional access also creates a subtle inheritance hazard in Smalltalk: `instVarAt: 1` can refer to *different* variables in a class vs. its subclass, because adding subclass variables shifts the array layout. Beamtalk's map-keyed access has no such ambiguity — `#x` is `#x` at every level of the hierarchy.

Additionally, the internal runtime already uses "field" terminology consistently:

- `beamtalk_reflection:field_names/1`
- `beamtalk_reflection:read_field/2`
- `beamtalk_reflection:write_field/3`
- `beamtalk_tagged_map:user_field_keys/1`
- The syntax rationale documents `self.value` as "field access"

This creates a naming split: the public API says "instVar" while the implementation says "field."

## Decision

Rename the reflection API from `instVar` to `field` at both instance and class levels.

### Instance-side reflection (Object)

| Current | New |
|---------|-----|
| `instVarNames` | `fieldNames` |
| `instVarAt:` | `fieldAt:` |
| `instVarAt:put:` | `fieldAt:put:` |

```beamtalk
c := Counter spawn

c fieldNames await              // => #(#value)
c fieldAt: #value await         // => 0
c fieldAt: #value put: 42 await // => 42
```

Note: sends to actors are async and return Futures. `await` resolves to the actual value. The return type annotations (e.g., `-> List`) describe the resolved value, not the Future.

### Class-side introspection (Behaviour)

| Current | New |
|---------|-----|
| `instanceVariableNames` | `fieldNames` |
| `allInstanceVariableNames` | `allFieldNames` |

```beamtalk
Counter fieldNames await        // => #(#value) — declared fields for this class
Counter allFieldNames await     // => #(#value) — includes inherited fields
```

The class-side `fieldNames` and instance-side `fieldNames` answer different questions — class-side returns the *declared* field names (the schema), instance-side returns the *actual* field names on a live object. Same selector, different receiver — idiomatic Smalltalk polymorphism.

**Tradeoff acknowledged:** Generic tooling iterating over a mixed collection of class objects and instances will get different semantics from the same selector. Tools must know whether they expect class-side or instance-side behavior. This is inherent to Smalltalk's polymorphic design and accepted as the standard tradeoff — alternative selectors like `declaredFieldNames` would break the polymorphic consistency.

Internal primitives are renamed accordingly:
- `classInstVarNames` → `classFieldNames`
- `classAllInstVarNames` → `classAllFieldNames`

### Class state declaration and reflection

The `classVar:` declaration keyword is renamed to `classState:` for two reasons:

1. **Consistency with `state:`** — instance fields use `state:`, class fields should use `classState:`
2. **Accuracy** — Beamtalk's "class variables" are stored per-class (each class gen_server has its own copy). Subclasses do *not* share the parent's values. In Smalltalk terminology, these are "class instance variables," not "class variables." The name `classVar:` implies Smalltalk's shared-across-hierarchy semantics, which is misleading.

| Current | New |
|---------|-----|
| `classVar: count = 0` | `classState: count = 0` |

```beamtalk
Actor subclass: Counter
  state: value = 0
  classState: instanceCount = 0
```

Class state reflection also adopts "field" terminology:

| Current | New |
|---------|-----|
| `classVarNames` (if/when added) | `classFieldNames` |

The declaration keywords (`state:`, `classState:`) describe *intent* ("this is state"). The reflection API (`fieldNames`, `classFieldNames`) describes *mechanism* ("give me the fields").

### Subclass field access — no positional ambiguity

In Smalltalk-80, `instVarAt:` takes an integer index into a fixed-size array. This creates a subtle problem with inheritance: the *position* of a variable can differ between a class and its subclass because subclass variables shift the array layout. `instVarAt: 1` on a parent might refer to `x`, but on a child it might refer to `childField` if the child's variables are prepended.

Beamtalk has no such ambiguity. Fields are keyed by atom in a flat map — `#x` is `#x` regardless of where in the hierarchy it was declared:

```beamtalk
Actor subclass: Base
  state: x = 0

Base subclass: Child
  state: y = 0

// Inside a Child method:
self fieldAt: #x         // => 0 — accesses parent's field, no ambiguity
self fieldAt: #x put: 42 // => 42 — self-send, allowed
self fieldNames           // => #(#x, #y) — flat map, all fields visible
```

This is a direct benefit of the map-backed design: field names are stable identifiers, not fragile positions. The "field" terminology reinforces this — fields are named, not numbered.

### Access control: fieldAt:put: encapsulation (deferred)

Beamtalk principle 6 states: *"Encapsulation enforced: only way to interact with an object is via messages."* While `fieldAt:put:` is technically a message, it allows any sender to mutate any field on any actor — bypassing the public method interface that the class author designed.

In Smalltalk-80 and Pharo, `instVarAt:put:` has the same problem. Pharo's documentation [explicitly warns](http://pharo.gforge.inria.fr/PBE1/PBE1ch15.html): *"using them to develop conventional applications is a bad idea: these reflective methods break the encapsulation boundary."*

**This ADR does NOT add access control to `fieldAt:put:`.** Restricting `fieldAt:put:` to self-sends requires resolving how the runtime distinguishes self-sends from external sends in a gen_server-backed actor — a non-trivial architectural question with implications for `perform:withArguments:`, Erlang interop, and compile-time vs. runtime enforcement. This is deferred to a separate ADR.

For now, `fieldAt:put:` remains unrestricted (matching current `instVarAt:put:` behavior). The rename stands on its own merits without the access control change.

### Error on value types (unchanged behavior)

```beamtalk
42 fieldAt: #x         // => raises #immutable_value
42 fieldAt: #x put: 99 // => raises #immutable_value
```

### REPL session

```beamtalk
> c := Counter spawn
=> a Counter
> c fieldNames await
=> #(#value)
> (c fieldNames await) class
=> List
> c fieldAt: #value await
=> 0
> c increment await
=> 1
> c fieldAt: #value await
=> 1
> c fieldAt: #value put: 42 await
=> 42
```

## Prior Art

### Smalltalk-80 / Pharo
- `instVarAt:` (integer index), `instVarAt:put:`, `instVarNamed:` (string name)
- Index-based access into fixed-size instance variable array
- Adding an ivar triggers class reshape and instance migration
- Pharo also has `instVarNamed:` for name-based access, acknowledging the index API is inconvenient

### Self
- "Slots" — unified concept for both data and methods
- Dynamic, prototype-based; no class recompilation needed
- Beamtalk borrows the dynamic spirit but not the unified slot model

### Newspeak
- "Slots" accessed only through generated getters/setters, never directly
- Mirror-based reflection for introspection
- More encapsulated than Beamtalk's direct field access

### Dylan / CLOS
- Dylan uses `slot` keyword for declaration (`slot count :: <integer> = 0`)
- CLOS uses `slot-value` for reflective access by name
- Both are dynamic, keyed by name — semantically close to Beamtalk
- "Slot" is the correct language-theoretic term but niche outside Lisp/Smalltalk communities

### Modern languages (Java, C#, Kotlin, Go, Rust, Dart, Python)
- All use "field" (or "attribute" in Python) for named data members
- Most widely understood term across the industry
- No positional/indexed connotation

## User Impact

### Newcomer (from Python/JS/Ruby)
"Field" is immediately understood — it's what Java, Kotlin, Dart, and Go call named data in objects. Zero learning curve for the concept. `fieldAt: #value` reads as "get the field named value."

### Smalltalk developer
A departure from tradition, but justified: Beamtalk's `instVarAt:` never took an integer and never had Smalltalk-80's rebuild-the-world semantics. The old name was familiar but inaccurate. The `fieldAt:put:` self-send restriction is stricter than Smalltalk, but aligns with the encapsulation principle that Smalltalk developers value — Pharo's own docs warn against using `instVarAt:put:` in application code.

### Erlang/BEAM developer
"Field" is neutral and clear. The underlying implementation (`maps:get/put`) is what they'd expect. More approachable than Smalltalk jargon.

### Production operator
No runtime behavior change — only method names in the dispatch tables change. Observable via the same BEAM tools.

## Steelman Analysis

### For `instVar` (status quo)
- **Smalltalk purist**: "This IS the standard Smalltalk reflection API. Every Smalltalk developer knows it. Changing it abandons our heritage for no functional benefit."
- **Newcomer**: "I can Google `instVarAt` and find decades of Smalltalk documentation explaining what it does."

### For `slot` (Self/Dylan/CLOS tradition)
- **Language designer**: "Slot is the correct term in dynamic OO language-theory. Self invented it, Dylan adopted it, CLOS uses it. It carries exactly the right connotations — a named, dynamic container."
- **Smalltalk purist**: "Self and Newspeak are respected successors to Smalltalk. Using their terminology is a principled evolution, not a departure."

### For `state` (BEAM-native)
- **BEAM veteran**: "This maps directly to gen_server state. I know exactly what's happening under the hood — it's the State argument in `handle_call/3`."

### Tension points
- Smalltalk purists and language designers would prefer `instVar` or `slot` respectively
- But neither `instVar` nor `slot` is widely understood outside their niche communities
- `field` wins on universality at the cost of language-theoretic precision
- The internal codebase already uses `field`, making it the path of least resistance

## Alternatives Considered

### Alternative: Keep `instVar`
The Smalltalk-80 names are familiar to Smalltalk developers but misleading for everyone else. `instVarAt:` implies indexed access that doesn't exist. The "instance variable" terminology carries connotations of fixed layout and rebuild-on-change that don't apply to Beamtalk's map-backed state. Rejected because the name actively misrepresents the semantics.

### Alternative: Use `slot`
Strong lineage in Self, Dylan, and CLOS. Semantically accurate — a named, dynamic container. However, "slot" is niche vocabulary outside the Lisp/Smalltalk family. Most developers coming from Python, JavaScript, Ruby, Java, Go, Rust, or Kotlin would not immediately understand "slot." Rejected in favor of the more universally understood "field," though this was a close call.

### Alternative: Use `state`
Honest about the gen_server implementation but leaky as an abstraction. "State" typically refers to the whole state map, not individual entries within it. `stateAt: #value` reads awkwardly — "the state at value" conflates the container with its contents. Rejected because it couples the public API to the implementation and doesn't read naturally.

## Consequences

### Positive
- Public API naming matches internal implementation naming (`field_names`, `read_field`, `write_field`)
- Public API naming matches syntax documentation ("field access" for `self.value`)
- No misleading positional/indexed access connotation
- Universally understood across programming language communities
- Accurate representation of the dynamic, map-backed semantics

### Negative
- Breaks existing code using `instVarNames`, `instVarAt:`, `instVarAt:put:`, `classVar:`, `instanceVariableNames`
- Departs from Smalltalk tradition (mitigated: Beamtalk is Smalltalk-*like*, not Smalltalk-*compatible*)
- Tests and documentation must be updated

### Neutral
- Internal runtime function names (`field_names`, `read_field`, etc.) remain as-is
- `fieldAt:put:` access control deferred to a separate ADR — encapsulation enforcement requires resolving self-send detection in gen_server actors

## Implementation

### Phase 1: Rename instance-side API
1. **Object.bt**: Rename intrinsic declarations from `instVarNames`/`instVarAt:`/`instVarAt:put:` to `fieldNames`/`fieldAt:`/`fieldAt:put:`
2. **Codegen** (`intrinsics.rs`): Update intrinsic name matching for the new selectors
3. **Runtime** (`beamtalk_object_ops.erl`): Update dispatch clauses for new selector atoms
4. **Runtime** (`beamtalk_primitive.erl`): Update `is_ivar_method` checks to new names
5. **Runtime** (`beamtalk_actor.erl`): Update any direct selector references

### Phase 2: Rename class-side API and declaration syntax
1. **Behaviour.bt**: Rename `instanceVariableNames` → `fieldNames`, `allInstanceVariableNames` → `allFieldNames`
2. **Parser**: Rename `classVar:` keyword to `classState:` in class definition parsing
3. **Runtime** (`beamtalk_object_class.erl`): Rename `instance_variables` field in `#class_state{}` to `fields`; rename `class_variables` to `class_state`; update handle_call clauses for new primitive names
4. **Runtime** (`beamtalk_class_instantiation.erl`): Update references to `instance_variables` and `class_variables` keys in class spec maps
5. **Codegen** (`methods.rs`): Update `instance_variables` key in generated class registration maps
6. **Stdlib**: Update all `classVar:` declarations to `classState:` in `SystemDictionary.bt`, `WorkspaceEnvironment.bt`, `TranscriptStream.bt`
7. **Tests**: Update test fixtures using `classVar:` (`class_var_point.bt`, `class_var_counter.bt`, `instance_access_counter.bt`)

### Phase 3: Update tests and docs
1. **Stdlib tests**: Update `reflection_basic_test.bt` and any other tests using the old API
2. **E2E tests**: Update any REPL test cases
3. **Erlang unit tests**: Update `beamtalk_object_ops_tests.erl`, `beamtalk_primitive_tests.erl`
4. **Language spec**: Update `beamtalk-language-features.md`
5. **ADR 0005, 0006, 0032**: Add notes referencing this ADR for the renamed API

### Affected components
- **Parser**: `classVar:` → `classState:` keyword recognition
- **Stdlib**: `Object.bt`, `Behaviour.bt`, `SystemDictionary.bt`, `WorkspaceEnvironment.bt`, `TranscriptStream.bt`
- **Codegen**: `intrinsics.rs`, `methods.rs`
- **Runtime**: `beamtalk_object_ops.erl`, `beamtalk_primitive.erl`, `beamtalk_actor.erl`, `beamtalk_object_class.erl`, `beamtalk_class_instantiation.erl`
- **Tests**: `reflection_basic_test.bt`, `beamtalk_object_ops_tests.erl`, `beamtalk_primitive_tests.erl`, `class_var_point.bt`, `class_var_counter.bt`, `instance_access_counter.bt`
- **Docs**: `beamtalk-language-features.md`, `beamtalk-syntax-rationale.md`

## Migration Path

This is a breaking change to the reflection API and declaration syntax. Since Beamtalk is pre-1.0, no deprecation period is required. The old names will stop working immediately.

All `classVar:` declarations must be changed to `classState:`. This is a simple find-and-replace across `.bt` files.

## Implementation Tracking

**Epic:** BT-804
**Issues:**
- BT-805 — Rename instance-side reflection API (runtime + codegen + stdlib) — Phase 1
- BT-806 — Parser: rename `classVar:` to `classState:` — Phase 1
- BT-807 — Rename class-side reflection API across runtime, codegen, stdlib — Phase 2 (blocked by BT-806)
- BT-808 — Update tests and docs — Phase 3 (blocked by BT-805, BT-807)

**Status:** Planned

## References
- Related issues: BT-796 (remove Flavors before/after infrastructure)
- Related ADRs: ADR 0005 (BEAM Object Model), ADR 0006 (Unified Method Dispatch), ADR 0032 (Early Class Protocol)
- Pharo reflection: [Pharo by Example — Reflection](http://pharo.gforge.inria.fr/PBE1/PBE1ch15.html)
- Newspeak spec: [newspeaklanguage.org/spec](https://newspeaklanguage.org/spec/newspeak-spec.pdf)
- Dylan slot reference: [Open Dylan documentation](https://opendylan.org/)
- Self slots: [Self Handbook](https://handbook.selflanguage.org/)
