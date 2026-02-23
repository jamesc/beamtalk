# ADR 0035: Rename Instance Variable Reflection API from instVar to field

## Status
Proposed (2026-02-23)

## Context

Beamtalk's reflection API currently uses Smalltalk-80 naming conventions for instance variable access:

```beamtalk
c instVarNames              // => #(#value)
c instVarAt: #value         // => 0
c instVarAt: #value put: 42 // => 42
```

These names are inherited from Smalltalk-80, but Beamtalk's underlying implementation is fundamentally different:

| Aspect | Smalltalk-80 | Beamtalk |
|--------|-------------|----------|
| Storage | Fixed-size array | Erlang map (`#{}`) |
| Access | By integer index (1-based) | By atom key |
| Adding a variable | Recompile class + rebuild all instances | `maps:put` — zero disruption |
| `instVarAt:` argument | Integer | Symbol/atom |

The name `instVarAt:` is actively misleading — it implies positional/indexed access (Pharo's `instVarAt:` takes an integer and uses VM primitive 73), but Beamtalk performs keyed map lookup. In Pharo, adding an instance variable to a class triggers a full recompilation cascade: the class is rebuilt, all subclasses are rebuilt, and every existing instance is migrated to a new layout. None of this applies to Beamtalk's map-backed state.

Additionally, the internal runtime already uses "field" terminology consistently:

- `beamtalk_reflection:field_names/1`
- `beamtalk_reflection:read_field/2`
- `beamtalk_reflection:write_field/3`
- `beamtalk_tagged_map:user_field_keys/1`
- The syntax rationale documents `self.value` as "field access"

This creates a naming split: the public API says "instVar" while the implementation says "field."

## Decision

Rename the reflection API from `instVar` to `field`:

| Current | New |
|---------|-----|
| `instVarNames` | `fieldNames` |
| `instVarAt:` | `fieldAt:` |
| `instVarAt:put:` | `fieldAt:put:` |

### Instance-side reflection (Object)

```beamtalk
c := Counter spawn

c fieldNames              // => #(#value)
c fieldAt: #value         // => 0
c fieldAt: #value put: 42 // => 42
```

### Class-side introspection

```beamtalk
Counter instanceVariableNames  // => #(#value) — keep for now (class protocol)
```

The class-side `instanceVariableNames` is part of the class protocol (ADR 0032) and reads naturally as a declarative query ("what variables do instances have?"). It does not carry the same indexed-access baggage. It may be revisited separately if the class protocol evolves.

### Error on value types (unchanged behavior)

```beamtalk
42 fieldAt: #x         // => raises #immutable_value
42 fieldAt: #x put: 99 // => raises #immutable_value
```

### REPL session

```
> c := Counter spawn
=> a Counter
> c fieldNames await
=> #(#value)
> c fieldAt: #value await
=> 0
> c fieldAt: #value put: 42 await
=> 42
> c fieldAt: #value await
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
A departure from tradition, but justified: Beamtalk's `instVarAt:` never took an integer and never had Smalltalk-80's rebuild-the-world semantics. The old name was familiar but inaccurate. The Smalltalk class protocol name `instanceVariableNames` is preserved.

### Erlang/BEAM developer
"Field" is neutral and clear. The underlying implementation (`maps:get/put`) is what they'd expect. More approachable than Smalltalk jargon.

### Production operator
No runtime behavior change — only method names in the dispatch tables change. Observable via the same BEAM tools.

## Steelman Analysis

### For `instVar` (status quo)
- **Smalltalk purist**: "This IS the standard Smalltalk reflection API. Every Smalltalk developer knows it. Changing it abandons our heritage for no functional benefit."
- **Newcomer**: "I can Google `instVarAt` and find decades of Smalltalk documentation explaining what it does."

### For `slot` (Self/Dylan/CLOS tradition)
- **Language designer**: "Slot is the correct term in dynamic OO language theory. Self invented it, Dylan adopted it, CLOS uses it. It carries exactly the right connotations — a named, dynamic container."
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
- Breaks existing code using `instVarNames`, `instVarAt:`, `instVarAt:put:`
- Departs from Smalltalk tradition (mitigated: Beamtalk is Smalltalk-*like*, not Smalltalk-*compatible*)
- Tests and documentation must be updated

### Neutral
- Runtime behavior is unchanged — only selector names change
- The class protocol method `instanceVariableNames` is unchanged (separate concern)
- Internal runtime function names (`field_names`, `read_field`, etc.) remain as-is

## Implementation

### Phase 1: Rename public API
1. **Object.bt**: Rename intrinsic declarations from `instVarNames`/`instVarAt:`/`instVarAt:put:` to `fieldNames`/`fieldAt:`/`fieldAt:put:`
2. **Codegen** (`intrinsics.rs`): Update intrinsic name matching for the new selectors
3. **Runtime** (`beamtalk_object_ops.erl`): Update dispatch clauses for new selector atoms
4. **Runtime** (`beamtalk_primitive.erl`): Update `is_ivar_method` checks to new names
5. **Runtime** (`beamtalk_actor.erl`): Update any direct selector references

### Phase 2: Update tests and docs
1. **Stdlib tests**: Update `reflection_basic_test.bt` and any other tests using the old API
2. **E2E tests**: Update any REPL test cases
3. **Erlang unit tests**: Update `beamtalk_object_ops_tests.erl`
4. **Language spec**: Update `beamtalk-language-features.md`
5. **ADR 0005, 0006, 0032**: Add notes referencing this ADR for the renamed API

### Affected components
- **Stdlib**: `Object.bt` (intrinsic declarations)
- **Codegen**: `intrinsics.rs` (intrinsic matching)
- **Runtime**: `beamtalk_object_ops.erl`, `beamtalk_primitive.erl`, `beamtalk_actor.erl`
- **Tests**: `reflection_basic_test.bt`, `beamtalk_object_ops_tests.erl`, `beamtalk_primitive_tests.erl`
- **Docs**: `beamtalk-language-features.md`

## Migration Path

This is a breaking change to the reflection API. Since Beamtalk is pre-1.0, no deprecation period is required. The old names will stop working immediately.

## References
- Related issues: BT-796 (remove Flavors before/after infrastructure)
- Related ADRs: ADR 0005 (BEAM Object Model), ADR 0006 (Unified Method Dispatch), ADR 0032 (Early Class Protocol)
- Pharo reflection: [Pharo by Example — Reflection](http://pharo.gforge.inria.fr/PBE1/PBE1ch15.html)
- Newspeak spec: [newspeaklanguage.org/spec](https://newspeaklanguage.org/spec/newspeak-spec.pdf)
- Dylan slot reference: [Open Dylan documentation](https://opendylan.org/)
- Self slots: [Self Handbook](https://handbook.selflanguage.org/)
