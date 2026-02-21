# ADR 0033: Runtime-Embedded Documentation on Class and CompiledMethod

## Status
Proposed (2026-02-21)

## Context

### Problem Statement

Documentation in Beamtalk is currently accessible only through EEP-48 chunks baked into `.beam` files at compile time (ADR 0008). The `:h` REPL command fetches docs via `code:get_doc/1`, parses the `docs_v1` tuple, and walks the class hierarchy to find inherited method docs. This works, but it means:

1. **Classes and methods are not self-describing.** `Counter doc` doesn't work — you must go through the `:h` REPL command or call `code:get_doc(counter)` and parse the result yourself. CompiledMethod has `source` and `selector` but no `doc`.

2. **Dynamic classes have no docs.** Classes created at runtime via `subclass:withMethods:` have no `.beam` file, so `code:get_doc/1` returns `{error, missing}`. There is no way to attach documentation to a dynamically created class or its methods.

3. **The REPL docs module is over-complex.** `beamtalk_repl_docs.erl` (545 lines) manually walks the class hierarchy, queries gen_server state, fetches EEP-48 chunks, matches selectors against doc entries by metadata, and formats output. Most of this complexity exists because docs live outside the object model rather than on it.

4. **Docs are not first-class.** In Smalltalk, `MyClass comment` and `(MyClass >> #foo) comment` are ordinary message sends. In Beamtalk, docs require a special REPL command and a detour through the Erlang `code` module. This contradicts Principle 6 (Messages All The Way Down) and Principle 8 (Reflection as Primitive).

### Current State

- `///` doc comments are parsed and stored in the AST (`ClassDefinition.doc_comment`, `MethodDefinition.doc_comment`)
- The compiler generates EEP-48 `docs_v1` chunks via `doc_chunks.rs` and injects them into `.beam` files
- `:h ClassName` and `:h ClassName selector` work via `beamtalk_repl_docs.erl`
- The `class_state` record has no doc field
- `method_info()` maps contain `arity`, `block`, and `is_sealed` — no doc
- `CompiledMethod` maps contain `__selector__`, `__source__`, `__method_info__` — no doc
- Dynamic classes store methods but have no documentation path

### Constraints

- EEP-48 chunks must continue to be generated for BEAM ecosystem interop (`code:get_doc/1`, Elixir's `h/1`, Erlang shell `h/2`)
- Must work for both compiled classes (with `.beam` files) and dynamic classes (without)
- Must integrate with ADR 0032's `Behaviour`/`Class` protocol — doc access should be methods on `Behaviour`
- The `///` source syntax (ADR 0008) is unchanged — this ADR changes where docs are stored at runtime, not how they're authored

## Decision

Add documentation strings as first-class state on `Behaviour` and `CompiledMethod`, with post-hoc setter messages for programmatic doc assignment. The `///` syntax compiles to the same data path as the setters — one unified mechanism for compiled and dynamic classes.

### Runtime Storage

**`class_state` record** gains a `doc` field and per-method docs in `method_info()`:

```erlang
-record(class_state, {
    %% ... existing fields ...
    doc = none :: binary() | none,                     % Class doc string
    %% method_info() gains:  doc => binary() | none
}).
```

**`CompiledMethod` map** gains `__doc__`:

```erlang
-type compiled_method() :: #{
    '$beamtalk_class' := 'CompiledMethod',
    '__selector__' := atom(),
    '__source__' := binary(),
    '__doc__' := binary() | nil,                        % NEW
    '__method_info__' := map()
}.
```

### Beamtalk API

**Reading docs — ordinary message sends:**

```beamtalk
Counter doc                      // => 'A counter actor that maintains state.'
(Counter >> #increment) doc      // => 'Increment the counter by 1.'
(Counter >> #class) doc          // => 'Return the class of the receiver.'
```

`Counter doc` returns the class doc string or `nil`. The `>>` operator already walks the hierarchy, so `(Counter >> #increment) doc` works for both local and inherited methods.

**Setting docs — post-hoc setters on Behaviour:**

```beamtalk
Counter doc: 'A counter actor that maintains state.'.
Counter docForMethod: #increment put: 'Increment the counter by 1.'.
```

These are ordinary message sends to the class object, handled by methods on `Behaviour`. They update the class gen_server state.

### Behaviour.bt Additions

```beamtalk
// In lib/Behaviour.bt — documentation protocol

/// Return the documentation string for this class, or nil if none.
///
/// ## Examples
/// ```beamtalk
/// Integer doc       // => 'Integer — Whole number arithmetic and operations...'
/// ```
sealed doc => @intrinsic classDoc

/// Set the documentation string for this class.
///
/// ## Examples
/// ```beamtalk
/// Counter doc: 'A counter actor'.
/// Counter doc   // => 'A counter actor'
/// ```
sealed doc: aString => @intrinsic classSetDoc

/// Set the documentation string for a locally defined method.
/// The method must exist in this class (not inherited).
///
/// ## Examples
/// ```beamtalk
/// Counter docForMethod: #increment put: 'Increment by 1'.
/// (Counter >> #increment) doc   // => 'Increment by 1'
/// ```
sealed docForMethod: selector put: aString => @intrinsic classSetMethodDoc
```

### CompiledMethod Addition

In `beamtalk_compiled_method_ops.erl`, add `doc` to the builtin dispatch:

```beamtalk
(Counter >> #increment) doc            // => 'Increment the counter by 1.'
(Counter >> #increment) selector       // => #increment
(Counter >> #increment) source         // => 'increment => self.count := ...'
```

### Compiler Integration

The `///` syntax compiles to doc fields in the `ClassInfo` map passed to `beamtalk_object_class:start/2` during module init. No separate setter calls needed at init time — the existing registration path carries docs alongside methods.

In `register_class/0` codegen (`methods.rs`), the `ClassInfo` map gains:

```erlang
#{
    %% ... existing fields ...
    doc => <<"A counter actor that maintains state.">>,
    method_docs => #{
        increment => <<"Increment the counter by 1.">>,
        'getValue' => <<"Return the current counter value.">>
    }
}
```

EEP-48 chunks continue to be generated in parallel (by `doc_chunks.rs`) for BEAM ecosystem interop. Both paths consume the same `///` source — no duplication in authoring.

### REPL Session

```beamtalk
>> Counter doc
=> "A counter actor that maintains state."

>> (Counter >> #increment) doc
=> "Increment the counter by 1."

>> Counter doc: 'Updated documentation.'
=> "Updated documentation."

>> Counter doc
=> "Updated documentation."

>> Counter docForMethod: #increment put: 'Add one to the count.'
=> "Add one to the count."

>> (Counter >> #increment) doc
=> "Add one to the count."

>> // Dynamic class — no .beam file, docs still work
>> Actor subclass: Greeter withMethods: #{
     #greet -> [:self | 'Hello!']
   }
=> Greeter

>> Greeter doc: 'A simple greeter actor.'
=> "A simple greeter actor."

>> Greeter docForMethod: #greet put: 'Return a greeting.'
=> "Return a greeting."

>> Greeter doc
=> "A simple greeter actor."
```

### Error Examples

```beamtalk
>> Counter docForMethod: #nonExistent put: 'docs for missing method'
=> Error: method #nonExistent is not defined locally on Counter

>> 42 doc
=> Error: 42 does not understand #doc
```

## Prior Art

| Language | Class Doc Access | Method Doc Access | Mutable? | Dynamic Classes |
|----------|-----------------|-------------------|----------|-----------------|
| **Pharo** | `MyClass comment` | `(MyClass >> #foo) comment` | Yes (`comment:`) | Yes (via changes file) |
| **Python** | `MyClass.__doc__` | `MyClass.foo.__doc__` | Yes (`__doc__ = ...`) | Yes (in memory) |
| **Elixir** | `Code.fetch_docs(M)` | `Code.fetch_docs(M)` | No (compile-time only) | No |
| **Ruby** | None native | `Method#source_location` only | No | No |
| **Newspeak** | Mirror reflection | Mirror reflection | Via mirrors | Yes (in image) |

**What we adopt:**

- **Pharo's `comment` / `comment:` pattern** — Docs as readable and writable attributes on class objects via ordinary message sends. We use `doc` / `doc:` (shorter, more modern) but the pattern is identical. The key insight from Pharo is that class documentation is mutable state that belongs on the class object, not in an external file.
- **Python's `__doc__` simplicity** — Docs stored directly on the object, always available, no external lookup needed. Python's `help()` function reads `__doc__` from objects. Our `doc` message is the same principle applied through message passing.

**What we adapt:**

- **Pharo extracts method comments from source code** (the first string literal in a method body). We use structured `///` syntax instead (ADR 0008) and store the extracted doc as a separate field rather than re-parsing source at runtime.

**What we preserve:**

- **Elixir/Erlang EEP-48 interop** — We continue generating EEP-48 chunks for `.beam` files. `code:get_doc(counter)` still works. Erlang `h(counter)` and Elixir `h Counter` still work. The runtime `doc` message is an additional access path, not a replacement for EEP-48.

## User Impact

### Newcomer (from Python/JS/Ruby)
- **Immediately intuitive**: `Counter doc` is as natural as Python's `Counter.__doc__`. The REPL becomes self-documenting — explore any class by asking it about itself.
- **Discoverable**: Tab completion on a class shows `doc` alongside `methods`, `superclass`, etc. No need to learn the `:h` command first.

### Smalltalk Developer
- **Faithful to the tradition**: Pharo's `MyClass comment` maps directly to `Counter doc`. Docs as mutable object state — not external metadata — is core Smalltalk philosophy.
- **CompiledMethod gets richer**: `(Counter >> #increment) doc` alongside `source` and `selector` makes CompiledMethod a proper reflective object.

### Erlang/BEAM Developer
- **EEP-48 preserved**: `code:get_doc/1` continues to work. No behavior change for existing BEAM tooling.
- **Gen_server state**: Docs are just another field in `class_state` — visible in Observer, debuggable with `sys:get_state/1`.

### Production Operator
- **Small memory cost**: Doc strings are binaries — typically 50-200 bytes per method. For a system with 500 methods, that's ~50-100KB total. Negligible.
- **No performance impact on dispatch**: Docs are stored alongside method_info but not consulted during dispatch. The `doc` field is read only when explicitly asked for.

## Steelman Analysis

### Alternative A: Lazy EEP-48 Accessor Only

| Cohort | Strongest argument |
|---|---|
| **BEAM veteran** | "EEP-48 is the standard. Adding a second doc storage path means two sources of truth that can diverge. `doc:` lets someone set runtime docs that don't match the `///` in source. Keep it simple — one source, one path, `code:get_doc/1`." |
| **Operator** | "Zero memory overhead for docs in production. Docs live on disk in `.beam` files, loaded only when someone asks. Why pay the RAM cost for something that's only used during development?" |

### Alternative C: Compiler-Only (Enrich ClassInfo, No Setters)

| Cohort | Strongest argument |
|---|---|
| **Language designer** | "Mutable docs are a foot-gun. Someone sets `Integer doc: 'lol'` and now the system lies. Docs should be authoritative — they come from source code, period. Make `doc` read-only and populate it from `///` during compilation." |
| **Pragmatist** | "Post-hoc setters are YAGNI for v0.1. Dynamic classes barely exist yet. Just thread docs through ClassInfo and add setters if/when dynamic classes need them." |

### Tension Points

- BEAM veterans prefer the single-source-of-truth of EEP-48-only. Smalltalk purists want mutable docs as part of the live object model. The post-hoc setter approach sides with Smalltalk but preserves EEP-48 for interop.
- The "two sources of truth" concern is real but manageable: runtime docs are authoritative for the running system (like Pharo's changes file), EEP-48 is authoritative for the compiled artifact. Hot reload updates both.

## Alternatives Considered

### Alternative A: Lazy EEP-48 Accessor

Add a `doc` message on `Behaviour` and `CompiledMethod` that calls `code:get_doc/1` on demand. No new state, no setters.

```beamtalk
Counter doc   // => calls code:get_doc(counter), parses docs_v1 tuple
```

**Rejected because:** Doesn't work for dynamic classes (no `.beam` file). Also makes `doc` inconsistent — it works for compiled classes but returns nil for dynamic ones, with no way to fix it. The whole point is making all classes self-describing regardless of origin.

### Alternative C: Compiler-Only (No Setters)

Thread doc strings through the `ClassInfo` map during `register_class/0` (like method source today), but provide only a read-only `doc` message. No `doc:` or `docForMethod:put:`.

```beamtalk
Counter doc                     // => works (populated at compile time)
Counter doc: 'new docs'         // => Error: does not understand #doc:
```

**Rejected because:** Breaks the Smalltalk principle that classes are live, mutable objects. Pharo's `comment:` is essential for interactive development — you fix a doc typo in the browser, not by recompiling. More practically, dynamic classes would have no way to get docs at all. The setters are simple (two intrinsics) and the flexibility is worth it.

## Consequences

### Positive
- **Self-describing objects**: Classes and CompiledMethods carry their documentation — no external lookup required
- **Works for dynamic classes**: Post-hoc setters provide the only doc path for classes without `.beam` files
- **Simpler `:h` implementation**: `beamtalk_repl_docs.erl` can use `doc` messages instead of parsing EEP-48 tuples directly
- **Principle alignment**: Satisfies Principle 6 (Messages All The Way Down) and 8 (Reflection as Primitive) — docs are just messages
- **Unified mechanism**: `///` and post-hoc setters both populate the same state — one runtime representation
- **BEAM interop preserved**: EEP-48 chunks continue to be generated alongside runtime docs

### Negative
- **Two doc storage paths**: EEP-48 chunks and runtime state can theoretically diverge (mitigated: both populated from `///` for compiled classes; hot reload updates both)
- **Memory cost**: Doc strings consume RAM proportional to total documentation volume (~50-100KB for a typical system)
- **Three new intrinsics**: `classDoc`, `classSetDoc`, `classSetMethodDoc` added to the intrinsic set

### Neutral
- **`:h` REPL command unchanged**: Continues to work as before; implementation can be simplified to use `doc` messages internally, but the user-facing command is the same
- **`///` syntax unchanged**: No authoring changes — this ADR affects storage and access, not how docs are written
- **EEP-48 generation unchanged**: `doc_chunks.rs` continues to generate chunks for `.beam` files

## Implementation

### Phase 1: Runtime Storage (S)

**Affected components:** Runtime (`beamtalk_runtime`)

1. Add `doc = none :: binary() | none` to `#class_state{}` in `beamtalk_object_class.erl`
2. Add `doc => binary() | none` to `method_info()` maps
3. Add `__doc__` field to `CompiledMethod` map construction (in `handle_call({method, Selector}, ...)`)
4. Add `doc` dispatch to `beamtalk_compiled_method_ops.erl`
5. Handle `doc` and `method_docs` keys in `ClassInfo` map during `init/1` and `handle_call({update_class, ...}, ...)`

### Phase 2: Intrinsics and Beamtalk API (S)

**Affected components:** Runtime (`beamtalk_runtime`), stdlib (`lib/`)

1. Implement three intrinsics in `beamtalk_behaviour_intrinsics.erl`:
   - `classDoc/1` — read `doc` from `class_state`
   - `classSetDoc/2` — write `doc` to `class_state`
   - `classSetMethodDoc/3` — write `doc` into `method_info()` for a given selector
2. Add `doc`, `doc:`, `docForMethod:put:` methods to `lib/Behaviour.bt`
3. Register intrinsics in the compiler's intrinsic table

### Phase 3: Compiler Integration (M)

**Affected components:** Codegen (`beamtalk-core`)

1. In `methods.rs` (`generate_register_class`), emit `doc` and `method_docs` keys in the `ClassInfo` map, populated from `ClassDefinition.doc_comment` and `MethodDefinition.doc_comment`
2. Ensure `update_class` (hot reload path) also carries updated docs
3. Add tests verifying doc roundtrip: `///` → compile → `Counter doc` returns the text

### Phase 4: Simplify REPL Docs (S)

**Affected components:** Workspace (`beamtalk_workspace`)

1. Update `beamtalk_repl_docs.erl` to use `doc` messages for class and method docs instead of calling `code:get_doc/1` directly
2. Keep EEP-48 fallback for classes that pre-date this change (backward compat during transition)
3. Simplify `format_method_doc/2` to use `(Class >> Selector) doc` instead of walking EEP-48 entries

## References
- Related ADRs: [ADR 0008](0008-doc-comments-and-api-documentation.md) (Doc Comments and API Documentation — establishes `///` syntax and EEP-48 generation), [ADR 0032](0032-early-class-protocol.md) (Early Class Protocol — `Behaviour`/`Class` as stdlib classes, intrinsic pattern)
- Related issues: BT-496 (Doc Comments epic — implemented), BT-731 (Early Class Protocol epic)
- Pharo class comments: [ClassDescription>>comment](https://github.com/pharo-project/pharo/blob/Pharo12/src/Kernel-CodeModel/ClassDescription.class.st)
- Python docstrings: [PEP 257](https://peps.python.org/pep-0257/)
- EEP-48: https://www.erlang.org/eeps/eep-0048.html
