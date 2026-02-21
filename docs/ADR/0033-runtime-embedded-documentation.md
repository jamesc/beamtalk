# ADR 0033: Runtime-Embedded Documentation on Class and CompiledMethod

## Status
Proposed (2026-02-21)

## Context

### Problem Statement

Documentation in Beamtalk is currently accessible only through EEP-48 chunks baked into `.beam` files at compile time (ADR 0008). The `:h` REPL command fetches docs via `code:get_doc/1`, parses the `docs_v1` tuple, and walks the class hierarchy to find inherited method docs. This works, but it means:

1. **Classes and methods are not self-describing.** `Counter doc` doesn't work — you must go through the `:h` REPL command or call `code:get_doc(counter)` and parse the result yourself. CompiledMethod has `source` and `selector` but no `doc`.

2. **Dynamic classes have no docs.** Classes created at runtime have no `.beam` file, so `code:get_doc/1` returns `{error, missing}`. There is no way to attach documentation to a dynamically created class or its methods.

3. **The REPL docs module is over-complex.** `beamtalk_repl_docs.erl` (~540 lines) manually walks the class hierarchy, queries gen_server state, fetches EEP-48 chunks, matches selectors against doc entries by metadata, and formats output. Most of this complexity exists because docs live outside the object model rather than on it.

4. **Docs are not first-class.** In Smalltalk, `MyClass comment` and `(MyClass >> #foo) comment` are ordinary message sends. In Beamtalk, docs require a special REPL command and a detour through the Erlang `code` module. This contradicts Principle 6 (Messages All The Way Down) and Principle 8 (Reflection as Primitive).

5. **EEP-48 adds pipeline complexity for little v0.1 value.** The current doc pipeline generates EEP-48 chunks in `doc_chunks.rs`, writes a `.docs` file alongside Core Erlang output, and injects the chunk into `.beam` files post-`erlc`. This is the most fragile part of the compilation pipeline. The primary consumer is `:h` in the Beamtalk REPL — not Erlang or Elixir tooling. For v0.1, no one is calling `h(counter)` from the Erlang shell.

### Current State

- `///` doc comments are parsed and stored in the AST (`ClassDefinition.doc_comment`, `MethodDefinition.doc_comment`)
- The compiler generates EEP-48 `docs_v1` chunks via `doc_chunks.rs` and injects them into `.beam` files
- `:h ClassName` and `:h ClassName selector` work via `beamtalk_repl_docs.erl`
- The `class_state` record has no doc field
- `method_info()` maps contain `arity`, `block`, and `is_sealed` — no doc
- `CompiledMethod` maps contain `__selector__`, `__source__`, `__method_info__` — no doc
- Dynamic classes store methods but have no documentation path
- The `>>` operator returns a CompiledMethod for the local method only — it does not walk the class hierarchy (this ADR changes that)

### Constraints

- Must work for both compiled classes (with `.beam` files) and dynamic classes (without)
- Must integrate with ADR 0032's `Behaviour`/`Class` protocol — doc access should be methods on `Behaviour`
- The `///` source syntax (ADR 0008) is unchanged — this ADR changes where docs are stored at runtime, not how they're authored
- Post-hoc doc setters require `Behaviour` to be fully registered — they are unavailable during the bootstrap window before `Behaviour` is loaded. This is acceptable because no user code runs during bootstrap.

## Decision

Add documentation strings as first-class state on `Behaviour` and `CompiledMethod`, with post-hoc setter messages for programmatic doc assignment. The `///` syntax compiles to the same data path as the setters — one unified mechanism for compiled and dynamic classes.

**Fix `>>` to walk the class hierarchy.** Currently `Counter >> #class` returns `nil` because `#class` is inherited from ProtoObject. This is inconsistent with Pharo (where `>>` walks the hierarchy) and makes method doc access unnecessarily difficult. After this change, `Counter >> #class` returns ProtoObject's CompiledMethod for `#class`, and `(Counter >> #class) doc` just works.

**Remove EEP-48 doc chunk generation.** Runtime-embedded docs replace EEP-48 as the single source of truth for documentation. This eliminates the `doc_chunks.rs` codegen, the `.docs` file intermediate, and the post-`erlc` beam chunk injection step. EEP-48 generation can be re-added later if BEAM ecosystem interop becomes a priority — the data is all available in `class_state`.

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

When constructing a `CompiledMethod` via `handle_call({method, Selector}, ...)`, the `__doc__` field is populated from the `doc` key in the local `method_info()` map. If the method has no doc, `__doc__` is `nil`.

**Note on `none` vs `nil`:** The `class_state` record uses Erlang's `none` atom (idiomatic for internal Erlang records), while the Beamtalk-facing `CompiledMethod` map uses `nil` (Beamtalk's null value). The translation happens at the boundary when constructing CompiledMethod from class state.

### Beamtalk API

**Reading docs — ordinary message sends:**

```beamtalk
Counter doc                      // => 'A counter actor that maintains state.'
(Counter >> #increment) doc      // => 'Increment the counter by 1.'
(Counter >> #class) doc          // => 'Return the class of the receiver.' (inherited)
```

`Counter doc` returns the class doc string or `nil`. The `>>` operator walks the class hierarchy (matching Pharo's behaviour), so `(Counter >> #class) doc` returns ProtoObject's doc for `#class` even though Counter doesn't define it locally.

**Setting docs — post-hoc setters on Behaviour:**

```beamtalk
Counter doc: "A counter actor that maintains state.".
Counter setDocForMethod: #increment to: "Increment the counter by 1.".
```

These are ordinary message sends to the class object, handled by methods on `Behaviour`. They update the class gen_server state. Doc setters only work for locally defined methods — you cannot set a doc on an inherited method without first overriding the method itself.

### `>>` Hierarchy Walking

The `>>` operator is changed to walk the class hierarchy, matching Pharo's semantics. Previously it only checked local methods.

```beamtalk
Counter >> #increment   // => CompiledMethod (local — works today)
Counter >> #class       // => CompiledMethod (inherited from ProtoObject — previously nil)
```

Implementation: `beamtalk_method_resolver:resolve/2` is updated to walk the superclass chain via the class gen_server, checking local methods at each level. When an inherited method is found, the returned CompiledMethod includes the `__doc__` from the defining class's `method_info()`.

### Doc Write/Read Asymmetry

Doc reading walks the hierarchy via `>>`; doc writing is local-only via `setDocForMethod:to:`. This mirrors method dispatch — you can *call* inherited methods but only *define* methods locally.

- `(Counter >> #increment) doc` — returns doc from Counter (local method)
- `(Counter >> #class) doc` — returns ProtoObject's doc for `#class` (inherited)
- `Counter setDocForMethod: #class to: 'text'` — **error**: `#class` is not defined locally on Counter

### Behaviour.bt Additions

```beamtalk
// In lib/Behaviour.bt — documentation protocol

/// Return the documentation string for this class, or nil if none.
///
/// ## Examples
/// ```beamtalk
/// Integer doc       // => "Integer — Whole number arithmetic and operations..."
/// ```
sealed doc => @intrinsic classDoc

/// Set the documentation string for this class.
///
/// ## Examples
/// ```beamtalk
/// Counter doc: "A counter actor".
/// Counter doc   // => "A counter actor"
/// ```
sealed doc: aString => @intrinsic classSetDoc

/// Set the documentation string for a locally defined method.
/// The method must exist in this class (not inherited).
///
/// ## Examples
/// ```beamtalk
/// Counter setDocForMethod: #increment to: "Increment by 1".
/// (Counter >> #increment) doc   // => "Increment by 1"
/// ```
sealed setDocForMethod: selector to: aString => @intrinsic classSetMethodDoc
```

### CompiledMethod Addition

In `beamtalk_compiled_method_ops.erl`, add `doc` to the built-in dispatch:

```beamtalk
(Counter >> #increment) doc            // => 'Increment the counter by 1.'
(Counter >> #increment) selector       // => #increment
(Counter >> #increment) source         // => 'increment => self.count := ...'
(Counter >> #class) doc                // => 'Return the class of the receiver.'
```

### Compiler Integration

The `///` syntax compiles to doc fields in the `ClassInfo` map passed to `beamtalk_object_class:start/2` during module init. No separate setter calls needed at init time — the existing registration path carries docs alongside methods.

In `generate_register_class` codegen (`methods.rs`), the `ClassInfo` map gains:

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

### REPL Session

```beamtalk
>> Counter doc
=> "A counter actor that maintains state."

>> (Counter >> #increment) doc
=> "Increment the counter by 1."

>> // Inherited method docs — >> walks the hierarchy
>> (Counter >> #class) doc
=> "Return the class of the receiver."

>> Counter doc: "Updated documentation."
=> "Updated documentation."

>> Counter doc
=> "Updated documentation."

>> Counter setDocForMethod: #increment to: "Add one to the count."
=> "Add one to the count."

>> (Counter >> #increment) doc
=> "Add one to the count."

>> // Dynamic class — docs work without .beam files
>> Greeter doc: "A simple greeter actor."
=> "A simple greeter actor."

>> Greeter setDocForMethod: #greet to: "Return a greeting."
=> "Return a greeting."

>> (Greeter >> #greet) doc
=> "Return a greeting."
```

### Error Examples

```beamtalk
>> Counter setDocForMethod: #nonExistent to: "docs for missing method"
=> Error: method #nonExistent is not defined locally on Counter

>> Counter setDocForMethod: #class to: "override inherited doc"
=> Error: method #class is not defined locally on Counter

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

- **Pharo's `comment` / `comment:` pattern** — Docs as readable and writable attributes on class objects via ordinary message sends. We use `doc` / `doc:` (shorter, more modern) but the pattern is identical. The key insight from Pharo is that class documentation is mutable state that belongs on the class object, not in an external file. Unlike Pharo, runtime doc mutations are session-local — they do not persist to disk or survive a restart. This is a deliberate compromise: Beamtalk is file-based (Principle 5: Code Lives in Files), not image-based.
- **Python's `__doc__` simplicity** — Docs stored directly on the object, always available, no external lookup needed. Python's `help()` function reads `__doc__` from objects. Our `doc` message is the same principle applied through message passing.

**What we adapt:**

- **Pharo extracts method comments from source code** (the first string literal in a method body). We use structured `///` syntax instead (ADR 0008) and store the extracted doc as a separate field rather than re-parsing source at runtime.

**What we reject:**

- **EEP-48 as the doc access path** — Elixir and Erlang store docs in `.beam` file chunks, read via `code:get_doc/1`. This doesn't work for dynamic classes and requires complex post-compilation beam rewriting. We store docs on the runtime objects instead. EEP-48 generation can be re-added for BEAM interop if needed — the data is available in `class_state`.

## User Impact

### Newcomer (from Python/JS/Ruby)
- **Immediately intuitive**: `Counter doc` is as natural as Python's `Counter.__doc__`. The REPL becomes self-documenting — explore any class by asking it about itself.
- **Discoverable**: Tab completion on a class shows `doc` alongside `methods`, `superclass`, etc. No need to learn the `:h` command first.
- **Caveat**: Newcomers might expect `doc:` mutations to persist across restarts. Error message or REPL warning should clarify that `doc:` is session-local; edit the `///` comment in source for permanent changes.

### Smalltalk Developer
- **Faithful to the tradition**: Pharo's `MyClass comment` maps directly to `Counter doc`. Docs as mutable object state — not external metadata — is core Smalltalk philosophy.
- **CompiledMethod gets richer**: `(Counter >> #increment) doc` alongside `source` and `selector` makes CompiledMethod a proper reflective object.
- **Caveat**: Unlike Pharo, mutations don't persist (no changes file). Smalltalkers may find this surprising.

### Erlang/BEAM Developer
- **Gen_server state**: Docs are just another field in `class_state` — visible in Observer, debuggable with `sys:get_state/1`.
- **Caveat**: EEP-48 chunks are no longer generated. `code:get_doc(counter)` will not return Beamtalk docs. If BEAM ecosystem interop for docs becomes important, EEP-48 generation can be re-added as a future phase.

### Production Operator
- **Small memory cost**: Doc strings are binaries — typically 50-200 bytes per method. For a system with 500 methods, that's ~50-100KB total. Negligible.
- **No performance impact on dispatch**: Docs are stored alongside method_info but not consulted during dispatch. The `doc` field is only accessed when explicitly requested via a `doc` message.
- **Simpler build pipeline**: Removing EEP-48 chunk injection eliminates the post-`erlc` beam rewriting step.

## Steelman Analysis

### Alternative A: Keep EEP-48 as Primary (Lazy Accessor)

| Cohort | Strongest argument |
|---|---|
| **BEAM veteran** | "EEP-48 is the BEAM standard. Every BEAM language uses it. Removing it means Beamtalk docs are invisible to `h/1` in Erlang and Elixir shells, to ExDoc, to any tool that reads beam chunks. You're building a walled garden for no reason — EEP-48 already works." |
| **Operator** | "Zero memory overhead for docs in production. Docs live on disk in `.beam` files, loaded only when someone asks. Why pay the RAM cost for something that's only used during development?" |

### Alternative C: Compiler-Only (Enrich ClassInfo, No Setters)

| Cohort | Strongest argument |
|---|---|
| **Language designer** | "Mutable docs are a foot-gun. Someone sets `Integer doc: 'lol'` and now the system lies. Docs should be authoritative — they come from source code, period. Make `doc` read-only and populate it from `///` during compilation." |
| **Pragmatist** | "Post-hoc setters are YAGNI for v0.1. Dynamic classes barely exist yet. Just thread docs through ClassInfo and add setters if/when dynamic classes need them." |

### Tension Points

- The deepest tension is between live object mutability (Smalltalk/Pharo) and immutable compiled artifacts (BEAM/Erlang). We side with Smalltalk for the REPL experience but treat runtime doc mutations as session-local. Production systems should not rely on post-hoc doc mutations for correctness.
- Dropping EEP-48 trades BEAM ecosystem interop for simplicity. For v0.1, this is the right trade — no one is using Beamtalk docs from Erlang or Elixir. If interop becomes important, EEP-48 generation can be re-added as a thin layer that reads from `class_state` (the data is all there).

## Alternatives Considered

### Alternative A: Keep EEP-48 Alongside Runtime Docs

Continue generating EEP-48 chunks and add runtime docs as a parallel storage path.

**Rejected because:** Two sources of truth that can diverge. Post-hoc `doc:` mutations make EEP-48 stale. The `:h` command needs to decide which source to trust. The EEP-48 pipeline (`doc_chunks.rs`, `.docs` file, post-`erlc` injection) is the most fragile part of compilation. All this complexity for a feature no v0.1 user needs (Erlang shell doc interop).

### Alternative B: Lazy EEP-48 Accessor Only

Add a `doc` message on `Behaviour` and `CompiledMethod` that calls `code:get_doc/1` on demand. No new state, no setters, keep EEP-48 as the only storage.

```beamtalk
Counter doc   // => calls code:get_doc(counter), parses docs_v1 tuple
```

**Rejected because:** Doesn't work for dynamic classes (no `.beam` file). Also makes `doc` inconsistent — it works for compiled classes but returns nil for dynamic ones, with no way to fix it. The whole point is making all classes self-describing regardless of origin.

### Alternative C: Compiler-Only (No Setters)

Thread doc strings through the `ClassInfo` map during `register_class/0` (like method source today), but provide only a read-only `doc` message. No `doc:` or `setDocForMethod:to:`.

```beamtalk
Counter doc                     // => works (populated at compile time)
Counter doc: "new docs"          // => Error: does not understand #doc:
```

**Rejected because:** Breaks the Smalltalk principle that classes are live, mutable objects. Pharo's `comment:` is essential for interactive development — you fix a doc typo in the browser, not by recompiling. More practically, dynamic classes would have no way to get docs at all. The setters are simple (two intrinsics) and the flexibility is worth it.

## Consequences

### Positive
- **Self-describing objects**: Classes and CompiledMethods carry their documentation — no external lookup required
- **Works for dynamic classes**: Post-hoc setters provide docs for classes without `.beam` files
- **Simpler build pipeline**: Removing EEP-48 generation eliminates `doc_chunks.rs`, the `.docs` intermediate file, and post-`erlc` beam chunk injection
- **Simpler `:h` implementation**: `beamtalk_repl_docs.erl` can use `doc` messages and `>>` instead of parsing EEP-48 tuples and walking the hierarchy manually
- **Single source of truth**: Runtime `class_state` is the only place docs live — no divergence between beam chunks and process state
- **Principle alignment**: Satisfies Principle 6 (Messages All The Way Down) and 8 (Reflection as Primitive) — docs are just messages
- **Unified mechanism**: `///` and post-hoc setters both populate the same state — one runtime representation

### Negative
- **No BEAM doc interop**: `code:get_doc(counter)` will no longer return Beamtalk docs. Erlang `h/1` and Elixir `h/1` won't show Beamtalk class docs. Mitigated by: no v0.1 user needs this; EEP-48 can be re-added later by reading from `class_state`.
- **Memory cost**: Doc strings consume RAM proportional to total documentation volume (~50-100KB for a typical system)
- **Two new intrinsics**: `classDoc`, `classSetDoc` added to the intrinsic set; `classSetMethodDoc` added as a gen_server call (not an intrinsic — operates on class state directly)
- **Session-local mutations**: Unlike Pharo's changes file, `doc:` mutations don't persist to disk. This may surprise Smalltalk developers who expect image-like persistence.

### Neutral
- **`:h` REPL command unchanged**: Continues to work as before; implementation simplified to use `doc` messages internally, but the user-facing command is the same
- **`///` syntax unchanged**: No authoring changes — this ADR affects storage and access, not how docs are written
- **`>>` walks the hierarchy**: Previously local-only, now walks the superclass chain matching Pharo's behaviour. This is a semantic change — `Counter >> #class` returns a CompiledMethod instead of `nil`.
- **Doc write/read asymmetry**: `(Counter >> #selector) doc` reads inherited docs (via `>>` hierarchy walk); `setDocForMethod:to:` writes local docs only. This mirrors method dispatch — you can call inherited methods but only define methods locally.

## Implementation

### Phase 1: Runtime Storage, Intrinsics, and Beamtalk API (M)

**Affected components:** Runtime (`beamtalk_runtime`), stdlib (`lib/`)

1. Add `doc = none :: binary() | none` to `#class_state{}` in `beamtalk_object_class.erl`
2. Add `doc => binary() | none` to `method_info()` maps
3. Add `__doc__` field to `CompiledMethod` map construction (in `handle_call({method, Selector}, ...)`), populated from the `doc` key in local `method_info()`
4. Add `doc` dispatch to `beamtalk_compiled_method_ops.erl`
5. Handle `doc` and `method_docs` keys in `ClassInfo` map during `init/1` and `handle_call({update_class, ...}, ...)`
6. **Fix `>>` to walk the class hierarchy**: Update `beamtalk_method_resolver:resolve/2` to walk the superclass chain when a selector is not found in the local method dict. Return the CompiledMethod from the defining class (with its `__doc__`).
7. Implement two intrinsics in `beamtalk_behaviour_intrinsics.erl`:
   - `classDoc/1` — read `doc` from `class_state`
   - `classSetDoc/2` — write `doc` to `class_state`
8. Add `setDocForMethod:to:` as a gen_server call handler in `beamtalk_object_class.erl` — writes `doc` into `method_info()` for a given selector (error if selector not in local methods)
9. Add `doc`, `doc:`, `setDocForMethod:to:` methods to `lib/Behaviour.bt`
10. Register intrinsics in the compiler's intrinsic table
11. Add tests: `>>` walks hierarchy, CompiledMethod `doc` access, roundtrip via setters, error on non-local method

### Phase 2: Compiler Integration (M)

**Affected components:** Codegen (`beamtalk-core`)

1. In `methods.rs` (`generate_register_class`), emit `doc` and `method_docs` keys in the `ClassInfo` map, populated from `ClassDefinition.doc_comment` and `MethodDefinition.doc_comment`
2. Ensure `update_class` (hot reload path) also carries updated docs, re-syncing runtime docs with source
3. Add tests verifying doc roundtrip: `///` → compile → `Counter doc` returns the text

### Phase 3: Remove EEP-48 and Simplify REPL Docs (M)

**Affected components:** Codegen (`beamtalk-core`), workspace (`beamtalk_workspace`), compile escript

1. Remove `doc_chunks.rs` from codegen
2. Remove `.docs` file generation from the compile pipeline
3. Remove EEP-48 chunk injection from the compile escript
4. Rewrite `beamtalk_repl_docs.erl` to use `doc` messages and `>>` exclusively — remove all `code:get_doc/1` calls and EEP-48 parsing
5. Simplify `format_class_docs/1` and `format_method_doc/2` to delegate to the object protocol
6. Update any tests that assert on EEP-48 chunk contents

## Future Considerations

- **EEP-48 re-addition**: If BEAM ecosystem doc interop becomes important, EEP-48 chunks can be re-generated by reading from `class_state` at compile time or as a post-load step. The data is all there — this is an additive change.
- **Metaclass compatibility**: If ADR 0032 Phase 2 introduces the full metaclass tower, verify that `Behaviour.doc` and any metaclass-level doc protocol are compatible. The `doc` / `doc:` methods are sealed on Behaviour, so metaclass additions would need to compose with (not conflict with) the existing protocol.
- **Doc persistence**: A future `writeBack:` or workspace persistence feature could write `doc:` mutations back to `///` comments in `.bt` source files, bridging the gap between session-local mutations and file-based persistence.

## References
- **Supersedes (partially):** [ADR 0008](0008-doc-comments-and-api-documentation.md) Phase 3 (EEP-48 generation) — runtime docs replace EEP-48 as the doc access path. ADR 0008 Phases 1-2 (`///` syntax, AST storage) remain in effect.
- Related ADRs: [ADR 0032](0032-early-class-protocol.md) (Early Class Protocol — `Behaviour`/`Class` as stdlib classes, intrinsic pattern)
- Related issues: BT-496 (Doc Comments epic — implemented), BT-731 (Early Class Protocol epic)
- Pharo class comments: [ClassDescription>>comment](https://github.com/pharo-project/pharo/blob/Pharo12/src/Kernel-CodeModel/ClassDescription.class.st)
- Python docstrings: [PEP 257](https://peps.python.org/pep-0257/)
- EEP-48: https://www.erlang.org/eeps/eep-0048.html
