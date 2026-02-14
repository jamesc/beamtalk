# ADR 0005: BEAM Object Model - Pragmatic Hybrid Approach

## Status
Implemented (2026-02-08) — Epic BT-162

## Context

Beamtalk aims to bring Smalltalk's "everything is an object" philosophy to the BEAM virtual machine. However, BEAM's architecture fundamentally differs from traditional Smalltalk VMs:

- **BEAM:** Process-based, distributed, immutable data, no shared heap
- **Smalltalk VM:** Single heap, mutable objects, runtime introspection, stack manipulation

This creates tension: Should we try to emulate Smalltalk's object model fully (slow, complex) or embrace BEAM's strengths while accepting limitations?

**Key constraints:**
1. BEAM processes are isolated with separate heaps (no global memory access)
2. No runtime stack frame manipulation or continuations
3. Cannot swap object identity (`become:`) without rebuilding entire process state
4. Immutable data means copying, not mutation, for value types

**Inspiration:** [LFE Flavors](https://github.com/rvirding/flavors) by Robert Virding successfully implements OOP on BEAM using a pragmatic approach. Flavors proves that process-based objects with error isolation and method combinations work well on BEAM.

## Decision

**Adopt a pragmatic hybrid approach:** Embrace BEAM's actor model rather than fight it. Reify what we can efficiently (classes, methods, blocks, processes) and explicitly document what we cannot (active stack frames, `become:`, global reference scanning).

### Core Design Principles

1. **"Everything is a process" aligns with "everything is an object"** - We shift reification from memory-level objects to process-level actors
2. **Value types vs Actors** - Distinguish between heap-allocated values (Point, Color) and process-based actors (Counter, Server)
3. **Sealed primitives** - Integer, String, Float, Boolean are sealed value types that cannot be subclassed
4. **Uniform message-sending syntax** - All method calls use the same syntax, compiler chooses implementation (inline call vs process message)

### What We Support

| Smalltalk Feature | BEAM Support | Beamtalk Implementation |
|-------------------|--------------|-------------------------|
| Classes as objects | ✅ Full | Maps with metaclass protocol, registered via process registry |
| Methods as objects | ✅ Full | Wrapped funs with metadata (selector, arity, source) |
| Blocks as closures | ✅ Full | Erlang funs (first-class, can capture variables) |
| Objects with identity | ✅ Full | Actors: processes with pids + `#beamtalk_object{}` record<br>Values: tuples/maps copied by value |
| `doesNotUnderstand:` | ✅ Full | Gen_server `handle_call` fallback, structured errors |
| Method combinations | ✅ Full | Before/after methods (inspired by Flavors) |
| Error isolation | ✅ Full | Catch at instance, re-raise at caller with context |
| Reflection | ✅ Full | `class`, `respondsTo:`, `allInstances` (ETS tracking) |

### What We Don't Support

| Smalltalk Feature | BEAM Limitation | Workaround |
|-------------------|-----------------|------------|
| Stack frames/`thisContext` | No runtime stack access | Post-exception stack traces only |
| `become:` (identity swap) | Cannot replace process state atomically | Proxy pattern, manual migration |
| Global reference scan (`pointersTo`) | No shared heap | Manual tracking via ETS registry |
| Continuations | Not available on BEAM | Use futures/promises for async control flow |
| Direct slot access (instVarAt:) | Violates gen_server encapsulation | Primitives error, actors disallowed |
| Changing object's class | Cannot hot-swap gen_server behavior | Requires process restart |

### Class Hierarchy

```
ProtoObject (minimal - identity, DNU)
  └─ Object (common behavior - nil testing, printing, reflection)
       ├─ Integer      (primitive - sealed, no process)
       ├─ Float        (primitive - sealed, no process)
       ├─ String       (primitive - sealed, no process)
       ├─ Boolean      (primitive - sealed, no process)
       ├─ Array, List  (primitive - sealed, no process)
       ├─ Point, Color (user value types - no process)
       └─ Actor        (process-based - has pid, mailbox)
            └─ Counter, MyService (user actors)
```

**Value types** (Object subclasses):
- No BEAM process
- Instantiate with `new`
- State lives in caller's heap
- Copied when passed between processes
- Methods compiled to direct function calls

**Actors** (Actor subclasses):
- Has BEAM process (pid, mailbox)
- Instantiate with `spawn`
- State lives in own process
- Passed by pid reference
- Methods compiled to `gen_server:call/cast`

### Implementation Strategy

**Compile-time codegen** (not runtime interpretation):
- Parser generates AST
- Codegen emits Core Erlang
- `erlc` compiles to BEAM bytecode
- Runtime provides support libraries (error handling, reflection, object registry)

**Error handling:**
- All errors use `#beamtalk_error{}` records (structured)
- Errors caught at instance level, re-raised at caller with context
- Stack traces preserved through error propagation

**Reflection and introspection:**
- Classes registered via Erlang process registry and OTP process groups at module load
- Methods stored as metadata (selector, arity, source location)
- `allInstances` tracks actors via ETS (value types not tracked)

## Consequences

### Positive

1. **Leverage BEAM strengths:** Massive concurrency, distribution, fault tolerance, hot code reloading
2. **Performance:** Direct function calls for primitives, no interpreter overhead
3. **Proven approach:** LFE Flavors validates this design works in production
4. **Clear mental model:** "Value types vs actors" is familiar to modern developers (Swift, Rust)
5. **Erlang interop:** Seamless integration with Erlang/Elixir libraries
6. **Error isolation:** Process crashes don't affect other objects (Smalltalk debugger requires manual handling)

### Negative

1. **Not Smalltalk-compatible:** Cannot run Smalltalk code without modification
2. **Limited metaprogramming:** No `become:`, `thisContext`, or continuations
3. **Manual tracking:** `allInstances` and reference finding require explicit registration
4. **Learning curve:** Developers must understand value vs actor distinction
5. **Migration cost:** Converting value types to actors requires code changes

### Neutral

1. **Trade-off accepted:** We gain BEAM's strengths at the cost of some Smalltalk metaprogramming features
2. **Documentation burden:** Must clearly explain what works and what doesn't (this ADR helps)
3. **Future extensions:** Could add more Smalltalk features (e.g., better image snapshots) if BEAM capabilities improve

## Alternatives Considered

Four approaches were evaluated in detail in [beamtalk-object-model.md](../beamtalk-object-model.md) Part 3:

### Option 1: Pragmatic Hybrid (Selected)

Embrace BEAM's actor model. Reify classes, methods, blocks, and processes. Accept limitations on stack frames, `become:`, and global reference scanning.

- **Pros:** Leverages BEAM strengths, proven by LFE Flavors, simple mental model
- **Cons:** Cannot support full Smalltalk metaprogramming

### Option 2: Meta-Circular Interpreter

Build a Smalltalk-like VM on top of BEAM — interpret bytecodes, manage own heap, implement stack frames.

- **Pros:** Full Smalltalk compatibility including `thisContext` and `become:`
- **Cons:** Loses all BEAM advantages (concurrency, distribution, fault tolerance). Essentially building a VM inside a VM. Performance would be 10-100x slower.

### Option 3: Dual-Mode Execution

Compile to native BEAM for normal execution, switch to interpreter mode for metaprogramming operations that need stack access.

- **Pros:** Fast path for common code, full features when needed
- **Cons:** Extreme complexity. Two execution modes means two sets of bugs, subtle semantic differences, and difficult debugging. No existing language does this successfully.

### Option 4: CPS Transformation

Use continuation-passing style to make stack frames explicit and capturable.

- **Pros:** Could enable `thisContext` and limited continuations
- **Cons:** CPS bloats generated code significantly, makes debugging nearly impossible, and doesn't solve `become:` or global reference scanning. High cost for limited benefit.

### Why Pragmatic Hybrid Won

The meta-circular interpreter loses BEAM's core value proposition. Dual-mode is too complex to maintain. CPS solves one problem at high cost. The pragmatic hybrid accepts real limitations but delivers the best developer experience on BEAM.

## References

- **Related documents:**
  - [docs/beamtalk-object-model.md](../beamtalk-object-model.md) - Full technical analysis and feasibility study (1772 lines)
  - [docs/beamtalk-principles.md](../beamtalk-principles.md) - Core philosophy (actors, async-first, hot reload)
  - [docs/beamtalk-architecture.md](../beamtalk-architecture.md) - Compiler and runtime architecture
  
- **External inspiration:**
  - [LFE Flavors](https://github.com/rvirding/flavors) - Robert Virding's OOP implementation on BEAM
  - [The BEAM Book](https://blog.stenmans.org/theBeamBook/) - BEAM VM internals
  
- **Linear issues:**
  - BT-162: Epic: BEAM Object Model Implementation (ADR 0005) — umbrella epic for all object model work
  - BT-213: Value types vs actors implementation (completed 2026-02-03)
  - BT-169: Structured error system (`#beamtalk_error{}`)
  - BT-274: Design: Mixins/traits composition mechanism (Q9)
  
- **Related ADRs:**
  - [ADR 0004](0004-persistent-workspace-management.md): Persistent Workspace Management (REPL and image snapshots)
  - [ADR 0006](0006-unified-method-dispatch.md): Unified Method Dispatch with Hierarchy Walking (builds on this decision)
  - [ADR 0007](0007-compilable-stdlib-with-primitive-injection.md): Compilable Standard Library with Primitive Injection (pragma-based declarations for primitives replace hardcoded compiler dispatch tables; sealed enforcement is future work)

## Open Questions

1. ~~**ProtoObject vs Object boundary**~~ **DECIDED:** Follow Pharo's split. ProtoObject: `class`, `==`, `/=`, `doesNotUnderstand:args:`. Object: nil testing (`isNil`, `notNil`, `ifNil:ifNotNil:`), reflection (`respondsTo:`, `instVarNames`, `instVarAt:`, `perform:`, `perform:withArguments:`), display (`printString`, `printOn:`, `inspect`, `describe`), other (`yourself`, `hash`, `new`). Follows proven Smalltalk convention — no reinvention needed.
2. ~~**Metaclass protocol**~~ **DECIDED:** Commit to full Smalltalk metaclass model as the target. Phase 1 (current): class objects understand a fixed protocol via `beamtalk_object_class.erl` (`methods`, `superclass`, `name`, `new`/`spawn`). Class method sends (e.g., `Integer methods`) route through `gen_server:call` to the class process — not direct module calls. The `ClassReference` AST node already resolves to a `#beamtalk_object{}` wrapping the class pid; codegen just needs to emit `gen_server:call(ClassPid, {Selector, Args})` instead of `call 'module':'method'()`. Phase 2 (future): real metaclass hierarchy mirroring instance side, with class-side method inheritance through the metaclass chain.
3. ~~**Extension methods**~~ **DECIDED:** Extensions are logically part of the class's method dictionary, checked during the hierarchy walk — not a separate lookup step. Matches Pharo's model where extensions are regular methods. For sealed primitives (where we can't modify the compiled module), the runtime checks the extension registry (`beamtalk_extensions.erl`) as part of that class's method lookup before walking to the superclass. Extensions on Integer take priority over inherited Object methods, same as Pharo.
4. ~~**`#beamtalk_object{}` record**~~ **DECIDED:** Core object model decision. Actors are wrapped in `{beamtalk_object, Class, ClassMod, Pid}` — enables dispatch (extract pid → gen_server:call), follows LFE Flavors' `#flavor-instance{}` pattern. Value types (including primitives) are bare Erlang terms with no wrapper — dispatch must know receiver type at compile time. Trade-offs: free Erlang interop for value types, but you can't inspect a value type's class at runtime without compiler support.
5. ~~**Object identity and equality**~~ **DECIDED:** Follow ADR 0002 — use Erlang operators. `==` is value equality, `===` is exact equality. For actors: two references to the same pid are `==` (records compare by value). For value types: `42 == 42` is true (value comparison). Identity and equality collapse for actors (same pid = same identity = same value). Consistent across all object types, no special cases.
6. ~~**Nil**~~ **DECIDED:** `nil` is the Erlang atom `'nil'`, class is `UndefinedObject`, dispatched through `beamtalk_nil.erl`. Sealed primitive singleton value type. Follows Pharo's model. Already implemented.
7. ~~**Blocks as message receivers**~~ **DECIDED:** Blocks are Erlang funs dispatched through `beamtalk_block.erl` — same pattern as other primitives. They respond to `value`, `value:`, `value:value:`, `class`, `respondsTo:`, `perform:`, etc. No gen_server needed — dispatch is a direct function call. Already implemented.
8. ~~**Single dispatch**~~ **DECIDED:** Beamtalk uses single dispatch (method lookup based on receiver only). Follows Smalltalk, matches BEAM's gen_server model. Behavior composition across unrelated classes uses mixins/traits (see Q9) — the modern approach (Rust, Swift, Kotlin, Pharo). No plans for multiple dispatch.
9. **Mixins/traits**: Deferred to a future ADR. We commit to single dispatch + a composition mechanism (traits, mixins, or similar) for sharing behavior across unrelated classes. Design must work well on BEAM — may go beyond Erlang behaviours. Needs its own ADR to explore Pharo traits, Newspeak mixins, and BEAM-specific options. Tracked as BT-274.
10. ~~**`self` and `super` semantics**~~ **DECIDED:** Follow Smalltalk. `self` always refers to the enclosing object — in actors it's the `#beamtalk_object{}` record, in value types it's the value itself, in blocks it's the enclosing method's `self` (lexical capture). `super` refers to the enclosing class's superclass, never the block. Already implemented via state threading in codegen.
