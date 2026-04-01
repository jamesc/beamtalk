# ADR 0074: Deferred Metaprogramming

## Status
Accepted (2026-04-01)

## Context

Beamtalk takes inspiration from Smalltalk-80's metaprogramming model, where classes are objects, methods are objects, and the runtime is fully reflective. However, the BEAM VM has fundamentally different architecture from a Smalltalk image — process isolation, immutable terms, no shared heap, no stack frame reification.

During early development (BT-151), a comprehensive metaprogramming design was drafted covering eight feature areas. Some have since shipped; this ADR documents what remains **deliberately deferred** and why.

### What Has Shipped

| Feature | Shipped via | Notes |
|---------|------------|-------|
| Metaclass tower | ADR 0036 | `Counter class`, `Counter class class`, `Metaclass` stdlib class |
| Runtime-embedded docs | ADR 0033 | `CompiledMethod` with `doc`, `source`, `selector`; `Class doc:` setters |
| Class protocol (Behaviour/Class) | ADR 0032 | `superclass`, `methods`, `allSuperclasses`, `respondsTo:` |
| Unified method dispatch | ADR 0006 | Hierarchy-walking dispatch via `beamtalk_message_dispatch:send/3` |
| Class objects as values | ADR 0013 | `cls := Beamtalk classNamed: #Counter; cls spawn` works |
| `self` in class methods | ADR 0013 | Class methods receive a `#beamtalk_object{}` as `self` |
| Class method inheritance | ADR 0032 | Class-side dispatch walks the superclass chain |
| Method hot-patching | — | `beamtalk_object_class:put_method/3` updates method dictionaries at runtime |
| System reflection | — | `Beamtalk allClasses`, `Beamtalk classNamed:`, `Beamtalk help:` |
| Field-based reflection | ADR 0035 | `fieldNames`, `fieldAt:`, `fieldAt:put:` |

### What Remains Deferred

Three categories of Smalltalk-80 metaprogramming are deliberately deferred:

1. **`thisContext` (stack frame reification)** — Smalltalk-80 exposes the execution stack as first-class objects: `thisContext sender`, `thisContext restart`, `thisContext method`. This enables debugger integration, continuations, and coroutines.

2. **`become:` (identity swap)** — Smalltalk-80 allows atomically swapping all references to two objects: `obj1 become: obj2`. This enables schema migration, proxy replacement, and transparent forwarding.

3. **Classes as actors** — The original design doc envisions each class as a full actor process: supervised, with mutable method dictionary state, participating in OTP lifecycle. Currently, class objects are backed by `beamtalk_object_class` gen_server processes that hold metadata and support dispatch, but these are runtime infrastructure — not user-visible actors with `state:` declarations, supervision trees, or actor lifecycle semantics.

## Decision

Defer these three features indefinitely. Each is either **impossible on BEAM** (thisContext, become:) or has a **poor cost/benefit ratio** at the current stage (class-as-actor). Document the reasoning so future developers don't re-derive these conclusions.

### 1. `thisContext` — Not Implementable on BEAM

**What Smalltalk provides:**
```smalltalk
thisContext              "Current stack frame"
thisContext sender       "Caller's frame"
thisContext method       "Current method"
thisContext restart      "Re-enter current frame"
```

**Why BEAM can't do this:**
- BEAM is a register-based VM — stack frames are internal implementation details, not reifiable objects
- Stack traces are only available *after* an exception (via `catch Class:Reason:Stacktrace` or `process_info(Pid, current_stacktrace)`)
- No way to capture, store, or resume a stack frame as a value
- This is a fundamental VM architecture difference, not a missing API

**What Beamtalk provides instead:**
- `StackFrame` value class — post-exception introspection of stack traces with `method`, `moduleName`, `arguments`, `sourceLocation`
- `Exception stackTrace` returns a list of `StackFrame` objects
- Compile-time method identity is available via `@primitive` metadata

**Impact:** The main Smalltalk use cases for `thisContext` are debuggers and continuations. BEAM debuggers use `int` module tracing instead of stack reification. Continuations are better served by BEAM processes and message passing.

### 2. `become:` — Not Implementable on BEAM

**What Smalltalk provides:**
```smalltalk
obj1 become: obj2   "All references to obj1 now point to obj2"
```

**Why BEAM can't do this:**
- BEAM processes have **separate heaps** — there is no shared object graph to scan
- Pids are immutable identifiers; you cannot redirect a pid to a different process
- Terms passed between processes are **copied**, not shared — there are no pointers to update
- Even within a single process, Erlang terms are immutable values, not mutable heap objects

**Workarounds documented in the design doc:**
- **Proxy pattern:** An actor that forwards all messages to a mutable target. Only works if all references go through the proxy.
- **Registry pattern:** A global registry mapping stable names to current pids. `beamtalk_class_registry` already uses this pattern.

**Impact:** The main Smalltalk use cases for `become:` are schema migration and transparent proxies:
- **Schema migration:** On BEAM, actor state migration is handled by OTP's `code_change/3` callback during release upgrades. For user-triggered method hot-patches (via `put_method/3`), existing actor state is preserved — the new method operates on the existing state shape.
- **Transparent proxies:** Handled by `doesNotUnderstand:` delegation. This adds dispatch overhead on the proxy path (not globally), and requires the proxy to exist as a separate object rather than transparently replacing the original. Both the proxy and registry workarounds share a fundamental BEAM limitation: neither achieves cross-node identity replacement, since distributed message passing copies terms.

### 3. Classes as Full Actors — Deferred by Choice

**What the design doc envisions:**
- Each class is a supervised actor process with OTP lifecycle
- Method dictionaries are mutable actor state
- Class objects participate in supervision trees
- `doesNotUnderstand:` on metaclasses enables dynamic class behaviour
- Classes can be stopped, restarted, migrated like any actor

**Current state:**
- Class objects ARE backed by gen_server processes (`beamtalk_object_class`)
- You CAN send messages to class objects, call class methods, pass classes as values
- Method hot-patching works via `put_method/3`
- But class processes are **runtime infrastructure**, not user-visible actors — no `state:` declarations, no user supervision, no actor lifecycle hooks

**Why defer:**
- **Complexity cost:** Making class processes full actors requires changes across the compiler, runtime, bootstrap, and supervision infrastructure. The bootstrap ordering alone is delicate — class processes must exist before any user code runs.
- **Limited practical value for v0.1:** The use cases enabled by class-as-actor (runtime mixin injection, metaclass-level interception, class migration) are exotic. The principled argument is that these features are framework-author tools, not application-developer tools — and Beamtalk doesn't yet have a framework ecosystem that would exercise them.
- **Performance concern:** Routing all dispatch through actor message passing (gen_server:call) adds latency. Current hybrid approach (compiled dispatch with dynamic overlay) is faster.
- **Incremental path exists:** The current `beamtalk_object_class` gen_server state already holds method dictionaries, superclass refs, and metadata. Promoting these to supervised actors is additive, not a rewrite — though this becomes harder if the class process API accumulates dependents without supervision contracts.

**Revisitation triggers:** This deferral should be revisited if: (a) a Beamtalk framework needs metaclass-level `doesNotUnderstand:` for dynamic routing, (b) class process crashes become a reliability concern in production, or (c) multi-node hot-patching requires class processes to participate in distributed state protocols.

## Prior Art

### Smalltalk-80 / Pharo
Full metaprogramming: `thisContext`, `become:`, classes as first-class objects with metaclasses. All enabled by a single shared heap with mutable object graph. Pharo's debugger relies heavily on `thisContext` for stack manipulation.

### Newspeak
Classes are first-class values and can be nested, but Newspeak doesn't rely on `thisContext` or `become:` — its module system achieves dynamism through class parameterization instead. Closer to Beamtalk's pragmatic approach.

### Erlang/OTP
No object model — modules are static, processes are the unit of identity. Hot code loading replaces entire modules atomically. `code:purge/1` and `code:load/2` are the metaprogramming primitives. Beamtalk's method-level hot-patching goes beyond this.

### Elixir
Compile-time metaprogramming via macros and `__using__`. No runtime `become:` or `thisContext`. Protocols provide dynamic dispatch without method dictionaries. Runtime reflection is limited to module attributes and `__info__/1`.

### Pony
No `become:` or `thisContext`. Reference capabilities ensure data-race safety through the type system rather than runtime reflection. Classes are not first-class values. Shows that a modern actor language can succeed without Smalltalk-80 metaprogramming.

## User Impact

**Newcomer:** No impact — these features are advanced and not expected by developers coming from Python/JS/Ruby.

**Smalltalk developer:** Will notice the absence of `thisContext` (debugger integration) and `become:` (transparent proxies). The workarounds (StackFrame, doesNotUnderstand: delegation) are adequate for most use cases but feel less elegant. Class objects being values-not-actors is unlikely to matter in practice.

**Erlang/BEAM developer:** Will find this natural — BEAM developers don't expect stack reification or identity swapping. The current reflection API (`class`, `methods`, `respondsTo:`) goes well beyond what Erlang offers.

**Production operator:** Benefits from the deferral — class processes as runtime infrastructure (not supervised actors) means fewer moving parts in the supervision tree, simpler debugging via Observer, and no risk of class process crashes affecting user actors.

## Steelman Analysis

### For implementing `thisContext` now:
- **Smalltalk purist:** "Debugger integration is a killer feature. Pharo's debugger can modify running code *in the stack frame*. Without `thisContext`, Beamtalk's debugger will always be second-class."
- **Rebuttal:** Valid concern, but BEAM's `int` module tracing and `dbg` provide equivalent debugging power through a different mechanism. The BEAM approach is actually better suited to distributed systems where stack frames span nodes.

### For implementing `become:` now:
- **Language designer:** "`become:` enables the most elegant proxy patterns. `doesNotUnderstand:` delegation is a poor substitute — it requires the proxy to exist as a separate object rather than transparently replacing the original."
- **Rebuttal:** True in a shared-heap VM. On BEAM, neither `become:` emulation nor DNU delegation achieves cross-node identity replacement — BEAM's distribution model copies terms. The proxy pattern makes this limitation explicit rather than hiding it behind a `become:` API that only works locally.

### For implementing class-as-actor now:
- **Smalltalk purist:** "Classes should be fully live objects. I want to supervise class processes, add methods dynamically, and have metaclass-level `doesNotUnderstand:` for framework hooks."
- **BEAM veteran:** "Making class processes unsupervised gen_servers is an OTP anti-pattern. If a class process crashes, recovery semantics are unclear."
- **Rebuttal:** The incremental path from current infrastructure to full class-as-actor is preserved. Implementing now would add complexity to bootstrap ordering and supervision without clear user demand.

### Tension Points
- Smalltalk purists would prefer all three features; every other cohort is comfortable with the deferral
- The BEAM veteran's concern about unsupervised class processes is valid but contained — class processes are started during `on_load` and are extremely stable in practice

## Alternatives Considered

### Alternative: Partial `thisContext` via Process Dictionary
Inject `{current_method, {Module, Selector, Arity}}` into the process dictionary at each method entry. Provides `thisMethod` but not `thisContext sender` or `thisContext restart`.

**Rejected:** The overhead of process dictionary writes on every method call is measurable, and the feature this enables (knowing the current method name) has limited utility compared to full `thisContext`. `StackFrame` already provides post-exception method identity.

### Alternative: `become:` via Global Registry
Implement a global name registry where all object references are indirected through a lookup table. `become:` updates the table entry.

**Rejected:** Requires *all* object references to be indirected through the registry (global overhead), unlike the proxy pattern which only adds indirection to objects that need it (local overhead). Both share the single-node limitation. The registry approach also breaks the BEAM convention that pids are stable, direct identifiers — a property that tools like Observer and `recon` depend on.

### Alternative: Opt-In Supervised Class Processes
Allow specific class processes to be supervised via a modifier (e.g. `supervised class Foo`), rather than requiring all class processes to be full actors.

**Rejected for now:** Narrower scope avoids the global bootstrap ordering problem, but still requires the compiler to distinguish supervised vs unsupervised class processes and generate different registration paths. A reasonable future step if class process reliability becomes a concern, but premature without evidence of class process crashes in practice.

### Alternative: Implement Class-as-Actor for v0.1
Promote all class processes to full supervised actors with `state:` declarations and OTP lifecycle.

**Rejected:** The bootstrap ordering is already the most complex part of the runtime. Adding supervision to class processes would require solving circular dependencies (supervisor classes need to be loaded to supervise class loading). The incremental path means this can be added later without breaking changes.

## Consequences

### Positive
- **Simpler runtime:** Fewer moving parts in the process architecture. Class processes are stable infrastructure, not dynamic actors that can crash and restart.
- **Better performance:** Compiled dispatch with dynamic overlay is faster than routing all dispatch through actor message passing.
- **Clearer mental model:** Users don't need to reason about class process lifecycle, supervision, or restart semantics.
- **Faster bootstrap:** Class registration during `on_load` is straightforward — no supervision tree to build before classes exist.

### Negative
- **Debugger limitations:** Cannot build a Pharo-style stack-manipulating debugger. Must rely on BEAM tracing tools (`int`, `dbg`, Observer) instead.
- **No transparent proxies:** `become:` absence means proxy patterns require explicit delegation via `doesNotUnderstand:`.
- **Multi-node hot-patching is unspecified:** Method hot-patches via `put_method/3` are node-local. In a multi-node cluster, each node's class processes diverge after a hot-patch. Supervised class actors could participate in distributed state protocols to solve this; without them, multi-node hot-patch consistency is left to the user.
- **Class process crash semantics are implicit:** If a class process crashes (e.g. via a malicious `put_method/3` call), recovery depends on whether `beamtalk_object_class` is linked to a supervisor. Currently class processes are started unlinked during `on_load` — a crash is unrecoverable without reloading the module.

### Neutral
- **This ADR is the authoritative reference** for which Smalltalk-80 metaprogramming features Beamtalk implements, defers, or rules out. The original design doc has been removed — this ADR supersedes it.
- The `beamtalk_object_class` gen_server architecture is explicitly designed to evolve toward class-as-actor if demand materialises. This deferral is a scope decision, not an architectural dead end.
- `known-limitations.md` documents the user-facing limitations concisely.

## Implementation

No implementation work — this is a documentation decision. Completed alongside this ADR:

1. Updated `docs/known-limitations.md` to reflect that classes are first-class values (not actors).
2. Removed `docs/internal/design-metaprogramming.md` — this ADR supersedes it.

## References
- Related issues: [BT-303](https://linear.app/beamtalk/issue/BT-303)
- Source material: `docs/internal/design-metaprogramming.md` (removed — superseded by this ADR)
- Related ADRs: [ADR 0005](0005-beam-object-model-pragmatic-hybrid.md) (object model), [ADR 0006](0006-unified-method-dispatch.md) (method dispatch), [ADR 0013](0013-class-variables-class-methods-instantiation.md) (class objects), [ADR 0032](0032-early-class-protocol.md) (Behaviour/Class protocol), [ADR 0033](0033-runtime-embedded-documentation.md) (CompiledMethod docs), [ADR 0036](0036-full-metaclass-tower.md) (metaclass tower)
- Documentation: `docs/known-limitations.md`
