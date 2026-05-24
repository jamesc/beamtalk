# ADR 0084: Class-Side Runtime Method Installation and Fun Dispatch

## Status
Accepted (2026-05-24)

## Context

### Problem

Beamtalk can install and dispatch **instance** methods at runtime — this is how
`>>` live patching (ADR 0066), `Behaviour compile:source:` (ADR 0082), and the
programmatic `ClassBuilder` (`methods:`, ADR 0038) all work. It cannot do the
same for **class-side** methods. There is no way to:

- give a programmatic `ClassBuilder` a *callable* class method
  (`Object classBuilder name: #C; classMethods: #{ #zero => [...] }; register`),
- live-patch a class method (`Counter class >> reset => …`), or
- `compile:source:` a class-side selector (ADR 0082's class-side path).

This blocks the "first-class ClassBuilder" epic (BT-2259) and the class-side
half of the live-editing story that ADR 0066 / ADR 0082 already sanctioned at
the syntax level (`ClassName class >> sel => body` parses today with an
`is_class_method` flag, but has no runtime dispatch path).

### Current state

**Instance side (works).** Instance dispatch checks a runtime method source
before the compiled class chain and invokes it as a fun that threads state:

- `beamtalk_dispatch:check_extension/2` → `invoke_extension(Fun, Args, Self, State)`
  runs `fun(Args, Self, State) -> {Result, NewState}` and commits `NewState`
  back to the gen_server (`beamtalk_dispatch.erl:459-466`).
- The fun is **compiler-generated**: `super`-sends and self-sends are lowered to
  explicit dispatch calls, and instance-variable writes are threaded as
  `{Result, NewState}`. It is never a naive user closure.

**Class side (does not work).** Class-method dispatch only ever calls a
*compiled* function:

- `beamtalk_class_dispatch:apply_class_method_in_context/6`
  (`beamtalk_class_dispatch.erl:457-493`) does
  `erlang:apply(DefiningModule, class_<sel>, [ClassSelf, ClassVars | Args])`
  and nothing else. There is no runtime-method lookup and no fun path.
- `beamtalk_class_builder:build_compiled_class_info/8` stores
  `class_methods => ClassMethods` **verbatim** — unlike instance methods, class
  methods are *not* run through `build_method_map/1`, so builder-supplied funs
  are never wrapped into a dispatchable shape
  (`beamtalk_class_builder.erl:316-318`).
- There is no `put_class_method/4` to mirror the instance-side
  `beamtalk_object_class:put_method/4` (`beamtalk_object_class.erl:642`).

### The BT-873 precedent (the reason for caution)

BT-873 **removed** "Path 2" — closure-based dynamic class dispatch — because it
was fundamentally broken: state mutation was silently dropped, `super` calls did
not work, and self-sends were broken (ADR 0038, "Single Path to Class Creation").
Any proposal to install class methods as runtime funs must explain why it will
not reproduce those failures.

### Constraints

1. **No BT-873 repeat.** Runtime class-method funs must thread class-variable
   state, resolve `super` up the metaclass chain, and make `self` sends dispatch
   as class messages — correctly, not "mostly".
2. **Class variables, not instance variables.** Class methods mutate *class
   variables* via a distinct protocol: a compiled class method returns
   `{class_var_result, Result, NewClassVars}` (or a plain value), and the class
   gen_server commits `NewClassVars` (`beamtalk_class_dispatch.erl:99,116,421`).
   This is *not* the instance `{Result, NewState}` protocol.
3. **`self` is the class.** Inside a class method, `self new` / `self otherCM`
   must dispatch as class messages against
   `ClassSelf = #beamtalk_object{class = Tag, class_mod = DefiningModule, pid = self()}`
   (`beamtalk_class_dispatch.erl:465-469`).
4. **One registry entry.** An edited or builder-defined class stays a single
   `beamtalk_class_registry` entry; instances keep working; the class stays
   navigable (`method_source` / `SystemNavigation`).
5. **Document/codegen rule.** All Core Erlang stays in the `Document` / `docvec!`
   API — no `format!` for codegen (CLAUDE.md, BT-875).

## Decision

**Add a class-side runtime method path that mirrors the compiled class-method
calling convention exactly, fed only by compiler-generated funs.**

### 1. Calling convention — identical to compiled class methods

A runtime-installed class-method fun has the **same signature and return
protocol** as a compiled `class_<Selector>` function:

```erlang
%% Compiled (today):   DefiningModule:class_<sel>(ClassSelf, ClassVars, A1, ..., An)
%% Runtime fun (new):  fun(ClassSelf, ClassVars, A1, ..., An) -> Result
%%                                                            |  {class_var_result, Result, NewClassVars}
```

`arity = n + 2`. Class-variable mutation is threaded through the *existing*
`{class_var_result, Result, NewClassVars}` return that every caller already
handles. No new state protocol is introduced.

### 2. Storage — the class gen_server `class_methods` map

Runtime class methods are stored as `#{block => Fun, arity => Arity}` in the
class gen_server's `class_methods` map, mirroring how instance methods are
stored in `instance_methods` (`beamtalk_object_class.erl:642-644`). Two changes:

- New `beamtalk_object_class:put_class_method/4` (mirrors `put_method/4`):
  installs/replaces one class method, clears stale class-side signature and
  return-type entries (as `put_method` does for the instance side).
- `beamtalk_class_builder:build_compiled_class_info/8` runs `classMethods:` funs
  through `build_method_map/1` (the wrapper instance methods already use), so
  builder-supplied class-method funs become `#{block, arity}` entries.
- **`put_class_method/4` keeps two ETS-backed stores in sync, because
  class-method resolution is a *two-step* lookup that must work for inherited
  methods without a per-send `gen_server` hop:**
  1. **Discoverability** — *which ancestor defines the selector.* Inherited
     resolution walks the chain via `beamtalk_class_metadata:lookup_methods/1`
     (ETS: selectors + module per class, `beamtalk_class_dispatch.erl:589`).
     `init/1` and `apply_class_info/2` already insert `maps:keys(ClassMethods)`
     here (`beamtalk_object_class.erl:419,1078`), so register-time builder class
     methods are discoverable by subclasses automatically; a live
     `put_class_method` must add its selector here too.
  2. **Retrieval** — *get the installed fun from that ancestor.* The metadata
     table holds only selectors + module, so it **cannot return the fun**
     (Copilot review, PR #2297). Runtime class-method funs are therefore also
     recorded in a dedicated ETS store keyed by
     `{DefiningClass, Selector} -> #{block, arity}` (an extension of
     `beamtalk_class_metadata` or a sibling table), written by both
     `build_compiled_class_info/8` (register-time) and `put_class_method/4`
     (live). After the chain walk resolves `DefiningClass`, dispatch does a cheap
     ETS read on `{DefiningClass, Selector}` — no `gen_server` hop, the same cost
     model as the existing metadata lookups.

  The gen_server `class_methods` map stays the **source of truth** (reflection,
  `update_class` rebuild, `local_class_methods`); the ETS store is the
  dispatch-time read cache, kept in sync on every install / replace / remove.
  Compiled methods are **absent** from the retrieval store — they are still
  `erlang:apply`'d by module — so the compiled hot path is untouched. The
  instance side has no equivalent obligation because instance dispatch consults
  the extension registry / gen_server map directly; the class side is genuinely
  different and the implementation must not assume symmetry.

### 3. Dispatch — check the map first, then fall back to compiled

`beamtalk_class_dispatch:apply_class_method_in_context/6` runs after the chain
walk has resolved the `DefiningClass`/`DefiningModule` for the selector. It does
a cheap ETS read on the retrieval store (§2) for a runtime fun before the
compiled apply — both keyed by the *defining* class, so this works identically
for own and inherited methods:

```erlang
%% DefiningClass already resolved by find_class_method_in_chain/2.
case beamtalk_class_metadata:lookup_class_method_fun(DefiningClass, Selector) of
    {ok, #{block := Fun}} ->
        apply(Fun, [ClassSelf, ClassVars | Args]);     %% runtime fun (ETS read, no hop)
    error ->
        FunName = class_method_fun_name(Selector),
        erlang:apply(DefiningModule, FunName, [ClassSelf, ClassVars | Args])  %% compiled (today)
end
```

Same arguments, same `{class_var_result, …}` handling, same error
classification. A runtime override of a compiled selector shadows it (last
writer wins), matching instance-side live-patch semantics.

This adds one ETS branch before the compiled apply; it does **not** violate ADR
0006 (unified dispatch) — it is the same "runtime source shadows compiled"
pattern the instance side already uses for extensions, with a single resolution
path. To honour BT-2008 (no per-send `gen_server` hops on the hot path), the
retrieval is an ETS read keyed by the already-resolved `DefiningClass` — never a
`gen_server` call — and is gated so compile-time-only classes (no runtime class
methods) skip it entirely, e.g. a per-class "has runtime class methods" flag in
the metadata row, checked before the ETS read.

**`ClassSelf.class_mod` for funs.** For a compiled method, `class_mod` is the
defining module, and `super`/self-helpers are co-resident named exports in that
module. An anonymous fun has **no** `class_<sel>` export and may not live in the
defining BEAM module at all, so `class_mod = DefiningModule` cannot be relied on
to locate the fun's helpers. The fun must be self-contained: all of its
`super`/self/helper calls are captured in the closure at compile time (see §4),
so dispatch only needs `ClassSelf` to carry the correct *class identity* (tag +
defining class name) for further self-sends — not a module it can `erlang:apply`
into.

### 4. Funs are compiler-generated — the BT-873 guard

The runtime funs are produced by **the compiler**, reusing the lowering it
already applies to named `class_<sel>` functions, emitting it into an anonymous
fun instead of a module export. The safety of this ADR rests on this lowering
being concretely specified — "the compiler will do it right" is a plan, not an
argument — so the implementation (Phase 2) must establish the following contract,
each independently tested *for the fun path* before Phase 2 is considered done:

1. **Class-variable threading.** Writes to class variables lower to the existing
   `{class_var_result, Result, NewClassVars}` return; the dispatch wrapper commits
   `NewClassVars`. Test: a builder/patched class method that mutates a class
   variable, called twice, accumulates (the BT-873 "dropped state" regression).
2. **`super` resolution without module identity.** `super sel` inside a
   class-method fun lowers to an explicit, **compile-time class-name-keyed**
   superclass dispatch (e.g. `beamtalk_class_dispatch:class_self_dispatch(
   SuperclassName, Sel, ClassVars, Args)`), with `SuperclassName` resolved from
   the *defining class* at compile time and captured in the closure. It must
   **not** rely on `erlang:apply(DefiningModule, …)` or on `ClassSelf.class_mod`,
   because an anonymous fun has no such export (see §3). Test: a builder class
   method whose body calls `super`, resolving to the superclass's class method.
3. **`self`-sends.** `self new` / `self otherCM` lower to explicit class-message
   dispatch against the `ClassSelf` the wrapper passes in. Test: a class method
   that calls another class method and `self new`.

They are never naive user closures. **This is precisely the property BT-873's
Path 2 lacked** — Path 2 wrapped raw blocks with no lowering, so it dropped state
and broke `super`/self. With the contract above, the runtime fun is
*behaviourally* equivalent to a compiled class method — verified by parity tests,
not assumed — differing only in *location* (an anonymous fun in the gen_server
map vs a named export). The one thing a named export can do that an anonymous fun
cannot is call **module-local helper functions** by name; the lowering must
therefore either inline such helpers into the closure or route them through a
stable module (the stdlib runtime), never through the (possibly absent) defining
module.

### 5. Scope boundary — own methods vs cross-class extensions

This ADR covers a class installing/redefining **its own** class methods (builder
creation + live edit of a class you own). **Cross-class class-side extensions**
(`SomeForeignClass class >> sel` from another package) belong to ADR 0066's
extension-registry model and would use a symmetric *class-side extension
registry*; that is explicitly out of scope here and noted as future work.

### REPL session

```beamtalk
>> c := Object classBuilder
     name: #Tally;
     superclass: Object;
     classState: #{ #total => 0 };
     classMethods: #{ #bump => [:self | self.total := self.total + 1. self.total] };
     register
=> Tally
>> Tally bump
=> 1
>> Tally bump
=> 2                                  // class variable threaded — not dropped

>> Counter class >> reset => self.count := 0   // live class-method patch
=> a CompiledMethod (#reset in Counter class)
>> Counter reset
=> 0
```

### Error example

```beamtalk
>> Object classBuilder name: #Bad; superclass: Object;
     classMethods: #{ #oops => [:self | super nope] }; register
>> Bad oops
=> error: Bad class does not understand #nope
         (super resolved up the metaclass chain — no silent drop)
```

## Prior Art

### Pharo / Squeak Smalltalk
A class method is just a method in the *metaclass's* method dictionary; the
System Browser installs a `CompiledMethod` into `Counter class` exactly as it
installs one into `Counter`. There is no second mechanism — instance side and
class side are symmetric. **Adopted:** the symmetry — class methods get the same
runtime-install path as instance methods. **Adapted:** Pharo compiles to
bytecode in the image; we compile block bodies to BEAM funs and store them in
the class gen_server, but the *calling convention matches the compiled class
method* so there is one behavioural contract, not two.

### Newspeak
Class-side state and methods live on the class object, mutated through ordinary
message sends; the IDE edits live class objects. **Adopted:** class methods as
first-class, runtime-editable members of the class object. **Diverged:** we keep
files as source of truth (ADR 0004); runtime install is memory-only until
flushed (ADR 0082).

### Erlang / BEAM
No class concept; `code:load_binary/3` hot-swaps a whole module. Our class
gen_server map is the finer-grained analogue — one selector at a time — without
recompiling the module. **Adopted:** memory-only runtime install, file remains
authoritative.

### Ruby
A class method is a method on the object's *singleton class* (metaclass); Ruby
installs them at runtime with `define_singleton_method` / `def self.x` / `class
<< self`, which mutate the singleton class's method table exactly as
`define_method` mutates the instance method table. State accessed via class
instance variables is read/written through the same `self` (the class object).
**Adopted:** the symmetry — runtime-defined class methods are first-class and
mutate the class object's own method table, not a side registry. **Diverged:**
Ruby has no compile step and no `super`-lowering concern — its dynamic dispatch
resolves `super` at call time via the ancestor chain; Beamtalk lowers `super`
ahead of time so the runtime fun carries explicit chain dispatch (the property
that prevents BT-873's `super` breakage).

### Beamtalk instance side (the local precedent)
The instance extension path (`invoke_extension`, ADR 0066) already proved that a
compiler-generated fun with proper state threading dispatches correctly at
runtime. This ADR is its class-side mirror, differing only where the domain
genuinely differs (class-variable threading; `self`-is-the-class).

## User Impact

- **Newcomer:** `Object classBuilder … classMethods: …; register` and
  `Counter class >> reset => …` both just work, symmetric with the instance
  forms they already learned. No new vocabulary.
- **Smalltalk developer:** matches the Pharo expectation that class-side methods
  are editable exactly like instance-side. Removes a surprising asymmetry.
- **Erlang/BEAM developer:** runtime class methods are observable in the class
  gen_server state (`class_methods` map); dispatch remains a normal apply with a
  predictable convention; no naive-closure surprises.
- **Operator:** class-method live patches are memory-only and (under ADR 0082)
  logged in the ChangeLog as `flushable: false` for dynamic classes — same
  audit story as instance patches.
- **Tooling developer:** builder/patched class methods carry `class_method_source`
  (BT-2195 channel), so `SystemNavigation` class-side scans see them.

## Steelman Analysis

### Alternative A — Class-side extension ETS registry (mirror ADR 0066 exactly)
- ⚙️ **BEAM veteran:** "The instance side uses an ETS extension table, not the
  gen_server map. Symmetry argues for the *same* table on the class side."
- 🎩 **Smalltalk purist:** "An extension registry already exists; reuse it
  instead of inventing a second store."
- **Why not (primary):** the ETS extension registry exists to add methods to
  *foreign* classes you don't own (cross-package `String >> shout`). Builder
  creation and editing *your own* class are naturally the class object's own
  state — the gen_server `class_methods` map, mirroring instance
  `instance_methods`. Cross-class class-side extensions *will* want the registry
  (noted as future work), but routing own-class methods through a foreign-method
  registry inverts ownership and complicates `class new`/metadata reads that
  already read the gen_server map. Chosen design keeps own-methods in the class,
  registry for foreign-methods — exactly the instance-side split.

### Alternative B — A distinct closure convention `fun(Args, ClassSelf, ClassVars)`
- 🎨 **Language designer:** "Match the instance extension shape
  `fun(Args, Self, State)` for cross-side consistency."
- **Why not:** it would diverge from the *compiled* class-method convention
  `(ClassSelf, ClassVars | Args)` that the dispatch site already uses, forcing a
  second adapter and a second `{…}` return shape. Matching the compiled
  convention means `apply_class_method_in_context` changes one line (which apply
  to call) and the `{class_var_result, …}` handling is untouched. One contract,
  not two.

### Alternative C — Status quo: class methods are compile-only
- 🏭 **Operator:** "Fewer runtime moving parts; class methods only come from
  compiled modules — maximally predictable."
- **Why not:** blocks BT-2259 entirely (no callable builder class methods) and
  leaves ADR 0066's `ClassName class >> sel` syntax permanently non-functional at
  runtime. The asymmetry (instance methods editable, class methods not) is a
  papercut every Smalltalker hits.

### Alternative D — Generic closure dispatch (revive BT-873 Path 2)
- 🧑‍💻 **Newcomer:** "Just store the block and call it — simplest possible."
- **Why not:** this *is* BT-873, which was removed for dropping state and
  breaking `super`/self. The decisive difference in this ADR is that funs are
  **compiler-lowered**, not raw blocks. Rejected explicitly.

### Tension points
- BEAM veteran (Alt A, ETS) vs the ownership model: resolved by scoping
  own-methods to the gen_server and foreign-methods to a future registry.
- Newcomer simplicity (Alt D) vs correctness (BT-873): correctness wins; the
  compiler does the heavy lifting so the simple-looking surface stays safe.

## Alternatives Considered
See Steelman. **A** (class-side ETS registry) — deferred to the cross-class
case. **B** (distinct convention) — rejected, gratuitous divergence from the
compiled shape. **C** (compile-only) — rejected, blocks the epic. **D** (generic
closures) — rejected, reproduces BT-873.

## Consequences

### Positive
- Programmatic `ClassBuilder` class methods become callable; `ClassName class >>`
  and class-side `compile:source:` (ADR 0082) get a runtime path.
- Instance/class symmetry: one runtime-install mental model.
- Minimal dispatch change — one branch added before the existing compiled apply;
  `{class_var_result, …}` handling reused verbatim.
- No new state protocol; class-variable threading reuses the compiled contract.

### Negative
- Compiler must lower class-method block bodies with full class-var/`self`/`super`
  lowering (the work that makes it safe). This is the bulk of the effort and the
  BT-873-risk surface — it must be tested, not assumed.
- A class can now have a class method in *two* places (compiled export and
  gen_server map); dispatch precedence (map shadows compiled) must be documented
  and tested for hot-reload ordering.
- Dispatch adds a `class_methods`-map lookup before the compiled apply. It must
  read from state already resolved during the class-chain walk (`DefiningModule`,
  `ClassVars`) — **not** introduce an extra `gen_server` hop per class-method
  send (cf. BT-2008, which removed such hops). If a hop is unavoidable, the
  lookup must be gated on the class actually having runtime class methods.
- `update_class/2` (recompile / `reload`) rebuilds the `class_methods` map from
  the incoming `ClassInfo`, which would **drop** runtime-installed class methods
  unless they are merged. This is the same memory-vs-disk reconciliation ADR 0082
  governs (runtime methods are memory-only; disk wins on reload). The
  merge-vs-replace behaviour on reload must be decided and tested, consistent
  with the instance-side `put_method` reload semantics.

### Neutral
- Cross-class class-side extensions remain unimplemented (future, ADR 0066
  registry). This ADR does not address them.
- Memory-only until flushed (ADR 0082); restart reloads from disk.
- `apply_class_method_in_context` already special-cases `runAll` / `run:` on
  `TestCase` subclasses (`test_spawn`, `beamtalk_class_dispatch.erl:461`). A
  runtime class method named with a test-execution selector on a `TestCase`
  subclass is shadowed by that guard. This is pre-existing behaviour; the fun
  path inherits it and should be tested, not silently changed.

## Implementation

| Layer | Change |
|-------|--------|
| `beamtalk_object_class.erl` | Add `put_class_method/4` (mirror `put_method/4`); store `#{block, arity}` in the `class_methods` map (source of truth); clear stale class-side signature/return-type; update `beamtalk_class_metadata` discoverability for the new selector (init/`apply_class_info` already do this for register-time methods, `:419,1078`); and write the fun into the retrieval store. |
| `beamtalk_class_metadata` | Add a retrieval store keyed by `{DefiningClass, Selector} -> #{block, arity}` plus `lookup_class_method_fun/2` and a per-class "has runtime class methods" flag for gating. Populated by register-time builder methods and `put_class_method/4`; invalidated on `update_class` / remove. |
| `beamtalk_class_builder.erl` | Run `classMethods:` through `build_method_map/1` in `build_compiled_class_info/8`, and seed the retrieval store + metadata discoverability for the funs (register-time). |
| `beamtalk_class_dispatch.erl` | In `apply_class_method_in_context/6`, look up a `#{block, arity}` entry and `apply(Fun, [ClassSelf, ClassVars | Args])` before the compiled `erlang:apply` fallback. Gate the lookup so compile-time-only classes pay no extra cost and no per-send `gen_server` hop is added (BT-2008). |
| `crates/beamtalk-core/src/codegen/` | Lower class-method block bodies (builder `classMethods:`, `ClassName class >> sel`) to **self-contained** funs: class-var threading via `{class_var_result, …}`, `ClassSelf`-based self-sends, and `super` lowered to compile-time class-name-keyed superclass dispatch (no reliance on the defining module — see Decision §4). |
| `stdlib/src/ClassBuilder.bt` | `classMethods:` (and `classState:`) state field + setter (parity feeds the runtime key already read by `register/1`). |
| tests | `stdlib/test/` + runtime EUnit — the three §4 contract tests (class-var threading, `super`, self) for the fun path; subclass *inheritance* of a runtime-installed class method; `update_class`/reload precedence; live `class >>` patch round-trip. |

Phasing: (1) runtime fun-path + `put_class_method` (incl. metadata update) +
builder wrapping — delivers callable builder class methods, the bulk of
BT-2259's value; (2) compiler lowering for `classMethods:` block literals with
the §4 contract + parity tests; (3) class-side `>>` / `compile:source:` wiring.
**Phase 3 is gated on ADR 0082 being accepted** (it owns the `compile:source:` /
ChangeLog model); if 0082 stalls, Phase 3 ships as its own follow-on rather than
blocking Phases 1–2. Each phase independently testable.

## Migration Path

None — additive. Existing compiled class methods dispatch exactly as before
(the runtime fun-path is checked first but is empty for file-defined classes
that supply no `classMethods:` block funs). No source, codegen output, or
on-disk format changes for current classes. The only behavioural change is that
the previously-inert `classMethods:` builder key and the parsed-but-unwired
`ClassName class >> sel` syntax start working.

## References
- Related issues: BT-2259 (epic), BT-873 (Path 2 removal), BT-2195 (class-side
  method source), BT-2246 (builder `methodSource:`), BT-2258 (register return)
- Related ADRs: ADR 0038 (ClassBuilder protocol), ADR 0066 (`>>` extension
  methods), ADR 0082 (method-level edit/save), ADR 0013 (class variables / class
  methods), ADR 0036 (metaclass tower), ADR 0006 (unified dispatch)
- Principle: Principle 11 — Live Patching is a Message Send
  (`docs/beamtalk-principles.md`)
