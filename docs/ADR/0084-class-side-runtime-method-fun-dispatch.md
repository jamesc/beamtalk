# ADR 0084: Class-Side Runtime Method Installation and Fun Dispatch

## Status
Proposed (2026-05-24)

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

### 3. Dispatch — check the map first, then fall back to compiled

`beamtalk_class_dispatch:apply_class_method_in_context/6` checks the live
`class_methods` map for a `#{block, arity}` entry before the compiled apply:

```erlang
case find_class_method_block(ClassPid, Selector) of
    {ok, #{block := Fun}} ->
        apply(Fun, [ClassSelf, ClassVars | Args]);     %% runtime fun
    error ->
        FunName = class_method_fun_name(Selector),
        erlang:apply(DefiningModule, FunName, [ClassSelf, ClassVars | Args])  %% compiled (today)
end
```

Same arguments, same `{class_var_result, …}` handling, same error
classification. A runtime override of a compiled selector shadows it (last
writer wins), matching instance-side live-patch semantics.

### 4. Funs are compiler-generated — the BT-873 guard

The runtime funs are produced by **the compiler**, lowering class-method block
bodies (`classMethods:` literals, `ClassName class >> sel`, `compile:source:`)
with the *same* lowering it already applies to named `class_<sel>` functions:

- class-variable writes → `{class_var_result, Result, NewClassVars}`,
- self-sends → explicit dispatch against `ClassSelf`,
- `super` sends → explicit superclass-chain dispatch at the metaclass level.

They are never naive user closures. **This is precisely the property BT-873's
Path 2 lacked** — Path 2 wrapped raw blocks with no lowering, so it dropped
state and broke `super`/self. Reusing the existing class-method lowering means
the runtime fun is byte-for-byte equivalent in behaviour to a compiled class
method; only its *location* (an anonymous fun in the gen_server map vs a named
export in the BEAM module) differs.

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
  (ADR 0195/BT-2195 channel), so `SystemNavigation` class-side scans see them.

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

### Neutral
- Cross-class class-side extensions remain unimplemented (future, ADR 0066
  registry). This ADR does not address them.
- Memory-only until flushed (ADR 0082); restart reloads from disk.

## Implementation

| Layer | Change |
|-------|--------|
| `beamtalk_object_class.erl` | Add `put_class_method/4` (mirror `put_method/4`); store `#{block, arity}` in `class_methods`; clear stale class-side signature/return-type. |
| `beamtalk_class_builder.erl` | Run `classMethods:` through `build_method_map/1` in `build_compiled_class_info/8`. |
| `beamtalk_class_dispatch.erl` | In `apply_class_method_in_context/6`, look up a `#{block, arity}` entry and `apply(Fun, [ClassSelf, ClassVars | Args])` before the compiled `erlang:apply` fallback. |
| `crates/beamtalk-core/src/codegen/` | Lower class-method block bodies (builder `classMethods:`, `ClassName class >> sel`) to funs with the compiled class-method convention: class-var threading, `ClassSelf` self-sends, metaclass-chain `super`. |
| `stdlib/src/ClassBuilder.bt` | `classMethods:` state field + setter (parity feeds the runtime key already read by `register/1`). |
| tests | `stdlib/test/` — builder class method callable + class-var mutation persists + `super`/self correct; live `class >>` patch round-trip. |

Phasing: (1) runtime fun-path + `put_class_method` + builder wrapping; (2)
compiler lowering for `classMethods:` block literals; (3) class-side `>>` /
`compile:source:` wiring (ADR 0082). Each phase independently testable.

## References
- Related issues: BT-2259 (epic), BT-873 (Path 2 removal), BT-2195 (class-side
  method source), BT-2246 (builder `methodSource:`), BT-2258 (register return)
- Related ADRs: ADR 0038 (ClassBuilder protocol), ADR 0066 (`>>` extension
  methods), ADR 0082 (method-level edit/save), ADR 0013 (class variables / class
  methods), ADR 0036 (metaclass tower), ADR 0006 (unified dispatch)
- Principle: Principle 11 — Live Patching is a Message Send
  (`docs/beamtalk-principles.md`)
