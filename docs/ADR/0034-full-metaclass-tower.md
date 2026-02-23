# ADR 0034: Full Metaclass Tower

## Status
Proposed (2026-02-23)

## Context

### Current State

ADR 0013 introduced *virtual metaclasses* — a pragmatic 95% solution that gives class objects the appearance of a Smalltalk metaclass with zero extra processes. The trick: the same class gen_server pid is wrapped in `#beamtalk_object{class='Counter class'}` to signal metaclass-side dispatch. The tower terminates at a bare atom:

```beamtalk
Counter class class     // => #Metaclass  (bare atom, not a class object)
```

ADR 0032 introduced `Behaviour` and `Class` as real Beamtalk stdlib classes. The class protocol now lives in Beamtalk, not Erlang. ADR 0032 was explicitly designed so that "a full metaclass tower is an incremental addition rather than a rewrite."

### What Doesn't Work

The sentinel atom `#Metaclass` is invisible to every Beamtalk subsystem:

| Subsystem | Problem |
|---|---|
| Compiler / type checker | `#Metaclass` is an atom literal, not a class object — DNU errors on any message send |
| LSP / completions | No methods to complete on `Counter class class` |
| Reflection system | `Counter class class isMeta` → `does_not_understand` |
| Framework authors | Cannot write code that treats metaclasses uniformly with other classes |
| Parallel hierarchy | `Counter class superclass == Actor class` holds; `Counter class class == Actor class class` is untestable |

### The 5% Gap

ADR 0013 Steelman Analysis identified the remaining gap:

> *"You can't add metaclass-specific instance variables. You can't define methods on individual metaclasses independently. In Pharo, I can do `Point class addInstVarNamed: 'cache'`."*

The gap matters for: SUnit test discovery (iterate metaclasses), code loaders (enumerate all metaclasses), and live debugging tools (distinguish class objects from metaclass objects).

### Why Now

ADR 0032 established the `Behaviour`/`Class` chain and removed the flattened table dependency. The bootstrap ordering is now `ProtoObject → Object → Behaviour → Class → ...`. Adding `Metaclass` as the next class in the chain — with `Class` as its superclass — is the natural extension. The two-phase circularity fix is well-understood in OTP and requires ~50 lines of back-patching.

### Constraints

- **No new process per metaclass**: Metaclasses are represented as `#beamtalk_object{}` structs backed by the same class gen_server as their associated class. One class process handles both class-side and metaclass-side dispatch (the virtual tag trick from ADR 0013 continues).
- **Sealed at v0.1**: `Metaclass` is sealed — users cannot subclass it. Per-metaclass instance variables are not supported in this ADR.
- **`sealed` allows stdlib-internal subclassing**: `Class.bt` is declared `sealed`, preventing user subclassing. However, `sealed` only restricts user code — stdlib-internal classes like `Metaclass` may subclass sealed classes. This is enforced by the compiler checking whether the subclass definition is in stdlib source (`stdlib/src/`). If the compiler does not yet support this distinction, `Class` must be unsealed as a prerequisite.
- **Bootstrap ordering**: `Metaclass` must be registered after `Class` in the bootstrap chain. Self-grounding (`Metaclass class class == Metaclass`) requires correct dispatch routing (see Section 5).
- **Breaking**: `metaclass_test.bt` sentinel assertions (`equals: #Metaclass`) must become class object assertions.

## Decision

Introduce `Metaclass` as a real sealed Beamtalk stdlib class, subclass of `Class`. Each class object's metaclass is a real `#beamtalk_object{}` that responds to the full `Behaviour` protocol plus metaclass-specific methods. The metaclass tower self-grounds: `Metaclass class class == Metaclass`.

### 1. New `Metaclass.bt` Stdlib Class

```beamtalk
// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

sealed Class subclass: Metaclass

  // Identity
  sealed isMeta => true
  sealed isClass => false
  sealed isMetaclass => true

  // thisClass: the class this metaclass describes
  sealed thisClass => @primitive "metaclassThisClass"

  // name: derived from thisClass (e.g., 'Counter class')
  sealed name => (self thisClass name asString) ++ " class"

  // printString: same as name
  sealed printString => self name asString

  // Parallel hierarchy: superclass of Counter class == superclass of Counter's metaclass
  // i.e. Counter class superclass == Actor class (not Actor)
  sealed superclass => @primitive "metaclassSuperclass"

  // Class-side method queries: introspect the described class's class methods
  sealed classMethods => @primitive "metaclassClassMethods"
  sealed localClassMethods => @primitive "metaclassLocalClassMethods"
  sealed classIncludesSelector: selector => @primitive "metaclassIncludesSelector"
```

`Metaclass` subclasses `Class`, so it inherits the full `Behaviour` protocol:
- `allSuperclasses`, `subclasses`, `inheritsFrom:`, `canUnderstand:`, etc.
- `isBehaviour` → `true`
- `isClass` overridden → `false` (metaclasses describe classes; they are not classes themselves)

### 2. REPL Examples

```beamtalk
// Counter's metaclass is a real object
>> Counter class class
=> Metaclass               (a real class object)

// Metaclass protocol
>> Counter class class isMeta
=> true

>> Counter class class isClass
=> false

>> Counter class class name
=> 'Counter class'

>> Counter class class thisClass
=> Counter

>> Counter class class printString
=> 'Counter class'

// Parallel hierarchy holds everywhere
>> Counter class superclass == Actor class
=> true

>> Actor class superclass == Object class
=> true

>> Object class superclass == ProtoObject class
=> true

// Self-grounding: the tower terminates cleanly
>> Metaclass class class == Metaclass
=> true

>> Metaclass class name
=> 'Metaclass class'

// Metaclass protocol: methods returns Metaclass instance methods
>> Counter class class methods
=> [isMeta, isMetaclass, thisClass, name, superclass, methods, ...]    (Metaclass protocol)

// Introspection use case: find all metaclasses
>> Metaclass allSubclasses
=> []     (sealed — no user subclasses)
```

### 3. Error Examples

```beamtalk
// Metaclass is sealed — cannot subclass
>> Metaclass subclass: MyMeta
=> Error: 'Metaclass is sealed and cannot be subclassed'

// Cannot construct a Metaclass directly
>> Metaclass new
=> Error: 'Use x class class to obtain a metaclass'
```

### 4. Parallel Hierarchy Invariant

The full parallel hierarchy holds at every level:

```
Counter class superclass  ==  Actor class         (✓ already held via virtual tags)
Actor class superclass    ==  Object class         (✓)
Object class superclass   ==  ProtoObject class    (✓)
ProtoObject class super   ==  Class               (new — terminal of parallel chain)
Metaclass class class     ==  Metaclass            (new — self-grounding)
```

The terminal: `ProtoObject class superclass` returns `Class` (the instance-side `Class`), which is the root of the metaclass protocol chain. This matches Smalltalk-80 and Pharo exactly.

### 5. Runtime Changes (Erlang)

#### `beamtalk_behaviour_intrinsics.erl` — `classClass/1`

Replace the sentinel atom return with a real `#beamtalk_object{}`:

```erlang
%% Before (ADR 0013 sentinel):
classClass(_Self) ->
    'Metaclass'.

%% After (ADR 0034 real metaclass object):
classClass(Self) ->
    %% Return same pid, but tagged with 'Metaclass' class for dispatch
    Pid = erlang:element(4, Self),
    #beamtalk_object{class = 'Metaclass', class_mod = beamtalk_metaclass_bt, pid = Pid}.
```

The metaclass object wraps the *same* class pid but dispatches through the `'Metaclass'` class chain. No new gen_server process.

**Self-grounding**: When `classClass/1` is called on an object already tagged with `class = 'Metaclass'` (i.e., `Metaclass class class`), the function returns a *new* `#beamtalk_object{}` with `class = 'Metaclass'` and the same pid. Meanwhile, `Metaclass` as a class reference evaluates to `#beamtalk_object{class = 'Metaclass class', pid = MetaclassPid}`. The self-grounding invariant `Metaclass class class == Metaclass` holds because Beamtalk's `==` on `#beamtalk_object{}` compares by **pid identity**, not by struct equality — two references to the same process are equal regardless of their class tags. This matches Smalltalk object identity semantics.

#### New primitives backing `Metaclass.bt`

| Primitive | Erlang backing |
|---|---|
| `metaclassThisClass` | Extract the class object for this metaclass: unwrap `#beamtalk_object{class='Foo class'}` → look up `'Foo'` in registry → return class object |
| `metaclassSuperclass` | `gen_server:call(Pid, superclass)` → returns parent class → call `classClass` on it → returns parent's metaclass |
| `metaclassClassMethods` | `gen_server:call(Pid, class_methods)` → returns class-side method selectors |
| `metaclassLocalClassMethods` | `gen_server:call(Pid, local_class_methods)` |
| `metaclassIncludesSelector` | Check class-side method dict for selector |

All in `beamtalk_behaviour_intrinsics.erl`, following the thin data-access pattern established in ADR 0032.

#### `beamtalk_class_dispatch.erl` — `try_class_chain_fallthrough/3`

The existing fallthrough dispatches all class objects through `'Class'`. After this ADR, metaclass objects (tagged `'Metaclass'`) must dispatch through `'Metaclass'` instead. The distinction:

- **Class objects** have tags like `'Counter class'` (suffix `" class"` via `class_object_tag/1`) — dispatch through `'Class'` chain (existing behavior)
- **Metaclass objects** have tag `'Metaclass'` (not a `" class"` suffix tag) — dispatch through `'Metaclass'` chain

Since metaclass objects are NOT detected by `is_class_name/1` (their tag is `'Metaclass'`, not `'Metaclass class'`), they will not enter `class_send` at all. Instead, they dispatch through `beamtalk_primitive:send/3` → `gen_server:call(Pid, {Selector, Args})`. The gen_server (the class process) receives the message and dispatches it using its module's `dispatch/4` function, which for metaclass objects should route through the `Metaclass` class chain.

The key change is in how the class gen_server handles messages for metaclass-tagged senders. This requires a **new message format** for metaclass dispatch: `{metaclass_method_call, Selector, Args}`, analogous to the existing `{class_method_call, Selector, Args}` format. The caller (`beamtalk_dispatch` or `beamtalk_primitive:send`) must detect the `'Metaclass'` tag and use this message format.

```erlang
%% In beamtalk_primitive:send or beamtalk_dispatch:
%% When Self has class = 'Metaclass', route through metaclass dispatch
send(#beamtalk_object{class = 'Metaclass', pid = Pid} = Self, Selector, Args) ->
    case gen_server:call(Pid, {metaclass_method_call, Selector, Args}) of
        {ok, Result} -> Result;
        {error, not_found} ->
            %% Fall through to Metaclass chain (Metaclass → Class → Behaviour → Object)
            case beamtalk_dispatch:lookup(Selector, Args, Self, #{}, 'Metaclass') of
                {reply, Result, _} -> Result;
                {error, #beamtalk_error{kind = does_not_understand}} ->
                    beamtalk_error:raise(beamtalk_error:new(does_not_understand, 'Metaclass', Selector))
            end
    end.
```

Implementation detail: the `{metaclass_method_call, Selector, Args}` handler in `beamtalk_object_class.erl` returns `{error, not_found}` for the bootstrap stub, since metaclass methods are defined in `Metaclass.bt`, not as user-defined class methods. The fallthrough to `'Metaclass'` chain handles all protocol methods.

#### `beamtalk_primitive.erl` — `class_of_object/1`

Three changes required:

1. **Remove the bare atom clause**: `class_of_object('Metaclass') -> 'Metaclass'` (line 61) — this handled the old sentinel atom. Remove it.

2. **Update the `#beamtalk_object{}` clause** to return a real metaclass object:

```erlang
%% Before: class of a class → sentinel atom
class_of_object(#beamtalk_object{class = ClassName}) ->
    case beamtalk_class_registry:is_class_name(ClassName) of
        true -> 'Metaclass';       %% returns bare atom
        false -> class_of_object_inner(ClassName)
    end.

%% After: class of a class → real Metaclass object
class_of_object(#beamtalk_object{class = ClassName, pid = Pid}) ->
    case beamtalk_class_registry:is_class_name(ClassName) of
        true ->
            %% Return the metaclass object (same pid, Metaclass class tag)
            #beamtalk_object{class = 'Metaclass',
                             class_mod = beamtalk_metaclass_bt,
                             pid = Pid};
        false ->
            class_of_object_inner(ClassName)
    end.
```

3. **Remove dead code**: `print_string('Metaclass') -> <<"Metaclass">>` (line 100) — the atom `'Metaclass'` is no longer returned by any code path. Remove this special case.

#### `beamtalk_primitive.erl` — `responds_to/2`

The `responds_to/2` function (line 227) checks `is_class_object(Obj)` and dispatches through `'Class'`. Metaclass objects have tag `'Metaclass'` which does NOT end with `" class"`, so `is_class_object` returns `false`. They fall through to `class_name_from_tag('Metaclass')` → resolves to atom `'Metaclass'` → dispatches through `beamtalk_dispatch:responds_to(Selector, 'Metaclass')`. This path works correctly IF `'Metaclass'` is registered in the class registry with its method chain — which it is (registered during bootstrap). No code change is needed, but this code path must be verified during testing.

#### `beamtalk_metaclass_bt.erl` — Bootstrap stub

Analogous to `beamtalk_class_bt.erl`, provides a minimal dispatch stub until `stdlib/src/Metaclass.bt` is compiled:

```erlang
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
-module(beamtalk_metaclass_bt).

dispatch(Selector, _Args, _Self, State) ->
    Error0 = beamtalk_error:new(does_not_understand, 'Metaclass'),
    Error = beamtalk_error:with_selector(Error0, Selector),
    {error, Error, State}.

has_method(_) -> false.

register_class() ->
    ClassInfo = #{
        name => 'Metaclass',
        superclass => 'Class',
        module => beamtalk_metaclass_bt,
        instance_variables => [],
        class_methods => #{},
        instance_methods => #{
            'isMeta' => #{arity => 0},
            'thisClass' => #{arity => 0},
            'name' => #{arity => 0}
        }
    },
    case beamtalk_object_class:start('Metaclass', ClassInfo) of
        {ok, _Pid} ->
            ?LOG_INFO("Registered Metaclass (ADR 0034 stub)", #{module => ?MODULE}),
            ok;
        {error, {already_started, _}} ->
            beamtalk_object_class:update_class('Metaclass', ClassInfo),
            ok;
        {error, Reason} ->
            ?LOG_WARNING("Failed to register Metaclass", #{reason => Reason}),
            ok
    end.
```

### 6. Self-Grounding Mechanism

The self-grounding invariant (`Metaclass class class == Metaclass`) works automatically through the virtual tag approach — **no two-phase back-patch is required**. Here's the trace:

1. `Metaclass` (class reference) → `#beamtalk_object{class = 'Metaclass class', pid = MetaclassPid}`
2. `Metaclass class` → `classClass/1` returns `#beamtalk_object{class = 'Metaclass', pid = MetaclassPid}` (same pid, different tag)
3. `Metaclass class class` → `classClass/1` is called again, returns `#beamtalk_object{class = 'Metaclass', pid = MetaclassPid}` (same pid again)

`Metaclass class class == Metaclass` evaluates to `true` because Beamtalk's `==` on `#beamtalk_object{}` compares by **pid identity** — both sides reference the same `MetaclassPid`. The different class tags (`'Metaclass'` vs `'Metaclass class'`) do not affect equality.

**No bootstrap circularity**: Unlike Smalltalk-80 where metaclass objects are heap-allocated and require back-patching, Beamtalk's virtual metaclasses are just re-tagged references to the same gen_server. `classClass/1` is a pure function that stamps `class = 'Metaclass'` on any class object — no state to back-patch.

**Post-bootstrap assertion** (validates the invariant is working):

```erlang
%% post_bootstrap_assertions/0 in beamtalk_runtime_app.erl
true = (beamtalk_eval("Metaclass class class == Metaclass") =:= true).
```

**Note**: This assertion depends on `==` using pid-based identity for `#beamtalk_object{}`. If the `==` implementation changes to structural comparison, the self-grounding mechanism must be revisited.

### 7. Bootstrap Order Extension

```
ProtoObject → Object → Behaviour → Class → Metaclass → Actor → user modules
```

`Metaclass` is registered immediately after `Class`. The back-patch in Phase 2 runs after `Actor` and user modules are loaded.

### 8. Breaking Changes

**`stdlib/test/metaclass_test.bt`** — sentinel assertions become class object assertions:

```beamtalk
// Before (sentinel):
self assert: (42 class class) equals: #Metaclass.

// After (real class object):
self assert: (42 class class) equals: Metaclass.
self assert: (42 class class isMeta) equals: true.
self assert: (42 class class isClass) equals: false.
```

**`beamtalk_primitive.erl`** — `class_of_object/1` now returns a `#beamtalk_object{}` instead of the atom `'Metaclass'`. Any Erlang code pattern-matching on the return value of this function must be updated.

## Prior Art

### Smalltalk-80 (Blue Book, Chapter 16)

The original metaclass tower. Every class `Foo` has a unique metaclass `Foo class`. The chain:

```
Point → Point class → Metaclass → Metaclass class → ...
```

Self-grounding: `Metaclass class` is itself a `Metaclass` instance. The tower terminates via this circularity — navigating `class` repeatedly always arrives at `Metaclass class`, which returns itself.

**Parallel hierarchy**: `Foo class superclass == Foo superclass class` holds everywhere, with `ProtoObject class superclass == Class` as the root.

Each metaclass is a unique object. Metaclass instance variables (defined on `Foo class`) give per-class state that is distinct from class variables. This enables advanced framework patterns (per-class caches, per-class policies).

**What we adopt**: The parallel hierarchy invariant, self-grounding via circularity, `Metaclass` as the class of all metaclasses.

**What we simplify**: No per-metaclass instance variables at v0.1 (class variables serve this role). No user-defined metaclasses (sealed). No separate allocation for metaclass objects — same gen_server, different dispatch tag.

### Pharo (Smalltalk)

Pharo follows Smalltalk-80 closely. `Metaclass.class.st` defines:

- `thisClass` — the class described by this metaclass
- `name` — derived as `self thisClass name , ' class'` (Smalltalk uses `,` for concatenation; Beamtalk uses `++`)
- `printOn:` — `aStream nextPutAll: self name`
- `subclassOf:` — delegates to `thisClass superclass class`
- `addInstVarNamed:` — dynamically adds a class instance variable

Pharo allows user-created metaclasses via `Metaclass subclass:`, enabling metaprogramming frameworks. We do not support this at v0.1 (sealed).

**What we adopt**: The `thisClass` back-reference, derived `name`, the `Metaclass` stdlib class structure.

**What we defer**: `addInstVarNamed:` and user-definable metaclasses.

### Newspeak

Newspeak has no metaclasses. Class-side behavior is expressed via nested classes and the module system. Every class is itself an instance of `Object` (no separate metaclass hierarchy). Mirror-based reflection (`Mirror on: anObject`) replaces the Smalltalk metaclass API for introspection.

This is a fundamentally different model — cleaner for module-based programming but incompatible with Smalltalk idioms. Beamtalk's Smalltalk heritage makes the metaclass approach the natural fit.

**What we note**: Per-class state (Newspeak's class slots) maps to our class variables. We do not adopt Newspeak's mirror reflection model.

### Python

`type(type) is type` — Python's metaclass circularity is structurally equivalent: `type` is an instance of itself. Python allows user-defined metaclasses (`class Meta(type): ...`) and uses them for ORM frameworks (SQLAlchemy), interface enforcement (ABCMeta), and attribute interception (dataclasses under the hood).

Python's `__class__` is roughly equivalent to Beamtalk's `class`. The parallel hierarchy concept is not explicitly present — Python uses `type` for all classes, not per-class metaclasses.

**What we adopt**: The self-grounding circular reference pattern. The use case of metaclasses for framework-level interception.

**What we reject**: Python's single-`type` model. Each Beamtalk class has its *own* metaclass identity, giving per-class reflection.

### Erlang/OTP

No metaclass concept. Module attributes are compile-time constants. Class-like behavior is typically implemented via `gen_server` with tagged state.

The two-phase bootstrap pattern (register with placeholder → back-patch circular reference) is standard in OTP for circular process topologies. Erlang's `ets:insert` and `gen_server:cast` enable safe post-start mutation.

**What we adopt**: The two-phase circular reference pattern for bootstrap.

## User Impact

### Newcomer (coming from Python/JS/Ruby)

Before: `Counter class class` returns a strange atom `#Metaclass` that you can't send messages to.

After: `Counter class class` returns a real object. `isMeta`, `name`, `thisClass` all work. The REPL can tab-complete metaclass messages. Errors are informative rather than `does_not_understand` on an atom.

Discoverability: `Counter class class methods` lists available metaclass selectors. `Counter class class class == Counter class class` terminates the tower cleanly with no confusing recursion.

### Smalltalk Developer

This is what they've been waiting for. The parallel hierarchy (`Foo class superclass == Foo superclass class`) is now algebraically correct. `isMeta` distinguishes class objects from metaclass objects. `thisClass` navigates back to the described class. The reflection idioms from Pharo work:

```beamtalk
aClass isMeta
    ifTrue: [ "working with a metaclass" ]
    ifFalse: [ "working with a class" ]
```

The tower is sealed (no user-defined metaclasses), which is a limitation versus Pharo, but acceptable for v0.1. Class variables handle the per-class state use case.

### Erlang/BEAM Developer

No new gen_server processes. The virtual tag trick (same pid, different class field) continues. The bootstrap back-patch is small and follows OTP conventions. The `#beamtalk_object{}` record layout is unchanged — only `class` and `class_mod` fields differ for metaclass objects.

`class_of_object/1` return type changes from `atom()` to `#beamtalk_object{}` — a breaking change in the Erlang-level API that any Erlang code pattern-matching on it must handle. This is a small surface (one function in `beamtalk_primitive.erl`).

### Production Operator

Zero new processes per class means no change to process tree depth or supervision topology. `observer` sees the same number of processes before and after. The metaclass back-patch runs once at startup and is idempotent.

### Tooling Developer (LSP, Debugger)

The LSP can now resolve the type of `Counter class class` to `Metaclass` rather than an untyped atom. Completions, hover, and go-to-definition work uniformly across the metaclass level. `isMeta` can be used in the hover display to distinguish class objects from metaclass objects. The ADR 0024 static-first approach applies: types resolve at compile time because `Counter` is a known class.

## Steelman Analysis

### Option A: Keep the Sentinel (no change)

**Newcomer**: "I don't need metaclasses for 99% of what I do. The sentinel is a fine terminator — it tells me I've reached the bottom. Adding a `Metaclass` class just adds a new thing to learn."

**BEAM veteran**: "The sentinel is an atom. Atoms are cheap, non-collectable, and instantly comparable. A real `#beamtalk_object{}` costs more. The class gen_server is already doing a lot — adding metaclass dispatch is another branch in an already complex dispatch path."

**Language designer**: "ADR 0013 was explicit: the 5% gap covers esoteric metaprogramming, not core use cases. SUnit, singletons, and REPL introspection all work today. The full tower adds complexity for marginal gain. Ship the 95% well."

**Why we still prefer Option B**: The `#Metaclass` atom is invisible to all Beamtalk tooling — you can't call any method on it, the LSP can't complete on it, and it leaks an Erlang implementation detail into user-visible code. Once `Behaviour` and `Class` exist (ADR 0032), the cost of `Metaclass` is a thin stdlib class and a small runtime change — far less than 5% of implementation effort.

### Option C: Lightweight Metaclass (Singleton `Metaclass`)

**BEAM veteran**: "Zero extra processes, zero bootstrap circularity. Replace the atom `#Metaclass` with a singleton `#beamtalk_object{class='Metaclass'}`. Users get `isMeta`, `isClass`, and `isBehaviour` — the things they actually need. No `thisClass`, no per-class name, but those are edge cases."

**Operator**: "No bootstrap circularity to debug. The simplest possible fix to the atom-terminal problem."

**Why we prefer Option B over Option C**: Option C breaks `Counter class class name == 'Counter class'` and `Counter class class thisClass == Counter`. These aren't edge cases — they're the foundation of any reflective framework that needs to know *which* class a metaclass describes. SUnit test discovery iterates class hierarchies; a code loader needs to map metaclass → class. Option C delivers weaker guarantees that would need to be revisited immediately.

### Tension Points

- Newcomers are neutral between B and C (both fix the atom problem).
- Smalltalk developers strongly prefer B (algebraically correct parallel hierarchy).
- BEAM veterans prefer C (simpler bootstrap) but accept B (the circular back-patch is principled OTP).
- Operators prefer C (fewer moving parts) but note that zero new processes makes B acceptable.
- Language designers unanimously prefer B (the invariant `Foo class superclass == Foo superclass class` is fundamental to the Smalltalk object model; compromising it creates subtle bugs in reflection code).

## Alternatives Considered

### Alternative: Keep Sentinel (`#Metaclass` atom)

Described in Steelman Analysis Option A. Rejected because the sentinel is invisible to all Beamtalk tooling and leaks an Erlang implementation detail.

### Alternative: Lightweight Singleton Metaclass

Described in Steelman Analysis Option C. Rejected because it breaks `thisClass` and per-class metaclass identity, which are required for framework-level reflection (SUnit, code loaders, debugger).

### Alternative: Per-Class Metaclass Gen_Server

Give each class its own metaclass gen_server process (like Smalltalk-80's memory-allocated metaclass objects). This would support per-metaclass instance variables (`addInstVarNamed:`), a feature Pharo supports.

**Rejected because**: One more process per class doubles the process count for all user classes. Bootstrap ordering becomes significantly more complex (class process must start its metaclass process before registering). The benefit — per-metaclass instance variables — is served by class variables in nearly all practical cases. Class variables are per-class (not inherited), which is what frameworks actually need.

### Alternative: Pharo-Style `addInstVarNamed:` (Per-Metaclass Variables)

Allow `Counter class addInstVarNamed: 'cache'` to add dynamic per-class state on the metaclass.

**Rejected (deferred)**: Requires either a separate metaclass process (see above) or dynamic state in the class gen_server with distinct namespacing from class variables. This is not needed for any v0.1 use case — class variables serve the purpose. Deferred to a future ADR when concrete use cases emerge.

## Consequences

### Positive

- `Counter class class` returns a real, message-receiving `Metaclass` object. All Beamtalk tooling (LSP, REPL, reflection) works uniformly.
- The parallel hierarchy invariant (`Foo class superclass == Foo superclass class`) is algebraically correct at every level, including `Metaclass class class == Metaclass`.
- `isMeta` cleanly distinguishes class objects from metaclass objects in reflective code.
- `thisClass` enables navigation from metaclass back to the described class — required for framework authors.
- Zero new gen_server processes. The process tree and supervision topology are unchanged.
- Follows the ADR 0032 incremental design: `Metaclass` is a natural extension of the `Behaviour → Class` chain.
- `Metaclass` inherits the full `Behaviour` protocol: `canUnderstand:`, `allSuperclasses`, `subclasses`, etc. work on metaclasses without new code.

### Negative

- **Breaking**: `metaclass_test.bt` sentinel assertions (`equals: #Metaclass`) must become class object assertions. Small migration, but a breaking change.
- **Bootstrap complexity**: Two-phase initialization adds ~50 lines to the bootstrap sequence. The back-patch must run after all classes are registered; incorrect ordering yields `undefined` metaclass pointers.
- **Erlang API break**: `class_of_object/1` in `beamtalk_primitive.erl` now returns `#beamtalk_object{}` instead of `atom()`. Any Erlang code that pattern-matches on this return must be updated.
- **Dispatch path added**: `try_class_chain_fallthrough/3` gains a branch to detect metaclass objects and route through the `Metaclass` chain. Slightly more complex dispatch logic.
- **`Metaclass` is sealed**: No user-defined metaclasses at v0.1. Advanced Pharo-style metaprogramming (per-metaclass traits, `addInstVarNamed:`) is not supported.

### Neutral

- The `#beamtalk_object{}` record layout is unchanged. `class` and `class_mod` fields differ for metaclass objects, but the struct is the same.
- Class variables continue to serve the per-class state use case that per-metaclass instance variables would otherwise provide.
- `Metaclass class name == 'Metaclass class'` — the metaclass of `Metaclass` has a name, consistent with the naming rule.
- Performance: same dispatch path cost as any other class object message. No regression.
- **`==` semantics dependency**: The self-grounding invariant (`Metaclass class class == Metaclass`) relies on `==` for `#beamtalk_object{}` comparing by **pid identity**, not structural equality. `Metaclass class class` produces `#beamtalk_object{class='Metaclass', pid=P}` while `Metaclass` produces `#beamtalk_object{class='Metaclass class', pid=P}` — same pid, different tags. If `==` ever changes to structural comparison, the self-grounding invariant would break.
- **`Class allSubclasses`** now includes `Metaclass` — a minor behavioral change for code enumerating class subclasses.

## Implementation

### Phase 1: Bootstrap Stub and Runtime Wiring

**Files**: `beamtalk_metaclass_bt.erl` (new), `beamtalk_primitive.erl`, `beamtalk_behaviour_intrinsics.erl`, `beamtalk_class_dispatch.erl`, `beamtalk_runtime_app.erl`

- Create `beamtalk_metaclass_bt.erl` following `beamtalk_class_bt.erl` pattern
- Prerequisite: ensure `sealed` on `Class.bt` allows stdlib-internal subclassing, or unseal `Class`
- Update `classClass/1` in `beamtalk_behaviour_intrinsics.erl`: return `#beamtalk_object{class='Metaclass', class_mod=beamtalk_metaclass_bt, pid=ClassPid}` instead of atom `'Metaclass'`
- Update `class_of_object/1` in `beamtalk_primitive.erl`: return metaclass object instead of atom; remove bare atom clause; remove `print_string('Metaclass')` special case
- Add `{metaclass_method_call, ...}` handler in `beamtalk_object_class.erl`
- Add metaclass dispatch routing in `beamtalk_primitive:send/3` for `class = 'Metaclass'` tagged objects
- Add `Metaclass` to bootstrap sequence after `Class`
- Add post-bootstrap assertion for self-grounding invariant
- Verify `responds_to/2` and `beamtalk_method_resolver.erl` work for metaclass-tagged objects

**Test**: `Counter class class isMeta` returns `true`. `Metaclass class class == Metaclass` is `true`. No existing tests break.

### Phase 2: New Primitives

**Files**: `beamtalk_behaviour_intrinsics.erl`

- Add `metaclassThisClass/1` — extract the class this metaclass describes
- Add `metaclassSuperclass/1` — return parent class's metaclass object
- Add `metaclassClassMethods/1` — return class-side selectors from class gen_server
- Add `metaclassLocalClassMethods/1` — local class-side selectors only
- Add `metaclassIncludesSelector/2` — check class-side method dict

All follow the thin data-access pattern from ADR 0032: raw data reads only, no logic.

### Phase 3: `Metaclass.bt` Stdlib Class

**Files**: `stdlib/src/Metaclass.bt` (new), `stdlib/test/metaclass_test.bt`

- Create `stdlib/src/Metaclass.bt` with full protocol (see Decision section)
- Replace `beamtalk_metaclass_bt.erl` stub with compiled `Metaclass.bt`
- Update `metaclass_test.bt`: replace sentinel assertions with class object assertions
- Add new test cases: `isMeta`, `isClass`, `name`, `thisClass`, `superclass` invariant, self-grounding

### Affected Components

| Component | Phase | Change |
|---|---|---|
| `runtime/apps/beamtalk_runtime/src/beamtalk_metaclass_bt.erl` | 1 | New file (bootstrap stub) |
| `runtime/apps/beamtalk_runtime/src/beamtalk_primitive.erl` | 1 | `class_of_object/1` returns object not atom; remove `class_of_object('Metaclass')` clause; remove `print_string('Metaclass')` clause; verify `responds_to/2` code path for metaclass objects |
| `runtime/apps/beamtalk_runtime/src/beamtalk_behaviour_intrinsics.erl` | 1+2 | `classClass/1` change + 5 new primitives |
| `runtime/apps/beamtalk_runtime/src/beamtalk_class_dispatch.erl` | 1 | Metaclass dispatch routing (new `{metaclass_method_call, ...}` message format) |
| `runtime/apps/beamtalk_runtime/src/beamtalk_object_class.erl` | 1 | Handle `{metaclass_method_call, ...}` in `handle_call` |
| `runtime/apps/beamtalk_runtime/src/beamtalk_runtime_app.erl` | 1 | Bootstrap order + post-bootstrap assertion |
| `runtime/apps/beamtalk_runtime/src/beamtalk_method_resolver.erl` | 1 | Verify `is_class_name(ClassTag)` code path for metaclass tags |
| `stdlib/src/Metaclass.bt` | 3 | New file |
| `stdlib/src/Class.bt` | 1 | Prerequisite: ensure `sealed` allows stdlib-internal subclassing (or unseal Class) |
| `stdlib/test/metaclass_test.bt` | 1 | Breaking: sentinel → class object assertions |

## Migration Path

**`stdlib/test/metaclass_test.bt`** (Phase 1):

```beamtalk
// Remove all sentinel assertions:
// self assert: (42 class class) equals: #Metaclass.      ← remove

// Replace with class object assertions:
self assert: (42 class class) equals: Metaclass.
self assert: (42 class class isMeta) equals: true.
self assert: (42 class class isClass) equals: false.
self assert: (42 class class name) equals: 'Integer class'.
self assert: (42 class class thisClass) equals: Integer.
self assert: (Metaclass class class == Metaclass) equals: true.
```

**Erlang code** using `class_of_object/1` return value: Check for `#beamtalk_object{class='Metaclass'}` pattern instead of the atom `'Metaclass'`.

## References

- Related issues: BT-792 (this ADR), BT-234 (Done: original metaclass design research), BT-162 (Epic: Self-as-Object and Reflection API)
- Related ADRs: ADR 0013 (Virtual Metaclasses — foundation), ADR 0032 (Early Class Protocol — stepping stone), ADR 0033 (Runtime-Embedded Documentation)
- Smalltalk-80 Blue Book: Chapter 16 (Metaclasses)
- Pharo by Example: Classes and Metaclasses chapter; `Metaclass.class.st` in Pharo repository
- Newspeak specification: Chapter on class-side behavior (mirror reflection alternative)
