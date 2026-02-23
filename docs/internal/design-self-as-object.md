# Design: Self-as-Object and Reflection API

**Issue:** BT-151  
**Status:** Draft  
**Author:** Design Review  
**Date:** 2026-02-01

## Executive Summary

This document specifies how Beamtalk methods receive a proper "self" reference (completing the object model started in BT-100) and defines the reflection API for runtime introspection.

**Key Decisions:**
1. Methods receive `Self` as a separate `#beamtalk_object{}` record parameter
2. State is kept separate from Self (no infinite recursion risk)
3. Primitives use virtual dispatch via `beamtalk_primitive` module (not actors). Primitive method declarations live in compiled `stdlib/src/*.bt` files via pragmas (see [ADR 0007](../ADR/0007-compilable-stdlib-with-primitive-injection.md)).
4. Phased rollout with backward compatibility layer

---

## Part 1: Current State Analysis

### What BT-100 Implemented

BT-100 introduced the `#beamtalk_object{}` record for **external** object references:

```erlang
%% From runtime/include/beamtalk.hrl
-record(beamtalk_object, {
    class :: atom(),      % 'Counter'
    class_mod :: atom(),  % 'counter'
    pid :: pid()
}).
```

**What works now:**
- `spawn/0` and `spawn/1` return `#beamtalk_object{}` records
- Message sends extract pid via `element(4, Obj)`
- External code can pass object references around

**What's missing:**
- Methods receive only `(Args, State)` — no proper `self` reference
- No reflection API (`obj class`, `obj respondsTo:`, etc.)
- Primitives (integers, strings) have no class identity

### Current Method Signature

Generated code in `dispatch/3`:

```erlang
'dispatch'/3 = fun (Selector, Args, State) ->
    case Selector of
        <'increment'> when 'true' ->
            %% Method body has access to State (a map) but not Self
            NewValue = maps:get('value', State) + 1,
            {'reply', NewValue, maps:put('value', NewValue, State)}
        ...
```

The method has:
- ✅ Access to `State` (the field values map)
- ✅ Access to `Args` (method arguments)
- ❌ No access to `Self` (the `#beamtalk_object{}` reference)
- ❌ No way to get own class, pid, or call reflection methods on self

---

## Part 2: Design Decisions

### Question 1: Method Signatures

**Decision: Pass Self as separate parameter, keep State separate**

```erlang
%% NEW signature
'dispatch'/4 = fun (Selector, Args, Self, State) ->
    %% Self = #beamtalk_object{class='Counter', class_mod='counter', pid=<0.123.0>}
    %% State = #{value => 0, '__class__' => 'Counter', '__methods__' => #{...}}
```

**Rationale:**

1. **Follows LFE Flavors pattern**: Flavors passes `self` (the instance record) as second argument to `combined-method`:
   ```erlang
   Fm:'combined-method'(Meth, Inst, Args)
   %% Inst is #'flavor-instance'{flavor_mod=Fm, instance=Pid}
   ```

2. **Avoids circular reference**: If we merged Self into State, we'd have:
   ```erlang
   State = #{
       '__self__' => #beamtalk_object{..., state = State}  % INFINITE!
   }
   ```
   
3. **Clear separation of concerns**:
   - `Self`: Identity and metadata (immutable during method execution)
   - `State`: Mutable field values

4. **Efficient**: Self is constructed once at dispatch, reused for all method calls within a single message handling.

**Code Generation Changes:**

```erlang
%% BEFORE (current)
'dispatch'/3 = fun (Selector, Args, State) ->

%% AFTER
'dispatch'/4 = fun (Selector, Args, Self, State) ->
```

**Performance Impact:** One additional parameter pass per method call. Negligible — Erlang is optimized for passing terms.

---

### Question 2: Primitive Types

**Decision: Virtual dispatch via `beamtalk_primitive` module, not actors**

Primitives (integers, floats, strings, atoms, lists, maps, booleans, nil) should NOT be actors:
- Creating a process for `42` would be absurdly expensive
- Would break BEAM interop (Erlang expects bare integers)
- Would prevent pattern matching

**Implementation:**

```erlang
%% beamtalk_primitive.erl - handles reflection on primitive types
-module(beamtalk_primitive).
-export([class_of/1, responds_to/2, send/3]).

%% Get the class of any Beamtalk value
class_of(X) when is_integer(X) -> 'Integer';
class_of(X) when is_float(X) -> 'Float';
class_of(X) when is_binary(X) -> 'String';
class_of(X) when is_atom(X), X =:= true; X =:= false -> 'Boolean';
class_of(nil) -> 'UndefinedObject';
class_of(X) when is_atom(X) -> 'Symbol';
class_of(X) when is_list(X) -> 'Array';
class_of(X) when is_map(X) -> 'Dictionary';
class_of(#beamtalk_object{class = C}) -> C;
class_of(_) -> 'Object'.  % Fallback

%% Check if a value responds to a selector
responds_to(X, Selector) when is_integer(X) ->
    beamtalk_integer:has_method(Selector);
responds_to(#beamtalk_object{class_mod = Mod}, Selector) ->
    Mod:has_method(Selector);
%% ... etc for other types

%% Universal message send (for primitives)
send(X, Selector, Args) when is_integer(X) ->
    beamtalk_integer:dispatch(Selector, Args, X);
%% ... etc
```

**Beamtalk Usage:**

```
42 class                    // => Integer
42 + 8                      // => 50 (handled by Integer class)
42 respondsTo: #factorial   // => true (if defined)

"hello" class               // => String
"hello" size                // => 5
```

**Class Hierarchy for Primitives:**

```
Object
├── Integer (wraps Erlang integer)
├── Float (wraps Erlang float)
├── String (wraps Erlang binary)
├── Symbol (wraps Erlang atom)
├── Boolean (wraps true/false atoms)
├── UndefinedObject (wraps nil atom)
├── Array (wraps Erlang list)
├── Dictionary (wraps Erlang map)
├── Block (wraps Erlang fun)
└── Actor (base for all actor classes)
    ├── Counter
    └── ...
```

**Key Insight:** Primitives are *value types* with *class identity*. They're not actors, but they respond to messages through static dispatch to their class module.

**Dispatch Strategy Decision:** Start with uniform dispatch (all primitive operations go through `beamtalk_primitive:send/3`). This enables tracing, debugging, and a consistent object model. Optimize to direct Erlang calls for operators (Option C) later when performance data justifies it — this is purely a codegen optimization with no semantic change.

**`doesNotUnderstand:` on Primitives:** Primitives support DNU handlers, enabling runtime extension of primitive types without modifying core modules. When a method isn't found in `beamtalk_string:dispatch`, it falls through to a DNU handler that checks an extension registry. This allows libraries to add methods like `"hello" json` without recompiling `beamtalk_string.erl`.

**Extension Registry Design (Pharo-style):**

Extensions use a global namespace with conflict tracking (following Pharo's proven approach):

```erlang
%% Registration tracks owner for provenance
beamtalk_extensions:register('String', 'json', Fun, mylib)

%% Conflict handling: warn and overwrite (last-writer-wins)
beamtalk_extensions:register('String', 'json', OtherFun, otherlib)
%% => Warning: 'json' on 'String' (from 'mylib') overwritten by 'otherlib'

%% Tooling: list all extensions on a class
beamtalk_extensions:list('String')
%% => [{json, otherlib}, {trim, stdlib}, ...]

%% Tooling: show all conflicts (methods registered by multiple owners)
beamtalk_extensions:conflicts()
%% => [{'String', json, [mylib, otherlib]}]

%% Lookup (returns current registration)
beamtalk_extensions:lookup('String', 'json')
%% => {ok, Fun, otherlib} | not_found
```

This approach is simple (no import declarations needed), but conflicts are visible and manageable through tooling.

---

### Question 3: Reflection API

**Decision: Implement core Smalltalk-80 reflection methods in phases**

#### Phase 1: Basic Identity (BT-152)

```
obj class                   // => Counter (class object/symbol)
obj respondsTo: #selector   // => true/false
```

**Implementation:**
- `class` is a special selector handled in `beamtalk_actor:handle_call/3`
- Returns `Self#beamtalk_object.class` for actors
- Returns result from `beamtalk_primitive:class_of/1` for primitives

```erlang
%% In beamtalk_actor.erl
handle_call({class}, _From, State) ->
    Class = maps:get('__class__', State),
    {reply, Class, State};

handle_call({respondsTo, Selector}, _From, State) ->
    Methods = maps:get('__methods__', State),
    {reply, maps:is_key(Selector, Methods), State};
```

#### Phase 2: Field Access (BT-153)

```bt
obj fieldNames              // => [value, name, ...]
obj fieldAt: #value         // => current value
obj fieldAt: #value put: 42 // => sets value, returns self
```

**Implementation:**
```erlang
handle_call({fieldNames}, _From, State) ->
    %% Filter out internal keys
    Names = [K || K <- maps:keys(State),
             not lists:member(K, ['__class__', '__methods__'])],
    {reply, Names, State};

handle_call({fieldAt, Name}, _From, State) ->
    {reply, maps:get(Name, State, nil), State};

handle_call({fieldAt, Name, put, Value}, _From, State) ->
    Self = build_self_from_state(State),  % Reconstruct #beamtalk_object{}
    {reply, Self, maps:put(Name, Value, State)};
```

#### Phase 3: Dynamic Message Send (BT-154)

```
obj perform: #increment                    // => sends increment message
obj perform: #'at:put:' withArgs: [1, 'x']   // => sends at:put: with args
```

**Implementation:**
```erlang
handle_call({perform, Selector}, From, State) ->
    %% Recursive dispatch
    handle_call({Selector, []}, From, State);

handle_call({perform, Selector, withArgs, Args}, From, State) ->
    handle_call({Selector, Args}, From, State);
```

#### Phase 4: Method Introspection (BT-155)

```
Counter methods              // => [increment, getValue, spawn, ...]
Counter >> #increment        // => CompiledMethod object
method selector              // => #increment
method argumentCount         // => 0
```

**Implementation Strategy:** Start with module function calls (`counter:method_table()`), evolve to class-as-process later.

- **Phase 4a (this design):** `Counter methods` compiles to `counter:method_table()` — simple, no extra processes
- **Phase 4b (future BT-xxx):** Full class-as-process for metaclass protocol (`Counter superclass`, runtime class modification, `Counter new` as message send)

This mirrors the primitive dispatch decision — start simple, optimize/expand when needed.

#### Reflection Method Summary

| Method | Returns | Phase |
|--------|---------|-------|
| `class` | Class symbol | 1 |
| `respondsTo:` | Boolean | 1 |
| `fieldNames` | Array of symbols | 2 |
| `fieldAt:` | Field value | 2 |
| `fieldAt:put:` | Self | 2 |
| `perform:` | Result | 3 |
| `perform:withArgs:` | Result | 3 |
| `methods` | Array of selectors | 4 |
| `>>` (method lookup) | CompiledMethod | 4 |

---

### Question 4: Self-Reference Semantics

#### Can `self` be reassigned?

**Decision: No. `self` is immutable within a method.**

Smalltalk-80 technically allows `self := other`, but:
1. It's widely considered bad practice
2. BEAM has no mechanism to change "which process am I"
3. It would break the actor model

```
// DISALLOWED
increment =>
    self := Counter spawn   // Compile error: cannot assign to self
```

#### How does `super` work?

**Decision: Static dispatch to superclass module (compile-time resolved)**

```
Counter subclass: SpecialCounter
  increment =>
    self log: "incrementing"
    super increment    // Call Counter's increment
```

**Compiles to:**
```erlang
%% super increment → direct call to superclass module
call 'counter':'dispatch'('increment', [], Self, State)
```

**Rules:**
- `super message` → call superclass module's dispatch directly
- `super.field` → compile error (super is not an object reference)
- `super` alone → compile error (must be followed by message)
- Superclass is resolved at compile time from class definition

**Codegen changes:**
```rust
// In generate_message_send, check if receiver is "super"
if receiver_is_super {
    let superclass_mod = self.current_class_superclass_module();
    write!(self.output, 
        "call '{superclass_mod}':'dispatch'('{selector}', Args, Self, State)")?;
}
```

**Future (with class-as-process):** When we implement runtime method dictionaries, `super` will use dynamic lookup starting from the superclass. The static approach is forward-compatible — we're just optimizing what would be a runtime lookup.

#### Class methods vs instance methods

**Decision: Class methods are module functions (no actor involved)**

```
// Class "methods" - compiled to module functions
Counter spawn           // → counter:spawn()
Counter defaultValue    // → counter:default_value()

// Instance methods - dispatched through actor
counter increment       // → gen_server:cast(pid, {increment, ...})
```

**Limitations (to be addressed with class-as-process):**

| Limitation | Current Behavior | Future Fix |
|------------|------------------|------------|
| `self` in class method | **Compile error** | Class-as-process provides self = class object |
| Class method inheritance | No automatic inheritance | Class-as-process with method dictionaries |
| Passing class as value | Use atom + lookup | Class-as-process makes classes first-class |
| `Counter respondsTo: #spawn` | Returns false (instance methods only) | Class-as-process reflection |

**Codegen for class methods:**
```rust
// If method is marked as class method:
// - Error if `self` is used in body
// - Generate as module function, not in dispatch/4
```

**Workaround for passing classes:**
```
// Can't do: factory := Counter
// Instead:
factory := #Counter
instance := (Beamtalk classNamed: factory) spawn
```

#### Initialization and Self

**Question:** How does the `initialize` method receive `Self`?

**Problem:** `Self` needs `pid`, but pid isn't known until the process starts.

**Solution:** Call `initialize` after process starts, when pid is available:

```erlang
%% Generated init/1 in counter.core
init(Args) ->
    %% 1. Create initial state with defaults
    State0 = #{
        '__class__' => 'Counter',
        '__class_mod__' => 'counter',
        '__methods__' => method_table(),
        value => 0
    },
    
    %% 2. Now pid is known - construct Self
    Self = #beamtalk_object{
        class = 'Counter',
        class_mod = 'counter', 
        pid = self()
    },
    
    %% 3. Call initialize if defined, passing spawn args
    State1 = case maps:find('initialize', maps:get('__methods__', State0)) of
        {ok, InitFun} -> 
            {reply, _, NewState} = InitFun(Args, Self, State0),
            NewState;
        error -> 
            State0
    end,
    
    {ok, State1}.
```

**Beamtalk usage:**
```
Counter subclass: ConfigurableCounter
  state: value = 0, name = ""
  
  initialize: config =>
    self.value := config at: #initial ifAbsent: [0]
    self.name := config at: #name ifAbsent: ["unnamed"]
    
// Spawn with config
counter := ConfigurableCounter spawn: #{initial => 10, name => "myCounter"}
```

**Key points:**
- `initialize` is called *inside* `init/1`, after process starts
- `Self` is fully constructed (pid = self())
- `initialize` receives spawn arguments
- `initialize` can set up state using `self.field :=`
- If no `initialize` defined, defaults are used

#### How does `self` interact with closures/blocks?

**Decision: Blocks capture `self` by value at block creation time.**

```
Counter subclass: Example
  state: value = 0
  
  makeIncrementer =>
    // Block captures self at creation time
    [self increment]
    
  useLater =>
    callback := self makeIncrementer await
    // Later, even if called from another context:
    callback value  // Still calls increment on original self
```

**Implementation:** When generating a block, if `self` is referenced, capture `Self` variable:

```erlang
%% Block captures Self in its closure environment
fun() ->
    %% Self is bound from outer scope
    gen_server:cast(Self#beamtalk_object.pid, {increment, [], FuturePid})
end
```

#### What's the pid in `Self#beamtalk_object.pid`?

**Decision: Always `self()` — the current process.**

```erlang
%% In dispatch, construct Self:
Self = #beamtalk_object{
    class = maps:get('__class__', State),
    class_mod = ?MODULE,
    pid = self()  % Always the current process
}
```

#### Thread-safety: can `Self` be passed to other actors?

**Decision: Yes, `Self` is just data. Passing it is safe.**

```
// Actor A sends its reference to Actor B
actorB tell: self

// Actor B can then send messages to Actor A
// (This is standard actor model behavior)
```

The `#beamtalk_object{}` record is immutable data. Passing it between processes is safe and efficient (BEAM optimizes this).

#### Cascades and Self

**Question:** Does `self foo; bar; baz` work correctly with dispatch/4?

**Answer: Yes.** Cascades reuse the receiver (Self), and state threads through.

```
self increment; increment; getValue
// Sends increment, then increment, then getValue to self
// Returns result of getValue (which is 2)
```

**Generated code:**
```erlang
%% self increment; increment; getValue
%% Self stays constant, State threads through
let <_Cascade0, State1> = dispatch('increment', [], Self, State0) in
let <_Cascade1, State2> = dispatch('increment', [], Self, State1) in
let <Result, State3> = dispatch('getValue', [], Self, State2) in
{'reply', Result, State3}
```

**Key points:**
- `Self` is the same for all messages (object identity)
- `State` threads through (each message sees previous state changes)
- Final result is the return value of the last message

#### Actor Crash Semantics

**Question:** What happens when sending to a dead actor?

```
counter := Counter spawn
other saveRef: counter    // other holds reference
counter crash             // counter process dies

// Later...
other useSavedRef         // Tries to send message to dead actor — what happens?
```

**The problem:**
- `#beamtalk_object{}` record still exists (immutable data)
- `pid` inside points to dead process
- Sync call → `noproc` error
- Async call → message lost, future never resolves

**Solution: Detect and report gracefully**

1. **`isAlive` method:** Check if actor is still running
   ```
   counter isAlive   // => true or false
   ```

2. **Async sends to dead actors:** Future rejects with `actor_dead` error
   ```
   counter increment        // => Future
   counter increment await  // => raises BeamtalkError(actor_dead)
   ```

3. **Error structure:**
   ```erlang
   #beamtalk_error{
       kind = actor_dead,
       class = 'Counter',
       selector = 'increment',
       message = <<"Actor process has terminated">>,
       hint = <<"Use 'isAlive' to check, or use monitors for lifecycle events">>
   }
   ```

**Implementation:**
```erlang
%% In message send codegen or runtime
send_async(#beamtalk_object{pid = Pid, class = Class}, Selector, Args, FuturePid) ->
    case erlang:is_process_alive(Pid) of
        true -> 
            gen_server:cast(Pid, {Selector, Args, FuturePid});
        false ->
            beamtalk_future:reject(FuturePid, #beamtalk_error{
                kind = actor_dead,
                class = Class,
                selector = Selector,
                message = <<"Actor process has terminated">>,
                hint = <<"Use 'isAlive' to check before sending">>
            })
    end.
```

**Monitoring (alternative):** For proactive notification, use BEAM monitors:
```
monitor := counter monitor    // Returns monitor reference

// When counter dies, receive notification:
receive: {#DOWN, monitor, #process, pid, reason} =>
    self handleDeath: reason
```

**⚠️ `isAlive` race condition:** The check-then-act pattern is inherently racy:
```
// DANGEROUS: actor could die between check and send
counter isAlive ifTrue: [counter increment]
```
`isAlive` is a hint, not a guarantee. The robust pattern is to **handle the error**:
```
// SAFE: handle the error
counter increment await 
    onError: [:e | e kind = #actor_dead ifTrue: [self handleDead]]
```

#### Future Await Timeout

**Question:** What if an actor is alive but hung?

```
counter verySlowMethod await  // Blocks forever?
```

**Decision: Configurable timeout with sensible default**

```
// Default timeout (e.g., 30 seconds)
result := counter slowMethod await

// Explicit timeout
result := counter slowMethod await: 5 seconds

// No timeout (explicit infinite wait)
result := counter slowMethod awaitForever
```

**Implementation:**
```erlang
%% await with timeout
await(Future) -> await(Future, 30000).  %% 30 second default

await(Future, Timeout) ->
    receive
        {resolved, Future, Value} -> Value;
        {rejected, Future, Error} -> error(Error)
    after Timeout ->
        error(#beamtalk_error{
            kind = timeout,
            message = <<"Await timed out">>,
            hint = <<"Use 'await: duration' for longer timeout, or 'awaitForever' for no timeout">>
        })
    end.
```

**Note:** Default timeout value (30s) should be configurable at system level.

---

### Question 5: Implementation Strategy

**Decision: Phased rollout with compatibility shim**

#### Phase 1: Runtime Changes (Non-Breaking)

1. Add `dispatch/4` alongside existing `dispatch/3`
2. `handle_call` and `handle_cast` construct `Self` and call `dispatch/4`
3. Old generated code using `dispatch/3` continues to work (shim layer)

```erlang
%% Backward compatibility: dispatch/3 wraps dispatch/4
dispatch(Selector, Args, State) ->
    %% Construct Self from State for legacy code
    Self = #beamtalk_object{
        class = maps:get('__class__', State),
        class_mod = maps:get('__class_mod__', State, undefined),
        pid = self()
    },
    dispatch(Selector, Args, Self, State).
```

#### Phase 2: Code Generation Changes

1. Update `generate_dispatch` in `mod.rs` to emit `dispatch/4`
2. Generate methods that use `Self` parameter
3. Add `__class_mod__` to state map for Self reconstruction

#### Phase 3: Add Reflection Methods

1. Add special-case handling in `dispatch/4` for `class`, `respondsTo:`, etc.
2. Generate `has_method/1` function in each class module

#### Phase 4: Remove Compatibility Layer ✅ DONE (BT-173)

Compatibility layer removed after all generated code transitioned to `dispatch/4`.

---

### Question 6: Actor State Structure

**Decision: State does NOT include Self reference directly**

**Current state structure:**
```erlang
#{
    '__class__' => 'Counter',
    '__methods__' => #{increment => fun/2, ...},
    value => 0
}
```

**New state structure (add `__class_mod__`):**
```erlang
#{
    '__class__' => 'Counter',
    '__class_mod__' => 'counter',  % NEW: needed to reconstruct Self
    '__methods__' => #{increment => fun/3, ...},  % Arity changes!
    value => 0
}
```

**Why store both?** Avoids locking in a naming convention. `HTTPClient` → `httpclient` vs `http_client` is ambiguous. Explicit is safer and consistent with `#beamtalk_object{}` from BT-100.

**Why not store Self in State?**

```erlang
%% DON'T DO THIS - infinite recursion risk
#{
    '__self__' => #beamtalk_object{
        class = 'Counter',
        class_mod = 'counter',
        pid = <0.123.0>
    },
    ...
}
%% What if someone does: maps:get('__self__', State)?
%% And then passes it around? The pid is fine, but it's redundant.
```

**Reconstructing Self at dispatch time:**

```erlang
%% In handle_call/handle_cast:
Self = #beamtalk_object{
    class = maps:get('__class__', State),
    class_mod = maps:get('__class_mod__', State),
    pid = self()
}
```

This is:
- **Safe:** No circular reference
- **Efficient:** Record construction is cheap
- **Correct:** `pid = self()` is always accurate

---

## Part 3: Detailed Specifications

### 3.1 Method Signature Format

**Generated method dispatch (after changes):**

```erlang
'dispatch'/4 = fun (Selector, Args, Self, State) ->
    %% Self = #beamtalk_object{class, class_mod, pid}
    %% State = #{...fields..., '__class__', '__class_mod__', '__methods__'}
    case Selector of
        <'increment'> when 'true' ->
            %% Method body can use Self for reflection, self-sends, etc.
            NewValue = call 'erlang':'+'(
                call 'maps':'get'('value', State),
                1
            ),
            NewState = call 'maps':'put'('value', NewValue, State),
            {'reply', NewValue, NewState};
        
        <'getSelf'> when 'true' ->
            %% Method can return Self
            {'reply', Self, State};
        
        %% ... more methods ...
        
        %% Reflection methods (built-in)
        <'class'> when 'true' ->
            {'reply', call 'erlang':'element'(2, Self), State};
        
        <'respondsTo:'> when 'true' ->
            case Args of
                <[Selector]> when 'true' ->
                    Methods = call 'maps':'get'('__methods__', State),
                    {'reply', call 'maps':'is_key'(Selector, Methods), State}
            end;
        
        %% Default: doesNotUnderstand
        <_> when 'true' ->
            %% ... DNU handling ...
    end
```

### 3.2 Dispatch Implementation

**Changes to `beamtalk_actor.erl`:**

```erlang
%% handle_cast constructs Self before dispatch
handle_cast({Selector, Args, FuturePid}, State) ->
    Self = make_self(State),
    case dispatch(Selector, Args, Self, State) of
        {reply, Result, NewState} ->
            beamtalk_future:resolve(FuturePid, Result),
            {noreply, NewState};
        %% ...
    end.

%% handle_call constructs Self before dispatch
handle_call({Selector, Args}, _From, State) ->
    Self = make_self(State),
    dispatch(Selector, Args, Self, State).

%% Helper to construct Self from State
make_self(State) ->
    #beamtalk_object{
        class = maps:get('__class__', State),
        class_mod = maps:get('__class_mod__', State),
        pid = self()
    }.
```

### 3.3 Primitive Type Handling

**New module: `beamtalk_primitive.erl`**

```erlang
-module(beamtalk_primitive).
-export([class_of/1, send/3, responds_to/2]).

-include("beamtalk.hrl").

%% Determine class of any Beamtalk value
-spec class_of(term()) -> atom().
class_of(X) when is_integer(X) -> 'Integer';
class_of(X) when is_float(X) -> 'Float';
class_of(X) when is_binary(X) -> 'String';
class_of(true) -> 'Boolean';
class_of(false) -> 'Boolean';
class_of(nil) -> 'UndefinedObject';
class_of(X) when is_function(X) -> 'Block';
class_of(X) when is_atom(X) -> 'Symbol';
class_of(X) when is_list(X) -> 'Array';
class_of(X) when is_map(X) -> 'Dictionary';
class_of(X) when is_tuple(X), element(1, X) =:= beamtalk_object ->
    element(2, X);  % class field
class_of(_) -> 'Object'.

%% Send message to any value (actors or primitives)
-spec send(term(), atom(), list()) -> term().
send(#beamtalk_object{pid = Pid}, Selector, Args) ->
    %% Actor: use gen_server
    gen_server:call(Pid, {Selector, Args});
send(X, Selector, Args) when is_integer(X) ->
    %% Primitive: static dispatch to class module
    beamtalk_integer:dispatch(Selector, Args, X);
send(X, Selector, Args) when is_binary(X) ->
    beamtalk_string:dispatch(Selector, Args, X);
%% ... etc for other primitives

%% Check if value responds to selector
-spec responds_to(term(), atom()) -> boolean().
responds_to(#beamtalk_object{class_mod = Mod}, Selector) ->
    Mod:has_method(Selector);
responds_to(X, Selector) when is_integer(X) ->
    beamtalk_integer:has_method(Selector);
%% ... etc
```

**Example primitive class module: `beamtalk_integer.erl`**

```erlang
-module(beamtalk_integer).
-export([dispatch/3, has_method/1]).

dispatch(Selector, Args, Value) ->
    case builtin_dispatch(Selector, Args, Value) of
        {ok, Result} -> Result;
        not_found -> does_not_understand(Selector, Args, Value)
    end.

builtin_dispatch('+', [Y], X) -> {ok, X + Y};
builtin_dispatch('-', [Y], X) -> {ok, X - Y};
builtin_dispatch('*', [Y], X) -> {ok, X * Y};
builtin_dispatch('/', [Y], X) -> {ok, X / Y};
builtin_dispatch('class', [], _X) -> {ok, 'Integer'};
builtin_dispatch('asString', [], X) -> {ok, integer_to_binary(X)};
builtin_dispatch('abs', [], X) -> {ok, abs(X)};
builtin_dispatch('negated', [], X) -> {ok, -X};
builtin_dispatch(_, _, _) -> not_found.

does_not_understand(Selector, Args, Value) ->
    %% Check extension registry for user-added methods
    case beamtalk_extensions:lookup('Integer', Selector) of
        {ok, Fun} -> Fun(Args, Value);
        not_found -> error({does_not_understand, 'Integer', Selector, length(Args)})
    end.

has_method(Selector) ->
    %% Check both builtins and extensions
    builtin_dispatch(Selector, [], 0) =/= not_found
        orelse beamtalk_extensions:has('Integer', Selector).
```

### 3.4 Reflection API Specification

| Method | Signature | Return Type | Works On |
|--------|-----------|-------------|----------|
| `class` | `obj class` | Symbol | All values |
| `respondsTo:` | `obj respondsTo: #sel` | Boolean | All values |
| `fieldNames` | `obj fieldNames` | Array | All values (empty for primitives) |
| `fieldAt:` | `obj fieldAt: #name` | Any | All values (nil for primitives) |
| `fieldAt:put:` | `obj fieldAt: #name put: val` | Self | Actors only (error on primitives) |
| `perform:` | `obj perform: #sel` | Any | All values |
| `perform:withArgs:` | `obj perform: #sel withArgs: args` | Any | All values |

**Primitive reflection behavior:** Primitives return sensible defaults for read-only reflection (`fieldNames` → `[]`, `fieldAt:` → `nil`). Mutating reflection (`fieldAt:put:`) errors on primitives since they're immutable values.

**Error messages for primitives:** When mutation is attempted on a primitive, provide helpful guidance:
```erlang
%% Instead of cryptic error:
{does_not_understand, 'Integer', 'fieldAt:put:', 2}

%% Provide actionable message (structured #beamtalk_error{}):
#beamtalk_error{
    kind = immutable_value,
    class = 'Integer',
    selector = 'fieldAt:put:',
    message = <<"Integers are immutable values. Use assignment (x := newValue) instead of mutation.">>,
    hint = <<"fieldAt:put: is only valid on actor objects">>,
    details = #{}
}
```

**Reflection is synchronous:** All reflection methods (`class`, `respondsTo:`, `fieldNames`, `fieldAt:`, `fieldAt:put:`, `perform:`) are **synchronous** even on actors. This ensures consistent behavior for introspection and debugging. The async-first model applies to user-defined methods, not reflection.

**Sync vs Async Mental Model:**

| Operation | Location | Sync/Async | Why |
|-----------|----------|------------|-----|
| `class` | Local (in `#beamtalk_object{}`) | Sync | Metadata in reference |
| `respondsTo:` | Local (check method table) | Sync | Can query module |
| `fieldNames` | Actor process | **Async (Future)** | Needs actor state |
| `fieldAt:` | Actor process | **Async (Future)** | Needs actor state |
| `fieldAt:put:` | Actor process | **Async (Future)** | Modifies actor state |
| `perform:` | Actor process | **Async (Future)** | Delegates to actor |
| User methods | Actor process | **Async (Future)** | Actor model |

**Rule:** If it needs to ask the actor, it returns a Future. Metadata in the object reference is sync.

**Error messages for Future confusion:** When a developer sends a message to a Future instead of awaiting:
```
Error: Sent 'size' to a Future.
  Did you mean: (counter fieldNames await) size

  fieldNames returns a Future because it queries actor state.
  Use 'await' to get the value.
```

### 3.5 Code Generation Changes

**File:** `crates/beamtalk-core/src/codegen/core_erlang/mod.rs`

1. **`generate_dispatch`**: Change from `/3` to `/4`, add `Self` parameter

2. **`generate_init`**: Add `'__class_mod__'` to initial state map

3. **Add built-in reflection selectors**: Handle `class`, `respondsTo:`, `fieldNames`, etc. in dispatch before user-defined methods

4. **Update method arity**: Methods now receive `(Args, Self, State)` internally

5. **Fix `self` identifier generation** (CRITICAL):

**Current behavior (WRONG):**
```rust
// In generate_identifier()
// `self` falls through and generates: maps:get('self', State)
```

**Required fix:**
```rust
fn generate_identifier(&mut self, id: &Identifier) -> Result<()> {
    match id.name.as_str() {
        "true" => write!(self.output, "'true'")?,
        "false" => write!(self.output, "'false'")?,
        "nil" => write!(self.output, "'nil'")?,
        "self" => write!(self.output, "Self")?,  // NEW: self → Self parameter
        _ => {
            // Check if it's a bound variable in current or outer scopes
            if let Some(var_name) = self.lookup_var(id.name.as_str()).cloned() {
                write!(self.output, "{var_name}")?;
            } else {
                // Field access from state
                let state_var = self.current_state_var();
                write!(self.output, "call 'maps':'get'('{}', {state_var})", id.name)?;
            }
        }
    }
    Ok(())
}
```

**Developer writes → Compiler generates:**

| Beamtalk Source | Meaning | Generated Core Erlang |
|-----------------|---------|----------------------|
| `self` | Object reference | `Self` |
| `self.value` | Field read | `call 'maps':'get'('value', State)` |
| `self.value := x` | Field write | State threading |
| `self increment` | Message to self | `gen_server:cast(element(4, Self), ...)` |
| `^self` | Return self | `Self` |
| `other tell: self` | Pass self | `Self` |

**Key insight:** The developer never sees `Self` vs `State` — they just write `self`. The compiler distinguishes:
- `self.field` → State (field access, handled by `generate_field_access`)
- `self` alone → Self (object reference, handled by `generate_identifier`)

### 3.6 Runtime Changes

**File:** `runtime/src/beamtalk_actor.erl`

1. Add `make_self/1` helper function
2. Modify `handle_call/3` to construct Self and call dispatch/4
3. Modify `handle_cast/2` to construct Self and call dispatch/4
4. Keep `dispatch/3` for backward compatibility (calls dispatch/4)
5. Add new `dispatch/4` export

**New files:**
- `runtime/src/beamtalk_primitive.erl` — Universal dispatch for primitives
- `runtime/src/beamtalk_extensions.erl` — Extension registry for adding methods to primitives
- `runtime/src/beamtalk_integer.erl` — Integer class methods
- `runtime/src/beamtalk_string.erl` — String class methods
- `runtime/src/beamtalk_boolean.erl` — Boolean class methods
- `runtime/src/beamtalk_nil.erl` — UndefinedObject (nil) class methods
- `runtime/src/beamtalk_block.erl` — Block/closure class methods
- (etc. for other primitive classes)

### 3.7 Migration Strategy

**Status: COMPLETED** (as of 2026-02-03)

Migration completed in phases:

1. **Update runtime first** (backward compatible) - ✅ DONE
   - Add `dispatch/4` - ✅ 
   - Add `make_self/1` - ✅
   - Keep `dispatch/3` as wrapper - ✅

2. **Update codegen** (generates new format) - ✅ DONE
   - All new compilations use `dispatch/4` - ✅
   - Add `__class_mod__` to state - ✅

3. **Existing compiled code during transition window** - ✅ DONE
   - During migration, existing compiled code continued to work via `dispatch/3` → `dispatch/4` wrapper - ✅
   - During migration, `__class_mod__` defaulted to deriving from `__class__` - ✅
   - Note: After cleanup in BT-173, any remaining compiled artifacts targeting `dispatch/3` must be recompiled to use `dispatch/4`.

4. **Cleanup** - ✅ DONE (BT-173)
   - Removed `dispatch/3` wrapper
   - After BT-173, all supported code is compiled against and uses `dispatch/4` exclusively

### 3.8 Error Handling Taxonomy

All Beamtalk errors use a consistent structure for tooling and developer experience.

**CRITICAL:** NO bare tuple errors exist in the codebase. All errors MUST use `#beamtalk_error{}` records, even when generated by the compiler.

```erlang
%% runtime/include/beamtalk.hrl

-record(beamtalk_error, {
    kind    :: atom(),              % does_not_understand | immutable_value | type_error | arity_mismatch | instantiation_error | ...
    class   :: atom(),              % 'Integer', 'Counter', 'String', 'Actor'
    selector:: atom() | undefined,  % method that failed
    message :: binary(),            % human-readable explanation
    hint    :: binary() | undefined,% actionable suggestion
    details :: map()                % additional context (arity, expected types, etc.)
}).

%% Separate wrapper for source locations (compile-time errors)
-record(located_error, {
    error :: #beamtalk_error{},
    span  :: span() | undefined     % {file, start_line, start_col, end_line, end_col}
}).
```

**Error kinds:**

| Kind | When | Example |
|------|------|---------|
| `does_not_understand` | Unknown method | `42 foo` |
| `immutable_value` | Mutation on primitive | `42 fieldAt:put:` |
| `arity_mismatch` | Wrong argument count | `counter at:` (missing arg) |
| `type_error` | Wrong argument type | `"hello" + 42` |
| `future_not_awaited` | Message sent to Future | `(counter getValue) size` |
| `instantiation_error` | Wrong instantiation method | `Counter new` (should use `spawn`) |
| `timeout` | Operation timed out | Future await exceeds deadline |

**Creating errors:**

Use `beamtalk_error` module helpers for consistency:

```erlang
%% Runtime Erlang code
Error0 = beamtalk_error:new(does_not_understand, 'Integer'),
Error1 = beamtalk_error:with_selector(Error0, 'foo'),
Error2 = beamtalk_error:with_hint(Error1, <<"Check spelling">>),
error(Error2)
```

**Generated Core Erlang code:**

Codegen MUST use `beamtalk_error` helpers, never bare tuples:

```erlang
%% WRONG - bare tuple (never do this!)
call 'erlang':'error'({'some_error', 'message'})

%% RIGHT - structured error
let Error0 = call 'beamtalk_error':'new'('instantiation_error', 'Actor') in
let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'new') in  
let Error2 = call 'beamtalk_error':'with_hint'(Error1, <<"Use spawn instead">>) in
call 'erlang':'error'(Error2)
```

**Example errors:**

```erlang
#beamtalk_error{
    kind = does_not_understand,
    class = 'Integer',
    selector = 'foo',
    message = <<"Integer does not understand 'foo'">>,
    hint = <<"Check spelling or use 'respondsTo:' to verify method exists">>,
    details = #{arity => 0}
}

#beamtalk_error{
    kind = future_not_awaited,
    class = 'Future',
    selector = 'size',
    message = <<"Sent 'size' to a Future">>,
    hint = <<"Use 'await' to get the value: (expr await) size">>,
    details = #{original_selector => 'fieldNames'}
}

#beamtalk_error{
    kind = instantiation_error,
    class = 'Actor',
    selector = 'new',
    message = <<"Cannot call 'new' on Actor">>,
    hint = <<"Use spawn instead">>,
    details = #{}
}
```

**Design rationale:** Spans are separate because compile-time errors have source locations, runtime errors have stack traces instead.

**User-facing error messages:** Error messages must use developer-facing names, not internal compiler names:

| Internal | User-facing |
|----------|-------------|
| `Self` | `self` |
| `State` | (not shown — refer to fields by name) |
| `#beamtalk_object{}` | "object" or class name |
| `dispatch/4` | (not shown — refer to method name) |

Example:
```
// WRONG (exposes internals)
Error: Self#beamtalk_object.pid is undefined

// RIGHT (user-facing)
Error: 'self' has no associated process (object not spawned?)
```

| Operation | Overhead | Notes |
|-----------|----------|-------|
| Self construction | ~100ns | One record creation per message |
| Extra parameter pass | ~10ns | Erlang is optimized for this |
| Reflection calls | ~1μs | Map lookup + function call |
| Primitive dispatch | ~100ns | Pattern match on type |

**Conclusion:** Overhead is negligible compared to gen_server message passing (~10-100μs).

### 3.9 Test Strategy

#### Unit Tests (runtime/test/)

1. **Self parameter tests**
   - Method receives correct Self
   - Self.pid equals self()
   - Self.class matches state.__class__

2. **Reflection tests**
   - `class` returns correct class
   - `respondsTo:` returns true/false correctly
   - `fieldNames` lists fields
   - `fieldAt:` reads fields
   - `fieldAt:put:` writes fields

3. **Primitive tests**
   - `42 class` returns `'Integer'`
   - `"hello" class` returns `'String'`
   - Primitive methods dispatch correctly

4. **Method signature compatibility tests**
   - Old-style Fun/2 methods still work via dispatch/4
   - New-style Fun/4 methods work correctly
   - Missing `__class_mod__` handled gracefully

#### Snapshot Tests (test-package-compiler/)

1. Generated code uses `dispatch/4`
2. State includes `__class_mod__`
3. Reflection methods are built-in

#### E2E Tests (tests/e2e/cases/)

1. `self-reference.bt` — Self works correctly in methods
2. `reflection.bt` — Reflection API works
3. `primitives.bt` — Primitive class identity

### 3.10 Erlang/Beamtalk Interop

**Beamtalk → Erlang:**

Erlang calls return an `ErlangResult` wrapper (not raw tuples) for type safety:

```erlang
%% runtime/include/beamtalk.hrl
-record(erlang_result, {
    value :: {ok, term()} | {error, term()} | term()
}).
```

```
// Erlang call returns ErlangResult
result := Erlang.gen_tcp connect: {127, 0, 0, 1} port: 8080

// Unwrap methods (only on ErlangResult, not all tuples)
socket := result unwrap                          // => value or raises
socket := result unwrapOr: defaultSocket         // => value or default
socket := result unwrapOrElse: [self makeSocket] // => value or evaluate block

// Query methods
result isOk      // => true if {ok, _}
result isError   // => true if {error, _}
result ok        // => value if ok, nil if error
result error     // => reason if error, nil if ok

// Functional combinators
result mapOk: [:socket | socket configure]    // Transform value if ok
result mapError: [:e | self logError: e]      // Transform error if error
result andThen: [:socket | socket read]       // Chain operations
```

**Note:** Raw Erlang interop (when you need the actual tuple) is available via `Erlang.raw`:
```
tuple := Erlang.raw gen_tcp connect: host port: 8080  // => {ok, Socket} or {error, Reason}
```

**Erlang → Beamtalk:**

Beamtalk objects are `#beamtalk_object{}` records. Use the helper module:

```erlang
%% Preferred: beamtalk helper module
Obj = counter:spawn(),
beamtalk:send(Obj, increment, []),              %% Sync call
Future = beamtalk:send_async(Obj, increment, []), %% Async, returns future
Result = beamtalk:await(Future),                %% Wait for future

%% Low-level: extract pid and use gen_server directly
Pid = Obj#beamtalk_object.pid,
gen_server:call(Pid, {increment, []})
```

**Primitive mapping:**

| Erlang | Beamtalk Class |
|--------|----------------|
| integer | `Integer` |
| float | `Float` |
| binary | `String` |
| atom | `Symbol` (or `Boolean` for true/false) |
| list | `Array` |
| map | `Dictionary` |
| tuple | `Tuple` |
| fun | `Block` |
| pid | `Pid` |
| port | `Port` |
| reference | `Reference` |
| `#beamtalk_object{}` | User class (`Counter`, etc.) |

---

## Part 4: Implementation Breakdown

### Proposed PR Sequence

| PR | Description | Blocks | Estimate |
|----|-------------|--------|----------|
| **BT-152** | Runtime: Add dispatch/4 and make_self | — | S |
| **BT-156** | Runtime: Add beamtalk_primitive module | — | M |
| **BT-157** | Codegen: Generate dispatch/4 | BT-152 | M |
| **BT-158** | Codegen: Add __class_mod__ to state | BT-157 | S |
| **BT-159** | Runtime: Reflection methods (class, respondsTo:) | BT-152 | S |
| **BT-160** | Runtime: field reflection methods | BT-159 | S |
| **BT-161** | Runtime: perform: dynamic send | BT-159 | S |
| **BT-162** | Stdlib: Integer class methods | BT-156 | M |
| **BT-163** | Stdlib: String class methods | BT-156 | M |
| **BT-164** | E2E tests for reflection | BT-159 | S |
| **BT-165** | Remove dispatch/3 compatibility | BT-164 | S |

### Dependency Graph

```
BT-152 (dispatch/4) ──┬──> BT-157 (codegen dispatch/4) ──> BT-158 (__class_mod__)
                      │
                      └──> BT-159 (class, respondsTo:) ──┬──> BT-160 (field)
                                                         │
                                                         └──> BT-161 (perform:)
                                                         │
                                                         └──> BT-164 (E2E tests) ──> BT-165 (cleanup)

BT-156 (primitive) ──┬──> BT-162 (Integer)
                     │
                     └──> BT-163 (String)
```

---

## Part 5: Open Questions

### Resolved During Design Review

| Question | Decision | Rationale |
|----------|----------|-----------|
| Primitive dispatch strategy | Uniform dispatch via `beamtalk_primitive:send/3` | Enables tracing, consistent model. Optimize to direct Erlang later. |
| Reflection on primitives | Return sensible defaults (`fieldNames` → `[]`) | "Everything is an object" — uniform protocol |
| `__class_mod__` storage | Explicit — store both class and module | Avoids naming convention lock-in |
| Method introspection | Module call now, class-as-process later | Unblocks reflection without registry complexity |
| DNU on primitives | Yes — route to extension registry | Enables runtime extension of String, Integer, etc. |
| Nil handling | Explicit `beamtalk_nil.erl` module | Consistent with other primitives |
| Async mental model | Local metadata sync, actor state async | Clear rule: "if it asks the actor, it's a Future" |
| Extension conflicts | Pharo-style: global + tracking + tooling | Simple, conflicts visible, no import declarations |
| Error taxonomy | Structured `#beamtalk_error{}` record | Consistent for tooling, spans separate |
| Interop | `ErlangResult` wrapper + `beamtalk:send/3` | Type-safe unwrapping, clean Erlang→Beamtalk API |

### Deferred for Future Design

See **[design-metaprogramming.md](design-metaprogramming.md)** for comprehensive coverage of:

- Class objects as processes (full metaclass protocol)
- Runtime method dictionaries and dynamic dispatch
- Dynamic `super` dispatch
- Class method inheritance and `self` in class methods
- Passing classes as first-class values
- Method objects (CompiledMethod introspection)
- `thisContext` emulation
- `become:` emulation
- Full class/system reflection API

**Also deferred (not metaprogramming):**

- **`isActor` method** — Distinguish actors from primitives at runtime
- **Tooling/IDE story** — Autocomplete, type inference, language server

### Decisions Needed from Human

None identified. This design follows established patterns from:
- LFE Flavors (self-reference pattern)
- Smalltalk-80 (reflection API)
- BT-100 (object record structure)

---

## References

- [BT-100 PR](https://github.com/jamesc/beamtalk/pull/103) — Object record implementation
- [LFE Flavors](https://github.com/rvirding/flavors) — Robert Virding's OOP on BEAM
- [Smalltalk-80 Blue Book](http://stephane.ducasse.free.fr/FreeBooks/BlueBook/Bluebook.pdf) — Chapter 5: Object Memory
- [ADR 0005](../ADR/0005-beam-object-model-pragmatic-hybrid.md) — BEAM Object Model (Appendix B: LFE Flavors lessons)
- [Pharo Reflection](https://books.pharo.org/booklet-ReflectiveCore/) — Modern Smalltalk reflection
