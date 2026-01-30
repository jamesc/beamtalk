# Beamtalk Object Model

How Beamtalk maps Smalltalk's "everything is an object" philosophy to the BEAM virtual machine.

This document analyzes the feasibility of full Smalltalk object reification on BEAM and recommends pragmatic design decisions for Beamtalk.

**Related documents:**
- [Design Principles](beamtalk-principles.md) — Core philosophy (actors, async-first, hot reload)
- [Architecture](beamtalk-architecture.md) — Compiler, runtime, and code generation details
- [BEAM Interop](beamtalk-interop.md) — Erlang/Elixir integration specification
- [Language Features](beamtalk-language-features.md) — Syntax and feature reference

**Important:** Beamtalk is **async-first** (see [Principle 7](beamtalk-principles.md#7-async-first-sync-when-needed)). All message sends return futures by default. This document focuses on object reification; see [Architecture](beamtalk-architecture.md#futurePromise-implementation) for async/future implementation details.

---

## Executive Summary

**Recommendation: Pragmatic Hybrid Approach**

Beamtalk should embrace BEAM's actor model rather than fight it. We reify what we can (classes, methods, blocks, processes) and explicitly document what we cannot (active stack frames, `become:`, global reference scanning).

**Key insight:** BEAM's "everything is a process" aligns well with Smalltalk's "everything is an object" — we just shift the reification from memory-level objects to process-level actors.

**Validation:** This approach is proven by [LFE Flavors](https://github.com/rvirding/flavors), Robert Virding's successful OOP implementation on BEAM. Our design adopts several Flavors patterns (error isolation, object reference records, method combinations) while adding Beamtalk-specific features (async-first, compile-time codegen).

| Smalltalk Feature | BEAM Support | Beamtalk Approach |
|-------------------|--------------|-------------------|
| Classes as objects | ✅ Full | Maps with metaclass protocol |
| Methods as objects | ✅ Full | Wrapped funs with metadata |
| Blocks as closures | ✅ Full | Erlang funs (first-class) |
| Objects with identity | ✅ Full | Processes with pids + `#beamtalk_object{}` record |
| `doesNotUnderstand:` | ✅ Full | Gen_server dispatch fallback |
| Method combinations | ✅ Full | Before/after methods (from Flavors) |
| Error isolation | ✅ Full | Catch at instance, re-raise at caller |
| Stack frames/thisContext | ❌ None | Post-exception stack traces only |
| `become:` (identity swap) | ❌ None | Proxy pattern workaround |
| Global reference scan | ❌ None | Manual tracking via ETS |
| Continuations | ❌ None | Not available on BEAM |

**Trade-off:** We gain massive concurrency, distribution, and fault tolerance. We lose some metaprogramming requiring global heap access and runtime stack manipulation.

---

## Part 1: What Works Naturally

These Smalltalk features map cleanly to BEAM with no semantic compromise.

### 1.1 Objects as Actors (Processes)

Smalltalk's objects have identity, state, and behavior. BEAM processes have exactly the same properties.

**Beamtalk:**
```
Actor subclass: Counter
  state: value = 0
  
  increment => self.value += 1
  getValue => ^self.value
```

**BEAM Mapping:**
```erlang
-module(beamtalk_counter).
-behaviour(gen_server).

%% Each "object" is a process with:
%% - Identity: pid
%% - State: gen_server state map
%% - Behavior: message dispatch

init(_Args) ->
    {ok, #{
        '__class__' => 'Counter',
        '__methods__' => #{
            increment => fun handle_increment/2,
            getValue => fun handle_getValue/2
        },
        value => 0
    }}.
```

**Semantic fidelity:** 100%. Smalltalk objects have identity (object reference); BEAM objects have identity (pid). Both support independent state and message-based interaction.

### 1.2 Classes as Objects

In Smalltalk, classes are objects too — instances of their metaclass. This enables runtime class modification, introspection, and factory patterns.

**Beamtalk class object representation:**
```erlang
%% Class object is a process holding class metadata
#{
    '__class__' => 'Counter class',  % Metaclass
    '__superclass__' => 'Actor',
    '__instance_methods__' => #{
        increment => fun handle_increment/2,
        getValue => fun handle_getValue/2
    },
    '__class_methods__' => #{
        new => fun handle_new/2,
        spawn => fun handle_spawn/2
    },
    '__instance_variables__' => [value],
    '__name__' => 'Counter'
}
```

**Usage:**
```
// Get class from instance
counter class             // => Counter class object

// Class introspection
Counter methods           // => [increment, getValue, ...]
Counter instanceVariables // => [value]
Counter superclass        // => Actor

// Factory method (class as object)
counter := Counter new
counter := Counter spawn  // Actor-style: returns pid
```

**BEAM implementation:**

Each class has a companion process that holds the class metadata. This process is registered with a well-known name (`beamtalk_class_counter`) for lookup.

```erlang
%% Class process
-module(beamtalk_class_counter).

init(_) ->
    {ok, #{
        '__class__' => 'Counter class',
        '__superclass__' => 'Actor',
        %% ... metadata
    }}.

%% Factory method
handle_call({spawn, Args}, _From, State) ->
    {ok, Pid} = beamtalk_counter:start_link(Args),
    {reply, Pid, State}.
```

**Semantic fidelity:** 100%. Classes are processes, just like instances. Full introspection available.

### 1.3 Methods as Objects

Methods can be inspected, replaced, and wrapped at runtime.

**Method object representation:**
```erlang
#{
    '__class__' => 'CompiledMethod',
    '__selector__' => increment,
    '__source__' => <<"increment => self.value += 1">>,
    '__bytecode__' => fun handle_increment/2,
    '__pragmas__' => [],
    '__literals__' => [],
    '__temp_names__' => []
}
```

**Usage:**
```
// Get method object
method := Counter >> #increment

// Introspect
method selector         // => #increment
method source           // => "increment => self.value += 1"
method argumentCount    // => 0

// Replace at runtime (hot patching) - low-level API
Counter >> #increment put: [:self |
  Telemetry log: 'incrementing'
  self.value += 1
]
```

**BEAM implementation:**

Methods are stored in the class object's method dictionary. Hot replacement updates the map entry, and BEAM's code loading makes the change visible to future invocations.

**Note:** The `>>put:` API is the low-level runtime mechanism. For user-facing hot patching with state migration, use the `patch` syntax documented in [Language Features: Live Patching](beamtalk-language-features.md#live-patching). The `patch` syntax compiles down to `>>put:` calls plus `code_change/3` callbacks for state migration.

**Semantic fidelity:** 100%. Methods are first-class, inspectable, replaceable.

### 1.4 Blocks as Closures

Blocks are Smalltalk's lambda expressions — first-class, closures over their environment.

**Beamtalk:**
```
// Block with arguments
square := [:x | x * x]
square value: 5  // => 25

// Block capturing environment
makeAdder := [:n |
  [:x | x + n]
]
addTen := makeAdder value: 10
addTen value: 5  // => 15

// Blocks for control flow
condition
  ifTrue: [self handleYes]
  ifFalse: [self handleNo]
```

**BEAM mapping:**
```erlang
%% Blocks compile to Erlang funs
Square = fun(X) -> X * X end,
apply(Square, [5]).  % => 25

%% Closures work naturally
MakeAdder = fun(N) ->
    fun(X) -> X + N end
end,
AddTen = MakeAdder(10),
AddTen(5).  % => 15
```

**Semantic fidelity:** 100%. Erlang funs are first-class closures. Full closure semantics preserved.

### 1.5 Message Dispatch and `doesNotUnderstand:`

Unknown messages can be intercepted for proxying, method synthesis, and metaprogramming.

**Beamtalk:**
```
Actor subclass: Proxy
  state: target = nil
  
  // Catch-all for unknown messages
  doesNotUnderstand: selector args: args =>
    self.target perform: selector withArgs: args
```

**BEAM implementation:**

Message dispatch integrates with Beamtalk's async-first model. When a message arrives via `handle_cast` (async) or `handle_call` (sync), dispatch routes it to the appropriate handler or `doesNotUnderstand:`.

```erlang
%% Async message dispatch (returns future to caller)
handle_cast({Selector, Args, FuturePid}, State) ->
    case dispatch(Selector, Args, State) of
        {reply, Result, NewState} ->
            FuturePid ! {resolved, Result},
            {noreply, NewState};
        {noreply, NewState} ->
            {noreply, NewState}
    end.

%% Sync message dispatch (blocks caller)
handle_call({Selector, Args}, _From, State) ->
    dispatch(Selector, Args, State).

dispatch(Selector, Args, State) ->
    Methods = maps:get('__methods__', State),
    case maps:find(Selector, Methods) of
        {ok, Fun} ->
            Fun(Args, State);
        error ->
            %% Method not found - try doesNotUnderstand:
            handle_dnu(Selector, Args, State)
    end.

handle_dnu(Selector, Args, State) ->
    case maps:find('doesNotUnderstand:args:', maps:get('__methods__', State)) of
        {ok, DnuFun} ->
            DnuFun([Selector, Args], State);
        error ->
            %% No handler - crash (let supervisor handle)
            error({unknown_message, Selector, maps:get('__class__', State)})
    end.
```

**Note:** `doesNotUnderstand:` follows the same async/sync semantics as regular methods. If the original send was async, the DNU handler's result resolves the future. See [Architecture: Future/Promise Implementation](beamtalk-architecture.md#futurePromise-implementation) for details.

**Semantic fidelity:** 100%. Full `doesNotUnderstand:` semantics available.

### 1.6 Reflection and Introspection

Runtime inspection of objects, classes, and the system.

**Beamtalk reflection API:**
```
// Object inspection
counter class                // => Counter
counter respondsTo: #increment  // => true
counter instVarNames         // => [value]
counter instVarAt: #value    // => 42

// System inspection
Smalltalk allClasses         // => [Counter, Actor, Object, ...]
Smalltalk at: #Counter       // => Counter class object

// Method lookup chain
Counter lookupSelector: #getValue  // => CompiledMethod in Counter
Counter lookupSelector: #spawn     // => CompiledMethod in Actor
```

**BEAM implementation:**

Reflection queries the class metadata processes. Since classes are processes holding maps, all introspection is just message passing to retrieve map values.

```erlang
%% Example: Get all methods
handle_call({methods}, _From, State) ->
    Methods = maps:keys(maps:get('__instance_methods__', State)),
    {reply, Methods, State}.

%% Example: Check if responds to selector
handle_call({responds_to, Selector}, _From, State) ->
    Methods = maps:get('__instance_methods__', State),
    {reply, maps:is_key(Selector, Methods), State}.
```

**Semantic fidelity:** 95%. Most introspection works. Limitations:
- No raw memory slot access (but named field access works)
- No system-wide "all objects" scan (per-process heaps)

---

## Part 2: The Hard Parts

These Smalltalk features conflict with BEAM's architecture. We document each, analyze why it's hard, and propose workarounds.

### 2.1 Stack Frames and `thisContext`

**What Smalltalk provides:**

`thisContext` gives access to the currently executing stack frame as a first-class object. This enables:

- Debugger implementation (inspect/modify stack)
- Non-local returns from blocks
- Exception restart mechanisms
- Continuation capture

```smalltalk
"Smalltalk stack manipulation"
MyClass >> exampleMethod
    | ctx |
    ctx := thisContext.
    ctx sender inspect.  "Look at caller's frame"
    ctx method selector. "Get current method name"
```

**Why BEAM can't do this:**

1. **No reified stack:** BEAM maintains execution state in registers and a non-inspectable call stack. There's no API to get a handle to a stack frame during execution.

2. **Tail call optimization:** BEAM aggressively optimizes tail calls, eliminating stack frames. A "call stack" may not exist in the way Smalltalk expects.

3. **Per-process isolation:** Even if we could inspect stacks, each process has its own — no global stack introspection.

**What we CAN do:**

| Feature | Availability |
|---------|--------------|
| Stack trace after exception | ✅ Available via `erlang:get_stacktrace()` |
| Current function/module | ✅ Via `?FUNCTION_NAME`, `?MODULE` macros |
| Caller information | ⚠️ Limited: only in exceptions |
| Modify running stack | ❌ Impossible |
| Capture continuation | ❌ Impossible |

**Beamtalk approach:**

We provide `ExecutionContext` objects that wrap exception stack traces:

```
// After exception, wrap the stack trace as an object
[self riskyOperation]
  on: Error
  do: [:error |
    | stack |
    stack := error stackTrace.  // List of StackFrame objects
    stack first method.         // => #riskyOperation
    stack first receiver.       // => self
    stack second method.        // => calling method
  ]
```

**Implementation:**
```erlang
%% StackFrame wrapper object
-record(stack_frame, {
    module :: atom(),
    function :: atom(),
    arity :: non_neg_integer(),
    file :: string(),
    line :: pos_integer()
}).

%% Convert erlang:get_stacktrace() to StackFrame objects
wrap_stacktrace(RawStack) ->
    [#stack_frame{
        module = M,
        function = F,
        arity = A,
        file = proplists:get_value(file, Info, "unknown"),
        line = proplists:get_value(line, Info, 0)
    } || {M, F, A, Info} <- RawStack].
```

**Trade-off assessment:**

| Use Case | Smalltalk | Beamtalk |
|----------|-----------|----------|
| Post-mortem debugging | ✅ | ✅ Stack traces available |
| Step debugger | ✅ | ⚠️ Via tracing, not stack manipulation |
| Exception restart | ✅ | ⚠️ Partial via conditions/restarts |
| Continuation capture | ✅ | ❌ Not possible |

### 2.2 `become:` (Object Identity Swapping)

**What Smalltalk provides:**

```smalltalk
"Swap all references"
oldObject become: newObject.
"Now every reference to oldObject points to newObject"
```

This is used for:
- Object migration during schema changes
- Proxy replacement
- Persistence frameworks

**Why BEAM can't do this:**

1. **Immutable pids:** Process IDs are immutable. You cannot redirect all messages sent to pid A to go to pid B instead.

2. **Per-process heaps:** There's no global heap to scan for references. Each process has isolated memory.

3. **No pointer indirection:** BEAM doesn't use pointer indirection that would allow transparent reference swapping.

**Workaround: Proxy Pattern + Registry**

```
// Instead of become:, use explicit delegation
Actor subclass: MigratableCounter
  state:
    delegate = nil
    value = 0
    
  // Forward all messages to delegate if set
  doesNotUnderstand: selector args: args =>
    self.delegate ifNotNil: [:d |
      ^d perform: selector withArgs: args
    ].
    super doesNotUnderstand: selector args: args
    
  // "become:" is explicit migration
  migrateTo: newImplementation =>
    self.delegate := newImplementation
```

**Alternative: Named Registry**

```
// Use registered names instead of raw pids
counter := Registry lookup: #mainCounter
// Later, replace the registered process
Registry register: #mainCounter as: newCounterPid
// Existing code using the name automatically uses new process
```

**Trade-off assessment:**

| Use Case | Smalltalk | Beamtalk |
|----------|-----------|----------|
| Object migration | ✅ Transparent | ⚠️ Explicit delegation |
| Proxy swap | ✅ Transparent | ⚠️ Via registry |
| Schema evolution | ✅ Transparent | ✅ Via gen_server code_change |

**Recommendation:** The registry pattern is more explicit but arguably cleaner. BEAM's hot code loading via `code_change/3` handles the schema evolution case well.

### 2.3 Finding All References (`pointersTo`)

**What Smalltalk provides:**

```smalltalk
"Find all objects referencing this one"
someObject pointersTo.  "=> collection of objects"

"Find all instances of a class"
Counter allInstances.   "=> all Counter objects in the system"
```

**Why BEAM can't do this:**

1. **Per-process heaps:** Each process has its own garbage-collected heap. There's no system-wide object graph.

2. **No object table:** BEAM doesn't maintain a table of all objects.

3. **Process isolation:** You can't enumerate objects in other processes' heaps.

**What we CAN do:**

| Feature | Availability |
|---------|--------------|
| All processes | ✅ `erlang:processes()` |
| Processes of a specific type | ✅ With explicit tracking |
| All instances of a class | ✅ If we track in ETS |
| Reference graph | ❌ Not possible |

**Workaround: Explicit Instance Tracking**

```erlang
%% ETS table for instance tracking
-define(INSTANCE_TABLE, beamtalk_instances).

%% Called when actor starts
register_instance(Class, Pid) ->
    ets:insert(?INSTANCE_TABLE, {{Class, Pid}, true}).

%% Called when actor terminates
unregister_instance(Class, Pid) ->
    ets:delete(?INSTANCE_TABLE, {Class, Pid}).

%% Get all instances
all_instances(Class) ->
    [Pid || {{C, Pid}, _} <- ets:tab2list(?INSTANCE_TABLE), C == Class].
```

**Beamtalk API:**
```
Counter allInstances         // => [pid1, pid2, ...]
Counter instanceCount        // => 42
Counter do: [:c | c inspect] // Iterate all instances
```

**Trade-off assessment:**

| Use Case | Smalltalk | Beamtalk |
|----------|-----------|----------|
| All instances | ✅ Heap scan | ✅ ETS tracking |
| Find referencers | ✅ Heap scan | ❌ Not possible |
| Instance count | ✅ Heap scan | ✅ ETS tracking |

**Recommendation:** Explicit tracking via ETS is actually more efficient than Smalltalk's heap scan for large systems.

### 2.4 Direct Memory Slot Access

**What Smalltalk provides:**

```smalltalk
"Access by position, not name"
point instVarAt: 1.       "=> x value"
point instVarAt: 1 put: 10. "Set first slot"
point basicSize.          "Number of slots"
```

**Why BEAM doesn't do this:**

BEAM values (terms) are tagged and opaque. You can't access "slot 3" of a term — you access named fields of maps/records.

**Beamtalk approach:**

We support named access only, which is actually safer and more maintainable:

```
// Named access (supported)
point at: #x              // => x value
point at: #x put: 10      // Set x

// Reflection
point instVarNames        // => [x, y]
point instVarAt: #x       // Named, not positional
```

**Implementation:**
```erlang
%% Named field access via maps
handle_call({inst_var_at, Name}, _From, State) ->
    case maps:find(Name, State) of
        {ok, Value} -> {reply, Value, State};
        error -> {reply, undefined, State}
    end.

%% Can enumerate field names
handle_call({inst_var_names}, _From, State) ->
    PublicFields = [K || K <- maps:keys(State), not is_special_key(K)],
    {reply, PublicFields, State}.

is_special_key('__class__') -> true;
is_special_key('__methods__') -> true;
is_special_key(_) -> false.
```

**Trade-off assessment:**

Positional slot access is a low-level Smalltalk feature rarely used in application code. Named access is cleaner and sufficient for nearly all use cases.

### 2.5 Weak References

**What Smalltalk provides:**

```smalltalk
weak := WeakArray with: object.
"When object has no strong refs, weak[1] becomes nil"
```

**Why BEAM is different:**

BEAM's garbage collection is per-process, not per-object. An object in your process heap isn't collected until your process terminates or you explicitly remove references.

**BEAM alternative: Process Monitors**

Instead of weak references to objects, use process monitors:

```
// Monitor another actor
monitor := actor monitor.

// When actor terminates, receive notification
receive: {#DOWN, monitor, #process, pid, reason} =>
  self handleTermination: pid reason: reason
```

**Implementation:**
```erlang
%% Monitor returns a reference
Monitor = erlang:monitor(process, ActorPid),

%% Receive DOWN message when process exits
receive
    {'DOWN', Monitor, process, ActorPid, Reason} ->
        handle_termination(Reason)
end.
```

**Weak reference emulation (if needed):**

```erlang
%% WeakRef using process + monitor
-module(beamtalk_weak_ref).

new(Target) when is_pid(Target) ->
    spawn(fun() ->
        Ref = erlang:monitor(process, Target),
        weak_ref_loop(Target, Ref)
    end).

weak_ref_loop(Target, Ref) ->
    receive
        {get, From} ->
            From ! {weak_ref, self(), Target},
            weak_ref_loop(Target, Ref);
        {'DOWN', Ref, process, Target, _} ->
            cleared_loop()
    end.

cleared_loop() ->
    receive
        {get, From} ->
            From ! {weak_ref, self(), undefined},
            cleared_loop()
    end.
```

**Trade-off assessment:**

| Use Case | Smalltalk | Beamtalk |
|----------|-----------|----------|
| Cache eviction | ✅ WeakArray | ✅ ETS with TTL or process monitors |
| Circular reference handling | ✅ Automatic | ✅ Per-process GC handles |
| Observer pattern | ✅ WeakArray | ✅ Process monitors (superior) |

**Recommendation:** Process monitors are actually better than weak references for actor systems because they provide explicit lifecycle notifications.

### 2.6 Image Snapshots

**What Smalltalk provides:**

```smalltalk
Smalltalk snapshot: true andQuit: false.
"Saves entire system state to disk"
```

**Why Beamtalk doesn't need this:**

1. **No image model:** Beamtalk follows BEAM's approach where code lives in files and state lives in running processes.

2. **Distributed state:** A Beamtalk system may span multiple nodes. A single image snapshot doesn't make sense.

3. **OTP patterns:** State persistence via ETS/DETS/Mnesia is more flexible.

**Beamtalk approach:**

```
// Persist actor state explicitly
Actor subclass: PersistentCounter
  state:
    value = 0
    
  afterChange =>
    Erlang.dets insert: #counters entry: {self name, self state}
    
  restore: name =>
    state := Erlang.dets lookup: #counters key: name
    ^self
```

**Or use Mnesia for distributed persistence:**

```
Actor subclass: DistributedCounter
  state: value = 0
  
  persist: #mnesia  // Declarative: state backed by Mnesia
```

**Trade-off assessment:**

| Use Case | Smalltalk | Beamtalk |
|----------|-----------|----------|
| Save entire system | ✅ Image | ⚠️ Persist each actor |
| Distributed state | ❌ Single image | ✅ Mnesia/DETS |
| Version control | ❌ Binary image | ✅ Text source files |
| Selective persistence | ⚠️ All or nothing | ✅ Per-actor control |

**Recommendation:** The no-image model is actually superior for distributed systems and modern development workflows.

### 2.7 Changing Object's Class

**What Smalltalk provides:**

```smalltalk
myPoint class: Point3D.
"myPoint is now a Point3D"
```

**Why BEAM can't do this directly:**

A process's behavior is determined by its code module. You can't change what module a process runs without restarting it.

**Workaround: State Pattern or Restart**

```
// Option 1: State pattern - embed class in state
Actor subclass: Evolvable
  state:
    currentClass = nil
    data = {}
    
  evolve: newClass =>
    self.currentClass := newClass
    // Messages now dispatch through newClass
    
  doesNotUnderstand: selector args: args =>
    self.currentClass perform: selector withArgs: args on: self
```

```
// Option 2: Restart with new class (BEAM idiom)
counter := Counter spawn.
// Later, "upgrade" to CounterV2
counter migrate: CounterV2  // Spawns new process, transfers state
```

**Implementation of migrate:**
```erlang
%% In the actor
handle_call({migrate, NewModule}, _From, State) ->
    %% Start new process with same state
    {ok, NewPid} = NewModule:start_link(State),
    %% Transfer any linked processes
    transfer_links(self(), NewPid),
    %% Reply with new pid, then terminate
    {stop, {migrated, NewPid}, NewPid, State}.
```

**Trade-off assessment:**

The explicit restart pattern is actually cleaner for distributed systems where you want clear upgrade boundaries.

### 2.8 Custom VM Primitives

**What Smalltalk provides:**

```smalltalk
MyClass >> doSomethingFast
    <primitive: 142>
    "Fallback if primitive fails"
    ^self slowImplementation
```

**BEAM alternatives:**

| Method | Description | Use Case |
|--------|-------------|----------|
| BIFs | Built-in functions in Erlang | Math, list ops, etc. |
| NIFs | Native Implemented Functions (C) | Performance-critical code |
| Port drivers | External programs | System integration |

**Beamtalk approach:**

```
// Call existing BIFs
result := Erlang.math sqrt: 16.0

// For custom native code, write NIFs in C
// and call from Beamtalk like any Erlang module
result := MyNif.fastOperation: data

// Foreign function declaration for type safety
foreign MyNif.fastOperation(data: Binary) -> Binary
```

**Trade-off:** You can't write primitives *in Beamtalk*, but you can call any Erlang NIF, which is the same capability.

---

## Part 3: Implementation Options Analysis

The Linear issue identified four potential approaches. Here's a detailed analysis of each.

### Option 1: Pragmatic Hybrid (Recommended)

**Description:** Embrace BEAM's model. Reify what's natural, provide workarounds for the rest, document limitations clearly.

**Implementation:**
- Classes, methods, blocks, actors: full reification as described in Part 1
- Stack frames: post-exception only
- `become:`: proxy pattern + registry
- Instance tracking: ETS-based
- No continuations

**Performance impact:** Minimal. Uses BEAM efficiently.

**Complexity:** Low. Straightforward mapping.

**Pros:**
- Fast execution (native BEAM)
- Small runtime overhead
- Predictable behavior
- Full use of OTP ecosystem

**Cons:**
- Some Smalltalk idioms impossible
- Must document limitations clearly
- Developers need to learn BEAM idioms

**Best for:** Production systems, agent platforms, distributed applications.

### Option 2: Meta-Circular Interpreter

**Description:** Write a Beamtalk interpreter in Beamtalk that maintains an explicit stack data structure.

**Implementation:**
```erlang
%% Interpreter maintains explicit stack
-record(frame, {
    method :: method(),
    pc :: integer(),
    locals :: map(),
    sender :: frame() | nil
}).

interpret(#frame{method = M, pc = PC} = Frame, Stack) ->
    Instruction = get_instruction(M, PC),
    case Instruction of
        {push_self} ->
            NewStack = [Frame#frame.receiver | Stack],
            interpret(Frame#frame{pc = PC + 1}, NewStack);
        {send, Selector, ArgCount} ->
            {Args, RestStack} = lists:split(ArgCount, Stack),
            [Receiver | _] = RestStack,
            NewFrame = make_frame(Receiver, Selector, Args, Frame),
            interpret(NewFrame, []);
        {return} ->
            [Result | _] = Stack,
            Sender = Frame#frame.sender,
            interpret(Sender, [Result | sender_stack(Sender)]);
        %% ... etc
    end.
```

**Performance impact:** 10-100x slower. Every operation goes through interpreter dispatch.

**Complexity:** Very high. Must reimplement everything.

**Pros:**
- Full Smalltalk semantics
- `thisContext` works
- Continuations possible

**Cons:**
- Defeats BEAM's concurrency advantages
- Interpreter in BEAM is slow
- Massive implementation effort
- Two different execution models (compiled vs interpreted)

**Best for:** Educational tools, Smalltalk compatibility layers (not production).

### Option 3: Dual-Mode Execution

**Description:** Two execution paths — efficient BEAM compilation for production, instrumented interpretation for debugging.

**Implementation:**
```erlang
%% Compile-time flag
-ifdef(DEBUG_MODE).
interpret(Method, Args, State) -> 
    %% Slow path with full stack tracking
    beamtalk_debug:interpret(Method, Args, State).
-else.
interpret(Method, Args, State) ->
    %% Fast path: direct gen_server call
    Method(Args, State).
-endif.
```

**Performance impact:** None in production, 10-100x slower in debug mode.

**Complexity:** High. Must maintain two implementations.

**Pros:**
- Full semantics in debug mode
- Fast production execution
- Best of both worlds

**Cons:**
- Two implementations to maintain
- Bugs may appear in only one mode
- Semantic differences between modes
- Complex tooling

**Best for:** Systems needing both production performance and full debugging.

### Option 4: CPS Transformation

**Description:** Compile to continuation-passing style. Each function takes an explicit continuation argument.

**Implementation:**
```erlang
%% Normal style
factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

%% CPS style
factorial_cps(0, K) -> K(1);
factorial_cps(N, K) -> 
    factorial_cps(N - 1, fun(R) -> K(N * R) end).
```

**Performance impact:** 2-5x slower due to closure allocation. Every call creates a fun.

**Complexity:** High. Requires CPS transform in compiler.

**Pros:**
- Continuations become possible
- Non-local returns work
- Some metaprogramming enabled

**Cons:**
- Significant performance cost
- Complex debugging (stack traces are continuation chains)
- Doesn't help with `thisContext` during execution
- Memory pressure from closure allocation

**Best for:** Languages where continuations are essential (not Beamtalk's primary use case).

### Comparison Matrix

| Aspect | Hybrid | Interpreter | Dual-Mode | CPS |
|--------|--------|-------------|-----------|-----|
| Performance | ⭐⭐⭐⭐⭐ | ⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐ |
| `thisContext` | ❌ | ✅ | ⚠️ Debug only | ⚠️ Partial |
| Continuations | ❌ | ✅ | ⚠️ Debug only | ✅ |
| Complexity | ⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐ |
| BEAM ecosystem | ✅ | ⚠️ | ✅ | ⚠️ |
| Maintenance | Easy | Hard | Medium | Medium |

---

## Part 4: Recommendation

### Primary Recommendation: Pragmatic Hybrid

For Beamtalk v1, we should implement the **Pragmatic Hybrid** approach for these reasons:

1. **Aligns with BEAM philosophy:** "Everything is a process" maps well to "everything is an object."

2. **Performance:** Agent systems need to scale to millions of concurrent actors. Interpreter overhead is unacceptable.

3. **OTP integration:** Full access to supervision, distribution, hot code loading.

4. **Developer expectations:** BEAM developers expect BEAM behavior. Smalltalk developers choosing BEAM accept its model.

5. **Incremental enhancement:** We can add debug-mode instrumentation later if needed.

### What We Explicitly Won't Support

These Smalltalk features are **intentionally not supported** in Beamtalk v1:

| Feature | Reason | Alternative |
|---------|--------|-------------|
| `thisContext` during execution | BEAM doesn't reify stack | Post-exception traces |
| `become:` | Immutable pids | Proxy pattern, registry |
| `pointersTo` | Per-process heaps | ETS tracking |
| Continuations | Not in BEAM | Use actors/futures |
| Positional slot access | Opaque terms | Named field access |
| Image snapshots | Distributed model | Explicit persistence |
| Custom primitives | VM limitation | Use NIFs |

### Future Considerations

For Beamtalk v2+, we may consider:

1. **Debug Mode (Option 3):** Add an optional interpreted mode for debugging with full stack inspection. Toggle via compile flag.

2. **Tracing-Based Debugging:** Leverage BEAM's tracing infrastructure (`dbg`, `recon_trace`) for step debugging without full interpretation.

3. **Source Mapping:** Generate source maps so BEAM stack traces can be rendered as Beamtalk stack traces.

4. **IDE Integration:** Build debugging UI that uses tracing and post-exception info to provide a Smalltalk-like debugging experience without requiring stack reification.

---

## Part 5: Code Sketches

The following sketches incorporate learnings from [LFE Flavors](https://github.com/rvirding/flavors), Robert Virding's successful OOP implementation on BEAM.

### Actor Instance Record

Following Flavors' `#flavor-instance{}` pattern, we define a record that bundles class info with the process. This enables reflection and makes "self" a proper object reference.

```erlang
%% In beamtalk.hrl
-record(beamtalk_object, {
    class :: atom(),           % Class name (e.g., 'Counter')
    class_mod :: atom(),       % Class module (e.g., 'beamtalk_counter_class')
    pid :: pid()               % The actor process
}).

%% Usage: pass this around instead of raw pid
%% obj#beamtalk_object.pid for the process
%% obj#beamtalk_object.class for reflection
```

### Actor Instance Implementation

Each actor is a gen_server. Key patterns from Flavors:
- **Error isolation:** Catch errors and re-raise at caller (don't crash the instance)
- **Self reference:** Pass `#beamtalk_object{}` as "self" to methods

```erlang
-module(beamtalk_actor).
-behaviour(gen_server).

-include("beamtalk.hrl").

-record(state, {
    class :: atom(),
    class_mod :: atom(),
    self :: #beamtalk_object{},
    fields :: map()
}).

%% Start an actor instance
start_link(Class, ClassMod, InitArgs) ->
    gen_server:start_link(?MODULE, {Class, ClassMod, InitArgs}, []).

init({Class, ClassMod, InitArgs}) ->
    Self = #beamtalk_object{class = Class, class_mod = ClassMod, pid = self()},
    
    %% Get default field values from class
    DefaultFields = ClassMod:default_fields(),
    Fields = maps:merge(DefaultFields, InitArgs),
    
    %% Register with instance tracker
    beamtalk_instances:register(Class, self()),
    
    State = #state{class = Class, class_mod = ClassMod, self = Self, fields = Fields},
    
    %% Call init method if defined (Flavors pattern)
    case catch dispatch(init, [InitArgs], State) of
        {'EXIT', _} -> {ok, State};  % No init method, that's fine
        {_, NewState} -> {ok, NewState}
    end.

%% Async message (returns future to caller) - Beamtalk's async-first model
handle_cast({Selector, Args, FuturePid}, State) ->
    case safe_dispatch(Selector, Args, State) of
        {ok, Result, NewState} ->
            FuturePid ! {resolved, Result},
            {noreply, NewState};
        {error, Error, NewState} ->
            FuturePid ! {rejected, Error},
            {noreply, NewState}
    end.

%% Sync message (blocks caller) - opt-in for simple cases
handle_call({Selector, Args}, _From, State) ->
    case safe_dispatch(Selector, Args, State) of
        {ok, Result, NewState} ->
            {reply, {ok, Result}, NewState};
        {error, Error, NewState} ->
            {reply, {error, Error}, NewState}
    end;

%% Reflection messages
handle_call({class}, _From, #state{self = Self} = State) ->
    {reply, Self#beamtalk_object.class, State};
handle_call({respondsTo, Selector}, _From, #state{class_mod = Mod} = State) ->
    {reply, Mod:has_method(Selector), State};
handle_call({instVarNames}, _From, #state{fields = F} = State) ->
    PublicFields = [K || K <- maps:keys(F), not is_private(K)],
    {reply, PublicFields, State};
handle_call({instVarAt, Name}, _From, #state{fields = F} = State) ->
    {reply, maps:get(Name, F, undefined), State}.

%% Hot patching: update method table
handle_cast({update_methods, NewMethods}, #state{class_mod = Mod} = State) ->
    %% Class module will use updated methods on next dispatch
    Mod:set_methods(NewMethods),
    {noreply, State}.

%% Error-isolating dispatch (from Flavors)
%% Errors don't crash the instance - they're returned to caller
safe_dispatch(Selector, Args, State) ->
    try
        {Result, NewState} = dispatch(Selector, Args, State),
        {ok, Result, NewState}
    catch
        error:Error -> 
            {error, {error, Error, erlang:get_stacktrace()}, State};
        exit:Exit -> 
            {error, {exit, Exit, erlang:get_stacktrace()}, State};
        throw:Thrown -> 
            {error, {throw, Thrown, erlang:get_stacktrace()}, State}
    end.

dispatch(Selector, Args, #state{class_mod = Mod, self = Self, fields = Fields} = State) ->
    case Mod:lookup_method(Selector) of
        {ok, Fun} ->
            %% Call method with self and fields
            {Result, NewFields} = Fun(Self, Fields, Args),
            {Result, State#state{fields = NewFields}};
        error ->
            %% Try doesNotUnderstand:
            case Mod:lookup_method('doesNotUnderstand:args:') of
                {ok, DnuFun} ->
                    {Result, NewFields} = DnuFun(Self, Fields, [Selector, Args]),
                    {Result, State#state{fields = NewFields}};
                error ->
                    error({unknown_message, Selector, State#state.class})
            end
    end.

is_private('__' ++ _) -> true;
is_private(_) -> false.

terminate(_Reason, #state{class = Class, class_mod = Mod, self = Self, fields = Fields}) ->
    %% Call terminate method if defined (Flavors pattern)
    catch Mod:call_method(terminate, Self, Fields, []),
    ok.
```

### Class Object Implementation

Following Flavors' two-module pattern, we separate:
- **`beamtalk_<class>_class`** — Class metadata, factory methods (one per class)
- **`beamtalk_<class>`** — Generated instance behavior module

```erlang
-module(beamtalk_class).
-behaviour(gen_server).

%% A class object holds metadata and factory methods

-record(class_state, {
    name :: atom(),
    superclass :: atom() | nil,
    instance_methods :: #{atom() => function()},
    class_methods :: #{atom() => function()},
    instance_variables :: [atom()],
    method_source :: #{atom() => binary()},
    %% Flavors-style: track method combinations (before/after daemons)
    before_methods :: #{atom() => [function()]},
    after_methods :: #{atom() => [function()]}
}).

init([Name, Super, InstVars, InstMethods, ClassMethods]) ->
    State = #class_state{
        name = Name,
        superclass = Super,
        instance_methods = InstMethods,
        class_methods = ClassMethods,
        instance_variables = InstVars,
        method_source = #{},
        before_methods = #{},
        after_methods = #{}
    },
    {ok, State}.

%% Factory: Create new instance
handle_call({new, Args}, _From, #class_state{name = Name} = State) ->
    ClassMod = class_module(Name),
    {ok, Pid} = beamtalk_actor:start_link(Name, ClassMod, Args),
    %% Return object reference, not raw pid (Flavors pattern)
    ObjRef = #beamtalk_object{class = Name, class_mod = ClassMod, pid = Pid},
    {reply, ObjRef, State};

%% Introspection: Get all methods
handle_call({methods}, _From, #class_state{instance_methods = M} = State) ->
    {reply, maps:keys(M), State};

%% Introspection: Get superclass
handle_call({superclass}, _From, #class_state{superclass = S} = State) ->
    {reply, S, State};

%% Introspection: Get method object
handle_call({method, Selector}, _From, #class_state{instance_methods = M, method_source = Src} = State) ->
    case maps:find(Selector, M) of
        {ok, Fun} ->
            Source = maps:get(Selector, Src, <<"">>),
            MethodObj = #{
                '__class__' => 'CompiledMethod',
                '__selector__' => Selector,
                '__source__' => Source,
                '__bytecode__' => Fun
            },
            {reply, MethodObj, State};
        error ->
            {reply, nil, State}
    end;

%% Hot patching: Replace method
handle_call({put_method, Selector, Fun, Source}, _From, State) ->
    NewMethods = maps:put(Selector, Fun, State#class_state.instance_methods),
    NewSource = maps:put(Selector, Source, State#class_state.method_source),
    NewState = State#class_state{instance_methods = NewMethods, method_source = NewSource},
    %% Notify running instances to pick up new method table
    notify_instances(State#class_state.name, NewMethods),
    {reply, ok, NewState};

%% Method combinations (Flavors' before/after daemons)
handle_call({add_before, Selector, Fun}, _From, State) ->
    Befores = maps:get(Selector, State#class_state.before_methods, []),
    NewBefores = maps:put(Selector, [Fun | Befores], State#class_state.before_methods),
    {reply, ok, State#class_state{before_methods = NewBefores}};

handle_call({add_after, Selector, Fun}, _From, State) ->
    Afters = maps:get(Selector, State#class_state.after_methods, []),
    NewAfters = maps:put(Selector, Afters ++ [Fun], State#class_state.after_methods),
    {reply, ok, State#class_state{after_methods = NewAfters}}.

class_module(Name) ->
    list_to_atom("beamtalk_" ++ string:lowercase(atom_to_list(Name)) ++ "_class").

notify_instances(ClassName, NewMethods) ->
    Instances = beamtalk_instances:all(ClassName),
    [gen_server:cast(Pid, {update_methods, NewMethods}) || Pid <- Instances].
```

### Instance Tracking

```erlang
-module(beamtalk_instances).

-define(TABLE, beamtalk_instance_registry).

init() ->
    ets:new(?TABLE, [named_table, public, bag]).

register(Class, Pid) ->
    ets:insert(?TABLE, {Class, Pid}),
    %% Monitor to auto-unregister on termination
    spawn(fun() ->
        Ref = erlang:monitor(process, Pid),
        receive
            {'DOWN', Ref, process, Pid, _} ->
                ets:delete_object(?TABLE, {Class, Pid})
        end
    end).

all(Class) ->
    [Pid || {_, Pid} <- ets:lookup(?TABLE, Class), erlang:is_process_alive(Pid)].

count(Class) ->
    length(all(Class)).
```

### StackTrace Wrapper

```erlang
-module(beamtalk_stack_frame).

-record(stack_frame, {
    class :: atom(),
    selector :: atom(),
    arguments :: [term()],
    file :: string(),
    line :: integer()
}).

wrap(RawStacktrace) ->
    [to_frame(Entry) || Entry <- RawStacktrace].

to_frame({Module, Function, Arity, Info}) ->
    #stack_frame{
        class = module_to_class(Module),
        selector = Function,
        arguments = Arity,  % Just arity, not actual args
        file = proplists:get_value(file, Info, "unknown"),
        line = proplists:get_value(line, Info, 0)
    };
to_frame({Module, Function, Args, Info}) when is_list(Args) ->
    #stack_frame{
        class = module_to_class(Module),
        selector = Function,
        arguments = Args,
        file = proplists:get_value(file, Info, "unknown"),
        line = proplists:get_value(line, Info, 0)
    }.

module_to_class(Module) ->
    Name = atom_to_list(Module),
    case lists:prefix("beamtalk_", Name) of
        true -> list_to_atom(string:titlecase(lists:nthtail(9, Name)));
        false -> Module
    end.

%% Beamtalk API
method(#stack_frame{selector = S}) -> S.
receiver_class(#stack_frame{class = C}) -> C.
arguments(#stack_frame{arguments = A}) -> A.
source_location(#stack_frame{file = F, line = L}) -> {F, L}.
```

---

## References

### Smalltalk
- [Smalltalk-80 Blue Book](http://stephane.ducasse.free.fr/FreeBooks/BlueBook/Bluebook.pdf) — Original execution context model
- [Pharo by Example](https://books.pharo.org/) — Modern Smalltalk implementation
- [Squeak Source](http://www.squeak.org/) — Open source Smalltalk

### BEAM
- [BEAM Book](https://blog.stenmans.org/theBeamBook/) — VM internals
- [Erlang Efficiency Guide](https://www.erlang.org/doc/efficiency_guide/) — Performance characteristics
- [Armstrong Thesis](https://erlang.org/download/armstrong_thesis_2003.pdf) — "Making reliable distributed systems"

### Related Languages
- [Newspeak Language](https://newspeaklanguage.org/) — Async actors, module system
- [Gleam](https://gleam.run/) — Rust-to-BEAM compilation (functional, no OOP)
- [Pony](https://www.ponylang.io/) — Actor model with capabilities

### Prior Art on BEAM
- [LFE Flavors](https://github.com/rvirding/flavors) — **Highly relevant**: OOP on BEAM by Robert Virding
- [LFE Object System (LOS)](https://github.com/lfex/los) — CLOS-inspired objects for LFE
- [ETOS](https://github.com/pichi/etos) — Erlang-to-Smalltalk (abandoned)
- [LFE](https://lfe.io/) — Lisp on BEAM, similar interop challenges

---

## Appendix: Lessons from Gleam and LFE

### Gleam's Approach: Explicit Rejection of OOP

Gleam deliberately avoids OOP, preferring:
- **Custom types (tagged unions)** instead of classes
- **Pattern matching** instead of polymorphic dispatch
- **Modules + functions** instead of methods
- **No type classes/traits** — simplicity over abstraction

**Gleam's Actor wrapper** (`gleam_otp`) wraps gen_server with type-safe message handling but exposes no OOP illusion — just typed processes.

**Lesson for Beamtalk:** Gleam proves you can build successful BEAM languages without OOP. But Beamtalk's goal is different — we *want* the Smalltalk programming model. Gleam validates that processes-as-actors is the right foundation.

### LFE Flavors: OOP Successfully Implemented on BEAM

Robert Virding's [Flavors](https://github.com/rvirding/flavors) library implements Lisp Machine-style OOP on BEAM. Key design decisions:

**1. Instances are gen_server processes:**
```erlang
%% From flavors_instance.erl
-record(state, {name,fm,self,ivars=none}).

init({Flav,Fm,Opts}) ->
    Ivars = Fm:'instance-variables'(),
    Mlist = make_map_list(Ivars, Opts),
    Imap = maps:from_list(Mlist),
    erlang:put('instance-variables', Imap),  % Process dictionary!
    Self = #'flavor-instance'{flavor=Flav, flavor_mod=Fm, instance=self()},
    Fm:'combined-method'(init, Self, {Opts}),
    {ok,#state{name=Flav,fm=Fm,self=Self}}.
```

**2. Two modules per class:**
- `*-flavor-core` — Compile-time metadata (methods, inheritance)
- `*-flavor` — Runtime access functions (generated on first instantiation)

This avoids generating modules for mixins that are never directly instantiated.

**3. Instance variables in process dictionary:**
```erlang
erlang:put('instance-variables', Imap)
```
Simple but effective. Beamtalk uses maps in gen_server state instead.

**4. Errors caught and re-raised at caller:**
```erlang
send_method(Meth, Args, #state{fm=Fm,self=Self}) ->
    try
        Result = Fm:'combined-method'(Meth, Self, Args),
        {ok,Result}
    catch
        error:Error -> {error,Error};
        exit:Exit -> {error,Exit};
        throw:Thrown -> {throw,Thrown}
    end.
```
Errors don't crash the instance — they're signaled to the caller. This preserves fault isolation.

**5. Synchronous message sending:**
```erlang
send(Ins, Meth, Args) ->
    gen_server:call(Ins, {send,Meth,Args}).
```
Flavors uses synchronous calls. Beamtalk uses async-first with explicit `await`.

**6. The `flavor-instance` record:**
```erlang
-record('flavor-instance',{flavor,flavor_mod,instance}).
```
A handle that bundles class name, class module, and pid. Similar to what Beamtalk could use for reflection.

### Lessons Applied to Beamtalk

| Flavors Pattern | Beamtalk Adoption | Notes |
|-----------------|-------------------|-------|
| gen_server per instance | ✅ Yes | Same approach |
| Two modules per class | ⚠️ Consider | Could reduce generated code for abstract classes |
| Process dictionary for ivars | ❌ No | Use map in gen_server state (more explicit) |
| Error catching at caller | ✅ Yes | Preserve fault isolation |
| Synchronous by default | ❌ No | Beamtalk is async-first |
| Instance handle record | ✅ Consider | Useful for reflection, passing "self" |

### Key Insight: Flavors Validates Our Approach

LFE Flavors proves that OOP semantics work well on BEAM when:
1. Each object is a process
2. Instance variables are maps
3. Methods dispatch via a method table
4. Errors are isolated (caught and re-raised)

**Beamtalk's additions:**
- Async-first (futures) instead of sync-first
- Compile-time code generation (Rust) instead of runtime macro expansion
- ETS-based instance tracking for `allInstances`
- Integration with Smalltalk-style reflection (`class`, `respondsTo:`, etc.)
