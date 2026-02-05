# Design: Metaprogramming and Class-as-Process

**Issue:** Future (TBD)  
**Status:** Draft — captures deferred work from BT-151  
**Depends on:** BT-151 (Self-as-Object and Reflection API)  
**Date:** 2026-02-01

## Executive Summary

This document captures metaprogramming features deferred from BT-151. These features require "class-as-process" — treating classes as first-class actor objects rather than just modules.

**Key features:**
- Classes as actor objects (full metaclass protocol)
- Runtime method dictionaries (dynamic dispatch)
- Dynamic `super` dispatch
- Class method inheritance
- Passing classes as values
- Method objects (CompiledMethod introspection)
- `thisContext` emulation
- `become:` emulation

---

## Part 1: Class-as-Process Architecture

### Current State (after BT-151)

- Classes compile to Erlang modules
- Class "methods" are module functions (`Counter.spawn` → `counter:spawn()`)
- No `self` in class methods
- No class method inheritance
- Classes can't be passed as values

### Target State

Each class is represented by two things:
1. **Module** — compiled methods (for performance)
2. **Class process** — holds method dictionary, superclass ref, metadata (for dynamism)

```erlang
%% Class process state
#{
    '__class__' => 'Counter class',      % Metaclass
    '__superclass__' => 'Actor',          % Superclass reference
    '__instance_methods__' => #{          % Method dictionary
        increment => fun counter:handle_increment/4,
        getValue => fun counter:handle_getValue/4
    },
    '__class_methods__' => #{
        spawn => fun counter:class_spawn/3,
        defaultValue => fun counter:class_defaultValue/3
    },
    '__instance_variables__' => [value],
    '__name__' => 'Counter'
}
```

### Class Process Registration

```erlang
%% Each class registers its process
%% Name: beamtalk_class_<classname>
register(beamtalk_object_class_counter, ClassPid).

%% Lookup
beamtalk:class('Counter') -> #beamtalk_object{class='Counter class', ...}
```

---

## Part 2: Dynamic Method Dispatch

### Current (BT-151): Static Dispatch

```erlang
%% Method call compiles to direct dispatch
call 'counter':'dispatch'('increment', Args, Self, State)
```

### Target: Dynamic Dispatch

```erlang
%% Method call looks up in method dictionary
dispatch(Selector, Args, Self, State) ->
    Class = Self#beamtalk_object.class,
    ClassPid = beamtalk:class_process(Class),
    case beamtalk_object_class:lookup_method(ClassPid, Selector) of
        {ok, Fun} -> Fun(Args, Self, State);
        not_found -> handle_dnu(Selector, Args, Self, State)
    end.
```

### Method Lookup Chain

```
SpecialCounter (method dict) 
    ↓ not found
Counter (method dict)
    ↓ not found  
Actor (method dict)
    ↓ not found
Object (method dict)
    ↓ not found
doesNotUnderstand:
```

### Performance Optimization

- **Inline caching:** Remember last lookup result, revalidate on class change
- **Method combination:** Pre-compute before/after chains at class load time
- **Fallback to static:** For sealed classes, use compile-time dispatch

---

## Part 3: Dynamic `super` Dispatch

### Current (BT-151): Static Super

```erlang
%% super increment → direct call to superclass module
call 'counter':'dispatch'('increment', Args, Self, State)
```

### Target: Dynamic Super

```erlang
%% super increment → lookup starting from superclass
super_dispatch(Selector, Args, Self, State, DefiningClass) ->
    Superclass = beamtalk:superclass_of(DefiningClass),
    lookup_method_from(Superclass, Selector, Args, Self, State).
```

**Key:** `super` starts lookup from the superclass of the class *where the method is defined*, not the receiver's class. This requires passing the defining class context.

---

## Part 4: Class Methods with Self

### Current (BT-151): No Self in Class Methods

```
Counter class method
  defaultValue => 42   // Works
  
  withLogging =>
    self log: "creating"  // ERROR: no self
    self spawn
```

### Target: Class Methods Have Self = Class Object

```
Counter class method
  withLogging =>
    self log: "creating"   // self = Counter class object
    self spawn             // Calls class method spawn
```

**Implementation:**
```erlang
%% Class method dispatch
class_dispatch(Selector, Args, ClassSelf, ClassState) ->
    %% ClassSelf = #beamtalk_object{class='Counter class', pid=ClassProcessPid}
    Methods = maps:get('__class_methods__', ClassState),
    case maps:find(Selector, Methods) of
        {ok, Fun} -> Fun(Args, ClassSelf, ClassState);
        error -> class_handle_dnu(Selector, Args, ClassSelf, ClassState)
    end.
```

---

## Part 5: Passing Classes as Values

### Current (BT-151): Can't Pass Classes

```
factory := Counter        // What is Counter? Just an atom reference
factory spawn             // Can't dispatch to an atom
```

### Target: Classes Are Objects

```
factory := Counter        // Counter is a #beamtalk_object{} (class object)
factory spawn             // Sends 'spawn' message to class object
factory class             // => 'Counter class' (the metaclass)

// Factory pattern
createWith: factory =>
    factory spawn
    
createWith: Counter       // Works!
createWith: SpecialCounter // Also works!
```

**Implementation:**
- `Counter` (bare class name) compiles to lookup of class object
- Class objects are `#beamtalk_object{}` with `class = 'Counter class'`
- Message sends to class objects go through class dispatch

---

## Part 6: Method Objects (CompiledMethod)

### Target API

```
method := Counter >> #increment   // Get method object

method selector          // => #increment
method argumentCount     // => 0
method source            // => "increment => self.value := self.value + 1"
method pragmas           // => []
method valueWithReceiver: obj args: []  // Execute method
```

### Implementation

```erlang
-record(beamtalk_method, {
    selector :: atom(),
    class :: atom(),
    arity :: non_neg_integer(),
    source :: binary() | undefined,
    pragmas :: list(),
    fun_ref :: function()
}).
```

### Method Replacement (Hot Patching)

```
Counter >> #increment put: [:self |
    Telemetry log: "incrementing"
    self.value := self.value + 1
]
```

Updates the method dictionary in the class process. Existing instances see the new method on next call (dynamic dispatch).

---

## Part 7: thisContext Emulation

### What Smalltalk Provides

```smalltalk
thisContext          "Current stack frame"
thisContext sender   "Caller's frame"  
thisContext method   "Current method"
```

### BEAM Limitation

BEAM doesn't expose stack frames as first-class objects. We can only get stack traces after exceptions.

### Partial Emulation

```
// Available (via process dictionary or parameter passing)
thisMethod           // Current method being executed (compile-time known)

// Available (after exception)
Exception stack      // Stack trace with method names

// NOT available
thisContext sender   // Can't access caller's frame
thisContext restart  // Can't restart execution
```

### Implementation

```erlang
%% Compile-time: inject method info
handle_increment(Args, Self, State) ->
    put('$beamtalk_current_method', {counter, increment, 0}),
    %% ... method body ...
    erase('$beamtalk_current_method').
```

---

## Part 8: `become:` Emulation

### What Smalltalk Provides

```smalltalk
obj1 become: obj2   "Swap identities - all refs to obj1 now point to obj2"
```

### BEAM Limitation

Can't mutate references held by other processes. Each process has its own copy of terms.

### Proxy Pattern Workaround

```
// Instead of become:, use a proxy that can change its target
Proxy subclass: MutableRef
  state: target = nil
  
  become: newTarget =>
    self.target := newTarget
    
  doesNotUnderstand: selector args: args =>
    self.target perform: selector withArgs: args
```

**Limitation:** Only works if all references go through the proxy. Direct pid references won't redirect.

### Registry Pattern

```erlang
%% Global registry maps stable IDs to current pids
beamtalk_registry:register(myObject, Pid1),
beamtalk_registry:become(myObject, Pid2),  %% Updates mapping
beamtalk_registry:lookup(myObject) -> Pid2
```

---

## Part 9: Full Reflection API

### Instance Reflection (BT-151)

```
obj class
obj respondsTo: #selector
obj instVarNames
obj instVarAt: #name
obj instVarAt: #name put: value
obj perform: #selector
obj perform: #selector withArgs: args
```

### Class Reflection (This Doc)

```
Counter superclass           // => Actor
Counter allSuperclasses      // => [Actor, Object]
Counter subclasses           // => [SpecialCounter, ...]
Counter allInstances         // => [...all instances...]
Counter instanceVariables    // => [value]
Counter methods              // => [increment, getValue, ...]
Counter methodDictionary     // => #{increment => Method, ...}
Counter >> #increment        // => CompiledMethod
```

### System Reflection

```
Beamtalk allClasses          // => [Counter, Actor, Object, ...]
Beamtalk classNamed: #Counter // => Counter class object
Beamtalk globals             // => #{...}
```

---

## Part 10: Implementation Roadmap

### Phase 1: Class Process Infrastructure
- [ ] Create class process for each compiled class
- [ ] Register class processes with well-known names
- [ ] Implement `beamtalk:class/1` lookup

### Phase 2: Dynamic Dispatch
- [ ] Add method dictionary to class process state
- [ ] Implement `lookup_method/2` with inheritance chain
- [ ] Update codegen to use dynamic dispatch (opt-in or default)

### Phase 3: Class Methods with Self
- [ ] Implement class method dispatch
- [ ] Allow `self` in class methods (refers to class object)
- [ ] Implement class method inheritance

### Phase 4: Method Objects
- [ ] Define `#beamtalk_method{}` record
- [ ] Implement `Class >> #selector` syntax
- [ ] Implement method replacement API

### Phase 5: Full Metaclass Protocol
- [ ] Each class has a metaclass
- [ ] Metaclasses are classes too
- [ ] `Counter class class` works

---

## References

- [BT-151: Self-as-Object Design](design-self-as-object.md)
- [Smalltalk-80 Blue Book](http://stephane.ducasse.free.fr/FreeBooks/BlueBook/Bluebook.pdf) — Chapter 5
- [Pharo Metaclasses](https://books.pharo.org/booklet-ReflectiveCore/)
- [Newspeak Modules](https://newspeaklanguage.org/documents.html)
