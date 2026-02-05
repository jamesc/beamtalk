# Dynamic Classes in the REPL - User Guide

This guide shows how to use dynamic classes interactively in the Beamtalk REPL.

## Overview

Beamtalk supports **runtime class creation** - you can define new classes interactively without writing files or recompiling. This enables true Smalltalk-style live development.

Dynamic classes are interpreter-based (Phase 1):
- Methods are stored as Erlang closures
- Slightly slower than compiled classes (acceptable for prototyping)
- Perfect for REPL experimentation and live coding

## Creating a Dynamic Class

Use `beamtalk_object_class:create_subclass/3` to define a new class:

```erlang
%% Create a simple Counter class
{ok, CounterClass} = beamtalk_object_class:create_subclass('Actor', 'Counter', #{
    instance_variables => [value],
    instance_methods => #{
        increment => fun(_Self, [], State) ->
            Value = maps:get(value, State, 0),
            {reply, Value + 1, maps:put(value, Value + 1, State)}
        end,
        decrement => fun(_Self, [], State) ->
            Value = maps:get(value, State, 0),
            {reply, Value - 1, maps:put(value, Value - 1, State)}
        end,
        getValue => fun(_Self, [], State) ->
            Value = maps:get(value, State, 0),
            {reply, Value, State}
        end
    }
}).
```

**Parameters:**
1. **Superclass**: Atom name of parent class (usually `'Actor'` or `'Object'`)
2. **ClassName**: Atom name for your new class
3. **ClassSpec**: Map with:
   - `instance_variables`: List of field names `[atom()]`
   - `instance_methods`: Map of `{Selector => Fun}` where `Fun` is arity 3:
     ```erlang
     fun(Self, Args, State) -> {reply, Result, NewState} end
     ```

## Spawning Instances

Once you have a class, spawn instances with `beamtalk_object_class:new/2`:

```erlang
%% Spawn with initial values
{ok, Counter} = beamtalk_object_class:new(CounterClass, [#{value => 10}]).

%% Spawn with default values (fields initialize to nil)
{ok, Counter2} = beamtalk_object_class:new(CounterClass, [#{}]).
```

Returns a `#beamtalk_object{}` record containing:
- `class`: Class name atom
- `class_mod`: Module name (beamtalk_dynamic_object for dynamic classes)
- `pid`: The actor process PID

## Sending Messages

Extract the PID and send messages using `gen_server:call/2`:

```erlang
CounterPid = Counter#beamtalk_object.pid.

gen_server:call(CounterPid, {getValue, []}).    %=> 10
gen_server:call(CounterPid, {increment, []}).   %=> 11
gen_server:call(CounterPid, {getValue, []}).    %=> 11
gen_server:call(CounterPid, {decrement, []}).   %=> 10
```

**Message Format:** `{Selector, Args}` where:
- `Selector`: Atom method name
- `Args`: List of arguments (empty list `[]` for no args)

## Class Introspection

Dynamic classes support full reflection:

```erlang
%% Look up a class by name
ClassPid = beamtalk_object_class:whereis_class('Counter').

%% Get class metadata
beamtalk_object_class:class_name(ClassPid).             %=> 'Counter'
beamtalk_object_class:superclass(ClassPid).             %=> 'Actor'
beamtalk_object_class:instance_variables(ClassPid).     %=> [value]
beamtalk_object_class:methods(ClassPid).                %=> [increment, decrement, getValue]
```

## Inheritance

Dynamic classes can inherit from other dynamic classes:

```erlang
%% Create parent class
{ok, ParentClass} = beamtalk_object_class:create_subclass('Actor', 'Parent', #{
    instance_variables => [],
    instance_methods => #{
        parentMethod => fun(_Self, [], State) ->
            {reply, from_parent, State}
        end
    }
}).

%% Create child class
{ok, ChildClass} = beamtalk_object_class:create_subclass('Parent', 'Child', #{
    instance_variables => [],
    instance_methods => #{
        childMethod => fun(_Self, [], State) ->
            {reply, from_child, State}
        end
    }
}).

%% Verify hierarchy
beamtalk_object_class:superclass(ChildClass).  %=> 'Parent'
```

**Note:** Method inheritance is handled by `beamtalk_class` lookup, not directly tested in Phase 1.

## Method Closures

Methods are functions of arity 3:

```erlang
fun(Self, Args, State) ->
    %% Self: #beamtalk_object{} record
    %% Args: [arg1, arg2, ...] list
    %% State: #{field => value, ...} map
    
    %% Access fields
    Field = maps:get(field_name, State),
    
    %% Update state
    NewState = maps:put(field_name, new_value, State),
    
    %% Return result
    {reply, Result, NewState}
end
```

**Return Values:**
- `{reply, Result, NewState}` - Return a value and update state
- `{noreply, NewState}` - No return value, just update state

## Example: Point Class

```erlang
%% Create a 2D Point class
{ok, PointClass} = beamtalk_object_class:create_subclass('Actor', 'Point', #{
    instance_variables => [x, y],
    instance_methods => #{
        getX => fun(_Self, [], State) ->
            {reply, maps:get(x, State), State}
        end,
        getY => fun(_Self, [], State) ->
            {reply, maps:get(y, State), State}
        end,
        setX => fun(_Self, [NewX], State) ->
            {reply, ok, maps:put(x, NewX, State)}
        end,
        setY => fun(_Self, [NewY], State) ->
            {reply, ok, maps:put(y, NewY, State)}
        end,
        distanceFrom => fun(_Self, [X2, Y2], State) ->
            X1 = maps:get(x, State),
            Y1 = maps:get(y, State),
            Dist = math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)),
            {reply, Dist, State}
        end
    }
}).

%% Spawn a point at (3, 4)
{ok, P} = beamtalk_object_class:new(PointClass, [#{x => 3, y => 4}]).
Pid = P#beamtalk_object.pid.

%% Use it
gen_server:call(Pid, {getX, []}).                    %=> 3
gen_server:call(Pid, {distanceFrom, [0, 0]}).        %=> 5.0
gen_server:call(Pid, {setX, [10]}).                  %=> ok
gen_server:call(Pid, {getX, []}).                    %=> 10
```

## Async Messaging (Futures)

Dynamic classes support async messages with futures:

```erlang
%% Send async message
FuturePid = beamtalk_future:new(),
gen_server:cast(Pid, {increment, [], FuturePid}),

%% Do other work...

%% Await result
Result = beamtalk_future:await(FuturePid).
```

## Limitations (Phase 1)

Current limitations:
- **Performance**: Slower than compiled classes (closure overhead)
- **Super calls**: Not yet supported in dynamic methods
- **No syntax sugar**: Must use `beamtalk_object_class:create_subclass/3` directly

These will be addressed in Phase 2 (compiler-embedded dynamic classes).

## Tips

1. **Save class definitions**: Store class specs in variables for reuse
   ```erlang
   CounterSpec = #{
       instance_variables => [value],
       instance_methods => #{...}
   }.
   {ok, C1} = beamtalk_object_class:create_subclass('Actor', 'Counter', CounterSpec).
   ```

2. **Extract PID early**: Get the PID once and reuse
   ```erlang
   {ok, Obj} = beamtalk_object_class:new(ClassPid, [InitState]),
   Pid = Obj#beamtalk_object.pid,
   %% Now use Pid for all messages
   ```

3. **Check for errors**: Class creation can fail
   ```erlang
   case beamtalk_object_class:create_subclass('BadParent', 'MyClass', Spec) of
       {ok, ClassPid} -> use_it(ClassPid);
       {error, {superclass_not_found, 'BadParent'}} -> handle_error()
   end
   ```

4. **Use introspection**: Explore classes interactively
   ```erlang
   beamtalk_object_class:methods(ClassPid).  %% See what methods exist
   ```

## Future: Phase 2

Phase 2 will add:
- **Compiler embedding**: Dynamic classes compiled to real gen_server code
- **Full performance**: Match compiled class performance
- **Syntax sugar**: `Actor subclass: 'MyClass'` message send
- **Method compilation**: Parse method source strings
- **Super calls**: Support `super` in dynamic methods

Stay tuned for Phase 2!

## See Also

- `runtime/test/beamtalk_dynamic_class_tests.erl` - Full test suite
- `runtime/src/beamtalk_dynamic_object.erl` - Implementation
- `runtime/src/beamtalk_class.erl` - Class registry
