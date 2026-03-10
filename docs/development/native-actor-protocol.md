# Native Actor Facade Protocol

This guide documents the hand-written facade protocol for native Erlang-backed Actor classes. It is the Phase 0 deliverable of [ADR 0056 — Native Erlang-Backed Actors](../ADR/0056-native-erlang-backed-actors.md).

Phase 1 will add `native:` and `self delegate` compiler support so the facade is generated automatically. Until then, library authors can hand-write facades using this protocol.

---

## Overview

Some Actor classes need a hand-written Erlang gen_server rather than compiler-generated Beamtalk bytecode. Examples: actors managing OS ports, ring buffers with pub/sub subscribers, or connection pool state that cannot be expressed in Beamtalk.

A **native-backed actor** consists of two files:

| File | Purpose |
|------|---------|
| `MyActor.bt` | Beamtalk class declaration + method stubs |
| `my_erlang_module.erl` | Erlang gen_server with all real logic |

The `.bt` file is compiled by the Beamtalk compiler as normal. Each method body calls through to the backing gen_server via `beamtalk_actor:sync_send/3`.

---

## Facade Module Structure

The Beamtalk-side `.bt` file should look like this:

```beamtalk
// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

Actor subclass: KeyValueStore

  /// Create a new store with a generated table name.
  class create -> KeyValueStore =>
    (Erlang beamtalk_kv_store) spawn: #{}

  /// Create a named store.
  class create: name :: String -> KeyValueStore =>
    (Erlang beamtalk_kv_store) spawn: #{"name" => name}

  get: key -> Object =>
    (Erlang beamtalk_kv_store) dispatch: self selector: 'get:' args: #(key)

  put: key :: Object value: value :: Object -> Nil =>
    (Erlang beamtalk_kv_store) dispatch: self selector: 'put:value:' args: #(key, value)

  delete: key -> Nil =>
    (Erlang beamtalk_kv_store) dispatch: self selector: 'delete:' args: #(key)

  keys -> List =>
    (Erlang beamtalk_kv_store) dispatch: self selector: 'keys' args: #()

  size -> Integer =>
    (Erlang beamtalk_kv_store) dispatch: self selector: 'size' args: #()
```

Until Phase 1 ships, use `(Erlang module)` FFI calls. The Erlang module must export `spawn/1` and `dispatch/3` as the facade entry points (see below).

---

## Erlang Facade Exports

Your backing Erlang module must export the following functions.

### `spawn/1`

Starts the gen_server and returns a Beamtalk actor object tuple.

```erlang
-spec spawn(map()) -> beamtalk_actor:actor_ref().
spawn(Config) ->
    case start_link(Config) of
        {ok, Pid} ->
            {'beamtalk_object', 'KeyValueStore', 'bt@mylib@key_value_store', Pid};
        {error, Reason} ->
            error(#beamtalk_error{kind = instantiation_error,
                                  details = #{reason => Reason}})
    end.
```

The second element is the class name atom, and the third is the module atom for the compiled `.bt` class. The fourth element is the gen_server pid.

### `dispatch/3`

Routes a Beamtalk message to the backing gen_server. Called from each instance method stub.

```erlang
-spec dispatch(beamtalk_actor:actor_ref(), atom(), list()) -> term().
dispatch(Self, Selector, Args) ->
    Pid = element(4, Self),
    beamtalk_actor:sync_send(Pid, Selector, Args).
```

### `has_method/1`

Returns `true` for each selector this facade handles. Used by the runtime for `respondsTo:` checks.

```erlang
-spec has_method(atom()) -> boolean().
has_method('get:')         -> true;
has_method('put:value:')   -> true;
has_method('delete:')      -> true;
has_method(keys)           -> true;
has_method(size)           -> true;
has_method(_)              -> false.
```

---

## `start_link/1` Contract

The backing gen_server must export `start_link/1` accepting a map:

```erlang
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).
```

- Always arity 1 — no `start_link/0` fallback.
- When the Beamtalk caller passes no arguments, Config is `#{}` (empty map).
- Map keys are binary strings (e.g. `<<"name">>`) because Beamtalk string literals compile to binaries.

---

## Wire Protocol: `handle_call/3`

All synchronous messages arrive as:

```erlang
{Selector, [Arg1, Arg2, ...]}
```

where `Selector` is an atom (the Beamtalk selector) and the second element is a list of arguments.

Examples:

```erlang
handle_call({'get:', [Key]}, _From, #{table := T} = State) ->
    Result = case ets:lookup(T, Key) of
        [{Key, Value}] -> {ok, Value};
        []             -> {ok, nil}
    end,
    {reply, Result, State};

handle_call({'put:value:', [Key, Value]}, _From, #{table := T} = State) ->
    ets:insert(T, {Key, Value}),
    {reply, {ok, nil}, State};

handle_call({'delete:', [Key]}, _From, #{table := T} = State) ->
    ets:delete(T, Key),
    {reply, {ok, nil}, State};

handle_call({keys, []}, _From, #{table := T} = State) ->
    Keys = [K || {K, _} <- ets:tab2list(T)],
    {reply, {ok, Keys}, State};

handle_call({size, []}, _From, #{table := T} = State) ->
    {reply, {ok, ets:info(T, size)}, State}.
```

### `{ok, Result}` Wrapping Requirement

**All new gen_servers MUST wrap replies in `{ok, Result}`:**

```erlang
{reply, {ok, Value}, NewState}    %% success
{reply, {ok, nil}, NewState}      %% void method
{reply, {error, Reason}, NewState} %% error propagated as exception
```

`beamtalk_actor:sync_send/3` unwraps `{ok, Value}` → `Value`, and raises `{error, Reason}` as a `#beamtalk_error{}` exception. This prevents legitimate `{error, _}` return values from being misinterpreted.

> **Note:** The existing `beamtalk_subprocess.erl` and `beamtalk_transcript_stream.erl` modules pre-date this protocol and use unwrapped replies for backward compatibility. New modules must use `{ok, Result}` wrapping.

---

## Fire-and-Forget (Cast) Methods

`beamtalk_actor:sync_send/3` is for synchronous calls. For fire-and-forget methods, use `beamtalk_actor:cast_send/3` in the dispatch function and implement `handle_cast/2`:

```erlang
%% In the facade dispatch/3:
dispatch(Self, show, [Value]) ->
    Pid = element(4, Self),
    beamtalk_actor:cast_send(Pid, show, [Value]);

%% In the gen_server:
handle_cast({cast, show, [Value]}, State) ->
    {noreply, buffer(Value, State)}.
```

The cast wire format is `{cast, Selector, Args}`.

---

## Complete Example: KeyValueStore

### `KeyValueStore.bt`

```beamtalk
// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

Actor subclass: KeyValueStore

  class create -> KeyValueStore =>
    (Erlang beamtalk_kv_store) spawn: #{}

  class create: name :: String -> KeyValueStore =>
    (Erlang beamtalk_kv_store) spawn: #{"name" => name}

  get: key -> Object =>
    (Erlang beamtalk_kv_store) dispatch: self selector: 'get:' args: #(key)

  put: key :: Object value: value :: Object -> Nil =>
    (Erlang beamtalk_kv_store) dispatch: self selector: 'put:value:' args: #(key, value)

  delete: key -> Nil =>
    (Erlang beamtalk_kv_store) dispatch: self selector: 'delete:' args: #(key)

  keys -> List =>
    (Erlang beamtalk_kv_store) dispatch: self selector: 'keys' args: #()

  size -> Integer =>
    (Erlang beamtalk_kv_store) dispatch: self selector: 'size' args: #()
```

### `beamtalk_kv_store.erl`

```erlang
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_kv_store).
-behaviour(gen_server).

-export([spawn/1, dispatch/3, has_method/1]).
-export([start_link/1, init/1, handle_call/3, handle_cast/2]).

%% Facade entry points

-spec spawn(map()) -> term().
spawn(Config) ->
    case start_link(Config) of
        {ok, Pid} ->
            {'beamtalk_object', 'KeyValueStore', 'bt@mylib@key_value_store', Pid};
        {error, Reason} ->
            error(#beamtalk_error{kind = instantiation_error,
                                  details = #{reason => Reason}})
    end.

-spec dispatch(term(), atom(), list()) -> term().
dispatch(Self, Selector, Args) ->
    Pid = element(4, Self),
    beamtalk_actor:sync_send(Pid, Selector, Args).

-spec has_method(atom()) -> boolean().
has_method('get:')       -> true;
has_method('put:value:') -> true;
has_method('delete:')    -> true;
has_method(keys)         -> true;
has_method(size)         -> true;
has_method(_)            -> false.

%% OTP callbacks

start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

init(Config) ->
    Name = maps:get(<<"name">>, Config, kv_table),
    Table = ets:new(Name, [set, protected]),
    {ok, #{table => Table}}.

handle_call({'get:', [Key]}, _From, #{table := T} = State) ->
    Result = case ets:lookup(T, Key) of
        [{Key, Value}] -> {ok, Value};
        []             -> {ok, nil}
    end,
    {reply, Result, State};

handle_call({'put:value:', [Key, Value]}, _From, #{table := T} = State) ->
    ets:insert(T, {Key, Value}),
    {reply, {ok, nil}, State};

handle_call({'delete:', [Key]}, _From, #{table := T} = State) ->
    ets:delete(T, Key),
    {reply, {ok, nil}, State};

handle_call({keys, []}, _From, #{table := T} = State) ->
    Keys = [K || {K, _} <- ets:tab2list(T)],
    {reply, {ok, Keys}, State};

handle_call({size, []}, _From, #{table := T} = State) ->
    {reply, {ok, ets:info(T, size)}, State};

handle_call(_Unknown, _From, State) ->
    {reply, {error, unknown_selector}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
```

---

## Checklist for New Native-Backed Actors

- [ ] Backing module exports `spawn/1`, `dispatch/3`, `has_method/1`
- [ ] `start_link/1` accepts a map (use `#{}` for no-arg construction)
- [ ] All `handle_call` replies use `{ok, Result}` or `{error, Reason}` wrapping
- [ ] All selectors for `handle_call` match the Beamtalk method names exactly (including colons)
- [ ] `has_method/1` lists all handled selectors
- [ ] OTP logger macros used for logging (`?LOG_ERROR`, etc.) — never `io:format`
- [ ] EUnit tests cover: success path, unknown selector, invalid config

---

## Future: Phase 1 Compiler Support

Phase 1 (BT-1205–BT-1210) will add `native:` syntax to the compiler, eliminating the boilerplate:

```beamtalk
// Phase 1 syntax (not yet supported):
Actor subclass: KeyValueStore native: beamtalk_kv_store

  class create -> KeyValueStore => self spawn
  class create: name :: String -> KeyValueStore => self spawnWith: #{"name" => name}

  get: key -> Object => self delegate
  put: key :: Object value: value :: Object -> Nil => self delegate
  delete: key -> Nil => self delegate
  keys -> List => self delegate
  size -> Integer => self delegate
```

The compiler will generate the facade and dispatch functions automatically. Phase 0 facades written per this guide will be migrated in Phase 2.
