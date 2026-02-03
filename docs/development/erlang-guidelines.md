# Erlang/OTP/BEAM Best Practices

Guidelines for generating Erlang code and working with the BEAM runtime in the Beamtalk compiler.

## References

- [OTP Design Principles](https://www.erlang.org/doc/system/design_principles.html) - Official OTP patterns
- [Erlang Efficiency Guide](https://www.erlang.org/doc/system/efficiency_guide.html) - Performance best practices
- [Core Erlang Specification](https://www.it.uu.se/research/group/hipe/cerl/) - Core Erlang format

---

## Generated Code Style

When generating Core Erlang or Erlang source:

```erlang
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_counter).
-behaviour(gen_server).
-compile([no_auto_import]).

-export([start_link/1, increment/1, get_value/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
```

**Naming:**
- `snake_case` for modules, functions, and variables
- Prefix unused variables with underscore: `_Var`
- Module names should be descriptive: `beamtalk_actor`, not `actor`

**Attributes:**
- Always include `-module(name).` and `-export([...]).`
- Add `-behaviour(gen_server).` for OTP behaviors
- Include `-compile([no_auto_import]).` to avoid import conflicts

**Documentation:**
- Use EDoc format: `%%% @doc`, `%%% @param`, `%%% @returns`
- Document the module purpose at the top
- Document public API functions

---

## Core Erlang Generation

```erlang
%% Generated Core Erlang should be readable
module 'my_module' ['main'/0]
  attributes []

'main'/0 = fun () ->
    call 'erlang':'+'(1, 2)
```

**Requirements:**
- Use fully qualified calls: `'erlang':'+'` not `+`
- Generate unique variable names to avoid shadowing (use counters: `_cor0`, `_cor1`)
- Preserve source locations in annotations for error messages
- Quote all atoms: `'module_name'`, `'function_name'`

**Variable Generation:**
```erlang
%% Use predictable unique names
let <_cor0> = <expr1> in
let <_cor1> = <expr2> in
call 'erlang':'+'(_cor0, _cor1)
```

---

## OTP Patterns

### gen_server for Actors

Beamtalk actors map to `gen_server`:

```erlang
%% Actor state structure
#{
  '__class__' => 'Counter',
  '__methods__' => #{
    increment => fun handle_increment/2,
    getValue => fun handle_getValue/2
  },
  %% User-defined state
  value => 0
}
```

**Best practices:**
- Use maps for state (not records) - easier hot code reload
- Store class name in `'__class__'` for reflection
- Store method dispatch table in `'__methods__'`
- Delegate to `beamtalk_actor` module for common behavior

### Supervision Trees

```erlang
%% Always supervise actors
-module(my_sup).
-behaviour(supervisor).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    Children = [
        #{id => worker1, start => {my_worker, start_link, []}}
    ],
    {ok, {SupFlags, Children}}.
```

**Rules:**
- Every long-running process must be supervised
- Use `one_for_one` strategy by default
- Set reasonable restart intensity (5 restarts in 10 seconds)
- Prefer `gen_statem` over deprecated `gen_fsm`

### Message Protocols

```erlang
%% Async message (returns future)
FuturePid = beamtalk_future:new(),
gen_server:cast(ActorPid, {Selector, Args, FuturePid}),
FuturePid

%% Sync message (blocks)
gen_server:call(ActorPid, {Selector, Args})
```

**Best practices:**
- Default to async for actor-to-actor communication
- Use sync only when result is immediately needed
- Always include timeout for sync calls: `gen_server:call(Pid, Msg, 5000)`
- Handle `{noreply, State}` vs `{reply, Result, State}` correctly

---

## BEAM Interop

### Type Mappings

| Beamtalk | Erlang | Notes |
|----------|--------|-------|
| Integer | `integer()` | Arbitrary precision |
| String | `binary()` | UTF-8 encoded |
| Symbol | `atom()` | Lowercase, quoted |
| Block | `fun()` | Anonymous function |
| List | `list()` | Linked list |
| Map | `map()` | Hash map |
| nil | `nil` atom | Not `undefined` |
| true/false | `true`/`false` atoms | |

### Atoms

```erlang
%% Always quote atoms in generated code
'my_atom'
'ClassName'
'method_name'

%% Avoid atom table exhaustion
%% Don't generate atoms from user input!
```

### Binaries (Strings)

```erlang
%% Prefer binaries over lists for strings
<<"hello">>           %% Good - O(1) size, compact
[104,101,108,108,111] %% Bad - O(n) size, slow

%% Binary pattern matching
<<First:8, Rest/binary>> = <<"hello">>
```

### Exception Handling

```erlang
%% Full exception handling
try
    dangerous_operation()
catch
    error:Reason:Stacktrace ->
        %% Handle errors (like throw in other languages)
        log_error(Reason, Stacktrace);
    throw:Value ->
        %% Handle throws (non-local returns)
        Value;
    exit:Reason ->
        %% Handle exits (process termination)
        {error, Reason}
end
```

---

## Performance Guidelines

### Process Design

```erlang
%% Good: Small, focused processes
%% Each actor handles one responsibility

%% Bad: Monolithic process with huge state
%% Leads to GC pauses and message queue buildup
```

**Rules:**
- Keep process state small (< 1MB ideally)
- Avoid process dictionary (`put`/`get`) - makes debugging hard
- Monitor message queue length in production
- Use `process_info(self(), message_queue_len)` for debugging

### Memory

```erlang
%% Prefer binaries > 64 bytes (stored on binary heap, shared)
LargeBinary = <<"this is more than 64 bytes...">>

%% Small binaries are copied (< 64 bytes)
SmallBinary = <<"hi">>

%% Use binary:copy/1 to force a copy when holding reference to large binary
Substr = binary:copy(binary:part(LargeBin, 0, 100))
```

### Tail Recursion

```erlang
%% Good: Tail recursive (constant stack)
loop(State) ->
    NewState = process(State),
    loop(NewState).

%% Bad: Body recursive (grows stack)
factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

%% Good: Tail recursive with accumulator
factorial(N) -> factorial(N, 1).
factorial(0, Acc) -> Acc;
factorial(N, Acc) -> factorial(N - 1, N * Acc).
```

---

## Testing Generated Code

### EUnit Tests

```erlang
-module(my_module_tests).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    ?assertEqual(42, my_module:answer()).

setup_test_() ->
    {setup,
     fun() -> my_module:start() end,      %% Setup
     fun(_) -> my_module:stop() end,      %% Cleanup
     fun(_) ->
         [?_assertEqual(ok, my_module:do_thing())]
     end}.
```

### Running Tests

```bash
# Run all tests
rebar3 eunit

# Run specific module tests
rebar3 eunit --module=beamtalk_actor_tests

# Run with coverage
rebar3 eunit --cover
rebar3 cover

# Quick test from command line
erl -noshell -eval 'my_module:test(), halt().'
```

### Testing OTP Behaviors

```erlang
%% Test gen_server lifecycle
gen_server_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = my_server:start_link([]),
         Pid
     end,
     fun(Pid) ->
         gen_server:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_assertEqual(ok, my_server:do_thing(Pid)),
          ?_assertEqual({error, invalid}, my_server:bad_thing(Pid))
         ]
     end}.
```

### Common Test Patterns

```erlang
%% Test message queue doesn't grow
no_message_leak_test() ->
    {ok, Pid} = my_server:start_link([]),
    [my_server:async_call(Pid) || _ <- lists:seq(1, 1000)],
    timer:sleep(100),
    {message_queue_len, Len} = process_info(Pid, message_queue_len),
    ?assertEqual(0, Len),
    gen_server:stop(Pid).

%% Test supervision restart
supervisor_restart_test() ->
    {ok, SupPid} = my_sup:start_link(),
    [{_, ChildPid, _, _}] = supervisor:which_children(SupPid),
    exit(ChildPid, kill),
    timer:sleep(100),
    [{_, NewChildPid, _, _}] = supervisor:which_children(SupPid),
    ?assertNotEqual(ChildPid, NewChildPid).
```

---

## Dialyzer

Run Dialyzer for static type checking:

```bash
# Build PLT (first time)
dialyzer --build_plt --apps erts kernel stdlib

# Check project
rebar3 dialyzer
```

**Type specs for generated code:**
```erlang
-spec increment(pid()) -> ok.
increment(Pid) ->
    gen_server:cast(Pid, increment).

-spec get_value(pid()) -> integer().
get_value(Pid) ->
    gen_server:call(Pid, get_value).
```
