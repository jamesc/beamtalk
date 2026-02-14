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

---

## Error Handling - CRITICAL

**NO bare tuple errors EVER!** All errors MUST use the structured `#beamtalk_error{}` system.

### Structured Error Records

All Beamtalk errors use `#beamtalk_error{}` records from `runtime/include/beamtalk.hrl`:

```erlang
-include("beamtalk.hrl").

%% Creating errors in runtime Erlang code
Error0 = beamtalk_error:new(does_not_understand, 'Integer'),
Error1 = beamtalk_error:with_selector(Error0, 'foo'),
Error2 = beamtalk_error:with_hint(Error1, <<"Check spelling">>),
Error3 = beamtalk_error:with_details(Error2, #{arity => 0}),
error(Error3).
```

### Generated Core Erlang Errors

When generating Core Erlang, ALWAYS use `beamtalk_error` module calls:

```erlang
%% ❌ WRONG - bare tuple errors are FORBIDDEN
call 'erlang':'error'({'some_error', 'message'})
call 'erlang':'error'('simple_atom')

%% ✅ RIGHT - structured error
let Error0 = call 'beamtalk_error':'new'('instantiation_error', 'Actor') in
let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'new') in
let Error2 = call 'beamtalk_error':'with_hint'(Error1, <<"Use spawn instead">>) in
call 'erlang':'error'(Error2)
```

### Error Kinds

| Kind | When | Class Example |
|------|------|---------------|
| `does_not_understand` | Unknown method | `'Integer'`, `'Counter'` |
| `immutable_value` | Mutation attempt on primitive | `'Integer'`, `'String'` |
| `type_error` | Wrong argument type | Any class |
| `arity_mismatch` | Wrong argument count | Any class |
| `instantiation_error` | Wrong instantiation method | `'Actor'` |
| `future_not_awaited` | Message sent to Future | `'Future'` |
| `timeout` | Operation timeout | Any class |

### Helper for Binary Strings in Core Erlang

When generating hints in Core Erlang, use the binary literal format:

```erlang
%% Binary "Use spawn instead" in Core Erlang
#{#<85>(8,1,'integer',['unsigned'|['big']]),
  #<115>(8,1,'integer',['unsigned'|['big']]),
  %% ... one entry per character
  #<100>(8,1,'integer',['unsigned'|['big']])}#
```

Or use a helper function in the codegen to generate this automatically.

### Benefits

1. **Consistency** - All errors have same structure
2. **Tooling** - Can pattern match on `kind`, `class`, `selector`
3. **User Experience** - Hints provide actionable guidance
4. **Debugging** - Details map stores context without breaking format
5. **Future-proof** - Easy to add metadata without breaking changes

### Example Complete Error

```erlang
#beamtalk_error{
    kind = instantiation_error,
    class = 'Actor',
    selector = 'new',
    message = <<"Cannot call 'new' on Actor">>,
    hint = <<"Use spawn instead">>,
    details = #{}
}
```

---

## Logging with OTP Logger

**Use OTP logger macros (`?LOG_*`) for all logging.** Do NOT use `io:format` for diagnostics or `logger:level/2` function calls.

Logger macros automatically include caller location metadata (module, function, arity, line) in every log entry, which is essential for debugging via file logs.

**Every `.erl` file that logs MUST include:**
```erlang
-include_lib("kernel/include/logger.hrl").
```

Place the include after `-behaviour(...)` (if present) or after `-module(...)`.

### Log Levels

| Level | Macro | Use Case | Example |
|-------|-------|----------|---------|
| **debug** | `?LOG_DEBUG(Msg, Meta)` | Detailed diagnostics, JSON parse errors, dispatch traces | Protocol parsing failures |
| **info** | `?LOG_INFO(Msg, Meta)` | Important events | Workspace started, daemon ready |
| **warning** | `?LOG_WARNING(Msg, Meta)` | Recoverable issues | Accept error, failed cleanup |
| **error** | `?LOG_ERROR(Msg, Meta)` | Errors affecting operations | Method not found, timeout |

### Usage Pattern

Always use structured metadata (maps), not format strings:

```erlang
%% ❌ WRONG - old style io:format
io:format(standard_error, "JSON parse failed: ~p~n", [Reason])

%% ❌ WRONG - function calls (no MFA metadata in log output)
logger:debug("JSON parse failed", #{reason => Reason})

%% ✅ RIGHT - logger macros (includes MFA automatically)
?LOG_DEBUG("JSON parse failed", #{
    class => Class,
    reason => Reason,
    stack => lists:sublist(Stack, 3),
    data => Data
})
```

### File Logging (BT-541)

Workspace nodes automatically write logs to `~/.beamtalk/workspaces/{workspace_id}/workspace.log`:
- **All levels** captured (debug and above)
- **Log rotation**: 5 files × 1 MB
- **Format**: `timestamp [level] module:function/arity message`
- **Disable**: Set `BEAMTALK_NO_FILE_LOG=1` environment variable
- **Handler**: `logger_std_h` added during workspace supervisor init

### Benefits

1. **MFA in logs** - Every log entry shows which module/function/line emitted it
2. **File logging** - Persistent workspace logs for post-hoc debugging
3. **Clean test output** - Configure logger level in test environment
4. **Structured metadata** - Easy to parse and filter
5. **Flexible output** - Console, file, or custom handlers
6. **Standard OTP** - Follows Erlang best practices
7. **Configurable** - Control via `sys.config` or runtime

### Test Configuration

Configure logger in `test/sys.config` to suppress non-error logs:

```erlang
[
    {kernel, [
        %% Only show errors in tests for clean output
        {logger_level, error},
        {logger, [
            {handler, default, logger_std_h, #{
                level => error,
                formatter => {logger_formatter, #{
                    single_line => true,
                    template => [msg]
                }}
            }}
        ]}
    ]}
].
```

### Production Configuration

For production, use info level with MFA and structured output:

```erlang
[
    {kernel, [
        {logger_level, info},
        {logger, [
            {handler, default, logger_std_h, #{
                level => info,
                formatter => {logger_formatter, #{
                    single_line => true,
                    template => [time, " [", level, "] ", mfa, " ", msg, "\n"]
                }}
            }}
        ]}
    ]}
].
```

### Runtime Control

Enable debug logging at runtime:

```erlang
%% Enable debug level for all modules
logger:set_primary_config(level, debug).

%% Enable debug for specific module
logger:set_module_level(beamtalk_repl_server, debug).

%% Disable debug for specific module
logger:unset_module_level(beamtalk_repl_server).
```

---

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

# Or use Just command
just dialyzer
```

### Type Specs for Generated Code

Always add `-spec` annotations for public functions:

```erlang
-spec increment(pid()) -> ok.
increment(Pid) ->
    gen_server:cast(Pid, increment).

-spec get_value(pid()) -> integer().
get_value(Pid) ->
    gen_server:call(Pid, get_value).
```

### Precise Specs for Custom Types ⚠️

**CRITICAL:** When returning Beamtalk's structured types (`#beamtalk_error{}`, `#beamtalk_object{}`, etc.), always use the **precise type** in specs, not `term()`.

**Why this matters:** Vague specs like `-> term()` won't catch bugs where you accidentally return bare tuples instead of structured records.

```erlang
%% ❌ WRONG - Vague spec, won't catch tuple bugs
-spec build_error(atom(), atom()) -> term().
build_error(Kind, Class) ->
    beamtalk_error:new(Kind, Class).
    %% If someone changes to {error, Kind, Class}, Dialyzer won't catch it!

%% ✅ RIGHT - Precise spec catches mistakes
-spec build_error(atom(), atom()) -> beamtalk_error:error().
build_error(Kind, Class) ->
    beamtalk_error:new(Kind, Class).
    %% Now if someone returns {error, Kind, Class}, Dialyzer fails!
```

**Custom types to use precisely:**

| Record | Type Alias | Use Case |
|--------|------------|----------|
| `#beamtalk_error{}` | `beamtalk_error:error()` | All error construction functions |
| `#beamtalk_object{}` | `#beamtalk_object{}` | Object creation/spawn functions |
| `#class_state{}` | Internal only | Class state in gen_server |

**Example from codebase:**
```erlang
%% All immutable_primitive_error functions
-spec immutable_primitive_error(atom(), term()) -> beamtalk_error:error().
immutable_primitive_error(Class, FieldName) ->
    Error0 = beamtalk_error:new(immutable_primitive, Class),
    Error1 = beamtalk_error:with_selector(Error0, 'instVarAt:put:'),
    beamtalk_error:with_hint(Error1, <<"Use assignment instead">>).
```

**When to be vague:**
- Functions that can return multiple types: `-> term()` is OK
- Internal helper functions with obvious types
- Callbacks that accept any term: `handle_info(term(), State) -> ...`

**When to be precise:**
- ✅ Functions returning custom records (`#beamtalk_error{}`, `#beamtalk_object{}`)
- ✅ Functions with specific error tuples: `-> {ok, Result} | {error, atom()}`
- ✅ Public API functions (always document return type)

---

## Common BEAM Pitfalls ⚠️

**Gotchas that commonly bite developers coming from other languages:**

### 1. Atom Table Exhaustion

**Problem:** Atoms are never garbage collected. Creating atoms from user input can exhaust the atom table (1,048,576 limit).

| ❌ Wrong | ✅ Right | Why |
|---------|---------|-----|
| `list_to_atom(UserInput)` | Use `binary()` for dynamic strings | Atoms never GC'd |
| `binary_to_atom(UserInput, utf8)` | Validate against whitelist first | Can exhaust atom table |
| Creating atoms in loops | Use binaries or existing atoms | Memory leak |

**Safe patterns:**
```erlang
%% ❌ DANGEROUS - creates atoms from user input
handle_call({method, MethodName}, _From, State) ->
    Method = binary_to_atom(MethodName, utf8),  % DANGEROUS!
    dispatch(Method, State).

%% ✅ SAFE - validate against known methods first
handle_call({method, MethodName}, _From, State) ->
    case maps:get(MethodName, maps:get('__methods__', State, #{}), undefined) of
        undefined -> {reply, {error, method_not_found}, State};
        MethodFun -> {reply, MethodFun(State), State}
    end.
```

### 2. String Type Confusion

**Problem:** Erlang has multiple string types. Using the wrong one causes performance issues.

| Type | Syntax | Use Case | Performance |
|------|--------|----------|-------------|
| **List** | `"hello"` | Legacy code, char ops | Slow (linked list) |
| **Binary** | `<<"hello">>` | Modern strings, I/O | Fast (compact) |
| **Atom** | `'hello'` | Constants, tags | Fast (integers) |

**Best practices:**
```erlang
%% ❌ WRONG - lists are slow for strings
String = "hello " ++ "world",  % Slow
Length = length(String),        % O(n)

%% ✅ RIGHT - binaries are fast
String = <<"hello world">>,       % Fast
Length = byte_size(String),       % O(1)

%% Generated Beamtalk code should always use binaries
'my_method'/1 = fun (Self) ->
    <<"Generated string literal">>
end
```

### 3. Process Limits

**Problem:** BEAM has process limits. Default max is 262,144 processes.

**Guidelines:**
- Design for 10K-100K actors in production
- Use pooling for high-volume short-lived operations
- Monitor process count: `erlang:system_info(process_count)`
- Increase limit only after profiling: `erl +P 1000000`

**Anti-patterns to avoid:**
```erlang
%% ❌ BAD - spawn per HTTP request
handle_request(Req) ->
    spawn(fun() -> process_request(Req) end).  % Will exhaust limit

%% ✅ GOOD - use poolboy or similar
handle_request(Req) ->
    poolboy:transaction(worker_pool, fun(Worker) ->
        gen_server:call(Worker, {process, Req})
    end).
```

### 4. Hot Code Loading Complexity

**Problem:** Hot code loading keeps two versions alive simultaneously during reload.

**Key facts:**
- Old version runs until all processes finish with it
- Recursive functions need fully qualified calls to upgrade
- `-on_load` attribute for initialization

**Safe code loading pattern:**
```erlang
%% ❌ RISKY - local call prevents upgrade
loop(State) ->
    receive
        Msg -> loop(handle(Msg, State))  % Stuck on old version!
    end.

%% ✅ SAFE - fully qualified call allows upgrade
loop(State) ->
    receive
        Msg -> ?MODULE:loop(handle(Msg, State))  % Upgrades on next iteration
    end.
```

### 5. Pattern Matching vs Evaluation

**Problem:** Erlang pattern matches, it doesn't evaluate expressions in patterns.

```erlang
%% ❌ WRONG - tries to match variable Value against 42
Value = get_value(),
case Value of
    Value == 42 -> true;  % SYNTAX ERROR!
    _ -> false
end.

%% ✅ RIGHT - pattern match against literal
case get_value() of
    42 -> true;
    _ -> false
end.

%% ✅ RIGHT - use guard for evaluation
case get_value() of
    Value when Value == 42 -> true;
    _ -> false
end.
```

### 6. Variable Single Assignment

**Problem:** Erlang variables are single-assignment (immutable).

```erlang
%% ❌ WRONG - cannot rebind X
X = 5,
X = X + 1.  % ERROR: badmatch

%% ✅ RIGHT - use new variable
X = 5,
X1 = X + 1,
X2 = X1 * 2.

%% Pattern matching allows "rebinding" if value matches
X = 5,
X = 5.  % OK - matches
X = 6.  % ERROR - doesn't match
```

**Codegen strategy:**
```erlang
%% Generate sequential variable names
let <X> = 5 in
let <X1> = call 'erlang':'+'(X, 1) in
let <X2> = call 'erlang':'*'(X1, 2) in
X2
```

### 7. Security: binary_to_term

**Problem:** `binary_to_term/1` can execute arbitrary code and exhaust resources.

```erlang
%% ❌ DANGEROUS - untrusted data
Data = receive_from_network(),
Term = binary_to_term(Data).  % CAN EXECUTE CODE!

%% ✅ SAFER - use safe option (but still risky)
Term = binary_to_term(Data, [safe]).

%% ✅ SAFEST - use explicit parsing
case parse_json(Data) of
    {ok, Parsed} -> Parsed;
    {error, Reason} -> handle_error(Reason)
end.
```

**Never use `binary_to_term/1` on:**
- Network input
- User uploads
- External storage
- Anything not generated by your own code

### 8. Message Queue Unbounded Growth

**Problem:** Process mailboxes can grow unbounded, exhausting memory.

**Protection strategies:**
```erlang
%% ✅ Monitor mailbox size
handle_info(check_mailbox, State) ->
    {message_queue_len, Len} = process_info(self(), message_queue_len),
    case Len > 10000 of
        true -> 
            error_logger:warning_msg("Mailbox overflow: ~p messages", [Len]),
            {noreply, State};
        false -> 
            {noreply, State}
    end.

%% ✅ Use process_flag for hibernation
init([]) ->
    process_flag(message_queue_data, off_heap),  % Reduce GC pressure
    {ok, #state{}}.

%% ✅ Add backpressure
handle_cast({work, Item}, #state{queue = Queue} = State) when length(Queue) > 1000 ->
    {noreply, State};  % Drop work when overloaded
handle_cast({work, Item}, #state{queue = Queue} = State) ->
    {noreply, State#state{queue = [Item | Queue]}}.
```

### 9. Large Binary Memory Leaks

**Problem:** Large binaries (>64 bytes) are reference counted. Holding references prevents GC.

```erlang
%% ❌ LEAKS - keeps entire binary in memory
extract_header(LargeBinary) ->
    <<Header:32/binary, _Rest/binary>> = LargeBinary,
    Header.  % Rest is still in memory via LargeBinary!

%% ✅ BETTER - copy small part
extract_header(LargeBinary) ->
    <<Header:32/binary, _Rest/binary>> = LargeBinary,
    binary:copy(Header).  % Now Rest can be GC'd
```

### 10. ETS Table Leaks

**Problem:** ETS tables aren't garbage collected. Owned by creating process.

```erlang
%% ❌ WRONG - table dies with temporary process
spawn(fun() ->
    Table = ets:new(temp, [public]),
    ets:insert(Table, {key, value})
    %% Process dies, table deleted!
end).

%% ✅ RIGHT - owned by long-lived process
init([]) ->
    Table = ets:new(persistent, [named_table, public, {read_concurrency, true}]),
    {ok, #state{table = Table}}.
```

### Summary Checklist

Before generating Erlang code, verify:

- [ ] No `list_to_atom/1` or `binary_to_atom/2` on untrusted input
- [ ] Use `binary()` for strings, not lists
- [ ] Process spawning is bounded (pooling, limits)
- [ ] Recursive functions use `?MODULE:function` for hot code loading
- [ ] Pattern matching doesn't try to evaluate expressions
- [ ] Variables follow single-assignment (use X, X1, X2...)
- [ ] No `binary_to_term/1` on untrusted data
- [ ] Message queue growth is monitored/bounded
- [ ] Large binaries are copied when extracting small parts
- [ ] ETS tables owned by long-lived processes
