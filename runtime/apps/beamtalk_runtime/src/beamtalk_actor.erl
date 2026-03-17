%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Beamtalk Actor Runtime (gen_server wrapper)
%%%
%%% **DDD Context:** Actor System Context
%%%
%%% Every Beamtalk actor is a BEAM process running a gen_server.
%%% This module provides the actor behavior template and message dispatch.
%%%
%%% ## Actor Lifecycle
%%%
%%% Actors support lifecycle methods:
%%% - `isAlive` - Returns true/false depending on whether the actor process is running
%%% - `monitor` - Creates an Erlang monitor on the actor process
%%% - `stop` - Gracefully stops the actor process
%%%
%%% These methods are handled at the SEND site (via async_send/4 and sync_send/3)
%%% rather than inside the actor, because a dead actor can't process messages.
%%%
%%% WARNING: isAlive check-then-act is inherently racy. The actor could die
%%% between the isAlive check and a subsequent message send. For robust
%%% lifecycle management, use monitors instead of isAlive polling:
%%%
%%% ```
%%% %% Racy pattern (avoid):
%%% counter isAlive ifTrue: [counter increment]
%%%
%%% %% Robust pattern (preferred):
%%% ref := counter monitor
%%% counter increment   %% handle 'DOWN' message if actor dies
%%% ```
%%%
%%% ## Actor State Structure
%%%
%%% Each actor maintains state in a map:
%%% ```erlang
%%% #{
%%%   '$beamtalk_class' => 'Counter',
%%%   '__methods__' => #{
%%%     increment => fun handle_increment/2,
%%%     getValue => fun handle_getValue/2
%%%   },
%%%   %% User-defined state fields
%%%   value => 0
%%% }
%%% ```
%%%
%%% ## Message Protocol
%%%
%%% Generated actors send messages in one of three forms:
%%%
%%% **Async (cast)** - Returns a future:
%%% ```erlang
%%% FuturePid = beamtalk_future:new(),
%%% gen_server:cast(ActorPid, {Selector, Args, FuturePid}),
%%% FuturePid
%%% ```
%%%
%%% **Fire-and-forget (cast)** - No return value:
%%% ```erlang
%%% gen_server:cast(ActorPid, {cast, Selector, Args}),
%%% ok
%%% ```
%%%
%%% **Sync (call)** - Blocks until result:
%%% ```erlang
%%% gen_server:call(ActorPid, {Selector, Args})
%%% ```
%%%
%%% ## Message Dispatch
%%%
%%% The dispatch/4 function looks up the method in the `__methods__` map:
%%% - If found, calls the method function with Args, Self, and State
%%% - If not found, calls doesNotUnderstand handler if defined
%%% - If no doesNotUnderstand handler, returns {error, {unknown_message, Selector}, State}
%%%
%%% ## Spawn Architecture (BT-411, BT-1417)
%%%
%%% There are multiple paths that create actor processes. The `initialize`
%%% hook (if defined) is called inside the generated `init/1` callback,
%%% so it runs for ALL spawn paths — direct, supervised, and named.
%%%
%%% | Path | Entry | Context | Initialize? |
%%% |------|-------|---------|-------------|
%%% | Module:spawn/0,1 | gen_server:start_link → init/1 | Batch/tests | Yes |
%%% | REPL spawn | Module:spawn/0,1 + register_spawned/4 | REPL | Yes |
%%% | class_send → spawn | erlang:apply(Module, spawn, Args) | Runtime | Yes |
%%% | Supervisor child | start_link/1 → gen_server:start_link → init/1 | Supervised | Yes |
%%% | dynamic_object | gen_server:start_link(?MODULE, ...) | Internal | No (by design) |
%%%
%%% Key invariant: `initialize` dispatch lives in generated `init/1`, not
%%% in `spawn/0,1`. This ensures supervised children also run initialize.
%%% The `register_spawned/4` function handles REPL actor registry
%%% integration as a separate concern after spawn completes.
%%%
%%% ## Code Hot Reload
%%%
%%% The code_change/3 callback supports state migration during hot reload.
%%% By default, it preserves existing state. Generated actors can override
%%% this to migrate state schemas.
%%%
%%% ## Example Generated Actor
%%%
%%% ```erlang
%%% -module(beamtalk_counter).
%%% -behaviour(gen_server).
%%%
%%% %% Public API
%%% start_link(Args) ->
%%%     beamtalk_actor:start_link(?MODULE, Args).
%%%
%%% %% Callbacks delegated to beamtalk_actor
%%% init(Args) ->
%%%     beamtalk_actor:init(#{
%%%         '$beamtalk_class' => 'Counter',
%%%         '__methods__' => #{
%%%             increment => fun handle_increment/2,
%%%             getValue => fun handle_getValue/2
%%%         },
%%%         value => proplists:get_value(initial, Args, 0)
%%%     }).
%%%
%%% handle_cast(Msg, State) ->
%%%     beamtalk_actor:handle_cast(Msg, State).
%%%
%%% handle_call(Msg, From, State) ->
%%%     beamtalk_actor:handle_call(Msg, From, State).
%%%
%%% handle_info(Msg, State) ->
%%%     beamtalk_actor:handle_info(Msg, State).
%%%
%%% code_change(OldVsn, State, Extra) ->
%%%     beamtalk_actor:code_change(OldVsn, State, Extra).
%%%
%%% terminate(Reason, State) ->
%%%     beamtalk_actor:terminate(Reason, State).
%%%
%%% %% Method implementations
%%% handle_increment([], State) ->
%%%     Value = maps:get(value, State),
%%%     NewValue = Value + 1,
%%%     NewState = maps:put(value, NewValue, State),
%%%     {noreply, NewState}.
%%%
%%% handle_getValue([], State) ->
%%%     Value = maps:get(value, State),
%%%     {reply, Value, State}.
%%% ```

-module(beamtalk_actor).
-behaviour(gen_server).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% Public API
-export([start_link/2, start_link/3, start_link_supervised/3, register_spawned/4]).

%% Message send helpers (lifecycle-aware wrappers)
-export([async_send/4, sync_send/3, cast_send/3]).

%% gen_server callbacks (for generated actors to delegate to)
-export([
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    code_change/3,
    terminate/2
]).

%% Internal dispatch
-export([dispatch/4, make_self/1]).

%%% Public API

%% @doc Start an actor as part of a supervision tree.
%% Module should implement the beamtalk actor pattern.
%% Args are passed to the module's init/1 callback.
-spec start_link(module(), term()) -> {ok, pid()} | {error, term()}.
start_link(Module, Args) ->
    gen_server:start_link(Module, Args, []).

%% @doc Start a registered actor as part of a supervision tree.
%% Name can be {local, Name}, {global, GlobalName}, or {via, Module, ViaName}.
-spec start_link(term(), module(), term()) -> {ok, pid()} | {error, term()}.
start_link(Name, Module, Args) ->
    gen_server:start_link(Name, Module, Args, []).

%% @doc Start an actor under the workspace actor supervisor.
%% This is used by beamtalk_actor_sup with simple_one_for_one strategy.
%% Module:Function should spawn the actor and return {ok, Pid}.
-spec start_link_supervised(module(), atom(), list()) -> {ok, pid()} | {error, term()}.
start_link_supervised(Module, Function, Args) ->
    erlang:apply(Module, Function, Args).

%% @doc Register an already-spawned actor with the REPL actor registry.
%% Called by generated REPL code after Module:spawn() returns.
%% This separates spawn lifecycle (handled by Module:spawn) from
%% REPL tracking (handled here).
%%
%% Replaced the former spawn_with_registry/3,4 functions which both
%% spawned and registered actors. BT-1417: initialize now runs inside
%% init/1, so all spawn paths (including supervised) call it automatically.
-spec register_spawned(pid(), pid(), atom(), module()) -> ok | {error, term()}.
register_spawned(RegistryPid, ActorPid, ClassName, Module) ->
    case application:get_env(beamtalk_runtime, actor_spawn_callback) of
        {ok, CallbackMod} ->
            try
                CallbackMod:on_actor_spawned(RegistryPid, ActorPid, ClassName, Module)
            catch
                error:undef ->
                    %% Callback module doesn't implement on_actor_spawned/4
                    ?LOG_WARNING("Actor spawn callback not implemented", #{
                        callback => CallbackMod,
                        registry_pid => RegistryPid,
                        actor_pid => ActorPid,
                        class => ClassName,
                        domain => [beamtalk, runtime]
                    }),
                    {error, {callback_undef, CallbackMod}};
                error:#beamtalk_error{} = BtError ->
                    %% Structured beamtalk error from callback
                    ?LOG_ERROR("Actor spawn callback failed with beamtalk error", #{
                        callback => CallbackMod,
                        error => BtError,
                        registry_pid => RegistryPid,
                        actor_pid => ActorPid,
                        class => ClassName,
                        domain => [beamtalk, runtime]
                    }),
                    {error, {beamtalk_error, BtError}};
                Kind:Reason ->
                    %% Unexpected failure in callback
                    ?LOG_ERROR("Actor spawn callback failed", #{
                        callback => CallbackMod,
                        kind => Kind,
                        reason => Reason,
                        registry_pid => RegistryPid,
                        actor_pid => ActorPid,
                        class => ClassName,
                        domain => [beamtalk, runtime]
                    }),
                    {error, {Kind, Reason}}
            end;
        undefined ->
            ok
    end.

%%% Message Send Helpers
%%%
%%% These functions wrap gen_server:cast/call with lifecycle-aware behavior:
%%% - `isAlive`, `monitor`, and `stop` are handled locally (no gen_server message needed)
%%% - Dead actor detection rejects futures / returns errors immediately
%%%
%%% WARNING: Race condition! is_process_alive/1 is a snapshot check.
%%% The actor could die between the alive check and the gen_server:cast.
%%% For robust lifecycle management, use monitors instead of isAlive polling.

%% @doc Send an asynchronous message to an actor, with lifecycle handling.
%%
%% Handles lifecycle methods locally without involving the actor process:
%% - `isAlive` - checks if process is alive, resolves Future with boolean
%% - `monitor` - creates a monitor reference, resolves Future with ref
%% - `stop` - gracefully stops the actor process, resolves Future with ok
%%
%% For all other messages, checks if the actor is alive first:
%% - If alive, sends via gen_server:cast (normal async path)
%% - If dead, rejects the Future with an `actor_dead` error
-spec async_send(pid(), atom(), list(), pid()) -> ok.
async_send(ActorPid, isAlive, [], FuturePid) ->
    %% isAlive is handled locally - no message to the actor
    Result = is_process_alive(ActorPid),
    beamtalk_future:resolve(FuturePid, Result),
    ok;
async_send(ActorPid, stop, [], FuturePid) ->
    %% stop is handled locally - gracefully stops the actor process
    try
        gen_server:stop(ActorPid, normal, 5000),
        beamtalk_future:resolve(FuturePid, ok)
    catch
        exit:noproc ->
            %% Actor already stopped (bare atom exit) - treat as successful stop
            beamtalk_future:resolve(FuturePid, ok);
        exit:{noproc, _} ->
            %% Actor already stopped (tuple exit) - treat as successful stop
            beamtalk_future:resolve(FuturePid, ok);
        exit:Reason ->
            %% Other stop failures (e.g., timeout) - reject Future deterministically
            Error = beamtalk_error:new(
                actor_dead,
                unknown,
                stop,
                iolist_to_binary(io_lib:format("Actor stop failed: ~p", [Reason]))
            ),
            beamtalk_future:reject(FuturePid, Error)
    end,
    ok;
async_send(ActorPid, kill, [], FuturePid) ->
    %% kill is handled locally - forcefully kills the actor process.
    %% Monitor BEFORE sending the kill signal to close the TOCTOU window:
    %% if we resolved the future immediately after exit/2 (which is async),
    %% a caller doing `actor kill. actor isAlive` could still see true because
    %% the BEAM scheduler had not yet processed the kill signal.
    %% Waiting for the DOWN message guarantees the process is gone before ok
    %% is delivered.  If the process is already dead, the DOWN arrives instantly.
    Ref = erlang:monitor(process, ActorPid),
    exit(ActorPid, kill),
    case
        receive
            {'DOWN', Ref, process, ActorPid, _Reason} -> ok
        after 5000 ->
            erlang:demonitor(Ref, [flush]),
            timeout
        end
    of
        ok ->
            beamtalk_future:resolve(FuturePid, ok);
        timeout ->
            %% We did not observe a DOWN within 5 s; cannot guarantee the actor
            %% is gone, so reject the future rather than silently claiming success.
            Error = beamtalk_error:new(
                timeout,
                unknown,
                kill,
                <<"Actor kill timed out: did not receive DOWN within 5000ms">>
            ),
            beamtalk_future:reject(FuturePid, Error)
    end,
    ok;
async_send(_ActorPid, delegate, [], FuturePid) ->
    %% BT-1208: Non-native Actors do not have a backing Erlang module.
    Error = beamtalk_error:new(signal, unknown, delegate),
    Error1 = Error#beamtalk_error{
        message = <<"delegate called on a non-native Actor">>
    },
    beamtalk_future:reject(FuturePid, Error1),
    ok;
async_send(ActorPid, monitor, [], FuturePid) ->
    %% monitor is handled locally - creates an Erlang monitor
    Ref = erlang:monitor(process, ActorPid),
    beamtalk_future:resolve(FuturePid, Ref),
    ok;
async_send(ActorPid, Selector, Args, FuturePid) ->
    %% BT-886: Check liveness before sending, and spawn a watcher to detect
    %% actor death during message processing. The watcher monitors both the
    %% actor and future processes: if the actor dies before the future is
    %% resolved, the watcher rejects the future with a structured error.
    case is_process_alive(ActorPid) of
        true ->
            gen_server:cast(ActorPid, {Selector, Args, FuturePid}),
            spawn_future_watcher(ActorPid, FuturePid, Selector),
            ok;
        false ->
            {error, Error} = actor_dead_error(Selector),
            beamtalk_future:reject(FuturePid, Error),
            ok
    end.

%% @doc Send a fire-and-forget message to an actor (no future, no return value).
%%
%% Checks if the actor is alive before sending. If dead, silently returns ok
%% (fire-and-forget semantics — the caller does not expect a reply).
%%
%% WARNING: Race condition! is_process_alive/1 is a snapshot check.
%% The actor could die between the alive check and the gen_server:cast.
-spec cast_send(pid(), atom(), list()) -> ok.
cast_send(ActorPid, Selector, Args) ->
    case is_process_alive(ActorPid) of
        true ->
            gen_server:cast(ActorPid, {cast, Selector, Args}),
            ok;
        false ->
            ok
    end.

%% @doc Send a synchronous message to an actor, with lifecycle handling.
%%
%% Handles lifecycle methods locally without involving the actor process:
%% - `isAlive` - checks if process is alive, returns boolean
%% - `monitor` - creates a monitor reference, returns ref
%% - `stop` - gracefully stops the actor process, returns ok
%%
%% For all other messages, checks if the actor is alive first:
%% - If alive, sends via gen_server:call and unwraps the result
%% - If dead, raises `#beamtalk_error{kind = actor_dead}`
%% - If timeout, raises `#beamtalk_error{kind = timeout}`
-spec sync_send(pid(), atom(), list()) -> term().
sync_send(ActorPid, isAlive, []) ->
    is_process_alive(ActorPid);
sync_send(ActorPid, stop, []) ->
    %% stop is handled locally - gracefully stops the actor process
    try
        gen_server:stop(ActorPid, normal, 5000)
    catch
        exit:noproc ->
            %% Idempotent: actor already stopped (bare atom exit)
            ok;
        exit:{noproc, _} ->
            %% Idempotent: actor already stopped (tuple exit)
            ok;
        exit:_Other ->
            %% Preserve sync_send/3 contract: translate exits to structured errors
            actor_dead_error(stop)
    end;
sync_send(ActorPid, kill, []) ->
    %% kill is handled locally - forcefully kills the actor process.
    %% Same monitor-before-kill pattern as async_send/4: wait for the DOWN
    %% message so that is_process_alive returns false immediately after kill.
    Ref = erlang:monitor(process, ActorPid),
    exit(ActorPid, kill),
    case
        receive
            {'DOWN', Ref, process, ActorPid, _Reason} -> ok
        after 5000 ->
            erlang:demonitor(Ref, [flush]),
            timeout
        end
    of
        ok -> ok;
        timeout -> raise_timeout(kill)
    end;
sync_send(_ActorPid, delegate, []) ->
    %% BT-1208: Non-native Actors do not have a backing Erlang module.
    %% Native Actors will override this at the codegen level.
    Error = beamtalk_error:new(signal, unknown, delegate),
    Error1 = Error#beamtalk_error{
        message = <<"delegate called on a non-native Actor">>
    },
    error(beamtalk_exception_handler:ensure_wrapped(Error1));
sync_send(ActorPid, monitor, []) ->
    erlang:monitor(process, ActorPid);
sync_send(ActorPid, Selector, Args) ->
    case is_process_alive(ActorPid) of
        true ->
            try
                %% BT-918: Generated handle_call/3 wraps replies as {ok, Result} or {error, Error}.
                %% Unwrap here so callers receive the value directly.
                %%
                %% The {error, Error} case has two sub-forms due to the safe_dispatch layer:
                %%   1. Error = #beamtalk_error{} — returned by dispatch_user_method try/catch
                %%   2. Error = {ErlType, Value} — raw Erlang exception caught by safe_dispatch
                %% We re-raise both forms as Erlang exceptions so the caller sees them correctly.
                case gen_server:call(ActorPid, {Selector, Args}) of
                    {ok, Result} ->
                        Result;
                    {error, {_ErlType, ErrorValue}} ->
                        %% safe_dispatch caught an Erlang exception; re-raise the actual value
                        error(beamtalk_exception_handler:ensure_wrapped(ErrorValue));
                    {error, Error} ->
                        error(beamtalk_exception_handler:ensure_wrapped(Error));
                    DirectValue ->
                        %% Backward compat: actors using beamtalk_actor:handle_call/3 directly
                        %% (rather than the generated handle_call) return values unwrapped.
                        DirectValue
                end
            catch
                exit:{noproc, _} ->
                    raise_actor_dead(Selector);
                exit:{normal, _} ->
                    raise_actor_dead(Selector);
                exit:{shutdown, _} ->
                    raise_actor_dead(Selector);
                exit:{timeout, _} ->
                    raise_timeout(Selector);
                exit:{_Reason, _} ->
                    %% Catch-all for other exit reasons: {shutdown, Term}, killed,
                    %% custom stop reasons, etc. All indicate the actor is unavailable.
                    raise_actor_dead(Selector)
            end;
        false ->
            raise_actor_dead(Selector)
    end.

%% @private
%% @doc Raise a structured actor_dead error as an Erlang exception.
%%
%% Used by sync_send/3 to signal that the actor process is not available.
%% The caller will see a beamtalk_error{kind = actor_dead} exception.
-spec raise_actor_dead(atom()) -> no_return().
raise_actor_dead(Selector) ->
    Error = beamtalk_error:new(
        actor_dead,
        unknown,
        Selector,
        <<"Use 'isAlive' to check, or use monitors for lifecycle events">>
    ),
    error(beamtalk_exception_handler:ensure_wrapped(Error)).

%% @private
%% @doc Raise a structured timeout error as an Erlang exception.
%%
%% Used by sync_send/3 when gen_server:call exceeds the timeout (default 5000ms).
%% A timeout means the actor didn't respond in time, NOT that it's dead —
%% it may be slow, overloaded, or deadlocked.
-spec raise_timeout(atom()) -> no_return().
raise_timeout(Selector) ->
    Error = beamtalk_error:new(
        timeout,
        unknown,
        Selector,
        <<
            "Actor did not respond within the timeout period. "
            "The actor may be slow, overloaded, or deadlocked"
        >>
    ),
    error(beamtalk_exception_handler:ensure_wrapped(Error)).

%% @private
%% @doc Construct a structured actor_dead error for the given selector.
-spec actor_dead_error(atom()) -> {error, #beamtalk_error{}}.
actor_dead_error(Selector) ->
    Error = beamtalk_error:new(
        actor_dead,
        unknown,
        Selector,
        <<"Use 'isAlive' to check, or use monitors for lifecycle events">>
    ),
    {error, Error}.

%% @doc Spawn a lightweight watcher that monitors both the actor and future.
%% BT-886: Closes the TOCTOU race in async_send — if the actor dies during
%% message processing (after the cast but before the future is resolved),
%% the watcher rejects the future with a structured actor_dead error.
%% The watcher also monitors the future process so it can clean up promptly
%% when the future completes normally. Times out after 30s as a safety net.
-spec spawn_future_watcher(pid(), pid(), atom()) -> pid().
spawn_future_watcher(ActorPid, FuturePid, Selector) ->
    spawn(fun() ->
        ActorRef = erlang:monitor(process, ActorPid),
        FutureRef = erlang:monitor(process, FuturePid),
        receive
            {'DOWN', ActorRef, process, ActorPid, _Reason} ->
                %% Actor died — reject future (no-op if already resolved)
                {error, Error} = actor_dead_error(Selector),
                beamtalk_future:reject(FuturePid, Error),
                erlang:demonitor(FutureRef, [flush]);
            {'DOWN', FutureRef, process, FuturePid, _Reason} ->
                %% Future completed and its process ended — clean up
                erlang:demonitor(ActorRef, [flush])
        after 30000 ->
            %% Safety cleanup: stop watching after 30s
            erlang:demonitor(ActorRef, [flush]),
            erlang:demonitor(FutureRef, [flush])
        end
    end).

%%% gen_server callbacks

%% @doc Initialize actor with state map.
%% State must be a map containing '$beamtalk_class' and '__methods__' keys.
-spec init(map()) -> {ok, map()} | {stop, term()}.
init(State) when is_map(State) ->
    logger:set_process_metadata(#{domain => [beamtalk, runtime]}),
    %% Validate required keys
    ClassKey = beamtalk_tagged_map:class_key(),
    case maps:find(ClassKey, State) of
        {ok, Class} when is_atom(Class) ->
            case maps:is_key('__methods__', State) of
                true ->
                    StateKeys = [
                        K
                     || K <- maps:keys(State),
                        K =/= '__methods__',
                        K =/= ClassKey,
                        K =/= '__class_mod__'
                    ],
                    ?LOG_INFO("Actor started", #{
                        class => Class,
                        pid => self(),
                        state_keys => StateKeys,
                        domain => [beamtalk, runtime]
                    }),
                    {ok, State};
                false ->
                    {stop, {missing_key, '__methods__'}}
            end;
        {ok, _NonAtom} ->
            {stop, {invalid_value, ClassKey}};
        error ->
            {stop, {missing_key, ClassKey}}
    end;
init(_NonMapState) ->
    {stop, {invalid_state, not_a_map}}.

%% @doc Handle asynchronous messages (cast).
%% Accepts two wire formats:
%%   {cast, Selector, Args}       - Fire-and-forget (no future, result discarded)
%%   {Selector, Args, FuturePid}  - Async with future (backward-compatible)
%% Errors in fire-and-forget are logged but do not crash the actor.
%% Errors in async-with-future are communicated via future rejection.
-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast({cast, Selector, Args}, State) when is_atom(Selector), is_list(Args) ->
    Self = make_self(State),
    ?LOG_DEBUG("Actor dispatch (cast fire-and-forget)", #{
        class => beamtalk_tagged_map:class_of(State, unknown),
        selector => Selector,
        mode => cast,
        domain => [beamtalk, runtime]
    }),
    T0 = erlang:monotonic_time(microsecond),
    case dispatch(Selector, Args, Self, State) of
        {reply, _Result, NewState} ->
            %% Fire-and-forget: result is discarded
            log_dispatch_complete(State, NewState, Selector, cast, T0),
            {noreply, NewState};
        {noreply, NewState} ->
            log_dispatch_complete(State, NewState, Selector, cast, T0),
            {noreply, NewState};
        {error, Reason, NewState} ->
            %% Log error but don't crash — caller expects no reply
            T1 = erlang:monotonic_time(microsecond),
            ?LOG_WARNING("Fire-and-forget dispatch error", #{
                selector => Selector,
                reason => Reason,
                duration_us => T1 - T0,
                domain => [beamtalk, runtime]
            }),
            {noreply, NewState}
    end;
handle_cast({Selector, Args, FuturePid}, State) ->
    Self = make_self(State),
    ?LOG_DEBUG("Actor dispatch (async)", #{
        class => beamtalk_tagged_map:class_of(State, unknown),
        selector => Selector,
        caller_pid => FuturePid,
        mode => async,
        domain => [beamtalk, runtime]
    }),
    T0 = erlang:monotonic_time(microsecond),
    case dispatch(Selector, Args, Self, State) of
        {reply, Result, NewState} ->
            %% Resolve the future with the result
            log_dispatch_complete(State, NewState, Selector, async, T0),
            beamtalk_future:resolve(FuturePid, Result),
            {noreply, NewState};
        {noreply, NewState} ->
            %% Method didn't return a value, resolve with nil
            log_dispatch_complete(State, NewState, Selector, async, T0),
            beamtalk_future:resolve(FuturePid, nil),
            {noreply, NewState};
        {error, Reason, NewState} ->
            %% Method failed, reject the future
            log_dispatch_complete(State, NewState, Selector, async, T0),
            beamtalk_future:reject(FuturePid, Reason),
            {noreply, NewState}
    end;
handle_cast(Msg, State) ->
    %% Unknown cast message format - log and ignore
    ?LOG_WARNING("Unknown cast message", #{message => Msg, domain => [beamtalk, runtime]}),
    {noreply, State}.

%% @doc Handle synchronous messages (call).
%% Message format: {Selector, Args}
%% Dispatches to method and returns result immediately.
-spec handle_call(term(), term(), map()) -> {reply, term(), map()}.
handle_call({Selector, Args}, From, State) ->
    Self = make_self(State),
    ?LOG_DEBUG("Actor dispatch (sync)", #{
        class => beamtalk_tagged_map:class_of(State, unknown),
        selector => Selector,
        caller_pid => element(1, From),
        mode => sync,
        domain => [beamtalk, runtime]
    }),
    T0 = erlang:monotonic_time(microsecond),
    case dispatch(Selector, Args, Self, State) of
        {reply, Result, NewState} ->
            log_dispatch_complete(State, NewState, Selector, sync, T0),
            {reply, Result, NewState};
        {noreply, NewState} ->
            %% Method didn't return a value, return nil
            log_dispatch_complete(State, NewState, Selector, sync, T0),
            {reply, nil, NewState};
        {error, Reason, NewState} ->
            %% Method failed, return error tuple
            log_dispatch_complete(State, NewState, Selector, sync, T0),
            {reply, {error, Reason}, NewState}
    end;
handle_call(Msg, _From, State) ->
    %% Unknown call message format
    ClassName = beamtalk_tagged_map:class_of(State, unknown),
    Error0 = beamtalk_error:new(does_not_understand, ClassName),
    Error1 = beamtalk_error:with_details(Error0, #{raw_message => Msg}),
    Error = beamtalk_error:with_hint(Error1, <<"Expected {Selector, Args} tuple">>),
    {reply, {error, Error}, State}.

%% @doc Handle out-of-band messages (info).
%% By default, unknown messages are ignored.
%% Generated actors can override this to handle custom messages.
-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info(_Msg, State) ->
    %% Ignore unknown info messages by default
    {noreply, State}.

%% @doc Handle hot code reload.
%% By default, preserves existing state unchanged.
%% Generated actors can override this to migrate state schemas.
-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(OldVsn, State, Extra) ->
    beamtalk_hot_reload:code_change(OldVsn, State, Extra).

%% @doc Clean up when actor is stopping.
%% By default, does nothing.
%% Generated actors can override this for cleanup.
-spec terminate(term(), map()) -> ok.
terminate(Reason, State) ->
    Class = beamtalk_tagged_map:class_of(State, unknown),
    ?LOG_INFO("Actor stopped", #{
        class => Class,
        pid => self(),
        reason => Reason,
        domain => [beamtalk, runtime]
    }),
    ok.

%%% Helper Functions

%% @private
%% @doc Log dispatch completion with timing and state mutation info.
-spec log_dispatch_complete(map(), map(), atom(), atom(), integer()) -> ok.
log_dispatch_complete(OldState, NewState, Selector, Mode, T0) ->
    T1 = erlang:monotonic_time(microsecond),
    Duration = T1 - T0,
    ChangedKeys = changed_state_keys(OldState, NewState),
    case ChangedKeys of
        [] ->
            ?LOG_DEBUG("Actor dispatch complete", #{
                selector => Selector,
                mode => Mode,
                duration_us => Duration,
                domain => [beamtalk, runtime]
            });
        _ ->
            ?LOG_DEBUG("Actor dispatch complete", #{
                selector => Selector,
                mode => Mode,
                duration_us => Duration,
                changed_keys => ChangedKeys,
                domain => [beamtalk, runtime]
            })
    end,
    ok.

%% @private
%% @doc Compute which user-visible state keys changed between two state maps.
%% Excludes internal keys (__methods__, $beamtalk_class, __class_mod__).
-spec changed_state_keys(map(), map()) -> [atom()].
changed_state_keys(OldState, NewState) ->
    InternalKeys = ['__methods__', beamtalk_tagged_map:class_key(), '__class_mod__'],
    AllKeys = lists:usort(maps:keys(OldState) ++ maps:keys(NewState)),
    UserKeys = AllKeys -- InternalKeys,
    [
        K
     || K <- UserKeys, maps:get(K, OldState, '__absent__') =/= maps:get(K, NewState, '__absent__')
    ].

%% @doc Construct a Self reference (#beamtalk_object{}) from the actor's state.
%% Self contains class metadata and the actor's pid, enabling reflection
%% and self-sends within methods.
-spec make_self(map()) -> #beamtalk_object{}.
make_self(State) ->
    #beamtalk_object{
        class = beamtalk_tagged_map:class_of(State),
        class_mod = maps:get('__class_mod__', State, undefined),
        pid = self()
    }.

%%% Message Dispatch

%% @doc Dispatch a message to the appropriate method (dispatch/4 version).
%% This is the new signature that passes Self as a separate parameter.
%% Looks up the selector in the __methods__ map and calls the method function.
%% If not found, attempts to call doesNotUnderstand handler.
%% Returns one of:
%%   {reply, Result, NewState} - Method returned a value
%%   {noreply, NewState} - Method didn't return a value
%%   {error, Reason, State} - Method or dispatch failed
-spec dispatch(atom(), list(), #beamtalk_object{}, map()) ->
    {reply, term(), map()} | {noreply, map()} | {error, term(), map()}.
dispatch(Selector, Args, Self, State) ->
    %% BT-427: Actor-specific methods that can't be in Object:
    %% - isAlive: checks process liveness
    %% - perform:/perform:withArguments:: re-dispatches through actor's own dispatch
    %% - respondsTo:: must check __methods__ map AND hierarchy
    %% All other Object methods (describe, inspect, fieldNames, hash, etc.)
    %% are discovered via hierarchy walk to Object.
    case Selector of
        isAlive when Args =:= [] ->
            %% Actor is alive if it's processing this message
            {reply, true, State};
        delegate when Args =:= [] ->
            %% BT-1208: Non-native Actors do not have a backing Erlang module.
            %% This path is reached via perform:/perform:withArguments: which
            %% bypass the send site and re-dispatch through dispatch/4.
            ClassName = beamtalk_tagged_map:class_of(State, unknown),
            Error = beamtalk_error:new(signal, ClassName, delegate),
            Error1 = Error#beamtalk_error{
                message = <<"delegate called on a non-native Actor">>
            },
            {error, Error1, State};
        'respondsTo:' when length(Args) =:= 1 ->
            %% Check user-defined methods first, then actor built-ins, then hierarchy
            [CheckSelector] = Args,
            Methods = maps:get('__methods__', State),
            case maps:is_key(CheckSelector, Methods) of
                true ->
                    {reply, true, State};
                false when CheckSelector =:= isAlive ->
                    %% isAlive is handled by actor dispatch, not in __methods__
                    {reply, true, State};
                false when CheckSelector =:= stop ->
                    %% stop is handled at send site, not in __methods__
                    {reply, true, State};
                false when CheckSelector =:= monitor ->
                    %% monitor is handled by actor lifecycle machinery, not in __methods__
                    {reply, true, State};
                false when CheckSelector =:= delegate ->
                    %% delegate is handled by actor dispatch, not in __methods__
                    {reply, true, State};
                false ->
                    %% Check inherited methods via hierarchy walk
                    ClassName = beamtalk_tagged_map:class_of(State, unknown),
                    Result =
                        try beamtalk_dispatch:responds_to(CheckSelector, ClassName) of
                            true ->
                                true;
                            false ->
                                %% Class may not be registered — check Object directly
                                beamtalk_object_ops:has_method(CheckSelector)
                        catch
                            _:_ -> beamtalk_object_ops:has_method(CheckSelector)
                        end,
                    {reply, Result, State}
            end;
        'perform:' when length(Args) =:= 1 ->
            %% Dynamic message send: obj perform: #increment => obj increment
            [TargetSelector] = Args,
            case is_atom(TargetSelector) of
                true ->
                    dispatch(TargetSelector, [], Self, State);
                false ->
                    ClassName = beamtalk_tagged_map:class_of(State, unknown),
                    Error = beamtalk_error:new(type_error, ClassName, 'perform:'),
                    {error, Error, State}
            end;
        'perform:withArguments:' when length(Args) =:= 2 ->
            %% Dynamic message send: obj perform: #'at:put:' withArguments: #(1, 'x')
            [TargetSelector, ArgList] = Args,
            case is_atom(TargetSelector) andalso is_list(ArgList) of
                true ->
                    dispatch(TargetSelector, ArgList, Self, State);
                false ->
                    ClassName = beamtalk_tagged_map:class_of(State, unknown),
                    Error = beamtalk_error:new(type_error, ClassName, 'perform:withArguments:'),
                    {error, Error, State}
            end;
        _ ->
            %% Check user-defined methods, then delegate to hierarchy walk
            dispatch_user_method(Selector, Args, Self, State)
    end.

%% @private
%% @doc Dispatch to user-defined methods after built-in checks.
-spec dispatch_user_method(atom(), list(), #beamtalk_object{}, map()) ->
    {reply, term(), map()} | {noreply, map()} | {error, term(), map()}.
dispatch_user_method(Selector, Args, Self, State) ->
    case Selector of
        initialize ->
            ?LOG_DEBUG("Initialize hook invoked", #{
                class => beamtalk_tagged_map:class_of(State, unknown),
                pid => self(),
                domain => [beamtalk, runtime]
            });
        _ ->
            ok
    end,
    Methods = maps:get('__methods__', State),
    case maps:find(Selector, Methods) of
        {ok, Fun} when is_function(Fun, 4) ->
            %% New-style method: Fun(Selector, Args, Self, State)
            try
                Fun(Selector, Args, Self, State)
            catch
                error:#beamtalk_error{} = BtError:_Stacktrace ->
                    %% Preserve structured beamtalk errors from method implementations
                    {error, BtError, State};
                Class:Reason:Stacktrace ->
                    wrap_method_error(Selector, State, Class, Reason, Stacktrace)
            end;
        {ok, Fun} when is_function(Fun, 2) ->
            %% Old-style method: Fun(Args, State) - for backward compatibility
            try
                Fun(Args, State)
            catch
                error:#beamtalk_error{} = BtError:_Stacktrace ->
                    %% Preserve structured beamtalk errors from method implementations
                    {error, BtError, State};
                Class:Reason:Stacktrace ->
                    wrap_method_error(Selector, State, Class, Reason, Stacktrace)
            end;
        {ok, _NotAFunction} ->
            %% Method value is not a function
            ClassName = beamtalk_tagged_map:class_of(State, unknown),
            Error = beamtalk_error:new(type_error, ClassName, Selector),
            {error, Error, State};
        error ->
            %% Method not found - try doesNotUnderstand
            handle_dnu(Selector, Args, Self, State)
    end.

%% @private
%% @doc Handle messages for unknown selectors.
%% Attempts to call the doesNotUnderstand:args: handler if defined.
%% Otherwise, returns an error.
-spec handle_dnu(atom(), list(), #beamtalk_object{}, map()) ->
    {reply, term(), map()} | {noreply, map()} | {error, term(), map()}.
handle_dnu(Selector, Args, Self, State) ->
    Methods = maps:get('__methods__', State),
    case maps:find('doesNotUnderstand:args:', Methods) of
        {ok, DnuFun} when is_function(DnuFun, 3) ->
            call_dnu_handler(DnuFun, [Selector, Args], Self, State, 3);
        {ok, DnuFun} when is_function(DnuFun, 2) ->
            call_dnu_handler(DnuFun, [Selector, Args], Self, State, 2);
        _ ->
            dispatch_via_hierarchy(Selector, Args, Self, State)
    end.

%% @private
%% @doc Call a doesNotUnderstand handler with error wrapping.
-spec call_dnu_handler(function(), list(), #beamtalk_object{}, map(), 2 | 3) ->
    {reply, term(), map()} | {noreply, map()} | {error, term(), map()}.
call_dnu_handler(DnuFun, DnuArgs, Self, State, 3) ->
    try
        DnuFun(DnuArgs, Self, State)
    catch
        error:#beamtalk_error{} = BtError:_ ->
            {error, BtError, State};
        Class:Reason:Stacktrace ->
            wrap_dnu_handler_error(hd(DnuArgs), State, Class, Reason, Stacktrace)
    end;
call_dnu_handler(DnuFun, DnuArgs, _Self, State, 2) ->
    try
        DnuFun(DnuArgs, State)
    catch
        error:#beamtalk_error{} = BtError:_ ->
            {error, BtError, State};
        Class:Reason:Stacktrace ->
            wrap_dnu_handler_error(hd(DnuArgs), State, Class, Reason, Stacktrace)
    end.

%% @private
%% @doc Dispatch via class hierarchy, falling back to Object on any failure.
-spec dispatch_via_hierarchy(atom(), list(), #beamtalk_object{}, map()) ->
    {reply, term(), map()} | {noreply, map()} | {error, term(), map()}.
dispatch_via_hierarchy(Selector, Args, Self, State) ->
    ClassName = beamtalk_tagged_map:class_of(State, unknown),
    try beamtalk_dispatch:lookup(Selector, Args, Self, State, ClassName) of
        {reply, Result, NewState} ->
            {reply, Result, NewState};
        {error, #beamtalk_error{kind = does_not_understand}} ->
            object_fallback(Selector, Args, Self, State, ClassName);
        {error, #beamtalk_error{kind = class_not_found}} ->
            object_fallback(Selector, Args, Self, State, ClassName);
        {error, Error} ->
            {error, Error, State}
    catch
        exit:{noproc, _} ->
            %% Class registry not available (e.g., in unit tests without bootstrap)
            object_fallback(Selector, Args, Self, State, ClassName);
        exit:{normal, _} ->
            %% Class process terminated normally
            object_fallback(Selector, Args, Self, State, ClassName);
        exit:{timeout, _} ->
            ?LOG_WARNING("Class dispatch lookup timed out", #{
                selector => Selector,
                class => ClassName,
                domain => [beamtalk, runtime]
            }),
            object_fallback(Selector, Args, Self, State, ClassName);
        exit:{Reason, _} ->
            ?LOG_WARNING("Class dispatch lookup failed", #{
                selector => Selector,
                class => ClassName,
                reason => Reason,
                domain => [beamtalk, runtime]
            }),
            object_fallback(Selector, Args, Self, State, ClassName)
    end.

%% @private
%% @doc Create a does_not_understand error result.
-spec make_dnu_error(atom(), atom(), map()) -> {error, term(), map()}.
make_dnu_error(Selector, ClassName, State) ->
    ?LOG_WARNING("doesNotUnderstand", #{
        class => ClassName,
        selector => Selector,
        pid => self(),
        domain => [beamtalk, runtime]
    }),
    Error = beamtalk_error:new(
        does_not_understand,
        ClassName,
        Selector,
        <<"Check spelling or use 'respondsTo:' to verify method exists">>
    ),
    {error, Error, State}.

%% @private
%% @doc Wrap method dispatch exceptions as type_error with source exception details.
-spec wrap_method_error(atom(), map(), term(), term(), list()) -> {error, term(), map()}.
wrap_method_error(Selector, State, Class, Reason, Stacktrace) ->
    ClassName = beamtalk_tagged_map:class_of(State, unknown),
    Message = format_method_error_message(ClassName, Selector, Class, Reason, Stacktrace),
    ?LOG_ERROR("Error in method", #{
        selector => Selector,
        class => Class,
        reason => Reason,
        stacktrace => Stacktrace,
        domain => [beamtalk, runtime]
    }),
    Error0 = beamtalk_error:new(runtime_error, ClassName, Selector),
    Error1 = beamtalk_error:with_message(Error0, Message),
    Error = beamtalk_error:with_details(Error1, #{
        original_class => Class,
        original_reason => Reason,
        erlang_stacktrace => Stacktrace
    }),
    {error, Error, State}.

%% @private
%% @doc Format a human-readable error message from a method dispatch failure.
-spec format_method_error_message(atom(), atom(), term(), term(), list()) -> binary().
format_method_error_message(ClassName, Selector, error, function_clause, [{M, F, A, Loc} | _]) ->
    iolist_to_binary(
        io_lib:format(
            "No matching clause in ~s>>~s (called ~s:~s/~s~s)",
            [ClassName, Selector, M, F, format_arity(A), format_location(Loc)]
        )
    );
format_method_error_message(ClassName, Selector, error, badarg, [{M, F, A, Loc} | _]) ->
    iolist_to_binary(
        io_lib:format(
            "Bad argument in ~s>>~s (called ~s:~s/~s~s)",
            [ClassName, Selector, M, F, format_arity(A), format_location(Loc)]
        )
    );
format_method_error_message(ClassName, Selector, error, undef, [{M, F, A, Loc} | _]) ->
    iolist_to_binary(
        io_lib:format(
            "Undefined function in ~s>>~s (called ~s:~s/~s~s)",
            [ClassName, Selector, M, F, format_arity(A), format_location(Loc)]
        )
    );
format_method_error_message(ClassName, Selector, error, badarith, [{M, F, A, Loc} | _]) ->
    iolist_to_binary(
        io_lib:format(
            "Arithmetic error in ~s>>~s (called ~s:~s/~s~s)",
            [ClassName, Selector, M, F, format_arity(A), format_location(Loc)]
        )
    );
format_method_error_message(ClassName, Selector, Class, Reason, _Stacktrace) ->
    iolist_to_binary(
        io_lib:format(
            "~p:~p in ~s>>~s",
            [Class, Reason, ClassName, Selector]
        )
    ).

%% @private
format_arity(A) when is_list(A) -> integer_to_list(length(A));
format_arity(A) when is_integer(A) -> integer_to_list(A).

%% @private
format_location(Loc) when is_list(Loc) ->
    case proplists:get_value(line, Loc) of
        undefined ->
            "";
        Line ->
            case proplists:get_value(file, Loc) of
                undefined -> io_lib:format(" at line ~B", [Line]);
                File -> io_lib:format(" at ~s:~B", [File, Line])
            end
    end.

%% @private
%% @doc Wrap doesNotUnderstand handler exceptions consistently for both arities.
-spec wrap_dnu_handler_error(atom(), map(), term(), term(), list()) -> {error, term(), map()}.
wrap_dnu_handler_error(Selector, State, Class, Reason, Stacktrace) ->
    ClassName = beamtalk_tagged_map:class_of(State, unknown),
    Message = format_method_error_message(ClassName, Selector, Class, Reason, Stacktrace),
    ?LOG_ERROR("Error in doesNotUnderstand handler", #{
        selector => Selector,
        class => Class,
        reason => Reason,
        stacktrace => Stacktrace,
        domain => [beamtalk, runtime]
    }),
    Error0 = beamtalk_error:new(runtime_error, ClassName, 'doesNotUnderstand:args:'),
    Error1 = beamtalk_error:with_message(Error0, Message),
    Error = beamtalk_error:with_details(Error1, #{
        original_class => Class,
        original_reason => Reason,
        erlang_stacktrace => Stacktrace
    }),
    {error, Error, State}.

%% @private
%% @doc Fall back to Object dispatch for inherited base methods.
%% Used when hierarchy walk fails (class not registered) or class registry unavailable.
-spec object_fallback(atom(), list(), #beamtalk_object{}, map(), atom()) ->
    {reply, term(), map()} | {noreply, map()} | {error, term(), map()}.
object_fallback(Selector, Args, Self, State, ClassName) ->
    case beamtalk_object_ops:dispatch(Selector, Args, Self, State) of
        {error, _, _} ->
            make_dnu_error(Selector, ClassName, State);
        Result ->
            Result
    end.
