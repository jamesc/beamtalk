%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Beamtalk Actor Runtime (gen_server wrapper)
%%%
%%% Every Beamtalk actor is a BEAM process running a gen_server.
%%% This module provides the actor behavior template and message dispatch.
%%%
%%% ## Actor Lifecycle
%%%
%%% Actors support lifecycle methods:
%%% - `isAlive` - Returns true/false depending on whether the actor process is running
%%% - `monitor` - Creates an Erlang monitor on the actor process
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
%%% Generated actors send messages in one of two forms:
%%%
%%% **Async (cast)** - Returns a future:
%%% ```erlang
%%% FuturePid = beamtalk_future:new(),
%%% gen_server:cast(ActorPid, {Selector, Args, FuturePid}),
%%% FuturePid
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

%% Public API
-export([start_link/2, start_link/3, start_link_supervised/3, spawn_with_registry/3, spawn_with_registry/4]).

%% Message send helpers (lifecycle-aware wrappers)
-export([async_send/4, sync_send/3]).

%% gen_server callbacks (for generated actors to delegate to)
-export([init/1, handle_cast/2, handle_call/3, handle_info/2,
         code_change/3, terminate/2]).

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

%% @doc Spawn an actor and register it with the REPL actor registry.
%% Returns {ok, Pid} or {error, Reason}.
%% Used by generated code when spawning actors in a REPL context.
-spec spawn_with_registry(pid(), module(), term()) -> {ok, pid()} | {error, term()}.
spawn_with_registry(RegistryPid, Module, Args) ->
    spawn_with_registry(RegistryPid, Module, Args, undefined).

%% @doc Spawn an actor with registry, specifying a class name.
%% ClassName is the Beamtalk class name (e.g., 'Counter'), used for display.
-spec spawn_with_registry(pid(), module(), term(), atom() | undefined) -> {ok, pid()} | {error, term()}.
spawn_with_registry(RegistryPid, Module, Args, ClassName) ->
    %% Add registry info to args for init callback
    ArgsWithRegistry = case is_map(Args) of
        true -> maps:put('__registry_pid__', RegistryPid, Args);
        false -> #{'__registry_pid__' => RegistryPid, '__init_args__' => Args}
    end,
    
    case gen_server:start_link(Module, ArgsWithRegistry, []) of
        {ok, Pid} ->
            %% Determine class name for callbacks
            Class = case ClassName of
                undefined -> Module; % Use module name as fallback
                _ -> ClassName
            end,
            
            %% Notify registered callback (if any) - supports optional REPL/workspace tracking.
            %% The beamtalk_repl app registers this callback on startup.
            %% Guarded with try/catch so actor spawning succeeds even if the
            %% callback module is missing, not loaded, or fails.
            case application:get_env(beamtalk_runtime, actor_spawn_callback) of
                {ok, CallbackMod} ->
                    try
                        CallbackMod:on_actor_spawned(RegistryPid, Pid, Class, Module)
                    catch
                        Kind:Reason ->
                            logger:warning("Actor spawn callback failed", #{
                                callback => CallbackMod,
                                kind => Kind,
                                reason => Reason,
                                actor_pid => Pid,
                                class => Class
                            }),
                            ok
                    end;
                undefined ->
                    ok
            end,
            
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

%%% Message Send Helpers
%%%
%%% These functions wrap gen_server:cast/call with lifecycle-aware behavior:
%%% - `isAlive` and `monitor` are handled locally (no gen_server message needed)
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
async_send(ActorPid, monitor, [], FuturePid) ->
    %% monitor is handled locally - creates an Erlang monitor
    Ref = erlang:monitor(process, ActorPid),
    beamtalk_future:resolve(FuturePid, Ref),
    ok;
async_send(ActorPid, Selector, Args, FuturePid) ->
    %% Monitor the actor to detect death during the send window.
    %% This narrows the TOCTOU race: if the actor dies between the
    %% is_process_alive check and when gen_server processes the cast,
    %% the DOWN message is already in our mailbox and we catch it.
    MonRef = erlang:monitor(process, ActorPid),
    case is_process_alive(ActorPid) of
        true ->
            gen_server:cast(ActorPid, {Selector, Args, FuturePid}),
            %% Non-blocking check: did actor die between cast and now?
            receive
                {'DOWN', MonRef, process, ActorPid, _Reason} ->
                    %% Actor died - reject Future (no-op if already resolved)
                    {error, Error} = actor_dead_error(Selector),
                    beamtalk_future:reject(FuturePid, Error)
            after 0 ->
                erlang:demonitor(MonRef, [flush])
            end,
            ok;
        false ->
            erlang:demonitor(MonRef, [flush]),
            {error, Error} = actor_dead_error(Selector),
            beamtalk_future:reject(FuturePid, Error),
            ok
    end.

%% @doc Send a synchronous message to an actor, with lifecycle handling.
%%
%% Handles lifecycle methods locally without involving the actor process:
%% - `isAlive` - checks if process is alive, returns boolean
%% - `monitor` - creates a monitor reference, returns ref
%%
%% For all other messages, checks if the actor is alive first:
%% - If alive, sends via gen_server:call (normal sync path)
%% - If dead, returns `{error, #beamtalk_error{kind = actor_dead}}`
-spec sync_send(pid(), atom(), list()) -> term().
sync_send(ActorPid, isAlive, []) ->
    is_process_alive(ActorPid);
sync_send(ActorPid, monitor, []) ->
    erlang:monitor(process, ActorPid);
sync_send(ActorPid, Selector, Args) ->
    case is_process_alive(ActorPid) of
        true ->
            try
                gen_server:call(ActorPid, {Selector, Args})
            catch
                exit:{noproc, _} ->
                    actor_dead_error(Selector);
                exit:{normal, _} ->
                    actor_dead_error(Selector);
                exit:{shutdown, _} ->
                    actor_dead_error(Selector)
            end;
        false ->
            actor_dead_error(Selector)
    end.

%% @private
%% @doc Construct a structured actor_dead error for the given selector.
-spec actor_dead_error(atom()) -> {error, #beamtalk_error{}}.
actor_dead_error(Selector) ->
    Error0 = beamtalk_error:new(actor_dead, unknown),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, <<"Use 'isAlive' to check, or use monitors for lifecycle events">>),
    {error, Error2}.

%%% gen_server callbacks

%% @doc Initialize actor with state map.
%% State must be a map containing '$beamtalk_class' and '__methods__' keys.
-spec init(map()) -> {ok, map()} | {stop, term()}.
init(State) when is_map(State) ->
    %% Validate required keys
    case {beamtalk_tagged_map:is_tagged(State), maps:is_key('__methods__', State)} of
        {true, true} ->
            {ok, State};
        {false, _} ->
            {stop, {missing_key, beamtalk_tagged_map:class_key()}};
        {_, false} ->
            {stop, {missing_key, '__methods__'}}
    end;
init(_NonMapState) ->
    {stop, {invalid_state, not_a_map}}.

%% @doc Handle asynchronous messages (cast).
%% Message format: {Selector, Args, FuturePid}
%% Dispatches to method and resolves/rejects the future.
%% Errors are communicated via the future (rejection) rather than crash.
-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast({Selector, Args, FuturePid}, State) ->
    Self = make_self(State),
    case dispatch(Selector, Args, Self, State) of
        {reply, Result, NewState} ->
            %% Resolve the future with the result
            beamtalk_future:resolve(FuturePid, Result),
            {noreply, NewState};
        {noreply, NewState} ->
            %% Method didn't return a value, resolve with nil
            beamtalk_future:resolve(FuturePid, nil),
            {noreply, NewState};
        {error, Reason, NewState} ->
            %% Method failed, reject the future
            beamtalk_future:reject(FuturePid, Reason),
            {noreply, NewState}
    end;
handle_cast(Msg, State) ->
    %% Unknown cast message format - log and ignore
    logger:warning("Unknown cast message", #{message => Msg}),
    {noreply, State}.

%% @doc Handle synchronous messages (call).
%% Message format: {Selector, Args}
%% Dispatches to method and returns result immediately.
-spec handle_call(term(), term(), map()) -> {reply, term(), map()}.
handle_call({Selector, Args}, _From, State) ->
    Self = make_self(State),
    case dispatch(Selector, Args, Self, State) of
        {reply, Result, NewState} ->
            {reply, Result, NewState};
        {noreply, NewState} ->
            %% Method didn't return a value, return nil
            {reply, nil, NewState};
        {error, Reason, NewState} ->
            %% Method failed, return error tuple
            {reply, {error, Reason}, NewState}
    end;
handle_call(Msg, _From, State) ->
    %% Unknown call message format
    {reply, {error, {unknown_call_format, Msg}}, State}.

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
terminate(_Reason, _State) ->
    ok.

%%% Helper Functions

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
    %% First, check for built-in Object reflection methods
    case Selector of
        isAlive when Args =:= [] ->
            %% Actor is alive if it's processing this message
            {reply, true, State};
        respondsTo when length(Args) =:= 1 ->
            %% Check if object responds to a selector
            [CheckSelector] = Args,
            Methods = maps:get('__methods__', State),
            Result = maps:is_key(CheckSelector, Methods),
            {reply, Result, State};
        instVarNames when Args =:= [] ->
            %% Return list of instance variable names (excluding internals)
            {reply, beamtalk_tagged_map:user_field_keys(State), State};
        instVarAt when length(Args) =:= 1 ->
            %% Read instance variable by name
            [Name] = Args,
            Result = maps:get(Name, State, nil),
            {reply, Result, State};
        'instVarAt:put:' ->
            %% Write instance variable by name, returns Self
            case Args of
                [Name, Value] ->
                    NewState = maps:put(Name, Value, State),
                    {reply, Self, NewState};
                _ ->
                    ClassName = beamtalk_tagged_map:class_of(State, unknown),
                    Error0 = beamtalk_error:new(does_not_understand, ClassName),
                    Error1 = beamtalk_error:with_selector(Error0, 'instVarAt:put:'),
                    Error2 = beamtalk_error:with_hint(Error1, <<"Expected 2 arguments: name and value">>),
                    error(Error2)
            end;
        perform ->
            %% Dynamic message send with no arguments
            %% obj perform: #increment  => obj increment
            case Args of
                [TargetSelector] when is_atom(TargetSelector) ->
                    dispatch(TargetSelector, [], Self, State);
                _ ->
                    ClassName = beamtalk_tagged_map:class_of(State, unknown),
                    Error0 = beamtalk_error:new(does_not_understand, ClassName),
                    Error1 = beamtalk_error:with_selector(Error0, perform),
                    Error2 = beamtalk_error:with_hint(Error1, <<"Expected 1 argument: a selector atom">>),
                    error(Error2)
            end;
        'perform:withArgs:' ->
            %% Dynamic message send with argument array
            %% obj perform: #'at:put:' withArgs: [1, 'x']  => obj at: 1 put: 'x'
            case Args of
                [TargetSelector, ArgList] when is_atom(TargetSelector) ->
                    when_list(ArgList,
                        fun() -> dispatch(TargetSelector, ArgList, Self, State) end,
                        fun() ->
                            ClassName = beamtalk_tagged_map:class_of(State, unknown),
                            Error0 = beamtalk_error:new(type_error, ClassName),
                            Error = beamtalk_error:with_selector(Error0, 'perform:withArgs:'),
                            {error, Error, State}
                        end);
                _ ->
                    ClassName = beamtalk_tagged_map:class_of(State, unknown),
                    Error0 = beamtalk_error:new(does_not_understand, ClassName),
                    Error1 = beamtalk_error:with_selector(Error0, 'perform:withArgs:'),
                    Error2 = beamtalk_error:with_hint(Error1, <<"Expected 2 arguments: a selector atom and an argument list">>),
                    error(Error2)
            end;
        _ ->
            %% Not a built-in method, check user-defined methods
            dispatch_user_method(Selector, Args, Self, State)
    end.

%% @private
%% @doc Dispatch to user-defined methods after built-in checks.
-spec dispatch_user_method(atom(), list(), #beamtalk_object{}, map()) ->
    {reply, term(), map()} | {noreply, map()} | {error, term(), map()}.
dispatch_user_method(Selector, Args, Self, State) ->
    Methods = maps:get('__methods__', State),
    case maps:find(Selector, Methods) of
        {ok, Fun} when is_function(Fun, 4) ->
            %% New-style method: Fun(Selector, Args, Self, State)
            try
                Fun(Selector, Args, Self, State)
            catch
                Class:Reason:_Stacktrace ->
                    %% Method threw an exception - log without stack trace to avoid leaking sensitive data
                    logger:error("Error in method", #{
                        selector => Selector,
                        class => Class,
                        reason => Reason
                    }),
                    ClassName = beamtalk_tagged_map:class_of(State, unknown),
                    Error0 = beamtalk_error:new(type_error, ClassName),
                    Error = beamtalk_error:with_selector(Error0, Selector),
                    {error, Error, State}
            end;
        {ok, Fun} when is_function(Fun, 2) ->
            %% Old-style method: Fun(Args, State) - for backward compatibility
            try
                Fun(Args, State)
            catch
                Class:Reason:_Stacktrace ->
                    %% Method threw an exception - log without stack trace to avoid leaking sensitive data
                    logger:error("Error in method", #{
                        selector => Selector,
                        class => Class,
                        reason => Reason
                    }),
                    ClassName = beamtalk_tagged_map:class_of(State, unknown),
                    Error0 = beamtalk_error:new(type_error, ClassName),
                    Error = beamtalk_error:with_selector(Error0, Selector),
                    {error, Error, State}
            end;
        {ok, _NotAFunction} ->
            %% Method value is not a function
            ClassName = beamtalk_tagged_map:class_of(State, unknown),
            Error0 = beamtalk_error:new(type_error, ClassName),
            Error = beamtalk_error:with_selector(Error0, Selector),
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
            %% New-style DNU handler with Self parameter
            %% DNU handlers receive ([Selector, Args], Self, State) - 3 args
            try
                DnuFun([Selector, Args], Self, State)
            catch
                Class:Reason:_Stacktrace ->
                    %% DNU handler threw an exception - log without stack trace to avoid leaking sensitive data
                    logger:error("Error in doesNotUnderstand handler", #{
                        selector => Selector,
                        class => Class,
                        reason => Reason
                    }),
                    ClassName = beamtalk_tagged_map:class_of(State, unknown),
                    Error0 = beamtalk_error:new(type_error, ClassName),
                    Error = beamtalk_error:with_selector(Error0, 'doesNotUnderstand:args:'),
                    {error, Error, State}
            end;
        {ok, DnuFun} when is_function(DnuFun, 2) ->
            %% Old-style DNU handler (backward compatibility)
            try
                DnuFun([Selector, Args], State)
            catch
                Class:Reason:_Stacktrace ->
                    %% DNU handler threw an exception - log without stack trace to avoid leaking sensitive data
                    logger:error("Error in doesNotUnderstand handler", #{
                        selector => Selector,
                        class => Class,
                        reason => Reason
                    }),
                    ClassName = beamtalk_tagged_map:class_of(State, unknown),
                    Error0 = beamtalk_error:new(type_error, ClassName),
                    Error = beamtalk_error:with_selector(Error0, 'doesNotUnderstand:args:'),
                    {error, Error, State}
            end;
        _ ->
            %% No DNU handler - method not found is an error
            ClassName = beamtalk_tagged_map:class_of(State, unknown),
            Error0 = beamtalk_error:new(does_not_understand, ClassName),
            Error1 = beamtalk_error:with_selector(Error0, Selector),
            Error = beamtalk_error:with_hint(Error1, <<"Check spelling or use 'respondsTo:' to verify method exists">>),
            {error, Error, State}
    end.

%% @private
%% @doc Execute ThenFun if Value is a list, else execute ElseFun.
%% Used for type checking in perform:withArgs:
-spec when_list(term(), fun(() -> term()), fun(() -> term())) -> term().
when_list(Value, ThenFun, _ElseFun) when is_list(Value) ->
    ThenFun();
when_list(_Value, _ThenFun, ElseFun) ->
    ElseFun().
