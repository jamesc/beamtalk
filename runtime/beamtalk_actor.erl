%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Beamtalk Actor Runtime (gen_server wrapper)
%%%
%%% Every Beamtalk actor is a BEAM process running a gen_server.
%%% This module provides the actor behavior template and message dispatch.
%%%
%%% ## Actor State Structure
%%%
%%% Each actor maintains state in a map:
%%% ```erlang
%%% #{
%%%   '__class__' => 'Counter',
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
%%% The dispatch/3 function looks up the method in the `__methods__` map:
%%% - If found, calls the method function with Args and State
%%% - If not found, calls doesNotUnderstand handler if defined
%%% - If no doesNotUnderstand handler, crashes with {unknown_message, Selector}
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
%%%         '__class__' => 'Counter',
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

%% Public API
-export([start_link/2, start_link/3, spawn_actor/2, spawn_actor/3]).

%% gen_server callbacks (for generated actors to delegate to)
-export([init/1, handle_cast/2, handle_call/3, handle_info/2,
         code_change/3, terminate/2]).

%% Internal dispatch
-export([dispatch/3]).

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

%% @doc Spawn an actor outside of a supervision tree.
%% Returns the pid directly (no {ok, Pid} tuple).
%% Useful for ad-hoc actor creation in the REPL.
-spec spawn_actor(module(), term()) -> pid().
spawn_actor(Module, Args) ->
    {ok, Pid} = gen_server:start(Module, Args, []),
    Pid.

%% @doc Spawn a registered actor outside of a supervision tree.
-spec spawn_actor(term(), module(), term()) -> pid().
spawn_actor(Name, Module, Args) ->
    {ok, Pid} = gen_server:start(Name, Module, Args, []),
    Pid.

%%% gen_server callbacks

%% @doc Initialize actor with state map.
%% State must be a map containing '__class__' and '__methods__' keys.
-spec init(map()) -> {ok, map()}.
init(State) when is_map(State) ->
    %% Validate required keys
    case {maps:is_key('__class__', State), maps:is_key('__methods__', State)} of
        {true, true} ->
            {ok, State};
        {false, _} ->
            {stop, {missing_key, '__class__'}};
        {_, false} ->
            {stop, {missing_key, '__methods__'}}
    end.

%% @doc Handle asynchronous messages (cast).
%% Message format: {Selector, Args, FuturePid}
%% Dispatches to method and resolves/rejects the future.
-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast({Selector, Args, FuturePid}, State) ->
    case dispatch(Selector, Args, State) of
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
    io:format(standard_error, "Unknown cast message: ~p~n", [Msg]),
    {noreply, State}.

%% @doc Handle synchronous messages (call).
%% Message format: {Selector, Args}
%% Dispatches to method and returns result immediately.
-spec handle_call(term(), term(), map()) -> {reply, term(), map()} | {stop, term(), map()}.
handle_call({Selector, Args}, _From, State) ->
    case dispatch(Selector, Args, State) of
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
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Clean up when actor is stopping.
%% By default, does nothing.
%% Generated actors can override this for cleanup.
-spec terminate(term(), map()) -> ok.
terminate(_Reason, _State) ->
    ok.

%%% Message Dispatch

%% @doc Dispatch a message to the appropriate method.
%% Looks up the selector in the __methods__ map and calls the method function.
%% If not found, attempts to call doesNotUnderstand handler.
%% Returns one of:
%%   {reply, Result, NewState} - Method returned a value
%%   {noreply, NewState} - Method didn't return a value
%%   {error, Reason, State} - Method or dispatch failed
-spec dispatch(atom(), list(), map()) ->
    {reply, term(), map()} | {noreply, map()} | {error, term(), map()}.
dispatch(Selector, Args, State) ->
    Methods = maps:get('__methods__', State),
    case maps:find(Selector, Methods) of
        {ok, Fun} when is_function(Fun, 2) ->
            %% Call the method function with Args and State
            try
                Fun(Args, State)
            catch
                Class:Reason:Stacktrace ->
                    %% Method threw an exception
                    io:format(standard_error,
                        "Error in method ~p: ~p:~p~n~p~n",
                        [Selector, Class, Reason, Stacktrace]),
                    {error, {method_error, Selector, Reason}, State}
            end;
        {ok, _NotAFunction} ->
            %% Method value is not a function
            {error, {invalid_method, Selector}, State};
        error ->
            %% Method not found - try doesNotUnderstand
            handle_dnu(Selector, Args, State)
    end.

%% @private
%% @doc Handle messages for unknown selectors.
%% Attempts to call the doesNotUnderstand:args: handler if defined.
%% Otherwise, returns an error.
-spec handle_dnu(atom(), list(), map()) ->
    {reply, term(), map()} | {noreply, map()} | {error, term(), map()}.
handle_dnu(Selector, Args, State) ->
    Methods = maps:get('__methods__', State),
    case maps:find('doesNotUnderstand:args:', Methods) of
        {ok, DnuFun} when is_function(DnuFun, 2) ->
            %% Call the DNU handler with [Selector, Args]
            try
                DnuFun([Selector, Args], State)
            catch
                Class:Reason:Stacktrace ->
                    io:format(standard_error,
                        "Error in doesNotUnderstand handler: ~p:~p~n~p~n",
                        [Class, Reason, Stacktrace]),
                    {error, {dnu_handler_error, Reason}, State}
            end;
        _ ->
            %% No DNU handler - method not found is an error
            {error, {unknown_message, Selector}, State}
    end.
