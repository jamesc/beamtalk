%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Dynamic Object Behavior for Interpreter-Based Classes
%%%
%%% This module implements a gen_server behavior for dynamically created
%%% Beamtalk classes. Unlike compiled classes which generate optimized
%%% Erlang code, dynamic classes store methods as closures and dispatch
%%% via apply/2.
%%%
%%% ## Architecture
%%%
%%% Dynamic classes are created at runtime via `beamtalk_object_class:create_subclass/3`.
%%% Instances of dynamic classes run this behavior, which:
%%% - Stores instance state in a map (like compiled actors)
%%% - Stores method closures in `__methods__` field
%%% - Dispatches method calls via `apply/2` on closures
%%%
%%% ## State Structure
%%%
%%% ```erlang
%%% #{
%%%   '$beamtalk_class' => 'MyDynamicClass',
%%%   '__methods__' => #{
%%%     'methodName' => fun(Self, Args, State) -> {reply, Result, NewState} end,
%%%     ...
%%%   },
%%%   '__class_pid__' => <0.123.0>,  % Reference to class process
%%%   %% User-defined instance variables
%%%   field1 => value1,
%%%   field2 => value2
%%% }
%%% ```
%%%
%%% ## Method Closure Protocol
%%%
%%% Each method is a function of arity 3:
%%% ```erlang
%%% fun(Self, Args, State) ->
%%%   {reply, Result, NewState} | {noreply, NewState}
%%% end
%%% ```
%%%
%%% Where:
%%% - Self: The beamtalk_object record for this instance
%%% - Args: List of arguments passed to the method
%%% - State: Current state map
%%%
%%% Returns match the gen_server protocol for actor dispatch.

-module(beamtalk_dynamic_object).
-behaviour(gen_server).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% Public API
-export([start_link/2, start_link/3]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2,
         code_change/3, terminate/2]).

%% Internal dispatch
-export([dispatch/4]).

-record(state, {
    class_name :: atom(),
    class_pid :: pid(),
    fields :: map()
}).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Start a dynamic object instance.
%% InitState should contain '$beamtalk_class', '__methods__', '__class_pid__', plus instance fields.
-spec start_link(atom(), map()) -> {ok, pid()} | {error, term()}.
start_link(ClassName, InitState) ->
    gen_server:start_link(?MODULE, {ClassName, InitState}, []).

%% @doc Start a registered dynamic object instance.
-spec start_link(term(), atom(), map()) -> {ok, pid()} | {error, term()}.
start_link(Name, ClassName, InitState) ->
    gen_server:start_link(Name, ?MODULE, {ClassName, InitState}, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init({ClassName, InitState}) ->
    %% Extract required fields from InitState
    ClassPid = maps:get('__class_pid__', InitState),
    
    %% Build internal state (methods stored in fields as '__methods__')
    State = #state{
        class_name = ClassName,
        class_pid = ClassPid,
        fields = InitState
    },
    {ok, State}.

%% Handle synchronous calls
handle_call({Selector, Args}, _From, State) ->
    case dispatch(Selector, Args, make_self(State), State#state.fields) of
        {reply, Result, NewFields} ->
            NewState = State#state{fields = NewFields},
            {reply, Result, NewState};
        {noreply, NewFields} ->
            %% Method doesn't return a value in sync context - return nil
            NewState = State#state{fields = NewFields},
            {reply, nil, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_message}, State}.

%% Handle asynchronous casts
handle_cast({Selector, Args, FuturePid}, State) ->
    case dispatch(Selector, Args, make_self(State), State#state.fields) of
        {reply, Result, NewFields} ->
            %% Send result to future
            beamtalk_future:resolve(FuturePid, Result),
            NewState = State#state{fields = NewFields},
            {noreply, NewState};
        {noreply, NewFields} ->
            %% Method doesn't return a value - resolve with nil
            beamtalk_future:resolve(FuturePid, nil),
            NewState = State#state{fields = NewFields},
            {noreply, NewState};
        {error, Reason} ->
            beamtalk_future:reject(FuturePid, Reason),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    beamtalk_hot_reload:code_change(OldVsn, State, Extra).

%%====================================================================
%% Internal Dispatch
%%====================================================================

%% @doc Dispatch a message to a dynamic method.
%% Looks up the method closure and invokes it with Self, Args, and State.
-spec dispatch(atom(), list(), term(), map()) ->
    {reply, term(), map()} | {noreply, map()} | {error, term()}.
dispatch(Selector, Args, Self, State) ->
    %% Extract methods map from state
    Methods = maps:get('__methods__', State, #{}),
    
    case maps:find(Selector, Methods) of
        {ok, MethodFun} ->
            %% Found method - invoke closure
            try
                apply(MethodFun, [Self, Args, State])
            catch
                Class:Reason:_Stacktrace ->
                    %% Method threw an exception - log without stack trace to avoid leaking sensitive data
                    ?LOG_ERROR("Error in method", #{
                        selector => Selector,
                        class => Class,
                        reason => Reason
                    }),
                    ClassName = beamtalk_tagged_map:class_of(State, unknown),
                    Error0 = beamtalk_error:new(type_error, ClassName),
                    Error = beamtalk_error:with_selector(Error0, Selector),
                    {error, Error}
            end;
        error ->
            %% Method not found - check for doesNotUnderstand
            case maps:find('doesNotUnderstand:args:', Methods) of
                {ok, DNUFun} ->
                    %% Call doesNotUnderstand handler with correct format: [Selector, Args]
                    try
                        apply(DNUFun, [Self, [Selector, Args], State])
                    catch
                        Class:Reason:_Stacktrace ->
                            %% DNU handler threw an exception - log without stack trace to avoid leaking sensitive data
                            ?LOG_ERROR("Error in doesNotUnderstand handler", #{
                                selector => Selector,
                                class => Class,
                                reason => Reason
                            }),
                            ClassName = beamtalk_tagged_map:class_of(State, unknown),
                            Error0 = beamtalk_error:new(type_error, ClassName),
                            Error = beamtalk_error:with_selector(Error0, 'doesNotUnderstand:args:'),
                            {error, Error}
                    end;
                error ->
                    %% No doesNotUnderstand - method not found is an error
                    ClassName = beamtalk_tagged_map:class_of(State, unknown),
                    Error0 = beamtalk_error:new(does_not_understand, ClassName),
                    Error1 = beamtalk_error:with_selector(Error0, Selector),
                    Error = beamtalk_error:with_hint(Error1, <<"Check spelling or use 'respondsTo:' to verify method exists">>),
                    {error, Error}
            end
    end.

%% @doc Create a Self reference for this dynamic object.
%% Returns a beamtalk_object record (from beamtalk.hrl).
make_self(#state{class_name = ClassName}) ->
    ActorPid = self(),
    #beamtalk_object{
        class = ClassName,
        class_mod = beamtalk_dynamic_object,
        pid = ActorPid
    }.
