%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Test actor that simulates backward-compatible error tuples.
%%%
%%% Compiled actors generated before BT-1822 return {error, {ErlType, ErrorValue}}
%%% from safe_dispatch (without stacktrace). This actor simulates that pattern
%%% to verify the backward-compat paths in beamtalk_actor preserve the exception
%%% class (ErlType) instead of discarding it.
%%%
%%% Two code paths are exercised:
%%%   1. sync_send (remote): handle_call returns {reply, {error, {ErlType, Value}}, State}
%%%   2. self_dispatch: safe_dispatch/3 returns {error, {ErlType, Value}, State}
%%%      Triggered when an actor sends to itself (ActorPid == self()).

-module(test_compat_error_actor).
-behaviour(gen_server).
-include("beamtalk.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    code_change/3,
    terminate/2
]).

%% Backward-compat safe_dispatch (returns 2-tuple errors without stacktrace)
-export([safe_dispatch/3]).

start_link() ->
    beamtalk_actor:start_link(?MODULE, []).

init(_Args) ->
    beamtalk_actor:init(#{
        '$beamtalk_class' => 'CompatErrorActor',
        '__class_mod__' => ?MODULE,
        '__methods__' => #{
            triggerExitError => fun([], _State) -> exit(simulated_exit) end,
            triggerThrowError => fun([], _State) -> throw(simulated_throw) end,
            triggerPlainError => fun([], _State) -> error(simulated_error) end,
            %% selfSendExit triggers a self-send, which goes through self_dispatch
            %% and then safe_dispatch/3 → unwrap_dispatch_result.
            selfSendExitError => fun([], _State) ->
                beamtalk_actor:sync_send(self(), triggerExitError, [])
            end,
            selfSendThrowError => fun([], _State) ->
                beamtalk_actor:sync_send(self(), triggerThrowError, [])
            end,
            selfSendPlainError => fun([], _State) ->
                beamtalk_actor:sync_send(self(), triggerPlainError, [])
            end,
            normalMethod => fun([], State) -> {reply, ok, State} end
        }
    }).

handle_cast(Msg, State) -> beamtalk_actor:handle_cast(Msg, State).

%% Custom handle_call that simulates backward-compat error replies.
%% Instead of delegating to beamtalk_actor:handle_call (which would use
%% dispatch/4 with its own try/catch), we produce the old-style
%% {error, {ErlType, ErrorValue}} reply directly.
%%
%% Note: selfSend* methods delegate to beamtalk_actor:handle_call so they
%% go through dispatch/4 → the method fun calls sync_send(self(), ...) →
%% self_dispatch → safe_dispatch/3 → unwrap_dispatch_result.
handle_call({triggerExitError, _Args}, _From, State) ->
    {reply, {error, {exit, simulated_exit}}, State};
handle_call({triggerThrowError, _Args}, _From, State) ->
    {reply, {error, {throw, simulated_throw}}, State};
handle_call({triggerPlainError, _Args}, _From, State) ->
    {reply, {error, {error, simulated_error}}, State};
handle_call({triggerExitError, _Args, _PropCtx}, _From, State) ->
    {reply, {error, {exit, simulated_exit}}, State};
handle_call({triggerThrowError, _Args, _PropCtx}, _From, State) ->
    {reply, {error, {throw, simulated_throw}}, State};
handle_call({triggerPlainError, _Args, _PropCtx}, _From, State) ->
    {reply, {error, {error, simulated_error}}, State};
handle_call(Msg, From, State) ->
    beamtalk_actor:handle_call(Msg, From, State).

handle_info(Msg, State) -> beamtalk_actor:handle_info(Msg, State).
code_change(OldVsn, State, Extra) -> beamtalk_actor:code_change(OldVsn, State, Extra).
terminate(Reason, State) -> beamtalk_actor:terminate(Reason, State).

%% @doc Backward-compat safe_dispatch/3 that returns 2-tuple errors.
%%
%% This simulates the old compiled actor pattern (pre-BT-1822) where
%% safe_dispatch catches exceptions and returns {error, {Type, Value}, State}
%% without the stacktrace third element.
safe_dispatch(triggerExitError, _Args, State) ->
    {error, {exit, simulated_exit}, State};
safe_dispatch(triggerThrowError, _Args, State) ->
    {error, {throw, simulated_throw}, State};
safe_dispatch(triggerPlainError, _Args, State) ->
    {error, {error, simulated_error}, State};
safe_dispatch(normalMethod, _Args, State) ->
    {reply, ok, State};
safe_dispatch(Selector, Args, State) ->
    %% For selfSend* methods, fall through to normal dispatch so they execute
    %% as regular methods (which then self-send to trigger safe_dispatch above).
    Self = beamtalk_actor:make_self(State),
    beamtalk_actor:dispatch(Selector, Args, Self, State).
