%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Test helper module for BT-732 class chain wire check.
%%%
%% **DDD Context:** Object System
%%%
%%% This module acts as a minimal Class implementation for EUnit tests.
%%% It provides `testClassProtocol` so tests can verify the dispatch
%%% fallthrough without putting probe methods in production code.
%%%
%%% Used only by beamtalk_class_chain_tests.erl.

-module(beamtalk_class_chain_test_helper).

-include("beamtalk.hrl").

-export([dispatch/4, has_method/1]).

%% @doc Test probe: returns {class_protocol_ok, Self} to verify dispatch.
%% Self's class field carries the metaclass tag, proving tag preservation.
-spec dispatch(atom(), list(), term(), map()) ->
    {reply, term(), map()} | {error, #beamtalk_error{}, map()}.
dispatch(testClassProtocol, [], Self, State) ->
    {reply, {class_protocol_ok, Self}, State};
dispatch(Selector, _Args, _Self, State) ->
    Error0 = beamtalk_error:new(does_not_understand, 'Class'),
    Error = beamtalk_error:with_selector(Error0, Selector),
    {error, Error, State}.

%% @doc Declare supported methods for has_method checks.
-spec has_method(atom()) -> boolean().
has_method(testClassProtocol) -> true;
has_method(_) -> false.
