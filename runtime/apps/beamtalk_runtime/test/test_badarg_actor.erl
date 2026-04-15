%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(test_badarg_actor).
-behaviour(gen_server).

-moduledoc "Test actor that triggers various Erlang error types for BT-1958 coverage".

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

%% Method implementations
-export([
    handle_triggerBadarg/2,
    handle_triggerBadarith/2,
    handle_triggerUndef/2,
    handle_triggerFunctionClause/2
]).

start_link() ->
    beamtalk_actor:start_link(?MODULE, []).

init(_Args) ->
    beamtalk_actor:init(#{
        '$beamtalk_class' => 'BadargActor',
        '__class_mod__' => 'test_badarg_actor',
        '__methods__' => #{
            triggerBadarg => fun ?MODULE:handle_triggerBadarg/2,
            triggerBadarith => fun ?MODULE:handle_triggerBadarith/2,
            triggerUndef => fun ?MODULE:handle_triggerUndef/2,
            triggerFunctionClause => fun ?MODULE:handle_triggerFunctionClause/2
        }
    }).

handle_cast(Msg, State) -> beamtalk_actor:handle_cast(Msg, State).
handle_call(Msg, From, State) -> beamtalk_actor:handle_call(Msg, From, State).
handle_info(Msg, State) -> beamtalk_actor:handle_info(Msg, State).
code_change(OldVsn, State, Extra) -> beamtalk_actor:code_change(OldVsn, State, Extra).
terminate(Reason, State) -> beamtalk_actor:terminate(Reason, State).

%% Trigger a badarg error
handle_triggerBadarg([], _State) ->
    %% list_to_integer with a non-numeric string triggers badarg
    _ = list_to_integer("not_a_number"),
    {reply, ok, _State}.

%% Trigger a badarith error
handle_triggerBadarith([], _State) ->
    %% Division by zero triggers badarith
    _ = 1 / 0,
    {reply, ok, _State}.

%% Trigger an undef error
handle_triggerUndef([], _State) ->
    %% Call a function that doesn't exist
    _ = nonexistent_module_bt1958:nonexistent_function(),
    {reply, ok, _State}.

%% Trigger a function_clause error
handle_triggerFunctionClause([], _State) ->
    %% Call a function with wrong arguments to trigger function_clause
    _ = trigger_clause_error(wrong_arg),
    {reply, ok, _State}.

%% Helper that only matches specific atoms
trigger_clause_error(only_this_atom_matches_nothing) ->
    ok.
