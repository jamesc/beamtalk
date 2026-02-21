%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Test actor that throws #beamtalk_error{} records in methods.
%%% Used to verify that structured errors are preserved through dispatch.

-module(test_bt_error_actor).
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

%% Method implementations
-export([handle_throwBtError/2, handle_normalMethod/2]).

start_link() ->
    beamtalk_actor:start_link(?MODULE, []).

init(_Args) ->
    beamtalk_actor:init(#{
        '$beamtalk_class' => 'BtErrorActor',
        '__class_mod__' => 'test_bt_error_actor',
        '__methods__' => #{
            throwBtError => fun ?MODULE:handle_throwBtError/2,
            normalMethod => fun ?MODULE:handle_normalMethod/2
        }
    }).

handle_cast(Msg, State) -> beamtalk_actor:handle_cast(Msg, State).
handle_call(Msg, From, State) -> beamtalk_actor:handle_call(Msg, From, State).
handle_info(Msg, State) -> beamtalk_actor:handle_info(Msg, State).
code_change(OldVsn, State, Extra) -> beamtalk_actor:code_change(OldVsn, State, Extra).
terminate(Reason, State) -> beamtalk_actor:terminate(Reason, State).

%% Method that throws a structured #beamtalk_error{}
handle_throwBtError([], _State) ->
    Error0 = beamtalk_error:new(instantiation_error, 'TestClass'),
    Error = beamtalk_error:with_selector(Error0, throwBtError),
    error(Error).

%% Normal method for comparison
handle_normalMethod([], State) ->
    {reply, ok, State}.
