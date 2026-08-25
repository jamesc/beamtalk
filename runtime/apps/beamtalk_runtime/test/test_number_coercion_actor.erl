%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(test_number_coercion_actor).
-behaviour(gen_server).

-moduledoc """
Test actor for beamtalk_message_dispatch:send_number_coercion/4 tests
(BT-3262, ADR 0116).

Exposes a present `plusFromNumber:`-style reflected method whose own body
DNUs on an unrelated selector (simulating a bug inside the method, not a
missing hook) and a present `divFromNumber:` that raises a non-DNU error —
used to confirm send_number_coercion/4's class+selector match doesn't
over-catch either case. A selector with no corresponding method at all (e.g.
`timesFromNumber:`) is left undefined so ordinary dispatch produces a
genuine "hook missing" does_not_understand.
""".
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
-export([
    'handle_plusFromNumber:'/2,
    'handle_divFromNumber:'/2
]).

start_link() ->
    beamtalk_actor:start_link(?MODULE, []).

init(_Args) ->
    beamtalk_actor:init(#{
        '$beamtalk_class' => 'CoercionActor',
        '__class_mod__' => 'test_number_coercion_actor',
        '__methods__' => #{
            'plusFromNumber:' => fun ?MODULE:'handle_plusFromNumber:'/2,
            'divFromNumber:' => fun ?MODULE:'handle_divFromNumber:'/2
        }
    }).

handle_cast(Msg, State) -> beamtalk_actor:handle_cast(Msg, State).
handle_call(Msg, From, State) -> beamtalk_actor:handle_call(Msg, From, State).
handle_info(Msg, State) -> beamtalk_actor:handle_info(Msg, State).
code_change(OldVsn, State, Extra) -> beamtalk_actor:code_change(OldVsn, State, Extra).
terminate(Reason, State) -> beamtalk_actor:terminate(Reason, State).

%% Present, but its own body sends an unrelated message that DNUs — the
%% method itself is not missing, something inside it is broken. Used to
%% confirm send_number_coercion/4 does not rewrite this as "hook missing".
'handle_plusFromNumber:'([_N], _State) ->
    Error = beamtalk_error:new(does_not_understand, 'CoercionActor', unrelatedSelector),
    error(Error).

%% Present, raises a non-DNU error (instantiation_error) — confirms an
%% unrelated exception kind passes through send_number_coercion/4 unchanged.
'handle_divFromNumber:'([_N], _State) ->
    Error = beamtalk_error:new(instantiation_error, 'CoercionActor', 'divFromNumber:'),
    error(Error).
