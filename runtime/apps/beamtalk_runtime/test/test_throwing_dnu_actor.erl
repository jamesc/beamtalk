%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(test_throwing_dnu_actor).
-behaviour(gen_server).

-moduledoc "Test actor with throwing doesNotUnderstand handler".

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
-export(['handle_doesNotUnderstand:args:'/2]).

start_link() ->
    beamtalk_actor:start_link(?MODULE, []).

init(_Args) ->
    test_actor_host:init(#{
        '$beamtalk_class' => 'ThrowingDnuActor',
        '__class_mod__' => 'test_throwing_dnu_actor',
        '__methods__' => #{
            'doesNotUnderstand:args:' => fun ?MODULE:'handle_doesNotUnderstand:args:'/2
        }
    }).

handle_cast(Msg, State) -> test_actor_host:handle_cast(Msg, State).
handle_call(Msg, From, State) -> test_actor_host:handle_call(Msg, From, State).
handle_info(Msg, State) -> test_actor_host:handle_info(Msg, State).
code_change(OldVsn, State, Extra) -> test_actor_host:code_change(OldVsn, State, Extra).
terminate(Reason, State) -> test_actor_host:terminate(Reason, State).

%% DNU handler that throws
'handle_doesNotUnderstand:args:'([_Selector, _Args], _State) ->
    error(dnu_intentional_error).
