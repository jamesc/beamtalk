%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_supervisor_test_helper).

%%% **DDD Context:** Actor System Context

-moduledoc """
Test helper module for BT-1980 beamtalk_supervisor:startLink/1 tests.

Provides three `start_link/0` variants keyed by an ETS flag so a single
Module:start_link() dispatch can return {ok, Pid}, {error, {already_started, Pid}},
or {error, Reason} as required by the test case.

Used only by beamtalk_supervisor_tests.erl.
""".

-export([start_link/0, init/1, set_mode/2, reset/0]).

-define(TAB, beamtalk_supervisor_test_helper_tab).

%%====================================================================
%% Mode control (set by test case before invoking startLink)
%%====================================================================

-doc "Set the start_link/0 behaviour. Mode is ok | already_started | error.".
-spec set_mode(atom(), term()) -> ok.
set_mode(Mode, Extra) ->
    ensure_table(),
    ets:insert(?TAB, {mode, Mode, Extra}),
    ok.

-doc "Clear the mode table between tests.".
-spec reset() -> ok.
reset() ->
    try
        ets:delete(?TAB)
    catch
        _:_ -> ok
    end,
    ok.

%%====================================================================
%% Supervisor-style start_link/0 with mocked behaviour
%%====================================================================

-doc "Dispatches on the mode flag set by the test case.".
-spec start_link() ->
    {ok, pid()} | {error, {already_started, pid()} | term()}.
start_link() ->
    ensure_table(),
    case ets:lookup(?TAB, mode) of
        [{mode, ok, _}] ->
            supervisor:start_link(?MODULE, []);
        [{mode, already_started, _}] ->
            %% Start once, then return already_started on subsequent calls.
            case supervisor:start_link(?MODULE, []) of
                {ok, Pid} -> {error, {already_started, Pid}};
                Other -> Other
            end;
        [{mode, error, Reason}] ->
            {error, Reason};
        [] ->
            {error, no_mode_set}
    end.

%% OTP supervisor callback: empty one_for_one supervisor.
init([]) ->
    {ok, {#{strategy => one_for_one, intensity => 0, period => 1}, []}}.

%%====================================================================
%% Internal
%%====================================================================

ensure_table() ->
    case ets:info(?TAB, id) of
        undefined ->
            try
                ets:new(?TAB, [named_table, public, set])
            catch
                error:badarg -> ok
            end;
        _ ->
            ok
    end.
