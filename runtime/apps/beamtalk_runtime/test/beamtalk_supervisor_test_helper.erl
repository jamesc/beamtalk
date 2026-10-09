%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_supervisor_test_helper).

%%% **DDD Context:** Actor System Context

-moduledoc """
Test helper module for beamtalk_supervisor:startLink/1 tests.

Provides three `start_link/0` variants keyed by an ETS flag so a single
Module:start_link() dispatch can return {ok, Pid}, {error, {already_started, Pid}},
or {error, Reason} as required by the test case.

Used only by beamtalk_supervisor_tests.erl.
""".

-export([start_link/0, init/1, set_mode/2, reset/0]).
%% BT-3759: class-side methods for the concurrent-supervise tests.
-export([class_supervise/1, 'class_initialize:'/2]).
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

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
        [{mode, named, Name}] ->
            %% Real named supervisor: a second call yields a genuine
            %% {error, {already_started, Pid}} from supervisor:start_link/3.
            supervisor:start_link({local, Name}, ?MODULE, []);
        [{mode, error, Reason}] ->
            {error, Reason};
        [] ->
            {error, no_mode_set}
    end.

%% OTP supervisor callback: empty one_for_one supervisor.
init([]) ->
    {ok, {#{strategy => one_for_one, intensity => 0, period => 1}, []}}.

%%====================================================================
%% Class-side methods (BT-3759): `X supervise` + `initialize:` stand-ins
%%====================================================================

-doc "Stand-in for `supervise`: startLink/1 wrapped as the FFI Result map.".
-spec class_supervise(term()) -> map().
class_supervise(ClassSelf) ->
    beamtalk_result:from_tagged_tuple(beamtalk_supervisor:startLink(ClassSelf)).

-doc """
Stand-in for the class-side `initialize:` hook. Tells the coordinator (the pid
in the `coordinator` ETS row) it has been entered, blocks until released, then
either raises (`{hook, fail}`) or marks the supervisor initialised.
""".
-spec 'class_initialize:'(term(), term()) -> nil.
'class_initialize:'(_ClassSelf, _SupTuple) ->
    [{coordinator, Coord}] = ets:lookup(?TAB, coordinator),
    Coord ! {init_entered, self()},
    receive
        {hook, ok} ->
            ets:insert(?TAB, {initialized, true}),
            nil;
        {hook, fail} ->
            beamtalk_error:raise(
                beamtalk_error:new(type_error, 'BT3759Class', 'initialize:', <<"hook failed">>)
            )
    end.

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
