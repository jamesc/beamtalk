%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_supervisor_test_helper).

%%% **DDD Context:** Actor System Context

-moduledoc """
Test helper module for the BT-3759 concurrent `supervise` tests.

Provides three `start_link/0` variants keyed by an ETS flag so a single
Module:start_link() dispatch can return {ok, Pid}, {error, {already_started, Pid}},
or {error, Reason} as required by the test case.

Used only by beamtalk_supervisor_tests.erl.
""".

-export([start_link/0, init/1, set_mode/2, reset/0]).
%% BT-3759: the instance-side `initialize` hook
%% (ADR 0135 §1) for the concurrent-supervise tests.
-export([dispatch/4, has_method/1, has_method_local/1]).
%% BT-3994: a long-lived owner for the `supervise` start-lock table.
-export([ensure_lock_table/0]).
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
%% BT-3759: instance-side `initialize` stand-in
%%====================================================================

-doc """
Instance-side dispatch for the supervisor handle: answers `initialize` only
(`beamtalk_supervisor:run_initialize/1` sends it to the new supervisor).
""".
dispatch(initialize, [], _Self, State) ->
    {reply, initialize_hook(), State}.

has_method(Selector) -> has_method_local(Selector).

has_method_local(initialize) -> true;
has_method_local(_) -> false.

-doc """
Stand-in for the instance-side `initialize` hook. Tells the coordinator (the
pid in the `coordinator` ETS row) it has been entered, blocks until released,
then either raises (`{hook, fail}`) or marks the supervisor initialised.
""".
-spec initialize_hook() -> nil.
initialize_hook() ->
    [{coordinator, Coord}] = ets:lookup(?TAB, coordinator),
    Coord ! {init_entered, self()},
    receive
        {hook, ok} ->
            ets:insert(?TAB, {initialized, true}),
            nil;
        {hook, fail} ->
            beamtalk_error:raise(
                beamtalk_error:new(type_error, 'BT3759Class', initialize, <<"hook failed">>)
            )
    end.

%%====================================================================
%% Internal
%%====================================================================

-doc """
Make sure `beamtalk_fifo_lock`'s table exists and is owned by a process that
outlives the test (BT-3994). Without the runtime app nothing creates it at
start, so the first `with_lock/3` caller would own it and take it down when it
exits, under any caller still waiting.
""".
-spec ensure_lock_table() -> ok.
ensure_lock_table() ->
    case ets:whereis(beamtalk_fifo_locks) of
        undefined ->
            Parent = self(),
            Owner = spawn(fun() ->
                beamtalk_fifo_lock:ensure_table(),
                Parent ! {lock_table_ready, self()},
                receive
                    stop -> ok
                end
            end),
            receive
                {lock_table_ready, Owner} -> ok
            after 5000 -> error(lock_table_not_created)
            end;
        _ ->
            ok
    end.

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
