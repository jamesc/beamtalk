%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Object System Context

-module(beamtalk_actor_state_proto_tests).
-moduledoc """
ADR 0137 Phase 0 (BT-4007): the OTP checks of the ADR's spike as EUnit tests.

`probe_server` below is a throwaway `gen_server` that follows the callback
contract of ADR 0137 §1 and §4: the wrapper installs the cell
(`'$bt_actor_state'`), the "method" mutates it (through the closure helpers
of `beamtalk_actor_state_proto`, as flagged codegen output does), and the wrapper
returns the cell on a normal exit and the state it was called with on an
exception. The cell is erased in an `after`.
""".

-behaviour(gen_server).
-behaviour(supervisor).

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%% gen_server / supervisor callbacks (the throwaway server)
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-define(CELL, '$bt_actor_state').

%%% ----------------------------------------------------------------------
%%% The throwaway gen_server
%%% ----------------------------------------------------------------------

init(supervisor_init) ->
    {ok,
        {#{strategy => one_for_one, intensity => 5, period => 10}, [
            #{
                id => probe,
                start => {gen_server, start_link, [?MODULE, #{x => init}, []]},
                restart => permanent
            }
        ]}};
init(Init) ->
    {ok, Init}.

handle_call(Req, From, State) ->
    Old = erlang:get(?CELL),
    erlang:put(?CELL, State),
    try method(Req, From, State) of
        {reply, R} ->
            New = erlang:get(?CELL),
            {reply, R, New};
        {reply_hibernate, R} ->
            New = erlang:get(?CELL),
            {reply, R, New, hibernate}
    catch
        Class:Reason ->
            %% Rollback: an error that escapes the callback discards its writes.
            {reply, {caught, Class, Reason}, State}
    after
        case Old of
            undefined -> erlang:erase(?CELL);
            _ -> erlang:put(?CELL, Old)
        end
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    %% Record what the callback saw: no cell, the gen_server state.
    {ok, State#{code_change_cell => erlang:get(?CELL)}}.

%% The "methods". `Self` is a #beamtalk_object{} for this process.
method(slow_set, _From, _State) ->
    beamtalk_actor_state_proto:put_field(self_obj(), x, 5),
    timer:sleep(200),
    {reply, ok};
method(closure_updates, _From, State) ->
    Self = self_obj(),
    Reader = fun() -> beamtalk_actor_state_proto:get_field(Self, State, x) end,
    Writer = fun(V) -> beamtalk_actor_state_proto:put_field(Self, x, V) end,
    Before = Reader(),
    Writer(7),
    After = Reader(),
    Writer(8),
    {reply, {Before, After, Reader()}};
method(error_after_write, _From, _State) ->
    beamtalk_actor_state_proto:put_field(self_obj(), x, 99),
    erlang:error(boom);
method(cell_seen, _From, _State) ->
    {reply, erlang:get(?CELL)};
method(hibernate_after_write, _From, _State) ->
    beamtalk_actor_state_proto:put_field(self_obj(), x, 3),
    {reply_hibernate, ok};
method(crash, _From, _State) ->
    erlang:put(?CELL, #{x => crashed}),
    %% Not catchable by the wrapper's try/catch: the process dies mid-callback.
    erlang:exit(self(), kill);
method(away_block, _From, _State) ->
    %% A block that leaves the actor's process runs away from home.
    Self = self_obj(),
    {reply, fun() -> beamtalk_actor_state_proto:put_field(Self, x, 1) end}.

self_obj() ->
    #beamtalk_object{class = 'ProbeActor', class_mod = undefined, pid = self()}.

start(Init) ->
    {ok, Pid} = gen_server:start(?MODULE, Init, []),
    Pid.

raised_error(Fun) ->
    try
        Fun(),
        no_error
    catch
        error:#{error := #beamtalk_error{} = E} -> E
    end.

%%% ----------------------------------------------------------------------
%%% Tests
%%% ----------------------------------------------------------------------

sys_get_state_during_callback_test() ->
    Pid = start(#{x => 0}),
    Caller = self(),
    spawn_link(fun() -> Caller ! {done, gen_server:call(Pid, slow_set)} end),
    timer:sleep(50),
    %% System messages are handled between callbacks: this waits for the
    %% 200 ms callback and gets the committed value, never a half-written one.
    {Micros, State} = timer:tc(fun() -> sys:get_state(Pid) end),
    ?assertEqual(#{x => 5}, State),
    ?assert(Micros > 50_000),
    receive
        {done, ok} -> ok
    after 2000 -> ?assert(false)
    end.

code_change_runs_between_callbacks_with_the_gen_server_state_test() ->
    Pid = start(#{x => 1}),
    ok = sys:suspend(Pid),
    ok = sys:change_code(Pid, ?MODULE, old, extra),
    ok = sys:resume(Pid),
    %% code_change/3 ran with the gen_server state and no cell.
    ?assertMatch(#{x := 1, code_change_cell := undefined}, sys:get_state(Pid)).

crash_restart_takes_the_cell_and_restarts_with_init_state_test() ->
    {ok, Sup} = supervisor:start_link(?MODULE, supervisor_init),
    try
        [{probe, Pid, _, _}] = supervisor:which_children(Sup),
        catch gen_server:call(Pid, crash),
        Pid2 = wait_for_restart(Sup, Pid, 100),
        %% A restarted process is a new pid with init's state and no cell.
        ?assertNotEqual(Pid, Pid2),
        ?assertEqual(#{x => init}, sys:get_state(Pid2)),
        {dictionary, Dict} = process_info(Pid2, dictionary),
        ?assertEqual(false, lists:keymember(?CELL, 1, Dict)),
        %% A block from the old process is not at home in the new one.
        OldSelf = #beamtalk_object{class = 'ProbeActor', class_mod = undefined, pid = Pid},
        ?assertEqual(false, beamtalk_actor_state_proto:at_home(OldSelf))
    after
        unlink(Sup),
        exit(Sup, shutdown)
    end.

wait_for_restart(_Sup, _Old, 0) ->
    erlang:error(no_restart);
wait_for_restart(Sup, Old, N) ->
    case supervisor:which_children(Sup) of
        [{probe, P, _, _}] when is_pid(P), P =/= Old ->
            P;
        _ ->
            timer:sleep(20),
            wait_for_restart(Sup, Old, N - 1)
    end.

hibernate_wakes_with_state_and_no_cell_test() ->
    Pid = start(#{x => 0}),
    ok = gen_server:call(Pid, hibernate_after_write),
    %% Hibernation happens after the cell is erased.
    timer:sleep(50),
    {dictionary, Dict} = process_info(Pid, dictionary),
    ?assertEqual(false, lists:keymember(?CELL, 1, Dict)),
    ?assertEqual(#{x => 3}, sys:get_state(Pid)),
    %% And the process wakes and keeps working.
    ?assertEqual({3, 7, 8}, gen_server:call(Pid, closure_updates)).

closures_read_and_write_the_live_cell_test() ->
    Pid = start(#{x => 0}),
    %% Before the write the reader sees 0; after Writer(7) the same reader
    %% (created before the write) sees 7; then 8.
    ?assertEqual({0, 7, 8}, gen_server:call(Pid, closure_updates)),
    ?assertEqual(#{x => 8}, sys:get_state(Pid)).

rollback_on_an_escaping_error_test() ->
    Pid = start(#{x => 1}),
    ?assertMatch({caught, error, boom}, gen_server:call(Pid, error_after_write)),
    %% The write made before the error is discarded.
    ?assertEqual(#{x => 1}, sys:get_state(Pid)).

cell_does_not_exist_between_callbacks_test() ->
    Pid = start(#{x => 1}),
    ?assertEqual(#{x => 1}, gen_server:call(Pid, cell_seen)),
    {dictionary, Dict} = process_info(Pid, dictionary),
    ?assertEqual(false, lists:keymember(?CELL, 1, Dict)).

away_write_raises_actor_state_unreachable_test() ->
    Pid = start(#{x => 0}),
    Block = gen_server:call(Pid, away_block),
    %% This test process is not the actor's process: the block is away.
    ?assertMatch(#beamtalk_error{kind = actor_state_unreachable}, raised_error(Block)),
    ?assertEqual(#{x => 0}, sys:get_state(Pid)).

away_read_uses_the_captured_state_test() ->
    Self = #beamtalk_object{class = 'ProbeActor', class_mod = undefined, pid = self()},
    %% No cell in this process: not at home, so the captured state answers.
    ?assertEqual(false, beamtalk_actor_state_proto:at_home(Self)),
    ?assertEqual(42, beamtalk_actor_state_proto:get_field(Self, #{x => 42}, x)),
    %% A cell is present but belongs to another pid: still away.
    erlang:put(?CELL, #{x => 1}),
    try
        Other = Self#beamtalk_object{pid = spawn(fun() -> ok end)},
        ?assertEqual(false, beamtalk_actor_state_proto:at_home(Other)),
        ?assertEqual(42, beamtalk_actor_state_proto:get_field(Other, #{x => 42}, x)),
        ?assertEqual(1, beamtalk_actor_state_proto:get_field(Self, #{x => 42}, x))
    after
        erlang:erase(?CELL)
    end.
