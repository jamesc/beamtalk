%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Object System Context

-module(beamtalk_actor_state_otp_tests).
-moduledoc """
ADR 0137 Phase 0 (BT-4007): the OTP checks of the ADR's spike, against the real
`beamtalk_actor_state`.

The throwaway `gen_server` below follows the callback contract of ADR 0137 §1
and §4: the wrapper installs the cell (`install/1`), the "method" mutates it
through the closure helpers (`put/3`, `get/3`, as flagged codegen output does),
and the wrapper returns the cell on a normal exit (`take/0`) and the state it
was called with on an exception. The cell is erased in an `after`
(`uninstall/1`). What is checked is how that contract meets OTP: `sys` messages,
`code_change/3`, a supervisor restart, hibernation, and rollback.
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
    Old = beamtalk_actor_state:install(State),
    try method(Req, From, State) of
        {reply, R} ->
            {reply, R, beamtalk_actor_state:take()};
        {reply_hibernate, R} ->
            {reply, R, beamtalk_actor_state:take(), hibernate}
    catch
        Class:Reason ->
            %% Rollback: an error that escapes the callback discards its writes.
            {reply, {caught, Class, Reason}, State}
    after
        beamtalk_actor_state:uninstall(Old)
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    %% Record what the callback saw: no cell, the gen_server state.
    {ok, State#{code_change_installed => beamtalk_actor_state:installed()}}.

%% The "methods". `Self` is a #beamtalk_object{} for this process.
method({slow_set_sync, Notify}, _From, _State) ->
    %% Rendezvous for sys_get_state_during_callback_test: write the value,
    %% signal that the callback is running, then block until the test
    %% releases us. A `sys` request issued while we are blocked here queues
    %% behind the callback and observes the committed state on return.
    ok = beamtalk_actor_state:put(self_obj(), x, 5),
    Notify ! slow_set_started,
    receive
        slow_set_release -> ok
    after 10000 ->
        erlang:error(slow_set_release_timeout)
    end,
    {reply, ok};
method(closure_updates, _From, _State) ->
    Self = self_obj(),
    Capture = beamtalk_actor_state:capture(Self, none),
    Reader = fun() -> beamtalk_actor_state:get(Self, x, Capture) end,
    Writer = fun(V) -> beamtalk_actor_state:put(Self, x, V) end,
    Before = Reader(),
    ok = Writer(7),
    After = Reader(),
    ok = Writer(8),
    {reply, {Before, After, Reader()}};
method(error_after_write, _From, _State) ->
    ok = beamtalk_actor_state:put(self_obj(), x, 99),
    erlang:error(boom);
method(cell_seen, _From, _State) ->
    {reply, beamtalk_actor_state:installed()};
method(hibernate_after_write, _From, _State) ->
    ok = beamtalk_actor_state:put(self_obj(), x, 3),
    {reply_hibernate, ok};
method(crash, _From, _State) ->
    ok = beamtalk_actor_state:put(self_obj(), x, crashed),
    %% Not catchable by the wrapper's try/catch: the process dies mid-callback.
    erlang:exit(self(), kill);
method(away_block, _From, _State) ->
    %% A block that leaves the actor's process runs away from home. It carries
    %% the state it was made with.
    Self = self_obj(),
    Capture = beamtalk_actor_state:capture(Self, none),
    {reply,
        {fun() -> beamtalk_actor_state:put(Self, x, 1) end, fun() ->
            beamtalk_actor_state:get(Self, x, Capture)
        end}}.

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

dictionary_has_cell(Pid) ->
    {dictionary, Dict} = process_info(Pid, dictionary),
    lists:keymember(?CELL, 1, Dict).

%%% ----------------------------------------------------------------------
%%% Tests
%%% ----------------------------------------------------------------------

sys_get_state_during_callback_test() ->
    Pid = start(#{x => 0}),
    Caller = self(),
    spawn_link(fun() ->
        Caller ! {done, gen_server:call(Pid, {slow_set_sync, Caller})}
    end),
    %% Rendezvous on the real condition: the callback has written x=5 and is
    %% blocked waiting for release, so the `sys` request below is guaranteed
    %% to arrive mid-callback. System messages are handled between callbacks:
    %% `sys:get_state` waits for the callback and gets the committed value,
    %% never a half-written one. No sleeps, no timing assertions.
    receive
        slow_set_started -> ok
    after 5000 ->
        ?assert(false)
    end,
    spawn_link(fun() -> Caller ! {sys_state, sys:get_state(Pid)} end),
    ok = wait_until_sys_queued(Pid),
    Pid ! slow_set_release,
    receive
        {sys_state, State} ->
            ?assertEqual(#{x => 5}, State)
    after 5000 ->
        ?assert(false)
    end,
    receive
        {done, ok} -> ok
    after 5000 ->
        ?assert(false)
    end.

%% Spin until the `sys` request has queued in the server's mailbox while the
%% callback is blocked. Uses `erlang:yield/0`, not `timer:sleep/1`: this waits
%% on the real condition (request queued) rather than assuming a delay.
wait_until_sys_queued(Pid) ->
    Deadline = erlang:monotonic_time(millisecond) + 5000,
    wait_until_sys_queued(Pid, Deadline).

wait_until_sys_queued(Pid, Deadline) ->
    case process_info(Pid, message_queue_len) of
        {message_queue_len, N} when N > 0 ->
            ok;
        _ ->
            case erlang:monotonic_time(millisecond) < Deadline of
                true ->
                    erlang:yield(),
                    wait_until_sys_queued(Pid, Deadline);
                false ->
                    ?assert(false)
            end
    end.

sys_replace_state_sees_the_returned_state_test() ->
    Pid = start(#{x => 0}),
    ok = gen_server:call(Pid, hibernate_after_write),
    New = sys:replace_state(Pid, fun(S) -> S#{x => replaced} end),
    ?assertEqual(#{x => replaced}, New),
    %% The next callback starts from the replaced state.
    ?assertEqual({replaced, 7, 8}, gen_server:call(Pid, closure_updates)).

code_change_runs_between_callbacks_with_the_gen_server_state_test() ->
    Pid = start(#{x => 1}),
    ok = sys:suspend(Pid),
    ok = sys:change_code(Pid, ?MODULE, old, extra),
    ok = sys:resume(Pid),
    %% code_change/3 ran with the gen_server state and no cell.
    ?assertMatch(#{x := 1, code_change_installed := false}, sys:get_state(Pid)).

crash_restart_takes_the_cell_and_restarts_with_init_state_test() ->
    {ok, Sup} = supervisor:start_link(?MODULE, supervisor_init),
    try
        [{probe, Pid, _, _}] = supervisor:which_children(Sup),
        catch gen_server:call(Pid, crash),
        Pid2 = wait_for_restart(Sup, Pid, 100),
        %% A restarted process is a new pid with init's state and no cell.
        ?assertNotEqual(Pid, Pid2),
        ?assertEqual(#{x => init}, sys:get_state(Pid2)),
        ?assertEqual(false, dictionary_has_cell(Pid2)),
        %% A block from the old process is not at home in the new one: its
        %% write raises, and its read needs the capture.
        OldSelf = #beamtalk_object{class = 'ProbeActor', class_mod = undefined, pid = Pid},
        ?assertMatch(
            #beamtalk_error{kind = actor_state_unreachable},
            raised_error(fun() -> beamtalk_actor_state:put(OldSelf, x, 1) end)
        )
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
    ?assertEqual(false, dictionary_has_cell(Pid)),
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
    ?assertEqual(#{x => 1}, sys:get_state(Pid)),
    ?assertEqual(false, dictionary_has_cell(Pid)).

cell_does_not_exist_between_callbacks_test() ->
    Pid = start(#{x => 1}),
    ?assertEqual(true, gen_server:call(Pid, cell_seen)),
    ?assertEqual(false, dictionary_has_cell(Pid)).

away_write_raises_actor_state_unreachable_test() ->
    Pid = start(#{x => 0}),
    {Write, Read} = gen_server:call(Pid, away_block),
    %% This test process is not the actor's process: the block is away.
    ?assertMatch(#beamtalk_error{kind = actor_state_unreachable}, raised_error(Write)),
    ?assertEqual(#{x => 0}, sys:get_state(Pid)),
    %% An away read answers from the state the block was made with.
    ?assertEqual(0, Read()).
