%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_session_sup_tests).

-moduledoc """
Unit tests for beamtalk_session_sup module

Tests session supervisor behavior.
""".
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests
%%====================================================================

%%% Supervisor initialization tests

init_returns_correct_strategy_test() ->
    {ok, {SupFlags, _ChildSpecs}} = beamtalk_session_sup:init([]),

    %% Should use simple_one_for_one strategy
    ?assertEqual(simple_one_for_one, maps:get(strategy, SupFlags)).

init_returns_correct_intensity_test() ->
    {ok, {SupFlags, _ChildSpecs}} = beamtalk_session_sup:init([]),

    %% Should allow 10 restarts in 60 seconds
    ?assertEqual(10, maps:get(intensity, SupFlags)),
    ?assertEqual(60, maps:get(period, SupFlags)).

init_returns_single_child_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_session_sup:init([]),

    %% simple_one_for_one should have exactly 1 child spec (the template)
    ?assertEqual(1, length(ChildSpecs)).

init_child_spec_is_temporary_test() ->
    {ok, {_SupFlags, [ChildSpec]}} = beamtalk_session_sup:init([]),

    %% Sessions should be temporary (not restarted on disconnect)
    ?assertEqual(temporary, maps:get(restart, ChildSpec)),
    ?assertEqual(worker, maps:get(type, ChildSpec)).

%%% Child spec structure tests

child_spec_uses_repl_shell_test() ->
    {ok, {_SupFlags, [ChildSpec]}} = beamtalk_session_sup:init([]),

    %% Should use beamtalk_repl_shell:start_link/1
    ?assertEqual({beamtalk_repl_shell, start_link, []}, maps:get(start, ChildSpec)).

%%% Client-kind normalisation tests (session origin metadata)

normalize_kind_accepts_known_binaries_test() ->
    [
        ?assertEqual(K, beamtalk_session_sup:normalize_kind(K))
     || K <- beamtalk_session_sup:known_kinds()
    ].

normalize_kind_lowercases_test() ->
    ?assertEqual(<<"repl">>, beamtalk_session_sup:normalize_kind(<<"REPL">>)),
    ?assertEqual(<<"liveview">>, beamtalk_session_sup:normalize_kind(<<"LiveView">>)).

normalize_kind_accepts_atoms_test() ->
    ?assertEqual(<<"mcp">>, beamtalk_session_sup:normalize_kind(mcp)),
    ?assertEqual(<<"lsp">>, beamtalk_session_sup:normalize_kind(lsp)).

normalize_kind_accepts_strings_test() ->
    ?assertEqual(<<"ide">>, beamtalk_session_sup:normalize_kind("ide")).

normalize_kind_unknown_string_falls_back_test() ->
    ?assertEqual(<<"unknown">>, beamtalk_session_sup:normalize_kind(<<"bogus">>)),
    ?assertEqual(<<"unknown">>, beamtalk_session_sup:normalize_kind(<<"">>)).

normalize_kind_undefined_and_garbage_fall_back_test() ->
    ?assertEqual(<<"unknown">>, beamtalk_session_sup:normalize_kind(undefined)),
    ?assertEqual(<<"unknown">>, beamtalk_session_sup:normalize_kind(42)),
    ?assertEqual(<<"unknown">>, beamtalk_session_sup:normalize_kind({tuple})).

known_kinds_includes_user_visible_surfaces_test() ->
    Known = beamtalk_session_sup:known_kinds(),
    [
        ?assert(lists:member(K, Known))
     || K <- [<<"repl">>, <<"mcp">>, <<"lsp">>, <<"liveview">>, <<"unknown">>]
    ].

%%% Integration test

supervisor_can_start_test() ->
    %% Start the supervisor
    {ok, Pid} = beamtalk_session_sup:start_link(),
    try
        %% Verify it's running
        ?assert(is_process_alive(Pid)),

        %% Get supervisor info
        Children = supervisor:which_children(Pid),

        %% Should have no children initially
        ?assertEqual([], Children)
    after
        stop_sup(Pid)
    end.

supervisor_count_children_test() ->
    %% Start the supervisor
    {ok, Pid} = beamtalk_session_sup:start_link(),
    try
        %% Count children
        Counts = supervisor:count_children(Pid),

        %% Initially should have 0 active children
        ?assertEqual(0, proplists:get_value(active, Counts)),
        ?assertEqual(0, proplists:get_value(workers, Counts))
    after
        stop_sup(Pid)
    end.

%%% stop_session/1 tests

stop_session_unknown_pid_returns_not_found_test() ->
    {ok, Sup} = beamtalk_session_sup:start_link(),
    try
        %% A *live* pid that is not a child of this supervisor. It must stay alive
        %% for the duration of the call: for a `simple_one_for_one` supervisor,
        %% `supervisor:terminate_child/2` only reports `{error, not_found}` for a
        %% pid it can see is still alive. A pid that has already exited is treated
        %% as an already-terminated child and yields a harmless `ok`
        %% (`supervisor:find_child/2` → `is_process_alive/1` → `false` branch). The
        %% previous `spawn(fun() -> ok end)` raced that exit: under scheduler load
        %% the process was usually dead by the time `stop_session/1` ran, so the
        %% call returned `ok` and this assertion flaked (BT-2523). Keep `Other`
        %% blocked until the assertion has run, then release it.
        Other = spawn(fun() ->
            receive
                stop -> ok
            end
        end),
        try
            ?assertEqual({error, not_found}, beamtalk_session_sup:stop_session(Other))
        after
            Other ! stop
        end
    after
        stop_sup(Sup)
    end.

stop_session_terminates_live_session_test() ->
    {ok, Sup} = beamtalk_session_sup:start_link(),
    try
        %% Start a real session under supervision.
        {ok, SessionPid} = beamtalk_session_sup:start_session(<<"stop-session-test">>),
        ?assert(is_process_alive(SessionPid)),
        ?assertEqual(1, proplists:get_value(active, supervisor:count_children(Sup))),

        %% Stopping it terminates the child and drops the active count.
        ?assertEqual(ok, beamtalk_session_sup:stop_session(SessionPid)),
        ?assertNot(is_process_alive(SessionPid)),
        ?assertEqual(0, proplists:get_value(active, supervisor:count_children(Sup))),

        %% Idempotent: a second stop on the just-terminated child is a non-fatal
        %% no-op. `supervisor:terminate_child/2` returns `ok` for a dynamic child it
        %% has recently terminated (vs `{error, not_found}` for a pid that was never
        %% a child of this supervisor — covered above). Either way the call site
        %% treats it as best-effort.
        ?assertEqual(ok, beamtalk_session_sup:stop_session(SessionPid))
    after
        stop_sup(Sup)
    end.

%%% Test helpers

%% Synchronously stop the (registered) session supervisor and wait until it —
%% and therefore its `{local, beamtalk_session_sup}` name — is gone before the
%% test returns. `exit(Sup, normal)` is asynchronous: under CI load/ordering the
%% supervisor could survive into the next test, so a fresh `start_link/0` (or the
%% workspace supervisor starting it as a registered child) would see
%% `{already_started, <pid>}` and the "no supervisor" / "unknown pid" expectations
%% would break. This is the teardown-race half of BT-2523. `unlink/1` first so the
%% shutdown exit signal does not also fell this (non-trapping) test process; the
%% registered name is cleared atomically as part of termination, so by the time the
%% `DOWN` arrives `whereis(beamtalk_session_sup)` is already `undefined`.
stop_sup(Sup) when is_pid(Sup) ->
    Ref = monitor(process, Sup),
    unlink(Sup),
    exit(Sup, shutdown),
    receive
        {'DOWN', Ref, process, Sup, _} -> ok
    after 5000 -> error(timeout_stopping_session_sup)
    end.
