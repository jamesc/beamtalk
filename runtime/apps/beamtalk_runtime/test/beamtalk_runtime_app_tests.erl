%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_runtime_app module
%%%
%%% Tests application behavior callbacks (start/stop).
%%%
%%% Note: These tests may run in a shared BEAM node where the runtime
%%% is already started by other tests. We handle `already_started`
%%% gracefully to avoid test pollution issues.

-module(beamtalk_runtime_app_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Helper to start runtime, handling already_started case
start_runtime() ->
    case beamtalk_runtime_app:start(normal, []) of
        {ok, Pid} -> {started, Pid};
        {error, {already_started, Pid}} -> {already_running, Pid}
    end.

%% Helper to stop runtime only if we started it
stop_runtime({started, Pid}) ->
    unlink(Pid),
    Ref = monitor(process, Pid),
    exit(Pid, shutdown),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 1000 ->
        error(supervisor_cleanup_timeout)
    end;
stop_runtime({already_running, _Pid}) ->
    %% Don't stop - it was already running before our test
    ok.

%%====================================================================
%% Tests
%%====================================================================

%%% Application start tests

start_returns_supervisor_pid_test() ->
    %% Start (or get already running) supervisor
    Result = start_runtime(),
    Pid = element(2, Result),

    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),

    %% Verify we can query it as a supervisor
    Children = supervisor:which_children(Pid),
    ?assert(is_list(Children)),

    %% Clean up only if we started it
    stop_runtime(Result).

%%% Application stop tests

stop_test() ->
    %% Stop should always return ok
    ?assertEqual(ok, beamtalk_runtime_app:stop([])),
    ?assertEqual(ok, beamtalk_runtime_app:stop(some_state)),
    ?assertEqual(ok, beamtalk_runtime_app:stop(#{})),
    ?assertEqual(ok, beamtalk_runtime_app:stop(undefined)).

%%% Behavioral tests

behaviour_implementation_test() ->
    %% Verify the module implements the application behaviour
    Behaviors = proplists:get_value(behaviour, beamtalk_runtime_app:module_info(attributes), []),
    ?assert(lists:member(application, Behaviors)).

exports_required_callbacks_test() ->
    %% Verify required callbacks are exported
    Exports = beamtalk_runtime_app:module_info(exports),
    ?assert(lists:member({start, 2}, Exports)),
    ?assert(lists:member({stop, 1}, Exports)).

%%% Stdlib auto-loading tests

system_dictionary_loaded_after_start_test() ->
    %% Start (or get already running) supervisor
    Result = start_runtime(),

    %% The SystemDictionary module provides system reflection (allClasses, classNamed:, etc.)
    %% via beamtalk_system_dictionary, replacing the legacy compiled beamtalk module.
    %% Use ensure_loaded to verify the module is available (not just loaded as side-effect).
    ?assertEqual(
        {module, beamtalk_system_dictionary},
        code:ensure_loaded(beamtalk_system_dictionary)
    ),
    ?assertEqual(true, beamtalk_system_dictionary:has_method(allClasses)),

    %% Clean up only if we started it
    stop_runtime(Result).
