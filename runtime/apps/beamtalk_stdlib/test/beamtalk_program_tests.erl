%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_program_tests).

-moduledoc """
Unit tests for the `Program` class runtime (`beamtalk_program`), focused on the
exit policy (ADR 0099 §3, BT-3634), which `beamtalk_capability:exit_policy/0`
derives from the recorded capabilities.

The graceful stop of a program-owned node calls `init:stop/1` and cannot be
exercised under EUnit (it would stop the test VM); those paths are covered by the
release/run e2e tests. These tests pin the other three outcomes: the entry-chain
`script_exit` throw, the structured `program_exit` (no workspace) and
`program_exit_outside_entry` (actor in a shared workspace) errors, and the
value/type validation that runs before the policy gate.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% Record capabilities for the duration of a test and restore whatever was
%% recorded before (a bare EUnit VM has none).
caps_setup(Caps) ->
    Prev = beamtalk_capability:recorded(),
    restore(Caps),
    Prev.

restore(none) -> beamtalk_capability:clear();
restore({ok, Caps}) -> beamtalk_capability:set(Caps);
restore(Caps) when is_map(Caps) -> beamtalk_capability:set(Caps).

caps_teardown(Prev) ->
    restore(Prev),
    ok.

with_caps(Caps, Tests) ->
    {setup, fun() -> caps_setup(Caps) end, fun caps_teardown/1, fun(_) -> Tests end}.

-define(SHARED, #{mode => workspace, include_compiler => true, node_owning => false}).
-define(OWNED, #{mode => run, include_compiler => true, node_owning => true}).

%% In a shared workspace, on the entry method's own call chain, `Program exit:
%% Code` throws the tagged `{beamtalk_script_exit, Code}` signal the session
%% evaluator catches.
shared_exit_with_code_throws_script_exit_test_() ->
    with_caps(?SHARED, [
        ?_assertThrow({beamtalk_script_exit, 5}, beamtalk_program:'exit:'(5)),
        ?_assertThrow({beamtalk_script_exit, 0}, beamtalk_program:'exit:'(0)),
        ?_assertThrow({beamtalk_script_exit, 255}, beamtalk_program:'exit:'(255)),
        %% Zero-arg `exit`/`exit:` shim both route through `exit:(0)`.
        ?_assertThrow({beamtalk_script_exit, 0}, beamtalk_program:exit()),
        ?_assertThrow({beamtalk_script_exit, 7}, beamtalk_program:exit(7))
    ]).

%% A program-owned node still throws on the entry chain: the harness at the top
%% of the chain (script harness, `eval`, `rpc`) decides what to do with it.
owned_exit_on_entry_chain_throws_script_exit_test_() ->
    with_caps(?OWNED, [
        ?_assertThrow({beamtalk_script_exit, 3}, beamtalk_program:'exit:'(3))
    ]).

%% From an actor (off any harness's call chain) in a shared workspace there is no
%% session to end: a structured error replaces the old `nocatch` crash.
shared_exit_from_actor_raises_program_exit_outside_entry_test_() ->
    with_caps(?SHARED, [
        fun() ->
            Old = beamtalk_actor_state:install(#{}),
            try
                ?assertError(
                    #{
                        error := #beamtalk_error{
                            kind = program_exit_outside_entry,
                            details = #{status := 4}
                        }
                    },
                    beamtalk_program:'exit:'(4)
                )
            after
                beamtalk_actor_state:uninstall(Old)
            end
        end
    ]).

%% With no workspace (`beamtalk test`, a bare runtime) it raises a structured,
%% assertable `program_exit` error carrying the status, never a raw throw.
no_workspace_exit_raises_program_exit_test_() ->
    with_caps(none, [
        ?_assertError(
            #{error := #beamtalk_error{kind = program_exit, details = #{status := 3}}},
            beamtalk_program:'exit:'(3)
        ),
        ?_assertError(
            #{error := #beamtalk_error{kind = program_exit, details = #{status := 0}}},
            beamtalk_program:exit()
        )
    ]).

%% Out-of-range is a wrong *value*, validated before the node-owning gate, so it
%% raises a structured error in any context (consistent with `System halt:`).
exit_out_of_range_raises_invalid_argument_test_() ->
    with_caps(?SHARED, [
        ?_assertError(
            #{error := #beamtalk_error{kind = invalid_argument}},
            beamtalk_program:'exit:'(999)
        ),
        ?_assertError(
            #{error := #beamtalk_error{kind = invalid_argument}},
            beamtalk_program:'exit:'(-1)
        )
    ]).

%% Non-integer is a wrong *type*, also validated before the node-owning gate.
exit_non_integer_raises_type_error_test_() ->
    with_caps(?SHARED, [
        ?_assertError(
            #{error := #beamtalk_error{kind = type_error}},
            beamtalk_program:'exit:'(<<"two">>)
        )
    ]).

%%====================================================================
%% commandName/0
%%====================================================================

%% When `program_name` is set as a binary it is returned directly (line 52).
commandName_returns_binary_when_set_as_binary_test() ->
    Prev = application:get_env(beamtalk_runtime, program_name),
    application:set_env(beamtalk_runtime, program_name, <<"my_app">>),
    try
        ?assertEqual(<<"my_app">>, beamtalk_program:commandName())
    after
        case Prev of
            undefined -> application:unset_env(beamtalk_runtime, program_name);
            {ok, V} -> application:set_env(beamtalk_runtime, program_name, V)
        end
    end.

%% When `program_name` is set as a charlist it is converted via
%% `list_to_binary/1` before being returned (line 53).
commandName_returns_binary_when_set_as_list_test() ->
    Prev = application:get_env(beamtalk_runtime, program_name),
    application:set_env(beamtalk_runtime, program_name, "my_escript"),
    try
        ?assertEqual(<<"my_escript">>, beamtalk_program:commandName())
    after
        case Prev of
            undefined -> application:unset_env(beamtalk_runtime, program_name);
            {ok, V} -> application:set_env(beamtalk_runtime, program_name, V)
        end
    end.

%%====================================================================
%% package/0
%%====================================================================

%% Helpers to save and restore the `root_package` app-env key.
save_root_package() ->
    application:get_env(beamtalk_runtime, root_package).

restore_root_package(undefined) ->
    application:unset_env(beamtalk_runtime, root_package);
restore_root_package({ok, V}) ->
    application:set_env(beamtalk_runtime, root_package, V).

%% When no root package has been set `package/0` raises `no_program_package`.
package_raises_no_program_package_when_undefined_test() ->
    Prev = save_root_package(),
    application:unset_env(beamtalk_runtime, root_package),
    try
        ?assertError(
            #{error := #beamtalk_error{kind = no_program_package}},
            beamtalk_program:package()
        )
    after
        restore_root_package(Prev)
    end.

%% When the root package is set to `ambiguous` (several packages under test),
%% `package/0` raises `ambiguous_program_package` (line 93).
package_raises_ambiguous_program_package_test() ->
    Prev = save_root_package(),
    beamtalk_package:set_ambiguous_root_package(),
    try
        ?assertError(
            #{error := #beamtalk_error{kind = ambiguous_program_package}},
            beamtalk_program:package()
        )
    after
        restore_root_package(Prev)
    end.

%% When a root package name is recorded but its OTP application is not on the
%% code path, `package/0` raises `package_not_loaded` (lines 82, 86).
package_raises_package_not_loaded_when_app_missing_test() ->
    Prev = save_root_package(),
    application:set_env(beamtalk_runtime, root_package, <<"bt_nonexistent_pkg_eunit">>),
    try
        ?assertError(
            #{error := #beamtalk_error{kind = package_not_loaded}},
            beamtalk_program:package()
        )
    after
        restore_root_package(Prev)
    end.

%%====================================================================
%% rootSupervisor (moved from Workspace, BT-3633)
%%====================================================================

root_supervisor_returns_nil_when_not_registered_test() ->
    %% rootSupervisor/0 returns nil when no root supervisor has been registered.
    (try
        ets:delete(beamtalk_root_supervisor)
    catch
        _:_ -> ok
    end),
    ?assertEqual(nil, beamtalk_program:rootSupervisor()).

root_supervisor_returns_registered_value_test() ->
    %% rootSupervisor/0 returns the tuple registered via beamtalk_supervisor:register_root/1.
    (try
        ets:delete(beamtalk_root_supervisor)
    catch
        _:_ -> ok
    end),
    SupTuple = {beamtalk_supervisor, 'AppSup', 'bt@my_app@app_sup', self()},
    beamtalk_supervisor:register_root(SupTuple),
    try
        ?assertEqual(SupTuple, beamtalk_program:rootSupervisor())
    after
        (try
            ets:delete(beamtalk_root_supervisor)
        catch
            _:_ -> ok
        end)
    end.
