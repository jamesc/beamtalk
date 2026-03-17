%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Runtime Context

%%% @doc EUnit tests for beamtalk_logging_config module.
%%%
%%% Tests log-level management, debug-target toggling, and logger
%%% introspection.

-module(beamtalk_logging_config_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Setup / teardown
%%====================================================================

%% Restore the log level to its original value after every test group.
logging_config_test_() ->
    {setup,
        fun() ->
            OrigLevel = beamtalk_logging_config:logLevel(),
            beamtalk_logging_config:ensure_table(),
            OrigLevel
        end,
        fun(OrigLevel) ->
            beamtalk_logging_config:disableAllDebug(),
            beamtalk_logging_config:logLevel(OrigLevel)
        end,
        [
            fun logLevel_returns_atom/0,
            fun logLevel_set_valid_changes_level/0,
            fun logLevel_set_invalid_returns_error/0,
            fun logLevel_set_non_atom_returns_error/0,
            fun debugTargets_returns_expected_list/0,
            fun enableDebug_valid_symbol_succeeds/0,
            fun enableDebug_invalid_symbol_returns_error/0,
            fun disableDebug_removes_target/0,
            fun disableAllDebug_clears_all/0,
            fun loggerInfo_returns_binary/0,
            fun activeDebugTargets_tracks_multiple/0,
            fun enable_disable_cycle_removes_target/0,
            fun enableDebug_domain_target_succeeds/0,
            fun disableDebug_domain_target_removes/0
        ]}.

%%====================================================================
%% Tests
%%====================================================================

logLevel_returns_atom() ->
    Level = beamtalk_logging_config:logLevel(),
    ?assert(is_atom(Level)).

logLevel_set_valid_changes_level() ->
    OrigLevel = beamtalk_logging_config:logLevel(),
    %% Set to debug
    ?assertEqual(nil, beamtalk_logging_config:logLevel(debug)),
    ?assertEqual(debug, beamtalk_logging_config:logLevel()),
    %% Set to warning
    ?assertEqual(nil, beamtalk_logging_config:logLevel(warning)),
    ?assertEqual(warning, beamtalk_logging_config:logLevel()),
    %% Restore
    beamtalk_logging_config:logLevel(OrigLevel).

logLevel_set_invalid_returns_error() ->
    Result = beamtalk_logging_config:logLevel(banana),
    ?assertMatch(#beamtalk_error{kind = type_error}, Result).

logLevel_set_non_atom_returns_error() ->
    Result = beamtalk_logging_config:logLevel(<<"debug">>),
    ?assertMatch(#beamtalk_error{kind = type_error}, Result).

debugTargets_returns_expected_list() ->
    Targets = beamtalk_logging_config:debugTargets(),
    ?assert(is_list(Targets)),
    ?assert(lists:member(actor, Targets)),
    ?assert(lists:member(supervisor, Targets)),
    ?assert(lists:member(dispatch, Targets)),
    ?assert(lists:member(compiler, Targets)),
    ?assert(lists:member(workspace, Targets)),
    ?assert(lists:member(stdlib, Targets)),
    ?assert(lists:member(hotreload, Targets)),
    ?assert(lists:member(runtime, Targets)),
    ?assert(lists:member(user, Targets)).

enableDebug_valid_symbol_succeeds() ->
    beamtalk_logging_config:disableAllDebug(),
    ?assertEqual(nil, beamtalk_logging_config:enableDebug(actor)),
    Active = beamtalk_logging_config:activeDebugTargets(),
    ?assert(lists:member(actor, Active)),
    beamtalk_logging_config:disableDebug(actor).

enableDebug_invalid_symbol_returns_error() ->
    Result = beamtalk_logging_config:enableDebug(nonexistent),
    ?assertMatch(#beamtalk_error{kind = type_error}, Result).

disableDebug_removes_target() ->
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_logging_config:enableDebug(dispatch),
    ?assert(lists:member(dispatch, beamtalk_logging_config:activeDebugTargets())),
    ?assertEqual(nil, beamtalk_logging_config:disableDebug(dispatch)),
    ?assertNot(lists:member(dispatch, beamtalk_logging_config:activeDebugTargets())).

disableAllDebug_clears_all() ->
    beamtalk_logging_config:enableDebug(actor),
    beamtalk_logging_config:enableDebug(dispatch),
    ?assert(length(beamtalk_logging_config:activeDebugTargets()) >= 2),
    ?assertEqual(nil, beamtalk_logging_config:disableAllDebug()),
    ?assertEqual([], beamtalk_logging_config:activeDebugTargets()).

loggerInfo_returns_binary() ->
    Info = beamtalk_logging_config:loggerInfo(),
    ?assert(is_binary(Info)),
    ?assertNotEqual(<<>>, Info).

activeDebugTargets_tracks_multiple() ->
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_logging_config:enableDebug(actor),
    beamtalk_logging_config:enableDebug(stdlib),
    Active = beamtalk_logging_config:activeDebugTargets(),
    ?assert(lists:member(actor, Active)),
    ?assert(lists:member(stdlib, Active)),
    beamtalk_logging_config:disableAllDebug().

enable_disable_cycle_removes_target() ->
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_logging_config:enableDebug(hotreload),
    ?assert(lists:member(hotreload, beamtalk_logging_config:activeDebugTargets())),
    beamtalk_logging_config:disableDebug(hotreload),
    ?assertNot(lists:member(hotreload, beamtalk_logging_config:activeDebugTargets())).

enableDebug_domain_target_succeeds() ->
    beamtalk_logging_config:disableAllDebug(),
    ?assertEqual(nil, beamtalk_logging_config:enableDebug(runtime)),
    ?assert(lists:member(runtime, beamtalk_logging_config:activeDebugTargets())),
    beamtalk_logging_config:disableDebug(runtime).

disableDebug_domain_target_removes() ->
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_logging_config:enableDebug(user),
    ?assert(lists:member(user, beamtalk_logging_config:activeDebugTargets())),
    beamtalk_logging_config:disableDebug(user),
    ?assertNot(lists:member(user, beamtalk_logging_config:activeDebugTargets())).
