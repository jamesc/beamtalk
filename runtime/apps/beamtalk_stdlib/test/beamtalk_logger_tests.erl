%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_logger module.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Tests info:/1, info:metadata:/2, warn:/1, warn:metadata:/2,
%%% error:/1, error:metadata:/2, debug:/1, debug:metadata:/2,
%%% setLevel:/1, FFI shims, and type error paths.

-module(beamtalk_logger_tests).

%% Logger handler callback for log capture in tests.
-export([log/2]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% info:/1
%%% ============================================================================

info_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_logger:'info:'(<<"hello">>)).

info_non_binary_raises_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'info:'(42)
    ).

%%% ============================================================================
%%% info:metadata:/2
%%% ============================================================================

info_metadata_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_logger:'info:metadata:'(<<"hello">>, #{key => val})).

info_metadata_non_binary_msg_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'info:metadata:'(42, #{})
    ).

info_metadata_non_map_meta_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'info:metadata:'(<<"hello">>, not_a_map)
    ).

%%% ============================================================================
%%% warn:/1
%%% ============================================================================

warn_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_logger:'warn:'(<<"caution">>)).

warn_non_binary_raises_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'warn:'(123)
    ).

%%% ============================================================================
%%% warn:metadata:/2
%%% ============================================================================

warn_metadata_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_logger:'warn:metadata:'(<<"caution">>, #{a => 1})).

warn_metadata_non_binary_msg_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'warn:metadata:'(123, #{})
    ).

warn_metadata_non_map_meta_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'warn:metadata:'(<<"caution">>, not_a_map)
    ).

%%% ============================================================================
%%% error:/1
%%% ============================================================================

error_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_logger:'error:'(<<"failure">>)).

error_non_binary_raises_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'error:'(false)
    ).

%%% ============================================================================
%%% error:metadata:/2
%%% ============================================================================

error_metadata_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_logger:'error:metadata:'(<<"failure">>, #{reason => timeout})).

error_metadata_non_binary_msg_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'error:metadata:'(false, #{})
    ).

error_metadata_non_map_meta_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'error:metadata:'(<<"failure">>, not_a_map)
    ).

%%% ============================================================================
%%% debug:/1
%%% ============================================================================

debug_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_logger:'debug:'(<<"trace">>)).

debug_non_binary_raises_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'debug:'([not_binary])
    ).

%%% ============================================================================
%%% debug:metadata:/2
%%% ============================================================================

debug_metadata_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_logger:'debug:metadata:'(<<"trace">>, #{count => 1})).

debug_metadata_non_binary_msg_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'debug:metadata:'([not_binary], #{})
    ).

debug_metadata_non_map_meta_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'debug:metadata:'(<<"trace">>, not_a_map)
    ).

%%% ============================================================================
%%% setLevel:/1
%%% ============================================================================

set_level_valid_level_test() ->
    %% Save original level, set to debug, then restore
    _ = persistent_term:erase(beamtalk_logger_setlevel_deprecated),
    #{level := OrigLevel} = logger:get_primary_config(),
    try
        ?assertEqual(nil, beamtalk_logger:'setLevel:'(debug)),
        #{level := debug} = logger:get_primary_config()
    after
        logger:set_primary_config(level, OrigLevel)
    end.

set_level_invalid_atom_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = argument_error}},
        beamtalk_logger:'setLevel:'(not_a_real_level)
    ).

set_level_non_atom_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'setLevel:'(<<"debug">>)
    ).

%%% ============================================================================
%%% FFI Shims
%%% ============================================================================

info_shim_test() ->
    ?assertEqual(nil, beamtalk_logger:info(<<"shim info">>)).

info_shim_metadata_test() ->
    ?assertEqual(nil, beamtalk_logger:info(<<"shim info">>, #{k => v})).

warn_shim_test() ->
    ?assertEqual(nil, beamtalk_logger:warn(<<"shim warn">>)).

warn_shim_metadata_test() ->
    ?assertEqual(nil, beamtalk_logger:warn(<<"shim warn">>, #{k => v})).

error_shim_test() ->
    ?assertEqual(nil, beamtalk_logger:error(<<"shim error">>)).

error_shim_metadata_test() ->
    ?assertEqual(nil, beamtalk_logger:error(<<"shim error">>, #{k => v})).

debug_shim_test() ->
    ?assertEqual(nil, beamtalk_logger:debug(<<"shim debug">>)).

debug_shim_metadata_test() ->
    ?assertEqual(nil, beamtalk_logger:debug(<<"shim debug">>, #{k => v})).

set_level_shim_test() ->
    _ = persistent_term:erase(beamtalk_logger_setlevel_deprecated),
    #{level := OrigLevel} = logger:get_primary_config(),
    try
        ?assertEqual(nil, beamtalk_logger:setLevel(info)),
        #{level := info} = logger:get_primary_config()
    after
        logger:set_primary_config(level, OrigLevel)
    end.

%%% ============================================================================
%%% logError shims (Object#error: intrinsic workaround)
%%% ============================================================================

log_error_shim_test() ->
    ?assertEqual(nil, beamtalk_logger:logError(<<"shim logError">>)).

log_error_shim_metadata_test() ->
    ?assertEqual(nil, beamtalk_logger:logError(<<"shim logError">>, #{k => v})).

%%% ============================================================================
%%% setLevel: deprecation warning
%%% ============================================================================

%% @doc Helper to reset the deprecation persistent_term before each test.
reset_deprecation_flag() ->
    _ = persistent_term:erase(beamtalk_logger_setlevel_deprecated),
    ok.

%% @doc Install a logger handler that captures log events into the calling
%% process's mailbox, so we can assert on warning messages.
install_capture_handler() ->
    HandlerId = beamtalk_logger_test_capture,
    Self = self(),
    logger:add_handler(HandlerId, logger_std_h, #{
        config => #{type => standard_io},
        filter_default => stop,
        filters => [{pass_all, {fun(Event, _) -> Event end, ok}}],
        level => all
    }),
    %% Replace with a custom handler function that sends to the test process
    logger:remove_handler(HandlerId),
    logger:add_handler(HandlerId, ?MODULE, #{
        level => all,
        filter_default => log,
        capture_pid => Self
    }),
    HandlerId.

remove_capture_handler(HandlerId) ->
    logger:remove_handler(HandlerId),
    ok.

%% Logger handler callback — sends log events to the capture pid.
log(LogEvent, #{capture_pid := Pid}) ->
    Pid ! {captured_log, LogEvent}.

set_level_emits_deprecation_warning_test() ->
    reset_deprecation_flag(),
    #{level := OrigLevel} = logger:get_primary_config(),
    HandlerId = install_capture_handler(),
    try
        %% Temporarily allow warning-level logs so our deprecation warning is emitted
        logger:set_primary_config(level, warning),
        ?assertEqual(nil, beamtalk_logger:'setLevel:'(info)),
        %% Verify the level was actually set
        #{level := info} = logger:get_primary_config(),
        %% Verify the deprecation warning was emitted
        receive
            {captured_log, #{level := warning, msg := {string, Msg}}} ->
                ?assertEqual(
                    "Logger setLevel: is deprecated. Use Beamtalk logLevel: instead.",
                    Msg
                )
        after 1000 ->
            ?assert(false, "Expected deprecation warning not received")
        end
    after
        remove_capture_handler(HandlerId),
        logger:set_primary_config(level, OrigLevel)
    end.

set_level_deprecation_warning_only_once_test() ->
    reset_deprecation_flag(),
    #{level := OrigLevel} = logger:get_primary_config(),
    HandlerId = install_capture_handler(),
    try
        logger:set_primary_config(level, warning),
        %% First call — should emit warning
        ?assertEqual(nil, beamtalk_logger:'setLevel:'(info)),
        receive
            {captured_log, #{level := warning}} -> ok
        after 1000 ->
            ?assert(false, "Expected first deprecation warning not received")
        end,
        %% Second call — should NOT emit warning
        ?assertEqual(nil, beamtalk_logger:'setLevel:'(debug)),
        receive
            {captured_log, #{level := warning}} ->
                ?assert(false, "Deprecation warning should only be emitted once")
        after 200 ->
            ok
        end
    after
        remove_capture_handler(HandlerId),
        logger:set_primary_config(level, OrigLevel)
    end.

set_level_still_works_after_deprecation_test() ->
    reset_deprecation_flag(),
    #{level := OrigLevel} = logger:get_primary_config(),
    try
        ?assertEqual(nil, beamtalk_logger:'setLevel:'(debug)),
        #{level := debug} = logger:get_primary_config(),
        ?assertEqual(nil, beamtalk_logger:'setLevel:'(info)),
        #{level := info} = logger:get_primary_config()
    after
        logger:set_primary_config(level, OrigLevel)
    end.
