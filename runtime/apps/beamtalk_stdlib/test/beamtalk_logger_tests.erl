%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_logger_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_logger module.

Tests setLevel:/1 and its FFI shim. Log methods (info:, warn:, error:,
debug: and their metadata: variants) are now compiler intrinsics
(BT-1478) and tested via stdlib/test/ Beamtalk tests.
""".

%% Logger handler callback for log capture in tests.
-export([log/2]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

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
%%% FFI Shim
%%% ============================================================================

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
%%% setLevel: deprecation warning
%%% ============================================================================

-doc "Helper to reset the deprecation persistent_term before each test.".
reset_deprecation_flag() ->
    _ = persistent_term:erase(beamtalk_logger_setlevel_deprecated),
    ok.

-doc """
Install a logger handler that captures log events into the calling
process's mailbox, so we can assert on warning messages.
""".
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
    Pid ! {captured_log, LogEvent},
    ok.

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
            error("Expected deprecation warning not received")
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
            {captured_log, #{level := warning, msg := {string, _}}} -> ok
        after 1000 ->
            error("Expected first deprecation warning not received")
        end,
        %% Second call — should NOT emit warning
        ?assertEqual(nil, beamtalk_logger:'setLevel:'(debug)),
        receive
            {captured_log, #{level := warning, msg := {string, _}}} ->
                error("Deprecation warning should only be emitted once")
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
