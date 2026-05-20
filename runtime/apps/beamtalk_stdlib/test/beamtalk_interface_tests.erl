%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_interface_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_interface module.

Tests log_compiler_diagnostics/2 (BT-2219): verifies that an OTP logger call
fires at the appropriate level when the compiler port returns {error, Diagnostics}
in findSendersIn/2 and findReferencesToIn/2, while [] is still returned
(per-method fault-tolerance contract unchanged).
""".

%% Logger handler callback for log capture
-export([log/2]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% Logger handler callback
%%% ============================================================================

%% Capture log events and forward to the test process.
log(LogEvent, #{config := #{parent := Parent}}) ->
    Parent ! {captured_log, LogEvent},
    ok.

%%% ============================================================================
%%% Helpers
%%% ============================================================================

install_capture_handler() ->
    HandlerId = beamtalk_interface_test_capture,
    ok = logger:add_handler(HandlerId, ?MODULE, #{
        config => #{parent => self()},
        level => all
    }),
    %% Lower the primary logger level to 'all' so our capture handler receives
    %% warning/error events. The sys.config for tests sets primary level to 'error'
    %% to suppress noise; we restore it after the test.
    #{level := OrigLevel} = logger:get_primary_config(),
    ok = logger:set_primary_config(level, all),
    {HandlerId, OrigLevel}.

remove_capture_handler({HandlerId, OrigLevel}) ->
    logger:remove_handler(HandlerId),
    logger:set_primary_config(level, OrigLevel).

%% Drain one log event from the mailbox that matches a given level.
%% Returns {ok, LogEvent} | not_found.
collect_log(Level, Timeout) ->
    receive
        {captured_log, #{level := Level} = E} ->
            {ok, E};
        {captured_log, _Other} ->
            collect_log(Level, Timeout)
    after Timeout ->
        not_found
    end.

%%% ============================================================================
%%% BT-2219: log_compiler_diagnostics/2 — warning for parse failure
%%% ============================================================================

log_compiler_diagnostics_warning_test() ->
    %% Diagnostics that do NOT indicate port unavailability should trigger
    %% a LOG_WARNING.
    Handle = install_capture_handler(),
    try
        Diagnostics = [#{message => <<"Parse error: unexpected token">>}],
        beamtalk_interface:log_compiler_diagnostics(Diagnostics, 'findSendersIn:selector:'),
        Result = collect_log(warning, 500),
        ?assertMatch({ok, _}, Result),
        {ok, Event} = Result,
        %% Verify domain metadata is present
        #{meta := Meta} = Event,
        ?assertEqual([beamtalk, stdlib], maps:get(domain, Meta))
    after
        remove_capture_handler(Handle)
    end.

%%% ============================================================================
%%% BT-2219: log_compiler_diagnostics/2 — error for port unavailability
%%% ============================================================================

log_compiler_diagnostics_error_on_port_unavailable_test() ->
    %% Diagnostics that indicate the compiler server is not available should
    %% trigger a LOG_ERROR (not just a warning).
    Handle = install_capture_handler(),
    try
        Diagnostics = [#{message => <<"Compiler server is not available">>}],
        beamtalk_interface:log_compiler_diagnostics(Diagnostics, 'findReferencesToIn:class:'),
        Result = collect_log(error, 500),
        ?assertMatch({ok, _}, Result),
        {ok, Event} = Result,
        #{meta := Meta} = Event,
        ?assertEqual([beamtalk, stdlib], maps:get(domain, Meta))
    after
        remove_capture_handler(Handle)
    end.

%%% ============================================================================
%%% BT-2219: log_compiler_diagnostics/2 — error for port timeout
%%% ============================================================================

log_compiler_diagnostics_error_on_timeout_test() ->
    %% "timed out" diagnostics also escalate to LOG_ERROR.
    Handle = install_capture_handler(),
    try
        Diagnostics = [#{message => <<"Compiler server timed out">>}],
        beamtalk_interface:log_compiler_diagnostics(Diagnostics, 'findSendersIn:selector:'),
        Result = collect_log(error, 500),
        ?assertMatch({ok, _}, Result),
        {ok, Event} = Result,
        #{meta := Meta} = Event,
        ?assertEqual([beamtalk, stdlib], maps:get(domain, Meta))
    after
        remove_capture_handler(Handle)
    end.

%%% ============================================================================
%%% BT-2219: findSendersIn/2 — still returns a list (happy path)
%%% ============================================================================

find_senders_in_returns_list_test() ->
    %% Even when the compiler returns an error (here we rely on whatever the
    %% compiler does with invalid source that cannot be parsed), findSendersIn/2
    %% must return []. The per-method fault-tolerance contract is unchanged.
    %%
    %% With a running compiler server, calling with empty binary produces {ok, []}
    %% (compiler returns empty senders for empty source), not an error. We test
    %% the error path via log_compiler_diagnostics/2 directly (above).
    %% This test confirms the happy-path return is still a list.
    Result = beamtalk_interface:findSendersIn(<<"x + 1">>, <<"ifTrue:">>),
    ?assert(is_list(Result)).

%%% ============================================================================
%%% BT-2219: findReferencesToIn/2 — still returns a list (happy path)
%%% ============================================================================

find_references_to_in_returns_list_test() ->
    %% As above: the happy path returns a list. The error-path is tested via
    %% log_compiler_diagnostics/2 directly.
    Result = beamtalk_interface:findReferencesToIn(<<"x + 1">>, 'Integer'),
    ?assert(is_list(Result)).
