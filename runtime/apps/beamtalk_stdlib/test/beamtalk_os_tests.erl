%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_os_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_os module (BT-1247).

Tests cover:
- run: success path (trimmed stdout)
- run: type error (non-binary argument)
- run:timeout: success path with explicit timeout
- run:timeout: type error (non-binary command)
- run:timeout: type error (non-integer timeout)
- run:timeout: timeout expiry raises #timeout error
- output exceeding MAX_OUTPUT_BYTES raises output_too_large error
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% run: — success path
%%% ============================================================================

run_returns_trimmed_stdout_test() ->
    %% `echo hello` works on both Unix (/bin/echo on PATH) and Windows (cmd builtin).
    ?assertEqual(<<"hello">>, beamtalk_os:'run:'(<<"echo hello">>)).

run_trims_trailing_newline_test() ->
    case os:type() of
        {unix, _} ->
            Result = beamtalk_os:'run:'(<<"printf 'line1\nline2\n'">>),
            ?assertEqual(<<"line1\nline2">>, Result);
        {win32, _} ->
            %% cmd echo emits \r\n; string:trim(trailing) strips the final \r\n
            %% but internal \r survive since trim only strips from the end.
            Result = beamtalk_os:'run:'(<<"echo line1& echo line2">>),
            ?assertEqual(<<"line1\r\nline2">>, Result)
    end.

run_empty_output_test() ->
    Cmd =
        case os:type() of
            {unix, _} -> <<"true">>;
            {win32, _} -> <<"ver >nul">>
        end,
    ?assertEqual(<<>>, beamtalk_os:'run:'(Cmd)).

%%% ============================================================================
%%% run: — type error
%%% ============================================================================

run_type_error_integer_test() ->
    ?assertError(
        #{error := #beamtalk_error{kind = type_error, class = 'OS'}},
        beamtalk_os:'run:'(42)
    ).

run_type_error_atom_test() ->
    ?assertError(
        #{error := #beamtalk_error{kind = type_error, class = 'OS'}},
        beamtalk_os:'run:'(echo)
    ).

%%% ============================================================================
%%% run:timeout: — success path
%%% ============================================================================

run_timeout_returns_stdout_test() ->
    ?assertEqual(<<"hi">>, beamtalk_os:'run:timeout:'(<<"echo hi">>, 5000)).

run_timeout_ffi_shim_test() ->
    ?assertEqual(<<"hi">>, beamtalk_os:run(<<"echo hi">>, 5000)).

%%% ============================================================================
%%% run:timeout: — type errors
%%% ============================================================================

run_timeout_type_error_non_binary_cmd_test() ->
    ?assertError(
        #{error := #beamtalk_error{kind = type_error, class = 'OS'}},
        beamtalk_os:'run:timeout:'(42, 5000)
    ).

run_timeout_type_error_non_integer_timeout_test() ->
    ?assertError(
        #{error := #beamtalk_error{kind = type_error, class = 'OS'}},
        beamtalk_os:'run:timeout:'(<<"echo hi">>, <<"5000">>)
    ).

%%% ============================================================================
%%% run:timeout: — timeout expiry
%%% ============================================================================

run_timeout_raises_on_expiry_test() ->
    Cmd =
        case os:type() of
            {unix, _} -> <<"sleep 60">>;
            {win32, _} -> <<"ping -n 100 127.0.0.1">>
        end,
    ?assertError(
        #{error := #beamtalk_error{kind = timeout, class = 'OS'}},
        beamtalk_os:'run:timeout:'(Cmd, 100)
    ).

%%% ============================================================================
%%% output_too_large
%%% ============================================================================

run_output_too_large_test() ->
    case os:type() of
        {unix, _} ->
            %% Generate ~11 MB of output via dd (exceeds 10 MB limit).
            ?assertError(
                #{error := #beamtalk_error{kind = output_too_large, class = 'OS'}},
                beamtalk_os:'run:'(<<"dd if=/dev/zero bs=1048576 count=11 2>/dev/null | cat">>)
            );
        _ ->
            {skip, "Unix-only: no dd equivalent on Windows"}
    end.
