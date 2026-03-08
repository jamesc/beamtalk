%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_os module (BT-1247).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Tests cover:
%%% - run: success path (trimmed stdout)
%%% - run: type error (non-binary argument)
%%% - run:timeout: success path with explicit timeout
%%% - run:timeout: type error (non-binary command)
%%% - run:timeout: type error (non-integer timeout)
%%% - run:timeout: timeout expiry raises #timeout error
%%% - output exceeding MAX_OUTPUT_BYTES raises output_too_large error

-module(beamtalk_os_tests).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% run: — success path
%%% ============================================================================

run_returns_trimmed_stdout_test() ->
    case os:type() of
        {unix, _} ->
            ?assertEqual(<<"hello">>, beamtalk_os:'run:'(<<"echo hello">>));
        _ ->
            ok
    end.

run_trims_trailing_newline_test() ->
    case os:type() of
        {unix, _} ->
            Result = beamtalk_os:'run:'(<<"printf 'line1\nline2\n'">>),
            ?assertEqual(<<"line1\nline2">>, Result);
        _ ->
            ok
    end.

run_empty_output_test() ->
    case os:type() of
        {unix, _} ->
            ?assertEqual(<<>>, beamtalk_os:'run:'(<<"true">>));
        _ ->
            ok
    end.

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
    case os:type() of
        {unix, _} ->
            ?assertEqual(<<"hi">>, beamtalk_os:'run:timeout:'(<<"echo hi">>, 5000));
        _ ->
            ok
    end.

run_timeout_ffi_shim_test() ->
    case os:type() of
        {unix, _} ->
            ?assertEqual(<<"hi">>, beamtalk_os:run(<<"echo hi">>, 5000));
        _ ->
            ok
    end.

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
    case os:type() of
        {unix, _} ->
            ?assertError(
                #{error := #beamtalk_error{kind = timeout, class = 'OS'}},
                beamtalk_os:'run:timeout:'(<<"sleep 60">>, 100)
            );
        _ ->
            ok
    end.

%%% ============================================================================
%%% output_too_large
%%% ============================================================================

run_output_too_large_test() ->
    case os:type() of
        {unix, _} ->
            %% Generate ~11 MB of output (exceeds 10 MB limit).
            %% yes outputs an infinite stream; we only need it to exceed the cap.
            ?assertError(
                #{error := #beamtalk_error{kind = output_too_large, class = 'OS'}},
                beamtalk_os:'run:'(<<"dd if=/dev/zero bs=1048576 count=11 2>/dev/null | cat">>)
            );
        _ ->
            ok
    end.
