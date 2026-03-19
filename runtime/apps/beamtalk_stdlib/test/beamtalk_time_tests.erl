%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_time module (BT-1551).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Tests the Time class backing functions: nowS/0, nowMs/0, nowUs/0.

-module(beamtalk_time_tests).

-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% nowS/0
%%% ============================================================================

now_s_returns_integer_test() ->
    ?assert(is_integer(beamtalk_time:nowS())).

now_s_is_positive_test() ->
    ?assert(beamtalk_time:nowS() > 0).

now_s_is_reasonable_epoch_test() ->
    %% Should be well past 2020 (1577836800)
    ?assert(beamtalk_time:nowS() > 1577836800).

%%% ============================================================================
%%% nowMs/0
%%% ============================================================================

now_ms_returns_integer_test() ->
    ?assert(is_integer(beamtalk_time:nowMs())).

now_ms_is_positive_test() ->
    ?assert(beamtalk_time:nowMs() > 0).

%%% ============================================================================
%%% nowUs/0
%%% ============================================================================

now_us_returns_integer_test() ->
    ?assert(is_integer(beamtalk_time:nowUs())).

now_us_is_positive_test() ->
    ?assert(beamtalk_time:nowUs() > 0).

%%% ============================================================================
%%% Cross-precision consistency
%%% ============================================================================

now_s_less_than_now_ms_test() ->
    S = beamtalk_time:nowS(),
    Ms = beamtalk_time:nowMs(),
    ?assert(Ms > S).

now_ms_less_than_now_us_test() ->
    Ms = beamtalk_time:nowMs(),
    Us = beamtalk_time:nowUs(),
    ?assert(Us > Ms).
