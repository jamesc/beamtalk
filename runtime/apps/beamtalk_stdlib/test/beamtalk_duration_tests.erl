%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_duration_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_duration module (BT-2969).

Tests constructors, conversions, arithmetic, comparison, formatting,
ISO 8601 round-trips, FFI shims, cross-module helpers, and error paths.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% Helpers
%%% ============================================================================

ms(N) -> beamtalk_duration:'milliseconds:'(N).

%%% ============================================================================
%%% Constructors
%%% ============================================================================

milliseconds_test() ->
    D = ms(1500),
    ?assertEqual('Duration', maps:get('$beamtalk_class', D)),
    ?assertEqual(1500, maps:get(millis, D)).

seconds_test() ->
    ?assertEqual(90000, maps:get(millis, beamtalk_duration:'seconds:'(90))).

minutes_test() ->
    ?assertEqual(120000, maps:get(millis, beamtalk_duration:'minutes:'(2))).

hours_test() ->
    ?assertEqual(7200000, maps:get(millis, beamtalk_duration:'hours:'(2))).

days_test() ->
    ?assertEqual(86400000, maps:get(millis, beamtalk_duration:'days:'(1))).

float_constructor_rounds_test() ->
    ?assertEqual(1500, maps:get(millis, beamtalk_duration:'seconds:'(1.5))),
    ?assertEqual(90000, maps:get(millis, beamtalk_duration:'minutes:'(1.5))),
    ?assertEqual(2, maps:get(millis, beamtalk_duration:'milliseconds:'(1.5))).

negative_constructor_test() ->
    ?assertEqual(-1000, maps:get(millis, beamtalk_duration:'seconds:'(-1))).

constructor_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_duration:'seconds:'(<<"5">>)
    ).

%%% ============================================================================
%%% Conversions
%%% ============================================================================

as_milliseconds_test() ->
    ?assertEqual(1500, beamtalk_duration:'asMilliseconds'(ms(1500))).

as_seconds_truncates_test() ->
    ?assertEqual(1, beamtalk_duration:'asSeconds'(ms(1999))).

as_seconds_negative_truncates_toward_zero_test() ->
    ?assertEqual(-1, beamtalk_duration:'asSeconds'(ms(-1999))).

as_minutes_test() ->
    ?assertEqual(1, beamtalk_duration:'asMinutes'(beamtalk_duration:'seconds:'(119))).

as_hours_test() ->
    ?assertEqual(1, beamtalk_duration:'asHours'(beamtalk_duration:'minutes:'(90))).

total_days_test() ->
    ?assertEqual(1, beamtalk_duration:'totalDays'(beamtalk_duration:'hours:'(36))).

%%% ============================================================================
%%% Predicates
%%% ============================================================================

is_zero_test() ->
    ?assertEqual(true, beamtalk_duration:'isZero'(ms(0))),
    ?assertEqual(false, beamtalk_duration:'isZero'(ms(1))).

is_negative_test() ->
    ?assertEqual(true, beamtalk_duration:'isNegative'(ms(-1))),
    ?assertEqual(false, beamtalk_duration:'isNegative'(ms(0))),
    ?assertEqual(false, beamtalk_duration:'isNegative'(ms(1))).

%%% ============================================================================
%%% Arithmetic
%%% ============================================================================

add_test() ->
    Sum = beamtalk_duration:'+'(beamtalk_duration:'hours:'(1), beamtalk_duration:'minutes:'(30)),
    ?assertEqual(90, beamtalk_duration:'asMinutes'(Sum)).

subtract_test() ->
    Diff = beamtalk_duration:'-'(beamtalk_duration:'hours:'(1), beamtalk_duration:'minutes:'(30)),
    ?assertEqual(30, beamtalk_duration:'asMinutes'(Diff)).

subtract_negative_result_test() ->
    Diff = beamtalk_duration:'-'(ms(1000), ms(3000)),
    ?assertEqual(-2000, beamtalk_duration:'asMilliseconds'(Diff)),
    ?assertEqual(true, beamtalk_duration:'isNegative'(Diff)).

multiply_integer_test() ->
    Scaled = beamtalk_duration:'*'(beamtalk_duration:'minutes:'(10), 3),
    ?assertEqual(30, beamtalk_duration:'asMinutes'(Scaled)).

multiply_float_rounds_test() ->
    Scaled = beamtalk_duration:'*'(beamtalk_duration:'seconds:'(10), 1.5),
    ?assertEqual(15000, beamtalk_duration:'asMilliseconds'(Scaled)).

add_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_duration:'+'(ms(1), 5)
    ).

multiply_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_duration:'*'(ms(1), ms(2))
    ).

%%% ============================================================================
%%% Comparison
%%% ============================================================================

comparison_test() ->
    A = beamtalk_duration:'seconds:'(1),
    B = beamtalk_duration:'seconds:'(2),
    ?assertEqual(true, beamtalk_duration:'<'(A, B)),
    ?assertEqual(false, beamtalk_duration:'<'(B, A)),
    ?assertEqual(true, beamtalk_duration:'>'(B, A)),
    ?assertEqual(true, beamtalk_duration:'=<'(A, A)),
    ?assertEqual(true, beamtalk_duration:'>='(A, A)),
    ?assertEqual(false, beamtalk_duration:'>='(A, B)),
    ?assertEqual(true, beamtalk_duration:'=:='(A, beamtalk_duration:'milliseconds:'(1000))),
    ?assertEqual(true, beamtalk_duration:'/='(A, B)),
    ?assertEqual(false, beamtalk_duration:'/='(A, A)).

comparison_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_duration:'<'(ms(1), 5)
    ).

%%% ============================================================================
%%% Formatting — printString
%%% ============================================================================

print_string_zero_test() ->
    ?assertEqual(<<"0ms">>, beamtalk_duration:'printString'(ms(0))).

print_string_ms_test() ->
    ?assertEqual(<<"1s 500ms">>, beamtalk_duration:'printString'(ms(1500))).

print_string_hm_test() ->
    ?assertEqual(<<"1h 30m">>, beamtalk_duration:'printString'(beamtalk_duration:'minutes:'(90))).

print_string_hms_test() ->
    ?assertEqual(
        <<"1h 30m 15s">>, beamtalk_duration:'printString'(beamtalk_duration:'seconds:'(5415))
    ).

print_string_full_test() ->
    ?assertEqual(<<"1d 1h 1m 1s 250ms">>, beamtalk_duration:'printString'(ms(90061250))).

print_string_negative_test() ->
    ?assertEqual(
        <<"-1h 30m">>, beamtalk_duration:'printString'(beamtalk_duration:'minutes:'(-90))
    ).

%%% ============================================================================
%%% Formatting — asString (ISO 8601)
%%% ============================================================================

as_string_zero_test() ->
    ?assertEqual(<<"PT0S">>, beamtalk_duration:'asString'(ms(0))).

as_string_hm_test() ->
    ?assertEqual(<<"PT1H30M">>, beamtalk_duration:'asString'(beamtalk_duration:'minutes:'(90))).

as_string_days_test() ->
    ?assertEqual(<<"P1DT2H">>, beamtalk_duration:'asString'(beamtalk_duration:'hours:'(26))).

as_string_days_only_test() ->
    ?assertEqual(<<"P2D">>, beamtalk_duration:'asString'(beamtalk_duration:'days:'(2))).

as_string_fractional_test() ->
    ?assertEqual(<<"PT0.250S">>, beamtalk_duration:'asString'(ms(250))).

as_string_full_test() ->
    ?assertEqual(<<"P1DT1H1M1.250S">>, beamtalk_duration:'asString'(ms(90061250))).

as_string_negative_test() ->
    ?assertEqual(<<"-PT1H30M">>, beamtalk_duration:'asString'(beamtalk_duration:'minutes:'(-90))).

%%% ============================================================================
%%% fromString: parsing and round-trip
%%% ============================================================================

from_string_basic_test() ->
    ?assertEqual(
        5400000, beamtalk_duration:'asMilliseconds'(beamtalk_duration:'fromString:'(<<"PT1H30M">>))
    ).

from_string_fractional_test() ->
    ?assertEqual(
        250, beamtalk_duration:'asMilliseconds'(beamtalk_duration:'fromString:'(<<"PT0.250S">>))
    ),
    %% Short fractions are padded: "0.2" == 200 ms, "0.25" == 250 ms
    ?assertEqual(
        200, beamtalk_duration:'asMilliseconds'(beamtalk_duration:'fromString:'(<<"PT0.2S">>))
    ),
    ?assertEqual(
        250, beamtalk_duration:'asMilliseconds'(beamtalk_duration:'fromString:'(<<"PT0.25S">>))
    ).

from_string_negative_test() ->
    ?assertEqual(
        -5400000,
        beamtalk_duration:'asMilliseconds'(beamtalk_duration:'fromString:'(<<"-PT1H30M">>))
    ).

round_trip_test_() ->
    Cases = [0, 1, 250, 1500, 90000, 5415000, 90061250, -90, -5400000, 86400000, 172800000],
    [
        ?_assertEqual(
            Ms,
            beamtalk_duration:'asMilliseconds'(
                beamtalk_duration:'fromString:'(beamtalk_duration:'asString'(ms(Ms)))
            )
        )
     || Ms <- Cases
    ].

from_string_invalid_test_() ->
    Invalid = [<<"">>, <<"P">>, <<"PT">>, <<"-P">>, <<"1h">>, <<"P1W">>, <<"PT1H30">>],
    [
        ?_assertError(
            #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
            beamtalk_duration:'fromString:'(S)
        )
     || S <- Invalid
    ].

from_string_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_duration:'fromString:'(42)
    ).

%%% ============================================================================
%%% FFI shims
%%% ============================================================================

shims_test() ->
    ?assertEqual(ms(5), beamtalk_duration:milliseconds(5)),
    ?assertEqual(ms(5000), beamtalk_duration:seconds(5)),
    ?assertEqual(ms(300000), beamtalk_duration:minutes(5)),
    ?assertEqual(ms(18000000), beamtalk_duration:hours(5)),
    ?assertEqual(ms(432000000), beamtalk_duration:days(5)),
    ?assertEqual(ms(1000), beamtalk_duration:fromString(<<"PT1S">>)),
    ?assertEqual(ms(3), beamtalk_duration:add(ms(1), ms(2))),
    ?assertEqual(ms(-1), beamtalk_duration:subtract(ms(1), ms(2))),
    ?assertEqual(ms(6), beamtalk_duration:multiply(ms(3), 2)),
    ?assertEqual(true, beamtalk_duration:lt(ms(1), ms(2))),
    ?assertEqual(true, beamtalk_duration:gt(ms(2), ms(1))),
    ?assertEqual(true, beamtalk_duration:lte(ms(1), ms(1))),
    ?assertEqual(true, beamtalk_duration:gte(ms(1), ms(1))),
    ?assertEqual(true, beamtalk_duration:eql(ms(1), ms(1))),
    ?assertEqual(true, beamtalk_duration:neq(ms(1), ms(2))),
    ?assertEqual(true, beamtalk_duration:sneq(ms(1), ms(2))),
    ?assertEqual(false, beamtalk_duration:sneq(ms(1), ms(1))).

%%% ============================================================================
%%% Cross-module helpers
%%% ============================================================================

to_millis_test() ->
    ?assertEqual({ok, 500}, beamtalk_duration:to_millis(500)),
    ?assertEqual({ok, 1500}, beamtalk_duration:to_millis(ms(1500))),
    ?assertEqual(error, beamtalk_duration:to_millis(<<"1500">>)),
    ?assertEqual(error, beamtalk_duration:to_millis(#{})).

is_duration_test() ->
    ?assertEqual(true, beamtalk_duration:is_duration(ms(1))),
    ?assertEqual(false, beamtalk_duration:is_duration(1)),
    ?assertEqual(false, beamtalk_duration:is_duration(#{'$beamtalk_class' => 'DateTime'})).

%%% ============================================================================
%%% Integration — Timer accepts Duration
%%% ============================================================================

timer_sleep_duration_test() ->
    ?assertEqual(nil, beamtalk_timer:'sleep:'(ms(1))).

timer_after_duration_test() ->
    T = beamtalk_timer:'after:do:'(beamtalk_duration:'seconds:'(10), fun() -> ok end),
    ?assertEqual(true, beamtalk_timer:'isActive'(T)),
    ?assertEqual(true, beamtalk_timer:cancel(T)).

timer_every_duration_test() ->
    T = beamtalk_timer:'every:do:'(beamtalk_duration:'seconds:'(10), fun() -> ok end),
    ?assertEqual(true, beamtalk_timer:cancel(T)).

%%% ============================================================================
%%% Integration — DateTime addDuration: / subtraction
%%% ============================================================================

datetime_add_duration_test() ->
    DT = beamtalk_datetime:'year:month:day:'(2026, 1, 1),
    DT2 = beamtalk_datetime:'addDuration:'(DT, beamtalk_duration:'hours:'(1)),
    ?assertEqual(1, maps:get(hour, DT2)).

datetime_add_duration_truncates_subsecond_test() ->
    DT = beamtalk_datetime:'year:month:day:'(2026, 1, 1),
    DT2 = beamtalk_datetime:'addDuration:'(DT, ms(1999)),
    ?assertEqual(1, maps:get(second, DT2)).

datetime_add_duration_type_error_test() ->
    DT = beamtalk_datetime:'year:month:day:'(2026, 1, 1),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_datetime:'addDuration:'(DT, 3600)
    ).

datetime_subtract_test() ->
    A = beamtalk_datetime:'year:month:day:'(2026, 1, 2),
    B = beamtalk_datetime:'year:month:day:'(2026, 1, 1),
    D = beamtalk_datetime:'-'(A, B),
    ?assertEqual(24, beamtalk_duration:'asHours'(D)),
    Neg = beamtalk_datetime:'-'(B, A),
    ?assertEqual(true, beamtalk_duration:'isNegative'(Neg)).

datetime_subtract_type_error_test() ->
    A = beamtalk_datetime:'year:month:day:'(2026, 1, 2),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_datetime:'-'(A, 42)
    ).

%%% ============================================================================
%%% Integration — Future await: accepts Duration
%%% ============================================================================

future_await_duration_test() ->
    F = beamtalk_future:new(),
    {beamtalk_future, Pid} = F,
    beamtalk_future:resolve(F, 42),
    %% Route through beamtalk_primitive dispatch as `future await: duration`
    ?assertEqual(
        42, beamtalk_primitive:send(Pid, 'await:', [beamtalk_duration:'seconds:'(5)])
    ).
