%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_datetime module (BT-1088).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Tests constructors, accessors, conversion, arithmetic, comparison,
%%% has_method/1, and error paths.

-module(beamtalk_datetime_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% Helpers
%%% ============================================================================

make_dt(Y, Mo, D, H, Mi, S) ->
    beamtalk_datetime:'year:month:day:hour:minute:second:'(Y, Mo, D, H, Mi, S).

make_date(Y, Mo, D) ->
    beamtalk_datetime:'year:month:day:'(Y, Mo, D).

%%% ============================================================================
%%% Constructor — year:month:day:
%%% ============================================================================

year_month_day_basic_test() ->
    DT = make_date(2024, 3, 15),
    ?assertEqual('DateTime', maps:get('$beamtalk_class', DT)),
    ?assertEqual(2024, maps:get(year, DT)),
    ?assertEqual(3, maps:get(month, DT)),
    ?assertEqual(15, maps:get(day, DT)),
    ?assertEqual(0, maps:get(hour, DT)),
    ?assertEqual(0, maps:get(minute, DT)),
    ?assertEqual(0, maps:get(second, DT)).

year_month_day_invalid_date_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        make_date(2024, 2, 30)
    ).

year_month_day_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_datetime:'year:month:day:'(<<"2024">>, 1, 1)
    ).

%%% ============================================================================
%%% Constructor — year:month:day:hour:minute:second:
%%% ============================================================================

full_constructor_test() ->
    DT = make_dt(2024, 6, 15, 10, 30, 45),
    ?assertEqual(2024, maps:get(year, DT)),
    ?assertEqual(6, maps:get(month, DT)),
    ?assertEqual(15, maps:get(day, DT)),
    ?assertEqual(10, maps:get(hour, DT)),
    ?assertEqual(30, maps:get(minute, DT)),
    ?assertEqual(45, maps:get(second, DT)).

full_constructor_invalid_time_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        make_dt(2024, 1, 1, 25, 0, 0)
    ).

full_constructor_negative_minute_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        make_dt(2024, 1, 1, 0, -1, 0)
    ).

full_constructor_second_out_of_range_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        make_dt(2024, 1, 1, 0, 0, 60)
    ).

full_constructor_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_datetime:'year:month:day:hour:minute:second:'(<<"2024">>, 1, 1, 0, 0, 0)
    ).

%%% ============================================================================
%%% Constructor — fromTimestamp:
%%% ============================================================================

from_timestamp_epoch_test() ->
    %% Unix epoch = 1970-01-01T00:00:00
    DT = beamtalk_datetime:'fromTimestamp:'(0),
    ?assertEqual(1970, maps:get(year, DT)),
    ?assertEqual(1, maps:get(month, DT)),
    ?assertEqual(1, maps:get(day, DT)),
    ?assertEqual(0, maps:get(hour, DT)),
    ?assertEqual(0, maps:get(minute, DT)),
    ?assertEqual(0, maps:get(second, DT)).

from_timestamp_known_test() ->
    %% 2024-01-01T00:00:00Z = 1704067200
    DT = beamtalk_datetime:'fromTimestamp:'(1704067200),
    ?assertEqual(2024, maps:get(year, DT)),
    ?assertEqual(1, maps:get(month, DT)),
    ?assertEqual(1, maps:get(day, DT)).

from_timestamp_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_datetime:'fromTimestamp:'(<<"1704067200">>)
    ).

%%% ============================================================================
%%% Constructor — fromString:
%%% ============================================================================

from_string_basic_test() ->
    DT = beamtalk_datetime:'fromString:'(<<"2024-06-15T10:30:45Z">>),
    ?assertEqual(2024, maps:get(year, DT)),
    ?assertEqual(6, maps:get(month, DT)),
    ?assertEqual(15, maps:get(day, DT)),
    ?assertEqual(10, maps:get(hour, DT)),
    ?assertEqual(30, maps:get(minute, DT)),
    ?assertEqual(45, maps:get(second, DT)).

from_string_no_z_test() ->
    DT = beamtalk_datetime:'fromString:'(<<"2024-06-15T10:30:45">>),
    ?assertEqual(2024, maps:get(year, DT)).

from_string_bad_format_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_datetime:'fromString:'(<<"not-a-date">>)
    ).

from_string_invalid_date_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_datetime:'fromString:'(<<"2024-02-30T00:00:00Z">>)
    ).

from_string_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_datetime:'fromString:'(20240615)
    ).

%%% ============================================================================
%%% Accessors
%%% ============================================================================

accessors_test() ->
    DT = make_dt(2024, 6, 15, 10, 30, 45),
    ?assertEqual(2024, beamtalk_datetime:year(DT)),
    ?assertEqual(6, beamtalk_datetime:month(DT)),
    ?assertEqual(15, beamtalk_datetime:day(DT)),
    ?assertEqual(10, beamtalk_datetime:hour(DT)),
    ?assertEqual(30, beamtalk_datetime:minute(DT)),
    ?assertEqual(45, beamtalk_datetime:second(DT)).

%%% ============================================================================
%%% Conversion
%%% ============================================================================

as_timestamp_epoch_test() ->
    DT = make_dt(1970, 1, 1, 0, 0, 0),
    ?assertEqual(0, beamtalk_datetime:'asTimestamp'(DT)).

as_timestamp_roundtrip_test() ->
    Ts = 1704067200,
    DT = beamtalk_datetime:'fromTimestamp:'(Ts),
    ?assertEqual(Ts, beamtalk_datetime:'asTimestamp'(DT)).

as_string_test() ->
    DT = make_dt(2024, 6, 5, 9, 5, 3),
    ?assertEqual(<<"2024-06-05T09:05:03Z">>, beamtalk_datetime:'asString'(DT)).

as_string_padding_test() ->
    DT = make_dt(2024, 12, 31, 23, 59, 59),
    ?assertEqual(<<"2024-12-31T23:59:59Z">>, beamtalk_datetime:'asString'(DT)).

print_string_test() ->
    DT = make_dt(2024, 1, 1, 0, 0, 0),
    Result = beamtalk_datetime:'printString'(DT),
    ?assertMatch(<<"a DateTime(", _/binary>>, Result).

%%% ============================================================================
%%% Arithmetic
%%% ============================================================================

add_seconds_positive_test() ->
    DT = make_dt(2024, 1, 1, 0, 0, 0),
    DT2 = beamtalk_datetime:'addSeconds:'(DT, 3661),
    ?assertEqual(1, beamtalk_datetime:hour(DT2)),
    ?assertEqual(1, beamtalk_datetime:minute(DT2)),
    ?assertEqual(1, beamtalk_datetime:second(DT2)).

add_seconds_negative_test() ->
    DT = make_dt(2024, 1, 2, 0, 0, 0),
    DT2 = beamtalk_datetime:'addSeconds:'(DT, -86400),
    ?assertEqual(2024, beamtalk_datetime:year(DT2)),
    ?assertEqual(1, beamtalk_datetime:month(DT2)),
    ?assertEqual(1, beamtalk_datetime:day(DT2)).

add_seconds_type_error_test() ->
    DT = make_dt(2024, 1, 1, 0, 0, 0),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_datetime:'addSeconds:'(DT, <<"3600">>)
    ).

add_days_test() ->
    DT = make_dt(2024, 1, 1, 12, 0, 0),
    DT2 = beamtalk_datetime:'addDays:'(DT, 30),
    ?assertEqual(2024, beamtalk_datetime:year(DT2)),
    ?assertEqual(1, beamtalk_datetime:month(DT2)),
    ?assertEqual(31, beamtalk_datetime:day(DT2)).

add_days_type_error_test() ->
    DT = make_dt(2024, 1, 1, 0, 0, 0),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_datetime:'addDays:'(DT, 1.5)
    ).

diff_seconds_test() ->
    DT1 = make_dt(2024, 1, 1, 1, 0, 0),
    DT2 = make_dt(2024, 1, 1, 0, 0, 0),
    ?assertEqual(3600, beamtalk_datetime:'diffSeconds:'(DT1, DT2)).

diff_seconds_zero_test() ->
    DT = make_dt(2024, 6, 15, 10, 30, 45),
    ?assertEqual(0, beamtalk_datetime:'diffSeconds:'(DT, DT)).

diff_seconds_type_error_test() ->
    DT = make_dt(2024, 1, 1, 0, 0, 0),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_datetime:'diffSeconds:'(DT, <<"other">>)
    ).

%%% ============================================================================
%%% Comparison
%%% ============================================================================

less_than_true_test() ->
    DT1 = make_dt(2024, 1, 1, 0, 0, 0),
    DT2 = make_dt(2024, 1, 2, 0, 0, 0),
    ?assert(beamtalk_datetime:'<'(DT1, DT2)).

less_than_false_test() ->
    DT1 = make_dt(2024, 1, 2, 0, 0, 0),
    DT2 = make_dt(2024, 1, 1, 0, 0, 0),
    ?assertNot(beamtalk_datetime:'<'(DT1, DT2)).

greater_than_test() ->
    DT1 = make_dt(2024, 6, 15, 0, 0, 0),
    DT2 = make_dt(2024, 1, 1, 0, 0, 0),
    ?assert(beamtalk_datetime:'>'(DT1, DT2)).

less_equal_true_test() ->
    DT1 = make_dt(2024, 1, 1, 0, 0, 0),
    DT2 = make_dt(2024, 1, 1, 0, 0, 0),
    ?assert(beamtalk_datetime:'=<'(DT1, DT2)).

greater_equal_true_test() ->
    DT1 = make_dt(2024, 1, 2, 0, 0, 0),
    DT2 = make_dt(2024, 1, 1, 0, 0, 0),
    ?assert(beamtalk_datetime:'>='(DT1, DT2)).

equal_test() ->
    DT1 = make_dt(2024, 6, 15, 10, 30, 45),
    DT2 = make_dt(2024, 6, 15, 10, 30, 45),
    ?assert(beamtalk_datetime:'=:='(DT1, DT2)).

not_equal_test() ->
    DT1 = make_dt(2024, 1, 1, 0, 0, 0),
    DT2 = make_dt(2024, 1, 2, 0, 0, 0),
    ?assert(beamtalk_datetime:'/='(DT1, DT2)).

comparison_type_error_lt_test() ->
    DT = make_dt(2024, 1, 1, 0, 0, 0),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_datetime:'<'(DT, <<"other">>)
    ).

comparison_type_error_gt_test() ->
    DT = make_dt(2024, 1, 1, 0, 0, 0),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_datetime:'>'(DT, <<"other">>)
    ).

comparison_type_error_le_test() ->
    DT = make_dt(2024, 1, 1, 0, 0, 0),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_datetime:'=<'(DT, <<"other">>)
    ).

comparison_type_error_ge_test() ->
    DT = make_dt(2024, 1, 1, 0, 0, 0),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_datetime:'>='(DT, <<"other">>)
    ).

comparison_type_error_eq_test() ->
    DT = make_dt(2024, 1, 1, 0, 0, 0),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_datetime:'=:='(DT, <<"other">>)
    ).

comparison_type_error_ne_test() ->
    DT = make_dt(2024, 1, 1, 0, 0, 0),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_datetime:'/='(DT, <<"other">>)
    ).

%%% ============================================================================
%%% has_method/1
%%% ============================================================================

has_method_known_test() ->
    ?assert(beamtalk_datetime:has_method(year)),
    ?assert(beamtalk_datetime:has_method(month)),
    ?assert(beamtalk_datetime:has_method(day)),
    ?assert(beamtalk_datetime:has_method(hour)),
    ?assert(beamtalk_datetime:has_method(minute)),
    ?assert(beamtalk_datetime:has_method(second)),
    ?assert(beamtalk_datetime:has_method('asTimestamp')),
    ?assert(beamtalk_datetime:has_method('asString')),
    ?assert(beamtalk_datetime:has_method('printString')),
    ?assert(beamtalk_datetime:has_method('addSeconds:')),
    ?assert(beamtalk_datetime:has_method('addDays:')),
    ?assert(beamtalk_datetime:has_method('diffSeconds:')),
    ?assert(beamtalk_datetime:has_method('<')),
    ?assert(beamtalk_datetime:has_method('>')),
    ?assert(beamtalk_datetime:has_method('=<')),
    ?assert(beamtalk_datetime:has_method('>=')),
    ?assert(beamtalk_datetime:has_method('=:=')),
    ?assert(beamtalk_datetime:has_method('/=')).

has_method_unknown_test() ->
    ?assertNot(beamtalk_datetime:has_method(frobnicateWidget)),
    ?assertNot(beamtalk_datetime:has_method(new)).

%%% ============================================================================
%%% monotonicNow
%%% ============================================================================

monotonic_now_test() ->
    T1 = beamtalk_datetime:'monotonicNow'(),
    T2 = beamtalk_datetime:'monotonicNow'(),
    ?assert(is_integer(T1)),
    ?assert(T2 >= T1).
