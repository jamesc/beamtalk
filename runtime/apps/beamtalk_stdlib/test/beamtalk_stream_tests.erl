%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_stream_tests).

-moduledoc "Unit tests for beamtalk_stream (BT-537, BT-1966).".

-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% Finalizer Support Tests
%%% ============================================================================

%% Test that make_stream/3 creates a stream with finalizer
make_stream_with_finalizer_test() ->
    Gen = fun() -> done end,
    Finalizer = fun() -> ok end,
    Stream = beamtalk_stream:make_stream(Gen, <<"test">>, Finalizer),
    ?assertEqual('Stream', maps:get('$beamtalk_class', Stream)),
    ?assert(maps:is_key(finalizer, Stream)).

%% Test that make_stream/2 creates a stream without finalizer
make_stream_without_finalizer_test() ->
    Gen = fun() -> done end,
    Stream = beamtalk_stream:make_stream(Gen, <<"test">>),
    ?assertNot(maps:is_key(finalizer, Stream)).

%% Test that take: calls the finalizer on early stop
take_calls_finalizer_test() ->
    Self = self(),
    Gen = make_counter_gen(1),
    Finalizer = fun() -> Self ! finalizer_called end,
    Stream = beamtalk_stream:make_stream(Gen, <<"test">>, Finalizer),
    Result = beamtalk_stream:take(Stream, 2),
    ?assertEqual([1, 2], Result),
    receive
        finalizer_called -> ok
    after 100 -> ?assert(false)
    end.

%% Test that take: calls finalizer even when stream exhausts naturally
take_calls_finalizer_on_exhaust_test() ->
    Self = self(),
    Gen = make_list_gen([1, 2]),
    Finalizer = fun() -> Self ! finalizer_called end,
    Stream = beamtalk_stream:make_stream(Gen, <<"test">>, Finalizer),
    Result = beamtalk_stream:take(Stream, 5),
    ?assertEqual([1, 2], Result),
    receive
        finalizer_called -> ok
    after 100 -> ?assert(false)
    end.

%% Test that detect: calls the finalizer
detect_calls_finalizer_test() ->
    Self = self(),
    Gen = make_counter_gen(1),
    Finalizer = fun() -> Self ! finalizer_called end,
    Stream = beamtalk_stream:make_stream(Gen, <<"test">>, Finalizer),
    Result = beamtalk_stream:detect(Stream, fun(X) -> X =:= 3 end),
    ?assertEqual(3, Result),
    receive
        finalizer_called -> ok
    after 100 -> ?assert(false)
    end.

%% Test that anySatisfy: calls the finalizer
any_satisfy_calls_finalizer_test() ->
    Self = self(),
    Gen = make_counter_gen(1),
    Finalizer = fun() -> Self ! finalizer_called end,
    Stream = beamtalk_stream:make_stream(Gen, <<"test">>, Finalizer),
    Result = beamtalk_stream:any_satisfy(Stream, fun(X) -> X =:= 2 end),
    ?assertEqual(true, Result),
    receive
        finalizer_called -> ok
    after 100 -> ?assert(false)
    end.

%% Test that allSatisfy: calls the finalizer
all_satisfy_calls_finalizer_test() ->
    Self = self(),
    Gen = make_counter_gen(1),
    Finalizer = fun() -> Self ! finalizer_called end,
    Stream = beamtalk_stream:make_stream(Gen, <<"test">>, Finalizer),
    Result = beamtalk_stream:all_satisfy(Stream, fun(X) -> X < 3 end),
    ?assertEqual(false, Result),
    receive
        finalizer_called -> ok
    after 100 -> ?assert(false)
    end.

%% Test that lazy ops propagate the finalizer
select_propagates_finalizer_test() ->
    Self = self(),
    Gen = make_counter_gen(1),
    Finalizer = fun() -> Self ! finalizer_called end,
    Stream = beamtalk_stream:make_stream(Gen, <<"test">>, Finalizer),
    Filtered = beamtalk_stream:'select'(Stream, fun(X) -> X rem 2 =:= 0 end),
    Result = beamtalk_stream:take(Filtered, 2),
    ?assertEqual([2, 4], Result),
    receive
        finalizer_called -> ok
    after 100 -> ?assert(false)
    end.

collect_propagates_finalizer_test() ->
    Self = self(),
    Gen = make_counter_gen(1),
    Finalizer = fun() -> Self ! finalizer_called end,
    Stream = beamtalk_stream:make_stream(Gen, <<"test">>, Finalizer),
    Mapped = beamtalk_stream:'collect'(Stream, fun(X) -> X * 10 end),
    Result = beamtalk_stream:take(Mapped, 2),
    ?assertEqual([10, 20], Result),
    receive
        finalizer_called -> ok
    after 100 -> ?assert(false)
    end.

reject_propagates_finalizer_test() ->
    Self = self(),
    Gen = make_counter_gen(1),
    Finalizer = fun() -> Self ! finalizer_called end,
    Stream = beamtalk_stream:make_stream(Gen, <<"test">>, Finalizer),
    Rejected = beamtalk_stream:'reject'(Stream, fun(X) -> X =:= 2 end),
    Result = beamtalk_stream:take(Rejected, 2),
    ?assertEqual([1, 3], Result),
    receive
        finalizer_called -> ok
    after 100 -> ?assert(false)
    end.

drop_propagates_finalizer_test() ->
    Self = self(),
    Gen = make_counter_gen(1),
    Finalizer = fun() -> Self ! finalizer_called end,
    Stream = beamtalk_stream:make_stream(Gen, <<"test">>, Finalizer),
    Dropped = beamtalk_stream:'drop'(Stream, 2),
    Result = beamtalk_stream:take(Dropped, 2),
    ?assertEqual([3, 4], Result),
    receive
        finalizer_called -> ok
    after 100 -> ?assert(false)
    end.

%% Test that streams without finalizer work fine (no crash)
take_without_finalizer_test() ->
    Gen = make_counter_gen(1),
    Stream = beamtalk_stream:make_stream(Gen, <<"test">>),
    Result = beamtalk_stream:take(Stream, 3),
    ?assertEqual([1, 2, 3], Result).

%% Test that do: calls the finalizer
do_calls_finalizer_test() ->
    Self = self(),
    Gen = make_list_gen([1, 2, 3]),
    Finalizer = fun() -> Self ! finalizer_called end,
    Stream = beamtalk_stream:make_stream(Gen, <<"test">>, Finalizer),
    beamtalk_stream:do(Stream, fun(_) -> ok end),
    receive
        finalizer_called -> ok
    after 100 -> ?assert(false)
    end.

%% Test that inject:into: calls the finalizer
inject_into_calls_finalizer_test() ->
    Self = self(),
    Gen = make_list_gen([1, 2, 3]),
    Finalizer = fun() -> Self ! finalizer_called end,
    Stream = beamtalk_stream:make_stream(Gen, <<"test">>, Finalizer),
    Result = beamtalk_stream:inject_into(Stream, 0, fun(Acc, X) -> Acc + X end),
    ?assertEqual(6, Result),
    receive
        finalizer_called -> ok
    after 100 -> ?assert(false)
    end.

%% Test that asList calls the finalizer
as_list_calls_finalizer_test() ->
    Self = self(),
    Gen = make_list_gen([1, 2, 3]),
    Finalizer = fun() -> Self ! finalizer_called end,
    Stream = beamtalk_stream:make_stream(Gen, <<"test">>, Finalizer),
    Result = beamtalk_stream:as_list(Stream),
    ?assertEqual([1, 2, 3], Result),
    receive
        finalizer_called -> ok
    after 100 -> ?assert(false)
    end.

%% Test that finalizer is safe to call multiple times (double-close)
double_finalizer_safe_test() ->
    Self = self(),
    Counter = atomics:new(1, [{signed, false}]),
    Finalizer = fun() ->
        atomics:add(Counter, 1, 1),
        Self ! finalizer_called
    end,
    Stream = beamtalk_stream:make_stream(fun() -> done end, <<"test">>, Finalizer),
    beamtalk_stream:take(Stream, 0),
    receive
        finalizer_called -> ok
    after 100 -> ?assert(false)
    end,
    ?assertEqual(1, atomics:get(Counter, 1)).

%% Test that finalizer is called even when block raises an exception
finalizer_called_on_exception_test() ->
    Self = self(),
    Gen = make_counter_gen(1),
    Finalizer = fun() -> Self ! finalizer_called end,
    Stream = beamtalk_stream:make_stream(Gen, <<"test">>, Finalizer),
    ?assertError(
        boom,
        beamtalk_stream:do(Stream, fun(X) ->
            case X of
                2 -> error(boom);
                _ -> ok
            end
        end)
    ),
    receive
        finalizer_called -> ok
    after 100 -> ?assert(false)
    end.

%%% ============================================================================
%%% File Handle Finalizer Test
%%% ============================================================================

%% Test that File lines: with take: closes the file handle
file_lines_take_closes_handle_test() ->
    %% Use a relative path (required by beamtalk_file security validation)
    TmpFile = "test_stream_finalizer_tmp.txt",
    ok = file:write_file(TmpFile, <<"line1\nline2\nline3\nline4\nline5\n">>),
    try
        LinesResult = beamtalk_file:'lines:'(list_to_binary(TmpFile)),
        #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := Stream} = LinesResult,
        Result = beamtalk_stream:take(Stream, 1),
        ?assertEqual([<<"line1">>], Result),
        timer:sleep(10),
        ok
    after
        file:delete(TmpFile)
    end.

%%% ============================================================================
%%% Constructor Tests (BT-1966)
%%% ============================================================================

%% from: creates an infinite stream starting at the given integer
from_integer_test() ->
    Stream = beamtalk_stream:from(1),
    ?assertEqual([1, 2, 3, 4, 5], beamtalk_stream:take(Stream, 5)).

%% from: with negative start
from_negative_test() ->
    Stream = beamtalk_stream:from(-2),
    ?assertEqual([-2, -1, 0, 1], beamtalk_stream:take(Stream, 4)).

%% from: with non-integer raises type error
from_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := 'TypeError'},
        beamtalk_stream:from(1.5)
    ).

%% from:by: creates a stream applying step function
from_by_step_test() ->
    Stream = beamtalk_stream:from_by(1, fun(N) -> N * 2 end),
    ?assertEqual([1, 2, 4, 8, 16], beamtalk_stream:take(Stream, 5)).

%% from:by: with non-function raises type error
from_by_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := 'TypeError'},
        beamtalk_stream:from_by(1, not_a_function)
    ).

%% on: creates a stream from a list
on_list_test() ->
    Stream = beamtalk_stream:on([10, 20, 30]),
    ?assertEqual([10, 20, 30], beamtalk_stream:as_list(Stream)).

%% on: creates a stream from a string (grapheme clusters)
on_string_test() ->
    Stream = beamtalk_stream:on(<<"hello">>),
    ?assertEqual([<<"h">>, <<"e">>, <<"l">>, <<"l">>, <<"o">>], beamtalk_stream:as_list(Stream)).

%% on: creates a stream from a Set
on_set_test() ->
    Set = #{'$beamtalk_class' => 'Set', elements => [1, 2, 3]},
    Stream = beamtalk_stream:on(Set),
    ?assertEqual([1, 2, 3], beamtalk_stream:as_list(Stream)).

%% on: rejects non-streamable types
on_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := 'TypeError'},
        beamtalk_stream:on(42)
    ).

%% on: rejects Dictionary maps
on_dictionary_error_test() ->
    Dict = #{a => 1},
    ?assertError(
        #{'$beamtalk_class' := 'TypeError'},
        beamtalk_stream:on(Dict)
    ).

%%% ============================================================================
%%% Empty Stream Edge Cases (BT-1966)
%%% ============================================================================

%% take on empty stream returns empty list
take_empty_stream_test() ->
    Stream = beamtalk_stream:on([]),
    ?assertEqual([], beamtalk_stream:take(Stream, 5)).

%% take 0 elements returns empty list
take_zero_test() ->
    Stream = beamtalk_stream:from(1),
    ?assertEqual([], beamtalk_stream:take(Stream, 0)).

%% as_list on empty stream returns empty list
as_list_empty_test() ->
    Stream = beamtalk_stream:on([]),
    ?assertEqual([], beamtalk_stream:as_list(Stream)).

%% inject:into: on empty stream returns initial value
inject_into_empty_test() ->
    Stream = beamtalk_stream:on([]),
    ?assertEqual(42, beamtalk_stream:inject_into(Stream, 42, fun(Acc, _X) -> Acc end)).

%% detect on empty stream returns nil
detect_empty_test() ->
    Stream = beamtalk_stream:on([]),
    ?assertEqual(nil, beamtalk_stream:detect(Stream, fun(_) -> true end)).

%% any_satisfy on empty stream returns false
any_satisfy_empty_test() ->
    Stream = beamtalk_stream:on([]),
    ?assertEqual(false, beamtalk_stream:any_satisfy(Stream, fun(_) -> true end)).

%% all_satisfy on empty stream returns true (vacuous truth)
all_satisfy_empty_test() ->
    Stream = beamtalk_stream:on([]),
    ?assertEqual(true, beamtalk_stream:all_satisfy(Stream, fun(_) -> false end)).

%%% ============================================================================
%%% Lazy Operation Chaining (BT-1966)
%%% ============================================================================

%% select filters elements from finite stream
select_finite_test() ->
    Stream = beamtalk_stream:on([1, 2, 3, 4, 5, 6]),
    Even = beamtalk_stream:'select'(Stream, fun(X) -> X rem 2 =:= 0 end),
    ?assertEqual([2, 4, 6], beamtalk_stream:as_list(Even)).

%% select where nothing matches yields empty
select_none_match_test() ->
    Stream = beamtalk_stream:on([1, 2, 3]),
    None = beamtalk_stream:'select'(Stream, fun(_) -> false end),
    ?assertEqual([], beamtalk_stream:as_list(None)).

%% collect transforms elements
collect_finite_test() ->
    Stream = beamtalk_stream:on([1, 2, 3]),
    Doubled = beamtalk_stream:'collect'(Stream, fun(X) -> X * 2 end),
    ?assertEqual([2, 4, 6], beamtalk_stream:as_list(Doubled)).

%% reject excludes matching elements
reject_finite_test() ->
    Stream = beamtalk_stream:on([1, 2, 3, 4, 5]),
    Odds = beamtalk_stream:'reject'(Stream, fun(X) -> X rem 2 =:= 0 end),
    ?assertEqual([1, 3, 5], beamtalk_stream:as_list(Odds)).

%% drop skips first N elements
drop_finite_test() ->
    Stream = beamtalk_stream:on([1, 2, 3, 4, 5]),
    Dropped = beamtalk_stream:'drop'(Stream, 3),
    ?assertEqual([4, 5], beamtalk_stream:as_list(Dropped)).

%% drop more than available returns empty
drop_all_test() ->
    Stream = beamtalk_stream:on([1, 2]),
    Dropped = beamtalk_stream:'drop'(Stream, 10),
    ?assertEqual([], beamtalk_stream:as_list(Dropped)).

%% drop 0 returns same stream
drop_zero_test() ->
    Stream = beamtalk_stream:on([1, 2, 3]),
    Dropped = beamtalk_stream:'drop'(Stream, 0),
    ?assertEqual([1, 2, 3], beamtalk_stream:as_list(Dropped)).

%% Chaining: select then collect on infinite stream with take
chained_select_collect_test() ->
    Stream = beamtalk_stream:from(1),
    Even = beamtalk_stream:'select'(Stream, fun(X) -> X rem 2 =:= 0 end),
    Doubled = beamtalk_stream:'collect'(Even, fun(X) -> X * 2 end),
    ?assertEqual([4, 8, 12], beamtalk_stream:take(Doubled, 3)).

%% Chaining: drop then take on infinite stream
chained_drop_take_test() ->
    Stream = beamtalk_stream:from(1),
    Dropped = beamtalk_stream:'drop'(Stream, 5),
    ?assertEqual([6, 7, 8], beamtalk_stream:take(Dropped, 3)).

%%% ============================================================================
%%% Infinite Stream Truncation (BT-1966)
%%% ============================================================================

%% Infinite stream truncated by take
infinite_take_test() ->
    Stream = beamtalk_stream:from(100),
    ?assertEqual([100, 101, 102], beamtalk_stream:take(Stream, 3)).

%% Infinite from:by: truncated by take
infinite_from_by_take_test() ->
    Stream = beamtalk_stream:from_by(1, fun(N) -> N + 10 end),
    ?assertEqual([1, 11, 21, 31], beamtalk_stream:take(Stream, 4)).

%% detect on infinite stream terminates
detect_infinite_test() ->
    Stream = beamtalk_stream:from(1),
    ?assertEqual(7, beamtalk_stream:detect(Stream, fun(X) -> X =:= 7 end)).

%% any_satisfy on infinite stream terminates on match
any_satisfy_infinite_test() ->
    Stream = beamtalk_stream:from(1),
    ?assertEqual(true, beamtalk_stream:any_satisfy(Stream, fun(X) -> X > 100 end)).

%%% ============================================================================
%%% Terminal Operation Tests (BT-1966)
%%% ============================================================================

%% do: iterates and returns nil
do_side_effect_test() ->
    Self = self(),
    Stream = beamtalk_stream:on([1, 2, 3]),
    Result = beamtalk_stream:do(Stream, fun(X) -> Self ! {elem, X} end),
    ?assertEqual(nil, Result),
    ?assertEqual(
        {elem, 1},
        receive
            M1 -> M1
        after 100 -> timeout
        end
    ),
    ?assertEqual(
        {elem, 2},
        receive
            M2 -> M2
        after 100 -> timeout
        end
    ),
    ?assertEqual(
        {elem, 3},
        receive
            M3 -> M3
        after 100 -> timeout
        end
    ).

%% inject:into: sums a stream
inject_into_sum_test() ->
    Stream = beamtalk_stream:on([1, 2, 3, 4]),
    Sum = beamtalk_stream:inject_into(Stream, 0, fun(Acc, X) -> Acc + X end),
    ?assertEqual(10, Sum).

%% detect returns nil if no element matches (finite)
detect_not_found_test() ->
    Stream = beamtalk_stream:on([1, 2, 3]),
    ?assertEqual(nil, beamtalk_stream:detect(Stream, fun(X) -> X > 10 end)).

%% all_satisfy true case
all_satisfy_true_test() ->
    Stream = beamtalk_stream:on([2, 4, 6]),
    ?assertEqual(true, beamtalk_stream:all_satisfy(Stream, fun(X) -> X rem 2 =:= 0 end)).

%% all_satisfy false case (short-circuits on infinite stream)
all_satisfy_false_infinite_test() ->
    Stream = beamtalk_stream:from(1),
    ?assertEqual(false, beamtalk_stream:all_satisfy(Stream, fun(X) -> X < 5 end)).

%%% ============================================================================
%%% Error Handling in Type Checks (BT-1966)
%%% ============================================================================

%% select: with non-function raises type error
select_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := 'TypeError'},
        beamtalk_stream:'select'(not_a_stream, not_a_fun)
    ).

%% collect: with non-function raises type error
collect_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := 'TypeError'},
        beamtalk_stream:'collect'(not_a_stream, not_a_fun)
    ).

%% reject: with non-function raises type error
reject_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := 'TypeError'},
        beamtalk_stream:'reject'(not_a_stream, not_a_fun)
    ).

%% drop: with negative number raises type error
drop_type_error_test() ->
    Stream = beamtalk_stream:on([1]),
    ?assertError(
        #{'$beamtalk_class' := 'TypeError'},
        beamtalk_stream:'drop'(Stream, -1)
    ).

%% take: with negative number raises type error
take_type_error_test() ->
    Stream = beamtalk_stream:on([1]),
    ?assertError(
        #{'$beamtalk_class' := 'TypeError'},
        beamtalk_stream:take(Stream, -1)
    ).

%% do: with non-function raises type error
do_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := 'TypeError'},
        beamtalk_stream:do(not_a_stream, not_a_fun)
    ).

%% inject:into: with non-function raises type error
inject_into_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := 'TypeError'},
        beamtalk_stream:inject_into(not_a_stream, 0, not_a_fun)
    ).

%% detect: with non-function raises type error
detect_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := 'TypeError'},
        beamtalk_stream:detect(not_a_stream, not_a_fun)
    ).

%% any_satisfy: with non-function raises type error
any_satisfy_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := 'TypeError'},
        beamtalk_stream:any_satisfy(not_a_stream, not_a_fun)
    ).

%% all_satisfy: with non-function raises type error
all_satisfy_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := 'TypeError'},
        beamtalk_stream:all_satisfy(not_a_stream, not_a_fun)
    ).

%%% ============================================================================
%%% Display Tests (BT-1966)
%%% ============================================================================

%% print_string returns description
print_string_from_test() ->
    Stream = beamtalk_stream:from(1),
    ?assertEqual(<<"Stream(from: 1)">>, beamtalk_stream:print_string(Stream)).

%% print_string shows pipeline description after lazy ops
print_string_pipeline_test() ->
    Stream = beamtalk_stream:from(1),
    Filtered = beamtalk_stream:'select'(Stream, fun(_) -> true end),
    Desc = beamtalk_stream:print_string(Filtered),
    ?assertEqual(<<"Stream(from: 1) | select: [...]">>, Desc).

%%% ============================================================================
%%% FFI Shim Tests (BT-1966)
%%% ============================================================================

%% from/2 shim delegates to from_by
ffi_from_by_shim_test() ->
    Stream = beamtalk_stream:from(1, fun(N) -> N + 1 end),
    ?assertEqual([1, 2, 3], beamtalk_stream:take(Stream, 3)).

%% asList/1 shim delegates to as_list
ffi_as_list_shim_test() ->
    Stream = beamtalk_stream:on([1, 2]),
    ?assertEqual([1, 2], beamtalk_stream:asList(Stream)).

%% printString/1 shim delegates to print_string
ffi_print_string_shim_test() ->
    Stream = beamtalk_stream:on([1]),
    ?assert(is_binary(beamtalk_stream:printString(Stream))).

%%% ============================================================================
%%% Test Helpers
%%% ============================================================================

%% Infinite counter generator: 1, 2, 3, ...
make_counter_gen(N) ->
    fun() -> {N, make_counter_gen(N + 1)} end.

%% List generator
make_list_gen([]) ->
    fun() -> done end;
make_list_gen([H | T]) ->
    fun() -> {H, make_list_gen(T)} end.
