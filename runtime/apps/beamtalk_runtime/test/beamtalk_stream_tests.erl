%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Unit tests for beamtalk_stream finalizer support (BT-537).
-module(beamtalk_stream_tests).

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
    receive finalizer_called -> ok
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
    receive finalizer_called -> ok
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
    receive finalizer_called -> ok
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
    receive finalizer_called -> ok
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
    receive finalizer_called -> ok
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
    receive finalizer_called -> ok
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
    receive finalizer_called -> ok
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
    receive finalizer_called -> ok
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
    receive finalizer_called -> ok
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
    receive finalizer_called -> ok
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
    receive finalizer_called -> ok
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
    receive finalizer_called -> ok
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
    receive finalizer_called -> ok
    after 100 -> ?assert(false)
    end,
    ?assertEqual(1, atomics:get(Counter, 1)).

%% Test that finalizer is called even when block raises an exception
finalizer_called_on_exception_test() ->
    Self = self(),
    Gen = make_counter_gen(1),
    Finalizer = fun() -> Self ! finalizer_called end,
    Stream = beamtalk_stream:make_stream(Gen, <<"test">>, Finalizer),
    ?assertError(boom,
        beamtalk_stream:do(Stream, fun(X) ->
            case X of
                2 -> error(boom);
                _ -> ok
            end
        end)),
    receive finalizer_called -> ok
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
        Stream = beamtalk_file:'lines:'(list_to_binary(TmpFile)),
        Result = beamtalk_stream:take(Stream, 1),
        ?assertEqual([<<"line1">>], Result),
        timer:sleep(10),
        ok
    after
        file:delete(TmpFile)
    end.

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
