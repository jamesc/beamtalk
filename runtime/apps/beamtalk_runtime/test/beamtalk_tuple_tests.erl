%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Tests for beamtalk_tuple_ops runtime helper (BT-417).
%%
%% Tests the runtime helper module that provides complex Tuple operations
%% (bounds-checked at:, unwrap*, asString). The compiled Tuple class
%% (from lib/Tuple.bt) delegates these methods to beamtalk_tuple_ops.

-module(beamtalk_tuple_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% at/2 Tests — bounds-checked element access
%%% ============================================================================

at_valid_index_test() ->
    ?assertEqual(a, beamtalk_tuple_ops:at({a, b, c}, 1)),
    ?assertEqual(b, beamtalk_tuple_ops:at({a, b, c}, 2)),
    ?assertEqual(c, beamtalk_tuple_ops:at({a, b, c}, 3)),
    ?assertEqual(42, beamtalk_tuple_ops:at({ok, 42}, 2)).

at_out_of_bounds_test() ->
    ?assertError(#{'$beamtalk_class' := _, error := #beamtalk_error{kind = does_not_understand, class = 'Tuple', selector = 'at:'}},
                 beamtalk_tuple_ops:at({a, b}, 0)),
    ?assertError(#{'$beamtalk_class' := _, error := #beamtalk_error{kind = does_not_understand, class = 'Tuple', selector = 'at:'}},
                 beamtalk_tuple_ops:at({a, b}, 3)),
    ?assertError(#{'$beamtalk_class' := _, error := #beamtalk_error{kind = does_not_understand, class = 'Tuple', selector = 'at:'}},
                 beamtalk_tuple_ops:at({a, b}, -1)).

at_empty_tuple_test() ->
    ?assertError(#{'$beamtalk_class' := _, error := #beamtalk_error{kind = does_not_understand, class = 'Tuple', selector = 'at:'}},
                 beamtalk_tuple_ops:at({}, 1)).

at_non_integer_index_test() ->
    ?assertError(#{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Tuple', selector = 'at:'}},
                 beamtalk_tuple_ops:at({a, b}, foo)).

%%% ============================================================================
%%% unwrap/1 Tests
%%% ============================================================================

unwrap_ok_test() ->
    ?assertEqual(42, beamtalk_tuple_ops:unwrap({ok, 42})),
    ?assertEqual(hello, beamtalk_tuple_ops:unwrap({ok, hello})),
    ?assertEqual({a, b}, beamtalk_tuple_ops:unwrap({ok, {a, b}})).

unwrap_error_test() ->
    ?assertError(#{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Tuple', selector = 'unwrap'}},
                 beamtalk_tuple_ops:unwrap({error, not_found})),
    ?assertError(#{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Tuple', selector = 'unwrap'}},
                 beamtalk_tuple_ops:unwrap({error, reason})).

unwrap_invalid_pattern_test() ->
    ?assertError(#{'$beamtalk_class' := _, error := #beamtalk_error{kind = does_not_understand, class = 'Tuple', selector = 'unwrap'}},
                 beamtalk_tuple_ops:unwrap({a, b})).

%%% ============================================================================
%%% unwrap_or/2 Tests
%%% ============================================================================

unwrap_or_test() ->
    ?assertEqual(42, beamtalk_tuple_ops:unwrap_or({ok, 42}, default)),
    ?assertEqual(default, beamtalk_tuple_ops:unwrap_or({error, reason}, default)),
    ?assertEqual(default, beamtalk_tuple_ops:unwrap_or({a, b}, default)),
    ?assertEqual(nil, beamtalk_tuple_ops:unwrap_or({error, not_found}, nil)).

%%% ============================================================================
%%% unwrap_or_else/2 Tests
%%% ============================================================================

unwrap_or_else_test() ->
    ?assertEqual(42, beamtalk_tuple_ops:unwrap_or_else({ok, 42}, fun() -> default end)),
    ?assertEqual(default, beamtalk_tuple_ops:unwrap_or_else({error, reason}, fun() -> default end)),
    ?assertEqual(default, beamtalk_tuple_ops:unwrap_or_else({a, b}, fun() -> default end)).

unwrap_or_else_side_effects_test() ->
    Self = self(),
    %% Block should not be evaluated for {ok, _}
    beamtalk_tuple_ops:unwrap_or_else({ok, 42}, fun() -> Self ! evaluated, default end),
    receive
        evaluated -> ?assert(false)
    after 10 ->
        ok
    end,
    %% Block should be evaluated for other patterns
    beamtalk_tuple_ops:unwrap_or_else({error, reason}, fun() -> Self ! evaluated, default end),
    receive
        evaluated -> ok
    after 100 ->
        ?assert(false)
    end.

unwrap_or_else_non_function_test() ->
    %% Non-function argument raises does_not_understand (not type_error)
    ?assertError(#{'$beamtalk_class' := _, error := #beamtalk_error{kind = does_not_understand, class = 'Tuple', selector = 'unwrapOrElse:'}},
                 beamtalk_tuple_ops:unwrap_or_else({error, reason}, 42)).

%%% ============================================================================
%%% as_string/1 Tests
%%% ============================================================================

as_string_test() ->
    %% BT-536: Atoms now use #symbol notation via beamtalk_primitive:print_string/1
    ?assertEqual(<<"{#a, #b}">>, beamtalk_tuple_ops:as_string({a, b})),
    ?assertEqual(<<"{#ok, 42}">>, beamtalk_tuple_ops:as_string({ok, 42})),
    ?assertEqual(<<"{#error, #not_found}">>, beamtalk_tuple_ops:as_string({error, not_found})),
    ?assertEqual(<<"{#hello}">>, beamtalk_tuple_ops:as_string({hello})),
    ?assertEqual(<<"{}">>, beamtalk_tuple_ops:as_string({})).

%%% ============================================================================
%%% Compiled dispatch integration tests
%%% Tests via 'bt@stdlib@tuple':dispatch/3 (compiled from lib/Tuple.bt)
%%% to verify BIF mappings are wired correctly end-to-end.
%%% ============================================================================

dispatch_size_test() ->
    ?assertEqual(3, 'bt@stdlib@tuple':dispatch('size', [], {a, b, c})),
    ?assertEqual(0, 'bt@stdlib@tuple':dispatch('size', [], {})).

dispatch_at_test() ->
    ?assertEqual(b, 'bt@stdlib@tuple':dispatch('at:', [2], {a, b, c})).

dispatch_at_out_of_bounds_test() ->
    ?assertError(#{'$beamtalk_class' := _, error := #beamtalk_error{kind = does_not_understand, class = 'Tuple', selector = 'at:'}},
                 'bt@stdlib@tuple':dispatch('at:', [0], {a, b})).

dispatch_is_ok_test() ->
    ?assertEqual(true, 'bt@stdlib@tuple':dispatch('isOk', [], {ok, 42})),
    ?assertEqual(false, 'bt@stdlib@tuple':dispatch('isOk', [], {error, reason})),
    ?assertEqual(false, 'bt@stdlib@tuple':dispatch('isOk', [], {a, b})).

dispatch_is_error_test() ->
    ?assertEqual(true, 'bt@stdlib@tuple':dispatch('isError', [], {error, reason})),
    ?assertEqual(false, 'bt@stdlib@tuple':dispatch('isError', [], {ok, 42})).

dispatch_unwrap_test() ->
    ?assertEqual(42, 'bt@stdlib@tuple':dispatch('unwrap', [], {ok, 42})),
    ?assertError(#{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Tuple', selector = 'unwrap'}},
                 'bt@stdlib@tuple':dispatch('unwrap', [], {error, reason})).

dispatch_unwrap_or_test() ->
    ?assertEqual(42, 'bt@stdlib@tuple':dispatch('unwrapOr:', [default], {ok, 42})),
    ?assertEqual(default, 'bt@stdlib@tuple':dispatch('unwrapOr:', [default], {error, reason})).

dispatch_as_string_test() ->
    %% BT-536: Atoms now use #symbol notation
    ?assertEqual(<<"{#ok, 42}">>, 'bt@stdlib@tuple':dispatch('asString', [], {ok, 42})).

dispatch_unwrap_or_else_test() ->
    ?assertEqual(42, 'bt@stdlib@tuple':dispatch('unwrapOrElse:', [fun() -> default end], {ok, 42})),
    ?assertEqual(default, 'bt@stdlib@tuple':dispatch('unwrapOrElse:', [fun() -> default end], {error, reason})).

dispatch_unwrap_invalid_pattern_test() ->
    ?assertError(#{'$beamtalk_class' := _, error := #beamtalk_error{kind = does_not_understand, class = 'Tuple', selector = 'unwrap'}},
                 'bt@stdlib@tuple':dispatch('unwrap', [], {a, b})).

%%% ============================================================================
%%% beamtalk_primitive:send integration tests
%%% Verifies the full dispatch path: primitive:send → compiled tuple module
%%% ============================================================================

primitive_send_size_test() ->
    ?assertEqual(3, beamtalk_primitive:send({a, b, c}, 'size', [])),
    ?assertEqual(0, beamtalk_primitive:send({}, 'size', [])).

primitive_send_at_test() ->
    ?assertEqual(b, beamtalk_primitive:send({a, b, c}, 'at:', [2])).

primitive_send_is_ok_test() ->
    ?assertEqual(true, beamtalk_primitive:send({ok, 42}, 'isOk', [])),
    ?assertEqual(false, beamtalk_primitive:send({error, reason}, 'isOk', [])).

primitive_send_unwrap_test() ->
    ?assertEqual(42, beamtalk_primitive:send({ok, 42}, 'unwrap', [])).

primitive_send_as_string_test() ->
    ?assertEqual(<<"{a, b}">>, beamtalk_primitive:send({a, b}, 'asString', [])).


