%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_json_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_json module.

Tests cover parse:/1, generate:/1, prettyPrint:/1, and the FFI no-colon
aliases (parse/1, generate/1, prettyPrint/1).
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% parse:/1
%%% ============================================================================

parse_integer_test() ->
    R = beamtalk_json:'parse:'(<<"42">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := 42}, R).

parse_string_test() ->
    R = beamtalk_json:'parse:'(<<"\"hello\"">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := {beamtalk_string, <<"hello">>}}, R).

parse_array_test() ->
    R = beamtalk_json:'parse:'(<<"[1,2,3]">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := [1, 2, 3]}, R).

parse_null_test() ->
    R = beamtalk_json:'parse:'(<<"null">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil}, R).

parse_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_json:'parse:'(not_a_binary)
    ).

parse_invalid_json_test() ->
    R = beamtalk_json:'parse:'(<<"{invalid">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{'$beamtalk_class' := _, error := #beamtalk_error{kind = parse_error}}
        },
        R
    ).

%%% ============================================================================
%%% parse:/1 — additional coverage
%%% ============================================================================

parse_object_test() ->
    R = beamtalk_json:'parse:'(<<"{\"a\":1,\"b\":2}">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
    #{okValue := Val} = R,
    ?assertEqual(1, maps:get({beamtalk_string, <<"a">>}, Val)),
    ?assertEqual(2, maps:get({beamtalk_string, <<"b">>}, Val)).

parse_boolean_true_test() ->
    R = beamtalk_json:'parse:'(<<"true">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := true}, R).

parse_boolean_false_test() ->
    R = beamtalk_json:'parse:'(<<"false">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := false}, R).

parse_float_test() ->
    R = beamtalk_json:'parse:'(<<"3.14">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
    #{okValue := Val} = R,
    ?assert(is_float(Val)),
    ?assert(abs(Val - 3.14) < 0.001).

parse_nested_test() ->
    Json = <<"{\"items\":[{\"name\":\"a\"},{\"name\":\"b\"}],\"count\":2}">>,
    R = beamtalk_json:'parse:'(Json),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
    #{okValue := Val} = R,
    ?assertEqual(2, maps:get({beamtalk_string, <<"count">>}, Val)),
    Items = maps:get({beamtalk_string, <<"items">>}, Val),
    ?assertEqual(2, length(Items)),
    ?assertEqual({beamtalk_string, <<"a">>}, maps:get({beamtalk_string, <<"name">>}, hd(Items))).

parse_unicode_string_test() ->
    R = beamtalk_json:'parse:'(<<"\"caf\\u00e9\"">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
    #{okValue := Val} = R,
    ?assertEqual({beamtalk_string, <<"café"/utf8>>}, Val).

parse_nested_null_test() ->
    R = beamtalk_json:'parse:'(<<"[null,{\"x\":null}]">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
    #{okValue := [nil, Map]} = R,
    ?assertEqual(nil, maps:get({beamtalk_string, <<"x">>}, Map)).

parse_empty_object_test() ->
    R = beamtalk_json:'parse:'(<<"{}">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := #{}}, R).

parse_empty_array_test() ->
    R = beamtalk_json:'parse:'(<<"[]">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := []}, R).

parse_unexpected_end_test() ->
    R = beamtalk_json:'parse:'(<<"[1,2,">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{'$beamtalk_class' := _, error := #beamtalk_error{kind = parse_error}}
        },
        R
    ).

%%% ============================================================================
%%% generate:/1
%%% ============================================================================

generate_integer_test() ->
    ?assertEqual({beamtalk_string, <<"42">>}, beamtalk_json:'generate:'(42)).

generate_null_test() ->
    ?assertEqual({beamtalk_string, <<"null">>}, beamtalk_json:'generate:'(nil)).

generate_list_test() ->
    ?assertEqual({beamtalk_string, <<"[1,2,3]">>}, beamtalk_json:'generate:'([1, 2, 3])).

generate_string_test() ->
    ?assertEqual({beamtalk_string, <<"\"hello\"">>}, beamtalk_json:'generate:'(<<"hello">>)).

generate_boolean_test() ->
    ?assertEqual({beamtalk_string, <<"true">>}, beamtalk_json:'generate:'(true)),
    ?assertEqual({beamtalk_string, <<"false">>}, beamtalk_json:'generate:'(false)).

generate_float_test() ->
    Result = beamtalk_json:'generate:'(3.14),
    ?assertMatch({beamtalk_string, _}, Result),
    %% Round-trip: parse it back
    Parsed = json:decode(element(2, Result)),
    ?assert(abs(Parsed - 3.14) < 0.001).

generate_map_test() ->
    Result = beamtalk_json:'generate:'(#{<<"key">> => <<"value">>}),
    Decoded = json:decode(element(2, Result)),
    ?assertEqual(<<"value">>, maps:get(<<"key">>, Decoded)).

generate_nested_test() ->
    Value = #{<<"items">> => [1, 2, #{<<"nested">> => true}]},
    Result = beamtalk_json:'generate:'(Value),
    Decoded = json:decode(element(2, Result)),
    Items = maps:get(<<"items">>, Decoded),
    ?assertEqual(3, length(Items)).

generate_atom_to_string_test() ->
    %% Atoms (symbols) should be converted to strings
    Result = beamtalk_json:'generate:'(hello),
    ?assertEqual({beamtalk_string, <<"\"hello\"">>}, Result).

generate_strips_beamtalk_class_test() ->
    %% Maps with $beamtalk_class should have it stripped
    Value = #{'$beamtalk_class' => 'Dictionary', <<"key">> => <<"val">>},
    Result = beamtalk_json:'generate:'(Value),
    Decoded = json:decode(element(2, Result)),
    ?assertNot(maps:is_key(<<"$beamtalk_class">>, Decoded)),
    ?assertEqual(<<"val">>, maps:get(<<"key">>, Decoded)).

generate_nil_in_list_test() ->
    Result = beamtalk_json:'generate:'([1, nil, <<"a">>]),
    ?assertEqual({beamtalk_string, <<"[1,null,\"a\"]">>}, Result).

generate_unsupported_type_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_json:'generate:'({unsupported, tuple})
    ).

%%% ============================================================================
%%% generate:/1 — asJson hook
%%%
%%% 'bt@json_hook_fixture' stands in for a compiled Value class named
%%% 'JsonHookFixture' whose instances implement asJson.
%%% ============================================================================

generate_as_json_hook_test() ->
    Result = beamtalk_json:'generate:'('bt@json_hook_fixture':new(plain)),
    Decoded = json:decode(element(2, Result)),
    ?assertEqual(<<"fixture">>, maps:get(<<"kind">>, Decoded)),
    ?assertEqual(true, maps:get(<<"ok">>, Decoded)).

generate_as_json_hook_in_list_test() ->
    Result = beamtalk_json:'generate:'([1, 'bt@json_hook_fixture':new(plain)]),
    [1, Decoded] = json:decode(element(2, Result)),
    ?assertEqual(<<"fixture">>, maps:get(<<"kind">>, Decoded)).

generate_as_json_hook_in_map_value_test() ->
    Result = beamtalk_json:'generate:'(#{<<"item">> => 'bt@json_hook_fixture':new(plain)}),
    Decoded = json:decode(element(2, Result)),
    ?assertEqual(<<"fixture">>, maps:get(<<"kind">>, maps:get(<<"item">>, Decoded))).

generate_as_json_hook_recursive_test() ->
    %% The hook's own result contains another asJson object — converted recursively.
    Result = beamtalk_json:'generate:'('bt@json_hook_fixture':new(nested)),
    Decoded = json:decode(element(2, Result)),
    Inner = maps:get(<<"inner">>, Decoded),
    ?assertEqual(<<"fixture">>, maps:get(<<"kind">>, Inner)).

generate_as_json_self_return_test() ->
    %% A hook returning the receiver itself must raise, not loop forever.
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Json'}},
        beamtalk_json:'generate:'('bt@json_hook_fixture':new(self_return))
    ).

generate_as_json_nlr_passes_through_test() ->
    %% BT-3740: a `^` out of an asJson hook is a non-local return, not an
    %% encode failure: both throw shapes reach the caller untouched.
    ?assertThrow(
        {'$bt_nlr', nlr_token, nlr_value},
        beamtalk_json:'generate:'('bt@json_hook_fixture':new(nlr))
    ),
    ?assertThrow(
        {'$bt_nlr', nlr_token, nlr_value, nlr_state},
        beamtalk_json:'generate:'('bt@json_hook_fixture':new(nlr_actor))
    ).

generate_as_json_mutual_cycle_test() ->
    %% Not a literal self-return: A's hook embeds a distinct B, whose hook
    %% embeds A back. Must raise before the recursion overflows the stack.
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Json'}},
        beamtalk_json:'generate:'('bt@json_hook_fixture':new(mutual_a))
    ).

generate_as_json_shared_reference_not_a_cycle_test() ->
    %% The same asJson object referenced twice as SIBLINGS (a DAG, not a
    %% cycle) must serialize fine — Seen tracks the ancestor chain only,
    %% not "every object encountered anywhere in this generate: call".
    Shared = 'bt@json_hook_fixture':new(plain),
    Result = beamtalk_json:'generate:'([Shared, Shared]),
    [First, Second] = json:decode(element(2, Result)),
    ?assertEqual(<<"fixture">>, maps:get(<<"kind">>, First)),
    ?assertEqual(<<"fixture">>, maps:get(<<"kind">>, Second)).

generate_tagged_map_without_as_json_keeps_fields_test() ->
    %% Legacy behaviour: a tagged map whose class has no asJson still encodes
    %% its fields directly with the class tag stripped.
    Value = #{'$beamtalk_class' => 'NoSuchHookClass', <<"a">> => 1},
    Decoded = json:decode(element(2, beamtalk_json:'generate:'(Value))),
    ?assertNot(maps:is_key(<<"$beamtalk_class">>, Decoded)),
    ?assertEqual(1, maps:get(<<"a">>, Decoded)).

pretty_print_as_json_hook_test() ->
    %% prettyPrint: shares the asJson hook with generate:.
    Result = beamtalk_json:'prettyPrint:'('bt@json_hook_fixture':new(plain)),
    Decoded = json:decode(element(2, Result)),
    ?assertEqual(<<"fixture">>, maps:get(<<"kind">>, Decoded)).

pretty_print_as_json_nlr_passes_through_test() ->
    %% BT-3740: prettyPrint: shares the encode pipeline, so a `^` out of an
    %% asJson hook passes through here too.
    ?assertThrow(
        {'$bt_nlr', nlr_token, nlr_value},
        beamtalk_json:'prettyPrint:'('bt@json_hook_fixture':new(nlr))
    ),
    ?assertThrow(
        {'$bt_nlr', nlr_token, nlr_value, nlr_state},
        beamtalk_json:'prettyPrint:'('bt@json_hook_fixture':new(nlr_actor))
    ).

%%% ============================================================================
%%% prettyPrint:/1
%%% ============================================================================

pretty_print_test() ->
    Result = beamtalk_json:'prettyPrint:'(42),
    ?assertMatch({beamtalk_string, _}, Result).

pretty_print_object_test() ->
    Result = beamtalk_json:'prettyPrint:'(#{<<"a">> => 1}),
    ?assertMatch({beamtalk_string, _}, Result),
    %% Should contain newlines (pretty-printed)
    ?assertNotEqual(nomatch, binary:match(element(2, Result), <<"\n">>)),
    %% Should be valid JSON
    Decoded = json:decode(element(2, Result)),
    ?assertEqual(1, maps:get(<<"a">>, Decoded)).

pretty_print_nested_test() ->
    Value = #{<<"outer">> => #{<<"inner">> => [1, 2, 3]}},
    Result = beamtalk_json:'prettyPrint:'(Value),
    Decoded = json:decode(element(2, Result)),
    Inner = maps:get(<<"inner">>, maps:get(<<"outer">>, Decoded)),
    ?assertEqual([1, 2, 3], Inner).

pretty_print_unsupported_type_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_json:'prettyPrint:'({unsupported, tuple})
    ).

%%% ============================================================================
%%% prettify_term/1
%%% ============================================================================

prettify_term_test() ->
    Result = beamtalk_json:prettify_term(#{<<"key">> => <<"value">>}),
    ?assert(is_binary(Result)),
    Decoded = json:decode(Result),
    ?assertEqual(<<"value">>, maps:get(<<"key">>, Decoded)).

%%% ============================================================================
%%% Round-trip tests
%%% ============================================================================

round_trip_integer_test() ->
    Original = 42,
    Json = beamtalk_json:'generate:'(Original),
    #{okValue := Decoded} = beamtalk_json:'parse:'(Json),
    ?assertEqual(Original, Decoded).

round_trip_string_test() ->
    %% BT-4173: a String round-trips exactly (tagged in, tagged out).
    Original = {beamtalk_string, <<"hello world">>},
    Json = beamtalk_json:'generate:'(Original),
    #{okValue := Decoded} = beamtalk_json:'parse:'(Json),
    ?assertEqual(Original, Decoded).

round_trip_list_test() ->
    Original = [1, {beamtalk_string, <<"two">>}, true, false, nil],
    Json = beamtalk_json:'generate:'(Original),
    #{okValue := Decoded} = beamtalk_json:'parse:'(Json),
    ?assertEqual(Original, Decoded).

round_trip_nested_map_test() ->
    Original = #{
        {beamtalk_string, <<"name">>} => {beamtalk_string, <<"test">>},
        {beamtalk_string, <<"values">>} => [1, 2, 3]
    },
    Json = beamtalk_json:'generate:'(Original),
    #{okValue := Decoded} = beamtalk_json:'parse:'(Json),
    ?assertEqual(Original, Decoded).

%%% ============================================================================
%%% FFI no-colon aliases
%%% ============================================================================

parse_alias_test() ->
    R = beamtalk_json:parse(<<"42">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := 42}, R).

parse_alias_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_json:parse(not_a_binary)
    ).

generate_alias_test() ->
    ?assertEqual({beamtalk_string, <<"42">>}, beamtalk_json:generate(42)).

pretty_print_alias_test() ->
    Result = beamtalk_json:prettyPrint(42),
    ?assertMatch({beamtalk_string, _}, Result).

%%% ============================================================================
%%% Additional coverage — prettify/4 edge cases
%%% ============================================================================

pretty_print_empty_object_test() ->
    %% Covers the empty-object branch (L233-234): the pretty-printer skips
    %% inserting a newline after the opening brace when it sees `}` next.
    Result = beamtalk_json:'prettyPrint:'(#{}),
    ?assertEqual({beamtalk_string, <<"{\n}">>}, Result),
    %% Should still decode back to an empty map
    ?assertEqual(#{}, json:decode(element(2, Result))).

pretty_print_empty_array_test() ->
    %% Covers the empty-array branch (L241-242).
    Result = beamtalk_json:'prettyPrint:'([]),
    ?assertEqual({beamtalk_string, <<"[\n]">>}, Result),
    ?assertEqual([], json:decode(element(2, Result))).

pretty_print_nested_empty_collections_test() ->
    %% Empty inside non-empty container
    Result = beamtalk_json:'prettyPrint:'(#{<<"a">> => #{}, <<"b">> => []}),
    ?assertMatch({beamtalk_string, _}, Result),
    Decoded = json:decode(element(2, Result)),
    ?assertEqual(#{}, maps:get(<<"a">>, Decoded)),
    ?assertEqual([], maps:get(<<"b">>, Decoded)).

pretty_print_escaped_backslash_test() ->
    %% Covers the in-string escape branch (L224-225): prettify sees \" inside string
    %% The encoded JSON contains \\" sequences (backslash + quote) inside string literals.
    Result = beamtalk_json:'prettyPrint:'(<<"quote\"backslash\\end">>),
    ?assertMatch({beamtalk_string, _}, Result),
    Decoded = json:decode(element(2, Result)),
    ?assertEqual(<<"quote\"backslash\\end">>, Decoded).

prettify_term_empty_object_test() ->
    %% Direct exercise of prettify_term helper with empty map.
    %% Same behaviour as prettyPrint:/1 — newline before the closing brace.
    ?assertEqual(<<"{\n}">>, beamtalk_json:prettify_term(#{})).

%%% ============================================================================
%%% Additional coverage — parse error catch-all
%%% ============================================================================

parse_unexpected_sequence_test() ->
    %% Lone garbage tokens raise {unexpected_sequence, _} from json:decode
    R = beamtalk_json:'parse:'(<<"xyz">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{'$beamtalk_class' := _, error := #beamtalk_error{kind = parse_error}}
        },
        R
    ).

parse_trailing_garbage_test() ->
    R = beamtalk_json:'parse:'(<<"{\"a\":1}junk">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{'$beamtalk_class' := _, error := #beamtalk_error{kind = parse_error}}
        },
        R
    ).

%%% ============================================================================
%%% Additional coverage — generate/prettyPrint error paths
%%% ============================================================================

generate_nested_unsupported_type_test() ->
    %% Covers prepare_for_encode raising inside the recursive list walk,
    %% hitting the #beamtalk_error{} re-raise branch in generate:/1 (L95-96).
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Json'}},
        beamtalk_json:'generate:'([1, {oops, bad}, 3])
    ).

generate_unsupported_in_map_test() ->
    %% Tuple as map value triggers prepare_for_encode error from inside map walk
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Json'}},
        beamtalk_json:'generate:'(#{<<"k">> => {oops, bad}})
    ).

pretty_print_nested_unsupported_type_test() ->
    %% Covers the #beamtalk_error{} re-raise branch in prettyPrint:/1 (L117-118).
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Json'}},
        beamtalk_json:'prettyPrint:'([1, {oops, bad}, 3])
    ).

%%% ============================================================================
%%% Additional coverage — large / deeply nested structures
%%% ============================================================================

parse_deeply_nested_array_test() ->
    %% Build [[[[[[[[[[42]]]]]]]]]] and round-trip it.
    Depth = 20,
    Json = iolist_to_binary([
        binary:copy(<<"[">>, Depth),
        <<"42">>,
        binary:copy(<<"]">>, Depth)
    ]),
    R = beamtalk_json:'parse:'(Json),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
    #{okValue := Decoded} = R,
    %% Unwrap back to integer
    Unwrap = fun
        U(X) when is_list(X) -> U(hd(X));
        U(N) -> N
    end,
    ?assertEqual(42, Unwrap(Decoded)).

generate_deeply_nested_map_test() ->
    %% Build {"k": {"k": ... {"k": 1}}} at depth 10
    Depth = 10,
    Nested = lists:foldl(
        fun(_, Acc) -> #{{beamtalk_string, <<"k">>} => Acc} end,
        1,
        lists:seq(1, Depth)
    ),
    Json = beamtalk_json:'generate:'(Nested),
    ?assertMatch({beamtalk_string, _}, Json),
    %% Round-trip
    #{okValue := Decoded} = beamtalk_json:'parse:'(Json),
    ?assertEqual(Nested, Decoded).

%%% ============================================================================
%%% BT-4171 — decoded strings answer String (validated at the boundary)
%%% ============================================================================

parse_string_leaf_answers_string_test() ->
    #{okValue := Val} = beamtalk_json:'parse:'(<<"\"hello\"">>),
    ?assertEqual({beamtalk_string, <<"hello">>}, Val),
    ?assertEqual('String', beamtalk_primitive:class_of(Val)).

parse_unicode_leaf_answers_string_test() ->
    %% \u00e9 escape decodes to é (U+00E9)
    #{okValue := Val} = beamtalk_json:'parse:'(<<"\"h\\u00e9llo\"">>),
    ?assertEqual({beamtalk_string, <<"héllo"/utf8>>}, Val),
    ?assertEqual('String', beamtalk_primitive:class_of(Val)).

parse_object_keys_and_values_answer_string_test() ->
    #{okValue := Val} = beamtalk_json:'parse:'(<<"{\"name\":\"Ada\",\"city\":\"Berlin\"}">>),
    ?assertEqual('String', beamtalk_primitive:class_of(maps:get({beamtalk_string, <<"name">>}, Val))),
    ?assertEqual('String', beamtalk_primitive:class_of(maps:get({beamtalk_string, <<"city">>}, Val))),
    lists:foreach(
        fun(K) -> ?assertEqual('String', beamtalk_primitive:class_of(K)) end,
        maps:keys(Val)
    ).

parse_nested_strings_answer_string_test() ->
    #{okValue := Val} = beamtalk_json:'parse:'(
        <<"{\"users\":[{\"name\":\"Ada\"},{\"name\":\"Grace\"}]}">>
    ),
    [U1, U2] = maps:get({beamtalk_string, <<"users">>}, Val),
    ?assertEqual('String', beamtalk_primitive:class_of(maps:get({beamtalk_string, <<"name">>}, U1))),
    ?assertEqual('String', beamtalk_primitive:class_of(maps:get({beamtalk_string, <<"name">>}, U2))).

parse_non_string_leaves_keep_class_test() ->
    %% The new binary arm in normalize_decoded/1 must not disturb other types.
    #{okValue := Val} = beamtalk_json:'parse:'(<<"{\"n\":42,\"t\":true,\"z\":null}">>),
    ?assertEqual('Integer', beamtalk_primitive:class_of(maps:get({beamtalk_string, <<"n">>}, Val))),
    ?assertEqual('True', beamtalk_primitive:class_of(maps:get({beamtalk_string, <<"t">>}, Val))),
    ?assertEqual('UndefinedObject', beamtalk_primitive:class_of(maps:get({beamtalk_string, <<"z">>}, Val))).

parse_invalid_utf8_input_answers_result_error_test() ->
    %% OTP rejects the bad byte before normalization; still Result error:.
    %% (Byte 255 as an explicit segment: never valid UTF-8.)
    R = beamtalk_json:'parse:'(<<"\"abc", 255, "def\"">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{'$beamtalk_class' := _, error := #beamtalk_error{kind = parse_error}}
        },
        R
    ).
