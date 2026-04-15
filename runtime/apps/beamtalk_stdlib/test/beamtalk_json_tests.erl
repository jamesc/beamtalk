%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_json_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_json module (BT-1142).

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
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := <<"hello">>}, R).

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
    ?assertEqual(1, maps:get(<<"a">>, Val)),
    ?assertEqual(2, maps:get(<<"b">>, Val)).

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
    ?assertEqual(2, maps:get(<<"count">>, Val)),
    Items = maps:get(<<"items">>, Val),
    ?assertEqual(2, length(Items)),
    ?assertEqual(<<"a">>, maps:get(<<"name">>, hd(Items))).

parse_unicode_string_test() ->
    R = beamtalk_json:'parse:'(<<"\"caf\\u00e9\"">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
    #{okValue := Val} = R,
    ?assertEqual(<<"café"/utf8>>, Val).

parse_nested_null_test() ->
    R = beamtalk_json:'parse:'(<<"[null,{\"x\":null}]">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
    #{okValue := [nil, Map]} = R,
    ?assertEqual(nil, maps:get(<<"x">>, Map)).

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
    ?assertEqual(<<"42">>, beamtalk_json:'generate:'(42)).

generate_null_test() ->
    ?assertEqual(<<"null">>, beamtalk_json:'generate:'(nil)).

generate_list_test() ->
    ?assertEqual(<<"[1,2,3]">>, beamtalk_json:'generate:'([1, 2, 3])).

generate_string_test() ->
    ?assertEqual(<<"\"hello\"">>, beamtalk_json:'generate:'(<<"hello">>)).

generate_boolean_test() ->
    ?assertEqual(<<"true">>, beamtalk_json:'generate:'(true)),
    ?assertEqual(<<"false">>, beamtalk_json:'generate:'(false)).

generate_float_test() ->
    Result = beamtalk_json:'generate:'(3.14),
    ?assert(is_binary(Result)),
    %% Round-trip: parse it back
    Parsed = json:decode(Result),
    ?assert(abs(Parsed - 3.14) < 0.001).

generate_map_test() ->
    Result = beamtalk_json:'generate:'(#{<<"key">> => <<"value">>}),
    Decoded = json:decode(Result),
    ?assertEqual(<<"value">>, maps:get(<<"key">>, Decoded)).

generate_nested_test() ->
    Value = #{<<"items">> => [1, 2, #{<<"nested">> => true}]},
    Result = beamtalk_json:'generate:'(Value),
    Decoded = json:decode(Result),
    Items = maps:get(<<"items">>, Decoded),
    ?assertEqual(3, length(Items)).

generate_atom_to_string_test() ->
    %% Atoms (symbols) should be converted to strings
    Result = beamtalk_json:'generate:'(hello),
    ?assertEqual(<<"\"hello\"">>, Result).

generate_strips_beamtalk_class_test() ->
    %% Maps with $beamtalk_class should have it stripped
    Value = #{'$beamtalk_class' => 'Dictionary', <<"key">> => <<"val">>},
    Result = beamtalk_json:'generate:'(Value),
    Decoded = json:decode(Result),
    ?assertNot(maps:is_key(<<"$beamtalk_class">>, Decoded)),
    ?assertEqual(<<"val">>, maps:get(<<"key">>, Decoded)).

generate_nil_in_list_test() ->
    Result = beamtalk_json:'generate:'([1, nil, <<"a">>]),
    ?assertEqual(<<"[1,null,\"a\"]">>, Result).

generate_unsupported_type_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_json:'generate:'({unsupported, tuple})
    ).

%%% ============================================================================
%%% prettyPrint:/1
%%% ============================================================================

pretty_print_test() ->
    Result = beamtalk_json:'prettyPrint:'(42),
    ?assert(is_binary(Result)).

pretty_print_object_test() ->
    Result = beamtalk_json:'prettyPrint:'(#{<<"a">> => 1}),
    ?assert(is_binary(Result)),
    %% Should contain newlines (pretty-printed)
    ?assertNotEqual(nomatch, binary:match(Result, <<"\n">>)),
    %% Should be valid JSON
    Decoded = json:decode(Result),
    ?assertEqual(1, maps:get(<<"a">>, Decoded)).

pretty_print_nested_test() ->
    Value = #{<<"outer">> => #{<<"inner">> => [1, 2, 3]}},
    Result = beamtalk_json:'prettyPrint:'(Value),
    Decoded = json:decode(Result),
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
    Original = <<"hello world">>,
    Json = beamtalk_json:'generate:'(Original),
    #{okValue := Decoded} = beamtalk_json:'parse:'(Json),
    ?assertEqual(Original, Decoded).

round_trip_list_test() ->
    Original = [1, <<"two">>, true, false, nil],
    Json = beamtalk_json:'generate:'(Original),
    #{okValue := Decoded} = beamtalk_json:'parse:'(Json),
    ?assertEqual([1, <<"two">>, true, false, nil], Decoded).

round_trip_nested_map_test() ->
    Original = #{<<"name">> => <<"test">>, <<"values">> => [1, 2, 3]},
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
    ?assertEqual(<<"42">>, beamtalk_json:generate(42)).

pretty_print_alias_test() ->
    Result = beamtalk_json:prettyPrint(42),
    ?assert(is_binary(Result)).

%%% ============================================================================
%%% Additional coverage — prettify/4 edge cases (BT-1983)
%%% ============================================================================

pretty_print_empty_object_test() ->
    %% Covers the empty-object branch (L233-234): the pretty-printer skips
    %% inserting a newline after the opening brace when it sees `}` next.
    Result = beamtalk_json:'prettyPrint:'(#{}),
    ?assertEqual(<<"{\n}">>, Result),
    %% Should still decode back to an empty map
    ?assertEqual(#{}, json:decode(Result)).

pretty_print_empty_array_test() ->
    %% Covers the empty-array branch (L241-242).
    Result = beamtalk_json:'prettyPrint:'([]),
    ?assertEqual(<<"[\n]">>, Result),
    ?assertEqual([], json:decode(Result)).

pretty_print_nested_empty_collections_test() ->
    %% Empty inside non-empty container
    Result = beamtalk_json:'prettyPrint:'(#{<<"a">> => #{}, <<"b">> => []}),
    ?assert(is_binary(Result)),
    Decoded = json:decode(Result),
    ?assertEqual(#{}, maps:get(<<"a">>, Decoded)),
    ?assertEqual([], maps:get(<<"b">>, Decoded)).

pretty_print_escaped_backslash_test() ->
    %% Covers the in-string escape branch (L224-225): prettify sees \" inside string
    %% The encoded JSON contains \\" sequences (backslash + quote) inside string literals.
    Result = beamtalk_json:'prettyPrint:'(<<"quote\"backslash\\end">>),
    ?assert(is_binary(Result)),
    Decoded = json:decode(Result),
    ?assertEqual(<<"quote\"backslash\\end">>, Decoded).

prettify_term_empty_object_test() ->
    %% Direct exercise of prettify_term helper with empty map.
    %% Same behaviour as prettyPrint:/1 — newline before the closing brace.
    ?assertEqual(<<"{\n}">>, beamtalk_json:prettify_term(#{})).

%%% ============================================================================
%%% Additional coverage — parse error catch-all (BT-1983)
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
%%% Additional coverage — generate/prettyPrint error paths (BT-1983)
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
%%% Additional coverage — large / deeply nested structures (BT-1983)
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
        fun(_, Acc) -> #{<<"k">> => Acc} end,
        1,
        lists:seq(1, Depth)
    ),
    Json = beamtalk_json:'generate:'(Nested),
    ?assert(is_binary(Json)),
    %% Round-trip
    #{okValue := Decoded} = beamtalk_json:'parse:'(Json),
    ?assertEqual(Nested, Decoded).
