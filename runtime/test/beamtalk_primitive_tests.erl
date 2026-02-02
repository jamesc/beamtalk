%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_primitive module.
%%%
%%% Tests class_of/1, send/3, and responds_to/2 for all primitive types.

-module(beamtalk_primitive_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% class_of/1 tests
%%% ============================================================================

class_of_integer_test() ->
    ?assertEqual('Integer', beamtalk_primitive:class_of(0)),
    ?assertEqual('Integer', beamtalk_primitive:class_of(42)),
    ?assertEqual('Integer', beamtalk_primitive:class_of(-100)),
    ?assertEqual('Integer', beamtalk_primitive:class_of(999999999999)).

class_of_float_test() ->
    ?assertEqual('Float', beamtalk_primitive:class_of(0.0)),
    ?assertEqual('Float', beamtalk_primitive:class_of(3.14)),
    ?assertEqual('Float', beamtalk_primitive:class_of(-2.5)),
    ?assertEqual('Float', beamtalk_primitive:class_of(1.0e10)).

class_of_string_test() ->
    ?assertEqual('String', beamtalk_primitive:class_of(<<>>)),
    ?assertEqual('String', beamtalk_primitive:class_of(<<"hello">>)),
    ?assertEqual('String', beamtalk_primitive:class_of(<<"with spaces">>)),
    ?assertEqual('String', beamtalk_primitive:class_of(<<"Unicode: 你好"/utf8>>)).

class_of_boolean_test() ->
    ?assertEqual('Boolean', beamtalk_primitive:class_of(true)),
    ?assertEqual('Boolean', beamtalk_primitive:class_of(false)).

class_of_nil_test() ->
    ?assertEqual('UndefinedObject', beamtalk_primitive:class_of(nil)).

class_of_block_test() ->
    ?assertEqual('Block', beamtalk_primitive:class_of(fun() -> ok end)),
    ?assertEqual('Block', beamtalk_primitive:class_of(fun(X) -> X + 1 end)),
    ?assertEqual('Block', beamtalk_primitive:class_of(fun(X, Y) -> X + Y end)).

class_of_symbol_test() ->
    ?assertEqual('Symbol', beamtalk_primitive:class_of(atom)),
    ?assertEqual('Symbol', beamtalk_primitive:class_of('hello')),
    ?assertEqual('Symbol', beamtalk_primitive:class_of('with_underscores')).

class_of_array_test() ->
    ?assertEqual('Array', beamtalk_primitive:class_of([])),
    ?assertEqual('Array', beamtalk_primitive:class_of([1, 2, 3])),
    ?assertEqual('Array', beamtalk_primitive:class_of([a, b, c])),
    ?assertEqual('Array', beamtalk_primitive:class_of([[1], [2], [3]])).

class_of_dictionary_test() ->
    ?assertEqual('Dictionary', beamtalk_primitive:class_of(#{})),
    ?assertEqual('Dictionary', beamtalk_primitive:class_of(#{a => 1})),
    ?assertEqual('Dictionary', beamtalk_primitive:class_of(#{key => value, x => 42})).

class_of_tuple_test() ->
    ?assertEqual('Tuple', beamtalk_primitive:class_of({})),
    ?assertEqual('Tuple', beamtalk_primitive:class_of({1, 2})),
    ?assertEqual('Tuple', beamtalk_primitive:class_of({a, b, c})),
    %% Non-object tuples
    ?assertEqual('Tuple', beamtalk_primitive:class_of({other_record, data})).

class_of_pid_test() ->
    ?assertEqual('Pid', beamtalk_primitive:class_of(self())),
    Pid = spawn(fun() -> ok end),
    ?assertEqual('Pid', beamtalk_primitive:class_of(Pid)).

class_of_port_test() ->
    {ok, Port} = gen_tcp:listen(0, []),
    ?assertEqual('Port', beamtalk_primitive:class_of(Port)),
    gen_tcp:close(Port).

class_of_reference_test() ->
    Ref = make_ref(),
    ?assertEqual('Reference', beamtalk_primitive:class_of(Ref)).

class_of_beamtalk_object_test() ->
    %% Create a mock beamtalk_object record
    Obj = #beamtalk_object{
        class = 'Counter',
        class_mod = 'counter',
        pid = self()
    },
    ?assertEqual('Counter', beamtalk_primitive:class_of(Obj)).

class_of_unknown_test() ->
    %% Note: Most Erlang types have specific classes
    %% The 'Object' fallback is unlikely to be reached in practice
    %% since atoms are Symbols, so this is just a safety test
    ?assertEqual('Symbol', beamtalk_primitive:class_of(anything_else)).

%%% ============================================================================
%%% send/3 tests
%%% ============================================================================

send_to_beamtalk_object_test() ->
    %% This test requires a real actor, so we'll skip for now
    %% (would need a test actor implementation)
    ok.

send_to_integer_test() ->
    %% Integer dispatch now implemented (BT-166)
    ?assertEqual(50, beamtalk_primitive:send(42, '+', [8])),
    ?assertEqual('Integer', beamtalk_primitive:send(42, 'class', [])).

send_to_string_not_implemented_test() ->
    %% String dispatch not yet implemented (BT-167)
    ?assertError({not_implemented, _}, beamtalk_primitive:send(<<"hello">>, '++', [<<"world">>])).

send_to_float_not_implemented_test() ->
    %% Float dispatch not yet implemented (BT-168)
    ?assertError({not_implemented, _}, beamtalk_primitive:send(3.14, '+', [2.0])).

%%% ============================================================================
%%% responds_to/2 tests
%%% ============================================================================

responds_to_beamtalk_object_test() ->
    %% For now, responds_to returns false for objects without has_method/1
    Obj = #beamtalk_object{
        class = 'Counter',
        class_mod = 'nonexistent_module',
        pid = self()
    },
    ?assertEqual(false, beamtalk_primitive:responds_to(Obj, 'increment')).

responds_to_integer_test_() ->
    {setup, 
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         %% Integer class now implemented (BT-166)
         ?assertEqual(true, beamtalk_primitive:responds_to(42, '+')),
         ?assertEqual(true, beamtalk_primitive:responds_to(42, 'class')),
         ?assertEqual(false, beamtalk_primitive:responds_to(42, 'unknownMethod'))
     end}.

responds_to_string_test() ->
    %% String class not implemented yet (BT-167)
    ?assertEqual(false, beamtalk_primitive:responds_to(<<"hello">>, '++')),
    ?assertEqual(false, beamtalk_primitive:responds_to(<<"hello">>, 'length')).

responds_to_other_primitives_test() ->
    %% Other primitives not implemented yet
    ?assertEqual(false, beamtalk_primitive:responds_to(true, 'not')),
    ?assertEqual(false, beamtalk_primitive:responds_to(3.14, '*')),
    ?assertEqual(false, beamtalk_primitive:responds_to([], 'size')).

%%% ============================================================================
%%% Edge cases and special values
%%% ============================================================================

class_of_special_atoms_test() ->
    %% Ensure true/false/nil are not treated as generic symbols
    ?assertEqual('Boolean', beamtalk_primitive:class_of(true)),
    ?assertEqual('Boolean', beamtalk_primitive:class_of(false)),
    ?assertEqual('UndefinedObject', beamtalk_primitive:class_of(nil)),
    
    %% But other atoms are symbols
    ?assertEqual('Symbol', beamtalk_primitive:class_of(ok)),
    ?assertEqual('Symbol', beamtalk_primitive:class_of(error)).

class_of_empty_collections_test() ->
    ?assertEqual('String', beamtalk_primitive:class_of(<<>>)),
    ?assertEqual('Array', beamtalk_primitive:class_of([])),
    ?assertEqual('Dictionary', beamtalk_primitive:class_of(#{})),
    ?assertEqual('Tuple', beamtalk_primitive:class_of({})).

class_of_nested_structures_test() ->
    ?assertEqual('Array', beamtalk_primitive:class_of([[1, 2], [3, 4]])),
    ?assertEqual('Dictionary', beamtalk_primitive:class_of(#{key => #{nested => value}})),
    ?assertEqual('Tuple', beamtalk_primitive:class_of({{a, b}, {c, d}})).
