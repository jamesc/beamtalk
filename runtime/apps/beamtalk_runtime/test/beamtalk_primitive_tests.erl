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

send_to_string_test() ->
    %% String dispatch now implemented (BT-167)
    ?assertEqual(<<"helloworld">>, beamtalk_primitive:send(<<"hello">>, '++', [<<"world">>])),
    ?assertEqual('String', beamtalk_primitive:send(<<"hello">>, 'class', [])).

send_to_float_test() ->
    %% Float dispatch implemented (BT-277)
    Result = beamtalk_primitive:send(3.14, '+', [2.0]),
    ?assert(is_float(Result)),
    ?assert(abs(Result - 5.14) < 0.0001),  % Floating point tolerance
    ?assertEqual('Float', beamtalk_primitive:send(3.14, 'class', [])).

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

responds_to_string_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         %% String class now implemented (BT-167)
         ?assertEqual(true, beamtalk_primitive:responds_to(<<"hello">>, '++')),
         ?assertEqual(true, beamtalk_primitive:responds_to(<<"hello">>, 'length')),
         ?assertEqual(true, beamtalk_primitive:responds_to(<<"hello">>, 'class')),
         ?assertEqual(false, beamtalk_primitive:responds_to(<<"hello">>, 'unknownMethod'))
     end}.

responds_to_boolean_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         %% Boolean class now implemented (BT-168)
         ?assertEqual(true, beamtalk_primitive:responds_to(true, 'not')),
         ?assertEqual(true, beamtalk_primitive:responds_to(false, 'not')),
         ?assertEqual(true, beamtalk_primitive:responds_to(true, 'ifTrue:')),
         ?assertEqual(false, beamtalk_primitive:responds_to(true, 'unknownMethod'))
     end}.

responds_to_nil_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         %% Nil class now implemented (BT-168)
         ?assertEqual(true, beamtalk_primitive:responds_to(nil, 'isNil')),
         ?assertEqual(true, beamtalk_primitive:responds_to(nil, 'ifNil:')),
         ?assertEqual(false, beamtalk_primitive:responds_to(nil, 'unknownMethod'))
     end}.

responds_to_block_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         %% Block class now implemented (BT-168)
         Block = fun() -> ok end,
         ?assertEqual(true, beamtalk_primitive:responds_to(Block, 'value')),
         ?assertEqual(true, beamtalk_primitive:responds_to(Block, 'arity')),
         ?assertEqual(false, beamtalk_primitive:responds_to(Block, 'unknownMethod'))
     end}.

responds_to_tuple_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         %% Tuple class now implemented (BT-168)
         ?assertEqual(true, beamtalk_primitive:responds_to({a, b}, 'size')),
         ?assertEqual(true, beamtalk_primitive:responds_to({ok, 42}, 'isOk')),
         ?assertEqual(false, beamtalk_primitive:responds_to({a, b}, 'unknownMethod'))
     end}.

responds_to_float_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         ?assertEqual(true, beamtalk_primitive:responds_to(3.14, '+')),
         ?assertEqual(true, beamtalk_primitive:responds_to(3.14, 'class')),
         ?assertEqual(false, beamtalk_primitive:responds_to(3.14, 'unknownMethod'))
     end}.

responds_to_other_primitives_test() ->
    %% Lists support size via beamtalk_list dispatch
    ?assertEqual(true, beamtalk_primitive:responds_to([], 'size')).

%%% ============================================================================
%%% List and Map dispatch routing (BT-296)
%%% ============================================================================

send_list_size_test() ->
    ?assertEqual(0, beamtalk_primitive:send([], 'size', [])),
    ?assertEqual(3, beamtalk_primitive:send([1, 2, 3], 'size', [])).

send_map_size_test() ->
    ?assertEqual(0, beamtalk_primitive:send(#{}, 'size', [])),
    ?assertEqual(2, beamtalk_primitive:send(#{a => 1, b => 2}, 'size', [])).

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

%%% ============================================================================
%%% BT-163: Reflection methods (class, respondsTo)
%%% ============================================================================

%% Test class reflection directly via dispatch
reflection_class_integer_test() ->
    ?assertEqual('Integer', beamtalk_integer:dispatch('class', [], 42)),
    ?assertEqual('Integer', beamtalk_integer:dispatch('class', [], -100)),
    ?assertEqual('Integer', beamtalk_integer:dispatch('class', [], 0)).

reflection_class_string_test() ->
    ?assertEqual('String', beamtalk_string:dispatch('class', [], <<"hello">>)),
    ?assertEqual('String', beamtalk_string:dispatch('class', [], <<>>)).

reflection_class_boolean_test() ->
    ?assertEqual('Boolean', beamtalk_boolean:dispatch('class', [], true)),
    ?assertEqual('Boolean', beamtalk_boolean:dispatch('class', [], false)).

reflection_class_nil_test() ->
    ?assertEqual('UndefinedObject', beamtalk_nil:dispatch('class', [], nil)).

reflection_class_block_test() ->
    ?assertEqual('Block', beamtalk_block:dispatch('class', [], fun() -> ok end)).

reflection_class_tuple_test() ->
    ?assertEqual('Tuple', beamtalk_tuple:dispatch('class', [], {a, b, c})).

%% Test respondsTo reflection directly via dispatch
reflection_responds_to_integer_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         %% True cases - methods that exist
         ?assertEqual(true, beamtalk_integer:dispatch('respondsTo', ['+'], 42)),
         ?assertEqual(true, beamtalk_integer:dispatch('respondsTo', ['class'], 42)),
         ?assertEqual(true, beamtalk_integer:dispatch('respondsTo', ['abs'], 42)),
         ?assertEqual(true, beamtalk_integer:dispatch('respondsTo', ['respondsTo'], 42)),
         
         %% False cases - methods that don't exist
         ?assertEqual(false, beamtalk_integer:dispatch('respondsTo', ['unknownMethod'], 42)),
         ?assertEqual(false, beamtalk_integer:dispatch('respondsTo', ['fooBar'], 42))
     end}.

reflection_responds_to_string_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         %% True cases
         ?assertEqual(true, beamtalk_string:dispatch('respondsTo', ['++'], <<"hi">>)),
         ?assertEqual(true, beamtalk_string:dispatch('respondsTo', ['class'], <<"hi">>)),
         ?assertEqual(true, beamtalk_string:dispatch('respondsTo', ['size'], <<"hi">>)),
         ?assertEqual(true, beamtalk_string:dispatch('respondsTo', ['respondsTo'], <<"hi">>)),
         
         %% False cases
         ?assertEqual(false, beamtalk_string:dispatch('respondsTo', ['unknownMethod'], <<"hi">>)),
         ?assertEqual(false, beamtalk_string:dispatch('respondsTo', ['+'], <<"hi">>))
     end}.

reflection_responds_to_boolean_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         %% True cases
         ?assertEqual(true, beamtalk_boolean:dispatch('respondsTo', ['not'], true)),
         ?assertEqual(true, beamtalk_boolean:dispatch('respondsTo', ['class'], true)),
         ?assertEqual(true, beamtalk_boolean:dispatch('respondsTo', ['ifTrue:'], true)),
         ?assertEqual(true, beamtalk_boolean:dispatch('respondsTo', ['respondsTo'], true)),
         
         %% False cases
         ?assertEqual(false, beamtalk_boolean:dispatch('respondsTo', ['unknownMethod'], true)),
         ?assertEqual(false, beamtalk_boolean:dispatch('respondsTo', ['+'], true))
     end}.

reflection_responds_to_nil_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         %% True cases
         ?assertEqual(true, beamtalk_nil:dispatch('respondsTo', ['isNil'], nil)),
         ?assertEqual(true, beamtalk_nil:dispatch('respondsTo', ['class'], nil)),
         ?assertEqual(true, beamtalk_nil:dispatch('respondsTo', ['ifNil:'], nil)),
         ?assertEqual(true, beamtalk_nil:dispatch('respondsTo', ['respondsTo'], nil)),
         
         %% False cases
         ?assertEqual(false, beamtalk_nil:dispatch('respondsTo', ['unknownMethod'], nil)),
         ?assertEqual(false, beamtalk_nil:dispatch('respondsTo', ['+'], nil))
     end}.

reflection_responds_to_block_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         Block = fun() -> ok end,
         %% True cases
         ?assertEqual(true, beamtalk_block:dispatch('respondsTo', ['value'], Block)),
         ?assertEqual(true, beamtalk_block:dispatch('respondsTo', ['class'], Block)),
         ?assertEqual(true, beamtalk_block:dispatch('respondsTo', ['arity'], Block)),
         ?assertEqual(true, beamtalk_block:dispatch('respondsTo', ['respondsTo'], Block)),
         
         %% False cases
         ?assertEqual(false, beamtalk_block:dispatch('respondsTo', ['unknownMethod'], Block)),
         ?assertEqual(false, beamtalk_block:dispatch('respondsTo', ['+'], Block))
     end}.

reflection_responds_to_tuple_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         Tuple = {a, b, c},
         %% True cases
         ?assertEqual(true, beamtalk_tuple:dispatch('respondsTo', ['size'], Tuple)),
         ?assertEqual(true, beamtalk_tuple:dispatch('respondsTo', ['class'], Tuple)),
         ?assertEqual(true, beamtalk_tuple:dispatch('respondsTo', ['at:'], Tuple)),
         ?assertEqual(true, beamtalk_tuple:dispatch('respondsTo', ['respondsTo'], Tuple)),
         
         %% False cases
         ?assertEqual(false, beamtalk_tuple:dispatch('respondsTo', ['unknownMethod'], Tuple)),
         ?assertEqual(false, beamtalk_tuple:dispatch('respondsTo', ['+'], Tuple))
     end}.

reflection_responds_to_float_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         %% True cases
         ?assertEqual(true, beamtalk_float:dispatch('respondsTo', ['+'], 3.14)),
         ?assertEqual(true, beamtalk_float:dispatch('respondsTo', ['class'], 3.14)),
         ?assertEqual(true, beamtalk_float:dispatch('respondsTo', ['abs'], 3.14)),
         ?assertEqual(true, beamtalk_float:dispatch('respondsTo', ['respondsTo'], 3.14)),
         
         %% False cases
         ?assertEqual(false, beamtalk_float:dispatch('respondsTo', ['unknownMethod'], 3.14)),
         ?assertEqual(false, beamtalk_float:dispatch('respondsTo', ['fooBar'], 3.14))
     end}.

%%% ============================================================================
%%% perform: dynamic message send tests (BT-165)
%%% ============================================================================

perform_on_integer_test() ->
    %% Test perform: on integer primitive
    %% 42 perform: #'+' withArgs: [8]  => 42 + 8  => 50
    Result = beamtalk_integer:dispatch('perform:withArgs:', ['+', [8]], 42),
    ?assertEqual(50, Result).

perform_on_string_test() ->
    %% Test perform: on string primitive
    %% "hello" perform: #'++' withArgs: [" world"]  => "hello" ++ " world"
    Result = beamtalk_string:dispatch('perform:withArgs:', ['++', [<<" world">>]], <<"hello">>),
    ?assertEqual(<<"hello world">>, Result).

perform_on_boolean_test() ->
    %% Test perform: on boolean primitive
    %% true perform: #'ifTrue:ifFalse:' withArgs: [yes, no]  => yes
    YesBlock = fun() -> yes end,
    NoBlock = fun() -> no end,
    Result = beamtalk_boolean:dispatch('perform:withArgs:', ['ifTrue:ifFalse:', [YesBlock, NoBlock]], true),
    ?assertEqual(yes, Result).

perform_with_unary_message_on_integer_test() ->
    %% Test perform: with unary message (no args)
    %% -5 perform: #abs  => 5
    Result = beamtalk_integer:dispatch('perform', ['abs'], -5),
    ?assertEqual(5, Result).

perform_withArgs_invalid_args_type_on_primitive_test() ->
    %% Test perform:withArgs: with non-list ArgList on primitive
    %% Should raise type_error, not does_not_understand
    %% This ensures consistency with actor behavior
    ?assertError(#beamtalk_error{kind = type_error, class = 'Integer', selector = 'perform:withArgs:'}, 
                 beamtalk_integer:dispatch('perform:withArgs:', ['+', 42], 10)).
