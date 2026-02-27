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
    ?assertEqual('True', beamtalk_primitive:class_of(true)),
    ?assertEqual('False', beamtalk_primitive:class_of(false)).

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

class_of_list_test() ->
    ?assertEqual('List', beamtalk_primitive:class_of([])),
    ?assertEqual('List', beamtalk_primitive:class_of([1, 2, 3])),
    ?assertEqual('List', beamtalk_primitive:class_of([a, b, c])),
    ?assertEqual('List', beamtalk_primitive:class_of([[1], [2], [3]])).

class_of_dictionary_test() ->
    ?assertEqual('Dictionary', beamtalk_primitive:class_of(#{})),
    ?assertEqual('Dictionary', beamtalk_primitive:class_of(#{a => 1})),
    ?assertEqual('Dictionary', beamtalk_primitive:class_of(#{key => value, x => 42})).

%% BT-324: Dictionary with user '__class__' key must NOT be misclassified
class_of_dictionary_with_old_class_key_collision_test() ->
    %% A user Dictionary that happens to have '__class__' key (old tag)
    %% must still be treated as Dictionary, not as a tagged map
    Dict = #{'__class__' => 'Integer', value => 42},
    ?assertEqual('Dictionary', beamtalk_primitive:class_of(Dict)).

class_of_dictionary_with_class_key_string_collision_test() ->
    %% Non-atom __class__ value was already handled, verify it still works
    Dict = #{'__class__' => <<"Integer">>, value => 42},
    ?assertEqual('Dictionary', beamtalk_primitive:class_of(Dict)).

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
    % Floating point tolerance
    ?assert(abs(Result - 5.14) < 0.0001),
    ?assertEqual('Float', beamtalk_primitive:send(3.14, 'class', [])).

%%% ============================================================================
%%% responds_to/2 tests
%%% ============================================================================

responds_to_beamtalk_object_test() ->
    %% ADR 0032 Phase 3: responds_to delegates to beamtalk_dispatch:responds_to/2
    %% which walks the full class hierarchy. Returns false for unknown class.
    Obj = #beamtalk_object{
        class = 'UnregisteredClass',
        class_mod = 'nonexistent_module',
        pid = self()
    },
    ?assertEqual(false, beamtalk_primitive:responds_to(Obj, 'increment')).

%% BT-735: responds_to for actor instances now delegates to beamtalk_dispatch:responds_to
%% which walks the full hierarchy, so inherited methods are detected correctly.
responds_to_actor_inherited_method_test_() ->
    {setup,
        fun() ->
            application:ensure_all_started(beamtalk_runtime),
            beamtalk_stdlib:init(),
            ok
        end,
        fun(_) -> ok end, fun() ->
            case beamtalk_class_registry:whereis_class('Counter') of
                undefined ->
                    %% Counter not loaded — explicitly skip test
                    {skip, "Counter class not loaded after beamtalk_stdlib:init()"};
                CounterPid ->
                    CounterObj = #beamtalk_object{
                        class = 'Counter',
                        class_mod = 'bt@counter',
                        pid = CounterPid
                    },
                    %% Local method — defined directly in Counter
                    ?assertEqual(true, beamtalk_primitive:responds_to(CounterObj, 'increment')),
                    %% Inherited method — defined in Object/ProtoObject, not Counter
                    ?assertEqual(true, beamtalk_primitive:responds_to(CounterObj, 'class')),
                    %% Non-existent method
                    ?assertEqual(false, beamtalk_primitive:responds_to(CounterObj, 'noSuchMethod'))
            end
        end}.

responds_to_integer_test_() ->
    {setup, fun() -> beamtalk_extensions:init() end, fun(_) -> ok end, fun() ->
        %% Integer class now implemented (BT-166)
        ?assertEqual(true, beamtalk_primitive:responds_to(42, '+')),
        ?assertEqual(true, beamtalk_primitive:responds_to(42, 'class')),
        ?assertEqual(false, beamtalk_primitive:responds_to(42, 'unknownMethod'))
    end}.

responds_to_string_test_() ->
    {setup, fun() -> beamtalk_extensions:init() end, fun(_) -> ok end, fun() ->
        %% String class now implemented (BT-167)
        ?assertEqual(true, beamtalk_primitive:responds_to(<<"hello">>, '++')),
        ?assertEqual(true, beamtalk_primitive:responds_to(<<"hello">>, 'length')),
        ?assertEqual(true, beamtalk_primitive:responds_to(<<"hello">>, 'class')),
        ?assertEqual(false, beamtalk_primitive:responds_to(<<"hello">>, 'unknownMethod'))
    end}.

responds_to_boolean_test_() ->
    {setup, fun() -> beamtalk_extensions:init() end, fun(_) -> ok end, fun() ->
        %% Boolean class now implemented (BT-168)
        ?assertEqual(true, beamtalk_primitive:responds_to(true, 'not')),
        ?assertEqual(true, beamtalk_primitive:responds_to(false, 'not')),
        ?assertEqual(true, beamtalk_primitive:responds_to(true, 'ifTrue:')),
        ?assertEqual(false, beamtalk_primitive:responds_to(true, 'unknownMethod'))
    end}.

responds_to_nil_test_() ->
    {setup, fun() -> beamtalk_extensions:init() end, fun(_) -> ok end, fun() ->
        %% Nil class now implemented (BT-168)
        ?assertEqual(true, beamtalk_primitive:responds_to(nil, 'isNil')),
        ?assertEqual(true, beamtalk_primitive:responds_to(nil, 'ifNil:')),
        ?assertEqual(false, beamtalk_primitive:responds_to(nil, 'unknownMethod'))
    end}.

responds_to_block_test_() ->
    {setup, fun() -> beamtalk_extensions:init() end, fun(_) -> ok end, fun() ->
        %% Block class now implemented (BT-168)
        Block = fun() -> ok end,
        ?assertEqual(true, beamtalk_primitive:responds_to(Block, 'value')),
        ?assertEqual(true, beamtalk_primitive:responds_to(Block, 'arity')),
        ?assertEqual(false, beamtalk_primitive:responds_to(Block, 'unknownMethod'))
    end}.

responds_to_tuple_test_() ->
    {setup, fun() -> beamtalk_extensions:init() end, fun(_) -> ok end, fun() ->
        %% Tuple class now implemented (BT-168)
        ?assertEqual(true, beamtalk_primitive:responds_to({a, b}, 'size')),
        ?assertEqual(true, beamtalk_primitive:responds_to({ok, 42}, 'isOk')),
        ?assertEqual(false, beamtalk_primitive:responds_to({a, b}, 'unknownMethod'))
    end}.

responds_to_float_test_() ->
    {setup, fun() -> beamtalk_extensions:init() end, fun(_) -> ok end, fun() ->
        ?assertEqual(true, beamtalk_primitive:responds_to(3.14, '+')),
        ?assertEqual(true, beamtalk_primitive:responds_to(3.14, 'class')),
        ?assertEqual(false, beamtalk_primitive:responds_to(3.14, 'unknownMethod'))
    end}.

responds_to_other_primitives_test() ->
    %% Lists support size via bt@stdlib@list dispatch
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
    ?assertEqual('True', beamtalk_primitive:class_of(true)),
    ?assertEqual('False', beamtalk_primitive:class_of(false)),
    ?assertEqual('UndefinedObject', beamtalk_primitive:class_of(nil)),

    %% But other atoms are symbols
    ?assertEqual('Symbol', beamtalk_primitive:class_of(ok)),
    ?assertEqual('Symbol', beamtalk_primitive:class_of(error)).

class_of_empty_collections_test() ->
    ?assertEqual('String', beamtalk_primitive:class_of(<<>>)),
    ?assertEqual('List', beamtalk_primitive:class_of([])),
    ?assertEqual('Dictionary', beamtalk_primitive:class_of(#{})),
    ?assertEqual('Tuple', beamtalk_primitive:class_of({})).

class_of_nested_structures_test() ->
    ?assertEqual('List', beamtalk_primitive:class_of([[1, 2], [3, 4]])),
    ?assertEqual('Dictionary', beamtalk_primitive:class_of(#{key => #{nested => value}})),
    ?assertEqual('Tuple', beamtalk_primitive:class_of({{a, b}, {c, d}})).

%%% ============================================================================
%%% BT-163: Reflection methods (class, respondsTo)
%%% ============================================================================

%% Test class reflection directly via dispatch
reflection_class_integer_test() ->
    ?assertEqual('Integer', 'bt@stdlib@integer':dispatch('class', [], 42)),
    ?assertEqual('Integer', 'bt@stdlib@integer':dispatch('class', [], -100)),
    ?assertEqual('Integer', 'bt@stdlib@integer':dispatch('class', [], 0)).

reflection_class_string_test() ->
    ?assertEqual('String', 'bt@stdlib@string':dispatch('class', [], <<"hello">>)),
    ?assertEqual('String', 'bt@stdlib@string':dispatch('class', [], <<>>)).

reflection_class_boolean_test() ->
    ?assertEqual('True', 'bt@stdlib@true':dispatch('class', [], true)),
    ?assertEqual('False', 'bt@stdlib@false':dispatch('class', [], false)).

reflection_class_nil_test() ->
    ?assertEqual('UndefinedObject', 'bt@stdlib@undefined_object':dispatch('class', [], nil)).

reflection_class_block_test() ->
    ?assertEqual('Block', 'bt@stdlib@block':dispatch('class', [], fun() -> ok end)).

reflection_class_tuple_test() ->
    ?assertEqual('Tuple', 'bt@stdlib@tuple':dispatch('class', [], {a, b, c})).

%% Test respondsTo reflection directly via dispatch
reflection_responds_to_integer_test_() ->
    {setup, fun() -> beamtalk_extensions:init() end, fun(_) -> ok end, fun() ->
        %% True cases - methods that exist
        ?assertEqual(true, 'bt@stdlib@integer':dispatch('respondsTo:', ['+'], 42)),
        ?assertEqual(true, 'bt@stdlib@integer':dispatch('respondsTo:', ['class'], 42)),
        ?assertEqual(true, 'bt@stdlib@integer':dispatch('respondsTo:', ['abs'], 42)),
        ?assertEqual(true, 'bt@stdlib@integer':dispatch('respondsTo:', ['respondsTo:'], 42)),

        %% False cases - methods that don't exist
        ?assertEqual(false, 'bt@stdlib@integer':dispatch('respondsTo:', ['unknownMethod'], 42)),
        ?assertEqual(false, 'bt@stdlib@integer':dispatch('respondsTo:', ['fooBar'], 42))
    end}.

reflection_responds_to_string_test_() ->
    {setup, fun() -> beamtalk_extensions:init() end, fun(_) -> ok end, fun() ->
        %% True cases
        ?assertEqual(true, 'bt@stdlib@string':dispatch('respondsTo:', ['++'], <<"hi">>)),
        ?assertEqual(true, 'bt@stdlib@string':dispatch('respondsTo:', ['class'], <<"hi">>)),
        ?assertEqual(true, 'bt@stdlib@string':dispatch('respondsTo:', ['size'], <<"hi">>)),
        ?assertEqual(true, 'bt@stdlib@string':dispatch('respondsTo:', ['respondsTo:'], <<"hi">>)),

        %% False cases
        ?assertEqual(
            false, 'bt@stdlib@string':dispatch('respondsTo:', ['unknownMethod'], <<"hi">>)
        ),
        ?assertEqual(false, 'bt@stdlib@string':dispatch('respondsTo:', ['+'], <<"hi">>))
    end}.

reflection_responds_to_boolean_test_() ->
    {setup, fun() -> beamtalk_extensions:init() end, fun(_) -> ok end, fun() ->
        %% True cases
        ?assertEqual(true, 'bt@stdlib@true':dispatch('respondsTo:', ['not'], true)),
        ?assertEqual(true, 'bt@stdlib@true':dispatch('respondsTo:', ['class'], true)),
        ?assertEqual(true, 'bt@stdlib@true':dispatch('respondsTo:', ['ifTrue:'], true)),
        ?assertEqual(true, 'bt@stdlib@true':dispatch('respondsTo:', ['respondsTo:'], true)),

        %% False cases
        ?assertEqual(false, 'bt@stdlib@true':dispatch('respondsTo:', ['unknownMethod'], true)),
        ?assertEqual(false, 'bt@stdlib@true':dispatch('respondsTo:', ['+'], true))
    end}.

reflection_responds_to_nil_test_() ->
    {setup, fun() -> beamtalk_extensions:init() end, fun(_) -> ok end, fun() ->
        %% True cases
        ?assertEqual(true, 'bt@stdlib@undefined_object':dispatch('respondsTo:', ['isNil'], nil)),
        ?assertEqual(true, 'bt@stdlib@undefined_object':dispatch('respondsTo:', ['class'], nil)),
        ?assertEqual(true, 'bt@stdlib@undefined_object':dispatch('respondsTo:', ['ifNil:'], nil)),
        ?assertEqual(
            true, 'bt@stdlib@undefined_object':dispatch('respondsTo:', ['respondsTo:'], nil)
        ),

        %% False cases
        ?assertEqual(
            false, 'bt@stdlib@undefined_object':dispatch('respondsTo:', ['unknownMethod'], nil)
        ),
        ?assertEqual(false, 'bt@stdlib@undefined_object':dispatch('respondsTo:', ['+'], nil))
    end}.

reflection_responds_to_block_test_() ->
    {setup, fun() -> beamtalk_extensions:init() end, fun(_) -> ok end, fun() ->
        Block = fun() -> ok end,
        %% True cases
        ?assertEqual(true, 'bt@stdlib@block':dispatch('respondsTo:', ['value'], Block)),
        ?assertEqual(true, 'bt@stdlib@block':dispatch('respondsTo:', ['class'], Block)),
        ?assertEqual(true, 'bt@stdlib@block':dispatch('respondsTo:', ['arity'], Block)),
        ?assertEqual(true, 'bt@stdlib@block':dispatch('respondsTo:', ['respondsTo:'], Block)),

        %% False cases
        ?assertEqual(false, 'bt@stdlib@block':dispatch('respondsTo:', ['unknownMethod'], Block)),
        ?assertEqual(false, 'bt@stdlib@block':dispatch('respondsTo:', ['+'], Block))
    end}.

reflection_responds_to_tuple_test_() ->
    {setup, fun() -> beamtalk_extensions:init() end, fun(_) -> ok end, fun() ->
        Tuple = {a, b, c},
        %% True cases
        ?assertEqual(true, 'bt@stdlib@tuple':dispatch('respondsTo:', ['size'], Tuple)),
        ?assertEqual(true, 'bt@stdlib@tuple':dispatch('respondsTo:', ['class'], Tuple)),
        ?assertEqual(true, 'bt@stdlib@tuple':dispatch('respondsTo:', ['at:'], Tuple)),
        ?assertEqual(true, 'bt@stdlib@tuple':dispatch('respondsTo:', ['respondsTo:'], Tuple)),

        %% False cases
        ?assertEqual(false, 'bt@stdlib@tuple':dispatch('respondsTo:', ['unknownMethod'], Tuple)),
        ?assertEqual(false, 'bt@stdlib@tuple':dispatch('respondsTo:', ['+'], Tuple))
    end}.

reflection_responds_to_float_test_() ->
    {setup, fun() -> beamtalk_extensions:init() end, fun(_) -> ok end, fun() ->
        %% True cases
        ?assertEqual(true, 'bt@stdlib@float':dispatch('respondsTo:', ['+'], 3.14)),
        ?assertEqual(true, 'bt@stdlib@float':dispatch('respondsTo:', ['class'], 3.14)),
        ?assertEqual(true, 'bt@stdlib@float':dispatch('respondsTo:', ['abs'], 3.14)),
        ?assertEqual(true, 'bt@stdlib@float':dispatch('respondsTo:', ['respondsTo:'], 3.14)),

        %% False cases
        ?assertEqual(false, 'bt@stdlib@float':dispatch('respondsTo:', ['unknownMethod'], 3.14)),
        ?assertEqual(false, 'bt@stdlib@float':dispatch('respondsTo:', ['fooBar'], 3.14))
    end}.

%%% ============================================================================
%%% perform: dynamic message send tests (BT-165)
%%% ============================================================================

perform_on_integer_test() ->
    %% Test perform: on integer primitive
    %% 42 perform: #'+' withArgs: [8]  => 42 + 8  => 50
    Result = 'bt@stdlib@integer':dispatch('perform:withArguments:', ['+', [8]], 42),
    ?assertEqual(50, Result).

perform_on_string_test() ->
    %% Test perform: on string primitive
    %% "hello" perform: #'++' withArgs: [" world"]  => "hello" ++ " world"
    Result = 'bt@stdlib@string':dispatch(
        'perform:withArguments:', ['++', [<<" world">>]], <<"hello">>
    ),
    ?assertEqual(<<"hello world">>, Result).

perform_on_boolean_test() ->
    %% Test perform: on boolean primitive
    %% true perform: #'ifTrue:ifFalse:' withArgs: [yes, no]  => yes
    YesBlock = fun() -> yes end,
    NoBlock = fun() -> no end,
    Result = 'bt@stdlib@true':dispatch(
        'perform:withArguments:', ['ifTrue:ifFalse:', [YesBlock, NoBlock]], true
    ),
    ?assertEqual(yes, Result).

perform_with_unary_message_on_integer_test() ->
    %% Test perform: with unary message (no args)
    %% -5 perform: #abs  => 5
    Result = 'bt@stdlib@integer':dispatch('perform:', ['abs'], -5),
    ?assertEqual(5, Result).

perform_withArgs_invalid_args_type_on_primitive_test() ->
    %% Test perform:withArguments: with non-list ArgList on primitive
    %% Generated dispatch calls hd/tl which raises badarg for non-list args
    ?assertError(
        badarg,
        'bt@stdlib@integer':dispatch('perform:withArguments:', ['+', 42], 10)
    ).

%%% ============================================================================
%%% Value Type Dispatch Tests (BT-354)
%%% ============================================================================

%% --- class_name_to_module/1 tests (ADR 0016: bt@ prefix) ---

class_name_to_module_simple_test() ->
    ?assertEqual('bt@point', beamtalk_primitive:class_name_to_module('Point')).

class_name_to_module_multi_word_test() ->
    ?assertEqual('bt@my_counter', beamtalk_primitive:class_name_to_module('MyCounter')).

class_name_to_module_three_words_test() ->
    ?assertEqual('bt@my_counter_actor', beamtalk_primitive:class_name_to_module('MyCounterActor')).

class_name_to_module_single_char_test() ->
    ?assertEqual('bt@x', beamtalk_primitive:class_name_to_module('X')).

class_name_to_module_acronym_test() ->
    %% Consecutive capitals are not separated (matches Rust to_module_name)
    ?assertEqual('bt@httprouter', beamtalk_primitive:class_name_to_module('HTTPRouter')).

%% --- Value type send/3 routing ---

value_type_send_routes_to_class_module_test() ->
    %% Create a mock value type module dynamically
    MockModule = create_mock_value_type_module('bt@mock_vt', 'MockVT', [
        {'getX', x}
    ]),
    Self = #{'$beamtalk_class' => 'MockVT', x => 42},
    try
        Result = beamtalk_primitive:send(Self, 'getX', []),
        ?assertEqual(42, Result)
    after
        code:purge(MockModule),
        code:delete(MockModule)
    end.

value_type_send_falls_back_to_object_class_test() ->
    %% Value type instances support inherited 'class' method
    Self = #{'$beamtalk_class' => 'MockVTEmpty'},
    create_mock_value_type_module('bt@mock_vtempty', 'MockVTEmpty', []),
    try
        Result = beamtalk_primitive:send(Self, class, []),
        ?assertEqual('MockVTEmpty', Result)
    after
        code:purge('bt@mock_vtempty'),
        code:delete('bt@mock_vtempty')
    end.

value_type_send_does_not_understand_test() ->
    %% Unknown method raises does_not_understand
    Self = #{'$beamtalk_class' => 'MockVTErr'},
    create_mock_value_type_module('bt@mock_vterr', 'MockVTErr', []),
    try
        ?assertError(
            #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = does_not_understand,
                    class = 'MockVTErr',
                    selector = 'nonexistent'
                }
            },
            beamtalk_primitive:send(Self, 'nonexistent', [])
        )
    after
        code:purge('bt@mock_vterr'),
        code:delete('bt@mock_vterr')
    end.

plain_map_still_routes_to_dictionary_test() ->
    %% Plain maps without $beamtalk_class still route to bt@stdlib@dictionary
    Self = #{a => 1, b => 2},
    Result = beamtalk_primitive:send(Self, 'size', []),
    ?assertEqual(2, Result).

%% BT-324: Dictionary with user '__class__' key dispatches as Dictionary
dictionary_with_old_class_key_dispatches_as_dictionary_test() ->
    %% A user Dictionary with '__class__' (old tag) must route to bt@stdlib@dictionary,
    %% not to whatever class name the key contains
    Dict = #{'__class__' => 'Integer', value => 42},
    ?assertEqual(2, beamtalk_primitive:send(Dict, 'size', [])).

dictionary_with_old_class_key_responds_to_dictionary_methods_test() ->
    %% responds_to should check Dictionary methods, not the colliding class
    Dict = #{'__class__' => 'Integer', value => 42},
    ?assert(beamtalk_primitive:responds_to(Dict, 'size')),
    ?assert(beamtalk_primitive:responds_to(Dict, 'at:')),
    ?assertNot(beamtalk_primitive:responds_to(Dict, '+')).

%% --- Value type responds_to/2 ---

value_type_responds_to_class_method_test() ->
    %% Value type instances respond to class module methods
    %% Class 'MockVtRt' maps to module 'bt@mock_vt_rt' via class_name_to_module
    create_mock_value_type_module('bt@mock_vt_rt', 'MockVtRt', [
        {'getX', x}
    ]),
    Self = #{'$beamtalk_class' => 'MockVtRt', x => 0},
    try
        ?assert(beamtalk_primitive:responds_to(Self, 'getX')),
        ?assertNot(beamtalk_primitive:responds_to(Self, 'nonexistent'))
    after
        code:purge('bt@mock_vt_rt'),
        code:delete('bt@mock_vt_rt')
    end.

value_type_responds_to_object_methods_test() ->
    %% Value type instances respond to inherited Object methods
    Self = #{'$beamtalk_class' => 'MockVTObj'},
    create_mock_value_type_module('bt@mock_vtobj', 'MockVTObj', []),
    try
        ?assert(beamtalk_primitive:responds_to(Self, class)),
        ?assert(beamtalk_primitive:responds_to(Self, 'printString'))
    after
        code:purge('bt@mock_vtobj'),
        code:delete('bt@mock_vtobj')
    end.

%%% ============================================================================
%%% BT-359: fieldAt: / fieldAt:put: on value types
%%% ============================================================================

value_type_inst_var_at_put_raises_immutable_value_test() ->
    %% fieldAt:put: on a value type should raise immutable_value
    Self = #{'$beamtalk_class' => 'MockVtIvar', x => 42},
    create_mock_value_type_module('bt@mock_vt_ivar', 'MockVtIvar', []),
    try
        ?assertError(
            #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = immutable_value,
                    class = 'MockVtIvar',
                    selector = 'fieldAt:put:'
                }
            },
            beamtalk_primitive:send(Self, 'fieldAt:put:', [x, 99])
        )
    after
        code:purge('bt@mock_vt_ivar'),
        code:delete('bt@mock_vt_ivar')
    end.

value_type_inst_var_at_reads_slot_test() ->
    %% BT-924: fieldAt: on a user-defined value type reads from the underlying map.
    %% Value objects store their slots as map keys, so reflection is read-only.
    Self = #{'$beamtalk_class' => 'MockVtIvar2', x => 42},
    create_mock_value_type_module('bt@mock_vt_ivar2', 'MockVtIvar2', []),
    try
        ?assertEqual(42, beamtalk_primitive:send(Self, 'fieldAt:', [x])),
        %% Non-existent field returns nil (Smalltalk-80 semantics)
        ?assertEqual(nil, beamtalk_primitive:send(Self, 'fieldAt:', [nonexistent]))
    after
        code:purge('bt@mock_vt_ivar2'),
        code:delete('bt@mock_vt_ivar2')
    end.

%%% ============================================================================
%%% Test Helpers
%%% ============================================================================

%% @doc Create a mock value type module with field accessor methods.
%% FieldMethods: [{MethodName, FieldAtom}] - generates MethodName(Self) -> maps:get(Field, Self)
%% Pass [] for a module with no instance methods.
create_mock_value_type_module(ModuleName, _ClassName, FieldMethods) ->
    ModuleForm = {attribute, 1, module, ModuleName},
    ExportForm = {attribute, 1, export, [{Name, 1} || {Name, _} <- FieldMethods]},
    FunForms = [maps_get_form(Name, Field) || {Name, Field} <- FieldMethods],
    AllForms = [ModuleForm, ExportForm | FunForms],
    {ok, ModuleName, Binary} = compile:forms(AllForms),
    code:load_binary(ModuleName, atom_to_list(ModuleName) ++ ".erl", Binary),
    ModuleName.

maps_get_form(Name, Field) ->
    {function, 1, Name, 1, [
        {clause, 1, [{var, 1, 'Self'}], [], [
            {call, 1, {remote, 1, {atom, 1, maps}, {atom, 1, get}}, [
                {atom, 1, Field}, {var, 1, 'Self'}
            ]}
        ]}
    ]}.

%%% ============================================================================
%%% print_string/1 Tests — Complex Types
%%% ============================================================================

print_string_integer_test() ->
    ?assertEqual(<<"42">>, beamtalk_primitive:print_string(42)),
    ?assertEqual(<<"-7">>, beamtalk_primitive:print_string(-7)).

print_string_float_test() ->
    Result = beamtalk_primitive:print_string(3.14),
    ?assert(is_binary(Result)).

print_string_string_test() ->
    ?assertEqual(<<"\"hello\"">>, beamtalk_primitive:print_string(<<"hello">>)).

print_string_boolean_test() ->
    ?assertEqual(<<"true">>, beamtalk_primitive:print_string(true)),
    ?assertEqual(<<"false">>, beamtalk_primitive:print_string(false)).

print_string_nil_test() ->
    ?assertEqual(<<"nil">>, beamtalk_primitive:print_string(nil)).

print_string_metaclass_test() ->
    %% ADR 0036 (BT-802): 'Metaclass' atom is no longer a sentinel with special print_string.
    %% It prints as a Symbol (atom) with '#' prefix, like any other atom.
    ?assertEqual(<<"#Metaclass">>, beamtalk_primitive:print_string('Metaclass')).

print_string_symbol_test() ->
    ?assertEqual(<<"#hello">>, beamtalk_primitive:print_string(hello)).

print_string_list_test() ->
    Result = beamtalk_primitive:print_string([1, 2, 3]),
    ?assertEqual(<<"#(1, 2, 3)">>, Result).

print_string_empty_list_test() ->
    ?assertEqual(<<"#()">>, beamtalk_primitive:print_string([])).

print_string_tuple_test() ->
    Result = beamtalk_primitive:print_string({1, <<"two">>, 3}),
    ?assertEqual(<<"{1, \"two\", 3}">>, Result).

print_string_empty_tuple_test() ->
    ?assertEqual(<<"{}">>, beamtalk_primitive:print_string({})).

print_string_beamtalk_error_test() ->
    Error = #beamtalk_error{
        kind = does_not_understand,
        class = 'Integer',
        selector = 'foo',
        message = <<"Integer does not understand 'foo'">>
    },
    Result = beamtalk_primitive:print_string(Error),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 0).

print_string_set_test() ->
    Set = #{'$beamtalk_class' => 'Set', elements => [1, 2, 3]},
    Result = beamtalk_primitive:print_string(Set),
    ?assertEqual(<<"Set(1, 2, 3)">>, Result).

print_string_plain_map_test_() ->
    {setup,
        fun() ->
            case whereis(pg) of
                undefined -> pg:start_link();
                _ -> ok
            end,
            beamtalk_extensions:init(),
            {ok, _} = beamtalk_bootstrap:start_link(),
            beamtalk_stdlib:init(),
            ok
        end,
        fun(_) -> ok end, fun() ->
            %% Plain map (Dictionary) — should use beamtalk_map_ops format
            Result = beamtalk_primitive:print_string(#{a => 1}),
            ?assert(is_binary(Result))
        end}.

print_string_catchall_test() ->
    %% Pid/reference should use the catch-all io_lib:format path
    Result = beamtalk_primitive:print_string(self()),
    ?assert(is_binary(Result)).

%%% ============================================================================
%%% class_of_object/1 Tests
%%% ============================================================================

class_of_object_metaclass_sentinel_test() ->
    %% ADR 0036 (BT-802): 'Metaclass' atom is no longer a sentinel.
    %% class_of_object('Metaclass') treats it as a Symbol (atom), returning its class object.
    Result = beamtalk_primitive:class_of_object('Metaclass'),
    ?assertMatch({beamtalk_object, _, _, _}, Result).

class_of_object_primitive_test_() ->
    {setup,
        fun() ->
            case whereis(pg) of
                undefined -> pg:start_link();
                _ -> ok
            end,
            beamtalk_extensions:init(),
            {ok, _} = beamtalk_bootstrap:start_link(),
            beamtalk_stdlib:init(),
            ok
        end,
        fun(_) -> ok end, [
            {"class_of_object for integer returns class object", fun() ->
                Result = beamtalk_primitive:class_of_object(42),
                ?assertMatch({beamtalk_object, _, _, _}, Result)
            end},
            {"class_of_object for string returns class object", fun() ->
                Result = beamtalk_primitive:class_of_object(<<"hello">>),
                ?assertMatch({beamtalk_object, _, _, _}, Result)
            end},
            {"class_of_object for class object returns real metaclass object", fun() ->
                IntegerPid = beamtalk_class_registry:whereis_class('Integer'),
                %% Class objects have class field ending in " class"
                ClassObj = #beamtalk_object{
                    class = 'Integer class', class_mod = beamtalk_object_class, pid = IntegerPid
                },
                Result = beamtalk_primitive:class_of_object(ClassObj),
                %% ADR 0036 (BT-802): Returns real metaclass object (not sentinel atom).
                ?assertMatch(
                    {beamtalk_object, 'Metaclass', beamtalk_metaclass_bt, IntegerPid}, Result
                )
            end}
        ]}.

%%% ============================================================================
%%% class_of_object_by_name/1 Tests
%%% ============================================================================

class_of_object_by_name_test_() ->
    {setup,
        fun() ->
            case whereis(pg) of
                undefined -> pg:start_link();
                _ -> ok
            end,
            beamtalk_extensions:init(),
            {ok, _} = beamtalk_bootstrap:start_link(),
            beamtalk_stdlib:init(),
            ok
        end,
        fun(_) -> ok end, [
            {"known class returns class object", fun() ->
                Result = beamtalk_primitive:class_of_object_by_name('Integer'),
                ?assertMatch({beamtalk_object, _, _, _}, Result)
            end},
            {"unknown class returns atom", fun() ->
                Result = beamtalk_primitive:class_of_object_by_name('NonExistentClass'),
                ?assertEqual('NonExistentClass', Result)
            end}
        ]}.

%%% ============================================================================
%%% camel_to_snake/1 Tests (via class_name_to_module)
%%% ============================================================================

class_name_to_module_test() ->
    ?assertEqual('bt@counter', beamtalk_primitive:class_name_to_module('Counter')),
    ?assertEqual('bt@my_class', beamtalk_primitive:class_name_to_module('MyClass')),
    %% Consecutive uppercase: only insert _ when transitioning from lowercase to uppercase
    ?assertEqual('bt@abc', beamtalk_primitive:class_name_to_module('ABC')),
    ?assertEqual('bt@htmlparser', beamtalk_primitive:class_name_to_module('HTMLParser')).
