%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Unit tests for beamtalk_stack_frame module (BT-624).
%%
%% Tests wrap/1, dispatch/3, has_method/1, and internal helpers.
-module(beamtalk_stack_frame_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ===================================================================
%%% wrap/1 tests
%%% ===================================================================

wrap_valid_stacktrace_test() ->
    Stacktrace = [
        {some_module, some_fun, 2, [{file, "test.erl"}, {line, 42}]},
        {other_mod, other_fun, [arg1, arg2], [{file, "other.erl"}, {line, 10}]}
    ],
    Result = beamtalk_stack_frame:wrap(Stacktrace),
    ?assertEqual(2, length(Result)),
    [Frame1, Frame2] = Result,
    ?assertMatch(#{'$beamtalk_class' := 'StackFrame'}, Frame1),
    ?assertMatch(#{'$beamtalk_class' := 'StackFrame'}, Frame2).

wrap_empty_list_test() ->
    ?assertEqual([], beamtalk_stack_frame:wrap([])).

wrap_non_list_returns_empty_test() ->
    ?assertEqual([], beamtalk_stack_frame:wrap(not_a_list)).

wrap_frame_with_arity_test() ->
    Stacktrace = [{mod, fun1, 3, [{file, "f.erl"}, {line, 1}]}],
    [Frame] = beamtalk_stack_frame:wrap(Stacktrace),
    ?assertEqual(3, maps:get(arity, Frame)).

wrap_frame_with_args_list_test() ->
    Stacktrace = [{mod, fun1, [a, b, c], [{file, "f.erl"}, {line, 1}]}],
    [Frame] = beamtalk_stack_frame:wrap(Stacktrace),
    ?assertEqual(3, maps:get(arity, Frame)).

wrap_frame_no_location_test() ->
    Stacktrace = [{mod, fun1, 2, []}],
    [Frame] = beamtalk_stack_frame:wrap(Stacktrace),
    ?assertEqual(nil, maps:get(file, Frame)),
    ?assertEqual(nil, maps:get(line, Frame)).

wrap_frame_file_becomes_binary_test() ->
    Stacktrace = [{mod, fun1, 0, [{file, "src/test.erl"}, {line, 5}]}],
    [Frame] = beamtalk_stack_frame:wrap(Stacktrace),
    ?assertEqual(<<"src/test.erl">>, maps:get(file, Frame)),
    ?assertEqual(5, maps:get(line, Frame)).

wrap_malformed_frame_test() ->
    Stacktrace = [not_a_tuple],
    [Frame] = beamtalk_stack_frame:wrap(Stacktrace),
    ?assertEqual(undefined, maps:get(module, Frame)),
    ?assertEqual(undefined, maps:get(function, Frame)),
    ?assertEqual(0, maps:get(arity, Frame)),
    ?assertEqual(nil, maps:get(file, Frame)),
    ?assertEqual(nil, maps:get(line, Frame)),
    ?assertEqual(nil, maps:get(class_name, Frame)).

%%% ===================================================================
%%% dispatch/3 tests
%%% ===================================================================

make_test_frame() ->
    #{
        '$beamtalk_class' => 'StackFrame',
        module => beamtalk_integer,
        function => '+',
        arity => 2,
        file => <<"src/beamtalk_integer.erl">>,
        line => 42,
        class_name => 'Integer'
    }.

make_nil_frame() ->
    #{
        '$beamtalk_class' => 'StackFrame',
        module => undefined,
        function => undefined,
        arity => 0,
        file => nil,
        line => nil,
        class_name => nil
    }.

dispatch_class_test() ->
    ?assertEqual('StackFrame', beamtalk_stack_frame:dispatch('class', [], make_test_frame())).

dispatch_method_test() ->
    ?assertEqual('+', beamtalk_stack_frame:dispatch('method', [], make_test_frame())).

dispatch_receiver_class_test() ->
    ?assertEqual('Integer', beamtalk_stack_frame:dispatch('receiverClass', [], make_test_frame())).

dispatch_receiver_class_nil_test() ->
    ?assertEqual(nil, beamtalk_stack_frame:dispatch('receiverClass', [], make_nil_frame())).

dispatch_arguments_test() ->
    ?assertEqual(2, beamtalk_stack_frame:dispatch('arguments', [], make_test_frame())).

dispatch_source_location_test() ->
    Result = beamtalk_stack_frame:dispatch('sourceLocation', [], make_test_frame()),
    ?assertEqual(<<"src/beamtalk_integer.erl:42">>, Result).

dispatch_source_location_nil_file_test() ->
    ?assertEqual(nil, beamtalk_stack_frame:dispatch('sourceLocation', [], make_nil_frame())).

dispatch_source_location_nil_line_test() ->
    Frame = (make_test_frame())#{line => nil},
    ?assertEqual(nil, beamtalk_stack_frame:dispatch('sourceLocation', [], Frame)).

dispatch_module_name_test() ->
    Result = beamtalk_stack_frame:dispatch('moduleName', [], make_test_frame()),
    ?assertEqual(<<"beamtalk_integer">>, Result).

dispatch_module_name_undefined_test() ->
    ?assertEqual(nil, beamtalk_stack_frame:dispatch('moduleName', [], make_nil_frame())).

dispatch_line_test() ->
    ?assertEqual(42, beamtalk_stack_frame:dispatch('line', [], make_test_frame())).

dispatch_line_nil_test() ->
    ?assertEqual(nil, beamtalk_stack_frame:dispatch('line', [], make_nil_frame())).

dispatch_file_test() ->
    ?assertEqual(<<"src/beamtalk_integer.erl">>, beamtalk_stack_frame:dispatch('file', [], make_test_frame())).

dispatch_file_nil_test() ->
    ?assertEqual(nil, beamtalk_stack_frame:dispatch('file', [], make_nil_frame())).

dispatch_print_string_test() ->
    Result = beamtalk_stack_frame:dispatch('printString', [], make_test_frame()),
    ?assert(is_binary(Result)),
    ?assertNotEqual(<<>>, Result).

dispatch_describe_same_as_print_string_test() ->
    Frame = make_test_frame(),
    PS = beamtalk_stack_frame:dispatch('printString', [], Frame),
    Desc = beamtalk_stack_frame:dispatch('describe', [], Frame),
    ?assertEqual(PS, Desc).

dispatch_unknown_selector_raises_test() ->
    Frame = make_test_frame(),
    try
        beamtalk_stack_frame:dispatch('nonExistent', [], Frame),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(does_not_understand, Inner#beamtalk_error.kind),
            ?assertEqual('StackFrame', Inner#beamtalk_error.class),
            ?assertEqual('nonExistent', Inner#beamtalk_error.selector)
    end.

%%% ===================================================================
%%% has_method/1 tests
%%% ===================================================================

has_method_class_test() -> ?assert(beamtalk_stack_frame:has_method('class')).
has_method_method_test() -> ?assert(beamtalk_stack_frame:has_method('method')).
has_method_receiver_class_test() -> ?assert(beamtalk_stack_frame:has_method('receiverClass')).
has_method_arguments_test() -> ?assert(beamtalk_stack_frame:has_method('arguments')).
has_method_source_location_test() -> ?assert(beamtalk_stack_frame:has_method('sourceLocation')).
has_method_module_name_test() -> ?assert(beamtalk_stack_frame:has_method('moduleName')).
has_method_line_test() -> ?assert(beamtalk_stack_frame:has_method('line')).
has_method_file_test() -> ?assert(beamtalk_stack_frame:has_method('file')).
has_method_print_string_test() -> ?assert(beamtalk_stack_frame:has_method('printString')).
has_method_describe_test() -> ?assert(beamtalk_stack_frame:has_method('describe')).
has_method_unknown_test() -> ?assertNot(beamtalk_stack_frame:has_method('nonExistent')).

%%% ===================================================================
%%% format_frame tests (via printString dispatch)
%%% ===================================================================

format_frame_with_full_location_test() ->
    Result = beamtalk_stack_frame:dispatch('printString', [], make_test_frame()),
    ?assertEqual(<<"Integer>>+/2 (src/beamtalk_integer.erl:42)">>, Result).

format_frame_nil_class_test() ->
    Frame = (make_test_frame())#{class_name => nil},
    Result = beamtalk_stack_frame:dispatch('printString', [], Frame),
    ?assertEqual(<<"?>>+/2 (src/beamtalk_integer.erl:42)">>, Result).

format_frame_undefined_function_test() ->
    Frame = (make_test_frame())#{function => undefined, class_name => nil},
    Result = beamtalk_stack_frame:dispatch('printString', [], Frame),
    ?assertEqual(<<"?>>?/2 (src/beamtalk_integer.erl:42)">>, Result).

format_frame_no_location_test() ->
    Frame = (make_test_frame())#{file => nil, line => nil},
    Result = beamtalk_stack_frame:dispatch('printString', [], Frame),
    ?assertEqual(<<"Integer>>+/2">>, Result).

format_frame_file_no_line_test() ->
    Frame = (make_test_frame())#{line => nil},
    Result = beamtalk_stack_frame:dispatch('printString', [], Frame),
    ?assertEqual(<<"Integer>>+/2 (src/beamtalk_integer.erl)">>, Result).
