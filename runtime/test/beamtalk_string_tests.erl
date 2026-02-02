%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_string_tests).
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% Test Setup/Cleanup
%%% ============================================================================

%% Setup: Initialize extension registry for DNU tests
setup() ->
    beamtalk_extensions:init(),
    ok.

%% Cleanup: Clear extension registry
cleanup(_) ->
    %% Clear any test extensions
    ok.

%%% ============================================================================
%%% Reflection Tests
%%% ============================================================================

class_test() ->
    ?assertEqual('String', beamtalk_string:dispatch('class', [], <<"hello">>)),
    ?assertEqual('String', beamtalk_string:dispatch('class', [], <<>>)),
    ?assertEqual('String', beamtalk_string:dispatch('class', [], <<"こんにちは"/utf8>>)).

%%% ============================================================================
%%% Size Operations Tests
%%% ============================================================================

size_test() ->
    ?assertEqual(5, beamtalk_string:dispatch('size', [], <<"hello">>)),
    ?assertEqual(0, beamtalk_string:dispatch('size', [], <<>>)),
    ?assertEqual(11, beamtalk_string:dispatch('size', [], <<"hello world">>)).

length_test() ->
    ?assertEqual(5, beamtalk_string:dispatch('length', [], <<"hello">>)),
    ?assertEqual(0, beamtalk_string:dispatch('length', [], <<>>)).

is_empty_test() ->
    ?assertEqual(true, beamtalk_string:dispatch('isEmpty', [], <<>>)),
    ?assertEqual(false, beamtalk_string:dispatch('isEmpty', [], <<"hello">>)),
    ?assertEqual(false, beamtalk_string:dispatch('isEmpty', [], <<" ">>)).

%%% ============================================================================
%%% Case Conversion Tests
%%% ============================================================================

uppercase_test() ->
    ?assertEqual(<<"HELLO">>, beamtalk_string:dispatch('uppercase', [], <<"hello">>)),
    ?assertEqual(<<"HELLO">>, beamtalk_string:dispatch('uppercase', [], <<"HELLO">>)),
    ?assertEqual(<<"HELLO WORLD">>, beamtalk_string:dispatch('uppercase', [], <<"HeLLo WoRLd">>)),
    ?assertEqual(<<>>, beamtalk_string:dispatch('uppercase', [], <<>>)).

lowercase_test() ->
    ?assertEqual(<<"hello">>, beamtalk_string:dispatch('lowercase', [], <<"HELLO">>)),
    ?assertEqual(<<"hello">>, beamtalk_string:dispatch('lowercase', [], <<"hello">>)),
    ?assertEqual(<<"hello world">>, beamtalk_string:dispatch('lowercase', [], <<"HeLLo WoRLd">>)),
    ?assertEqual(<<>>, beamtalk_string:dispatch('lowercase', [], <<>>)).

%%% ============================================================================
%%% Trimming Tests
%%% ============================================================================

trim_test() ->
    ?assertEqual(<<"hello">>, beamtalk_string:dispatch('trim', [], <<"  hello  ">>)),
    ?assertEqual(<<"hello">>, beamtalk_string:dispatch('trim', [], <<"hello  ">>)),
    ?assertEqual(<<"hello">>, beamtalk_string:dispatch('trim', [], <<"  hello">>)),
    ?assertEqual(<<"hello">>, beamtalk_string:dispatch('trim', [], <<"hello">>)),
    ?assertEqual(<<>>, beamtalk_string:dispatch('trim', [], <<"   ">>)),
    ?assertEqual(<<"hello world">>, beamtalk_string:dispatch('trim', [], <<"  hello world  ">>)).

%%% ============================================================================
%%% Concatenation Tests
%%% ============================================================================

concatenation_binary_op_test() ->
    ?assertEqual(<<"hello world">>, 
                 beamtalk_string:dispatch('++', [<<" world">>], <<"hello">>)),
    ?assertEqual(<<"hello">>, 
                 beamtalk_string:dispatch('++', [<<>>], <<"hello">>)),
    ?assertEqual(<<"hello">>, 
                 beamtalk_string:dispatch('++', [<<"hello">>], <<>>)),
    ?assertEqual(<<>>, 
                 beamtalk_string:dispatch('++', [<<>>], <<>>)).

concatenation_keyword_test() ->
    ?assertEqual(<<"hello world">>, 
                 beamtalk_string:dispatch('concat:', [<<" world">>], <<"hello">>)),
    ?assertEqual(<<"abc">>, 
                 beamtalk_string:dispatch('concat:', [<<"c">>], <<"ab">>)).

concatenation_type_safety_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Concatenation with non-binary should raise does_not_understand", fun() ->
            ?assertError({does_not_understand, 'String', '++', 1},
                         beamtalk_string:dispatch('++', [42], <<"hello">>)),
            ?assertError({does_not_understand, 'String', 'concat:', 1},
                         beamtalk_string:dispatch('concat:', ['atom'], <<"hello">>))
        end}
    ]}.

%%% ============================================================================
%%% Access Tests
%%% ============================================================================

at_test() ->
    ?assertEqual(<<"h">>, beamtalk_string:dispatch('at:', [1], <<"hello">>)),
    ?assertEqual(<<"e">>, beamtalk_string:dispatch('at:', [2], <<"hello">>)),
    ?assertEqual(<<"o">>, beamtalk_string:dispatch('at:', [5], <<"hello">>)),
    ?assertEqual(<<" ">>, beamtalk_string:dispatch('at:', [6], <<"hello world">>)).

at_out_of_bounds_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Out of bounds access should raise does_not_understand", fun() ->
            ?assertError({does_not_understand, 'String', 'at:', 1},
                         beamtalk_string:dispatch('at:', [0], <<"hello">>)),
            ?assertError({does_not_understand, 'String', 'at:', 1},
                         beamtalk_string:dispatch('at:', [6], <<"hello">>)),
            ?assertError({does_not_understand, 'String', 'at:', 1},
                         beamtalk_string:dispatch('at:', [-1], <<"hello">>))
        end}
    ]}.

at_empty_string_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Empty string access raises does_not_understand", fun() ->
            ?assertError({does_not_understand, 'String', 'at:', 1},
                         beamtalk_string:dispatch('at:', [1], <<>>))
        end}
    ]}.

%%% ============================================================================
%%% Search Tests
%%% ============================================================================

includes_test() ->
    ?assertEqual(true, beamtalk_string:dispatch('includes:', [<<"ell">>], <<"hello">>)),
    ?assertEqual(true, beamtalk_string:dispatch('includes:', [<<"hello">>], <<"hello">>)),
    ?assertEqual(true, beamtalk_string:dispatch('includes:', [<<"o">>], <<"hello">>)),
    ?assertEqual(false, beamtalk_string:dispatch('includes:', [<<"xyz">>], <<"hello">>)),
    ?assertEqual(false, beamtalk_string:dispatch('includes:', [<<"Hello">>], <<"hello">>)),
    ?assertEqual(true, beamtalk_string:dispatch('includes:', [<<>>], <<"hello">>)).

includes_empty_string_test() ->
    ?assertEqual(false, beamtalk_string:dispatch('includes:', [<<"a">>], <<>>)),
    ?assertEqual(true, beamtalk_string:dispatch('includes:', [<<>>], <<"hello">>)),
    ?assertEqual(true, beamtalk_string:dispatch('includes:', [<<>>], <<>>)).

includes_type_safety_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"includes with non-binary should raise does_not_understand", fun() ->
            ?assertError({does_not_understand, 'String', 'includes:', 1},
                         beamtalk_string:dispatch('includes:', [42], <<"hello">>))
        end}
    ]}.

%%% ============================================================================
%%% Split Tests
%%% ============================================================================

split_test() ->
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>], 
                 beamtalk_string:dispatch('split:', [<<",">>], <<"a,b,c">>)),
    ?assertEqual([<<"hello">>, <<"world">>], 
                 beamtalk_string:dispatch('split:', [<<" ">>], <<"hello world">>)),
    ?assertEqual([<<"one">>, <<"two">>, <<"three">>], 
                 beamtalk_string:dispatch('split:', [<<"-">>], <<"one-two-three">>)).

split_no_match_test() ->
    ?assertEqual([<<"hello">>], 
                 beamtalk_string:dispatch('split:', [<<",">>], <<"hello">>)),
    ?assertEqual([<<>>], 
                 beamtalk_string:dispatch('split:', [<<",">>], <<>>)).

split_empty_delimiter_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Empty delimiter raises does_not_understand", fun() ->
            ?assertError({does_not_understand, 'String', 'split:', 1},
                         beamtalk_string:dispatch('split:', [<<>>], <<"abc">>))
        end}
    ]}.

split_type_safety_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"split with non-binary should raise does_not_understand", fun() ->
            ?assertError({does_not_understand, 'String', 'split:', 1},
                         beamtalk_string:dispatch('split:', [42], <<"hello">>))
        end}
    ]}.

%%% ============================================================================
%%% Conversion Tests
%%% ============================================================================

as_integer_test() ->
    ?assertEqual(42, beamtalk_string:dispatch('asInteger', [], <<"42">>)),
    ?assertEqual(-5, beamtalk_string:dispatch('asInteger', [], <<"-5">>)),
    ?assertEqual(0, beamtalk_string:dispatch('asInteger', [], <<"0">>)),
    ?assertEqual(12345, beamtalk_string:dispatch('asInteger', [], <<"12345">>)).

as_integer_invalid_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Invalid integer should raise does_not_understand", fun() ->
            ?assertError({does_not_understand, 'String', 'asInteger', 0},
                         beamtalk_string:dispatch('asInteger', [], <<"hello">>)),
            ?assertError({does_not_understand, 'String', 'asInteger', 0},
                         beamtalk_string:dispatch('asInteger', [], <<"3.14">>)),
            ?assertError({does_not_understand, 'String', 'asInteger', 0},
                         beamtalk_string:dispatch('asInteger', [], <<>>)),
            ?assertError({does_not_understand, 'String', 'asInteger', 0},
                         beamtalk_string:dispatch('asInteger', [], <<"  42  ">>))
        end}
    ]}.

%%% ============================================================================
%%% has_method Tests
%%% ============================================================================

has_method_builtin_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Builtin methods return true", fun() ->
            ?assertEqual(true, beamtalk_string:has_method('class')),
            ?assertEqual(true, beamtalk_string:has_method('size')),
            ?assertEqual(true, beamtalk_string:has_method('length')),
            ?assertEqual(true, beamtalk_string:has_method('isEmpty')),
            ?assertEqual(true, beamtalk_string:has_method('uppercase')),
            ?assertEqual(true, beamtalk_string:has_method('lowercase')),
            ?assertEqual(true, beamtalk_string:has_method('trim')),
            ?assertEqual(true, beamtalk_string:has_method('++')),
            ?assertEqual(true, beamtalk_string:has_method('concat:')),
            ?assertEqual(true, beamtalk_string:has_method('at:')),
            ?assertEqual(true, beamtalk_string:has_method('includes:')),
            ?assertEqual(true, beamtalk_string:has_method('split:')),
            ?assertEqual(true, beamtalk_string:has_method('asInteger')),
            ?assertEqual(false, beamtalk_string:has_method('unknownMethod'))
        end}
    ]}.

%%% ============================================================================
%%% Extension Registry Tests
%%% ============================================================================

extension_method_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Extension methods work via DNU", fun() ->
            %% Register a custom extension method
            ExtFun = fun([_Sep], Str) -> <<"reversed: ", (list_to_binary(lists:reverse(binary_to_list(Str))))/binary>> end,
            beamtalk_extensions:register('String', 'reverse:', ExtFun, test_module),
            
            %% Should find the extension method
            ?assertEqual(true, beamtalk_string:has_method('reverse:')),
            
            %% Should dispatch to extension
            Result = beamtalk_string:dispatch('reverse:', [<<",">>], <<"hello">>),
            ?assertEqual(<<"reversed: olleh">>, Result)
        end},
        
        {"Unknown method raises does_not_understand", fun() ->
            ?assertError({does_not_understand, 'String', 'unknownMethod', 0},
                         beamtalk_string:dispatch('unknownMethod', [], <<"hello">>))
        end}
    ]}.
