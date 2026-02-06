%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_string_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

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

length_utf8_test() ->
    %% length returns grapheme count, not byte count
    ?assertEqual(5, beamtalk_string:dispatch('length', [], <<"こんにちは"/utf8>>)),
    ?assertEqual(11, beamtalk_string:dispatch('size', [], <<"hello world">>)),
    ?assertEqual(15, beamtalk_string:dispatch('size', [], <<"こんにちは"/utf8>>)),  % 15 bytes for 5 characters
    ?assertEqual(2, beamtalk_string:dispatch('length', [], <<"👋🌍"/utf8>>)),
    ?assertEqual(8, beamtalk_string:dispatch('size', [], <<"👋🌍"/utf8>>)).  % 8 bytes for 2 emoji

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
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'String', selector = '++'},
                         beamtalk_string:dispatch('++', [42], <<"hello">>)),
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'String', selector = 'concat:'},
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

at_utf8_test() ->
    %% Test UTF-8 grapheme handling
    ?assertEqual(<<"こ"/utf8>>, beamtalk_string:dispatch('at:', [1], <<"こんにちは"/utf8>>)),
    ?assertEqual(<<"ん"/utf8>>, beamtalk_string:dispatch('at:', [2], <<"こんにちは"/utf8>>)),
    ?assertEqual(<<"は"/utf8>>, beamtalk_string:dispatch('at:', [5], <<"こんにちは"/utf8>>)),
    %% Test emoji (multi-byte grapheme)
    ?assertEqual(<<"👋"/utf8>>, beamtalk_string:dispatch('at:', [1], <<"👋🌍"/utf8>>)),
    ?assertEqual(<<"🌍"/utf8>>, beamtalk_string:dispatch('at:', [2], <<"👋🌍"/utf8>>)).

at_out_of_bounds_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Out of bounds access should raise does_not_understand", fun() ->
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'String', selector = 'at:'},
                         beamtalk_string:dispatch('at:', [0], <<"hello">>)),
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'String', selector = 'at:'},
                         beamtalk_string:dispatch('at:', [6], <<"hello">>)),
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'String', selector = 'at:'},
                         beamtalk_string:dispatch('at:', [-1], <<"hello">>))
        end}
    ]}.

at_empty_string_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Empty string access raises does_not_understand", fun() ->
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'String', selector = 'at:'},
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
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'String', selector = 'includes:'},
                         beamtalk_string:dispatch('includes:', [42], <<"hello">>))
        end}
    ]}.

%%% ============================================================================
%%% startsWith / endsWith Tests
%%% ============================================================================

starts_with_test() ->
    ?assertEqual(true, beamtalk_string:dispatch('startsWith:', [<<"hel">>], <<"hello">>)),
    ?assertEqual(true, beamtalk_string:dispatch('startsWith:', [<<"hello">>], <<"hello">>)),
    ?assertEqual(false, beamtalk_string:dispatch('startsWith:', [<<"ello">>], <<"hello">>)),
    ?assertEqual(true, beamtalk_string:dispatch('startsWith:', [<<>>], <<"hello">>)),
    ?assertEqual(false, beamtalk_string:dispatch('startsWith:', [<<"hello world">>], <<"hello">>)).

starts_with_utf8_test() ->
    ?assertEqual(true, beamtalk_string:dispatch('startsWith:', [<<"こん"/utf8>>], <<"こんにちは"/utf8>>)),
    ?assertEqual(false, beamtalk_string:dispatch('startsWith:', [<<"にち"/utf8>>], <<"こんにちは"/utf8>>)),
    ?assertEqual(true, beamtalk_string:dispatch('startsWith:', [<<"👋"/utf8>>], <<"👋🌍"/utf8>>)).

ends_with_test() ->
    ?assertEqual(true, beamtalk_string:dispatch('endsWith:', [<<"llo">>], <<"hello">>)),
    ?assertEqual(true, beamtalk_string:dispatch('endsWith:', [<<"hello">>], <<"hello">>)),
    ?assertEqual(false, beamtalk_string:dispatch('endsWith:', [<<"hell">>], <<"hello">>)),
    ?assertEqual(true, beamtalk_string:dispatch('endsWith:', [<<>>], <<"hello">>)),
    ?assertEqual(false, beamtalk_string:dispatch('endsWith:', [<<"hello world">>], <<"world">>)).

ends_with_utf8_test() ->
    ?assertEqual(true, beamtalk_string:dispatch('endsWith:', [<<"ちは"/utf8>>], <<"こんにちは"/utf8>>)),
    ?assertEqual(false, beamtalk_string:dispatch('endsWith:', [<<"こん"/utf8>>], <<"こんにちは"/utf8>>)),
    ?assertEqual(true, beamtalk_string:dispatch('endsWith:', [<<"🌍"/utf8>>], <<"👋🌍"/utf8>>)).

%%% ============================================================================
%%% indexOf Tests
%%% ============================================================================

index_of_test() ->
    ?assertEqual(1, beamtalk_string:dispatch('indexOf:', [<<"h">>], <<"hello">>)),
    ?assertEqual(2, beamtalk_string:dispatch('indexOf:', [<<"e">>], <<"hello">>)),
    ?assertEqual(3, beamtalk_string:dispatch('indexOf:', [<<"ll">>], <<"hello">>)),
    ?assertEqual(1, beamtalk_string:dispatch('indexOf:', [<<"hello">>], <<"hello">>)),
    ?assertEqual(nil, beamtalk_string:dispatch('indexOf:', [<<"xyz">>], <<"hello">>)),
    ?assertEqual(nil, beamtalk_string:dispatch('indexOf:', [<<>>], <<"hello">>)).

index_of_utf8_test() ->
    ?assertEqual(1, beamtalk_string:dispatch('indexOf:', [<<"こ"/utf8>>], <<"こんにちは"/utf8>>)),
    ?assertEqual(3, beamtalk_string:dispatch('indexOf:', [<<"に"/utf8>>], <<"こんにちは"/utf8>>)),
    ?assertEqual(2, beamtalk_string:dispatch('indexOf:', [<<"んに"/utf8>>], <<"こんにちは"/utf8>>)),
    ?assertEqual(nil, beamtalk_string:dispatch('indexOf:', [<<"xyz">>], <<"こんにちは"/utf8>>)).

%%% ============================================================================
%%% replace Tests
%%% ============================================================================

replace_test() ->
    ?assertEqual(<<"hi world">>, beamtalk_string:dispatch('replace:with:', [<<"hello">>, <<"hi">>], <<"hello world">>)),
    ?assertEqual(<<"he11o">>, beamtalk_string:dispatch('replace:with:', [<<"l">>, <<"1">>], <<"hello">>)),
    ?assertEqual(<<"abc abc abc">>, beamtalk_string:dispatch('replace:with:', [<<"x">>, <<"abc">>], <<"x x x">>)),
    ?assertEqual(<<"hello">>, beamtalk_string:dispatch('replace:with:', [<<"xyz">>, <<"abc">>], <<"hello">>)),
    ?assertEqual(<<"">>, beamtalk_string:dispatch('replace:with:', [<<"hello">>, <<>>], <<"hello">>)).

replace_utf8_test() ->
    ?assertEqual(<<"さようならにちは"/utf8>>, 
                 beamtalk_string:dispatch('replace:with:', [<<"こん"/utf8>>, <<"さようなら"/utf8>>], <<"こんにちは"/utf8>>)),
    ?assertEqual(<<"🎉🎉"/utf8>>, 
                 beamtalk_string:dispatch('replace:with:', [<<"👋"/utf8>>, <<"🎉"/utf8>>], <<"👋👋"/utf8>>)).

replace_empty_pattern_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Empty pattern raises does_not_understand", fun() ->
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'String', selector = 'replace:with:'},
                         beamtalk_string:dispatch('replace:with:', [<<>>, <<"x">>], <<"hello">>))
        end}
    ]}.

%%% ============================================================================
%%% substring Tests
%%% ============================================================================

substring_test() ->
    ?assertEqual(<<"hel">>, beamtalk_string:dispatch('substring:to:', [1, 3], <<"hello">>)),
    ?assertEqual(<<"ell">>, beamtalk_string:dispatch('substring:to:', [2, 4], <<"hello">>)),
    ?assertEqual(<<"hello">>, beamtalk_string:dispatch('substring:to:', [1, 5], <<"hello">>)),
    ?assertEqual(<<"o">>, beamtalk_string:dispatch('substring:to:', [5, 5], <<"hello">>)),
    ?assertEqual(<<>>, beamtalk_string:dispatch('substring:to:', [3, 2], <<"hello">>)).

substring_bounds_test() ->
    %% End beyond string length should be capped
    ?assertEqual(<<"hello">>, beamtalk_string:dispatch('substring:to:', [1, 100], <<"hello">>)),
    ?assertEqual(<<"llo">>, beamtalk_string:dispatch('substring:to:', [3, 100], <<"hello">>)).

substring_utf8_test() ->
    ?assertEqual(<<"こん"/utf8>>, beamtalk_string:dispatch('substring:to:', [1, 2], <<"こんにちは"/utf8>>)),
    ?assertEqual(<<"んに"/utf8>>, beamtalk_string:dispatch('substring:to:', [2, 3], <<"こんにちは"/utf8>>)),
    ?assertEqual(<<"は"/utf8>>, beamtalk_string:dispatch('substring:to:', [5, 5], <<"こんにちは"/utf8>>)),
    ?assertEqual(<<"👋"/utf8>>, beamtalk_string:dispatch('substring:to:', [1, 1], <<"👋🌍"/utf8>>)).

substring_invalid_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Invalid range raises does_not_understand", fun() ->
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'String', selector = 'substring:to:'},
                         beamtalk_string:dispatch('substring:to:', [0, 3], <<"hello">>)),
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'String', selector = 'substring:to:'},
                         beamtalk_string:dispatch('substring:to:', [10, 20], <<"hello">>))
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
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'String', selector = 'split:'},
                         beamtalk_string:dispatch('split:', [<<>>], <<"abc">>))
        end}
    ]}.

split_type_safety_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"split with non-binary should raise does_not_understand", fun() ->
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'String', selector = 'split:'},
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
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'String', selector = 'asInteger'},
                         beamtalk_string:dispatch('asInteger', [], <<"hello">>)),
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'String', selector = 'asInteger'},
                         beamtalk_string:dispatch('asInteger', [], <<"3.14">>)),
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'String', selector = 'asInteger'},
                         beamtalk_string:dispatch('asInteger', [], <<>>)),
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'String', selector = 'asInteger'},
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
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'String', selector = 'unknownMethod'},
                         beamtalk_string:dispatch('unknownMethod', [], <<"hello">>))
        end}
    ]}.

%%% ============================================================================
%%% Instance Variable Reflection Tests (BT-164)
%%% ============================================================================

instVarNames_test() ->
    %% Primitives have no instance variables
    ?assertEqual([], beamtalk_string:dispatch('instVarNames', [], <<"hello">>)).

instVarAt_test() ->
    %% Primitives always return nil for instVarAt:
    ?assertEqual(nil, beamtalk_string:dispatch('instVarAt', [anyField], <<"test">>)),
    ?assertEqual(nil, beamtalk_string:dispatch('instVarAt', [value], <<"string">>)).

instVarAt_put_immutable_error_test() ->
    %% Trying to mutate a primitive should raise an immutable_primitive error
    ?assertError({beamtalk_error, immutable_primitive, 'String', 'instVarAt:put:', _, _, _},
                 beamtalk_string:dispatch('instVarAt:put:', [value, <<"new">>], <<"old">>)).
