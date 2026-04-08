%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_erlang_help_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% format_module_help/1
%%====================================================================

format_module_help_returns_ok_for_lists_test() ->
    {ok, Text} = beamtalk_erlang_help:format_module_help(lists),
    ?assertNotEqual(nomatch, binary:match(Text, <<"Erlang module: lists">>)),
    ?assertNotEqual(nomatch, binary:match(Text, <<"Functions:">>)).

format_module_help_returns_ok_for_preloaded_test() ->
    %% erlang is preloaded — should return exports list
    {ok, Text} = beamtalk_erlang_help:format_module_help(erlang),
    ?assertNotEqual(nomatch, binary:match(Text, <<"Erlang module: erlang">>)).

format_module_help_returns_error_for_nonexistent_test() ->
    ?assertEqual(
        {error, not_found}, beamtalk_erlang_help:format_module_help(nonexistent_module_xyz)
    ).

%%====================================================================
%% format_function_help/2
%%====================================================================

format_function_help_returns_ok_for_lists_reverse_test() ->
    {ok, Text} = beamtalk_erlang_help:format_function_help(lists, reverse),
    ?assertNotEqual(nomatch, binary:match(Text, <<"lists:reverse">>)).

format_function_help_returns_error_for_nonexistent_function_test() ->
    ?assertEqual(
        {error, not_found},
        beamtalk_erlang_help:format_function_help(lists, zzz_nonexistent_fn_xyz)
    ).

format_function_help_returns_error_for_nonexistent_module_test() ->
    ?assertEqual(
        {error, not_found},
        beamtalk_erlang_help:format_function_help(nonexistent_module_xyz, foo)
    ).

format_function_help_accepts_binary_name_test() ->
    %% Binary function names should work (used when atom doesn't exist yet)
    {ok, Text} = beamtalk_erlang_help:format_function_help(lists, <<"reverse">>),
    ?assertNotEqual(nomatch, binary:match(Text, <<"lists:reverse">>)).

format_function_help_binary_nonexistent_returns_error_test() ->
    ?assertEqual(
        {error, not_found},
        beamtalk_erlang_help:format_function_help(lists, <<"zzz_nonexistent_xyz">>)
    ).

%%====================================================================
%% available_modules/0
%%====================================================================

available_modules_returns_sorted_list_test() ->
    Modules = beamtalk_erlang_help:available_modules(),
    ?assert(is_list(Modules)),
    ?assert(length(Modules) > 0),
    %% Should be sorted
    ?assertEqual(Modules, lists:sort(Modules)).

available_modules_contains_common_modules_test() ->
    Modules = beamtalk_erlang_help:available_modules(),
    ?assert(lists:member(<<"lists">>, Modules)),
    ?assert(lists:member(<<"maps">>, Modules)).

available_modules_excludes_bt_modules_test() ->
    Modules = beamtalk_erlang_help:available_modules(),
    BtModules = [M || <<"bt@", _/binary>> = M <- Modules],
    ?assertEqual([], BtModules).

%%====================================================================
%% format_beamtalk_signature/3 (moved from beamtalk_repl_ops_dev_tests)
%%====================================================================

signature_no_double_colon_for_keyword_name_test() ->
    Params = [#{name => <<"input">>, type => <<"String">>}],
    Result = beamtalk_erlang_help:format_beamtalk_signature(<<"parse:">>, Params, <<"Map">>),
    ?assertNotEqual(nomatch, binary:match(Result, <<"parse: input">>)),
    ?assertEqual(nomatch, binary:match(Result, <<"parse::">>)).

signature_adds_colon_for_plain_name_test() ->
    Params = [#{name => <<"input">>, type => <<"String">>}],
    Result = beamtalk_erlang_help:format_beamtalk_signature(<<"parse">>, Params, <<"Map">>),
    ?assertNotEqual(nomatch, binary:match(Result, <<"parse: input">>)).

signature_nullary_unchanged_test() ->
    Result = beamtalk_erlang_help:format_beamtalk_signature(<<"version">>, [], <<"String">>),
    ?assertEqual(<<"version -> String">>, Result).

%%====================================================================
%% dedupe_keyword_aliases/1 (moved from beamtalk_repl_ops_dev_tests)
%%====================================================================

dedupe_removes_plain_alias_when_keyword_exists_test() ->
    Specs = [
        #{name => <<"parse">>, arity => 1, params => [], return_type => <<"Map">>},
        #{name => <<"parse:">>, arity => 1, params => [], return_type => <<"Map">>}
    ],
    Result = beamtalk_erlang_help:dedupe_keyword_aliases(Specs),
    ?assertEqual(1, length(Result)),
    ?assertEqual(<<"parse:">>, maps:get(name, hd(Result))).

dedupe_empty_list_test() ->
    ?assertEqual([], beamtalk_erlang_help:dedupe_keyword_aliases([])).

%%====================================================================
%% find_function_arities/2
%%====================================================================

find_function_arities_returns_sorted_arities_test() ->
    %% lists:reverse has arity 1 and 2
    Arities = beamtalk_erlang_help:find_function_arities(lists, reverse),
    ?assert(is_list(Arities)),
    ?assert(length(Arities) > 0),
    ?assertEqual(Arities, lists:sort(Arities)).

find_function_arities_returns_empty_for_nonexistent_test() ->
    ?assertEqual([], beamtalk_erlang_help:find_function_arities(lists, zzz_nonexistent_fn_xyz)).
