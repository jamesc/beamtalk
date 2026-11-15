%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_row_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
EUnit tests for `beamtalk_repl_row`: the row-building helpers shared by the
navigation and browser ops.
""".

-include_lib("eunit/include/eunit.hrl").

first_line_test() ->
    ?assertEqual(null, beamtalk_repl_row:first_line(null)),
    ?assertEqual(<<"one">>, beamtalk_repl_row:first_line(<<"one">>)),
    ?assertEqual(<<"one">>, beamtalk_repl_row:first_line(<<"one\ntwo\nthree">>)),
    ?assertEqual(<<>>, beamtalk_repl_row:first_line(<<>>)),
    ?assertEqual(<<>>, beamtalk_repl_row:first_line(<<"\nrest">>)).

source_file_of_unknown_module_is_null_test() ->
    ?assertEqual(null, beamtalk_repl_row:source_file_of(xyzzy_row_no_such_module)).

class_source_file_unknown_class_is_null_test() ->
    ?assertEqual(null, beamtalk_repl_row:class_source_file('XyzzyRowNoSuchClass')).

source_origin_of_unknown_class_is_project_test() ->
    ?assertEqual(<<"project">>, beamtalk_repl_row:source_origin_of('XyzzyRowNoSuchClass')).

method_line_unindexed_method_is_null_test() ->
    ?assertEqual(
        null, beamtalk_repl_row:method_line('XyzzyRowNoSuchClass', false, 'noSuchSelector')
    ).
