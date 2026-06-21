%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_reshape_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% String helpers (leading whitespace + trailing newlines)
%%
%% Reshaping proper (re-indent + re-layout) is done by the compiler port
%% (`beamtalk_compiler:reindent_method_source/2`) and proven byte-identical to
%% the on-disk span by the Rust corpus round-trip tests
%% (`corpus_methods_round_trip_byte_identical`). This module only holds the
%% pure string helpers shared by the install hook and flush (BT-2594).
%%====================================================================

leading_ws_test() ->
    ?assertEqual(<<>>, beamtalk_workspace_reshape:leading_ws(<<"foo">>)),
    ?assertEqual(<<"  ">>, beamtalk_workspace_reshape:leading_ws(<<"  foo">>)),
    ?assertEqual(<<"\t ">>, beamtalk_workspace_reshape:leading_ws(<<"\t foo\n  bar">>)).

strip_trailing_newlines_test() ->
    ?assertEqual(<<>>, beamtalk_workspace_reshape:strip_trailing_newlines(<<>>)),
    ?assertEqual(<<"a">>, beamtalk_workspace_reshape:strip_trailing_newlines(<<"a\n\n">>)),
    ?assertEqual(<<"a">>, beamtalk_workspace_reshape:strip_trailing_newlines(<<"a">>)).

ensure_trailing_newline_test() ->
    ?assertEqual(<<"\n">>, beamtalk_workspace_reshape:ensure_trailing_newline(<<>>)),
    ?assertEqual(<<"a\n">>, beamtalk_workspace_reshape:ensure_trailing_newline(<<"a">>)),
    ?assertEqual(<<"a\n">>, beamtalk_workspace_reshape:ensure_trailing_newline(<<"a\n">>)).
