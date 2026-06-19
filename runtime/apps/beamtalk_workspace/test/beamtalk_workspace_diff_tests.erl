%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_diff_tests).

-moduledoc "Unit tests for beamtalk_workspace_diff (ADR 0082, BT-2575).".

-include_lib("eunit/include/eunit.hrl").

%% A single changed line reads as a delete of the old then an insert of the new,
%% with the surrounding lines kept as context.
changed_line_test() ->
    Old = <<"inc =>\n  self.v := self.v + 1">>,
    New = <<"inc =>\n  self.v := self.v + 2">>,
    Diff = beamtalk_workspace_diff:unified(Old, New),
    ?assertEqual(
        <<"  inc =>\n", "- ", "  self.v := self.v + 1\n", "+ ", "  self.v := self.v + 2\n">>,
        Diff
    ).

%% A brand-new method (no on-disk body) is all additions.
all_added_test() ->
    Diff = beamtalk_workspace_diff:unified(<<>>, <<"doubled =>\n  self.v * 2">>),
    ?assertEqual(<<"+ doubled =>\n+   self.v * 2\n">>, Diff).

%% A pure deletion (every line removed) is all deletions.
all_removed_test() ->
    Diff = beamtalk_workspace_diff:unified(<<"a\nb">>, <<>>),
    ?assertEqual(<<"- a\n- b\n">>, Diff).

%% Identical bodies produce only context lines (callers normally skip these).
identical_test() ->
    Diff = beamtalk_workspace_diff:unified(<<"a\nb">>, <<"a\nb">>),
    ?assertEqual(<<"  a\n  b\n">>, Diff).

%% A trailing newline is a line terminator, not an extra blank line — so a body
%% that differs only by a trailing newline diffs as identical.
trailing_newline_is_not_a_line_test() ->
    ?assertEqual(
        beamtalk_workspace_diff:ops(<<"a\nb">>, <<"a\nb">>),
        beamtalk_workspace_diff:ops(<<"a\nb\n">>, <<"a\nb">>)
    ).

%% An added line in the middle keeps the unchanged lines as context around it.
inserted_middle_line_test() ->
    Ops = beamtalk_workspace_diff:ops(<<"a\nc">>, <<"a\nb\nc">>),
    ?assertEqual([{eq, <<"a">>}, {ins, <<"b">>}, {eq, <<"c">>}], Ops).
