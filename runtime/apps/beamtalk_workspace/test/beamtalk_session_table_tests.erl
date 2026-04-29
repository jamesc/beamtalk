%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_session_table_tests).

-moduledoc """
Unit tests for beamtalk_session_table.

Covers new/0 (including idempotency), insert/2, lookup/1 (found and
missing branches), and delete/1. The resolve_pid/2 branches are already
exercised in beamtalk_repl_server_tests (BT-1045).
""".

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test fixture
%%====================================================================

session_table_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun new_creates_table/1,
        fun new_is_idempotent/1,
        fun insert_and_lookup/1,
        fun lookup_missing_returns_error/1,
        fun delete_removes_entry/1,
        fun delete_missing_is_safe/1
    ]}.

setup() ->
    case ets:whereis(beamtalk_sessions) of
        undefined -> ok;
        _Tid -> ets:delete(beamtalk_sessions)
    end,
    beamtalk_session_table:new(),
    ok.

cleanup(_) ->
    case ets:whereis(beamtalk_sessions) of
        undefined -> ok;
        _Tid -> ets:delete(beamtalk_sessions)
    end.

%%====================================================================
%% Tests
%%====================================================================

%%% new/0 tests

new_creates_table(_) ->
    [?_assertNotEqual(undefined, ets:whereis(beamtalk_sessions))].

new_is_idempotent(_) ->
    %% A second call on an already-existing table must still return ok.
    [?_assertEqual(ok, beamtalk_session_table:new())].

%%% insert/2 + lookup/1 tests

insert_and_lookup(_) ->
    Pid = self(),
    beamtalk_session_table:insert(<<"s1">>, Pid),
    [?_assertEqual({ok, Pid}, beamtalk_session_table:lookup(<<"s1">>))].

lookup_missing_returns_error(_) ->
    [?_assertEqual(error, beamtalk_session_table:lookup(<<"no_such_session">>))].

%%% delete/1 tests

delete_removes_entry(_) ->
    Pid = self(),
    beamtalk_session_table:insert(<<"s2">>, Pid),
    beamtalk_session_table:delete(<<"s2">>),
    [?_assertEqual(error, beamtalk_session_table:lookup(<<"s2">>))].

delete_missing_is_safe(_) ->
    %% ets:delete/2 on a missing key returns true — must not raise.
    [?_assertEqual(true, beamtalk_session_table:delete(<<"ghost">>))].
