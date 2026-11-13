%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Object System Context

-module(beamtalk_class_session_tests).
-moduledoc "EUnit tests for beamtalk_class_session (BT-4771 unchanged-context fast path).".

-include_lib("eunit/include/eunit.hrl").

keys() ->
    [
        beamtalk_session_pid,
        beamtalk_session_id,
        beamtalk_session_meta,
        beamtalk_entry_group_leader
    ].

snapshot() -> [{K, get(K)} || K <- keys()].

clear() ->
    [erase(K) || K <- keys()],
    ok.

seed_changed_then_restore_test() ->
    clear(),
    Ctx = {self(), <<"s1">>, #{kind => repl}, undefined},
    Restore = beamtalk_class_session:seed_session_context_from(Ctx),
    ?assertEqual(self(), get(beamtalk_session_pid)),
    ?assertEqual(<<"s1">>, get(beamtalk_session_id)),
    ?assertEqual(#{kind => repl}, get(beamtalk_session_meta)),
    ok = Restore(),
    ?assertEqual([{K, undefined} || K <- keys()], snapshot()).

empty_context_on_empty_process_is_noop_test() ->
    clear(),
    Restore = beamtalk_class_session:seed_session_context_from(
        {undefined, undefined, undefined, undefined}
    ),
    ?assertEqual([{K, undefined} || K <- keys()], snapshot()),
    ok = Restore(),
    ?assertEqual([{K, undefined} || K <- keys()], snapshot()).

unchanged_context_leaves_dictionary_alone_test() ->
    clear(),
    put(beamtalk_session_id, <<"s1">>),
    put(beamtalk_session_pid, self()),
    Before = snapshot(),
    Restore = beamtalk_class_session:seed_session_context_from(
        {self(), <<"s1">>, undefined, undefined}
    ),
    ?assertEqual(Before, snapshot()),
    ok = Restore(),
    %% Restoring an unchanged context must not erase what was already there.
    ?assertEqual(Before, snapshot()),
    clear().

changed_context_restores_previous_values_test() ->
    clear(),
    put(beamtalk_session_id, <<"outer">>),
    Before = snapshot(),
    Restore = beamtalk_class_session:seed_session_context_from(
        {undefined, <<"inner">>, undefined, undefined}
    ),
    ?assertEqual(<<"inner">>, get(beamtalk_session_id)),
    ok = Restore(),
    ?assertEqual(Before, snapshot()),
    clear().

pid_sink_always_takes_full_path_test() ->
    clear(),
    Sink = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    GL = group_leader(),
    put(beamtalk_entry_group_leader, Sink),
    Restore = beamtalk_class_session:seed_session_context_from(
        {undefined, undefined, undefined, Sink}
    ),
    ?assertEqual(Sink, group_leader()),
    ok = Restore(),
    ?assertEqual(GL, group_leader()),
    ?assertEqual(Sink, get(beamtalk_entry_group_leader)),
    Sink ! stop,
    clear().

malformed_context_is_noop_test() ->
    clear(),
    Restore = beamtalk_class_session:seed_session_context_from(not_a_tuple),
    ?assertEqual(ok, Restore()),
    ?assertEqual([{K, undefined} || K <- keys()], snapshot()).
