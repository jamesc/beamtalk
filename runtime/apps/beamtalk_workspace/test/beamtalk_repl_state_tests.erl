%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_state_tests).

-moduledoc """
Unit tests for beamtalk_repl_state module

Tests REPL state management: bindings, counters, loaded modules, and configuration.
""".
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests
%%====================================================================

%%% State creation tests

new_default_test() ->
    State = beamtalk_repl_state:new(undefined, 1234),
    ?assertEqual(undefined, beamtalk_repl_state:get_listen_socket(State)),
    ?assertEqual(1234, beamtalk_repl_state:get_port(State)),
    ?assertEqual(#{}, beamtalk_repl_state:get_bindings(State)),
    ?assertEqual(0, beamtalk_repl_state:get_eval_counter(State)),
    ?assertEqual([], beamtalk_repl_state:get_loaded_modules(State)).

new_with_socket_test() ->
    %% Create a dummy socket (self() will work for testing)
    MockSocket = self(),
    State = beamtalk_repl_state:new(MockSocket, 5678),
    ?assertEqual(MockSocket, beamtalk_repl_state:get_listen_socket(State)),
    ?assertEqual(5678, beamtalk_repl_state:get_port(State)).

%%% Session origin/debug metadata tests

new_default_client_meta_test() ->
    %% Absent options → metadata defaults to a map with kind => unknown.
    State = beamtalk_repl_state:new(undefined, 0),
    ?assertEqual(#{kind => <<"unknown">>}, beamtalk_repl_state:get_client_meta(State)),
    ?assertEqual(<<"unknown">>, beamtalk_repl_state:get_client_kind(State)).

new_with_client_meta_test() ->
    %% client_meta option is stored and surfaced; kind is read from it.
    Meta = #{kind => <<"repl">>, peer => <<"127.0.0.1:5000">>},
    State = beamtalk_repl_state:new(undefined, 0, #{client_meta => Meta}),
    ?assertEqual(Meta, beamtalk_repl_state:get_client_meta(State)),
    ?assertEqual(<<"repl">>, beamtalk_repl_state:get_client_kind(State)).

new_client_meta_without_kind_defaults_kind_test() ->
    %% A metadata map missing `kind` is normalised to include kind => unknown.
    State = beamtalk_repl_state:new(undefined, 0, #{client_meta => #{peer => <<"h:1">>}}),
    ?assertEqual(<<"unknown">>, beamtalk_repl_state:get_client_kind(State)),
    ?assertEqual(
        #{kind => <<"unknown">>, peer => <<"h:1">>}, beamtalk_repl_state:get_client_meta(State)
    ).

new_client_meta_drops_undefined_values_test() ->
    %% Keys carrying `undefined` (e.g. a ws peer that could not be formatted) are
    %% pruned so `Session info` never surfaces an `undefined` value. Known keys
    %% and a normalised `kind` survive.
    Meta = #{kind => <<"repl">>, peer => undefined, node => undefined, user => <<"alice">>},
    State = beamtalk_repl_state:new(undefined, 0, #{client_meta => Meta}),
    ?assertEqual(
        #{kind => <<"repl">>, user => <<"alice">>}, beamtalk_repl_state:get_client_meta(State)
    ).

%%% Bindings tests

get_set_bindings_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    ?assertEqual(#{}, beamtalk_repl_state:get_bindings(State)),

    Bindings = #{x => 1, y => 2},
    State2 = beamtalk_repl_state:set_bindings(Bindings, State),
    ?assertEqual(Bindings, beamtalk_repl_state:get_bindings(State2)).

clear_bindings_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    State2 = beamtalk_repl_state:set_bindings(#{x => 1, y => 2}, State),
    ?assertNotEqual(#{}, beamtalk_repl_state:get_bindings(State2)),

    State3 = beamtalk_repl_state:clear_bindings(State2),
    ?assertEqual(#{}, beamtalk_repl_state:get_bindings(State3)).

bindings_immutability_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    State2 = beamtalk_repl_state:set_bindings(#{x => 1}, State),

    %% Original state should be unchanged
    ?assertEqual(#{}, beamtalk_repl_state:get_bindings(State)),
    ?assertEqual(#{x => 1}, beamtalk_repl_state:get_bindings(State2)).

%%% Eval counter tests

eval_counter_starts_at_zero_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    ?assertEqual(0, beamtalk_repl_state:get_eval_counter(State)).

increment_eval_counter_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    State2 = beamtalk_repl_state:increment_eval_counter(State),
    ?assertEqual(1, beamtalk_repl_state:get_eval_counter(State2)),

    State3 = beamtalk_repl_state:increment_eval_counter(State2),
    ?assertEqual(2, beamtalk_repl_state:get_eval_counter(State3)).

eval_counter_immutability_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    _State2 = beamtalk_repl_state:increment_eval_counter(State),

    %% Original state should be unchanged
    ?assertEqual(0, beamtalk_repl_state:get_eval_counter(State)).

eval_counter_many_increments_test() ->
    State = lists:foldl(
        fun(_, S) -> beamtalk_repl_state:increment_eval_counter(S) end,
        beamtalk_repl_state:new(undefined, 0),
        lists:seq(1, 100)
    ),
    ?assertEqual(100, beamtalk_repl_state:get_eval_counter(State)).

%%% Loaded modules tests

loaded_modules_starts_empty_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    ?assertEqual([], beamtalk_repl_state:get_loaded_modules(State)).

add_loaded_module_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    State2 = beamtalk_repl_state:add_loaded_module(counter, State),
    ?assertEqual([counter], beamtalk_repl_state:get_loaded_modules(State2)).

add_multiple_loaded_modules_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    State2 = beamtalk_repl_state:add_loaded_module(counter, State),
    State3 = beamtalk_repl_state:add_loaded_module(point, State2),
    State4 = beamtalk_repl_state:add_loaded_module(actor, State3),

    Modules = beamtalk_repl_state:get_loaded_modules(State4),
    ?assertEqual(3, length(Modules)),
    ?assert(lists:member(counter, Modules)),
    ?assert(lists:member(point, Modules)),
    ?assert(lists:member(actor, Modules)).

loaded_modules_immutability_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    _State2 = beamtalk_repl_state:add_loaded_module(counter, State),

    %% Original state should be unchanged
    ?assertEqual([], beamtalk_repl_state:get_loaded_modules(State)).

loaded_modules_order_test() ->
    %% Modules are added to the front of the list
    State = beamtalk_repl_state:new(undefined, 0),
    State2 = beamtalk_repl_state:add_loaded_module(mod1, State),
    State3 = beamtalk_repl_state:add_loaded_module(mod2, State2),
    State4 = beamtalk_repl_state:add_loaded_module(mod3, State3),

    ?assertEqual([mod3, mod2, mod1], beamtalk_repl_state:get_loaded_modules(State4)).

%%% Configuration tests

listen_socket_test() ->
    MockSocket = self(),
    State = beamtalk_repl_state:new(MockSocket, 1234),
    ?assertEqual(MockSocket, beamtalk_repl_state:get_listen_socket(State)).

port_test() ->
    State = beamtalk_repl_state:new(undefined, 9876),
    ?assertEqual(9876, beamtalk_repl_state:get_port(State)).

%%% State composition tests

complex_state_test() ->
    %% Test a state with all fields set
    State = beamtalk_repl_state:new(self(), 1234),
    State2 = beamtalk_repl_state:set_bindings(#{x => 1, y => 2}, State),
    State3 = beamtalk_repl_state:increment_eval_counter(State2),
    State4 = beamtalk_repl_state:increment_eval_counter(State3),
    State5 = beamtalk_repl_state:add_loaded_module(counter, State4),
    State6 = beamtalk_repl_state:add_loaded_module(point, State5),

    %% Verify all fields
    ?assertEqual(self(), beamtalk_repl_state:get_listen_socket(State6)),
    ?assertEqual(1234, beamtalk_repl_state:get_port(State6)),
    ?assertEqual(#{x => 1, y => 2}, beamtalk_repl_state:get_bindings(State6)),
    ?assertEqual(2, beamtalk_repl_state:get_eval_counter(State6)),
    ?assertEqual([point, counter], beamtalk_repl_state:get_loaded_modules(State6)).

%%% BT-2366: pending session-local mutations (ADR 0081 Phase 2)

pending_mutations_starts_empty_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    ?assertEqual([], beamtalk_repl_state:get_pending_mutations(State)).

add_pending_mutation_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    State2 = beamtalk_repl_state:add_pending_mutation({put, x, 1}, State),
    ?assertEqual([{put, x, 1}], beamtalk_repl_state:get_pending_mutations(State2)).

pending_mutations_preserve_enqueue_order_test() ->
    %% Newest is appended at the tail so the queue replays in issue order.
    State0 = beamtalk_repl_state:new(undefined, 0),
    State1 = beamtalk_repl_state:add_pending_mutation({put, x, 1}, State0),
    State2 = beamtalk_repl_state:add_pending_mutation({remove, y, undefined}, State1),
    State3 = beamtalk_repl_state:add_pending_mutation({clear, undefined, undefined}, State2),
    ?assertEqual(
        [{put, x, 1}, {remove, y, undefined}, {clear, undefined, undefined}],
        beamtalk_repl_state:get_pending_mutations(State3)
    ).

clear_pending_mutations_test() ->
    State0 = beamtalk_repl_state:new(undefined, 0),
    State1 = beamtalk_repl_state:add_pending_mutation({put, x, 1}, State0),
    State2 = beamtalk_repl_state:clear_pending_mutations(State1),
    ?assertEqual([], beamtalk_repl_state:get_pending_mutations(State2)).

pending_mutations_immutability_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    _State2 = beamtalk_repl_state:add_pending_mutation({put, x, 1}, State),
    ?assertEqual([], beamtalk_repl_state:get_pending_mutations(State)).

state_independence_test() ->
    %% Test that operations on one state don't affect other states
    State1 = beamtalk_repl_state:new(undefined, 0),
    State2 = beamtalk_repl_state:set_bindings(#{x => 1}, State1),
    State3 = beamtalk_repl_state:increment_eval_counter(State1),

    %% All three states should be independent
    ?assertEqual(#{}, beamtalk_repl_state:get_bindings(State1)),
    ?assertEqual(0, beamtalk_repl_state:get_eval_counter(State1)),

    ?assertEqual(#{x => 1}, beamtalk_repl_state:get_bindings(State2)),
    ?assertEqual(0, beamtalk_repl_state:get_eval_counter(State2)),

    ?assertEqual(#{}, beamtalk_repl_state:get_bindings(State3)),
    ?assertEqual(1, beamtalk_repl_state:get_eval_counter(State3)).

%%% BT-2938: fresh-session seeding from stdlib's own compiled aliases

%% Temporarily set `beamtalk_stdlib`'s `type_aliases` env to `Aliases`, run
%% `Fun`, then restore the original env (`undefined` → unset). Mirrors
%% `beamtalk_repl_ops_browse_tests:with_stdlib_aliases/2` — stdlib is the one
%% app always loaded under the EUnit harness, and `application:set_env/3` on
%% it is the minimal, precedent-consistent way to fixture its alias env
%% without building a real `.app` file end-to-end.
with_stdlib_aliases(Aliases, Fun) ->
    _ = application:load(beamtalk_stdlib),
    Previous = application:get_env(beamtalk_stdlib, type_aliases),
    ok = application:set_env(beamtalk_stdlib, type_aliases, Aliases),
    try
        Fun()
    after
        case Previous of
            {ok, V} -> application:set_env(beamtalk_stdlib, type_aliases, V);
            undefined -> application:unset_env(beamtalk_stdlib, type_aliases)
        end
    end.

stdlib_alias_seeded_into_fresh_session_test() ->
    with_stdlib_aliases(
        [
            #{
                name => 'SupervisionStrategy',
                expansion => "#oneForOne | #oneForAll | #restForOne",
                doc => "Restart strategy for a supervised child.",
                source_file => "stdlib/src/Supervisor.bt",
                internal => false
            }
        ],
        fun() ->
            State = beamtalk_repl_state:new(undefined, 0),
            AliasTable = beamtalk_repl_state:get_alias_table(State),
            ?assertEqual(
                #{
                    expansion => <<"#oneForOne | #oneForAll | #restForOne">>,
                    doc_comment => <<"Restart strategy for a supervised child.">>,
                    declared_in => <<"stdlib">>
                },
                maps:get(<<"SupervisionStrategy">>, AliasTable)
            )
        end
    ).

%% `known_type_alias_sources/1` folds every seeded alias (stdlib included)
%% into the compiler-port `known_type_aliases` request field — this is what
%% makes a `::`-typed local referencing a stdlib alias resolve.
stdlib_alias_seeded_reaches_known_type_alias_sources_test() ->
    with_stdlib_aliases(
        [
            #{
                name => 'SupervisionStrategy',
                expansion => "#oneForOne | #oneForAll | #restForOne",
                doc => undefined,
                source_file => "stdlib/src/Supervisor.bt",
                internal => false
            }
        ],
        fun() ->
            State = beamtalk_repl_state:new(undefined, 0),
            Sources = beamtalk_repl_state:known_type_alias_sources(State),
            ?assertEqual(
                [<<"type SupervisionStrategy = #oneForOne | #oneForAll | #restForOne">>], Sources
            )
        end
    ).

stdlib_alias_without_doc_comment_test() ->
    with_stdlib_aliases(
        [
            #{
                name => 'LogFormat',
                expansion => "#text | #json",
                doc => undefined,
                source_file => "stdlib/src/BeamtalkInterface.bt",
                internal => false
            }
        ],
        fun() ->
            State = beamtalk_repl_state:new(undefined, 0),
            Entry = maps:get(<<"LogFormat">>, beamtalk_repl_state:get_alias_table(State)),
            ?assertEqual(undefined, maps:get(doc_comment, Entry))
        end
    ).

%% Seeding-boundary exclusion (mirrors `beamtalk_repl_ops_browse:
%% alias_visible/2`): an `internal type Foo = ...` stdlib alias is never
%% seeded into a session — a REPL session is never "inside" the stdlib
%% package.
stdlib_internal_alias_excluded_test() ->
    with_stdlib_aliases(
        [
            #{
                name => 'InternalOnlyStrategy',
                expansion => "#a | #b",
                doc => undefined,
                source_file => "stdlib/src/Fixture.bt",
                internal => true
            }
        ],
        fun() ->
            State = beamtalk_repl_state:new(undefined, 0),
            AliasTable = beamtalk_repl_state:get_alias_table(State),
            ?assertNot(maps:is_key(<<"InternalOnlyStrategy">>, AliasTable))
        end
    ).

%% Fail-safe (not fail-open) on a row missing the `internal` key entirely —
%% mirrors `beamtalk_repl_ops_browse:alias_visible/2`'s convention. Should
%% never happen against real `format_type_aliases_entry` output (which
%% always emits `internal`), but a row shape this reader doesn't recognise
%% must not default to seeding it as public.
stdlib_alias_missing_internal_key_excluded_test() ->
    with_stdlib_aliases(
        [
            #{
                name => 'NoInternalKeyStrategy',
                expansion => "#a | #b",
                doc => undefined,
                source_file => "stdlib/src/Fixture.bt"
            }
        ],
        fun() ->
            State = beamtalk_repl_state:new(undefined, 0),
            AliasTable = beamtalk_repl_state:get_alias_table(State),
            ?assertNot(maps:is_key(<<"NoInternalKeyStrategy">>, AliasTable))
        end
    ).

%% A non-boolean `internal` value (hand-edited/corrupted `.app` file) must
%% only drop this one row, not wipe every other well-formed alias in the
%% same env — the `case` in `stdlib_alias_fold/2` needs its own catch-all
%% arm, not just the outer `try/catch` in `stdlib_alias_table/0`, or a
%% `case_clause` here would surface as an empty table (review finding,
%% Claude BeamTalk Review on this PR).
stdlib_alias_non_boolean_internal_drops_only_that_row_test() ->
    with_stdlib_aliases(
        [
            #{
                name => 'GoodStrategy',
                expansion => "#a | #b",
                doc => undefined,
                source_file => "stdlib/src/Fixture.bt",
                internal => false
            },
            #{
                name => 'BadInternalStrategy',
                expansion => "#c | #d",
                doc => undefined,
                source_file => "stdlib/src/Fixture.bt",
                internal => not_a_boolean
            }
        ],
        fun() ->
            State = beamtalk_repl_state:new(undefined, 0),
            AliasTable = beamtalk_repl_state:get_alias_table(State),
            ?assert(maps:is_key(<<"GoodStrategy">>, AliasTable)),
            ?assertNot(maps:is_key(<<"BadInternalStrategy">>, AliasTable))
        end
    ).

%% Same row-level-isolation guarantee, this time for a field so malformed
%% that `app_env_binary`/`app_env_doc` themselves throw (e.g. a `name` that
%% is neither atom, binary, nor list) — not just a bad `internal` value.
%% The inner `try/catch` in `stdlib_alias_fold/2` must catch this too, or
%% it escapes to `stdlib_alias_table/0`'s outer `try/catch` and wipes every
%% other well-formed row (review finding, Claude BeamTalk Review on this
%% PR).
stdlib_alias_unconvertible_field_drops_only_that_row_test() ->
    with_stdlib_aliases(
        [
            #{
                name => 'GoodStrategy',
                expansion => "#a | #b",
                doc => undefined,
                source_file => "stdlib/src/Fixture.bt",
                internal => false
            },
            #{
                %% Neither atom, binary, nor list — app_env_binary/1 has no
                %% catch-all clause for this, so it throws function_clause.
                name => 42,
                expansion => "#c | #d",
                doc => undefined,
                source_file => "stdlib/src/Fixture.bt",
                internal => false
            }
        ],
        fun() ->
            State = beamtalk_repl_state:new(undefined, 0),
            AliasTable = beamtalk_repl_state:get_alias_table(State),
            ?assert(maps:is_key(<<"GoodStrategy">>, AliasTable))
        end
    ).

%% A session-declared `type Name = ...` legally shadows a same-named stdlib
%% entry (ADR 0108 Semantics' "current turn wins" precedent) rather than
%% erroring or being ignored.
session_declared_alias_shadows_stdlib_entry_test() ->
    with_stdlib_aliases(
        [
            #{
                name => 'SupervisionStrategy',
                expansion => "#oneForOne | #oneForAll | #restForOne",
                doc => undefined,
                source_file => "stdlib/src/Supervisor.bt",
                internal => false
            }
        ],
        fun() ->
            State0 = beamtalk_repl_state:new(undefined, 0),
            SessionEntry = #{
                expansion => <<"#custom">>, doc_comment => undefined, declared_in => <<"REPL">>
            },
            State1 = beamtalk_repl_state:put_alias(<<"SupervisionStrategy">>, SessionEntry, State0),
            ?assertEqual(
                SessionEntry,
                maps:get(<<"SupervisionStrategy">>, beamtalk_repl_state:get_alias_table(State1))
            )
        end
    ).

%% Best-effort: a malformed `type_aliases` env (not even a list) seeds an
%% empty table rather than crashing session creation.
stdlib_alias_table_malformed_env_is_empty_test() ->
    with_stdlib_aliases(
        not_a_list,
        fun() ->
            State = beamtalk_repl_state:new(undefined, 0),
            ?assertEqual(#{}, beamtalk_repl_state:get_alias_table(State))
        end
    ).

%% No `type_aliases` env key at all (older `.app` build, or a package with
%% no aliases) — same empty-table fallback, no crash.
stdlib_alias_table_missing_env_is_empty_test() ->
    _ = application:load(beamtalk_stdlib),
    Previous = application:get_env(beamtalk_stdlib, type_aliases),
    application:unset_env(beamtalk_stdlib, type_aliases),
    try
        State = beamtalk_repl_state:new(undefined, 0),
        ?assertEqual(#{}, beamtalk_repl_state:get_alias_table(State))
    after
        case Previous of
            {ok, V} -> application:set_env(beamtalk_stdlib, type_aliases, V);
            undefined -> ok
        end
    end.
