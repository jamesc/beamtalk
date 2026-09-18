%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Hot Reload Context

-module(beamtalk_shape_chain_tests).

-moduledoc """
EUnit tests for beamtalk_shape_chain (ADR 0123 Phase 2, BT-3536).

Pure leaf module — no class registry, no `__beamtalk_meta`, no process.
Every test here supplies its own fake `Invoke` fun.
""".
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Chain order (v1 -> v2 -> v3)
%%====================================================================

chain_applies_steps_in_order_test() ->
    Migrations = #{
        1 => 'migrateFromV1:',
        2 => 'migrateFromV2:'
    },
    Invoke = fun
        ('migrateFromV1:', Fields) -> Fields#{step1 => true};
        ('migrateFromV2:', Fields) -> Fields#{step2 => true}
    end,
    {ok, NewFields} = beamtalk_shape_chain:migrate(Migrations, {1, 3}, #{}, Invoke),
    ?assertEqual(#{step1 => true, step2 => true}, NewFields).

%% Each step sees the accumulated dictionary the prior step left, not the
%% original Fields — a step's own selector confirms which invocation it was.
chain_threads_accumulated_dictionary_through_steps_test() ->
    Migrations = #{1 => 'migrateFromV1:', 2 => 'migrateFromV2:'},
    Invoke = fun(Selector, Fields) ->
        Seen = maps:get(seen, Fields, []),
        Fields#{seen => Seen ++ [Selector]}
    end,
    {ok, NewFields} = beamtalk_shape_chain:migrate(Migrations, {1, 3}, #{}, Invoke),
    ?assertEqual(['migrateFromV1:', 'migrateFromV2:'], maps:get(seen, NewFields)).

%%====================================================================
%% Gaps — a step with no table entry is a no-op
%%====================================================================

chain_gap_step_is_a_no_op_test() ->
    %% Only v2 has a declared migration; v1 and v3 are gaps.
    Migrations = #{2 => 'migrateFromV2:'},
    Invoke = fun('migrateFromV2:', Fields) -> Fields#{total => 40} end,
    {ok, NewFields} = beamtalk_shape_chain:migrate(Migrations, {1, 4}, #{items => 4}, Invoke),
    ?assertEqual(#{items => 4, total => 40}, NewFields).

%% A chain with no declared migrations at all is a pure identity walk —
%% every step is a gap. This module never defaults or drops fields itself
%% (that is beamtalk_shape_migration's reconcile job); a gap step leaves the
%% dictionary bit-for-bit unchanged.
chain_all_gaps_leaves_fields_unchanged_test() ->
    Invoke = fun(_Selector, _Fields) -> error(should_not_be_called) end,
    {ok, NewFields} = beamtalk_shape_chain:migrate(#{}, {1, 5}, #{a => 1}, Invoke),
    ?assertEqual(#{a => 1}, NewFields).

%%====================================================================
%% Downgrade (To < From) and idempotency (To =:= From)
%%====================================================================

chain_downgrade_runs_no_steps_test() ->
    Invoke = fun(_Selector, _Fields) -> error(should_not_be_called) end,
    {ok, NewFields} = beamtalk_shape_chain:migrate(
        #{1 => 'migrateFromV1:'}, {3, 1}, #{value => 9}, Invoke
    ),
    ?assertEqual(#{value => 9}, NewFields).

chain_idempotent_when_already_current_test() ->
    Invoke = fun(_Selector, _Fields) -> error(should_not_be_called) end,
    {ok, NewFields} = beamtalk_shape_chain:migrate(
        #{1 => 'migrateFromV1:'}, {2, 2}, #{value => 9}, Invoke
    ),
    ?assertEqual(#{value => 9}, NewFields).

%%====================================================================
%% Step failure
%%====================================================================

%% A hook result that is not a map fails the step — the chain halts, the
%% remaining (unreached) step never runs.
chain_non_dictionary_result_fails_the_step_test() ->
    Migrations = #{1 => 'migrateFromV1:', 2 => 'migrateFromV2:'},
    Invoke = fun
        ('migrateFromV1:', _Fields) -> not_a_map;
        ('migrateFromV2:', _Fields) -> error(should_not_be_called)
    end,
    {error, {1, 'migrateFromV1:', Reason}} =
        beamtalk_shape_chain:migrate(Migrations, {1, 3}, #{}, Invoke),
    ?assertMatch({non_dictionary_result, not_a_map}, Reason).

%% A raising hook is caught and turned into a step failure, not propagated.
chain_raising_hook_fails_the_step_test() ->
    Migrations = #{1 => 'migrateFromV1:'},
    Invoke = fun('migrateFromV1:', _Fields) -> error(boom) end,
    {error, {1, 'migrateFromV1:', Reason}} =
        beamtalk_shape_chain:migrate(Migrations, {1, 2}, #{}, Invoke),
    ?assertMatch({error, boom}, Reason).

%% An Invoke that returns {error, Reason} directly (rather than raising)
%% is also a step failure, with the reason passed through unwrapped.
chain_invoke_error_return_fails_the_step_test() ->
    Migrations = #{1 => 'migrateFromV1:'},
    Invoke = fun('migrateFromV1:', _Fields) -> {error, not_registered} end,
    {error, {1, 'migrateFromV1:', not_registered}} =
        beamtalk_shape_chain:migrate(Migrations, {1, 2}, #{}, Invoke).

%% A gap step after a failed step never runs — the chain halts immediately.
chain_failure_halts_remaining_steps_test() ->
    Migrations = #{1 => 'migrateFromV1:', 3 => 'migrateFromV3:'},
    Invoke = fun
        ('migrateFromV1:', _Fields) -> error(boom);
        ('migrateFromV3:', _Fields) -> error(should_not_be_called)
    end,
    {error, {1, 'migrateFromV1:', _}} =
        beamtalk_shape_chain:migrate(Migrations, {1, 4}, #{}, Invoke).
