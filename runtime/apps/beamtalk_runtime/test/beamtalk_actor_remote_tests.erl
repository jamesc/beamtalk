%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_actor_remote_tests).

%%% **DDD Context:** Actor System Context

-moduledoc """
Unit tests for the pure helper functions in `beamtalk_actor_remote`
(ADR 0126 §3, Phase 2+).

The module's distributed happy paths are exercised by the two-node EUnit
suite `beamtalk_dist_remote_spawn_tests` (which needs a live peer). This
file covers the error-construction and argument-conversion helpers that do
not require a live remote node — all four were at 0% in the nightly
coverage report (run 43875698232, 2026-11-16).
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% -----------------------------------------------------------------------
%%% remote_timeout_error_record/3 — idempotent=true arm (line 354)
%%% -----------------------------------------------------------------------

%% The `false` arm is already hit by the two-node spawn-timeout tests.
%% The `true` arm (read-only/retryable lookups) was completely uncovered.
remote_timeout_error_record_idempotent_true_test() ->
    Err = beamtalk_actor_remote:remote_timeout_error_record('Actor', 'named:on:', true),
    ?assertEqual(timeout, Err#beamtalk_error.kind),
    ?assertEqual('Actor', Err#beamtalk_error.class),
    ?assertEqual('named:on:', Err#beamtalk_error.selector),
    %% The hint must name it safe to retry (not the non-idempotent warning).
    {beamtalk_string, Hint} = Err#beamtalk_error.hint,
    ?assertNotEqual(nomatch, binary:match(Hint, <<"safe to retry">>)).

remote_timeout_error_record_idempotent_false_test() ->
    Err = beamtalk_actor_remote:remote_timeout_error_record('Counter', 'spawnOn:', false),
    ?assertEqual(timeout, Err#beamtalk_error.kind),
    ?assertEqual('Counter', Err#beamtalk_error.class),
    ?assertEqual('spawnOn:', Err#beamtalk_error.selector),
    {beamtalk_string, Hint} = Err#beamtalk_error.hint,
    ?assertNotEqual(nomatch, binary:match(Hint, <<"not guaranteed to be safe">>)).

%%% -----------------------------------------------------------------------
%%% node_arg_to_atom/1 — both clauses (lines 365, 367)
%%% -----------------------------------------------------------------------

node_arg_to_atom_valid_node_map_test() ->
    NodeAtom = 'peer@example',
    Result = beamtalk_actor_remote:node_arg_to_atom(
        #{'$beamtalk_class' => 'Node', name => NodeAtom}
    ),
    ?assertEqual({ok, NodeAtom}, Result).

node_arg_to_atom_non_node_value_returns_type_error_test() ->
    Result = beamtalk_actor_remote:node_arg_to_atom(not_a_node),
    ?assertMatch({error, #beamtalk_error{kind = type_error}}, Result),
    {error, Err} = Result,
    %% The hint must name the unexpected value.
    {beamtalk_string, Hint} = Err#beamtalk_error.hint,
    ?assertNotEqual(nomatch, binary:match(Hint, <<"Expected a Node value">>)).

node_arg_to_atom_wrong_class_returns_type_error_test() ->
    %% A map that looks similar but is not a 'Node' class value.
    Result = beamtalk_actor_remote:node_arg_to_atom(
        #{'$beamtalk_class' => 'String', name => 'peer@example'}
    ),
    ?assertMatch({error, #beamtalk_error{kind = type_error}}, Result).
