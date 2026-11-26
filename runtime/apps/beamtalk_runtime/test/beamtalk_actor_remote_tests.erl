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

%%% -----------------------------------------------------------------------
%%% Origin-side error paths that need no live peer
%%%
%%% These run on a non-distributed test node (TLS distribution off), so
%%% `connect_policy/2` refuses any off-host node and answers `ok` for a
%%% `localhost` peer; an `erpc` to a `localhost` peer that is not running
%%% then fails with `noconnection`, i.e. the node-down path.
%%% -----------------------------------------------------------------------

-define(OFF_HOST, 'peer@remote.invalid').
-define(DOWN_PEER, 'bt_actor_remote_absent@localhost').

remote_spawn_refused_off_host_is_tagged_with_class_and_selector_test() ->
    ?assertMatch(
        {error, #beamtalk_error{
            kind = insecure_distribution, class = 'Counter', selector = 'spawnOn:'
        }},
        beamtalk_actor_remote:remote_spawn(?OFF_HOST, 'Counter', undefined, 'spawnOn:')
    ).

remote_spawn_with_refused_before_encoding_test() ->
    %% A refused connection is reported without running the wire-encode, so
    %% an unencodable InitArgs still yields the policy error.
    ?assertMatch(
        {error, #beamtalk_error{
            kind = insecure_distribution, class = 'Counter', selector = 'spawnWith:on:'
        }},
        beamtalk_actor_remote:remote_spawn_with(
            ?OFF_HOST, 'Counter', undefined, too_deep(), 'spawnWith:on:'
        )
    ).

remote_spawn_with_encode_failure_is_tagged_and_never_reaches_network_test() ->
    %% Peer is on-host (policy allows it) and absent: if encoding did not fail
    %% first, the result would be node_down rather than not_serialisable.
    ?assertMatch(
        {error, #beamtalk_error{
            kind = not_serialisable, class = 'Counter', selector = 'spawnWith:on:'
        }},
        beamtalk_actor_remote:remote_spawn_with(
            ?DOWN_PEER, 'Counter', undefined, too_deep(), 'spawnWith:on:'
        )
    ).

remote_spawn_node_down_test() ->
    {error, Err} = beamtalk_actor_remote:remote_spawn(
        ?DOWN_PEER, 'Counter', undefined, 'spawnOn:'
    ),
    ?assertEqual(beamtalk_actor_send:node_down_error_record(?DOWN_PEER, 'spawnOn:'), Err).

remote_spawn_with_node_down_test() ->
    {error, Err} = beamtalk_actor_remote:remote_spawn_with(
        ?DOWN_PEER, 'Counter', 'worker', #{a => 1}, 'spawnWith:as:on:'
    ),
    ?assertEqual(
        beamtalk_actor_send:node_down_error_record(?DOWN_PEER, 'spawnWith:as:on:'), Err
    ).

remote_named_refused_off_host_test() ->
    ?assertMatch(
        {error, #beamtalk_error{
            kind = insecure_distribution, class = 'Counter', selector = 'named:on:'
        }},
        beamtalk_actor_remote:remote_named(?OFF_HOST, 'Counter', worker)
    ).

remote_named_node_down_test() ->
    {error, Err} = beamtalk_actor_remote:remote_named(?DOWN_PEER, 'Counter', worker),
    ?assertEqual(beamtalk_actor_send:node_down_error_record(?DOWN_PEER, 'named:on:'), Err).

remote_all_registered_refused_off_host_test() ->
    ?assertMatch(
        {error, #beamtalk_error{
            kind = insecure_distribution, class = 'Actor', selector = 'allRegisteredOn:'
        }},
        beamtalk_actor_remote:remote_all_registered(?OFF_HOST)
    ).

remote_all_registered_node_down_test() ->
    {error, Err} = beamtalk_actor_remote:remote_all_registered(?DOWN_PEER),
    ?assertEqual(
        beamtalk_actor_send:node_down_error_record(?DOWN_PEER, 'allRegisteredOn:'), Err
    ).

%%% -----------------------------------------------------------------------
%%% Target-side error paths (run locally; the erpc worker is just a process)
%%% -----------------------------------------------------------------------

remote_spawn_target_unknown_class_test() ->
    ?assertMatch(
        {error, #beamtalk_error{kind = class_not_found, class = 'BtNoSuchRemoteClass'}},
        beamtalk_actor_remote:remote_spawn_target('BtNoSuchRemoteClass', undefined)
    ),
    %% The class is resolved before the name is considered.
    ?assertMatch(
        {error, #beamtalk_error{kind = class_not_found}},
        beamtalk_actor_remote:remote_spawn_target('BtNoSuchRemoteClass', worker)
    ).

remote_spawn_with_target_unknown_class_test() ->
    ?assertMatch(
        {error, #beamtalk_error{kind = class_not_found, class = 'BtNoSuchRemoteClass'}},
        beamtalk_actor_remote:remote_spawn_with_target('BtNoSuchRemoteClass', undefined, #{a => 1})
    ).

remote_spawn_with_target_undecodable_args_test() ->
    %% A class ref naming a class this node does not have fails the decode,
    %% before the target class is even looked up.
    ?assertMatch(
        {error, #beamtalk_error{kind = class_not_found, class = 'BtNoSuchArgClass'}},
        beamtalk_actor_remote:remote_spawn_with_target(
            'BtNoSuchRemoteClass', undefined, {'$beamtalk_class_ref', 'BtNoSuchArgClass'}
        )
    ).

remote_named_target_unknown_class_test() ->
    Result = beamtalk_actor_remote:remote_named_target('BtNoSuchRemoteClass', bt_no_such_name),
    ?assertMatch({error, #beamtalk_error{}}, Result).

%%% -----------------------------------------------------------------------
%%% Helpers
%%% -----------------------------------------------------------------------

%% A list nested deeper than the wire walk's recursion cap (not_serialisable).
too_deep() ->
    lists:foldl(
        fun(_, Acc) -> [Acc] end,
        [],
        lists:seq(1, beamtalk_shape_migration:max_pack_depth() + 1)
    ).
