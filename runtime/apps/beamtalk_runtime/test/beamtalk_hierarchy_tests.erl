%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Object System Context

-module(beamtalk_hierarchy_tests).

-moduledoc """
Unit tests for `beamtalk_hierarchy:walk_ancestors/3` — the generic
depth-guarded ancestor-chain walker (BT-2786).

Every current caller (`beamtalk_dispatch`, `beamtalk_class_dispatch`,
`beamtalk_method_resolver`, `beamtalk_behaviour_intrinsics`, `beamtalk_xref`,
`beamtalk_hierarchy_docs`) only exercises this module indirectly through a
real class hierarchy backed by live class gen_server processes, so the
generic walk contract — depth counting, the `none`-terminates-immediately
short circuit, and the `max_depth_exceeded` cycle guard returning the
*next* unvisited node rather than the last visited one — has never been
pinned down directly against the pure function itself. These tests exercise
`walk_ancestors/3` standalone, with plain atoms as nodes and no runtime
process involved, so the module's contract is documented and protected
independently of any particular caller's own test coverage.
""".

-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% StartNode = none — walk never invokes StepFun
%%% ============================================================================

start_node_none_test() ->
    StepFun = fun(_Node, _Depth) -> error(step_fun_should_not_be_called) end,
    ?assertEqual(not_found, beamtalk_hierarchy:walk_ancestors(none, StepFun, 10)).

%%% ============================================================================
%%% {found, Result} — stops immediately, propagates Result
%%% ============================================================================

found_at_start_node_test() ->
    StepFun = fun(a, 0) -> {found, hit} end,
    ?assertEqual({found, hit}, beamtalk_hierarchy:walk_ancestors(a, StepFun, 10)).

found_after_several_next_hops_test() ->
    Chain = #{a => b, b => c, c => d},
    StepFun = fun(Node, _Depth) ->
        case Node of
            d -> {found, reached_d};
            _ -> {next, maps:get(Node, Chain)}
        end
    end,
    ?assertEqual({found, reached_d}, beamtalk_hierarchy:walk_ancestors(a, StepFun, 10)).

%%% ============================================================================
%%% not_found — StepFun gives up, or the chain runs out via `next, none`
%%% ============================================================================

not_found_reported_by_step_fun_test() ->
    StepFun = fun(a, 0) -> not_found end,
    ?assertEqual(not_found, beamtalk_hierarchy:walk_ancestors(a, StepFun, 10)).

not_found_when_chain_ends_at_none_test() ->
    Chain = #{a => b, b => none},
    StepFun = fun(Node, _Depth) ->
        case maps:find(Node, Chain) of
            {ok, Next} -> {next, Next};
            error -> not_found
        end
    end,
    ?assertEqual(not_found, beamtalk_hierarchy:walk_ancestors(a, StepFun, 10)).

%%% ============================================================================
%%% Depth argument — StepFun sees the 0-based depth of the node it is visiting
%%% ============================================================================

depth_argument_increments_per_hop_test() ->
    Self = self(),
    StepFun = fun(Node, Depth) ->
        Self ! {visited, Node, Depth},
        case Node of
            c -> {found, done};
            a -> {next, b};
            b -> {next, c}
        end
    end,
    {found, done} = beamtalk_hierarchy:walk_ancestors(a, StepFun, 10),
    %% Selective receive on the `{visited, _, _}` tag only: this test process's
    %% mailbox is shared with the rest of the EUnit node, which may deliver
    %% unrelated messages (e.g. a stray monitor 'DOWN' from a concurrently
    %% running suite) — a bare `receive Msg -> Msg end` would wrongly grab one
    %% of those instead of skipping over it.
    ?assertEqual({visited, a, 0}, receive_visited()),
    ?assertEqual({visited, b, 1}, receive_visited()),
    ?assertEqual({visited, c, 2}, receive_visited()).

receive_visited() ->
    receive
        {visited, _, _} = Msg -> Msg
    after 0 -> timeout
    end.

%%% ============================================================================
%%% max_depth_exceeded — guard trips before StepFun is invoked on LastNode
%%% ============================================================================

max_depth_exceeded_on_infinite_chain_test() ->
    %% StepFun always advances to the next integer — an unbounded "chain" —
    %% so the only way the walk stops is the depth guard. With MaxDepth=3,
    %% StepFun runs at depths 0..3 (node 0..3), each advancing the node by
    %% one; the guard trips on the depth-4 node (4 > 3) before StepFun is
    %% ever invoked on it, so LastNode is 4, not 3.
    StepFun = fun(Node, _Depth) -> {next, Node + 1} end,
    ?assertEqual({max_depth_exceeded, 4}, beamtalk_hierarchy:walk_ancestors(0, StepFun, 3)).

max_depth_exceeded_on_self_cycle_test() ->
    %% A direct cycle (a -> a -> a -> ...), the shape a buggy superclass
    %% chain would take. LastNode is still just `a` on every trip.
    StepFun = fun(a, _Depth) -> {next, a} end,
    ?assertEqual({max_depth_exceeded, a}, beamtalk_hierarchy:walk_ancestors(a, StepFun, 5)).

max_depth_boundary_not_exceeded_at_exactly_max_depth_test() ->
    %% MaxDepth=2 allows StepFun to run at depths 0, 1, and 2 (Depth > MaxDepth
    %% is the guard, not Depth >= MaxDepth) before a further hop would trip it.
    StepFun = fun(Node, Depth) ->
        case Depth of
            2 -> {found, {reached, Node}};
            _ -> {next, Node + 1}
        end
    end,
    ?assertEqual({found, {reached, 2}}, beamtalk_hierarchy:walk_ancestors(0, StepFun, 2)).

max_depth_zero_still_runs_step_fun_once_test() ->
    %% MaxDepth=0: depth 0 is not > 0, so StepFun still runs once at the
    %% start node; only a further hop (depth 1 > 0) trips the guard.
    StepFun = fun(Node, _Depth) -> {next, Node + 1} end,
    ?assertEqual({max_depth_exceeded, 1}, beamtalk_hierarchy:walk_ancestors(0, StepFun, 0)).

%%% ============================================================================
%%% Recovering fold state from LastNode (BT-3096) — callers that ride an
%%% accumulator inside the node itself, e.g. {ClassName, Acc}, as documented
%%% by beamtalk_behaviour_intrinsics:walk_hierarchy/3's moduledoc.
%%% ============================================================================

max_depth_exceeded_carries_rider_accumulator_test() ->
    %% With MaxDepth=2, StepFun runs at depths 0, 1, and 2 — each prepending
    %% `a` to the rider — before the depth-3 node trips the guard, so the
    %% recovered accumulator has 3 entries, not 2.
    StepFun = fun({Name, Acc}, _Depth) -> {next, {Name, [Name | Acc]}} end,
    Result = beamtalk_hierarchy:walk_ancestors({a, []}, StepFun, 2),
    ?assertMatch({max_depth_exceeded, {a, [a, a, a]}}, Result).
