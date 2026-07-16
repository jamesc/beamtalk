%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_hierarchy).

%%% **DDD Context:** Object System Context

-moduledoc """
Generic depth-guarded ancestor-chain walker (BT-2786).

Before this module existed, `beamtalk_dispatch`, `beamtalk_method_resolver`,
and `beamtalk_class_dispatch` each hand-rolled their own "walk up the
superclass chain, bail out past `?MAX_HIERARCHY_DEPTH`" recursion — identical
in shape, differing only in what they probed at each level (a `has_method/2`
+ `superclass/1` gen_server round-trip, a `{method, Selector}` gen_server
call, or an ETS metadata read) and in the node type they walked over (a class
name atom or a class process pid).

`walk_ancestors/3` factors out the shared shape: start at a node, ask a
caller-supplied `StepFun` what to do at that node, and either stop (found /
not found) or advance to the next node — with the depth guard and cycle
warning handled once, here.

## Design

The walk is intentionally untyped in the node it walks over: callers may
walk class-name atoms (`beamtalk_class_dispatch`, `beamtalk_dispatch`) or
class-process pids (`beamtalk_method_resolver`) — `walk_ancestors/3` only
ever inspects a node for the sentinel `none`, which terminates the walk
early (mirrors "no superclass").

`StepFun` is invoked once per node and must return one of:

- `{found, Result}` — stop, propagate `Result` to the caller.
- `{next, NextNode}` — advance to `NextNode` (may itself be `none`).
- `not_found` — stop, nothing found anywhere in the chain.

Callers that need caller-specific behaviour at the max-depth boundary (e.g.
a structured `#beamtalk_error{}` naming the class that triggered it, or a
contextual `?LOG_WARNING` naming the selector) inspect the
`max_depth_exceeded` return and build their own error/log line; this module
only owns the depth guard itself, not the logging.
""".

-export([walk_ancestors/3]).

-type node_id() :: term().
-type step_result(Result) :: {found, Result} | {next, node_id() | none} | not_found.
-type step_fun(Result) :: fun((node_id(), non_neg_integer()) -> step_result(Result)).
-type walk_result(Result) :: {found, Result} | not_found | max_depth_exceeded.

-export_type([step_result/1, step_fun/1, walk_result/1]).

-doc """
Walk an ancestor chain starting at `StartNode`, applying `StepFun` at each
node until it reports `{found, Result}` or `not_found`, or the walk exceeds
`MaxDepth` levels.

`StartNode` may be `none` (an empty chain — e.g. a class with no
superclass), in which case the walk terminates immediately with `not_found`
and `StepFun` is never invoked.
""".
-spec walk_ancestors(node_id() | none, step_fun(Result), non_neg_integer()) ->
    walk_result(Result).
walk_ancestors(StartNode, StepFun, MaxDepth) ->
    walk_ancestors(StartNode, StepFun, MaxDepth, 0).

-spec walk_ancestors(node_id() | none, step_fun(Result), non_neg_integer(), non_neg_integer()) ->
    walk_result(Result).
walk_ancestors(none, _StepFun, _MaxDepth, _Depth) ->
    not_found;
walk_ancestors(_Node, _StepFun, MaxDepth, Depth) when Depth > MaxDepth ->
    max_depth_exceeded;
walk_ancestors(Node, StepFun, MaxDepth, Depth) ->
    case StepFun(Node, Depth) of
        {found, Result} ->
            {found, Result};
        {next, NextNode} ->
            walk_ancestors(NextNode, StepFun, MaxDepth, Depth + 1);
        not_found ->
            not_found
    end.
