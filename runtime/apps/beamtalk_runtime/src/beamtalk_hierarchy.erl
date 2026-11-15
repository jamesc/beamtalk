%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_hierarchy).

%%% **DDD Context:** Object System Context

-moduledoc """
Generic depth-guarded ancestor-chain walker.

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
`{max_depth_exceeded, LastNode}` return and build their own error/log line;
this module only owns the depth guard itself, not the logging.

## Recovering state on `max_depth_exceeded`

`max_depth_exceeded` carries the node the walk was about to visit when the
guard tripped — `LastNode` in `{max_depth_exceeded, LastNode}`. Two things
fall out of that:

- **Naming the actual cycle point.** A caller walking bare node ids (a class
  name atom, a class-process pid) can name the concrete ancestor where the
  cycle was detected in its log/error, instead of only the original
  receiver the walk started from.
- **Recovering partial fold state.** A caller that threads an accumulator
  through the walk (since `StepFun` only receives/returns a single node,
  such callers ride their accumulator along *inside* the node itself, e.g.
  `{ClassName, Acc}` — see `beamtalk_behaviour_intrinsics:walk_hierarchy/3`'s
  moduledoc) can pull `Acc` back out of `LastNode` and return the fold
  built up through every ancestor actually visited, rather than discarding
  it in favour of a fixed default.
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([walk_ancestors/3, walk_or_warn/4]).

-type node_id() :: term().
-type step_result(Result) :: {found, Result} | {next, node_id() | none} | not_found.
-type step_fun(Result) :: fun((node_id(), non_neg_integer()) -> step_result(Result)).
-type walk_result(Result) :: {found, Result} | not_found | {max_depth_exceeded, node_id()}.

-type warn_result(Result) :: {found, Result} | not_found | {cycle, node_id()}.
%% Extra report fields for `walk_or_warn/4`'s cycle warning. The optional key
%% `node_name` is a `fun((node_id()) -> term())` that renders the cycle node
%% for the log line (a pid -> class name, a `{Name, Acc}` rider -> `Name`);
%% every other key is logged verbatim as `Key=Value`.
-type warn_ctx() :: #{atom() => term()}.

-export_type([step_result/1, step_fun/1, walk_result/1, warn_result/1, warn_ctx/0]).

-doc """
Walk an ancestor chain starting at `StartNode`, applying `StepFun` at each
node until it reports `{found, Result}` or `not_found`, or the walk exceeds
`MaxDepth` levels.

`StartNode` may be `none` (an empty chain — e.g. a class with no
superclass), in which case the walk terminates immediately with `not_found`
and `StepFun` is never invoked.

On depth exhaustion, returns `{max_depth_exceeded, LastNode}` where
`LastNode` is the node the walk was about to visit next (i.e. the node
returned by the last successful `{next, NextNode}`) when the guard tripped
— `StepFun` is never invoked on it. See the moduledoc for how callers use
`LastNode` to name the cycle point or recover partial fold state.
""".
-spec walk_ancestors(node_id() | none, step_fun(Result), non_neg_integer()) ->
    walk_result(Result).
walk_ancestors(StartNode, StepFun, MaxDepth) ->
    walk_ancestors(StartNode, StepFun, MaxDepth, 0).

-spec walk_ancestors(node_id() | none, step_fun(Result), non_neg_integer(), non_neg_integer()) ->
    walk_result(Result).
walk_ancestors(none, _StepFun, _MaxDepth, _Depth) ->
    not_found;
walk_ancestors(Node, _StepFun, MaxDepth, Depth) when Depth > MaxDepth ->
    {max_depth_exceeded, Node};
walk_ancestors(Node, StepFun, MaxDepth, Depth) ->
    case StepFun(Node, Depth) of
        {found, Result} ->
            {found, Result};
        {next, NextNode} ->
            walk_ancestors(NextNode, StepFun, MaxDepth, Depth + 1);
        not_found ->
            not_found
    end.


-doc """
`walk_ancestors/3` bounded by `?MAX_HIERARCHY_DEPTH`, with the shared
"possible cycle" epilogue: on depth exhaustion it logs one `?LOG_WARNING`
and returns `{cycle, LastNode}`.

Returns `{found, Result} | not_found | {cycle, LastNode}`. `LastNode` is the
node the walk was about to visit (see `walk_ancestors/3`); a caller that
rides an accumulator inside the node pulls its partial fold back out of it,
any other caller just maps `{cycle, _}` to its fallback.

`Label` names the walk in the log line (usually the calling function).
`Ctx` adds the caller's own fields (`#{start => Class, selector => Sel}`);
see `warn_ctx()` for the `node_name` key.

Callers that treat a cycle as an ordinary miss and must not log (a probe
whose real walk warns elsewhere, or a path too hot to log on) call
`walk_ancestors/3` directly and say why next to the call.
""".
-spec walk_or_warn(node_id() | none, step_fun(Result), atom() | string(), warn_ctx()) ->
    warn_result(Result).
walk_or_warn(StartNode, StepFun, Label, Ctx) ->
    case walk_ancestors(StartNode, StepFun, ?MAX_HIERARCHY_DEPTH) of
        {max_depth_exceeded, LastNode} ->
            NameFun = maps:get(node_name, Ctx, fun(N) -> N end),
            Fields = maps:remove(node_name, Ctx),
            ?LOG_WARNING(
                "~ts: max hierarchy depth ~p exceeded at ~p~ts — possible cycle",
                [Label, ?MAX_HIERARCHY_DEPTH, NameFun(LastNode), format_ctx(Fields)],
                #{domain => [beamtalk, runtime]}
            ),
            {cycle, LastNode};
        Other ->
            Other
    end.

-spec format_ctx(#{atom() => term()}) -> string().
format_ctx(Fields) ->
    lists:append([
        io_lib:format(" ~ts=~p", [K, V])
     || {K, V} <- lists:sort(maps:to_list(Fields))
    ]).
