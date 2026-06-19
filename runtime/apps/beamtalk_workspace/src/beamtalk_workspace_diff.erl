%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_diff).

%%% **DDD Context:** Workspace Context

-moduledoc """
Line-oriented unified diff for ChangeLog method bodies (ADR 0082 Phase 5+,
BT-2575).

`unified/2` renders the net change between two method-body snapshots (the
current on-disk body vs the installed in-memory body) as a compact, surface-
agnostic text diff: each line is prefixed with `"  "` (unchanged), `"- "`
(removed / on disk) or `"+ "` (added / in memory). Generating the diff here —
not per surface — keeps `Workspace changes` byte-identical across REPL, MCP, and
the LiveView IDE (surface parity).

Alignment uses a longest-common-subsequence over lines. Method bodies are small
(tens of lines), so the O(n·m) table is fine and avoids pulling in a diff
dependency.
""".

-export([unified/2, ops/2]).

-export_type([op/0]).

%% One aligned line: kept (in both), deleted (only on disk), inserted (only in
%% the in-memory body).
-type op() :: {eq | del | ins, binary()}.

-doc """
Render the line diff from `Old` (on-disk body) to `New` (in-memory body) as a
unified-style text binary. Returns `<<>>` when both sides are byte-equal after
line splitting (callers normally only diff known-different bodies). A trailing
newline on either side is treated as a line terminator, not an extra blank line.
""".
-spec unified(binary(), binary()) -> binary().
unified(Old, New) ->
    render(ops(Old, New)).

-doc """
Compute the aligned line operations from `Old` to `New` (exposed for tests and
for callers that want to render the diff differently). Oldest-to-newest order.
""".
-spec ops(binary(), binary()) -> [op()].
ops(Old, New) ->
    OldT = list_to_tuple(split_lines(Old)),
    NewT = list_to_tuple(split_lines(New)),
    Table = lcs_table(OldT, NewT),
    backtrack(OldT, NewT, tuple_size(OldT), tuple_size(NewT), Table, []).

%%% ----------------------------------------------------------------------------
%%% Internals
%%% ----------------------------------------------------------------------------

%% Split on newlines, dropping exactly one trailing empty element so that a body
%% with a trailing newline is not reported as ending in a spurious blank line.
-spec split_lines(binary()) -> [binary()].
split_lines(Bin) ->
    drop_trailing_empty(binary:split(Bin, <<"\n">>, [global])).

-spec drop_trailing_empty([binary()]) -> [binary()].
drop_trailing_empty(Lines) ->
    case lists:reverse(Lines) of
        [<<>> | Rest] -> lists:reverse(Rest);
        _ -> Lines
    end.

%% LCS length table keyed `{I, J}` → length of the LCS of Old[1..I], New[1..J].
%% Missing keys (any index 0) are treated as 0 by lcs_at/3.
-spec lcs_table(tuple(), tuple()) -> #{{non_neg_integer(), non_neg_integer()} => non_neg_integer()}.
lcs_table(OldT, NewT) ->
    N = tuple_size(OldT),
    M = tuple_size(NewT),
    lists:foldl(
        fun(I, AccI) ->
            lists:foldl(
                fun(J, AccJ) ->
                    Val =
                        case element(I, OldT) =:= element(J, NewT) of
                            true -> lcs_at(AccJ, I - 1, J - 1) + 1;
                            false -> max(lcs_at(AccJ, I - 1, J), lcs_at(AccJ, I, J - 1))
                        end,
                    AccJ#{{I, J} => Val}
                end,
                AccI,
                lists:seq(1, M)
            )
        end,
        #{},
        lists:seq(1, N)
    ).

-spec lcs_at(map(), non_neg_integer(), non_neg_integer()) -> non_neg_integer().
lcs_at(_Table, 0, _J) -> 0;
lcs_at(_Table, _I, 0) -> 0;
lcs_at(Table, I, J) -> maps:get({I, J}, Table, 0).

%% Walk the table back to front, emitting ops in forward order. On a tie prefer
%% the insertion *step* here, which (because ops are prepended during the
%% backtrack) renders a replaced line as "- old" then "+ new".
-spec backtrack(tuple(), tuple(), non_neg_integer(), non_neg_integer(), map(), [op()]) -> [op()].
backtrack(_OldT, _NewT, 0, 0, _Table, Acc) ->
    Acc;
backtrack(OldT, NewT, I, J, Table, Acc) when I > 0, J > 0 ->
    case element(I, OldT) =:= element(J, NewT) of
        true ->
            backtrack(OldT, NewT, I - 1, J - 1, Table, [{eq, element(I, OldT)} | Acc]);
        false ->
            case lcs_at(Table, I - 1, J) > lcs_at(Table, I, J - 1) of
                true -> backtrack(OldT, NewT, I - 1, J, Table, [{del, element(I, OldT)} | Acc]);
                false -> backtrack(OldT, NewT, I, J - 1, Table, [{ins, element(J, NewT)} | Acc])
            end
    end;
backtrack(OldT, NewT, I, 0, Table, Acc) when I > 0 ->
    backtrack(OldT, NewT, I - 1, 0, Table, [{del, element(I, OldT)} | Acc]);
backtrack(OldT, NewT, 0, J, Table, Acc) when J > 0 ->
    backtrack(OldT, NewT, 0, J - 1, Table, [{ins, element(J, NewT)} | Acc]).

-spec render([op()]) -> binary().
render(Ops) ->
    iolist_to_binary([render_op(Op) || Op <- Ops]).

-spec render_op(op()) -> iolist().
render_op({eq, Line}) -> [<<"  ">>, Line, <<"\n">>];
render_op({del, Line}) -> [<<"- ">>, Line, <<"\n">>];
render_op({ins, Line}) -> [<<"+ ">>, Line, <<"\n">>].
