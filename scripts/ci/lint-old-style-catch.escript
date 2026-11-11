#!/usr/bin/env escript
%% -*- erlang -*-
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Lint (BT-4616): no old-style `catch Expr` in runtime sources, runtime tests,
%% perf or benchmarks. OTP 29 deprecates it; use `try ... catch ... end`.
%% Parses with epp_dodger/erl_syntax, so comments and strings are never flagged.

main(_) ->
    Files = lists:append([
        filelib:wildcard("runtime/apps/*/{src,test}/**/*.{erl,hrl}"),
        filelib:wildcard("runtime/perf/**/*.{erl,hrl}"),
        filelib:wildcard("benchmarks/**/*.{erl,hrl}")
    ]),
    Hits = lists:append([scan(F) || F <- lists:sort(Files)]),
    case Hits of
        [] ->
            io:format("lint-old-style-catch: ok (~b files)~n", [length(Files)]);
        _ ->
            [io:format("~s:~b: old-style `catch Expr` (use try ... catch ... end)~n", [F, L]) || {F, L} <- Hits],
            halt(1)
    end.

scan(File) ->
    case epp_dodger:parse_file(File) of
        {ok, Forms} ->
            lists:append([find(F, File) || F <- Forms]);
        {error, Reason} ->
            io:format("~s: cannot parse: ~p~n", [File, Reason]),
            halt(2)
    end.

find(Tree, File) ->
    Here =
        case erl_syntax:type(Tree) of
            catch_expr -> [{File, erl_syntax:get_pos(Tree)}];
            _ -> []
        end,
    Here ++ lists:append([find(C, File) || G <- erl_syntax:subtrees(Tree), C <- G]).
