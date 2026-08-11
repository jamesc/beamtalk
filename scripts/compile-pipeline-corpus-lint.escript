#!/usr/bin/env escript
%%% Copyright 2026 James Casey
%%% SPDX-License-Identifier: Apache-2.0
%%%
%%% BT-3124 — corpus-through-BEAM Erlang half: batch-compile the `.core`
%%% files `crates/beamtalk-core/examples/compile_pipeline_corpus.rs` wrote,
%%% via the same `compile:file/2` options `beamtalk_build_worker.erl` and
%%% `compile.escript` use in production (`from_core`, `return_errors`,
%%% `return_warnings`, `clint` — `core_lint` already runs unconditionally on
%%% the `from_core` pipeline regardless of `clint`, see
%%% `beamtalk_compile_diagnostics` moduledoc).
%%%
%%% This is where "erlc rejects it" / `core_lint` flags an unbound variable
%%% surfaces for `.core` output beamtalk's own `generate_module` considered
%%% valid — without putting BEAM in the libFuzzer hot loop. Exits non-zero
%%% and prints one report block per failing file if any `.core` file fails
%%% to compile.
%%%
%%% Usage:
%%%   escript scripts/compile-pipeline-corpus-lint.escript [core_dir]
%%%
%%% Default `core_dir` is `target/compile-pipeline-corpus`.

-mode(compile).

main(Args) ->
    CoreDir =
        case Args of
            [Dir] -> Dir;
            _ -> "target/compile-pipeline-corpus"
        end,
    io:format("BT-3124 — corpus-through-BEAM lint~n"),
    io:format("===================================~n"),
    io:format("Core dir: ~s~n~n", [CoreDir]),

    CoreFiles = lists:sort(filelib:wildcard(filename:join(CoreDir, "*.core"))),
    io:format("Found ~p .core file(s)~n~n", [length(CoreFiles)]),

    OutDir = filename:join(CoreDir, "beam_out"),
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy")),

    Results = [compile_one(F, OutDir) || F <- CoreFiles],
    Failures = [R || {fail, _, _} = R <- Results],
    Warned = [R || {warn, _, _} = R <- Results],

    lists:foreach(fun report_warning/1, Warned),
    lists:foreach(fun report_failure/1, Failures),

    io:format(
        "~n~p/~p compiled cleanly, ~p with warnings, ~p failed.~n",
        [length(Results) - length(Failures), length(Results), length(Warned), length(Failures)]
    ),

    case Failures of
        [] -> halt(0);
        _ -> halt(1)
    end.

compile_one(CoreFile, OutDir) ->
    Options = [from_core, return_errors, return_warnings, clint, {outdir, OutDir}],
    case compile:file(CoreFile, Options) of
        {ok, _ModuleName} ->
            ok;
        {ok, _ModuleName, Warnings} ->
            %% `return_warnings' always returns the 3-tuple shape, even with
            %% an empty (or all-empty-per-file) Warnings list -- only treat
            %% this as an actual warning if there is text to show.
            case has_messages(Warnings) of
                true -> {warn, CoreFile, Warnings};
                false -> ok
            end;
        {error, Errors, _Warnings} ->
            {fail, CoreFile, Errors}
    end.

has_messages(Messages) ->
    lists:any(fun({_File, Infos}) -> Infos =/= [] end, Messages).

report_warning({warn, CoreFile, Warnings}) ->
    io:format("~n⚠️  WARNING: ~s~n", [CoreFile]),
    print_messages(Warnings).

report_failure({fail, CoreFile, Errors}) ->
    io:format("~n❌ FAILED: ~s~n", [CoreFile]),
    print_messages(Errors).

print_messages(Messages) ->
    lists:foreach(
        fun({File, Infos}) ->
            lists:foreach(
                fun({_Loc, Text}) -> io:format("   ~s", [Text]) end,
                sys_messages:format_messages(File, "", Infos, [])
            )
        end,
        Messages
    ).
