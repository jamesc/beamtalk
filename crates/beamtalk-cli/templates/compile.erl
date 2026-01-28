#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% Beamtalk Core Erlang batch compiler
%%%
%%% This escript compiles .core files to .beam bytecode using erlc.
%%% It communicates with the Rust CLI via stdin/stdout using a simple protocol.
%%%
%%% Protocol:
%%%   INPUT:  Erlang term: {OutputDir, [CoreFile1, CoreFile2, ...]}
%%%   OUTPUT: "beamtalk-compile-module:<module_name>" for each compiled module
%%%           "beamtalk-compile-result-ok" on success
%%%           "beamtalk-compile-result-error" on failure
%%%           Compilation errors/warnings printed to stderr
%%%
%%% Inspired by Gleam's compile.erl escript

-mode(compile).

main(_) ->
    ok = io:setopts([binary, {encoding, utf8}]),
    compile_loop().

compile_loop() ->
    case io:get_line("") of
        eof -> 
            ok;
        Line ->
            Chars = unicode:characters_to_list(Line),
            case erl_scan:string(Chars) of
                {ok, Tokens, _} ->
                    case erl_parse:parse_term(Tokens) of
                        {ok, {OutDir, CoreFiles}} ->
                            case compile_modules(OutDir, CoreFiles) of
                                {ok, ModuleNames} ->
                                    lists:foreach(
                                        fun(ModuleName) ->
                                            io:put_chars("beamtalk-compile-module:" ++ 
                                                        atom_to_list(ModuleName) ++ "\n")
                                        end,
                                        ModuleNames
                                    ),
                                    io:put_chars("beamtalk-compile-result-ok\n");
                                error ->
                                    io:put_chars("beamtalk-compile-result-error\n")
                            end;
                        {error, _} ->
                            io:put_chars(standard_error, "Error: Invalid input format\n"),
                            io:put_chars("beamtalk-compile-result-error\n")
                    end;
                {error, _, _} ->
                    io:put_chars(standard_error, "Error: Invalid input format\n"),
                    io:put_chars("beamtalk-compile-result-error\n")
            end,
            compile_loop()
    end.

compile_modules(OutDir, CoreFiles) ->
    %% Ensure output directory exists
    case filelib:ensure_dir(filename:join(OutDir, "dummy")) of
        ok ->
            %% Start parallel compiler workers
            Workers = start_compiler_workers(OutDir),
            ok = producer_loop(CoreFiles, Workers),
            
            %% Collect results - wait for all files to be compiled
            collect_results(length(CoreFiles), {true, []});
        {error, Reason} ->
            io:put_chars(standard_error, 
                io_lib:format("Error creating output directory: ~p~n", [Reason])),
            error
    end.

collect_results(0, {Result, Modules}) ->
    case Result of
        true -> {ok, Modules};
        false -> error
    end;
collect_results(ExpectedCount, {Result, Modules}) when ExpectedCount > 0 ->
    receive
        {compiled, ModuleName} -> 
            collect_results(ExpectedCount - 1, {Result, [ModuleName | Modules]});
        failed -> 
            collect_results(ExpectedCount - 1, {false, Modules})
    end.

producer_loop([], 0) ->
    ok;
producer_loop([], Workers) ->
    receive
        {work_please, _} -> producer_loop([], Workers - 1)
    end;
producer_loop([CoreFile | CoreFiles], Workers) ->
    receive
        {work_please, Worker} ->
            erlang:send(Worker, {module, CoreFile}),
            producer_loop(CoreFiles, Workers)
    end.

start_compiler_workers(OutDir) ->
    Parent = self(),
    NumSchedulers = erlang:system_info(schedulers),
    SpawnWorker = fun(_) ->
        erlang:spawn_link(fun() -> worker_loop(Parent, OutDir) end)
    end,
    Workers = [SpawnWorker(I) || I <- lists:seq(1, NumSchedulers)],
    length(Workers).

worker_loop(Parent, OutDir) ->
    %% Compile options for Core Erlang
    Options = [
        from_core,           % Compile from Core Erlang
        report_errors,       % Print errors
        report_warnings,     % Print warnings
        debug_info,          % Include debug info for hot code reload
        {outdir, OutDir}     % Output directory
    ],
    
    erlang:send(Parent, {work_please, self()}),
    receive
        {module, CoreFile} ->
            case compile:file(CoreFile, Options) of
                {ok, ModuleName} ->
                    erlang:send(Parent, {compiled, ModuleName}),
                    worker_loop(Parent, OutDir);
                error ->
                    erlang:send(Parent, failed),
                    worker_loop(Parent, OutDir)
            end
    end.
