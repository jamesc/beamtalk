%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_build_worker).

%%% **DDD Context:** Compilation (Anti-Corruption Layer)

-moduledoc """
Batch compiler worker for `beamtalk build' (ADR 0022, Phase 3).

Replaces `compile.escript' with an in-memory compilation pipeline.
Uses `core_scan:string/1' → `core_parse:parse/1' → `compile:forms/2'
to compile Core Erlang to BEAM bytecode without temporary files.
Includes `debug_info' for hot code reload support.

Also supports spec reading for ADR 0075 (Erlang FFI Type Definitions).

Protocol:
  INPUT:  Erlang term on stdin, one of:
    {OutputDir, [CoreFile1, CoreFile2, ...]}  — compile Core Erlang
    {read_specs, [BeamFile1, BeamFile2, ...]} — read specs from .beam files
  OUTPUT (compile):
    "beamtalk-compile-module:<module_name>" for each compiled module
    "beamtalk-compile-result-ok" on success
    "beamtalk-compile-result-error" on failure
    Compilation errors/warnings printed to stderr
  OUTPUT (read_specs):
    "beamtalk-specs-module:<module_name>:<erlang_term>" per module
    "beamtalk-specs-result-ok" on success
    "beamtalk-specs-result-error" on failure
""".

-export([main/0]).

-ifdef(TEST).
-export([
    compile_core_erlang/1,
    compile_core_file/2,
    compile_modules/2,
    handle_read_specs/1
]).
-endif.

-spec main() -> no_return().
main() ->
    ok = io:setopts([binary, {encoding, utf8}]),
    compile_loop(),
    halt(0).

compile_loop() ->
    case io:get_line("") of
        eof ->
            ok;
        {error, _} ->
            ok;
        Line ->
            Chars = unicode:characters_to_list(Line),
            case erl_scan:string(Chars) of
                {ok, Tokens, _} ->
                    case erl_parse:parse_term(Tokens) of
                        {ok, {read_specs, BeamFiles}} ->
                            handle_read_specs(BeamFiles);
                        {ok, {OutDir, CoreFiles}} ->
                            case compile_modules(OutDir, CoreFiles) of
                                {ok, ModuleNames} ->
                                    lists:foreach(
                                        fun(ModuleName) ->
                                            io:put_chars(
                                                "beamtalk-compile-module:" ++
                                                    atom_to_list(ModuleName) ++ "\n"
                                            )
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

%% Handle {read_specs, BeamFiles} command via beamtalk_spec_reader.
%%
%% Emits one line per module: "beamtalk-specs-module:<module>:<erlang_term>"
%% where <erlang_term> is a printable Erlang term of the spec list.
%% Ends with "beamtalk-specs-result-ok" or "beamtalk-specs-result-error".
handle_read_specs(BeamFiles) ->
    try
        Results = beamtalk_spec_reader:read_specs_batch(BeamFiles),
        lists:foreach(
            fun
                ({ModName, {ok, Specs}}) ->
                    %% Encode specs as an Erlang term string on one line
                    TermStr = io_lib:format("~0tp", [Specs]),
                    io:put_chars([
                        "beamtalk-specs-module:",
                        ModName,
                        ":",
                        TermStr,
                        "\n"
                    ]);
                ({ModName, {error, Reason}}) ->
                    io:put_chars(
                        standard_error,
                        io_lib:format("Warning: ~s: ~tp~n", [ModName, Reason])
                    )
            end,
            Results
        ),
        io:put_chars("beamtalk-specs-result-ok\n")
    catch
        Class:Reason:Stack ->
            io:put_chars(
                standard_error,
                io_lib:format(
                    "Error reading specs: ~tp:~tp~n~tp~n",
                    [Class, Reason, Stack]
                )
            ),
            io:put_chars("beamtalk-specs-result-error\n")
    end.

compile_modules(OutDir, CoreFiles) ->
    case filelib:ensure_dir(filename:join(OutDir, "dummy")) of
        ok ->
            Workers = start_compiler_workers(OutDir),
            ok = producer_loop(CoreFiles, Workers),
            collect_results(length(CoreFiles), {true, []});
        {error, Reason} ->
            io:put_chars(
                standard_error,
                io_lib:format("Error creating output directory: ~p~n", [Reason])
            ),
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
    erlang:send(Parent, {work_please, self()}),
    receive
        {module, CoreFile} ->
            case compile_core_file(CoreFile, OutDir) of
                {ok, ModuleName} ->
                    erlang:send(Parent, {compiled, ModuleName}),
                    worker_loop(Parent, OutDir);
                error ->
                    erlang:send(Parent, failed),
                    worker_loop(Parent, OutDir)
            end
    end.

%% Compile a single .core file using in-memory compilation.
%% Reads the file, compiles via core_scan/core_parse/compile:forms,
%% and writes the resulting .beam file to OutDir.
compile_core_file(CoreFile, OutDir) ->
    case file:read_file(CoreFile) of
        {ok, CoreErlangBin} ->
            case compile_core_erlang(CoreErlangBin) of
                {ok, ModuleName, BeamBinary} ->
                    BeamFile = filename:join(
                        OutDir,
                        atom_to_list(ModuleName) ++ ".beam"
                    ),
                    case file:write_file(BeamFile, BeamBinary) of
                        ok ->
                            {ok, ModuleName};
                        {error, WriteReason} ->
                            io:put_chars(
                                standard_error,
                                io_lib:format(
                                    "Error writing ~s: ~p~n",
                                    [BeamFile, WriteReason]
                                )
                            ),
                            error
                    end;
                {error, Reason} ->
                    io:put_chars(
                        standard_error,
                        io_lib:format(
                            "Error compiling ~s: ~p~n",
                            [CoreFile, Reason]
                        )
                    ),
                    error
            end;
        {error, ReadReason} ->
            io:put_chars(
                standard_error,
                io_lib:format(
                    "Error reading ~s: ~p~n",
                    [CoreFile, ReadReason]
                )
            ),
            error
    end.

%% Compile Core Erlang binary to BEAM bytecode in memory.
%% Like beamtalk_compiler_server:compile_core_erlang/1 but includes
%% debug_info for hot code reload support.
compile_core_erlang(CoreErlangBin) ->
    CoreErlangStr = binary_to_list(CoreErlangBin),
    case core_scan:string(CoreErlangStr) of
        {ok, Tokens, _} ->
            case core_parse:parse(Tokens) of
                {ok, CoreModule} ->
                    case
                        compile:forms(
                            CoreModule,
                            [
                                from_core,
                                binary,
                                return_errors,
                                report_warnings,
                                debug_info
                            ]
                        )
                    of
                        {ok, ModuleName, Binary} ->
                            {ok, ModuleName, Binary};
                        {ok, ModuleName, Binary, _Warnings} ->
                            {ok, ModuleName, Binary};
                        {error, Errors, _Warnings} ->
                            {error, {core_compile_error, Errors}}
                    end;
                {error, ParseError} ->
                    {error, {core_parse_error, ParseError}}
            end;
        {error, ScanError, _Loc} ->
            {error, {core_scan_error, ScanError}}
    end.
