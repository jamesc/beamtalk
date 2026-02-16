#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Compile test fixtures for runtime tests.
%% Called by rebar3 pre-hook before eunit.
%% Portable replacement for compile.sh (works on Windows, macOS, Linux).

main([]) ->
    ScriptDir = filename:dirname(filename:absname(escript:script_name())),
    RepoRoot = filename:dirname(filename:dirname(filename:dirname(filename:dirname(ScriptDir)))),
    FixturesDir = filename:join([RepoRoot, "runtime", "apps", "beamtalk_runtime", "test_fixtures"]),

    %% Determine beamtalk binary path
    Beamtalk = case os:type() of
        {win32, _} -> filename:join([RepoRoot, "target", "debug", "beamtalk.exe"]);
        _          -> filename:join([RepoRoot, "target", "debug", "beamtalk"])
    end,

    %% Ensure beamtalk binary is built
    CargoBin = case os:type() of
        {win32, _} -> os:find_executable("cargo.exe");
        _          -> os:find_executable("cargo")
    end,
    case CargoBin of
        false ->
            io:format(standard_error, "cargo not found in PATH~n", []),
            halt(1);
        _ -> ok
    end,
    CargoPort = open_port({spawn_executable, CargoBin},
                          [{args, ["build", "--bin", "beamtalk", "--quiet"]},
                           exit_status, stderr_to_stdout, binary]),
    _CargoOutput = collect_port_output(CargoPort, []),

    %% Verify the binary exists
    case filelib:is_regular(Beamtalk) of
        true -> ok;
        false ->
            io:format(standard_error, "beamtalk binary not found at ~s (cargo build may have failed)~n", [Beamtalk]),
            halt(1)
    end,

    %% Clean old artifacts
    CounterBuildDir = filename:join([RepoRoot, "tests", "e2e", "fixtures", "build"]),
    FixtureBuildDir = filename:join([FixturesDir, "build"]),
    delete_if_exists(filename:join(CounterBuildDir, "bt@counter.beam")),
    delete_if_exists(filename:join(CounterBuildDir, "bt@counter.core")),
    delete_if_exists(filename:join(FixtureBuildDir, "bt@logging_counter.beam")),
    delete_if_exists(filename:join(FixtureBuildDir, "bt@logging_counter.core")),

    %% Build counter fixture (using E2E fixture - BT-239)
    CounterSrc = filename:join([RepoRoot, "tests", "e2e", "fixtures", "counter.bt"]),
    CounterBeam = filename:join(CounterBuildDir, "bt@counter.beam"),
    run_beamtalk(Beamtalk, CounterSrc),
    case filelib:is_regular(CounterBeam) of
        true -> ok;
        false ->
            io:format(standard_error, "Failed to compile counter.bt (no .beam output)~n"
                      "  Expected: ~s~n"
                      "  Beamtalk: ~s~n"
                      "  Source:   ~s~n"
                      "  RepoRoot: ~s~n", [CounterBeam, Beamtalk, CounterSrc, RepoRoot]),
            halt(1)
    end,

    %% Copy to rebar3 build directories
    copy_to_build_dirs(RepoRoot, CounterBeam, "bt@counter.beam"),

    %% Build logging_counter fixture (BT-108 - super keyword tests)
    LoggingCounterSrc = filename:join([FixturesDir, "logging_counter.bt"]),
    LoggingCounterBeam = filename:join(FixtureBuildDir, "bt@logging_counter.beam"),
    run_beamtalk(Beamtalk, LoggingCounterSrc),
    case filelib:is_regular(LoggingCounterBeam) of
        true -> ok;
        false ->
            io:format(standard_error, "Failed to compile logging_counter.bt (no .beam output)~n"
                      "  Expected: ~s~n"
                      "  Beamtalk: ~s~n"
                      "  Source:   ~s~n", [LoggingCounterBeam, Beamtalk, LoggingCounterSrc]),
            halt(1)
    end,

    %% Copy to rebar3 build directories
    copy_to_build_dirs(RepoRoot, LoggingCounterBeam, "bt@logging_counter.beam"),

    ok.

%% @private Run beamtalk build on a source file.
%% Uses open_port/spawn_executable to avoid cmd.exe quoting issues on Windows.
run_beamtalk(Beamtalk, SrcFile) ->
    Port = open_port({spawn_executable, Beamtalk},
                     [{args, ["build", SrcFile]},
                      exit_status, stderr_to_stdout, binary]),
    Output = collect_port_output(Port, []),
    case Output of
        <<>> -> ok;
        _   -> io:put_chars(["beamtalk build ", filename:basename(SrcFile), ":\n", Output, "\n"])
    end.

%% @private Collect all output from an open_port until exit.
collect_port_output(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_port_output(Port, [Acc, Data]);
        {Port, {exit_status, 0}} ->
            iolist_to_binary(Acc);
        {Port, {exit_status, Code}} ->
            io:format(standard_error, "beamtalk build exited with code ~p~n", [Code]),
            iolist_to_binary(Acc)
    after 60000 ->
        io:format(standard_error, "beamtalk build timed out after 60s~n", []),
        iolist_to_binary(Acc)
    end.

%% @private Delete a file if it exists.
delete_if_exists(Path) ->
    case filelib:is_regular(Path) of
        true -> file:delete(Path);
        false -> ok
    end.

%% @private Copy a beam file to all rebar3 test build directories.
copy_to_build_dirs(RepoRoot, SrcFile, FileName) ->
    Pattern = filename:join([RepoRoot, "runtime", "_build", "*", "lib", "beamtalk_runtime", "test"]),
    Dirs = filelib:wildcard(Pattern),
    lists:foreach(fun(Dir) ->
        Dest = filename:join(Dir, FileName),
        case file:copy(SrcFile, Dest) of
            {ok, _BytesCopied} ->
                ok;
            {error, Reason} ->
                io:format(standard_error,
                          "Failed to copy ~s to ~s: ~p~n",
                          [SrcFile, Dest, Reason]),
                halt(1)
        end
    end, Dirs).
