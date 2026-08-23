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
    case collect_port_output(CargoPort, []) of
        {ok, _} -> ok;
        {error, Reason, Output} ->
            io:format(standard_error, "cargo build failed (~p)~n~s", [Reason, Output]),
            halt(1)
    end,

    %% Verify the binary exists
    case filelib:is_regular(Beamtalk) of
        true -> ok;
        false ->
            io:format(standard_error, "beamtalk binary not found at ~s (cargo build may have failed)~n", [Beamtalk]),
            halt(1)
    end,

    %% Clean old artifacts
    CounterBuildDir = filename:join([RepoRoot, "tests", "repl-protocol", "fixtures", "build"]),
    FixtureBuildDir = filename:join([FixturesDir, "build"]),
    delete_if_exists(filename:join(CounterBuildDir, "bt@counter.beam")),
    delete_if_exists(filename:join(CounterBuildDir, "bt@counter.core")),

    %% Build counter fixture (using REPL-protocol fixture - BT-239)
    CounterSrc = filename:join([RepoRoot, "tests", "repl-protocol", "fixtures", "counter.bt"]),
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

    %% Build the remaining local fixtures that live alongside this script.
    %% Each entry is a .bt source file compiled to a module named after its
    %% snake_case basename (bt@<basename>). Add new fixtures here rather
    %% than hand-rolling another copy of this block (BT-3093).
    LocalFixtures = [
        %% BT-108 - super keyword tests
        "logging_counter",
        %% BT-3093 - migrated hand-simulated Counter/Rectangle/Box/Spawner/
        %% instance-var fixtures onto real compiled dispatch
        "arithmetic_actor",
        "rectangle_actor",
        "box_actor",
        "spawner_actor",
        "shadow_actor",
        "coordinate_actor"
    ],
    lists:foreach(
        fun(Basename) -> build_local_fixture(Beamtalk, FixturesDir, FixtureBuildDir, RepoRoot, Basename) end,
        LocalFixtures
    ),

    %% BT-3251: fixtures needed by beamtalk_stdlib's EUnit scope (a real
    %% compiled TestCase subclass for beamtalk_test_case's BIF-fallback-path
    %% regression test), in addition to beamtalk_runtime's. Kept as a
    %% separate list/app-target pair rather than widening every existing
    %% LocalFixtures entry's destination, since those are beamtalk_runtime-only.
    StdlibFixtures = [
        %% BT-3251 - BIF-fallback (run_all/1 etc.) regression fixture
        "bif_fallback_test_case"
    ],
    lists:foreach(
        fun(Basename) ->
            build_local_fixture(
                Beamtalk, FixturesDir, FixtureBuildDir, RepoRoot, Basename, [
                    {"test", "beamtalk_stdlib", "ebin"}
                ]
            )
        end,
        StdlibFixtures
    ),

    ok.

%% @private Compile a `<Basename>.bt` fixture from FixturesDir and copy the
%% resulting `bt@<Basename>.beam` into every rebar3 test build directory for
%% `beamtalk_runtime`'s `test` dir (the default consumer of this fixtures
%% directory).
build_local_fixture(Beamtalk, FixturesDir, FixtureBuildDir, RepoRoot, Basename) ->
    build_local_fixture(Beamtalk, FixturesDir, FixtureBuildDir, RepoRoot, Basename, [
        {"*", "beamtalk_runtime", "test"}
    ]).

%% @private Compile a `<Basename>.bt` fixture from FixturesDir and copy the
%% resulting `bt@<Basename>.beam` into every rebar3 build directory of each
%% `{Profile, App, SubDir}` in `Targets` (BT-3251: a fixture consumed by more
%% than one app's EUnit scope, e.g. beamtalk_stdlib, needs its `.beam` on
%% that app's code path too).
%%
%% SubDir matters: `rebar3 eunit --dir=apps/<App>/test` treats every `.beam`
%% file already sitting in that app's `test` build dir as a candidate test
%% suite and probes it for `_test`/`_test_`-suffixed exports — which, for a
%% genuinely Beamtalk-compiled module, means calling something equivalent to
%% `Module:module_info(exports)` and hitting the very `undef` this fixture
%% exists to reproduce, this time inside EUnit's own discovery instead of
%% the code under test (observed as an EUnit-level `{abort,
%% {module_not_found, Mod}}` and cascading `cancelled` tests). `ebin` (the
%% app's normal, non-test compiled code) is also on the VM code path during
%% `eunit --dir=...` — so `resolve_module/1`'s `code:get_object_code/1` /
%% `code:ensure_loaded/1` still finds it there — without being inside the
%% directory EUnit scans for test suites.
%%
%% Profile matters too: an `ebin`-targeted fixture must be pinned to the
%% `test` rebar3 profile, not the `*` wildcard `beamtalk_runtime`'s `test`-dir
%% fixtures use. `_build/default/lib/<App>/ebin` is exactly what `just
%% install` copies into the install prefix wholesale, and `*` would also
%% match it whenever a prior `rebar3 compile`/`build-erlang` run (independent
%% of this eunit-only pre-hook) already created that directory — silently
%% shipping an EUnit fixture class into a production install.
build_local_fixture(Beamtalk, FixturesDir, FixtureBuildDir, RepoRoot, Basename, Targets) ->
    ModuleFile = "bt@" ++ Basename ++ ".beam",
    Src = filename:join([FixturesDir, Basename ++ ".bt"]),
    Beam = filename:join(FixtureBuildDir, ModuleFile),
    delete_if_exists(Beam),
    delete_if_exists(filename:join(FixtureBuildDir, "bt@" ++ Basename ++ ".core")),
    run_beamtalk(Beamtalk, Src),
    case filelib:is_regular(Beam) of
        true -> ok;
        false ->
            io:format(standard_error, "Failed to compile ~s.bt (no .beam output)~n"
                      "  Expected: ~s~n"
                      "  Beamtalk: ~s~n"
                      "  Source:   ~s~n", [Basename, Beam, Beamtalk, Src]),
            halt(1)
    end,
    lists:foreach(
        fun({Profile, App, SubDir}) ->
            copy_to_build_dirs(RepoRoot, Beam, ModuleFile, Profile, App, SubDir)
        end,
        Targets
    ).

%% @private Run beamtalk build on a source file.
%% Uses open_port/spawn_executable to avoid cmd.exe quoting issues on Windows.
run_beamtalk(Beamtalk, SrcFile) ->
    Port = open_port({spawn_executable, Beamtalk},
                     [{args, ["build", SrcFile]},
                      exit_status, stderr_to_stdout, binary]),
    case collect_port_output(Port, []) of
        {ok, <<>>} -> ok;
        {ok, Output} ->
            io:put_chars(["beamtalk build ", filename:basename(SrcFile), ":\n", Output, "\n"]);
        {error, Reason, Output} ->
            io:put_chars(["beamtalk build ", filename:basename(SrcFile), ":\n", Output, "\n"]),
            io:format(standard_error, "beamtalk build failed (~p)~n", [Reason]),
            halt(1)
    end.

%% @private Collect all output from an open_port until exit.
collect_port_output(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_port_output(Port, [Acc, Data]);
        {Port, {exit_status, 0}} ->
            {ok, iolist_to_binary(Acc)};
        {Port, {exit_status, Code}} ->
            {error, {exit_status, Code}, iolist_to_binary(Acc)}
    after 60000 ->
        (try port_close(Port) catch _:_ -> ok end),
        {error, timeout, iolist_to_binary(Acc)}
    end.

%% @private Delete a file if it exists.
delete_if_exists(Path) ->
    case filelib:is_regular(Path) of
        true -> file:delete(Path);
        false -> ok
    end.

%% @private Copy a beam file to all of `beamtalk_runtime`'s rebar3 test build
%% directories, across every profile (the default consumer of this fixtures
%% directory).
copy_to_build_dirs(RepoRoot, SrcFile, FileName) ->
    copy_to_build_dirs(RepoRoot, SrcFile, FileName, "*", "beamtalk_runtime", "test").

%% @private Copy a beam file to `App`'s rebar3 `SubDir` build directory under
%% `Profile` (a literal rebar3 profile name like `"test"`, or `"*"` to match
%% every profile directory present) (BT-3251: a fixture may be consumed by
%% more than one app's EUnit scope, and may need to land in `ebin` rather
%% than `test`, pinned to the `test` profile only — see
%% `build_local_fixture/6`'s doc for why).
%%
%% Fails loudly (rather than silently copying nowhere) when the wildcard
%% matches no directories — e.g. a mistyped `SubDir`/`App`, or `Profile`
%% pinned to `"test"` before `rebar3 eunit` has ever created that profile's
%% `_build` tree — since a fixture that never lands anywhere degrades to
%% the exact same symptom the bug it exists to catch produces ("No test
%% methods found in ..."), silently passing instead of failing the build.
copy_to_build_dirs(RepoRoot, SrcFile, FileName, Profile, App, SubDir) ->
    Pattern = filename:join([RepoRoot, "runtime", "_build", Profile, "lib", App, SubDir]),
    case filelib:wildcard(Pattern) of
        [] ->
            io:format(standard_error,
                      "No build directories matched ~s — nowhere to copy ~s~n",
                      [Pattern, FileName]),
            halt(1);
        Dirs ->
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
            end, Dirs)
    end.
