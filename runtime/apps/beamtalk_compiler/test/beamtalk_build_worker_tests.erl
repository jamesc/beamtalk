%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Tests for beamtalk_build_worker (ADR 0022, Phase 3).
%%
%% Tests the batch compiler worker's in-memory compilation pipeline,
%% file I/O, and parallel worker management.

-module(beamtalk_build_worker_tests).

-include_lib("eunit/include/eunit.hrl").

%%% ---------------------------------------------------------------
%%% compile_core_erlang/1 — pure function, no port or binary needed
%%% ---------------------------------------------------------------

%% Valid Core Erlang compiles successfully.
compile_core_erlang_valid_test() ->
    CoreErlang = valid_core_erlang_source(),
    {ok, ModuleName, Binary} = beamtalk_build_worker:compile_core_erlang(CoreErlang),
    ?assertEqual(test_build_worker_mod, ModuleName),
    ?assert(is_binary(Binary)),
    ?assert(byte_size(Binary) > 0).

%% Compiled module is loadable and executable.
compile_core_erlang_loadable_test() ->
    CoreErlang = valid_core_erlang_source(),
    {ok, ModuleName, Binary} = beamtalk_build_worker:compile_core_erlang(CoreErlang),
    {module, ModuleName} = code:load_binary(ModuleName, "", Binary),
    Result = ModuleName:hello(),
    code:purge(ModuleName),
    code:delete(ModuleName),
    ?assertEqual(world, Result).

%% Invalid Core Erlang returns scan error.
compile_core_erlang_scan_error_test() ->
    Result = beamtalk_build_worker:compile_core_erlang(<<"not valid core erlang @#$">>),
    ?assertMatch({error, {core_scan_error, _}}, Result).

%% Syntactically scannable but unparseable Core Erlang returns parse error.
compile_core_erlang_parse_error_test() ->
    %% Scannable tokens but not valid Core Erlang structure
    Result = beamtalk_build_worker:compile_core_erlang(<<"module 'x' []\n">>),
    ?assertMatch({error, {core_parse_error, _}}, Result).

%% Empty input returns error.
compile_core_erlang_empty_test() ->
    Result = beamtalk_build_worker:compile_core_erlang(<<>>),
    ?assertMatch({error, _}, Result).

%%% ---------------------------------------------------------------
%%% compile_core_file/2 — reads file, compiles, writes .beam
%%% ---------------------------------------------------------------

compile_core_file_test_() ->
    {setup, fun setup_temp_dir/0, fun cleanup_temp_dir/1, fun(TmpDir) ->
        [
            {"compiles a valid .core file to .beam", fun() -> compile_core_file_valid(TmpDir) end},
            {"returns error for missing .core file", fun() -> compile_core_file_missing(TmpDir) end},
            {"returns error for invalid .core content", fun() ->
                compile_core_file_invalid(TmpDir)
            end}
        ]
    end}.

compile_core_file_valid(TmpDir) ->
    CoreFile = filename:join(TmpDir, "test_build_worker_mod.core"),
    OutDir = filename:join(TmpDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy")),
    ok = file:write_file(CoreFile, valid_core_erlang_source()),
    {ok, test_build_worker_mod} = beamtalk_build_worker:compile_core_file(CoreFile, OutDir),
    BeamFile = filename:join(OutDir, "test_build_worker_mod.beam"),
    ?assert(filelib:is_regular(BeamFile)).

compile_core_file_missing(TmpDir) ->
    OutDir = filename:join(TmpDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy")),
    Result = beamtalk_build_worker:compile_core_file("/nonexistent/file.core", OutDir),
    ?assertEqual(error, Result).

compile_core_file_invalid(TmpDir) ->
    CoreFile = filename:join(TmpDir, "bad.core"),
    OutDir = filename:join(TmpDir, "ebin"),
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy")),
    ok = file:write_file(CoreFile, <<"not core erlang">>),
    Result = beamtalk_build_worker:compile_core_file(CoreFile, OutDir),
    ?assertEqual(error, Result).

%%% ---------------------------------------------------------------
%%% compile_modules/2 — parallel worker pool
%%% ---------------------------------------------------------------

compile_modules_test_() ->
    {setup, fun setup_temp_dir/0, fun cleanup_temp_dir/1, fun(TmpDir) ->
        [
            {"compiles multiple .core files in parallel", fun() ->
                compile_modules_batch(TmpDir)
            end},
            {"returns error for invalid output directory", fun() ->
                compile_modules_bad_outdir()
            end},
            {"handles mix of valid and invalid files", fun() -> compile_modules_mixed(TmpDir) end}
        ]
    end}.

compile_modules_batch(TmpDir) ->
    OutDir = filename:join(TmpDir, "batch_out"),
    CoreFile1 = filename:join(TmpDir, "mod_a.core"),
    CoreFile2 = filename:join(TmpDir, "mod_b.core"),
    ok = file:write_file(CoreFile1, core_erlang_module(mod_a)),
    ok = file:write_file(CoreFile2, core_erlang_module(mod_b)),
    {ok, Modules} = beamtalk_build_worker:compile_modules(OutDir, [CoreFile1, CoreFile2]),
    ?assertEqual(2, length(Modules)),
    ?assert(lists:member(mod_a, Modules)),
    ?assert(lists:member(mod_b, Modules)).

compile_modules_bad_outdir() ->
    %% Create a regular file — compile_modules can't create a directory inside a file
    TmpDir = setup_temp_dir(),
    try
        BlockingFile = filename:join(TmpDir, "not_a_dir"),
        ok = file:write_file(BlockingFile, <<>>),
        BadOutDir = filename:join(BlockingFile, "ebin"),
        Result = beamtalk_build_worker:compile_modules(BadOutDir, ["dummy.core"]),
        ?assertEqual(error, Result)
    after
        cleanup_temp_dir(TmpDir)
    end.

compile_modules_mixed(TmpDir) ->
    OutDir = filename:join(TmpDir, "mixed_out"),
    GoodFile = filename:join(TmpDir, "good.core"),
    BadFile = filename:join(TmpDir, "bad.core"),
    ok = file:write_file(GoodFile, core_erlang_module(good)),
    ok = file:write_file(BadFile, <<"not valid core erlang">>),
    %% Should return error since one file failed
    Result = beamtalk_build_worker:compile_modules(OutDir, [GoodFile, BadFile]),
    ?assertEqual(error, Result).

%%% ---------------------------------------------------------------
%%% Helpers
%%% ---------------------------------------------------------------

setup_temp_dir() ->
    TmpBase =
        case os:getenv("TMPDIR") of
            false ->
                case os:getenv("TEMP") of
                    false -> "/tmp";
                    "" -> "/tmp";
                    WinDir -> WinDir
                end;
            "" ->
                "/tmp";
            Dir ->
                Dir
        end,
    TmpDir = filename:join([
        TmpBase,
        "beamtalk_build_worker_test_" ++
            integer_to_list(erlang:unique_integer([positive]))
    ]),
    ok = filelib:ensure_dir(filename:join(TmpDir, "dummy")),
    TmpDir.

cleanup_temp_dir(TmpDir) ->
    remove_dir_recursive(TmpDir).

remove_dir_recursive(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foreach(
                fun(F) ->
                    Path = filename:join(Dir, F),
                    case filelib:is_dir(Path) of
                        true -> remove_dir_recursive(Path);
                        false -> file:delete(Path)
                    end
                end,
                Files
            ),
            file:del_dir(Dir),
            ok;
        {error, _} ->
            ok
    end.

%% Minimal valid Core Erlang module with a hello/0 function.
valid_core_erlang_source() ->
    <<
        "module 'test_build_worker_mod' ['hello'/0]\n"
        "  attributes []\n"
        "  'hello'/0 = fun () -> 'world'\n"
        "end\n"
    >>.

%% Generate a Core Erlang module with a given name.
core_erlang_module(Name) ->
    NameBin = atom_to_binary(Name, utf8),
    <<"module '", NameBin/binary,
        "' ['hello'/0]\n"
        "  attributes []\n"
        "  'hello'/0 = fun () -> 'ok'\n"
        "end\n">>.
