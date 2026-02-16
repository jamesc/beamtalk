%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Tests for beamtalk_build_worker (ADR 0022, Phase 3).
%%
%% Tests the batch compiler worker's in-memory compilation pipeline,
%% file I/O, docs chunk injection, and parallel worker management.

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
    {setup,
     fun setup_temp_dir/0,
     fun cleanup_temp_dir/1,
     fun(TmpDir) ->
         [
          {"compiles a valid .core file to .beam",
           fun() -> compile_core_file_valid(TmpDir) end},
          {"returns error for missing .core file",
           fun() -> compile_core_file_missing(TmpDir) end},
          {"returns error for invalid .core content",
           fun() -> compile_core_file_invalid(TmpDir) end}
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
%%% inject_docs_chunk/3 — EEP-48 doc chunk injection
%%% ---------------------------------------------------------------

inject_docs_chunk_test_() ->
    {setup,
     fun setup_temp_dir/0,
     fun cleanup_temp_dir/1,
     fun(TmpDir) ->
         [
          {"injects docs chunk into .beam file",
           fun() -> inject_docs_chunk_valid(TmpDir) end},
          {"silently ignores missing .docs file",
           fun() -> inject_docs_chunk_no_docs(TmpDir) end},
          {"silently ignores missing .beam file",
           fun() -> inject_docs_chunk_no_beam(TmpDir) end}
         ]
     end}.

inject_docs_chunk_valid(TmpDir) ->
    %% First compile a .core file to get a real .beam
    CoreFile = filename:join(TmpDir, "test_build_worker_mod.core"),
    OutDir = filename:join(TmpDir, "ebin2"),
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy")),
    ok = file:write_file(CoreFile, valid_core_erlang_source()),
    {ok, test_build_worker_mod} = beamtalk_build_worker:compile_core_file(CoreFile, OutDir),
    %% Create a .docs file next to the .core file
    DocsFile = filename:join(TmpDir, "test_build_worker_mod.docs"),
    DocsTerm = {docs_v1, erlang:system_info(otp_release), erlang, <<"text/plain">>,
                #{<<"en">> => <<"Test module doc">>}, #{}, []},
    ok = file:write_file(DocsFile, io_lib:format("~p.", [DocsTerm])),
    %% Inject — should succeed silently (ok)
    ok = beamtalk_build_worker:inject_docs_chunk(CoreFile, test_build_worker_mod, OutDir),
    %% Verify the Docs chunk exists
    BeamFile = filename:join(OutDir, "test_build_worker_mod.beam"),
    {ok, _, AllChunks} = beam_lib:all_chunks(BeamFile),
    DocsChunks = [Data || {"Docs", Data} <- AllChunks],
    ?assertEqual(1, length(DocsChunks)).

inject_docs_chunk_no_docs(TmpDir) ->
    %% No .docs file exists — should return ok (silently ignored)
    CoreFile = filename:join(TmpDir, "no_docs_mod.core"),
    ok = beamtalk_build_worker:inject_docs_chunk(CoreFile, no_docs_mod, TmpDir).

inject_docs_chunk_no_beam(TmpDir) ->
    %% .docs file exists but no .beam — should return ok (silently ignored)
    CoreFile = filename:join(TmpDir, "no_beam_mod.core"),
    DocsFile = filename:join(TmpDir, "no_beam_mod.docs"),
    ok = file:write_file(DocsFile, io_lib:format("~p.", [{docs_v1, [], erlang, none, none, #{}, []}])),
    ok = beamtalk_build_worker:inject_docs_chunk(CoreFile, no_beam_mod, TmpDir).

%%% ---------------------------------------------------------------
%%% compile_modules/2 — parallel worker pool
%%% ---------------------------------------------------------------

compile_modules_test_() ->
    {setup,
     fun setup_temp_dir/0,
     fun cleanup_temp_dir/1,
     fun(TmpDir) ->
         [
          {"compiles multiple .core files in parallel",
           fun() -> compile_modules_batch(TmpDir) end},
          {"returns error for invalid output directory",
           fun() -> compile_modules_bad_outdir() end},
          {"handles mix of valid and invalid files",
           fun() -> compile_modules_mixed(TmpDir) end}
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
    %% /dev/null/ebin cannot be created as a directory
    Result = beamtalk_build_worker:compile_modules("/dev/null/ebin", ["dummy.core"]),
    ?assertEqual(error, Result).

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
    TmpDir = filename:join(["/tmp", "beamtalk_build_worker_test_" ++
                            integer_to_list(erlang:unique_integer([positive]))]),
    ok = filelib:ensure_dir(filename:join(TmpDir, "dummy")),
    TmpDir.

cleanup_temp_dir(TmpDir) ->
    os:cmd("rm -rf " ++ TmpDir),
    ok.

%% Minimal valid Core Erlang module with a hello/0 function.
valid_core_erlang_source() ->
    <<"module 'test_build_worker_mod' ['hello'/0]\n"
      "  attributes []\n"
      "  'hello'/0 = fun () -> 'world'\n"
      "end\n">>.

%% Generate a Core Erlang module with a given name.
core_erlang_module(Name) ->
    NameBin = atom_to_binary(Name, utf8),
    <<"module '", NameBin/binary, "' ['hello'/0]\n"
      "  attributes []\n"
      "  'hello'/0 = fun () -> 'ok'\n"
      "end\n">>.
