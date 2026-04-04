%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Unit tests for beamtalk_native_docs module (BT-1851, BT-1876).
%%
%% Tests the EEP-48 documentation reader against real OTP modules.
%% Uses `lists` and `maps` as known-good sources of EEP-48 docs.
%% Also tests security hardening: safe binary_to_term, non-v1 format
%% handling, and binary path support from code:which/1.
-module(beamtalk_native_docs_tests).
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% lookup/3 Tests — Success Path
%%% ============================================================================

lookup_known_function_test() ->
    %% lists:reverse/1 is documented in all OTP versions with EEP-48
    Result = beamtalk_native_docs:lookup(lists, reverse, 1),
    ?assertMatch(#{doc := _, sig := _, examples := _}, Result),
    #{sig := Sig} = Result,
    ?assertNotEqual(<<>>, Sig).

lookup_returns_doc_text_test() ->
    #{doc := Doc} = beamtalk_native_docs:lookup(lists, reverse, 1),
    %% Doc should contain something about reversing
    ?assert(byte_size(Doc) > 0).

lookup_returns_signature_test() ->
    #{sig := Sig} = beamtalk_native_docs:lookup(lists, sort, 1),
    %% Signature should mention "sort"
    ?assertNotEqual(nomatch, binary:match(Sig, <<"sort">>)).

lookup_maps_module_test() ->
    %% Verify it works for modules other than lists
    Result = beamtalk_native_docs:lookup(maps, get, 2),
    ?assertMatch(#{doc := _, sig := _, examples := _}, Result).

%%% ============================================================================
%%% lookup/3 Tests — No Docs / Error Path
%%% ============================================================================

lookup_nonexistent_module_test() ->
    ?assertEqual(
        {error, no_docs},
        beamtalk_native_docs:lookup(nonexistent_module_xyz, foo, 0)
    ).

lookup_nonexistent_function_test() ->
    ?assertEqual(
        {error, no_docs},
        beamtalk_native_docs:lookup(lists, nonexistent_function_xyz, 0)
    ).

lookup_wrong_arity_test() ->
    %% lists:reverse/1 exists, but lists:reverse/99 does not
    ?assertEqual(
        {error, no_docs},
        beamtalk_native_docs:lookup(lists, reverse, 99)
    ).

lookup_preloaded_module_test() ->
    %% erlang is preloaded — no .beam file on disk
    ?assertEqual(
        {error, no_docs},
        beamtalk_native_docs:lookup(erlang, self, 0)
    ).

%%% ============================================================================
%%% lookup/3 Tests — Undocumented (none/hidden) Functions
%%% ============================================================================

lookup_hidden_function_test() ->
    %% lists:zf/2 is hidden in OTP docs — should return {error, no_docs}
    ?assertEqual(
        {error, no_docs},
        beamtalk_native_docs:lookup(lists, zf, 2)
    ).

lookup_undocumented_function_has_signature_test() ->
    %% Functions with `none` doc still have a signature
    %% Find one by checking lists module for `none` entries
    %% lists:umerge/1 is typically undocumented (none) in some OTP versions
    %% We test the general principle: if a function has `none` doc,
    %% we still return a result with empty doc and a signature
    case beamtalk_native_docs:lookup(lists, suffix, 2) of
        #{doc := Doc, sig := Sig} ->
            %% This is a `none`-doc function — doc may be empty, sig should exist
            ?assert(is_binary(Doc)),
            ?assert(is_binary(Sig));
        {error, no_docs} ->
            %% Also acceptable — it may be hidden or missing entirely
            ok
    end.

%%% ============================================================================
%%% module_doc/1 Tests — Success Path
%%% ============================================================================

module_doc_known_module_test() ->
    Result = beamtalk_native_docs:module_doc(lists),
    ?assertMatch(#{doc := _}, Result),
    #{doc := Doc} = Result,
    ?assert(byte_size(Doc) > 0).

module_doc_maps_test() ->
    Result = beamtalk_native_docs:module_doc(maps),
    ?assertMatch(#{doc := _}, Result).

%%% ============================================================================
%%% module_doc/1 Tests — Error Path
%%% ============================================================================

module_doc_nonexistent_test() ->
    ?assertEqual(
        {error, no_docs},
        beamtalk_native_docs:module_doc(nonexistent_module_xyz)
    ).

module_doc_preloaded_test() ->
    ?assertEqual(
        {error, no_docs},
        beamtalk_native_docs:module_doc(erlang)
    ).

%%% ============================================================================
%%% Examples Extraction Tests
%%% ============================================================================

lookup_with_examples_test() ->
    %% Many OTP functions have examples sections
    %% lists:sort/1 often has examples in OTP 25+
    case beamtalk_native_docs:lookup(lists, sort, 1) of
        #{examples := Examples} when byte_size(Examples) > 0 ->
            %% Examples section was found and extracted
            ?assert(true);
        #{examples := <<>>} ->
            %% No examples section — still valid
            ?assert(true);
        {error, no_docs} ->
            %% Shouldn't happen for lists:sort/1
            ?assert(false)
    end.

%%% ============================================================================
%%% Non-v1 Docs Format Tests (BT-1876)
%%% ============================================================================

non_v1_docs_format_lookup_test() ->
    {BeamPath, Module} = create_beam_with_docs_chunk({docs_v2, fake, data}),
    try
        ?assertEqual(
            {error, unsupported_format},
            beamtalk_native_docs:lookup(Module, foo, 0)
        )
    after
        cleanup_fake_beam(BeamPath, Module)
    end.

non_v1_docs_format_module_doc_test() ->
    {BeamPath, Module} = create_beam_with_docs_chunk({docs_v2, fake, data}),
    try
        ?assertEqual(
            {error, unsupported_format},
            beamtalk_native_docs:module_doc(Module)
        )
    after
        cleanup_fake_beam(BeamPath, Module)
    end.

%%% ============================================================================
%%% Corrupt Docs Chunk Tests (BT-1876)
%%% ============================================================================

corrupt_docs_chunk_lookup_test() ->
    {BeamPath, Module} = create_beam_with_raw_docs_chunk(<<"not valid erlang term">>),
    try
        ?assertEqual(
            {error, no_docs},
            beamtalk_native_docs:lookup(Module, foo, 0)
        )
    after
        cleanup_fake_beam(BeamPath, Module)
    end.

corrupt_docs_chunk_module_doc_test() ->
    {BeamPath, Module} = create_beam_with_raw_docs_chunk(<<"not valid erlang term">>),
    try
        ?assertEqual(
            {error, no_docs},
            beamtalk_native_docs:module_doc(Module)
        )
    after
        cleanup_fake_beam(BeamPath, Module)
    end.

%%% ============================================================================
%%% EEP-48 Doc Chunk Generation Tests (BT-1900)
%%% ============================================================================

compile_with_doc_attributes_has_docs_chunk_test() ->
    %% Compile a module with -doc attributes via compile:file and verify
    %% that the resulting .beam contains a Docs chunk (OTP 27+).
    {BeamPath, Module} = create_beam_with_doc_attributes(),
    try
        Result = beamtalk_native_docs:lookup(Module, greet, 1),
        ?assertMatch(#{doc := _, sig := _, examples := _}, Result),
        #{doc := Doc} = Result,
        %% The -doc text should appear in the docs chunk
        ?assertNotEqual(<<>>, Doc),
        ?assertNotEqual(nomatch, binary:match(Doc, <<"greeting">>))
    after
        cleanup_fake_beam(BeamPath, Module)
    end.

compile_with_doc_false_is_hidden_test() ->
    %% Functions marked with -doc false should be hidden in the Docs chunk.
    {BeamPath, Module} = create_beam_with_doc_attributes(),
    try
        ?assertEqual(
            {error, no_docs},
            beamtalk_native_docs:lookup(Module, internal, 0)
        )
    after
        cleanup_fake_beam(BeamPath, Module)
    end.

compile_with_moduledoc_test() ->
    %% Module-level -moduledoc should appear in module_doc/1.
    {BeamPath, Module} = create_beam_with_doc_attributes(),
    try
        Result = beamtalk_native_docs:module_doc(Module),
        ?assertMatch(#{doc := _}, Result),
        #{doc := Doc} = Result,
        ?assertNotEqual(nomatch, binary:match(Doc, <<"documented module">>))
    after
        cleanup_fake_beam(BeamPath, Module)
    end.

%%% ============================================================================
%%% Helpers
%%% ============================================================================

%% @private Create a temporary .beam file with a custom Docs chunk.
%% The DocsTerm is serialized with term_to_binary and embedded
%% into the Docs chunk of a minimal .beam file.
create_beam_with_docs_chunk(DocsTerm) ->
    DocsBin = term_to_binary(DocsTerm),
    create_beam_with_raw_docs_chunk(DocsBin).

%% @private Create a temporary .beam with raw bytes in the Docs chunk.
create_beam_with_raw_docs_chunk(RawDocsBin) ->
    Module = beamtalk_native_docs_test_fake,
    %% Compile a minimal module to get a valid .beam
    Forms = [
        {attribute, 1, module, Module},
        {attribute, 2, export, [{foo, 0}]},
        {function, 3, foo, 0, [{clause, 3, [], [], [{atom, 3, ok}]}]}
    ],
    {ok, Module, BeamBin} = compile:forms(Forms),
    %% Inject custom Docs chunk into the beam binary
    {ok, _, Chunks} = beam_lib:all_chunks(BeamBin),
    Chunks2 = [{"Docs", RawDocsBin} | lists:keydelete("Docs", 1, Chunks)],
    {ok, NewBeamBin} = beam_lib:build_module(Chunks2),
    %% Write to a temp file in the test working directory
    BeamPath = atom_to_list(Module) ++ ".beam",
    ok = file:write_file(BeamPath, NewBeamBin),
    %% Load the module from the file so code:which/1 finds it
    {module, Module} = code:load_abs(filename:rootname(BeamPath)),
    {BeamPath, Module}.

%% @private Create a temporary .beam with -doc/-moduledoc attributes (OTP 27+).
%% Uses compile:file/2 to produce a beam binary with a real Docs chunk.
create_beam_with_doc_attributes() ->
    Module = beamtalk_native_docs_test_docattr,
    %% Write a temporary .erl file with -doc attributes, then compile it.
    %% compile:forms/2 does not support -doc attributes directly, so we
    %% write source and compile via compile:file/2.
    SrcFile = atom_to_list(Module) ++ ".erl",
    Source = [
        "-module(",
        atom_to_list(Module),
        ").\n",
        "-moduledoc \"A documented module for testing.\".\n",
        "-export([greet/1, internal/0]).\n",
        "\n",
        "-doc \"Returns a greeting for the given name.\".\n",
        "-spec greet(binary()) -> binary().\n",
        "greet(Name) -> <<\"Hello, \", Name/binary>>.\n",
        "\n",
        "-doc false.\n",
        "-spec internal() -> ok.\n",
        "internal() -> ok.\n"
    ],
    ok = file:write_file(SrcFile, Source),
    {ok, Module} = compile:file(SrcFile, [debug_info, {outdir, "."}]),
    BeamPath = atom_to_list(Module) ++ ".beam",
    {module, Module} = code:load_abs(filename:rootname(BeamPath)),
    _ = file:delete(SrcFile),
    {BeamPath, Module}.

%% @private Clean up a fake beam file and purge the module.
cleanup_fake_beam(BeamPath, Module) ->
    code:purge(Module),
    code:delete(Module),
    code:purge(Module),
    file:delete(BeamPath).
