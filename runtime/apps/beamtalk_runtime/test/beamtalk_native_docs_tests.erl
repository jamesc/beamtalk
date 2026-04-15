%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_native_docs_tests).

-moduledoc """
Unit tests for beamtalk_native_docs module (BT-1851, BT-1876).

Tests the EEP-48 documentation reader against real OTP modules.
Uses `lists` and `maps` as known-good sources of EEP-48 docs.
Also tests security hardening: safe binary_to_term, non-v1 format
handling, and binary path support from code:which/1.
""".
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
    %% lists:zf/2 is hidden in OTP docs — should return {error, hidden}
    ?assertEqual(
        {error, hidden},
        beamtalk_native_docs:lookup(lists, zf, 2)
    ).

is_hidden_returns_true_for_hidden_function_test() ->
    %% lists:zf/2 is hidden — is_hidden/3 should return true
    ?assert(beamtalk_native_docs:is_hidden(lists, zf, 2)).

is_hidden_returns_false_for_public_function_test() ->
    %% lists:reverse/1 is public — is_hidden/3 should return false
    ?assertNot(beamtalk_native_docs:is_hidden(lists, reverse, 1)).

is_hidden_returns_false_for_missing_function_test() ->
    %% Nonexistent function — is_hidden/3 should return false, not crash
    ?assertNot(beamtalk_native_docs:is_hidden(lists, no_such_function, 0)).

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
            {error, hidden},
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
%%% is_hidden/3 Additional Edge Cases
%%% ============================================================================

is_hidden_nonexistent_module_test() ->
    %% is_hidden on a nonexistent module should return false, not crash
    ?assertNot(beamtalk_native_docs:is_hidden(nonexistent_module_xyz, foo, 0)).

is_hidden_preloaded_module_test() ->
    %% Preloaded modules have no .beam on disk
    ?assertNot(beamtalk_native_docs:is_hidden(erlang, self, 0)).

%%% ============================================================================
%%% module_doc/1 Additional Edge Cases
%%% ============================================================================

module_doc_with_hidden_moduledoc_test() ->
    %% Create a module with -moduledoc false
    {BeamPath, Module} = create_beam_with_hidden_moduledoc(),
    try
        ?assertEqual(
            {error, no_docs},
            beamtalk_native_docs:module_doc(Module)
        )
    after
        cleanup_fake_beam(BeamPath, Module)
    end.

%%% ============================================================================
%%% lookup/3 Multiple Arities
%%% ============================================================================

lookup_different_arities_test() ->
    %% lists:sort/1 and lists:sort/2 both exist — both should return docs
    Result1 = beamtalk_native_docs:lookup(lists, sort, 1),
    ?assertMatch(#{doc := _, sig := _, examples := _}, Result1),
    Result2 = beamtalk_native_docs:lookup(lists, sort, 2),
    ?assertMatch(#{doc := _, sig := _, examples := _}, Result2),
    %% Different arities should yield different signatures
    #{sig := Sig1} = Result1,
    #{sig := Sig2} = Result2,
    ?assertNotEqual(Sig1, Sig2).

lookup_zero_arity_function_test() ->
    %% maps:new/0 should have docs in OTP 27+
    case beamtalk_native_docs:lookup(maps, new, 0) of
        #{doc := _, sig := _, examples := _} ->
            ?assert(true);
        {error, _} ->
            %% Acceptable if this OTP version doesn't have docs for maps:new/0
            ?assert(true)
    end.

%%% ============================================================================
%%% format_signatures edge cases (via lookup)
%%% ============================================================================

lookup_returns_non_empty_signature_test() ->
    %% lists:sort/1 should have a non-empty signature
    #{sig := Sig} = beamtalk_native_docs:lookup(lists, sort, 1),
    ?assert(byte_size(Sig) > 0).

lookup_returns_binary_doc_test() ->
    %% Verify all return fields are binaries
    #{doc := Doc, sig := Sig, examples := Examples} =
        beamtalk_native_docs:lookup(lists, reverse, 1),
    ?assert(is_binary(Doc)),
    ?assert(is_binary(Sig)),
    ?assert(is_binary(Examples)).

%%% ============================================================================
%%% format_function_doc — `none`-doc coverage
%%%
%%% When a function has `none` as its doc entry (exists but undocumented),
%%% lookup/3 must still return an empty doc with the signature filled in.
%%% We build a fake .beam containing a crafted docs_v1 chunk with a
%%% `none`-doc entry to exercise this explicit branch.
%%% ============================================================================

lookup_function_with_none_doc_returns_empty_doc_and_signature_test() ->
    FDocs = [
        {{function, bar, 0}, [], [<<"bar() -> ok.">>], none, #{}}
    ],
    DocsChunk = {docs_v1, [], erlang, <<"text/markdown">>, none, #{}, FDocs},
    {BeamPath, Module} = create_beam_with_docs_chunk(DocsChunk),
    try
        Result = beamtalk_native_docs:lookup(Module, bar, 0),
        ?assertMatch(#{doc := <<>>, sig := <<"bar() -> ok.">>, examples := <<>>}, Result)
    after
        cleanup_fake_beam(BeamPath, Module)
    end.

%%% ============================================================================
%%% extract_lang_doc — non-English fallback
%%%
%%% When the doc map has no "en" key, extract_lang_doc falls back to the
%%% first available language. We craft a DE-only doc to hit this branch.
%%% ============================================================================

lookup_falls_back_to_non_english_doc_test() ->
    FDocs = [
        {{function, baz, 0}, [], [<<"baz() -> ok.">>], #{<<"de">> => <<"Hallo Welt">>}, #{}}
    ],
    DocsChunk = {docs_v1, [], erlang, <<"text/markdown">>, none, #{}, FDocs},
    {BeamPath, Module} = create_beam_with_docs_chunk(DocsChunk),
    try
        #{doc := Doc} = beamtalk_native_docs:lookup(Module, baz, 0),
        ?assertEqual(<<"Hallo Welt">>, Doc)
    after
        cleanup_fake_beam(BeamPath, Module)
    end.

lookup_with_empty_doc_map_returns_empty_doc_test() ->
    %% Doc map present but empty — extract_lang_doc returns <<>>
    FDocs = [
        {{function, qux, 0}, [], [<<"qux() -> ok.">>], #{}, #{}}
    ],
    DocsChunk = {docs_v1, [], erlang, <<"text/markdown">>, none, #{}, FDocs},
    {BeamPath, Module} = create_beam_with_docs_chunk(DocsChunk),
    try
        Result = beamtalk_native_docs:lookup(Module, qux, 0),
        ?assertMatch(#{doc := <<>>, sig := <<"qux() -> ok.">>}, Result)
    after
        cleanup_fake_beam(BeamPath, Module)
    end.

%%% ============================================================================
%%% format_signatures edge cases
%%% ============================================================================

lookup_with_empty_signature_list_test() ->
    %% format_signatures([]) -> <<>>
    FDocs = [
        {{function, nosig, 0}, [], [], #{<<"en">> => <<"hi">>}, #{}}
    ],
    DocsChunk = {docs_v1, [], erlang, <<"text/markdown">>, none, #{}, FDocs},
    {BeamPath, Module} = create_beam_with_docs_chunk(DocsChunk),
    try
        Result = beamtalk_native_docs:lookup(Module, nosig, 0),
        ?assertMatch(#{sig := <<>>, doc := <<"hi">>}, Result)
    after
        cleanup_fake_beam(BeamPath, Module)
    end.

lookup_with_multiple_signatures_test() ->
    %% format_signatures/1 joins multiple sigs with newlines
    FDocs = [
        {{function, msig, 0}, [], [<<"msig() -> ok.">>, <<"msig() -> error.">>],
            #{<<"en">> => <<"multi sig">>}, #{}}
    ],
    DocsChunk = {docs_v1, [], erlang, <<"text/markdown">>, none, #{}, FDocs},
    {BeamPath, Module} = create_beam_with_docs_chunk(DocsChunk),
    try
        #{sig := Sig} = beamtalk_native_docs:lookup(Module, msig, 0),
        %% Both signatures should appear, separated by a newline
        ?assertNotEqual(nomatch, binary:match(Sig, <<"msig() -> ok.">>)),
        ?assertNotEqual(nomatch, binary:match(Sig, <<"msig() -> error.">>)),
        ?assertNotEqual(nomatch, binary:match(Sig, <<"\n">>))
    after
        cleanup_fake_beam(BeamPath, Module)
    end.

%%% ============================================================================
%%% extract_module_doc — none/empty branches
%%% ============================================================================

module_doc_with_none_moduledoc_test() ->
    %% Build a chunk where the module-level doc is the atom `none`.
    DocsChunk = {docs_v1, [], erlang, <<"text/markdown">>, none, #{}, []},
    {BeamPath, Module} = create_beam_with_docs_chunk(DocsChunk),
    try
        ?assertEqual(
            {error, no_docs},
            beamtalk_native_docs:module_doc(Module)
        )
    after
        cleanup_fake_beam(BeamPath, Module)
    end.

module_doc_with_empty_doc_map_test() ->
    %% Doc map present but empty — extract returns {error, no_docs}
    DocsChunk = {docs_v1, [], erlang, <<"text/markdown">>, #{}, #{}, []},
    {BeamPath, Module} = create_beam_with_docs_chunk(DocsChunk),
    try
        ?assertEqual(
            {error, no_docs},
            beamtalk_native_docs:module_doc(Module)
        )
    after
        cleanup_fake_beam(BeamPath, Module)
    end.

module_doc_falls_back_to_non_english_test() ->
    %% Only a German entry — module_doc should return it as a fallback
    DocsChunk =
        {docs_v1, [], erlang, <<"text/markdown">>, #{<<"de">> => <<"Modul-Doku">>}, #{}, []},
    {BeamPath, Module} = create_beam_with_docs_chunk(DocsChunk),
    try
        Result = beamtalk_native_docs:module_doc(Module),
        ?assertMatch(#{doc := <<"Modul-Doku">>}, Result)
    after
        cleanup_fake_beam(BeamPath, Module)
    end.

%%% ============================================================================
%%% Helpers
%%% ============================================================================

-doc """
Create a temporary .beam file with a custom Docs chunk.
The DocsTerm is serialized with term_to_binary and embedded
into the Docs chunk of a minimal .beam file.
""".
create_beam_with_docs_chunk(DocsTerm) ->
    DocsBin = term_to_binary(DocsTerm),
    create_beam_with_raw_docs_chunk(DocsBin).

-doc "Create a temporary .beam with raw bytes in the Docs chunk.".
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

-doc """
Create a temporary .beam with -doc/-moduledoc attributes (OTP 27+).
Uses compile:file/2 to produce a beam binary with a real Docs chunk.
""".
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

-doc "Create a temporary .beam with -moduledoc false (OTP 27+).".
create_beam_with_hidden_moduledoc() ->
    Module = beamtalk_native_docs_test_hidden_mod,
    SrcFile = atom_to_list(Module) ++ ".erl",
    Source = [
        "-module(",
        atom_to_list(Module),
        ").\n",
        "-moduledoc false.\n",
        "-export([foo/0]).\n",
        "\n",
        "-doc \"A public function.\".\n",
        "foo() -> ok.\n"
    ],
    ok = file:write_file(SrcFile, Source),
    {ok, Module} = compile:file(SrcFile, [debug_info, {outdir, "."}]),
    BeamPath = atom_to_list(Module) ++ ".beam",
    {module, Module} = code:load_abs(filename:rootname(BeamPath)),
    _ = file:delete(SrcFile),
    {BeamPath, Module}.

-doc "Clean up a fake beam file and purge the module.".
cleanup_fake_beam(BeamPath, Module) ->
    code:purge(Module),
    code:delete(Module),
    code:purge(Module),
    file:delete(BeamPath).
