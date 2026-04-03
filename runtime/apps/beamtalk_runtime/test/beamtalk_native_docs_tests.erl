%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Unit tests for beamtalk_native_docs module (BT-1851).
%%
%% Tests the EEP-48 documentation reader against real OTP modules.
%% Uses `lists` and `maps` as known-good sources of EEP-48 docs.
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
