%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc On-demand EEP-48 documentation reader for Erlang modules.
%%
%%% **DDD Context:** Runtime Context
%%
%% Reads EEP-48 `"Docs"` chunks from compiled `.beam` files at runtime.
%% Single codepath shared by REPL (`:help Erlang <module>`), LSP hover,
%% and MCP tools.
%%
%% Always reads fresh from disk via `beam_lib:chunks/2` — no caching.
%% Graceful fallback when the Docs chunk is absent (pre-OTP 25, stripped
%% modules, or preloaded modules).
%%
%% @see <a href="https://www.erlang.org/eeps/eep-0048">EEP-48</a>
-module(beamtalk_native_docs).

-export([
    lookup/3,
    module_doc/1
]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Look up documentation for a specific function in a module.
%%
%% Reads the EEP-48 Docs chunk from the module's `.beam` file and extracts
%% the documentation entry for the given function and arity.
%%
%% Returns `#{doc => Binary, sig => Binary, examples => Binary}` on success,
%% or `{error, no_docs}` when:
%% - The module doesn't exist or is preloaded
%% - The Docs chunk is absent
%% - No documentation entry exists for the given function/arity
-spec lookup(module(), atom(), non_neg_integer()) ->
    #{doc := binary(), sig := binary(), examples := binary()}
    | {error, no_docs}.
lookup(Module, Function, Arity) ->
    case read_docs_chunk(Module) of
        {ok, {docs_v1, _Anno, _Lang, _Format, _ModDoc, _Meta, FDocs}} ->
            find_function_doc(Function, Arity, FDocs);
        {error, _Reason} ->
            {error, no_docs}
    end.

%% @doc Look up module-level documentation.
%%
%% Returns `#{doc => Binary}` with the module's top-level documentation,
%% or `{error, no_docs}` when the module has no Docs chunk or the module
%% doc is marked as `none` or `hidden`.
-spec module_doc(module()) ->
    #{doc := binary()} | {error, no_docs}.
module_doc(Module) ->
    case read_docs_chunk(Module) of
        {ok, {docs_v1, _Anno, _Lang, _Format, ModDoc, _Meta, _FDocs}} ->
            extract_module_doc(ModDoc);
        {error, _Reason} ->
            {error, no_docs}
    end.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @private Read the raw Docs chunk from a module's .beam file.
-spec read_docs_chunk(module()) ->
    {ok, tuple()} | {error, term()}.
read_docs_chunk(Module) ->
    case code:which(Module) of
        non_existing ->
            {error, module_not_found};
        preloaded ->
            %% Preloaded modules (erlang, init, etc.) have no .beam file on disk
            {error, preloaded};
        cover_compiled ->
            {error, cover_compiled};
        BeamPath when is_list(BeamPath) ->
            read_docs_from_beam(BeamPath)
    end.

%% @private Read and decode the Docs chunk from a .beam file path.
-spec read_docs_from_beam(file:filename()) ->
    {ok, tuple()} | {error, term()}.
read_docs_from_beam(BeamPath) ->
    case beam_lib:chunks(BeamPath, ["Docs"]) of
        {ok, {_Mod, [{"Docs", DocsBin}]}} ->
            try
                {ok, binary_to_term(DocsBin)}
            catch
                _:_ -> {error, corrupt_docs}
            end;
        {error, _Mod, {missing_chunk, _, _}} ->
            {error, no_docs_chunk};
        {error, _Mod, Reason} ->
            {error, Reason}
    end.

%% @private Find a specific function's documentation in the function doc list.
-spec find_function_doc(atom(), non_neg_integer(), list()) ->
    #{doc := binary(), sig := binary(), examples := binary()}
    | {error, no_docs}.
find_function_doc(Function, Arity, FDocs) ->
    Matches = [
        Entry
     || {{function, F, A}, _Anno, _Sig, _Doc, _Meta} = Entry <- FDocs,
        F =:= Function,
        A =:= Arity
    ],
    case Matches of
        [{{function, _, _}, _Anno, Sigs, Doc, _Meta}] ->
            format_function_doc(Sigs, Doc);
        [] ->
            {error, no_docs}
    end.

%% @private Format a function doc entry into the return map.
-spec format_function_doc(list(), map() | none | hidden) ->
    #{doc := binary(), sig := binary(), examples := binary()}
    | {error, no_docs}.
format_function_doc(_Sigs, hidden) ->
    {error, no_docs};
format_function_doc(Sigs, none) ->
    %% Function exists but has no doc text — still return the signature
    #{
        doc => <<>>,
        sig => format_signatures(Sigs),
        examples => <<>>
    };
format_function_doc(Sigs, DocMap) when is_map(DocMap) ->
    DocText = extract_lang_doc(DocMap),
    {MainDoc, Examples} = split_examples(DocText),
    #{
        doc => MainDoc,
        sig => format_signatures(Sigs),
        examples => Examples
    }.

%% @private Extract the documentation text, preferring English.
-spec extract_lang_doc(map()) -> binary().
extract_lang_doc(DocMap) ->
    case maps:find(<<"en">>, DocMap) of
        {ok, Text} ->
            Text;
        error ->
            %% Fall back to first available language
            case maps:values(DocMap) of
                [First | _] -> First;
                [] -> <<>>
            end
    end.

%% @private Split doc text into main documentation and examples section.
%% Looks for "## Examples" or "### Examples" markdown headers.
-spec split_examples(binary()) -> {binary(), binary()}.
split_examples(DocText) ->
    %% Look for Examples section header (## Examples or ### Examples)
    case binary:match(DocText, [<<"## Examples">>, <<"### Examples">>]) of
        {Start, _Len} ->
            MainDoc = string:trim(binary:part(DocText, 0, Start), trailing),
            ExamplesSection = binary:part(DocText, Start, byte_size(DocText) - Start),
            {MainDoc, ExamplesSection};
        nomatch ->
            {DocText, <<>>}
    end.

%% @private Format signature list into a single binary.
-spec format_signatures([binary()]) -> binary().
format_signatures([]) -> <<>>;
format_signatures([Sig]) -> Sig;
format_signatures(Sigs) -> iolist_to_binary(lists:join(<<"\n">>, Sigs)).

%% @private Extract module-level documentation.
-spec extract_module_doc(map() | none | hidden) ->
    #{doc := binary()} | {error, no_docs}.
extract_module_doc(none) ->
    {error, no_docs};
extract_module_doc(hidden) ->
    {error, no_docs};
extract_module_doc(DocMap) when is_map(DocMap) ->
    case extract_lang_doc(DocMap) of
        <<>> -> {error, no_docs};
        Text -> #{doc => Text}
    end.
