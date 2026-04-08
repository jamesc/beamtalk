%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_erlang_help).

%%% **DDD Context:** Runtime Context

-moduledoc """
Erlang FFI help formatting — shared by the REPL `:h Erlang` command,
`BeamtalkInterface erlangHelp:`, and MCP `docs` tool.

All public functions return `{ok, Text}` or `{error, Reason}` — callers
handle protocol encoding.
""".

-export([
    format_module_help/1,
    format_function_help/2,
    available_modules/0
]).

%% Exported for EUnit testing
-ifdef(TEST).
-export([
    format_beamtalk_signature/3,
    dedupe_keyword_aliases/1,
    format_exports_list/1,
    find_function_arities/2
]).
-endif.

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc """
Format help for an entire Erlang module.

Returns type signatures from `-spec` attributes when available, plus
module-level EEP-48 documentation if present.
""".
-spec format_module_help(module()) -> {ok, binary()} | {error, not_found}.
format_module_help(Module) ->
    case code:which(Module) of
        non_existing ->
            {error, not_found};
        preloaded ->
            {ok, format_module_exports(Module)};
        cover_compiled ->
            {ok, format_module_exports(Module)};
        BeamPath when is_list(BeamPath) ->
            {ok, format_module_with_specs(Module, BeamPath)}
    end.

-doc """
Format help for a specific function (or type) in an Erlang module.

Falls through to type definition lookup when no exported function matches.
""".
-spec format_function_help(module(), atom() | binary()) ->
    {ok, binary()} | {error, not_found}.
format_function_help(Module, FunctionBin) when is_binary(FunctionBin) ->
    %% Binary name — atom may not exist yet (user typo).
    %% Try atom first, fall back to binary-only type search.
    try binary_to_existing_atom(FunctionBin, utf8) of
        Function -> format_function_help(Module, Function)
    catch
        error:badarg ->
            format_type_by_name_or_not_found(Module, FunctionBin)
    end;
format_function_help(Module, Function) when is_atom(Function) ->
    case code:which(Module) of
        non_existing ->
            {error, not_found};
        BeamLoc ->
            BeamPath =
                case BeamLoc of
                    preloaded -> preloaded;
                    cover_compiled -> cover_compiled;
                    P when is_list(P) -> P
                end,
            format_function_with_docs(Module, Function, BeamPath)
    end.

-doc """
List available Erlang module names for tab completion.

Returns modules that have either specs or EEP-48 docs, sorted
alphabetically. Filters out Beamtalk-internal compiled modules
(bt@* prefix).
""".
-spec available_modules() -> [binary()].
available_modules() ->
    All = code:all_available(),
    Names = lists:filtermap(
        fun({ModName, _Path, _Loaded}) ->
            Bin =
                if
                    is_atom(ModName) -> atom_to_binary(ModName);
                    is_list(ModName) -> list_to_binary(ModName)
                end,
            %% Skip Beamtalk compiled modules (bt@package@Class)
            case Bin of
                <<"bt@", _/binary>> -> false;
                _ -> {true, Bin}
            end
        end,
        All
    ),
    lists:sort(Names).

%%% ============================================================================
%%% Module-level formatting
%%% ============================================================================

-doc "Format module help when we have a .beam path (specs + docs).".
-spec format_module_with_specs(module(), file:filename()) -> binary().
format_module_with_specs(Module, BeamPath) ->
    ModName = atom_to_binary(Module),
    %% Read type specs from abstract code
    Specs =
        case beamtalk_spec_reader:read_specs(BeamPath) of
            {ok, SpecList} -> SpecList;
            {error, _} -> []
        end,
    %% Read module-level doc
    ModDoc =
        case beamtalk_native_docs:module_doc(Module) of
            #{doc := ModDocText} -> ModDocText;
            {error, _} -> <<>>
        end,
    %% Build output
    Header = iolist_to_binary([<<"Erlang module: ">>, ModName, <<"\n">>]),
    DocSection =
        case ModDoc of
            <<>> -> <<>>;
            _ -> iolist_to_binary([<<"\n">>, ModDoc, <<"\n">>])
        end,
    FnSection =
        case Specs of
            [] ->
                %% No specs — try EEP-48 signatures, then fall back to exports list
                format_eep48_signatures_or_exports(Module);
            _ ->
                format_specs_list(Module, Specs)
        end,
    iolist_to_binary([Header, DocSection, <<"\n">>, FnSection]).

-doc "Format module help when we only have exports (preloaded/cover_compiled).".
-spec format_module_exports(module()) -> binary().
format_module_exports(Module) ->
    ModName = atom_to_binary(Module),
    Header = iolist_to_binary([<<"Erlang module: ">>, ModName, <<"\n">>]),
    FnSection = format_exports_list(Module),
    iolist_to_binary([Header, <<"\n">>, FnSection]).

%%% ============================================================================
%%% Function-level formatting
%%% ============================================================================

-doc "Format help for a specific function, combining specs and docs.".
-spec format_function_with_docs(
    module(),
    atom(),
    file:filename() | preloaded | cover_compiled
) -> {ok, binary()} | {error, not_found}.
format_function_with_docs(Module, Function, BeamPath) ->
    FnName = atom_to_binary(Function),
    ModName = atom_to_binary(Module),
    %% Find all arities for this function
    Arities = find_function_arities(Module, Function),
    %% Single lookup per arity: filter hidden and collect docs in one pass
    ArityDocs = [
        {A, beamtalk_native_docs:lookup(Module, Function, A)}
     || A <- Arities
    ],
    VisibleArities = [A || {A, Doc} <- ArityDocs, Doc =/= {error, hidden}],
    case VisibleArities of
        [] ->
            %% No visible exported function — fall through to type lookup
            format_type_or_not_found(Module, Function, BeamPath);
        _ ->
            %% Read specs and raw specs if we have a beam path
            {Specs, RawSpecs} =
                case BeamPath of
                    preloaded ->
                        {[], []};
                    cover_compiled ->
                        {[], []};
                    Path ->
                        MappedSpecs =
                            case beamtalk_spec_reader:read_specs(Path) of
                                {ok, AllSpecs} ->
                                    [
                                        S
                                     || S <- AllSpecs,
                                        maps:get(name, S) =:= Function orelse
                                            maps:get(name, S) =:= FnName
                                    ];
                                {error, _} ->
                                    []
                            end,
                        OrigSpecs =
                            case beamtalk_spec_reader:read_raw_specs(Path) of
                                {ok, AllRaw} ->
                                    [
                                        {N, A, Form}
                                     || {N, A, Form} <- AllRaw,
                                        N =:= Function
                                    ];
                                {error, _} ->
                                    []
                            end,
                        {MappedSpecs, OrigSpecs}
                end,
            %% Build Erlang spec section (original -spec lines)
            ErlSpecSection =
                case RawSpecs of
                    [] ->
                        <<>>;
                    _ ->
                        ErlLines = lists:map(
                            fun({_N, _A, Form}) ->
                                iolist_to_binary([
                                    <<"  Erlang: ">>,
                                    beamtalk_spec_reader:format_erlang_spec(Form)
                                ])
                            end,
                            RawSpecs
                        ),
                        iolist_to_binary(lists:join(<<"\n">>, ErlLines))
                end,
            %% Build Beamtalk signature section
            BtSigSection =
                case Specs of
                    [] ->
                        %% No specs — show arity info
                        ArityLines = [
                            iolist_to_binary([
                                <<"  ">>, FnName, <<"/">>, integer_to_binary(A)
                            ])
                         || A <- VisibleArities
                        ],
                        iolist_to_binary(lists:join(<<"\n">>, ArityLines));
                    _ ->
                        SigLines = lists:map(
                            fun(Spec) ->
                                Params = maps:get(params, Spec, []),
                                RetType = maps:get(return_type, Spec, <<"Dynamic">>),
                                iolist_to_binary([
                                    <<"  ">>,
                                    format_beamtalk_signature(FnName, Params, RetType)
                                ])
                            end,
                            Specs
                        ),
                        iolist_to_binary(lists:join(<<"\n">>, SigLines))
                end,
            %% Combine Erlang spec + Beamtalk signature
            SigSection =
                case ErlSpecSection of
                    <<>> -> BtSigSection;
                    _ -> iolist_to_binary([ErlSpecSection, <<"\n">>, BtSigSection])
                end,
            %% Extract docs from pre-fetched lookups (no second disk read)
            DocSections = lists:filtermap(
                fun({_Arity, Doc}) ->
                    case Doc of
                        #{doc := DocText, examples := Examples} ->
                            DocPart =
                                case DocText of
                                    <<>> -> [];
                                    _ -> [DocText]
                                end,
                            ExPart =
                                case Examples of
                                    <<>> -> [];
                                    _ -> [<<"\n">>, Examples]
                                end,
                            case DocPart ++ ExPart of
                                [] -> false;
                                Combined -> {true, iolist_to_binary(Combined)}
                            end;
                        {error, _} ->
                            false
                    end
                end,
                [{A, D} || {A, D} <- ArityDocs, D =/= {error, hidden}]
            ),
            FnDocText =
                case DocSections of
                    [] -> <<>>;
                    _ -> iolist_to_binary([<<"\n">>, lists:join(<<"\n\n">>, DocSections)])
                end,
            FnHeader = iolist_to_binary([ModName, <<":">>, FnName, <<"\n">>]),
            FullText = iolist_to_binary([FnHeader, SigSection, FnDocText, <<"\n">>]),
            {ok, FullText}
    end.

%%% ============================================================================
%%% Type lookup
%%% ============================================================================

-doc """
Fall through to type definition lookup when no exported function matches.
""".
-spec format_type_or_not_found(
    module(),
    atom(),
    file:filename() | preloaded | cover_compiled
) -> {ok, binary()} | {error, not_found}.
format_type_or_not_found(Module, Name, BeamPath) ->
    NameBin = atom_to_binary(Name),
    TypeResult = find_types_in_beam(BeamPath, fun(N) -> N =:= Name end),
    render_type_result_or_not_found(Module, NameBin, TypeResult).

-doc """
Type lookup by binary name — used when the atom doesn't exist yet.
""".
-spec format_type_by_name_or_not_found(module(), binary()) ->
    {ok, binary()} | {error, not_found}.
format_type_by_name_or_not_found(Module, NameBin) ->
    BeamPath = code:which(Module),
    TypeResult = find_types_in_beam(BeamPath, fun(N) -> atom_to_binary(N) =:= NameBin end),
    render_type_result_or_not_found(Module, NameBin, TypeResult).

-doc "Search for type definitions in a beam file matching a predicate.".
-spec find_types_in_beam(
    file:filename() | preloaded | cover_compiled | non_existing,
    fun((atom()) -> boolean())
) -> {ok, [{atom(), non_neg_integer(), tuple()}]} | {error, term()}.
find_types_in_beam(preloaded, _Pred) ->
    {error, no_debug_info};
find_types_in_beam(cover_compiled, _Pred) ->
    {error, no_debug_info};
find_types_in_beam(non_existing, _Pred) ->
    {error, not_found};
find_types_in_beam(Path, Pred) ->
    case beamtalk_spec_reader:read_types(Path) of
        {ok, AllTypes} ->
            Matching = [{N, A, Form} || {N, A, Form} <- AllTypes, Pred(N)],
            case Matching of
                [] -> {error, not_found};
                _ -> {ok, Matching}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-doc "Render matched type definitions, or return not_found.".
-spec render_type_result_or_not_found(
    module(),
    binary(),
    {ok, [{atom(), non_neg_integer(), tuple()}]} | {error, term()}
) -> {ok, binary()} | {error, not_found}.
render_type_result_or_not_found(Module, NameBin, {ok, Types}) ->
    ModName = atom_to_binary(Module),
    TypeLines = lists:map(
        fun({_N, _A, Form}) ->
            iolist_to_binary([
                <<"  ">>,
                beamtalk_spec_reader:format_erlang_type(Form)
            ])
        end,
        Types
    ),
    Header = iolist_to_binary([ModName, <<":">>, NameBin, <<"\n">>]),
    Body = iolist_to_binary(lists:join(<<"\n">>, TypeLines)),
    FullText = iolist_to_binary([Header, Body, <<"\n">>]),
    {ok, FullText};
render_type_result_or_not_found(_Module, _NameBin, {error, _}) ->
    {error, not_found}.

%%% ============================================================================
%%% Formatting helpers
%%% ============================================================================

-doc "Format a bare exports list for modules without specs.".
-spec format_exports_list(module()) -> binary().
format_exports_list(Module) ->
    try Module:module_info(exports) of
        Exports ->
            %% Filter out module_info/0,1
            Filtered = [
                {F, A}
             || {F, A} <- Exports,
                F =/= module_info
            ],
            Sorted = lists:sort(Filtered),
            Lines = [
                iolist_to_binary([
                    <<"  ">>,
                    atom_to_binary(F),
                    <<"/">>,
                    integer_to_binary(A)
                ])
             || {F, A} <- Sorted
            ],
            iolist_to_binary([<<"Functions:\n">>, lists:join(<<"\n">>, Lines), <<"\n">>])
    catch
        _:_ -> <<"(no export information available)\n">>
    end.

-doc """
Try to show EEP-48 signatures for exported functions; fall back to
bare exports list when no EEP-48 docs are available.
""".
-spec format_eep48_signatures_or_exports(module()) -> binary().
format_eep48_signatures_or_exports(Module) ->
    try Module:module_info(exports) of
        Exports ->
            Filtered = [{F, A} || {F, A} <- Exports, F =/= module_info],
            %% Single lookup per function: filter hidden and get signature in one pass
            Lookups = [
                {{F, A}, beamtalk_native_docs:lookup(Module, F, A)}
             || {F, A} <- Filtered
            ],
            Visible = [
                {{F, A}, Doc}
             || {{F, A}, Doc} <- Lookups,
                Doc =/= {error, hidden}
            ],
            Sorted = lists:sort(Visible),
            Lines = lists:map(
                fun({{F, A}, Doc}) ->
                    case Doc of
                        #{sig := Sig} when Sig =/= <<>> ->
                            iolist_to_binary([<<"  ">>, Sig]);
                        _ ->
                            iolist_to_binary([
                                <<"  ">>,
                                atom_to_binary(F),
                                <<"/">>,
                                integer_to_binary(A)
                            ])
                    end
                end,
                Sorted
            ),
            iolist_to_binary([<<"Functions:\n">>, lists:join(<<"\n">>, Lines), <<"\n">>])
    catch
        _:_ -> <<"(no export information available)\n">>
    end.

-doc """
Format specs into a readable list with Beamtalk-style signatures.
Filters out functions marked as hidden (@private) in EEP-48 docs.
""".
-spec format_specs_list(module(), [map()]) -> binary().
format_specs_list(Module, Specs) ->
    %% Deduplicate: when both foo/N and 'foo:'/N exist, keep only 'foo:'/N
    Deduped = dedupe_keyword_aliases(Specs),
    %% Filter out hidden (@private) functions
    Visible = lists:filter(
        fun(Spec) ->
            Name = maps:get(name, Spec),
            Arity = maps:get(arity, Spec),
            case
                if
                    is_atom(Name) ->
                        {ok, Name};
                    is_binary(Name) ->
                        try
                            {ok, binary_to_existing_atom(Name, utf8)}
                        catch
                            error:badarg -> skip
                        end;
                    true ->
                        skip
                end
            of
                {ok, FnAtom} ->
                    not beamtalk_native_docs:is_hidden(Module, FnAtom, Arity);
                skip ->
                    %% Can't resolve to atom — include rather than hiding
                    true
            end
        end,
        Deduped
    ),
    Sorted = lists:sort(
        fun(A, B) ->
            {maps:get(name, A), maps:get(arity, A)} =<
                {maps:get(name, B), maps:get(arity, B)}
        end,
        Visible
    ),
    Lines = lists:map(
        fun(Spec) ->
            Name = maps:get(name, Spec),
            Params = maps:get(params, Spec, []),
            RetType = maps:get(return_type, Spec, <<"Dynamic">>),
            NameBin =
                if
                    is_atom(Name) -> atom_to_binary(Name);
                    is_binary(Name) -> Name;
                    true -> iolist_to_binary(io_lib:format("~p", [Name]))
                end,
            Sig = format_beamtalk_signature(NameBin, Params, RetType),
            iolist_to_binary([<<"  ">>, Sig])
        end,
        Sorted
    ),
    iolist_to_binary([<<"Functions:\n">>, lists:join(<<"\n">>, Lines), <<"\n">>]).

%%% ============================================================================
%%% Keyword alias deduplication
%%% ============================================================================

-doc """
Remove dispatch-alias specs that duplicate keyword messages.

When a Beamtalk native module exports both `parse/1` (FFI alias) and
`'parse:'/1` (keyword message), both appear in specs.  The plain variant
is just a compiler-generated dispatch alias, so we hide it when the
colon-suffixed variant exists at the same arity.  This also handles
multi-keyword selectors: `run/2` is the alias for `'run:timeout:'/2`.
""".
-spec dedupe_keyword_aliases([map()]) -> [map()].
dedupe_keyword_aliases(Specs) ->
    %% Build a set of {BaseName, Arity} for all colon-suffixed names
    ColonSet = sets:from_list([
        {base_keyword_name(maps:get(name, S)), maps:get(arity, S)}
     || S <- Specs,
        is_keyword_name(maps:get(name, S))
    ]),
    %% Keep a spec unless its plain name is shadowed by a colon-suffixed variant
    [
        S
     || S <- Specs,
        not is_keyword_alias(maps:get(name, S), maps:get(arity, S), ColonSet)
    ].

-doc """
True when the spec name does NOT end in ':' but a colon-suffixed
variant at the same arity exists in the set.
""".
-spec is_keyword_alias(binary(), non_neg_integer(), sets:set()) -> boolean().
is_keyword_alias(Name, Arity, ColonSet) ->
    case is_keyword_name(Name) of
        true -> false;
        false -> sets:is_element({Name, Arity}, ColonSet)
    end.

-doc "True when a binary name ends with ':'.".
-spec is_keyword_name(binary()) -> boolean().
is_keyword_name(<<>>) -> false;
is_keyword_name(Name) -> binary:last(Name) =:= $:.

-doc """
Extract the base name from a keyword selector — the segment before
the first colon.
""".
-spec base_keyword_name(binary()) -> binary().
base_keyword_name(Name) ->
    case binary:match(Name, <<":">>) of
        {Pos, _} -> binary:part(Name, 0, Pos);
        nomatch -> Name
    end.

%%% ============================================================================
%%% Signature formatting
%%% ============================================================================

-doc """
Format a single function as a Beamtalk-style type signature.

Examples:
  reverse: list :: List -> List
  seq: from :: Integer to: to :: Integer -> List
  node -> Symbol  (nullary)
""".
-spec format_beamtalk_signature(binary(), [map()], binary()) -> binary().
format_beamtalk_signature(Name, [], RetType) ->
    iolist_to_binary([Name, <<" -> ">>, RetType]);
format_beamtalk_signature(Name, Params, RetType) ->
    ParamParts = format_params(Name, Params, 0),
    iolist_to_binary([lists:join(<<" ">>, ParamParts), <<" -> ">>, RetType]).

-doc "Format parameter list with keywords.".
-spec format_params(binary(), [map()], non_neg_integer()) -> [iodata()].
format_params(_Name, [], _Idx) ->
    [];
format_params(Name, [Param | Rest], 0) ->
    ParamName = maps:get(name, Param, <<"arg">>),
    ParamType = maps:get(type, Param, <<"Dynamic">>),
    Keyword =
        case binary:last(Name) of
            $: -> Name;
            _ -> iolist_to_binary([Name, <<":">>])
        end,
    Part = iolist_to_binary([Keyword, <<" ">>, format_param_name(ParamName), <<" :: ">>, ParamType]),
    [Part | format_params(Name, Rest, 1)];
format_params(Name, [Param | Rest], Idx) ->
    ParamName = maps:get(name, Param, <<"arg">>),
    ParamType = maps:get(type, Param, <<"Dynamic">>),
    Keyword = iolist_to_binary([format_param_name(ParamName), <<":">>]),
    Part = iolist_to_binary([Keyword, <<" ">>, format_param_name(ParamName), <<" :: ">>, ParamType]),
    [Part | format_params(Name, Rest, Idx + 1)].

-doc "Format a parameter name, using a fallback for empty/missing names.".
-spec format_param_name(binary()) -> binary().
format_param_name(<<>>) -> <<"arg">>;
format_param_name(Name) -> Name.

-doc "Find all arities for a function exported by a module.".
-spec find_function_arities(module(), atom()) -> [non_neg_integer()].
find_function_arities(Module, Function) ->
    try Module:module_info(exports) of
        Exports ->
            Arities = [A || {F, A} <- Exports, F =:= Function],
            lists:sort(Arities)
    catch
        _:_ -> []
    end.
