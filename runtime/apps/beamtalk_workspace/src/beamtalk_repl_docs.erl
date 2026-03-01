%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc REPL documentation helper — fetches and formats runtime docs.
%%%
%%% **DDD Context:** REPL — Documentation
%%%
%%% Provides documentation lookup for `:help ClassName` and
%%% `:help ClassName selector` commands. Uses the runtime-embedded
%%% documentation system (ADR 0033) via message sends on live objects.
%%% Class docs are retrieved via `gen_server:call(ClassPid, get_doc)`.
%%% Method docs are retrieved via `>> #selector` (CompiledMethod maps).

-module(beamtalk_repl_docs).

-export([
    format_class_docs/1,
    format_method_doc/2
]).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% Exported for testing (only in test builds)
-ifdef(TEST).
-export([
    safe_to_existing_atom/1,
    get_method_doc_from_class/2,
    group_by_class/1,
    format_class_output/6,
    format_superclass/1,
    format_modifiers/1,
    format_method_output/4,
    format_metaclass_docs/0,
    format_metaclass_method_doc/1,
    metaclass_method_doc/1
]).
-endif.

%% @doc Format documentation for a class, including method listing.
%% Returns `{ok, FormattedBinary}` or `{error, Reason}`.
-spec format_class_docs(atom()) -> {ok, binary()} | {error, term()}.
format_class_docs('Metaclass') ->
    {ok, format_metaclass_docs()};
format_class_docs(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            {error, {class_not_found, ClassName}};
        ClassPid ->
            try
                Superclass = gen_server:call(ClassPid, superclass, 5000),
                IsSealed = gen_server:call(ClassPid, is_sealed, 5000),
                IsAbstract = gen_server:call(ClassPid, is_abstract, 5000),

                %% Get class doc from runtime (ADR 0033)
                ModuleDoc =
                    case gen_server:call(ClassPid, get_doc, 5000) of
                        none -> none;
                        Doc when is_binary(Doc) -> Doc
                    end,

                %% Walk class hierarchy to collect #{Sel => {DefiningClass, MethodInfo}}
                Flattened = collect_flattened_methods(ClassName, ClassPid),

                %% Build method listing grouped by defining class
                {Own, Inherited} = maps:fold(
                    fun(Selector, {DefiningClass, MethodInfo}, {OwnAcc, InhAcc}) ->
                        case DefiningClass of
                            ClassName ->
                                MethodSealed = maps:get(is_sealed, MethodInfo, false),
                                {[{Selector, MethodSealed} | OwnAcc], InhAcc};
                            _ ->
                                {OwnAcc, [{Selector, DefiningClass} | InhAcc]}
                        end
                    end,
                    {[], []},
                    Flattened
                ),

                %% Get method signatures and docs from runtime for own methods
                OwnSelectors = lists:sort([S || {S, _} <- Own]),
                SealedMap = maps:from_list(Own),
                OwnDocs = get_method_signatures_with_sealed(
                    ClassPid, OwnSelectors, SealedMap
                ),

                %% Group inherited by defining class
                InheritedGrouped = group_by_class(lists:sort(Inherited)),

                %% Format the output
                Modifiers = #{is_sealed => IsSealed, is_abstract => IsAbstract},
                Output = format_class_output(
                    ClassName,
                    Superclass,
                    Modifiers,
                    ModuleDoc,
                    OwnDocs,
                    InheritedGrouped
                ),
                {ok, Output}
            catch
                exit:{timeout, _} ->
                    {error, {class_not_found, ClassName}};
                exit:{noproc, _} ->
                    {error, {class_not_found, ClassName}}
            end
    end.

%% @doc Format documentation for a specific method on a class.
%% Walks class hierarchy to find the method.
%% Returns `{ok, FormattedBinary}` or `{error, Reason}`.
-spec format_method_doc(atom(), binary()) -> {ok, binary()} | {error, term()}.
format_method_doc('Metaclass', SelectorBin) ->
    format_metaclass_method_doc(SelectorBin);
format_method_doc(ClassName, SelectorBin) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            {error, {class_not_found, ClassName}};
        ClassPid ->
            case safe_to_existing_atom(SelectorBin) of
                {error, badarg} ->
                    {error, {method_not_found, ClassName, SelectorBin}};
                {ok, SelectorAtom} ->
                    try
                        %% Use >> to walk hierarchy and get CompiledMethod
                        case beamtalk_method_resolver:resolve(ClassPid, SelectorAtom) of
                            nil ->
                                %% BT-990: Try class-side method before giving up
                                case
                                    gen_server:call(
                                        ClassPid,
                                        {class_method, SelectorAtom},
                                        5000
                                    )
                                of
                                    nil ->
                                        {error, {method_not_found, ClassName, SelectorBin}};
                                    ClassMethodObj when is_map(ClassMethodObj) ->
                                        DocInfo = method_doc_info(
                                            ClassMethodObj, SelectorAtom
                                        ),
                                        Output = format_method_output(
                                            ClassName,
                                            SelectorBin,
                                            ClassName,
                                            DocInfo
                                        ),
                                        {ok, Output}
                                end;
                            MethodObj when is_map(MethodObj) ->
                                %% Find which class defines this method
                                DefiningClass = find_defining_class(
                                    ClassPid, SelectorAtom
                                ),
                                %% Extract doc, signature, and sealed from CompiledMethod
                                DocInfo = method_doc_info(MethodObj, SelectorAtom),
                                Output = format_method_output(
                                    ClassName,
                                    SelectorBin,
                                    DefiningClass,
                                    DocInfo
                                ),
                                {ok, Output}
                        end
                    catch
                        exit:{timeout, _} ->
                            {error, {class_not_found, ClassName}};
                        exit:{noproc, _} ->
                            {error, {class_not_found, ClassName}}
                    end
            end
    end.

%%% Internal functions

%% @doc Return hardcoded documentation for the Metaclass terminal sentinel.
-spec format_metaclass_docs() -> binary().
format_metaclass_docs() ->
    iolist_to_binary([
        <<"== Metaclass ==\n">>,
        <<"The terminal sentinel of the class hierarchy.\n\n">>,
        <<"Every class object is an instance of its metaclass, and every metaclass\n">>,
        <<"is an instance of Metaclass. Metaclass is its own class — the tower\n">>,
        <<"terminates here.\n\n">>,
        <<"Common class-side methods:\n">>,
        <<"  new              Create a new instance\n">>,
        <<"  spawn            Create a new actor instance\n">>,
        <<"  spawnWith:       Create a new actor with initial state\n">>,
        <<"\nUse :help ClassName for docs on a specific class.">>
    ]).

%% @doc Return documentation for a Metaclass method.
-spec format_metaclass_method_doc(binary()) -> {ok, binary()} | {error, term()}.
format_metaclass_method_doc(SelectorBin) ->
    case metaclass_method_doc(SelectorBin) of
        {ok, Doc} ->
            {ok,
                iolist_to_binary([
                    <<"== Metaclass >> ">>,
                    SelectorBin,
                    <<" ==">>,
                    <<"\n  ">>,
                    SelectorBin,
                    <<"\n\n">>,
                    Doc
                ])};
        not_found ->
            {error, {method_not_found, 'Metaclass', SelectorBin}}
    end.

%% @doc Lookup doc text for known Metaclass class-side methods.
-spec metaclass_method_doc(binary()) -> {ok, binary()} | not_found.
metaclass_method_doc(<<"new">>) ->
    {ok, <<"Create a new instance of the class.">>};
metaclass_method_doc(<<"spawn">>) ->
    {ok, <<"Create a new actor instance. Returns an actor reference.">>};
metaclass_method_doc(<<"spawnWith:">>) ->
    {ok, <<"Create a new actor with initial state from a Dictionary.">>};
metaclass_method_doc(_) ->
    not_found.

%% @doc Safe atom conversion — returns error instead of creating new atoms.
-spec safe_to_existing_atom(binary()) -> {ok, atom()} | {error, badarg}.
safe_to_existing_atom(<<>>) ->
    {error, badarg};
safe_to_existing_atom(Bin) when is_binary(Bin) ->
    try binary_to_existing_atom(Bin, utf8) of
        Atom -> {ok, Atom}
    catch
        error:badarg -> {error, badarg}
    end.

%% @doc Get method doc info from a CompiledMethod map.
%% Returns {Signature, DocText | none, IsSealed}.
-spec method_doc_info(map(), atom()) -> {binary(), binary() | none, boolean()}.
method_doc_info(MethodObj, SelectorAtom) ->
    %% Extract doc from __doc__ field
    Doc =
        case maps:get('__doc__', MethodObj, nil) of
            nil -> none;
            DocBin when is_binary(DocBin) -> DocBin
        end,
    %% BT-988: Use __signature__ if present, fall back to selector atom
    Signature =
        case maps:get('__signature__', MethodObj, nil) of
            nil -> atom_to_binary(SelectorAtom, utf8);
            SigBin when is_binary(SigBin) -> SigBin
        end,
    %% Extract is_sealed from __method_info__
    IsSealed =
        case maps:get('__method_info__', MethodObj, #{}) of
            MethodInfo when is_map(MethodInfo) ->
                maps:get(is_sealed, MethodInfo, false);
            _ ->
                false
        end,
    {Signature, Doc, IsSealed}.

%% @doc Get method signatures and docs from runtime for a list of selectors.
-spec get_method_signatures_with_sealed(pid(), [atom()], map()) ->
    [{atom(), binary(), binary() | none, boolean()}].
get_method_signatures_with_sealed(ClassPid, Selectors, SealedMap) ->
    lists:map(
        fun(Selector) ->
            {Sig, Doc} = get_method_doc_from_class(ClassPid, Selector),
            IsSealed = maps:get(Selector, SealedMap, false),
            {Selector, Sig, Doc, IsSealed}
        end,
        Selectors
    ).

%% @doc Get method signature and doc text from a class's runtime state.
-spec get_method_doc_from_class(pid(), atom()) -> {binary(), binary() | none}.
get_method_doc_from_class(ClassPid, Selector) ->
    case gen_server:call(ClassPid, {method, Selector}, 5000) of
        nil ->
            {atom_to_binary(Selector, utf8), none};
        MethodObj when is_map(MethodObj) ->
            {Sig, Doc, _IsSealed} = method_doc_info(MethodObj, Selector),
            {Sig, Doc}
    end.

%% @doc Find which class in the hierarchy defines a selector.
%% Returns the class name atom.
-spec find_defining_class(pid(), atom()) -> atom().
find_defining_class(ClassPid, Selector) ->
    find_defining_class(ClassPid, Selector, 0).

-spec find_defining_class(pid(), atom(), non_neg_integer()) -> atom().
find_defining_class(ClassPid, Selector, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    ClassName = gen_server:call(ClassPid, class_name, 5000),
    ?LOG_WARNING(
        "find_defining_class: max hierarchy depth ~p exceeded at ~p for selector ~p — possible cycle",
        [?MAX_HIERARCHY_DEPTH, ClassName, Selector]
    ),
    ClassName;
find_defining_class(ClassPid, Selector, Depth) ->
    ClassName = gen_server:call(ClassPid, class_name, 5000),
    case gen_server:call(ClassPid, {method, Selector}, 5000) of
        nil ->
            %% Not in local instance_methods — check superclass
            case gen_server:call(ClassPid, superclass, 5000) of
                none ->
                    ClassName;
                Super ->
                    case beamtalk_class_registry:whereis_class(Super) of
                        undefined -> ClassName;
                        SuperPid -> find_defining_class(SuperPid, Selector, Depth + 1)
                    end
            end;
        _MethodInfo ->
            ClassName
    end.

%% @doc Group inherited methods by defining class.
-spec group_by_class([{atom(), atom()}]) -> [{atom(), [atom()]}].
group_by_class(Methods) ->
    Grouped = lists:foldl(
        fun({Selector, DefClass}, Acc) ->
            Existing = maps:get(DefClass, Acc, []),
            maps:put(DefClass, [Selector | Existing], Acc)
        end,
        #{},
        Methods
    ),
    %% Convert to sorted list of {ClassName, SortedSelectors}
    lists:sort(
        maps:fold(
            fun(Class, Selectors, Acc) ->
                [{Class, lists:sort(Selectors)} | Acc]
            end,
            [],
            Grouped
        )
    ).

%% @doc Format the complete class documentation output.
-spec format_class_output(
    atom(),
    atom() | none,
    map(),
    binary() | none,
    [{atom(), binary(), binary() | none, boolean()}],
    [{atom(), [atom()]}]
) -> binary().
format_class_output(ClassName, Superclass, Modifiers, ModuleDoc, OwnDocs, InheritedGrouped) ->
    Parts = [
        %% Header
        iolist_to_binary([
            <<"== ">>,
            atom_to_binary(ClassName, utf8),
            format_superclass(Superclass),
            <<" ==">>
        ]),

        %% Class modifiers
        format_modifiers(Modifiers),

        %% Module doc
        case ModuleDoc of
            none -> <<>>;
            Text -> iolist_to_binary([<<"\n">>, Text])
        end,

        %% Own methods
        case OwnDocs of
            [] ->
                <<>>;
            _ ->
                MethodLines = lists:map(
                    fun
                        ({_Sel, Sig, _Doc, true}) ->
                            iolist_to_binary([<<"  ">>, Sig, <<" [sealed]">>]);
                        ({_Sel, Sig, _Doc, false}) ->
                            iolist_to_binary([<<"  ">>, Sig])
                    end,
                    OwnDocs
                ),
                iolist_to_binary([
                    <<"\nInstance methods:\n">>,
                    lists:join(<<"\n">>, MethodLines)
                ])
        end,

        %% Inherited methods (compact summary per class)
        lists:map(
            fun({FromClass, Selectors}) ->
                Count = length(Selectors),
                Summary =
                    case Count =< 5 of
                        true ->
                            %% Show all if 5 or fewer
                            SelectorStrs = lists:map(
                                fun(S) -> atom_to_binary(S, utf8) end,
                                Selectors
                            ),
                            lists:join(<<", ">>, SelectorStrs);
                        false ->
                            %% Show first 3 + count of remaining
                            {First3, _Rest} = lists:split(3, Selectors),
                            Shown = lists:map(
                                fun(S) -> atom_to_binary(S, utf8) end,
                                First3
                            ),
                            Remaining = Count - 3,
                            iolist_to_binary([
                                lists:join(<<", ">>, Shown),
                                <<", ... (">>,
                                integer_to_binary(Remaining),
                                <<" more)">>
                            ])
                    end,
                iolist_to_binary([
                    <<"\nInherited from ">>,
                    atom_to_binary(FromClass, utf8),
                    <<" (">>,
                    integer_to_binary(Count),
                    <<" methods): ">>,
                    Summary
                ])
            end,
            InheritedGrouped
        ),

        %% Hint
        <<"\nUse :help ClassName selector for method details.">>
    ],
    iolist_to_binary(
        lists:filter(
            fun
                (<<>>) -> false;
                (_) -> true
            end,
            lists:flatten(Parts)
        )
    ).

%% @doc Format superclass portion of header.
-spec format_superclass(atom() | none) -> iolist().
format_superclass(none) -> [];
format_superclass(Superclass) -> [<<" < ">>, atom_to_binary(Superclass, utf8)].

%% @doc Format class modifiers (sealed/abstract) for display.
-spec format_modifiers(map()) -> binary().
format_modifiers(#{is_sealed := true, is_abstract := true}) ->
    <<"\n[sealed] [abstract]">>;
format_modifiers(#{is_sealed := true}) ->
    <<"\n[sealed]">>;
format_modifiers(#{is_abstract := true}) ->
    <<"\n[abstract]">>;
format_modifiers(_) ->
    <<>>.

%% @private
%% @doc Walk the class hierarchy and build a flattened method map.
%%
%% ADR 0032 Phase 1: Replaces get_flattened_methods gen_server call.
%% Returns #{Selector => {DefiningClass, MethodInfo}} where local methods
%% shadow inherited ones, walking from ClassName upward.
-spec collect_flattened_methods(atom(), pid()) -> map().
collect_flattened_methods(ClassName, ClassPid) ->
    collect_flattened_methods(ClassName, ClassPid, 0).

-spec collect_flattened_methods(atom(), pid(), non_neg_integer()) -> map().
collect_flattened_methods(ClassName, _ClassPid, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    ?LOG_WARNING(
        "collect_flattened_methods: max hierarchy depth ~p exceeded at ~p — possible cycle",
        [?MAX_HIERARCHY_DEPTH, ClassName]
    ),
    #{};
collect_flattened_methods(ClassName, ClassPid, Depth) ->
    {ok, LocalMethods} = gen_server:call(ClassPid, get_instance_methods, 5000),
    LocalFlat = maps:map(fun(_Sel, Info) -> {ClassName, Info} end, LocalMethods),
    Superclass = gen_server:call(ClassPid, superclass, 5000),
    SuperFlat = collect_chain_methods(Superclass, Depth + 1),
    maps:merge(SuperFlat, LocalFlat).

-spec collect_chain_methods(atom() | none, non_neg_integer()) -> map().
collect_chain_methods(none, _Depth) ->
    #{};
collect_chain_methods(SuperName, Depth) ->
    case beamtalk_class_registry:whereis_class(SuperName) of
        undefined -> #{};
        SuperPid -> collect_flattened_methods(SuperName, SuperPid, Depth)
    end.

%% @doc Format method-specific documentation output.
-spec format_method_output(
    atom(),
    binary(),
    atom(),
    {binary(), binary() | none, boolean()}
) -> binary().
format_method_output(ClassName, SelectorBin, DefiningClass, {Signature, DocText, IsSealed}) ->
    Header = iolist_to_binary([
        <<"== ">>,
        atom_to_binary(ClassName, utf8),
        <<" >> ">>,
        SelectorBin,
        <<" ==">>
    ]),
    SealedLine =
        case IsSealed of
            true -> <<"\n[sealed]">>;
            false -> <<>>
        end,
    Inherited =
        case DefiningClass of
            ClassName ->
                <<>>;
            _ ->
                iolist_to_binary([
                    <<"\n(inherited from ">>,
                    atom_to_binary(DefiningClass, utf8),
                    <<")">>
                ])
        end,
    SignatureLine = iolist_to_binary([<<"\n  ">>, Signature]),
    Doc =
        case DocText of
            none -> <<>>;
            Text -> iolist_to_binary([<<"\n\n">>, Text])
        end,
    iolist_to_binary([Header, SealedLine, Inherited, SignatureLine, Doc]).
