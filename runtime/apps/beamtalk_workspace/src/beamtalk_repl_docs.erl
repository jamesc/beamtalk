%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc REPL documentation helper — fetches and formats runtime docs.
%%%
%%% **DDD Context:** REPL Session Context
%%%
%%% Provides documentation lookup for `:help ClassName` and
%%% `:help ClassName selector` commands. Uses the runtime-embedded
%%% documentation system (ADR 0033) via message sends on live objects.
%%% Class docs are retrieved via `gen_server:call(ClassPid, get_doc)`.
%%% Method docs are retrieved via `>> #selector` (CompiledMethod maps).

-module(beamtalk_repl_docs).

-export([
    format_class_docs/1,
    format_class_docs_class_side/1,
    format_method_doc/2,
    format_method_doc_class_side/2
]).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% Exported for testing (only in test builds)
-ifdef(TEST).
-export([
    safe_to_existing_atom/1,
    get_method_doc_from_class/2,
    group_by_class/1,
    format_class_output/7,
    format_superclass/1,
    format_modifiers/1,
    format_method_output/4,
    format_method_line/1,
    format_metaclass_docs/0,
    format_metaclass_method_doc/1,
    metaclass_method_doc/1,
    format_selector_summary/2,
    format_class_side_output/4,
    extract_see_also/1,
    alternative_classes/1,
    format_see_also/1,
    build_see_also/3,
    format_package_provenance/1
]).
-endif.

%% @doc Format documentation for a class, including method listing.
%% Returns `{ok, FormattedBinary}` or `{error, Reason}`.
-spec format_class_docs(atom()) -> {ok, binary()} | {error, term()}.
format_class_docs('Metaclass') ->
    {ok, format_metaclass_docs()};
format_class_docs(ClassName) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined ->
            {error, {class_not_found, ClassName}};
        ClassPid ->
            try
                Superclass = beamtalk_runtime_api:superclass(ClassPid),
                IsSealed = beamtalk_runtime_api:is_sealed(ClassPid),
                IsAbstract = beamtalk_runtime_api:is_abstract(ClassPid),
                %% ADR 0071 Phase 5: Read visibility
                IsInternal =
                    try
                        beamtalk_runtime_api:is_internal(ClassPid)
                    catch
                        _:_ -> false
                    end,

                %% Get class doc from runtime (ADR 0033)
                ModuleDoc =
                    case gen_server:call(ClassPid, get_doc, 5000) of
                        none -> none;
                        Doc when is_binary(Doc) -> Doc
                    end,

                %% Walk class hierarchy to collect #{Sel => {DefiningClass, MethodInfo}}
                Flattened = collect_flattened_methods(ClassName, ClassPid),

                %% Build method listing grouped by defining class.
                %% ADR 0071 Phase 5: Also extract method visibility for annotation.
                {Own, Inherited} = maps:fold(
                    fun(Selector, {DefiningClass, MethodInfo}, {OwnAcc, InhAcc}) ->
                        case DefiningClass of
                            ClassName ->
                                MethodSealed = maps:get(is_sealed, MethodInfo, false),
                                MethodInternal =
                                    maps:get(visibility, MethodInfo, public) =:= internal,
                                {[{Selector, MethodSealed, MethodInternal} | OwnAcc], InhAcc};
                            _ ->
                                {OwnAcc, [{Selector, DefiningClass} | InhAcc]}
                        end
                    end,
                    {[], []},
                    Flattened
                ),

                %% Get method signatures and docs from runtime for own methods
                OwnSelectors = lists:sort([S || {S, _, _} <- Own]),
                SealedMap = maps:from_list([{S, Sealed} || {S, Sealed, _} <- Own]),
                InternalMap = maps:from_list([{S, Int} || {S, _, Int} <- Own]),
                OwnDocs = get_method_signatures_with_modifiers(
                    ClassPid, OwnSelectors, SealedMap, InternalMap
                ),

                %% Group inherited by defining class
                InheritedGrouped = group_by_class(lists:sort(Inherited)),

                %% Build see-also cross-references
                SeeAlso = build_see_also(ClassName, Superclass, ModuleDoc),

                %% Format the output
                Modifiers = #{
                    is_sealed => IsSealed, is_abstract => IsAbstract, is_internal => IsInternal
                },
                Output = format_class_output(
                    ClassName,
                    Superclass,
                    Modifiers,
                    ModuleDoc,
                    OwnDocs,
                    InheritedGrouped,
                    SeeAlso
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
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined ->
            {error, {class_not_found, ClassName}};
        ClassPid ->
            case safe_to_existing_atom(SelectorBin) of
                {error, badarg} ->
                    {error, {method_not_found, ClassName, SelectorBin}};
                {ok, SelectorAtom} ->
                    try
                        case resolve_method_obj(ClassPid, SelectorAtom) of
                            {ok, MethodObj, DefClass} ->
                                DocInfo = method_doc_info(MethodObj, SelectorAtom),
                                {ok,
                                    format_method_output(ClassName, SelectorBin, DefClass, DocInfo)};
                            not_found ->
                                {error, {method_not_found, ClassName, SelectorBin}}
                        end
                    catch
                        exit:{timeout, _} ->
                            {error, {class_not_found, ClassName}};
                        exit:{noproc, _} ->
                            {error, {class_not_found, ClassName}}
                    end
            end
    end.

%% @doc Format documentation for a specific method on a class object (class-side).
%% Uses the class-object lookup path (class methods + class-protocol fallthrough).
%% Returns `{ok, FormattedBinary}` or `{error, Reason}`.
-spec format_method_doc_class_side(atom(), binary()) -> {ok, binary()} | {error, term()}.
format_method_doc_class_side('Metaclass', SelectorBin) ->
    format_metaclass_method_doc(SelectorBin);
format_method_doc_class_side(ClassName, SelectorBin) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined ->
            {error, {class_not_found, ClassName}};
        ClassPid ->
            case safe_to_existing_atom(SelectorBin) of
                {error, badarg} ->
                    {error, {method_not_found, ClassName, SelectorBin}};
                {ok, SelectorAtom} ->
                    try
                        case resolve_class_side_method_obj(ClassPid, SelectorAtom) of
                            {ok, MethodObj, DefClass} ->
                                DocInfo = method_doc_info(MethodObj, SelectorAtom),
                                {ok,
                                    format_method_output(
                                        ClassName, SelectorBin, DefClass, DocInfo
                                    )};
                            not_found ->
                                {error, {method_not_found, ClassName, SelectorBin}}
                        end
                    catch
                        exit:{timeout, _} ->
                            {error, {class_not_found, ClassName}};
                        exit:{noproc, _} ->
                            {error, {class_not_found, ClassName}}
                    end
            end
    end.

%% @doc Format documentation for the class-object side of a class (`:h ClassName class`).
%% Shows class methods and class-protocol methods separately.
%% Returns `{ok, FormattedBinary}` or `{error, Reason}`.
-spec format_class_docs_class_side(atom()) -> {ok, binary()} | {error, term()}.
format_class_docs_class_side('Metaclass') ->
    {ok, format_metaclass_docs()};
format_class_docs_class_side(ClassName) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined ->
            {error, {class_not_found, ClassName}};
        ClassPid ->
            try
                %% User-defined class methods walked up the superclass chain
                FlatCM = collect_flattened_class_methods(ClassName, ClassPid),
                {OwnCM, InhCM} = maps:fold(
                    fun(Sel, DefClass, {OwnAcc, InhAcc}) ->
                        case DefClass of
                            ClassName -> {[Sel | OwnAcc], InhAcc};
                            _ -> {OwnAcc, [{Sel, DefClass} | InhAcc]}
                        end
                    end,
                    {[], []},
                    FlatCM
                ),
                OwnCMSorted = lists:sort(OwnCM),
                InhCMGrouped = group_by_class(lists:sort(InhCM)),

                %% Class-protocol methods (Class→Behaviour→Object instance methods)
                ProtoGrouped =
                    case beamtalk_runtime_api:whereis_class('Class') of
                        undefined ->
                            [];
                        ClassClassPid ->
                            Proto = collect_flattened_methods('Class', ClassClassPid),
                            group_by_class(
                                lists:sort([{Sel, DC} || {Sel, {DC, _}} <- maps:to_list(Proto)])
                            )
                    end,

                {ok, format_class_side_output(ClassName, OwnCMSorted, InhCMGrouped, ProtoGrouped)}
            catch
                exit:{timeout, _} -> {error, {class_not_found, ClassName}};
                exit:{noproc, _} -> {error, {class_not_found, ClassName}}
            end
    end.

%%% Internal functions

%% @doc Try instance-side and class-side lookup strategies for a method:
%%   1. Instance-side resolution  (42 foo — instance method chain)
%%   2. Class-side lookup         (explicitly defined class methods, walked up hierarchy)
%%
%% Class-protocol fallthrough (Class→Behaviour→Object) is intentionally excluded:
%% methods like `allMethods`, `name`, `reload` are class-object methods and should
%% only appear under `:h ClassName class selector`, not `:h ClassName selector`.
-spec resolve_method_obj(pid(), atom()) -> {ok, map(), atom()} | not_found.
resolve_method_obj(ClassPid, Selector) ->
    case beamtalk_method_resolver:resolve(ClassPid, Selector) of
        MethodObj when is_map(MethodObj) ->
            {ok, MethodObj, find_defining_class(ClassPid, Selector)};
        nil ->
            case gen_server:call(ClassPid, {class_method, Selector}, 5000) of
                ClassMethodObj when is_map(ClassMethodObj) ->
                    {ok, ClassMethodObj, find_defining_class_method(ClassPid, Selector)};
                nil ->
                    not_found
            end
    end.

%% @doc Try class-side lookup strategies for a class object (Integer foo, not 42 foo):
%%   1. Class-side methods (explicitly declared class methods, walked up hierarchy)
%%   2. Class-protocol fallthrough (Class→Behaviour→Object instance methods)
-spec resolve_class_side_method_obj(pid(), atom()) -> {ok, map(), atom()} | not_found.
resolve_class_side_method_obj(ClassPid, Selector) ->
    case gen_server:call(ClassPid, {class_method, Selector}, 5000) of
        ClassMethodObj when is_map(ClassMethodObj) ->
            {ok, ClassMethodObj, find_defining_class_method(ClassPid, Selector)};
        nil ->
            case beamtalk_runtime_api:whereis_class('Class') of
                undefined ->
                    not_found;
                _ ->
                    case beamtalk_method_resolver:resolve('Class', Selector) of
                        ClassProtoObj when is_map(ClassProtoObj) ->
                            {ok, ClassProtoObj, find_class_protocol_defining_class(Selector)};
                        nil ->
                            not_found
                    end
            end
    end.

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
%% Returns {Signature, DocText | none, IsSealed, IsInternal}.
%% ADR 0071 Phase 5: Now includes method-level visibility.
-spec method_doc_info(map(), atom()) -> {binary(), binary() | none, boolean(), boolean()}.
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
    %% Extract is_sealed and visibility from __method_info__
    {IsSealed, IsInternal} =
        case maps:get('__method_info__', MethodObj, #{}) of
            MethodInfo when is_map(MethodInfo) ->
                {
                    maps:get(is_sealed, MethodInfo, false),
                    maps:get(visibility, MethodInfo, public) =:= internal
                };
            _ ->
                {false, false}
        end,
    {Signature, Doc, IsSealed, IsInternal}.

%% @doc Get method signatures and docs with both sealed and internal flags.
%% ADR 0071 Phase 5: Extended version that includes method visibility.
-spec get_method_signatures_with_modifiers(pid(), [atom()], map(), map()) ->
    [{atom(), binary(), binary() | none, boolean(), boolean()}].
get_method_signatures_with_modifiers(ClassPid, Selectors, SealedMap, InternalMap) ->
    lists:map(
        fun(Selector) ->
            {Sig, Doc} = get_method_doc_from_class(ClassPid, Selector),
            IsSealed = maps:get(Selector, SealedMap, false),
            IsInternal = maps:get(Selector, InternalMap, false),
            {Selector, Sig, Doc, IsSealed, IsInternal}
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
            {Sig, Doc, _IsSealed, _IsInternal} = method_doc_info(MethodObj, Selector),
            {Sig, Doc}
    end.

%% @doc Find which class in the hierarchy defines a selector.
%% Returns the class name atom.
-spec find_defining_class(pid(), atom()) -> atom().
find_defining_class(ClassPid, Selector) ->
    find_defining_class(ClassPid, Selector, 0).

-spec find_defining_class(pid(), atom(), non_neg_integer()) -> atom().
find_defining_class(ClassPid, Selector, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    ClassName = beamtalk_runtime_api:class_name(ClassPid),
    ?LOG_WARNING(
        "find_defining_class: max hierarchy depth ~p exceeded at ~p for selector ~p — possible cycle",
        [?MAX_HIERARCHY_DEPTH, ClassName, Selector],
        #{domain => [beamtalk, runtime]}
    ),
    ClassName;
find_defining_class(ClassPid, Selector, Depth) ->
    ClassName = beamtalk_runtime_api:class_name(ClassPid),
    case gen_server:call(ClassPid, {method, Selector}, 5000) of
        nil ->
            %% Not in local instance_methods — check superclass
            case beamtalk_runtime_api:superclass(ClassPid) of
                none ->
                    ClassName;
                Super ->
                    case beamtalk_runtime_api:whereis_class(Super) of
                        undefined -> ClassName;
                        SuperPid -> find_defining_class(SuperPid, Selector, Depth + 1)
                    end
            end;
        _MethodInfo ->
            ClassName
    end.

%% @doc Find which class in the hierarchy defines a class-side method.
%% Uses {class_method, Selector} which walks the chain internally;
%% we query each class locally via get_local_class_methods to find
%% exactly where the method is defined.
-spec find_defining_class_method(pid(), atom()) -> atom().
find_defining_class_method(ClassPid, Selector) ->
    find_defining_class_method(ClassPid, Selector, 0).

-spec find_defining_class_method(pid(), atom(), non_neg_integer()) -> atom().
find_defining_class_method(ClassPid, _Selector, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    beamtalk_runtime_api:class_name(ClassPid);
find_defining_class_method(ClassPid, Selector, Depth) ->
    ClassName = beamtalk_runtime_api:class_name(ClassPid),
    LocalClassMethods = gen_server:call(ClassPid, get_local_class_methods, 5000),
    case maps:is_key(Selector, LocalClassMethods) of
        true ->
            ClassName;
        false ->
            case beamtalk_runtime_api:superclass(ClassPid) of
                none ->
                    ClassName;
                Super ->
                    case beamtalk_runtime_api:whereis_class(Super) of
                        undefined -> ClassName;
                        SuperPid -> find_defining_class_method(SuperPid, Selector, Depth + 1)
                    end
            end
    end.

%% @doc Find which class in the Class chain defines a class-protocol method.
%%
%% Used by the class-side lookup path (`resolve_class_side_method_obj`).
%% Class-protocol methods (name, printString, allMethods, methods, canUnderstand:,
%% reload, etc.) are instance methods of the Class→Behaviour→Object chain that class
%% objects inherit via the class-chain fallthrough mechanism (try_class_chain_fallthrough
%% dispatches from 'Class'). This function walks from 'Class' upward to find the
%% exact defining class.
-spec find_class_protocol_defining_class(atom()) -> atom().
find_class_protocol_defining_class(Selector) ->
    case beamtalk_runtime_api:whereis_class('Class') of
        undefined -> 'Class';
        ClassPid -> find_defining_class(ClassPid, Selector, 0)
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
%% ADR 0071 Phase 5: OwnDocs may be 5-tuples with internal flag.
-spec format_class_output(
    atom(),
    atom() | none,
    map(),
    binary() | none,
    [{atom(), binary(), binary() | none, boolean(), boolean()}],
    [{atom(), [atom()]}],
    [{atom(), binary()}]
) -> binary().
format_class_output(
    ClassName, Superclass, Modifiers, ModuleDoc, OwnDocs, InheritedGrouped, SeeAlso
) ->
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

        %% Package provenance (ADR 0070 Phase 5, BT-1658)
        format_package_provenance(ClassName),

        %% Module doc
        case ModuleDoc of
            none -> <<>>;
            Text -> iolist_to_binary([<<"\n">>, Text])
        end,

        %% Own methods — ADR 0071 Phase 5: annotate [internal] and [sealed]
        case OwnDocs of
            [] ->
                <<>>;
            _ ->
                MethodLines = lists:map(
                    fun(MethodEntry) ->
                        format_method_line(MethodEntry)
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
                iolist_to_binary([
                    <<"\nInherited from ">>,
                    atom_to_binary(FromClass, utf8),
                    <<" (">>,
                    integer_to_binary(Count),
                    <<" methods): ">>,
                    format_selector_summary(Count, Selectors)
                ])
            end,
            InheritedGrouped
        ),

        %% See also cross-references
        format_see_also(SeeAlso),

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

%% @doc Format class modifiers (sealed/abstract/internal) for display.
%% ADR 0071 Phase 5: Include [internal] annotation when present.
-spec format_modifiers(map()) -> binary().
format_modifiers(Modifiers) ->
    Tags = lists:filter(fun(T) -> T =/= <<>> end, [
        case maps:get(is_internal, Modifiers, false) of
            true -> <<"[internal]">>;
            false -> <<>>
        end,
        case maps:get(is_sealed, Modifiers, false) of
            true -> <<"[sealed]">>;
            false -> <<>>
        end,
        case maps:get(is_abstract, Modifiers, false) of
            true -> <<"[abstract]">>;
            false -> <<>>
        end
    ]),
    case Tags of
        [] -> <<>>;
        _ -> iolist_to_binary([<<"\n">>, lists:join(<<" ">>, Tags)])
    end.

%% @doc Format a single method line in the class doc method listing.
%% ADR 0071 Phase 5: Handles 5-tuple with both sealed and internal flags.
-spec format_method_line({atom(), binary(), binary() | none, boolean(), boolean()}) -> binary().
format_method_line({_Sel, Sig, _Doc, IsSealed, IsInternal}) ->
    Tags = lists:filter(fun(T) -> T =/= <<>> end, [
        case IsInternal of
            true -> <<"[internal]">>;
            false -> <<>>
        end,
        case IsSealed of
            true -> <<"[sealed]">>;
            false -> <<>>
        end
    ]),
    case Tags of
        [] -> iolist_to_binary([<<"  ">>, Sig]);
        _ -> iolist_to_binary([<<"  ">>, Sig, <<" ">>, lists:join(<<" ">>, Tags)])
    end.

%% @doc Format package provenance for a class (ADR 0070 Phase 5, BT-1658).
%%
%% Shows which package a class belongs to using `beamtalk_package:package_name/1`.
%% Returns an empty binary if the class has no package or the lookup fails.
-spec format_package_provenance(atom()) -> binary().
format_package_provenance(ClassName) ->
    try beamtalk_package:package_name(ClassName) of
        nil ->
            <<>>;
        PkgName when is_binary(PkgName) ->
            iolist_to_binary([<<"\nPackage: ">>, PkgName])
    catch
        _:_ -> <<>>
    end.

%% @doc Extract @see references from doc text.
%%
%% Matches `@see ClassName` and `@see ClassName (description)` patterns.
%% Use one @see per line for multiple references.
%%
%% Returns a list of {ClassName, Description} tuples.
-spec extract_see_also(binary() | none) -> [{atom(), binary()}].
extract_see_also(none) ->
    [];
extract_see_also(DocText) ->
    case
        re:run(
            DocText,
            <<"@see\\s+([A-Z][A-Za-z0-9_]*)(?:\\s+\\(([^)]+)\\))?">>,
            [global, {capture, all_but_first, binary}]
        )
    of
        {match, Matches} ->
            lists:filtermap(
                fun(Match) ->
                    case safe_to_existing_atom(hd(Match)) of
                        {ok, Atom} ->
                            Desc =
                                case Match of
                                    [_, D] -> D;
                                    _ -> <<>>
                                end,
                            {true, {Atom, Desc}};
                        {error, badarg} ->
                            %% Unknown atom — skip to avoid atom table exhaustion
                            false
                    end
                end,
                Matches
            );
        nomatch ->
            []
    end.

%% @doc Well-known alternative base class suggestions.
-spec alternative_classes(atom()) -> [{atom(), binary()}].
alternative_classes('Object') ->
    [
        {'Value', <<"immutable data (Value subclass)">>},
        {'Actor', <<"concurrent processes (Actor subclass)">>}
    ];
alternative_classes('Value') ->
    [
        {'Object', <<"mutable reference types">>},
        {'Actor', <<"concurrent processes">>}
    ];
alternative_classes('Actor') ->
    [
        {'Object', <<"non-actor reference types">>},
        {'Value', <<"immutable data">>}
    ];
alternative_classes(_) ->
    [].

%% @doc Find sibling classes (other direct subclasses of the same parent).
%% Limited to 5 entries to avoid overwhelming output.
-spec sibling_classes(atom(), atom() | none) -> [{atom(), binary()}].
sibling_classes(_ClassName, none) ->
    [];
sibling_classes(ClassName, Superclass) ->
    try
        Subs = beamtalk_class_registry:direct_subclasses(Superclass),
        Siblings = lists:sort([S || S <- Subs, S =/= ClassName]),
        case length(Siblings) of
            0 ->
                [];
            N when N =< 5 ->
                [{S, <<>>} || S <- Siblings];
            N ->
                {First5, _} = lists:split(5, Siblings),
                Remaining = N - 5,
                Entries = [{S, <<>>} || S <- First5],
                Suffix = iolist_to_binary([
                    <<"(">>, integer_to_binary(Remaining), <<" more siblings)">>
                ]),
                Entries ++ [{'...', Suffix}]
        end
    catch
        _:_ -> []
    end.

%% @doc Build the complete see-also list from all sources, deduplicated.
-spec build_see_also(atom(), atom() | none, binary() | none) -> [{atom(), binary()}].
build_see_also(ClassName, Superclass, ModuleDoc) ->
    %% Collect from all three sources
    FromDoc = extract_see_also(ModuleDoc),
    FromAlternatives = alternative_classes(ClassName),
    FromSiblings = sibling_classes(ClassName, Superclass),
    %% Merge all entries, deduplicating by class name.
    %% Priority: @see tags > alternatives > siblings (first wins).
    All = FromDoc ++ FromAlternatives ++ FromSiblings,
    deduplicate_see_also(All, #{}, []).

%% @doc Deduplicate see-also entries, keeping the first occurrence of each class name.
-spec deduplicate_see_also([{atom(), binary()}], map(), [{atom(), binary()}]) ->
    [{atom(), binary()}].
deduplicate_see_also([], _Seen, Acc) ->
    lists:reverse(Acc);
deduplicate_see_also([{Name, Desc} | Rest], Seen, Acc) ->
    case maps:is_key(Name, Seen) of
        true -> deduplicate_see_also(Rest, Seen, Acc);
        false -> deduplicate_see_also(Rest, Seen#{Name => true}, [{Name, Desc} | Acc])
    end.

%% @doc Format the see-also section for display.
-spec format_see_also([{atom(), binary()}]) -> binary().
format_see_also([]) ->
    <<>>;
format_see_also(Entries) ->
    Lines = [
        case Desc of
            <<>> -> iolist_to_binary([<<"  ">>, atom_to_binary(Name, utf8)]);
            _ -> iolist_to_binary([<<"  ">>, atom_to_binary(Name, utf8), <<" — ">>, Desc])
        end
     || {Name, Desc} <- Entries
    ],
    iolist_to_binary([<<"\nSee also:\n">>, lists:join(<<"\n">>, Lines)]).

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
        [?MAX_HIERARCHY_DEPTH, ClassName],
        #{domain => [beamtalk, runtime]}
    ),
    #{};
collect_flattened_methods(ClassName, ClassPid, Depth) ->
    {ok, LocalMethods} = gen_server:call(ClassPid, get_instance_methods, 5000),
    LocalFlat = maps:map(fun(_Sel, Info) -> {ClassName, Info} end, LocalMethods),
    Superclass = beamtalk_runtime_api:superclass(ClassPid),
    SuperFlat = collect_chain_methods(Superclass, Depth + 1),
    maps:merge(SuperFlat, LocalFlat).

-spec collect_chain_methods(atom() | none, non_neg_integer()) -> map().
collect_chain_methods(none, _Depth) ->
    #{};
collect_chain_methods(SuperName, Depth) ->
    case beamtalk_runtime_api:whereis_class(SuperName) of
        undefined -> #{};
        SuperPid -> collect_flattened_methods(SuperName, SuperPid, Depth)
    end.

%% @doc Format method-specific documentation output.
-spec format_method_output(
    atom(),
    binary(),
    atom(),
    {binary(), binary() | none, boolean(), boolean()}
) -> binary().
format_method_output(
    ClassName, SelectorBin, DefiningClass, {Signature, DocText, IsSealed, IsInternal}
) ->
    Header = iolist_to_binary([
        <<"== ">>,
        atom_to_binary(ClassName, utf8),
        <<" >> ">>,
        SelectorBin,
        <<" ==">>
    ]),
    %% ADR 0071 Phase 5: Show [internal] and [sealed] modifier tags
    ModifierTags = lists:filter(fun(T) -> T =/= <<>> end, [
        case IsInternal of
            true -> <<"[internal]">>;
            false -> <<>>
        end,
        case IsSealed of
            true -> <<"[sealed]">>;
            false -> <<>>
        end
    ]),
    SealedLine =
        case ModifierTags of
            [] -> <<>>;
            _ -> iolist_to_binary([<<"\n">>, lists:join(<<" ">>, ModifierTags)])
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

%% @doc Format a summary of selectors: show all if ≤5, else first 3 + count.
-spec format_selector_summary(non_neg_integer(), [atom()]) -> iolist().
format_selector_summary(Count, Selectors) ->
    case Count =< 5 of
        true ->
            lists:join(<<", ">>, [atom_to_binary(S, utf8) || S <- Selectors]);
        false ->
            {First3, _Rest} = lists:split(3, Selectors),
            Remaining = Count - 3,
            [
                lists:join(<<", ">>, [atom_to_binary(S, utf8) || S <- First3]),
                <<", ... (">>,
                integer_to_binary(Remaining),
                <<" more)">>
            ]
    end.

%% @doc Format the class-object side documentation output.
-spec format_class_side_output(atom(), [atom()], [{atom(), [atom()]}], [{atom(), [atom()]}]) ->
    binary().
format_class_side_output(ClassName, OwnCM, InhCMGrouped, ProtoGrouped) ->
    ClassNameBin = atom_to_binary(ClassName, utf8),
    Parts = [
        iolist_to_binary([<<"== ">>, ClassNameBin, <<" class ==">>]),

        case OwnCM of
            [] ->
                <<>>;
            _ ->
                Lines = [iolist_to_binary([<<"  ">>, atom_to_binary(S, utf8)]) || S <- OwnCM],
                iolist_to_binary([<<"\nClass methods:\n">>, lists:join(<<"\n">>, Lines)])
        end,

        lists:map(
            fun({FromClass, Selectors}) ->
                Count = length(Selectors),
                iolist_to_binary([
                    <<"\nInherited class methods from ">>,
                    atom_to_binary(FromClass, utf8),
                    <<" (">>,
                    integer_to_binary(Count),
                    <<" methods): ">>,
                    format_selector_summary(Count, Selectors)
                ])
            end,
            InhCMGrouped
        ),

        lists:map(
            fun({FromClass, Selectors}) ->
                Count = length(Selectors),
                iolist_to_binary([
                    <<"\nClass protocol from ">>,
                    atom_to_binary(FromClass, utf8),
                    <<" (">>,
                    integer_to_binary(Count),
                    <<" methods): ">>,
                    format_selector_summary(Count, Selectors)
                ])
            end,
            ProtoGrouped
        ),

        iolist_to_binary([
            <<"\nUse :help ">>, ClassNameBin, <<" class selector for method details.">>
        ])
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

%% @doc Walk the class hierarchy collecting class-side methods.
%% Returns #{Selector => DefiningClass} — local methods shadow inherited ones.
-spec collect_flattened_class_methods(atom(), pid()) -> #{atom() => atom()}.
collect_flattened_class_methods(ClassName, ClassPid) ->
    collect_flattened_class_methods(ClassName, ClassPid, 0).

-spec collect_flattened_class_methods(atom(), pid(), non_neg_integer()) -> #{atom() => atom()}.
collect_flattened_class_methods(_ClassName, _ClassPid, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    #{};
collect_flattened_class_methods(ClassName, ClassPid, Depth) ->
    LocalMethods = gen_server:call(ClassPid, get_local_class_methods, 5000),
    LocalFlat = maps:map(fun(_Sel, _Info) -> ClassName end, LocalMethods),
    Superclass = beamtalk_runtime_api:superclass(ClassPid),
    SuperFlat = collect_class_method_superchain(Superclass, Depth + 1),
    maps:merge(SuperFlat, LocalFlat).

-spec collect_class_method_superchain(atom() | none, non_neg_integer()) -> #{atom() => atom()}.
collect_class_method_superchain(none, _Depth) ->
    #{};
collect_class_method_superchain(SuperName, Depth) ->
    case beamtalk_runtime_api:whereis_class(SuperName) of
        undefined -> #{};
        SuperPid -> collect_flattened_class_methods(SuperName, SuperPid, Depth)
    end.
