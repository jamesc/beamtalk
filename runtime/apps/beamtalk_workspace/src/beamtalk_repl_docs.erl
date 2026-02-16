%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc REPL documentation helper — fetches and formats EEP-48 docs.
%%%
%%% **DDD Context:** REPL — Documentation
%%%
%%% Provides documentation lookup for `:help ClassName` and
%%% `:help ClassName selector` commands. Fetches EEP-48 doc chunks
%%% from compiled .beam files via `code:get_doc/1` and walks the
%%% class hierarchy for inherited method docs.

-module(beamtalk_repl_docs).

-export([format_class_docs/1, format_method_doc/2]).

%% Exported for testing (only in test builds)
-ifdef(TEST).
-export([safe_to_existing_atom/1, get_module_doc/1, get_doc_entries/1,
         find_doc_entry/2, get_method_signatures/2, get_method_doc/2,
         group_by_class/1, format_class_output/5, format_superclass/1,
         format_method_output/4, format_metaclass_docs/0,
         format_metaclass_method_doc/1, metaclass_method_doc/1]).
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
                ModuleName = gen_server:call(ClassPid, module_name, 5000),
                Superclass = gen_server:call(ClassPid, superclass, 5000),
                {ok, Flattened} = gen_server:call(ClassPid, get_flattened_methods, 5000),

                %% Get module doc from EEP-48 chunks
                ModuleDoc = get_module_doc(ModuleName),

                %% Build method listing grouped by defining class
                {Own, Inherited} = maps:fold(
                    fun(Selector, {DefiningClass, _MethodInfo}, {OwnAcc, InhAcc}) ->
                        case DefiningClass of
                            ClassName ->
                                {[Selector | OwnAcc], InhAcc};
                            _ ->
                                {OwnAcc, [{Selector, DefiningClass} | InhAcc]}
                        end
                    end,
                    {[], []},
                    Flattened
                ),

                %% Get EEP-48 method signatures for own methods
                OwnDocs = get_method_signatures(ModuleName, lists:sort(Own)),

                %% Group inherited by defining class
                InheritedGrouped = group_by_class(lists:sort(Inherited)),

                %% Format the output
                Output = format_class_output(ClassName, Superclass, ModuleDoc,
                                              OwnDocs, InheritedGrouped),
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
                    case gen_server:call(ClassPid, {lookup_flattened, SelectorAtom}, 5000) of
                        {ok, DefiningClass, _MethodInfo} ->
                            %% Found the method — get its doc from the defining class
                            DocInfo = case beamtalk_class_registry:whereis_class(DefiningClass) of
                                undefined ->
                                    {atom_to_binary(SelectorAtom, utf8), none};
                                DefClassPid ->
                                    DefModule = gen_server:call(DefClassPid, module_name, 5000),
                                    get_method_doc(DefModule, SelectorAtom)
                            end,
                            Output = format_method_output(ClassName, SelectorBin,
                                                           DefiningClass, DocInfo),
                            {ok, Output};
                        not_found ->
                            {error, {method_not_found, ClassName, SelectorBin}}
                    end
            end
    end.

%%% Internal functions

%% @private Return hardcoded documentation for the Metaclass terminal sentinel.
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
        <<"  describe         Return a description of the class\n">>,
        <<"\nUse :help ClassName for docs on a specific class.">>
    ]).

%% @private Return documentation for a Metaclass method.
-spec format_metaclass_method_doc(binary()) -> {ok, binary()} | {error, term()}.
format_metaclass_method_doc(SelectorBin) ->
    case metaclass_method_doc(SelectorBin) of
        {ok, Doc} ->
            {ok, iolist_to_binary([
                <<"Metaclass >> ">>, SelectorBin,
                <<"\n  ">>, SelectorBin,
                <<"\n\n">>, Doc
            ])};
        not_found ->
            {error, {method_not_found, 'Metaclass', SelectorBin}}
    end.

%% @private Lookup doc text for known Metaclass class-side methods.
-spec metaclass_method_doc(binary()) -> {ok, binary()} | not_found.
metaclass_method_doc(<<"new">>) ->
    {ok, <<"Create a new instance of the class.">>};
metaclass_method_doc(<<"spawn">>) ->
    {ok, <<"Create a new actor instance. Returns an actor reference.">>};
metaclass_method_doc(<<"spawnWith:">>) ->
    {ok, <<"Create a new actor with initial state from a Dictionary.">>};
metaclass_method_doc(<<"describe">>) ->
    {ok, <<"Return a description of the class.">>};
metaclass_method_doc(_) ->
    not_found.

%% @private Safe atom conversion — returns error instead of creating new atoms.
-spec safe_to_existing_atom(binary()) -> {ok, atom()} | {error, badarg}.
safe_to_existing_atom(<<>>) -> {error, badarg};
safe_to_existing_atom(Bin) when is_binary(Bin) ->
    try binary_to_existing_atom(Bin, utf8) of
        Atom -> {ok, Atom}
    catch
        error:badarg -> {error, badarg}
    end.

%% @private Get module-level doc from EEP-48 chunks.
-spec get_module_doc(atom()) -> binary() | none.
get_module_doc(ModuleName) ->
    case code:get_doc(ModuleName) of
        {ok, {docs_v1, _Anno, _Lang, _Format, ModDoc, _Meta, _Docs}} ->
            case ModDoc of
                #{<<"en">> := Text} -> Text;
                _ -> none
            end;
        _ ->
            none
    end.

%% @private Get method signatures from EEP-48 doc entries.
-spec get_method_signatures(atom(), [atom()]) -> [{atom(), binary(), binary() | none}].
get_method_signatures(ModuleName, Selectors) ->
    DocEntries = get_doc_entries(ModuleName),
    lists:filtermap(
        fun(Selector) ->
            case find_doc_entry(Selector, DocEntries) of
                {ok, Signature, Doc} ->
                    {true, {Selector, Signature, Doc}};
                not_found ->
                    %% Method exists but no doc entry — show selector name
                    {true, {Selector, atom_to_binary(Selector, utf8), none}}
            end
        end,
        Selectors
    ).

%% @private Get full method doc (signature + doc text) for a specific selector.
-spec get_method_doc(atom(), atom()) -> {binary(), binary() | none}.
get_method_doc(ModuleName, Selector) ->
    DocEntries = get_doc_entries(ModuleName),
    case find_doc_entry(Selector, DocEntries) of
        {ok, Signature, Doc} -> {Signature, Doc};
        not_found -> {atom_to_binary(Selector, utf8), none}
    end.

%% @private Extract doc entries from EEP-48 chunks.
-spec get_doc_entries(atom()) -> list().
get_doc_entries(ModuleName) ->
    case code:get_doc(ModuleName) of
        {ok, {docs_v1, _Anno, _Lang, _Format, _ModDoc, _Meta, Docs}} ->
            Docs;
        _ ->
            []
    end.

%% @private Find a doc entry by selector atom.
-spec find_doc_entry(atom(), list()) -> {ok, binary(), binary() | none} | not_found.
find_doc_entry(Selector, DocEntries) ->
    Result = lists:dropwhile(
        fun(Entry) ->
            case Entry of
                {{function, _Name, _Arity}, _Anno, _Sigs, _Doc, #{selector := S}} ->
                    S =/= Selector;
                _ ->
                    true
            end
        end,
        DocEntries
    ),
    case Result of
        [{{function, _Name, _Arity}, _Anno, Sigs, Doc, _Meta} | _] ->
            Signature = case Sigs of
                [S | _] -> S;
                _ -> atom_to_binary(Selector, utf8)
            end,
            DocText = case Doc of
                #{<<"en">> := Text} -> Text;
                _ -> none
            end,
            {ok, Signature, DocText};
        _ ->
            not_found
    end.

%% @private Group inherited methods by defining class.
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

%% @private Format the complete class documentation output.
-spec format_class_output(atom(), atom() | none, binary() | none,
                          [{atom(), binary(), binary() | none}],
                          [{atom(), [atom()]}]) -> binary().
format_class_output(ClassName, Superclass, ModuleDoc, OwnDocs, InheritedGrouped) ->
    Parts = [
        %% Header
        iolist_to_binary([<<"== ">>, atom_to_binary(ClassName, utf8),
                          format_superclass(Superclass), <<" ==">>]),

        %% Module doc
        case ModuleDoc of
            none -> <<>>;
            Text ->
                iolist_to_binary([<<"\n">>, Text])
        end,

        %% Own methods
        case OwnDocs of
            [] -> <<>>;
            _ ->
                MethodLines = lists:map(
                    fun({_Sel, Sig, _Doc}) ->
                        iolist_to_binary([<<"  ">>, Sig])
                    end,
                    OwnDocs
                ),
                iolist_to_binary([<<"\nInstance methods:\n">>,
                                  lists:join(<<"\n">>, MethodLines)])
        end,

        %% Inherited methods (compact summary per class)
        lists:map(
            fun({FromClass, Selectors}) ->
                Count = length(Selectors),
                Summary = case Count =< 5 of
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
                iolist_to_binary([<<"\nInherited from ">>,
                                  atom_to_binary(FromClass, utf8),
                                  <<" (">>, integer_to_binary(Count), <<" methods): ">>,
                                  Summary])
            end,
            InheritedGrouped
        ),

        %% Hint
        <<"\nUse :help ClassName selector for method details.">>
    ],
    iolist_to_binary(lists:filter(fun(<<>>) -> false; (_) -> true end,
                                   lists:flatten(Parts))).

%% @private Format superclass portion of header.
-spec format_superclass(atom() | none) -> iolist().
format_superclass(none) -> [];
format_superclass(Superclass) ->
    [<<" < ">>, atom_to_binary(Superclass, utf8)].

%% @private Format method-specific documentation output.
-spec format_method_output(atom(), binary(), atom(),
                           {binary(), binary() | none}) -> binary().
format_method_output(ClassName, SelectorBin, DefiningClass, {Signature, DocText}) ->
    Header = iolist_to_binary([atom_to_binary(ClassName, utf8),
                                <<" >> ">>, SelectorBin]),
    Inherited = case DefiningClass of
        ClassName -> <<>>;
        _ -> iolist_to_binary([<<"\n(inherited from ">>,
                                atom_to_binary(DefiningClass, utf8), <<")">>])
    end,
    SignatureLine = iolist_to_binary([<<"\n  ">>, Signature]),
    Doc = case DocText of
        none -> <<>>;
        Text -> iolist_to_binary([<<"\n\n">>, Text])
    end,
    iolist_to_binary([Header, Inherited, SignatureLine, Doc]).

