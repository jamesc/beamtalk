%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Primitive implementations for the BeamtalkInterface singleton actor.
%%%
%%% **DDD Context:** Runtime
%%%
%%% Implements the `@primitive` methods for the BeamtalkInterface class. The
%%% compiled Beamtalk actor (`bt@stdlib@beamtalk_interface`) delegates all
%%% `@primitive` method calls here via `dispatch/3`.
%%%
%%% All methods are stateless reads from the class registry; no process
%%% dictionary or ETS state is required.
%%%
%%% ## Methods
%%%
%%% | Selector          | Description                                       |
%%% |-------------------|---------------------------------------------------|
%%% | `allClasses'      | List of all registered class names                |
%%% | `classNamed:'     | Class object reference by name, or nil            |
%%% | `globals'         | Class registry snapshot as a map                  |
%%% | `help:'           | Formatted class documentation                     |
%%% | `help:selector:'  | Formatted method documentation                    |
%%% | `version'         | Beamtalk runtime version string                   |

-module(beamtalk_interface_primitives).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([dispatch/3]).

%%% ============================================================================
%%% dispatch/3 — called from compiled bt@stdlib@beamtalk_interface for @primitives
%%% ============================================================================

%% @doc Dispatch a primitive method call for BeamtalkInterface.
%%
%% Called by the compiled `bt@stdlib@beamtalk_interface:dispatch/3`.
-spec dispatch(atom(), list(), term()) -> term().
dispatch(allClasses, [], _Self) ->
    [Name || {Name, _Mod, _Pid} <- live_class_entries()];
dispatch('classNamed:', [ClassName], _Self) ->
    handle_class_named(ClassName);
dispatch(globals, [], _Self) ->
    handle_globals();
dispatch('help:', [ClassArg], _Self) ->
    case handle_help(ClassArg) of
        {error, Err} -> beamtalk_error:raise(Err);
        Result -> Result
    end;
dispatch('help:selector:', [ClassArg, SelectorArg], _Self) ->
    case handle_help_selector(ClassArg, SelectorArg) of
        {error, Err} -> beamtalk_error:raise(Err);
        Result -> Result
    end;
dispatch(version, [], _Self) ->
    case application:get_key(beamtalk_runtime, vsn) of
        {ok, Vsn} -> list_to_binary(Vsn);
        _ -> <<"unknown">>
    end;
dispatch(Selector, _Args, _Self) ->
    Err0 = beamtalk_error:new(does_not_understand, 'BeamtalkInterface'),
    Err1 = beamtalk_error:with_selector(Err0, Selector),
    beamtalk_error:raise(Err1).

%%% ============================================================================
%%% Internal method implementations
%%% ============================================================================

%% @private Look up a class by name.
-spec handle_class_named(binary() | atom() | term()) ->
    tuple() | 'nil' | {error, #beamtalk_error{}}.
handle_class_named(ClassName) when is_binary(ClassName) ->
    try
        ClassAtom = binary_to_existing_atom(ClassName, utf8),
        handle_class_named(ClassAtom)
    catch
        error:badarg ->
            nil
    end;
handle_class_named(ClassName) when is_atom(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            nil;
        Pid when is_pid(Pid) ->
            ModuleName = beamtalk_object_class:module_name(Pid),
            ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
            {beamtalk_object, ClassTag, ModuleName, Pid}
    end;
handle_class_named(_ClassName) ->
    Error0 = beamtalk_error:new(type_error, 'BeamtalkInterface'),
    Error1 = beamtalk_error:with_selector(Error0, 'classNamed:'),
    Error2 = beamtalk_error:with_hint(
        Error1, <<"classNamed: expects an atom or binary class name">>
    ),
    {error, Error2}.

%% @private Get workspace global bindings as a map from class name to class object.
-spec handle_globals() -> map().
handle_globals() ->
    lists:foldl(
        fun({Name, ModuleName, Pid}, Acc) ->
            ClassTag = beamtalk_class_registry:class_object_tag(Name),
            ClassObj = {beamtalk_object, ClassTag, ModuleName, Pid},
            maps:put(Name, ClassObj, Acc)
        end,
        #{},
        live_class_entries()
    ).

%% @private Format class documentation for help:.
-spec handle_help(term()) -> binary() | {error, #beamtalk_error{}}.
handle_help(ClassArg) ->
    case resolve_class_name(ClassArg) of
        {error, Err} ->
            {error, Err};
        {ok, ClassName} ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                undefined ->
                    {error, make_class_not_found_error(ClassName)};
                ClassPid ->
                    try
                        format_class_help(ClassName, ClassPid)
                    catch
                        exit:{noproc, _} -> {error, make_class_not_found_error(ClassName)};
                        exit:{timeout, _} -> {error, make_class_not_found_error(ClassName)}
                    end
            end
    end.

%% @private Format method documentation for help:selector:.
-spec handle_help_selector(term(), atom()) -> binary() | {error, #beamtalk_error{}}.
handle_help_selector(ClassArg, SelectorArg) ->
    case resolve_class_name(ClassArg) of
        {error, Err} ->
            {error, Err};
        {ok, ClassName} ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                undefined ->
                    {error, make_class_not_found_error(ClassName)};
                ClassPid ->
                    case ensure_atom(SelectorArg) of
                        {error, Err} ->
                            {error, Err};
                        SelectorAtom ->
                            try
                                case beamtalk_method_resolver:resolve(ClassPid, SelectorAtom) of
                                    nil ->
                                        {error,
                                            make_method_not_found_error(ClassName, SelectorAtom)};
                                    MethodObj when is_map(MethodObj) ->
                                        DefiningClass = find_defining_class(ClassPid, SelectorAtom),
                                        format_method_help(
                                            ClassName, SelectorAtom, DefiningClass, MethodObj
                                        )
                                end
                            catch
                                exit:{noproc, _} ->
                                    {error, make_class_not_found_error(ClassName)};
                                exit:{timeout, _} ->
                                    {error, make_class_not_found_error(ClassName)}
                            end
                    end
            end
    end.

%% @private Resolve a class argument to an atom class name.
-spec resolve_class_name(term()) -> {ok, atom()} | {error, #beamtalk_error{}}.
resolve_class_name({beamtalk_object, _ClassTag, _Mod, ClassPid}) when is_pid(ClassPid) ->
    try
        Name = beamtalk_object_class:class_name(ClassPid),
        {ok, Name}
    catch
        exit:{noproc, _} ->
            Error0 = beamtalk_error:new(class_not_found, 'BeamtalkInterface'),
            {error, beamtalk_error:with_message(Error0, <<"Class process no longer alive">>)};
        exit:{timeout, _} ->
            Error0 = beamtalk_error:new(class_not_found, 'BeamtalkInterface'),
            {error, beamtalk_error:with_message(Error0, <<"Class process not responding">>)}
    end;
resolve_class_name(Name) when is_atom(Name) ->
    {ok, Name};
resolve_class_name(Name) when is_binary(Name) ->
    try
        {ok, binary_to_existing_atom(Name, utf8)}
    catch
        error:badarg ->
            {error, make_class_not_found_error(Name)}
    end;
resolve_class_name(_Other) ->
    Error0 = beamtalk_error:new(type_error, 'BeamtalkInterface'),
    {error, beamtalk_error:with_message(Error0, <<"Expected a class or symbol argument">>)}.

%% @private Ensure a selector argument is an existing atom.
-spec ensure_atom(atom() | binary()) -> atom() | {error, #beamtalk_error{}}.
ensure_atom(A) when is_atom(A) -> A;
ensure_atom(B) when is_binary(B) ->
    try
        binary_to_existing_atom(B, utf8)
    catch
        error:badarg ->
            Err0 = beamtalk_error:new(type_error, 'BeamtalkInterface'),
            Err1 = beamtalk_error:with_selector(Err0, 'help:selector:'),
            {error,
                beamtalk_error:with_message(
                    Err1,
                    iolist_to_binary([<<"Unknown selector: ">>, B])
                )}
    end.

%% @private Format class-level help output.
-spec format_class_help(atom(), pid()) -> binary().
format_class_help(ClassName, ClassPid) ->
    Superclass = gen_server:call(ClassPid, superclass, 5000),
    IsSealed = gen_server:call(ClassPid, is_sealed, 5000),
    IsAbstract = gen_server:call(ClassPid, is_abstract, 5000),
    ModuleDoc =
        case gen_server:call(ClassPid, get_doc, 5000) of
            none -> none;
            Doc when is_binary(Doc) -> Doc
        end,

    Flattened = collect_flattened_methods(ClassName, ClassPid),

    {Own, Inherited} = maps:fold(
        fun(Selector, {DefClass, MethodInfo}, {OwnAcc, InhAcc}) ->
            case DefClass of
                ClassName ->
                    MethodSealed = maps:get(is_sealed, MethodInfo, false),
                    {[{Selector, MethodSealed} | OwnAcc], InhAcc};
                _ ->
                    {OwnAcc, [{Selector, DefClass} | InhAcc]}
            end
        end,
        {[], []},
        Flattened
    ),

    OwnSelectors = lists:sort([S || {S, _} <- Own]),
    SealedMap = maps:from_list(Own),
    OwnDocs = lists:map(
        fun(Sel) ->
            {Sig, _Doc} = get_method_sig(ClassPid, Sel),
            IsSealedMethod = maps:get(Sel, SealedMap, false),
            {Sel, Sig, IsSealedMethod}
        end,
        OwnSelectors
    ),

    InheritedGrouped = group_by_class(lists:sort(Inherited)),

    NameBin = atom_to_binary(ClassName, utf8),
    Header =
        case Superclass of
            none ->
                iolist_to_binary([<<"== ">>, NameBin, <<" ==">>]);
            Super ->
                iolist_to_binary([
                    <<"== ">>, NameBin, <<" < ">>, atom_to_binary(Super, utf8), <<" ==">>
                ])
        end,

    ModifierPart =
        case {IsSealed, IsAbstract} of
            {true, true} -> <<"\n[sealed] [abstract]">>;
            {true, false} -> <<"\n[sealed]">>;
            {false, true} -> <<"\n[abstract]">>;
            {false, false} -> <<>>
        end,

    DocPart =
        case ModuleDoc of
            none -> <<>>;
            Text -> iolist_to_binary([<<"\n">>, Text])
        end,

    OwnMethodsPart =
        case OwnDocs of
            [] ->
                <<>>;
            _ ->
                Lines = lists:map(
                    fun
                        ({_Sel, Sig, true}) ->
                            iolist_to_binary([<<"  ">>, Sig, <<" [sealed]">>]);
                        ({_Sel, Sig, false}) ->
                            iolist_to_binary([<<"  ">>, Sig])
                    end,
                    OwnDocs
                ),
                iolist_to_binary([<<"\nInstance methods:\n">>, lists:join(<<"\n">>, Lines)])
        end,

    InheritedParts = lists:map(
        fun({FromClass, Selectors}) ->
            Count = length(Selectors),
            Summary =
                case Count =< 5 of
                    true ->
                        lists:join(<<", ">>, [atom_to_binary(S, utf8) || S <- Selectors]);
                    false ->
                        {First3, _} = lists:split(3, Selectors),
                        Remaining = Count - 3,
                        iolist_to_binary([
                            lists:join(<<", ">>, [atom_to_binary(S, utf8) || S <- First3]),
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

    HintPart = <<"\nUse Beamtalk help: ClassName selector: #method for method details.">>,

    AllParts = [Header, ModifierPart, DocPart, OwnMethodsPart | InheritedParts] ++ [HintPart],
    iolist_to_binary(
        lists:filter(
            fun
                (<<>>) -> false;
                (_) -> true
            end,
            lists:flatten(AllParts)
        )
    ).

%% @private Format method-level help output.
-spec format_method_help(atom(), atom(), atom(), map()) -> binary().
format_method_help(ClassName, SelectorAtom, DefiningClass, MethodObj) ->
    SelectorBin = atom_to_binary(SelectorAtom, utf8),
    NameBin = atom_to_binary(ClassName, utf8),

    Header = iolist_to_binary([<<"== ">>, NameBin, <<" >> ">>, SelectorBin, <<" ==">>]),

    IsSealed =
        case maps:get('__method_info__', MethodObj, #{}) of
            MethodInfo when is_map(MethodInfo) ->
                maps:get(is_sealed, MethodInfo, false);
            _ ->
                false
        end,

    SealedLine =
        case IsSealed of
            true -> <<"\n[sealed]">>;
            false -> <<>>
        end,

    InheritedPart =
        case DefiningClass of
            ClassName ->
                <<>>;
            _ ->
                iolist_to_binary([
                    <<"\n(inherited from ">>, atom_to_binary(DefiningClass, utf8), <<")">>
                ])
        end,

    Signature =
        case maps:get('__signature__', MethodObj, nil) of
            nil -> SelectorBin;
            SigBin when is_binary(SigBin) -> SigBin
        end,

    SignatureLine = iolist_to_binary([<<"\n  ">>, Signature]),

    DocPart =
        case maps:get('__doc__', MethodObj, nil) of
            nil -> <<>>;
            DocBin when is_binary(DocBin) -> iolist_to_binary([<<"\n\n">>, DocBin])
        end,

    iolist_to_binary([Header, SealedLine, InheritedPart, SignatureLine, DocPart]).

%% @private Get method signature from a class pid.
-spec get_method_sig(pid(), atom()) -> {binary(), binary() | none}.
get_method_sig(ClassPid, Selector) ->
    case gen_server:call(ClassPid, {method, Selector}, 5000) of
        nil ->
            {atom_to_binary(Selector, utf8), none};
        MethodObj when is_map(MethodObj) ->
            Doc =
                case maps:get('__doc__', MethodObj, nil) of
                    nil -> none;
                    D when is_binary(D) -> D
                end,
            {atom_to_binary(Selector, utf8), Doc}
    end.

%% @private Walk the class hierarchy to collect flattened method map.
-spec collect_flattened_methods(atom(), pid()) -> map().
collect_flattened_methods(ClassName, ClassPid) ->
    collect_flattened_methods(ClassName, ClassPid, 0).

-spec collect_flattened_methods(atom(), pid(), non_neg_integer()) -> map().
collect_flattened_methods(_ClassName, _ClassPid, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
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

%% @private Find which class in the hierarchy defines a selector.
-spec find_defining_class(pid(), atom()) -> atom().
find_defining_class(ClassPid, Selector) ->
    find_defining_class(ClassPid, Selector, 0).

-spec find_defining_class(pid(), atom(), non_neg_integer()) -> atom().
find_defining_class(ClassPid, _Selector, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    gen_server:call(ClassPid, class_name, 5000);
find_defining_class(ClassPid, Selector, Depth) ->
    ClassName = gen_server:call(ClassPid, class_name, 5000),
    case gen_server:call(ClassPid, {method, Selector}, 5000) of
        nil ->
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
    lists:sort(
        maps:fold(
            fun(Class, Selectors, Acc) ->
                [{Class, lists:sort(Selectors)} | Acc]
            end,
            [],
            Grouped
        )
    ).

%% @private Build a structured error for a class not found.
-spec make_class_not_found_error(atom() | binary()) -> #beamtalk_error{}.
make_class_not_found_error(ClassName) ->
    NameBin =
        case ClassName of
            A when is_atom(A) -> atom_to_binary(A, utf8);
            B when is_binary(B) -> B
        end,
    Err0 = beamtalk_error:new(class_not_found, 'BeamtalkInterface'),
    Err1 = beamtalk_error:with_message(
        Err0,
        iolist_to_binary([<<"Class '">>, NameBin, <<"' not found.">>])
    ),
    beamtalk_error:with_hint(
        Err1,
        <<"Use Beamtalk allClasses for available classes.">>
    ).

%% @private Build a structured error for a method not found.
-spec make_method_not_found_error(atom(), atom()) -> #beamtalk_error{}.
make_method_not_found_error(ClassName, Selector) ->
    NameBin = atom_to_binary(ClassName, utf8),
    SelBin = atom_to_binary(Selector, utf8),
    Err0 = beamtalk_error:new(does_not_understand, ClassName),
    Err1 = beamtalk_error:with_selector(Err0, Selector),
    Err2 = beamtalk_error:with_message(
        Err1,
        iolist_to_binary([NameBin, <<" does not understand ">>, SelBin])
    ),
    beamtalk_error:with_hint(
        Err2,
        iolist_to_binary([<<"Use Beamtalk help: ">>, NameBin, <<" to see available methods.">>])
    ).

%% @private Fetch all live class entries from the registry.
-type class_entry() :: {atom(), module(), pid()}.
-spec live_class_entries() -> [class_entry()].
live_class_entries() ->
    try
        ClassPids = beamtalk_class_registry:all_classes(),
        lists:filtermap(
            fun(Pid) ->
                try
                    Name = beamtalk_object_class:class_name(Pid),
                    Mod = beamtalk_object_class:module_name(Pid),
                    {true, {Name, Mod, Pid}}
                catch
                    exit:{noproc, _} -> false;
                    exit:{timeout, _} -> false
                end
            end,
            ClassPids
        )
    catch
        exit:{noproc, _} ->
            ?LOG_WARNING("pg not started when fetching class entries", #{module => ?MODULE}),
            []
    end.
