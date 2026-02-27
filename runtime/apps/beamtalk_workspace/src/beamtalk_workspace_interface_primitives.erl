%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Primitive implementations for the WorkspaceInterface singleton actor.
%%%
%%% **DDD Context:** Workspace
%%%
%%% Implements the `@primitive` methods for the WorkspaceInterface class. The
%%% compiled Beamtalk actor (`bt@stdlib@workspace_interface`) delegates all
%%% `@primitive` method calls here via `dispatch/3`.
%%%
%%% ## State management
%%%
%%% User-registered bindings (`bind:as:` / `unbind:`) are stored in a public
%%% named ETS table `beamtalk_wi_user_bindings` keyed by `{GenServerPid, Name}`.
%%% The ETS table is created lazily on the first dispatch/3 call. It is
%%% accessible from external processes (`get_user_bindings/0`, `get_session_bindings/0`).
%%%
%%% ## External API
%%%
%%% `get_user_bindings/0` and `get_session_bindings/0` are called by
%%% `beamtalk_repl_eval` and `beamtalk_repl_shell` to inject workspace bindings
%%% into REPL session state. They read from ETS for the registered `'Workspace'`
%%% gen_server pid.
%%%
%%% ## Methods
%%%
%%% | Selector      | Description                                         |
%%% |---------------|-----------------------------------------------------|
%%% | `actors'      | List all live actor object references               |
%%% | `actorAt:'    | Look up actor by pid string                         |
%%% | `classes'     | List all loaded user classes                        |
%%% | `load:'       | Compile and load a .bt file                         |
%%% | `globals'     | Full workspace namespace snapshot (Dictionary)      |
%%% | `bind:as:'    | Register a value in workspace namespace             |
%%% | `unbind:'     | Remove a value from workspace namespace             |

-module(beamtalk_workspace_interface_primitives).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([dispatch/3]).
%% Stable external API (called by repl_eval and repl_shell)
-export([get_user_bindings/0, get_session_bindings/0]).

%% ETS table name for user workspace bindings
-define(WI_BINDINGS_TABLE, beamtalk_wi_user_bindings).

%%% ============================================================================
%%% dispatch/3 — called from compiled bt@stdlib@workspace_interface for @primitives
%%% ============================================================================

%% @doc Dispatch a primitive method call for WorkspaceInterface.
%%
%% Called by the compiled `bt@stdlib@workspace_interface:dispatch/3`.
%% All mutable state is stored in a public named ETS table keyed by the
%% gen_server PID so external callers can access user bindings without
%% re-entering the gen_server.
-spec dispatch(atom(), list(), term()) -> term().
dispatch(actors, [], _Self) ->
    handle_actors();
dispatch('actorAt:', [PidStr], _Self) ->
    handle_actor_at(PidStr);
dispatch(classes, [], _Self) ->
    handle_classes();
dispatch('load:', [Path], _Self) ->
    case handle_load(Path) of
        {error, Err} -> beamtalk_error:raise(Err);
        Result -> Result
    end;
dispatch(globals, [], Self) ->
    GsPid = erlang:element(4, Self),
    ModuleName = erlang:element(3, Self),
    ensure_bindings_table(),
    UserBindings = user_bindings_for(GsPid),
    handle_globals(GsPid, ModuleName, UserBindings);
dispatch('bind:as:', [Value, Name], Self) ->
    ensure_bindings_table(),
    GsPid = erlang:element(4, Self),
    case to_atom_name(Name) of
        {error, Err} ->
            beamtalk_error:raise(Err);
        AtomName ->
            case check_bind_conflicts(AtomName) of
                ok ->
                    ets:insert(?WI_BINDINGS_TABLE, {{GsPid, AtomName}, Value}),
                    spawn(fun() -> maybe_warn_loaded_class(AtomName) end),
                    nil;
                {error, Err} ->
                    beamtalk_error:raise(Err)
            end
    end;
dispatch('unbind:', [Name], Self) ->
    ensure_bindings_table(),
    GsPid = erlang:element(4, Self),
    case to_atom_name(Name) of
        {error, Err} ->
            beamtalk_error:raise(Err);
        AtomName ->
            case ets:lookup(?WI_BINDINGS_TABLE, {GsPid, AtomName}) of
                [] ->
                    Err0 = beamtalk_error:new(name_not_found, 'WorkspaceInterface'),
                    Err1 = beamtalk_error:with_selector(Err0, 'unbind:'),
                    beamtalk_error:raise(
                        beamtalk_error:with_message(
                            Err1,
                            iolist_to_binary([
                                atom_to_binary(AtomName, utf8),
                                <<" is not a registered workspace name">>
                            ])
                        )
                    );
                _ ->
                    ets:delete(?WI_BINDINGS_TABLE, {GsPid, AtomName}),
                    nil
            end
    end;
dispatch(Selector, _Args, _Self) ->
    Err0 = beamtalk_error:new(does_not_understand, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, Selector),
    beamtalk_error:raise(
        beamtalk_error:with_hint(
            Err1, <<"To list available selectors, use: Workspace methods">>
        )
    ).

%%% ============================================================================
%%% Stable external API
%%% ============================================================================

%% @doc Return workspace-level user bindings (for REPL session injection).
%% Called before each eval to merge workspace bindings into session bindings.
-spec get_user_bindings() -> #{atom() => term()}.
get_user_bindings() ->
    case whereis('Workspace') of
        undefined ->
            #{};
        Pid ->
            case ets:info(?WI_BINDINGS_TABLE, id) of
                undefined -> #{};
                _ -> user_bindings_for(Pid)
            end
    end.

%% @doc Return non-class workspace globals for session binding injection.
%% Includes singletons (Transcript, Beamtalk, Workspace) and user-registered
%% bind:as: names. Class objects are excluded.
-spec get_session_bindings() -> #{atom() => term()}.
get_session_bindings() ->
    case whereis('Workspace') of
        undefined ->
            #{};
        Pid ->
            UserBindings =
                case ets:info(?WI_BINDINGS_TABLE, id) of
                    undefined -> #{};
                    _ -> user_bindings_for(Pid)
                end,
            handle_session_bindings(Pid, undefined, UserBindings)
    end.

%%% ============================================================================
%%% Internal helpers — ETS management
%%% ============================================================================

%% @private Ensure the bindings ETS table exists.
%% Creates it on first call; safe to call repeatedly.
-spec ensure_bindings_table() -> ok.
ensure_bindings_table() ->
    case ets:info(?WI_BINDINGS_TABLE, id) of
        undefined ->
            try
                ets:new(?WI_BINDINGS_TABLE, [set, public, named_table])
            catch
                error:badarg ->
                    %% Another process created it concurrently — that's fine
                    ok
            end,
            ok;
        _ ->
            ok
    end.

%% @private Read all user bindings for a given gen_server PID as a map.
-spec user_bindings_for(pid()) -> #{atom() => term()}.
user_bindings_for(Pid) ->
    Entries = ets:select(
        ?WI_BINDINGS_TABLE,
        [{{{Pid, '$1'}, '$2'}, [], [{{'$1', '$2'}}]}]
    ),
    maps:from_list(Entries).

%%% ============================================================================
%%% Internal method implementations
%%% ============================================================================

%% @private Get all live actors as beamtalk_object references.
-spec handle_actors() -> [tuple()].
handle_actors() ->
    case whereis(beamtalk_actor_registry) of
        undefined ->
            [];
        RegistryPid ->
            Actors = beamtalk_repl_actors:list_actors(RegistryPid),
            lists:filtermap(fun wrap_actor/1, Actors)
    end.

%% @private Look up a specific actor by pid string.
-spec handle_actor_at(binary() | list()) -> tuple() | 'nil'.
handle_actor_at(PidStr) when is_binary(PidStr) ->
    handle_actor_at(binary_to_list(PidStr));
handle_actor_at(PidStr) when is_list(PidStr) ->
    try
        Pid = list_to_pid(PidStr),
        case whereis(beamtalk_actor_registry) of
            undefined ->
                nil;
            RegistryPid ->
                case beamtalk_repl_actors:get_actor(RegistryPid, Pid) of
                    {ok, Metadata} ->
                        case wrap_actor(Metadata) of
                            {true, Obj} -> Obj;
                            false -> nil
                        end;
                    {error, not_found} ->
                        nil
                end
        end
    catch
        error:badarg -> nil
    end;
handle_actor_at(_) ->
    nil.

%% @private Return all loaded user classes (those with a source file recorded).
-spec handle_classes() -> [tuple()].
handle_classes() ->
    try
        ClassPids = beamtalk_class_registry:all_classes(),
        lists:filtermap(
            fun(Pid) ->
                try
                    ClassName = beamtalk_object_class:class_name(Pid),
                    ModuleName = beamtalk_object_class:module_name(Pid),
                    case source_file_from_module(ModuleName) of
                        nil ->
                            false;
                        _SourceFile ->
                            ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
                            {true, {beamtalk_object, ClassTag, ModuleName, Pid}}
                    end
                catch
                    exit:{noproc, _} -> false;
                    exit:{timeout, _} -> false
                end
            end,
            ClassPids
        )
    catch
        exit:{noproc, _} ->
            []
    end.

%% @private Load a .bt file, compiling and registering the class.
-spec handle_load(term()) -> nil | {error, #beamtalk_error{}}.
handle_load(Path) when is_binary(Path) ->
    handle_load(binary_to_list(Path));
handle_load(Path) when is_list(Path) ->
    case beamtalk_repl_eval:reload_class_file(Path) of
        ok ->
            nil;
        {error, {file_not_found, _}} ->
            Err0 = beamtalk_error:new(file_not_found, 'WorkspaceInterface'),
            Err1 = beamtalk_error:with_selector(Err0, 'load:'),
            {error,
                beamtalk_error:with_message(
                    Err1,
                    iolist_to_binary([<<"File not found: ">>, Path])
                )};
        {error, Reason} ->
            Err0 = beamtalk_error:new(load_error, 'WorkspaceInterface'),
            Err1 = beamtalk_error:with_selector(Err0, 'load:'),
            Err2 = beamtalk_error:with_message(
                Err1,
                iolist_to_binary([<<"Failed to load: ">>, Path])
            ),
            {error, beamtalk_error:with_details(Err2, #{reason => Reason})}
    end;
handle_load(Other) ->
    TypeName = value_type_name(Other),
    Err0 = beamtalk_error:new(type_error, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'load:'),
    {error,
        beamtalk_error:with_message(
            Err1,
            iolist_to_binary([<<"load: expects a String path, got ">>, TypeName])
        )}.

%% @private Return the full workspace globals snapshot.
-spec handle_globals(pid(), atom(), map()) -> map().
handle_globals(GsPid, ModuleName, UserBindings) ->
    Base = handle_session_bindings(GsPid, ModuleName, UserBindings),
    Classes = handle_classes(),
    lists:foldl(
        fun
            ({beamtalk_object, ClassTag, _Mod, _Pid} = ClassObj, Acc) ->
                ClassName = base_class_name(ClassTag),
                maps:put(ClassName, ClassObj, Acc);
            (_, Acc) ->
                Acc
        end,
        Base,
        Classes
    ).

%% @private Return non-class session bindings: singletons + user bind:as: entries.
-spec handle_session_bindings(pid(), atom() | undefined, map()) -> map().
handle_session_bindings(GsPid, ModuleName, UserBindings) ->
    %% Build WorkspaceSelf reference using the module name from Self
    WsMod =
        case ModuleName of
            undefined -> 'bt@stdlib@workspace_interface';
            M -> M
        end,
    WorkspaceSelf = {beamtalk_object, 'WorkspaceInterface', WsMod, GsPid},
    Base0 = UserBindings,
    Base1 =
        case resolve_singleton('TranscriptStream') of
            nil -> Base0;
            TranscriptObj -> maps:put('Transcript', TranscriptObj, Base0)
        end,
    Base2 =
        case resolve_singleton('BeamtalkInterface') of
            nil -> Base1;
            BeamtalkObj -> maps:put('Beamtalk', BeamtalkObj, Base1)
        end,
    maps:put('Workspace', WorkspaceSelf, Base2).

%% @private Convert a name argument to an atom.
-spec to_atom_name(term()) -> atom() | {error, #beamtalk_error{}}.
to_atom_name(Name) when is_atom(Name) -> Name;
to_atom_name(Other) ->
    TypeName = value_type_name(Other),
    Err0 = beamtalk_error:new(type_error, 'WorkspaceInterface'),
    {error,
        beamtalk_error:with_message(
            Err0,
            iolist_to_binary([<<"Expected a Symbol name, got ">>, TypeName])
        )}.

%% @private Check if a name conflicts with Beamtalk system globals.
-spec check_bind_conflicts(atom()) -> ok | {error, #beamtalk_error{}}.
check_bind_conflicts(AtomName) ->
    case is_protected_name(AtomName) of
        true ->
            Err0 = beamtalk_error:new(name_conflict, 'WorkspaceInterface'),
            Err1 = beamtalk_error:with_selector(Err0, 'bind:as:'),
            {error,
                beamtalk_error:with_message(
                    Err1,
                    iolist_to_binary([
                        atom_to_binary(AtomName, utf8),
                        <<" is a system name and cannot be shadowed">>
                    ])
                )};
        false ->
            ok
    end.

-spec is_protected_name(atom()) -> boolean().
is_protected_name('Transcript') -> true;
is_protected_name('Beamtalk') -> true;
is_protected_name('Workspace') -> true;
is_protected_name(_) -> false.

%% @private Warn if name is an existing loaded class.
-spec maybe_warn_loaded_class(atom()) -> ok.
maybe_warn_loaded_class(AtomName) ->
    Classes = handle_classes(),
    IsLoadedClass = lists:any(
        fun
            ({beamtalk_object, ClassTag, _Mod, _Pid}) ->
                base_class_name(ClassTag) =:= AtomName;
            (_) ->
                false
        end,
        Classes
    ),
    case IsLoadedClass of
        true ->
            WarningMsg = iolist_to_binary([
                <<"Warning: ">>,
                atom_to_binary(AtomName, utf8),
                <<" is a loaded class. Use reload instead.">>
            ]),
            ?LOG_WARNING("~s", [WarningMsg]);
        false ->
            ok
    end.

%% @private Read beamtalk_source attribute from a module.
-spec source_file_from_module(atom()) -> binary() | 'nil'.
source_file_from_module(ModuleName) ->
    try erlang:get_module_info(ModuleName, attributes) of
        Attrs ->
            case lists:keyfind(beamtalk_source, 1, Attrs) of
                {beamtalk_source, [Path]} when is_binary(Path) -> Path;
                {beamtalk_source, [Path]} when is_list(Path) -> list_to_binary(Path);
                _ -> nil
            end
    catch
        error:badarg -> nil
    end.

%% @private Wrap actor metadata into a beamtalk_object tuple.
-spec wrap_actor(beamtalk_repl_actors:actor_metadata()) -> {true, tuple()} | false.
wrap_actor(#{pid := Pid, class := Class, module := Module}) ->
    case is_process_alive(Pid) of
        true ->
            {true, {beamtalk_object, Class, Module, Pid}};
        false ->
            false
    end.

%% @private Resolve a singleton class instance.
-spec resolve_singleton(atom()) -> tuple() | 'nil'.
resolve_singleton(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            nil;
        ClassPid ->
            try
                beamtalk_class_dispatch:class_send(ClassPid, current, [])
            catch
                _:_ -> nil
            end
    end.

%% @private Extract the base class name from a class tag (e.g. 'Counter class' -> 'Counter').
-spec base_class_name(atom()) -> atom().
base_class_name(Tag) ->
    Bin = beamtalk_class_registry:class_display_name(Tag),
    try
        binary_to_existing_atom(Bin, utf8)
    catch
        error:badarg -> Tag
    end.

%% @private Return a human-readable type name for an Erlang/Beamtalk value.
-spec value_type_name(term()) -> binary().
value_type_name(V) when is_integer(V) -> <<"Integer">>;
value_type_name(V) when is_float(V) -> <<"Float">>;
value_type_name(V) when is_boolean(V) -> <<"Boolean">>;
value_type_name(nil) -> <<"nil">>;
value_type_name(V) when is_atom(V) -> <<"Symbol">>;
value_type_name(V) when is_list(V) -> <<"List">>;
value_type_name(V) when is_map(V) -> <<"Dictionary">>;
value_type_name({beamtalk_object, _, _, _}) -> <<"Object">>;
value_type_name(_) -> <<"Unknown">>.
