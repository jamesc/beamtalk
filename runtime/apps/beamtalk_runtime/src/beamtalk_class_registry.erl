%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Class registry and hierarchy management for Beamtalk.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Provides stateless lookup and hierarchy functions for the class system.
%%% These functions don't require gen_server state and are used across many
%%% runtime modules (dispatch, primitive, stdlib, beamtalk_interface, REPL).
%%%
%%% Extracted from `beamtalk_object_class` (BT-576) for single-responsibility.
%%%
%%% ## Responsibilities
%%%
%%% - Class process lookup via Erlang registry
%%% - Class enumeration via pg (process groups)
%%% - Class hierarchy ETS table management
%%% - Inheritance queries (inherits_from/2)
%%% - Class object identity checks (is_class_object/1, is_class_name/1)
%%% - Flattened method table invalidation broadcasts
-module(beamtalk_class_registry).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    whereis_class/1,
    all_classes/0,
    live_class_entries/0,
    user_classes/0,
    registry_name/1,
    ensure_pg_started/0,
    ensure_hierarchy_table/0,
    ensure_class_warnings_table/0,
    record_class_collision_warning/3,
    drain_class_warnings_by_names/1,
    record_pending_load_error/2,
    drain_pending_load_errors_by_names/1,
    validate_class_update/3,
    is_stdlib_module/1,
    inherits_from/2,
    direct_subclasses/1,
    all_subclasses/1,
    class_object_tag/1,
    is_class_object/1,
    is_class_name/1,
    class_display_name/1,
    get_method_return_type/2,
    get_class_method_return_type/2
]).

-export_type([
    class_entry/0,
    user_class_entry/0
]).

-type class_name() :: atom().

%%====================================================================
%% Class Lookup
%%====================================================================

%% @doc Look up a class by name.
-spec whereis_class(class_name()) -> pid() | undefined.
whereis_class(ClassName) ->
    RegName = registry_name(ClassName),
    erlang:whereis(RegName).

%% @doc Get all class processes.
-spec all_classes() -> [pid()].
all_classes() ->
    pg:get_members(beamtalk_classes).

%% @doc Fetch all live class entries from the registry.
%%
%% Returns a list of {Name, ModuleName, Pid} tuples for all registered class
%% processes that are still alive. Dead processes (noproc, timeout) are silently
%% filtered out. Returns [] if the pg process group is not running.
-type class_entry() :: {atom(), module(), pid()}.
-spec live_class_entries() -> [class_entry()].
live_class_entries() ->
    try
        ClassPids = all_classes(),
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

%% @doc Return all loaded user classes (those with a source file recorded).
%%
%% Excludes stdlib and ClassBuilder-created classes (they have no source file).
%% Returns a list of {beamtalk_object, ClassTag, ModuleName, Pid} tuples for
%% all user-defined classes that are still alive. Dead processes are silently
%% filtered out. Returns [] if the pg process group is not running.
-type user_class_entry() :: {beamtalk_object, atom(), module(), pid()}.
-spec user_classes() -> [user_class_entry()].
user_classes() ->
    try
        ClassPids = all_classes(),
        lists:filtermap(
            fun(Pid) ->
                try
                    ClassName = beamtalk_object_class:class_name(Pid),
                    ModuleName = beamtalk_object_class:module_name(Pid),
                    case beamtalk_reflection:source_file_from_module(ModuleName) of
                        nil ->
                            false;
                        _SourceFile ->
                            ClassTag = class_object_tag(ClassName),
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

%% @doc Compute the Erlang registry name for a class.
-spec registry_name(class_name()) -> atom().
registry_name(ClassName) ->
    list_to_atom("beamtalk_class_" ++ atom_to_list(ClassName)).

%%====================================================================
%% Process Group & ETS Bootstrap
%%====================================================================

%% @doc Ensure pg (process groups) is started.
%% pg is used for tracking all class processes.
-spec ensure_pg_started() -> ok.
ensure_pg_started() ->
    case whereis(pg) of
        undefined ->
            case pg:start_link() of
                {ok, _Pid} -> ok;
                {error, {already_started, _}} -> ok
            end;
        _Pid ->
            ok
    end.

%% @doc Ensure the class hierarchy ETS table exists.
%% Delegates to beamtalk_class_hierarchy_table (BT-1062).
-spec ensure_hierarchy_table() -> ok.
ensure_hierarchy_table() ->
    beamtalk_class_hierarchy_table:new().

%% @doc Ensure the class collision warnings ETS table exists.
%% BT-737: Stores collision warnings keyed by class name (atom).
%% Uses bag type so multiple warnings per class are all captured.
%% Uses try/catch to handle concurrent creation race (TOCTOU safe).
-spec ensure_class_warnings_table() -> ok.
ensure_class_warnings_table() ->
    case ets:info(beamtalk_class_warnings) of
        undefined ->
            try
                ets:new(
                    beamtalk_class_warnings,
                    [bag, public, named_table, {write_concurrency, true}]
                ),
                ok
            catch
                error:badarg -> ok
            end;
        _ ->
            ok
    end.

%% @doc Record a class collision warning keyed by class name.
%% BT-737: Called by beamtalk_object_class when update_class detects module mismatch.
%% Keyed by ClassName (not caller PID) so it can be drained by the load handler
%% regardless of which process the on_load function ran in.
-spec record_class_collision_warning(atom(), atom(), atom()) -> ok.
record_class_collision_warning(ClassName, OldModule, NewModule) ->
    ensure_class_warnings_table(),
    ets:insert(beamtalk_class_warnings, {ClassName, OldModule, NewModule}),
    ok.

%% @doc Drain collision warnings for the given class names after a file load.
%% BT-737: Called by the REPL load handler after loading a file, to collect
%% warnings and surface them to the client. Removes entries from the table.
-spec drain_class_warnings_by_names([atom()]) -> [{atom(), atom(), atom()}].
drain_class_warnings_by_names(ClassNames) ->
    case ets:info(beamtalk_class_warnings) of
        undefined ->
            [];
        _ ->
            lists:flatmap(
                fun(ClassName) ->
                    %% ets:take/2 atomically removes and returns entries (OTP 21+).
                    %% Avoids TOCTOU race between lookup and delete in concurrent loads.
                    [
                        {CN, OldMod, NewMod}
                     || {CN, OldMod, NewMod} <- ets:take(beamtalk_class_warnings, ClassName)
                    ]
                end,
                ClassNames
            )
    end.

%% @doc Record a structured error to be surfaced after a failed module load.
%% BT-738: Called by beamtalk_object_class when update_class detects stdlib shadowing.
%% The error is keyed by class name (atom) and drained by the REPL load handler
%% after code:load_binary returns {error, _} due to on_load failure.
-spec record_pending_load_error(atom(), #beamtalk_error{}) -> ok.
record_pending_load_error(ClassName, Error) ->
    ensure_pending_errors_table(),
    ets:insert(beamtalk_pending_load_errors, {ClassName, Error}),
    ok.

%% @doc Drain pending load errors for the given class names.
%% BT-738: Called by the REPL load handler after a failed code:load_binary,
%% to retrieve any structured stdlib_shadowing errors for the attempted classes.
%% Removes entries from the table (atomically via ets:take/2).
-spec drain_pending_load_errors_by_names([atom()]) -> [{atom(), #beamtalk_error{}}].
drain_pending_load_errors_by_names(ClassNames) ->
    case ets:info(beamtalk_pending_load_errors) of
        undefined ->
            [];
        _ ->
            lists:flatmap(
                fun(ClassName) ->
                    [{CN, Err} || {CN, Err} <- ets:take(beamtalk_pending_load_errors, ClassName)]
                end,
                ClassNames
            )
    end.

%% @doc Validate a class update for stdlib shadowing and cross-module redefinition.
%% BT-738: Rejects updates where a user module tries to shadow a stdlib class.
%% BT-737: Emits a warning when a class is redefined from a different module.
%% Returns ok if the update is permitted, {error, Error} if it must be rejected.
-spec validate_class_update(atom(), atom(), map()) -> ok | {error, #beamtalk_error{}}.
validate_class_update(ClassName, OldModule, ClassInfo) ->
    NewModule = maps:get(module, ClassInfo, OldModule),
    case is_stdlib_module(OldModule) andalso not is_stdlib_module(NewModule) of
        true ->
            Error0 = beamtalk_error:new(stdlib_shadowing, ClassName),
            Error1 = beamtalk_error:with_hint(
                Error0,
                <<"Choose a different class name. Stdlib class names are protected.">>
            ),
            ?LOG_WARNING("Rejected stdlib class shadowing attempt", #{
                class => ClassName,
                stdlib_module => OldModule,
                user_module => NewModule
            }),
            record_pending_load_error(ClassName, Error1),
            {error, Error1};
        false ->
            case OldModule =:= NewModule of
                false ->
                    ?LOG_WARNING("Class redefined from different module", #{
                        class => ClassName,
                        old_module => OldModule,
                        new_module => NewModule
                    }),
                    record_class_collision_warning(ClassName, OldModule, NewModule);
                true ->
                    ok
            end,
            ok
    end.

%% @doc Returns true if the given module atom belongs to the Beamtalk stdlib.
%% BT-738: Stdlib modules have the prefix 'bt@stdlib@'.
-spec is_stdlib_module(atom()) -> boolean().
is_stdlib_module(Module) when is_atom(Module) ->
    case atom_to_binary(Module, utf8) of
        <<"bt@stdlib@", _/binary>> -> true;
        _ -> false
    end;
is_stdlib_module(_) ->
    false.

%% @private Ensure the pending load errors ETS table exists.
-spec ensure_pending_errors_table() -> ok.
ensure_pending_errors_table() ->
    case ets:info(beamtalk_pending_load_errors) of
        undefined ->
            try
                ets:new(
                    beamtalk_pending_load_errors,
                    [set, public, named_table, {write_concurrency, true}]
                ),
                ok
            catch
                error:badarg -> ok
            end;
        _ ->
            ok
    end.

%%====================================================================
%% Hierarchy Queries
%%====================================================================

%% @doc Check if a class inherits from a given ancestor (walks superclass chain).
%%
%% BT-510: Uses ETS hierarchy table for O(1) lookups per level instead of
%% gen_server calls. No process messaging needed — pure ETS reads.
%%
%% Returns true if ClassName is equal to or a subclass of Ancestor.
%% Returns false if the class is not in the hierarchy table (safe during bootstrap).
%% Used by beamtalk_exception_handler for hierarchy-aware matching (BT-475).
-spec inherits_from(class_name() | none, class_name()) -> boolean().
inherits_from(none, _Ancestor) ->
    false;
inherits_from(ClassName, Ancestor) when ClassName =:= Ancestor ->
    true;
inherits_from(ClassName, Ancestor) ->
    case beamtalk_class_hierarchy_table:lookup(ClassName) of
        not_found -> false;
        {ok, none} -> false;
        {ok, SuperclassName} -> inherits_from(SuperclassName, Ancestor)
    end.

%% @doc Return sorted list of direct subclass names for a given class.
%%
%% BT-573: Queries the ETS hierarchy table for all classes whose superclass
%% matches the given class name. Returns sorted atom list for deterministic output.
-spec direct_subclasses(class_name()) -> [class_name()].
direct_subclasses(ClassName) ->
    lists:sort(beamtalk_class_hierarchy_table:match_subclasses(ClassName)).

%% @doc Return sorted list of all subclass names recursively.
%%
%% BT-573: Walks the hierarchy tree depth-first, collecting all transitive
%% subclasses. Returns sorted atom list for deterministic output.
-spec all_subclasses(class_name()) -> [class_name()].
all_subclasses(ClassName) ->
    lists:sort(all_subclasses_acc([ClassName], [])).

%% @doc Accumulator helper for all_subclasses/1; walks hierarchy depth-first.
-spec all_subclasses_acc([class_name()], [class_name()]) -> [class_name()].
all_subclasses_acc([], Acc) ->
    Acc;
all_subclasses_acc([Current | Rest], Acc) ->
    Children = direct_subclasses(Current),
    all_subclasses_acc(Children ++ Rest, Children ++ Acc).

%%====================================================================
%% Class Object Identity
%%====================================================================

%% @doc Convert a class name atom to a class object tag (BT-246).
%%
%% Appends " class" to the atom, e.g. 'Point' -> 'Point class'.
%% Used by codegen to create class object records with the right tag
%% for is_class_object/1 detection.
-spec class_object_tag(atom()) -> atom().
class_object_tag(ClassName) when is_atom(ClassName) ->
    list_to_atom(atom_to_list(ClassName) ++ " class").

%% @doc Check if a value is a class object (BT-246).
%%
%% Class objects are beamtalk_object records whose class name ends with " class".
%% This distinguishes class objects from actor instances at runtime.
-spec is_class_object(term()) -> boolean().
is_class_object({beamtalk_object, Class, _Mod, _Pid}) when is_atom(Class) ->
    is_class_name(Class);
is_class_object(_) ->
    false.

%% @doc Check if an atom class name represents a class object (ends with " class").
-spec is_class_name(atom()) -> boolean().
is_class_name(ClassName) when is_atom(ClassName) ->
    ClassBin = atom_to_binary(ClassName, utf8),
    Size = byte_size(ClassBin) - 6,
    Size >= 0 andalso binary:part(ClassBin, Size, 6) =:= <<" class">>;
is_class_name(_) ->
    false.

%% @doc Strip " class" suffix from a class object name to get the display name.
%%
%% Returns the base class name (e.g., `'Integer class'` → `<<"Integer">>`).
%% Returns the full name as binary if not a class name.
-spec class_display_name(atom()) -> binary().
class_display_name(ClassName) when is_atom(ClassName) ->
    ClassBin = atom_to_binary(ClassName, utf8),
    Size = byte_size(ClassBin) - 6,
    case Size >= 0 andalso binary:part(ClassBin, Size, 6) =:= <<" class">> of
        true -> binary:part(ClassBin, 0, Size);
        false -> ClassBin
    end.

%%====================================================================
%% Return-Type Lookup (BT-1002 / ADR 0045)
%%====================================================================

%% @doc Look up the return type for an instance method, walking the superclass chain.
%%
%% Returns `{ok, TypeAtom}` if the method has a Simple return-type annotation anywhere
%% in the hierarchy, or `{error, not_found}` if none is found.
%%
%% Absence means "dynamic" — the REPL should not rely on a specific type.
%% Chain walking follows the same superclass order as method dispatch.
-spec get_method_return_type(atom(), atom()) -> {ok, atom()} | {error, not_found}.
get_method_return_type(none, _Selector) ->
    {error, not_found};
get_method_return_type(ClassName, Selector) ->
    case whereis_class(ClassName) of
        undefined ->
            {error, not_found};
        Pid ->
            Result =
                try
                    gen_server:call(Pid, {get_method_return_type, Selector}, 5000)
                catch
                    exit:{timeout, _} -> not_found;
                    exit:{noproc, _} -> not_found;
                    exit:{normal, _} -> not_found;
                    exit:{shutdown, _} -> not_found;
                    exit:{{shutdown, _}, _} -> not_found
                end,
            case Result of
                {ok, _} = Found ->
                    Found;
                {error, not_found} ->
                    %% Walk to superclass using the hierarchy table.
                    case beamtalk_class_hierarchy_table:lookup(ClassName) of
                        not_found -> {error, not_found};
                        {ok, Super} -> get_method_return_type(Super, Selector)
                    end;
                not_found ->
                    %% Process exited during call — not in this class, walk to superclass.
                    case beamtalk_class_hierarchy_table:lookup(ClassName) of
                        not_found -> {error, not_found};
                        {ok, Super} -> get_method_return_type(Super, Selector)
                    end
            end
    end.

%% @doc Look up the return type for a class-side method, walking the superclass chain.
%%
%% Same semantics as `get_method_return_type/2` but consults `class_method_return_types`.
-spec get_class_method_return_type(atom(), atom()) -> {ok, atom()} | {error, not_found}.
get_class_method_return_type(none, _Selector) ->
    {error, not_found};
get_class_method_return_type(ClassName, Selector) ->
    case whereis_class(ClassName) of
        undefined ->
            {error, not_found};
        Pid ->
            Result =
                try
                    gen_server:call(Pid, {get_class_method_return_type, Selector}, 5000)
                catch
                    exit:{timeout, _} -> not_found;
                    exit:{noproc, _} -> not_found;
                    exit:{normal, _} -> not_found;
                    exit:{shutdown, _} -> not_found;
                    exit:{{shutdown, _}, _} -> not_found
                end,
            case Result of
                {ok, _} = Found ->
                    Found;
                {error, not_found} ->
                    case beamtalk_class_hierarchy_table:lookup(ClassName) of
                        not_found -> {error, not_found};
                        {ok, Super} -> get_class_method_return_type(Super, Selector)
                    end;
                not_found ->
                    %% Process exited during call — not in this class, walk to superclass.
                    case beamtalk_class_hierarchy_table:lookup(ClassName) of
                        not_found -> {error, not_found};
                        {ok, Super} -> get_class_method_return_type(Super, Selector)
                    end
            end
    end.
