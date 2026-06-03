%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_registry).

%%% **DDD Context:** Object System Context

-moduledoc """
Class registry and hierarchy management for Beamtalk.

Provides stateless lookup and hierarchy functions for the class system.
These functions don't require gen_server state and are used across many
runtime modules (dispatch, primitive, stdlib, beamtalk_interface, REPL).

Extracted from `beamtalk_object_class` (BT-576) for single-responsibility.

## Responsibilities

- Class process lookup via Erlang registry
- Class enumeration via pg (process groups)
- Class hierarchy ETS table management
- Inheritance queries (inherits_from/2)
- Class object identity checks (is_class_object/1, is_class_name/1)
- Flattened method table invalidation broadcasts
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    whereis_class/1,
    all_classes/0,
    live_class_entries/0,
    user_classes/0,
    class_object_from_pid/1,
    registry_name/1,
    ensure_pg_started/0,
    ensure_hierarchy_table/0,
    ensure_module_table/0,
    ensure_methods_table/0,
    ensure_class_warnings_table/0,
    heir_option/0,
    maybe_set_heir/1,
    record_class_collision_warning/3,
    drain_class_warnings_by_names/1,
    drain_class_warnings_by_qualified_names/1,
    extract_package_from_module/1,
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
    get_class_method_return_type/2,
    restart_class/1,
    ensure_pid_table/0,
    ensure_pending_errors_table/0,
    record_class_pid/2,
    class_name_for_pid/1,
    ensure_loaded_classes_table/0,
    record_loaded_class/2,
    forget_loaded_class/2,
    loaded_class_names/0,
    loaded_class_entries/0
]).

-export_type([
    class_entry/0,
    user_class_entry/0
]).

-type class_name() :: atom().

%%====================================================================
%% Class Lookup
%%====================================================================

-doc "Look up a class by name.".
-spec whereis_class(class_name()) -> pid() | undefined.
whereis_class(ClassName) ->
    RegName = registry_name(ClassName),
    erlang:whereis(RegName).

-doc "Get all class processes.".
-spec all_classes() -> [pid()].
all_classes() ->
    pg:get_members(beamtalk_classes).

-doc """
Fetch all live class entries from the registry.

Returns a list of {Name, ModuleName, Pid} tuples for all registered class
processes that are still alive. Dead processes (noproc, timeout) are silently
filtered out. Returns [] if the pg process group is not running.
""".
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
            ?LOG_WARNING("pg not started when fetching class entries", #{
                module => ?MODULE, domain => [beamtalk, runtime]
            }),
            []
    end.

-doc """
Return all loaded user classes (those with a source file recorded).

Excludes stdlib and ClassBuilder-created classes (they have no source file).
Returns a list of {beamtalk_object, ClassTag, ModuleName, Pid} tuples for
all user-defined classes that are still alive. Dead processes are silently
filtered out. Returns [] if the pg process group is not running.
""".
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

-doc """
Build a canonical class object record from a class gen_server pid (BT-2258).

Mirrors the inline `{beamtalk_object, ClassTag, ModuleName, Pid}` construction
in user_classes/0. Used by the programmatic ClassBuilder `register` intrinsic
so its return value matches the shape produced by codegen class references and
beamtalk_interface:handle_class_named/1 (i.e. is_class_object/1 is true and it
is `==` to the registry reference).
""".
-spec class_object_from_pid(pid()) -> #beamtalk_object{}.
class_object_from_pid(Pid) when is_pid(Pid) ->
    ClassName = beamtalk_object_class:class_name(Pid),
    ModuleName = beamtalk_object_class:module_name(Pid),
    ClassTag = class_object_tag(ClassName),
    #beamtalk_object{class = ClassTag, class_mod = ModuleName, pid = Pid}.

-doc "Compute the Erlang registry name for a class.".
-spec registry_name(class_name()) -> atom().
registry_name(ClassName) ->
    % elp:fixme W0023 intentional atom creation
    list_to_atom("beamtalk_class_" ++ atom_to_list(ClassName)).

%%====================================================================
%% Process Group & ETS Bootstrap
%%====================================================================

-doc """
Ensure pg (process groups) is started.
pg is used for tracking all class processes.
""".
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

-doc """
Ensure the unified class metadata ETS table exists (BT-2222).

Delegates to `beamtalk_class_metadata:new/0`. The hierarchy, module, and
method-selector columns now share one row, so all three `ensure_*` functions
create the same table; the separate names are kept for the existing bootstrap
and test call sites. Originally three tables (BT-1062 hierarchy, BT-1285
module, BT-2008 methods).
""".
-spec ensure_hierarchy_table() -> ok.
ensure_hierarchy_table() ->
    beamtalk_class_metadata:new().

-doc """
Ensure the unified class metadata ETS table exists (BT-2222).

Alias for `ensure_hierarchy_table/0`; see its docs. Called from
`beamtalk_object_class:init/1` and `beamtalk_runtime_app:start/2`.
""".
-spec ensure_module_table() -> ok.
ensure_module_table() ->
    beamtalk_class_metadata:new().

-doc """
Ensure the unified class metadata ETS table exists (BT-2222).

Alias for `ensure_hierarchy_table/0`. Created (and heir-set, BT-1888) so the
chain walker in `beamtalk_class_dispatch:find_class_method_in_ancestors/3`
can resolve inherited class-method dispatch without any gen_server
round-trips.
""".
-spec ensure_methods_table() -> ok.
ensure_methods_table() ->
    beamtalk_class_metadata:new().

-doc """
Ensure the class collision warnings ETS table exists.
BT-737/BT-742: Stores collision warnings keyed by {Package, ClassName}.
Uses bag type so multiple warnings per class are all captured.
Uses try/catch to handle concurrent creation race (TOCTOU safe).
BT-742: Key changed from flat ClassName to {Package | undefined, ClassName}
so that draining warnings for one package doesn't affect another's.
BT-1888: Uses {heir, ...} to prevent table loss when owner dies.
""".
-spec ensure_class_warnings_table() -> ok.
ensure_class_warnings_table() ->
    case ets:info(beamtalk_class_warnings) of
        undefined ->
            try
                ets:new(
                    beamtalk_class_warnings,
                    [
                        bag,
                        public,
                        named_table,
                        {write_concurrency, true}
                        | heir_option()
                    ]
                ),
                ok
            catch
                error:badarg -> ok
            end;
        _ ->
            maybe_set_heir(beamtalk_class_warnings),
            ok
    end.

-doc """
Record a class collision warning keyed by {Package, ClassName}.
BT-737: Called by beamtalk_object_class when update_class detects module mismatch.
BT-742: Package is extracted from NewModule (the replacing module) so that
warnings can be drained per-package during selective reloads.
""".
-spec record_class_collision_warning(atom(), atom(), atom()) -> ok.
record_class_collision_warning(ClassName, OldModule, NewModule) ->
    ensure_class_warnings_table(),
    Package = extract_package_from_module(NewModule),
    ets:insert(beamtalk_class_warnings, {{Package, ClassName}, OldModule, NewModule}),
    ok.

-doc """
Drain collision warnings for the given class names after a file load.
BT-737: Called by the REPL load handler after loading a file, to collect
warnings and surface them to the client. Removes entries from the table.
BT-742: Drains ALL packages for each class name. Use
drain_class_warnings_by_qualified_names/1 for per-package precision.
""".
-spec drain_class_warnings_by_names([atom()]) -> [{atom(), atom(), atom()}].
drain_class_warnings_by_names(ClassNames) ->
    case ets:info(beamtalk_class_warnings) of
        undefined ->
            [];
        _ ->
            lists:flatmap(
                fun(ClassName) ->
                    %% BT-742: Key is now {Package, ClassName}. Use match_object
                    %% to drain all packages for this class name. Unlike the old
                    %% ets:take/2, this is not atomic — concurrent inserts between
                    %% match and delete may survive (which is correct behavior).
                    Pattern = {{'_', ClassName}, '_', '_'},
                    Matches = ets:match_object(beamtalk_class_warnings, Pattern),
                    lists:foreach(
                        fun(Entry) ->
                            ets:delete_object(beamtalk_class_warnings, Entry)
                        end,
                        Matches
                    ),
                    [{CN, OldMod, NewMod} || {{_Pkg, CN}, OldMod, NewMod} <- Matches]
                end,
                ClassNames
            )
    end.

-doc """
Drain collision warnings for the given {Package, ClassName} pairs.
BT-742: Package-aware drain that only removes warnings for the specified
package, leaving other packages' warnings intact. Use this from reload
handlers where the package context is known.
""".
-spec drain_class_warnings_by_qualified_names([{atom() | undefined, atom()}]) ->
    [{atom(), atom(), atom()}].
drain_class_warnings_by_qualified_names(QualifiedNames) ->
    case ets:info(beamtalk_class_warnings) of
        undefined ->
            [];
        _ ->
            lists:flatmap(
                fun({Package, ClassName}) ->
                    %% ets:take/2 atomically removes and returns entries (OTP 21+).
                    [
                        {CN, OldMod, NewMod}
                     || {{_Pkg, CN}, OldMod, NewMod} <-
                            ets:take(beamtalk_class_warnings, {Package, ClassName})
                    ]
                end,
                QualifiedNames
            )
    end.

-doc """
Extract the package segment from a bt@{pkg}@{class} module name.
Returns the package name atom or undefined for unqualified modules.
BT-742: Used to derive the package portion of the ETS key from module atoms.
""".
-spec extract_package_from_module(atom()) -> atom() | undefined.
extract_package_from_module(ModuleName) when is_atom(ModuleName) ->
    ModStr = atom_to_list(ModuleName),
    case string:split(ModStr, "@", all) of
        ["bt", Pkg, _Class | _Rest] when Pkg =/= [] ->
            % elp:fixme W0023 intentional atom creation
            list_to_atom(Pkg);
        _ ->
            undefined
    end.

-doc """
Record a structured error to be surfaced after a failed module load.
BT-738: Called by beamtalk_object_class when update_class detects stdlib shadowing.
The error is keyed by class name (atom) and drained by the REPL load handler
after code:load_binary returns {error, _} due to on_load failure.
""".
-spec record_pending_load_error(atom(), #beamtalk_error{}) -> ok.
record_pending_load_error(ClassName, Error) ->
    ensure_pending_errors_table(),
    ets:insert(beamtalk_pending_load_errors, {ClassName, Error}),
    ok.

-doc """
Drain pending load errors for the given class names.
BT-738: Called by the REPL load handler after a failed code:load_binary,
to retrieve any structured stdlib_shadowing errors for the attempted classes.
Removes entries from the table (atomically via ets:take/2).
""".
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

-doc """
Validate a class update for stdlib shadowing and cross-module redefinition.
BT-738: Rejects updates where a user module tries to shadow a stdlib class.
BT-737: Emits a warning when a class is redefined from a different module.
Returns ok if the update is permitted, {error, Error} if it must be rejected.
""".
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
                user_module => NewModule,
                domain => [beamtalk, runtime]
            }),
            record_pending_load_error(ClassName, Error1),
            {error, Error1};
        false ->
            case OldModule =:= NewModule of
                false ->
                    %% Bootstrap stubs (Class, Metaclass, ClassBuilder) are intentionally
                    %% replaced by compiled stdlib modules during startup. This is expected
                    %% behaviour, not a collision — suppress the warning.
                    case is_bootstrap_stub_module(OldModule) andalso is_stdlib_module(NewModule) of
                        true ->
                            ?LOG_DEBUG("Bootstrap stub replaced by stdlib module", #{
                                class => ClassName,
                                stub => OldModule,
                                stdlib => NewModule,
                                domain => [beamtalk, runtime]
                            });
                        false ->
                            ?LOG_WARNING("Class redefined from different module", #{
                                class => ClassName,
                                old_module => OldModule,
                                new_module => NewModule,
                                domain => [beamtalk, runtime]
                            }),
                            record_class_collision_warning(ClassName, OldModule, NewModule)
                    end;
                true ->
                    ok
            end,
            ok
    end.

-doc """
Returns true if the given module atom belongs to the Beamtalk stdlib.
BT-738: Stdlib modules have the prefix 'bt@stdlib@'.
""".
-spec is_stdlib_module(atom()) -> boolean().
is_stdlib_module(Module) when is_atom(Module) ->
    case atom_to_binary(Module, utf8) of
        <<"bt@stdlib@", _/binary>> -> true;
        _ -> false
    end;
is_stdlib_module(_) ->
    false.

-doc """
Returns true if the given module atom is a bootstrap stub.
Bootstrap stubs are hand-written modules that register placeholder classes
(Class, Metaclass, ClassBuilder) before the compiled stdlib loads.
They are intentionally replaced during stdlib initialization.
""".
-spec is_bootstrap_stub_module(atom()) -> boolean().
is_bootstrap_stub_module(beamtalk_class_bt) -> true;
is_bootstrap_stub_module(beamtalk_metaclass_bt) -> true;
is_bootstrap_stub_module(beamtalk_class_builder_bt) -> true;
is_bootstrap_stub_module(_) -> false.

-doc """
Ensure the pending load errors ETS table exists.
BT-1888: Uses {heir, ...} to prevent table loss when owner dies.
""".
-spec ensure_pending_errors_table() -> ok.
ensure_pending_errors_table() ->
    case ets:info(beamtalk_pending_load_errors) of
        undefined ->
            try
                ets:new(
                    beamtalk_pending_load_errors,
                    [
                        set,
                        public,
                        named_table,
                        {write_concurrency, true}
                        | heir_option()
                    ]
                ),
                ok
            catch
                error:badarg -> ok
            end;
        _ ->
            maybe_set_heir(beamtalk_pending_load_errors),
            ok
    end.

%%====================================================================
%% ETS Ownership Helpers (BT-1888)
%%====================================================================

-doc """
Return an ETS heir option for table survival across process crashes.

BT-1888: ETS tables created by a transient process (e.g. a class gen_server)
are destroyed when that process dies.  Using `{heir, Pid, Data}` hands the
table to a long-lived process instead of deleting it.

We pick the runtime supervisor (`beamtalk_runtime_sup`) if it is alive.
If the supervisor is not yet running (early bootstrap, tests), we omit
the heir option — the table will be owned by the caller and recreated
if needed via the existing ensure_* idempotent guards.
""".
-spec heir_option() -> [tuple()].
heir_option() ->
    case whereis(beamtalk_runtime_sup) of
        undefined -> [];
        Pid -> [{heir, Pid, undefined}]
    end.

-doc """
Retroactively set the heir on an existing table if one is not set.

BT-1888: Tables created early in boot (from `beamtalk_runtime_app:start/2`)
run before the supervisor is alive, so `heir_option/0` returns `[]`.
Subsequent `ensure_*` calls (e.g., from `beamtalk_object_class:init/1`)
invoke this to set the heir once the supervisor is available.
Only the table owner can call `ets:setopts/2`, so we guard on ownership.
""".
-spec maybe_set_heir(atom()) -> ok.
maybe_set_heir(Table) ->
    case whereis(beamtalk_runtime_sup) of
        undefined ->
            ok;
        SupPid ->
            case ets:info(Table, heir) of
                SupPid ->
                    %% Already set correctly.
                    ok;
                _ ->
                    %% Only the owner can change heir. If we're not the owner,
                    %% skip silently — the owner will set it on their next ensure call.
                    case ets:info(Table, owner) of
                        Owner when Owner =:= self() ->
                            try
                                ets:setopts(Table, {heir, SupPid, undefined})
                            catch
                                %% Heir PID died between whereis and setopts (TOCTOU).
                                error:badarg -> ok
                            end,
                            ok;
                        _ ->
                            ok
                    end
            end
    end.

%%====================================================================
%% Hierarchy Queries
%%====================================================================

-doc """
Check if a class inherits from a given ancestor (walks superclass chain).

BT-510: Uses ETS hierarchy table for O(1) lookups per level instead of
gen_server calls. No process messaging needed — pure ETS reads.

Returns true if ClassName is equal to or a subclass of Ancestor.
Returns false if the class is not in the hierarchy table (safe during bootstrap).
Used by beamtalk_exception_handler for hierarchy-aware matching (BT-475).
""".
-spec inherits_from(class_name() | none, class_name()) -> boolean().
inherits_from(none, _Ancestor) ->
    false;
inherits_from(ClassName, Ancestor) when ClassName =:= Ancestor ->
    true;
inherits_from(ClassName, Ancestor) ->
    case beamtalk_class_metadata:lookup_superclass(ClassName) of
        not_found -> false;
        {ok, none} -> false;
        {ok, SuperclassName} -> inherits_from(SuperclassName, Ancestor)
    end.

-doc """
Return sorted list of direct subclass names for a given class.

BT-573: Queries the ETS hierarchy table for all classes whose superclass
matches the given class name. Returns sorted atom list for deterministic output.
""".
-spec direct_subclasses(class_name()) -> [class_name()].
direct_subclasses(ClassName) ->
    lists:sort(beamtalk_class_metadata:match_subclasses(ClassName)).

-doc """
Return sorted list of all subclass names recursively.

BT-573: Walks the hierarchy tree depth-first, collecting all transitive
subclasses. Returns sorted atom list for deterministic output.
""".
-spec all_subclasses(class_name()) -> [class_name()].
all_subclasses(ClassName) ->
    lists:sort(all_subclasses_acc([ClassName], [])).

-doc "Accumulator helper for all_subclasses/1; walks hierarchy depth-first.".
-spec all_subclasses_acc([class_name()], [class_name()]) -> [class_name()].
all_subclasses_acc([], Acc) ->
    Acc;
all_subclasses_acc([Current | Rest], Acc) ->
    Children = direct_subclasses(Current),
    all_subclasses_acc(Children ++ Rest, Children ++ Acc).

%%====================================================================
%% Class Object Identity
%%====================================================================

-doc """
Convert a class name atom to a class object tag (BT-246).

Appends " class" to the atom, e.g. 'Point' -> 'Point class'.
Used by codegen to create class object records with the right tag
for is_class_object/1 detection.
""".
-spec class_object_tag(atom()) -> atom().
class_object_tag(ClassName) when is_atom(ClassName) ->
    % elp:fixme W0023 intentional atom creation
    list_to_atom(atom_to_list(ClassName) ++ " class").

-doc """
Check if a value is a class object (BT-246).

Class objects are beamtalk_object records whose class name ends with " class".
This distinguishes class objects from actor instances at runtime.
""".
-spec is_class_object(term()) -> boolean().
is_class_object(#beamtalk_object{class = Class}) when is_atom(Class) ->
    is_class_name(Class);
is_class_object(_) ->
    false.

-doc "Check if a class name (atom or binary) represents a class object (ends with \" class\").".
-spec is_class_name(atom() | binary()) -> boolean().
is_class_name(ClassName) when is_atom(ClassName) ->
    is_class_name(atom_to_binary(ClassName, utf8));
is_class_name(ClassName) when is_binary(ClassName) ->
    Size = byte_size(ClassName) - 6,
    Size >= 0 andalso binary:part(ClassName, Size, 6) =:= <<" class">>;
is_class_name(_) ->
    false.

-doc """
Strip " class" suffix from a class object name to get the display name.

Returns the base class name (e.g., `'Integer class'` → `<<"Integer">>`).
Returns the full name as binary if not a class name. Accepts an atom or a
binary tag — the binary form lets callers decode a tag without first interning
the full `'Foo class'` atom (only the base class atom needs to exist).
""".
-spec class_display_name(atom() | binary()) -> binary().
class_display_name(ClassName) when is_atom(ClassName) ->
    class_display_name(atom_to_binary(ClassName, utf8));
class_display_name(ClassName) when is_binary(ClassName) ->
    Size = byte_size(ClassName) - 6,
    case Size >= 0 andalso binary:part(ClassName, Size, 6) =:= <<" class">> of
        true -> binary:part(ClassName, 0, Size);
        false -> ClassName
    end.

%%====================================================================
%% Class Pid Reverse Index (BT-1768)
%%====================================================================

-doc """
Ensure the pid→classname reverse index table exists (idempotent).

BT-1768: This table maps `{Pid, ClassName}` so that when a class process
crashes and the Erlang registry removes its name, we can still recover the
class name from the stale pid for auto-restart.
BT-1888: Uses {heir, ...} to prevent table loss when owner dies.
""".
-spec ensure_pid_table() -> ok.
ensure_pid_table() ->
    case ets:info(beamtalk_class_pids) of
        undefined ->
            try
                ets:new(
                    beamtalk_class_pids,
                    [
                        set,
                        public,
                        named_table,
                        {read_concurrency, true}
                        | heir_option()
                    ]
                ),
                ok
            catch
                error:badarg -> ok
            end;
        _ ->
            maybe_set_heir(beamtalk_class_pids),
            ok
    end.

-doc """
Record a class pid→classname mapping.

Called from `beamtalk_object_class:init/1` after the class process starts.
""".
-spec record_class_pid(pid(), class_name()) -> ok.
record_class_pid(Pid, ClassName) ->
    ensure_pid_table(),
    ets:insert(beamtalk_class_pids, {Pid, ClassName}),
    ok.

-doc """
Look up the class name for a (possibly dead) pid.

Returns `{ok, ClassName}` or `not_found`. The entry persists after the
process dies, which is the whole point — we need this for crash recovery.
""".
-spec class_name_for_pid(pid()) -> {ok, class_name()} | not_found.
class_name_for_pid(Pid) ->
    case ets:info(beamtalk_class_pids) of
        undefined ->
            not_found;
        _ ->
            case ets:lookup(beamtalk_class_pids, Pid) of
                [{_, ClassName}] -> {ok, ClassName};
                [] -> not_found
            end
    end.

%%====================================================================
%% Loaded-Class Name Index (BT-2384)
%%====================================================================

-doc """
Ensure the loaded-class name index table exists (idempotent).

BT-2384: This `set` table holds one `{ClassName, Pid}` row per loaded class,
maintained by the class lifecycle: a row is inserted in
`beamtalk_object_class:init/1` (via `record_loaded_class/2`) and removed in its
`terminate/1` (via `forget_loaded_class/2`). It exists so that the ADR 0087
xref miss-policy can compute the loaded-class *set* with a single ETS read
instead of one `gen_server:call` per loaded class (`live_class_entries/0` asks
each class process for its name + module). The full triple is unnecessary for
the miss partition — it only needs names (for stale-drop) and pids (to gate the
typically-empty fallback set), both of which this row carries.

The table is `public` so the class process (any pid) can write its own row, and
heir-protected (BT-1888) so it survives the owner's death. A row keyed by name
keeps the membership idempotent across reload: a re-registering class simply
overwrites its own row with the new pid.
""".
-spec ensure_loaded_classes_table() -> ok.
ensure_loaded_classes_table() ->
    case ets:info(beamtalk_loaded_classes) of
        undefined ->
            try
                ets:new(
                    beamtalk_loaded_classes,
                    [
                        set,
                        public,
                        named_table,
                        {read_concurrency, true},
                        {write_concurrency, true}
                        | heir_option()
                    ]
                ),
                ok
            catch
                error:badarg -> ok
            end;
        _ ->
            maybe_set_heir(beamtalk_loaded_classes),
            ok
    end.

-doc """
Record a class as loaded in the fast loaded-class name index.

Called from `beamtalk_object_class:init/1`. Keyed by class name, so a reload
(new pid for the same name) overwrites the prior row rather than accumulating
duplicates — mirroring how the registry treats one name as one live class.
""".
-spec record_loaded_class(class_name(), pid()) -> ok.
record_loaded_class(ClassName, Pid) when is_atom(ClassName), is_pid(Pid) ->
    ensure_loaded_classes_table(),
    ets:insert(beamtalk_loaded_classes, {ClassName, Pid}),
    ok.

-doc """
Remove a class from the fast loaded-class name index.

Called from `beamtalk_object_class:terminate/1` on graceful shutdown. The
delete is guarded on the *pid* so that a stale row left by a crashed-then-
restarted class (whose new init already re-recorded a different pid) is not
clobbered by the dying old process: only a row still pointing at `Pid` is
removed. On a hard crash `terminate/1` does not run, so a dead row can linger;
`loaded_class_names/0` / `loaded_class_entries/0` filter those out with a cheap
local `is_process_alive/1` (no messaging), exactly as `live_class_entries/0`
drops dead pg members.
""".
-spec forget_loaded_class(class_name(), pid()) -> ok.
forget_loaded_class(ClassName, Pid) when is_atom(ClassName), is_pid(Pid) ->
    case ets:info(beamtalk_loaded_classes) of
        undefined ->
            ok;
        _ ->
            %% Only delete the row if it still names this pid — a reload may have
            %% already overwritten it with the replacement process's pid.
            _ = ets:select_delete(
                beamtalk_loaded_classes,
                [{{ClassName, Pid}, [], [true]}]
            ),
            ok
    end.

-doc """
Return the set of currently-loaded class names (BT-2384).

Reads the loaded-class index table and drops any row whose pid is no longer
alive (a crash that skipped `terminate/1`), so the result matches the live set
`live_class_entries/0` would report — but with a single ETS scan plus cheap
local `is_process_alive/1` checks rather than O(classes) `gen_server:call`s.
Returns an empty set if the table has not been created yet (early bootstrap).
""".
-spec loaded_class_names() -> sets:set(class_name()).
loaded_class_names() ->
    sets:from_list(
        [Name || {Name, _Pid} <- loaded_class_entries()], [{version, 2}]
    ).

-doc """
Return the live `{ClassName, Pid}` rows from the loaded-class index (BT-2384).

The pid-carrying counterpart to `loaded_class_names/0`: dead-pid rows are
filtered out with a local `is_process_alive/1` (no gen_server hop). The xref
miss-policy uses the names for stale-drop and the pids to interrogate only the
(typically empty) loaded-but-unindexed set.
""".
-spec loaded_class_entries() -> [{class_name(), pid()}].
loaded_class_entries() ->
    case ets:info(beamtalk_loaded_classes) of
        undefined ->
            [];
        _ ->
            [
                {Name, Pid}
             || {Name, Pid} <- ets:tab2list(beamtalk_loaded_classes),
                is_process_alive(Pid)
            ]
    end.

%%====================================================================
%% Class Process Recovery (BT-1768)
%%====================================================================

-doc """
Attempt to restart a crashed class process from compiled module state.

BT-1768: When a class gen_server crashes, the Erlang registry automatically
unregisters its name. This function reconstructs a minimal ClassInfo from:
  1. The ETS module table (ClassName → Module mapping, survives process death)
  2. The ETS hierarchy table (ClassName → Superclass mapping)
  3. The compiled module's `__beamtalk_meta/0` (static metadata baked into BEAM)

Hot-patched methods and class variable state are lost — only compiled state
is recovered. Logs a warning so the user knows recovery happened.

Returns `{ok, NewPid}` on success, `{error, Reason}` on failure.
""".
-spec restart_class(class_name()) -> {ok, pid()} | {error, term()}.
restart_class(ClassName) ->
    case beamtalk_class_metadata:lookup_module(ClassName) of
        not_found ->
            {error, {no_module_for_class, ClassName}};
        {ok, Module} ->
            Superclass =
                case beamtalk_class_metadata:lookup_superclass(ClassName) of
                    {ok, S} -> S;
                    not_found -> none
                end,
            %% Read static metadata from the compiled module.
            %% This is always available as long as the BEAM file is loaded.
            %% Ensure the module is loaded so function_exported/3 returns true.
            %% The BEAM file should still be on disk even after the class process crashed.
            code:ensure_loaded(Module),
            Meta =
                case erlang:function_exported(Module, '__beamtalk_meta', 0) of
                    true ->
                        try Module:'__beamtalk_meta'() of
                            M when is_map(M) -> M;
                            _ -> #{}
                        catch
                            _:_ -> #{}
                        end;
                    false ->
                        #{}
                end,
            ClassInfo = #{
                module => Module,
                superclass => Superclass,
                meta => Meta
            },
            %% Clean up stale pid entries for this class before restarting.
            %% On crash, terminate doesn't run, so old {DeadPid, ClassName} entries
            %% linger. Remove them so they don't accumulate on repeated crashes.
            _ =
                (try
                    ets:match_delete(beamtalk_class_pids, {'_', ClassName})
                catch
                    _:_ -> ok
                end),
            case beamtalk_object_class:start(ClassName, ClassInfo) of
                {ok, NewPid} ->
                    ?LOG_WARNING(
                        "Class process for '~p' crashed and was auto-restarted "
                        "(hot-patched methods and class variable state were lost)",
                        [ClassName],
                        #{
                            class => ClassName,
                            module => Module,
                            new_pid => NewPid,
                            domain => [beamtalk, runtime]
                        }
                    ),
                    {ok, NewPid};
                {error, {already_started, ExistingPid}} ->
                    %% Another caller already restarted this class (concurrent race).
                    %% Return the existing process — no need to log an error.
                    {ok, ExistingPid};
                {error, Reason} = Err ->
                    ?LOG_ERROR(
                        "Failed to restart crashed class process for '~p': ~p",
                        [ClassName, Reason],
                        #{
                            class => ClassName,
                            module => Module,
                            domain => [beamtalk, runtime]
                        }
                    ),
                    Err
            end
    end.

%%====================================================================
%% Return-Type Lookup (BT-1002 / ADR 0045)
%%====================================================================

-doc """
Look up the return type for an instance method, walking the superclass chain.

Returns `{ok, TypeAtom}` if the method has a Simple return-type annotation anywhere
in the hierarchy, or `{error, not_found}` if none is found.

Absence means "dynamic" — the REPL should not rely on a specific type.
Chain walking follows the same superclass order as method dispatch.
""".
-spec get_method_return_type(atom(), atom()) -> {ok, atom() | tuple()} | {error, not_found}.
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
                    case beamtalk_class_metadata:lookup_superclass(ClassName) of
                        not_found -> {error, not_found};
                        {ok, Super} -> get_method_return_type(Super, Selector)
                    end;
                not_found ->
                    %% Process exited during call — not in this class, walk to superclass.
                    case beamtalk_class_metadata:lookup_superclass(ClassName) of
                        not_found -> {error, not_found};
                        {ok, Super} -> get_method_return_type(Super, Selector)
                    end
            end
    end.

-doc """
Look up the return type for a class-side method, walking the superclass chain.

Same semantics as `get_method_return_type/2` but consults `class_method_return_types`.
""".
-spec get_class_method_return_type(atom(), atom()) -> {ok, atom() | tuple()} | {error, not_found}.
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
                    case beamtalk_class_metadata:lookup_superclass(ClassName) of
                        not_found -> {error, not_found};
                        {ok, Super} -> get_class_method_return_type(Super, Selector)
                    end;
                not_found ->
                    %% Process exited during call — not in this class, walk to superclass.
                    case beamtalk_class_metadata:lookup_superclass(ClassName) of
                        not_found -> {error, not_found};
                        {ok, Super} -> get_class_method_return_type(Super, Selector)
                    end
            end
    end.
