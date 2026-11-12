%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_actor_registry).

%%% **DDD Context:** Actor System Context

-moduledoc """
Named actor registration and cluster-unique names (ADR 0079, ADR 0126 §4).

Split out of `beamtalk_actor` (BT-4701). Owns:

- the local registry intrinsics (`register_name/2`, `unregister_name/1`,
  `whereis_name/1`, `all_registered/0`, `is_beamtalk_actor/1`) that wire
  Beamtalk actors into OTP's `erlang:register/2`;
- the named spawns (`spawnAs/2,3`, `spawnAsGlobal/2,3`, `spawn_named_for_caller/4`,
  `spawn_named_unlinked/3`) and the reserved-name policy (`reserved_name/1`);
- the pid → class / registered-name lookups (`pid_class_name/1`,
  `pid_class_candidates/1`, `registered_name_for_pid/1`);
- the `global` partition-heal conflict resolver (ADR 0126 §4,
  `resolve_global_conflict/3`).

Not to be confused with the `beamtalk_actor_registry` *process* (the
node-wide REPL actor tracker started by `beamtalk_repl_actors`): this module
shares the name but not the concern.

## Wire / persisted-MFA compatibility

`beamtalk_actor` keeps one-line delegates for `spawnAs/2,3`,
`spawnAsGlobal/2,3`, `resolve_global_conflict/3`, `started_at_from_dictionary/1`,
`pid_class_candidates/1` and `pid_class_name/1`. The `erpc:call/5`s this module
makes to a peer node name `beamtalk_actor` (`?BT_ACTOR_WIRE_MODULE`), as does
the resolve function registered with `global` (a registration made before an
upgrade still holds the old MFA), so a mixed-version cluster (ADR 0125) keeps
working in both directions. Supervisor child MFAs recorded by an earlier
release may also still name `spawnAs`. The delegates stay for at least one
release.
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    all_registered/0,
    is_beamtalk_actor/1,
    pid_class_candidates/1,
    pid_class_candidates_remote_safe/1,
    pid_class_name/1,
    register_name/2,
    registered_name_for_pid/1,
    reserved_name/1,
    resolve_global_conflict/3,
    'spawnAs'/2,
    'spawnAs'/3,
    'spawnAsGlobal'/2,
    'spawnAsGlobal'/3,
    spawn_named_for_caller/4,
    spawn_named_unlinked/3,
    started_at_from_dictionary/1,
    unregister_name/1,
    whereis_name/1
]).

-doc """
Check whether a pid is a Beamtalk actor process.

Returns `true` if the process has the `'$beamtalk_actor'` marker
set in its process dictionary (set by `init/1` for every Beamtalk
actor). Returns `false` for raw OTP processes (including kernel
processes, gen_servers not built on beamtalk_actor, etc.) and for
dead processes.
""".
-spec is_beamtalk_actor(term()) -> boolean().
is_beamtalk_actor(Pid) when is_pid(Pid) ->
    case erlang:process_info(Pid, dictionary) of
        {dictionary, Dict} when is_list(Dict) ->
            lists:keymember('$beamtalk_actor', 1, Dict);
        undefined ->
            %% Process is dead
            false
    end;
is_beamtalk_actor(_) ->
    false.

-doc """
Register a pid under a name in the local atom registry.

Returns `{ok, Name}` on success or `{error, #beamtalk_error{}}` on
failure with `kind` one of:
  - `type_error` — `Name` is not an atom or `Pid` is not a pid, or
    the `Pid` is not registerable (dead, remote, or already has a
    registered name).
  - `reserved_name` — `Name` is in the reserved-name blocklist
  - `name_registered` — another process is already registered
    under `Name`.

Note: `erlang:register/2` raises `badarg` for multiple reasons —
duplicate name, dead pid, remote pid, pid already registered
under another name. We disambiguate by checking
`erlang:whereis(Name)` after `badarg`: if taken, it's a duplicate
name; otherwise the Pid itself is unregisterable.
""".
-spec register_name(term(), term()) -> {ok, atom()} | {error, #beamtalk_error{}}.
register_name(Name, Pid) when is_atom(Name), is_pid(Pid) ->
    case reserved_name(Name) of
        true ->
            {error,
                beamtalk_error:with_hint(
                    beamtalk_error:new(reserved_name, 'Actor', registerAs),
                    beamtalk_error:chars_to_binary(
                        io_lib:format(
                            "Cannot register actor under reserved name '~ts'", [Name]
                        )
                    )
                )};
        false ->
            try erlang:register(Name, Pid) of
                true -> {ok, Name}
            catch
                error:badarg ->
                    case erlang:whereis(Name) =/= undefined of
                        true ->
                            {error,
                                beamtalk_error:with_hint(
                                    beamtalk_error:new(
                                        name_registered, 'Actor', registerAs
                                    ),
                                    beamtalk_error:chars_to_binary(
                                        io_lib:format(
                                            "Name '~ts' is already registered", [Name]
                                        )
                                    )
                                )};
                        false ->
                            {error,
                                beamtalk_error:with_hint(
                                    beamtalk_error:new(
                                        type_error, 'Actor', registerAs
                                    ),
                                    beamtalk_error:chars_to_binary(
                                        io_lib:format(
                                            "Pid ~tp is not registerable "
                                            "(dead, remote, or already "
                                            "registered under another name)",
                                            [Pid]
                                        )
                                    )
                                )}
                    end
            end
    end;
register_name(Name, Pid) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor', registerAs),
            beamtalk_error:chars_to_binary(
                io_lib:format(
                    "register_name/2 expects (atom, pid), got (~tp, ~tp)", [Name, Pid]
                )
            )
        )}.

-doc """
Unregister a name previously registered via `register_name/2`.

Returns `ok` on success or `{error, #beamtalk_error{}}` if `Name`
is not currently registered. Idempotent-friendly callers should
check `whereis_name/1` first if they want "unregister-if-present"
semantics.
""".
-spec unregister_name(term()) -> ok | {error, #beamtalk_error{}}.
unregister_name(Name) when is_atom(Name) ->
    case reserved_name(Name) of
        true ->
            {error,
                beamtalk_error:with_hint(
                    beamtalk_error:new(reserved_name, 'Actor', unregister),
                    beamtalk_error:chars_to_binary(
                        io_lib:format(
                            "Cannot unregister reserved name '~ts'", [Name]
                        )
                    )
                )};
        false ->
            try erlang:unregister(Name) of
                true -> ok
            catch
                error:badarg ->
                    {error,
                        beamtalk_error:with_hint(
                            beamtalk_error:new(
                                name_registered, 'Actor', unregister
                            ),
                            beamtalk_error:chars_to_binary(
                                io_lib:format(
                                    "Name '~ts' is not registered", [Name]
                                )
                            )
                        )}
            end
    end;
unregister_name(Name) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor', unregister),
            beamtalk_error:chars_to_binary(
                io_lib:format(
                    "unregister_name/1 expects atom, got ~tp", [Name]
                )
            )
        )}.

-doc """
Look up the pid registered under `Name`.

Returns `{ok, Pid}` if a process is registered, or `undefined` if
no process is registered under that name. Non-atom input returns
`{error, #beamtalk_error{kind = type_error}}`.
""".
-spec whereis_name(term()) -> {ok, pid()} | undefined | {error, #beamtalk_error{}}.
whereis_name(Name) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined -> undefined;
        Pid when is_pid(Pid) -> {ok, Pid}
    end;
whereis_name(Name) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor', whereisName),
            beamtalk_error:chars_to_binary(
                io_lib:format(
                    "whereis_name/1 expects atom, got ~tp", [Name]
                )
            )
        )}.

-doc """
List all names currently registered to Beamtalk actors.

Filters `erlang:registered/0` down to processes that carry the
`'$beamtalk_actor'` process-dictionary marker, so kernel processes,
gen_servers outside the Beamtalk runtime, and other raw OTP
processes are excluded.
""".
-spec all_registered() -> [atom()].
all_registered() ->
    [
        Name
     || Name <- erlang:registered(),
        case erlang:whereis(Name) of
            undefined -> false;
            Pid -> is_beamtalk_actor(Pid)
        end
    ].

-doc """
Spawn an actor under a registered name (arity 2).

Equivalent to `start_link({local, Name}, Module, [])`. Returns
`{ok, Pid}` on success or `{error, #beamtalk_error{}}` on
name-related failures. Other errors (crash in init, etc.) pass
through from `gen_server:start_link/4` as `{error, Reason}`.
""".
-spec 'spawnAs'(term(), term()) -> {ok, pid()} | {error, term()}.
'spawnAs'(Name, Module) ->
    %% Beamtalk `init/1` expects a state map (possibly empty) — matching the
    %% shape used everywhere else in the runtime (`doSpawnAs/2` passes `#{}`,
    %% the `spawn`/`spawnWith:` callers pass state maps). Defaulting to the
    %% empty list here would crash supervised named starts in `init/1` with
    %% `{badmap, []}`. Keep the default aligned with the rest of the runtime.
    'spawnAs'(Name, Module, #{}).

-doc """
Spawn an actor under a registered name (arity 3).

Delegates to `spawn_named_scoped/4` with `local` scope — see that
function's doc for the shared reserved-name/badarg handling. On
`{already_started, _}` or `badarg` from the registry, returns a
structured `#beamtalk_error{}` with kind `name_registered` so the stdlib
boundary can translate to `Result error: ...`.
""".
-spec 'spawnAs'(term(), term(), term()) -> {ok, pid()} | {error, term()}.
'spawnAs'(Name, Module, Args) when is_atom(Name), is_atom(Module) ->
    spawn_named_scoped(local, Name, Module, Args);
'spawnAs'(Name, Module, Args) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor', spawnAs),
            beamtalk_error:chars_to_binary(
                io_lib:format(
                    "spawnAs/3 expects (atom, module, term), got (~tp, ~tp, ~tp)",
                    [Name, Module, Args]
                )
            )
        )}.

-doc """
Spawn an actor cluster-globally registered under a name (arity 2, ADR 0126
§4, Phase 5, BT-3603). `'spawnAsGlobal'/3`'s default-args sibling — see
that function's doc.
""".
-spec 'spawnAsGlobal'(term(), term()) -> {ok, pid()} | {error, term()}.
'spawnAsGlobal'(Name, Module) ->
    'spawnAsGlobal'(Name, Module, #{}).

-doc """
Spawn an actor under a cluster-globally registered name (arity 3, ADR 0126
§4, Phase 5, BT-3603) — `'spawnAs'/3`'s `scope: #global` sibling, backed by
OTP `global` (`safe_spawn_named(global, ...)`) instead of the local process
registry. Same reserved-name blocklist, same `#beamtalk_error{}` shapes —
`spawn_named_scoped/4` is the single shared implementation behind both this
and `'spawnAs'/3` (AGENTS.md "No duplicate implementations").
""".
-spec 'spawnAsGlobal'(term(), term(), term()) -> {ok, pid()} | {error, term()}.
'spawnAsGlobal'(Name, Module, Args) when is_atom(Name), is_atom(Module) ->
    spawn_named_scoped(global, Name, Module, Args);
'spawnAsGlobal'(Name, Module, Args) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor', 'spawnAs:scope:'),
            beamtalk_error:chars_to_binary(
                io_lib:format(
                    "spawnAsGlobal/3 expects (atom, module, term), got (~tp, ~tp, ~tp)",
                    [Name, Module, Args]
                )
            )
        )}.

-doc """
Shared reserved-name check + `safe_spawn_named/5` dispatch behind
`'spawnAs'/3` (`Scope = local`) and `'spawnAsGlobal'/3` (`Scope = global`) —
the two differ only in which registry they land in (ADR 0126 §4: "`global`
names go through the SAME `reserved_name/1` check as local names"). On
`{already_started, _}` (both scopes — `global`'s own `safe_spawn_named/5`
clause translates a lost `global:register_name/3` race to this exact shape,
see its doc) or `badarg` (the `local` scope's own
`gen_server:start_link({local, Name}, ...)` failure mode) this returns the
structured `name_registered` error so the stdlib boundary can translate to
`Result error: ...`.
""".
-spec spawn_named_scoped(local | global, atom(), module(), term()) ->
    {ok, pid()} | {error, term()}.
spawn_named_scoped(Scope, Name, Module, Args) ->
    spawn_named_scoped(Scope, linked, Name, Module, Args).

-doc """
`spawn_named_scoped/4` with an explicit link mode (BT-3992): a caller that
must not stay linked to the named actor passes `unlinked` (or
caller_link_mode/0) instead of unlinking after the spawn returned — see
start_link_and_await/3.
""".
-spec spawn_named_scoped(local | global, beamtalk_actor:link_mode(), atom(), module(), term()) ->
    {ok, pid()} | {error, term()}.
spawn_named_scoped(Scope, Link, Name, Module, Args) ->
    case reserved_name(Name) of
        true ->
            {error,
                beamtalk_error:with_hint(
                    beamtalk_error:new(reserved_name, 'Actor', spawnAs),
                    beamtalk_error:chars_to_binary(
                        io_lib:format(
                            "Cannot spawn actor under reserved name '~ts'", [Name]
                        )
                    )
                )};
        false ->
            try beamtalk_actor:safe_spawn_named(Scope, Link, Name, Module, Args) of
                {ok, Pid} ->
                    {ok, Pid};
                {error, {already_started, _Other}} ->
                    name_registered_error(Name);
                {error, Reason} ->
                    {error, Reason}
            catch
                %% gen_server:start_link can raise badarg from the local
                %% registry (e.g., name already taken at the moment of
                %% registration). Translate to the structured error.
                error:badarg ->
                    name_registered_error(Name)
            end
    end.

-doc """
BT-3992: spawn a named actor (`'spawnAs'/3` for `local`, `'spawnAsGlobal'/3`
for `global`) for a caller that must not stay linked to it — a class
method's `self spawnAs:` running in the class process, or a remote spawn's
erpc worker. The link is removed inside the start (start_link_and_await/3),
before the caller stops trapping exits; unlinking after this returned would
leave a window in which a continuation requested by `initialize` that stops
the actor kills the caller.

`withClassMethod:` supervisor factories are the exception: when
`?BT_SUPERVISOR_SPAWN_CONTEXT_KEY` is set the caller really is the OTP
supervisor and the actor stays linked (see `safe_spawn/2`'s doc).
""".
-spec spawn_named_for_caller(local | global, term(), term(), term()) ->
    {ok, pid()} | {error, term()}.
spawn_named_for_caller(Scope, Name, Module, Args) when is_atom(Name), is_atom(Module) ->
    Link =
        case get(?BT_SUPERVISOR_SPAWN_CONTEXT_KEY) of
            true -> linked;
            _ -> unlinked
        end,
    spawn_named_scoped(Scope, Link, Name, Module, Args);
spawn_named_for_caller(local, Name, Module, Args) ->
    'spawnAs'(Name, Module, Args);
spawn_named_for_caller(global, Name, Module, Args) ->
    'spawnAsGlobal'(Name, Module, Args).

-doc """
BT-3992: `spawn_named_for_caller/4` without the supervisor exception — the
actor is never left linked to the caller (remote spawns' erpc workers).
""".
-spec spawn_named_unlinked(term(), term(), term()) -> {ok, pid()} | {error, term()}.
spawn_named_unlinked(Name, Module, Args) when is_atom(Name), is_atom(Module) ->
    spawn_named_scoped(local, unlinked, Name, Module, Args);
spawn_named_unlinked(Name, Module, Args) ->
    'spawnAs'(Name, Module, Args).

%% The registered name of a pid, or the Beamtalk `nil` atom when unregistered
%% or dead. Canonical implementation: previously duplicated (with a
%% disagreeing `undefined` sentinel in this module and a `nil` sentinel in
%% beamtalk_process_navigation.erl's now-removed `registered_name/1`).
%% Beamtalk-facing surfaces speak Beamtalk's null concept, not raw Erlang
%% `undefined`, so `nil` is the one true sentinel; callers that need Erlang's
%% `undefined` for an internal `=/=`/`case` guard pattern-match on `nil` now
%% instead.
-spec registered_name_for_pid(pid()) -> atom() | nil.
registered_name_for_pid(Pid) when is_pid(Pid) ->
    case erlang:process_info(Pid, registered_name) of
        {registered_name, Name} when is_atom(Name) -> Name;
        _ -> nil
    end.

%% The Beamtalk class name for an actor pid, read from the `'$beamtalk_actor'`
%% process-dictionary marker planted by every actor's `init/1`. Canonical
%% implementation: previously duplicated as `pid_class_name/1` here
%% (sentinel `undefined`), `beamtalk_inspector:actor_class/1` and
%% `beamtalk_process_navigation:actor_class_name/1` (both sentinel `nil`).
%% `nil` wins as the single sentinel — see `registered_name_for_pid/1` above.
%%
%% A `native:` actor's backing gen_server (ADR 0056) has no marker; its class
%% comes from the `'$initial_call'` fallback (`native_pid_class_candidates/1`,
%% BT-4657), so native actors also get a class label in the inspector and are
%% resolvable via `named:` / `allRegistered`. (Process navigation still
%% classifies via the marker-only `is_beamtalk_actor/1`.)
%%
%% Local-pid only: `erlang:process_info/2` raises `badarg` for a pid on
%% another node. `pid_class_candidates_remote_safe/1` below is the node-aware
%% wrapper for a caller (like `named_lookup/4`'s `global` scope) whose pid
%% may not be local.
-spec pid_class_name(pid()) -> atom() | nil.
pid_class_name(Pid) ->
    case pid_class_candidates(Pid) of
        [Class] -> Class;
        %% No class (non-Beamtalk process) or an ambiguous shared backing
        %% module (BT-4664): no single class to name.
        _ -> nil
    end.

-doc """
The live classes a pid could be an instance of (BT-4664): `[Class]` for a
compiled actor (marker) or a `native:` actor with an unshared backing module,
several classes for a `native:` actor whose backing module is shared, and `[]`
for a non-Beamtalk process. Local-pid only; see
`pid_class_candidates_remote_safe/1`.
""".
-spec pid_class_candidates(pid()) -> [atom()].
pid_class_candidates(Pid) ->
    case erlang:process_info(Pid, dictionary) of
        {dictionary, Dict} when is_list(Dict) ->
            case lists:keyfind('$beamtalk_actor', 1, Dict) of
                {'$beamtalk_actor', Class} when is_atom(Class) -> [Class];
                _ -> native_pid_class_candidates(Dict)
            end;
        _ ->
            []
    end.

%% BT-4657: a `native:` actor's hand-written backing gen_server (ADR 0056)
%% never plants the `'$beamtalk_actor'` marker. Its classes are read from the
%% process's `'$initial_call'` (`{BackingModule, init, 1}`, set by
%% `proc_lib`) through the backing-module index. A backing module shared by
%% several classes yields all of them (BT-4664).
-spec native_pid_class_candidates(list()) -> [atom()].
native_pid_class_candidates(Dict) ->
    case lists:keyfind('$initial_call', 1, Dict) of
        {'$initial_call', {Backing, init, 1}} when is_atom(Backing) ->
            %% The index can hold a stale row for a class whose process died
            %% (`forget_backing_module_entries/2`), so keep live classes only.
            lists:usort([
                C
             || C <- beamtalk_class_registry:classes_backing_module(Backing),
                beamtalk_class_registry:whereis_class(C) =/= undefined
            ]);
        _ ->
            []
    end.

-doc """
Node-aware wrapper over `pid_class_candidates/1` (ADR 0126 §4, BT-3603):
`global:whereis_name/1` can resolve to a pid on any connected node, and
`erlang:process_info/2` raises `badarg` for a remote pid, so a remote pid is
read via `erpc:call/5`. An unreachable node or dead remote process resolves
to `[]`, the same as a non-Beamtalk process.
""".
-spec pid_class_candidates_remote_safe(pid()) -> [atom()].
pid_class_candidates_remote_safe(Pid) when node(Pid) =:= node() ->
    pid_class_candidates(Pid);
pid_class_candidates_remote_safe(Pid) ->
    try
        erpc:call(
            node(Pid), ?BT_ACTOR_WIRE_MODULE, pid_class_candidates, [Pid], ?BT_REMOTE_CALL_TIMEOUT
        )
    of
        Result -> Result
    catch
        %% A node that predates `pid_class_candidates/1` (rolling upgrade,
        %% ADR 0125): fall back to the single-class answer it does have.
        error:{exception, undef, _} -> remote_single_class_candidates(Pid);
        error:{erpc, _ErpcReason} -> [];
        error:{exception, _Reason, _Stack} -> [];
        exit:{exception, _Reason} -> []
    end.

-spec remote_single_class_candidates(pid()) -> [atom()].
remote_single_class_candidates(Pid) ->
    try
        erpc:call(node(Pid), ?BT_ACTOR_WIRE_MODULE, pid_class_name, [Pid], ?BT_REMOTE_CALL_TIMEOUT)
    of
        Class when is_atom(Class), Class =/= nil, Class =/= undefined -> [Class];
        _ -> []
    catch
        error:{erpc, _ErpcReason} -> [];
        error:{exception, _Reason, _Stack} -> [];
        exit:{exception, _Reason} -> []
    end.

-spec name_registered_error(atom()) -> {error, #beamtalk_error{}}.
name_registered_error(Name) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(name_registered, 'Actor', spawnAs),
            beamtalk_error:chars_to_binary(
                io_lib:format(
                    "Name '~ts' is already registered", [Name]
                )
            )
        )}.

-doc """
Check whether `Name` is on the reserved-name blocklist.

Reserved names are:
  - OTP kernel / stdlib registered process names that a user atom
    collision with would be catastrophic (application_controller,
    code_server, erl_prim_loader, erl_signal_server, error_logger,
    erts_code_purger, file_server_2, global_group, global_group_check,
    global_name_server, inet_db, init, kernel_refc, kernel_safe_sup,
    kernel_sup, logger, logger_handler_watcher, logger_proxy,
    logger_std_h_default, logger_sup, net_kernel, net_sup, rex,
    socket_registry, standard_error, standard_error_sup,
    standard_error_writer, user, user_drv, user_drv_reader,
    user_drv_writer).
  - Any atom whose textual form is prefixed `beamtalk_` — reserved
    for internal runtime / supervision use.

See ADR 0079 ("Reserved-name policy") for rationale.
""".
-spec reserved_name(atom()) -> boolean().
reserved_name(Name) when is_atom(Name) ->
    case is_kernel_reserved(Name) of
        true ->
            true;
        false ->
            NameStr = atom_to_list(Name),
            lists:prefix("beamtalk_", NameStr)
    end;
reserved_name(_) ->
    false.

%% Static blocklist of OTP kernel / stdlib registered names.
%% Kept as a function (not a macro) so dialyzer sees the pattern match.
-spec is_kernel_reserved(atom()) -> boolean().
is_kernel_reserved(application_controller) -> true;
is_kernel_reserved(code_server) -> true;
is_kernel_reserved(erl_prim_loader) -> true;
is_kernel_reserved(erl_signal_server) -> true;
is_kernel_reserved(error_logger) -> true;
is_kernel_reserved(erts_code_purger) -> true;
is_kernel_reserved(file_server_2) -> true;
is_kernel_reserved(global_group) -> true;
is_kernel_reserved(global_group_check) -> true;
is_kernel_reserved(global_name_server) -> true;
is_kernel_reserved(inet_db) -> true;
is_kernel_reserved(init) -> true;
is_kernel_reserved(kernel_refc) -> true;
is_kernel_reserved(kernel_safe_sup) -> true;
is_kernel_reserved(kernel_sup) -> true;
is_kernel_reserved(logger) -> true;
is_kernel_reserved(logger_handler_watcher) -> true;
is_kernel_reserved(logger_proxy) -> true;
is_kernel_reserved(logger_std_h_default) -> true;
is_kernel_reserved(logger_sup) -> true;
is_kernel_reserved(net_kernel) -> true;
is_kernel_reserved(net_sup) -> true;
is_kernel_reserved(rex) -> true;
is_kernel_reserved(socket_registry) -> true;
is_kernel_reserved(standard_error) -> true;
is_kernel_reserved(standard_error_sup) -> true;
is_kernel_reserved(standard_error_writer) -> true;
is_kernel_reserved(user) -> true;
is_kernel_reserved(user_drv) -> true;
is_kernel_reserved(user_drv_reader) -> true;
is_kernel_reserved(user_drv_writer) -> true;
is_kernel_reserved(_) -> false.

-doc """
`global`'s resolve function for every Beamtalk `scope: #global` name (ADR
0126 §4). Compares each candidate's start time (`actor_started_at/1`,
reading the `'$beamtalk_actor_started_at'` process-dictionary marker
`init/1` sets — possibly on the *other* node, via `erpc`) and keeps the
older one, stopping the younger with `stop_global_conflict_loser/1`.

Falls back to OTP's own `global:random_exit_name/3` tie-break when a start
time cannot be read for either side (e.g. a raw, non-Beamtalk process
happens to hold the conflicting registration, or the actor terminated in
the narrow window between the conflict being detected and this function
running) — `global_name_server` calls this synchronously and expects a
decision back, never an exception, so this must always return one. Note
this fallback (only reachable when ages genuinely cannot be compared, per
ADR 0126 §4 "falling back to random_exit_name's choice") inherits
`random_exit_name/3`'s own force-kill of the pid it does not return — the
`stop_global_conflict_loser/1` call after it is then a harmless no-op on an
already-dead pid. The comparable-ages path above never does this: it always
resolves via a graceful stop, never a kill.
""".
-spec resolve_global_conflict(term(), pid(), pid()) -> pid().
resolve_global_conflict(Name, Pid1, Pid2) ->
    case {actor_started_at(Pid1), actor_started_at(Pid2)} of
        {T1, T2} when is_integer(T1), is_integer(T2) ->
            {Winner, Loser} =
                case T1 =< T2 of
                    true -> {Pid1, Pid2};
                    false -> {Pid2, Pid1}
                end,
            stop_global_conflict_loser(Loser),
            Winner;
        _ ->
            %% `global:random_exit_name/3` always returns one of its two
            %% candidate pids (never `none`) per its own typespec.
            case global:random_exit_name(Name, Pid1, Pid2) of
                Pid1 ->
                    stop_global_conflict_loser(Pid2),
                    Pid1;
                Pid2 ->
                    stop_global_conflict_loser(Pid1),
                    Pid2
            end
    end.

-doc """
Stop the losing side of a resolved `global` name conflict with
`{shutdown, global_name_conflict}` (ADR 0126 §4) — never `exit(Pid, kill)`,
which OTP's own default resolver uses and which skips `terminate/2`
entirely. Runs the (synchronous, blocking) `gen_server:stop/3` in a
throwaway process rather than inline: `resolve_global_conflict/3` executes
inside `global_name_server`'s own call, and blocking that process for the
loser's full `terminate/2` would stall every other name operation
cluster-wide until it returns. Best-effort: a `Loser` that is not a
`gen_server` (or has already died) falls back to a plain `exit(Pid, kill)`,
and any other failure is swallowed — the resolver's decision (which pid
wins the name) is already final by the time this runs; a failure to tidily
stop the loser must never surface back into `global`.
""".
-spec stop_global_conflict_loser(pid()) -> ok.
stop_global_conflict_loser(Loser) ->
    spawn(fun() ->
        try
            gen_server:stop(Loser, {shutdown, global_name_conflict}, ?BT_REMOTE_CALL_TIMEOUT)
        catch
            _:_ ->
                try
                    exit(Loser, kill)
                catch
                    _:_ -> ok
                end
        end
    end),
    ok.

-doc """
Read `Pid`'s `'$beamtalk_actor_started_at'` process-dictionary marker
(`init/1`), the age `resolve_global_conflict/3` compares. `Pid` may be on
another node — a netsplit-heal conflict is exactly the case where the two
candidates most likely live on different (just-reconnected) nodes — so a
remote `Pid` reads it via `erpc:call/5` to `started_at_from_dictionary/1`
on `node(Pid)` rather than the local-only `erlang:process_info/2`. Returns
`undefined` for a dead process, a non-Beamtalk process (no marker), or an
unreachable node — `resolve_global_conflict/3` treats that as
"can't compare" and falls back to OTP's own tie-break.
""".
-spec actor_started_at(pid()) -> integer() | undefined.
actor_started_at(Pid) when node(Pid) =:= node() ->
    started_at_from_dictionary(Pid);
actor_started_at(Pid) ->
    try
        erpc:call(
            node(Pid),
            ?BT_ACTOR_WIRE_MODULE,
            started_at_from_dictionary,
            [Pid],
            ?BT_REMOTE_CALL_TIMEOUT
        )
    of
        Result -> Result
    catch
        error:{erpc, _ErpcReason} -> undefined;
        error:{exception, _Reason, _Stack} -> undefined;
        exit:{exception, _Reason} -> undefined
    end.

-doc """
Local-only read of `Pid`'s start-time marker — the `erpc` target
`actor_started_at/1` calls for a `Pid` on another node, and (for a local
`Pid`) what it delegates to directly. Exported for `erpc:call/5`.
""".
-spec started_at_from_dictionary(pid()) -> integer() | undefined.
started_at_from_dictionary(Pid) ->
    case erlang:process_info(Pid, dictionary) of
        {dictionary, Dict} when is_list(Dict) ->
            case lists:keyfind('$beamtalk_actor_started_at', 1, Dict) of
                {'$beamtalk_actor_started_at', T} when is_integer(T) -> T;
                _ -> undefined
            end;
        _ ->
            undefined
    end.
