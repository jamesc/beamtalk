%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_hot_reload).

%%% **DDD Context:** Hot Reload Context

-moduledoc """
Hot reload domain service for state migration during code upgrades.

This domain service implements the StateMigrator pattern from the DDD model.
It centralizes code_change/3 logic for OTP hot code upgrade, providing a
consistent state migration strategy across all gen_server behaviors in the
runtime.

**Current Migrations:**
- Rewrites `__class__` → `$beamtalk_class` tag key for actors
  with legacy state maps.
- Field migration — when Extra contains `#{module := Module}`, delegates to
  `beamtalk_shape_migration:migrate/3` (ADR 0123 Phase 2, BT-3536): runs the
  class's migration chain (if any `'shape_migrations'` are declared — none
  are, until Phase 3 ships the `shapeVersion:`/`migrateFromVN:` language
  surface) and reconciles the result against the **flattened** declared
  field list (inherited fields included, BT-3531), defaulting from
  `Module:init(#{'__skip_initialize__' => true})` (BT-3532). This module's
  own job is narrower now: read `'__shape_version__'` from the *old* state
  before any new default seeds it (`migrate_state/3`'s read-before-seed,
  BT-3534), strip/re-attach the internal keys `beamtalk_shape_migration`
  never sees, and let a migration failure propagate as `{error, _}` so
  `try_change_code/3`'s existing suspend-on-failure path (BT-3534) catches
  it — unchanged from before this delegation.

**References:**
- docs/beamtalk-ddd-model.md (Hot Reload Context, StateMigrator)
- docs/ADR/0123-versioned-state-migration.md
- http://erlang.org/doc/design_principles/appup_cookbook.html
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk_sentinel_tags.hrl").

%% How long `try_change_code/3` waits for one actor before reporting `timeout`
%% (the previous `sys` default of 5 s).
-define(CHANGE_WAIT_MS, 5000).
%% Bounds the worker's own `sys` calls: the worker, not the caller, owns the
%% suspension (BT-5084).
-define(SUSPEND_TIMEOUT_MS, 5000).
-define(CHANGE_TIMEOUT_MS, 60000).

%% Public API
-export([code_change/3, trigger_code_change/2, trigger_code_change/3]).

%%====================================================================
%% Public API
%%====================================================================

-doc """
Migrate state during hot code upgrade.

This is the domain service implementation of OTP's code_change/3 callback.
Called by all gen_server behaviors in the runtime when BEAM loads a new
version of a module.

**Current Migrations:**
- Rewrites `__class__` → `$beamtalk_class` for legacy actor state maps.
  Idempotent: already-migrated state is returned unchanged.
- When Extra is `#{module := Module}`, migrates actor state by calling
  the module's init to get new defaults, then merging (ADR 0123 Phase 0,
  BT-3534). `Extra` carries only the module — the flattened field list is
  derived from the state's own class tag, not passed by the caller.
""".
%% @param OldVsn The old version (either {down, Vsn} or Vsn atom/term) — ignored;
%%        the state's own '__shape_version__' is the source of truth (ADR 0123).
%% @param State The current gen_server state
%% @param Extra Application-specific upgrade data passed via sys:change_code/4
%% @returns {ok, NewState} on success, or {error, Reason} on failure
-spec code_change(OldVsn :: term(), State :: term(), Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.
code_change(_OldVsn, State, #{module := Module} = Extra) when is_map(State), is_atom(Module) ->
    %% Field migration during hot reload
    ?LOG_DEBUG("code_change: field migration", #{
        module => Module,
        domain => [beamtalk, runtime]
    }),
    MigratedState = maybe_migrate_class_key(State),
    SkipStrayWarning = maps:get(skip_stray_warning, Extra, false),
    case migrate_state(MigratedState, Module, SkipStrayWarning) of
        {ok, NewState} ->
            {ok, NewState};
        {error, Reason} ->
            {error, Reason}
    end;
code_change(_OldVsn, State, _Extra) when is_map(State) ->
    ?LOG_DEBUG("code_change: class key migration check", #{domain => [beamtalk, runtime]}),
    {ok, maybe_migrate_class_key(State)};
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-doc """
Trigger code_change for a list of actor PIDs after module reload.

Calls sys:change_code/4 for each actor PID with empty Extra.
""".
%% @param Module The module that was reloaded
%% @param Pids List of actor PIDs to upgrade
%% @returns {ok, Upgraded, Failures} where Upgraded is count of successes
%%          and Failures is list of {Pid, Reason} tuples
-spec trigger_code_change(atom(), [pid()]) ->
    {ok, non_neg_integer(), [{pid(), term()}]}.
trigger_code_change(Module, Pids) ->
    trigger_code_change(Module, Pids, []).

-doc """
Trigger code_change for a list of actor PIDs with Extra data.

Extra can be `#{module := Module}` to enable field migration (ADR 0123
Phase 0, BT-3534). Calls sys:change_code/4 for each actor PID. Failures
are collected but do not prevent other actors from being upgraded — and,
per BT-3534, a failed actor is left **suspended** rather than resumed
onto new code with its old-shaped state (see `try_change_code/3`).
""".
-spec trigger_code_change(atom(), [pid()], term()) ->
    {ok, non_neg_integer(), [{pid(), term()}]}.
trigger_code_change(Module, Pids, Extra) ->
    PidCount = length(Pids),
    ?LOG_DEBUG("Triggering code_change", #{
        module => Module, actor_count => PidCount, domain => [beamtalk, runtime]
    }),
    Result = lists:foldl(
        fun(Pid, {ok, Upgraded, Failures}) ->
            case try_change_code(Pid, Module, Extra) of
                ok ->
                    {ok, Upgraded + 1, Failures};
                {error, Reason} ->
                    {ok, Upgraded, [{Pid, Reason} | Failures]}
            end
        end,
        {ok, 0, []},
        Pids
    ),
    {ok, Upgraded, Failures} = Result,
    case Failures of
        [] ->
            ?LOG_DEBUG("code_change complete", #{
                module => Module,
                upgraded => Upgraded,
                domain => [beamtalk, runtime]
            });
        _ ->
            ?LOG_WARNING("code_change complete with failures", #{
                module => Module,
                upgraded => Upgraded,
                failure_count => length(Failures),
                domain => [beamtalk, runtime]
            })
    end,
    Result.

%%====================================================================
%% Internal functions
%%====================================================================

-doc """
Migrate old `__class__` tag key to `$beamtalk_class`.

Idempotent: migration may add the new key and/or remove the old key,
but will not change an existing `$beamtalk_class` value.
Only adds the new key when the legacy value is an atom (actor class names);
when both keys exist, the legacy key is removed regardless to complete cleanup.
""".
-spec maybe_migrate_class_key(map()) -> map().
maybe_migrate_class_key(State) ->
    ClassKey = beamtalk_tagged_map:class_key(),
    case {maps:find(ClassKey, State), maps:find('__class__', State)} of
        {error, {ok, Class}} when is_atom(Class) ->
            %% New key missing, old key present - migrate and remove legacy key
            maps:remove('__class__', State#{ClassKey => Class});
        {{ok, _}, {ok, _}} ->
            %% Both keys present - keep new key, remove legacy key
            maps:remove('__class__', State);
        _ ->
            State
    end.

-doc """
Migrate actor state during hot reload by delegating to
`beamtalk_shape_migration:migrate/3` (ADR 0123 Phase 2, BT-3536).

**Read before seed.** `'__shape_version__'` is read from the *incoming*
`OldState` before anything from the new module's defaults can overwrite it
— the ordering BT-3534 established, still required now that the version
feeds `migrate/3`'s `FromVersion` argument (rather than just being
restamped unchanged).

Strips the internal keys (`beamtalk_tagged_map:internal_fields/0`) before
calling `beamtalk_shape_migration:migrate/3` — its `Fields` contract is user
fields only, no `'$beamtalk_class'`/`'__methods__'`/etc. — and re-attaches
them from `Module:init(#{'__skip_initialize__' => true})`'s own defaults on
success, the same "internal keys come from the new module" rule
`migrate_fields/2` (this function's ADR 0123 Phase 0/1 predecessor) used,
finally overlaying `migrate/3`'s returned `ToVersion` as the new
`'__shape_version__'`.

A migration failure (a raising `migrateFromVN:` hook, or a typed field left
unset — see `beamtalk_shape_migration:migrate/3`) returns `{error, _}`
unchanged: `code_change/3`'s caller, `try_change_code/3`, already treats any
non-`{ok, _}` return as "leave this pid suspended, state intact" (BT-3534) —
the same suspend-on-failure contract the migration hook napkin spike
(BT-3535) exercised, generalised here to the whole chain.

**`SkipStrayWarning`** (BT-3543): `true` when the class-level reload
orchestration (`beamtalk_repl_loader_activate:hot_reload_class/2`) already ran
`beamtalk_shape_migration:check_stray_migrations/1` once for this class
before triggering `code_change/3` across its live instances — passed through
so this per-instance call does not repeat that same class-process check (and
any resulting `?LOG_WARNING`) once per live instance.
""".
-spec migrate_state(map(), atom(), boolean()) -> {ok, map()} | {error, term()}.
migrate_state(OldState, Module, SkipStrayWarning) ->
    %% BT-3534/BT-3536: read-before-seed — capture the old version before
    %% anything below can seed a different one. Absent means version 1.
    OldShapeVersion = maps:get('__shape_version__', OldState, 1),
    ClassName = beamtalk_tagged_map:class_of(OldState, unknown),
    InternalKeys = beamtalk_tagged_map:internal_fields(),
    OldInternalKeys = maps:with(InternalKeys, OldState),
    UserFields = maps:without(InternalKeys, OldState),
    MigrateOpts = #{skip_stray_warning => SkipStrayWarning},
    case beamtalk_shape_migration:migrate(ClassName, OldShapeVersion, UserFields, MigrateOpts) of
        {ok, NewFields, NewShapeVersion} ->
            BaseState = seed_internal_keys(ClassName, Module, InternalKeys, OldInternalKeys),
            {ok, maps:merge(BaseState, NewFields#{'__shape_version__' => NewShapeVersion})};
        {error, _BeamtalkError} = Err ->
            Err
    end.

-doc """
Seed the internal keys (`'$beamtalk_class'`, `'__class_mod__'`,
`'__methods__'`, `'__registry_pid__'`) from the freshly reloaded module's own
`init(#{'__skip_initialize__' => true})` defaults — the updated method
table, in particular, must come from the *new* code, never carried over from
old state. Falls back to `FallbackKeys` (the incoming state's own internal
keys, unchanged) when `init/1` is not usable — the same "keep what we had"
degrade the pre-BT-3536 `migrate_fields/3` fell back to wholesale, now
scoped to just the internal keys since `beamtalk_shape_migration`'s own
reconcile step degrades the user fields independently — with a
`?LOG_WARNING` naming the returned shape.
""".
-spec seed_internal_keys(atom(), atom(), [atom()], map()) -> map().
seed_internal_keys(ClassName, Module, InternalKeys, FallbackKeys) ->
    case
        %% bt-catcher-audit: not-applicable-no-block - default-state init/1, no caller-supplied
        %% block
        try
            Module:init(#{?BT_SKIP_INITIALIZE_KEY => true})
        catch
            _:_ -> init_error
        end
    of
        {ok, NewDefaults} when is_map(NewDefaults) ->
            lists:foldl(
                fun(Key, Acc) ->
                    % elp:fixme W0032 maps:find with complex branch logic
                    case maps:find(Key, NewDefaults) of
                        {ok, Val} -> Acc#{Key => Val};
                        error -> Acc
                    end
                end,
                #{},
                InternalKeys
            );
        Other ->
            ?LOG_WARNING(
                "Hot reload: unexpected init/1 return while seeding internal keys",
                #{
                    class => ClassName,
                    module => Module,
                    returned => Other,
                    domain => [beamtalk, runtime]
                }
            ),
            FallbackKeys
    end.

-doc """
Try to trigger code_change for a single actor via sys:change_code/4.

**Suspend-on-failure** (ADR 0123 §3, BT-3534): only a *successful*
`sys:change_code/4` resumes the actor. A pid whose `code_change` fails —
returns `{error, _}`, or raises (`sys`'s own `system_code_change/4` wraps
the callback in a bare catch, so a throw or exit becomes `{error, Reason}`
before it ever reaches here) — is left suspended, state intact,
`'__shape_version__'` unchanged; resuming it would run new code on top of
its still-old-shaped state, the exact outcome this rule exists to
prevent. `sys:get_state/1` still works for inspection while suspended.
The next call here (the next `Cart reload`) re-suspends — `sys:suspend/1`
is idempotent on an already-suspended pid — and retries the migration
from the old version. A `sys:resume/1` failure after a *successful*
`change_code` (e.g. the pid exits in the gap) is swallowed, same as
before this change: it must not turn a successful migration into a
reported failure.

**Who owns the suspension (BT-5084).** The suspend / change / resume sequence
runs in a worker process of its own, so the actor is never left suspended by
anything but a failed migration. If the reloading process dies, or stops
waiting after 5 s (`{error, timeout}`), the worker carries on: a change that
finishes late resumes the actor, one that never finishes within 60 s is
resumed with an ERROR log, and a suspend that timed out behind a busy handler
has its resume queued behind it. Only a migration that returned `{error, _}`
stays suspended, and the reload reports those pids (`instances_suspended`).
""".
-spec try_change_code(pid(), atom(), term()) -> ok | {error, term()}.
try_change_code(Pid, Module, Extra) ->
    %% BT-5084: the suspend / change / resume sequence runs in its own process,
    %% so the suspension has an owner that outlives the caller's wait. If the
    %% reloading process dies or stops waiting (timeout), the worker still
    %% resumes the actor as soon as the change is confirmed finished.
    {Worker, Mon} = spawn_monitor(fun() ->
        exit({change_result, run_change(Pid, Module, Extra)})
    end),
    receive
        {'DOWN', Mon, process, Worker, {change_result, Result}} ->
            Result;
        {'DOWN', Mon, process, Worker, Other} ->
            ?LOG_ERROR(
                #{msg => <<"code change worker crashed">>, module => Module, pid => Pid},
                #{reason => Other, domain => [beamtalk, runtime]}
            ),
            {error, {Other, worker_crashed}}
    after ?CHANGE_WAIT_MS ->
        %% The worker keeps the suspension and resumes the actor once the change
        %% finishes (or logs why it could not). Never silent: ERROR now.
        erlang:demonitor(Mon, [flush]),
        ?LOG_ERROR(
            #{
                msg =>
                    <<"code change timed out; the actor is resumed when the change finishes">>,
                module => Module,
                pid => Pid
            },
            #{domain => [beamtalk, runtime]}
        ),
        {error, timeout}
    end.

%% The suspend / change_code / resume sequence for one actor. Runs in the
%% `try_change_code/3` worker.
-spec run_change(pid(), atom(), term()) -> ok | {error, term()}.
run_change(Pid, Module, Extra) ->
    try
        ok = sys:suspend(Pid, ?SUSPEND_TIMEOUT_MS),
        change_then_resume(Pid, Module, Extra)
    catch
        exit:{noproc, _} ->
            {error, noproc};
        exit:{timeout, _} ->
            %% The suspend request is queued behind a busy handler and will take
            %% effect later with nobody to undo it: queue the resume behind it.
            _ = resume_quietly(Pid),
            {error, timeout};
        Class:Error:Stacktrace ->
            ?LOG_DEBUG(
                "Code change failed for ~p: ~p:~p",
                [Module, Class, Error],
                #{stacktrace => Stacktrace, domain => [beamtalk, runtime]}
            ),
            _ = resume_quietly(Pid),
            {error, {Class, Error}}
    end.

-spec change_then_resume(pid(), atom(), term()) -> ok | {error, term()}.
change_then_resume(Pid, Module, Extra) ->
    try sys:change_code(Pid, Module, undefined, Extra, ?CHANGE_TIMEOUT_MS) of
        ok ->
            _ = resume_quietly(Pid),
            ok;
        {error, _} = ChangeError ->
            %% ADR 0123 §3: left suspended, state intact. The reload reports the
            %% pid in its `instances_suspended` finding.
            ChangeError
    catch
        exit:{timeout, _} ->
            %% Still running after the long wait: outcome unknown. Resume
            %% (queued behind the change) rather than leave the actor
            %% unresponsive indefinitely, and say so.
            ?LOG_ERROR(
                #{
                    msg => <<"code change did not finish; resuming actor with unknown outcome">>,
                    module => Module,
                    pid => Pid
                },
                #{domain => [beamtalk, runtime]}
            ),
            _ = resume_quietly(Pid),
            {error, timeout}
    end.

%% Resume, swallowing a failure (the actor may have exited): a resume failure
%% after a successful change must not turn it into a reported failure.
-spec resume_quietly(pid()) -> ok.
resume_quietly(Pid) ->
    try
        ok = sys:resume(Pid, ?CHANGE_TIMEOUT_MS)
    catch
        _:_ -> ok
    end.
