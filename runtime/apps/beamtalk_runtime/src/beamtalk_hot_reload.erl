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
- Field migration — adds new fields with defaults, drops removed
  fields (with log warning) when Extra contains `#{module := Module}`
  (ADR 0123 Phase 0, BT-3534). The flattened field list (inherited fields
  included) is derived here, from the state's own `$beamtalk_class` tag —
  the caller passes only what the state cannot tell you.
- **Migration hook spike** (ADR 0123 Phase 1, BT-3535): if the reloading
  module exports a class-side `migrateFromV1:`, it is invoked via
  `beamtalk_object_class:local_call/3` — in the migrating process, against
  the module `code:load_binary/3` has *just* swapped in — on the raw old
  field dictionary, before the structural fallback above reconciles it.
  This is a temporary, hand-wired call site proving the one mechanism
  ADR 0123 Phase 2's general chain (`beamtalk_shape_migration`) rests on;
  it has no `shapeVersion:`, no chain beyond a single `migrateFromV1:`
  step, and no validators. Phase 2 replaces it outright. See
  `maybe_apply_migration_hook/3`.

**References:**
- docs/beamtalk-ddd-model.md (Hot Reload Context, StateMigrator)
- docs/ADR/0123-versioned-state-migration.md
- http://erlang.org/doc/design_principles/appup_cookbook.html
""".

-include_lib("kernel/include/logger.hrl").

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
code_change(_OldVsn, State, #{module := Module}) when is_map(State), is_atom(Module) ->
    %% Field migration during hot reload
    ?LOG_DEBUG("code_change: field migration", #{
        module => Module,
        domain => [beamtalk, runtime]
    }),
    MigratedState = maybe_migrate_class_key(State),
    NewState = migrate_fields(MigratedState, Module),
    {ok, NewState};
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
Migrate actor state fields during hot reload.

Calls the module's init(#{'__skip_initialize__' => true}) to get default
state — the 2-tuple, no-telemetry branch, so migration never fires
`initialize` or lifecycle start telemetry — then:
- Preserves all existing field values from old state
- Adds new fields with their default values
- Drops removed fields (with log warning)
- Preserves internal keys present in new init defaults (e.g. __class_mod__)

ADR 0123 Phase 0 (BT-3534): the flattened field list (inherited fields
included) is derived here — from the class registry, via the state's own
`$beamtalk_class` tag — rather than being passed by the caller; the old
`{NewInstanceVars, Module}` `Extra` shape let the caller choose which list
to pass, which was itself the cause of BT-3531's dropped-inherited-fields
bug.

**Read before seed.** `'__shape_version__'` is read from the *incoming*
`OldState` before `BaseState` is seeded from the new init's defaults
(which, as of BT-3534, also carry `'__shape_version__'` — always `1`
today, since `shapeVersion:` itself is a later phase). Seeding first would
stamp the new default's version in ahead of the read, losing the state's
actual old version — the ordering a future migration chain (ADR 0123
Phase 1/2) will need to run from the right `FromVersion`. Until that chain
exists, the *old* version is restamped onto the migrated result unchanged:
no migration has actually run, so nothing has earned advancing it.
""".
-spec migrate_fields(map(), atom()) -> map().
migrate_fields(OldState, Module) ->
    %% BT-3534: read-before-seed — capture the old version before any new
    %% defaults are seeded into BaseState below. Absent means version 1.
    OldShapeVersion = maps:get('__shape_version__', OldState, 1),
    ClassName = beamtalk_tagged_map:class_of(OldState, unknown),
    NewInstanceVars = beamtalk_behaviour_intrinsics:classAllFieldNamesByName(ClassName),
    %% Get new default state by calling init with the skip-initialize flag,
    %% guaranteeing the plain {ok, Map} return regardless of whether the
    %% class defines `initialize` or has typed-no-default fields.
    case
        try
            Module:init(#{'__skip_initialize__' => true})
        catch
            _:_ -> init_error
        end
    of
        {ok, NewDefaults} when is_map(NewDefaults) ->
            %% Internal keys to always preserve from new defaults
            InternalKeys = beamtalk_tagged_map:internal_fields(),
            %% Start with internal keys from new defaults (updated method table etc.)
            BaseState0 = lists:foldl(
                fun(Key, Acc) ->
                    % elp:fixme W0032 maps:find with complex branch logic
                    case maps:find(Key, NewDefaults) of
                        {ok, Val} -> Acc#{Key => Val};
                        error -> Acc
                    end
                end,
                #{},
                InternalKeys
            ),
            %% Read-before-seed (BT-3534): restamp the version read above,
            %% not whatever the new init's defaults contributed.
            BaseState = BaseState0#{'__shape_version__' => OldShapeVersion},
            %% Add new field defaults
            NewVarSet = sets:from_list(NewInstanceVars, [{version, 2}]),
            WithDefaults = lists:foldl(
                fun(Var, Acc) ->
                    % elp:fixme W0032 maps:find with complex branch logic
                    case maps:find(Var, NewDefaults) of
                        {ok, Default} -> Acc#{Var => Default};
                        error -> Acc
                    end
                end,
                BaseState,
                NewInstanceVars
            ),
            %% Overlay old state values (existing values win).
            %% BT-3535 (ADR 0123 Phase 1 spike): run the migration hook, if
            %% the reloading module defines one, before the structural
            %% fold below reconciles the raw old fields against the new
            %% declared shape — see maybe_apply_migration_hook/3.
            RawOldInstanceVars = maps:without(InternalKeys, OldState),
            OldInstanceVars = maybe_apply_migration_hook(ClassName, Module, RawOldInstanceVars),
            {Kept, Dropped} = maps:fold(
                fun(Key, Value, {KeepAcc, DropAcc}) ->
                    case sets:is_element(Key, NewVarSet) of
                        true -> {KeepAcc#{Key => Value}, DropAcc};
                        false -> {KeepAcc, [Key | DropAcc]}
                    end
                end,
                {WithDefaults, []},
                OldInstanceVars
            ),
            %% Log warning for dropped fields
            case Dropped of
                [] ->
                    ok;
                _ ->
                    ?LOG_WARNING(
                        "Hot reload dropped fields",
                        #{class => ClassName, fields => Dropped, domain => [beamtalk, runtime]}
                    )
            end,
            Kept;
        Other ->
            %% init returned an unexpected shape (or raised) — keep state
            %% unchanged, but this is not silent: log it.
            ?LOG_WARNING(
                "Hot reload field migration skipped: unexpected init/1 return",
                #{
                    class => ClassName,
                    module => Module,
                    returned => Other,
                    domain => [beamtalk, runtime]
                }
            ),
            OldState
    end.

-doc """
BT-3535 (ADR 0123 Phase 1 spike): run a class's `migrateFromV1:` hook, if
it defines one, against the raw old field dictionary.

Proves the one mechanism ADR 0123's Phase 2 general migration chain rests
on: `beamtalk_object_class:local_call/3` invoked **from inside**
`code_change/3`, while the reloading class's own module is mid-reload.
`code:load_binary/3` has already replaced `Module` by the time
`code_change/3` runs (`beamtalk_repl_loader:activate_module/4` loads before
`trigger_hot_reload/2` fires), so `local_call/3`'s own `code:ensure_loaded/1`
+ `erlang:function_exported/3` resolve against the **new** module — confirmed
by `tests/repl-protocol/cases/hot_reload_shape_hook.btscript`, where v1 has
no hook at all and v2's hook computes a value the structural fallback's
plain default could never produce.

Unconditional and single-step, on purpose: no `shapeVersion:`, no chain
beyond this one selector, no validators — Phase 2's
`beamtalk_shape_migration` replaces this outright. A class with no such
export is untouched (`OldFields` unchanged). A hook that raises is **not**
caught here: it propagates through `code_change/3` into `try_change_code/3`,
which already leaves a failed pid suspended with its state intact (BT-3534)
— the existing suspend-on-failure contract, exercised for free.
""".
-spec maybe_apply_migration_hook(atom(), atom(), map()) -> map().
maybe_apply_migration_hook(ClassName, Module, OldFields) ->
    HookFunName = beamtalk_class_dispatch:class_method_fun_name('migrateFromV1:'),
    code:ensure_loaded(Module),
    case erlang:function_exported(Module, HookFunName, 3) of
        true ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                undefined ->
                    OldFields;
                ClassPid ->
                    ClassObj = beamtalk_class_registry:class_object_from_pid(ClassPid),
                    case
                        beamtalk_object_class:local_call(ClassObj, 'migrateFromV1:', [OldFields])
                    of
                        NewFields when is_map(NewFields) ->
                            NewFields;
                        Other ->
                            ?LOG_WARNING(
                                "migrateFromV1: hook returned a non-map value; ignoring",
                                #{
                                    class => ClassName,
                                    module => Module,
                                    returned => Other,
                                    domain => [beamtalk, runtime]
                                }
                            ),
                            OldFields
                    end
            end;
        false ->
            OldFields
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
""".
-spec try_change_code(pid(), atom(), term()) -> ok | {error, term()}.
try_change_code(Pid, Module, Extra) ->
    try
        ok = sys:suspend(Pid),
        case sys:change_code(Pid, Module, undefined, Extra) of
            ok ->
                try
                    ok = sys:resume(Pid)
                catch
                    _:_ -> ok
                end,
                ok;
            {error, _} = ChangeError ->
                ChangeError
        end
    catch
        exit:{noproc, _} ->
            {error, noproc};
        exit:{timeout, _} ->
            {error, timeout};
        Class:Error:Stacktrace ->
            ?LOG_DEBUG(
                "Code change failed for ~p: ~p:~p",
                [Module, Class, Error],
                #{stacktrace => Stacktrace, domain => [beamtalk, runtime]}
            ),
            {error, {Class, Error}}
    end.
