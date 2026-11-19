%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_activate).

%%% **DDD Context:** Workspace Context

-moduledoc """
Module activation for the loader: loading a compiled class module into the class registry and driving the hot-reload cascade (`register_class_module/1`, `trigger_hot_reload/2`, descendant reload, suspended-actor findings). Moved verbatim out of `beamtalk_repl_loader` (BT-4907).
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include("beamtalk_repl_loader.hrl").

-export([
    activate_module/2,
    activate_module/3,
    activate_module/4,
    register_class_module/1
]).

%% Exported for testing (only in test builds)
-ifdef(TEST).
-export([
    resolve_class_name/1,
    existing_atom_or_undefined/1,
    trigger_hot_reload/2
]).
-endif.

-doc """
Activate a loaded module: register classes, trigger hot reload,
and update workspace metadata.
""".
-spec activate_module(atom(), [map()]) -> ok | {error, #beamtalk_error{}}.
activate_module(ModuleName, Classes) ->
    activate_module(ModuleName, Classes, undefined).

-doc """
Activate a loaded module with an optional source path for workspace metadata.

BT-5015: returns `{error, #beamtalk_error{kind = registration_error}}` when
`register_class/0` fails, before hot reload, workspace bookkeeping and rechecks.
The module is already loaded by then and is deliberately **not** purged here: on
a reload the previous code is already replaced, so deleting the module would
take down a live class. Callers report the error and skip their own
bookkeeping (source tracking, ChangeLog); `beamtalk_repl_eval` purges a freshly
defined protocol module itself.
Passing SourcePath ensures the source file is recorded in workspace_meta so that
new VS Code sessions (which have an empty session tracker) can still navigate to source.

ADR 0105 Phase 2: `spawn_shape_recheck/1` fires last, after
`register_class_module/1` has installed the new `register_class/0` (which is what
refreshes the compiler port's ambient class-hierarchy cache — see
`beamtalk_recheck:trigger_shape/2`'s moduledoc) — so a shape-change re-check
always sees the *new* shape when it recompiles a candidate dependent. Runs
for every `activate_module/3` call, not just the three class-body-reload
paths that call `prime_shape_capture/1` first: for the others (a method
patch, a method removal, a brand-new class, a protocol) the shape store was
never primed for these classes, so `beamtalk_workspace_shape_store:capture/1`
self-seeds and always classifies `no_op` — see its moduledoc.

**Asynchronous *and serialised*, unlike the method-signature path's
`maybe_trigger_recheck/4`** (called synchronously from
`load_recompiled_method/8`/`remove_method/3`, which only fire on an
explicit `>>` patch/removal — comparatively rare during bulk loading).
`activate_module/3` is the common path for *every* class-body install
(`:load`, inline `subclass:` redefinition, a file reload), so it runs on
every ordinary class load, not just explicit patches — and `spawnWith:`
(always in `trigger_shape/2`'s dependent-selector set, ADR 0105 §Mechanism
step 2 / this ADR's Alternatives) is close to the worst-case common
selector, used by every Actor subclass in the image. Running the recheck
synchronously here measurably regressed a heavy sequential-reload scenario
(discovered via the `repl_protocol` e2e suite) — dozens of
shape-changing reloads in one session each paying the per-reload
caller-cap's up-to-20 compiler round trips, serialised in front of every
subsequent REPL response, enough to trip a client-side timeout.

Off the response path is not enough on its own, though: a bare `spawn/1`
per reload would let an unbounded number of these checks run *concurrently*,
each independently hammering the single, already-serialising
`beamtalk_compiler_server` (ADR 0022) that also carries the still-synchronous
method-signature recheck and ordinary editor/LSP diagnostic requests — that
just relocates the latency risk from "this reload's own response" onto
"an unrelated concurrent request sharing the same compiler port" (found in
adversarial review). `spawn_shape_recheck/1` therefore hands off to
`beamtalk_workspace_shape_recheck_worker:enqueue/1`, a single dedicated
`gen_server` (started under `beamtalk_workspace_sup`) whose mailbox
processes one shape re-check at a time — bounding in-flight compiler-port
contention from this path to 1, same order of magnitude as the synchronous
method-signature path's own footprint, while still returning immediately to
the caller. The re-check and its publish
(`beamtalk_workspace_findings_store` + the `'ReloadCheckCompleted'`
announcement) still happen, just off the install's response path and queued
behind any earlier reload's check — every consumer already treats that
announcement as an asynchronous push (the LSP/REPL/workspace-UI listeners
all already `receive`/subscribe rather than read a synchronous return
value), so this is not a behaviour change for any surface, only a latency
and contention fix for the trigger.

**ADR 0107 Phase A:** this 3-arity form always passes `[]` for the
leaf-change detection `activate_module/4` (below) accepts — see that
function's doc for why the detection has to happen in the *caller*, before
`code:load_binary/3`, not here.
""".
-spec activate_module(atom(), [map()], string() | undefined) -> ok | {error, #beamtalk_error{}}.
activate_module(ModuleName, Classes, SourcePath) ->
    activate_module(ModuleName, Classes, SourcePath, []).

-doc """
ADR 0107 Phase A: same as `activate_module/3`, plus
`NewlyNonLeafSuperclasses` — the result of
`superclasses_losing_leaf_status/1`, which the **caller** must have computed
against `Classes` *before* its own `code:load_binary/3` call (not passed as
`Classes` here and computed internally): every generated class module
carries `'on_load' = [{register_class, 0}]` (`actor_codegen.rs`),
and `code:load_binary/3` runs a module's `on_load` function **synchronously,
before returning** — so by the time *any* `activate_module/*` arity could
inspect the hierarchy, the calling `register_class/0` (registering the new
subclass link) has already run, even though `activate_module/3`'s own
`register_class_module/1` call (a redundant, harmless second invocation of the
same idempotent `register_class/0`) has not. This is exactly why
`prime_shape_capture/1` (ADR 0105 Phase 2's shape-generation seed) already
has to run before `code:load_binary/3` too — same ordering hazard, same
fix shape. Only call sites that can introduce a genuinely new class
definition (`load_class_module/3`, `load_compiled_module/6`,
`reload_compile_and_load/4`, `beamtalk_repl_loader_newclass:new_class_install/8`) bother computing this;
protocol loads (superclass always `"Object"`, never newly-non-leaf) and
method-only patches (`load_recompiled_method/8` — never changes a class's
declared superclass) pass `[]` via the `activate_module/2,3` arities
instead, which is correct, not a gap: neither path can ever produce a
transition this mechanism needs to catch.
""".
-spec activate_module(atom(), [map()], string() | undefined, [binary()]) ->
    ok | {error, #beamtalk_error{}}.
activate_module(ModuleName, Classes, SourcePath, NewlyNonLeafSuperclasses) ->
    case register_class_module(ModuleName) of
        ok ->
            trigger_hot_reload(ModuleName, Classes),
            beamtalk_workspace_meta:register_module(ModuleName, SourcePath),
            beamtalk_workspace_meta:update_activity(),
            beamtalk_repl_loader_recheck:spawn_shape_recheck(Classes),
            beamtalk_repl_loader_recheck:spawn_leaf_change_recheck(NewlyNonLeafSuperclasses);
        {error, _} = Error ->
            %% BT-5015: no hot reload, module registration or recheck for a
            %% class that did not register.
            Error
    end.

-doc """
Register a loaded module's classes by calling its `register_class/0`.

BT-5015: a module that exports no `register_class/0` registers nothing (`ok`).
Any other failure — a raise, or an `{error, _}` return — is logged at ERROR and
returned as a structured `registration_error`, so the load fails instead of
reporting a class that never registered (bad superclass, name collision,
registry error) as loaded. Nothing is tolerated: `code:load_binary/3` already ran
this same function as the module's `on_load`, so a failure here is a real one.
""".
-spec register_class_module(atom()) -> ok | {error, #beamtalk_error{}}.
register_class_module(ModuleName) ->
    case erlang:function_exported(ModuleName, register_class, 0) of
        true ->
            %% bt-catcher-audit: not-applicable-no-block - compiled register_class/0, registration
            %% only
            try ModuleName:register_class() of
                {error, RegReason} ->
                    registration_failed(ModuleName, RegReason, undefined);
                _Ok ->
                    ok
            catch
                Class:Reason:Stacktrace ->
                    registration_failed(ModuleName, Reason, {Class, Stacktrace})
            end;
        false ->
            ok
    end.

-spec registration_failed(atom(), term(), undefined | {atom(), list()}) ->
    {error, #beamtalk_error{}}.
registration_failed(ModuleName, Reason, Raised) ->
    ?LOG_ERROR(
        "register_class/0 failed for ~p: ~p",
        [ModuleName, Reason],
        #{domain => [beamtalk, runtime], module => ModuleName, raised => Raised}
    ),
    {error,
        beamtalk_repl_errors:ensure_structured_error(
            {registration_error, {ModuleName, Reason}}
        )}.

-doc """
Trigger hot reload for existing actors after module reload.

BT-3531: also reconciles every loaded descendant of each reloaded class —
a class hierarchy's live subclass instances carry the superclass's fields
too (generated `init/1` merges them in), so a field change on the
superclass must reach them even though the descendant's own module wasn't
recompiled.

When a single reloaded file defines a class together with one of its own
subclasses, that subclass is already handled directly by this loop's own
`Classes` entry — `ReloadedNames` lets `hot_reload_descendants/2` skip it
there instead of migrating it a second time.
""".
-spec trigger_hot_reload(atom(), [map()]) -> ok.
trigger_hot_reload(ModuleName, Classes) ->
    ReloadedNames = sets:from_list(
        [N || ClassMap <- Classes, N <- [resolve_class_name(ClassMap)], N =/= undefined],
        [{version, 2}]
    ),
    %% Plan order does not matter: each entry migrates its own instances, and
    %% `usort` only drops duplicates (a class named by two `Classes` entries).
    Plan = lists:usort(
        lists:append([
            [{ModuleName, ClassName} | descendant_targets(ClassName, ReloadedNames)]
         || ClassMap <- Classes,
            ClassName <- [resolve_class_name(ClassMap)],
            ClassName =/= undefined
        ])
    ),
    %% ADR 0150: one pass over the node's processes serves the whole reload —
    %% marked actors only (the `native:` fallback would also match other
    %% classes' and non-Beamtalk processes, which must not be migrated) —
    %% the reloaded classes and every descendant, grouped by class — and is
    %% taken here, after the new code is loaded, never cached from an earlier
    %% reload (owner condition (a): readers re-scan, no stale list).
    Instances =
        case Plan of
            [] -> #{};
            _ -> beamtalk_actor_scan:scan_marked_classes([C || {_, C} <- Plan])
        end,
    lists:foreach(
        fun({Module, ClassName}) ->
            Pids = [Pid || {Pid, _StartedAt} <- maps:get(ClassName, Instances, [])],
            hot_reload_class(Module, ClassName, Pids)
        end,
        Plan
    ),
    ok.

-doc """
BT-3531: the hot-reload targets `{Module, Descendant}` for every loaded
descendant of ClassName, each against its own (unchanged) module — only the
field set being reconciled changed, via the ancestor's field addition/removal,
not the descendant's code.

Skips any descendant in `ReloadedNames` — it already has its own direct entry
in `trigger_hot_reload/2`'s plan, so reaching it again here would double the
suspend/change_code/resume cycle for the same instances.

`whereis_class/1` and `module_name/1` are two independent calls with a gap
between them — if a descendant's class process exits in that gap
(concurrent removal/rename from another session, or an untrappable `kill`
before `terminate/2` runs), `module_name/1` raises `exit({noproc, _})`.
Since this runs before `activate_module/4`'s workspace bookkeeping steps
and the new module code is already loaded by this point, an uncaught
crash here would abort the whole reload half-applied — so a vanished
descendant is skipped, same as `undefined` above.
""".
-spec descendant_targets(atom(), sets:set(atom())) -> [{atom(), atom()}].
descendant_targets(ClassName, ReloadedNames) ->
    lists:filtermap(
        fun(Descendant) ->
            case sets:is_element(Descendant, ReloadedNames) of
                true ->
                    false;
                false ->
                    case beamtalk_runtime_api:whereis_class(Descendant) of
                        undefined ->
                            false;
                        ClassPid ->
                            try
                                {true, {beamtalk_runtime_api:module_name(ClassPid), Descendant}}
                            catch
                                exit:{noproc, _} -> false
                            end
                    end
            end
        end,
        beamtalk_class_registry:all_subclasses(ClassName)
    ).

%% Trigger hot reload for a single class's live instances (`Pids`, from the
%% reload's one scan), against ModuleName (that class's own BEAM module — the
%% just-reloaded module for the reloaded class itself, or an unchanged
%% descendant module).
%%
%% BT-3534: Extra carries `module => ModuleName` — the loader no longer
%% computes a field list at all; the flattened field list (inherited
%% fields included) is derived inside beamtalk_hot_reload from the live
%% state's own class tag. The old `{IVars, ModuleName}` shape let the
%% caller choose which list to pass, which was itself the cause of
%% BT-3531's dropped-inherited-fields bug.
%%
%% BT-3543: Extra also carries `skip_stray_warning => true`, since this
%% function already ran that check once for ClassName below.
%%
%% BT-3538 (ADR 0123 §4 row 5): when `trigger_code_change/3` leaves any
%% instance suspended, publishes an `instances_suspended` reload finding
%% (`beamtalk_shape_diff:suspended_finding/3`) synchronously, right here —
%% not deferred through `beamtalk_workspace_shape_recheck_worker`'s async
%% queue like the field/version findings (`maybe_trigger_shape_recheck_for_class/1`)
%% are, since this applies uniformly to a directly-reloaded class *and* every
%% descendant (neither of which is primed/captured in the shape store for a
%% descendant, which never has its own module recompiled) and needs no
%% shape/version diff to report — it is purely "did any live instance of this
%% class end up suspended", which `trigger_code_change/3`'s own return answers.
-spec hot_reload_class(atom(), atom(), [pid()]) -> ok.
hot_reload_class(_ModuleName, _ClassName, []) ->
    ok;
hot_reload_class(ModuleName, ClassName, Pids) ->
    %% BT-3543: the stray-migration-outside-table check only depends
    %% on ClassName, never on an instance — run it once here rather
    %% than once per pid inside code_change/3 (skip_stray_warning
    %% tells the per-instance path below not to repeat it).
    beamtalk_shape_migration:check_stray_migrations(ClassName),
    Extra = #{module => ModuleName, skip_stray_warning => true},
    {ok, Upgraded, Failures} = beamtalk_runtime_api:trigger_code_change(
        ModuleName, Pids, Extra
    ),
    publish_suspended_finding(ModuleName, ClassName, Upgraded, Failures).

%% Best-effort publish of the `instances_suspended` finding (ADR 0123 §4 row
%% 5) for `ClassName` through the existing reload-findings channel. Reads
%% the just-installed `ToVersion` directly off `ModuleName`'s own meta
%% (cheap, already-loaded local call) rather than the shape store, since
%% this runs for descendants too, which the shape store never primes/captures
%% (their own module is not recompiled by this reload — see
%% `hot_reload_class/3`'s doc). Never raises: any internal failure is logged
%% and swallowed — this is advisory plumbing on the reload's own response
%% path, never a gate on the reload having already succeeded.
-spec publish_suspended_finding(atom(), atom(), non_neg_integer(), [{pid(), term()}]) -> ok.
publish_suspended_finding(_ModuleName, _ClassName, _Upgraded, []) ->
    ok;
publish_suspended_finding(ModuleName, ClassName, Upgraded, Failures) ->
    try
        ClassNameBin = atom_to_binary(ClassName, utf8),
        ToVersion = read_installed_shape_version(ModuleName),
        Outcome = #{migrated => Upgraded, suspended => Failures},
        Findings = beamtalk_shape_diff:suspended_finding(ClassNameBin, ToVersion, Outcome),
        beamtalk_repl_loader_recheck:publish_reload_findings(ClassNameBin, Findings)
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Failed to publish suspended-instances finding (reload unaffected)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    class => ClassName,
                    domain => [beamtalk, runtime]
                }
            ),
            ok
    end.

%% `ModuleName`'s currently-installed 'shape_version' (default 1, BT-3537's
%% "no shapeVersion: clause declared" convention) — tolerant of a module with
%% no `__beamtalk_meta/0` at all (a dynamic/legacy class), same degrade
%% `beamtalk_workspace_shape_store:read_own_meta/1` uses.
-spec read_installed_shape_version(atom()) -> pos_integer().
read_installed_shape_version(ModuleName) ->
    case beamtalk_class_metadata:read_meta(ModuleName) of
        {ok, Meta} -> maps:get(shape_version, Meta, 1);
        not_available -> 1
    end.

%% Resolve a class name atom from a class map entry.
-spec resolve_class_name(map()) -> atom() | undefined.
resolve_class_name(ClassMap) ->
    case maps:get(name, ClassMap, undefined) of
        N when is_binary(N) ->
            existing_atom_or_undefined(N);
        N when is_atom(N) ->
            N;
        N when is_list(N) ->
            existing_atom_or_undefined(N);
        _ ->
            undefined
    end.

%% `atom() | undefined` view of `beamtalk_text:to_existing_atom/1`.
-spec existing_atom_or_undefined(binary() | string()) -> atom() | undefined.
existing_atom_or_undefined(Name) ->
    case beamtalk_text:to_existing_atom(Name) of
        {ok, Atom} -> Atom;
        error -> undefined
    end.
