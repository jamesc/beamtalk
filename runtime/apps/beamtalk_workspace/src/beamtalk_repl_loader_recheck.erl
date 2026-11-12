%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_recheck).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Reload-induced re-check triggers and publishing for the Beamtalk REPL
(ADR 0105, 0107, 0108, 0123).

Owns the glue between a hot reload and `beamtalk_recheck`: capturing the
pre-install signature/shape generation (`capture_signature_generation/1`,
`prime_shape_capture/1`), deciding whether a re-check is warranted
(`maybe_trigger_*`, `spawn_*_recheck`), and publishing the outcome into
`beamtalk_workspace_findings_store` and the `reload_check` stream
(`publish_*`).

`beamtalk_recheck` stays the pure re-check engine — it computes findings and
never touches the findings store or announcements; this module does the
store/stream plumbing around it, which is why these functions live here and
not in `beamtalk_recheck`.

Extracted from `beamtalk_repl_loader` (BT-4702). The loader calls into this
module from its install/hot-reload paths; this module calls back into the
loader only for `normalize_class_source_key/1`.
""".

-export([
    %% ADR 0105 Phase 1: capture/rollback around a method install, and the
    %% post-install re-check trigger.
    capture_signature_generation/1,
    rollback_signature_generation/4,
    maybe_trigger_recheck/4,
    %% ADR 0105 Phase 2: shape capture before load, shape re-check after.
    prime_shape_capture/1,
    spawn_shape_recheck/1,
    %% Same cross-module reason: called from beamtalk_workspace_shape_recheck_worker.
    maybe_trigger_shape_recheck/1,
    %% Reload-time reflection finding publishing (called from hot_reload_class/2).
    publish_reload_findings/2,
    %% ADR 0107 Phase A: leaf-change detection and re-check.
    superclasses_losing_leaf_status/1,
    spawn_leaf_change_recheck/1,
    maybe_trigger_leaf_change_recheck/1,
    %% ADR 0108: alias-change re-check (enqueue half is called from
    %% beamtalk_repl_eval:handle_type_alias_definition/3).
    spawn_alias_change_recheck/1,
    maybe_trigger_alias_change_recheck/1
]).

%% Exported for testing (only in test builds)
-ifdef(TEST).
-export([
    %% ADR 0107 Phase A: leaf-change detection/publish helpers.
    was_leaf_class/1,
    publish_leaf_change_recheck_outcome/2,
    %% ADR 0108 hot-reload re-check trigger: publish helper.
    publish_alias_change_recheck_outcome/2
]).
-endif.

-export_type([capture_outcome/0]).

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% ChangeLog entry construction lives in its own module (BT-4683).
-define(ENTRIES, beamtalk_repl_loader_changelog_entries).

%% The outcome of a best-effort signature-store capture (ADR 0105 Phase 1):
%% `{captured, Prev, Classification}` when the store call succeeded
%% (`Prev` is whatever it reported as the pre-capture generation — feed this
%% straight back to rollback_signature_generation/4 on a subsequent install
%% failure; `Classification` is `beamtalk_signature_diff:classification/0`,
%% consumed by the re-check trigger below), or `not_captured` when the
%% capture itself failed (nothing to roll back, nothing to re-check).
-type capture_outcome() ::
    {captured, beamtalk_workspace_signature_store:maybe_signature(),
        beamtalk_signature_diff:classification()}
    | not_captured.

%% Capture the freshly-compiled signature into the signature-generation store
%% (ADR 0105 Phase 1). Called from load_recompiled_method/8 *before*
%% the patch installs — see the call site for why ordering matters. Best-effort
%% and self-swallowing, mirroring emit_change_entry/1: the store is diagnostic
%% plumbing for a later re-check, never a gate on the install itself.
%% Returns the capture_outcome() so the caller can roll back on install failure.
-spec capture_signature_generation(map()) -> capture_outcome().
capture_signature_generation(MethodInfo) ->
    try
        do_capture_signature_generation(MethodInfo)
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Failed to capture method signature generation (install proceeding)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    method_info => maps:with([class_name, selector], MethodInfo),
                    domain => [beamtalk, runtime]
                }
            ),
            not_captured
    end.

-spec do_capture_signature_generation(map()) -> capture_outcome().
do_capture_signature_generation(MethodInfo) ->
    ClassNameBin = maps:get(class_name, MethodInfo),
    SelectorBin = maps:get(selector, MethodInfo),
    IsClassMethod = maps:get(is_class_method, MethodInfo, false),
    Side = ?ENTRIES:patch_side(IsClassMethod),
    NewSignature = #{
        return_type => maps:get(return_type, MethodInfo, <<"Dynamic">>),
        param_types => maps:get(param_types, MethodInfo, [])
    },
    {Prev, Classification} = beamtalk_workspace_signature_store:capture(
        ClassNameBin, SelectorBin, Side, NewSignature
    ),
    {captured, Prev, Classification}.

%% Undo a capture_signature_generation/1 (or capture_signature_removal/3) call
%% whose install/removal subsequently failed (ADR 0105 Phase 1).
%% `not_captured` is a no-op (the capture itself never wrote anything). Best-
%% effort and self-swallowing — a rollback failure must never surface as the
%% install/removal error the caller is already propagating.
-spec rollback_signature_generation(binary(), binary(), instance | class, capture_outcome()) -> ok.
rollback_signature_generation(_ClassNameBin, _SelectorBin, _Side, not_captured) ->
    ok;
rollback_signature_generation(ClassNameBin, SelectorBin, Side, {captured, Prev, _Classification}) ->
    try
        _ = beamtalk_workspace_signature_store:rollback(ClassNameBin, SelectorBin, Side, Prev),
        ok
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Failed to roll back method signature generation (install/removal failure proceeding)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    class => ClassNameBin,
                    selector => SelectorBin,
                    domain => [beamtalk, runtime]
                }
            ),
            ok
    end.

%% Fire the re-check orchestration (ADR 0105 Phase 1) for a
%% successfully-installed patch/removal, then publish the outcome (ADR 0105
%% Phase 1): update `beamtalk_workspace_findings_store` and, when
%% there is something for a live surface (LSP / REPL / workspace UI) to act
%% on, broadcast a `'ReloadCheckCompleted'` system announcement.
%%
%% `ClassNameBin`'s source just changed (install or removal — a
%% `Workspace changes revert:` re-install routes through this exact same
%% path, `do_revert/2` -> `install_revert_patch/4` / `revert_removal/3` ->
%% `load_recompiled_method/8` / `remove_method/3`), so any reload-induced
%% findings previously recorded with `ClassNameBin` as the *caller* reference
%% byte offsets into source that no longer exists — they are cleared
%% unconditionally, before deciding whether a dependent re-check is even
%% warranted (`beamtalk_workspace_findings_store`'s moduledoc "Clearing-by-
%% replacement" section explains why this single hook covers the ADR's
%% explicit revert bullet with no bespoke revert-specific code, and also
%% closes the same gap for a plain hand-edit that fixes what a reload broke).
%%
%% Accepted tradeoff:
%% `Classification` is only as correct as the signature-generation store's
%% chain. Two concurrent sessions patching the same `{Class, Selector, Side}`
%% key can race `capture/4`/`rollback/4` such that a losing session's
%% rollback overwrites the store with a generation that is no longer the
%% actually-live one (`beamtalk_workspace_signature_store:rollback/4` does an
%% unconditional put, not a conditional "restore only if I'm still current"
%% write) — the *next* capture then diffs against the wrong baseline and this
%% function can fire on a false `signature_change`/`no_op`. Not fixed here:
%% the store is a merged surface, and a full fix (per-key conditional
%% rollback) is out of scope here. Advisory-only mitigates the
%% blast radius (a wrong finding is noise, not a build/runtime failure).
-spec maybe_trigger_recheck(binary(), binary(), instance | class, capture_outcome()) -> ok.
maybe_trigger_recheck(ClassNameBin, SelectorBin, Side, CaptureOutcome) ->
    PrevOwnFindings = findings_store_clear_owner(ClassNameBin),
    {Classification, DependentResult} = maybe_run_recheck(
        ClassNameBin, SelectorBin, Side, CaptureOutcome
    ),
    publish_recheck_outcome(
        ClassNameBin, SelectorBin, Classification, PrevOwnFindings, DependentResult
    ).

%% Best-effort wrapper around `beamtalk_workspace_findings_store:clear_owner/1`
%% (ADR 0105 Phase 1) — the store is REPL-mode-only (see
%% `beamtalk_workspace_sup`'s `repl_child_specs/6`), so it is legitimately
%% absent under `beamtalk_repl_loader:install_method/9`'s non-REPL callers
%% (e.g. `beamtalk_repl_loader_tests.erl`'s unit fixtures, and — per the same
%% reasoning `capture_signature_generation/1` already documents — a run-mode
%% precompiled artifact never reaches this hook at all). Degrades to "nothing
%% was cleared" on any failure rather than crashing the install/removal that
%% called `maybe_trigger_recheck/4` — the ADR's "advisory, never blocking"
%% guarantee applies to publishing exactly as it does to the re-check itself.
-spec findings_store_clear_owner(binary()) -> [beamtalk_recheck:finding()].
findings_store_clear_owner(OwnerBin) ->
    try
        beamtalk_workspace_findings_store:clear_owner(OwnerBin)
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Reload-findings store unavailable (clear skipped)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    owner => OwnerBin,
                    domain => [
                        beamtalk, runtime
                    ]
                }
            ),
            []
    end.

%% Best-effort wrapper around
%% `beamtalk_workspace_findings_store:put_owner_origin/3` — see
%% `findings_store_clear_owner/1`'s doc for why the store may legitimately be
%% absent, and why a failure here must degrade rather than crash.
%%
%% `ChangedClassBin` scopes the replacement to *this* changed class's
%% contribution to `OwnerBin`'s findings (ADR 0105 §Mechanism step 4,
%% `beamtalk_workspace_findings_store`'s moduledoc) — a caller broken by two
%% independently-reloading classes keeps both findings; only the one that
%% actually just got re-checked is replaced.
%%
%% Returns the store's *previous* bucket for this exact
%% `{OwnerBin, ChangedClassBin}` origin (`[]` when there was nothing stored,
%% and `[]` on any store failure) — `put_owner_origin/3` hands this back
%% precisely so a caller can tell "this write actually cleared something a
%% surface may still be displaying" from "this write was a no-op", with no
%% extra store round trip. `publish_leaf_change_recheck_outcome/2` and
%% `publish_alias_change_recheck_outcome/2` use it to decide whether a
%% findings-free re-check is still worth announcing.
-spec findings_store_put_owner_origin(binary(), binary(), [beamtalk_recheck:finding()]) ->
    [beamtalk_recheck:finding()].
findings_store_put_owner_origin(OwnerBin, ChangedClassBin, Findings) ->
    try
        beamtalk_workspace_findings_store:put_owner_origin(OwnerBin, ChangedClassBin, Findings)
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Reload-findings store unavailable (publish skipped)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    owner => OwnerBin,
                    changed_class => ChangedClassBin,
                    domain => [
                        beamtalk, runtime
                    ]
                }
            ),
            []
    end.

%% Runs `beamtalk_recheck:trigger/4` only when `CaptureOutcome` warrants it
%% (`{captured, _, signature_change | removal}`) — `no_op`/`not_captured`
%% skip it, since there is nothing to diff dependents against. When it does
%% run, replaces every checked owner's stored findings **for this changed
%% class** via `beamtalk_workspace_findings_store:put_owner_origin/3` — this
%% is the clearing-by-replacement rule applied per `{caller, changed class}`
%% origin (not per caller alone — see the store's moduledoc for why a caller
%% broken by two independently-reloading classes needs both origins kept
%% separate), including a clean re-check (`put_owner_origin(Owner,
%% ClassNameBin, [])`) and a self-referential caller (already cleared in full
%% by `clear_owner/1` above, since that call is un-scoped; a fresh
%% empty-or-not set here re-adds only this origin's contribution).
%%
%% Returns `{Classification, Result}` when a re-check ran, or `{self_edit,
%% undefined}` when it did not — `self_edit` stands in for "this install had
%% no dependents worth diffing", so `publish_recheck_outcome/5` always has a
%% classification to log/announce with even when the only newsworthy thing
%% that happened is `ClassNameBin`'s own stale findings being cleared.
-spec maybe_run_recheck(binary(), binary(), instance | class, capture_outcome()) ->
    {self_edit | beamtalk_recheck:classification(), beamtalk_recheck:result() | undefined}.
maybe_run_recheck(_ClassNameBin, _SelectorBin, _Side, not_captured) ->
    {self_edit, undefined};
maybe_run_recheck(_ClassNameBin, _SelectorBin, _Side, {captured, _Prev, no_op}) ->
    {self_edit, undefined};
maybe_run_recheck(ClassNameBin, SelectorBin, Side, {captured, _Prev, Classification}) ->
    Result = beamtalk_recheck:trigger(ClassNameBin, SelectorBin, Side, Classification),
    #{
        checked_owners := CheckedOwners,
        findings := Findings,
        not_verified_owners := NotVerifiedOwners
    } = Result,
    lists:foreach(
        fun(OwnerBin) ->
            OwnerFindings = [F || F <- Findings, maps:get(owner, F) =:= OwnerBin],
            findings_store_put_owner_origin(OwnerBin, ClassNameBin, OwnerFindings)
        end,
        CheckedOwners
    ),
    mark_unverified_findings_stale(ClassNameBin, NotVerifiedOwners),
    {Classification, Result}.

%% A candidate whose diagnostics round-trip never completed
%% this reload (`NotVerifiedOwners` — `beamtalk_recheck:result()`'s
%% `not_verified_owners`, covering the caller-cap-dropped candidates AND any
%% `Kept` candidate that came back `skipped` (no live source recorded) or
%% `failed` (compile/compiler-port error)) never went through
%% `put_owner_origin/3` above, so any finding it already carries *for this
%% changed class* is left exactly as a previous, possibly-now-stale reload
%% wrote it — nothing here re-verified whether the caller's problem still
%% holds. Rather than let that finding keep asserting itself as current
%% forever or silently drop it (could hide a real, still-live problem), overwrite its
%% `note` in place, still through `put_owner_origin/3`'s ordinary replace
%% semantics, to say so. A candidate with no existing finding for this origin
%% has nothing to mark — most unverified candidates, every reload — so this
%% is a no-op for them (`findings_store_get_origin/2` returns `[]`).
-spec mark_unverified_findings_stale(binary(), [binary()]) -> ok.
mark_unverified_findings_stale(ClassNameBin, NotVerifiedOwners) ->
    lists:foreach(
        fun(OwnerBin) ->
            case findings_store_get_origin(OwnerBin, ClassNameBin) of
                [] ->
                    ok;
                Existing ->
                    Stale = [mark_stale_finding(ClassNameBin, F) || F <- Existing],
                    findings_store_put_owner_origin(OwnerBin, ClassNameBin, Stale)
            end
        end,
        NotVerifiedOwners
    ),
    ok.

%% Best-effort wrapper around `beamtalk_workspace_findings_store:get_origin/2`
%% — mirrors `findings_store_clear_owner/1`'s "store may legitimately be
%% absent, degrade rather than crash" reasoning.
-spec findings_store_get_origin(binary(), binary()) -> [beamtalk_recheck:finding()].
findings_store_get_origin(OwnerBin, ChangedClassBin) ->
    try
        beamtalk_workspace_findings_store:get_origin(OwnerBin, ChangedClassBin)
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Reload-findings store unavailable (staleness check skipped)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    owner => OwnerBin,
                    changed_class => ChangedClassBin,
                    domain => [
                        beamtalk, runtime
                    ]
                }
            ),
            []
    end.

%% The marker substring `stale_note/2` looks for to avoid re-wrapping a note
%% that is already marked — a candidate parked outside the cap (or
%% repeatedly skipped/failed) for many consecutive reloads must not
%% accumulate one "not re-checked" suffix per reload.
-define(RECHECK_STALE_MARKER, <<"not re-checked against the latest reload">>).

%% Overwrite `Finding`'s `note` to flag it as not re-verified this reload,
%% unless it is already so marked (idempotent across consecutive
%% not-verified reloads — see `?RECHECK_STALE_MARKER`).
-spec mark_stale_finding(binary(), beamtalk_recheck:finding()) -> beamtalk_recheck:finding().
mark_stale_finding(ClassNameBin, Finding = #{note := Note}) when is_binary(Note) ->
    case binary:match(Note, ?RECHECK_STALE_MARKER) of
        nomatch -> Finding#{note => stale_note(ClassNameBin, Note)};
        _ -> Finding
    end;
mark_stale_finding(ClassNameBin, Finding) ->
    Finding#{note => stale_note(ClassNameBin, undefined)}.

%% The reason a candidate went unverified is deliberately not named
%% here (caller-cap limit vs. no live source vs. a compiler-port failure) —
%% `mark_unverified_findings_stale/2` is fed one merged
%% `not_verified_owners` set with no per-owner reason attached, and inventing
%% one back out would either guess or need threading three more outcome
%% tags through `beamtalk_recheck:result()` for a note string alone. "May be
%% stale" is accurate and reason-agnostic for all three causes.
-spec stale_note(binary(), binary() | undefined) -> binary().
stale_note(ClassNameBin, undefined) ->
    <<"not re-checked against the latest reload of ", ClassNameBin/binary, "; may be stale">>;
stale_note(ClassNameBin, PrevNote) ->
    <<PrevNote/binary, "; not re-checked against the latest reload of ", ClassNameBin/binary,
        "; may be stale">>.

%% Log + broadcast the outcome of one `maybe_trigger_recheck/4` call — but
%% only when a live surface has something to act on: either a dependent
%% re-check ran against at least one owner (`DependentResult`'s
%% `checked_owners` non-empty), or clearing `ClassNameBin`'s own findings
%% above (`PrevOwnFindings`) actually removed something a surface might
%% still be showing. A no-op edit with no prior findings and no known
%% dependents announces nothing — the common case, and not worth a push
%% frame on every keystroke-save.
-spec publish_recheck_outcome(
    binary(),
    binary(),
    self_edit | beamtalk_recheck:classification(),
    [beamtalk_recheck:finding()],
    beamtalk_recheck:result() | undefined
) -> ok.
publish_recheck_outcome(
    ClassNameBin, SelectorBin, Classification, PrevOwnFindings, DependentResult
) ->
    {DependentCheckedOwners, Findings, Checked, NotChecked, CapNote} =
        case DependentResult of
            undefined ->
                {[], [], 0, 0, undefined};
            #{
                checked_owners := CO,
                findings := F,
                checked := C,
                not_checked := NC,
                cap_note := CN
            } ->
                {CO, F, C, NC, CN}
        end,
    TouchedOwners = lists:usort(
        DependentCheckedOwners ++
            case PrevOwnFindings of
                [] -> [];
                _ -> [ClassNameBin]
            end
    ),
    case TouchedOwners of
        [] ->
            ok;
        _ ->
            case Findings of
                [] ->
                    ok;
                _ ->
                    ?LOG_INFO(
                        "Reload re-check produced findings",
                        #{
                            class => ClassNameBin,
                            selector => SelectorBin,
                            classification => Classification,
                            callers_checked => Checked,
                            callers_not_checked => NotChecked,
                            finding_count => length(Findings),
                            domain => [beamtalk, runtime]
                        }
                    )
            end,
            beamtalk_announcements:system_announce('ReloadCheckCompleted', #{
                changedClass => ClassNameBin,
                changedSelector => SelectorBin,
                classification => Classification,
                checked => Checked,
                notChecked => NotChecked,
                capNote => CapNote,
                checkedOwners => TouchedOwners,
                findings => Findings
            }),
            ok
    end.

%%====================================================================
%% Shape-change re-check (ADR 0105 Phase 2)
%%====================================================================

-doc """
Seed the shape-generation store from each class's *currently-loaded* module
(before this class-body reload's `code:load_binary` replaces it) — the
`prime/1` half of `beamtalk_workspace_shape_store`'s two-phase capture. Call
sites: `load_class_module/3`, `load_compiled_module/6`,
`reload_compile_and_load/4` — every path that installs a full class body
(as opposed to a single-method patch or removal, neither of which can
change `state:`/`field:` slots). Best-effort and self-swallowing, mirroring
`capture_signature_generation/1`: priming is diagnostic plumbing for a later
re-check, never a gate on the install itself.
""".
-spec prime_shape_capture([map()]) -> ok.
prime_shape_capture(Classes) ->
    lists:foreach(
        fun(#{name := Name}) ->
            try
                ok = beamtalk_workspace_shape_store:prime(
                    beamtalk_repl_loader:normalize_class_source_key(Name)
                )
            catch
                Class:Reason:Stack ->
                    ?LOG_WARNING(
                        "Failed to prime shape-generation store (install proceeding)",
                        #{
                            error_class => Class,
                            reason => Reason,
                            stack => Stack,
                            class => Name,
                            domain => [beamtalk, runtime]
                        }
                    )
            end
        end,
        Classes
    ),
    ok.

-doc """
Hand `Classes` off to `beamtalk_workspace_shape_recheck_worker` and return
immediately — see `activate_module/3`'s doc ("Asynchronous *and
serialised*...") for why this must neither run on the install's response
path nor run unbounded-concurrently. The worker's mailbox is the queue: a
`cast` here never blocks the caller, and the worker processes one reload's
recheck at a time, so this call site never needs to know about ordering or
backpressure.
""".
-spec spawn_shape_recheck([map()]) -> ok.
spawn_shape_recheck(Classes) ->
    beamtalk_workspace_shape_recheck_worker:enqueue(Classes).

-doc """
Capture each class's post-install shape and, on a genuine `shape_change`,
run the shape re-check orchestration (`beamtalk_recheck:trigger_shape/2`)
and publish its findings. Reached in production only via
`beamtalk_workspace_shape_recheck_worker`'s single-worker queue (`enqueue/1`
-> `spawn_shape_recheck/1`, off the install's response path and serialised
against every other pending shape re-check — see `activate_module/3`'s doc,
"Asynchronous *and serialised*"), which is also why this is exported outside
the `-ifdef(TEST)` block: the worker lives in a different module. See
`activate_module/3`'s doc for why an un-primed class (a method patch, a new
class, a protocol) is a harmless `no_op` here.
""".
-spec maybe_trigger_shape_recheck([map()]) -> ok.
maybe_trigger_shape_recheck(Classes) ->
    lists:foreach(fun maybe_trigger_shape_recheck_for_class/1, Classes),
    ok.

-spec maybe_trigger_shape_recheck_for_class(map()) -> ok.
maybe_trigger_shape_recheck_for_class(#{name := Name}) ->
    ClassNameBin = beamtalk_repl_loader:normalize_class_source_key(Name),
    try
        {Prev, NewGen, DiffResult} = beamtalk_workspace_shape_store:capture(ClassNameBin),
        {Classification, FieldChanges} = DiffResult,
        %% ADR 0123 §4 rows 1-4 (BT-3538): joins the field-level diff with
        %% the previous/new generation's 'shape_version'/'shape_migrations'
        %% — independent of `Classification`, since a version-only finding
        %% (bumped-without-migration, version-decreased) can fire with no
        %% field change at all. Row 5 (instances_suspended) is published
        %% separately and synchronously from `hot_reload_class/2`, which
        %% runs before this worker call and needs no shape/version diff.
        ReloadFindings = beamtalk_shape_diff:reload_findings(
            ClassNameBin, Prev, NewGen, DiffResult
        ),
        publish_reload_findings(ClassNameBin, ReloadFindings),
        case Classification of
            no_op ->
                ok;
            shape_change ->
                Result = beamtalk_recheck:trigger_shape(ClassNameBin, FieldChanges),
                publish_shape_recheck_outcome(ClassNameBin, FieldChanges, Result)
        end
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Shape re-check trigger failed (reload unaffected)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    class => ClassNameBin,
                    domain => [beamtalk, runtime]
                }
            ),
            ok
    end.

-doc """
Publish `Findings` (ADR 0123 §4, BT-3538: `beamtalk_shape_diff:reload_finding()`)
through the `reload_check` channel's `'ShapeReloadFindingsCompleted'`
announcement — a genuinely distinct event from `'ReloadCheckCompleted'`
(`publish_shape_recheck_outcome/3`'s ADR 0105 xref-dependent-caller
findings), not a repurposing of it: the two finding types have different
schemas (`class`/`kind`/`fields`/`migrated`/`suspended`/`pids` vs.
`owner`/`selector`/`classification`/`sites`) and different subjects (this
reload's *own class and its live instances*, not a caller class broken by
the change) — see `beamtalk_ws_handler`'s `'ShapeReloadFindingsCompleted'`
clause for the wire encoding, and `beamtalk_repl_subscriptions:
announcement_classes/1` for why it rides the same `reload_check` stream
without a new subscription. A no-op when `Findings` is empty — unlike
`publish_shape_recheck_outcome/3`, there is no store-clearing signal to
carry on an empty list (findings here are one-shot advisory notices, not a
per-owner replaceable snapshot).
""".
-spec publish_reload_findings(binary(), [beamtalk_shape_diff:reload_finding()]) -> ok.
publish_reload_findings(_ClassNameBin, []) ->
    ok;
publish_reload_findings(ClassNameBin, Findings) ->
    ?LOG_INFO(
        "Shape reload findings",
        #{
            class => ClassNameBin,
            finding_count => length(Findings),
            kinds => lists:usort([maps:get(kind, F) || F <- Findings]),
            domain => [beamtalk, runtime]
        }
    ),
    beamtalk_announcements:system_announce('ShapeReloadFindingsCompleted', #{
        class => ClassNameBin,
        findings => Findings
    }),
    ok.

-doc """
Publish a shape re-check's outcome — mirrors `maybe_run_recheck/4`'s
per-owner `put_owner_origin/3` replacement and `publish_recheck_outcome/5`'s
`'ReloadCheckCompleted'` broadcast, reusing the exact same findings-store and
announcement schema so every existing surface (LSP / REPL / workspace UI)
renders a shape-change finding without new wiring. `changedSelector` has no
single selector to report for a shape change, so it carries
`shape_change_summary/1`'s comma-joined list of affected slot names instead
— still meaningful in the generic "`{changed_class}`>>`{changed_selector}`"
templates every surface already uses.

The announce fires whenever `CheckedOwners` is non-empty, **not** only when
`Findings` is — an empty `Findings` with non-empty `CheckedOwners` is exactly
the "reload-fixes-reload" clearing signal `publish_recheck_outcome/5` already
relies on: `put_owner_origin/3` above already cleared each checked owner's
stale shape-change findings from the store, and every surface needs the
announcement itself to know to drop what it was showing, not just a quiet
store write nobody hears about. Matching `publish_recheck_outcome/5`'s
structure exactly (found in review): the `?LOG_INFO` is gated on
`Findings` (nothing worth logging about a clean re-check), the announce is
gated on `CheckedOwners` alone.
""".
-spec publish_shape_recheck_outcome(
    binary(), [beamtalk_shape_diff:field_change()], beamtalk_recheck:result()
) ->
    ok.
publish_shape_recheck_outcome(ClassNameBin, FieldChanges, Result) ->
    #{
        checked_owners := CheckedOwners,
        findings := Findings,
        checked := Checked,
        not_checked := NotChecked,
        cap_note := CapNote,
        not_verified_owners := NotVerifiedOwners
    } = Result,
    lists:foreach(
        fun(OwnerBin) ->
            OwnerFindings = [F || F <- Findings, maps:get(owner, F) =:= OwnerBin],
            findings_store_put_owner_origin(OwnerBin, ClassNameBin, OwnerFindings)
        end,
        CheckedOwners
    ),
    %% Same not-verified staleness marking as
    %% `maybe_run_recheck/4` (see `mark_unverified_findings_stale/2`'s doc) —
    %% a shape change's candidates go through the same `apply_cap/2` limit
    %% and the same `recheck_owner_for_shape/4` skipped/failed outcomes.
    mark_unverified_findings_stale(ClassNameBin, NotVerifiedOwners),
    case CheckedOwners of
        [] ->
            ok;
        _ ->
            case Findings of
                [] ->
                    ok;
                _ ->
                    ?LOG_INFO(
                        "Shape reload re-check produced findings",
                        #{
                            class => ClassNameBin,
                            field_changes => FieldChanges,
                            callers_checked => Checked,
                            callers_not_checked => NotChecked,
                            finding_count => length(Findings),
                            domain => [beamtalk, runtime]
                        }
                    )
            end,
            beamtalk_announcements:system_announce('ReloadCheckCompleted', #{
                changedClass => ClassNameBin,
                changedSelector => shape_change_summary(FieldChanges),
                classification => shape_change,
                checked => Checked,
                notChecked => NotChecked,
                capNote => CapNote,
                checkedOwners => CheckedOwners,
                findings => Findings
            }),
            ok
    end.

%% ── ADR 0107 Phase A: leaf-change re-check ────────────────────

-doc """
Which superclass names declared by `Classes` are, at this exact moment,
leaf classes with zero direct subclasses, and are therefore about to lose
that status once `Classes` installs.

**Must be called before the caller's own `code:load_binary/3`, not just
before `register_classes/2`** — see `activate_module/4`'s doc for why the
naive "before `register_classes/2`, inside `activate_module`" placement
this function first shipped with was still too late (`code:load_binary/3`'s
synchronous `on_load` hook already registers the new subclass link before
`activate_module/*` is ever reached).

A `matchExhaustive:`/`Type`-pattern site elsewhere in the image may have a
compile-time proof that depended on exactly this class being leaf (ADR 0107
Phase A's "has subclasses" compile-error restriction,
`match_validators:validate_type_pattern_class` on the Rust side) — once
this registration completes, that proof is stale.
`beamtalk_recheck:trigger_leaf_change/1` (run via
`maybe_trigger_leaf_change_recheck/1`, below) is the re-check half that
re-surfaces it instead of leaving it to crash at runtime with an opaque
`case_clause` error.

A re-registration of an *already-existing* subclass (a method-only reload,
unchanged superclass) is correctly excluded by this function's own
ordering requirement: that superclass was already non-leaf the first time
this same class registered, so `direct_subclasses/1` already finds it
non-empty on every later reload and this never re-fires for it.

`Classes` entries' `superclass` value is a `string()` on the
`compile_file_core/3` path but already a `binary()` on others (e.g. the
`load_class_module/3` path this same function is called from) —
`unicode:characters_to_binary/1` (not `list_to_binary/1`, which badargs on
an already-binary input) normalises either shape.

**Never raises: any internal failure degrades to `[]` (advisory, never
blocking — ADR 0105's own rule).** This is called on every single
new-class-defining load, immediately before `code:load_binary/3`
(`activate_module/4`'s doc) — unlike every other call site in this
mechanism (`recheck_owner_for_leaf_change/3`, `was_leaf_class/1`'s own
`binary_to_existing_atom/2` guard), a failure here sits directly in the
class-install hot path with no surrounding `try/catch` at any of its
call sites, so this function must be defensive on its own, the same way
`prime_shape_capture/1` wraps each class's store-priming individually
rather than trusting its own caller to catch anything.
""".
-spec superclasses_losing_leaf_status([map()]) -> [binary()].
superclasses_losing_leaf_status(Classes) ->
    try
        SuperclassNames = lists:usort([
            unicode:characters_to_binary(Super)
         || #{superclass := Super} <- Classes, Super =/= undefined
        ]),
        lists:filter(fun was_leaf_class/1, SuperclassNames)
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Leaf-change detection failed (install proceeding)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    domain => [beamtalk, runtime]
                }
            ),
            []
    end.

-doc """
Was `SuperclassBin` a leaf class (zero direct subclasses) at the moment
this is called? A superclass name that is not even an interned atom yet was
never loaded as a class this session, so nothing could depend on it being
leaf either — `false`, not a crash, matching every other
`binary_to_existing_atom/2` safety check in this module (this adds no
new atom-table-exhaustion surface: superclass names here are always
already-loaded class names or the `badarg` catch below fires instead of
minting a fresh atom from reload-derived text).
""".
-spec was_leaf_class(binary()) -> boolean().
was_leaf_class(SuperclassBin) ->
    try binary_to_existing_atom(SuperclassBin, utf8) of
        SuperclassAtom ->
            beamtalk_class_registry:direct_subclasses(SuperclassAtom) =:= []
    catch
        error:badarg -> false
    end.

-doc """
Hand `NewlyNonLeafSuperclasses` off to the shape-recheck worker's queue and
return immediately — reuses `beamtalk_workspace_shape_recheck_worker`'s
existing single-worker serialisation (see `spawn_shape_recheck/1`'s doc for
why an unbounded-concurrent or response-path-synchronous trigger is unsafe
here too: `trigger_leaf_change/1` is a whole-image compiler-port sweep, at
least as expensive as a shape re-check). A no-op when the list is empty —
the overwhelmingly common case, since most reloads add no new class at all,
and most that do target an already-non-leaf superclass.
""".
-spec spawn_leaf_change_recheck([binary()]) -> ok.
spawn_leaf_change_recheck([]) ->
    ok;
spawn_leaf_change_recheck(NewlyNonLeafSuperclasses) ->
    beamtalk_workspace_shape_recheck_worker:enqueue_leaf_change(NewlyNonLeafSuperclasses).

-doc """
Run the leaf-change re-check for `Superclasses` — the *whole* list one
reload event produced — and publish each superclass's own findings.
Reached in production only via `beamtalk_workspace_shape_recheck_worker`'s
queue (mirrors `maybe_trigger_shape_recheck/1` exactly — see its doc — which
is also why this is exported outside the `-ifdef(TEST)` block, same reason:
the worker lives in a different module).

`beamtalk_recheck:trigger_leaf_change/1` runs **one** shared
whole-image sweep covering every superclass in `Superclasses` (never raises
— see its own doc), instead of one independent sweep per superclass
(`spawn_leaf_change_recheck/1`'s doc explains why paying for N sweeps when
N superclasses transition in the same reload was wasteful: every sweep
would recompile the identical candidate set against the identical,
already-updated hierarchy). Publishing still happens once per superclass
(`publish_leaf_change_recheck_outcome/2` filters the shared result's
findings down to just the ones attributed to its own superclass), preserving
the existing one-`'ReloadCheckCompleted'`-announcement-per-superclass
contract every consumer already relies on — each publish call is
independently try/catch-wrapped so a failure publishing one superclass's
outcome cannot prevent another's from being published.
""".
-spec maybe_trigger_leaf_change_recheck([binary()]) -> ok.
maybe_trigger_leaf_change_recheck([]) ->
    ok;
maybe_trigger_leaf_change_recheck(Superclasses) ->
    Result = beamtalk_recheck:trigger_leaf_change(Superclasses),
    lists:foreach(
        fun(SuperclassBin) -> publish_leaf_change_recheck_outcome_safe(SuperclassBin, Result) end,
        Superclasses
    ),
    ok.

-spec publish_leaf_change_recheck_outcome_safe(binary(), beamtalk_recheck:result()) -> ok.
publish_leaf_change_recheck_outcome_safe(SuperclassBin, Result) ->
    try
        publish_leaf_change_recheck_outcome(SuperclassBin, Result)
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Leaf-change re-check publish failed (reload unaffected)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    superclass => SuperclassBin,
                    domain => [beamtalk, runtime]
                }
            ),
            ok
    end.

%% Write each checked owner's slice of `Findings` to its
%% `{OwnerBin, OriginBin}` origin bucket, returning `true` when at least one
%% of those writes replaced a *previously non-empty* bucket — i.e. when this
%% re-check actually cleared or superseded something a surface may still be
%% displaying. Shared by `publish_leaf_change_recheck_outcome/2` and
%% `publish_alias_change_recheck_outcome/2`, whose announce gates both need
%% that signal (see their docs for why "did we displace anything" is the
%% right gate for them, where `publish_shape_recheck_outcome/3`'s "were any
%% owners checked" is right for its own capped, xref-derived candidate set).
%%
%% Deliberately *not* narrowed to "cleared to empty" (PR #3116 review): the
%% flag covers a supersession (non-empty replaced by different non-empty)
%% too. That case is redundant with the callers' own `Findings =/= []` half
%% and so never changes an outcome — but narrowing it would mean reading
%% each new bucket back to classify the write, for no behavioural gain.
%%
%% Every owner is written unconditionally — the fold never short-circuits,
%% so the ADR 0105 "replacement, not just agreement" rule still applies to
%% every checked owner regardless of what earlier owners returned.
-spec put_owner_origins_replacing_any(
    [binary()], binary(), [beamtalk_recheck:finding()]
) -> boolean().
put_owner_origins_replacing_any(CheckedOwners, OriginBin, Findings) ->
    lists:foldl(
        fun(OwnerBin, ReplacedAny) ->
            OwnerFindings = [F || F <- Findings, maps:get(owner, F) =:= OwnerBin],
            Prev = findings_store_put_owner_origin(OwnerBin, OriginBin, OwnerFindings),
            ReplacedAny orelse Prev =/= []
        end,
        false,
        CheckedOwners
    ).

-doc """
Publish a leaf-change re-check's outcome for `SuperclassBin` — mirrors
`publish_shape_recheck_outcome/3`'s store-write/announce shape, reusing the
exact same findings-store and `'ReloadCheckCompleted'` announcement schema
so every existing surface renders a leaf-change finding without new wiring.
`changedSelector` carries `SuperclassBin` itself (there is no single call
site selector this is "about" — same reasoning `finding()`'s doc already
gives for `shape_change`).

`Result` may be a *shared* outcome covering several
superclasses at once (`maybe_trigger_leaf_change_recheck/1`'s batched
sweep) — `Findings` is filtered down to just the ones whose own
`changed_class` is `SuperclassBin` before anything else runs, so a finding
attributed to a sibling superclass in the same batch never leaks into this
superclass's findings-store origin or announcement. `CheckedOwners`/
`Checked`/`NotChecked`/`CapNote` are **not** filtered — they describe the
one shared diagnostics sweep itself (identical for every superclass in the
batch, since it is literally the same round trip), matching exactly what an
unbatched single-superclass sweep would have reported for this superclass
alone.

The announce fires when this re-check has something for a surface to act
on: either it produced `Findings`, or its store writes displaced a
previously non-empty `{Owner, SuperclassBin}` origin bucket that a surface
may still be displaying (`ReplacedNonEmpty` —
`findings_store_put_owner_origin/3` hands back each previous bucket, so
this costs no extra store round trip).

Deliberately **not** `publish_shape_recheck_outcome/3`'s "announce whenever
`CheckedOwners` is non-empty" gate (PR #2965 review, which suggested
mirroring it): that gate is proportionate for a shape re-check, whose
`checked_owners` is a *capped, xref-derived* set of that change's actual
dependents — non-empty there genuinely means "we checked this change's
dependents". A leaf-change sweep's candidate set is the **entire live
image** (`beamtalk_recheck:do_trigger_leaf_change/1` reads
`beamtalk_workspace_meta:all_class_sources/0`, uncapped — see
`trigger_leaf_change/1`'s doc for why there is no xref candidate set to
scope it), so `CheckedOwners =/= []` degenerates to "some class exists" and
would broadcast a whole-image `checkedOwners` list to every subscriber on
every first-subclass event, the overwhelming majority of which find and
clear nothing. Gating on the clearing signal itself keeps the review's fix
(a cleared finding always reaches the surface displaying it) without that
amplification.

A superclass cannot *regain* leaf status through this path (losing leaf
status is monotonic short of `removeFromSystem`), so there is no
"reload fixes reload" case symmetrical to a method/shape edit — but the
clean-sweep write can still clear a bucket an *earlier* re-check of the
same class left behind, since method/shape re-checks of `SuperclassBin`
write under the exact same `{Owner, ChangedClass}` key this sweep replaces.
The `?LOG_INFO` stays gated on `Findings` alone (nothing worth logging
about a re-check that only cleared).
""".
-spec publish_leaf_change_recheck_outcome(binary(), beamtalk_recheck:result()) -> ok.
publish_leaf_change_recheck_outcome(SuperclassBin, Result) ->
    #{
        checked_owners := CheckedOwners,
        findings := AllFindings,
        checked := Checked,
        not_checked := NotChecked,
        cap_note := CapNote,
        not_verified_owners := NotVerifiedOwners
    } = Result,
    Findings = [F || F <- AllFindings, maps:get(changed_class, F) =:= SuperclassBin],
    ReplacedNonEmpty = put_owner_origins_replacing_any(
        CheckedOwners, SuperclassBin, Findings
    ),
    mark_unverified_findings_stale(SuperclassBin, NotVerifiedOwners),
    case Findings =/= [] orelse ReplacedNonEmpty of
        false ->
            ok;
        true ->
            case Findings of
                [] ->
                    ok;
                _ ->
                    ?LOG_INFO(
                        "Leaf-change reload re-check produced findings",
                        #{
                            superclass => SuperclassBin,
                            callers_checked => Checked,
                            callers_not_checked => NotChecked,
                            finding_count => length(Findings),
                            domain => [beamtalk, runtime]
                        }
                    )
            end,
            beamtalk_announcements:system_announce('ReloadCheckCompleted', #{
                changedClass => SuperclassBin,
                changedSelector => SuperclassBin,
                classification => leaf_change,
                checked => Checked,
                notChecked => NotChecked,
                capNote => CapNote,
                checkedOwners => CheckedOwners,
                findings => Findings
            }),
            ok
    end.

%% ── ADR 0108 hot-reload re-check trigger: alias-change re-check ──

-doc """
Hand `AliasNameBins` off to the shape-recheck worker's queue and return
immediately — reuses `beamtalk_workspace_shape_recheck_worker`'s existing
single-worker serialisation, exactly like `spawn_leaf_change_recheck/1`
(see its doc): `beamtalk_recheck:trigger_alias_change/1` is a batch of
`diagnostics/3` round trips, same contention profile as a shape/leaf-change
re-check.

`AliasNameBins` is expected to be the redefined alias name — as a
single-element list for the ordinary case — with the *primary* (actually
redefined) name first if a caller ever has more than one (this trigger's
one production caller, `beamtalk_repl_eval:handle_type_alias_definition/3`,
always passes exactly one).

A no-op when the list is empty.
""".
-spec spawn_alias_change_recheck([binary()]) -> ok.
spawn_alias_change_recheck([]) ->
    ok;
spawn_alias_change_recheck(AliasNameBins) ->
    beamtalk_workspace_shape_recheck_worker:enqueue_alias_change(AliasNameBins).

-doc """
Run the alias-change re-check for `AliasNameBins` and publish the outcome.
Reached in production only via `beamtalk_workspace_shape_recheck_worker`'s
queue (mirrors `maybe_trigger_leaf_change_recheck/1` exactly — see its doc —
which is also why this is exported outside the `-ifdef(TEST)` block, same
reason: the worker lives in a different module).

Unlike the leaf-change trigger's batch-of-superclasses shape, this always
publishes against the *first* (primary) name in `AliasNameBins` — see
`recheck_owner_for_alias_change/2`'s doc for why a finding is always
attributed to the redefined alias itself, not a transitively-affected one.
""".
-spec maybe_trigger_alias_change_recheck([binary()]) -> ok.
maybe_trigger_alias_change_recheck([]) ->
    ok;
maybe_trigger_alias_change_recheck([PrimaryAliasBin | _] = AliasNameBins) ->
    Result = beamtalk_recheck:trigger_alias_change(AliasNameBins),
    publish_alias_change_recheck_outcome_safe(PrimaryAliasBin, Result),
    ok.

-spec publish_alias_change_recheck_outcome_safe(binary(), beamtalk_recheck:result()) -> ok.
publish_alias_change_recheck_outcome_safe(AliasNameBin, Result) ->
    try
        publish_alias_change_recheck_outcome(AliasNameBin, Result)
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Alias-change re-check publish failed (redefinition unaffected)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    alias => AliasNameBin,
                    domain => [beamtalk, runtime]
                }
            ),
            ok
    end.

-doc """
Publish an alias-change re-check's outcome for `AliasNameBin` — mirrors
`publish_leaf_change_recheck_outcome/2`'s store-write/announce shape,
reusing the exact same findings-store and `'ReloadCheckCompleted'`
announcement schema so every existing surface renders an alias-change
finding without new wiring. `changedSelector` carries `AliasNameBin` itself
(there is no single call-site selector this is "about" — same reasoning
`finding()`'s doc already gives for `shape_change`/`leaf_change`).

Announce is gated on "produced findings **or** cleared something", matching
`publish_leaf_change_recheck_outcome/2` (see its doc for why that is the
right gate here and `publish_shape_recheck_outcome/3`'s `CheckedOwners`
gate is the right one there). This path needs the clearing half more
directly than the leaf one does: unlike a leaf-status transition, an alias
redefinition is *repeatable*, so "redefinition fixes redefinition" is a
first-class scenario — a dependent flagged by one redefinition and
re-checked clean by the next has its store entry cleared by the
unconditional `put_owner_origin(Owner, AliasNameBin, [])` write above, and
a surface needs the announcement itself to know to drop what it was
showing, not just a quiet store write nobody hears about (PR #2965 review).
A redefinition that changes nothing anyone was flagged for stays silent, as
before. The `?LOG_INFO` stays gated on `Findings` alone (nothing worth
logging about a re-check that only cleared).
""".
-spec publish_alias_change_recheck_outcome(binary(), beamtalk_recheck:result()) -> ok.
publish_alias_change_recheck_outcome(AliasNameBin, Result) ->
    #{
        checked_owners := CheckedOwners,
        findings := Findings,
        checked := Checked,
        not_checked := NotChecked,
        cap_note := CapNote,
        not_verified_owners := NotVerifiedOwners
    } = Result,
    ReplacedNonEmpty = put_owner_origins_replacing_any(
        CheckedOwners, AliasNameBin, Findings
    ),
    mark_unverified_findings_stale(AliasNameBin, NotVerifiedOwners),
    case Findings =/= [] orelse ReplacedNonEmpty of
        false ->
            ok;
        true ->
            case Findings of
                [] ->
                    ok;
                _ ->
                    ?LOG_INFO(
                        "Alias-change reload re-check produced findings",
                        #{
                            alias => AliasNameBin,
                            callers_checked => Checked,
                            callers_not_checked => NotChecked,
                            finding_count => length(Findings),
                            domain => [beamtalk, runtime]
                        }
                    )
            end,
            beamtalk_announcements:system_announce('ReloadCheckCompleted', #{
                changedClass => AliasNameBin,
                changedSelector => AliasNameBin,
                classification => alias_change,
                checked => Checked,
                notChecked => NotChecked,
                capNote => CapNote,
                checkedOwners => CheckedOwners,
                findings => Findings
            }),
            ok
    end.

-doc """
A comma-joined, de-duplicated list of every changed slot's name, e.g.
`<<"count, name">>` for a reload that both retyped `count` and removed
`name` — the closest a shape change has to `signature_change`/`removal`'s
single changed selector, for surfaces that render `changedSelector` as
free text.
""".
-spec shape_change_summary([beamtalk_shape_diff:field_change()]) -> binary().
shape_change_summary(FieldChanges) ->
    Names = lists:usort([beamtalk_shape_diff:field_name(FC) || FC <- FieldChanges]),
    iolist_to_binary(lists:join(<<", ">>, Names)).
