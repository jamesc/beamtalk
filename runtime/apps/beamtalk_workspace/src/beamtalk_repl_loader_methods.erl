%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_methods).

%%% **DDD Context:** Workspace Context

-moduledoc """
Single-method install, precheck and removal for the loader (`install_method/8,9`, `precheck_method/4`, `precheck_class_shape/2`, `remove_method/3`): recompiling one method's class, the shape/pending-change prechecks, signature capture and autoflush. Moved verbatim out of `beamtalk_repl_loader` (BT-4907).
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include("beamtalk_repl_loader.hrl").

-export([
    changelog_warnings/1,
    install_method_with_source/10,
    maybe_autoflush/1,
    install_method/8,
    install_method/9,
    remove_method/3,
    precheck_method/4,
    precheck_class_shape/2,
    capture_signature_removal/3,
    splice_out_span/2
]).

-doc """
Install a single method into a class via the structured backend compile
(the rock-solid live-image write-surface idiom — IDE save / `compile:source:` /
MCP `save_method`).

`MethodSource` is the BARE method body (comments and all) or a full
`selector => ...` definition; the backend synthesises the header for a bare
body (BT-4911). It is parsed standalone — no `Class >>` text wrap — so the
stored source round-trips byte-for-byte. The class's
package-qualified module name and on-disk source path are preserved, so the
patched class stays flushable + revertable.
""".
-spec install_method(
    binary(),
    binary(),
    binary(),
    durable | ephemeral,
    binary(),
    human | agent,
    [beamtalk_repl_protocol:warning()],
    beamtalk_repl_state:state()
) ->
    {ok, term(), binary(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()}.
install_method(
    ClassNameBin, SelectorBin, MethodSource, Intent, Author, AuthorKind, Warnings, State
) ->
    %% Default to instance-side for existing callers (`compile:source:` / MCP
    %% `save_method` / IDE save). The side-aware `install_method/9` backs
    %% class-side revert re-installs.
    install_method(
        ClassNameBin, SelectorBin, MethodSource, Intent, Author, AuthorKind, Warnings, State, false
    ).

-doc """
Install a single method into a class, threading the patch side.

`IsClassMethod` selects the side: `false` installs an instance method (the
`compile:source:` / IDE-save default), `true` installs a class-side (static)
method. Backs class-side revert re-installs, which must recompile the class with
the prior class-side body — the instance-side default would synthesise the method
on the wrong side. Otherwise identical to `install_method/8`.
""".
-spec install_method(
    binary(),
    binary(),
    binary(),
    durable | ephemeral,
    binary(),
    human | agent,
    [beamtalk_repl_protocol:warning()],
    beamtalk_repl_state:state(),
    boolean()
) ->
    {ok, term(), binary(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()}.
install_method(
    ClassNameBin,
    SelectorBin,
    MethodSource,
    Intent,
    Author,
    AuthorKind,
    Warnings,
    State,
    IsClassMethod
) ->
    case beamtalk_repl_loader_protocol:is_protocol_name(ClassNameBin) of
        true ->
            %% ADR 0127 §11 (IDE save / `compile:source:` / MCP `save_method`
            %% on a protocol provision): same two-stage protocol patch as the
            %% REPL's `P >> sel =>`.
            beamtalk_repl_loader_protocol:patch_protocol_provision(
                ClassNameBin,
                SelectorBin,
                MethodSource,
                #{
                    is_class_method => IsClassMethod,
                    intent => Intent,
                    author => Author,
                    author_kind => AuthorKind
                },
                Warnings,
                State
            );
        false ->
            install_class_method(
                ClassNameBin,
                SelectorBin,
                MethodSource,
                Intent,
                Author,
                AuthorKind,
                Warnings,
                State,
                IsClassMethod
            )
    end.

-spec install_class_method(
    binary(),
    binary(),
    binary(),
    durable | ephemeral,
    binary(),
    human | agent,
    [beamtalk_repl_protocol:warning()],
    beamtalk_repl_state:state(),
    boolean()
) ->
    {ok, term(), binary(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()}.
install_class_method(
    ClassNameBin,
    SelectorBin,
    MethodSource,
    Intent,
    Author,
    AuthorKind,
    Warnings,
    State,
    IsClassMethod
) ->
    case beamtalk_workspace_meta:get_class_source(ClassNameBin) of
        undefined ->
            ErrorMsg =
                <<"Class source not available for ", ClassNameBin/binary,
                    " (source not recorded or workspace metadata unavailable)">>,
            {error, {compile_error, ErrorMsg}, <<>>, Warnings, State};
        ClassSource ->
            install_method_with_source(
                ClassNameBin,
                SelectorBin,
                MethodSource,
                ClassSource,
                Intent,
                Author,
                AuthorKind,
                Warnings,
                State,
                IsClassMethod
            )
    end.

-spec install_method_with_source(
    binary(),
    binary(),
    binary(),
    string(),
    durable | ephemeral,
    binary(),
    human | agent,
    [beamtalk_repl_protocol:warning()],
    beamtalk_repl_state:state(),
    boolean()
) ->
    {ok, term(), binary(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()}.
install_method_with_source(
    ClassNameBin,
    SelectorBin,
    MethodSource,
    ClassSource,
    Intent,
    Author,
    AuthorKind,
    Warnings,
    State,
    IsClassMethod
) ->
    ClassSourceBin = unicode:characters_to_binary(ClassSource),
    MethodSourceBin = unicode:characters_to_binary(MethodSource),
    {ModuleNameOverride, SourcePath} = beamtalk_repl_loader_support:patch_module_target(
        ClassNameBin
    ),
    SuperclassIndex = beamtalk_repl_compiler:build_class_superclass_index(),
    ModuleIndex = beamtalk_repl_compiler:build_class_module_index(),
    Options = #{
        class_name => ClassNameBin,
        %% The selector being patched: lets the backend synthesise the
        %% `selector => ' header when `MethodSource' is a bare body (BT-4911).
        selector => SelectorBin,
        %% Side comes from the caller: `false` for the instance-side
        %% `compile:source:` / MCP `save_method` / IDE-save chokepoint; `true`
        %% for a class-side revert re-install and for the REPL's `Class class >>
        %% sel` (`reload_method_definition` reads the side from its `MethodInfo`
        %% and calls this function).
        is_class_method => IsClassMethod,
        module_name => ModuleNameOverride,
        source_path => beamtalk_repl_loader_support:source_path_binary(SourcePath),
        class_superclass_index => SuperclassIndex,
        class_module_index => ModuleIndex
    },
    case beamtalk_repl_compiler:compile_method_reload(ClassSourceBin, MethodSourceBin, Options) of
        {ok, #{selector := Selector}} when Selector =/= SelectorBin ->
            %% The body declares a different selector than the caller asked to
            %% patch (e.g. `compile: #foo source: "bar => ..."'). Reject loudly
            %% rather than silently install under the body's selector.
            ErrorMsg =
                <<"Method selector mismatch: asked to compile '", SelectorBin/binary,
                    "' but the source defines '", Selector/binary, "'">>,
            {error, {compile_error, ErrorMsg}, <<>>, Warnings, State};
        {ok, Result} ->
            #{
                binary := Binary,
                module_name := ModName,
                classes := Classes,
                selector := Selector,
                is_class_method := IsClassMethod,
                method_source := CanonicalSource,
                merged_class_source := MergedClassSource,
                warnings := RecompileWarnings
            } = Result,
            %% ADR 0105 Phase 1: declared signature, carried through to
            %% load_recompiled_method's capture-before-install hook.
            ReturnType = maps:get(return_type, Result, <<"Dynamic">>),
            ParamTypes = maps:get(param_types, Result, []),
            MethodInfo = #{
                class_name => ClassNameBin,
                selector => Selector,
                is_class_method => IsClassMethod,
                method_source => CanonicalSource,
                intent => Intent,
                author => Author,
                author_kind => AuthorKind,
                return_type => ReturnType,
                param_types => ParamTypes
            },
            AllWarnings = Warnings ++ RecompileWarnings,
            load_recompiled_method(
                Binary,
                ModName,
                Classes,
                MethodInfo,
                unicode:characters_to_list(MergedClassSource),
                beamtalk_repl_loader_support:source_path_or_empty(SourcePath),
                AllWarnings,
                State
            );
        {error, Reason} ->
            {error, Reason, <<>>, Warnings, State}
    end.

-doc """
Pre-save advisory (ADR 0105 Phase 3): compile a pending method edit
and report would-be-stale dependents **without installing** — the read-only
sibling of `install_method_with_source/10`. Backs `Behaviour>>precheckCompile:
source:', the editor/LSP's "check before save" hook (the ADR's Phase 3
steelman accommodation: the post-reload image check stays the authority;
this is a non-blocking early warning against the *pending* edit).

Shares `install_method_with_source/10`'s compile step
(`beamtalk_repl_compiler:compile_method_reload/3`) up to the point that
function would call `load_recompiled_method/8` — this function stops there
and never calls `code:load_binary/3`, never emits a ChangeLog entry, and
never touches `beamtalk_workspace_signature_store`'s recorded generation
(`previous/3` is a **read-only** peek, unlike the real install's `capture/4`,
which would consume a generation slot for an edit that never happened).

Diffs the pending signature against `beamtalk_workspace_signature_store:
previous/3` (the same baseline a real install would capture against) via the
same `beamtalk_signature_diff:diff/2` classification `capture/4` uses; a
`no_op` diff (nothing type-relevant changed, or no baseline to compare
against) short-circuits to an empty report — there is nothing pending worth
dependent-checking. Otherwise delegates to `beamtalk_recheck:trigger_pending/5`.

Returns `{ok, beamtalk_recheck:result()}` on a successful compile (`result()`
is empty for a `no_op` diff) or `{error, Reason}` on a compile failure — the
same failure shape `install_method_with_source/10` returns, since the
compile step is identical.
""".
-spec precheck_method(binary(), binary(), binary(), boolean()) ->
    {ok, beamtalk_recheck:result()} | {error, term()}.
precheck_method(ClassNameBin, SelectorBin, MethodSource, IsClassMethod) ->
    case beamtalk_workspace_meta:get_class_source(ClassNameBin) of
        undefined ->
            ErrorMsg =
                <<"Class source not available for ", ClassNameBin/binary,
                    " (source not recorded or workspace metadata unavailable)">>,
            {error, {compile_error, ErrorMsg}};
        ClassSource ->
            precheck_method_with_source(
                ClassNameBin, SelectorBin, MethodSource, ClassSource, IsClassMethod
            )
    end.

-spec precheck_method_with_source(binary(), binary(), binary(), string(), boolean()) ->
    {ok, beamtalk_recheck:result()} | {error, term()}.
precheck_method_with_source(ClassNameBin, SelectorBin, MethodSource, ClassSource, IsClassMethod) ->
    ClassSourceBin = unicode:characters_to_binary(ClassSource),
    MethodSourceBin = unicode:characters_to_binary(MethodSource),
    {ModuleNameOverride, SourcePath} = beamtalk_repl_loader_support:patch_module_target(
        ClassNameBin
    ),
    SuperclassIndex = beamtalk_repl_compiler:build_class_superclass_index(),
    ModuleIndex = beamtalk_repl_compiler:build_class_module_index(),
    Options = #{
        class_name => ClassNameBin,
        selector => SelectorBin,
        is_class_method => IsClassMethod,
        module_name => ModuleNameOverride,
        source_path => beamtalk_repl_loader_support:source_path_binary(SourcePath),
        class_superclass_index => SuperclassIndex,
        class_module_index => ModuleIndex
    },
    case beamtalk_repl_compiler:compile_method_reload(ClassSourceBin, MethodSourceBin, Options) of
        {ok, #{selector := Selector}} when Selector =/= SelectorBin ->
            ErrorMsg =
                <<"Method selector mismatch: asked to precheck '", SelectorBin/binary,
                    "' but the source defines '", Selector/binary, "'">>,
            {error, {compile_error, ErrorMsg}};
        {ok, Result} ->
            #{
                selector := Selector,
                is_class_method := ActualIsClassMethod
            } = Result,
            ReturnType = maps:get(return_type, Result, <<"Dynamic">>),
            ParamTypes = maps:get(param_types, Result, []),
            Side = ?ENTRIES:patch_side(ActualIsClassMethod),
            PendingSignature = #{return_type => ReturnType, param_types => ParamTypes},
            Prev = beamtalk_workspace_signature_store:previous(ClassNameBin, Selector, Side),
            case beamtalk_signature_diff:diff(Prev, PendingSignature) of
                no_op ->
                    {ok, no_pending_change_result()};
                Classification ->
                    {ok,
                        beamtalk_recheck:trigger_pending(
                            ClassNameBin, Selector, Side, Classification, PendingSignature
                        )}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Mirrors `beamtalk_recheck`'s internal `empty_result/0` shape (not exported
%% — this is the "nothing pending is worth checking" case, distinct from that
%% module's "the check ran and found nothing").
-spec no_pending_change_result() -> beamtalk_recheck:result().
no_pending_change_result() ->
    #{
        findings => [],
        checked => 0,
        total_candidates => 0,
        not_checked => 0,
        cap_note => undefined,
        checked_owners => [],
        not_checked_owners => [],
        not_verified_owners => []
    }.

-doc """
Pre-save advisory (ADR 0105 Phase 3, ADR 0123 §4 BT-3538): compile a pending
**full class-body** edit and report the "dropped/retyped without a
`shapeVersion:` bump" and version-drift findings
(`beamtalk_shape_diff:reload_findings/4`) it would produce **without
installing** — the class-body-level sibling of `precheck_method/4`, which
only ever sees a single method.

## Why this cannot reuse `precheck_method/4`'s mechanism

A method precheck reads its pending signature straight off the compiler
port's structured response (`return_type`/`param_types` fields on a
`method_definition` result). A **class** compile's structured response
carries only `name`/`superclass` per class
(`beamtalk_repl_compiler:compile_file/4`'s spec); `field_types`/
`'shape_version'`/`'shape_migrations'` are Rust `class_meta.rs` codegen,
emitted only into the compiled module's `__beamtalk_meta/0`. This compiles
`PendingSource` via `beamtalk_compiler:compile/2` directly (not
`beamtalk_repl_compiler:compile_file/4`, which also calls
`register_alias_xref_for_classes/3` in `replace` mode keyed to the class's
real name) and reads that meta off the resulting **Core Erlang text** with
`beamtalk_compiler:class_meta_from_core_erlang/1`.

## Read-only by construction (BT-4285)

Nothing is compiled to BEAM and nothing is loaded. Loading a generated class
module always runs its `on_load` `register_class/0`, which registers the
class it defines with the live runtime: an earlier design loaded the pending
compile under a scratch module name to call its `__beamtalk_meta/0`, and
that `on_load` rebound the *live* class onto the scratch module (then
deleted it), recorded a collision warning, re-published reflection/xref and
announced `ClassLoaded`. Reading the meta from the Core Erlang leaves the
live class, the code server and the atom table untouched, and needs no
module name, no lock and no cleanup, so concurrent prechecks are
independent.

## Scope

Only the field-diff/version-drift findings (`reload_findings/4`) apply
pre-save — `instances_suspended` (row 5) reports a migration that already
ran, which cannot have happened yet for an edit that has not installed.
Returns `{ok, []}` (not an error) when `ClassNameBin` has no previous
generation recorded this session (nothing to compare against, mirrors
`beamtalk_shape_diff:diff/2`'s own "no baseline" `no_op` rule) or when the
pending source fails to compile — a `{compile_error, ...}` from this
advisory-only precheck is swallowed to `{ok, []}` rather than surfaced as a
blocking error, since a syntactically-broken pending edit already gets its
own diagnostics from the editor's live parse-diagnostics op
(`beamtalk_repl_ops_dev:diagnostics_for/3`) — this precheck adding a second,
differently-worded compile-failure report would be redundant, not helpful.

The baseline is `beamtalk_workspace_shape_store:previous/1`, which an
install records asynchronously (`spawn_shape_recheck/1` casts the capture
to `beamtalk_workspace_shape_recheck_worker`). A precheck sent immediately
after a class's *first* install would therefore race the worker and could
still see no baseline and return `{ok, []}` (BT-3945), so this drains the
worker's queue first (`beamtalk_workspace_shape_recheck_worker:sync/1`,
best-effort: a stopped worker or a congested backlog simply falls back to
whatever the store currently holds), and only then reads the baseline. A
`{ok, []}` from here therefore means "genuinely nothing recorded", not
"the install's capture has not run yet".
""".
-spec precheck_class_shape(binary(), binary()) ->
    {ok, [beamtalk_shape_diff:reload_finding()]}.
precheck_class_shape(ClassNameBin, PendingSource) ->
    drain_shape_recheck_worker(),
    case beamtalk_workspace_shape_store:previous(ClassNameBin) of
        undefined ->
            {ok, []};
        PrevGen ->
            {ok, precheck_class_shape_against(ClassNameBin, PrevGen, PendingSource)}
    end.

%% Best-effort drain of `beamtalk_workspace_shape_recheck_worker` (BT-3945):
%% a `sync/0` call returns only after the worker has handled every cast
%% queued ahead of it (including this session's install captures), so the
%% `previous/1` read in `precheck_class_shape/2` cannot observe a
%% not-yet-captured baseline. Never fails the precheck: when the worker is
%% not running (no live reload path) or the queue is congested past the
%% call timeout, fall back to whatever the store currently holds. The
%% timeout is short (BT-4382): a precheck is an interactive, advisory
%% query, so a long re-check backlog must not stall it for the
%% `gen_server:call` default of 5 s.
-define(PRECHECK_DRAIN_TIMEOUT_MS, 1000).

drain_shape_recheck_worker() ->
    try beamtalk_workspace_shape_recheck_worker:sync(?PRECHECK_DRAIN_TIMEOUT_MS) of
        ok -> ok
    catch
        exit:_ -> ok;
        error:_ -> ok
    end.

-spec precheck_class_shape_against(binary(), beamtalk_shape_diff:generation(), binary()) ->
    [beamtalk_shape_diff:reload_finding()].
precheck_class_shape_against(ClassNameBin, PrevGen, PendingSource) ->
    try
        SourceBin = unicode:characters_to_binary(PendingSource),
        Options = beamtalk_repl_compiler:build_class_indexes(),
        case beamtalk_compiler:compile(SourceBin, Options) of
            {ok, #{core_erlang := CoreErlang}} ->
                case beamtalk_compiler:class_meta_from_core_erlang(CoreErlang) of
                    {ok, Meta} -> pending_findings_from_meta(ClassNameBin, PrevGen, Meta);
                    not_available -> []
                end;
            _NotAClassDefinitionOrError ->
                []
        end
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Pre-save shape precheck failed (advisory only, no findings reported)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    class => ClassNameBin,
                    domain => [beamtalk, runtime]
                }
            ),
            []
    end.

%% Diff the *pending* generation described by `Meta` (a pending compile's
%% `__beamtalk_meta/0` map) against `PrevGen`. `PrevGen`'s `ancestor_shape`
%% (BT-3560) — captured directly by `beamtalk_workspace_shape_store`, never
%% derived by subtracting `own_shape` back out of the flattened `shape`
%% (lossy whenever a field is shadowed — see
%% `beamtalk_shape_diff:generation/0`'s doc) — is the ancestor contribution
%% (which this same-class edit cannot itself have changed); the pending
%% edit's own field types are merged on top of it: exactly
%% `beamtalk_workspace_shape_store`'s own ancestor-then-own-class merge
%% precedence, without re-walking the ancestor chain.
-spec pending_findings_from_meta(binary(), beamtalk_shape_diff:generation(), map()) ->
    [beamtalk_shape_diff:reload_finding()].
pending_findings_from_meta(ClassNameBin, PrevGen, Meta) ->
    #{shape := PrevShape, ancestor_shape := AncestorOnlyShape} = PrevGen,
    PendingOwnFieldTypes = maps:get(field_types, Meta, #{}),
    PendingOwnFieldKinds = maps:get(field_kinds, Meta, #{}),
    PendingOwnShape = beamtalk_workspace_shape_store:normalize_shape(
        PendingOwnFieldTypes, PendingOwnFieldKinds
    ),
    PendingShape = maps:merge(AncestorOnlyShape, PendingOwnShape),
    NewGen = #{
        shape => PendingShape,
        own_shape => PendingOwnShape,
        ancestor_shape => AncestorOnlyShape,
        version => maps:get(shape_version, Meta, 1),
        migrations => maps:get(shape_migrations, Meta, #{})
    },
    DiffResult = beamtalk_shape_diff:diff(PrevShape, PendingShape),
    beamtalk_shape_diff:reload_findings(ClassNameBin, PrevGen, NewGen, DiffResult).

-doc """
Remove a live method from a class by recompiling the class without it.
Backs the *add* revert case: when a freshly-added method (instance or
class side) is reverted, its pre-patch state was "absent", so the revert deletes
the method rather than re-installing a prior body.

`Side` (`instance | class`) selects which side's method to drop. The method's
span is resolved against the class's CURRENT in-memory merged source, spliced out,
and the remaining class source is recompiled + hot-reloaded. Returns
`{ok, ClassNameBin}` on success or `{error, Reason}` if the class source is
unavailable, the selector cannot be located, or the recompile/reload fails (the
live image is unchanged in the error cases). The removal does NOT itself emit a
ChangeEntry — the caller curtails the original add entry separately (a
`revert:`-of-an-add), or, for `removeSelector:` (ADR 0112 Phase 3),
calls `emit_remove_change_entry/5` afterward.
""".
-spec remove_method(binary(), atom() | binary(), instance | class) ->
    {ok, binary()} | {error, term()}.
remove_method(ClassNameBin, Selector, Side) ->
    SelectorBin = ?ENTRIES:method_selector_binary(Selector),
    case beamtalk_repl_loader_protocol:is_protocol_name(ClassNameBin) of
        true ->
            beamtalk_repl_loader_protocol:remove_protocol_provision(
                ClassNameBin, SelectorBin, Side
            );
        false ->
            remove_class_method(ClassNameBin, SelectorBin, Side)
    end.

-spec remove_class_method(binary(), binary(), instance | class) ->
    {ok, binary()} | {error, term()}.
remove_class_method(ClassNameBin, SelectorBin, Side) ->
    case beamtalk_workspace_meta:get_class_source(ClassNameBin) of
        undefined ->
            {error,
                {compile_error,
                    <<"Class source not available for ", ClassNameBin/binary,
                        " (cannot remove method)">>}};
        ClassSource ->
            ClassSourceBin = unicode:characters_to_binary(ClassSource),
            case
                beamtalk_compiler:resolve_method_span(
                    ClassSourceBin, ClassNameBin, SelectorBin, Side
                )
            of
                {ok, Span, _Body} ->
                    %% ADR 0105 Phase 1: record the removal in the
                    %% signature-generation store BEFORE the recompile-without-
                    %% the-method installs (mirrors capture_signature_generation/1
                    %% in load_recompiled_method/8 — this IS the install for a
                    %% deletion, and must run beforehand so a first-ever capture
                    %% still seeds the method's pre-removal signature from
                    %% __beamtalk_meta/0 rather than the just-recompiled module,
                    %% which no longer has this selector at all). Rolled back
                    %% below if the recompile fails, so a failed removal never
                    %% poisons the store with a removal that didn't happen.
                    RemovalCapture = capture_signature_removal(ClassNameBin, SelectorBin, Side),
                    NewSourceBin = splice_out_span(ClassSourceBin, Span),
                    case reload_class_without_method(ClassNameBin, NewSourceBin) of
                        {ok, _} = Ok ->
                            %% ADR 0105 Phase 1: the removal is
                            %% live — re-check known dependents (mirrors the
                            %% install success path in load_recompiled_method/8,
                            %% including that function's ordering-invariant
                            %% comment: reload_class_without_method above ran
                            %% through reload_compile_and_load's synchronous
                            %% activate_module/3 call before returning here, so
                            %% the compiler port's ambient class cache already
                            %% reflects this removal by the time
                            %% maybe_trigger_recheck's diagnostics/3 call reaches
                            %% it — same invariant, same fragility if that
                            %% registration path ever becomes async).
                            beamtalk_repl_loader_recheck:maybe_trigger_recheck(
                                ClassNameBin, SelectorBin, Side, RemovalCapture
                            ),
                            Ok;
                        {error, _} = Error ->
                            beamtalk_repl_loader_recheck:rollback_signature_generation(
                                ClassNameBin, SelectorBin, Side, RemovalCapture
                            ),
                            Error
                    end;
                {error, Reason, Msg} ->
                    {error, {method_span_unresolved, Reason, Msg}}
            end
    end.

%% Record a method removal into the signature-generation store (ADR 0105 Phase
%% 1). Best-effort and self-swallowing, mirroring
%% capture_signature_generation/1 — a store failure must never block the
%% removal itself. Returns the same beamtalk_repl_loader_recheck:capture_outcome() so
%% the caller can roll back on a subsequent recompile failure.
-spec capture_signature_removal(binary(), binary(), instance | class) ->
    beamtalk_repl_loader_recheck:capture_outcome().
capture_signature_removal(ClassNameBin, SelectorBin, Side) ->
    try
        {Prev, Classification} = beamtalk_workspace_signature_store:capture(
            ClassNameBin, SelectorBin, Side, removed
        ),
        {captured, Prev, Classification}
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Failed to capture method-removal signature generation (removal proceeding)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    class => ClassNameBin,
                    selector => SelectorBin,
                    domain => [beamtalk, runtime]
                }
            ),
            not_captured
    end.

%% Recompile the (method-removed) class source and hot-reload it, preserving the
%% class's package-qualified module name and on-disk source path so it stays a
%% project class. Updates the workspace_meta class-source cache so subsequent
%% patches resolve against the new (shorter) source.
-spec reload_class_without_method(binary(), binary()) -> {ok, binary()} | {error, term()}.
reload_class_without_method(ClassNameBin, NewSourceBin) ->
    {ModuleNameOverride, SourcePath} = beamtalk_repl_loader_support:patch_module_target(
        ClassNameBin
    ),
    LoadPath = beamtalk_repl_loader_support:source_path_or_empty(SourcePath),
    NewSourceStr = unicode:characters_to_list(NewSourceBin),
    case
        beamtalk_repl_loader_reload:reload_compile_and_load(
            NewSourceStr, LoadPath, ModuleNameOverride, undefined
        )
    of
        {ok, ClassNames} ->
            lists:foreach(
                fun(#{name := Name}) ->
                    beamtalk_workspace_meta:set_class_source(
                        beamtalk_repl_loader_support:normalize_class_source_key(Name), NewSourceStr
                    )
                end,
                ClassNames
            ),
            {ok, ClassNameBin};
        {error, Reason} ->
            {error, Reason}
    end.

%% Cut the bytes `[start, end)' out of `Source', joining the surrounding text.
%% Removal is replacement with the empty binary (`beamtalk_workspace_span:splice/3`).
-spec splice_out_span(binary(), beamtalk_repl_loader_rewrite:rewrite_span()) -> binary().
splice_out_span(Source, Span) ->
    beamtalk_workspace_span:splice(Source, Span, <<>>).
%% Load a recompiled method-patched class binary into BEAM.
-spec load_recompiled_method(
    binary(), atom(), list(), map(), string(), string(), [binary()], beamtalk_repl_state:state()
) ->
    {ok, term(), binary(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()}.
load_recompiled_method(
    Binary,
    ModName,
    Classes,
    MethodInfo,
    CombinedSource,
    SourcePath,
    AllWarnings,
    State
) ->
    #{class_name := ClassNameBin, selector := SelectorBin} = MethodInfo,
    Side = ?ENTRIES:patch_side(maps:get(is_class_method, MethodInfo, false)),
    %% ADR 0105 Phase 1: capture the freshly-compiled signature into
    %% the signature-generation store BEFORE the patch installs. Must run here
    %% (not after code:load_binary below) — install reloads the class's
    %% compiled module under its *existing* atom, so a first-ever capture made
    %% after install would seed from the just-installed module's own
    %% __beamtalk_meta/0 (comparing the new generation against itself) instead
    %% of the true pre-patch original. Rolled back on the {error, LoadReason}
    %% branch below, so a load failure never leaves the store holding a
    %% generation that was never actually live. Best-effort: a store failure
    %% must never block the install.
    CaptureOutcome = beamtalk_repl_loader_recheck:capture_signature_generation(MethodInfo),
    %% Pass the class's on-disk source path (when known) so `code:which/1`
    %% reports a real path — keeping a patched project class classified as a
    %% project class, not "stdlib"/"dynamic".
    %% BT-5015: activation (`register_class/0`) is part of the install — a
    %% failure there is reported like a failed load, not as a live patch.
    InstallResult =
        case
            beamtalk_repl_loader_support:load_binary_scoped(ModName, SourcePath, Binary, Classes)
        of
            {module, ModName} ->
                case beamtalk_repl_loader_activate:activate_module(ModName, Classes) of
                    ok -> {module, ModName};
                    {error, _} = ActivateError -> {activation_failed, ActivateError}
                end;
            LoadError ->
                LoadError
        end,
    case InstallResult of
        {module, ModName} ->
            %% (2) Install in memory. The memory install is the visible effect;
            %% the ChangeEntry below is step (3) — emitted only after install
            %% succeeds (all-or-nothing between install and log, ADR 0082).
            %% Update all classes compiled in this module so sibling class entries
            %% reflect the latest combined source and stay consistent for future >> calls.
            lists:foreach(
                fun(#{name := Name}) ->
                    NameBin = beamtalk_repl_loader_support:normalize_class_source_key(Name),
                    beamtalk_workspace_meta:set_class_source(NameBin, CombinedSource)
                end,
                Classes
            ),
            %% (3) Emit a ChangeEntry for the live patch (ADR 0082 Phase 1).
            %% Best-effort: a ChangeLog failure must never fail the install — the
            %% method is already live in memory. emit_change_entry/1 logs and
            %% swallows its own errors.
            ChangeLogWarnings = changelog_warnings(?ENTRIES:emit_change_entry(MethodInfo)),
            %% (4) ADR 0082 Phase 4: when the workspace is in `autoflush: true'
            %% mode, every successful durable in-memory patch is immediately
            %% flushed to disk. Best-effort and synchronous; a flush failure does
            %% NOT roll back the BEAM module install (prior binary may already be
            %% unloaded and live actors may hold references to the new closures)
            %% — the entry simply stays pending in the log for manual flush
            %% reconciliation. Ephemeral patches are not autoflushed because
            %% only durable+flushable entries are written by `flush/0'.
            maybe_autoflush(maps:get(intent, MethodInfo, durable)),
            %% (5) ADR 0105 Phase 1: re-check known dependents of a
            %% signature_change/removal now that the new generation is live.
            %% Best-effort, never affects this reply's *content* — see the
            %% function doc — but it IS synchronous here, so it does delay
            %% this reply by the re-check's wall time (bounded by the caller
            %% cap; ~18.5ms/candidate warm per the Phase 0 spike, so normally
            %% sub-second even at the default cap of 20). Moving this off the
            %% install's critical path is deferred until findings
            %% have somewhere to go (publish/clearing across surfaces).
            %%
            %% Ordering invariant this relies on: beamtalk_recheck's re-check
            %% needs the compiler port's ambient class cache
            %% (beamtalk_compiler_server's `classes` map) to already reflect
            %% THIS class's new signature. activate_module/2 above is
            %% synchronous — it runs the freshly-loaded module's
            %% register_class/0, which (via beamtalk_object_class:start/2,
            %% ADR 0050 Phase 4) casts the new metadata to
            %% beamtalk_compiler_server *before* activate_module returns here
            %% — so by the time maybe_trigger_recheck's diagnostics/3 call
            %% reaches that same gen_server, the cast is already enqueued
            %% ahead of it. This holds because activate_module blocks on
            %% class registration; it would break if that registration ever
            %% became async relative to this call site.
            beamtalk_repl_loader_recheck:maybe_trigger_recheck(
                ClassNameBin, SelectorBin, Side, CaptureOutcome
            ),
            Result = <<ClassNameBin/binary, ">>", SelectorBin/binary>>,
            {ok, Result, <<>>, AllWarnings ++ ChangeLogWarnings, State};
        {activation_failed, {error, ActivationReason}} ->
            beamtalk_repl_loader_recheck:rollback_signature_generation(
                ClassNameBin, SelectorBin, Side, CaptureOutcome
            ),
            {error, ActivationReason, <<>>, AllWarnings, State};
        {error, LoadReason} ->
            %% ADR 0105 Phase 1: the install this capture described
            %% never happened — undo it so the store still reflects the actually
            %% live generation.
            beamtalk_repl_loader_recheck:rollback_signature_generation(
                ClassNameBin, SelectorBin, Side, CaptureOutcome
            ),
            ClassAtoms = beamtalk_repl_loader_support:class_name_atoms(Classes),
            case beamtalk_runtime_api:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError, <<>>, AllWarnings, State};
                [] ->
                    {error, {load_error, LoadReason}, <<>>, AllWarnings, State}
            end
    end.

%% Reply warnings for a ChangeLog emit that did not record its entry (BT-5013).
-spec changelog_warnings(beamtalk_repl_loader_changelog_entries:emit_result()) -> [binary()].
changelog_warnings(ok) -> [];
changelog_warnings({warning, Text}) -> [Text].

%% Trigger `Workspace flush' when `autoflush: true' is set on the workspace
%% (ADR 0082 Phase 4). Best-effort and synchronous:
%%
%%   - Ephemeral patches are never autoflushed (they are not flushable by
%%     definition — only `durable AND flushable' entries are written).
%%   - The flush call itself is wrapped in try/catch so a flush failure (e.g.
%%     external-edit conflict surfaces a conflict-summary, not an exception,
%%     but the ChangeLog server being unreachable would exit the gen_server
%%     call) cannot bubble up and undo the BEAM module install — the patch is
%%     already live in memory.
%%   - The flush is best-effort. A conflict / I/O failure leaves the entry
%%     pending in the log; the user can re-flush manually after reconciling.
%%
%% A successful flush returns a `FlushResult' summary; we log a warning when
%% the summary reports conflicts so an autoflush failure is observable in the
%% workspace log even though the install path returns successfully.
-spec maybe_autoflush(durable | ephemeral) -> ok.
maybe_autoflush(ephemeral) ->
    ok;
maybe_autoflush(durable) ->
    %% A release has no working tree to flush to (ADR 0125 §1.5), even when it
    %% was built with `include_compiler` and so can install a live patch.
    case
        beamtalk_capability:available(autoflush) andalso
            beamtalk_workspace_meta:get_setting(autoflush, false) =:= true
    of
        true -> do_autoflush();
        false -> ok
    end.

-spec do_autoflush() -> ok.
do_autoflush() ->
    try beamtalk_workspace_flush:flush() of
        {ok, #{conflicts := Conflicts} = Summary} when Conflicts =/= [] ->
            ?LOG_WARNING(
                "Autoflush reported conflicts — pending entries remain in the log",
                #{conflicts => Conflicts, summary => Summary, domain => [beamtalk, runtime]}
            ),
            ok;
        {ok, _Summary} ->
            ok;
        {error, Reason} ->
            ?LOG_WARNING(
                "Autoflush returned a structured error (entries remain pending)",
                #{reason => Reason, domain => [beamtalk, runtime]}
            ),
            ok
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Autoflush crashed (entries remain pending; patch still installed)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    domain => [beamtalk, runtime]
                }
            ),
            ok
    end.
