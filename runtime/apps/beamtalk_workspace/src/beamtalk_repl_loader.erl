%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Module loading and class activation for the Beamtalk REPL

Handles loading compiled Beamtalk modules into the BEAM runtime,
registering classes, triggering hot reload, and managing file paths.
Extracted from beamtalk_repl_eval (BT-863).
""".

-export([
    handle_load/2,
    handle_load/3,
    handle_load_source/3,
    load_class_module/3,
    reload_method_definition/4,
    install_method/8,
    install_method/9,
    remove_method/3,
    %% ADR 0112 Phase 3 (BT-3187): best-effort ChangeLog append after a
    %% successful `removeSelector:` call — see remove_method/3's doc for why
    %% this is a separate call rather than folded into remove_method/3 itself.
    emit_remove_change_entry/5,
    emit_extension_remove_change_entry/7,
    %% ADR 0114 (BT-3270): the shared multi-site rewrite mechanism —
    %% generalizes remove_method/3 for `renameTo:`/`renameSelector:to:`
    %% (future issues) to call with different site lists. See
    %% rewrite_sites/2's doc for the in-memory atomicity protocol.
    rewrite_sites/2,
    validate_sites/2,
    emit_rewrite_change_entry/2,
    %% ADR 0114 Phase 2 (BT-3272): `Workspace moveClass:to:` — a pure file
    %% move, reusing rewrite_sites/2 + emit_rewrite_change_entry/2 above with
    %% a single byte-identical definition site. See move_class/2's own doc.
    move_class/2,
    %% ADR 0114 Phase 4 (BT-3274): `Workspace changes revert:` for a pending
    %% `'rename-class'`/`'rename-method'` entry — rewrites every recorded
    %% `sites` entry back to its own `prev_source_ref`, reusing rewrite_sites/2
    %% (BT-3270) in reverse. See revert_rename_sites/1's own doc.
    revert_rename_sites/1,
    %% BT-3206: best-effort snapshot + ChangeLog append for a successful
    %% `removeFromSystem` (class removal) — see
    %% capture_class_removal_snapshot/1's doc for why the snapshot is a
    %% separate call taken before teardown, not folded into the append itself.
    capture_class_removal_snapshot/1,
    emit_remove_class_change_entry/4,
    activate_module/2,
    activate_module/3,
    activate_module/4,
    register_classes/2,
    trigger_hot_reload/2,
    reload_class_file/1,
    reload_class_file/2,
    is_stdlib_path/1,
    to_snake_case/1,
    verify_class_present/3,
    compute_package_module_name/1,
    new_class/2,
    %% ADR 0113 (BT-3208) — `Workspace changes revert:` extension for a pending
    %% `'remove-class'` entry: reinstalls the class from its recorded prior
    %% source, reusing new_class/2's own compile+install chokepoint minus its
    %% target-must-not-exist check (see revert_remove_class/2's doc for why).
    revert_remove_class/2,
    %% ADR 0105 Phase 2 (BT-2780): called cross-module by
    %% beamtalk_workspace_shape_recheck_worker — see activate_module/3's doc.
    maybe_trigger_shape_recheck/1,
    %% BT-2856 / ADR 0107 Phase A: same cross-module reason as
    %% maybe_trigger_shape_recheck/1 above — see activate_module/3's doc.
    maybe_trigger_leaf_change_recheck/1,
    %% ADR 0108 hot-reload re-check trigger (BT-2899): same cross-module
    %% reason as maybe_trigger_leaf_change_recheck/1 above — called from
    %% beamtalk_workspace_shape_recheck_worker via enqueue_alias_change/1.
    maybe_trigger_alias_change_recheck/1,
    %% ADR 0108 hot-reload re-check trigger (BT-2899): the enqueue half —
    %% called from beamtalk_repl_eval:handle_type_alias_definition/3, the
    %% one production site that commits a live alias (re)definition.
    spawn_alias_change_recheck/1,
    precheck_method/4,
    %% BT-3238: the Cockpit section-authoring write path
    %% (`beamtalk_repl_ops_load:save_section/5`) needs the same "which `.bt`
    %% file backs this class, and is it inside the project (safe to write)"
    %% resolution the ADR 0082 install hook already relies on — reused here
    %% rather than re-derived, per the project's no-duplicate-implementations
    %% rule.
    class_source_file/1,
    classify_source_file/1,
    %% BT-3280: exported (not merely -ifdef(TEST)) so `install_rewrite_group/5`
    %% can call it as `?MODULE:install_reload_result/2` — a genuine external
    %% call, needed so `beamtalk_repl_loader_rewrite_sites_tests.erl` can
    %% `meck:new(?MODULE, [passthrough])` + `meck:expect/3` it to exercise
    %% rewrite_sites/2's partial_install_failure path (an install-time
    %% anomaly with no natural trigger against a binary that just compiled
    %% successfully). A LOCAL call here would compile to a direct intra-module
    %% jump that bypasses meck's module-replacement entirely, so this export +
    %% qualification is required in every build, not only test builds — an
    %% `-ifdef(TEST)`-only export would leave the production call site calling
    %% an unexported function and crash with `undef`. See that test module's
    %% own moduledoc for the full seam rationale.
    install_reload_result/2
]).

%% Exported for testing (only in test builds)
-ifdef(TEST).
-export([
    load_compiled_module/6,
    normalize_class_source_key/1,
    extract_trailing_info/1,
    resolve_class_name/1,
    safe_binary_to_atom/1,
    safe_list_to_atom/1,
    safe_atom_result/1,
    resolve_package_module/4,
    try_package_relative/3,
    maybe_add_loaded_module/2,
    store_file_class_sources/3,
    store_class_sources/4,
    %% ADR 0082 Phase 1 (BT-2283): pure helpers behind the install hook.
    is_path_inside/2,
    method_source_binary/1,
    patch_side/1,
    span_error_entry/3,
    new_method_entry/3,
    sibling_method_indent/1,
    %% ADR 0082 Phase 1 (BT-2285): pure validation helpers for new_class/2.
    declared_class_name/1,
    validate_new_class/3,
    validate_target_path/1,
    %% ADR 0105 Phase 1 (BT-2779): the reload-check publish/clear hook.
    maybe_trigger_recheck/4,
    %% BT-2856 / ADR 0107 Phase A: leaf-change detection/publish helpers.
    superclasses_losing_leaf_status/1,
    was_leaf_class/1,
    publish_leaf_change_recheck_outcome/2,
    %% ADR 0108 hot-reload re-check trigger (BT-2899): publish helper.
    publish_alias_change_recheck_outcome/2,
    %% ADR 0082 extension (BT-3248): class-redefinition ChangeLog entry helpers.
    snapshot_class_def_prev_sources/1,
    emit_class_def_entries/3,
    add_class_def_flushability/2,
    %% ADR 0114 Phase 4 (BT-3274): multi-site rewrite revert helpers.
    class_names_by_source_file/0,
    current_spans_for_group/1,
    build_revert_sites/1
]).
-endif.

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ----------------------------------------------------------------------------
%%% Multi-site rewrite types (ADR 0114, BT-3270) — see rewrite_sites/2's doc.
%%% ----------------------------------------------------------------------------

-type rewrite_span() :: #{start := non_neg_integer(), 'end' := non_neg_integer()}.

%% One site a multi-site rewrite must touch: `Class`'s CURRENT
%% `beamtalk_workspace_meta:get_class_source/1` text has `NewText` spliced in
%% at `Span` (the same "current in-memory merged source" convention
%% `remove_method/3` already uses for its own single-site span — NOT
%% necessarily the on-disk byte offsets; resolving a site's span against
%% whatever source a caller's site-discovery step used is that step's own
%% responsibility, out of scope here per BT-3270's issue text).
%% `SourceFile` is carried through only for ChangeLog attribution
%% (`beamtalk_workspace_changelog`'s `site()` shape) — it plays no role in
%% the splice/compile/install mechanism itself.
-type rewrite_site() :: #{
    class := binary(),
    source_file := binary() | undefined,
    span := rewrite_span(),
    new_text := binary()
}.

%% A `rewrite_site()` after a successful `rewrite_sites/2` call: `prev_source`
%% is the text that occupied `span` before the rewrite (sliced from the
%% class's pre-rewrite source) and `source` is `new_text` again, surfaced
%% under the ChangeLog's own field name — together these are exactly a
%% ChangeLog `site()`'s `prev_source_ref`/`source_ref` bodies before they are
%% written to `sources/` and turned into refs (see `emit_rewrite_change_entry/2`).
-type installed_rewrite_site() :: #{
    class := binary(),
    source_file := binary() | undefined,
    span := rewrite_span(),
    prev_source := binary(),
    source := binary()
}.

%% `rewrite_sites/2`'s success result: `definition` is the installed
%% definition site (`undefined` when the caller passed no definition site —
%% mirrors the ChangeLog schema's `sites[0] = null` dynamic-class case) and
%% `sites` is every installed reference/sender site, in the order their
%% owning class-groups were installed (see `rewrite_sites/2`'s doc for why
%% that is not always the caller's exact original interleaving).
-type rewrite_result() :: #{
    definition := installed_rewrite_site() | undefined,
    sites := [installed_rewrite_site()]
}.

-export_type([
    rewrite_span/0,
    rewrite_site/0,
    installed_rewrite_site/0,
    rewrite_result/0
]).

%%% Public API

-doc "Load a Beamtalk file and register its classes.".
-spec handle_load(string(), beamtalk_repl_state:state()) ->
    {ok, [map()], beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
handle_load(Path, State) ->
    case filelib:is_file(Path) of
        false ->
            {error, {file_not_found, Path}, State};
        true ->
            case file:read_file(Path) of
                {error, Reason} ->
                    {error, {read_error, Reason}, State};
                {ok, SourceBin} ->
                    Source = binary_to_list(SourceBin),
                    StdlibMode = is_stdlib_path(Path),
                    ModuleNameOverride = compute_package_module_name(Path),
                    case
                        beamtalk_repl_compiler:compile_file(
                            Source, Path, StdlibMode, ModuleNameOverride
                        )
                    of
                        %% BT-1950: Protocol definition from file compilation.
                        %% Must be matched before the generic 4-tuple to avoid
                        %% {ok, protocol_definition, Info, Warnings} binding to
                        %% {ok, Binary, ClassNames, ModuleName}.
                        {ok, protocol_definition, ProtocolInfo, _Warnings} ->
                            load_protocol_module(ProtocolInfo, Path, State);
                        {ok, Binary, ClassNames, ModuleName} ->
                            load_compiled_module(
                                Binary, ClassNames, ModuleName, Source, Path, State
                            );
                        {error, Reason} ->
                            {error, Reason, State}
                    end
            end
    end.

-doc """
Load a Beamtalk file with pre-built class indexes (BT-1543).

Like `handle_load/2' but accepts pre-built class indexes to avoid
redundant class registry scans during batch loads (e.g. :load dir).
""".
-spec handle_load(string(), beamtalk_repl_state:state(), map()) ->
    {ok, [map()], beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
handle_load(Path, State, PrebuiltIndexes) ->
    case filelib:is_file(Path) of
        false ->
            {error, {file_not_found, Path}, State};
        true ->
            case file:read_file(Path) of
                {error, Reason} ->
                    {error, {read_error, Reason}, State};
                {ok, SourceBin} ->
                    Source = binary_to_list(SourceBin),
                    StdlibMode = is_stdlib_path(Path),
                    ModuleNameOverride = compute_package_module_name(Path),
                    case
                        beamtalk_repl_compiler:compile_file(
                            Source, Path, StdlibMode, ModuleNameOverride, PrebuiltIndexes
                        )
                    of
                        %% BT-1950: Protocol definition — must match before generic 4-tuple.
                        {ok, protocol_definition, ProtocolInfo, _Warnings} ->
                            load_protocol_module(ProtocolInfo, Path, State);
                        {ok, Binary, ClassNames, ModuleName} ->
                            load_compiled_module(
                                Binary, ClassNames, ModuleName, Source, Path, State
                            );
                        {error, Reason} ->
                            {error, Reason, State}
                    end
            end
    end.

-doc "Load Beamtalk source from an inline binary string (no file path).".
-spec handle_load_source(binary(), string(), beamtalk_repl_state:state()) ->
    {ok, [map()], beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
handle_load_source(SourceBin, Label, State) ->
    Source = binary_to_list(SourceBin),
    case beamtalk_repl_compiler:compile_file(Source, Label, false, undefined) of
        %% BT-1950: Protocol definition — must match before generic 4-tuple.
        {ok, protocol_definition, ProtocolInfo, _Warnings} ->
            load_protocol_module(ProtocolInfo, undefined, State);
        {ok, Binary, ClassNames, ModuleName} ->
            load_compiled_module(Binary, ClassNames, ModuleName, Source, undefined, State);
        {error, Reason} ->
            {error, Reason, State}
    end.

-doc """
Load a compiled class module, activate it, and update REPL state.

Returns:
  {ok, ClassName, no_trailing, NewState}    - class loaded, no trailing expressions
  {ok, ClassName, {trailing, ModName, Bin}, NewState} - has trailing expressions to eval
  {error, Reason, NewState}
""".
-spec load_class_module(map(), string(), beamtalk_repl_state:state()) ->
    {ok, term(), no_trailing | {trailing, atom(), binary()}, beamtalk_repl_state:state()}
    | {error, term(), beamtalk_repl_state:state()}.
load_class_module(ClassInfo, Expression, State) ->
    #{binary := Binary, module_name := ClassModName, classes := Classes} = ClassInfo,
    %% ADR 0105 Phase 2 (BT-2780): seed the shape-generation store from the
    %% about-to-be-replaced module's CURRENT __beamtalk_meta/0 before this
    %% class-body reload installs — see
    %% beamtalk_workspace_shape_store's moduledoc "Two-phase capture" for why
    %% this must run before code:load_binary, not after.
    prime_shape_capture(Classes),
    %% BT-3248: snapshot each class's currently-tracked source BEFORE
    %% store_class_sources below overwrites it — see emit_class_def_entries/3's
    %% doc for why a redefinition of an already-loaded class needs the OLD
    %% source captured ahead of the install, not after.
    PrevSources = snapshot_class_def_prev_sources(Classes),
    %% BT-2856 / ADR 0107 Phase A, BT-2873 hardening: load_class_binary/4
    %% bakes the "superclasses_losing_leaf_status/1 before code:load_binary/3"
    %% ordering requirement into one call — see its own doc and
    %% activate_module/4's doc for why.
    case load_class_binary(ClassModName, "", Binary, Classes) of
        {ok, NewlyNonLeafSuperclasses} ->
            activate_module(ClassModName, Classes, undefined, NewlyNonLeafSuperclasses),
            NewState1 = maybe_add_loaded_module(ClassModName, State),
            {ClassName, NewState2} = store_class_sources(
                Classes, ClassModName, Expression, NewState1
            ),
            %% BT-3248: log a pending 'class-def' ChangeLog entry for each
            %% class in this eval that redefined an already-loaded class (the
            %% cockpit `:def` tab's "Compile" action against an *existing*
            %% class). Best-effort: a ChangeLog write must never fail or undo
            %% the in-memory install, which already succeeded above.
            case emit_class_def_entries(Classes, Expression, PrevSources) of
                true -> maybe_autoflush(durable);
                false -> ok
            end,
            TrailingInfo = extract_trailing_info(ClassInfo),
            {ok, ClassName, TrailingInfo, NewState2};
        {error, Reason} ->
            ClassAtoms = class_name_atoms(Classes),
            case beamtalk_runtime_api:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError, State};
                [] ->
                    {error, {load_error, Reason}, State}
            end
    end.

-doc "Recompile and reload a class after a standalone method definition (BT-571).".
-spec reload_method_definition(map(), [binary()], string(), beamtalk_repl_state:state()) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
reload_method_definition(MethodInfo, Warnings, Expression, State) ->
    #{class_name := ClassNameBin} = MethodInfo,
    ExistingSource = beamtalk_workspace_meta:get_class_source(ClassNameBin),
    case ExistingSource of
        undefined ->
            ErrorMsg =
                <<"Class source not available for ", ClassNameBin/binary,
                    " (source not recorded or workspace metadata unavailable)">>,
            {error, {compile_error, ErrorMsg}, <<>>, Warnings, State};
        ClassSource ->
            recompile_with_method(
                ClassSource, MethodInfo, Expression, Warnings, State
            )
    end.

-doc """
BT-2873 (ADR 0107 Phase A caller-discipline hardening, BT-2856 adversarial
review finding #3): load a class-defining module's compiled `Binary` and
compute `superclasses_losing_leaf_status/1` against `Classes` in the one
required order, as a single call.

Every generated class module's `on_load` hook (`register_class/0`,
BT-1610) runs **synchronously inside** `code:load_binary/3` and registers
any new subclass link before that call returns — so
`superclasses_losing_leaf_status/1` MUST run against `Classes` *before*
`code:load_binary/3`, never after (`activate_module/4`'s doc has the full
mechanism). Every one of today's four class-defining call sites
(`load_class_module/3`, `load_compiled_module/6`,
`reload_compile_and_load/4`, `new_class_install/7`) already followed this
exact two-step sequence by convention alone, with nothing enforcing it —
this collapses the two steps into one call so a future call site (or a
refactor of an existing one) cannot obtain a `{module, ModuleName}` result
without `superclasses_losing_leaf_status/1` having already run first: there
is no way to call this function and skip that step, unlike calling
`code:load_binary/3` directly ever again would allow.

Returns `{ok, NewlyNonLeafSuperclasses}` — feed straight into
`activate_module/4`'s fourth argument — on a successful load, or
`{error, Reason}` (the load itself failed; nothing installed, nothing to
re-check) otherwise.
""".
-spec load_class_binary(atom(), string(), binary(), [map()]) ->
    {ok, [binary()]} | {error, term()}.
load_class_binary(ModuleName, LoadPath, Binary, Classes) ->
    NewlyNonLeafSuperclasses = superclasses_losing_leaf_status(Classes),
    case code:load_binary(ModuleName, LoadPath, Binary) of
        {module, ModuleName} -> {ok, NewlyNonLeafSuperclasses};
        {error, Reason} -> {error, Reason}
    end.

-doc """
Activate a loaded module: register classes, trigger hot reload,
and update workspace metadata.
""".
-spec activate_module(atom(), [map()]) -> ok.
activate_module(ModuleName, Classes) ->
    activate_module(ModuleName, Classes, undefined).

-doc """
Activate a loaded module with an optional source path for workspace metadata.
Passing SourcePath ensures the source file is recorded in workspace_meta so that
new VS Code sessions (which have an empty session tracker) can still navigate to source.

ADR 0105 Phase 2 (BT-2780): `spawn_shape_recheck/1` fires last, after
`register_classes/2` has installed the new `register_class/0` (which is what
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
(discovered via the `repl_protocol` e2e suite, BT-2780 review) — dozens of
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

**BT-2856 / ADR 0107 Phase A:** this 3-arity form always passes `[]` for the
leaf-change detection `activate_module/4` (below) accepts — see that
function's doc for why the detection has to happen in the *caller*, before
`code:load_binary/3`, not here.
""".
-spec activate_module(atom(), [map()], string() | undefined) -> ok.
activate_module(ModuleName, Classes, SourcePath) ->
    activate_module(ModuleName, Classes, SourcePath, []).

-doc """
BT-2856 / ADR 0107 Phase A: same as `activate_module/3`, plus
`NewlyNonLeafSuperclasses` — the result of
`superclasses_losing_leaf_status/1`, which the **caller** must have computed
against `Classes` *before* its own `code:load_binary/3` call (not passed as
`Classes` here and computed internally): every generated class module
carries `'on_load' = [{register_class, 0}]` (BT-1610/`actor_codegen.rs`),
and `code:load_binary/3` runs a module's `on_load` function **synchronously,
before returning** — so by the time *any* `activate_module/*` arity could
inspect the hierarchy, the calling `register_class/0` (registering the new
subclass link) has already run, even though `activate_module/3`'s own
`register_classes/2` call (a redundant, harmless second invocation of the
same idempotent `register_class/0`) has not. This is exactly why
`prime_shape_capture/1` (ADR 0105 Phase 2's shape-generation seed) already
has to run before `code:load_binary/3` too — same ordering hazard, same
fix shape. Only call sites that can introduce a genuinely new class
definition (`load_class_module/3`, `load_compiled_module/6`,
`reload_compile_and_load/4`, `new_class_install/7`) bother computing this;
protocol loads (superclass always `"Object"`, never newly-non-leaf) and
method-only patches (`load_recompiled_method/8` — never changes a class's
declared superclass) pass `[]` via the `activate_module/2,3` arities
instead, which is correct, not a gap: neither path can ever produce a
transition this mechanism needs to catch.
""".
-spec activate_module(atom(), [map()], string() | undefined, [binary()]) -> ok.
activate_module(ModuleName, Classes, SourcePath, NewlyNonLeafSuperclasses) ->
    register_classes(Classes, ModuleName),
    trigger_hot_reload(ModuleName, Classes),
    beamtalk_workspace_meta:register_module(ModuleName, SourcePath),
    beamtalk_workspace_meta:update_activity(),
    spawn_shape_recheck(Classes),
    spawn_leaf_change_recheck(NewlyNonLeafSuperclasses).

-doc "Register loaded classes by calling the module's register_class/0 function.".
-spec register_classes([map()], atom()) -> ok.
register_classes(_ClassInfoList, ModuleName) ->
    case erlang:function_exported(ModuleName, register_class, 0) of
        true ->
            try
                ModuleName:register_class()
            catch
                _:_ -> ok
            end;
        false ->
            ok
    end.

-doc "Trigger hot reload for existing actors after module reload (BT-572).".
-spec trigger_hot_reload(atom(), [map()]) -> ok.
trigger_hot_reload(ModuleName, Classes) ->
    lists:foreach(
        fun(ClassMap) ->
            hot_reload_class(ModuleName, ClassMap)
        end,
        Classes
    ),
    ok.

-doc """
Compile and load a source file without REPL session state (BT-845).

Called from beamtalk_behaviour_intrinsics:classReload/1 via erlang:apply/3
to avoid a compile-time dependency from beamtalk_runtime to beamtalk_workspace.
""".
-spec reload_class_file(string()) -> {ok, [map()]} | {error, term()}.
reload_class_file(Path) ->
    reload_class_file_impl(Path, undefined).

%% BT-868: ExpectedClassName (atom) is verified against the compiled class list.
-spec reload_class_file(string(), atom()) -> {ok, [map()]} | {error, term()}.
reload_class_file(Path, ExpectedClassName) ->
    reload_class_file_impl(Path, ExpectedClassName).

-doc "Check if a file path refers to a stdlib file (under stdlib/src/ directory).".
-spec is_stdlib_path(string()) -> boolean().
is_stdlib_path("stdlib/src/" ++ _) ->
    true;
is_stdlib_path(Path) ->
    case string:find(Path, "/stdlib/src/") of
        nomatch -> false;
        _ -> true
    end.

-doc """
Convert a string to snake_case (e.g., "SchemeSymbol" -> "scheme_symbol").
Matches the Rust to_module_name() convention: inserts underscore before
uppercase only when the previous character was lowercase.

BT-3081: delegates to `beamtalk_module_name:camel_to_snake/1`, the single
Erlang-side authority for this conversion. This function previously had its
own drifted copy that force-lowercased the first character unconditionally
(discarding whether it actually started lowercase) and used Unicode
`string:to_lower/1` where the other three copies used ASCII-only `$A..$Z`
arithmetic — e.g. `"eTag"` wrongly became `"etag"` instead of `"e_tag"`.
""".
-spec to_snake_case(string()) -> string().
to_snake_case(Str) ->
    beamtalk_module_name:camel_to_snake(Str).

-doc """
Verify that the expected class name appears in the compiled class list (BT-868).
""".
-spec verify_class_present(atom() | undefined, [#{name := string()}], string()) ->
    ok | {error, term()}.
verify_class_present(undefined, _ClassNames, _Path) ->
    ok;
verify_class_present(ExpectedClassName, ClassNames, Path) ->
    ExpectedName = atom_to_list(ExpectedClassName),
    DefinedNames = [N || #{name := N} <- ClassNames],
    case lists:member(ExpectedName, DefinedNames) of
        true -> ok;
        false -> {error, {class_not_found, ExpectedClassName, Path, DefinedNames}}
    end.

%%% Internal functions

%% Load a compiled module into BEAM, register its classes, and update REPL state.
%%
%% BT-3248: deliberately does NOT emit a `'class-def'` ChangeLog entry, unlike
%% `load_class_module/3`. Every caller of this function (`handle_load/2,3`,
%% `handle_load_source/3`) compiles `Source` from `SourcePath` itself — a
%% `:load <file>` (or the initial project load) installs a class from the
%% SAME file `Workspace flush` would otherwise write it back to, so the live
%% image is, by construction, already in sync with disk: there is nothing
%% pending to log or flush. Logging here would produce a ChangeEntry whose
%% `source`/`prev_source` are byte-identical (a no-op diff) on every ordinary
%% project load — pure log noise, not a fix for anything.
-spec load_compiled_module(
    binary(),
    [map()],
    atom(),
    string(),
    string() | undefined,
    beamtalk_repl_state:state()
) ->
    {ok, [map()], beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
load_compiled_module(Binary, ClassNames, ModuleName, Source, SourcePath, State) ->
    LoadPath =
        case SourcePath of
            undefined -> "";
            _ -> SourcePath
        end,
    %% ADR 0105 Phase 2 (BT-2780): see load_class_module/3's identical comment.
    prime_shape_capture(ClassNames),
    %% BT-2856 / ADR 0107 Phase A, BT-2873 hardening: see load_class_binary/4's doc.
    case load_class_binary(ModuleName, LoadPath, Binary, ClassNames) of
        {ok, NewlyNonLeafSuperclasses} ->
            activate_module(ModuleName, ClassNames, SourcePath, NewlyNonLeafSuperclasses),
            NewState1 = maybe_add_loaded_module(ModuleName, State),
            NewState2 = track_module_source(ModuleName, SourcePath, NewState1),
            store_file_class_sources(ClassNames, Source, NewState2),
            {ok, ClassNames, NewState2};
        {error, Reason} ->
            ClassAtoms = class_name_atoms(ClassNames),
            case beamtalk_runtime_api:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError, State};
                [] ->
                    {error, {load_error, Reason}, State}
            end
    end.

%% BT-1950: Load a protocol module into BEAM, register it, and update REPL state.
%% Used by handle_load/2, handle_load/3, and handle_load_source.
-spec load_protocol_module(map(), string() | undefined, beamtalk_repl_state:state()) ->
    {ok, [map()], beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
load_protocol_module(ProtocolInfo, Path, State) ->
    #{binary := Binary, module_name := ModuleName, protocols := Protocols} = ProtocolInfo,
    ProtocolClassNames = [
        #{name => binary_to_list(P), superclass => "Object"}
     || P <- Protocols
    ],
    LoadPath =
        case Path of
            undefined -> "";
            _ -> Path
        end,
    case code:load_binary(ModuleName, LoadPath, Binary) of
        {module, ModuleName} ->
            %% activate_module calls register_class/0 which registers the protocol
            activate_module(ModuleName, ProtocolClassNames, Path),
            NewState1 = maybe_add_loaded_module(ModuleName, State),
            NewState2 = track_module_source(ModuleName, Path, NewState1),
            {ok, ProtocolClassNames, NewState2};
        {error, Reason} ->
            ClassAtoms = class_name_atoms(ProtocolClassNames),
            case beamtalk_runtime_api:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError, State};
                [] ->
                    {error, {load_error, Reason}, State}
            end
    end.

%% BT-1950: Load a protocol module without session state (stateless path).
%% Used by reload_compile_and_load for load_files_stateless.
-spec load_protocol_module_stateless(map(), string()) -> {ok, [map()]} | {error, term()}.
load_protocol_module_stateless(ProtocolInfo, Path) ->
    #{binary := Binary, module_name := ModuleName, protocols := Protocols} = ProtocolInfo,
    ProtocolClassNames = [
        #{name => binary_to_list(P), superclass => "Object"}
     || P <- Protocols
    ],
    case code:load_binary(ModuleName, Path, Binary) of
        {module, ModuleName} ->
            activate_module(ModuleName, ProtocolClassNames, Path),
            {ok, ProtocolClassNames};
        {error, Reason} ->
            ClassAtoms = class_name_atoms(ProtocolClassNames),
            case beamtalk_runtime_api:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError};
                [] ->
                    {error, {load_error, Reason}}
            end
    end.

%% Add a module to the loaded modules list if not already present.
-spec maybe_add_loaded_module(atom(), beamtalk_repl_state:state()) -> beamtalk_repl_state:state().
maybe_add_loaded_module(ModuleName, State) ->
    LoadedModules = beamtalk_repl_state:get_loaded_modules(State),
    case lists:member(ModuleName, LoadedModules) of
        true -> State;
        false -> beamtalk_repl_state:add_loaded_module(ModuleName, State)
    end.

%% Track loaded module in the module tracker.
-spec track_module_source(atom(), string() | undefined, beamtalk_repl_state:state()) ->
    beamtalk_repl_state:state().
track_module_source(ModuleName, SourcePath, State) ->
    Tracker = beamtalk_repl_state:get_module_tracker(State),
    NewTracker = beamtalk_repl_modules:add_module(ModuleName, SourcePath, Tracker),
    beamtalk_repl_state:set_module_tracker(NewTracker, State).

%% Store class source for later method patching (file load case).
%% Writes to workspace_meta; State is returned unchanged.
-spec store_file_class_sources([map()], string(), beamtalk_repl_state:state()) ->
    beamtalk_repl_state:state().
store_file_class_sources(ClassNames, Source, State) ->
    lists:foreach(
        fun(#{name := Name}) ->
            NameBin = normalize_class_source_key(Name),
            beamtalk_workspace_meta:set_class_source(NameBin, Source)
        end,
        ClassNames
    ),
    State.

%% Store class source for later method patching (inline class definition case).
%% Writes to workspace_meta; State is returned unchanged.
-spec store_class_sources([map()], atom(), string(), beamtalk_repl_state:state()) ->
    {term(), beamtalk_repl_state:state()}.
store_class_sources([], ClassModName, Expression, State) ->
    FallbackName = atom_to_binary(ClassModName, utf8),
    beamtalk_workspace_meta:set_class_source(FallbackName, Expression),
    {FallbackName, State};
store_class_sources(Classes, _ClassModName, Expression, State) ->
    lists:foreach(
        fun(#{name := Name}) ->
            NameBin = normalize_class_source_key(Name),
            beamtalk_workspace_meta:set_class_source(NameBin, Expression)
        end,
        Classes
    ),
    [#{name := FirstName} | _] = Classes,
    {normalize_class_source_key(FirstName), State}.

-spec normalize_class_source_key(atom() | binary() | list()) -> binary().
normalize_class_source_key(Name) when is_binary(Name) -> Name;
normalize_class_source_key(Name) when is_atom(Name) -> atom_to_binary(Name, utf8);
normalize_class_source_key(Name) when is_list(Name) -> list_to_binary(Name).

%%% ----------------------------------------------------------------------------
%%% Class-redefinition ChangeLog entry (ADR 0082 extension, BT-3248)
%%% ----------------------------------------------------------------------------

%% Snapshot each class's currently-tracked source (workspace_meta's
%% `class_sources`) before an inline class-body install overwrites it via
%% `store_class_sources/4`. `undefined` for a class with no prior tracked
%% source — see `emit_class_def_entries/3`'s doc for why that case is
%% excluded from logging.
-spec snapshot_class_def_prev_sources([map()]) -> #{binary() => string() | undefined}.
snapshot_class_def_prev_sources(Classes) ->
    maps:from_list([
        {NameBin, beamtalk_workspace_meta:get_class_source(NameBin)}
     || #{name := Name} <- Classes, NameBin <- [normalize_class_source_key(Name)]
    ]).

-doc """
Log a pending `'class-def'` ChangeLog entry for each class in `Classes` that
*redefined* an already-loaded class (ADR 0082 extension, BT-3248).

Called from `load_class_module/3` after a successful inline class-body
install — the cockpit `:def` tab's "Compile" action against an *existing*
class routes through here
(`beamtalk_repl_eval:handle_class_definition/7` → `load_class_module/3`), and
previously installed the new class body with no ChangeLog entry at all: the
CHANGES dock stayed at "No pending changes" and `Workspace flush` silently
discarded the edit.

Only classes with a PRIOR tracked source (`PrevSources`, snapshotted by
`snapshot_class_def_prev_sources/1` before this install overwrote it) are
logged here — a class with no prior source is a genuinely brand-new class
defined inline (`Foo subclass: Bar [...]` typed directly, never installed
before), which is out of this issue's scope: BT-3248's acceptance criteria is
scoped to redefining an "*existing*" class, and `newClass:at:`'s own
`emit_new_class_entry/3` already covers the brand-new-class case for the one
FFI chokepoint that creates a file for it. A REPL-typed brand-new inline class
recording no ChangeLog entry is the same pre-existing (unrelated) gap it
always was, not a regression introduced here.

Also skips logging entirely when `Classes` has more than one entry. The `:def`
tab always edits exactly one class; a multi-class inline eval (typed directly
at the REPL, not through the tab) would otherwise log one entry per class,
each carrying the *same* whole `Expression` text as its `source` — there is
no per-class slice of a raw multi-class eval to attribute correctly, and
logging the same shared text under N different classes would be misleading
audit history, not a fix. This is the same "out of scope" carve-out as the
brand-new-class case above, not a data-loss concern (see
`add_class_def_flushability/2`'s doc — no `'class-def'` entry is ever
flushed to disk yet regardless).

Returns `true` iff at least one entry was appended, so the caller knows
whether an autoflush pass is worth triggering.
""".
-spec emit_class_def_entries([map()], string(), #{binary() => string() | undefined}) -> boolean().
emit_class_def_entries([#{name := Name}], Expression, PrevSources) ->
    NameBin = normalize_class_source_key(Name),
    case maps:get(NameBin, PrevSources, undefined) of
        undefined ->
            false;
        PrevSource ->
            emit_class_def_entry(NameBin, Expression, PrevSource),
            true
    end;
emit_class_def_entries(_MultipleOrNoClasses, _Expression, _PrevSources) ->
    false.

%% Emit the durable `'class-def'` ChangeEntry for one redefined class. Best
%% effort: a ChangeLog write must never fail or undo the in-memory install
%% (the class is already live), mirroring emit_change_entry/1's contract.
-spec emit_class_def_entry(binary(), string(), string()) -> ok.
emit_class_def_entry(ClassNameBin, NewSource, PrevSource) ->
    try
        NewSourceBin = unicode:characters_to_binary(NewSource),
        PrevSourceBin = unicode:characters_to_binary(PrevSource),
        Base0 = #{
            class => ClassNameBin,
            kind => 'class-def',
            source => NewSourceBin,
            intent => durable,
            %% Reuses the generic (despite its name) newClass:at: author
            %% helpers below — both read the same `$beamtalk_author(_kind)`
            %% process-dictionary convention an eval-driven install (this
            %% path) shares with the FFI chokepoint, defaulting to
            %% human/repl when unset.
            author => new_class_author(),
            author_kind => new_class_author_kind()
        },
        Base = maybe_put_prev_source(Base0, PrevSourceBin),
        Entry = add_class_def_flushability(Base, ClassNameBin),
        _ = beamtalk_workspace_changelog:append(Entry),
        ok
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Failed to emit ChangeLog entry for class redefinition (class still installed)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    class_name => ClassNameBin,
                    domain => [beamtalk, runtime]
                }
            ),
            ok
    end.

-doc """
Classify a `'class-def'` entry's flushability (BT-3248 Phase 1 → BT-3254).

Reuses `class_source_file/1` / `classify_source_file/1` / `no_source_reason/1`
(the same classification `add_flushability/4` uses for a method patch) to
decide whether the class is genuinely an in-project file at all — stdlib,
dependency, and file-less (dynamic) classes stay `flushable: false` exactly
like a method patch on that kind of class.

BT-3254 made the cockpit `:def` tab's resubmitted skeleton
(`beamtalk_repl_ops_browse:class_definition_text/7`) round-trip-safe: it now
carries the `sealed`/`abstract`/`typed` modifier keywords, the
`field:`/`state:` keyword choice, and `::` type annotations, closing the gap
an earlier version of this function's doc described — and the gap that made
an even earlier version of this function (which resolved a real on-disk span
and marked the entry flushable whenever the span resolved, regardless of what
the skeleton actually contained) unsafe: it would have silently downgraded
e.g. `sealed typed Value subclass: Foo` + `field: x :: Integer` to
`Value subclass: Foo` + `state: x` on every ordinary flush, caught in
adversarial review before BT-3248 shipped.

With the skeleton itself fixed, this function now resolves the real on-disk
span (`beamtalk_compiler:resolve_class_span/2` — the same resolver
`beamtalk_workspace_changelog:disk_class_body/2` already used read-only for
the CHANGES-dock diff) and marks the entry flushable when it resolves — the
same span-resolve-or-downgrade shape `add_flushability/4` uses for a method
patch, just resolved at whole-class (header + state) granularity rather than
one method. Unlike a method body, the `:def` tab's skeleton is already built
at the on-disk column-0-header / 2-space-state-line shape, so this never needs
`reindent_method_source` — the only reshape is matching the resolved span's
trailing-newline state (the skeleton itself never carries one).

This chokepoint is NOT exclusive to the cockpit `:def` tab, though: any
successful redefinition of an already-loaded class (e.g. a raw REPL eval of a
FULL class body with methods) reaches `load_class_module/3` → here too, with
whatever text the caller compiled as `Base`'s `source`. Before trusting the
disk span at all, `add_class_def_span_or_downgrade/4` first confirms `source`
ITSELF is header+state-only (`class_def_source_is_skeleton_shaped/2`, reusing
`resolve_class_span/2` against the candidate text rather than the disk file) —
otherwise splicing a `source` that also carries methods into the disk span
(which, by construction, stops before the file's first method) would jam the
new methods into the header/state region while leaving the file's real
methods stale and duplicated right after it. Same bug class BT-3248's
adversarial review caught for the disk side; this closes the mirror-image gap
on the resubmitted-text side.

A second, independent adversarial-review finding closes a further gap: a
compiled (non-`ClassBuilder`) class's field DEFAULT-VALUE TEXT is not
recoverable from live reflection at all (only whether a field has one —
`__beamtalk_meta/0`'s `field_has_default`, BT-1976), so `class_definition_
text/7`'s skeleton always renders `default => null` for every field of any
real, file-backed class — even one whose on-disk declaration has one.
`resolve_class_def_span_entry/4` therefore ALSO compares each field's
default-presence between the disk source and the candidate replacement
(`class_def_preserves_field_defaults/3`, via
`beamtalk_compiler:class_state_field_defaults/2`) and refuses to flush when a
field that had a default on disk would lose it — otherwise a byte-accurate,
method-safe, modifier-complete splice could still silently delete a default.

A third finding (Claude BeamTalk Review, this same PR) closes a related span
gap: `resolve_class_span/2` clamps its span to end before the class's first
method, so a `state:`/`field:` declared AT OR AFTER a method (legal Beamtalk)
is excluded from the span/`PrevSource` entirely — but the live-reflected
skeleton includes it regardless of position, so splicing the skeleton into
the clamped span duplicates that field's declaration rather than losing it
(the field survives, untouched, past the spliced region too).
`class_def_span_contains_all_state_fields/3` refuses to flush whenever the
disk class has any field outside the span that would be spliced.
""".
-spec add_class_def_flushability(map(), binary()) -> map().
add_class_def_flushability(Base, ClassNameBin) ->
    case class_source_file(ClassNameBin) of
        nil ->
            Base#{flushable => false, not_flushable_reason => no_source_reason(ClassNameBin)};
        SourceFile when is_binary(SourceFile) ->
            case classify_source_file(SourceFile) of
                {flushable, AbsPath} ->
                    add_class_def_span_or_downgrade(Base, ClassNameBin, SourceFile, AbsPath);
                {not_flushable, Reason} ->
                    Base#{flushable => false, not_flushable_reason => Reason}
            end
    end.

%% Read the current on-disk file and resolve the class's span against it —
%% but ONLY once `Base`'s own `source` has been confirmed header+state-only
%% (`class_def_source_is_skeleton_shaped/2`). `add_class_def_flushability/2`
%% is reached from `load_class_module/3`, which is NOT exclusive to the
%% cockpit `:def` tab's own skeleton — any successful redefinition of an
%% already-loaded class routes through it, e.g. a raw REPL eval of a FULL
%% class body (header + state + methods). Splicing such a `source` into the
%% disk span — which, by `resolve_class_span/2`'s own "never reaches a
%% method" guarantee, stops before the *first* method — would jam the new
%% text's methods into the header/state region while leaving the file's
%% actual (now stale/duplicated) methods untouched right after it: exactly
%% the class of bug BT-3248's adversarial review caught, just triggered from
%% the resubmitted text's shape instead of the disk span's. Mirrors
%% `add_span_or_downgrade/6`'s disk-read-failure handling exactly for the
%% read itself (an unreadable file downgrades to memory-only rather than
%% blocking the install that already happened).
-spec add_class_def_span_or_downgrade(map(), binary(), binary(), string()) -> map().
add_class_def_span_or_downgrade(#{source := Canonical} = Base, ClassNameBin, SourceFile, AbsPath) ->
    case class_def_source_is_skeleton_shaped(Canonical, ClassNameBin) of
        false ->
            Base#{
                flushable => false,
                not_flushable_reason => <<"class_def_source_not_skeleton_shaped">>,
                source_file => SourceFile
            };
        true ->
            case file:read_file(AbsPath) of
                {ok, DiskSource} ->
                    resolve_class_def_span_entry(Base, ClassNameBin, SourceFile, DiskSource);
                {error, ReadReason} ->
                    ?LOG_WARNING(
                        "ChangeLog: could not read sourceFile for class-def entry; "
                        "recording memory-only",
                        #{
                            source_file => SourceFile,
                            reason => ReadReason,
                            domain => [beamtalk, runtime]
                        }
                    ),
                    Base#{flushable => false, not_flushable_reason => <<"disk_read_failed">>}
            end
    end.

%% True iff `Source` (the resubmitted `'class-def'` text) is ITSELF entirely
%% header + state — i.e. `resolve_class_span/2` applied to `Source` (not the
%% on-disk file) resolves a span reaching all the way to the end of `Source`
%% AND starting at its very beginning, modulo whitespace on either side.
%% Reuses the exact same resolver the disk side uses, just pointed at the
%% candidate replacement text instead:
%%
%%  * trailing bytes past the resolved span — if `Source` carries a method,
%%    `resolve_class_span` clamps its span to end BEFORE that method (its own
%%    "never reaches a method" guarantee — see `class_span.rs`'s module doc),
%%    so any non-blank bytes left over past the resolved span are exactly the
%%    method text this guard exists to catch.
%%  * leading bytes before the resolved span — `resolve_class_span`'s span
%%    deliberately starts at the class's OWN declaration line, never backing
%%    up across a `///` doc comment or `//` license header (see that module's
%%    "Span boundaries" doc). A candidate `Source` with such a leading
%%    comment (e.g. a raw REPL eval that includes a NEW doc comment, unlike
%%    the `:def` tab's own skeleton, which never carries one) would still
%%    pass a trailing-only check, and splicing the whole `Source` — comment
%%    included — into the disk span (which starts only at the header,
%%    excluding the disk's own doc comment) would duplicate/misplace it.
-spec class_def_source_is_skeleton_shaped(binary(), binary()) -> boolean().
class_def_source_is_skeleton_shaped(Source, ClassNameBin) ->
    case beamtalk_compiler:resolve_class_span(Source, ClassNameBin) of
        {ok, #{start := Start, 'end' := End}, _Body} when
            Start =< End, End =< byte_size(Source)
        ->
            %% `string:trim/1` (not a hand-rolled whitespace scan): blank
            %% bytes on both sides of the resolved span means `Source` is
            %% entirely header+state; anything else (a method, a leading
            %% comment) is not blank.
            string:trim(binary:part(Source, 0, Start)) =:= <<>> andalso
                string:trim(binary:part(Source, End, byte_size(Source) - End)) =:= <<>>;
        _ ->
            %% Resolution failure (`class_not_found`/`ambiguous`) on text that
            %% was just successfully compiled under this exact class name
            %% should not happen — but fails closed (not skeleton-shaped)
            %% rather than risking an unsafe splice on a resolver surprise.
            false
    end.

-spec resolve_class_def_span_entry(map(), binary(), binary(), binary()) -> map().
resolve_class_def_span_entry(#{source := Canonical} = Base, ClassNameBin, SourceFile, DiskSource) ->
    case beamtalk_compiler:resolve_class_span(DiskSource, ClassNameBin) of
        {ok, Span, PrevSource} ->
            case class_def_span_contains_all_state_fields(DiskSource, PrevSource, ClassNameBin) of
                false ->
                    Base#{
                        flushable => false,
                        not_flushable_reason => <<"class_def_state_after_method">>,
                        source_file => SourceFile
                    };
                true ->
                    case class_def_preserves_field_defaults(DiskSource, Canonical, ClassNameBin) of
                        true ->
                            store_class_def_disk_shaped_entry(Base, SourceFile, Span, PrevSource);
                        false ->
                            Base#{
                                flushable => false,
                                not_flushable_reason => <<"class_def_would_drop_field_default">>,
                                source_file => SourceFile
                            }
                    end
            end;
        {error, Reason, _Message} ->
            %% `class_not_found` (renamed/moved out from under the live class)
            %% or `ambiguous` (duplicate declarations) — either downgrades to
            %% memory-only, same as a method patch's span-resolution failure.
            span_error_entry(Base, SourceFile, Reason)
    end.

%% True iff EVERY `state:`/`field:` declaration `ClassNameBin` has anywhere in
%% `DiskSource` (the whole file) is also visible within `SpanText` (the
%% `resolve_class_span/2`-clamped region that a flush would actually splice
%% into) — i.e. the disk class has no field declared at or after its first
%% method.
%%
%% Why this exists (Claude BeamTalk Review finding on this PR, BT-3254):
%% `resolve_class_span/2` deliberately clamps its span to end BEFORE the
%% class's first method (its own "never reaches a method" guarantee — see
%% `class_span.rs`'s module doc) — legal Beamtalk allows a `state:`/`field:`
%% declaration positioned at/after a method, and such a field is EXCLUDED
%% from the clamped span/`PrevSource`. But the `:def` tab's skeleton is built
%% from LIVE reflection (`instance_variables`), which is position-agnostic —
%% it always includes every current field regardless of where it was
%% declared on disk. `class_def_preserves_field_defaults/3` alone does not
%% catch this: a field with no default (so nothing to "lose") still gets
%% written into the skeleton, and splicing that skeleton into the clamped
%% span duplicates the field's declaration — once newly inserted at the
%% span's location, once still present, untouched, after the method. Repro:
%% `Actor subclass: Foo\n  method1 => 1\n\n  state: b\n` (b has no default)
%% resolves a span of just `Actor subclass: Foo\n`; the regenerated skeleton
%% `Actor subclass: Foo\n  state: b\n` spliced into that span leaves `state:
%% b` duplicated in the file (once before `method1`, once after).
%%
%% Reuses `beamtalk_compiler:class_state_field_defaults/2` for its field-NAME
%% enumeration (the boolean default-presence values themselves are unused
%% here) against BOTH `DiskSource` (every field on disk, any position) and
%% `SpanText` (only the fields within the region that would actually be
%% spliced) — any field present in the former but not the latter is exactly
%% the "declared after the first method" case this guards against.
-spec class_def_span_contains_all_state_fields(binary(), binary(), binary()) -> boolean().
class_def_span_contains_all_state_fields(DiskSource, SpanText, ClassNameBin) ->
    case
        {
            beamtalk_compiler:class_state_field_defaults(DiskSource, ClassNameBin),
            beamtalk_compiler:class_state_field_defaults(SpanText, ClassNameBin)
        }
    of
        {{ok, AllFields}, {ok, SpanFields}} ->
            sets:is_subset(
                sets:from_list(maps:keys(AllFields), [{version, 2}]),
                sets:from_list(maps:keys(SpanFields), [{version, 2}])
            );
        _ ->
            %% Either side failed to resolve — fail closed, same posture as
            %% `class_def_preserves_field_defaults/3`.
            false
    end.

%% True iff redefining `ClassNameBin` in `OldSource` (the on-disk file) with
%% `NewSource` (the candidate replacement — already confirmed skeleton-shaped
%% by `class_def_source_is_skeleton_shaped/2`) would NOT silently drop any
%% field's default value.
%%
%% Why this exists (BT-3254 adversarial-review finding): `class_definition_
%% text/7`'s `default` field is read from LIVE class reflection
%% (`beamtalk_repl_ops_browse:state_slots/2`'s `field_defaults`), and that
%% reflection is populated ONLY for a `beamtalk_class_builder`-created class
%% (file-less, so it never reaches this function at all — `class_source_
%% file/1` already gates on a real `sourceFile`). A COMPILED `.bt` class's
%% `__beamtalk_meta/0` carries `field_has_default` (a boolean per field,
%% BT-1976) but never the default-value TEXT, so `field_defaults` reflection
%% is always empty and the skeleton always renders `default => null` — even
%% for a field the on-disk source declares with one. Splicing that skeleton
%% into a byte-accurate disk span (the span/method-safety guarantee
%% `resolve_class_span/2` gives) would still silently DELETE the default —
%% exactly the class of data-loss bug this whole feature exists to prevent,
%% just triggered by an unreflectable value instead of an unrendered
%% modifier keyword.
%%
%% Reuses `beamtalk_compiler:class_state_field_defaults/2` (the parser's own
%% `StateDeclaration.default_value`, not a text heuristic) against BOTH
%% texts: a field present in both with a default in `OldSource` must also
%% have one in `NewSource`. A field ABSENT from `NewSource` (an intentional
%% removal — the user deleted that field via the editor) is not a violation;
%% only "still declared, default silently gone" is.
-spec class_def_preserves_field_defaults(binary(), binary(), binary()) -> boolean().
class_def_preserves_field_defaults(OldSource, NewSource, ClassNameBin) ->
    case
        {
            beamtalk_compiler:class_state_field_defaults(OldSource, ClassNameBin),
            beamtalk_compiler:class_state_field_defaults(NewSource, ClassNameBin)
        }
    of
        {{ok, OldDefaults}, {ok, NewDefaults}} ->
            maps:fold(
                fun(Field, HadDefault, Acc) ->
                    Acc andalso
                        (not HadDefault orelse
                            case maps:find(Field, NewDefaults) of
                                {ok, StillHasDefault} -> StillHasDefault;
                                error -> true
                            end)
                end,
                true,
                OldDefaults
            );
        _ ->
            %% Either side failed to resolve (transport error, or the class
            %% somehow doesn't parse under its own name — pathological given
            %% `resolve_class_span/2` on `OldSource` just succeeded and
            %% `NewSource` was already confirmed skeleton-shaped): fail
            %% closed rather than risk an unsafe splice on a resolver
            %% surprise.
            false
    end.

%% Store the flushable shape of a `'class-def'` entry once its span has
%% resolved. Unlike `store_disk_shaped_entry/4` (a method patch), the
%% skeleton's `source` needs no `reindent_method_source` reshape — a class
%% header and its `state:`/`field:` lines are already built at the on-disk
%% column-0 / 2-space convention (`class_definition_text/7`) — only the
%% trailing-newline shape can mismatch (the skeleton never carries one; the
%% resolved span usually does), matched the same way a method patch's body is.
-spec store_class_def_disk_shaped_entry(
    map(), binary(), #{start := non_neg_integer(), 'end' := non_neg_integer()}, binary()
) -> map().
store_class_def_disk_shaped_entry(#{source := Canonical} = Base, SourceFile, Span, PrevSource) ->
    DiskShaped = match_trailing_newline(Canonical, PrevSource),
    Base#{
        source => DiskShaped,
        flushable => true,
        source_file => SourceFile,
        span => Span,
        prev_source => PrevSource
    }.

%% Extract trailing expression info from a class definition result (BT-885).
-spec extract_trailing_info(map()) ->
    no_trailing | {trailing, atom(), binary()}.
extract_trailing_info(ClassInfo) ->
    case maps:find(trailing_binary, ClassInfo) of
        {ok, TrailingBinary} ->
            TrailingModName = maps:get(trailing_module_name, ClassInfo),
            {trailing, TrailingModName, TrailingBinary};
        error ->
            no_trailing
    end.

%% Trigger hot reload for a single class.
-spec hot_reload_class(atom(), map()) -> ok.
hot_reload_class(ModuleName, ClassMap) ->
    ClassName = resolve_class_name(ClassMap),
    case ClassName of
        undefined ->
            ok;
        _ ->
            Pids =
                try
                    beamtalk_runtime_api:all_instances(ClassName)
                catch
                    error:badarg -> []
                end,
            case Pids of
                [] ->
                    ok;
                _ ->
                    IVars = fetch_instance_vars(ClassName),
                    Extra = {IVars, ModuleName},
                    beamtalk_runtime_api:trigger_code_change(ModuleName, Pids, Extra)
            end
    end.

%% Resolve a class name atom from a class map entry.
-spec resolve_class_name(map()) -> atom() | undefined.
resolve_class_name(ClassMap) ->
    case maps:get(name, ClassMap, undefined) of
        N when is_binary(N) ->
            safe_binary_to_atom(N);
        N when is_atom(N) ->
            N;
        N when is_list(N) ->
            safe_list_to_atom(N);
        _ ->
            undefined
    end.

-spec safe_binary_to_atom(binary()) -> atom() | undefined.
safe_binary_to_atom(Bin) ->
    try
        binary_to_existing_atom(Bin, utf8)
    catch
        error:badarg -> undefined
    end.

-spec safe_list_to_atom(string()) -> atom() | undefined.
safe_list_to_atom(List) ->
    try
        list_to_existing_atom(List)
    catch
        error:badarg -> undefined
    end.

%% Fetch instance variables from the class registry.
-spec fetch_instance_vars(atom()) -> list().
fetch_instance_vars(ClassName) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined ->
            [];
        ClassPid ->
            try
                beamtalk_runtime_api:instance_variables(ClassPid)
            catch
                _:_ -> []
            end
    end.

%% Reload a class file without REPL session state.
-spec reload_class_file_impl(string(), atom() | undefined) -> {ok, [map()]} | {error, term()}.
reload_class_file_impl(Path, ExpectedClassName) ->
    case filelib:is_file(Path) of
        false ->
            {error, {file_not_found, Path}};
        true ->
            case file:read_file(Path) of
                {error, Reason} ->
                    {error, {read_error, Reason}};
                {ok, SourceBin} ->
                    Source = binary_to_list(SourceBin),
                    ModuleNameOverride = compute_package_module_name(Path),
                    reload_compile_and_load(Source, Path, ModuleNameOverride, ExpectedClassName)
            end
    end.

%% Compile and load a file for stateless reload.
%%
%% BT-3248: deliberately does NOT emit a `'class-def'` ChangeLog entry, same
%% reasoning as `load_compiled_module/6`. Both of this function's callers
%% compile `Source` straight from `Path` — `reload_class_file_impl/2` backs
%% `Counter reload` / `:reload Counter` (an explicit reload FROM the on-disk
%% file after an external edit) and `remove_method/3`'s "reload the class
%% WITHOUT the removed method" (which recompiles a spliced *in-memory* source
%% but is followed by its own `emit_remove_change_entry/5` call at the
%% `removeSelector:` call site, ADR 0112 Phase 3 BT-3187 — already logged via
%% a different, more specific kind). Neither case has a class-definition edit
%% pending relative to disk that a `'class-def'` entry would newly capture.
-spec reload_compile_and_load(
    string(), string(), binary() | undefined, atom() | undefined
) -> {ok, [map()]} | {error, term()}.
reload_compile_and_load(Source, Path, ModuleNameOverride, ExpectedClassName) ->
    case compile_reload_source(Source, Path, ModuleNameOverride, ExpectedClassName) of
        {ok, _Tag, _} = ProtocolResult ->
            install_reload_result(ProtocolResult, Path);
        {ok, _Tag, _, _, _} = CompiledResult ->
            install_reload_result(CompiledResult, Path);
        {error, _} = Err ->
            Err
    end.

-doc """
Compile half of `reload_compile_and_load/4` — split out for BT-3270's
in-memory atomicity protocol (ADR 0114 § Decision, final paragraph), which
needs to compile/validate every site in a multi-site rewrite batch *before*
installing any of them (see `rewrite_sites/2`'s doc). Never touches
`code:load_binary/3` or any other mutating step — a caller can call this as
many times as it likes (one per candidate rewrite) with no risk of leaving a
class half-installed, unlike `reload_compile_and_load/4` itself, which always
compiles and installs in one step.

Returns the same shapes `beamtalk_repl_compiler:compile_file/4` does (tagged
so `install_reload_result/2` can dispatch on them), or `{error, Reason}` on a
compile failure — this function's whole contract is "tell me whether this
source is installable", so a compile failure here is reported, never raised.
""".
-spec compile_reload_source(string(), string(), binary() | undefined, atom() | undefined) ->
    {ok, protocol_definition, map()}
    | {ok, compiled, binary(), [map()], atom()}
    | {error, term()}.
compile_reload_source(Source, Path, ModuleNameOverride, ExpectedClassName) ->
    StdlibMode = is_stdlib_path(Path),
    case beamtalk_repl_compiler:compile_file(Source, Path, StdlibMode, ModuleNameOverride) of
        %% BT-1950: Protocol definition — must match before generic 4-tuple.
        {ok, protocol_definition, ProtocolInfo, _Warnings} ->
            {ok, protocol_definition, ProtocolInfo};
        {ok, Binary, ClassNames, ModuleName} ->
            case verify_class_present(ExpectedClassName, ClassNames, Path) of
                ok -> {ok, compiled, Binary, ClassNames, ModuleName};
                {error, _} = Err -> Err
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-doc """
Install half of `reload_compile_and_load/4` — see `compile_reload_source/4`'s
doc for why this is split out. Takes a successful `compile_reload_source/4`
result and performs the mutating half: `load_class_binary/4` +
`activate_module/4` (or the protocol-module equivalent). Never called on a
value that failed `compile_reload_source/4` — callers gate on `{ok, ...}`
first, which is exactly BT-3270's atomicity protocol: nothing calls this
until every site in a batch has independently validated.

Exported (BT-3280) purely so `install_rewrite_group/5`'s own
`?MODULE:install_reload_result/2` call can be intercepted by `meck` in
`beamtalk_repl_loader_rewrite_sites_tests.erl`'s `partial_install_failure`
coverage — not part of this module's intended public API otherwise; every
other caller (`reload_class_file_impl/2`, `remove_method/3`,
`install_rewrite_group/5`) is itself already inside this module.
""".
-spec install_reload_result(
    {ok, protocol_definition, map()} | {ok, compiled, binary(), [map()], atom()}, string()
) -> {ok, [map()]} | {error, term()}.
install_reload_result({ok, protocol_definition, ProtocolInfo}, Path) ->
    load_protocol_module_stateless(ProtocolInfo, Path);
install_reload_result({ok, compiled, Binary, ClassNames, ModuleName}, Path) ->
    %% ADR 0105 Phase 2 (BT-2780): see load_class_module/3's identical
    %% comment. Covers every caller of this helper: reload_class_file_impl/2
    %% (file reload after an on-disk edit), remove_method/3's "reload the
    %% class WITHOUT the removed method", and rewrite_sites/2's per-site
    %% install (BT-3270) — none of these change `state:`/`field:` slots, so
    %% priming it is harmless (the subsequent capture/1 always diffs an
    %% unchanged shape to itself, `no_op`).
    prime_shape_capture(ClassNames),
    %% BT-2856 / ADR 0107 Phase A, BT-2873 hardening: see load_class_binary/4's doc.
    case load_class_binary(ModuleName, Path, Binary, ClassNames) of
        {ok, NewlyNonLeafSuperclasses} ->
            activate_module(ModuleName, ClassNames, Path, NewlyNonLeafSuperclasses),
            {ok, ClassNames};
        {error, Reason} ->
            {error, {load_error, Reason}}
    end.

%% Recompile a class with a new method definition.
%%
%% BT-911: Delegates to beamtalk_repl_compiler:compile_for_method_reload/2 which
%% wraps all compiler calls in wrap_compiler_errors, preventing compiler crashes
%% from propagating as exits that would kill the REPL process.
-spec recompile_with_method(
    string(), map(), string(), [binary()], beamtalk_repl_state:state()
) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
recompile_with_method(ClassSource, MethodInfo, Expression, Warnings, State) ->
    CombinedSource = ClassSource ++ "\n" ++ Expression,
    %% Source may carry non-Latin1 characters (em dash, arrows, smart quotes in
    %% doc comments) — `unicode:characters_to_binary/1' produces the UTF-8 the
    %% compiler expects, where `list_to_binary/1' crashes with `badarg' on any
    %% codepoint > 255. Both inputs are already-validated source (the stored class
    %% source and a just-compiled expression), so the conversion always yields a
    %% binary here — never the `{error,_,_}' tuple of malformed input.
    SourceBin = unicode:characters_to_binary(CombinedSource),
    %% BT-907: Include superclass index so cross-file inheritance resolves correctly.
    SuperclassIndex = beamtalk_repl_compiler:build_class_superclass_index(),
    Options0 = #{stdlib_mode => false, workspace_mode => true},
    Options1 =
        case map_size(SuperclassIndex) of
            0 -> Options0;
            _ -> Options0#{class_superclass_index => SuperclassIndex}
        end,
    %% Include module index for correct cross-directory class references.
    ModuleIndex = beamtalk_repl_compiler:build_class_module_index(),
    Options2 =
        case map_size(ModuleIndex) of
            0 -> Options1;
            _ -> Options1#{class_module_index => ModuleIndex}
        end,
    %% BT-2553 follow-up: preserve the class's package-qualified module name and
    %% on-disk source path across the patch so a project class stays
    %% `bt@pkg@mod' (flushable, revertable) instead of degrading to a stem-named,
    %% source-less `bt@mod'.
    #{class_name := ClassNameBin} = MethodInfo,
    {ModuleNameOverride, SourcePath} = patch_module_target(ClassNameBin),
    Options3 = beamtalk_repl_compiler:apply_module_name_override(Options2, ModuleNameOverride),
    Options = beamtalk_repl_compiler:apply_source_path(Options3, SourcePath),
    LoadPath = source_path_or_empty(SourcePath),
    case beamtalk_repl_compiler:compile_for_method_reload(SourceBin, Options) of
        {ok, Binary, ModName, Classes, RecompileWarnings} ->
            AllWarnings = Warnings ++ RecompileWarnings,
            load_recompiled_method(
                Binary,
                ModName,
                Classes,
                MethodInfo,
                CombinedSource,
                LoadPath,
                AllWarnings,
                State
            );
        {error, Reason} ->
            {error, Reason, <<>>, Warnings, State}
    end.

-doc """
Install a single method into a class via the structured backend compile
(the rock-solid live-image write-surface idiom — IDE save / `compile:source:` /
MCP `save_method`).

`MethodSource` is the BARE method body (comments and all). It is parsed
standalone by the backend — no `Class >>` text wrap, no `normalize_method_source`
header-sniffing — so the stored source round-trips byte-for-byte. The class's
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
    [binary()],
    beamtalk_repl_state:state()
) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
install_method(
    ClassNameBin, SelectorBin, MethodSource, Intent, Author, AuthorKind, Warnings, State
) ->
    %% Default to instance-side for existing callers (`compile:source:` / MCP
    %% `save_method` / IDE save). The side-aware `install_method/9` backs
    %% class-side revert re-installs (BT-2665).
    install_method(
        ClassNameBin, SelectorBin, MethodSource, Intent, Author, AuthorKind, Warnings, State, false
    ).

-doc """
Install a single method into a class, threading the patch side (BT-2665).

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
    [binary()],
    beamtalk_repl_state:state(),
    boolean()
) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
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
    [binary()],
    beamtalk_repl_state:state(),
    boolean()
) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
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
    {ModuleNameOverride, SourcePath} = patch_module_target(ClassNameBin),
    SuperclassIndex = beamtalk_repl_compiler:build_class_superclass_index(),
    ModuleIndex = beamtalk_repl_compiler:build_class_module_index(),
    Options = #{
        class_name => ClassNameBin,
        %% Side comes from the caller: `false` for the instance-side
        %% `compile:source:` / MCP `save_method` / IDE-save chokepoint; `true`
        %% for a class-side revert re-install (BT-2665). The REPL `Class class >>
        %% sel` path still flows through `reload_method_definition`, which carries
        %% the side in its `MethodInfo`.
        is_class_method => IsClassMethod,
        workspace_mode => true,
        module_name => ModuleNameOverride,
        source_path => source_path_binary(SourcePath),
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
            %% ADR 0105 Phase 1 (BT-2777): declared signature, carried through to
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
                source_path_or_empty(SourcePath),
                AllWarnings,
                State
            );
        {error, Reason} ->
            {error, Reason, <<>>, Warnings, State}
    end.

-doc """
Pre-save advisory (ADR 0105 Phase 3, BT-2782): compile a pending method edit
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
    {ModuleNameOverride, SourcePath} = patch_module_target(ClassNameBin),
    SuperclassIndex = beamtalk_repl_compiler:build_class_superclass_index(),
    ModuleIndex = beamtalk_repl_compiler:build_class_module_index(),
    Options = #{
        class_name => ClassNameBin,
        is_class_method => IsClassMethod,
        workspace_mode => true,
        module_name => ModuleNameOverride,
        source_path => source_path_binary(SourcePath),
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
            Side = patch_side(ActualIsClassMethod),
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
Remove a live method from a class by recompiling the class without it (BT-2663,
BT-2665). Backs the *add* revert case: when a freshly-added method (instance or
class side) is reverted, its pre-patch state was "absent", so the revert deletes
the method rather than re-installing a prior body.

`Side` (`instance | class`) selects which side's method to drop. The method's
span is resolved against the class's CURRENT in-memory merged source, spliced out,
and the remaining class source is recompiled + hot-reloaded. Returns
`{ok, ClassNameBin}` on success or `{error, Reason}` if the class source is
unavailable, the selector cannot be located, or the recompile/reload fails (the
live image is unchanged in the error cases). The removal does NOT itself emit a
ChangeEntry — the caller curtails the original add entry separately (a
`revert:`-of-an-add), or, for `removeSelector:` (ADR 0112 Phase 3, BT-3187),
calls `emit_remove_change_entry/5` afterward.
""".
-spec remove_method(binary(), atom() | binary(), instance | class) ->
    {ok, binary()} | {error, term()}.
remove_method(ClassNameBin, Selector, Side) ->
    SelectorBin = method_selector_binary(Selector),
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
                    %% ADR 0105 Phase 1 (BT-2777): record the removal in the
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
                            %% ADR 0105 Phase 1 (BT-2778): the removal is
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
                            maybe_trigger_recheck(
                                ClassNameBin, SelectorBin, Side, RemovalCapture
                            ),
                            Ok;
                        {error, _} = Error ->
                            rollback_signature_generation(
                                ClassNameBin, SelectorBin, Side, RemovalCapture
                            ),
                            Error
                    end;
                {error, Reason, _Msg} ->
                    {error, {method_not_found, Reason}}
            end
    end.

%% Record a method removal into the signature-generation store (ADR 0105 Phase
%% 1, BT-2777). Best-effort and self-swallowing, mirroring
%% capture_signature_generation/1 — a store failure must never block the
%% removal itself. Returns the same capture_outcome() so
%% the caller can roll back on a subsequent recompile failure.
-spec capture_signature_removal(binary(), binary(), instance | class) -> capture_outcome().
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
    {ModuleNameOverride, SourcePath} = patch_module_target(ClassNameBin),
    LoadPath = source_path_or_empty(SourcePath),
    NewSourceStr = unicode:characters_to_list(NewSourceBin),
    case reload_compile_and_load(NewSourceStr, LoadPath, ModuleNameOverride, undefined) of
        {ok, ClassNames} ->
            lists:foreach(
                fun(#{name := Name}) ->
                    beamtalk_workspace_meta:set_class_source(
                        normalize_class_source_key(Name), NewSourceStr
                    )
                end,
                ClassNames
            ),
            {ok, ClassNameBin};
        {error, Reason} ->
            {error, Reason}
    end.

%% Cut the bytes `[start, end)' out of `Source', joining the surrounding text.
%% A thin wrapper over `splice_replace/3` (BT-3270) — removal is replacement
%% with the empty binary.
-spec splice_out_span(binary(), rewrite_span()) -> binary().
splice_out_span(Source, Span) ->
    splice_replace(Source, Span, <<>>).

%% Replace the bytes `[start, end)' in `Source' with `NewText', joining the
%% surrounding text. Shared leaf primitive (CLAUDE.md's no-duplicate-
%% implementations rule) behind both `splice_out_span/2` (removal — ADR 0112)
%% and `rewrite_sites/2`'s per-site splice (ADR 0114, BT-3270).
-spec splice_replace(binary(), rewrite_span(), binary()) -> binary().
splice_replace(Source, #{start := Start, 'end' := End}, NewText) ->
    <<Before:Start/binary, _Old:(End - Start)/binary, After/binary>> = Source,
    <<Before/binary, NewText/binary, After/binary>>.

-spec method_selector_binary(atom() | binary()) -> binary().
method_selector_binary(Sel) when is_binary(Sel) -> Sel;
method_selector_binary(Sel) when is_atom(Sel) -> atom_to_binary(Sel, utf8).

%% Resolve a class's package-qualified module name + on-disk source path for a
%% patch, by reusing the class's CURRENT loaded module (which already carries the
%% correct package-qualified name for both project and dependency classes) and
%% its `beamtalk_source' attribute. Returns `{undefined, undefined}' for
%% dynamic/source-less classes so they keep the default stem naming.
-spec patch_module_target(binary()) -> {binary() | undefined, string() | undefined}.
patch_module_target(ClassNameBin) ->
    case class_module(ClassNameBin) of
        {ok, Module} ->
            case beamtalk_reflection:source_file_from_module(Module) of
                Path when is_binary(Path) ->
                    {atom_to_binary(Module, utf8), binary_to_list(Path)};
                _ ->
                    {undefined, undefined}
            end;
        error ->
            {undefined, undefined}
    end.

-spec source_path_binary(string() | undefined) -> binary() | undefined.
source_path_binary(undefined) -> undefined;
source_path_binary(Path) -> list_to_binary(Path).

-spec source_path_or_empty(string() | undefined) -> string().
source_path_or_empty(undefined) -> "";
source_path_or_empty(Path) -> Path.

%%% ----------------------------------------------------------------------------
%%% Shared multi-site rewrite mechanism (ADR 0114, BT-3270)
%%% ----------------------------------------------------------------------------

-doc """
Rewrite a definition site plus N reference/sender sites transactionally, in
memory, generalizing `remove_method/3`'s single-site splice+recompile+
hot-reload for the multi-site case ADR 0114's `renameTo:`/
`renameSelector:to:` both need (BT-3271/BT-3272, not yet built — this is the
shared mechanism both will call with different, already-computed site lists;
site *discovery* itself is out of scope here, per this issue's text).

`DefinitionSite` is the primary target's own declaration/definition
(`undefined` only for a dynamic, source-less class being renamed — mirrors
the ChangeLog schema's `sites[0] = null` case, ADR 0114 § ChangeLog schema).
`ReferenceSites` is every other site to rewrite alongside it. Both are plain
`rewrite_site()` maps — this function has no opinion on WHY a site is being
rewritten (a class name, a selector, anything else a future primitive needs);
it only knows how to splice `new_text` into `class`'s current source at
`span`, recompile, and hot-reload.

## The in-memory atomicity protocol (ADR 0114 § Decision, final paragraph)

The ADR explicitly leaves open how to avoid leaving a rename half-applied in
memory if rewriting confirmed site 5 of 10 fails partway through — before any
flush happens, across N separate class gen_servers, with no OTP cross-process
transaction primitive to roll back a hot-reloaded module once live actors may
hold references to it (same precedent ADR 0082 already established). This
function's answer, per the ADR's own steer:

1. **Group sites by owning class.** Two sites can legitimately target the
   SAME class (e.g. a method-rename's definition and a same-class self-send
   both live in `Counter`'s own file) — these MUST be merged into one splice
   + one recompile, never two independent ones, or the second recompile
   would silently discard the first's edit (it would start again from the
   unmodified `beamtalk_workspace_meta` source). `group_sites_by_class/1`
   also rejects overlapping/out-of-bounds spans within a class up front —
   a structural bug in the caller's site list, not a compile failure, so it
   is reported before any compile is even attempted.
2. **Validate every class-group's rewritten source FIRST — a pure,
   non-mutating pass.** Each group's post-splice source is compiled via
   `compile_reload_source/4` (the same `beamtalk_repl_compiler:compile_file/4`
   + `verify_class_present/3` step `reload_compile_and_load/4` already runs
   for the single-site case) but NEVER installed at this stage — no
   `code:load_binary/3`, no `activate_module/4`, nothing observable to any
   other process. If ANY group fails to compile, `rewrite_sites/2` returns
   `{error, {validation_failed, PerClassReasons}}` immediately and NOTHING
   has been mutated: every class's `beamtalk_workspace_meta` source and
   loaded module are exactly as they were before this call. This is what
   gives the "no class left in a half-rewritten state" guarantee.
3. **Only once every group has validated does the install pass run** — for
   each group in turn, `class_source_unchanged/1` re-checks that the class's
   `beamtalk_workspace_meta` source is still byte-identical to what
   `build_class_group/2` snapshotted into it BEFORE validation ran (BT-3280 —
   see point 4 below), then `install_reload_result/2` (load + hot-reload)
   updates `beamtalk_workspace_meta`'s tracked source to match. Since every
   group already compiled successfully, `install_reload_result/2` failing
   here is expected to be rare (see its own doc — `code:load_binary/3`
   failing on a binary that just came out of a successful compile is a
   BEAM-level anomaly, not a source problem). It is still handled
   defensively: **the pathological case where install fails after
   validation passed** leaves every group installed BEFORE the failing one
   already live (there is still no cross-gen-server rollback — the ADR's own
   accepted limit) while the failing group and everything after it in
   install order never installed. `rewrite_sites/2` reports exactly this via
   `{error, {partial_install_failure, FailedClass, Reason, InstalledClasses}}`
   so the caller (and its ChangeLog bookkeeping) can tell a documented,
   bounded partial application apart from "nothing happened" — it must NOT
   be treated as equivalent to a clean validation-phase abort.
4. **Concurrent-writer detection at install time (BT-3280).** Steps 1-2 only
   guarantee atomicity WITHIN this one call — nothing serializes a batch
   against another eval-worker process concurrently mutating the same class
   via `Counter compile: '...'`, a single-site patch, or another
   `rewrite_sites/2` call entirely (each REPL/MCP/LSP session gets its own
   worker off `beamtalk_repl_shell`, and this module has never had a
   cross-session lock around `beamtalk_workspace_meta`'s read-then-later-
   write sequence — same class of race the single-site `capture/4`/
   `rollback/4` "two concurrent sessions... race" comment already
   acknowledges elsewhere in this module, just widened here roughly
   proportionally to batch size, since validating every OTHER group first
   can leave an early group's snapshot stale by the time its own turn to
   install comes up). Rather than a distributed lock (explicitly out of
   scope — this is detect-and-reject, not a mutex), each group re-checks its
   own class's CURRENT tracked source against its snapshot immediately
   before `?MODULE:install_reload_result/2` starts for that group. A
   mismatch (or the class no longer having a tracked source at all) means
   some other write landed in the validate-or-earlier-install window;
   `rewrite_sites/2` reports `{error, {stale_snapshot, Class}}` for that
   group and stops the batch there — installing this group's precomputed
   `new_source` anyway would silently discard the concurrent edit (the
   lost-update bug this exists to close). Groups already installed earlier
   in this same call stay installed (same bounded-partial-application shape
   point 3 already documents); nothing after the stale group installs. The
   caller can always retry safely: re-run site discovery against current
   state and call `rewrite_sites/2` again — a rejected install never
   corrupts anything, it only refuses to overwrite what it can no longer
   prove is still the snapshot it validated against.

   This check narrows the race window but does not fully close it: a writer
   that lands strictly BETWEEN `install_reload_result/2` returning
   (`code:load_binary/3` + `activate_module/4` — real work, not
   instantaneous) and this group's own `set_class_source/2` call a few lines
   later is not detected — that write's own tracked source is silently
   overwritten by this group's `set_class_source/2`, the one gap
   `class_source_unchanged/1`'s single check-before-install placement does
   not cover. Closing that residual gap would mean re-running the check
   again immediately before `set_class_source/2`, but by then this group's
   OWN code is already loaded and live (`install_reload_result/2` already
   ran) — failing at that point cannot "undo" the load, so it would trade one
   silent-overwrite bug for a different inconsistency (code live, tracked
   source refused) with no clean recovery story of its own. Left as a known,
   narrower residual instance of the same accepted-limitation class this
   module's `capture/4`/`rollback/4` comment already documents for the
   single-site path, rather than introduced as a new one.

Known, accepted limitation this protocol does NOT close: `compile_file/4`
itself has a side effect independent of installation — it registers each
compiled class's `referenced_aliases` into `beamtalk_alias_xref` (BT-2952,
`compile_file_core/4`'s own doc) as part of merely COMPILING, not installing.
A class-group that validates but is never installed (because an earlier or
later group in the same batch failed validation) can therefore still leave a
stale alias-xref registration behind. This is not a regression BT-3270
introduces — `reload_compile_and_load/4`'s existing single-site path already
has this exact property today (compile-time alias registration happens
before the install half runs) — so generalizing to N sites does not make it
qualitatively worse. Fixing it is `beamtalk_alias_xref` bookkeeping
robustness, not the class-reinstallation atomicity ADR 0114 asks this issue
to design.

## What this function does NOT do

No ChangeLog entry (see `emit_rewrite_change_entry/2`, a separate best-effort
call mirroring `emit_remove_change_entry/5`'s placement after `remove_method/3`)
and no `beamtalk_class_lifecycle`-style purge of a class's OLD registered
name — that only applies when a rename changes what name a class is
registered under, which is `renameTo:`'s own concern (out of scope: this
function never touches `beamtalk_class_registry`). Per-site xref reindexing
IS covered, but not via an explicit purge call: `activate_module/4`'s
`register_classes/2` → generated `register_class/0` → `beamtalk_object_class:
update_class/2` → `refresh_xref/2` already purges-then-reregisters a class's
xref rows on every ordinary recompile (ADR 0087 Phase 2) — this function's
job is simply to make sure THAT existing pipeline runs for every rewritten
site's own class, not just a single primary target, which is exactly the
"must be wired in explicitly" gap ADR 0114's Consequences section names.
Calling `beamtalk_class_lifecycle:purge_compiler_cache/1` additionally here
would be actively wrong, not merely redundant: it unconditionally drops a
class's `beamtalk_compiler_server` cache entry, which would immediately
erase the entry `activate_module/4`'s own `register_class/0` call just
freshly (re-)registered for a same-name class-group.
""".
-spec rewrite_sites(rewrite_site() | undefined, [rewrite_site()]) ->
    {ok, rewrite_result()}
    | {error,
        no_sites
        | {class_source_unavailable, binary()}
        | {invalid_or_overlapping_span, binary(), rewrite_span()}
        | {validation_failed, [{binary(), term()}]}
        | {partial_install_failure, binary(), term(), [binary()]}
        | {stale_snapshot, binary()}}.
rewrite_sites(undefined, []) ->
    {error, no_sites};
rewrite_sites(DefinitionSite, ReferenceSites) when is_list(ReferenceSites) ->
    case validate_and_group_sites(DefinitionSite, ReferenceSites) of
        {ok, Validated} -> install_rewrite_groups(Validated, DefinitionSite);
        {error, _} = Err -> Err
    end.

-doc """
Run `rewrite_sites/2`'s own validation pass (grouping + the non-mutating
compile-only check) WITHOUT installing anything — same pure prefix
`rewrite_sites/2` itself runs before its install pass, exposed standalone
for a caller that needs to know a rewrite WOULD succeed before committing
some other, unrelated mutation first (ADR 0114, BT-3278 review follow-up:
`beamtalk_behaviour_intrinsics:classRenameTo/2`'s dynamic-class path moves
the class's registry identity via a completely separate call,
`beamtalk_object_class:rename/2` — outside this module's own atomicity
protocol entirely — so it validates the reference-site rewrite here FIRST,
moves the identity second, and only then calls `rewrite_sites/2` itself to
actually install; that ordering is what keeps a rejected rewrite from ever
being preceded by a committed identity move).

Returns `ok` on success (the caller then repeats the same arguments to
`rewrite_sites/2` to actually install) or the same `{error, Reason}` shapes
`rewrite_sites/2` itself returns for its own validation-phase failures —
`no_sites`, `{class_source_unavailable, _}`, `{invalid_or_overlapping_span,
_, _}`, or `{validation_failed, _}`. Never returns
`{partial_install_failure, ...}` — that shape is specific to the install
pass this function never runs.
""".
-spec validate_sites(rewrite_site() | undefined, [rewrite_site()]) ->
    ok
    | {error,
        no_sites
        | {class_source_unavailable, binary()}
        | {invalid_or_overlapping_span, binary(), rewrite_span()}
        | {validation_failed, [{binary(), term()}]}}.
validate_sites(undefined, []) ->
    {error, no_sites};
validate_sites(DefinitionSite, ReferenceSites) when is_list(ReferenceSites) ->
    case validate_and_group_sites(DefinitionSite, ReferenceSites) of
        {ok, _Validated} -> ok;
        {error, _} = Err -> Err
    end.

%% One class's worth of a multi-site rewrite: every site targeting `class`,
%% merged into a single pre-computed post-splice source ready to compile.
-record(rewrite_class_group, {
    class :: binary(),
    module_name_override :: binary() | undefined,
    source_path :: string() | undefined,
    original_source :: binary(),
    new_source :: binary(),
    sites :: [rewrite_site()]
}).

%% Partition `Sites` by their `class` field, preserving first-seen class
%% order (so install order is deterministic) and each class's own sites in
%% their original relative order, then build a `#rewrite_class_group{}` per
%% class: fetch that class's CURRENT tracked source once, validate its sites'
%% spans don't overlap or run past the end of that source, and pre-compute
%% the merged post-splice source (`apply_site_splices/2`).
-spec group_sites_by_class([rewrite_site()]) ->
    {ok, [#rewrite_class_group{}]}
    | {error,
        {class_source_unavailable, binary()}
        | {invalid_or_overlapping_span, binary(), rewrite_span()}}.
group_sites_by_class(Sites) ->
    {ClassOrderRev, Grouped} = lists:foldl(
        fun(#{class := Class} = Site, {OrderAcc, MapAcc}) ->
            case maps:is_key(Class, MapAcc) of
                true -> {OrderAcc, MapAcc#{Class => [Site | maps:get(Class, MapAcc)]}};
                false -> {[Class | OrderAcc], MapAcc#{Class => [Site]}}
            end
        end,
        {[], #{}},
        Sites
    ),
    build_class_groups(lists:reverse(ClassOrderRev), Grouped, []).

-spec build_class_groups([binary()], #{binary() => [rewrite_site()]}, [#rewrite_class_group{}]) ->
    {ok, [#rewrite_class_group{}]}
    | {error,
        {class_source_unavailable, binary()}
        | {invalid_or_overlapping_span, binary(), rewrite_span()}}.
build_class_groups([], _Grouped, Acc) ->
    {ok, lists:reverse(Acc)};
build_class_groups([Class | Rest], Grouped, Acc) ->
    ClassSites = lists:reverse(maps:get(Class, Grouped)),
    case build_class_group(Class, ClassSites) of
        {ok, Group} -> build_class_groups(Rest, Grouped, [Group | Acc]);
        {error, _} = Err -> Err
    end.

-spec build_class_group(binary(), [rewrite_site()]) ->
    {ok, #rewrite_class_group{}}
    | {error,
        {class_source_unavailable, binary()}
        | {invalid_or_overlapping_span, binary(), rewrite_span()}}.
build_class_group(Class, Sites) ->
    case beamtalk_workspace_meta:get_class_source(Class) of
        undefined ->
            {error, {class_source_unavailable, Class}};
        Source ->
            SourceBin = unicode:characters_to_binary(Source),
            case validate_no_overlaps(Class, Sites, byte_size(SourceBin)) of
                ok ->
                    {ModuleNameOverride, SourcePath} = patch_module_target(Class),
                    {ok, #rewrite_class_group{
                        class = Class,
                        module_name_override = ModuleNameOverride,
                        source_path = SourcePath,
                        original_source = SourceBin,
                        new_source = apply_site_splices(SourceBin, Sites),
                        sites = Sites
                    }};
                {error, _} = Err ->
                    Err
            end
    end.

-doc """
The shared `{start, 'end'}` sort key for a `rewrite_span()` (ADR 0114,
BT-3270/BT-3274) — the ONE place every function that must order same-class
sites consistently derives its comparator from: `validate_no_overlaps/3`
(ascending, overlap detection), `apply_site_splices/2` (descending, so it
applies rightmost-first), and revert's own `current_spans_for_group/1`
(ascending — the inverse of `apply_site_splices/2`'s own ordering, needed to
replay the same cumulative-offset math left-to-right). A same-start tie (a
zero-length insertion sharing a start with a same-position replacement) MUST
sort identically across all three, or a mismatch can silently corrupt a
spliced source (PR #3522's finding) or misplace a reverted one — each call
site still picks its own ascending/descending direction (a genuine semantic
difference), but none may re-derive the KEY itself, which is what actually
needs to stay identical.
""".
-spec span_start_end(rewrite_span()) -> {non_neg_integer(), non_neg_integer()}.
span_start_end(#{start := Start, 'end' := End}) -> {Start, End}.

%% Every span for one class must be well-formed (`start =< end =< SourceSize`)
%% and disjoint from every other span for that same class — two sites
%% touching overlapping text is a structural bug in the caller's site list
%% (site discovery, out of scope here), not something a splice can resolve.
%% Sorting by start first turns pairwise overlap-checking into a single
%% linear "does the next span start at or after where the last one ended"
%% fold.
-spec validate_no_overlaps(binary(), [rewrite_site()], non_neg_integer()) ->
    ok | {error, {invalid_or_overlapping_span, binary(), rewrite_span()}}.
validate_no_overlaps(Class, Sites, SourceSize) ->
    %% Secondary sort key on `'end'` (not just `start`), via the shared
    %% `span_start_end/1` key — see that function's own doc for why two sites
    %% sharing a start (e.g. a zero-length insertion alongside a same-position
    %% replacement) must sort the same way here as in `apply_site_splices/2`
    %% (reversed, since it applies rightmost-first).
    Spans = lists:sort(
        fun(A, B) -> span_start_end(A) =< span_start_end(B) end,
        [maps:get(span, S) || S <- Sites]
    ),
    validate_spans(Class, Spans, 0, SourceSize).

-spec validate_spans(binary(), [rewrite_span()], non_neg_integer(), non_neg_integer()) ->
    ok | {error, {invalid_or_overlapping_span, binary(), rewrite_span()}}.
validate_spans(_Class, [], _Min, _Size) ->
    ok;
validate_spans(Class, [#{start := Start, 'end' := End} = Span | Rest], Min, Size) ->
    case Start =< End andalso End =< Size andalso Start >= Min of
        true -> validate_spans(Class, Rest, End, Size);
        false -> {error, {invalid_or_overlapping_span, Class, Span}}
    end.

%% Apply every site's splice to `Source` in one pass. Sites are applied
%% rightmost-span-first so that an earlier (lower-offset) site's byte offsets
%% are never invalidated by a splice whose replacement text has a different
%% length than the span it replaces — this is exactly what lets two sites in
%% the same class (e.g. a method-rename's definition plus a same-class
%% self-send) merge into one recompile instead of two independent ones that
%% would silently clobber each other.
%%
%% Ties on `start` (e.g. a zero-length insertion sharing a start with a
%% same-position replacement — `validate_no_overlaps/3` deterministically
%% accepts exactly this shape) break on `'end'`, descending: the larger-`end`
%% span (the "real" replacement) is applied first, so the zero-length
%% insertion is spliced against an accumulator where that replacement's new
%% text already begins at `start` — landing cleanly at that boundary rather
%% than being applied against the ORIGINAL span first and then having a
%% same-start sibling slice into already-shifted, no-longer-original-relative
%% bytes. Per review feedback on PR #3522: before this comparator existed,
%% `apply_site_splices/2`'s ties fell back to `lists:sort/2`'s stability
%% (the caller's original list order), which could corrupt the merged source
%% for the caller-order half of that tie that put the zero-length site first.
%% This uses the SAME `span_start_end/1` key `validate_no_overlaps/3` does
%% (only the direction differs) — see that function's own doc for why the key
%% itself must never be re-derived independently.
-spec apply_site_splices(binary(), [rewrite_site()]) -> binary().
apply_site_splices(Source, Sites) ->
    RightmostFirst = lists:sort(
        fun(#{span := SpanA}, #{span := SpanB}) ->
            span_start_end(SpanA) >= span_start_end(SpanB)
        end,
        Sites
    ),
    lists:foldl(
        fun(#{span := Span, new_text := NewText}, Acc) -> splice_replace(Acc, Span, NewText) end,
        Source,
        RightmostFirst
    ).

%% Phase 1 of the atomicity protocol: compile (never install) every group's
%% pre-computed post-splice source. All-or-nothing — a single failure aborts
%% before any group's `{Group, Compiled}` pair is even assembled, let alone
%% installed.
-spec validate_rewrite_groups([#rewrite_class_group{}]) ->
    {ok, [{#rewrite_class_group{}, term()}]} | {error, {validation_failed, [{binary(), term()}]}}.
validate_rewrite_groups(Groups) ->
    Results = [{Group, compile_rewrite_group(Group)} || Group <- Groups],
    %% Every element of Results is either `{G, {error, Reason}}` or
    %% `{G, {ok, ...}}` (compile_rewrite_group/1's only two return shapes) —
    %% when no failures exist, Results IS already the all-`{ok, ...}` list
    %% install_rewrite_groups/2 expects, in original group order.
    case [{G#rewrite_class_group.class, Reason} || {G, {error, Reason}} <- Results] of
        [] -> {ok, Results};
        Failures -> {error, {validation_failed, Failures}}
    end.

%% Shared prefix of `rewrite_sites/2` and `validate_sites/2`: group + validate
%% only, never install. `rewrite_sites/2` feeds this call's `{ok, Validated}`
%% straight into `install_rewrite_groups/2`; `validate_sites/2` just discards
%% `Validated` and reports `ok`.
-spec validate_and_group_sites(rewrite_site() | undefined, [rewrite_site()]) ->
    {ok, [{#rewrite_class_group{}, term()}]}
    | {error,
        no_sites
        | {class_source_unavailable, binary()}
        | {invalid_or_overlapping_span, binary(), rewrite_span()}
        | {validation_failed, [{binary(), term()}]}}.
validate_and_group_sites(undefined, []) ->
    {error, no_sites};
validate_and_group_sites(DefinitionSite, ReferenceSites) ->
    AllSites =
        case DefinitionSite of
            undefined -> ReferenceSites;
            _ -> [DefinitionSite | ReferenceSites]
        end,
    case group_sites_by_class(AllSites) of
        {ok, Groups} -> validate_rewrite_groups(Groups);
        {error, _} = Err -> Err
    end.

-spec compile_rewrite_group(#rewrite_class_group{}) ->
    {ok, protocol_definition, map()} | {ok, compiled, binary(), [map()], atom()} | {error, term()}.
compile_rewrite_group(#rewrite_class_group{
    module_name_override = ModuleNameOverride,
    source_path = SourcePath,
    new_source = NewSourceBin
}) ->
    LoadPath = source_path_or_empty(SourcePath),
    NewSourceStr = unicode:characters_to_list(NewSourceBin),
    compile_reload_source(NewSourceStr, LoadPath, ModuleNameOverride, undefined).

%% BT-3280: is `Group`'s class's CURRENT `beamtalk_workspace_meta` source
%% still byte-identical to what `build_class_group/2` snapshotted into
%% `original_source` before this batch's validation pass ran? `false` for a
%% class whose source is no longer trackable at all (e.g. removed by a
%% concurrent `removeFromSystem` in the same window) — that is exactly as
%% unsafe to install over as a changed source, not a separate case. See
%% `rewrite_sites/2`'s doc, point 4, for why this check exists and why it is
%% re-run per group immediately before that group's own install rather than
%% once up front.
-spec class_source_unchanged(#rewrite_class_group{}) -> boolean().
class_source_unchanged(#rewrite_class_group{class = Class, original_source = OriginalSource}) ->
    case beamtalk_workspace_meta:get_class_source(Class) of
        undefined -> false;
        CurrentSource -> unicode:characters_to_binary(CurrentSource) =:= OriginalSource
    end.

%% Phase 2 of the atomicity protocol: install every already-validated group,
%% in class-group order, updating `beamtalk_workspace_meta`'s tracked source
%% to match each newly-installed class. See `rewrite_sites/2`'s doc for the
%% pathological partial-install-failure case and the BT-3280 stale-snapshot
%% case this handles defensively.
-spec install_rewrite_groups([{#rewrite_class_group{}, term()}], rewrite_site() | undefined) ->
    {ok, rewrite_result()}
    | {error, {partial_install_failure, binary(), term(), [binary()]} | {stale_snapshot, binary()}}.
install_rewrite_groups(ValidatedGroups, DefinitionSite) ->
    install_rewrite_groups(ValidatedGroups, DefinitionSite, []).

-spec install_rewrite_groups(
    [{#rewrite_class_group{}, term()}], rewrite_site() | undefined, [#rewrite_class_group{}]
) ->
    {ok, rewrite_result()}
    | {error, {partial_install_failure, binary(), term(), [binary()]} | {stale_snapshot, binary()}}.
install_rewrite_groups([], DefinitionSite, InstalledRev) ->
    {ok, build_rewrite_result(DefinitionSite, lists:reverse(InstalledRev))};
install_rewrite_groups([{Group, Compiled} | Rest], DefinitionSite, InstalledRev) ->
    #rewrite_class_group{class = Class} = Group,
    case class_source_unchanged(Group) of
        false ->
            %% BT-3280: a concurrent writer landed on this class's tracked
            %% source between this batch's own validation pass and this
            %% group's own install turn. Fail cleanly and stop the batch here
            %% — never install `NewSource` over it, which would silently
            %% discard the other writer's edit (the lost-update bug this
            %% check exists to close). Everything strictly before this group
            %% in `InstalledRev` already installed and stays installed —
            %% same bounded-partial-application shape `partial_install_failure`
            %% below already has, just for a different, cleanly-detected cause.
            {error, {stale_snapshot, Class}};
        true ->
            install_rewrite_group(Group, Compiled, Rest, DefinitionSite, InstalledRev)
    end.

%% The actual load+hot-reload+tracked-source-update for one already-validated,
%% not-stale group — split out of `install_rewrite_groups/3` purely so that
%% function's own `case class_source_unchanged(Group) of ... end` reads as a
%% single guard clause rather than nesting this whole body a level deeper.
%% `SourcePath`/`NewSource` are read straight off `Group` rather than taking
%% them as separate params — they are already `Group`'s own fields, so
%% passing them alongside `Group` would just be re-stating what `Group`
%% already carries.
-spec install_rewrite_group(
    #rewrite_class_group{},
    term(),
    [{#rewrite_class_group{}, term()}],
    rewrite_site() | undefined,
    [#rewrite_class_group{}]
) ->
    {ok, rewrite_result()}
    | {error, {partial_install_failure, binary(), term(), [binary()]} | {stale_snapshot, binary()}}.
install_rewrite_group(Group, Compiled, Rest, DefinitionSite, InstalledRev) ->
    #rewrite_class_group{class = Class, source_path = SourcePath, new_source = NewSource} = Group,
    LoadPath = source_path_or_empty(SourcePath),
    %% BT-3280: qualified as `?MODULE:` (rather than a local call) purely so
    %% `beamtalk_repl_loader_rewrite_sites_tests.erl`'s `partial_install_failure`
    %% coverage can intercept it via `meck:new(?MODULE, [passthrough])` — see
    %% that test module's own moduledoc for why. Behaviourally identical to a
    %% local call in production (same module, same code version).
    case ?MODULE:install_reload_result(Compiled, LoadPath) of
        {ok, ClassNames} ->
            NewSourceStr = unicode:characters_to_list(NewSource),
            lists:foreach(
                fun(#{name := Name}) ->
                    beamtalk_workspace_meta:set_class_source(
                        normalize_class_source_key(Name), NewSourceStr
                    )
                end,
                ClassNames
            ),
            install_rewrite_groups(Rest, DefinitionSite, [Group | InstalledRev]);
        {error, Reason} ->
            InstalledClasses = [G#rewrite_class_group.class || G <- lists:reverse(InstalledRev)],
            {error, {partial_install_failure, Class, Reason, InstalledClasses}}
    end.

%% Build `rewrite_sites/2`'s success result from the installed groups: split
%% each group's sites back into "the definition site" (matched by
%% `{class, span}` against the caller's original `DefinitionSite`) and
%% "everything else", recording each site's pre-rewrite text (sliced from
%% its group's `original_source`, before any splice) alongside its `new_text`
%% under the ChangeLog's own `prev_source`/`source` field names.
-spec build_rewrite_result(rewrite_site() | undefined, [#rewrite_class_group{}]) ->
    rewrite_result().
build_rewrite_result(DefinitionSite, InstalledGroups) ->
    IndexedSites = lists:flatmap(
        fun(#rewrite_class_group{original_source = OrigSource, sites = Sites}) ->
            [installed_site(OrigSource, Site) || Site <- Sites]
        end,
        InstalledGroups
    ),
    case DefinitionSite of
        undefined ->
            #{definition => undefined, sites => IndexedSites};
        #{class := DefClass, span := DefSpan} ->
            {DefList, RefList} = lists:partition(
                fun(#{class := C, span := S}) -> C =:= DefClass andalso S =:= DefSpan end,
                IndexedSites
            ),
            case DefList of
                [DefInstalled | _] -> #{definition => DefInstalled, sites => RefList};
                [] -> #{definition => undefined, sites => IndexedSites}
            end
    end.

-spec installed_site(binary(), rewrite_site()) -> installed_rewrite_site().
installed_site(OriginalSource, #{
    class := Class, source_file := SourceFile, span := Span, new_text := NewText
}) ->
    #{
        class => Class,
        source_file => SourceFile,
        span => Span,
        prev_source => slice(OriginalSource, Span),
        source => NewText
    }.

-spec slice(binary(), rewrite_span()) -> binary().
slice(Source, #{start := Start, 'end' := End}) ->
    Len = End - Start,
    <<_Before:Start/binary, Text:Len/binary, _After/binary>> = Source,
    Text.

-doc """
Emit a `'rename-class'`/`'rename-method'` ChangeLog entry for a just-completed
`rewrite_sites/2` call (ADR 0114, BT-3270) — mirrors `emit_remove_change_entry/5`'s
placement (called by the caller AFTER the rewrite is already live; a
ChangeLog write failure never undoes an installed rewrite) and its best-
effort/self-swallowing failure handling.

`Spec` carries the identity fields the `sites`/`candidate_sites` schema
itself does not (ADR 0114 § ChangeLog schema) — `kind` (`'rename-class'` |
`'rename-method'`), the new `class` name, `intent`/`author`/`author_kind`,
and whichever of `selector`/`old_selector`/`side` (rename-method) or
`old_class`/`old_path`/`new_path` (rename-class) apply; `candidate_sites`
(rename-method's reported-never-rewritten senders) passes through verbatim —
this function neither computes nor validates it, since candidate-site
discovery is a future primitive's concern, not this mechanism's.

`flushable`/`not_flushable_reason` are derived generically from `RewriteResult`
via `classify_installed_site/1` + `beamtalk_workspace_changelog:sites_flushable/1`
— true iff every site (definition included) resolves to a flushable file,
matching both kinds' documented rule exactly. A `Definition` of `undefined`
(the dynamic-class case) always classifies the whole entry
`{not_flushable, <<"dynamic">>}`, matching both schemas' `not_flushable_reason:
"dynamic"` case. Each site's body is persisted to the ChangeLog's `sources/`
directory via `beamtalk_workspace_changelog:store_site_body/1` before the
entry itself is appended (a site's `source_ref`/`prev_source_ref` must
already be a written ref by the time `append/1` sees it — see that
function's doc).
""".
-spec emit_rewrite_change_entry(map(), rewrite_result()) -> ok.
emit_rewrite_change_entry(Spec, RewriteResult) ->
    try
        do_emit_rewrite_change_entry(Spec, RewriteResult)
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Failed to emit ChangeLog entry for multi-site rewrite (rewrite still installed)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    spec => Spec,
                    domain => [beamtalk, runtime]
                }
            ),
            ok
    end.

-spec do_emit_rewrite_change_entry(map(), rewrite_result()) -> ok.
do_emit_rewrite_change_entry(Spec, #{definition := Definition, sites := Sites}) ->
    #{
        kind := Kind,
        class := ClassNameBin,
        intent := Intent,
        author := Author,
        author_kind := AuthorKind
    } = Spec,
    AllInstalled = [Definition | Sites],
    {Flushable, NotFlushableReason} =
        beamtalk_workspace_changelog:sites_flushable(
            [classify_installed_site(S) || S <- AllInstalled]
        ),
    Entry = #{
        class => ClassNameBin,
        kind => Kind,
        selector => maps:get(selector, Spec, undefined),
        old_selector => maps:get(old_selector, Spec, undefined),
        side => maps:get(side, Spec, undefined),
        old_class => maps:get(old_class, Spec, undefined),
        old_path => maps:get(old_path, Spec, undefined),
        new_path => maps:get(new_path, Spec, undefined),
        sites => [site_append_input(S) || S <- AllInstalled],
        candidate_sites => maps:get(candidate_sites, Spec, undefined),
        intent => Intent,
        flushable => Flushable,
        not_flushable_reason => NotFlushableReason,
        author => Author,
        author_kind => AuthorKind
    },
    _ = beamtalk_workspace_changelog:append(Entry),
    ok.

%% `undefined` (the dynamic-class definition-site case) always classifies as
%% not-flushable/"dynamic", matching both `'rename-class'`'s and
%% `'rename-method'`'s documented dynamic-class row (ADR 0114 § Refusal vs
%% flushability) — a class with no backing file can never be flushed
%% regardless of what its other sites look like. Reuses `classify_source_file/1`
%% (already exported for exactly this kind of reuse — see its own callers)
%% rather than re-deriving stdlib/dependency/project classification.
-spec classify_installed_site(installed_rewrite_site() | undefined) ->
    flushable | {not_flushable, binary()}.
classify_installed_site(undefined) ->
    {not_flushable, <<"dynamic">>};
classify_installed_site(#{source_file := undefined}) ->
    {not_flushable, <<"dynamic">>};
classify_installed_site(#{source_file := SourceFile}) ->
    case classify_source_file(SourceFile) of
        {flushable, _AbsPath} -> flushable;
        {not_flushable, Reason} -> {not_flushable, Reason}
    end.

-spec site_append_input(installed_rewrite_site() | undefined) ->
    beamtalk_workspace_changelog:site() | undefined.
site_append_input(undefined) ->
    undefined;
site_append_input(#{
    source_file := SourceFile, span := Span, prev_source := PrevSource, source := Source
}) ->
    #{
        source_file => SourceFile,
        span => Span,
        source_ref => store_rewrite_site_ref(Source),
        prev_source_ref => store_rewrite_site_ref(PrevSource)
    }.

-spec store_rewrite_site_ref(binary()) -> binary() | undefined.
store_rewrite_site_ref(Body) ->
    case beamtalk_workspace_changelog:store_site_body(Body) of
        {ok, Ref} ->
            Ref;
        undefined ->
            undefined;
        {error, Reason} ->
            ?LOG_WARNING(
                "Failed to persist rewrite-site body to ChangeLog sources/ (site recorded without a body ref)",
                #{reason => Reason, domain => [beamtalk, runtime]}
            ),
            undefined
    end.

%%% ----------------------------------------------------------------------------
%%% Class move (ADR 0114 Phase 2, BT-3272)
%%% ----------------------------------------------------------------------------

-doc """
Move `ClassName`'s `.bt` file to `NewPathBin`, leaving its name and every
reference to it untouched (ADR 0114 Phase 2, BT-3272; backs `Workspace
moveClass:to:`).

Unlike `beamtalk_behaviour_intrinsics:classRenameTo/2` (BT-3278), nothing
about the class's identity changes here — no collision check, no cross-file
reference discovery, no registry re-registration — so `moveClass:to:` has no
sites beyond the single file being moved (ADR 0114 § "`Workspace
moveClass:to:`"). The mechanism is still the shared one (BT-3270): resolve
the class's own declaration span (`beamtalk_compiler:resolve_class_span/2`,
the same "header + state, never a method" resolver `add_class_def_flushability/2`
above already uses) and rewrite it to ITSELF — a byte-identical splice — via
`rewrite_sites/2`, purely so a `'rename-class'` ChangeLog entry with
`old_class == class`, `old_path` = the class's current `source_file`, and
`new_path = NewPathBin` gets recorded. `Workspace flush` (BT-3271) already
knows how to replay that entry as a pure file move (the declaration site,
plus any same-file self-reference, folded into one splice against `old_path`,
written to `new_path`, then `old_path` unlinked).

Reusing `rewrite_sites/2` rather than a bespoke "just log it" shortcut keeps
this on the one tested atomicity mechanism every other rename-shaped entry
already goes through — in particular, it still forces a real compile of the
class's current tracked source before anything is logged, catching a source
that has drifted into an unparseable state rather than silently recording a
move for a class that cannot actually be reinstalled later. Because the
registered name never changes, `rewrite_sites/2`'s own hot-reload reuses the
SAME pid (`beamtalk_behaviour_intrinsics:install_class_rename/3`'s doc: "a
NEW pid — hot-reload only reuses the SAME pid when the registered name is
unchanged, which a rename by definition is not" — a move by definition IS
unchanged) — the caller's own class-object reference stays valid, so this
returns `ok` rather than a reinstalled object; the FFI boundary
(`beamtalk_workspace_interface_primitives:moveClass/2`) returns the caller's
own `aClass` argument unchanged.

## Refusal (ADR 0114 § "`Workspace moveClass:to:`")

Classification reuses `capture_class_removal_snapshot/1` — the exact
stdlib/dependency/dynamic split `classRenameTo/2` already applies — with one
deliberate divergence: a dynamic (`ClassBuilder`) class has no backing file
to move at all, so it raises `no_source_file` here rather than
`classRenameTo/2`'s permissive `flushable: false` ("dynamic") treatment of
the identical classification — moving *nothing* is not a legitimate
in-memory action the way patching a dynamic class's body is. Stdlib and
dependency classes are refused for the same reason `classRenameTo/2` refuses
them: the classification this primitive shares with it only ever covers
in-project source, so neither primitive can vouch for a class living outside
it.

`NewPathBin` must resolve inside the active project tree
(`classify_source_file/1` — the same check `newClass:at:`'s
`validate_target_path/1` already applies): `Workspace flush` only ever
writes into the project tree, so a target outside it could never actually be
reached.

Returns `ok` on success or `{error, #beamtalk_error{}}` on any refusal,
resolution, or rewrite failure — the FFI boundary raises it.
""".
-spec move_class(atom(), binary()) -> ok | {error, #beamtalk_error{}}.
move_class(ClassName, NewPathBin) when is_atom(ClassName), is_binary(NewPathBin) ->
    ClassNameBin = atom_to_binary(ClassName, utf8),
    Classification = capture_class_removal_snapshot(ClassNameBin),
    case move_class_ensure_movable(ClassName, Classification) of
        {error, _} = Err ->
            Err;
        {ok, OldPathBin} ->
            case move_class_validate_target(ClassName, OldPathBin, NewPathBin) of
                {error, _} = Err ->
                    Err;
                ok ->
                    Source = maps:get(prev_source, Classification, undefined),
                    move_class_rewrite(ClassName, ClassNameBin, OldPathBin, NewPathBin, Source)
            end
    end.

%% Dynamic (no backing file at all) -> `no_source_file`, stricter than
%% `classRenameTo/2`'s permissive treatment of the same classification (see
%% `move_class/2`'s own doc for why). Stdlib / dependency -> refuse,
%% mirroring `classRenameTo/2`'s own refusal table exactly. An ordinary
%% flushable project class -> proceed, carrying its current `source_file`
%% forward as `old_path`.
-spec move_class_ensure_movable(atom(), map()) -> {ok, binary()} | {error, #beamtalk_error{}}.
move_class_ensure_movable(_ClassName, #{flushable := true, source_file := SourceFile}) ->
    {ok, SourceFile};
move_class_ensure_movable(ClassName, #{not_flushable_reason := <<"dynamic">>}) ->
    {error, move_class_no_source_file_error(ClassName)};
move_class_ensure_movable(ClassName, #{not_flushable_reason := <<"stdlib">>}) ->
    {error, move_class_stdlib_refusal_error(ClassName)};
move_class_ensure_movable(ClassName, #{not_flushable_reason := <<"dependency:", _/binary>> = Reason}) ->
    {error, move_class_dependency_refusal_error(ClassName, Reason)};
move_class_ensure_movable(ClassName, _Classification) ->
    %% Defensive: `capture_class_removal_snapshot/1` never produces any other
    %% shape (its only non-flushable reasons are "dynamic", "stdlib", or a
    %% "dependency:" prefix — see `no_source_reason/1`/`classify_source_file/1`)
    %% but fails the same way as the dynamic case rather than crashing on an
    %% unmatched map, per "never panic on user input".
    {error, move_class_no_source_file_error(ClassName)}.

-spec move_class_validate_target(atom(), binary(), binary()) -> ok | {error, #beamtalk_error{}}.
move_class_validate_target(ClassName, OldPathBin, NewPathBin) ->
    %% A target equal to the class's current path is not a legitimate move: the
    %% flush commit path for `op = move` (`beamtalk_workspace_flush:commit/1`)
    %% renames the staged .tmp into NewPath and then deletes OldPath — when the
    %% two are the same file, that delete removes the file it just wrote,
    %% losing the class's source entirely. Refuse eagerly, mirroring
    %% `newClass:at:`'s own eager `validate_target_path/1` check.
    case
        filename:absname(binary_to_list(NewPathBin)) =:=
            filename:absname(binary_to_list(OldPathBin))
    of
        true ->
            {error,
                move_class_error(
                    same_path,
                    ClassName,
                    <<"moveClass:to: target is the class's current path; nothing to move">>
                )};
        false ->
            case classify_source_file(NewPathBin) of
                {flushable, _AbsPath} ->
                    ok;
                {not_flushable, _Reason} ->
                    {error,
                        move_class_error(
                            target_outside_project,
                            ClassName,
                            iolist_to_binary([
                                <<"moveClass:to: target is outside the project source tree: ">>,
                                NewPathBin,
                                <<"; a class can only be moved to a path inside the current project">>
                            ])
                        )}
            end
    end.

-spec move_class_rewrite(atom(), binary(), binary(), binary(), binary() | undefined) ->
    ok | {error, #beamtalk_error{}}.
move_class_rewrite(ClassName, _ClassNameBin, _OldPathBin, _NewPathBin, undefined) ->
    {error,
        move_class_error(
            class_source_unavailable,
            ClassName,
            <<"moveClass:to: could not read this class's current tracked source">>
        )};
move_class_rewrite(ClassName, ClassNameBin, OldPathBin, NewPathBin, Source) ->
    case beamtalk_compiler:resolve_class_span(Source, ClassNameBin) of
        {ok, #{start := Start, 'end' := End} = Span, _PrevSource} ->
            Text = binary:part(Source, Start, End - Start),
            DefinitionSite = #{
                class => ClassNameBin,
                source_file => OldPathBin,
                span => Span,
                new_text => Text
            },
            case rewrite_sites(DefinitionSite, []) of
                {ok, RewriteResult} ->
                    emit_move_class_change_entry(
                        ClassNameBin, OldPathBin, NewPathBin, RewriteResult
                    ),
                    ok;
                {error, Reason} ->
                    {error, move_class_rewrite_failed_error(ClassName, Reason)}
            end;
        {error, Reason, Message} ->
            {error,
                move_class_error(
                    class_span_unresolved,
                    ClassName,
                    iolist_to_binary([
                        <<"moveClass:to: could not locate this class's own declaration: ">>,
                        io_lib:format("~p ~s", [Reason, Message])
                    ])
                )}
    end.

-spec emit_move_class_change_entry(binary(), binary(), binary(), rewrite_result()) -> ok.
emit_move_class_change_entry(ClassNameBin, OldPathBin, NewPathBin, RewriteResult) ->
    Spec = #{
        kind => 'rename-class',
        class => ClassNameBin,
        old_class => ClassNameBin,
        old_path => OldPathBin,
        new_path => NewPathBin,
        intent => durable,
        author => new_class_author(),
        author_kind => new_class_author_kind()
    },
    emit_rewrite_change_entry(Spec, RewriteResult).

-spec move_class_error(atom(), atom(), binary()) -> #beamtalk_error{}.
move_class_error(Kind, ClassName, Message) ->
    Err0 = beamtalk_error:new(Kind, ClassName),
    Err1 = beamtalk_error:with_selector(Err0, 'moveClass:to:'),
    beamtalk_error:with_message(Err1, Message).

-spec move_class_no_source_file_error(atom()) -> #beamtalk_error{}.
move_class_no_source_file_error(ClassName) ->
    ClassNameBin = atom_to_binary(ClassName, utf8),
    move_class_error(
        no_source_file,
        ClassName,
        iolist_to_binary([
            ClassNameBin,
            <<" has no backing .bt file to move (it was created dynamically via ClassBuilder)">>
        ])
    ).

-spec move_class_stdlib_refusal_error(atom()) -> #beamtalk_error{}.
move_class_stdlib_refusal_error(ClassName) ->
    ClassNameBin = atom_to_binary(ClassName, utf8),
    move_class_error(
        runtime_error,
        ClassName,
        iolist_to_binary([<<"Cannot move stdlib class '">>, ClassNameBin, <<"'">>])
    ).

-spec move_class_dependency_refusal_error(atom(), binary()) -> #beamtalk_error{}.
move_class_dependency_refusal_error(ClassName, Reason) ->
    ClassNameBin = atom_to_binary(ClassName, utf8),
    move_class_error(
        runtime_error,
        ClassName,
        iolist_to_binary([
            <<"Cannot move dependency class '">>, ClassNameBin, <<"' (">>, Reason, <<")">>
        ])
    ).

-spec move_class_rewrite_failed_error(atom(), term()) -> #beamtalk_error{}.
move_class_rewrite_failed_error(ClassName, Reason) ->
    ClassNameBin = atom_to_binary(ClassName, utf8),
    move_class_error(
        runtime_error,
        ClassName,
        iolist_to_binary(
            io_lib:format("Could not move class '~s': ~p", [ClassNameBin, Reason])
        )
    ).

%%% ----------------------------------------------------------------------------
%%% Multi-site rewrite revert (ADR 0114, BT-3274)
%%% ----------------------------------------------------------------------------

-doc """
Revert a pending `'rename-class'`/`'rename-method'` ChangeEntry (ADR 0114 §
Undo): rewrite every recorded `sites` entry back to its own `prev_source_ref`,
against that site's own recorded location — not by re-running `renameTo:`/
`renameSelector:to:`, which would re-execute xref discovery against
POST-rename state and could compute a different site list than the original
rename touched (a referencing file could have been independently edited in
between, per the ADR's own reasoning). `candidate_sites` are never touched —
they were never rewritten in the first place (ADR 0114: reported, never
auto-rewritten senders).

## Locating each site's CURRENT position

A `site()`'s own recorded `span` is where it sat in its owning class's source
BEFORE the original rewrite (`rewrite_sites/2`'s `installed_site/2` records
the caller's PRE-splice span verbatim — see that function's own doc). By the
time revert runs, that class's tracked source
(`beamtalk_workspace_meta:get_class_source/1`) has the NEW text (`source_ref`)
sitting there instead, at a position shifted by however much EARLIER
(lower-offset) sites in the SAME class-group changed length. `current_spans_
for_group/1` recomputes each site's live position by replaying that same
per-class cumulative-offset math forward from every site's own recorded
`span`/`source_ref` length — the same rightmost-first non-interference
property `apply_site_splices/2` relies on for splicing, run left-to-right
here to LOCATE positions instead.

## Resolving each site's owning class

A persisted `site()` carries `sourceFile`, not the class name that owns it
(dropped when `installed_site/2`'s own `class` field is flattened into the
ChangeLog's `site()` shape — see `site_append_input/1`) — and a file's
basename is not a safe class-name guess (`newClass:at:` accepts a
`to_snake_case/1` basename too, per `beamtalk_workspace_flush:derive_new_
path/3`'s doc). `class_names_by_source_file/0` asks the registry the same way
`class_source_file/1` resolves the FORWARD direction, for every currently
live class, and inverts it.

## Class-identity restoration (`'rename-class'` only)

Reference-site reversal alone is enough for `'rename-method'` (no identity
ever changed there). For `'rename-class'`, the definition site's own
reverse-splice (`sites[0]`'s `prev_source_ref`) already re-declares
`old_class` in the class's own source — `rewrite_sites/2`'s own install
pipeline therefore registers a fresh pid under `old_class` as an ordinary
side effect of that recompile, exactly mirroring how the FORWARD rename's own
definition-site splice is what originally registered the NEW name
(`beamtalk_behaviour_intrinsics:install_class_rename/3`'s doc). `finish_
rename_class_revert/1` only needs to retire the now-stale registration under
the CURRENT (post-rename) name — reusing `install_class_rename/3` itself,
called with the two names swapped, rather than a second copy of its
whereis/stop/purge sequence.

A dynamic class renamed with zero reference sites (`sites = [undefined]`,
ADR 0114's `sites[0] = null` case) has nothing to splice either way —
`do_revert_rewrite/2` mirrors `rewrite_class_sites/4`'s own trivial-success
shortcut for that shape, so only the identity move runs.

Returns `{ok, RevertedClassNameBin}` (the class's name AFTER revert —
`old_class` for `'rename-class'`, the entry's own stable `class` for
`'rename-method'`) on success, or `{error, Reason}` on any resolution/read/
rewrite failure — never a partial revert: a `rewrite_sites/2` validation
failure (the shared mechanism's own all-or-nothing guarantee) or a single
site whose owning class can no longer be resolved, or whose recorded body can
no longer be read, aborts before anything is spliced. The caller (`beamtalk_
workspace_interface_primitives:revert_rename_entry/2`) wraps any `{error, _}`
into a structured `#beamtalk_error{}` and retires the original entry
(`mark_flushed/1`, mirroring `'remove-class'` revert's own "undo emits no
fresh entry" convention) on success.

Before splicing, every touched class's CURRENT tracked source is verified to
still hold exactly the bytes the original rewrite left there
(`current_spans_for_group/1` → `verify_current_spans/1` — see that function's
own doc): an intervening, UNRELATED edit to one of those same classes between
the rename and this revert is refused loudly here rather than spliced over.
This is revert's own analogue of `'remove-class'` revert's explicit drift
check (`check_no_external_drift/3`, BT-3213), applied to a tracked-source
SPAN rather than a whole disk file, since a rename revert can touch several
classes' sources rather than one file. Ordinary method-patch revert
(`install_revert_patch/4`) still has no equivalent check and re-installs
`PrevBody` unconditionally — this function does not mirror that.
""".
-spec revert_rename_sites(beamtalk_workspace_changelog:entry()) ->
    {ok, binary()} | {error, term()}.
revert_rename_sites(Entry) ->
    case build_revert_sites(Entry) of
        {ok, {DefinitionSite, ReferenceSites}} ->
            revert_rename_sites(Entry, DefinitionSite, ReferenceSites);
        {error, _} = Err ->
            Err
    end.

%% `DefinitionSite =:= undefined` with a NON-empty `ReferenceSites` is the
%% dynamic-class `'rename-class'` signature (ADR 0114's `sites[0] = null`
%% shape, filtered out by `resolve_revert_sites/2` before this point — see
%% this section's own doc). That is the one shape where identity restore
%% (`finish_rename_revert/1`) and the reference-site splice are separate
%% mutations with no shared rollback, mirroring the forward path's identical
%% split (`beamtalk_behaviour_intrinsics:do_rename_and_rewrite/7`'s
%% dynamic-class clause). Splicing first and moving identity second — safe
%% for an ordinary class, whose definition-site splice performs the identity
%% move as a side effect of the SAME `rewrite_sites/2` transaction — would
%% leave reference sites pointing at a name nothing answers to if the
%% identity move then failed, with no way to retry (the sites are already
%% spliced, so `verify_current_spans/1`'s drift check refuses a second
%% attempt on the next call). Fixed the same way `do_rename_and_rewrite/7`
%% was: validate the splice FIRST (non-mutating), move identity second,
%% splice the reference sites (now expected to succeed) last. Every other
%% shape — an ordinary-class `'rename-class'` (`DefinitionSite` defined) or
%% any `'rename-method'` (identity never changes) — keeps the original
%% splice-then-finish order.
-spec revert_rename_sites(
    beamtalk_workspace_changelog:entry(), rewrite_site() | undefined, [rewrite_site()]
) -> {ok, binary()} | {error, term()}.
revert_rename_sites(Entry, undefined, [_ | _] = ReferenceSites) ->
    case beamtalk_workspace_changelog:entry_kind(Entry) of
        'rename-class' -> revert_dynamic_class_rename_sites(Entry, ReferenceSites);
        'rename-method' -> revert_rename_sites_splice_first(Entry, undefined, ReferenceSites)
    end;
revert_rename_sites(Entry, DefinitionSite, ReferenceSites) ->
    revert_rename_sites_splice_first(Entry, DefinitionSite, ReferenceSites).

-spec revert_dynamic_class_rename_sites(beamtalk_workspace_changelog:entry(), [rewrite_site()]) ->
    {ok, binary()} | {error, term()}.
revert_dynamic_class_rename_sites(Entry, ReferenceSites) ->
    case validate_sites(undefined, ReferenceSites) of
        ok ->
            case finish_rename_revert(Entry) of
                {ok, _RevertedClassNameBin} = Ok ->
                    case do_revert_rewrite(undefined, ReferenceSites) of
                        {ok, _RewriteResult} -> Ok;
                        {error, _} = Err -> Err
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

-spec revert_rename_sites_splice_first(
    beamtalk_workspace_changelog:entry(), rewrite_site() | undefined, [rewrite_site()]
) -> {ok, binary()} | {error, term()}.
revert_rename_sites_splice_first(Entry, DefinitionSite, ReferenceSites) ->
    case do_revert_rewrite(DefinitionSite, ReferenceSites) of
        {ok, _RewriteResult} -> finish_rename_revert(Entry);
        {error, _} = Err -> Err
    end.

%% Trivial-success shortcut mirroring `rewrite_class_sites/4`'s own
%% dynamic-class-with-nothing-to-rewrite case (see this section's own doc).
-spec do_revert_rewrite(rewrite_site() | undefined, [rewrite_site()]) ->
    {ok, rewrite_result()} | {error, term()}.
do_revert_rewrite(undefined, []) ->
    {ok, #{definition => undefined, sites => []}};
do_revert_rewrite(DefinitionSite, ReferenceSites) ->
    rewrite_sites(DefinitionSite, ReferenceSites).

-spec finish_rename_revert(beamtalk_workspace_changelog:entry()) ->
    {ok, binary()} | {error, term()}.
finish_rename_revert(Entry) ->
    case beamtalk_workspace_changelog:entry_kind(Entry) of
        'rename-class' -> finish_rename_class_revert(Entry);
        'rename-method' -> {ok, beamtalk_workspace_changelog:entry_class(Entry)}
    end.

%% Retire the stale post-rename registration and confirm `old_class` is now
%% live, reusing `install_class_rename/3` with the two names swapped (see
%% this section's own doc for why that is sound, not a coincidental fit).
-spec finish_rename_class_revert(beamtalk_workspace_changelog:entry()) ->
    {ok, binary()} | {error, term()}.
finish_rename_class_revert(Entry) ->
    CurrentNameBin = beamtalk_workspace_changelog:entry_class(Entry),
    OldNameBin = beamtalk_workspace_changelog:entry_old_class(Entry),
    case
        {
            beamtalk_repl_server:safe_to_existing_atom(CurrentNameBin),
            beamtalk_repl_server:safe_to_existing_atom(OldNameBin)
        }
    of
        {{ok, CurrentName}, {ok, OldName}} ->
            Classification = capture_class_removal_snapshot(CurrentNameBin),
            try
                _ = beamtalk_behaviour_intrinsics:install_class_rename(
                    CurrentName, OldName, Classification
                ),
                {ok, OldNameBin}
            catch
                error:#{error := #beamtalk_error{} = Err} -> {error, Err};
                Class:Reason -> {error, {rename_identity_restore_failed, Class, Reason}}
            end;
        _ ->
            {error,
                {rename_identity_restore_failed, unresolvable_class_atom,
                    {CurrentNameBin, OldNameBin}}}
    end.

%% Build the reversed `rewrite_site()` list from `Entry`'s recorded `sites`:
%% resolve each site's owning class + read both its recorded bodies (aborting
%% on the first unresolvable/unreadable one), recompute current positions per
%% class-group and verify each one still holds exactly the text the original
%% rewrite left there (`assign_current_spans/1` — an intervening, unrelated
%% edit to a touched class between the rename and this revert is refused
%% loudly here rather than spliced over), then split the result back into
%% `{DefinitionSite, ReferenceSites}` for `rewrite_sites/2`.
-spec build_revert_sites(beamtalk_workspace_changelog:entry()) ->
    {ok, {rewrite_site() | undefined, [rewrite_site()]}} | {error, term()}.
build_revert_sites(Entry) ->
    Sites = beamtalk_workspace_changelog:entry_sites(Entry),
    ClassMap = class_names_by_source_file(),
    case resolve_revert_sites(Sites, ClassMap) of
        {ok, Resolved} ->
            case assign_current_spans(Resolved) of
                {ok, WithSpans} -> {ok, split_definition(WithSpans)};
                {error, _} = Err -> Err
            end;
        {error, _} = Err ->
            Err
    end.

%% A `SourceFile -> ClassNameBin` reverse map built from every currently live
%% class (see this section's own doc for why a basename guess is not safe
%% here instead).
-spec class_names_by_source_file() -> #{binary() => binary()}.
class_names_by_source_file() ->
    lists:foldl(
        fun({Name, ModuleName, _Pid}, Acc) ->
            case beamtalk_reflection:source_file_from_module(ModuleName) of
                nil ->
                    Acc;
                SourceFile when is_binary(SourceFile) ->
                    Acc#{SourceFile => atom_to_binary(Name, utf8)}
            end
        end,
        #{},
        beamtalk_class_registry:live_class_entries()
    ).

%% One resolved (but not yet position-adjusted) revert site, as a plain map:
%% `is_definition` (was this `Entry`'s `sites[0]`?), `class` (this site's own
%% resolved owning class), `source_file`, `orig_span` (the recorded pre-rewrite
%% span), `cur_len` (the byte length of what's occupying that span NOW —
%% `source_ref`'s own body), `new_text` (what to splice back in —
%% `prev_source_ref`'s own body).
-type revert_site() :: #{
    is_definition := boolean(),
    class := binary(),
    source_file := binary(),
    orig_span := rewrite_span(),
    cur_len := non_neg_integer(),
    expected_current := binary(),
    new_text := binary()
}.

-spec resolve_revert_sites(
    [beamtalk_workspace_changelog:site() | undefined] | undefined, #{binary() => binary()}
) -> {ok, [revert_site()]} | {error, term()}.
resolve_revert_sites(Sites, ClassMap) ->
    %% `undefined` (never actually produced today — `entry_sites/1` is always
    %% a list, possibly `[undefined]` for a sourceless dynamic-class
    %% definition — BT-3269 § ChangeLog schema) degrades to "no sites",
    %% defensively, rather than crashing on a future producer that omits it.
    IndexedSites = lists:zip(
        lists:seq(0, length(sites_or_undefined(Sites)) - 1), sites_or_undefined(Sites)
    ),
    resolve_revert_sites(IndexedSites, ClassMap, []).

-spec sites_or_undefined([beamtalk_workspace_changelog:site() | undefined] | undefined) ->
    [beamtalk_workspace_changelog:site() | undefined].
sites_or_undefined(undefined) -> [];
sites_or_undefined(Sites) when is_list(Sites) -> Sites.

-spec resolve_revert_sites(
    [{non_neg_integer(), beamtalk_workspace_changelog:site() | undefined}],
    #{binary() => binary()},
    [revert_site()]
) -> {ok, [revert_site()]} | {error, term()}.
resolve_revert_sites([], _ClassMap, Acc) ->
    {ok, lists:reverse(Acc)};
resolve_revert_sites([{0, undefined} | Rest], ClassMap, Acc) ->
    %% `sites[0] = null`: the dynamic-class "no declaration site" case — never
    %% legitimately anywhere else in the list (ADR 0114 § ChangeLog schema).
    resolve_revert_sites(Rest, ClassMap, Acc);
resolve_revert_sites([{_Index, undefined} | _Rest], _ClassMap, _Acc) ->
    %% An `undefined` site past index 0 would violate the invariant the
    %% clause above relies on — refuse loudly (this module's own convention
    %% elsewhere: `verify_current_spans/1`, `check_no_external_drift/3`, ...)
    %% rather than silently under-reverting one site. Unreachable under every
    %% current ChangeLog producer; a defensive backstop, not a live path.
    {error, revert_site_unexpectedly_undefined};
resolve_revert_sites([{Index, Site} | Rest], ClassMap, Acc) ->
    case resolve_revert_site(Site, ClassMap, Index =:= 0) of
        {ok, Resolved} -> resolve_revert_sites(Rest, ClassMap, [Resolved | Acc]);
        {error, _} = Err -> Err
    end.

-spec resolve_revert_site(
    beamtalk_workspace_changelog:site(), #{binary() => binary()}, boolean()
) -> {ok, revert_site()} | {error, term()}.
resolve_revert_site(#{source_file := undefined}, _ClassMap, _IsDefinition) ->
    %% A NON-null site (this clause never reaches the dynamic-class
    %% `sites[0] = null` case — that is filtered out by index before this
    %% function is ever called) can still legitimately carry `source_file =
    %% undefined`: `rewrite_site()`'s own doc says the field is
    %% ChangeLog-attribution-only, and `class_source_file_for/1` returns
    %% `undefined` for a class with no backing file — reachable if a
    %% `'rename-method'` confirmed sender site lives in a sourceless dynamic
    %% subclass. `class_names_by_source_file/0`'s reverse lookup has no file
    %% to key by in that case, so this is reported distinctly from a
    %% resolvable-but-unrecognised file rather than folded into the generic
    %% `revert_site_malformed` catch-all below.
    {error, revert_site_no_source_file};
resolve_revert_site(#{source_file := SourceFile, span := Span} = Site, ClassMap, IsDefinition) when
    is_binary(SourceFile), is_map(Span)
->
    case maps:get(SourceFile, ClassMap, undefined) of
        undefined ->
            {error, {revert_site_class_unresolved, SourceFile}};
        ClassBin ->
            case
                {
                    beamtalk_workspace_changelog:read_site_body(
                        maps:get(source_ref, Site, undefined)
                    ),
                    beamtalk_workspace_changelog:read_site_body(
                        maps:get(prev_source_ref, Site, undefined)
                    )
                }
            of
                {{ok, CurrentBody}, {ok, PrevBody}} ->
                    {ok, #{
                        is_definition => IsDefinition,
                        class => ClassBin,
                        source_file => SourceFile,
                        orig_span => Span,
                        cur_len => byte_size(CurrentBody),
                        expected_current => CurrentBody,
                        new_text => PrevBody
                    }};
                _ ->
                    {error, {revert_site_body_unreadable, SourceFile, Span}}
            end
    end;
resolve_revert_site(_Site, _ClassMap, _IsDefinition) ->
    {error, revert_site_malformed}.

%% Group `RevertSites` by owning class and recompute + verify each group's
%% CURRENT spans (`current_spans_for_group/1`) — `rewrite_sites/2`'s own
%% grouping re-derives class membership from each `rewrite_site()`'s own
%% `class` field regardless of input order, so the flattened output order
%% here does not matter to correctness. Aborts on the first group whose
%% drift check fails, before any group is spliced.
-spec assign_current_spans([revert_site()]) ->
    {ok, [{revert_site(), rewrite_span()}]} | {error, term()}.
assign_current_spans(RevertSites) ->
    Grouped = lists:foldr(
        fun(#{class := Class} = S, Acc) ->
            maps:update_with(Class, fun(L) -> [S | L] end, [S], Acc)
        end,
        #{},
        RevertSites
    ),
    assign_current_spans_by_group(maps:values(Grouped), []).

-spec assign_current_spans_by_group([[revert_site()]], [{revert_site(), rewrite_span()}]) ->
    {ok, [{revert_site(), rewrite_span()}]} | {error, term()}.
assign_current_spans_by_group([], Acc) ->
    {ok, Acc};
assign_current_spans_by_group([Group | Rest], Acc) ->
    case current_spans_for_group(Group) of
        {ok, WithSpans} -> assign_current_spans_by_group(Rest, WithSpans ++ Acc);
        {error, _} = Err -> Err
    end.

-doc """
Within one class-group, sorted by each site's ORIGINAL span via the SAME
shared `span_start_end/1` key `validate_no_overlaps/3`/`apply_site_splices/2`
use (see that function's own doc for why the key itself must never be
re-derived independently): a site's CURRENT start offset is its own original
start plus the sum of every EARLIER (lower-original-offset) site's own length
delta (`cur_len - original span length`) — the same non-interference property
`apply_site_splices/2`'s rightmost-first application relies on for splicing,
replayed left-to-right here to LOCATE positions instead.

Once every site's current span is computed, `verify_current_spans/1` confirms
each one's owning class's CURRENT tracked source actually holds the exact
bytes the original rewrite left there (`expected_current`, the site's own
recorded `source_ref` body) before this group is trusted for splicing — an
intervening, unrelated edit to a touched class between the original rename
and this revert (e.g. an ordinary `compile:source:` patch landing on the
same file while the rename sat pending) would otherwise silently shift what
this cumulative-offset math computes as "current", corrupting the class on
splice rather than merely producing a stale result. This is revert's own
analogue of `check_no_external_drift/3`'s `'remove-class'`-specific disk
comparison (BT-3213) — same "never guess, refuse loudly" posture, applied to
a tracked-source SPAN rather than a whole disk file, since a rename revert
can touch several classes' sources rather than one file.
""".
-spec current_spans_for_group([revert_site()]) ->
    {ok, [{revert_site(), rewrite_span()}]} | {error, term()}.
current_spans_for_group(Group) ->
    Sorted = lists:sort(
        fun(#{orig_span := SpanA}, #{orig_span := SpanB}) ->
            span_start_end(SpanA) =< span_start_end(SpanB)
        end,
        Group
    ),
    %% `lists:mapfoldl/3` returns `{MappedList, FinalAcc}` — the mapped list
    %% FIRST, the accumulator SECOND (easy to get backwards; got it backwards
    %% once already during development, per this comment's own existence).
    {WithSpans, _Delta} = lists:mapfoldl(
        fun(#{orig_span := #{start := S, 'end' := E}, cur_len := CurLen} = Site, Delta) ->
            CurStart = S + Delta,
            CurEnd = CurStart + CurLen,
            NewDelta = Delta + (CurLen - (E - S)),
            {{Site, #{start => CurStart, 'end' => CurEnd}}, NewDelta}
        end,
        0,
        Sorted
    ),
    verify_current_spans(WithSpans).

%% Read the group's shared owning class's CURRENT tracked source ONCE and
%% confirm every site's computed span holds exactly its recorded
%% `expected_current` bytes there.
-spec verify_current_spans([{revert_site(), rewrite_span()}]) ->
    {ok, [{revert_site(), rewrite_span()}]} | {error, term()}.
verify_current_spans([]) ->
    {ok, []};
verify_current_spans([{#{class := Class}, _} | _] = WithSpans) ->
    case beamtalk_workspace_meta:get_class_source(Class) of
        undefined ->
            {error, {revert_class_source_unavailable, Class}};
        Source ->
            check_spans_match(WithSpans, unicode:characters_to_binary(Source))
    end.

-spec check_spans_match([{revert_site(), rewrite_span()}], binary()) ->
    {ok, [{revert_site(), rewrite_span()}]} | {error, term()}.
check_spans_match([], _SourceBin) ->
    {ok, []};
check_spans_match(
    [
        {#{class := Class, expected_current := Expected} = Site, #{start := S, 'end' := E} = Span}
        | Rest
    ],
    SourceBin
) ->
    Len = E - S,
    case SourceBin of
        <<_:S/binary, Expected:Len/binary, _/binary>> ->
            case check_spans_match(Rest, SourceBin) of
                {ok, More} -> {ok, [{Site, Span} | More]};
                {error, _} = Err -> Err
            end;
        _ ->
            {error, {revert_site_drifted, Class, Span}}
    end.

-spec split_definition([{revert_site(), rewrite_span()}]) ->
    {rewrite_site() | undefined, [rewrite_site()]}.
split_definition(SitesWithSpans) ->
    lists:foldl(
        fun({#{is_definition := IsDef} = Site, CurSpan}, {DefAcc, RefAcc}) ->
            RewriteSite = to_rewrite_site(Site, CurSpan),
            case IsDef of
                true -> {RewriteSite, RefAcc};
                false -> {DefAcc, [RewriteSite | RefAcc]}
            end
        end,
        {undefined, []},
        SitesWithSpans
    ).

-spec to_rewrite_site(revert_site(), rewrite_span()) -> rewrite_site().
to_rewrite_site(#{class := Class, source_file := SourceFile, new_text := NewText}, CurSpan) ->
    #{class => Class, source_file => SourceFile, span => CurSpan, new_text => NewText}.

%% Load a recompiled method-patched class binary into BEAM.
-spec load_recompiled_method(
    binary(), atom(), list(), map(), string(), string(), [binary()], beamtalk_repl_state:state()
) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
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
    Side = patch_side(maps:get(is_class_method, MethodInfo, false)),
    %% ADR 0105 Phase 1 (BT-2777): capture the freshly-compiled signature into
    %% the signature-generation store BEFORE the patch installs. Must run here
    %% (not after code:load_binary below) — install reloads the class's
    %% compiled module under its *existing* atom, so a first-ever capture made
    %% after install would seed from the just-installed module's own
    %% __beamtalk_meta/0 (comparing the new generation against itself) instead
    %% of the true pre-patch original. Rolled back on the {error, LoadReason}
    %% branch below, so a load failure never leaves the store holding a
    %% generation that was never actually live. Best-effort: a store failure
    %% must never block the install.
    CaptureOutcome = capture_signature_generation(MethodInfo),
    %% Pass the class's on-disk source path (when known) so `code:which/1`
    %% reports a real path — keeping a patched project class classified as a
    %% project class, not "stdlib"/"dynamic" (BT-2553 follow-up).
    case code:load_binary(ModName, SourcePath, Binary) of
        {module, ModName} ->
            %% (2) Install in memory. The memory install is the visible effect;
            %% the ChangeEntry below is step (3) — emitted only after install
            %% succeeds (all-or-nothing between install and log, ADR 0082).
            activate_module(ModName, Classes),
            %% Update all classes compiled in this module so sibling class entries
            %% reflect the latest combined source and stay consistent for future >> calls.
            lists:foreach(
                fun(#{name := Name}) ->
                    NameBin = normalize_class_source_key(Name),
                    beamtalk_workspace_meta:set_class_source(NameBin, CombinedSource)
                end,
                Classes
            ),
            %% (3) Emit a ChangeEntry for the live patch (ADR 0082 Phase 1).
            %% Best-effort: a ChangeLog failure must never fail the install — the
            %% method is already live in memory. emit_change_entry/1 logs and
            %% swallows its own errors.
            emit_change_entry(MethodInfo),
            %% (4) ADR 0082 Phase 4: when the workspace is in `autoflush: true'
            %% mode, every successful durable in-memory patch is immediately
            %% flushed to disk. Best-effort and synchronous; a flush failure does
            %% NOT roll back the BEAM module install (prior binary may already be
            %% unloaded and live actors may hold references to the new closures)
            %% — the entry simply stays pending in the log for manual flush
            %% reconciliation. Ephemeral patches are not autoflushed because
            %% only durable+flushable entries are written by `flush/0'.
            maybe_autoflush(maps:get(intent, MethodInfo, durable)),
            %% (5) ADR 0105 Phase 1 (BT-2778): re-check known dependents of a
            %% signature_change/removal now that the new generation is live.
            %% Best-effort, never affects this reply's *content* — see the
            %% function doc — but it IS synchronous here, so it does delay
            %% this reply by the re-check's wall time (bounded by the caller
            %% cap; ~18.5ms/candidate warm per the Phase 0 spike, so normally
            %% sub-second even at the default cap of 20). Moving this off the
            %% install's critical path is BT-2779's concern once findings
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
            maybe_trigger_recheck(ClassNameBin, SelectorBin, Side, CaptureOutcome),
            Result = <<ClassNameBin/binary, ">>", SelectorBin/binary>>,
            {ok, Result, <<>>, AllWarnings, State};
        {error, LoadReason} ->
            %% ADR 0105 Phase 1 (BT-2777): the install this capture described
            %% never happened — undo it so the store still reflects the actually
            %% live generation.
            rollback_signature_generation(ClassNameBin, SelectorBin, Side, CaptureOutcome),
            ClassAtoms = class_name_atoms(Classes),
            case beamtalk_runtime_api:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError, <<>>, AllWarnings, State};
                [] ->
                    {error, {load_error, LoadReason}, <<>>, AllWarnings, State}
            end
    end.

%%% ----------------------------------------------------------------------------
%%% New-class creation (ADR 0082 Phase 1, BT-2285)
%%% ----------------------------------------------------------------------------

-doc """
Create a brand-new class from a source String at `TargetPath` (ADR 0082 Phase 1).

Compiles and installs the class in memory, then logs a `kind: "new-class"`
ChangeEntry (`intent: durable`, `flushable: true`, `prev_source = nil`,
`span = nil`, full source). Phase 1 does NOT write `TargetPath` to disk — the
file is written later by `Workspace flush` (Phase 2), which replays the
new-class entry. The entry records `sourceFile = TargetPath` so the flush knows
where to write.

Validation is loud and specific — every failure is an `#beamtalk_error{}` with
no silent fallback (ADR 0082, *`Workspace newClass:` validation*). The op raises
when, in order:

  (a) `TargetPath` already exists on disk;
  (b) `TargetPath` lies outside the project source tree;
  (c) the declared class name does not match the basename of `TargetPath`
      (one-class-per-file convention, ADR 0040);
  (d) a class with that name is already loaded in memory.

On success returns `{ok, [ClassObject]}` (the loaded class object(s), matching
`load:`); on any validation/compile/install failure returns
`{error, #beamtalk_error{}}` so the FFI boundary can raise it.
""".
-spec new_class(binary() | string(), binary() | string()) ->
    {ok, [#beamtalk_object{}]} | {error, #beamtalk_error{}}.
new_class(Source, TargetPath) when is_binary(Source) ->
    new_class(binary_to_list(Source), TargetPath);
new_class(Source, TargetPath) when is_binary(TargetPath) ->
    new_class(Source, binary_to_list(TargetPath));
new_class(Source, TargetPath) when is_list(Source), is_list(TargetPath) ->
    %% (a) TargetPath must not already exist on disk; (b) must be in-project.
    %% These checks run before compiling so a bad path fails fast and cheaply.
    case validate_target_path(TargetPath) of
        {ok, AbsPath} ->
            new_class_compile(Source, TargetPath, AbsPath);
        {error, _} = PathErr ->
            PathErr
    end;
new_class(_Source, _TargetPath) ->
    {error, new_class_type_error(<<"newClass:at: expects String source and path arguments">>)}.

-doc """
Recompile and reinstall a class from a recorded prior source, reusing the same
compile+install chokepoint `newClass:at:` uses (ADR 0082, BT-2664) rather than
a second whole-class-install mechanism (ADR 0113, BT-3208 — `Workspace changes
revert:` extended to a pending `'remove-class'` entry).

`TargetPath` is the removed class's own recorded `sourceFile` (already an
absolute path — `class_source_file/1`'s value at removal time), and `Source`
is the entry's `prev_source_ref` body: the exact whole-file text that was on
disk immediately before the removal.

Deliberately skips `new_class/2`'s `validate_target_path/1` step: that check
exists to stop a *fresh* `newClass:at:` from silently overwriting an unrelated
file, but a revert of a still-*pending* `'remove-class'` entry is restoring a
file that was never actually deleted — Tier 2 (`flushIncludingDestructive`,
ADR 0113) is the only thing that unlinks it, and a pending entry never reached
that step. Requiring the target's *absence* here would reject the exact case
this function exists to handle. Every other `newClass:at:` validation still
runs via `new_class_compile/3`'s existing chain (declared name matches
`TargetPath`'s basename, no class of that name is already loaded).

Installs in `no_log` mode (see `new_class_install/8`): the reinstalled file was
never actually deleted (revert is pre-flush-only), so its content already
matches disk — emitting a fresh `'new-class'`-kind ChangeEntry here would
permanently misrepresent it as a pending brand-new-file addition (that kind's
`clean` check is hardcoded "always pending", and any later flush would treat
the never-deleted file as an unresolvable `target_exists` conflict). The
caller is responsible for retiring the original `'remove-class'` entry (e.g.
`mark_flushed/1`) once this returns successfully — undoing a removal, like
undoing an add (`revert_removal/3`'s `'new-class'` case), does not itself
emit a new ChangeEntry.

The "its content already matches disk" assumption above holds only absent a
concurrent out-of-band edit (another session, git, an editor touching the
file while the removal sat pending) — this function does not itself guard
against that. The caller,
`beamtalk_workspace_interface_primitives:reinstall_reverted_class/3`, runs
`check_no_external_drift/3` first and never calls this function at all if the
on-disk file has drifted from the recorded `prev_source_ref` snapshot (BT-3213,
Claude review follow-up on BT-3208) — see that function's doc.
""".
-spec revert_remove_class(binary() | string(), binary() | string()) ->
    {ok, [#beamtalk_object{}]} | {error, #beamtalk_error{}}.
revert_remove_class(Source, TargetPath) when is_binary(Source) ->
    revert_remove_class(binary_to_list(Source), TargetPath);
revert_remove_class(Source, TargetPath) when is_binary(TargetPath) ->
    revert_remove_class(Source, binary_to_list(TargetPath));
revert_remove_class(Source, TargetPath) when is_list(Source), is_list(TargetPath) ->
    new_class_compile(Source, TargetPath, TargetPath, no_log);
revert_remove_class(_Source, _TargetPath) ->
    {error,
        new_class_type_error(<<"revert: remove-class reinstall expects String source and path">>)}.

%% Compile (without installing) to discover the declared class name, validate
%% (c) name == basename and (d) not already loaded, then install (+ log unless
%% `Mode` is `no_log` — see `new_class_install/8`).
-spec new_class_compile(string(), string(), string()) ->
    {ok, [#beamtalk_object{}]} | {error, #beamtalk_error{}}.
new_class_compile(Source, TargetPath, AbsPath) ->
    new_class_compile(Source, TargetPath, AbsPath, log).

-spec new_class_compile(string(), string(), string(), log | no_log) ->
    {ok, [#beamtalk_object{}]} | {error, #beamtalk_error{}}.
new_class_compile(Source, TargetPath, AbsPath, Mode) ->
    ModuleNameOverride = compute_package_module_name(TargetPath),
    StdlibMode = is_stdlib_path(TargetPath),
    %% `compile_file/4`'s success-typing return here is a class binary or an
    %% error (the protocol-definition variant declared in its spec is produced by
    %% a different compiler entry, not this one — dialyzer confirms it can never
    %% arrive). A protocol-only source therefore surfaces as a compile error or
    %% as a class-less result that `declared_class_name([])` rejects loudly.
    case beamtalk_repl_compiler:compile_file(Source, TargetPath, StdlibMode, ModuleNameOverride) of
        {ok, Binary, ClassNames, ModuleName} ->
            new_class_validate_and_install(
                Source, TargetPath, AbsPath, Binary, ClassNames, ModuleName, Mode
            );
        {error, Reason} ->
            {error, beamtalk_repl_errors:ensure_structured_error(Reason)}
    end.

%% With a successful compile, finish validation against the declared class name,
%% then install the already-compiled binary (+ log unless `Mode` is `no_log`).
-spec new_class_validate_and_install(
    string(), string(), string(), binary(), [map()], atom(), log | no_log
) -> {ok, [#beamtalk_object{}]} | {error, #beamtalk_error{}}.
new_class_validate_and_install(Source, TargetPath, AbsPath, Binary, ClassNames, ModuleName, Mode) ->
    case declared_class_name(ClassNames) of
        {error, _} = NameErr ->
            NameErr;
        {ok, DeclaredName} ->
            case validate_new_class(DeclaredName, TargetPath, class_loaded(DeclaredName)) of
                ok ->
                    new_class_install(
                        Source,
                        TargetPath,
                        AbsPath,
                        Binary,
                        ClassNames,
                        ModuleName,
                        DeclaredName,
                        Mode
                    );
                {error, _} = ValErr ->
                    ValErr
            end
    end.

%% Install the compiled binary in memory (mirrors load_compiled_module/6's
%% activation path, but stateless). In `log` mode (ordinary `newClass:at:`,
%% ADR 0082) also emits the durable new-class ChangeEntry and autoflushes; a
%% ChangeLog failure does not undo the install — the class is already live.
%% In `no_log` mode (a `'remove-class'` revert, ADR 0113 BT-3208 — see
%% `revert_remove_class/2`'s doc) does neither: the file being reinstalled was
%% never deleted, so there is nothing new to log or flush.
-spec new_class_install(
    string(), string(), string(), binary(), [map()], atom(), binary(), log | no_log
) -> {ok, [#beamtalk_object{}]} | {error, #beamtalk_error{}}.
new_class_install(Source, TargetPath, AbsPath, Binary, ClassNames, ModuleName, DeclaredName, Mode) ->
    %% BT-2856 / ADR 0107 Phase A, BT-2873 hardening: see load_class_binary/4's doc.
    case load_class_binary(ModuleName, AbsPath, Binary, ClassNames) of
        {ok, NewlyNonLeafSuperclasses} ->
            activate_module(ModuleName, ClassNames, AbsPath, NewlyNonLeafSuperclasses),
            %% Record class source so subsequent `>>` / compile:source: patches
            %% against the new class resolve their span (mirrors the file-load path).
            lists:foreach(
                fun(#{name := Name}) ->
                    beamtalk_workspace_meta:set_class_source(
                        normalize_class_source_key(Name), Source
                    )
                end,
                ClassNames
            ),
            case Mode of
                log ->
                    emit_new_class_entry(
                        DeclaredName, list_to_binary(Source), list_to_binary(AbsPath)
                    ),
                    %% ADR 0082 Phase 4: autoflush also covers new-class entries
                    %% (they are durable + flushable by construction). See the
                    %% analogous comment in load_recompiled_method/7 for the
                    %% failure semantics.
                    maybe_autoflush(durable);
                no_log ->
                    ok
            end,
            {ok, loaded_class_objects(ClassNames)};
        {error, LoadReason} ->
            ClassAtoms = class_name_atoms(ClassNames),
            case beamtalk_runtime_api:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError};
                [] ->
                    {error,
                        new_class_error(
                            new_class_load_failed,
                            iolist_to_binary(
                                io_lib:format("Could not load new class: ~p", [LoadReason])
                            ),
                            TargetPath
                        )}
            end
    end.

%% Validate (a) the path does not already exist and (b) it is inside the project
%% source tree. Returns the absolute path on success. `classify_source_file/1`
%% already encodes the in-project containment rule (and treats a workspace with
%% no project context as "outside", which is the correct conservative answer —
%% there is no tree to create the file in).
-spec validate_target_path(string()) -> {ok, string()} | {error, #beamtalk_error{}}.
validate_target_path(TargetPath) ->
    case file:read_file_info(TargetPath) of
        {error, enoent} ->
            case classify_source_file(list_to_binary(TargetPath)) of
                {flushable, AbsPath} ->
                    {ok, AbsPath};
                {not_flushable, _Reason} ->
                    {error,
                        new_class_error(
                            target_outside_project,
                            iolist_to_binary([
                                <<"newClass:at: target is outside the project source tree: ">>,
                                list_to_binary(TargetPath),
                                <<"; new classes must be created inside the current project">>
                            ]),
                            TargetPath
                        )}
            end;
        _Other ->
            %% Any existing filesystem entry (regular file, directory, symlink)
            %% blocks new-class; also treat unreadable paths (eaccess, etc.) as
            %% existing rather than silently overwriting.
            {error,
                new_class_error(
                    target_exists,
                    iolist_to_binary([
                        <<"newClass:at: target already exists on disk: ">>,
                        list_to_binary(TargetPath),
                        <<"; use compile:source: against the existing class, or choose a new path">>
                    ]),
                    TargetPath
                )}
    end.

-doc """
Extract the single declared class name from a compile result's class list.

Enforces the one-class-per-file convention (ADR 0040): `newClass:at:` accepts
exactly one class. An empty list (no class declared) or more than one class is a
loud error. Pure — exported for tests.
""".
-spec declared_class_name([map()]) -> {ok, binary()} | {error, #beamtalk_error{}}.
declared_class_name([#{name := Name}]) ->
    {ok, normalize_class_source_key(Name)};
declared_class_name([]) ->
    {error,
        new_class_error(
            no_class_declared,
            <<"newClass:at: source does not declare a class">>,
            undefined
        )};
declared_class_name(ClassNames) when length(ClassNames) > 1 ->
    Names = [normalize_class_source_key(N) || #{name := N} <- ClassNames],
    {error,
        new_class_error(
            multiple_classes_declared,
            iolist_to_binary([
                <<"newClass:at: source declares multiple classes (">>,
                lists:join(<<", ">>, Names),
                <<"); one class per file (ADR 0040)">>
            ]),
            undefined
        )}.

-doc """
Validate the declared class name against the target path (ADR 0082 Phase 1).

Checks (c) the declared name matches the basename of `TargetPath` (one class per
file, ADR 0040) and (d) no class of that name is already loaded (`Loaded` is the
caller-supplied result of `class_loaded/1`, threaded in so this helper stays
pure and unit-testable). Returns `ok` or `{error, #beamtalk_error{}}`.

The basename match is *snake_case-normalised* so both established file-naming
conventions are accepted for a class `Greeter`: `Greeter.bt` (PascalCase, the
stdlib convention) and `greeter.bt` (snake_case, the examples/fixtures
convention). Both resolve to the same module name in the compiler, so both are
"matching" here. The class name must still be the same word as the file stem —
`Welcomer` at `greeter.bt` is rejected.
""".
-spec validate_new_class(binary(), string(), boolean()) -> ok | {error, #beamtalk_error{}}.
validate_new_class(DeclaredName, TargetPath, Loaded) ->
    BaseName = filename:basename(TargetPath, ".bt"),
    Expected = list_to_binary(BaseName),
    %% Accept either an exact match (Greeter.bt) or a snake_case match
    %% (greeter.bt) — both map to the same module name as the class.
    DeclaredSnake = to_snake_case(binary_to_list(DeclaredName)),
    BaseSnake = to_snake_case(BaseName),
    Matches = (DeclaredName =:= Expected) orelse (DeclaredSnake =:= BaseSnake),
    case Matches of
        false ->
            {error,
                new_class_error(
                    class_name_mismatch,
                    iolist_to_binary([
                        <<"newClass:at: declared class ">>,
                        DeclaredName,
                        <<" does not match basename '">>,
                        Expected,
                        <<"' of ">>,
                        list_to_binary(TargetPath),
                        <<"; either rename the class to match the basename, or use a path with basename ">>,
                        DeclaredName,
                        <<".bt or ">>,
                        list_to_binary(DeclaredSnake),
                        <<".bt. One class per file (ADR 0040)">>
                    ]),
                    TargetPath
                )};
        true when Loaded ->
            {error,
                new_class_error(
                    class_already_loaded,
                    iolist_to_binary([
                        <<"newClass:at: class ">>,
                        DeclaredName,
                        <<" is already loaded; use compile:source: against it, or remove it first">>
                    ]),
                    TargetPath
                )};
        true ->
            ok
    end.

%% True iff a class of this name is currently registered in the runtime.
-spec class_loaded(binary()) -> boolean().
class_loaded(ClassNameBin) ->
    case beamtalk_repl_server:safe_to_existing_atom(ClassNameBin) of
        {ok, ClassName} ->
            is_pid(beamtalk_class_registry:whereis_class(ClassName));
        {error, _} ->
            %% Name has never been interned as an atom, so it cannot be a loaded
            %% class — safe to treat as not loaded.
            false
    end.

%% Resolve loaded class info maps to Beamtalk class objects (same shape `load:`
%% returns). Reuses the workspace primitives' helper so the FFI surfaces the
%% created class to the REPL identically to a file load.
-spec loaded_class_objects([map()]) -> [#beamtalk_object{}].
loaded_class_objects(ClassNames) ->
    beamtalk_workspace_interface_primitives:loaded_class_objects(ClassNames).

%% Trigger `Workspace flush' when `autoflush: true' is set on the workspace
%% (ADR 0082 Phase 4, BT-2290). Best-effort and synchronous:
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
    case beamtalk_workspace_meta:get_setting(autoflush, false) of
        true -> do_autoflush();
        _ -> ok
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

%% Emit the durable `new-class` ChangeEntry for a freshly created class. Best
%% effort: a ChangeLog write must never fail or undo the in-memory install (the
%% class is already live), mirroring emit_change_entry/1's contract.
-spec emit_new_class_entry(binary(), binary(), binary()) -> ok.
emit_new_class_entry(ClassNameBin, Source, SourceFile) ->
    try
        Entry = #{
            class => ClassNameBin,
            kind => 'new-class',
            source => Source,
            %% Explicit per ADR 0082 contract: new-class entries carry no prior
            %% disk body and no byte span (the file does not yet exist).
            prev_source => undefined,
            span => undefined,
            intent => durable,
            flushable => true,
            source_file => SourceFile,
            author => new_class_author(),
            author_kind => new_class_author_kind()
        },
        _ = beamtalk_workspace_changelog:append(Entry),
        ok
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Failed to emit ChangeLog entry for new class (class still installed)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    class_name => ClassNameBin,
                    domain => [beamtalk, runtime]
                }
            ),
            ok
    end.

%% Audit author for a new-class entry. MCP `save_class` stamps `agent` into the
%% process dictionary at the submission boundary (same mechanism `compile:source:`
%% uses); a direct REPL call defaults to `human`/`repl`.
-spec new_class_author() -> binary().
new_class_author() ->
    case erlang:get('$beamtalk_author') of
        A when is_binary(A) -> A;
        _ -> new_class_default_author()
    end.

-spec new_class_default_author() -> binary().
new_class_default_author() ->
    case erlang:get('$beamtalk_author_kind') of
        agent -> <<"agent">>;
        _ -> <<"repl">>
    end.

-spec new_class_author_kind() -> human | agent.
new_class_author_kind() ->
    case erlang:get('$beamtalk_author_kind') of
        agent -> agent;
        _ -> human
    end.

-spec new_class_error(atom(), binary(), string() | undefined) -> #beamtalk_error{}.
new_class_error(Kind, Message, TargetPath) ->
    Err0 = beamtalk_error:new(Kind, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'newClass:at:'),
    Err2 = beamtalk_error:with_message(Err1, Message),
    case TargetPath of
        undefined -> Err2;
        _ -> beamtalk_error:with_details(Err2, #{target => list_to_binary(TargetPath)})
    end.

-spec new_class_type_error(binary()) -> #beamtalk_error{}.
new_class_type_error(Message) ->
    Err0 = beamtalk_error:new(type_error, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'newClass:at:'),
    beamtalk_error:with_message(Err1, Message).

%% Emit a ChangeLog entry for a live in-memory method patch (ADR 0082 Phase 1).
%%
%% Called from load_recompiled_method/7 *after* the patched class is installed in
%% memory. Captures (per the ADR's "Method patch flow"):
%%   - `kind' (`instance' / `class') from the patch side,
%%   - `intent' (`durable' for `>>' / `compile:source:'; `ephemeral' for
%%     `tryCompile:source:'),
%%   - `author_kind' (`human' / `agent') from the eval submission metadata,
%%   - `flushable' + `sourceFile' + `span' + `prev_source' when the class is
%%     backed by an in-project `.bt' file whose method span resolves cleanly.
%%
%% Non-flushable classes (stdlib / dynamic / dependency — `sourceFile' nil or
%% out-of-project) still log an entry, with `flushable: false' and a reason, so
%% the audit trail stays exhaustive ("every in-memory method mutation produces a
%% ChangeEntry"). A disk-read or span-resolution failure downgrades the entry to
%% non-flushable rather than failing the install.
%%
%% Best-effort: every failure path is logged and swallowed. The method is already
%% live; a ChangeLog write must not undo or block that (the all-or-nothing rule
%% only requires that the entry is emitted *after* a successful install — if
%% emission itself fails, the patch still stands).
-spec emit_change_entry(map()) -> ok.
emit_change_entry(MethodInfo) ->
    try
        do_emit_change_entry(MethodInfo)
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Failed to emit ChangeLog entry for live patch (patch still installed)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    method_info => maps:with([class_name, selector], MethodInfo),
                    domain => [beamtalk, runtime]
                }
            ),
            ok
    end.

-spec do_emit_change_entry(map()) -> ok.
do_emit_change_entry(MethodInfo) ->
    ClassNameBin = maps:get(class_name, MethodInfo),
    SelectorBin = maps:get(selector, MethodInfo),
    IsClassMethod = maps:get(is_class_method, MethodInfo, false),
    %% `>>' and `compile:source:' are durable; `tryCompile:source:' is ephemeral.
    Intent = maps:get(intent, MethodInfo, durable),
    %% MCP-issued patches tag `agent'; REPL / `>>' default to `human'.
    AuthorKind = maps:get(author_kind, MethodInfo, human),
    Author = maps:get(author, MethodInfo, <<"repl">>),
    Source = method_source_binary(MethodInfo),
    Kind = patch_side(IsClassMethod),
    Side = patch_side(IsClassMethod),
    Base = #{
        class => ClassNameBin,
        selector => SelectorBin,
        kind => Kind,
        source => Source,
        intent => Intent,
        author => Author,
        author_kind => AuthorKind
    },
    Entry = add_flushability(Base, ClassNameBin, SelectorBin, Side),
    _ = beamtalk_workspace_changelog:append(Entry),
    ok.

%% The outcome of a best-effort signature-store capture (ADR 0105 Phase 1,
%% BT-2777): `{captured, Prev, Classification}` when the store call succeeded
%% (`Prev` is whatever it reported as the pre-capture generation — feed this
%% straight back to rollback_signature_generation/4 on a subsequent install
%% failure; `Classification` is `beamtalk_signature_diff:classification/0`,
%% consumed by BT-2778's re-check trigger below), or `not_captured` when the
%% capture itself failed (nothing to roll back, nothing to re-check).
-type capture_outcome() ::
    {captured, beamtalk_workspace_signature_store:maybe_signature(),
        beamtalk_signature_diff:classification()}
    | not_captured.

%% Capture the freshly-compiled signature into the signature-generation store
%% (ADR 0105 Phase 1, BT-2777). Called from load_recompiled_method/8 *before*
%% the patch installs — see the call site for why ordering matters. Best-effort
%% and self-swallowing, mirroring emit_change_entry/1: the store is diagnostic
%% plumbing for a later re-check (BT-2778), never a gate on the install itself.
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
    Side = patch_side(IsClassMethod),
    NewSignature = #{
        return_type => maps:get(return_type, MethodInfo, <<"Dynamic">>),
        param_types => maps:get(param_types, MethodInfo, [])
    },
    {Prev, Classification} = beamtalk_workspace_signature_store:capture(
        ClassNameBin, SelectorBin, Side, NewSignature
    ),
    {captured, Prev, Classification}.

%% Undo a capture_signature_generation/1 (or capture_signature_removal/3) call
%% whose install/removal subsequently failed (ADR 0105 Phase 1, BT-2777).
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

%% Fire the re-check orchestration (ADR 0105 Phase 1, BT-2778) for a
%% successfully-installed patch/removal, then publish the outcome (ADR 0105
%% Phase 1, BT-2779): update `beamtalk_workspace_findings_store` and, when
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
%% Accepted tradeoff (flagged on BT-2777's review, recorded on BT-2778):
%% `Classification` is only as correct as the signature-generation store's
%% chain. Two concurrent sessions patching the same `{Class, Selector, Side}`
%% key can race `capture/4`/`rollback/4` such that a losing session's
%% rollback overwrites the store with a generation that is no longer the
%% actually-live one (`beamtalk_workspace_signature_store:rollback/4` does an
%% unconditional put, not a conditional "restore only if I'm still current"
%% write) — the *next* capture then diffs against the wrong baseline and this
%% function can fire on a false `signature_change`/`no_op`. Not fixed here:
%% the store is BT-2777's merged surface, and a full fix (per-key conditional
%% rollback) is out of this issue's scope. Advisory-only mitigates the
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
%% (ADR 0105 Phase 1, BT-2779) — the store is REPL-mode-only (see
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

%% BT-2802/BT-2828: a candidate whose diagnostics round-trip never completed
%% this reload (`NotVerifiedOwners` — `beamtalk_recheck:result()`'s
%% `not_verified_owners`, covering the caller-cap-dropped candidates AND any
%% `Kept` candidate that came back `skipped` (no live source recorded) or
%% `failed` (compile/compiler-port error)) never went through
%% `put_owner_origin/3` above, so any finding it already carries *for this
%% changed class* is left exactly as a previous, possibly-now-stale reload
%% wrote it — nothing here re-verified whether the caller's problem still
%% holds. Rather than let that finding keep asserting itself as current
%% forever (the BT-2802 bug, and its BT-2828 skipped/failed-outcome sibling)
%% or silently drop it (could hide a real, still-live problem), overwrite its
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
%% repeatedly skipped/failed, BT-2828) for many consecutive reloads must not
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

%% BT-2828: the reason a candidate went unverified is deliberately not named
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
%% Shape-change re-check (ADR 0105 Phase 2, BT-2780)
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
                ok = beamtalk_workspace_shape_store:prime(normalize_class_source_key(Name))
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
    ClassNameBin = normalize_class_source_key(Name),
    try
        {_Prev, {Classification, FieldChanges}} =
            beamtalk_workspace_shape_store:capture(ClassNameBin),
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
structure exactly (found in review, BT-2780): the `?LOG_INFO` is gated on
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
    %% BT-2802/BT-2828: same not-verified staleness marking as
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

%% ── BT-2856 / ADR 0107 Phase A: leaf-change re-check ────────────────────

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
`binary_to_existing_atom/2` safety check in this module (BT-2856 adds no
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

**BT-2873:** `beamtalk_recheck:trigger_leaf_change/1` runs **one** shared
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

**BT-2873:** `Result` may be a *shared* outcome covering several
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

%% ── ADR 0108 hot-reload re-check trigger (BT-2899): alias-change re-check ──

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

-spec patch_side(boolean()) -> instance | class.
patch_side(true) -> class;
patch_side(false) -> instance.

%% Best source for the patched method body, preferring the compiler-extracted
%% `method_source' (the `selector => body' fragment) and falling back to the raw
%% `expression' (the full `Class >> selector => body' line).
-spec method_source_binary(map()) -> binary().
method_source_binary(MethodInfo) ->
    case maps:get(method_source, MethodInfo, undefined) of
        Bin when is_binary(Bin) ->
            Bin;
        Str when is_list(Str) ->
            list_to_binary(Str);
        undefined ->
            case maps:get(expression, MethodInfo, <<>>) of
                B when is_binary(B) -> B;
                L when is_list(L) -> list_to_binary(L);
                _ -> <<>>
            end
    end.

%% Derive flushability and, when flushable, the on-disk span + prev_source.
%%
%% A class is flushable iff its `sourceFile' is non-nil AND lies inside the active
%% project tree. For flushable classes we resolve the method's byte span against
%% the *current on-disk* file (not the in-memory combined source) so a later
%% flush splices into exactly what is on disk. If the file cannot be read or the
%% span cannot be resolved, the patch downgrades to non-flushable.
-spec add_flushability(map(), binary(), binary(), instance | class) -> map().
add_flushability(Base, ClassNameBin, SelectorBin, Side) ->
    case class_source_file(ClassNameBin) of
        nil ->
            Base#{flushable => false, not_flushable_reason => no_source_reason(ClassNameBin)};
        SourceFile when is_binary(SourceFile) ->
            case classify_source_file(SourceFile) of
                {flushable, AbsPath} ->
                    add_span_or_downgrade(
                        Base, ClassNameBin, SelectorBin, Side, SourceFile, AbsPath
                    );
                {not_flushable, Reason} ->
                    Base#{flushable => false, not_flushable_reason => Reason}
            end
    end.

%% Reason for a class with no backing `.bt' sourceFile (`class_source_file/1'
%% returned nil). Distinguishes a stdlib class — whose module is compiled to a
%% `.beam' on disk (`code:which/1' returns a path) — from a dynamically-created
%% / live-only class whose module exists only in memory (`code:which/1' returns
%% a non-path atom such as `non_existing', `cover_compiled', or `preloaded').
%% Matches the ChangeLog schema's documented "stdlib" / "dynamic" reasons so flush
%% summaries can tell the two apart.
-spec no_source_reason(binary()) -> binary().
no_source_reason(ClassNameBin) ->
    case class_module(ClassNameBin) of
        {ok, Module} ->
            case code:which(Module) of
                %% A non-EMPTY path means a real `.beam' on disk → a stdlib /
                %% precompiled class. `code:which/1' returns `[]' (an empty list,
                %% so `is_list/1' is still true) for a module loaded from binary
                %% with no file — that is a live-only / dynamic class, NOT stdlib.
                Path when is_list(Path), Path =/= [] -> <<"stdlib">>;
                _NonPathOrEmpty -> <<"dynamic">>
            end;
        error ->
            <<"dynamic">>
    end.

%% Resolve a class name binary to its loaded BEAM module name, if the class is
%% registered. Returns `error' when the class is unknown / not loaded.
-spec class_module(binary()) -> {ok, atom()} | error.
class_module(ClassNameBin) ->
    case beamtalk_repl_server:safe_to_existing_atom(ClassNameBin) of
        {ok, ClassName} ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                Pid when is_pid(Pid) ->
                    {ok, beamtalk_object_class:module_name_safe(Pid)};
                _ ->
                    error
            end;
        {error, _} ->
            error
    end.

%% Resolve the disk span/prev_source for a flushable class; downgrade on failure.
-spec add_span_or_downgrade(map(), binary(), binary(), instance | class, binary(), string()) ->
    map().
add_span_or_downgrade(Base, ClassNameBin, SelectorBin, Side, SourceFile, AbsPath) ->
    case file:read_file(AbsPath) of
        {ok, DiskSource} ->
            resolve_span_entry(Base, ClassNameBin, SelectorBin, Side, SourceFile, DiskSource);
        {error, ReadReason} ->
            ?LOG_WARNING(
                "ChangeLog: could not read sourceFile for live patch; recording memory-only",
                #{
                    source_file => SourceFile,
                    reason => ReadReason,
                    domain => [beamtalk, runtime]
                }
            ),
            Base#{flushable => false, not_flushable_reason => <<"disk_read_failed">>}
    end.

-spec resolve_span_entry(map(), binary(), binary(), instance | class, binary(), binary()) -> map().
resolve_span_entry(Base, ClassNameBin, SelectorBin, Side, SourceFile, DiskSource) ->
    case beamtalk_compiler:resolve_method_span(DiskSource, ClassNameBin, SelectorBin, Side) of
        {ok, Span, PrevSource} ->
            store_disk_shaped_entry(Base, SourceFile, Span, PrevSource);
        {error, selector_not_found, _Message} ->
            %% Selector absent on disk (a brand-new method added live) is normal:
            %% record a flushable entry with no prev span — a later flush appends
            %% the method. The compiler's canonical body is column-0, but the
            %% class body on disk is indented, so reshape the body to the class's
            %% sibling-method indentation at store time (BT-2583) — mirroring
            %% `store_disk_shaped_entry`'s reshape — so flush's `append_method`
            %% stays a verbatim append into an indented body.
            new_method_entry(Base, SourceFile, DiskSource);
        {error, Reason, _Message} ->
            %% Any other resolution failure downgrades to memory-only.
            span_error_entry(Base, SourceFile, Reason)
    end.

%% Reshape a brand-new method (no prior on-disk span) to the target class body's
%% indentation at store time, so flush's `append_method' stays a verbatim append
%% (BT-2583). The compiler's canonical `unparse_method' body is column-0, but the
%% class body on disk is indented; an un-reshaped append would write the method
%% at column 0 into an indented class. The base indentation is derived from a
%% sibling method already on disk (falling back to the project's 2-space step),
%% mirroring `store_disk_shaped_entry''s store-time reshape. If the reshape FFI is
%% unavailable, downgrade to memory-only rather than store a column-0 body that
%% flush would append un-indented.
-spec new_method_entry(map(), binary(), binary()) -> map().
new_method_entry(#{source := Canonical} = Base, SourceFile, DiskSource) ->
    BaseIndent = sibling_method_indent(DiskSource),
    %% Reshape via the compiler port: it re-lays-out the canonical body at the
    %% target indent (re-breaking width-sensitive lines, which a pure whitespace
    %% shift cannot — BT-2594), so the stored body is byte-identical to what
    %% `bt fmt' produces on disk. The port is already up here (the method was just
    %% compiled through it); a transient failure downgrades to memory-only rather
    %% than store a column-0 body flush would append un-indented.
    case beamtalk_compiler:reindent_method_source(Canonical, BaseIndent) of
        {ok, Reindented} ->
            Base#{source => Reindented, flushable => true, source_file => SourceFile};
        {error, ReindentReason, _Msg} ->
            ?LOG_WARNING(
                "ChangeLog: could not reshape new-method body to class indentation; "
                "recording memory-only",
                #{
                    source_file => SourceFile,
                    reason => ReindentReason,
                    domain => [beamtalk, runtime]
                }
            ),
            %% BT-2594 deliberately re-introduces this memory-only path that
            %% BT-2592 removed: the pure-Erlang shift was total but reshaped
            %% width-sensitive methods wrongly; port re-layout is correct, at the
            %% cost of a rare transient-failure downgrade.
            Base#{flushable => false, not_flushable_reason => <<"reindent_failed">>}
    end.

%% Derive the base indentation for a brand-new method from the class body on
%% disk: the leading whitespace of the first indented, non-blank, non-comment
%% line (a sibling method or field definition). Falls back to the project's
%% 2-space convention when the class body has no indented member yet (e.g. an
%% empty class). Comment lines (`//`, `///`) and the unindented class header are
%% skipped so the step reflects member indentation, not column 0.
-spec sibling_method_indent(binary()) -> binary().
sibling_method_indent(DiskSource) ->
    Lines = binary:split(DiskSource, <<"\n">>, [global]),
    sibling_method_indent_lines(Lines).

-spec sibling_method_indent_lines([binary()]) -> binary().
sibling_method_indent_lines([]) ->
    default_method_indent();
sibling_method_indent_lines([Line | Rest]) ->
    Indent = beamtalk_workspace_reshape:leading_ws(Line),
    Content = strip_leading_ws(Line),
    case Indent =/= <<>> andalso not is_comment_or_blank(Content) of
        true -> Indent;
        false -> sibling_method_indent_lines(Rest)
    end.

%% The project's default member indentation: two spaces (ADR 0082 / stdlib
%% convention). Used for a class with no indented member to copy.
-spec default_method_indent() -> binary().
default_method_indent() -> <<"  ">>.

%% True for a line whose content (leading whitespace already stripped) is empty
%% or a line comment — such lines do not establish member indentation.
-spec is_comment_or_blank(binary()) -> boolean().
is_comment_or_blank(<<>>) -> true;
is_comment_or_blank(<<"//", _/binary>>) -> true;
is_comment_or_blank(_) -> false.

%% Drop the leading run of spaces/tabs from a line.
-spec strip_leading_ws(binary()) -> binary().
strip_leading_ws(<<C, Rest/binary>>) when C =:= $\s; C =:= $\t ->
    strip_leading_ws(Rest);
strip_leading_ws(Bin) ->
    Bin.

%% Reshape the stored `source' (the compiler's canonical column-0
%% `unparse_method' body) to the on-disk byte-span shape so the ChangeEntry's
%% `source_ref' is a drop-in for `disk[span]' — `source_ref == disk[span]' by
%% construction (BT-2584). A later `Workspace flush' then splices it verbatim
%% with no reindent. The base indentation is the leading whitespace of the disk
%% slice (`PrevSource') the patch replaces. If the reshape FFI is unavailable,
%% downgrade to memory-only rather than store a column-0 body that flush would
%% splice into an indented region and corrupt the file.
-spec store_disk_shaped_entry(
    map(), binary(), #{start := non_neg_integer(), 'end' := non_neg_integer()}, binary()
) -> map().
store_disk_shaped_entry(#{source := Canonical} = Base, SourceFile, Span, PrevSource) ->
    BaseIndent = beamtalk_workspace_reshape:leading_ws(PrevSource),
    %% Reshape via the compiler port (re-layout at the span's indent — BT-2594),
    %% so `source_ref == disk[span]' even for width-sensitive methods a pure shift
    %% would reformat. The port is already up (the patch was just compiled through
    %% it); a transient failure downgrades to memory-only rather than store a
    %% column-0 body flush would splice into an indented region and corrupt it.
    case beamtalk_compiler:reindent_method_source(Canonical, BaseIndent) of
        {ok, Reindented} ->
            %% The disk byte-span ends in a trailing newline unless the method is
            %% the last line of the file with no terminator (ADR 0082). Match that
            %% trailing-newline state on the reshaped body — regardless of whether
            %% the canonical body carries its own — so the splice is a true drop-in
            %% and never glues the next line or leaves a stray blank one (BT-2584).
            DiskShaped = match_trailing_newline(Reindented, PrevSource),
            Base#{
                source => DiskShaped,
                flushable => true,
                source_file => SourceFile,
                span => Span,
                prev_source => PrevSource
            };
        {error, ReindentReason, _Msg} ->
            ?LOG_WARNING(
                "ChangeLog: could not reshape patch body to disk indentation; "
                "recording memory-only",
                #{
                    source_file => SourceFile,
                    reason => ReindentReason,
                    domain => [beamtalk, runtime]
                }
            ),
            %% BT-2594 deliberately re-introduces this memory-only path that
            %% BT-2592 removed (see new_method_entry/3): port re-layout is correct
            %% where the pure-Erlang shift reshaped width-sensitive methods wrongly.
            Base#{flushable => false, not_flushable_reason => <<"reindent_failed">>}
    end.

%% Make `Source''s trailing-newline state match `Reference''s: append a single
%% `\n' when the reference ends in one and the source does not; strip trailing
%% `\n's when the reference has none (the clamped-at-EOF last-method case). Keeps
%% the reshaped body a true drop-in for the disk slice it replaces.
-spec match_trailing_newline(binary(), binary()) -> binary().
match_trailing_newline(Source, Reference) ->
    case ends_with_newline(Reference) of
        true -> beamtalk_workspace_reshape:ensure_trailing_newline(Source);
        false -> beamtalk_workspace_reshape:strip_trailing_newlines(Source)
    end.

-spec ends_with_newline(binary()) -> boolean().
ends_with_newline(<<>>) -> false;
ends_with_newline(Bin) -> binary:last(Bin) =:= $\n.

%% A genuine span-resolution failure (`ambiguous', a port/transport error, ...)
%% downgrades the entry to memory-only with a reason. The brand-new-method case
%% (`selector_not_found') does NOT come here — it is reshaped and recorded
%% flushable by `new_method_entry/3' (BT-2583).
-spec span_error_entry(map(), binary(), atom()) -> map().
span_error_entry(Base, _SourceFile, Reason) ->
    Base#{
        flushable => false,
        not_flushable_reason =>
            iolist_to_binary([<<"span_unresolved:">>, atom_to_binary(Reason, utf8)])
    }.

%%% ----------------------------------------------------------------------------
%%% Method-removal ChangeLog entry (ADR 0112 Phase 3, BT-3187)
%%% ----------------------------------------------------------------------------

-doc """
Emit a `"remove-method"` ChangeLog entry for a just-completed LOCAL (non-
extension) method removal.

Called by `beamtalk_behaviour_intrinsics:remove_selector/2` (backing
`Behaviour>>removeSelector:` / `removeSelector:ifAbsent:`) *after*
`remove_method/3` has already spliced the method out and hot-reloaded the
class — mirrors `emit_change_entry/1`'s placement for a patch (step 3, after
the memory install; ADR 0082's "install is authoritative, log is best-effort"
ordering, reused verbatim for removal per ADR 0112 § ChangeLog interaction)
and its best-effort/self-swallowing failure handling: a ChangeLog write
failure never undoes the removal, which is already live.

`sourceFile` / `flushable` / `not_flushable_reason` are derived by the exact
same classification `add_flushability/4` uses for a patch
(`class_source_file/1` / `classify_source_file/1` / `no_source_reason/1`,
reused directly rather than re-derived) — stdlib/dynamic/dependency classes
get `flushable: false` with the matching reason; an in-project class gets
`flushable: true`. `span` / `prev_source` are resolved against the CURRENT
on-disk file (not the in-memory source `remove_method/3` just spliced) —
matching how a patch's span is always resolved against disk rather than
memory — since that is the byte span BT-2192's future flush-excise step will
need. A selector already absent from disk (the method being removed was
itself a live, never-flushed addition) has nothing to excise there either, so
the entry downgrades to `flushable: false, not_flushable_reason: "not_on_disk"`
rather than inventing a span. `source_ref` is always absent — a removal has
no new body to store (ADR 0112 § ChangeLog interaction: `source_ref: null`);
the append input map below simply carries no `source` key, which
`beamtalk_workspace_changelog:do_append/2` treats as "no source_ref".
""".
-spec emit_remove_change_entry(
    binary(), atom() | binary(), instance | class, binary(), human | agent
) ->
    ok.
emit_remove_change_entry(ClassNameBin, Selector, Side, Author, AuthorKind) ->
    try
        do_emit_remove_change_entry(ClassNameBin, Selector, Side, Author, AuthorKind)
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Failed to emit ChangeLog entry for method removal (removal still installed)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    class => ClassNameBin,
                    selector => Selector,
                    domain => [beamtalk, runtime]
                }
            ),
            ok
    end.

-spec do_emit_remove_change_entry(
    binary(), atom() | binary(), instance | class, binary(), human | agent
) -> ok.
do_emit_remove_change_entry(ClassNameBin, Selector, Side, Author, AuthorKind) ->
    SelectorBin = method_selector_binary(Selector),
    Base = #{
        class => ClassNameBin,
        selector => SelectorBin,
        kind => 'remove-method',
        side => Side,
        intent => durable,
        author => Author,
        author_kind => AuthorKind
    },
    Entry = add_removal_flushability(Base, ClassNameBin, SelectorBin, Side),
    _ = beamtalk_workspace_changelog:append(Entry),
    ok.

%% Derive flushability + (when flushable) the on-disk span/prev_source for a
%% removal entry. Mirrors `add_flushability/4`'s classification exactly (same
%% helpers, same stdlib/dynamic/dependency table) but never reshapes/stores a
%% new `source` — a removal has none to reshape.
-spec add_removal_flushability(map(), binary(), binary(), instance | class) -> map().
add_removal_flushability(Base, ClassNameBin, SelectorBin, Side) ->
    case class_source_file(ClassNameBin) of
        nil ->
            Base#{flushable => false, not_flushable_reason => no_source_reason(ClassNameBin)};
        SourceFile when is_binary(SourceFile) ->
            case classify_source_file(SourceFile) of
                {flushable, AbsPath} ->
                    add_removal_span_or_downgrade(
                        Base, ClassNameBin, SelectorBin, Side, SourceFile, AbsPath
                    );
                {not_flushable, Reason} ->
                    Base#{flushable => false, not_flushable_reason => Reason}
            end
    end.

-spec add_removal_span_or_downgrade(
    map(), binary(), binary(), instance | class, binary(), string()
) -> map().
add_removal_span_or_downgrade(Base, ClassNameBin, SelectorBin, Side, SourceFile, AbsPath) ->
    case file:read_file(AbsPath) of
        {ok, DiskSource} ->
            resolve_removal_span_entry(
                Base, ClassNameBin, SelectorBin, Side, SourceFile, DiskSource
            );
        {error, ReadReason} ->
            ?LOG_WARNING(
                "ChangeLog: could not read sourceFile for method removal; recording memory-only",
                #{
                    source_file => SourceFile,
                    reason => ReadReason,
                    domain => [beamtalk, runtime]
                }
            ),
            Base#{flushable => false, not_flushable_reason => <<"disk_read_failed">>}
    end.

-spec resolve_removal_span_entry(
    map(), binary(), binary(), instance | class, binary(), binary()
) -> map().
resolve_removal_span_entry(Base, ClassNameBin, SelectorBin, Side, SourceFile, DiskSource) ->
    case beamtalk_compiler:resolve_method_span(DiskSource, ClassNameBin, SelectorBin, Side) of
        {ok, Span, PrevSource} ->
            Base#{
                flushable => true,
                source_file => SourceFile,
                span => Span,
                prev_source => PrevSource
            };
        {error, selector_not_found, _Message} ->
            %% Nothing on disk to excise (the method was a live, never-flushed
            %% addition) — nothing for BT-2192's future flush-excise step to
            %% act on either.
            Base#{flushable => false, not_flushable_reason => <<"not_on_disk">>};
        {error, Reason, _Message} ->
            %% Any other resolution failure (ambiguous / port error / the file
            %% changed underneath us) downgrades to memory-only, mirroring
            %% span_error_entry/3's patch-side handling.
            Base#{
                flushable => false,
                not_flushable_reason =>
                    iolist_to_binary([<<"span_unresolved:">>, atom_to_binary(Reason, utf8)])
            }
    end.

-doc """
Emit a `"remove-method"` ChangeLog entry for a just-completed EXTENSION
method removal (ADR 0066 open classes; ADR 0112 § Extension methods, §
ChangeLog interaction).

Unlike `emit_remove_change_entry/5` (a local class-body method, whose
span/prev_source resolve against the target class's own on-disk file), an
extension method's source lives in a *different* file than the extended
class's — `beamtalk_extensions` tracks only the owning package/module atom
(`Owner`, ADR 0070), not which file registered a given `{Class, Selector}`,
so this attributes the entry the most it can honestly claim:

  - `sourceFile` resolves iff `Owner` also happens to name a currently-loaded
    class (the common case: a file with no explicit package declaration
    registers under its own module-derived name) — reuses
    `class_source_file/1`, the same resolver an ordinary class uses. Any
    other `Owner` (a genuine multi-class package, or one that never loaded)
    leaves `sourceFile` unset rather than guessing wrong.
  - `span` is always `undefined` — locating a standalone `TargetClass >>
    selector` definition's byte offsets inside an arbitrary owner file has no
    resolver in this codebase (`beamtalk_compiler:resolve_method_span/4`
    looks for a method *inside* a named class's own body, not a foreign
    extension line); building one is out of this issue's scope.
  - The entry is always `flushable: false, not_flushable_reason:
    "extension"` — precise disk attribution/excise for extension removals is
    unbuilt infrastructure (BT-2192 territory, same boundary as every other
    `remove-method` flush-excise case), not something this issue invents.

`Owner` and `PrevBody` (the extension's stored source text) must be captured
by the CALLER before unregistering — `beamtalk_extensions:unregister/3`
deletes both the moment it returns, so by the time this function runs neither
is recoverable from the registry itself. `PrevBody` becomes `prev_source` so
the audit trail at least records what was removed, even though automated
revert cannot yet act on it (reverting a `sourceFile`-less, `span`-less entry
has nothing to re-install against).
""".
-spec emit_extension_remove_change_entry(
    binary(),
    atom() | binary(),
    instance | class,
    atom() | undefined,
    binary() | undefined,
    binary(),
    human | agent
) -> ok.
emit_extension_remove_change_entry(
    ClassNameBin, Selector, Side, Owner, PrevBody, Author, AuthorKind
) ->
    try
        do_emit_extension_remove_change_entry(
            ClassNameBin, Selector, Side, Owner, PrevBody, Author, AuthorKind
        )
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Failed to emit ChangeLog entry for extension removal (removal still installed)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    class => ClassNameBin,
                    selector => Selector,
                    domain => [beamtalk, runtime]
                }
            ),
            ok
    end.

-spec do_emit_extension_remove_change_entry(
    binary(),
    atom() | binary(),
    instance | class,
    atom() | undefined,
    binary() | undefined,
    binary(),
    human | agent
) -> ok.
do_emit_extension_remove_change_entry(
    ClassNameBin, Selector, Side, Owner, PrevBody, Author, AuthorKind
) ->
    SelectorBin = method_selector_binary(Selector),
    Base0 = #{
        class => ClassNameBin,
        selector => SelectorBin,
        kind => 'remove-method',
        side => Side,
        intent => durable,
        author => Author,
        author_kind => AuthorKind,
        flushable => false,
        not_flushable_reason => <<"extension">>
    },
    Base1 = maybe_put_prev_source(Base0, PrevBody),
    Entry = maybe_put_extension_source_file(Base1, Owner),
    _ = beamtalk_workspace_changelog:append(Entry),
    ok.

-spec maybe_put_prev_source(map(), binary() | undefined) -> map().
maybe_put_prev_source(Base, undefined) -> Base;
maybe_put_prev_source(Base, PrevBody) when is_binary(PrevBody) -> Base#{prev_source => PrevBody}.

-spec maybe_put_extension_source_file(map(), atom() | undefined) -> map().
maybe_put_extension_source_file(Base, undefined) ->
    Base;
maybe_put_extension_source_file(Base, Owner) when is_atom(Owner) ->
    case class_source_file(atom_to_binary(Owner, utf8)) of
        nil -> Base;
        SourceFile -> Base#{source_file => SourceFile}
    end.

%%% ----------------------------------------------------------------------------
%%% Class-removal ChangeLog entry (BT-3206)
%%% ----------------------------------------------------------------------------

-doc """
Snapshot a class's current full source and its on-disk flushability
classification, for a `"remove-class"` ChangeLog entry that will be appended
later — after `classRemoveFromSystemByName/1`'s teardown has already run.

Must be called BEFORE that teardown starts: `class_source_file/1` needs the
class's still-live registry pid to resolve its module (teardown stops the
class gen_server), and `beamtalk_workspace_meta:get_class_source/1` needs the
`class_sources` entry that `beamtalk_class_lifecycle:class_removed/2` purges
as part of the same teardown (BT-3105). Capturing here — mirrors the
"read+parse before mutate" ordering `compile:source:`'s patch hook already
uses — is what gives `revert:` (BT-3207, later phase) something to restore
from.

Reuses `class_source_file/1` / `classify_source_file/1` / `no_source_reason/1`
— the exact same classification `add_flushability/4` uses for a patch — so a
dynamically-created (ClassBuilder) class gets `flushable => false,
not_flushable_reason => <<"dynamic">>` and an ordinary in-project class gets
`flushable => true, source_file => SourceFile`. `classRemoveFromSystemByName/1`
already refuses stdlib classes before this is ever called, so the `"stdlib"`
reason `no_source_reason/1` can also produce never actually reaches here.
Unlike the method-removal snapshot, there is no span to resolve (a class
removal excises the whole file entry, not a byte range within it) — the
ChangeLog entry this feeds always carries `span: null`.
""".
-spec capture_class_removal_snapshot(binary()) -> map().
capture_class_removal_snapshot(ClassNameBin) ->
    Base = maybe_put_prev_source(#{}, current_class_source(ClassNameBin)),
    case class_source_file(ClassNameBin) of
        nil ->
            Base#{flushable => false, not_flushable_reason => no_source_reason(ClassNameBin)};
        SourceFile when is_binary(SourceFile) ->
            case classify_source_file(SourceFile) of
                {flushable, _AbsPath} ->
                    Base#{flushable => true, source_file => SourceFile};
                {not_flushable, Reason} ->
                    Base#{flushable => false, not_flushable_reason => Reason}
            end
    end.

%% The class's current tracked source text (whole-file, per `set_class_source/2`),
%% or `undefined` when nothing has been recorded for it (mirrors
%% `beamtalk_workspace_meta:get_class_source/1`'s own `undefined` degrade for
%% an untracked class). `get_class_source/1` always returns a plain list
%% (`string()`) or `undefined` per its own spec, never a binary.
-spec current_class_source(binary()) -> binary() | undefined.
current_class_source(ClassNameBin) ->
    case beamtalk_workspace_meta:get_class_source(ClassNameBin) of
        Source when is_list(Source) -> unicode:characters_to_binary(Source);
        undefined -> undefined
    end.

-doc """
Emit a `"remove-class"` ChangeLog entry for a just-completed
`removeFromSystem` (BT-3206).

Called by `beamtalk_behaviour_intrinsics:classRemoveFromSystemByName/1` at
its existing success point — immediately after `publish_class_removed/2` —
with `Snapshot` already captured by `capture_class_removal_snapshot/1` before
teardown began. Mirrors `emit_remove_change_entry/5`'s placement (ADR 0082's
"install is authoritative, log is best-effort" ordering) and its
best-effort/self-swallowing failure handling: a ChangeLog write failure never
undoes the removal, which is already irreversible in memory by this point.

`selector` and `side` are always absent (`null` in the persisted entry) — a
class removal has no method-level target (ADR 0082's established schema,
reused verbatim: `side: null` for every kind but `"remove-method"`).
""".
-spec emit_remove_class_change_entry(binary(), map(), binary(), human | agent) -> ok.
emit_remove_class_change_entry(ClassNameBin, Snapshot, Author, AuthorKind) ->
    try
        do_emit_remove_class_change_entry(ClassNameBin, Snapshot, Author, AuthorKind)
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Failed to emit ChangeLog entry for class removal (removal still installed)",
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

-spec do_emit_remove_class_change_entry(binary(), map(), binary(), human | agent) -> ok.
do_emit_remove_class_change_entry(ClassNameBin, Snapshot, Author, AuthorKind) ->
    Base = #{
        class => ClassNameBin,
        kind => 'remove-class',
        intent => durable,
        author => Author,
        author_kind => AuthorKind
    },
    Entry = maps:merge(
        Base, maps:with([flushable, not_flushable_reason, source_file, prev_source], Snapshot)
    ),
    _ = beamtalk_workspace_changelog:append(Entry),
    ok.

%% Resolve the class's source file via the BEAM module attribute (the same
%% source-of-truth `Behaviour>>sourceFile' reads). Returns nil for classes with
%% no backing file (stdlib / dynamic / not loaded).
-spec class_source_file(binary()) -> binary() | nil.
class_source_file(ClassNameBin) ->
    case beamtalk_repl_server:safe_to_existing_atom(ClassNameBin) of
        {ok, ClassName} ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                Pid when is_pid(Pid) ->
                    ModuleName = beamtalk_object_class:module_name_safe(Pid),
                    beamtalk_reflection:source_file_from_module(ModuleName);
                _ ->
                    nil
            end;
        {error, _} ->
            nil
    end.

%% Classify a class's source file as flushable (in-project) or not.
%%
%% `flushable' requires the file to resolve to an absolute path inside the active
%% project source tree (per workspace metadata `project_path'). Files outside it
%% are a dependency; a workspace with no project path treats all paths as
%% non-flushable (nothing to flush into).
-spec classify_source_file(binary()) ->
    {flushable, string()} | {not_flushable, binary()}.
classify_source_file(SourceFile) ->
    SourceStr = binary_to_list(SourceFile),
    AbsPath = filename:absname(SourceStr),
    case beamtalk_workspace_meta:get_metadata() of
        {ok, #{project_path := ProjectPath}} when is_binary(ProjectPath) ->
            ProjectRoot = filename:absname(binary_to_list(ProjectPath)),
            case is_path_inside(ProjectRoot, AbsPath) of
                true -> {flushable, AbsPath};
                false -> {not_flushable, dependency_reason(SourceFile)}
            end;
        _ ->
            %% No project context — cannot flush into a tree we do not know.
            {not_flushable, dependency_reason(SourceFile)}
    end.

-spec dependency_reason(binary()) -> binary().
dependency_reason(SourceFile) ->
    iolist_to_binary([<<"dependency:">>, SourceFile]).

%% True iff `Path' is `Root' itself or lives beneath it (component-wise prefix,
%% so "/a/bc" is not considered inside "/a/b").
-spec is_path_inside(string(), string()) -> boolean().
is_path_inside(Root, Path) ->
    RootParts = filename:split(Root),
    PathParts = filename:split(Path),
    lists:prefix(RootParts, PathParts).

%% Convert a list of class info maps to a list of existing atoms (BT-738).
-spec class_name_atoms([map()]) -> [atom()].
class_name_atoms(Classes) ->
    lists:filtermap(
        fun
            (#{name := Name}) when is_list(Name) ->
                safe_atom_result(beamtalk_repl_server:safe_to_existing_atom(list_to_binary(Name)));
            (#{name := Name}) when is_binary(Name) ->
                safe_atom_result(beamtalk_repl_server:safe_to_existing_atom(Name));
            (_) ->
                false
        end,
        Classes
    ).

-spec safe_atom_result({ok, atom()} | {error, badarg}) -> {true, atom()} | false.
safe_atom_result({ok, Atom}) -> {true, Atom};
safe_atom_result({error, badarg}) -> false.

%% Compute a package-qualified module name for a file (BT-775 / BT-1670).
%%
%% With package context: derives `bt@{package}@{relative_path_segments}`
%% for files under src/ or test/.  Files outside those directories (e.g.
%% examples/, fixtures/) fall back to `bt@{stem_snake_case}` so the same
%% class always gets the same module name regardless of load path.
%% Without package context: returns `undefined` so the compiler port
%% derives the name from the class name instead.  This is intentional —
%% class-name-based naming in the REPL enables hot reload across file
%% renames (e.g., hot_counter.bt and hot_counter_v2.bt both define
%% HotCounter → same module bt@hot_counter).
-spec compute_package_module_name(string()) -> binary() | undefined.
compute_package_module_name(Path) ->
    case beamtalk_workspace_meta:get_metadata() of
        {ok, #{package_name := PackageName, project_path := ProjectPath}} when
            is_binary(PackageName), is_binary(ProjectPath)
        ->
            AbsPath = filename:absname(Path),
            ProjectRoot = binary_to_list(ProjectPath),
            resolve_package_module(AbsPath, ProjectRoot, PackageName, Path);
        _ ->
            undefined
    end.

%% Try src/ then test/ to resolve the package module name.
%% Falls back to bt@{stem} for files outside src/ and test/ (e.g. examples/).
-spec resolve_package_module(string(), string(), binary(), string()) -> binary().
resolve_package_module(AbsPath, ProjectRoot, PackageName, OrigPath) ->
    case try_package_relative(AbsPath, ProjectRoot, "src") of
        {ok, ModuleName} ->
            iolist_to_binary(["bt@", PackageName, "@", ModuleName]);
        undefined ->
            case try_package_relative(AbsPath, ProjectRoot, "test") of
                {ok, ModuleName} ->
                    iolist_to_binary(["bt@", PackageName, "@test@", ModuleName]);
                undefined ->
                    stem_module_name(OrigPath)
            end
    end.

%% Derive module name from the file stem: bt@{snake_case_stem}.
-spec stem_module_name(string()) -> binary().
stem_module_name(Path) ->
    Basename = filename:basename(Path, ".bt"),
    Snake = to_snake_case(Basename),
    iolist_to_binary(["bt@", Snake]).

%% Check if AbsPath is under ProjectRoot/SubDir and return the relative module path.
-spec try_package_relative(string(), string(), string()) ->
    {ok, iodata()} | undefined.
try_package_relative(AbsPath, ProjectRoot, SubDir) ->
    Dir = filename:join(ProjectRoot, SubDir),
    AbsDir = filename:absname(Dir),
    DirParts = filename:split(AbsDir),
    PathParts = filename:split(AbsPath),
    DirLen = length(DirParts),
    case length(PathParts) > DirLen andalso lists:prefix(DirParts, PathParts) of
        true ->
            RelParts = lists:nthtail(DirLen, PathParts),
            Last = lists:last(RelParts),
            RelPartsNoExt = lists:droplast(RelParts) ++ [filename:rootname(Last)],
            SnakeSegments = [to_snake_case(S) || S <- RelPartsNoExt],
            {ok, lists:join("@", SnakeSegments)};
        false ->
            undefined
    end.
