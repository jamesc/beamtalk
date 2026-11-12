%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Module loading and class activation for the Beamtalk REPL

Handles loading compiled Beamtalk modules into the BEAM runtime,
registering classes, triggering hot reload, and managing file paths.
Extracted from beamtalk_repl_eval.
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
    %% ADR 0112 Phase 3: best-effort ChangeLog append after a
    %% successful `removeSelector:` call — see remove_method/3's doc for why
    %% this is a separate call rather than folded into remove_method/3 itself.
    emit_remove_change_entry/5,
    emit_extension_remove_change_entry/7,
    %% ADR 0114: ChangeLog append for a multi-site rewrite. The rewrite
    %% mechanism itself lives in `beamtalk_repl_loader_rewrite` (BT-4702).
    emit_rewrite_change_entry/2,
    %% Best-effort snapshot + ChangeLog append for a successful
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
    reload_class_file/3,
    is_stdlib_path/1,
    to_snake_case/1,
    verify_class_present/3,
    compute_package_module_name/1,
    %% Re-check triggers/publishing live in `beamtalk_repl_loader_recheck` (BT-4702).
    precheck_method/4,
    %% ADR 0123 §4 (BT-3538): the class-body-level sibling of
    %% precheck_method/4 — see its own doc for why the mechanism differs.
    precheck_class_shape/2,
    %% The Cockpit section-authoring write path
    %% (`beamtalk_repl_ops_load:save_section/5`) needs the same "which `.bt`
    %% file backs this class, and is it inside the project (safe to write)"
    %% resolution the ADR 0082 install hook already relies on — reused here
    %% rather than re-derived, per the project's no-duplicate-implementations
    %% rule.
    class_source_file/1,
    classify_source_file/1,
    %% Primitives that `beamtalk_repl_loader_rewrite` (BT-4702) calls into
    %% from outside this module. They stay here because the single-site
    %% install/remove paths use them too; they are exported only so the
    %% split compiles, not because they are public API.
    compile_reload_source/5,
    normalize_class_source_key/1,
    patch_module_target/1,
    source_path_or_empty/1,
    splice_replace/3,
    with_ambient_protocol_sources/1,
    %% Primitives that `beamtalk_repl_loader_newclass` (BT-4702) calls into.
    %% Same reason as above: shared with the single-site paths, exported only
    %% so the split compiles.
    class_name_atoms/1,
    %% Primitives that `beamtalk_repl_loader_protocol` (BT-4702) calls into.
    %% Same reason as above: shared with the single-class paths, exported only
    %% so the split compiles.
    capture_signature_removal/3,
    compile_reload_source/4,
    safe_atom_result/1,
    splice_out_span/2,
    track_protocol_source/2,
    load_class_binary/4,
    maybe_autoflush/1,
    %% Also a seam for `beamtalk_repl_loader_rewrite_sites_tests.erl`, which
    %% `meck:new(beamtalk_repl_loader, [passthrough])` + `meck:expect/3`s it
    %% to exercise `beamtalk_repl_loader_rewrite:rewrite_sites/2`'s
    %% partial_install_failure path (an install-time anomaly with no natural
    %% trigger against a binary that just compiled successfully). The caller
    %% is in another module, so meck's module replacement intercepts it. See
    %% that test module's own moduledoc for the full seam rationale.
    install_reload_result/2
]).

%% Exported for testing (only in test builds)
-ifdef(TEST).
-export([
    load_compiled_module/6,
    extract_trailing_info/1,
    resolve_class_name/1,
    safe_binary_to_atom/1,
    safe_list_to_atom/1,
    resolve_package_module/4,
    try_package_relative/3,
    maybe_add_loaded_module/2,
    store_file_class_sources/3,
    store_class_sources/4,
    %% ADR 0082 Phase 1: pure helpers behind the install hook.
    is_path_inside/2,
    method_source_binary/1,
    patch_side/1,
    span_error_entry/3,
    new_method_entry/3,
    sibling_method_indent/1,
    %% ADR 0082 extension: class-redefinition ChangeLog entry helpers.
    snapshot_class_def_prev_sources/1,
    emit_class_def_entries/3,
    add_class_def_flushability/2
]).
-endif.

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% ChangeLog entry construction lives in its own module (BT-4683).
-define(ENTRIES, beamtalk_repl_loader_changelog_entries).

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
                    %% ADR 0127 §10a / BT-3593: ambient `protocol_sources`
                    %% context — see `with_ambient_protocol_sources/1`'s doc
                    %% for why a same-file-only `uses:` isn't enough here.
                    PrebuiltIndexes = with_ambient_protocol_sources(
                        beamtalk_repl_compiler:build_class_indexes()
                    ),
                    case
                        beamtalk_repl_compiler:compile_file(
                            Source, Path, StdlibMode, ModuleNameOverride, PrebuiltIndexes
                        )
                    of
                        %% Protocol definition from file compilation.
                        %% Must be matched before the generic 4-tuple to avoid
                        %% {ok, protocol_definition, Info, Warnings} binding to
                        %% {ok, Binary, ClassNames, ModuleName}.
                        {ok, protocol_definition, ProtocolInfo, _Warnings} ->
                            track_protocol_source(ProtocolInfo, Source),
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
Load a Beamtalk file with pre-built class indexes.

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
                    FullIndexes = with_ambient_protocol_sources(PrebuiltIndexes),
                    case
                        beamtalk_repl_compiler:compile_file(
                            Source, Path, StdlibMode, ModuleNameOverride, FullIndexes
                        )
                    of
                        %% Protocol definition — must match before generic 4-tuple.
                        {ok, protocol_definition, ProtocolInfo, _Warnings} ->
                            track_protocol_source(ProtocolInfo, Source),
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
    PrebuiltIndexes = with_ambient_protocol_sources(
        beamtalk_repl_compiler:build_class_indexes()
    ),
    case beamtalk_repl_compiler:compile_file(Source, Label, false, undefined, PrebuiltIndexes) of
        %% Protocol definition — must match before generic 4-tuple.
        {ok, protocol_definition, ProtocolInfo, _Warnings} ->
            track_protocol_source(ProtocolInfo, Source),
            load_protocol_module(ProtocolInfo, undefined, State);
        {ok, Binary, ClassNames, ModuleName} ->
            load_compiled_module(Binary, ClassNames, ModuleName, Source, undefined, State);
        {error, Reason} ->
            {error, Reason, State}
    end.

-doc """
Merge the ambient `protocol_sources` context (ADR 0127 §10a / BT-3593) into
`Indexes` — every protocol source `beamtalk_workspace_meta` has tracked so
far this session (`all_protocol_sources/0`), keeping any entry `Indexes`
already carries (a caller's own value always wins over the ambient default,
same convention `class_hierarchy`/`protocol_registry` follow elsewhere in
this file).

Without this, a class in one file `uses:`ing a protocol defined in a
DIFFERENT file only resolves within the compile-time-known
`class_superclass_index`/`class_module_index` indexes above — never the
protocol's own full AST, which `trait_expansion::expand_module` needs to
flatten provided methods. `protocol_registry` (injected unconditionally by
`beamtalk_compiler_server` itself, see its own doc) is signature-only and
cannot substitute for this.
""".
-spec with_ambient_protocol_sources(map()) -> map().
with_ambient_protocol_sources(Indexes) ->
    maps:merge(#{protocol_sources => beamtalk_workspace_meta:all_protocol_sources()}, Indexes).

%% Record this file's protocol source(s) (ADR 0127 §10a / BT-3593) so a
%% LATER cross-file `uses:` — an ordinary `:load` of a class in a different
%% file, or a protocol-reload fan-out's own recompile of ITS users — can
%% carry this protocol's full AST via `with_ambient_protocol_sources/1`.
%% Best-effort bookkeeping: called just before the actual `code:load_binary/3`
%% this protocol's own install still has to run, mirroring this module's
%% other pre-install bookkeeping calls (e.g. `prime_shape_capture/1`).
-spec track_protocol_source(map(), string()) -> ok.
track_protocol_source(#{protocols := ProtocolNameBins}, Source) ->
    lists:foreach(
        fun(NameBin) -> beamtalk_workspace_meta:set_protocol_source(NameBin, Source) end,
        ProtocolNameBins
    ).

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
    %% ADR 0105 Phase 2: seed the shape-generation store from the
    %% about-to-be-replaced module's CURRENT __beamtalk_meta/0 before this
    %% class-body reload installs — see
    %% beamtalk_workspace_shape_store's moduledoc "Two-phase capture" for why
    %% this must run before code:load_binary, not after.
    beamtalk_repl_loader_recheck:prime_shape_capture(Classes),
    %% Snapshot each class's currently-tracked source BEFORE
    %% store_class_sources below overwrites it — see emit_class_def_entries/3's
    %% doc for why a redefinition of an already-loaded class needs the OLD
    %% source captured ahead of the install, not after.
    PrevSources = snapshot_class_def_prev_sources(Classes),
    %% ADR 0107 Phase A: load_class_binary/4
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
            %% Log a pending 'class-def' ChangeLog entry for each
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

-doc "Recompile and reload a class after a standalone method definition.".
-spec reload_method_definition(
    map(), [beamtalk_repl_protocol:warning()], string(), beamtalk_repl_state:state()
) ->
    {ok, term(), binary(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()}.
reload_method_definition(MethodInfo, Warnings, Expression, State) ->
    #{class_name := ClassNameBin} = MethodInfo,
    case beamtalk_repl_loader_protocol:is_protocol_name(ClassNameBin) of
        true ->
            %% ADR 0127 §11: `P >> sel => ...` at the REPL edits the protocol's
            %% own source and re-expands every user (two-stage fan-out).
            beamtalk_repl_loader_protocol:patch_protocol_provision(
                ClassNameBin,
                maps:get(selector, MethodInfo),
                ?ENTRIES:method_source_binary(MethodInfo),
                MethodInfo#{intent => maps:get(intent, MethodInfo, durable)},
                Warnings,
                State
            );
        false ->
            reload_class_method_definition(MethodInfo, Warnings, Expression, State)
    end.

-spec reload_class_method_definition(
    map(), [beamtalk_repl_protocol:warning()], string(), beamtalk_repl_state:state()
) ->
    {ok, term(), binary(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()}.
reload_class_method_definition(MethodInfo, Warnings, Expression, State) ->
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
ADR 0107 Phase A caller-discipline hardening: load a class-defining module's
compiled `Binary` and compute `superclasses_losing_leaf_status/1` against
`Classes` in the one required order, as a single call.

Every generated class module's `on_load` hook (`register_class/0`)
runs **synchronously inside** `code:load_binary/3` and registers
any new subclass link before that call returns — so
`superclasses_losing_leaf_status/1` MUST run against `Classes` *before*
`code:load_binary/3`, never after (`activate_module/4`'s doc has the full
mechanism). Every one of today's four class-defining call sites
(`load_class_module/3`, `load_compiled_module/6`,
`reload_compile_and_load/4`, `beamtalk_repl_loader_newclass:new_class_install/8`) already followed this
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
    NewlyNonLeafSuperclasses = beamtalk_repl_loader_recheck:superclasses_losing_leaf_status(
        Classes
    ),
    case load_binary_scoped(ModuleName, LoadPath, Binary, Classes) of
        {module, ModuleName} -> {ok, NewlyNonLeafSuperclasses};
        {error, Reason} -> {error, Reason}
    end.

-doc """
`code:load_binary/3` with the pending-load-error table scoped to this attempt
(BT-3773). Every REPL loader load goes through here, so the drain after a
failed load sees a structured refusal (`abi_mismatch`,
`stdlib_shadowing`) parked by THIS load's `-on_load` hook (assuming no concurrent
load of the same class: clear/load/drain is not atomic, so a racing loader can
clear it and the load then falls back to `on_load_failure`): an entry left behind
by an earlier load nothing drained (`beamtalk_module_activation`, code-server
autoload) is discarded first instead of being blamed on this load's real cause.
""".
-spec load_binary_scoped(atom(), string(), binary(), [map()]) ->
    {module, atom()} | {error, term()}.
load_binary_scoped(ModuleName, LoadPath, Binary, Classes) ->
    ok = beamtalk_runtime_api:clear_pending_load_errors_by_names(class_name_atoms(Classes)),
    code:load_binary(ModuleName, LoadPath, Binary).

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

ADR 0105 Phase 2: `spawn_shape_recheck/1` fires last, after
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
-spec activate_module(atom(), [map()], string() | undefined) -> ok.
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
`register_classes/2` call (a redundant, harmless second invocation of the
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
-spec activate_module(atom(), [map()], string() | undefined, [binary()]) -> ok.
activate_module(ModuleName, Classes, SourcePath, NewlyNonLeafSuperclasses) ->
    register_classes(Classes, ModuleName),
    trigger_hot_reload(ModuleName, Classes),
    beamtalk_workspace_meta:register_module(ModuleName, SourcePath),
    beamtalk_workspace_meta:update_activity(),
    beamtalk_repl_loader_recheck:spawn_shape_recheck(Classes),
    beamtalk_repl_loader_recheck:spawn_leaf_change_recheck(NewlyNonLeafSuperclasses).

-doc "Register loaded classes by calling the module's register_class/0 function.".
-spec register_classes([map()], atom()) -> ok.
register_classes(_ClassInfoList, ModuleName) ->
    case erlang:function_exported(ModuleName, register_class, 0) of
        true ->
            %% bt-catcher-audit: not-applicable-no-block - compiled register_class/0, registration
            %% only
            try
                ModuleName:register_class()
            catch
                _:_ -> ok
            end;
        false ->
            ok
    end.

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
    lists:foreach(
        fun(ClassMap) ->
            case resolve_class_name(ClassMap) of
                undefined ->
                    ok;
                ClassName ->
                    hot_reload_class(ModuleName, ClassName),
                    hot_reload_descendants(ClassName, ReloadedNames)
            end
        end,
        Classes
    ),
    ok.

-doc """
BT-3531: hot-reload every loaded descendant of ClassName, each against its
own (unchanged) module — only the field set being reconciled changed,
via the ancestor's field addition/removal, not the descendant's code.

Skips any descendant in `ReloadedNames` — it already has (or will get) its
own direct `hot_reload_class/2` call from `trigger_hot_reload/2`'s own loop
over `Classes`, so reaching it again here would double the
suspend/change_code/resume cycle for the same instances.

`whereis_class/1` and `module_name/1` are two independent calls with a gap
between them — if a descendant's class process exits in that gap
(concurrent removal/rename from another session, or an untrappable `kill`
before `terminate/2` runs), `module_name/1` raises `exit({noproc, _})`.
Since this runs before `activate_module/4`'s workspace bookkeeping steps
and the new module code is already loaded by this point, an uncaught
crash here would abort the whole reload half-applied — so a vanished
descendant is skipped, same as `undefined` above, mirroring the
`catch error:badarg -> []` guard `hot_reload_class/2` already applies to
`all_instances/1` for the same class of transient race.
""".
-spec hot_reload_descendants(atom(), sets:set(atom())) -> ok.
hot_reload_descendants(ClassName, ReloadedNames) ->
    lists:foreach(
        fun(Descendant) ->
            case sets:is_element(Descendant, ReloadedNames) of
                true ->
                    ok;
                false ->
                    case beamtalk_runtime_api:whereis_class(Descendant) of
                        undefined ->
                            ok;
                        ClassPid ->
                            try
                                DescendantModule = beamtalk_runtime_api:module_name(ClassPid),
                                hot_reload_class(DescendantModule, Descendant)
                            catch
                                exit:{noproc, _} -> ok
                            end
                    end
            end
        end,
        beamtalk_class_registry:all_subclasses(ClassName)
    ).

-doc """
Compile and load a source file without REPL session state.

Called from beamtalk_behaviour_intrinsics:classReload/1 via erlang:apply/3
to avoid a compile-time dependency from beamtalk_runtime to beamtalk_workspace.
""".
-spec reload_class_file(string()) -> {ok, [map()]} | {error, term()}.
reload_class_file(Path) ->
    reload_class_file_impl(Path, undefined, #{}).

%% ExpectedClassName (atom) is verified against the compiled class list.
-spec reload_class_file(string(), atom()) -> {ok, [map()]} | {error, term()}.
reload_class_file(Path, ExpectedClassName) ->
    reload_class_file_impl(Path, ExpectedClassName, #{}).

-doc """
`reload_class_file/2` as one file of a `load_project` batch (BT-4394).

`BatchOpts` is the batch's `#{project_batch => binary()}` compile option
(or `#{}`), merged into every compile this reload issues — the file's own
compile and, for a protocol file, each fan-out user's recompile — so the
compiler port verifies the project scope once per batch. Without it a
fan-out compile mid-batch carries no token, resets the port's verified
batch, and the next file of the batch re-stats the whole project.
""".
-spec reload_class_file(string(), atom() | undefined, map()) -> {ok, [map()]} | {error, term()}.
reload_class_file(Path, ExpectedClassName, BatchOpts) ->
    reload_class_file_impl(Path, ExpectedClassName, BatchOpts).

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

Delegates to `beamtalk_module_name:camel_to_snake/1`, the single
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
Verify that the expected class name appears in the compiled class list.
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
%% Deliberately does NOT emit a `'class-def'` ChangeLog entry, unlike
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
    %% ADR 0105 Phase 2: see load_class_module/3's identical comment.
    beamtalk_repl_loader_recheck:prime_shape_capture(ClassNames),
    %% ADR 0107 Phase A: see load_class_binary/4's doc.
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

%% Load a protocol module into BEAM, register it, and update REPL state.
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
    case load_binary_scoped(ModuleName, LoadPath, Binary, ProtocolClassNames) of
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

%% Load a protocol module without session state (stateless path).
%% Used by reload_compile_and_load for load_files_stateless.
-spec load_protocol_module_stateless(map(), string()) -> {ok, [map()]} | {error, term()}.
load_protocol_module_stateless(ProtocolInfo, Path) ->
    #{binary := Binary, module_name := ModuleName, protocols := Protocols} = ProtocolInfo,
    ProtocolClassNames = [
        #{name => binary_to_list(P), superclass => "Object"}
     || P <- Protocols
    ],
    case load_binary_scoped(ModuleName, Path, Binary, ProtocolClassNames) of
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
%%% Class-redefinition ChangeLog entry (ADR 0082 extension)
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
*redefined* an already-loaded class (ADR 0082 extension).

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
before), which is out of scope here: this feature's acceptance criteria is
scoped to redefining an "*existing*" class, and `newClass:at:`'s own
`beamtalk_repl_loader_changelog_entries:emit_new_class_entry/3` already covers the brand-new-class case for the one
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
            ?ENTRIES:emit_class_def_entry(NameBin, Expression, PrevSource),
            true
    end;
emit_class_def_entries(_MultipleOrNoClasses, _Expression, _PrevSources) ->
    false.

%% Extract trailing expression info from a class definition result.
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

%% Trigger hot reload for a single class's live instances, against
%% ModuleName (that class's own BEAM module — the just-reloaded module for
%% the reloaded class itself, or an unchanged descendant module when called
%% from hot_reload_descendants/1).
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
%% descendant `hot_reload_descendants/2` reconciles (neither of which is
%% primed/captured in the shape store for a descendant, which never has its
%% own module recompiled) and needs no shape/version diff to report — it is
%% purely "did any live instance of this class end up suspended", which
%% `trigger_code_change/3`'s own return already answers.
-spec hot_reload_class(atom(), atom()) -> ok.
hot_reload_class(ModuleName, ClassName) ->
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
            %% BT-3543: the stray-migration-outside-table check only depends
            %% on ClassName, never on an instance — run it once here rather
            %% than once per pid inside code_change/3 (skip_stray_warning
            %% tells the per-instance path below not to repeat it).
            beamtalk_shape_migration:check_stray_migrations(ClassName),
            Extra = #{module => ModuleName, skip_stray_warning => true},
            {ok, Upgraded, Failures} = beamtalk_runtime_api:trigger_code_change(
                ModuleName, Pids, Extra
            ),
            publish_suspended_finding(ModuleName, ClassName, Upgraded, Failures)
    end.

%% Best-effort publish of the `instances_suspended` finding (ADR 0123 §4 row
%% 5) for `ClassName` through the existing reload-findings channel. Reads
%% the just-installed `ToVersion` directly off `ModuleName`'s own meta
%% (cheap, already-loaded local call) rather than the shape store, since
%% this runs for descendants too, which the shape store never primes/captures
%% (their own module is not recompiled by this reload — see
%% `hot_reload_class/2`'s doc). Never raises: any internal failure is logged
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

%% Reload a class file without REPL session state.
%% `BatchOpts` is `reload_class_file/3`'s batch compile option (or `#{}`).
-spec reload_class_file_impl(string(), atom() | undefined, map()) ->
    {ok, [map()]} | {error, term()}.
reload_class_file_impl(Path, ExpectedClassName, BatchOpts) ->
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
                    reload_compile_and_load(
                        Source, Path, ModuleNameOverride, ExpectedClassName, BatchOpts
                    )
            end
    end.

%% Compile and load a file for stateless reload.
%%
%% Deliberately does NOT emit a `'class-def'` ChangeLog entry, same
%% reasoning as `load_compiled_module/6`. Both of this function's callers
%% compile `Source` straight from `Path` — `reload_class_file_impl/2` backs
%% `Counter reload` / `:reload Counter` (an explicit reload FROM the on-disk
%% file after an external edit) and `remove_method/3`'s "reload the class
%% WITHOUT the removed method" (which recompiles a spliced *in-memory* source
%% but is followed by its own `emit_remove_change_entry/5` call at the
%% `removeSelector:` call site, ADR 0112 Phase 3 — already logged via
%% a different, more specific kind). Neither case has a class-definition edit
%% pending relative to disk that a `'class-def'` entry would newly capture.
-spec reload_compile_and_load(
    string(), string(), binary() | undefined, atom() | undefined
) -> {ok, [map()]} | {error, term()}.
reload_compile_and_load(Source, Path, ModuleNameOverride, ExpectedClassName) ->
    reload_compile_and_load(Source, Path, ModuleNameOverride, ExpectedClassName, #{}).

%% `reload_compile_and_load/4` inside a `load_project` batch: `BatchOpts`
%% (`reload_class_file/3`'s `#{project_batch => _}`, or `#{}`) rides every
%% compile this reload issues, fan-out included (BT-4394).
-spec reload_compile_and_load(
    string(), string(), binary() | undefined, atom() | undefined, map()
) -> {ok, [map()]} | {error, term()}.
reload_compile_and_load(Source, Path, ModuleNameOverride, ExpectedClassName, BatchOpts) ->
    %% ADR 0127 §11 / BT-3593: a protocol file's reload can never install
    %% just the protocol module the way a class file's reload installs just
    %% that class — every loaded user's flattened body is baked from the
    %% protocol's OLD provisions until it, too, recompiles. See
    %% `reload_protocol_fanout/3`'s own doc for the two-stage atomicity this
    %% routes through instead of `install_reload_result/2` directly.
    %%
    %% ADR 0127 §10a (BT-3594): this call site is *every* stateless load —
    %% `Workspace load:`'s `handle_load_after_native/1` (`beamtalk_repl_eval:
    %% reload_class_file/1`) routes every first-time-or-not `Workspace load:`
    %% through here, not just an explicit `Counter reload`/`:reload` — so
    %% without the same `with_ambient_protocol_sources/1` merge `handle_load/
    %% 2,3` already does for the *stateful* `:load` path, a class loaded via
    %% `Workspace load:` whose `uses:` names a protocol defined in a
    %% *different*, already-loaded file reported "unknown protocol" even
    %% though the exact same two files load fine one after another via
    %% `:load`/`beamtalk_repl_loader:handle_load/2`. `compile_reload_source/5`
    %% already carries this merge for the reload-fanout's own recompile of a
    %% protocol's users (see that arity's doc); this reuses it for every
    %% caller instead of only that one.
    PrebuiltIndexes = with_ambient_protocol_sources(
        maps:merge(beamtalk_repl_compiler:build_class_indexes(), BatchOpts)
    ),
    case
        compile_reload_source(Source, Path, ModuleNameOverride, ExpectedClassName, PrebuiltIndexes)
    of
        {ok, protocol_definition, _ProtocolInfo} = ProtocolResult ->
            beamtalk_repl_loader_protocol:reload_protocol_fanout(
                ProtocolResult, Path, Source, BatchOpts
            );
        {ok, _Tag, _, _, _} = CompiledResult ->
            install_reload_result(CompiledResult, Path);
        {error, _} = Err ->
            Err
    end.

-doc """
Compile half of `reload_compile_and_load/4` — split out for the multi-site
rewrite's in-memory atomicity protocol (ADR 0114 § Decision, final paragraph), which
needs to compile/validate every site in a multi-site rewrite batch *before*
installing any of them (see `beamtalk_repl_loader_rewrite:rewrite_sites/2`'s doc). Never touches
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
    handle_compile_reload_result(
        beamtalk_repl_compiler:compile_file(Source, Path, StdlibMode, ModuleNameOverride),
        Path,
        ExpectedClassName
    ).

-doc """
`compile_reload_source/4`, additionally accepting a `PrebuiltIndexes` map
forwarded to `beamtalk_repl_compiler:compile_file/5` (a real map, always —
unlike `compile_file/4`'s own internal `use_runtime_indexes` shortcut,
which is `compile_file/5`'s to interpret, not this function's to pass
through; doing so here previously widened this arity's declared domain
past what `compile_file/5`'s own spec promises, which is exactly the kind
of cross-function spec mismatch Dialyzer's whole-module success-typing
propagates into unrelated "will never be called" findings elsewhere in
this file — found via `just dialyzer` on this very change).

BT-3593's protocol-reload fan-out was this arity's first production caller,
and BT-3594's `reload_compile_and_load/4` (every stateless `Workspace load:`,
not just an explicit reload) is now a second — both merge
`beamtalk_repl_compiler:build_class_indexes/0` (the same superclass/module
indexes an ordinary reload gets) with a `protocol_sources` entry, so a
recompile flattens `uses:` against real provisions (ADR 0127 §10a) rather
than resolving against nothing. See `reload_compile_and_load/4`'s own doc
for why it needed this too.
""".
-spec compile_reload_source(string(), string(), binary() | undefined, atom() | undefined, map()) ->
    {ok, protocol_definition, map()}
    | {ok, compiled, binary(), [map()], atom()}
    | {error, term()}.
compile_reload_source(Source, Path, ModuleNameOverride, ExpectedClassName, PrebuiltIndexes) when
    is_map(PrebuiltIndexes)
->
    StdlibMode = is_stdlib_path(Path),
    handle_compile_reload_result(
        beamtalk_repl_compiler:compile_file(
            Source, Path, StdlibMode, ModuleNameOverride, PrebuiltIndexes
        ),
        Path,
        ExpectedClassName
    ).

%% Shared compile-result dispatch both `compile_reload_source` arities above
%% reduce to — see `compile_reload_source/4`'s own doc for the shapes this
%% translates between.
-spec handle_compile_reload_result(
    {ok, protocol_definition, map(), [beamtalk_repl_protocol:warning()]}
    | {ok, binary(), [map()], atom()}
    | {error, term()},
    string(),
    atom() | undefined
) ->
    {ok, protocol_definition, map()}
    | {ok, compiled, binary(), [map()], atom()}
    | {error, term()}.
handle_compile_reload_result(
    {ok, protocol_definition, ProtocolInfo, _Warnings}, _Path, _ExpectedClassName
) ->
    {ok, protocol_definition, ProtocolInfo};
handle_compile_reload_result({ok, Binary, ClassNames, ModuleName}, Path, ExpectedClassName) ->
    case verify_class_present(ExpectedClassName, ClassNames, Path) of
        ok -> {ok, compiled, Binary, ClassNames, ModuleName};
        {error, _} = Err -> Err
    end;
handle_compile_reload_result({error, Reason}, _Path, _ExpectedClassName) ->
    {error, Reason}.

-doc """
Install half of `reload_compile_and_load/4` — see `compile_reload_source/4`'s
doc for why this is split out. Takes a successful `compile_reload_source/4`
result and performs the mutating half: `load_class_binary/4` +
`activate_module/4` (or the protocol-module equivalent). Never called on a
value that failed `compile_reload_source/4` — callers gate on `{ok, ...}`
first, which is exactly the multi-site rewrite's atomicity protocol: nothing calls this
until every site in a batch has independently validated.

Exported so `beamtalk_repl_loader_rewrite:install_rewrite_group/5` can call it
from outside this module, and so `meck` can intercept it in
`beamtalk_repl_loader_rewrite_sites_tests.erl`'s `partial_install_failure`
coverage — not part of this module's intended public API otherwise.
""".
-spec install_reload_result(
    {ok, protocol_definition, map()} | {ok, compiled, binary(), [map()], atom()}, string()
) -> {ok, [map()]} | {error, term()}.
install_reload_result({ok, protocol_definition, ProtocolInfo}, Path) ->
    load_protocol_module_stateless(ProtocolInfo, Path);
install_reload_result({ok, compiled, Binary, ClassNames, ModuleName}, Path) ->
    %% ADR 0105 Phase 2: see load_class_module/3's identical
    %% comment. Covers every caller of this helper: reload_class_file_impl/2
    %% (file reload after an on-disk edit), remove_method/3's "reload the
    %% class WITHOUT the removed method", and beamtalk_repl_loader_rewrite:rewrite_sites/2's per-site
    %% install — none of these change `state:`/`field:` slots, so
    %% priming it is harmless (the subsequent capture/1 always diffs an
    %% unchanged shape to itself, `no_op`).
    beamtalk_repl_loader_recheck:prime_shape_capture(ClassNames),
    %% ADR 0107 Phase A: see load_class_binary/4's doc.
    case load_class_binary(ModuleName, Path, Binary, ClassNames) of
        {ok, NewlyNonLeafSuperclasses} ->
            activate_module(ModuleName, ClassNames, Path, NewlyNonLeafSuperclasses),
            {ok, ClassNames};
        {error, Reason} ->
            %% BT-3722: surface a structured refusal (e.g. `abi_mismatch`)
            %% parked during the module's `-on_load` instead of `on_load_failure`.
            case
                beamtalk_runtime_api:drain_pending_load_errors_by_names(
                    class_name_atoms(ClassNames)
                )
            of
                [{_ClassName, StructuredError} | _] -> {error, StructuredError};
                [] -> {error, {load_error, Reason}}
            end
    end.

%% Recompile a class with a new method definition.
%%
%% Delegates to beamtalk_repl_compiler:compile_for_method_reload/2 which
%% wraps all compiler calls in wrap_compiler_errors, preventing compiler crashes
%% from propagating as exits that would kill the REPL process.
-spec recompile_with_method(
    string(), map(), string(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()
) ->
    {ok, term(), binary(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()}.
recompile_with_method(ClassSource, MethodInfo, Expression, Warnings, State) ->
    CombinedSource = ClassSource ++ "\n" ++ Expression,
    %% Source may carry non-Latin1 characters (em dash, arrows, smart quotes in
    %% doc comments) — `unicode:characters_to_binary/1' produces the UTF-8 the
    %% compiler expects, where `list_to_binary/1' crashes with `badarg' on any
    %% codepoint > 255. Both inputs are already-validated source (the stored class
    %% source and a just-compiled expression), so the conversion always yields a
    %% binary here — never the `{error,_,_}' tuple of malformed input.
    SourceBin = unicode:characters_to_binary(CombinedSource),
    %% Include superclass index so cross-file inheritance resolves correctly.
    SuperclassIndex = beamtalk_repl_compiler:build_class_superclass_index(),
    Options0 = #{stdlib_mode => false},
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
    %% Preserve the class's package-qualified module name and
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
    {ModuleNameOverride, SourcePath} = patch_module_target(ClassNameBin),
    SuperclassIndex = beamtalk_repl_compiler:build_class_superclass_index(),
    ModuleIndex = beamtalk_repl_compiler:build_class_module_index(),
    Options = #{
        class_name => ClassNameBin,
        %% Side comes from the caller: `false` for the instance-side
        %% `compile:source:` / MCP `save_method` / IDE-save chokepoint; `true`
        %% for a class-side revert re-install. The REPL `Class class >>
        %% sel` path still flows through `reload_method_definition`, which carries
        %% the side in its `MethodInfo`.
        is_class_method => IsClassMethod,
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
                source_path_or_empty(SourcePath),
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
    {ModuleNameOverride, SourcePath} = patch_module_target(ClassNameBin),
    SuperclassIndex = beamtalk_repl_compiler:build_class_superclass_index(),
    ModuleIndex = beamtalk_repl_compiler:build_class_module_index(),
    Options = #{
        class_name => ClassNameBin,
        is_class_method => IsClassMethod,
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
                {error, Reason, _Msg} ->
                    {error, {method_not_found, Reason}}
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
%% A thin wrapper over `splice_replace/3` — removal is replacement
%% with the empty binary.
-spec splice_out_span(binary(), beamtalk_repl_loader_rewrite:rewrite_span()) -> binary().
splice_out_span(Source, Span) ->
    splice_replace(Source, Span, <<>>).

%% Replace the bytes `[start, end)' in `Source' with `NewText', joining the
%% surrounding text. Shared leaf primitive (AGENTS.md's no-duplicate-
%% implementations rule) behind both `splice_out_span/2` (removal — ADR 0112)
%% and `beamtalk_repl_loader_rewrite:rewrite_sites/2`'s per-site splice (ADR 0114).
-spec splice_replace(binary(), beamtalk_repl_loader_rewrite:rewrite_span(), binary()) -> binary().
splice_replace(Source, #{start := Start, 'end' := End}, NewText) ->
    <<Before:Start/binary, _Old:(End - Start)/binary, After/binary>> = Source,
    <<Before/binary, NewText/binary, After/binary>>.

%% Resolve a class's package-qualified module name + on-disk source path for a
%% patch, by reusing the class's CURRENT loaded module (which already carries the
%% correct package-qualified name for both project and dependency classes) and
%% its `beamtalk_source' attribute. Returns `{undefined, undefined}' for
%% dynamic/source-less classes so they keep the default stem naming.
-spec patch_module_target(binary()) -> {binary() | undefined, string() | undefined}.
patch_module_target(ClassNameBin) ->
    case ?ENTRIES:class_module(ClassNameBin) of
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
    case load_binary_scoped(ModName, SourcePath, Binary, Classes) of
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
            ?ENTRIES:emit_change_entry(MethodInfo),
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
            {ok, Result, <<>>, AllWarnings, State};
        {error, LoadReason} ->
            %% ADR 0105 Phase 1: the install this capture described
            %% never happened — undo it so the store still reflects the actually
            %% live generation.
            beamtalk_repl_loader_recheck:rollback_signature_generation(
                ClassNameBin, SelectorBin, Side, CaptureOutcome
            ),
            ClassAtoms = class_name_atoms(Classes),
            case beamtalk_runtime_api:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError, <<>>, AllWarnings, State};
                [] ->
                    {error, {load_error, LoadReason}, <<>>, AllWarnings, State}
            end
    end.

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

%%% ----------------------------------------------------------------------------
%%% ChangeLog entries — public entry points kept on this module
%%% ----------------------------------------------------------------------------
%%
%% The implementations live in `beamtalk_repl_loader_changelog_entries`
%% (BT-4683). These exports stay here because callers outside this module
%% (`beamtalk_repl_eval`, `beamtalk_repl_ops_load`, `beamtalk_repl_ops_browse`,
%% and `beamtalk_behaviour_intrinsics` via `erlang:apply/3`) address them on
%% `beamtalk_repl_loader`.

-doc "See `beamtalk_repl_loader_changelog_entries:emit_remove_change_entry/5`.".
-spec emit_remove_change_entry(
    binary(), atom() | binary(), instance | class, binary(), human | agent
) -> ok.
emit_remove_change_entry(ClassNameBin, Selector, Side, Author, AuthorKind) ->
    ?ENTRIES:emit_remove_change_entry(ClassNameBin, Selector, Side, Author, AuthorKind).

-doc "See `beamtalk_repl_loader_changelog_entries:emit_extension_remove_change_entry/7`.".
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
    ?ENTRIES:emit_extension_remove_change_entry(
        ClassNameBin, Selector, Side, Owner, PrevBody, Author, AuthorKind
    ).

-doc "See `beamtalk_repl_loader_changelog_entries:emit_rewrite_change_entry/2`.".
-spec emit_rewrite_change_entry(map(), beamtalk_repl_loader_rewrite:rewrite_result()) -> ok.
emit_rewrite_change_entry(Spec, RewriteResult) ->
    ?ENTRIES:emit_rewrite_change_entry(Spec, RewriteResult).

-doc "See `beamtalk_repl_loader_changelog_entries:capture_class_removal_snapshot/1`.".
-spec capture_class_removal_snapshot(binary()) -> map().
capture_class_removal_snapshot(ClassNameBin) ->
    ?ENTRIES:capture_class_removal_snapshot(ClassNameBin).

-doc "See `beamtalk_repl_loader_changelog_entries:emit_remove_class_change_entry/4`.".
-spec emit_remove_class_change_entry(binary(), map(), binary(), human | agent) -> ok.
emit_remove_class_change_entry(ClassNameBin, Snapshot, Author, AuthorKind) ->
    ?ENTRIES:emit_remove_class_change_entry(ClassNameBin, Snapshot, Author, AuthorKind).

-doc "See `beamtalk_repl_loader_changelog_entries:class_source_file/1`.".
-spec class_source_file(binary()) -> binary() | nil.
class_source_file(ClassNameBin) ->
    ?ENTRIES:class_source_file(ClassNameBin).

-doc "See `beamtalk_repl_loader_changelog_entries:classify_source_file/1`.".
-spec classify_source_file(binary()) -> {flushable, string()} | {not_flushable, binary()}.
classify_source_file(SourceFile) ->
    ?ENTRIES:classify_source_file(SourceFile).

-ifdef(TEST).
%% Test-only entry points the loader's existing EUnit suites call on this
%% module; the helpers themselves moved to
%% `beamtalk_repl_loader_changelog_entries` / `beamtalk_workspace_file`.
is_path_inside(Root, Path) -> beamtalk_workspace_file:path_inside(Root, Path).
method_source_binary(MethodInfo) -> ?ENTRIES:method_source_binary(MethodInfo).
patch_side(IsClassMethod) -> ?ENTRIES:patch_side(IsClassMethod).
span_error_entry(Base, SourceFile, Reason) -> ?ENTRIES:span_error_entry(Base, SourceFile, Reason).
new_method_entry(Base, SourceFile, DiskSource) ->
    ?ENTRIES:new_method_entry(Base, SourceFile, DiskSource).
sibling_method_indent(DiskSource) -> ?ENTRIES:sibling_method_indent(DiskSource).
add_class_def_flushability(Base, ClassNameBin) ->
    ?ENTRIES:add_class_def_flushability(Base, ClassNameBin).
-endif.

%% Convert a list of class info maps to a list of existing atoms.
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

%% Compute a package-qualified module name for a file.
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
    %% Strictly below `AbsDir` (a non-empty relative part list): the directory
    %% itself is not a file to name a module after.
    case beamtalk_workspace_file:relative_parts(AbsDir, AbsPath) of
        {ok, [_ | _] = RelParts} ->
            Last = lists:last(RelParts),
            RelPartsNoExt = lists:droplast(RelParts) ++ [filename:rootname(Last)],
            SnakeSegments = [to_snake_case(S) || S <- RelPartsNoExt],
            {ok, lists:join("@", SnakeSegments)};
        _NotStrictlyInside ->
            undefined
    end.
