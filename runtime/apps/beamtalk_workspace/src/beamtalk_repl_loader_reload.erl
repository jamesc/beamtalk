%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_reload).

%%% **DDD Context:** Workspace Context

-moduledoc """
Class-file reload for the loader: `reload_class_file/1,2,3`, compiling a source through the port, verifying the class is present, installing the result, and package-relative module-name derivation. Moved verbatim out of `beamtalk_repl_loader` (BT-4907).
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include("beamtalk_repl_loader.hrl").

-export([
    compute_package_module_name/1,
    reload_compile_and_load/4,
    reload_class_file/1,
    reload_class_file/2,
    reload_class_file/3,
    compile_reload_source/4,
    compile_reload_source/5,
    compile_reload_source_by_path/3,
    compile_reload_source_by_path/4,
    install_reload_result/2
]).

%% Exported for tests only (BT-4903): no production caller.
-ifdef(TEST).
-export([
    verify_class_present/3
]).
-endif.

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

%% Load a protocol module without session state (stateless path).
%% Used by reload_compile_and_load for load_files_stateless.
-spec load_protocol_module_stateless(map(), string()) -> {ok, [map()]} | {error, term()}.
load_protocol_module_stateless(ProtocolInfo, Path) ->
    #{binary := Binary, module_name := ModuleName, protocols := Protocols} = ProtocolInfo,
    ProtocolClassNames = [
        #{name => binary_to_list(P), superclass => "Object"}
     || P <- Protocols
    ],
    case
        beamtalk_repl_loader_support:load_binary_scoped(
            ModuleName, Path, Binary, ProtocolClassNames
        )
    of
        {module, ModuleName} ->
            case
                beamtalk_repl_loader_activate:activate_module(ModuleName, ProtocolClassNames, Path)
            of
                ok -> {ok, ProtocolClassNames};
                {error, _} = ActivateError -> ActivateError
            end;
        {error, Reason} ->
            ClassAtoms = beamtalk_repl_loader_support:class_name_atoms(ProtocolClassNames),
            case beamtalk_runtime_api:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError};
                [] ->
                    {error, {load_error, Reason}}
            end
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
                    case compute_package_module_name(Path) of
                        {ok, ModuleNameOverride} ->
                            reload_compile_and_load(
                                Source, Path, ModuleNameOverride, ExpectedClassName, BatchOpts
                            );
                        {error, _} = Error ->
                            Error
                    end
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
    PrebuiltIndexes = beamtalk_repl_loader_support:with_ambient_protocol_sources(
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
    StdlibMode = beamtalk_repl_loader_support:is_stdlib_path(Path),
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
    StdlibMode = beamtalk_repl_loader_support:is_stdlib_path(Path),
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
    case beamtalk_repl_loader_support:load_class_binary(ModuleName, Path, Binary, ClassNames) of
        {ok, NewlyNonLeafSuperclasses} ->
            case
                beamtalk_repl_loader_activate:activate_module(
                    ModuleName, ClassNames, Path, NewlyNonLeafSuperclasses
                )
            of
                ok -> {ok, ClassNames};
                {error, _} = ActivateError -> ActivateError
            end;
        {error, Reason} ->
            %% BT-3722: surface a structured refusal (e.g. `abi_mismatch`)
            %% parked during the module's `-on_load` instead of `on_load_failure`.
            case
                beamtalk_runtime_api:drain_pending_load_errors_by_names(
                    beamtalk_repl_loader_support:class_name_atoms(ClassNames)
                )
            of
                [{_ClassName, StructuredError} | _] -> {error, StructuredError};
                [] -> {error, {load_error, Reason}}
            end
    end.

%% `compile_reload_source/4,5` under the module name `compute_package_module_name/1`
%% derives for `Path` (the file-backed reload paths: protocol fan-out and its
%% rollbacks, protocol edits). A path the naming rule rejects is an `{error, _}`
%% before anything is compiled.
-spec compile_reload_source_by_path(string(), string(), atom() | undefined) ->
    {ok, protocol_definition, map()}
    | {ok, compiled, binary(), [map()], atom()}
    | {error, term()}.
compile_reload_source_by_path(Source, Path, ExpectedClassName) ->
    case compute_package_module_name(Path) of
        {ok, ModuleNameOverride} ->
            compile_reload_source(Source, Path, ModuleNameOverride, ExpectedClassName);
        {error, _} = Error ->
            Error
    end.

-spec compile_reload_source_by_path(string(), string(), atom() | undefined, map()) ->
    {ok, protocol_definition, map()}
    | {ok, compiled, binary(), [map()], atom()}
    | {error, term()}.
compile_reload_source_by_path(Source, Path, ExpectedClassName, PrebuiltIndexes) ->
    case compute_package_module_name(Path) of
        {ok, ModuleNameOverride} ->
            compile_reload_source(
                Source, Path, ModuleNameOverride, ExpectedClassName, PrebuiltIndexes
            );
        {error, _} = Error ->
            Error
    end.

%% Compute the module name override for a file (BT-4868).
%%
%% With package context the name comes from the compiler port's one
%% path-to-module rule (`beamtalk_compiler:module_name_for_path/3`, built on
%% Rust `relative_module_segments`, the rule `beamtalk build` and `beamtalk
%% test` use): `bt@{package}@{path}` for a file under `src/`,
%% `bt@{package}@test@{path}` under `test/`, and `bt@{stem}` for a file under
%% neither (examples/, fixtures/), so the same class gets the same module name
%% regardless of load path. This module only works out which root the file is
%% under; it never spells a module name itself.
%%
%% Without package context: `{ok, undefined}`, so the compiler port derives
%% the name from the class name instead.  This is intentional —
%% class-name-based naming in the REPL enables hot reload across file
%% renames (e.g., hot_counter.bt and hot_counter_v2.bt both define
%% HotCounter → same module bt@hot_counter).
%%
%% A path segment outside `[A-Za-z0-9_]` (`my-class.bt`) is a load error, the
%% same rejection `beamtalk build` gives.
-spec compute_package_module_name(string()) ->
    {ok, binary() | undefined} | {error, {compile_error, binary()}}.
compute_package_module_name("") ->
    %% No backing file (a REPL-defined class, e.g. in protocol fan-out): let the
    %% port derive the module name from the class. `filename:absname("")` is
    %% the VM's cwd, which must never name a module.
    {ok, undefined};
compute_package_module_name(Path) ->
    case beamtalk_workspace_meta:get_metadata() of
        {ok, #{package_name := PackageName, project_path := ProjectPath}} when
            is_binary(PackageName), is_binary(ProjectPath)
        ->
            AbsPath = filename:absname(Path),
            {RootKind, RelPath} = module_root_and_relative_path(
                AbsPath, binary_to_list(ProjectPath)
            ),
            case
                beamtalk_compiler:module_name_for_path(
                    unicode:characters_to_binary(RelPath), PackageName, RootKind
                )
            of
                {ok, ModuleName} -> {ok, ModuleName};
                {error, _Reason, Message} -> {error, {compile_error, Message}}
            end;
        _ ->
            {ok, undefined}
    end.

%% Which root `AbsPath` is under, and its `/`-joined path relative to it:
%% `src/`, then `test/`, else `none` (the whole path; the port uses only the
%% file name). Which `test/` files are fixtures is the port's call, not this
%% module's. The directory itself is not a file to name a module after.
-spec module_root_and_relative_path(string(), string()) -> {src | test | none, string()}.
module_root_and_relative_path(AbsPath, ProjectRoot) ->
    case relative_below(AbsPath, ProjectRoot, "src") of
        {ok, Rel} ->
            {src, Rel};
        undefined ->
            case relative_below(AbsPath, ProjectRoot, "test") of
                {ok, Rel} -> {test, Rel};
                undefined -> {none, AbsPath}
            end
    end.

-spec relative_below(string(), string(), string()) -> {ok, string()} | undefined.
relative_below(AbsPath, ProjectRoot, SubDir) ->
    AbsDir = filename:absname(filename:join(ProjectRoot, SubDir)),
    case beamtalk_workspace_file:relative_parts(AbsDir, AbsPath) of
        {ok, [_ | _] = RelParts} -> {ok, lists:flatten(lists:join("/", RelParts))};
        _NotStrictlyInside -> undefined
    end.
