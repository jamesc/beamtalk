%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_support).

%%% **DDD Context:** Workspace Context

-moduledoc """
Shared leaf helpers for the loader modules: class-name atom extraction, scoped binary loading, loaded-module tracking, class-source key normalisation, path classification and patch-target shaping. Moved verbatim out of `beamtalk_repl_loader` (BT-4907) so the activate, reload and method-install modules can use them without calling back up into the loader.
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include("beamtalk_repl_loader.hrl").

-export([
    track_protocol_source/2,
    track_protocol_source/3,
    with_ambient_protocol_sources/1,
    class_name_atoms/1,
    is_stdlib_path/1,
    load_binary_scoped/4,
    load_class_binary/4,
    maybe_add_loaded_module/2,
    normalize_class_source_key/1,
    patch_module_target/1,
    source_path_binary/1,
    source_path_or_empty/1,
    track_module_source/3,
    to_snake_case/1,
    safe_atom_result/1
]).

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

-spec normalize_class_source_key(atom() | binary() | list()) -> binary().
normalize_class_source_key(Name) when is_binary(Name) -> Name;
normalize_class_source_key(Name) when is_atom(Name) -> atom_to_binary(Name, utf8);
normalize_class_source_key(Name) when is_list(Name) -> list_to_binary(Name).

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

%% Convert a list of class info maps to a list of existing atoms.
-spec class_name_atoms([map()]) -> [atom()].
class_name_atoms(Classes) ->
    lists:filtermap(
        fun
            (#{name := Name}) when is_list(Name) ->
                safe_atom_result(beamtalk_repl_errors:safe_to_existing_atom(list_to_binary(Name)));
            (#{name := Name}) when is_binary(Name) ->
                safe_atom_result(beamtalk_repl_errors:safe_to_existing_atom(Name));
            (_) ->
                false
        end,
        Classes
    ).

-spec safe_atom_result({ok, atom()} | {error, badarg}) -> {true, atom()} | false.
safe_atom_result({ok, Atom}) -> {true, Atom};
safe_atom_result({error, badarg}) -> false.

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
    maps:merge(
        #{
            protocol_sources => beamtalk_workspace_meta:all_protocol_sources(),
            %% BT-4828: the paths ride beside the texts, so codegen can
            %% attribute a cross-file protocol's methods to its own file.
            protocol_source_paths => beamtalk_workspace_meta:all_protocol_source_paths()
        },
        Indexes
    ).

%% Record this file's protocol source(s) (ADR 0127 §10a / BT-3593) so a
%% LATER cross-file `uses:` — an ordinary `:load` of a class in a different
%% file, or a protocol-reload fan-out's own recompile of ITS users — can
%% carry this protocol's full AST via `with_ambient_protocol_sources/1`.
%% Best-effort bookkeeping: called just before the actual `code:load_binary/3`
%% this protocol's own install still has to run, mirroring this module's
%% other pre-install bookkeeping calls (e.g. `prime_shape_capture/1`).
-spec track_protocol_source(map(), string()) -> ok.
track_protocol_source(ProtocolInfo, Source) ->
    track_protocol_source(ProtocolInfo, Source, undefined).

%% BT-4828: also records the protocol file's path (`undefined` for a
%% REPL-defined protocol) so it can ride to codegen beside the source.
-spec track_protocol_source(map(), string(), string() | undefined) -> ok.
track_protocol_source(#{protocols := ProtocolNameBins}, Source, Path) ->
    lists:foreach(
        fun(NameBin) -> beamtalk_workspace_meta:set_protocol_source(NameBin, Source, Path) end,
        ProtocolNameBins
    ).
