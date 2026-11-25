%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_browse_native).

%%% **DDD Context:** Workspace Context

-moduledoc """
Native-module browsing for the system browser (ADR 0096, ADR 0056): native module listing and source (`browse-native-source`, `browse-native-modules`), the editable-target authorization seam, and the `self delegate` caller/clause analysis. Moved verbatim out of `beamtalk_repl_ops_browse` (BT-4907).
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([
    browse_native_module_source/2,
    browse_native_modules/0,
    browse_native_source/2,
    is_native_delegate/3,
    meta_is_native/1,
    native_meta_of/1,
    optional_selector/1
]).

%% The editable-native-target resolver, called by the save-native-source
%% op (`beamtalk_repl_ops_load`) to re-derive — server-side, never trusting the
%% client — whether a native module is a project-owned, editable `.erl` and where
%% its source lives. Authorization seam: only `<<"project">>` is writable.
-export([native_module_editable_target/1]).

%% The ADR 0056 `self delegate` callers of a native module — the
%% complement of the explicit `(Erlang <module>)` FFI callers
%% `beamtalk_xref:callers_of_native_module/1` reports. Called by the
%% `callers_of_native_module` nav op (`beamtalk_repl_ops_nav`), which merges both
%% sets, so a native module that backs an actor class lists that class's
%% delegating methods under "Callers".
-export([delegate_callers_of_native_module/1]).

-ifdef(TEST).
%% Pure helpers exercised directly in EUnit: clause parsing and the
%% delegate-source marker have no live-class dependency, and the
%% `dispatch_<selector>` export → `self delegate` selector recovery.
-export([
    handle_call_clause_lines/1,
    clause_selector/1,
    delegate_exported/2,
    self_delegate_selectors/1
]).
-endif.

%%% ====================================================================
%%% Op 5 — browse-native-source
%%% ====================================================================

%% The backing Erlang source for a `native:` class (ADR 0056). A native class
%% only has Beamtalk *delegating* methods (`=> self delegate`); the real logic is
%% gen_server callbacks (`handle_call/3`, `handle_info/2`) in a SEPARATE module
%% (e.g. `Subprocess` → `beamtalk_subprocess`). So the reliable unit is the whole
%% backing module, with a best-effort `handle_call` clause line-map and, when a
%% `selector` is given, the matching clause (the selector↔clause mapping is loose
%% — e.g. `Subprocess>>readLine` replies from `handle_info`, not its `handle_call`
%% clause). Read-only; `editable` reflects `source_origin` (project-owned native
%% is editable in a future R/W phase, stdlib/dependency never).
-spec browse_native_source(atom(), atom() | undefined) -> beamtalk_repl_ops:op_result().
browse_native_source(ClassName, Selector) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined ->
            beamtalk_repl_ops_browse_support:not_found_error(<<"browse-native-source">>, ClassName);
        ClassPid ->
            ModName = beamtalk_runtime_api:module_name(ClassPid),
            NativeMeta = native_meta_of(ModName),
            case beamtalk_class_registry:meta_backing_module(NativeMeta) of
                none ->
                    beamtalk_repl_ops_browse_support:arg_error(
                        <<"browse-native-source">>,
                        iolist_to_binary([
                            <<"class `">>,
                            atom_to_binary(ClassName, utf8),
                            <<"` is not native-backed">>
                        ])
                    );
                Backing when is_atom(Backing) ->
                    native_source_value(ClassName, ModName, Backing, Selector)
            end
    end.

-spec native_source_value(atom(), atom(), atom(), atom() | undefined) ->
    beamtalk_repl_ops:op_result().
native_source_value(ClassName, ModName, Backing, Selector) ->
    {BackingFile, Content} = backing_source(Backing),
    %% `source_origin` keys editability off where the .erl lives: stdlib and
    %% dependency native are read-only; project-owned native is the seam where a
    %% future R/W phase enables editing (out of scope here). Also
    %% require the source content to be present: a project class whose `.erl` was
    %% stripped from the release (or is unreadable) has nothing to edit even
    %% though its build-time path is known, so it must not advertise `editable`.
    SourceOrigin = beamtalk_repl_ops_browse_origin:source_origin_of(ModName, BackingFile),
    Clauses = handle_call_clause_lines(Content),
    {value, #{
        <<"class">> => atom_to_binary(ClassName, utf8),
        <<"backing_module">> => atom_to_binary(Backing, utf8),
        <<"source_file">> => BackingFile,
        <<"source_origin">> => SourceOrigin,
        <<"editable">> => SourceOrigin =:= <<"project">> andalso Content =/= null,
        <<"content">> => Content,
        <<"clauses">> => Clauses,
        <<"selected_clause">> => selected_clause(Clauses, Selector)
    }}.

%% The read-only native pane keyed directly by a standalone native
%% module (not a `native:` class's backing module). Reuses the exact same
%% reader/clause machinery `native_source_value/4` uses — the only difference is
%% the key (a module the `browse-native-modules` enumeration surfaced) and that
%% there is no owning class, so `class` is `null` and `backing_module` is the
%% module itself. A module with no readable on-disk source degrades to
%% `content = null` (the "source not available" empty state), never an error.
-spec browse_native_module_source(atom(), atom() | undefined) ->
    beamtalk_repl_ops:op_result().
browse_native_module_source(Module, Selector) ->
    {SourceFile, Content} = backing_source(Module),
    SourceOrigin = native_module_origin(Module, SourceFile),
    Clauses = handle_call_clause_lines(Content),
    {value, #{
        <<"class">> => null,
        <<"backing_module">> => atom_to_binary(Module, utf8),
        <<"source_file">> => SourceFile,
        <<"source_origin">> => SourceOrigin,
        %% Project-owned native modules are editable (edit → compile →
        %% reload → write-back, gated to the Owner via the `save-native-source`
        %% op's `:execute` capability). Deps/stdlib natives stay strictly
        %% read-only. Require readable on-disk content too: a `.beam`-only
        %% (release-stripped) module has nothing to edit even though its path is
        %% known, so it must not advertise `editable`. The save op re-derives this
        %% same predicate server-side (`native_module_editable_target/1`) — the
        %% client flag is a UX hint, never the authority.
        <<"editable">> =>
            SourceOrigin =:= <<"project">> andalso SourceFile =/= null andalso Content =/= null,
        <<"content">> => Content,
        <<"clauses">> => Clauses,
        <<"selected_clause">> => selected_clause(Clauses, Selector)
    }}.

-doc """
Resolve a native module to its editable on-disk `.erl` target.

This is the **server-side authorization seam** for `save-native-source`: it
re-derives — independently of any client-supplied path — where the module's
source lives and whether it is a *project-owned* native that may be written. It
NEVER trusts a path from the request; the path is read from the module's own
compile info (`module_info(compile)` → `source`), so a crafted request cannot
redirect a write outside the project tree.

Returns:
  - `{ok, SourceFile}` — `SourceFile` is the absolute `.erl` path, and the module
    is project-owned (origin `<<"project">>`) with readable on-disk source.
  - `{error, not_editable}` — a deps/stdlib native, or no readable on-disk source
    (`.beam`-only / release-stripped). The caller maps this to a structured
    `#beamtalk_error{}`.
""".
-spec native_module_editable_target(atom()) -> {ok, string()} | {error, not_editable}.
native_module_editable_target(Module) when is_atom(Module) ->
    case backing_source_file(Module) of
        undefined ->
            {error, not_editable};
        Path when is_list(Path) ->
            SourceFileBin =
                case unicode:characters_to_binary(Path) of
                    B when is_binary(B) -> B;
                    _ -> list_to_binary(Path)
                end,
            case native_module_origin(Module, SourceFileBin) of
                <<"project">> -> {ok, Path};
                _ -> {error, not_editable}
            end
    end.

%%% ====================================================================
%%% Op 6 — browse-native-modules
%%% ====================================================================

%% Enumerate the loaded packages' *hand-written* native Erlang modules so the
%% System Browser can navigate to a dependency's `.erl` files even when no
%% `native:` class backs them (the reported beamtalk-http case: a dependency
%% loaded without instantiating its classes is invisible to the class-keyed
%% `browse-classes`).
%%
%% **Filter rule (ADR 0072 `native_modules`):** a "native module" is a
%% module a Beamtalk package *declares* it ships, never the whole code path:
%%
%%   1. Walk `beamtalk_package:all/0` — every loaded OTP application that is a
%%      Beamtalk package (has a non-empty `classes` env, ADR 0070, or a
%%      non-empty `type_aliases` env with no classes).
%%   2. For each package's app, take its declared native modules:
%%        * user / dependency packages: the generated `{native_modules, [...]}`
%%          app-env key (ADR 0072 Phase 1; the `native/*.erl` stems);
%%        * stdlib (whose `.app.src` carries no `native_modules` key): the app's
%%          OTP `modules` list filtered to non-`bt@` modules — i.e. the
%%          hand-written `beamtalk_*` backing modules, not the `bt@stdlib@*`
%%          class facades.
%%   3. EXCLUDE the auto-generated `bt@{pkg}@{class}` class facade modules by
%%      construction: those live in the OTP `modules` key, never in
%%      `native_modules`, and the stdlib filter drops the `bt@` prefix — so a
%%      class facade is never duplicated here (it is already in browse-classes).
%%
%% Each row carries the module name, on-disk source path (compile-info `source`,
%% like `backing_source/1`; `null` when no readable source → not openable),
%% package name, and origin (`project`/`dependency`/`stdlib`). Rows are sorted by
%% module name for a stable tree.
-spec browse_native_modules() -> [map()].
browse_native_modules() ->
    Packages =
        try
            %% BT-4173: all/0 answers Strings — rows are JSON-shaped
            %% maps, so unwrap to the raw name bytes here.
            [P || {beamtalk_string, P} <- beamtalk_package:all()]
        catch
            _:_ -> []
        end,
    Rows = lists:flatmap(fun native_modules_of_package/1, Packages),
    %% Dedup by module name (a module declared by more than one app is listed
    %% once) then sort for a stable tree.
    Deduped = maps:values(
        lists:foldl(
            fun(#{<<"module">> := M} = Row, Acc) -> maps:put(M, Row, Acc) end,
            #{},
            Rows
        )
    ),
    lists:sort(
        fun(A, B) -> maps:get(<<"module">>, A) =< maps:get(<<"module">>, B) end,
        Deduped
    ).

%% The native-module rows for one package: resolve its owning app, read the
%% declared native modules, and build a row per module tagged with the package's
%% name + origin. A package whose app cannot be resolved contributes nothing.
-spec native_modules_of_package(binary()) -> [map()].
native_modules_of_package(PkgName) ->
    case beamtalk_package:find_app_for_package(PkgName) of
        {ok, App} ->
            Origin = beamtalk_repl_ops_browse_origin:package_origin(App, PkgName),
            [native_module_row(Mod, PkgName, Origin) || Mod <- package_native_modules(App)];
        error ->
            []
    end.

%% An app's declared native modules. User / dependency packages carry the
%% generated `{native_modules, [...]}` env key (ADR 0072); stdlib has none, so
%% fall back to the OTP `modules` list filtered to the non-`bt@` (hand-written)
%% modules — which excludes the `bt@stdlib@*` class facades by construction.
-spec package_native_modules(atom()) -> [atom()].
package_native_modules(App) ->
    case beamtalk_package:native_modules_of_app(App) of
        [] -> stdlib_fallback_native_modules(App);
        Mods -> Mods
    end.

%% Stdlib (and any package with no `native_modules` env): the OTP `modules` list
%% minus the `bt@*` class facades — i.e. the hand-written `beamtalk_*` backing
%% modules. A non-stdlib app with no `native_modules` key (an older build) gets
%% nothing rather than its facade modules misreported as native.
-spec stdlib_fallback_native_modules(atom()) -> [atom()].
stdlib_fallback_native_modules(beamtalk_stdlib = App) ->
    case application:get_key(App, modules) of
        {ok, Mods} when is_list(Mods) ->
            [M || M <- Mods, is_atom(M), not is_bt_module(M)];
        _ ->
            []
    end;
stdlib_fallback_native_modules(_App) ->
    [].

%% True for an auto-generated `bt@{pkg}@{class}` class facade module atom.
-spec is_bt_module(atom()) -> boolean().
is_bt_module(Module) when is_atom(Module) ->
    beamtalk_module_name:is_class_module(Module).

%% Source origin for a standalone native module's pane. A native module is not a
%% `bt@{pkg}@{class}` atom, so `source_origin_of/2`'s package-segment path can't
%% classify it; instead, badge stdlib by app membership (the stdlib app declares
%% it) and fall back to the on-disk path check (`classify_origin/3`) for the
%% project/dependency split.
-spec native_module_origin(atom(), binary() | null) -> binary().
native_module_origin(Module, SourceFile) ->
    case is_stdlib_native_module(Module) of
        true -> <<"stdlib">>;
        false -> beamtalk_repl_ops_browse_origin:path_origin(SourceFile)
    end.

%% True when `Module` is one of the stdlib app's hand-written native modules.
-spec is_stdlib_native_module(atom()) -> boolean().
is_stdlib_native_module(Module) ->
    lists:member(Module, stdlib_fallback_native_modules(beamtalk_stdlib)).

%% One native-module row: module name, on-disk source path (compile-info
%% `source`, `null` when no readable source → not openable), package, and origin.
-spec native_module_row(atom(), binary(), binary()) -> map().
native_module_row(Module, PkgName, Origin) ->
    %% Path only (no `file:read_file/1`) — enumeration just needs the path +
    %% `openable` flag; the content is read lazily by `browse-native-source` when
    %% a module is actually opened. Mirrors `backing_source/1`'s path
    %% normalisation without the per-module read.
    SourceFile =
        case backing_source_file(Module) of
            undefined ->
                null;
            Path ->
                case unicode:characters_to_binary(Path) of
                    B when is_binary(B) -> B;
                    _ -> list_to_binary(Path)
                end
        end,
    #{
        <<"module">> => atom_to_binary(Module, utf8),
        <<"source_file">> => SourceFile,
        <<"package">> => PkgName,
        <<"source_origin">> => Origin,
        %% Openable iff a readable on-disk `.erl` was found (`.beam`-only /
        %% release-stripped modules are listed but not openable — the existing
        %% "Erlang source not available" path).
        <<"openable">> => SourceFile =/= null
    }.

%% The backing module's on-disk `.erl` and its content. The source path comes
%% from the module's own compile info (`module_info(compile)` → `source`), so a
%% `.beam`-only release (no shipped source) degrades to `{null, null}` —
%% "source not available" — rather than an error. A path that no longer exists
%% (moved/release-stripped) returns its known path with `null` content.
-spec backing_source(atom()) -> {binary() | null, binary() | null}.
backing_source(Backing) ->
    case backing_source_file(Backing) of
        undefined ->
            {null, null};
        Path ->
            %% `unicode:characters_to_binary/1` returns an `{error, _, _}` /
            %% `{incomplete, _, _}` tuple (not a binary) for a malformed path, which
            %% must not leak into the result map as `source_file`. Fall back to a
            %% raw Latin-1 binary so the field is always a binary. Build-machine
            %% paths are virtually always UTF-8, so the fallback is pathological-only.
            PathBin =
                case unicode:characters_to_binary(Path) of
                    B when is_binary(B) -> B;
                    _ -> list_to_binary(Path)
                end,
            case file:read_file(Path) of
                {ok, Bin} -> {PathBin, Bin};
                {error, _} -> {PathBin, null}
            end
    end.

-spec backing_source_file(atom()) -> string() | undefined.
backing_source_file(Backing) ->
    try Backing:module_info(compile) of
        Info when is_list(Info) ->
            case lists:keyfind(source, 1, Info) of
                {source, Path} when is_list(Path) -> Path;
                _ -> undefined
            end;
        _ ->
            undefined
    catch
        _:_ -> undefined
    end.

%% Best-effort `handle_call` clause line-map: each clause whose first message
%% element is a concrete selector (a quoted atom like `'writeLine:'` or a bare
%% lowercase atom like `readLine`) becomes `#{selector, line}`. Generic clauses
%% — `handle_call({Selector, Args, _}, …)`, the catch-all `handle_call(Msg, …)`
%% — bind variables (uppercase / `_`), match no selector, and are skipped.
%% Matches single-line clause heads only; a head split across multiple lines is
%% not captured (the selector and its trailing comma must appear on one line).
-spec handle_call_clause_lines(binary() | null) -> [map()].
handle_call_clause_lines(null) ->
    [];
handle_call_clause_lines(Content) when is_binary(Content) ->
    Lines = binary:split(Content, <<"\n">>, [global]),
    {Rows, _} = lists:foldl(
        fun(Line, {Acc, N}) ->
            case clause_selector(Line) of
                {ok, Sel} -> {[#{<<"selector">> => Sel, <<"line">> => N} | Acc], N + 1};
                none -> {Acc, N + 1}
            end
        end,
        {[], 1},
        Lines
    ),
    lists:reverse(Rows).

%% A line's `handle_call` selector, or `none`. The first tuple element of the
%% call message, captured whether written as a quoted atom (`'writeLine:'`) or a
%% bare lowercase atom (`readLine`). The lowercase-initial requirement excludes
%% the generic clauses that bind variables (`{Selector, Args, _}`, `Msg`). A
%% single capture group avoids `re`'s truncation of trailing unmatched groups.
%%
%% This is one half of a shared selector→clause-line definition. The
%% Rust LSP mirrors this algorithm in
%% `definition_provider::clause_selector` (so go-to-implementation lands on the
%% same clause the System Browser jump does), and both are pinned to the shared
%% conformance corpus
%% `test/fixtures/handle_call_clause_corpus.json`
%% (asserted here by `clause_selector_corpus_test/0` and on the Rust side by
%% `definition_provider::tests::clause_selector_matches_shared_corpus`). Keep the
%% regex and the Rust matcher in lockstep; extend the corpus when adding a case.
-spec clause_selector(binary()) -> {ok, binary()} | none.
clause_selector(Line) ->
    %% Anchored to line start (after optional indentation): a `handle_call`
    %% mention mid-line — inside a comment (`%% see handle_call({foo, …`) or an
    %% assignment (`R = handle_call({…`) — is not a clause head and must not
    %% produce a spurious clause row.
    Pattern = <<"^\\s*handle_call\\(\\{\\s*'?([a-z][A-Za-z0-9_:]*)'?\\s*,">>,
    case re:run(Line, Pattern, [{capture, all_but_first, binary}]) of
        {match, [Sel]} when byte_size(Sel) > 0 -> {ok, Sel};
        _ -> none
    end.

%% The clause row for a requested selector, or `null` (no selector asked, or no
%% matching `handle_call` clause — a delegate whose work lives elsewhere).
-spec selected_clause([map()], atom() | undefined) -> map() | null.
selected_clause(_Clauses, undefined) ->
    null;
selected_clause(Clauses, Selector) ->
    SelBin = atom_to_binary(Selector, utf8),
    case lists:search(fun(#{<<"selector">> := S}) -> S =:= SelBin end, Clauses) of
        {value, Row} -> Row;
        false -> null
    end.

%% Native facade metadata (ADR 0056): the facade module exports
%% `__beamtalk_meta/0` carrying `native => true` and `backing_module => atom()`.
%% Non-native / non-Beamtalk modules don't export it → `#{}`.
-spec native_meta_of(atom()) -> map().
native_meta_of(ModName) when is_atom(ModName) ->
    case beamtalk_class_metadata:read_meta(ModName) of
        {ok, Meta} -> Meta;
        not_available -> #{}
    end.

-spec meta_is_native(map()) -> boolean().
meta_is_native(Meta) ->
    maps:get(native, Meta, false) =:= true.

%% True when `Selector` (instance side) is a `self delegate` method on the native
%% facade `ModName`. The compiler emits a `dispatch_<selector>` function ONLY for
%% a `self delegate` method (native_facade.rs collects `is_self_delegate()` methods
%% and generates one dispatch fun each), so the presence of that export is the
%% compiler's own decision surfaced at runtime — not a body-text guess. Class-side
%% selectors are never delegated this way, so they are always `false`.
-spec is_native_delegate(atom(), boolean(), atom()) -> boolean().
is_native_delegate(ModName, false, Selector) when is_atom(ModName) ->
    delegate_exported(beamtalk_module_activation:safe_module_exports(ModName), Selector);
is_native_delegate(_ModName, _ClassSide, _Selector) ->
    false.

%% Pure: does `Exports` contain a `dispatch_<Selector>` function (any arity)? The
%% dispatch fun name embeds the selector verbatim, keyword colon included
%% (`dispatch_readLine`, `'dispatch_writeLine:'`), so an exact name compare is
%% exact — no arity arithmetic, no atom-table growth.
-spec delegate_exported([{atom(), arity()}], atom()) -> boolean().
delegate_exported(Exports, Selector) ->
    Target = "dispatch_" ++ atom_to_list(Selector),
    lists:any(fun({Name, _Arity}) -> atom_to_list(Name) =:= Target end, Exports).

%%% ====================================================================
%%% Native-module delegate callers
%%% ====================================================================

-doc """
The Beamtalk methods that delegate into native module `Module` via ADR 0056
`self delegate` — the complement of the explicit `(Erlang <module>)` FFI callers
`beamtalk_xref:callers_of_native_module/1` reports.

A `native:` class compiles each `self delegate` method into a `dispatch_<selector>`
function on its facade module, routed through `beamtalk_actor:sync_send/3`. Those
sends are generated dispatch, not `erlang_ffi` sends, so the xref FFI index never
sees them and the reverse "Callers" view would miss the most common native module
in a project — the gen_server that backs an actor class. Here we walk the loaded
classes, keep those whose backing native module is `Module`, and surface each
class's `self delegate` instance methods as `native_caller_row()`s anchored at the
delegating method's own header line (the same shape the FFI callers use, so the
IDE renders both together).

Class-side selectors are never delegated this way, so every row is instance-side.
Returns `[]` for a module that backs no loaded class (or the unresolved-module
sentinel), so a module with neither FFI callers nor delegating classes keeps the
honest empty state.

The candidate class names come from `beamtalk_class_registry`'s
backing-module reverse index — an O(1) ETS `lookup/2` keyed by `Module`,
populated at class registration/reload time — instead of walking every loaded
class via `all_classes/0` and probing `erlang:function_exported/3` on each.
""".
-spec delegate_callers_of_native_module(atom()) -> [beamtalk_xref:native_caller_row()].
delegate_callers_of_native_module(none) ->
    %% `none` is `meta_backing_module/1`'s "no backing module" sentinel, never a
    %% real native module name. Guarding it here keeps a `module="none"` query
    %% from match-binding against that sentinel in `delegate_rows_for_class_name/2`
    %% and mis-attributing the delegates of a class that reports no backing module.
    [];
delegate_callers_of_native_module(Module) when is_atom(Module) ->
    ClassNames =
        try
            beamtalk_class_registry:classes_backing_module(Module)
        catch
            _:_ -> []
        end,
    lists:flatmap(
        fun(ClassName) -> delegate_rows_for_class_name(ClassName, Module) end, ClassNames
    ).

%% One `native_caller_row()` per `self delegate` instance method of `ClassName`,
%% but only when that class's backing native module is still `Module`. The
%% index in `beamtalk_class_registry:classes_backing_module/1` only narrows the
%% candidate set — this re-verifies against a live process (a class name in the
%% index can be a stale row left by a hard crash, per
%% `forget_backing_module_entries/2`'s doc) and degrades to `[]` on any
%% reflection failure, mirroring `class_row/2`'s per-class isolation so the
%% Callers view still lists the other classes' delegates.
-spec delegate_rows_for_class_name(atom(), atom()) -> [beamtalk_xref:native_caller_row()].
delegate_rows_for_class_name(ClassName, Module) ->
    try
        case beamtalk_runtime_api:whereis_class(ClassName) of
            undefined ->
                [];
            Pid ->
                ModName = beamtalk_runtime_api:module_name(Pid),
                case beamtalk_class_registry:meta_backing_module(native_meta_of(ModName)) of
                    Module ->
                        Exports = beamtalk_module_activation:safe_module_exports(ModName),
                        [delegate_row(ClassName, Sel) || Sel <- self_delegate_selectors(Exports)];
                    _ ->
                        []
                end
        end
    catch
        _:_ -> []
    end.

%% The `self delegate` instance selectors a facade exposes, recovered from its
%% `dispatch_<selector>` exports (the compiler's own `is_self_delegate` decision,
%% native_facade.rs). The dispatch name embeds the selector verbatim — keyword
%% colon included (`dispatch_readLine`, `'dispatch_writeLine:'`) — so stripping the
%% `dispatch_` prefix recovers it exactly. `usort` collapses any duplicate export
%% pair; a stripped name that is not already an interned atom (never true for a
%% real, registered method) is skipped rather than growing the atom table.
-spec self_delegate_selectors([{atom(), arity()}]) -> [atom()].
self_delegate_selectors(Exports) ->
    lists:usort(lists:filtermap(fun dispatch_export_selector/1, Exports)).

-spec dispatch_export_selector({atom(), arity()}) -> {true, atom()} | false.
dispatch_export_selector({Name, _Arity}) ->
    case atom_to_list(Name) of
        "dispatch_" ++ Rest when Rest =/= [] ->
            case beamtalk_text:to_existing_atom(Rest) of
                {ok, Selector} -> {true, Selector};
                error -> false
            end;
        _ ->
            false
    end.

%% A single delegate caller row: the delegating class + selector, anchored at the
%% method's own header line from the xref index. A `self delegate` method is always
%% registered, so `method_info/3` resolves its line; the line=1 fallback is purely
%% defensive so the row stays clickable rather than being silently dropped.
-spec delegate_row(atom(), atom()) -> beamtalk_xref:native_caller_row().
delegate_row(ClassName, Selector) ->
    Line =
        case beamtalk_repl_row:method_line(ClassName, false, Selector) of
            null -> 1;
            L -> L
        end,
    #{owner => ClassName, class_side => false, method => Selector, line => Line}.

%% Optional `selector` param for `browse-native-source`: absent/empty → no clause
%% selection; otherwise resolved to an existing atom (an unknown selector reads as
%% a sentinel → no matching clause, never an atom-table growth).
-spec optional_selector(map()) -> atom() | undefined.
optional_selector(Params) ->
    case beamtalk_repl_params:nonempty_binary(Params, <<"selector">>) of
        {ok, Sel} ->
            {ok, Atom} = beamtalk_repl_params:existing_atom_param(Sel, sentinel),
            Atom;
        error ->
            undefined
    end.
