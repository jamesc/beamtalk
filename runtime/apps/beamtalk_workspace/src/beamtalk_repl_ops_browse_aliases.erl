%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_browse_aliases).

%%% **DDD Context:** Workspace Context

-moduledoc """
Type-alias browsing for the system browser (`browse-type-aliases`, `browse-alias-source`): per-package alias listing, seeding-boundary visibility, row shaping and the alias-source disk read with its relative-path guard. Moved verbatim out of `beamtalk_repl_ops_browse` (BT-4907).
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([
    browse_alias_source/2,
    browse_type_aliases/0,
    validate_alias/1
]).

-ifdef(TEST).
%% The seeding-boundary exclusion + row-shaping helpers behind
%% `browse-type-aliases`, the alias-field string-shape normaliser and the
%% alias-source path guard.
-export([
    alias_visible/2,
    alias_row/3,
    package_type_aliases/1,
    safe_relative_path/1,
    alias_field_binary/1
]).
-endif.

%%% ====================================================================
%%% Op 7 — browse-type-aliases (ADR 0108 Phase 8)
%%% ====================================================================

%% Enumerate every loaded package's declared `type` aliases (ADR 0108).
%%
%% Aliases erase entirely at compile time — no BEAM module, no live process —
%% so unlike `browse-classes` there is nothing to reflect on. The `.app`
%% file's `{type_aliases, [...]}` env key (`app_file.rs`
%% `AliasMetadata`/`format_type_aliases_entry`) is the only durable record of
%% a package's declarations once compiled, so this walks `beamtalk_package:
%% all/0` and reads each package's env, the same enumeration strategy
%% `browse-native-modules` uses.
%%
%% **Seeding-boundary exclusion (ADR 0108 Implementation — "System Browser /
%% LiveView IDE"):** an `internal` alias declared in a package other than the
%% browsing session's current project package is dropped — the browse-op
%% analogue of how `AliasRegistry::add_pre_loaded` never seeds a
%% dependency's internal aliases into a consumer's alias table at compile
%% time. There is no live "current compilation" alias table to consult here
%% (this op runs against a loaded system, not a single in-flight compile), so
%% the equivalent guarantee is reconstructed per row instead of per seeded
%% table: an alias belonging to the current project package is visible
%% regardless of `internal` (an internal alias is usable within its own
%% package — ADR 0108 Semantics), and every other package's (including
%% stdlib's) internal aliases are excluded, mirroring the ADR's explicit
%% instruction that this filter "belongs at the seeding boundary ... not as a
%% query-time filter over `browse-type-aliases`'s result set" — the entries
%% are dropped during enumeration, before a row is ever built, not filtered
%% out of an already-built result list.
%%
%% Unlike `browse-native-modules`, rows are **not** deduped by name: a native
%% module atom is a genuine BEAM-global identity, but two independently
%% loaded packages can each legally declare an alias of the same name (only a
%% *consumer* of both would hit `add_pre_loaded`'s collision diagnostic) —
%% deduping here would silently hide one package's real declaration instead
%% of surfacing both, distinguished by their `package`/`source_origin` tags.
%%
%% **Types-only packages:** discovery walks `beamtalk_package:all/0`,
%% which recognises a package via a non-empty `classes` app-env key (ADR 0070)
%% *or* a non-empty `type_aliases` app-env key with no classes — a types-only
%% "shared vocabulary" package with zero classes is enumerated the same as any
%% other package, deriving its name from the OTP application's own atom name
%% (app name == package name by construction). Its aliases (public ones, and
%% internal ones from the current project) surface here the same as any other
%% package's, subject to the seeding-boundary logic above.
-spec browse_type_aliases() -> [map()].
browse_type_aliases() ->
    Packages =
        try
            %% BT-4173: all/0 answers Strings — rows are JSON-shaped
            %% maps, so unwrap to the raw name bytes here.
            [P || {beamtalk_string, P} <- beamtalk_package:all()]
        catch
            _:_ -> []
        end,
    Rows = lists:flatmap(fun type_aliases_of_package/1, Packages),
    lists:sort(
        fun(A, B) ->
            {maps:get(<<"name">>, A), maps:get(<<"package">>, A)} =<
                {maps:get(<<"name">>, B), maps:get(<<"package">>, B)}
        end,
        Rows
    ).

%% The alias rows for one package: resolve its owning app, read its declared
%% `type_aliases` env, drop internal aliases that fail the seeding-boundary
%% check, and tag each surviving row with the package's name + origin.
%%
%% Wrapped in a try/catch — unlike `native_modules_of_package/1`, whose row
%% builder only touches `module_info/1` reflection (itself already
%% defensively wrapped in `backing_source_file/1`) — because `alias_row/3`
%% assumes each `type_aliases` env entry is a map (`maps:get/2,3` raises
%% `{badmap, _}` otherwise). That assumption holds for every entry this
%% workspace's own `beamtalk build` ever writes (`app_file.rs`
%% `format_type_aliases_entry`), but a hand-edited or toolchain-mismatched
%% `.app` file is still just a text file on disk — one malformed package must
%% not take down every other package's rows in the same response, mirroring
%% `class_row/2`'s per-class isolation.
-spec type_aliases_of_package(binary()) -> [map()].
type_aliases_of_package(PkgName) ->
    try
        case beamtalk_package:find_app_for_package(PkgName) of
            {ok, App} ->
                Origin = beamtalk_repl_ops_browse_origin:package_origin(App, PkgName),
                [
                    alias_row(Entry, PkgName, Origin)
                 || Entry <- package_type_aliases(App),
                    alias_visible(Entry, Origin)
                ];
            error ->
                []
        end
    catch
        Class:Reason ->
            ?LOG_WARNING(
                "browse-type-aliases: skipping package ~p: ~p:~p",
                [PkgName, Class, Reason],
                #{domain => [beamtalk, runtime]}
            ),
            []
    end.

%% An app's declared type aliases — the `.app` `{type_aliases, [...]}` env key.
%% `[]` for a package with no `type` declarations (most packages),
%% or one built before this key existed — no fallback reconstruction needed,
%% unlike `package_native_modules/1`'s stdlib special case, since there is no
%% legacy alias surface to recover data from.
-spec package_type_aliases(atom()) -> [map()].
package_type_aliases(App) ->
    case application:get_env(App, type_aliases) of
        {ok, Aliases} when is_list(Aliases) -> Aliases;
        _ -> []
    end.

%% Seeding-boundary exclusion (see `browse_type_aliases/0` doc): an internal
%% alias is visible only from its own package (`Origin =:= project`) — every
%% other package's internal aliases, including stdlib's, are dropped. A
%% non-internal (public) alias is always visible.
-spec alias_visible(map(), binary()) -> boolean().
alias_visible(#{internal := true}, Origin) ->
    Origin =:= <<"project">>;
alias_visible(#{internal := _}, _Origin) ->
    true;
alias_visible(_Entry, _Origin) ->
    %% No `internal` key at all (should not happen — every entry the Rust
    %% build pipeline emits carries it) — fail safe by treating as internal.
    false.

%% One AliasRow (ADR 0108: `name`, `expansion`, `doc`, `source_file`,
%% `internal`), extended with `package`/`source_origin` the way `ClassRow`
%% and `NativeModuleRow` are — the LiveView "Type Aliases" panel and the VS
%% Code sidebar both group/badge by package the same way the Classes and
%% Native panels already do.
-spec alias_row(map(), binary(), binary()) -> map().
alias_row(Entry, PkgName, Origin) ->
    #{
        <<"name">> => alias_field_binary(maps:get(name, Entry, <<>>)),
        <<"expansion">> => alias_field_binary(maps:get(expansion, Entry, <<>>)),
        <<"doc">> => alias_field_or_null(maps:get(doc, Entry, undefined)),
        <<"source_file">> => alias_field_binary(maps:get(source_file, Entry, <<>>)),
        <<"internal">> => maps:get(internal, Entry, false),
        <<"package">> => PkgName,
        <<"source_origin">> => Origin
    }.

%% `.app`-file alias fields round-trip as atoms (`name`), Erlang strings
%% (`expansion`/`source_file`/`doc`, via `escape_erlang_string` +
%% `file:consult`), or `undefined` (`doc`) — never binaries, since
%% `app_file.rs` emits a literal `.app` Erlang term, not a JSON payload.
%% Normalise whichever shape comes back to the binary the term
%% contract requires.
-spec alias_field_binary(atom() | binary() | string()) -> binary().
alias_field_binary(V) when is_binary(V) ->
    V;
alias_field_binary(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
alias_field_binary(V) when is_list(V) ->
    case unicode:characters_to_binary(V) of
        B when is_binary(B) -> B;
        _ -> list_to_binary(V)
    end;
alias_field_binary(_) ->
    <<>>.

-spec alias_field_or_null(undefined | atom() | binary() | string()) -> binary() | null.
alias_field_or_null(undefined) ->
    null;
alias_field_or_null(V) ->
    alias_field_binary(V).

%%% ====================================================================
%%% Op 8 — browse-alias-source
%%% ====================================================================

%% Read-only source view for a declared type alias, keyed by `name` +
%% `package` (aliases are not deduped by name across packages —
%% `browse_type_aliases/0`'s no-dedupe note — so both are needed to
%% disambiguate a same-named alias declared by more than one package).
%%
%% Unlike a `native:` class or standalone native module, a `type Name = ...`
%% declaration produces no BEAM module (aliases erase entirely), so there is
%% no compiled-module attribute / `module_info(compile)` path to recover an
%% absolute file location from at read time — `AliasMetadata.source_file`
%% (`app_file.rs`) is a package-relative display path like
%% `"src/restart_strategy.bt"`, not an absolute one. Project-owned aliases
%% resolve against the workspace's own recorded project root
%% (`beamtalk_workspace_meta:get_metadata/0`); stdlib/dependency aliases have
%% no live path cache to resolve against (the stdlib source tree and a
%% fetched dependency's checkout are Rust-CLI/build-time-only concepts) and
%% degrade to the honest `content = null` empty state — the same pattern
%% `browse-native-source` already uses for a `.beam`-only module. `editable`
%% is always `false` — there is no `save-alias-source` op.
-spec browse_alias_source(binary(), binary() | undefined) -> beamtalk_repl_ops:op_result().
browse_alias_source(Name, Package) ->
    case find_alias_row(Name, Package) of
        undefined ->
            beamtalk_repl_ops_browse_support:arg_error(
                <<"browse-alias-source">>, alias_not_found_message(Name, Package)
            );
        Row ->
            {value, alias_source_value(Row)}
    end.

%% When `Package` is known (the common case — the UI's click always carries
%% it), scan only that package's own rows via `type_aliases_of_package/1`
%% (app resolution + the seeding-boundary visibility filter + row-building,
%% all reused as-is) rather than recomputing and sorting the full
%% cross-package `browse_type_aliases/0` catalog just to find one row. With no
%% `Package` (REPL/MCP callers that haven't resolved one), there is no
%% cheaper option than the full scan — no cross-package name index exists.
-spec find_alias_row(binary(), binary() | undefined) -> map() | undefined.
find_alias_row(Name, undefined) ->
    first_match(fun(R) -> maps:get(<<"name">>, R) =:= Name end, browse_type_aliases());
find_alias_row(Name, Package) ->
    first_match(fun(R) -> maps:get(<<"name">>, R) =:= Name end, type_aliases_of_package(Package)).

-spec first_match(fun((map()) -> boolean()), [map()]) -> map() | undefined.
first_match(Pred, Rows) ->
    case lists:filter(Pred, Rows) of
        [Row | _] -> Row;
        [] -> undefined
    end.

-spec alias_not_found_message(binary(), binary() | undefined) -> binary().
alias_not_found_message(Name, undefined) ->
    iolist_to_binary([<<"type alias `">>, Name, <<"` not found">>]);
alias_not_found_message(Name, Package) ->
    iolist_to_binary([
        <<"type alias `">>, Name, <<"` not found in package `">>, Package, <<"`">>
    ]).

-spec alias_source_value(map()) -> map().
alias_source_value(#{
    <<"name">> := Name,
    <<"source_file">> := SourceFile,
    <<"source_origin">> := Origin,
    <<"package">> := Package
}) ->
    #{
        <<"name">> => Name,
        <<"package">> => Package,
        <<"source_file">> => SourceFile,
        <<"source_origin">> => Origin,
        <<"editable">> => false,
        <<"content">> => alias_disk_content(Origin, SourceFile)
    }.

%% Project-owned aliases resolve against the workspace's own recorded project
%% root; stdlib/dependency aliases have no live path cache to resolve against
%% (see `browse_alias_source/2` doc), so they degrade to `null` — never an
%% error.
%%
%% Security: `SourceFile` comes from a compiled `.app` file's `type_aliases`
%% entry (`type_aliases_of_package/1`'s own doc already flags this as
%% "still just a text file on disk" — a hand-edited or toolchain-mismatched
%% one is not a from-source-only threat model). `filename:join/2` does NOT
%% sanitize traversal: an absolute `SourceFile` silently discards the root
%% entirely (`filename:join("/proj", "/etc/passwd") =:= "/etc/passwd"`), and a
%% `..`-laden relative one escapes it at the OS level. `safe_relative_path/1`
%% rejects both *before* any join/read is attempted, so a malformed entry
%% degrades to the same `null` empty state as a merely-missing file, never a
%% host-filesystem read outside the project tree.
-spec alias_disk_content(binary(), binary()) -> binary() | null.
alias_disk_content(<<"project">>, SourceFile) when byte_size(SourceFile) > 0 ->
    case safe_relative_path(SourceFile) of
        false ->
            null;
        true ->
            %% `safe_relative_path/1` already guarantees `SourceFile` has no
            %% `..` component and is not itself absolute, so joining it onto
            %% `ProjectPath` cannot resolve outside that root — no further
            %% containment check (`beamtalk_workspace_file:path_inside/2`)
            %% is needed.
            case beamtalk_workspace_meta:get_metadata() of
                {ok, #{project_path := ProjectPath}} when is_binary(ProjectPath) ->
                    Path = filename:join(binary_to_list(ProjectPath), binary_to_list(SourceFile)),
                    case file:read_file(Path) of
                        {ok, Bin} -> Bin;
                        {error, _} -> null
                    end;
                _ ->
                    null
            end
    end;
alias_disk_content(_Origin, _SourceFile) ->
    null.

%% True iff `SourceFile` is a relative path with no `..` component — i.e. it
%% cannot escape whatever root it is later joined against. Rejects both an
%% absolute path (which `filename:join/2` would let override the root
%% entirely) and any `..` segment (which escapes the root at the OS level
%% even though it survives a naive string-prefix containment check on the
%% *unresolved* joined path).
-spec safe_relative_path(binary()) -> boolean().
safe_relative_path(SourceFile) ->
    Str = binary_to_list(SourceFile),
    filename:pathtype(Str) =:= relative andalso
        not lists:member("..", filename:split(Str)).

%% `name` is required; `package` is optional (disambiguates a
%% same-named alias declared by more than one package). Aliases are not
%% runtime atoms/modules, so — unlike `validate_class`/`validate_module` —
%% no atom resolution is needed, just non-empty binary validation.
-spec validate_alias(map()) -> {ok, {binary(), binary() | undefined}} | {error, binary()}.
validate_alias(Params) ->
    case maps:get(<<"name">>, Params, undefined) of
        Name when is_binary(Name), byte_size(Name) > 0 ->
            Package =
                case maps:get(<<"package">>, Params, undefined) of
                    P when is_binary(P), byte_size(P) > 0 -> P;
                    _ -> undefined
                end,
            {ok, {Name, Package}};
        _ ->
            {error, <<"`name` (non-empty string) is required">>}
    end.
