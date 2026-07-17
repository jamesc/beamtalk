%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_package).

%%% **DDD Context:** Object System Context

-moduledoc """
Package reflection for Beamtalk.

Provides runtime access to package metadata backed by OTP application
environment and the `bt@{pkg}@{class}` BEAM module naming convention.

Packages map 1:1 to OTP applications whose env contains a non-empty
`classes` key (the metadata format defined by ADR 0070 Phase 4) *or* a
non-empty `type_aliases` key (ADR 0108 Phase 8) — the latter covers a
"types-only" package with `type` alias declarations and zero classes
(BT-2915), which has no class entry to derive a package name from, so its
name is instead taken from the OTP application's own atom name (app name
== package name by construction for `beamtalk build`'s generated `.app`
files).

## Responsibilities

- Enumerate loaded packages (`all/0`)
- Look up package info by name (`named/1`)
- Reverse-lookup: which package owns a class? (`package_name/1`)
- List classes within a package (`classes/1`)
- List dependencies of a package (`dependencies/1`)
- Find the OTP application hosting a package (`find_app_for_package/1`) —
  exported so other package-discovery consumers (e.g.
  `beamtalk_repl_ops_browse:browse_type_aliases/0`) share this logic
  instead of re-deriving it against only `classes`.

See also: docs/ADR/0070-package-namespaces-and-dependencies.md Section 8
""".

-export([
    all/0,
    named/1,
    package_name/1,
    classes/1,
    dependencies/1,
    find_app_for_package/1,
    %% Beamtalk FFI shim: `Package packageNameFor: #ClassName`
    packageNameFor/1
]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc """
Returns a list of loaded package names (binaries).

A "package" is any OTP application whose env includes a non-empty
`{classes, [...]}` list, or (BT-2915) a non-empty `{type_aliases, [...]}`
list with no classes. The stdlib package is always present; user packages
appear after their OTP application is loaded.
""".
-spec all() -> [binary()].
all() ->
    Apps = application:loaded_applications(),
    lists:filtermap(
        fun({AppName, _Desc, _Vsn}) ->
            case package_name_for_app(AppName) of
                undefined -> false;
                Name -> {true, Name}
            end
        end,
        Apps
    ).

-doc """
Returns a package info map for the given package name.

The name can be a binary (`<<"stdlib">>`) or an atom (`stdlib`).
Returns a map with keys: name, version, classes, dependencies, source.
Raises `#beamtalk_error{}` with kind `package_not_found` if no such package.
""".
-spec named(binary() | atom()) -> term().
named(Name) when is_atom(Name) ->
    named(atom_to_binary(Name, utf8));
named(Name) when is_binary(Name) ->
    case find_app_for_package(Name) of
        {ok, AppName} ->
            build_package_info(AppName, Name);
        error ->
            LoadedPkgs = all(),
            Hint =
                case LoadedPkgs of
                    [] ->
                        <<"No packages are currently loaded.">>;
                    _ ->
                        PkgList = lists:join(<<", ">>, LoadedPkgs),
                        iolist_to_binary([
                            <<"Loaded packages: ">>,
                            PkgList
                        ])
                end,
            Error0 = beamtalk_error:new(package_not_found, 'Package', 'named:'),
            Error1 = beamtalk_error:with_message(
                Error0,
                <<"Package not found: ", Name/binary>>
            ),
            Error2 = beamtalk_error:with_hint(Error1, Hint),
            beamtalk_error:raise(Error2)
    end.

-doc """
Returns the package name (binary) for a given class name.

Extracts the package segment from the BEAM module name (`bt@{pkg}@{class}`)
via `__beamtalk_meta/0` or from the `.app` class list metadata.
Returns `nil` if the class is not found or has no package.
""".
-spec package_name(atom()) -> binary() | nil.
package_name(ClassName) when is_atom(ClassName) ->
    %% Strategy 1: Try the class registry to get the module name,
    %% then extract from the bt@{pkg}@{class} convention.
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            %% Class not registered — try scanning .app metadata
            package_name_from_app_metadata(ClassName);
        Pid ->
            try beamtalk_object_class:module_name(Pid) of
                ModuleName ->
                    extract_package_from_module(ModuleName)
            catch
                exit:{noproc, _} ->
                    package_name_from_app_metadata(ClassName);
                exit:{timeout, _} ->
                    package_name_from_app_metadata(ClassName)
            end
    end.

-doc """
Returns the list of class names (atoms) belonging to a package.

Reads from the `.app` environment `{classes, [...]}` metadata.
Returns an empty list if the package is not found.
""".
-spec classes(binary() | atom()) -> [atom()].
classes(Name) when is_atom(Name) ->
    classes(atom_to_binary(Name, utf8));
classes(Name) when is_binary(Name) ->
    case find_app_for_package(Name) of
        {ok, AppName} ->
            case application:get_env(AppName, classes) of
                {ok, ClassList} when is_list(ClassList) ->
                    [class_entry_name(E) || E <- ClassList];
                _ ->
                    []
            end;
        error ->
            []
    end.

-doc """
Returns the list of dependency package names (binaries) for a package.

Reads the OTP application dependency list and filters to only those
applications that are themselves Beamtalk packages (have a non-empty
`classes` env, or — when `classes` is absent/empty — a non-empty
`type_aliases` env).
Returns an empty list if the package is not found.
""".
-spec dependencies(binary() | atom()) -> [binary()].
dependencies(Name) when is_atom(Name) ->
    dependencies(atom_to_binary(Name, utf8));
dependencies(Name) when is_binary(Name) ->
    case find_app_for_package(Name) of
        {ok, AppName} ->
            case application:get_key(AppName, applications) of
                {ok, DepApps} ->
                    lists:filtermap(
                        fun(DepApp) ->
                            case package_name_for_app(DepApp) of
                                undefined -> false;
                                N -> {true, N}
                            end
                        end,
                        DepApps
                    );
                undefined ->
                    []
            end;
        error ->
            []
    end.

-doc """
Beamtalk FFI shim for `Package packageNameFor: #ClassName`.

Delegates to `package_name/1`. The Beamtalk FFI maps `packageNameFor:`
to `packageNameFor/1`.
""".
-spec packageNameFor(atom()) -> binary() | nil.
packageNameFor(ClassName) -> package_name(ClassName).

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

-doc """
Extract the package name from the first entry in a class list.

All classes in a package share the same package name, so we only need
to check the first entry.
""".
-spec package_name_from_classes([map() | tuple()]) -> binary() | undefined.
package_name_from_classes([#{package := Pkg} | _]) when is_atom(Pkg) ->
    atom_to_binary(Pkg, utf8);
package_name_from_classes([#{package := Pkg} | _]) when is_binary(Pkg) ->
    Pkg;
package_name_from_classes(_) ->
    undefined.

-doc """
Returns the package name (binary) an OTP application hosts, or `undefined`
if it is not a Beamtalk package.

Prefers a non-empty `classes` env key (ADR 0070), deriving the name from
the class entries via `package_name_from_classes/1`. Falls back to a
non-empty `type_aliases` env key (BT-2915) when there are no classes to
derive a name from — a types-only package has no class entry at all, so
the OTP application's own atom name is used instead (app name == package
name by construction for `beamtalk build`'s generated `.app` files).
""".
-spec package_name_for_app(atom()) -> binary() | undefined.
package_name_for_app(AppName) ->
    case application:get_env(AppName, classes) of
        {ok, ClassList} when is_list(ClassList), ClassList =/= [] ->
            package_name_from_classes(ClassList);
        _ ->
            case application:get_env(AppName, type_aliases) of
                {ok, Aliases} when is_list(Aliases), Aliases =/= [] ->
                    atom_to_binary(AppName, utf8);
                _ ->
                    undefined
            end
    end.

-doc """
Find the OTP application that hosts a given package name.

Exported so other package-discovery consumers (e.g.
`beamtalk_repl_ops_browse:browse_type_aliases/0`,
`browse_native_modules/0`) can resolve a package name back to its OTP
application without re-deriving the classes-or-type_aliases discovery
signal themselves.
""".
-spec find_app_for_package(binary()) -> {ok, atom()} | error.
find_app_for_package(PkgName) ->
    Apps = application:loaded_applications(),
    find_app_for_package(PkgName, Apps).

find_app_for_package(_PkgName, []) ->
    error;
find_app_for_package(PkgName, [{AppName, _Desc, _Vsn} | Rest]) ->
    case package_name_for_app(AppName) of
        PkgName -> {ok, AppName};
        _ -> find_app_for_package(PkgName, Rest)
    end.

-doc """
Build a Package tagged map from OTP application metadata.

Returns a map with `$beamtalk_class => 'Package'` so the runtime
dispatches instance methods defined in Package.bt.
""".
-spec build_package_info(atom(), binary()) -> map().
build_package_info(AppName, PkgName) ->
    Version =
        case application:get_key(AppName, vsn) of
            {ok, Vsn} when is_list(Vsn) -> list_to_binary(Vsn);
            {ok, Vsn} when is_binary(Vsn) -> Vsn;
            _ -> <<"unknown">>
        end,
    ClassNames =
        case application:get_env(AppName, classes) of
            {ok, ClassList} when is_list(ClassList) ->
                [class_entry_name(E) || E <- ClassList];
            _ ->
                []
        end,
    Deps = dependencies(PkgName),
    Source =
        case application:get_key(AppName, description) of
            {ok, Desc} when is_list(Desc) -> list_to_binary(Desc);
            {ok, Desc} when is_binary(Desc) -> Desc;
            _ -> <<"">>
        end,
    #{
        '$beamtalk_class' => 'Package',
        name => PkgName,
        version => Version,
        classes => ClassNames,
        dependencies => Deps,
        source => Source
    }.

-doc "Extract the package segment from a bt@{pkg}@{class} module name.".
-spec extract_package_from_module(atom()) -> binary() | nil.
extract_package_from_module(ModuleName) when is_atom(ModuleName) ->
    ModStr = atom_to_list(ModuleName),
    case string:split(ModStr, "@", all) of
        ["bt", Pkg | _Rest] when Pkg =/= [] ->
            list_to_binary(Pkg);
        _ ->
            nil
    end.

-doc "Look up the package name for a class from .app metadata.".
-spec package_name_from_app_metadata(atom()) -> binary() | nil.
package_name_from_app_metadata(ClassName) ->
    Apps = application:loaded_applications(),
    search_apps_for_class(ClassName, Apps).

search_apps_for_class(_ClassName, []) ->
    nil;
search_apps_for_class(ClassName, [{AppName, _Desc, _Vsn} | Rest]) ->
    case application:get_env(AppName, classes) of
        {ok, ClassList} when is_list(ClassList) ->
            case lists:any(fun(E) -> class_entry_name(E) =:= ClassName end, ClassList) of
                true ->
                    case package_name_from_classes(ClassList) of
                        undefined -> nil;
                        PkgName -> PkgName
                    end;
                false ->
                    search_apps_for_class(ClassName, Rest)
            end;
        _ ->
            search_apps_for_class(ClassName, Rest)
    end.

-doc """
Extract the class name from a class entry.

Supports both the new map format (ADR 0070) and legacy tuple format.
""".
-spec class_entry_name(map() | tuple()) -> atom().
class_entry_name(#{name := Name}) -> Name;
class_entry_name({_Mod, Name, _Super}) -> Name.
