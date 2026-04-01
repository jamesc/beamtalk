%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Package reflection for Beamtalk.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Provides runtime access to package metadata backed by OTP application
%%% environment and the `bt@{pkg}@{class}` BEAM module naming convention.
%%%
%%% Packages map 1:1 to OTP applications whose env contains a `classes` key
%%% (the metadata format defined by ADR 0070 Phase 4).
%%%
%%% ## Responsibilities
%%%
%%% - Enumerate loaded packages (`all/0`)
%%% - Look up package info by name (`named/1`)
%%% - Reverse-lookup: which package owns a class? (`package_name/1`)
%%% - List classes within a package (`classes/1`)
%%% - List dependencies of a package (`dependencies/1`)
%%%
%%% @see docs/ADR/0070-package-namespaces-and-dependencies.md Section 8
-module(beamtalk_package).

-include("beamtalk.hrl").

-export([
    all/0,
    named/1,
    package_name/1,
    classes/1,
    dependencies/1,
    %% Beamtalk FFI shim: `Package packageNameFor: #ClassName`
    packageNameFor/1
]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Returns a list of loaded package names (binaries).
%%
%% A "package" is any OTP application whose env includes `{classes, [...]}`.
%% The stdlib package is always present; user packages appear after their
%% OTP application is loaded.
-spec all() -> [binary()].
all() ->
    Apps = application:loaded_applications(),
    lists:filtermap(
        fun({AppName, _Desc, _Vsn}) ->
            case application:get_env(AppName, classes) of
                {ok, ClassList} when is_list(ClassList), ClassList =/= [] ->
                    PkgName = package_name_from_classes(ClassList),
                    case PkgName of
                        undefined -> false;
                        Name -> {true, Name}
                    end;
                _ ->
                    false
            end
        end,
        Apps
    ).

%% @doc Returns a package info map for the given package name.
%%
%% The name can be a binary (`<<"stdlib">>`) or an atom (`stdlib`).
%% Returns a map with keys: name, version, classes, dependencies, source.
%% Raises `#beamtalk_error{}` with kind `package_not_found` if no such package.
-spec named(binary() | atom()) -> map().
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

%% @doc Returns the package name (binary) for a given class name.
%%
%% Extracts the package segment from the BEAM module name (`bt@{pkg}@{class}`)
%% via `__beamtalk_meta/0` or from the `.app` class list metadata.
%% Returns `nil` if the class is not found or has no package.
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

%% @doc Returns the list of class names (atoms) belonging to a package.
%%
%% Reads from the `.app` environment `{classes, [...]}` metadata.
%% Returns an empty list if the package is not found.
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

%% @doc Returns the list of dependency package names (binaries) for a package.
%%
%% Reads the OTP application dependency list and filters to only those
%% applications that are themselves Beamtalk packages (have a `classes` env).
%% Returns an empty list if the package is not found.
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
                            case application:get_env(DepApp, classes) of
                                {ok, ClassList} when is_list(ClassList), ClassList =/= [] ->
                                    PkgName = package_name_from_classes(ClassList),
                                    case PkgName of
                                        undefined -> false;
                                        N -> {true, N}
                                    end;
                                _ ->
                                    false
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

%% @doc Beamtalk FFI shim for `Package packageNameFor: #ClassName`.
%%
%% Delegates to `package_name/1`. The Beamtalk FFI maps `packageNameFor:`
%% to `packageNameFor/1`.
-spec packageNameFor(atom()) -> binary() | nil.
packageNameFor(ClassName) -> package_name(ClassName).

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

%% @private Extract the package name from the first entry in a class list.
%%
%% All classes in a package share the same package name, so we only need
%% to check the first entry.
-spec package_name_from_classes([map() | tuple()]) -> binary() | undefined.
package_name_from_classes([#{package := Pkg} | _]) when is_atom(Pkg) ->
    atom_to_binary(Pkg, utf8);
package_name_from_classes([#{package := Pkg} | _]) when is_binary(Pkg) ->
    Pkg;
package_name_from_classes(_) ->
    undefined.

%% @private Find the OTP application that hosts a given package name.
-spec find_app_for_package(binary()) -> {ok, atom()} | error.
find_app_for_package(PkgName) ->
    Apps = application:loaded_applications(),
    find_app_for_package(PkgName, Apps).

find_app_for_package(_PkgName, []) ->
    error;
find_app_for_package(PkgName, [{AppName, _Desc, _Vsn} | Rest]) ->
    case application:get_env(AppName, classes) of
        {ok, ClassList} when is_list(ClassList), ClassList =/= [] ->
            case package_name_from_classes(ClassList) of
                PkgName -> {ok, AppName};
                _ -> find_app_for_package(PkgName, Rest)
            end;
        _ ->
            find_app_for_package(PkgName, Rest)
    end.

%% @private Build a Package tagged map from OTP application metadata.
%%
%% Returns a map with `$beamtalk_class => 'Package'` so the runtime
%% dispatches instance methods defined in Package.bt.
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
        'packageClasses' => ClassNames,
        'packageDependencies' => Deps,
        source => Source
    }.

%% @private Extract the package segment from a bt@{pkg}@{class} module name.
-spec extract_package_from_module(atom()) -> binary() | nil.
extract_package_from_module(ModuleName) when is_atom(ModuleName) ->
    ModStr = atom_to_list(ModuleName),
    case string:split(ModStr, "@", all) of
        ["bt", Pkg | _Rest] when Pkg =/= [] ->
            list_to_binary(Pkg);
        _ ->
            nil
    end.

%% @private Look up the package name for a class from .app metadata.
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

%% @private Extract the class name from a class entry.
%%
%% Supports both the new map format (ADR 0070) and legacy tuple format.
-spec class_entry_name(map() | tuple()) -> atom().
class_entry_name(#{name := Name}) -> Name;
class_entry_name({_Mod, Name, _Super}) -> Name.
