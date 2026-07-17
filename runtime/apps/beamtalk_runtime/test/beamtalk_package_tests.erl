%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Object System Context

-module(beamtalk_package_tests).

-moduledoc """
EUnit tests for beamtalk_package module.

Tests package reflection: enumeration, lookup, reverse-lookup,
class listing, and dependency queries.
""".

-include("beamtalk.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% Test setup — ensure beamtalk_stdlib app metadata is loaded
%%% ============================================================================

setup() ->
    _ = application:load(beamtalk_stdlib),
    _ = application:load(beamtalk_runtime),
    ok.

%%% ============================================================================
%%% all/0 tests
%%% ============================================================================

all_returns_list_test() ->
    setup(),
    Result = beamtalk_package:all(),
    ?assert(is_list(Result)).

all_includes_stdlib_test() ->
    setup(),
    Result = beamtalk_package:all(),
    ?assert(lists:member(<<"stdlib">>, Result)).

all_excludes_non_package_apps_test() ->
    setup(),
    Result = beamtalk_package:all(),
    %% kernel and stdlib (Erlang's) are not Beamtalk packages
    ?assertNot(lists:member(<<"kernel">>, Result)),
    ?assertNot(lists:member(<<"stdlib_erlang">>, Result)).

all_returns_binaries_test() ->
    setup(),
    Result = beamtalk_package:all(),
    lists:foreach(
        fun(Name) -> ?assert(is_binary(Name)) end,
        Result
    ).

%%% ============================================================================
%%% named/1 tests
%%% ============================================================================

named_returns_tagged_package_map_test() ->
    setup(),
    Info = beamtalk_package:named(<<"stdlib">>),
    ?assert(is_map(Info)),
    ?assertEqual('Package', maps:get('$beamtalk_class', Info)),
    ?assertEqual(<<"stdlib">>, maps:get(name, Info)),
    ?assert(is_binary(maps:get(version, Info))),
    ?assert(is_list(maps:get(classes, Info))),
    ?assert(is_list(maps:get(dependencies, Info))),
    ?assert(is_binary(maps:get(source, Info))).

named_accepts_atom_test() ->
    setup(),
    Info = beamtalk_package:named(stdlib),
    ?assertEqual(<<"stdlib">>, maps:get(name, Info)).

named_classes_contains_known_classes_test() ->
    setup(),
    Info = beamtalk_package:named(<<"stdlib">>),
    Classes = maps:get(classes, Info),
    ?assert(lists:member('Object', Classes)),
    ?assert(lists:member('Integer', Classes)),
    ?assert(lists:member('String', Classes)),
    ?assert(lists:member('Array', Classes)).

named_raises_error_for_unknown_package_test() ->
    setup(),
    ?assertError(
        #{error := #beamtalk_error{kind = package_not_found}},
        beamtalk_package:named(<<"nonexistent_package_xyz">>)
    ).

named_raises_error_for_unknown_atom_test() ->
    setup(),
    ?assertError(
        #{error := #beamtalk_error{kind = package_not_found}},
        beamtalk_package:named(nonexistent_package_xyz)
    ).

named_error_includes_hint_with_loaded_packages_test() ->
    setup(),
    try
        beamtalk_package:named(<<"nonexistent_package_xyz">>),
        ?assert(false)
    catch
        error:#{error := #beamtalk_error{hint = Hint}} ->
            ?assertNotEqual(undefined, Hint),
            ?assert(is_binary(Hint)),
            %% Hint should mention loaded packages
            ?assertMatch({match, _}, re:run(Hint, <<"stdlib">>))
    end.

%%% ============================================================================
%%% package_name/1 tests
%%% ============================================================================

package_name_returns_stdlib_for_stdlib_class_test() ->
    setup(),
    %% Object is a stdlib class — should return <<"stdlib">>
    Result = beamtalk_package:package_name('Object'),
    ?assertEqual(<<"stdlib">>, Result).

package_name_returns_nil_for_unknown_class_test() ->
    setup(),
    Result = beamtalk_package:package_name('CompletelyFakeClass_BT1656'),
    ?assertEqual(nil, Result).

package_name_returns_stdlib_for_integer_test() ->
    setup(),
    Result = beamtalk_package:package_name('Integer'),
    ?assertEqual(<<"stdlib">>, Result).

%%% ============================================================================
%%% classes/1 tests
%%% ============================================================================

classes_returns_list_for_stdlib_test() ->
    setup(),
    Result = beamtalk_package:classes(<<"stdlib">>),
    ?assert(is_list(Result)),
    ?assert(length(Result) > 0).

classes_accepts_atom_test() ->
    setup(),
    Result = beamtalk_package:classes(stdlib),
    ?assert(is_list(Result)),
    ?assert(length(Result) > 0).

classes_contains_known_classes_test() ->
    setup(),
    Result = beamtalk_package:classes(<<"stdlib">>),
    ?assert(lists:member('Object', Result)),
    ?assert(lists:member('Array', Result)),
    ?assert(lists:member('Dictionary', Result)).

classes_returns_empty_for_unknown_package_test() ->
    setup(),
    Result = beamtalk_package:classes(<<"nonexistent_package_xyz">>),
    ?assertEqual([], Result).

classes_returns_atoms_test() ->
    setup(),
    Result = beamtalk_package:classes(<<"stdlib">>),
    lists:foreach(
        fun(Name) -> ?assert(is_atom(Name)) end,
        Result
    ).

%%% ============================================================================
%%% dependencies/1 tests
%%% ============================================================================

dependencies_returns_list_test() ->
    setup(),
    Result = beamtalk_package:dependencies(<<"stdlib">>),
    ?assert(is_list(Result)).

dependencies_accepts_atom_test() ->
    setup(),
    Result = beamtalk_package:dependencies(stdlib),
    ?assert(is_list(Result)).

dependencies_returns_empty_for_unknown_package_test() ->
    setup(),
    Result = beamtalk_package:dependencies(<<"nonexistent_package_xyz">>),
    ?assertEqual([], Result).

dependencies_returns_binaries_test() ->
    setup(),
    Result = beamtalk_package:dependencies(<<"stdlib">>),
    lists:foreach(
        fun(Name) -> ?assert(is_binary(Name)) end,
        Result
    ).

%%% ============================================================================
%%% Synthetic-app tests — exercise package metadata branches that need OTP
%%% application env shapes the stdlib package does not provide (BT coverage push).
%%% ============================================================================

%% Load a throwaway OTP application from an in-memory spec so we control its
%% `classes`/`applications`/`vsn` env. Unloads any prior copy first.
load_fake_app(Name, ExtraProps) ->
    _ = application:unload(Name),
    Props = [{description, "fake"}, {vsn, "1.0.0"} | ExtraProps],
    ok = application:load({application, Name, Props}).

%% all/0 must drop an app whose classes carry no package key
%% (package_name_from_classes/1 -> undefined -> filtered out).
all_excludes_app_with_unpackaged_classes_test() ->
    setup(),
    App = bt_fake_nopkg_app,
    load_fake_app(App, [{env, [{classes, [#{name => 'FooNoPkg'}]}]}]),
    try
        Result = beamtalk_package:all(),
        ?assert(is_list(Result)),
        %% A classes list without a `package` key never surfaces as a package.
        ?assertNot(lists:member(undefined, Result))
    after
        _ = application:unload(App)
    end.

%% all/0 surfaces a package declared with a binary `package` value
%% (package_name_from_classes/1 binary clause).
all_includes_binary_package_test() ->
    setup(),
    App = bt_fake_binpkg_app,
    load_fake_app(App, [{env, [{classes, [#{package => <<"binpkg">>, name => 'FooBin'}]}]}]),
    try
        ?assert(lists:member(<<"binpkg">>, beamtalk_package:all()))
    after
        _ = application:unload(App)
    end.

%% dependencies/1 includes deps that are themselves Beamtalk packages.
dependencies_includes_beamtalk_package_deps_test() ->
    setup(),
    Dep = bt_fake_dep_pkg,
    Main = bt_fake_main_pkg,
    load_fake_app(Dep, [{env, [{classes, [#{package => depp, name => 'D'}]}]}]),
    load_fake_app(
        Main,
        [{applications, [Dep]}, {env, [{classes, [#{package => mainp, name => 'M'}]}]}]
    ),
    try
        Deps = beamtalk_package:dependencies(<<"mainp">>),
        ?assert(lists:member(<<"depp">>, Deps))
    after
        _ = application:unload(Main),
        _ = application:unload(Dep)
    end.

%% dependencies/1 drops a dep app whose classes carry no package key.
dependencies_excludes_unpackaged_dep_test() ->
    setup(),
    Dep = bt_fake_dep_nopkg,
    Main = bt_fake_main2_pkg,
    load_fake_app(Dep, [{env, [{classes, [#{name => 'XNoPkg'}]}]}]),
    load_fake_app(
        Main,
        [{applications, [Dep]}, {env, [{classes, [#{package => main2p, name => 'M2'}]}]}]
    ),
    try
        Deps = beamtalk_package:dependencies(<<"main2p">>),
        ?assert(is_list(Deps)),
        ?assertNot(lists:member(undefined, Deps))
    after
        _ = application:unload(Main),
        _ = application:unload(Dep)
    end.

%% classes/1 accepts the legacy `{Mod, Name, Super}` tuple entry shape alongside
%% the map shape (class_entry_name/1 tuple clause).
classes_handles_legacy_tuple_entries_test() ->
    setup(),
    App = bt_fake_legacy_pkg,
    load_fake_app(
        App,
        [
            {env, [
                {classes, [
                    #{package => legacyp, name => 'LegacyA'}, {some_mod, 'LegacyB', 'Object'}
                ]}
            ]}
        ]
    ),
    try
        Classes = beamtalk_package:classes(<<"legacyp">>),
        ?assert(lists:member('LegacyA', Classes)),
        ?assert(lists:member('LegacyB', Classes))
    after
        _ = application:unload(App)
    end.

%% packageNameFor/1 is the Beamtalk FFI shim and must mirror package_name/1.
package_name_for_delegates_to_package_name_test() ->
    setup(),
    ?assertEqual(
        beamtalk_package:package_name('Object'),
        beamtalk_package:packageNameFor('Object')
    ).

%%% ============================================================================
%%% Types-only package tests (BT-2915) — a package with zero classes and a
%%% non-empty `type_aliases` env key must still be discoverable.
%%% ============================================================================

%% all/0 discovers a package that declares only `type_aliases`, no `classes`,
%% deriving the package name from the OTP application's own atom name (app
%% name == package name by construction for `beamtalk build`'s `.app` files).
all_includes_types_only_app_test() ->
    setup(),
    App = bt_fake_types_only_pkg,
    load_fake_app(
        App,
        [{env, [{type_aliases, [#{name => 'RestartStrategy', internal => false}]}]}]
    ),
    try
        ?assert(lists:member(<<"bt_fake_types_only_pkg">>, beamtalk_package:all()))
    after
        _ = application:unload(App)
    end.

%% all/0 excludes an app with neither `classes` nor `type_aliases` (or with
%% both empty) — no discovery signal at all.
all_excludes_app_with_no_classes_or_aliases_test() ->
    setup(),
    App = bt_fake_empty_pkg,
    load_fake_app(App, [{env, [{classes, []}, {type_aliases, []}]}]),
    try
        ?assertNot(lists:member(<<"bt_fake_empty_pkg">>, beamtalk_package:all()))
    after
        _ = application:unload(App)
    end.

%% find_app_for_package/1 resolves a types-only package's name back to its
%% OTP application — exercised by `browse-type-aliases`
%% (`beamtalk_repl_ops_browse:type_aliases_of_package/1`).
find_app_for_package_resolves_types_only_app_test() ->
    setup(),
    App = bt_fake_types_only_pkg2,
    load_fake_app(
        App,
        [{env, [{type_aliases, [#{name => 'JsonValue', internal => false}]}]}]
    ),
    try
        ?assertEqual(
            {ok, App}, beamtalk_package:find_app_for_package(<<"bt_fake_types_only_pkg2">>)
        )
    after
        _ = application:unload(App)
    end.

%% named/1 builds a Package info map for a types-only package — `classes` is
%% empty but the package itself still resolves (no `package_not_found`).
named_resolves_types_only_package_test() ->
    setup(),
    App = bt_fake_types_only_pkg3,
    load_fake_app(
        App,
        [{env, [{type_aliases, [#{name => 'JsonValue', internal => false}]}]}]
    ),
    try
        Info = beamtalk_package:named(<<"bt_fake_types_only_pkg3">>),
        ?assertEqual(<<"bt_fake_types_only_pkg3">>, maps:get(name, Info)),
        ?assertEqual([], maps:get(classes, Info)),
        ?assertEqual(<<"1.0.0">>, maps:get(version, Info)),
        ?assertEqual([], maps:get(dependencies, Info))
    after
        _ = application:unload(App)
    end.

%% package_name_for_app/1 must NOT fall through to type_aliases when classes
%% is non-empty but malformed (no `package` key on any entry) — a non-empty
%% classes env means "not types-only", even if type_aliases is also present.
types_only_not_used_when_classes_env_nonempty_test() ->
    setup(),
    App = bt_fake_malformed_classes_pkg,
    load_fake_app(
        App,
        [
            {env, [
                {classes, [#{name => 'X'}]},
                {type_aliases, [#{name => 'Y', internal => false}]}
            ]}
        ]
    ),
    try
        ?assertNot(lists:member(atom_to_binary(App, utf8), beamtalk_package:all()))
    after
        _ = application:unload(App)
    end.
