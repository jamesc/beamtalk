%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_release_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
EUnit tests for beamtalk_release (ADR 0125 §1.8/§3.4, BT-3575).

`info/0`'s release-tree-reading half
(`release_dir_and_vsn/0`/`read_provenance/2`) cannot be exercised end to end
here — that needs a real `-boot_var RELEASE_DIR`/systools boot script, which
`crates/beamtalk-cli/tests/cli/cli_release.rs` and
`crates/beamtalk-cli/tests/repl_protocol.rs` cover instead — so this suite
tests `read_provenance/2` directly against a hand-written provenance file
(covering the JSON-decode/atomize path) and `info/0`'s "not a release"
degrade (the actual case for every plain `rebar3 eunit` node, which has no
`-boot_var`). `shape_manifest/0` reuses the real, compiled
`beamtalk_release_shapes` fixtures (`ReleaseShapesRoot`/`ReleaseShapesLeaf`)
`beamtalk_release_shapes_tests` already relies on, and asserts its per-class
entries are `beamtalk_release_shapes:class_shape_entry/1`'s own return value
— the literal "same projection function, live or build" claim BT-3575
requires (ADR 0125 §2.2/§3.4).
""".

-include_lib("eunit/include/eunit.hrl").

fixtures_dir() ->
    filename:dirname(code:which(?MODULE)).

%%====================================================================
%% info/0 — not a release (the ordinary eunit-node case)
%%====================================================================

info_on_non_release_node_is_release_nil_test() ->
    %% A plain `rebar3 eunit` node has no `-boot_var RELEASE_DIR` — the same
    %% degrade a real `run`/`workspace` node hits.
    ?assertEqual(#{release => nil}, beamtalk_release:info()).

release_dir_and_vsn_is_not_a_release_without_boot_var_test() ->
    ?assertEqual(not_a_release, beamtalk_release:release_dir_and_vsn()).

%%====================================================================
%% read_provenance/2 — JSON decode + atomize
%%====================================================================

read_provenance_decodes_and_atomizes_keys_test() ->
    TmpDir = binary_to_list(beamtalk_file:'tempDirectory'()),
    ReleaseRoot = filename:join(TmpDir, "bt_release_tests_provenance"),
    ReleasesDir = filename:join([ReleaseRoot, "releases", "1.4.0"]),
    ok = filelib:ensure_dir(filename:join(ReleasesDir, "beamtalk-provenance.json")),
    Json =
        <<
            "{\"schema\":1,\"release\":\"orders\",\"release_version\":\"1.4.0\","
            "\"required_otp\":{\"min\":28,\"max\":30},"
            "\"apps\":[{\"name\":\"orders\",\"vsn\":\"1.4.0\"}]}"
        >>,
    ok = file:write_file(filename:join(ReleasesDir, "beamtalk-provenance.json"), Json),
    try
        Result = beamtalk_release:read_provenance(ReleaseRoot, "1.4.0"),
        ?assertEqual(1, maps:get(schema, Result)),
        ?assertEqual(<<"orders">>, maps:get(release, Result)),
        ?assertEqual(<<"1.4.0">>, maps:get(release_version, Result)),
        %% Nested maps are atomized too.
        ?assertEqual(#{min => 28, max => 30}, maps:get(required_otp, Result)),
        %% Nested list-of-maps is atomized element-wise.
        ?assertEqual([#{name => <<"orders">>, vsn => <<"1.4.0">>}], maps:get(apps, Result))
    after
        _ = file:del_dir_r(ReleaseRoot)
    end.

read_provenance_missing_file_is_release_nil_test() ->
    TmpDir = binary_to_list(beamtalk_file:'tempDirectory'()),
    Dir = filename:join(TmpDir, "bt_release_tests_missing"),
    ?assertEqual(#{release => nil}, beamtalk_release:read_provenance(Dir, "9.9.9")).

read_provenance_malformed_json_is_release_nil_test() ->
    TmpDir = binary_to_list(beamtalk_file:'tempDirectory'()),
    ReleaseRoot = filename:join(TmpDir, "bt_release_tests_malformed"),
    ReleasesDir = filename:join([ReleaseRoot, "releases", "1.0.0"]),
    ok = filelib:ensure_dir(filename:join(ReleasesDir, "beamtalk-provenance.json")),
    ok = file:write_file(filename:join(ReleasesDir, "beamtalk-provenance.json"), <<"not json">>),
    try
        ?assertEqual(#{release => nil}, beamtalk_release:read_provenance(ReleaseRoot, "1.0.0"))
    after
        _ = file:del_dir_r(ReleaseRoot)
    end.

%%====================================================================
%% atomize_json_map/1
%%====================================================================

atomize_json_map_converts_binary_keys_recursively_test() ->
    Decoded = #{
        <<"a">> => 1,
        <<"b">> => #{<<"c">> => <<"d">>},
        <<"e">> => [#{<<"f">> => true}, 2]
    },
    ?assertEqual(
        #{a => 1, b => #{c => <<"d">>}, e => [#{f => true}, 2]},
        beamtalk_release:atomize_json_map(Decoded)
    ).

%%====================================================================
%% shape_manifest/0 — same projection as beamtalk_release_shapes, live
%%====================================================================

shape_manifest_matches_the_shared_projection_for_registered_fixture_classes_test() ->
    Dir = fixtures_dir(),
    %% A side effect of extract_shapes/2: starts beamtalk_stdlib and
    %% registers ReleaseShapesRoot/ReleaseShapesLeaf in the live class
    %% registry (see beamtalk_release_shapes_tests, which asserts these
    %% same fixtures' shapes directly).
    {ok, _BuildTimeShapes} = beamtalk_release_shapes:extract_shapes([], [Dir]),
    Manifest = beamtalk_release:shape_manifest(),
    ?assertEqual(
        beamtalk_release_shapes:class_shape_entry('ReleaseShapesRoot'),
        maps:get('ReleaseShapesRoot', Manifest)
    ),
    ?assertEqual(
        beamtalk_release_shapes:class_shape_entry('ReleaseShapesLeaf'),
        maps:get('ReleaseShapesLeaf', Manifest)
    ).

shape_manifest_excludes_stdlib_classes_test() ->
    Dir = fixtures_dir(),
    {ok, _} = beamtalk_release_shapes:extract_shapes([], [Dir]),
    Manifest = beamtalk_release:shape_manifest(),
    StdlibClassNames = [
        Name
     || {Name, ModuleName, _Pid} <- beamtalk_class_registry:live_class_entries(),
        not beamtalk_module_activation:is_release_class_module(ModuleName)
    ],
    ?assert(StdlibClassNames =/= []),
    ?assertEqual(
        [],
        [Name || Name <- maps:keys(Manifest), lists:member(Name, StdlibClassNames)]
    ).

shapeManifest_is_an_alias_for_shape_manifest_test() ->
    Dir = fixtures_dir(),
    {ok, _} = beamtalk_release_shapes:extract_shapes([], [Dir]),
    ?assertEqual(beamtalk_release:shape_manifest(), beamtalk_release:shapeManifest()).
