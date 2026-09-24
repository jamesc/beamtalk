%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_release_shapes_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_release_shapes (ADR 0125 §2.2/§3.4, BT-3571).

Runs `extract_shapes/2` against the real compiled `release_shapes_root.bt`/
`release_shapes_leaf.bt` fixtures (`test_fixtures/`, compiled by
`compile_fixtures.escript` and copied next to this very test module's own
`.beam`, so `code:which(?MODULE)`'s directory is exactly the directory to
scan — the same directory every other fixture in this suite already lives
in) — a real subclass, with a real declared `shapeVersion: 2` +
`migrateFromV1:` class method, asserting the extractor's flattened field
map (ReleaseShapesLeaf's own `qty` merged with inherited `label`) and
migration table.
""".

-include_lib("eunit/include/eunit.hrl").

fixtures_dir() ->
    filename:dirname(code:which(?MODULE)).

%%====================================================================
%% extract_shapes/2 — the real, compiled two-class fixture
%%====================================================================

extract_shapes_finds_both_fixture_classes_test() ->
    Dir = fixtures_dir(),
    {ok, Shapes} = beamtalk_release_shapes:extract_shapes([], [Dir]),
    ?assert(maps:is_key(<<"ReleaseShapesRoot">>, Shapes)),
    ?assert(maps:is_key(<<"ReleaseShapesLeaf">>, Shapes)).

extract_shapes_root_entry_has_default_version_and_own_field_test() ->
    Dir = fixtures_dir(),
    {ok, Shapes} = beamtalk_release_shapes:extract_shapes([], [Dir]),
    RootEntry = maps:get(<<"ReleaseShapesRoot">>, Shapes),
    ?assertEqual(1, maps:get(version, RootEntry)),
    ?assertEqual(#{<<"label">> => <<"String">>}, maps:get(fields, RootEntry)),
    ?assertEqual(#{}, maps:get(migrations, RootEntry)).

extract_shapes_leaf_entry_has_declared_version_and_flattened_fields_test() ->
    Dir = fixtures_dir(),
    {ok, Shapes} = beamtalk_release_shapes:extract_shapes([], [Dir]),
    LeafEntry = maps:get(<<"ReleaseShapesLeaf">>, Shapes),
    ?assertEqual(2, maps:get(version, LeafEntry)),
    %% Flattened: this class's own `qty` merged with the inherited `label`.
    ?assertEqual(
        #{<<"qty">> => <<"Integer">>, <<"label">> => <<"String">>},
        maps:get(fields, LeafEntry)
    ),
    ?assertEqual(
        #{<<"1">> => <<"migrateFromV1:">>}, maps:get(migrations, LeafEntry)
    ).

%%====================================================================
%% class_shape_entry/1 — direct calls once the fixtures are activated
%%====================================================================

class_shape_entry_returns_undefined_for_an_unregistered_class_test() ->
    ?assertEqual(undefined, beamtalk_release_shapes:class_shape_entry('NoSuchClassAtAll')).

class_shape_entry_matches_the_extract_shapes_projection_test() ->
    Dir = fixtures_dir(),
    {ok, Shapes} = beamtalk_release_shapes:extract_shapes([], [Dir]),
    %% Now that extract_shapes/2 has activated the fixtures, calling
    %% class_shape_entry/1 directly (as a future live `Beamtalk
    %% shapeManifest` would) must report the exact same entry —
    %% the "one shared projection" contract ADR 0125 §2.2 requires.
    ?assertEqual(
        maps:get(<<"ReleaseShapesLeaf">>, Shapes),
        beamtalk_release_shapes:class_shape_entry('ReleaseShapesLeaf')
    ).

%%====================================================================
%% write_shapes_json/4
%%====================================================================

write_shapes_json_writes_a_parseable_manifest_test() ->
    Dir = fixtures_dir(),
    TmpDir = beamtalk_file:'tempDirectory'(),
    OutPath = filename:join(
        TmpDir, "beamtalk_release_shapes_tests_shapes.json"
    ),
    ok = filelib:ensure_dir(OutPath),
    try
        ok = beamtalk_release_shapes:write_shapes_json([], [Dir], OutPath, <<"1.0.0">>),
        {ok, Bin} = file:read_file(OutPath),
        Decoded = json:decode(Bin),
        ?assertEqual(1, maps:get(<<"schema">>, Decoded)),
        ?assertEqual(<<"1.0.0">>, maps:get(<<"release_version">>, Decoded)),
        ShapesJson = maps:get(<<"shapes">>, Decoded),
        LeafJson = maps:get(<<"ReleaseShapesLeaf">>, ShapesJson),
        ?assertEqual(2, maps:get(<<"version">>, LeafJson)),
        ?assertEqual(
            #{<<"qty">> => <<"Integer">>, <<"label">> => <<"String">>},
            maps:get(<<"fields">>, LeafJson)
        ),
        ?assertEqual(
            #{<<"1">> => <<"migrateFromV1:">>}, maps:get(<<"migrations">>, LeafJson)
        )
    after
        file:delete(OutPath)
    end.
