%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Object System Context

-module(beamtalk_module_activation_tests).

-moduledoc """
EUnit tests for beamtalk_module_activation module.

Tests the unified module discovery, topological sorting, activation,
and OTP application loading used by stdlib, workspace bootstrap,
and load-project.
""".

-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% topo_sort/1 tests — tuple format
%%% ============================================================================

topo_sort_empty_test() ->
    ?assertEqual([], beamtalk_module_activation:topo_sort([])).

topo_sort_single_entry_test() ->
    Entries = [{mod_a, 'A', 'Object'}],
    ?assertEqual(Entries, beamtalk_module_activation:topo_sort(Entries)).

topo_sort_already_ordered_test() ->
    Entries = [{mod_a, 'A', 'Object'}, {mod_b, 'B', 'A'}],
    Result = beamtalk_module_activation:topo_sort(Entries),
    ?assertEqual(Entries, Result).

topo_sort_reverse_order_test() ->
    Entries = [{mod_b, 'B', 'A'}, {mod_a, 'A', 'Object'}],
    Result = beamtalk_module_activation:topo_sort(Entries),
    %% A must come before B
    ?assertEqual([{mod_a, 'A', 'Object'}, {mod_b, 'B', 'A'}], Result).

topo_sort_external_superclass_test() ->
    %% Both classes have external superclasses — order preserved
    Entries = [{mod_a, 'A', 'Object'}, {mod_b, 'B', 'Value'}],
    Result = beamtalk_module_activation:topo_sort(Entries),
    ?assertEqual(Entries, Result).

topo_sort_diamond_dependency_test() ->
    %% Object → A, Object → B, A → C, B → C
    Entries = [
        {mod_c, 'C', 'A'},
        {mod_b, 'B', 'Object'},
        {mod_a, 'A', 'Object'}
    ],
    Result = beamtalk_module_activation:topo_sort(Entries),
    %% A and B before C; both have external superclass so emit first
    Names = [Name || {_, Name, _} <- Result],
    APos = index_of('A', Names),
    CPos = index_of('C', Names),
    ?assertNotEqual(not_found, APos),
    ?assertNotEqual(not_found, CPos),
    ?assert(APos < CPos).

topo_sort_deep_chain_test() ->
    Entries = [
        {mod_d, 'D', 'C'},
        {mod_c, 'C', 'B'},
        {mod_b, 'B', 'A'},
        {mod_a, 'A', 'Object'}
    ],
    Result = beamtalk_module_activation:topo_sort(Entries),
    ?assertEqual(
        [
            {mod_a, 'A', 'Object'},
            {mod_b, 'B', 'A'},
            {mod_c, 'C', 'B'},
            {mod_d, 'D', 'C'}
        ],
        Result
    ).

%%% ============================================================================
%%% topo_sort/1 tests — map format (ADR 0070 Phase 4)
%%% ============================================================================

topo_sort_map_format_test() ->
    Entries = [
        #{name => 'B', module => mod_b, parent => 'A', package => stdlib},
        #{name => 'A', module => mod_a, parent => 'Object', package => stdlib}
    ],
    Result = beamtalk_module_activation:topo_sort(Entries),
    [First | _] = Result,
    ?assertEqual('A', maps:get(name, First)).

%%% ============================================================================
%%% is_valid_module_name/1 tests
%%% ============================================================================

valid_module_name_test() ->
    ?assert(beamtalk_module_activation:is_valid_module_name("bt@pkg@MyClass")).

valid_module_name_with_underscore_test() ->
    ?assert(beamtalk_module_activation:is_valid_module_name("bt@my_pkg@My_Class")).

invalid_module_name_empty_test() ->
    ?assertNot(beamtalk_module_activation:is_valid_module_name("")).

invalid_module_name_spaces_test() ->
    ?assertNot(beamtalk_module_activation:is_valid_module_name("bt@my class")).

invalid_module_name_dots_test() ->
    ?assertNot(beamtalk_module_activation:is_valid_module_name("bt.pkg.Class")).

%%% ============================================================================
%%% find_bt_modules_in_dir/1 tests
%%% ============================================================================

find_modules_missing_dir_test() ->
    ?assertEqual([], beamtalk_module_activation:find_bt_modules_in_dir("/nonexistent/path")).

%%% ============================================================================
%%% sort_modules_by_dependency/2 tests
%%% ============================================================================

sort_modules_empty_test() ->
    ?assertEqual([], beamtalk_module_activation:sort_modules_by_dependency("/tmp", [])).

%%% ============================================================================
%%% load_app_from_ebin/1 tests
%%% ============================================================================

load_app_missing_dir_test() ->
    %% Should not crash on missing directory (platform-agnostic path)
    NonExistent = filename:join(get_tmp_base(), "nonexistent_ebin_dir_xyz"),
    ?assertEqual({ok, []}, beamtalk_module_activation:load_app_from_ebin(NonExistent)).

load_app_from_ebin_loads_metadata_test() ->
    %% Create a temp directory with a minimal .app file
    TmpDir = create_temp_dir(),
    try
        AppFile = filename:join(TmpDir, "beamtalk_test_fake.app"),
        AppContent =
            "{application, beamtalk_test_fake, [{description, \"test\"}, {vsn, \"0.1.0\"}, {modules, []}, {env, [{classes, [#{name => 'FakeClass', module => 'bt@fake@FakeClass', parent => 'Object', package => fake}]}]}]}.",
        ok = file:write_file(AppFile, AppContent),
        _ = code:add_pathz(TmpDir),

        %% Ensure not already loaded
        _ = application:unload(beamtalk_test_fake),

        %% Load it
        {ok, []} = beamtalk_module_activation:load_app_from_ebin(TmpDir),

        %% Verify the app metadata is now visible
        {ok, Classes} = application:get_env(beamtalk_test_fake, classes),
        ?assert(is_list(Classes)),
        ?assertEqual(1, length(Classes)),

        %% Clean up
        _ = application:unload(beamtalk_test_fake),
        _ = code:del_path(TmpDir)
    after
        remove_temp_dir(TmpDir)
    end.

%%% ============================================================================
%%% extract_source_path/1 tests
%%% ============================================================================

extract_source_path_missing_module_test() ->
    ?assertEqual(undefined, beamtalk_module_activation:extract_source_path(nonexistent_module_xyz)).

%%% ============================================================================
%%% extract_class_names/1 tests
%%% ============================================================================

extract_class_names_missing_module_test() ->
    ?assertEqual([], beamtalk_module_activation:extract_class_names(nonexistent_module_xyz)).

%%% ============================================================================
%%% activate_module/2 tests
%%% ============================================================================

activate_module_nonexistent_test() ->
    Result = beamtalk_module_activation:activate_module(nonexistent_module_xyz_123),
    ?assertMatch({error, _}, Result).

%%% ============================================================================
%%% Helpers
%%% ============================================================================

index_of(Elem, List) ->
    index_of(Elem, List, 1).
index_of(_Elem, [], _N) ->
    not_found;
index_of(Elem, [Elem | _], N) ->
    N;
index_of(Elem, [_ | Rest], N) ->
    index_of(Elem, Rest, N + 1).

create_temp_dir() ->
    Base = filename:join([
        get_tmp_base(),
        "bt_activation_test_" ++ integer_to_list(erlang:unique_integer([positive]))
    ]),
    ok = filelib:ensure_dir(filename:join(Base, "dummy")),
    Base.

-doc "Platform-agnostic temp directory.".
get_tmp_base() ->
    case os:getenv("TMPDIR") of
        false ->
            case os:getenv("TEMP") of
                false -> "/tmp";
                Dir -> Dir
            end;
        Dir ->
            Dir
    end.

remove_temp_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foreach(fun(F) -> file:delete(filename:join(Dir, F)) end, Files),
            file:del_dir(Dir);
        _ ->
            ok
    end.
