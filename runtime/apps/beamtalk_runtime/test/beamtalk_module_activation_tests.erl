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

activate_module_with_custom_log_domain_test() ->
    %% Non-existent module, but with custom log_domain — should return error
    %% without crashing (verifies opts plumbing for log_domain).
    Result = beamtalk_module_activation:activate_module(
        nonexistent_module_xyz_456, #{log_domain => [beamtalk, test]}
    ),
    ?assertMatch({error, _}, Result).

activate_module_on_activate_callback_error_test() ->
    %% Verify that a crashing on_activate callback does not propagate —
    %% activate_module/2 should still return ok.
    %% We use the `lists` module as a stand-in: it is already loaded,
    %% has no register_class/0 (skipped), and triggers the callback path.
    CrashingCallback = fun({_Mod, _Path}) -> error(callback_boom) end,
    Result = beamtalk_module_activation:activate_module(
        lists, #{on_activate => CrashingCallback}
    ),
    ?assertEqual(ok, Result).

activate_module_on_activate_callback_invoked_test() ->
    %% Verify the on_activate callback is called with {Module, SourcePath}.
    Self = self(),
    Callback = fun({Mod, Path}) -> Self ! {activated, Mod, Path} end,
    ok = beamtalk_module_activation:activate_module(
        lists, #{on_activate => Callback}
    ),
    receive
        {activated, lists, _Path} -> ok
    after 1000 ->
        error(on_activate_callback_not_invoked)
    end.

activate_module_register_class_crash_test() ->
    %% Compile a dynamic module whose register_class/0 crashes.
    %% activate_module should return {error, {error, register_class_boom}}.
    Forms = [
        {attribute, 1, module, bt_test_crashing_register},
        {attribute, 2, export, [{register_class, 0}]},
        {function, 3, register_class, 0, [
            {clause, 3, [], [], [
                {call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, error}}, [
                    {atom, 3, register_class_boom}
                ]}
            ]}
        ]}
    ],
    {ok, Mod, Bin} = compile:forms(Forms),
    {module, Mod} = code:load_binary(Mod, "bt_test_crashing_register.beam", Bin),
    try
        Result = beamtalk_module_activation:activate_module(Mod),
        ?assertMatch({error, {error, register_class_boom}}, Result)
    after
        code:purge(bt_test_crashing_register),
        code:delete(bt_test_crashing_register)
    end.

%%% ============================================================================
%%% topo_sort/1 — circular dependency handling
%%% ============================================================================

topo_sort_circular_dependency_test() ->
    %% A depends on B and B depends on A — both are in ClassSet,
    %% neither superclass is satisfied. topo_sort should emit a warning
    %% and append the remaining entries rather than looping forever.
    Entries = [{mod_a, 'A', 'B'}, {mod_b, 'B', 'A'}],
    Result = beamtalk_module_activation:topo_sort(Entries),
    %% Both entries must still appear in the result
    ?assertEqual(2, length(Result)),
    ResultNames = [Name || {_, Name, _} <- Result],
    ?assert(lists:member('A', ResultNames)),
    ?assert(lists:member('B', ResultNames)).

topo_sort_partial_circular_test() ->
    %% A → Object (external), B → C, C → B (circular among B and C).
    %% A should be emitted first; B and C appended as unresolvable.
    Entries = [
        {mod_c, 'C', 'B'},
        {mod_b, 'B', 'C'},
        {mod_a, 'A', 'Object'}
    ],
    Result = beamtalk_module_activation:topo_sort(Entries),
    ?assertEqual(3, length(Result)),
    %% A must be first since it has an external superclass
    [{_, FirstName, _} | _] = Result,
    ?assertEqual('A', FirstName).

%%% ============================================================================
%%% find_bt_modules_in_dir/1 — with actual files
%%% ============================================================================

find_modules_with_beam_files_test() ->
    TmpDir = create_temp_dir(),
    try
        %% Create fake .beam files
        ok = file:write_file(filename:join(TmpDir, "bt@pkg@MyClass.beam"), <<>>),
        ok = file:write_file(filename:join(TmpDir, "bt@pkg@Other.beam"), <<>>),
        %% Non-bt file should be excluded
        ok = file:write_file(filename:join(TmpDir, "some_erlang_mod.beam"), <<>>),
        %% stdlib file should be excluded
        ok = file:write_file(filename:join(TmpDir, "bt@stdlib@Array.beam"), <<>>),
        %% Non-beam file should be excluded
        ok = file:write_file(filename:join(TmpDir, "bt@pkg@Readme.txt"), <<>>),

        Result = beamtalk_module_activation:find_bt_modules_in_dir(TmpDir),
        ?assertEqual(2, length(Result)),
        ?assert(lists:member('bt@pkg@MyClass', Result)),
        ?assert(lists:member('bt@pkg@Other', Result))
    after
        remove_temp_dir(TmpDir)
    end.

find_modules_skips_invalid_names_test() ->
    TmpDir = create_temp_dir(),
    try
        %% A beam file with spaces in the name — invalid module name
        ok = file:write_file(filename:join(TmpDir, "bt@my class.beam"), <<>>),
        %% A valid one
        ok = file:write_file(filename:join(TmpDir, "bt@pkg@Valid.beam"), <<>>),

        Result = beamtalk_module_activation:find_bt_modules_in_dir(TmpDir),
        ?assertEqual(1, length(Result)),
        ?assertEqual(['bt@pkg@Valid'], Result)
    after
        remove_temp_dir(TmpDir)
    end.

%%% ============================================================================
%%% activate_ebin/2 tests
%%% ============================================================================

activate_ebin_nonexistent_dir_test() ->
    %% Non-existent directory should return {ok, []} with no crash
    NonExistent = filename:join(
        get_tmp_base(),
        "nonexistent_ebin_" ++
            integer_to_list(erlang:unique_integer([positive]))
    ),
    ?assertEqual({ok, []}, beamtalk_module_activation:activate_ebin(NonExistent)).

activate_ebin_empty_dir_test() ->
    TmpDir = create_temp_dir(),
    try
        {ok, Errors} = beamtalk_module_activation:activate_ebin(TmpDir),
        ?assertEqual([], Errors),
        %% activate_ebin adds TmpDir to code path — clean up
        _ = code:del_path(TmpDir)
    after
        remove_temp_dir(TmpDir)
    end.

%%% ============================================================================
%%% extract_class_names/1 — loaded module tests
%%% ============================================================================

extract_class_names_no_attribute_test() ->
    %% `lists` is a standard Erlang module with no beamtalk_class attribute
    ?assertEqual([], beamtalk_module_activation:extract_class_names(lists)).

%%% ============================================================================
%%% extract_source_path/1 — loaded module tests
%%% ============================================================================

extract_source_path_no_attribute_test() ->
    %% `lists` has no beamtalk_source attribute
    ?assertEqual(undefined, beamtalk_module_activation:extract_source_path(lists)).

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
