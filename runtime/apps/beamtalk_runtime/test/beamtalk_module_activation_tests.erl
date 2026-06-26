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
%%% activate_modules/2 tests (ADR 0099 §4 — escript-friendly list activation)
%%% ============================================================================

activate_modules_empty_test() ->
    %% No modules → no work, no errors.
    ?assertEqual({ok, []}, beamtalk_module_activation:activate_modules([], #{})).

activate_modules_unloadable_reports_error_test() ->
    %% An unknown/unloadable module is sorted into the no-class bucket and then
    %% reported as an activation error rather than crashing the whole batch.
    Bogus = 'bt@nonexistent@phantom_xyz',
    {ok, Errors} = beamtalk_module_activation:activate_modules([Bogus], #{}),
    ?assertMatch([{Bogus, _Reason}], Errors).

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
%%% is_valid_module_name/1 — digit characters
%%% ============================================================================

valid_module_name_with_digits_test() ->
    %% Exercises the digit clause of is_valid_module_char/1.
    ?assert(beamtalk_module_activation:is_valid_module_name("bt@pkg2@Class9")).

%%% ============================================================================
%%% sort_modules_by_dependency/2 — real beam attribute scan
%%% ============================================================================

%% Compiling beam files with `beamtalk_class` attributes and sorting them
%% exercises extract_class_info_from_beam/2 (beam_lib scan) and the
%% topo-sort-by-superclass path of sort_modules_by_dependency/2.
sort_modules_orders_subclass_after_superclass_test() ->
    TmpDir = create_temp_dir(),
    try
        ParentMod = write_class_beam(TmpDir, "bt@srt@Parent", 'SrtParent', 'Object'),
        ChildMod = write_class_beam(TmpDir, "bt@srt@Child", 'SrtChild', 'SrtParent'),
        %% Pass child before parent — sort must reorder so parent precedes child.
        Sorted = beamtalk_module_activation:sort_modules_by_dependency(
            TmpDir, [ChildMod, ParentMod]
        ),
        ?assertEqual([ParentMod, ChildMod], Sorted)
    after
        remove_temp_dir(TmpDir)
    end.

%% A beam file with no beamtalk_class attribute is placed first (WithoutClass
%% branch of sort_modules_by_dependency/2 + the error branch of
%% extract_class_info_from_beam/2).
sort_modules_no_attribute_placed_first_test() ->
    TmpDir = create_temp_dir(),
    try
        PlainMod = write_plain_beam(TmpDir, "bt@srt@Plain"),
        ClassMod = write_class_beam(TmpDir, "bt@srt@Klass", 'SrtKlass', 'Object'),
        Sorted = beamtalk_module_activation:sort_modules_by_dependency(
            TmpDir, [ClassMod, PlainMod]
        ),
        %% Modules without a class attribute come first.
        ?assertEqual([PlainMod, ClassMod], Sorted)
    after
        remove_temp_dir(TmpDir)
    end.

%%% ============================================================================
%%% activate_ebin/2 — directory with real beam modules
%%% ============================================================================

%% A populated ebin dir drives the full activate_ebin/2 pipeline: code path,
%% load_app_from_ebin, find_bt_modules_in_dir, sort, and activate_module per
%% module (covering the filtermap success branch where activate_module -> ok).
activate_ebin_with_modules_test() ->
    TmpDir = create_temp_dir(),
    try
        %% Module with register_class/0 returning ok — activates cleanly.
        Mod = write_registering_beam(TmpDir, "bt@aeb@Ok"),
        {ok, Errors} = beamtalk_module_activation:activate_ebin(TmpDir),
        ?assertEqual([], Errors),
        ?assert(erlang:function_exported(Mod, register_class, 0))
    after
        _ = code:del_path(TmpDir),
        purge_mod('bt@aeb@Ok'),
        remove_temp_dir(TmpDir)
    end.

%% A module whose register_class/0 crashes surfaces as an error pair in the
%% activate_ebin/2 result (covers the {error, Reason} -> {true, ...} branch
%% of the filtermap in activate_ebin/2).
activate_ebin_collects_module_errors_test() ->
    TmpDir = create_temp_dir(),
    try
        Mod = write_crashing_register_beam(TmpDir, "bt@aeb@Crash"),
        {ok, Errors} = beamtalk_module_activation:activate_ebin(TmpDir),
        ?assertMatch([{Mod, {error, _}}], Errors)
    after
        _ = code:del_path(TmpDir),
        purge_mod('bt@aeb@Crash'),
        remove_temp_dir(TmpDir)
    end.

%%% ============================================================================
%%% activate_dependencies/1,2
%%% ============================================================================

%% No _build/deps directory and no native ebin — returns [] without crashing.
activate_dependencies_no_deps_dir_test() ->
    TmpDir = create_temp_dir(),
    try
        ?assertEqual([], beamtalk_module_activation:activate_dependencies(TmpDir))
    after
        remove_temp_dir(TmpDir)
    end.

%% A project layout with a dependency ebin containing a registering module
%% drives the deps-scan + activate_ebin loop, plus the native/hex ebin path
%% additions. Returns [] errors.
activate_dependencies_with_dep_test() ->
    ProjectDir = create_temp_dir(),
    try
        DepEbin = filename:join([ProjectDir, "_build", "deps", "mydep", "ebin"]),
        ok = filelib:ensure_dir(filename:join(DepEbin, "dummy")),
        _ = write_registering_beam(DepEbin, "bt@dep@Thing"),
        %% Also create the native + hex lib dirs to exercise those code-path
        %% branches (empty hex lib so the inner loop runs over no deps).
        NativeEbin = filename:join([ProjectDir, "_build", "dev", "native", "ebin"]),
        ok = filelib:ensure_dir(filename:join(NativeEbin, "dummy")),
        HexLib = filename:join([ProjectDir, "_build", "dev", "native", "default", "lib"]),
        ok = filelib:ensure_dir(filename:join(HexLib, "dummy")),
        Errors = beamtalk_module_activation:activate_dependencies(ProjectDir, #{}),
        ?assertEqual([], Errors)
    after
        _ = code:del_path(filename:join([ProjectDir, "_build", "deps", "mydep", "ebin"])),
        _ = code:del_path(filename:join([ProjectDir, "_build", "dev", "native", "ebin"])),
        purge_mod('bt@dep@Thing'),
        remove_temp_dir_recursive(ProjectDir)
    end.

%% A dependency ebin holding a hex lib dir with its own nested ebin exercises
%% the inner add_pathz branch of the rebar3 hex-dep loop.
activate_dependencies_hex_lib_ebin_test() ->
    ProjectDir = create_temp_dir(),
    try
        HexEbin = filename:join([
            ProjectDir, "_build", "dev", "native", "default", "lib", "cowboy", "ebin"
        ]),
        ok = filelib:ensure_dir(filename:join(HexEbin, "dummy")),
        Errors = beamtalk_module_activation:activate_dependencies(ProjectDir, #{}),
        ?assertEqual([], Errors),
        %% The hex ebin should have been added to the code path.
        ?assert(lists:member(HexEbin, code:get_path()))
    after
        _ = code:del_path(
            filename:join([
                ProjectDir, "_build", "dev", "native", "default", "lib", "cowboy", "ebin"
            ])
        ),
        remove_temp_dir_recursive(ProjectDir)
    end.

%%% ============================================================================
%%% load_app_from_ebin/1 — invalid .app basename
%%% ============================================================================

%% A .app file whose basename is not a valid module name is skipped (the
%% `false -> false` branch of load_app_from_ebin/1's filtermap).
load_app_invalid_basename_skipped_test() ->
    TmpDir = create_temp_dir(),
    try
        %% Basename "bad name" contains a space — invalid module name.
        BadApp = filename:join(TmpDir, "bad name.app"),
        ok = file:write_file(BadApp, <<"{application, ignored, []}.">>),
        ?assertEqual({ok, []}, beamtalk_module_activation:load_app_from_ebin(TmpDir))
    after
        remove_temp_dir(TmpDir)
    end.

%%% ============================================================================
%%% extract_source_path/1 — module with a beamtalk_source attribute
%%% ============================================================================

%% A loaded module carrying `beamtalk_source = ["path.bt"]` returns the path
%% (covers the `[Path] when is_list(Path)` clause of extract_source_path/1).
extract_source_path_with_attribute_test() ->
    Mod = bt_test_source_attr_mod,
    Forms = [
        {attribute, 1, module, Mod},
        {attribute, 2, beamtalk_source, ["src/Foo.bt"]},
        {attribute, 3, export, []}
    ],
    {ok, Mod, Bin} = compile:forms(Forms, [return_errors]),
    {module, Mod} = code:load_binary(Mod, "bt_test_source_attr_mod.beam", Bin),
    try
        ?assertEqual("src/Foo.bt", beamtalk_module_activation:extract_source_path(Mod))
    after
        purge_mod(Mod)
    end.

%%% ============================================================================
%%% extract_class_names/1 — module with a beamtalk_class attribute
%%% ============================================================================

%% A loaded module with `beamtalk_class = [{Name, Super}]` yields the class
%% name list.
extract_class_names_with_attribute_test() ->
    Mod = bt_test_class_attr_mod,
    Forms = [
        {attribute, 1, module, Mod},
        {attribute, 2, beamtalk_class, [{'MyClass', 'Object'}]},
        {attribute, 3, export, []}
    ],
    {ok, Mod, Bin} = compile:forms(Forms, [return_errors]),
    {module, Mod} = code:load_binary(Mod, "bt_test_class_attr_mod.beam", Bin),
    try
        ?assertEqual(['MyClass'], beamtalk_module_activation:extract_class_names(Mod))
    after
        purge_mod(Mod)
    end.

%%% ============================================================================
%%% sort_modules_by_dependency/2 — beam with multiple class entries
%%% ============================================================================

%% A beam whose beamtalk_class attribute lists more than one class exercises
%% the `[{ClassName, Superclass} | _]` multi-entry clause of
%% extract_class_info_from_beam/2 (only the first pair is used for sorting).
sort_modules_multi_class_attribute_test() ->
    TmpDir = create_temp_dir(),
    try
        ModName = "bt@srt@Multi",
        Mod = list_to_atom(ModName),
        Forms = [
            {attribute, 1, module, Mod},
            {attribute, 2, beamtalk_class, [{'SrtFirst', 'Object'}, {'SrtSecond', 'SrtFirst'}]},
            {attribute, 3, export, []}
        ],
        {ok, Mod, Bin} = compile:forms(Forms, [return_errors]),
        ok = file:write_file(filename:join(TmpDir, ModName ++ ".beam"), Bin),
        Sorted = beamtalk_module_activation:sort_modules_by_dependency(TmpDir, [Mod]),
        ?assertEqual([Mod], Sorted)
    after
        remove_temp_dir(TmpDir)
    end.

%%% ============================================================================
%%% activate_module/2 — register_class/0 unexpected return
%%% ============================================================================

%% register_class/0 returning a non-ok value is logged but still treated as
%% success (covers the `Other ->` branch of try_register_class/2).
activate_module_register_class_unexpected_return_test() ->
    Mod = bt_test_unexpected_register,
    Forms = [
        {attribute, 1, module, Mod},
        {attribute, 2, export, [{register_class, 0}]},
        {function, 3, register_class, 0, [
            {clause, 3, [], [], [{atom, 3, surprise}]}
        ]}
    ],
    {ok, Mod, Bin} = compile:forms(Forms, [return_errors]),
    {module, Mod} = code:load_binary(Mod, "bt_test_unexpected_register.beam", Bin),
    try
        ?assertEqual(ok, beamtalk_module_activation:activate_module(Mod))
    after
        purge_mod(Mod)
    end.

%%% ============================================================================
%%% load_app_from_ebin/1 — malformed .app surfaces an error
%%% ============================================================================

%% A syntactically valid but unloadable .app (wrong term shape) makes
%% application:load/1 fail, surfacing a {AppName, Reason} error pair (covers
%% the {error, Reason} branch of load_single_app/1).
load_app_malformed_surfaces_error_test() ->
    TmpDir = create_temp_dir(),
    try
        %% Valid module-name basename, but the file content is not a proper
        %% {application, Name, Props} term — application:load/1 returns error.
        AppName = "beamtalk_bad_app_xyz",
        AppFile = filename:join(TmpDir, AppName ++ ".app"),
        ok = file:write_file(AppFile, <<"{not_an_application, oops}.">>),
        _ = code:add_pathz(TmpDir),
        _ = application:unload(list_to_atom(AppName)),
        {ok, Errors} = beamtalk_module_activation:load_app_from_ebin(TmpDir),
        ?assertMatch([{_, _}], Errors)
    after
        _ = code:del_path(TmpDir),
        remove_temp_dir(TmpDir)
    end.

%%% ============================================================================
%%% Helpers
%%% ============================================================================

%% Compile and write a beam carrying a beamtalk_class attribute into Dir.
%% Returns the module atom.
write_class_beam(Dir, ModName, ClassName, Superclass) ->
    Mod = list_to_atom(ModName),
    Forms = [
        {attribute, 1, module, Mod},
        {attribute, 2, beamtalk_class, [{ClassName, Superclass}]},
        {attribute, 3, export, []}
    ],
    {ok, Mod, Bin} = compile:forms(Forms, [return_errors]),
    ok = file:write_file(filename:join(Dir, ModName ++ ".beam"), Bin),
    Mod.

%% Compile and write a plain beam (no beamtalk_class attribute) into Dir.
write_plain_beam(Dir, ModName) ->
    Mod = list_to_atom(ModName),
    Forms = [
        {attribute, 1, module, Mod},
        {attribute, 2, export, []}
    ],
    {ok, Mod, Bin} = compile:forms(Forms, [return_errors]),
    ok = file:write_file(filename:join(Dir, ModName ++ ".beam"), Bin),
    Mod.

%% Compile and write a beam exporting register_class/0 -> ok into Dir.
write_registering_beam(Dir, ModName) ->
    Mod = list_to_atom(ModName),
    Forms = [
        {attribute, 1, module, Mod},
        {attribute, 2, export, [{register_class, 0}]},
        {function, 3, register_class, 0, [{clause, 3, [], [], [{atom, 3, ok}]}]}
    ],
    {ok, Mod, Bin} = compile:forms(Forms, [return_errors]),
    ok = file:write_file(filename:join(Dir, ModName ++ ".beam"), Bin),
    Mod.

%% Compile and write a beam whose register_class/0 crashes into Dir.
write_crashing_register_beam(Dir, ModName) ->
    Mod = list_to_atom(ModName),
    Forms = [
        {attribute, 1, module, Mod},
        {attribute, 2, export, [{register_class, 0}]},
        {function, 3, register_class, 0, [
            {clause, 3, [], [], [
                {call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, error}}, [
                    {atom, 3, boom}
                ]}
            ]}
        ]}
    ],
    {ok, Mod, Bin} = compile:forms(Forms, [return_errors]),
    ok = file:write_file(filename:join(Dir, ModName ++ ".beam"), Bin),
    Mod.

purge_mod(ModName) when is_list(ModName) ->
    purge_mod(list_to_atom(ModName));
purge_mod(Mod) when is_atom(Mod) ->
    _ = code:purge(Mod),
    _ = code:delete(Mod),
    _ = code:purge(Mod),
    ok.

%% Recursively remove a temp directory tree (for nested _build layouts).
remove_temp_dir_recursive(Dir) ->
    case file:list_dir(Dir) of
        {ok, Entries} ->
            lists:foreach(
                fun(E) ->
                    Path = filename:join(Dir, E),
                    case filelib:is_dir(Path) of
                        true -> remove_temp_dir_recursive(Path);
                        false -> file:delete(Path)
                    end
                end,
                Entries
            ),
            file:del_dir(Dir);
        _ ->
            ok
    end.

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
