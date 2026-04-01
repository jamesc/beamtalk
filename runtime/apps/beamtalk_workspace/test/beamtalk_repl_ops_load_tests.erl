%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_repl_ops_load module.
%%%
%%% **DDD Context:** REPL Session Context
%%%
%%% Tests the load-project helpers: find_bt_files, extract_bt_class_info,
%%% sort_bt_files_by_deps, and structured_file_errors. These are pure
%%% filesystem/parsing functions that do not require a running REPL workspace.

-module(beamtalk_repl_ops_load_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Helpers
%%====================================================================

%% Write a file to a temp directory for testing.
write_temp_file(Dir, Name, Content) ->
    Path = filename:join(Dir, Name),
    ok = file:write_file(Path, Content),
    Path.

%% Create a fresh temp directory for a test using a relative path.
%% Relative directories work cross-platform and avoid POSIX /tmp assumptions.
make_temp_dir() ->
    Base = "bt_ops_load_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = file:make_dir(Base),
    Base.

%% Delete a temp directory and all its contents recursively.
rm_temp_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Entries} ->
            lists:foreach(
                fun(F) ->
                    Path = filename:join(Dir, F),
                    case filelib:is_dir(Path) of
                        true -> rm_temp_dir(Path);
                        false -> file:delete(Path)
                    end
                end,
                Entries
            ),
            file:del_dir(Dir);
        {error, _} ->
            ok
    end.

%%====================================================================
%% find_bt_files/1
%%====================================================================

find_bt_files_empty_dir_test() ->
    Dir = make_temp_dir(),
    try
        ?assertEqual([], beamtalk_repl_ops_load:find_bt_files(Dir))
    after
        rm_temp_dir(Dir)
    end.

find_bt_files_nonexistent_dir_test() ->
    Missing = "missing_bt_ops_load_test_dir_" ++ integer_to_list(erlang:unique_integer([positive])),
    ?assertEqual([], beamtalk_repl_ops_load:find_bt_files(Missing)).

find_bt_files_finds_bt_files_test() ->
    Dir = make_temp_dir(),
    try
        write_temp_file(Dir, "a.bt", <<"Object subclass: A">>),
        write_temp_file(Dir, "b.bt", <<"A subclass: B">>),
        write_temp_file(Dir, "readme.md", <<"docs">>),
        Found = beamtalk_repl_ops_load:find_bt_files(Dir),
        ?assertEqual(2, length(Found)),
        ?assert(lists:any(fun(F) -> lists:suffix("a.bt", F) end, Found)),
        ?assert(lists:any(fun(F) -> lists:suffix("b.bt", F) end, Found))
    after
        rm_temp_dir(Dir)
    end.

%%====================================================================
%% extract_bt_class_info/1
%%====================================================================

extract_bt_class_info_simple_test() ->
    Dir = make_temp_dir(),
    try
        Path = write_temp_file(
            Dir, "counter.bt", <<"Object subclass: Counter\n  state: count = 0\n">>
        ),
        ?assertEqual(
            {<<"Counter">>, <<"Object">>}, beamtalk_repl_ops_load:extract_bt_class_info(Path)
        )
    after
        rm_temp_dir(Dir)
    end.

extract_bt_class_info_subclass_test() ->
    Dir = make_temp_dir(),
    try
        Path = write_temp_file(Dir, "special.bt", <<"Counter subclass: SpecialCounter\n">>),
        ?assertEqual(
            {<<"SpecialCounter">>, <<"Counter">>},
            beamtalk_repl_ops_load:extract_bt_class_info(Path)
        )
    after
        rm_temp_dir(Dir)
    end.

extract_bt_class_info_no_class_test() ->
    Dir = make_temp_dir(),
    try
        Path = write_temp_file(Dir, "empty.bt", <<"// just a comment\n">>),
        ?assertEqual({undefined, undefined}, beamtalk_repl_ops_load:extract_bt_class_info(Path))
    after
        rm_temp_dir(Dir)
    end.

extract_bt_class_info_missing_file_test() ->
    ?assertEqual(
        {undefined, undefined}, beamtalk_repl_ops_load:extract_bt_class_info("/no/such/file.bt")
    ).

%%====================================================================
%% sort_bt_files_by_deps/1
%%====================================================================

sort_bt_files_empty_test() ->
    ?assertEqual([], beamtalk_repl_ops_load:sort_bt_files_by_deps([])).

sort_bt_files_superclass_before_subclass_test() ->
    Dir = make_temp_dir(),
    try
        PathA = write_temp_file(Dir, "a.bt", <<"Object subclass: A">>),
        PathB = write_temp_file(Dir, "b.bt", <<"A subclass: B">>),
        %% Provide in "wrong" order — B before A; sort must reorder so A loads first.
        Sorted = beamtalk_repl_ops_load:sort_bt_files_by_deps([PathB, PathA]),
        ?assertEqual([PathA, PathB], Sorted)
    after
        rm_temp_dir(Dir)
    end.

sort_bt_files_independent_files_stable_test() ->
    Dir = make_temp_dir(),
    try
        %% Both files declare a direct Object subclass (superclass is outside the project).
        %% Neither depends on the other, so input order should be preserved.
        PathA = write_temp_file(Dir, "a.bt", <<"Object subclass: A">>),
        PathB = write_temp_file(Dir, "b.bt", <<"Object subclass: B">>),
        Sorted = beamtalk_repl_ops_load:sort_bt_files_by_deps([PathA, PathB]),
        ?assertEqual([PathA, PathB], Sorted)
    after
        rm_temp_dir(Dir)
    end.

%%====================================================================
%% structured_file_errors/2
%%====================================================================

structured_file_errors_compile_diagnostics_test() ->
    Diags = [
        #{message => <<"undefined variable 'x'">>, line => 5, hint => <<"Did you mean 'self'?">>},
        #{message => <<"unexpected token">>}
    ],
    Result = beamtalk_repl_ops_load:structured_file_errors(
        "/src/Broken.bt", {compile_error, Diags}
    ),
    ?assertEqual(2, length(Result)),
    [E1, E2] = Result,
    ?assertEqual(<<"/src/Broken.bt">>, maps:get(<<"path">>, E1)),
    ?assertEqual(<<"compile_error">>, maps:get(<<"kind">>, E1)),
    ?assertEqual(<<"undefined variable 'x'">>, maps:get(<<"message">>, E1)),
    ?assertEqual(5, maps:get(<<"line">>, E1)),
    ?assertEqual(<<"Did you mean 'self'?">>, maps:get(<<"hint">>, E1)),
    %% Second diagnostic has no line or hint.
    ?assertEqual(<<"unexpected token">>, maps:get(<<"message">>, E2)),
    ?assertEqual(error, maps:find(<<"line">>, E2)),
    ?assertEqual(error, maps:find(<<"hint">>, E2)).

structured_file_errors_binary_diagnostic_test() ->
    Result = beamtalk_repl_ops_load:structured_file_errors(
        "/src/Bad.bt", {compile_error, [<<"raw error text">>]}
    ),
    ?assertEqual(1, length(Result)),
    [E] = Result,
    ?assertEqual(<<"raw error text">>, maps:get(<<"message">>, E)).

structured_file_errors_non_compile_error_test() ->
    Result = beamtalk_repl_ops_load:structured_file_errors(
        "/src/Missing.bt", {file_not_found, "/src/Missing.bt"}
    ),
    ?assertEqual(1, length(Result)),
    [E] = Result,
    ?assertEqual(<<"/src/Missing.bt">>, maps:get(<<"path">>, E)),
    ?assertEqual(<<"file_not_found">>, maps:get(<<"kind">>, E)).

%%====================================================================
%% format_collision_warning/3 (BT-1659)
%%====================================================================

format_collision_warning_with_packages_test() ->
    %% When both modules are package-qualified, the warning should include a hint.
    Result = beamtalk_repl_ops_load:format_collision_warning(
        'Parser', 'bt@json@parser', 'bt@xml@parser'
    ),
    ?assert(binary:match(Result, <<"Class 'Parser' redefined">>) =/= nomatch),
    ?assert(binary:match(Result, <<"json@Parser">>) =/= nomatch),
    ?assert(binary:match(Result, <<"xml@Parser">>) =/= nomatch),
    ?assert(binary:match(Result, <<"to be explicit">>) =/= nomatch).

format_collision_warning_without_packages_test() ->
    %% When modules aren't package-qualified, no hint is added.
    Result = beamtalk_repl_ops_load:format_collision_warning(
        'Counter', 'bt@counter', 'bt@counter_v2'
    ),
    ?assert(binary:match(Result, <<"Class 'Counter' redefined">>) =/= nomatch),
    %% No qualified name hint when there's no second @ segment
    ?assertEqual(nomatch, binary:match(Result, <<"to be explicit">>)).

extract_package_from_module_qualified_test() ->
    ?assertEqual(<<"json">>, beamtalk_repl_ops_load:extract_package_from_module('bt@json@parser')).

extract_package_from_module_unqualified_test() ->
    ?assertEqual(undefined, beamtalk_repl_ops_load:extract_package_from_module('bt@counter')).

%%====================================================================
%% classify_files_by_change/2 (BT-1685)
%%====================================================================

classify_files_all_new_test() ->
    %% No previous mtimes — all files should be classified as changed.
    Files = ["/src/a.bt", "/src/b.bt"],
    {Changed, Unchanged, Deleted} =
        beamtalk_repl_ops_load:classify_files_by_change(Files, #{}),
    ?assertEqual(2, length(Changed)),
    ?assertEqual(0, length(Unchanged)),
    ?assertEqual(0, length(Deleted)).

classify_files_unchanged_test() ->
    %% When mtimes match, files are unchanged.
    Dir = make_temp_dir(),
    try
        PathA = write_temp_file(Dir, "a.bt", <<"Object subclass: A">>),
        PathB = write_temp_file(Dir, "b.bt", <<"Object subclass: B">>),
        MtimeA = beamtalk_repl_ops_load:get_file_mtime(PathA),
        MtimeB = beamtalk_repl_ops_load:get_file_mtime(PathB),
        PrevMtimes = #{PathA => MtimeA, PathB => MtimeB},
        {Changed, Unchanged, Deleted} =
            beamtalk_repl_ops_load:classify_files_by_change([PathA, PathB], PrevMtimes),
        ?assertEqual(0, length(Changed)),
        ?assertEqual(2, length(Unchanged)),
        ?assertEqual(0, length(Deleted))
    after
        rm_temp_dir(Dir)
    end.

classify_files_deleted_test() ->
    %% Files in PreviousMtimes but not in current list are deleted.
    Dir = make_temp_dir(),
    try
        PathA = write_temp_file(Dir, "a.bt", <<"Object subclass: A">>),
        MtimeA = beamtalk_repl_ops_load:get_file_mtime(PathA),
        PrevMtimes = #{
            PathA => MtimeA,
            "/src/deleted.bt" => {{2025, 1, 1}, {0, 0, 0}}
        },
        {Changed, Unchanged, Deleted} =
            beamtalk_repl_ops_load:classify_files_by_change([PathA], PrevMtimes),
        ?assertEqual(0, length(Changed)),
        ?assertEqual(1, length(Unchanged)),
        ?assertEqual(1, length(Deleted)),
        ?assertEqual(["/src/deleted.bt"], Deleted)
    after
        rm_temp_dir(Dir)
    end.

classify_files_mixed_test() ->
    %% Mix of new, changed, unchanged, and deleted files.
    Dir = make_temp_dir(),
    try
        PathA = write_temp_file(Dir, "a.bt", <<"Object subclass: A">>),
        PathB = write_temp_file(Dir, "b.bt", <<"Object subclass: B">>),
        PathC = write_temp_file(Dir, "c.bt", <<"Object subclass: C">>),
        MtimeA = beamtalk_repl_ops_load:get_file_mtime(PathA),
        PrevMtimes = #{
            PathA => MtimeA,
            %% b.bt has a stale mtime — will be detected as changed
            PathB => {{2020, 1, 1}, {0, 0, 0}},
            %% deleted.bt is not in current files
            "/src/deleted.bt" => {{2025, 1, 1}, {0, 0, 0}}
        },
        %% c.bt is new (not in PrevMtimes)
        {Changed, Unchanged, Deleted} =
            beamtalk_repl_ops_load:classify_files_by_change(
                [PathA, PathB, PathC], PrevMtimes
            ),
        ?assertEqual(1, length(Unchanged)),
        ?assert(lists:member(PathA, Unchanged)),
        ?assertEqual(2, length(Changed)),
        ?assert(lists:member(PathB, Changed)),
        ?assert(lists:member(PathC, Changed)),
        ?assertEqual(1, length(Deleted)),
        ?assertEqual(["/src/deleted.bt"], Deleted)
    after
        rm_temp_dir(Dir)
    end.

%%====================================================================
%% get_file_mtime/1 (BT-1685)
%%====================================================================

get_file_mtime_existing_file_test() ->
    Dir = make_temp_dir(),
    try
        Path = write_temp_file(Dir, "test.bt", <<"hello">>),
        Mtime = beamtalk_repl_ops_load:get_file_mtime(Path),
        %% Mtime should be a valid datetime tuple
        ?assertMatch({{_, _, _}, {_, _, _}}, Mtime),
        %% Should not be the zero tuple
        ?assertNotEqual({{0, 0, 0}, {0, 0, 0}}, Mtime)
    after
        rm_temp_dir(Dir)
    end.

get_file_mtime_missing_file_test() ->
    Mtime = beamtalk_repl_ops_load:get_file_mtime("/no/such/file.bt"),
    ?assertEqual({{0, 0, 0}, {0, 0, 0}}, Mtime).

%%====================================================================
%% find_erl_files/1 (BT-1716)
%%====================================================================

find_erl_files_nonexistent_dir_test() ->
    Missing = "missing_native_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    ?assertEqual([], beamtalk_repl_ops_load:find_erl_files(Missing)).

find_erl_files_empty_dir_test() ->
    Dir = make_temp_dir(),
    try
        ?assertEqual([], beamtalk_repl_ops_load:find_erl_files(Dir))
    after
        rm_temp_dir(Dir)
    end.

find_erl_files_finds_erl_only_test() ->
    Dir = make_temp_dir(),
    try
        write_temp_file(Dir, "my_mod.erl", <<"-module(my_mod).">>),
        write_temp_file(Dir, "readme.md", <<"docs">>),
        write_temp_file(Dir, "something.bt", <<"Object subclass: X">>),
        Found = beamtalk_repl_ops_load:find_erl_files(Dir),
        ?assertEqual(1, length(Found)),
        ?assert(lists:any(fun(F) -> lists:suffix("my_mod.erl", F) end, Found))
    after
        rm_temp_dir(Dir)
    end.

find_erl_files_skips_test_dir_test() ->
    Dir = make_temp_dir(),
    try
        write_temp_file(Dir, "my_mod.erl", <<"-module(my_mod).">>),
        TestDir = filename:join(Dir, "test"),
        ok = file:make_dir(TestDir),
        write_temp_file(TestDir, "my_mod_tests.erl", <<"-module(my_mod_tests).">>),
        Found = beamtalk_repl_ops_load:find_erl_files(Dir),
        ?assertEqual(1, length(Found)),
        ?assert(lists:any(fun(F) -> lists:suffix("my_mod.erl", F) end, Found)),
        %% Test file should not be included.
        ?assertNot(lists:any(fun(F) -> lists:suffix("my_mod_tests.erl", F) end, Found))
    after
        rm_temp_dir(Dir)
    end.

%%====================================================================
%% compile_native_erl_files/2 (BT-1716)
%%====================================================================

compile_native_erl_files_empty_test() ->
    {Errors, Count} = beamtalk_repl_ops_load:compile_native_erl_files([], "/tmp"),
    ?assertEqual([], Errors),
    ?assertEqual(0, Count).

compile_native_erl_files_valid_module_test() ->
    Dir = make_temp_dir(),
    try
        NativeDir = filename:join(Dir, "native"),
        ok = file:make_dir(NativeDir),
        ErlSrc = <<
            "-module(bt_native_test_valid_mod).\n"
            "-export([hello/0]).\n"
            "hello() -> world.\n"
        >>,
        ErlPath = write_temp_file(NativeDir, "bt_native_test_valid_mod.erl", ErlSrc),
        {Errors, Count} = beamtalk_repl_ops_load:compile_native_erl_files([ErlPath], Dir),
        ?assertEqual([], Errors),
        ?assertEqual(1, Count),
        %% Module should be loadable and callable.
        ?assertEqual(world, bt_native_test_valid_mod:hello()),
        %% Cleanup: purge test module.
        code:purge(bt_native_test_valid_mod),
        code:delete(bt_native_test_valid_mod)
    after
        rm_temp_dir(Dir)
    end.

compile_native_erl_files_syntax_error_test() ->
    Dir = make_temp_dir(),
    try
        NativeDir = filename:join(Dir, "native"),
        ok = file:make_dir(NativeDir),
        ErlSrc = <<
            "-module(bt_native_test_bad_mod).\n"
            "-export([hello/0]).\n"
            "hello( -> world.\n"
        >>,
        ErlPath = write_temp_file(NativeDir, "bt_native_test_bad_mod.erl", ErlSrc),
        {Errors, Count} = beamtalk_repl_ops_load:compile_native_erl_files([ErlPath], Dir),
        ?assertEqual(0, Count),
        ?assert(length(Errors) > 0),
        [ErrMap | _] = Errors,
        ?assertEqual(<<"compile_error">>, maps:get(<<"kind">>, ErrMap)),
        ?assert(is_binary(maps:get(<<"message">>, ErrMap)))
    after
        rm_temp_dir(Dir)
    end.

compile_native_erl_files_with_include_test() ->
    Dir = make_temp_dir(),
    try
        NativeDir = filename:join(Dir, "native"),
        ok = file:make_dir(NativeDir),
        IncludeDir = filename:join(NativeDir, "include"),
        ok = file:make_dir(IncludeDir),
        %% Write a header file.
        write_temp_file(
            IncludeDir,
            "bt_native_test_inc.hrl",
            <<"-define(BT_TEST_VAL, 42).\n">>
        ),
        %% Write a module that uses the header.
        ErlSrc = <<
            "-module(bt_native_test_inc_mod).\n"
            "-include(\"bt_native_test_inc.hrl\").\n"
            "-export([val/0]).\n"
            "val() -> ?BT_TEST_VAL.\n"
        >>,
        ErlPath = write_temp_file(NativeDir, "bt_native_test_inc_mod.erl", ErlSrc),
        {Errors, Count} = beamtalk_repl_ops_load:compile_native_erl_files([ErlPath], Dir),
        ?assertEqual([], Errors),
        ?assertEqual(1, Count),
        ?assertEqual(42, bt_native_test_inc_mod:val()),
        %% Cleanup.
        code:purge(bt_native_test_inc_mod),
        code:delete(bt_native_test_inc_mod)
    after
        rm_temp_dir(Dir)
    end.

compile_native_erl_files_multiple_test() ->
    Dir = make_temp_dir(),
    try
        NativeDir = filename:join(Dir, "native"),
        ok = file:make_dir(NativeDir),
        ErlSrc1 = <<
            "-module(bt_native_test_multi1).\n"
            "-export([a/0]).\n"
            "a() -> one.\n"
        >>,
        ErlSrc2 = <<
            "-module(bt_native_test_multi2).\n"
            "-export([b/0]).\n"
            "b() -> two.\n"
        >>,
        Path1 = write_temp_file(NativeDir, "bt_native_test_multi1.erl", ErlSrc1),
        Path2 = write_temp_file(NativeDir, "bt_native_test_multi2.erl", ErlSrc2),
        {Errors, Count} = beamtalk_repl_ops_load:compile_native_erl_files(
            [Path1, Path2], Dir
        ),
        ?assertEqual([], Errors),
        ?assertEqual(2, Count),
        ?assertEqual(one, bt_native_test_multi1:a()),
        ?assertEqual(two, bt_native_test_multi2:b()),
        %% Cleanup.
        code:purge(bt_native_test_multi1),
        code:delete(bt_native_test_multi1),
        code:purge(bt_native_test_multi2),
        code:delete(bt_native_test_multi2)
    after
        rm_temp_dir(Dir)
    end.

%%====================================================================
%% extract_native_refs/1 (BT-1717)
%%====================================================================

extract_native_refs_native_annotation_test() ->
    Dir = make_temp_dir(),
    try
        Path = write_temp_file(
            Dir,
            "server.bt",
            <<
                "Actor subclass: HTTPServer native: beamtalk_http_server\n"
                "  start => self\n"
            >>
        ),
        Refs = beamtalk_repl_ops_load:extract_native_refs(Path),
        ?assertEqual([<<"beamtalk_http_server">>], Refs)
    after
        rm_temp_dir(Dir)
    end.

extract_native_refs_ffi_reference_test() ->
    Dir = make_temp_dir(),
    try
        Path = write_temp_file(
            Dir,
            "datetime.bt",
            <<
                "Object subclass: DateTime\n"
                "  now => (Erlang beamtalk_datetime) now\n"
                "  year => (Erlang beamtalk_datetime) year: self\n"
            >>
        ),
        Refs = beamtalk_repl_ops_load:extract_native_refs(Path),
        %% Should be deduplicated.
        ?assertEqual([<<"beamtalk_datetime">>], Refs)
    after
        rm_temp_dir(Dir)
    end.

extract_native_refs_both_native_and_ffi_test() ->
    Dir = make_temp_dir(),
    try
        Path = write_temp_file(
            Dir,
            "mixed.bt",
            <<
                "Actor subclass: MyActor native: my_native_mod\n"
                "  helper => (Erlang my_ffi_mod) doStuff\n"
            >>
        ),
        Refs = beamtalk_repl_ops_load:extract_native_refs(Path),
        ?assertEqual([<<"my_ffi_mod">>, <<"my_native_mod">>], Refs)
    after
        rm_temp_dir(Dir)
    end.

extract_native_refs_no_refs_test() ->
    Dir = make_temp_dir(),
    try
        Path = write_temp_file(
            Dir,
            "pure.bt",
            <<
                "Object subclass: Counter\n"
                "  state: count = 0\n"
                "  increment => self.count := self.count + 1\n"
            >>
        ),
        Refs = beamtalk_repl_ops_load:extract_native_refs(Path),
        ?assertEqual([], Refs)
    after
        rm_temp_dir(Dir)
    end.

extract_native_refs_missing_file_test() ->
    Refs = beamtalk_repl_ops_load:extract_native_refs("/no/such/file.bt"),
    ?assertEqual([], Refs).

%%====================================================================
%% find_project_root/1 (BT-1717)
%%====================================================================

find_project_root_with_manifest_test() ->
    Dir = make_temp_dir(),
    try
        write_temp_file(Dir, "beamtalk.toml", <<"[package]\nname = \"test\"\n">>),
        SrcDir = filename:join(Dir, "src"),
        ok = file:make_dir(SrcDir),
        BtFile = filename:join(SrcDir, "Counter.bt"),
        ok = file:write_file(BtFile, <<"Object subclass: Counter">>),
        AbsDir = filename:absname(Dir),
        ?assertEqual(AbsDir, beamtalk_repl_ops_load:find_project_root(BtFile))
    after
        rm_temp_dir(Dir)
    end.

find_project_root_no_manifest_test() ->
    %% A path with no beamtalk.toml anywhere up the tree should return undefined.
    %% Use an isolated temp directory to avoid stray manifests in ancestor dirs.
    Dir = make_temp_dir(),
    try
        NoProjectDir = filename:join(Dir, "no_project"),
        ok = file:make_dir(NoProjectDir),
        SrcDir = filename:join(NoProjectDir, "src"),
        ok = file:make_dir(SrcDir),
        FakePath = write_temp_file(SrcDir, "X.bt", <<"">>),
        ?assertEqual(undefined, beamtalk_repl_ops_load:find_project_root(FakePath))
    after
        rm_temp_dir(Dir)
    end.

%%====================================================================
%% maybe_recompile_native_deps/2 (BT-1717)
%%====================================================================

maybe_recompile_native_deps_no_project_root_test() ->
    ?assertEqual(
        {ok, 0},
        beamtalk_repl_ops_load:maybe_recompile_native_deps("/src/X.bt", undefined)
    ).

maybe_recompile_native_deps_no_refs_test() ->
    Dir = make_temp_dir(),
    try
        NativeDir = filename:join(Dir, "native"),
        ok = file:make_dir(NativeDir),
        SrcDir = filename:join(Dir, "src"),
        ok = file:make_dir(SrcDir),
        BtPath = write_temp_file(
            SrcDir,
            "Pure.bt",
            <<"Object subclass: Pure\n  hello => \"world\"\n">>
        ),
        ?assertEqual(
            {ok, 0},
            beamtalk_repl_ops_load:maybe_recompile_native_deps(BtPath, Dir)
        )
    after
        rm_temp_dir(Dir)
    end.

maybe_recompile_native_deps_compiles_stale_test() ->
    Dir = make_temp_dir(),
    try
        NativeDir = filename:join(Dir, "native"),
        ok = file:make_dir(NativeDir),
        SrcDir = filename:join(Dir, "src"),
        ok = file:make_dir(SrcDir),
        %% Write native .erl module.
        ErlSrc = <<
            "-module(bt_native_test_demand_mod).\n"
            "-export([greet/0]).\n"
            "greet() -> hello.\n"
        >>,
        write_temp_file(NativeDir, "bt_native_test_demand_mod.erl", ErlSrc),
        %% Write .bt file referencing it via native: annotation.
        BtPath = write_temp_file(
            SrcDir,
            "MyActor.bt",
            <<
                "Actor subclass: MyActor native: bt_native_test_demand_mod\n"
                "  start => self\n"
            >>
        ),
        %% Module is not loaded — should trigger compilation.
        code:purge(bt_native_test_demand_mod),
        code:delete(bt_native_test_demand_mod),
        ?assertEqual(
            {ok, 1},
            beamtalk_repl_ops_load:maybe_recompile_native_deps(BtPath, Dir)
        ),
        %% Module should now be callable.
        ?assertEqual(hello, bt_native_test_demand_mod:greet()),
        %% Cleanup.
        code:purge(bt_native_test_demand_mod),
        code:delete(bt_native_test_demand_mod)
    after
        rm_temp_dir(Dir)
    end.

maybe_recompile_native_deps_skips_unchanged_test() ->
    Dir = make_temp_dir(),
    try
        NativeDir = filename:join(Dir, "native"),
        ok = file:make_dir(NativeDir),
        SrcDir = filename:join(Dir, "src"),
        ok = file:make_dir(SrcDir),
        %% Write and compile native .erl module.
        ErlSrc = <<
            "-module(bt_native_test_nochange_mod).\n"
            "-export([val/0]).\n"
            "val() -> 42.\n"
        >>,
        ErlPath = write_temp_file(NativeDir, "bt_native_test_nochange_mod.erl", ErlSrc),
        %% Pre-compile it so it's already loaded.
        {[], 1} = beamtalk_repl_ops_load:compile_native_erl_files([ErlPath], Dir),
        %% Write .bt file referencing it.
        BtPath = write_temp_file(
            SrcDir,
            "MyActor2.bt",
            <<
                "Actor subclass: MyActor2 native: bt_native_test_nochange_mod\n"
                "  start => self\n"
            >>
        ),
        %% The module is already loaded from the same .erl file, and the .erl file
        %% has not been modified — should not recompile.
        %% Note: Since compile_native_erl_files uses code:load_binary which sets
        %% the file path as the .erl source, and the .erl file hasn't changed,
        %% the mtime comparison should show it's not stale.
        Result = beamtalk_repl_ops_load:maybe_recompile_native_deps(BtPath, Dir),
        ?assertEqual({ok, 0}, Result),
        %% Cleanup.
        code:purge(bt_native_test_nochange_mod),
        code:delete(bt_native_test_nochange_mod)
    after
        rm_temp_dir(Dir)
    end.

maybe_recompile_native_deps_no_erl_file_test() ->
    Dir = make_temp_dir(),
    try
        NativeDir = filename:join(Dir, "native"),
        ok = file:make_dir(NativeDir),
        SrcDir = filename:join(Dir, "src"),
        ok = file:make_dir(SrcDir),
        %% .bt references a module but no .erl exists in native/.
        BtPath = write_temp_file(
            SrcDir,
            "Missing.bt",
            <<
                "Actor subclass: Missing native: bt_nonexistent_mod\n"
                "  start => self\n"
            >>
        ),
        ?assertEqual(
            {ok, 0},
            beamtalk_repl_ops_load:maybe_recompile_native_deps(BtPath, Dir)
        )
    after
        rm_temp_dir(Dir)
    end.

maybe_recompile_native_deps_ffi_ref_compiles_test() ->
    Dir = make_temp_dir(),
    try
        NativeDir = filename:join(Dir, "native"),
        ok = file:make_dir(NativeDir),
        SrcDir = filename:join(Dir, "src"),
        ok = file:make_dir(SrcDir),
        %% Write native .erl module.
        ErlSrc = <<
            "-module(bt_native_test_ffi_demand).\n"
            "-export([work/0]).\n"
            "work() -> done.\n"
        >>,
        write_temp_file(NativeDir, "bt_native_test_ffi_demand.erl", ErlSrc),
        %% Write .bt file referencing it via (Erlang ...) FFI.
        BtPath = write_temp_file(
            SrcDir,
            "Worker.bt",
            <<
                "Object subclass: Worker\n"
                "  doWork => (Erlang bt_native_test_ffi_demand) work\n"
            >>
        ),
        %% Module is not loaded — should trigger compilation.
        code:purge(bt_native_test_ffi_demand),
        code:delete(bt_native_test_ffi_demand),
        ?assertEqual(
            {ok, 1},
            beamtalk_repl_ops_load:maybe_recompile_native_deps(BtPath, Dir)
        ),
        ?assertEqual(done, bt_native_test_ffi_demand:work()),
        %% Cleanup.
        code:purge(bt_native_test_ffi_demand),
        code:delete(bt_native_test_ffi_demand)
    after
        rm_temp_dir(Dir)
    end.

%%====================================================================
%% activate_dependency_modules tests
%%====================================================================

activate_deps_no_build_dir_test() ->
    Dir = filename:absname(make_temp_dir()),
    try
        %% No _build dir — should be a no-op.
        ?assertEqual([], beamtalk_repl_ops_load:activate_dependency_modules(Dir))
    after
        rm_temp_dir(Dir)
    end.

activate_deps_adds_code_path_test() ->
    Dir = filename:absname(make_temp_dir()),
    try
        EbinDir = filename:join([Dir, "_build", "deps", "testdep", "ebin"]),
        ok = filelib:ensure_dir(filename:join(EbinDir, "dummy")),
        %% Ensure it's not already on the code path.
        code:del_path(EbinDir),
        ?assertNot(lists:member(EbinDir, code:get_path())),
        ?assertEqual([], beamtalk_repl_ops_load:activate_dependency_modules(Dir)),
        ?assert(lists:member(EbinDir, code:get_path())),
        %% Cleanup.
        code:del_path(EbinDir)
    after
        rm_temp_dir(Dir)
    end.

activate_deps_adds_native_ebin_path_test() ->
    Dir = filename:absname(make_temp_dir()),
    try
        NativeEbin = filename:join([Dir, "_build", "dev", "native", "ebin"]),
        ok = filelib:ensure_dir(filename:join(NativeEbin, "dummy")),
        code:del_path(NativeEbin),
        ?assertNot(lists:member(NativeEbin, code:get_path())),
        ?assertEqual([], beamtalk_repl_ops_load:activate_dependency_modules(Dir)),
        ?assert(lists:member(NativeEbin, code:get_path())),
        code:del_path(NativeEbin)
    after
        rm_temp_dir(Dir)
    end.

activate_deps_adds_hex_ebin_paths_test() ->
    Dir = filename:absname(make_temp_dir()),
    try
        HexEbin = filename:join([Dir, "_build", "dev", "native", "default", "lib", "cowboy", "ebin"]),
        ok = filelib:ensure_dir(filename:join(HexEbin, "dummy")),
        code:del_path(HexEbin),
        ?assertNot(lists:member(HexEbin, code:get_path())),
        ?assertEqual([], beamtalk_repl_ops_load:activate_dependency_modules(Dir)),
        ?assert(lists:member(HexEbin, code:get_path())),
        code:del_path(HexEbin)
    after
        rm_temp_dir(Dir)
    end.

activate_dep_ebin_nonexistent_test() ->
    %% Non-existent dir should be a no-op (delegated to beamtalk_module_activation).
    ?assertEqual({ok, []}, beamtalk_module_activation:activate_ebin("/nonexistent/path")).
