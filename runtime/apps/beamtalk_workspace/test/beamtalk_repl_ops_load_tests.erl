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
