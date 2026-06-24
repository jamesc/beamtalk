%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_load_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
EUnit tests for beamtalk_repl_ops_load module.

Tests the load-project helpers: find_bt_files, extract_bt_class_info,
sort_bt_files_by_deps, and structured_file_errors. These are pure
filesystem/parsing functions that do not require a running REPL workspace.
""".

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
%% filter_mtimes_under_project/2 (BT-2089)
%%====================================================================

filter_mtimes_under_project_basic_test() ->
    %% Files under the project root are kept; files outside are dropped.
    Mtimes = #{
        "/projects/foo/src/a.bt" => {{2026, 4, 26}, {0, 0, 0}},
        "/projects/foo/src/b.bt" => {{2026, 4, 26}, {0, 0, 0}},
        "/projects/bar/src/c.bt" => {{2026, 4, 26}, {0, 0, 0}}
    },
    Filtered = beamtalk_repl_ops_load:filter_mtimes_under_project(
        Mtimes, "/projects/foo"
    ),
    ?assertEqual(2, maps:size(Filtered)),
    ?assert(maps:is_key("/projects/foo/src/a.bt", Filtered)),
    ?assert(maps:is_key("/projects/foo/src/b.bt", Filtered)),
    ?assertNot(maps:is_key("/projects/bar/src/c.bt", Filtered)).

filter_mtimes_under_project_prefix_boundary_test() ->
    %% A path that shares a string prefix with the project root but lies in
    %% a different directory must NOT match — `/projects/foobar` is not a
    %% child of `/projects/foo`.
    Mtimes = #{
        "/projects/foo/src/a.bt" => {{2026, 4, 26}, {0, 0, 0}},
        "/projects/foobar/src/c.bt" => {{2026, 4, 26}, {0, 0, 0}}
    },
    Filtered = beamtalk_repl_ops_load:filter_mtimes_under_project(
        Mtimes, "/projects/foo"
    ),
    ?assertEqual(1, maps:size(Filtered)),
    ?assert(maps:is_key("/projects/foo/src/a.bt", Filtered)),
    ?assertNot(maps:is_key("/projects/foobar/src/c.bt", Filtered)).

filter_mtimes_under_project_trailing_slash_test() ->
    %% A trailing slash on the project root must be tolerated.
    Mtimes = #{
        "/projects/foo/src/a.bt" => {{2026, 4, 26}, {0, 0, 0}},
        "/projects/bar/src/b.bt" => {{2026, 4, 26}, {0, 0, 0}}
    },
    Filtered = beamtalk_repl_ops_load:filter_mtimes_under_project(
        Mtimes, "/projects/foo/"
    ),
    ?assertEqual(1, maps:size(Filtered)),
    ?assert(maps:is_key("/projects/foo/src/a.bt", Filtered)).

filter_mtimes_under_project_empty_test() ->
    %% Empty input map yields empty output.
    ?assertEqual(
        #{},
        beamtalk_repl_ops_load:filter_mtimes_under_project(#{}, "/projects/foo")
    ).

filter_mtimes_under_project_no_matches_test() ->
    %% No files under the root → empty result.
    Mtimes = #{
        "/other/a.bt" => {{2026, 4, 26}, {0, 0, 0}},
        "/elsewhere/b.bt" => {{2026, 4, 26}, {0, 0, 0}}
    },
    ?assertEqual(
        #{},
        beamtalk_repl_ops_load:filter_mtimes_under_project(Mtimes, "/projects/foo")
    ).

filter_mtimes_under_project_keeps_test_dir_test() ->
    %% Test files under the project root are still scoped in.
    %% (BT-2089: scoping out test/ when include_tests=false is the
    %% caller's responsibility — this helper only filters by project root.)
    Mtimes = #{
        "/projects/foo/src/a.bt" => {{2026, 4, 26}, {0, 0, 0}},
        "/projects/foo/test/b.bt" => {{2026, 4, 26}, {0, 0, 0}}
    },
    Filtered = beamtalk_repl_ops_load:filter_mtimes_under_project(
        Mtimes, "/projects/foo"
    ),
    ?assertEqual(2, maps:size(Filtered)).

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
%% find_erl_files/2 (BT-1716, BT-2653)
%%====================================================================

find_erl_files_nonexistent_dir_test() ->
    Missing = "missing_native_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    ?assertEqual([], beamtalk_repl_ops_load:find_erl_files(Missing, false)).

find_erl_files_empty_dir_test() ->
    Dir = make_temp_dir(),
    try
        ?assertEqual([], beamtalk_repl_ops_load:find_erl_files(Dir, false))
    after
        rm_temp_dir(Dir)
    end.

find_erl_files_finds_erl_only_test() ->
    Dir = make_temp_dir(),
    try
        write_temp_file(Dir, "my_mod.erl", <<"-module(my_mod).">>),
        write_temp_file(Dir, "readme.md", <<"docs">>),
        write_temp_file(Dir, "something.bt", <<"Object subclass: X">>),
        Found = beamtalk_repl_ops_load:find_erl_files(Dir, false),
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
        %% include_tests=false: native/test/ helpers are excluded.
        Found = beamtalk_repl_ops_load:find_erl_files(Dir, false),
        ?assertEqual(1, length(Found)),
        ?assert(lists:any(fun(F) -> lists:suffix("my_mod.erl", F) end, Found)),
        %% Test file should not be included.
        ?assertNot(lists:any(fun(F) -> lists:suffix("my_mod_tests.erl", F) end, Found))
    after
        rm_temp_dir(Dir)
    end.

%% BT-2653: include_tests=true MUST include native/test/ helpers so a `.bt`
%% test can drive them via `(Erlang <helper>) <msg>` on the test-load path.
find_erl_files_includes_test_dir_when_flag_set_test() ->
    Dir = make_temp_dir(),
    try
        write_temp_file(Dir, "my_mod.erl", <<"-module(my_mod).">>),
        TestDir = filename:join(Dir, "test"),
        ok = file:make_dir(TestDir),
        write_temp_file(TestDir, "my_helper.erl", <<"-module(my_helper).">>),
        Found = beamtalk_repl_ops_load:find_erl_files(Dir, true),
        ?assertEqual(2, length(Found)),
        ?assert(lists:any(fun(F) -> lists:suffix("my_mod.erl", F) end, Found)),
        %% Test helper IS included when the flag is set.
        ?assert(lists:any(fun(F) -> lists:suffix("my_helper.erl", F) end, Found))
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
        ?assertEqual([], beamtalk_workspace_bootstrap:activate_dependency_modules(Dir))
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
        ?assertEqual([], beamtalk_workspace_bootstrap:activate_dependency_modules(Dir)),
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
        ?assertEqual([], beamtalk_workspace_bootstrap:activate_dependency_modules(Dir)),
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
        ?assertEqual([], beamtalk_workspace_bootstrap:activate_dependency_modules(Dir)),
        ?assert(lists:member(HexEbin, code:get_path())),
        code:del_path(HexEbin)
    after
        rm_temp_dir(Dir)
    end.

activate_dep_ebin_nonexistent_test() ->
    %% Non-existent dir should be a no-op (delegated to beamtalk_module_activation).
    ?assertEqual({ok, []}, beamtalk_module_activation:activate_ebin("/nonexistent/path")).

%%====================================================================
%% render_class_header/1 (BT-2653)
%%====================================================================

render_class_header_empty_test() ->
    %% An empty index still produces a valid, guarded header.
    Bin = iolist_to_binary(beamtalk_repl_ops_load:render_class_header(#{})),
    ?assert(binary:match(Bin, <<"-ifndef(BEAMTALK_CLASSES_HRL)">>) =/= nomatch),
    ?assert(binary:match(Bin, <<"-define(BEAMTALK_CLASSES_HRL, true)">>) =/= nomatch),
    ?assert(binary:match(Bin, <<"-endif">>) =/= nomatch),
    %% No class macros for an empty index.
    ?assertEqual(nomatch, binary:match(Bin, <<"BT_CLASS_MODULE_">>)).

render_class_header_defines_macros_test() ->
    Index = #{
        <<"HTTPResponse">> => <<"bt@http@http_response">>,
        <<"Counter">> => <<"bt@counter">>
    },
    Bin = iolist_to_binary(beamtalk_repl_ops_load:render_class_header(Index)),
    ?assert(
        binary:match(
            Bin, <<"-define(BT_CLASS_MODULE_HTTPResponse, 'bt@http@http_response').">>
        ) =/= nomatch
    ),
    ?assert(
        binary:match(Bin, <<"-define(BT_CLASS_MODULE_Counter, 'bt@counter').">>) =/= nomatch
    ).

render_class_header_sorted_test() ->
    %% Entries are sorted for deterministic output (Aaa before Zzz).
    Index = #{<<"Zzz">> => <<"bt@zzz">>, <<"Aaa">> => <<"bt@aaa">>},
    Bin = iolist_to_binary(beamtalk_repl_ops_load:render_class_header(Index)),
    {AaaPos, _} = binary:match(Bin, <<"BT_CLASS_MODULE_Aaa">>),
    {ZzzPos, _} = binary:match(Bin, <<"BT_CLASS_MODULE_Zzz">>),
    ?assert(AaaPos < ZzzPos).

render_class_header_skips_unsafe_names_test() ->
    %% A class name that is not a bare identifier, or a module name that would
    %% break out of the quoted atom, must be skipped so the generated header
    %% stays compilable. The safe entry survives; the unsafe ones are dropped
    %% and the result still includes/compiles to a valid Erlang header.
    Index = #{
        <<"Good">> => <<"bt@good">>,
        %% Class name with a space → invalid macro identifier.
        <<"Bad Name">> => <<"bt@bad">>,
        %% Module name with a quote → would break the quoted atom.
        <<"Quoted">> => <<"bt@qu'ote">>
    },
    Bin = iolist_to_binary(beamtalk_repl_ops_load:render_class_header(Index)),
    ?assert(binary:match(Bin, <<"-define(BT_CLASS_MODULE_Good, 'bt@good').">>) =/= nomatch),
    ?assertEqual(nomatch, binary:match(Bin, <<"Bad Name">>)),
    ?assertEqual(nomatch, binary:match(Bin, <<"qu'ote">>)),
    %% Sanity: the rendered header is parseable by erl_scan (no stray tokens).
    {ok, _Tokens, _} = erl_scan:string(binary_to_list(Bin)).

%%====================================================================
%% regenerate_native_class_header/1 (BT-2653)
%%====================================================================

regenerate_native_class_header_writes_file_test() ->
    %% The header is written under _build/dev/native/include/ even when no
    %% classes are registered (empty index → guarded, macro-free header).
    %% The first write reports `true` (content changed from nothing).
    Dir = filename:absname(make_temp_dir()),
    try
        ?assertEqual(true, beamtalk_repl_ops_load:regenerate_native_class_header(Dir)),
        HrlPath = filename:join(
            beamtalk_repl_ops_load:native_generated_include_dir(Dir),
            "beamtalk_classes.hrl"
        ),
        ?assert(filelib:is_file(HrlPath)),
        {ok, Content} = file:read_file(HrlPath),
        ?assert(binary:match(Content, <<"BEAMTALK_CLASSES_HRL">>) =/= nomatch)
    after
        rm_temp_dir(Dir)
    end.

regenerate_native_class_header_idempotent_test() ->
    %% A second regeneration with the same class set must report `false` (no
    %% change) so callers don't force a needless native rebuild. No leftover
    %% .tmp files should remain.
    Dir = filename:absname(make_temp_dir()),
    try
        ?assertEqual(true, beamtalk_repl_ops_load:regenerate_native_class_header(Dir)),
        ?assertEqual(false, beamtalk_repl_ops_load:regenerate_native_class_header(Dir)),
        IncludeDir = beamtalk_repl_ops_load:native_generated_include_dir(Dir),
        {ok, Entries} = file:list_dir(IncludeDir),
        ?assertEqual(
            [], [E || E <- Entries, lists:prefix("beamtalk_classes.hrl.tmp", E)]
        )
    after
        rm_temp_dir(Dir)
    end.

native_generated_include_dir_layout_test() ->
    %% Matches the CLI's BuildLayout native_include_dir/0 location so the
    %% workspace and CLI share one generated-header directory.
    Dir = "/projects/foo",
    ?assertEqual(
        "/projects/foo/_build/dev/native/include",
        beamtalk_repl_ops_load:native_generated_include_dir(Dir)
    ).

%%====================================================================
%% compile_native_erl_files/2 — generated-header include path (BT-2653)
%%====================================================================

compile_native_uses_generated_class_header_test() ->
    %% Regression for the symptom-2 `spec for undefined function` cascade: a
    %% native module that `-include("beamtalk_classes.hrl")` must compile
    %% against the freshly generated header on the workspace path, the same way
    %% the `beamtalk test` CLI builds it.
    Dir = filename:absname(make_temp_dir()),
    try
        %% Regenerate the header into _build/dev/native/include/.
        ?assertEqual(true, beamtalk_repl_ops_load:regenerate_native_class_header(Dir)),
        NativeDir = filename:join(Dir, "native"),
        ok = file:make_dir(NativeDir),
        %% A native module that includes the generated header. Even with an
        %% empty class index the include must resolve (guard + no macros), so
        %% compilation succeeds rather than failing on a missing header.
        ErlSrc = <<
            "-module(bt_native_test_uses_header).\n"
            "-include(\"beamtalk_classes.hrl\").\n"
            "-export([ok/0]).\n"
            "ok() -> included.\n"
        >>,
        ErlPath = write_temp_file(NativeDir, "bt_native_test_uses_header.erl", ErlSrc),
        {Errors, Count} = beamtalk_repl_ops_load:compile_native_erl_files([ErlPath], Dir),
        ?assertEqual([], Errors),
        ?assertEqual(1, Count),
        ?assertEqual(included, bt_native_test_uses_header:ok())
    after
        %% Cleanup in `after` so a failed assertion can't leave the module loaded.
        code:purge(bt_native_test_uses_header),
        code:delete(bt_native_test_uses_header),
        rm_temp_dir(Dir)
    end.

%%====================================================================
%% Test-load native path: native/test/ helper end-to-end (BT-2653)
%%
%% Mirrors the beamtalk-http failure: a `.bt` test drives a test-only native
%% helper in native/test/ via `(Erlang <helper>) <msg>`. On the test-load path
%% (sync_project with include_tests=true) the helper must be discovered,
%% compiled, and code-pathed so the call resolves.
%%====================================================================

test_load_compiles_native_test_helper_test() ->
    Dir = filename:absname(make_temp_dir()),
    try
        %% A minimal package: beamtalk.toml + a native/test/ helper module.
        write_temp_file(
            Dir, "beamtalk.toml", <<"[package]\nname = \"fixture\"\n">>
        ),
        SrcDir = filename:join(Dir, "src"),
        ok = file:make_dir(SrcDir),
        NativeDir = filename:join(Dir, "native"),
        ok = file:make_dir(NativeDir),
        NativeTestDir = filename:join(NativeDir, "test"),
        ok = file:make_dir(NativeTestDir),
        HelperSrc = <<
            "-module(bt_native_test_helper_e2e).\n"
            "-export([start/0]).\n"
            "start() -> started.\n"
        >>,
        write_temp_file(NativeTestDir, "bt_native_test_helper_e2e.erl", HelperSrc),
        %% Ensure the helper is not loaded before the test-load path runs.
        code:purge(bt_native_test_helper_e2e),
        code:delete(bt_native_test_helper_e2e),
        ?assertEqual(false, code:is_loaded(bt_native_test_helper_e2e)),
        %% Run the shared test-load path (include_tests=true).
        {ok, Result} = beamtalk_repl_ops_load:sync_project(
            Dir, #{include_tests => true}
        ),
        ?assertEqual([], maps:get(errors, Result, [])),
        %% The native/test/ helper is now compiled and callable.
        ?assertNotEqual(false, code:is_loaded(bt_native_test_helper_e2e)),
        ?assertEqual(started, bt_native_test_helper_e2e:start())
    after
        %% Cleanup in `after` so a failed assertion can't leave the module loaded.
        code:purge(bt_native_test_helper_e2e),
        code:delete(bt_native_test_helper_e2e),
        rm_temp_dir(Dir)
    end.

test_load_excludes_native_test_helper_without_flag_test() ->
    %% Symmetry check: a plain load (include_tests=false) must NOT compile the
    %% native/test/ helper — those stay EUnit-only, off the runtime code path.
    Dir = filename:absname(make_temp_dir()),
    try
        write_temp_file(
            Dir, "beamtalk.toml", <<"[package]\nname = \"fixture2\"\n">>
        ),
        SrcDir = filename:join(Dir, "src"),
        ok = file:make_dir(SrcDir),
        NativeDir = filename:join(Dir, "native"),
        ok = file:make_dir(NativeDir),
        NativeTestDir = filename:join(NativeDir, "test"),
        ok = file:make_dir(NativeTestDir),
        HelperSrc = <<
            "-module(bt_native_test_helper_excluded).\n"
            "-export([start/0]).\n"
            "start() -> started.\n"
        >>,
        write_temp_file(
            NativeTestDir, "bt_native_test_helper_excluded.erl", HelperSrc
        ),
        code:purge(bt_native_test_helper_excluded),
        code:delete(bt_native_test_helper_excluded),
        {ok, _Result} = beamtalk_repl_ops_load:sync_project(
            Dir, #{include_tests => false}
        ),
        %% Helper must remain unloaded under the normal load path.
        ?assertEqual(false, code:is_loaded(bt_native_test_helper_excluded))
    after
        rm_temp_dir(Dir)
    end.

%%====================================================================
%% BT-2671: source-AST-derived class index (cold-load header parity)
%%====================================================================

read_package_name_reads_toml_test() ->
    Dir = filename:absname(make_temp_dir()),
    try
        write_temp_file(Dir, "beamtalk.toml", <<"[package]\nname = \"my_pkg\"\n">>),
        ?assertEqual(<<"my_pkg">>, beamtalk_repl_ops_load:read_package_name(Dir))
    after
        rm_temp_dir(Dir)
    end.

read_package_name_missing_manifest_test() ->
    %% No beamtalk.toml → undefined (the index builder then falls back to the
    %% live registry alone, the previous behaviour).
    Dir = filename:absname(make_temp_dir()),
    try
        ?assertEqual(undefined, beamtalk_repl_ops_load:read_package_name(Dir))
    after
        rm_temp_dir(Dir)
    end.

extract_all_bt_classes_multiple_test() ->
    %% Unlike extract_bt_class_info/1 (first class only), this returns every
    %% declared class in the file so a multi-class source contributes all of
    %% them to the cold-load index.
    Dir = filename:absname(make_temp_dir()),
    try
        Src = <<
            "Object subclass: Alpha\n\n"
            "Actor subclass: Beta\n  state: x = 0\n\n"
            "Value subclass: Gamma\n"
        >>,
        Path = write_temp_file(Dir, "multi.bt", Src),
        ?assertEqual(
            [<<"Alpha">>, <<"Beta">>, <<"Gamma">>],
            beamtalk_repl_ops_load:extract_all_bt_classes(Path)
        )
    after
        rm_temp_dir(Dir)
    end.

extract_all_bt_classes_missing_file_test() ->
    ?assertEqual([], beamtalk_repl_ops_load:extract_all_bt_classes("/nonexistent/x.bt")).

source_module_name_root_test() ->
    %% A src/-root file maps to bt@<pkg>@<snake(stem)>, the same atom the CLI's
    %% compute_relative_module produces.
    Dir = filename:absname(make_temp_dir()),
    try
        SrcDir = filename:join(Dir, "src"),
        ok = file:make_dir(SrcDir),
        Path = filename:join(SrcDir, "http_response.bt"),
        ?assertEqual(
            <<"bt@web@http_response">>,
            beamtalk_repl_ops_load:source_module_name(Path, SrcDir, <<"web">>)
        )
    after
        rm_temp_dir(Dir)
    end.

source_module_name_subdir_test() ->
    %% A subdirectory under src/ becomes an `@' segment, each snake-cased —
    %% bt@<pkg>@<dir>@<stem> — exactly as the CLI nests subdirectory modules.
    Dir = filename:absname(make_temp_dir()),
    try
        SrcDir = filename:join(Dir, "src"),
        SubDir = filename:join(SrcDir, "util"),
        ok = file:make_dir(SrcDir),
        ok = file:make_dir(SubDir),
        Path = filename:join(SubDir, "math_helper.bt"),
        ?assertEqual(
            <<"bt@web@util@math_helper">>,
            beamtalk_repl_ops_load:source_module_name(Path, SrcDir, <<"web">>)
        )
    after
        rm_temp_dir(Dir)
    end.

build_source_class_module_index_cold_test() ->
    %% The whole point of BT-2671: on a COLD load (no class registered yet) the
    %% source index already knows every class defined in src/, keyed to its
    %% package-qualified module atom.
    Dir = filename:absname(make_temp_dir()),
    try
        write_temp_file(Dir, "beamtalk.toml", <<"[package]\nname = \"coldpkg\"\n">>),
        SrcDir = filename:join(Dir, "src"),
        ok = file:make_dir(SrcDir),
        write_temp_file(SrcDir, "foo.bt", <<"Object subclass: Foo\n">>),
        write_temp_file(
            SrcDir, "bar.bt", <<"Actor subclass: Bar\n  state: x = 0\n">>
        ),
        Index = beamtalk_repl_ops_load:build_source_class_module_index(Dir),
        ?assertEqual(<<"bt@coldpkg@foo">>, maps:get(<<"Foo">>, Index)),
        ?assertEqual(<<"bt@coldpkg@bar">>, maps:get(<<"Bar">>, Index))
    after
        rm_temp_dir(Dir)
    end.

build_source_class_module_index_no_package_test() ->
    %% No package name → empty index (caller falls back to the live registry).
    Dir = filename:absname(make_temp_dir()),
    try
        SrcDir = filename:join(Dir, "src"),
        ok = file:make_dir(SrcDir),
        write_temp_file(SrcDir, "foo.bt", <<"Object subclass: Foo\n">>),
        ?assertEqual(#{}, beamtalk_repl_ops_load:build_source_class_module_index(Dir))
    after
        rm_temp_dir(Dir)
    end.

regenerate_header_includes_cold_source_class_test() ->
    %% BT-2671 acceptance: regenerate_native_class_header/1 must emit a
    %% -define for a class defined in src/ even though it is NOT registered
    %% (cold load). The previous registry-only path produced no macro for it.
    Dir = filename:absname(make_temp_dir()),
    try
        write_temp_file(Dir, "beamtalk.toml", <<"[package]\nname = \"coldhdr\"\n">>),
        SrcDir = filename:join(Dir, "src"),
        ok = file:make_dir(SrcDir),
        write_temp_file(SrcDir, "widget.bt", <<"Object subclass: Widget\n">>),
        ?assertEqual(true, beamtalk_repl_ops_load:regenerate_native_class_header(Dir)),
        HrlPath = filename:join(
            beamtalk_repl_ops_load:native_generated_include_dir(Dir),
            "beamtalk_classes.hrl"
        ),
        {ok, Content} = file:read_file(HrlPath),
        ?assert(
            binary:match(
                Content, <<"-define(BT_CLASS_MODULE_Widget, 'bt@coldhdr@widget').">>
            ) =/= nomatch
        )
    after
        rm_temp_dir(Dir)
    end.

cold_load_native_macro_compiles_test() ->
    %% End-to-end cold-load parity (acceptance criterion 1): a native module
    %% using ?BT_CLASS_MODULE_<Class> for a class defined in the SAME package
    %% being loaded must compile+resolve, with the class NEVER registered. The
    %% pre-BT-2671 registry-only header would omit the macro → compile failure
    %% (relocated symptom-2 cascade).
    Dir = filename:absname(make_temp_dir()),
    try
        write_temp_file(Dir, "beamtalk.toml", <<"[package]\nname = \"e2ecold\"\n">>),
        SrcDir = filename:join(Dir, "src"),
        ok = file:make_dir(SrcDir),
        %% A source class that is NOT registered into the live class registry.
        write_temp_file(SrcDir, "gadget.bt", <<"Object subclass: Gadget\n">>),
        %% Regenerate the header from source (cold path).
        ?assertEqual(true, beamtalk_repl_ops_load:regenerate_native_class_header(Dir)),
        NativeDir = filename:join(Dir, "native"),
        ok = file:make_dir(NativeDir),
        %% A native module that expands the macro for the same-package class.
        ErlSrc = <<
            "-module(bt_native_cold_macro).\n"
            "-include(\"beamtalk_classes.hrl\").\n"
            "-export([gadget_module/0]).\n"
            "gadget_module() -> ?BT_CLASS_MODULE_Gadget.\n"
        >>,
        ErlPath = write_temp_file(NativeDir, "bt_native_cold_macro.erl", ErlSrc),
        {Errors, Count} = beamtalk_repl_ops_load:compile_native_erl_files([ErlPath], Dir),
        ?assertEqual([], Errors),
        ?assertEqual(1, Count),
        %% The macro resolved to the package-qualified module atom.
        ?assertEqual('bt@e2ecold@gadget', bt_native_cold_macro:gadget_module())
    after
        %% Cleanup in `after` so a failed assertion can't leave the module loaded.
        code:purge(bt_native_cold_macro),
        code:delete(bt_native_cold_macro),
        rm_temp_dir(Dir)
    end.

%%====================================================================
%% BT-2671: live-registry vs CLI module-atom casing parity (gap 2)
%%
%% Loads a real class into the live class registry, then asserts the registry's
%% module_name/1 atom equals the module atom the source-AST index derives for
%% the same class. The source index uses the identical snake-case transform the
%% CLI build applies (build.rs compute_relative_module → to_module_name), so a
%% match locks the casing parity between the workspace runtime atom and the
%% CLI-generated `.beam' module name.
%%====================================================================

module_atom_casing_parity_test_() ->
    {setup, fun parity_setup/0, fun parity_teardown/1, fun parity_assert/1}.

parity_setup() ->
    application:ensure_all_started(compiler),
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    application:ensure_all_started(beamtalk_runtime),
    Tmp = unicode:characters_to_list(beamtalk_file:'tempDirectory'()),
    Proj =
        Tmp ++ "/bt_ops_parity_" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = file:make_dir(Proj),
    ok = file:write_file(
        filename:join(Proj, "beamtalk.toml"),
        <<"[package]\nname = \"paritypkg\"\n">>
    ),
    %% A workspace_meta with a real project_path so compute_package_module_name/1
    %% derives the package-qualified module atom on load.
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        MetaPid -> gen_server:stop(MetaPid)
    end,
    {ok, _} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"ops_parity_ws">>,
        project_path => list_to_binary(Proj),
        created_at => erlang:system_time(second)
    }),
    Proj.

parity_teardown(Proj) ->
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        MetaPid -> gen_server:stop(MetaPid)
    end,
    %% NB: only stop beamtalk_compiler here. Do NOT stop beamtalk_runtime — it is
    %% a shared OTP application other tests in this EUnit node rely on being up
    %% (e.g. beamtalk_repl_server_tests' ranch listener, beamtalk_workspace_sup_tests).
    %% `parity_setup` uses ensure_all_started, so when it was already running we
    %% must leave it running; stopping it here triggers a `noproc`/`already_started`
    %% cascade in sibling tests.
    _ = application:stop(beamtalk_compiler),
    rm_temp_dir(Proj),
    ok.

parity_assert(Proj) ->
    %% A multi-word class name exercises the snake_case transform (the place
    %% casing could diverge between the runtime atom and the CLI-built module
    %% name). Load it into the live registry under src/.
    SrcDir = filename:join(Proj, "src"),
    ok = filelib:ensure_dir(filename:join(SrcDir, "anchor")),
    Path = filename:join(SrcDir, "HttpResponse.bt"),
    ok = file:write_file(
        Path, <<"Object subclass: HttpResponse\n\n  ok -> Boolean => true\n">>
    ),
    State0 = beamtalk_repl_state:new(undefined, 0),
    {ok, _, _State1} = beamtalk_repl_loader:handle_load(Path, State0),
    %% The atom the live registry records for the loaded class.
    RegistryPid = beamtalk_class_registry:whereis_class('HttpResponse'),
    LiveModule = beamtalk_object_class:module_name(RegistryPid),
    %% The atom the source-AST index derives — built the same way the CLI does
    %% (build.rs compute_relative_module → to_module_name), so this binary is
    %% byte-identical to the CLI-generated `.beam' module name.
    SourceIndex = beamtalk_repl_ops_load:build_source_class_module_index(Proj),
    SourceModuleBin = maps:get(<<"HttpResponse">>, SourceIndex),
    %% Compare the casing-sensitive class-name segment (the last `@'-part) of the
    %% live atom against the source/CLI atom. The package prefix can legitimately
    %% differ when the live workspace-meta singleton (shared across the EUnit
    %% node) resolved a stem-only module name, but the *casing* of the class
    %% segment — the only thing this parity check guards — must always agree.
    LiveSegment = last_module_segment(atom_to_binary(LiveModule, utf8)),
    SourceSegment = last_module_segment(SourceModuleBin),
    [
        %% Casing parity: the runtime atom and the CLI-formula atom snake-case
        %% the class name identically.
        ?_assertEqual(<<"http_response">>, SourceSegment),
        ?_assertEqual(LiveSegment, SourceSegment),
        %% The source/CLI atom is the expected snake-cased, package-qualified one.
        ?_assertEqual(<<"bt@paritypkg@http_response">>, SourceModuleBin)
    ].

%% Last `@'-delimited segment of a `bt@…@class' module-name binary.
last_module_segment(ModuleBin) ->
    lists:last(binary:split(ModuleBin, <<"@">>, [global])).

%%====================================================================
%% save-native-source (BT-2670)
%%
%% Edit -> compile -> reload -> write-back for a *project-owned* native `.erl`.
%% These set up a real on-disk project (beamtalk.toml + native/<mod>.erl),
%% compile + load the module so its `module_info(compile)` source resolves (the
%% server-side ownership re-derivation reads it), then drive the op via the
%% protocol-shaped `handle_term/4` entry point. With no workspace-meta singleton
%% set, a freshly-compiled temp native classifies as `project` (the path-origin
%% fallback) — exactly the editable case. Each test purges its module and removes
%% its temp dir in an `after` block so reuse never pollutes a later run.
%%====================================================================

%% Create a temp project: <Dir>/beamtalk.toml + <Dir>/native/<Mod>.erl, compile
%% the module from that path (so its compile-info `source` is the project path)
%% and load it. Returns {AbsErlPath, Module}.
setup_project_native(ModName, Src) ->
    Dir = make_temp_dir(),
    AbsDir = filename:absname(Dir),
    ok = file:write_file(
        filename:join(AbsDir, "beamtalk.toml"), <<"[package]\nname = \"proj\"\n">>
    ),
    NativeDir = filename:join(AbsDir, "native"),
    ok = file:make_dir(NativeDir),
    FileName = atom_to_list(ModName) ++ ".erl",
    ErlPath = filename:join(NativeDir, FileName),
    ok = file:write_file(ErlPath, Src),
    {ok, ModName, Bin} = compile:file(ErlPath, [debug_info, binary, return_errors]),
    {module, ModName} = code:load_binary(ModName, ErlPath, Bin),
    {Dir, ErlPath, ModName}.

teardown_project_native(Dir, ModName) ->
    code:purge(ModName),
    code:delete(ModName),
    rm_temp_dir(Dir).

native_module_editable_target_project_test() ->
    Mod = bt_native_save_editable_mod,
    Src = <<
        "-module(bt_native_save_editable_mod).\n"
        "-export([go/0]).\n"
        "go() -> ok.\n"
    >>,
    {Dir, ErlPath, _} = setup_project_native(Mod, Src),
    try
        %% A project-owned, loaded native with readable source is editable.
        ?assertEqual(
            {ok, ErlPath},
            beamtalk_repl_ops_browse:native_module_editable_target(Mod)
        )
    after
        teardown_project_native(Dir, Mod)
    end.

native_module_editable_target_unknown_test() ->
    %% A module that is not even loaded has no compile-info source → not editable.
    ?assertEqual(
        {error, not_editable},
        beamtalk_repl_ops_browse:native_module_editable_target(
            bt_native_save_definitely_not_loaded_xyz
        )
    ).

save_native_source_clean_compile_test() ->
    Mod = bt_native_save_clean_mod,
    Src = <<
        "-module(bt_native_save_clean_mod).\n"
        "-export([go/0]).\n"
        "go() -> v1.\n"
    >>,
    {Dir, ErlPath, _} = setup_project_native(Mod, Src),
    try
        ?assertEqual(v1, bt_native_save_clean_mod:go()),
        NewSrc = <<
            "-module(bt_native_save_clean_mod).\n"
            "-export([go/0]).\n"
            "go() -> v2.\n"
        >>,
        Params = #{
            <<"module">> => <<"bt_native_save_clean_mod">>,
            <<"source">> => NewSrc
        },
        Result = beamtalk_repl_ops_load:handle_term(
            <<"save-native-source">>, Params, undefined, self()
        ),
        ?assertMatch({value, #{<<"ok">> := true}}, Result),
        %% The module was reloaded into the live VM (new code runs).
        ?assertEqual(v2, bt_native_save_clean_mod:go()),
        %% The source was written to disk.
        ?assertEqual({ok, NewSrc}, file:read_file(ErlPath))
    after
        teardown_project_native(Dir, Mod)
    end.

save_native_source_second_save_targets_real_file_test() ->
    %% Regression: after the first save, the validation compile must NOT leave the
    %% module's compile-info `source` pointing at the (deleted) temp `.erl`.
    %% Otherwise the SECOND save re-derives the editable target as the temp path
    %% and writes there, leaving the real `.erl` stale. Two consecutive saves must
    %% both land on the real file.
    Mod = bt_native_save_twice_mod,
    Src = <<
        "-module(bt_native_save_twice_mod).\n"
        "-export([go/0]).\n"
        "go() -> v1.\n"
    >>,
    {Dir, ErlPath, _} = setup_project_native(Mod, Src),
    try
        Save = fun(NewSrc) ->
            Params = #{
                <<"module">> => <<"bt_native_save_twice_mod">>,
                <<"source">> => NewSrc
            },
            beamtalk_repl_ops_load:handle_term(
                <<"save-native-source">>, Params, undefined, self()
            )
        end,
        Src2 = <<
            "-module(bt_native_save_twice_mod).\n"
            "-export([go/0]).\n"
            "go() -> v2.\n"
        >>,
        ?assertMatch({value, #{<<"ok">> := true}}, Save(Src2)),
        ?assertEqual(v2, bt_native_save_twice_mod:go()),
        ?assertEqual({ok, Src2}, file:read_file(ErlPath)),
        %% After the first save, the editable target must still be the REAL file
        %% (not a deleted temp) — this is the property the bug violated.
        ?assertEqual(
            {ok, ErlPath},
            beamtalk_repl_ops_browse:native_module_editable_target(Mod)
        ),
        %% The second save must also write the real file and reload the module.
        Src3 = <<
            "-module(bt_native_save_twice_mod).\n"
            "-export([go/0]).\n"
            "go() -> v3.\n"
        >>,
        ?assertMatch({value, #{<<"ok">> := true}}, Save(Src3)),
        ?assertEqual(v3, bt_native_save_twice_mod:go()),
        ?assertEqual({ok, Src3}, file:read_file(ErlPath)),
        %% No orphaned validation-temp file left behind.
        ?assertEqual(false, filelib:is_regular(ErlPath ++ ".bt_native_save_tmp.erl"))
    after
        teardown_project_native(Dir, Mod)
    end.

save_native_source_compile_error_leaves_disk_untouched_test() ->
    Mod = bt_native_save_err_mod,
    Src = <<
        "-module(bt_native_save_err_mod).\n"
        "-export([go/0]).\n"
        "go() -> ok.\n"
    >>,
    {Dir, ErlPath, _} = setup_project_native(Mod, Src),
    try
        %% A syntactically broken edit.
        BadSrc = <<
            "-module(bt_native_save_err_mod).\n"
            "-export([go/0]).\n"
            "go( -> ok.\n"
        >>,
        Params = #{
            <<"module">> => <<"bt_native_save_err_mod">>,
            <<"source">> => BadSrc
        },
        Result = beamtalk_repl_ops_load:handle_term(
            <<"save-native-source">>, Params, undefined, self()
        ),
        %% Structured compile errors, not a crash.
        ?assertMatch({value, #{<<"errors">> := [_ | _]}}, Result),
        {value, #{<<"errors">> := [ErrMap | _]}} = Result,
        ?assertEqual(<<"compile_error">>, maps:get(<<"kind">>, ErrMap)),
        %% The error is retargeted at the real `.erl` path (not the temp file).
        ?assertEqual(list_to_binary(ErlPath), maps:get(<<"path">>, ErrMap)),
        %% Fail-safe: the on-disk source is untouched and the live module still
        %% runs the original code.
        ?assertEqual({ok, Src}, file:read_file(ErlPath)),
        ?assertEqual(ok, bt_native_save_err_mod:go())
    after
        teardown_project_native(Dir, Mod)
    end.

save_native_source_rejects_unknown_module_test() ->
    %% A module the workspace never loaded is rejected read-only (no atom / no
    %% project source), as a structured #beamtalk_error{}, never a disk write.
    Params = #{
        <<"module">> => <<"bt_native_save_unknown_module_qwerty">>,
        <<"source">> => <<"-module(x).\n">>
    },
    Result = beamtalk_repl_ops_load:handle_term(
        <<"save-native-source">>, Params, undefined, self()
    ),
    ?assertMatch({error, _}, Result).

save_native_source_requires_module_test() ->
    ?assertMatch(
        {error, _},
        beamtalk_repl_ops_load:handle_term(
            <<"save-native-source">>,
            #{<<"module">> => <<>>, <<"source">> => <<"x">>},
            undefined,
            self()
        )
    ).

save_native_source_requires_source_test() ->
    ?assertMatch(
        {error, _},
        beamtalk_repl_ops_load:handle_term(
            <<"save-native-source">>,
            #{<<"module">> => <<"bt_native_save_clean_mod">>, <<"source">> => <<>>},
            undefined,
            self()
        )
    ).

%%====================================================================
%% ADR 0098 Phase 4 — build-artifact provenance on workspace attach
%%====================================================================

%% A schema this reader doesn't understand → stale (rebuild), never an error.
provenance_unknown_schema_is_stale_test() ->
    Stamp = #{
        <<"schema">> => 999,
        <<"beamtalk_version">> => <<"9.9.9">>,
        <<"otp_release">> => beamtalk_repl_ops_load:current_otp_release()
    },
    ?assertMatch({stale, _}, beamtalk_repl_ops_load:stamp_matches_current(Stamp)).

%% A stamp built for a different OTP release → stale even if the version matches.
provenance_otp_mismatch_is_stale_test() ->
    Stamp = #{
        <<"schema">> => 1,
        <<"beamtalk_version">> => stamp_version(),
        <<"otp_release">> => <<"1-0.0">>
    },
    ?assertMatch({stale, _}, beamtalk_repl_ops_load:stamp_matches_current(Stamp)).

%% A stamp with no OTP key → stale (the older-toolchain case).
provenance_missing_otp_key_is_stale_test() ->
    Stamp = #{<<"schema">> => 1, <<"beamtalk_version">> => stamp_version()},
    ?assertMatch({stale, _}, beamtalk_repl_ops_load:stamp_matches_current(Stamp)).

%% A stamp matching the running toolchain → fresh.
provenance_matching_stamp_is_fresh_test() ->
    Stamp = #{
        <<"schema">> => 1,
        <<"beamtalk_version">> => stamp_version(),
        <<"otp_release">> => beamtalk_repl_ops_load:current_otp_release()
    },
    ?assertEqual(fresh, beamtalk_repl_ops_load:stamp_matches_current(Stamp)).

%% Version mismatch → stale. Only assert when the compiler port can report a
%% version; otherwise the version axis is not testable and the OTP axis covers
%% staleness (the compiler server is not started in this EUnit context).
provenance_version_mismatch_is_stale_test() ->
    case safe_version() of
        undefined ->
            ok;
        _ ->
            Stamp = #{
                <<"schema">> => 1,
                <<"beamtalk_version">> => <<"0.0.0-not-current">>,
                <<"otp_release">> => beamtalk_repl_ops_load:current_otp_release()
            },
            ?assertMatch({stale, _}, beamtalk_repl_ops_load:stamp_matches_current(Stamp))
    end.

provenance_read_stamp_round_trip_test() ->
    Dir = make_temp_dir(),
    try
        StampPath = filename:join(Dir, ".beamtalk-stamp.json"),
        ok = file:write_file(StampPath, <<"{\"schema\":1,\"beamtalk_version\":\"x\"}">>),
        ?assertMatch(
            {ok, #{<<"schema">> := 1}}, beamtalk_repl_ops_load:read_provenance_stamp(StampPath)
        )
    after
        rm_temp_dir(Dir)
    end.

provenance_read_corrupt_stamp_is_error_test() ->
    Dir = make_temp_dir(),
    try
        StampPath = filename:join(Dir, ".beamtalk-stamp.json"),
        ok = file:write_file(StampPath, <<"{ not json">>),
        ?assertMatch({error, _}, beamtalk_repl_ops_load:read_provenance_stamp(StampPath))
    after
        rm_temp_dir(Dir)
    end.

provenance_read_missing_stamp_is_error_test() ->
    ?assertMatch(
        {error, _},
        beamtalk_repl_ops_load:read_provenance_stamp("does/not/exist/.beamtalk-stamp.json")
    ).

%% A project with no stamp is NOT forced: the workspace recompiles project
%% sources from scratch on attach anyway, so there is nothing on-disk to
%% invalidate — and it must never fabricate a stamp the CLI's Phase-1 gate would
%% trust (the workspace compiles in-memory and never writes _build/dev/ebin).
provenance_missing_stamp_does_not_force_test() ->
    Dir = make_temp_dir(),
    try
        ?assertEqual(false, beamtalk_repl_ops_load:project_provenance_stale(Dir))
    after
        rm_temp_dir(Dir)
    end.

%% A project whose on-disk stamp matches the running toolchain is fresh; once the
%% stamp's OTP is tampered (an older-toolchain artifact), the next attach is
%% stale → recompile. (The full compile cycle is exercised end-to-end by the
%% repl-protocol `sync_project` case, which needs the compiler port.)
provenance_stale_stamp_flips_attach_decision_test() ->
    Dir = filename:absname(make_temp_dir()),
    try
        ProfileDir = filename:join([Dir, "_build", "dev"]),
        ok = filelib:ensure_dir(filename:join(ProfileDir, ".keep")),
        StampPath = filename:join(ProfileDir, ".beamtalk-stamp.json"),

        Fresh = #{
            <<"schema">> => 1,
            <<"beamtalk_version">> => stamp_version(),
            <<"otp_release">> => beamtalk_repl_ops_load:current_otp_release()
        },
        ok = file:write_file(StampPath, iolist_to_binary(json:encode(Fresh))),
        ?assertEqual(false, beamtalk_repl_ops_load:project_provenance_stale(Dir)),

        Tampered = Fresh#{<<"otp_release">> => <<"1-0.0">>},
        ok = file:write_file(StampPath, iolist_to_binary(json:encode(Tampered))),
        ?assertEqual(true, beamtalk_repl_ops_load:project_provenance_stale(Dir))
    after
        rm_temp_dir(Dir)
    end.

%% A stamp present but missing the beamtalk_version key is foreign/corrupt →
%% stale (fail toward rebuild), even if OTP matches and the port is down.
provenance_missing_version_key_is_stale_test() ->
    Stamp = #{
        <<"schema">> => 1,
        <<"otp_release">> => beamtalk_repl_ops_load:current_otp_release()
    },
    ?assertMatch({stale, _}, beamtalk_repl_ops_load:stamp_matches_current(Stamp)).

%% The running OTP version is the compound `<release>-<erts>` key.
provenance_current_otp_release_is_compound_test() ->
    Otp = beamtalk_repl_ops_load:current_otp_release(),
    ?assert(is_binary(Otp)),
    ?assertMatch([_Rel, _Erts], binary:split(Otp, <<"-">>)).

%% The compiler port's version, or `undefined` if the server isn't running
%% (the compiler app is not started in this EUnit context). Crash-safe.
safe_version() ->
    try beamtalk_compiler_server:version() of
        {ok, V} when is_binary(V) -> V;
        _ -> undefined
    catch
        _:_ -> undefined
    end.

%% A binary version for stamps that should pass the version axis: the real
%% running version when the port is up (so it matches), or a placeholder when
%% it's down (the version axis is skipped, so the value is irrelevant). Always a
%% binary, so `json:encode/1` never sees the `undefined` atom.
stamp_version() ->
    case safe_version() of
        V when is_binary(V) -> V;
        undefined -> <<"0.0.0-test-placeholder">>
    end.
