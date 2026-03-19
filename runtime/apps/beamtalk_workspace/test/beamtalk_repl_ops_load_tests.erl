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

%% Delete a temp directory and all its contents (files only — tests do not
%% create nested subdirectories, so recursive removal is not needed here).
rm_temp_dir(Dir) ->
    {ok, Entries} = file:list_dir(Dir),
    lists:foreach(fun(F) -> file:delete(filename:join(Dir, F)) end, Entries),
    file:del_dir(Dir).

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
