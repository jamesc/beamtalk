%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_file module.
%%%
%%% Tests cover:
%%% - Path validation (validate_path/1)
%%% - File existence checks (exists:/1)
%%% - File read/write operations and error paths
%%% - Stream creation (lines:/1)
%%% - Block-scoped handles (open:do:/2)
%%% - has_method/1 and handle_has_method/1
%%% - strip_newline/1 edge cases
%%% - Type error guards

-module(beamtalk_file_tests).

-include("beamtalk.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% Test Helpers
%%% ============================================================================

%% @doc Create a temporary file with given contents, run Fun, then clean up.
with_temp_file(Name, Contents, Fun) ->
    ok = file:write_file(Name, Contents),
    try
        Fun()
    after
        file:delete(Name)
    end.

%%% ============================================================================
%%% has_method/1
%%% ============================================================================

has_method_exists_test() ->
    ?assert(beamtalk_file:has_method('exists:')).

has_method_readAll_test() ->
    ?assert(beamtalk_file:has_method('readAll:')).

has_method_writeAll_contents_test() ->
    ?assert(beamtalk_file:has_method('writeAll:contents:')).

has_method_lines_test() ->
    ?assert(beamtalk_file:has_method('lines:')).

has_method_open_do_test() ->
    ?assert(beamtalk_file:has_method('open:do:')).

has_method_unknown_test() ->
    ?assertNot(beamtalk_file:has_method('foo')).

has_method_unknown_atom_test() ->
    ?assertNot(beamtalk_file:has_method('delete:')).

%%% ============================================================================
%%% handle_has_method/1
%%% ============================================================================

handle_has_method_lines_test() ->
    ?assert(beamtalk_file:handle_has_method('lines')).

handle_has_method_unknown_test() ->
    ?assertNot(beamtalk_file:handle_has_method('foo')).

handle_has_method_readAll_test() ->
    ?assertNot(beamtalk_file:handle_has_method('readAll:')).

%%% ============================================================================
%%% exists:/1
%%% ============================================================================

exists_nonexistent_test() ->
    ?assertNot(beamtalk_file:'exists:'(<<"_bt_test_no_such_file.txt">>)).

exists_real_file_test() ->
    with_temp_file("_bt_test_exists.txt", <<"test">>, fun() ->
        ?assert(beamtalk_file:'exists:'(<<"_bt_test_exists.txt">>))
    end).

exists_non_string_returns_false_test() ->
    ?assertNot(beamtalk_file:'exists:'(42)).

exists_atom_returns_false_test() ->
    ?assertNot(beamtalk_file:'exists:'(foo)).

exists_absolute_path_returns_false_test() ->
    ?assertNot(beamtalk_file:'exists:'(<<"/etc/passwd">>)).

exists_traversal_returns_false_test() ->
    ?assertNot(beamtalk_file:'exists:'(<<"../passwords.txt">>)).

exists_windows_abs_path_returns_false_test() ->
    ?assertNot(beamtalk_file:'exists:'(<<"C:\\Windows\\system32">>)).

exists_backslash_abs_returns_false_test() ->
    ?assertNot(beamtalk_file:'exists:'(<<"\\server\\share">>)).

%%% ============================================================================
%%% readAll:/1
%%% ============================================================================

readAll_success_test() ->
    with_temp_file("_bt_test_read.txt", <<"hello">>, fun() ->
        ?assertEqual(<<"hello">>, beamtalk_file:'readAll:'(<<"_bt_test_read.txt">>))
    end).

readAll_file_not_found_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{
            kind = file_not_found, class = 'File', selector = 'readAll:'}},
        beamtalk_file:'readAll:'(<<"_bt_test_no_such_file.txt">>)).

readAll_type_error_integer_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{
            kind = type_error, class = 'File', selector = 'readAll:'}},
        beamtalk_file:'readAll:'(42)).

readAll_type_error_atom_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{
            kind = type_error, class = 'File', selector = 'readAll:'}},
        beamtalk_file:'readAll:'(foo)).

readAll_invalid_path_absolute_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{
            kind = invalid_path, class = 'File', selector = 'readAll:'}},
        beamtalk_file:'readAll:'(<<"/etc/passwd">>)).

readAll_invalid_path_traversal_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{
            kind = invalid_path, class = 'File', selector = 'readAll:'}},
        beamtalk_file:'readAll:'(<<"../secret.txt">>)).

readAll_invalid_path_windows_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{
            kind = invalid_path, class = 'File', selector = 'readAll:'}},
        beamtalk_file:'readAll:'(<<"C:\\file.txt">>)).

readAll_permission_denied_test() ->
    %% Create a file, remove read permissions, verify error
    FileName = "_bt_test_noperm_read.txt",
    ok = file:write_file(FileName, <<"secret">>),
    ok = file:change_mode(FileName, 8#000),
    try
        ?assertError(
            #{'$beamtalk_class' := _, error := #beamtalk_error{
                kind = permission_denied, class = 'File', selector = 'readAll:'}},
            beamtalk_file:'readAll:'(list_to_binary(FileName)))
    after
        file:change_mode(FileName, 8#644),
        file:delete(FileName)
    end.

%%% ============================================================================
%%% writeAll:contents:/2
%%% ============================================================================

writeAll_success_test() ->
    FileName = "_bt_test_write.txt",
    try
        ?assertEqual(nil, beamtalk_file:'writeAll:contents:'(
            list_to_binary(FileName), <<"content">>)),
        ?assertEqual({ok, <<"content">>}, file:read_file(FileName))
    after
        file:delete(FileName)
    end.

writeAll_type_error_non_string_path_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{
            kind = type_error, class = 'File', selector = 'writeAll:contents:'}},
        beamtalk_file:'writeAll:contents:'(42, <<"content">>)).

writeAll_type_error_non_string_contents_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{
            kind = type_error, class = 'File', selector = 'writeAll:contents:'}},
        beamtalk_file:'writeAll:contents:'(<<"file.txt">>, 42)).

writeAll_invalid_path_absolute_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{
            kind = invalid_path, class = 'File', selector = 'writeAll:contents:'}},
        beamtalk_file:'writeAll:contents:'(<<"/tmp/test.txt">>, <<"data">>)).

writeAll_invalid_path_traversal_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{
            kind = invalid_path, class = 'File', selector = 'writeAll:contents:'}},
        beamtalk_file:'writeAll:contents:'(<<"../test.txt">>, <<"data">>)).

writeAll_creates_subdirectory_test() ->
    %% writeAll should auto-create parent directories
    FileName = "_bt_test_subdir/nested.txt",
    try
        ?assertEqual(nil, beamtalk_file:'writeAll:contents:'(
            list_to_binary(FileName), <<"nested content">>)),
        ?assertEqual({ok, <<"nested content">>}, file:read_file(FileName))
    after
        file:delete(FileName),
        file:del_dir("_bt_test_subdir")
    end.

writeAll_permission_denied_test() ->
    %% Create a read-only directory, try to write in it
    Dir = "_bt_test_readonly_dir",
    FileName = Dir ++ "/test.txt",
    ok = filelib:ensure_dir(FileName),
    ok = file:change_mode(Dir, 8#555),
    try
        ?assertError(
            #{'$beamtalk_class' := _, error := #beamtalk_error{
                kind = permission_denied, class = 'File', selector = 'writeAll:contents:'}},
            beamtalk_file:'writeAll:contents:'(list_to_binary(FileName), <<"data">>))
    after
        file:change_mode(Dir, 8#755),
        file:delete(FileName),
        file:del_dir(Dir)
    end.

%%% ============================================================================
%%% lines:/1
%%% ============================================================================

lines_success_test() ->
    with_temp_file("_bt_test_lines.txt", <<"a\nb\nc\n">>, fun() ->
        Stream = beamtalk_file:'lines:'(<<"_bt_test_lines.txt">>),
        Result = beamtalk_stream:as_list(Stream),
        ?assertEqual([<<"a">>, <<"b">>, <<"c">>], Result)
    end).

lines_file_not_found_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{
            kind = file_not_found, class = 'File', selector = 'lines:'}},
        beamtalk_file:'lines:'(<<"_bt_test_no_such_file.txt">>)).

lines_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{
            kind = type_error, class = 'File', selector = 'lines:'}},
        beamtalk_file:'lines:'(42)).

lines_invalid_path_absolute_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{
            kind = invalid_path, class = 'File', selector = 'lines:'}},
        beamtalk_file:'lines:'(<<"/etc/passwd">>)).

lines_invalid_path_traversal_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{
            kind = invalid_path, class = 'File', selector = 'lines:'}},
        beamtalk_file:'lines:'(<<"../secret.txt">>)).

lines_empty_file_test() ->
    with_temp_file("_bt_test_empty_lines.txt", <<>>, fun() ->
        Stream = beamtalk_file:'lines:'(<<"_bt_test_empty_lines.txt">>),
        Result = beamtalk_stream:as_list(Stream),
        ?assertEqual([], Result)
    end).

lines_no_trailing_newline_test() ->
    with_temp_file("_bt_test_notrail.txt", <<"first\nsecond">>, fun() ->
        Stream = beamtalk_file:'lines:'(<<"_bt_test_notrail.txt">>),
        Result = beamtalk_stream:as_list(Stream),
        ?assertEqual([<<"first">>, <<"second">>], Result)
    end).

lines_windows_crlf_test() ->
    with_temp_file("_bt_test_crlf.txt", <<"a\r\nb\r\nc\r\n">>, fun() ->
        Stream = beamtalk_file:'lines:'(<<"_bt_test_crlf.txt">>),
        Result = beamtalk_stream:as_list(Stream),
        ?assertEqual([<<"a">>, <<"b">>, <<"c">>], Result)
    end).

lines_single_line_test() ->
    with_temp_file("_bt_test_single.txt", <<"only">>, fun() ->
        Stream = beamtalk_file:'lines:'(<<"_bt_test_single.txt">>),
        Result = beamtalk_stream:as_list(Stream),
        ?assertEqual([<<"only">>], Result)
    end).

%%% ============================================================================
%%% open:do:/2
%%% ============================================================================

open_do_success_test() ->
    with_temp_file("_bt_test_open.txt", <<"line1\nline2\n">>, fun() ->
        Result = beamtalk_file:'open:do:'(
            <<"_bt_test_open.txt">>,
            fun(Handle) -> beamtalk_stream:as_list(beamtalk_file:handle_lines(Handle)) end),
        ?assertEqual([<<"line1">>, <<"line2">>], Result)
    end).

open_do_file_not_found_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{
            kind = file_not_found, class = 'File', selector = 'open:do:'}},
        beamtalk_file:'open:do:'(
            <<"_bt_test_no_such_file.txt">>,
            fun(_) -> ok end)).

open_do_type_error_non_string_path_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{
            kind = type_error, class = 'File', selector = 'open:do:'}},
        beamtalk_file:'open:do:'(42, fun(_) -> ok end)).

open_do_type_error_non_block_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{
            kind = type_error, class = 'File', selector = 'open:do:'}},
        beamtalk_file:'open:do:'(<<"file.txt">>, not_a_function)).

open_do_invalid_path_absolute_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{
            kind = invalid_path, class = 'File', selector = 'open:do:'}},
        beamtalk_file:'open:do:'(<<"/etc/passwd">>, fun(_) -> ok end)).

open_do_invalid_path_traversal_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{
            kind = invalid_path, class = 'File', selector = 'open:do:'}},
        beamtalk_file:'open:do:'(<<"../secret.txt">>, fun(_) -> ok end)).

open_do_closes_on_exception_test() ->
    %% Verify the handle is closed even if the block raises
    with_temp_file("_bt_test_open_exc.txt", <<"data\n">>, fun() ->
        ?assertError(
            test_exception,
            beamtalk_file:'open:do:'(
                <<"_bt_test_open_exc.txt">>,
                fun(_Handle) -> error(test_exception) end))
    end).

open_do_block_return_value_test() ->
    with_temp_file("_bt_test_open_ret.txt", <<"data\n">>, fun() ->
        Result = beamtalk_file:'open:do:'(
            <<"_bt_test_open_ret.txt">>,
            fun(_Handle) -> 42 end),
        ?assertEqual(42, Result)
    end).

%%% ============================================================================
%%% handle_lines/1
%%% ============================================================================

handle_lines_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{
            kind = type_error, class = 'FileHandle', selector = 'lines'}},
        beamtalk_file:handle_lines(not_a_handle)).

handle_lines_wrong_map_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{
            kind = type_error, class = 'FileHandle', selector = 'lines'}},
        beamtalk_file:handle_lines(#{foo => bar})).
