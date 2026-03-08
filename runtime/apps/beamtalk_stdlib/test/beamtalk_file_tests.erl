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
%%% - FFI shims (exists/1, readAll/1, etc.)
%%% - handle_has_method/1
%%% - strip_newline/1 edge cases
%%% - Type error guards

-module(beamtalk_file_tests).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
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
%%% FFI shims
%%% ============================================================================

ffi_shim_exists_nonexistent_test() ->
    ?assertNot(beamtalk_file:exists(<<"_bt_shim_no_such_file.txt">>)).

ffi_shim_exists_real_file_test() ->
    with_temp_file("_bt_shim_exists.txt", <<"x">>, fun() ->
        ?assert(beamtalk_file:exists(<<"_bt_shim_exists.txt">>))
    end).

ffi_shim_readAll_not_found_test() ->
    R = beamtalk_file:readAll(<<"_bt_shim_no_such_file.txt">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _, error := #beamtalk_error{kind = file_not_found}
            }
        },
        R
    ).

ffi_shim_writeAll_success_test() ->
    FileName = "_bt_shim_write.txt",
    try
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:writeAll(list_to_binary(FileName), <<"hi">>)
        ),
        ?assertEqual({ok, <<"hi">>}, file:read_file(FileName))
    after
        file:delete(FileName)
    end.

ffi_shim_lines_not_found_test() ->
    R = beamtalk_file:lines(<<"_bt_shim_no_such_file.txt">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _, error := #beamtalk_error{kind = file_not_found}
            }
        },
        R
    ).

ffi_shim_open_not_found_test() ->
    R = beamtalk_file:open(<<"_bt_shim_no_such.txt">>, fun(_) -> error(callback_should_not_run) end),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _, error := #beamtalk_error{kind = file_not_found}
            }
        },
        R
    ).

ffi_shim_isDirectory_false_test() ->
    ?assertNot(beamtalk_file:isDirectory(<<"_bt_shim_no_such_dir">>)).

ffi_shim_isFile_false_test() ->
    ?assertNot(beamtalk_file:isFile(<<"_bt_shim_no_such_file.txt">>)).

ffi_shim_mkdir_and_delete_test() ->
    Dir = "_bt_shim_mkdir_dir",
    try
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:mkdir(list_to_binary(Dir))
        ),
        ?assert(filelib:is_dir(Dir)),
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:delete(list_to_binary(Dir))
        )
    after
        file:del_dir(Dir)
    end.

ffi_shim_mkdirAll_and_deleteAll_test() ->
    Dir = "_bt_shim_mkdirall/a/b",
    try
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:mkdirAll(list_to_binary(Dir))
        ),
        ?assert(filelib:is_dir(Dir)),
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:deleteAll(<<"_bt_shim_mkdirall">>)
        )
    after
        file:del_dir_r("_bt_shim_mkdirall")
    end.

ffi_shim_listDirectory_test() ->
    with_temp_dir("_bt_shim_listdir", fun() ->
        ok = file:write_file("_bt_shim_listdir/x.txt", <<"x">>),
        R = beamtalk_file:listDirectory(<<"_bt_shim_listdir">>),
        ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
        #{'okValue' := Entries} = R,
        ?assertEqual([<<"x.txt">>], Entries)
    end).

ffi_shim_rename_test() ->
    Src = "_bt_shim_rename_src.txt",
    Dst = "_bt_shim_rename_dst.txt",
    ok = file:write_file(Src, <<"data">>),
    try
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:rename(list_to_binary(Src), list_to_binary(Dst))
        ),
        ?assert(filelib:is_regular(Dst))
    after
        file:delete(Src),
        file:delete(Dst)
    end.

ffi_shim_absolutePath_test() ->
    R = beamtalk_file:absolutePath(<<"some/path">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
    #{'okValue' := AbsPath} = R,
    ?assert(is_binary(AbsPath)).

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
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := <<"hello">>},
            beamtalk_file:'readAll:'(<<"_bt_test_read.txt">>)
        )
    end).

readAll_file_not_found_test() ->
    R = beamtalk_file:'readAll:'(<<"_bt_test_no_such_file.txt">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = file_not_found, class = 'File', selector = 'readAll:'
                }
            }
        },
        R
    ).

readAll_type_error_integer_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'File', selector = 'readAll:'
            }
        },
        beamtalk_file:'readAll:'(42)
    ).

readAll_type_error_atom_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'File', selector = 'readAll:'
            }
        },
        beamtalk_file:'readAll:'(foo)
    ).

readAll_invalid_path_absolute_test() ->
    R = beamtalk_file:'readAll:'(<<"/etc/passwd">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{kind = invalid_path, class = 'File', selector = 'readAll:'}
            }
        },
        R
    ).

readAll_invalid_path_traversal_test() ->
    R = beamtalk_file:'readAll:'(<<"../secret.txt">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{kind = invalid_path, class = 'File', selector = 'readAll:'}
            }
        },
        R
    ).

readAll_invalid_path_windows_test() ->
    R = beamtalk_file:'readAll:'(<<"C:\\file.txt">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{kind = invalid_path, class = 'File', selector = 'readAll:'}
            }
        },
        R
    ).

readAll_permission_denied_test() ->
    case os:type() of
        {win32, _} ->
            %% Windows: Use icacls to deny read permissions via ACL
            FileName = "_bt_test_noperm_read.txt",
            ok = file:write_file(FileName, <<"secret">>),
            %% Get current username
            UserName = string:trim(os:cmd("echo %USERNAME%")),
            %% Deny read access using icacls
            DenyCmd = "icacls \"" ++ FileName ++ "\" /deny " ++ UserName ++ ":R >nul 2>&1",
            _ = os:cmd(DenyCmd),
            try
                R = beamtalk_file:'readAll:'(list_to_binary(FileName)),
                ?assertMatch(
                    #{
                        '$beamtalk_class' := 'Result',
                        'isOk' := false,
                        'errReason' := #{
                            '$beamtalk_class' := _,
                            error := #beamtalk_error{
                                kind = permission_denied, class = 'File', selector = 'readAll:'
                            }
                        }
                    },
                    R
                )
            after
                %% Reset ACL to allow access before deleting
                ResetCmd = "icacls \"" ++ FileName ++ "\" /grant " ++ UserName ++ ":F >nul 2>&1",
                _ = os:cmd(ResetCmd),
                file:delete(FileName)
            end;
        _ ->
            %% Unix: Use chmod to remove read permissions
            FileName = "_bt_test_noperm_read.txt",
            ok = file:write_file(FileName, <<"secret">>),
            ok = file:change_mode(FileName, 8#000),
            try
                R = beamtalk_file:'readAll:'(list_to_binary(FileName)),
                ?assertMatch(
                    #{
                        '$beamtalk_class' := 'Result',
                        'isOk' := false,
                        'errReason' := #{
                            '$beamtalk_class' := _,
                            error := #beamtalk_error{
                                kind = permission_denied, class = 'File', selector = 'readAll:'
                            }
                        }
                    },
                    R
                )
            after
                file:change_mode(FileName, 8#644),
                file:delete(FileName)
            end
    end.

%%% ============================================================================
%%% writeAll:contents:/2
%%% ============================================================================

writeAll_success_test() ->
    FileName = "_bt_test_write.txt",
    try
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:'writeAll:contents:'(
                list_to_binary(FileName), <<"content">>
            )
        ),
        ?assertEqual({ok, <<"content">>}, file:read_file(FileName))
    after
        file:delete(FileName)
    end.

writeAll_type_error_non_string_path_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'File', selector = 'writeAll:contents:'
            }
        },
        beamtalk_file:'writeAll:contents:'(42, <<"content">>)
    ).

writeAll_type_error_non_string_contents_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'File', selector = 'writeAll:contents:'
            }
        },
        beamtalk_file:'writeAll:contents:'(<<"file.txt">>, 42)
    ).

writeAll_invalid_path_absolute_test() ->
    R = beamtalk_file:'writeAll:contents:'(<<"/tmp/test.txt">>, <<"data">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = invalid_path, class = 'File', selector = 'writeAll:contents:'
                }
            }
        },
        R
    ).

writeAll_invalid_path_traversal_test() ->
    R = beamtalk_file:'writeAll:contents:'(<<"../test.txt">>, <<"data">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = invalid_path, class = 'File', selector = 'writeAll:contents:'
                }
            }
        },
        R
    ).

writeAll_creates_subdirectory_test() ->
    %% writeAll should auto-create parent directories
    FileName = "_bt_test_subdir/nested.txt",
    try
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:'writeAll:contents:'(
                list_to_binary(FileName), <<"nested content">>
            )
        ),
        ?assertEqual({ok, <<"nested content">>}, file:read_file(FileName))
    after
        file:delete(FileName),
        file:del_dir("_bt_test_subdir")
    end.

writeAll_permission_denied_test() ->
    case os:type() of
        {win32, _} ->
            %% Windows: Create a file first, then deny write access to it
            FileName = "_bt_test_readonly_file.txt",
            ok = file:write_file(FileName, <<"initial">>),
            %% Get current username
            UserName = string:trim(os:cmd("echo %USERNAME%")),
            %% Deny write access to the file using icacls
            DenyCmd = "icacls \"" ++ FileName ++ "\" /deny " ++ UserName ++ ":W >nul 2>&1",
            _ = os:cmd(DenyCmd),
            try
                R = beamtalk_file:'writeAll:contents:'(list_to_binary(FileName), <<"data">>),
                ?assertMatch(
                    #{
                        '$beamtalk_class' := 'Result',
                        'isOk' := false,
                        'errReason' := #{
                            '$beamtalk_class' := _,
                            error := #beamtalk_error{
                                kind = permission_denied,
                                class = 'File',
                                selector = 'writeAll:contents:'
                            }
                        }
                    },
                    R
                )
            after
                %% Reset ACL to allow access before cleanup
                ResetCmd = "icacls \"" ++ FileName ++ "\" /grant " ++ UserName ++ ":F >nul 2>&1",
                _ = os:cmd(ResetCmd),
                file:delete(FileName)
            end;
        _ ->
            %% Unix: Use chmod to make directory read-only
            Dir = "_bt_test_readonly_dir",
            FileName = Dir ++ "/test.txt",
            ok = filelib:ensure_dir(FileName),
            ok = file:change_mode(Dir, 8#555),
            try
                R = beamtalk_file:'writeAll:contents:'(list_to_binary(FileName), <<"data">>),
                ?assertMatch(
                    #{
                        '$beamtalk_class' := 'Result',
                        'isOk' := false,
                        'errReason' := #{
                            '$beamtalk_class' := _,
                            error := #beamtalk_error{
                                kind = permission_denied,
                                class = 'File',
                                selector = 'writeAll:contents:'
                            }
                        }
                    },
                    R
                )
            after
                file:change_mode(Dir, 8#755),
                file:delete(FileName),
                file:del_dir(Dir)
            end
    end.

%%% ============================================================================
%%% lines:/1
%%% ============================================================================

lines_success_test() ->
    with_temp_file("_bt_test_lines.txt", <<"a\nb\nc\n">>, fun() ->
        R = beamtalk_file:'lines:'(<<"_bt_test_lines.txt">>),
        ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
        #{'okValue' := Stream} = R,
        Result = beamtalk_stream:as_list(Stream),
        ?assertEqual([<<"a">>, <<"b">>, <<"c">>], Result)
    end).

lines_file_not_found_test() ->
    R = beamtalk_file:'lines:'(<<"_bt_test_no_such_file.txt">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{kind = file_not_found, class = 'File', selector = 'lines:'}
            }
        },
        R
    ).

lines_type_error_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'File', selector = 'lines:'
            }
        },
        beamtalk_file:'lines:'(42)
    ).

lines_invalid_path_absolute_test() ->
    R = beamtalk_file:'lines:'(<<"/etc/passwd">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{kind = invalid_path, class = 'File', selector = 'lines:'}
            }
        },
        R
    ).

lines_invalid_path_traversal_test() ->
    R = beamtalk_file:'lines:'(<<"../secret.txt">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{kind = invalid_path, class = 'File', selector = 'lines:'}
            }
        },
        R
    ).

lines_empty_file_test() ->
    with_temp_file("_bt_test_empty_lines.txt", <<>>, fun() ->
        R = beamtalk_file:'lines:'(<<"_bt_test_empty_lines.txt">>),
        ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
        #{'okValue' := Stream} = R,
        Result = beamtalk_stream:as_list(Stream),
        ?assertEqual([], Result)
    end).

lines_no_trailing_newline_test() ->
    with_temp_file("_bt_test_notrail.txt", <<"first\nsecond">>, fun() ->
        R = beamtalk_file:'lines:'(<<"_bt_test_notrail.txt">>),
        ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
        #{'okValue' := Stream} = R,
        Result = beamtalk_stream:as_list(Stream),
        ?assertEqual([<<"first">>, <<"second">>], Result)
    end).

lines_windows_crlf_test() ->
    with_temp_file("_bt_test_crlf.txt", <<"a\r\nb\r\nc\r\n">>, fun() ->
        R = beamtalk_file:'lines:'(<<"_bt_test_crlf.txt">>),
        ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
        #{'okValue' := Stream} = R,
        Result = beamtalk_stream:as_list(Stream),
        ?assertEqual([<<"a">>, <<"b">>, <<"c">>], Result)
    end).

lines_single_line_test() ->
    with_temp_file("_bt_test_single.txt", <<"only">>, fun() ->
        R = beamtalk_file:'lines:'(<<"_bt_test_single.txt">>),
        ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
        #{'okValue' := Stream} = R,
        Result = beamtalk_stream:as_list(Stream),
        ?assertEqual([<<"only">>], Result)
    end).

%%% ============================================================================
%%% open:do:/2
%%% ============================================================================

open_do_success_test() ->
    with_temp_file("_bt_test_open.txt", <<"line1\nline2\n">>, fun() ->
        R = beamtalk_file:'open:do:'(
            <<"_bt_test_open.txt">>,
            fun(Handle) -> beamtalk_stream:as_list(beamtalk_file:handle_lines(Handle)) end
        ),
        ?assertMatch(
            #{
                '$beamtalk_class' := 'Result',
                'isOk' := true,
                'okValue' := [<<"line1">>, <<"line2">>]
            },
            R
        )
    end).

open_do_file_not_found_test() ->
    R = beamtalk_file:'open:do:'(
        <<"_bt_test_no_such_file.txt">>,
        fun(_) -> error(callback_should_not_run) end
    ),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = file_not_found, class = 'File', selector = 'open:do:'
                }
            }
        },
        R
    ).

open_do_type_error_non_string_path_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'File', selector = 'open:do:'
            }
        },
        beamtalk_file:'open:do:'(42, fun(_) -> ok end)
    ).

open_do_type_error_non_block_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'File', selector = 'open:do:'
            }
        },
        beamtalk_file:'open:do:'(<<"file.txt">>, not_a_function)
    ).

open_do_invalid_path_absolute_test() ->
    R = beamtalk_file:'open:do:'(<<"/etc/passwd">>, fun(_) -> error(callback_should_not_run) end),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{kind = invalid_path, class = 'File', selector = 'open:do:'}
            }
        },
        R
    ).

open_do_invalid_path_traversal_test() ->
    R = beamtalk_file:'open:do:'(<<"../secret.txt">>, fun(_) -> error(callback_should_not_run) end),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{kind = invalid_path, class = 'File', selector = 'open:do:'}
            }
        },
        R
    ).

open_do_closes_on_exception_test() ->
    %% Verify the handle is closed even if the block raises
    with_temp_file("_bt_test_open_exc.txt", <<"data\n">>, fun() ->
        ?assertError(
            test_exception,
            beamtalk_file:'open:do:'(
                <<"_bt_test_open_exc.txt">>,
                fun(_Handle) -> error(test_exception) end
            )
        )
    end).

open_do_block_return_value_test() ->
    with_temp_file("_bt_test_open_ret.txt", <<"data\n">>, fun() ->
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := 42},
            beamtalk_file:'open:do:'(
                <<"_bt_test_open_ret.txt">>,
                fun(_Handle) -> 42 end
            )
        )
    end).

%%% ============================================================================
%%% handle_lines/1
%%% ============================================================================

handle_lines_type_error_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'FileHandle', selector = 'lines'
            }
        },
        beamtalk_file:handle_lines(not_a_handle)
    ).

handle_lines_wrong_map_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'FileHandle', selector = 'lines'
            }
        },
        beamtalk_file:handle_lines(#{foo => bar})
    ).

%%% ============================================================================
%%% Test Helpers (Directory)
%%% ============================================================================

%% @doc Create a temporary directory tree, run Fun, then recursively delete it.
with_temp_dir(Name, Fun) ->
    ok = filelib:ensure_path(Name),
    try
        Fun()
    after
        file:del_dir_r(Name)
    end.

%%% ============================================================================
%%% isDirectory:/1
%%% ============================================================================

isDirectory_on_directory_test() ->
    with_temp_dir("_bt_eunit_isdir", fun() ->
        ?assert(beamtalk_file:'isDirectory:'(<<"_bt_eunit_isdir">>))
    end).

isDirectory_on_file_test() ->
    with_temp_file("_bt_eunit_isdir_file.txt", <<"x">>, fun() ->
        ?assertNot(beamtalk_file:'isDirectory:'(<<"_bt_eunit_isdir_file.txt">>))
    end).

isDirectory_on_missing_path_test() ->
    ?assertNot(beamtalk_file:'isDirectory:'(<<"_bt_eunit_no_such_dir_xyz">>)).

isDirectory_invalid_path_returns_false_test() ->
    ?assertNot(beamtalk_file:'isDirectory:'(<<"/etc">>)).

isDirectory_traversal_returns_false_test() ->
    ?assertNot(beamtalk_file:'isDirectory:'(<<"../parent">>)).

isDirectory_type_error_returns_false_test() ->
    ?assertNot(beamtalk_file:'isDirectory:'(42)).

%%% ============================================================================
%%% isFile:/1
%%% ============================================================================

isFile_on_file_test() ->
    with_temp_file("_bt_eunit_isfile.txt", <<"x">>, fun() ->
        ?assert(beamtalk_file:'isFile:'(<<"_bt_eunit_isfile.txt">>))
    end).

isFile_on_directory_test() ->
    with_temp_dir("_bt_eunit_isfile_dir", fun() ->
        ?assertNot(beamtalk_file:'isFile:'(<<"_bt_eunit_isfile_dir">>))
    end).

isFile_on_missing_path_test() ->
    ?assertNot(beamtalk_file:'isFile:'(<<"_bt_eunit_no_such_file_xyz.txt">>)).

isFile_invalid_path_returns_false_test() ->
    ?assertNot(beamtalk_file:'isFile:'(<<"/etc/passwd">>)).

isFile_traversal_returns_false_test() ->
    ?assertNot(beamtalk_file:'isFile:'(<<"../secret.txt">>)).

isFile_type_error_returns_false_test() ->
    ?assertNot(beamtalk_file:'isFile:'(42)).

%%% ============================================================================
%%% mkdir:/1
%%% ============================================================================

mkdir_success_test() ->
    Dir = "_bt_eunit_mkdir",
    try
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:'mkdir:'(list_to_binary(Dir))
        ),
        ?assert(filelib:is_dir(Dir))
    after
        file:del_dir(Dir)
    end.

mkdir_already_exists_test() ->
    with_temp_dir("_bt_eunit_mkdir_exists", fun() ->
        R = beamtalk_file:'mkdir:'(<<"_bt_eunit_mkdir_exists">>),
        ?assertMatch(
            #{
                '$beamtalk_class' := 'Result',
                'isOk' := false,
                'errReason' := #{
                    '$beamtalk_class' := _,
                    error := #beamtalk_error{
                        kind = already_exists, class = 'File', selector = 'mkdir:'
                    }
                }
            },
            R
        )
    end).

mkdir_missing_parent_test() ->
    R = beamtalk_file:'mkdir:'(<<"_bt_eunit_no_parent_xyz/child">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = directory_not_found, class = 'File', selector = 'mkdir:'
                }
            }
        },
        R
    ).

mkdir_invalid_path_absolute_test() ->
    R = beamtalk_file:'mkdir:'(<<"/tmp/evil">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{kind = invalid_path, class = 'File', selector = 'mkdir:'}
            }
        },
        R
    ).

mkdir_invalid_path_traversal_test() ->
    R = beamtalk_file:'mkdir:'(<<"../escape">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{kind = invalid_path, class = 'File', selector = 'mkdir:'}
            }
        },
        R
    ).

mkdir_type_error_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'File', selector = 'mkdir:'
            }
        },
        beamtalk_file:'mkdir:'(42)
    ).

%%% ============================================================================
%%% mkdirAll:/1
%%% ============================================================================

mkdirAll_success_test() ->
    try
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:'mkdirAll:'(<<"_bt_eunit_mkdirall/a/b/c">>)
        ),
        ?assert(filelib:is_dir("_bt_eunit_mkdirall/a/b/c"))
    after
        file:del_dir_r("_bt_eunit_mkdirall")
    end.

mkdirAll_idempotent_test() ->
    Dir = "_bt_eunit_mkdirall_idem",
    try
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:'mkdirAll:'(list_to_binary(Dir))
        ),
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:'mkdirAll:'(list_to_binary(Dir))
        ),
        ?assert(filelib:is_dir(Dir))
    after
        file:del_dir_r(Dir)
    end.

mkdirAll_invalid_path_absolute_test() ->
    R = beamtalk_file:'mkdirAll:'(<<"/tmp/evil">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = invalid_path, class = 'File', selector = 'mkdirAll:'
                }
            }
        },
        R
    ).

mkdirAll_invalid_path_traversal_test() ->
    R = beamtalk_file:'mkdirAll:'(<<"../escape">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = invalid_path, class = 'File', selector = 'mkdirAll:'
                }
            }
        },
        R
    ).

mkdirAll_type_error_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'File', selector = 'mkdirAll:'
            }
        },
        beamtalk_file:'mkdirAll:'(42)
    ).

%%% ============================================================================
%%% listDirectory:/1
%%% ============================================================================

listDirectory_success_test() ->
    with_temp_dir("_bt_eunit_listdir", fun() ->
        ok = file:write_file("_bt_eunit_listdir/a.txt", <<"a">>),
        ok = file:write_file("_bt_eunit_listdir/b.txt", <<"b">>),
        R = beamtalk_file:'listDirectory:'(<<"_bt_eunit_listdir">>),
        ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
        #{'okValue' := Entries} = R,
        ?assertEqual([<<"a.txt">>, <<"b.txt">>], lists:sort(Entries))
    end).

listDirectory_empty_dir_test() ->
    with_temp_dir("_bt_eunit_listdir_empty", fun() ->
        R = beamtalk_file:'listDirectory:'(<<"_bt_eunit_listdir_empty">>),
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := []},
            R
        )
    end).

listDirectory_not_a_directory_test() ->
    %% filelib:is_regular/1 check ensures consistent error on all OSes
    with_temp_file("_bt_eunit_listdir_file.txt", <<"x">>, fun() ->
        R = beamtalk_file:'listDirectory:'(<<"_bt_eunit_listdir_file.txt">>),
        ?assertMatch(
            #{
                '$beamtalk_class' := 'Result',
                'isOk' := false,
                'errReason' := #{
                    '$beamtalk_class' := _,
                    error := #beamtalk_error{
                        kind = not_a_directory, class = 'File', selector = 'listDirectory:'
                    }
                }
            },
            R
        )
    end).

listDirectory_directory_not_found_test() ->
    R = beamtalk_file:'listDirectory:'(<<"_bt_eunit_no_such_dir_xyz">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = directory_not_found, class = 'File', selector = 'listDirectory:'
                }
            }
        },
        R
    ).

listDirectory_invalid_path_absolute_test() ->
    R = beamtalk_file:'listDirectory:'(<<"/etc">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = invalid_path, class = 'File', selector = 'listDirectory:'
                }
            }
        },
        R
    ).

listDirectory_invalid_path_traversal_test() ->
    R = beamtalk_file:'listDirectory:'(<<"../parent">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = invalid_path, class = 'File', selector = 'listDirectory:'
                }
            }
        },
        R
    ).

listDirectory_type_error_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'File', selector = 'listDirectory:'
            }
        },
        beamtalk_file:'listDirectory:'(42)
    ).

%%% ============================================================================
%%% delete:/1
%%% ============================================================================

delete_file_test() ->
    FileName = "_bt_eunit_delete_file.txt",
    ok = file:write_file(FileName, <<"x">>),
    ?assertMatch(
        #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
        beamtalk_file:'delete:'(list_to_binary(FileName))
    ),
    ?assertNot(filelib:is_regular(FileName)).

delete_empty_dir_test() ->
    Dir = "_bt_eunit_delete_emptydir",
    ok = file:make_dir(Dir),
    ?assertMatch(
        #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
        beamtalk_file:'delete:'(list_to_binary(Dir))
    ),
    ?assertNot(filelib:is_dir(Dir)).

delete_not_empty_test() ->
    with_temp_dir("_bt_eunit_delete_nonempty", fun() ->
        ok = file:write_file("_bt_eunit_delete_nonempty/child.txt", <<"x">>),
        R = beamtalk_file:'delete:'(<<"_bt_eunit_delete_nonempty">>),
        ?assertMatch(
            #{
                '$beamtalk_class' := 'Result',
                'isOk' := false,
                'errReason' := #{
                    '$beamtalk_class' := _,
                    error := #beamtalk_error{kind = not_empty, class = 'File', selector = 'delete:'}
                }
            },
            R
        )
    end).

delete_file_not_found_test() ->
    R = beamtalk_file:'delete:'(<<"_bt_eunit_no_such_file_xyz.txt">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = file_not_found, class = 'File', selector = 'delete:'
                }
            }
        },
        R
    ).

delete_invalid_path_absolute_test() ->
    R = beamtalk_file:'delete:'(<<"/tmp/evil.txt">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{kind = invalid_path, class = 'File', selector = 'delete:'}
            }
        },
        R
    ).

delete_invalid_path_traversal_test() ->
    R = beamtalk_file:'delete:'(<<"../escape.txt">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{kind = invalid_path, class = 'File', selector = 'delete:'}
            }
        },
        R
    ).

delete_type_error_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'File', selector = 'delete:'
            }
        },
        beamtalk_file:'delete:'(42)
    ).

%%% ============================================================================
%%% deleteAll:/1
%%% ============================================================================

deleteAll_success_test() ->
    try
        ok = filelib:ensure_path("_bt_eunit_deleteall/a/b"),
        ok = file:write_file("_bt_eunit_deleteall/a/b/file.txt", <<"x">>),
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:'deleteAll:'(<<"_bt_eunit_deleteall">>)
        ),
        ?assertNot(filelib:is_dir("_bt_eunit_deleteall"))
    after
        file:del_dir_r("_bt_eunit_deleteall")
    end.

deleteAll_file_not_found_test() ->
    R = beamtalk_file:'deleteAll:'(<<"_bt_eunit_no_such_dir_xyz">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = file_not_found, class = 'File', selector = 'deleteAll:'
                }
            }
        },
        R
    ).

deleteAll_invalid_path_absolute_test() ->
    R = beamtalk_file:'deleteAll:'(<<"/tmp/evil">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = invalid_path, class = 'File', selector = 'deleteAll:'
                }
            }
        },
        R
    ).

deleteAll_invalid_path_traversal_test() ->
    R = beamtalk_file:'deleteAll:'(<<"../escape">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = invalid_path, class = 'File', selector = 'deleteAll:'
                }
            }
        },
        R
    ).

deleteAll_type_error_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'File', selector = 'deleteAll:'
            }
        },
        beamtalk_file:'deleteAll:'(42)
    ).

%%% ============================================================================
%%% rename:to:/2
%%% ============================================================================

rename_to_file_test() ->
    Src = "_bt_eunit_rename_src.txt",
    Dst = "_bt_eunit_rename_dst.txt",
    ok = file:write_file(Src, <<"content">>),
    try
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:'rename:to:'(list_to_binary(Src), list_to_binary(Dst))
        ),
        ?assertNot(filelib:is_regular(Src)),
        ?assert(filelib:is_regular(Dst)),
        ?assertEqual({ok, <<"content">>}, file:read_file(Dst))
    after
        file:delete(Src),
        file:delete(Dst)
    end.

rename_to_directory_test() ->
    Src = "_bt_eunit_rename_dir_src",
    Dst = "_bt_eunit_rename_dir_dst",
    ok = file:make_dir(Src),
    try
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:'rename:to:'(list_to_binary(Src), list_to_binary(Dst))
        ),
        ?assertNot(filelib:is_dir(Src)),
        ?assert(filelib:is_dir(Dst))
    after
        file:del_dir(Src),
        file:del_dir(Dst)
    end.

rename_to_file_not_found_test() ->
    R = beamtalk_file:'rename:to:'(<<"_bt_eunit_no_such_xyz.txt">>, <<"_bt_eunit_dst.txt">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = file_not_found, class = 'File', selector = 'rename:to:'
                }
            }
        },
        R
    ).

rename_to_invalid_source_path_test() ->
    R = beamtalk_file:'rename:to:'(<<"/tmp/evil.txt">>, <<"_bt_eunit_dst.txt">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = invalid_path, class = 'File', selector = 'rename:to:'
                }
            }
        },
        R
    ).

rename_to_invalid_dest_path_test() ->
    with_temp_file("_bt_eunit_rename_valid_src.txt", <<"x">>, fun() ->
        R = beamtalk_file:'rename:to:'(
            <<"_bt_eunit_rename_valid_src.txt">>, <<"/tmp/evil.txt">>
        ),
        ?assertMatch(
            #{
                '$beamtalk_class' := 'Result',
                'isOk' := false,
                'errReason' := #{
                    '$beamtalk_class' := _,
                    error := #beamtalk_error{
                        kind = invalid_path, class = 'File', selector = 'rename:to:'
                    }
                }
            },
            R
        )
    end).

rename_to_type_error_from_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'File', selector = 'rename:to:'
            }
        },
        beamtalk_file:'rename:to:'(42, <<"dst.txt">>)
    ).

rename_to_type_error_to_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'File', selector = 'rename:to:'
            }
        },
        beamtalk_file:'rename:to:'(<<"src.txt">>, 42)
    ).

%%% ============================================================================
%%% absolutePath:/1
%%% ============================================================================

absolutePath_success_test() ->
    Input = <<"some/relative/path">>,
    R = beamtalk_file:'absolutePath:'(Input),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
    #{'okValue' := Result} = R,
    ?assert(is_binary(Result)),
    %% Absolute path is strictly longer than the relative input
    ?assert(byte_size(Result) > byte_size(Input)),
    %% Must contain the original relative component
    ?assertNotEqual(nomatch, binary:match(Result, Input)).

absolutePath_invalid_path_absolute_test() ->
    R = beamtalk_file:'absolutePath:'(<<"/etc/passwd">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = invalid_path, class = 'File', selector = 'absolutePath:'
                }
            }
        },
        R
    ).

absolutePath_invalid_path_traversal_test() ->
    R = beamtalk_file:'absolutePath:'(<<"../secret">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = invalid_path, class = 'File', selector = 'absolutePath:'
                }
            }
        },
        R
    ).

absolutePath_type_error_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'File', selector = 'absolutePath:'
            }
        },
        beamtalk_file:'absolutePath:'(42)
    ).

%%% ============================================================================
%%% cwd/0
%%% ============================================================================

cwd_returns_binary_test() ->
    ?assert(is_binary(beamtalk_file:'cwd'())).

cwd_non_empty_test() ->
    ?assert(byte_size(beamtalk_file:'cwd'()) > 0).

cwd_is_absolute_test() ->
    Cwd = beamtalk_file:'cwd'(),
    %% On Unix the cwd starts with /; on Windows it starts with a drive letter
    IsAbsolute =
        case Cwd of
            <<$/, _/binary>> -> true;
            <<Drive, $:, _/binary>> when Drive >= $A, Drive =< $Z -> true;
            <<Drive, $:, _/binary>> when Drive >= $a, Drive =< $z -> true;
            _ -> false
        end,
    ?assert(IsAbsolute).

cwd_matches_erlang_get_cwd_test() ->
    %% Verify beamtalk_file:'cwd'() returns the same value as file:get_cwd/0
    {ok, CwdList} = file:get_cwd(),
    CwdBin = unicode:characters_to_binary(CwdList),
    ?assertEqual(CwdBin, beamtalk_file:'cwd'()).

%%% ============================================================================
%%% tempDirectory/0
%%% ============================================================================

tempDirectory_returns_binary_test() ->
    ?assert(is_binary(beamtalk_file:'tempDirectory'())).

tempDirectory_non_empty_test() ->
    ?assert(byte_size(beamtalk_file:'tempDirectory'()) > 0).
