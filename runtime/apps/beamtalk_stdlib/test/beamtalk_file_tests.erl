%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_file_tests).

-moduledoc """
EUnit tests for beamtalk_file module.

Tests cover:
- File existence checks (exists:/1)
- File read/write operations and error paths
- Binary I/O (readBinary:/writeBinary:/appendBinary:)
- Stream creation (lines:/1) including large files
- Block-scoped handles (open:do:/2)
- Directory operations (mkdir:/mkdirAll:/listDirectory:/delete:/deleteAll:)
- File rename and overwrite (rename:to:/2)
- Permission denied error paths (Unix)
- FFI shims (exists/1, readAll/1, handleLines/1, etc.)
- handle_has_method/1
- strip_newline/1 edge cases
- Type error guards
- Absolute path operations and normalization (ADR 0063)
- Unicode roundtrip and large file handling
- tempDirectory/0 validation
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% Test Helpers
%%% ============================================================================

-doc "Create a temporary file with given contents, run Fun, then clean up.".
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

exists_absolute_path_test() ->
    %% /tmp always exists on Unix; on Windows this returns false (no /tmp)
    case os:type() of
        {unix, _} -> ?assertNot(beamtalk_file:'exists:'(<<"/tmp">>));
        _ -> ok
    end.

exists_tmp_directory_via_tempDirectory_test() ->
    TmpDir = beamtalk_file:'tempDirectory'(),
    %% tempDirectory returns a real directory, but exists: checks regular files
    ?assertNot(beamtalk_file:'exists:'(TmpDir)).

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

readAll_absolute_path_test() ->
    %% Absolute paths now work — read a known file
    case os:type() of
        {unix, _} ->
            R = beamtalk_file:'readAll:'(<<"/etc/hostname">>),
            %% May succeed or fail (file_not_found) depending on OS, but never invalid_path
            case R of
                #{'isOk' := true, 'okValue' := V} -> ?assert(is_binary(V));
                #{'isOk' := false} -> ok
            end;
        _ ->
            ok
    end.

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

writeAll_absolute_path_via_temp_test() ->
    %% Write and read back a file using an absolute path in the temp directory
    TmpDir = beamtalk_file:'tempDirectory'(),
    TmpFile = <<TmpDir/binary, "/_bt_eunit_abs_write.txt">>,
    try
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:'writeAll:contents:'(TmpFile, <<"abs-path-data">>)
        ),
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := <<"abs-path-data">>},
            beamtalk_file:'readAll:'(TmpFile)
        )
    after
        file:delete(binary_to_list(TmpFile))
    end.

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
%%% readBinary:/1
%%% ============================================================================

readBinary_success_test() ->
    with_temp_file("_bt_test_read_bin.dat", <<1, 2, 3, 0, 255>>, fun() ->
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := <<1, 2, 3, 0, 255>>},
            beamtalk_file:'readBinary:'(<<"_bt_test_read_bin.dat">>)
        )
    end).

readBinary_file_not_found_test() ->
    R = beamtalk_file:'readBinary:'(<<"_bt_test_no_such_file.bin">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = file_not_found, class = 'File', selector = 'readBinary:'
                }
            }
        },
        R
    ).

readBinary_type_error_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'File', selector = 'readBinary:'
            }
        },
        beamtalk_file:'readBinary:'(42)
    ).

readBinary_empty_file_test() ->
    with_temp_file("_bt_test_read_bin_empty.dat", <<>>, fun() ->
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := <<>>},
            beamtalk_file:'readBinary:'(<<"_bt_test_read_bin_empty.dat">>)
        )
    end).

ffi_shim_readBinary_not_found_test() ->
    R = beamtalk_file:readBinary(<<"_bt_shim_no_such_file.bin">>),
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

%%% ============================================================================
%%% writeBinary:contents:/2
%%% ============================================================================

writeBinary_success_test() ->
    FileName = "_bt_test_write_bin.dat",
    try
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:'writeBinary:contents:'(
                list_to_binary(FileName), <<1, 2, 3, 0, 255>>
            )
        ),
        ?assertEqual({ok, <<1, 2, 3, 0, 255>>}, file:read_file(FileName))
    after
        file:delete(FileName)
    end.

writeBinary_creates_subdirectory_test() ->
    FileName = "_bt_test_bin_subdir/nested.dat",
    try
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:'writeBinary:contents:'(
                list_to_binary(FileName), <<42>>
            )
        ),
        ?assertEqual({ok, <<42>>}, file:read_file(FileName))
    after
        file:delete(FileName),
        file:del_dir("_bt_test_bin_subdir")
    end.

writeBinary_type_error_non_string_path_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'File', selector = 'writeBinary:contents:'
            }
        },
        beamtalk_file:'writeBinary:contents:'(42, <<1, 2, 3>>)
    ).

writeBinary_type_error_non_binary_contents_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'File', selector = 'writeBinary:contents:'
            }
        },
        beamtalk_file:'writeBinary:contents:'(<<"file.dat">>, 42)
    ).

ffi_shim_writeBinary_success_test() ->
    FileName = "_bt_shim_write_bin.dat",
    try
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:writeBinary(list_to_binary(FileName), <<7, 8, 9>>)
        ),
        ?assertEqual({ok, <<7, 8, 9>>}, file:read_file(FileName))
    after
        file:delete(FileName)
    end.

%%% ============================================================================
%%% appendBinary:contents:/2
%%% ============================================================================

appendBinary_creates_file_test() ->
    FileName = "_bt_test_append_bin_new.dat",
    try
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:'appendBinary:contents:'(
                list_to_binary(FileName), <<1, 2, 3>>
            )
        ),
        ?assertEqual({ok, <<1, 2, 3>>}, file:read_file(FileName))
    after
        file:delete(FileName)
    end.

appendBinary_appends_to_existing_test() ->
    FileName = "_bt_test_append_bin_exist.dat",
    try
        ok = file:write_file(FileName, <<1, 2, 3>>),
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:'appendBinary:contents:'(
                list_to_binary(FileName), <<4, 5, 6>>
            )
        ),
        ?assertEqual({ok, <<1, 2, 3, 4, 5, 6>>}, file:read_file(FileName))
    after
        file:delete(FileName)
    end.

appendBinary_creates_subdirectory_test() ->
    FileName = "_bt_test_append_bin_subdir/nested.dat",
    try
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:'appendBinary:contents:'(
                list_to_binary(FileName), <<10, 20>>
            )
        ),
        ?assertEqual({ok, <<10, 20>>}, file:read_file(FileName))
    after
        file:delete(FileName),
        file:del_dir("_bt_test_append_bin_subdir")
    end.

appendBinary_type_error_non_string_path_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'File', selector = 'appendBinary:contents:'
            }
        },
        beamtalk_file:'appendBinary:contents:'(42, <<1, 2, 3>>)
    ).

appendBinary_type_error_non_binary_contents_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'File', selector = 'appendBinary:contents:'
            }
        },
        beamtalk_file:'appendBinary:contents:'(<<"file.dat">>, 42)
    ).

ffi_shim_appendBinary_success_test() ->
    FileName = "_bt_shim_append_bin.dat",
    try
        ok = file:write_file(FileName, <<10>>),
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:appendBinary(list_to_binary(FileName), <<20>>)
        ),
        ?assertEqual({ok, <<10, 20>>}, file:read_file(FileName))
    after
        file:delete(FileName)
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

-doc "Create a temporary directory tree, run Fun, then recursively delete it.".
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

isDirectory_absolute_path_test() ->
    %% Absolute paths now work — /tmp is a directory on Unix
    case os:type() of
        {unix, _} -> ?assert(beamtalk_file:'isDirectory:'(<<"/tmp">>));
        _ -> ok
    end.

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

isFile_absolute_path_test() ->
    %% Absolute paths now work
    case os:type() of
        {unix, _} -> ?assertNot(beamtalk_file:'isFile:'(<<"/tmp">>));
        _ -> ok
    end.

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

absolutePath_absolute_input_test() ->
    %% absolutePath: on an already-absolute path returns it unchanged
    TmpDir = beamtalk_file:'tempDirectory'(),
    R = beamtalk_file:'absolutePath:'(TmpDir),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := TmpDir}, R).

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
%%% lastModified:/1
%%% ============================================================================

lastModified_success_test() ->
    with_temp_file("_bt_eunit_lastmod.txt", <<"data">>, fun() ->
        R = beamtalk_file:'lastModified:'(<<"_bt_eunit_lastmod.txt">>),
        ?assertMatch(
            #{
                '$beamtalk_class' := 'Result',
                'isOk' := true,
                'okValue' := #{'$beamtalk_class' := 'DateTime'}
            },
            R
        ),
        #{'okValue' := DT} = R,
        %% Year should be reasonable (2020+)
        ?assert(maps:get(year, DT) >= 2020)
    end).

lastModified_file_not_found_test() ->
    R = beamtalk_file:'lastModified:'(<<"_bt_eunit_no_such_lastmod.txt">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = file_not_found, class = 'File', selector = 'lastModified:'
                }
            }
        },
        R
    ).

lastModified_type_error_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'File', selector = 'lastModified:'
            }
        },
        beamtalk_file:'lastModified:'(42)
    ).

ffi_shim_lastModified_test() ->
    with_temp_file("_bt_shim_lastmod.txt", <<"x">>, fun() ->
        R = beamtalk_file:lastModified(<<"_bt_shim_lastmod.txt">>),
        ?assertMatch(
            #{
                '$beamtalk_class' := 'Result',
                'isOk' := true,
                'okValue' := #{'$beamtalk_class' := 'DateTime'}
            },
            R
        )
    end).

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

tempDirectory_is_a_directory_test() ->
    TmpDir = beamtalk_file:'tempDirectory'(),
    ?assert(beamtalk_file:'isDirectory:'(TmpDir)).

%%% ============================================================================
%%% readAll: — additional error paths
%%% ============================================================================

readAll_empty_file_test() ->
    with_temp_file("_bt_test_read_empty.txt", <<>>, fun() ->
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := <<>>},
            beamtalk_file:'readAll:'(<<"_bt_test_read_empty.txt">>)
        )
    end).

%%% ============================================================================
%%% writeAll:contents: — overwrite existing file
%%% ============================================================================

writeAll_overwrite_existing_test() ->
    FileName = "_bt_test_overwrite.txt",
    try
        ok = file:write_file(FileName, <<"original">>),
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:'writeAll:contents:'(
                list_to_binary(FileName), <<"replaced">>
            )
        ),
        ?assertEqual({ok, <<"replaced">>}, file:read_file(FileName))
    after
        file:delete(FileName)
    end.

%%% ============================================================================
%%% readBinary: — permission denied
%%% ============================================================================

readBinary_permission_denied_test() ->
    case os:type() of
        {unix, _} ->
            FileName = "_bt_test_noperm_readbin.dat",
            ok = file:write_file(FileName, <<1, 2, 3>>),
            ok = file:change_mode(FileName, 8#000),
            try
                R = beamtalk_file:'readBinary:'(list_to_binary(FileName)),
                ?assertMatch(
                    #{
                        '$beamtalk_class' := 'Result',
                        'isOk' := false,
                        'errReason' := #{
                            '$beamtalk_class' := _,
                            error := #beamtalk_error{
                                kind = permission_denied,
                                class = 'File',
                                selector = 'readBinary:'
                            }
                        }
                    },
                    R
                )
            after
                file:change_mode(FileName, 8#644),
                file:delete(FileName)
            end;
        _ ->
            ok
    end.

%%% ============================================================================
%%% writeBinary:contents: — permission denied
%%% ============================================================================

writeBinary_permission_denied_test() ->
    case os:type() of
        {unix, _} ->
            Dir = "_bt_test_readonly_bindir",
            FileName = Dir ++ "/test.dat",
            ok = filelib:ensure_dir(FileName),
            ok = file:change_mode(Dir, 8#555),
            try
                R = beamtalk_file:'writeBinary:contents:'(list_to_binary(FileName), <<1, 2, 3>>),
                ?assertMatch(
                    #{
                        '$beamtalk_class' := 'Result',
                        'isOk' := false,
                        'errReason' := #{
                            '$beamtalk_class' := _,
                            error := #beamtalk_error{
                                kind = permission_denied,
                                class = 'File',
                                selector = 'writeBinary:contents:'
                            }
                        }
                    },
                    R
                )
            after
                file:change_mode(Dir, 8#755),
                file:delete(FileName),
                file:del_dir(Dir)
            end;
        _ ->
            ok
    end.

%%% ============================================================================
%%% appendBinary:contents: — permission denied
%%% ============================================================================

appendBinary_permission_denied_test() ->
    case os:type() of
        {unix, _} ->
            Dir = "_bt_test_readonly_appenddir",
            FileName = Dir ++ "/test.dat",
            ok = filelib:ensure_dir(FileName),
            ok = file:change_mode(Dir, 8#555),
            try
                R = beamtalk_file:'appendBinary:contents:'(
                    list_to_binary(FileName), <<1, 2, 3>>
                ),
                ?assertMatch(
                    #{
                        '$beamtalk_class' := 'Result',
                        'isOk' := false,
                        'errReason' := #{
                            '$beamtalk_class' := _,
                            error := #beamtalk_error{
                                kind = permission_denied,
                                class = 'File',
                                selector = 'appendBinary:contents:'
                            }
                        }
                    },
                    R
                )
            after
                file:change_mode(Dir, 8#755),
                file:delete(FileName),
                file:del_dir(Dir)
            end;
        _ ->
            ok
    end.

%%% ============================================================================
%%% lines: — permission denied
%%% ============================================================================

lines_permission_denied_test() ->
    case os:type() of
        {unix, _} ->
            FileName = "_bt_test_noperm_lines.txt",
            ok = file:write_file(FileName, <<"line1\nline2\n">>),
            ok = file:change_mode(FileName, 8#000),
            try
                R = beamtalk_file:'lines:'(list_to_binary(FileName)),
                ?assertMatch(
                    #{
                        '$beamtalk_class' := 'Result',
                        'isOk' := false,
                        'errReason' := #{
                            '$beamtalk_class' := _,
                            error := #beamtalk_error{
                                kind = permission_denied,
                                class = 'File',
                                selector = 'lines:'
                            }
                        }
                    },
                    R
                )
            after
                file:change_mode(FileName, 8#644),
                file:delete(FileName)
            end;
        _ ->
            ok
    end.

%%% ============================================================================
%%% open:do: — permission denied
%%% ============================================================================

open_do_permission_denied_test() ->
    case os:type() of
        {unix, _} ->
            FileName = "_bt_test_noperm_open.txt",
            ok = file:write_file(FileName, <<"data\n">>),
            ok = file:change_mode(FileName, 8#000),
            try
                R = beamtalk_file:'open:do:'(
                    list_to_binary(FileName),
                    fun(_) -> ok end
                ),
                ?assertMatch(
                    #{
                        '$beamtalk_class' := 'Result',
                        'isOk' := false,
                        'errReason' := #{
                            '$beamtalk_class' := _,
                            error := #beamtalk_error{
                                kind = permission_denied,
                                class = 'File',
                                selector = 'open:do:'
                            }
                        }
                    },
                    R
                )
            after
                file:change_mode(FileName, 8#644),
                file:delete(FileName)
            end;
        _ ->
            ok
    end.

%%% ============================================================================
%%% listDirectory: — permission denied & mixed entries
%%% ============================================================================

listDirectory_permission_denied_test() ->
    case os:type() of
        {unix, _} ->
            Dir = "_bt_eunit_listdir_noperm",
            ok = filelib:ensure_path(Dir),
            ok = file:write_file(Dir ++ "/a.txt", <<"a">>),
            ok = file:change_mode(Dir, 8#000),
            try
                R = beamtalk_file:'listDirectory:'(list_to_binary(Dir)),
                ?assertMatch(
                    #{
                        '$beamtalk_class' := 'Result',
                        'isOk' := false,
                        'errReason' := #{
                            '$beamtalk_class' := _,
                            error := #beamtalk_error{
                                kind = permission_denied,
                                class = 'File',
                                selector = 'listDirectory:'
                            }
                        }
                    },
                    R
                )
            after
                file:change_mode(Dir, 8#755),
                file:del_dir_r(Dir)
            end;
        _ ->
            ok
    end.

listDirectory_includes_subdirectories_test() ->
    with_temp_dir("_bt_eunit_listdir_mixed", fun() ->
        ok = file:write_file("_bt_eunit_listdir_mixed/file.txt", <<"x">>),
        ok = file:make_dir("_bt_eunit_listdir_mixed/subdir"),
        R = beamtalk_file:'listDirectory:'(<<"_bt_eunit_listdir_mixed">>),
        ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
        #{'okValue' := Entries} = R,
        Sorted = lists:sort(Entries),
        ?assertEqual([<<"file.txt">>, <<"subdir">>], Sorted)
    end).

%%% ============================================================================
%%% mkdir: — permission denied
%%% ============================================================================

mkdir_permission_denied_test() ->
    case os:type() of
        {unix, _} ->
            Dir = "_bt_eunit_mkdir_noperm",
            ok = file:make_dir(Dir),
            ok = file:change_mode(Dir, 8#555),
            try
                R = beamtalk_file:'mkdir:'(list_to_binary(Dir ++ "/child")),
                ?assertMatch(
                    #{
                        '$beamtalk_class' := 'Result',
                        'isOk' := false,
                        'errReason' := #{
                            '$beamtalk_class' := _,
                            error := #beamtalk_error{
                                kind = permission_denied,
                                class = 'File',
                                selector = 'mkdir:'
                            }
                        }
                    },
                    R
                )
            after
                file:change_mode(Dir, 8#755),
                file:del_dir(Dir)
            end;
        _ ->
            ok
    end.

%%% ============================================================================
%%% rename:to: — permission denied & overwrite destination
%%% ============================================================================

rename_to_permission_denied_test() ->
    case os:type() of
        {unix, _} ->
            Dir = "_bt_eunit_rename_noperm",
            Src = Dir ++ "/src.txt",
            Dst = Dir ++ "/dst.txt",
            ok = filelib:ensure_path(Dir),
            ok = file:write_file(Src, <<"data">>),
            ok = file:change_mode(Dir, 8#555),
            try
                R = beamtalk_file:'rename:to:'(list_to_binary(Src), list_to_binary(Dst)),
                ?assertMatch(
                    #{
                        '$beamtalk_class' := 'Result',
                        'isOk' := false,
                        'errReason' := #{
                            '$beamtalk_class' := _,
                            error := #beamtalk_error{
                                kind = permission_denied,
                                class = 'File',
                                selector = 'rename:to:'
                            }
                        }
                    },
                    R
                )
            after
                file:change_mode(Dir, 8#755),
                file:del_dir_r(Dir)
            end;
        _ ->
            ok
    end.

rename_to_overwrites_destination_test() ->
    Src = "_bt_eunit_rename_overwrite_src.txt",
    Dst = "_bt_eunit_rename_overwrite_dst.txt",
    ok = file:write_file(Src, <<"new">>),
    ok = file:write_file(Dst, <<"old">>),
    try
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:'rename:to:'(list_to_binary(Src), list_to_binary(Dst))
        ),
        ?assertNot(filelib:is_regular(Src)),
        ?assertEqual({ok, <<"new">>}, file:read_file(Dst))
    after
        file:delete(Src),
        file:delete(Dst)
    end.

%%% ============================================================================
%%% delete: — permission denied
%%% ============================================================================

delete_permission_denied_test() ->
    case os:type() of
        {unix, _} ->
            Dir = "_bt_eunit_delete_noperm",
            FileName = Dir ++ "/protected.txt",
            ok = filelib:ensure_path(Dir),
            ok = file:write_file(FileName, <<"x">>),
            ok = file:change_mode(Dir, 8#555),
            try
                R = beamtalk_file:'delete:'(list_to_binary(FileName)),
                ?assertMatch(
                    #{
                        '$beamtalk_class' := 'Result',
                        'isOk' := false,
                        'errReason' := #{
                            '$beamtalk_class' := _,
                            error := #beamtalk_error{
                                kind = permission_denied,
                                class = 'File',
                                selector = 'delete:'
                            }
                        }
                    },
                    R
                )
            after
                file:change_mode(Dir, 8#755),
                file:del_dir_r(Dir)
            end;
        _ ->
            ok
    end.

%%% ============================================================================
%%% deleteAll: on a single file
%%% ============================================================================

deleteAll_single_file_test() ->
    FileName = "_bt_eunit_deleteall_single.txt",
    ok = file:write_file(FileName, <<"x">>),
    try
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:'deleteAll:'(list_to_binary(FileName))
        ),
        ?assertNot(filelib:is_regular(FileName))
    after
        file:delete(FileName)
    end.

%%% ============================================================================
%%% lines: — large file handling
%%% ============================================================================

lines_large_file_test() ->
    %% Generate a file with 1000 lines
    Lines = [integer_to_binary(N) || N <- lists:seq(1, 1000)],
    Content = iolist_to_binary(lists:join(<<"\n">>, Lines)),
    with_temp_file("_bt_test_large_lines.txt", Content, fun() ->
        R = beamtalk_file:'lines:'(<<"_bt_test_large_lines.txt">>),
        ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
        #{'okValue' := Stream} = R,
        Result = beamtalk_stream:as_list(Stream),
        ?assertEqual(1000, length(Result)),
        ?assertEqual(<<"1">>, hd(Result)),
        ?assertEqual(<<"1000">>, lists:last(Result))
    end).

%%% ============================================================================
%%% absolutePath: — path normalization
%%% ============================================================================

absolutePath_dot_relative_test() ->
    %% ./foo should resolve to cwd/foo
    R = beamtalk_file:'absolutePath:'(<<"./foo">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
    #{'okValue' := AbsPath} = R,
    Cwd = beamtalk_file:'cwd'(),
    ?assertNotEqual(nomatch, binary:match(AbsPath, Cwd)).

absolutePath_empty_string_test() ->
    %% Empty string should resolve to cwd
    R = beamtalk_file:'absolutePath:'(<<>>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
    #{'okValue' := AbsPath} = R,
    ?assert(byte_size(AbsPath) > 0).

%%% ============================================================================
%%% FFI shim: handleLines
%%% ============================================================================

ffi_shim_handleLines_test() ->
    with_temp_file("_bt_test_handlelines.txt", <<"a\nb\n">>, fun() ->
        R = beamtalk_file:'open:do:'(
            <<"_bt_test_handlelines.txt">>,
            fun(Handle) -> beamtalk_stream:as_list(beamtalk_file:handleLines(Handle)) end
        ),
        ?assertMatch(
            #{
                '$beamtalk_class' := 'Result',
                'isOk' := true,
                'okValue' := [<<"a">>, <<"b">>]
            },
            R
        )
    end).

%%% ============================================================================
%%% FFI shims: additional coverage
%%% ============================================================================

ffi_shim_isDirectory_true_test() ->
    with_temp_dir("_bt_shim_isdir_true", fun() ->
        ?assert(beamtalk_file:isDirectory(<<"_bt_shim_isdir_true">>))
    end).

ffi_shim_isFile_true_test() ->
    with_temp_file("_bt_shim_isfile_true.txt", <<"x">>, fun() ->
        ?assert(beamtalk_file:isFile(<<"_bt_shim_isfile_true.txt">>))
    end).

ffi_shim_listDirectory_not_found_test() ->
    R = beamtalk_file:listDirectory(<<"_bt_shim_no_such_dir_xyz">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{kind = directory_not_found}
            }
        },
        R
    ).

ffi_shim_delete_not_found_test() ->
    R = beamtalk_file:delete(<<"_bt_shim_no_such_file_xyz.txt">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{kind = file_not_found}
            }
        },
        R
    ).

ffi_shim_deleteAll_not_found_test() ->
    R = beamtalk_file:deleteAll(<<"_bt_shim_no_such_dir_xyz">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{kind = file_not_found}
            }
        },
        R
    ).

ffi_shim_absolutePath_relative_test() ->
    R = beamtalk_file:absolutePath(<<"relative/path">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R).

%%% ============================================================================
%%% writeAll:contents: — large file handling
%%% ============================================================================

writeAll_large_file_test() ->
    FileName = "_bt_test_write_large.txt",
    %% 100KB of data
    Content = binary:copy(<<"abcdefghij">>, 10000),
    try
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:'writeAll:contents:'(
                list_to_binary(FileName), Content
            )
        ),
        ?assertEqual({ok, Content}, file:read_file(FileName))
    after
        file:delete(FileName)
    end.

%%% ============================================================================
%%% readAll:/writeAll: roundtrip with unicode
%%% ============================================================================

readAll_writeAll_unicode_roundtrip_test() ->
    FileName = "_bt_test_unicode_roundtrip.txt",
    Content = unicode:characters_to_binary("hello 世界 🌍"),
    try
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil},
            beamtalk_file:'writeAll:contents:'(list_to_binary(FileName), Content)
        ),
        ?assertMatch(
            #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := Content},
            beamtalk_file:'readAll:'(list_to_binary(FileName))
        )
    after
        file:delete(FileName)
    end.

%%% ============================================================================
%%% open:do: with handle_lines — multiple reads
%%% ============================================================================

open_do_handle_lines_success_test() ->
    with_temp_file("_bt_test_open_handle_lines.txt", <<"x\ny\nz\n">>, fun() ->
        R = beamtalk_file:'open:do:'(
            <<"_bt_test_open_handle_lines.txt">>,
            fun(Handle) ->
                Stream = beamtalk_file:handle_lines(Handle),
                beamtalk_stream:as_list(Stream)
            end
        ),
        ?assertMatch(
            #{
                '$beamtalk_class' := 'Result',
                'isOk' := true,
                'okValue' := [<<"x">>, <<"y">>, <<"z">>]
            },
            R
        )
    end).

%%% ============================================================================
%%% BT-1983 — additional error path coverage
%%% ============================================================================

%% Attempting to read/write a directory (as if it were a file) exercises
%% the catch-all io_error branches: file:read_file/write_file/open on a
%% directory returns {error, eisdir} (not enoent, not eacces).

readAll_eisdir_test() ->
    Dir = "_bt_eunit_readAll_eisdir",
    try
        ok = filelib:ensure_path(Dir),
        R = beamtalk_file:'readAll:'(list_to_binary(Dir)),
        ?assertMatch(
            #{
                '$beamtalk_class' := 'Result',
                'isOk' := false,
                'errReason' := #{'$beamtalk_class' := _, error := #beamtalk_error{kind = io_error}}
            },
            R
        )
    after
        file:del_dir_r(Dir)
    end.

readBinary_eisdir_test() ->
    Dir = "_bt_eunit_readBinary_eisdir",
    try
        ok = filelib:ensure_path(Dir),
        R = beamtalk_file:'readBinary:'(list_to_binary(Dir)),
        ?assertMatch(
            #{
                '$beamtalk_class' := 'Result',
                'isOk' := false,
                'errReason' := #{'$beamtalk_class' := _, error := #beamtalk_error{kind = io_error}}
            },
            R
        )
    after
        file:del_dir_r(Dir)
    end.

writeAll_eisdir_test() ->
    Dir = "_bt_eunit_writeAll_eisdir",
    try
        ok = filelib:ensure_path(Dir),
        R = beamtalk_file:'writeAll:contents:'(list_to_binary(Dir), <<"x">>),
        ?assertMatch(
            #{
                '$beamtalk_class' := 'Result',
                'isOk' := false,
                'errReason' := #{'$beamtalk_class' := _, error := #beamtalk_error{kind = io_error}}
            },
            R
        )
    after
        file:del_dir_r(Dir)
    end.

writeBinary_eisdir_test() ->
    Dir = "_bt_eunit_writeBinary_eisdir",
    try
        ok = filelib:ensure_path(Dir),
        R = beamtalk_file:'writeBinary:contents:'(list_to_binary(Dir), <<"x">>),
        ?assertMatch(
            #{
                '$beamtalk_class' := 'Result',
                'isOk' := false,
                'errReason' := #{'$beamtalk_class' := _, error := #beamtalk_error{kind = io_error}}
            },
            R
        )
    after
        file:del_dir_r(Dir)
    end.

appendBinary_eisdir_test() ->
    Dir = "_bt_eunit_appendBinary_eisdir",
    try
        ok = filelib:ensure_path(Dir),
        R = beamtalk_file:'appendBinary:contents:'(list_to_binary(Dir), <<"x">>),
        ?assertMatch(
            #{
                '$beamtalk_class' := 'Result',
                'isOk' := false,
                'errReason' := #{'$beamtalk_class' := _, error := #beamtalk_error{kind = io_error}}
            },
            R
        )
    after
        file:del_dir_r(Dir)
    end.

lines_eisdir_test() ->
    Dir = "_bt_eunit_lines_eisdir",
    try
        ok = filelib:ensure_path(Dir),
        R = beamtalk_file:'lines:'(list_to_binary(Dir)),
        ?assertMatch(
            #{
                '$beamtalk_class' := 'Result',
                'isOk' := false,
                'errReason' := #{'$beamtalk_class' := _, error := #beamtalk_error{kind = io_error}}
            },
            R
        )
    after
        file:del_dir_r(Dir)
    end.

open_do_eisdir_test() ->
    Dir = "_bt_eunit_open_eisdir",
    try
        ok = filelib:ensure_path(Dir),
        R = beamtalk_file:'open:do:'(
            list_to_binary(Dir), fun(_) -> error(callback_should_not_run) end
        ),
        ?assertMatch(
            #{
                '$beamtalk_class' := 'Result',
                'isOk' := false,
                'errReason' := #{'$beamtalk_class' := _, error := #beamtalk_error{kind = io_error}}
            },
            R
        )
    after
        file:del_dir_r(Dir)
    end.

%%% ----------------------------------------------------------------------------
%%% mkdir / mkdirAll / listDirectory / delete / deleteAll / rename catch-alls
%%% ----------------------------------------------------------------------------

mkdir_already_exists_as_file_test() ->
    %% mkdir on a path that exists as a regular file → {error, eexist}
    %% (covers the eexist branch, which is distinct from directory_not_found)
    FileName = "_bt_eunit_mkdir_eexist.txt",
    try
        ok = file:write_file(FileName, <<"x">>),
        R = beamtalk_file:'mkdir:'(list_to_binary(FileName)),
        ?assertMatch(
            #{
                '$beamtalk_class' := 'Result',
                'isOk' := false,
                'errReason' := #{
                    '$beamtalk_class' := _, error := #beamtalk_error{kind = already_exists}
                }
            },
            R
        )
    after
        file:delete(FileName)
    end.

mkdirAll_parent_is_file_test() ->
    %% mkdirAll when a path component is a regular file → io_error
    Parent = "_bt_eunit_mkdirAll_parent_file.txt",
    try
        ok = file:write_file(Parent, <<"x">>),
        R = beamtalk_file:'mkdirAll:'(list_to_binary(Parent ++ "/child")),
        ?assertMatch(
            #{
                '$beamtalk_class' := 'Result',
                'isOk' := false,
                'errReason' := #{'$beamtalk_class' := _, error := #beamtalk_error{}}
            },
            R
        )
    after
        file:delete(Parent)
    end.

listDirectory_missing_dir_test() ->
    R = beamtalk_file:'listDirectory:'(<<"_bt_eunit_listdir_missing">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _, error := #beamtalk_error{kind = directory_not_found}
            }
        },
        R
    ).

listDirectory_upfront_file_check_test() ->
    %% listDirectory on a regular file → not_a_directory (upfront check)
    FileName = "_bt_eunit_listdir_file.txt",
    try
        ok = file:write_file(FileName, <<"x">>),
        R = beamtalk_file:'listDirectory:'(list_to_binary(FileName)),
        ?assertMatch(
            #{
                '$beamtalk_class' := 'Result',
                'isOk' := false,
                'errReason' := #{
                    '$beamtalk_class' := _, error := #beamtalk_error{kind = not_a_directory}
                }
            },
            R
        )
    after
        file:delete(FileName)
    end.

rename_to_destination_is_directory_test() ->
    %% rename a file to a target that is an existing, non-empty directory
    Src = "_bt_eunit_rename_src_file.txt",
    Dst = "_bt_eunit_rename_dst_dir",
    DstChild = Dst ++ "/child.txt",
    try
        ok = file:write_file(Src, <<"x">>),
        ok = filelib:ensure_path(Dst),
        ok = file:write_file(DstChild, <<"y">>),
        R = beamtalk_file:'rename:to:'(list_to_binary(Src), list_to_binary(Dst)),
        ?assertMatch(
            #{
                '$beamtalk_class' := 'Result',
                'isOk' := false,
                'errReason' := #{'$beamtalk_class' := _, error := #beamtalk_error{}}
            },
            R
        )
    after
        file:delete(Src),
        file:del_dir_r(Dst)
    end.

%%% ----------------------------------------------------------------------------
%%% strip_newline edge cases — exercised through lines:/1
%%% ----------------------------------------------------------------------------

lines_empty_trailing_newline_only_test() ->
    with_temp_file("_bt_eunit_lines_empty_newline.txt", <<"\n">>, fun() ->
        R = beamtalk_file:'lines:'(<<"_bt_eunit_lines_empty_newline.txt">>),
        ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
        #{okValue := Stream} = R,
        Lines = beamtalk_stream:as_list(Stream),
        %% Expect a single empty line
        ?assertEqual([<<>>], Lines)
    end).

lines_bare_cr_in_middle_test() ->
    %% A bare \r inside a line is preserved (only trailing \r\n is stripped)
    with_temp_file("_bt_eunit_lines_bare_cr.txt", <<"ab\rcd\nef\n">>, fun() ->
        R = beamtalk_file:'lines:'(<<"_bt_eunit_lines_bare_cr.txt">>),
        #{okValue := Stream} = R,
        Lines = beamtalk_stream:as_list(Stream),
        ?assertEqual([<<"ab\rcd">>, <<"ef">>], Lines)
    end).

%%% ----------------------------------------------------------------------------
%%% tempDirectory: exercise the os:getenv fallback branches
%%% ----------------------------------------------------------------------------

temp_directory_respects_TMPDIR_test() ->
    %% Set TMPDIR to a known value and verify it is returned.
    OrigTmpdir = os:getenv("TMPDIR"),
    try
        os:putenv("TMPDIR", "/custom/tmp/path"),
        ?assertEqual(<<"/custom/tmp/path">>, beamtalk_file:'tempDirectory'())
    after
        case OrigTmpdir of
            false -> os:unsetenv("TMPDIR");
            V -> os:putenv("TMPDIR", V)
        end
    end.

temp_directory_falls_back_to_TMP_test() ->
    %% Unset TMPDIR, set TMP, expect TMP's value.
    OrigTmpdir = os:getenv("TMPDIR"),
    OrigTmp = os:getenv("TMP"),
    try
        os:unsetenv("TMPDIR"),
        os:putenv("TMP", "/tmp-fallback"),
        ?assertEqual(<<"/tmp-fallback">>, beamtalk_file:'tempDirectory'())
    after
        case OrigTmp of
            false -> os:unsetenv("TMP");
            V -> os:putenv("TMP", V)
        end,
        case OrigTmpdir of
            false -> ok;
            V2 -> os:putenv("TMPDIR", V2)
        end
    end.

temp_directory_falls_back_to_TEMP_test() ->
    %% Unset TMPDIR and TMP, set TEMP.
    OrigTmpdir = os:getenv("TMPDIR"),
    OrigTmp = os:getenv("TMP"),
    OrigTemp = os:getenv("TEMP"),
    try
        os:unsetenv("TMPDIR"),
        os:unsetenv("TMP"),
        os:putenv("TEMP", "/temp-fallback"),
        ?assertEqual(<<"/temp-fallback">>, beamtalk_file:'tempDirectory'())
    after
        case OrigTemp of
            false -> os:unsetenv("TEMP");
            V -> os:putenv("TEMP", V)
        end,
        case OrigTmp of
            false -> ok;
            V2 -> os:putenv("TMP", V2)
        end,
        case OrigTmpdir of
            false -> ok;
            V3 -> os:putenv("TMPDIR", V3)
        end
    end.

temp_directory_platform_default_test() ->
    %% Unset all three env vars; the function must still return a platform-
    %% appropriate default ("/tmp" on Unix, "C:\\Windows\\Temp" on Windows).
    OrigTmpdir = os:getenv("TMPDIR"),
    OrigTmp = os:getenv("TMP"),
    OrigTemp = os:getenv("TEMP"),
    try
        os:unsetenv("TMPDIR"),
        os:unsetenv("TMP"),
        os:unsetenv("TEMP"),
        Result = beamtalk_file:'tempDirectory'(),
        ?assert(is_binary(Result)),
        ?assert(byte_size(Result) > 0)
    after
        case OrigTmpdir of
            false -> ok;
            V1 -> os:putenv("TMPDIR", V1)
        end,
        case OrigTmp of
            false -> ok;
            V2 -> os:putenv("TMP", V2)
        end,
        case OrigTemp of
            false -> ok;
            V3 -> os:putenv("TEMP", V3)
        end
    end.
