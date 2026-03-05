%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc File class implementation - file system I/O operations.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% File provides basic file I/O operations wrapping Erlang's file module.
%%% All operations are class methods. Error handling uses structured
%%% beamtalk_error records.
%%%
%%% ## Methods
%%%
%%% | Selector                     | Description                            |
%%% |------------------------------|----------------------------------------|
%%% | `exists:`                    | Check if file exists (returns Bool)    |
%%% | `readAll:`                   | Read entire file as String             |
%%% | `writeAll:contents:`         | Write string to file                   |
%%% | `lines:`                     | Lazy Stream of lines (constant memory) |
%%% | `open:do:`                   | Block-scoped handle with auto-close    |
%%%
%%% ## Usage
%%%
%%% ```beamtalk
%%% File exists: 'test.txt'
%%% File readAll: 'test.txt'
%%% File writeAll: 'output.txt' contents: 'hello world'
%%% (File lines: 'data.csv') do: [:line | Transcript show: line]
%%% File open: 'data.csv' do: [:handle | handle lines take: 10]
%%% ```
%%%
%%% ## Security
%%%
%%% - File paths are validated to prevent directory traversal
%%% - Paths must be relative or within the current working directory
%%% - Absolute paths outside the project are rejected

-module(beamtalk_file).

-export([
    'exists:'/1,
    'readAll:'/1,
    'writeAll:contents:'/2,
    'lines:'/1,
    'open:do:'/2,
    'isDirectory:'/1,
    'isFile:'/1,
    'mkdir:'/1,
    'mkdirAll:'/1,
    'listDirectory:'/1,
    'delete:'/1,
    'deleteAll:'/1,
    'rename:to:'/2,
    'absolutePath:'/1,
    'tempDirectory'/0
]).
-export([handle_lines/1, has_method/1, handle_has_method/1]).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% Public API (called directly by codegen)
%%% ============================================================================

%% @doc Check if a file exists.
%%
%% Returns true if the file exists, false otherwise.
%% Does not raise errors (returns false for invalid paths).
-spec 'exists:'(binary()) -> boolean().
'exists:'(Path) when is_binary(Path) ->
    case validate_path(Path) of
        {ok, ValidPath} ->
            filelib:is_regular(ValidPath);
        {error, _} ->
            false
    end;
'exists:'(_) ->
    false.

%% @doc Read entire file contents as a String.
%%
%% Returns the file contents as a binary (String), or raises a
%% structured error if the file cannot be read.
-spec 'readAll:'(binary()) -> binary().
'readAll:'(Path) when is_binary(Path) ->
    case validate_path(Path) of
        {ok, ValidPath} ->
            case file:read_file(ValidPath) of
                {ok, Contents} ->
                    Contents;
                {error, enoent} ->
                    Error0 = beamtalk_error:new(file_not_found, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'readAll:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Check that the file exists">>),
                    beamtalk_error:raise(Error3);
                {error, eacces} ->
                    Error0 = beamtalk_error:new(permission_denied, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'readAll:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Check file permissions">>),
                    beamtalk_error:raise(Error3);
                {error, Reason} ->
                    Error0 = beamtalk_error:new(io_error, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'readAll:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
                    beamtalk_error:raise(Error2)
            end;
        {error, Reason} ->
            Error0 = beamtalk_error:new(invalid_path, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'readAll:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
            Hint =
                case Reason of
                    absolute_path -> <<"Use relative paths only">>;
                    directory_traversal -> <<"Use relative paths within the project">>
                end,
            Error3 = beamtalk_error:with_hint(Error2, Hint),
            beamtalk_error:raise(Error3)
    end;
'readAll:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'readAll:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

%% @doc Write string contents to a file.
%%
%% Creates the file if it doesn't exist, overwrites if it does.
%% Returns nil on success, raises structured error on failure.
-spec 'writeAll:contents:'(binary(), binary()) -> 'nil'.
'writeAll:contents:'(Path, Contents) when is_binary(Path), is_binary(Contents) ->
    case validate_path(Path) of
        {ok, ValidPath} ->
            %% Ensure directory exists
            Dir = filename:dirname(ValidPath),
            case filelib:ensure_dir(filename:join(Dir, "dummy")) of
                ok ->
                    case file:write_file(ValidPath, Contents) of
                        ok ->
                            nil;
                        {error, eacces} ->
                            Error0 = beamtalk_error:new(permission_denied, 'File'),
                            Error1 = beamtalk_error:with_selector(Error0, 'writeAll:contents:'),
                            Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                            Error3 = beamtalk_error:with_hint(Error2, <<"Check file permissions">>),
                            beamtalk_error:raise(Error3);
                        {error, Reason} ->
                            Error0 = beamtalk_error:new(io_error, 'File'),
                            Error1 = beamtalk_error:with_selector(Error0, 'writeAll:contents:'),
                            Error2 = beamtalk_error:with_details(Error1, #{
                                path => Path, reason => Reason
                            }),
                            beamtalk_error:raise(Error2)
                    end;
                {error, Reason} ->
                    Error0 = beamtalk_error:new(io_error, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'writeAll:contents:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Could not create directory">>),
                    beamtalk_error:raise(Error3)
            end;
        {error, Reason} ->
            Error0 = beamtalk_error:new(invalid_path, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'writeAll:contents:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
            Hint =
                case Reason of
                    absolute_path -> <<"Use relative paths only">>;
                    directory_traversal -> <<"Use relative paths within the project">>
                end,
            Error3 = beamtalk_error:with_hint(Error2, Hint),
            beamtalk_error:raise(Error3)
    end;
'writeAll:contents:'(Path, _) when is_binary(Path) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'writeAll:contents:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Contents must be a String">>),
    beamtalk_error:raise(Error2);
'writeAll:contents:'(_, _) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'writeAll:contents:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

%% @doc Check if File responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method('exists:') -> true;
has_method('readAll:') -> true;
has_method('writeAll:contents:') -> true;
has_method('lines:') -> true;
has_method('open:do:') -> true;
has_method('isDirectory:') -> true;
has_method('isFile:') -> true;
has_method('mkdir:') -> true;
has_method('mkdirAll:') -> true;
has_method('listDirectory:') -> true;
has_method('delete:') -> true;
has_method('deleteAll:') -> true;
has_method('rename:to:') -> true;
has_method('absolutePath:') -> true;
has_method('tempDirectory') -> true;
has_method(_) -> false.

%% @doc Check if FileHandle responds to the given selector.
-spec handle_has_method(atom()) -> boolean().
handle_has_method('lines') -> true;
handle_has_method(_) -> false.

%%% ============================================================================
%%% File Streaming (BT-513)
%%% ============================================================================

%% @doc Return a lazy Stream of lines from a file.
%%
%% Opens the file handle and returns a Stream whose generator reads one line
%% at a time via file:read_line/1. The handle closes automatically when the
%% stream is exhausted. If the stream is abandoned, the BEAM's process-linked
%% file handle ensures cleanup when the owning process exits.
%%
%% Cross-process constraint: file-backed Streams must be consumed by the same
%% process that created them (BEAM file handles are process-local).
-spec 'lines:'(binary()) -> map().
'lines:'(Path) when is_binary(Path) ->
    case validate_path(Path) of
        {ok, ValidPath} ->
            case file:open(ValidPath, [read, binary]) of
                {ok, Fd} ->
                    make_line_stream(Fd, Path);
                {error, enoent} ->
                    Error0 = beamtalk_error:new(file_not_found, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'lines:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Check that the file exists">>),
                    beamtalk_error:raise(Error3);
                {error, eacces} ->
                    Error0 = beamtalk_error:new(permission_denied, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'lines:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Check file permissions">>),
                    beamtalk_error:raise(Error3);
                {error, Reason} ->
                    Error0 = beamtalk_error:new(io_error, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'lines:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
                    beamtalk_error:raise(Error2)
            end;
        {error, Reason} ->
            Error0 = beamtalk_error:new(invalid_path, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'lines:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
            Hint =
                case Reason of
                    absolute_path -> <<"Use relative paths only">>;
                    directory_traversal -> <<"Use relative paths within the project">>
                end,
            Error3 = beamtalk_error:with_hint(Error2, Hint),
            beamtalk_error:raise(Error3)
    end;
'lines:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'lines:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

%% @doc Block-scoped file handle management.
%%
%% Opens the file, passes a FileHandle to the block, and ensures the handle
%% is closed when the block exits (whether normally or via exception).
%% Returns the result of the block.
-spec 'open:do:'(binary(), fun((map()) -> term())) -> term().
'open:do:'(Path, Block) when is_binary(Path), is_function(Block, 1) ->
    case validate_path(Path) of
        {ok, ValidPath} ->
            case file:open(ValidPath, [read, binary]) of
                {ok, Fd} ->
                    Handle = #{'$beamtalk_class' => 'FileHandle', fd => Fd},
                    try
                        Block(Handle)
                    after
                        file:close(Fd)
                    end;
                {error, enoent} ->
                    Error0 = beamtalk_error:new(file_not_found, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'open:do:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Check that the file exists">>),
                    beamtalk_error:raise(Error3);
                {error, eacces} ->
                    Error0 = beamtalk_error:new(permission_denied, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'open:do:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Check file permissions">>),
                    beamtalk_error:raise(Error3);
                {error, Reason} ->
                    Error0 = beamtalk_error:new(io_error, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'open:do:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
                    beamtalk_error:raise(Error2)
            end;
        {error, Reason} ->
            Error0 = beamtalk_error:new(invalid_path, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'open:do:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
            Hint =
                case Reason of
                    absolute_path -> <<"Use relative paths only">>;
                    directory_traversal -> <<"Use relative paths within the project">>
                end,
            Error3 = beamtalk_error:with_hint(Error2, Hint),
            beamtalk_error:raise(Error3)
    end;
'open:do:'(Path, _) when is_binary(Path) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'open:do:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Expected a Block with 1 argument">>),
    beamtalk_error:raise(Error2);
'open:do:'(_, _) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'open:do:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

%%% ============================================================================
%%% Directory Operations (BT-1120)
%%% ============================================================================

%% @doc Test if a path refers to a directory.
%%
%% Returns true if path exists and is a directory, false otherwise.
%% Does not raise errors (returns false for invalid paths).
-spec 'isDirectory:'(binary()) -> boolean().
'isDirectory:'(Path) when is_binary(Path) ->
    case validate_path(Path) of
        {ok, ValidPath} ->
            filelib:is_dir(ValidPath);
        {error, _} ->
            false
    end;
'isDirectory:'(_) ->
    false.

%% @doc Test if a path refers to a regular file.
%%
%% Returns true if path exists and is a regular file, false otherwise.
%% Does not raise errors (returns false for invalid paths).
-spec 'isFile:'(binary()) -> boolean().
'isFile:'(Path) when is_binary(Path) ->
    case validate_path(Path) of
        {ok, ValidPath} ->
            filelib:is_regular(ValidPath);
        {error, _} ->
            false
    end;
'isFile:'(_) ->
    false.

%% @doc Create a directory. Raises an error if the parent does not exist.
%%
%% Returns nil on success, raises a structured error on failure.
-spec 'mkdir:'(binary()) -> 'nil'.
'mkdir:'(Path) when is_binary(Path) ->
    case validate_path(Path) of
        {ok, ValidPath} ->
            case file:make_dir(ValidPath) of
                ok ->
                    nil;
                {error, enoent} ->
                    Error0 = beamtalk_error:new(directory_not_found, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'mkdir:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(
                        Error2, <<"Parent directory does not exist">>
                    ),
                    beamtalk_error:raise(Error3);
                {error, eexist} ->
                    Error0 = beamtalk_error:new(already_exists, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'mkdir:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Directory already exists">>),
                    beamtalk_error:raise(Error3);
                {error, eacces} ->
                    Error0 = beamtalk_error:new(permission_denied, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'mkdir:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Check directory permissions">>),
                    beamtalk_error:raise(Error3);
                {error, Reason} ->
                    Error0 = beamtalk_error:new(io_error, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'mkdir:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
                    beamtalk_error:raise(Error2)
            end;
        {error, Reason} ->
            raise_invalid_path(Reason, Path, 'mkdir:')
    end;
'mkdir:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'mkdir:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

%% @doc Create a directory and all missing parent directories.
%%
%% Returns nil on success, raises a structured error on failure.
-spec 'mkdirAll:'(binary()) -> 'nil'.
'mkdirAll:'(Path) when is_binary(Path) ->
    case validate_path(Path) of
        {ok, ValidPath} ->
            %% filelib:ensure_path/1 creates the full path including the final component
            case filelib:ensure_path(ValidPath) of
                ok ->
                    nil;
                {error, eacces} ->
                    Error0 = beamtalk_error:new(permission_denied, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'mkdirAll:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Check directory permissions">>),
                    beamtalk_error:raise(Error3);
                {error, Reason} ->
                    Error0 = beamtalk_error:new(io_error, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'mkdirAll:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
                    beamtalk_error:raise(Error2)
            end;
        {error, Reason} ->
            raise_invalid_path(Reason, Path, 'mkdirAll:')
    end;
'mkdirAll:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'mkdirAll:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

%% @doc List entries in a directory as a List (Beamtalk array) of Strings.
%%
%% Returns only entry names (not full paths). Raises a structured error
%% if the directory does not exist or cannot be read.
-spec 'listDirectory:'(binary()) -> list().
'listDirectory:'(Path) when is_binary(Path) ->
    case validate_path(Path) of
        {ok, ValidPath} ->
            case file:list_dir(ValidPath) of
                {ok, Entries} ->
                    [list_to_binary(E) || E <- Entries];
                {error, enoent} ->
                    Error0 = beamtalk_error:new(directory_not_found, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'listDirectory:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(
                        Error2, <<"Check that the directory exists">>
                    ),
                    beamtalk_error:raise(Error3);
                {error, enotdir} ->
                    Error0 = beamtalk_error:new(not_a_directory, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'listDirectory:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Path is not a directory">>),
                    beamtalk_error:raise(Error3);
                {error, eacces} ->
                    Error0 = beamtalk_error:new(permission_denied, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'listDirectory:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Check directory permissions">>),
                    beamtalk_error:raise(Error3);
                {error, Reason} ->
                    Error0 = beamtalk_error:new(io_error, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'listDirectory:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
                    beamtalk_error:raise(Error2)
            end;
        {error, Reason} ->
            raise_invalid_path(Reason, Path, 'listDirectory:')
    end;
'listDirectory:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'listDirectory:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

%% @doc Delete a file or empty directory.
%%
%% Returns nil on success, raises a structured error on failure.
%% Uses filelib:is_dir/1 to distinguish directories from files, because
%% file:delete/1 returns {error, eperm} for directories on Linux
%% (not {error, eisdir} as documented in some OTP versions).
-spec 'delete:'(binary()) -> 'nil'.
'delete:'(Path) when is_binary(Path) ->
    case validate_path(Path) of
        {ok, ValidPath} ->
            case filelib:is_dir(ValidPath) of
                true ->
                    %% It's a directory — del_dir only succeeds if empty
                    case file:del_dir(ValidPath) of
                        ok ->
                            nil;
                        {error, DirReason} when
                            DirReason =:= enotempty; DirReason =:= eexist
                        ->
                            %% enotempty on POSIX, eexist on some OTP/Linux versions
                            Error0 = beamtalk_error:new(not_empty, 'File'),
                            Error1 = beamtalk_error:with_selector(Error0, 'delete:'),
                            Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                            Error3 = beamtalk_error:with_hint(
                                Error2, <<"Directory is not empty; use deleteAll:">>
                            ),
                            beamtalk_error:raise(Error3);
                        {error, eacces} ->
                            Error0 = beamtalk_error:new(permission_denied, 'File'),
                            Error1 = beamtalk_error:with_selector(Error0, 'delete:'),
                            Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                            Error3 = beamtalk_error:with_hint(Error2, <<"Check permissions">>),
                            beamtalk_error:raise(Error3);
                        {error, DirReason} ->
                            Error0 = beamtalk_error:new(io_error, 'File'),
                            Error1 = beamtalk_error:with_selector(Error0, 'delete:'),
                            Error2 = beamtalk_error:with_details(Error1, #{
                                path => Path, reason => DirReason
                            }),
                            beamtalk_error:raise(Error2)
                    end;
                false ->
                    %% Treat as a file (or non-existent path)
                    case file:delete(ValidPath) of
                        ok ->
                            nil;
                        {error, enoent} ->
                            Error0 = beamtalk_error:new(file_not_found, 'File'),
                            Error1 = beamtalk_error:with_selector(Error0, 'delete:'),
                            Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                            Error3 = beamtalk_error:with_hint(
                                Error2, <<"File or directory does not exist">>
                            ),
                            beamtalk_error:raise(Error3);
                        {error, eacces} ->
                            Error0 = beamtalk_error:new(permission_denied, 'File'),
                            Error1 = beamtalk_error:with_selector(Error0, 'delete:'),
                            Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                            Error3 = beamtalk_error:with_hint(Error2, <<"Check file permissions">>),
                            beamtalk_error:raise(Error3);
                        {error, Reason} ->
                            Error0 = beamtalk_error:new(io_error, 'File'),
                            Error1 = beamtalk_error:with_selector(Error0, 'delete:'),
                            Error2 = beamtalk_error:with_details(Error1, #{
                                path => Path, reason => Reason
                            }),
                            beamtalk_error:raise(Error2)
                    end
            end;
        {error, Reason} ->
            raise_invalid_path(Reason, Path, 'delete:')
    end;
'delete:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'delete:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

%% @doc Recursively delete a directory tree.
%%
%% Returns nil on success, raises a structured error on failure.
-spec 'deleteAll:'(binary()) -> 'nil'.
'deleteAll:'(Path) when is_binary(Path) ->
    case validate_path(Path) of
        {ok, ValidPath} ->
            case file:del_dir_r(ValidPath) of
                ok ->
                    nil;
                {error, enoent} ->
                    Error0 = beamtalk_error:new(file_not_found, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'deleteAll:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Path does not exist">>),
                    beamtalk_error:raise(Error3);
                {error, eacces} ->
                    Error0 = beamtalk_error:new(permission_denied, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'deleteAll:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Check permissions">>),
                    beamtalk_error:raise(Error3);
                {error, Reason} ->
                    Error0 = beamtalk_error:new(io_error, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'deleteAll:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
                    beamtalk_error:raise(Error2)
            end;
        {error, Reason} ->
            raise_invalid_path(Reason, Path, 'deleteAll:')
    end;
'deleteAll:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'deleteAll:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

%% @doc Rename or move a file or directory.
%%
%% Returns nil on success, raises a structured error on failure.
-spec 'rename:to:'(binary(), binary()) -> 'nil'.
'rename:to:'(From, To) when is_binary(From), is_binary(To) ->
    case {validate_path(From), validate_path(To)} of
        {{ok, ValidFrom}, {ok, ValidTo}} ->
            case file:rename(ValidFrom, ValidTo) of
                ok ->
                    nil;
                {error, enoent} ->
                    Error0 = beamtalk_error:new(file_not_found, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'rename:to:'),
                    Error2 = beamtalk_error:with_details(Error1, #{from => From, to => To}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Source path does not exist">>),
                    beamtalk_error:raise(Error3);
                {error, eacces} ->
                    Error0 = beamtalk_error:new(permission_denied, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'rename:to:'),
                    Error2 = beamtalk_error:with_details(Error1, #{from => From, to => To}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Check permissions">>),
                    beamtalk_error:raise(Error3);
                {error, Reason} ->
                    Error0 = beamtalk_error:new(io_error, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'rename:to:'),
                    Error2 = beamtalk_error:with_details(Error1, #{
                        from => From, to => To, reason => Reason
                    }),
                    beamtalk_error:raise(Error2)
            end;
        {{error, Reason}, _} ->
            raise_invalid_path(Reason, From, 'rename:to:');
        {_, {error, Reason}} ->
            raise_invalid_path(Reason, To, 'rename:to:')
    end;
'rename:to:'(From, _) when is_binary(From) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'rename:to:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Destination must be a String">>),
    beamtalk_error:raise(Error2);
'rename:to:'(_, _) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'rename:to:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

%% @doc Resolve a relative path to its absolute path.
%%
%% Returns the absolute path as a String.
-spec 'absolutePath:'(binary()) -> binary().
'absolutePath:'(Path) when is_binary(Path) ->
    case validate_path(Path) of
        {ok, ValidPath} ->
            AbsPath = filename:absname(ValidPath),
            list_to_binary(AbsPath);
        {error, Reason} ->
            raise_invalid_path(Reason, Path, 'absolutePath:')
    end;
'absolutePath:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'absolutePath:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

%% @doc Return the OS temporary directory path.
%%
%% Returns the system temp directory as a String.
-spec 'tempDirectory'() -> binary().
'tempDirectory'() ->
    %% Check standard environment variables, fall back to /tmp
    Dir =
        case os:getenv("TMPDIR") of
            false ->
                case os:getenv("TMP") of
                    false ->
                        case os:getenv("TEMP") of
                            false -> "/tmp";
                            V -> V
                        end;
                    V ->
                        V
                end;
            V ->
                V
        end,
    list_to_binary(Dir).

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

%% @private
%% @doc Raise a structured invalid_path error for security violations.
-spec raise_invalid_path(term(), binary(), atom()) -> no_return().
raise_invalid_path(Reason, Path, Selector) ->
    Error0 = beamtalk_error:new(invalid_path, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
    Hint =
        case Reason of
            absolute_path -> <<"Use relative paths only">>;
            directory_traversal -> <<"Use relative paths within the project">>
        end,
    Error3 = beamtalk_error:with_hint(Error2, Hint),
    beamtalk_error:raise(Error3).

%% @doc Return a lazy Stream of lines from a FileHandle.
%%
%% Used within File open:do: blocks. The Stream reads lines from the
%% already-open file handle. The handle's lifetime is managed by open:do:.
-spec handle_lines(map()) -> map().
handle_lines(#{'$beamtalk_class' := 'FileHandle', fd := Fd}) ->
    make_line_stream_from_fd(Fd);
handle_lines(_) ->
    Error0 = beamtalk_error:new(type_error, 'FileHandle'),
    Error1 = beamtalk_error:with_selector(Error0, 'lines'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Expected a FileHandle">>),
    beamtalk_error:raise(Error2).

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @private
%% @doc Validate and normalize a file path for security.
%%
%% Security checks:
%% - Reject absolute paths (must be relative)
%% - Reject paths with ".." (directory traversal)
%% - Convert to string for file operations
%%
%% Note: This does not protect against symbolic links, which is
%% acceptable for a development language. Production systems should
%% use additional OS-level protections.
%%
%% Returns {ok, ValidPath} or {error, Reason}.
-spec validate_path(binary()) -> {ok, string()} | {error, term()}.
validate_path(Path) when is_binary(Path) ->
    PathStr = binary_to_list(Path),
    %% Check for absolute paths (Unix or Windows style)
    case PathStr of
        [$/ | _] ->
            {error, absolute_path};
        [$\\ | _] ->
            {error, absolute_path};
        [Drive, $: | _] when Drive >= $A, Drive =< $Z; Drive >= $a, Drive =< $z ->
            {error, absolute_path};
        _ ->
            %% Check for directory traversal attempts
            Components = filename:split(PathStr),
            case lists:member("..", Components) of
                true ->
                    {error, directory_traversal};
                false ->
                    {ok, PathStr}
            end
    end.

%%% ============================================================================
%%% Stream Generator Helpers (BT-513)
%%% ============================================================================

%% @private
%% @doc Create a Stream of lines from a file path, with finalizer-based cleanup.
make_line_stream(Fd, Path) ->
    Gen = make_line_gen_no_close(Fd),
    Desc = iolist_to_binary([<<"File.lines('">>, Path, <<"')">>]),
    Finalizer = fun() -> file:close(Fd) end,
    beamtalk_stream:make_stream(Gen, Desc, Finalizer).

%% @private
%% @doc Create a Stream of lines from an already-open file handle.
%% Used by handle lines within open:do: blocks.
make_line_stream_from_fd(Fd) ->
    Gen = make_line_gen_no_close(Fd),
    beamtalk_stream:make_stream(Gen, <<"FileHandle.lines">>).

%% @private
%% @doc Generator that reads lines without closing (handle managed by finalizer or open:do:).
make_line_gen_no_close(Fd) ->
    fun() ->
        case file:read_line(Fd) of
            {ok, Line} ->
                Stripped = strip_newline(Line),
                {Stripped, make_line_gen_no_close(Fd)};
            eof ->
                done;
            {error, Reason} ->
                ?LOG_WARNING("File stream read error", #{reason => Reason}),
                done
        end
    end.

%% @private
%% @doc Strip trailing newline (and \r\n) from a line read by file:read_line/1.
-spec strip_newline(binary()) -> binary().
strip_newline(<<>>) ->
    <<>>;
strip_newline(Line) when is_binary(Line) ->
    case binary:last(Line) of
        $\n ->
            Trimmed = binary:part(Line, 0, byte_size(Line) - 1),
            %% Also strip \r for Windows-style \r\n line endings
            case byte_size(Trimmed) > 0 andalso binary:last(Trimmed) of
                $\r -> binary:part(Trimmed, 0, byte_size(Trimmed) - 1);
                _ -> Trimmed
            end;
        _ ->
            Line
    end.
