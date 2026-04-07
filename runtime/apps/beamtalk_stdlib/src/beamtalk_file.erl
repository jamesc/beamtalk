%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_file).

%%% **DDD Context:** Object System Context

-moduledoc """
File class implementation - file system I/O operations.

File provides basic file I/O operations wrapping Erlang's file module.
All operations are class methods. Error handling uses structured
beamtalk_error records.

## Methods

| Selector                     | Description                            |
|------------------------------|----------------------------------------|
| `cwd`                        | Current working directory (String)     |
| `exists:`                    | Check if file exists (returns Bool)    |
| `readAll:`                   | Read entire file as String             |
| `writeAll:contents:`         | Write string to file                   |
| `readBinary:`                | Read entire file as raw binary         |
| `writeBinary:contents:`      | Write binary data to file              |
| `appendBinary:contents:`     | Append binary data to file             |
| `lines:`                     | Lazy Stream of lines (constant memory) |
| `open:do:`                   | Block-scoped handle with auto-close    |

## Usage

```beamtalk
File exists: 'test.txt'
File readAll: 'test.txt'
File writeAll: 'output.txt' contents: 'hello world'
(File lines: 'data.csv') do: [:line | Transcript show: line]
File open: 'data.csv' do: [:handle | handle lines take: 10]
```

## Security

File operations use OS-level permissions. No path restrictions are
enforced — Beamtalk is a trusted developer tool (ADR 0058, 0063).
""".

-export([
    'exists:'/1,
    'readAll:'/1,
    'writeAll:contents:'/2,
    'readBinary:'/1,
    'writeBinary:contents:'/2,
    'appendBinary:contents:'/2,
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
    'lastModified:'/1,
    'cwd'/0,
    'tempDirectory'/0
]).
-export([handle_lines/1, handle_has_method/1]).

-type file_handle() :: #{'$beamtalk_class' := 'FileHandle', fd := file:io_device()}.
-export_type([file_handle/0]).

%% FFI shims for (Erlang beamtalk_file) dispatch
-export([
    exists/1,
    readAll/1,
    writeAll/2,
    readBinary/1,
    writeBinary/2,
    appendBinary/2,
    lines/1,
    open/2,
    isDirectory/1,
    isFile/1,
    mkdir/1,
    mkdirAll/1,
    listDirectory/1,
    delete/1,
    deleteAll/1,
    rename/2,
    absolutePath/1,
    lastModified/1,
    handleLines/1
]).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% Public API (called directly by codegen)
%%% ============================================================================

-doc """
Check if a file exists.

Returns true if the file exists, false otherwise.
Does not raise errors (returns false for non-existent paths or non-binary input).
""".
-spec 'exists:'(binary()) -> boolean().
'exists:'(Path) when is_binary(Path) ->
    filelib:is_regular(unicode:characters_to_list(Path));
'exists:'(_) ->
    false.

-doc """
Read entire file contents as a String.

Returns a Result ok map with the file contents as a binary (String), or a
Result error map if the file cannot be read.
""".
-spec 'readAll:'(binary()) -> beamtalk_result:t().
'readAll:'(Path) when is_binary(Path) ->
    case file:read_file(unicode:characters_to_list(Path)) of
        {ok, Contents} ->
            beamtalk_result:from_tagged_tuple({ok, Contents});
        {error, enoent} ->
            Error0 = beamtalk_error:new(file_not_found, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'readAll:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Check that the file exists">>),
            beamtalk_result:from_tagged_tuple({error, Error3});
        {error, eacces} ->
            Error0 = beamtalk_error:new(permission_denied, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'readAll:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Check file permissions">>),
            beamtalk_result:from_tagged_tuple({error, Error3});
        {error, Reason} ->
            Error0 = beamtalk_error:new(io_error, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'readAll:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
            beamtalk_result:from_tagged_tuple({error, Error2})
    end;
'readAll:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'readAll:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

-doc """
Write string contents to a file.

Creates the file if it doesn't exist, overwrites if it does.
Returns a Result ok map on success, Result error map on failure.
""".
-spec 'writeAll:contents:'(binary(), binary()) -> beamtalk_result:t().
'writeAll:contents:'(Path, Contents) when is_binary(Path), is_binary(Contents) ->
    PathStr = unicode:characters_to_list(Path),
    %% Ensure directory exists
    Dir = filename:dirname(PathStr),
    case filelib:ensure_dir(filename:join(Dir, "dummy")) of
        ok ->
            case file:write_file(PathStr, Contents) of
                ok ->
                    beamtalk_result:from_tagged_tuple({ok, nil});
                {error, eacces} ->
                    Error0 = beamtalk_error:new(permission_denied, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'writeAll:contents:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Check file permissions">>),
                    beamtalk_result:from_tagged_tuple({error, Error3});
                {error, Reason} ->
                    Error0 = beamtalk_error:new(io_error, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'writeAll:contents:'),
                    Error2 = beamtalk_error:with_details(Error1, #{
                        path => Path, reason => Reason
                    }),
                    beamtalk_result:from_tagged_tuple({error, Error2})
            end;
        {error, Reason} ->
            Error0 = beamtalk_error:new(io_error, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'writeAll:contents:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Could not create directory">>),
            beamtalk_result:from_tagged_tuple({error, Error3})
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

%%% ============================================================================
%%% Binary I/O (BT-1555)
%%% ============================================================================

-doc """
Read entire file contents as raw binary.

Returns a Result ok map with the file contents as a binary, or a
Result error map if the file cannot be read. Unlike readAll:, this
does not assume the contents are a UTF-8 string.
""".
-spec 'readBinary:'(binary()) -> beamtalk_result:t().
'readBinary:'(Path) when is_binary(Path) ->
    case file:read_file(unicode:characters_to_list(Path)) of
        {ok, Contents} ->
            beamtalk_result:from_tagged_tuple({ok, Contents});
        {error, enoent} ->
            Error0 = beamtalk_error:new(file_not_found, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'readBinary:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Check that the file exists">>),
            beamtalk_result:from_tagged_tuple({error, Error3});
        {error, eacces} ->
            Error0 = beamtalk_error:new(permission_denied, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'readBinary:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Check file permissions">>),
            beamtalk_result:from_tagged_tuple({error, Error3});
        {error, Reason} ->
            Error0 = beamtalk_error:new(io_error, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'readBinary:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
            beamtalk_result:from_tagged_tuple({error, Error2})
    end;
'readBinary:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'readBinary:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

-doc """
Write binary data to a file, creating or overwriting it.

Creates the file if it doesn't exist, overwrites if it does.
Auto-creates parent directories. Contents must be a binary.
Returns a Result ok map on success, Result error map on failure.
""".
-spec 'writeBinary:contents:'(binary(), binary()) -> beamtalk_result:t().
'writeBinary:contents:'(Path, Contents) when is_binary(Path), is_binary(Contents) ->
    PathStr = unicode:characters_to_list(Path),
    Dir = filename:dirname(PathStr),
    case filelib:ensure_dir(filename:join(Dir, "dummy")) of
        ok ->
            case file:write_file(PathStr, Contents, [raw]) of
                ok ->
                    beamtalk_result:from_tagged_tuple({ok, nil});
                {error, eacces} ->
                    Error0 = beamtalk_error:new(permission_denied, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'writeBinary:contents:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Check file permissions">>),
                    beamtalk_result:from_tagged_tuple({error, Error3});
                {error, Reason} ->
                    Error0 = beamtalk_error:new(io_error, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'writeBinary:contents:'),
                    Error2 = beamtalk_error:with_details(Error1, #{
                        path => Path, reason => Reason
                    }),
                    beamtalk_result:from_tagged_tuple({error, Error2})
            end;
        {error, Reason} ->
            Error0 = beamtalk_error:new(io_error, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'writeBinary:contents:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Could not create directory">>),
            beamtalk_result:from_tagged_tuple({error, Error3})
    end;
'writeBinary:contents:'(Path, _) when is_binary(Path) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'writeBinary:contents:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Contents must be a binary">>),
    beamtalk_error:raise(Error2);
'writeBinary:contents:'(_, _) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'writeBinary:contents:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

-doc """
Append binary data to a file, creating it if it doesn't exist.

Opens the file in append mode and writes the binary contents.
Auto-creates parent directories. Contents must be a binary.
Returns a Result ok map on success, Result error map on failure.
""".
-spec 'appendBinary:contents:'(binary(), binary()) -> beamtalk_result:t().
'appendBinary:contents:'(Path, Contents) when is_binary(Path), is_binary(Contents) ->
    PathStr = unicode:characters_to_list(Path),
    Dir = filename:dirname(PathStr),
    case filelib:ensure_dir(filename:join(Dir, "dummy")) of
        ok ->
            case file:write_file(PathStr, Contents, [append, raw]) of
                ok ->
                    beamtalk_result:from_tagged_tuple({ok, nil});
                {error, eacces} ->
                    Error0 = beamtalk_error:new(permission_denied, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'appendBinary:contents:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Check file permissions">>),
                    beamtalk_result:from_tagged_tuple({error, Error3});
                {error, Reason} ->
                    Error0 = beamtalk_error:new(io_error, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'appendBinary:contents:'),
                    Error2 = beamtalk_error:with_details(Error1, #{
                        path => Path, reason => Reason
                    }),
                    beamtalk_result:from_tagged_tuple({error, Error2})
            end;
        {error, Reason} ->
            Error0 = beamtalk_error:new(io_error, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'appendBinary:contents:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Could not create directory">>),
            beamtalk_result:from_tagged_tuple({error, Error3})
    end;
'appendBinary:contents:'(Path, _) when is_binary(Path) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'appendBinary:contents:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Contents must be a binary">>),
    beamtalk_error:raise(Error2);
'appendBinary:contents:'(_, _) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'appendBinary:contents:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

-doc "Check if FileHandle responds to the given selector.".
-spec handle_has_method(atom()) -> boolean().
handle_has_method('lines') -> true;
handle_has_method(_) -> false.

%%% ============================================================================
%%% File Streaming (BT-513)
%%% ============================================================================

-doc """
Return a lazy Stream of lines from a file.

Opens the file handle and returns a Result ok map with a Stream whose
generator reads one line at a time via file:read_line/1. The handle closes
automatically when the stream is exhausted. If the stream is abandoned, the
BEAM's process-linked file handle ensures cleanup when the owning process exits.

Cross-process constraint: file-backed Streams must be consumed by the same
process that created them (BEAM file handles are process-local).
""".
-spec 'lines:'(binary()) -> beamtalk_result:t().
'lines:'(Path) when is_binary(Path) ->
    case file:open(unicode:characters_to_list(Path), [read, binary]) of
        {ok, Fd} ->
            beamtalk_result:from_tagged_tuple({ok, make_line_stream(Fd, Path)});
        {error, enoent} ->
            Error0 = beamtalk_error:new(file_not_found, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'lines:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Check that the file exists">>),
            beamtalk_result:from_tagged_tuple({error, Error3});
        {error, eacces} ->
            Error0 = beamtalk_error:new(permission_denied, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'lines:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Check file permissions">>),
            beamtalk_result:from_tagged_tuple({error, Error3});
        {error, Reason} ->
            Error0 = beamtalk_error:new(io_error, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'lines:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
            beamtalk_result:from_tagged_tuple({error, Error2})
    end;
'lines:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'lines:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

-doc """
Block-scoped file handle management.

Opens the file, passes a FileHandle to the block, and ensures the handle
is closed when the block exits (whether normally or via exception).
Returns a Result ok map with the result of the block.
""".
-spec 'open:do:'(binary(), fun((map()) -> term())) -> beamtalk_result:t().
'open:do:'(Path, Block) when is_binary(Path), is_function(Block, 1) ->
    case file:open(unicode:characters_to_list(Path), [read, binary]) of
        {ok, Fd} ->
            Handle = #{'$beamtalk_class' => 'FileHandle', fd => Fd},
            try
                BlockResult = Block(Handle),
                beamtalk_result:from_tagged_tuple({ok, BlockResult})
            after
                file:close(Fd)
            end;
        {error, enoent} ->
            Error0 = beamtalk_error:new(file_not_found, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'open:do:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Check that the file exists">>),
            beamtalk_result:from_tagged_tuple({error, Error3});
        {error, eacces} ->
            Error0 = beamtalk_error:new(permission_denied, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'open:do:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Check file permissions">>),
            beamtalk_result:from_tagged_tuple({error, Error3});
        {error, Reason} ->
            Error0 = beamtalk_error:new(io_error, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'open:do:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
            beamtalk_result:from_tagged_tuple({error, Error2})
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

-doc """
Test if a path refers to a directory.

Returns true if path exists and is a directory, false otherwise.
Does not raise errors (returns false for non-existent paths or non-binary input).
""".
-spec 'isDirectory:'(binary()) -> boolean().
'isDirectory:'(Path) when is_binary(Path) ->
    filelib:is_dir(unicode:characters_to_list(Path));
'isDirectory:'(_) ->
    false.

-doc """
Test if a path refers to a regular file.

Returns true if path exists and is a regular file, false otherwise.
Does not raise errors (returns false for non-existent paths or non-binary input).
""".
-spec 'isFile:'(binary()) -> boolean().
'isFile:'(Path) when is_binary(Path) ->
    filelib:is_regular(unicode:characters_to_list(Path));
'isFile:'(_) ->
    false.

-doc """
Create a directory. Returns a Result error if the parent does not exist.

Returns a Result ok map on success, Result error map on failure.
""".
-spec 'mkdir:'(binary()) -> beamtalk_result:t().
'mkdir:'(Path) when is_binary(Path) ->
    case file:make_dir(unicode:characters_to_list(Path)) of
        ok ->
            beamtalk_result:from_tagged_tuple({ok, nil});
        {error, enoent} ->
            Error0 = beamtalk_error:new(directory_not_found, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'mkdir:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
            Error3 = beamtalk_error:with_hint(
                Error2, <<"Parent directory does not exist">>
            ),
            beamtalk_result:from_tagged_tuple({error, Error3});
        {error, eexist} ->
            Error0 = beamtalk_error:new(already_exists, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'mkdir:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Directory already exists">>),
            beamtalk_result:from_tagged_tuple({error, Error3});
        {error, eacces} ->
            Error0 = beamtalk_error:new(permission_denied, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'mkdir:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Check directory permissions">>),
            beamtalk_result:from_tagged_tuple({error, Error3});
        {error, Reason} ->
            Error0 = beamtalk_error:new(io_error, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'mkdir:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
            beamtalk_result:from_tagged_tuple({error, Error2})
    end;
'mkdir:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'mkdir:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

-doc """
Create a directory and all missing parent directories.

Returns a Result ok map on success, Result error map on failure.
""".
-spec 'mkdirAll:'(binary()) -> beamtalk_result:t().
'mkdirAll:'(Path) when is_binary(Path) ->
    %% filelib:ensure_path/1 creates the full path including the final component
    case filelib:ensure_path(unicode:characters_to_list(Path)) of
        ok ->
            beamtalk_result:from_tagged_tuple({ok, nil});
        {error, eacces} ->
            Error0 = beamtalk_error:new(permission_denied, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'mkdirAll:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Check directory permissions">>),
            beamtalk_result:from_tagged_tuple({error, Error3});
        {error, Reason} ->
            Error0 = beamtalk_error:new(io_error, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'mkdirAll:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
            beamtalk_result:from_tagged_tuple({error, Error2})
    end;
'mkdirAll:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'mkdirAll:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

-doc """
List entries in a directory as a List (Beamtalk array) of Strings.

Returns only entry names (not full paths). Returns a Result error map
if the directory does not exist or cannot be read.
""".
-spec 'listDirectory:'(binary()) -> beamtalk_result:t().
'listDirectory:'(Path) when is_binary(Path) ->
    PathStr = unicode:characters_to_list(Path),
    %% Check for regular file first: file:list_dir/1 returns different
    %% error codes on different OSes when given a file path. By checking
    %% filelib:is_regular/1 upfront we get a consistent not_a_directory
    %% error on all platforms.
    case filelib:is_regular(PathStr) of
        true ->
            Error0 = beamtalk_error:new(not_a_directory, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'listDirectory:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Path is not a directory">>),
            beamtalk_result:from_tagged_tuple({error, Error3});
        false ->
            case file:list_dir(PathStr) of
                {ok, Entries} ->
                    beamtalk_result:from_tagged_tuple(
                        {ok, [unicode:characters_to_binary(E) || E <- Entries]}
                    );
                {error, enoent} ->
                    Error0 = beamtalk_error:new(directory_not_found, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'listDirectory:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(
                        Error2, <<"Check that the directory exists">>
                    ),
                    beamtalk_result:from_tagged_tuple({error, Error3});
                {error, enotdir} ->
                    Error0 = beamtalk_error:new(not_a_directory, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'listDirectory:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(
                        Error2, <<"Path is not a directory">>
                    ),
                    beamtalk_result:from_tagged_tuple({error, Error3});
                {error, eacces} ->
                    Error0 = beamtalk_error:new(permission_denied, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'listDirectory:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(
                        Error2, <<"Check directory permissions">>
                    ),
                    beamtalk_result:from_tagged_tuple({error, Error3});
                {error, Reason} ->
                    Error0 = beamtalk_error:new(io_error, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'listDirectory:'),
                    Error2 = beamtalk_error:with_details(Error1, #{
                        path => Path, reason => Reason
                    }),
                    beamtalk_result:from_tagged_tuple({error, Error2})
            end
    end;
'listDirectory:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'listDirectory:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

-doc """
Delete a file or empty directory.

Returns a Result ok map on success, Result error map on failure.
Uses filelib:is_dir/1 to distinguish directories from files, because
file:delete/1 returns {error, eperm} for directories on Linux
(not {error, eisdir} as documented in some OTP versions).
""".
-spec 'delete:'(binary()) -> beamtalk_result:t().
'delete:'(Path) when is_binary(Path) ->
    PathStr = unicode:characters_to_list(Path),
    case filelib:is_dir(PathStr) of
        true ->
            %% It's a directory — del_dir only succeeds if empty
            case file:del_dir(PathStr) of
                ok ->
                    beamtalk_result:from_tagged_tuple({ok, nil});
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
                    beamtalk_result:from_tagged_tuple({error, Error3});
                {error, eacces} ->
                    Error0 = beamtalk_error:new(permission_denied, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'delete:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Check permissions">>),
                    beamtalk_result:from_tagged_tuple({error, Error3});
                {error, DirReason} ->
                    Error0 = beamtalk_error:new(io_error, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'delete:'),
                    Error2 = beamtalk_error:with_details(Error1, #{
                        path => Path, reason => DirReason
                    }),
                    beamtalk_result:from_tagged_tuple({error, Error2})
            end;
        false ->
            %% Treat as a file (or non-existent path)
            case file:delete(PathStr) of
                ok ->
                    beamtalk_result:from_tagged_tuple({ok, nil});
                {error, enoent} ->
                    Error0 = beamtalk_error:new(file_not_found, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'delete:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(
                        Error2, <<"File or directory does not exist">>
                    ),
                    beamtalk_result:from_tagged_tuple({error, Error3});
                {error, eacces} ->
                    Error0 = beamtalk_error:new(permission_denied, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'delete:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Check file permissions">>),
                    beamtalk_result:from_tagged_tuple({error, Error3});
                {error, Reason} ->
                    Error0 = beamtalk_error:new(io_error, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'delete:'),
                    Error2 = beamtalk_error:with_details(Error1, #{
                        path => Path, reason => Reason
                    }),
                    beamtalk_result:from_tagged_tuple({error, Error2})
            end
    end;
'delete:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'delete:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

-doc """
Recursively delete a directory tree.

Returns a Result ok map on success, Result error map on failure.
""".
-spec 'deleteAll:'(binary()) -> beamtalk_result:t().
'deleteAll:'(Path) when is_binary(Path) ->
    case file:del_dir_r(unicode:characters_to_list(Path)) of
        ok ->
            beamtalk_result:from_tagged_tuple({ok, nil});
        {error, enoent} ->
            Error0 = beamtalk_error:new(file_not_found, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'deleteAll:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Path does not exist">>),
            beamtalk_result:from_tagged_tuple({error, Error3});
        {error, eacces} ->
            Error0 = beamtalk_error:new(permission_denied, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'deleteAll:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Check permissions">>),
            beamtalk_result:from_tagged_tuple({error, Error3});
        {error, Reason} ->
            Error0 = beamtalk_error:new(io_error, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'deleteAll:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
            beamtalk_result:from_tagged_tuple({error, Error2})
    end;
'deleteAll:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'deleteAll:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

-doc """
Rename or move a file or directory.

Returns a Result ok map on success, Result error map on failure.
""".
-spec 'rename:to:'(binary(), binary()) -> beamtalk_result:t().
'rename:to:'(From, To) when is_binary(From), is_binary(To) ->
    case file:rename(unicode:characters_to_list(From), unicode:characters_to_list(To)) of
        ok ->
            beamtalk_result:from_tagged_tuple({ok, nil});
        {error, enoent} ->
            Error0 = beamtalk_error:new(file_not_found, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'rename:to:'),
            Error2 = beamtalk_error:with_details(Error1, #{from => From, to => To}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Source path does not exist">>),
            beamtalk_result:from_tagged_tuple({error, Error3});
        {error, eacces} ->
            Error0 = beamtalk_error:new(permission_denied, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'rename:to:'),
            Error2 = beamtalk_error:with_details(Error1, #{from => From, to => To}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Check permissions">>),
            beamtalk_result:from_tagged_tuple({error, Error3});
        {error, Reason} ->
            Error0 = beamtalk_error:new(io_error, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'rename:to:'),
            Error2 = beamtalk_error:with_details(Error1, #{
                from => From, to => To, reason => Reason
            }),
            beamtalk_result:from_tagged_tuple({error, Error2})
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

-doc """
Resolve a relative path to its absolute path.

Returns a Result ok map with the absolute path as a String.
""".
-spec 'absolutePath:'(binary()) -> beamtalk_result:t().
'absolutePath:'(Path) when is_binary(Path) ->
    PathList = unicode:characters_to_list(Path),
    case filename:pathtype(PathList) of
        absolute ->
            beamtalk_result:from_tagged_tuple({ok, Path});
        _ ->
            AbsPath = filename:absname(PathList),
            beamtalk_result:from_tagged_tuple({ok, unicode:characters_to_binary(AbsPath)})
    end;
'absolutePath:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'absolutePath:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

-doc """
Get the last modification time of a file.

Returns a Result ok map with a DateTime on success, or a Result error map
if the file does not exist.
""".
-spec 'lastModified:'(binary()) -> beamtalk_result:t().
'lastModified:'(Path) when is_binary(Path) ->
    case filelib:last_modified(unicode:characters_to_list(Path)) of
        0 ->
            Error0 = beamtalk_error:new(file_not_found, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'lastModified:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Check that the file exists">>),
            beamtalk_result:from_tagged_tuple({error, Error3});
        LocalTime ->
            %% filelib:last_modified/1 returns local time; convert to UTC
            %% for consistency with DateTime now (which uses calendar:universal_time).
            {{Y, Mo, D}, {H, Mi, S}} =
                case calendar:local_time_to_universal_time_dst(LocalTime) of
                    [UtcTime] -> UtcTime;
                    [_DstTime, StdTime] -> StdTime;
                    %% fallback: keep local time if conversion fails
                    [] -> LocalTime
                end,
            DT = beamtalk_datetime:'year:month:day:hour:minute:second:'(Y, Mo, D, H, Mi, S),
            beamtalk_result:from_tagged_tuple({ok, DT})
    end;
'lastModified:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'lastModified:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    beamtalk_error:raise(Error2).

-doc """
Return the current working directory.

Returns the current working directory as a String (absolute path).
""".
-spec 'cwd'() -> binary().
'cwd'() ->
    case file:get_cwd() of
        {ok, Dir} ->
            unicode:characters_to_binary(Dir);
        {error, Reason} ->
            Error0 = beamtalk_error:new(io_error, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'cwd'),
            Error2 = beamtalk_error:with_details(Error1, #{reason => Reason}),
            Error3 = beamtalk_error:with_hint(
                Error2, <<"Could not determine current working directory">>
            ),
            beamtalk_error:raise(Error3)
    end.

-doc """
Return the OS temporary directory path.

Returns the system temp directory as a String.
""".
-spec 'tempDirectory'() -> binary().
'tempDirectory'() ->
    %% Check standard environment variables, fall back to platform-appropriate temp dir
    Dir =
        case os:getenv("TMPDIR") of
            false ->
                case os:getenv("TMP") of
                    false ->
                        case os:getenv("TEMP") of
                            false ->
                                case os:type() of
                                    {win32, _} -> "C:\\Windows\\Temp";
                                    _ -> "/tmp"
                                end;
                            V ->
                                V
                        end;
                    V ->
                        V
                end;
            V ->
                V
        end,
    unicode:characters_to_binary(Dir).

%%% ============================================================================
%%% FFI Shims
%%%
%%% The (Erlang beamtalk_file) FFI uses beamtalk_erlang_proxy:direct_call/3,
%%% which derives the Erlang function name from the first keyword of the
%%% Beamtalk selector (stripping the trailing colon). These shims provide
%%% the colon-free entry points that the proxy calls:
%%%
%%%   (Erlang beamtalk_file) exists: path          → exists/1
%%%   (Erlang beamtalk_file) readAll: path         → readAll/1
%%%   (Erlang beamtalk_file) writeAll: p contents: t → writeAll/2
%%%   (Erlang beamtalk_file) readBinary: path      → readBinary/1
%%%   (Erlang beamtalk_file) writeBinary: p contents: b → writeBinary/2
%%%   (Erlang beamtalk_file) appendBinary: p contents: b → appendBinary/2
%%%   (Erlang beamtalk_file) lines: path           → lines/1
%%%   (Erlang beamtalk_file) open: path do: block  → open/2
%%%   (Erlang beamtalk_file) isDirectory: path     → isDirectory/1
%%%   (Erlang beamtalk_file) isFile: path          → isFile/1
%%%   (Erlang beamtalk_file) mkdir: path           → mkdir/1
%%%   (Erlang beamtalk_file) mkdirAll: path        → mkdirAll/1
%%%   (Erlang beamtalk_file) listDirectory: path   → listDirectory/1
%%%   (Erlang beamtalk_file) delete: path          → delete/1
%%%   (Erlang beamtalk_file) deleteAll: path       → deleteAll/1
%%%   (Erlang beamtalk_file) rename: from to: to   → rename/2
%%%   (Erlang beamtalk_file) absolutePath: path     → absolutePath/1
%%%   (Erlang beamtalk_file) lastModified: path    → lastModified/1
%%%   (Erlang beamtalk_file) handleLines: handle   → handleLines/1
%%%   (Erlang beamtalk_file) cwd                   → 'cwd'/0 (direct)
%%%   (Erlang beamtalk_file) tempDirectory         → 'tempDirectory'/0 (direct)
%%% ============================================================================

exists(Path) -> 'exists:'(Path).
readAll(Path) -> 'readAll:'(Path).
writeAll(Path, Contents) -> 'writeAll:contents:'(Path, Contents).
readBinary(Path) -> 'readBinary:'(Path).
writeBinary(Path, Contents) -> 'writeBinary:contents:'(Path, Contents).
appendBinary(Path, Contents) -> 'appendBinary:contents:'(Path, Contents).
lines(Path) -> 'lines:'(Path).
open(Path, Block) -> 'open:do:'(Path, Block).
isDirectory(Path) -> 'isDirectory:'(Path).
isFile(Path) -> 'isFile:'(Path).
mkdir(Path) -> 'mkdir:'(Path).
mkdirAll(Path) -> 'mkdirAll:'(Path).
listDirectory(Path) -> 'listDirectory:'(Path).
delete(Path) -> 'delete:'(Path).
deleteAll(Path) -> 'deleteAll:'(Path).
rename(From, To) -> 'rename:to:'(From, To).
absolutePath(Path) -> 'absolutePath:'(Path).
lastModified(Path) -> 'lastModified:'(Path).
-spec handleLines(file_handle()) -> beamtalk_stream:t().
handleLines(Handle) -> handle_lines(Handle).

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

-doc """
Return a lazy Stream of lines from a FileHandle.

Used within File open:do: blocks. The Stream reads lines from the
already-open file handle. The handle's lifetime is managed by open:do:.
""".
-spec handle_lines(file_handle()) -> beamtalk_stream:t().
handle_lines(#{'$beamtalk_class' := 'FileHandle', fd := Fd}) ->
    make_line_stream_from_fd(Fd);
handle_lines(_) ->
    Error0 = beamtalk_error:new(type_error, 'FileHandle'),
    Error1 = beamtalk_error:with_selector(Error0, 'lines'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Expected a FileHandle">>),
    beamtalk_error:raise(Error2).

%%% ============================================================================
%%% Stream Generator Helpers (BT-513)
%%% ============================================================================

-doc "Create a Stream of lines from a file path, with finalizer-based cleanup.".
make_line_stream(Fd, Path) ->
    Gen = make_line_gen_no_close(Fd),
    Desc = iolist_to_binary([<<"File.lines('">>, Path, <<"')">>]),
    Finalizer = fun() -> file:close(Fd) end,
    beamtalk_stream:make_stream(Gen, Desc, Finalizer).

-doc """
Create a Stream of lines from an already-open file handle.
Used by handle lines within open:do: blocks.
""".
make_line_stream_from_fd(Fd) ->
    Gen = make_line_gen_no_close(Fd),
    beamtalk_stream:make_stream(Gen, <<"FileHandle.lines">>).

-doc """
Generator that reads lines without closing (handle managed by finalizer or open:do:).
""".
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

-doc "Strip trailing newline (and \\r\\n) from a line read by file:read_line/1.".
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
