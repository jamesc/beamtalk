%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc File class implementation - file system I/O operations.
%%%
%%% **DDD Context:** Runtime Context
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

-export(['exists:'/1, 'readAll:'/1, 'writeAll:contents:'/2,
         'lines:'/1, 'open:do:'/2]).
-export([handle_lines/1, has_method/1, handle_has_method/1]).

-include("beamtalk.hrl").

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
            Hint = case Reason of
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
                            Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
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
            Hint = case Reason of
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
            case file:open(ValidPath, [read, binary, {encoding, utf8}]) of
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
            Hint = case Reason of
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
            case file:open(ValidPath, [read, binary, {encoding, utf8}]) of
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
            Hint = case Reason of
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
%% @doc Create a Stream of lines from a file path, with auto-close on exhaustion.
make_line_stream(Fd, Path) ->
    Gen = make_line_gen(Fd),
    Desc = iolist_to_binary([<<"File.lines('">>, Path, <<"')">>]),
    beamtalk_stream:make_stream(Gen, Desc).

%% @private
%% @doc Create a Stream of lines from an already-open file handle.
%% Used by handle lines within open:do: blocks.
make_line_stream_from_fd(Fd) ->
    Gen = make_line_gen_no_close(Fd),
    beamtalk_stream:make_stream(Gen, <<"FileHandle.lines">>).

%% @private
%% @doc Generator that reads lines and closes the handle on exhaustion.
make_line_gen(Fd) ->
    fun() ->
        case file:read_line(Fd) of
            {ok, Line} ->
                %% Strip trailing newline
                Stripped = strip_newline(Line),
                {Stripped, make_line_gen(Fd)};
            eof ->
                file:close(Fd),
                done;
            {error, Reason} ->
                logger:warning("File stream read error", #{reason => Reason}),
                file:close(Fd),
                done
        end
    end.

%% @private
%% @doc Generator that reads lines without closing (handle managed by open:do:).
make_line_gen_no_close(Fd) ->
    fun() ->
        case file:read_line(Fd) of
            {ok, Line} ->
                Stripped = strip_newline(Line),
                {Stripped, make_line_gen_no_close(Fd)};
            eof ->
                done;
            {error, Reason} ->
                logger:warning("File stream read error", #{reason => Reason}),
                done
        end
    end.

%% @private
%% @doc Strip trailing newline (and \r\n) from a line read by file:read_line/1.
-spec strip_newline(binary()) -> binary().
strip_newline(<<>>) -> <<>>;
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
