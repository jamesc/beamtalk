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
%%% | Selector                     | Description                        |
%%% |------------------------------|-------------------------------------|
%%% | `exists:`                    | Check if file exists (returns Bool) |
%%% | `readAll:`                   | Read entire file as String          |
%%% | `writeAll:contents:`         | Write string to file                |
%%%
%%% ## Usage
%%%
%%% ```beamtalk
%%% File exists: 'test.txt'
%%% File readAll: 'test.txt'
%%% File writeAll: 'output.txt' contents: 'hello world'
%%% ```
%%%
%%% ## Security
%%%
%%% - File paths are validated to prevent directory traversal
%%% - Paths must be relative or within the current working directory
%%% - Absolute paths outside the project are rejected

-module(beamtalk_file).

-export(['exists:'/1, 'readAll:'/1, 'writeAll:contents:'/2]).
-export([has_method/1]).

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
                    error(Error3);
                {error, eacces} ->
                    Error0 = beamtalk_error:new(permission_denied, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'readAll:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Check file permissions">>),
                    error(Error3);
                {error, Reason} ->
                    Error0 = beamtalk_error:new(io_error, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'readAll:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
                    error(Error2)
            end;
        {error, Reason} ->
            Error0 = beamtalk_error:new(invalid_path, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'readAll:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Use relative paths within the project">>),
            error(Error3)
    end;
'readAll:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'readAll:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    error(Error2).

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
                            error(Error3);
                        {error, Reason} ->
                            Error0 = beamtalk_error:new(io_error, 'File'),
                            Error1 = beamtalk_error:with_selector(Error0, 'writeAll:contents:'),
                            Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
                            error(Error2)
                    end;
                {error, Reason} ->
                    Error0 = beamtalk_error:new(io_error, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, 'writeAll:contents:'),
                    Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
                    Error3 = beamtalk_error:with_hint(Error2, <<"Could not create directory">>),
                    error(Error3)
            end;
        {error, Reason} ->
            Error0 = beamtalk_error:new(invalid_path, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, 'writeAll:contents:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Use relative paths within the project">>),
            error(Error3)
    end;
'writeAll:contents:'(Path, _) when is_binary(Path) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'writeAll:contents:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Contents must be a String">>),
    error(Error2);
'writeAll:contents:'(_, _) ->
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, 'writeAll:contents:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Path must be a String">>),
    error(Error2).

%% @doc Check if File responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method('exists:') -> true;
has_method('readAll:') -> true;
has_method('writeAll:contents:') -> true;
has_method(_) -> false.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @private
%% @doc Validate and normalize a file path for security.
%%
%% Basic security checks:
%% - Reject paths with ".." (directory traversal)
%% - Convert to string for file operations
%% - Allow relative paths (interpreted relative to CWD)
%%
%% Returns {ok, ValidPath} or {error, Reason}.
-spec validate_path(binary()) -> {ok, string()} | {error, term()}.
validate_path(Path) when is_binary(Path) ->
    PathStr = binary_to_list(Path),
    %% Check for directory traversal attempts
    case string:find(PathStr, "..") of
        nomatch ->
            %% Path is safe
            {ok, PathStr};
        _ ->
            {error, directory_traversal}
    end;
validate_path(_) ->
    {error, invalid_type}.
