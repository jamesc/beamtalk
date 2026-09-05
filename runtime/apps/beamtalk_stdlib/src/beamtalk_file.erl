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
| `open:do:`                   | Block-scoped read handle, auto-closed  |
| `open:mode:`                 | Open a FileHandle the caller closes    |
| `open:mode:do:`              | Block-scoped handle, auto-closed       |

## Usage

```beamtalk
File exists: 'test.txt'
File readAll: 'test.txt'
File writeAll: 'output.txt' contents: 'hello world'
(File lines: 'data.csv') do: [:line | Transcript show: line]
File open: 'data.csv' do: [:handle | handle lines take: 10]
File open: 'log.etf' mode: #append do: [:h | h write: record. h sync]
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
    'open:mode:'/2,
    'open:mode:do:'/3,
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
    'tempDirectory'/0,
    'openHandles'/0
]).
-export([handle_lines/1, handle_has_method/1]).

-type file_handle() :: beamtalk_file_handle:t().
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
    open/3,
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
    beamtalk_error:raise_type_error('File', 'readAll:', <<"Path must be a String">>).

-doc """
Write string contents to a file.

Creates the file if it doesn't exist, overwrites if it does.
Returns a Result ok map on success, Result error map on failure.
""".
-spec 'writeAll:contents:'(binary(), Contents :: binary()) -> beamtalk_result:t().
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
    beamtalk_error:raise_type_error('File', 'writeAll:contents:', <<"Contents must be a String">>);
'writeAll:contents:'(_, _) ->
    beamtalk_error:raise_type_error('File', 'writeAll:contents:', <<"Path must be a String">>).

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
    beamtalk_error:raise_type_error('File', 'readBinary:', <<"Path must be a String">>).

-doc """
Write binary data to a file, creating or overwriting it.

Creates the file if it doesn't exist, overwrites if it does.
Auto-creates parent directories. Contents must be a binary.
Returns a Result ok map on success, Result error map on failure.
""".
-spec 'writeBinary:contents:'(binary(), Contents :: binary()) -> beamtalk_result:t().
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
    beamtalk_error:raise_type_error(
        'File', 'writeBinary:contents:', <<"Contents must be a binary">>
    );
'writeBinary:contents:'(_, _) ->
    beamtalk_error:raise_type_error('File', 'writeBinary:contents:', <<"Path must be a String">>).

-doc """
Append binary data to a file, creating it if it doesn't exist.

Opens the file in append mode and writes the binary contents.
Auto-creates parent directories. Contents must be a binary.
Returns a Result ok map on success, Result error map on failure.
""".
-spec 'appendBinary:contents:'(binary(), Contents :: binary()) -> beamtalk_result:t().
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
    beamtalk_error:raise_type_error(
        'File', 'appendBinary:contents:', <<"Contents must be a binary">>
    );
'appendBinary:contents:'(_, _) ->
    beamtalk_error:raise_type_error('File', 'appendBinary:contents:', <<"Path must be a String">>).

-doc "Check if FileHandle responds to the given selector.".
-spec handle_has_method(atom()) -> boolean().
handle_has_method('lines') -> true;
handle_has_method('read:') -> true;
handle_has_method('readAll') -> true;
handle_has_method('write:') -> true;
handle_has_method('writeLine:') -> true;
handle_has_method('position') -> true;
handle_has_method('seek:') -> true;
handle_has_method('flush') -> true;
handle_has_method('sync') -> true;
handle_has_method('close') -> true;
handle_has_method('isOpen') -> true;
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
    beamtalk_error:raise_type_error('File', 'lines:', <<"Path must be a String">>).

-doc """
Block-scoped file handle management.

Opens the file, passes a FileHandle to the block, and ensures the handle
is closed when the block exits (whether normally or via exception).
Returns a Result ok map with the result of the block.
""".
-spec 'open:do:'(binary(), Do :: fun((map()) -> term())) -> beamtalk_result:t().
'open:do:'(Path, Block) when is_binary(Path), is_function(Block, 1) ->
    case file:open(unicode:characters_to_list(Path), [read, binary]) of
        {ok, Fd} ->
            Handle = beamtalk_file_handle:new(Fd, read, Path),
            try
                BlockResult = Block(Handle),
                beamtalk_result:from_tagged_tuple({ok, BlockResult})
            after
                beamtalk_file_handle:close_handle(Handle)
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
    beamtalk_error:raise_type_error('File', 'open:do:', <<"Expected a Block with 1 argument">>);
'open:do:'(_, _) ->
    beamtalk_error:raise_type_error('File', 'open:do:', <<"Path must be a String">>).

%%% ============================================================================
%%% Incremental handle I/O (BT-2975)
%%% ============================================================================

-doc """
Open a file in the given mode and return a FileHandle the caller owns.

Modes map onto binary `file:open/2` option sets:

| Mode         | Options                 | Behaviour                       |
|--------------|-------------------------|---------------------------------|
| `#read`      | `[read, binary]`        | Read only; file must exist      |
| `#write`     | `[write, binary]`       | Truncate or create, write only  |
| `#append`    | `[append, binary]`      | Create if absent, writes append |
| `#readWrite` | `[read, write, binary]` | Create if absent, keeps content |

Write-capable modes auto-create parent directories, matching
`writeBinary:contents:`.

The caller is responsible for `close`, but is not the sole backstop (BT-3020):
the handle is registered against a resolved *owner* — the REPL/workspace
session if there is one, else the calling Beamtalk actor, else unowned — and
`beamtalk_file_handle_registry` closes an owned handle when its owner dies.
`File openHandles` lists every outstanding handle for diagnostics regardless
of tier. Prefer `open:mode:do:` whenever a block scope will do.

Returns a Result ok map holding the handle, or a Result error map.
""".
-spec 'open:mode:'(binary(), atom()) -> beamtalk_result:t().
'open:mode:'(Path, Mode) when is_binary(Path), is_atom(Mode) ->
    case do_open(Path, Mode, 'open:mode:') of
        {ok, Handle} ->
            beamtalk_file_handle_registry:register(Handle, resolve_owner()),
            beamtalk_result:from_tagged_tuple({ok, Handle});
        {error, Error} ->
            beamtalk_result:from_tagged_tuple({error, Error})
    end;
'open:mode:'(Path, _) when is_binary(Path) ->
    beamtalk_error:raise_type_error('File', 'open:mode:', <<"Mode must be a Symbol">>);
'open:mode:'(_, _) ->
    beamtalk_error:raise_type_error('File', 'open:mode:', <<"Path must be a String">>).

-doc """
Resolve the owner `open:mode:` registers its handle against (BT-3020, decision (a)).

`open:mode:` is not call-site lowered (see `mode_options/1`), so this runs
inside the File class gen_server, where `self()` is the class process rather
than the caller. Two process-dictionary keys mirrored into *this* process for
the duration of the call stand in for "who is calling":

1. `beamtalk_session_pid` — the long-lived REPL/workspace session shell pid,
   explicitly carried by `class_send_dispatch/3` (ADR 0081 / BT-2379) so
   `Session current` works the same way from inside a class method. A pid here
   outlives the short-lived eval worker that made this particular call, so it
   survives across REPL turns — the property the rejected call-site-lowering
   plan (see `mode_options/1`) could not deliver.
2. The immediate caller's pid, via `beamtalk_object_class:dispatch_caller_pid/0`
   — mirrored by `beamtalk_object_class:dispatch_class_method/5` from the
   `From` every `handle_call` already carries (no wire-protocol change). Used
   only when there is no session: if that pid is itself a Beamtalk actor
   (`beamtalk_actor:is_beamtalk_actor/1`), the actor owns the handle.

Otherwise the handle is unowned (tier 3): registered for `openHandles`
diagnostics, reclaimed only by an explicit `close` or node shutdown.

Tier 2 is *not* transitive the way tier 1 is: the session pid is re-emitted by
`local_session_context/0` on every nested class-method call, so an actor
calling `Logger openFor: path` (say) which itself calls `File open:mode:`
still resolves the session correctly if one exists. But
`beamtalk_dispatch_caller_pid` is only ever the *immediate* caller — in that
same nested-call shape with no session, `open:mode:` sees `Logger`'s class
gen_server pid (not a Beamtalk actor), not the originating actor, and the
handle falls through to unowned. A future reader tempted to make tier 2
transitive too should know this is a known, accepted gap, not an oversight.
""".
-spec resolve_owner() -> pid() | undefined.
resolve_owner() ->
    case get(beamtalk_session_pid) of
        SessionPid when is_pid(SessionPid) ->
            SessionPid;
        _ ->
            case beamtalk_object_class:dispatch_caller_pid() of
                CallerPid when is_pid(CallerPid) ->
                    case beamtalk_actor:is_beamtalk_actor(CallerPid) of
                        true -> CallerPid;
                        false -> undefined
                    end;
                undefined ->
                    undefined
            end
    end.

-doc """
List every outstanding `open:mode:` handle for diagnostics (BT-3020).

Returns an Array of 3-element Arrays `#(path mode owner)`. `owner` is the
session/actor pid for tiers 1-2, `nil` for an unowned (tier 3) handle. Handles
from `open:do:` / `open:mode:do:` never appear — they are block-scoped and
always closed before the call returns, so there is nothing to enumerate.
""".
-spec 'openHandles'() -> map().
'openHandles'() ->
    Entries = [
        beamtalk_array:from_list([Path, Mode, owner_to_beamtalk(Owner)])
     || {Path, Mode, Owner} <- beamtalk_file_handle_registry:open_handles()
    ],
    beamtalk_array:from_list(Entries).

-spec owner_to_beamtalk(pid() | undefined) -> pid() | nil.
owner_to_beamtalk(undefined) -> nil;
owner_to_beamtalk(Pid) when is_pid(Pid) -> Pid.

-doc """
Block-scoped handle in the given mode, closed however the block exits.

Opens the file, passes the FileHandle to the block, and closes the handle on
the way out — normal return, raised error, or non-local return alike (the same
guarantee `ensure:` gives at the Beamtalk level). A block that closes the
handle itself is fine: closing is idempotent.

The block normally runs in the *caller's* process: BT-3018 / ADR 0109 lowers
`File open:…do:` at the call site to a direct call on the `open/3` shim below,
so it never reaches the File class process. Nothing here bounds what the block
may do — it can message `File` again, it holds nothing else up, and there is
no time limit.

The exception is reaching this function through dynamic dispatch (`perform:`),
which still goes via the class gen_server and so runs the block there. In that
case the pre-ADR-0109 constraints apply: the block cannot message `File` (that
deadlocks), it serialises every other `File` call in the node behind it, and it
must finish inside `beamtalk_class_dispatch`'s 60-second class-call timeout —
past that the caller gets a timeout while the block keeps running.

Returns a Result ok map holding the block's value, or a Result error map if the
file could not be opened.
""".
-spec 'open:mode:do:'(binary(), atom(), Do :: fun((map()) -> term())) -> beamtalk_result:t().
'open:mode:do:'(Path, Mode, Block) when
    is_binary(Path), is_atom(Mode), is_function(Block, 1)
->
    case do_open(Path, Mode, 'open:mode:do:') of
        {ok, Handle} ->
            try
                beamtalk_result:from_tagged_tuple({ok, Block(Handle)})
            after
                beamtalk_file_handle:close_handle(Handle)
            end;
        {error, Error} ->
            beamtalk_result:from_tagged_tuple({error, Error})
    end;
'open:mode:do:'(Path, Mode, _) when is_binary(Path), is_atom(Mode) ->
    beamtalk_error:raise_type_error(
        'File', 'open:mode:do:', <<"Expected a Block with 1 argument">>
    );
'open:mode:do:'(Path, _, _) when is_binary(Path) ->
    beamtalk_error:raise_type_error('File', 'open:mode:do:', <<"Mode must be a Symbol">>);
'open:mode:do:'(_, _, _) ->
    beamtalk_error:raise_type_error('File', 'open:mode:do:', <<"Path must be a String">>).

-doc """
Shared open path for open:mode: and open:mode:do:.

`open:mode:` (unlike the `do:` variants) is deliberately **not** call-site
lowered to the caller's process, and must stay that way — BT-3020 measured why:
OTP already auto-closes a handle when the process that opened it dies (a
non-`raw` descriptor is a `file_io_server` process that monitors its opener),
so lowering `open:mode:` would seem to make that auto-close "free". But the
REPL spawns a fresh worker per evaluated statement (`spawn_monitor` in
`beamtalk_repl_shell:handle_call({eval, ...})`), so a handle opened on one
turn would die the instant that turn's statement finished — breaking the
documented multi-turn `file.bt` workflow (`handle := (File open: … mode: …)
unwrap` on one line, `handle writeLine: …` on the next). Handles survive turns
today only because they're opened in the long-lived File class process
instead. See `resolve_owner/0` for how ownership is resolved without lowering.
""".
-spec do_open(binary(), atom(), atom()) ->
    {ok, beamtalk_file_handle:t()} | {error, beamtalk_error:error()}.
do_open(Path, Mode, Selector) ->
    case mode_options(Mode) of
        {ok, Options} ->
            PathStr = unicode:characters_to_list(Path),
            case ensure_parent_dir(Mode, PathStr) of
                ok ->
                    case check_regular(PathStr, Selector, Path) of
                        ok ->
                            %% Do NOT add `raw` to Options. A raw descriptor may
                            %% only be used by the process that opened it, and
                            %% `open:mode:` still opens in the File class
                            %% process — every handle it hands back would fail
                            %% with not_on_controlling_process. (ADR 0109 moved
                            %% only the block-scoped selectors to the caller.)
                            %% See mode_options/1.
                            case file:open(PathStr, Options) of
                                {ok, Fd} -> {ok, beamtalk_file_handle:new(Fd, Mode, Path)};
                                {error, Reason} -> {error, open_error(Selector, Path, Reason)}
                            end;
                        {error, _} = NotRegular ->
                            NotRegular
                    end;
                {error, Reason} ->
                    Error0 = beamtalk_error:new(io_error, 'File'),
                    Error1 = beamtalk_error:with_selector(Error0, Selector),
                    Error2 = beamtalk_error:with_details(Error1, #{
                        path => Path, reason => Reason
                    }),
                    {error, beamtalk_error:with_hint(Error2, <<"Could not create directory">>)}
            end;
        error ->
            Error0 = beamtalk_error:new(type_error, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, Selector),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path, mode => Mode}),
            {error,
                beamtalk_error:with_hint(
                    Error2, <<"Mode must be #read, #write, #append, or #readWrite">>
                )}
    end.

-doc """
Binary `file:open/2` options for each Beamtalk mode Symbol.

Deliberately **not** `raw`: a raw descriptor may only be used by the process
that opened it, and `open:mode:` is a class method, so it opens in the File
class process rather than the caller's. The handle it returns would be dead on
arrival. (ADR 0109 moved `open:…do:` to the caller, but not `open:mode:`.)
Non-raw descriptors are BEAM processes, so a handle stays usable wherever it is
held — the same property that lets `File lines:` streams outlive the open call.
""".
-spec mode_options(atom()) -> {ok, [file:mode()]} | error.
mode_options(read) -> {ok, [read, binary]};
mode_options(write) -> {ok, [write, binary]};
mode_options(append) -> {ok, [append, binary]};
mode_options(readWrite) -> {ok, [read, write, binary]};
mode_options(_) -> error.

-doc """
Refuse to open something that exists but is not a regular file.

A FIFO or character device never reports end-of-file, so `readAll` on one loops
forever — and via `open:mode:`, which still opens in the File class process, a
wedged read takes every other `File` operation in the node with it. A directory
is rejected here too, turning a bare `eisdir` into a message that says what was
wrong.

A path that does not exist yet is fine: the write-capable modes create it.
""".
-spec check_regular(string(), atom(), binary()) -> ok | {error, beamtalk_error:error()}.
check_regular(PathStr, Selector, Path) ->
    case filelib:is_file(PathStr) andalso not filelib:is_regular(PathStr) of
        false ->
            ok;
        true ->
            Error0 = beamtalk_error:new(io_error, 'File'),
            Error1 = beamtalk_error:with_selector(Error0, Selector),
            Error2 = beamtalk_error:with_message(
                Error1, <<"FileHandle path is not a regular file">>
            ),
            Error3 = beamtalk_error:with_details(Error2, #{path => Path}),
            {error,
                beamtalk_error:with_hint(
                    Error3,
                    <<
                        "File handles are for regular files; a directory, FIFO or device "
                        "cannot be read to end-of-file"
                    >>
                )}
    end.

-doc "Create missing parent directories for write-capable modes only.".
-spec ensure_parent_dir(atom(), string()) -> ok | {error, term()}.
ensure_parent_dir(read, _PathStr) ->
    ok;
ensure_parent_dir(_Mode, PathStr) ->
    %% ensure_dir/1 creates the directories the *named file* sits in, and does
    %% not create the file itself — no need to join a placeholder onto dirname.
    filelib:ensure_dir(PathStr).

-doc "Map a file:open/2 failure onto the structured error kinds File uses.".
-spec open_error(atom(), binary(), term()) -> beamtalk_error:error().
open_error(Selector, Path, enoent) ->
    Error0 = beamtalk_error:new(file_not_found, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
    beamtalk_error:with_hint(Error2, <<"Check that the file exists">>);
open_error(Selector, Path, eacces) ->
    Error0 = beamtalk_error:new(permission_denied, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_details(Error1, #{path => Path}),
    beamtalk_error:with_hint(Error2, <<"Check file permissions">>);
open_error(Selector, Path, Reason) ->
    Error0 = beamtalk_error:new(io_error, 'File'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}).

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
    beamtalk_error:raise_type_error('File', 'mkdir:', <<"Path must be a String">>).

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
    beamtalk_error:raise_type_error('File', 'mkdirAll:', <<"Path must be a String">>).

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
    beamtalk_error:raise_type_error('File', 'listDirectory:', <<"Path must be a String">>).

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
    beamtalk_error:raise_type_error('File', 'delete:', <<"Path must be a String">>).

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
    beamtalk_error:raise_type_error('File', 'deleteAll:', <<"Path must be a String">>).

-doc """
Rename or move a file or directory.

Returns a Result ok map on success, Result error map on failure.
""".
-spec 'rename:to:'(binary(), To :: binary()) -> beamtalk_result:t().
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
    beamtalk_error:raise_type_error('File', 'rename:to:', <<"Destination must be a String">>);
'rename:to:'(_, _) ->
    beamtalk_error:raise_type_error('File', 'rename:to:', <<"Path must be a String">>).

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
    beamtalk_error:raise_type_error('File', 'absolutePath:', <<"Path must be a String">>).

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
    beamtalk_error:raise_type_error('File', 'lastModified:', <<"Path must be a String">>).

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
%%%   (Erlang beamtalk_file) open: path mode: m    → open/2
%%%   (Erlang beamtalk_file) open: p mode: m do: b → open/3
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
%%%   (Erlang beamtalk_file) openHandles           → 'openHandles'/0 (direct)
%%% ============================================================================

exists(Path) -> 'exists:'(Path).
readAll(Path) -> 'readAll:'(Path).
-spec writeAll(binary(), Contents :: binary()) -> beamtalk_result:t().
writeAll(Path, Contents) -> 'writeAll:contents:'(Path, Contents).
readBinary(Path) -> 'readBinary:'(Path).
-spec writeBinary(binary(), Contents :: binary()) -> beamtalk_result:t().
writeBinary(Path, Contents) -> 'writeBinary:contents:'(Path, Contents).
-spec appendBinary(binary(), Contents :: binary()) -> beamtalk_result:t().
appendBinary(Path, Contents) -> 'appendBinary:contents:'(Path, Contents).
lines(Path) -> 'lines:'(Path).
%% `open:do:` and `open:mode:` both lower to the arity-2 shim (the shim name is
%% the first keyword), so they are told apart by their second argument: a Block
%% is a fun, a mode is a Symbol.
%%
%% These two shims have a second caller beyond the inline FFI: BT-3018 / ADR
%% 0109 compiles `File open:…do:` straight to `native_call(beamtalk_file, open,
%% …)` so the user's block runs in the caller rather than the File class
%% process. Removing the fun clause below would send those call sites to
%% `open:mode:` with a fun as the mode.
%%
%% Every atom goes to `open:mode:`, not just the four valid modes. A misspelled
%% mode is the common mistake, and `open:mode:` answers it with a `Result
%% error:` naming all four — far better than a raise claiming the Symbol you
%% passed is not a Symbol. The cost is that `File open: p do: nil` (nil and the
%% booleans are atoms too) is reported against `open:mode:`, but passing a
%% non-Block as a block is the rarer and more obviously broken call, and the
%% message still names what a valid mode looks like.
-spec open(binary(), Do :: fun((map()) -> term()) | atom()) -> beamtalk_result:t().
open(Path, Block) when is_function(Block, 1) ->
    'open:do:'(Path, Block);
open(Path, Mode) when is_atom(Mode) ->
    'open:mode:'(Path, Mode);
open(_Path, _Other) ->
    %% Deliberately no selector on the error: we cannot tell which of
    %% `open:do:` / `open:mode:` was meant, and a made-up `open:` would mislead
    %% anyone reading the record's selector field. The message names both.
    Error0 = beamtalk_error:new(type_error, 'File'),
    Error1 = beamtalk_error:with_message(
        Error0, <<"File 'open:': second argument is neither a Block nor a mode Symbol">>
    ),
    beamtalk_error:raise(
        beamtalk_error:with_hint(
            Error1,
            <<
                "Pass a 1-argument Block for 'open:do:', or one of #read, #write, "
                "#append, #readWrite for 'open:mode:'"
            >>
        )
    ).
-spec open(binary(), atom(), Do :: fun((map()) -> term())) -> beamtalk_result:t().
open(Path, Mode, Block) -> 'open:mode:do:'(Path, Mode, Block).
isDirectory(Path) -> 'isDirectory:'(Path).
isFile(Path) -> 'isFile:'(Path).
mkdir(Path) -> 'mkdir:'(Path).
mkdirAll(Path) -> 'mkdirAll:'(Path).
listDirectory(Path) -> 'listDirectory:'(Path).
delete(Path) -> 'delete:'(Path).
deleteAll(Path) -> 'deleteAll:'(Path).
-spec rename(binary(), To :: binary()) -> beamtalk_result:t().
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

The Stream reads lines from the already-open file handle, so it is bounded by
that handle's lifetime — the enclosing block for `open:do:` / `open:mode:do:`,
or the caller's `close` for a handle from `open:mode:`.
""".
-spec handle_lines(file_handle()) -> beamtalk_stream:t().
handle_lines(#{'$beamtalk_class' := 'FileHandle', fd := Fd} = Handle) ->
    %% BT-2975: `lines` returns a Stream, not a Result, so a closed or
    %% write-only handle has to raise. Reading a write-only descriptor crashes
    %% the file_io_server, silently breaking every later write on the handle —
    %% and a closed one would surface only as a mysteriously empty stream.
    ok = beamtalk_file_handle:ensure_readable(Handle, 'lines'),
    make_line_stream_from_fd(Fd);
handle_lines(_) ->
    beamtalk_error:raise_type_error('FileHandle', 'lines', <<"Expected a FileHandle">>).

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
                ?LOG_WARNING("File stream read error", #{
                    reason => Reason, domain => [beamtalk, stdlib]
                }),
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
