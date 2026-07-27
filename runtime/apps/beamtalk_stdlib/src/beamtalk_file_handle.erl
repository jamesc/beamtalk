%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_file_handle).

%%% **DDD Context:** Object System Context

-moduledoc """
FileHandle instance-side implementation and dispatch (BT-513, BT-871, BT-1762, BT-2975).

A `FileHandle` is a tagged map wrapping an open `file:io_device()` plus the
mode it was opened with, the path it came from, and an `atomics` cell holding
its open/closed state. The atomics cell is what makes `close`/`isOpen` work on
an otherwise immutable map: every copy of the handle shares the same cell, so
closing through one copy is visible through all of them.

## Methods

| Selector     | Description                                        |
|--------------|----------------------------------------------------|
| `lines`      | Lazy Stream of lines from the current position     |
| `read:`      | Read N bytes, `Result ok: <<>>` at end-of-file     |
| `readAll`    | Read from the current position to end-of-file      |
| `write:`     | Write a String or Binary at the current position   |
| `writeLine:` | Write followed by a newline                        |
| `position`   | Current byte offset from the start of the file     |
| `seek:`      | Move to an absolute byte offset, returning it      |
| `flush`      | Push buffered writes to the OS (no-op, see below)  |
| `sync`       | fsync — force written data to physical storage     |
| `close`      | Close the handle (idempotent)                      |
| `isOpen`     | Whether the handle is still open                   |

Handles are unbuffered (opened without `delayed_write`), so `flush` has nothing
to push and always succeeds on an open handle. It exists so callers can express
intent, and stays correct if buffering is ever introduced. Durability requires
`sync`, not `flush`.

Every operation on a closed handle returns a structured `#beamtalk_error{}`
Result rather than crashing on a stale file descriptor.

See also: beamtalk_file for File class-side methods (`open:mode:`, `readAll:`, etc.)
""".

-export([dispatch/3, has_method/1]).

%% FFI shims for (Erlang beamtalk_file_handle) dispatch (BT-2975)
-export([
    read/2,
    readAll/1,
    write/2,
    writeLine/2,
    position/1,
    seek/2,
    flush/1,
    sync/1,
    close/1,
    isOpen/1
]).

%% Handle lifecycle — used by beamtalk_file's open:mode: / open:do: family.
-export([new/3, close_handle/1, is_open/1, ensure_readable/2]).

-type mode() :: read | write | append | readWrite.
-type t() :: #{
    '$beamtalk_class' := 'FileHandle',
    fd := file:io_device(),
    mode := mode(),
    path := binary(),
    state := atomics:atomics_ref()
}.
-export_type([t/0, mode/0]).

%% Modes that permit reading / writing. `new/3` always records a mode, so the
%% `read` default in `with_mode/5` only guards a malformed handle.
-define(READ_MODES, [read, readWrite]).
-define(WRITE_MODES, [write, append, readWrite]).

%%% ============================================================================
%%% Handle lifecycle
%%% ============================================================================

-doc """
Build a FileHandle around an already-open file descriptor.

The returned handle carries a fresh atomics cell marked open. Callers must not
share a descriptor between two handles — closing one would leave the other
believing it is still open.
""".
-spec new(file:io_device(), mode(), binary()) -> t().
new(Fd, Mode, Path) when is_atom(Mode), is_binary(Path) ->
    Ref = atomics:new(1, [{signed, false}]),
    atomics:put(Ref, 1, 1),
    #{
        '$beamtalk_class' => 'FileHandle',
        fd => Fd,
        mode => Mode,
        path => Path,
        state => Ref
    }.

-doc """
Close a handle's descriptor exactly once, whoever asks first.

Returns `ok` for a handle that was already closed, so `open:mode:do:`'s
guaranteed close does not fail when the block closed the handle itself.
""".
-spec close_handle(map()) -> ok | {error, term()}.
close_handle(#{fd := Fd, state := Ref}) ->
    case atomics:exchange(Ref, 1, 0) of
        1 -> file:close(Fd);
        _ -> ok
    end;
close_handle(#{fd := Fd}) ->
    %% No state cell to claim — close best-effort rather than crash.
    file:close(Fd).

-doc """
Whether a handle is still open.

Two things have to hold. The state cell must say nobody has closed the handle,
and the descriptor itself must still be alive — a non-raw descriptor is a BEAM
process, and it can die without anyone calling `close`: the owning process
exits, or an illegal request (a read on a write-only descriptor) crashes the
`file_io_server`. Checking only the cell would leave `isOpen` answering `true`
for a dead descriptor, and every later operation would report a bare
`terminated` instead of a clear closed-handle error.

`new/3` is the only constructor, so every real handle carries a state cell. A
map missing one is malformed and reports closed: callers then produce a
closed-handle error instead of reaching for an `fd` key that may not be there.
""".
-spec is_open(term()) -> boolean().
is_open(#{'$beamtalk_class' := 'FileHandle', state := Ref, fd := Fd}) ->
    atomics:get(Ref, 1) =:= 1 andalso descriptor_alive(Fd);
is_open(_) ->
    false.

-spec descriptor_alive(file:io_device()) -> boolean().
descriptor_alive(Fd) when is_pid(Fd) -> is_process_alive(Fd);
%% A raw descriptor is a #file_descriptor{} record with no liveness to check;
%% the state cell is all we have.
descriptor_alive(_Fd) -> true.

%%% ============================================================================
%%% Instance methods (BT-2975)
%%% ============================================================================

-doc """
Read up to `Count` bytes from the current position.

Returns `Result ok: binary`. For a `Count` above zero, a shorter binary means
end-of-file was reached and an empty binary means the handle was already there.
`Count` of zero is a no-op that always answers an empty binary without moving
the position, so an empty result only implies end-of-file when `Count > 0`.
""".
-spec read(t(), integer()) -> beamtalk_result:t().
read(#{'$beamtalk_class' := 'FileHandle'} = Handle, Count) when
    is_integer(Count), Count >= 0
->
    with_readable(Handle, 'read:', fun(Fd) ->
        case file:read(Fd, Count) of
            eof -> {ok, <<>>};
            Other -> Other
        end
    end);
read(#{'$beamtalk_class' := 'FileHandle'}, _) ->
    beamtalk_error:raise_type_error(
        'FileHandle', 'read:', <<"Byte count must be a non-negative Integer">>
    );
read(_, _) ->
    beamtalk_error:raise_type_error('FileHandle', 'read:', <<"Expected a FileHandle">>).

-doc """
Read from the current position to end-of-file.

Returns `Result ok: binary`. Reading a handle already at end-of-file yields an
empty binary.

Unbounded: the whole remainder is accumulated in memory, and on a descriptor
that never reports end-of-file (a device or FIFO) this does not return. Both
matter more than usual here because the caller is the File class process — see
`beamtalk_file:'open:mode:do:'`.
""".
-spec readAll(t()) -> beamtalk_result:t().
readAll(#{'$beamtalk_class' := 'FileHandle'} = Handle) ->
    with_readable(Handle, 'readAll', fun(Fd) -> read_to_eof(Fd, []) end);
readAll(_) ->
    beamtalk_error:raise_type_error('FileHandle', 'readAll', <<"Expected a FileHandle">>).

-doc """
Write a String or Binary at the current position.

On a handle opened `#append`, writes always land at end-of-file regardless of
the current position. Returns `Result ok: nil`.
""".
-spec write(t(), binary()) -> beamtalk_result:t().
write(#{'$beamtalk_class' := 'FileHandle'} = Handle, Data) when is_binary(Data) ->
    with_writable(Handle, 'write:', fun(Fd) -> file:write(Fd, Data) end);
write(#{'$beamtalk_class' := 'FileHandle'}, _) ->
    beamtalk_error:raise_type_error('FileHandle', 'write:', <<"Data must be a String or Binary">>);
write(_, _) ->
    beamtalk_error:raise_type_error('FileHandle', 'write:', <<"Expected a FileHandle">>).

-doc "Write a String or Binary followed by a newline. Returns `Result ok: nil`.".
-spec writeLine(t(), binary()) -> beamtalk_result:t().
writeLine(#{'$beamtalk_class' := 'FileHandle'} = Handle, Data) when is_binary(Data) ->
    with_writable(Handle, 'writeLine:', fun(Fd) -> file:write(Fd, [Data, $\n]) end);
writeLine(#{'$beamtalk_class' := 'FileHandle'}, _) ->
    beamtalk_error:raise_type_error(
        'FileHandle', 'writeLine:', <<"Data must be a String or Binary">>
    );
writeLine(_, _) ->
    beamtalk_error:raise_type_error('FileHandle', 'writeLine:', <<"Expected a FileHandle">>).

-doc "Current byte offset from the start of the file. Returns `Result ok: integer`.".
-spec position(t()) -> beamtalk_result:t().
position(#{'$beamtalk_class' := 'FileHandle'} = Handle) ->
    with_open(Handle, 'position', fun(Fd) -> file:position(Fd, cur) end);
position(_) ->
    beamtalk_error:raise_type_error('FileHandle', 'position', <<"Expected a FileHandle">>).

-doc """
Move to an absolute byte offset measured from the start of the file.

Returns `Result ok: newPosition`. Seeking past end-of-file is allowed — writing
there leaves a hole, reading there returns an empty binary.
""".
-spec seek(t(), integer()) -> beamtalk_result:t().
seek(#{'$beamtalk_class' := 'FileHandle'} = Handle, Offset) when
    is_integer(Offset), Offset >= 0
->
    with_open(Handle, 'seek:', fun(Fd) -> file:position(Fd, {bof, Offset}) end);
seek(#{'$beamtalk_class' := 'FileHandle'}, _) ->
    beamtalk_error:raise_type_error(
        'FileHandle', 'seek:', <<"Offset must be a non-negative Integer">>
    );
seek(_, _) ->
    beamtalk_error:raise_type_error('FileHandle', 'seek:', <<"Expected a FileHandle">>).

-doc """
Push buffered writes to the operating system.

Beamtalk file handles are unbuffered, so there is never anything to push and
this succeeds immediately on an open handle. Use `sync` for durability.
""".
-spec flush(t()) -> beamtalk_result:t().
flush(#{'$beamtalk_class' := 'FileHandle'} = Handle) ->
    with_open(Handle, 'flush', fun(_Fd) -> ok end);
flush(_) ->
    beamtalk_error:raise_type_error('FileHandle', 'flush', <<"Expected a FileHandle">>).

-doc """
Force written data to physical storage via `file:sync/1` (fsync).

Returns `Result ok: nil` once the OS reports the data durable. This is the
operation a crash-safe append-only log needs after each record.
""".
-spec sync(t()) -> beamtalk_result:t().
sync(#{'$beamtalk_class' := 'FileHandle'} = Handle) ->
    with_open(Handle, 'sync', fun(Fd) -> file:sync(Fd) end);
sync(_) ->
    beamtalk_error:raise_type_error('FileHandle', 'sync', <<"Expected a FileHandle">>).

-doc """
Close the handle. Idempotent — closing an already-closed handle returns
`Result ok: nil` rather than an error.
""".
-spec close(t()) -> beamtalk_result:t().
close(#{'$beamtalk_class' := 'FileHandle'} = Handle) ->
    case close_handle(Handle) of
        ok -> beamtalk_result:from_tagged_tuple({ok, nil});
        {error, Reason} -> io_error(Handle, 'close', Reason)
    end;
close(_) ->
    beamtalk_error:raise_type_error('FileHandle', 'close', <<"Expected a FileHandle">>).

-doc """
Raise unless the handle is open and readable.

For callers whose return type has no room for a `Result` — `lines` hands back a
Stream, so a wrong-mode handle has to raise. Reading a write-only descriptor is
not a recoverable error at the OS level: it crashes the `file_io_server`, after
which every later write on that handle fails silently. Gate before touching it.
""".
-spec ensure_readable(map(), atom()) -> ok | no_return().
ensure_readable(Handle, Selector) ->
    case is_open(Handle) of
        false ->
            beamtalk_error:raise(closed_error_record(Handle, Selector));
        true ->
            Mode = maps:get(mode, Handle, read),
            case lists:member(Mode, ?READ_MODES) of
                true ->
                    ok;
                false ->
                    beamtalk_error:raise(
                        mode_error_record(Handle, Selector, Mode, <<"reading">>)
                    )
            end
    end.

-doc "Whether the handle is still open.".
-spec isOpen(t()) -> boolean().
isOpen(#{'$beamtalk_class' := 'FileHandle'} = Handle) ->
    is_open(Handle);
isOpen(_) ->
    beamtalk_error:raise_type_error('FileHandle', 'isOpen', <<"Expected a FileHandle">>).

%%% ============================================================================
%%% Dispatch
%%% ============================================================================

-doc """
Dispatch a message to a FileHandle instance.

Handles the FileHandle selectors directly and falls through to the base
Object protocol for everything else.
""".
-spec dispatch(atom(), list(), map()) -> term().
dispatch('lines', [], X) ->
    beamtalk_file:handle_lines(X);
dispatch('read:', [Count], X) ->
    read(X, Count);
dispatch('readAll', [], X) ->
    readAll(X);
dispatch('write:', [Data], X) ->
    write(X, Data);
dispatch('writeLine:', [Data], X) ->
    writeLine(X, Data);
dispatch('position', [], X) ->
    position(X);
dispatch('seek:', [Offset], X) ->
    seek(X, Offset);
dispatch('flush', [], X) ->
    flush(X);
dispatch('sync', [], X) ->
    sync(X);
dispatch('close', [], X) ->
    close(X);
dispatch('isOpen', [], X) ->
    isOpen(X);
dispatch(Selector, Args, X) ->
    case beamtalk_object_ops:try_dispatch(Selector, Args, X) of
        {ok, Result} ->
            Result;
        false ->
            %% Defensive fallback: direct callers can reach this with an
            %% unknown selector (previously crashed with case_clause).
            beamtalk_error:raise(beamtalk_error:new(does_not_understand, 'FileHandle', Selector))
    end.

-doc "Check if a FileHandle responds to the given selector.".
-spec has_method(atom()) -> boolean().
has_method(Selector) ->
    beamtalk_file:handle_has_method(Selector) orelse beamtalk_object_ops:has_method(Selector).

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

-doc "Run `Fun` on the descriptor if the handle is open, else a closed-handle error.".
-spec with_open(map(), atom(), fun((file:io_device()) -> term())) -> beamtalk_result:t().
with_open(Handle, Selector, Fun) ->
    case is_open(Handle) of
        true -> to_result(Handle, Selector, Fun(maps:get(fd, Handle)));
        false -> closed_error(Handle, Selector)
    end.

-spec with_readable(map(), atom(), fun((file:io_device()) -> term())) -> beamtalk_result:t().
with_readable(Handle, Selector, Fun) ->
    with_mode(Handle, Selector, ?READ_MODES, <<"reading">>, Fun).

-spec with_writable(map(), atom(), fun((file:io_device()) -> term())) -> beamtalk_result:t().
with_writable(Handle, Selector, Fun) ->
    with_mode(Handle, Selector, ?WRITE_MODES, <<"writing">>, Fun).

-spec with_mode(map(), atom(), [mode()], binary(), fun((file:io_device()) -> term())) ->
    beamtalk_result:t().
with_mode(Handle, Selector, Allowed, Intent, Fun) ->
    case is_open(Handle) of
        false ->
            closed_error(Handle, Selector);
        true ->
            Mode = maps:get(mode, Handle, read),
            case lists:member(Mode, Allowed) of
                true -> to_result(Handle, Selector, Fun(maps:get(fd, Handle)));
                false -> mode_error(Handle, Selector, Mode, Intent)
            end
    end.

-doc "Normalise `ok | {ok, V} | {error, R}` from the file module into a Result.".
-spec to_result(map(), atom(), ok | {ok, term()} | {error, term()}) -> beamtalk_result:t().
to_result(_Handle, _Selector, ok) ->
    beamtalk_result:from_tagged_tuple({ok, nil});
to_result(_Handle, _Selector, {ok, Value}) ->
    beamtalk_result:from_tagged_tuple({ok, Value});
to_result(Handle, Selector, {error, Reason}) ->
    io_error(Handle, Selector, Reason).

-spec read_to_eof(file:io_device(), [binary()]) -> {ok, binary()} | {error, term()}.
read_to_eof(Fd, Acc) ->
    case file:read(Fd, 65536) of
        {ok, Chunk} -> read_to_eof(Fd, [Chunk | Acc]);
        eof -> {ok, iolist_to_binary(lists:reverse(Acc))};
        {error, Reason} -> {error, Reason}
    end.

-spec io_error(map(), atom(), term()) -> beamtalk_result:t().
io_error(Handle, Selector, Reason) ->
    Error0 = beamtalk_error:new(io_error, 'FileHandle'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_details(Error1, #{path => path_of(Handle), reason => Reason}),
    beamtalk_result:from_tagged_tuple({error, Error2}).

-spec closed_error(map(), atom()) -> beamtalk_result:t().
closed_error(Handle, Selector) ->
    beamtalk_result:from_tagged_tuple({error, closed_error_record(Handle, Selector)}).

-spec closed_error_record(map(), atom()) -> beamtalk_error:error().
closed_error_record(Handle, Selector) ->
    Error0 = beamtalk_error:new(io_error, 'FileHandle'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_message(Error1, <<"FileHandle is closed">>),
    Error3 = beamtalk_error:with_details(Error2, #{path => path_of(Handle)}),
    beamtalk_error:with_hint(
        Error3, <<"The handle was already closed — reopen it with 'File open:mode:'">>
    ).

-spec mode_error(map(), atom(), mode(), binary()) -> beamtalk_result:t().
mode_error(Handle, Selector, Mode, Intent) ->
    beamtalk_result:from_tagged_tuple(
        {error, mode_error_record(Handle, Selector, Mode, Intent)}
    ).

-spec mode_error_record(map(), atom(), mode(), binary()) -> beamtalk_error:error().
mode_error_record(Handle, Selector, Mode, Intent) ->
    Error0 = beamtalk_error:new(io_error, 'FileHandle'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_message(
        Error1,
        iolist_to_binary([<<"FileHandle was not opened for ">>, Intent])
    ),
    Error3 = beamtalk_error:with_details(Error2, #{path => path_of(Handle), mode => Mode}),
    beamtalk_error:with_hint(
        Error3, <<"Reopen with 'File open: path mode: #readWrite' to both read and write">>
    ).

-spec path_of(map()) -> binary().
path_of(Handle) ->
    maps:get(path, Handle, <<"<unknown>">>).
