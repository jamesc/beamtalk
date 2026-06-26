%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_console).

%%% **DDD Context:** Object System Context

-moduledoc """
Console class implementation — the running process's standard I/O (ADR 0099 §1).

`Console` wraps the BEAM `standard_io` / `standard_error` of the *current*
OS process: line-oriented, point-to-point with the terminal. It is distinct
from `Transcript` (`beamtalk_transcript_stream`), which is the per-workspace
pub/sub log. A CLI tool writes to `Console`; a live-coding session observes
`Transcript`.

All methods are class-side — `Console` has no instances.

## Methods

| Selector      | Description                                       |
|---------------|---------------------------------------------------|
| `print:`      | Write a value to stdout (no newline)              |
| `printLine:`  | Write a value to stdout followed by a newline     |
| `error:`      | Write a value to stderr (no newline)              |
| `errorLine:`  | Write a value to stderr followed by a newline     |
| `flush`       | Flush buffered output (no-op on the BEAM io layer)|
| `readLine`    | Read one line from stdin; `nil` at EOF            |
| `readLine:`   | Write a prompt to stdout, then read one line      |

## Rendering

`print:` / `printLine:` / `error:` / `errorLine:` render their argument via the
`displayString` protocol (ADR 0094) using `beamtalk_primitive:display_string/1`
— the same rendering `Transcript show:` uses — so `"abc"` prints as `abc`,
not `"abc"`.

## Error contract (ADR 0099 §1 Open Questions, resolved here)

Both the read and write sides share one principle: **a closed or absent device
is benign; a genuine I/O failure is surfaced.**

* **Read side.** `readLine` / `readLine:` map `eof` to `nil`. A closed or
  never-opened stdin — `{error, terminated}` (the io server for a detached
  node has gone) and `{error, ebadf}` (stdin was never opened) — is treated as
  end-of-input and also maps to `nil`. Any *other* `{error, Reason}` (a genuine
  mid-stream failure) raises `#beamtalk_error{kind = io_error}`, keeping EOF and
  a real read error distinguishable.
  * `{error, enotsup}` is **not** on the closed-stdin allowlist: ordinary
    piped/redirected stdin returns data then `eof`, and `enotsup` is a property
    of `io:getopts`/tty queries rather than a `get_line` result, so a
    `get_line` yielding `enotsup` is a genuine anomaly worth surfacing.

* **Write side.** `print:` / `printLine:` / `error:` / `errorLine:` / `flush`
  drop silently (returning `nil`) when the device is closed/absent
  (`terminated`, `ebadf`) — writing to a detached node's stdout must not crash a
  program. Any *other* failure, including `epipe` (a broken pipe is a genuine
  mid-stream write failure, not an absent device), raises
  `#beamtalk_error{kind = io_error}`.

## REPL caveat

Under a detached/persistent workspace these calls reach the *node's* stdio, not
the connected client's terminal, and `readLine` typically sees a closed stdin
and returns `nil`. Interactive console I/O is a `beamtalk run` / packaged-script
story; inside the REPL prefer `Transcript`.
""".

-export(['print:'/1, 'printLine:'/1, 'error:'/1, 'errorLine:'/1]).
-export([flush/0, readLine/0, 'readLine:'/1]).
%% FFI shims: the Erlang proxy strips the trailing colon from keyword
%% selectors before dispatch, so `print:` arrives as `print`, etc.
-export([print/1, printLine/1, error/1, errorLine/1, readLine/1]).

%%% ============================================================================
%%% Public API — write side
%%% ============================================================================

-doc "Write a value to stdout with no trailing newline. Returns nil.".
-spec 'print:'(term()) -> 'nil'.
'print:'(Value) ->
    write(standard_io, render(Value), 'print:').

-doc "Write a value to stdout followed by a newline. Returns nil.".
-spec 'printLine:'(term()) -> 'nil'.
'printLine:'(Value) ->
    write(standard_io, [render(Value), $\n], 'printLine:').

-doc "Write a value to stderr with no trailing newline. Returns nil.".
-spec 'error:'(term()) -> 'nil'.
'error:'(Value) ->
    write(standard_error, render(Value), 'error:').

-doc "Write a value to stderr followed by a newline. Returns nil.".
-spec 'errorLine:'(term()) -> 'nil'.
'errorLine:'(Value) ->
    write(standard_error, [render(Value), $\n], 'errorLine:').

-doc """
Flush buffered output. Returns nil.

The BEAM io protocol is synchronous request/reply, so output written via
`print:`/`printLine:` has already been delivered to the io server by the time
the call returns; there is no user-space buffer above the io layer to flush
(ADR 0099 §3). `flush` therefore exists for API symmetry and forward
compatibility and is currently a no-op.
""".
-spec flush() -> 'nil'.
flush() ->
    nil.

%%% ============================================================================
%%% Public API — read side
%%% ============================================================================

-doc """
Read one line from stdin, with the trailing newline stripped.

Returns the line as a String, or `nil` at end of input (including a
closed/absent stdin). Raises `#beamtalk_error{kind = io_error}` on a genuine
mid-stream read failure. See the module doc for the closed-stdin allowlist.
""".
-spec readLine() -> binary() | 'nil'.
readLine() ->
    read(<<>>, readLine).

-doc """
Write a prompt to stdout (no newline), then read one line from stdin.

Same return contract as `readLine`.
""".
-spec 'readLine:'(term()) -> binary() | 'nil'.
'readLine:'(Prompt) ->
    read(render(Prompt), 'readLine:').

%%% ============================================================================
%%% FFI shims (proxy strips the trailing colon before dispatch)
%%% ============================================================================

-doc "FFI shim for `(Erlang beamtalk_console) print:`.".
-spec print(term()) -> 'nil'.
print(Value) -> 'print:'(Value).

-doc "FFI shim for `(Erlang beamtalk_console) printLine:`.".
-spec printLine(term()) -> 'nil'.
printLine(Value) -> 'printLine:'(Value).

-doc "FFI shim for `(Erlang beamtalk_console) error:`.".
-spec error(term()) -> 'nil'.
error(Value) -> 'error:'(Value).

-doc "FFI shim for `(Erlang beamtalk_console) errorLine:`.".
-spec errorLine(term()) -> 'nil'.
errorLine(Value) -> 'errorLine:'(Value).

-doc "FFI shim for `(Erlang beamtalk_console) readLine:`.".
-spec readLine(term()) -> binary() | 'nil'.
readLine(Prompt) -> 'readLine:'(Prompt).

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

-doc "Render a value via the displayString protocol (matches Transcript show:).".
-spec render(term()) -> binary().
render(Value) ->
    beamtalk_primitive:display_string(Value).

-doc """
Write iodata to a device, applying the write-side error contract.

A closed/absent device drops silently (returns nil); any other failure raises
`#beamtalk_error{kind = io_error}`.
""".
-spec write(atom(), iodata(), atom()) -> 'nil'.
write(Device, Data, Selector) ->
    %% io:put_chars/2 returns `ok` and raises on failure. A dead device surfaces
    %% as `error:terminated`, which the catch classifies; if the io *server
    %% process* itself is gone the call can instead raise an `exit`-class
    %% exception (e.g. `exit:{noproc, …}`) — that is the closed/absent-device
    %% case, so it drops silently per the write-side contract.
    try
        io:put_chars(Device, Data),
        nil
    catch
        error:Reason -> handle_io_error(Reason, Selector);
        exit:_Reason -> nil
    end.

-doc """
Read one line from stdin after writing the (already-rendered) prompt iodata,
applying the read-side error contract.
""".
-spec read(iodata(), atom()) -> binary() | 'nil'.
read(Prompt, Selector) ->
    try io:get_line(standard_io, Prompt) of
        eof -> nil;
        {error, Reason} -> handle_io_error(Reason, Selector);
        Data -> strip_eol(to_binary(Data))
    catch
        error:Reason -> handle_io_error(Reason, Selector);
        %% A dead io-server process surfaces as an exit-class exception — the
        %% closed/absent-stdin case, treated as end-of-input (nil).
        exit:_Reason -> nil
    end.

-doc """
Apply the shared error contract: a closed/absent device maps to `nil`, any
other reason raises `#beamtalk_error{kind = io_error}`.
""".
-spec handle_io_error(term(), atom()) -> 'nil' | no_return().
handle_io_error(Reason, Selector) ->
    case closed_device(Reason) of
        true -> nil;
        false -> raise_io_error(Selector, Reason)
    end.

-doc """
The closed/absent-device allowlist shared by the read and write sides.

`terminated` — the io server (e.g. a detached node's) has gone.
`ebadf` — the descriptor was never opened.
Everything else (incl. `epipe`, `enotsup`) is a genuine failure — see module doc.
""".
-spec closed_device(term()) -> boolean().
closed_device(terminated) -> true;
closed_device(ebadf) -> true;
closed_device(_) -> false.

-doc "Convert io:get_line/2 output (a codepoint list or binary) to a UTF-8 binary.".
-spec to_binary(string() | binary()) -> binary().
to_binary(Data) when is_binary(Data) ->
    Data;
to_binary(Data) when is_list(Data) ->
    case unicode:characters_to_binary(Data) of
        Bin when is_binary(Bin) -> Bin;
        _ -> iolist_to_binary(Data)
    end.

-doc "Strip a single trailing line terminator (\\n or \\r\\n) from a line.".
-spec strip_eol(binary()) -> binary().
strip_eol(Bin) ->
    Size = byte_size(Bin),
    case Bin of
        <<Head:(Size - 2)/binary, "\r\n">> -> Head;
        <<Head:(Size - 1)/binary, "\n">> -> Head;
        _ -> Bin
    end.

-spec raise_io_error(atom(), term()) -> no_return().
raise_io_error(Selector, Reason) ->
    Error0 = beamtalk_error:new(io_error, 'Console'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_details(Error1, #{reason => Reason}),
    Error3 = beamtalk_error:with_hint(Error2, <<"Console I/O operation failed">>),
    beamtalk_error:raise(Error3).
