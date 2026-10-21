%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_process_ops).

%%% **DDD Context:** Runtime Context

-moduledoc """
Process primitives behind `Pid>>send:`, `Reference new` and
`Actor>>processLabel:` / `processLabel` (BT-4049).

Each function is a thin wrapper over a BIF or `proc_lib`, and the module opts
out of the FFI result coercion (`beamtalk_ffi_raw_results/0`). The terms it
handles are user data. A message sent with `send/2` or a label read back with
`labelOf/1` may be `ok`, `{ok, _}` or a list of small integers, and must not
turn into a `Result` or a String on the way back. No function here signals
success or failure through an `ok`/`{error, _}` return.
""".

-export([send/2, makeRef/0, setLabel/1, labelOf/1]).
-export([beamtalk_ffi_raw_results/0]).

-doc """
Send `Msg` to `Pid` as a plain Erlang message (`erlang:send/2`). Answers
`nil`. Sending to a dead process is a no-op.
""".
-spec send(pid(), term()) -> nil.
send(Pid, Msg) when is_pid(Pid) ->
    _ = erlang:send(Pid, Msg),
    nil.

-doc "A fresh unique reference (`erlang:make_ref/0`).".
-spec makeRef() -> reference().
makeRef() ->
    erlang:make_ref().

-doc """
Label the calling process with `Label` (`proc_lib:set_label/1`, OTP 27+).
Answers `nil`.
""".
-spec setLabel(term()) -> nil.
setLabel(Label) ->
    ok = proc_lib:set_label(Label),
    nil.

-doc """
The label of `Pid` (`proc_lib:get_label/1`), or `nil` when it has none.
Erlang reports "no label" as `undefined`, so a label of `undefined` also
answers `nil`.
""".
-spec labelOf(pid()) -> term().
labelOf(Pid) when is_pid(Pid) ->
    case proc_lib:get_label(Pid) of
        undefined -> nil;
        Label -> Label
    end.

-doc """
Marker read by `beamtalk_erlang_proxy:coerce_ffi_result/2`: results of this
module are user data and reach Beamtalk unchanged.
""".
-spec beamtalk_ffi_raw_results() -> true.
beamtalk_ffi_raw_results() -> true.
