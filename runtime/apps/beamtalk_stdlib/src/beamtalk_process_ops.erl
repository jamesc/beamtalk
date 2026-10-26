%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_process_ops).

%%% **DDD Context:** Runtime Context

-moduledoc """
Process primitives behind `Pid>>send:`, `Reference new` and
`Actor>>processLabel:` / `processLabel` (BT-4049), plus `Pid current`,
`Pid>>monitor` and `Block>>fork` (BT-4077).

Each function is a thin wrapper over a BIF or `proc_lib`, and the module opts
out of the FFI result coercion (`beamtalk_ffi_raw_results/0`). The terms it
handles are user data. A message sent with `send/2` or a label read back with
`labelOf/1` may be `ok`, `{ok, _}` or a list of small integers, and must not
turn into a `Result` or a String on the way back. No function here signals
success or failure through an `ok`/`{error, _}` return.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([send/2, makeRef/0, setLabel/1, labelOf/1]).
-export([current/0, monitor/1, fork/1]).
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
The calling process (`erlang:self/0`). Answers a `Pid`.
""".
-spec current() -> pid().
current() ->
    erlang:self().

-doc """
Monitor `Pid` (`erlang:monitor/2` with type `process`), the same underlying
call as `Actor>>monitor`, so the `'DOWN'` tuple a Server receives in
`handleInfo:` has the identical shape. Answers the monitor `Reference`.
Monitoring a dead pid still delivers `DOWN` (reason `noproc`). Cancel with
`Reference>>demonitor`.
""".
-spec monitor(pid()) -> reference().
monitor(Pid) when is_pid(Pid) ->
    erlang:monitor(process, Pid).

-doc """
Run `Block` in a fresh unlinked process (`erlang:spawn/1`) and answer the
worker's `Pid` immediately. `Block` must be a zero-argument Block
(`is_function(Block, 0)`, mirroring `Parallel.validate_blocks`); anything
else raises a structured `type_error`.

The worker body catches only a `^` non-local return (a `throw` matching
`?IS_NLR`, which has no home method to return to across the process
boundary) and exits with reason `{fork_non_local_return, ValueOrNil}`.
Everything else crashes the worker naturally with its original
class/reason/stack and a normal crash report — observable via `monitor/1` —
while the unlinked caller carries on.
""".
-spec fork(fun(() -> term())) -> pid().
fork(Block) when is_function(Block, 0) ->
    erlang:spawn(fun() -> fork_worker(Block) end);
fork(Block) ->
    raise_fork_type_error(Block).

-doc """
Worker body for `fork/1`: evaluate `Block`, turning only a stray `^` into
the `{fork_non_local_return, _}` exit reason. Any other exception propagates
out of the worker untouched.
""".
-spec fork_worker(fun(() -> term())) -> ok.
fork_worker(Block) ->
    %% bt-catcher-audit: not-applicable-other-process - the block runs in a
    %% freshly spawned worker, not the caller's process (no home entry for
    %% the caller's class there), and only an NLR is caught.
    try Block() of
        _Value -> ok
    catch
        throw:Nlr when ?IS_NLR(Nlr) ->
            exit({fork_non_local_return, fork_nlr_value(Nlr)})
    end.

-doc """
The value carried by an in-flight `^` (a `?IS_NLR` tuple): `element(3)` of
the state-carrying 4-tuple, `nil` for the legacy 3-tuple, which carries no
value. Uses `tuple_size/1` and `element/2`, never a literal tuple pattern
(`just lint-nlr-tuple-matches` rejects hand-rolled matches).
""".
-spec fork_nlr_value(tuple()) -> term().
fork_nlr_value(Nlr) when ?IS_NLR(Nlr), tuple_size(Nlr) =:= 4 ->
    element(3, Nlr);
fork_nlr_value(Nlr) when ?IS_NLR(Nlr) ->
    nil.

-spec raise_fork_type_error(term()) -> no_return().
raise_fork_type_error(Block) ->
    Error0 = beamtalk_error:new(type_error, 'Block'),
    Error1 = beamtalk_error:with_selector(Error0, fork),
    Error2 = beamtalk_error:with_hint(Error1, <<"fork needs a zero-argument Block">>),
    Error3 = beamtalk_error:with_details(Error2, #{got => Block}),
    beamtalk_error:raise(Error3).

-doc """
Marker read by `beamtalk_erlang_proxy:coerce_ffi_result/2`: results of this
module are user data and reach Beamtalk unchanged.
""".
-spec beamtalk_ffi_raw_results() -> true.
beamtalk_ffi_raw_results() -> true.
