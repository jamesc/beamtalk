%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Fixture callee for row 13d: an FFI target returning the bare atom `ok`, which
%% the call site wraps to `Result ok: nil`. No pure OTP function returns bare
%% `ok` without side effects, so the suite provides its own.
%%
%% It lives in the Beamtalk package's `native/` directory, not in the harness,
%% on purpose (BT-4770): the compiler reads `-spec`s only from OTP, the runtime
%% and the package's own `native/` modules, so only a callee here has its
%% `ok` return proved and the `Result` built inline at the call site.

-module(vs_ffi).

-export([ffi_ok/0, await_down/1, await_downs/1]).

-spec ffi_ok() -> ok.
ffi_ok() -> ok.

%% Rows 16a-16c (BT-4845): wait for the `'DOWN'` of a monitor the calling
%% process owns; answers 1 for a `normal` exit and 0 otherwise (an integer, so
%% the loop's result is live). The benchmark loop is the caller,
%% so the monitor `spawnMonitoredAsync` makes lands in its own mailbox.
-spec await_down(reference()) -> 0 | 1.
await_down(Ref) ->
    receive
        {'DOWN', Ref, process, _Pid, normal} -> 1;
        {'DOWN', Ref, process, _Pid, _Reason} -> 0
    end.

%% Row 16d: wait for every monitor in a list (the actors run concurrently);
%% answers how many exited `normal`.
-spec await_downs([reference()]) -> non_neg_integer().
await_downs(Refs) -> lists:sum([await_down(R) || R <- Refs]).
