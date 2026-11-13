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

-export([ffi_ok/0]).

-spec ffi_ok() -> ok.
ffi_ok() -> ok.
