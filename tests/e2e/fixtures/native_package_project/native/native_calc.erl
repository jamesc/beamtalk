%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Native Erlang helper for NativeCalc Beamtalk class.
%% Used by native_package.btscript E2E test (BT-1719).
-module(native_calc).
-export([add/2, multiply/2, version/0]).

-spec add(integer(), integer()) -> integer().
add(A, B) -> A + B.

-spec multiply(integer(), integer()) -> integer().
multiply(A, B) -> A * B.

-spec version() -> integer().
version() -> 1.
