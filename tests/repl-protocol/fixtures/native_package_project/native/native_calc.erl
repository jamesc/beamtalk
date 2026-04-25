%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Native Erlang helper for NativeCalc Beamtalk class.
%% Used by native_package.btscript E2E test (BT-1719).
-module(native_calc).
-moduledoc "Native arithmetic helpers for NativeCalc.".
-export([add/2, multiply/2, version/0]).

-doc "Add two integers.".
-spec add(integer(), integer()) -> integer().
add(A, B) -> A + B.

-doc "Multiply two integers.".
-spec multiply(integer(), integer()) -> integer().
multiply(A, B) -> A * B.

-doc "Return the module version.".
-spec version() -> integer().
version() -> 1.
