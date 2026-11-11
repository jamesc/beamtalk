%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
-module(parity_lint_stub_ffi).
-moduledoc "BT-4643 fixture native module; typed only by its project stub.".
-export([double/1]).
-doc "Returns twice `X`.".
double(X) -> X * 2.
