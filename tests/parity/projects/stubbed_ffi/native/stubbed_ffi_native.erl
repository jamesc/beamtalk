%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
-module(stubbed_ffi_native).
-moduledoc "BT-4378 fixture native module. It deliberately has no -spec, so only the project stub types it.".
-export([answer/0]).
-doc "Returns 42.".
answer() -> 42.
