%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% E2E fixture: deliberately broken native module (BT-3336). See
%% wi_load_broken_native_project/src/WiLoadBrokenNativeUser.bt.

-module(wi_load_broken_native).
-export([go/0]).
go( -> broken.
