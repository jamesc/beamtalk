%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Backend dispatch for the Beamtalk compiler (ADR 0022, Phase 5).
%%
%%% **DDD Context:** Compilation (Anti-Corruption Layer)
%%
%% Returns the compiler backend. Port is the sole backend (ADR 0022).

-module(beamtalk_compiler_backend).

-export([backend/0]).

%% @doc Return the active compiler backend atom.
%% Always returns `port' (OTP Port with ETF encoding).
-spec backend() -> port.
backend() ->
    port.
