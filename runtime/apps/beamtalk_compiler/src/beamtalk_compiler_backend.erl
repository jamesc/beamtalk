%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_compiler_backend).

%%% **DDD Context:** Compilation (Anti-Corruption Layer)

-moduledoc """
Backend dispatch for the Beamtalk compiler (ADR 0022, Phase 5).

Returns the compiler backend. Port is the sole backend (ADR 0022).
""".

-export([backend/0]).

-doc """
Return the active compiler backend atom.
Always returns `port' (OTP Port with ETF encoding).
""".
-spec backend() -> port.
backend() ->
    port.
