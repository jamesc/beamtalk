%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Backend dispatch for the Beamtalk compiler (ADR 0022, Phase 1).
%%
%% DDD Context: Compilation (Anti-Corruption Layer)
%%
%% Determines which compiler backend to use based on the `BEAMTALK_COMPILER'
%% environment variable. Defaults to `port' (OTP Port to Rust binary).
%%
%% Supported backends:
%% - `port' — OTP Port with ETF encoding (default, ADR 0022)
%% - `daemon' — JSON-RPC over Unix socket (legacy)

-module(beamtalk_compiler_backend).

-export([backend/0]).

%% @doc Return the active compiler backend atom.
%% Reads `BEAMTALK_COMPILER' env var. Defaults to `port'.
-spec backend() -> port | daemon.
backend() ->
    case os:getenv("BEAMTALK_COMPILER") of
        "daemon" -> daemon;
        _ -> port
    end.
