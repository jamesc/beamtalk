%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Cross-platform helpers for OS-specific operations.
%%
%%% **DDD Context:** Runtime
%%
%% Provides portable abstractions over platform-specific paths and
%% environment variables (HOME vs USERPROFILE, filesystem roots, etc.).
-module(beamtalk_platform).

-export([home_dir/0]).

%% @doc Return the user's home directory, or `false` if unavailable.
%%
%% Checks `HOME` first (Unix, WSL, Git Bash on Windows), then falls back
%% to `USERPROFILE` (native Windows). Empty strings are treated as unset.
-spec home_dir() -> string() | false.
home_dir() ->
    normalize_env(
        case os:getenv("HOME") of
            false -> os:getenv("USERPROFILE");
            Home -> Home
        end
    ).

%% @private Treat empty env var values as unset.
-spec normalize_env(string() | false) -> string() | false.
normalize_env(false) -> false;
normalize_env("") -> false;
normalize_env(Val) -> Val.
