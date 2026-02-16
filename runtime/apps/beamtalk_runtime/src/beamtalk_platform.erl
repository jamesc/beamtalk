%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Cross-platform helpers for OS-specific operations.
%%
%% **DDD Context:** Runtime
%%
%% Provides portable abstractions over platform-specific paths and
%% environment variables (HOME vs USERPROFILE, filesystem roots, etc.).
-module(beamtalk_platform).

-export([home_dir/0]).

%% @doc Return the user's home directory, or `false` if unavailable.
%%
%% Checks `HOME` first (Unix, WSL, Git Bash on Windows), then falls back
%% to `USERPROFILE` (native Windows).
-spec home_dir() -> string() | false.
home_dir() ->
    case os:getenv("HOME") of
        false -> os:getenv("USERPROFILE");
        Home  -> Home
    end.
