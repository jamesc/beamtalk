%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_platform).

%%% **DDD Context:** Object System Context

-moduledoc """
Cross-platform helpers for OS-specific operations.

Provides portable abstractions over platform-specific paths and
environment variables (HOME vs USERPROFILE, filesystem roots, etc.).
""".

-export([home_dir/0, beamtalk_root_dir/0, workspaces_base_dir/0, workspace_dir/1]).

-doc """
Return the user's home directory, or `false` if unavailable.

Checks `HOME` first (Unix, WSL, Git Bash on Windows), then falls back
to `USERPROFILE` (native Windows). Empty strings are treated as unset.
""".
-spec home_dir() -> string() | false.
home_dir() ->
    normalize_env(
        case os:getenv("HOME") of
            false -> os:getenv("USERPROFILE");
            Home -> Home
        end
    ).

-doc """
Return the Beamtalk global config directory (`~/.beamtalk`), or `undefined`
when no home directory can be determined (BT-3680).

This is the single Erlang resolver for every `~/.beamtalk/*` path; the Rust
counterpart is `beamtalk_home::beamtalk_root_dir`. Resolution, in order:

1. `BEAMTALK_HOME` (non-empty) — explicit override, used for hermetic tests.
2. `home_dir()` joined with `.beamtalk`.
3. Otherwise `undefined`. There is deliberately NO user-cache fallback: the
   Rust CLI errors when it has no home, so state written elsewhere could
   never be discovered by it. Callers skip persistence (or return a
   structured error) on `undefined`.

The Rust/Erlang agreement is enforced by the shared fixture
`runtime/apps/beamtalk_runtime/test/fixtures/beamtalk_root_dir_conformance.json`.
""".
-spec beamtalk_root_dir() -> file:filename() | undefined.
beamtalk_root_dir() ->
    case normalize_env(os:getenv("BEAMTALK_HOME")) of
        false ->
            case home_dir() of
                false -> undefined;
                Home -> filename:join(Home, ".beamtalk")
            end;
        Override ->
            Override
    end.

-doc "Root directory holding per-workspace directories, or `undefined` without a home.".
-spec workspaces_base_dir() -> file:filename() | undefined.
workspaces_base_dir() ->
    case beamtalk_root_dir() of
        undefined -> undefined;
        Root -> filename:join(Root, "workspaces")
    end.

-doc "Directory of one workspace (`<workspaces_base_dir>/<id>`), or `undefined` without a home.".
-spec workspace_dir(binary()) -> file:filename() | undefined.
workspace_dir(WorkspaceId) when is_binary(WorkspaceId) ->
    case workspaces_base_dir() of
        undefined -> undefined;
        Base -> filename:join(Base, binary_to_list(WorkspaceId))
    end.

-doc "Treat empty env var values as unset.".
-spec normalize_env(string() | false) -> string() | false.
normalize_env(false) -> false;
normalize_env("") -> false;
normalize_env(Val) -> Val.
