%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_build_provenance).

%%% **DDD Context:** Workspace Context

-moduledoc """
Build-artifact provenance check on workspace attach (ADR 0098 Phase 4): whether the project's on-disk `_build` artifacts, and those of its dependencies, were produced by a different toolchain than the one now running, in which case the attach forces a full recompile.

Split out of `beamtalk_workspace_project_sync` (BT-5048); a leaf with no workspace dependencies.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    project_provenance_stale/1,
    collect_stale_dep_provenance/1,
    fail_stale_dep_attach/1
]).

%% Exported for white-box tests only.
-ifdef(TEST).
-export([
    read_provenance_stamp/1,
    stamp_matches_current/1,
    current_otp_release/0
]).
-endif.

%% Provenance-stamp schema understood by this reader. A newer stamp (or anything
%% unrecognised) is treated as a miss, never an error.
-define(PROVENANCE_SCHEMA, 1).

-doc """
Returns `true` only when the project's `_build/dev` artifacts carry a stamp that
was produced by a *different* toolchain than the one now running (a
`beamtalk_version` / OTP mismatch or an unrecognised schema). Those on-disk
`.beam` files are loaded on attach, so they must be force-recompiled from source
to override the stale bytes.

A **missing or unreadable** stamp returns `false`: the workspace recompiles
project sources from scratch on a fresh attach regardless, so there is nothing
stale to invalidate — and, crucially, the workspace must never *fabricate* a
stamp, because it compiles in-memory (`code:load_binary`) and never writes
`_build/dev/ebin/*.beam`. A stamp written here would lie to the CLI's Phase-1
gate about on-disk artifacts the workspace never produced.
""".
-spec project_provenance_stale(string()) -> boolean().
project_provenance_stale(AbsPath) ->
    StampPath = filename:join([AbsPath, "_build", "dev", ".beamtalk-stamp.json"]),
    case read_provenance_stamp(StampPath) of
        {ok, Stamp} ->
            case stamp_matches_current(Stamp) of
                fresh ->
                    false;
                {stale, Reason} ->
                    ?LOG_INFO(
                        "workspace provenance miss (~s) — recompiling project at ~s",
                        [Reason, AbsPath],
                        #{domain => [beamtalk, runtime]}
                    ),
                    true
            end;
        {error, _} ->
            %% No (readable) stamp → nothing on-disk whose provenance we can
            %% pin as stale; the source recompile on attach already covers it.
            false
    end.

-spec read_provenance_stamp(string()) -> {ok, map()} | {error, binary()}.
read_provenance_stamp(StampPath) ->
    case file:read_file(StampPath) of
        {ok, Bin} ->
            try json:decode(Bin) of
                Map when is_map(Map) -> {ok, Map};
                _ -> {error, <<"corrupt provenance stamp">>}
            catch
                _:_ -> {error, <<"corrupt provenance stamp">>}
            end;
        {error, _} ->
            {error, <<"no provenance stamp">>}
    end.

%% Compare a decoded stamp's invalidation fields against the running toolchain.
%% Only `beamtalk_version` and the compound OTP version are compared (`built_at`
%% is informational). A missing key compares unequal → stale, which is exactly
%% the older-toolchain case this check exists to catch.
-spec stamp_matches_current(map()) -> fresh | {stale, binary()}.
stamp_matches_current(Stamp) ->
    case maps:get(<<"schema">>, Stamp, undefined) of
        ?PROVENANCE_SCHEMA -> check_version(Stamp);
        _ -> {stale, <<"unrecognised provenance schema">>}
    end.

-spec check_version(map()) -> fresh | {stale, binary()}.
check_version(Stamp) ->
    case maps:get(<<"beamtalk_version">>, Stamp, undefined) of
        undefined ->
            %% A present stamp with no version key is foreign/corrupt — fail
            %% toward stale rather than reusing it (this also closes the gap
            %% where the compiler port is unavailable and OTP happens to match).
            {stale, <<"stamp has no beamtalk version">>};
        StampVsn ->
            case current_beamtalk_version() of
                {ok, CurVsn} when StampVsn =/= CurVsn ->
                    {stale, <<"built by a different beamtalk version">>};
                %% Version matches, or the compiler version is unavailable: fall
                %% through to the OTP axis (an unavailable version never forces
                %% on its own).
                _ ->
                    check_otp(Stamp)
            end
    end.

-spec check_otp(map()) -> fresh | {stale, binary()}.
check_otp(Stamp) ->
    StampOtp = maps:get(<<"otp_release">>, Stamp, undefined),
    case current_otp_release() of
        StampOtp -> fresh;
        _ -> {stale, <<"built for a different OTP release">>}
    end.

%% The running toolchain's full beamtalk version, via the compiler port — the
%% same `BEAMTALK_VERSION` the on-disk stamp records. `error` if the port is
%% unavailable, in which case provenance is compared on the OTP axis alone.
-spec current_beamtalk_version() -> {ok, binary()} | error.
current_beamtalk_version() ->
    try beamtalk_compiler_server:version() of
        {ok, V} when is_binary(V) -> {ok, V};
        _ -> error
    catch
        _:_ -> error
    end.

%% The running BEAM's compound OTP version (`<release>-<erts>`), identical to the
%% key the build stamp records (`27-15.0.1`), not the bare release (`27`).
-spec current_otp_release() -> binary().
current_otp_release() ->
    Rel = list_to_binary(erlang:system_info(otp_release)),
    Erts = list_to_binary(erlang:system_info(version)),
    <<Rel/binary, "-", Erts/binary>>.

-doc """
Collect every dependency whose compiled artifacts carry a stamp produced by a
different toolchain (a `beamtalk_version` / OTP mismatch or unrecognised schema).
The workspace recompiles project sources, not dependencies, so these cannot
self-heal on attach — ADR 0098 §4 fails the attach (see `fail_stale_dep_attach`)
rather than activating the stale `.beam`. A dep with no stamp is skipped (a
pre-stamp build the next `beamtalk build` will stamp), so the gate fires only on
a *definitively* foreign-toolchain dependency.
""".
-spec collect_stale_dep_provenance(string()) -> [{string(), binary()}].
collect_stale_dep_provenance(AbsPath) ->
    DepsDir = filename:join([AbsPath, "_build", "deps"]),
    case file:list_dir(DepsDir) of
        {ok, Names} ->
            lists:filtermap(fun(Name) -> stale_dep_entry(DepsDir, Name) end, Names);
        {error, _} ->
            []
    end.

-spec stale_dep_entry(string(), string()) -> {true, {string(), binary()}} | false.
stale_dep_entry(DepsDir, Name) ->
    DepDir = filename:join(DepsDir, Name),
    StampPath = filename:join(DepDir, ".beamtalk-stamp.json"),
    case filelib:is_dir(DepDir) andalso read_provenance_stamp(StampPath) of
        {ok, Stamp} ->
            case stamp_matches_current(Stamp) of
                fresh -> false;
                {stale, Reason} -> {true, {Name, Reason}}
            end;
        _ ->
            %% Not a directory, or no/unreadable stamp — nothing definitive.
            false
    end.

-doc """
Fail a workspace attach because one or more dependencies were compiled by a
different toolchain (ADR 0098 §4). Logs each at error level and returns a
structured, directed error. Called before any `code:load_file/1`, so the code
server is untouched — nothing to roll back.
""".
-spec fail_stale_dep_attach([{string(), binary()}]) -> {error, #beamtalk_error{}}.
fail_stale_dep_attach(StaleDeps) ->
    lists:foreach(
        fun({Name, Reason}) ->
            ?LOG_ERROR(
                "dependency '~s' provenance miss (~s) — run "
                "`beamtalk clean --deps` and rebuild",
                [Name, Reason],
                #{domain => [beamtalk, runtime]}
            )
        end,
        StaleDeps
    ),
    Names = lists:join(", ", [Name || {Name, _Reason} <- StaleDeps]),
    {error,
        beamtalk_repl_errors:make(
            stale_dependency,
            'Workspace',
            iolist_to_binary([
                "Dependency artifact(s) [",
                Names,
                "] were built by a different toolchain and cannot be recompiled "
                "from the workspace"
            ]),
            <<"Run `beamtalk clean --deps` and `beamtalk build`, then re-attach">>
        )}.
