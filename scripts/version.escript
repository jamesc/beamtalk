#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Read the Beamtalk version from the VERSION file and optionally append
%% a dev suffix when the current commit is not an exact release tag.
%%
%% Used by rebar3 {vsn, {cmd, "escript ../../../scripts/version.escript"}}
%% in .app.src files so that all Erlang OTP applications share a single
%% version source. Path is relative to each app dir (runtime/apps/*/).
%%
%% Output (no trailing newline):
%%   "0.2.0"              — when HEAD is tagged v0.2.0
%%   "0.2.0-dev+abc1234"  — when HEAD is not on a release tag

main(_Args) ->
    %% Locate VERSION relative to this script's directory.
    ScriptDir = filename:dirname(escript:script_name()),
    VersionFile = filename:join(filename:dirname(ScriptDir), "VERSION"),
    BaseVersion = read_version(VersionFile),
    FullVersion = maybe_append_git_suffix(BaseVersion),
    io:format("~s", [FullVersion]).

read_version(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            string:trim(binary_to_list(Bin));
        {error, Reason} ->
            io:format(standard_error, "Failed to read ~s: ~p~n", [Path, Reason]),
            halt(1)
    end.

maybe_append_git_suffix(BaseVersion) ->
    case os:find_executable("git") of
        false ->
            BaseVersion;
        _Git ->
            case run_git(["describe", "--exact-match", "--tags", "HEAD"]) of
                {ok, _Tag} ->
                    %% We're on a release tag — use clean version.
                    BaseVersion;
                error ->
                    %% Not on a tag — append dev + short SHA.
                    case run_git(["rev-parse", "--short", "HEAD"]) of
                        {ok, ShortSha} ->
                            BaseVersion ++ "-dev+" ++ ShortSha;
                        error ->
                            BaseVersion
                    end
            end
    end.

run_git(Args) ->
    Cmd = "git " ++ string:join(Args, " "),
    Port = open_port({spawn, Cmd}, [exit_status, stderr_to_stdout, {line, 256}]),
    collect_port_output(Port, []).

collect_port_output(Port, Acc) ->
    receive
        {Port, {data, {eol, Line}}} ->
            collect_port_output(Port, [Line | Acc]);
        {Port, {data, {noeol, Line}}} ->
            collect_port_output(Port, [Line | Acc]);
        {Port, {exit_status, 0}} ->
            case Acc of
                [] -> {ok, ""};
                _ -> {ok, string:trim(lists:last(lists:reverse(Acc)))}
            end;
        {Port, {exit_status, _}} ->
            error
    end.
