%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_app_tests).

%%% **DDD Context:** Workspace Context

-moduledoc """
EUnit coverage for `beamtalk_workspace_app`'s app-env-driven start path
(ADR 0125 §1.4): `env_workspace_config/1`'s defaults and `parse_bind_addr/1`'s
fallback behaviour on a malformed `[release] bind` value. The happy path
(a real node booting with `mode => release` in `sys.config`) is covered
end-to-end by `beamtalk_release_wire_check_tests`; this suite is the
fast, no-boot-required coverage of the config-parsing edge cases that
suite cannot reach without a full boot per case.
""".

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% env_workspace_config/1
%%====================================================================

env_workspace_config_defaults_test() ->
    clear_env(),
    Config = beamtalk_workspace_app:env_workspace_config(release),
    ?assertEqual(release, maps:get(mode, Config)),
    ?assertEqual(<<"release">>, maps:get(workspace_id, Config)),
    ?assertEqual(undefined, maps:get(project_path, Config)),
    ?assertEqual(false, maps:get(console, Config)),
    ?assertEqual(false, maps:get(include_compiler, Config)),
    ?assertEqual(undefined, maps:get(tcp_port, Config)),
    ?assertEqual({127, 0, 0, 1}, maps:get(bind_addr, Config)),
    ?assertEqual(false, maps:get(auto_cleanup, Config)).

env_workspace_config_reads_overrides_test() ->
    clear_env(),
    ok = application:set_env(beamtalk_workspace, console, true),
    ok = application:set_env(beamtalk_workspace, tcp_port, 4000),
    ok = application:set_env(beamtalk_workspace, bind, "0.0.0.0"),
    try
        Config = beamtalk_workspace_app:env_workspace_config(release),
        ?assertEqual(true, maps:get(console, Config)),
        ?assertEqual(4000, maps:get(tcp_port, Config)),
        ?assertEqual({0, 0, 0, 0}, maps:get(bind_addr, Config))
    after
        clear_env()
    end.

%%====================================================================
%% parse_bind_addr/1
%%====================================================================

parse_bind_addr_valid_string_test() ->
    ?assertEqual({192, 168, 1, 1}, beamtalk_workspace_app:parse_bind_addr("192.168.1.1")).

parse_bind_addr_binary_test() ->
    ?assertEqual({127, 0, 0, 1}, beamtalk_workspace_app:parse_bind_addr(<<"127.0.0.1">>)).

parse_bind_addr_already_tuple_test() ->
    ?assertEqual({10, 0, 0, 1}, beamtalk_workspace_app:parse_bind_addr({10, 0, 0, 1})).

%% A malformed tuple (wrong arity, or a byte out of 0..255) from a
%% hand-edited sys.config falls back to loopback too, instead of being
%% passed through unvalidated to beamtalk_repl_server's bind call.
parse_bind_addr_wrong_arity_tuple_falls_back_test() ->
    ?assertEqual({127, 0, 0, 1}, beamtalk_workspace_app:parse_bind_addr({10, 0, 0})).

parse_bind_addr_out_of_range_tuple_falls_back_test() ->
    ?assertEqual({127, 0, 0, 1}, beamtalk_workspace_app:parse_bind_addr({10, 0, 0, 999})).

parse_bind_addr_non_integer_tuple_falls_back_test() ->
    ?assertEqual({127, 0, 0, 1}, beamtalk_workspace_app:parse_bind_addr({10, 0, 0, not_a_byte})).

%% A config typo (unparseable string) falls back to loopback rather than
%% crashing the whole start/2.
parse_bind_addr_unparseable_string_falls_back_test() ->
    ?assertEqual({127, 0, 0, 1}, beamtalk_workspace_app:parse_bind_addr("not-an-address")).

%% A wrong-typed config value (e.g. an atom, from a hand-edited sys.config)
%% falls back too, instead of a function_clause crash.
parse_bind_addr_wrong_type_falls_back_test() ->
    ?assertEqual({127, 0, 0, 1}, beamtalk_workspace_app:parse_bind_addr(not_a_string)).

%%====================================================================
%% maybe_start_workspace/0
%%====================================================================

%% No `mode` set (the ordinary case for every mode except release, which
%% starts its workspace via the CLI's `-eval` string instead) is a no-op,
%% not an error.
maybe_start_workspace_noop_without_mode_test() ->
    clear_env(),
    ?assertEqual(ok, beamtalk_workspace_app:maybe_start_workspace()).

%% A bad config caught by beamtalk_workspace_sup:init/1 — console => true
%% with no tcp_port — surfaces as {error, _} from maybe_start_workspace/0
%% rather than crashing the caller, so beamtalk_workspace_app:start/2 can
%% fail the whole application boot loudly instead of leaving a half-started
%% node. Requires a real beamtalk_workspace_app_sup, since start_workspace/1
%% adds the failing child to it — reused if another suite already started
%% one in this shared EUnit node (own only what we started ourselves, so a
%% full `--app=beamtalk_workspace` run doesn't fight over the registered
%% name).
maybe_start_workspace_bad_config_returns_error_test_() ->
    {setup,
        fun() ->
            clear_env(),
            ok = application:set_env(beamtalk_workspace, mode, release),
            ok = application:set_env(beamtalk_workspace, console, true),
            %% Deliberately no tcp_port set.
            ensure_workspace_app_sup()
        end,
        fun(OwnedPid) ->
            clear_env(),
            stop_owned_workspace_app_sup(OwnedPid)
        end,
        fun(_OwnedPid) ->
            ?_assertMatch({error, _}, beamtalk_workspace_app:maybe_start_workspace())
        end}.

%%====================================================================
%% Helpers
%%====================================================================

clear_env() ->
    lists:foreach(
        fun(Key) -> application:unset_env(beamtalk_workspace, Key) end,
        [mode, workspace_id, project_path, console, include_compiler, tcp_port, bind, auto_cleanup]
    ).

-doc """
Start `beamtalk_workspace_app_sup` if nothing else already has (a `{local,
beamtalk_workspace_app_sup}`-registered process is shared across every
EUnit test in this node). Returns `owned` when this call started it —
cleanup then owns stopping it — or `not_owned` when it was already running,
in which case cleanup leaves it alone.
""".
-spec ensure_workspace_app_sup() -> owned | not_owned.
ensure_workspace_app_sup() ->
    case whereis(beamtalk_workspace_app_sup) of
        undefined ->
            {ok, Pid} = beamtalk_workspace_app_sup:start_link(),
            true = erlang:unlink(Pid),
            owned;
        _Pid ->
            not_owned
    end.

-spec stop_owned_workspace_app_sup(owned | not_owned) -> ok.
stop_owned_workspace_app_sup(owned) ->
    case whereis(beamtalk_workspace_app_sup) of
        undefined -> ok;
        Pid -> exit(Pid, shutdown)
    end,
    ok;
stop_owned_workspace_app_sup(not_owned) ->
    ok.
