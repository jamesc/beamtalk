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
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% ensure_console_cookie/1 (ADR 0125 §1.6, BT-3575)
%%====================================================================

%% run/workspace modes are CLI-driven and always pass an explicit cookie
%% already — the check is a no-op regardless of `console`.
ensure_console_cookie_noop_for_run_mode_test() ->
    ?assertEqual(
        ok, beamtalk_workspace_app:ensure_console_cookie(#{mode => run, console => true})
    ).

ensure_console_cookie_noop_for_workspace_mode_test() ->
    ?assertEqual(
        ok, beamtalk_workspace_app:ensure_console_cookie(#{mode => workspace, console => true})
    ).

%% A release with the console off opens no listener for the cookie to guard.
ensure_console_cookie_noop_for_release_without_console_test() ->
    ?assertEqual(
        ok, beamtalk_workspace_app:ensure_console_cookie(#{mode => release, console => false})
    ).

%% `mode => release, console => true` is the only combination that actually
%% consults `init:get_argument(setcookie)` — asserted here against whatever
%% that argument's real value is in this eunit node (never `erlang:
%% get_cookie/0`, which is never `nocookie` once OTP's own
%% `$HOME/.erlang.cookie` fallback has run — see the module doc for why).
%% A `beamtalk repl`/e2e-style boot that explicitly passes `-setcookie`
%% (`crates/beamtalk-cli/tests/repl_protocol.rs`) exercises the `ok` branch
%% end to end; this assertion is self-consistent either way.
ensure_console_cookie_release_console_matches_setcookie_argument_test() ->
    Result = beamtalk_workspace_app:ensure_console_cookie(#{mode => release, console => true}),
    case init:get_argument(setcookie) of
        {ok, _} ->
            ?assertEqual(ok, Result);
        error ->
            ?assertMatch({error, #beamtalk_error{kind = release_console_no_cookie}}, Result)
    end.

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
%% release_workspace_id/0 (BT-3617 follow-up — RELEASE_NODE, ADR 0126 §9)
%%====================================================================

%% No RELEASE_NODE set: falls back to the sys.config-baked workspace_id
%% (the release's own name, `generate_sys_config`) — today's behaviour,
%% unchanged.
release_workspace_id_falls_back_to_sys_config_test() ->
    clear_env(),
    ok = application:set_env(beamtalk_workspace, workspace_id, <<"symphony">>),
    true = os:unsetenv("RELEASE_NODE"),
    try
        ?assertEqual(<<"symphony">>, beamtalk_workspace_app:release_workspace_id())
    after
        clear_env()
    end.

%% RELEASE_NODE set: it wins over the sys.config-baked workspace_id — this
%% is what lets two instances of the *same* release, started with distinct
%% RELEASE_NODE values, scope their REPL port files
%% (~/.beamtalk/workspaces/<workspace_id>/port) independently, matching the
%% distinct `-sname`s `launcher.sh`'s `node_sname` gives them.
release_workspace_id_prefers_release_node_env_var_test() ->
    clear_env(),
    ok = application:set_env(beamtalk_workspace, workspace_id, <<"symphony">>),
    true = os:putenv("RELEASE_NODE", "symphony1"),
    try
        ?assertEqual(<<"symphony1">>, beamtalk_workspace_app:release_workspace_id())
    after
        os:unsetenv("RELEASE_NODE"),
        clear_env()
    end.

%% An empty RELEASE_NODE (e.g. `RELEASE_NODE= bin/symphony foreground`,
%% rather than genuinely unset) must not produce an empty-binary
%% workspace_id — treated the same as unset.
release_workspace_id_treats_empty_release_node_as_unset_test() ->
    clear_env(),
    ok = application:set_env(beamtalk_workspace, workspace_id, <<"symphony">>),
    true = os:putenv("RELEASE_NODE", ""),
    try
        ?assertEqual(<<"symphony">>, beamtalk_workspace_app:release_workspace_id())
    after
        os:unsetenv("RELEASE_NODE"),
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
%% start/2 — orphaned-supervisor cleanup on a failed workspace start
%%====================================================================

%% A failed maybe_start_workspace/0 must not leave beamtalk_workspace_app_sup
%% running. application:start/2 only links to (and so only cleans up) the
%% Pid start/2 successfully *returns*; a normal {error, _} return from
%% start/2 does not, by itself, propagate through the link to the
%% supervisor start/2 already started via beamtalk_workspace_app_sup:
%% start_link/0. Left alone, that supervisor would survive registered under
%% its own name — orphaned, unsupervised, and permanently blocking every
%% later application:start(beamtalk_workspace) retry with
%% {error, {already_started, OldPid}} until the node restarts. Proven two
%% ways below: the registered name is free again immediately after the
%% failed start/2 call, and a second start/2 call can retry — failing again
%% for the *same* config reason, never {already_started, _}.
%%
%% Some other suite in this shared EUnit node keeps a *real*
%% `beamtalk_workspace` application running for the rest of the test run
%% (e.g. beamtalk_repl_server_tests.erl, via ensure_all_started, never
%% torn down) — many later tests (workspace_meta's package name, etc.)
%% depend on that instance's accumulated state surviving. Stopping and
%% restarting the whole application here to get a "clean slate" was tried
%% and rejected: it reset that shared state and broke unrelated suites
%% (verified empirically). Instead, this test only ever touches the
%% *name* `beamtalk_workspace_app_sup` is registered under, briefly:
%% unregister it (the real process, if any, keeps running completely
%% undisturbed — application_master holds its Pid directly, not the name),
%% run beamtalk_workspace_app:start/2 as a fresh, isolated instance under
%% that now-free name, then restore the original registration (or confirm
%% our own instance is gone, if there was none) in cleanup.
start_cleans_up_orphaned_supervisor_on_failed_workspace_start_test_() ->
    {setup,
        fun() ->
            PriorPid = whereis(beamtalk_workspace_app_sup),
            case PriorPid of
                undefined -> ok;
                Pid when is_pid(Pid) -> true = erlang:unregister(beamtalk_workspace_app_sup)
            end,
            clear_env(),
            ok = application:set_env(beamtalk_workspace, mode, release),
            ok = application:set_env(beamtalk_workspace, console, true),
            %% Deliberately no tcp_port set.
            PriorPid
        end,
        fun(PriorPid) ->
            clear_env(),
            case whereis(beamtalk_workspace_app_sup) of
                undefined -> ok;
                OwnPid -> ok = proc_lib:stop(OwnPid, shutdown, 5000)
            end,
            case PriorPid of
                undefined -> ok;
                Pid when is_pid(Pid) -> true = erlang:register(beamtalk_workspace_app_sup, Pid)
            end
        end,
        fun(_PriorPid) ->
            [
                ?_test(begin
                    ?assertMatch({error, _}, beamtalk_workspace_app:start(normal, [])),
                    ?assertEqual(undefined, whereis(beamtalk_workspace_app_sup))
                end),
                ?_test(begin
                    Result = beamtalk_workspace_app:start(normal, []),
                    ?assertMatch({error, _}, Result),
                    ?assertNotMatch({error, {already_started, _}}, Result),
                    ?assertEqual(undefined, whereis(beamtalk_workspace_app_sup))
                end)
            ]
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

-doc """
Synchronous, unlike a bare `exit(Pid, shutdown)` — waits (via
`proc_lib:stop/3`) for the process to actually terminate and unregister
before returning, so a later test's own `whereis(beamtalk_workspace_app_sup)`
check can't race an async shutdown left over from this one.
""".
-spec stop_owned_workspace_app_sup(owned | not_owned) -> ok.
stop_owned_workspace_app_sup(owned) ->
    case whereis(beamtalk_workspace_app_sup) of
        undefined -> ok;
        Pid -> ok = proc_lib:stop(Pid, shutdown, 5000)
    end,
    ok;
stop_owned_workspace_app_sup(not_owned) ->
    ok.
