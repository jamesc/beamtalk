%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_release_launcher).

%%% **DDD Context:** Workspace Context — Release Launcher

-moduledoc """
Erlang-side entry points for `bin/<name>`/`bin/<name>.cmd`'s `eval` and `rpc`
verbs (ADR 0125 §1.7, BT-3573). The shell/`.cmd` launcher scripts do only
process/argument wiring (which `erl` to run, `-pa`/`-sname`/`-extra`); every
verb that dispatches into Beamtalk code calls one function here, so there is
exactly one dispatcher (`beamtalk_repl_eval:dispatch_sync/3`) behind both the
WebSocket `run-entry` op and these launcher verbs — never a second one.

* `eval_main/0` runs in `eval`'s throwaway VM: starts the runtime closure
  (`beamtalk_workspace` in release mode, `console` forced off by the launcher's
  `-beamtalk_workspace console false`), dispatches once, and halts with the
  outcome — the separate-VM half of ADR 0125 §1.7's `eval` row.
* `rpc_entry/3` runs *on the already-running foreground node*, dispatched into
  via `rpc:call/5` from `rpc_client_main/0`'s thin client VM. It never halts:
  `Program exit:` inside it is caught and returned as data
  (`{script_exit, Code}`), because §1.7 says it "crashes the dispatching
  session, not the node".
* `rpc_client_main/0`, `ping_client_main/0`, `stop_client_main/0` are the thin
  clients `bin/<name> rpc|ping|stop` boot: a hidden, loopback node that
  resolves the target via `net_adm:ping/1`/`rpc:call/5` and reports through
  its own exit code, never touching the target node's supervision tree except
  through `init:stop/0` (`stop`) or one `run-entry` (`rpc`).
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([
    eval_main/0,
    rpc_entry/3,
    rpc_client_main/0,
    ping_client_main/0,
    stop_client_main/0
]).

%%% ============================================================================
%%% eval — separate VM, dispatch once, halt with the outcome
%%% ============================================================================

-doc """
`bin/<name> eval "Class selector [args…]"`'s Erlang-side entry point.

Expects `init:get_plain_arguments/0` to be `[ClassStr, SelectorStr | Args]`
(the launcher script word-splits the quoted `"Class selector [args]"` and
passes the pieces after `-extra`). Sets `node_owning = true` (ADR 0099 §3 —
`Program exit: N` halts this throwaway VM directly, the same contract the
escript boot module uses), starts the runtime closure, and dispatches through
the shared synchronous path.
""".
-spec eval_main() -> no_return().
eval_main() ->
    case init:get_plain_arguments() of
        [ClassStr, SelectorStr | RestArgs] ->
            application:set_env(beamtalk_runtime, node_owning, true),
            application:set_env(
                beamtalk_runtime, program_name, <<"eval">>
            ),
            case application:ensure_all_started(beamtalk_workspace) of
                {ok, _} ->
                    %% `ensure_all_started/1` loads/starts only the runtime
                    %% closure (`beamtalk_runtime`/`beamtalk_stdlib`/
                    %% `beamtalk_workspace`), whose own start already
                    %% activated *their* bt@* classes
                    %% (`beamtalk_workspace_app:activate_release_modules/0`).
                    %% This VM boots with no `-boot` script at all (§1.7:
                    %% eval starts no distribution and not the project's own
                    %% application), so nothing has yet `application:load`ed
                    %% the project's own app (or its ADR 0070 dependency
                    %% closure) the way a real release boot's systools
                    %% script loads every declared app up front, before any
                    %% of them start. `load_apps_on_code_path/0` does that
                    %% load-only step for every `.app` reachable on the
                    %% `-pa`'d code path, then a second activation pass picks
                    %% up the classes that load newly registers.
                    load_apps_on_code_path(),
                    activate_all_loaded_app_modules(),
                    do_eval_main(ClassStr, SelectorStr, RestArgs);
                {error, Reason} ->
                    io:format(
                        standard_error,
                        "Error: could not start the runtime (~p)~n",
                        [Reason]
                    ),
                    erlang:halt(1)
            end;
        _ ->
            io:format(
                standard_error,
                "usage: eval \"ClassName selector [args...]\"~n",
                []
            ),
            erlang:halt(2)
    end.

-doc """
Best-effort `application:load/1` for every `.app` file reachable on the
current code path (`code:get_path/0`, which already includes every `-pa`
the `eval` launcher script passed) that is not already loaded. This is the
`eval` VM's substitute for what a real boot script's `{load, App}`
instructions do for every declared app *before any of them start* — the
project's own app and its ADR 0070 dependency closure are otherwise never
`application:load`ed here at all, since `eval` deliberately starts only the
runtime closure and never the project's own application (ADR 0125 §1.7).
Failures are silently skipped: a `.app` file that fails to load here is the
same "class won't be found, dispatch errors cleanly" outcome as any other
release oddity, not a launcher crash.
""".
-spec load_apps_on_code_path() -> ok.
load_apps_on_code_path() ->
    lists:foreach(
        fun(Dir) ->
            AppFiles = filelib:wildcard(filename:join(Dir, "*.app")),
            lists:foreach(
                fun(AppFile) ->
                    AppName = list_to_atom(filename:basename(AppFile, ".app")),
                    _ = application:load(AppName)
                end,
                AppFiles
            )
        end,
        code:get_path()
    ).

-doc """
Re-run `beamtalk_module_activation:activate_modules/2` over every module
`collect_loaded_app_modules/0` now sees — the same function
`beamtalk_workspace_app:activate_release_modules/0` calls, run again after
[`load_apps_on_code_path/0`] has loaded apps that were not yet loaded (and so
not yet in `collect_loaded_app_modules/0`'s view) at that first call.
Re-activating an already-loaded module is a no-op (`code:ensure_loaded/1` on
a module already current does not re-run its `-on_load` hook), so this is
safe to call unconditionally rather than tracking which modules are new.
""".
-spec activate_all_loaded_app_modules() -> ok.
activate_all_loaded_app_modules() ->
    Modules = beamtalk_module_activation:collect_loaded_app_modules(),
    {ok, _Errors} = beamtalk_module_activation:activate_modules(Modules, #{}),
    ok.

-spec do_eval_main(string(), string(), [string()]) -> no_return().
do_eval_main(ClassStr, SelectorStr, RestArgs) ->
    ClassBin = unicode:characters_to_binary(ClassStr),
    SelectorBin = unicode:characters_to_binary(SelectorStr),
    Argv = [unicode:characters_to_binary(A) || A <- RestArgs],
    case beamtalk_repl_eval:validate_run_entry(ClassBin, SelectorBin, Argv) of
        {ok, ValidArgv} ->
            case beamtalk_repl_eval:dispatch_sync(ClassBin, SelectorBin, ValidArgv) of
                {ok, _Value} ->
                    erlang:halt(0);
                {script_exit, Code} ->
                    erlang:halt(Code);
                {error, Err} ->
                    catch io:put_chars(
                        standard_error, [beamtalk_error:format_safe(Err, []), $\n]
                    ),
                    erlang:halt(1)
            end;
        {error, Err} ->
            catch io:put_chars(standard_error, [beamtalk_error:format_safe(Err, []), $\n]),
            erlang:halt(1)
    end.

%%% ============================================================================
%%% rpc — dispatch into the running node over distribution
%%% ============================================================================

-doc """
Runs **on the foreground node**, invoked via `rpc:call/5` from
`rpc_client_main/0`. Validates the run-entry shape, then dispatches through
the same `beamtalk_repl_eval:dispatch_sync/3` `eval_main/0` uses. Never
halts and never lets `Program exit:` propagate out of the `rpc:call` worker
(`dispatch_sync/3` already catches it) — the result is always plain data the
client can print.
""".
-spec rpc_entry(binary(), binary(), [binary()]) ->
    {ok, term()} | {script_exit, integer()} | {error, #beamtalk_error{} | term()}.
rpc_entry(ClassBin, SelectorBin, Argv) ->
    case beamtalk_repl_eval:validate_run_entry(ClassBin, SelectorBin, Argv) of
        {ok, ValidArgv} ->
            beamtalk_repl_eval:dispatch_sync(ClassBin, SelectorBin, ValidArgv);
        {error, Err} ->
            {error, Err}
    end.

-doc """
`bin/<name> rpc "Class selector [args…]"`'s thin-client entry point. Expects
`init:get_plain_arguments/0` to be `[NodeStr, ClassStr, SelectorStr | Args]`
(the launcher script resolves the target node name and word-splits the quoted
entry). Runs in a hidden, loopback-only node the launcher already started
with `-sname`/`-hidden`/the release cookie; this function only resolves the
target and calls `rpc_entry/3` on it.
""".
-spec rpc_client_main() -> no_return().
rpc_client_main() ->
    case init:get_plain_arguments() of
        [NodeStr, ClassStr, SelectorStr | RestArgs] ->
            Node = list_to_atom(NodeStr),
            ClassBin = unicode:characters_to_binary(ClassStr),
            SelectorBin = unicode:characters_to_binary(SelectorStr),
            Argv = [unicode:characters_to_binary(A) || A <- RestArgs],
            case rpc:call(Node, ?MODULE, rpc_entry, [ClassBin, SelectorBin, Argv], 60000) of
                {ok, Value} ->
                    io:format("~tp~n", [Value]),
                    erlang:halt(0);
                {script_exit, Code} ->
                    erlang:halt(Code);
                {error, Err} ->
                    catch io:put_chars(
                        standard_error, [beamtalk_error:format_safe(Err, []), $\n]
                    ),
                    erlang:halt(1);
                {badrpc, nodedown} ->
                    io:format(
                        standard_error, "~s is not running (no response from ~p)~n", [
                            NodeStr, Node
                        ]
                    ),
                    erlang:halt(1);
                {badrpc, Reason} ->
                    io:format(standard_error, "rpc failed: ~p~n", [Reason]),
                    erlang:halt(1)
            end;
        _ ->
            io:format(
                standard_error,
                "usage: rpc \"ClassName selector [args...]\"~n",
                []
            ),
            erlang:halt(2)
    end.

%%% ============================================================================
%%% ping / stop — liveness and graceful shutdown over distribution
%%% ============================================================================

-doc """
`bin/<name> ping`'s Erlang-side entry point. Expects
`init:get_plain_arguments/0` to be `[NodeStr]`. Exit code reflects the
result — `0` for `pong`, `1` for `pang` (ADR 0125 §1.7).
""".
-spec ping_client_main() -> no_return().
ping_client_main() ->
    case init:get_plain_arguments() of
        [NodeStr] ->
            Node = list_to_atom(NodeStr),
            case net_adm:ping(Node) of
                pong ->
                    io:format("~s is running (pong)~n", [NodeStr]),
                    erlang:halt(0);
                pang ->
                    io:format(standard_error, "~s is not running (pang)~n", [NodeStr]),
                    erlang:halt(1)
            end;
        _ ->
            erlang:halt(2)
    end.

-doc """
`bin/<name> stop`'s Erlang-side entry point. Expects
`init:get_plain_arguments/0` to be `[NodeStr]`. Calls `init:stop/0` on the
target over distribution — a graceful shutdown that runs the supervision
tree down, unlike `System halt:` (ADR 0099 §3).
""".
-spec stop_client_main() -> no_return().
stop_client_main() ->
    case init:get_plain_arguments() of
        [NodeStr] ->
            Node = list_to_atom(NodeStr),
            case net_adm:ping(Node) of
                pong ->
                    _ = rpc:call(Node, init, stop, []),
                    erlang:halt(0);
                pang ->
                    io:format(standard_error, "~s is not running~n", [NodeStr]),
                    erlang:halt(1)
            end;
        _ ->
            erlang:halt(2)
    end.
