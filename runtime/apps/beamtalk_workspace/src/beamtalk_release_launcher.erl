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
* `tls_preflight_main/0` runs before any distributed node when the operator
  opts in to cross-host clustering with `RELEASE_DISTRIBUTION=tls` (ADR 0126
  amendment D2, BT-5233): it refuses to go on unless the TLS option file makes
  both sides verify their peer, and prints the distribution flags the launcher
  then passes to every node it starts.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% How long the `rpc` client waits for the target node's run-entry. Keep it
%% below the outer wall-clock limit in examples/cluster/scripts/e2e.sh
%% (CLUSTER_E2E_TIMEOUT, default 150 s) so the `{badrpc, timeout}` failure
%% is reported before the outer limit kills the client.
-define(RPC_CALL_TIMEOUT_MS, 120000).

-export([
    eval_main/0,
    rpc_entry/3,
    rpc_client_main/0,
    ping_client_main/0,
    stop_client_main/0,
    tls_preflight_main/0,
    tls_preflight/2
]).

%%% ============================================================================
%%% eval — separate VM, dispatch once, halt with the outcome
%%% ============================================================================

-doc """
`bin/<name> eval "Class selector [args…]"`'s Erlang-side entry point.

Expects `init:get_plain_arguments/0` to be `[ClassStr, SelectorStr | Args]`
(the launcher script word-splits the quoted `"Class selector [args]"` and
passes the pieces after `-extra`). The release's workspace boots with
`node_owning = true` (a capability, BT-3634), so `Program exit: N` stops this
throwaway VM gracefully with status `N` (`beamtalk_script_harness:stop_node/1`,
the same contract the escript boot module uses). Starts the runtime closure
and dispatches through the shared synchronous path.
""".
-spec eval_main() -> no_return().
eval_main() ->
    %% eval VM has no sys.config logger redirect; route to stderr before any log call.
    _ = logger:update_handler_config(default, config, #{type => standard_error}),
    logger:set_process_metadata(#{domain => [beamtalk, runtime]}),
    case init:get_plain_arguments() of
        [ClassStr, SelectorStr | RestArgs] ->
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
                    ?LOG_ERROR("could not start the runtime: ~p", [Reason], #{
                        domain => [beamtalk, runtime]
                    }),
                    beamtalk_script_harness:halt_unless_stopping(1)
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
                    beamtalk_script_harness:halt_unless_stopping(0);
                {script_exit, Code} ->
                    beamtalk_script_harness:stop_node(Code);
                {error, Err} ->
                    try
                        io:put_chars(
                            standard_error, [beamtalk_error:format_safe(Err, []), $\n]
                        )
                    catch
                        _:_ -> ok
                    end,
                    %% The entry's call chain can fail (e.g. time out) while an
                    %% actor's `Program exit: N` graceful stop is in flight.
                    beamtalk_script_harness:halt_unless_stopping(1)
            end;
        {error, Err} ->
            try
                io:put_chars(standard_error, [beamtalk_error:format_safe(Err, []), $\n])
            catch
                _:_ -> ok
            end,
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
            case
                rpc:call(
                    Node, ?MODULE, rpc_entry, [ClassBin, SelectorBin, Argv], ?RPC_CALL_TIMEOUT_MS
                )
            of
                {ok, Value} ->
                    io:format("~tp~n", [Value]),
                    erlang:halt(0);
                {script_exit, Code} ->
                    erlang:halt(Code);
                {error, Err} ->
                    try
                        io:put_chars(
                            standard_error, [beamtalk_error:format_safe(Err, []), $\n]
                        )
                    catch
                        _:_ -> ok
                    end,
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

%%% ============================================================================
%%% RELEASE_DISTRIBUTION=tls — check the opt-in before any node starts
%%% ============================================================================

-doc """
The `RELEASE_DISTRIBUTION=tls` check `bin/<name>` runs (in a VM with no
distribution, but the release's `vm.args` and the `ERL_*FLAGS` variables, so
it sees every `-ssl_dist_opt` flag the node will) before it starts any
distributed node (ADR 0126 amendment D2, BT-5233). On success it prints one
`tls-<key>=<value>` line per value the launcher needs and halts with 0;
otherwise it prints each problem with its fix to stderr and halts with 2, so
the launcher refuses to start. See `tls_preflight/2` for what it checks.
""".
-spec tls_preflight_main() -> no_return().
tls_preflight_main() ->
    Args = #{
        dist_opt_words => init_words(ssl_dist_opt),
        extra_optfiles => init_words(ssl_dist_optfile),
        extra_proto_dists => init_words(proto_dist),
        epmd_listening => fun epmd_listening/2
    },
    case tls_preflight(fun os:getenv/1, Args) of
        {ok, #{proto := Proto, interface := Ip, host := Host, optfile := OptFile} = Result} ->
            %% Notes are for the operator starting the node; a health check's
            %% `ping` every few seconds should not repeat them.
            [
                io:format(standard_error, "note: ~ts~n", [Note])
             || init:get_plain_arguments() =:= ["foreground"], Note <- maps:get(notes, Result)
            ],
            io:format(
                "tls-proto=~ts~ntls-interface=~w~ntls-epmd=~ts~ntls-host=~ts~ntls-optfile=~ts~n",
                [Proto, Ip, inet:ntoa(Ip), Host, OptFile]
            ),
            erlang:halt(0);
        {error, Problems} ->
            io:format(
                standard_error,
                "error: RELEASE_DISTRIBUTION=tls is set, but this node cannot start with it:~n",
                []
            ),
            [io:format(standard_error, "  - ~ts~n", [P]) || P <- Problems],
            erlang:halt(2)
    end.

-doc """
Check a `RELEASE_DISTRIBUTION=tls` opt-in, reading the environment with
`GetEnv` (`os:getenv/1`'s contract):

* `RELEASE_HOST` names this host as the other nodes reach it (a DNS name its
  certificate names, or an IP address); it becomes the host part of `-name`.
* `RELEASE_DIST_INTERFACE` is one IP address of this host to listen on, parsed
  by the shared rule (`beamtalk_dist_address:parse/1`; a `%zone` is refused,
  since the listener cannot carry one). There is no default, so the node
  listens on every interface (`0.0.0.0`) only if the operator names it. An
  IPv6 address selects `inet6_tls`. If an epmd already runs on this host (on
  loopback) but does not listen on the interface, other hosts could never
  find the node: epmd keeps the address of whatever started it first.
* `RELEASE_SSL_DIST_OPTFILE` is the `-ssl_dist_optfile` file. With the
  `-ssl_dist_opt` words the node will also get (`dist_opt_words`), it must
  pass `beamtalk_dist_tls:check_optfile/2`. A second option file from
  `vm.args` (`extra_optfiles`) is refused, and so is a `-proto_dist` there
  (`extra_proto_dists`): the launcher passes its own first, so OTP would
  ignore it, with only a warning.
* A `RELEASE_HOST` that is an IP address must be the same family (IPv4 or
  IPv6) as the interface, or other nodes could not connect to it.

`Args` carries what the VM supplies: `dist_opt_words`, `extra_optfiles`,
`extra_proto_dists` (optional, default none) and
`epmd_listening(Address, Port) -> boolean()`. Returns every problem at once,
each with its fix.
""".
-spec tls_preflight(fun((string()) -> string() | false), #{
    dist_opt_words := [string()],
    extra_optfiles := [string()],
    extra_proto_dists => [string()],
    epmd_listening := fun((inet:ip_address(), inet:port_number()) -> boolean())
}) ->
    {ok, #{
        proto := string(),
        interface := inet:ip_address(),
        host := string(),
        optfile := string(),
        notes := [string()]
    }}
    | {error, [unicode:chardata(), ...]}.
tls_preflight(
    GetEnv, #{dist_opt_words := Words, extra_optfiles := Extra, epmd_listening := Epmd} = Args
) ->
    Host = tls_host(read_env("RELEASE_HOST", GetEnv)),
    Interface = tls_interface(read_env("RELEASE_DIST_INTERFACE", GetEnv)),
    OptFile = tls_optfile(read_env("RELEASE_SSL_DIST_OPTFILE", GetEnv), Words, Extra),
    EpmdCheck =
        case Interface of
            {ok, {Ip0, _}} -> tls_epmd(Ip0, epmd_port(GetEnv), Epmd);
            {error, _} -> ok
        end,
    ProtoCheck =
        case maps:get(extra_proto_dists, Args, []) of
            [] ->
                ok;
            Protos ->
                {error,
                    io_lib:format(
                        "vm.args (or ERL_FLAGS) also sets -proto_dist ~ts; remove it: "
                        "RELEASE_DISTRIBUTION=tls sets the protocol",
                        [lists:join(" ", Protos)]
                    )}
        end,
    FamilyCheck =
        case {Host, Interface} of
            {{ok, H0}, {ok, {Ip1, _}}} -> tls_family(H0, Ip1);
            _ -> ok
        end,
    Checks = [Host, Interface, OptFile, EpmdCheck, ProtoCheck, FamilyCheck],
    case [Problem || {error, Problem} <- Checks] of
        [] ->
            {ok, H} = Host,
            {ok, {Ip, Notes0}} = Interface,
            Notes = Notes0 ++ [Note || {note, Note} <- [EpmdCheck]],
            {ok, F} = OptFile,
            Proto =
                case tuple_size(Ip) of
                    4 -> "inet_tls";
                    8 -> "inet6_tls"
                end,
            {ok, #{proto => Proto, interface => Ip, host => H, optfile => F, notes => Notes}};
        Problems ->
            {error, Problems}
    end.

%% An IP-literal RELEASE_HOST names the address other nodes dial; it has to be
%% reachable over the protocol family the interface selects.
tls_family(Host, Ip) ->
    case beamtalk_dist_address:parse(Host) of
        {ok, HostIp} when tuple_size(HostIp) =/= tuple_size(Ip) ->
            {error,
                io_lib:format(
                    "RELEASE_HOST ~ts and RELEASE_DIST_INTERFACE ~ts are different IP families; "
                    "the node listens on one only, so use addresses of the same family",
                    [Host, inet:ntoa(Ip)]
                )};
        _ ->
            ok
    end.

init_words(Flag) ->
    case init:get_argument(Flag) of
        {ok, Values} -> lists:append(Values);
        error -> []
    end.

%% Whether something accepts TCP connections at `Address:Port` (epmd's).
epmd_listening(Address, Port) ->
    case gen_tcp:connect(Address, Port, [], 1000) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            true;
        {error, _} ->
            false
    end.

epmd_port(GetEnv) ->
    case string:to_integer(read_env("ERL_EPMD_PORT", GetEnv)) of
        {Port, ""} when Port > 0, Port < 65536 -> Port;
        _ -> 4369
    end.

%% An epmd already running on loopback but not on the interface keeps its
%% address (it is shared, and binds whatever the first starter gave it), so
%% this node would register where no other host can look it up. Nothing to
%% check for a loopback listener. For every interface (`0.0.0.0`, `::`) a
%% probe cannot tell the cases apart (it always reaches loopback), so a
%% running epmd only earns a note.
tls_epmd(Ip, Port, Epmd) ->
    Wildcard = Ip =:= {0, 0, 0, 0} orelse Ip =:= {0, 0, 0, 0, 0, 0, 0, 0},
    case
        beamtalk_dist_address:is_loopback(Ip) orelse not Epmd({127, 0, 0, 1}, Port) orelse
            (not Wildcard andalso Epmd(Ip, Port))
    of
        true ->
            ok;
        false when Wildcard ->
            {note,
                io_lib:format(
                    "an epmd is already running on this host (port ~b) and keeps the address "
                    "it was started with; if a loopback release or a beamtalk REPL started it, "
                    "other hosts cannot find this node (check with `epmd -names` from another "
                    "host, or run `epmd -kill` and start this node first)",
                    [Port]
                )};
        false ->
            {error,
                io_lib:format(
                    "an epmd is already running on this host but does not listen on ~ts "
                    "(port ~b), so other hosts could not find this node; it keeps the address "
                    "of whatever started it first (a loopback release or a beamtalk REPL). "
                    "Stop the nodes using it and run `epmd -kill`, then start this node first",
                    [inet:ntoa(Ip), Port]
                )}
    end.

read_env(Name, GetEnv) ->
    case GetEnv(Name) of
        false -> "";
        Value -> string:trim(Value)
    end.

tls_host("") ->
    {error,
        "RELEASE_HOST is not set: set it to this host's name as the other nodes reach it "
        "(the DNS name its certificate names), e.g. RELEASE_HOST=node1.example.com"};
tls_host(Host) ->
    %% `parse/1` drops a `%zone`, which would then end up in `-name`.
    case not lists:member($%, Host) andalso beamtalk_dist_address:parse(Host) of
        {ok, _} ->
            {ok, Host};
        _ ->
            case valid_host_name(Host) of
                true ->
                    {ok, Host};
                false ->
                    {error,
                        io_lib:format(
                            "RELEASE_HOST '~ts' is not a host name or IP address "
                            "(letters, digits, '-' and '.' only)",
                            [Host]
                        )}
            end
    end.

%% A name whose labels are all digits (`10.1`, `010.0.0.1`) is not an IP
%% literal by the shared rule, but the resolver would still read it as one in
%% legacy shorthand, so it is refused rather than passed to `-name`.
valid_host_name(Host) ->
    Labels = string:split(Host, ".", all),
    not lists:all(
        fun(L) -> L =/= "" andalso lists:all(fun(C) -> C >= $0 andalso C =< $9 end, L) end, Labels
    ) andalso
        length(Host) =< 253 andalso
        lists:all(
            fun(Label) ->
                Label =/= "" andalso length(Label) =< 63 andalso
                    hd(Label) =/= $- andalso lists:last(Label) =/= $- andalso
                    lists:all(fun host_char/1, Label)
            end,
            string:split(Host, ".", all)
        ).

host_char(C) ->
    (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) orelse
        (C >= $0 andalso C =< $9) orelse C =:= $-.

tls_interface("") ->
    {error,
        "RELEASE_DIST_INTERFACE is not set: set it to the IP address of this host "
        "the other nodes connect to, e.g. RELEASE_DIST_INTERFACE=10.0.0.5 "
        "(name 0.0.0.0 to listen on every interface)"};
tls_interface(Value) ->
    case lists:member($%, Value) of
        true ->
            {error,
                io_lib:format(
                    "RELEASE_DIST_INTERFACE '~ts' has a %zone, which the distribution listener "
                    "cannot use; name a global or unique-local address instead",
                    [Value]
                )};
        false ->
            tls_interface_address(Value)
    end.

tls_interface_address(Value) ->
    case beamtalk_dist_address:parse(Value) of
        {ok, Ip} ->
            Notes =
                case beamtalk_dist_address:loopback_dist(Value) of
                    true ->
                        [
                            io_lib:format(
                                "RELEASE_DIST_INTERFACE ~ts is a loopback address: "
                                "other hosts cannot reach this node",
                                [Value]
                            )
                        ];
                    false ->
                        []
                end,
            {ok, {Ip, Notes}};
        error ->
            {error,
                io_lib:format(
                    "RELEASE_DIST_INTERFACE '~ts' is not one IP address "
                    "(a host name or a list is not accepted), e.g. 10.0.0.5 or fd00::5",
                    [Value]
                )}
    end.

tls_optfile("", _Words, _Extra) ->
    {error,
        "RELEASE_SSL_DIST_OPTFILE is not set: set it to the TLS distribution option file "
        "that names this node's certificate, key and CA, and makes both sides verify their "
        "peer (see docs/development/deploying.md, \"Clustering across hosts\")"};
tls_optfile(_Path, _Words, [_ | _] = Extra) ->
    {error,
        io_lib:format(
            "vm.args (or ERL_FLAGS) also sets -ssl_dist_optfile ~ts; remove it and set "
            "RELEASE_SSL_DIST_OPTFILE instead",
            [lists:join(" ", Extra)]
        )};
tls_optfile(Path0, Words, []) ->
    Path = filename:absname(Path0),
    %% The launchers splice the path into `erl`'s command line; `launcher.cmd`
    %% runs under delayed expansion, where `!`, `^` and `%` are not literal.
    case [C || C <- Path, lists:member(C, "\"!^%") orelse C < 32] of
        [_ | _] ->
            {error,
                io_lib:format(
                    "RELEASE_SSL_DIST_OPTFILE '~ts' must not contain '\"', '!', '^', '%' "
                    "or control characters",
                    [Path]
                )};
        [] ->
            case beamtalk_dist_tls:check_optfile(Path, Words) of
                ok ->
                    {ok, Path};
                {error, Reason} ->
                    {error, beamtalk_dist_tls:format_error(Reason)}
            end
    end.
