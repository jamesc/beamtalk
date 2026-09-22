%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_app).
-behaviour(application).

%%% **DDD Context:** Workspace Context

-moduledoc """
Application callback module for Beamtalk workspace management.

This application provides the interactive development environment including:
- REPL evaluation and protocol
- Workspace management and supervision
- Actor and module tracking
- Session management
- Idle monitoring and auto-cleanup

Dependencies: beamtalk_runtime (core language runtime)

When started, this application registers itself with the runtime to receive
actor spawn notifications, enabling workspace-wide actor tracking.

## App-env-driven workspace start (ADR 0125 §1.4)

The CLI's `run`/`workspace` modes and the escript both start
`beamtalk_workspace_sup` themselves, directly, via an `-eval` string
(`repl_startup.rs`, `run.rs`, `escript.rs`) — `start/2` never sees a `mode`
for them and behaves exactly as before: it starts
`beamtalk_workspace_app_sup` with no children. A release has no `-eval`
string; its `sys.config` sets `mode` (and `console`/`bind`/`auto_cleanup`)
in the `beamtalk_workspace` application env instead, and `start/2` reads it
and starts `beamtalk_workspace_sup` under `beamtalk_workspace_app_sup` via
`beamtalk_workspace_app_sup:start_workspace/1`. Once that workspace is up,
every shipped `bt@*` class is activated from the loaded application
`{modules, …}` lists (`beamtalk_module_activation:collect_loaded_app_modules/0`
+ `activate_modules/2` — the same function the escript uses) rather than
scanned from a `_build/` directory a release host does not have. Both steps
run synchronously inside `start/2`, so they complete — and every class is
registered — before OTP starts the *next* application in the `.rel`, which
is the project's own (ADR 0125 §1.2: `app_file.rs` lists `beamtalk_workspace`
in the project `.app`'s `{applications, …}`, right after `beamtalk_runtime`).
""".

-include_lib("kernel/include/logger.hrl").

-export([start/2, stop/1]).

-ifdef(TEST).
%% Exported for EUnit coverage of the app-env-driven start path
%% (ADR 0125 §1.4) without booting a whole node.
-export([env_workspace_config/1, parse_bind_addr/1, maybe_start_workspace/0]).
-endif.

-doc "Start the workspace application and register actor spawn callback.".
-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    %% Register actor spawn callback with runtime
    %% This allows the runtime to notify us when actors spawn, enabling
    %% workspace-wide tracking without creating a compile-time dependency
    application:set_env(beamtalk_runtime, actor_spawn_callback, beamtalk_repl_actors),
    %% The class-loaded push stream now rides the SystemAnnouncer bus
    %% (`ClassLoaded` / `ClassRemoved` announcements from `beamtalk_object_class`,
    %% subscribed via `beamtalk_repl_subscriptions`), so the legacy
    %% `class_load_callback` → `beamtalk_class_events` wiring was retired.

    %% Start the workspace supervisor tree
    case beamtalk_workspace_app_sup:start_link() of
        {ok, _Pid} = Ok ->
            case maybe_start_workspace() of
                ok -> Ok;
                {error, _} = Err -> Err
            end;
        Err ->
            Err
    end.

-doc """
Start a workspace under `beamtalk_workspace_app_sup` when the
`beamtalk_workspace` application env declares a `mode` (ADR 0125 §1.4).
A missing `mode` key is not an error here — it is the ordinary case for
every mode except release, which starts its workspace elsewhere — so this
returns `ok` and does nothing.
""".
-spec maybe_start_workspace() -> ok | {error, term()}.
maybe_start_workspace() ->
    case application:get_env(beamtalk_workspace, mode) of
        undefined ->
            ok;
        {ok, Mode} ->
            case beamtalk_workspace_app_sup:start_workspace(env_workspace_config(Mode)) of
                {ok, _Pid} ->
                    activate_release_modules(),
                    ok;
                {error, {already_started, _}} ->
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-doc """
Build the `beamtalk_workspace_sup` config map from the `beamtalk_workspace`
application env (ADR 0125 §1.4). `mode` is the only key `maybe_start_workspace/0`
already read; the rest default exactly as `beamtalk_workspace_sup:init/1`
would if the key were absent from an ordinary config map, except `bind`
(a string, the `[release] bind` manifest shape — ADR 0125 §1.2) which is
parsed into the `inet:ip4_address()` `beamtalk_workspace_sup` expects.
""".
-spec env_workspace_config(beamtalk_workspace_sup:mode()) ->
    beamtalk_workspace_sup:workspace_config().
env_workspace_config(Mode) ->
    #{
        workspace_id => env(workspace_id, <<"release">>),
        project_path => env(project_path, undefined),
        mode => Mode,
        console => env(console, false),
        include_compiler => env(include_compiler, false),
        tcp_port => env(tcp_port, undefined),
        bind_addr => parse_bind_addr(env(bind, "127.0.0.1")),
        auto_cleanup => env(auto_cleanup, false)
    }.

-spec env(atom(), term()) -> term().
env(Key, Default) ->
    application:get_env(beamtalk_workspace, Key, Default).

-doc """
Parse the `[release] bind` manifest string into an `inet:ip4_address()`.
Never raises: a malformed `sys.config` value (wrong type, unparseable
string) falls back to loopback with a warning rather than crashing the
whole `start/2` — a config typo should not take the release down.
""".
-spec parse_bind_addr(term()) -> inet:ip4_address().
parse_bind_addr(Addr) when is_tuple(Addr) ->
    Addr;
parse_bind_addr(Addr) when is_binary(Addr) ->
    parse_bind_addr(binary_to_list(Addr));
parse_bind_addr(Addr) when is_list(Addr) ->
    case inet:parse_ipv4_address(Addr) of
        {ok, Parsed} ->
            Parsed;
        {error, Reason} ->
            bind_addr_fallback(Addr, Reason)
    end;
parse_bind_addr(Addr) ->
    bind_addr_fallback(Addr, not_a_string_or_tuple).

-spec bind_addr_fallback(term(), term()) -> inet:ip4_address().
bind_addr_fallback(Addr, Reason) ->
    ?LOG_WARNING(
        "Invalid beamtalk_workspace 'bind' address, falling back to loopback",
        #{bind => Addr, reason => Reason, domain => [beamtalk, runtime]}
    ),
    {127, 0, 0, 1}.

-doc """
Activate every shipped app's `bt@*` classes (ADR 0125 §1.4). Delegates to
`beamtalk_module_activation:activate_modules/2` — the function the escript
also calls (`escript.rs:289-297`) — over the module list
`collect_loaded_app_modules/0` reads from each loaded application's
`{modules, …}` key. There is no `_build/` scan: a release host has no
working tree for one.
""".
-spec activate_release_modules() -> ok.
activate_release_modules() ->
    Modules = beamtalk_module_activation:collect_loaded_app_modules(),
    {ok, Errors} = beamtalk_module_activation:activate_modules(Modules, #{}),
    case Errors of
        [] ->
            ok;
        _ ->
            ?LOG_WARNING(
                "Release activation: some class module(s) failed to activate",
                #{count => length(Errors), errors => Errors, domain => [beamtalk, runtime]}
            )
    end,
    ok.

-doc "Stop the workspace application and unregister actor spawn callback.".
-spec stop(term()) -> ok.
stop(_State) ->
    %% Unregister actor spawn callback
    application:unset_env(beamtalk_runtime, actor_spawn_callback),
    %% Remove WebSocket log handler
    _ = logger:remove_handler(beamtalk_ws_log),
    ok.
