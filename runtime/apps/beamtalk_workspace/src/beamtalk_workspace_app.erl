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

## Console cookie check (ADR 0125 §1.6, BT-3575)

`mode => release, console => true` with no cookie configured refuses to
boot — `maybe_start_workspace/0` checks this *before* calling
`start_workspace/1`, so a release with the console on and no cookie never
starts `beamtalk_workspace_sup` at all: `start/2` returns `{error, Err}`
with `Err` a structured `#beamtalk_error{}`, which fails
`application:start(beamtalk_workspace)` and so the whole boot script — a
release with an unauthenticated eval endpoint is refused outright, not
booted with a warning. `ensure_console_cookie/1` checks
`init:get_argument(setcookie)` rather than `erlang:get_cookie/0`: the latter
is never `nocookie` on a node that ever started distribution, because OTP
silently falls back to `$HOME/.erlang.cookie` when no `-setcookie` was
given — exactly the "the operator never actually configured one" case this
check exists to catch (ADR 0125 §1.6: "The cookie is read from
`RELEASE_COOKIE`/`vm.args`, never generated into the artifact"). Whichever
of those two sources supplied a cookie, it reaches `init`'s argument list as
an ordinary `-setcookie` flag — the launcher's `RELEASE_COOKIE`-derived
`cookie_args()`, or a line in a user `[release] vm-args` file — so this one
check covers both.
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([start/2, stop/1]).

-ifdef(TEST).
%% Exported for EUnit coverage of the app-env-driven start path
%% (ADR 0125 §1.4) without booting a whole node.
-export([
    env_workspace_config/1,
    parse_bind_addr/1,
    maybe_start_workspace/0,
    ensure_console_cookie/1,
    release_workspace_id/0
]).
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
        {ok, SupPid} = Ok ->
            case maybe_start_workspace() of
                ok ->
                    Ok;
                {error, _} = Err ->
                    %% A normal {error, _} return from start/2 does not, by
                    %% itself, take down a process this callback already
                    %% link-started: application_master only links to (and
                    %% so only cleans up) the Pid start/2 successfully
                    %% returns. Left alone, SupPid would survive registered
                    %% under its own name — orphaned, unsupervised, and
                    %% blocking every later `application:start(beamtalk_
                    %% workspace)` retry with `{error, {already_started,
                    %% SupPid}}` until the node restarts. Stop it ourselves
                    %% before reporting the failure.
                    stop_orphaned_app_sup(SupPid),
                    Err
            end;
        Err ->
            Err
    end.

-doc """
Synchronously stop `beamtalk_workspace_app_sup` after a failed
`maybe_start_workspace/0` (see the comment at its call site), via
`proc_lib:stop/3` — the generic synchronous-stop-and-wait every `proc_lib`
process gets regardless of behaviour, and (unlike `supervisor`, which has
no `stop/1,3` of its own) works directly on a supervisor pid; used here
rather than `gen_server:stop/1` only because that name would misleadingly
suggest `beamtalk_workspace_app_sup` were a `gen_server` — it delegates to
this exact same function underneath (`gen_server:stop/1` → `gen:stop/1` →
`proc_lib:stop/3`), so either spelling stops it identically. Unlinks first
so the shutdown does not also deliver an EXIT signal to this process —
`start/2`'s caller handles the `{error, _}` it is about to return; it
should not additionally receive one.
""".
-spec stop_orphaned_app_sup(pid()) -> ok.
stop_orphaned_app_sup(SupPid) ->
    true = erlang:unlink(SupPid),
    ok = proc_lib:stop(SupPid, shutdown, 5000).

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
            Config = env_workspace_config(Mode),
            case ensure_console_cookie(Config) of
                ok ->
                    start_workspace(Config);
                {error, Err} ->
                    {error, Err}
            end
    end.

-spec start_workspace(beamtalk_workspace_sup:workspace_config()) -> ok | {error, term()}.
start_workspace(Config) ->
    case beamtalk_workspace_app_sup:start_workspace(Config) of
        {ok, _Pid} ->
            activate_release_modules(),
            ok;
        {error, {already_started, _}} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-doc """
Refuse `mode => release, console => true` with no cookie configured (ADR
0125 §1.6) — see the moduledoc's "Console cookie check" section for why
`init:get_argument(setcookie)`, not `erlang:get_cookie/0`, is the actual
test. A no-op for every other `{mode, console}` combination: `run`/
`workspace` are CLI-driven and always pass an explicit cookie already, and a
release with `console => false` opens no listener for the cookie to guard.
""".
-spec ensure_console_cookie(beamtalk_workspace_sup:workspace_config()) ->
    ok | {error, #beamtalk_error{}}.
ensure_console_cookie(#{mode := release, console := true}) ->
    case init:get_argument(setcookie) of
        {ok, _} -> ok;
        error -> {error, no_console_cookie_error()}
    end;
ensure_console_cookie(_Config) ->
    ok.

-doc "The structured refusal `ensure_console_cookie/1` raises — see its own doc.".
-spec no_console_cookie_error() -> #beamtalk_error{}.
no_console_cookie_error() ->
    Message = <<
        "[release] console = true requires a cookie, and none is configured.\n\n"
        "  This node ships an authenticated WebSocket console with no cookie set,\n"
        "  which is not a smaller version of the trust boundary ADR 0058 describes;\n"
        "  it is no boundary at all. A cookie baked into the release tarball is not\n"
        "  an option either: that would be a shared secret sitting in a registry."
    >>,
    Hint = <<
        "Set RELEASE_COOKIE before starting the release (bin/<name> foreground), "
        "or add '-setcookie <cookie>' to a [release] vm-args file (see ADR 0125 "
        "section 1.6)."
    >>,
    Err0 = beamtalk_error:new(release_console_no_cookie, 'Beamtalk'),
    Err1 = beamtalk_error:with_message(Err0, Message),
    beamtalk_error:with_hint(Err1, Hint).

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
        workspace_id => release_workspace_id(),
        project_path => env(project_path, undefined),
        mode => Mode,
        console => env(console, false),
        include_compiler => env(include_compiler, false),
        tcp_port => env(tcp_port, undefined),
        bind_addr => parse_bind_addr(env(bind, "127.0.0.1")),
        auto_cleanup => env(auto_cleanup, false)
    }.

-doc """
This node's `workspace_id` — the `RELEASE_NODE` environment variable when
set, else the sys.config-baked `workspace_id` (the release's own name,
`generate_sys_config`). `RELEASE_NODE` is the launcher's own `-sname`
override (`launcher.sh`'s `node_sname`, mirroring `RELEASE_COOKIE` and
`mix release`'s identically-named variable) — reading the same env var here
means one setting scopes both the distribution node name *and* the REPL
port file (`~/.beamtalk/workspaces/<workspace_id>/port`), so two instances
of the same release started with different `RELEASE_NODE` values on one
host never collide on either.
""".
-spec release_workspace_id() -> binary().
release_workspace_id() ->
    case os:getenv("RELEASE_NODE") of
        false -> env(workspace_id, <<"release">>);
        "" -> env(workspace_id, <<"release">>);
        NodeStr -> list_to_binary(NodeStr)
    end.

-spec env(atom(), term()) -> term().
env(Key, Default) ->
    application:get_env(beamtalk_workspace, Key, Default).

-doc """
Parse the `[release] bind` manifest string into an `inet:ip4_address()`.
Never raises: a malformed `sys.config` value — wrong type, an unparseable
string, or a tuple that is not a well-formed 4-tuple of `0..255` integers
(wrong arity, a non-integer element, or an out-of-range byte) — falls back
to loopback with a warning rather than crashing the whole `start/2` — a
config typo should not take the release down.
""".
-spec parse_bind_addr(term()) -> inet:ip4_address().
parse_bind_addr({A, B, C, D} = Addr) when
    is_integer(A),
    is_integer(B),
    is_integer(C),
    is_integer(D),
    A >= 0,
    A =< 255,
    B >= 0,
    B =< 255,
    C >= 0,
    C =< 255,
    D >= 0,
    D =< 255
->
    Addr;
parse_bind_addr(Addr) when is_tuple(Addr) ->
    bind_addr_fallback(Addr, invalid_ip4_tuple);
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
