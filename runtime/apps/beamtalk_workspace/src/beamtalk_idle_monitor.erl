%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_idle_monitor).
-behaviour(gen_server).

%%% **DDD Context:** Workspace Context

-moduledoc """
Idle monitor for automatic workspace cleanup

Monitors workspace activity and initiates graceful shutdown if the
workspace has been idle for too long. This prevents abandoned BEAM
nodes from accumulating indefinitely.

Activity is tracked via beamtalk_workspace_meta:update_activity/0,
which should be called by:
- REPL sessions (on connection)
- Actors (on spawn or message handling)
- Code reloading

## Cleanup Criteria (from ADR 0004)

A workspace is considered active if:
- Any REPL session is connected (a session whose client disconnected and has not
  resumed does not count; it is stopped by the shell after its own idle period)
- Flushable edits are still pending in the ChangeLog
- Actor message sent/received in last N seconds
- Code hot-reloaded recently

If idle for more than max_idle_seconds, the node self-terminates
via init:stop/0.

## Production Nodes

Nodes started with --persistent flag disable auto-cleanup by setting
enabled: false in the config.
""".
-include_lib("kernel/include/logger.hrl").

%% Public API
-export([start_link/1, mark_activity/0]).

%% Exported for white-box tests only.
-ifdef(TEST).
-export([should_terminate/1, has_active_sessions/0]).
-endif.

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% Check every 10 minutes
-define(CHECK_INTERVAL, 600000).

-record(state, {
    enabled :: boolean(),
    max_idle_seconds :: integer(),
    timer_ref :: reference() | undefined
}).

%%% Public API

-doc """
Start the idle monitor.
Config: #{enabled => boolean(), max_idle_seconds => integer()}
""".
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

-doc """
Mark activity (convenience wrapper for workspace_meta).
This is a convenience function that components can call to indicate activity.
""".
-spec mark_activity() -> ok.
mark_activity() ->
    beamtalk_workspace_meta:update_activity().

%%% gen_server callbacks

init(Config) ->
    beamtalk_logging_config:set_domain(runtime),
    Enabled = maps:get(enabled, Config, true),
    MaxIdleSeconds = maps:get(max_idle_seconds, Config, 3600 * 4),

    State = #state{
        enabled = Enabled,
        max_idle_seconds = MaxIdleSeconds,
        timer_ref = undefined
    },

    %% Start periodic checks if enabled
    NewState =
        case Enabled of
            true ->
                TRef = erlang:send_after(?CHECK_INTERVAL, self(), check_idle),
                State#state{timer_ref = TRef};
            false ->
                State
        end,

    {ok, NewState}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_idle, State = #state{enabled = false}) ->
    %% Auto-cleanup disabled, don't schedule next check
    {noreply, State};
handle_info(check_idle, State = #state{enabled = true, max_idle_seconds = MaxIdle}) ->
    %% Check if we should terminate
    case should_terminate(MaxIdle) of
        true ->
            ?LOG_WARNING("Workspace idle, shutting down", #{
                max_idle_seconds => MaxIdle, domain => [beamtalk, runtime]
            }),
            %% Graceful shutdown - init:stop() is async, stop this gen_server too
            init:stop(),
            {stop, normal, State};
        false ->
            %% Schedule next check
            TRef = erlang:send_after(?CHECK_INTERVAL, self(), check_idle),
            {noreply, State#state{timer_ref = TRef}}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{timer_ref = TRef}) ->
    %% Cancel timer if set
    case TRef of
        undefined -> ok;
        _ -> erlang:cancel_timer(TRef)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal Functions

-doc """
Determine if the workspace should terminate due to inactivity.

Auto-shutdown may fire only when ALL of these hold:
- no session is attached to a client, running an eval, or never-attached (a
  *detached* session — a client connected and left — does not count);
- no flushable edit is pending in the ChangeLog (`Workspace flush` would write it);
- the last activity is older than `MaxIdleSeconds`.
""".
-spec should_terminate(integer()) -> boolean().
should_terminate(MaxIdleSeconds) ->
    %% Check idle time first: cheap, and avoids polling sessions while active.
    case beamtalk_workspace_meta:get_last_activity() of
        {ok, LastActivity} ->
            Now = erlang:system_time(second),
            IdleTime = Now - LastActivity,
            (IdleTime > MaxIdleSeconds) andalso
                (not has_active_sessions()) andalso
                (not has_unflushed_edits());
        {error, _} ->
            %% Can't determine activity, don't terminate
            false
    end.

-doc "Whether the ChangeLog holds edits that `Workspace flush` has not written yet.".
-spec has_unflushed_edits() -> boolean().
has_unflushed_edits() ->
    try
        beamtalk_workspace_changelog:flushable_pending() =/= []
    catch
        %% If we can't check, assume there are (safe default: don't shut down).
        _:_ -> true
    end.

-doc """
Check if any REPL session keeps the workspace alive: one with a client
attached, an eval still running, or one that never had a client (explicitly
stopped by its owner). Sessions whose client has gone (detached) do not count.
""".
-spec has_active_sessions() -> boolean().
has_active_sessions() ->
    try
        case whereis(beamtalk_session_sup) of
            undefined ->
                false;
            SupPid ->
                Pids = [P || {_, P, _, _} <- supervisor:which_children(SupPid), is_pid(P)],
                lists:any(fun session_keeps_workspace_alive/1, Pids)
        end
    catch
        _:_ ->
            %% If we can't check, assume there are sessions (safe default)
            true
    end.

-spec session_keeps_workspace_alive(pid()) -> boolean().
session_keeps_workspace_alive(Pid) ->
    try
        beamtalk_repl_shell:lifecycle_state(Pid, 2000) =/= detached
    catch
        %% A session that just exited is not active; a slow one is assumed active.
        exit:{noproc, _} -> false;
        exit:{normal, _} -> false;
        _:_ -> true
    end.
