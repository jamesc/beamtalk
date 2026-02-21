%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Idle monitor for automatic workspace cleanup
%%%
%%% **DDD Context:** Workspace
%%%
%%% Monitors workspace activity and initiates graceful shutdown if the
%%% workspace has been idle for too long. This prevents abandoned BEAM
%%% nodes from accumulating indefinitely.
%%%
%%% Activity is tracked via beamtalk_workspace_meta:update_activity/0,
%%% which should be called by:
%%% - REPL sessions (on connection)
%%% - Actors (on spawn or message handling)
%%% - Code reloading
%%%
%%% ## Cleanup Criteria (from ADR 0004)
%%%
%%% A workspace is considered active if:
%%% - Any REPL session is connected
%%% - Actor message sent/received in last N seconds
%%% - Code hot-reloaded recently
%%%
%%% If idle for more than max_idle_seconds, the node self-terminates
%%% via init:stop/0.
%%%
%%% ## Production Nodes
%%%
%%% Nodes started with --persistent flag disable auto-cleanup by setting
%%% enabled: false in the config.

-module(beamtalk_idle_monitor).
-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").

%% Public API
-export([start_link/1, mark_activity/0]).

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

%% @doc Start the idle monitor.
%% Config: #{enabled => boolean(), max_idle_seconds => integer()}
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Mark activity (convenience wrapper for workspace_meta).
%% This is a convenience function that components can call to indicate activity.
-spec mark_activity() -> ok.
mark_activity() ->
    beamtalk_workspace_meta:update_activity().

%%% gen_server callbacks

%% @private
init(Config) ->
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

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(check_idle, State = #state{enabled = false}) ->
    %% Auto-cleanup disabled, don't schedule next check
    {noreply, State};
handle_info(check_idle, State = #state{enabled = true, max_idle_seconds = MaxIdle}) ->
    %% Check if we should terminate
    case should_terminate(MaxIdle) of
        true ->
            ?LOG_WARNING("Workspace idle, shutting down", #{max_idle_seconds => MaxIdle}),
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

%% @private
terminate(_Reason, #state{timer_ref = TRef}) ->
    %% Cancel timer if set
    case TRef of
        undefined -> ok;
        _ -> erlang:cancel_timer(TRef)
    end,
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal Functions

%% @doc Determine if the workspace should terminate due to inactivity.
-spec should_terminate(integer()) -> boolean().
should_terminate(MaxIdleSeconds) ->
    %% Check if there are any active sessions
    HasSessions = has_active_sessions(),

    %% Check idle time
    case beamtalk_workspace_meta:get_last_activity() of
        {ok, LastActivity} ->
            Now = erlang:system_time(second),
            IdleTime = Now - LastActivity,

            %% Terminate if no sessions and idle too long
            (not HasSessions) andalso (IdleTime > MaxIdleSeconds);
        {error, _} ->
            %% Can't determine activity, don't terminate
            false
    end.

%% @doc Check if there are any active REPL sessions connected.
-spec has_active_sessions() -> boolean().
has_active_sessions() ->
    %% Check if beamtalk_session_sup exists and has children
    try
        case whereis(beamtalk_session_sup) of
            undefined ->
                false;
            SupPid ->
                Children = supervisor:count_children(SupPid),
                Active = proplists:get_value(active, Children, 0),
                Active > 0
        end
    catch
        _:_ ->
            %% If we can't check, assume there are sessions (safe default)
            true
    end.
