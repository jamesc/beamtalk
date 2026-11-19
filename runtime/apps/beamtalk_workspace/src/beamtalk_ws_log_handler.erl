%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_ws_log_handler).

%%% **DDD Context:** Workspace Context

-moduledoc """
Custom OTP logger handler that forwards log events to subscribed
WebSocket sessions.

Each WebSocket handler process (cowboy_websocket) can subscribe to
receive log events by calling `subscribe/0` or `subscribe/1` with a
minimum log level. Events below the subscriber's level filter are
silently dropped. Subscribers are automatically cleaned up when the
process terminates (via monitor).

Each subscriber has exactly one monitor process, started on its first
`subscribe/1` and kept across level changes (a re-subscribe only updates the
level). `unsubscribe/0` stops it, so level changes never accumulate processes.

## Backpressure

A subscriber is a WebSocket handler, and its mailbox is the only buffer between
the node's log volume and a possibly slow socket. `log/2` therefore skips any
subscriber whose mailbox already holds `queue_limit/0` messages: the event is
dropped for that subscriber only (others still get it) and its drop counter,
readable via `dropped_count/1`, is incremented. Dropping is silent on the wire;
the client keeps receiving the newest events once it catches up.

The handler is registered with OTP logger during workspace startup
(`beamtalk_workspace_sup`) and removed on shutdown.

Follows a self-monitoring subscribe/unsubscribe pattern: a separate diagnostic
log stream, distinct from the workspace push-stream facade
(`beamtalk_repl_subscriptions`).

## Log Event Format

Subscribers receive `{log_event, Map}` messages where Map contains:
```erlang
#{
  level => atom(),
  time => binary(),           % ISO 8601 UTC
  msg => binary(),
  domain => binary() | undefined,
  class => binary() | undefined,
  selector => binary() | undefined,
  mfa => binary() | undefined,
  pid => binary() | undefined
}
```
""".

%% Public API
-export([subscribe/0, subscribe/1, unsubscribe/0, queue_limit/0, dropped_count/1]).

%% OTP logger handler callbacks
-export([log/2, adding_handler/1, removing_handler/1]).

%% ETS table for subscriber tracking
-define(SUB_TABLE, beamtalk_ws_log_subscribers).

%% Subscriber mailbox length at which further log events are dropped for it.
-define(QUEUE_LIMIT, 1000).

%% OTP log level ordering (lower number = more severe)
-define(LEVEL_VALUE(L),
    case L of
        emergency -> 0;
        alert -> 1;
        critical -> 2;
        error -> 3;
        warning -> 4;
        notice -> 5;
        info -> 6;
        debug -> 7
    end
).

%%====================================================================
%% Public API
%%====================================================================

-doc "Subscribe the calling process to all log events (debug and above).".
-spec subscribe() -> ok.
subscribe() ->
    subscribe(debug).

-doc """
Subscribe the calling process to log events at or above `Level`.

Calling it again updates the level of the existing subscription; the
subscriber's monitor process is reused, not replaced.
""".
-spec subscribe(logger:level()) -> ok.
subscribe(Level) when is_atom(Level) ->
    ensure_table(),
    Pid = self(),
    case ets:lookup(?SUB_TABLE, Pid) of
        [{Pid, _OldLevel, _MonPid, _Dropped}] ->
            %% Update only the level: rewriting the row could overwrite a drop
            %% counted by `deliver/2` in the meantime.
            ets:update_element(?SUB_TABLE, Pid, {2, Level}),
            ok;
        [] ->
            %% Insert the subscription entry, then release the monitor process,
            %% which only starts watching `Pid` once told the row exists. If the
            %% subscriber dies before that, `erlang:monitor` on a dead pid
            %% delivers DOWN immediately and the row is cleaned up all the same.
            MonPid = spawn(fun() -> monitor_subscriber(Pid) end),
            ets:insert(?SUB_TABLE, {Pid, Level, MonPid, 0}),
            MonPid ! registered
    end,
    ok.

-doc "Unsubscribe the calling process from log events and stop its monitor.".
-spec unsubscribe() -> ok.
unsubscribe() ->
    case ets:info(?SUB_TABLE) of
        undefined ->
            ok;
        _ ->
            case ets:take(?SUB_TABLE, self()) of
                [{_Pid, _Level, MonPid, _Dropped}] -> MonPid ! stop;
                [] -> ok
            end
    end,
    ok.

-doc "Mailbox length at which log events start being dropped for a subscriber.".
-spec queue_limit() -> pos_integer().
queue_limit() ->
    ?QUEUE_LIMIT.

-doc """
Number of log events dropped for `Pid` because its mailbox was at
`queue_limit/0`. Zero for a process that is not subscribed.
""".
-spec dropped_count(pid()) -> non_neg_integer().
dropped_count(Pid) ->
    case ets:info(?SUB_TABLE) of
        undefined ->
            0;
        _ ->
            case ets:lookup(?SUB_TABLE, Pid) of
                [{_, _, _, Dropped}] -> Dropped;
                [] -> 0
            end
    end.

%%====================================================================
%% OTP logger handler callbacks
%%====================================================================

-doc """
Called by OTP logger when this handler is added.
Creates the ETS table for subscriber tracking.
""".
-spec adding_handler(logger:handler_config()) -> {ok, logger:handler_config()}.
adding_handler(Config) ->
    ensure_table(),
    {ok, Config}.

-doc """
Called by OTP logger when this handler is removed.
Deletes the ETS table.
""".
-spec removing_handler(logger:handler_config()) -> ok.
removing_handler(_Config) ->
    case ets:info(?SUB_TABLE) of
        undefined -> ok;
        _ -> ets:delete(?SUB_TABLE)
    end,
    ok.

-doc """
Called by OTP logger for each log event.
Formats the event and sends it to all subscribers whose level filter passes.
""".
-spec log(logger:log_event(), logger:handler_config()) -> ok.
log(#{level := Level} = LogEvent, _Config) ->
    case ets:info(?SUB_TABLE) of
        undefined ->
            ok;
        _ ->
            LevelVal = ?LEVEL_VALUE(Level),
            Subscribers = ets:tab2list(?SUB_TABLE),
            case Subscribers of
                [] ->
                    ok;
                _ ->
                    %% Format once, send to all matching subscribers.
                    %% Wrap in try/catch — a crash in log/2 causes OTP to
                    %% remove the handler entirely, silently breaking streaming.
                    Formatted =
                        try
                            format_event(LogEvent)
                        catch
                            _:_ ->
                                #{level => atom_to_binary(Level, utf8), msg => <<"[format error]">>}
                        end,
                    lists:foreach(
                        fun({Pid, SubLevel, _MonPid, _Dropped}) ->
                            SubLevelVal = ?LEVEL_VALUE(SubLevel),
                            case LevelVal =< SubLevelVal of
                                true -> deliver(Pid, Formatted);
                                false -> ok
                            end
                        end,
                        Subscribers
                    )
            end
    end,
    ok.

%%====================================================================
%% Internal helpers
%%====================================================================

-doc """
Send a log event to a subscriber unless its mailbox is at the limit, in which
case the event is dropped and counted. A dead subscriber is skipped (its
monitor process removes the row).
""".
-spec deliver(pid(), map()) -> ok.
deliver(Pid, Formatted) ->
    case process_info(Pid, message_queue_len) of
        {message_queue_len, Len} when Len >= ?QUEUE_LIMIT ->
            %% The row may have been removed since `log/2` read it.
            try
                ets:update_counter(?SUB_TABLE, Pid, {4, 1})
            catch
                error:badarg -> ok
            end,
            ok;
        {message_queue_len, _} ->
            Pid ! {log_event, Formatted},
            ok;
        undefined ->
            ok
    end.

-doc """
Monitor a subscriber and clean up its ETS entry when it dies. Started by
`subscribe/1`; ends on the subscriber's death or when `unsubscribe/0` sends
`stop`.
""".
-spec monitor_subscriber(pid()) -> ok.
monitor_subscriber(Pid) ->
    receive
        registered -> ok;
        stop -> exit(normal)
    end,
    MonRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MonRef, process, Pid, _Reason} ->
            %% Delete only a row this process owns: after unsubscribe/subscribe
            %% the row belongs to a newer monitor.
            case ets:info(?SUB_TABLE) of
                undefined -> ok;
                _ -> ets:match_delete(?SUB_TABLE, {Pid, '_', self(), '_'})
            end,
            ok;
        stop ->
            erlang:demonitor(MonRef, [flush]),
            ok
    end.

-doc "Ensure the subscriber ETS table exists.".
-spec ensure_table() -> ok.
ensure_table() ->
    case ets:info(?SUB_TABLE) of
        undefined ->
            try
                ets:new(?SUB_TABLE, [
                    named_table,
                    public,
                    set,
                    {read_concurrency, true}
                ]),
                ok
            catch
                error:badarg ->
                    %% Race: another process created it between our check and new
                    ok
            end;
        _ ->
            ok
    end.

-doc "Format a log event into a map suitable for JSON encoding.".
-spec format_event(logger:log_event()) -> map().
format_event(#{level := Level, msg := Msg, meta := Meta}) ->
    Base = #{
        level => atom_to_binary(Level, utf8),
        time => format_time(Meta),
        msg => format_msg(Msg, Meta)
    },
    M1 = maybe_put(domain, beamtalk_log_format:format_domain(Meta), Base),
    M2 = maybe_put(class, format_class(Meta), M1),
    M3 = maybe_put(selector, format_selector(Meta), M2),
    M4 = maybe_put(mfa, beamtalk_log_format:format_mfa(Meta), M3),
    maybe_put(pid, format_pid(Meta), M4).

-spec format_time(map()) -> binary().
format_time(Meta) ->
    beamtalk_log_format:format_time(Meta).

-spec format_msg(term(), map()) -> binary().
format_msg({string, Msg}, _Meta) ->
    log_text_to_binary(Msg);
format_msg({report, Report}, Meta) ->
    log_text_to_binary(beamtalk_log_format:report_chardata(Report, Meta));
format_msg({Format, Args}, _Meta) ->
    log_text_to_binary(io_lib:format(Format, Args)).

%% Callers format log text with either `~s` on a UTF-8 binary (bytes surface as
%% latin1 chars; `iolist_to_binary/1` passes them back through unchanged) or
%% `~ts` (real codepoints, possibly above 255, where `iolist_to_binary/1` raises
%% `badarg`; and a crashing `log/2` gets this handler removed by OTP logger).
%% Keep the byte pass-through when it is valid UTF-8, else encode the codepoints.
-spec log_text_to_binary(unicode:chardata()) -> binary().
log_text_to_binary(Chardata) ->
    try iolist_to_binary(Chardata) of
        Bin ->
            case unicode:characters_to_binary(Bin, utf8, utf8) of
                Valid when is_binary(Valid) -> Bin;
                _ -> beamtalk_error:chars_to_binary(Chardata)
            end
    catch
        error:badarg -> beamtalk_error:chars_to_binary(Chardata)
    end.

-spec format_class(map()) -> binary() | undefined.
format_class(#{beamtalk_class := Class}) when is_atom(Class) ->
    atom_to_binary(Class, utf8);
format_class(_) ->
    undefined.

-spec format_selector(map()) -> binary() | undefined.
format_selector(#{beamtalk_selector := Sel}) when is_atom(Sel) ->
    atom_to_binary(Sel, utf8);
format_selector(_) ->
    undefined.

-spec format_pid(map()) -> binary() | undefined.
format_pid(Meta) ->
    beamtalk_log_format:format_pid(Meta).

-spec maybe_put(atom(), term(), map()) -> map().
maybe_put(_Key, undefined, Map) ->
    Map;
maybe_put(Key, Value, Map) ->
    Map#{Key => Value}.
