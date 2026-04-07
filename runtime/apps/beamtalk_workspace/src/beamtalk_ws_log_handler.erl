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

The handler is registered with OTP logger during workspace startup
(`beamtalk_workspace_sup`) and removed on shutdown.

Follows the same subscribe/unsubscribe pattern as
`beamtalk_repl_actors`, `beamtalk_class_events`, etc.

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

-include_lib("kernel/include/logger.hrl").

%% Public API
-export([subscribe/0, subscribe/1, unsubscribe/0]).

%% OTP logger handler callbacks
-export([log/2, adding_handler/1, removing_handler/1]).

%% Handler ID used with logger:add_handler/3
-define(HANDLER_ID, beamtalk_ws_log).

%% ETS table for subscriber tracking
-define(SUB_TABLE, beamtalk_ws_log_subscribers).

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

-doc "Subscribe the calling process to log events at or above `Level`.".
-spec subscribe(logger:level()) -> ok.
subscribe(Level) when is_atom(Level) ->
    ensure_table(),
    Pid = self(),
    %% Remove any existing subscription (allows level update).
    ets:delete(?SUB_TABLE, Pid),
    %% Insert the subscription entry, then spawn a monitor process.
    %% The monitor must be established *after* insert so that if the
    %% subscriber dies between insert and monitor, the DOWN fires and
    %% cleans up the entry. If the subscriber dies *before* insert,
    %% `erlang:monitor` on a dead pid delivers DOWN immediately,
    %% cleaning up the entry we just inserted.
    ets:insert(?SUB_TABLE, {Pid, Level}),
    spawn(fun() -> monitor_subscriber(Pid) end),
    ok.

-doc "Unsubscribe the calling process from log events.".
-spec unsubscribe() -> ok.
unsubscribe() ->
    case ets:info(?SUB_TABLE) of
        undefined -> ok;
        _ -> ets:delete(?SUB_TABLE, self())
    end,
    ok.

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
                        fun({Pid, SubLevel}) ->
                            SubLevelVal = ?LEVEL_VALUE(SubLevel),
                            case LevelVal =< SubLevelVal of
                                true ->
                                    Pid ! {log_event, Formatted};
                                false ->
                                    ok
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

-doc "Monitor a subscriber and clean up its ETS entry when it dies.".
-spec monitor_subscriber(pid()) -> ok.
monitor_subscriber(Pid) ->
    MonRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MonRef, process, Pid, _Reason} ->
            case ets:info(?SUB_TABLE) of
                undefined -> ok;
                _ -> ets:delete(?SUB_TABLE, Pid)
            end,
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
    M1 = maybe_put(domain, format_domain(Meta), Base),
    M2 = maybe_put(class, format_class(Meta), M1),
    M3 = maybe_put(selector, format_selector(Meta), M2),
    M4 = maybe_put(mfa, format_mfa(Meta), M3),
    maybe_put(pid, format_pid(Meta), M4).

-spec format_time(map()) -> binary().
format_time(#{time := Timestamp}) ->
    Micros = Timestamp rem 1000000,
    Millis = Micros div 1000,
    Seconds = Timestamp div 1000000,
    {{Y, Mo, D}, {H, Mi, S}} = calendar:system_time_to_universal_time(Seconds, second),
    iolist_to_binary(
        io_lib:format(
            "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
            [Y, Mo, D, H, Mi, S, Millis]
        )
    );
format_time(_) ->
    <<"unknown">>.

-spec format_msg(term(), map()) -> binary().
format_msg({string, Msg}, _Meta) ->
    iolist_to_binary(Msg);
format_msg({report, Report}, Meta) when is_map(Report) ->
    case maps:get(report_cb, Meta, undefined) of
        undefined ->
            iolist_to_binary(io_lib:format("~tp", [Report]));
        Fun when is_function(Fun, 1) ->
            {Format, Args} = Fun(Report),
            iolist_to_binary(io_lib:format(Format, Args));
        Fun when is_function(Fun, 2) ->
            iolist_to_binary(Fun(Report, #{single_line => true, depth => unlimited}))
    end;
format_msg({report, Report}, Meta) ->
    format_msg_report_list(Report, Meta);
format_msg({Format, Args}, _Meta) ->
    iolist_to_binary(io_lib:format(Format, Args)).

-spec format_msg_report_list(term(), map()) -> binary().
format_msg_report_list(Report, Meta) ->
    case maps:get(report_cb, Meta, undefined) of
        undefined ->
            iolist_to_binary(io_lib:format("~tp", [Report]));
        Fun when is_function(Fun, 1) ->
            {Format, Args} = Fun(Report),
            iolist_to_binary(io_lib:format(Format, Args));
        Fun when is_function(Fun, 2) ->
            iolist_to_binary(Fun(Report, #{single_line => true, depth => unlimited}))
    end.

-spec format_domain(map()) -> binary() | undefined.
format_domain(#{domain := Domain}) when is_list(Domain) ->
    iolist_to_binary(lists:join($., [atom_to_list(D) || D <- Domain]));
format_domain(_) ->
    undefined.

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

-spec format_mfa(map()) -> binary() | undefined.
format_mfa(#{mfa := {M, F, A}}) when is_atom(M), is_atom(F), is_integer(A) ->
    iolist_to_binary(
        io_lib:format("~s:~s/~B", [atom_to_list(M), atom_to_list(F), A])
    );
format_mfa(_) ->
    undefined.

-spec format_pid(map()) -> binary() | undefined.
format_pid(#{pid := Pid}) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid));
format_pid(_) ->
    undefined.

-spec maybe_put(atom(), term(), map()) -> map().
maybe_put(_Key, undefined, Map) ->
    Map;
maybe_put(Key, Value, Map) ->
    maps:put(Key, Value, Map).
