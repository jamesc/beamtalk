%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc JSON log formatter for the OTP logger framework.
%%%
%%% **DDD Context:** Runtime Context
%%%
%%% Implements the `logger_formatter` behaviour to produce structured JSON
%%% log lines. Used when the operator switches log format to `json` via
%%% `beamtalk_logging_config:logFormat/1`.
%%%
%%% Output format (one JSON object per line):
%%% ```json
%%% {"time":"2026-03-17T14:23:01.234Z","level":"info","msg":"Actor initialized",
%%%  "domain":"runtime","class":"Counter","selector":"increment"}
%%% ```
%%%
%%% Uses the `jsx` library (already a runtime dependency) for JSON encoding.

-module(beamtalk_json_formatter).

-export([format/2, check_config/1]).

%% @doc Format a log event as a single-line JSON object followed by a newline.
-spec format(logger:log_event(), logger:formatter_config()) -> unicode:chardata().
format(#{level := Level, msg := Msg, meta := Meta}, _Config) ->
    TimeBin = format_time(Meta),
    MsgBin = format_msg(Msg, Meta),
    Fields0 = [
        {<<"time">>, TimeBin},
        {<<"level">>, atom_to_binary(Level, utf8)},
        {<<"msg">>, MsgBin}
    ],
    Fields1 = maybe_add(<<"domain">>, format_domain(Meta), Fields0),
    Fields2 = maybe_add(<<"class">>, maps:get(beamtalk_class, Meta, undefined), Fields1),
    Fields3 = maybe_add(<<"selector">>, maps:get(beamtalk_selector, Meta, undefined), Fields2),
    Fields4 = maybe_add(<<"mfa">>, format_mfa(Meta), Fields3),
    Fields5 = maybe_add(<<"pid">>, format_pid(Meta), Fields4),
    Json = jsx:encode(lists:reverse(Fields5)),
    [Json, $\n].

%% @doc Validate formatter configuration. Accepts any config.
-spec check_config(logger:formatter_config()) -> ok | {error, term()}.
check_config(_Config) ->
    ok.

%%====================================================================
%% Internal helpers
%%====================================================================

%% @doc Format the timestamp from metadata as an ISO 8601 UTC string.
-spec format_time(map()) -> binary().
format_time(#{time := Timestamp}) ->
    %% Timestamp is in microseconds since epoch
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

%% @doc Format the log message into a binary string.
-spec format_msg(term(), map()) -> binary().
format_msg({string, Msg}, _Meta) ->
    iolist_to_binary(Msg);
format_msg({report, Report}, Meta) ->
    case maps:get(report_cb, Meta, undefined) of
        undefined ->
            iolist_to_binary(io_lib:format("~tp", [Report]));
        Fun when is_function(Fun, 1) ->
            {Format, Args} = Fun(Report),
            iolist_to_binary(io_lib:format(Format, Args));
        Fun when is_function(Fun, 2) ->
            iolist_to_binary(Fun(Report, #{single_line => true, depth => unlimited}))
    end;
format_msg({Format, Args}, _Meta) ->
    iolist_to_binary(io_lib:format(Format, Args)).

%% @doc Format the domain metadata as a dot-separated binary.
-spec format_domain(map()) -> binary() | undefined.
format_domain(#{domain := Domain}) when is_list(Domain) ->
    iolist_to_binary(
        lists:join($., [atom_to_list(D) || D <- Domain])
    );
format_domain(_) ->
    undefined.

%% @doc Format the MFA metadata as a binary.
-spec format_mfa(map()) -> binary() | undefined.
format_mfa(#{mfa := {M, F, A}}) ->
    iolist_to_binary(io_lib:format("~s:~s/~B", [M, F, A]));
format_mfa(_) ->
    undefined.

%% @doc Format the PID metadata as a binary.
-spec format_pid(map()) -> binary() | undefined.
format_pid(#{pid := Pid}) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid));
format_pid(_) ->
    undefined.

%% @doc Conditionally prepend a key-value pair if the value is not undefined.
-spec maybe_add(binary(), term(), [{binary(), term()}]) -> [{binary(), term()}].
maybe_add(_Key, undefined, Acc) ->
    Acc;
maybe_add(Key, Value, Acc) when is_atom(Value) ->
    [{Key, atom_to_binary(Value, utf8)} | Acc];
maybe_add(Key, Value, Acc) ->
    [{Key, Value} | Acc].
