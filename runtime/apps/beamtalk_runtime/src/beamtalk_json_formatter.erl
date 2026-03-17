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
    {MsgBin, ReportFields} = format_msg_structured(Msg, Meta),
    Fields0 = [
        {<<"time">>, TimeBin},
        {<<"level">>, atom_to_binary(Level, utf8)},
        {<<"msg">>, MsgBin}
    ],
    Fields1 = maybe_add(<<"domain">>, format_domain(Msg, Meta), Fields0),
    Fields2 = maybe_add(<<"class">>, maps:get(beamtalk_class, Meta, undefined), Fields1),
    Fields3 = maybe_add(<<"selector">>, maps:get(beamtalk_selector, Meta, undefined), Fields2),
    Fields4 = maybe_add(<<"mfa">>, format_mfa(Msg, Meta), Fields3),
    Fields5 = maybe_add(<<"pid">>, format_pid(Meta), Fields4),
    %% Append structured fields extracted from OTP reports
    Fields6 = lists:foldl(fun({K, V}, Acc) -> maybe_add(K, V, Acc) end, Fields5, ReportFields),
    Json = jsx:encode(lists:reverse(Fields6)),
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

%% @doc Format the log message and extract structured fields from OTP reports.
%%
%% Returns {MessageBinary, ExtraFields} where ExtraFields is a list of
%% {Key, Value} pairs extracted from structured OTP reports.
-spec format_msg_structured(term(), map()) -> {binary(), [{binary(), term()}]}.
format_msg_structured({string, Msg}, _Meta) ->
    {iolist_to_binary(Msg), []};
format_msg_structured({report, Report}, Meta) when is_map(Report) ->
    %% Try to decompose structured OTP reports into fields
    case extract_report_fields(Report, Meta) of
        {MsgBin, Fields} ->
            {MsgBin, Fields};
        undefined ->
            {format_report_fallback(Report, Meta), []}
    end;
format_msg_structured({report, Report}, Meta) ->
    {format_report_fallback(Report, Meta), []};
format_msg_structured({Format, Args}, _Meta) ->
    {iolist_to_binary(io_lib:format(Format, Args)), []}.

%% @doc Extract structured fields from OTP report maps.
%%
%% Recognises gen_server terminate, supervisor, and proc_lib crash reports.
-spec extract_report_fields(map(), map()) ->
    {binary(), [{binary(), term()}]} | undefined.
extract_report_fields(#{label := {gen_server, terminate}, name := Name, reason := Reason}, _Meta) ->
    Msg = iolist_to_binary(io_lib:format("gen_server ~tp terminated", [Name])),
    Fields = [
        {<<"report_type">>, <<"gen_server_terminate">>},
        {<<"reason">>, iolist_to_binary(io_lib:format("~tp", [Reason]))}
    ],
    {Msg, Fields};
extract_report_fields(
    #{label := {supervisor, progress}, supervisor := Sup, started := Started}, _Meta
) ->
    ChildId =
        case Started of
            L when is_list(L) -> proplists:get_value(id, L, unknown);
            _ -> unknown
        end,
    Msg = iolist_to_binary(io_lib:format("supervisor ~tp started child ~tp", [Sup, ChildId])),
    Fields = [
        {<<"report_type">>, <<"supervisor_progress">>}
    ],
    {Msg, Fields};
extract_report_fields(
    #{label := {supervisor, child_terminated}, supervisor := Sup, reason := Reason}, _Meta
) ->
    Msg = iolist_to_binary(io_lib:format("supervisor ~tp child terminated: ~tp", [Sup, Reason])),
    Fields = [
        {<<"report_type">>, <<"supervisor_child_terminated">>},
        {<<"reason">>, iolist_to_binary(io_lib:format("~tp", [Reason]))}
    ],
    {Msg, Fields};
extract_report_fields(#{label := {proc_lib, crash}, report := CrashInfo}, _Meta) when
    is_list(CrashInfo)
->
    Reason = proplists:get_value(error_info, CrashInfo, unknown),
    InitCall = proplists:get_value(initial_call, CrashInfo, unknown),
    Msg = iolist_to_binary(io_lib:format("process crash: ~tp", [Reason])),
    Fields = [
        {<<"report_type">>, <<"proc_lib_crash">>},
        {<<"initial_call">>, iolist_to_binary(io_lib:format("~tp", [InitCall]))}
    ],
    {Msg, Fields};
extract_report_fields(_Report, _Meta) ->
    undefined.

%% @doc Format a report using the report callback or fallback.
-spec format_report_fallback(term(), map()) -> binary().
format_report_fallback(Report, Meta) ->
    case maps:get(report_cb, Meta, undefined) of
        undefined ->
            iolist_to_binary(io_lib:format("~tp", [Report]));
        Fun when is_function(Fun, 1) ->
            {Format, Args} = Fun(Report),
            iolist_to_binary(io_lib:format(Format, Args));
        Fun when is_function(Fun, 2) ->
            iolist_to_binary(Fun(Report, #{single_line => true, depth => unlimited}))
    end.

%% @doc Format the domain metadata as a dot-separated binary.
%%
%% Uses explicit domain from metadata if present. Otherwise, infers domain
%% from OTP report labels (gen_server, supervisor, proc_lib → "otp").
-spec format_domain(term(), map()) -> binary() | undefined.
format_domain(_Msg, #{domain := Domain}) when is_list(Domain) ->
    iolist_to_binary(
        lists:join($., [atom_to_list(D) || D <- Domain])
    );
format_domain({report, Report}, _Meta) when is_map(Report) ->
    infer_domain_from_report(Report);
format_domain(_Msg, _Meta) ->
    undefined.

%% @doc Infer domain from OTP report labels.
-spec infer_domain_from_report(map()) -> binary() | undefined.
infer_domain_from_report(#{label := {gen_server, _}}) -> <<"otp">>;
infer_domain_from_report(#{label := {supervisor, _}}) -> <<"otp">>;
infer_domain_from_report(#{label := {proc_lib, _}}) -> <<"otp">>;
infer_domain_from_report(#{label := {application_controller, _}}) -> <<"otp">>;
infer_domain_from_report(_) -> undefined.

%% @doc Format the MFA metadata as a binary.
%%
%% Uses explicit mfa from metadata if present. For OTP reports, extracts
%% mfa from the report's initial_call or registered name when available.
-spec format_mfa(term(), map()) -> binary() | undefined.
format_mfa(_Msg, #{mfa := {M, F, A}}) ->
    iolist_to_binary(io_lib:format("~s:~s/~B", [M, F, A]));
format_mfa({report, Report}, _Meta) when is_map(Report) ->
    infer_mfa_from_report(Report);
format_mfa(_Msg, _Meta) ->
    undefined.

%% @doc Extract MFA from OTP report fields.
-spec infer_mfa_from_report(map()) -> binary() | undefined.
infer_mfa_from_report(#{label := {gen_server, terminate}, name := Name}) when is_atom(Name) ->
    iolist_to_binary(io_lib:format("~s", [Name]));
infer_mfa_from_report(#{label := {supervisor, _}, supervisor := {_, Name}}) when is_atom(Name) ->
    iolist_to_binary(io_lib:format("~s", [Name]));
infer_mfa_from_report(#{label := {supervisor, _}, supervisor := Name}) when is_atom(Name) ->
    iolist_to_binary(io_lib:format("~s", [Name]));
infer_mfa_from_report(_) ->
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
