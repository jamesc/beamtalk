%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_json_formatter).

%%% **DDD Context:** Runtime Context

-moduledoc """
JSON log formatter for the OTP logger framework.

Implements the `logger_formatter` behaviour to produce structured JSON
log lines. Used when the operator switches log format to `json` via
`beamtalk_logging_config:logFormat/1`.

Output format (one JSON object per line):
```json
{"time":"2026-03-17T14:23:01.234Z","level":"info","msg":"Actor initialized",
 "domain":"runtime","class":"Counter","selector":"increment"}
```

Uses the OTP `json` module (OTP 27+) for JSON encoding.
""".

-export([format/2, check_config/1]).

-doc "Format a log event as a single-line JSON object followed by a newline.".
-spec format(logger:log_event(), logger:formatter_config()) -> unicode:chardata().
format(#{level := Level, msg := Msg, meta := Meta}, _Config) ->
    try
        format_json(Level, Msg, Meta)
    catch
        Class:Reason:Stack ->
            %% A logger formatter must never crash — fall back to plain text.
            io_lib:format(
                "~ts ~ts: ~tp [formatter error: ~p:~p in ~tp]~n",
                [
                    format_time(Meta),
                    Level,
                    format_msg_fallback(Msg),
                    Class,
                    Reason,
                    hd(Stack)
                ]
            )
    end.

-doc """
Build the JSON log line. Separated from format/2 so the try-catch
wrapper stays small.
""".
-spec format_json(atom(), term(), map()) -> unicode:chardata().
format_json(Level, Msg, Meta) ->
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
    %% Append extra metadata fields (reason, stacktrace, etc.) not already handled
    Fields7 = append_extra_meta(Meta, Fields6),
    Json = iolist_to_binary(json:encode(maps:from_list(lists:reverse(Fields7)))),
    [Json, $\n].

-doc "Validate formatter configuration. Accepts any config.".
-spec check_config(logger:formatter_config()) -> ok | {error, term()}.
check_config(_Config) ->
    ok.

%%====================================================================
%% Internal helpers
%%====================================================================

-doc "Format the timestamp from metadata as an ISO 8601 UTC string.".
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

-doc "Best-effort message extraction for the crash fallback path.".
-spec format_msg_fallback(term()) -> unicode:chardata().
format_msg_fallback({string, Msg}) ->
    Msg;
format_msg_fallback({report, Report}) ->
    io_lib:format("~tp", [Report]);
format_msg_fallback({Format, Args}) when is_list(Format), is_list(Args) ->
    try
        io_lib:format(Format, Args)
    catch
        _:_ -> io_lib:format("~tp", [{Format, Args}])
    end;
format_msg_fallback(Other) ->
    io_lib:format("~tp", [Other]).

-doc """
Format the log message and extract structured fields from OTP reports.

Returns {MessageBinary, ExtraFields} where ExtraFields is a list of
{Key, Value} pairs extracted from structured OTP reports.
""".
-spec format_msg_structured(term(), map()) -> {binary(), [{binary(), term()}]}.
format_msg_structured({string, Msg}, _Meta) ->
    {safe_to_binary(Msg), []};
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
    {safe_to_binary(io_lib:format(Format, Args)), []}.

-doc """
Extract structured fields from OTP report maps.

Recognises gen_server terminate, supervisor, and proc_lib crash reports.
""".
-spec extract_report_fields(map(), map()) ->
    {binary(), [{binary(), term()}]} | undefined.
extract_report_fields(#{label := {gen_server, terminate}, name := Name, reason := Reason}, _Meta) ->
    Msg = format_reason_with_bt_error(
        io_lib:format("gen_server ~tp terminated", [Name]), Reason
    ),
    Fields = [
        {<<"report_type">>, <<"gen_server_terminate">>},
        {<<"name">>, safe_to_binary(io_lib:format("~tp", [Name]))},
        {<<"reason">>, safe_to_binary(io_lib:format("~tp", [Reason]))}
    ],
    {Msg, Fields};
extract_report_fields(
    #{label := {supervisor, progress}, report := Report}, _Meta
) when is_list(Report) ->
    %% OTP supervisor format: data in proplist under `report` key
    Sup = proplists:get_value(supervisor, Report, unknown),
    Started = proplists:get_value(started, Report, []),
    ChildId =
        case Started of
            L when is_list(L) -> proplists:get_value(id, L, unknown);
            _ -> unknown
        end,
    Msg = safe_to_binary(io_lib:format("Supervisor: ~tp. Started: id=~tp.", [Sup, ChildId])),
    Fields = [
        {<<"report_type">>, <<"supervisor_progress">>},
        {<<"name">>, safe_to_binary(io_lib:format("~tp", [Sup]))}
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
    Msg = safe_to_binary(io_lib:format("Supervisor: ~tp. Started: id=~tp.", [Sup, ChildId])),
    Fields = [
        {<<"report_type">>, <<"supervisor_progress">>},
        {<<"name">>, safe_to_binary(io_lib:format("~tp", [Sup]))}
    ],
    {Msg, Fields};
extract_report_fields(
    #{label := {supervisor, child_terminated}, report := Report}, _Meta
) when is_list(Report) ->
    %% OTP supervisor format: data is in a proplist under `report` key
    Sup = proplists:get_value(supervisor, Report, unknown),
    Reason = proplists:get_value(reason, Report, unknown),
    Offender = proplists:get_value(offender, Report, []),
    ChildId =
        case Offender of
            L when is_list(L) -> proplists:get_value(id, L, unknown);
            _ -> unknown
        end,
    Msg = format_reason_with_bt_error(
        io_lib:format("Supervisor: ~tp child ~tp terminated", [Sup, ChildId]), Reason
    ),
    Fields = [
        {<<"report_type">>, <<"supervisor_child_terminated">>},
        {<<"name">>, safe_to_binary(io_lib:format("~tp", [Sup]))},
        {<<"reason">>, format_reason_concise(Reason)}
    ],
    {Msg, Fields};
extract_report_fields(
    #{
        label := {supervisor, child_terminated},
        supervisor := Sup,
        reason := Reason,
        offender := Offender
    },
    _Meta
) ->
    %% Fallback: top-level keys (used by tests / older OTP)
    ChildId =
        case Offender of
            L when is_list(L) -> proplists:get_value(id, L, unknown);
            _ -> unknown
        end,
    Msg = format_reason_with_bt_error(
        io_lib:format("Supervisor: ~tp child ~tp terminated", [Sup, ChildId]), Reason
    ),
    Fields = [
        {<<"report_type">>, <<"supervisor_child_terminated">>},
        {<<"name">>, safe_to_binary(io_lib:format("~tp", [Sup]))},
        {<<"reason">>, format_reason_concise(Reason)}
    ],
    {Msg, Fields};
extract_report_fields(
    #{label := {supervisor, child_terminated}, supervisor := Sup, reason := Reason}, _Meta
) ->
    Msg = format_reason_with_bt_error(
        io_lib:format("Supervisor: ~tp child terminated", [Sup]), Reason
    ),
    Fields = [
        {<<"report_type">>, <<"supervisor_child_terminated">>},
        {<<"name">>, safe_to_binary(io_lib:format("~tp", [Sup]))},
        {<<"reason">>, format_reason_concise(Reason)}
    ],
    {Msg, Fields};
extract_report_fields(#{label := {proc_lib, crash}, report := CrashInfo}, _Meta) when
    is_list(CrashInfo)
->
    Reason = proplists:get_value(error_info, CrashInfo, unknown),
    InitCall = proplists:get_value(initial_call, CrashInfo, unknown),
    Msg = safe_to_binary(io_lib:format("process crash: ~tp", [Reason])),
    Fields = [
        {<<"report_type">>, <<"proc_lib_crash">>},
        {<<"initial_call">>, safe_to_binary(io_lib:format("~tp", [InitCall]))}
    ],
    {Msg, Fields};
extract_report_fields(_Report, _Meta) ->
    undefined.

-doc """
Build a human-readable message, appending any beamtalk error found in
the OTP crash reason. Falls back to the raw reason if no beamtalk error.
""".
-spec format_reason_with_bt_error(iolist(), term()) -> binary().
format_reason_with_bt_error(Prefix, Reason) ->
    try
        case beamtalk_error:extract_beamtalk_error(Reason, 4) of
            undefined ->
                safe_to_binary(io_lib:format("~ts: ~tp", [Prefix, Reason]));
            BtError ->
                BtMsg = beamtalk_error:format(BtError),
                safe_to_binary(io_lib:format("~ts: ~ts", [Prefix, BtMsg]))
        end
    catch
        _:_ -> safe_to_binary(io_lib:format("~ts: ~tp", [Prefix, Reason]))
    end.

-doc """
Format a crash reason concisely for the "reason" JSON field.
Extracts beamtalk error message if present, otherwise uses ~tp.
""".
-spec format_reason_concise(term()) -> binary().
format_reason_concise(Reason) ->
    try
        case beamtalk_error:extract_beamtalk_error(Reason, 4) of
            undefined ->
                safe_to_binary(io_lib:format("~tp", [Reason]));
            BtError ->
                beamtalk_error:format(BtError)
        end
    catch
        _:_ -> safe_to_binary(io_lib:format("~tp", [Reason]))
    end.

-doc "Format a report using the report callback or fallback.".
-spec format_report_fallback(term(), map()) -> binary().
format_report_fallback(Report, Meta) ->
    case maps:get(report_cb, Meta, undefined) of
        undefined ->
            safe_to_binary(io_lib:format("~tp", [Report]));
        Fun when is_function(Fun, 1) ->
            {Format, Args} = Fun(Report),
            safe_to_binary(io_lib:format(Format, Args));
        Fun when is_function(Fun, 2) ->
            safe_to_binary(Fun(Report, #{single_line => true, depth => unlimited}))
    end.

-doc """
Format the domain metadata as a dot-separated binary.

Uses explicit domain from metadata if present. Otherwise, infers domain
from OTP report labels (gen_server, supervisor, proc_lib → "otp").
""".
-spec format_domain(term(), map()) -> binary() | undefined.
format_domain(_Msg, #{domain := Domain}) when is_list(Domain) ->
    iolist_to_binary(
        lists:join($., [atom_to_list(D) || D <- Domain])
    );
format_domain({report, Report}, _Meta) when is_map(Report) ->
    infer_domain_from_report(Report);
format_domain(_Msg, _Meta) ->
    undefined.

-doc "Infer domain from OTP report labels.".
-spec infer_domain_from_report(map()) -> binary() | undefined.
infer_domain_from_report(#{label := {gen_server, _}}) -> <<"otp">>;
infer_domain_from_report(#{label := {supervisor, _}}) -> <<"otp">>;
infer_domain_from_report(#{label := {proc_lib, _}}) -> <<"otp">>;
infer_domain_from_report(#{label := {application_controller, _}}) -> <<"otp">>;
infer_domain_from_report(_) -> undefined.

-doc """
Format the MFA metadata as a binary.

Uses explicit mfa from log event metadata. Does not infer mfa from OTP
report fields — registered names belong in the "name" field, not "mfa".
""".
-spec format_mfa(term(), map()) -> binary() | undefined.
format_mfa(_Msg, #{mfa := {M, F, A}}) ->
    iolist_to_binary(io_lib:format("~s:~s/~B", [M, F, A]));
format_mfa(_Msg, _Meta) ->
    undefined.

-doc "Format the PID metadata as a binary.".
-spec format_pid(map()) -> binary() | undefined.
format_pid(#{pid := Pid}) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid));
format_pid(_) ->
    undefined.

-doc """
Append user-supplied metadata fields that aren't handled by the core formatter.

OTP standard keys (time, pid, gl, file, line, mfa, domain, report_cb, etc.)
are excluded. Everything else is formatted as `~tp` and included.
""".
-spec append_extra_meta(map(), [{binary(), term()}]) -> [{binary(), term()}].
append_extra_meta(Meta, Fields) ->
    Dominated = [
        time,
        pid,
        gl,
        file,
        line,
        mfa,
        domain,
        report_cb,
        error_logger,
        logger_formatter,
        beamtalk_class,
        beamtalk_selector
    ],
    Extra = maps:without(Dominated, Meta),
    maps:fold(
        fun(Key, Value, Acc) ->
            BinKey = key_to_binary(Key),
            maybe_add(BinKey, format_extra_value(Key, Value), Acc)
        end,
        Fields,
        Extra
    ).

-doc """
Format an extra metadata value for JSON output.

Stacktraces get special formatting: Erlang stacktrace lists are converted
to human-readable frames via `beamtalk_stack_frame`. Non-list stacktraces
(e.g., tuples from Core Erlang catch) are scanned for beamtalk errors.
""".
-spec format_extra_value(atom() | binary(), term()) -> binary().
format_extra_value(stacktrace, Stack) when is_list(Stack) ->
    %% Standard Erlang stacktrace — convert to human-readable frames
    try
        Frames = beamtalk_stack_frame:wrap(Stack),
        Lines = [format_stack_frame(F) || F <- Frames],
        safe_to_binary(lists:join($\n, Lines))
    catch
        _:_ ->
            %% Fallback if wrap fails
            Formatted = lists:join($\n, [io_lib:format("  ~tp", [Frame]) || Frame <- Stack]),
            safe_to_binary(Formatted)
    end;
format_extra_value(stacktrace, Stack) ->
    %% Non-list stacktrace (e.g., tuple from Core Erlang catch).
    %% Try to extract a beamtalk error for a useful message, then append raw term.
    try
        case beamtalk_error:extract_beamtalk_error(Stack, 4) of
            undefined ->
                safe_to_binary(io_lib:format("~tp", [Stack]));
            BtError ->
                ErrorMsg = beamtalk_error:format(BtError),
                safe_to_binary(io_lib:format("~ts\n  raw: ~tp", [ErrorMsg, Stack]))
        end
    catch
        _:_ -> safe_to_binary(io_lib:format("~tp", [Stack]))
    end;
format_extra_value(_Key, Value) when is_binary(Value) ->
    Value;
format_extra_value(_Key, Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
format_extra_value(_Key, Value) when is_integer(Value) ->
    integer_to_binary(Value);
format_extra_value(_Key, Value) when is_pid(Value) ->
    list_to_binary(pid_to_list(Value));
format_extra_value(_Key, Value) ->
    safe_to_binary(io_lib:format("~tp", [Value])).

-doc "Format a single StackFrame tagged map as a concise string.".
-spec format_stack_frame(map()) -> iolist().
format_stack_frame(#{class_name := ClassName, function := Function, arity := Arity} = Frame) ->
    ClassPart =
        case ClassName of
            nil -> <<"?">>;
            _ -> atom_to_binary(ClassName, utf8)
        end,
    FunPart =
        case Function of
            undefined -> <<"?">>;
            _ -> atom_to_binary(Function, utf8)
        end,
    Location =
        case Frame of
            #{file := File, line := Line} when File =/= nil, Line =/= nil ->
                io_lib:format(" (~ts:~B)", [File, Line]);
            _ ->
                ""
        end,
    io_lib:format("  ~ts>>~ts/~B~ts", [ClassPart, FunPart, Arity, Location]).

-doc """
Convert a metadata key to a binary.  Keys are usually atoms, but
user-supplied metadata (e.g. from Beamtalk Dictionary) may use binaries.
""".
-spec key_to_binary(atom() | binary()) -> binary().
key_to_binary(Key) when is_atom(Key) -> atom_to_binary(Key, utf8);
key_to_binary(Key) when is_binary(Key) -> Key;
key_to_binary(Key) -> iolist_to_binary(io_lib:format("~tp", [Key])).

-doc "Convert unicode chardata to binary, falling back to latin1 on error.".
-spec safe_to_binary(unicode:chardata()) -> binary().
safe_to_binary(Chardata) ->
    case unicode:characters_to_binary(Chardata) of
        Bin when is_binary(Bin) -> Bin;
        {error, Encoded, _} -> Encoded;
        {incomplete, Encoded, _} -> Encoded
    end.

-doc "Conditionally prepend a key-value pair if the value is not undefined.".
-spec maybe_add(binary(), term(), [{binary(), term()}]) -> [{binary(), term()}].
maybe_add(_Key, undefined, Acc) ->
    Acc;
maybe_add(Key, Value, Acc) when is_atom(Value) ->
    [{Key, atom_to_binary(Value, utf8)} | Acc];
maybe_add(Key, Value, Acc) ->
    [{Key, Value} | Acc].
