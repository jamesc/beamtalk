%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_log_format).

%%% **DDD Context:** Runtime Context — shared leaf

-moduledoc """
Shared log-event formatting helpers used by multiple OTP logger handlers.

Extracted to avoid duplicating identical logic between
`beamtalk_json_formatter` (Runtime), `beamtalk_ws_log_handler` (Workspace) and
`beamtalk_repl_ops_watch` (Workspace).
""".

-export([
    format_time/1,
    format_pid/1,
    format_domain/1,
    format_mfa/1,
    format_mfa_or/2,
    report_chardata/2
]).

-doc "Format the timestamp from OTP logger metadata as an ISO 8601 UTC binary.".
-spec format_time(map()) -> binary().
format_time(#{time := Timestamp}) ->
    Micros = Timestamp rem 1000000,
    Millis = Micros div 1000,
    Seconds = Timestamp div 1000000,
    {{Y, Mo, D}, {H, Mi, S}} = calendar:system_time_to_universal_time(Seconds, second),
    beamtalk_error:chars_to_binary(
        io_lib:format(
            "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
            [Y, Mo, D, H, Mi, S, Millis]
        )
    );
format_time(_) ->
    <<"unknown">>.

-doc "Format the pid from OTP logger metadata as a binary, or undefined.".
-spec format_pid(map()) -> binary() | undefined.
format_pid(#{pid := Pid}) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid));
format_pid(_) ->
    undefined.

-doc "Format explicit `domain` metadata as a dot-separated binary, or undefined.".
-spec format_domain(map()) -> binary() | undefined.
format_domain(#{domain := Domain}) when is_list(Domain) ->
    iolist_to_binary(lists:join($., [atom_to_list(D) || D <- Domain]));
format_domain(_) ->
    undefined.

-doc """
Format explicit `mfa` log metadata as `module:function/arity`, or undefined.
Does not infer an MFA from report fields.
""".
-spec format_mfa(map()) -> binary() | undefined.
format_mfa(#{mfa := Mfa}) ->
    format_mfa_or(Mfa, undefined);
format_mfa(_) ->
    undefined.

-doc "Render a bare `{M, F, A}` as `module:function/arity`, or `Default` for anything else.".
-spec format_mfa_or(term(), Default) -> binary() | Default.
format_mfa_or({M, F, A} = Mfa, _Default) when is_atom(M), is_atom(F), is_integer(A) ->
    beamtalk_stack_frame:raw_mfa(Mfa);
format_mfa_or(_, Default) ->
    Default.

-doc """
Render a logger report to chardata using the event's `report_cb` (arity 1 or
2), falling back to `~tp` of the report when none is set. Callers convert the
chardata to a binary with their own policy.
""".
-spec report_chardata(term(), map()) -> unicode:chardata().
report_chardata(Report, Meta) ->
    case maps:get(report_cb, Meta, undefined) of
        undefined ->
            io_lib:format("~tp", [Report]);
        Fun when is_function(Fun, 1) ->
            {Format, Args} = Fun(Report),
            io_lib:format(Format, Args);
        Fun when is_function(Fun, 2) ->
            Fun(Report, #{single_line => true, depth => unlimited})
    end.
