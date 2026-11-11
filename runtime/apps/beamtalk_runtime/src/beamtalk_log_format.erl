%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_log_format).

%%% **DDD Context:** Runtime Context — shared leaf

-moduledoc """
Shared log-event formatting helpers used by multiple OTP logger handlers.

Extracted to avoid duplicating identical logic between
`beamtalk_json_formatter` (Runtime) and `beamtalk_ws_log_handler` (Workspace).
""".

-export([format_time/1, format_pid/1]).

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
