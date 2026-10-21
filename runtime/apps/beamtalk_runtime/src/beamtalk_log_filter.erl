%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_log_filter).

%%% **DDD Context:** Runtime Context

-moduledoc """
Logger filters for suppressing redundant OTP crash reports.

A Beamtalk actor that crashes (a failed `initialize`, or an error in a
`!` cast, `handleInfo:` or a continuation — ADR 0134) is reported by
OTP's own crash reports (gen_server terminate, proc_lib crash,
supervisor child_terminated), the gen_server one carrying the last
message and the wrapped Exception with its stacktrace. The proc_lib crash report adds no useful
information ("process crash: unknown"), so we filter it out.
""".

-export([filter_proc_lib_crash/2]).

-doc """
Drop proc_lib crash reports — they say "process crash: unknown"
and add nothing beyond the gen_server terminate and supervisor reports.
""".
-spec filter_proc_lib_crash(logger:log_event(), term()) -> logger:filter_return().
filter_proc_lib_crash(#{msg := {report, #{label := {proc_lib, crash}}}}, _Extra) ->
    stop;
filter_proc_lib_crash(_Event, _Extra) ->
    ignore.
