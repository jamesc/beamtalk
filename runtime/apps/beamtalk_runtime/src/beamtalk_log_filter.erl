%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Logger filters for suppressing redundant OTP crash reports.
%%%
%%% **DDD Context:** Runtime Context
%%%
%%% Beamtalk actors log errors via the generated `handle_continue`,
%%% `handle_cast`, and `handle_info` callbacks with structured fields
%%% (class, selector, stacktrace). OTP also emits its own crash reports
%%% (gen_server terminate, proc_lib crash, supervisor child_terminated)
%%% for the same event. The proc_lib crash report adds no useful
%%% information ("process crash: unknown"), so we filter it out.

-module(beamtalk_log_filter).

-export([filter_proc_lib_crash/2]).

%% @doc Drop proc_lib crash reports — they say "process crash: unknown"
%% and add nothing beyond the gen_server terminate and supervisor reports.
-spec filter_proc_lib_crash(logger:log_event(), term()) -> logger:filter_return().
filter_proc_lib_crash(#{msg := {report, #{label := {proc_lib, crash}}}}, _Extra) ->
    stop;
filter_proc_lib_crash(_Event, _Extra) ->
    ignore.
