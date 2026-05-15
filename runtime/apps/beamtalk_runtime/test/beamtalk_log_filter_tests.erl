%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_log_filter_tests).

-moduledoc """
EUnit tests for beamtalk_log_filter module.

Tests both clauses of filter_proc_lib_crash/2:
  - proc_lib crash reports are dropped (stop)
  - all other log events are passed through (ignore)
""".
-include_lib("eunit/include/eunit.hrl").

filter_proc_lib_crash_stops_proc_lib_crash_reports_test() ->
    Event = #{msg => {report, #{label => {proc_lib, crash}}}},
    ?assertEqual(stop, beamtalk_log_filter:filter_proc_lib_crash(Event, undefined)).

filter_proc_lib_crash_ignores_other_events_test() ->
    Event = #{msg => {report, #{label => {supervisor, child_terminated}}}},
    ?assertEqual(ignore, beamtalk_log_filter:filter_proc_lib_crash(Event, undefined)).
