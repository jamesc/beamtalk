%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Time class implementation — current time accessors.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Time provides class-side methods for reading the system clock
%%% as an integer Unix-epoch timestamp.
%%%
%%% ## Methods
%%%
%%% | Selector  | Description                              |
%%% |-----------|------------------------------------------|
%%% | `nowS`    | Current time in seconds                  |
%%% | `nowMs`   | Current time in milliseconds             |
%%% | `nowUs`   | Current time in microseconds             |

-module(beamtalk_time).

-export([nowS/0, nowMs/0, nowUs/0]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Current time in seconds since the Unix epoch.
-spec nowS() -> integer().
nowS() ->
    erlang:system_time(second).

%% @doc Current time in milliseconds since the Unix epoch.
-spec nowMs() -> integer().
nowMs() ->
    erlang:system_time(millisecond).

%% @doc Current time in microseconds since the Unix epoch.
-spec nowUs() -> integer().
nowUs() ->
    erlang:system_time(microsecond).
