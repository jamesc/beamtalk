%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_actor_scan_tests_sup).

%%% **DDD Context:** Actor System Context

-moduledoc "Plain OTP supervisor with one `test_counter` child, for `beamtalk_actor_scan_tests`.".

-behaviour(supervisor).

-export([init/1]).

init([]) ->
    {ok,
        {#{strategy => one_for_one, intensity => 0, period => 1}, [
            #{id => counter, start => {test_counter, start_link, [0]}}
        ]}}.
