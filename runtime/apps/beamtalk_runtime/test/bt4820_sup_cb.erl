%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(bt4820_sup_cb).
-moduledoc "Supervisor callback for beamtalk_crash_report_filter_tests (BT-4820).".
-behaviour(supervisor).
-export([init/1]).

init([]) ->
    {ok,
        {#{strategy => one_for_one, intensity => 10, period => 1}, [
            #{
                id => crasher,
                start => {'bt@bt4820pkg@bt4820_crasher', start_link, []},
                restart => temporary
            }
        ]}}.
