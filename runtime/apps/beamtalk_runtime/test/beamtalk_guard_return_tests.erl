%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_guard_return_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
BT-4415: `beamtalk_guard_return`. A Boolean answers itself; the non-Boolean
path (a receiver that calls or ignores the block) is covered end to end by
`stdlib/test/guard_return_test.bt`.
""".

-include_lib("eunit/include/eunit.hrl").

booleans_answer_themselves_test() ->
    ?assert(beamtalk_guard_return:if_true(true)),
    ?assertNot(beamtalk_guard_return:if_true(false)),
    ?assert(beamtalk_guard_return:if_false(true)),
    ?assertNot(beamtalk_guard_return:if_false(false)).
