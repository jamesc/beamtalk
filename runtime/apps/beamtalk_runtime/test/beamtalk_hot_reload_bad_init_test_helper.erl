%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_hot_reload_bad_init_test_helper).

-moduledoc """
Test helper module whose init/1 returns a shape that is neither
`{ok, map()}` nor an exception — for beamtalk_hot_reload_tests.erl's
"unexpected init/1 return is logged, not silently swallowed" case (BT-3532).

Used only by beamtalk_hot_reload_tests.erl.
""".

-export([init/1]).

-doc "Always returns an error tuple, regardless of Args.".
-spec init(term()) -> {error, not_a_state_map}.
init(_Args) ->
    {error, not_a_state_map}.
