%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Actor System Context

-module(beamtalk_bt3826_raising_fixture).
-moduledoc """
Fake supervisor class module whose `strategy` method raises
`{class_not_registered, _}` from its own body and counts its invocations in the
`counters` ref stored under `persistent_term` key `bt3826_raising_calls`. Backs
the BT-3826 test that a failure inside a config method is never mistaken for
"no in-process definition" and re-run through `class_send`.
""".

-export([class_strategy/1, class_maxRestarts/1, class_restartWindow/1]).

class_strategy(_ClassSelf) ->
    counters:add(persistent_term:get(bt3826_raising_calls), 1, 1),
    error({class_not_registered, from_method_body}).

class_maxRestarts(_ClassSelf) -> 3.
class_restartWindow(_ClassSelf) -> 9.
