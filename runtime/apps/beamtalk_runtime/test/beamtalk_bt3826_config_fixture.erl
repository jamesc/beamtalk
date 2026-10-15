%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Actor System Context

-module(beamtalk_bt3826_config_fixture).
-moduledoc """
Fake supervisor class module answering the three nullary configuration class
methods `beamtalk_process_navigation` reads (`strategy`, `maxRestarts`,
`restartWindow`). Backs the BT-3826 no-cross-class-deadlock regression test in
`beamtalk_class_registry_tests`.
""".

-export([class_strategy/1, class_maxRestarts/1, class_restartWindow/1]).

class_strategy(_ClassSelf) -> oneForAll.
class_maxRestarts(_ClassSelf) -> 7.
class_restartWindow(_ClassSelf) -> 42.
