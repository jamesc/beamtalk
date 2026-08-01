%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(test_new_no_init).

%% Minimal value-type fixture used by beamtalk_class_instantiation_tests.
%%
%% Exports new/0 but intentionally NOT new/1 — exercises the
%% `handle_new_compiled` branch that falls back to new/0 when a caller
%% passes a Map arg but the module has no new/1 (line 363 of
%% beamtalk_class_instantiation.erl).
-export([new/0]).

new() ->
    #{'$beamtalk_class' => 'TestNewNoInit'}.
