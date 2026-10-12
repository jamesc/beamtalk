%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Actor System Context

-module(beamtalk_supervisor_writing_fixture).
-moduledoc """
Fake supervisor class module whose class-side hooks write a class variable
(BT-3708). `beamtalk_supervisor` runs `class children` and `class initialize:`
against a read-only class-state snapshot, so a write from either must raise
`class_state_read_only`.

Replaces the `bt3708_*` process-dictionary mode flags the main test module
used to toggle: each hook here always writes. The remaining supervisor class
methods delegate to `beamtalk_supervisor_tests` so the fake hierarchy is
defined once.

Used only by beamtalk_supervisor_tests.erl.
""".

-export([
    class_children/1,
    class_strategy/1,
    class_maxRestarts/1,
    class_restartWindow/1,
    'class_initialize:'/2
]).

class_children(ClassSelf) ->
    beamtalk_class_vars:put(ClassSelf, n, 1),
    [].

class_strategy(ClassSelf) -> beamtalk_supervisor_tests:class_strategy(ClassSelf).
class_maxRestarts(ClassSelf) -> beamtalk_supervisor_tests:class_maxRestarts(ClassSelf).
class_restartWindow(ClassSelf) -> beamtalk_supervisor_tests:class_restartWindow(ClassSelf).

'class_initialize:'(ClassSelf, _SupTuple) ->
    beamtalk_class_vars:put(ClassSelf, n, 1),
    nil.
