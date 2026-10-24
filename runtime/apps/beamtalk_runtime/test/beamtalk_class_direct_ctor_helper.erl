%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_direct_ctor_helper).

%%% **DDD Context:** Runtime — Object System

-moduledoc """
Test helper module for direct-construction tests.

Provides `new/0`, `new/1` and a two-slot keyword constructor, each answering
the builder process's pid (plus its arguments), so the tests can tell a
caller-side build (`self()` is the test process) from a class-process build
(`self()` is the class gen_server) — the same trick
`beamtalk_class_dispatch_test_helper:class_testSelf/1` uses for direct calls.

Deliberately NOT extending that helper: it must keep having no `new/0` /
`new/1` exports, so the "missing export falls through" tests have a module
to point at.
""".

-export([
    new/0,
    new/1,
    'class_x:y:'/3
]).

-doc "Nullary constructor answering the builder's pid.".
-spec new() -> pid().
new() ->
    self().

-doc "Map constructor answering the builder's pid and the initialiser.".
-spec new(map()) -> {pid(), map()}.
new(InitMap) when is_map(InitMap) ->
    {self(), InitMap}.

-doc "Two-slot keyword constructor answering the builder's pid and the slots.".
-spec 'class_x:y:'(term(), term(), term()) -> {pid(), term(), term()}.
'class_x:y:'(_ClassSelf, X, Y) ->
    {self(), X, Y}.
