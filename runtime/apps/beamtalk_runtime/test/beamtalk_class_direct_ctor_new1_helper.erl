%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_direct_ctor_new1_helper).

%%% **DDD Context:** Runtime — Object System

-moduledoc """
Test helper module for BT-4409: an ancestor class whose compiled module
defines a user class-side `new:` (`'class_new:'/2`), so a descendant's
`new:` sent through the class process reaches it.

Kept apart from `beamtalk_class_direct_ctor_helper`, whose missing
`'class_new:'/2` export is what lets its classes build `new:` with the
synthesized `new/1`.
""".

-export([
    new/0,
    new/1,
    'class_new:'/2
]).

-doc "Nullary constructor answering the builder's pid.".
-spec new() -> pid().
new() ->
    self().

-doc "Map constructor answering the builder's pid and the initialiser.".
-spec new(map()) -> {pid(), map()}.
new(InitMap) when is_map(InitMap) ->
    {self(), InitMap}.

-doc "The user class-side `new:`: tags its answer so a test can tell it ran.".
-spec 'class_new:'(term(), term()) -> {ancestor_new1, pid(), term()}.
'class_new:'(_ClassSelf, Init) ->
    {ancestor_new1, self(), Init}.
