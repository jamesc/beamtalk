%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_direct_ctor_new0_helper).

%%% **DDD Context:** Runtime — Object System

-moduledoc """
Test helper module for BT-4479: an ancestor class whose compiled module
defines a user class-side `new` (`'class_new'/1`), so a descendant that
defines none reaches it when sent `new`.

Kept apart from `beamtalk_class_direct_ctor_helper`, whose missing
`'class_new'/1` export is what lets its classes build `new` with the
synthesized `new/0`.
""".

-export([
    new/0,
    'class_new'/1
]).

-doc "Nullary constructor answering the builder's pid.".
-spec new() -> pid().
new() ->
    self().

-doc "The user class-side `new`: tags its answer so a test can tell it ran.".
-spec 'class_new'(term()) -> {ancestor_new0, pid()}.
'class_new'(_ClassSelf) ->
    {ancestor_new0, self()}.
