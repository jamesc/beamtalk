%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_threaded_wire_test_helper).

%%% **DDD Context:** Object System Context

-moduledoc """
Hand-written class module for the ADR 0132 Phase 0 wire check (BT-3868).

Stands in for what ADR 0132 Phase 1c/1d codegen will emit, before any codegen
exists: one body over `Acc` (`'__t2_class_twice:'/3`, `'__t2_twice:'/3`) with
the plain entry as a wrapper over it, and a `'__beamtalk_meta'/0` advertising
the entries in `tier2_class_methods` / `tier2_methods`. `ghost:` is listed in
both maps but its entry function is deliberately **not** exported, so the
`erlang:function_exported/3` guard is exercised.

`beamtalk_threaded_dispatch_tests` registers classes over this module.
""".

-export([
    '__beamtalk_meta'/0,
    'class_twice:'/2,
    '__t2_class_twice:'/3,
    'class_ghost:'/2,
    'twice:'/2,
    '__t2_twice:'/3,
    'value:'/2
]).

-doc "The metadata the wire check registers: `twice:` threads arg 0 on both sides.".
'__beamtalk_meta'() ->
    #{
        %% ADR 0130 §3: the load gate refuses a module's meta without it.
        class_var_abi => 1,
        direct_class_methods => #{'twice:' => 'class_twice:', 'ghost:' => 'class_ghost:'},
        tier2_methods => #{
            'twice:' => {'__t2_twice:', [0]},
            'ghost:' => {'__t2_ghost:', [0]}
        },
        tier2_class_methods => #{
            'twice:' => {'__t2_class_twice:', [0]},
            'ghost:' => {'__t2_class_ghost:', [0]}
        }
    }.

-doc "The plain class-side entry: today's name and arity, over the threaded body.".
'class_twice:'(ClassSelf, Block) ->
    element(1, '__t2_class_twice:'(ClassSelf, Block, #{})).

-doc "The class-side threaded body: invokes `Block` twice through the tier gate.".
'__t2_class_twice:'(_ClassSelf, Block, Acc0) ->
    {_V1, Acc1} = beamtalk_dispatch:call_threaded(Block, [], Acc0),
    beamtalk_dispatch:call_threaded(Block, [], Acc1).

-doc "A direct-callable class method whose advertised entry is not exported.".
'class_ghost:'(_ClassSelf, Block) ->
    Block.

-doc "The plain instance-side entry, over the threaded body.".
'twice:'(Self, Block) ->
    element(1, '__t2_twice:'(Self, Block, #{})).

-doc "The instance-side threaded body.".
'__t2_twice:'(_Self, Block, Acc0) ->
    {_V1, Acc1} = beamtalk_dispatch:call_threaded(Block, [], Acc0),
    beamtalk_dispatch:call_threaded(Block, [], Acc1).

-doc "Makes an instance a non-fun callable (an object answering `value:`).".
'value:'(_Self, X) ->
    {called_with, X}.
