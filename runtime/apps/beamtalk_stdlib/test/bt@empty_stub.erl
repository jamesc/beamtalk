%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Minimal stub with NO test methods.
%% Named 'bt@empty_stub' so resolve_module('EmptyStub') finds it.
%% Used by BT-2404 to exercise the no-test-methods branch of run_all/1
%% (lines 221–226 of beamtalk_test_case.erl).
-module('bt@empty_stub').

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([new/0, dispatch/3]).

new() ->
    #{'$beamtalk_class' => 'EmptyStub'}.

dispatch(setUp, [], Instance) ->
    Instance;
dispatch(tearDown, [], _Instance) ->
    nil.
