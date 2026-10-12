%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Minimal stub that mimics a compiled Beamtalk class module.
%%
%% Named 'bt@tc_stub' so that beamtalk_test_case:resolve_module('TcStub')
%% finds this module via the bt@<snake_case> naming convention.
%% Used by tests of run_all/1, run_single/2, run_all_structured/1,
%% and run_single_structured/2 (the BIF-fallback paths).
-module('bt@tc_stub').

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% Compiled-Beamtalk module contract
-export([new/0, dispatch/3]).

%% Test methods are not exported: since BT-3786 tests come from the class
%% hierarchy (beamtalk_test_case:class_test_plan/1), never from module exports,
%% and this stub is not a registered class. run_single/2 and
%% run_single_structured/2 still run a named method through dispatch/3.

new() ->
    #{'$beamtalk_class' => 'TcStub'}.

dispatch(testStubPass, [], _Instance) ->
    nil;
dispatch(testStubFail, [], _Instance) ->
    error(intentional_stub_failure);
dispatch(setUp, [], Instance) ->
    Instance;
dispatch(tearDown, [], _Instance) ->
    nil.
