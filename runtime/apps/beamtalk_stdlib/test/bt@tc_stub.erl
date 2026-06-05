%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Minimal stub that mimics a compiled Beamtalk class module.
%%
%% Named 'bt@tc_stub' so that beamtalk_test_case:resolve_module('TcStub')
%% finds this module via the bt@<snake_case> naming convention.
%% Used by BT-2404 tests of run_all/1, run_single/2, run_all_structured/1,
%% and run_single_structured/2 (the BIF-fallback paths).
-module('bt@tc_stub').

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% Compiled-Beamtalk module contract
-export([new/0, dispatch/3]).

%% Exported test methods — discovered by discover_test_methods_from_module/1
%% which scans module_info(exports) for names beginning with "test".
%% Arity is ignored by the discovery logic; actual execution goes through dispatch/3.
-export([testStubPass/1, testStubFail/1]).

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

%% Stub implementations (never called — dispatch/3 is used instead).
testStubPass(_) -> nil.
testStubFail(_) -> error(intentional_stub_failure).
