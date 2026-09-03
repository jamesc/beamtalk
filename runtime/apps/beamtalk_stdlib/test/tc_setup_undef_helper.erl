%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Helper for setUp-failure path: setUp triggers error:undef.
%% Exercises the outer catch clause at lines 857-863 of beamtalk_test_case.erl:
%%   error:undef:SetupST -> beamtalk_exception_handler:ensure_wrapped(...) -> {fail, ...}
-module(tc_setup_undef_helper).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([new/0, dispatch/3]).

new() ->
    #{'$beamtalk_class' => 'FakeTest'}.

dispatch(setUp, [], _Instance) ->
    nonexistent_module_nightly_coverage_xyz:'call'();
dispatch(testPass, [], _Instance) ->
    nil;
dispatch(tearDown, [], _Instance) ->
    nil.
