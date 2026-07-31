%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Helper for setUp-failure paths in beamtalk_test_case_stdlib_tests.
%% setUp raises a generic Erlang error — exercises the outer catch-all
%% in run_test_method/5 (lines 868–871 of beamtalk_test_case.erl).
-module(tc_setup_fail_helper).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([new/0, dispatch/3]).

new() ->
    #{'$beamtalk_class' => 'FakeTest'}.

dispatch(setUp, [], _Instance) ->
    error(setup_went_wrong);
dispatch(testPass, [], _Instance) ->
    nil;
dispatch(tearDown, [], _Instance) ->
    nil.
