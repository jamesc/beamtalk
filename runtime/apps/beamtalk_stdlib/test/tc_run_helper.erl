%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Minimal fake compiled-Beamtalk module used by beamtalk_test_case_stdlib_tests.
%% Provides the new/0 + dispatch/3 contract that run_test_method/5 expects.
-module(tc_run_helper).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([new/0, dispatch/3]).

new() ->
    #{'$beamtalk_class' => 'FakeTest'}.

dispatch(testPass, [], _Instance) ->
    nil;
dispatch(testError, [], _Instance) ->
    error(raw_error_reason);
dispatch(testThrow, [], _Instance) ->
    throw(thrown_reason);
dispatch(testExit, [], _Instance) ->
    exit(exit_reason);
dispatch(testSkip, [], _Instance) ->
    throw({bunit_skip, <<"skip reason">>});
dispatch(testAssertFail, [], _Instance) ->
    error(#beamtalk_error{kind = assertion_failed, class = 'TestCase', message = <<"assert failed">>});
dispatch(setUp, [], Instance) ->
    Instance;
dispatch(tearDown, [], _Instance) ->
    nil.
