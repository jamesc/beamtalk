%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Helper for setUp-skip path in beamtalk_test_case_stdlib_tests.
%% setUp throws {bunit_skip, ...} — exercises the throw:{bunit_skip, ...}
%% outer catch clause in run_test_method/5 (line 853 of beamtalk_test_case.erl).
-module(tc_setup_skip_helper).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([new/0, dispatch/3]).

new() ->
    #{'$beamtalk_class' => 'FakeTest'}.

dispatch(setUp, [], _Instance) ->
    throw({bunit_skip, <<"skipping in setUp">>});
dispatch(testPass, [], _Instance) ->
    nil;
dispatch(tearDown, [], _Instance) ->
    nil.
