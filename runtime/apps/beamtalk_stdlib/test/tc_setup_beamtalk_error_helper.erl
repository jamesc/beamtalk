%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Helper for setUp-failure path: setUp raises a raw #beamtalk_error{} record.
%% Exercises the outer catch clause at lines 854-856 of beamtalk_test_case.erl:
%%   error:#beamtalk_error{message = SetupErrMsg} -> {fail, MethodName, ...}
-module(tc_setup_beamtalk_error_helper).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([new/0, dispatch/3]).

new() ->
    #{'$beamtalk_class' => 'FakeTest'}.

dispatch(setUp, [], _Instance) ->
    %% Raise a raw (unwrapped) #beamtalk_error{} — not via beamtalk_error:raise/1.
    %% Normal BT code wraps errors; this simulates unusual interop callers.
    error(#beamtalk_error{kind = setup_failed, class = 'TestCase', message = <<"setUp bt error">>});
dispatch(testPass, [], _Instance) ->
    nil;
dispatch(tearDown, [], _Instance) ->
    nil.
