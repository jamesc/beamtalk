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
    error(#beamtalk_error{
        kind = assertion_failed,
        class = 'TestCase',
        selector = undefined,
        message = <<"assert failed">>,
        hint = undefined,
        details = #{}
    });
dispatch(testBeamtalkError, [], _Instance) ->
    error(#beamtalk_error{
        kind = type_error,
        class = 'TestCase',
        selector = undefined,
        message = <<"generic error">>,
        hint = undefined,
        details = #{}
    });
dispatch(testUndef, [], _Instance) ->
    nonexistent_module_xyz_bt2391:'nope'();
%% BT-2404: BEAM error-shape dispatchers for decode_beam_error/1 coverage.
%% These all fall through to error:TestReason:TestST in run_test_method/5
%% (they are not #beamtalk_error{} records nor `undef`, so they bypass
%% the specialised catch clauses and reach format_test_error/3).
dispatch(testFutureRejected, [], _Instance) ->
    error({future_rejected, inner_reason});
dispatch(testErrorMapWithKey, [], _Instance) ->
    error({error, #{error => raw_inner_reason}});
dispatch(testErrorMapNoKey, [], _Instance) ->
    error({error, #{unrelated => value}});
dispatch(testExceptionMap, [], _Instance) ->
    error(#{
        class => 'Exception',
        '$beamtalk_class' => 'Exception',
        error => #beamtalk_error{
            kind = type_error,
            class = 'TestCase',
            selector = undefined,
            message = <<"exc msg">>,
            hint = undefined,
            details = #{}
        }
    });
dispatch(testClassMap, [], _Instance) ->
    error(#{
        '$beamtalk_class' => 'RuntimeError',
        error => #beamtalk_error{
            kind = type_error,
            class = 'TestCase',
            selector = undefined,
            message = <<"cls msg">>,
            hint = undefined,
            details = #{}
        }
    });
dispatch(testBadKey, [], _Instance) ->
    error({badkey, my_key});
dispatch(testBadMap, [], _Instance) ->
    error({badmap, 42});
dispatch(testBadMatch, [], _Instance) ->
    error({badmatch, some_value});
dispatch(testCaseClause, [], _Instance) ->
    error({case_clause, some_value});
dispatch(testTryClause, [], _Instance) ->
    error({try_clause, some_value});
dispatch(testFunctionClause, [], _Instance) ->
    error(function_clause);
dispatch(testIfClause, [], _Instance) ->
    error(if_clause);
dispatch(testBadArity, [], _Instance) ->
    F = fun(X) -> X end,
    error({badarity, {F, [1, 2]}});
dispatch(testBadFun, [], _Instance) ->
    error({badfun, not_a_fun});
dispatch(testBadArg, [], _Instance) ->
    error(badarg);
dispatch(testBadArith, [], _Instance) ->
    error(badarith);
dispatch(testNoproc, [], _Instance) ->
    error(noproc);
dispatch(testTimeout, [], _Instance) ->
    error(timeout);
dispatch(setUp, [], Instance) ->
    Instance;
dispatch(tearDown, [], _Instance) ->
    nil.
