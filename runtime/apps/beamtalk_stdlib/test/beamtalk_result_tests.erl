%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_result_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_result module (BT-1254).

Tests cover:
- from_tagged_tuple/1 — ok tuple, error tuple (atom, #beamtalk_error{}, wrapped map)
- class_tryDo:/3 — success path, exception path
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% from_tagged_tuple/1 — ok path
%%% ============================================================================

from_tagged_tuple_ok_integer_test() ->
    Result = beamtalk_result:from_tagged_tuple({ok, 42}),
    ?assertMatch(
        #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := 42, 'errReason' := nil},
        Result
    ).

from_tagged_tuple_ok_binary_test() ->
    Result = beamtalk_result:from_tagged_tuple({ok, <<"hello">>}),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := true,
            'okValue' := <<"hello">>,
            'errReason' := nil
        },
        Result
    ).

from_tagged_tuple_ok_nil_test() ->
    Result = beamtalk_result:from_tagged_tuple({ok, nil}),
    ?assertMatch(
        #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil, 'errReason' := nil},
        Result
    ).

%%% ============================================================================
%%% from_tagged_tuple/1 — error path with atom reason
%%% ============================================================================

from_tagged_tuple_error_atom_test() ->
    %% Bare atom errors are wrapped into Exception objects by ensure_wrapped/1
    Result = beamtalk_result:from_tagged_tuple({error, file_not_found}),
    ?assertMatch(
        #{'$beamtalk_class' := 'Result', 'isOk' := false, 'okValue' := nil},
        Result
    ),
    #{'errReason' := ErrReason} = Result,
    %% errReason is now a Beamtalk Exception tagged map (ensure_wrapped applied)
    ?assertMatch(#{'$beamtalk_class' := _, 'error' := #beamtalk_error{}}, ErrReason).

%%% ============================================================================
%%% makeError:/1 — direct error constructor (reason stored as-is)
%%% ============================================================================

error_atom_stores_as_is_test() ->
    %% makeError: does NOT wrap — reason is stored verbatim
    Result = beamtalk_result:'makeError:'(file_not_found),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'okValue' := nil,
            'errReason' := file_not_found
        },
        Result
    ).

ok_stores_value_test() ->
    Result = beamtalk_result:'ok:'(42),
    ?assertMatch(
        #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := 42, 'errReason' := nil},
        Result
    ).

%%% ============================================================================
%%% from_tagged_tuple/1 — error path with #beamtalk_error{} record
%%% ============================================================================

from_tagged_tuple_error_beamtalk_error_test() ->
    Error = #beamtalk_error{
        kind = file_not_found,
        class = 'File',
        selector = 'readAll:',
        message = <<"File not found: test.txt">>,
        hint = <<"Check the path">>,
        details = #{path => <<"test.txt">>}
    },
    Result = beamtalk_result:from_tagged_tuple({error, Error}),
    ?assertMatch(
        #{'$beamtalk_class' := 'Result', 'isOk' := false, 'okValue' := nil},
        Result
    ),
    #{'errReason' := ErrReason} = Result,
    %% errReason is the wrapped Exception tagged map
    ?assertMatch(
        #{'$beamtalk_class' := _, 'error' := #beamtalk_error{kind = file_not_found}}, ErrReason
    ).

%%% ============================================================================
%%% from_tagged_tuple/1 — error path with already-wrapped Exception map
%%% ============================================================================

from_tagged_tuple_error_already_wrapped_test() ->
    %% ensure_wrapped/1 is idempotent — already-wrapped maps pass through
    AlreadyWrapped = #{
        '$beamtalk_class' => 'RuntimeError',
        'error' => #beamtalk_error{
            kind = runtime_error,
            class = 'Object',
            selector = undefined,
            message = <<"something failed">>,
            hint = undefined,
            details = #{}
        }
    },
    Result = beamtalk_result:from_tagged_tuple({error, AlreadyWrapped}),
    ?assertMatch(
        #{'$beamtalk_class' := 'Result', 'isOk' := false, 'okValue' := nil},
        Result
    ),
    #{'errReason' := ErrReason} = Result,
    ?assertEqual(AlreadyWrapped, ErrReason).

%%% ============================================================================
%%% tryDo: — type guard (non-fun argument)
%%% ============================================================================

try_do_non_fun_raises_type_error_test() ->
    %% Passing a non-block (e.g. an integer) raises type_error rather than badfun crash.
    try
        beamtalk_result:'tryDo:'(42),
        ?assert(false, "tryDo: should have raised a type_error")
    catch
        error:Caught ->
            ?assertMatch(
                #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}}, Caught
            ),
            #{error := Inner} = Caught,
            ?assertEqual('Result', Inner#beamtalk_error.class),
            ?assertEqual('tryDo:', Inner#beamtalk_error.selector)
    end.

%%% ============================================================================
%%% class_tryDo:/3 — success path
%%% ============================================================================

class_try_do_success_test() ->
    %% A block that returns a value normally
    Block = fun() -> 99 end,
    Result = beamtalk_result:'tryDo:'(Block),
    ?assertMatch(
        #{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := 99, 'errReason' := nil},
        Result
    ).

%%% ============================================================================
%%% class_tryDo:/3 — exception path
%%% ============================================================================

class_try_do_exception_test() ->
    %% A block that raises a beamtalk_error exception
    Error = #beamtalk_error{
        kind = runtime_error,
        class = 'Object',
        selector = undefined,
        message = <<"test exception">>,
        hint = undefined,
        details = #{}
    },
    Block = fun() -> beamtalk_error:raise(Error) end,
    Result = beamtalk_result:'tryDo:'(Block),
    ?assertMatch(
        #{'$beamtalk_class' := 'Result', 'isOk' := false, 'okValue' := nil},
        Result
    ),
    #{'errReason' := ErrReason} = Result,
    ?assertMatch(
        #{'$beamtalk_class' := _, 'error' := #beamtalk_error{kind = runtime_error}}, ErrReason
    ).

%%% ============================================================================
%%% unwrapError:/2 — re-raise preserves error identity
%%% ============================================================================

unwrap_error_rewraps_exception_test() ->
    %% tryDo: catches an exception, then unwrapError: re-raises it preserving the original
    %% error identity (class, message, kind). beamtalk_error:raise/1 raises a tagged map
    %% (not the raw record), so we catch the tagged map and check the embedded record.
    OrigError = #beamtalk_error{
        kind = file_not_found,
        class = 'File',
        selector = 'readAll:',
        message = <<"File not found: test.txt">>,
        hint = <<"Check the path">>,
        details = #{path => <<"test.txt">>}
    },
    Block = fun() -> beamtalk_error:raise(OrigError) end,
    Result = beamtalk_result:'tryDo:'(Block),
    #{'errReason' := ErrReason} = Result,
    %% ErrReason is the wrapped exception map; unwrapError: should re-raise the embedded error
    try
        beamtalk_result:'unwrapError:'(undefined, ErrReason),
        ?assert(false, "unwrapError: should have raised an exception")
    catch
        error:Caught ->
            ?assertMatch(#{'$beamtalk_class' := _, error := _}, Caught),
            #{error := Inner} = Caught,
            ?assertEqual(file_not_found, Inner#beamtalk_error.kind),
            ?assertEqual('File', Inner#beamtalk_error.class),
            ?assertEqual('readAll:', Inner#beamtalk_error.selector)
    end.

unwrap_error_raw_symbol_test() ->
    %% unwrapError: with a raw symbol (not a wrapped exception) raises a generic signal error
    try
        beamtalk_result:'unwrapError:'(undefined, not_found),
        ?assert(false, "unwrapError: should have raised an exception")
    catch
        error:Caught ->
            ?assertMatch(
                #{'$beamtalk_class' := _, error := #beamtalk_error{kind = signal}}, Caught
            ),
            #{error := Inner} = Caught,
            ?assertEqual('Result', Inner#beamtalk_error.class),
            ?assertEqual('unwrap', Inner#beamtalk_error.selector),
            ?assertEqual(
                <<"unwrap called on Result error: #not_found">>,
                Inner#beamtalk_error.message
            )
    end.

unwrap_error_raw_map_test() ->
    %% unwrapError: with a raw map formats it via print_string, not ~p
    try
        beamtalk_result:'unwrapError:'(undefined, #{<<"detail">> => <<"oops">>}),
        ?assert(false, "unwrapError: should have raised an exception")
    catch
        error:Caught ->
            #{error := Inner} = Caught,
            Msg = Inner#beamtalk_error.message,
            %% Should use Beamtalk formatting, not raw Erlang ~p
            ?assertNotEqual(nomatch, binary:match(Msg, <<"unwrap called on Result error:">>)),
            %% Must NOT contain Erlang binary syntax
            ?assertEqual(nomatch, binary:match(Msg, <<"<<\"">>))
    end.

%%% ============================================================================
%%% tryDo: — NLR re-raise
%%% ============================================================================

try_do_nlr_reraise_test() ->
    %% tryDo: must NOT catch non-local return throws (they use {'$bt_nlr', ...} tuples).
    %% Verify that the 4-tuple form propagates through.
    FakeToken = make_ref(),
    ?assertThrow(
        {'$bt_nlr', FakeToken, 42, some_state},
        beamtalk_result:'tryDo:'(fun() -> throw({'$bt_nlr', FakeToken, 42, some_state}) end)
    ).

try_do_nlr_reraise_3tuple_test() ->
    %% 3-tuple NLR form (backward compatibility) also propagates through.
    FakeToken = make_ref(),
    ?assertThrow(
        {'$bt_nlr', FakeToken, hello},
        beamtalk_result:'tryDo:'(fun() -> throw({'$bt_nlr', FakeToken, hello}) end)
    ).
