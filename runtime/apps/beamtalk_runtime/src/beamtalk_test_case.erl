%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc TestCase primitive implementations for BUnit test framework.
%%%
%%% This module provides the runtime support for TestCase assertion methods.
%%% All assertions create structured #beamtalk_error{} records with kind
%%% `assertion_failed` when they fail.
%%%
%%% Part of ADR 0014: BUnit — Beamtalk Test Framework (Phase 2).

-module(beamtalk_test_case).
-include("beamtalk.hrl").

-export([
    assert/1,
    assert_equals/2,
    deny/1,
    should_raise/2,
    fail/1
]).

%% @doc Assert that a condition is true.
%%
%% Raises assertion_failed error if condition is false.
%% Condition must be a boolean (true or false atom).
%%
%% Example:
%%   assert(true)   % => passes
%%   assert(false)  % => fails with assertion_failed
-spec assert(boolean()) -> nil.
assert(true) ->
    nil;
assert(false) ->
    Error0 = beamtalk_error:new(assertion_failed, 'TestCase'),
    Error1 = beamtalk_error:with_selector(Error0, 'assert:'),
    Error2 = beamtalk_error:with_message(Error1, <<"Assertion failed: expected true but got false">>),
    Error3 = beamtalk_error:with_details(Error2, #{expected => true, actual => false}),
    beamtalk_error:raise(Error3);
assert(Other) ->
    Error0 = beamtalk_error:new(type_error, 'TestCase'),
    Error1 = beamtalk_error:with_selector(Error0, 'assert:'),
    Message = iolist_to_binary(io_lib:format("Expected boolean, got: ~p", [Other])),
    Error2 = beamtalk_error:with_message(Error1, Message),
    beamtalk_error:raise(Error2).

%% @doc Assert that two values are equal.
%%
%% Uses Erlang's == operator for comparison (value equality).
%% Raises assertion_failed error if values are not equal.
%%
%% Example:
%%   assert_equals(3, 3)   % => passes
%%   assert_equals(3, 4)   % => fails with assertion_failed
-spec assert_equals(term(), term()) -> nil.
assert_equals(Expected, Actual) when Expected == Actual ->
    nil;
assert_equals(Expected, Actual) ->
    Error0 = beamtalk_error:new(assertion_failed, 'TestCase'),
    Error1 = beamtalk_error:with_selector(Error0, 'assert:equals:'),
    Message = format_comparison_error(Expected, Actual),
    Error2 = beamtalk_error:with_message(Error1, Message),
    Error3 = beamtalk_error:with_details(Error2, #{expected => Expected, actual => Actual}),
    beamtalk_error:raise(Error3).

%% @doc Assert that a condition is false.
%%
%% Raises assertion_failed error if condition is true.
%% Condition must be a boolean (true or false atom).
%%
%% Example:
%%   deny(false)  % => passes
%%   deny(true)   % => fails with assertion_failed
-spec deny(boolean()) -> nil.
deny(false) ->
    nil;
deny(true) ->
    Error0 = beamtalk_error:new(assertion_failed, 'TestCase'),
    Error1 = beamtalk_error:with_selector(Error0, 'deny:'),
    Error2 = beamtalk_error:with_message(Error1, <<"Assertion failed: expected false but got true">>),
    Error3 = beamtalk_error:with_details(Error2, #{expected => false, actual => true}),
    beamtalk_error:raise(Error3);
deny(Other) ->
    Error0 = beamtalk_error:new(type_error, 'TestCase'),
    Error1 = beamtalk_error:with_selector(Error0, 'deny:'),
    Message = iolist_to_binary(io_lib:format("Expected boolean, got: ~p", [Other])),
    Error2 = beamtalk_error:with_message(Error1, Message),
    beamtalk_error:raise(Error2).

%% @doc Assert that a block raises an error of the specified kind.
%%
%% Executes the block and verifies that it raises an error with the
%% given kind atom. If the block completes normally or raises a different
%% kind of error, the assertion fails.
%%
%% Example:
%%   Block = fun() -> error(my_error) end,
%%   should_raise(Block, my_error)  % => passes
%%
%% Note: Block is a zero-argument Erlang fun in Core Erlang codegen.
-spec should_raise(fun(() -> term()), atom()) -> nil.
should_raise(Block, ExpectedKind) when is_function(Block, 0), is_atom(ExpectedKind) ->
    try
        Block(),
        % Block completed without error
        Error0 = beamtalk_error:new(assertion_failed, 'TestCase'),
        Error1 = beamtalk_error:with_selector(Error0, 'should:raise:'),
        Message = iolist_to_binary(io_lib:format("Expected block to raise ~s but it completed normally", [ExpectedKind])),
        Error2 = beamtalk_error:with_message(Error1, Message),
        Error3 = beamtalk_error:with_details(Error2, #{expected_kind => ExpectedKind, actual => completed}),
        beamtalk_error:raise(Error3)
    catch
        error:Exception ->
            ActualKind = extract_error_kind(Exception),
            case ActualKind of
                ExpectedKind ->
                    nil;
                _ ->
                    Error0 = beamtalk_error:new(assertion_failed, 'TestCase'),
                    Error1 = beamtalk_error:with_selector(Error0, 'should:raise:'),
                    Message = iolist_to_binary(io_lib:format("Expected error kind ~s but got ~s", [ExpectedKind, ActualKind])),
                    Error2 = beamtalk_error:with_message(Error1, Message),
                    Error3 = beamtalk_error:with_details(Error2, #{expected_kind => ExpectedKind, actual_kind => ActualKind}),
                    beamtalk_error:raise(Error3)
            end
    end.

%% @doc Unconditionally fail with a message.
%%
%% Always raises an assertion_failed error with the given message.
%% Used when a test detects an invariant violation.
%%
%% Example:
%%   fail(<<"This should not happen">>)
-spec fail(binary() | atom()) -> no_return().
fail(Message) when is_binary(Message) ->
    Error0 = beamtalk_error:new(assertion_failed, 'TestCase'),
    Error1 = beamtalk_error:with_selector(Error0, 'fail:'),
    Error2 = beamtalk_error:with_message(Error1, Message),
    beamtalk_error:raise(Error2);
fail(Message) when is_atom(Message) ->
    fail(atom_to_binary(Message, utf8));
fail(Message) ->
    MessageBin = iolist_to_binary(io_lib:format("~p", [Message])),
    fail(MessageBin).

%%% Internal helpers

%% @doc Format a comparison error message.
-spec format_comparison_error(term(), term()) -> binary().
format_comparison_error(Expected, Actual) ->
    ExpectedStr = format_value(Expected),
    ActualStr = format_value(Actual),
    iolist_to_binary(io_lib:format("Expected ~s, got ~s", [ExpectedStr, ActualStr])).

%% @doc Format a value for display in error messages.
-spec format_value(term()) -> binary().
format_value(Value) when is_binary(Value) ->
    % String - show as 'string'
    <<"'", Value/binary, "'">>;
format_value(Value) when is_atom(Value) ->
    % Atom - show as #symbol
    atom_to_binary(Value, utf8);
format_value(Value) when is_list(Value) ->
    % List - show as [...] (truncated if long)
    case length(Value) of
        Len when Len =< 5 ->
            iolist_to_binary(io_lib:format("~p", [Value]));
        Len ->
            Truncated = lists:sublist(Value, 5),
            iolist_to_binary(io_lib:format("~p... (~p items)", [Truncated, Len]))
    end;
format_value(Value) ->
    % Other types - use Erlang's ~p formatter
    iolist_to_binary(io_lib:format("~p", [Value])).

%% @doc Extract the error kind from an exception.
%%
%% Handles both #beamtalk_error{} records and wrapped Exception objects (ADR 0015).
-spec extract_error_kind(term()) -> atom().
extract_error_kind(#beamtalk_error{kind = Kind}) ->
    Kind;
extract_error_kind(#{class := 'Exception', error := #beamtalk_error{kind = Kind}}) ->
    % ADR 0015: Exception objects wrap #beamtalk_error{} records
    Kind;
extract_error_kind(_) ->
    % Unknown error format - return generic 'error' kind
    error.
