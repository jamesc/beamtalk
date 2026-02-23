%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_error_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% Test: Create basic error with new/2
new_creates_error_with_kind_and_class_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Integer'),
    ?assertEqual(does_not_understand, Error#beamtalk_error.kind),
    ?assertEqual('Integer', Error#beamtalk_error.class),
    ?assertEqual(undefined, Error#beamtalk_error.selector),
    ?assertEqual(undefined, Error#beamtalk_error.hint),
    ?assertEqual(#{}, Error#beamtalk_error.details).

%%% Test: Create error with new/3
new3_creates_error_with_selector_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Integer', 'foo'),
    ?assertEqual(does_not_understand, Error#beamtalk_error.kind),
    ?assertEqual('Integer', Error#beamtalk_error.class),
    ?assertEqual('foo', Error#beamtalk_error.selector),
    ?assertEqual(undefined, Error#beamtalk_error.hint),
    ?assertEqual(<<"Integer does not understand 'foo'">>, Error#beamtalk_error.message).

%%% Test: Create error with new/4
new4_creates_error_with_selector_and_hint_test() ->
    Hint = <<"Check spelling">>,
    Error = beamtalk_error:new(does_not_understand, 'Integer', 'foo', Hint),
    ?assertEqual(does_not_understand, Error#beamtalk_error.kind),
    ?assertEqual('Integer', Error#beamtalk_error.class),
    ?assertEqual('foo', Error#beamtalk_error.selector),
    ?assertEqual(Hint, Error#beamtalk_error.hint),
    ?assertEqual(<<"Integer does not understand 'foo'">>, Error#beamtalk_error.message).

%%% Test: with_selector/2 adds selector and updates message
with_selector_adds_selector_test() ->
    Error0 = beamtalk_error:new(does_not_understand, 'Integer'),
    Error = beamtalk_error:with_selector(Error0, 'foo'),
    ?assertEqual('foo', Error#beamtalk_error.selector),
    ?assertMatch(<<"Integer does not understand 'foo'">>, Error#beamtalk_error.message).

%%% Test: with_hint/2 adds hint
with_hint_adds_hint_test() ->
    Hint = <<"Check spelling">>,
    Error0 = beamtalk_error:new(does_not_understand, 'Integer'),
    Error = beamtalk_error:with_hint(Error0, Hint),
    ?assertEqual(Hint, Error#beamtalk_error.hint).

%%% Test: with_details/2 adds details map
with_details_adds_details_test() ->
    Details = #{arity => 2, expected => 1},
    Error0 = beamtalk_error:new(arity_mismatch, 'Counter'),
    Error = beamtalk_error:with_details(Error0, Details),
    ?assertEqual(Details, Error#beamtalk_error.details).

%%% Test: format/1 returns message when no hint
format_without_hint_test() ->
    Error0 = beamtalk_error:new(does_not_understand, 'Integer'),
    Error = beamtalk_error:with_selector(Error0, 'foo'),
    Formatted = iolist_to_binary(beamtalk_error:format(Error)),
    ?assertEqual(<<"Integer does not understand 'foo'">>, Formatted).

%%% Test: format/1 includes hint when present
format_with_hint_test() ->
    Error0 = beamtalk_error:new(does_not_understand, 'Integer'),
    Error1 = beamtalk_error:with_selector(Error0, 'foo'),
    Error = beamtalk_error:with_hint(
        Error1, <<"Check spelling or use 'respondsTo:' to verify method exists">>
    ),
    Formatted = iolist_to_binary(beamtalk_error:format(Error)),
    Expected =
        <<"Integer does not understand 'foo'\nHint: Check spelling or use 'respondsTo:' to verify method exists">>,
    ?assertEqual(Expected, Formatted).

%%% Test: Pipeline style error construction
pipeline_construction_test() ->
    Error0 = beamtalk_error:new(does_not_understand, 'Counter'),
    Error1 = beamtalk_error:with_selector(Error0, 'foo'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Did you mean 'getValue'?">>),
    Error = beamtalk_error:with_details(Error2, #{arity => 0}),

    ?assertEqual(does_not_understand, Error#beamtalk_error.kind),
    ?assertEqual('Counter', Error#beamtalk_error.class),
    ?assertEqual('foo', Error#beamtalk_error.selector),
    ?assertEqual(<<"Did you mean 'getValue'?">>, Error#beamtalk_error.hint),
    ?assertEqual(#{arity => 0}, Error#beamtalk_error.details).

%%% Test: does_not_understand message generation
does_not_understand_message_test() ->
    Error1 = beamtalk_error:new(does_not_understand, 'Integer'),
    ?assertEqual(<<"Integer does not understand message">>, Error1#beamtalk_error.message),

    Error2 = beamtalk_error:with_selector(Error1, 'foo'),
    ?assertEqual(<<"Integer does not understand 'foo'">>, Error2#beamtalk_error.message).

%%% Test: immutable_value message generation
immutable_value_message_test() ->
    Error1 = beamtalk_error:new(immutable_value, 'Integer'),
    ?assertEqual(<<"Cannot mutate Integer (immutable value)">>, Error1#beamtalk_error.message),

    Error2 = beamtalk_error:with_selector(Error1, 'fieldAt:put:'),
    ?assertEqual(
        <<"Cannot call 'fieldAt:put:' on Integer (immutable value)">>,
        Error2#beamtalk_error.message
    ).

%%% Test: arity_mismatch message generation
arity_mismatch_message_test() ->
    Error1 = beamtalk_error:new(arity_mismatch, 'Counter'),
    ?assertEqual(<<"Wrong number of arguments to Counter method">>, Error1#beamtalk_error.message),

    Error2 = beamtalk_error:with_selector(Error1, 'at:'),
    ?assertEqual(
        <<"Wrong number of arguments to 'at:' on Counter">>, Error2#beamtalk_error.message
    ).

%%% Test: type_error message generation
type_error_message_test() ->
    Error1 = beamtalk_error:new(type_error, 'String'),
    ?assertEqual(<<"Type error in String">>, Error1#beamtalk_error.message),

    Error2 = beamtalk_error:with_selector(Error1, '+'),
    ?assertEqual(<<"Type error in '+' on String">>, Error2#beamtalk_error.message).

%%% Test: future_not_awaited message generation
future_not_awaited_message_test() ->
    Error0 = beamtalk_error:new(future_not_awaited, 'Future'),
    Error = beamtalk_error:with_selector(Error0, 'size'),
    ?assertEqual(<<"Sent 'size' to a Future">>, Error#beamtalk_error.message).

%%% Test: future_not_awaited with hint (from design doc example)
future_not_awaited_with_hint_test() ->
    Error0 = beamtalk_error:new(future_not_awaited, 'Future'),
    Error1 = beamtalk_error:with_selector(Error0, 'size'),
    Error2 = beamtalk_error:with_hint(
        Error1, <<"Use 'await' to get the value: (expr await) size">>
    ),
    Error = beamtalk_error:with_details(Error2, #{original_selector => 'fieldNames'}),

    Formatted = iolist_to_binary(beamtalk_error:format(Error)),
    Expected = <<"Sent 'size' to a Future\nHint: Use 'await' to get the value: (expr await) size">>,
    ?assertEqual(Expected, Formatted),
    ?assertEqual(#{original_selector => 'fieldNames'}, Error#beamtalk_error.details).

%%% Test: future_not_awaited without selector (edge case)
future_not_awaited_without_selector_test() ->
    %% This tests the edge case where future_not_awaited is created without selector
    %% Should not crash - should have a reasonable default message
    Error = beamtalk_error:new(future_not_awaited, 'Future'),
    ?assertEqual(future_not_awaited, Error#beamtalk_error.kind),
    ?assertEqual('Future', Error#beamtalk_error.class),
    ?assertEqual(undefined, Error#beamtalk_error.selector),
    %% Should have a reasonable message even without selector
    ?assertMatch(<<"Sent message to a Future">>, Error#beamtalk_error.message),

    %% Should be able to format without crashing
    Formatted = iolist_to_binary(beamtalk_error:format(Error)),
    ?assertEqual(<<"Sent message to a Future">>, Formatted).

%%% Test: does_not_understand with hint (from design doc example)
does_not_understand_with_hint_test() ->
    Error0 = beamtalk_error:new(does_not_understand, 'Integer'),
    Error1 = beamtalk_error:with_selector(Error0, 'foo'),
    Error2 = beamtalk_error:with_hint(
        Error1, <<"Check spelling or use 'respondsTo:' to verify method exists">>
    ),
    Error = beamtalk_error:with_details(Error2, #{arity => 0}),

    ?assertEqual(does_not_understand, Error#beamtalk_error.kind),
    ?assertEqual('Integer', Error#beamtalk_error.class),
    ?assertEqual('foo', Error#beamtalk_error.selector),
    ?assertEqual(<<"Integer does not understand 'foo'">>, Error#beamtalk_error.message),
    ?assertEqual(
        <<"Check spelling or use 'respondsTo:' to verify method exists">>, Error#beamtalk_error.hint
    ),
    ?assertEqual(#{arity => 0}, Error#beamtalk_error.details),

    Formatted = iolist_to_binary(beamtalk_error:format(Error)),
    Expected =
        <<"Integer does not understand 'foo'\nHint: Check spelling or use 'respondsTo:' to verify method exists">>,
    ?assertEqual(Expected, Formatted).

%%% Tests for new error kinds added in BT-455

class_not_found_message_test() ->
    Error = beamtalk_error:new(class_not_found, 'Counter'),
    ?assertEqual(<<"Class 'Counter' not found">>, Error#beamtalk_error.message),
    ErrorWithSel = beamtalk_error:with_selector(Error, increment),
    ?assertEqual(
        <<"Class 'Counter' not found (while resolving 'increment')">>,
        ErrorWithSel#beamtalk_error.message
    ).

no_superclass_message_test() ->
    Error = beamtalk_error:new(no_superclass, 'ProtoObject'),
    ?assertEqual(<<"ProtoObject has no superclass">>, Error#beamtalk_error.message),
    ErrorWithSel = beamtalk_error:with_selector(Error, someMethod),
    ?assertEqual(
        <<"ProtoObject has no superclass (cannot resolve 'someMethod' via super)">>,
        ErrorWithSel#beamtalk_error.message
    ).

class_already_exists_message_test() ->
    Error = beamtalk_error:new(class_already_exists, 'Counter'),
    ?assertEqual(<<"Class 'Counter' already exists">>, Error#beamtalk_error.message).

internal_error_message_test() ->
    Error = beamtalk_error:new(internal_error, 'Runtime'),
    ?assertEqual(<<"Internal error in Runtime">>, Error#beamtalk_error.message),
    ErrorWithSel = beamtalk_error:with_selector(Error, dispatch),
    ?assertEqual(
        <<"Internal error in 'dispatch' on Runtime">>, ErrorWithSel#beamtalk_error.message
    ).

dispatch_error_message_test() ->
    Error = beamtalk_error:new(dispatch_error, 'Counter'),
    ?assertEqual(<<"Dispatch error for Counter">>, Error#beamtalk_error.message),
    ErrorWithSel = beamtalk_error:with_selector(Error, increment),
    ?assertEqual(
        <<"Dispatch error for 'increment' on Counter">>, ErrorWithSel#beamtalk_error.message
    ).

callback_failed_message_test() ->
    Error = beamtalk_error:new(callback_failed, 'Actor'),
    ?assertEqual(<<"Callback failed for Actor">>, Error#beamtalk_error.message),
    ErrorWithSel = beamtalk_error:with_selector(Error, 'on_actor_spawned'),
    ?assertEqual(
        <<"Callback 'on_actor_spawned' failed for Actor">>, ErrorWithSel#beamtalk_error.message
    ).

%%% Test: raise/1 wraps and throws as Exception tagged map (ADR 0015)
raise_wraps_and_throws_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Integer'),
    Error1 = beamtalk_error:with_selector(Error, 'foo'),
    try
        beamtalk_error:raise(Error1)
    catch
        error:Caught ->
            ?assertMatch(#{'$beamtalk_class' := _, error := _}, Caught),
            #{'$beamtalk_class' := _, error := Inner} = Caught,
            ?assertEqual(does_not_understand, Inner#beamtalk_error.kind),
            ?assertEqual('Integer', Inner#beamtalk_error.class),
            ?assertEqual('foo', Inner#beamtalk_error.selector)
    end.

%%% Test: raise/1 produces correct exception class based on error kind (BT-452)
raise_produces_exception_class_test() ->
    Error = beamtalk_error:new(type_error, 'String'),
    try
        beamtalk_error:raise(Error)
    catch
        error:#{'$beamtalk_class' := Class} ->
            ?assertEqual('TypeError', Class)
    end.
