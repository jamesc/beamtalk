%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Error construction and formatting helpers.
%%%
%%% This module provides helper functions for creating and formatting
%%% beamtalk_error records. All runtime errors should use these helpers
%%% to ensure consistent error messages and structure.
%%%
%%% See docs/internal/design-self-as-object.md Section 3.8 for error taxonomy.

-module(beamtalk_error).
-include("beamtalk.hrl").

-export([
    new/2,
    with_selector/2,
    with_hint/2,
    with_details/2,
    format/1
]).

%% Type definition for error record
-type error() :: #beamtalk_error{}.
-export_type([error/0]).

%% @doc Create a new error with the specified kind and class.
%%
%% The message is generated automatically based on kind and class.
%% Use with_selector/2, with_hint/2, and with_details/2 to add more context.
%%
%% Example:
%%   Error = beamtalk_error:new(does_not_understand, 'Integer')
-spec new(atom(), atom()) -> #beamtalk_error{}.
new(Kind, Class) ->
    Message = generate_message(Kind, Class, undefined),
    #beamtalk_error{
        kind = Kind,
        class = Class,
        selector = undefined,
        message = Message,
        hint = undefined,
        details = #{}
    }.

%% @doc Add a selector to an existing error.
%%
%% This is used when the error is related to a specific method call.
%% The message is regenerated to include the selector.
%%
%% Example:
%%   Error0 = beamtalk_error:new(does_not_understand, 'Integer'),
%%   Error = beamtalk_error:with_selector(Error0, 'foo')
-spec with_selector(#beamtalk_error{}, atom()) -> #beamtalk_error{}.
with_selector(#beamtalk_error{kind = Kind, class = Class} = Error, Selector) ->
    Message = generate_message(Kind, Class, Selector),
    Error#beamtalk_error{
        selector = Selector,
        message = Message
    }.

%% @doc Add a hint to an existing error.
%%
%% Hints provide actionable suggestions for fixing the error.
%%
%% Example:
%%   Error0 = beamtalk_error:new(does_not_understand, 'Integer'),
%%   Error = beamtalk_error:with_hint(Error0, <<"Check spelling">>)
-spec with_hint(#beamtalk_error{}, binary()) -> #beamtalk_error{}.
with_hint(Error, Hint) ->
    Error#beamtalk_error{hint = Hint}.

%% @doc Add additional context details to an error.
%%
%% Details is a map with additional context like arity, expected types, etc.
%%
%% Example:
%%   Error0 = beamtalk_error:new(arity_mismatch, 'Counter'),
%%   Error = beamtalk_error:with_details(Error0, #{expected => 1, got => 0})
-spec with_details(#beamtalk_error{}, map()) -> #beamtalk_error{}.
with_details(Error, Details) ->
    Error#beamtalk_error{details = Details}.

%% @doc Format an error for user-facing display.
%%
%% Returns a formatted error message as a binary, suitable for printing
%% or returning to the user. Uses user-facing names (e.g., 'self' not 'Self').
%%
%% Example:
%%   iolist_to_binary(beamtalk_error:format(Error))
%%   % => <<"Integer does not understand 'foo'\nHint: Check spelling">>
-spec format(#beamtalk_error{}) -> iolist().
format(#beamtalk_error{message = Message, hint = undefined}) ->
    Message;
format(#beamtalk_error{message = Message, hint = Hint}) ->
    [Message, <<"\nHint: ">>, Hint].

%% Internal helper to generate error messages
-spec generate_message(atom(), atom(), atom() | undefined) -> binary().
generate_message(does_not_understand, Class, undefined) ->
    iolist_to_binary(io_lib:format("~s does not understand message", [Class]));
generate_message(does_not_understand, Class, Selector) ->
    iolist_to_binary(io_lib:format("~s does not understand '~s'", [Class, Selector]));
generate_message(immutable_value, Class, undefined) ->
    iolist_to_binary(io_lib:format("Cannot mutate ~s (immutable value)", [Class]));
generate_message(immutable_value, Class, Selector) ->
    iolist_to_binary(io_lib:format("Cannot call '~s' on ~s (immutable value)", [Selector, Class]));
generate_message(arity_mismatch, Class, undefined) ->
    iolist_to_binary(io_lib:format("Wrong number of arguments to ~s method", [Class]));
generate_message(arity_mismatch, Class, Selector) ->
    iolist_to_binary(io_lib:format("Wrong number of arguments to '~s' on ~s", [Selector, Class]));
generate_message(type_error, Class, undefined) ->
    iolist_to_binary(io_lib:format("Type error in ~s", [Class]));
generate_message(type_error, Class, Selector) ->
    iolist_to_binary(io_lib:format("Type error in '~s' on ~s", [Selector, Class]));
generate_message(future_not_awaited, _Class, undefined) ->
    iolist_to_binary(io_lib:format("Sent message to a Future", []));
generate_message(future_not_awaited, _Class, Selector) ->
    iolist_to_binary(io_lib:format("Sent '~s' to a Future", [Selector]));
generate_message(instantiation_error, Class, undefined) ->
    iolist_to_binary(io_lib:format("Cannot instantiate ~s", [Class]));
generate_message(instantiation_error, Class, Selector) ->
    iolist_to_binary(io_lib:format("Cannot call '~s' on ~s", [Selector, Class]));
generate_message(Kind, Class, undefined) ->
    iolist_to_binary(io_lib:format("~s error in ~s", [Kind, Class]));
generate_message(Kind, Class, Selector) ->
    iolist_to_binary(io_lib:format("~s error in '~s' on ~s", [Kind, Selector, Class])).
