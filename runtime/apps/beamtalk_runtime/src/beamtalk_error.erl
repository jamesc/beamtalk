%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Error construction and formatting helpers.
%%%
%%% **DDD Context:** Object System Context
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
    new/3,
    new/4,
    with_message/2,
    with_selector/2,
    with_hint/2,
    with_details/2,
    format/1,
    format_safe/1,
    format_safe/2,
    extract_beamtalk_error/2,
    raise/1,
    generate_message/3,
    format_reason/2
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

%% @doc Create a new error with kind, class, and selector.
-spec new(atom(), atom(), atom()) -> #beamtalk_error{}.
new(Kind, Class, Selector) ->
    with_selector(new(Kind, Class), Selector).

%% @doc Create a new error with kind, class, selector, and hint.
-spec new(atom(), atom(), atom(), term()) -> #beamtalk_error{}.
new(Kind, Class, Selector, Hint) ->
    with_hint(new(Kind, Class, Selector), Hint).

%% @doc Set the message on an existing error.
%%
%% Overrides the auto-generated message with a custom one.
%% Used by Object>>error: for user-supplied error messages.
%%
%% Example:
%%   Error0 = beamtalk_error:new(user_error, 'Counter'),
%%   Error = beamtalk_error:with_message(Error0, <<"invalid state">>)
-spec with_message(#beamtalk_error{}, term()) -> #beamtalk_error{}.
with_message(Error, Message) when is_binary(Message) ->
    Error#beamtalk_error{message = Message};
with_message(Error, Message) when is_atom(Message) ->
    Error#beamtalk_error{message = atom_to_binary(Message, utf8)};
with_message(Error, Message) ->
    Error#beamtalk_error{message = iolist_to_binary(io_lib:format("~p", [Message]))}.

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
-spec with_hint(#beamtalk_error{}, term()) -> #beamtalk_error{}.
with_hint(Error, undefined) ->
    Error;
with_hint(Error, Hint) when is_binary(Hint) ->
    Error#beamtalk_error{hint = Hint};
with_hint(Error, Hint) when is_atom(Hint) ->
    Error#beamtalk_error{hint = atom_to_binary(Hint, utf8)};
with_hint(Error, Hint) ->
    Error#beamtalk_error{hint = iolist_to_binary(io_lib:format("~tp", [Hint]))}.

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
%% Always returns a binary — safe to pass directly to Beamtalk as a String.
%%
%% Example:
%%   beamtalk_error:format(Error)
%%   % => <<"Integer does not understand 'foo'\nHint: Check spelling">>
-spec format(#beamtalk_error{}) -> binary().
format(#beamtalk_error{kind = does_not_understand} = Error) ->
    Enriched = maybe_enrich_dnu_hint(Error),
    format_parts(Enriched#beamtalk_error.message, Enriched#beamtalk_error.hint);
format(#beamtalk_error{message = Message, hint = Hint}) ->
    format_parts(Message, Hint).

%% @doc Safely format any error term as a human-readable binary.
%%
%% If the term is a `#beamtalk_error{}`, uses `format/1` to produce a rich
%% message with hints. For any other term, falls back to `~0p` formatting.
%% Never crashes — used by generated `handle_continue` to log initialize failures.
-spec format_safe(term()) -> binary().
format_safe(#beamtalk_error{} = Error) ->
    try
        format(Error)
    catch
        _:_ -> iolist_to_binary(io_lib:format("~0p", [Error]))
    end;
format_safe(Term) ->
    iolist_to_binary(io_lib:format("~0p", [Term])).

%% @doc Safely format an error term, searching the stacktrace for a
%% `#beamtalk_error{}` if the primary reason is not informative (e.g.,
%% `{error, function_clause}`).
%%
%% Used by generated `handle_continue` to produce useful log messages even
%% when the Erlang-level reason is a raw function_clause or case_clause.
-spec format_safe(term(), term()) -> binary().
format_safe(#beamtalk_error{} = Error, _Stacktrace) ->
    format_safe(Error);
format_safe(#{'$beamtalk_class' := _, error := #beamtalk_error{} = Error}, _Stacktrace) ->
    format_safe(Error);
format_safe(Reason, Stacktrace) ->
    %% Try to extract a beamtalk_error from the stacktrace args
    case extract_beamtalk_error(Stacktrace, 4) of
        undefined ->
            %% Also try the reason itself (may be nested)
            case extract_beamtalk_error(Reason, 3) of
                undefined -> format_safe(Reason);
                Error -> format_safe(Error)
            end;
        Error ->
            format_safe(Error)
    end.

%% @doc Recursively search a term for a `#beamtalk_error{}` record.
%% Depth-limited to avoid traversing enormous state maps.
-spec extract_beamtalk_error(term(), non_neg_integer()) -> #beamtalk_error{} | undefined.
%% Direct matches succeed at any depth — depth only limits further recursion
extract_beamtalk_error(#beamtalk_error{} = E, _Depth) ->
    E;
extract_beamtalk_error(#{error := #beamtalk_error{} = E}, _Depth) ->
    E;
extract_beamtalk_error(_Term, 0) ->
    undefined;
extract_beamtalk_error(T, Depth) when is_tuple(T) ->
    extract_from_tuple(T, 1, tuple_size(T), Depth - 1);
extract_beamtalk_error([H | T], Depth) ->
    case extract_beamtalk_error(H, Depth - 1) of
        undefined -> extract_beamtalk_error(T, Depth);
        Found -> Found
    end;
extract_beamtalk_error(_, _Depth) ->
    undefined.

%% @private Scan tuple elements for a beamtalk_error.
-spec extract_from_tuple(tuple(), pos_integer(), non_neg_integer(), non_neg_integer()) ->
    #beamtalk_error{} | undefined.
extract_from_tuple(_T, I, Size, _Depth) when I > Size ->
    undefined;
extract_from_tuple(T, I, Size, Depth) ->
    case extract_beamtalk_error(element(I, T), Depth) of
        undefined -> extract_from_tuple(T, I + 1, Size, Depth);
        Found -> Found
    end.

%% @private Format message with optional hint.
-spec format_parts(binary(), binary() | undefined) -> binary().
format_parts(Message, undefined) ->
    Message;
format_parts(Message, Hint) ->
    iolist_to_binary([Message, <<"\nHint: ">>, Hint]).

%% @doc Wrap an error as an Exception object and raise it.
%%
%% This is the canonical way to signal errors in the Beamtalk runtime.
%% It wraps the `#beamtalk_error{}` record as an Exception tagged map
%% before throwing, ensuring all exceptions are Beamtalk objects at
%% signal time (ADR 0015).
%%
%% Example:
%%   Error = beamtalk_error:new(does_not_understand, 'Integer'),
%%   beamtalk_error:raise(Error)
-spec raise(#beamtalk_error{}) -> no_return().
raise(#beamtalk_error{} = Error) ->
    Wrapped = beamtalk_exception_handler:wrap(Error),
    error(Wrapped).

%% @doc Generate a human-readable error message from kind, class, and optional selector.
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
generate_message(actor_dead, Class, undefined) ->
    iolist_to_binary(io_lib:format("~s actor process has terminated", [Class]));
generate_message(actor_dead, Class, Selector) ->
    iolist_to_binary(
        io_lib:format("Cannot send '~s' to ~s (actor process has terminated)", [Selector, Class])
    );
generate_message(future_not_awaited, _Class, undefined) ->
    iolist_to_binary(io_lib:format("Sent message to a Future", []));
generate_message(future_not_awaited, _Class, Selector) ->
    iolist_to_binary(io_lib:format("Sent '~s' to a Future", [Selector]));
generate_message(instantiation_error, Class, undefined) ->
    iolist_to_binary(io_lib:format("Cannot instantiate ~s", [Class]));
generate_message(instantiation_error, Class, Selector) ->
    iolist_to_binary(io_lib:format("Cannot call '~s' on ~s", [Selector, Class]));
generate_message(file_not_found, Class, undefined) ->
    iolist_to_binary(io_lib:format("~s: file not found", [Class]));
generate_message(file_not_found, Class, Selector) ->
    iolist_to_binary(io_lib:format("~s '~s': file not found", [Class, Selector]));
generate_message(permission_denied, Class, undefined) ->
    iolist_to_binary(io_lib:format("~s: permission denied", [Class]));
generate_message(permission_denied, Class, Selector) ->
    iolist_to_binary(io_lib:format("~s '~s': permission denied", [Class, Selector]));
generate_message(io_error, Class, undefined) ->
    iolist_to_binary(io_lib:format("~s: I/O error", [Class]));
generate_message(io_error, Class, Selector) ->
    iolist_to_binary(io_lib:format("~s '~s': I/O error", [Class, Selector]));
generate_message(user_error, _Class, undefined) ->
    <<"Error">>;
generate_message(user_error, _Class, Selector) ->
    iolist_to_binary(io_lib:format("~p", [Selector]));
generate_message(stdlib_shadowing, Class, undefined) ->
    iolist_to_binary(io_lib:format("Cannot redefine stdlib class '~s'", [Class]));
generate_message(stdlib_shadowing, Class, Selector) ->
    iolist_to_binary(
        io_lib:format("Cannot redefine stdlib class '~s' (via '~s')", [Class, Selector])
    );
generate_message(class_not_found, Class, undefined) ->
    iolist_to_binary(io_lib:format("Class '~s' not found", [Class]));
generate_message(class_not_found, Class, Selector) ->
    iolist_to_binary(
        io_lib:format("Class '~s' not found (while resolving '~s')", [Class, Selector])
    );
generate_message(no_superclass, Class, undefined) ->
    iolist_to_binary(io_lib:format("~s has no superclass", [Class]));
generate_message(no_superclass, Class, Selector) ->
    iolist_to_binary(
        io_lib:format("~s has no superclass (cannot resolve '~s' via super)", [Class, Selector])
    );
generate_message(class_already_exists, Class, undefined) ->
    iolist_to_binary(io_lib:format("Class '~s' already exists", [Class]));
generate_message(class_already_exists, Class, Selector) ->
    iolist_to_binary(io_lib:format("Class '~s' already exists (via '~s')", [Class, Selector]));
generate_message(internal_error, Class, undefined) ->
    iolist_to_binary(io_lib:format("Internal error in ~s", [Class]));
generate_message(internal_error, Class, Selector) ->
    iolist_to_binary(io_lib:format("Internal error in '~s' on ~s", [Selector, Class]));
generate_message(dispatch_error, Class, undefined) ->
    iolist_to_binary(io_lib:format("Dispatch error for ~s", [Class]));
generate_message(dispatch_error, Class, Selector) ->
    iolist_to_binary(io_lib:format("Dispatch error for '~s' on ~s", [Selector, Class]));
generate_message(callback_failed, Class, undefined) ->
    iolist_to_binary(io_lib:format("Callback failed for ~s", [Class]));
generate_message(callback_failed, Class, Selector) ->
    iolist_to_binary(io_lib:format("Callback '~s' failed for ~s", [Selector, Class]));
generate_message(Kind, Class, undefined) ->
    iolist_to_binary(io_lib:format("~s error in ~s", [Kind, Class]));
generate_message(Kind, Class, Selector) ->
    iolist_to_binary(io_lib:format("~s error in '~s' on ~s", [Kind, Selector, Class])).

%% @private Enrich DNU hint when the selector is a class-side message.
%%
%% If the instance doesn't understand a selector but the class hierarchy does
%% (e.g., `methods`, `allMethods`, `superclass`), suggest using `class` to
%% access it. Degrades gracefully if the class registry is unavailable.
-spec maybe_enrich_dnu_hint(#beamtalk_error{}) -> #beamtalk_error{}.
maybe_enrich_dnu_hint(#beamtalk_error{class = Class, selector = Selector} = Error) when
    is_atom(Class), is_atom(Selector), Selector =/= undefined
->
    try beamtalk_dispatch:responds_to(Selector, 'Class') of
        true ->
            SelBin = atom_to_binary(Selector, utf8),
            Hint = iolist_to_binary([
                <<"'">>,
                SelBin,
                <<"' is a class-side message — use 'class ">>,
                SelBin,
                <<"' to send it to an instance's class">>
            ]),
            Error#beamtalk_error{hint = Hint};
        false ->
            Error
    catch
        _:_ -> Error
    end;
maybe_enrich_dnu_hint(Error) ->
    Error.

%% @doc Format an exception class + reason as a compact human-readable binary.
%%
%% Extracts the message from #beamtalk_error{} records and Exception wrappers
%% instead of dumping raw Erlang tuples. Used by debug-level logging in
%% dispatch catch blocks to avoid verbose ~tp output.
-spec format_reason(atom(), term()) -> binary().
format_reason(_ErrClass, #beamtalk_error{kind = Kind, class = Class, message = Msg}) ->
    iolist_to_binary(io_lib:format("~s (~s:~s)", [Msg, Class, Kind]));
format_reason(_ErrClass, #{'$beamtalk_class' := _, error := #beamtalk_error{} = Err}) ->
    format_reason(error, Err);
format_reason(ErrClass, Reason) ->
    iolist_to_binary(io_lib:format("~p:~tp", [ErrClass, Reason])).
