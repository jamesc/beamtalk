%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc FileHandle instance-side dispatch (BT-513, BT-871).
%%%
%%% **DDD Context:** Runtime Context
%%%
%%% Implements the compiled stdlib dispatch interface (`dispatch/3`,
%%% `has_method/1`) for `FileHandle` tagged-map instances, enabling
%%% registration in `module_for_value/1` without special-casing in
%%% `beamtalk_primitive`.
%%%
%%% @see beamtalk_file for File class-side methods (`exists:`, `readAll:`, etc.)

-module(beamtalk_file_handle).

-export([dispatch/3, has_method/1]).

-include("beamtalk.hrl").

%% @doc Dispatch a message to a FileHandle instance.
%%
%% Handles the `lines` selector directly, falls through to the base
%% Object protocol for everything else, and raises DNU if unhandled.
-spec dispatch(atom(), list(), map()) -> term().
dispatch('lines', [], X) ->
    beamtalk_file:handle_lines(X);
dispatch(Selector, Args, X) ->
    case beamtalk_object_ops:has_method(Selector) of
        true ->
            case beamtalk_object_ops:dispatch(Selector, Args, X, X) of
                {reply, Result, _State} -> Result;
                {error, Error, _State} -> beamtalk_error:raise(Error)
            end;
        false ->
            beamtalk_error:raise(
                beamtalk_error:new(
                    does_not_understand,
                    'FileHandle',
                    Selector,
                    <<"FileHandle does not understand this message">>
                )
            )
    end.

%% @doc Check if a FileHandle responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method(Selector) ->
    beamtalk_file:handle_has_method(Selector) orelse beamtalk_object_ops:has_method(Selector).
