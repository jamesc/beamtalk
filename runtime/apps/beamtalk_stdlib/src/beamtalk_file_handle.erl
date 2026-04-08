%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_file_handle).

%%% **DDD Context:** Object System Context

-moduledoc """
FileHandle instance-side dispatch (BT-513, BT-871, BT-1762).

Provides low-level dispatch for `FileHandle` tagged-map instances.
The `lines` selector is handled directly; Object protocol selectors
delegate to `beamtalk_object_ops`. Unknown selectors are handled by
the compiled `bt@stdlib@file_handle` module via Object inheritance.

See also: beamtalk_file for File class-side methods (`exists:`, `readAll:`, etc.)
""".

-export([dispatch/3, has_method/1]).

-doc """
Dispatch a message to a FileHandle instance.

Handles the `lines` selector directly and falls through to the base
Object protocol for everything else.
""".
-spec dispatch(atom(), list(), map()) -> term().
dispatch('lines', [], X) ->
    beamtalk_file:handle_lines(X);
dispatch(Selector, Args, X) ->
    case beamtalk_object_ops:has_method(Selector) of
        true ->
            case beamtalk_object_ops:dispatch(Selector, Args, X, X) of
                {reply, Result, _State} -> Result;
                {error, Error, _State} -> beamtalk_error:raise(Error)
            end
    end.

-doc "Check if a FileHandle responds to the given selector.".
-spec has_method(atom()) -> boolean().
has_method(Selector) ->
    beamtalk_file:handle_has_method(Selector) orelse beamtalk_object_ops:has_method(Selector).
