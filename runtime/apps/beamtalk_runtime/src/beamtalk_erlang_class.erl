%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Erlang class-side dispatch (BT-676, BT-871).
%%%
%%% **DDD Context:** Runtime — BEAM Interop
%%%
%%% Dispatches messages to the `Erlang` class object — a tagged map used as
%%% the entry point for Erlang module interop. Unary selectors become
%%% ErlangModule proxy lookups (`Erlang lists` → `ErlangModule` proxy for
%%% `lists`).
%%%
%%% Implements the compiled stdlib dispatch interface (`dispatch/3`,
%%% `has_method/1`) for `Erlang` tagged-map instances, enabling registration
%%% in `module_for_value/1` without special-casing in `beamtalk_primitive`.
%%%
%%% @see beamtalk_erlang_proxy for ErlangModule instance dispatch

-module(beamtalk_erlang_class).

-export([dispatch/3, has_method/1]).

-include("beamtalk.hrl").

%% @doc Dispatch a message to the Erlang class object.
%%
%% Unary selectors (no colon, no args) resolve to ErlangModule proxies.
%% Keyword selectors and multi-arg calls raise errors.
%% Object protocol messages are handled via beamtalk_object_ops.
-spec dispatch(atom(), list(), map()) -> term().
dispatch('class', _Args, _Self) ->
    'Erlang';
dispatch('printString', _Args, _Self) ->
    <<"Erlang">>;
dispatch(Selector, Args, Self) ->
    case beamtalk_object_ops:has_method(Selector) of
        true ->
            case beamtalk_object_ops:dispatch(Selector, Args, Self, Self) of
                {reply, Result, _State} -> Result;
                {error, Error, _State} -> beamtalk_error:raise(Error)
            end;
        false ->
            case lists:member($:, atom_to_list(Selector)) of
                true ->
                    beamtalk_error:raise(
                        beamtalk_error:new(
                            does_not_understand,
                            'Erlang',
                            Selector,
                            <<"Use unary message for module name: Erlang moduleName">>
                        )
                    );
                false when Args =/= [] ->
                    beamtalk_error:raise(
                        beamtalk_error:new(
                            arity_mismatch,
                            'Erlang',
                            Selector,
                            <<"Module lookup takes no arguments: Erlang moduleName">>
                        )
                    );
                false ->
                    beamtalk_erlang_proxy:new(Selector)
            end
    end.

%% @doc Check if Erlang class responds to the given selector.
%%
%% Always returns true — any unary selector is a valid module lookup.
-spec has_method(atom()) -> boolean().
has_method(_Selector) -> true.
