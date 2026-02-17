%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc ErlangModule proxy dispatch for BEAM interop (BT-676).
%%%
%%% Forwards Beamtalk messages to Erlang function calls via `erlang:apply/3`.
%%% ErlangModule proxies are tagged maps of the form:
%%% ```
%%% #{
%%%   '$beamtalk_class' => 'ErlangModule',
%%%   module => lists    % the Erlang module to proxy
%%% }
%%% ```
%%%
%%% **Dispatch rules:**
%%% - Keyword selectors (ending in `:`) → strip colon, use as function name
%%%   `reverse:` + [Arg] → `lists:reverse(Arg)`
%%% - Multi-keyword selectors → first keyword = function name, all args positional
%%%   `sublist:length:` + [Start, Len] → `lists:sublist(Start, Len)`
%%% - Unary selectors (no colon) → zero-arg function call
%%%   `node` → `erlang:node()`
%%%
%%% **DDD Context:** Runtime — BEAM Interop
%%%
%%% See: ADR 0028 §1 (Module Proxy Pattern)

-module(beamtalk_erlang_proxy).

-export([dispatch/3, new/1]).

-include("beamtalk.hrl").

%% @doc Create a new ErlangModule proxy for the given module atom.
-spec new(atom()) -> map().
new(Module) when is_atom(Module) ->
    #{'$beamtalk_class' => 'ErlangModule', module => Module}.

%% @doc Dispatch a Beamtalk message to the proxied Erlang module.
%%
%% Handles Object protocol messages (class, printString) locally,
%% then delegates everything else to erlang:apply/3.
-spec dispatch(atom(), list(), map()) -> term().
dispatch('class', _Args, _Self) ->
    'ErlangModule';
dispatch('printString', _Args, Self) ->
    Module = maps:get(module, Self),
    iolist_to_binary([<<"#ErlangModule<">>, atom_to_binary(Module, utf8), <<">">>]);
dispatch(Selector, Args, Self) ->
    Module = maps:get(module, Self),
    %% Check Object protocol methods first
    case beamtalk_object_ops:has_method(Selector) of
        true ->
            case beamtalk_object_ops:dispatch(Selector, Args, Self, Self) of
                {reply, Result, _State} -> Result;
                {error, Error, _State} -> beamtalk_error:raise(Error)
            end;
        false ->
            FunName = selector_to_function(Selector),
            try
                erlang:apply(Module, FunName, Args)
            catch
                error:undef ->
                    Error0 = beamtalk_error:new(does_not_understand, 'ErlangModule'),
                    Error1 = beamtalk_error:with_selector(Error0, Selector),
                    Hint = iolist_to_binary([
                        atom_to_binary(Module, utf8), <<":">>,
                        atom_to_binary(FunName, utf8), <<"/">>,
                        integer_to_binary(length(Args)),
                        <<" is not exported or does not exist">>
                    ]),
                    Error2 = beamtalk_error:with_hint(Error1, Hint),
                    beamtalk_error:raise(Error2);
                error:badarg ->
                    Error0 = beamtalk_error:new(type_error, 'ErlangModule'),
                    Error1 = beamtalk_error:with_selector(Error0, Selector),
                    Error2 = beamtalk_error:with_hint(Error1, <<"Bad argument in Erlang call">>),
                    beamtalk_error:raise(Error2);
                error:Reason ->
                    Error0 = beamtalk_error:new(type_error, 'ErlangModule'),
                    Error1 = beamtalk_error:with_selector(Error0, Selector),
                    ReasonBin = iolist_to_binary(io_lib:format("~p", [Reason])),
                    Hint2 = iolist_to_binary([
                        <<"Erlang error in ">>,
                        atom_to_binary(Module, utf8), <<":">>,
                        atom_to_binary(FunName, utf8), <<": ">>,
                        ReasonBin
                    ]),
                    Error2 = beamtalk_error:with_hint(Error1, Hint2),
                    beamtalk_error:raise(Error2)
            end
    end.

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

%% @doc Convert a Beamtalk selector atom to an Erlang function name.
%%
%% Keyword selectors like `'reverse:'` → `reverse`
%% Multi-keyword like `'sublist:length:'` → `sublist`  (first keyword only)
%% Unary selectors like `'node'` → `node`
-spec selector_to_function(atom()) -> atom().
selector_to_function(Selector) ->
    SelectorStr = atom_to_list(Selector),
    case lists:member($:, SelectorStr) of
        true ->
            %% Keyword selector — extract first keyword (before first colon)
            [FunStr | _] = string:split(SelectorStr, ":"),
            try list_to_existing_atom(FunStr)
            catch error:badarg ->
                %% Function name not in atom table — cannot exist as exported function
                list_to_atom(FunStr)
            end;
        false ->
            %% Unary selector — use as-is
            Selector
    end.
