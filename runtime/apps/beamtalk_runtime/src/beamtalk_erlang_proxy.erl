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
            case selector_to_function(Selector) of
                {ok, FunName} ->
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
                            beamtalk_error:raise(
                                beamtalk_error:with_details(Error2,
                                    #{erlang_error => undef}));
                        error:badarg ->
                            Error0 = beamtalk_error:new(type_error, 'ErlangModule'),
                            Error1 = beamtalk_error:with_selector(Error0, Selector),
                            Error2 = beamtalk_error:with_hint(Error1,
                                erlang_error_hint(Module, FunName, badarg)),
                            beamtalk_error:raise(
                                beamtalk_error:with_details(Error2,
                                    #{erlang_error => badarg}));
                        error:function_clause ->
                            Error0 = beamtalk_error:new(arity_mismatch, 'ErlangModule'),
                            Error1 = beamtalk_error:with_selector(Error0, Selector),
                            Error2 = beamtalk_error:with_hint(Error1,
                                erlang_error_hint(Module, FunName, function_clause)),
                            beamtalk_error:raise(
                                beamtalk_error:with_details(Error2,
                                    #{erlang_error => function_clause}));
                        error:badarith ->
                            Error0 = beamtalk_error:new(type_error, 'ErlangModule'),
                            Error1 = beamtalk_error:with_selector(Error0, Selector),
                            Error2 = beamtalk_error:with_hint(Error1,
                                erlang_error_hint(Module, FunName, badarith)),
                            beamtalk_error:raise(
                                beamtalk_error:with_details(Error2,
                                    #{erlang_error => badarith}));
                        error:Reason ->
                            Error0 = beamtalk_error:new(runtime_error, 'ErlangModule'),
                            Error1 = beamtalk_error:with_selector(Error0, Selector),
                            Error2 = beamtalk_error:with_hint(Error1,
                                erlang_error_hint(Module, FunName, Reason)),
                            beamtalk_error:raise(
                                beamtalk_error:with_details(Error2,
                                    #{erlang_error => Reason}));
                        exit:Reason ->
                            Error0 = beamtalk_error:new(erlang_exit, 'ErlangModule'),
                            Error1 = beamtalk_error:with_selector(Error0, Selector),
                            ReasonBin = iolist_to_binary(io_lib:format("~p", [Reason])),
                            Hint = iolist_to_binary([
                                <<"Erlang process exit in ">>,
                                atom_to_binary(Module, utf8), <<":">>,
                                atom_to_binary(FunName, utf8), <<": ">>,
                                ReasonBin
                            ]),
                            Error2 = beamtalk_error:with_hint(Error1, Hint),
                            beamtalk_error:raise(
                                beamtalk_error:with_details(Error2,
                                    #{erlang_exit_reason => Reason}));
                        throw:Value ->
                            Error0 = beamtalk_error:new(erlang_throw, 'ErlangModule'),
                            Error1 = beamtalk_error:with_selector(Error0, Selector),
                            ValueBin = iolist_to_binary(io_lib:format("~p", [Value])),
                            Hint = iolist_to_binary([
                                <<"Erlang throw in ">>,
                                atom_to_binary(Module, utf8), <<":">>,
                                atom_to_binary(FunName, utf8), <<": ">>,
                                ValueBin
                            ]),
                            Error2 = beamtalk_error:with_hint(Error1, Hint),
                            beamtalk_error:raise(
                                beamtalk_error:with_details(Error2,
                                    #{erlang_throw_value => Value}))
                    end;
                {error, FunStr} ->
                    Error0 = beamtalk_error:new(does_not_understand, 'ErlangModule'),
                    Error1 = beamtalk_error:with_selector(Error0, Selector),
                    Hint = iolist_to_binary([
                        atom_to_binary(Module, utf8), <<":">>,
                        FunStr, <<"/">>,
                        integer_to_binary(length(Args)),
                        <<" is not exported or does not exist">>
                    ]),
                    Error2 = beamtalk_error:with_hint(Error1, Hint),
                    beamtalk_error:raise(Error2)
            end
    end.

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

%% @doc Format an actionable hint for an Erlang error.
-spec erlang_error_hint(atom(), atom(), term()) -> binary().
erlang_error_hint(Module, FunName, Reason) ->
    ReasonBin = iolist_to_binary(io_lib:format("~p", [Reason])),
    iolist_to_binary([
        <<"Erlang error in ">>,
        atom_to_binary(Module, utf8), <<":">>,
        atom_to_binary(FunName, utf8), <<": ">>,
        ReasonBin
    ]).

%% @doc Convert a Beamtalk selector atom to an Erlang function name.
%%
%% Returns {ok, Atom} when the function name is a known atom,
%% or {error, Binary} when it's not in the atom table (cannot exist as
%% an exported function). This avoids creating new atoms for unknown selectors.
%%
%% Keyword selectors like `'reverse:'` → `{ok, reverse}`
%% Multi-keyword like `'sublist:length:'` → `{ok, sublist}`  (first keyword only)
%% Unary selectors like `'node'` → `{ok, node}`
-spec selector_to_function(atom()) -> {ok, atom()} | {error, binary()}.
selector_to_function(Selector) ->
    SelectorStr = atom_to_list(Selector),
    case lists:member($:, SelectorStr) of
        true ->
            %% Keyword selector — extract first keyword (before first colon)
            [FunStr | _] = string:split(SelectorStr, ":"),
            try {ok, list_to_existing_atom(FunStr)}
            catch error:badarg ->
                {error, list_to_binary(FunStr)}
            end;
        false ->
            %% Unary selector — use as-is (already an atom)
            {ok, Selector}
    end.
