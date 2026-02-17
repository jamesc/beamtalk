%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc ErlangModule proxy dispatch for BEAM interop (BT-676, BT-679).
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
%%% **Export introspection (BT-679):**
%%% Before dispatch, validates function existence and arity via
%%% `Module:module_info(exports)`. Provides actionable error messages for:
%%% - Wrong arity: "lists:reverse/1 exists but was called with 2 arguments"
%%% - Missing function: "This Erlang function does not exist."
%%% - Unloaded module: "Erlang module 'bogus' is not loaded."
%%%
%%% **Escape hatch:** `call:args:` bypasses selector→function mapping
%%% for reserved selectors (class, ==, /=, self, etc.).
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
%% Handles Object protocol messages (class, printString, methods) locally,
%% validates export existence/arity before dispatch, and provides
%% actionable error messages (BT-679).
-spec dispatch(atom(), list(), map()) -> term().
dispatch('class', _Args, _Self) ->
    'ErlangModule';
dispatch('printString', _Args, Self) ->
    Module = maps:get(module, Self),
    iolist_to_binary([<<"#ErlangModule<">>, atom_to_binary(Module, utf8), <<">">>]);
dispatch('methods', _Args, Self) ->
    %% BT-679: REPL discoverability — return exported functions with arities
    Module = maps:get(module, Self),
    case get_exports(Module) of
        {ok, Exports} ->
            %% Format as list of "function/arity" strings
            lists:sort([iolist_to_binary([atom_to_binary(F, utf8), <<"/">>,
                                         integer_to_binary(A)])
                        || {F, A} <- Exports, F =/= module_info]);
        {error, not_loaded} ->
            raise_module_not_loaded(Module, 'methods')
    end;
dispatch('call:args:', [SelectorSym, ArgsTuple], Self) when is_atom(SelectorSym), is_tuple(ArgsTuple) ->
    %% BT-679: Escape hatch for reserved selectors (class, ==, /=, self, etc.)
    Module = maps:get(module, Self),
    Args = tuple_to_list(ArgsTuple),
    validate_and_apply(Module, SelectorSym, Args, 'call:args:');
dispatch('call:args:', [SelectorSym, ArgsTuple], _Self) ->
    %% Type validation for call:args: arguments
    case {is_atom(SelectorSym), is_tuple(ArgsTuple)} of
        {false, _} ->
            Error0 = beamtalk_error:new(type_error, 'ErlangModule'),
            Error1 = beamtalk_error:with_selector(Error0, 'call:args:'),
            Error2 = beamtalk_error:with_hint(Error1,
                <<"call:args: first argument must be a Symbol (function name)">>),
            beamtalk_error:raise(Error2);
        {_, false} ->
            Error0 = beamtalk_error:new(type_error, 'ErlangModule'),
            Error1 = beamtalk_error:with_selector(Error0, 'call:args:'),
            Error2 = beamtalk_error:with_hint(Error1,
                <<"call:args: second argument must be a Tuple of arguments">>),
            beamtalk_error:raise(Error2)
    end;
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
                    validate_and_apply(Module, FunName, Args, Selector);
                {error, FunStr} ->
                    %% Function name not in atom table — cannot be an export
                    Error0 = beamtalk_error:new(does_not_understand, 'ErlangModule'),
                    Error1 = beamtalk_error:with_selector(Error0, Selector),
                    Hint = iolist_to_binary([
                        <<"This Erlang function does not exist. Check spelling and arity.">>
                    ]),
                    Error2 = beamtalk_error:with_hint(Error1, Hint),
                    Error3 = beamtalk_error:with_details(Error2, #{
                        module => Module,
                        function => FunStr,
                        arity => length(Args)
                    }),
                    beamtalk_error:raise(Error3)
            end
    end.

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

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

%% @doc Get the exports for a module, checking if it's loaded first.
%%
%% Returns {ok, [{Function, Arity}]} or {error, not_loaded}.
-spec get_exports(atom()) -> {ok, [{atom(), non_neg_integer()}]} | {error, not_loaded}.
get_exports(Module) ->
    try
        {ok, Module:module_info(exports)}
    catch
        error:undef -> {error, not_loaded}
    end.

%% @doc Validate function existence and arity, then apply.
%%
%% Checks module_info(exports) before calling erlang:apply/3 to provide
%% actionable error messages (BT-679). No caching — hot code reload must work.
-spec validate_and_apply(atom(), atom(), list(), atom()) -> term().
validate_and_apply(Module, FunName, Args, OrigSelector) ->
    Arity = length(Args),
    case get_exports(Module) of
        {error, not_loaded} ->
            raise_module_not_loaded(Module, OrigSelector);
        {ok, Exports} ->
            case lists:member({FunName, Arity}, Exports) of
                true ->
                    %% Exact match — call directly
                    try
                        erlang:apply(Module, FunName, Args)
                    catch
                        error:#{error := _} = Wrapped ->
                            %% Already a beamtalk error — re-raise as-is
                            error(Wrapped);
                        error:badarg ->
                            Error0 = beamtalk_error:new(type_error, 'ErlangModule'),
                            Error1 = beamtalk_error:with_selector(Error0, OrigSelector),
                            Error2 = beamtalk_error:with_hint(Error1, <<"Bad argument in Erlang call">>),
                            beamtalk_error:raise(Error2);
                        error:Reason ->
                            Error0 = beamtalk_error:new(type_error, 'ErlangModule'),
                            Error1 = beamtalk_error:with_selector(Error0, OrigSelector),
                            ReasonBin = iolist_to_binary(io_lib:format("~p", [Reason])),
                            Hint2 = iolist_to_binary([
                                <<"Erlang error in ">>,
                                atom_to_binary(Module, utf8), <<":">>,
                                atom_to_binary(FunName, utf8), <<": ">>,
                                ReasonBin
                            ]),
                            Error2 = beamtalk_error:with_hint(Error1, Hint2),
                            beamtalk_error:raise(Error2)
                    end;
                false ->
                    %% Function/arity not found — check if function exists with different arity
                    MatchingArities = [A || {F, A} <- Exports, F =:= FunName],
                    case MatchingArities of
                        [] ->
                            %% Function doesn't exist at all
                            Error0 = beamtalk_error:new(does_not_understand, 'ErlangModule'),
                            Error1 = beamtalk_error:with_selector(Error0, OrigSelector),
                            Error2 = beamtalk_error:with_hint(Error1,
                                <<"This Erlang function does not exist. Check spelling and arity.">>),
                            Error3 = beamtalk_error:with_details(Error2, #{
                                module => Module,
                                function => FunName,
                                arity => Arity
                            }),
                            beamtalk_error:raise(Error3);
                        _ ->
                            %% Function exists but with different arity
                            AritiesStr = lists:join(<<", ">>,
                                [integer_to_binary(A) || A <- lists:sort(MatchingArities)]),
                            Hint = iolist_to_binary([
                                atom_to_binary(Module, utf8), <<":">>,
                                atom_to_binary(FunName, utf8), <<"/">>,
                                iolist_to_binary(AritiesStr),
                                <<" exists but was called with ">>,
                                integer_to_binary(Arity),
                                <<" arguments">>
                            ]),
                            Error0 = beamtalk_error:new(arity_mismatch, 'ErlangModule'),
                            Error1 = beamtalk_error:with_selector(Error0, OrigSelector),
                            Error2 = beamtalk_error:with_hint(Error1, Hint),
                            Error3 = beamtalk_error:with_details(Error2, #{
                                module => Module,
                                function => FunName,
                                expected_arities => lists:sort(MatchingArities),
                                actual_arity => Arity
                            }),
                            beamtalk_error:raise(Error3)
                    end
            end
    end.

%% @doc Raise error for unloaded module.
-spec raise_module_not_loaded(atom(), atom()) -> no_return().
raise_module_not_loaded(Module, Selector) ->
    Hint = iolist_to_binary([
        <<"Erlang module '">>,
        atom_to_binary(Module, utf8),
        <<"' is not loaded. Is it on the code path?">>
    ]),
    Error0 = beamtalk_error:new(does_not_understand, 'ErlangModule'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    Error3 = beamtalk_error:with_details(Error2, #{module => Module}),
    beamtalk_error:raise(Error3).
