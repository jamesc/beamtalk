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
%%% **DDD Context:** Object System Context
%%%
%%% See: ADR 0028 §1 (Module Proxy Pattern)

-module(beamtalk_erlang_proxy).

-export([direct_call/3, dispatch/3, has_method/1, new/1]).

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
            lists:sort([
                iolist_to_binary([
                    atom_to_binary(F, utf8),
                    <<"/">>,
                    integer_to_binary(A)
                ])
             || {F, A} <- Exports, F =/= module_info
            ]);
        {error, not_loaded} ->
            raise_module_not_loaded(Module, 'methods')
    end;
dispatch('call:args:', [SelectorSym, ArgsTuple], Self) when
    is_atom(SelectorSym), is_tuple(ArgsTuple)
->
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
            Error2 = beamtalk_error:with_hint(
                Error1,
                <<"call:args: first argument must be a Symbol (function name)">>
            ),
            beamtalk_error:raise(Error2);
        {_, false} ->
            Error0 = beamtalk_error:new(type_error, 'ErlangModule'),
            Error1 = beamtalk_error:with_selector(Error0, 'call:args:'),
            Error2 = beamtalk_error:with_hint(
                Error1,
                <<"call:args: second argument must be a Tuple of arguments">>
            ),
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
            %% Ensure the module is loaded so its exported function atoms are
            %% in the atom table before selector_to_function/1 calls
            %% list_to_existing_atom/1. New FFI shim functions (e.g. isDirectory/1
            %% in beamtalk_file) only enter the atom table when the module loads.
            _ = code:ensure_loaded(Module),
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

%% @doc Check if an ErlangModule responds to the given selector.
%%
%% Always returns true — any selector may map to an Erlang function call.
%% Actual validation (arity, existence) happens at dispatch time.
-spec has_method(atom()) -> boolean().
has_method(_Selector) -> true.

%% @doc Entry point for direct Erlang calls (BT-1127).
%%
%% The codegen emits `call 'beamtalk_erlang_proxy':'direct_call'(M, F, [args])`
%% for all `Erlang M fn: arg` expressions, routing through the proxy instead
%% of calling `call 'M':'F'(args)` directly. This enables:
%% - Export validation and actionable error messages (BT-679)
%% - Automatic binary→charlist coercion on badarg (BT-1127)
%% - Charlist→binary result coercion for consistent string types
%%
%% Unlike `validate_and_apply/4`, this does NOT wrap exit/throw exceptions —
%% those propagate naturally so `Erlang erlang exit: 1` behaves as expected.
-spec direct_call(atom(), atom(), list()) -> term().
direct_call(Module, FunName, Args) ->
    Arity = length(Args),
    case get_exports(Module) of
        {error, not_loaded} ->
            raise_module_not_loaded(Module, FunName);
        {ok, Exports} ->
            case lists:member({FunName, Arity}, Exports) of
                true ->
                    apply_with_coercion(Module, FunName, Args, FunName);
                false ->
                    raise_function_or_arity_error(
                        Module, FunName, Arity, FunName, Exports
                    )
            end
    end.

%% @doc Call Module:FunName(Args), retrying with charlist-coerced args on badarg.
%%
%% Only catches badarg (for coercion retry). All other exceptions (exit, throw,
%% and other errors) propagate naturally — this preserves the semantics of
%% direct calls like `Erlang erlang exit: 1`.
-spec apply_with_coercion(atom(), atom(), list(), atom()) -> term().
apply_with_coercion(Module, FunName, Args, OrigSelector) ->
    try
        Result = erlang:apply(Module, FunName, Args),
        coerce_charlist_result(Result)
    catch
        error:#{error := #beamtalk_error{}} = Wrapped:_ ->
            error(Wrapped);
        error:undef:_Stack ->
            %% TOCTOU: function unloaded between export check and apply
            raise_undef_error(Module, FunName, length(Args), OrigSelector);
        error:badarg:Stack ->
            CoercedArgs = coerce_binaries_to_charlists(Args),
            case CoercedArgs =/= Args of
                true ->
                    try
                        RetryResult = erlang:apply(Module, FunName, CoercedArgs),
                        coerce_charlist_result(RetryResult)
                    catch
                        error:badarg:Stack2 ->
                            raise_badarg_error(Module, FunName, OrigSelector, Stack2);
                        error:#{error := #beamtalk_error{}} = Wrapped2:_ ->
                            error(Wrapped2);
                        error:undef:_Stack2 ->
                            raise_undef_error(Module, FunName, length(CoercedArgs), OrigSelector)
                    end;
                false ->
                    raise_badarg_error(Module, FunName, OrigSelector, Stack)
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
        atom_to_binary(Module, utf8),
        <<":">>,
        atom_to_binary(FunName, utf8),
        <<": ">>,
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
            try
                {ok, list_to_existing_atom(FunStr)}
            catch
                error:badarg ->
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
                        error:#{error := #beamtalk_error{}} = Wrapped:_Stack ->
                            %% Re-raise already-wrapped Beamtalk exceptions
                            %% (e.g., from Beamtalk code called via Erlang)
                            error(Wrapped);
                        error:undef:Stack ->
                            %% Unlikely with export validation (BT-679), but
                            %% possible during hot code reload race conditions
                            Error0 = beamtalk_error:new(does_not_understand, 'ErlangModule'),
                            Error1 = beamtalk_error:with_selector(Error0, OrigSelector),
                            Error2 = beamtalk_error:with_hint(
                                Error1,
                                erlang_error_hint(Module, FunName, undef)
                            ),
                            beamtalk_error:raise(
                                beamtalk_error:with_details(
                                    Error2,
                                    #{
                                        erlang_error => undef,
                                        erlang_stacktrace => Stack
                                    }
                                )
                            );
                        error:badarg:Stack ->
                            %% BT-1127: Auto-coerce binary args to charlists and retry.
                            %% Many Erlang functions (os:cmd/1, file:read_file/1, etc.)
                            %% expect charlists but Beamtalk strings are binaries.
                            CoercedArgs = coerce_binaries_to_charlists(Args),
                            case CoercedArgs =/= Args of
                                true ->
                                    try
                                        Result = erlang:apply(Module, FunName, CoercedArgs),
                                        coerce_charlist_result(Result)
                                    catch
                                        error:badarg:Stack2 ->
                                            raise_badarg_error(
                                                Module, FunName, OrigSelector, Stack2
                                            );
                                        error:#{error := #beamtalk_error{}} = Wrapped:_Stack2 ->
                                            error(Wrapped);
                                        error:undef:_Stack2 ->
                                            raise_undef_error(
                                                Module,
                                                FunName,
                                                length(CoercedArgs),
                                                OrigSelector
                                            )
                                    end;
                                false ->
                                    raise_badarg_error(Module, FunName, OrigSelector, Stack)
                            end;
                        error:function_clause:Stack ->
                            Error0 = beamtalk_error:new(arity_mismatch, 'ErlangModule'),
                            Error1 = beamtalk_error:with_selector(Error0, OrigSelector),
                            Error2 = beamtalk_error:with_hint(
                                Error1,
                                erlang_error_hint(Module, FunName, function_clause)
                            ),
                            beamtalk_error:raise(
                                beamtalk_error:with_details(
                                    Error2,
                                    #{
                                        erlang_error => function_clause,
                                        erlang_stacktrace => Stack
                                    }
                                )
                            );
                        error:badarith:Stack ->
                            Error0 = beamtalk_error:new(type_error, 'ErlangModule'),
                            Error1 = beamtalk_error:with_selector(Error0, OrigSelector),
                            Error2 = beamtalk_error:with_hint(
                                Error1,
                                erlang_error_hint(Module, FunName, badarith)
                            ),
                            beamtalk_error:raise(
                                beamtalk_error:with_details(
                                    Error2,
                                    #{
                                        erlang_error => badarith,
                                        erlang_stacktrace => Stack
                                    }
                                )
                            );
                        error:Reason:Stack ->
                            Error0 = beamtalk_error:new(runtime_error, 'ErlangModule'),
                            Error1 = beamtalk_error:with_selector(Error0, OrigSelector),
                            Error2 = beamtalk_error:with_hint(
                                Error1,
                                erlang_error_hint(Module, FunName, Reason)
                            ),
                            beamtalk_error:raise(
                                beamtalk_error:with_details(
                                    Error2,
                                    #{
                                        erlang_error => Reason,
                                        erlang_stacktrace => Stack
                                    }
                                )
                            );
                        exit:Reason:Stack ->
                            Error0 = beamtalk_error:new(erlang_exit, 'ErlangModule'),
                            Error1 = beamtalk_error:with_selector(Error0, OrigSelector),
                            ReasonBin = iolist_to_binary(io_lib:format("~p", [Reason])),
                            Hint = iolist_to_binary([
                                <<"Erlang process exit in ">>,
                                atom_to_binary(Module, utf8),
                                <<":">>,
                                atom_to_binary(FunName, utf8),
                                <<": ">>,
                                ReasonBin
                            ]),
                            Error2 = beamtalk_error:with_hint(Error1, Hint),
                            beamtalk_error:raise(
                                beamtalk_error:with_details(
                                    Error2,
                                    #{
                                        erlang_exit_reason => Reason,
                                        erlang_stacktrace => Stack
                                    }
                                )
                            );
                        throw:Value:Stack ->
                            Error0 = beamtalk_error:new(erlang_throw, 'ErlangModule'),
                            Error1 = beamtalk_error:with_selector(Error0, OrigSelector),
                            ValueBin = iolist_to_binary(io_lib:format("~p", [Value])),
                            Hint = iolist_to_binary([
                                <<"Erlang throw in ">>,
                                atom_to_binary(Module, utf8),
                                <<":">>,
                                atom_to_binary(FunName, utf8),
                                <<": ">>,
                                ValueBin
                            ]),
                            Error2 = beamtalk_error:with_hint(Error1, Hint),
                            beamtalk_error:raise(
                                beamtalk_error:with_details(
                                    Error2,
                                    #{
                                        erlang_throw_value => Value,
                                        erlang_stacktrace => Stack
                                    }
                                )
                            )
                    end;
                false ->
                    raise_function_or_arity_error(
                        Module, FunName, Arity, OrigSelector, Exports
                    )
            end
    end.

%% @doc Raise an error for missing function or wrong arity (BT-679).
%%
%% Shared by `direct_call/3` and `validate_and_apply/4`.
-spec raise_function_or_arity_error(atom(), atom(), non_neg_integer(), atom(), [
    {atom(), non_neg_integer()}
]) -> no_return().
raise_function_or_arity_error(Module, FunName, Arity, OrigSelector, Exports) ->
    MatchingArities = [A || {F, A} <- Exports, F =:= FunName],
    case MatchingArities of
        [] ->
            Error0 = beamtalk_error:new(does_not_understand, 'ErlangModule'),
            Error1 = beamtalk_error:with_selector(Error0, OrigSelector),
            Error2 = beamtalk_error:with_hint(
                Error1,
                <<"This Erlang function does not exist. Check spelling and arity.">>
            ),
            beamtalk_error:raise(
                beamtalk_error:with_details(Error2, #{
                    module => Module,
                    function => FunName,
                    arity => Arity
                })
            );
        _ ->
            AritiesStr = lists:join(
                <<", ">>,
                [integer_to_binary(A) || A <- lists:sort(MatchingArities)]
            ),
            Hint = iolist_to_binary([
                atom_to_binary(Module, utf8),
                <<":">>,
                atom_to_binary(FunName, utf8),
                <<"/">>,
                iolist_to_binary(AritiesStr),
                <<" exists but was called with ">>,
                integer_to_binary(Arity),
                <<" arguments">>
            ]),
            Error0 = beamtalk_error:new(arity_mismatch, 'ErlangModule'),
            Error1 = beamtalk_error:with_selector(Error0, OrigSelector),
            Error2 = beamtalk_error:with_hint(Error1, Hint),
            beamtalk_error:raise(
                beamtalk_error:with_details(Error2, #{
                    module => Module,
                    function => FunName,
                    expected_arities => lists:sort(MatchingArities),
                    actual_arity => Arity
                })
            )
    end.

%% @doc Raise a structured undef error (TOCTOU: function unloaded between check and apply).
%%
%% Pass the actual arity so the error message is accurate (not "0 arguments").
-spec raise_undef_error(atom(), atom(), non_neg_integer(), atom()) -> no_return().
raise_undef_error(Module, FunName, Arity, OrigSelector) ->
    case get_exports(Module) of
        {error, not_loaded} ->
            raise_module_not_loaded(Module, OrigSelector);
        {ok, Exports} ->
            raise_function_or_arity_error(
                Module, FunName, Arity, OrigSelector, Exports
            )
    end.

%% @doc Raise a structured badarg error.
-spec raise_badarg_error(atom(), atom(), atom(), list()) -> no_return().
raise_badarg_error(Module, FunName, OrigSelector, Stack) ->
    Error0 = beamtalk_error:new(type_error, 'ErlangModule'),
    Error1 = beamtalk_error:with_selector(Error0, OrigSelector),
    Error2 = beamtalk_error:with_hint(
        Error1,
        erlang_error_hint(Module, FunName, badarg)
    ),
    beamtalk_error:raise(
        beamtalk_error:with_details(
            Error2,
            #{
                erlang_error => badarg,
                erlang_stacktrace => Stack
            }
        )
    ).

%% @doc Coerce all binary values in a list to charlists (BT-1127).
%%
%% Beamtalk strings are UTF-8 binaries. Many Erlang functions (os:cmd/1,
%% file:read_file/1, io:format/1) expect charlists. This converts each
%% binary in the arg list to a charlist using unicode:characters_to_list/2.
%% Non-binary args are passed through unchanged.
-spec coerce_binaries_to_charlists(list()) -> list().
coerce_binaries_to_charlists(Args) ->
    lists:map(fun coerce_arg/1, Args).

-spec coerce_arg(term()) -> term().
coerce_arg(Arg) when is_binary(Arg) ->
    case unicode:characters_to_list(Arg, utf8) of
        Charlist when is_list(Charlist) -> Charlist;
        _ -> Arg
    end;
coerce_arg(#beamtalk_object{pid = Pid}) when is_pid(Pid) ->
    %% BT-1442: Auto-coerce Actor proxy objects to raw PIDs for Erlang FFI.
    %% This enables `Erlang erlang monitor: #process arg: actor` without badarg.
    Pid;
coerce_arg(Arg) ->
    Arg.

%% @doc Convert a charlist result to a binary (BT-1127, BT-1398).
%%
%% Erlang functions may return charlists (e.g., os:cmd/1, calendar:system_time_to_rfc3339/1).
%% Convert those back to binaries for consistent Beamtalk string representation.
%% Applied on both the direct success path and the badarg retry path.
%%
%% Uses `io_lib:printable_unicode_list/1` rather than `io_lib:char_list/1` to avoid
%% false positives on lists of small integers (e.g., [1,2,3]) that are valid codepoints
%% but not printable text (BT-1398).
-spec coerce_charlist_result(term()) -> term().
coerce_charlist_result([]) ->
    [];
coerce_charlist_result(Result) when is_list(Result) ->
    case io_lib:printable_unicode_list(Result) of
        true ->
            case unicode:characters_to_binary(Result, utf8) of
                Bin when is_binary(Bin) -> Bin;
                _ -> Result
            end;
        false ->
            Result
    end;
coerce_charlist_result(Result) ->
    Result.

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
