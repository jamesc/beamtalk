%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_erlang_proxy).

%%% **DDD Context:** Object System Context

-moduledoc """
ErlangModule proxy dispatch for BEAM interop (BT-676, BT-679).

Forwards Beamtalk messages to Erlang function calls via `erlang:apply/3`.
ErlangModule proxies are tagged maps of the form:
```
#{
  '$beamtalk_class' => 'ErlangModule',
  module => lists    % the Erlang module to proxy
}
```

**Dispatch rules:**
- Keyword selectors (ending in `:`) → strip colon, use as function name
  `reverse:` + [Arg] → `lists:reverse(Arg)`
- Multi-keyword selectors → first keyword = function name, all args positional
  `sublist:length:` + [Start, Len] → `lists:sublist(Start, Len)`
- Unary selectors (no colon) → zero-arg function call
  `node` → `erlang:node()`

**Export introspection (BT-679):**
Before dispatch, validates function existence and arity via
`Module:module_info(exports)`. Provides actionable error messages for:
- Wrong arity: "lists:reverse/1 exists but was called with 2 arguments"
- Missing function: "This Erlang function does not exist."
- Unloaded module: "Erlang module 'bogus' is not loaded."

**Escape hatch:** `call:args:` bypasses selector→function mapping
for reserved selectors (class, ==, /=, self, etc.).

See: ADR 0028 §1 (Module Proxy Pattern)
""".

-export([coerce_result/1, direct_call/3, dispatch/3, has_method/1, native_call/4, new/1]).

-include("beamtalk.hrl").

%%% Error-reporting context for the unified apply path (ADR 0101 Part 2).
%%%
%%% - `none` — inline `(Erlang module)` FFI. Wrapped errors report the
%%%   Erlang-facing `'ErlangModule'` class and the original selector.
%%% - `{Class, Selector}` — a `native:` Object delegation (BT-2720). Wrapped
%%%   errors report the Beamtalk class/selector so they read `Stream>>take:`
%%%   rather than the bare Erlang MFA.
-type error_context() :: none | {atom(), atom()}.

-doc "Create a new ErlangModule proxy for the given module atom.".
-spec new(atom()) -> map().
new(Module) when is_atom(Module) ->
    #{'$beamtalk_class' => 'ErlangModule', module => Module}.

-doc """
Dispatch a Beamtalk message to the proxied Erlang module.

Handles Object protocol messages (class, printString, methods) locally,
validates export existence/arity before dispatch, and provides
actionable error messages (BT-679).
""".
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

-doc """
Check if an ErlangModule responds to the given selector.

Always returns true — any selector may map to an Erlang function call.
Actual validation (arity, existence) happens at dispatch time.
""".
-spec has_method(atom()) -> boolean().
has_method(_Selector) -> true.

-doc """
Entry point for direct Erlang calls (BT-1127).

The codegen emits `call 'beamtalk_erlang_proxy':'direct_call'(M, F, [args])`
for all `Erlang M fn: arg` expressions, routing through the proxy instead
of calling `call 'M':'F'(args)` directly. This enables:
- Export validation and actionable error messages (BT-679)
- Automatic binary→charlist coercion on badarg (BT-1127)
- Charlist→binary result coercion for consistent string types

Wrapping policy is unified across all apply paths via `apply_with_coercion/5`
(ADR 0101 Part 2): `error:*` is converted to a structured `#beamtalk_error{}`,
while `exit:*` and `throw:*` propagate naturally so `Erlang erlang exit: 1`
behaves as expected and foreign throws stay throws.
""".
-spec direct_call(atom(), atom(), list()) -> term().
direct_call(Module, FunName, Args) ->
    Arity = length(Args),
    case get_exports(Module) of
        {error, not_loaded} ->
            raise_module_not_loaded(Module, FunName);
        {ok, Exports} ->
            case lists:member({FunName, Arity}, Exports) of
                true ->
                    apply_with_coercion(Module, FunName, Args, FunName, none);
                false ->
                    raise_function_or_arity_error(
                        Module, FunName, Arity, FunName, Exports
                    )
            end
    end.

-doc """
Entry point for `native:` Object delegation (ADR 0101 Part 1, BT-2720).

`native:` Object methods lower to
`beamtalk_erlang_proxy:native_call(Mod, Fn, [Self | Args], {Class, Sel})`.
Unlike `direct_call/3`/`validate_and_apply/4` this **skips the proactive
`get_exports`/arity pre-check** — codegen guarantees the call shape at compile
time, so the check is pure overhead. It still routes through the shared
`apply_with_coercion/5`, so it gets the **same** `error:*` → `#beamtalk_error{}`
wrapping and ADR 0076 ok/error → `Result` coercion as inline FFI.

The `{Class, Sel}` context makes wrapped errors read `Stream>>take:` instead of
the bare Erlang MFA. Crucially, `error:undef` is still caught (the compile-time
guarantee does not survive **hot code reload** — a backing function can be
swapped between compile and call) and surfaced as a structured
`does_not_understand` carrying `{Class, Sel}`.

**Codegen-only.** This entry deliberately omits the proactive
`get_exports`/arity pre-check that `direct_call/3` and `validate_and_apply/4`
run, so it must only be called with the exact `{Mod, Fn, Args}` shape codegen
emits. Hand-written code wanting export validation up front should use
`direct_call/3` (or the `(Erlang module)` FFI surface) instead.
""".
-spec native_call(atom(), atom(), list(), {atom(), atom()}) -> term().
native_call(Module, FunName, Args, {Class, Selector}) ->
    apply_with_coercion(Module, FunName, Args, FunName, {Class, Selector}).

-doc """
The single, unified apply path for all FFI (ADR 0101 Part 2).

Calls `Module:FunName(Args)`, coercing ok/error → `Result` (ADR 0076) on the
happy path and retrying with charlist-coerced args on `badarg` (BT-1127).

Exception policy — converged across `direct_call/3`, `validate_and_apply/4`,
and `native_call/4`:
- `error:#beamtalk_error{}` (already wrapped) → re-raised unchanged
- `error:undef` → structured `does_not_understand` (hot-reload TOCTOU)
- `error:badarg` → charlist retry, else `type_error`
- `error:function_clause` → `arity_mismatch`
- `error:badarith` → `type_error`
- any other `error:Reason` → `runtime_error` (this is the catch-all that makes
  casual FFI safe by default — previously `direct_call` leaked these)
- `exit:*` → **propagates** (not caught)
- `throw:*` → **passes through** (not caught)

`Context` selects the class/selector reported on wrapped errors — see
`error_context()`. The wrapping helpers (`ensure_wrapped`-style) run only on the
error path; the happy path pays nothing beyond the `try` frame + coercion.
""".
-spec apply_with_coercion(atom(), atom(), list(), atom(), error_context()) -> term().
apply_with_coercion(Module, FunName, Args, OrigSelector, Context) ->
    try
        coerce_ffi_result(Module, erlang:apply(Module, FunName, Args))
    catch
        %% Clause order mirrors maybe_retry_badarg/6 for readability. The patterns
        %% are mutually exclusive (distinct atoms vs map vs record), so order only
        %% matters for the trailing catch-all, which must stay last.
        error:#{error := #beamtalk_error{}} = Wrapped:WrappedStack ->
            %% Re-raise already-wrapped Beamtalk exceptions unchanged
            %% (e.g. from Beamtalk code reached via Erlang). Idempotent.
            erlang:raise(error, Wrapped, WrappedStack);
        error:#beamtalk_error{} = Rec:RecStack ->
            %% A bare #beamtalk_error{} record raised by an Erlang shim
            %% (e.g. error(#beamtalk_error{kind = type_error, …})) is already
            %% classified — wrap it into the map form *preserving its kind*
            %% (idempotent); do not reclassify it as a generic runtime_error.
            erlang:raise(error, beamtalk_exception_handler:wrap(Rec), RecStack);
        error:badarg:Stack ->
            maybe_retry_badarg(Module, FunName, Args, OrigSelector, Context, Stack);
        error:undef:_Stack ->
            %% TOCTOU / hot code reload: function unloaded between check and apply
            raise_undef_error(Module, FunName, length(Args), OrigSelector, Context);
        error:function_clause:Stack ->
            raise_function_clause_error(Module, FunName, OrigSelector, Context, Stack);
        error:badarith:Stack ->
            raise_badarith_error(Module, FunName, OrigSelector, Context, Stack);
        error:Reason:Stack ->
            raise_generic_error(Module, FunName, OrigSelector, Context, Reason, Stack)
        %% exit:* and throw:* are deliberately NOT caught — they propagate
        %% (ADR 0101 Part 2: process exit and foreign throws are semantically
        %% meaningful and must not be flattened into #beamtalk_error{}).
    end.

-doc """
BT-1127: auto-coerce binary args to charlists and retry once on `badarg`.

Many Erlang functions (os:cmd/1, file:read_file/1, …) expect charlists but
Beamtalk strings are binaries. Applies the same unified catch policy on the
retry; if the retry still fails (or no binary args were coercible) the error is
wrapped per `apply_with_coercion/5`.
""".
-spec maybe_retry_badarg(atom(), atom(), list(), atom(), error_context(), list()) -> term().
maybe_retry_badarg(Module, FunName, Args, OrigSelector, Context, Stack) ->
    CoercedArgs = coerce_binaries_to_charlists(Args),
    case CoercedArgs =/= Args of
        true ->
            try
                coerce_ffi_result(Module, erlang:apply(Module, FunName, CoercedArgs))
            catch
                error:#{error := #beamtalk_error{}} = Wrapped:WrappedStack ->
                    erlang:raise(error, Wrapped, WrappedStack);
                error:#beamtalk_error{} = Rec:RecStack ->
                    erlang:raise(error, beamtalk_exception_handler:wrap(Rec), RecStack);
                error:badarg:Stack2 ->
                    raise_badarg_error(Module, FunName, OrigSelector, Context, Stack2);
                error:undef:_Stack2 ->
                    raise_undef_error(Module, FunName, length(CoercedArgs), OrigSelector, Context);
                error:function_clause:Stack2 ->
                    raise_function_clause_error(Module, FunName, OrigSelector, Context, Stack2);
                error:badarith:Stack2 ->
                    raise_badarith_error(Module, FunName, OrigSelector, Context, Stack2);
                error:Reason2:Stack2 ->
                    raise_generic_error(Module, FunName, OrigSelector, Context, Reason2, Stack2)
                %% exit:* / throw:* propagate (see apply_with_coercion/5)
            end;
        false ->
            raise_badarg_error(Module, FunName, OrigSelector, Context, Stack)
    end.

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

-doc "Format an actionable hint for an Erlang error.".
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

-doc """
Convert a Beamtalk selector atom to an Erlang function name.

Returns {ok, Atom} when the function name is a known atom,
or {error, Binary} when it's not in the atom table (cannot exist as
an exported function). This avoids creating new atoms for unknown selectors.

Keyword selectors like `'reverse:'` → `{ok, reverse}`
Multi-keyword like `'sublist:length:'` → `{ok, sublist}`  (first keyword only)
Unary selectors like `'node'` → `{ok, node}`
""".
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

-doc """
Get the exports for a module, checking if it's loaded first.

Returns {ok, [{Function, Arity}]} or {error, not_loaded}.
""".
-spec get_exports(atom()) -> {ok, [{atom(), non_neg_integer()}]} | {error, not_loaded}.
get_exports(Module) ->
    try
        {ok, Module:module_info(exports)}
    catch
        error:undef -> {error, not_loaded}
    end.

-doc """
Validate function existence and arity, then apply.

Checks module_info(exports) before calling erlang:apply/3 to provide
actionable error messages (BT-679). No caching — hot code reload must work.

Once validated, delegates to the unified `apply_with_coercion/5` so the
`call:args:` / dispatch path shares the **same** exception policy as inline FFI
(ADR 0101 Part 2): `error:*` wrapped, `exit:*`/`throw:*` propagated. This rolls
back the former `exit`→`erlang_exit` / `throw`→`erlang_throw` wrapping.
""".
-spec validate_and_apply(atom(), atom(), list(), atom()) -> term().
validate_and_apply(Module, FunName, Args, OrigSelector) ->
    Arity = length(Args),
    case get_exports(Module) of
        {error, not_loaded} ->
            raise_module_not_loaded(Module, OrigSelector);
        {ok, Exports} ->
            case lists:member({FunName, Arity}, Exports) of
                true ->
                    apply_with_coercion(Module, FunName, Args, OrigSelector, none);
                false ->
                    raise_function_or_arity_error(
                        Module, FunName, Arity, OrigSelector, Exports
                    )
            end
    end.

-doc """
Raise an error for missing function or wrong arity (BT-679).

Shared by `direct_call/3` and `validate_and_apply/4`.
""".
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

-doc """
Resolve the `{Class, Selector}` to attach to a wrapped FFI error.

Inline FFI (`Context = none`) reports the Erlang-facing `'ErlangModule'` class
and the original selector — preserving the long-standing error contract.
A `native:` delegation (`Context = {Class, Sel}`) reports the Beamtalk
class/selector so errors read `Stream>>take:` (ADR 0101 Part 2).
""".
-spec error_class_selector(error_context(), atom()) -> {atom(), atom()}.
error_class_selector(none, OrigSelector) -> {'ErlangModule', OrigSelector};
error_class_selector({Class, Selector}, _OrigSelector) -> {Class, Selector}.

-doc """
Raise a structured undef error (TOCTOU: function unloaded between check and apply).

Pass the actual arity so the error message is accurate (not "0 arguments").

For inline FFI (`Context = none`) this re-checks exports to distinguish
"missing function" from "wrong arity". For a `native:` delegation
(`Context = {Class, Sel}`) the pre-check was skipped by design, so surface a
`does_not_understand` carrying the Beamtalk `{Class, Sel}` directly.
""".
-spec raise_undef_error(atom(), atom(), non_neg_integer(), atom(), error_context()) -> no_return().
raise_undef_error(Module, FunName, Arity, OrigSelector, none) ->
    case get_exports(Module) of
        {error, not_loaded} ->
            raise_module_not_loaded(Module, OrigSelector);
        {ok, Exports} ->
            raise_function_or_arity_error(
                Module, FunName, Arity, OrigSelector, Exports
            )
    end;
raise_undef_error(Module, FunName, Arity, _OrigSelector, {Class, Selector}) ->
    Error0 = beamtalk_error:new(does_not_understand, Class),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(
        Error1,
        erlang_error_hint(Module, FunName, undef)
    ),
    beamtalk_error:raise(
        beamtalk_error:with_details(Error2, #{
            module => Module,
            function => FunName,
            arity => Arity,
            erlang_error => undef
        })
    ).

-doc "Raise a structured badarg error (type_error).".
-spec raise_badarg_error(atom(), atom(), atom(), error_context(), list()) -> no_return().
raise_badarg_error(Module, FunName, OrigSelector, Context, Stack) ->
    raise_wrapped_error(type_error, badarg, Module, FunName, OrigSelector, Context, Stack).

-doc "Raise a structured function_clause error (arity_mismatch).".
-spec raise_function_clause_error(atom(), atom(), atom(), error_context(), list()) -> no_return().
raise_function_clause_error(Module, FunName, OrigSelector, Context, Stack) ->
    raise_wrapped_error(
        arity_mismatch, function_clause, Module, FunName, OrigSelector, Context, Stack
    ).

-doc "Raise a structured badarith error (type_error).".
-spec raise_badarith_error(atom(), atom(), atom(), error_context(), list()) -> no_return().
raise_badarith_error(Module, FunName, OrigSelector, Context, Stack) ->
    raise_wrapped_error(type_error, badarith, Module, FunName, OrigSelector, Context, Stack).

-doc """
Catch-all wrapper for any other `error:Reason` (ADR 0101 Part 2).

This is what makes casual FFI safe by default: any `error:*` not handled by a
specific clause above becomes a structured `#beamtalk_error{}` rather than
leaking a raw Erlang fault to the user/REPL.

Delegates to the canonical classifier `beamtalk_exception_handler:ensure_wrapped/4`
(AC: "via ensure_wrapped"), so unclassified BEAM shapes get readable, bucketed
messages: `{badkey, K}` → key_error "key not found: K", `{badmap, M}`,
`{badmatch, V}`, `noproc`, `timeout`, … (BT-2704/2707). The `{Class, Selector}`
breadcrumb locates the message (e.g. `Stream>>take:`). This reuses the existing
classifier rather than re-deriving FFI error text in the proxy.
""".
-spec raise_generic_error(atom(), atom(), atom(), error_context(), term(), list()) -> no_return().
raise_generic_error(_Module, _FunName, OrigSelector, Context, Reason, Stack) ->
    Ctx = generic_error_context(Context, OrigSelector),
    Wrapped = beamtalk_exception_handler:ensure_wrapped(error, Reason, Stack, Ctx),
    erlang:raise(error, Wrapped, Stack).

-doc """
Build the dispatch breadcrumb map (`#{class, selector}`) handed to
`ensure_wrapped/4` so a generically-wrapped FFI error is located. Inline FFI
reports `'ErlangModule'` + the Erlang function; `native:` reports the Beamtalk
class/selector.
""".
-spec generic_error_context(error_context(), atom()) -> map().
generic_error_context(none, OrigSelector) ->
    #{class => 'ErlangModule', selector => OrigSelector};
generic_error_context({Class, Selector}, _OrigSelector) ->
    #{class => Class, selector => Selector}.

-doc """
Shared builder for a wrapped `error:*` FFI exception.

Attaches `{Class, Selector}` per `error_class_selector/2`, an actionable hint,
and the original Erlang error + stacktrace in details.
""".
-spec raise_wrapped_error(atom(), term(), atom(), atom(), atom(), error_context(), list()) ->
    no_return().
raise_wrapped_error(Kind, ErlangError, Module, FunName, OrigSelector, Context, Stack) ->
    {ErrClass, ErrSelector} = error_class_selector(Context, OrigSelector),
    Error0 = beamtalk_error:new(Kind, ErrClass),
    Error1 = beamtalk_error:with_selector(Error0, ErrSelector),
    Error2 = beamtalk_error:with_hint(
        Error1,
        erlang_error_hint(Module, FunName, ErlangError)
    ),
    beamtalk_error:raise(
        beamtalk_error:with_details(
            Error2,
            #{
                erlang_error => ErlangError,
                erlang_stacktrace => Stack
            }
        )
    ).

-doc """
Coerce all binary values in a list to charlists (BT-1127).

Beamtalk strings are UTF-8 binaries. Many Erlang functions (os:cmd/1,
file:read_file/1, io:format/1) expect charlists. This converts each
binary in the arg list to a charlist using unicode:characters_to_list/2.
Non-binary args are passed through unchanged.
""".
-spec coerce_binaries_to_charlists(list()) -> list().
coerce_binaries_to_charlists(Args) ->
    [coerce_arg(A) || A <- Args].

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
    %% ADR 0079 / BT-1990: name-resolving proxies (`pid = {registered, _}`)
    %% deliberately pass through unchanged so the receiving FFI shim
    %% (e.g. `beamtalk_actor:registeredName/1`, `unregister/1`) can answer
    %% from the proxy's identity slot rather than resolving the name. Code
    %% that needs a raw pid (e.g. `Erlang erlang monitor: actor`) should
    %% call `actor pid` first to materialise the current pid explicitly.
    Arg.

-doc """
Convert a charlist result to a binary (BT-1127, BT-1398).

Erlang functions may return charlists (e.g., os:cmd/1, calendar:system_time_to_rfc3339/1).
Convert those back to binaries for consistent Beamtalk string representation.
Applied on both the direct success path and the badarg retry path.

Uses `io_lib:printable_unicode_list/1` rather than `io_lib:char_list/1` to avoid
false positives on lists of small integers (e.g., [1,2,3]) that are valid codepoints
but not printable text (BT-1398).
""".
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

-doc """
Apply both charlist and result coercion, skipping charlist coercion
for Beamtalk's own modules (BT-1839).

Beamtalk runtime modules (beamtalk_*) and compiled classes (bt@*) already
return properly-typed values — their lists are genuine Beamtalk lists, not
Erlang charlists.  Applying `coerce_charlist_result/1` to their results
corrupts lists of small integers (e.g., [10, 11, 12] from Stream take:)
by converting them to binaries.

Charlist coercion is only needed for stock Erlang modules whose APIs return
charlists (e.g., os:cmd/1, calendar:system_time_to_rfc3339/1).
""".
-spec coerce_ffi_result(atom(), term()) -> term().
coerce_ffi_result(Module, Result) ->
    case is_beamtalk_module(Module) of
        true ->
            coerce_result_no_charlist(Result);
        false ->
            coerce_result(coerce_charlist_result(Result))
    end.

-doc """
Like coerce_result/1 but skips charlist coercion on inner values.
Used for Beamtalk modules whose lists are genuine typed values.
""".
-spec coerce_result_no_charlist(term()) -> term().
coerce_result_no_charlist({ok, Value}) ->
    beamtalk_result:from_tagged_tuple({ok, Value});
coerce_result_no_charlist({error, Reason}) ->
    beamtalk_result:from_tagged_tuple({error, Reason});
coerce_result_no_charlist(ok) ->
    beamtalk_result:from_tagged_tuple({ok, nil});
coerce_result_no_charlist(error) ->
    beamtalk_result:from_tagged_tuple({error, nil});
coerce_result_no_charlist(Other) ->
    Other.

-doc """
Check whether a module is part of the Beamtalk runtime/stdlib.

Beamtalk modules use two naming conventions:
- `beamtalk_*` — hand-written runtime helpers (beamtalk_stream, beamtalk_file, etc.)
- `bt@*` — compiler-generated class modules (bt@stdlib@list, bt@test@foo, etc.)

These modules return properly-typed Beamtalk values and must NOT have their
list results coerced from charlists to binaries.
""".
-spec is_beamtalk_module(atom()) -> boolean().
is_beamtalk_module(Module) ->
    ModStr = atom_to_list(Module),
    lists:prefix("beamtalk_", ModStr) orelse lists:prefix("bt@", ModStr).

-doc """
Coerce Erlang ok/error tuples to Beamtalk Result objects (ADR 0076).

Applied after `coerce_charlist_result/1` in the FFI pipeline. Converts:
- `{ok, Value}` → `Result ok: Value` via `from_tagged_tuple/1`
- `{error, Reason}` → `Result error: Reason` via `from_tagged_tuple/1`
- bare `ok` atom → `Result ok: nil`
- bare `error` atom → `Result error: nil`
- 3+ element tuples (e.g. `{ok, V1, V2}`) pass through as Tuple
- Non-ok/error tuples pass through as Tuple
- Non-tuple/non-atom values pass through unchanged
""".
-spec coerce_result(term()) -> term().
coerce_result({ok, Value}) ->
    beamtalk_result:from_tagged_tuple({ok, coerce_charlist_result(Value)});
coerce_result({error, Reason}) ->
    beamtalk_result:from_tagged_tuple({error, coerce_charlist_result(Reason)});
coerce_result(ok) ->
    beamtalk_result:from_tagged_tuple({ok, nil});
coerce_result(error) ->
    beamtalk_result:from_tagged_tuple({error, nil});
coerce_result(Other) ->
    Other.

-doc "Raise error for unloaded module.".
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
