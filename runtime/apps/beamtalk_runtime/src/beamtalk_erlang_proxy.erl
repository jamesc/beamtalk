%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_erlang_proxy).

%%% **DDD Context:** Object System Context

-moduledoc """
ErlangModule proxy dispatch for BEAM interop.

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

**Export introspection:**
Before dispatch, validates function existence and arity via
`Module:module_info(exports)`. Provides actionable error messages for:
- Wrong arity: "lists:reverse/1 exists but was called with 2 arguments"
- Missing function: "This Erlang function does not exist."
- Unloaded module: "Erlang module 'bogus' is not loaded."

**Escape hatch:** `call:args:` bypasses selector→function mapping
for reserved selectors (class, ==, /=, self, etc.).

See: ADR 0028 §1 (Module Proxy Pattern)
""".

-export([
    coerce_ffi_result/2,
    coerce_ffi_result/3,
    coerce_result/1,
    direct_call/3,
    direct_call/4,
    dispatch/3,
    has_method/1,
    native_call/4,
    native_call/5,
    new/1
]).

-include("beamtalk.hrl").

%%% Error-reporting context for the unified apply path (ADR 0101 Part 2).
%%%
%%% - `none` — inline `(Erlang module)` FFI. Wrapped errors report the
%%%   Erlang-facing `'ErlangModule'` class and the original selector.
%%% - `{Class, Selector}` — a `native:` Object delegation. Wrapped
%%%   errors report the Beamtalk class/selector so they read `Stream>>take:`
%%%   rather than the bare Erlang MFA.
-type error_context() :: none | {atom(), atom()}.

%%% Which result-coercion rule applies (BT-4153).
%%%
%%% - `stock` — a stock OTP / third-party module: scan list results for
%%%   charlists and coerce the bare `error` atom, like `coerce_result/1`.
%%% - `beamtalk` — a Beamtalk-owned module (`beamtalk_*` or `bt@*`, see
%%%   `is_beamtalk_module/1`): lists pass through untouched and the bare
%%%   `error` atom is a legitimate value (BT-3660); modules exporting
%%%   `beamtalk_ffi_raw_results/0` skip Result coercion as well.
%%%
%%% A pure function of the module atom, so codegen precomputes it for
%%% statically-known call sites (`direct_call/4`, `native_call/5`) and the
%%% per-call module-name scan is skipped. The dynamic paths (`direct_call/3`,
%%% `native_call/4`, `validate_and_apply/4`) derive it here per call.
%%% Renaming a module across the stock/beamtalk boundary requires
%%% recompiling its callers, as with any rename — a stale caller beam keeps
%%% passing the mode the old name implied.
-type coercion_mode() :: stock | beamtalk.

-doc "Create a new ErlangModule proxy for the given module atom.".
-spec new(atom()) -> map().
new(Module) when is_atom(Module) ->
    #{'$beamtalk_class' => 'ErlangModule', module => Module}.

-doc """
Dispatch a Beamtalk message to the proxied Erlang module.

Handles Object protocol messages (class, printString, methods) locally,
validates export existence/arity before dispatch, and provides
actionable error messages.
""".
-spec dispatch(atom(), list(), map()) -> term().
dispatch('class', _Args, _Self) ->
    'ErlangModule';
dispatch('printString', _Args, Self) ->
    Module = maps:get(module, Self),
    iolist_to_binary([<<"#ErlangModule<">>, atom_to_binary(Module, utf8), <<">">>]);
dispatch('methods', _Args, Self) ->
    %% REPL discoverability: return exported functions with arities.
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
             || {F, A} <- Exports, not beamtalk_reflection:is_predefined_function(F)
            ]);
        {error, not_loaded} ->
            raise_module_not_loaded(Module, 'methods')
    end;
dispatch('call:args:', [SelectorSym, ArgsTuple], Self) when
    is_atom(SelectorSym), is_tuple(ArgsTuple)
->
    %% Escape hatch for reserved selectors (class, ==, /=, self, etc.).
    Module = maps:get(module, Self),
    Args = tuple_to_list(ArgsTuple),
    validate_and_apply(Module, SelectorSym, Args, 'call:args:');
dispatch('call:args:', [SelectorSym, ArgsTuple], _Self) ->
    %% Type validation for call:args: arguments
    case {is_atom(SelectorSym), is_tuple(ArgsTuple)} of
        {false, _} ->
            beamtalk_error:raise_type_error(
                'ErlangModule',
                'call:args:',
                <<"call:args: first argument must be a Symbol (function name)">>
            );
        {_, false} ->
            beamtalk_error:raise_type_error(
                'ErlangModule',
                'call:args:',
                <<"call:args: second argument must be a Tuple of arguments">>
            )
    end;
dispatch(Selector, Args, Self) ->
    Module = maps:get(module, Self),
    %% Check Object protocol methods first
    case beamtalk_object_ops:try_dispatch(Selector, Args, Self) of
        {ok, Result} ->
            Result;
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
                    Error1 = beamtalk_error:new(does_not_understand, 'ErlangModule', Selector),
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
Entry point for direct Erlang calls.

The codegen emits `call 'beamtalk_erlang_proxy':'direct_call'(M, F, [args])`
for all `Erlang M fn: arg` expressions, routing through the proxy instead
of calling `call 'M':'F'(args)` directly. This enables:
- Actionable error messages for a missing module, function or arity
- Automatic binary→charlist coercion on badarg
- Charlist→binary result coercion for consistent string types

Export validation is **lazy** (BT-4021): the happy path applies straight away
and only reads `Module:module_info(exports)` after the apply fails with
`error:undef` whose top stack frame is the FFI target itself (see
`is_ffi_target_undef/4`). Reading the export list on every call cost ~5-14 µs
per send. Nothing is cached, so hot code reload and purge stay correct: every
call resolves against whatever code is loaded at that moment.

Because the apply is no longer gated on `module_info(exports)`, a module that
exports `'$handle_undefined_function'/2` now handles unknown calls exactly as
it would for a plain Erlang caller; the old pre-check rejected them.

Wrapping policy is unified across all apply paths via `apply_with_coercion/6`
(ADR 0101 Part 2): `error:*` is converted to a structured `#beamtalk_error{}`,
while `exit:*` and `throw:*` propagate naturally so `Erlang erlang exit: 1`
behaves as expected and foreign throws stay throws.

Derives the coercion mode from `Module` on every call. Codegen emits
`direct_call/4` with the mode precomputed instead (BT-4153).
""".
-spec direct_call(atom(), atom(), list()) -> term().
direct_call(Module, FunName, Args) ->
    direct_call(Module, FunName, Args, coercion_mode(Module)).

-doc """
Entry point for direct Erlang calls with a precomputed coercion mode.

Identical to `direct_call/3` except `Mode` — `stock` or `beamtalk`, a pure
function of the statically-known `Module` — is supplied by the caller, so the
per-call module-name scan (`is_beamtalk_module/1`) is skipped. Codegen emits
this for every inline `(Erlang module)` send (BT-4153); `direct_call/3`
derives the mode at runtime for dynamic callers.

Callers must pass the mode for `Module` — a mismatch silently applies the
wrong coercion rule. Anything but `stock`/`beamtalk` fails fast with
`function_clause` here, before the `try`, so a typo is never misattributed
to the callee.
""".
-spec direct_call(atom(), atom(), list(), coercion_mode()) -> term().
direct_call(Module, FunName, Args, Mode) when Mode =:= stock; Mode =:= beamtalk ->
    apply_with_coercion(Module, FunName, Args, FunName, none, Mode).

-doc """
Entry point for `native:` Object delegation (ADR 0101 Part 1).

`native:` Object methods lower to
`beamtalk_erlang_proxy:native_call(Mod, Fn, [Self | Args], {Class, Sel}, Mode)`.
Like `direct_call/3`/`validate_and_apply/4` it does no export pre-check and
routes through the shared `apply_with_coercion/6`, so it gets the **same**
`error:*` → `#beamtalk_error{}` wrapping and ADR 0076 ok/error → `Result`
coercion as inline FFI. The difference is the error context: a missing target
is reported against the Beamtalk `{Class, Sel}` instead of `'ErlangModule'`.

The `{Class, Sel}` context makes wrapped errors read `Stream>>take:` instead of
the bare Erlang MFA. Crucially, `error:undef` is still caught (the compile-time
guarantee does not survive **hot code reload** — a backing function can be
swapped between compile and call) and surfaced as a structured
`does_not_understand` carrying `{Class, Sel}`.

**Codegen-only.** A missing target is reported as a plain
`does_not_understand` on `{Class, Sel}` without the missing-function vs.
wrong-arity distinction, so call it only with the exact `{Mod, Fn, Args}` shape
codegen emits. Hand-written code wanting those messages should use
`direct_call/3` (or the `(Erlang module)` FFI surface) instead.

Derives the coercion mode from `Module` on every call. Codegen emits
`native_call/5` with the mode precomputed instead (BT-4153).
""".
-spec native_call(atom(), atom(), list(), {atom(), atom()}) -> term().
native_call(Module, FunName, Args, {Class, Selector}) ->
    native_call(Module, FunName, Args, {Class, Selector}, coercion_mode(Module)).

-doc """
Entry point for `native:` Object delegation with a precomputed coercion mode.

Identical to `native_call/4` except `Mode` — `stock` or `beamtalk`, a pure
function of the statically-known `Module` — is supplied by the caller, so the
per-call module-name scan (`is_beamtalk_module/1`) is skipped. Codegen emits
this for every `native:` delegation (BT-4153).

Callers must pass the mode for `Module` — a mismatch silently applies the
wrong coercion rule. Anything but `stock`/`beamtalk` fails fast with
`function_clause` here, before the `try`, so a typo is never misattributed
to the callee.
""".
-spec native_call(atom(), atom(), list(), {atom(), atom()}, coercion_mode()) -> term().
native_call(Module, FunName, Args, {Class, Selector}, Mode) when
    Mode =:= stock; Mode =:= beamtalk
->
    apply_with_coercion(Module, FunName, Args, FunName, {Class, Selector}, Mode).

-doc """
The single, unified apply path for all FFI (ADR 0101 Part 2).

Calls `Module:FunName(Args)`, coercing ok/error → `Result` (ADR 0076) on the
happy path and retrying with charlist-coerced args on `badarg`.

Exception policy — converged across `direct_call/3`, `validate_and_apply/4`,
and `native_call/4`:
- `error:#beamtalk_error{}` (already wrapped) → re-raised unchanged
- `error:undef` for the target itself → structured `does_not_understand` /
  `arity_mismatch` / module-not-loaded (`raise_undef_error/6`); an `undef`
  raised *inside* the callee → `runtime_error` like any other `error:*`
- `error:badarg` → charlist retry, else `type_error`
- `error:function_clause` → `arity_mismatch`
- `error:badarith` → `type_error`
- any other `error:Reason` → `runtime_error` (this is the catch-all that makes
  casual FFI safe by default — previously `direct_call` leaked these)
- `exit:*` → **propagates** (not caught)
- `throw:*` → **passes through** (not caught)

`Context` selects the class/selector reported on wrapped errors — see
`error_context()`. `Mode` selects the result-coercion rule without a per-call
module-name scan — see `coercion_mode()`. The wrapping helpers
(`ensure_wrapped`-style) run only on the error path; the happy path pays
nothing beyond the `try` frame, coercion and one class-variable snapshot read.
""".
-spec apply_with_coercion(atom(), atom(), list(), atom(), error_context(), coercion_mode()) ->
    term().
apply_with_coercion(Module, FunName, Args, OrigSelector, Context, Mode) ->
    %% The first attempt can run a Beamtalk block passed as an argument. If it
    %% raises `badarg` and the charlist retry runs, the retry is a fresh attempt:
    %% class-variable writes the failed first attempt made must not survive into
    %% it (ADR 0130 §4, BT-3728), so remember the pre-call state.
    Snap = beamtalk_class_vars:snapshot(),
    %% bt-catcher-audit: converted - snapshot taken here is restored before the badarg retry
    %% (BT-3728)
    try
        coerce_ffi_result(Module, erlang:apply(Module, FunName, Args), Mode)
    catch
        Class:Reason:Stack ->
            %% Single catch → shared classifier: the apply and the
            %% badarg-retry paths must not carry near-verbatim copies of this
            %% clause set, or they drift apart silently when one gains a clause.
            %% On `badarg` this path retries once with charlist-coerced args;
            %% see classify_ffi_exception/9 for the full policy.
            classify_ffi_exception(
                Class,
                Reason,
                Stack,
                Module,
                FunName,
                Args,
                OrigSelector,
                Context,
                fun(M, F, A, Sel, Ctx, St) ->
                    maybe_retry_badarg(M, F, A, Sel, Ctx, St, Snap, Mode)
                end
            )
    end.

-doc """
Auto-coerce binary args to charlists and retry once on `badarg`.

No retry when a direct argument is a fun: re-running the call would re-run
that code (a Beamtalk block's side effects would happen twice), so the
`badarg` is raised as a `type_error` instead (BT-4055). Funs nested inside a
list, map or tuple argument are not detected.

Many Erlang functions (os:cmd/1, file:read_file/1, …) expect charlists but
Beamtalk strings are binaries. Applies the same unified catch policy on the
retry; if the retry still fails (or no binary args were coercible) the error is
wrapped per `apply_with_coercion/6`.

`Snap` is the class-variable snapshot taken before the first attempt. A retry
restores it first, so writes made by a block run inside the failed attempt are
discarded, the same rule `beamtalk_class_vars:protect/1` applies (BT-3728).
""".
-spec maybe_retry_badarg(
    atom(),
    atom(),
    list(),
    atom(),
    error_context(),
    list(),
    beamtalk_class_vars:snapshot(),
    coercion_mode()
) -> term().
maybe_retry_badarg(Module, FunName, Args, OrigSelector, Context, Stack, Snap, Mode) ->
    CoercedArgs = coerce_binaries_to_charlists(Args),
    %% Never retry a call that was passed a fun (a Beamtalk block): the retry
    %% would run caller code a second time, side effects included (BT-4055).
    case CoercedArgs =/= Args andalso not lists:any(fun erlang:is_function/1, Args) of
        true ->
            beamtalk_class_vars:restore(Snap),
            %% bt-catcher-audit: converted - restores the pre-call snapshot before retrying; the
            %% retry re-raises
            try
                coerce_ffi_result(Module, erlang:apply(Module, FunName, CoercedArgs), Mode)
            catch
                %% Same classifier as apply_with_coercion/6, but `badarg` is now
                %% terminal (no second retry) — the OnBadarg continuation raises
                %% directly instead of looping back here.
                Class:Reason:Stack2 ->
                    classify_ffi_exception(
                        Class,
                        Reason,
                        Stack2,
                        Module,
                        FunName,
                        CoercedArgs,
                        OrigSelector,
                        Context,
                        fun raise_badarg_terminal/6
                    )
            end;
        false ->
            raise_badarg_error(Module, FunName, OrigSelector, Context, Stack)
    end.

-doc """
Terminal `OnBadarg` continuation for the badarg-retry path: the charlist retry
already happened, so a second `badarg` is raised, not retried. Drops the unused
`Args` to match the 6-arg continuation shape `classify_ffi_exception/9` calls.
""".
-spec raise_badarg_terminal(atom(), atom(), list(), atom(), error_context(), list()) -> no_return().
raise_badarg_terminal(Module, FunName, _Args, OrigSelector, Context, Stack) ->
    raise_badarg_error(Module, FunName, OrigSelector, Context, Stack).

-doc """
The single FFI exception classifier shared by the apply and badarg-retry paths.

Both `apply_with_coercion/6` and `maybe_retry_badarg/8` used to inline a copy of
this clause set; they only differed in what `badarg` does (retry vs. raise). That
divergence is now the `OnBadarg` continuation — a `fun/6` invoked as
`OnBadarg(Module, FunName, Args, OrigSelector, Context, Stack)` — so adding a new
exception clause here cannot silently skip one path.

Policy (ADR 0101 Part 2), matching the doc on `apply_with_coercion/6`:
- `error:#{error := #beamtalk_error{}}` / `error:#beamtalk_error{}` → re-raised
  (idempotent; never reclassified)
- any other `error:*` raised in compiled Beamtalk code that the callee ran
  (a Beamtalk block passed to a `native:` method or to inline FFI) → re-raised
  unchanged, with no badarg retry: the fault is the block's, not the FFI
  call's, so it keeps the kind it would have outside the call
  (`raised_in_beamtalk_code/2`, BT-4055). The rules below apply to errors
  raised by Erlang code.
- `error:badarg` → `OnBadarg` (charlist retry on the first pass, terminal on the
  retry pass)
- `error:undef` for the target → structured `does_not_understand` (missing
  module/function/arity, including hot-reload removal); `undef` from inside the
  callee → `runtime_error` catch-all
- `error:function_clause` → `arity_mismatch`
- `error:badarith` → `type_error`
- any other `error:Reason` → `runtime_error` catch-all
- `exit:*` / `throw:*` → re-raised unchanged with the captured stacktrace so
  process exit and foreign throws stay semantically meaningful
""".
%% Returns `term()`, not `no_return()`: every classified-exception clause raises,
%% but the `badarg` clause defers to `OnBadarg`, and the first-pass continuation
%% (`maybe_retry_badarg/8`) can *return* the charlist-coerced retry result on
%% success. That value flows back through here to `apply_with_coercion/6`.
-spec classify_ffi_exception(
    error | exit | throw,
    term(),
    list(),
    atom(),
    atom(),
    list(),
    atom(),
    error_context(),
    fun((atom(), atom(), list(), atom(), error_context(), list()) -> term())
) -> term().
classify_ffi_exception(
    error, #{error := #beamtalk_error{}} = Wrapped, Stack, _M, _F, _Args, _Sel, _Ctx, _OnBadarg
) ->
    %% Re-raise already-wrapped Beamtalk exceptions unchanged (e.g. from Beamtalk
    %% code reached via Erlang). Idempotent.
    erlang:raise(error, Wrapped, Stack);
classify_ffi_exception(error, #beamtalk_error{} = Rec, Stack, _M, _F, _Args, _Sel, _Ctx, _OnBadarg) ->
    %% A bare #beamtalk_error{} record raised by an Erlang shim is already
    %% classified — wrap into the map form *preserving its kind* (idempotent); do
    %% not reclassify it as a generic runtime_error.
    erlang:raise(error, beamtalk_exception_handler:wrap(Rec), Stack);
classify_ffi_exception(
    error, Reason, Stack, Module, FunName, Args, OrigSelector, Context, OnBadarg
) ->
    case raised_in_beamtalk_code(Module, Stack) of
        true ->
            %% Raised by compiled Beamtalk code the callee ran (a block passed
            %% in, say): not an FFI fault, so it propagates exactly as it would
            %% had the block run outside the call (BT-4055).
            erlang:raise(error, Reason, Stack);
        false ->
            classify_erlang_error(
                Reason, Stack, Module, FunName, Args, OrigSelector, Context, OnBadarg
            )
    end;
classify_ffi_exception(Class, Reason, Stack, _M, _F, _Args, _Sel, _Ctx, _OnBadarg) ->
    %% exit:* and throw:* propagate unchanged — re-raising with the captured
    %% stacktrace is transparent, but lets a single catch own the whole policy.
    erlang:raise(Class, Reason, Stack).

-doc """
Classify an `error:Reason` raised by Erlang code: the FFI target itself or
anything it called other than compiled Beamtalk code. See
`classify_ffi_exception/9` for the policy.
""".
-spec classify_erlang_error(
    term(),
    list(),
    atom(),
    atom(),
    list(),
    atom(),
    error_context(),
    fun((atom(), atom(), list(), atom(), error_context(), list()) -> term())
) -> term().
classify_erlang_error(
    badarg, Stack, Module, FunName, Args, OrigSelector, Context, OnBadarg
) ->
    OnBadarg(Module, FunName, Args, OrigSelector, Context, Stack);
classify_erlang_error(
    undef, Stack, Module, FunName, Args, OrigSelector, Context, _OnBadarg
) ->
    Arity = length(Args),
    case is_ffi_target_undef(Module, FunName, Arity, Stack) of
        true ->
            %% The FFI target itself is missing: unloaded module, unknown
            %% function, wrong arity, or a hot-reload removal since compile.
            raise_undef_error(Module, FunName, Arity, OrigSelector, Context, Stack);
        false ->
            %% The target ran and *something it called* is undefined. That is
            %% a fault inside the callee, not a missing FFI function, so wrap
            %% it like any other error:* rather than reporting a DNU on the
            %% target (BT-4021).
            raise_generic_error(Module, FunName, OrigSelector, Context, undef, Stack)
    end;
classify_erlang_error(
    function_clause, Stack, Module, FunName, _Args, OrigSelector, Context, _OnBadarg
) ->
    raise_function_clause_error(Module, FunName, OrigSelector, Context, Stack);
classify_erlang_error(
    badarith, Stack, Module, FunName, _Args, OrigSelector, Context, _OnBadarg
) ->
    raise_badarith_error(Module, FunName, OrigSelector, Context, Stack);
classify_erlang_error(
    Reason, Stack, Module, FunName, _Args, OrigSelector, Context, _OnBadarg
) ->
    %% Catch-all that makes casual FFI safe by default (ADR 0101 Part 2).
    raise_generic_error(Module, FunName, OrigSelector, Context, Reason, Stack).

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
    %% bt-catcher-audit: not-applicable-no-block - module_info/1 BIF
    try
        {ok, Module:module_info(exports)}
    catch
        error:undef -> {error, not_loaded}
    end.

-doc """
Apply with lazy validation of function existence and arity.

Delegates to the unified `apply_with_coercion/6` so the `call:args:` / dispatch
path shares the **same** exception policy as inline FFI (ADR 0101 Part 2):
`error:*` wrapped, `exit:*`/`throw:*` propagated. Like `direct_call/3`,
`module_info(exports)` is read only after an `error:undef` on the target
(BT-4021), to build the missing-function / wrong-arity error. No caching — hot
code reload must work. The module is known only at runtime here, so the
coercion mode is derived per call (BT-4153).
""".
-spec validate_and_apply(atom(), atom(), list(), atom()) -> term().
validate_and_apply(Module, FunName, Args, OrigSelector) ->
    apply_with_coercion(Module, FunName, Args, OrigSelector, none, coercion_mode(Module)).

-doc """
Raise an error for missing function or wrong arity.

Used by `raise_missing_target/6` for inline FFI (`direct_call/3`, `validate_and_apply/4`).
""".
-spec raise_function_or_arity_error(atom(), atom(), non_neg_integer(), atom(), [
    {atom(), non_neg_integer()}
]) -> no_return().
raise_function_or_arity_error(Module, FunName, Arity, OrigSelector, Exports) ->
    MatchingArities = [A || {F, A} <- Exports, F =:= FunName],
    case MatchingArities of
        [] ->
            Error1 = beamtalk_error:new(does_not_understand, 'ErlangModule', OrigSelector),
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
            Error1 = beamtalk_error:new(arity_mismatch, 'ErlangModule', OrigSelector),
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
Is an `error:undef` about the FFI target `Module:FunName/Arity` itself?

When `erlang:apply/3` cannot find the target (module missing or not loadable,
function not exported, wrong arity) the top stack frame is the target MFA with
the **argument list** (`{M, F, Args, []}`), including when `error_handler`
or a `'$handle_undefined_function'/2` re-raises it.

Anything else is a fault inside a callee that did run, and must not be
reported as the FFI function being missing: an `undef` from a nested call has
another MFA on top, and a target that runs and calls `erlang:error(undef)`
itself has its own frame on top in the integer-arity form.

An empty stacktrace (e.g. `erlang:system_flag(backtrace_depth, 0)`) carries no
evidence either way, so it counts as the target: `raise_undef_error/6` then
reads exports and still falls back to a plain wrapped `undef` if the target
turns out to be exported.
""".
-spec is_ffi_target_undef(atom(), atom(), non_neg_integer(), list()) -> boolean().
is_ffi_target_undef(_Module, _FunName, _Arity, []) ->
    true;
is_ffi_target_undef(Module, FunName, Arity, [{Module, FunName, Args, _Loc} | _]) when
    is_list(Args)
->
    length(Args) =:= Arity;
is_ffi_target_undef(_Module, _FunName, _Arity, _Stack) ->
    false.

-doc """
True when an exception was raised in compiled Beamtalk code (a `bt@` class
module, or a REPL/inspector evaluation module) that the FFI target ran, such as
a Beamtalk block passed in as an argument.

Scans the stacktrace from the raise point toward the caller. Reaching a `bt@`
frame first means Beamtalk code raised it, directly or through a runtime helper
it called. Reaching the target module or this proxy first means Erlang code
raised it (the target, or a stock module it called), so the FFI classification
applies. A stacktrace with neither is classified as before.

Best effort: a tail call out of Beamtalk code into a raising Erlang function, or
a raise more than `backtrace_depth` frames below the block, hides the Beamtalk
frame, and the error is then classified as the call's. `maybe_retry_badarg/8`
does not rely on this: it never retries a call with a fun as a direct
argument. A fun nested inside a list, map or tuple argument is not detected.
""".
-spec raised_in_beamtalk_code(atom(), list()) -> boolean().
raised_in_beamtalk_code(_Target, []) ->
    false;
raised_in_beamtalk_code(Target, [{Target, _, _, _} | _]) ->
    false;
raised_in_beamtalk_code(_Target, [{?MODULE, _, _, _} | _]) ->
    false;
raised_in_beamtalk_code(Target, [{Module, _, _, _} | Rest]) ->
    case beamtalk_module_name:is_compiled_beamtalk_module(Module) of
        true -> true;
        false -> raised_in_beamtalk_code(Target, Rest)
    end.

-doc """
Raise a structured error for an FFI target that is not callable.

Reached when the apply fails with `error:undef` and `is_ffi_target_undef/4`
says the target itself may be missing (unloaded module, unknown function, wrong
arity, removed by a hot reload, or an empty stacktrace that gives no evidence).

Every context first confirms that against the loaded code with
`ffi_target_status/3`. If the target *is* exported, there is no missing
function to report: the `undef` came from inside the callee (seen with an empty
stacktrace) or a reload landed between the apply and this read. It is then
wrapped like any other `error:*` (`raise_generic_error/6`) instead of a
self-contradicting "does not understand" / arity error.

Otherwise, pass the actual arity so the error message is accurate. Inline FFI
(`Context = none`) distinguishes "module not loaded", "missing function" and
"wrong arity". A `native:` delegation (`Context = {Class, Sel}`) surfaces a
`does_not_understand` carrying the Beamtalk `{Class, Sel}`.
""".
-spec raise_undef_error(atom(), atom(), non_neg_integer(), atom(), error_context(), list()) ->
    no_return().
raise_undef_error(Module, FunName, Arity, OrigSelector, Context, Stack) ->
    case ffi_target_status(Module, FunName, Arity) of
        exported ->
            raise_generic_error(Module, FunName, OrigSelector, Context, undef, Stack);
        Missing ->
            raise_missing_target(Module, FunName, Arity, OrigSelector, Context, Missing)
    end.

-doc """
Is `Module:FunName/Arity` callable in the code loaded right now?

Read only on the `undef` error path, never per call (BT-4021), so it always
reflects the current code and needs no invalidation on reload or purge.
""".
-spec ffi_target_status(atom(), atom(), non_neg_integer()) ->
    exported | not_loaded | {not_exported, [{atom(), non_neg_integer()}]}.
ffi_target_status(Module, FunName, Arity) ->
    case get_exports(Module) of
        {error, not_loaded} ->
            not_loaded;
        {ok, Exports} ->
            case lists:member({FunName, Arity}, Exports) of
                true -> exported;
                false -> {not_exported, Exports}
            end
    end.

-spec raise_missing_target(
    atom(),
    atom(),
    non_neg_integer(),
    atom(),
    error_context(),
    not_loaded | {not_exported, [{atom(), non_neg_integer()}]}
) -> no_return().
raise_missing_target(Module, _FunName, _Arity, OrigSelector, none, not_loaded) ->
    raise_module_not_loaded(Module, OrigSelector);
raise_missing_target(Module, FunName, Arity, OrigSelector, none, {not_exported, Exports}) ->
    raise_function_or_arity_error(Module, FunName, Arity, OrigSelector, Exports);
raise_missing_target(Module, FunName, Arity, _OrigSelector, {Class, Selector}, _Missing) ->
    Error1 = beamtalk_error:new(does_not_understand, Class, Selector),
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
`{badmatch, V}`, `noproc`, `timeout`, …. The `{Class, Selector}`
breadcrumb locates the message (e.g. `Stream>>take:`). This reuses the existing
classifier rather than re-deriving FFI error text in the proxy.
""".
-spec raise_generic_error(atom(), atom(), atom(), error_context(), term(), list()) -> no_return().
raise_generic_error(_Module, _FunName, OrigSelector, Context, Reason, Stack) ->
    Ctx = generic_error_context(Context, OrigSelector),
    Wrapped0 = beamtalk_exception_handler:ensure_wrapped(error, Reason, Stack, Ctx),
    %% Keeps the FFI `details` contract uniform. The specific clauses
    %% (raise_wrapped_error/7) store the raw Erlang fault under `erlang_error`
    %% (+ `erlang_stacktrace`); the shared classifier (wrap_raw) instead records
    %% the canonical `reason`/`key`/`value`. Backfill the FFI keys here — without
    %% clobbering a classifier key — so `details at: #erlang_error` resolves on
    %% every wrapped FFI error, whichever clause produced it.
    Wrapped = backfill_ffi_error_details(Wrapped0, Reason, Stack),
    erlang:raise(error, Wrapped, Stack).

-doc """
Merge the FFI `details` contract keys (`erlang_error` + `erlang_stacktrace`) into
a generically-wrapped error so its details shape matches the specific clauses.
Passing the backfill keys as `maps:merge/2`'s *first* argument and the
classifier's own `Details` as the *second* means `Details` wins on any key clash
(the second map takes precedence), so a pre-existing `erlang_error`/
`erlang_stacktrace` (there is none today, but stay defensive) is never overwritten.
""".
-spec backfill_ffi_error_details(map(), term(), list()) -> map().
backfill_ffi_error_details(
    #{error := #beamtalk_error{details = Details} = Err} = Wrapped, Reason, Stack
) ->
    Merged = maps:merge(#{erlang_error => Reason, erlang_stacktrace => Stack}, Details),
    Wrapped#{error => Err#beamtalk_error{details = Merged}};
backfill_ffi_error_details(Wrapped, _Reason, _Stack) ->
    Wrapped.

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
    Error1 = beamtalk_error:new(Kind, ErrClass, ErrSelector),
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
Coerce all binary values in a list to charlists.

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
    %% Auto-coerce Actor proxy objects to raw PIDs for Erlang FFI.
    %% This enables `Erlang erlang monitor: #process arg: actor` without badarg.
    Pid;
coerce_arg(Arg) ->
    %% ADR 0079: name-resolving proxies (`pid = {registered, _}`)
    %% deliberately pass through unchanged so the receiving FFI shim
    %% (e.g. `beamtalk_actor:registeredName/1`, `unregister/1`) can answer
    %% from the proxy's identity slot rather than resolving the name. Code
    %% that needs a raw pid (e.g. `Erlang erlang monitor: actor`) should
    %% call `actor pid` first to materialise the current pid explicitly.
    Arg.

-doc """
Convert a charlist result to a binary.

Erlang functions may return charlists (e.g., os:cmd/1, calendar:system_time_to_rfc3339/1).
Convert those back to binaries for consistent Beamtalk string representation.
Applied on both the direct success path and the badarg retry path.

Uses `io_lib:printable_unicode_list/1` rather than `io_lib:char_list/1` to avoid
false positives on lists of small integers (e.g., [1,2,3]) that are valid codepoints
but not printable text.
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
for Beamtalk's own modules.

Beamtalk runtime modules (beamtalk_*) and compiled classes (bt@*) already
return properly-typed values — their lists are genuine Beamtalk lists, not
Erlang charlists.  Applying `coerce_charlist_result/1` to their results
corrupts lists of small integers (e.g., [10, 11, 12] from Stream take:)
by converting them to binaries.

Charlist coercion is only needed for stock Erlang modules whose APIs return
charlists (e.g., os:cmd/1, calendar:system_time_to_rfc3339/1).

Derives the coercion mode from `Module` on every call; prefer
`coerce_ffi_result/3` with a precomputed mode (BT-4153).
""".
-spec coerce_ffi_result(atom(), term()) -> term().
coerce_ffi_result(Module, Result) ->
    coerce_ffi_result(Module, Result, coercion_mode(Module)).

-doc """
Derive the result-coercion rule for a runtime module atom (BT-4153).

Codegen precomputes this for statically-known call sites; the dynamic paths
call it here. A pure function of the atom — no loaded-code dependency, so no
invalidation on reload or purge.
""".
-spec coercion_mode(atom()) -> coercion_mode().
coercion_mode(Module) ->
    case is_beamtalk_module(Module) of
        true -> beamtalk;
        false -> stock
    end.

-doc """
Apply both charlist and result coercion under a precomputed mode.

Identical to `coerce_ffi_result/2` except `Mode` selects the rule, so the
module-name scan is skipped.

Fast path (BT-4153): results whose coercion is the identity in *every* mode —
scalars, binaries and bitstrings, pids, ports, references, funs, maps, `[]`,
atoms other than `ok`/`error`, and tuples that are not `{ok, _}`/`{error, _}`
— pass through with no module check and no allocation. Only non-empty lists,
ok/error-tagged results and the bare `ok`/`error` atoms reach the slow path,
which keeps the exact `coerce_ffi_result/2` semantics per mode.

Coupled to the coercion helpers below staying shallow: `coerce_charlist_result/1`
inspects only top-level lists and `coerce_result/1` recurses at most one level
into `{ok, _}`/`{error, _}` payloads. A coercion change that recurses into
tuples/maps, or a new mode-divergent bare-atom rule, must update these clauses
(and the absolute tests) with it — the fast path would otherwise bypass it.
""".
-spec coerce_ffi_result(atom(), term(), coercion_mode()) -> term().
coerce_ffi_result(_Module, Result, _Mode) when
    is_integer(Result);
    is_float(Result);
    %% is_bitstring subsumes is_binary: every binary is a bitstring.
    is_bitstring(Result);
    is_pid(Result);
    is_port(Result);
    is_reference(Result);
    is_function(Result);
    is_map(Result)
->
    Result;
coerce_ffi_result(_Module, [], _Mode) ->
    [];
coerce_ffi_result(_Module, Result, _Mode) when is_atom(Result), Result =/= ok, Result =/= error ->
    Result;
coerce_ffi_result(_Module, Result, _Mode) when
    is_tuple(Result),
    (tuple_size(Result) =/= 2 orelse
        (element(1, Result) =/= ok andalso element(1, Result) =/= error))
->
    Result;
coerce_ffi_result(_Module, ok, stock) ->
    beamtalk_result:from_tagged_tuple({ok, nil});
coerce_ffi_result(Module, ok, beamtalk) ->
    case returns_raw_results(Module) of
        true -> ok;
        false -> beamtalk_result:from_tagged_tuple({ok, nil})
    end;
coerce_ffi_result(_Module, error, stock) ->
    beamtalk_result:from_tagged_tuple({error, nil});
coerce_ffi_result(_Module, error, beamtalk) ->
    %% The bare atom `error` is a legitimate value for Beamtalk-owned
    %% modules (BT-3660) — and raw-result modules pass everything through —
    %% so both beamtalk cases agree here with no raw check.
    error;
coerce_ffi_result(_Module, Result, stock) ->
    coerce_result(coerce_charlist_result(Result));
coerce_ffi_result(Module, Result, beamtalk) ->
    case returns_raw_results(Module) of
        true -> Result;
        false -> coerce_result_no_charlist(Result)
    end.

-doc """
Opt-out marker for Beamtalk-owned modules whose functions return **user data**.

A module that exports `beamtalk_ffi_raw_results/0` (returning `true`) has its
results passed through unchanged: no ADR 0076 `ok`/`{ok, _}`/`{error, _}` →
`Result` coercion. `Ets` needs this because a stored or computed value such as
`#ok` must come back as `#ok`, not as `Result ok: nil`. Only modules whose
functions never signal success or failure through `ok`/`{error, _}` returns may
opt out, since opting out also removes the coercion for their own returns.
""".
-spec returns_raw_results(atom()) -> boolean().
returns_raw_results(Module) ->
    erlang:function_exported(Module, beamtalk_ffi_raw_results, 0) andalso
        Module:beamtalk_ffi_raw_results() =:= true.

-doc """
Like coerce_result/1 but skips charlist coercion on inner values and does not
coerce the bare atom `error` (a legitimate value, e.g. the `error` log level).
Used for Beamtalk modules whose values are genuine typed values.
""".
-spec coerce_result_no_charlist(term()) -> term().
coerce_result_no_charlist({ok, Value}) ->
    beamtalk_result:from_tagged_tuple({ok, Value});
coerce_result_no_charlist({error, Reason}) ->
    beamtalk_result:from_tagged_tuple({error, Reason});
coerce_result_no_charlist(ok) ->
    beamtalk_result:from_tagged_tuple({ok, nil});
%% The bare atom `error` is deliberately NOT coerced here (BT-3660): it is a
%% valid value for Beamtalk-owned modules (e.g. the `error` OTP log level
%% returned by `beamtalk_logging_config:logLevel/0`). No `.bt` caller relies on
%% a bare `error` return from a `beamtalk_*` module; such functions signal
%% failure with `{error, Reason}` or `#beamtalk_error{}`. Stock Erlang modules
%% (e.g. `maps:find/2`) still get the bare-atom coercion in `coerce_result/1`.
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
    Error1 = beamtalk_error:new(does_not_understand, 'ErlangModule', Selector),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    Error3 = beamtalk_error:with_details(Error2, #{module => Module}),
    beamtalk_error:raise(Error3).
