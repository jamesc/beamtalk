%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Object System Context

-module(beamtalk_result).
-moduledoc """
Runtime helpers for the Result value type (ADR 0060).

Provides two entry points:

- `from_tagged_tuple/1` — Converts Erlang `{ok, V} | {error, R}` tuples to
  `Result` tagged maps. Wraps error reasons via `ensure_wrapped/1` so that
  `errReason` is always a Beamtalk Exception object (re-raiseable by `unwrap`).
  This is an **internal helper** for FFI modules — callers must supply
  structured errors (not bare atoms) before surfacing Results to Beamtalk code.

- `tryDo:/1` — Implements the `Result tryDo: block` class method.
  Evaluates a block (Erlang fun), catching any exception and returning
  it as a `Result error:`.

- `tryDo:/2` — The Tier 2 twin of `tryDo:/1` (ADR 0131 §5). Evaluates a
  stateful block (`fun(StateAcc) -> {Value, StateAcc1}`) that writes outer
  locals, answering `{Result, StateAcc}` so codegen can rebind them.

Result tagged maps have the form:
```
#{
  '$beamtalk_class' => 'Result',
  'isOk'      => true | false,
  'okValue'   => Value | nil,
  'errReason' => nil | ExceptionObject
}
```
Field names match the `state:` declarations in `stdlib/src/result.bt`.
""".
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% `error/1` shadows the auto-imported `erlang:error/1` BIF; this module never
%% calls it unqualified (raises go through the Beamtalk exception machinery,
%% not `erlang:error/1`).
-compile({no_auto_import, [error/1]}).

-export([
    from_tagged_tuple/1,
    'ok:'/1,
    'makeError:'/1,
    'fromTuple:'/1,
    'tryDo:'/1,
    'tryDo:'/2,
    'unwrapError:'/2
]).

-doc """
Type representing a Beamtalk Result tagged map.

Use `t/0` for unparameterized specs (any ok/error types) or `t/2` when the
ok-value and error-reason types are known:

```erlang
-spec 'readAll:'(binary()) -> beamtalk_result:t().
-spec 'parse:'(binary()) -> beamtalk_result:t(map(), beamtalk_error:t()).
```
""".
-type t() :: t(term(), term()).
-type t(OkType, ErrType) :: #{
    '$beamtalk_class' := 'Result',
    'isOk' := boolean(),
    'okValue' := OkType | nil,
    'errReason' := ErrType | nil
}.
-export_type([t/0, t/2]).
%% FFI shims: beamtalk_erlang_proxy:selector_to_function/1 strips the colon
%% suffix from keyword selectors (e.g. 'ok:' → ok). Each shim delegates to
%% the canonical quoted function that is the real implementation.
-export([ok/1, makeError/1, fromTuple/1, tryDo/1, unwrapError/1]).
%% `error/1` is a same-named alias for `self delegate` on `Result class
%% error:` to reach — kept separate from `makeError/1` (used by
%% `beamtalk_erlang_proxy:selector_to_function/1`'s keyword-FFI shim
%% convention) purely so the exported name matches the Beamtalk selector.
-export([error/1]).

-spec ok(term()) -> t().
ok(Value) -> 'ok:'(Value).
-spec makeError(term()) -> t().
makeError(Reason) -> 'makeError:'(Reason).
-spec error(term()) -> t().
error(Reason) -> makeError(Reason).
-spec fromTuple(tuple()) -> t().
fromTuple(Tuple) -> 'fromTuple:'(Tuple).
-spec tryDo(function()) -> t().
tryDo(Block) -> 'tryDo:'(Block).
-spec unwrapError(term()) -> no_return().
unwrapError(ErrReason) -> 'unwrapError:'(undefined, ErrReason).

-doc """
Convert an Erlang `{ok, V} | {error, R}` tuple to a Result tagged map.

For `{ok, Value}`: wraps Value as-is into a successful Result.
For `{error, Reason}`: promotes Reason to a Beamtalk Exception object via
`ensure_wrapped/1` so that `errReason` is always re-raiseable by `unwrap`.

NOTE: This function only handles the two-element tagged tuple form.
- Bare atom `ok` is NOT accepted — it maps to the Beamtalk Symbol `#ok`,
  not boolean `true`. FFI authors must use `{ok, nil}` for unit results.
- Multi-value tuples like `{ok, V1, V2}` must be normalized before calling.
- Functions that crash on failure (raising exceptions) should use `tryDo:`.

## Examples (Erlang)
```erlang
beamtalk_result:from_tagged_tuple({ok, 42})
  % => #{'$beamtalk_class' => 'Result', 'isOk' => true, 'okValue' => 42, 'errReason' => nil}

beamtalk_result:from_tagged_tuple({error, enoent})
  % => #{'$beamtalk_class' => 'Result', 'isOk' => false, 'okValue' => nil,
  %      'errReason' => #{...Exception tagged map...}}
```
""".
-spec from_tagged_tuple({ok, term()} | {error, term()}) -> t().
from_tagged_tuple({ok, Value}) ->
    #{'$beamtalk_class' => 'Result', 'isOk' => true, 'okValue' => Value, 'errReason' => nil};
from_tagged_tuple({error, Reason}) ->
    %% Promote error reasons to Beamtalk Exception objects so that
    %% errReason is always re-raiseable. ensure_wrapped/1 is idempotent:
    %% already-wrapped Exception maps pass through unchanged.
    ExObj = beamtalk_exception_handler:ensure_wrapped(Reason),
    #{'$beamtalk_class' => 'Result', 'isOk' => false, 'okValue' => nil, 'errReason' => ExObj}.

-doc """
Implements `Result ok: value` constructor — builds an ok Result.

Called from Beamtalk as `(Erlang beamtalk_result) ok: value`.

## Examples (Erlang)
```erlang
beamtalk_result:'ok:'(42)
  % => #{'$beamtalk_class' => 'Result', 'isOk' => true, 'okValue' => 42, 'errReason' => nil}
```
""".
-spec 'ok:'(term()) -> t().
'ok:'(Value) ->
    from_tagged_tuple({ok, Value}).

-doc """
Implements `Result error: reason` constructor — builds an error Result.

The reason is stored as-is (NOT wrapped via ensure_wrapped/1). Direct Beamtalk
callers supply structured errors or bare symbols — wrapping is their responsibility.
FFI modules should use `from_tagged_tuple/1` which wraps automatically.

Called from Beamtalk as `(Erlang beamtalk_result) makeError: reason`.

## Examples (Erlang)
```erlang
beamtalk_result:'makeError:'(file_not_found)
  % => #{'$beamtalk_class' => 'Result', 'isOk' => false, 'okValue' => nil,
  %      'errReason' => file_not_found}
```
""".
-spec 'makeError:'(term()) -> t().
'makeError:'(Reason) ->
    %% Store reason as-is — the public API does NOT call ensure_wrapped here.
    %% from_tagged_tuple/1 wraps for FFI callers; direct Beamtalk callers supply
    %% structured errors (or bare symbols, their responsibility).
    #{'$beamtalk_class' => 'Result', 'isOk' => false, 'okValue' => nil, 'errReason' => Reason}.

-doc """
Implements `Result fromTuple: tuple` — converts an Erlang ok/error tuple to a Result.

Delegates to `from_tagged_tuple/1` for `{ok, V}` and `{error, R}` tuples.
Raises a structured type error for any other value.

Called from Beamtalk as `(Erlang beamtalk_result) fromTuple: tuple` (class method).

## Examples (Beamtalk)
```beamtalk
Result fromTuple: #(#ok, 42)         // => Result ok: 42
Result fromTuple: #(#error, #oops)   // => Result error: ...
Result fromTuple: #(1, 2, 3)         // => raises Exception
```
""".
-spec 'fromTuple:'(tuple()) -> t() | no_return().
'fromTuple:'({ok, _Value} = Tuple) ->
    from_tagged_tuple(Tuple);
'fromTuple:'({error, _Reason} = Tuple) ->
    from_tagged_tuple(Tuple);
'fromTuple:'(Other) ->
    beamtalk_error:raise(#beamtalk_error{
        kind = type_error,
        class = 'Result',
        selector = 'fromTuple:',
        message = beamtalk_primitive:string_from_binary(
            <<"fromTuple: expected an {ok, Value} or {error, Reason} tuple, got: ",
                (beamtalk_primitive:print_string(Other))/binary>>
        ),
        hint = beamtalk_primitive:string_from_binary(
            <<"Pass a two-element tuple tagged with ok or error, e.g. Tuple withAll: #(#ok, 42)">>
        ),
        details = #{got => Other}
    }).

-doc """
Implements `Result tryDo: block` — wraps exception-raising blocks as Results.

Evaluates the block (an Erlang zero-arity fun) directly via `Block()`. On success,
returns `Result ok: value`. On exception, wraps it and returns
`Result error: exceptionObject`.

The exception object is wrapped with `ensure_wrapped/3` (includes stacktrace),
so the errReason is a full Beamtalk Exception tagged map and `unwrap` will
re-raise it preserving the original class, message, and hints.

Called from Beamtalk as `(Erlang beamtalk_result) tryDo: block` (class method).

## Examples (Beamtalk)
```beamtalk
Result tryDo: [42]                              // => Result ok: 42
Result tryDo: [Exception new signal: "oops"]    // => Result error: <Exception>
Result tryDo: [1 / 0]                           // => Result error: <RuntimeError>
```
""".
-spec 'tryDo:'(function()) -> t() | no_return().
'tryDo:'(Block) when is_function(Block, 0) ->
    %% ADR 0130 §4: a catch boundary around a Beamtalk block. `protect/1` restores
    %% the class-variable map on an error before we wrap it (writes made before
    %% entering the block are kept; `^` passes through untouched).
    %% bt-catcher-audit: converted - the block runs under protect/1, which restores on error
    try beamtalk_class_vars:protect(Block) of
        Value ->
            from_tagged_tuple({ok, Value})
    catch
        %% Non-local returns (^ inside blocks) use throw({'$bt_nlr', ...}).
        %% Re-raise so the enclosing method's NLR handler can intercept them.
        throw:NLR when ?IS_NLR(NLR) ->
            throw(NLR);
        Class:Reason:Stack ->
            ExObj = beamtalk_exception_handler:ensure_wrapped(Class, Reason, Stack),
            from_tagged_tuple({error, ExObj})
    end;
'tryDo:'(Block) ->
    %% Block is not a zero-arity fun — raise a structured type error rather than crashing
    %% with a bare `badfun` exception.
    beamtalk_error:raise(#beamtalk_error{
        kind = type_error,
        class = 'Result',
        selector = 'tryDo:',
        message = beamtalk_primitive:string_from_binary(
            <<"tryDo: expected a zero-arity block, got: ",
                (beamtalk_primitive:print_string(Block))/binary>>
        ),
        hint = beamtalk_primitive:string_from_binary(<<"Pass a Beamtalk block literal, e.g. Result tryDo: [expr]">>),
        details = #{got => Block}
    }).

-doc """
Implements `Result tryDo: block` for a Tier 2 (stateful) block — ADR 0131 §5.

`Block` is the `fun(StateAcc) -> {Value, StateAcc1}` that codegen emits for a
block literal that writes outer locals. Codegen calls this instead of
`'tryDo:'/1` when the argument to `tryDo:` is such a literal, and treats the
`{Result, StateAcc}` answer as a local-threading construct tuple.

It is the same catch boundary as `'tryDo:'/1` — `protect/1` restores the
class-variable map on a raise, `^` passes through, the exception is wrapped by
`ensure_wrapped/3` — so the Tier 1 and Tier 2 paths share one implementation of
it rather than a copy across the Rust/Erlang line.

- On success the answer is `{Result ok: Value, StateAcc1}`: the block's local
  writes are kept.
- On a raise the answer is `{Result error: Exception, StateAcc}` with the
  **entry** `StateAcc`: the block's local writes are discarded, exactly as its
  class-variable writes are (`protect/1`) and as `on:do:` does.

## Examples (Erlang)
```erlang
{Ok, #{n := 1}} = beamtalk_result:'tryDo:'(fun(S) -> {7, S#{n => 1}} end, #{n => 0}).
{Err, #{n := 0}} = beamtalk_result:'tryDo:'(fun(S) -> _ = S#{n => 1}, error(boom) end, #{n => 0}).
```
""".
-spec 'tryDo:'(fun((map()) -> {term(), map()}), map()) -> {t(), map()}.
'tryDo:'(Block, StateAcc) when is_function(Block, 1) ->
    %% bt-catcher-audit: converted - the block runs under protect/1, which restores on error
    try beamtalk_class_vars:protect(fun() -> Block(StateAcc) end) of
        {Value, StateAcc1} ->
            {from_tagged_tuple({ok, Value}), StateAcc1}
    catch
        %% Non-local returns (^ inside blocks) use throw({'$bt_nlr', ...}).
        throw:NLR when ?IS_NLR(NLR) ->
            throw(NLR);
        Class:Reason:Stack ->
            ExObj = beamtalk_exception_handler:ensure_wrapped(Class, Reason, Stack),
            {from_tagged_tuple({error, ExObj}), StateAcc}
    end;
'tryDo:'(Block, _StateAcc) ->
    beamtalk_error:raise(#beamtalk_error{
        kind = type_error,
        class = 'Result',
        selector = 'tryDo:',
        message = beamtalk_primitive:string_from_binary(
            <<"tryDo: expected a one-argument stateful block, got: ",
                (beamtalk_primitive:print_string(Block))/binary>>
        ),
        hint = beamtalk_primitive:string_from_binary(<<"Pass a Beamtalk block literal, e.g. Result tryDo: [expr]">>),
        details = #{got => Block}
    }).

-doc """
Implements the error branch of `Result unwrap`.

If `ErrReason` is an already-wrapped Exception tagged map, re-raises its
underlying `#beamtalk_error{}`. For any other value (raw symbol, atom, term),
signals a generic Error with a descriptive message.

Called from Beamtalk as `(Erlang beamtalk_result) unwrapError: self.errReason`
from the sealed `unwrap` method on Result. The indirection avoids a type-system
warning from the compiler (which sees errReason as Object, not Exception).

## Examples (Erlang)
```erlang
beamtalk_result:'unwrapError:'(undefined, #{'$beamtalk_class' => 'RuntimeError', error => Error})
  % raises the embedded error

beamtalk_result:'unwrapError:'(undefined, file_not_found)
  % raises: "unwrap called on Result error: #file_not_found"
```
""".
-spec 'unwrapError:'(term(), term()) -> no_return().
'unwrapError:'(_Self, #{'$beamtalk_class' := _, error := #beamtalk_error{} = Error}) ->
    %% Already-wrapped Exception — re-raise preserving class, message, and hints.
    %% Pattern requires error field to be a #beamtalk_error{} to avoid crashing on
    %% malformed maps that happen to have an 'error' key with a non-record value.
    beamtalk_error:raise(Error);
'unwrapError:'(_Self, ErrReason) ->
    %% Raw value (symbol, atom, etc.) — signal a generic descriptive error
    ReasonStr = beamtalk_primitive:print_string(ErrReason),
    Message = <<"unwrap called on Result error: ", ReasonStr/binary>>,
    GenError = #beamtalk_error{
        kind = signal,
        class = 'Result',
        selector = 'unwrap',
        %% BT-4173: error message/hint are tagged Strings (see beamtalk.hrl).
        message = beamtalk_primitive:string_from_binary(Message),
        hint = beamtalk_primitive:string_from_binary(
            <<"Use valueOr:, valueOrDo:, or ifOk:ifError: to handle the error case">>
        ),
        details = #{reason => ErrReason}
    },
    beamtalk_error:raise(GenError).
