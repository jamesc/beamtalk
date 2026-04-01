%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Runtime helpers for the Result value type (ADR 0060).
%%%
%%% Provides two entry points:
%%%
%%% - `from_tagged_tuple/1` — Converts Erlang `{ok, V} | {error, R}` tuples to
%%%   `Result` tagged maps. Wraps error reasons via `ensure_wrapped/1` so that
%%%   `errReason` is always a Beamtalk Exception object (re-raiseable by `unwrap`).
%%%   This is an **internal helper** for FFI modules — callers must supply
%%%   structured errors (not bare atoms) before surfacing Results to Beamtalk code.
%%%
%%% - `tryDo:/1` — Implements the `Result tryDo: block` class method.
%%%   Evaluates a block (Erlang fun), catching any exception and returning
%%%   it as a `Result error:`.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Result tagged maps have the form:
%%% ```
%%% #{
%%%   '$beamtalk_class' => 'Result',
%%%   'isOk'      => true | false,
%%%   'okValue'   => Value | nil,
%%%   'errReason' => nil | ExceptionObject
%%% }
%%% ```
%%% Field names match the `state:` declarations in `stdlib/src/Result.bt`.

-module(beamtalk_result).
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([
    from_tagged_tuple/1,
    'ok:'/1,
    'makeError:'/1,
    'tryDo:'/1,
    'unwrapError:'/2
]).
%% FFI shims: beamtalk_erlang_proxy:selector_to_function/1 strips the colon
%% suffix from keyword selectors (e.g. 'ok:' → ok). Each shim delegates to
%% the canonical quoted function that is the real implementation.
-export([ok/1, makeError/1, tryDo/1, unwrapError/1]).

-spec ok(term()) -> map().
ok(Value) -> 'ok:'(Value).
-spec makeError(term()) -> map().
makeError(Reason) -> 'makeError:'(Reason).
-spec tryDo(function()) -> map().
tryDo(Block) -> 'tryDo:'(Block).
-spec unwrapError(term()) -> no_return().
unwrapError(ErrReason) -> 'unwrapError:'(undefined, ErrReason).

%% @doc Convert an Erlang `{ok, V} | {error, R}` tuple to a Result tagged map.
%%
%% For `{ok, Value}`: wraps Value as-is into a successful Result.
%% For `{error, Reason}`: promotes Reason to a Beamtalk Exception object via
%% `ensure_wrapped/1` so that `errReason` is always re-raiseable by `unwrap`.
%%
%% NOTE: This function only handles the two-element tagged tuple form.
%% - Bare atom `ok` is NOT accepted — it maps to the Beamtalk Symbol `#ok`,
%%   not boolean `true`. FFI authors must use `{ok, nil}` for unit results.
%% - Multi-value tuples like `{ok, V1, V2}` must be normalized before calling.
%% - Functions that crash on failure (raising exceptions) should use `tryDo:`.
%%
%% ## Examples (Erlang)
%% ```erlang
%% beamtalk_result:from_tagged_tuple({ok, 42})
%%   % => #{'$beamtalk_class' => 'Result', 'isOk' => true, 'okValue' => 42, 'errReason' => nil}
%%
%% beamtalk_result:from_tagged_tuple({error, enoent})
%%   % => #{'$beamtalk_class' => 'Result', 'isOk' => false, 'okValue' => nil,
%%   %      'errReason' => #{...Exception tagged map...}}
%% ```
-spec from_tagged_tuple({ok, term()} | {error, term()}) -> map().
from_tagged_tuple({ok, Value}) ->
    #{'$beamtalk_class' => 'Result', 'isOk' => true, 'okValue' => Value, 'errReason' => nil};
from_tagged_tuple({error, Reason}) ->
    %% Promote error reasons to Beamtalk Exception objects so that
    %% errReason is always re-raiseable. ensure_wrapped/1 is idempotent:
    %% already-wrapped Exception maps pass through unchanged.
    ExObj = beamtalk_exception_handler:ensure_wrapped(Reason),
    #{'$beamtalk_class' => 'Result', 'isOk' => false, 'okValue' => nil, 'errReason' => ExObj}.

%% @doc Implements `Result ok: value` constructor — builds an ok Result.
%%
%% Called from Beamtalk as `(Erlang beamtalk_result) ok: value`.
%%
%% ## Examples (Erlang)
%% ```erlang
%% beamtalk_result:'ok:'(42)
%%   % => #{'$beamtalk_class' => 'Result', 'isOk' => true, 'okValue' => 42, 'errReason' => nil}
%% ```
-spec 'ok:'(term()) -> map().
'ok:'(Value) ->
    from_tagged_tuple({ok, Value}).

%% @doc Implements `Result error: reason` constructor — builds an error Result.
%%
%% The reason is stored as-is (NOT wrapped via ensure_wrapped/1). Direct Beamtalk
%% callers supply structured errors or bare symbols — wrapping is their responsibility.
%% FFI modules should use `from_tagged_tuple/1` which wraps automatically.
%%
%% Called from Beamtalk as `(Erlang beamtalk_result) makeError: reason`.
%%
%% ## Examples (Erlang)
%% ```erlang
%% beamtalk_result:'makeError:'(file_not_found)
%%   % => #{'$beamtalk_class' => 'Result', 'isOk' => false, 'okValue' => nil,
%%   %      'errReason' => file_not_found}
%% ```
-spec 'makeError:'(term()) -> map().
'makeError:'(Reason) ->
    %% Store reason as-is — the public API does NOT call ensure_wrapped here.
    %% from_tagged_tuple/1 wraps for FFI callers; direct Beamtalk callers supply
    %% structured errors (or bare symbols, their responsibility).
    #{'$beamtalk_class' => 'Result', 'isOk' => false, 'okValue' => nil, 'errReason' => Reason}.

%% @doc Implements `Result tryDo: block` — wraps exception-raising blocks as Results.
%%
%% Evaluates the block (an Erlang zero-arity fun) directly via `Block()`. On success,
%% returns `Result ok: value`. On exception, wraps it and returns
%% `Result error: exceptionObject`.
%%
%% The exception object is wrapped with `ensure_wrapped/3` (includes stacktrace),
%% so the errReason is a full Beamtalk Exception tagged map and `unwrap` will
%% re-raise it preserving the original class, message, and hints.
%%
%% Called from Beamtalk as `(Erlang beamtalk_result) tryDo: block` (class method).
%%
%% ## Examples (Beamtalk)
%% ```beamtalk
%% Result tryDo: [42]                              // => Result ok: 42
%% Result tryDo: [Exception new signal: "oops"]    // => Result error: <Exception>
%% Result tryDo: [1 / 0]                           // => Result error: <RuntimeError>
%% ```
-spec 'tryDo:'(function()) -> map().
'tryDo:'(Block) when is_function(Block, 0) ->
    try Block() of
        Value ->
            from_tagged_tuple({ok, Value})
    catch
        %% BT-754/BT-761: Non-local returns (^ inside blocks) use throw({'$bt_nlr', ...}).
        %% Re-raise so the enclosing method's NLR handler can intercept them.
        %% The 3-tuple arm is a safety net for backward compatibility.
        throw:{'$bt_nlr', _, _, _} = NLR ->
            throw(NLR);
        throw:{'$bt_nlr', _, _} = NLR ->
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
        message = <<"tryDo: expected a zero-arity block, got: ",
            (beamtalk_primitive:print_string(Block))/binary>>,
        hint = <<"Pass a Beamtalk block literal, e.g. Result tryDo: [expr]">>,
        details = #{got => Block}
    }).

%% @doc Implements the error branch of `Result unwrap`.
%%
%% If `ErrReason` is an already-wrapped Exception tagged map, re-raises its
%% underlying `#beamtalk_error{}`. For any other value (raw symbol, atom, term),
%% signals a generic Error with a descriptive message.
%%
%% Called from Beamtalk as `(Erlang beamtalk_result) unwrapError: self.errReason`
%% from the sealed `unwrap` method on Result. The indirection avoids a type-system
%% warning from the compiler (which sees errReason as Object, not Exception).
%%
%% ## Examples (Erlang)
%% ```erlang
%% beamtalk_result:'unwrapError:'(undefined, #{'$beamtalk_class' => 'RuntimeError', error => Error})
%%   % raises the embedded error
%%
%% beamtalk_result:'unwrapError:'(undefined, file_not_found)
%%   % raises: "unwrap called on Result error: file_not_found"
%% ```
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
        message = Message,
        hint = <<"Use valueOr:, valueOrDo:, or ifOk:ifError: to handle the error case">>,
        details = #{reason => ErrReason}
    },
    beamtalk_error:raise(GenError).
