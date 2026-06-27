%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_exception_handler).

%%% **DDD Context:** Object System Context

-moduledoc """
Exception handling runtime support for Beamtalk.

Provides exception wrapping, class matching, and Exception object field
access. Wraps `#beamtalk_error{}' records as exception hierarchy value
type objects (tagged maps). Called by compiler-generated try/catch code.

Exception objects are value types (tagged maps), not actors.
The `$beamtalk_class' is set based on error kind (BT-452), or from the
error's class field if it is a user-defined exception subclass (BT-480):
```
#{
  '$beamtalk_class' => 'MyCustomError',  %% user-defined, or RuntimeError, TypeError, etc.
  error => #beamtalk_error{kind, class, selector, message, hint, details}
}
```

**BT-480: User-defined error subclasses.** When `signal` or `signal:` is
called on an instance of a user-defined Exception subclass (e.g.,
`Error subclass: MyCustomError`), the class name is preserved in the
`#beamtalk_error.class' field. `wrap/1' and `matches_class_name/2' check
whether `error.class' is a registered exception subclass via
`is_exception_class/1'. If so, it is used directly as `$beamtalk_class'
and for hierarchy matching. For built-in errors (where `error.class' is
the originating class like `Integer'), `kind_to_class/1' is used as before.

The compiler generates inline Core Erlang try/catch for `on:do:` and
`ensure:` (structural intrinsics). This module provides the runtime
helpers called by that generated code: `wrap/1` and `matches_class/2`.
""".
-include("beamtalk.hrl").

-export([
    wrap/1,
    wrap_raw/1,
    wrap_raw/2,
    ensure_wrapped/1,
    ensure_wrapped/2,
    ensure_wrapped/3,
    ensure_wrapped/4,
    matches_class/2,
    dispatch/3,
    has_method/1,
    signal/1,
    signal_message/1,
    signal_message/2,
    signal_from_class/1,
    class_signal/1,
    class_signal_message/2,
    kind_to_class/1,
    class_to_kind/1,
    classify_kind/1,
    is_exception_class/1
]).

-doc """
Map an error kind atom to the appropriate exception class name.

Used by wrap/1 to set `$beamtalk_class` based on the error's kind field.
Falls back to 'Error' for unknown kinds (safe bootstrap default).

NOTE: When adding new error subclasses (e.g., IOError), add a clause here.
is_exception_class/1 and matches_class_name/2 derive hierarchy from the
class system automatically (BT-475).
""".
-spec kind_to_class(atom()) -> atom().
kind_to_class(does_not_understand) -> 'RuntimeError';
kind_to_class(arity_mismatch) -> 'RuntimeError';
kind_to_class(immutable_value) -> 'RuntimeError';
kind_to_class(runtime_error) -> 'RuntimeError';
kind_to_class(index_out_of_bounds) -> 'RuntimeError';
kind_to_class(class_not_found) -> 'RuntimeError';
kind_to_class(no_superclass) -> 'RuntimeError';
kind_to_class(class_already_exists) -> 'RuntimeError';
kind_to_class(dispatch_error) -> 'RuntimeError';
kind_to_class(callback_failed) -> 'RuntimeError';
kind_to_class(actor_dead) -> 'RuntimeError';
kind_to_class(future_not_awaited) -> 'RuntimeError';
kind_to_class(internal_error) -> 'RuntimeError';
%% BT-2707: classified raw-error kinds (bucket A user-input, bucket C resource/env).
%% No dedicated stdlib class exists for these yet, so they live under RuntimeError
%% for catchability (a future KeyError/ResourceError hierarchy can refine this).
kind_to_class(key_error) -> 'RuntimeError';
kind_to_class(argument_error) -> 'RuntimeError';
kind_to_class(resource_error) -> 'RuntimeError';
kind_to_class(process_not_found) -> 'RuntimeError';
kind_to_class(timeout_error) -> 'RuntimeError';
kind_to_class(type_error) -> 'TypeError';
kind_to_class(instantiation_error) -> 'InstantiationError';
%% BEAM interop exceptions (ADR 0028 §1, BT-678)
kind_to_class(erlang_exit) -> 'ExitError';
kind_to_class(erlang_throw) -> 'ThrowError';
%% signal (from signal_message/1) stays Error — user decides semantics.
%% file_*/io_error/permission_denied stay Error — future IOError (ADR 0015 Phase 6).
kind_to_class(signal) -> 'Error';
kind_to_class(_) -> 'Error'.

-doc """
Derive the canonical error kind for a named exception class.

Inverse of kind_to_class/1 for the stdlib exception subclasses that have
a dedicated kind (BT-1056: InstantiationError and TypeError have canonical kinds
so that `SomeClass new signal: msg` produces the correct catchable kind).
User-defined subclasses without a dedicated kind use `signal`.
""".
-spec class_to_kind(atom()) -> atom().
class_to_kind('InstantiationError') -> instantiation_error;
class_to_kind('TypeError') -> type_error;
class_to_kind(_) -> signal.

-doc """
Check if a class name belongs to the exception hierarchy.

Delegates to the class system's superclass chain (BT-475).
Returns true if ClassName is 'Exception' or any subclass of 'Exception'.
Returns false if class is not registered (safe during bootstrap).
""".
-spec is_exception_class(atom()) -> boolean().
is_exception_class(ClassName) ->
    beamtalk_class_registry:inherits_from(ClassName, 'Exception').

-doc """
Check if an error matches the requested exception class.

Handles both raw `#beamtalk_error{}` records and wrapped Exception
tagged maps (ADR 0015). After signal-time wrapping, caught errors are
typically wrapped maps.

- nil → match all (no filter)
- Exception class object → match all (root of hierarchy)
- Error class object → match all (subclass of Exception)
- Other class object → match by error kind atom
- Atom → match by error kind directly
""".
-spec matches_class(term(), term()) -> boolean().
matches_class(nil, _Error) ->
    true;
matches_class(Filter, #{'$beamtalk_class' := _, error := Error}) ->
    matches_class(Filter, Error);
matches_class(#beamtalk_object{class = ClassName}, Error) ->
    matches_class_name(ClassName, Error);
matches_class(ClassName, Error) when is_atom(ClassName) ->
    matches_class_name(ClassName, Error);
matches_class(_Other, _Error) ->
    %% Unknown filter type — catch all for safety
    true.

-doc """
Match by class name atom with hierarchy-aware matching (BT-475).

Derives the error's class from kind_to_class/1, then uses the class
system's superclass chain to check if it matches the requested filter.
If the error's class field is itself an exception class (BT-480:
user-defined error subclasses), uses it directly instead of kind_to_class.
Handles both "ClassName" and "ClassName class" variants (metaclass refs).
Raw Erlang errors (not #beamtalk_error{}) are wrapped first.
""".
-spec matches_class_name(atom(), term()) -> boolean().
matches_class_name(ClassName, #beamtalk_error{kind = Kind, class = ErrorClass}) ->
    %% Strip " class" suffix if present (metaclass reference from class objects)
    BaseName = strip_class_suffix(ClassName),
    %% BT-480: If error.class is an exception class, use it directly.
    %% Otherwise fall back to kind_to_class (built-in error kinds).
    ActualClass =
        case is_exception_class(ErrorClass) of
            true -> ErrorClass;
            false -> kind_to_class(Kind)
        end,
    beamtalk_class_registry:inherits_from(ActualClass, BaseName);
matches_class_name(ClassName, RawError) when not is_map(RawError) ->
    %% Raw Erlang error (e.g. badarith) — wrap to get a kind, then match
    #{'$beamtalk_class' := _, error := Inner} = wrap(RawError),
    matches_class_name(ClassName, Inner);
matches_class_name(_ClassName, _Other) ->
    false.

-doc """
Strip " class" suffix from metaclass names.
e.g., 'RuntimeError class' → 'RuntimeError', 'TypeError' → 'TypeError'
""".
-spec strip_class_suffix(atom()) -> atom().
strip_class_suffix(ClassName) ->
    Str = atom_to_list(ClassName),
    case lists:suffix(" class", Str) of
        % elp:fixme W0023 class name derived from known class tag in exception path
        true -> list_to_atom(lists:sublist(Str, length(Str) - 6));
        false -> ClassName
    end.

-doc """
Wrap a `#beamtalk_error{}` record as an Exception tagged map.

Sets `$beamtalk_class` based on the error kind (BT-452), or uses the
error's class field directly if it is a registered exception subclass
(BT-480: user-defined error subclasses).
""".
-spec wrap(#beamtalk_error{} | term()) -> map().
wrap(#beamtalk_error{kind = Kind, class = ErrorClass} = Error) ->
    Class =
        case is_exception_class(ErrorClass) of
            true -> ErrorClass;
            false -> kind_to_class(Kind)
        end,
    #{'$beamtalk_class' => Class, error => Error};
wrap(Other) ->
    wrap_raw(Other).

-doc """
Map a raw Erlang error reason to its Beamtalk error kind (BT-2707).

The single source of truth for raw-error classification. `wrap_raw/2` uses it to
pick the kind for the error it builds; the runtime-only actor method-error path
(`beamtalk_actor:wrap_method_error/5`) borrows it to classify the kind while
keeping its own richer MFA message — so a raw error surfaces as the *same* kind
regardless of whether the failing method was compiled or runtime-defined.

Buckets: A user-input (`type_error`/`key_error`/`argument_error`), B internal
bugs (`internal_error`), C resource/env (`resource_error`/`process_not_found`/
`timeout_error`); anything unrecognised stays `runtime_error`.
""".
-spec classify_kind(term()) -> atom().
%% Bucket A — user-input
classify_kind(badarith) -> type_error;
classify_kind({badmap, _}) -> type_error;
classify_kind({badkey, _}) -> key_error;
classify_kind(badarg) -> argument_error;
%% Bucket B — internal bugs
classify_kind(function_clause) -> internal_error;
classify_kind(if_clause) -> internal_error;
classify_kind({case_clause, _}) -> internal_error;
classify_kind({badmatch, _}) -> internal_error;
classify_kind({try_clause, _}) -> internal_error;
%% Bucket C — resource/environment
classify_kind(system_limit) -> resource_error;
classify_kind(noproc) -> process_not_found;
classify_kind({noproc, _}) -> process_not_found;
classify_kind(timeout) -> timeout_error;
classify_kind({timeout, _}) -> timeout_error;
%% Fallback
classify_kind(_) -> runtime_error.

-doc """
Wrap a raw Erlang error into a classified Exception tagged map.

Equivalent to `wrap_raw/2` with no dispatch context. See `wrap_raw/2`.
""".
-spec wrap_raw(term()) -> map().
wrap_raw(Reason) ->
    wrap_raw(Reason, #{}).

-doc """
Wrap a raw Erlang error into a well-classified `#beamtalk_error{}` (BT-2707).

Raw Erlang errors (`badarith`, `{badkey,K}`, `function_clause`, …) are sorted
into three buckets that are presented differently (BT-2704):

* **Bucket A — user-input** (`type_error`/`key_error`/`argument_error`):
  `badarith`, `{badkey,K}`, `{badmap,M}`, `badarg`. Carries selector + value
  + an actionable hint.
* **Bucket B — internal bugs** (`internal_error`): `function_clause`,
  `case_clause`, `badmatch`, `if_clause`. Loud, "please report" — NOT dressed
  up as a user mistake (mislabeling a bug as a user error misdirects debugging).
* **Bucket C — resource/env** (`resource_error`/`process_not_found`/
  `timeout_error`): `system_limit`, `noproc`, `timeout`.

`Context` is the dispatch-layer breadcrumb (BT-2705): a map that may carry
`selector` and `class` for the active send. It disambiguates the overloaded
`badarg` and lets bucket-A errors render located messages
(e.g. `Tuple>>sum: …`). It is consulted only on this (error) path, so it adds
nothing to successful dispatch.
""".
-spec wrap_raw(term(), map()) -> map().
wrap_raw({badarity, {Fun, Args}}, _Context) when is_function(Fun), is_list(Args) ->
    {arity, Expected} = erlang:fun_info(Fun, arity),
    Actual = length(Args),
    Message = iolist_to_binary(
        io_lib:format(
            "Wrong number of arguments: block expects ~B but was called with ~B",
            [Expected, Actual]
        )
    ),
    Hint =
        <<"Check that your block has the right number of parameters (e.g., [:x | ...] for 1 argument)">>,
    wrap_classified(arity_mismatch, 'Block', undefined, Message, Hint, #{
        expected_args => Expected, actual_args => Actual
    });
%%% --- Bucket A: user-input errors ----------------------------------------
wrap_raw(badarith, Context) ->
    {Sel, Cls} = breadcrumb(Context),
    Message = located(Sel, Cls, <<"bad arithmetic operation">>),
    Hint =
        <<"Arithmetic requires numbers; check for a non-numeric operand or division by zero.">>,
    wrap_classified(classify_kind(badarith), Cls, Sel, Message, Hint, #{reason => badarith});
wrap_raw({badkey, Key}, Context) ->
    {Sel, Cls} = breadcrumb(Context),
    Core = iolist_to_binary(io_lib:format("key not found: ~tp", [Key])),
    Message = located(Sel, Cls, Core),
    Hint = <<"Use 'includesKey:' to test first, or 'at:ifAbsent:' to supply a default.">>,
    wrap_classified(classify_kind({badkey, Key}), Cls, Sel, Message, Hint, #{key => Key});
wrap_raw({badmap, Value}, Context) ->
    {Sel, Cls} = breadcrumb(Context),
    Message = located(Sel, Cls, <<"expected a Dictionary but got a non-map value">>),
    Hint = <<"This message is only understood by Dictionary values.">>,
    wrap_classified(classify_kind({badmap, Value}), Cls, Sel, Message, Hint, #{value => Value});
wrap_raw(badarg, Context) ->
    %% `badarg` is overloaded (user mistake vs internal misuse) and the bare
    %% error can't tell which. With a dispatch breadcrumb we can locate it as a
    %% user argument error; without one we still give it its own kind rather
    %% than the generic runtime_error catch-all (BT-2707 floor).
    {Sel, Cls} = breadcrumb(Context),
    Message = located(Sel, Cls, <<"invalid argument">>),
    Hint = <<"Check the argument types and values for this message.">>,
    wrap_classified(classify_kind(badarg), Cls, Sel, Message, Hint, #{reason => badarg});
%%% --- Bucket B: internal bugs --------------------------------------------
wrap_raw(Reason, Context) when
    Reason =:= function_clause orelse
        Reason =:= if_clause orelse
        (is_tuple(Reason) andalso tuple_size(Reason) =:= 2 andalso
            (element(1, Reason) =:= case_clause orelse
                element(1, Reason) =:= badmatch orelse
                element(1, Reason) =:= try_clause))
->
    {Sel, Cls} = breadcrumb(Context),
    Tag = internal_reason_tag(Reason),
    Message = located(
        Sel,
        Cls,
        iolist_to_binary(
            io_lib:format("internal error (~s) - this is a bug in Beamtalk", [Tag])
        )
    ),
    Hint = <<"Please report this with the code that triggered it; it should not happen.">>,
    wrap_classified(classify_kind(Reason), Cls, Sel, Message, Hint, #{reason => Reason});
%%% --- Bucket C: resource/environment -------------------------------------
wrap_raw(system_limit, Context) ->
    {Sel, Cls} = breadcrumb(Context),
    Message = located(Sel, Cls, <<"a system limit was reached">>),
    Hint = <<"The VM hit a hard limit (e.g. too many processes, atoms, or ports).">>,
    wrap_classified(classify_kind(system_limit), Cls, Sel, Message, Hint, #{reason => system_limit});
wrap_raw(Reason, Context) when
    Reason =:= noproc orelse
        (is_tuple(Reason) andalso tuple_size(Reason) >= 1 andalso element(1, Reason) =:= noproc)
->
    {Sel, Cls} = breadcrumb(Context),
    Message = located(Sel, Cls, <<"the target process no longer exists">>),
    Hint = <<"The actor or process may have already terminated.">>,
    wrap_classified(classify_kind(Reason), Cls, Sel, Message, Hint, #{reason => Reason});
wrap_raw(Reason, Context) when
    Reason =:= timeout orelse
        (is_tuple(Reason) andalso tuple_size(Reason) >= 1 andalso element(1, Reason) =:= timeout)
->
    {Sel, Cls} = breadcrumb(Context),
    Message = located(Sel, Cls, <<"the operation timed out">>),
    Hint = <<"The operation did not complete within its time limit.">>,
    wrap_classified(classify_kind(Reason), Cls, Sel, Message, Hint, #{reason => Reason});
%%% --- Fallback: unclassified ---------------------------------------------
wrap_raw(Other, Context) ->
    {Sel, Cls} = breadcrumb(Context),
    wrap_classified(
        classify_kind(Other),
        Cls,
        Sel,
        iolist_to_binary(io_lib:format("~p", [Other])),
        undefined,
        #{reason => Other}
    ).

%% Build a classified Exception tagged map from explicit error parts.
-spec wrap_classified(
    atom(), atom() | undefined, atom() | undefined, binary(), binary() | undefined, map()
) ->
    map().
wrap_classified(Kind, Class, Selector, Message, Hint, Details) ->
    %% `Class` is the *receiver* breadcrumb (already baked into the located
    %% Message). Do not store it as the error's class when it is itself an
    %% exception class: matches_class_name/2 treats #beamtalk_error.class as
    %% authoritative for hierarchy matching (BT-480), so a raw error that merely
    %% occurred on an Exception-subclass receiver must not hijack catch matching.
    %% Fall back to undefined there, letting the kind-derived class drive matching.
    RecordClass =
        case Class =/= undefined andalso is_exception_class(Class) of
            true -> undefined;
            false -> Class
        end,
    GenError = #beamtalk_error{
        kind = Kind,
        class = RecordClass,
        selector = Selector,
        message = Message,
        hint = Hint,
        details = Details
    },
    #{'$beamtalk_class' => kind_to_class(Kind), error => GenError}.

%% Extract the {Selector, Class} breadcrumb from a dispatch context (BT-2705).
-spec breadcrumb(map()) -> {atom() | undefined, atom() | undefined}.
breadcrumb(Context) ->
    {maps:get(selector, Context, undefined), maps:get(class, Context, undefined)}.

%% Prefix a core error message with its location (class/selector) when known.
-spec located(atom() | undefined, atom() | undefined, binary()) -> binary().
located(undefined, _Class, Core) ->
    Core;
%% ~ts (not ~s): an error formatter must never itself crash, even on an
%% interop-supplied class/selector atom carrying non-Latin1 codepoints.
located(Selector, undefined, Core) ->
    iolist_to_binary(io_lib:format("'~ts': ~ts", [Selector, Core]));
located(Selector, Class, Core) ->
    iolist_to_binary(io_lib:format("~ts>>~ts: ~ts", [Class, Selector, Core])).

%% Human-readable tag for a bucket-B internal reason.
-spec internal_reason_tag(term()) -> atom().
internal_reason_tag(Reason) when is_atom(Reason) -> Reason;
internal_reason_tag(Reason) when is_tuple(Reason) -> element(1, Reason).

-doc """
Idempotent exception wrapper (ADR 0015).

Ensures the value is a wrapped Exception tagged map. If already wrapped,
passes through unchanged. Used in `on:do:` catch clauses to handle both
pre-wrapped exceptions (from raise/1) and raw Erlang exceptions.
""".
-spec ensure_wrapped(term()) -> map().
ensure_wrapped(#{'$beamtalk_class' := _} = Already) ->
    Already;
ensure_wrapped(Other) ->
    wrap(Other).

-doc """
Idempotent exception wrapper with stacktrace capture (BT-107).

Like ensure_wrapped/1 but also stores the Erlang stacktrace as a list
of StackFrame objects on the exception tagged map.
""".
-spec ensure_wrapped(term(), list()) -> map().
ensure_wrapped(#{'$beamtalk_class' := _} = Already, Stacktrace) ->
    Already#{stacktrace => beamtalk_stack_frame:wrap(Stacktrace)};
ensure_wrapped(Other, Stacktrace) ->
    Wrapped = wrap(Other),
    Wrapped#{stacktrace => beamtalk_stack_frame:wrap(Stacktrace)}.

-doc """
Idempotent exception wrapper with Erlang exception type (BT-728).

Like ensure_wrapped/2 but also accepts the Erlang exception class atom
(error, exit, throw) from the try/catch <Type, Error, Stack> triple.
Maps exit → erlang_exit kind, throw → erlang_throw kind so that
ExitError/ThrowError classes are used instead of RuntimeError.
""".
-spec ensure_wrapped(atom(), term(), list()) -> map().
ensure_wrapped(_Type, #{'$beamtalk_class' := _} = Already, Stacktrace) ->
    Already#{stacktrace => beamtalk_stack_frame:wrap(Stacktrace)};
ensure_wrapped(_Type, #beamtalk_error{} = Error, Stacktrace) ->
    Wrapped = wrap(Error),
    Wrapped#{stacktrace => beamtalk_stack_frame:wrap(Stacktrace)};
ensure_wrapped(error, undef, Stacktrace) ->
    Message =
        case Stacktrace of
            [{Mod, Fun, ArgsOrArity, _} | _] ->
                Arity =
                    if
                        is_list(ArgsOrArity) -> length(ArgsOrArity);
                        is_integer(ArgsOrArity) -> ArgsOrArity;
                        true -> 0
                    end,
                iolist_to_binary(
                    io_lib:format("Undefined function: ~p:~p/~p", [Mod, Fun, Arity])
                );
            _ ->
                <<"Undefined function">>
        end,
    GenError = #beamtalk_error{
        kind = runtime_error,
        class = undefined,
        selector = undefined,
        message = Message,
        hint = <<"Check that the module is loaded and the function exists">>,
        details = #{reason => undef}
    },
    Wrapped = #{'$beamtalk_class' => kind_to_class(runtime_error), error => GenError},
    Wrapped#{stacktrace => beamtalk_stack_frame:wrap(Stacktrace)};
ensure_wrapped(exit, Reason, Stacktrace) ->
    Error = #beamtalk_error{
        kind = erlang_exit,
        class = undefined,
        selector = undefined,
        message = iolist_to_binary(io_lib:format("~p", [Reason])),
        hint = undefined,
        details = #{reason => Reason}
    },
    Wrapped = wrap(Error),
    Wrapped#{stacktrace => beamtalk_stack_frame:wrap(Stacktrace)};
ensure_wrapped(throw, Reason, Stacktrace) ->
    %% BT-869: Unwrap future_rejected errors (thrown from beamtalk_future:await/*)
    %% Future rejects with {future_rejected, ActualError} where ActualError may be:
    %%   - #beamtalk_error{}           (old hand-written gen_servers via beamtalk_future:reject/2)
    %%   - #{error := #beamtalk_error{}} (already-wrapped map, e.g. from compiled actors)
    %%   - {error, #{error := #beamtalk_error{}}} (compiled actor safe_dispatch catches {error, Wrapped})
    %% We need to unwrap it so the actual error can be caught properly
    case Reason of
        {future_rejected, #beamtalk_error{} = InnerError} ->
            Wrapped = wrap(InnerError);
        {future_rejected, #{error := #beamtalk_error{}} = AlreadyWrapped} ->
            Wrapped = AlreadyWrapped;
        {future_rejected, {error, #{error := #beamtalk_error{}} = AlreadyWrapped}} ->
            Wrapped = AlreadyWrapped;
        _ ->
            Error = #beamtalk_error{
                kind = erlang_throw,
                class = undefined,
                selector = undefined,
                message = iolist_to_binary(io_lib:format("~p", [Reason])),
                hint = undefined,
                details = #{reason => Reason}
            },
            Wrapped = wrap(Error)
    end,
    Wrapped#{stacktrace => beamtalk_stack_frame:wrap(Stacktrace)};
ensure_wrapped(_Type, Other, Stacktrace) ->
    Wrapped = wrap(Other),
    Wrapped#{stacktrace => beamtalk_stack_frame:wrap(Stacktrace)}.

-doc """
Idempotent exception wrapper with a dispatch breadcrumb (BT-2705).

Like ensure_wrapped/3 but threads a dispatch `Context` (a map that may carry
`selector` and `class` for the active send) into `wrap_raw/2`, so raw Erlang
errors escaping a send are classified *and* located.

The breadcrumb is consulted only here, on the error path, so successful
dispatch is unaffected. Already-wrapped exceptions pass through untouched so
the *innermost* frame's classification wins — an outer frame never overwrites a
more specific inner one. `undef`, `exit`, `throw`, and `#beamtalk_error{}` keep
their existing ensure_wrapped/3 handling.
""".
-spec ensure_wrapped(atom(), term(), list(), map()) -> map().
ensure_wrapped(_Type, #{'$beamtalk_class' := _} = Already, Stacktrace, _Context) ->
    %% Innermost classification already applied — preserve it and any stacktrace it
    %% already carries. Backfill from this frame only when absent, so stacktrace
    %% capture never regresses versus ensure_wrapped/3 (which always attached one).
    case Already of
        #{stacktrace := _} -> Already;
        _ -> Already#{stacktrace => beamtalk_stack_frame:wrap(Stacktrace)}
    end;
ensure_wrapped(error, Reason, Stacktrace, Context) when
    map_size(Context) > 0, Reason =/= undef, not is_record(Reason, beamtalk_error)
->
    Wrapped = wrap_raw(Reason, Context),
    Wrapped#{stacktrace => beamtalk_stack_frame:wrap(Stacktrace)};
ensure_wrapped(Type, Reason, Stacktrace, _Context) ->
    ensure_wrapped(Type, Reason, Stacktrace).

-doc """
Dispatch a message to an Exception object.

Exception objects expose the underlying `#beamtalk_error{}` fields.
Matches any exception hierarchy class (Exception, Error, RuntimeError, etc.).
""".
-spec dispatch(atom(), list(), map()) -> term().
dispatch('message', [], #{error := Error}) ->
    Error#beamtalk_error.message;
dispatch('hint', [], #{error := Error}) ->
    case Error#beamtalk_error.hint of
        undefined -> nil;
        Hint -> Hint
    end;
dispatch('kind', [], #{error := Error}) ->
    Error#beamtalk_error.kind;
dispatch('selector', [], #{error := Error}) ->
    case Error#beamtalk_error.selector of
        undefined -> nil;
        Selector -> Selector
    end;
dispatch('errorClass', [], #{error := Error}) ->
    Error#beamtalk_error.class;
dispatch('printString', [], #{error := Error}) ->
    beamtalk_error:format(Error);
dispatch('stackTrace', [], #{stacktrace := Frames}) ->
    Frames;
dispatch('stackTrace', [], _) ->
    %% No stacktrace captured (e.g., signal-time error without catch)
    [];
dispatch('class', [], #{'$beamtalk_class' := Class}) ->
    Class;
dispatch('signal', [], #{error := Error}) ->
    beamtalk_error:raise(Error);
dispatch('signal', [], #{'$beamtalk_class' := ClassName}) ->
    %% BT-480: New exception instance (no error field yet) — create and raise
    signal_from_class(ClassName);
dispatch('signal:', [Message], #{'$beamtalk_class' := ClassName}) ->
    %% BT-480: Preserve exception class name from the signaling object
    signal_message(Message, ClassName).

-doc "Check if Exception responds to a selector.".
-spec has_method(atom()) -> boolean().
has_method('message') -> true;
has_method('hint') -> true;
has_method('kind') -> true;
has_method('selector') -> true;
has_method('errorClass') -> true;
has_method('printString') -> true;
has_method('stackTrace') -> true;
has_method('class') -> true;
has_method('signal') -> true;
has_method('signal:') -> true;
has_method(_) -> false.

-doc """
Raise an exception.

Used internally for various error signaling scenarios.
""".
-spec signal(Kind :: atom()) -> no_return().
signal(Kind) when is_atom(Kind) ->
    Error = beamtalk_error:new(Kind, 'Exception'),
    beamtalk_error:raise(Error).

-doc """
Raise a new Error exception with the given message.

Usage: `Exception signal: 'something went wrong'`
""".
-spec signal_message(term()) -> no_return().
signal_message(Message) when is_binary(Message) ->
    Error = #beamtalk_error{
        kind = signal,
        class = undefined,
        selector = undefined,
        message = Message,
        hint = undefined,
        details = #{}
    },
    beamtalk_error:raise(Error);
signal_message(Message) when is_atom(Message) ->
    signal_message(atom_to_binary(Message, utf8));
signal_message(Message) ->
    %% Convert other types to binary for robustness
    signal_message(iolist_to_binary(io_lib:format("~p", [Message]))).

-doc """
Raise a new exception with a message, preserving the exception class.

BT-480: Used when signal: is called on a user-defined exception instance.
The exception class is taken from the signaling object's $beamtalk_class.
""".
-spec signal_message(term(), atom()) -> no_return().
signal_message(Message, ExceptionClass) when is_binary(Message) ->
    Error = #beamtalk_error{
        kind = class_to_kind(ExceptionClass),
        class = ExceptionClass,
        selector = undefined,
        message = Message,
        hint = undefined,
        details = #{}
    },
    beamtalk_error:raise(Error);
signal_message(Message, ExceptionClass) when is_atom(Message) ->
    signal_message(atom_to_binary(Message, utf8), ExceptionClass);
signal_message(Message, ExceptionClass) ->
    signal_message(iolist_to_binary(io_lib:format("~p", [Message])), ExceptionClass).

-doc """
Raise a new exception from a class instance without a message.

BT-480: Used when signal (no args) is called on a new exception instance
that has $beamtalk_class but no error field yet.
""".
-spec signal_from_class(atom()) -> no_return().
signal_from_class(ClassName) ->
    Error = #beamtalk_error{
        kind = class_to_kind(ClassName),
        class = ClassName,
        selector = undefined,
        message = atom_to_binary(ClassName, utf8),
        hint = undefined,
        details = #{}
    },
    beamtalk_error:raise(Error).

-doc """
Class-side signal: raise an exception with message from a class object.

BT-1524: Called by `Exception signal: "msg"` or `MyCustomError signal: "msg"`.
Extracts the class name from the ClassSelf record and delegates to signal_message/2.
""".
-spec class_signal_message(term(), #beamtalk_object{} | term()) -> no_return().
class_signal_message(Message, #beamtalk_object{class = ClassTag}) ->
    ClassName = strip_class_suffix(ClassTag),
    signal_message(Message, ClassName);
class_signal_message(Message, _ClassSelf) ->
    %% Fallback: if ClassSelf is not a beamtalk_object, signal as generic Exception
    signal_message(Message, 'Exception').

-doc """
Class-side signal: raise an exception from a class object (no message).

BT-1524: Called by `Exception signal` or `MyCustomError signal`.
Extracts the class name from the ClassSelf record and delegates to signal_from_class/1.
""".
-spec class_signal(#beamtalk_object{} | term()) -> no_return().
class_signal(#beamtalk_object{class = ClassTag}) ->
    ClassName = strip_class_suffix(ClassTag),
    signal_from_class(ClassName);
class_signal(_ClassSelf) ->
    signal_from_class('Exception').
