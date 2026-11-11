%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Object System Context

-module(beamtalk_error).
-moduledoc """
Error construction and formatting helpers.

This module provides helper functions for creating and formatting
beamtalk_error records. All runtime errors should use these helpers
to ensure consistent error messages and structure.

See docs/internal/design-self-as-object.md Section 3.8 for error taxonomy.
""".
-include("beamtalk.hrl").

-export([
    chars_to_binary/1,
    new/2,
    new/3,
    new/4,
    with_message/2,
    render_text/1,
    with_selector/2,
    with_hint/2,
    with_details/2,
    format/1,
    format_safe/1,
    format_safe/2,
    extract_beamtalk_error/2,
    raise/1,
    raise_type_error/3,
    generate_message/3,
    format_reason/2,
    is_does_not_understand/1,
    is_unknown_field/1
]).

%% Type definition for the internal error record.
-type error() :: #beamtalk_error{}.

-doc """
Type representing a Beamtalk Exception tagged map (the wrapped form
visible to Beamtalk code and returned inside Result error values).

```erlang
-spec my_function(term()) -> beamtalk_result:t(binary(), beamtalk_error:t()).
```
""".
-type t() :: #{
    '$beamtalk_class' := atom(),
    error := #beamtalk_error{}
}.
-export_type([error/0, t/0]).

-doc """
Create a new error with the specified kind and class.

The message is generated automatically based on kind and class.
Use with_selector/2, with_hint/2, and with_details/2 to add more context.

Example:
  Error = beamtalk_error:new(does_not_understand, 'Integer')
""".
-spec new(atom(), atom()) -> #beamtalk_error{}.
new(Kind, Class) ->
    %% BT-4170: error messages are `String` values (tagged), so `.bt`
    %% readers (`e message`) get text that dispatches as text.
    Message = beamtalk_primitive:string_from_binary(generate_message(Kind, Class, undefined)),
    #beamtalk_error{
        kind = Kind,
        class = Class,
        selector = undefined,
        message = Message,
        hint = undefined,
        details = #{}
    }.

-doc "Create a new error with kind, class, and selector.".
-spec new(atom(), atom(), atom()) -> #beamtalk_error{}.
new(Kind, Class, Selector) ->
    with_selector(new(Kind, Class), Selector).

-doc "Create a new error with kind, class, selector, and hint.".
-spec new(atom(), atom(), atom(), term()) -> #beamtalk_error{}.
new(Kind, Class, Selector, Hint) ->
    with_hint(new(Kind, Class, Selector), Hint).

-doc """
Set the message on an existing error.

Overrides the auto-generated message with a custom one.
Used by Object>>error: for user-supplied error messages.

Example:
  Error0 = beamtalk_error:new(user_error, 'Counter'),
  Error = beamtalk_error:with_message(Error0, <<"invalid state">>)
""".
-spec with_message(#beamtalk_error{}, term()) -> #beamtalk_error{}.
with_message(Error, {beamtalk_string, _} = Message) ->
    Error#beamtalk_error{message = Message};
with_message(Error, Message) when is_binary(Message) ->
    %% BT-4170: raw binaries wrap unchecked — callers pass `~s`-formatted
    %% ASCII or already-validated text (generate_message/3, repl_errors);
    %% arbitrary bytes must go through a validating boundary first.
    Error#beamtalk_error{message = beamtalk_primitive:string_from_binary(Message)};
with_message(Error, Message) when is_atom(Message) ->
    Error#beamtalk_error{
        message = beamtalk_primitive:string_from_binary(atom_to_binary(Message, utf8))
    };
with_message(Error, Message) ->
    Error#beamtalk_error{
        message = beamtalk_primitive:string_from_binary(
            chars_to_binary(io_lib:format("~p", [Message]))
        )
    }.

-doc """
Add a selector to an existing error.

This is used when the error is related to a specific method call.
The message is regenerated to include the selector.

Example:
  Error0 = beamtalk_error:new(does_not_understand, 'Integer'),
  Error = beamtalk_error:with_selector(Error0, 'foo')
""".
-spec with_selector(#beamtalk_error{}, atom()) -> #beamtalk_error{}.
with_selector(#beamtalk_error{kind = Kind, class = Class} = Error, Selector) ->
    %% BT-4170: see new/2 — messages are tagged `String` values.
    Message = beamtalk_primitive:string_from_binary(generate_message(Kind, Class, Selector)),
    Error#beamtalk_error{
        selector = Selector,
        message = Message
    }.

-doc """
Add a hint to an existing error.

Hints provide actionable suggestions for fixing the error.

Example:
  Error0 = beamtalk_error:new(does_not_understand, 'Integer'),
  Error = beamtalk_error:with_hint(Error0, <<"Check spelling">>)
""".
-spec with_hint(#beamtalk_error{}, term()) -> #beamtalk_error{}.
with_hint(Error, undefined) ->
    Error;
with_hint(Error, {beamtalk_string, _} = Hint) ->
    Error#beamtalk_error{hint = Hint};
with_hint(Error, Hint) when is_binary(Hint) ->
    %% BT-4170: hints are `String` values (tagged); raw binaries wrap
    %% unchecked — see with_message/2 for the precondition.
    Error#beamtalk_error{hint = beamtalk_primitive:string_from_binary(Hint)};
with_hint(Error, Hint) when is_atom(Hint) ->
    Error#beamtalk_error{
        hint = beamtalk_primitive:string_from_binary(atom_to_binary(Hint, utf8))
    };
with_hint(Error, Hint) ->
    Error#beamtalk_error{
        hint = beamtalk_primitive:string_from_binary(
            chars_to_binary(io_lib:format("~tp", [Hint]))
        )
    }.

-doc """
Add additional context details to an error.

Details is a map with additional context like arity, expected types, etc.

Example:
  Error0 = beamtalk_error:new(arity_mismatch, 'Counter'),
  Error = beamtalk_error:with_details(Error0, #{expected => 1, got => 0})
""".
-spec with_details(#beamtalk_error{}, map()) -> #beamtalk_error{}.
with_details(Error, Details) ->
    Error#beamtalk_error{details = Details}.

-doc """
Format an error for user-facing display.

Returns a formatted error message as a binary, suitable for printing
or returning to the user. Uses user-facing names (e.g., 'self' not 'Self').

Always returns a raw binary (Erlang-side rendering, not a `String`
value): callers that surface it to `.bt` as text must wrap it via
`beamtalk_primitive:string_from_binary/1`.

Example:
  beamtalk_error:format(Error)
  % => <<"Integer does not understand 'foo'\nHint: Check spelling">>
""".
-spec format(#beamtalk_error{}) -> binary().
format(#beamtalk_error{kind = does_not_understand} = Error) ->
    Enriched = maybe_enrich_dnu_hint(Error),
    format_parts(Enriched#beamtalk_error.message, Enriched#beamtalk_error.hint);
format(#beamtalk_error{message = Message, hint = Hint, details = #{signal_reason := Reason}}) ->
    %% BT-4159: `signal:reason:` — show the reason in uncaught reports.
    chars_to_binary([
        format_parts(Message, Hint), <<"\nReason: ">>, format_reason_term(Reason)
    ]);
format(#beamtalk_error{message = Message, hint = Hint}) ->
    format_parts(Message, Hint).

-doc """
Safely format any error term as a human-readable binary.

If the term is a `#beamtalk_error{}`, uses `format/1` to produce a rich
message with hints. For any other term, falls back to `~0p` formatting.
Never crashes — used by generated `handle_continue` to log initialize failures.
""".
-spec format_safe(term()) -> binary().
format_safe(#beamtalk_error{} = Error) ->
    try
        format(Error)
    catch
        _:_ -> chars_to_binary(io_lib:format("~0p", [Error]))
    end;
format_safe(Term) ->
    chars_to_binary(io_lib:format("~0p", [Term])).

-doc """
Safely format an error term, searching the stacktrace for a
`#beamtalk_error{}` if the primary reason is not informative (e.g.,
`{error, function_clause}`).

Used by generated `handle_continue` to produce useful log messages even
when the Erlang-level reason is a raw function_clause or case_clause.
""".
-spec format_safe(term(), term()) -> binary().
format_safe(#beamtalk_error{} = Error, _Stacktrace) ->
    format_safe(Error);
format_safe(#{'$beamtalk_class' := _, error := #beamtalk_error{} = Error}, _Stacktrace) ->
    format_safe(Error);
format_safe(Reason, Stacktrace) ->
    %% Try to extract a beamtalk_error from the stacktrace args
    case extract_beamtalk_error(Stacktrace, 4) of
        undefined ->
            %% Also try the reason itself (may be nested)
            case extract_beamtalk_error(Reason, 3) of
                undefined -> format_safe(Reason);
                Error -> format_safe(Error)
            end;
        Error ->
            format_safe(Error)
    end.

-doc """
Recursively search a term for a `#beamtalk_error{}` record.
Depth-limited to avoid traversing enormous state maps.
""".
-spec extract_beamtalk_error(term(), non_neg_integer()) -> #beamtalk_error{} | undefined.
%% Direct matches succeed at any depth — depth only limits further recursion
extract_beamtalk_error(#beamtalk_error{} = E, _Depth) ->
    E;
extract_beamtalk_error(#{error := #beamtalk_error{} = E}, _Depth) ->
    E;
extract_beamtalk_error(_Term, 0) ->
    undefined;
extract_beamtalk_error(T, Depth) when is_tuple(T) ->
    extract_from_tuple(T, 1, tuple_size(T), Depth - 1);
extract_beamtalk_error([H | T], Depth) ->
    case extract_beamtalk_error(H, Depth - 1) of
        undefined -> extract_beamtalk_error(T, Depth);
        Found -> Found
    end;
extract_beamtalk_error(_, _Depth) ->
    undefined.

%% Scan tuple elements for a beamtalk_error.
-spec extract_from_tuple(tuple(), pos_integer(), non_neg_integer(), non_neg_integer()) ->
    #beamtalk_error{} | undefined.
extract_from_tuple(_T, I, Size, _Depth) when I > Size ->
    undefined;
extract_from_tuple(T, I, Size, Depth) ->
    case extract_beamtalk_error(element(I, T), Depth) of
        undefined -> extract_from_tuple(T, I + 1, Size, Depth);
        Found -> Found
    end.

%% BT-4159: render a `signal:reason:` term. Strings print quoted; everything
%% else uses `~0tp` (pure formatting; never messages an actor).
-spec format_reason_term(term()) -> iodata().
format_reason_term({beamtalk_string, _} = Str) ->
    beamtalk_primitive:print_string(Str);
format_reason_term(Reason) ->
    io_lib:format("~0tp", [Reason]).

%% Format message with optional hint.
-spec format_parts(term(), term()) -> binary().
format_parts(Message, undefined) ->
    render_text(Message);
format_parts(Message, Hint) ->
    iolist_to_binary([render_text(Message), <<"\nHint: ">>, render_text(Hint)]).

-doc """
Render a message/hint field to raw bytes for `format/1`.

BT-4170: fields built via `new/2`/`with_message/2`/`with_hint/2` are
tagged `String` values and unwrap here; a hand-built record carrying a
raw binary still renders (defensive — records can be constructed
literally, bypassing the setters).
""".
-spec render_text(term()) -> binary().
render_text({beamtalk_string, Bin}) when is_binary(Bin) -> Bin;
render_text(Bin) when is_binary(Bin) -> Bin;
render_text(Other) -> chars_to_binary(io_lib:format("~tp", [Other])).

-doc """
Wrap an error as an Exception object and raise it.

This is the canonical way to signal errors in the Beamtalk runtime.
It wraps the `#beamtalk_error{}` record as an Exception tagged map
before throwing, ensuring all exceptions are Beamtalk objects at
signal time (ADR 0015).

Example:
  Error = beamtalk_error:new(does_not_understand, 'Integer'),
  beamtalk_error:raise(Error)
""".
-spec raise(#beamtalk_error{}) -> no_return().
raise(#beamtalk_error{} = Error) ->
    Wrapped = beamtalk_exception_handler:wrap(Error),
    error(Wrapped).

-doc "Raise a type_error for a given class, selector, and hint term. Shared helper used by stdlib modules.".
-spec raise_type_error(atom(), atom(), term()) -> no_return().
raise_type_error(Class, Selector, Hint) ->
    raise(new(type_error, Class, Selector, Hint)).

-doc """
Predicate: does this error represent a genuine "selector not found anywhere
in the hierarchy" (`does_not_understand`)?

`beamtalk_dispatch:super/5` (and the `lookup/5` path it shares via
`lookup_in_class_chain/5`) returns `{error, E}` for two
semantically different situations — the selector was never found, or the
selector was found and invoked but the method itself raised. Generated
dispatch code uses this predicate to tell them apart instead of pattern
matching the record's raw tuple layout, so it can re-raise real errors
instead of masking them as a fabricated `does_not_understand`.

A method that was invoked and raised answers the wrapped exception map
(BT-4593: the walk never unwraps a raise back to a record). That is never a
lookup miss, so it answers `false` — even when the raised error is itself a
`does_not_understand` from inside the method body.

Total: any other term answers `false` too, like `is_unknown_field/1`. A `^`
non-local return or a `Program exit:` throw never reaches here as an
`{error, _}` result (the walk and the value-class `dispatch/4` adapter
re-raise them), so a non-record is always a real error to re-raise.

Example:
  beamtalk_error:is_does_not_understand(beamtalk_error:new(does_not_understand, 'Integer'))
  % => true
""".
-spec is_does_not_understand(term()) -> boolean().
is_does_not_understand(#beamtalk_error{kind = does_not_understand}) -> true;
is_does_not_understand(_) -> false.

-doc """
Predicate: is this error an `unknown_field` initialiser rejection?

Generated actor `spawn/0` and `spawn/1` use this to tell `init/1`'s own
`check_init_args/5` rejection (re-raised transparently, BT-4030) apart
from every other start failure (still wrapped as `instantiation_error`,
e.g. a throwing `initialize` — see `actor_initialize_test.bt`).

Total: `undefined` (no error found in the start reason) and any other
term answer `false`, so generated code can call it directly on
`extract_beamtalk_error/2`'s result.
""".
-spec is_unknown_field(term()) -> boolean().
is_unknown_field(#beamtalk_error{kind = unknown_field}) -> true;
is_unknown_field(_) -> false.

-doc """
Generate a human-readable error message from kind, class, and optional selector.

Selector is normally an atom, but callers that only have an unregistered
method name as a binary (e.g. `binary_to_existing_atom/2` failed, so there is
no atom to attach to the record's `selector` field) may pass a binary
instead — both format identically via `~ts`. This keeps the
canonical `does_not_understand` wording (quoted selector) reachable from
call sites that can't safely mint a new atom.
""".
-spec generate_message(atom(), atom(), atom() | binary() | undefined) -> binary().
generate_message(does_not_understand, Class, undefined) ->
    chars_to_binary(io_lib:format("~ts does not understand message", [Class]));
generate_message(does_not_understand, Class, Selector) ->
    chars_to_binary(io_lib:format("~ts does not understand '~ts'", [Class, Selector]));
generate_message(immutable_value, Class, undefined) ->
    chars_to_binary(io_lib:format("Cannot mutate ~ts (immutable value)", [Class]));
generate_message(immutable_value, Class, Selector) ->
    chars_to_binary(io_lib:format("Cannot call '~ts' on ~ts (immutable value)", [Selector, Class]));
generate_message(stateful_block_dispatch, Class, undefined) ->
    chars_to_binary(
        io_lib:format("~ts cannot be invoked via perform: with these arguments", [Class])
    );
generate_message(stateful_block_dispatch, Class, Selector) ->
    %% The underlying check (erlang:is_function/2 arity
    %% comparison) cannot distinguish "this block captures mutable state and
    %% needs StateAcc threading" from "this block was simply called with the
    %% wrong argument count" — both shapes present as the same raw fun arity.
    %% Don't assert a specific cause the check hasn't actually confirmed.
    chars_to_binary(
        io_lib:format(
            "Cannot call '~ts' on ~ts via perform: (wrong number of arguments, or the block captures mutable state and must be invoked directly)",
            [Selector, Class]
        )
    );
generate_message(arity_mismatch, Class, undefined) ->
    chars_to_binary(io_lib:format("Wrong number of arguments to ~ts method", [Class]));
generate_message(arity_mismatch, Class, Selector) ->
    chars_to_binary(io_lib:format("Wrong number of arguments to '~ts' on ~ts", [Selector, Class]));
generate_message(type_error, Class, undefined) ->
    chars_to_binary(io_lib:format("Type error in ~ts", [Class]));
generate_message(type_error, Class, Selector) ->
    chars_to_binary(io_lib:format("Type error in '~ts' on ~ts", [Selector, Class]));
%% Empty-collection access is a distinct condition from a dispatch
%% failure — the receiver understands the selector, it just has no element to
%% answer. Naming it lets `on:do:` discriminate it from a genuine typo.
generate_message(empty_collection, Class, undefined) ->
    chars_to_binary(io_lib:format("~ts is empty", [Class]));
generate_message(empty_collection, Class, Selector) ->
    %% Selector is quoted because keyword selectors already end in `:` —
    %% an unquoted `at:` would render as `at:: List is empty`.
    chars_to_binary(io_lib:format("'~ts': ~ts is empty", [Selector, Class]));
generate_message(index_out_of_bounds, Class, undefined) ->
    chars_to_binary(io_lib:format("Index out of bounds for ~ts", [Class]));
generate_message(index_out_of_bounds, Class, Selector) ->
    chars_to_binary(io_lib:format("'~ts': index out of bounds for ~ts", [Selector, Class]));
%% A lookup that ran to completion without finding anything.
%% `key_error` was the closest existing kind for `List detect:` but names a key
%% the receiver was asked for, which a predicate search has none of; and
%% `empty_collection` says the wrong thing when the collection is full of
%% non-matches.
%%
%% Deliberately generic wording: `not_found` also covers non-collection
%% receivers (`AtomicCounter named:`, `Ets named:`, `Beamtalk`), which
%% otherwise fall through to the `~ts error in ...` fallback. A collection-
%% flavoured "no matching element" would read as nonsense on those. The
%% specifics belong in the hint, which every call site already sets.
generate_message(not_found, Class, undefined) ->
    chars_to_binary(io_lib:format("Not found in ~ts", [Class]));
generate_message(not_found, Class, Selector) ->
    %% Selector is quoted because keyword selectors already end in `:`.
    chars_to_binary(io_lib:format("'~ts': not found in ~ts", [Selector, Class]));
generate_message(actor_dead, Class, undefined) ->
    chars_to_binary(io_lib:format("~ts actor process has terminated", [Class]));
generate_message(actor_dead, Class, Selector) ->
    chars_to_binary(
        io_lib:format("Cannot send '~ts' to ~ts (actor process has terminated)", [Selector, Class])
    );
%% ADR 0126 §7.1: the target node is unreachable — down, partitioned, or
%% never connected — distinct from actor_dead (the node answered, the
%% actor specifically is gone). Class is 'unknown' at every call site
%% (a partition is discovered before the actor's class can be resolved),
%% mirroring actor_dead's own generic wording above.
generate_message(node_down, Class, undefined) ->
    chars_to_binary(io_lib:format("~ts: node is unreachable", [Class]));
generate_message(node_down, Class, Selector) ->
    chars_to_binary(
        io_lib:format("Cannot send '~ts' to ~ts (node is unreachable)", [Selector, Class])
    );
generate_message(future_not_awaited, _Class, undefined) ->
    chars_to_binary(io_lib:format("Sent message to a Future", []));
generate_message(future_not_awaited, _Class, Selector) ->
    chars_to_binary(io_lib:format("Sent '~ts' to a Future", [Selector]));
generate_message(instantiation_error, Class, undefined) ->
    chars_to_binary(io_lib:format("Cannot instantiate ~ts", [Class]));
generate_message(instantiation_error, Class, Selector) ->
    chars_to_binary(io_lib:format("Cannot call '~ts' on ~ts", [Selector, Class]));
generate_message(file_not_found, Class, undefined) ->
    chars_to_binary(io_lib:format("~ts: file not found", [Class]));
generate_message(file_not_found, Class, Selector) ->
    chars_to_binary(io_lib:format("~ts '~ts': file not found", [Class, Selector]));
generate_message(permission_denied, Class, undefined) ->
    chars_to_binary(io_lib:format("~ts: permission denied", [Class]));
generate_message(permission_denied, Class, Selector) ->
    chars_to_binary(io_lib:format("~ts '~ts': permission denied", [Class, Selector]));
generate_message(io_error, Class, undefined) ->
    chars_to_binary(io_lib:format("~ts: I/O error", [Class]));
generate_message(io_error, Class, Selector) ->
    chars_to_binary(io_lib:format("~ts '~ts': I/O error", [Class, Selector]));
generate_message(user_error, _Class, undefined) ->
    <<"Error">>;
generate_message(user_error, _Class, Selector) ->
    chars_to_binary(io_lib:format("~p", [Selector]));
generate_message(stdlib_shadowing, Class, undefined) ->
    chars_to_binary(io_lib:format("Cannot redefine stdlib class '~ts'", [Class]));
generate_message(stdlib_shadowing, Class, Selector) ->
    chars_to_binary(
        io_lib:format("Cannot redefine stdlib class '~ts' (via '~ts')", [Class, Selector])
    );
generate_message(class_not_found, Class, undefined) ->
    chars_to_binary(io_lib:format("Class '~ts' not found", [Class]));
generate_message(class_not_found, Class, Selector) ->
    chars_to_binary(
        io_lib:format("Class '~ts' not found (while resolving '~ts')", [Class, Selector])
    );
generate_message(no_superclass, Class, undefined) ->
    chars_to_binary(io_lib:format("~ts has no superclass", [Class]));
generate_message(no_superclass, Class, Selector) ->
    chars_to_binary(
        io_lib:format("~ts has no superclass (cannot resolve '~ts' via super)", [Class, Selector])
    );
generate_message(class_already_exists, Class, undefined) ->
    chars_to_binary(io_lib:format("Class '~ts' already exists", [Class]));
generate_message(class_already_exists, Class, Selector) ->
    chars_to_binary(io_lib:format("Class '~ts' already exists (via '~ts')", [Class, Selector]));
generate_message(internal_error, Class, undefined) ->
    chars_to_binary(io_lib:format("Internal error in ~ts", [Class]));
generate_message(internal_error, Class, Selector) ->
    chars_to_binary(io_lib:format("Internal error in '~ts' on ~ts", [Selector, Class]));
generate_message(dispatch_error, Class, undefined) ->
    chars_to_binary(io_lib:format("Dispatch error for ~ts", [Class]));
generate_message(dispatch_error, Class, Selector) ->
    chars_to_binary(io_lib:format("Dispatch error for '~ts' on ~ts", [Selector, Class]));
generate_message(callback_failed, Class, undefined) ->
    chars_to_binary(io_lib:format("Callback failed for ~ts", [Class]));
generate_message(callback_failed, Class, Selector) ->
    chars_to_binary(io_lib:format("Callback '~ts' failed for ~ts", [Selector, Class]));
generate_message(uninitialized_state_error, Class, undefined) ->
    chars_to_binary(io_lib:format("UninitializedStateError in ~ts", [Class]));
generate_message(uninitialized_state_error, Class, Selector) ->
    chars_to_binary(io_lib:format("UninitializedStateError in '~ts' on ~ts", [Selector, Class]));
%% BT-4030: `new:`/`spawnWith:` with an undeclared initialiser key. The
%% `check_init_args/5` call site overrides this with a message naming the
%% key; these clauses keep a bare `new(unknown_field, ...)` readable.
generate_message(unknown_field, Class, undefined) ->
    chars_to_binary(io_lib:format("Unknown field for ~ts", [Class]));
generate_message(unknown_field, Class, Selector) ->
    chars_to_binary(io_lib:format("'~ts': unknown field for ~ts", [Selector, Class]));
generate_message(Kind, Class, undefined) ->
    chars_to_binary(io_lib:format("~ts error in ~ts", [Kind, Class]));
generate_message(Kind, Class, Selector) ->
    chars_to_binary(io_lib:format("~ts error in '~ts' on ~ts", [Kind, Selector, Class])).

%% Enrich DNU hint when the selector is a class-side message.
%%
%% If the instance doesn't understand a selector but the class hierarchy does
%% (e.g., `methods`, `allMethods`, `superclass`), suggest using `class` to
%% access it. Degrades gracefully if the class registry is unavailable.
-spec maybe_enrich_dnu_hint(#beamtalk_error{}) -> #beamtalk_error{}.
maybe_enrich_dnu_hint(#beamtalk_error{class = Class, selector = Selector} = Error) when
    is_atom(Class), is_atom(Selector), Selector =/= undefined
->
    try beamtalk_dispatch:responds_to(Selector, 'Class') of
        true ->
            SelBin = atom_to_binary(Selector, utf8),
            Hint = iolist_to_binary([
                <<"'">>,
                SelBin,
                <<"' is a class-side message; use 'class ">>,
                SelBin,
                <<"' to send it to an instance's class">>
            ]),
            Error#beamtalk_error{
                hint = beamtalk_primitive:string_from_binary(Hint)
            };
        false ->
            Error
    catch
        _:_ -> Error
    end;
maybe_enrich_dnu_hint(Error) ->
    Error.

-doc """
Format an exception class + reason as a compact human-readable binary.

Extracts the message from #beamtalk_error{} records and Exception wrappers
instead of dumping raw Erlang tuples. Used by debug-level logging in
dispatch catch blocks to avoid verbose ~tp output.
""".
-spec format_reason(atom(), term()) -> binary().
format_reason(_ErrClass, #beamtalk_error{kind = Kind, class = Class, message = Msg}) ->
    %% BT-4170: the message field is a tagged `String` (or raw bytes in a
    %% hand-built record) — render it before `~s`-formatting.
    chars_to_binary(io_lib:format("~ts (~ts:~ts)", [render_text(Msg), Class, Kind]));
format_reason(_ErrClass, #{'$beamtalk_class' := _, error := #beamtalk_error{} = Err}) ->
    format_reason(error, Err);
format_reason(ErrClass, Reason) ->
    chars_to_binary(io_lib:format("~p:~tp", [ErrClass, Reason])).

-doc """
Encode formatted text (e.g. `io_lib:format/2` output) as a UTF-8 binary.

`iolist_to_binary/1` raises `badarg` on codepoints above 255, which `~ts` emits
for e.g. a selector like `x_λ:`; this is the one place that encodes formatted
error text. Raises `badarg` rather than returning an `{error, _, _}` tuple, so a
bad input fails at the call site instead of landing in a message field.
""".
-spec chars_to_binary(io_lib:chars()) -> binary().
chars_to_binary(Chars) ->
    case unicode:characters_to_binary(Chars) of
        Bin when is_binary(Bin) -> Bin;
        _ -> erlang:error(badarg, [Chars])
    end.
