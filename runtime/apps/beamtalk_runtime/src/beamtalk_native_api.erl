%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_native_api).

%%% **DDD Context:** Object System Context / package native code boundary

-moduledoc """
Supported API for package native Erlang code to call back into Beamtalk
(BT-4556).

A package's `native/` Erlang modules (ADR 0072) sometimes run inside an
Erlang library and must call a Beamtalk object: an adapter that a Beamtalk
class hands to an Erlang library as a callback, for instance. This module is
the **only supported entry point** for that direction. Every other runtime
module (`beamtalk_message_dispatch`, `beamtalk_class_dispatch`, compiled
`bt@...` modules and their `class_*` functions) is internal and may change
without notice.

Each function delegates to the runtime's own implementation; nothing here
re-implements dispatch or a value representation.

## Sending messages

`send/3` sends `Selector` with `Args` to any Beamtalk object: an instance (a
value, a `String`, an actor) or a class object. It returns the method's
result and raises what the method raises, exactly as a Beamtalk send of the
same message does. `send/4` takes a timeout; see below for what it bounds.

Selectors are atoms spelled as in Beamtalk: `size`, `'at:'`,
`'request:url:options:'`. `Args` has one element per keyword part.

## Where a class-side send runs

A class-side send runs **in the caller** (the native code's own process, with
no class-process hop and no timeout) when ADR 0142's rules hold: neither the
class nor any ancestor declares class variables, nothing shadows the class
method (no class-side extension, no runtime-installed method), and the
selector is not process-bound (`new`, `spawn`, ...). The answer can change
at run time (loading a class-side extension, or redefining the class with
class variables), so a caller that must not time out passes a `send/4`
timeout either way.

Otherwise the class method runs **in the class's process**, and the caller
waits for it. With `send/3` the wait is bounded by the default class-method
call timeout (60 s). Pass a longer bound with `send/4`; running out of it
raises a structured `timeout` error. An actor instance likewise answers in
its own process, and `send/4`'s timeout bounds that wait too.

## Values crossing the boundary

Beamtalk values are plain Erlang terms, with two that native code must not
build by hand:

- A `String` is `{beamtalk_string, Binary}`. Build one with
  `string_from_binary/1` and read one with `string_to_binary/1`. A bare
  binary is a Beamtalk `Binary`, not a `String`. This applies to arguments
  you pass and to Strings nested inside results (a map's values, a list's
  elements). It also applies to a native module named `beamtalk_*`, which a
  Beamtalk caller passes Strings still tagged.
- A `Result` is a tagged map. Convert one to `{ok, Value} | {error, Reason}`
  with `result_to_tuple/1`, and back with `result_from_tuple/1`.

Integers, floats, atoms (Symbols), `true`/`false`, `nil`, lists, maps and
tuples cross unchanged.

See `docs/beamtalk-native-erlang.md` § Calling Beamtalk from native code.
""".

-include("beamtalk.hrl").

-export([
    send/3,
    send/4,
    class_named/1,
    string_from_binary/1,
    string_to_binary/1,
    result_from_tuple/1,
    result_to_tuple/1
]).

%% A Beamtalk object: any value, an actor, or a class object.
-type object() :: term().
%% A Beamtalk `String`: build and read it through this module, never by hand.
-type bt_string() :: {beamtalk_string, binary()}.
-export_type([object/0, bt_string/0]).

%%% ====================================================================
%%% Message sends
%%% ====================================================================

-doc """
Send `Selector` with `Args` to `Receiver` and return the result.

Delegates to `beamtalk_message_dispatch:send/3`, the entry point compiled
Beamtalk code uses for a dynamic send. Errors are raised as Beamtalk raises
them (`error:#{'$beamtalk_class' := _, error := #beamtalk_error{}}`).
""".
-spec send(object(), atom(), list()) -> term().
send(Receiver, Selector, Args) when is_atom(Selector), is_list(Args) ->
    beamtalk_message_dispatch:send(Receiver, Selector, Args).

-doc """
`send/3` with a bound on the wait for another process.

`Timeout` (milliseconds or `infinity`) bounds the wait for an actor's answer,
and for a class method that runs in its class's process. A class method that
runs in the caller and a send to a value have nothing to
wait for, so `Timeout` does not limit them. Running out of `Timeout` raises a
structured `timeout` error. Delegates to `beamtalk_message_dispatch:send/4`.
""".
-spec send(object(), atom(), list(), timeout()) -> term().
send(Receiver, Selector, Args, Timeout) when is_atom(Selector), is_list(Args) ->
    beamtalk_message_dispatch:send(Receiver, Selector, Args, Timeout).

%%% ====================================================================
%%% Class references
%%% ====================================================================

-doc """
The class object of the loaded class named `ClassName`, the value the class
name evaluates to in Beamtalk, or a `class_not_found` error when no class of
that name is loaded.

A class object passed in from Beamtalk is the same value; prefer receiving one
when the Beamtalk side already has it, since it names exactly the class the
caller meant. Delegates to `beamtalk_class_registry:resolve_class_object/1`.
""".
-spec class_named(atom()) -> {ok, object()} | {error, #beamtalk_error{}}.
class_named(ClassName) when is_atom(ClassName) ->
    case beamtalk_class_registry:resolve_class_object(ClassName) of
        undefined ->
            {error,
                beamtalk_error:new(
                    class_not_found,
                    ClassName,
                    undefined,
                    <<"No class of this name is loaded">>
                )};
        ClassObject ->
            {ok, ClassObject}
    end.

%%% ====================================================================
%%% Strings
%%% ====================================================================

-doc """
The Beamtalk `String` holding the UTF-8 text `Bin`. Raises `type_error` when
`Bin` is not valid UTF-8 (a Beamtalk `String` is text; bytes are a `Binary`).
Delegates to `beamtalk_primitive:string_from_binary/1`, the single
constructor for the representation.
""".
-spec string_from_binary(binary()) -> bt_string().
string_from_binary(Bin) when is_binary(Bin) ->
    case beamtalk_primitive:is_utf8(Bin) of
        true ->
            beamtalk_primitive:string_from_binary(Bin);
        false ->
            beamtalk_error:raise_type_error(
                'String', string_from_binary, <<"Expected UTF-8 text">>
            )
    end;
string_from_binary(_) ->
    beamtalk_error:raise_type_error('String', string_from_binary, <<"Expected a binary">>).

-doc """
The UTF-8 bytes of the Beamtalk `String` `Str`. Raises `type_error` for
anything else, including a bare binary (a Beamtalk `Binary`). Delegates to
`beamtalk_primitive:string_to_binary/1`.
""".
-spec string_to_binary(bt_string()) -> binary().
string_to_binary(Str) ->
    beamtalk_primitive:string_to_binary(Str).

%%% ====================================================================
%%% Results
%%% ====================================================================

-doc """
The Beamtalk `Result` for `{ok, Value}` or `{error, Reason}`. An error
`Reason` becomes a Beamtalk Exception object, so the `Result`'s `unwrap`
re-raises it; pass a `#beamtalk_error{}` for a meaningful one. Delegates to
`beamtalk_result:from_tagged_tuple/1`.
""".
-spec result_from_tuple({ok, term()} | {error, term()}) -> beamtalk_result:t().
result_from_tuple(Tuple) ->
    beamtalk_result:from_tagged_tuple(Tuple).

-doc """
`{ok, Value}` for an ok `Result`, `{error, Reason}` for an error one, where
`Reason` is the `Result`'s Exception object; raises `type_error` for anything
else. `result_from_tuple(result_to_tuple(R))` answers `R`. Delegates to
`beamtalk_result:to_tagged_tuple/1`.
""".
-spec result_to_tuple(beamtalk_result:t()) -> {ok, term()} | {error, term()}.
result_to_tuple(Result) ->
    beamtalk_result:to_tagged_tuple(Result).
