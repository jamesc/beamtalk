%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_compiled_method_ops).

%%% **DDD Context:** Object System Context

-moduledoc """
CompiledMethod primitive class implementation.

This module provides method dispatch for CompiledMethod objects, enabling
method introspection in Beamtalk. CompiledMethod objects are maps with
`'$beamtalk_class' => 'CompiledMethod'` returned by the `>>` operator:

```beamtalk
method := Counter >> #increment
method selector         // => #increment
method source           // => "increment => self.value := self.value + 1"
method argumentCount    // => 0
```

## Builtin Methods

| Selector         | Args | Description                        |
|------------------|------|------------------------------------|
| `selector`       | []   | Returns the method's selector atom |
| `source`         | []   | Returns the method's source code   |
| `doc`            | []   | Returns the method's doc string    |
| `argumentCount`  | []   | Returns the method's arity         |
| `origin`         | []   | Protocol this method was flattened from, or nil (ADR 0127) |
| `class`          | []   | Returns `'CompiledMethod'`         |
| `printString`    | []   | Human-readable representation      |
| `asString`       | []   | Same as printString                |
| `respondsTo:`    | [S]  | Check if responds to selector S    |
""".
-export([dispatch/3, has_method/1]).

-doc "Dispatch a message to a CompiledMethod map.".
-spec dispatch(atom(), list(), map()) -> term().
dispatch(Selector, Args, Value) ->
    case builtin_dispatch(Selector, Args, Value) of
        {ok, Result} -> Result;
        not_found -> does_not_understand(Selector, Args, Value)
    end.

-doc "Check if CompiledMethod responds to the given selector.".
-spec has_method(atom()) -> boolean().
has_method(Selector) ->
    is_builtin(Selector).

-doc "Return true if the given selector is a builtin CompiledMethod method.".
-spec is_builtin(atom()) -> boolean().
is_builtin('selector') -> true;
is_builtin('source') -> true;
is_builtin('doc') -> true;
is_builtin('argumentCount') -> true;
is_builtin('origin') -> true;
is_builtin('class') -> true;
is_builtin('printString') -> true;
is_builtin('asString') -> true;
is_builtin('respondsTo:') -> true;
is_builtin(_) -> false.

%%% ============================================================================
%%% Builtin Dispatch
%%% ============================================================================

-doc """
Dispatch a builtin selector on a CompiledMethod map, returning
`{ok, Result}' or `not_found'.
""".
-spec builtin_dispatch(atom(), list(), map()) -> {ok, term()} | not_found.

%% selector => returns the method's selector atom
builtin_dispatch('selector', [], #{'__selector__' := Sel}) ->
    {ok, Sel};
%% source => returns the method's source code as a String
%% (.bt sources are UTF-8 by definition, so the wrap is sound unchecked)
builtin_dispatch('source', [], #{'__source__' := Src}) when is_binary(Src) ->
    {ok, beamtalk_primitive:string_from_binary(Src)};
builtin_dispatch('source', [], #{'__source__' := Src}) ->
    {ok, Src};
%% doc => returns the method's documentation string (or nil)
builtin_dispatch('doc', [], #{'__doc__' := Doc}) when is_binary(Doc) ->
    {ok, beamtalk_primitive:string_from_binary(Doc)};
builtin_dispatch('doc', [], #{'__doc__' := Doc}) ->
    {ok, Doc};
%% argumentCount => returns the method's arity
builtin_dispatch('argumentCount', [], #{'__method_info__' := Info}) ->
    {ok, maps:get(arity, Info, 0)};
%% origin => the protocol this method was flattened from, or nil (ADR 0127 §12)
builtin_dispatch('origin', [], Value) ->
    {ok, origin(Value)};
%% class => returns 'CompiledMethod'
builtin_dispatch('class', [], _Value) ->
    {ok, 'CompiledMethod'};
%% printString => human-readable representation
builtin_dispatch('printString', [], #{
    '__selector__' := Sel
}) ->
    {ok,
        beamtalk_primitive:string_from_binary(
            iolist_to_binary(io_lib:format("CompiledMethod(~s)", [Sel]))
        )};
%% asString => same as printString
builtin_dispatch('asString', [], Value) ->
    builtin_dispatch('printString', [], Value);
%% respondsTo: => check if responds to a selector
builtin_dispatch('respondsTo:', [Sel], _Value) ->
    {ok, is_builtin(Sel)};
builtin_dispatch(_Selector, _Args, _Value) ->
    not_found.

-doc """
The protocol a `CompiledMethod` was flattened from (ADR 0127 §12), or nil for a
method its class wrote itself. Read from `beamtalk_xref` on demand, keyed by
the defining class and side `beamtalk_object_class:compiled_method_map/7`
records: resolving a method for dispatch must not pay for an xref lookup
(BT-4071), so only this reflective read does. The answer reflects the index
at read time: a `CompiledMethod` held across a reload or removal of its class
reports the current provenance (nil once the class is gone).
""".
-spec origin(map()) -> atom() | nil.
origin(#{'__defining_class__' := Class, '__side__' := Side, '__selector__' := Sel}) when
    is_atom(Class)
->
    beamtalk_xref:method_origin(Class, Side =:= class, Sel);
origin(_Value) ->
    nil.

%%% ============================================================================
%%% Error Handling
%%% ============================================================================

-doc "Raise a does_not_understand error for an unknown CompiledMethod selector.".
-spec does_not_understand(atom(), list(), map()) -> no_return().
does_not_understand(Selector, _Args, _Value) ->
    Error1 = beamtalk_error:new(does_not_understand, 'CompiledMethod', Selector),
    Error2 = beamtalk_error:with_hint(
        Error1,
        <<"CompiledMethod supports: selector, source, doc, argumentCount, origin, class, printString, asString, respondsTo:">>
    ),
    beamtalk_error:raise(Error2).
