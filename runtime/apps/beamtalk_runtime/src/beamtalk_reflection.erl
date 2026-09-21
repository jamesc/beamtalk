%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_reflection).

%%% **DDD Context:** Object System Context

-moduledoc """
Reflection services for instance variable access.

This domain service provides uniform access to instance variables
(fields) on Beamtalk objects. It extracts the reflection logic that
was previously inline in `beamtalk_object_ops:dispatch/4`, giving a
single source of truth for field read/write semantics.

Follows Smalltalk-80 conventions:
- `read_field/2` returns `nil` for non-existent fields
- `write_field/3` creates new fields if they don't exist
- `write_field/3` returns the value (not the object)

See also: beamtalk_object
See also: beamtalk_tagged_map
""".

-export([
    field_names/1,
    read_field/2,
    write_field/3,
    has_field/2,
    clear_field/2,
    inspect_string/1,
    source_file_from_module/1
]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc """
Returns the list of user-visible instance variable names.

Filters out internal metadata fields (`$beamtalk_class`, `__methods__`, etc.)
leaving only the fields defined by the class or added at runtime.
""".
-spec field_names(map()) -> [atom()].
field_names(State) when is_map(State) ->
    beamtalk_tagged_map:user_field_keys(State).

-doc """
Reads an instance variable by name.

Returns `nil` for non-existent fields (Smalltalk-80 semantics) — UNLESS
`Name` is declared `late` (ADR 0124 §1/§9/B4) on the receiver's class or an
ancestor, in which case an absent key or a `nil` value both raise
`uninitialized_state_error`, agreeing with the direct `self.slot` read
(`CoreErlangGenerator::generate_late_field_read`, `expressions.rs`) instead
of silently answering `nil`. Keyed on *declared* late, via
`beamtalk_behaviour_intrinsics:classAllFieldKindsByName/1`, not on mere
absence — a typo'd field name is also missing, and must keep today's `nil`
behaviour.
""".
-spec read_field(atom(), map()) -> term().
read_field(Name, State) when is_atom(Name), is_map(State) ->
    case declared_late(Name, State) of
        true ->
            case maps:find(Name, State) of
                {ok, nil} -> raise_uninitialized_state(Name, State);
                {ok, Value} -> Value;
                error -> raise_uninitialized_state(Name, State)
            end;
        false ->
            maps:get(Name, State, nil)
    end.

-doc """
Writes an instance variable, returning `{Value, NewState}`.

Creates the field if it doesn't exist (Smalltalk-80 semantics).
Returns the written value (not the object), matching `fieldAt:put:`
convention.
""".
-spec write_field(atom(), term(), map()) -> {term(), map()}.
write_field(Name, Value, State) when is_atom(Name), is_map(State) ->
    NewState = State#{Name => Value},
    {Value, NewState}.

-doc """
Presence test for an instance variable by name (ADR 0124 §1/§9/B4).

Never raises, unlike `read_field/2` — the reflective counterpart to
`self hasField: #name`. `maps:is_key/2` alone answers correctly for both an
eager field and a declared-`late` one: absence is exactly what `hasField:`
means to test, and there is no separate "declared but not yet assigned" case
to special-case here the way `read_field/2` must.
""".
-spec has_field(atom(), map()) -> boolean().
has_field(Name, State) when is_atom(Name), is_map(State) ->
    maps:is_key(Name, State).

-doc """
Returns a `late` instance variable to unassigned (ADR 0124 §1/§4f/B4).

The reflective counterpart to `self clearField: #name` — `maps:remove/2`,
never `maps:put/3` with a sentinel, per this ADR's "absence, not `nil`, is
the unassigned representation" rule (§2). Works uniformly on any field, not
only a declared-`late` one, exactly as `write_field/3` writes any field —
whether clearing a non-`late` field is a sound thing for a caller to do is a
question for the caller, not this primitive.
""".
-spec clear_field(atom(), map()) -> map().
clear_field(Name, State) when is_atom(Name), is_map(State) ->
    maps:remove(Name, State).

-doc """
Generates a structural string for an object: `ClassName(field1: val1, field2: val2)`.

ADR 0094 (Critical Risk #4): delegates to the canonical structural renderer
(`beamtalk_object_printer`) so every value/object formatter produces
byte-identical output. There are no independent formatters left.

Extracted from beamtalk_object_ops:dispatch/4 so that compiled
Object modules can call it from generated dispatch/4 code.
""".
-spec inspect_string(map()) -> binary().
inspect_string(State) when is_map(State) ->
    beamtalk_object_printer:structural_from_state(State).

-doc """
Read the beamtalk_source file path from a compiled module's attributes.

Uses erlang:get_module_info/2 BIF instead of Mod:module_info/1 because
Beamtalk modules compiled from Core Erlang via compile:forms(_, [from_core])
do not export module_info/0,1. The BIF works regardless of exports.
Returns nil for stdlib/bootstrap/dynamically-created classes.
""".
-spec source_file_from_module(atom()) -> binary() | 'nil'.
source_file_from_module(ModuleName) ->
    try erlang:get_module_info(ModuleName, attributes) of
        Attrs ->
            case lists:keyfind(beamtalk_source, 1, Attrs) of
                {beamtalk_source, [Path]} when is_binary(Path) -> Path;
                {beamtalk_source, [Path]} when is_list(Path) -> list_to_binary(Path);
                _ -> nil
            end
    catch
        error:badarg -> nil
    end.

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

-doc """
True when `Name` is declared `late` (ADR 0124 §1) on `State`'s class or an
ancestor — the class lookup `read_field/2` needs per reflective read (§9,
§4h), keyed on *declared* late rather than mere absence so a typo'd field
name still gets today's plain `nil` behaviour.

Resolves the class from the state map alone via `beamtalk_tagged_map:class_of/1`
(the object itself is not available here — `read_field/2` takes `(Name,
State)` only) and consults B5b's flattened field-kind metadata,
`beamtalk_behaviour_intrinsics:classAllFieldKindsByName/1`. An unresolvable
class (no `'$beamtalk_class'` key) degrades to `false`.
""".
-spec declared_late(atom(), map()) -> boolean().
declared_late(Name, State) ->
    case beamtalk_tagged_map:class_of(State) of
        undefined ->
            false;
        ClassName ->
            FieldKinds = beamtalk_behaviour_intrinsics:classAllFieldKindsByName(ClassName),
            maps:get(Name, FieldKinds, eager) =:= late
    end.

-doc """
Raises `uninitialized_state_error` for an unassigned declared-`late` field
read reflectively, matching the hint text
`CoreErlangGenerator::generate_late_field_read` emits for the direct
`self.slot` read (ADR 0124 §3) — so reflection agrees with the direct read
whichever surface a caller hits first. Uses `'fieldAt:'` as the error's
selector (this IS the `fieldAt:` reflective operation; there is no
enclosing-method selector available here the way the direct-read codegen
has `current_method_selector`).
""".
-spec raise_uninitialized_state(atom(), map()) -> no_return().
raise_uninitialized_state(Name, State) ->
    ClassName = beamtalk_tagged_map:class_of(State, 'Object'),
    FieldTypes = beamtalk_behaviour_intrinsics:classAllFieldTypesByName(ClassName),
    TypeName =
        case maps:get(Name, FieldTypes, none) of
            none -> 'Unknown';
            DeclaredType -> DeclaredType
        end,
    Hint = iolist_to_binary(
        io_lib:format(
            "~s field '~s' (:: ~s) is declared `late` and has not been assigned yet",
            [ClassName, Name, TypeName]
        )
    ),
    Error0 = beamtalk_error:new(uninitialized_state_error, ClassName, 'fieldAt:'),
    Error1 = beamtalk_error:with_hint(Error0, Hint),
    beamtalk_error:raise(Error1).
