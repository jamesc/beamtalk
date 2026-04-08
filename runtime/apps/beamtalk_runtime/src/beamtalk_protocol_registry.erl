%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_protocol_registry).

%%% **DDD Context:** Object System Context

-moduledoc """
Runtime protocol registry and query engine (ADR 0068 Phase 2c).

Provides an ETS-based registry of protocol definitions and runtime query
functions for protocol introspection. Protocol metadata is registered by
compiled modules during `on_load` via `register_protocol/1`.

## Protocol Registration

Each protocol is stored as a map:
```erlang
#{name => 'Printable',
  required_methods => [#{'selector' => 'asString', 'arity' => 0}],
  type_params => [],
  extending => undefined}
```

## Query API

| Function                    | Description                                    |
|-----------------------------|------------------------------------------------|
| conforms_to/2               | Check if class conforms to protocol             |
| protocols_for_class/1       | List protocols a class conforms to              |
| required_methods/1          | Required method selectors for a protocol        |
| conforming_classes/1        | Classes conforming to a protocol                |
| protocol_info/1             | Full protocol metadata                          |
| is_protocol/1               | Check if a name is a registered protocol        |

## Conformance Model

Runtime conformance uses structural checking — a class conforms if it
responds to all required selectors. This mirrors the compile-time
`ProtocolRegistry::check_conformance` but operates on live runtime data
via `beamtalk_behaviour_intrinsics:classCanUnderstandFromName/2`.

See also: docs/ADR/0068-parametric-types-and-protocols.md — Stage 2
See also: beamtalk_behaviour_intrinsics — backs the class-side primitives
""".

-include_lib("kernel/include/logger.hrl").

-export([
    init/0,
    register_protocol/1,
    conforms_to/2,
    protocols_for_class/1,
    required_methods/1,
    conforming_classes/1,
    protocol_info/1,
    is_protocol/1,
    all_protocol_names/0
]).

-define(PROTOCOL_TABLE, beamtalk_protocol_registry).

%%% ============================================================================
%%% Initialization
%%% ============================================================================

-doc """
Initialize the protocol registry ETS table.

Called during application startup (beamtalk_runtime_app:start/2) before
any compiled modules load their protocol definitions.
""".
-spec init() -> ok.
init() ->
    case ets:info(?PROTOCOL_TABLE) of
        undefined ->
            ets:new(?PROTOCOL_TABLE, [
                named_table,
                set,
                public,
                {read_concurrency, true}
            ]),
            ok;
        _ ->
            %% Table already exists (e.g., re-init after hot reload)
            ok
    end.

%%% ============================================================================
%%% Registration
%%% ============================================================================

-doc """
Register a protocol definition.

Called from compiled module `on_load` callbacks when a module defines
protocols. The `Info` map must contain:
- `name` (atom): Protocol name (e.g., 'Printable')
- `required_methods` (list of maps): Each with `selector` (atom) and `arity` (integer)
- `type_params` (list of atoms): Type parameter names, or `[]`
- `extending` (atom or `undefined`): Parent protocol name

Duplicate registrations overwrite the previous entry (idempotent for hot reload).
""".
-spec register_protocol(map()) -> ok.
register_protocol(#{name := Name} = Info) ->
    ets:insert(?PROTOCOL_TABLE, {Name, Info}),
    ?LOG_DEBUG(
        "Registered protocol ~p",
        [Name],
        #{domain => [beamtalk, runtime]}
    ),
    maybe_create_protocol_class(Name, Info),
    ok;
register_protocol(BadInfo) ->
    ?LOG_WARNING(
        "Invalid protocol registration (missing 'name' key): ~p",
        [BadInfo],
        #{domain => [beamtalk, runtime]}
    ),
    ok.

%%% ============================================================================
%%% Query API
%%% ============================================================================

-doc """
Check if a class conforms to a protocol.

Structural conformance: a class conforms if it responds to all required
selectors of the protocol (including inherited requirements from
`extending` protocols), and all required class methods (BT-1611).

Returns `true` if:
- The class responds to all required instance selectors
- The class responds to all required class method selectors
- The protocol is not registered (unknown protocols — conservative)

Returns `false` if:
- The class is missing one or more required selectors (instance or class)
""".
-spec conforms_to(atom(), atom()) -> boolean().
conforms_to(ClassName, ProtocolName) ->
    case protocol_info(ProtocolName) of
        undefined ->
            %% Unknown protocol — conservative, assume true
            true;
        Info ->
            AllMethods = all_required_methods(Info),
            AllClassMethods = all_required_class_methods(Info),
            try
                InstanceOk = lists:all(
                    fun(#{selector := Selector}) ->
                        beamtalk_behaviour_intrinsics:classCanUnderstandFromName(
                            ClassName, Selector
                        )
                    end,
                    AllMethods
                ),
                ClassOk = lists:all(
                    fun(#{selector := Selector}) ->
                        class_has_class_method(ClassName, Selector)
                    end,
                    AllClassMethods
                ),
                InstanceOk andalso ClassOk
            catch
                Kind:Reason:ST ->
                    %% If the class process is dead or unreachable, assume non-conformance
                    ?LOG_DEBUG(
                        "Protocol conformance check failed for ~p (protocol ~p): ~p:~p",
                        [ClassName, ProtocolName, Kind, Reason],
                        #{stacktrace => ST, domain => [beamtalk, runtime]}
                    ),
                    false
            end
    end.

-doc """
Return the list of protocols a class conforms to.

Checks all registered protocols against the class. Returns a list of
protocol name atoms, sorted alphabetically for deterministic output.
""".
-spec protocols_for_class(atom()) -> [atom()].
protocols_for_class(ClassName) ->
    Names = all_protocol_names(),
    Conforming = [N || N <- Names, conforms_to(ClassName, N)],
    lists:sort(Conforming).

-doc """
Return the required method selectors for a protocol.

Returns a list of selector atoms. Includes methods from extended protocols.
BT-1611: Class method selectors are included with a `class ` prefix atom
(e.g., `'class fromString:'`) to distinguish them from instance methods.
Returns `[]` if the protocol is not registered.
""".
-spec required_methods(atom()) -> [atom()].
required_methods(ProtocolName) ->
    case protocol_info(ProtocolName) of
        undefined ->
            [];
        Info ->
            AllMethods = all_required_methods(Info),
            InstanceSels = [Sel || #{selector := Sel} <- AllMethods],
            AllClassMethods = all_required_class_methods(Info),
            ClassSels = [
                % elp:fixme W0023 intentional atom creation
                list_to_atom("class " ++ atom_to_list(Sel))
             || #{selector := Sel} <- AllClassMethods
            ],
            InstanceSels ++ ClassSels
    end.

-doc """
Return the list of classes conforming to a protocol.

Checks all registered classes against the protocol. Returns a list of
class name atoms, sorted alphabetically for deterministic output.
""".
-spec conforming_classes(atom()) -> [atom()].
conforming_classes(ProtocolName) ->
    case protocol_info(ProtocolName) of
        undefined ->
            [];
        _Info ->
            try
                AllClasses = [
                    Name
                 || {Name, _Mod, _Pid} <-
                        beamtalk_class_registry:live_class_entries()
                ],
                Conforming = [C || C <- AllClasses, conforms_to(C, ProtocolName)],
                lists:sort(Conforming)
            catch
                _:_ -> []
            end
    end.

-doc """
Retrieve full protocol metadata by name.

Returns the protocol info map, or `undefined` if not registered.
""".
-spec protocol_info(atom()) -> map() | undefined.
protocol_info(ProtocolName) ->
    case ets:info(?PROTOCOL_TABLE) of
        undefined ->
            undefined;
        _ ->
            ets:lookup_element(?PROTOCOL_TABLE, ProtocolName, 2, undefined)
    end.

-doc "Check if a name is a registered protocol.".
-spec is_protocol(atom()) -> boolean().
is_protocol(Name) ->
    case ets:info(?PROTOCOL_TABLE) of
        undefined ->
            false;
        _ ->
            case ets:lookup(?PROTOCOL_TABLE, Name) of
                [{_, _}] -> true;
                [] -> false
            end
    end.

-doc "Return all registered protocol names.".
-spec all_protocol_names() -> [atom()].
all_protocol_names() ->
    case ets:info(?PROTOCOL_TABLE) of
        undefined -> [];
        _ -> [Name || {Name, _} <- ets:tab2list(?PROTOCOL_TABLE)]
    end.

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

-doc "Collect all required instance methods including from extending protocols.".
-spec all_required_methods(map()) -> [map()].
all_required_methods(#{required_methods := Methods} = Info) ->
    ParentMethods =
        case maps:get(extending, Info, undefined) of
            undefined ->
                [];
            ParentName ->
                case protocol_info(ParentName) of
                    undefined -> [];
                    ParentInfo -> all_required_methods(ParentInfo)
                end
        end,
    %% Merge: own methods take precedence over parent methods with same selector
    OwnSelectors = [S || #{selector := S} <- Methods],
    FilteredParent = [
        M
     || #{selector := S} = M <- ParentMethods,
        not lists:member(S, OwnSelectors)
    ],
    Methods ++ FilteredParent;
all_required_methods(_) ->
    [].

-doc """
Collect all required class methods including from extending protocols (BT-1611).
""".
-spec all_required_class_methods(map()) -> [map()].
all_required_class_methods(Info) ->
    ClassMethods = maps:get(required_class_methods, Info, []),
    ParentClassMethods =
        case maps:get(extending, Info, undefined) of
            undefined ->
                [];
            ParentName ->
                case protocol_info(ParentName) of
                    undefined -> [];
                    ParentInfo -> all_required_class_methods(ParentInfo)
                end
        end,
    %% Merge: own class methods take precedence over parent with same selector
    OwnSelectors = [S || #{selector := S} <- ClassMethods],
    FilteredParent = [
        M
     || #{selector := S} = M <- ParentClassMethods,
        not lists:member(S, OwnSelectors)
    ],
    ClassMethods ++ FilteredParent.

-doc """
Check if a class has a class-side method (walks hierarchy + extensions) (BT-1611/BT-1617).

Walks the superclass chain checking each class's local class methods map.
Falls back to the extensions ETS table for class-side extensions registered
via `Class class >> selector` (keyed as `{'ClassName class', Selector}`).
This mirrors the actual class-side dispatch path.
""".
-spec class_has_class_method(atom(), atom()) -> boolean().
class_has_class_method(ClassName, Selector) ->
    case class_has_class_method_in_chain(ClassName, Selector) of
        true ->
            true;
        false ->
            %% BT-1617: Check extensions ETS table for class-side extensions.
            %% Class-side extensions use the metaclass tag atom (e.g. 'Integer class')
            %% as the class key in the extensions registry.
            MetaclassTag = beamtalk_class_registry:class_object_tag(ClassName),
            check_class_extension(MetaclassTag, Selector)
    end.

-doc "Walk the superclass chain checking local class methods maps.".
-spec class_has_class_method_in_chain(atom(), atom()) -> boolean().
class_has_class_method_in_chain(ClassName, Selector) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            false;
        ClassPid ->
            try
                ClassMethods = beamtalk_object_class:local_class_methods(ClassPid),
                case lists:member(Selector, ClassMethods) of
                    true ->
                        true;
                    false ->
                        %% Walk superclass chain
                        case beamtalk_object_class:superclass(ClassPid) of
                            none -> false;
                            SuperName -> class_has_class_method_in_chain(SuperName, Selector)
                        end
                end
            catch
                _:_ -> false
            end
    end.

-doc """
Safe extension registry lookup for class-side methods (BT-1617).

Guards against the ETS table not existing (e.g., during early bootstrap).
""".
-spec check_class_extension(atom(), atom()) -> boolean().
check_class_extension(MetaclassTag, Selector) ->
    try
        beamtalk_extensions:has(MetaclassTag, Selector)
    catch
        error:badarg ->
            %% ETS table doesn't exist yet (early bootstrap)
            false
    end.

%%% ============================================================================
%%% Protocol Class Object Creation (ADR 0068)
%%% ============================================================================

-doc """
Create a class object for a protocol if one doesn't already exist.

Protocol class objects are sealed abstract subclasses of Protocol that
respond to class-side messages like `requiredMethods` and `conformingClasses`.
This is called from `register_protocol/1` after storing the protocol metadata.

Skipped during early bootstrap (before the Protocol class exists) and for
hot reload (class already exists).
""".
-spec maybe_create_protocol_class(atom(), map()) -> ok.
maybe_create_protocol_class(Name, Info) ->
    %% Only create if the Protocol class exists (not during early bootstrap)
    %% and this protocol doesn't already have a class object (idempotent).
    case beamtalk_class_registry:whereis_class('Protocol') of
        undefined ->
            %% Protocol class not loaded yet — skip (early bootstrap).
            ok;
        _ProtocolPid ->
            case beamtalk_class_registry:whereis_class(Name) of
                undefined ->
                    create_protocol_class(Name, Info);
                _Pid ->
                    %% Already exists (hot reload) — no-op.
                    ok
            end
    end.

-doc """
Create a sealed abstract class object for a protocol definition.

Uses `beamtalk_protocol_object` as the shared dispatch module for all
protocol class objects. The class methods (`requiredMethods`, `conformingClasses`)
extract the protocol name from the ClassSelf tuple and query the registry.
""".
-spec create_protocol_class(atom(), map()) -> ok.
create_protocol_class(Name, Info) ->
    Doc = maps:get(doc, Info, none),
    ClassInfo = #{
        module => beamtalk_protocol_object,
        superclass => 'Protocol',
        is_sealed => true,
        is_abstract => true,
        meta => #{
            is_sealed => true,
            is_abstract => true
        },
        class_methods => #{
            requiredMethods => #{arity => 0, is_sealed => true},
            conformingClasses => #{arity => 0, is_sealed => true}
        },
        class_method_signatures => #{
            requiredMethods => <<"requiredMethods -> List">>,
            conformingClasses => <<"conformingClasses -> List">>
        },
        class_method_docs => #{
            requiredMethods => <<"Return the required method selectors for this protocol.">>,
            conformingClasses => <<"Return the classes conforming to this protocol.">>
        },
        instance_methods => #{},
        fields => [],
        doc => Doc
    },
    case beamtalk_object_class:start(Name, ClassInfo) of
        {ok, _Pid} ->
            ?LOG_DEBUG(
                "Created class object for protocol ~p",
                [Name],
                #{domain => [beamtalk, runtime]}
            ),
            ok;
        {error, {already_started, _Pid}} ->
            ok;
        {error, Reason} ->
            ?LOG_WARNING(
                "Failed to create class object for protocol ~p: ~p",
                [Name, Reason],
                #{domain => [beamtalk, runtime]}
            ),
            ok
    end.
