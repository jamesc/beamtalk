%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_protocol_object).

%%% **DDD Context:** Object System Context

-moduledoc """
Shared dispatch module for protocol class objects (ADR 0068).

When a protocol is registered via `beamtalk_protocol_registry:register_protocol/1`,
a class process is created (sealed abstract subclass of Protocol) using this module
for method dispatch. All protocol class objects share this single module.

Protocol class objects respond to class-side messages:
- `requiredMethods` — returns the required method selectors for this protocol
- `conformingClasses` — returns the classes conforming to this protocol

The protocol name is extracted from the ClassSelf tuple's class tag
(e.g., `'Printable class'` → `'Printable'`).
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([
    class_requiredMethods/2,
    class_conformingClasses/2
]).

%%% ============================================================================
%%% Class Method Dispatch
%%% ============================================================================

-doc """
Return the required method selectors for this protocol.

Called when a protocol class object receives `requiredMethods` as a class
method. Extracts the protocol name from ClassSelf and queries the registry.
""".
-spec class_requiredMethods(#beamtalk_object{}, map()) -> [atom()].
class_requiredMethods(ClassSelf, _ClassVars) ->
    ProtocolName = protocol_name_from_class_self(ClassSelf),
    beamtalk_protocol_registry:required_methods(ProtocolName).

-doc """
Return the classes conforming to this protocol.

Called when a protocol class object receives `conformingClasses` as a class
method. Extracts the protocol name from ClassSelf and queries the registry.
""".
-spec class_conformingClasses(#beamtalk_object{}, map()) -> [atom()].
class_conformingClasses(ClassSelf, _ClassVars) ->
    beamtalk_protocol_registry:conforming_classes(
        protocol_name_from_class_self(ClassSelf)
    ).

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

-doc """
Extract the protocol name from a class self object.

Class self objects use the tag format `'ProtocolName class'`
(from `beamtalk_class_registry:class_object_tag/1`). This function
strips the ` class` suffix to recover the protocol name atom.
""".
-spec protocol_name_from_class_self(#beamtalk_object{}) -> atom().
protocol_name_from_class_self(#beamtalk_object{class = ClassTag}) ->
    TagStr = atom_to_list(ClassTag),
    %% Strip " class" suffix (6 characters)
    ProtocolStr = lists:sublist(TagStr, length(TagStr) - 6),
    % elp:fixme W0023 intentional atom creation — protocol names are already registered
    list_to_atom(ProtocolStr).
