%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_collection_selectors).

%%% **DDD Context:** Object System Context

-moduledoc """
The messages whose stdlib provision ends in `self species withAll:` (ADR 0133,
BT-3916, BT-3951).

The compiler's `state_threading_selectors` module holds the list. This module
is its runtime copy, read by
`beamtalk_behaviour_intrinsics:speciesConstructionOverriddenFromName/2`. The
copy is enforced, not trusted: the Rust test
`runtime_species_dependent_list_matches_the_compiler_list` fails when the list
between its markers differs from the compiler's.
""".

-export([
    species_dependent/0
]).

-doc """
The selectors whose `Collection` provision ends in `self species withAll:`.
""".
-spec species_dependent() -> [atom()].
species_dependent() ->
    %% BEGIN SPECIES DEPENDENT
    [
        'collect:',
        'select:',
        'reject:'
    ].
%% END SPECIES DEPENDENT
