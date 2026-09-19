%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_shape_store_late_fixture).

-moduledoc """
Static `__beamtalk_meta/0` fixture for `beamtalk_workspace_shape_store_tests`
(ADR 0124 Section 9/B9, BT-3556) — a `field_kinds` map alongside
`field_types`, exercising `normalize_shape/2`'s zip into `shape()`'s
`{DeclaredType, Kind}` values: `proc` is `late`, `label` has no entry at all
(a class/level predating `field_kinds` meta, or one `field_kinds` simply
never mentions), so it must default to `eager` the same way
`classAllFieldKindsByName/1` does.
""".

-export(['__beamtalk_meta'/0]).

'__beamtalk_meta'() ->
    #{
        class => 'ShapeFixtureLateClass',
        superclass => 'Actor',
        fields => [proc, label],
        field_types => #{
            proc => 'String',
            label => 'String'
        },
        field_kinds => #{
            proc => late
        }
    }.
