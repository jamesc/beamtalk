%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_shape_store_superclass_fixture).

-moduledoc """
Superclass half of the two-level-hierarchy `__beamtalk_meta/0` fixture for
`beamtalk_workspace_shape_store_tests`' flattened-shape tests (ADR 0123
Phase 4, BT-3538) — paired with
`beamtalk_shape_store_subclass_fixture`. A root class (`superclass => none`)
declaring `count`/`taxRate`, no `shapeVersion:`/`migrateFromVN:` (defaults
`1`/`#{}`).
""".

-export(['__beamtalk_meta'/0]).

'__beamtalk_meta'() ->
    #{
        class => 'ShapeFixtureSuperclass',
        superclass => none,
        fields => [count, taxRate],
        field_types => #{
            count => 'Integer',
            taxRate => 'Float'
        }
    }.
