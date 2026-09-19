%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_shape_store_subclass_fixture).

-moduledoc """
Subclass half of the two-level-hierarchy `__beamtalk_meta/0` fixture for
`beamtalk_workspace_shape_store_tests`' flattened-shape tests (ADR 0123
Phase 4, BT-3538) — paired with
`beamtalk_shape_store_superclass_fixture`. Declares only its own field
(`name`) — a flattened read must also surface the superclass's `count`/
`taxRate`. Declares `shapeVersion: 2` with a `migrateFromV1:` table entry, so
`read_generation_from_meta/1` exercises the declared-version path (as
opposed to `beamtalk_shape_store_fixture`'s undeclared-defaults path).
""".

-export(['__beamtalk_meta'/0]).

'__beamtalk_meta'() ->
    #{
        class => 'ShapeFixtureSubclass',
        superclass => 'ShapeFixtureSuperclass',
        fields => [name],
        field_types => #{
            name => none
        },
        shape_version => 2,
        shape_migrations => #{1 => 'migrateFromV1:'}
    }.
