%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_shape_store_fixture).

-moduledoc """
Static `__beamtalk_meta/0` fixture for `beamtalk_workspace_shape_store_tests`
(ADR 0105 Phase 2).

Hand-written to match the exact shape the Rust codegen emits for `fields`/
`field_types` (`meta_field_types_map` in
`crates/beamtalk-core/src/codegen/core_erlang/gen_server/methods.rs`), so a
test registering this module via `beamtalk_class_metadata:insert/4` exercises
`beamtalk_workspace_shape_store:read_shape_from_meta/1` against a realistic
currently-installed shape. Declares no `shapeVersion:`/`migrateFromVN:` —
`beamtalk_workspace_shape_store:read_generation_from_meta/1` degrades those
to the defaults `1`/`#{}`, exercising the undeclared-version path.

The two-level-hierarchy fixtures for the flattened-shape tests (ADR 0123
Phase 4, BT-3538) live in `beamtalk_shape_store_superclass_fixture` and
`beamtalk_shape_store_subclass_fixture` — each needs its own module (a
`__beamtalk_meta/0` fixture is one static value per module, the same reason
this module and every sibling ADR 0105 fixture module is single-purpose).
""".

-export(['__beamtalk_meta'/0]).

'__beamtalk_meta'() ->
    #{
        class => 'ShapeFixtureClass',
        superclass => 'Actor',
        fields => [count, name],
        field_types => #{
            count => 'Integer',
            name => none
        }
    }.
