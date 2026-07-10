%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_shape_store_fixture).

-moduledoc """
Static `__beamtalk_meta/0` fixture for `beamtalk_workspace_shape_store_tests`
(ADR 0105 Phase 2, BT-2780).

Hand-written to match the exact shape the Rust codegen emits for `fields`/
`field_types` (`meta_field_types_map` in
`crates/beamtalk-core/src/codegen/core_erlang/gen_server/methods.rs`), so a
test registering this module via `beamtalk_class_metadata:insert/4` exercises
`beamtalk_workspace_shape_store:read_shape_from_meta/1` against a realistic
currently-installed shape.
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
