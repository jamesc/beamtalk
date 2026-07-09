%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_signature_store_fixture).

-moduledoc """
Static `__beamtalk_meta/0` fixture for `beamtalk_workspace_signature_store_tests`
(ADR 0105 Phase 1, BT-2777).

Hand-written to match the exact shape the Rust codegen emits for
`method_info`/`class_method_info` entries (`meta_method_info_map` in
`crates/beamtalk-core/src/codegen/core_erlang/gen_server/methods.rs`), so a
test registering this module via `beamtalk_class_metadata:insert/4` exercises
`beamtalk_workspace_signature_store:seed_from_meta/3` against a realistic
never-patched original signature — the fallback case a compile-response
capture never touches.
""".

-export(['__beamtalk_meta'/0]).

'__beamtalk_meta'() ->
    #{
        class => 'FixtureClass',
        superclass => 'Object',
        method_info => #{
            greet => #{
                arity => 1,
                param_types => ['String'],
                return_type => 'Integer',
                is_sealed => false,
                visibility => public
            },
            untyped => #{
                arity => 0,
                param_types => [],
                return_type => none,
                is_sealed => false,
                visibility => public
            },
            generic_result => #{
                arity => 0,
                param_types => [],
                return_type => {generic, 'Result', ['Integer', 'String']},
                is_sealed => false,
                visibility => public
            }
        },
        class_method_info => #{
            'new' => #{
                arity => 0,
                param_types => [],
                return_type => 'FixtureClass',
                is_sealed => false,
                visibility => public
            }
        }
    }.
