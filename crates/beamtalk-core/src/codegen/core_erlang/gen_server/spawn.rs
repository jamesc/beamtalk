// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor spawn and instantiation error code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates `spawn/0`, `spawn/1` class methods and `new/0`, `new/1` error
//! methods that prevent incorrect actor instantiation.

use super::super::document::{Document, INDENT, line, nest};
use super::super::{CoreErlangGenerator, Result};
use crate::ast::Module;
use crate::docvec;

impl CoreErlangGenerator {
    /// Generates a Document for registering an actor instance with the hot reload
    /// tracking registry. Uses try-catch to handle cases where the registry isn't
    /// running (e.g., stdlib tests).
    pub(super) fn instance_registration_doc(class_name: &str) -> Document<'static> {
        docvec![
            docvec![
                "let _InstReg = try call 'beamtalk_object_instances':'register'('",
                Document::String(class_name.to_string()),
                "', Pid)",
            ],
            nest(
                INDENT,
                docvec![
                    line(),
                    "of _RegOk -> _RegOk",
                    line(),
                    "catch <_RegT, _RegE, _RegS> -> 'ok'",
                ]
            ),
            line(),
            "in",
        ]
    }

    /// Generates the `spawn/0` class method for creating actor instances.
    ///
    /// This is a class-level method (not an instance method) that instantiates
    /// a new actor process. The function:
    /// 1. Calls `gen_server:start_link/3` with an empty init args map
    /// 2. Wraps the pid in a `#beamtalk_object{}` record with class metadata
    /// 3. Returns the object record, or throws error on failure
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'spawn'/0 = fun () ->
    ///     case call 'gen_server':'start_link'('counter', ~{}~, []) of
    ///         <{'ok', Pid}> when 'true' ->
    ///             {'beamtalk_object', 'Counter', 'counter', Pid};
    ///         <{'error', Reason}> when 'true' ->
    ///             let SpawnErr0 = call 'beamtalk_error':'new'('instantiation_error', 'Counter') in
    ///             let SpawnErr1 = call 'beamtalk_error':'with_selector'(SpawnErr0, 'spawn') in
    ///             call 'beamtalk_error':'raise'(SpawnErr1)
    ///     end
    /// ```
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::codegen::core_erlang) fn generate_spawn_function(
        &mut self,
        _module: &Module,
    ) -> Result<Document<'static>> {
        // BT-1417: initialize is now called inside init/1, not here.
        // This ensures all spawn paths (direct, supervised, named) run initialize.
        let class_name = self.class_name();
        let module_name = self.module_name.clone();

        let ok_body = docvec![
            "{'beamtalk_object', '",
            Document::String(class_name.clone()),
            "', '",
            Document::String(module_name.clone()),
            "', Pid}",
        ];

        let doc = docvec![
            "'spawn'/0 = fun () ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    // BT-1417: Trap exits so that initialize failures in init/1
                    // don't kill the caller. Restore after start_link returns.
                    "let _OldTrap = call 'erlang':'process_flag'('trap_exit', 'true') in",
                    line(),
                    docvec![
                        "let _SpawnResult = call 'gen_server':'start_link'('",
                        Document::String(module_name.clone()),
                        "', ~{}~, []) in",
                    ],
                    line(),
                    "let _ = call 'erlang':'process_flag'('trap_exit', _OldTrap) in",
                    line(),
                    "case _SpawnResult of",
                    nest(
                        INDENT,
                        docvec![
                            line(),
                            "<{'ok', Pid}> when 'true' ->",
                            nest(
                                INDENT,
                                docvec![
                                    line(),
                                    // BT-572: Register instance for hot reload tracking
                                    Self::instance_registration_doc(&class_name),
                                    line(),
                                    ok_body,
                                ]
                            ),
                            line(),
                            "<{'error', Reason}> when 'true' ->",
                            nest(
                                INDENT,
                                docvec![
                                    line(),
                                    docvec![
                                        "let SpawnErr0 = call 'beamtalk_error':'new'('instantiation_error', '",
                                        Document::String(class_name.clone()),
                                        "') in",
                                    ],
                                    line(),
                                    "let SpawnErr1 = call 'beamtalk_error':'with_selector'(SpawnErr0, 'spawn') in",
                                    line(),
                                    "call 'beamtalk_error':'raise'(SpawnErr1)",
                                ]
                            ),
                        ]
                    ),
                    line(),
                    "end",
                ]
            ),
            "\n",
            "\n",
        ];
        Ok(doc)
    }

    /// Generates the `spawn/1` class method for creating actor instances with init args.
    ///
    /// This is a class-level method that instantiates a new actor process
    /// with initialization arguments passed to `init/1`. The function:
    /// 1. Calls `gen_server:start_link/3` with the provided args
    /// 2. Wraps the pid in a `#beamtalk_object{}` record with class metadata
    /// 3. Returns the object record, or throws error on failure
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'spawn'/1 = fun (InitArgs) ->
    ///     case call 'erlang':'is_map'(InitArgs) of
    ///       <'false'> when 'true' ->
    ///         %% BT-473: type_error for non-map arguments
    ///       <'true'> when 'true' ->
    ///         case call 'gen_server':'start_link'('counter', InitArgs, []) of
    ///             <{'ok', Pid}> when 'true' ->
    ///                 {'beamtalk_object', 'Counter', 'counter', Pid};
    ///             <{'error', Reason}> when 'true' ->
    ///                 %% instantiation_error
    ///         end
    ///     end
    /// ```
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    #[expect(
        clippy::too_many_lines,
        reason = "spawn-with-args codegen handles arg mapping and instance registration"
    )]
    pub(in crate::codegen::core_erlang) fn generate_spawn_with_args_function(
        &mut self,
        _module: &Module,
    ) -> Result<Document<'static>> {
        // BT-1417: initialize is now called inside init/1, not here.
        let class_name = self.class_name();
        let module_name = self.module_name.clone();

        let ok_body = docvec![
            "{'beamtalk_object', '",
            Document::String(class_name.clone()),
            "', '",
            Document::String(module_name.clone()),
            "', Pid}",
        ];

        // BT-473: Validate InitArgs is a map before passing to gen_server
        // BT-476: This is the single source of truth for spawnWith: argument validation.
        // The runtime (beamtalk_object_class.erl handle_call({spawn, Args})) delegates
        // validation to this generated code for both static and dynamic dispatch paths.
        let hint_binary = Self::binary_string_literal("spawnWith: expects a Dictionary argument");
        let doc = docvec![
            "'spawn'/1 = fun (InitArgs) ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    "case call 'erlang':'is_map'(InitArgs) of",
                    nest(
                        INDENT,
                        docvec![
                            line(),
                            "<'false'> when 'true' ->",
                            nest(
                                INDENT,
                                docvec![
                                    line(),
                                    docvec![
                                        "let TypeErr0 = call 'beamtalk_error':'new'('type_error', '",
                                        Document::String(class_name.clone()),
                                        "') in",
                                    ],
                                    line(),
                                    "let TypeErr1 = call 'beamtalk_error':'with_selector'(TypeErr0, 'spawnWith:') in",
                                    line(),
                                    docvec![
                                        "let TypeErr2 = call 'beamtalk_error':'with_hint'(TypeErr1, ",
                                        Document::String(hint_binary.clone()),
                                        ") in",
                                    ],
                                    line(),
                                    "call 'beamtalk_error':'raise'(TypeErr2)",
                                ]
                            ),
                            line(),
                            "<'true'> when 'true' ->",
                            nest(
                                INDENT,
                                docvec![
                                    line(),
                                    // BT-1417: Trap exits so that initialize failures in init/1
                                    // don't kill the caller. Restore after start_link returns.
                                    "let _OldTrap1 = call 'erlang':'process_flag'('trap_exit', 'true') in",
                                    line(),
                                    docvec![
                                        "let _SpawnResult1 = call 'gen_server':'start_link'('",
                                        Document::String(module_name.clone()),
                                        "', InitArgs, []) in",
                                    ],
                                    line(),
                                    "let _ = call 'erlang':'process_flag'('trap_exit', _OldTrap1) in",
                                    line(),
                                    "case _SpawnResult1 of",
                                    nest(
                                        INDENT,
                                        docvec![
                                            line(),
                                            "<{'ok', Pid}> when 'true' ->",
                                            nest(
                                                INDENT,
                                                docvec![
                                                    line(),
                                                    // BT-572: Register instance for hot reload tracking
                                                    Self::instance_registration_doc(&class_name),
                                                    line(),
                                                    ok_body,
                                                ]
                                            ),
                                            line(),
                                            "<{'error', Reason}> when 'true' ->",
                                            nest(
                                                INDENT,
                                                docvec![
                                                    line(),
                                                    docvec![
                                                        "let SpawnErr0 = call 'beamtalk_error':'new'('instantiation_error', '",
                                                        Document::String(class_name.clone()),
                                                        "') in",
                                                    ],
                                                    line(),
                                                    "let SpawnErr1 = call 'beamtalk_error':'with_selector'(SpawnErr0, 'spawnWith:') in",
                                                    line(),
                                                    "call 'beamtalk_error':'raise'(SpawnErr1)",
                                                ]
                                            ),
                                        ]
                                    ),
                                    line(),
                                    "end",
                                ]
                            ),
                        ]
                    ),
                    line(),
                    "end",
                ]
            ),
            "\n",
            "\n",
        ];
        Ok(doc)
    }

    /// Generates the `new/0` error method for actors (BT-217).
    ///
    /// Actors cannot be instantiated with `new` - they must use `spawn`.
    /// This function generates a method that throws a structured `#beamtalk_error{}`
    /// record with `kind=instantiation_error`.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'new'/0 = fun () ->
    ///     let Error0 = call 'beamtalk_error':'new'('instantiation_error', 'Actor') in
    ///     let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'new') in
    ///     let Error2 = call 'beamtalk_error':'with_hint'(Error1, <<"Use spawn instead">>) in
    ///     call 'beamtalk_error':'raise'(Error2)
    /// ```
    #[allow(clippy::unused_self)] // method on impl for API consistency
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::codegen::core_erlang) fn generate_actor_new_error_method(
        &self,
    ) -> Result<Document<'static>> {
        let hint_binary = Self::binary_string_literal("Use spawn instead");
        let doc = docvec![
            "'new'/0 = fun () ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    "let Error0 = call 'beamtalk_error':'new'('instantiation_error', 'Actor') in",
                    line(),
                    "let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'new') in",
                    line(),
                    docvec![
                        "let Error2 = call 'beamtalk_error':'with_hint'(Error1, ",
                        Document::String(hint_binary.clone()),
                        ") in",
                    ],
                    line(),
                    "call 'beamtalk_error':'raise'(Error2)",
                ]
            ),
            "\n",
        ];
        Ok(doc)
    }

    /// Generates the `new/1` error method for actors (BT-217).
    ///
    /// Actors cannot be instantiated with `new:` - they must use `spawnWith:`.
    /// This function generates a method that throws a structured `#beamtalk_error{}`
    /// record with `kind=instantiation_error`.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'new'/1 = fun (_InitArgs) ->
    ///     let Error0 = call 'beamtalk_error':'new'('instantiation_error', 'Actor') in
    ///     let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'new:') in
    ///     let Error2 = call 'beamtalk_error':'with_hint'(Error1, <<"Use spawnWith: instead">>) in
    ///     call 'beamtalk_error':'raise'(Error2)
    /// ```
    #[allow(clippy::unused_self)] // method on impl for API consistency
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::codegen::core_erlang) fn generate_actor_new_with_args_error_method(
        &self,
    ) -> Result<Document<'static>> {
        let hint_binary = Self::binary_string_literal("Use spawnWith: instead");
        let doc = docvec![
            "'new'/1 = fun (_InitArgs) ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    "let Error0 = call 'beamtalk_error':'new'('instantiation_error', 'Actor') in",
                    line(),
                    "let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'new:') in",
                    line(),
                    docvec![
                        "let Error2 = call 'beamtalk_error':'with_hint'(Error1, ",
                        Document::String(hint_binary.clone()),
                        ") in",
                    ],
                    line(),
                    "call 'beamtalk_error':'raise'(Error2)",
                ]
            ),
            "\n",
        ];
        Ok(doc)
    }

    /// Generates the `spawn/0` error method for abstract classes (BT-105).
    ///
    /// Abstract classes cannot be instantiated — they must be subclassed first.
    /// This function generates a method that throws a structured `#beamtalk_error{}`
    /// record with `kind=instantiation_error`.
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::codegen::core_erlang) fn generate_abstract_spawn_error_method(
        &mut self,
    ) -> Result<Document<'static>> {
        let class_name = self.class_name();
        let hint_binary = Self::binary_string_literal(
            "Abstract classes cannot be instantiated. Subclass it first.",
        );
        let doc = docvec![
            "'spawn'/0 = fun () ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    docvec![
                        "let Error0 = call 'beamtalk_error':'new'('instantiation_error', '",
                        Document::String(class_name.clone()),
                        "') in",
                    ],
                    line(),
                    "let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'spawn') in",
                    line(),
                    docvec![
                        "let Error2 = call 'beamtalk_error':'with_hint'(Error1, ",
                        Document::String(hint_binary.clone()),
                        ") in",
                    ],
                    line(),
                    "call 'beamtalk_error':'raise'(Error2)",
                ]
            ),
            "\n",
        ];
        Ok(doc)
    }

    /// Generates the `spawn/1` error method for abstract classes (BT-105).
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::codegen::core_erlang) fn generate_abstract_spawn_with_args_error_method(
        &mut self,
    ) -> Result<Document<'static>> {
        let class_name = self.class_name();
        let hint_binary = Self::binary_string_literal(
            "Abstract classes cannot be instantiated. Subclass it first.",
        );
        let doc = docvec![
            "'spawn'/1 = fun (_InitArgs) ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    docvec![
                        "let Error0 = call 'beamtalk_error':'new'('instantiation_error', '",
                        Document::String(class_name.clone()),
                        "') in",
                    ],
                    line(),
                    "let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'spawnWith:') in",
                    line(),
                    docvec![
                        "let Error2 = call 'beamtalk_error':'with_hint'(Error1, ",
                        Document::String(hint_binary.clone()),
                        ") in",
                    ],
                    line(),
                    "call 'beamtalk_error':'raise'(Error2)",
                ]
            ),
            "\n",
        ];
        Ok(doc)
    }

    /// Returns a Core Erlang binary string literal for the given string.
    ///
    /// Produces: `#{#<byte1>(8,1,'integer',['unsigned'|['big']]), ...}#`
    pub(in crate::codegen::core_erlang) fn binary_string_literal(s: &str) -> String {
        let mut result = String::from("#{");
        result.push_str(&Self::binary_byte_segments(s));
        result.push_str("}#");
        result
    }

    /// Returns Core Erlang binary byte segments for a string, without `#{...}#` wrapping.
    ///
    /// Used by `binary_string_literal` and string interpolation codegen.
    /// Produces: `#<byte1>(8,1,'integer',['unsigned'|['big']]),#<byte2>(...), ...`
    pub(in crate::codegen::core_erlang) fn binary_byte_segments(s: &str) -> String {
        use std::fmt::Write;
        let mut result = String::new();
        for (i, byte) in s.bytes().enumerate() {
            if i > 0 {
                result.push(',');
            }
            write!(result, "#<{byte}>(8,1,'integer',['unsigned'|['big']])").unwrap();
        }
        result
    }

    /// Generates the `superclass/0` class method for reflection.
    ///
    /// Returns the superclass name as an atom, or `'nil'` for root classes.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'superclass'/0 = fun () -> 'Actor'
    /// ```
    #[allow(clippy::unused_self)] // method on impl for API consistency
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::codegen::core_erlang) fn generate_superclass_function(
        &self,
        module: &Module,
    ) -> Result<Document<'static>> {
        let superclass_atom = module
            .classes
            .first()
            .and_then(|c| c.superclass.as_ref())
            .map_or("nil", |s| s.name.as_str());

        let doc = docvec![
            docvec![
                "'superclass'/0 = fun () -> '",
                Document::String(superclass_atom.to_string()),
                "'",
            ],
            "\n",
            "\n",
        ];
        Ok(doc)
    }
}
