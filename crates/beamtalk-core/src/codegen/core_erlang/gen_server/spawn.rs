// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor spawn and instantiation error code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates `spawn/0`, `spawn/1` class methods and `new/0`, `new/1` error
//! methods that prevent incorrect actor instantiation.

use super::super::document::{INDENT, line, nest};
use super::super::{CoreErlangGenerator, Result};
use crate::ast::Module;
use crate::docvec;

impl CoreErlangGenerator {
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
    #[allow(clippy::unnecessary_wraps)]
    pub(in crate::codegen::core_erlang) fn generate_spawn_function(
        &mut self,
        module: &Module,
    ) -> Result<()> {
        // BT-411: Check if class defines an initialize method
        let has_initialize = module
            .classes
            .first()
            .is_some_and(|c| c.methods.iter().any(|m| m.selector.name() == "initialize"));

        let class_name = self.class_name();
        let module_name = self.module_name.clone();

        let ok_body = if has_initialize {
            Self::spawn_initialize_block_doc(&class_name, &module_name)
        } else {
            docvec![format!(
                "{{'beamtalk_object', '{class_name}', '{module_name}', Pid}}"
            )]
        };

        let doc = docvec![
            "'spawn'/0 = fun () ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    format!("case call 'gen_server':'start_link'('{module_name}', ~{{}}~, []) of"),
                    nest(
                        INDENT,
                        docvec![
                            line(),
                            "<{'ok', Pid}> when 'true' ->",
                            nest(INDENT, docvec![line(), ok_body,]),
                            line(),
                            "<{'error', Reason}> when 'true' ->",
                            nest(
                                INDENT,
                                docvec![
                                    line(),
                                    format!(
                                        "let SpawnErr0 = call 'beamtalk_error':'new'('instantiation_error', '{class_name}') in"
                                    ),
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
        self.write_document(&doc);
        Ok(())
    }

    /// Builds the Document for the initialize-with-cleanup block (BT-425).
    ///
    /// Wraps the `initialize` call in a try-catch. If initialize fails,
    /// stops the spawned process before re-raising the error to prevent leaks.
    fn spawn_initialize_block_doc(
        class_name: &str,
        module_name: &str,
    ) -> crate::codegen::core_erlang::document::Document<'static> {
        docvec![
            format!("let _Obj = {{'beamtalk_object', '{class_name}', '{module_name}', Pid}} in"),
            line(),
            "try call 'gen_server':'call'(Pid, {'initialize', []})",
            line(),
            "of _InitOk -> _Obj",
            line(),
            "catch <_InitClass, _InitErr, _InitStack> ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    "let _Stop = call 'gen_server':'stop'(Pid) in",
                    line(),
                    "call 'erlang':'error'(_InitErr)",
                ]
            ),
        ]
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
    ///     case call 'gen_server':'start_link'('counter', InitArgs, []) of
    ///         <{'ok', Pid}> when 'true' ->
    ///             {'beamtalk_object', 'Counter', 'counter', Pid};
    ///         <{'error', Reason}> when 'true' ->
    ///             let SpawnErr0 = call 'beamtalk_error':'new'('instantiation_error', 'Counter') in
    ///             let SpawnErr1 = call 'beamtalk_error':'with_selector'(SpawnErr0, 'spawn') in
    ///             call 'beamtalk_error':'raise'(SpawnErr1)
    ///     end
    /// ```
    #[allow(clippy::unnecessary_wraps)]
    pub(in crate::codegen::core_erlang) fn generate_spawn_with_args_function(
        &mut self,
        module: &Module,
    ) -> Result<()> {
        // BT-411: Check if class defines an initialize method
        let has_initialize = module
            .classes
            .first()
            .is_some_and(|c| c.methods.iter().any(|m| m.selector.name() == "initialize"));

        let class_name = self.class_name();
        let module_name = self.module_name.clone();

        let ok_body = if has_initialize {
            Self::spawn_initialize_block_doc(&class_name, &module_name)
        } else {
            docvec![format!(
                "{{'beamtalk_object', '{class_name}', '{module_name}', Pid}}"
            )]
        };

        let doc = docvec![
            "'spawn'/1 = fun (InitArgs) ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    format!(
                        "case call 'gen_server':'start_link'('{module_name}', InitArgs, []) of"
                    ),
                    nest(
                        INDENT,
                        docvec![
                            line(),
                            "<{'ok', Pid}> when 'true' ->",
                            nest(INDENT, docvec![line(), ok_body,]),
                            line(),
                            "<{'error', Reason}> when 'true' ->",
                            nest(
                                INDENT,
                                docvec![
                                    line(),
                                    format!(
                                        "let SpawnErr0 = call 'beamtalk_error':'new'('instantiation_error', '{class_name}') in"
                                    ),
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
            "\n",
            "\n",
        ];
        self.write_document(&doc);
        Ok(())
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
    #[allow(clippy::unnecessary_wraps)]
    pub(in crate::codegen::core_erlang) fn generate_actor_new_error_method(
        &mut self,
    ) -> Result<()> {
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
                    format!(
                        "let Error2 = call 'beamtalk_error':'with_hint'(Error1, {hint_binary}) in"
                    ),
                    line(),
                    "call 'beamtalk_error':'raise'(Error2)",
                ]
            ),
            "\n",
        ];
        self.write_document(&doc);
        Ok(())
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
    #[allow(clippy::unnecessary_wraps)]
    pub(in crate::codegen::core_erlang) fn generate_actor_new_with_args_error_method(
        &mut self,
    ) -> Result<()> {
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
                    format!(
                        "let Error2 = call 'beamtalk_error':'with_hint'(Error1, {hint_binary}) in"
                    ),
                    line(),
                    "call 'beamtalk_error':'raise'(Error2)",
                ]
            ),
            "\n",
        ];
        self.write_document(&doc);
        Ok(())
    }

    /// Generates the `spawn/0` error method for abstract classes (BT-105).
    ///
    /// Abstract classes cannot be instantiated — they must be subclassed first.
    /// This function generates a method that throws a structured `#beamtalk_error{}`
    /// record with `kind=instantiation_error`.
    #[allow(clippy::unnecessary_wraps)]
    pub(in crate::codegen::core_erlang) fn generate_abstract_spawn_error_method(
        &mut self,
    ) -> Result<()> {
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
                    format!(
                        "let Error0 = call 'beamtalk_error':'new'('instantiation_error', '{class_name}') in"
                    ),
                    line(),
                    "let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'spawn') in",
                    line(),
                    format!(
                        "let Error2 = call 'beamtalk_error':'with_hint'(Error1, {hint_binary}) in"
                    ),
                    line(),
                    "call 'beamtalk_error':'raise'(Error2)",
                ]
            ),
            "\n",
        ];
        self.write_document(&doc);
        Ok(())
    }

    /// Generates the `spawn/1` error method for abstract classes (BT-105).
    #[allow(clippy::unnecessary_wraps)]
    pub(in crate::codegen::core_erlang) fn generate_abstract_spawn_with_args_error_method(
        &mut self,
    ) -> Result<()> {
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
                    format!(
                        "let Error0 = call 'beamtalk_error':'new'('instantiation_error', '{class_name}') in"
                    ),
                    line(),
                    "let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'spawnWith:') in",
                    line(),
                    format!(
                        "let Error2 = call 'beamtalk_error':'with_hint'(Error1, {hint_binary}) in"
                    ),
                    line(),
                    "call 'beamtalk_error':'raise'(Error2)",
                ]
            ),
            "\n",
        ];
        self.write_document(&doc);
        Ok(())
    }

    /// Returns a Core Erlang binary string literal for the given string.
    ///
    /// Produces: `#{#<byte1>(8,1,'integer',['unsigned'|['big']]), ...}#`
    pub(in crate::codegen::core_erlang) fn binary_string_literal(s: &str) -> String {
        use std::fmt::Write;
        let mut result = String::from("#{");
        for (i, byte) in s.bytes().enumerate() {
            if i > 0 {
                result.push(',');
            }
            write!(
                result,
                "#<{}>(8,1,'integer',['unsigned'|['big']])",
                byte
            )
            .unwrap();
        }
        result.push_str("}#");
        result
    }

    /// Helper to generate a binary string literal in Core Erlang format.
    ///
    /// Generates: #{#<char1>(...), #<char2>(...), ...}#
    pub(in crate::codegen::core_erlang) fn generate_binary_string(
        &mut self,
        s: &str,
    ) -> Result<()> {
        use std::fmt::Write;
        write!(self.output, "{}", Self::binary_string_literal(s))?;
        Ok(())
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
    #[allow(clippy::unnecessary_wraps)]
    pub(in crate::codegen::core_erlang) fn generate_superclass_function(
        &mut self,
        module: &Module,
    ) -> Result<()> {
        let superclass_atom = module
            .classes
            .first()
            .and_then(|c| c.superclass.as_ref())
            .map_or("nil", |s| s.name.as_str());

        let doc = docvec![
            format!("'superclass'/0 = fun () -> '{superclass_atom}'"),
            "\n",
            "\n",
        ];
        self.write_document(&doc);
        Ok(())
    }
}
