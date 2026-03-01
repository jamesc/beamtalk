// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Primitive binding table for stdlib-driven dispatch.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Phase 3 of ADR 0007: The codegen reads primitive bindings from parsed stdlib
//! source files instead of relying solely on hardcoded dispatch tables.
//!
//! # Architecture
//!
//! The `PrimitiveBindingTable` is built at compile time by parsing `lib/*.bt`
//! stdlib files and extracting `@primitive` pragma bindings from method bodies.
//! When generating user code, the codegen consults this table to determine how
//! to dispatch messages to known primitive methods.
//!
//! # Binding Types
//!
//! - **Selector-based** (`@primitive "+"`): The quoted name is a selector that
//!   routes through `beamtalk_X:dispatch('selector', Args, Self)` at runtime.
//! - **Structural intrinsic** (`@primitive timesRepeat`): The unquoted name
//!   identifies a code generation pattern handled by the compiler.

use crate::ast::{Expression, Module};
use std::collections::{HashMap, HashSet};

/// A single primitive binding extracted from a stdlib method.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimitiveBinding {
    /// Selector-based primitive: routes through runtime dispatch.
    ///
    /// Example: `+ other => @primitive "+"` generates
    /// `call 'beamtalk_integer':'dispatch'('+', [Other], Self)`
    SelectorBased {
        /// The selector atom to pass to the runtime dispatch module.
        selector: String,
    },

    /// Structural intrinsic: the compiler generates custom code.
    ///
    /// Example: `timesRepeat: block => @primitive timesRepeat` generates
    /// an inline loop construct.
    StructuralIntrinsic {
        /// The intrinsic name (e.g., `timesRepeat`, `blockValue`, `actorSpawn`).
        name: String,
    },
}

/// Key for looking up a primitive binding: (`class_name`, selector).
///
/// The selector is in Erlang atom format (e.g., `"+"`, `"at:put:"`).
type BindingKey = (String, String);

/// Table of primitive bindings extracted from compiled stdlib.
///
/// Built by parsing `lib/*.bt` files and walking class definitions to find
/// `@primitive` pragmas in method bodies.
#[derive(Debug, Clone, Default)]
pub struct PrimitiveBindingTable {
    /// Map from (`class_name`, selector) to primitive binding.
    bindings: HashMap<BindingKey, PrimitiveBinding>,
}

impl PrimitiveBindingTable {
    /// Creates an empty binding table.
    #[must_use]
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    /// Builds a binding table from a parsed stdlib module.
    ///
    /// Walks all class definitions and their methods, extracting `@primitive`
    /// bindings from method bodies.
    pub fn add_from_module(&mut self, module: &Module) {
        for class in &module.classes {
            let class_name = class.name.name.to_string();

            // Walk both instance methods and class methods for @primitive bindings.
            for method in class.methods.iter().chain(class.class_methods.iter()) {
                // A primitive method has exactly one expression in its body:
                // an Expression::Primitive node.
                if method.body.len() == 1 {
                    if let Expression::Primitive {
                        name, is_quoted, ..
                    } = &method.body[0].expression
                    {
                        let selector = method.selector.to_erlang_atom();
                        let binding = if *is_quoted {
                            PrimitiveBinding::SelectorBased {
                                selector: name.to_string(),
                            }
                        } else {
                            PrimitiveBinding::StructuralIntrinsic {
                                name: name.to_string(),
                            }
                        };

                        self.bindings
                            .insert((class_name.clone(), selector), binding);
                    }
                }
            }
        }
    }

    /// Looks up a primitive binding for a given class and selector.
    ///
    /// Returns `None` if no primitive binding exists (the method is either
    /// pure Beamtalk or not in the stdlib).
    #[must_use]
    pub fn lookup(&self, class_name: &str, selector: &str) -> Option<&PrimitiveBinding> {
        self.bindings
            .get(&(class_name.to_string(), selector.to_string()))
    }

    /// Returns the number of bindings in the table.
    #[must_use]
    pub fn len(&self) -> usize {
        self.bindings.len()
    }

    /// Returns true if the table has no bindings.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.bindings.is_empty()
    }

    /// Finds a selector-based primitive binding for any class.
    ///
    /// Since we don't have static type information at the call site, this searches
    /// all classes for a matching selector. Returns the match only if EXACTLY ONE
    /// class has a selector-based primitive for this selector name.
    ///
    /// Returns `None` if:
    /// - No class has this selector
    /// - Multiple classes have this selector (ambiguous without type info)
    /// - The only match is a structural intrinsic
    #[must_use]
    pub fn find_selector(&self, selector: &str) -> Option<(String, PrimitiveBinding)> {
        let mut result: Option<(String, PrimitiveBinding)> = None;
        for ((class_name, sel), binding) in &self.bindings {
            if sel == selector {
                if let PrimitiveBinding::SelectorBased { .. } = binding {
                    if result.is_some() {
                        // Ambiguous: multiple classes have this selector.
                        // Fall through to generic runtime dispatch.
                        return None;
                    }
                    result = Some((class_name.clone(), binding.clone()));
                }
            }
        }
        result
    }

    /// Returns the set of known runtime module names from all class names in the table.
    ///
    /// Used during codegen (BT-938) to validate that a referenced `bt@stdlib@X`
    /// module will actually be compiled. Returns an empty set when the table is
    /// empty (no stdlib bindings loaded), in which case validation is skipped.
    #[must_use]
    pub fn known_runtime_modules(&self) -> HashSet<String> {
        self.bindings
            .keys()
            .map(|(class_name, _)| Self::runtime_module_for_class(class_name))
            .collect()
    }

    /// Returns the runtime module name for a class's selector-based primitives.
    ///
    /// ADR 0016: All stdlib classes use `bt@stdlib@{snake_case}` prefix.
    /// Uses `to_module_name()` for proper `CamelCase` → `snake_case` conversion,
    /// which handles multi-word names like `TranscriptStream` → `bt@stdlib@transcript_stream`.
    #[must_use]
    pub fn runtime_module_for_class(class_name: &str) -> String {
        format!("bt@stdlib@{}", super::to_module_name(class_name))
    }
}

/// Builds a `PrimitiveBindingTable` from parsed stdlib modules.
///
/// Accepts an iterator of parsed `Module` ASTs (one per `lib/*.bt` file).
#[must_use]
pub fn build_binding_table<'a>(
    modules: impl IntoIterator<Item = &'a Module>,
) -> PrimitiveBindingTable {
    let mut table = PrimitiveBindingTable::new();
    for module in modules {
        table.add_from_module(module);
    }
    table
}

/// Loads primitive bindings by parsing all `.bt` files in a directory.
///
/// This is the primary entry point for building the binding table from stdlib
/// source files. It reads each `.bt` file, lexes and parses it, then extracts
/// `@primitive` bindings from class method definitions.
///
/// Parsing errors are silently ignored — files that fail to parse simply
/// contribute no bindings. This is safe because the binding table is used
/// as an optimization hint, not a correctness requirement.
pub fn load_from_directory(lib_dir: &std::path::Path) -> PrimitiveBindingTable {
    let mut table = PrimitiveBindingTable::new();

    let Ok(entries) = std::fs::read_dir(lib_dir) else {
        return table;
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("bt") {
            continue;
        }

        let Ok(source) = std::fs::read_to_string(&path) else {
            continue;
        };

        let tokens = crate::source_analysis::lex_with_eof(&source);
        let (module, _diagnostics) = crate::source_analysis::parse(tokens);
        table.add_from_module(&module);
    }

    table
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        ClassDefinition, Expression, ExpressionStatement, Identifier, KeywordPart, MessageSelector,
        MethodDefinition, Module, ParameterDefinition,
    };
    use crate::source_analysis::Span;

    fn span() -> Span {
        Span::new(0, 0)
    }

    fn make_primitive_method(
        selector: MessageSelector,
        params: Vec<ParameterDefinition>,
        prim_name: &str,
        is_quoted: bool,
    ) -> MethodDefinition {
        MethodDefinition::new(
            selector,
            params,
            vec![ExpressionStatement::bare(Expression::Primitive {
                name: prim_name.into(),
                is_quoted,
                is_intrinsic: false,
                span: span(),
            })],
            span(),
        )
    }

    fn make_pure_method(selector: MessageSelector) -> MethodDefinition {
        MethodDefinition::new(
            selector,
            vec![],
            vec![ExpressionStatement::bare(Expression::Literal(
                crate::ast::Literal::Integer(42),
                span(),
            ))],
            span(),
        )
    }

    #[test]
    fn test_empty_table() {
        let table = PrimitiveBindingTable::new();
        assert!(table.is_empty());
        assert_eq!(table.len(), 0);
        assert!(table.lookup("Integer", "+").is_none());
    }

    #[test]
    fn test_selector_based_binding() {
        let class = ClassDefinition::new(
            Identifier::new("Integer", span()),
            Identifier::new("Object", span()),
            vec![],
            vec![make_primitive_method(
                MessageSelector::Binary("+".into()),
                vec![ParameterDefinition::new(Identifier::new("other", span()))],
                "+",
                true,
            )],
            span(),
        );

        let module = Module::with_classes(vec![class], span());
        let mut table = PrimitiveBindingTable::new();
        table.add_from_module(&module);

        assert_eq!(table.len(), 1);
        assert_eq!(
            table.lookup("Integer", "+"),
            Some(&PrimitiveBinding::SelectorBased {
                selector: "+".to_string(),
            })
        );
    }

    #[test]
    fn test_structural_intrinsic_binding() {
        let class = ClassDefinition::new(
            Identifier::new("Block", span()),
            Identifier::new("Object", span()),
            vec![],
            vec![make_primitive_method(
                MessageSelector::Unary("value".into()),
                vec![],
                "blockValue",
                false,
            )],
            span(),
        );

        let module = Module::with_classes(vec![class], span());
        let mut table = PrimitiveBindingTable::new();
        table.add_from_module(&module);

        assert_eq!(table.len(), 1);
        assert_eq!(
            table.lookup("Block", "value"),
            Some(&PrimitiveBinding::StructuralIntrinsic {
                name: "blockValue".to_string(),
            })
        );
    }

    #[test]
    fn test_keyword_selector() {
        let class = ClassDefinition::new(
            Identifier::new("Integer", span()),
            Identifier::new("Object", span()),
            vec![],
            vec![make_primitive_method(
                MessageSelector::Keyword(vec![
                    KeywordPart::new("to:", span()),
                    KeywordPart::new("do:", span()),
                ]),
                vec![
                    ParameterDefinition::new(Identifier::new("end", span())),
                    ParameterDefinition::new(Identifier::new("block", span())),
                ],
                "toDo",
                false,
            )],
            span(),
        );

        let module = Module::with_classes(vec![class], span());
        let mut table = PrimitiveBindingTable::new();
        table.add_from_module(&module);

        assert_eq!(
            table.lookup("Integer", "to:do:"),
            Some(&PrimitiveBinding::StructuralIntrinsic {
                name: "toDo".to_string(),
            })
        );
    }

    #[test]
    fn test_pure_beamtalk_methods_not_in_table() {
        let class = ClassDefinition::new(
            Identifier::new("Integer", span()),
            Identifier::new("Object", span()),
            vec![],
            vec![
                make_primitive_method(
                    MessageSelector::Binary("+".into()),
                    vec![ParameterDefinition::new(Identifier::new("other", span()))],
                    "+",
                    true,
                ),
                make_pure_method(MessageSelector::Unary("negated".into())),
            ],
            span(),
        );

        let module = Module::with_classes(vec![class], span());
        let mut table = PrimitiveBindingTable::new();
        table.add_from_module(&module);

        // Only the primitive method should be in the table
        assert_eq!(table.len(), 1);
        assert!(table.lookup("Integer", "+").is_some());
        assert!(table.lookup("Integer", "negated").is_none());
    }

    #[test]
    fn test_multiple_modules() {
        let int_class = ClassDefinition::new(
            Identifier::new("Integer", span()),
            Identifier::new("Object", span()),
            vec![],
            vec![make_primitive_method(
                MessageSelector::Binary("+".into()),
                vec![ParameterDefinition::new(Identifier::new("other", span()))],
                "+",
                true,
            )],
            span(),
        );

        let str_class = ClassDefinition::new(
            Identifier::new("String", span()),
            Identifier::new("Object", span()),
            vec![],
            vec![make_primitive_method(
                MessageSelector::Unary("length".into()),
                vec![],
                "length",
                true,
            )],
            span(),
        );

        let mod1 = Module::with_classes(vec![int_class], span());
        let mod2 = Module::with_classes(vec![str_class], span());
        let table = build_binding_table([&mod1, &mod2]);

        assert_eq!(table.len(), 2);
        assert!(table.lookup("Integer", "+").is_some());
        assert!(table.lookup("String", "length").is_some());
    }

    #[test]
    fn test_runtime_module_for_class() {
        // ADR 0016: All stdlib classes use bt@stdlib@ prefix
        assert_eq!(
            PrimitiveBindingTable::runtime_module_for_class("Integer"),
            "bt@stdlib@integer"
        );
        assert_eq!(
            PrimitiveBindingTable::runtime_module_for_class("String"),
            "bt@stdlib@string"
        );
        assert_eq!(
            PrimitiveBindingTable::runtime_module_for_class("Block"),
            "bt@stdlib@block"
        );
        assert_eq!(
            PrimitiveBindingTable::runtime_module_for_class("True"),
            "bt@stdlib@true"
        );
        assert_eq!(
            PrimitiveBindingTable::runtime_module_for_class("False"),
            "bt@stdlib@false"
        );
        // Multi-word class names use snake_case
        assert_eq!(
            PrimitiveBindingTable::runtime_module_for_class("SequenceableCollection"),
            "bt@stdlib@sequenceable_collection"
        );
        // BT-375: Actor-based singletons
        assert_eq!(
            PrimitiveBindingTable::runtime_module_for_class("TranscriptStream"),
            "bt@stdlib@transcript_stream"
        );
        assert_eq!(
            PrimitiveBindingTable::runtime_module_for_class("BeamtalkInterface"),
            "bt@stdlib@beamtalk_interface"
        );
    }

    #[test]
    fn test_find_selector_returns_selector_based() {
        let class = ClassDefinition::new(
            Identifier::new("Integer", span()),
            Identifier::new("Object", span()),
            vec![],
            vec![
                make_primitive_method(
                    MessageSelector::Unary("asFloat".into()),
                    vec![],
                    "asFloat",
                    true,
                ),
                make_primitive_method(
                    MessageSelector::Unary("value".into()),
                    vec![],
                    "blockValue",
                    false, // structural intrinsic
                ),
            ],
            span(),
        );

        let module = Module::with_classes(vec![class], span());
        let mut table = PrimitiveBindingTable::new();
        table.add_from_module(&module);

        // find_selector should return the selector-based binding
        let result = table.find_selector("asFloat");
        assert!(result.is_some());
        let (class_name, binding) = result.unwrap();
        assert_eq!(class_name, "Integer");
        assert_eq!(
            binding,
            PrimitiveBinding::SelectorBased {
                selector: "asFloat".to_string(),
            }
        );

        // find_selector should NOT return structural intrinsics
        assert!(table.find_selector("value").is_none());

        // Unknown selector
        assert!(table.find_selector("nonexistent").is_none());
    }

    #[test]
    fn test_find_selector_ambiguous_returns_none() {
        // Same selector in multiple classes → ambiguous, should return None
        let int_class = ClassDefinition::new(
            Identifier::new("Integer", span()),
            Identifier::new("Object", span()),
            vec![],
            vec![make_primitive_method(
                MessageSelector::Binary("=:=".into()),
                vec![ParameterDefinition::new(Identifier::new("other", span()))],
                "=:=",
                true,
            )],
            span(),
        );

        let str_class = ClassDefinition::new(
            Identifier::new("String", span()),
            Identifier::new("Object", span()),
            vec![],
            vec![make_primitive_method(
                MessageSelector::Binary("=:=".into()),
                vec![ParameterDefinition::new(Identifier::new("other", span()))],
                "=:=",
                true,
            )],
            span(),
        );

        let mod1 = Module::with_classes(vec![int_class], span());
        let mod2 = Module::with_classes(vec![str_class], span());
        let table = build_binding_table([&mod1, &mod2]);

        // Both Integer and String have '=:=' → ambiguous
        assert!(
            table.find_selector("=:=").is_none(),
            "Ambiguous selector should return None"
        );
    }

    #[test]
    fn test_class_method_primitive_extraction() {
        // BT-444: Verify that @primitive bindings in class_methods are extracted.
        let mut class = ClassDefinition::new(
            Identifier::new("File", span()),
            Identifier::new("Object", span()),
            vec![],
            vec![], // no instance methods
            span(),
        );
        class.class_methods = vec![make_primitive_method(
            MessageSelector::Keyword(vec![KeywordPart::new("exists:", span())]),
            vec![ParameterDefinition::new(Identifier::new("path", span()))],
            "exists:",
            true,
        )];

        let module = Module::with_classes(vec![class], span());
        let mut table = PrimitiveBindingTable::new();
        table.add_from_module(&module);

        assert_eq!(table.len(), 1);
        assert_eq!(
            table.lookup("File", "exists:"),
            Some(&PrimitiveBinding::SelectorBased {
                selector: "exists:".to_string(),
            })
        );
    }

    #[test]
    fn test_load_from_real_stdlib() {
        // This test loads the actual lib/*.bt files from the project root.
        // It verifies that the binding table is populated correctly.
        let lib_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("stdlib/src");

        if !lib_dir.exists() {
            // Skip test if lib/ doesn't exist (e.g., in CI without full repo)
            return;
        }

        let table = load_from_directory(&lib_dir);

        // Verify we loaded bindings from multiple classes
        assert!(
            table.len() > 20,
            "Expected >20 bindings, got {}",
            table.len()
        );

        // Verify specific known bindings exist
        // Integer + is a selector-based primitive
        assert_eq!(
            table.lookup("Integer", "+"),
            Some(&PrimitiveBinding::SelectorBased {
                selector: "+".to_string(),
            })
        );

        // Block value is a structural intrinsic
        assert_eq!(
            table.lookup("Block", "value"),
            Some(&PrimitiveBinding::StructuralIntrinsic {
                name: "blockValue".to_string(),
            })
        );

        // Integer timesRepeat: is a structural intrinsic
        assert_eq!(
            table.lookup("Integer", "timesRepeat:"),
            Some(&PrimitiveBinding::StructuralIntrinsic {
                name: "timesRepeat".to_string(),
            })
        );

        // String length is a selector-based primitive
        assert_eq!(
            table.lookup("String", "length"),
            Some(&PrimitiveBinding::SelectorBased {
                selector: "length".to_string(),
            })
        );

        // Pure Beamtalk methods should NOT be in the table
        assert!(table.lookup("Integer", "negated").is_none());
        assert!(table.lookup("Integer", "abs").is_none());
        assert!(table.lookup("String", "isEmpty").is_none());
    }
}
