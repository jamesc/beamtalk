// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Built-in class definitions for the class hierarchy.
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module defines the built-in classes (`ProtoObject`, `Object`, `Actor`,
//! `Integer`, `String`, etc.) that form the foundation of the Beamtalk type
//! hierarchy. These are registered before any user-defined classes.

use super::{ClassInfo, MethodInfo};
use crate::ast::MethodKind;
use ecow::EcoString;
use std::collections::HashMap;

/// Create a built-in method info (always primary, never sealed).
pub(super) fn builtin_method(selector: &str, arity: usize, defined_in: &str) -> MethodInfo {
    MethodInfo {
        selector: selector.into(),
        arity,
        kind: MethodKind::Primary,
        defined_in: defined_in.into(),
        is_sealed: false,
    }
}

/// Create a sealed built-in method info.
pub(super) fn builtin_sealed_method(selector: &str, arity: usize, defined_in: &str) -> MethodInfo {
    MethodInfo {
        selector: selector.into(),
        arity,
        kind: MethodKind::Primary,
        defined_in: defined_in.into(),
        is_sealed: true,
    }
}

/// Returns true if the given class name is a built-in class.
///
/// This is a fast O(1) check using a static set, suitable for hot paths
/// like `merge()` and `remove_classes()`.
pub(super) fn is_builtin_class(name: &str) -> bool {
    // Keep in sync with builtin_classes() below.
    // Using a match for zero-allocation O(1) lookup.
    matches!(
        name,
        "ProtoObject"
            | "Object"
            | "UndefinedObject"
            | "Boolean"
            | "True"
            | "False"
            | "Number"
            | "Integer"
            | "Float"
            | "String"
            | "Symbol"
            | "Character"
            | "Block"
            | "Actor"
            | "Future"
            | "Collection"
            | "List"
            | "Dictionary"
            | "Set"
            | "Tuple"
            | "Association"
            | "Stream"
            | "TranscriptStream"
            | "File"
            | "SystemDictionary"
            | "WorkspaceEnvironment"
            | "CompiledMethod"
            | "Exception"
            | "Error"
            | "TypeError"
            | "RuntimeError"
            | "InstantiationError"
            | "TestCase"
    )
}

/// Returns all built-in class definitions.
#[allow(clippy::too_many_lines)] // one entry per builtin class with methods list
pub(super) fn builtin_classes() -> HashMap<EcoString, ClassInfo> {
    let mut classes = HashMap::new();

    // ProtoObject — root of the hierarchy
    classes.insert(
        "ProtoObject".into(),
        ClassInfo {
            name: "ProtoObject".into(),
            superclass: None,
            is_sealed: false,
            is_abstract: true,
            state: vec![],
            methods: vec![
                builtin_method("class", 0, "ProtoObject"),
                builtin_method("==", 1, "ProtoObject"),
                builtin_method("/=", 1, "ProtoObject"),
                builtin_method("doesNotUnderstand:args:", 2, "ProtoObject"),
                builtin_method("perform:withArguments:", 2, "ProtoObject"),
                builtin_method("error:", 1, "ProtoObject"),
            ],
            class_methods: vec![],
            class_variables: vec![],
        },
    );

    // Object — default base class
    classes.insert(
        "Object".into(),
        ClassInfo {
            name: "Object".into(),
            superclass: Some("ProtoObject".into()),
            is_sealed: false,
            is_abstract: false,
            state: vec![],
            methods: vec![
                builtin_method("isNil", 0, "Object"),
                builtin_method("notNil", 0, "Object"),
                builtin_method("ifNil:", 1, "Object"),
                builtin_method("ifNotNil:", 1, "Object"),
                builtin_method("ifNil:ifNotNil:", 2, "Object"),
                builtin_method("printString", 0, "Object"),
                builtin_method("inspect", 0, "Object"),
                builtin_method("describe", 0, "Object"),
                builtin_sealed_method("yourself", 0, "Object"),
                builtin_method("hash", 0, "Object"),
                builtin_sealed_method("respondsTo:", 1, "Object"),
                builtin_sealed_method("instVarNames", 0, "Object"),
                builtin_sealed_method("instVarAt:", 1, "Object"),
                builtin_sealed_method("instVarAt:put:", 2, "Object"),
                builtin_sealed_method("perform:", 1, "Object"),
                builtin_sealed_method("perform:withArguments:", 2, "Object"),
                builtin_method("new", 0, "Object"),
                builtin_method("new:", 1, "Object"),
            ],
            class_methods: vec![],
            class_variables: vec![],
        },
    );

    // Actor
    classes.insert(
        "Actor".into(),
        ClassInfo {
            name: "Actor".into(),
            superclass: Some("Object".into()),
            is_sealed: false,
            is_abstract: false,
            state: vec![],
            methods: vec![
                builtin_sealed_method("spawn", 0, "Actor"),
                builtin_sealed_method("spawn:", 1, "Actor"),
                builtin_sealed_method("new", 0, "Actor"),
                builtin_sealed_method("new:", 1, "Actor"),
                builtin_method("describe", 0, "Actor"),
            ],
            class_methods: vec![],
            class_variables: vec![],
        },
    );

    // Number (abstract, not sealed — shared numeric protocol)
    classes.insert(
        "Number".into(),
        ClassInfo {
            name: "Number".into(),
            superclass: Some("Object".into()),
            is_sealed: false,
            is_abstract: true,
            state: vec![],
            methods: vec![
                builtin_method("isZero", 0, "Number"),
                builtin_method("isPositive", 0, "Number"),
                builtin_method("isNegative", 0, "Number"),
                builtin_method("sign", 0, "Number"),
                builtin_method("between:and:", 2, "Number"),
            ],
            class_methods: vec![],
            class_variables: vec![],
        },
    );

    // Integer (sealed)
    classes.insert(
        "Integer".into(),
        ClassInfo {
            name: "Integer".into(),
            superclass: Some("Number".into()),
            is_sealed: true,
            is_abstract: false,
            state: vec![],
            methods: vec![
                builtin_method("+", 1, "Integer"),
                builtin_method("-", 1, "Integer"),
                builtin_method("*", 1, "Integer"),
                builtin_method("/", 1, "Integer"),
                builtin_method("%", 1, "Integer"),
                builtin_method("**", 1, "Integer"),
                builtin_method("=", 1, "Integer"),
                builtin_method("<", 1, "Integer"),
                builtin_method("<=", 1, "Integer"),
                builtin_method(">", 1, "Integer"),
                builtin_method(">=", 1, "Integer"),
                builtin_method("negated", 0, "Integer"),
                builtin_method("abs", 0, "Integer"),
                builtin_method("isEven", 0, "Integer"),
                builtin_method("isOdd", 0, "Integer"),
                builtin_method("min:", 1, "Integer"),
                builtin_method("max:", 1, "Integer"),
                builtin_method("timesRepeat:", 1, "Integer"),
                builtin_method("to:do:", 2, "Integer"),
                builtin_method("to:by:do:", 3, "Integer"),
                builtin_method("asFloat", 0, "Integer"),
                builtin_method("asString", 0, "Integer"),
            ],
            class_methods: vec![],
            class_variables: vec![],
        },
    );

    // Float (sealed)
    classes.insert(
        "Float".into(),
        ClassInfo {
            name: "Float".into(),
            superclass: Some("Number".into()),
            is_sealed: true,
            is_abstract: false,
            state: vec![],
            methods: vec![
                builtin_method("+", 1, "Float"),
                builtin_method("-", 1, "Float"),
                builtin_method("*", 1, "Float"),
                builtin_method("/", 1, "Float"),
                builtin_method("=", 1, "Float"),
                builtin_method("<", 1, "Float"),
                builtin_method("<=", 1, "Float"),
                builtin_method(">", 1, "Float"),
                builtin_method(">=", 1, "Float"),
                builtin_method("negated", 0, "Float"),
                builtin_method("abs", 0, "Float"),
                builtin_method("min:", 1, "Float"),
                builtin_method("max:", 1, "Float"),
                builtin_method("asString", 0, "Float"),
                builtin_method("asInteger", 0, "Float"),
                builtin_method("rounded", 0, "Float"),
                builtin_method("ceiling", 0, "Float"),
                builtin_method("floor", 0, "Float"),
                builtin_method("truncated", 0, "Float"),
                builtin_method("isNaN", 0, "Float"),
                builtin_method("isInfinite", 0, "Float"),
                builtin_method("isZero", 0, "Float"),
            ],
            class_methods: vec![],
            class_variables: vec![],
        },
    );

    // String (sealed)
    classes.insert(
        "String".into(),
        ClassInfo {
            name: "String".into(),
            superclass: Some("Object".into()),
            is_sealed: true,
            is_abstract: false,
            state: vec![],
            methods: vec![
                builtin_method("length", 0, "String"),
                builtin_method("at:", 1, "String"),
                builtin_method("++", 1, "String"),
                builtin_method(",", 1, "String"),
                builtin_method("=", 1, "String"),
                builtin_method("<", 1, "String"),
                builtin_method(">", 1, "String"),
                builtin_method("<=", 1, "String"),
                builtin_method(">=", 1, "String"),
                builtin_method("uppercase", 0, "String"),
                builtin_method("lowercase", 0, "String"),
                builtin_method("trim", 0, "String"),
                builtin_method("includes:", 1, "String"),
                builtin_method("startsWith:", 1, "String"),
                builtin_method("endsWith:", 1, "String"),
                builtin_method("split:", 1, "String"),
                builtin_method("isEmpty", 0, "String"),
                builtin_method("isNotEmpty", 0, "String"),
                builtin_method("reverse", 0, "String"),
                builtin_method("asInteger", 0, "String"),
                builtin_method("asFloat", 0, "String"),
                builtin_method("asList", 0, "String"),
            ],
            class_methods: vec![],
            class_variables: vec![],
        },
    );

    // List (sealed) — Erlang linked list
    classes.insert(
        "List".into(),
        ClassInfo {
            name: "List".into(),
            superclass: Some("Collection".into()),
            is_sealed: true,
            is_abstract: false,
            state: vec![],
            methods: vec![
                builtin_method("size", 0, "List"),
                builtin_method("isEmpty", 0, "List"),
                builtin_method("first", 0, "List"),
                builtin_method("rest", 0, "List"),
                builtin_method("last", 0, "List"),
                builtin_method("at:", 1, "List"),
                builtin_method("includes:", 1, "List"),
                builtin_method("sort", 0, "List"),
                builtin_method("sort:", 1, "List"),
                builtin_method("reversed", 0, "List"),
                builtin_method("unique", 0, "List"),
                builtin_method("detect:", 1, "List"),
                builtin_method("detect:ifNone:", 2, "List"),
                builtin_method("do:", 1, "List"),
                builtin_method("collect:", 1, "List"),
                builtin_method("select:", 1, "List"),
                builtin_method("reject:", 1, "List"),
                builtin_method("inject:into:", 2, "List"),
                builtin_method("take:", 1, "List"),
                builtin_method("drop:", 1, "List"),
                builtin_method("flatten", 0, "List"),
                builtin_method("flatMap:", 1, "List"),
                builtin_method("count:", 1, "List"),
                builtin_method("anySatisfy:", 1, "List"),
                builtin_method("allSatisfy:", 1, "List"),
                builtin_method("zip:", 1, "List"),
                builtin_method("groupBy:", 1, "List"),
                builtin_method("partition:", 1, "List"),
                builtin_method("takeWhile:", 1, "List"),
                builtin_method("dropWhile:", 1, "List"),
                builtin_method("intersperse:", 1, "List"),
                builtin_method("add:", 1, "List"),
                builtin_method("=", 1, "List"),
            ],
            class_methods: vec![],
            class_variables: vec![],
        },
    );

    // Dictionary (sealed)
    classes.insert(
        "Dictionary".into(),
        ClassInfo {
            name: "Dictionary".into(),
            superclass: Some("Collection".into()),
            is_sealed: true,
            is_abstract: false,
            state: vec![],
            methods: vec![
                builtin_method("at:", 1, "Dictionary"),
                builtin_method("at:put:", 2, "Dictionary"),
                builtin_method("size", 0, "Dictionary"),
                builtin_method("keys", 0, "Dictionary"),
                builtin_method("values", 0, "Dictionary"),
                builtin_method("includesKey:", 1, "Dictionary"),
                builtin_method("removeKey:", 1, "Dictionary"),
                builtin_method("do:", 1, "Dictionary"),
                builtin_method("includes:", 1, "Dictionary"),
                builtin_method("collect:", 1, "Dictionary"),
                builtin_method("select:", 1, "Dictionary"),
                builtin_method("inject:into:", 2, "Dictionary"),
                builtin_method("=", 1, "Dictionary"),
                builtin_method("asList", 0, "Dictionary"),
            ],
            class_methods: vec![],
            class_variables: vec![],
        },
    );

    // Set (sealed)
    classes.insert(
        "Set".into(),
        ClassInfo {
            name: "Set".into(),
            superclass: Some("Collection".into()),
            is_sealed: true,
            is_abstract: false,
            state: vec![],
            methods: vec![
                builtin_method("add:", 1, "Set"),
                builtin_method("remove:", 1, "Set"),
                builtin_method("includes:", 1, "Set"),
                builtin_method("size", 0, "Set"),
                builtin_method("do:", 1, "Set"),
                builtin_method("collect:", 1, "Set"),
                builtin_method("select:", 1, "Set"),
                builtin_method("=", 1, "Set"),
                builtin_method("asList", 0, "Set"),
                builtin_method("asArray", 0, "Set"),
            ],
            class_methods: vec![],
            class_variables: vec![],
        },
    );

    // Stream (sealed) — BT-511: lazy closure-based sequences
    classes.insert(
        "Stream".into(),
        ClassInfo {
            name: "Stream".into(),
            superclass: Some("Object".into()),
            is_sealed: true,
            is_abstract: false,
            state: vec![],
            methods: vec![
                builtin_method("select:", 1, "Stream"),
                builtin_method("collect:", 1, "Stream"),
                builtin_method("reject:", 1, "Stream"),
                builtin_method("drop:", 1, "Stream"),
                builtin_method("take:", 1, "Stream"),
                builtin_method("do:", 1, "Stream"),
                builtin_method("inject:into:", 2, "Stream"),
                builtin_method("detect:", 1, "Stream"),
                builtin_method("asList", 0, "Stream"),
                builtin_method("anySatisfy:", 1, "Stream"),
                builtin_method("allSatisfy:", 1, "Stream"),
            ],
            class_methods: vec![],
            class_variables: vec![],
        },
    );

    // Tuple (sealed)
    classes.insert(
        "Tuple".into(),
        ClassInfo {
            name: "Tuple".into(),
            superclass: Some("Collection".into()),
            is_sealed: true,
            is_abstract: false,
            state: vec![],
            methods: vec![
                builtin_method("size", 0, "Tuple"),
                builtin_method("at:", 1, "Tuple"),
                builtin_method("do:", 1, "Tuple"),
                builtin_method("isOk", 0, "Tuple"),
                builtin_method("isError", 0, "Tuple"),
                builtin_method("unwrap", 0, "Tuple"),
                builtin_method("unwrapOr:", 1, "Tuple"),
                builtin_method("unwrapOrElse:", 1, "Tuple"),
                builtin_method("asString", 0, "Tuple"),
            ],
            class_methods: vec![],
            class_variables: vec![],
        },
    );

    // Block (sealed)
    classes.insert(
        "Block".into(),
        ClassInfo {
            name: "Block".into(),
            superclass: Some("Object".into()),
            is_sealed: true,
            is_abstract: false,
            state: vec![],
            methods: vec![
                builtin_method("value", 0, "Block"),
                builtin_method("value:", 1, "Block"),
                builtin_method("value:value:", 2, "Block"),
                builtin_method("value:value:value:", 3, "Block"),
                builtin_method("whileTrue:", 1, "Block"),
                builtin_method("whileFalse:", 1, "Block"),
                builtin_method("repeat", 0, "Block"),
            ],
            class_methods: vec![],
            class_variables: vec![],
        },
    );

    // UndefinedObject (nil, sealed)
    classes.insert(
        "UndefinedObject".into(),
        ClassInfo {
            name: "UndefinedObject".into(),
            superclass: Some("Object".into()),
            is_sealed: true,
            is_abstract: false,
            state: vec![],
            methods: vec![
                builtin_method("isNil", 0, "UndefinedObject"),
                builtin_method("notNil", 0, "UndefinedObject"),
                builtin_method("ifNil:", 1, "UndefinedObject"),
                builtin_method("ifNotNil:", 1, "UndefinedObject"),
                builtin_method("ifNil:ifNotNil:", 2, "UndefinedObject"),
            ],
            class_methods: vec![],
            class_variables: vec![],
        },
    );

    // Boolean (abstract, not sealed — shared boolean protocol)
    classes.insert(
        "Boolean".into(),
        ClassInfo {
            name: "Boolean".into(),
            superclass: Some("Object".into()),
            is_sealed: false,
            is_abstract: true,
            state: vec![],
            methods: vec![
                builtin_method("isBoolean", 0, "Boolean"),
                builtin_method("and:", 1, "Boolean"),
                builtin_method("or:", 1, "Boolean"),
                builtin_method("xor:", 1, "Boolean"),
            ],
            class_methods: vec![],
            class_variables: vec![],
        },
    );

    // True (sealed)
    classes.insert(
        "True".into(),
        ClassInfo {
            name: "True".into(),
            superclass: Some("Boolean".into()),
            is_sealed: true,
            is_abstract: false,
            state: vec![],
            methods: vec![
                builtin_method("ifTrue:ifFalse:", 2, "True"),
                builtin_method("ifTrue:", 1, "True"),
                builtin_method("ifFalse:", 1, "True"),
                builtin_method("not", 0, "True"),
            ],
            class_methods: vec![],
            class_variables: vec![],
        },
    );

    // False (sealed)
    classes.insert(
        "False".into(),
        ClassInfo {
            name: "False".into(),
            superclass: Some("Boolean".into()),
            is_sealed: true,
            is_abstract: false,
            state: vec![],
            methods: vec![
                builtin_method("ifTrue:ifFalse:", 2, "False"),
                builtin_method("ifTrue:", 1, "False"),
                builtin_method("ifFalse:", 1, "False"),
                builtin_method("not", 0, "False"),
            ],
            class_methods: vec![],
            class_variables: vec![],
        },
    );

    // Collection (abstract, not sealed)
    classes.insert(
        "Collection".into(),
        ClassInfo {
            name: "Collection".into(),
            superclass: Some("Object".into()),
            is_sealed: false,
            is_abstract: true,
            state: vec![],
            methods: vec![
                builtin_method("isEmpty", 0, "Collection"),
                builtin_method("isNotEmpty", 0, "Collection"),
                builtin_method("includes:", 1, "Collection"),
                builtin_method("do:", 1, "Collection"),
                builtin_method("collect:", 1, "Collection"),
                builtin_method("select:", 1, "Collection"),
                builtin_method("reject:", 1, "Collection"),
                builtin_method("inject:into:", 2, "Collection"),
                builtin_method("detect:", 1, "Collection"),
                builtin_method("detect:ifNone:", 2, "Collection"),
                builtin_method("anySatisfy:", 1, "Collection"),
                builtin_method("allSatisfy:", 1, "Collection"),
                builtin_method("asString", 0, "Collection"),
            ],
            class_methods: vec![],
            class_variables: vec![],
        },
    );

    // TestCase — BUnit test framework base class (ADR 0014 Phase 2)
    classes.insert(
        "TestCase".into(),
        ClassInfo {
            name: "TestCase".into(),
            superclass: Some("Object".into()),
            is_sealed: false,
            is_abstract: false,
            state: vec![],
            methods: vec![
                builtin_method("setUp", 0, "TestCase"),
                builtin_method("tearDown", 0, "TestCase"),
                builtin_method("assert:", 1, "TestCase"),
                builtin_method("assert:equals:", 2, "TestCase"),
                builtin_method("deny:", 1, "TestCase"),
                builtin_method("should:raise:", 2, "TestCase"),
                builtin_method("fail:", 1, "TestCase"),
            ],
            class_methods: vec![],
            class_variables: vec![],
        },
    );

    classes
}
