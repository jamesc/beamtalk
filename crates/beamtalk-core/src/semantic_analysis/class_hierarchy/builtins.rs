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

/// Returns all built-in class definitions.
#[allow(clippy::too_many_lines)]
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
                builtin_method("~=", 1, "ProtoObject"),
                builtin_method("doesNotUnderstand:args:", 2, "ProtoObject"),
                builtin_method("perform:withArguments:", 2, "ProtoObject"),
                builtin_method("error:", 1, "ProtoObject"),
            ],
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
        },
    );

    // List (sealed) — Erlang linked list
    classes.insert(
        "List".into(),
        ClassInfo {
            name: "List".into(),
            superclass: Some("Object".into()),
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
        },
    );

    // Dictionary (sealed)
    classes.insert(
        "Dictionary".into(),
        ClassInfo {
            name: "Dictionary".into(),
            superclass: Some("Object".into()),
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
                builtin_method("each:", 1, "Dictionary"),
                builtin_method("collect:", 1, "Dictionary"),
                builtin_method("select:", 1, "Dictionary"),
                builtin_method("inject:into:", 2, "Dictionary"),
                builtin_method("=", 1, "Dictionary"),
                builtin_method("asList", 0, "Dictionary"),
            ],
        },
    );

    // Set (sealed)
    classes.insert(
        "Set".into(),
        ClassInfo {
            name: "Set".into(),
            superclass: Some("Object".into()),
            is_sealed: true,
            is_abstract: false,
            state: vec![],
            methods: vec![
                builtin_method("add:", 1, "Set"),
                builtin_method("remove:", 1, "Set"),
                builtin_method("includes:", 1, "Set"),
                builtin_method("size", 0, "Set"),
                builtin_method("each:", 1, "Set"),
                builtin_method("collect:", 1, "Set"),
                builtin_method("select:", 1, "Set"),
                builtin_method("=", 1, "Set"),
                builtin_method("asList", 0, "Set"),
                builtin_method("asArray", 0, "Set"),
            ],
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
        },
    );

    // True (sealed)
    classes.insert(
        "True".into(),
        ClassInfo {
            name: "True".into(),
            superclass: Some("Object".into()),
            is_sealed: true,
            is_abstract: false,
            state: vec![],
            methods: vec![
                builtin_method("ifTrue:ifFalse:", 2, "True"),
                builtin_method("ifTrue:", 1, "True"),
                builtin_method("ifFalse:", 1, "True"),
                builtin_method("and:", 1, "True"),
                builtin_method("or:", 1, "True"),
                builtin_method("not", 0, "True"),
            ],
        },
    );

    // False (sealed)
    classes.insert(
        "False".into(),
        ClassInfo {
            name: "False".into(),
            superclass: Some("Object".into()),
            is_sealed: true,
            is_abstract: false,
            state: vec![],
            methods: vec![
                builtin_method("ifTrue:ifFalse:", 2, "False"),
                builtin_method("ifTrue:", 1, "False"),
                builtin_method("ifFalse:", 1, "False"),
                builtin_method("and:", 1, "False"),
                builtin_method("or:", 1, "False"),
                builtin_method("not", 0, "False"),
            ],
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
                builtin_method("size", 0, "Collection"),
                builtin_method("isEmpty", 0, "Collection"),
                builtin_method("isNotEmpty", 0, "Collection"),
                builtin_method("includes:", 1, "Collection"),
                builtin_method("each:", 1, "Collection"),
                builtin_method("collect:", 1, "Collection"),
                builtin_method("select:", 1, "Collection"),
                builtin_method("reject:", 1, "Collection"),
                builtin_method("inject:into:", 2, "Collection"),
            ],
        },
    );

    classes
}
