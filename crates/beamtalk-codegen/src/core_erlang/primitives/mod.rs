// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Primitive implementation mapping for stdlib codegen.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Maps `@primitive "selector"` annotations to direct Erlang BIF calls.
//! When a stdlib `.bt` file declares `+ other => @primitive "+"`, this module
//! provides the actual Core Erlang expression to emit instead of delegating
//! through a hand-written dispatch module.
//!
//! Compiled stdlib modules are self-sufficient: hand-written Erlang
//! dispatch modules can be deleted.

mod actor_types;
mod array;
mod behaviour;
mod binary;
mod block;
mod character;
mod collection;
mod dictionary;
mod error_handling;
mod float;
mod integer;
pub(in crate::core_erlang) mod intrinsic_bodies;
mod list;
mod protocol;
mod reflection;
mod string;
mod value_types;

use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;

pub(crate) use error_handling::{generate_class_signal_bif, lowers_to_caller_safe_raise};

/// `call 'beamtalk_primitive':'string_from_binary'(<inner>)` — wrap
/// BIF-produced bytes as a `String` value (BT-4170: `printString` and
/// friends answer tagged `String`s, while the Erlang renderers they call
/// return raw binaries).
pub(crate) fn wrap_string_result(inner: Document<'static>) -> Document<'static> {
    docvec![
        "call 'beamtalk_primitive':'string_from_binary'(",
        inner,
        ")"
    ]
}

/// Core Erlang expression for `printString` — delegates to the runtime's
/// `beamtalk_primitive:print_string/1` which formats any value for display,
/// then wraps the rendered bytes as a `String` (BT-4170).
pub(crate) const PRINT_STRING: Document<'static> = Document::Str(
    "call 'beamtalk_primitive':'string_from_binary'(call 'beamtalk_primitive':'print_string'(Self))",
);

/// `call 'erlang':'phash2'(<var>)` — the one lowering of `hash`, shared by
/// the `hash` call-site intrinsic (`try_generate_object_identity`), the
/// compiled `Object>>hash` method body (`IntrinsicBody::Hash`, reached via
/// generic dispatch such as `perform: #hash`, BT-4432) and the quoted
/// `@primitive "hash"` bodies, so every route answers the same value.
pub(crate) fn phash2_doc(var: impl Into<String>) -> Document<'static> {
    docvec!["call 'erlang':'phash2'(", leaf::var(var.into()), ")"]
}

/// `call 'beamtalk_reflection':'field_names'(<var>)` — the one lowering of
/// a value's `fieldNames`, shared by the generated value-type dispatch arm
/// and the compiled `Object>>fieldNames` method body
/// (`IntrinsicBody::FieldNames`, BT-4432).
pub(crate) fn field_names_doc(var: impl Into<String>) -> Document<'static> {
    docvec![
        "call 'beamtalk_reflection':'field_names'(",
        leaf::var(var.into()),
        ")"
    ]
}

/// Generates Core Erlang for a selector-based primitive implementation.
///
/// Returns `Some(code)` if a direct implementation was emitted, `None` if
/// the selector has no known BIF mapping (e.g., unimplemented methods).
///
/// When `None` is returned, the caller should fall back to generating a
/// `does_not_understand` error call.
///
/// # Arguments
///
/// * `class_name` - The class context (e.g., "Integer", "String")
/// * `selector` - The primitive selector (e.g., "+", "length")
/// * `params` - The method parameters (excluding Self)
pub fn generate_primitive_bif(
    class_name: &str,
    selector: &str,
    params: &[String],
) -> Option<Document<'static>> {
    let lower = REGISTRY
        .iter()
        .find_map(|(class, lower)| (*class == class_name).then_some(lower))?;
    lower(selector, params)
}

/// Lowering function for one class's `@primitive` selectors: given the selector
/// and the method's parameters, returns the Core Erlang to emit (or `None` if
/// the selector has no inline lowering).
type PrimitiveLowerFn = fn(&str, &[String]) -> Option<Document<'static>>;

/// The single authoritative primitive-lowering registry.
///
/// This is the one place that maps a class to how its primitives lower,
/// replacing the former per-class `match class_name` dispatch. Classes that
/// share a lowering table point at the **same** function: the
/// `Behaviour` / `Class` / `Metaclass` tower all route to
/// [`behaviour::generate_tower_bif`], so a primitive's lowering follows the
/// primitive, not the class that happens to declare it. That closes the gap
/// that let `className` silently fall through to runtime dispatch when it moved
/// up the tower.
pub(in crate::core_erlang) const REGISTRY: &[(&str, PrimitiveLowerFn)] = &[
    ("Array", array::generate_array_bif),
    ("Binary", binary::generate_binary_bif),
    ("Integer", integer::generate_integer_bif),
    ("Float", float::generate_float_bif),
    ("String", string::generate_string_bif),
    ("Block", block::generate_block_bif),
    ("Exception", error_handling::generate_exception_bif),
    ("Symbol", reflection::generate_symbol_bif),
    ("Tuple", value_types::generate_tuple_bif),
    ("List", list::generate_list_bif),
    ("Dictionary", dictionary::generate_dictionary_bif),
    ("ProtoObject", value_types::generate_proto_object_bif),
    ("Object", value_types::generate_object_bif),
    ("Value", value_types::generate_value_bif),
    ("Set", value_types::generate_set_bif),
    ("CompiledMethod", reflection::generate_compiled_method_bif),
    ("Character", character::generate_character_bif),
    ("Collection", collection::generate_collection_bif),
    // Behaviour / Class / Metaclass tower: one shared table.
    ("Behaviour", behaviour::generate_tower_bif),
    ("Class", behaviour::generate_tower_bif),
    ("Metaclass", behaviour::generate_tower_bif),
    ("Protocol", protocol::generate_protocol_bif),
    ("StackFrame", error_handling::generate_stack_frame_bif),
    ("Pid", actor_types::generate_pid_bif),
    ("Port", actor_types::generate_port_bif),
    ("Reference", actor_types::generate_reference_bif),
    ("Future", actor_types::generate_future_bif),
    ("FileHandle", actor_types::generate_file_handle_bif),
];

/// Quoted `@primitive "selector"` / `@intrinsic "selector"`
/// declarations that intentionally route through runtime dispatch instead of an
/// inline BIF, so the fail-loud cross-check (and `generate_primitive`'s hard
/// error in `mod.rs`) must not flag them as unmapped. (Both labels lower
/// identically when quoted — only `is_quoted` drives the body codegen — so the
/// ADR 0101 substrate methods relabelled `@primitive`→`@intrinsic "class"`
/// etc. stay on this list, since their selectors are keyword selectors that
/// cannot be bare-identifier structural intrinsics.)
///
/// Each entry is `(defining class, quoted selector)`. These are
/// call-site-intercepted reflective / dynamic-dispatch operations whose
/// dynamic sends are also answered before the method body is reached (the
/// value-type `dispatch/3` arms for `class` / `perform:withArguments:`, the
/// actor and class-object dispatchers), so the runtime-dispatch fallback is
/// an unreachable placeholder. See `dispatch_codegen.rs`
/// (`beamtalk_object_class:class_send/3`).
///
/// Keep this list minimal: a quoted primitive that maps 1:1 to a runtime
/// function should be added to a per-class BIF table instead, not here. A
/// `ProtoObject` placeholder that a dynamic send *can* reach re-enters
/// `bt@stdlib@proto_object:dispatch/3` with its own selector and loops forever
/// — that is why the equality operators and the timeout / local `perform`
/// variants are lowered in `generate_proto_object_bif` instead (BT-3919).
const RUNTIME_DISPATCHED_PRIMITIVES: &[(&str, &str)] = &[
    ("ProtoObject", "class"),
    ("ProtoObject", "perform:withArguments:"),
    ("Object", "class"),
];

/// Returns `true` if `(class_name, selector)` is an intentionally
/// runtime-dispatched quoted primitive (see `RUNTIME_DISPATCHED_PRIMITIVES`).
#[must_use]
pub(crate) fn is_runtime_dispatched_primitive(class_name: &str, selector: &str) -> bool {
    RUNTIME_DISPATCHED_PRIMITIVES.contains(&(class_name, selector))
}

// Shared helpers used by per-class modules

/// Generates a comparison BIF call for the standard Erlang comparison operators.
///
/// Used by Integer, Float, String, and Character to avoid duplicating the
/// same 7-operator match block (ADR 0002: Erlang operators).
pub(crate) fn generate_comparison_bif(
    selector: &str,
    params: &[String],
) -> Option<Document<'static>> {
    match selector {
        "=:=" => binary_bif("=:=", params),
        "/=" => binary_bif("/=", params),
        "=/=" => binary_bif("=/=", params),
        "<" => binary_bif("<", params),
        ">" => binary_bif(">", params),
        "<=" => binary_bif("=<", params),
        ">=" => binary_bif(">=", params),
        _ => None,
    }
}

/// Returns a binary BIF call: `call 'erlang':'op'(Self, Param0)`
pub(crate) fn binary_bif(erlang_op: &'static str, params: &[String]) -> Option<Document<'static>> {
    let p0 = params.first()?;
    Some(docvec![
        "call 'erlang':'",
        erlang_op,
        "'(Self, ",
        leaf::var(p0.clone()),
        ")",
    ])
}

/// Extracts the parameter at `index` from `params`, returning `default` if the
/// index is out of bounds.
///
/// The conventional default names follow the `_ArgN` pattern (e.g., `_Arg0`,
/// `_Arg1`), though callers may use descriptive names like `_Block` or `_Key`
/// to clarify the role of a generated Core Erlang variable.
pub(crate) fn param<'a>(params: &'a [String], index: usize, default: &'static str) -> &'a str {
    params.get(index).map_or(default, String::as_str)
}

/// Returns a call document with Self as the first argument and one extra parameter:
/// `call 'module':'function'(Self, p0)`
pub(crate) fn call_self_p0(
    module: &'static str,
    function: &'static str,
    p0: &str,
) -> Document<'static> {
    docvec![
        "call '",
        module,
        "':'",
        function,
        "'(Self, ",
        leaf::var(p0),
        ")",
    ]
}

/// Returns a call document with one extra parameter first, Self second:
/// `call 'module':'function'(p0, Self)`
///
/// Used for functional-style Erlang APIs (e.g. `lists:map/2`, `lists:filter/2`)
/// where the function argument precedes the collection.
pub(crate) fn call_p0_self(
    module: &'static str,
    function: &'static str,
    p0: &str,
) -> Document<'static> {
    docvec![
        "call '",
        module,
        "':'",
        function,
        "'(",
        leaf::var(p0),
        ", Self)",
    ]
}

/// Returns power implementation: `call 'math':'pow'(Self, Param0)`
pub(crate) fn power_bif(params: &[String]) -> Option<Document<'static>> {
    let p0 = params.first()?;
    Some(docvec![
        "call 'erlang':'round'(call 'math':'pow'(\
         call 'erlang':'float'(Self), call 'erlang':'float'(",
        leaf::var(p0.clone()),
        ")))",
    ])
}

/// Builds the 4-step `new → with_selector → with_hint → raise` structured-error
/// Document for the given error `kind`.
///
/// Produces the Core Erlang let-chain:
/// ```text
/// let Error0 = call 'beamtalk_error':'new'('<kind>', '<class>') in
/// let Error1 = call 'beamtalk_error':'with_selector'(Error0, '<selector>') in
/// let Error2 = call 'beamtalk_error':'with_hint'(Error1, <hint_binary>) in
/// call 'beamtalk_error':'raise'(Error2)
/// ```
///
/// `kind` must be a kind the runtime knows about — see
/// `beamtalk_exception_handler:kind_to_class/1`, which maps it to the Beamtalk
/// exception class users catch with `on:do:`.
pub(crate) fn build_error_doc(
    kind: impl Into<String>,
    class: impl Into<String>,
    selector: impl Into<String>,
    hint: &str,
) -> Document<'static> {
    docvec![
        "let Error0 = call 'beamtalk_error':'new'(",
        leaf::atom(kind),
        ", ",
        leaf::atom(class),
        ") in ",
        "let Error1 = call 'beamtalk_error':'with_selector'(Error0, ",
        leaf::atom(selector),
        ") in ",
        "let Error2 = call 'beamtalk_error':'with_hint'(Error1, ",
        leaf::binary_lit(hint),
        ") in ",
        "call 'beamtalk_error':'raise'(Error2)",
    ]
}

/// Converts an `Option<Document>` to an `Option<String>` for test assertions.
#[cfg(test)]
pub(crate) fn doc_to_string(doc: Option<Document<'_>>) -> Option<String> {
    doc.map(|d| d.to_pretty_string())
}

/// A primitive generator under test: `(selector, params) -> Option<Document>`.
#[cfg(test)]
type BifGen = fn(&str, &[String]) -> Option<Document<'static>>;

#[cfg(test)]
fn owned_params(params: &[&str]) -> Vec<String> {
    params.iter().map(|p| (*p).to_string()).collect()
}

/// Table driver (BT-4875): each row is `(name, selector, params, expected)`
/// and asserts `gen(selector, params)` renders exactly `expected`. `name` is
/// the former one-assert test's name, so a failing row is as easy to find as
/// a failing test was; the failure message leads with it.
#[cfg(test)]
pub(crate) fn assert_bif_table(generate: BifGen, rows: &[(&str, &str, &[&str], &str)]) {
    for (name, selector, params, expected) in rows {
        let actual = doc_to_string(generate(selector, &owned_params(params)));
        assert_eq!(
            actual.as_deref(),
            Some(*expected),
            "[{name}] {selector} {params:?}"
        );
    }
}

/// Table driver for selectors the generator must decline (`None`):
/// rows are `(name, selector, params)`.
#[cfg(test)]
pub(crate) fn assert_bif_none_table(generate: BifGen, rows: &[(&str, &str, &[&str])]) {
    for (name, selector, params) in rows {
        let actual = doc_to_string(generate(selector, &owned_params(params)));
        assert_eq!(actual, None, "[{name}] {selector} {params:?}");
    }
}

/// Like [`assert_bif_table`] for [`generate_primitive_bif`], which also
/// dispatches on class: rows are `(name, class, selector, params, expected)`.
#[cfg(test)]
pub(crate) fn assert_primitive_bif_table(rows: &[(&str, &str, &str, &[&str], &str)]) {
    for (name, class, selector, params, expected) in rows {
        let actual = doc_to_string(generate_primitive_bif(
            class,
            selector,
            &owned_params(params),
        ));
        assert_eq!(
            actual.as_deref(),
            Some(*expected),
            "[{name}] {class}>>{selector} {params:?}"
        );
    }
}

/// Like [`assert_bif_none_table`] for [`generate_primitive_bif`]:
/// rows are `(name, class, selector, params)`.
#[cfg(test)]
pub(crate) fn assert_primitive_bif_none_table(rows: &[(&str, &str, &str, &[&str])]) {
    for (name, class, selector, params) in rows {
        let actual = doc_to_string(generate_primitive_bif(
            class,
            selector,
            &owned_params(params),
        ));
        assert_eq!(actual, None, "[{name}] {class}>>{selector} {params:?}");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_concat() {
        let result = doc_to_string(generate_primitive_bif(
            "String",
            "++",
            &["Other".to_string()],
        ));
        let output = result.unwrap();
        assert!(output.contains("iolist_to_binary"));
    }

    #[test]
    fn test_proto_object_dnu_raises_error() {
        let result = doc_to_string(generate_primitive_bif(
            "ProtoObject",
            "doesNotUnderstand:args:",
            &["Selector".to_string(), "Args".to_string()],
        ));
        let output = result.expect("ProtoObject DNU should produce code");
        assert!(output.contains("'does_not_understand'"));
        assert!(output.contains("beamtalk_error':'with_selector'"));
        assert!(output.contains("beamtalk_error':'raise'"));
    }

    #[test]
    fn test_symbol_ordering() {
        // BT-3924: Symbol defines `< > <= >=` (atom order = alphabetical).
        for (sel, erl) in [("<", "<"), (">", ">"), ("<=", "=<"), (">=", ">=")] {
            let result = doc_to_string(generate_primitive_bif(
                "Symbol",
                sel,
                &["Other".to_string()],
            ));
            assert_eq!(
                result,
                Some(format!("call 'erlang':'{erl}'(Self, Other)")),
                "Symbol {sel}"
            );
        }
    }

    #[test]
    fn test_call_self_p0_renders_correctly() {
        let doc = call_self_p0("beamtalk_list", "detect", "Block");
        assert_eq!(
            doc.to_pretty_string(),
            "call 'beamtalk_list':'detect'(Self, Block)"
        );
    }

    #[test]
    fn test_call_p0_self_renders_correctly() {
        let doc = call_p0_self("lists", "map", "Block");
        assert_eq!(doc.to_pretty_string(), "call 'lists':'map'(Block, Self)");
    }

    #[test]
    fn test_is_runtime_dispatched_primitive() {
        // Allowlisted call-site-intercepted operations.
        assert!(is_runtime_dispatched_primitive("ProtoObject", "class"));
        assert!(is_runtime_dispatched_primitive(
            "ProtoObject",
            "perform:withArguments:"
        ));
        assert!(is_runtime_dispatched_primitive("Object", "class"));
        // Not allowlisted: mapped primitives and unrelated pairs.
        assert!(!is_runtime_dispatched_primitive("Integer", "+"));
        assert!(!is_runtime_dispatched_primitive(
            "Behaviour",
            "classFieldNames"
        ));
        assert!(!is_runtime_dispatched_primitive(
            "Object",
            "perform:withArguments:"
        ));
        // BT-3919: reachable by a dynamic send, so lowered to a real body.
        for selector in [
            "==",
            "/=",
            "=:=",
            "=/=",
            "perform:withArguments:timeout:",
            "performLocally:withArguments:",
        ] {
            assert!(!is_runtime_dispatched_primitive("ProtoObject", selector));
        }
    }

    /// BT-3919: `ProtoObject`'s equality bodies are the Erlang BIF, not the
    /// runtime-dispatch placeholder that re-entered
    /// `bt@stdlib@proto_object:dispatch/3` with the same selector.
    #[test]
    fn test_proto_object_equality_lowers_to_bif() {
        for (selector, erlang_op) in [("==", "=="), ("/=", "/="), ("=:=", "=:="), ("=/=", "=/=")] {
            let result = doc_to_string(generate_primitive_bif(
                "ProtoObject",
                selector,
                &["Other".to_string()],
            ));
            assert_eq!(
                result,
                Some(format!("call 'erlang':'{erlang_op}'(Self, Other)")),
                "ProtoObject {selector}"
            );
        }
    }

    /// BT-4410: `Object>>isKindOf:` is a direct call to the runtime
    /// intrinsic, not the foreign-FFI proxy (`beamtalk_erlang_proxy`).
    #[test]
    fn test_object_is_kind_of_lowers_to_direct_intrinsic_call() {
        let result = doc_to_string(generate_primitive_bif(
            "Object",
            "isKindOf:",
            &["AClass".to_string()],
        ));
        assert_eq!(
            result,
            Some("call 'beamtalk_behaviour_intrinsics':'objectIsKindOf'(Self, AClass)".to_string())
        );
    }

    #[test]
    fn test_proto_object_perform_variants_do_not_self_dispatch() {
        let timeout = doc_to_string(generate_primitive_bif(
            "ProtoObject",
            "perform:withArguments:timeout:",
            &["Sel".to_string(), "Args".to_string(), "Ms".to_string()],
        ));
        assert_eq!(
            timeout,
            Some("call 'beamtalk_message_dispatch':'send'(Self, Sel, Args, Ms)".to_string())
        );
        let local = doc_to_string(generate_primitive_bif(
            "ProtoObject",
            "performLocally:withArguments:",
            &["Sel".to_string(), "Args".to_string()],
        ));
        assert_eq!(
            local,
            Some("call 'beamtalk_object_class':'local_call'(Self, Sel, Args)".to_string())
        );
    }

    /// Cross-check that every quoted `@primitive "X"` declared in the
    /// stdlib resolves to a registered inline BIF lowering. An unmapped quoted
    /// primitive in a value-type class would silently fall back to runtime
    /// dispatch and raise `does_not_understand` at runtime — as happened when
    /// moving `name` to Behaviour left `className` unmapped. This fast
    /// `cargo test` catches such gaps before the slower stdlib build does.
    ///
    /// Exclusions, to avoid false positives (acceptance criterion 3):
    /// - Unquoted `@primitive` (structural intrinsics) — handled by the compiler
    ///   at the call site, never via `generate_primitive_bif`.
    /// - Actor-backed classes — they legitimately route unmapped quoted
    ///   primitives through their hand-written `beamtalk_X:dispatch/3` module
    ///   (e.g. `Actor`'s `actorPid`, `ReactiveSubprocess`'s `open:args:notify:`).
    #[test]
    fn test_every_quoted_primitive_resolves_to_a_bif() {
        use beamtalk_core::ast::Expression;
        use std::collections::{HashMap, HashSet};

        let lib_dir = crate::core_erlang::tests::repo_root().join("stdlib/src");

        if !lib_dir.exists() {
            // Skip when stdlib sources aren't present (e.g. partial checkouts).
            return;
        }

        // Parse every stdlib module once. Parse diagnostics are ignored — class
        // and method structure (which is all we need) parses fine per-file, the
        // same approach `primitive_bindings::load_from_directory` relies on.
        let mut modules = Vec::new();
        for entry in std::fs::read_dir(&lib_dir)
            .expect("read stdlib/src")
            .flatten()
        {
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) != Some("bt") {
                continue;
            }
            let source = std::fs::read_to_string(&path).expect("read .bt file");
            let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
            let (module, _diagnostics) = beamtalk_core::source_analysis::parse(tokens);
            modules.push(module);
        }

        // Build a class -> superclass map across all modules to classify
        // actor-backed classes (mirrors the spirit of `is_actor_class`).
        let mut superclass: HashMap<String, String> = HashMap::new();
        for module in &modules {
            for class in &module.classes {
                superclass.insert(
                    class.name.name.to_string(),
                    class.superclass_name().to_string(),
                );
            }
        }

        let is_actor_backed = |start: &str| -> bool {
            if start == "Actor" {
                return true;
            }
            let mut current = start.to_string();
            // Bounded walk guards against cycles in malformed hierarchies.
            for _ in 0..64 {
                match superclass.get(&current) {
                    Some(parent) if parent == "Actor" => return true,
                    Some(parent) if parent != "none" => current = parent.clone(),
                    _ => return false,
                }
            }
            false
        };

        let mut unmapped: Vec<String> = Vec::new();
        // Every quoted (class, primitive) pair seen, for the allowlist-rot check.
        let mut all_quoted: HashSet<(String, String)> = HashSet::new();
        for module in &modules {
            for class in &module.classes {
                let class_name = class.name.name.as_str();
                // Mirror `is_actor_class`: actor classes (by chain) and concrete
                // supervisor subclasses compile as actors, so their unmapped
                // quoted primitives route through hand-written dispatch.
                let is_concrete_supervisor = class.supervisor_kind.is_some() && !class.is_abstract;
                let actor_backed = is_concrete_supervisor || is_actor_backed(class_name);
                for method in class.methods.iter().chain(class.class_methods.iter()) {
                    if method.body.len() != 1 {
                        continue;
                    }
                    let Expression::Primitive {
                        name, is_quoted, ..
                    } = &method.body[0].expression
                    else {
                        continue;
                    };
                    if !is_quoted {
                        continue;
                    }
                    all_quoted.insert((class_name.to_string(), name.to_string()));
                    if actor_backed || is_runtime_dispatched_primitive(class_name, name.as_str()) {
                        continue;
                    }
                    // Mirror the real codegen path: `current_method_params` holds
                    // one entry per method parameter (names are irrelevant here).
                    let params: Vec<String> = (0..method.parameters.len())
                        .map(|i| format!("Arg{i}"))
                        .collect();
                    if generate_primitive_bif(class_name, name.as_str(), &params).is_none() {
                        unmapped.push(format!(
                            "{class_name}:{name} (arity {})",
                            method.parameters.len()
                        ));
                    }
                }
            }
        }
        unmapped.sort();

        assert!(
            unmapped.is_empty(),
            "Quoted @primitive selectors with no inline BIF lowering in \
             generate_primitive_bif. These would silently fall back to runtime \
             dispatch and raise does_not_understand at runtime (BT-2232/BT-2233). \
             Add a mapping in the relevant primitives/*.rs table:\n  {}",
            unmapped.join("\n  ")
        );

        // Keep the allowlist honest — a stale entry (for a quoted
        // primitive that no longer exists in the stdlib) would silently mask a
        // future unmapped-primitive bug. Every allowlist entry must still
        // correspond to a real quoted @primitive declaration.
        let stale: Vec<String> = RUNTIME_DISPATCHED_PRIMITIVES
            .iter()
            .filter(|(class, selector)| {
                !all_quoted.contains(&((*class).to_string(), (*selector).to_string()))
            })
            .map(|(class, selector)| format!("{class}:{selector}"))
            .collect();
        assert!(
            stale.is_empty(),
            "RUNTIME_DISPATCHED_PRIMITIVES has stale entries with no matching \
             quoted @primitive in the stdlib (remove them):\n  {}",
            stale.join("\n  ")
        );
    }

    /// Guard (BT-4055): the primitive-lowering `REGISTRY` holds only native
    /// value types and compiler substrate, never a stateless service facade.
    ///
    /// A class-side-only facade over one Erlang module (`Telemetry`, `Digest`,
    /// `File`, ...) belongs on `native: module` with `=> self delegate`
    /// bodies (ADR 0101), which lower through the shared
    /// `beamtalk_erlang_proxy:native_call/4` path. A per-class name/arity table
    /// here duplicates the Erlang exports with nothing enforcing the match, and
    /// grows the primitive surface ADR 0034/0037 keep minimal. If such a call
    /// is too slow, fix `native_call` (or its lowering) once for every
    /// `native:` class.
    ///
    /// The test tells the two kinds apart by where their quoted `@primitive`s
    /// live: a value type or substrate class lowers instance-side primitives
    /// (`Integer>>+`, `Behaviour>>name`, `Pid>>isAlive`), while a facade
    /// declares quoted `@primitive`s on the class side only.
    ///
    /// The one class-side-only exception is `Protocol`, a reflection facade
    /// over the runtime protocol registry that ADR 0101 Part 3 keeps on
    /// `@primitive` deliberately (runtime-extensible reflection). Anything else
    /// joining that list needs the same kind of ADR-level justification.
    #[test]
    fn test_registry_holds_no_service_facade() {
        use beamtalk_core::ast::Expression;
        use std::collections::HashMap;

        /// Class-side-only registry classes ADR 0101 Part 3 keeps on `@primitive`.
        const REFLECTION_FACADES: &[&str] = &["Protocol"];

        let lib_dir = crate::core_erlang::tests::repo_root().join("stdlib/src");
        if !lib_dir.exists() {
            return;
        }

        // class name -> is a facade: it declares quoted primitives, all class-side
        let mut classes: HashMap<String, bool> = HashMap::new();
        let has_quoted_primitive = |methods: &[beamtalk_core::ast::MethodDefinition]| {
            methods.iter().any(|m| {
                m.body.iter().any(|stmt| {
                    matches!(
                        stmt.expression,
                        Expression::Primitive {
                            is_quoted: true,
                            ..
                        }
                    )
                })
            })
        };
        for entry in std::fs::read_dir(&lib_dir)
            .expect("read stdlib/src")
            .flatten()
        {
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) != Some("bt") {
                continue;
            }
            let source = std::fs::read_to_string(&path).expect("read .bt file");
            let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
            let (module, _diagnostics) = beamtalk_core::source_analysis::parse(tokens);
            for class in &module.classes {
                let facade = has_quoted_primitive(&class.class_methods)
                    && !has_quoted_primitive(&class.methods);
                classes.insert(class.name.name.to_string(), facade);
            }
        }
        assert!(
            classes.contains_key("Integer") && classes.contains_key("Protocol"),
            "stdlib/src did not parse: the facade guard would check nothing"
        );

        let offenders: Vec<&str> = REGISTRY
            .iter()
            .map(|(class, _)| *class)
            .filter(|class| !REFLECTION_FACADES.contains(class))
            .filter(|class| classes.get(*class) == Some(&true))
            .collect();
        assert!(
            offenders.is_empty(),
            "primitives::REGISTRY must hold only value types and substrate. \
             A stateless service facade uses `native: module` with \
             `=> self delegate` instead (ADR 0101, BT-4055). These registered \
             classes have only class-side quoted @primitives:\n  {}",
            offenders.join("\n  ")
        );
        // Keep the exception list honest: each entry must still be registered
        // and still be class-side only, or it is masking nothing.
        for facade in REFLECTION_FACADES {
            assert!(
                REGISTRY.iter().any(|(class, _)| class == facade)
                    && classes.get(*facade) == Some(&true),
                "REFLECTION_FACADES entry `{facade}` is stale: remove it"
            );
        }
    }

    #[test]
    #[allow(clippy::too_many_lines)] // one contiguous table; splitting hurts readability
    fn generate_primitive_bif_table() {
        assert_primitive_bif_table(&[
            (
                "test_integer_plus",
                "Integer",
                "+",
                &["Other"],
                "call 'erlang':'+'(Self, Other)",
            ),
            (
                "test_integer_modulo",
                "Integer",
                "%",
                &["Other"],
                "call 'erlang':'rem'(Self, Other)",
            ),
            (
                "test_integer_as_string",
                "Integer",
                "asString",
                &[],
                "call 'beamtalk_primitive':'string_from_binary'(call 'erlang':'integer_to_binary'(Self))",
            ),
            (
                "test_string_length",
                "String",
                "length",
                &[],
                "call 'string':'length'(call 'beamtalk_primitive':'string_to_binary'(Self))",
            ),
            (
                "test_float_as_string",
                "Float",
                "asString",
                &[],
                "call 'beamtalk_primitive':'string_from_binary'(call 'erlang':'float_to_binary'(Self, ['short']))",
            ),
            (
                "test_float_rounded",
                "Float",
                "rounded",
                &[],
                "call 'erlang':'round'(Self)",
            ),
            (
                "test_float_ceiling",
                "Float",
                "ceiling",
                &[],
                "call 'erlang':'ceil'(Self)",
            ),
            (
                "test_float_floor",
                "Float",
                "floor",
                &[],
                "call 'erlang':'floor'(Self)",
            ),
            (
                "test_float_truncated",
                "Float",
                "truncated",
                &[],
                "call 'erlang':'trunc'(Self)",
            ),
            (
                "test_float_as_integer",
                "Float",
                "asInteger",
                &[],
                "call 'erlang':'trunc'(Self)",
            ),
            (
                "test_symbol_as_string",
                "Symbol",
                "asString",
                &[],
                "call 'beamtalk_primitive':'string_from_binary'(call 'erlang':'atom_to_binary'(Self, 'utf8'))",
            ),
            ("test_symbol_as_atom", "Symbol", "asAtom", &[], "Self"),
            (
                "test_symbol_equality",
                "Symbol",
                "=:=",
                &["Other"],
                "call 'erlang':'=:='(Self, Other)",
            ),
            (
                "test_symbol_hash",
                "Symbol",
                "hash",
                &[],
                "call 'erlang':'phash2'(Self)",
            ),
            (
                "test_symbol_print_string",
                "Symbol",
                "printString",
                &[],
                "call 'beamtalk_primitive':'string_from_binary'(call 'beamtalk_primitive':'print_string'(Self))",
            ),
            (
                "test_list_concat",
                "List",
                "++",
                &["Other"],
                "call 'erlang':'++'(Self, Other)",
            ),
            (
                "test_list_from_to",
                "List",
                "from:to:",
                &["Start", "End"],
                "call 'beamtalk_list':'from_to'(Self, Start, End)",
            ),
            // Character primitive tests
            (
                "test_character_as_integer",
                "Character",
                "asInteger",
                &[],
                "Self",
            ),
            (
                "test_character_as_string",
                "Character",
                "asString",
                &[],
                "call 'beamtalk_primitive':'string_from_binary'(call 'beamtalk_character':'as_string'(Self))",
            ),
            (
                "test_character_print_string",
                "Character",
                "printString",
                &[],
                "call 'beamtalk_primitive':'string_from_binary'(call 'beamtalk_character':'print_string'(Self))",
            ),
            (
                "test_character_equality",
                "Character",
                "=:=",
                &["Other"],
                "call 'erlang':'=:='(Self, Other)",
            ),
            (
                "test_character_is_letter",
                "Character",
                "isLetter",
                &[],
                "call 'beamtalk_character':'is_letter'(Self)",
            ),
            (
                "test_character_is_digit",
                "Character",
                "isDigit",
                &[],
                "call 'beamtalk_character':'is_digit'(Self)",
            ),
            (
                "test_character_value_factory",
                "Character",
                "value:",
                &["CP"],
                "call 'beamtalk_character':'value'(CP)",
            ),
            (
                "test_character_hash",
                "Character",
                "hash",
                &[],
                "call 'erlang':'phash2'(Self)",
            ),
            (
                "test_character_is_uppercase",
                "Character",
                "isUppercase",
                &[],
                "call 'beamtalk_character':'is_uppercase'(Self)",
            ),
            (
                "test_character_is_lowercase",
                "Character",
                "isLowercase",
                &[],
                "call 'beamtalk_character':'is_lowercase'(Self)",
            ),
            (
                "test_character_is_whitespace",
                "Character",
                "isWhitespace",
                &[],
                "call 'beamtalk_character':'is_whitespace'(Self)",
            ),
            (
                "test_character_uppercase",
                "Character",
                "uppercase",
                &[],
                "call 'beamtalk_character':'to_uppercase'(Self)",
            ),
            (
                "test_character_lowercase",
                "Character",
                "lowercase",
                &[],
                "call 'beamtalk_character':'to_lowercase'(Self)",
            ),
            (
                "test_character_comparison",
                "Character",
                "<",
                &["Other"],
                "call 'erlang':'<'(Self, Other)",
            ),
            // Pid primitive tests
            (
                "test_pid_is_alive",
                "Pid",
                "isAlive",
                &[],
                "call 'erlang':'is_process_alive'(Self)",
            ),
            (
                "test_pid_kill",
                "Pid",
                "kill",
                &[],
                "call 'erlang':'exit'(Self, 'kill')",
            ),
            (
                "test_pid_exit",
                "Pid",
                "exit:",
                &["Reason"],
                "call 'erlang':'exit'(Self, Reason)",
            ),
            // Integer character predicate tests
            (
                "test_integer_is_letter",
                "Integer",
                "isLetter",
                &[],
                "call 'beamtalk_character':'is_letter'(Self)",
            ),
            (
                "test_integer_is_digit",
                "Integer",
                "isDigit",
                &[],
                "call 'beamtalk_character':'is_digit'(Self)",
            ),
            // Array primitive tests
            (
                "test_array_size",
                "Array",
                "size",
                &[],
                "call 'beamtalk_array':'size'(Self)",
            ),
            (
                "test_array_is_empty",
                "Array",
                "isEmpty",
                &[],
                "call 'beamtalk_array':'is_empty'(Self)",
            ),
            (
                "test_array_do",
                "Array",
                "do:",
                &["Block"],
                "call 'beamtalk_array':'do'(Self, Block)",
            ),
            (
                "test_array_at",
                "Array",
                "at:",
                &["Index"],
                "call 'beamtalk_array':'at'(Self, Index)",
            ),
            (
                "test_array_at_put",
                "Array",
                "at:put:",
                &["Index", "Value"],
                "call 'beamtalk_array':'at_put'(Self, Index, Value)",
            ),
            (
                "test_array_with_all",
                "Array",
                "withAll:",
                &["List"],
                "call 'beamtalk_array':'from_list'(List)",
            ),
            (
                "test_array_collect",
                "Array",
                "collect:",
                &["Block"],
                "call 'beamtalk_array':'collect'(Self, Block)",
            ),
            (
                "test_array_select",
                "Array",
                "select:",
                &["Block"],
                "call 'beamtalk_array':'select'(Self, Block)",
            ),
            (
                "test_array_inject_into",
                "Array",
                "inject:into:",
                &["Init", "Block"],
                "call 'beamtalk_array':'inject_into'(Self, Init, Block)",
            ),
            (
                "test_array_includes",
                "Array",
                "includes:",
                &["Elem"],
                "call 'beamtalk_array':'includes'(Self, Elem)",
            ),
            (
                "test_array_print_string",
                "Array",
                "printString",
                &[],
                "call 'beamtalk_primitive':'string_from_binary'(call 'beamtalk_array':'print_string'(Self))",
            ),
            // Dictionary primitive tests
            (
                "test_dictionary_size",
                "Dictionary",
                "size",
                &[],
                "call 'erlang':'map_size'(Self)",
            ),
            (
                "test_dictionary_keys",
                "Dictionary",
                "keys",
                &[],
                "call 'maps':'keys'(Self)",
            ),
            (
                "test_dictionary_values",
                "Dictionary",
                "values",
                &[],
                "call 'maps':'values'(Self)",
            ),
            (
                "test_dictionary_at",
                "Dictionary",
                "at:",
                &["Key"],
                "call 'maps':'get'(Key, Self)",
            ),
            (
                "test_dictionary_at_if_absent",
                "Dictionary",
                "at:ifAbsent:",
                &["Key", "Block"],
                "call 'beamtalk_map':'at_if_absent'(Self, Key, Block)",
            ),
            (
                "test_dictionary_at_if_present",
                "Dictionary",
                "at:ifPresent:",
                &["Key", "Block"],
                "call 'beamtalk_map':'at_if_present'(Self, Key, Block)",
            ),
            (
                "test_dictionary_at_if_present_if_absent",
                "Dictionary",
                "at:ifPresent:ifAbsent:",
                &["Key", "Present", "Absent"],
                "call 'beamtalk_map':'at_if_present_if_absent'(Self, Key, Present, Absent)",
            ),
            (
                "test_dictionary_at_put",
                "Dictionary",
                "at:put:",
                &["Key", "Value"],
                "call 'maps':'put'(Key, Value, Self)",
            ),
            (
                "test_dictionary_includes_key",
                "Dictionary",
                "includesKey:",
                &["Key"],
                "call 'maps':'is_key'(Key, Self)",
            ),
            (
                "test_dictionary_remove_key",
                "Dictionary",
                "removeKey:",
                &["Key"],
                "call 'maps':'remove'(Key, Self)",
            ),
            (
                "test_dictionary_merge",
                "Dictionary",
                "merge:",
                &["Other"],
                "call 'maps':'merge'(Self, Other)",
            ),
            (
                "test_dictionary_do_with_key",
                "Dictionary",
                "doWithKey:",
                &["Block"],
                "call 'beamtalk_map':'do_with_key'(Self, Block)",
            ),
            (
                "test_dictionary_do",
                "Dictionary",
                "do:",
                &["Block"],
                "call 'beamtalk_map':'do'(Self, Block)",
            ),
            (
                "test_dictionary_includes",
                "Dictionary",
                "includes:",
                &["Elem"],
                "call 'beamtalk_map':'includes'(Self, Elem)",
            ),
            (
                "test_dictionary_print_string",
                "Dictionary",
                "printString",
                &[],
                "call 'beamtalk_primitive':'string_from_binary'(call 'beamtalk_map':'print_string'(Self))",
            ),
            // String primitive tests — transform group
            (
                "test_string_at",
                "String",
                "at:",
                &["Index"],
                "call 'beamtalk_string':'at'(Self, Index)",
            ),
            (
                "test_string_uppercase",
                "String",
                "uppercase",
                &[],
                "call 'beamtalk_primitive':'string_from_binary'(call 'unicode':'characters_to_binary'(call 'string':'uppercase'(call 'beamtalk_primitive':'string_to_binary'(Self))))",
            ),
            (
                "test_string_lowercase",
                "String",
                "lowercase",
                &[],
                "call 'beamtalk_primitive':'string_from_binary'(call 'unicode':'characters_to_binary'(call 'string':'lowercase'(call 'beamtalk_primitive':'string_to_binary'(Self))))",
            ),
            (
                "test_string_capitalize",
                "String",
                "capitalize",
                &[],
                "call 'beamtalk_string':'capitalize'(Self)",
            ),
            (
                "test_string_trim",
                "String",
                "trim",
                &[],
                "call 'beamtalk_primitive':'string_from_binary'(call 'unicode':'characters_to_binary'(call 'string':'trim'(call 'beamtalk_primitive':'string_to_binary'(Self), 'both')))",
            ),
            (
                "test_string_trim_left",
                "String",
                "trimLeft",
                &[],
                "call 'beamtalk_primitive':'string_from_binary'(call 'unicode':'characters_to_binary'(call 'string':'trim'(call 'beamtalk_primitive':'string_to_binary'(Self), 'leading')))",
            ),
            (
                "test_string_trim_right",
                "String",
                "trimRight",
                &[],
                "call 'beamtalk_primitive':'string_from_binary'(call 'unicode':'characters_to_binary'(call 'string':'trim'(call 'beamtalk_primitive':'string_to_binary'(Self), 'trailing')))",
            ),
            (
                "test_string_reverse",
                "String",
                "reverse",
                &[],
                "call 'beamtalk_string':'reverse'(Self)",
            ),
            (
                "test_string_comma_concat",
                "String",
                ",",
                &["Other"],
                "call 'beamtalk_primitive':'string_from_binary'(call 'erlang':'iolist_to_binary'([call 'beamtalk_primitive':'string_to_binary'(Self), call 'beamtalk_primitive':'string_to_binary'(Other)]))",
            ),
            // String primitive tests — search group
            (
                "test_string_includes",
                "String",
                "includes:",
                &["Sub"],
                "call 'beamtalk_string':'includes'(Self, Sub)",
            ),
            (
                "test_string_starts_with",
                "String",
                "startsWith:",
                &["Prefix"],
                "call 'beamtalk_string':'starts_with'(Self, Prefix)",
            ),
            (
                "test_string_ends_with",
                "String",
                "endsWith:",
                &["Suffix"],
                "call 'beamtalk_string':'ends_with'(Self, Suffix)",
            ),
            (
                "test_string_index_of",
                "String",
                "indexOf:",
                &["Sub"],
                "call 'beamtalk_string':'index_of'(Self, Sub)",
            ),
            (
                "test_string_split",
                "String",
                "split:",
                &["Sep"],
                "call 'beamtalk_string':'split'(Self, Sep)",
            ),
            (
                "test_string_split_on",
                "String",
                "splitOn:",
                &["Sep"],
                "call 'beamtalk_string':'split_on'(Self, Sep)",
            ),
            (
                "test_string_repeat",
                "String",
                "repeat:",
                &["N"],
                "call 'beamtalk_string':'repeat'(Self, N)",
            ),
            (
                "test_string_lines",
                "String",
                "lines",
                &[],
                "call 'beamtalk_string':'lines'(Self)",
            ),
            (
                "test_string_words",
                "String",
                "words",
                &[],
                "call 'beamtalk_string':'words'(Self)",
            ),
            (
                "test_string_replace_all_with",
                "String",
                "replaceAll:with:",
                &["Pat", "Repl"],
                "call 'beamtalk_primitive':'string_from_binary'(call 'binary':'replace'(call 'beamtalk_primitive':'string_to_binary'(Self), call 'beamtalk_primitive':'string_to_binary'(Pat), call 'beamtalk_primitive':'string_to_binary'(Repl), ['global']))",
            ),
            (
                "test_string_replace_first_with",
                "String",
                "replaceFirst:with:",
                &["Pat", "Repl"],
                "call 'beamtalk_primitive':'string_from_binary'(call 'binary':'replace'(call 'beamtalk_primitive':'string_to_binary'(Self), call 'beamtalk_primitive':'string_to_binary'(Pat), call 'beamtalk_primitive':'string_to_binary'(Repl), []))",
            ),
            (
                "test_string_take",
                "String",
                "take:",
                &["N"],
                "call 'beamtalk_string':'take'(Self, N)",
            ),
            (
                "test_string_drop",
                "String",
                "drop:",
                &["N"],
                "call 'beamtalk_string':'drop'(Self, N)",
            ),
            (
                "test_string_pad_left",
                "String",
                "padLeft:",
                &["N"],
                "call 'beamtalk_primitive':'string_from_binary'(call 'unicode':'characters_to_binary'(call 'string':'pad'(call 'beamtalk_primitive':'string_to_binary'(Self), N, 'leading')))",
            ),
            (
                "test_string_pad_right",
                "String",
                "padRight:",
                &["N"],
                "call 'beamtalk_primitive':'string_from_binary'(call 'unicode':'characters_to_binary'(call 'string':'pad'(call 'beamtalk_primitive':'string_to_binary'(Self), N, 'trailing')))",
            ),
            (
                "test_string_pad_left_with",
                "String",
                "padLeft:with:",
                &["N", "Ch"],
                "call 'beamtalk_primitive':'string_from_binary'(call 'unicode':'characters_to_binary'(call 'string':'pad'(call 'beamtalk_primitive':'string_to_binary'(Self), N, 'leading', call 'beamtalk_primitive':'string_to_binary'(Ch))))",
            ),
            (
                "test_string_pad_right_with",
                "String",
                "padRight:with:",
                &["N", "Ch"],
                "call 'beamtalk_primitive':'string_from_binary'(call 'unicode':'characters_to_binary'(call 'string':'pad'(call 'beamtalk_primitive':'string_to_binary'(Self), N, 'trailing', call 'beamtalk_primitive':'string_to_binary'(Ch))))",
            ),
            // String primitive tests — misc group
            (
                "test_string_is_blank",
                "String",
                "isBlank",
                &[],
                "call 'beamtalk_string':'is_blank'(Self)",
            ),
            (
                "test_string_is_digit",
                "String",
                "isDigit",
                &[],
                "call 'beamtalk_string':'is_digit'(Self)",
            ),
            (
                "test_string_is_alpha",
                "String",
                "isAlpha",
                &[],
                "call 'beamtalk_string':'is_alpha'(Self)",
            ),
            (
                "test_string_as_integer",
                "String",
                "asInteger",
                &[],
                "call 'erlang':'binary_to_integer'(call 'beamtalk_primitive':'string_to_binary'(Self))",
            ),
            (
                "test_string_as_float",
                "String",
                "asFloat",
                &[],
                "call 'erlang':'binary_to_float'(call 'beamtalk_primitive':'string_to_binary'(Self))",
            ),
            (
                "test_string_as_atom",
                "String",
                "asAtom",
                &[],
                "call 'erlang':'binary_to_atom'(call 'beamtalk_primitive':'string_to_binary'(Self), 'utf8')",
            ),
            (
                "test_string_as_list",
                "String",
                "asList",
                &[],
                "call 'beamtalk_string':'as_list'(Self)",
            ),
            (
                "test_string_each",
                "String",
                "each:",
                &["Block"],
                "call 'beamtalk_string':'each'(Self, Block)",
            ),
            (
                "test_string_collect",
                "String",
                "collect:",
                &["Block"],
                "call 'beamtalk_string':'collect'(Self, Block)",
            ),
            (
                "test_string_select",
                "String",
                "select:",
                &["Block"],
                "call 'beamtalk_string':'select'(Self, Block)",
            ),
            (
                "test_string_reject",
                "String",
                "reject:",
                &["Block"],
                "call 'beamtalk_string':'reject'(Self, Block)",
            ),
            (
                "test_string_stream",
                "String",
                "stream",
                &[],
                "call 'beamtalk_stream':'on'(Self)",
            ),
            (
                "test_string_with_all_factory",
                "String",
                "withAll:",
                &["List"],
                "call 'beamtalk_string':'join'(List)",
            ),
            (
                "test_string_from_code_point_factory",
                "String",
                "fromCodePoint:",
                &["CP"],
                "call 'beamtalk_string':'from_code_point'(CP)",
            ),
            (
                "test_string_from_code_points_factory",
                "String",
                "fromCodePoints:",
                &["CPs"],
                "call 'beamtalk_string':'from_code_points'(CPs)",
            ),
            (
                "test_string_from_iolist_factory",
                "String",
                "fromIolist:",
                &["IoList"],
                "call 'beamtalk_string':'from_iolist'(IoList)",
            ),
            // String primitive tests — comparison group
            (
                "test_string_equality",
                "String",
                "=:=",
                &["Other"],
                "call 'erlang':'=:='(Self, Other)",
            ),
            (
                "test_string_not_equal",
                "String",
                "/=",
                &["Other"],
                "call 'erlang':'/='(Self, Other)",
            ),
            (
                "test_string_strict_not_equal",
                "String",
                "=/=",
                &["Other"],
                "call 'erlang':'=/='(Self, Other)",
            ),
            (
                "test_string_less_than",
                "String",
                "<",
                &["Other"],
                "call 'erlang':'<'(Self, Other)",
            ),
            (
                "test_string_greater_than",
                "String",
                ">",
                &["Other"],
                "call 'erlang':'>'(Self, Other)",
            ),
            (
                "test_string_less_than_or_equal",
                "String",
                "<=",
                &["Other"],
                "call 'erlang':'=<'(Self, Other)",
            ),
            (
                "test_string_greater_than_or_equal",
                "String",
                ">=",
                &["Other"],
                "call 'erlang':'>='(Self, Other)",
            ),
            // String primitive tests — regex group
            (
                "test_string_matches_regex",
                "String",
                "matchesRegex:",
                &["Pat"],
                "call 'beamtalk_regex':'matches_regex'(Self, Pat)",
            ),
            (
                "test_string_matches_regex_options",
                "String",
                "matchesRegex:options:",
                &["Pat", "Opts"],
                "call 'beamtalk_regex':'matches_regex_options'(Self, Pat, Opts)",
            ),
            (
                "test_string_first_match",
                "String",
                "firstMatch:",
                &["Pat"],
                "call 'beamtalk_regex':'first_match'(Self, Pat)",
            ),
            (
                "test_string_all_matches",
                "String",
                "allMatches:",
                &["Pat"],
                "call 'beamtalk_regex':'all_matches'(Self, Pat)",
            ),
            (
                "test_string_replace_regex_with",
                "String",
                "replaceRegex:with:",
                &["Pat", "Repl"],
                "call 'beamtalk_regex':'replace_regex'(Self, Pat, Repl)",
            ),
            (
                "test_string_replace_all_regex_with",
                "String",
                "replaceAllRegex:with:",
                &["Pat", "Repl"],
                "call 'beamtalk_regex':'replace_all_regex'(Self, Pat, Repl)",
            ),
            (
                "test_string_split_regex",
                "String",
                "splitRegex:",
                &["Pat"],
                "call 'beamtalk_regex':'split_regex'(Self, Pat)",
            ),
            // CompiledMethod primitive tests
            (
                "test_compiled_method_selector",
                "CompiledMethod",
                "selector",
                &[],
                "call 'beamtalk_compiled_method_ops':'dispatch'('selector', [], Self)",
            ),
            (
                "test_compiled_method_source",
                "CompiledMethod",
                "source",
                &[],
                "call 'beamtalk_compiled_method_ops':'dispatch'('source', [], Self)",
            ),
            (
                "test_compiled_method_doc",
                "CompiledMethod",
                "doc",
                &[],
                "call 'beamtalk_compiled_method_ops':'dispatch'('doc', [], Self)",
            ),
            (
                "test_compiled_method_argument_count",
                "CompiledMethod",
                "argumentCount",
                &[],
                "call 'beamtalk_compiled_method_ops':'dispatch'('argumentCount', [], Self)",
            ),
            (
                "test_compiled_method_origin",
                "CompiledMethod",
                "origin",
                &[],
                "call 'beamtalk_compiled_method_ops':'dispatch'('origin', [], Self)",
            ),
            (
                "test_compiled_method_print_string",
                "CompiledMethod",
                "printString",
                &[],
                "call 'beamtalk_compiled_method_ops':'dispatch'('printString', [], Self)",
            ),
            (
                "test_compiled_method_as_string",
                "CompiledMethod",
                "asString",
                &[],
                "call 'beamtalk_compiled_method_ops':'dispatch'('asString', [], Self)",
            ),
            // Integer arithmetic — remaining ops
            (
                "test_integer_minus",
                "Integer",
                "-",
                &["Other"],
                "call 'erlang':'-'(Self, Other)",
            ),
            (
                "test_integer_multiply",
                "Integer",
                "*",
                &["Other"],
                "call 'erlang':'*'(Self, Other)",
            ),
            (
                "test_integer_divide",
                "Integer",
                "/",
                &["Other"],
                "call 'erlang':'/'(Self, Other)",
            ),
            (
                "test_integer_integer_divide",
                "Integer",
                "div:",
                &["Other"],
                "call 'erlang':'div'(Self, Other)",
            ),
            (
                "test_integer_power",
                "Integer",
                "**",
                &["Other"],
                "call 'erlang':'round'(call 'math':'pow'(\
                 call 'erlang':'float'(Self), call 'erlang':'float'(Other)))",
            ),
            // Integer comparisons — all 7 operators
            (
                "test_integer_eq",
                "Integer",
                "=:=",
                &["Other"],
                "call 'erlang':'=:='(Self, Other)",
            ),
            (
                "test_integer_neq",
                "Integer",
                "/=",
                &["Other"],
                "call 'erlang':'/='(Self, Other)",
            ),
            (
                "test_integer_strict_neq",
                "Integer",
                "=/=",
                &["Other"],
                "call 'erlang':'=/='(Self, Other)",
            ),
            (
                "test_integer_lt",
                "Integer",
                "<",
                &["Other"],
                "call 'erlang':'<'(Self, Other)",
            ),
            (
                "test_integer_gt",
                "Integer",
                ">",
                &["Other"],
                "call 'erlang':'>'(Self, Other)",
            ),
            (
                "test_integer_le",
                "Integer",
                "<=",
                &["Other"],
                "call 'erlang':'=<'(Self, Other)",
            ),
            (
                "test_integer_ge",
                "Integer",
                ">=",
                &["Other"],
                "call 'erlang':'>='(Self, Other)",
            ),
            // Integer conversions — asFloat and printString
            (
                "test_integer_as_float",
                "Integer",
                "asFloat",
                &[],
                "call 'erlang':'float'(Self)",
            ),
            (
                "test_integer_print_string",
                "Integer",
                "printString",
                &[],
                "call 'beamtalk_primitive':'string_from_binary'(call 'erlang':'integer_to_binary'(Self))",
            ),
            // Integer bitwise operations
            (
                "test_integer_bit_and",
                "Integer",
                "bitAnd:",
                &["Other"],
                "call 'erlang':'band'(Self, Other)",
            ),
            (
                "test_integer_bit_or",
                "Integer",
                "bitOr:",
                &["Other"],
                "call 'erlang':'bor'(Self, Other)",
            ),
            (
                "test_integer_bit_xor",
                "Integer",
                "bitXor:",
                &["Other"],
                "call 'erlang':'bxor'(Self, Other)",
            ),
            (
                "test_integer_bit_not",
                "Integer",
                "bitNot",
                &[],
                "call 'erlang':'bnot'(Self)",
            ),
            (
                "test_integer_bit_shift",
                "Integer",
                "bitShift:",
                &["N"],
                "case call 'erlang':'>='(N, 0) of \
                 'true' when 'true' -> call 'erlang':'bsl'(Self, N) \
                 'false' when 'true' -> call 'erlang':'bsr'(Self, call 'erlang':'-'(0, N)) end",
            ),
            // Integer character predicates — remaining three
            (
                "test_integer_is_uppercase",
                "Integer",
                "isUppercase",
                &[],
                "call 'beamtalk_character':'is_uppercase'(Self)",
            ),
            (
                "test_integer_is_lowercase",
                "Integer",
                "isLowercase",
                &[],
                "call 'beamtalk_character':'is_lowercase'(Self)",
            ),
            (
                "test_integer_is_whitespace",
                "Integer",
                "isWhitespace",
                &[],
                "call 'beamtalk_character':'is_whitespace'(Self)",
            ),
            // Integer math functions
            (
                "test_integer_sqrt",
                "Integer",
                "sqrt",
                &[],
                "call 'math':'sqrt'(call 'erlang':'float'(Self))",
            ),
            (
                "test_integer_log",
                "Integer",
                "log",
                &[],
                "call 'math':'log'(call 'erlang':'float'(Self))",
            ),
            (
                "test_integer_ln",
                "Integer",
                "ln",
                &[],
                "call 'math':'log'(call 'erlang':'float'(Self))",
            ),
            (
                "test_integer_log2",
                "Integer",
                "log2",
                &[],
                "call 'math':'log2'(call 'erlang':'float'(Self))",
            ),
            (
                "test_integer_log10",
                "Integer",
                "log10",
                &[],
                "call 'math':'log10'(call 'erlang':'float'(Self))",
            ),
            (
                "test_integer_exp",
                "Integer",
                "exp",
                &[],
                "call 'math':'exp'(call 'erlang':'float'(Self))",
            ),
            (
                "test_integer_raised_to",
                "Integer",
                "raisedTo:",
                &["Exp"],
                "case call 'erlang':'is_integer'(Exp) of \
                 'true' when 'true' -> \
                   case call 'erlang':'>='(Exp, 0) of \
                     'true' when 'true' -> \
                       call 'erlang':'round'(call 'math':'pow'(call 'erlang':'float'(Self), call 'erlang':'float'(Exp))) \
                     'false' when 'true' -> \
                       call 'math':'pow'(call 'erlang':'float'(Self), call 'erlang':'float'(Exp)) \
                   end \
                 'false' when 'true' -> \
                   call 'math':'pow'(call 'erlang':'float'(Self), call 'erlang':'float'(Exp)) \
               end",
            ),
        ]);
    }

    #[test]
    fn generate_primitive_bif_none_table() {
        assert_primitive_bif_none_table(&[
            ("test_unknown_class", "Counter", "+", &["Other"]),
            ("test_unknown_selector", "Integer", "unknownMethod", &[]),
            ("test_array_unknown_selector", "Array", "notAMethod", &[]),
            (
                "test_dictionary_unknown_selector",
                "Dictionary",
                "notAMethod",
                &[],
            ),
            (
                "test_string_unknown_selector",
                "String",
                "notAStringMethod",
                &[],
            ),
            (
                "test_compiled_method_unknown_selector",
                "CompiledMethod",
                "notAMethod",
                &[],
            ),
            // Integer edge cases
            (
                "test_integer_unknown_selector",
                "Integer",
                "notAMethod",
                &[],
            ),
            ("test_integer_binary_op_missing_param", "Integer", "+", &[]),
            ("test_integer_power_missing_param", "Integer", "**", &[]),
            (
                "test_integer_bit_shift_missing_param",
                "Integer",
                "bitShift:",
                &[],
            ),
            (
                "test_integer_raised_to_missing_param",
                "Integer",
                "raisedTo:",
                &[],
            ),
        ]);
    }
}
