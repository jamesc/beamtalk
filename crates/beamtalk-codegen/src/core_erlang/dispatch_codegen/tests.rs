// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use super::{class_self_send_reflective_primitive, is_class_auto_export_selector};
use crate::core_erlang::CoreErlangGenerator;
use crate::core_erlang::expr_shape::is_character_typed_receiver;
use beamtalk_core::ast::{
    Expression, Identifier, KeywordPart, Literal, MessageSelector, MethodDefinition, TypeAnnotation,
};
use beamtalk_core::source_analysis::{Severity, Span, lex_with_eof, parse};
use std::collections::BTreeSet;

fn s() -> Span {
    Span::new(0, 0)
}

/// Index of the `HANDLERS` entry named `name`, or panics — shared by the
/// ordering tests below.
fn handlers_index_of(name: &str) -> usize {
    super::HANDLERS
        .iter()
        .position(|(n, _)| *n == name)
        .unwrap_or_else(|| panic!("no HANDLERS entry named {name:?}"))
}

/// A duplicated name would break `handlers_index_of` (and
/// `try_handle_character_typed_message`'s by-name skip) silently: lookups
/// would resolve to whichever entry happens to come first, with no signal
/// that a second, unreachable entry exists.
#[test]
fn handlers_have_unique_names() {
    let mut seen = std::collections::HashSet::new();
    for (name, _) in super::HANDLERS {
        assert!(seen.insert(*name), "duplicate HANDLERS entry: {name:?}");
    }
}

/// `character_typed` must precede `protoobject`/`object` in
/// `HANDLERS` — those two unconditionally claim `class`/`respondsTo:`/
/// `perform:` family selectors for *any* receiver (keyed on runtime
/// `class_of/1`), so a Character-typed receiver's `class`/`respondsTo:`/
/// `perform:` send would be wrongly resolved by them before
/// `try_handle_character_typed_message` ever got a chance to override it.
/// `try_handle_character_typed_message` itself skips this entry by *name*
/// when re-running the rest of the chain (not by slicing past a hardcoded
/// index), so this ordering is the one remaining position-sensitive part —
/// a reorder that violates it fails loudly here instead of miscompiling
/// Character-typed sends silently.
#[test]
fn character_typed_handler_precedes_protoobject_and_object() {
    let character_typed = handlers_index_of("character_typed");
    assert!(character_typed < handlers_index_of("protoobject"));
    assert!(character_typed < handlers_index_of("object"));
}

/// Dictionary iteration selectors (`do:`, `doWithKey:`, `keysAndValuesDo:`)
/// overlap with List's, so `dict` must run before `list` or a Dictionary
/// receiver's `do:` would be miscompiled as a List operation.
#[test]
fn dict_handler_precedes_list_handler() {
    assert!(handlers_index_of("dict") < handlers_index_of("list"));
}

/// A self-send in a `class`-side method must route through `class_send`
/// (`class_method_self_send`), not the actor-instance direct-dispatch path
/// (`self_dispatch`) — the two key on different `self`-binding conventions
/// (`ClassSelf` vs `Self`), so trying `self_dispatch` first would resolve a
/// class-method self-send against the wrong process.
#[test]
fn class_method_self_send_precedes_self_dispatch() {
    assert!(handlers_index_of("class_method_self_send") < handlers_index_of("self_dispatch"));
}

/// The classifier must stay in sync with the actual reachable auto-exports
/// on generated class modules. `class_name/0` is reachable via plain
/// self-send and must short-circuit to a direct call; `superclass` moved to
/// `class_self_send_reflective_primitive` because its raw export returns a
/// bare atom instead of a class object,
/// so it must NOT be classified as an auto-export here anymore.
/// `methods/0` does not exist on the current codegen (an earlier mistaken
/// inclusion); `method_table/0` and `has_method/1` are codegen-internal
/// reflection APIs with no Beamtalk surface and must NOT be classified as
/// auto-exports (they would compile to a direct call that users cannot
/// reach anyway, but including them would bypass the structured DNU path
/// that catches typos). Arity mismatches must also return false so that,
/// e.g., `self class_name: X` does not get hijacked into a direct call to
/// the 0-arity `class_name/0`.
#[test]
fn is_class_auto_export_selector_matches_reachable_exports() {
    assert!(is_class_auto_export_selector("class_name", 0));

    // `superclass` now routes through the reflective-primitive path (its
    // raw export is unwrapped and identity-broken), not here.
    assert!(!is_class_auto_export_selector("superclass", 0));

    // Codegen-internal, not reachable via Beamtalk self-send.
    assert!(!is_class_auto_export_selector("method_table", 0));
    assert!(!is_class_auto_export_selector("has_method", 1));
    assert!(!is_class_auto_export_selector("register_class", 0));
    assert!(!is_class_auto_export_selector("__beamtalk_meta", 0));

    // Historical mistake — `methods/0` is not emitted by the current
    // codegen, so classifying it as auto-export would produce a call
    // to a non-existent function.
    assert!(!is_class_auto_export_selector("methods", 0));

    // Arity mismatches must not match.
    assert!(!is_class_auto_export_selector("superclass", 1));
    assert!(!is_class_auto_export_selector("class_name", 1));

    // Arbitrary user selectors must fall through to inherited dispatch.
    assert!(!is_class_auto_export_selector("increment", 0));
    assert!(!is_class_auto_export_selector("at:put:", 2));
}

/// `superclass` and `includesSelector:` must route to their
/// real `beamtalk_behaviour_intrinsics` implementations so a
/// class-method self-send produces the same value non-self-send dispatch
/// would (a genuine `#beamtalk_object{}` for `superclass`, a proper
/// dispatch instead of DNU for `includesSelector:`). Selectors whose
/// intrinsic is not deadlock-safe from inside the class's own process
/// (`subclasses`, `allSubclasses` — see the function doc) must NOT
/// appear here; arity mismatches must not match either.
#[test]
fn class_self_send_reflective_primitive_matches_safe_selectors_only() {
    assert_eq!(
        class_self_send_reflective_primitive("superclass", 0),
        Some("classSuperclass")
    );
    assert_eq!(
        class_self_send_reflective_primitive("includesSelector:", 1),
        Some("classIncludesSelector")
    );

    // Arity mismatches must not match.
    assert_eq!(class_self_send_reflective_primitive("superclass", 1), None);
    assert_eq!(
        class_self_send_reflective_primitive("includesSelector:", 0),
        None
    );

    // Not deadlock-safe (unconditional gen_server:call in the intrinsic) —
    // must stay off this list until audited/fixed.
    assert_eq!(class_self_send_reflective_primitive("subclasses", 0), None);
    assert_eq!(
        class_self_send_reflective_primitive("allSubclasses", 0),
        None
    );

    // Arbitrary user selectors must fall through to inherited dispatch.
    assert_eq!(class_self_send_reflective_primitive("increment", 0), None);
}

/// ADR 0109: `File open:…do:` is lowered at the call site so the
/// user's block runs in the caller rather than the File class `gen_server`.
/// The interception is keyed on the *unqualified* stdlib `File` — a
/// package-qualified `mylib@File` is an unrelated class that happens to
/// share the name, and must keep reaching its own implementation.
#[test]
fn block_scoped_file_open_is_lowered_only_for_unqualified_file() {
    /// Lowers `[package@]File <keywords>` with one argument per keyword.
    fn lower(package: Option<&str>, keywords: &[&str]) -> String {
        let mut generator = CoreErlangGenerator::new("test");
        let receiver = Expression::ClassReference {
            name: Identifier::new("File", s()),
            package: package.map(|p| Identifier::new(p, s())),
            span: s(),
        };
        let selector =
            MessageSelector::Keyword(keywords.iter().map(|k| KeywordPart::new(*k, s())).collect());
        let arguments: Vec<_> = keywords
            .iter()
            .map(|k| Expression::Identifier(Identifier::new(k.trim_end_matches(':'), s())))
            .collect();
        generator
            .generate_message_send(&receiver, &selector, &arguments)
            .unwrap()
            .to_pretty_string()
    }

    // Both intercepted selectors, so dropping either from the `matches!`
    // list fails here rather than silently reintroducing the deadlock.
    for keywords in [&["open:", "do:"][..], &["open:", "mode:", "do:"][..]] {
        let selector_atom = keywords.concat();

        let unqualified = lower(None, keywords);
        assert!(
            unqualified.contains("'native_call'(")
                && unqualified.contains("'beamtalk_file'")
                && unqualified.contains(&format!("'{selector_atom}'")),
            "unqualified File {selector_atom} should lower to a native_call in the \
             caller. Got: {unqualified}"
        );
        assert!(
            !unqualified.contains("class_send"),
            "the block must not reach the class gen_server. Got: {unqualified}"
        );

        // A package-qualified receiver keeps the ordinary class-send path:
        // asserted positively, so an empty or otherwise-shaped lowering
        // cannot pass by merely lacking the stdlib module name.
        let qualified = lower(Some("mylib"), keywords);
        assert!(
            !qualified.contains("'beamtalk_file'"),
            "mylib@File {selector_atom} is a different class and must not be \
             redirected to the stdlib File shim. Got: {qualified}"
        );
        assert!(
            qualified.contains("class_send"),
            "mylib@File {selector_atom} should fall through to a normal class \
             send. Got: {qualified}"
        );
    }
}

#[test]
fn test_generate_message_send_unary_uses_dispatch() {
    let mut generator = CoreErlangGenerator::new("test");
    let receiver = Expression::Identifier(Identifier::new("counter", s()));
    let selector = MessageSelector::Unary("increment".into());
    let doc = generator
        .generate_message_send(&receiver, &selector, &[])
        .unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("beamtalk_message_dispatch':'send'("),
        "unary send should use unified dispatch. Got: {output}"
    );
    assert!(
        output.contains("'increment'"),
        "should include selector atom. Got: {output}"
    );
}

#[test]
fn test_generate_cast_send_non_actor_routes_via_cast() {
    let mut generator = CoreErlangGenerator::new("test");
    let receiver = Expression::Identifier(Identifier::new("other", s()));
    let selector = MessageSelector::Unary("doIt".into());
    let doc = generator
        .generate_cast_send(&receiver, &selector, &[])
        .unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("beamtalk_message_dispatch':'cast'("),
        "non-actor cast send should route through cast/3. Got: {output}"
    );
}

#[test]
fn test_generate_super_send_uses_beamtalk_dispatch() {
    let mut generator = CoreErlangGenerator::new("test");
    let selector = MessageSelector::Unary("initialize".into());
    let doc = generator.generate_super_send(&selector, &[]).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("beamtalk_dispatch':'super'("),
        "super send should use beamtalk_dispatch:super. Got: {output}"
    );
    assert!(
        output.contains("'initialize'"),
        "should include selector. Got: {output}"
    );
}

/// In a value/primitive context the generated fun is
/// `fun(Args, Self) -> Result` with no `State` binding, so `super` must
/// lower to `super_value/4` rather than the state-threading `super/5`.
/// Referencing the absent `State` produced invalid Core Erlang
/// (variable 'State' is unbound).
#[test]
fn test_generate_super_send_value_context_uses_super_value() {
    let mut generator = CoreErlangGenerator::new("test");
    generator.context = crate::core_erlang::CodeGenContext::ValueType;
    let selector = MessageSelector::Unary("printString".into());
    let doc = generator.generate_super_send(&selector, &[]).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("beamtalk_dispatch':'super_value'("),
        "value-context super should route to super_value/4. Got: {output}"
    );
    assert!(
        !output.contains("State"),
        "value-context super must not reference an unbound State. Got: {output}"
    );
    assert!(
        output.contains("'printString'"),
        "should include selector. Got: {output}"
    );
}

#[test]
fn test_generate_actor_spawn_non_repl() {
    let mut generator = CoreErlangGenerator::new("test");
    let doc = generator
        .generate_actor_spawn_qualified("Counter", None, None)
        .unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'spawn'()"),
        "spawn should call spawn/0. Got: {output}"
    );
    assert!(
        output.contains("counter"),
        "spawn should reference module. Got: {output}"
    );
}

#[test]
fn test_generate_message_send_keyword_includes_selector() {
    let mut generator = CoreErlangGenerator::new("test");
    let receiver = Expression::Identifier(Identifier::new("obj", s()));
    let selector = MessageSelector::Keyword(vec![
        KeywordPart::new("at:", s()),
        KeywordPart::new("put:", s()),
    ]);
    let arguments = vec![
        Expression::Literal(Literal::Integer(1), s()),
        Expression::Literal(Literal::Integer(2), s()),
    ];
    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'at:put:'"),
        "keyword send should combine selector parts. Got: {output}"
    );
}

#[test]
fn test_generate_message_send_binary_op_addition() {
    let mut generator = CoreErlangGenerator::new("test");
    let receiver = Expression::Literal(Literal::Integer(3), s());
    let selector = MessageSelector::Binary("+".into());
    let arguments = vec![Expression::Literal(Literal::Integer(4), s())];
    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("erlang':'+'("),
        "binary + should compile to erlang arithmetic. Got: {output}"
    );
}

#[test]
fn test_generate_cast_send_actor_self_uses_safe_dispatch() {
    let mut generator = CoreErlangGenerator::new("test");
    generator.context = crate::core_erlang::CodeGenContext::Actor;
    let receiver = Expression::Identifier(Identifier::new("self", s()));
    let selector = MessageSelector::Unary("doIt".into());
    let doc = generator
        .generate_cast_send(&receiver, &selector, &[])
        .unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("safe_dispatch"),
        "actor self cast should use safe_dispatch. Got: {output}"
    );
    assert!(
        output.contains("'ok'"),
        "actor self cast should return 'ok'. Got: {output}"
    );
}

/// Self-cast inside a block must route through the actor mailbox,
/// not call `safe_dispatch` directly, because the block may execute in a
/// different process (Timer callback, cross-actor callback).
#[test]
fn test_generate_cast_send_actor_self_in_block_uses_mailbox() {
    let mut generator = CoreErlangGenerator::new("test");
    generator.context = crate::core_erlang::CodeGenContext::Actor;
    generator.block_depth = 1; // Simulate being inside a block
    let receiver = Expression::Identifier(Identifier::new("self", s()));
    let selector = MessageSelector::Unary("bump".into());
    let doc = generator
        .generate_cast_send(&receiver, &selector, &[])
        .unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("beamtalk_message_dispatch"),
        "self cast inside block should route through mailbox. Got: {output}"
    );
    assert!(
        output.contains("cast"),
        "self cast inside block should use cast dispatch. Got: {output}"
    );
    assert!(
        !output.contains("safe_dispatch"),
        "self cast inside block must NOT use safe_dispatch. Got: {output}"
    );
}

/// `is_character_typed_receiver` must recognize both syntactic
/// shapes that statically produce a Character — a literal (`$A`) and a
/// `Character value:` factory call — including through any number of
/// parenthesizations, since `(Character value: 10) asString` parses the
/// factory call as `Parenthesized(MessageSend(..))`. Everything else
/// (plain integers, other class factory methods, a package-qualified
/// `Character`) must NOT match, or the codegen would incorrectly route
/// an actual Integer/other-class receiver through Character's dispatch.
#[test]
fn is_character_typed_receiver_matches_literal_and_value_factory() {
    let char_literal = Expression::Literal(Literal::Character('A'), s());
    assert!(is_character_typed_receiver(&char_literal));

    let character_value_call = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Character", s()),
            package: None,
            span: s(),
        }),
        selector: MessageSelector::Keyword(vec![KeywordPart::new("value:", s())]),
        arguments: vec![Expression::Literal(Literal::Integer(10), s())],
        is_cast: false,
        span: s(),
    };
    assert!(is_character_typed_receiver(&character_value_call));

    // The reported bug's exact shape: `(Character value: 10)` as a
    // parenthesized receiver of a further send (`asString`).
    let parenthesized_once = Expression::Parenthesized {
        expression: Box::new(character_value_call.clone()),
        span: s(),
    };
    assert!(is_character_typed_receiver(&parenthesized_once));

    // Nested parens must also see through.
    let parenthesized_twice = Expression::Parenthesized {
        expression: Box::new(parenthesized_once),
        span: s(),
    };
    assert!(is_character_typed_receiver(&parenthesized_twice));

    // A parenthesized literal must match too (`($A) asString`).
    let parenthesized_literal = Expression::Parenthesized {
        expression: Box::new(char_literal),
        span: s(),
    };
    assert!(is_character_typed_receiver(&parenthesized_literal));
}

/// `uppercase`/`lowercase` also have a declared `-> Character`
/// return type (`character.bt`), so a chain like `$a uppercase asString`
/// hits the identical bug as `(Character value: 10) asString` — the
/// receiver of `asString` (`$a uppercase`) is statically Character but
/// isn't a literal or a `value:` call. The check must recurse: applying
/// `uppercase`/`lowercase` to an already Character-typed receiver stays
/// Character-typed, however deep the chain (`$a uppercase lowercase`).
#[test]
fn is_character_typed_receiver_recurses_through_uppercase_lowercase() {
    fn unary_send(receiver: Expression, selector: &str) -> Expression {
        Expression::MessageSend {
            receiver: Box::new(receiver),
            selector: MessageSelector::Unary(selector.into()),
            arguments: vec![],
            is_cast: false,
            span: s(),
        }
    }

    let char_literal = Expression::Literal(Literal::Character('a'), s());
    let uppercased = unary_send(char_literal.clone(), "uppercase");
    assert!(is_character_typed_receiver(&uppercased));

    // Chains recurse arbitrarily deep.
    let round_tripped = unary_send(uppercased, "lowercase");
    assert!(is_character_typed_receiver(&round_tripped));

    // Also recognized on a `Character value:` receiver, not just a literal.
    let value_call = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Character", s()),
            package: None,
            span: s(),
        }),
        selector: MessageSelector::Keyword(vec![KeywordPart::new("value:", s())]),
        arguments: vec![Expression::Literal(Literal::Integer(97), s())],
        is_cast: false,
        span: s(),
    };
    assert!(is_character_typed_receiver(&unary_send(
        value_call,
        "uppercase"
    )));

    // A non-Character-returning unary selector on a Character receiver
    // must NOT match — only `uppercase`/`lowercase` are Character-typed.
    assert!(!is_character_typed_receiver(&unary_send(
        char_literal.clone(),
        "asInteger"
    )));

    // `uppercase` on a receiver that is NOT itself Character-typed must
    // not match — recursion must terminate on a real Character source,
    // not accept any arbitrarily nested `uppercase` send.
    let int_literal = Expression::Literal(Literal::Integer(97), s());
    assert!(!is_character_typed_receiver(&unary_send(
        int_literal,
        "uppercase"
    )));
}

/// Enforces the invariant `is_character_typed_receiver` depends
/// on — that its hardcoded selector set (`value:` as the class factory,
/// `uppercase`/`lowercase` as the Character-returning instance methods)
/// is *exactly* the set of methods `stdlib/src/character.bt` declares
/// with a `-> Character` return type. This is the enforcing test
/// architecture-principles.md requires for any "must stay in sync"
/// coupling: parses the real `character.bt` off disk and fails loudly if
/// a future edit adds, removes, or renames a Character-returning method
/// there without updating the codegen recognizer to match — silent drift
/// here would silently reopen the exact bug this issue fixes for the new
/// method (dispatch misrouted to Integer's BIF module).
#[test]
fn character_bt_character_returning_methods_match_codegen_recognizer() {
    let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crates/")
        .parent()
        .expect("repo root")
        .to_path_buf();
    let character_bt_path = repo_root.join("stdlib/src/character.bt");
    let Ok(source) = std::fs::read_to_string(&character_bt_path) else {
        eprintln!(
            "skipping: {} not present in this checkout",
            character_bt_path.display()
        );
        return;
    };

    let tokens = lex_with_eof(&source);
    let (module, diags) = parse(tokens);
    assert!(
        diags.iter().all(|d| d.severity != Severity::Error),
        "character.bt must parse without errors: {diags:?}"
    );

    let character_class = module
        .classes
        .iter()
        .find(|c| c.name.name == "Character")
        .expect("character.bt must define the Character class");

    let returns_character = |method: &MethodDefinition| -> bool {
        matches!(
            &method.return_type,
            Some(TypeAnnotation::Simple(id)) if id.name == "Character"
        )
    };

    let class_side: BTreeSet<String> = character_class
        .class_methods
        .iter()
        .filter(|m| returns_character(m))
        .map(|m| m.selector.name().to_string())
        .collect();
    let instance_side: BTreeSet<String> = character_class
        .methods
        .iter()
        .filter(|m| returns_character(m))
        .map(|m| m.selector.name().to_string())
        .collect();

    assert_eq!(
        class_side,
        BTreeSet::from(["value:".to_string()]),
        "is_character_typed_receiver's class-side factory-method list \
         (\"value:\") no longer matches character.bt's actual \
         `-> Character` class methods — update the recognizer in \
         dispatch_codegen.rs to match"
    );
    assert_eq!(
        instance_side,
        BTreeSet::from(["uppercase".to_string(), "lowercase".to_string()]),
        "is_character_typed_receiver's instance-side selector list \
         (\"uppercase\", \"lowercase\") no longer matches character.bt's \
         actual `-> Character` instance methods — update the recognizer \
         in dispatch_codegen.rs to match"
    );
}

#[test]
fn is_character_typed_receiver_rejects_non_character_shapes() {
    // A bare integer literal is not Character-typed.
    assert!(!is_character_typed_receiver(&Expression::Literal(
        Literal::Integer(10),
        s()
    )));

    // A different class's factory method must not match.
    let other_factory = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Integer", s()),
            package: None,
            span: s(),
        }),
        selector: MessageSelector::Keyword(vec![KeywordPart::new("value:", s())]),
        arguments: vec![Expression::Literal(Literal::Integer(10), s())],
        is_cast: false,
        span: s(),
    };
    assert!(!is_character_typed_receiver(&other_factory));

    // A different selector on Character itself must not match — only
    // the `value:` factory is statically known to return Character.
    let wrong_selector = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Character", s()),
            package: None,
            span: s(),
        }),
        selector: MessageSelector::Unary("someOtherMethod".into()),
        arguments: vec![],
        is_cast: false,
        span: s(),
    };
    assert!(!is_character_typed_receiver(&wrong_selector));

    // A package-qualified `Character` is a different, user-defined class
    // that merely shares the name — must not be special-cased.
    let package_qualified = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Character", s()),
            package: Some(Identifier::new("mylib", s())),
            span: s(),
        }),
        selector: MessageSelector::Keyword(vec![KeywordPart::new("value:", s())]),
        arguments: vec![Expression::Literal(Literal::Integer(10), s())],
        is_cast: false,
        span: s(),
    };
    assert!(!is_character_typed_receiver(&package_qualified));
}

/// Codegen for `(Character value: 10) asString` must emit a
/// direct call to `bt@stdlib@character:dispatch/3`, not fall through to
/// the generic runtime-dispatch path (which would key on `is_integer/1`
/// and misroute to `bt@stdlib@integer`, producing `"10"` instead of a
/// genuine 1-byte LF string).
#[test]
fn character_value_factory_receiver_dispatches_to_character_module() {
    let mut generator = CoreErlangGenerator::new("test");
    let receiver = Expression::Parenthesized {
        expression: Box::new(Expression::MessageSend {
            receiver: Box::new(Expression::ClassReference {
                name: Identifier::new("Character", s()),
                package: None,
                span: s(),
            }),
            selector: MessageSelector::Keyword(vec![KeywordPart::new("value:", s())]),
            arguments: vec![Expression::Literal(Literal::Integer(10), s())],
            is_cast: false,
            span: s(),
        }),
        span: s(),
    };
    let selector = MessageSelector::Unary("asString".into());
    let output = generator
        .generate_message_send(&receiver, &selector, &[])
        .unwrap()
        .to_pretty_string();

    assert!(
        output.contains("'bt@stdlib@character':'dispatch'"),
        "expected direct Character dispatch, got: {output}"
    );
    assert!(
        !output.contains("beamtalk_message_dispatch"),
        "must not fall through to generic runtime dispatch (which would \
         misroute via is_integer/1 to Integer). Got: {output}"
    );
}

// ─── generate_field_assignment_open (Closure::Open) × FieldWriteSite ─

/// Builds `self.<field_name> := <value>` as an `Expression::Assignment` over
/// a `self`-receiver `FieldAccess` target — the shape
/// `generate_field_assignment_open` matches on.
fn self_field_assignment_expr(field_name: &str, value: Expression) -> Expression {
    Expression::Assignment {
        target: Box::new(Expression::FieldAccess {
            receiver: Box::new(Expression::Identifier(Identifier::new("self", s()))),
            field: Identifier::new(field_name, s()),
            span: s(),
        }),
        value: Box::new(value),
        type_annotation: None,
        span: s(),
    }
}

#[test]
fn test_field_assignment_open_actor_threads_state() {
    let mut generator = CoreErlangGenerator::new("test");
    let expr = self_field_assignment_expr("count", Expression::Literal(Literal::Integer(42), s()));
    let (doc, val_var) = generator.generate_field_assignment_open(&expr).unwrap();
    let output = doc.to_pretty_string();
    assert_eq!(
        output,
        "let _Val1 = 42 in let State1 = call 'maps':'put'('count', _Val1, State) in "
    );
    assert_eq!(val_var, "_Val1");
}

/// `generate_field_assignment_open`'s `ValueType` arm: a value-type field
/// write reaching it (from inside a loop/conditional/block body) threads
/// through `Self`, not the actor `State`/`StateAcc` map — a variable that
/// does not exist in a value-type method.
#[test]
fn test_field_assignment_open_value_type_threads_self() {
    let mut generator = CoreErlangGenerator::new("test");
    generator.context = crate::core_erlang::CodeGenContext::ValueType;
    let expr = self_field_assignment_expr("count", Expression::Literal(Literal::Integer(42), s()));
    let (doc, val_var) = generator.generate_field_assignment_open(&expr).unwrap();
    let output = doc.to_pretty_string();
    assert_eq!(
        output,
        "let _Val1 = 42 in let Self1 = call 'maps':'put'('count', _Val1, Self) in "
    );
    assert_eq!(val_var, "_Val1");
    assert!(
        !output.contains("State"),
        "value-type field write must never reference State. Got: {output}"
    );
}

/// A class-var write directly inside a Letrec loop body that threads
/// `ClassVars` through the loop's own recursive tail call —
/// `generate_field_assignment_open`'s one pre-existing `ClassVar` arm.
#[test]
fn test_field_assignment_open_class_var_threads_class_vars_with_shadow_write() {
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_in_class_method(true);
    generator.class_var_names_mut().insert("total".to_string());
    generator.loop_mode.loop_threads_class_vars = true;
    let expr = self_field_assignment_expr("total", Expression::Literal(Literal::Integer(42), s()));
    let (doc, val_var) = generator.generate_field_assignment_open(&expr).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("let ClassVars1 = call 'maps':'put'('total', _Val1, ClassVars) in"),
        "class-var write should thread ClassVars. Got: {output}"
    );
    assert!(
        output.contains("'$bt_class_vars_shadow'"),
        "class-var write should carry ADR 0110's shadow write. Got: {output}"
    );
    assert_eq!(val_var, "_Val1");
}
