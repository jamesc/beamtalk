// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use super::is_class_auto_export_selector;
use crate::core_erlang::CoreErlangGenerator;
use crate::core_erlang::expr_shape::is_character_typed_receiver;
use crate::core_erlang::gen_server::SpawnVariant;
use beamtalk_core::ast::{
    Block, ClassDefinition, Expression, ExpressionStatement, Identifier, KeywordPart, Literal,
    MessageSelector, MethodDefinition, TypeAnnotation,
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
/// self-send and must short-circuit to a direct call; `superclass` takes the
/// virtual self-send path (whose class-object protocol fallthrough answers a
/// class object, BT-4252) because its raw export returns a bare atom, so it
/// must NOT be classified as an auto-export here.
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

    // `superclass` takes the virtual self-send path (its raw export is
    // unwrapped and identity-broken), not here.
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
            unqualified.contains("}, 'beamtalk')"),
            "File open:do: must carry the precomputed 'beamtalk' mode (BT-4153). \
             Got: {unqualified}"
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
        .generate_actor_spawn_qualified("Counter", None, None, SpawnVariant::Plain)
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
fn test_generate_actor_spawn_monitored_non_repl() {
    // BT-4104: `Counter spawnMonitored` calls the module's own
    // `spawn_monitored/0` in the sender's process.
    let mut generator = CoreErlangGenerator::new("test");
    let doc = generator
        .generate_actor_spawn_qualified("Counter", None, None, SpawnVariant::Monitored)
        .unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains(":'spawn_monitored'()"),
        "spawnMonitored should call spawn_monitored/0. Got: {output}"
    );
    assert!(
        !output.contains("'spawn'("),
        "spawnMonitored must not call spawn/0. Got: {output}"
    );
}

#[test]
fn test_generate_actor_spawn_monitored_repl_registers_pair_actor() {
    // BT-4104: in the REPL the registry gets the pid of the pair's actor
    // (element 4 of element 1), and the whole pair is answered.
    let mut generator = CoreErlangGenerator::new("test");
    generator.bind_var("__bindings__", "Bindings");
    let doc = generator
        .generate_actor_spawn_qualified("Counter", None, None, SpawnVariant::Monitored)
        .unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.matches(":'spawn_monitored'()").count() == 2,
        "both REPL arms should call spawn_monitored/0. Got: {output}"
    );
    assert!(
        output.contains(
            "let SpawnPid = call 'erlang':'element'(4, call 'erlang':'element'(1, SpawnResult)) in"
        ),
        "REPL registration must take the pid from the pair's actor. Got: {output}"
    );
    assert!(
        output.contains("register_spawned"),
        "REPL variant must register the actor. Got: {output}"
    );
}

#[test]
fn test_generate_actor_spawn_repl_registers_plain_result() {
    let mut generator = CoreErlangGenerator::new("test");
    generator.bind_var("__bindings__", "Bindings");
    let doc = generator
        .generate_actor_spawn_qualified("Counter", None, None, SpawnVariant::Plain)
        .unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("let SpawnPid = call 'erlang':'element'(4, SpawnResult) in"),
        "plain REPL spawn takes the pid from the actor itself. Got: {output}"
    );
}

#[test]
fn test_spawn_variant_of_selector() {
    assert_eq!(
        SpawnVariant::of_selector("spawn", 0),
        Some(SpawnVariant::Plain)
    );
    assert_eq!(
        SpawnVariant::of_selector("spawnWith:", 1),
        Some(SpawnVariant::Plain)
    );
    assert_eq!(
        SpawnVariant::of_selector("spawnMonitored", 0),
        Some(SpawnVariant::Monitored)
    );
    assert_eq!(
        SpawnVariant::of_selector("spawnMonitoredWith:", 1),
        Some(SpawnVariant::Monitored)
    );
    assert_eq!(SpawnVariant::of_selector("spawnAs:", 1), None);
    assert_eq!(SpawnVariant::of_selector("spawnMonitored", 1), None);
}

#[test]
fn test_supervisor_spawn_selector_includes_monitored() {
    // BT-4104: a supervisor receiver of the monitored selectors is routed to
    // `beamtalk_supervisor:class_start/4` (which raises unsupported), never
    // to a `spawn_monitored` export supervisor modules do not have.
    assert!(super::is_supervisor_spawn_selector("spawnMonitored", 0));
    assert!(super::is_supervisor_spawn_selector(
        "spawnMonitoredWith:",
        1
    ));
    assert!(!super::is_supervisor_spawn_selector("spawnMonitored", 1));
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
fn test_generate_cast_send_actor_self_routes_to_mailbox() {
    let mut generator = CoreErlangGenerator::new("test");
    generator.context = crate::core_erlang::CodeGenContext::Actor;
    let receiver = Expression::Identifier(Identifier::new("self", s()));
    let selector = MessageSelector::Unary("doIt".into());
    let doc = generator
        .generate_cast_send(&receiver, &selector, &[])
        .unwrap();
    let output = doc.to_pretty_string();
    // BT-3998: a self-cast is a real cast to the mailbox (lands after the
    // current message), never an inline dispatch whose State is dropped.
    assert!(
        output.contains("'beamtalk_message_dispatch':'cast'(") && output.contains("'doIt'"),
        "actor self cast should route through beamtalk_message_dispatch:cast. Got: {output}"
    );
    assert!(
        !output.contains("safe_dispatch"),
        "actor self cast must not dispatch inline. Got: {output}"
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

    // BT-4161: a cast (`!`) always evaluates to `'ok'`, so even a
    // Character-typed shape under a cast is not Character-typed.
    let mut factory_cast = character_value_call(Expression::Literal(Literal::Integer(65), s()));
    let Expression::MessageSend { is_cast, .. } = &mut factory_cast else {
        panic!("character_value_call must build a MessageSend");
    };
    *is_cast = true;
    assert!(!is_character_typed_receiver(&factory_cast));
}

/// Builds `Character value: <arg>`.
fn character_value_call(arg: Expression) -> Expression {
    Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Character", s()),
            package: None,
            span: s(),
        }),
        selector: MessageSelector::Keyword(vec![KeywordPart::new("value:", s())]),
        arguments: vec![arg],
        is_cast: false,
        span: s(),
    }
}

/// Builds `[body]` — a zero-parameter block literal answering `body`.
fn block_answering(body: Expression) -> Expression {
    Expression::Block(Block::new(
        vec![],
        vec![ExpressionStatement::bare(body)],
        s(),
    ))
}

/// Builds `<condition> ifTrue: <true_branch> ifFalse: <false_branch>`.
fn if_true_if_false(condition: Expression, tb: Expression, fb: Expression) -> Expression {
    Expression::MessageSend {
        receiver: Box::new(condition),
        selector: MessageSelector::Keyword(vec![
            KeywordPart::new("ifTrue:", s()),
            KeywordPart::new("ifFalse:", s()),
        ]),
        arguments: vec![tb, fb],
        is_cast: false,
        span: s(),
    }
}

fn int_lit(n: i64) -> Expression {
    Expression::Literal(Literal::Integer(n), s())
}

fn bool_ident(name: &str) -> Expression {
    Expression::Identifier(Identifier::new(name, s()))
}

/// BT-4161: with a literal-`true` condition, only the taken (`ifTrue:`)
/// branch decides — the untaken branch is never evaluated
/// (`True>>ifTrue:ifFalse:` answers `trueBlock value`), so its type is
/// irrelevant, and a Character-typed `ifFalse:` branch must not rescue an
/// Integer-typed taken branch.
#[test]
fn if_true_if_false_literal_true_taken_branch_decides() {
    let char_branch = block_answering(character_value_call(int_lit(54)));
    let int_branch = block_answering(int_lit(0));
    assert!(is_character_typed_receiver(&if_true_if_false(
        bool_ident("true"),
        char_branch.clone(),
        int_branch.clone(),
    )));
    assert!(!is_character_typed_receiver(&if_true_if_false(
        bool_ident("true"),
        int_branch,
        char_branch,
    )));
}

/// BT-4161: mirror of the above for a literal-`false` condition
/// (`False>>ifTrue:ifFalse:` answers `falseBlock value`).
#[test]
fn if_true_if_false_literal_false_taken_branch_decides() {
    let char_branch = block_answering(character_value_call(int_lit(54)));
    let int_branch = block_answering(int_lit(0));
    assert!(is_character_typed_receiver(&if_true_if_false(
        bool_ident("false"),
        int_branch.clone(),
        char_branch.clone(),
    )));
    assert!(!is_character_typed_receiver(&if_true_if_false(
        bool_ident("false"),
        char_branch,
        int_branch,
    )));
}

/// BT-4161: with a non-constant condition the taken branch is unknown
/// statically, so both branches must be Character-typed — the answer is a
/// branch value either way. A mixed pair stays Integer-dispatched (current
/// behavior), since routing it to Character would miscompile the
/// Integer-typed path.
#[test]
fn if_true_if_false_non_constant_condition_requires_both_branches() {
    let cond = Expression::Identifier(Identifier::new("flag", s()));
    assert!(is_character_typed_receiver(&if_true_if_false(
        cond.clone(),
        block_answering(Expression::Literal(Literal::Character('A'), s())),
        block_answering(character_value_call(int_lit(66))),
    )));
    assert!(!is_character_typed_receiver(&if_true_if_false(
        cond.clone(),
        block_answering(Expression::Literal(Literal::Character('A'), s())),
        block_answering(int_lit(0)),
    )));
    assert!(!is_character_typed_receiver(&if_true_if_false(
        cond,
        block_answering(int_lit(0)),
        block_answering(Expression::Literal(Literal::Character('A'), s())),
    )));
}

/// BT-4161: transparency recurses — a branch body that is itself a
/// Character-typed conditional (or chain) counts, and only the LAST body
/// statement decides (earlier statements' values are discarded).
#[test]
fn if_true_if_false_recurses_through_nested_branches() {
    // Nested conditional as the taken branch's answer.
    let nested = if_true_if_false(
        bool_ident("false"),
        block_answering(int_lit(1)),
        block_answering(Expression::Literal(Literal::Character('B'), s())),
    );
    assert!(is_character_typed_receiver(&if_true_if_false(
        bool_ident("true"),
        block_answering(nested),
        block_answering(int_lit(0)),
    )));

    // Multi-statement branch: the last statement decides.
    let multi = Expression::Block(Block::new(
        vec![],
        vec![
            ExpressionStatement::bare(int_lit(99)),
            ExpressionStatement::bare(Expression::Literal(Literal::Character('C'), s())),
        ],
        s(),
    ));
    assert!(is_character_typed_receiver(&if_true_if_false(
        bool_ident("true"),
        multi,
        block_answering(int_lit(0)),
    )));

    // Parenthesized condition and branch see through, like every other arm:
    // the operands through `ast::normalize`, which runs before codegen
    // (BT-4232), the whole send through the recognizer's own peel.
    let mut parenthesized = Expression::Parenthesized {
        expression: Box::new(if_true_if_false(
            Expression::Parenthesized {
                expression: Box::new(bool_ident("true")),
                span: s(),
            },
            Expression::Parenthesized {
                expression: Box::new(block_answering(character_value_call(int_lit(54)))),
                span: s(),
            },
            block_answering(int_lit(0)),
        )),
        span: s(),
    };
    beamtalk_core::ast::normalize::peel_operand_parens_expr_recording(
        &mut parenthesized,
        &mut beamtalk_core::ast::normalize::PeeledParens::default(),
    );
    assert!(is_character_typed_receiver(&parenthesized));
}

/// BT-4161: shapes that must NOT be transparent. An empty block answers
/// `nil`; a non-block branch argument cannot be inspected (whatever its
/// `value` answers is dynamically typed); and a cast (`!`) always evaluates
/// to `'ok'`. (Single-branch forms are covered by the BT-4181 tests below.)
#[test]
fn if_true_if_false_rejects_untyped_shapes() {
    // Empty taken branch answers nil.
    let empty = Expression::Block(Block::new(vec![], vec![], s()));
    assert!(!is_character_typed_receiver(&if_true_if_false(
        bool_ident("true"),
        empty,
        block_answering(int_lit(0)),
    )));

    // Non-block branch argument: dynamically typed.
    assert!(!is_character_typed_receiver(&if_true_if_false(
        bool_ident("true"),
        Expression::Identifier(Identifier::new("someBlock", s())),
        block_answering(int_lit(0)),
    )));

    // A cast evaluates to 'ok', never a Character.
    let mut cast = if_true_if_false(
        bool_ident("true"),
        block_answering(character_value_call(int_lit(54))),
        block_answering(int_lit(0)),
    );
    let Expression::MessageSend { is_cast, .. } = &mut cast else {
        panic!("if_true_if_false must build a MessageSend");
    };
    *is_cast = true;
    assert!(!is_character_typed_receiver(&cast));
}

/// Builds `<receiver> <keyword> <block>` — a single-block boolean send
/// (`ifTrue:`, `ifFalse:`, `and:`, `or:`).
fn single_branch(receiver: Expression, keyword: &str, block: Expression) -> Expression {
    Expression::MessageSend {
        receiver: Box::new(receiver),
        selector: MessageSelector::Keyword(vec![KeywordPart::new(keyword, s())]),
        arguments: vec![block],
        is_cast: false,
        span: s(),
    }
}

/// BT-4181: a single-block boolean send whose literal receiver takes the
/// block — `true ifTrue:`, `false ifFalse:`, `true and:`, `false or:` —
/// answers the block's value, so a Character-answering block makes the
/// whole send Character-typed (including through parens and nesting).
#[test]
fn single_branch_constant_taken_is_character_typed() {
    let char_branch = || block_answering(character_value_call(int_lit(54)));
    for (receiver, keyword) in [
        ("true", "ifTrue:"),
        ("false", "ifFalse:"),
        ("true", "and:"),
        ("false", "or:"),
    ] {
        assert!(
            is_character_typed_receiver(&single_branch(
                bool_ident(receiver),
                keyword,
                char_branch(),
            )),
            "`{receiver} {keyword} [Character value: 54]` must be Character-typed"
        );

        // Parenthesized receiver, block and whole send all see through: the
        // operands through `ast::normalize`, which runs before codegen
        // (BT-4232), the whole send through the recognizer's own peel.
        let mut parenthesized = Expression::Parenthesized {
            expression: Box::new(single_branch(
                Expression::Parenthesized {
                    expression: Box::new(bool_ident(receiver)),
                    span: s(),
                },
                keyword,
                Expression::Parenthesized {
                    expression: Box::new(block_answering(Expression::Literal(
                        Literal::Character('A'),
                        s(),
                    ))),
                    span: s(),
                },
            )),
            span: s(),
        };
        beamtalk_core::ast::normalize::peel_operand_parens_expr_recording(
            &mut parenthesized,
            &mut beamtalk_core::ast::normalize::PeeledParens::default(),
        );
        assert!(
            is_character_typed_receiver(&parenthesized),
            "parenthesized `{receiver} {keyword} [$A]` must be Character-typed"
        );
    }

    // Composes with the other arms: a nested single-branch send as the
    // block's answer, and `uppercase` applied to one.
    let nested = single_branch(
        bool_ident("true"),
        "and:",
        block_answering(single_branch(
            bool_ident("false"),
            "ifFalse:",
            block_answering(Expression::Literal(Literal::Character('B'), s())),
        )),
    );
    assert!(is_character_typed_receiver(&nested));
    let uppercased = Expression::MessageSend {
        receiver: Box::new(Expression::Parenthesized {
            expression: Box::new(nested),
            span: s(),
        }),
        selector: MessageSelector::Unary("uppercase".into()),
        arguments: vec![],
        is_cast: false,
        span: s(),
    };
    assert!(is_character_typed_receiver(&uppercased));
}

/// BT-4181: everything else stays conservatively Integer-dispatched. A
/// constant receiver that skips the block answers a Boolean
/// (`False>>ifTrue:`/`True>>ifFalse:` answer `self`, `false and:` answers
/// `false`, `true or:` answers `true`) — never `nil`, never a Character; a
/// non-constant receiver may take either path; and a non-Character block,
/// an empty block (`nil`), a non-block argument, a cast, or another
/// single-keyword selector never qualify.
#[test]
fn single_branch_untaken_or_non_constant_is_not_character_typed() {
    let char_branch = || block_answering(Expression::Literal(Literal::Character('A'), s()));

    // Constant-untaken.
    for (receiver, keyword) in [
        ("false", "ifTrue:"),
        ("true", "ifFalse:"),
        ("false", "and:"),
        ("true", "or:"),
    ] {
        assert!(
            !is_character_typed_receiver(&single_branch(
                bool_ident(receiver),
                keyword,
                char_branch(),
            )),
            "`{receiver} {keyword} [$A]` answers a Boolean and must not be Character-typed"
        );
    }

    // Non-constant receiver.
    for keyword in ["ifTrue:", "ifFalse:", "and:", "or:"] {
        assert!(
            !is_character_typed_receiver(&single_branch(
                Expression::Identifier(Identifier::new("flag", s())),
                keyword,
                char_branch(),
            )),
            "`flag {keyword} [$A]` has an unknown path and must not be Character-typed"
        );
    }

    // Taken, but the block does not answer a Character.
    assert!(!is_character_typed_receiver(&single_branch(
        bool_ident("true"),
        "ifTrue:",
        block_answering(int_lit(65)),
    )));
    assert!(!is_character_typed_receiver(&single_branch(
        bool_ident("true"),
        "and:",
        Expression::Block(Block::new(vec![], vec![], s())),
    )));
    assert!(!is_character_typed_receiver(&single_branch(
        bool_ident("false"),
        "or:",
        Expression::Identifier(Identifier::new("someBlock", s())),
    )));

    // A cast evaluates to 'ok'.
    let mut cast = single_branch(bool_ident("true"), "ifTrue:", char_branch());
    let Expression::MessageSend { is_cast, .. } = &mut cast else {
        panic!("single_branch must build a MessageSend");
    };
    *is_cast = true;
    assert!(!is_character_typed_receiver(&cast));

    // Some other single-keyword selector on a literal Boolean.
    assert!(!is_character_typed_receiver(&single_branch(
        bool_ident("true"),
        "xor:",
        char_branch(),
    )));
}

/// BT-4181: the issue's surface expressions parse to shapes the recognizer
/// accepts (and the untaken mirror to one it rejects) — guards against
/// precedence surprises between the surface syntax and the hand-built ASTs.
#[test]
fn single_branch_issue_expressions_parse_to_expected_shapes() {
    for (source, expected) in [
        ("(true ifTrue: [$A]) asString", true),
        ("(false ifFalse: [$A]) asString", true),
        ("(true and: [$A]) asString", true),
        ("(false or: [$A]) asString", true),
        ("(false ifTrue: [$A]) asString", false),
        ("(true ifFalse: [$A]) asString", false),
        ("(false and: [$A]) asString", false),
        ("(true or: [$A]) asString", false),
    ] {
        let (module, diags) = parse(lex_with_eof(source));
        assert!(
            diags.iter().all(|d| d.severity != Severity::Error),
            "{source} must parse without errors: {diags:?}"
        );
        let Expression::MessageSend { receiver, .. } = &module.expressions[0].expression else {
            panic!("expected an outer asString send for {source}");
        };
        assert_eq!(
            is_character_typed_receiver(receiver),
            expected,
            "recognizer verdict for the receiver of {source}"
        );
    }
}

/// BT-4161: the issue's exact expression parses to a shape the recognizer
/// accepts — guards against precedence/parenthesization surprises between
/// the surface syntax and the hand-built ASTs above.
#[test]
fn if_true_if_false_issue_expression_parses_to_character_typed_shape() {
    let source = "(true ifTrue: [Character value: (48 + 6)] ifFalse: [0]) asString";
    let (module, diags) = parse(lex_with_eof(source));
    assert!(
        diags.iter().all(|d| d.severity != Severity::Error),
        "snippet must parse without errors: {diags:?}"
    );
    let Expression::MessageSend { receiver, .. } = &module.expressions[0].expression else {
        panic!(
            "expected an outer asString send, got: {:?}",
            module.expressions[0].expression
        );
    };
    assert!(is_character_typed_receiver(receiver));
}

/// BT-4161: enforces the invariant the `ifTrue:ifFalse:` transparency arm
/// of `is_character_typed_receiver` depends on — that
/// `True>>ifTrue:ifFalse:` answers `trueBlock value` and
/// `False>>ifTrue:ifFalse:` answers `falseBlock value` (single-statement
/// bodies evaluating exactly the taken branch). Parses the real
/// `true.bt`/`false.bt` off disk and fails loudly if a future edit changes
/// a branch body without updating the recognizer — silent drift here would
/// misroute a genuinely Integer-typed answer through Character dispatch.
#[test]
fn true_false_if_true_if_false_bodies_match_codegen_recognizer() {
    for (file, class_name, taken_param) in [
        ("stdlib/src/true.bt", "True", "trueBlock"),
        ("stdlib/src/false.bt", "False", "falseBlock"),
    ] {
        let Some(method) = stdlib_instance_method(file, class_name, "ifTrue:ifFalse:") else {
            return;
        };
        assert_body_answers_param_value(&method, class_name, taken_param);
    }
}

/// Parses `stdlib/<file>` off disk and answers a clone of
/// `<class_name>>><selector>` (instance side), or `None` — after logging a
/// skip — when the file is absent from this checkout. Panics if the file
/// fails to parse or lacks the class/method: the sync tests below must fail
/// loudly on drift.
fn stdlib_instance_method(
    file: &str,
    class_name: &str,
    selector: &str,
) -> Option<MethodDefinition> {
    let class = stdlib_class(file, class_name)?;
    let method = class
        .methods
        .iter()
        .find(|m| m.selector.name() == selector)
        .unwrap_or_else(|| panic!("{file} must define {class_name}>>{selector}"));
    Some(method.clone())
}

/// Parses `stdlib/<file>` off disk and answers a clone of its `<class_name>`
/// class, or `None` — after logging a skip — when the file is absent from
/// this checkout. Panics if the file fails to parse or lacks the class.
fn stdlib_class(file: &str, class_name: &str) -> Option<ClassDefinition> {
    let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crates/")
        .parent()
        .expect("repo root")
        .to_path_buf();
    let path = repo_root.join(file);
    let Ok(source) = std::fs::read_to_string(&path) else {
        eprintln!("skipping: {} not present in this checkout", path.display());
        return None;
    };

    let (module, diags) = parse(lex_with_eof(&source));
    assert!(
        diags.iter().all(|d| d.severity != Severity::Error),
        "{file} must parse without errors: {diags:?}"
    );
    let class = module
        .classes
        .iter()
        .find(|c| c.name.name == class_name)
        .unwrap_or_else(|| panic!("{file} must define the {class_name} class"));
    Some(class.clone())
}

/// Asserts `method`'s body is the single statement `<param> value` — it
/// answers exactly the block parameter's value. Shared by the
/// `is_character_typed_receiver` sync tests (BT-4161, BT-4181).
fn assert_body_answers_param_value(method: &MethodDefinition, class_name: &str, param: &str) {
    let selector = method.selector.name();
    let [stmt] = method.body.as_slice() else {
        panic!(
            "{class_name}>>{selector} must be a single statement (the recognizer \
             assumes it answers exactly the taken block's value) — update \
             is_character_typed_receiver to match"
        );
    };
    let Expression::MessageSend {
        receiver,
        selector: sent,
        arguments,
        is_cast: false,
        ..
    } = &stmt.expression
    else {
        panic!(
            "{class_name}>>{selector} must evaluate a block — update \
             is_character_typed_receiver to match"
        );
    };
    assert!(
        matches!(receiver.as_ref(), Expression::Identifier(id) if id.name == param)
            && matches!(sent, MessageSelector::Unary(name) if name == "value")
            && arguments.is_empty(),
        "{class_name}>>{selector} must answer `{param} value` (the recognizer \
         routes on the taken block's value) — update is_character_typed_receiver \
         to match"
    );
}

/// BT-4181: enforces the stdlib bodies the single-branch arm of
/// `is_character_typed_receiver` (`single_branch_result_is_character_typed`)
/// depends on. On the taken path, `True>>ifTrue:` and `False>>ifFalse:`
/// must answer `<block> value`, and `Boolean>>and:`/`or:` must delegate to
/// `self ifTrue:ifFalse:` with the argument block in the taken position
/// (`and:` → `self ifTrue: aBlock ifFalse: [false]`, `or:` → `self ifTrue:
/// [true] ifFalse: aBlock`), so with the `True`/`False>>ifTrue:ifFalse:`
/// rule pinned above `true and: B` / `false or: B` answer `B value`. On the
/// untaken path, `True>>ifFalse:` and `False>>ifTrue:` must answer `self` —
/// a Boolean, never a Character — which is why the recognizer only accepts
/// a constant-taken receiver; if that ever changes, revisit the arm.
#[test]
fn single_branch_boolean_bodies_match_codegen_recognizer() {
    // Taken path: `<block> value`.
    for (file, class_name, selector, param) in [
        ("stdlib/src/true.bt", "True", "ifTrue:", "trueBlock"),
        ("stdlib/src/false.bt", "False", "ifFalse:", "falseBlock"),
    ] {
        let Some(method) = stdlib_instance_method(file, class_name, selector) else {
            return;
        };
        assert_body_answers_param_value(&method, class_name, param);
    }

    // Untaken path: `self`.
    for (file, class_name, selector) in [
        ("stdlib/src/true.bt", "True", "ifFalse:"),
        ("stdlib/src/false.bt", "False", "ifTrue:"),
    ] {
        let Some(method) = stdlib_instance_method(file, class_name, selector) else {
            return;
        };
        assert!(
            matches!(
                method.body.as_slice(),
                [stmt] if matches!(&stmt.expression, Expression::Identifier(id) if id.name == "self")
            ),
            "{class_name}>>{selector} must answer `self` on the untaken path — \
             update is_character_typed_receiver to match"
        );
    }

    // BT-4231: `true and:` / `false or:` resolve to the `Boolean` bodies
    // pinned below only while `True`/`False` do not override them. An
    // override answering a Boolean (a fast path such as
    // `and: aBlock => aBlock value == true`) would otherwise leave this test
    // green while the recognizer routes a Boolean through Character dispatch.
    for (file, class_name, selector) in [
        ("stdlib/src/true.bt", "True", "and:"),
        ("stdlib/src/false.bt", "False", "or:"),
    ] {
        let Some(class) = stdlib_class(file, class_name) else {
            return;
        };
        assert!(
            !class.methods.iter().any(|m| m.selector.name() == selector),
            "{class_name} must not override {selector}: the recognizer assumes it \
             inherits Boolean>>{selector} — pin the override's body here and \
             update is_character_typed_receiver to match"
        );
    }

    // `and:`/`or:` delegate to `self ifTrue:ifFalse:` with the argument
    // block as the taken arm (index 0 for `and:`, 1 for `or:`).
    for (selector, taken_index) in [("and:", 0usize), ("or:", 1usize)] {
        let Some(method) = stdlib_instance_method("stdlib/src/boolean.bt", "Boolean", selector)
        else {
            return;
        };
        let param = method.parameters.first().map(|p| p.name.name.to_string());
        let ok = match method.body.as_slice() {
            [stmt] => match &stmt.expression {
                Expression::MessageSend {
                    receiver,
                    selector: sent,
                    arguments,
                    is_cast: false,
                    ..
                } => {
                    matches!(receiver.as_ref(), Expression::Identifier(id) if id.name == "self")
                        && sent.name() == "ifTrue:ifFalse:"
                        && matches!(
                            arguments.get(taken_index),
                            Some(Expression::Identifier(id)) if Some(id.name.to_string()) == param
                        )
                }
                _ => false,
            },
            _ => false,
        };
        assert!(
            ok,
            "Boolean>>{selector} must be `self ifTrue:ifFalse:` with its block \
             argument as the {} arm — update is_character_typed_receiver to match",
            if taken_index == 0 {
                "ifTrue:"
            } else {
                "ifFalse:"
            }
        );
    }
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

/// BT-4161: codegen for `(true ifTrue: [Character value: 54] ifFalse: [0])
/// asString` must emit a direct call to `bt@stdlib@character:dispatch/3`.
/// The conditional's taken branch is statically Character-typed, so without
/// transparency the send falls through to generic runtime dispatch and is
/// misrouted to `Integer>>asString`, producing `"54"` instead of `"6"`.
#[test]
fn conditional_character_receiver_dispatches_to_character_module() {
    let mut generator = CoreErlangGenerator::new("test");
    let receiver = Expression::Parenthesized {
        expression: Box::new(if_true_if_false(
            bool_ident("true"),
            block_answering(character_value_call(int_lit(54))),
            block_answering(int_lit(0)),
        )),
        span: s(),
    };
    let selector = MessageSelector::Unary("asString".into());
    let output = generator
        .generate_message_send(&receiver, &selector, &[])
        .unwrap()
        .to_pretty_string();

    assert!(
        output.contains("'bt@stdlib@character':'dispatch'"),
        "expected direct Character dispatch for the outer send, got: {output}"
    );
    // No `beamtalk_message_dispatch` absence check here (unlike the factory
    // test above): the INNER `ifTrue:ifFalse:` send itself still compiles
    // through generic runtime dispatch — only the OUTER send's routing
    // changes, from Integer-misrouted to Character-direct.
}

/// BT-4181: codegen for a constant-taken single-block boolean send used as a
/// receiver — `(true ifTrue: [$A]) asString`, `(false ifFalse: [$A])
/// asString`, `(true and: [$A]) asString`, `(false or: [$A]) asString` —
/// must emit a direct call to `bt@stdlib@character:dispatch/3` for the
/// outer send, while the constant-untaken mirror (`(false ifTrue: [$A])
/// asString`, which answers `false`) must not.
#[test]
fn single_branch_character_receiver_dispatches_to_character_module() {
    let selector = MessageSelector::Unary("asString".into());
    let char_block = || block_answering(Expression::Literal(Literal::Character('A'), s()));
    for (receiver, keyword, expect_character) in [
        ("true", "ifTrue:", true),
        ("false", "ifFalse:", true),
        ("true", "and:", true),
        ("false", "or:", true),
        ("false", "ifTrue:", false),
        ("true", "or:", false),
    ] {
        let mut generator = CoreErlangGenerator::new("test");
        let send = Expression::Parenthesized {
            expression: Box::new(single_branch(bool_ident(receiver), keyword, char_block())),
            span: s(),
        };
        let output = generator
            .generate_message_send(&send, &selector, &[])
            .unwrap()
            .to_pretty_string();
        assert_eq!(
            output.contains("'bt@stdlib@character':'dispatch'"),
            expect_character,
            "`({receiver} {keyword} [$A]) asString`: expected Character dispatch = \
             {expect_character}, got: {output}"
        );
    }
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
        "let _Val@1 = 42 in let State1 = call 'maps':'put'('count', _Val@1, State) in "
    );
    assert_eq!(val_var, "_Val@1");
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
        "let _Val@1 = 42 in let Self1 = call 'maps':'put'('count', _Val@1, Self) in "
    );
    assert_eq!(val_var, "_Val@1");
    assert!(
        !output.contains("State"),
        "value-type field write must never reference State. Got: {output}"
    );
}

/// ADR 0130 §2: a class-variable write reaching `generate_field_assignment_open`
/// (a loop, conditional or block body) is an in-place `put` bound to a value
/// variable, never a `ClassVars` rebind.
#[test]
fn test_field_assignment_open_class_var_is_an_in_place_put() {
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_in_class_method(true);
    generator.class_var_names_mut().insert("total".to_string());
    let expr = self_field_assignment_expr("total", Expression::Literal(Literal::Integer(42), s()));
    let (doc, val_var) = generator.generate_field_assignment_open(&expr).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.starts_with(&format!("let {val_var} = ")) && output.ends_with(" in "),
        "an open write binds its value for the caller's continuation. Got: {output}"
    );
    assert!(
        output.contains("call 'maps':'put'('total', ")
            && output.contains("call 'erlang':'put'({'$bt_class_vars', "),
        "class-var write should be an inlined in-place put. Got: {output}"
    );
    assert!(
        !output.contains("ClassVars") && !output.contains("'$bt_class_vars_shadow'"),
        "nothing is threaded and there is no shadow write. Got: {output}"
    );
}

// ─── BT-3811: hybrid field rebind is lowered in the caller's frame ────

/// A generator inside a hybrid loop body (`in_loop_body`, so
/// `current_frame()` is a non-`ROOT` frame) whose `m` and `n` fields were
/// extracted to fun parameters.
fn hybrid_loop_body_generator() -> (
    CoreErlangGenerator,
    crate::core_erlang::threaded_ir::FrameId,
) {
    let mut generator = CoreErlangGenerator::new("test");
    generator.loop_mode.in_hybrid_loop = true;
    generator
        .loop_mode
        .hybrid_mutated_fields
        .extend(["m".to_string(), "n".to_string()]);
    generator.in_loop_body = true;
    generator.active_branch_frame = 7;
    let frame = generator.current_frame();
    assert_ne!(frame, crate::core_erlang::threaded_ir::FrameId::ROOT);
    (generator, frame)
}

/// `self.m := (self.n := 1)`: the nested write is an ahead prelude. It is
/// spliced as a real `Bind` in the caller's frame (not flattened to an opaque
/// `Statement` under a hardcoded `FrameId::ROOT`), so the verifier sees it.
#[test]
fn test_hybrid_field_rebind_splices_ahead_prelude_in_the_callers_frame() {
    use crate::core_erlang::threaded_ir::{ThreadedStmt, verify_bind_frames};
    let (mut generator, frame) = hybrid_loop_body_generator();
    let inner = self_field_assignment_expr("n", Expression::Literal(Literal::Integer(1), s()));
    let outer = self_field_assignment_expr("m", inner);
    let Expression::Assignment { value, .. } = &outer else {
        unreachable!()
    };
    let mut stmts = Vec::new();
    generator
        .lower_hybrid_field_rebind("m", value, frame, s(), &mut stmts)
        .unwrap();
    let bind_frames: Vec<_> = stmts
        .iter()
        .filter_map(|stmt| match stmt {
            ThreadedStmt::Bind { target, .. } => Some(target.frame),
            _ => None,
        })
        .collect();
    assert_eq!(
        bind_frames,
        vec![frame, frame],
        "both the nested `n` rebind and the outer `m` rebind are Binds in the caller's frame"
    );
    assert_eq!(verify_bind_frames(&stmts, frame), Vec::new());
}

/// The `Document` API renders the same lowering: both field params are
/// rebound, the nested one first.
#[test]
fn test_field_assignment_open_hybrid_renders_nested_rebind_before_outer() {
    let (mut generator, _) = hybrid_loop_body_generator();
    let inner = self_field_assignment_expr("n", Expression::Literal(Literal::Integer(1), s()));
    let outer = self_field_assignment_expr("m", inner);
    let (doc, _val) = generator.generate_field_assignment_open(&outer).unwrap();
    let output = doc.to_pretty_string();
    let n_rebind = output.find("NField").expect("nested rebind rendered");
    let m_rebind = output.find("MField").expect("outer rebind rendered");
    assert!(n_rebind < m_rebind, "nested write first. Got: {output}");
}

// ─── BT-4005: an own-class reference is the dispatching self ────

/// `binary_send_dispatches_to_self` is the one predicate that sends a binary
/// selector down `generate_message_send`'s `HANDLERS` path: bare `self` in an
/// actor instance method or a class method, and an unqualified own-class
/// reference in a class method — never another class, a package-qualified
/// reference, an own-class reference outside a class method, or a
/// structurally lowered operator.
#[test]
fn binary_send_dispatches_to_self_admits_an_own_class_reference() {
    use crate::core_erlang::CodeGenContext;
    use crate::core_erlang::expr_shape::{ShapeCtx, binary_send_dispatches_to_self};
    use std::collections::HashSet;

    let empty = HashSet::new();
    let ctx = |context, in_class_method| ShapeCtx {
        context,
        in_class_method,
        in_cell_closure: false,
        class_var_names: &empty,
        class_method_selectors: &empty,
        class_name: "C".to_string(),
        self_var_bound: false,
    };
    let class_ref = |name: &str, package: Option<&str>| Expression::ClassReference {
        name: Identifier::new(name, s()),
        package: package.map(|p| Identifier::new(p, s())),
        span: s(),
    };
    let self_id = Expression::Identifier(Identifier::new("self", s()));
    let own = class_ref("C", None);

    let class_side = ctx(CodeGenContext::ValueType, true);
    assert!(binary_send_dispatches_to_self(&class_side, &own, "+"));
    assert!(binary_send_dispatches_to_self(&class_side, &own, "<"));
    assert!(binary_send_dispatches_to_self(&class_side, &self_id, "+"));
    assert!(!binary_send_dispatches_to_self(&class_side, &own, "=="));
    assert!(!binary_send_dispatches_to_self(&class_side, &own, ">>"));
    assert!(!binary_send_dispatches_to_self(
        &class_side,
        &class_ref("D", None),
        "+"
    ));
    assert!(!binary_send_dispatches_to_self(
        &class_side,
        &class_ref("C", Some("pkg")),
        "+"
    ));

    let actor_instance = ctx(CodeGenContext::Actor, false);
    assert!(binary_send_dispatches_to_self(
        &actor_instance,
        &self_id,
        "+"
    ));
    assert!(!binary_send_dispatches_to_self(&actor_instance, &own, "+"));

    let value_instance = ctx(CodeGenContext::ValueType, false);
    assert!(!binary_send_dispatches_to_self(
        &value_instance,
        &self_id,
        "+"
    ));
    assert!(!binary_send_dispatches_to_self(&value_instance, &own, "+"));
}
