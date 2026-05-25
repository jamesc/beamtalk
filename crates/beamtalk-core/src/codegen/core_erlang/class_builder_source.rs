// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `ClassBuilder` `methodSource:` auto-population (BT-2246).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! When a class is built programmatically via the `ClassBuilder` cascade API —
//! `Object classBuilder name: #C; methods: #{ #m => [:self | ...] }; register`
//! — the method bodies are block literals written directly in source. This
//! module recognises that shape and synthesises a `methodSource:` cascade
//! message carrying each block's reconstructed source text, so builder-defined
//! classes are visible to `SystemNavigation` source-text queries (`sendersOf:`,
//! `methodsMatching:`, ...) exactly like file-defined classes.
//!
//! Scope is the **instance** side only. Class methods defined via the builder
//! (`classMethods:`) are not handled: a programmatic builder stores them as
//! runtime funs, but class-method dispatch invokes a compiled `class_<sel>`
//! module function (`beamtalk_class_dispatch`), so builder class methods are not
//! callable in the first place — indexing their source would serve no method.
//! Class-side source indexing already works for file-defined classes (BT-2195).
//!
//! Only the **literal** case is handled: a symbol-literal selector mapped to a
//! block literal. Computed selectors or funs assembled at runtime have no
//! source literal; they are silently skipped and remain known-present but
//! unindexable (no crash) — the runtime defaults their `__source__` to an
//! empty binary.

use crate::ast::{
    Block, CascadeMessage, Expression, Identifier, KeywordPart, Literal, MapPair, MessageSelector,
    MethodDefinition, ParameterDefinition,
};
use crate::source_analysis::Span;

/// Mirrors the lexer's `is_binary_selector_char` so a symbol-literal selector
/// can be reclassified as binary/unary/keyword.
fn is_binary_selector_char(c: char) -> bool {
    matches!(
        c,
        '+' | '-' | '*' | '/' | '<' | '>' | '=' | '~' | '%' | '&' | '?' | ',' | '\\'
    )
}

/// Returns `true` if `expr` names the `ClassBuilder` class — either as a
/// `ClassReference` (the usual parse for a capitalised name) or a bare
/// identifier.
fn is_class_builder_name(expr: &Expression) -> bool {
    match expr {
        Expression::ClassReference { name, .. } => name.name == "ClassBuilder",
        Expression::Identifier(id) => id.name == "ClassBuilder",
        _ => false,
    }
}

/// Returns `true` if `expr` constructs a fresh `ClassBuilder`:
/// `ClassBuilder new` or `<receiver> classBuilder`.
fn is_class_builder_construction(expr: &Expression) -> bool {
    match expr {
        Expression::Parenthesized { expression, .. } => is_class_builder_construction(expression),
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } => match selector {
            MessageSelector::Unary(name) if name == "classBuilder" => true,
            MessageSelector::Unary(name) if name == "new" => {
                arguments.is_empty() && is_class_builder_name(receiver)
            }
            _ => false,
        },
        _ => false,
    }
}

/// Parses a selector symbol string into a [`MessageSelector`].
///
/// Returns `None` for malformed selectors (e.g. `at:put` — an interior colon
/// without a trailing one), which are left unindexed rather than guessed at.
pub(super) fn selector_from_symbol(sym: &str) -> Option<MessageSelector> {
    if sym.is_empty() {
        return None;
    }
    if sym.contains(':') {
        if !sym.ends_with(':') {
            return None;
        }
        let parts: Vec<KeywordPart> = sym
            .split_inclusive(':')
            .map(|part| KeywordPart::new(part, Span::new(0, 0)))
            .collect();
        Some(MessageSelector::Keyword(parts))
    } else if sym.chars().all(is_binary_selector_char) {
        Some(MessageSelector::Binary(sym.into()))
    } else {
        Some(MessageSelector::Unary(sym.into()))
    }
}

/// Reconstructs canonical method source for a `#selector => [block]` entry by
/// rebuilding a [`MethodDefinition`] and unparsing it — yielding source
/// identical in shape to a file-defined method.
///
/// The block's first parameter is the receiver (`self`); the remaining
/// parameters map positionally onto the selector's argument slots. Returns
/// `None` when the block shape does not conform to the selector (wrong
/// parameter count), leaving that method unindexed.
fn block_method_source(selector_sym: &str, block: &Block) -> Option<String> {
    let selector = selector_from_symbol(selector_sym)?;
    // A method fun receives `self` plus one argument per selector slot, so the
    // block must declare exactly `arity + 1` parameters.
    if block.parameters.len() != selector.arity() + 1 {
        return None;
    }
    let parameters: Vec<ParameterDefinition> = block.parameters[1..]
        .iter()
        .map(|bp| ParameterDefinition::new(Identifier::new(bp.name.clone(), bp.span)))
        .collect();
    let method = MethodDefinition::new(selector, parameters, block.body.clone(), block.span);
    Some(crate::unparse::unparse_method(&method))
}

/// Builds a `selector => "source"` map-literal expression from the literal
/// block entries in `pairs`. Returns `None` when no entry is an indexable
/// literal block (nothing to record).
fn build_source_map(pairs: &[MapPair]) -> Option<Expression> {
    let mut source_pairs: Vec<MapPair> = Vec::new();
    for pair in pairs {
        if let (Expression::Literal(Literal::Symbol(sym), _), Expression::Block(block)) =
            (&pair.key, &pair.value)
        {
            if let Some(source) = block_method_source(sym, block) {
                source_pairs.push(MapPair {
                    key: pair.key.clone(),
                    value: Expression::Literal(Literal::String(source.into()), pair.span),
                    span: pair.span,
                });
            }
        }
    }
    if source_pairs.is_empty() {
        None
    } else {
        Some(Expression::MapLiteral {
            pairs: source_pairs,
            span: Span::new(0, 0),
        })
    }
}

/// Returns the pairs of a single map-literal argument, if that is exactly what
/// `args` is.
fn single_map_literal(args: &[Expression]) -> Option<&[MapPair]> {
    match args {
        [Expression::MapLiteral { pairs, .. }] => Some(pairs),
        _ => None,
    }
}

/// Builds a one-part keyword selector such as `methodSource:`.
fn keyword_setter(name: &str) -> MessageSelector {
    MessageSelector::Keyword(vec![KeywordPart::new(name, Span::new(0, 0))])
}

/// Recognises a `ClassBuilder` construction cascade (terminating in `register`)
/// with literal block instance methods and returns an augmented message list
/// with a synthesised `methodSource:` setter prepended.
///
/// Returns `None` when the cascade is not a recognised `ClassBuilder`
/// construction, has nothing indexable, or already carries an explicit
/// `methodSource:` setter (which is left untouched).
pub(super) fn inject_method_source(
    receiver: &Expression,
    messages: &[CascadeMessage],
) -> Option<Vec<CascadeMessage>> {
    // The parser keeps the first cascade message inside `receiver` (as the
    // outer MessageSend); the object the cascade targets is that send's
    // receiver. A bare receiver (no leading message) is used as-is.
    let (underlying, first_msg): (&Expression, Option<(&MessageSelector, &[Expression])>) =
        match receiver {
            Expression::MessageSend {
                receiver: inner,
                selector,
                arguments,
                ..
            } => (inner.as_ref(), Some((selector, arguments.as_slice()))),
            other => (other, None),
        };

    if !is_class_builder_construction(underlying) {
        return None;
    }

    let mut methods_map: Option<&[MapPair]> = None;
    let mut has_method_source = false;
    let mut last_is_register = false;

    let all = first_msg.into_iter().chain(
        messages
            .iter()
            .map(|m| (&m.selector, m.arguments.as_slice())),
    );
    for (selector, args) in all {
        let name = selector.name();
        last_is_register = name.as_str() == "register";
        match name.as_str() {
            // Later setters win in a cascade, so track the *last* `methods:`
            // map. A later non-literal `methods:` clears it — the methods it
            // installs have no source literal to index.
            "methods:" => methods_map = single_map_literal(args),
            "methodSource:" => has_method_source = true,
            _ => {}
        }
    }

    // Require a *terminal* `register`: an inline builder construction always
    // registers as the final cascade message. Anchoring on the terminal
    // position (not mere presence) keeps the recognition from firing on an
    // unrelated `classBuilder`/`methods:` naming coincidence, where the injected
    // setter would be a runtime DNU.
    if !last_is_register || has_method_source {
        return None;
    }

    let map_expr = methods_map.and_then(build_source_map)?;
    let mut injected = vec![CascadeMessage::new(
        keyword_setter("methodSource:"),
        vec![map_expr],
        Span::new(0, 0),
    )];

    // Prepend the setter so it runs before the terminal `register`; ordering
    // among the pure state setters is irrelevant.
    injected.extend(messages.iter().cloned());
    Some(injected)
}

/// ADR 0084 / BT-2267: Recognise a `ClassBuilder` construction cascade that
/// carries a `classMethods:` setter, returning `(ClassName, ClassVarNames)` for
/// the codegen that lowers the class-method block literals into funs.
///
/// `ClassName` comes from a **literal** `name:` symbol (required — it keys the
/// runtime self/`super` dispatch the funs emit); `ClassVarNames` are the keys of
/// the `classVars:` map (used to recognise `self.cvar` as class-variable access).
/// Returns `None` when the cascade is not a recognised `ClassBuilder`
/// construction, lacks a terminal `register`, has no `classMethods:` setter, or
/// has no literal `name:`.
pub(super) fn builder_class_method_context(
    receiver: &Expression,
    messages: &[CascadeMessage],
) -> Option<(String, Vec<String>)> {
    let (underlying, first_msg): (&Expression, Option<(&MessageSelector, &[Expression])>) =
        match receiver {
            Expression::MessageSend {
                receiver: inner,
                selector,
                arguments,
                ..
            } => (inner.as_ref(), Some((selector, arguments.as_slice()))),
            other => (other, None),
        };

    if !is_class_builder_construction(underlying) {
        return None;
    }

    let mut class_name: Option<String> = None;
    let mut class_var_names: Vec<String> = Vec::new();
    let mut has_class_methods = false;
    let mut last_is_register = false;

    let all = first_msg.into_iter().chain(
        messages
            .iter()
            .map(|m| (&m.selector, m.arguments.as_slice())),
    );
    for (selector, args) in all {
        let name = selector.name();
        last_is_register = name.as_str() == "register";
        match name.as_str() {
            // Later setters win in a cascade — keep the last value.
            "name:" => {
                if let [Expression::Literal(Literal::Symbol(sym), _)] = args {
                    class_name = Some(sym.to_string());
                }
            }
            "classVars:" => {
                if let Some(pairs) = single_map_literal(args) {
                    class_var_names = pairs
                        .iter()
                        .filter_map(|p| match &p.key {
                            Expression::Literal(Literal::Symbol(s), _) => Some(s.to_string()),
                            _ => None,
                        })
                        .collect();
                }
            }
            "classMethods:" => has_class_methods = true,
            _ => {}
        }
    }

    if !last_is_register || !has_class_methods {
        return None;
    }
    class_name.map(|cn| (cn, class_var_names))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_cascade(src: &str) -> (Expression, Vec<CascadeMessage>) {
        use crate::source_analysis::{lex_with_eof, parse};
        let (module, diags) = parse(lex_with_eof(src));
        assert!(
            module.expressions.len() == 1,
            "expected one top-level expression; diags: {diags:?}"
        );
        match module.expressions.into_iter().next().unwrap().expression {
            Expression::Cascade {
                receiver, messages, ..
            } => (*receiver, messages),
            other => panic!("expected cascade, got {other:?}"),
        }
    }

    fn injected_source_map(messages: &[CascadeMessage], setter: &str) -> Vec<(String, String)> {
        let msg = messages
            .iter()
            .find(|m| m.selector.name() == setter)
            .unwrap_or_else(|| panic!("{setter} not injected"));
        match &msg.arguments[0] {
            Expression::MapLiteral { pairs, .. } => pairs
                .iter()
                .map(|p| {
                    let key = match &p.key {
                        Expression::Literal(Literal::Symbol(s), _) => s.to_string(),
                        other => panic!("expected symbol key, got {other:?}"),
                    };
                    let val = match &p.value {
                        Expression::Literal(Literal::String(s), _) => s.to_string(),
                        other => panic!("expected string value, got {other:?}"),
                    };
                    (key, val)
                })
                .collect(),
            other => panic!("expected map literal, got {other:?}"),
        }
    }

    #[test]
    fn selector_from_symbol_classifies_each_kind() {
        assert!(matches!(
            selector_from_symbol("inc"),
            Some(MessageSelector::Unary(_))
        ));
        assert!(matches!(
            selector_from_symbol("+"),
            Some(MessageSelector::Binary(_))
        ));
        match selector_from_symbol("at:put:") {
            Some(MessageSelector::Keyword(parts)) => assert_eq!(parts.len(), 2),
            other => panic!("expected keyword, got {other:?}"),
        }
        assert!(selector_from_symbol("at:put").is_none());
        assert!(selector_from_symbol("").is_none());
    }

    #[test]
    fn injects_method_source_for_literal_block() {
        let (recv, msgs) = parse_cascade(
            "Object classBuilder name: #Foo; \
             methods: #{ #greet => [:self | self name asUppercase] }; register",
        );
        let augmented = inject_method_source(&recv, &msgs).expect("should inject");
        let entries = injected_source_map(&augmented, "methodSource:");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].0, "greet");
        assert!(
            entries[0].1.contains("greet") && entries[0].1.contains("asUppercase"),
            "unexpected source: {}",
            entries[0].1
        );
        // The terminal register message is preserved and stays last.
        assert_eq!(augmented.last().unwrap().selector.name(), "register");
    }

    #[test]
    fn maps_keyword_block_params_to_method_signature() {
        let (recv, msgs) = parse_cascade(
            "ClassBuilder new methods: #{ #add: => [:self :amount | self.total := amount] }; \
             register",
        );
        let augmented = inject_method_source(&recv, &msgs).expect("should inject");
        let entries = injected_source_map(&augmented, "methodSource:");
        assert_eq!(entries.len(), 1);
        assert!(
            entries[0].1.contains("add:") && entries[0].1.contains("amount"),
            "unexpected source: {}",
            entries[0].1
        );
    }

    #[test]
    fn skips_non_class_builder_cascade() {
        let (recv, msgs) = parse_cascade("foo bar; methods: #{ #m => [:self | self x] }; baz");
        assert!(inject_method_source(&recv, &msgs).is_none());
    }

    #[test]
    fn skips_computed_non_literal_method_values() {
        // The fun is referenced, not a literal block — nothing to index.
        let (recv, msgs) =
            parse_cascade("Object classBuilder methods: #{ #m => someFun }; register");
        assert!(inject_method_source(&recv, &msgs).is_none());
    }

    #[test]
    fn skips_block_with_wrong_arity() {
        // Keyword selector `add:` needs `self` + one arg; block declares only `self`.
        let (recv, msgs) =
            parse_cascade("Object classBuilder methods: #{ #add: => [:self | self x] }; register");
        assert!(inject_method_source(&recv, &msgs).is_none());
    }

    #[test]
    fn skips_cascade_without_terminal_register() {
        // A ClassBuilder cascade with literal methods but no `register` is not a
        // complete construction — leave it untouched.
        let (recv, msgs) =
            parse_cascade("Object classBuilder name: #Foo; methods: #{ #m => [:_self | _self x] }");
        assert!(inject_method_source(&recv, &msgs).is_none());
    }

    #[test]
    fn skips_when_register_is_not_the_terminal_message() {
        // `register` present but not last — the guard anchors on terminal position.
        let (recv, msgs) = parse_cascade(
            "Object classBuilder methods: #{ #m => [:_self | _self x] }; register; name: #Foo",
        );
        assert!(inject_method_source(&recv, &msgs).is_none());
    }

    #[test]
    fn last_methods_map_wins_when_repeated() {
        // Later setters win in a cascade; the indexed source must come from the
        // last `methods:` map, not the first.
        let (recv, msgs) = parse_cascade(
            "Object classBuilder methods: #{ #a => [:_self | _self aa] }; \
             methods: #{ #b => [:_self | _self bb] }; register",
        );
        let augmented = inject_method_source(&recv, &msgs).expect("should inject");
        let entries = injected_source_map(&augmented, "methodSource:");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].0, "b");
    }

    #[test]
    fn does_not_double_inject_when_method_source_already_present() {
        let (recv, msgs) = parse_cascade(
            "Object classBuilder methods: #{ #m => [:self | self x] }; \
             methodSource: #{ #m => \"m => self x\" }; register",
        );
        assert!(inject_method_source(&recv, &msgs).is_none());
    }
}
