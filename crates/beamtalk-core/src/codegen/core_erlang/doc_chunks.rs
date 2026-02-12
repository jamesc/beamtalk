// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! EEP-48 doc chunk generation for Beamtalk.
//!
//! **DDD Context:** Code Generation
//!
//! Generates EEP-48 `docs_v1` Erlang terms from AST doc comments, enabling
//! runtime documentation access via `code:get_doc/1` and `shell_docs:render/2`.
//!
//! The generated term is written as an Erlang term file (`.docs`) alongside
//! the Core Erlang output. The compile escript then injects it as a "Docs"
//! chunk into the compiled `.beam` file.
//!
//! # EEP-48 Format
//!
//! ```erlang
//! {docs_v1, Anno, beamtalk, <<"text/markdown">>, ModuleDoc, Metadata, Docs}
//! ```
//!
//! Where each method doc entry is:
//! ```erlang
//! {{function, SelectorAtom, Arity}, Anno, [Signature], Doc, Metadata}
//! ```

use crate::ast::{MessageSelector, MethodDefinition, Module};

/// Generates an EEP-48 `docs_v1` Erlang term from a module's doc comments.
///
/// Returns `None` if there are no doc comments on the class or any methods.
/// Returns `Some(term)` with the complete Erlang term as a string suitable
/// for `file:consult/1`.
#[must_use]
pub fn generate_docs_term(module: &Module) -> Option<String> {
    let class = module.classes.first()?;

    let module_doc = format_doc(class.doc_comment.as_ref());
    let method_docs = generate_method_docs(&class.methods);
    let class_method_docs = generate_method_docs(&class.class_methods);

    // Only generate if there's at least one doc comment
    let has_any_docs = class.doc_comment.is_some()
        || class.methods.iter().any(|m| m.doc_comment.is_some())
        || class.class_methods.iter().any(|m| m.doc_comment.is_some());

    if !has_any_docs {
        return None;
    }

    let mut all_docs = method_docs;
    all_docs.extend(class_method_docs);
    let docs_list = if all_docs.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n{}\n]", all_docs.join(",\n"))
    };

    // {docs_v1, Anno, beamtalk, <<"text/markdown">>, ModuleDoc, Metadata, Docs}
    let term = format!(
        "{{docs_v1, #{{}}, beamtalk, <<\"text/markdown\">>, {module_doc}, #{{}}, {docs_list}}}.\n"
    );

    Some(term)
}

/// Formats a doc comment as an EEP-48 doc value.
///
/// - `Some(text)` → `#{<<"en">> => <<"text">>}`
/// - `None` → `none`
fn format_doc(doc: Option<&String>) -> String {
    match doc {
        Some(text) => {
            let escaped = escape_erlang_binary(text);
            format!("#{{<<\"en\">> => <<\"{escaped}\"/utf8>>}}")
        }
        None => "none".to_string(),
    }
}

/// Generates EEP-48 doc entries for a list of methods.
fn generate_method_docs(methods: &[MethodDefinition]) -> Vec<String> {
    methods.iter().map(generate_method_doc_entry).collect()
}

/// Generates a single EEP-48 doc entry for a method.
///
/// Format: `{{function, SelectorAtom, Arity}, Anno, [Signature], Doc, Metadata}`
fn generate_method_doc_entry(method: &MethodDefinition) -> String {
    let selector_atom = escape_erlang_atom(&method.selector.to_erlang_atom());
    let arity = method.selector.arity();
    let signature = format_signature(&method.selector, &method.parameters);
    let doc = format_doc(method.doc_comment.as_ref());
    let metadata = format_method_metadata(&method.selector);

    format!(
        "  {{{{{function}, '{selector_atom}', {arity}}}, #{{}}, [<<\"{signature}\"/utf8>>], {doc}, {metadata}}}",
        function = "function"
    )
}

/// Formats a method signature for display.
///
/// - Unary: `"increment"`
/// - Binary: `"+ other"`
/// - Keyword: `"at: index put: value"`
fn format_signature(selector: &MessageSelector, parameters: &[crate::ast::Identifier]) -> String {
    match selector {
        MessageSelector::Unary(name) => name.to_string(),
        MessageSelector::Binary(op) => {
            if let Some(param) = parameters.first() {
                format!("{op} {}", param.name)
            } else {
                op.to_string()
            }
        }
        MessageSelector::Keyword(parts) => {
            let mut sig = String::new();
            for (i, part) in parts.iter().enumerate() {
                if i > 0 {
                    sig.push(' ');
                }
                sig.push_str(&part.keyword);
                if let Some(param) = parameters.get(i) {
                    sig.push(' ');
                    sig.push_str(&param.name);
                }
            }
            sig
        }
    }
}

/// Formats method metadata with the selector name.
fn format_method_metadata(selector: &MessageSelector) -> String {
    let selector_atom = escape_erlang_atom(&selector.to_erlang_atom());
    format!("#{{selector => '{selector_atom}'}}")
}

/// Escapes a string for use inside an Erlang binary literal (`<<"...">>`).
fn escape_erlang_binary(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            _ => result.push(c),
        }
    }
    result
}

/// Escapes a string for use as an Erlang atom (inside single quotes).
fn escape_erlang_atom(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\'' => result.push_str("\\'"),
            '\\' => result.push_str("\\\\"),
            _ => result.push(c),
        }
    }
    result
}

/// Returns `true` if a module has any doc comments worth generating.
#[must_use]
pub fn has_docs(module: &Module) -> bool {
    module.classes.first().is_some_and(|class| {
        class.doc_comment.is_some()
            || class.methods.iter().any(|m| m.doc_comment.is_some())
            || class.class_methods.iter().any(|m| m.doc_comment.is_some())
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        ClassDefinition, Expression, Identifier, KeywordPart, Literal, MethodDefinition,
    };
    use crate::source_analysis::Span;

    fn make_class(doc: Option<&str>, methods: Vec<MethodDefinition>) -> ClassDefinition {
        let mut class = ClassDefinition::new(
            Identifier::new("Counter", Span::new(0, 7)),
            Identifier::new("Actor", Span::new(0, 5)),
            vec![],
            methods,
            Span::new(0, 100),
        );
        class.doc_comment = doc.map(String::from);
        class
    }

    fn make_method(
        selector: MessageSelector,
        params: Vec<&str>,
        doc: Option<&str>,
    ) -> MethodDefinition {
        let parameters = params
            .into_iter()
            .map(|p| Identifier::new(p, Span::new(0, u32::try_from(p.len()).unwrap_or(0))))
            .collect();
        let mut method = MethodDefinition::new(
            selector,
            parameters,
            vec![Expression::Literal(Literal::Integer(42), Span::new(0, 2))],
            Span::new(0, 50),
        );
        method.doc_comment = doc.map(String::from);
        method
    }

    fn make_module(class: ClassDefinition) -> Module {
        Module::with_classes(vec![class], Span::new(0, 200))
    }

    #[test]
    fn no_docs_returns_none() {
        let class = make_class(
            None,
            vec![make_method(
                MessageSelector::Unary("increment".into()),
                vec![],
                None,
            )],
        );
        let module = make_module(class);
        assert!(generate_docs_term(&module).is_none());
    }

    #[test]
    fn class_doc_only() {
        let class = make_class(Some("A counter class."), vec![]);
        let module = make_module(class);
        let term = generate_docs_term(&module).unwrap();
        assert!(term.contains("docs_v1"));
        assert!(term.contains("A counter class."));
        assert!(term.contains("text/markdown"));
        assert!(term.contains("beamtalk"));
    }

    #[test]
    fn method_doc_unary() {
        let method = make_method(
            MessageSelector::Unary("increment".into()),
            vec![],
            Some("Increment the counter."),
        );
        let class = make_class(None, vec![method]);
        let module = make_module(class);
        let term = generate_docs_term(&module).unwrap();
        assert!(term.contains("function"));
        assert!(term.contains("'increment'"));
        assert!(term.contains(", 0}"));
        assert!(term.contains("Increment the counter."));
    }

    #[test]
    fn method_doc_binary() {
        let method = make_method(
            MessageSelector::Binary("+".into()),
            vec!["other"],
            Some("Add to the receiver."),
        );
        let class = make_class(None, vec![method]);
        let module = make_module(class);
        let term = generate_docs_term(&module).unwrap();
        assert!(term.contains("'+'"));
        assert!(term.contains(", 1}"));
        assert!(term.contains("+ other"));
    }

    #[test]
    fn method_doc_keyword() {
        let method = make_method(
            MessageSelector::Keyword(vec![
                KeywordPart::new("at:", Span::new(0, 3)),
                KeywordPart::new("put:", Span::new(5, 9)),
            ]),
            vec!["index", "value"],
            Some("Put a value at an index."),
        );
        let class = make_class(None, vec![method]);
        let module = make_module(class);
        let term = generate_docs_term(&module).unwrap();
        assert!(term.contains("'at:put:'"));
        assert!(term.contains(", 2}"));
        assert!(term.contains("at: index put: value"));
    }

    #[test]
    fn escapes_special_chars_in_doc() {
        let method = make_method(
            MessageSelector::Unary("test".into()),
            vec![],
            Some("A \"quoted\" doc with\nnewline."),
        );
        let class = make_class(None, vec![method]);
        let module = make_module(class);
        let term = generate_docs_term(&module).unwrap();
        assert!(term.contains("\\\"quoted\\\""));
        assert!(term.contains("\\n"));
    }

    #[test]
    fn format_signature_unary() {
        let sig = format_signature(&MessageSelector::Unary("size".into()), &[]);
        assert_eq!(sig, "size");
    }

    #[test]
    fn format_signature_binary() {
        let params = vec![Identifier::new("other", Span::new(0, 5))];
        let sig = format_signature(&MessageSelector::Binary("+".into()), &params);
        assert_eq!(sig, "+ other");
    }

    #[test]
    fn format_signature_keyword() {
        let params = vec![
            Identifier::new("index", Span::new(0, 5)),
            Identifier::new("value", Span::new(0, 5)),
        ];
        let sig = format_signature(
            &MessageSelector::Keyword(vec![
                KeywordPart::new("at:", Span::new(0, 3)),
                KeywordPart::new("put:", Span::new(5, 9)),
            ]),
            &params,
        );
        assert_eq!(sig, "at: index put: value");
    }

    #[test]
    fn has_docs_empty_module() {
        let module = Module::new(vec![], Span::new(0, 0));
        assert!(!has_docs(&module));
    }

    #[test]
    fn has_docs_with_class_doc() {
        let class = make_class(Some("Documented class."), vec![]);
        let module = make_module(class);
        assert!(has_docs(&module));
    }

    #[test]
    fn has_docs_with_method_doc() {
        let method = make_method(
            MessageSelector::Unary("test".into()),
            vec![],
            Some("Documented method."),
        );
        let class = make_class(None, vec![method]);
        let module = make_module(class);
        assert!(has_docs(&module));
    }
}
