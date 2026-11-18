// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Declaration unparsing: classes, methods, protocols, type aliases, native declarations and `@expect` categories.
//! Split from `mod.rs`; see the module docs there.

use super::{
    ClassDefinition, Document, ExpectCategory, Identifier, KeywordPart, Literal,
    LiteralCommentScope, MethodDefinition, Module, NativeDeclaration, PipelineScope,
    ProtocolDefinition, ProtocolMethodSignature, ProtocolUse, SlotKind, StandaloneMethodDefinition,
    StateDeclaration, TypeAliasDefinition, break_, concat, docvec, group, join_docs,
    leading_ends_with_orphaned_doc_comment, leaf, line, nest, nil, unparse_comment,
    unparse_comment_attachment_leading, unparse_expression, unparse_expression_statement,
    unparse_literal, unparse_method_display_signature_doc, unparse_selector_and_params,
    unparse_type_annotation,
};

/// One of the four top-level declaration kinds that can appear in a
/// [`Module`] outside of standalone methods and expressions.
///
/// Used only by [`unparse_module_doc`] to interleave classes, protocols,
/// type aliases, and native declarations back into their original source
/// order rather than grouping them by kind.
enum TopLevelDecl<'a> {
    Class(&'a ClassDefinition),
    Protocol(&'a ProtocolDefinition),
    TypeAlias(&'a TypeAliasDefinition),
    Native(&'a NativeDeclaration),
    Method(&'a StandaloneMethodDefinition),
}

impl TopLevelDecl<'_> {
    /// Whether a blank line preceded this declaration in the source.
    fn preceding_blank_line(&self) -> bool {
        match self {
            Self::Class(class) => class.comments.leading_blank_line,
            Self::Protocol(protocol) => protocol.comments.leading_blank_line,
            Self::TypeAlias(type_alias) => type_alias.comments.leading_blank_line,
            Self::Native(native) => native.comments.leading_blank_line,
            Self::Method(smd) => smd.method.comments.leading_blank_line,
        }
    }

    /// Source offset where this declaration starts.
    fn start(&self) -> u32 {
        match self {
            Self::Class(class) => class.span.start(),
            Self::Protocol(protocol) => protocol.span.start(),
            Self::TypeAlias(type_alias) => type_alias.span.start(),
            Self::Native(native) => native.span.start(),
            Self::Method(smd) => smd.span.start(),
        }
    }
}

/// Builds a [`Document`] for a [`Module`].
#[must_use]
pub(crate) fn unparse_module_doc(module: &Module) -> Document<'static> {
    // Code outside any method re-sugars from the module's own `|>` record;
    // each method replaces it with its own (ADR 0145).
    let _pipeline_scope = PipelineScope::enter(&module.pipeline_sends);
    let _literal_scope = LiteralCommentScope::enter(&module.literal_comments);
    let mut docs: Vec<Document<'static>> = Vec::new();

    // File-level leading comments (empty module edge case — ADR 0044)
    for comment in &module.file_leading_comments {
        docs.push(unparse_comment(comment));
        docs.push(line());
    }

    // Classes, protocols, type aliases, native declarations (ADR 0068
    // Phase 2a, ADR 0108 Phase 1) and standalone/extension methods are
    // interleaved back into their original source order rather than grouped
    // by kind — a `type` alias declared after a class must stay after that
    // class, and a leading `Port >> foo` extension (with the license header
    // attached to it) must stay ahead of the class that follows it
    // (BT-4488). `sort_by_key` is a stable sort, so declarations that
    // legitimately share a start offset (shouldn't happen for top-level
    // decls) keep this append order.
    let mut top_level_decls: Vec<TopLevelDecl<'_>> = Vec::new();
    top_level_decls.extend(module.classes.iter().map(TopLevelDecl::Class));
    top_level_decls.extend(module.protocols.iter().map(TopLevelDecl::Protocol));
    top_level_decls.extend(module.type_aliases.iter().map(TopLevelDecl::TypeAlias));
    top_level_decls.extend(module.native_declarations.iter().map(TopLevelDecl::Native));
    top_level_decls.extend(module.method_definitions.iter().map(TopLevelDecl::Method));
    top_level_decls.sort_by_key(TopLevelDecl::start);

    for decl in top_level_decls {
        // Re-emit the blank line the author placed between this
        // declaration and whatever precedes it. Dropped when nothing
        // precedes it — a blank line before the very first top-level
        // declaration is not preserved; we only preserve inter-item gaps.
        if !docs.is_empty() && decl.preceding_blank_line() {
            docs.push(line());
        }
        match decl {
            TopLevelDecl::Class(class) => docs.push(unparse_class_definition(class)),
            TopLevelDecl::Protocol(protocol) => docs.push(unparse_protocol_definition(protocol)),
            TopLevelDecl::TypeAlias(type_alias) => {
                docs.push(unparse_type_alias_definition(type_alias));
            }
            TopLevelDecl::Native(native) => {
                docs.push(unparse_native_declaration(native));
            }
            TopLevelDecl::Method(smd) => {
                docs.push(unparse_standalone_method_definition(smd));
            }
        }
        docs.push(line());
    }

    // Top-level expressions (script / REPL)
    for (i, stmt) in module.expressions.iter().enumerate() {
        if i > 0 {
            // Emit an extra blank line if present in source
            if stmt.preceding_blank_line {
                docs.push(line());
            }
            docs.push(line());
        } else if !docs.is_empty() && stmt.preceding_blank_line {
            // Preserve a blank line separating the first top-level
            // expression from the declarations/standalone-methods section(s)
            // above it. The i > 0 branch above already provides its own
            // separator `line()` between expressions; here, the previous
            // section's trailing `line()` already plays that role, so a
            // single extra `line()` is enough to open up the blank line.
            docs.push(line());
        }
        docs.push(unparse_expression_statement(stmt));
    }

    // File-level trailing comments (after the last class/method/expression)
    for (i, comment) in module.file_trailing_comments.iter().enumerate() {
        // Top-level expressions end without a line break, so the first
        // trailing comment must start on a fresh line rather than being
        // glued onto the last statement's own trailing comment (BT-4790).
        if i == 0 && !module.expressions.is_empty() {
            docs.push(line());
        }
        if comment.preceding_blank_line {
            docs.push(line());
        }
        docs.push(unparse_comment(comment));
        docs.push(line());
    }

    concat(docs)
}

/// Builds a [`Document`] for a [`ClassDefinition`].
///
/// Emits non-doc comments, then the optional doc comment, then the class header,
/// state declarations, and methods.
#[must_use]
#[allow(clippy::too_many_lines)]
pub(crate) fn unparse_class_definition(class: &ClassDefinition) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::new();

    // Non-doc leading comments
    docs.extend(unparse_comment_attachment_leading(&class.comments));

    // Blank line between leading comments (e.g. license block) and doc/header:
    // either because the source actually had one there, or because
    // the last leading entry is an orphaned `///` block that must, by
    // construction, always be separated from what follows — see
    // `leading_ends_with_orphaned_doc_comment`.
    if leading_ends_with_orphaned_doc_comment(&class.comments)
        || class.comments.blank_line_after_comments
    {
        docs.push(line());
    }

    // Doc comment — emit `///` for empty lines (no trailing space)
    if let Some(doc) = &class.doc_comment {
        for line_text in doc.lines() {
            if line_text.is_empty() {
                docs.push(Document::Str("///"));
            } else {
                docs.push(docvec!["/// ", leaf::raw_text(line_text)]);
            }
            docs.push(line());
        }
    }

    // Class header: `Superclass subclass: ClassName`
    let superclass = class
        .superclass
        .as_ref()
        .map_or_else(|| "nil".to_string(), |s| s.name.to_string());

    let mut modifiers: Vec<Document<'static>> = Vec::new();
    if class.is_internal {
        modifiers.push(Document::Str("internal "));
    }
    if class.is_abstract {
        modifiers.push(Document::Str("abstract "));
    }
    if class.is_sealed {
        modifiers.push(Document::Str("sealed "));
    }
    if class.is_typed {
        modifiers.push(Document::Str("typed "));
    }

    let class_name_doc = if class.type_params.is_empty() {
        leaf::ident(&class.name.name)
    } else {
        let params: Vec<Document<'static>> = class
            .type_params
            .iter()
            .enumerate()
            .map(|(i, p)| {
                let param_doc = if let Some(ref bound) = p.bound {
                    docvec![leaf::ident(&p.name.name), " :: ", leaf::ident(&bound.name)]
                } else {
                    leaf::ident(&p.name.name)
                };
                if i == 0 {
                    param_doc
                } else {
                    docvec![", ", param_doc]
                }
            })
            .collect();
        docvec![leaf::ident(&class.name.name), "(", concat(params), ")"]
    };

    // Emit superclass type args: `Collection(E)` or `Collection(Integer)`
    // A package-qualified superclass (ADR 0070) keeps its `package@` prefix, so
    // `beamtalk fmt` does not change which class is inherited.
    let superclass_name_doc = match &class.superclass_package {
        Some(package) if class.superclass.is_some() => {
            docvec![leaf::ident(&package.name), "@", leaf::ident(&superclass)]
        }
        _ => leaf::ident(&superclass),
    };
    let superclass_with_type_args = if class.superclass_type_args.is_empty() {
        superclass_name_doc
    } else {
        let mut parts = vec![superclass_name_doc, Document::Str("(")];
        for (i, ta) in class.superclass_type_args.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            parts.push(leaf::ident(ta.type_name()));
        }
        parts.push(Document::Str(")"));
        concat(parts)
    };

    let class_header = docvec![
        concat(modifiers),
        superclass_with_type_args,
        " subclass: ",
        class_name_doc,
    ];

    let header = if let Some(module) = &class.backing_module {
        docvec![class_header, " native: ", leaf::ident(&module.name)]
    } else {
        class_header
    };

    let header = if let Some(trail) = &class.comments.trailing {
        docvec![header, "  ", unparse_comment(trail)]
    } else {
        header
    };

    // `handleScope: #symbol` (ADR 0103) — canonical style puts it on its own
    // indented line following the class header (and its trailing comment, if
    // any), mirroring `docs/ADR/0103-sendability-typing-from-class-kinds.md`.
    let header = if let Some(hs) = &class.handle_scope {
        docvec![
            header,
            nest(2, docvec![line(), "handleScope: #", leaf::ident(&hs.name)])
        ]
    } else {
        header
    };

    // `shapeVersion: N` (ADR 0123 §1) — same canonical placement as
    // `handleScope:` above: its own indented line following the class
    // header (and any earlier header clause).
    let header = if let Some(sv) = &class.shape_version {
        docvec![
            header,
            nest(
                2,
                docvec![
                    line(),
                    "shapeVersion: ",
                    leaf::int_lit(i64::from(sv.version))
                ]
            )
        ]
    } else {
        header
    };

    docs.push(header);

    // `uses:` lines (ADR 0127 §2) — one per line, immediately after the
    // header and before any state/class-var/method, matching where the
    // parser requires them.
    for use_line in &class.uses {
        docs.push(nest(2, docvec![line(), unparse_protocol_use(use_line)]));
    }

    // State declarations — use nest(2, ...) so that leading comments and
    // doc-comment lines inside the declaration are also indented at column 2,
    // matching the `line()` inside nest() pattern used for instance methods.
    for state in &class.state {
        docs.push(nest(2, docvec![line(), unparse_state_declaration(state)]));
    }

    // Class variables
    for state in &class.class_variables {
        docs.push(nest(
            2,
            docvec![line(), unparse_class_state_declaration(state)],
        ));
    }

    // Blank line before first method (always, regardless of whether state is present)
    if !class.methods.is_empty() || !class.class_methods.is_empty() {
        docs.push(line());
    }

    // Class-side methods (before instance methods)
    for (i, method) in class.class_methods.iter().enumerate() {
        if i > 0 {
            // Blank line between consecutive class-side methods
            docs.push(line());
        }
        docs.push(nest(
            2,
            docvec![
                line(),
                unparse_method_definition_with_prefix(method, Document::Str("class "))
            ],
        ));
    }

    // Blank line between last class-side method and first instance method
    if !class.methods.is_empty() && !class.class_methods.is_empty() {
        docs.push(line());
    }

    // Instance methods — the line() is placed INSIDE nest(2, ...) so it renders
    // at indent=2, giving the leading comment and method signature their correct
    // 2-space indentation.  A leading comment's trailing line() also runs at
    // indent=2, so the signature is never shifted to column 0.
    for (i, method) in class.methods.iter().enumerate() {
        if i > 0 {
            // Blank line between consecutive methods
            docs.push(line());
        }
        docs.push(nest(2, docvec![line(), unparse_method_definition(method)]));
    }

    concat(docs)
}

/// Builds a [`Document`] for a [`StandaloneMethodDefinition`].
///
/// Example: `Counter >> increment => self.value := self.value + 1`
#[must_use]
pub(crate) fn unparse_standalone_method_definition(
    smd: &StandaloneMethodDefinition,
) -> Document<'static> {
    let class = leaf::ident(&smd.class_name.name);
    // Cross-package extension methods (ADR 0070): `package@ClassName >> ...`.
    // `smd.package`, when present, must be consulted here so `beamtalk fmt`
    // does not drop the package qualifier from a cross-package
    // standalone method.
    let class = if let Some(package) = &smd.package {
        docvec![leaf::ident(&package.name), "@", class]
    } else {
        class
    };
    let separator = if smd.is_class_method {
        Document::Str(" class >> ")
    } else {
        Document::Str(" >> ")
    };
    // The target goes on the signature line, after the method's leading
    // comments and doc comment — not in front of them (BT-4253).
    // A standalone method is a top-level declaration, so — like a class,
    // protocol, type alias or native declaration — it keeps the blank line
    // the author left between its leading comments (e.g. a license header)
    // and the method itself (BT-4488).
    unparse_method_definition_inner(
        &smd.method,
        docvec![class, separator],
        EmitLeadingComments::YesKeepingTrailingGap,
    )
}

/// Builds a [`Document`] for a [`TypeAliasDefinition`] (ADR 0108 Phase 1).
///
/// Emits the optional doc comment followed by the declaration header
/// (`type Name = <TypeAnnotation>`).
#[must_use]
fn unparse_type_alias_definition(type_alias: &TypeAliasDefinition) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::new();

    // Non-doc leading comments
    docs.extend(unparse_comment_attachment_leading(&type_alias.comments));

    // Blank line between this alias's leading comments and its own doc
    // comment/header: either because the source actually had one there,
    // or because the last leading entry is a preserved, earlier
    // `///` block that broke away from a different declaration —
    // see `leading_ends_with_orphaned_doc_comment`.
    if leading_ends_with_orphaned_doc_comment(&type_alias.comments)
        || type_alias.comments.blank_line_after_comments
    {
        docs.push(line());
    }

    // Doc comment
    if let Some(doc) = &type_alias.doc_comment {
        for line_text in doc.lines() {
            if line_text.is_empty() {
                docs.push(Document::Str("///"));
            } else {
                docs.push(docvec!["/// ", leaf::raw_text(line_text)]);
            }
            docs.push(line());
        }
    }

    // `(internal )?type Name = <TypeAnnotation>`
    let internal_prefix = if type_alias.is_internal {
        Document::Str("internal ")
    } else {
        Document::Nil
    };
    let header = docvec![
        internal_prefix,
        "type ",
        leaf::ident(&type_alias.name.name),
        " = ",
        unparse_type_annotation(&type_alias.annotation),
    ];

    // Trailing end-of-line comment on the declaration line, e.g.
    // `type Port = Integer // comment`.
    let header = if let Some(trail) = &type_alias.comments.trailing {
        docvec![header, "  ", unparse_comment(trail)]
    } else {
        header
    };

    docs.push(header);

    concat(docs)
}

/// Builds a [`Document`] for a [`ProtocolDefinition`] (ADR 0068 Phase 2a).
///
/// Emits the protocol header (`Protocol define: Name`) followed by
/// optional type parameters, `extending:` clause, and method signatures.
#[must_use]
fn unparse_protocol_definition(protocol: &ProtocolDefinition) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::new();

    // Non-doc leading comments
    docs.extend(unparse_comment_attachment_leading(&protocol.comments));

    // Blank line between this protocol's leading comments and its own doc
    // comment/header: either because the source actually had one there,
    // or because the last leading entry is a preserved, earlier
    // `///` block that broke away from a different declaration —
    // see `leading_ends_with_orphaned_doc_comment`.
    if leading_ends_with_orphaned_doc_comment(&protocol.comments)
        || protocol.comments.blank_line_after_comments
    {
        docs.push(line());
    }

    // Doc comment
    if let Some(doc) = &protocol.doc_comment {
        for line_text in doc.lines() {
            if line_text.is_empty() {
                docs.push(Document::Str("///"));
            } else {
                docs.push(docvec!["/// ", leaf::raw_text(line_text)]);
            }
            docs.push(line());
        }
    }

    // Protocol header: `Protocol define: Name`
    let mut header: Vec<Document<'static>> = vec![
        Document::Str("Protocol define: "),
        leaf::ident(&protocol.name.name),
    ];

    // Type parameters: `(E)`, `(K, V)`
    if !protocol.type_params.is_empty() {
        header.push(Document::Str("("));
        for (i, tp) in protocol.type_params.iter().enumerate() {
            if i > 0 {
                header.push(Document::Str(", "));
            }
            header.push(leaf::ident(&tp.name.name));
        }
        header.push(Document::Str(")"));
    }

    docs.push(Document::Vec(header));

    // Trailing end-of-line comment on the declaration header line, e.g.
    // `Protocol define: Sortable // comment`. Emitted right after
    // the header, matching where the parser collects it — before
    // `extending:`/the body, which start on their own indented lines.
    if let Some(trail) = &protocol.comments.trailing {
        docs.push(Document::Str("  "));
        docs.push(unparse_comment(trail));
    }

    // `extending: ParentProtocol`
    if let Some(ext) = &protocol.extending {
        docs.push(line());
        docs.push(docvec!["  extending: ", leaf::ident(&ext.name)]);
    }

    // Instance and class method signatures (indented by 2 spaces). Tracked
    // across both lists with a single `first_signature` flag so a blank
    // line is preserved at the instance/class boundary too (a `class`
    // signature immediately after an instance signature is just as much
    // an "inter-signature gap" as two instance signatures).
    let mut first_signature = true;
    for sig in &protocol.method_signatures {
        docs.push(nest(
            2,
            unparse_protocol_signature_entry(sig, None, &mut first_signature),
        ));
    }

    // Class method signatures (indented by 2 spaces, prefixed with `class`)
    for sig in &protocol.class_method_signatures {
        docs.push(nest(
            2,
            unparse_protocol_signature_entry(sig, Some("class "), &mut first_signature),
        ));
    }

    // Provided (instance-side) methods — `=>` bodies (ADR 0127 §1). Reuses
    // the same `first_signature` optional-blank-line-preservation flag as
    // the signature lists above (rather than the class body's own
    // always-blank-line-between-methods rule), matching the ADR's own
    // `comparable.bt` example where consecutive short provisions
    // (`>`, `<=`, `>=`) have no blank line between them. `unparse_method_definition`
    // already emits the method's own leading comments/doc comment, so only
    // the separating blank line is added here.
    for method in &protocol.provided_methods {
        if !first_signature && method.comments.leading_blank_line {
            docs.push(line());
        }
        docs.push(nest(2, docvec![line(), unparse_method_definition(method)]));
        first_signature = false;
    }

    concat(docs)
}

/// Builds a [`Document`] for a [`NativeDeclaration`] (ADR 0075 Phase 2).
///
/// Emits the header (`declare native: <module>`) followed by the typed
/// method signatures. Structurally a slimmed-down
/// [`unparse_protocol_definition`]: no type parameters, no `extending:`
/// clause, no class-side signatures, no provided methods — signature
/// entries (including their doc comments and inter-signature blank lines)
/// reuse [`unparse_protocol_signature_entry`] directly.
#[must_use]
fn unparse_native_declaration(native: &NativeDeclaration) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::new();

    // Non-doc leading comments
    docs.extend(unparse_comment_attachment_leading(&native.comments));

    // Blank line between this declaration's leading comments and its own
    // doc comment/header — mirrors the protocol handling above.
    if leading_ends_with_orphaned_doc_comment(&native.comments)
        || native.comments.blank_line_after_comments
    {
        docs.push(line());
    }

    // Doc comment
    if let Some(doc) = &native.doc_comment {
        for line_text in doc.lines() {
            if line_text.is_empty() {
                docs.push(Document::Str("///"));
            } else {
                docs.push(docvec!["/// ", leaf::raw_text(line_text)]);
            }
            docs.push(line());
        }
    }

    // Header: `declare native: <module>`
    docs.push(docvec![
        "declare native: ",
        leaf::ident(&native.module.name),
    ]);

    // Trailing end-of-line comment on the header line, e.g.
    // `declare native: lists // comment`.
    if let Some(trail) = &native.comments.trailing {
        docs.push(Document::Str("  "));
        docs.push(unparse_comment(trail));
    }

    // Method signatures (indented by 2 spaces), reusing the protocol
    // signature-entry renderer for inter-signature blank lines.
    let mut first_signature = true;
    for sig in &native.method_signatures {
        docs.push(nest(
            2,
            unparse_protocol_signature_entry(sig, None, &mut first_signature),
        ));
    }

    concat(docs)
}

/// Builds the `Document` for a single protocol method signature entry
/// within `unparse_protocol_definition`'s signature list — the separating
/// `line()` before it, plus an extra blank line re-emitted when
/// the signature had one before it in the source. Skipped for the very
/// first signature in the body — a blank line there (between the header/
/// `extending:` clause and the first signature) is dropped, same as
/// pre-existing behaviour and mirroring the top-level-declaration
/// handling; only inter-signature gaps are preserved.
fn unparse_protocol_signature_entry(
    sig: &ProtocolMethodSignature,
    prefix: Option<&'static str>,
    first_signature: &mut bool,
) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::new();
    if !*first_signature && sig.comments.leading_blank_line {
        docs.push(line());
    }
    docs.push(line());
    docs.push(unparse_protocol_method_signature(sig, prefix));
    *first_signature = false;
    concat(docs)
}

/// Builds a [`Document`] for a [`ProtocolMethodSignature`].
///
/// Protocol method signatures look like method definitions without `=>` and body:
/// `asString -> String`, `do: block :: Block(E, Object)`
///
/// An optional `prefix` (e.g. `"class "`) is inserted after any doc comment lines
/// but before the selector, so doc comments appear above the signature line.
fn unparse_protocol_method_signature(
    sig: &ProtocolMethodSignature,
    prefix: Option<&'static str>,
) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::new();

    // Non-doc leading comments
    docs.extend(unparse_comment_attachment_leading(&sig.comments));

    // Blank line between a preserved, earlier `///` block that broke away
    // from a different declaration and this signature's own doc
    // comment — see `leading_ends_with_orphaned_doc_comment`.
    if leading_ends_with_orphaned_doc_comment(&sig.comments) {
        docs.push(line());
    }

    // Doc comment
    if let Some(doc) = &sig.doc_comment {
        for line_text in doc.lines() {
            if line_text.is_empty() {
                docs.push(Document::Str("///"));
            } else {
                docs.push(docvec!["/// ", leaf::raw_text(line_text)]);
            }
            docs.push(line());
        }
    }

    // Optional prefix (e.g. `class `) appears on the signature line, after doc comments
    if let Some(p) = prefix {
        docs.push(Document::Str(p));
    }

    // Selector and parameters (shared with method signatures via
    // `unparse_selector_and_params`, rather than an independent copy of the
    // same keyword/parameter interleaving loop).
    docs.push(unparse_selector_and_params(&sig.selector, &sig.parameters));

    // Return type
    if let Some(ref ret) = sig.return_type {
        docs.push(Document::Str(" -> "));
        docs.push(unparse_type_annotation(ret));
    }

    concat(docs)
}

/// Builds a [`Document`] for a [`MethodDefinition`].
///
/// Emits:
/// 1. Non-doc leading comments (one per line)
/// 2. Doc comment lines (one per line)
/// 3. Method signature (`selector param =>`)
/// 4. Method body — single-expression (no leading comments) goes on the same
///    line; multi-expression bodies or bodies with leading comments go on new lines.
#[must_use]
pub(crate) fn unparse_method_definition(method: &MethodDefinition) -> Document<'static> {
    unparse_method_definition_with_prefix(method, nil())
}

/// Builds a method definition document with an optional prefix before the signature.
///
/// The prefix (e.g. `"class "`) is inserted between the comments/doc-comment and the
/// method signature, so that `class` appears on the signature line, not before the comments.
fn unparse_method_definition_with_prefix(
    method: &MethodDefinition,
    prefix: Document<'static>,
) -> Document<'static> {
    unparse_method_definition_inner(method, prefix, EmitLeadingComments::Yes)
}

/// Whether to emit a method's leading non-doc comments. Whole-file unparse keeps
/// them (file fidelity); the per-method [`unparse_method`] drops them so the
/// per-method source matches its byte span (see `unparse_method`).
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum EmitLeadingComments {
    Yes,
    /// Like [`Self::Yes`], and also re-emit a blank line the source had
    /// between the leading comments and the method (its
    /// `blank_line_after_comments`). Used for top-level standalone methods,
    /// matching the other top-level declarations.
    YesKeepingTrailingGap,
    No,
}

pub(super) fn unparse_method_definition_inner(
    method: &MethodDefinition,
    prefix: Document<'static>,
    emit_leading: EmitLeadingComments,
) -> Document<'static> {
    // The body re-sugars from the method's own `|>` record (ADR 0145).
    let _pipeline_scope = PipelineScope::enter(&method.pipeline_sends);
    let _literal_scope = LiteralCommentScope::enter(&method.literal_comments);
    let mut docs: Vec<Document<'static>> = Vec::new();

    // Non-doc leading comments (section dividers etc.) — emitted only on the
    // whole-file path; the per-method edit unit excludes them.
    let keep_trailing_gap = emit_leading == EmitLeadingComments::YesKeepingTrailingGap;
    let emit_leading = emit_leading != EmitLeadingComments::No;
    if emit_leading {
        docs.extend(unparse_comment_attachment_leading(&method.comments));
    }

    // A method with no doc comment of its own still needs a separator after
    // a preserved, earlier `///` block that broke away from a different
    // declaration — otherwise the orphaned block re-attaches to
    // this method as its doc comment on the next parse. When the method
    // *does* have its own doc comment, the blank-line push inside the `if
    // let Some(doc)` block below already covers this (it fires for any
    // non-empty leading comments, orphaned or not) — do not also push here,
    // or the separator doubles up.
    if method.doc_comment.is_none()
        && emit_leading
        && (leading_ends_with_orphaned_doc_comment(&method.comments)
            || (keep_trailing_gap && method.comments.blank_line_after_comments))
    {
        docs.push(line());
    }

    // Doc comment — emit `///` for empty lines (no trailing space)
    if let Some(doc) = &method.doc_comment {
        // Blank line between leading comments and doc comment (e.g. section separators)
        if emit_leading && !method.comments.leading.is_empty() {
            docs.push(line());
        }
        for line_text in doc.lines() {
            if line_text.is_empty() {
                docs.push(Document::Str("///"));
            } else {
                docs.push(docvec!["/// ", leaf::raw_text(line_text)]);
            }
            docs.push(line());
        }
    }

    // Emit @expect directive before the method declaration
    if let Some((ref cats, ref reason, _)) = method.expect {
        let base = docvec!["@expect ", unparse_expect_categories(cats)];
        if let Some(reason) = reason {
            docs.push(docvec![base, " \"", leaf::string_content(reason), "\""]);
        } else {
            docs.push(base);
        }
        docs.push(line());
    }

    // Optional prefix (e.g. "class ") then method signature
    docs.push(prefix);
    docs.push(unparse_method_signature(method));

    // Body — single expression with no leading comments goes on the same line;
    // multi-expression bodies (or expressions with leading comments) go on new lines.
    match method.body.as_slice() {
        [] => {
            // Empty body — just the signature
            if let Some(trail) = &method.comments.trailing {
                docs.push(Document::Str("  "));
                docs.push(unparse_comment(trail));
            }
        }
        [single] if single.comments.leading.is_empty() => {
            // Single expression with no leading comments — try inline,
            // break to indented next line if too wide or if body is multi-line.
            let body = unparse_expression(&single.expression);
            let trail_doc = if let Some(trail) = &single.comments.trailing {
                docvec!["  ", unparse_comment(trail)]
            } else {
                nil()
            };
            // If the body renders as multi-line, always break to next line
            // to avoid half the expression dangling on the signature line.
            let body_str = body.to_pretty_string();
            if body_str.contains('\n') {
                docs.push(nest(2, docvec![line(), body, trail_doc]));
            } else {
                docs.push(group(docvec![nest(
                    2,
                    docvec![break_("", " "), body, trail_doc]
                ),]));
            }
        }
        stmts => {
            // Multiple expressions, or single with leading comments — emit on new lines
            if let Some(trail) = &method.comments.trailing {
                docs.push(Document::Str("  "));
                docs.push(unparse_comment(trail));
            }
            let mut body_docs: Vec<Document<'static>> = Vec::new();
            for stmt in stmts {
                // Emit an extra blank line before statements that had one in source
                if stmt.preceding_blank_line {
                    // Use a raw newline for blank lines to avoid trailing whitespace
                    // from indentation on empty lines.
                    body_docs.push(line());
                }
                body_docs.push(line());
                body_docs.extend(unparse_comment_attachment_leading(&stmt.comments));
                body_docs.push(unparse_expression(&stmt.expression));
                if let Some(trail) = &stmt.comments.trailing {
                    body_docs.push(Document::Str("  "));
                    body_docs.push(unparse_comment(trail));
                }
            }
            docs.push(nest(2, concat(body_docs)));
        }
    }

    concat(docs)
}

/// Builds the method signature (selector + parameters + return type + arrow).
///
/// Delegates the selector/parameters/return-type portion to
/// [`unparse_method_display_signature_doc`] and adds the declaration-only
/// wrapping: `sealed `/`internal ` prefixes and the trailing ` =>`.
fn unparse_method_signature(method: &MethodDefinition) -> Document<'static> {
    let sealed = if method.is_sealed {
        Document::Str("sealed ")
    } else {
        nil()
    };

    let internal = if method.is_internal {
        Document::Str("internal ")
    } else {
        nil()
    };

    docvec![
        internal,
        sealed,
        unparse_method_display_signature_doc(method),
        " =>"
    ]
}

pub(super) fn unparse_keyword_part(part: &KeywordPart) -> Document<'static> {
    leaf::ident(&part.keyword)
}

/// Builds a [`Document`] for a [`StateDeclaration`].
///
/// Emits non-doc comments, doc comment, then `state: name [: Type] [= default]`.
#[must_use]
pub(crate) fn unparse_state_declaration(state: &StateDeclaration) -> Document<'static> {
    unparse_state_declaration_inner(state, false)
}

/// Builds a [`Document`] for a class-variable declaration.
fn unparse_class_state_declaration(state: &StateDeclaration) -> Document<'static> {
    unparse_state_declaration_inner(state, true)
}

fn unparse_state_declaration_inner(state: &StateDeclaration, is_class: bool) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::new();

    // Non-doc leading comments
    docs.extend(unparse_comment_attachment_leading(&state.comments));

    // Blank line between a preserved, earlier `///` block that broke away
    // from a different declaration and this field's own doc
    // comment — see `leading_ends_with_orphaned_doc_comment`.
    if leading_ends_with_orphaned_doc_comment(&state.comments) {
        docs.push(line());
    }

    // Doc comment — emit `///` for empty lines (no trailing space)
    if let Some(doc) = &state.doc_comment {
        for line_text in doc.lines() {
            if line_text.is_empty() {
                docs.push(Document::Str("///"));
            } else {
                docs.push(docvec!["/// ", leaf::raw_text(line_text)]);
            }
            docs.push(line());
        }
    }

    // Emit @expect directive before the declaration
    if let Some((ref cats, ref reason, _)) = state.expect {
        if let Some(reason) = reason {
            docs.push(docvec![
                "@expect ",
                unparse_expect_categories(cats),
                " \"",
                leaf::string_content(reason),
                "\""
            ]);
        } else {
            docs.push(docvec!["@expect ", unparse_expect_categories(cats)]);
        }
        docs.push(line());
    }

    let keyword = if is_class {
        "classState: "
    } else {
        state.declared_keyword.as_str()
    };
    let mut decl: Vec<Document<'static>> = Vec::new();
    // ADR 0124 §1: `late` precedes the declaration keyword, matching the
    // class-header modifier position.
    if state.slot_kind == SlotKind::Late {
        decl.push(Document::Str("late "));
    }
    decl.push(Document::Str(keyword));
    decl.push(leaf::ident(&state.name.name));

    if let Some(ty) = &state.type_annotation {
        decl.push(Document::Str(" :: "));
        decl.push(unparse_type_annotation(ty));
    }

    if let Some(default) = &state.default_value {
        decl.push(Document::Str(" = "));
        decl.push(unparse_expression(default));
    }

    // Trailing comment
    if let Some(trail) = &state.comments.trailing {
        decl.push(Document::Str("  "));
        decl.push(unparse_comment(trail));
    }

    docs.push(concat(decl));
    concat(docs)
}

/// Builds a [`Document`] for a [`ProtocolUse`] — a `uses:` line in a class
/// body (ADR 0127 §2).
///
/// `uses: [package@]ProtocolName[(TypeArgs)] [excluding: #(#sel, …)]
/// [overriding: #(#sel, …)]`. No `aliasing:` clause yet — it is reserved
/// but not stored on this node (not yet supported in v1).
fn unparse_protocol_use(use_line: &ProtocolUse) -> Document<'static> {
    let mut decl: Vec<Document<'static>> = vec![Document::Str("uses: ")];

    if let Some(pkg) = &use_line.package {
        decl.push(leaf::ident(&pkg.name));
        decl.push(Document::Str("@"));
    }
    decl.push(leaf::ident(&use_line.protocol.name));

    if !use_line.type_args.is_empty() {
        decl.push(Document::Str("("));
        for (i, ta) in use_line.type_args.iter().enumerate() {
            if i > 0 {
                decl.push(Document::Str(", "));
            }
            decl.push(unparse_type_annotation(ta));
        }
        decl.push(Document::Str(")"));
    }

    if !use_line.excluding.is_empty() {
        decl.push(Document::Str(" excluding: "));
        decl.push(unparse_selector_symbol_list(&use_line.excluding));
    }

    if !use_line.overriding.is_empty() {
        decl.push(Document::Str(" overriding: "));
        decl.push(unparse_selector_symbol_list(&use_line.overriding));
    }

    concat(decl)
}

/// Builds a [`Document`] for a `#(#sel, #sel2, …)` selector-symbol list —
/// a `uses:` line's `excluding:`/`overriding:` argument (ADR 0127 §2).
///
/// Delegates each entry to [`unparse_literal`] (as `Literal::Symbol`) so the
/// same quoting rule (`#'sel with space'`) applies here as everywhere else a
/// symbol is printed, rather than re-deriving it.
fn unparse_selector_symbol_list(selectors: &[Identifier]) -> Document<'static> {
    let mut parts: Vec<Document<'static>> = vec![Document::Str("#(")];
    for (i, sel) in selectors.iter().enumerate() {
        if i > 0 {
            parts.push(Document::Str(", "));
        }
        parts.push(unparse_literal(&Literal::Symbol(sel.name.clone())));
    }
    parts.push(Document::Str(")"));
    concat(parts)
}

// --- Expect category unparsing ---

fn unparse_expect_category(cat: ExpectCategory) -> Document<'static> {
    Document::Str(cat.as_str())
}

/// Unparses a (possibly multi-category) `@expect` category list as
/// `cat1, cat2, ...`.
pub(super) fn unparse_expect_categories(cats: &[ExpectCategory]) -> Document<'static> {
    let docs: Vec<Document<'static>> = cats.iter().copied().map(unparse_expect_category).collect();
    join_docs(docs, ", ")
}
