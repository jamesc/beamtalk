// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Static rename planning (`textDocument/prepareRename` and `rename`) on
//! [`SimpleLanguageService`] (BT-5007).
//!
//! **DDD Context:** Language Service
//!
//! Follows the rule ADR 0114 set for live rename, so the editor and the
//! workspace change the same sites:
//!
//! * **Class rename** is complete: the declaration, every reference in the
//!   indexed project, and the file move when the file is named after the
//!   class. A stdlib or dependency class is refused.
//! * **Selector rename** has two tiers. *Confirmed* sites are the definition
//!   plus `self`/`super` sends inside the class or its subclass tree, on the
//!   same side, and only when no class in that subtree overrides the
//!   selector on that side. *Candidate* sites are every other send of the
//!   selector. They are reported, never applied by this module.
//!
//! The planner is pure: it returns spans and replacement text. The LSP layer
//! turns that into a `WorkspaceEdit` (annotating candidates).

use super::{EcoString, MethodDefinition, Position, SimpleLanguageService, Span, Utf8PathBuf};
use crate::STDLIB_PACKAGE_MARKER;
use crate::queries::references_provider::{find_class_declarations, find_class_references};
use crate::queries::selector_rename_query::{
    SelectorSendSite, SendReceiver, find_definition_selector_spans, find_selector_send_sites_in,
    find_symbol_literal_sites,
};
use beamtalk_core::ast::{Expression, ExpressionStatement};
use beamtalk_core::build_layout::{BUILD_DIR_NAME, DEPS_DIR_NAME};
use beamtalk_core::semantic_analysis::CURRENT_PROJECT_PACKAGE_MARKER;
use beamtalk_core::source_analysis::MethodSide;
use std::collections::HashSet;

/// A rename that cannot proceed, with the message to show the user.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RenameRefusal {
    /// Human-readable reason.
    pub message: String,
}

impl RenameRefusal {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

/// What kind of symbol a rename targets.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RenameKind {
    /// A class, protocol or type alias name.
    Class,
    /// A method selector, anchored to one defining class and side.
    Selector,
}

/// Result of `prepareRename`: the token under the cursor and the name to
/// pre-fill the rename box with.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreparedRename {
    /// The token span under the cursor (one keyword part for a keyword selector).
    pub span: Span,
    /// The full current name (the whole selector, e.g. `at:put:`).
    pub placeholder: EcoString,
    /// Class or selector.
    pub kind: RenameKind,
}

/// One text replacement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RenameEdit {
    /// File to edit.
    pub file: Utf8PathBuf,
    /// Byte span replaced.
    pub span: Span,
    /// Replacement text.
    pub new_text: String,
}

/// A file move that accompanies a class rename.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileMove {
    /// Current path.
    pub from: Utf8PathBuf,
    /// Path after the rename.
    pub to: Utf8PathBuf,
}

/// The sites a rename changes, split by ADR 0114's two tiers.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct RenamePlan {
    /// Sites rewritten by default.
    pub confirmed: Vec<RenameEdit>,
    /// Sites that need the user's confirmation, one group per send (a
    /// keyword send has one edit per keyword part).
    pub candidates: Vec<Vec<RenameEdit>>,
    /// File move for a class declared in a file named after it.
    pub file_move: Option<FileMove>,
}

/// One method (or the top-level statements) the walkers visit.
struct BodyCtx<'a> {
    file: &'a Utf8PathBuf,
    source: &'a str,
    /// Enclosing class and side; `None` for top-level statements.
    owner: Option<(&'a str, MethodSide)>,
    /// Every root expression of the method / statement list / default.
    roots: Vec<&'a Expression>,
}

/// A method definition of the target selector.
struct DefSite<'a> {
    file: &'a Utf8PathBuf,
    class: &'a str,
    side: MethodSide,
}

/// Resolved rename anchor.
enum Target {
    Class(EcoString),
    Selector {
        name: EcoString,
        class: EcoString,
        side: MethodSide,
    },
}

fn stmts(body: &[ExpressionStatement]) -> Vec<&Expression> {
    body.iter().map(|s| &s.expression).collect()
}

const fn side_of(is_class_method: bool) -> MethodSide {
    if is_class_method {
        MethodSide::Class
    } else {
        MethodSide::Instance
    }
}

impl SimpleLanguageService {
    fn body_contexts(&self) -> Vec<BodyCtx<'_>> {
        let mut out = Vec::new();
        for (file, data) in &self.files {
            let source = data.source.as_str();
            out.push(BodyCtx {
                file,
                source,
                owner: None,
                roots: stmts(&data.module.expressions),
            });
            for class in &data.module.classes {
                for (methods, side) in [
                    (&class.methods, MethodSide::Instance),
                    (&class.class_methods, MethodSide::Class),
                ] {
                    for m in methods {
                        out.push(BodyCtx {
                            file,
                            source,
                            owner: Some((class.name.name.as_str(), side)),
                            roots: stmts(&m.body),
                        });
                    }
                }
            }
            for smd in &data.module.method_definitions {
                out.push(BodyCtx {
                    file,
                    source,
                    owner: Some((smd.class_name.name.as_str(), side_of(smd.is_class_method))),
                    roots: stmts(&smd.method.body),
                });
            }
            // State / class-variable defaults and protocol provided methods
            // (BT-5079): not a method body of any class, so no owner.
            for expr in data.module.state_default_exprs() {
                out.push(BodyCtx {
                    file,
                    source,
                    owner: None,
                    roots: vec![expr],
                });
            }
            for m in data.module.unused_protocol_provided_methods() {
                out.push(BodyCtx {
                    file,
                    source,
                    owner: None,
                    roots: stmts(&m.body),
                });
            }
        }
        out
    }

    fn definitions_of(&self, selector: &str) -> Vec<DefSite<'_>> {
        let mut out = Vec::new();
        for (file, data) in &self.files {
            for class in &data.module.classes {
                for (methods, side) in [
                    (&class.methods, MethodSide::Instance),
                    (&class.class_methods, MethodSide::Class),
                ] {
                    if methods.iter().any(|m| m.selector.name() == selector) {
                        out.push(DefSite {
                            file,
                            class: class.name.name.as_str(),
                            side,
                        });
                    }
                }
            }
            for smd in &data.module.method_definitions {
                if smd.method.selector.name() == selector {
                    out.push(DefSite {
                        file,
                        class: smd.class_name.name.as_str(),
                        side: side_of(smd.is_class_method),
                    });
                }
            }
        }
        out
    }

    /// `true` for a file the user cannot rename into: stdlib sources and
    /// dependency checkouts (`_build/deps/...`).
    fn is_readonly_file(&self, file: &Utf8PathBuf) -> bool {
        let mut names = file.components().map(|c| c.as_str());
        let in_deps = names.any(|n| n == BUILD_DIR_NAME) && names.any(|n| n == DEPS_DIR_NAME);
        self.project_index.is_stdlib_file(file) || in_deps
    }

    /// Classifies the cursor and returns the rename anchor plus the token span.
    fn resolve_rename_target(
        &self,
        file: &Utf8PathBuf,
        position: Position,
    ) -> Result<(Target, Span, EcoString), RenameRefusal> {
        let nothing =
            || RenameRefusal::new("Only class names and method selectors can be renamed here.");
        let data = self.files.get(file).ok_or_else(nothing)?;
        let offset = position
            .to_byte_offset(&data.source)
            .ok_or_else(nothing)?
            .get();

        let selector = Self::find_selector_at_offset(&data.module, offset)
            .map(|lookup| lookup.selector_name)
            .or_else(|| {
                Self::find_method_header_selector_at_offset(&data.module, &data.source, offset)
            });
        if let Some(selector) = selector {
            return self.resolve_selector_target(file, offset, &selector);
        }

        let (ident, span, _ctx) = self
            .find_identifier_at_position(file, position)
            .ok_or_else(nothing)?;
        let name = ident.name;
        let known = self.project_index.hierarchy().has_class(&name)
            || self.project_index.alias_registry().has_alias(&name);
        if !known {
            return Err(nothing());
        }
        let decls = find_class_declarations(&name, self.files.iter().map(|(p, d)| (p, &d.module)));
        if decls.is_empty() || decls.iter().any(|d| self.is_readonly_file(&d.file)) {
            return Err(RenameRefusal::new(format!(
                "Cannot rename {name}: it is declared in the standard library or a dependency, \
                 not in this project."
            )));
        }
        Ok((Target::Class(name.clone()), span, name))
    }

    fn resolve_selector_target(
        &self,
        file: &Utf8PathBuf,
        offset: u32,
        selector: &EcoString,
    ) -> Result<(Target, Span, EcoString), RenameRefusal> {
        let data = &self.files[file];
        // Token under the cursor: a keyword part of a definition header or of a send.
        let mut hit: Option<Span> = None;
        let mut header_owner: Option<(String, MethodSide)> = None;
        let mut visit_def = |class: &str, side: MethodSide, m: &MethodDefinition| {
            if m.selector.name() != selector.as_str() {
                return;
            }
            if let Ok(spans) =
                find_definition_selector_spans(&data.source, class, selector, selector, side)
                && let Some(s) = spans
                    .iter()
                    .find(|s| s.span.start() <= offset && offset < s.span.end())
            {
                hit = Some(s.span);
                header_owner = Some((class.to_string(), side));
            }
        };
        for class in &data.module.classes {
            for m in &class.methods {
                visit_def(&class.name.name, MethodSide::Instance, m);
            }
            for m in &class.class_methods {
                visit_def(&class.name.name, MethodSide::Class, m);
            }
        }
        for smd in &data.module.method_definitions {
            visit_def(
                &smd.class_name.name,
                side_of(smd.is_class_method),
                &smd.method,
            );
        }

        let mut send_ctx: Option<(Option<(String, MethodSide)>, SendReceiver)> = None;
        if hit.is_none() {
            for ctx in self.body_contexts().into_iter().filter(|c| c.file == file) {
                for site in find_selector_send_sites_in(
                    ctx.roots.iter().copied(),
                    ctx.source,
                    selector,
                    selector,
                ) {
                    if let Some(s) = site
                        .spans
                        .iter()
                        .find(|s| s.span.start() <= offset && offset < s.span.end())
                    {
                        hit = Some(s.span);
                        send_ctx = Some((
                            ctx.owner.map(|(c, side)| (c.to_string(), side)),
                            site.receiver,
                        ));
                    }
                }
            }
        }
        let Some(span) = hit else {
            return Err(RenameRefusal::new(
                "Only class names and method selectors can be renamed here.",
            ));
        };

        let defs = self.definitions_of(selector);
        let (class, side) = if let Some((class, side)) = header_owner {
            (class, side)
        } else {
            let (owner, receiver) = send_ctx.expect("send hit recorded with its context");
            self.pick_send_target(selector, &defs, owner.as_ref(), receiver)?
        };
        let writable = defs
            .iter()
            .any(|d| d.class == class && d.side == side && !self.is_readonly_file(d.file));
        if !writable {
            return Err(RenameRefusal::new(format!(
                "Cannot rename #{selector}: {class} defines it in the standard library or a \
                 dependency, not in this project."
            )));
        }
        Ok((
            Target::Selector {
                name: selector.clone(),
                class: class.into(),
                side,
            },
            span,
            selector.clone(),
        ))
    }

    /// Picks the (class, side) a send's selector belongs to.
    fn pick_send_target(
        &self,
        selector: &str,
        defs: &[DefSite<'_>],
        owner: Option<&(String, MethodSide)>,
        receiver: SendReceiver,
    ) -> Result<(String, MethodSide), RenameRefusal> {
        let hierarchy = self.project_index.hierarchy();
        if let (Some((enclosing, side)), SendReceiver::SelfReceiver | SendReceiver::SuperReceiver) =
            (owner, receiver)
        {
            let mut chain: Vec<EcoString> = Vec::new();
            if receiver == SendReceiver::SelfReceiver {
                chain.push(enclosing.as_str().into());
            }
            chain.extend(hierarchy.superclass_chain(enclosing));
            for candidate in &chain {
                if defs
                    .iter()
                    .any(|d| d.class == candidate.as_str() && d.side == *side)
                {
                    return Ok((candidate.to_string(), *side));
                }
            }
        }
        let mut distinct: Vec<(String, MethodSide)> = Vec::new();
        for d in defs {
            let key = (d.class.to_string(), d.side);
            if !distinct.contains(&key) {
                distinct.push(key);
            }
        }
        match distinct.len() {
            0 => Err(RenameRefusal::new(format!(
                "Cannot rename #{selector}: no definition of it was found in this project."
            ))),
            1 => Ok(distinct.remove(0)),
            _ => {
                let mut names: Vec<String> = distinct.iter().map(|(c, _)| c.clone()).collect();
                names.sort();
                names.dedup();
                Err(RenameRefusal::new(format!(
                    "#{selector} is defined by several classes ({}). Put the cursor on the \
                     method definition you want to rename.",
                    names.join(", ")
                )))
            }
        }
    }

    /// `textDocument/prepareRename`: the token under the cursor, or a refusal.
    ///
    /// # Errors
    ///
    /// A [`RenameRefusal`] when the cursor is not on a project class name or
    /// project-defined selector.
    pub fn prepare_rename(
        &self,
        file: &Utf8PathBuf,
        position: Position,
    ) -> Result<PreparedRename, RenameRefusal> {
        let (target, span, placeholder) = self.resolve_rename_target(file, position)?;
        let kind = match target {
            Target::Class(_) => RenameKind::Class,
            Target::Selector { .. } => RenameKind::Selector,
        };
        Ok(PreparedRename {
            span,
            placeholder,
            kind,
        })
    }

    /// `textDocument/rename`: the sites to change, split by ADR 0114's tiers.
    ///
    /// # Errors
    ///
    /// A [`RenameRefusal`] for an invalid new name, a collision, or a symbol
    /// that is not renameable.
    pub fn plan_rename(
        &self,
        file: &Utf8PathBuf,
        position: Position,
        new_name: &str,
    ) -> Result<RenamePlan, RenameRefusal> {
        let (target, _span, _) = self.resolve_rename_target(file, position)?;
        match target {
            Target::Class(old) => self.plan_class_rename(&old, new_name),
            Target::Selector { name, class, side } => {
                self.plan_selector_rename(&name, &class, side, new_name)
            }
        }
    }

    fn plan_class_rename(&self, old: &str, new: &str) -> Result<RenamePlan, RenameRefusal> {
        let mut chars = new.chars();
        let valid = chars.next().is_some_and(|c| c.is_ascii_uppercase())
            && chars.all(|c| c.is_ascii_alphanumeric() || c == '_');
        if !valid {
            return Err(RenameRefusal::new(format!(
                "'{new}' is not a valid class name: it must start with an uppercase letter and \
                 contain only letters, digits and underscores."
            )));
        }
        if new == old {
            return Err(RenameRefusal::new(
                "The new name is the same as the old one.",
            ));
        }
        let modules = || self.files.iter().map(|(p, d)| (p, &d.module));
        if self.project_index.hierarchy().has_class(new)
            || self.project_index.alias_registry().has_alias(new)
            || !find_class_declarations(new, modules()).is_empty()
        {
            return Err(RenameRefusal::new(format!(
                "Cannot rename {old} to {new}: {new} already exists."
            )));
        }

        let own_package = self.declaring_package(old);
        let mut seen = HashSet::new();
        let mut confirmed = Vec::new();
        for loc in find_class_references(old, modules()) {
            if !seen.insert((loc.file.clone(), loc.span.start(), loc.span.end())) {
                continue;
            }
            // `pkg@Name` names `Name` in package `pkg`. It refers to this
            // class only when `pkg` is the package that declares it; any
            // other qualifier (`http@Json` when renaming the project's
            // `Json`) is a different class and stays untouched. When the
            // declaring package is unknown the qualifier cannot be proven to
            // match, so it is left alone too.
            if let Some(qualifier) = self.qualifier_of(&loc, old)
                && own_package.as_deref() != Some(qualifier)
            {
                continue;
            }
            // A site we cannot rewrite would leave a dangling reference that
            // fails later with `class_not_found`, so refuse the whole rename.
            let unresolved = |why: &str| {
                RenameRefusal::new(format!(
                    "Cannot rename {old} to {new}: a reference in {} {why}, so it cannot be \
                     rewritten safely. Nothing was changed.",
                    loc.file
                ))
            };
            if self.is_readonly_file(&loc.file) {
                return Err(unresolved("is in the standard library or a dependency"));
            }
            // Narrow to the bare name: a qualified `pkg@Name` reference may span more.
            let rel = self
                .files
                .get(&loc.file)
                .and_then(|data| {
                    data.source
                        .get(loc.span.start() as usize..loc.span.end() as usize)
                })
                .and_then(|text| whole_word_offset(text, old))
                .ok_or_else(|| unresolved("could not be narrowed to the bare class name"))?;
            let start = loc.span.start() + u32::try_from(rel).unwrap_or(0);
            confirmed.push(RenameEdit {
                file: loc.file,
                span: Span::new(start, start + u32::try_from(old.len()).unwrap_or(0)),
                new_text: new.to_string(),
            });
        }
        confirmed.sort_by(|a, b| (&a.file, a.span.start()).cmp(&(&b.file, b.span.start())));

        let file_move = find_class_declarations(old, modules())
            .into_iter()
            .find(|d| d.file.file_stem() == Some(old))
            .and_then(|d| {
                let to = d.file.with_file_name(format!("{new}.bt"));
                (d.file.extension() == Some("bt")).then_some(FileMove { from: d.file, to })
            });
        if let Some(mv) = &file_move
            && (self.files.contains_key(&mv.to) || mv.to.exists())
        {
            return Err(RenameRefusal::new(format!(
                "Cannot rename {old} to {new}: the file {} already exists.",
                mv.to
            )));
        }
        Ok(RenamePlan {
            confirmed,
            candidates: Vec::new(),
            file_move,
        })
    }

    /// The real package name of the project file that declares `class`, or
    /// `None` when no workspace root names it (the same-project marker).
    fn declaring_package(&self, class: &str) -> Option<EcoString> {
        let decls = find_class_declarations(class, self.files.iter().map(|(p, d)| (p, &d.module)));
        let package = self
            .project_index
            .alias_package_for_file(&decls.first()?.file);
        (package != CURRENT_PROJECT_PACKAGE_MARKER && package != STDLIB_PACKAGE_MARKER)
            .then_some(package)
    }

    /// The `pkg` of a `pkg@Name` reference at `loc`, read from the source text
    /// just before the name.
    fn qualifier_of(&self, loc: &crate::Location, name: &str) -> Option<&str> {
        let text = &self.files.get(&loc.file)?.source;
        let end = text.get(..loc.span.end() as usize)?;
        let name_start = end.len().checked_sub(name.len())?;
        let before = end.get(..name_start)?.strip_suffix('@')?;
        let start = before
            .rfind(|c: char| !(c.is_alphanumeric() || c == '_'))
            .map_or(0, |i| i + 1);
        let q = &before[start..];
        (!q.is_empty()).then_some(q)
    }

    fn plan_selector_rename(
        &self,
        old: &str,
        class: &str,
        side: MethodSide,
        new: &str,
    ) -> Result<RenamePlan, RenameRefusal> {
        validate_new_selector(old, new)?;
        let defs = self.definitions_of(old);
        let hierarchy = self.project_index.hierarchy();
        let defines = |c: &str, selector: &str| -> bool {
            hierarchy.get_class(c).is_some_and(|info| {
                let methods = match side {
                    MethodSide::Instance => &info.methods,
                    MethodSide::Class => &info.class_methods,
                };
                methods.iter().any(|m| m.selector == selector)
            }) || self
                .definitions_of(selector)
                .iter()
                .any(|d| d.class == c && d.side == side)
        };
        let ancestors = hierarchy.superclass_chain(class);
        let descendants = hierarchy.all_subclasses(class);

        // An ancestor already defines `old`: this method overrides it, and a
        // rename would detach the override (callers still reach the ancestor).
        if let Some(anc) = ancestors.iter().find(|a| defines(a, old)) {
            return Err(RenameRefusal::new(format!(
                "Cannot rename #{old}: {class} overrides #{old} from {anc}, and renaming only \
                 the override would detach it from {anc}'s callers. Rename it on {anc} instead."
            )));
        }
        // The new name is already taken on this class, an ancestor or a
        // descendant: the rename would create or hide an override.
        if let Some(other) = std::iter::once(&EcoString::from(class))
            .chain(ancestors.iter())
            .chain(descendants.iter())
            .find(|c| defines(c, new))
        {
            return Err(RenameRefusal::new(format!(
                "Cannot rename #{old} to #{new}: {other} already defines #{new}, so the rename \
                 would create or hide an override."
            )));
        }

        let mut subtree: HashSet<String> = descendants.iter().map(ToString::to_string).collect();
        subtree.insert(class.to_string());
        let overridden = defs
            .iter()
            .any(|d| d.side == side && d.class != class && subtree.contains(d.class));

        let mut plan = RenamePlan::default();
        // The definition itself.
        let def_file = defs
            .iter()
            .find(|d| d.class == class && d.side == side)
            .map(|d| d.file)
            .ok_or_else(|| {
                RenameRefusal::new(format!("No definition of #{old} found on {class}."))
            })?;
        let def_source = &self.files[def_file].source;
        let spans = find_definition_selector_spans(def_source, class, old, new, side)
            .map_err(|e| RenameRefusal::new(format!("Cannot rename #{old} on {class}: {e}")))?;
        plan.confirmed.extend(spans.into_iter().map(|s| RenameEdit {
            file: def_file.clone(),
            span: s.span,
            new_text: s.new_text,
        }));

        let mut contexts = self.body_contexts();
        contexts.sort_by(|a, b| a.file.cmp(b.file));
        for ctx in contexts {
            for SelectorSendSite { receiver, spans } in
                find_selector_send_sites_in(ctx.roots.iter().copied(), ctx.source, old, new)
            {
                let edits: Vec<RenameEdit> = spans
                    .into_iter()
                    .map(|s| RenameEdit {
                        file: ctx.file.clone(),
                        span: s.span,
                        new_text: s.new_text,
                    })
                    .collect();
                let is_confirmed = !overridden
                    && receiver != SendReceiver::Other
                    && ctx
                        .owner
                        .is_some_and(|(c, s)| s == side && subtree.contains(c));
                if is_confirmed {
                    plan.confirmed.extend(edits);
                } else {
                    plan.candidates.push(edits);
                }
            }
        }
        // `#old` literals (`perform: #old`, `respondsTo: #old`, ...): a dynamic
        // use cannot be proven to name this method, so candidates only.
        for ctx in self.body_contexts() {
            for span in find_symbol_literal_sites(ctx.roots.iter().copied(), ctx.source, old) {
                plan.candidates.push(vec![RenameEdit {
                    file: ctx.file.clone(),
                    span,
                    new_text: format!("#{new}"),
                }]);
            }
        }
        plan.confirmed
            .sort_by(|a, b| (&a.file, a.span.start()).cmp(&(&b.file, b.span.start())));
        plan.candidates
            .sort_by(|a, b| (&a[0].file, a[0].span.start()).cmp(&(&b[0].file, b[0].span.start())));
        Ok(plan)
    }
}

/// Byte offset of `name` in `text` when it occurs exactly once as a whole word.
fn whole_word_offset(text: &str, name: &str) -> Option<usize> {
    let is_word = |c: char| c.is_alphanumeric() || c == '_';
    let mut found = None;
    for (idx, _) in text.match_indices(name) {
        let before_ok = !text[..idx].chars().next_back().is_some_and(is_word);
        let after_ok = !text[idx + name.len()..].chars().next().is_some_and(is_word);
        if before_ok && after_ok {
            if found.is_some() {
                return None;
            }
            found = Some(idx);
        }
    }
    found
}

/// The new selector must have the same shape and arity as the old one.
fn validate_new_selector(old: &str, new: &str) -> Result<(), RenameRefusal> {
    if new == old {
        return Err(RenameRefusal::new(
            "The new name is the same as the old one.",
        ));
    }
    let ident = |s: &str| {
        let mut cs = s.chars();
        cs.next()
            .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
            && cs.all(|c| c.is_ascii_alphanumeric() || c == '_')
    };
    let bad = |why: &str| {
        Err(RenameRefusal::new(format!(
            "'{new}' is not a valid replacement for #{old}: {why}."
        )))
    };
    if old.contains(':') {
        let parts: Vec<&str> = new.split_inclusive(':').collect();
        let well_formed = new.ends_with(':')
            && parts
                .iter()
                .all(|p| p.ends_with(':') && ident(&p[..p.len() - 1]));
        if !well_formed {
            return bad("a keyword selector needs parts like `name:`");
        }
        if parts.len() != old.matches(':').count() {
            return bad("it must keep the same number of keyword parts");
        }
    } else if old
        .chars()
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
    {
        if !ident(new) {
            return bad("a unary selector must be a plain identifier");
        }
    } else if new.is_empty() || !new.chars().all(|c| "+-*/\\<>=~@%|&?,".contains(c)) {
        return bad("a binary selector must be made of operator characters");
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::whole_word_offset;

    #[test]
    fn whole_word_offset_requires_exactly_one_whole_word_match() {
        assert_eq!(whole_word_offset("Foo", "Foo"), Some(0));
        assert_eq!(whole_word_offset("pkg@Foo", "Foo"), Some(4));
        assert_eq!(whole_word_offset("Foo@Foo", "Foo"), None);
        assert_eq!(whole_word_offset("FooBar", "Foo"), None);
        assert_eq!(whole_word_offset("Bar", "Foo"), None);
    }
}
