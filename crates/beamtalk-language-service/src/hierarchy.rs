// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type-hierarchy queries on [`SimpleLanguageService`] (split from `lib.rs`).

use super::{EcoString, Location, Position, SimpleLanguageService, Utf8PathBuf};

impl SimpleLanguageService {
    /// Classify the cursor for a `textDocument/prepareTypeHierarchy`
    /// request and return the class name plus its declaration site, or
    /// `None` when the cursor is not on a known class name.
    ///
    /// "Class name" means any identifier whose token matches a class in the
    /// merged project hierarchy (`ProjectIndex::hierarchy`). That picks up
    /// the class header (`Foo subclass: Bar` — both names), superclass
    /// references in other classes, type annotations, and class-literal
    /// expressions inside method bodies.
    ///
    /// Returns the `(class_name, declaration_location)` pair. The
    /// declaration location is the *class header span* — the name of the
    /// declared class in its own definition (matches `definition_provider`'s
    /// class-name lookup). When no source file backs the class (built-in /
    /// stdlib precompiled, or an injected `add_from_beam_meta` entry), the
    /// location is `None` and consumers fall back to "name only" presentation.
    ///
    /// The classification is selector-aware: clicks on a method selector
    /// token that happens to be capitalised do not match here — selector
    /// classification ([`Self::implementors_query_at`]) already covers
    /// those.
    #[must_use]
    pub fn type_hierarchy_prepare_at(
        &self,
        file: &Utf8PathBuf,
        position: Position,
    ) -> Option<(EcoString, Option<Location>)> {
        // Re-use the identifier walker the references handler uses — it
        // already rejects selectors and parameter names, and returns the
        // identifier *and* its span at the cursor.
        let (ident, _span, _ctx) = self.find_identifier_at_position(file, position)?;
        if !self.project_index.hierarchy().has_class(&ident.name) {
            return None;
        }
        let class_name = ident.name.clone();
        let declaration = self.find_class_declaration_location(class_name.as_str());
        Some((class_name, declaration))
    }

    /// Find the declaration site of `class_name`, scanning every
    /// indexed file for a `ClassDefinition` whose name matches.
    ///
    /// The returned [`Location::span`] is the *class-name span* (e.g. the
    /// `Counter` token in `Actor subclass: Counter`), matching
    /// `definition_provider` and what LSP `selectionRange` / `range`
    /// expects for `TypeHierarchyItem`.
    ///
    /// Returns `None` for builtin / runtime-only classes whose source file
    /// the language service hasn't indexed (the editor still shows the
    /// item via the name returned from
    /// [`Self::type_hierarchy_prepare_at`]; navigation just doesn't have a
    /// jump target).
    ///
    /// Resolution delegates to
    /// [`definition_provider::find_class_or_protocol_declaration`], the same
    /// helper goto-definition uses, so the protocol/class shadowing
    /// rule applies on this path too. When a name is defined as both a real
    /// class and a synthetic protocol (across separate files), the real class
    /// wins; a protocol declaration is only returned when the hierarchy marks
    /// the name as a protocol class — so `prepareTypeHierarchy` on a protocol
    /// name still lands on the protocol header.
    fn find_class_declaration_location(&self, class_name: &str) -> Option<Location> {
        crate::queries::definition_provider::find_class_or_protocol_declaration(
            class_name,
            &self.project_index,
            self.files.iter().map(|(path, data)| (path, &data.module)),
        )
    }

    /// Resolve the supertype chain of `class_name` to declaration
    /// locations. Mirrors `Behaviour superclassChain` (the stdlib query
    /// `typeHierarchy/supertypes` is wired to) — returns the names paired
    /// with their declaration site when one exists in the indexed corpus.
    ///
    /// `class_name` is excluded (consistent with `superclass_chain` itself).
    /// Entries whose source file is not indexed return `(name, None)` so
    /// the LSP layer can still emit a `TypeHierarchyItem` for the name and
    /// let the editor present "no jump target" rather than dropping the row.
    #[must_use]
    pub fn supertypes_of(&self, class_name: &str) -> Vec<(EcoString, Option<Location>)> {
        self.project_index
            .hierarchy()
            .superclass_chain(class_name)
            .into_iter()
            .map(|name| {
                let loc = self.find_class_declaration_location(name.as_str());
                (name, loc)
            })
            .collect()
    }

    /// Resolve all transitive subtypes of `class_name`. Mirrors
    /// `Behaviour allSubclasses` (the stdlib query
    /// `typeHierarchy/subtypes` is wired to) — returns the names paired
    /// with their declaration site when one exists in the indexed corpus.
    ///
    /// Order matches `ClassHierarchy::all_subclasses`: BFS over the
    /// inheritance tree, so direct children precede grandchildren. The
    /// receiver itself is not included.
    #[must_use]
    pub fn subtypes_of(&self, class_name: &str) -> Vec<(EcoString, Option<Location>)> {
        self.project_index
            .hierarchy()
            .all_subclasses(class_name)
            .into_iter()
            .map(|name| {
                let loc = self.find_class_declaration_location(name.as_str());
                (name, loc)
            })
            .collect()
    }
}
