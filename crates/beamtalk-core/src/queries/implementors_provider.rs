// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Implementors provider for the language service (BT-2241).
//!
//! **DDD Context:** Language Service
//!
//! Powers `textDocument/implementation` in cold-file mode: given a selector
//! under the cursor, return every method-definition site (one per implementing
//! class, both instance-side and class-side) so the editor can jump to the
//! method header.
//!
//! # Design
//!
//! Walks every indexed file's `module.classes` and `module.method_definitions`
//! and emits one [`Location`] per `MethodDefinition` whose selector matches.
//! Mirrors `SystemNavigation default implementorsOf:` semantics:
//!
//! * **Local methods only.** Inherited (un-overridden) methods are *not*
//!   reported — only classes with their own definition of the selector.
//! * **Both sides.** A class that defines the selector instance-side and
//!   class-side appears twice (once per `MethodDefinition`).
//! * **Empty-result is a real answer.** When no class defines the selector,
//!   returns an empty vector — the caller decides whether to surface "no
//!   implementations" or fall through to something else.
//!
//! The reported [`Location::span`] is the method definition's full span
//! (matching `find_selector_references` for method definition sites). The
//! LSP layer collapses this to a 0-width range at the start when goto-impl
//! cares only about cursor-landing, but keeping the full span lets callers
//! that want to highlight the whole method (e.g. peek-implementation UIs)
//! pick the right anchor.
//!
//! # Parity with the runtime path
//!
//! The runtime answers the same query via `beamtalk_xref` (the maintained
//! selector→sites index) and emits one row per implementing class with
//! `method`/`line` populated. The cold-file AST walker here is the
//! parity-preserving fallback used when no runtime is attached or the
//! `delegateToRuntime` flag is off — the extension-method case (ADR 0066)
//! is the documented divergence: open-class extensions installed at runtime
//! aren't visible to the AST walker until the file backing the extension
//! is also indexed, which is fine for source-tree extensions but not for
//! REPL-defined ones.
//!
//! # References
//!
//! - BT-2241 (this provider)
//! - BT-2215 (delegate-nav epic), BT-2239 (foundation)
//! - ADR 0066 (open-class extension methods)

use crate::ast::Module;
use crate::language_service::Location;
use camino::Utf8PathBuf;

/// Find every class that locally defines `selector_name`, returning one
/// [`Location`] per `MethodDefinition` (both instance-side and class-side,
/// inside class bodies and in standalone `Class >> method` form).
///
/// Inherited methods are not reported; this matches `SystemNavigation
/// implementorsOf:`. Returns an empty vector when no class defines the
/// selector — callers must not fall through to a "find references"
/// behaviour on empty (per BT-2239's runtime-trust rule, the fallback to
/// AST has already happened by the time we're here).
///
/// The returned `Location::span` is the full `MethodDefinition::span`. LSP
/// callers that want only the header line can collapse to `span.start()`.
pub fn find_implementors<'a>(
    selector_name: &str,
    files: impl IntoIterator<Item = (&'a Utf8PathBuf, &'a Module)>,
) -> Vec<Location> {
    let mut results = Vec::new();

    for (file_path, module) in files {
        // Methods defined inside class bodies (instance + class side).
        for class in &module.classes {
            for method in class.methods.iter().chain(class.class_methods.iter()) {
                if method.selector.name() == selector_name {
                    results.push(Location::new(file_path.clone(), method.span));
                }
            }
        }

        // Standalone `Class >> selector => ...` method definitions.
        for smd in &module.method_definitions {
            if smd.method.selector.name() == selector_name {
                results.push(Location::new(file_path.clone(), smd.method.span));
            }
        }
    }

    results
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::{lex_with_eof, parse};

    fn parse_source(source: &str) -> Module {
        let tokens = lex_with_eof(source);
        parse(tokens).0
    }

    #[test]
    fn finds_single_implementor_in_one_file() {
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source("Object subclass: Foo\n  bar => 1");

        let impls = find_implementors("bar", [(&file, &module)]);
        assert_eq!(impls.len(), 1);
        assert_eq!(impls[0].file, file);
    }

    #[test]
    fn finds_implementors_across_files() {
        let file_a = Utf8PathBuf::from("a.bt");
        let module_a = parse_source("Object subclass: Foo\n  bar => 1");
        let file_b = Utf8PathBuf::from("b.bt");
        let module_b = parse_source("Object subclass: Baz\n  bar => 2");

        let impls = find_implementors("bar", [(&file_a, &module_a), (&file_b, &module_b)]);
        assert_eq!(impls.len(), 2, "expected one site per class, got {impls:?}");
        assert!(impls.iter().any(|r| r.file == file_a));
        assert!(impls.iter().any(|r| r.file == file_b));
    }

    #[test]
    fn skips_call_sites_only_returns_definitions() {
        // `bar` appears as a call site in the second class's method body but
        // that class doesn't define `bar` — only `Foo` does.
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source(
            "Object subclass: Foo\n  bar => 1\n\
             Object subclass: User\n  go => self bar",
        );

        let impls = find_implementors("bar", [(&file, &module)]);
        assert_eq!(
            impls.len(),
            1,
            "expected only Foo's definition, got {impls:?}"
        );
        // Should match the Foo method's span, not the call-site span.
        assert_eq!(impls[0].span, module.classes[0].methods[0].span);
    }

    #[test]
    fn finds_both_instance_and_class_side() {
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source(
            "Object subclass: Counter\n  \
             class new => self basicNew init\n  \
             init => 0",
        );

        let impls = find_implementors("new", [(&file, &module)]);
        // class-side `new` only — `init` is the instance-side method.
        assert_eq!(impls.len(), 1);
        // The class-side method is in class_methods.
        assert_eq!(impls[0].span, module.classes[0].class_methods[0].span);
    }

    #[test]
    fn finds_keyword_selector_implementors() {
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source(
            "Object subclass: Dict\n  at: k put: v => self\n\
             Object subclass: List\n  at: k put: v => self",
        );

        let impls = find_implementors("at:put:", [(&file, &module)]);
        assert_eq!(impls.len(), 2);
    }

    #[test]
    fn returns_empty_when_no_implementors() {
        let file = Utf8PathBuf::from("test.bt");
        let module = parse_source("Object subclass: Foo\n  bar => 1");

        let impls = find_implementors("nonExistent", [(&file, &module)]);
        assert!(impls.is_empty());
    }

    #[test]
    fn finds_standalone_method_definitions() {
        // `Class >> selector` form (open-class extension in source).
        let file = Utf8PathBuf::from("ext.bt");
        let module = parse_source("Integer >> double => self * 2");

        let impls = find_implementors("double", [(&file, &module)]);
        assert_eq!(impls.len(), 1);
        assert_eq!(impls[0].span, module.method_definitions[0].method.span);
    }
}
