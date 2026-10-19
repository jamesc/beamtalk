// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The stdlib's provision-bearing protocols (`Comparable`, `Enumerable`, …),
//! available to every user compilation (ADR 0127 §10a; BT-3915).
//!
//! **DDD Context:** Semantic Analysis
//!
//! The stdlib is not a resolved dependency of a user package, so no compile
//! path carries its protocol ASTs in `pre_loaded_protocol_defs` the way it
//! carries a path dependency's. Instead they are baked into the compiler,
//! exactly like the stdlib's classes (`ClassHierarchy::with_builtins`): `beamtalk
//! build-stdlib` generates `generated_stdlib_protocol_files()` (in
//! `class_hierarchy/generated_builtins.rs`), which `include_str!`s each stdlib
//! file declaring a provision-bearing protocol — the stdlib source itself, not
//! a copy — and this module parses those files once.
//!
//! They are the *last* tier of `uses:` resolution: the current module's own
//! protocols win, then every protocol carried in from elsewhere
//! (`external_protocols` — another file of the package, a dependency), and
//! only then the stdlib's. So a package that declares its own `Comparable`
//! keeps it. The fallback lives in `trait_expansion`'s resolver, the one
//! place every surface (CLI `build`/`test`/`lint`, REPL/workspace, LSP, MCP
//! lint) resolves a `uses:` line, so surfaces cannot disagree. It is off
//! while compiling the stdlib itself ([`is_stdlib_package`]), which carries
//! its own protocols live from `stdlib/src/` rather than from the snapshot
//! baked into the compiler that is building it.

use std::collections::{HashMap, HashSet};
use std::sync::OnceLock;

use ecow::EcoString;

use crate::ast::ProtocolDefinition;
use crate::semantic_analysis::protocol_source::{ProtocolSource, ProtocolSourceMap};
use crate::semantic_analysis::trait_expansion::first_wins_protocol_map;
use crate::source_analysis::Diagnostic;

/// The package every stdlib protocol is declared in — the same identity
/// `build_stdlib.rs` stamps on stdlib classes, aliases and protocol sources.
pub const STDLIB_PACKAGE: &str = "stdlib";

/// Whether `package` is the stdlib itself, which never falls back to the
/// compiler-baked stdlib protocols (see the module doc).
#[must_use]
pub fn is_stdlib_package(package: Option<&str>) -> bool {
    package == Some(STDLIB_PACKAGE)
}

/// Parsed stdlib protocols, built once from the generated file table.
struct StdlibProtocols {
    /// Keyed as [`first_wins_protocol_map`] keys a map: bare name and
    /// `stdlib@Name`.
    map: HashMap<EcoString, ProtocolDefinition>,
    /// Each protocol's source file, by bare name.
    sources: ProtocolSourceMap,
}

fn stdlib_protocols() -> &'static StdlibProtocols {
    static PROTOCOLS: OnceLock<StdlibProtocols> = OnceLock::new();
    PROTOCOLS.get_or_init(|| {
        parse_protocol_files(
            crate::semantic_analysis::class_hierarchy::generated_stdlib_protocol_files(),
        )
    })
}

/// Parses `(path, text)` stdlib files into their provision-bearing protocols,
/// stamped as declared in [`STDLIB_PACKAGE`].
fn parse_protocol_files(files: Vec<(&'static str, &'static str)>) -> StdlibProtocols {
    let mut defs = Vec::new();
    let mut sources = ProtocolSourceMap::new();
    for (path, text) in files {
        let (module, _diagnostics) =
            crate::source_analysis::parse(crate::source_analysis::lex_with_eof(text));
        for mut protocol in module
            .protocols
            .into_iter()
            .filter(crate::semantic_analysis::trait_expansion::is_provision_bearing)
        {
            protocol.package = Some(STDLIB_PACKAGE.into());
            sources.insert(
                protocol.name.name.clone(),
                ProtocolSource {
                    path: Some(path.into()),
                    text: text.into(),
                    package: Some(STDLIB_PACKAGE.into()),
                },
            );
            defs.push(protocol);
        }
    }
    StdlibProtocols {
        map: first_wins_protocol_map(defs),
        sources,
    }
}

/// The stdlib protocols keyed as [`first_wins_protocol_map`] keys a map, for
/// the resolver's last-tier fallback.
pub(crate) fn stdlib_protocol_map() -> &'static HashMap<EcoString, ProtocolDefinition> {
    &stdlib_protocols().map
}

/// Whether `name` (bare) names one of the stdlib's provision-bearing
/// protocols — a name a package's own protocol must be carried under to
/// shadow it ([`is_carried_protocol`](crate::semantic_analysis::trait_expansion::is_carried_protocol)).
#[must_use]
pub fn is_stdlib_protocol_name(name: &str) -> bool {
    stdlib_protocols().sources.contains_key(name)
}

/// The source file of stdlib protocol `name`, if it is one.
#[must_use]
pub fn stdlib_protocol_source(name: &str) -> Option<&'static ProtocolSource> {
    stdlib_protocols().sources.get(name)
}

/// Stamps [`ProvisionOrigin::package`](crate::source_analysis::ProvisionOrigin::package) as the stdlib on every provision
/// diagnostic raised in a class whose `uses:` of that protocol resolved to the
/// stdlib's own (`uses`, from
/// [`trait_expansion::expand_module_with_stdlib_uses`](crate::semantic_analysis::trait_expansion::expand_module_with_stdlib_uses)),
/// so a reporter renders it against the stdlib file and never against a
/// same-named protocol of the user's.
#[allow(clippy::implicit_hasher)] // concrete HashSet, as `AnalysisResult::stdlib_protocol_uses` holds it
pub fn stamp_stdlib_provisions(
    diagnostics: &mut [Diagnostic],
    uses: &HashSet<(EcoString, EcoString)>,
) {
    if uses.is_empty() {
        return;
    }
    for diagnostic in diagnostics {
        let Some(origin) = diagnostic.provision.as_mut() else {
            continue;
        };
        if origin
            .users
            .iter()
            .any(|user| uses.contains(&(user.clone(), origin.protocol.clone())))
        {
            origin.package = Some(STDLIB_PACKAGE.into());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every stdlib provision-bearing protocol, sorted by name.
    fn stdlib_protocol_defs() -> Vec<&'static ProtocolDefinition> {
        let mut defs: Vec<_> = stdlib_protocols()
            .map
            .iter()
            .filter(|(key, def)| **key == def.name.name)
            .map(|(_, def)| def)
            .collect();
        defs.sort_by(|a, b| a.name.name.cmp(&b.name.name));
        defs
    }

    #[test]
    fn carries_comparable_and_enumerable_from_the_stdlib_sources() {
        let names: Vec<&str> = stdlib_protocol_defs()
            .iter()
            .map(|p| p.name.name.as_str())
            .collect();
        assert!(names.contains(&"Comparable"), "{names:?}");
        assert!(names.contains(&"Enumerable"), "{names:?}");
        for def in stdlib_protocol_defs() {
            assert_eq!(def.package.as_deref(), Some(STDLIB_PACKAGE));
            assert!(!def.provided_methods.is_empty());
        }
        let source = stdlib_protocol_source("Enumerable").expect("Enumerable source");
        assert_eq!(source.path.as_deref(), Some("stdlib/src/enumerable.bt"));
        assert!(source.text.contains("Protocol define: Enumerable(E)"));
    }

    /// The generated table names exactly the stdlib files that declare a
    /// provision-bearing protocol, so a new one in `stdlib/src/` cannot be
    /// silently missing from user compilation (and a removed one cannot
    /// linger) until `build-stdlib` regenerates the table.
    #[test]
    fn generated_table_matches_stdlib_sources() {
        let stdlib_src = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../stdlib/src");
        let mut expected = Vec::new();
        for entry in std::fs::read_dir(&stdlib_src).expect("stdlib/src") {
            let path = entry.expect("dir entry").path();
            // `build_stdlib.rs`'s `provision_bearing_protocol_file_names`
            // keys the generated table by file name, which is only unique
            // while `stdlib/src` is flat.
            assert!(
                !path.is_dir(),
                "stdlib/src must stay flat, found directory {}",
                path.display()
            );
            if path.extension().and_then(|e| e.to_str()) != Some("bt") {
                continue;
            }
            let text = std::fs::read_to_string(&path).expect("read stdlib source");
            let (module, _) =
                crate::source_analysis::parse(crate::source_analysis::lex_with_eof(&text));
            if module
                .protocols
                .iter()
                .any(crate::semantic_analysis::trait_expansion::is_provision_bearing)
            {
                let name = path
                    .file_name()
                    .and_then(|n| n.to_str())
                    .expect("file name");
                expected.push(format!("stdlib/src/{name}"));
            }
        }
        expected.sort();
        let generated: Vec<String> =
            crate::semantic_analysis::class_hierarchy::generated_stdlib_protocol_files()
                .into_iter()
                .map(|(path, _)| path.to_string())
                .collect();
        assert_eq!(
            generated, expected,
            "generated_stdlib_protocol_files() is stale — run `just build` (build-stdlib) to \
             regenerate generated_builtins.rs"
        );
    }

    #[test]
    fn stamps_only_provisions_of_stdlib_resolved_uses() {
        let span = crate::source_analysis::Span::default();
        let mut diagnostics = vec![
            Diagnostic::warning("in stdlib provision", span)
                .with_provision_origin("Comparable", "Rank"),
            Diagnostic::warning("in user provision", span)
                .with_provision_origin("Comparable", "Other"),
        ];
        let uses: HashSet<(EcoString, EcoString)> =
            [("Rank".into(), "Comparable".into())].into_iter().collect();
        stamp_stdlib_provisions(&mut diagnostics, &uses);
        assert_eq!(
            diagnostics[0]
                .provision
                .as_ref()
                .and_then(|o| o.package.as_deref()),
            Some(STDLIB_PACKAGE)
        );
        assert_eq!(
            diagnostics[1]
                .provision
                .as_ref()
                .and_then(|o| o.package.as_deref()),
            None
        );
    }
}
