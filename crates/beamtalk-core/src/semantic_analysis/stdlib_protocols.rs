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

use std::collections::HashMap;
use std::sync::OnceLock;

use ecow::EcoString;

use crate::ast::ProtocolDefinition;
use crate::semantic_analysis::protocol_source::{ProtocolSource, ProtocolSourceMap};
use crate::semantic_analysis::trait_expansion::first_wins_protocol_map;

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
            .filter(|p| !p.provided_methods.is_empty())
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

/// Every stdlib provision-bearing protocol, sorted by name.
#[must_use]
pub fn stdlib_protocol_defs() -> Vec<&'static ProtocolDefinition> {
    let mut defs: Vec<_> = stdlib_protocols()
        .map
        .iter()
        .filter(|(key, def)| **key == def.name.name)
        .map(|(_, def)| def)
        .collect();
    defs.sort_by(|a, b| a.name.name.cmp(&b.name.name));
    defs
}

/// The source file of stdlib protocol `name`, if it is one.
#[must_use]
pub fn stdlib_protocol_source(name: &str) -> Option<&'static ProtocolSource> {
    stdlib_protocols().sources.get(name)
}

/// Adds the source of every stdlib protocol to `sources` that neither
/// `sources` nor `module_protocols` (the module being compiled) already
/// declares — the protocols a `uses:` there resolves to the stdlib's.
///
/// Codegen maps a flattened provision's spans through this map (ADR 0127 §3,
/// "Source locations"); a protocol of the module's own file has no entry, as
/// its spans are already the module's, so a same-named local protocol must not
/// pick up the stdlib file.
pub fn seed_stdlib_protocol_sources(
    sources: &mut ProtocolSourceMap,
    module_protocols: &[ProtocolDefinition],
) {
    for (name, source) in &stdlib_protocols().sources {
        if sources.contains_key(name) || module_protocols.iter().any(|p| &p.name.name == name) {
            continue;
        }
        sources.insert(name.clone(), source.clone());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
            if path.extension().and_then(|e| e.to_str()) != Some("bt") {
                continue;
            }
            let text = std::fs::read_to_string(&path).expect("read stdlib source");
            let (module, _) =
                crate::source_analysis::parse(crate::source_analysis::lex_with_eof(&text));
            if module
                .protocols
                .iter()
                .any(|p| !p.provided_methods.is_empty())
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
    fn seeding_skips_carried_and_module_local_protocols() {
        let mut sources = ProtocolSourceMap::new();
        sources.insert(
            "Enumerable".into(),
            ProtocolSource {
                path: Some("dep/enumerable.bt".into()),
                text: "".into(),
                package: Some("dep".into()),
            },
        );
        let (module, _) = crate::source_analysis::parse(crate::source_analysis::lex_with_eof(
            "Protocol define: Comparable\n  < other -> Boolean\n",
        ));
        seed_stdlib_protocol_sources(&mut sources, &module.protocols);
        assert_eq!(
            sources["Enumerable"].path.as_deref(),
            Some("dep/enumerable.bt"),
            "a carried protocol keeps its own source"
        );
        assert!(
            !sources.contains_key("Comparable"),
            "a module-local protocol maps through the module's own text"
        );
    }
}
