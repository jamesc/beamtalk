// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Source identity of a provision-bearing protocol (ADR 0127 §3, "Source
//! locations").
//!
//! **DDD Context:** Semantic Analysis
//!
//! A `uses:` class's flattened provisions keep the *spans* of the protocol
//! file they were parsed from — [`Span`](crate::source_analysis::Span) is
//! byte offsets only, with no file tag. Anything that turns one of those
//! spans into a line (BEAM line annotations, stack traces, per-method source
//! slices) therefore has to map it through the *protocol's* source text, not
//! the using class's. [`ProtocolSource`] is that text plus its path, keyed by
//! protocol name — and, for a protocol with a known package, its
//! [`qualified_protocol_key`] — in a [`ProtocolSourceMap`] that every compile path builds
//! from the same files it already read to obtain the protocol's AST.

use std::collections::HashMap;

use ecow::EcoString;

use crate::semantic_analysis::stdlib_protocols;
use crate::semantic_analysis::trait_expansion::qualified_protocol_key;
use crate::source_analysis::Diagnostic;

/// The source file a provision-bearing protocol was parsed from.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProtocolSource {
    /// Path of the protocol's `.bt` file, when it has one (a REPL-defined
    /// protocol has text but no path).
    pub path: Option<EcoString>,
    /// The full source text of the protocol's file.
    pub text: EcoString,
    /// The package the protocol is declared in, when known (ADR 0127 §3,
    /// "Name resolution"): a provision's free class names resolve there, not
    /// in the using class's package. `None` keeps today's resolution in the
    /// user's package.
    pub package: Option<EcoString>,
}

/// Protocol key → [`ProtocolSource`]. Keyed like
/// [`first_wins_protocol_map`](crate::semantic_analysis::trait_expansion::first_wins_protocol_map):
/// by bare name (first definition wins, so it agrees with a bare `uses:`) and,
/// for a protocol whose [`ProtocolSource::package`] is known, also by
/// [`qualified_protocol_key`], so `uses: dep@Name` finds `dep`'s file even when
/// another package declares a `Name` too (BT-3944). Always fill and merge it
/// with [`insert_protocol_source`] / [`merge_protocol_sources`].
pub type ProtocolSourceMap = HashMap<EcoString, ProtocolSource>;

/// Records `source` for protocol `name` under its bare name and, when its
/// package is known, its [`qualified_protocol_key`]; an existing entry under
/// either key wins (first definition wins). The one place a
/// [`ProtocolSourceMap`] is keyed.
pub fn insert_protocol_source(
    map: &mut ProtocolSourceMap,
    name: &EcoString,
    source: ProtocolSource,
) {
    if let Some(package) = &source.package {
        map.entry(qualified_protocol_key(package, name))
            .or_insert_with(|| source.clone());
    }
    map.entry(name.clone()).or_insert(source);
}

/// Merges `from` into `into`, an existing entry under a key winning —
/// project before dependencies. Both maps are already keyed by
/// [`insert_protocol_source`].
pub fn merge_protocol_sources(into: &mut ProtocolSourceMap, from: ProtocolSourceMap) {
    for (key, source) in from {
        into.entry(key).or_insert(source);
    }
}

/// Collapses the per-user copies of a shared flattened-provision diagnostic
/// (ADR 0127 §3, "Source locations") to one
/// ([`merge_provision_diagnostics`](crate::source_analysis::merge_provision_diagnostics)),
/// so a provision shared by A and B is reported once with a "while flattening
/// into A, B" note. Untagged diagnostics keep their order and come first.
/// Shared by every surface that reports diagnostics per file (CLI `build`/
/// `lint`, MCP `lint`/`diagnostic_summary`).
#[must_use]
pub fn dedupe_provision_diagnostics(diagnostics: Vec<Diagnostic>) -> Vec<Diagnostic> {
    let (tagged, mut rest): (Vec<_>, Vec<_>) =
        diagnostics.into_iter().partition(|d| d.provision.is_some());
    rest.extend(crate::source_analysis::merge_provision_diagnostics(tagged));
    rest
}

/// The file a provision diagnostic is reported against — its spans are
/// offsets into the protocol's file, not the using class's: the protocol's
/// path and text, `<stdlib Name>` (with the stdlib's text) for the stdlib's
/// own protocol, or `<protocol Name>` (no text) when the protocol's source
/// identity is not carried. `None` for a diagnostic not raised in a provision.
#[must_use]
pub fn provision_report_target<'a>(
    diagnostic: &Diagnostic,
    protocol_sources: &'a ProtocolSourceMap,
) -> Option<(String, &'a str)> {
    let origin = diagnostic.provision.as_ref()?;
    // A provision flattened from the stdlib's own protocol (BT-3915, stamped
    // by analysis) is reported against the stdlib's text, never against a
    // same-named carried protocol, and labelled as the stdlib's — its
    // repository path (`stdlib/src/…`) names no file in a user's project.
    if stdlib_protocols::is_stdlib_package(origin.package.as_deref()) {
        let text = stdlib_protocols::stdlib_protocol_source(&origin.protocol)
            .map_or("", |src| src.text.as_str());
        return Some((format!("<stdlib {}>", origin.protocol), text));
    }
    // A provision resolved to another package's protocol (BT-3944) is looked up
    // under its qualified key — the bare name may belong to a same-named
    // protocol of the package being compiled or of an earlier dependency.
    let found = match origin.package.as_deref() {
        Some(package) => protocol_sources.get(&qualified_protocol_key(package, &origin.protocol)),
        None => protocol_sources.get(&origin.protocol),
    };
    Some(match found {
        Some(src) => (
            src.path
                .as_deref()
                .map_or_else(|| format!("<protocol {}>", origin.protocol), str::to_string),
            src.text.as_str(),
        ),
        None => (format!("<protocol {}>", origin.protocol), ""),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::Span;

    /// BT-3915: a provision diagnostic renders against the stdlib file only
    /// when analysis stamped it as resolved to the stdlib's protocol — a
    /// user's own `Comparable` with no carried source never borrows the
    /// stdlib's file, and a stdlib one never borrows a carried same-named one.
    #[test]
    fn provision_diagnostic_uses_the_stdlib_file_only_when_stamped_stdlib() {
        let diag = || {
            Diagnostic::error("in a provision", Span::new(0, 1))
                .with_provision_origin("Comparable", "Rank")
        };
        let mut stamped = diag();
        if let Some(origin) = stamped.provision.as_mut() {
            origin.package = Some("stdlib".into());
        }
        let carried: ProtocolSourceMap = [(
            "Comparable".into(),
            ProtocolSource {
                path: Some("src/comparable.bt".into()),
                text: "Protocol define: Comparable\n".into(),
                package: None,
            },
        )]
        .into_iter()
        .collect();

        let (path, _) = provision_report_target(&diag(), &ProtocolSourceMap::new()).unwrap();
        assert_eq!(path, "<protocol Comparable>");
        let (path, _) = provision_report_target(&diag(), &carried).unwrap();
        assert_eq!(path, "src/comparable.bt");
        let (path, text) = provision_report_target(&stamped, &carried).unwrap();
        assert_eq!(path, "<stdlib Comparable>");
        assert!(text.contains("Protocol define: Comparable"));
        assert!(
            provision_report_target(&Diagnostic::error("plain", Span::new(0, 1)), &carried)
                .is_none()
        );
    }
}
