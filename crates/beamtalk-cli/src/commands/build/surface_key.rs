// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Per-file incremental-build keys over the cross-file analysis surface
//! (BT-4690, phase 2 of BT-4586).
//!
//! A file's analysis and codegen record the [`ConsultedSurface`] they read
//! (class, protocol and alias names looked up, and whole-store scans). The key
//! of an unchanged file is then a hash of *just that slice* of the current
//! surface, so a signature change rebuilds the files that consulted it instead
//! of the whole package. [`SurfaceKeys`] holds the per-name hashes, computed
//! once per build from the same per-item values `util::analysis_surface_hash`
//! hashes, so the two cannot drift apart.
//!
//! Soundness rules (when in doubt a file keys on the full hash):
//! - a file with no stored [`ConsultedSurface`] (first build, an old or
//!   unreadable sidecar) keys on the full package-wide hash;
//! - so does a file that read extension state while dependency extensions were
//!   not all loaded (`extensions_unknown`);
//! - a name's hash covers the class entry, the protocol and alias of that name,
//!   the standalone extensions targeting it, and its entries in the superclass
//!   and module indexes, and is closed over its superclass chain (a class's
//!   normalised flags are derived from its ancestors);
//! - a name absent from the surface hashes as a fixed "absent" value, so a
//!   consulted name that is added later, or removed, changes the key;
//! - a scan of a store keys on a digest of that whole store.

use beamtalk_core::semantic_analysis::consulted::ConsultedSurface;
use ecow::EcoString;
use std::collections::{BTreeMap, BTreeSet, HashMap};

use crate::commands::util::{
    alias_surface_value, extension_surface_value, protocol_surface_value, sha256_hex,
    signature_only_class_info,
};

/// What a name hashes to when nothing on the surface carries it.
const ABSENT: &str = "absent";

/// Per-name and per-store hashes of the cross-file analysis surface.
#[derive(Debug, Default, Clone)]
pub(crate) struct SurfaceKeys {
    /// Hash per bare name, over every namespace and the superclass chain.
    names: HashMap<EcoString, String>,
    /// Digest of every class, extension and index entry (a class-map scan).
    classes_digest: String,
    /// Digest of every protocol (a protocol-registry scan).
    protocols_digest: String,
    /// Digest of every type alias (an alias-registry scan).
    aliases_digest: String,
    /// Package-wide inputs no name addresses (the dependency-extension guard,
    /// the project-known Erlang modules); folded into every file's key.
    pub(crate) globals: String,
}

/// Serialised parts of a surface, grouped by the bare name they are about.
#[derive(Default)]
struct Parts {
    class: BTreeMap<EcoString, Vec<String>>,
    protocol: BTreeMap<EcoString, Vec<String>>,
    alias: BTreeMap<EcoString, Vec<String>>,
}

impl SurfaceKeys {
    /// Hash the surface `Pass 2` type-checks against. `None` when a part of it
    /// cannot be serialised: the caller then keys every file on the full hash.
    #[allow(clippy::too_many_lines)] // collect parts, then hash them: one linear pass
    pub(crate) fn build(
        class_infos: &[beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo],
        protocol_infos: &[beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo],
        alias_infos: &[beamtalk_core::semantic_analysis::AliasInfo],
        extension_index: &beamtalk_core::compilation::extension_index::ExtensionIndex,
        class_superclass_index: &HashMap<String, String>,
        class_module_index: &HashMap<String, String>,
    ) -> Option<Self> {
        let mut parts = Parts::default();
        let mut superclass_of: HashMap<EcoString, EcoString> = HashMap::new();

        for info in class_infos {
            let value = serde_json::to_value(signature_only_class_info(info)).ok()?;
            parts
                .class
                .entry(info.name.clone())
                .or_default()
                .push(format!("class:{value}"));
            if let Some(sup) = &info.superclass {
                superclass_of
                    .entry(info.name.clone())
                    .or_insert_with(|| sup.clone());
            }
        }
        for (name, sup) in class_superclass_index {
            parts
                .class
                .entry(name.as_str().into())
                .or_default()
                .push(format!("superclass:{sup}"));
            superclass_of
                .entry(name.as_str().into())
                .or_insert_with(|| sup.as_str().into());
        }
        for (name, module) in class_module_index {
            parts
                .class
                .entry(name.as_str().into())
                .or_default()
                .push(format!("module:{module}"));
        }
        for (key, locations) in extension_index.entries() {
            for loc in locations {
                parts
                    .class
                    .entry(key.class_name.clone())
                    .or_default()
                    .push(format!("extension:{}", extension_surface_value(key, loc)));
            }
        }
        for info in protocol_infos {
            parts
                .protocol
                .entry(info.name.clone())
                .or_default()
                .push(protocol_surface_value(info).to_string());
        }
        for info in alias_infos {
            parts
                .alias
                .entry(info.name.clone())
                .or_default()
                .push(alias_surface_value(info).to_string());
        }
        // Several entries can share a name (two packages): order must not matter.
        for list in parts
            .class
            .values_mut()
            .chain(parts.protocol.values_mut())
            .chain(parts.alias.values_mut())
        {
            list.sort_unstable();
        }

        let digest = |map: &BTreeMap<EcoString, Vec<String>>| {
            let mut buf = String::new();
            for (name, list) in map {
                buf.push_str(name);
                buf.push('\n');
                buf.push_str(&list.join("\n"));
                buf.push('\u{1}');
            }
            sha256_hex(buf.as_bytes())
        };
        let own_part = |map: &BTreeMap<EcoString, Vec<String>>, name: &str| {
            map.get(name).map(|list| list.join("\n"))
        };

        let all_names: BTreeSet<&EcoString> = parts
            .class
            .keys()
            .chain(parts.protocol.keys())
            .chain(parts.alias.keys())
            .collect();
        let mut names = HashMap::with_capacity(all_names.len());
        for name in all_names {
            let mut buf = String::new();
            // The class part, closed over the superclass chain: a class's
            // normalised flags and inherited surface come from its ancestors.
            let mut seen: BTreeSet<&str> = BTreeSet::new();
            let mut cursor: Option<&str> = Some(name.as_str());
            while let Some(current) = cursor {
                if !seen.insert(current) {
                    break; // a cycle in the declared hierarchy
                }
                buf.push_str("class:");
                buf.push_str(current);
                buf.push('=');
                buf.push_str(&own_part(&parts.class, current).unwrap_or_else(|| ABSENT.into()));
                buf.push('\u{1}');
                cursor = superclass_of.get(current).map(EcoString::as_str);
            }
            buf.push_str("protocol=");
            buf.push_str(&own_part(&parts.protocol, name).unwrap_or_else(|| ABSENT.into()));
            buf.push_str("\u{1}alias=");
            buf.push_str(&own_part(&parts.alias, name).unwrap_or_else(|| ABSENT.into()));
            names.insert(name.clone(), sha256_hex(buf.as_bytes()));
        }

        Some(Self {
            names,
            classes_digest: digest(&parts.class),
            protocols_digest: digest(&parts.protocol),
            aliases_digest: digest(&parts.alias),
            globals: String::new(),
        })
    }

    /// The hash of `name`'s slice of the surface; a fixed value if it is absent.
    fn name_hash(&self, name: &str) -> &str {
        self.names.get(name).map_or(ABSENT, String::as_str)
    }

    /// The text a file that consulted `surface` folds into its cache key, or
    /// `None` when it must key on the full package-wide hash instead.
    pub(crate) fn key_part(&self, surface: &ConsultedSurface) -> Option<String> {
        if surface.extensions_unknown {
            return None;
        }
        let mut buf = String::from("names\n");
        buf.push_str(&self.globals);
        if surface.scanned_classes {
            buf.push_str("\nclasses:");
            buf.push_str(&self.classes_digest);
        }
        if surface.scanned_protocols {
            buf.push_str("\nprotocols:");
            buf.push_str(&self.protocols_digest);
        }
        if surface.scanned_aliases {
            buf.push_str("\naliases:");
            buf.push_str(&self.aliases_digest);
        }
        // `names` is a sorted set, so the order is stable.
        for name in &surface.names {
            buf.push('\n');
            buf.push_str(name);
            buf.push('=');
            buf.push_str(self.name_hash(name));
        }
        Some(sha256_hex(buf.as_bytes()))
    }
}

/// The sidecar key stashing a file's consulted surface next to its beam hash
/// (the way `own_hash_key` stashes the own-content hash). `\0` can never
/// appear in a real path, so this cannot collide with one.
pub(crate) fn consulted_key(source_file: &camino::Utf8Path) -> String {
    format!("{source_file}\u{0}consulted")
}

/// Encode a consulted surface for the sidecar.
pub(crate) fn encode_surface(surface: &ConsultedSurface) -> String {
    serde_json::json!({
        "n": surface.names.iter().map(EcoString::as_str).collect::<Vec<_>>(),
        "c": surface.scanned_classes,
        "p": surface.scanned_protocols,
        "a": surface.scanned_aliases,
        "x": surface.extensions_unknown,
    })
    .to_string()
}

/// Decode a sidecar entry; `None` for anything unreadable, which makes the
/// file key on the full hash.
pub(crate) fn decode_surface(text: &str) -> Option<ConsultedSurface> {
    let value: serde_json::Value = serde_json::from_str(text).ok()?;
    let flag = |k: &str| value.get(k).and_then(serde_json::Value::as_bool);
    Some(ConsultedSurface {
        names: value
            .get("n")?
            .as_array()?
            .iter()
            .map(|n| n.as_str().map(EcoString::from))
            .collect::<Option<_>>()?,
        scanned_classes: flag("c")?,
        scanned_protocols: flag("p")?,
        scanned_aliases: flag("a")?,
        extensions_unknown: flag("x")?,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn surface(names: &[&str]) -> ConsultedSurface {
        ConsultedSurface {
            names: names.iter().map(|n| EcoString::from(*n)).collect(),
            ..ConsultedSurface::default()
        }
    }

    #[test]
    fn a_surface_survives_the_sidecar_round_trip() {
        let original = ConsultedSurface {
            names: ["A", "B"].iter().map(|n| EcoString::from(*n)).collect(),
            scanned_classes: true,
            scanned_protocols: false,
            scanned_aliases: true,
            extensions_unknown: false,
        };
        assert_eq!(decode_surface(&encode_surface(&original)), Some(original));
    }

    #[test]
    fn an_unreadable_sidecar_entry_decodes_to_nothing() {
        for text in ["", "not json", "{}", "{\"n\":[1],\"c\":false}", "[]"] {
            assert_eq!(decode_surface(text), None, "{text:?}");
        }
    }

    #[test]
    fn extensions_unknown_falls_back_to_the_full_hash() {
        let keys = SurfaceKeys::default();
        let mut s = surface(&["A"]);
        assert!(keys.key_part(&s).is_some());
        s.extensions_unknown = true;
        assert_eq!(keys.key_part(&s), None);
    }

    #[test]
    fn the_key_covers_exactly_the_names_and_scans_a_file_consulted() {
        let keys = SurfaceKeys::default();
        let base = keys.key_part(&surface(&["A"])).unwrap();
        assert_eq!(keys.key_part(&surface(&["A"])).unwrap(), base);
        assert_ne!(keys.key_part(&surface(&["A", "B"])).unwrap(), base);
        for scan in [
            ConsultedSurface {
                scanned_classes: true,
                ..surface(&["A"])
            },
            ConsultedSurface {
                scanned_protocols: true,
                ..surface(&["A"])
            },
            ConsultedSurface {
                scanned_aliases: true,
                ..surface(&["A"])
            },
        ] {
            assert_ne!(keys.key_part(&scan).unwrap(), base, "{scan:?}");
        }
    }

    #[test]
    fn package_wide_globals_are_part_of_every_key() {
        let mut keys = SurfaceKeys::default();
        let before = keys.key_part(&surface(&["A"])).unwrap();
        keys.globals = "erlang:1:lists".into();
        assert_ne!(keys.key_part(&surface(&["A"])).unwrap(), before);
    }

    #[test]
    fn a_name_present_on_the_surface_no_longer_hashes_as_absent() {
        let absent = SurfaceKeys::default();
        let mut present = SurfaceKeys::default();
        present.names.insert("A".into(), "deadbeef".into());
        assert_ne!(
            absent.key_part(&surface(&["A"])),
            present.key_part(&surface(&["A"]))
        );
        // A name the file did not consult does not move its key.
        assert_eq!(
            absent.key_part(&surface(&["B"])),
            present.key_part(&surface(&["B"]))
        );
    }
}
