// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Curated export table for commonly-used Erlang/OTP modules (BT-4371).
//!
//! **DDD Context:** Compilation — shared leaf
//!
//! The single source of truth for the Erlang functions Beamtalk knows the
//! arities of without a runtime connection. Two consumers read it:
//!
//! - the `arity_mismatch` lint (`semantic_analysis::validators::structural_validators`),
//!   which warns when a listed `(module, function)` is called with an arity
//!   that is not listed — so **every listed function must list every real
//!   arity**, or a valid call is flagged;
//! - `Erlang <module> ` completions in `beamtalk-language-service`
//!   (`queries::erlang_modules`), which offer each listed export as a
//!   Beamtalk selector.
//!
//! Completeness is enforced by the `every_listed_function_has_all_otp_arities`
//! test against `tests/fixtures/otp_curated_exports.txt`, a checked-in dump of
//! `Module:module_info(exports)` on the OTP release pinned in `.tool-versions`
//! (regenerate with `scripts/gen-otp-curated-exports.escript`). Adding a
//! function here means adding all of its arities; adding a module means
//! regenerating the fixture. Arities are those of the pinned release; an
//! arity added in a later OTP release needs a fixture regeneration (and the
//! `fixture_was_generated_on_the_pinned_otp_major` test forces one when
//! `.tool-versions` moves).

/// An Erlang module with its curated exports.
#[derive(Debug)]
pub struct ErlangModuleInfo {
    /// Module name (e.g., "lists").
    pub name: &'static str,
    /// Brief description of the module.
    pub description: &'static str,
    /// Exported functions as `(name, arity)` pairs, sorted by name then
    /// arity. Every arity of each listed function that the pinned OTP
    /// release exports is present.
    pub exports: &'static [(&'static str, u8)],
}

/// The curated Erlang modules and their exports.
pub const CURATED_MODULES: &[ErlangModuleInfo] = &[
    ErlangModuleInfo {
        name: "lists",
        description: "List processing functions",
        exports: &[
            ("append", 1),
            ("append", 2),
            ("concat", 1),
            ("delete", 2),
            ("droplast", 1),
            ("dropwhile", 2),
            ("duplicate", 2),
            ("filter", 2),
            ("filtermap", 2),
            ("flatlength", 1),
            ("flatmap", 2),
            ("flatten", 1),
            ("flatten", 2),
            ("foldl", 3),
            ("foldr", 3),
            ("foreach", 2),
            ("join", 2),
            ("keydelete", 3),
            ("keyfind", 3),
            ("keymember", 3),
            ("keysort", 2),
            ("keystore", 4),
            ("last", 1),
            ("map", 2),
            ("mapfoldl", 3),
            ("max", 1),
            ("member", 2),
            ("min", 1),
            ("nth", 2),
            ("nthtail", 2),
            ("partition", 2),
            ("reverse", 1),
            ("reverse", 2),
            ("search", 2),
            ("seq", 2),
            ("seq", 3),
            ("sort", 1),
            ("sort", 2),
            ("split", 2),
            ("splitwith", 2),
            ("sublist", 2),
            ("sublist", 3),
            ("subtract", 2),
            ("suffix", 2),
            ("sum", 1),
            ("takewhile", 2),
            ("uniq", 1),
            ("uniq", 2),
            ("unzip", 1),
            ("zip", 2),
            ("zip", 3),
            ("zipwith", 3),
            ("zipwith", 4),
        ],
    },
    ErlangModuleInfo {
        name: "maps",
        description: "Map (dictionary) operations",
        exports: &[
            ("filter", 2),
            ("filtermap", 2),
            ("find", 2),
            ("fold", 3),
            ("foreach", 2),
            ("from_keys", 2),
            ("from_list", 1),
            ("get", 2),
            ("get", 3),
            ("is_key", 2),
            ("keys", 1),
            ("map", 2),
            ("merge", 2),
            ("new", 0),
            ("put", 3),
            ("remove", 2),
            ("size", 1),
            ("take", 2),
            ("to_list", 1),
            ("update", 3),
            ("values", 1),
            ("with", 2),
            ("without", 2),
        ],
    },
    ErlangModuleInfo {
        name: "erlang",
        description: "Erlang BIFs (built-in functions)",
        exports: &[
            ("abs", 1),
            ("atom_to_binary", 1),
            ("atom_to_binary", 2),
            ("atom_to_list", 1),
            ("binary_to_atom", 1),
            ("binary_to_atom", 2),
            ("binary_to_integer", 1),
            ("binary_to_integer", 2),
            ("binary_to_list", 1),
            ("binary_to_list", 3),
            ("binary_to_term", 1),
            ("binary_to_term", 2),
            ("display", 1),
            ("element", 2),
            ("error", 1),
            ("error", 2),
            ("error", 3),
            ("exit", 1),
            ("exit", 2),
            ("exit", 3),
            ("float_to_binary", 1),
            ("float_to_binary", 2),
            ("hd", 1),
            ("integer_to_binary", 1),
            ("integer_to_binary", 2),
            ("integer_to_list", 1),
            ("integer_to_list", 2),
            ("is_atom", 1),
            ("is_binary", 1),
            ("is_boolean", 1),
            ("is_float", 1),
            ("is_integer", 1),
            ("is_list", 1),
            ("is_map", 1),
            ("is_number", 1),
            ("is_pid", 1),
            ("is_tuple", 1),
            ("length", 1),
            ("list_to_atom", 1),
            ("list_to_binary", 1),
            ("list_to_tuple", 1),
            ("make_ref", 0),
            ("max", 2),
            ("min", 2),
            ("node", 0),
            ("node", 1),
            ("now", 0),
            ("round", 1),
            ("self", 0),
            ("setelement", 3),
            ("size", 1),
            ("spawn", 1),
            ("spawn", 2),
            ("spawn", 3),
            ("spawn", 4),
            ("term_to_binary", 1),
            ("term_to_binary", 2),
            ("throw", 1),
            ("tl", 1),
            ("trunc", 1),
            ("tuple_size", 1),
            ("tuple_to_list", 1),
        ],
    },
    ErlangModuleInfo {
        name: "string",
        description: "Unicode string processing",
        exports: &[
            ("casefold", 1),
            ("chomp", 1),
            ("equal", 2),
            ("equal", 3),
            ("equal", 4),
            ("find", 2),
            ("find", 3),
            ("is_empty", 1),
            ("join", 2),
            ("length", 1),
            ("lexemes", 2),
            ("lowercase", 1),
            ("pad", 2),
            ("pad", 3),
            ("pad", 4),
            ("prefix", 2),
            ("replace", 3),
            ("replace", 4),
            ("reverse", 1),
            ("slice", 2),
            ("slice", 3),
            ("split", 2),
            ("split", 3),
            ("titlecase", 1),
            ("to_float", 1),
            ("to_integer", 1),
            ("trim", 1),
            ("trim", 2),
            ("trim", 3),
            ("uppercase", 1),
        ],
    },
    ErlangModuleInfo {
        name: "io",
        description: "Standard I/O operations",
        exports: &[
            ("format", 1),
            ("format", 2),
            ("format", 3),
            ("fwrite", 1),
            ("fwrite", 2),
            ("fwrite", 3),
            ("get_line", 1),
            ("get_line", 2),
            ("nl", 0),
            ("nl", 1),
            ("put_chars", 1),
            ("put_chars", 2),
            ("read", 1),
            ("read", 2),
            ("read", 3),
            ("read", 4),
            ("write", 1),
            ("write", 2),
        ],
    },
    ErlangModuleInfo {
        name: "io_lib",
        description: "I/O formatting library",
        exports: &[
            ("format", 2),
            ("format", 3),
            ("fwrite", 2),
            ("fwrite", 3),
            ("print", 1),
            ("print", 4),
            ("write", 1),
            ("write", 2),
            ("write", 3),
            ("write", 5),
        ],
    },
    ErlangModuleInfo {
        name: "math",
        description: "Mathematical functions",
        exports: &[
            ("acos", 1),
            ("asin", 1),
            ("atan", 1),
            ("atan2", 2),
            ("ceil", 1),
            ("cos", 1),
            ("exp", 1),
            ("floor", 1),
            ("fmod", 2),
            ("log", 1),
            ("log10", 1),
            ("log2", 1),
            ("pi", 0),
            ("pow", 2),
            ("sin", 1),
            ("sqrt", 1),
            ("tan", 1),
        ],
    },
    ErlangModuleInfo {
        name: "timer",
        description: "Timer and delay functions",
        exports: &[
            ("apply_after", 2),
            ("apply_after", 3),
            ("apply_after", 4),
            ("cancel", 1),
            ("exit_after", 2),
            ("exit_after", 3),
            ("hms", 3),
            ("hours", 1),
            ("minutes", 1),
            ("now_diff", 2),
            ("seconds", 1),
            ("send_after", 2),
            ("send_after", 3),
            ("sleep", 1),
            ("tc", 1),
            ("tc", 2),
            ("tc", 3),
            ("tc", 4),
        ],
    },
    ErlangModuleInfo {
        name: "file",
        description: "File I/O operations",
        exports: &[
            ("close", 1),
            ("consult", 1),
            ("copy", 2),
            ("copy", 3),
            ("del_dir", 1),
            ("delete", 1),
            ("delete", 2),
            ("list_dir", 1),
            ("make_dir", 1),
            ("open", 2),
            ("read", 2),
            ("read_file", 1),
            ("read_file", 2),
            ("rename", 2),
            ("write", 2),
            ("write_file", 2),
            ("write_file", 3),
        ],
    },
    ErlangModuleInfo {
        name: "ets",
        description: "Erlang Term Storage",
        exports: &[
            ("delete", 1),
            ("delete", 2),
            ("insert", 2),
            ("lookup", 2),
            ("match", 1),
            ("match", 2),
            ("match", 3),
            ("member", 2),
            ("new", 2),
            ("select", 1),
            ("select", 2),
            ("select", 3),
            ("tab2list", 1),
        ],
    },
    ErlangModuleInfo {
        name: "gen_server",
        description: "Generic server behaviour",
        exports: &[
            ("call", 2),
            ("call", 3),
            ("cast", 2),
            ("reply", 2),
            ("start", 3),
            ("start", 4),
            ("start_link", 3),
            ("start_link", 4),
            ("stop", 1),
            ("stop", 3),
        ],
    },
    ErlangModuleInfo {
        name: "proplists",
        description: "Property list functions",
        exports: &[
            ("delete", 2),
            ("get_all_values", 2),
            ("get_value", 2),
            ("get_value", 3),
            ("is_defined", 2),
            ("lookup", 2),
        ],
    },
    ErlangModuleInfo {
        name: "rand",
        description: "Random number generation",
        exports: &[
            ("normal", 0),
            ("normal", 2),
            ("seed", 1),
            ("seed", 2),
            ("uniform", 0),
            ("uniform", 1),
        ],
    },
    ErlangModuleInfo {
        name: "os",
        description: "Operating system interface",
        exports: &[
            ("cmd", 1),
            ("cmd", 2),
            ("getenv", 0),
            ("getenv", 1),
            ("getenv", 2),
            ("putenv", 2),
            ("system_time", 0),
            ("system_time", 1),
            ("timestamp", 0),
            ("type", 0),
        ],
    },
    ErlangModuleInfo {
        name: "binary",
        description: "Binary data processing",
        exports: &[
            ("at", 2),
            ("bin_to_list", 1),
            ("bin_to_list", 2),
            ("bin_to_list", 3),
            ("copy", 1),
            ("copy", 2),
            ("decode_unsigned", 1),
            ("decode_unsigned", 2),
            ("encode_unsigned", 1),
            ("encode_unsigned", 2),
            ("first", 1),
            ("last", 1),
            ("list_to_bin", 1),
            ("match", 2),
            ("match", 3),
            ("part", 2),
            ("part", 3),
            ("replace", 3),
            ("replace", 4),
            ("split", 2),
            ("split", 3),
        ],
    },
    ErlangModuleInfo {
        name: "re",
        description: "Regular expressions (PCRE)",
        exports: &[("compile", 1), ("compile", 2), ("run", 2), ("run", 3)],
    },
    ErlangModuleInfo {
        name: "json",
        description: "JSON encoding and decoding",
        exports: &[("decode", 1), ("decode", 3), ("encode", 1), ("encode", 2)],
    },
];

/// Looks up a curated Erlang module by name.
#[must_use]
pub fn find_module(name: &str) -> Option<&'static ErlangModuleInfo> {
    CURATED_MODULES.iter().find(|m| m.name == name)
}

/// The listed arities of `module:function`, ascending.
///
/// Empty when the function (or module) is not curated — callers must treat
/// that as "unknown", not "no valid arity".
#[must_use]
pub fn known_arities(module: &str, function: &str) -> Vec<u8> {
    find_module(module)
        .map(|m| {
            m.exports
                .iter()
                .filter(|(f, _)| *f == function)
                .map(|&(_, a)| a)
                .collect()
        })
        .unwrap_or_default()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::{BTreeMap, BTreeSet};

    /// `Module:module_info(exports)` on the pinned OTP release, for every
    /// curated module. See the module docs for how to regenerate it.
    const OTP_EXPORTS: &str = include_str!("../tests/fixtures/otp_curated_exports.txt");

    type Exports = BTreeMap<&'static str, BTreeMap<&'static str, BTreeSet<u8>>>;

    fn otp_exports() -> Exports {
        let mut out: Exports = BTreeMap::new();
        for line in OTP_EXPORTS.lines() {
            if line.starts_with('#') || line.trim().is_empty() {
                continue;
            }
            let mut parts = line.split_whitespace();
            let (Some(m), Some(f), Some(a), None) =
                (parts.next(), parts.next(), parts.next(), parts.next())
            else {
                panic!("malformed fixture line: {line:?}");
            };
            let a: u8 = a.parse().expect("arity is a small integer");
            out.entry(m).or_default().entry(f).or_default().insert(a);
        }
        out
    }

    fn regenerate_hint() -> String {
        let mut mods: Vec<&str> = CURATED_MODULES.iter().map(|m| m.name).collect();
        mods.sort_unstable();
        format!(
            "regenerate on the OTP release in .tool-versions with:\n  \
             escript scripts/gen-otp-curated-exports.escript {} \
             > crates/beamtalk-core/tests/fixtures/otp_curated_exports.txt",
            mods.join(" ")
        )
    }

    #[test]
    fn fixture_covers_exactly_the_curated_modules() {
        let fixture: BTreeSet<&str> = otp_exports().keys().copied().collect();
        let curated: BTreeSet<&str> = CURATED_MODULES.iter().map(|m| m.name).collect();
        assert_eq!(fixture, curated, "{}", regenerate_hint());
    }

    #[test]
    fn every_listed_function_has_all_otp_arities() {
        let otp = otp_exports();
        let mut problems = Vec::new();
        for module in CURATED_MODULES {
            let Some(real) = otp.get(module.name) else {
                problems.push(format!("{}: not in fixture", module.name));
                continue;
            };
            let mut listed: BTreeMap<&str, BTreeSet<u8>> = BTreeMap::new();
            for &(f, a) in module.exports {
                listed.entry(f).or_default().insert(a);
            }
            for (f, arities) in &listed {
                match real.get(f) {
                    None => problems.push(format!("{}:{f} is not exported", module.name)),
                    Some(real_arities) if real_arities != arities => problems.push(format!(
                        "{}:{f} lists arities {arities:?}, OTP exports {real_arities:?}",
                        module.name
                    )),
                    Some(_) => {}
                }
            }
        }
        assert!(
            problems.is_empty(),
            "curated FFI exports disagree with OTP (every listed function must \
             list every real arity, or `arity_mismatch` flags valid calls):\n  {}",
            problems.join("\n  ")
        );
    }

    /// OTP major in the fixture's `# OTP <major> exports ...` header.
    fn fixture_otp_major(text: &str) -> &str {
        text.lines()
            .next()
            .and_then(|l| l.strip_prefix("# OTP "))
            .and_then(|l| l.split_whitespace().next())
            .expect("fixture header is `# OTP <major> exports ...`")
    }

    #[test]
    fn fixture_was_generated_on_the_pinned_otp_major() {
        // The fixture's arities are those of one OTP release; it must be the
        // one `.tool-versions` pins, so a toolchain bump forces regeneration.
        let tool_versions = include_str!("../../../.tool-versions");
        let pinned = tool_versions
            .lines()
            .find_map(|l| l.strip_prefix("erlang "))
            .and_then(|v| v.trim().split('.').next())
            .expect(".tool-versions pins `erlang <version>`");
        assert_eq!(
            fixture_otp_major(OTP_EXPORTS),
            pinned,
            "fixture OTP major differs from .tool-versions; {}",
            regenerate_hint()
        );
    }

    #[test]
    fn fixture_matches_live_otp_when_available() {
        // Guards against a hand-edited or stale fixture: regenerate it with
        // the installed OTP and compare. Skipped when no working `escript` is on PATH
        // or the installed OTP major differs from the fixture's (e.g. a CI
        // leg on an older supported release).
        let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let mut mods: Vec<&str> = CURATED_MODULES.iter().map(|m| m.name).collect();
        mods.sort_unstable();
        let Ok(output) = std::process::Command::new("escript")
            .arg(root.join("scripts/gen-otp-curated-exports.escript"))
            .args(&mods)
            .output()
        else {
            eprintln!("skipping: escript not available");
            return;
        };
        if !output.status.success() {
            // e.g. a mise/asdf shim with no Erlang installed behind it.
            eprintln!(
                "skipping: escript could not run the generator: {}",
                String::from_utf8_lossy(&output.stderr)
            );
            return;
        }
        let live = String::from_utf8(output.stdout).expect("generator output is UTF-8");
        if fixture_otp_major(&live) != fixture_otp_major(OTP_EXPORTS) {
            eprintln!("skipping: installed OTP differs from the fixture's");
            return;
        }
        // Line-wise, so a CRLF checkout of the fixture (Windows) still matches.
        assert!(
            live.lines().eq(OTP_EXPORTS.lines()),
            "fixture differs from live OTP exports; {}",
            regenerate_hint()
        );
    }

    #[test]
    fn exports_are_sorted_and_unique() {
        for module in CURATED_MODULES {
            assert!(
                module.exports.windows(2).all(|w| w[0] < w[1]),
                "{}: exports must be sorted by (name, arity) without duplicates",
                module.name
            );
        }
        let names: BTreeSet<&str> = CURATED_MODULES.iter().map(|m| m.name).collect();
        assert_eq!(names.len(), CURATED_MODULES.len(), "duplicate module");
    }

    #[test]
    fn known_arities_lists_every_arity() {
        assert_eq!(known_arities("os", "getenv"), vec![0, 1, 2]);
        assert_eq!(known_arities("string", "trim"), vec![1, 2, 3]);
        assert_eq!(known_arities("lists", "flatten"), vec![1, 2]);
        assert!(known_arities("lists", "no_such_function").is_empty());
        assert!(known_arities("no_such_module", "f").is_empty());
    }
}
