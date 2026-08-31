// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Static export data for common Erlang/OTP modules.
//!
//! **DDD Context:** Language Service
//!
//! Provides completion data for `Erlang <module>` expressions in the editor.
//! Since the LSP has no runtime connection to query `module_info(exports)`,
//! we maintain a curated list of commonly-used OTP module exports.
//!
//! ## Selector Conversion
//!
//! Erlang function/arity pairs are converted to Beamtalk keyword selectors:
//! - `reverse/1` → `reverse:`
//! - `seq/2` → `seq:with:`
//! - `seq/3` → `seq:with:with:`
//! - `node/0` → `node` (nullary)

/// An Erlang module with its commonly-used exports.
pub(crate) struct ErlangModuleInfo {
    /// Module name (e.g., "lists").
    pub name: &'static str,
    /// Brief description of the module.
    pub description: &'static str,
    /// Exported functions as (name, arity) pairs.
    pub exports: &'static [(&'static str, u8)],
}

/// Converts an Erlang function name and arity to a Beamtalk keyword selector.
///
/// # Examples
/// - `("reverse", 1)` → `"reverse:"`
/// - `("seq", 2)` → `"seq:with:"`
/// - `("node", 0)` → `"node"`
#[must_use]
pub(crate) fn export_to_selector(name: &str, arity: u8) -> String {
    match arity {
        0 => name.to_string(),
        1 => format!("{name}:"),
        _ => {
            let mut selector = format!("{name}:");
            for _ in 1..arity {
                selector.push_str("with:");
            }
            selector
        }
    }
}

/// Returns the detail string for an Erlang export completion.
#[must_use]
pub(crate) fn export_detail(module: &str, name: &str, arity: u8) -> String {
    format!("{module}:{name}/{arity}")
}

/// Common Erlang module names suggested when typing `Erlang `.
pub(crate) const COMMON_MODULES: &[ErlangModuleInfo] = &[
    ErlangModuleInfo {
        name: "lists",
        description: "List processing functions",
        exports: &[
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
            ("unzip", 1),
            ("zip", 2),
            ("zipwith", 3),
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
            ("atom_to_list", 1),
            ("binary_to_atom", 1),
            ("binary_to_integer", 1),
            ("binary_to_list", 1),
            ("binary_to_term", 1),
            ("display", 1),
            ("element", 2),
            ("error", 1),
            ("exit", 1),
            ("float_to_binary", 1),
            ("hd", 1),
            ("integer_to_binary", 1),
            ("integer_to_list", 1),
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
            ("now", 0),
            ("round", 1),
            ("self", 0),
            ("setelement", 3),
            ("size", 1),
            ("spawn", 1),
            ("term_to_binary", 1),
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
            ("find", 2),
            ("is_empty", 1),
            ("join", 2),
            ("length", 1),
            ("lexemes", 2),
            ("lowercase", 1),
            ("pad", 2),
            ("pad", 3),
            ("prefix", 2),
            ("replace", 3),
            ("reverse", 1),
            ("slice", 2),
            ("slice", 3),
            ("split", 2),
            ("titlecase", 1),
            ("to_integer", 1),
            ("to_float", 1),
            ("trim", 1),
            ("trim", 2),
            ("uppercase", 1),
        ],
    },
    ErlangModuleInfo {
        name: "io",
        description: "Standard I/O operations",
        exports: &[
            ("format", 1),
            ("format", 2),
            ("fwrite", 1),
            ("fwrite", 2),
            ("get_line", 1),
            ("nl", 0),
            ("put_chars", 1),
            ("read", 1),
            ("write", 1),
        ],
    },
    ErlangModuleInfo {
        name: "io_lib",
        description: "I/O formatting library",
        exports: &[("format", 2), ("fwrite", 2), ("print", 1), ("write", 1)],
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
            ("log2", 1),
            ("log10", 1),
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
            ("apply_after", 4),
            ("cancel", 1),
            ("exit_after", 2),
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
        ],
    },
    ErlangModuleInfo {
        name: "file",
        description: "File I/O operations",
        exports: &[
            ("close", 1),
            ("consult", 1),
            ("copy", 2),
            ("del_dir", 1),
            ("delete", 1),
            ("list_dir", 1),
            ("make_dir", 1),
            ("open", 2),
            ("read", 2),
            ("read_file", 1),
            ("rename", 2),
            ("write", 2),
            ("write_file", 2),
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
            ("match", 2),
            ("member", 2),
            ("new", 2),
            ("select", 2),
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
        exports: &[("normal", 0), ("seed", 1), ("uniform", 0), ("uniform", 1)],
    },
    ErlangModuleInfo {
        name: "os",
        description: "Operating system interface",
        exports: &[
            ("cmd", 1),
            ("getenv", 0),
            ("getenv", 1),
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
            ("copy", 1),
            ("copy", 2),
            ("decode_unsigned", 1),
            ("encode_unsigned", 1),
            ("first", 1),
            ("last", 1),
            ("list_to_bin", 1),
            ("match", 2),
            ("part", 2),
            ("part", 3),
            ("replace", 3),
            ("split", 2),
        ],
    },
];

/// Look up an Erlang module by name and return its info.
#[must_use]
pub(crate) fn find_module(name: &str) -> Option<&'static ErlangModuleInfo> {
    COMMON_MODULES.iter().find(|m| m.name == name)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn export_to_selector_unary() {
        assert_eq!(export_to_selector("node", 0), "node");
    }

    #[test]
    fn export_to_selector_one_arg() {
        assert_eq!(export_to_selector("reverse", 1), "reverse:");
    }

    #[test]
    fn export_to_selector_two_args() {
        assert_eq!(export_to_selector("seq", 2), "seq:with:");
    }

    #[test]
    fn export_to_selector_three_args() {
        assert_eq!(export_to_selector("seq", 3), "seq:with:with:");
    }

    #[test]
    fn find_module_exists() {
        assert!(find_module("lists").is_some());
        assert_eq!(find_module("lists").unwrap().name, "lists");
    }

    #[test]
    fn find_module_not_found() {
        assert!(find_module("nonexistent_module").is_none());
    }

    #[test]
    fn export_detail_format() {
        assert_eq!(export_detail("lists", "reverse", 1), "lists:reverse/1");
    }
}
