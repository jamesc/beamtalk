// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The one Core Erlang module skeleton every generated module is built from.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! [`core_module`] emits `module 'M' [Exports] attributes [Attrs] Body end`
//! and is the single place the predefined `module_info/0,1` functions are
//! exported and defined (BT-4067). `erlc` adds them for Erlang sources in
//! `v3_core` (`erl_internal:add_predefined_functions/1`), a pass that works
//! on abstract forms and so never runs for `from_core` input; no compile
//! option adds them for `.core`. Without them OTP tools that call
//! `M:module_info/0,1` (`eprof`, `tprof`, `fprof`, `xref`, `cover`) crash on
//! any Beamtalk module.
//!
//! Every module generator (class modules, stdlib, packages, REPL/workspace
//! eval and test modules) routes through [`core_module`], so no generator can
//! omit the functions or emit them twice.

use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf::{atom, fname};
use beamtalk_cerl_doc::{Document, leaf};
// The predefined function's name. Semantic analysis rejects a Beamtalk
// selector that would compile to a second `module_info/1`
// (`check_reserved_module_info`), so the name is shared from there.
use beamtalk_core::semantic_analysis::validators::MODULE_INFO_FUNCTION as MODULE_INFO;

/// Builds a complete Core Erlang module:
///
/// ```text
/// module 'M' [<exports>, 'module_info'/0, 'module_info'/1]
///   attributes [<attributes>]
/// <body>
/// 'module_info'/0 = fun () -> call 'erlang':'get_module_info'('M')
/// 'module_info'/1 = fun (Key) -> call 'erlang':'get_module_info'('M', Key)
/// end
/// ```
///
/// `exports` must be non-empty (every generated module exports at least one
/// function of its own); `attributes` is the comma-separated attribute list
/// without the surrounding brackets, possibly empty.
#[must_use]
pub fn core_module(
    module_name: impl Into<String>,
    exports: Document<'static>,
    attributes: Document<'static>,
    body: Document<'static>,
) -> Document<'static> {
    let module_name = module_name.into();
    docvec![
        "module ",
        atom(module_name.clone()),
        " [",
        exports,
        ", ",
        fname(MODULE_INFO, 0),
        ", ",
        fname(MODULE_INFO, 1),
        "]\n",
        "  attributes [",
        attributes,
        "]\n",
        body,
        module_info_functions(module_name),
        "end\n",
    ]
}

/// The bodies `erlc` gives `module_info/0,1` for Erlang sources.
fn module_info_functions(module_name: String) -> Document<'static> {
    docvec![
        fname(MODULE_INFO, 0),
        " = fun () ->\n    call 'erlang':'get_module_info'(",
        atom(module_name.clone()),
        ")\n",
        fname(MODULE_INFO, 1),
        " = fun (",
        leaf::var("Key"),
        ") ->\n    call 'erlang':'get_module_info'(",
        atom(module_name),
        ", ",
        leaf::var("Key"),
        ")\n",
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn core_module_exports_and_defines_module_info() {
        let doc = core_module(
            "bt@m",
            Document::Str("'f'/0"),
            Document::Nil,
            Document::Str("'f'/0 = fun () -> 'ok'\n"),
        );
        let code = doc.to_pretty_string();
        assert!(
            code.starts_with("module 'bt@m' ['f'/0, 'module_info'/0, 'module_info'/1]\n"),
            "{code}"
        );
        assert!(code.contains("  attributes []\n"), "{code}");
        assert!(
            code.contains(
                "'module_info'/0 = fun () ->\n    call 'erlang':'get_module_info'('bt@m')"
            ),
            "{code}"
        );
        assert!(
            code.contains(
                "'module_info'/1 = fun (Key) ->\n    call 'erlang':'get_module_info'('bt@m', Key)"
            ),
            "{code}"
        );
        assert!(code.ends_with("end\n"), "{code}");
    }
}
