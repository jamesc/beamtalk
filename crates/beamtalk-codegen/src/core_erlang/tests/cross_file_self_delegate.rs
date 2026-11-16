// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4930: what `Foo >> sel => self delegate` compiles to on a `native:`
//! class, for the three ways the method can be written.
//!
//! The inline method and the same-file extension are lowered through the
//! backing module. The cross-file extension is a foreign extension fun in which
//! `self delegate` is a plain send. That is why semantic analysis rejects the
//! cross-file form with an error (`check_cross_file_native_delegate`), so this
//! shape is never built from a program that compiles; the test pins what
//! codegen does if it is reached, and the validator's reasoning with it.

use std::collections::HashMap;

use beamtalk_core::semantic_analysis::ClassHierarchy;

const PACKAGE: &str = "xf";
const HOST: &str = "Value subclass: Host native: host_backing\n  \
                    inl -> Integer => self delegate\n";
const NATIVE_CALL: &str = "'beamtalk_erlang_proxy':'native_call'('host_backing'";

fn parse(src: &str) -> beamtalk_core::ast::Module {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    module
}

/// Compile `src` as `bt@xf@file`; `host_src` (if any) is another file of the
/// package that declares `Host`, handed over as a package build does.
fn compile(src: &str, host_src: Option<&str>) -> String {
    let mut class_module_index = HashMap::new();
    let mut class_superclass_index = HashMap::new();
    let mut opts = crate::core_erlang::CodegenOptions::new(&format!("bt@{PACKAGE}@file"));
    if let Some(host_src) = host_src {
        let host = parse(host_src);
        let mut infos = ClassHierarchy::extract_class_infos(&host);
        ClassHierarchy::stamp_package_on_infos(&mut infos, PACKAGE);
        class_module_index.insert("Host".to_string(), format!("bt@{PACKAGE}@host"));
        class_superclass_index.insert("Host".to_string(), "Value".to_string());
        opts = opts
            .with_class_module_index(class_module_index)
            .with_class_superclass_index(class_superclass_index)
            .with_class_hierarchy(infos);
    }
    crate::core_erlang::generate_module_with_warnings(&parse(src), opts)
        .expect("codegen should succeed")
        .code
}

#[test]
fn inline_method_calls_the_backing_module() {
    let code = compile(HOST, None);
    assert!(code.contains(NATIVE_CALL), "{code}");
    assert!(code.contains("'inl'"), "{code}");
}

#[test]
fn same_file_extension_calls_the_backing_module() {
    let code = compile(
        &format!("{HOST}Host >> same -> Integer => self delegate\n"),
        None,
    );
    assert!(
        code.contains(&format!("{NATIVE_CALL}, 'same', [Self]")),
        "{code}"
    );
}

#[test]
fn cross_file_extension_is_a_plain_send_of_delegate() {
    // Codegen contract the BT-4930 error relies on: no `native_call` to `host_backing`; `delegate` is sent
    // to the receiver (`bt@xf@host:dispatch('delegate', [], Self)` when it is
    // a Host, else `beamtalk_message_dispatch:send`). At run time that reaches
    // the `Object>>delegate` sentinel, which raises `user_error`
    // "delegate called on a non-native Object".
    let code = compile("Host >> cross -> Integer => self delegate\n", Some(HOST));
    assert!(
        code.contains("'beamtalk_extensions':'register'('Host', 'cross'"),
        "{code}"
    );
    assert!(!code.contains(NATIVE_CALL), "{code}");
    assert!(code.contains("'delegate', []"), "{code}");
}
