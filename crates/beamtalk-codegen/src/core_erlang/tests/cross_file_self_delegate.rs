// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4930: what `Foo >> sel => self delegate` compiles to on a `native:`
//! class: the inline method and the same-file extension are both lowered
//! through the backing module. (A cross-file `>>` is a compile error, ADR 0152.)

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
fn native_class_meta_carries_the_native_flag_and_others_do_not() {
    // `decode.rs` reads this key back so a compiled dependency's class is
    // native for the validators. Non-native classes emit no key at all.
    let native = compile(HOST, None);
    assert!(native.contains("'native' => 'true'"), "{native}");
    let plain = compile("Value subclass: Plain\n  ping -> Integer => 1\n", None);
    assert!(!plain.contains("'native' =>"), "{plain}");
}
