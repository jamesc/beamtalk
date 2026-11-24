// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `ProjectIndex::file_interface` change detection (BT-5131): what a file
//! exports — classes, type aliases, protocols — decides whether other open
//! files must be re-checked.

use super::common::*;
use crate::FileInterface;

fn interface_after(first: &str, second: &str) -> (Option<FileInterface>, Option<FileInterface>) {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("a.bt");
    service.update_file(file.clone(), first.to_string());
    let before = service.project_index().file_interface(&file);
    service.update_file(file.clone(), second.to_string());
    (before, service.project_index().file_interface(&file))
}

#[test]
fn alias_expansion_change_changes_interface() {
    let (before, after) = interface_after("type Id = Integer\n", "type Id = String\n");
    assert_ne!(before, after);
}

#[test]
fn alias_added_changes_interface() {
    let (before, after) = interface_after(
        "Object subclass: Foo\n  go => 1\n",
        "type Id = Integer\nObject subclass: Foo\n  go => 1\n",
    );
    assert_ne!(before, after);
}

#[test]
fn alias_moved_without_change_keeps_interface() {
    let (before, after) = interface_after("type Id = Integer\n", "\n\n\ntype Id = Integer\n");
    assert_eq!(before, after);
}

#[test]
fn protocol_requirement_change_changes_interface() {
    let (before, after) = interface_after(
        "Protocol define: Printable\n  asString -> String",
        "Protocol define: Printable\n  asString -> Integer",
    );
    assert_ne!(before, after);
}

#[test]
fn protocol_added_requirement_changes_interface() {
    let (before, after) = interface_after(
        "Protocol define: Printable\n  asString -> String",
        "Protocol define: Printable\n  asString -> String\n  show -> String",
    );
    assert_ne!(before, after);
}

#[test]
fn protocol_moved_without_change_keeps_interface() {
    let (before, after) = interface_after(
        "Protocol define: Printable\n  asString -> String",
        "\n\nProtocol define: Printable\n  asString -> String",
    );
    assert_eq!(before, after);
}

#[test]
fn method_body_edit_keeps_interface() {
    let (before, after) = interface_after(
        "Object subclass: Foo\n  go => 1\n",
        "Object subclass: Foo\n  go => 2\n",
    );
    assert_eq!(before, after);
}

#[test]
fn project_file_wins_duplicate_class_over_dependency_copy() {
    let mut service = SimpleLanguageService::new();
    // `_build/deps/...` sorts before `src/...`, so the old sorted-first rule
    // let the dependency copy win.
    let dep = Utf8PathBuf::from("/p/_build/deps/lib/src/dup.bt");
    let own = Utf8PathBuf::from("/p/src/dup.bt");
    service.update_file(dep.clone(), "Object subclass: Dup\n  a => 1\n".to_string());
    service.update_file(own.clone(), "Object subclass: Dup\n  b => 1\n".to_string());
    let dups = service.project_index().duplicate_classes_of(&own);
    assert_eq!(dups.len(), 1);
    assert_eq!(dups[0].2, own, "project file must be the winner");
}
