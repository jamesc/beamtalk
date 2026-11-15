// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4862: instance-side protocol conformance is checked on every send
//! `walk_sends` reaches — later cascade messages (resolved against the
//! cascade's shared receiver) and sends inside `match:` arms and guards —
//! not only the sends the old hand-written recursion descended into.

use super::common::*;

fn source_with(body: &str) -> String {
    format!(
        r#"
Protocol define: Greeter
  greet -> String

Object subclass: Friendly
  greet -> String => "hi"

Object subclass: Mute

Object subclass: Host
  ping -> Nil => nil
  take: g :: Greeter -> Nil => nil
{body}"#
    )
}

fn conformance_warnings(body: &str) -> Vec<String> {
    messages(&source_with(body))
        .into_iter()
        .filter(|m| m.contains("Mute") && m.contains("Greeter"))
        .collect()
}

#[test]
fn later_cascade_message_checked_against_shared_receiver() {
    let w = conformance_warnings(
        r"
Object subclass: Caller
  run: h :: Host -> Nil =>
    h ping; take: Mute new
    nil
",
    );
    assert_eq!(w.len(), 1, "{w:#?}");
}

#[test]
fn match_arm_body_send_still_checked() {
    let w = conformance_warnings(
        r"
Object subclass: Caller
  run: h :: Host n: n :: Integer -> Nil =>
    n match: [
      1 -> (h take: Mute new);
      _ -> nil
    ]
    nil
",
    );
    assert_eq!(w.len(), 1, "{w:#?}");
}

#[test]
fn conformer_in_nested_sites_no_warning() {
    let w = conformance_warnings(
        r"
Object subclass: Caller
  run: h :: Host n: n :: Integer -> Nil =>
    h ping; take: Friendly new
    n match: [
      1 -> (h take: Friendly new);
      _ -> nil
    ]
    nil
",
    );
    assert!(w.is_empty(), "{w:#?}");
}
