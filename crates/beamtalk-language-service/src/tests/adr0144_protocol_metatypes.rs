// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0144 (BT-4436): completion and hover on a `P class` value — the class
//! object of some class conforming to protocol `P` (`ProtocolMeta`).
//! Completion offers `P`'s `class` requirements with their signatures, then
//! the class-object protocol; hover shows `P class`, typed as the compiler
//! types it; signature help shows the `class` requirement's signature
//! (BT-4450).

use super::common::*;

const PROTOCOL: &str = "Protocol define: Transport
  class open: host :: String -> Integer
  class scheme -> String
";

const CONFORMERS: &str = "sealed Object subclass: Tcp
  class open: host :: String -> Integer => 1
  class scheme -> String => \"tcp\"

sealed Object subclass: Tls
  class open: host :: String -> Integer => 2
  class scheme -> String => \"tls\"
";

/// The `Client` class: a `:: Transport class` field, a parameter of the same
/// type, and one line per probe (`probe` ends in a space so completion runs
/// after the receiver).
const CLIENT: &str = concat!(
    "Object subclass: Client\n",
    "  field: transport :: Transport class = Tcp\n",
    "\n",
    "  viaParam: t :: Transport class =>\n",
    "    t \n",
    "\n",
    "  viaField =>\n",
    "    self.transport \n",
    "\n",
    "  locals: t :: Transport class =>\n",
    "    copy := self.transport\n",
    "    s := t scheme\n",
    "    s size\n",
    "    copy\n",
);

/// A service with one file holding `sources` concatenated.
fn service_with(sources: &[&str]) -> (SimpleLanguageService, Utf8PathBuf, String) {
    let source = sources.join("\n");
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("client.bt");
    service.update_file(file.clone(), source.clone());
    (service, file, source)
}

/// The position just after the last character of the first line equal to `line`.
fn end_of_line(source: &str, line: &str) -> Position {
    let index = source
        .lines()
        .position(|l| l == line)
        .unwrap_or_else(|| panic!("no line {line:?} in source"));
    Position::new(
        u32::try_from(index).unwrap(),
        u32::try_from(line.len()).unwrap(),
    )
}

/// The position of the first `needle` on the first line containing `line_has`.
fn position_of(source: &str, line_has: &str, needle: &str) -> Position {
    let (index, line) = source
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains(line_has))
        .unwrap_or_else(|| panic!("no line containing {line_has:?}"));
    let column = line.find(needle).expect("needle on line");
    Position::new(
        u32::try_from(index).unwrap(),
        u32::try_from(column).unwrap(),
    )
}

fn labels(completions: &[Completion]) -> Vec<&str> {
    completions.iter().map(|c| c.label.as_str()).collect()
}

/// Asserts the protocol-metatype completion shape: the `class` requirements
/// with their signatures, then the class-object protocol — and none of the
/// protocol object's own class-side methods.
fn assert_protocol_meta_completions(completions: &[Completion]) {
    let labels = labels(completions);
    let index = |label: &str| {
        labels
            .iter()
            .position(|l| *l == label)
            .unwrap_or_else(|| panic!("`{label}` missing from {labels:?}"))
    };
    let detail = |label: &str| {
        completions
            .iter()
            .find(|c| c.label == label)
            .and_then(|c| c.detail.as_deref())
            .unwrap_or_default()
            .to_string()
    };
    assert_eq!(
        detail("open:"),
        "on Transport class (String) -> Integer",
        "a requirement completes with its declared signature"
    );
    assert_eq!(detail("scheme"), "on Transport class -> String");
    for class_object_selector in ["name", "respondsTo:", "printString"] {
        assert!(
            index("open:") < index(class_object_selector)
                && index("scheme") < index(class_object_selector),
            "the requirements come before the class-object protocol's \
             `{class_object_selector}`: {labels:?}"
        );
    }
    for protocol_object_selector in ["requiredMethods", "conformingClasses"] {
        assert!(
            !labels.contains(&protocol_object_selector),
            "`{protocol_object_selector}` is the protocol object's, not a conformer's: {labels:?}"
        );
    }
}

#[test]
fn completion_on_protocol_metatype_parameter() {
    let (service, file, source) = service_with(&[PROTOCOL, CONFORMERS, CLIENT]);
    let completions = service.completions(&file, end_of_line(&source, "    t "));
    assert_protocol_meta_completions(&completions);
}

#[test]
fn completion_on_protocol_metatype_field() {
    let (service, file, source) = service_with(&[PROTOCOL, CONFORMERS, CLIENT]);
    let completions = service.completions(&file, end_of_line(&source, "    self.transport "));
    assert_protocol_meta_completions(&completions);
}

#[test]
fn completion_on_protocol_metatype_with_cross_file_protocol() {
    let mut service = SimpleLanguageService::new();
    let protocol_file = Utf8PathBuf::from("transport.bt");
    service.update_file(protocol_file, PROTOCOL.to_string());
    let (file, source) = (
        Utf8PathBuf::from("client.bt"),
        [CONFORMERS, CLIENT].join("\n"),
    );
    service.update_file(file.clone(), source.clone());
    let completions = service.completions(&file, end_of_line(&source, "    t "));
    assert_protocol_meta_completions(&completions);
}

#[test]
fn hover_on_protocol_metatype_variables() {
    let (service, file, source) = service_with(&[PROTOCOL, CONFORMERS, CLIENT]);
    let hover_at = |position| {
        service
            .hover(&file, position)
            .unwrap_or_else(|| panic!("hover expected at {position:?}"))
            .contents
    };
    // A parameter and a local copied from the field.
    let param = hover_at(position_of(&source, "s := t scheme", "t scheme"));
    assert!(param.contains("Type: Transport class"), "{param}");
    let copy = hover_at(Position::new(end_of_line(&source, "    copy").line, 4));
    assert!(copy.contains("Type: Transport class"), "{copy}");
    // The field itself.
    let field = hover_at(position_of(&source, "copy := self.transport", "transport"));
    assert!(field.contains("transport :: Transport class"), "{field}");
    // A send through it answers the requirement's declared return type —
    // the protocol metatype, not the protocol object's class side.
    let result = hover_at(position_of(&source, "    s size", "s size"));
    assert!(result.contains("Type: String"), "{result}");
}

/// ADR 0144 / BT-4450: signature help on a `P class` receiver resolves `P`
/// from another project file, like completion and hover do.
#[test]
fn signature_help_on_cross_file_protocol_metatype() {
    let mut service = SimpleLanguageService::new();
    service.update_file(
        Utf8PathBuf::from("wire.bt"),
        "Protocol define: Wire\n  class send: data :: Object to: port :: Integer -> Boolean\n"
            .to_string(),
    );
    let file = Utf8PathBuf::from("caller.bt");
    service.update_file(
        file.clone(),
        "Object subclass: Caller\n  run: w :: Wire class =>\n    w send: 1 to: 2\n".to_string(),
    );

    let help = service
        .signature_help(&file, Position::new(2, 19))
        .expect("the class requirement's signature is shown");
    assert_eq!(
        help.signatures[0].label.as_str(),
        "send: Object to: Integer -> Boolean"
    );
    assert_eq!(
        help.signatures[0].documentation.as_deref(),
        Some("class requirement of protocol `Wire`")
    );
}
