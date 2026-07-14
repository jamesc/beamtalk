// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-2879: the cascade continuation loop (`Expression::Cascade`'s
//! `for msg in messages` loop) had no `InferredType::Meta` branch at all —
//! neither for `check_argument_types` nor for `check_spawn_with_map_keys` —
//! even though the non-cascade path (`infer_message_send_with_receiver_ty`)
//! has handled a `Meta`-typed receiver (ADR 0083 / BT-2255) since before
//! BT-2850. [BT-2850] fixed the cascade loop's `ClassReference` and
//! `self`-in-class-method branches; this is the loop's third, independent
//! gap — a `someVar :: SomeClass class`-typed cascade target silently skipped
//! both checks on every continuation message.
//!
//! [BT-2850]: https://linear.app/beamtalk/issue/BT-2850

use super::common::*;

fn key_diags(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.message.contains("state key"))
        .collect()
}

fn expects_diags(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type) && d.message.contains("expects"))
        .collect()
}

/// AC: a `Meta`-typed cascade target's *continuation* `spawnWith:` message
/// (i.e. not the cascade's first send, which already went through
/// `infer_message_send_with_receiver_ty`'s `Meta` branch) gets literal-map key
/// checking, matching `Counter spawnWith: #{...}` written on its own.
#[test]
fn bt2879_cascade_meta_receiver_continuation_spawn_with_typo_warns() {
    let source = "\
Actor subclass: Counter
  state: count = 0

typed Object subclass: Runner
  run: cls :: Counter class -> Object =>
    cls spawn; spawnWith: #{#cuont => 0}
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let diags = key_diags(&diags);
    assert_eq!(
        diags.len(),
        1,
        "Meta-receiver cascade continuation `spawnWith:` with a typo'd key should warn: {diags:?}"
    );
    assert!(
        diags[0].message.contains("cuont") && diags[0].message.contains("did you mean"),
        "message should name the unknown key and suggest the nearest slot: {}",
        diags[0].message
    );
}

/// Negative control: the same cascade shape with a correctly-spelled key
/// produces no new diagnostic.
#[test]
fn bt2879_cascade_meta_receiver_continuation_spawn_with_known_key_is_clean() {
    let source = "\
Actor subclass: Counter
  state: count = 0

typed Object subclass: Runner
  run: cls :: Counter class -> Object =>
    cls spawn; spawnWith: #{#count => 0}
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    assert!(
        key_diags(&diags).is_empty(),
        "a correctly-spelled key on a Meta-receiver cascade continuation must not warn: {diags:?}"
    );
}

/// AC: a `Meta`-typed cascade target's continuation message with an
/// incompatible `Known` argument type produces a `Type` diagnostic, matching
/// `check_argument_types` behavior for the class-reference and
/// self-in-class-method cascade branches (BT-2850/BT-2877).
#[test]
fn bt2879_cascade_meta_receiver_continuation_incompatible_arg_warns() {
    let source = "\
typed Object subclass: Thing
  class classTakeStr: aString :: String -> String => aString
  class classSecondObj: anObject :: Object -> Object => anObject
  class classSecond: aString :: String -> String => aString

typed Object subclass: Runner
  run: cls :: Thing class -> Object =>
    cls classTakeStr: \"seed\"; classSecond: 1
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let diags = expects_diags(&diags);
    assert_eq!(
        diags.len(),
        1,
        "Meta-receiver cascade continuation with an incompatible argument should warn: {diags:?}"
    );
}

/// Negative control: a compatible argument on the same shape produces no
/// `Type` diagnostic.
#[test]
fn bt2879_cascade_meta_receiver_continuation_compatible_arg_is_clean() {
    let source = "\
typed Object subclass: Thing
  class classTakeStr: aString :: String -> String => aString
  class classSecond: aString :: String -> String => aString

typed Object subclass: Runner
  run: cls :: Thing class -> Object =>
    cls classTakeStr: \"seed\"; classSecond: \"ok\"
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    assert!(
        expects_diags(&diags).is_empty(),
        "a compatible argument on a Meta-receiver cascade continuation must not warn: {diags:?}"
    );
}

/// AC (block-param propagation): a Meta-typed cascade target's continuation
/// message block arguments must resolve against the class's *class-side*
/// method signatures, not instance methods — mirroring
/// `block_params_typed_for_class_method_in_cascade_bt2158`'s class-reference
/// case (`dynamic_and_blocks.rs`). This is only correct because
/// `is_class_side_send` (used by `infer_args_with_block_context` for every
/// message in the cascade loop) now also treats a `Meta`-typed dispatch
/// target as class-side, matching `infer_generic_send_args`'s ADR 0083
/// handling (~line 1241).
///
/// Asserts against the type map directly rather than diagnostics: when the
/// block param falls back to Dynamic, it does so as `Dynamic(DynamicReceiver)`
/// (BT-2042), which is deliberately excluded from the "Dynamic in typed
/// class" warning — so a diagnostics-only assertion here would pass whether
/// or not the params were actually typed, giving false confidence.
#[test]
fn bt2879_cascade_meta_receiver_continuation_block_params_typed() {
    let source = "
typed Object subclass: HTTPRouteBuilder
  get: path :: String -> HTTPRouteBuilder => self

typed Object subclass: HTTPRouter
  class build: aBlock :: Block(HTTPRouteBuilder, Object) -> Integer => 42
  class buildMore: aBlock :: Block(HTTPRouteBuilder, Object) -> Integer => 43

typed Object subclass: App
  go: cls :: HTTPRouter class -> Object =>
    cls
      build: [:r | r get: \"/\"];
      buildMore: [:r2 | r2 get: \"/more\"]
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // Every `Block(...)` entry recorded in the type map should have its first
    // param type resolved to `HTTPRouteBuilder`, not left as `Dynamic`.
    let block_first_params: Vec<Option<ecow::EcoString>> = checker
        .type_map()
        .iter()
        .filter_map(|(_, ty)| {
            if let InferredType::Known {
                class_name,
                type_args,
                ..
            } = ty
            {
                if class_name.as_str() == "Block" {
                    return Some(type_args.first().and_then(InferredType::as_known).cloned());
                }
            }
            None
        })
        .collect();
    assert_eq!(
        block_first_params.len(),
        2,
        "expected one Block(...) type-map entry per cascade continuation message; got {block_first_params:?}"
    );
    for param_ty in &block_first_params {
        assert_eq!(
            param_ty.as_deref(),
            Some("HTTPRouteBuilder"),
            "Meta-receiver cascade continuation block param should resolve to \
             `HTTPRouteBuilder` from the class-side method signature, not Dynamic; \
             got {block_first_params:?}"
        );
    }
}
