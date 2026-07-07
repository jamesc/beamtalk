// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0104 (Typed Actor Protocols) end-to-end integration tests (BT-2752).
//!
//! Unlike the per-feature unit tests (`cast_and_sync_send`, `spawn_with_keys`,
//! `with_timeout_transparency`), which build the AST by hand and call
//! `infer_expr` directly, these drive the **full diagnostic pipeline from real
//! Beamtalk source strings** via `parse_source` + `run_with_expect`. They
//! exercise all four ADR-0104 typing edges the way a user's program would hit
//! them, so a regression in lexing, parsing, hierarchy construction, or
//! checking — not just inference — is caught.
//!
//! The four edges, each covered here from source:
//!
//! 1. **Sync send return type** — `c increment` on an Actor resolves the
//!    method's declared return (`Integer`).
//! 2. **Cast (`!`) → `Nil`** — a bare cast statement types as `Nil`.
//! 3. **`spawnWith:` key checking** — an unknown literal-map key warns with a
//!    typo suggestion.
//! 4. **`withTimeout:` transparency + cross-process DNU** — a forwarded call on
//!    the timeout proxy resolves the wrapped actor's real return type, and an
//!    unknown selector on a known Actor warns with local-send wording.
//!
//! Where "the send types as `T`" cannot be asserted directly (the harness
//! surfaces diagnostics, not inferred types), we prove it with a **return-type
//! control**: a method declaring the *wrong* return type over the same body
//! produces the `check_return_type` mismatch warning ("declares return type X,
//! but body returns Y") — which only fires when the body is a concrete `Known`
//! type, so its presence pins the inferred type. Every edge is source-driven;
//! no edge falls back to the AST-builder style.

use super::common::*;

/// Collect diagnostics whose message reports a `check_return_type` mismatch.
fn return_type_diags(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect()
}

// ── Edge 1: sync send resolves the method's declared return type ────────────

/// End-to-end spawn form (`c := Counter spawn` then `c increment` /
/// `c getValue`): the sync sends resolve real Actor methods, so the program is
/// clean — no DNU, no return-type noise.
#[test]
fn sync_send_on_spawned_actor_is_clean() {
    let source = "\
Actor subclass: Counter
  state: count = 0
  increment -> Integer => self.count := self.count + 1
  getValue -> Integer => self.count

c := Counter spawn
c increment
c getValue
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    assert!(
        diags
            .iter()
            .all(|d| d.category != Some(DiagnosticCategory::Dnu)),
        "sync sends to real Actor methods must not warn as unknown selectors: {diags:?}"
    );
    assert!(
        return_type_diags(&diags).is_empty(),
        "no return-type diagnostics expected for a clean program: {diags:?}"
    );
}

/// Positive: a method declaring `-> Integer` whose body is the sync send
/// `c increment` (a `-> Integer` Actor method) type-checks clean — proving the
/// send resolves to `Integer`, the declared return.
#[test]
fn sync_send_matching_declared_return_is_clean() {
    let source = "\
Actor subclass: Counter
  state: count = 0
  increment -> Integer => self.count := self.count + 1

Object subclass: Client
  useCounter: c :: Counter -> Integer => c increment
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    assert!(
        return_type_diags(&diags).is_empty(),
        "`c increment` types as Integer, matching the declared return: {diags:?}"
    );
}

/// Return-type control: declaring the *wrong* return (`-> String`) over the
/// same `c increment` body warns "declares return type String, but body
/// returns Integer" — pinning the sync send's inferred type as `Integer`.
#[test]
fn sync_send_wrong_declared_return_warns_proving_integer() {
    let source = "\
Actor subclass: Counter
  state: count = 0
  increment -> Integer => self.count := self.count + 1

Object subclass: Client
  useCounter: c :: Counter -> String => c increment
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let mismatches = return_type_diags(&diags);
    assert_eq!(
        mismatches.len(),
        1,
        "expected exactly one return-type mismatch: {diags:?}"
    );
    assert!(
        mismatches[0].message.contains("String") && mismatches[0].message.contains("Integer"),
        "mismatch must name declared String and inferred Integer: {}",
        mismatches[0].message
    );
    assert!(
        mismatches[0].message.contains("body returns Integer"),
        "the sync send must be inferred as Integer: {}",
        mismatches[0].message
    );
}

// ── Edge 2: a bare cast statement (`!`) types as `Nil` ──────────────────────

/// Positive: a method declaring `-> Nil` whose body ends in a bare cast
/// (`c increment!`) type-checks clean — the fire-and-forget cast evaluates to
/// `Nil`, matching the declared return.
#[test]
fn bare_cast_matching_nil_return_is_clean() {
    let source = "\
Actor subclass: Counter
  state: count = 0
  increment -> Integer => self.count := self.count + 1

Object subclass: Client
  notify: c :: Counter -> Nil => c increment!
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    assert!(
        return_type_diags(&diags).is_empty(),
        "a bare cast body types as Nil, matching `-> Nil`: {diags:?}"
    );
}

/// Return-type control (the ADR migration case): declaring a non-`Nil` return
/// (`-> Integer`) over a body that *ends in a bare cast* newly warns "body
/// returns Nil" — pinning the cast statement's type as `Nil` even though the
/// cast target (`increment`) itself declares `-> Integer`.
#[test]
fn bare_cast_under_nonnil_return_warns_body_returns_nil() {
    let source = "\
Actor subclass: Counter
  state: count = 0
  increment -> Integer => self.count := self.count + 1

Object subclass: Client
  notify: c :: Counter -> Integer => c increment!
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let mismatches = return_type_diags(&diags);
    assert_eq!(
        mismatches.len(),
        1,
        "expected exactly one return-type mismatch for the bare-cast tail: {diags:?}"
    );
    assert!(
        mismatches[0].message.contains("Integer") && mismatches[0].message.contains("Nil"),
        "mismatch must name declared Integer and inferred Nil: {}",
        mismatches[0].message
    );
    assert!(
        mismatches[0].message.contains("body returns Nil"),
        "the bare cast statement must be inferred as Nil: {}",
        mismatches[0].message
    );
}

// ── Edge 3: `spawnWith:` literal-map key checking ───────────────────────────

/// An unknown `spawnWith:` map key warns (a Warning per ADR 0100's
/// provably-failing tier) and names the nearest declared slot as a suggestion.
#[test]
fn spawn_with_unknown_key_warns_with_suggestion() {
    let source = "\
Actor subclass: Counter
  state: count = 0

Counter spawnWith: #{#cuont => 0}
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let key_diags: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("state key"))
        .collect();
    assert_eq!(
        key_diags.len(),
        1,
        "expected one unknown-key warning: {diags:?}"
    );
    assert_eq!(
        key_diags[0].severity,
        crate::source_analysis::Severity::Warning
    );
    assert!(
        key_diags[0].message.contains("cuont")
            && key_diags[0].message.contains("count")
            && key_diags[0].message.contains("did you mean"),
        "warning must name the unknown key `cuont`, suggest `count`, and read \
         as a typo suggestion: {}",
        key_diags[0].message
    );
}

/// A known `spawnWith:` map key (matching a declared `state:` slot) produces no
/// key warning — the constructor is well-formed.
#[test]
fn spawn_with_known_key_is_clean() {
    let source = "\
Actor subclass: Counter
  state: count = 0

Counter spawnWith: #{#count => 0}
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    assert!(
        diags.iter().all(|d| !d.message.contains("state key")),
        "a declared slot key must not warn: {diags:?}"
    );
}

// ── Edge 4: `withTimeout:` transparency + cross-process DNU ──────────────────

/// Positive: a forwarded call through the timeout proxy
/// (`(db withTimeout: 30000) query: "..."`) resolves `query:`'s declared return
/// (`List`), so a method declaring `-> List` over that body is clean. Before
/// transparency the proxy collapsed the forwarded call to `Dynamic`.
#[test]
fn with_timeout_forwarded_call_resolves_wrapped_return() {
    let source = "\
Actor subclass: SlowDb
  state: rows = nil
  query: sql :: String -> List => #(1, 2, 3)

Object subclass: Client
  run: db :: SlowDb -> List => (db withTimeout: 30000) query: \"SELECT\"
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    assert!(
        return_type_diags(&diags).is_empty(),
        "the forwarded `query:` resolves List through the transparent proxy: {diags:?}"
    );
    assert!(
        diags
            .iter()
            .all(|d| d.category != Some(DiagnosticCategory::Dnu)),
        "a real forwarded method must not warn as an unknown selector: {diags:?}"
    );
}

/// Return-type control for transparency: declaring the wrong return
/// (`-> String`) over the forwarded `query:` body warns "body returns List" —
/// pinning that `withTimeout:` is transparent (result typed `SlowDb`) and the
/// forwarded call resolves `query:`'s real `List` return, not `Dynamic`.
#[test]
fn with_timeout_wrong_return_warns_proving_list() {
    let source = "\
Actor subclass: SlowDb
  state: rows = nil
  query: sql :: String -> List => #(1, 2, 3)

Object subclass: Client
  run: db :: SlowDb -> String => (db withTimeout: 30000) query: \"SELECT\"
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let mismatches = return_type_diags(&diags);
    assert_eq!(
        mismatches.len(),
        1,
        "expected one return-type mismatch for the forwarded call: {diags:?}"
    );
    assert!(
        mismatches[0].message.contains("String") && mismatches[0].message.contains("List"),
        "mismatch must name declared String and forwarded List: {}",
        mismatches[0].message
    );
    assert!(
        mismatches[0].message.contains("body returns List"),
        "the forwarded proxy call must be inferred as List: {}",
        mismatches[0].message
    );
}

/// Transparency preserves the receiver's **type args** for a *generic* actor:
/// `b :: Boxed(Integer)`, and `(b withTimeout: t) echo: n` (where `echo:` is
/// `x :: E -> E`) resolves `E = Integer`. A method declaring `-> Integer` over
/// that body is clean. (Unlike the hand-built unit hierarchy in
/// `with_timeout_transparency.rs`, the full compile pipeline exercised here
/// reproduces the generic actor's method inheritance, so the proxy result is a
/// concrete `Integer`, not `Dynamic`.)
#[test]
fn generic_actor_through_proxy_preserves_type_arg() {
    let source = "\
Actor subclass: Boxed(E)
  echo: x :: E -> E => x

Object subclass: Client
  run: n :: Integer -> Integer =>
    b :: Boxed(Integer) := Boxed spawn
    (b withTimeout: 5000) echo: n
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    assert!(
        return_type_diags(&diags).is_empty(),
        "the generic forwarded `echo:` resolves E=Integer through the proxy: {diags:?}"
    );
    assert!(
        diags
            .iter()
            .all(|d| d.category != Some(DiagnosticCategory::Dnu)),
        "a real forwarded method on a generic actor must not warn as unknown: {diags:?}"
    );
}

/// Return-type control for the generic case: declaring `-> String` over the
/// same generic forwarded `echo: n` (n :: Integer) body warns "body returns
/// Integer" — proving the type arg survives the proxy (result is concrete
/// `Integer`, not `Dynamic`, which would produce no warning at all).
#[test]
fn generic_actor_through_proxy_wrong_return_warns_proving_integer() {
    let source = "\
Actor subclass: Boxed(E)
  echo: x :: E -> E => x

Object subclass: Client
  run: n :: Integer -> String =>
    b :: Boxed(Integer) := Boxed spawn
    (b withTimeout: 5000) echo: n
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let mismatches = return_type_diags(&diags);
    assert_eq!(
        mismatches.len(),
        1,
        "expected one return-type mismatch for the generic forwarded call: {diags:?}"
    );
    assert!(
        mismatches[0].message.contains("body returns Integer"),
        "the generic proxy call must be inferred as Integer (type arg preserved): {}",
        mismatches[0].message
    );
}

/// Cross-process DNU: an unknown selector on a statically-known Actor
/// (`logger logg: "hi"`) warns with the *same wording as a local send* — the
/// process boundary is invisible to the diagnostic (ADR 0100). Matches the
/// ADR's error example, including the did-you-mean hint naming `log:`.
#[test]
fn cross_process_dnu_warns_like_local_send() {
    let source = "\
Actor subclass: Logger
  state: sink = nil
  log: msg => self.sink := msg

Object subclass: Client
  useLogger: logger :: Logger => logger logg: \"hi\"
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let dnu = diags
        .iter()
        .find(|d| d.category == Some(DiagnosticCategory::Dnu))
        .expect("an unknown selector on a known Actor must emit a DNU diagnostic");
    assert_eq!(
        dnu.message.as_str(),
        "Logger does not understand 'logg:'",
        "cross-process DNU wording must match the unified local-send format"
    );
    assert_eq!(
        dnu.hint.as_deref(),
        Some("Did you mean 'log:'?"),
        "the did-you-mean hint must survive the process boundary"
    );
}
