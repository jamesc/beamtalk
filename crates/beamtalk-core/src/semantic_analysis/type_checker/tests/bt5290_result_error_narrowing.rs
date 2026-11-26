// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5290: inside a branch that only runs when a `Result(T, E)` is the
//! error variant (`x isError ifTrue:`, `x isOk ifFalse:`, `x ifError:`,
//! the error block of `x ifOk:ifError:`), returning `x` can never deliver a
//! wrong success value, so `^x` fits any declared `Result(U, E')` where `E`
//! fits `E'`. The BT-5283 return-type check must not reject that idiom, and
//! must still reject a mismatched success type without such a guard.

use super::common::*;

fn type_messages(source: &str) -> Vec<String> {
    let (_, parse_diags) =
        crate::source_analysis::parse(crate::source_analysis::lex_with_eof(source));
    assert!(
        parse_diags.is_empty(),
        "fixture must parse: {parse_diags:?}"
    );
    type_diags(source)
        .iter()
        .map(|d| d.message.to_string())
        .collect()
}

fn return_mismatches(diags: &[String]) -> Vec<&String> {
    diags
        .iter()
        .filter(|m| m.contains("declares return type"))
        .collect()
}

/// The beamtalk-mint `readHead:` shape: propagate the error of a
/// `Result(Binary, Object)` from a `-> Result(Tuple, Object)` method.
#[test]
fn is_error_if_true_return_propagates_error_variant() {
    let diags = type_messages(
        r"typed Object subclass: ZzMint
  readLine: ms :: Integer -> Result(Binary, Object) => Result ok: <<>>
  readHead: ms :: Integer -> Result(Tuple, Object) =>
    line := self readLine: ms
    line isError ifTrue: [^line]
    Result ok: {#head, line value}
",
    );
    assert!(
        return_mismatches(&diags).is_empty(),
        "`line isError ifTrue: [^line]` must type-check: {diags:?}"
    );
}

/// `isOk ifFalse:` is the same guard spelled the other way round.
#[test]
fn is_ok_if_false_return_propagates_error_variant() {
    let diags = type_messages(
        r"typed Object subclass: ZzMint
  readLine: ms :: Integer -> Result(Binary, Object) => Result ok: <<>>
  readHead: ms :: Integer -> Result(Tuple, Object) =>
    line := self readLine: ms
    line isOk ifFalse: [^line]
    Result ok: {#head, line value}
",
    );
    assert!(
        return_mismatches(&diags).is_empty(),
        "`line isOk ifFalse: [^line]` must type-check: {diags:?}"
    );
}

/// The mint `readUntilClose:ms:` / `connectTlsOn:...` shapes: a multi-statement
/// error branch whose `^x` sits inside a nested conditional, on the class side,
/// with a `Result(Nil, Object)` propagated from a `-> Result(ZzMint, Object)`.
#[test]
fn nested_return_inside_error_branch_type_checks() {
    let diags = type_messages(
        r"typed Object subclass: ZzMint
  class check: n :: Integer -> Result(Nil, Object) => Result ok: nil
  class connect: n :: Integer -> Result(ZzMint, Object) =>
    check := self check: n
    check isError
      ifTrue: [
        check error =:= #closed ifTrue: [^Result ok: self new] ifFalse: [^check]
      ]
    Result ok: self new
",
    );
    assert!(
        return_mismatches(&diags).is_empty(),
        "a nested `^check` in the error branch must type-check: {diags:?}"
    );
}

/// The mint `parseHeaders:` / `request:...` shape: the guard sits inside a
/// `do:` block, on a local reassigned each iteration.
#[test]
fn guard_inside_loop_block_type_checks() {
    let diags = type_messages(
        r#"typed Object subclass: ZzMint
  sendLine: s :: String -> Result(Nil, Object) => Result ok: nil
  request: lines :: List(String) -> Result(Tuple, Object) =>
    sent := self sendLine: "GET"
    sent isError ifTrue: [^sent]
    lines do: [:l |
      sent := self sendLine: l
      sent isError ifTrue: [^sent]
    ]
    Result ok: {#sent}
"#,
    );
    assert!(
        return_mismatches(&diags).is_empty(),
        "guards inside a `do:` block must type-check: {diags:?}"
    );
}

/// The error block of `ifError:` only runs for the error variant.
#[test]
fn if_error_block_return_propagates_error_variant() {
    let diags = type_messages(
        r"typed Object subclass: ZzMint
  readLine: ms :: Integer -> Result(Binary, Object) => Result ok: <<>>
  readHead: ms :: Integer -> Result(Tuple, Object) =>
    line := self readLine: ms
    line ifError: [:e | ^line]
    Result ok: {#head, line value}
",
    );
    assert!(
        return_mismatches(&diags).is_empty(),
        "`line ifError: [:e | ^line]` must type-check: {diags:?}"
    );
}

/// The error block of `ifOk:ifError:` only runs for the error variant.
#[test]
fn if_ok_if_error_error_block_return_propagates_error_variant() {
    let diags = type_messages(
        r"typed Object subclass: ZzMint
  readLine: ms :: Integer -> Result(Binary, Object) => Result ok: <<>>
  readHead: ms :: Integer -> Result(Tuple, Object) =>
    line := self readLine: ms
    line ifOk: [:v | Result ok: {#head, v}] ifError: [:e | ^line]
",
    );
    assert!(
        return_mismatches(&diags).is_empty(),
        "`^line` in the `ifError:` block must type-check: {diags:?}"
    );
}

// ---- BT-5283's check stays effective -------------------------------------

/// No guard: returning a Result with the wrong success type is reported.
#[test]
fn unguarded_mismatched_success_type_still_reported() {
    let diags = type_messages(
        r"typed Object subclass: ZzMint
  readLine: ms :: Integer -> Result(Binary, Object) => Result ok: <<>>
  readHead: ms :: Integer -> Result(Tuple, Object) =>
    line := self readLine: ms
    ms < 0 ifTrue: [^line]
    Result ok: {#head, line value}
",
    );
    assert!(
        diags
            .iter()
            .any(|m| m.contains("declares return type Result(Tuple, Object)")
                && m.contains("Result(Binary, Object)")),
        "an unguarded `^line` must still be reported: {diags:?}"
    );
}

/// The ok branch (`isOk ifTrue:`, `isError ifFalse:`) is not narrowed to the
/// error variant: its success value is the wrong type.
#[test]
fn ok_branch_return_with_mismatched_success_type_still_reported() {
    for guard in ["line isOk ifTrue: [^line]", "line isError ifFalse: [^line]"] {
        let source = format!(
            "typed Object subclass: ZzMint
  readLine: ms :: Integer -> Result(Binary, Object) => Result ok: <<>>
  readHead: ms :: Integer -> Result(Tuple, Object) =>
    line := self readLine: ms
    {guard}
    Result ok: {{#head, line value}}
"
        );
        let diags = type_messages(&source);
        assert!(
            diags
                .iter()
                .any(|m| m.contains("declares return type Result(Tuple, Object)")),
            "`{guard}` returns the ok variant and must be reported: {diags:?}"
        );
    }
}

/// The `ifOk:` block of `ifOk:ifError:` is the ok variant: still reported.
#[test]
fn if_ok_block_return_with_mismatched_success_type_still_reported() {
    let diags = type_messages(
        r"typed Object subclass: ZzMint
  readLine: ms :: Integer -> Result(Binary, Object) => Result ok: <<>>
  readHead: ms :: Integer -> Result(Tuple, Object) =>
    line := self readLine: ms
    line ifOk: [:v | ^line] ifError: [:e | ^line]
    Result ok: {#head}
",
    );
    assert!(
        diags
            .iter()
            .any(|m| m.contains("declares return type Result(Tuple, Object)")),
        "`^line` in the `ifOk:` block must be reported: {diags:?}"
    );
}

/// The narrowed error variant keeps its error type: an error type that does
/// not fit the declared one is still reported.
#[test]
fn mismatched_error_type_still_reported() {
    let diags = type_messages(
        r"typed Object subclass: ZzMint
  readLine: ms :: Integer -> Result(Binary, String) => Result ok: <<>>
  readHead: ms :: Integer -> Result(Tuple, Integer) =>
    line := self readLine: ms
    line isError ifTrue: [^line]
    Result ok: {#head}
",
    );
    assert!(
        diags.iter().any(|m| m.contains(
            "declares return type Result(Tuple, Integer), but body returns Result(Tuple, String)"
        )),
        "a `Result(_, String)` error must not fit `Result(_, Integer)`, and the \
         message must name the error type, not `Never`: {diags:?}"
    );
}

/// Reassigning the variable inside the error branch drops the narrowing.
#[test]
fn reassignment_inside_error_branch_drops_narrowing() {
    let diags = type_messages(
        r"typed Object subclass: ZzMint
  readLine: ms :: Integer -> Result(Binary, Object) => Result ok: <<>>
  readHead: ms :: Integer -> Result(Tuple, Object) =>
    line := self readLine: ms
    line isError ifTrue: [
      line := self readLine: ms
      ^line
    ]
    Result ok: {#head}
",
    );
    assert!(
        diags
            .iter()
            .any(|m| m.contains("declares return type Result(Tuple, Object)")),
        "a reassigned `line` is no longer known to be an error: {diags:?}"
    );
}

/// Sends to the narrowed variable inside the error branch stay clean: its
/// binding keeps `Result(T, E)`, so `error` answers `E` and `value` /
/// `valueOr:` / `map:` are typed by `T` as before.
#[test]
fn sends_in_error_branch_stay_clean() {
    let diags = type_messages(
        r"typed Object subclass: ZzMint
  readLine: ms :: Integer -> Result(Binary, Object) => Result ok: <<>>
  readHead: ms :: Integer -> Object =>
    line := self readLine: ms
    line isError ifTrue: [
      line error printString
      line valueOr: <<>>
      line ifOk: [:v | v size] ifError: [:e | e]
      line mapError: [:e | e]
      line map: [:v | v size]
      ^line value
    ]
    line value size
",
    );
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
}

// ---- The narrowing is a flow fact, not a `Never` success type -------------

/// `valueOr:` on the narrowed variable answers its default — `T`, not
/// `Never` — so returning it is still checked.
#[test]
fn value_or_in_error_branch_is_still_checked() {
    let diags = type_messages(
        r#"typed Object subclass: ZzMint
  readLine: ms :: Integer -> Result(Binary, Object) => Result ok: <<>>
  readHead: ms :: Integer -> String =>
    line := self readLine: ms
    line isError ifTrue: [^line valueOr: <<>>]
    "ok"
"#,
    );
    assert!(
        diags
            .iter()
            .any(|m| m.contains("declares return type String") && m.contains("Binary")),
        "`^line valueOr: <<>>` answers a Binary and must be reported: {diags:?}"
    );
}

/// A `valueOr:` inside a guard does not make the guard look diverging, so it
/// does not narrow the variable it guards for the rest of the branch.
#[test]
fn value_or_does_not_make_a_guard_diverge() {
    let diags = type_messages(
        r"typed Object subclass: ZzMint
  pick: ms :: Integer -> Integer | Nil => nil
  readLine: ms :: Integer -> Result(Binary, Object) => Result ok: <<>>
  readHead: ms :: Integer -> Integer =>
    x := self pick: ms
    line := self readLine: ms
    line isError ifTrue: [
      x isNil ifTrue: [line valueOr: <<>>]
      ^x
    ]
    0
",
    );
    assert!(
        diags
            .iter()
            .any(|m| m.contains("declares return type Integer") && m.contains("Nil")),
        "`x` may still be nil after a non-diverging guard: {diags:?}"
    );
}

// ---- Review findings: the fact never outlives what it describes ----------

/// A write to the local inside a nested block — even of the same type —
/// ends the narrowing.
#[test]
fn same_type_write_in_nested_block_drops_narrowing() {
    let diags = type_messages(
        r"typed Object subclass: ZzMint
  readLine: ms :: Integer -> Result(Binary, Object) => Result ok: <<>>
  readHead: ms :: Integer -> Result(Tuple, Object) =>
    line := self readLine: ms
    line isError ifTrue: [
      #(1, 2) do: [:i | line := self readLine: ms]
      ^line
    ]
    Result ok: {#head}
",
    );
    assert!(
        diags
            .iter()
            .any(|m| m.contains("declares return type Result(Tuple, Object)")),
        "`line` may be an ok value after the loop rebinds it: {diags:?}"
    );
}

/// A `self.<field>` is not narrowed: a self-send could reassign it.
#[test]
fn field_is_not_narrowed() {
    let diags = type_messages(
        r"typed Actor subclass: ZzMintActor
  state: res :: Result(Binary, Object) = Result ok: <<>>
  reload => self.res := Result ok: <<>>
  readHead: ms :: Integer -> Result(Tuple, Object) =>
    self.res isError ifTrue: [
      self reload
      ^self.res
    ]
    Result ok: {#head}
",
    );
    assert!(
        diags
            .iter()
            .any(|m| m.contains("declares return type Result(Tuple, Object)")),
        "`^self.res` after a self-send must be reported: {diags:?}"
    );
}

/// A user-declared `Result(Never, E)` is an ordinary type: a nil narrowing
/// of `Result(Never, E) | Nil` still applies.
#[test]
fn declared_never_success_result_narrows_normally() {
    let diags = type_messages(
        r"typed Object subclass: ZzMint
  pick: ms :: Integer -> Result(Never, Symbol) | Nil => nil
  readHead: ms :: Integer -> Result(Never, Symbol) =>
    r := self pick: ms
    r notNil ifTrue: [^r]
    Result error: #none
",
    );
    assert!(
        return_mismatches(&diags).is_empty(),
        "`r notNil ifTrue: [^r]` must narrow away Nil: {diags:?}"
    );
}

/// An unguarded `^x` of a user-declared `Result(Never, E)` is reported with
/// its own type, not rewritten to the declared success type.
#[test]
fn unguarded_declared_never_success_result_keeps_its_display() {
    let diags = type_messages(
        r"typed Object subclass: ZzMint
  pick: ms :: Integer -> Result(Never, Symbol) => Result error: #none
  readHead: ms :: Integer -> Result(Tuple, Integer) =>
    r := self pick: ms
    ms < 0 ifTrue: [^r]
    Result ok: {#head}
",
    );
    assert!(
        diags
            .iter()
            .any(|m| m.contains("body returns Result(Never, Symbol)")),
        "the message must name the value's real type: {diags:?}"
    );
}
