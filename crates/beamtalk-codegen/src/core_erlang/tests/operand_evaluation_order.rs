// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4296: the operands of one Core Erlang construct — collection-literal
//! elements, runtime-dispatch arguments — evaluate in source order.
//!
//! Core Erlang leaves the evaluation order of a construct's operands
//! unspecified, and erlc evaluates a list cell's tail before its head, so
//! `#[a bump, a n]` lowered to `from_list([send(a, bump), send(a, n)])` ran
//! `a n` first. `thread_subexprs` now binds every non-trivial operand before
//! the last one to a `let` temp in source order. Each case here checks that
//! the first side-effecting send is bound ahead of the construct and the last
//! one stays in place inside it; the behaviour itself is pinned by
//! `stdlib/test/eval_order_test.bt`.

use super::{assert_compiles_through_erlc, codegen};

pub(super) const SRC: &str = "Value subclass: Probe
  field: x = 0
  field: y = 0
  arrayOf: a => #[a bump, a n]
  listOf: a => #(a bump, a n)
  listTailOf: a => #(a bump | a rest)
  tupleOf: a => {a bump, a n}
  mapOf: a => #{#first => a bump, #second => a n}
  argsOf: a with: h => h pair: a bump with: a n
  oneSend: a => #[a bump, 1, #x]
  fieldReads => self.x + self.y
  cascadeOf: a with: h => h yourself; pair: a bump with: a n
  ffiOf: a => Erlang lists seq: a bump with: a n
  superOf: a => super pair: a bump with: a n
  class arrayOf: a => #[a bump, a n]
";

/// The text of method `name`'s function definition in `code`.
fn method_body<'a>(code: &'a str, name: &str) -> &'a str {
    // At a line start: the module header's export list names it first.
    let head = format!("\n'{name}'/");
    let start = code
        .find(&head)
        .unwrap_or_else(|| panic!("no function {name} in:\n{code}"));
    let rest = &code[start..];
    let end = rest.find("\n\n").unwrap_or(rest.len());
    &rest[..end]
}

/// Asserts that in method `name`, the send of `first` is let-bound before the
/// construct opened by `opener` and the send of `last` is evaluated inside it.
fn assert_first_bound_before(code: &str, name: &str, opener: &str, first: &str, last: &str) {
    let body = method_body(code, name);
    let at_opener = body
        .find(opener)
        .unwrap_or_else(|| panic!("`{opener}` not in {name}:\n{body}"));
    let (before, inside) = body.split_at(at_opener);
    let first_send = format!("'{first}', [])");
    let last_send = format!("'{last}', [])");
    assert!(
        before.contains(&first_send) && !inside.contains(&first_send),
        "`{first}` should be let-bound before `{opener}` in {name}:\n{body}"
    );
    assert!(
        inside.contains(&last_send),
        "`{last}` should be evaluated in place inside `{opener}` in {name}:\n{body}"
    );
}

#[test]
fn bt4296_collection_literal_elements_bind_in_source_order() {
    let code = codegen(SRC);
    assert_first_bound_before(&code, "arrayOf:", "'from_list'([", "bump", "n");
    assert_first_bound_before(&code, "listOf:", "[_ListLit@", "bump", "n");
    assert_first_bound_before(&code, "listTailOf:", "[_ListLit@", "bump", "rest");
    assert_first_bound_before(&code, "tupleOf:", "{_TupLit@", "bump", "n");
    assert_first_bound_before(&code, "mapOf:", "~{ 'first' => _MapLit@", "bump", "n");
    assert_first_bound_before(&code, "class_arrayOf:", "'from_list'([", "bump", "n");
    assert_compiles_through_erlc("test", &code);
}

#[test]
fn bt4296_runtime_dispatch_arguments_bind_in_source_order() {
    let code = codegen(SRC);
    assert_first_bound_before(&code, "argsOf:with:", "'pair:with:', [", "bump", "n");
}

#[test]
fn bt4296_cascade_ffi_and_super_arguments_bind_in_source_order() {
    let code = codegen(SRC);
    // A cascade hoists every argument once any is hoisted (all or nothing).
    let cascade = method_body(&code, "cascadeOf:with:");
    let (before, inside) = cascade.split_at(cascade.find("'pair:with:', [").expect("send"));
    let (bump, n) = (before.find("'bump', [])"), before.find("'n', [])"));
    assert!(
        bump.zip(n).is_some_and(|(b, n)| b < n) && !inside.contains("'bump', [])"),
        "cascade arguments should be bound in source order:\n{cascade}"
    );
    assert_first_bound_before(&code, "ffiOf:", "'seq', [", "bump", "n");
    assert_first_bound_before(&code, "superOf:", "'super_value'(", "bump", "n");
    assert_compiles_through_erlc("test", &code);
}

#[test]
fn bt4296_field_reads_do_not_race() {
    // A field read runs no code, so `self.x + self.y` needs no temp — the
    // BT-4279 fast path relies on an empty operand prelude.
    let code = codegen(SRC);
    let body = method_body(&code, "fieldReads");
    assert!(
        !body.contains("_BinOp@"),
        "no operand temp expected:\n{body}"
    );
}

/// The actor-closure column (ADR 0137 §3): inside a block an actor's sends
/// are not sync-bracketed, so a `super` send's arguments and a field read (a
/// live cell lookup) racing a sibling send are bound in source order too.
const ACTOR_SRC: &str = "Actor subclass: ActorProbe
  state: n = 0
  blockSuperOf: a => #(1) collect: [:_i | super pair: a bump with: a n]
  blockFieldOf: blk => #(1) collect: [:_i | #[self.n, blk value]]
";

/// The text of actor method `name`'s clause in the generated dispatch `case`.
fn actor_method_body<'a>(code: &'a str, name: &str) -> &'a str {
    let head = format!("<'{name}'> when 'true' ->");
    let start = code
        .find(&head)
        .unwrap_or_else(|| panic!("no dispatch clause for {name} in:\n{code}"));
    let rest = &code[start + head.len()..];
    let end = rest.find("\n        <'").unwrap_or(rest.len());
    &rest[..end]
}

#[test]
fn bt4296_actor_closure_operands_bind_in_source_order() {
    let code = super::codegen_named(ACTOR_SRC, "actor_probe");
    // `super` inside a block: `bump` bound ahead of the argument list.
    let body = actor_method_body(&code, "blockSuperOf:");
    let at = body.find("'pair:with:', [").expect("super send");
    let (before, inside) = body.split_at(at);
    assert!(
        before.contains("'bump', [])") && !inside[..inside.find(']').unwrap()].contains("'bump'"),
        "`bump` should be let-bound before the super argument list:\n{body}"
    );
    // A cell field read before `blk value`: bound ahead of the array.
    let body = actor_method_body(&code, "blockFieldOf:");
    let at = body.find("'from_list'([").expect("array literal");
    let inside = &body[at..];
    let elems = &inside[..inside.find("])").expect("array end")];
    assert!(
        elems.starts_with("'from_list'([_ArrLit@"),
        "the cell field read should be let-bound ahead of the array:\n{body}"
    );
    assert_compiles_through_erlc("actor_probe", &code);
}

#[test]
fn bt4296_a_single_side_effecting_operand_stays_in_place() {
    let code = codegen(SRC);
    let body = method_body(&code, "oneSend:");
    assert!(
        body.contains("'from_list'([call 'beamtalk_message_dispatch':'send'(_a@"),
        "a lone non-trivial element has nothing to race with and needs no temp:\n{body}"
    );
    assert!(!body.contains("_ArrLit@"), "no temp expected:\n{body}");
}
