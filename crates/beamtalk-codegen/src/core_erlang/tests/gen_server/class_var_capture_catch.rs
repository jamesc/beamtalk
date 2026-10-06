// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0130 §4/§5 codegen: a block literal that reads a class variable binds a
//! creation-time capture (and only such a block), and every compiled `on:do:`
//! is a catch boundary (`snapshot/0` before the `try`, `restore/1` first in the
//! non-NLR catch arm, after both `$bt_nlr` pass-through arms).

use super::class_var_access::function_text;
use super::*;

const CAPTURE_CALL: &str = "call 'beamtalk_class_vars':'capture'(ClassSelf, ";

/// The variable a `let <Var> = call 'beamtalk_class_vars':'capture'(ClassSelf, ` binds,
/// for the `n`th (0-based) capture in `text`.
fn capture_var(text: &str, n: usize) -> String {
    let at = text
        .match_indices(CAPTURE_CALL)
        .nth(n)
        .unwrap_or_else(|| panic!("capture #{n} not found in:\n{text}"))
        .0;
    let before = &text[..at];
    let let_at = before.rfind("let ").expect("capture is a let binding");
    before[let_at + 4..]
        .trim_end_matches([' ', '='])
        .to_string()
}

/// ADR 0130 §5: only a block that reads a class variable binds a capture; the
/// read's miss path takes it; writes and capture-free blocks bind nothing.
#[test]
fn capture_is_bound_only_where_a_block_reads_a_class_variable() {
    let src = concat!(
        "Object subclass: Counter\n",
        "  classState: n = 0\n\n",
        "  class reader => [:x | x + self.n]\n\n",
        "  class writer => [:x | self.n := x]\n\n",
        "  class plain => [:x | x + 1]\n\n",
        "  class presence => [:x | self hasField: x]\n\n",
        "  class methodLevelRead => self.n\n",
    );
    let code = codegen(src);

    let reader = function_text(&code, "'class_reader'/1 = fun");
    let var = capture_var(reader, 0);
    assert_eq!(reader.matches(CAPTURE_CALL).count(), 1, "{reader}");
    assert!(
        reader.contains(&format!("{CAPTURE_CALL}'none') in fun (")),
        "a block created at method level captures with Outer = 'none', around the fun: {reader}"
    );
    assert!(
        reader.contains(&format!(
            "call 'beamtalk_class_vars':'get'(ClassSelf, 'n', {var})"
        )),
        "the read's miss path is the 3-arity captured fallback: {reader}"
    );
    assert!(
        !reader.contains("call 'beamtalk_class_vars':'get'(ClassSelf, 'n')"),
        "no 2-arity fallback is left inside the block: {reader}"
    );
    assert!(
        reader.contains("call 'maps':'find'('n', "),
        "the inlined hit path stays as is: {reader}"
    );

    let writer = function_text(&code, "'class_writer'/1 = fun");
    assert!(
        !writer.contains("'capture'"),
        "a block that only writes binds no capture: {writer}"
    );
    assert!(
        writer.contains("call 'beamtalk_class_vars':'put'(ClassSelf, 'n', "),
        "writes keep the ordinary put: {writer}"
    );

    let plain = function_text(&code, "'class_plain'/1 = fun");
    assert!(
        !plain.contains("beamtalk_class_vars") && !plain.contains("$bt_class_vars"),
        "a block that reads no class variable binds nothing: {plain}"
    );

    let presence = function_text(&code, "'class_presence'/1 = fun");
    let pvar = capture_var(presence, 0);
    assert!(
        presence.contains("call 'beamtalk_class_vars':'has'(ClassSelf, ")
            && presence.contains(&format!(", {pvar})")),
        "hasField: inside a block takes the capture too: {presence}"
    );

    let method_level = function_text(&code, "'class_methodLevelRead'/1 = fun");
    assert!(
        !method_level.contains("'capture'")
            && method_level.contains("call 'beamtalk_class_vars':'get'(ClassSelf, 'n')"),
        "a method-level read is the 2-arity form with no capture: {method_level}"
    );
    assert_compiles_through_erlc("test", &code);
}

/// A `late classState:` read inside a block uses `get_late/3`.
#[test]
fn late_class_var_read_in_a_block_uses_get_late_with_the_capture() {
    let src = concat!(
        "Object subclass: Lazy\n",
        "  late classState: cache :: Integer\n\n",
        "  class reader => [self.cache]\n",
    );
    let code = codegen(src);
    let reader = function_text(&code, "'class_reader'/1 = fun");
    let var = capture_var(reader, 0);
    assert!(
        reader.contains(&format!(
            "call 'beamtalk_class_vars':'get_late'(ClassSelf, 'cache', {var})"
        )),
        "{reader}"
    );
}

/// A block created inside a capturing block inherits its parent's capture:
/// the inner `capture/2` takes the outer capture variable as `Outer`, and the
/// inner read uses the inner capture. A block nested in a block that reads
/// nothing itself still binds when the nested block reads.
#[test]
fn nested_blocks_inherit_the_enclosing_capture() {
    let src = concat!(
        "Object subclass: Counter\n",
        "  classState: n = 0\n\n",
        "  class nested => [:a | [:b | a + b + self.n]]\n\n",
        "  class bothRead => [:a | self.n + [:b | b + self.n]]\n\n",
        "  class siblings => [[1] value + [self.n] value]\n",
    );
    let code = codegen(src);

    let nested = function_text(&code, "'class_nested'/1 = fun");
    assert_eq!(nested.matches(CAPTURE_CALL).count(), 2, "{nested}");
    let outer = capture_var(nested, 0);
    let inner = capture_var(nested, 1);
    assert!(
        nested.contains(&format!("{CAPTURE_CALL}'none')")),
        "the outer block is created at method level: {nested}"
    );
    assert!(
        nested.contains(&format!("{CAPTURE_CALL}{outer}) in fun (")),
        "the inner block's Outer is the enclosing block's capture: {nested}"
    );
    assert!(
        nested.contains(&format!(
            "call 'beamtalk_class_vars':'get'(ClassSelf, 'n', {inner})"
        )),
        "the inner read takes the inner capture: {nested}"
    );

    let both = function_text(&code, "'class_bothRead'/1 = fun");
    let outer = capture_var(both, 0);
    let inner = capture_var(both, 1);
    assert!(
        both.contains(&format!(
            "call 'beamtalk_class_vars':'get'(ClassSelf, 'n', {outer})"
        )) && both.contains(&format!(
            "call 'beamtalk_class_vars':'get'(ClassSelf, 'n', {inner})"
        )),
        "each block's reads use its own capture: {both}"
    );

    // Blocks that are inlined (a literal receiver of `value`) create no
    // closure and need no capture of their own; whichever form is chosen it
    // must still compile.
    let _ = function_text(&code, "'class_siblings'/1 = fun");
    assert_compiles_through_erlc("test", &code);
}

/// Inlined control flow inside a capturing block (a loop, an arm, an `on:do:`
/// body) inherits the block's capture on its miss path.
#[test]
fn inlined_scopes_inside_a_block_use_the_blocks_capture() {
    let src = concat!(
        "Object subclass: Counter\n",
        "  classState: n = 0\n\n",
        "  class scoped =>\n",
        "    [:xs |\n",
        "      xs do: [:x | self.n]\n",
        "      3 timesRepeat: [self.n]\n",
        "      xs isEmpty ifTrue: [self.n] ifFalse: [self.n + 1]\n",
        "      [self.n] on: Error do: [:e | self.n]\n",
        "    ]\n",
    );
    let code = codegen(src);
    let scoped = function_text(&code, "'class_scoped'/1 = fun");
    let var = capture_var(scoped, 0);
    // The loop/arm/protected-block literals are closures here (each is created
    // inside a block) and chain to the enclosing block's capture; none is
    // created with `none`, and none falls back to the 2-arity helper.
    assert_eq!(
        scoped.matches(&format!("{CAPTURE_CALL}'none')")).count(),
        1,
        "only the outermost block is created at method level: {scoped}"
    );
    assert!(
        scoped.matches(CAPTURE_CALL).count() > 1
            && scoped.matches(&format!("{CAPTURE_CALL}{var})")).count()
                == scoped.matches(CAPTURE_CALL).count() - 1,
        "every nested closure inherits the enclosing block's capture: {scoped}"
    );
    assert_eq!(
        scoped
            .matches("call 'beamtalk_class_vars':'get'(ClassSelf, 'n')")
            .count(),
        0,
        "no 2-arity miss path survives inside the block: {scoped}"
    );
    assert!(
        scoped
            .matches("call 'beamtalk_class_vars':'get'(ClassSelf, 'n', ")
            .count()
            >= 10,
        "every read falls back to a capture: {scoped}"
    );
    assert_compiles_through_erlc("test", &code);
}

/// Asserts the §4 order inside every compiled `on:do:` catch of `text` (each
/// is anchored by its `build_stacktrace` wrap, which only `on:do:` emits):
/// a snapshot immediately before each `try`, then, within the catch, the
/// 4-tuple NLR arm, the 3-tuple NLR arm, the restore, the wrap, the filter.
fn assert_catch_boundary_order(text: &str, expected_catches: usize) {
    let anchors: Vec<usize> = text
        .match_indices("primop 'build_stacktrace'(")
        .map(|(i, _)| i)
        .collect();
    assert_eq!(anchors.len(), expected_catches, "{text}");
    assert_eq!(
        text.matches("call 'beamtalk_class_vars':'snapshot'() in try")
            .count(),
        expected_catches,
        "a snapshot immediately precedes every try: {text}"
    );
    assert_eq!(
        text.matches("do call 'beamtalk_class_vars':'restore'(")
            .count(),
        expected_catches,
        "{text}"
    );
    for anchor in anchors {
        let catch_at = text[..anchor].rfind("catch <").expect("a catch precedes");
        let end = text[anchor..]
            .find("'beamtalk_exception_handler':'matches_class'")
            .map(|i| i + anchor)
            .expect("a class filter follows");
        let region = &text[catch_at..end];
        let four = region.find("{'$bt_nlr', ").expect("first NLR arm");
        let three = region[four + 1..]
            .find("{'$bt_nlr', ")
            .map(|i| i + four + 1)
            .expect("second NLR arm");
        let restore = region
            .find("do call 'beamtalk_class_vars':'restore'(")
            .unwrap_or_else(|| panic!("restore missing in: {region}"));
        assert!(
            four < three && three < restore,
            "NLR 4-tuple arm, NLR 3-tuple arm, then the restore: {region}"
        );
        // The 4-tuple arm carries the state element, the 3-tuple does not.
        assert!(region[four..three].matches(", ").count() >= 3, "{region}");
        // Between the restore and the class filter: only the wrap.
        assert!(
            region[restore..].contains("'ensure_wrapped'"),
            "the wrap follows the restore: {region}"
        );
    }
}

/// Every compiled `on:do:` is a catch boundary, in a class method (with and
/// without a writing handler), an instance-side actor method, a value-type
/// method, and a `class sealed` stateless method that callers direct-call;
/// `ensure:` emits nothing.
#[test]
fn on_do_emits_snapshot_and_restore_in_every_method_context() {
    // Class-side method of a class with class state (never direct-called);
    // the handler writes, so the threaded `on:do:` form is used as well.
    let class_side = codegen(concat!(
        "Object subclass: Counter\n",
        "  classState: n = 0\n\n",
        "  class plain => [1 / 0] on: Error do: [:e | 0]\n\n",
        "  class threaded =>\n",
        "    seen := 0\n",
        "    [1 / 0] on: Error do: [:e | seen := 1]\n",
        "    seen\n\n",
        "  class viaSend => [self.n := self.n + 1. 1 / 0] on: Error do: [:e | self.n]\n",
    ));
    assert_catch_boundary_order(function_text(&class_side, "'class_plain'/1 = fun"), 1);
    assert_catch_boundary_order(function_text(&class_side, "'class_threaded'/1 = fun"), 1);
    assert_catch_boundary_order(function_text(&class_side, "'class_viaSend'/1 = fun"), 1);
    assert_compiles_through_erlc("test", &class_side);

    // Instance-side actor method: the `try` body's `State` threading is untouched.
    let actor = codegen(concat!(
        "Actor subclass: Worker\n",
        "  state: count = 0\n\n",
        "  run => [1 / 0] on: Error do: [:e | 0]\n\n",
        "  runCounting =>\n",
        "    [self.count := self.count + 1. 1 / 0] on: Error do: [:e | self.count := 9]\n",
    ));
    assert_catch_boundary_order(&actor, 2);
    assert_compiles_through_erlc("test", &actor);

    // Value-type instance method.
    let value = codegen(concat!(
        "Value subclass: Box\n",
        "  state: n = 0\n\n",
        "  guarded => [1 / 0] on: Error do: [:e | 0]\n",
    ));
    assert_catch_boundary_order(&value, 1);
    assert_compiles_through_erlc("test", &value);

    // A `class sealed` method of a sealed, stateless class: callers direct-call
    // it, so it runs in the caller's process, where the boundary is the same
    // pass-through when no invocation is live.
    let sealed = codegen(concat!(
        "sealed Object subclass: Facade\n\n",
        "  class sealed guarded => [1 / 0] on: Error do: [:e | 0]\n",
    ));
    assert_catch_boundary_order(function_text(&sealed, "'class_guarded'/1 = fun"), 1);
    assert_compiles_through_erlc("test", &sealed);
}

#[test]
fn ensure_is_not_a_catch_boundary() {
    let code = codegen(concat!(
        "Object subclass: Counter\n",
        "  classState: n = 0\n\n",
        "  class guarded => [self.n := 1] ensure: [self.n := 0]\n",
    ));
    let guarded = function_text(&code, "'class_guarded'/1 = fun");
    assert!(
        !guarded.contains("'snapshot'") && !guarded.contains("'restore'"),
        "ensure: neither snapshots nor restores: {guarded}"
    );
}

/// Nested `on:do:` each get their own snapshot and a restore of their own.
#[test]
fn nested_on_do_each_snapshot_and_restore() {
    let code = codegen(concat!(
        "Object subclass: Counter\n",
        "  classState: n = 0\n\n",
        "  class nested =>\n",
        "    [[self.n := 1. 1 / 0] on: ZeroDivide do: [:e | e signal]] on: Error do: [:e | self.n]\n",
    ));
    let nested = function_text(&code, "'class_nested'/1 = fun");
    assert_eq!(
        nested
            .matches("call 'beamtalk_class_vars':'snapshot'() in try")
            .count(),
        2,
        "{nested}"
    );
    assert_eq!(nested.matches("'restore'(").count(), 2, "{nested}");
    assert_compiles_through_erlc("test", &code);
}

/// The capture walker and the lowering share `is_class_var_field_read` /
/// `is_class_var_has_field`. A `hasField:` that is a cascade message lowers
/// through `beamtalk_message_dispatch:send`, never through the `HasField`
/// intrinsic, so it emits no `beamtalk_class_vars:has` read and the block binds
/// no capture ("a block binds a capture exactly when something inside it lowers
/// to a read"); the plain-send spelling, which does lower to `has`, binds one.
#[test]
fn has_field_as_a_cascade_message_lowers_through_dispatch_and_binds_no_capture() {
    let src = concat!(
        "Object subclass: Counter\n",
        "  classState: n = 0\n\n",
        "  class cascaded => [:x | self hasField: #n; hasField: #m]\n",
        "  class plain => [:x | self hasField: #n]\n",
    );
    let code = codegen(src);
    let cascaded = function_text(&code, "'class_cascaded'/1 = fun");
    assert!(
        !cascaded.contains("'beamtalk_class_vars':'has'("),
        "a cascade `hasField:` is a dispatched send, not a `has` read: {cascaded}"
    );
    assert!(
        !cascaded.contains(CAPTURE_CALL),
        "nothing in the cascade reads through a capture: {cascaded}"
    );
    assert!(
        cascaded.contains("'beamtalk_message_dispatch':'send'("),
        "the cascade messages go through dispatch: {cascaded}"
    );
    let plain = function_text(&code, "'class_plain'/1 = fun");
    assert!(
        plain.contains("'beamtalk_class_vars':'has'(") && plain.contains(CAPTURE_CALL),
        "the plain `hasField:` lowers to `has` and binds a capture: {plain}"
    );
    assert_compiles_through_erlc("test", &code);
}

/// `self hasField:` is the constant `false` only where `ClassSelf` is `nil`: a
/// direct-called method (sealed class, no class variables, `class sealed`). A
/// `ClassBuilder` class-method fun declared *inside* such a method is never
/// direct-called: it runs with the built class as a real `ClassSelf` that
/// declares `classVars:`, so its `hasField:` asks `beamtalk_class_vars:has`
/// even though `current_method_selector` and `class_name()` still describe the
/// enclosing direct-called method.
#[test]
fn has_field_in_a_builder_fun_inside_a_direct_called_method_still_asks_the_runtime() {
    let src = concat!(
        "sealed Object subclass: Factory\n\n",
        "  class sealed plain => self hasField: #n\n",
        "  class sealed build =>\n",
        "    Object classBuilder name: #FactoryBuilt; superclass: Object; ",
        "classVars: #{ #n => 0 }; ",
        "classMethods: #{ #probe => [:self | self hasField: #n] }; register\n",
    );
    let code = codegen(src);
    let plain = function_text(&code, "'class_plain'/1 = fun");
    assert!(
        !plain.contains("'beamtalk_class_vars':'has'("),
        "a direct-called `self hasField:` has no ClassSelf to ask: {plain}"
    );
    let build = function_text(&code, "'class_build'/1 = fun");
    assert!(
        build.contains("'beamtalk_class_vars':'has'("),
        "the builder fun runs with a real ClassSelf declaring `n`, so it asks the runtime: {build}"
    );
    assert_compiles_through_erlc("test", &code);
}
