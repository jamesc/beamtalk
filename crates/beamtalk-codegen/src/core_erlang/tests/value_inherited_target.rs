// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4260: a value type's `dispatch/3` default arm asks BT-4280's memoised
//! `beamtalk_dispatch:inherited_target/2` verdict and, on
//! `{'value', Owner, Module}`, calls the owner's `dispatch/3` directly instead
//! of walking the superclass `dispatch/3` chain with an extension lookup per
//! level. Every other verdict takes the unchanged route, and the module
//! exports the strictly-local `has_method_local/1` probe `resolve_owner/2`
//! walks with.

use super::codegen;

/// The `<_Other>` default arm of `dispatch/3`, up to the end of the function.
fn default_arm(code: &str) -> &str {
    let dispatch = code
        .find("'dispatch'/3 = fun (Selector, Args, Self) ->")
        .unwrap_or_else(|| panic!("no dispatch/3 in:\n{code}"));
    let rest = &code[dispatch..];
    let arm = rest
        .find("<_Other> when 'true' ->")
        .unwrap_or_else(|| panic!("no default arm in:\n{rest}"));
    let end = rest[arm..].find("\n\n").map_or(rest.len(), |e| arm + e);
    &rest[arm..end]
}

#[test]
fn value_default_arm_asks_the_inherited_verdict_first() {
    let code = codegen("Value subclass: Bt4260Point\n  field: x :: Integer = 0\n  sum => self.x\n");
    let arm = default_arm(&code);
    assert!(
        arm.contains("call 'beamtalk_dispatch':'inherited_target'('Bt4260Point', Selector)"),
        "the default arm must ask the verdict with its own class name. Got:\n{arm}"
    );
    let value_at = arm
        .find("<{'value', _InhOwner, InhModule}> when 'true' ->")
        .unwrap_or_else(|| panic!("no value-verdict arm in:\n{arm}"));
    let slow_at = arm
        .find("<_InhSlow> when 'true' ->")
        .unwrap_or_else(|| panic!("no slow arm in:\n{arm}"));
    let direct = &arm[value_at..slow_at];
    assert!(
        direct.contains("call InhModule:'dispatch'(Selector, Args, Self)"),
        "the value verdict must call the owner's dispatch/3. Got:\n{direct}"
    );
    let slow = &arm[slow_at..];
    assert!(
        slow.contains("call 'beamtalk_extensions':'lookup'('Bt4260Point', Selector)")
            && slow.contains("call 'bt@stdlib@value':'dispatch'(Selector, Args, Self)"),
        "every other verdict keeps the extension lookup and superclass delegation. Got:\n{slow}"
    );
    assert_eq!(
        arm.matches("InhModule:'dispatch'").count(),
        1,
        "only the value-verdict arm calls the owner directly. Got:\n{arm}"
    );
}

#[test]
fn selectors_the_superclass_defines_skip_the_verdict() {
    // `Value` defines `printString` itself (the built-in class hierarchy says
    // so), so it is one hop away from a `Value subclass:`.
    let code = codegen("Value subclass: Bt4260Sub\n  own => 3\n");
    let arm = default_arm(&code);
    assert!(
        arm.contains("<'printString'> when 'true' -> 'slow'"),
        "a selector the superclass defines takes the unchanged route. Got:\n{arm}"
    );
    assert!(
        !arm.contains("<'isKindOf:'> when 'true' -> 'slow'"),
        "a selector only Object defines is not one hop away. Got:\n{arm}"
    );
    assert!(
        arm.contains(
            "<_> when 'true' -> call 'beamtalk_dispatch':'inherited_target'('Bt4260Sub', Selector)"
        ),
        "every other selector asks the verdict. Got:\n{arm}"
    );
}

#[test]
fn class_without_instance_methods_asks_the_inherited_verdict() {
    let code = codegen("Value subclass: Bt4260Empty\n  class make => 1\n");
    let arm = default_arm(&code);
    assert!(
        arm.contains("call 'beamtalk_dispatch':'inherited_target'('Bt4260Empty', Selector)")
            && arm.contains("call InhModule:'dispatch'(Selector, Args, Self)"),
        "the minimal dispatch/3 asks the verdict too. Got:\n{arm}"
    );
}

#[test]
fn value_type_exports_and_defines_the_local_probe() {
    let code = codegen("Value subclass: Bt4260Probe\n  probe => 1\n");
    let header = code.lines().next().unwrap_or_default();
    assert!(
        header.contains("'has_method_local'/1"),
        "a value type exports has_method_local/1. Got:\n{header}"
    );
    let at = code
        .find("'has_method_local'/1 = fun (Selector) ->")
        .unwrap_or_else(|| panic!("no has_method_local/1 in:\n{code}"));
    let local = &code[at..];
    let body = &local[..local.find("\n\n").unwrap_or(local.len())];
    assert!(
        body.contains("'probe'")
            && body.contains("call 'beamtalk_extensions':'has'('Bt4260Probe', Selector)")
            && !body.contains(":'has_method'("),
        "the local probe lists the class's own selectors and extensions and never delegates. Got:\n{body}"
    );
}

#[test]
fn catch_all_dnu_class_does_not_ask_the_verdict() {
    let code = codegen(concat!(
        "Object subclass: Bt4260Catcher\n",
        "  doesNotUnderstand: selector :: Symbol args: arguments :: List -> Object =>\n",
        "    @intrinsic erlangApply\n",
    ));
    let arm = default_arm(&code);
    assert!(
        arm.contains(":'doesNotUnderstand:args:'(Self, Selector, Args)"),
        "the fixture must be a catch-all DNU class. Got:\n{arm}"
    );
    assert!(
        !arm.contains("inherited_target"),
        "a catch-all DNU class never delegates to its superclass, so it has no inherited route. Got:\n{arm}"
    );
}
