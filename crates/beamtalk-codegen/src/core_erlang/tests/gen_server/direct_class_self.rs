// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4029: a direct-called class method (ADR 0129 Phase 0b: sealed class, no
//! class variables, `class sealed`) is entered with `ClassSelf = nil`. When
//! its body reads `ClassSelf` (any class-side `self`), the parameter is renamed
//! and a prologue rebinds `ClassSelf` to the real class object; a body that
//! never reads it does no lookup.

use super::class_var_access::function_text;
use super::*;

const PROLOGUE: &str = "let ClassSelf = case _DirectClassSelf of <'nil'> when 'true' -> \
                        case call 'beamtalk_class_registry':'whereis_class'('Pt') of ";

const SRC: &str = concat!(
    "sealed Value subclass: Pt\n",
    "  field: x = 0\n\n",
    "  class sealed viaSelfNew => self new\n",
    "  class sealed viaNameNew => Pt new: #{#x => 1}\n",
    "  class sealed me => self\n",
    "  class sealed sibling => self viaSelfNew\n",
    "  class sealed forward => self pure: 1\n",
    "  class sealed inBlock => [self new] value\n",
    "  class sealed viaKeyword => Pt x: 2\n",
    "  class sealed pure: n => n + 1\n",
    "  class viaOpenSelfNew => self new\n",
);

/// Collapses runs of whitespace so the prologue matches across line breaks.
fn squash(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}

/// Every `ClassSelf`-reading form in a direct-called method gets the prologue.
#[test]
fn direct_called_methods_that_read_class_self_resolve_it_first() {
    let code = codegen(SRC);
    for header in [
        "'class_viaSelfNew'/1 = fun (_DirectClassSelf) ->",
        "'class_viaNameNew'/1 = fun (_DirectClassSelf) ->",
        "'class_me'/1 = fun (_DirectClassSelf) ->",
        "'class_inBlock'/1 = fun (_DirectClassSelf) ->",
    ] {
        let text = function_text(&code, header);
        assert!(
            squash(text).contains(PROLOGUE),
            "{header} must rebind ClassSelf before its body: {text}"
        );
    }
    assert_compiles_through_erlc("test", &code);
}

/// A direct-called body that never reads `ClassSelf` (a keyword constructor,
/// plain arithmetic, a forward to a direct-called sibling, which passes the
/// raw incoming class-self along) does no lookup, and neither does a method
/// that is not direct-called.
#[test]
fn methods_that_do_not_read_class_self_do_no_lookup() {
    let code = codegen(SRC);
    for header in [
        "'class_viaKeyword'/1 = fun (_DirectClassSelf) ->",
        "'class_pure:'/2 = fun (_DirectClassSelf, ",
        "'class_forward'/1 = fun (_DirectClassSelf) ->",
        "'class_sibling'/1 = fun (_DirectClassSelf) ->",
        "'class_viaOpenSelfNew'/1 = fun (ClassSelf) ->",
    ] {
        let text = function_text(&code, header);
        assert!(
            !text.contains("whereis_class"),
            "{header} must not resolve the class object: {text}"
        );
    }
    let forward = function_text(&code, "'class_forward'/1 = fun");
    assert!(
        forward.contains("call 'test':'class_pure:'(_DirectClassSelf, 1)"),
        "a direct-called sibling gets the raw incoming class-self: {forward}"
    );
    let keyword = function_text(&code, "'class_viaKeyword'/1 = fun");
    assert!(
        keyword.contains("call 'test':'class_x:'('nil', 2)"),
        "the keyword constructor ignores its class-self, so it gets nil: {keyword}"
    );
    assert_compiles_through_erlc("test", &code);
}
