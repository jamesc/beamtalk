// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4301 (retiring BT-4029's convention): every class method, one that runs
//! in the caller included, is entered with the real class object as
//! `ClassSelf`. No caller passes `nil`, so no method renames its parameter or
//! resolves its class object in a prologue, and a sibling self-send passes
//! `ClassSelf` itself.

use super::class_var_access::function_text;
use super::*;

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

/// Every class method takes `ClassSelf` as its parameter and resolves nothing.
#[test]
fn every_class_method_is_entered_with_class_self() {
    let code = codegen(SRC);
    for header in [
        "'class_viaSelfNew'/1 = fun (ClassSelf) ->",
        "'class_viaNameNew'/1 = fun (ClassSelf) ->",
        "'class_me'/1 = fun (ClassSelf) ->",
        "'class_inBlock'/1 = fun (ClassSelf) ->",
        "'class_viaKeyword'/1 = fun (ClassSelf) ->",
        "'class_pure:'/2 = fun (ClassSelf, ",
        "'class_forward'/1 = fun (ClassSelf) ->",
        "'class_sibling'/1 = fun (ClassSelf) ->",
        "'class_viaOpenSelfNew'/1 = fun (ClassSelf) ->",
    ] {
        let text = function_text(&code, header);
        assert!(
            !text.contains("_DirectClassSelf") && !text.contains("<'nil'> when 'true' -> case"),
            "{header} must not resolve a nil class self: {text}"
        );
    }
    assert!(!code.contains("_DirectClassSelf"), "{code}");
    assert_compiles_through_erlc("test", &code);
}

/// A self-send to a sibling passes `ClassSelf`, and the keyword constructor
/// gets the class reference, never `nil` (BT-4037).
#[test]
fn self_sends_pass_class_self() {
    let code = codegen(SRC);
    let forward = function_text(&code, "'class_forward'/1 = fun");
    assert!(
        forward.contains("call 'test':'class_pure:'(ClassSelf, "),
        "a sibling self-send passes ClassSelf: {forward}"
    );
    let keyword = function_text(&code, "'class_viaKeyword'/1 = fun");
    assert!(
        keyword.contains("call 'test':'class_x:'(") && !keyword.contains("'nil'"),
        "the keyword constructor gets the class reference, not nil: {keyword}"
    );
}
