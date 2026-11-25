// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0116: the one operator → reflected-selector table.
//!
//! **DDD Context:** Semantic Analysis
//!
//! A number-on-the-left send `n <op> x` whose right operand is not a number
//! falls back to the reflected method `x <name>FromNumber: n`, sent to the
//! *right* operand with the *left* operand as its argument (ADR 0116
//! § Reflected method protocol). Codegen emits that fallback and the type
//! checker reasons about it, so the map lives here, below both
//! (`docs/development/architecture-principles.md` § Duplication & the
//! Shared-Leaf-Module Pattern). No other copy of these selector strings may
//! exist in Rust.

/// Which family a reflected operator is in. Callers pick the runtime entry
/// point from it: arithmetic falls back through
/// `beamtalk_message_dispatch:send_number_coercion/4`, ordering through
/// `send_number_comparison/4` (ADR 0116 § Addendum).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OperatorFamily {
    /// `+ - * /`.
    Arithmetic,
    /// `< <= > >=`.
    Ordering,
}

impl OperatorFamily {
    /// The family of `op`, or `None` when `op` has no reflected selector.
    #[must_use]
    pub fn of(op: &str) -> Option<Self> {
        reflected_operator(op).map(|r| r.family)
    }
}

/// An operator's number-on-the-left fallback: its family and the reflected
/// selector sent to the right operand.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ReflectedOperator {
    /// Arithmetic or ordering.
    pub family: OperatorFamily,
    /// The reflected keyword selector, e.g. `plusFromNumber:`.
    pub selector: &'static str,
}

/// The table: every operator with a number-on-the-left reflected selector.
const REFLECTED_OPERATORS: [(&str, ReflectedOperator); 8] = [
    ("+", arithmetic("plusFromNumber:")),
    ("-", arithmetic("minusFromNumber:")),
    ("*", arithmetic("timesFromNumber:")),
    ("/", arithmetic("divFromNumber:")),
    ("<", ordering("lessThanFromNumber:")),
    ("<=", ordering("lessOrEqualFromNumber:")),
    (">", ordering("greaterThanFromNumber:")),
    (">=", ordering("greaterOrEqualFromNumber:")),
];

const fn arithmetic(selector: &'static str) -> ReflectedOperator {
    ReflectedOperator {
        family: OperatorFamily::Arithmetic,
        selector,
    }
}

const fn ordering(selector: &'static str) -> ReflectedOperator {
    ReflectedOperator {
        family: OperatorFamily::Ordering,
        selector,
    }
}

/// The reflected selector and family for `op`, or `None` for any operator
/// outside `+ - * / < <= > >=` (`%`, `**`, `=:=`, keyword selectors, …).
#[must_use]
pub fn reflected_operator(op: &str) -> Option<ReflectedOperator> {
    REFLECTED_OPERATORS
        .iter()
        .find(|(name, _)| *name == op)
        .map(|(_, reflected)| *reflected)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_entry_maps_to_its_selector_and_family() {
        use OperatorFamily::{Arithmetic, Ordering};
        for (op, selector, family) in [
            ("+", "plusFromNumber:", Arithmetic),
            ("-", "minusFromNumber:", Arithmetic),
            ("*", "timesFromNumber:", Arithmetic),
            ("/", "divFromNumber:", Arithmetic),
            ("<", "lessThanFromNumber:", Ordering),
            ("<=", "lessOrEqualFromNumber:", Ordering),
            (">", "greaterThanFromNumber:", Ordering),
            (">=", "greaterOrEqualFromNumber:", Ordering),
        ] {
            assert_eq!(
                reflected_operator(op),
                Some(ReflectedOperator { family, selector }),
                "operator {op}"
            );
            assert_eq!(OperatorFamily::of(op), Some(family), "operator {op}");
        }
    }

    #[test]
    fn other_operators_have_no_entry() {
        for op in ["%", "**", "=:=", "==", "~=", ",", "at:", ""] {
            assert_eq!(reflected_operator(op), None, "operator {op:?}");
            assert_eq!(OperatorFamily::of(op), None, "operator {op:?}");
        }
    }
}
