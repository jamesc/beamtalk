// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3724: the release-mode diagnostic for a verifier finding is built by a
//! `cfg`-independent function, so it is asserted here in every build profile.

use super::*;
use crate::core_erlang::threaded_ir::verify::verify_errors_to_diagnostic;
use beamtalk_core::source_analysis::{DiagnosticCategory, Severity};

#[test]
fn verifier_finding_is_an_internal_warning_with_its_own_category() {
    let errors = [VerifyError::ThreadingModeUnpackMismatch {
        mode: ThreadingMode::DirectParams,
        at: span(),
    }];
    let diag = verify_errors_to_diagnostic(&errors, "some invariant", Span::new(3, 9));

    assert_eq!(diag.severity, Severity::Warning, "must never block a build");
    assert_eq!(diag.category, Some(DiagnosticCategory::InternalVerifier));
    assert!(
        diag.message.starts_with("internal: some invariant: "),
        "message keeps the `internal:` prefix, got {:?}",
        diag.message
    );
    assert!(
        diag.message.contains("ThreadingModeUnpackMismatch"),
        "message names the violated invariant, got {:?}",
        diag.message
    );
    assert_eq!(diag.span, Span::new(3, 9));
}
