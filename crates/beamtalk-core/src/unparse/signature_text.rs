// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared single-line signature-text composer (BT-3097).
//!
//! **DDD Context:** Language Service — Formatting / Unparse
//!
//! Six independent implementations of "render a method/function signature as
//! text" had drifted across the codebase: the `bt fmt` unparser, the hover
//! provider (twice — a signature renderer and a separate type-annotation
//! renderer), the `beamtalk doc` extractor, generated `.bt` stubs, and
//! signature help. They legitimately want different *levels of detail*
//! (whether types are shown, whether a parameter name is available at all,
//! whether a return arrow is present) but the actual list-of-parameters
//! composition — join the keyword parts with the parameter names and
//! optional types, one space between fragments — is exactly the same
//! problem everywhere.
//!
//! This module is that shared composition core. It is deliberately **not**
//! coupled to [`crate::ast::TypeAnnotation`]: callers render a parameter's
//! type from whatever domain-specific representation they hold —
//! `TypeAnnotation` via [`super::unparse_type_annotation_display`] for
//! AST-based consumers, pre-stringified `ClassHierarchy::MethodInfo` fields,
//! or `InferredType` from the native type registry
//! (`beamtalk_cli::commands::generate::stubs::format_type`) — and pass the
//! already-rendered text in as `&str`. That text-in/text-out contract is
//! what lets one composer serve every consumer without an AST adapter for
//! the structurally different native-type input (see BT-3097's discussion of
//! why `generate::stubs` can't just be merged into an AST-shaped renderer).
//!
//! The `bt fmt` unparser itself ([`super::unparse_method_signature`] et al.)
//! does *not* go through this module — it must stay on the `Document` API
//! for width-aware line breaking of long signatures (ADR 0044), which a
//! flat string composer cannot provide. This module is for the single-line,
//! non-breaking display consumers (hover, doc extraction, stub generation,
//! signature help).

use std::fmt::Write as _;

/// One parameter (or keyword-part) slot in a rendered signature.
///
/// `keyword` already carries its trailing `:` for a keyword-message part
/// (`"at:"`), or is the bare operator for a binary message (`"+"`) — this
/// mirrors [`crate::ast::KeywordPart::keyword`], which stores the colon too,
/// so AST-based callers can pass it straight through.
#[derive(Debug, Clone, Copy)]
pub struct SignatureParam<'a> {
    /// The keyword token (with trailing `:`) or bare binary operator.
    pub keyword: &'a str,
    /// The parameter's binding name, when the source has one. `None` for
    /// sources that carry only a resolved type per parameter with no name —
    /// `ClassHierarchy::MethodInfo` (used by hover's resolved-call display
    /// and signature help) is the motivating case.
    pub name: Option<&'a str>,
    /// Pre-rendered type-annotation text, when the consumer displays types.
    pub type_text: Option<&'a str>,
}

impl SignatureParam<'_> {
    /// Renders this parameter's fragment alone, e.g. `"at: index :: Integer"`,
    /// `"at: Integer"` (no name available), `"at: index"` (name only, no
    /// type shown), or bare `"at:"` (neither).
    #[must_use]
    pub fn render(&self) -> String {
        let mut out = self.keyword.to_string();
        match (self.name, self.type_text) {
            (Some(name), Some(ty)) => {
                let _ = write!(out, " {name} :: {ty}");
            }
            (Some(name), None) => {
                let _ = write!(out, " {name}");
            }
            (None, Some(ty)) => {
                let _ = write!(out, " {ty}");
            }
            (None, None) => {}
        }
        out
    }
}

/// The selector shape of a rendered signature: a bare unary name, or a
/// binary/keyword selector expressed as one-or-more [`SignatureParam`]s
/// (a binary selector is a single param whose `keyword` is the operator).
#[derive(Debug, Clone, Copy)]
pub enum SignatureSelector<'a> {
    /// A unary selector — no parameters (`"size"`, `"printString"`).
    Unary(&'a str),
    /// A binary (single-element slice) or keyword (multi-element slice)
    /// selector's parameters, already built as [`SignatureParam`]s.
    Params(&'a [SignatureParam<'a>]),
}

/// The optional pieces around a rendered signature — the legitimate
/// per-consumer differences identified while unifying the diverged
/// signature renderers (BT-3097).
#[derive(Debug, Clone, Copy)]
pub struct SignatureRenderOptions {
    /// Text before the selector (`"class "`, or empty for none). Method
    /// declarations' `sealed `/`internal ` prefixes are handled by the
    /// `bt fmt` unparser directly (see module docs) since only that
    /// consumer needs them.
    pub prefix: &'static str,
    /// Whether a present return type is appended as `" -> Type"`.
    pub show_return_type: bool,
    /// Text appended at the very end (`" =>"` for a full method
    /// declaration line, empty for a display-only signature).
    pub suffix: &'static str,
}

impl SignatureRenderOptions {
    /// The common case: no prefix, show the return type, no suffix. Used by
    /// hover's declaration signature, resolved-call signature, and
    /// generated stub signatures.
    pub const DISPLAY: Self = Self {
        prefix: "",
        show_return_type: true,
        suffix: "",
    };

    /// Names/keywords only, no return type — the `beamtalk doc` extractor's
    /// listing style, which intentionally omits types (BT-3097).
    pub const NAMES_ONLY: Self = Self {
        prefix: "",
        show_return_type: false,
        suffix: "",
    };
}

/// Renders a signature from its selector shape, optional pre-rendered
/// return-type text, and [`SignatureRenderOptions`] — the shared core for
/// every single-line (non-`bt fmt`) signature display in the codebase
/// (BT-3097).
#[must_use]
pub fn render_signature_text(
    selector: SignatureSelector<'_>,
    return_type: Option<&str>,
    options: &SignatureRenderOptions,
) -> String {
    let mut out = options.prefix.to_string();

    match selector {
        SignatureSelector::Unary(name) => out.push_str(name),
        SignatureSelector::Params(params) => {
            for (i, param) in params.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                out.push_str(&param.render());
            }
        }
    }

    if options.show_return_type {
        if let Some(ret) = return_type {
            let _ = write!(out, " -> {ret}");
        }
    }

    out.push_str(options.suffix);
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    // --- SignatureParam::render ---

    #[test]
    fn param_render_with_name_and_type() {
        let p = SignatureParam {
            keyword: "at:",
            name: Some("index"),
            type_text: Some("Integer"),
        };
        assert_eq!(p.render(), "at: index :: Integer");
    }

    #[test]
    fn param_render_name_only() {
        let p = SignatureParam {
            keyword: "at:",
            name: Some("index"),
            type_text: None,
        };
        assert_eq!(p.render(), "at: index");
    }

    #[test]
    fn param_render_type_only_no_name() {
        // ClassHierarchy::MethodInfo has no parameter names — only types.
        let p = SignatureParam {
            keyword: "at:",
            name: None,
            type_text: Some("Integer"),
        };
        assert_eq!(p.render(), "at: Integer");
    }

    #[test]
    fn param_render_bare_keyword() {
        let p = SignatureParam {
            keyword: "at:",
            name: None,
            type_text: None,
        };
        assert_eq!(p.render(), "at:");
    }

    // --- render_signature_text: unary/binary/keyword shapes ---

    #[test]
    fn unary_selector() {
        let out = render_signature_text(
            SignatureSelector::Unary("size"),
            None,
            &SignatureRenderOptions::DISPLAY,
        );
        assert_eq!(out, "size");
    }

    #[test]
    fn binary_selector_with_param() {
        let params = [SignatureParam {
            keyword: "+",
            name: Some("other"),
            type_text: Some("Number"),
        }];
        let out = render_signature_text(
            SignatureSelector::Params(&params),
            None,
            &SignatureRenderOptions::DISPLAY,
        );
        assert_eq!(out, "+ other :: Number");
    }

    #[test]
    fn keyword_selector_multiple_params() {
        let params = [
            SignatureParam {
                keyword: "at:",
                name: Some("index"),
                type_text: Some("Integer"),
            },
            SignatureParam {
                keyword: "put:",
                name: Some("value"),
                type_text: Some("Object"),
            },
        ];
        let out = render_signature_text(
            SignatureSelector::Params(&params),
            Some("Object"),
            &SignatureRenderOptions::DISPLAY,
        );
        assert_eq!(out, "at: index :: Integer put: value :: Object -> Object");
    }

    // --- Options: return type, prefix, suffix ---

    #[test]
    fn return_type_hidden_when_show_return_type_false() {
        let out = render_signature_text(
            SignatureSelector::Unary("size"),
            Some("Integer"),
            &SignatureRenderOptions::NAMES_ONLY,
        );
        assert_eq!(out, "size");
    }

    #[test]
    fn missing_return_type_appends_nothing_even_when_shown() {
        let out = render_signature_text(
            SignatureSelector::Unary("size"),
            None,
            &SignatureRenderOptions::DISPLAY,
        );
        assert_eq!(out, "size");
    }

    #[test]
    fn prefix_and_suffix_wrap_the_signature() {
        let opts = SignatureRenderOptions {
            prefix: "class ",
            show_return_type: true,
            suffix: " =>",
        };
        let out = render_signature_text(SignatureSelector::Unary("size"), Some("Integer"), &opts);
        assert_eq!(out, "class size -> Integer =>");
    }

    // --- Family-B (no parameter names): hover's resolved-call display and
    // signature help both consume `ClassHierarchy::MethodInfo`, which only
    // has types, not names (BT-3097).

    #[test]
    fn no_param_names_renders_keyword_type_pairs() {
        let params = [
            SignatureParam {
                keyword: "deposit:",
                name: None,
                type_text: Some("Integer"),
            },
            SignatureParam {
                keyword: "into:",
                name: None,
                type_text: Some("Account"),
            },
        ];
        let out = render_signature_text(
            SignatureSelector::Params(&params),
            Some("Integer"),
            &SignatureRenderOptions::DISPLAY,
        );
        assert_eq!(out, "deposit: Integer into: Account -> Integer");
    }
}
