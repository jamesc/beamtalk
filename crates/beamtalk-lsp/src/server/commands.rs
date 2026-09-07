// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `workspace/executeCommand` command identifiers, request-argument
//! validation, and translation of a command invocation into the
//! corresponding beamtalk-core tool expression.

use beamtalk_core::tool_expr::{
    FlushFilter, flush_expr, precheck_method_expr, remove_method_expr,
    remove_method_if_absent_expr, save_class_expr,
};

/// ADR 0082 Phase 3: LSP `workspace/executeCommand` identifiers
/// surfaced to clients. Each command compiles to a Beamtalk expression
/// submitted via the workspace's existing `evaluate` REPL op — no new
/// workspace-side dispatch is added (per ADR 0082 "Rationale: why no new REPL
/// ops"). Keep the names stable; editors bind to them by string match.
pub(crate) const CMD_FLUSH: &str = "beamtalk.flush";
pub(crate) const CMD_FLUSH_CLASS: &str = "beamtalk.flush.class";
pub(crate) const CMD_FLUSH_FILE: &str = "beamtalk.flush.file";
pub(crate) const CMD_FLUSH_KIND: &str = "beamtalk.flush.kind";
pub(crate) const CMD_SAVE_CLASS: &str = "beamtalk.saveClass";
/// ADR 0105 Phase 3: the editor's "check before save" pre-save
/// advisory hook — compiles a pending method edit and reports would-be-stale
/// dependents without installing it. Non-blocking; the caller decides
/// whether/when to follow up with the real save (`compile:source:` via
/// whichever surface the editor uses for that).
pub(crate) const CMD_PRECHECK_METHOD: &str = "beamtalk.precheckMethod";
/// ADR 0105 Phase 3: the explicit whole-image re-check
/// (`Workspace recheckImage` / REPL `:recheck image`).
pub(crate) const CMD_RECHECK_IMAGE: &str = "beamtalk.recheckImage";
/// ADR 0112 Phase 4: remove a method from a class
/// (`Behaviour>>removeSelector:` / `removeSelector:ifAbsent:`). Its
/// expression shape is shared with MCP's `remove_method` tool via
/// `beamtalk_core::tool_expr::remove_method_expr` — the two can't
/// drift, since both call the same function.
pub(crate) const CMD_REMOVE_METHOD: &str = "beamtalk.removeMethod";
/// All commands surfaced via `executeCommand`. Wired into
/// `ServerCapabilities::execute_command_provider` and used by the LSP→runtime
/// dispatch in [`Backend::execute_command`].
pub(crate) const BEAMTALK_LSP_COMMANDS: &[&str] = &[
    CMD_FLUSH,
    CMD_FLUSH_CLASS,
    CMD_FLUSH_FILE,
    CMD_FLUSH_KIND,
    CMD_SAVE_CLASS,
    CMD_PRECHECK_METHOD,
    CMD_RECHECK_IMAGE,
    CMD_REMOVE_METHOD,
];
/// ADR 0082 Phase 3: map a `workspace/executeCommand` invocation
/// to the Beamtalk expression that compiles to the same effect on the live
/// workspace. Delegates the actual expression construction to
/// `beamtalk_core::tool_expr`, the same shared builders `beamtalk-mcp`'s
/// typed tools call — a single implementation both surfaces call into, so
/// REPL `:flush` ≡ MCP `flush` ≡ LSP `beamtalk.flush` can't drift apart
/// (see `beamtalk_core::tool_expr`'s module docs and unit tests for
/// the enforcing conformance suite).
///
/// Returns the expression string on success or a human-readable parameter
/// error on failure (which the LSP layer surfaces as `invalid_params`).
pub(crate) fn build_command_expression(
    command: &str,
    arguments: &[serde_json::Value],
) -> std::result::Result<String, String> {
    match command {
        CMD_FLUSH => {
            // No arguments — `Workspace flush` on the whole pending set.
            if !arguments.is_empty() {
                return Err(format!(
                    "{CMD_FLUSH}: expected no arguments, got {}",
                    arguments.len()
                ));
            }
            Ok(flush_expr(FlushFilter::None))
        }
        CMD_FLUSH_CLASS => {
            let class = expect_string_arg(arguments, 0, "class")?;
            validate_class_name(&class)?;
            // The class is named literally in the expression (no escaping);
            // validation above prevents any shape that could parse
            // differently.
            Ok(flush_expr(FlushFilter::Class(&class)))
        }
        CMD_FLUSH_FILE => {
            let file = expect_string_arg(arguments, 0, "file")?;
            Ok(flush_expr(FlushFilter::File(&file)))
        }
        CMD_FLUSH_KIND => {
            let kind = expect_string_arg(arguments, 0, "kind")?;
            // Allow either bare `new-class` or `#'new-class'`. Emit the
            // quoted-symbol form so hyphenated kinds parse cleanly.
            let bare = kind.strip_prefix('#').unwrap_or(&kind);
            let bare = bare.trim_matches('\'');
            if bare.is_empty()
                || !bare
                    .chars()
                    .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
            {
                return Err(format!(
                    "{CMD_FLUSH_KIND}: 'kind' must be an identifier (letters, digits, '-' or '_'); got '{kind}'"
                ));
            }
            Ok(flush_expr(FlushFilter::Kind(bare)))
        }
        CMD_SAVE_CLASS => {
            let source = expect_string_arg(arguments, 0, "source")?;
            let path = expect_string_arg(arguments, 1, "path")?;
            if source.is_empty() {
                return Err(format!("{CMD_SAVE_CLASS}: 'source' must not be empty"));
            }
            if path.is_empty() {
                return Err(format!("{CMD_SAVE_CLASS}: 'path' must not be empty"));
            }
            Ok(save_class_expr(&source, &path))
        }
        CMD_PRECHECK_METHOD => {
            let class = expect_string_arg(arguments, 0, "class")?;
            validate_class_name(&class)?;
            let selector = expect_string_arg(arguments, 1, "selector")?;
            // Accepted with or without a leading '#', mirroring MCP's
            // `precheck_method` tool so both surfaces agree on input shape.
            let selector = selector.strip_prefix('#').unwrap_or(&selector);
            validate_selector(selector)?;
            let source = expect_string_arg(arguments, 2, "source")?;
            if source.is_empty() {
                return Err(format!("{CMD_PRECHECK_METHOD}: 'source' must not be empty"));
            }
            Ok(precheck_method_expr(&class, selector, &source))
        }
        CMD_RECHECK_IMAGE => {
            if !arguments.is_empty() {
                return Err(format!(
                    "{CMD_RECHECK_IMAGE}: expected no arguments, got {}",
                    arguments.len()
                ));
            }
            Ok("Workspace recheckImage".to_string())
        }
        CMD_REMOVE_METHOD => {
            let class = expect_string_arg(arguments, 0, "class")?;
            validate_class_name(&class)?;
            let selector = expect_string_arg(arguments, 1, "selector")?;
            // Accepted with or without a leading '#', mirroring MCP's
            // `remove_method` tool so both surfaces agree on input shape.
            let selector = selector.strip_prefix('#').unwrap_or(&selector);
            validate_selector(selector)?;
            // Optional third argument: an `ifAbsent:` fallback. Unlike
            // `CMD_PRECHECK_METHOD`'s `source`, this is raw Beamtalk
            // expression code embedded as the fallback block's body, not a
            // String value passed to a `compile:source:`-style primitive.
            // Missing and explicit `null` are both treated as "no fallback"
            // — some JSON-RPC clients pad positional arguments with `null`
            // rather than omitting the trailing slot.
            if matches!(arguments.get(2), None | Some(serde_json::Value::Null)) {
                return Ok(remove_method_expr(&class, selector));
            }
            let if_absent = expect_string_arg(arguments, 2, "ifAbsent")?;
            Ok(remove_method_if_absent_expr(&class, selector, &if_absent))
        }
        _ => Err(format!("unknown LSP command: {command}")),
    }
}
/// Extract the i-th argument from an `executeCommand` invocation as a string.
/// LSP clients pack arguments as a `Vec<Value>`; we accept either a bare
/// JSON string or an object whose `name` field is the value the parameter
/// expects, to be friendly to both raw JSON-RPC callers and editors that wrap
/// arguments in objects.
pub(in crate::server) fn expect_string_arg(
    arguments: &[serde_json::Value],
    index: usize,
    name: &str,
) -> std::result::Result<String, String> {
    let value = arguments
        .get(index)
        .ok_or_else(|| format!("missing argument {index} ({name})"))?;
    match value {
        serde_json::Value::String(s) => Ok(s.clone()),
        serde_json::Value::Object(map) => {
            if let Some(serde_json::Value::String(s)) = map.get(name) {
                Ok(s.clone())
            } else {
                Err(format!(
                    "argument {index} must be a string or an object with a '{name}' string field"
                ))
            }
        }
        _ => Err(format!(
            "argument {index} ({name}) must be a string, got {value}"
        )),
    }
}
/// Validate that a class name argument is a Beamtalk identifier
/// (`PascalCase`, ASCII-only, underscores OK). Prevents an injection-shaped
/// argument like `"X. delete Workspace; X"` from being concatenated into the
/// flush expression. Tight on purpose — Beamtalk class names are a closed
/// alphabet.
pub(in crate::server) fn validate_class_name(name: &str) -> std::result::Result<(), String> {
    if beamtalk_core::source_analysis::is_valid_class_name(name) {
        return Ok(());
    }
    if name.is_empty() {
        return Err("class name must not be empty".to_string());
    }
    if !name.starts_with(|c: char| c.is_ascii_uppercase()) {
        return Err(format!(
            "class name '{name}' must start with an uppercase letter"
        ));
    }
    // is_valid_class_name returned false, name is non-empty with uppercase start,
    // so there must be at least one disallowed character.
    let c = name
        .chars()
        .find(|c| !(c.is_ascii_alphanumeric() || *c == '_'))
        .expect(
            "invariant: is_valid_class_name=false, non-empty, starts-uppercase → must have bad char",
        );
    Err(format!(
        "class name '{name}' contains invalid character '{c}' (allowed: letters, digits, underscore)"
    ))
}
/// Validate a Beamtalk selector (unary, keyword, or binary) before splicing
/// it unescaped into a `#selector` literal in a built expression (ADR 0105
/// Phase 3's `CMD_PRECHECK_METHOD`). Delegates to the canonical
/// implementation in `beamtalk_core::source_analysis::validate_selector_input`.
pub(in crate::server) fn validate_selector(sel: &str) -> std::result::Result<(), String> {
    beamtalk_core::source_analysis::validate_selector_input(sel)
}
