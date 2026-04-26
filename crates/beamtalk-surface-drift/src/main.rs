// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Surface parity drift checker (BT-2082).
//!
//! Compares the inventory of REPL ops (Erlang), MCP tools (`#[tool(...)]`
//! attributes), REPL meta-commands (`:cmd` literals in the CLI dispatcher),
//! and LSP `ServerCapabilities` fields against the contract recorded in
//! `docs/development/surface-parity.md`.
//!
//! The check fails when any of the following drift conditions hold:
//!
//! * A REPL op exists in the runtime that is not listed in the parity doc.
//! * The parity doc lists a non–`surface-specific` binding for a REPL op
//!   on a surface where the corresponding code artifact is missing.
//! * An MCP tool is implemented in `crates/beamtalk-mcp/src/server.rs` but
//!   is not referenced anywhere in the parity doc.
//! * A REPL meta-command is dispatched in
//!   `crates/beamtalk-cli/src/commands/repl/mod.rs` but is not referenced
//!   in the parity doc.
//! * An LSP capability is enabled in `crates/beamtalk-lsp/src/server.rs`
//!   but is not referenced in the parity doc.
//!
//! Run via `just check-surface-drift` — the binary discovers the repo
//! root by walking up from `CARGO_MANIFEST_DIR`, so it can be invoked
//! from any working directory.

use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

/// Path fragments (relative to repo root) that the checker reads.
const PARITY_DOC: &str = "docs/development/surface-parity.md";
const REPL_OPS_DIR: &str = "runtime/apps/beamtalk_workspace/src";
const MCP_SERVER: &str = "crates/beamtalk-mcp/src/server.rs";
const REPL_DISPATCH: &str = "crates/beamtalk-cli/src/commands/repl/mod.rs";
const LSP_SERVER: &str = "crates/beamtalk-lsp/src/server.rs";

fn main() -> ExitCode {
    match run() {
        Ok(()) => {
            println!("surface-drift: OK");
            ExitCode::SUCCESS
        }
        Err(errors) => {
            eprintln!("surface-drift: {} drift error(s) detected\n", errors.len());
            for e in &errors {
                eprintln!("  - {e}");
            }
            eprintln!(
                "\nUpdate `docs/development/surface-parity.md` (the parity contract) \
                 or fix the surface bindings, then re-run `just check-surface-drift`."
            );
            ExitCode::FAILURE
        }
    }
}

fn run() -> Result<(), Vec<String>> {
    let repo_root = find_repo_root().map_err(|e| vec![e])?;
    let doc = ParityDoc::load(&repo_root.join(PARITY_DOC)).map_err(|e| vec![e])?;
    let code = CodeInventory::scan(&repo_root).map_err(|e| vec![e])?;
    let mut errors = Vec::new();
    check_drift(&doc, &code, &mut errors);
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// Walk upwards from `CARGO_MANIFEST_DIR` until we find a `VERSION` file.
fn find_repo_root() -> Result<PathBuf, String> {
    let manifest_dir = env_var("CARGO_MANIFEST_DIR")?;
    let mut dir: PathBuf = PathBuf::from(manifest_dir);
    loop {
        if dir.join("VERSION").exists() && dir.join("Cargo.toml").exists() {
            return Ok(dir);
        }
        if !dir.pop() {
            return Err("could not locate beamtalk repo root (no VERSION file found)".into());
        }
    }
}

fn env_var(name: &str) -> Result<String, String> {
    std::env::var(name).map_err(|_| format!("environment variable {name} not set"))
}

/// Parsed view of `docs/development/surface-parity.md`.
#[derive(Debug, Default)]
struct ParityDoc {
    /// Per-REPL-op binding map.
    ops: BTreeMap<String, OpBindings>,
    /// CLI subcommands referenced anywhere in the doc.
    cli_subcommands: BTreeSet<String>,
    /// REPL meta-commands referenced anywhere in the doc (e.g. `:test`).
    repl_meta: BTreeSet<String>,
    /// MCP tool names referenced anywhere in the doc.
    mcp_tools: BTreeSet<String>,
    /// LSP capabilities referenced anywhere in the doc (e.g. `textDocument/hover`).
    lsp_caps: BTreeSet<String>,
}

#[derive(Debug, Default, Clone)]
struct OpBindings {
    cli: CellState,
    repl_meta: CellState,
    mcp: CellState,
    lsp: CellState,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
enum CellState {
    /// `--`: not applicable (no binding required).
    #[default]
    NotApplicable,
    /// Binding name expected to exist in the corresponding source file.
    Bound(String),
    /// `surface-specific: …` or italics like `*(implicit: …)*` — accept whatever code does.
    SurfaceSpecific,
}

impl ParityDoc {
    fn load(path: &Path) -> Result<Self, String> {
        let raw = fs::read_to_string(path)
            .map_err(|e| format!("failed to read {}: {e}", path.display()))?;
        let mut doc = ParityDoc::default();
        let mut current_section = Section::Other;
        for line in raw.lines() {
            if let Some(rest) = line.strip_prefix("## ") {
                current_section = Section::classify(rest.trim());
                continue;
            }
            if !line.trim_start().starts_with('|') {
                continue;
            }
            // Skip the table delimiter row (|---|---|...).
            if line.contains("---") {
                continue;
            }
            let cells = split_table_row(line);
            // Header rows contain literal "REPL op" / "CLI subcommand" / etc.
            if cells.first().is_some_and(|c| {
                let lower = c.to_ascii_lowercase();
                lower == "repl op"
                    || lower == "cli subcommand"
                    || lower == "mcp tool"
                    || lower == "lsp capability"
                    || lower == "meta-command"
            }) {
                continue;
            }
            doc.ingest_row(current_section, &cells);
        }
        Ok(doc)
    }

    fn ingest_row(&mut self, section: Section, cells: &[String]) {
        match section {
            Section::OpTable => {
                // Columns: REPL op | CLI | REPL meta | MCP | LSP | Notes
                if cells.len() < 5 {
                    return;
                }
                let Some(op) = parse_code_or_skip(&cells[0]) else {
                    return;
                };
                let bindings = OpBindings {
                    cli: parse_cell(&cells[1]),
                    repl_meta: parse_meta_cell(&cells[2]),
                    mcp: parse_cell(&cells[3]),
                    lsp: parse_cell(&cells[4]),
                };
                if let CellState::Bound(ref name) = bindings.cli {
                    self.cli_subcommands.insert(name.clone());
                }
                if let CellState::Bound(ref name) = bindings.repl_meta {
                    // The primary binding is already extracted sans backticks,
                    // so insert it directly. Also parse the raw cell for
                    // backtick-delimited aliases (`:cmd` / `:alias`).
                    self.repl_meta.insert(name.clone());
                    for meta in split_meta_aliases(&cells[2]) {
                        self.repl_meta.insert(meta);
                    }
                }
                if let CellState::Bound(ref name) = bindings.mcp {
                    self.mcp_tools.insert(name.clone());
                }
                if let CellState::Bound(ref name) = bindings.lsp {
                    self.lsp_caps.insert(name.clone());
                }
                self.ops.insert(op, bindings);
            }
            Section::CliOnly => {
                // Columns: CLI subcommand | MCP tool | LSP capability | Notes
                if cells.len() < 3 {
                    return;
                }
                if let Some(name) = parse_code_or_skip(&cells[0]) {
                    self.cli_subcommands.insert(name);
                }
                if let CellState::Bound(name) = parse_cell(&cells[1]) {
                    self.mcp_tools.insert(name);
                }
                if let CellState::Bound(name) = parse_cell(&cells[2]) {
                    self.lsp_caps.insert(name);
                }
            }
            Section::McpOnly => {
                // Columns: MCP tool | Notes
                if let Some(name) = cells.first().and_then(|c| parse_code_or_skip(c)) {
                    self.mcp_tools.insert(name);
                }
            }
            Section::LspOnly => {
                if let Some(name) = cells.first().and_then(|c| parse_code_or_skip(c)) {
                    self.lsp_caps.insert(name);
                }
            }
            Section::MetaCommandRef => {
                // Columns: Meta-command | Aliases | REPL op
                for cell in cells.iter().take(2) {
                    for alias in split_meta_aliases(cell) {
                        self.repl_meta.insert(alias);
                    }
                }
            }
            Section::Other => {}
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Section {
    OpTable,
    CliOnly,
    McpOnly,
    LspOnly,
    MetaCommandRef,
    Other,
}

impl Section {
    fn classify(heading: &str) -> Self {
        let lower = heading.to_ascii_lowercase();
        if lower.starts_with("cli-only") {
            Section::CliOnly
        } else if lower.starts_with("mcp-only") {
            Section::McpOnly
        } else if lower.starts_with("lsp-only") {
            Section::LspOnly
        } else if lower.contains("meta-command reference") {
            Section::MetaCommandRef
        } else if lower.contains("operations") {
            Section::OpTable
        } else {
            Section::Other
        }
    }
}

/// Split a markdown table row into its cells, trimming whitespace and the
/// leading/trailing `|` characters.
fn split_table_row(line: &str) -> Vec<String> {
    let trimmed = line.trim();
    let inner = trimmed.trim_start_matches('|').trim_end_matches('|');
    inner.split('|').map(|c| c.trim().to_string()).collect()
}

/// Extract the first ``code`` span from a cell, or `None` if the cell is
/// `--`, italic-only, or otherwise has no code span.
fn parse_code_or_skip(cell: &str) -> Option<String> {
    let trimmed = cell.trim();
    if trimmed == "--" || trimmed.is_empty() {
        return None;
    }
    extract_first_code(trimmed)
}

fn parse_cell(cell: &str) -> CellState {
    let trimmed = cell.trim();
    if trimmed == "--" || trimmed.is_empty() {
        return CellState::NotApplicable;
    }
    if trimmed.contains("surface-specific") || trimmed.starts_with("*(") {
        return CellState::SurfaceSpecific;
    }
    if let Some(code) = extract_first_code(trimmed) {
        if is_non_binding_code(&code) {
            return CellState::SurfaceSpecific;
        }
        CellState::Bound(code)
    } else {
        CellState::NotApplicable
    }
}

/// Code spans that document the cell rather than naming a code artifact.
/// `via X` (e.g. `via Workspace classes`) means the capability is reached
/// as a message-send on `X`, not via a meta-cmd / tool. `MISSING (BT-…)`
/// flags an intentionally unbound cell with a tracking issue. Both are
/// treated as `SurfaceSpecific` for drift purposes — there is no code
/// artifact to verify here.
fn is_non_binding_code(code: &str) -> bool {
    let lower = code.trim().to_ascii_lowercase();
    lower.starts_with("via ") || lower.starts_with("missing (") || lower.starts_with("missing(")
}

/// REPL meta-cells often list aliases (e.g. ``:test`` / ``:t``) and
/// occasionally include a placeholder argument like ``:unload <class>``.
/// We collapse to the first whitespace-delimited token of the first code
/// span so parity-required checking matches the literal in the dispatcher.
fn parse_meta_cell(cell: &str) -> CellState {
    let trimmed = cell.trim();
    if trimmed == "--" || trimmed.is_empty() {
        return CellState::NotApplicable;
    }
    if trimmed.contains("surface-specific") || trimmed.starts_with("*(") {
        return CellState::SurfaceSpecific;
    }
    if let Some(first) = extract_first_code(trimmed) {
        if is_non_binding_code(&first) {
            return CellState::SurfaceSpecific;
        }
        let token = first.split_whitespace().next().unwrap_or("").to_string();
        if token.is_empty() {
            CellState::NotApplicable
        } else {
            CellState::Bound(token)
        }
    } else {
        CellState::NotApplicable
    }
}

/// Find every backtick-delimited token in `s`.
fn extract_all_code(s: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut chars = s.char_indices();
    while let Some((i, c)) = chars.next() {
        if c != '`' {
            continue;
        }
        let start = i + 1;
        let rest = &s[start..];
        if let Some(end) = rest.find('`') {
            out.push(rest[..end].to_string());
            // Advance past closing backtick.
            for _ in 0..=end {
                chars.next();
            }
        }
    }
    out
}

fn extract_first_code(s: &str) -> Option<String> {
    extract_all_code(s).into_iter().next()
}

fn split_meta_aliases(cell: &str) -> Vec<String> {
    extract_all_code(cell)
        .into_iter()
        .filter(|s| s.starts_with(':'))
        .map(|s| {
            // Strip placeholder argument (e.g. `:unload <class>`).
            s.split_whitespace().next().unwrap_or("").to_string()
        })
        .filter(|s| !s.is_empty())
        .collect()
}

/// Inventory of artifacts present in the codebase.
#[derive(Debug, Default)]
struct CodeInventory {
    /// REPL op names declared in `*_repl_ops_*.erl` describe-ops maps.
    repl_ops: BTreeSet<String>,
    /// MCP tool function names from `#[tool(...)]` attributes.
    mcp_tools: BTreeSet<String>,
    /// REPL meta-commands dispatched by `handle_repl_command`.
    repl_meta: BTreeSet<String>,
    /// LSP capabilities (e.g. `textDocument/hover`) wired into
    /// `ServerCapabilities`.
    lsp_caps: BTreeSet<String>,
}

impl CodeInventory {
    fn scan(repo_root: &Path) -> Result<Self, String> {
        let mut inv = CodeInventory::default();
        inv.scan_repl_ops(&repo_root.join(REPL_OPS_DIR))?;
        inv.scan_mcp_tools(&repo_root.join(MCP_SERVER))?;
        inv.scan_repl_meta(&repo_root.join(REPL_DISPATCH))?;
        inv.scan_lsp_caps(&repo_root.join(LSP_SERVER))?;
        // Sanity-check: if any scanner found zero items, the heuristic may
        // have broken. Fail loudly rather than producing false-positive drift
        // errors for every op/tool.
        if inv.repl_ops.is_empty() {
            return Err(
                "scanner found 0 REPL ops — the describe_ops()/base_ops() pattern \
                 may have changed. Update the Erlang scanner in beamtalk-surface-drift."
                    .into(),
            );
        }
        if inv.mcp_tools.is_empty() {
            return Err("scanner found 0 MCP tools — the #[tool(...)] pattern \
                 may have changed. Update the MCP scanner in beamtalk-surface-drift."
                .into());
        }
        if inv.repl_meta.is_empty() {
            return Err(
                "scanner found 0 REPL meta-commands — the handle_repl_command() \
                 pattern may have changed. Update the REPL scanner in beamtalk-surface-drift."
                    .into(),
            );
        }
        if inv.lsp_caps.is_empty() {
            return Err(
                "scanner found 0 LSP capabilities — the ServerCapabilities pattern \
                 may have changed. Update the LSP scanner in beamtalk-surface-drift."
                    .into(),
            );
        }
        Ok(inv)
    }

    fn scan_repl_ops(&mut self, dir: &Path) -> Result<(), String> {
        let entries =
            fs::read_dir(dir).map_err(|e| format!("failed to read {}: {e}", dir.display()))?;
        for entry in entries {
            let entry = entry.map_err(|e| format!("dir entry error: {e}"))?;
            let path = entry.path();
            let Some(name) = path.file_name().and_then(|s| s.to_str()) else {
                continue;
            };
            let is_erl = Path::new(name)
                .extension()
                .is_some_and(|ext| ext.eq_ignore_ascii_case("erl"));
            if !name.starts_with("beamtalk_repl_ops_") || !is_erl {
                continue;
            }
            let text = fs::read_to_string(&path)
                .map_err(|e| format!("failed to read {}: {e}", path.display()))?;
            extract_repl_ops_from_erlang(&text, &mut self.repl_ops);
        }
        Ok(())
    }

    fn scan_mcp_tools(&mut self, path: &Path) -> Result<(), String> {
        let text = fs::read_to_string(path)
            .map_err(|e| format!("failed to read {}: {e}", path.display()))?;
        extract_mcp_tools(&text, &mut self.mcp_tools);
        Ok(())
    }

    fn scan_repl_meta(&mut self, path: &Path) -> Result<(), String> {
        let text = fs::read_to_string(path)
            .map_err(|e| format!("failed to read {}: {e}", path.display()))?;
        extract_repl_meta(&text, &mut self.repl_meta);
        Ok(())
    }

    fn scan_lsp_caps(&mut self, path: &Path) -> Result<(), String> {
        let text = fs::read_to_string(path)
            .map_err(|e| format!("failed to read {}: {e}", path.display()))?;
        extract_lsp_caps(&text, &mut self.lsp_caps);
        Ok(())
    }
}

/// Erlang scanner — collect the *top-level* map keys of the ops map literal
/// returned by `describe_ops()` or `base_ops()`. The shape is:
///
/// ```erlang
/// describe_ops() ->
///     #{
///         <<"eval">> => #{<<"params">> => [<<"code">>]},
///         <<"complete">> => #{<<"params">> => [<<"code">>]}
///     }.
/// ```
///
/// We treat `#{` as the opener of a map literal and `}` as a closer, and
/// only collect `<<"name">>` keys when the map-literal nesting depth is
/// exactly 1 (the outer ops map). Inner keys like `<<"params">>` are at
/// depth ≥ 2 and skipped. This is a heuristic but stable across the
/// codebase (BT-2082 risk note in the issue).
fn extract_repl_ops_from_erlang(text: &str, out: &mut BTreeSet<String>) {
    let mut in_target_fn = false;
    let mut map_depth: i32 = 0;
    let mut after_arrow = false;
    for line in text.lines() {
        let trimmed = line.trim_start();
        if !in_target_fn {
            if trimmed.starts_with("describe_ops() ->") || trimmed.starts_with("base_ops() ->") {
                in_target_fn = true;
                map_depth = 0;
                after_arrow = true;
            }
            continue;
        }
        let stripped = strip_erlang_line_comment(line);

        // Update map nesting and harvest depth-1 keys in lockstep.
        let bytes = stripped.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            if i + 1 < bytes.len() && bytes[i] == b'#' && bytes[i + 1] == b'{' {
                map_depth += 1;
                i += 2;
                continue;
            }
            if bytes[i] == b'}' {
                if map_depth > 0 {
                    map_depth -= 1;
                }
                i += 1;
                continue;
            }
            // Look for `<<"name">>` only when at outer-map depth.
            if map_depth == 1
                && i + 3 < bytes.len()
                && bytes[i] == b'<'
                && bytes[i + 1] == b'<'
                && bytes[i + 2] == b'"'
            {
                let start = i + 3;
                if let Some(end_off) = stripped[start..].find("\">>") {
                    let name = &stripped[start..start + end_off];
                    if !name.is_empty() {
                        out.insert(name.to_string());
                    }
                    i = start + end_off + 3;
                    continue;
                }
            }
            i += 1;
        }

        // End the function when we see a line that ends with `.` after the
        // outer map has closed (map_depth back to 0) — a stable terminator
        // for the ops-builder functions.
        if after_arrow && map_depth == 0 && stripped.trim_end().ends_with('.') {
            in_target_fn = false;
            after_arrow = false;
        }
    }
}

fn strip_erlang_line_comment(line: &str) -> &str {
    match line.find('%') {
        Some(idx) => &line[..idx],
        None => line,
    }
}

/// MCP scanner — every `#[tool(...)]` attribute is followed within a few
/// lines by `(pub )?async fn <name>(...)`. We capture the next `async fn`
/// after each `#[tool` occurrence.
fn extract_mcp_tools(text: &str, out: &mut BTreeSet<String>) {
    let lines: Vec<&str> = text.lines().collect();
    let mut i = 0;
    while i < lines.len() {
        if lines[i].trim_start().starts_with("#[tool(") {
            // Advance until we find the function signature.
            let mut j = i + 1;
            while j < lines.len() && j - i < 30 {
                if let Some(name) = extract_async_fn_name(lines[j]) {
                    out.insert(name);
                    break;
                }
                j += 1;
            }
            i = j + 1;
        } else {
            i += 1;
        }
    }
}

fn extract_async_fn_name(line: &str) -> Option<String> {
    let trimmed = line.trim_start();
    let rest = trimmed.strip_prefix("pub ").or(Some(trimmed))?;
    let rest = rest.strip_prefix("async fn ")?;
    let end = rest
        .find(|c: char| !c.is_ascii_alphanumeric() && c != '_')
        .unwrap_or(rest.len());
    let name = &rest[..end];
    if name.is_empty() {
        None
    } else {
        Some(name.to_string())
    }
}

/// CLI scanner — match `":<word>"` literals appearing inside the
/// `handle_repl_command` function body. We delimit by the function header
/// and the first closing brace at column 0 after it.
fn extract_repl_meta(text: &str, out: &mut BTreeSet<String>) {
    let Some(start) = text.find("fn handle_repl_command(") else {
        return;
    };
    let body = &text[start..];
    // Find first opening `{` after the signature.
    let Some(open_idx) = body.find('{') else {
        return;
    };
    let body = &body[open_idx..];
    // Walk forward, balancing braces.
    let mut depth: i32 = 0;
    let mut end: usize = body.len();
    for (idx, c) in body.char_indices() {
        match c {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    end = idx + 1;
                    break;
                }
            }
            _ => {}
        }
    }
    let fn_body = &body[..end];
    for line in fn_body.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("//") {
            continue;
        }
        for cmd in extract_meta_literals(line) {
            out.insert(cmd);
        }
    }
}

/// Find `":word"` and `":word "` (with-arg) literals.
fn extract_meta_literals(line: &str) -> Vec<String> {
    let mut out = Vec::new();
    let bytes = line.as_bytes();
    let mut i = 0;
    while i + 2 < bytes.len() {
        if bytes[i] == b'"' && bytes[i + 1] == b':' {
            let start = i + 1;
            let rest = &line[start..];
            if let Some(end_off) = rest.find('"') {
                let token = &rest[..end_off];
                let core = token.split_whitespace().next().unwrap_or("");
                let valid = !core.is_empty()
                    && core
                        .chars()
                        .skip(1)
                        .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '?');
                if valid {
                    out.push(core.to_string());
                }
                i = start + end_off + 1;
                continue;
            }
        }
        i += 1;
    }
    out
}

/// LSP scanner — collect `*_provider:` field assignments inside the
/// `ServerCapabilities { ... }` literal and translate to `textDocument/<x>`
/// (or `workspace/<x>`) names matching the parity doc convention.
fn extract_lsp_caps(text: &str, out: &mut BTreeSet<String>) {
    let Some(start) = text.find("ServerCapabilities {") else {
        return;
    };
    let body = &text[start..];
    let Some(open_idx) = body.find('{') else {
        return;
    };
    let after_open = &body[open_idx..];
    let mut depth: i32 = 0;
    let mut end: usize = after_open.len();
    for (idx, c) in after_open.char_indices() {
        match c {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    end = idx + 1;
                    break;
                }
            }
            _ => {}
        }
    }
    let block = &after_open[..end];
    for line in block.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("//") {
            continue;
        }
        if let Some(name) = parse_capability_field(trimmed) {
            for canonical in capability_to_doc_names(&name) {
                out.insert(canonical);
            }
        }
    }
    // `textDocument/didOpen`/`didChange`/`didClose`/`didSave` are implicit
    // when text_document_sync is configured with `open_close: Some(true)` /
    // `change: …` / `save: …`. The doc tracks them individually, so when we
    // see a `text_document_sync` entry, mark them present.
    if block.contains("text_document_sync:") && block.contains("Some(") {
        if block.contains("open_close: Some(true)") {
            out.insert("textDocument/didOpen".into());
            out.insert("textDocument/didClose".into());
        }
        if block.contains("change:") {
            out.insert("textDocument/didChange".into());
        }
        if block.contains("save:") {
            out.insert("textDocument/didSave".into());
        }
    }
}

fn parse_capability_field(line: &str) -> Option<String> {
    let colon = line.find(':')?;
    let name = &line[..colon];
    if name.is_empty() {
        return None;
    }
    let valid = name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_');
    if !valid {
        return None;
    }
    let after = line[colon + 1..].trim_start();
    if !after.starts_with("Some(") {
        return None;
    }
    Some(name.to_string())
}

fn capability_to_doc_names(field: &str) -> Vec<String> {
    match field {
        "completion_provider" => vec!["completion".into()],
        "hover_provider" => vec!["textDocument/hover".into()],
        "signature_help_provider" => vec!["textDocument/signatureHelp".into()],
        "definition_provider" => vec!["textDocument/definition".into()],
        "references_provider" => vec!["textDocument/references".into()],
        "document_symbol_provider" => vec!["textDocument/documentSymbol".into()],
        "workspace_symbol_provider" => vec!["workspace/symbol".into()],
        "document_formatting_provider" => vec!["textDocument/formatting".into()],
        "document_range_formatting_provider" => vec!["textDocument/rangeFormatting".into()],
        "code_action_provider" => vec!["textDocument/codeAction".into()],
        // `publishDiagnostics` is server→client, never declared in the
        // capabilities literal — handled outside this map.
        _ => Vec::new(),
    }
}

/// Run the actual drift checks and append any issues to `errors`.
fn check_drift(doc: &ParityDoc, code: &CodeInventory, errors: &mut Vec<String>) {
    // 1) Every REPL op present in code must be documented.
    for op in &code.repl_ops {
        if !doc.ops.contains_key(op) {
            errors.push(format!(
                "REPL op `{op}` is registered in runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_*.erl \
                 but missing from {PARITY_DOC}. Add a row to the matching table."
            ));
        }
    }

    // 2) For each documented op, every `Bound` cell must point at a real artifact.
    //    (CLI subcommand presence is not cross-checked against the `Command`
    //    enum yet; the doc's `CLI-Only Commands` table is the source of truth
    //    for those, and any binding that maps to an op also has to be wired
    //    on the REPL/MCP/LSP side, which is what we verify below.)
    for (op, bindings) in &doc.ops {
        if let CellState::Bound(name) = &bindings.repl_meta
            && !code.repl_meta.contains(name)
        {
            errors.push(format!(
                "REPL op `{op}` is documented as bound to REPL meta-command `{name}`, \
                 but `{name}` is not dispatched in {REPL_DISPATCH}."
            ));
        }
        if let CellState::Bound(name) = &bindings.mcp
            && !code.mcp_tools.contains(name)
        {
            errors.push(format!(
                "REPL op `{op}` is documented as bound to MCP tool `{name}`, \
                 but no `#[tool(...)]` `async fn {name}` exists in {MCP_SERVER}."
            ));
        }
        if let CellState::Bound(name) = &bindings.lsp
            && !code.lsp_caps.contains(name)
        {
            errors.push(format!(
                "REPL op `{op}` is documented as bound to LSP capability `{name}`, \
                 but it is not enabled in {LSP_SERVER}."
            ));
        }
    }

    // 3) MCP tools in code must appear somewhere in the doc.
    for tool in &code.mcp_tools {
        if !doc.mcp_tools.contains(tool) {
            errors.push(format!(
                "MCP tool `{tool}` is implemented in {MCP_SERVER} but missing from {PARITY_DOC}. \
                 Add it to the matching op row, or to the `MCP-Only Tools` section if surface-specific."
            ));
        }
    }

    // 4) REPL meta-commands in code must appear in the doc.
    for cmd in &code.repl_meta {
        if !doc.repl_meta.contains(cmd) {
            errors.push(format!(
                "REPL meta-command `{cmd}` is dispatched in {REPL_DISPATCH} but missing from {PARITY_DOC}. \
                 Add it to the matching op row or to the `REPL Meta-Command Reference` table."
            ));
        }
    }

    // 5) LSP capabilities in code must appear in the doc.
    for cap in &code.lsp_caps {
        if !doc.lsp_caps.contains(cap) {
            errors.push(format!(
                "LSP capability `{cap}` is enabled in {LSP_SERVER} but missing from {PARITY_DOC}. \
                 Add it to the `LSP-Only Capabilities` section (or the matching op row)."
            ));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_cell_treats_via_and_missing_as_surface_specific() {
        assert!(matches!(
            parse_cell("`via Workspace classes`"),
            CellState::SurfaceSpecific
        ));
        assert!(matches!(
            parse_cell("`MISSING (BT-2090)`"),
            CellState::SurfaceSpecific
        ));
    }

    #[test]
    fn parse_meta_cell_treats_via_and_missing_as_surface_specific() {
        assert!(matches!(
            parse_meta_cell("`via Workspace test`"),
            CellState::SurfaceSpecific
        ));
        assert!(matches!(
            parse_meta_cell("`MISSING (BT-2090)`"),
            CellState::SurfaceSpecific
        ));
    }

    #[test]
    fn is_non_binding_code_does_not_match_legitimate_names() {
        assert!(!is_non_binding_code("missingPlugin"));
        assert!(!is_non_binding_code("missingness"));
        assert!(!is_non_binding_code("viable"));
        assert!(is_non_binding_code("via Workspace classes"));
        assert!(is_non_binding_code("MISSING (BT-2090)"));
        assert!(is_non_binding_code("missing(BT-1)"));
    }

    #[test]
    fn extracts_repl_ops_only_within_describe_ops() {
        let src = r#"
some_other_function() ->
    #{<<"not-an-op">> => true}.

describe_ops() ->
    #{
        <<"eval">> => #{<<"params">> => [<<"code">>]},
        <<"complete">> => #{<<"params">> => [<<"code">>]}
    }.
"#;
        let mut out = BTreeSet::new();
        extract_repl_ops_from_erlang(src, &mut out);
        assert!(out.contains("eval"));
        assert!(out.contains("complete"));
        // Inner map keys (`<<"params">>`, `<<"code">>`) are at depth ≥ 2 and
        // must not leak into the op set.
        assert!(!out.contains("params"));
        assert!(!out.contains("code"));
        // Functions other than `describe_ops`/`base_ops` are ignored entirely.
        assert!(!out.contains("not-an-op"));
    }

    #[test]
    fn extracts_meta_literals() {
        let line = r#"        ":exit" | ":quit" | ":q" => return CommandResult::Exit,"#;
        let mut got = extract_meta_literals(line);
        got.sort();
        assert_eq!(got, vec![":exit", ":q", ":quit"]);
    }

    #[test]
    fn meta_literals_skip_non_command_strings() {
        let line = r#"            eprintln!("{e}");"#;
        assert!(extract_meta_literals(line).is_empty());
    }

    #[test]
    fn extracts_async_fn_name() {
        assert_eq!(
            extract_async_fn_name("    async fn evaluate("),
            Some("evaluate".into())
        );
        assert_eq!(
            extract_async_fn_name("    pub async fn complete<'a>("),
            Some("complete".into())
        );
        assert!(extract_async_fn_name("fn not_async() {").is_none());
    }

    #[test]
    fn parses_capability_field() {
        assert_eq!(
            parse_capability_field("hover_provider: Some(HoverProviderCapability::Simple(true)),"),
            Some("hover_provider".to_string())
        );
        assert!(parse_capability_field("..Default::default()").is_none());
    }

    #[test]
    fn parses_meta_aliases() {
        let aliases = split_meta_aliases("`:exit` | `:quit`, `:q`");
        assert_eq!(aliases, vec![":exit", ":quit", ":q"]);
    }

    #[test]
    fn parses_unload_with_argument() {
        let aliases = split_meta_aliases("`:unload <class>`");
        assert_eq!(aliases, vec![":unload"]);
    }

    #[test]
    fn parse_cell_handles_surface_specific() {
        assert!(matches!(
            parse_cell("`surface-specific: offline compiler, no workspace`"),
            CellState::SurfaceSpecific
        ));
        assert!(matches!(parse_cell("--"), CellState::NotApplicable));
        assert!(matches!(
            parse_cell("`evaluate`"),
            CellState::Bound(ref s) if s == "evaluate"
        ));
        assert!(matches!(
            parse_cell("*(implicit: any expression)*"),
            CellState::SurfaceSpecific
        ));
    }

    fn make_doc(op: &str, mcp: &CellState) -> ParityDoc {
        let mut doc = ParityDoc::default();
        let bindings = OpBindings {
            cli: CellState::NotApplicable,
            repl_meta: CellState::NotApplicable,
            mcp: mcp.clone(),
            lsp: CellState::NotApplicable,
        };
        if let CellState::Bound(ref name) = bindings.mcp {
            doc.mcp_tools.insert(name.clone());
        }
        doc.ops.insert(op.to_string(), bindings);
        doc
    }

    #[test]
    fn drift_flags_undocumented_repl_op() {
        let doc = ParityDoc::default();
        let mut code = CodeInventory::default();
        code.repl_ops.insert("brand-new-op".into());
        let mut errors = Vec::new();
        check_drift(&doc, &code, &mut errors);
        assert!(errors.iter().any(|e| e.contains("brand-new-op")));
    }

    #[test]
    fn drift_flags_undocumented_mcp_tool() {
        let doc = ParityDoc::default();
        let mut code = CodeInventory::default();
        code.mcp_tools.insert("undocumented_tool".into());
        let mut errors = Vec::new();
        check_drift(&doc, &code, &mut errors);
        assert!(errors.iter().any(|e| e.contains("undocumented_tool")));
    }

    #[test]
    fn drift_flags_missing_mcp_binding() {
        let doc = make_doc("eval", &CellState::Bound("evaluate".into()));
        let mut code = CodeInventory::default();
        code.repl_ops.insert("eval".into());
        // No `evaluate` MCP tool wired in code → should error.
        let mut errors = Vec::new();
        check_drift(&doc, &code, &mut errors);
        assert!(errors.iter().any(|e| e.contains("evaluate")));
    }

    #[test]
    fn drift_clean_when_doc_matches_code() {
        let doc = make_doc("eval", &CellState::Bound("evaluate".into()));
        let mut code = CodeInventory::default();
        code.repl_ops.insert("eval".into());
        code.mcp_tools.insert("evaluate".into());
        let mut errors = Vec::new();
        check_drift(&doc, &code, &mut errors);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    }

    #[test]
    fn drift_accepts_surface_specific_without_binding() {
        let doc = make_doc("eval", &CellState::SurfaceSpecific);
        let mut code = CodeInventory::default();
        code.repl_ops.insert("eval".into());
        // Even without an MCP tool, `surface-specific` must not error.
        let mut errors = Vec::new();
        check_drift(&doc, &code, &mut errors);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    }

    #[test]
    fn parity_doc_loads_real_inventory() {
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        let path = Path::new(manifest_dir)
            .parent()
            .and_then(|p| p.parent())
            .map(|p| p.join("docs/development/surface-parity.md"))
            .expect("repo root");
        if !path.exists() {
            // Allow the test to be a no-op when run from a packaged crate.
            return;
        }
        let doc = ParityDoc::load(&path).expect("load parity doc");
        // Spot-check a handful of well-known entries that must always exist.
        assert!(doc.ops.contains_key("eval"), "eval op missing from doc");
        assert!(doc.mcp_tools.contains("evaluate"));
        assert!(doc.repl_meta.contains(":exit"));
        assert!(doc.lsp_caps.contains("textDocument/hover"));
    }
}
