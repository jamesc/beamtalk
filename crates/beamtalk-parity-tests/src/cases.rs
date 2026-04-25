// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Parity case file format and parser.
//!
//! Each `*.parity.bt` case is a small declarative file describing:
//!
//! * one or more inputs to drive through one or more surfaces, and
//! * one or more expected outcomes (either a shared `@expect` block, a per-
//!   surface override, or a structural expectation).
//!
//! # Example
//!
//! ```text
//! // @input
//! 3 + 4
//! // @surfaces repl, mcp
//! // @expect 7
//! ```
//!
//! The parser is intentionally line-oriented and forgiving: comments outside
//! of `@`-directives are ignored, and unknown directives produce a parse error
//! so typos surface fast.

use std::collections::BTreeSet;
use std::fmt;
use std::path::{Path, PathBuf};

/// All surfaces a parity case can target.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Surface {
    /// Direct WebSocket REPL client (the same protocol `beamtalk-mcp` and
    /// `beamtalk-cli` use internally).
    Repl,
    /// `beamtalk-mcp` binary driven over stdio with MCP JSON-RPC.
    Mcp,
    /// `beamtalk` CLI binary driven via `std::process::Command`.
    Cli,
    /// `beamtalk-lsp` binary driven over stdio with LSP JSON-RPC.
    Lsp,
}

impl Surface {
    /// Parse a surface name from a `@surfaces` declaration.
    pub fn parse(s: &str) -> Result<Self, String> {
        match s.trim().to_ascii_lowercase().as_str() {
            "repl" => Ok(Self::Repl),
            "mcp" => Ok(Self::Mcp),
            "cli" => Ok(Self::Cli),
            "lsp" => Ok(Self::Lsp),
            other => Err(format!("Unknown surface '{other}'")),
        }
    }

    /// Short stable label used in assertion failure messages.
    pub fn label(self) -> &'static str {
        match self {
            Self::Repl => "repl",
            Self::Mcp => "mcp",
            Self::Cli => "cli",
            Self::Lsp => "lsp",
        }
    }
}

impl fmt::Display for Surface {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.label())
    }
}

/// Operation a step exercises across surfaces.
///
/// Each op maps to a specific driver method on each surface; mismatches —
/// e.g. asking for `Op::Eval` on the LSP — produce a clear failure.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    /// Evaluate the input as a Beamtalk expression.
    Eval,
    /// Load the project at the given path (input is a directory).
    Load,
    /// Lint the project at the given path.
    Lint,
    /// Run `BUnit` tests (input is the class name on REPL/MCP, or a directory
    /// for the CLI).
    Test,
    /// Open the file at the given path and collect diagnostics.
    Diagnose,
}

impl Op {
    pub fn parse(s: &str) -> Result<Self, String> {
        match s.trim() {
            "eval" => Ok(Self::Eval),
            "load" => Ok(Self::Load),
            "lint" => Ok(Self::Lint),
            "test" => Ok(Self::Test),
            "diagnose" => Ok(Self::Diagnose),
            other => Err(format!("Unknown op `{other}`")),
        }
    }
}

/// What a parity step expects from each surface.
///
/// `Value` matches the normalized text returned by every surface against a
/// literal expected string. `Classes` checks that a particular set of class
/// names was observed (used by `load_project`-style cases). `DiagnosticCount`
/// matches a single integer per surface.
#[derive(Debug, Clone)]
pub enum Expectation {
    /// All surfaces must produce the given normalized value.
    Value(String),
    /// All surfaces must observe (at least) this set of class names.
    Classes(BTreeSet<String>),
    /// All surfaces must report this number of diagnostics.
    DiagnosticCount(usize),
}

/// A single parity step parsed from a `.parity.bt` file.
#[derive(Debug, Clone)]
pub struct Step {
    /// 1-based line number where `@input` was declared (used in error
    /// messages so failures point at the right place).
    pub line: usize,
    /// The input string as written between `@input` and `@surfaces`.
    pub input: String,
    /// Set of surfaces this step targets.
    pub surfaces: BTreeSet<Surface>,
    /// What every surface should produce.
    pub expect: Expectation,
    /// Operation to drive on every surface. Defaults to `Eval`.
    pub op: Op,
}

/// A parsed `.parity.bt` case file.
#[derive(Debug, Clone)]
pub struct Case {
    /// Source path on disk; used to label failures.
    pub path: PathBuf,
    /// Steps in declaration order.
    pub steps: Vec<Step>,
}

impl Case {
    /// Load and parse a case file from disk.
    pub fn from_path(path: &Path) -> Result<Self, String> {
        let text =
            std::fs::read_to_string(path).map_err(|e| format!("read {}: {e}", path.display()))?;
        let steps = parse(&text).map_err(|e| format!("{}: {e}", path.display()))?;
        Ok(Self {
            path: path.to_path_buf(),
            steps,
        })
    }
}

/// Discover every `*.parity.bt` case under a directory (non-recursively).
///
/// Returns an alphabetically sorted list so failure ordering is stable.
pub fn discover(dir: &Path) -> Result<Vec<PathBuf>, String> {
    let mut out = Vec::new();
    let entries = std::fs::read_dir(dir).map_err(|e| format!("read_dir {}: {e}", dir.display()))?;
    for entry in entries {
        let entry = entry.map_err(|e| format!("read_dir entry: {e}"))?;
        let p = entry.path();
        if p.is_file() && p.to_string_lossy().ends_with(".parity.bt") {
            out.push(p);
        }
    }
    out.sort();
    Ok(out)
}

/// Parse a case-file body into its constituent steps.
///
/// The grammar is informal:
///
/// ```text
/// step       := '// @input' '\n' input '// @surfaces' surfaces '// @expect…'
/// surfaces   := comma-separated identifiers
/// expect     := one of: value | classes | diagnostic-count
/// ```
fn parse(text: &str) -> Result<Vec<Step>, String> {
    let mut steps = Vec::new();
    let lines: Vec<&str> = text.lines().collect();
    let mut i = 0;
    while i < lines.len() {
        let line = lines[i].trim();
        if line.is_empty() || (line.starts_with("//") && !line.starts_with("// @")) {
            i += 1;
            continue;
        }
        if line == "// @input" {
            let input_start_line = i + 1;
            i += 1;
            // Collect input lines until the next directive or EOF.
            let mut input_lines = Vec::new();
            while i < lines.len() {
                let l = lines[i].trim_start();
                if l.starts_with("// @") {
                    break;
                }
                input_lines.push(lines[i]);
                i += 1;
            }
            // Parse @surfaces
            if i >= lines.len() {
                return Err(format!(
                    "line {}: @input without @surfaces directive",
                    input_start_line
                ));
            }
            let surfaces_line = lines[i].trim();
            let surfaces_str = surfaces_line.strip_prefix("// @surfaces ").ok_or_else(|| {
                format!(
                    "line {}: expected `// @surfaces ...`, got `{}`",
                    i + 1,
                    surfaces_line
                )
            })?;
            let surfaces =
                parse_surfaces(surfaces_str).map_err(|e| format!("line {}: {}", i + 1, e))?;
            i += 1;

            // Optional @op directive
            let mut op = Op::Eval;
            if i < lines.len() {
                let maybe_op = lines[i].trim();
                if let Some(rest) = maybe_op.strip_prefix("// @op ") {
                    op = Op::parse(rest).map_err(|e| format!("line {}: {}", i + 1, e))?;
                    i += 1;
                }
            }

            // Parse expectation directive
            if i >= lines.len() {
                return Err(format!(
                    "line {}: @surfaces without @expect directive",
                    input_start_line
                ));
            }
            let expect_line = lines[i].trim();
            let expect = parse_expect(expect_line).map_err(|e| format!("line {}: {}", i + 1, e))?;
            i += 1;

            // Trim trailing blank lines from input
            while input_lines.last().is_some_and(|l| l.trim().is_empty()) {
                input_lines.pop();
            }
            steps.push(Step {
                line: input_start_line,
                input: input_lines.join("\n"),
                surfaces,
                expect,
                op,
            });
            continue;
        }
        return Err(format!("line {}: unexpected directive `{}`", i + 1, line));
    }
    if steps.is_empty() {
        return Err("no @input directives found".to_string());
    }
    Ok(steps)
}

fn parse_surfaces(s: &str) -> Result<BTreeSet<Surface>, String> {
    let mut out = BTreeSet::new();
    for tok in s.split(',') {
        let tok = tok.trim();
        if tok.is_empty() {
            continue;
        }
        out.insert(Surface::parse(tok)?);
    }
    if out.is_empty() {
        return Err("@surfaces requires at least one surface".to_string());
    }
    Ok(out)
}

fn parse_expect(line: &str) -> Result<Expectation, String> {
    if let Some(rest) = line.strip_prefix("// @expect-classes ") {
        let mut classes = BTreeSet::new();
        for tok in rest.split(',') {
            let t = tok.trim();
            if !t.is_empty() {
                classes.insert(t.to_string());
            }
        }
        return Ok(Expectation::Classes(classes));
    }
    if let Some(rest) = line.strip_prefix("// @expect-diagnostics ") {
        let n: usize = rest
            .trim()
            .parse()
            .map_err(|e| format!("invalid diagnostic count `{}`: {e}", rest.trim()))?;
        return Ok(Expectation::DiagnosticCount(n));
    }
    if let Some(rest) = line.strip_prefix("// @expect ") {
        return Ok(Expectation::Value(rest.trim().to_string()));
    }
    Err(format!("expected `// @expect…` directive, got `{}`", line))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple_value_case() {
        let src = "// @input\n3 + 4\n// @surfaces repl, mcp\n// @expect 7\n";
        let steps = parse(src).unwrap();
        assert_eq!(steps.len(), 1);
        assert_eq!(steps[0].input, "3 + 4");
        assert!(steps[0].surfaces.contains(&Surface::Repl));
        assert!(steps[0].surfaces.contains(&Surface::Mcp));
        match &steps[0].expect {
            Expectation::Value(v) => assert_eq!(v, "7"),
            _ => panic!("wrong expectation"),
        }
    }

    #[test]
    fn parse_classes_expectation() {
        let src =
            "// @input\nload\n// @surfaces repl, mcp, cli\n// @expect-classes Counter, Bank\n";
        let steps = parse(src).unwrap();
        match &steps[0].expect {
            Expectation::Classes(cs) => {
                assert!(cs.contains("Counter"));
                assert!(cs.contains("Bank"));
            }
            _ => panic!("wrong expectation"),
        }
    }

    #[test]
    fn parse_diagnostic_expectation() {
        let src = "// @input\nbad\n// @surfaces cli, lsp\n// @expect-diagnostics 1\n";
        let steps = parse(src).unwrap();
        match &steps[0].expect {
            Expectation::DiagnosticCount(n) => assert_eq!(*n, 1),
            _ => panic!("wrong expectation"),
        }
    }

    #[test]
    fn surface_parse_unknown() {
        assert!(Surface::parse("xyz").is_err());
    }

    #[test]
    fn parse_rejects_input_without_surfaces() {
        let src = "// @input\nfoo\n";
        assert!(parse(src).is_err());
    }
}
