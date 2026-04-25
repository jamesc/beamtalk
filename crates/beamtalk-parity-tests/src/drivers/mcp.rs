// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! MCP surface driver.
//!
//! Spawns the real `beamtalk-mcp` binary in the same way an MCP host would,
//! pointed at the shared workspace. Uses MCP's stdio JSON-RPC framing
//! (LSP-style: each line is a single JSON object with no `Content-Length`
//! prefix — `rmcp`'s stdio transport is line-delimited).

use std::collections::BTreeSet;
use std::process::Stdio;
use std::time::Duration;

use serde_json::{Value, json};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::process::{Child, ChildStdin, ChildStdout};

use crate::drivers::SurfaceOutput;
use crate::normalize;
use crate::pool::{SharedRepl, beamtalk_binary};

/// Long-lived MCP child process plus stdio handles.
#[derive(Debug)]
pub struct McpDriver {
    child: Child,
    stdin: ChildStdin,
    stdout: BufReader<ChildStdout>,
    next_id: i64,
}

impl McpDriver {
    /// Spawn `beamtalk-mcp --workspace-id <id>` and complete the handshake.
    pub async fn spawn(repl: &SharedRepl) -> Result<Self, String> {
        let bin =
            beamtalk_binary("beamtalk-mcp").map_err(|e| format!("locate beamtalk-mcp: {e}"))?;
        let mut child = tokio::process::Command::new(&bin)
            .args(["--workspace-id", &repl.workspace_id])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .map_err(|e| format!("spawn {}: {e}", bin.display()))?;
        let stdin = child.stdin.take().ok_or("no stdin handle on mcp child")?;
        let stdout = BufReader::new(child.stdout.take().ok_or("no stdout handle on mcp child")?);
        let mut driver = Self {
            child,
            stdin,
            stdout,
            next_id: 1,
        };
        driver.handshake().await?;
        Ok(driver)
    }

    async fn handshake(&mut self) -> Result<(), String> {
        // initialize request
        let req = self.build_request(
            "initialize",
            json!({
                "protocolVersion": "2024-11-05",
                "capabilities": {},
                "clientInfo": {"name": "beamtalk-parity", "version": "0"}
            }),
        );
        let _resp = self.rpc(req).await?;
        // initialized notification (no id, no response)
        let note = json!({
            "jsonrpc": "2.0",
            "method": "notifications/initialized",
            "params": {}
        });
        self.send(&note).await?;
        Ok(())
    }

    fn build_request(&mut self, method: &str, params: Value) -> Value {
        let id = self.next_id;
        self.next_id += 1;
        json!({
            "jsonrpc": "2.0",
            "id": id,
            "method": method,
            "params": params
        })
    }

    async fn send(&mut self, msg: &Value) -> Result<(), String> {
        let mut line = msg.to_string();
        line.push('\n');
        self.stdin
            .write_all(line.as_bytes())
            .await
            .map_err(|e| format!("write mcp stdin: {e}"))?;
        self.stdin
            .flush()
            .await
            .map_err(|e| format!("flush mcp stdin: {e}"))?;
        Ok(())
    }

    async fn rpc(&mut self, req: Value) -> Result<Value, String> {
        let want_id = req["id"].clone();
        self.send(&req).await?;
        // Read until we get a response with the matching id (skip notifications).
        loop {
            let mut buf = String::new();
            let read =
                tokio::time::timeout(Duration::from_secs(45), self.stdout.read_line(&mut buf))
                    .await
                    .map_err(|_| "mcp stdout read timed out".to_string())?
                    .map_err(|e| format!("read mcp stdout: {e}"))?;
            if read == 0 {
                return Err("mcp stdout closed unexpectedly".to_string());
            }
            let trimmed = buf.trim();
            if trimmed.is_empty() {
                continue;
            }
            let v: Value = serde_json::from_str(trimmed)
                .map_err(|e| format!("parse mcp message `{trimmed}`: {e}"))?;
            if v.get("id") == Some(&want_id) {
                if let Some(err) = v.get("error") {
                    return Err(format!("mcp error: {err}"));
                }
                return Ok(v.get("result").cloned().unwrap_or(Value::Null));
            }
            // Otherwise it's a notification or response for a different id; loop.
        }
    }

    /// Call an MCP tool by name and return the joined text of the result.
    pub async fn call_tool(&mut self, name: &str, args: Value) -> Result<Value, String> {
        let req = self.build_request(
            "tools/call",
            json!({
                "name": name,
                "arguments": args
            }),
        );
        self.rpc(req).await
    }

    /// Drive the `evaluate` MCP tool.
    pub async fn evaluate(&mut self, code: &str) -> Result<SurfaceOutput, String> {
        let result = self.call_tool("evaluate", json!({"code": code})).await?;
        let text = extract_content_text(&result);
        let value = strip_value_label(&text);
        Ok(SurfaceOutput {
            value: Some(normalize::value(&value)),
            raw: text,
            ..SurfaceOutput::default()
        })
    }

    /// Drive the `load_project` MCP tool.
    pub async fn load_project(&mut self, path: &str) -> Result<SurfaceOutput, String> {
        // `force=true` so re-running across cases (after REPL pre-loaded the
        // same files) still produces a fresh class list rather than a
        // "0 of 2 unchanged" diff.
        let result = self
            .call_tool(
                "load_project",
                json!({"path": path, "include_tests": false, "force": true}),
            )
            .await?;
        let text = extract_content_text(&result);
        let mut classes = BTreeSet::new();
        // MCP renders the success path as a single line: "Loaded classes: A, B, C"
        for line in text.lines() {
            let trimmed = line.trim();
            if let Some(rest) = trimmed.strip_prefix("Loaded classes:") {
                for tok in rest.split(',') {
                    let name = tok.trim();
                    if !name.is_empty() {
                        classes.insert(name.to_string());
                    }
                }
            }
        }
        // Also harvest any "classes": [...] field in the JSON result for
        // forward-compat with structured envelopes.
        if let Some(list) = result.pointer("/classes").and_then(Value::as_array) {
            for c in list.iter().filter_map(Value::as_str) {
                classes.insert(c.to_string());
            }
        }
        let diagnostic_count = result
            .pointer("/errors")
            .and_then(Value::as_array)
            .map(Vec::len)
            .unwrap_or(count_error_lines(&text));
        Ok(SurfaceOutput {
            classes: Some(classes),
            diagnostic_count: Some(diagnostic_count),
            raw: text,
            ..SurfaceOutput::default()
        })
    }

    /// Like [`load_project`] but with `include_tests=true`.
    pub async fn load_project_with_tests(&mut self, path: &str) -> Result<SurfaceOutput, String> {
        let result = self
            .call_tool(
                "load_project",
                json!({"path": path, "include_tests": true, "force": true}),
            )
            .await?;
        let text = extract_content_text(&result);
        let mut classes = BTreeSet::new();
        for line in text.lines() {
            let trimmed = line.trim();
            if let Some(rest) = trimmed.strip_prefix("Loaded classes:") {
                for tok in rest.split(',') {
                    let name = tok.trim();
                    if !name.is_empty() {
                        classes.insert(name.to_string());
                    }
                }
            }
        }
        Ok(SurfaceOutput {
            classes: Some(classes),
            raw: text,
            ..SurfaceOutput::default()
        })
    }

    /// Drive the `test` MCP tool against a class name.
    ///
    /// MCP returns either a JSON test-result envelope (success path) or a
    /// `TEST FAILURES:\n<json>` blob (`is_error=true` path). Both carry a
    /// `failed` integer in the embedded JSON; we trust that field over any
    /// loose text scraping so a JSON key like `"failed": 1` doesn't get
    /// misread as the literal word "fail".
    pub async fn test_class(&mut self, class: &str) -> Result<SurfaceOutput, String> {
        let result = self.call_tool("test", json!({"class": class})).await?;
        let text = extract_content_text(&result);
        let summary = if test_summary_failed(&text) {
            "fail".to_string()
        } else {
            "pass".to_string()
        };
        Ok(SurfaceOutput {
            value: Some(summary),
            raw: text,
            ..SurfaceOutput::default()
        })
    }

    /// Drive the `lint` MCP tool against a path.
    pub async fn lint(&mut self, path: &str) -> Result<SurfaceOutput, String> {
        let result = self.call_tool("lint", json!({"path": path})).await?;
        let text = extract_content_text(&result);
        // The `lint` tool returns a JSON envelope with `errors`, `warnings`,
        // and `total`; prefer the structured number when present.
        let count = serde_json::from_str::<serde_json::Value>(&text)
            .ok()
            .and_then(|v| v.get("total").and_then(serde_json::Value::as_u64))
            .and_then(|n| usize::try_from(n).ok())
            .unwrap_or_else(|| count_diagnostic_lines(&text));
        Ok(SurfaceOutput {
            diagnostic_count: Some(count),
            raw: text,
            ..SurfaceOutput::default()
        })
    }

    /// Drive the `load_file` MCP tool to flush a file through the full
    /// parse/load pipeline. Used by the diagnostic parity case because plain
    /// `lint` misses parse errors.
    pub async fn diagnose_file(&mut self, path: &str) -> Result<SurfaceOutput, String> {
        let result = self.call_tool("load_file", json!({"path": path})).await?;
        let text = extract_content_text(&result);
        let count = if text.to_lowercase().contains("error")
            || text.to_lowercase().contains("failed")
            || text.contains('×')
        {
            text.lines()
                .filter(|l| {
                    let t = l.trim();
                    t.starts_with("Error")
                        || t.starts_with("error:")
                        || t.starts_with("× ")
                        || t.contains(": error:")
                })
                .count()
                .max(1)
        } else {
            0
        };
        Ok(SurfaceOutput {
            diagnostic_count: Some(count),
            raw: text,
            ..SurfaceOutput::default()
        })
    }

    /// Send `shutdown` and reap the child. Best-effort.
    pub async fn close(mut self) {
        // Sending a graceful exit notification is the polite thing to do; we
        // don't strictly need a response.
        let note = json!({"jsonrpc": "2.0", "method": "notifications/cancelled"});
        let _ = self.send(&note).await;
        // Drop stdin first to signal EOF.
        drop(self.stdin);
        let _ = tokio::time::timeout(Duration::from_secs(5), self.child.wait()).await;
        let _ = self.child.start_kill();
    }
}

/// Pull the concatenated `text` content out of an MCP `tools/call` result.
fn extract_content_text(result: &Value) -> String {
    let mut out = String::new();
    if let Some(arr) = result.get("content").and_then(Value::as_array) {
        for item in arr {
            if let Some(t) = item.get("text").and_then(Value::as_str) {
                if !out.is_empty() {
                    out.push('\n');
                }
                out.push_str(t);
            }
        }
    }
    out
}

/// MCP's `evaluate` tool prefixes its response with `Value: …` on one line and
/// optionally `Output:` on subsequent lines. The parity harness only cares
/// about the value.
fn strip_value_label(text: &str) -> String {
    for line in text.lines() {
        if let Some(rest) = line.strip_prefix("Value: ") {
            return rest.to_string();
        }
    }
    text.to_string()
}

fn count_diagnostic_lines(text: &str) -> usize {
    text.lines()
        .filter(|l| {
            let t = l.trim_start();
            t.starts_with("warning")
                || t.starts_with("error")
                || t.starts_with("note")
                || t.contains(": warning:")
                || t.contains(": error:")
        })
        .count()
}

fn count_error_lines(text: &str) -> usize {
    text.lines()
        .filter(|l| l.to_lowercase().contains("error"))
        .count()
}

/// True when the MCP `test` tool's text content reports at least one failure.
///
/// Prefers the `failed` integer in the embedded JSON envelope (success or
/// `TEST FAILURES:` paths both carry it) and falls back to a token scan for
/// older response shapes. This avoids the JSON-key-name false positive
/// where a literal `"failed":` or `"error":` substring made the test look
/// like it failed when the count was actually zero.
fn test_summary_failed(text: &str) -> bool {
    // Prefix variant: MCP returns `TEST FAILURES:\n{...}` for runs with
    // failures. Treat any well-formed JSON body as authoritative.
    let json_text = text
        .find('{')
        .map(|idx| &text[idx..])
        .unwrap_or(text)
        .trim();
    if let Ok(v) = serde_json::from_str::<Value>(json_text) {
        if let Some(n) = v.get("failed").and_then(Value::as_u64) {
            return n > 0;
        }
    }
    // Fallback: token scan for legacy text formats. `has_nonzero_failures`
    // tolerates both `<n> failed` and `failed: <n>` orderings.
    has_nonzero_failures(text)
}

fn has_nonzero_failures(text: &str) -> bool {
    // Accept patterns like "1 failed" / "2 errors" / `"failed": 1` /
    // `failed: 3` but not "0 failed". Walks token windows in both
    // orderings so JSON-style `key: value` and prose `count word` both
    // round-trip without false positives.
    let lower = text.to_lowercase();
    let tokens: Vec<&str> = lower.split_whitespace().collect();
    tokens.windows(2).any(|w| {
        // `<n> fail*` / `<n> error*`
        let count_first = w[0].trim_end_matches(',').parse::<u64>().unwrap_or(0);
        if count_first > 0 && (w[1].starts_with("fail") || w[1].starts_with("error")) {
            return true;
        }
        // `failed[":,]?` `<n>` — handles JSON `"failed": 1` after stripping
        // surrounding quotes, comma and trailing punctuation. The key half
        // MUST contain a `:` so prose like "failed, 1" doesn't match here
        // (it's already covered by the count-first branch above).
        if w[0].contains(':') {
            let key = w[0].trim_matches(|c: char| c == '"' || c == ',' || c == ':');
            let val = w[1].trim_end_matches(',').trim_end_matches('}');
            let count_second = val.parse::<u64>().unwrap_or(0);
            if count_second > 0 && (key.starts_with("failed") || key.starts_with("errors")) {
                return true;
            }
        }
        false
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extracts_text_content() {
        let v = json!({"content": [{"type":"text","text":"hello"},{"type":"text","text":"world"}]});
        assert_eq!(extract_content_text(&v), "hello\nworld");
    }

    #[test]
    fn strip_value_label_returns_value() {
        assert_eq!(strip_value_label("Value: 7\nOutput: ok"), "7");
        assert_eq!(strip_value_label("no label"), "no label");
    }

    #[test]
    fn nonzero_failure_detection() {
        assert!(has_nonzero_failures("1 failed"));
        assert!(has_nonzero_failures("3 errors detected"));
        assert!(!has_nonzero_failures("0 failed, 0 errors"));
        // JSON-style ordering — required by the MCP `test` envelope.
        assert!(has_nonzero_failures("\"failed\": 1"));
        assert!(has_nonzero_failures("\"errors\": 2,"));
        assert!(!has_nonzero_failures("\"failed\": 0"));
    }

    #[test]
    fn test_summary_failed_uses_embedded_json() {
        // BT-2080: MCP `test` envelope (failure path).
        let envelope = "TEST FAILURES:\n{\n  \"failed\": 1,\n  \"passed\": 0,\n  \"total\": 1\n}";
        assert!(test_summary_failed(envelope));

        // Success path with the same key but zero count.
        let success = "{\n  \"failed\": 0,\n  \"passed\": 3,\n  \"total\": 3\n}";
        assert!(!test_summary_failed(success));

        // Plain prose text falls through to the token scan.
        assert!(test_summary_failed("Tests: 0 passed, 2 failed"));
        assert!(!test_summary_failed("Tests: 5 passed, 0 failed"));
    }
}
