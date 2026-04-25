// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! LSP surface driver.
//!
//! Spawns `beamtalk-lsp` over stdio, performs the LSP `initialize` /
//! `initialized` handshake, opens a single file via `textDocument/didOpen`,
//! and waits for the matching `textDocument/publishDiagnostics` notification.
//!
//! LSP uses Content-Length framing (unlike MCP, which is line-delimited), so
//! we hand-roll a small parser instead of pulling in an LSP client crate.

use std::path::Path;
use std::process::Stdio;
use std::time::Duration;

use serde_json::{Value, json};
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::process::{Child, ChildStdin, ChildStdout};

use crate::drivers::SurfaceOutput;
use crate::pool::beamtalk_binary;

/// Spawned LSP child process driver.
#[derive(Debug)]
pub struct LspDriver {
    child: Child,
    stdin: ChildStdin,
    stdout: ChildStdout,
    next_id: i64,
}

impl LspDriver {
    /// Spawn `beamtalk-lsp` and complete the LSP handshake.
    pub async fn spawn(workspace_root: &Path) -> Result<Self, String> {
        let bin =
            beamtalk_binary("beamtalk-lsp").map_err(|e| format!("locate beamtalk-lsp: {e}"))?;
        let mut child = tokio::process::Command::new(&bin)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .map_err(|e| format!("spawn {}: {e}", bin.display()))?;
        let stdin = child.stdin.take().ok_or("no stdin handle on lsp child")?;
        let stdout = child.stdout.take().ok_or("no stdout handle on lsp child")?;
        let mut driver = Self {
            child,
            stdin,
            stdout,
            next_id: 1,
        };
        driver.handshake(workspace_root).await?;
        Ok(driver)
    }

    async fn handshake(&mut self, workspace_root: &Path) -> Result<(), String> {
        let root_uri = path_to_uri(workspace_root);
        let init_id = self.next_id;
        self.next_id += 1;
        let init = json!({
            "jsonrpc": "2.0",
            "id": init_id,
            "method": "initialize",
            "params": {
                "processId": std::process::id(),
                "rootUri": root_uri,
                "capabilities": {},
                "workspaceFolders": [{
                    "uri": root_uri,
                    "name": "parity"
                }]
            }
        });
        self.send(&init).await?;
        // Read messages until we see the response with our id.
        loop {
            let v = self.read_message(Duration::from_secs(20)).await?;
            if v.get("id") == Some(&Value::from(init_id)) {
                if let Some(err) = v.get("error") {
                    return Err(format!("lsp initialize error: {err}"));
                }
                break;
            }
            // Skip any notifications (e.g. window/logMessage).
        }
        let initialized = json!({
            "jsonrpc": "2.0",
            "method": "initialized",
            "params": {}
        });
        self.send(&initialized).await?;
        Ok(())
    }

    /// Open a file and capture the first `publishDiagnostics` for it.
    pub async fn diagnose(&mut self, path: &Path) -> Result<SurfaceOutput, String> {
        let text =
            std::fs::read_to_string(path).map_err(|e| format!("read {}: {e}", path.display()))?;
        let uri = path_to_uri(path);
        let did_open = json!({
            "jsonrpc": "2.0",
            "method": "textDocument/didOpen",
            "params": {
                "textDocument": {
                    "uri": uri,
                    "languageId": "beamtalk",
                    "version": 1,
                    "text": text
                }
            }
        });
        self.send(&did_open).await?;
        // Look for publishDiagnostics for this URI.
        let deadline = tokio::time::Instant::now() + Duration::from_secs(20);
        loop {
            if tokio::time::Instant::now() >= deadline {
                return Err("timed out waiting for publishDiagnostics".to_string());
            }
            let remaining = deadline - tokio::time::Instant::now();
            let v = self.read_message(remaining).await?;
            if v.get("method").and_then(Value::as_str) == Some("textDocument/publishDiagnostics")
                && v.pointer("/params/uri").and_then(Value::as_str) == Some(uri.as_str())
            {
                let diags = v
                    .pointer("/params/diagnostics")
                    .and_then(Value::as_array)
                    .cloned()
                    .unwrap_or_default();
                let raw = serde_json::to_string(&diags).unwrap_or_default();
                return Ok(SurfaceOutput {
                    diagnostic_count: Some(diags.len()),
                    raw,
                    ..SurfaceOutput::default()
                });
            }
        }
    }

    /// Send `shutdown` + `exit` and reap the child.
    pub async fn close(mut self) {
        let shutdown_id = self.next_id;
        self.next_id += 1;
        let shutdown = json!({"jsonrpc": "2.0", "id": shutdown_id, "method": "shutdown"});
        let _ = self.send(&shutdown).await;
        let _ = self.read_message(Duration::from_secs(2)).await;
        let exit = json!({"jsonrpc": "2.0", "method": "exit"});
        let _ = self.send(&exit).await;
        drop(self.stdin);
        let _ = tokio::time::timeout(Duration::from_secs(5), self.child.wait()).await;
        let _ = self.child.start_kill();
    }

    async fn send(&mut self, msg: &Value) -> Result<(), String> {
        let body = msg.to_string();
        let header = format!("Content-Length: {}\r\n\r\n", body.len());
        self.stdin
            .write_all(header.as_bytes())
            .await
            .map_err(|e| format!("write lsp header: {e}"))?;
        self.stdin
            .write_all(body.as_bytes())
            .await
            .map_err(|e| format!("write lsp body: {e}"))?;
        self.stdin
            .flush()
            .await
            .map_err(|e| format!("flush lsp stdin: {e}"))?;
        Ok(())
    }

    async fn read_message(&mut self, timeout: Duration) -> Result<Value, String> {
        let body = tokio::time::timeout(timeout, read_lsp_frame(&mut self.stdout))
            .await
            .map_err(|_| "lsp read timed out".to_string())??;
        serde_json::from_slice(&body).map_err(|e| format!("parse lsp body: {e}"))
    }
}

/// Read a single Content-Length-framed LSP message body from a stream.
async fn read_lsp_frame(stream: &mut ChildStdout) -> Result<Vec<u8>, String> {
    let mut header = Vec::new();
    let mut byte = [0u8; 1];
    // Read header bytes until "\r\n\r\n".
    loop {
        let n = stream
            .read(&mut byte)
            .await
            .map_err(|e| format!("read lsp header byte: {e}"))?;
        if n == 0 {
            return Err("lsp stdout closed".to_string());
        }
        header.push(byte[0]);
        if header.ends_with(b"\r\n\r\n") {
            break;
        }
        if header.len() > 8192 {
            return Err("lsp header too long".to_string());
        }
    }
    let header_str = std::str::from_utf8(&header).map_err(|e| format!("lsp header utf8: {e}"))?;
    let mut content_length = 0usize;
    for line in header_str.split("\r\n") {
        if let Some(rest) = line.strip_prefix("Content-Length: ") {
            content_length = rest
                .trim()
                .parse()
                .map_err(|e| format!("parse Content-Length `{rest}`: {e}"))?;
        }
    }
    if content_length == 0 {
        return Err(format!("missing Content-Length in `{header_str}`"));
    }
    let mut body = vec![0u8; content_length];
    stream
        .read_exact(&mut body)
        .await
        .map_err(|e| format!("read lsp body ({content_length} bytes): {e}"))?;
    Ok(body)
}

/// Render a path as a `file://` URI. Good enough for LSP rootUri / textDocument.uri
/// — only used for absolute paths from `tempfile::TempDir`.
fn path_to_uri(p: &Path) -> String {
    let abs = p.canonicalize().unwrap_or_else(|_| p.to_path_buf());
    let s = abs.to_string_lossy().replace('\\', "/");
    if s.starts_with('/') {
        format!("file://{s}")
    } else {
        // Windows drive paths (`C:/foo`).
        format!("file:///{s}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn path_to_uri_handles_unix_absolute() {
        let uri = path_to_uri(Path::new("/tmp/x.bt"));
        assert!(uri.starts_with("file:///") || uri.starts_with("file:////"));
    }
}
