// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Minimal WebSocket client for the Beamtalk LSP server to talk to a running
//! workspace REPL (ADR 0082 Phase 3, BT-2289).
//!
//! **DDD Context:** Language Service ↔ REPL (Workspace) bridge
//!
//! Mirrors the shape of `beamtalk-mcp`'s `ReplClient` but trimmed to the LSP
//! surface: `evaluate` for `workspace/executeCommand` dispatch, plus a
//! background listener for `flush_completed` push frames so the LSP can emit
//! `workspace/applyEdit` per touched file.
//!
//! The client is single-connection and best-effort: workspace discovery
//! consults `~/.beamtalk/workspaces/<id>/{port,cookie}` via the
//! `beamtalk-workspace` helper; on connect failure the client surfaces an
//! error and the LSP layer reports it back to the IDE as a command-failure
//! response. There is no auto-reconnect today — the LSP can be restarted by
//! the editor.

use std::path::Path;
use std::sync::Arc;
use std::time::Duration;

use futures_util::{SinkExt, StreamExt};
use serde::Deserialize;
use serde::de::DeserializeOwned;
use tokio::net::TcpStream;
use tokio::sync::{Mutex, mpsc, oneshot};
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::{MaybeTlsStream, WebSocketStream, connect_async};
use tracing::{debug, info, warn};

use beamtalk_core::language_service::{
    NavQuery, NavQueryResponse, NavSite, NavSymbolClass, NavSymbolsResponse,
};
use beamtalk_repl_protocol::{ReplResponse, RequestBuilder, handshake};

/// How long to wait for individual WebSocket reads / writes during the auth
/// handshake and per `evaluate` call. Generous enough that a slow local
/// workspace startup doesn't time out, tight enough that LSP commands return
/// in a reasonable time when the workspace has gone away.
const IO_TIMEOUT: Duration = Duration::from_secs(30);

/// How long to wait for the initial TCP+WebSocket upgrade during `connect`.
const CONNECT_TIMEOUT: Duration = Duration::from_secs(5);

type WsStream = WebSocketStream<MaybeTlsStream<TcpStream>>;

/// Errors surfaced by [`RuntimeClient`] / discovery.
#[derive(Debug, thiserror::Error)]
pub enum RuntimeError {
    /// Failed to locate a running workspace for this project — no port file
    /// or cookie file, or the `workspace_id` directory does not exist. Resolved
    /// by the user running `beamtalk repl` / `beamtalk run` against the
    /// project root.
    #[error("no running workspace found for project at {project_path}: {reason}")]
    WorkspaceNotFound {
        /// Project path the LSP attempted to attach to.
        project_path: String,
        /// Underlying reason (missing port file, etc.).
        reason: String,
    },

    /// Failed to open the WebSocket / authenticate.
    #[error("failed to connect to workspace at port {port}: {reason}")]
    Connect {
        /// Port the workspace was advertising.
        port: u16,
        /// Underlying connect error.
        reason: String,
    },

    /// Protocol / I/O error after the connection was established.
    #[error("runtime protocol error: {0}")]
    Protocol(String),
}

/// A flush-completion event surfaced to the LSP server so it can emit
/// `workspace/applyEdit` per touched file.
#[derive(Debug, Clone)]
pub struct FlushEvent {
    /// One entry per file touched by the flush — written (patch, `new-class`,
    /// `remove-method`) *or*, since ADR 0113 Phase 2 (BT-3207), deleted
    /// (`remove-class`, Tier 2 destructive flush).
    pub files: Vec<FlushedFile>,
}

/// One file touched by a flush, paired with its operation kind when the
/// runtime reported one (ADR 0113 LSP follow-up, BT-3212).
#[derive(Debug, Clone)]
pub struct FlushedFile {
    /// Absolute or workspace-relative path as `ChangeEntry.sourceFile`
    /// carried it — resolved against the LSP's workspace roots by
    /// `resolve_flushed_path` (`crates/beamtalk-lsp/src/server.rs`).
    pub path: String,
    /// The per-file operation kind from the wire's `fileKinds` companion
    /// list (`beamtalk_workspace_changelog:entry_kind/1`'s own enum value,
    /// bucketed into the shapes the LSP acts on), or `None` when the
    /// producer predates BT-3212 and sent no `fileKinds` entry for this
    /// path — the older, filesystem-existence-based classification in
    /// `resolve_flushed_path` remains the fallback for that case (BT-3209
    /// backward-compat tolerance).
    pub kind: Option<FlushFileKind>,
    /// ADR 0114 LSP follow-up (BT-3275): the pre-rename path, present only
    /// for the one `RenameClass`-kind file that IS the moved declaration
    /// (`beamtalk_workspace_flush:file_kind_map/1`'s `op = move` case,
    /// forwarded on the wire as `oldFile`). `None` for every other file,
    /// including a `RenameClass`-kind file that is an ordinary same-batch
    /// reference rewrite in a file that did not itself move — `old_path` is
    /// the only signal that distinguishes the two, since both share the
    /// same `kind`.
    pub old_path: Option<String>,
}

/// The flush operation shapes the LSP distinguishes on the wire (ADR 0113
/// LSP follow-up, BT-3212; ADR 0114 LSP follow-up, BT-3275) — bucketed
/// client-side from the runtime's own `entry_kind/1` enum value rather than
/// a value this crate invents: `'new-class'`, `'remove-class'`,
/// `'rename-class'`, and `'rename-method'` map straight across, and every
/// other `entry_kind/1` value (`instance`, `class`, `'remove-method'`, or an
/// unrecognised future value) buckets to `Patch` — the ordinary
/// `TextEdit`/`Change` shape, unchanged since ADR 0082 Phase 3.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FlushFileKind {
    /// `Workspace newClass:at:` — the file did not exist before this flush.
    /// Emitted as a `CreateFile` resource operation.
    NewClass,
    /// `removeFromSystem` (Tier 2 destructive flush, ADR 0113) — the flush
    /// already unlinked the file from disk. Emitted as a `DeleteFile`
    /// resource operation (BT-3209).
    RemoveClass,
    /// `renameTo:`/`moveClass:to:` (Tier 2 destructive flush, ADR 0114,
    /// BT-3271/BT-3275) — a file touched by a class rename. Paired with
    /// `FlushedFile::old_path` (`Some`), emitted as a `RenameFile` resource
    /// operation plus a `TextDocumentEdit` carrying the moved file's new
    /// content; without it (`None`), an ordinary same-batch reference
    /// rewrite in a file that did not itself move — an ordinary patch.
    RenameClass,
    /// `renameSelector:to:` (Tier 2 destructive flush, ADR 0114, BT-3273/
    /// BT-3275) — the definition site or a confirmed sender site of a
    /// method rename (never a `candidate_sites` entry, which is never
    /// staged/written and so never appears on this wire at all). Emitted as
    /// a `TextDocumentEdit`.
    RenameMethod,
    /// Everything else: an ordinary method patch or `'remove-method'`
    /// excision against a file that existed both before and after.
    /// Emitted as a whole-document `TextEdit`.
    Patch,
}

impl FlushFileKind {
    /// Bucket a raw `entry_kind/1` wire value (e.g. `"new-class"`,
    /// `"instance"`) into the LSP-relevant shapes.
    fn from_wire(kind: &str) -> Self {
        match kind {
            "new-class" => Self::NewClass,
            "remove-class" => Self::RemoveClass,
            "rename-class" => Self::RenameClass,
            "rename-method" => Self::RenameMethod,
            _ => Self::Patch,
        }
    }
}

/// A class-load or class-reload event surfaced to the LSP server so it
/// can invalidate caches that depend on the class's method dictionary or
/// senders (BT-2239).
///
/// Today the workspace emits `class_loaded` for every register / re-register
/// — including the per-method `Behaviour >>` install path (ADR 0082 Phase 1),
/// since those routes both go through `beamtalk_class_builder`. The LSP
/// treats this as a coarse "any nav cache for this class is stale" signal;
/// it does not yet distinguish method-level patches from full reloads.
#[derive(Debug, Clone)]
pub struct ClassChangedEvent {
    /// Beamtalk class name (as reported on the wire, no `class` suffix).
    pub class_name: String,
}

/// One caller call-site inside a reload-induced finding (ADR 0105 Phase 1,
/// BT-2779) — mirrors `beamtalk_recheck:site_ref()`
/// (`runtime/apps/beamtalk_workspace/src/beamtalk_recheck.erl`). `line` is
/// the 1-based line xref recorded for the call site, not a byte offset —
/// same precedent as [`beamtalk_core::language_service::NavSite`].
#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct ReloadSite {
    /// Selector of the caller method containing the site.
    pub method: String,
    /// 1-based line number of the send.
    pub line: u32,
}

/// One reload-induced finding — a caller whose re-check surfaced a
/// signature-change or removed-selector diagnostic attributable to a live
/// reload. Mirrors `beamtalk_recheck:finding()`.
#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct ReloadFinding {
    /// Caller class this finding is attributed to (the document the LSP
    /// publishes the diagnostic against).
    pub owner: String,
    /// The class whose reload triggered this re-check.
    #[serde(rename = "changedClass")]
    pub changed_class: String,
    /// The selector that changed or was removed on `changed_class`.
    pub selector: String,
    /// `"signature_change"` or `"removal"` (ADR 0105 §Mechanism step 1).
    pub classification: String,
    /// ADR 0100 severity string (`"error"` | `"warning"` | `"lint"` |
    /// `"hint"`) — `"error"` never appears (`beamtalk_recheck` drops it,
    /// a reload finding is advisory, never build-failing).
    pub severity: String,
    /// Diagnostic category (`"Dnu"` / `"Type"`), when the checker reported one.
    pub category: Option<String>,
    /// The underlying diagnostic's message.
    pub message: String,
    /// Reload attribution note (e.g. "removed by the reload of Counter"),
    /// present only for `removal` findings.
    pub note: Option<String>,
    /// Call sites xref recorded for this owner/selector pair.
    pub sites: Vec<ReloadSite>,
    /// Byte-offset span of the diagnostic in the owner's live combined
    /// source (not necessarily the same bytes as the on-disk file — a
    /// flagged caller may itself carry unflushed edits). The LSP surface
    /// uses [`Self::sites`]' line numbers instead of this span, since there
    /// is no existing machinery to map a byte offset in the *live* combined
    /// source back to an on-disk position (see `nav_site_to_location`'s doc)
    /// — kept here for parity with the wire payload and possible future use.
    #[allow(dead_code, reason = "kept for wire parity; LSP publishes by site line")]
    pub start: u32,
    /// See [`Self::start`].
    #[allow(dead_code, reason = "kept for wire parity; LSP publishes by site line")]
    pub end: u32,
}

/// A `reload_check`/`completed` push event (ADR 0105 Phase 1, BT-2779): the
/// outcome of `beamtalk_repl_loader:maybe_trigger_recheck/4` for one live
/// reload, surfaced to the LSP so it can publish/clear reload-induced
/// diagnostics on the caller classes' documents.
///
/// `checked_owners` is the clearing-by-replacement signal (ADR 0105
/// §Mechanism step 4): for every owner listed, the LSP must replace
/// whatever reload-induced diagnostics it is currently showing for that
/// owner's document with `findings` filtered to that owner — possibly
/// none, which is exactly how a stale finding disappears without anyone
/// editing the caller (reload-fixes-reload) or how a plain hand-edit's own
/// stale findings get cleared (`classification: "self_edit"`, `findings:
/// []`, `checked: 0`).
#[derive(Debug, Clone, Deserialize)]
pub struct ReloadCheckEvent {
    /// The class whose reload triggered this event.
    #[serde(rename = "changedClass")]
    pub changed_class: String,
    /// The selector that changed or was removed.
    #[serde(rename = "changedSelector")]
    pub changed_selector: String,
    /// `"signature_change"` | `"removal"` | `"self_edit"` (the last meaning
    /// no dependent re-check ran — only clearing happened).
    pub classification: String,
    /// Number of distinct caller classes a diagnostics round-trip completed for.
    pub checked: u32,
    /// Number of known dependents skipped by the per-reload caller cap.
    #[serde(rename = "notChecked")]
    pub not_checked: u32,
    /// `"N more not checked"` note when the caller cap was exceeded.
    #[serde(rename = "capNote")]
    pub cap_note: Option<String>,
    /// Every caller class this event's `findings` are authoritative for —
    /// see the struct doc's clearing-by-replacement note.
    #[serde(rename = "checkedOwners")]
    pub checked_owners: Vec<String>,
    /// The stale findings themselves (empty when every checked owner came
    /// back clean).
    pub findings: Vec<ReloadFinding>,
}

/// Snapshot payload for the `reload-findings` op response (BT-2801, ADR 0105
/// surface-parity gap) — deserializes the op's `{"findings": [...]}` `value`
/// payload. `ReloadFinding` is the exact per-finding shape shared with
/// [`ReloadCheckEvent::findings`]; the Erlang side produces both from the
/// same `encode_reload_finding/1`, so the two never disagree on shape.
#[derive(Debug, Clone, Deserialize)]
struct ReloadFindingsResponse {
    findings: Vec<ReloadFinding>,
}

/// WebSocket client to a running Beamtalk workspace.
///
/// Single-connection, single-process. Cloneable handle so async tasks can
/// share it (the inner socket is mutex-protected). When the last clone is
/// dropped the inner reader/writer tasks are aborted (via [`Drop`] for
/// `RuntimeInner`) so the WebSocket is reliably torn down — no need to call
/// [`RuntimeClient::close`] explicitly. `close` is still available for
/// callers that want to tear down eagerly.
#[derive(Clone)]
pub struct RuntimeClient {
    inner: Arc<RuntimeInner>,
}

struct RuntimeInner {
    /// Sender half of the writer task channel. The LSP side calls
    /// `evaluate(...)` which sends a request + correlation oneshot here; the
    /// writer task serialises to the socket and the reader task fulfills the
    /// oneshot when the reply arrives.
    sender: mpsc::Sender<EvalRequest>,
    /// Listener task handle; stored so the task survives as long as the
    /// `RuntimeClient` exists, and so `close()` can abort it explicitly.
    #[allow(dead_code)] // accessed via `close()`
    listener: Mutex<Option<tokio::task::JoinHandle<()>>>,
    /// Writer task handle; same purpose as `listener`.
    #[allow(dead_code)] // accessed via `close()`
    writer: Mutex<Option<tokio::task::JoinHandle<()>>>,
}

struct EvalRequest {
    request: serde_json::Value,
    id: String,
    reply_to: oneshot::Sender<Result<ReplResponse, RuntimeError>>,
}

impl Drop for RuntimeInner {
    fn drop(&mut self) {
        // Abort the writer/listener tasks so the WebSocket is reliably torn
        // down when the last `RuntimeClient` handle is dropped. By the time
        // `Drop` runs the inner `Arc` has reached refcount zero, so
        // `try_lock` cannot contend with any other holder of these mutexes.
        if let Ok(mut g) = self.writer.try_lock() {
            if let Some(h) = g.take() {
                h.abort();
            }
        }
        if let Ok(mut g) = self.listener.try_lock() {
            if let Some(h) = g.take() {
                h.abort();
            }
        }
    }
}

impl RuntimeClient {
    /// Discover and connect to the workspace owning `project_path`.
    ///
    /// Reads the workspace id from the project path (via
    /// `beamtalk_workspace::generate_workspace_id`), then the port + cookie
    /// from `~/.beamtalk/workspaces/<id>/`. Errors with
    /// [`RuntimeError::WorkspaceNotFound`] if no port file is present —
    /// callers treat this as "no workspace running, give up" and either skip
    /// the runtime-backed feature or surface a friendly message to the
    /// editor.
    ///
    /// `flush_tx` receives `{flush_completed, files: [...]}` push frames
    /// translated to [`FlushEvent`]. The channel is unbounded so a slow
    /// `applyEdit` task can't backpressure the listener.
    ///
    /// `class_changed_tx` receives `{classes, loaded}` push frames
    /// translated to [`ClassChangedEvent`] (BT-2239) — used by the LSP to
    /// invalidate runtime-attached nav caches. Pass an unbounded sender so
    /// the listener never blocks. Listeners that don't care can drop the
    /// receiver — the send will fail silently, which is fine for a
    /// best-effort signal.
    ///
    /// `reload_check_tx` receives `{reload_check, completed}` push frames
    /// translated to [`ReloadCheckEvent`] (ADR 0105 Phase 1, BT-2779) — used
    /// by the LSP to publish/clear reload-induced diagnostics.
    pub async fn connect(
        project_path: &Path,
        flush_tx: mpsc::UnboundedSender<FlushEvent>,
        class_changed_tx: mpsc::UnboundedSender<ClassChangedEvent>,
        reload_check_tx: mpsc::UnboundedSender<ReloadCheckEvent>,
    ) -> Result<Self, RuntimeError> {
        let workspace_id =
            beamtalk_workspace::generate_workspace_id(project_path).map_err(|e| {
                RuntimeError::WorkspaceNotFound {
                    project_path: project_path.display().to_string(),
                    reason: format!("failed to derive workspace id: {e}"),
                }
            })?;

        let (port, _nonce) = beamtalk_workspace::read_port_file(&workspace_id)
            .map_err(|e| RuntimeError::WorkspaceNotFound {
                project_path: project_path.display().to_string(),
                reason: format!("failed to read port file: {e}"),
            })?
            .ok_or_else(|| RuntimeError::WorkspaceNotFound {
                project_path: project_path.display().to_string(),
                reason: "no port file under ~/.beamtalk/workspaces/<id>/".to_string(),
            })?;

        let cookie = beamtalk_workspace::read_cookie_file(&workspace_id)
            .map_err(|e| RuntimeError::WorkspaceNotFound {
                project_path: project_path.display().to_string(),
                reason: format!("failed to read cookie file: {e}"),
            })?
            .ok_or_else(|| RuntimeError::WorkspaceNotFound {
                project_path: project_path.display().to_string(),
                reason: "no cookie file under ~/.beamtalk/workspaces/<id>/".to_string(),
            })?;

        Self::connect_to(port, &cookie, flush_tx, class_changed_tx, reload_check_tx).await
    }

    /// Connect directly to a workspace on `port` with `cookie`. Used by
    /// tests; production callers go through [`RuntimeClient::connect`].
    pub async fn connect_to(
        port: u16,
        cookie: &str,
        flush_tx: mpsc::UnboundedSender<FlushEvent>,
        class_changed_tx: mpsc::UnboundedSender<ClassChangedEvent>,
        reload_check_tx: mpsc::UnboundedSender<ReloadCheckEvent>,
    ) -> Result<Self, RuntimeError> {
        let url = format!("ws://127.0.0.1:{port}/ws");
        let connect_fut = connect_async(&url);
        let (mut ws, _resp) = tokio::time::timeout(CONNECT_TIMEOUT, connect_fut)
            .await
            .map_err(|_| RuntimeError::Connect {
                port,
                reason: format!(
                    "timed out connecting to workspace at {url} ({}s)",
                    CONNECT_TIMEOUT.as_secs()
                ),
            })?
            .map_err(|e| RuntimeError::Connect {
                port,
                reason: format!("websocket connect failed: {e}"),
            })?;

        perform_auth_handshake(&mut ws, cookie)
            .await
            .map_err(|e| RuntimeError::Connect { port, reason: e })?;

        info!(port, "LSP runtime client connected to workspace");

        // Split the socket so the reader and writer halves can run
        // independently. The reader dispatches replies + push frames; the
        // writer serialises eval submissions.
        let (sink, stream) = ws.split();
        let pending: Arc<Mutex<PendingMap>> = Arc::new(Mutex::new(PendingMap::default()));
        let (req_tx, req_rx) = mpsc::channel::<EvalRequest>(64);

        let writer = tokio::spawn(writer_task(sink, req_rx, Arc::clone(&pending)));
        let listener = tokio::spawn(listener_task(
            stream,
            pending,
            flush_tx,
            class_changed_tx,
            reload_check_tx,
        ));

        Ok(Self {
            inner: Arc::new(RuntimeInner {
                sender: req_tx,
                listener: Mutex::new(Some(listener)),
                writer: Mutex::new(Some(writer)),
            }),
        })
    }

    /// Submit `code` as an `eval` request and wait for the reply.
    ///
    /// Returns the parsed [`ReplResponse`]; the caller checks `is_error()` to
    /// distinguish between a structured `#beamtalk_error{}` and a successful
    /// value. A transport-level failure (socket closed, timeout) surfaces as
    /// [`RuntimeError::Protocol`].
    pub async fn evaluate(&self, code: &str) -> Result<ReplResponse, RuntimeError> {
        let request = RequestBuilder::eval(code);
        let id = request
            .get("id")
            .and_then(|v| v.as_str())
            .ok_or_else(|| RuntimeError::Protocol("eval request missing id".to_string()))?
            .to_string();
        self.dispatch_request(request, &id, "eval").await
    }

    /// Submit a structured `nav-query` request and decode the typed reply
    /// (BT-2239).
    ///
    /// Unlike [`Self::evaluate`], this op bypasses the Beamtalk inspect-string
    /// formatter — the runtime serialises `beamtalk_xref` site records as
    /// plain JSON arrays/objects, so the reply decodes directly into typed
    /// [`NavSite`] records.
    ///
    /// Returns:
    /// * `Ok(Vec<NavSite>)` on a successful reply (possibly empty when no
    ///   matches were found — distinguished from an absent runtime by the
    ///   surrounding `Backend::delegate_nav_query` seam).
    /// * `Err(RuntimeError::Protocol)` for transport-level failures or a
    ///   structured `#beamtalk_error{}` reply (the latter is rare — the op
    ///   validates inputs up front and `beamtalk_xref` lookups don't fail).
    pub async fn nav_query(&self, query: &NavQuery) -> Result<Vec<NavSite>, RuntimeError> {
        let arg = query
            .selector()
            .or_else(|| query.class_name())
            .ok_or_else(|| {
                RuntimeError::Protocol("nav-query: missing selector/class argument".to_string())
            })?;
        let request = RequestBuilder::nav_query(query.kind(), arg);
        let id = request
            .get("id")
            .and_then(|v| v.as_str())
            .ok_or_else(|| RuntimeError::Protocol("nav-query request missing id".to_string()))?
            .to_string();

        let response = self.dispatch_request(request, &id, "nav-query").await?;

        let payload: NavQueryResponse = decode_rpc_reply(response, "nav-query")?;
        Ok(payload.sites)
    }

    /// Submit a `nav-symbols` request (BT-2244) and decode the typed reply.
    ///
    /// The op is the bulk-outline sibling of `nav-query` — used by the LSP
    /// `textDocument/documentSymbol` and `workspace/symbol` handlers to
    /// source their class+method set from the live class registry (so
    /// REPL-loaded classes and live-edited methods surface in the editor
    /// even though they have no `.bt` file the AST walker can index).
    ///
    /// `scope` filters the result set:
    /// * `Some("user")` — only classes with a backing source file (used by
    ///   `documentSymbol`, where a URI is the natural lookup key)
    /// * `Some("all")` / `None` — every loaded class (used by
    ///   `workspace/symbol`, where source-less classes are the headline
    ///   win)
    ///
    /// Returns:
    /// * `Ok(Vec<NavSymbolClass>)` on success (possibly empty).
    /// * `Err(RuntimeError::Protocol)` for transport-level failures or a
    ///   structured `#beamtalk_error{}` reply.
    pub async fn nav_symbols(
        &self,
        scope: Option<&str>,
    ) -> Result<Vec<NavSymbolClass>, RuntimeError> {
        let request = RequestBuilder::nav_symbols(scope);
        let id = request
            .get("id")
            .and_then(|v| v.as_str())
            .ok_or_else(|| RuntimeError::Protocol("nav-symbols request missing id".to_string()))?
            .to_string();

        let response = self.dispatch_request(request, &id, "nav-symbols").await?;

        let payload: NavSymbolsResponse = decode_rpc_reply(response, "nav-symbols")?;
        Ok(payload.classes)
    }

    /// One-shot snapshot read of every currently-live reload-induced finding
    /// (BT-2801, ADR 0105 surface-parity gap) — the request/response
    /// counterpart to the `reload_check`/`completed` push frame
    /// [`ReloadCheckEvent`] arrives on. Callers use this to seed state on
    /// attach (findings that already existed in
    /// `beamtalk_workspace_findings_store` before this client connected are
    /// otherwise invisible until the next reload happens to touch a given
    /// caller again).
    ///
    /// Returns:
    /// * `Ok(Vec<ReloadFinding>)` on success (possibly empty).
    /// * `Err(RuntimeError::Protocol)` for transport-level failures or a
    ///   structured `#beamtalk_error{}` reply.
    pub async fn reload_findings(&self) -> Result<Vec<ReloadFinding>, RuntimeError> {
        let request = RequestBuilder::reload_findings();
        let id = request
            .get("id")
            .and_then(|v| v.as_str())
            .ok_or_else(|| {
                RuntimeError::Protocol("reload-findings request missing id".to_string())
            })?
            .to_string();

        let response = self
            .dispatch_request(request, &id, "reload-findings")
            .await?;

        let payload: ReloadFindingsResponse = decode_rpc_reply(response, "reload-findings")?;
        Ok(payload.findings)
    }

    /// Send `request` through the eval channel and wait up to `IO_TIMEOUT` for
    /// the reply. `op` names the operation (e.g. `"eval"`, `"nav-query"`) and
    /// is interpolated into every error message so failures are easy to
    /// diagnose. Returns the inner `Result<ReplResponse, RuntimeError>` carried
    /// by the oneshot channel, with transport-level failures (timeout, shutdown,
    /// dropped channel) converted to `Err(RuntimeError::Protocol)` and
    /// propagated early.
    async fn dispatch_request(
        &self,
        request: serde_json::Value,
        id: &str,
        op: &str,
    ) -> Result<ReplResponse, RuntimeError> {
        let (reply_tx, reply_rx) = oneshot::channel();
        let req = EvalRequest {
            request,
            id: id.to_string(),
            reply_to: reply_tx,
        };
        self.inner
            .sender
            .send(req)
            .await
            .map_err(|_| RuntimeError::Protocol("runtime client shut down".to_string()))?;

        tokio::time::timeout(IO_TIMEOUT, reply_rx)
            .await
            .map_err(|_| {
                RuntimeError::Protocol(format!(
                    "{op} timed out after {}s waiting for reply (id={id})",
                    IO_TIMEOUT.as_secs()
                ))
            })?
            .map_err(|_| {
                RuntimeError::Protocol(format!("{op} reply channel dropped before response"))
            })?
    }

    /// Close the underlying connection and abort the listener/writer tasks.
    /// Safe to call multiple times. Not currently called by the LSP backend —
    /// the runtime client lives as long as the LSP process and the OS closes
    /// the TCP socket on exit — but exposed for tests and future use.
    #[allow(dead_code)]
    pub async fn close(&self) {
        if let Some(handle) = self.inner.writer.lock().await.take() {
            handle.abort();
        }
        if let Some(handle) = self.inner.listener.lock().await.take() {
            handle.abort();
        }
    }
}

#[derive(Default)]
struct PendingMap {
    by_id: std::collections::HashMap<String, oneshot::Sender<Result<ReplResponse, RuntimeError>>>,
}

async fn writer_task(
    mut sink: futures_util::stream::SplitSink<WsStream, Message>,
    mut req_rx: mpsc::Receiver<EvalRequest>,
    pending: Arc<Mutex<PendingMap>>,
) {
    while let Some(EvalRequest {
        request,
        id,
        reply_to,
    }) = req_rx.recv().await
    {
        let body = match serde_json::to_string(&request) {
            Ok(s) => s,
            Err(e) => {
                let _ = reply_to.send(Err(RuntimeError::Protocol(format!(
                    "failed to serialise eval request: {e}"
                ))));
                continue;
            }
        };
        pending.lock().await.by_id.insert(id.clone(), reply_to);
        if let Err(e) = sink.send(Message::Text(body.into())).await {
            // Pull the reply_to back out so we can fail it. If something else
            // already removed it (e.g. the listener task), nothing to do.
            if let Some(tx) = pending.lock().await.by_id.remove(&id) {
                let _ = tx.send(Err(RuntimeError::Protocol(format!(
                    "websocket send failed: {e}"
                ))));
            }
            break;
        }
    }
}

async fn listener_task(
    mut stream: futures_util::stream::SplitStream<WsStream>,
    pending: Arc<Mutex<PendingMap>>,
    flush_tx: mpsc::UnboundedSender<FlushEvent>,
    class_changed_tx: mpsc::UnboundedSender<ClassChangedEvent>,
    reload_check_tx: mpsc::UnboundedSender<ReloadCheckEvent>,
) {
    while let Some(msg) = stream.next().await {
        match msg {
            Ok(Message::Text(text)) => {
                let value: serde_json::Value = match serde_json::from_str(&text) {
                    Ok(v) => v,
                    Err(e) => {
                        warn!(error = %e, frame = %text, "runtime: unparseable frame");
                        continue;
                    }
                };
                // Push frame? Dispatch and continue.
                if value.get("type").and_then(|v| v.as_str()) == Some("push") {
                    handle_push_frame(&value, &flush_tx, &class_changed_tx, &reload_check_tx);
                    continue;
                }
                // Otherwise it's a reply to a pending request — look up by id.
                if let Some(id) = value.get("id").and_then(|v| v.as_str()) {
                    let id = id.to_string();
                    let tx_opt = pending.lock().await.by_id.remove(&id);
                    if let Some(tx) = tx_opt {
                        match serde_json::from_value::<ReplResponse>(value) {
                            Ok(resp) => {
                                let _ = tx.send(Ok(resp));
                            }
                            Err(e) => {
                                let _ = tx.send(Err(RuntimeError::Protocol(format!(
                                    "failed to parse runtime reply: {e}"
                                ))));
                            }
                        }
                    } else {
                        debug!(id, "runtime: reply for unknown id");
                    }
                }
            }
            Ok(Message::Close(_)) => {
                debug!("runtime: websocket closed by server");
                break;
            }
            Ok(_) => {
                // Ignore binary / ping / pong frames.
            }
            Err(e) => {
                warn!(error = %e, "runtime: websocket error");
                break;
            }
        }
    }
    // Drain pending requests with a transport error so callers don't hang.
    let mut p = pending.lock().await;
    for (_id, tx) in p.by_id.drain() {
        let _ = tx.send(Err(RuntimeError::Protocol(
            "runtime websocket closed before reply".to_string(),
        )));
    }
}

fn handle_push_frame(
    value: &serde_json::Value,
    flush_tx: &mpsc::UnboundedSender<FlushEvent>,
    class_changed_tx: &mpsc::UnboundedSender<ClassChangedEvent>,
    reload_check_tx: &mpsc::UnboundedSender<ReloadCheckEvent>,
) {
    let channel = value.get("channel").and_then(|v| v.as_str());
    let event = value.get("event").and_then(|v| v.as_str());
    // Other push channels (actors, bindings, transcript, logs) are not
    // consumed by the LSP today — fall through and drop silently.
    match (channel, event) {
        (Some("workspace"), Some("flush_completed")) => {
            let data = value.get("data");
            let paths = data
                .and_then(|d| d.get("files"))
                .and_then(|f| f.as_array())
                .map(|arr| {
                    arr.iter()
                        .filter_map(|v| v.as_str().map(String::from))
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default();
            if paths.is_empty() {
                debug!("runtime: flush_completed with empty files list");
                return;
            }
            // BT-3212: per-file operation kind, keyed by path — absent (an
            // older producer, or a path this list simply omits) leaves that
            // file's `kind` as `None`, so `flush_event_listener` falls back
            // to the pre-BT-3212 existence check for it (BT-3209 backward
            // compat). BT-3275: also carries the optional `oldFile`
            // companion (present only for the moved `'rename-class'` file).
            let kinds_by_path: std::collections::HashMap<&str, (FlushFileKind, Option<String>)> =
                data.and_then(|d| d.get("fileKinds"))
                    .and_then(|f| f.as_array())
                    .map(|arr| {
                        arr.iter()
                            .filter_map(|entry| {
                                let path = entry.get("file")?.as_str()?;
                                let kind = entry.get("kind")?.as_str()?;
                                let old_path = entry
                                    .get("oldFile")
                                    .and_then(|v| v.as_str())
                                    .map(String::from);
                                Some((path, (FlushFileKind::from_wire(kind), old_path)))
                            })
                            .collect()
                    })
                    .unwrap_or_default();
            let files = paths
                .into_iter()
                .map(|path| {
                    let (kind, old_path) = match kinds_by_path.get(path.as_str()) {
                        Some((kind, old_path)) => (Some(*kind), old_path.clone()),
                        None => (None, None),
                    };
                    FlushedFile {
                        path,
                        kind,
                        old_path,
                    }
                })
                .collect();
            if let Err(e) = flush_tx.send(FlushEvent { files }) {
                warn!(error = %e, "runtime: flush_tx receiver dropped");
            }
        }
        // BT-2239: a class load / reload / method-install (all routed through
        // `beamtalk_class_builder`) invalidates any runtime-attached nav
        // cache keyed on that class's method dictionary or senders.
        (Some("classes"), Some("loaded")) => {
            let class_name = value
                .get("data")
                .and_then(|d| d.get("class"))
                .and_then(|c| c.as_str())
                .map(String::from);
            let Some(class_name) = class_name else {
                debug!("runtime: classes/loaded push with no class name");
                return;
            };
            if let Err(e) = class_changed_tx.send(ClassChangedEvent { class_name }) {
                warn!(error = %e, "runtime: class_changed_tx receiver dropped");
            }
        }
        // ADR 0105 Phase 1 (BT-2779): reload-induced re-check outcome —
        // publish/clear diagnostics on the affected caller classes.
        (Some("reload_check"), Some("completed")) => {
            let Some(data) = value.get("data") else {
                debug!("runtime: reload_check/completed push with no data");
                return;
            };
            let event: ReloadCheckEvent = match serde_json::from_value(data.clone()) {
                Ok(e) => e,
                Err(e) => {
                    warn!(error = %e, "runtime: malformed reload_check/completed payload");
                    return;
                }
            };
            if let Err(e) = reload_check_tx.send(event) {
                warn!(error = %e, "runtime: reload_check_tx receiver dropped");
            }
        }
        _ => {}
    }
}

async fn perform_auth_handshake(ws: &mut WsStream, cookie: &str) -> Result<(), String> {
    use tokio_tungstenite::tungstenite::Message;

    // Read auth-required. Frame recognition goes through
    // `beamtalk_repl_protocol::handshake` (BT-3330) rather than re-matching
    // the JSON here — see that module's doc comment for why.
    let auth_required = read_text(ws).await?;
    let auth_required_json: serde_json::Value = serde_json::from_str(&auth_required)
        .map_err(|e| format!("failed to parse auth-required: {e}"))?;
    if !handshake::is_auth_required(&auth_required_json) {
        return Err(format!("unexpected pre-auth message: {auth_required_json}"));
    }

    // Send auth (no resume — LSP always opens fresh). `client` tags the session
    // surface so `Workspace sessions` can show it originated from the LSP.
    let auth_msg = handshake::auth_request(cookie, "lsp", None);
    let auth_str =
        serde_json::to_string(&auth_msg).map_err(|e| format!("failed to serialise auth: {e}"))?;
    ws.send(Message::Text(auth_str.into()))
        .await
        .map_err(|e| format!("failed to send auth: {e}"))?;

    // Read auth_ok / auth_error
    let resp = read_text(ws).await?;
    let resp_json: serde_json::Value =
        serde_json::from_str(&resp).map_err(|e| format!("failed to parse auth response: {e}"))?;
    match handshake::parse_auth_ack(&resp_json) {
        Some(handshake::AuthAck::Ok) => {}
        Some(handshake::AuthAck::Error { message }) => {
            let msg = message.as_deref().unwrap_or("authentication failed");
            return Err(format!("workspace authentication failed: {msg}"));
        }
        None => return Err(format!("unexpected auth response: {resp_json}")),
    }

    // Read session-started
    let started = read_text(ws).await?;
    let started_json: serde_json::Value = serde_json::from_str(&started)
        .map_err(|e| format!("failed to parse session-started: {e}"))?;
    if !handshake::is_session_started(&started_json) {
        return Err(format!("unexpected post-auth message: {started_json}"));
    }
    Ok(())
}

async fn read_text(ws: &mut WsStream) -> Result<String, String> {
    let read_fut = async {
        loop {
            match ws.next().await {
                Some(Ok(Message::Text(text))) => return Ok::<String, String>(text.to_string()),
                Some(Ok(Message::Close(_))) => {
                    return Err("workspace closed websocket during handshake".to_string());
                }
                Some(Ok(_)) => {
                    // Ignore binary / ping / pong frames during the
                    // handshake; we only care about the JSON text frames.
                }
                Some(Err(e)) => return Err(format!("websocket read failed: {e}")),
                None => return Err("websocket stream ended during handshake".to_string()),
            }
        }
    };
    tokio::time::timeout(IO_TIMEOUT, read_fut)
        .await
        .map_err(|_| format!("websocket read timed out after {}s", IO_TIMEOUT.as_secs()))?
}

/// Decode a typed payload from a [`ReplResponse`] received after a named RPC op.
///
/// Shared by [`RuntimeClient::nav_query`] and [`RuntimeClient::nav_symbols`]
/// (and any future typed ops) to avoid repeating the same three-step check:
/// 1. error flag → `RuntimeError::Protocol("{op} error: {msg}")`
/// 2. missing `value` field → `RuntimeError::Protocol("{op}: reply missing `value`")`
/// 3. JSON deserialise failure → `RuntimeError::Protocol("{op}: malformed reply payload: {e}")`
fn decode_rpc_reply<T: DeserializeOwned>(
    response: ReplResponse,
    op: &str,
) -> Result<T, RuntimeError> {
    if response.is_error() {
        let msg = response
            .error
            .or(response.message)
            .unwrap_or_else(|| "unknown error".to_string());
        return Err(RuntimeError::Protocol(format!("{op} error: {msg}")));
    }
    let value = response
        .value
        .ok_or_else(|| RuntimeError::Protocol(format!("{op}: reply missing `value`")))?;
    serde_json::from_value(value)
        .map_err(|e| RuntimeError::Protocol(format!("{op}: malformed reply payload: {e}")))
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde::Deserialize;
    use serde_json::json;
    use tokio::sync::mpsc::unbounded_channel;

    #[derive(Debug, Deserialize, PartialEq)]
    struct DummyPayload {
        foo: String,
    }

    /// The three push-event channels [`RuntimeClient::connect_to`] takes,
    /// bundled so a connection test names only the receivers it asserts on.
    /// (The `handle_push_frame` unit tests above build their channels inline
    /// — each of them asserts on all three.)
    struct Sinks {
        flush_tx: mpsc::UnboundedSender<FlushEvent>,
        flush_rx: mpsc::UnboundedReceiver<FlushEvent>,
        class_tx: mpsc::UnboundedSender<ClassChangedEvent>,
        class_rx: mpsc::UnboundedReceiver<ClassChangedEvent>,
        reload_tx: mpsc::UnboundedSender<ReloadCheckEvent>,
        reload_rx: mpsc::UnboundedReceiver<ReloadCheckEvent>,
    }

    impl Sinks {
        fn new() -> Self {
            let (flush_tx, flush_rx) = unbounded_channel();
            let (class_tx, class_rx) = unbounded_channel();
            let (reload_tx, reload_rx) = unbounded_channel();
            Self {
                flush_tx,
                flush_rx,
                class_tx,
                class_rx,
                reload_tx,
                reload_rx,
            }
        }
    }

    #[test]
    fn decode_rpc_reply_success() {
        let response: ReplResponse =
            serde_json::from_value(json!({"id": "1", "value": {"foo": "bar"}})).unwrap();
        let payload: DummyPayload = decode_rpc_reply(response, "dummy-op").unwrap();
        assert_eq!(
            payload,
            DummyPayload {
                foo: "bar".to_string()
            }
        );
    }

    #[test]
    fn decode_rpc_reply_error_flag_set() {
        let response: ReplResponse = serde_json::from_value(json!({
            "id": "1",
            "status": ["done", "error"],
            "error": "boom"
        }))
        .unwrap();
        let err = decode_rpc_reply::<DummyPayload>(response, "dummy-op").unwrap_err();
        assert_eq!(
            err.to_string(),
            "runtime protocol error: dummy-op error: boom"
        );
    }

    #[test]
    fn decode_rpc_reply_falls_back_to_legacy_message_field() {
        // Older producers carry the text in `message`, not `error`.
        let response: ReplResponse = serde_json::from_value(json!({
            "id": "1",
            "status": ["done", "error"],
            "message": "legacy failure text"
        }))
        .unwrap();
        let err = decode_rpc_reply::<DummyPayload>(response, "dummy-op").unwrap_err();
        assert_eq!(
            err.to_string(),
            "runtime protocol error: dummy-op error: legacy failure text"
        );
    }

    #[test]
    fn decode_rpc_reply_error_flag_without_any_message() {
        let response: ReplResponse =
            serde_json::from_value(json!({"id": "1", "status": ["done", "error"]})).unwrap();
        let err = decode_rpc_reply::<DummyPayload>(response, "dummy-op").unwrap_err();
        assert_eq!(
            err.to_string(),
            "runtime protocol error: dummy-op error: unknown error"
        );
    }

    #[test]
    fn decode_rpc_reply_missing_value() {
        let response: ReplResponse = serde_json::from_value(json!({"id": "1"})).unwrap();
        let err = decode_rpc_reply::<DummyPayload>(response, "dummy-op").unwrap_err();
        assert_eq!(
            err.to_string(),
            "runtime protocol error: dummy-op: reply missing `value`"
        );
    }

    #[test]
    fn decode_rpc_reply_malformed_payload() {
        let response: ReplResponse =
            serde_json::from_value(json!({"id": "1", "value": {"foo": 42}})).unwrap();
        let err = decode_rpc_reply::<DummyPayload>(response, "dummy-op").unwrap_err();
        let msg = err.to_string();
        assert!(
            msg.starts_with("runtime protocol error: dummy-op: malformed reply payload:"),
            "unexpected error message: {msg}"
        );
    }

    #[tokio::test]
    async fn push_frame_with_files_is_forwarded() {
        let (tx, mut rx) = unbounded_channel::<FlushEvent>();
        let (class_tx, _class_rx) = unbounded_channel::<ClassChangedEvent>();
        let (reload_tx, _reload_rx) = unbounded_channel::<ReloadCheckEvent>();
        handle_push_frame(
            &json!({
                "type": "push",
                "channel": "workspace",
                "event": "flush_completed",
                "data": {
                    "files": ["src/counter.bt", "src/foo.bt"]
                }
            }),
            &tx,
            &class_tx,
            &reload_tx,
        );
        let evt = rx.recv().await.expect("flush event");
        let paths: Vec<&str> = evt.files.iter().map(|f| f.path.as_str()).collect();
        assert_eq!(paths, vec!["src/counter.bt", "src/foo.bt"]);
        // No `fileKinds` companion on the wire: every file's kind is `None`
        // (BT-3209 fallback tolerance for a pre-BT-3212 producer shape).
        assert!(evt.files.iter().all(|f| f.kind.is_none()));
    }

    #[tokio::test]
    async fn push_frame_with_file_kinds_is_forwarded() {
        let (tx, mut rx) = unbounded_channel::<FlushEvent>();
        let (class_tx, _class_rx) = unbounded_channel::<ClassChangedEvent>();
        let (reload_tx, _reload_rx) = unbounded_channel::<ReloadCheckEvent>();
        handle_push_frame(
            &json!({
                "type": "push",
                "channel": "workspace",
                "event": "flush_completed",
                "data": {
                    "files": ["src/greeter.bt", "src/counter.bt", "src/widget.bt"],
                    "fileKinds": [
                        {"file": "src/greeter.bt", "kind": "new-class"},
                        {"file": "src/counter.bt", "kind": "instance"},
                        {"file": "src/widget.bt", "kind": "remove-class"}
                    ]
                }
            }),
            &tx,
            &class_tx,
            &reload_tx,
        );
        let evt = rx.recv().await.expect("flush event");
        let kinds: Vec<(&str, Option<FlushFileKind>)> = evt
            .files
            .iter()
            .map(|f| (f.path.as_str(), f.kind))
            .collect();
        assert_eq!(
            kinds,
            vec![
                ("src/greeter.bt", Some(FlushFileKind::NewClass)),
                ("src/counter.bt", Some(FlushFileKind::Patch)),
                ("src/widget.bt", Some(FlushFileKind::RemoveClass)),
            ]
        );
    }

    #[tokio::test]
    async fn push_frame_with_rename_class_old_file_is_forwarded() {
        // BT-3275: `oldFile` distinguishes the moved declaration file
        // (`RenameClass` + `old_path = Some`) from an ordinary same-batch
        // reference-rewrite file that shares the same `kind` but never
        // moved (`RenameClass` + `old_path = None`).
        let (tx, mut rx) = unbounded_channel::<FlushEvent>();
        let (class_tx, _class_rx) = unbounded_channel::<ClassChangedEvent>();
        let (reload_tx, _reload_rx) = unbounded_channel::<ReloadCheckEvent>();
        handle_push_frame(
            &json!({
                "type": "push",
                "channel": "workspace",
                "event": "flush_completed",
                "data": {
                    "files": ["src/accumulator.bt", "src/widget.bt"],
                    "fileKinds": [
                        {"file": "src/accumulator.bt", "kind": "rename-class", "oldFile": "src/counter.bt"},
                        {"file": "src/widget.bt", "kind": "rename-class"}
                    ]
                }
            }),
            &tx,
            &class_tx,
            &reload_tx,
        );
        let evt = rx.recv().await.expect("flush event");
        let got: Vec<(&str, Option<FlushFileKind>, Option<&str>)> = evt
            .files
            .iter()
            .map(|f| (f.path.as_str(), f.kind, f.old_path.as_deref()))
            .collect();
        assert_eq!(
            got,
            vec![
                (
                    "src/accumulator.bt",
                    Some(FlushFileKind::RenameClass),
                    Some("src/counter.bt")
                ),
                ("src/widget.bt", Some(FlushFileKind::RenameClass), None),
            ]
        );
    }

    #[tokio::test]
    async fn push_frame_with_rename_method_kind_is_forwarded() {
        let (tx, mut rx) = unbounded_channel::<FlushEvent>();
        let (class_tx, _class_rx) = unbounded_channel::<ClassChangedEvent>();
        let (reload_tx, _reload_rx) = unbounded_channel::<ReloadCheckEvent>();
        handle_push_frame(
            &json!({
                "type": "push",
                "channel": "workspace",
                "event": "flush_completed",
                "data": {
                    "files": ["src/counter.bt", "src/sub_counter.bt"],
                    "fileKinds": [
                        {"file": "src/counter.bt", "kind": "rename-method"},
                        {"file": "src/sub_counter.bt", "kind": "rename-method"}
                    ]
                }
            }),
            &tx,
            &class_tx,
            &reload_tx,
        );
        let evt = rx.recv().await.expect("flush event");
        assert!(
            evt.files
                .iter()
                .all(|f| f.kind == Some(FlushFileKind::RenameMethod) && f.old_path.is_none())
        );
    }

    /// BT-3275 conformance: `FlushFileKind::from_wire` must bucket every
    /// atom `beamtalk_workspace_changelog:kind()` (Erlang) admits the same
    /// way this corpus pins it. The corpus is the single source of truth
    /// both language-side implementations are pinned to; the Erlang side
    /// asserts the identical wire-string set against
    /// `beamtalk_workspace_changelog:known_entry_kinds/0` — the
    /// runtime-introspectable image of that type's literal union — in
    /// `beamtalk_workspace_changelog_tests:
    /// known_entry_kinds_matches_shared_wire_corpus/0`. Neither side
    /// hand-derives the other's expected values; both read the same file
    /// (`docs/development/architecture-principles.md` §6: a rule crossing
    /// the Rust/Erlang boundary needs a shared conformance fixture, not a
    /// comment).
    #[test]
    fn from_wire_matches_shared_wire_corpus() {
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("crates/")
            .parent()
            .expect("repo root")
            .join("runtime/apps/beamtalk_workspace/test/fixtures/flush_file_kind_wire_corpus.json");
        let raw = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("read corpus {}: {e}", path.display()));
        let cases: Vec<serde_json::Value> =
            serde_json::from_str(&raw).expect("corpus is a JSON array");
        assert!(!cases.is_empty(), "corpus must have cases");
        for case in &cases {
            let wire = case["wire"].as_str().expect("case.wire is a string");
            let expected = case["rust_variant"]
                .as_str()
                .expect("case.rust_variant is a string");
            let why = case["why"].as_str().unwrap_or("");
            assert_eq!(
                format!("{:?}", FlushFileKind::from_wire(wire)),
                expected,
                "corpus mismatch for wire kind {wire:?} ({why})"
            );
        }
    }

    #[tokio::test]
    async fn push_frame_with_empty_files_is_dropped() {
        let (tx, mut rx) = unbounded_channel::<FlushEvent>();
        let (class_tx, _class_rx) = unbounded_channel::<ClassChangedEvent>();
        let (reload_tx, _reload_rx) = unbounded_channel::<ReloadCheckEvent>();
        handle_push_frame(
            &json!({
                "type": "push",
                "channel": "workspace",
                "event": "flush_completed",
                "data": { "files": [] }
            }),
            &tx,
            &class_tx,
            &reload_tx,
        );
        // Empty files: nothing should arrive.
        assert!(rx.try_recv().is_err());
    }

    #[tokio::test]
    async fn push_frame_unknown_channel_is_ignored() {
        let (tx, mut rx) = unbounded_channel::<FlushEvent>();
        let (class_tx, mut class_rx) = unbounded_channel::<ClassChangedEvent>();
        let (reload_tx, mut reload_rx) = unbounded_channel::<ReloadCheckEvent>();
        handle_push_frame(
            &json!({
                "type": "push",
                "channel": "actors",
                "event": "spawned",
                "data": { "class": "Counter", "pid": "<0.1.0>" }
            }),
            &tx,
            &class_tx,
            &reload_tx,
        );
        assert!(rx.try_recv().is_err());
        assert!(class_rx.try_recv().is_err());
        assert!(reload_rx.try_recv().is_err());
    }

    #[tokio::test]
    async fn class_loaded_push_forwards_to_class_changed_channel() {
        let (tx, mut rx) = unbounded_channel::<FlushEvent>();
        let (class_tx, mut class_rx) = unbounded_channel::<ClassChangedEvent>();
        let (reload_tx, _reload_rx) = unbounded_channel::<ReloadCheckEvent>();
        handle_push_frame(
            &json!({
                "type": "push",
                "channel": "classes",
                "event": "loaded",
                "data": { "class": "Counter" }
            }),
            &tx,
            &class_tx,
            &reload_tx,
        );
        let evt = class_rx.recv().await.expect("class changed");
        assert_eq!(evt.class_name, "Counter");
        // Flush channel must not be touched.
        assert!(rx.try_recv().is_err());
    }

    #[tokio::test]
    async fn class_loaded_push_without_class_name_is_dropped() {
        let (tx, _rx) = unbounded_channel::<FlushEvent>();
        let (class_tx, mut class_rx) = unbounded_channel::<ClassChangedEvent>();
        let (reload_tx, _reload_rx) = unbounded_channel::<ReloadCheckEvent>();
        handle_push_frame(
            &json!({
                "type": "push",
                "channel": "classes",
                "event": "loaded",
                "data": {}
            }),
            &tx,
            &class_tx,
            &reload_tx,
        );
        assert!(class_rx.try_recv().is_err());
    }

    #[tokio::test]
    async fn push_frame_missing_data_is_ignored() {
        let (tx, mut rx) = unbounded_channel::<FlushEvent>();
        let (class_tx, _class_rx) = unbounded_channel::<ClassChangedEvent>();
        let (reload_tx, _reload_rx) = unbounded_channel::<ReloadCheckEvent>();
        handle_push_frame(
            &json!({
                "type": "push",
                "channel": "workspace",
                "event": "flush_completed"
            }),
            &tx,
            &class_tx,
            &reload_tx,
        );
        assert!(rx.try_recv().is_err());
    }

    #[tokio::test]
    async fn reload_check_completed_push_is_forwarded() {
        let (tx, _rx) = unbounded_channel::<FlushEvent>();
        let (class_tx, _class_rx) = unbounded_channel::<ClassChangedEvent>();
        let (reload_tx, mut reload_rx) = unbounded_channel::<ReloadCheckEvent>();
        handle_push_frame(
            &json!({
                "type": "push",
                "channel": "reload_check",
                "event": "completed",
                "data": {
                    "changedClass": "Counter",
                    "changedSelector": "getCount",
                    "classification": "signature_change",
                    "checked": 1,
                    "notChecked": 0,
                    "capNote": null,
                    "checkedOwners": ["Dashboard"],
                    "findings": [{
                        "owner": "Dashboard",
                        "changedClass": "Counter",
                        "selector": "getCount",
                        "classification": "signature_change",
                        "severity": "warning",
                        "category": "Dnu",
                        "message": "String does not understand '+'",
                        "note": null,
                        "sites": [{"method": "refresh", "line": 14}],
                        "start": 0,
                        "end": 5
                    }]
                }
            }),
            &tx,
            &class_tx,
            &reload_tx,
        );
        let evt = reload_rx.recv().await.expect("reload check event");
        assert_eq!(evt.changed_class, "Counter");
        assert_eq!(evt.checked_owners, vec!["Dashboard".to_string()]);
        assert_eq!(evt.findings.len(), 1);
        assert_eq!(evt.findings[0].sites[0].line, 14);
    }

    #[tokio::test]
    async fn reload_check_completed_push_with_malformed_data_is_dropped() {
        let (tx, _rx) = unbounded_channel::<FlushEvent>();
        let (class_tx, _class_rx) = unbounded_channel::<ClassChangedEvent>();
        let (reload_tx, mut reload_rx) = unbounded_channel::<ReloadCheckEvent>();
        handle_push_frame(
            &json!({
                "type": "push",
                "channel": "reload_check",
                "event": "completed",
                "data": { "changedClass": "Counter" }
            }),
            &tx,
            &class_tx,
            &reload_tx,
        );
        assert!(reload_rx.try_recv().is_err());
    }

    /// A dropped receiver is a best-effort signal going nowhere, not a fault:
    /// `handle_push_frame` must log and carry on rather than panic or unwind
    /// the listener task. One case per forwarding channel.
    #[test]
    fn push_frames_tolerate_dropped_receivers() {
        let (flush_tx, flush_rx) = unbounded_channel::<FlushEvent>();
        let (class_tx, class_rx) = unbounded_channel::<ClassChangedEvent>();
        let (reload_tx, reload_rx) = unbounded_channel::<ReloadCheckEvent>();
        drop(flush_rx);
        drop(class_rx);
        drop(reload_rx);

        for frame in [
            json!({
                "type": "push", "channel": "workspace", "event": "flush_completed",
                "data": {"files": ["src/counter.bt"]}
            }),
            json!({
                "type": "push", "channel": "classes", "event": "loaded",
                "data": {"class": "Counter"}
            }),
            json!({
                "type": "push", "channel": "reload_check", "event": "completed",
                "data": {
                    "changedClass": "Counter", "changedSelector": "getCount",
                    "classification": "self_edit", "checked": 0, "notChecked": 0,
                    "capNote": null, "checkedOwners": [], "findings": []
                }
            }),
        ] {
            handle_push_frame(&frame, &flush_tx, &class_tx, &reload_tx);
        }
    }

    // ------------------------------------------------------------------
    // Fake workspace: an in-process WebSocket server speaking just enough of
    // the REPL wire protocol (`docs/repl-protocol.md`) to drive
    // `connect_to` / `perform_auth_handshake` / `writer_task` /
    // `listener_task` end-to-end without a BEAM node. Everything below the
    // handshake is scripted by the test, so error branches that a real
    // workspace never produces on demand (auth failure, a truncated
    // handshake, a malformed reply, a socket that closes mid-request) are
    // reachable deterministically.
    //
    // Fidelity caveat — this is a test double, not a second implementation of
    // a shared rule: `perform_auth_handshake` above no longer re-matches the
    // handshake JSON itself, it builds/recognises every frame through
    // `beamtalk_repl_protocol::handshake` (BT-3330), which is pinned to
    // `beamtalk_ws_handler.erl`'s actual production frame shapes via the
    // shared `ws_auth_handshake_wire_corpus.json` fixture — read on the Rust
    // side by `handshake::tests::matches_shared_wire_corpus` and on the
    // Erlang side by `beamtalk_ws_handler_tests`'s
    // `handshake_pre_auth_frame_matches_shared_wire_corpus_test/0`,
    // `handshake_auth_error_matches_shared_wire_corpus_test/0`, and
    // `handshake_success_matches_shared_wire_corpus/0`. A rename on the
    // Erlang side now fails those Erlang tests directly, rather than leaving
    // every Rust client's tests green while it broke against a live
    // workspace. The scripted frames below still aren't a live BEAM node —
    // the `lsp_parity` e2e suite still never attaches a runtime, so
    // `connect_to`'s actual I/O (timeouts, reconnect, transport errors) has
    // no real-node exercise from the LSP specifically — but the wire
    // *contract* itself is enforced by something executable now, per
    // `docs/development/architecture-principles.md` §6. The CLI's
    // `ProtocolClient` and the parity harness's `ReplDriver` share the same
    // fix; the MCP `ReplClient` additionally gets real-node exercise of this
    // exact handshake code via its own `#[ignore]` integration tests
    // (`just test-mcp`).
    // ------------------------------------------------------------------

    /// How the fake workspace behaves during the pre-`session-started`
    /// handshake. One variant per branch of [`perform_auth_handshake`] and
    /// [`read_text`].
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum Handshake {
        /// The real sequence: `auth-required` → (client auth) → `auth_ok` →
        /// `session-started`.
        Ok,
        /// Same, but a binary frame precedes each text frame — `read_text`
        /// must skip non-text frames rather than treat them as the handshake.
        OkWithBinaryNoise,
        /// Close the socket before sending anything.
        CloseImmediately,
        /// Drop the socket (FIN, no close frame) before sending anything.
        DropImmediately,
        /// First frame is not JSON.
        UnparseableAuthRequired,
        /// First frame is JSON but not `op: auth-required`.
        WrongPreAuthOp,
        /// Reply to the client's auth with `auth_error`.
        AuthError,
        /// Reply to the client's auth with `auth_error` carrying no `message`.
        AuthErrorWithoutMessage,
        /// Reply to the client's auth with an unrecognised `type`.
        UnexpectedAuthResponse,
        /// Auth succeeds but the follow-up frame is not `op: session-started`.
        WrongPostAuthOp,
    }

    /// Frames the fake workspace sends back for one received request.
    type Responder = Box<dyn Fn(&serde_json::Value) -> Vec<Message> + Send + Sync>;

    /// A running fake workspace. Aborts its task on drop, so a test that
    /// returns early never leaks a listener.
    struct FakeWorkspace {
        port: u16,
        /// Every request frame the server received, in arrival order.
        seen: Arc<Mutex<Vec<serde_json::Value>>>,
        /// Fires once the server's post-handshake read loop has exited —
        /// i.e. the client hung up.
        disconnected: Option<oneshot::Receiver<()>>,
        task: tokio::task::JoinHandle<()>,
    }

    impl Drop for FakeWorkspace {
        fn drop(&mut self) {
            self.task.abort();
        }
    }

    fn text(value: &serde_json::Value) -> Message {
        Message::Text(value.to_string().into())
    }

    /// Spawn a fake workspace on an ephemeral loopback port.
    async fn spawn_workspace(handshake: Handshake, responder: Responder) -> FakeWorkspace {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0")
            .await
            .expect("bind loopback");
        let port = listener.local_addr().expect("local addr").port();
        let seen: Arc<Mutex<Vec<serde_json::Value>>> = Arc::new(Mutex::new(Vec::new()));
        let seen_task = Arc::clone(&seen);
        let (done_tx, done_rx) = oneshot::channel();

        let task = tokio::spawn(async move {
            let Ok((stream, _peer)) = listener.accept().await else {
                return;
            };
            let Ok(mut ws) = tokio_tungstenite::accept_async(stream).await else {
                return;
            };

            if handshake == Handshake::DropImmediately {
                drop(ws);
                return;
            }
            if handshake == Handshake::CloseImmediately {
                let _ = ws.close(None).await;
                return;
            }
            if handshake == Handshake::OkWithBinaryNoise {
                let _ = ws.send(Message::Binary(vec![0xF0, 0x9F].into())).await;
            }

            match handshake {
                Handshake::UnparseableAuthRequired => {
                    let _ = ws.send(Message::Text("not json at all".into())).await;
                    return;
                }
                Handshake::WrongPreAuthOp => {
                    let _ = ws.send(text(&json!({"op": "something-else"}))).await;
                    return;
                }
                _ => {
                    let _ = ws.send(text(&json!({"op": "auth-required"}))).await;
                }
            }

            // The client's auth frame.
            let auth = ws.next().await;
            let Some(Ok(Message::Text(auth))) = auth else {
                return;
            };
            let auth: serde_json::Value = serde_json::from_str(&auth).unwrap_or(json!({}));
            seen_task.lock().await.push(auth);

            if handshake == Handshake::OkWithBinaryNoise {
                let _ = ws.send(Message::Binary(vec![0x00].into())).await;
            }
            match handshake {
                Handshake::AuthError => {
                    let _ = ws
                        .send(text(
                            &json!({"type": "auth_error", "message": "invalid cookie"}),
                        ))
                        .await;
                    return;
                }
                Handshake::AuthErrorWithoutMessage => {
                    let _ = ws.send(text(&json!({"type": "auth_error"}))).await;
                    return;
                }
                Handshake::UnexpectedAuthResponse => {
                    let _ = ws.send(text(&json!({"type": "who_are_you"}))).await;
                    return;
                }
                _ => {
                    let _ = ws.send(text(&json!({"type": "auth_ok"}))).await;
                }
            }

            if handshake == Handshake::WrongPostAuthOp {
                let _ = ws.send(text(&json!({"op": "not-session-started"}))).await;
                return;
            }
            let _ = ws.send(text(&json!({"op": "session-started"}))).await;

            while let Some(Ok(msg)) = ws.next().await {
                let Message::Text(body) = msg else { continue };
                let request: serde_json::Value = serde_json::from_str(&body).unwrap_or(json!({}));
                seen_task.lock().await.push(request.clone());
                for frame in responder(&request) {
                    let closing = matches!(frame, Message::Close(_));
                    if ws.send(frame).await.is_err() || closing {
                        break;
                    }
                }
            }
            let _ = done_tx.send(());
        });

        FakeWorkspace {
            port,
            seen,
            disconnected: Some(done_rx),
            task,
        }
    }

    /// A responder that answers every request with `value`, echoing the
    /// request's own correlation id.
    fn reply_with(value: serde_json::Value) -> Responder {
        Box::new(move |request| {
            vec![text(&json!({
                "id": request["id"],
                "status": ["done"],
                "value": value.clone(),
            }))]
        })
    }

    /// A responder that never answers.
    fn reply_nothing() -> Responder {
        Box::new(|_| Vec::new())
    }

    /// Connect a client to `ws`, asserting the handshake succeeded.
    async fn connect_client(ws: &FakeWorkspace, sinks: &Sinks) -> RuntimeClient {
        RuntimeClient::connect_to(
            ws.port,
            "test-cookie",
            sinks.flush_tx.clone(),
            sinks.class_tx.clone(),
            sinks.reload_tx.clone(),
        )
        .await
        .expect("handshake completes")
    }

    /// `Result::expect_err` requires `T: Debug`, and `RuntimeClient` (a handle
    /// over task join handles) does not implement it — unwrap by hand instead.
    fn expect_connect_failure(
        result: Result<RuntimeClient, RuntimeError>,
        ctx: &str,
    ) -> RuntimeError {
        match result {
            Ok(_) => panic!("expected {ctx} to fail, but a client connected"),
            Err(e) => e,
        }
    }

    /// Connect expecting failure, returning the error.
    async fn connect_err(ws: &FakeWorkspace, sinks: &Sinks) -> RuntimeError {
        let result = RuntimeClient::connect_to(
            ws.port,
            "test-cookie",
            sinks.flush_tx.clone(),
            sinks.class_tx.clone(),
            sinks.reload_tx.clone(),
        )
        .await;
        expect_connect_failure(result, "the handshake")
    }

    fn assert_connect_error(err: &RuntimeError, port: u16, expected_fragment: &str) {
        match err {
            RuntimeError::Connect {
                port: got_port,
                reason,
            } => {
                assert_eq!(*got_port, port, "error must name the port it dialled");
                assert!(
                    reason.contains(expected_fragment),
                    "expected reason containing {expected_fragment:?}, got {reason:?}"
                );
            }
            other => panic!("expected RuntimeError::Connect, got {other:?}"),
        }
    }

    // --- handshake ----------------------------------------------------

    #[tokio::test]
    async fn connect_to_completes_handshake_and_tags_the_session_as_lsp() {
        let ws = spawn_workspace(Handshake::Ok, reply_nothing()).await;
        let sinks = Sinks::new();
        let _client = connect_client(&ws, &sinks).await;

        // The auth frame is the only thing the server has seen so far.
        let seen = ws.seen.lock().await;
        assert_eq!(seen.len(), 1, "exactly the auth frame: {seen:?}");
        assert_eq!(seen[0]["type"], "auth");
        assert_eq!(seen[0]["cookie"], "test-cookie");
        // `Workspace sessions` shows the originating surface from this field.
        assert_eq!(seen[0]["client"], "lsp");
        assert!(
            seen[0].get("resume").is_none(),
            "the LSP always opens a fresh session"
        );
    }

    #[tokio::test]
    async fn connect_to_skips_non_text_frames_during_handshake() {
        let ws = spawn_workspace(Handshake::OkWithBinaryNoise, reply_nothing()).await;
        let sinks = Sinks::new();
        let _client = connect_client(&ws, &sinks).await;
    }

    #[tokio::test]
    async fn connect_to_fails_when_nothing_is_listening() {
        // Bind then drop, so the port is known-free for the length of the test.
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0")
            .await
            .expect("bind");
        let port = listener.local_addr().expect("addr").port();
        drop(listener);

        let sinks = Sinks::new();
        let result = RuntimeClient::connect_to(
            port,
            "test-cookie",
            sinks.flush_tx,
            sinks.class_tx,
            sinks.reload_tx,
        )
        .await;
        let err = expect_connect_failure(result, "connecting to a closed port");
        assert_connect_error(&err, port, "websocket connect failed");
    }

    #[tokio::test]
    async fn connect_to_reports_close_during_handshake() {
        let ws = spawn_workspace(Handshake::CloseImmediately, reply_nothing()).await;
        let sinks = Sinks::new();
        let err = connect_err(&ws, &sinks).await;
        assert_connect_error(&err, ws.port, "closed websocket during handshake");
    }

    #[tokio::test]
    async fn connect_to_reports_stream_end_during_handshake() {
        let ws = spawn_workspace(Handshake::DropImmediately, reply_nothing()).await;
        let sinks = Sinks::new();
        let err = connect_err(&ws, &sinks).await;
        // A dropped socket surfaces either as a clean EOF or as a reset,
        // depending on how the kernel delivers the FIN — both are handshake
        // read failures, and neither is a successful connect.
        match &err {
            RuntimeError::Connect { port, reason } => {
                assert_eq!(*port, ws.port);
                assert!(
                    reason.contains("stream ended during handshake")
                        || reason.contains("read failed"),
                    "unexpected reason: {reason}"
                );
            }
            other => panic!("expected RuntimeError::Connect, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn connect_to_reports_unparseable_auth_required() {
        let ws = spawn_workspace(Handshake::UnparseableAuthRequired, reply_nothing()).await;
        let sinks = Sinks::new();
        let err = connect_err(&ws, &sinks).await;
        assert_connect_error(&err, ws.port, "failed to parse auth-required");
    }

    #[tokio::test]
    async fn connect_to_reports_unexpected_pre_auth_message() {
        let ws = spawn_workspace(Handshake::WrongPreAuthOp, reply_nothing()).await;
        let sinks = Sinks::new();
        let err = connect_err(&ws, &sinks).await;
        assert_connect_error(&err, ws.port, "unexpected pre-auth message");
    }

    #[tokio::test]
    async fn connect_to_reports_auth_error_message() {
        let ws = spawn_workspace(Handshake::AuthError, reply_nothing()).await;
        let sinks = Sinks::new();
        let err = connect_err(&ws, &sinks).await;
        assert_connect_error(
            &err,
            ws.port,
            "workspace authentication failed: invalid cookie",
        );
    }

    #[tokio::test]
    async fn connect_to_reports_auth_error_without_message() {
        let ws = spawn_workspace(Handshake::AuthErrorWithoutMessage, reply_nothing()).await;
        let sinks = Sinks::new();
        let err = connect_err(&ws, &sinks).await;
        assert_connect_error(
            &err,
            ws.port,
            "workspace authentication failed: authentication failed",
        );
    }

    #[tokio::test]
    async fn connect_to_reports_unexpected_auth_response() {
        let ws = spawn_workspace(Handshake::UnexpectedAuthResponse, reply_nothing()).await;
        let sinks = Sinks::new();
        let err = connect_err(&ws, &sinks).await;
        assert_connect_error(&err, ws.port, "unexpected auth response");
    }

    #[tokio::test]
    async fn connect_to_reports_unexpected_post_auth_message() {
        let ws = spawn_workspace(Handshake::WrongPostAuthOp, reply_nothing()).await;
        let sinks = Sinks::new();
        let err = connect_err(&ws, &sinks).await;
        assert_connect_error(&err, ws.port, "unexpected post-auth message");
    }

    // --- request/response ops -----------------------------------------

    #[tokio::test]
    async fn evaluate_round_trips_a_value() {
        let ws = spawn_workspace(Handshake::Ok, reply_with(json!("42"))).await;
        let sinks = Sinks::new();
        let client = connect_client(&ws, &sinks).await;

        let response = client.evaluate("40 + 2").await.expect("eval reply");
        assert!(!response.is_error());
        assert_eq!(response.value, Some(json!("42")));

        let seen = ws.seen.lock().await;
        let request = seen.last().expect("eval request");
        assert_eq!(request["op"], "eval");
        assert_eq!(request["code"], "40 + 2");
    }

    #[tokio::test]
    async fn evaluate_surfaces_a_structured_error_reply_as_ok() {
        // `evaluate` does not decode — a `#beamtalk_error{}` reply comes back
        // as a successful transport result the caller inspects with
        // `is_error()`. Only transport failures are `Err`.
        let ws = spawn_workspace(
            Handshake::Ok,
            Box::new(|request| {
                vec![text(&json!({
                    "id": request["id"],
                    "status": ["done", "error"],
                    "error": "Integer does not understand 'nope'",
                }))]
            }),
        )
        .await;
        let sinks = Sinks::new();
        let client = connect_client(&ws, &sinks).await;

        let response = client.evaluate("1 nope").await.expect("transport ok");
        assert!(response.is_error());
        assert_eq!(
            response.error.as_deref(),
            Some("Integer does not understand 'nope'")
        );
    }

    #[tokio::test]
    async fn nav_query_decodes_sites_and_sends_the_selector_argument() {
        let ws = spawn_workspace(
            Handshake::Ok,
            reply_with(json!({
                "sites": [
                    {"class": "Dashboard", "class_side": false, "method": "refresh",
                     "line": 14, "source_file": "/proj/src/dashboard.bt"},
                    {"class": "Counter", "class_side": true, "method": "increment", "line": 3},
                ]
            })),
        )
        .await;
        let sinks = Sinks::new();
        let client = connect_client(&ws, &sinks).await;

        let sites = client
            .nav_query(&NavQuery::SendersOf("increment".into()))
            .await
            .expect("nav-query reply");
        assert_eq!(sites.len(), 2);
        assert_eq!(sites[0].class, "Dashboard");
        assert_eq!(sites[0].line, 14);
        assert_eq!(
            sites[0].source_file.as_deref(),
            Some("/proj/src/dashboard.bt")
        );
        // A source-less row still decodes — consumers treat it as non-navigable.
        assert!(sites[1].source_file.is_none());
        assert!(sites[1].class_side);

        let seen = ws.seen.lock().await;
        let request = seen.last().expect("nav-query request");
        assert_eq!(request["op"], "nav-query");
        assert_eq!(request["kind"], "senders");
        assert_eq!(request["selector"], "increment");
    }

    #[tokio::test]
    async fn nav_query_sends_the_class_argument_for_references() {
        let ws = spawn_workspace(Handshake::Ok, reply_with(json!({"sites": []}))).await;
        let sinks = Sinks::new();
        let client = connect_client(&ws, &sinks).await;

        let sites = client
            .nav_query(&NavQuery::ReferencesTo("Counter".into()))
            .await
            .expect("nav-query reply");
        assert!(sites.is_empty(), "an empty result set is not an error");

        let seen = ws.seen.lock().await;
        let request = seen.last().expect("nav-query request");
        assert_eq!(request["kind"], "references");
        assert_eq!(request["class"], "Counter");
        assert!(request.get("selector").is_none());
    }

    #[tokio::test]
    async fn nav_query_surfaces_a_structured_error_reply() {
        let ws = spawn_workspace(
            Handshake::Ok,
            Box::new(|request| {
                vec![text(&json!({
                    "id": request["id"],
                    "status": ["done", "error"],
                    "error": "unknown kind",
                }))]
            }),
        )
        .await;
        let sinks = Sinks::new();
        let client = connect_client(&ws, &sinks).await;

        let err = client
            .nav_query(&NavQuery::ImplementorsOf("asString".into()))
            .await
            .expect_err("error reply");
        assert_eq!(
            err.to_string(),
            "runtime protocol error: nav-query error: unknown kind"
        );
    }

    #[tokio::test]
    async fn nav_symbols_decodes_classes_and_forwards_the_scope() {
        let ws = spawn_workspace(
            Handshake::Ok,
            reply_with(json!({
                "classes": [{
                    "name": "Counter",
                    "source_file": "/proj/src/counter.bt",
                    "line": 1,
                    "methods": [
                        {"selector": "increment", "class_side": false, "line": 3},
                        {"selector": "new", "class_side": true},
                    ],
                }]
            })),
        )
        .await;
        let sinks = Sinks::new();
        let client = connect_client(&ws, &sinks).await;

        let classes = client.nav_symbols(Some("all")).await.expect("nav-symbols");
        assert_eq!(classes.len(), 1);
        assert_eq!(classes[0].name, "Counter");
        assert_eq!(classes[0].methods.len(), 2);
        // A method with no xref entry decodes with `line: None`.
        assert_eq!(classes[0].methods[1].line, None);

        let seen = ws.seen.lock().await;
        let request = seen.last().expect("nav-symbols request");
        assert_eq!(request["op"], "nav-symbols");
        assert_eq!(request["scope"], "all");
    }

    #[tokio::test]
    async fn nav_symbols_omits_the_scope_field_when_none() {
        let ws = spawn_workspace(Handshake::Ok, reply_with(json!({"classes": []}))).await;
        let sinks = Sinks::new();
        let client = connect_client(&ws, &sinks).await;

        assert!(client.nav_symbols(None).await.expect("reply").is_empty());
        let seen = ws.seen.lock().await;
        assert!(seen.last().expect("request").get("scope").is_none());
    }

    #[tokio::test]
    async fn reload_findings_decodes_the_snapshot() {
        let ws = spawn_workspace(
            Handshake::Ok,
            reply_with(json!({
                "findings": [{
                    "owner": "Dashboard",
                    "changedClass": "Counter",
                    "selector": "getCount",
                    "classification": "removal",
                    "severity": "warning",
                    "category": null,
                    "message": "Counter does not understand 'getCount'",
                    "note": "removed by the reload of Counter",
                    "sites": [{"method": "refresh", "line": 14}],
                    "start": 0,
                    "end": 5,
                }]
            })),
        )
        .await;
        let sinks = Sinks::new();
        let client = connect_client(&ws, &sinks).await;

        let findings = client.reload_findings().await.expect("reload-findings");
        assert_eq!(findings.len(), 1);
        assert_eq!(findings[0].owner, "Dashboard");
        assert_eq!(findings[0].category, None);
        assert_eq!(
            findings[0].sites,
            vec![ReloadSite {
                method: "refresh".to_string(),
                line: 14,
            }]
        );

        let seen = ws.seen.lock().await;
        assert_eq!(seen.last().expect("request")["op"], "reload-findings");
    }

    #[tokio::test]
    async fn reload_findings_reports_a_reply_missing_value() {
        let ws = spawn_workspace(
            Handshake::Ok,
            Box::new(|request| vec![text(&json!({"id": request["id"], "status": ["done"]}))]),
        )
        .await;
        let sinks = Sinks::new();
        let client = connect_client(&ws, &sinks).await;

        let err = client.reload_findings().await.expect_err("no value field");
        assert_eq!(
            err.to_string(),
            "runtime protocol error: reload-findings: reply missing `value`"
        );
    }

    // --- listener behaviour -------------------------------------------

    #[tokio::test]
    async fn listener_skips_noise_frames_and_still_delivers_the_reply() {
        // Three frames the listener must step over on its way to the real
        // reply: unparseable JSON, a reply for an id nobody is waiting on,
        // and a binary frame.
        let ws = spawn_workspace(
            Handshake::Ok,
            Box::new(|request| {
                vec![
                    Message::Text("}{ not json".into()),
                    text(&json!({"id": "no-such-request", "status": ["done"], "value": "stray"})),
                    Message::Binary(vec![1, 2, 3].into()),
                    text(&json!({"id": request["id"], "status": ["done"], "value": "7"})),
                ]
            }),
        )
        .await;
        let sinks = Sinks::new();
        let client = connect_client(&ws, &sinks).await;

        let response = client.evaluate("3 + 4").await.expect("reply arrives");
        assert_eq!(response.value, Some(json!("7")));
    }

    #[tokio::test]
    async fn listener_reports_a_reply_that_is_not_a_repl_response() {
        // `status` must be a list of strings; an integer fails
        // `ReplResponse`'s deserialise after the id lookup already matched.
        let ws = spawn_workspace(
            Handshake::Ok,
            Box::new(|request| vec![text(&json!({"id": request["id"], "status": 5}))]),
        )
        .await;
        let sinks = Sinks::new();
        let client = connect_client(&ws, &sinks).await;

        let err = client.evaluate("1").await.expect_err("malformed reply");
        let msg = err.to_string();
        assert!(
            msg.starts_with("runtime protocol error: failed to parse runtime reply:"),
            "unexpected message: {msg}"
        );
    }

    #[tokio::test]
    async fn listener_forwards_push_frames_arriving_on_the_socket() {
        // The push-frame *decoding* is unit-tested above; this covers the
        // listener's own "is this a push or a reply?" fork on a live socket.
        let ws = spawn_workspace(
            Handshake::Ok,
            Box::new(|request| {
                vec![
                    text(&json!({
                        "type": "push",
                        "channel": "classes",
                        "event": "loaded",
                        "data": {"class": "Counter"},
                    })),
                    text(&json!({"id": request["id"], "status": ["done"], "value": "ok"})),
                ]
            }),
        )
        .await;
        let mut sinks = Sinks::new();
        let client = connect_client(&ws, &sinks).await;

        client.evaluate("Counter new").await.expect("reply");
        let event = sinks.class_rx.recv().await.expect("class_changed event");
        assert_eq!(event.class_name, "Counter");
        assert!(sinks.flush_rx.try_recv().is_err());
        assert!(sinks.reload_rx.try_recv().is_err());
    }

    #[tokio::test]
    async fn pending_request_fails_when_the_workspace_closes_the_socket() {
        let ws = spawn_workspace(Handshake::Ok, Box::new(|_| vec![Message::Close(None)])).await;
        let sinks = Sinks::new();
        let client = connect_client(&ws, &sinks).await;

        let err = client.evaluate("1").await.expect_err("socket closed");
        assert_eq!(
            err.to_string(),
            "runtime protocol error: runtime websocket closed before reply"
        );
    }

    #[tokio::test]
    async fn request_times_out_when_the_workspace_never_replies() {
        // A workspace that accepts the request and then goes quiet — the LSP
        // must give the editor an error rather than hang its command forever.
        // The connect itself needs real I/O, so the clock is paused only
        // afterwards; from there nothing is runnable, tokio auto-advances to
        // `IO_TIMEOUT`, and the test takes milliseconds rather than 30s.
        let ws = spawn_workspace(Handshake::Ok, reply_nothing()).await;
        let sinks = Sinks::new();
        let client = connect_client(&ws, &sinks).await;

        tokio::time::pause();
        let err = client.evaluate("Program sleepForever").await.expect_err(
            "a workspace that never replies must surface a timeout, not hang the command",
        );
        let msg = err.to_string();
        assert!(
            msg.starts_with("runtime protocol error: eval timed out after 30s waiting for reply"),
            "unexpected message: {msg}"
        );
        // The op name is interpolated so a failure names the call that hung.
        assert!(msg.contains("id="), "timeout must name the request: {msg}");
    }

    // --- lifecycle ----------------------------------------------------

    #[tokio::test]
    async fn close_shuts_the_client_down_and_later_calls_fail_fast() {
        let ws = spawn_workspace(Handshake::Ok, reply_with(json!("1"))).await;
        let sinks = Sinks::new();
        let client = connect_client(&ws, &sinks).await;
        client.evaluate("1").await.expect("works before close");

        client.close().await;
        // `close` aborts the writer task, which drops the request receiver.
        // Abort takes effect on the aborted task's next poll, so poll for the
        // observable consequence rather than sleeping a guessed interval. The
        // sleep (rather than a bare `yield_now`) keeps this correct if the
        // test is ever moved to a multi-thread runtime, where yielding does
        // not guarantee another worker gets scheduled.
        for _ in 0..500 {
            if client.inner.sender.is_closed() {
                break;
            }
            tokio::time::sleep(Duration::from_millis(1)).await;
        }
        assert!(client.inner.sender.is_closed(), "writer task did not stop");

        let err = client.evaluate("1").await.expect_err("client is shut down");
        assert_eq!(
            err.to_string(),
            "runtime protocol error: runtime client shut down"
        );

        // Idempotent: a second close is a no-op, not a panic.
        client.close().await;
    }

    #[tokio::test]
    async fn dropping_the_last_clone_tears_down_the_connection() {
        let mut ws = spawn_workspace(Handshake::Ok, reply_with(json!("1"))).await;
        let sinks = Sinks::new();
        let client = connect_client(&ws, &sinks).await;
        let clone = client.clone();
        client.evaluate("1").await.expect("connected");

        drop(client);
        // One handle still alive: the connection must survive.
        clone.evaluate("1").await.expect("clone keeps it alive");

        drop(clone);
        // `RuntimeInner::drop` aborts both tasks, so the server sees EOF.
        let disconnected = ws.disconnected.take().expect("receiver");
        tokio::time::timeout(Duration::from_secs(5), disconnected)
            .await
            .expect("workspace should see the client hang up")
            .expect("server task ran to completion");
    }

    // --- discovery (`connect`) ----------------------------------------

    #[tokio::test]
    async fn connect_reports_workspace_not_found_for_an_unresolvable_project_path() {
        let sinks = Sinks::new();
        let path = std::path::Path::new("/definitely/not/a/real/beamtalk/project");
        let result =
            RuntimeClient::connect(path, sinks.flush_tx, sinks.class_tx, sinks.reload_tx).await;
        let err = expect_connect_failure(result, "connecting to an unresolvable path");
        match err {
            RuntimeError::WorkspaceNotFound {
                project_path,
                reason,
            } => {
                assert_eq!(project_path, path.display().to_string());
                assert!(
                    reason.starts_with("failed to derive workspace id:"),
                    "unexpected reason: {reason}"
                );
            }
            other => panic!("expected WorkspaceNotFound, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn connect_reports_workspace_not_found_when_no_port_file_exists() {
        let project = tempfile::tempdir().expect("tempdir");
        let sinks = Sinks::new();
        let result = RuntimeClient::connect(
            project.path(),
            sinks.flush_tx,
            sinks.class_tx,
            sinks.reload_tx,
        )
        .await;
        let err = expect_connect_failure(result, "connecting with no port file");
        match err {
            RuntimeError::WorkspaceNotFound { reason, .. } => {
                assert!(reason.contains("port file"), "unexpected reason: {reason}");
            }
            other => panic!("expected WorkspaceNotFound, got {other:?}"),
        }
    }

    /// A workspace state directory (`~/.beamtalk/workspaces/<id>/`) for
    /// `project`, removed on drop. The id is a hash of the project path, so a
    /// throwaway temp dir can never collide with a real workspace — the same
    /// approach `beamtalk-mcp`'s `read_port_file` tests take.
    struct WorkspaceFiles {
        dir: std::path::PathBuf,
    }

    impl WorkspaceFiles {
        fn new(project: &Path, port: Option<u16>, cookie: Option<&str>) -> Self {
            let id = beamtalk_workspace::generate_workspace_id(project).expect("workspace id");
            let dir = beamtalk_workspace::workspaces_base_dir()
                .expect("workspaces dir")
                .join(id);
            std::fs::create_dir_all(&dir).expect("create workspace dir");
            if let Some(port) = port {
                std::fs::write(dir.join("port"), format!("{port}\ntest-nonce\n")).expect("port");
            }
            if let Some(cookie) = cookie {
                std::fs::write(dir.join("cookie"), cookie).expect("cookie");
            }
            Self { dir }
        }
    }

    impl Drop for WorkspaceFiles {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.dir);
        }
    }

    #[tokio::test]
    async fn connect_reports_workspace_not_found_when_the_cookie_file_is_missing() {
        let project = tempfile::tempdir().expect("tempdir");
        let _files = WorkspaceFiles::new(project.path(), Some(1), None);

        let sinks = Sinks::new();
        let result = RuntimeClient::connect(
            project.path(),
            sinks.flush_tx,
            sinks.class_tx,
            sinks.reload_tx,
        )
        .await;
        let err = expect_connect_failure(result, "connecting with no cookie file");
        match err {
            RuntimeError::WorkspaceNotFound { reason, .. } => assert!(
                reason.contains("cookie file"),
                "unexpected reason: {reason}"
            ),
            other => panic!("expected WorkspaceNotFound, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn connect_discovers_the_port_and_cookie_and_authenticates() {
        let ws = spawn_workspace(Handshake::Ok, reply_with(json!("discovered"))).await;
        let project = tempfile::tempdir().expect("tempdir");
        let _files =
            WorkspaceFiles::new(project.path(), Some(ws.port), Some("discovered-cookie\n"));

        let sinks = Sinks::new();
        let client = RuntimeClient::connect(
            project.path(),
            sinks.flush_tx.clone(),
            sinks.class_tx.clone(),
            sinks.reload_tx.clone(),
        )
        .await
        .expect("discovery + handshake");

        let response = client.evaluate("1").await.expect("reply");
        assert_eq!(response.value, Some(json!("discovered")));

        // The cookie read from disk is the one presented to the workspace,
        // trimmed of the trailing newline the port/cookie writer leaves.
        let seen = ws.seen.lock().await;
        assert_eq!(seen[0]["cookie"], "discovered-cookie");
    }
}
