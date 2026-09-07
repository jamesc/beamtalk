// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Flush-event listener: consumes `FlushEvent`s from the runtime and turns
//! each into a `workspace/applyEdit` (or `RenameFile`/`CreateFile`/`DeleteFile`)
//! request against the editor.

use super::reload::OpenPathsHandle;
use super::{DocumentMoved, DocumentMovedParams};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::runtime::{FlushEvent, FlushFileKind, FlushedFile};

use camino::Utf8PathBuf;
use tower_lsp::Client;
use tower_lsp::lsp_types::{
    CreateFile, CreateFileOptions, DeleteFile, DeleteFileOptions, DocumentChangeOperation,
    DocumentChanges, OneOf, OptionalVersionedTextDocumentIdentifier, Position, Range, ResourceOp,
    TextDocumentEdit, TextEdit, Url, WorkspaceEdit,
};

/// ADR 0082 Phase 3; ADR 0113 Phase 4a and LSP follow-up
///; ADR 0114 LSP follow-up: consume `FlushEvent`s from
/// the runtime listener and emit `workspace/applyEdit` per flushed file.
///
/// For each file in the event, the listener:
/// 1. Resolves the runtime-reported path against the LSP workspace roots to
///    an absolute filesystem path ([`resolve_flushed_path`]), which also
///    reports whether the leaf still exists on disk — the fallback signal
///    used only when the wire carried no per-file `kind` (see below).
/// 2. Dispatches on the wire's per-file `kind` directly when
///    present, before any filesystem probing:
///    - `rename-class` **with** an `oldFile` companion — the one
///      file among a `'rename-class'` flush's touched files that IS the
///      moved declaration — routes to [`apply_rename_class_move`], never
///      through [`classify_flush_action`].
///    - `rename-class` **without** `oldFile` (an ordinary same-batch
///      reference rewrite in a file that did not itself move) and
///      `rename-method` (a definition or confirmed-sender site)
///      each route to their own branch below, likewise bypassing
///      [`classify_flush_action`].
///    - Everything else goes through [`classify_flush_action`]: `new-class`
///      -> `CreateFile`, `remove-class` -> `DeleteFile`, anything else ->
///      an ordinary patch. A producer that sends no `kind` for a path falls
///      back to the original existence heuristic (gone -> `DeleteFile`,
///      still there -> patch), so
///      `CreateFile` is only ever reachable via an explicit wire `kind` —
///      the existence check alone can never tell "freshly created" from
///      "patched in place" (both leave the leaf present).
/// 3. **`DeleteFile`**: emits a [`DocumentChangeOperation::Op`] `DeleteFile`
///    resource operation ([`delete_file_edit`]) — unconditionally, not gated
///    on the open-paths check below, since a deletion is project-wide state
///    (an open tab that must close, stale diagnostics) rather than something
///    only an open buffer cares about.
/// 4. **`CreateFile`**: reads the freshly-written file (it already exists on
///    disk by the time this fires — Phase B already committed) and emits a
///    `CreateFile` resource operation paired with a `TextDocumentEdit`
///    carrying the full content ([`create_file_edit`]) — also unconditional,
///    since a brand-new file was by definition never open before this flush.
/// 5. **Rename-class move**: sends the custom
///    `beamtalk-lsp/documentMoved` notification ([`DocumentMoved`]) carrying
///    `{old_uri, new_uri}` — unconditional, mirroring `CreateFile`/
///    `DeleteFile`: a file move is project-wide state, not something only an
///    open buffer cares about. No `workspace/applyEdit` `RenameFile` op is
///    sent any more; see [`DocumentMoved`]'s doc for why.
/// 6. **Rename-method site**: checks the *live* open-paths handle
///    exactly like an ordinary patch (step 7) — every file this reaches was
///    an explicitly CONFIRMED site the caller already approved via
///    `confirmDestructive` (never a `candidate_sites` entry, which is never
///    staged/written and so never reaches this listener at all), but
///    "was this open" still gates whether refreshing the *editor buffer* is
///    worth doing — the on-disk bytes are already correct either way. Emits
///    a `TextDocumentEdit` via `documentChanges` ([`rename_method_site_edit`])
///    rather than the plain-`changes`-map shape ordinary patches use,
///    matching the ADR's "`TextDocumentEdit` per confirmed site" wording.
/// 7. **Patch** (an ordinary content edit, unchanged since ADR 0082 Phase 3):
///    checks the *live* open-paths handle to see whether the path is
///    currently open in the editor. Files that aren't open are skipped —
///    `VSCode` reads them fresh on next `did_open`. The check happens per
///    event so files opened after the listener started are still picked up.
///    Reads the new on-disk content and issues `apply_edit` with a single
///    `TextEdit` covering the whole document ([`change_file_edit`]), so the
///    open buffer realigns with the post-flush bytes. `VSCode`'s conflict UX
///    handles unsaved local edits per the LSP spec.
pub(in crate::server) async fn flush_event_listener(
    client: Client,
    workspace_roots: Vec<PathBuf>,
    open_paths: OpenPathsHandle,
    mut flush_rx: tokio::sync::mpsc::UnboundedReceiver<FlushEvent>,
) {
    while let Some(event) = flush_rx.recv().await {
        for FlushedFile {
            path: raw_path,
            kind,
            old_path,
        } in event.files
        {
            let resolved = resolve_flushed_path(&raw_path, &workspace_roots);
            let Some((abs_path, existed)) = resolved else {
                tracing::debug!(
                    raw_path,
                    "flush_event_listener: could not resolve runtime path against workspace roots"
                );
                continue;
            };
            let Ok(uri) = Url::from_file_path(&abs_path) else {
                tracing::debug!(
                    ?abs_path,
                    "flush_event_listener: could not build file:// URI"
                );
                continue;
            };

            match (kind, old_path.as_deref()) {
                (Some(FlushFileKind::RenameClass), Some(old_raw)) => {
                    apply_rename_class_move(&client, &workspace_roots, uri, old_raw).await;
                }
                (Some(FlushFileKind::RenameMethod), _) => {
                    apply_rename_method_site(&client, &abs_path, &raw_path, uri, &open_paths).await;
                }
                _ => match classify_flush_action(kind, existed) {
                    FlushAction::Delete => apply_delete_file(&client, uri).await,
                    FlushAction::Create => apply_create_file(&client, &abs_path, uri).await,
                    FlushAction::Patch => {
                        apply_patch_file(&client, &abs_path, &raw_path, uri, &open_paths).await;
                    }
                },
            }
        }
    }
}
/// Send `edit` via `client.apply_edit` and log the outcome uniformly — shared
/// by the three `flush_event_listener` branches (`Delete`/`Create`/`Patch`,
/// ADR 0113 LSP follow-up) so the three near-identical response-match
/// arms exist in exactly one place.
pub(in crate::server) async fn apply_flush_edit(
    client: &Client,
    uri: &Url,
    edit: WorkspaceEdit,
    op_name: &str,
) {
    match client.apply_edit(edit).await {
        Ok(resp) if resp.applied => {
            tracing::debug!(%uri, op_name, "flush_event_listener: applied");
        }
        Ok(resp) => {
            tracing::info!(
                %uri,
                op_name,
                failure_reason = ?resp.failure_reason,
                "flush_event_listener: client declined applyEdit"
            );
        }
        Err(e) => {
            tracing::warn!(%uri, op_name, error = %e, "flush_event_listener: applyEdit failed");
        }
    }
}
/// `FlushAction::Delete`: unconditional — a deletion is
/// project-wide state, not gated on the open-paths check the patch branch
/// uses.
pub(in crate::server) async fn apply_delete_file(client: &Client, uri: Url) {
    let edit = delete_file_edit(uri.clone());
    apply_flush_edit(client, &uri, edit, "DeleteFile").await;
}
/// `FlushAction::Create` (ADR 0113 LSP follow-up): also
/// unconditional — a brand-new file was by definition never open before
/// this flush. The file already exists on disk (Phase B already committed
/// by the time this event fires) so the read is expected to succeed.
pub(in crate::server) async fn apply_create_file(client: &Client, abs_path: &Path, uri: Url) {
    let Ok(content) = tokio::fs::read_to_string(abs_path).await else {
        tracing::warn!(
            %uri,
            "flush_event_listener: failed to read newly-created file from disk"
        );
        return;
    };
    let edit = create_file_edit(uri.clone(), content);
    apply_flush_edit(client, &uri, edit, "CreateFile").await;
}
/// Resolves a `'rename-class'` flush's `oldFile` companion (`old_raw`)
/// against the workspace roots via [`resolve_flushed_path`] — tolerating its
/// already-deleted state on disk (Phase B already unlinked it by the time
/// the flush event fires), the same way that function already tolerates a
/// `DeleteFile` target being gone — into the `old_uri` [`apply_rename_class_move`]
/// sends in its [`DocumentMoved`] notification.
///
/// Split out of `apply_rename_class_move` purely so this resolution step is
/// unit-testable on its own: it needs no `Client`, whereas exercising
/// `apply_rename_class_move` itself through a real `LspService`/socket pair
/// would additionally require driving the service through a full LSP
/// `initialize` handshake first — `Client::send_notification` (unlike
/// `show_message`/`log_message`, which use its `_unchecked` sibling) only
/// actually sends once `ServerState` has reached `Initialized`, and silently
/// no-ops (never touching the socket) otherwise, so a socket-side test
/// without that handshake would hang waiting for a message that never
/// arrives.
pub(in crate::server) fn resolve_rename_class_old_uri(
    workspace_roots: &[PathBuf],
    old_raw: &str,
) -> Option<Url> {
    let Some((old_abs, _existed)) = resolve_flushed_path(old_raw, workspace_roots) else {
        tracing::debug!(
            old_raw,
            "flush_event_listener: could not resolve rename-class old path against workspace roots"
        );
        return None;
    };
    let Ok(old_uri) = Url::from_file_path(&old_abs) else {
        tracing::debug!(
            ?old_abs,
            "flush_event_listener: could not build file:// URI for rename-class old path"
        );
        return None;
    };
    Some(old_uri)
}
/// `FlushFileKind::RenameClass` with `oldFile` present (ADR 0114 LSP
/// follow-up): the one file among a
/// `'rename-class'` flush's touched files that IS the moved declaration.
/// Unconditional, like `Create`/`Delete`: a file move is project-wide state,
/// not something only an open buffer cares about.
///
/// Sends the custom [`DocumentMoved`] notification rather than a
/// `workspace/applyEdit` `RenameFile` op (the original approach, which
/// this replaces): `old_uri` is *always* already gone from disk by the time
/// this runs, which is exactly the state VS Code's own `RenameOperation`
/// treats as "already done" and silently skips — no error, but no
/// editor-state retargeting either, so an open tab at `old_uri` never
/// actually followed the rename in VS Code. No
/// file content needs reading here any more, since the notification carries
/// no content — the receiving client reopens `new_uri` itself and reads its
/// already-correct on-disk bytes fresh.
pub(in crate::server) async fn apply_rename_class_move(
    client: &Client,
    workspace_roots: &[PathBuf],
    new_uri: Url,
    old_raw: &str,
) {
    let Some(old_uri) = resolve_rename_class_old_uri(workspace_roots, old_raw) else {
        return;
    };
    client
        .send_notification::<DocumentMoved>(DocumentMovedParams {
            old_uri,
            new_uri: new_uri.clone(),
        })
        .await;
    tracing::debug!(
        %new_uri,
        "flush_event_listener: sent beamtalk-lsp/documentMoved notification"
    );
}
/// `FlushFileKind::RenameMethod` (ADR 0114 LSP follow-up): the
/// definition site or a confirmed sender site of a `'rename-method'` flush
/// (never a `candidate_sites` entry — those are never staged/written, so
/// they never reach this listener at all). Gated on the file being open,
/// exactly like [`apply_patch_file`] — the on-disk bytes are already correct
/// either way (Phase B already committed), so this check is purely about
/// whether an open editor buffer is worth refreshing, same as an ordinary
/// patch. Emits a `TextDocumentEdit` via `documentChanges`
/// ([`rename_method_site_edit`]) rather than the plain-`changes`-map shape
/// [`change_file_edit`] uses, matching the ADR's "`TextDocumentEdit` per
/// confirmed site" wording.
pub(in crate::server) async fn apply_rename_method_site(
    client: &Client,
    abs_path: &Path,
    raw_path: &str,
    uri: Url,
    open_paths: &OpenPathsHandle,
) {
    let Ok(utf8_path) = Utf8PathBuf::try_from(abs_path.to_path_buf()) else {
        tracing::debug!(raw_path, "flush_event_listener: resolved path is not UTF-8");
        return;
    };
    if !open_paths.contains(&utf8_path) {
        tracing::debug!(%utf8_path, "flush_event_listener: skipping closed rename-method site");
        return;
    }
    let Ok(content) = tokio::fs::read_to_string(abs_path).await else {
        tracing::warn!(
            %utf8_path,
            "flush_event_listener: failed to read rename-method site from disk"
        );
        return;
    };
    let edit = rename_method_site_edit(uri.clone(), content);
    apply_flush_edit(client, &uri, edit, "RenameMethodSite").await;
}
/// `FlushAction::Patch` (an ordinary content edit, unchanged since ADR 0082
/// Phase 3): gated on the file being open — `VSCode` reads a closed file fresh
/// on next `did_open`.
pub(in crate::server) async fn apply_patch_file(
    client: &Client,
    abs_path: &Path,
    raw_path: &str,
    uri: Url,
    open_paths: &OpenPathsHandle,
) {
    let Ok(utf8_path) = Utf8PathBuf::try_from(abs_path.to_path_buf()) else {
        tracing::debug!(raw_path, "flush_event_listener: resolved path is not UTF-8");
        return;
    };
    if !open_paths.contains(&utf8_path) {
        tracing::debug!(%utf8_path, "flush_event_listener: skipping closed file");
        return;
    }
    let Ok(content) = tokio::fs::read_to_string(abs_path).await else {
        tracing::warn!(
            %utf8_path,
            "flush_event_listener: failed to read flushed file from disk"
        );
        return;
    };
    let edit = change_file_edit(uri.clone(), content);
    apply_flush_edit(client, &uri, edit, "Change").await;
}
/// The `workspace/applyEdit` shape to emit for one flushed file (ADR 0113
/// LSP follow-up).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::server) enum FlushAction {
    /// `CreateFile` + a `TextDocumentEdit` carrying the new content.
    Create,
    /// `DeleteFile`.
    Delete,
    /// A whole-document `TextEdit`, gated on the file being open.
    Patch,
}
/// Decide the [`FlushAction`] for one flushed file (ADR 0113 LSP
/// follow-up). When the wire reported a `kind`, it drives the decision
/// directly — no filesystem probing needed. When `kind` is `None` (a
/// producer that omits this one path), falls back to the original existence
/// heuristic: gone -> `Delete`, still there -> `Patch`. That fallback can
/// never produce `Create` — a `new-class` file with no `kind` degrades to
/// `Patch` (its exact previous behaviour), since post-flush existence alone
/// cannot distinguish "freshly created" from "patched in place".
///
/// `RenameClass`/`RenameMethod` (ADR 0114 LSP follow-up) are
/// dispatched to their own dedicated branches in `flush_event_listener`
/// *before* this function is ever called for them — `RenameClass` only
/// reaches here when the wire carried no `oldFile` (an ordinary same-batch
/// reference rewrite, not the moved file itself), and `RenameMethod` never
/// reaches here at all. Both bucket to `Patch` here defensively, matching
/// what the fallback would have done for an unrecognised kind.
pub(in crate::server) fn classify_flush_action(
    kind: Option<FlushFileKind>,
    existed: bool,
) -> FlushAction {
    match kind {
        Some(FlushFileKind::NewClass) => FlushAction::Create,
        Some(FlushFileKind::Patch | FlushFileKind::RenameClass | FlushFileKind::RenameMethod) => {
            FlushAction::Patch
        }
        None if existed => FlushAction::Patch,
        Some(FlushFileKind::RemoveClass) | None => FlushAction::Delete,
    }
}
/// Build a single `TextEdit` that replaces an entire document's content —
/// the `changes`/`documentChanges` payload shape every flush-driven edit
/// builder below sends (`change_file_edit`, `create_file_edit`,
/// `rename_method_site_edit`): the flush already
/// spliced the on-disk bytes server-side (no incremental diff is computed),
/// so the client is always handed the whole new content rather than a
/// localized range. `u32::MAX`/`u32::MAX` is the LSP convention for "end of
/// file" — any line longer than this is unrealistic for source code and
/// clients clamp to actual EOF.
pub(in crate::server) fn whole_document_text_edit(content: String) -> TextEdit {
    TextEdit {
        range: Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: u32::MAX,
                character: u32::MAX,
            },
        },
        new_text: content,
    }
}
/// Build the `workspace/applyEdit` payload for a file that still exists on
/// disk after the flush (patch / `new-class` / `remove-method` — Tier 1,
/// unchanged from the behaviour that shipped in ADR 0082 Phase 3): a single
/// `TextEdit` covering the whole document with the new on-disk content.
pub(in crate::server) fn change_file_edit(uri: Url, content: String) -> WorkspaceEdit {
    WorkspaceEdit {
        changes: Some({
            let mut changes = HashMap::new();
            changes.insert(uri, vec![whole_document_text_edit(content)]);
            changes
        }),
        ..Default::default()
    }
}
/// Build the `workspace/applyEdit` payload for a file the flush already
/// deleted from disk (ADR 0113 Phase 4a: a Tier 2 destructive flush
/// `remove-class` entry) — a typed `DeleteFile` resource operation via
/// `documentChanges`, not a text edit, since there is no content left to
/// send. `ignoreIfNotExists: true` is defensive: some further time has
/// passed between the existence check that classified this as a deletion
/// and the client actually receiving this request, so the client's own view
/// might already agree the file is gone.
pub(in crate::server) fn delete_file_edit(uri: Url) -> WorkspaceEdit {
    WorkspaceEdit {
        document_changes: Some(DocumentChanges::Operations(vec![
            DocumentChangeOperation::Op(ResourceOp::Delete(DeleteFile {
                uri,
                options: Some(DeleteFileOptions {
                    recursive: Some(false),
                    ignore_if_not_exists: Some(true),
                    annotation_id: None,
                }),
            })),
        ])),
        ..Default::default()
    }
}
/// Build the `workspace/applyEdit` payload for a file the flush just wrote
/// to disk for the first time (ADR 0113 LSP follow-up: a
/// `new-class` entry) — a typed `CreateFile` resource operation via
/// `documentChanges`, paired with a `TextDocumentEdit` carrying the new
/// file's full content (ADR 0113's LSP section: "`Workspace newClass:at:`
/// flush should also switch from the generic `Change` shape to
/// `CreateFile`"). Both operations live in the same `documentChanges` array
/// (the LSP spec does not allow mixing the plain `changes` map with
/// `documentChanges` in one edit) so a client applies the create and the
/// content atomically as one workspace edit.
///
/// The physical file already exists on disk by the time this fires
/// (`beamtalk_workspace_flush:complete_flush/5` announces after Phase B
/// commits) — `ignoreIfExists: true` is defensive for exactly that reason:
/// the client's own `CreateFile` step must not fail just because the flush
/// beat it to the write, and the paired `TextDocumentEdit` still supplies
/// the correct content for the client's in-memory buffer either way.
pub(in crate::server) fn create_file_edit(uri: Url, content: String) -> WorkspaceEdit {
    WorkspaceEdit {
        document_changes: Some(DocumentChanges::Operations(vec![
            DocumentChangeOperation::Op(ResourceOp::Create(CreateFile {
                uri: uri.clone(),
                options: Some(CreateFileOptions {
                    overwrite: None,
                    ignore_if_exists: Some(true),
                }),
                annotation_id: None,
            })),
            DocumentChangeOperation::Edit(TextDocumentEdit {
                text_document: OptionalVersionedTextDocumentIdentifier { uri, version: None },
                edits: vec![OneOf::Left(whole_document_text_edit(content))],
            }),
        ])),
        ..Default::default()
    }
}
/// Build the `workspace/applyEdit` payload for one confirmed site of a
/// `'rename-method'` flush (ADR 0114 LSP follow-up:
/// `renameSelector:to:`) — a single `TextDocumentEdit` via `documentChanges`
/// carrying the file's new content (a whole-document replacement, same
/// convention as [`change_file_edit`]/[`create_file_edit`]'s content edits).
/// Deliberately the typed `documentChanges`/`TextDocumentEdit` shape rather
/// than [`change_file_edit`]'s plain `changes` map — the ADR's LSP section
/// calls for "a `TextDocumentEdit` per confirmed site", not a generic patch,
/// even though both end up replacing the whole document the same way.
pub(in crate::server) fn rename_method_site_edit(uri: Url, content: String) -> WorkspaceEdit {
    WorkspaceEdit {
        document_changes: Some(DocumentChanges::Operations(vec![
            DocumentChangeOperation::Edit(TextDocumentEdit {
                text_document: OptionalVersionedTextDocumentIdentifier { uri, version: None },
                edits: vec![OneOf::Left(whole_document_text_edit(content))],
            }),
        ])),
        ..Default::default()
    }
}
/// Resolve a path reported by the runtime against the LSP workspace roots,
/// tolerating an already-deleted target (ADR 0113 Phase 4a): a
/// Tier 2 destructive flush (`remove-class`) has already unlinked its file
/// by the time the `flush_completed` push fires
/// (`beamtalk_workspace_flush:complete_flush/5` announces after Phase B
/// commits), so `canonicalize()` — which requires the leaf to exist — can't
/// validate that case the way it does for an ordinary write.
///
/// The runtime stores `ChangeEntry.sourceFile` as whatever was passed at
/// `compile:source:` hook time — typically a workspace-relative path
/// (`"src/counter.bt"`) when the workspace was started in the project root.
/// We try, in order:
///
/// 1. Absolute → use as-is.
/// 2. For each workspace root, join.
///
/// For each candidate: canonicalise if the leaf exists (`existed = true`);
/// otherwise fall back to the literal candidate path if its *parent*
/// directory is real (`existed = false`) — enough to build a `file://` URI
/// for a `DeleteFile` resource operation without being able to canonicalise
/// a path that no longer exists. Canonicalisation matters for the
/// `existed = true` case because the LSP stores open documents under
/// canonical paths (`uri_to_path` runs `canonicalize`); a non-canonical
/// lookup would always miss the open-paths check.
///
/// Returns `None` if no root (nor the absolute-path case) finds even a real
/// parent directory for `raw`.
///
/// **Multi-root ambiguity for a deleted leaf:** with `existed = true` the
/// per-root loop picks the (necessarily unique) root that actually contains
/// the file. With `existed = false` there is no such tiebreaker — the first
/// root whose parent directory is real wins, even if a sibling root's same
/// relative path would *also* have a real parent. Worst case this reports
/// the deletion against the wrong root's copy of the path; the resulting
/// `DeleteFile` targets a path the editor never had open, which is a no-op
/// there, not a wrong deletion (the flush already deleted the *real* file
/// before this event fired — this function only decides which URI to name
/// in the notification). Relevant only for multi-root workspaces with a
/// same-relative-path collision across roots.
pub(crate) fn resolve_flushed_path(raw: &str, roots: &[PathBuf]) -> Option<(PathBuf, bool)> {
    let candidate = PathBuf::from(raw);
    if candidate.is_absolute() {
        return resolve_candidate(candidate);
    }
    for root in roots {
        if let Some(resolved) = resolve_candidate(root.join(&candidate)) {
            return Some(resolved);
        }
    }
    None
}
/// Resolve one absolute candidate path — see [`resolve_flushed_path`] for
/// the existed/deleted classification this implements.
pub(in crate::server) fn resolve_candidate(candidate: PathBuf) -> Option<(PathBuf, bool)> {
    match candidate.canonicalize() {
        Ok(canon) => return Some((canon, true)),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
        // A non-NotFound failure (permission denied, symlink cycle, transient
        // I/O error, non-directory path component) does not mean the file was
        // deleted — treating it as "deleted" would fire a DeleteFile applyEdit
        // for a file that's still there, possibly discarding a client's open,
        // unsaved buffer for it.
        Err(_) => return None,
    }
    let parent = candidate.parent()?;
    if parent.exists() {
        Some((candidate, false))
    } else {
        None
    }
}
