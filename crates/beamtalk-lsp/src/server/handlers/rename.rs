// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `textDocument/prepareRename` and `textDocument/rename` (BT-5007).
//!
//! The language service plans the rename ([`SimpleLanguageService::plan_rename`]);
//! this module turns the plan into a `WorkspaceEdit`. It follows ADR 0114's
//! two tiers: confirmed sites are plain edits, candidate sites are edits
//! under a `ChangeAnnotation` with `needsConfirmation: true` (so an editor
//! with a refactor preview shows them unticked). A client that does not
//! declare `changeAnnotationSupport` gets the confirmed edits only, and the
//! candidates are listed in a `window/showMessage`.

use super::super::Backend;
use super::super::convert::{path_to_uri, span_to_range, to_bt_position};
use crate::server::lock::LockRecover;
use beamtalk_language_service::{LanguageService, RenameEdit, RenamePlan};
use camino::Utf8PathBuf;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs;
use tower_lsp::jsonrpc::{Error, ErrorCode, Result};
use tower_lsp::lsp_types::{
    AnnotatedTextEdit, ChangeAnnotation, DocumentChangeOperation, DocumentChanges, MessageType,
    OneOf, OptionalVersionedTextDocumentIdentifier, PrepareRenameResponse, RenameFile,
    RenameParams, ResourceOp, TextDocumentEdit, TextDocumentPositionParams, TextEdit, Url,
    WorkspaceEdit,
};

/// `RequestFailed` (LSP 3.17): the request was valid but could not be done.
const REQUEST_FAILED: i64 = -32803;
/// Refusal for a document whose URI cannot hold a rename (BT-5188: says why).
const NOT_RENAMEABLE_DOCUMENT: &str = "Rename is only available in Beamtalk files and untitled \
     Beamtalk buffers, not in this document.";
/// Refusal for a document the server has not indexed (yet).
const NOT_INDEXED_DOCUMENT: &str = "This document is not indexed yet. Open it in the editor (or \
     wait for the project to finish loading) and run the rename again.";
/// Id of the annotation that marks candidate sites.
const CANDIDATE_ANNOTATION: &str = "beamtalk.rename.candidates";
/// How many candidate sites a fallback message lists before summarising.
const MESSAGE_SITE_LIMIT: usize = 20;

/// What the connected client told us at `initialize` about workspace edits.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(in crate::server) struct RenameClientCaps {
    /// `workspace.workspaceEdit.documentChanges`.
    pub document_changes: bool,
    /// `workspace.workspaceEdit.changeAnnotationSupport` is declared.
    pub change_annotations: bool,
    /// `workspace.workspaceEdit.resourceOperations` includes `rename`.
    pub rename_file: bool,
}

impl RenameClientCaps {
    pub(in crate::server) fn from_params(params: &tower_lsp::lsp_types::InitializeParams) -> Self {
        let edit = params
            .capabilities
            .workspace
            .as_ref()
            .and_then(|w| w.workspace_edit.as_ref());
        Self {
            document_changes: edit.is_some_and(|e| e.document_changes == Some(true)),
            change_annotations: edit.is_some_and(|e| e.change_annotation_support.is_some()),
            rename_file: edit.is_some_and(|e| {
                e.resource_operations.as_ref().is_some_and(|ops| {
                    ops.contains(&tower_lsp::lsp_types::ResourceOperationKind::Rename)
                })
            }),
        }
    }
}

/// A built rename: the edit, plus a message for whatever the edit left out.
#[derive(Debug)]
pub(in crate::server) struct RenameOutcome {
    pub edit: WorkspaceEdit,
    pub message: Option<String>,
}

/// Every file a plan edits (confirmed and candidate sites).
fn plan_files(plan: &RenamePlan) -> HashSet<Utf8PathBuf> {
    plan.confirmed
        .iter()
        .chain(plan.candidates.iter().flatten())
        .map(|edit| edit.file.clone())
        .collect()
}

/// The plan's files that are not open in the editor and whose on-disk text
/// no longer matches the indexed text (`disk` is `None` when unreadable, e.g.
/// deleted). Open files are exempt: the editor's buffer is the truth and the
/// client checks the document version.
pub(in crate::server) fn stale_closed_files(
    plan: &RenamePlan,
    open: &HashSet<Utf8PathBuf>,
    indexed: &HashMap<Utf8PathBuf, String>,
    read_disk: &dyn Fn(&Utf8PathBuf) -> Option<String>,
) -> Vec<(Utf8PathBuf, Option<String>)> {
    let mut files: Vec<_> = plan_files(plan).into_iter().collect();
    files.sort();
    files
        .into_iter()
        .filter(|f| !open.contains(f))
        .filter_map(|f| {
            let disk = read_disk(&f);
            (disk.as_deref() != indexed.get(&f).map(String::as_str)).then_some((f, disk))
        })
        .collect()
}

fn refusal(message: impl Into<String>) -> Error {
    Error {
        code: ErrorCode::ServerError(REQUEST_FAILED),
        message: message.into().into(),
        data: None,
    }
}

/// Builds the `WorkspaceEdit` for `plan`, honouring what the client supports.
///
/// `source_of` returns a file's current text (for byte span → LSP range);
/// `versions` supplies the document version for open files.
pub(in crate::server) fn build_rename_edit(
    plan: &RenamePlan,
    caps: RenameClientCaps,
    source_of: &dyn Fn(&Utf8PathBuf) -> Option<String>,
    versions: &HashMap<Utf8PathBuf, i32>,
) -> std::result::Result<RenameOutcome, String> {
    let annotate = caps.document_changes && caps.change_annotations;
    let mut notes: Vec<String> = Vec::new();

    // file → (confirmed edits, candidate edits), in file order.
    let mut by_file: BTreeMap<&Utf8PathBuf, (Vec<&RenameEdit>, Vec<&RenameEdit>)> = BTreeMap::new();
    for edit in &plan.confirmed {
        by_file.entry(&edit.file).or_default().0.push(edit);
    }
    if annotate {
        for edit in plan.candidates.iter().flatten() {
            by_file.entry(&edit.file).or_default().1.push(edit);
        }
    } else if !plan.candidates.is_empty() {
        notes.push(candidate_message(plan, source_of));
    }

    // BT-5132: an edit that cannot be converted fails the whole rename. A
    // dropped file or edit would apply as a partial rename that reports
    // success and leaves dangling references.
    let to_range = |edit: &RenameEdit| -> std::result::Result<tower_lsp::lsp_types::Range, String> {
        let src =
            source_of(&edit.file).ok_or_else(|| format!("{} is no longer indexed", edit.file))?;
        if edit.span.end() as usize > src.len()
            || !src.is_char_boundary(edit.span.start() as usize)
            || !src.is_char_boundary(edit.span.end() as usize)
        {
            return Err(format!("an edit in {} is outside the file", edit.file));
        }
        Ok(span_to_range(edit.span, &src))
    };

    let mut text_edits: Vec<FileEdits> = Vec::new();
    for (file, (confirmed, candidates)) in by_file {
        let uri = path_to_uri(file)
            .ok_or_else(|| format!("{file} cannot be represented as a document URI"))?;
        let mut edits = Vec::new();
        for edit in confirmed {
            edits.push(OneOf::Left(TextEdit {
                range: to_range(edit)?,
                new_text: edit.new_text.clone(),
            }));
        }
        for edit in candidates {
            edits.push(OneOf::Right(AnnotatedTextEdit {
                text_edit: TextEdit {
                    range: to_range(edit)?,
                    new_text: edit.new_text.clone(),
                },
                annotation_id: CANDIDATE_ANNOTATION.to_string(),
            }));
        }
        text_edits.push(FileEdits {
            uri,
            version: versions.get(file).copied(),
            edits,
        });
    }

    let file_move = match plan.file_move.as_ref() {
        Some(mv) if caps.document_changes && caps.rename_file => {
            let from = path_to_uri(&mv.from)
                .ok_or_else(|| format!("{} cannot be represented as a document URI", mv.from))?;
            let to = path_to_uri(&mv.to)
                .ok_or_else(|| format!("{} cannot be represented as a document URI", mv.to))?;
            Some((from, to))
        }
        Some(mv) => {
            notes.push(format!(
                "Rename the file {} to {} to match the new class name.",
                mv.from, mv.to
            ));
            None
        }
        None => None,
    };

    let edit = if caps.document_changes {
        document_changes_edit(
            text_edits,
            file_move,
            annotate && !plan.candidates.is_empty(),
        )
    } else {
        changes_map_edit(text_edits)
    };

    Ok(RenameOutcome {
        edit,
        message: (!notes.is_empty()).then(|| notes.join("\n")),
    })
}

/// The edits for one file, ready for a `TextDocumentEdit`.
struct FileEdits {
    uri: Url,
    version: Option<i32>,
    edits: Vec<OneOf<TextEdit, AnnotatedTextEdit>>,
}

/// `documentChanges` shape: typed text edits, then the file rename (the
/// edits are addressed to the old URI, so the move goes last).
fn document_changes_edit(
    files: Vec<FileEdits>,
    file_move: Option<(Url, Url)>,
    with_annotation: bool,
) -> WorkspaceEdit {
    let mut ops: Vec<DocumentChangeOperation> = files
        .into_iter()
        .map(|file| {
            DocumentChangeOperation::Edit(TextDocumentEdit {
                text_document: OptionalVersionedTextDocumentIdentifier {
                    uri: file.uri,
                    version: file.version,
                },
                edits: file.edits,
            })
        })
        .collect();
    if let Some((old_uri, new_uri)) = file_move {
        ops.push(DocumentChangeOperation::Op(ResourceOp::Rename(
            RenameFile {
                old_uri,
                new_uri,
                options: None,
                annotation_id: None,
            },
        )));
    }
    let change_annotations = with_annotation.then(|| {
        HashMap::from([(
            CANDIDATE_ANNOTATION.to_string(),
            ChangeAnnotation {
                label: "Possible callers (review)".to_string(),
                needs_confirmation: Some(true),
                description: Some(
                    "Sends of this selector the editor cannot prove belong to the renamed \
                     method; they may call an unrelated method of the same name."
                        .to_string(),
                ),
            },
        )])
    });
    WorkspaceEdit {
        document_changes: Some(DocumentChanges::Operations(ops)),
        change_annotations,
        ..Default::default()
    }
}

/// Legacy `changes` map for a client without `documentChanges`.
fn changes_map_edit(files: Vec<FileEdits>) -> WorkspaceEdit {
    let changes = files
        .into_iter()
        .map(|file| {
            let flat = file
                .edits
                .into_iter()
                .map(|e| match e {
                    OneOf::Left(t) => t,
                    OneOf::Right(a) => a.text_edit,
                })
                .collect();
            (file.uri, flat)
        })
        .collect();
    WorkspaceEdit {
        changes: Some(changes),
        ..Default::default()
    }
}

/// The `window/showMessage` text listing candidate sites the edit omitted.
fn candidate_message(
    plan: &RenamePlan,
    source_of: &dyn Fn(&Utf8PathBuf) -> Option<String>,
) -> String {
    let total = plan.candidates.len();
    let mut lines = vec![format!(
        "Renamed the definition and the self/super sends that provably call it. {total} other \
         send(s) of this selector were not changed because they may call an unrelated method; \
         review and rename them by hand:"
    )];
    for group in plan.candidates.iter().take(MESSAGE_SITE_LIMIT) {
        let site = &group[0];
        let line =
            source_of(&site.file).map_or(0, |src| span_to_range(site.span, &src).start.line + 1);
        lines.push(format!("  {}:{line}", site.file));
    }
    if total > MESSAGE_SITE_LIMIT {
        lines.push(format!("  ... and {} more", total - MESSAGE_SITE_LIMIT));
    }
    lines.join("\n")
}

impl Backend {
    /// `textDocument/prepareRename`: the token range of a project class or selector.
    #[allow(
        clippy::unused_async,
        reason = "mirrors the async LanguageServer::prepare_rename signature it's dispatched from"
    )]
    pub(in crate::server) async fn handle_prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let Some(path) = self.resolve_path_for_uri(&params.text_document.uri) else {
            return Err(refusal(NOT_RENAMEABLE_DOCUMENT));
        };
        let svc = self.service.lock_recover();
        let Some(source) = svc.file_source(&path) else {
            return Err(refusal(NOT_INDEXED_DOCUMENT));
        };
        let pos = to_bt_position(params.position, &source);
        match svc.prepare_rename(&path, pos) {
            Ok(prepared) => Ok(Some(PrepareRenameResponse::RangeWithPlaceholder {
                range: span_to_range(prepared.span, &source),
                placeholder: prepared.placeholder.to_string(),
            })),
            Err(refused) => Err(refusal(refused.message)),
        }
    }

    /// `textDocument/rename`: ADR 0114's confirmed sites applied, candidates annotated.
    pub(in crate::server) async fn handle_rename(
        &self,
        params: RenameParams,
    ) -> Result<Option<WorkspaceEdit>> {
        let uri = &params.text_document_position.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Err(refusal(NOT_RENAMEABLE_DOCUMENT));
        };
        let caps = *self.rename_caps.lock_recover();
        // Plan under the lock and snapshot, in the same critical section, the
        // text of every file the plan touches plus the document versions and
        // the open set. `text_sync` serialises this with `did_change` /
        // `did_open` (which update text and version together), so a version
        // can never be newer than the text its ranges were computed from
        // (BT-5132). Disk I/O happens after release.
        let (plan, sources, versions, open_files) = {
            let _sync = self.text_sync.lock_recover();
            let svc = self.service.lock_recover();
            let Some(source) = svc.file_source(&path) else {
                return Err(refusal(NOT_INDEXED_DOCUMENT));
            };
            let pos = to_bt_position(params.text_document_position.position, &source);
            let plan = svc
                .plan_rename(&path, pos, &params.new_name)
                .map_err(|refused| refusal(refused.message))?;
            let sources: HashMap<Utf8PathBuf, String> = plan_files(&plan)
                .into_iter()
                .filter_map(|f| svc.file_source(&f).map(|src| (f, src)))
                .collect();
            let versions: HashMap<Utf8PathBuf, i32> = self.versions.lock_recover().clone();
            let open_files: HashSet<Utf8PathBuf> = versions.keys().cloned().collect();
            (plan, sources, versions, open_files)
        };
        // BT-5078: spans are byte offsets into the *indexed* text. For a file
        // the editor does not have open, that text may be older than what is
        // on disk (`git pull`, an external edit, a delete), and the client
        // would apply our ranges to the real file. Refuse rather than write
        // edits computed from stale text.
        let stale = stale_closed_files(&plan, &open_files, &sources, &|f| {
            fs::read_to_string(f).ok()
        });
        if !stale.is_empty() {
            // Bring the index up to date so the user's retry plans against
            // the real text. BT-5132: under `text_sync`, and skipping a file
            // opened since the snapshot, whose buffer text must win.
            {
                let _sync = self.text_sync.lock_recover();
                let mut svc = self.service.lock_recover();
                let versions = self.versions.lock_recover();
                for (file, disk) in &stale {
                    if versions.contains_key(file) {
                        continue;
                    }
                    match disk {
                        Some(text) => svc.update_file(file.clone(), text.clone()),
                        None => svc.remove_file(file),
                    }
                }
            }
            let names: Vec<String> = stale.iter().map(|(f, _)| f.to_string()).collect();
            return Err(refusal(format!(
                "Rename of {} cancelled: {} changed on disk since it was indexed, so the edits \
                 could land in the wrong place. The index has been refreshed; run the rename \
                 again.",
                plan.target,
                names.join(", ")
            )));
        }
        let outcome = build_rename_edit(&plan, caps, &|f| sources.get(f).cloned(), &versions)
            .map_err(|why| {
                refusal(format!(
                    "Rename of {} cancelled: {why}, so the rename would be incomplete. Nothing \
                     was changed; save the affected files and run the rename again.",
                    plan.target
                ))
            })?;
        if let Some(message) = outcome.message {
            self.client
                .show_message(MessageType::WARNING, message)
                .await;
        }
        Ok(Some(outcome.edit))
    }
}
