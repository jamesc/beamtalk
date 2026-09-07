// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lifecycle handlers: initialize/initialized/shutdown and the
//! didOpen/didChange/didClose/didSave text-document sync notifications.

use super::super::Backend;
use super::super::commands::BEAMTALK_LSP_COMMANDS;
use super::super::config::{
    PreloadConfig, configured_delegate_to_runtime, configured_stdlib_source_dir,
    configured_stdlib_source_dirs, preload_covers, workspace_roots,
};
use super::super::convert::uri_to_path;
use std::fs;

use beamtalk_language_service::LanguageService;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CallHierarchyOptions, CallHierarchyServerCapability, CodeActionProviderCapability,
    CompletionOptions, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, ExecuteCommandOptions,
    FoldingRangeProviderCapability, HoverProviderCapability, ImplementationProviderCapability,
    InitializeParams, InitializeResult, InitializedParams, OneOf, ServerCapabilities,
    SignatureHelpOptions, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions, TextDocumentSyncSaveOptions, WorkDoneProgressOptions,
};
use tracing::debug;

const DIAGNOSTIC_DEBOUNCE_DURATION: std::time::Duration = std::time::Duration::from_millis(150);

impl Backend {
    /// Reports server capabilities to the client during handshake.
    #[allow(
        clippy::unused_async,
        reason = "mirrors the async LanguageServer::initialize signature it's dispatched from"
    )]
    pub(in crate::server) async fn handle_initialize(
        &self,
        params: InitializeParams,
    ) -> Result<InitializeResult> {
        let roots = workspace_roots(&params);
        let configured_stdlib = configured_stdlib_source_dir(&params);
        let stdlib_dirs = configured_stdlib_source_dirs(configured_stdlib.as_deref(), &roots);
        // read the `delegateToRuntime` flag from
        // `initializationOptions`. Default is `false` (foundation issue
        // keeps current behaviour; per-method children opt the flag on as
        // they're rolled out).
        let delegate = configured_delegate_to_runtime(&params);
        self.set_delegate_to_runtime(delegate);
        {
            let mut stored_roots = self
                .workspace_roots
                .lock()
                .expect("workspace_roots lock poisoned");
            (*stored_roots).clone_from(&roots);
        }
        {
            let mut preload_config = self
                .preload_config
                .lock()
                .expect("preload_config lock poisoned");
            *preload_config = Some(PreloadConfig { roots, stdlib_dirs });
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        save: Some(TextDocumentSyncSaveOptions::Supported(true)),
                        ..Default::default()
                    },
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".into(), ":".into()]),
                    ..Default::default()
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec![":".into()]),
                    retrigger_characters: Some(vec![" ".into()]),
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                // `textDocument/implementation` — selector under
                // the cursor → `SystemNavigation implementorsOf:` via the
                // runtime-delegate seam (cold-file fallback walks
                // the AST). Wired alongside `references_provider` because
                // both go through the same `Backend::delegate_nav_query`
                // helper and the same `nav-query` REPL op.
                implementation_provider: Some(ImplementationProviderCapability::Simple(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                // `textDocument/foldingRange` — one range per
                // `// === Name ===` section divider, AST-only (see
                // `Backend::folding_range`'s doc comment).
                folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
                // `textDocument/prepareTypeHierarchy` +
                // `typeHierarchy/{supertypes,subtypes}` — the class under
                // the cursor → `Behaviour superclassChain` / `allSubclasses`.
                // `lsp-types` 0.94.1 does not yet expose a typed
                // `type_hierarchy_provider` field on `ServerCapabilities`
                // (added upstream in 0.95+), so we advertise via the typed
                // `experimental` escape hatch. VSCode and Helix both accept
                // the capability through this channel; the surface-drift
                // checker recognises the `experimental` JSON value so the
                // surface-parity doc keeps verifying.
                experimental: Some(serde_json::json!({
                    "typeHierarchyProvider": true,
                })),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                document_range_formatting_provider: Some(OneOf::Left(true)),
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                // ADR 0082 Phase 3: workspace/executeCommand for
                // `beamtalk.flush` and `beamtalk.saveClass` — the LSP-surface
                // wrappers that dispatch to `Workspace flush` /
                // `Workspace newClass:at:` on the attached runtime. Keep the
                // command identifiers stable; the surface-parity drift checker
                // hashes them.
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: BEAMTALK_LSP_COMMANDS
                        .iter()
                        .map(|s| (*s).to_string())
                        .collect(),
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                }),
                // `textDocument/prepareCallHierarchy` plus
                // `callHierarchy/{incomingCalls,outgoingCalls}` (prepare
                // lives under `textDocument/` per the LSP spec because
                // it takes a text-document position; the follow-ups live
                // under `callHierarchy/` because they take a
                // `CallHierarchyItem`). Incoming calls are answered via
                // the existing `nav-query` `senders` kind through
                // `Backend::delegate_nav_query`; outgoing calls walk the
                // method body's AST in-process (the cold-file path is the
                // only correct answer because the body lives in the open
                // file). The advertised capability is wired regardless of
                // the `delegateToRuntime` flag — runtime delegation flips
                // only the incoming-calls fallback path between the AST
                // walker and the runtime's `senders_of` xref index.
                call_hierarchy_provider: Some(CallHierarchyServerCapability::Options(
                    CallHierarchyOptions {
                        work_done_progress_options: WorkDoneProgressOptions::default(),
                    },
                )),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    /// Called after the client acknowledges initialization.
    pub(in crate::server) async fn handle_initialized(&self, _: InitializedParams) {
        let preload_config = {
            let mut preload_config = self
                .preload_config
                .lock()
                .expect("preload_config lock poisoned");
            preload_config.take()
        };
        if let Some(ref config) = preload_config {
            // mark preload in-flight so a didOpen/didChange racing
            // this sequence defers its own `publish_diagnostics` to the
            // self-healing republish below instead of racing it to be the
            // last notification sent — see `is_preload_in_progress`'s doc.
            {
                let mut svc = self.service.lock().expect("service lock poisoned");
                svc.set_preload_in_progress(true);
            }
            // load each workspace root's real beamtalk.toml
            // [package] name before preload indexes files under that root,
            // so first-time stamping is already correct. This
            // ordering is best-effort, not a guarantee — a didOpen/didChange
            // racing this sequence still stamps eagerly with the fallback
            // marker, and set_root_packages/mark_stdlib_file re-stamp those
            // files when they run. (The [diagnostics] table below needs no
            // such care: it is applied per-request at diagnostic-computation
            // time.)
            self.load_root_packages(&config.roots).await;
            self.preload_workspace_source_files(config.clone()).await;
            // ADR 0075: Load type cache from _build/type_cache/ for typed completions.
            self.load_type_cache(&config.roots).await;
            // ADR 0100 Rule 3: load beamtalk.toml's [diagnostics]
            // severity-override table so the LSP agrees with `beamtalk build`.
            self.load_diagnostics_table(&config.roots).await;
            // the project index is now fully populated (and won't
            // change again from this startup sequence) — clear the flag
            // *before* republishing below, so republish's own
            // `publish_diagnostics` calls actually send.
            {
                let mut svc = self.service.lock().expect("service lock poisoned");
                svc.set_preload_in_progress(false);
            }

            // Re-publish diagnostics for every open file after preload
            // completes. If a file was opened via `did_open` before preload
            // finished (or against an incomplete project index), its initial
            // diagnostics may contain stale `unresolved_class` warnings against
            // classes that have since been indexed. Republishing self-heals
            // those without requiring the user to touch the file.
            self.republish_open_diagnostics().await;
        }

        // Resolve OTP lib dir for FFI goto-definition.
        self.resolve_otp_lib_dir().await;

        // dynamically register the type-hierarchy capability so
        // clients that ignore the `experimental` channel still pick it up.
        // `lsp-types` 0.94.1 doesn't expose `type_hierarchy_provider` on
        // `ServerCapabilities`, but `client/registerCapability` works for
        // every LSP-3.17 client regardless of the typed-field availability.
        // Failures here are non-fatal — older clients may reject dynamic
        // registration entirely, in which case the `experimental` field
        // remains the advertisement channel.
        self.register_type_hierarchy_capability().await;

        debug!("beamtalk-lsp initialized");
        self.client
            .log_message(
                tower_lsp::lsp_types::MessageType::INFO,
                "Beamtalk language server ready",
            )
            .await;
    }

    /// Handles a graceful shutdown request from the client.
    #[allow(
        clippy::unused_async,
        reason = "mirrors the async LanguageServer::shutdown signature it's dispatched from"
    )]
    pub(in crate::server) async fn handle_shutdown(&self) -> Result<()> {
        Ok(())
    }

    /// Indexes a newly opened document and publishes diagnostics.
    pub(in crate::server) async fn handle_did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(path) = self.resolve_path_for_uri(&uri) {
            // Recorded *before* `update_file` so a concurrently dispatched
            // `did_close` for the same path (a close immediately followed by
            // a reopen) can observe "this path is open again" as early as
            // possible — see `did_close`'s own comment for why this specific
            // ordering, paired with the lock it holds there, is what makes
            // the reopen race-proof rather than just less likely.
            {
                let generation = self
                    .next_open_generation
                    .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                let mut open_generation = self
                    .open_generation
                    .lock()
                    .expect("open_generation lock poisoned");
                open_generation.insert(path.clone(), generation);
            }
            {
                let mut versions = self.versions.lock().expect("versions lock poisoned");
                versions.insert(path.clone(), params.text_document.version);
            }
            if uri.scheme() != "beamtalk-stdlib" {
                // Stdlib files are pre-loaded at startup; skip re-indexing.
                let mut svc = self.service.lock().expect("service lock poisoned");
                svc.update_file(path, params.text_document.text);
            }
            self.publish_diagnostics(&uri).await;
        }
    }

    /// Re-indexes a document after edits and republishes diagnostics.
    pub(in crate::server) async fn handle_did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        // Stdlib virtual documents are read-only; ignore all change events.
        if uri.scheme() == "beamtalk-stdlib" {
            return;
        }
        if let (Some(path), Some(change)) =
            (uri_to_path(&uri), params.content_changes.into_iter().last())
        {
            {
                let mut svc = self.service.lock().expect("service lock poisoned");
                svc.update_file(path.clone(), change.text);
            }
            {
                let mut versions = self.versions.lock().expect("versions lock poisoned");
                versions.insert(path.clone(), params.text_document.version);
            }
            // Buffer now diverges from the on-disk bytes (and the
            // runtime's compiled module) until the next `didSave`.
            self.mark_dirty(path);

            let generation = {
                let mut generations = self
                    .diagnostic_generation
                    .lock()
                    .expect("diagnostic_generation lock poisoned");
                let entry = generations.entry(uri.clone()).or_insert(0);
                *entry += 1;
                *entry
            };

            tokio::time::sleep(DIAGNOSTIC_DEBOUNCE_DURATION).await;

            let is_latest = {
                let generations = self
                    .diagnostic_generation
                    .lock()
                    .expect("diagnostic_generation lock poisoned");
                generations.get(&uri).copied() == Some(generation)
            };

            if is_latest {
                self.publish_diagnostics(&uri).await;
            }
        }
    }

    /// Removes a closed document from the index and clears its diagnostics.
    ///
    /// The non-stdlib path below does a disk read that is not instantaneous,
    /// so a `did_open` for the same path (a close immediately followed by a
    /// reopen) can be dispatched concurrently and complete while this is
    /// still reading — applying the now-stale disk snapshot afterwards would
    /// silently clobber that fresher in-memory content. Guarded via
    /// [`Self::open_generation`]: snapshotted before the I/O-bearing work,
    /// rechecked inside the critical section that applies the update. A
    /// mismatch means a genuine reopen raced in and won, so this whole
    /// handler backs off entirely — leaving `svc`, `versions`, and the
    /// reopened document's diagnostics to it.
    ///
    /// Deliberately keyed on `open_generation`, not `versions`: an ordinary
    /// `did_change` racing the same close (e.g. a final keystroke, or a
    /// format-on-save edit, landing right before the tab closes) also bumps
    /// `versions`, but is not a reopen — the document is still closing
    /// either way, so its edit is moot and must not make this handler back
    /// off *permanently* (which would leak the exact eviction bug this
    /// method exists to fix, plus skip the dirty/diagnostic-generation
    /// cleanup and the empty-diagnostics publish below, forever). Only
    /// `did_open` bumps `open_generation`, so it alone can trigger the
    /// back-off.
    pub(in crate::server) async fn handle_did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(path) = self.resolve_path_for_uri(&uri) {
            // Snapshotted *before* any of the (I/O-bearing, for the
            // non-stdlib branch) work below, so it reflects this document's
            // open-generation at the instant we started closing it — not a
            // later one a racing `did_open` on the same path might install.
            let generation_before = {
                let open_generation = self
                    .open_generation
                    .lock()
                    .expect("open_generation lock poisoned");
                open_generation.get(&path).copied()
            };
            let disk_content = if uri.scheme() == "beamtalk-stdlib" {
                None
            } else {
                // Startup preload indexes every file under each
                // root's `src/`/`test/`/`_build/deps/*/src` regardless of
                // whether it is open, so the index is workspace-wide, not
                // "currently open files". Closing a tab therefore must not
                // evict the file — that silently removed its classes from
                // the merged hierarchy, and every other file referencing them
                // reported `Unresolved class` until the next restart. Revert
                // to the on-disk content instead (unsaved edits are discarded
                // on close, so disk is the truth). Files preload never
                // covered — scratch files outside those dirs, untitled
                // buffers, files deleted from disk — are still removed.
                let covered = {
                    let roots = self
                        .workspace_roots
                        .lock()
                        .expect("workspace_roots lock poisoned");
                    preload_covers(path.as_std_path(), &roots)
                };
                let is_stdlib = {
                    let svc = self.service.lock().expect("service lock poisoned");
                    svc.project_index().is_stdlib_file(&path)
                };
                let keep = covered || is_stdlib;
                // Read with no lock held, per this method's own doc.
                keep.then(|| fs::read_to_string(&path).ok()).flatten()
            };

            // Guard against the reopen race: hold `open_generation` across
            // both the check *and* the `svc`/`versions` mutation below, not
            // just the check — releasing it in between would reopen the
            // exact same window under a different name (a reopen landing
            // after we decide "safe to write" but before we actually write
            // would still get silently clobbered by our stale disk
            // snapshot). `did_open` records its path here *before* touching
            // `svc`/`versions` (see its own comment), so if a reopen's
            // `did_open` acquired this lock first, the generation check
            // below is guaranteed to see it and we back off entirely,
            // touching nothing. If we acquire this lock first instead,
            // `did_open`'s insert simply blocks until we release it (after
            // our own write below completes), so its own `update_file` call
            // is guaranteed to run after — and therefore win over — ours.
            let reopened = {
                let mut open_generation = self
                    .open_generation
                    .lock()
                    .expect("open_generation lock poisoned");
                let reopened = open_generation.get(&path).copied() != generation_before;
                if !reopened {
                    if uri.scheme() == "beamtalk-stdlib" {
                        let mut versions = self.versions.lock().expect("versions lock poisoned");
                        versions.remove(&path);
                    } else {
                        let mut svc = self.service.lock().expect("service lock poisoned");
                        match disk_content {
                            Some(content) => svc.update_file(path.clone(), content),
                            None => svc.remove_file(&path),
                        }
                        let mut versions = self.versions.lock().expect("versions lock poisoned");
                        versions.remove(&path);
                    }
                    open_generation.remove(&path);
                }
                reopened
            };
            if reopened {
                // Leave `svc`, `versions`, and this entry alone — they
                // belong to the reopen now.
                return;
            }
            self.clear_dirty(&path);
            {
                let mut generations = self
                    .diagnostic_generation
                    .lock()
                    .expect("diagnostic_generation lock poisoned");
                generations.remove(&uri);
            }
            if uri.scheme() != "beamtalk-stdlib" {
                self.client.publish_diagnostics(uri, Vec::new(), None).await;
            }
        }
    }

    /// Handles save notifications and republishes diagnostics.
    pub(in crate::server) async fn handle_did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;
        debug!(uri = %uri, "did_save");
        // Editor buffer now matches the on-disk bytes again. The runtime
        // will still be stale until the workspace recompiles the module
        // (a separate flush cycle), but the AST path is no longer
        // *strictly* required for outline correctness.
        if let Some(path) = self.resolve_path_for_uri(&uri) {
            self.clear_dirty(&path);
        }
        self.publish_diagnostics(&uri).await;
    }
}
