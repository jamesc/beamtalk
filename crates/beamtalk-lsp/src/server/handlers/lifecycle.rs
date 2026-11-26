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
use super::super::core::PreloadGuard;
use super::super::guard::panic_message;
use crate::server::lock::LockRecover;
use futures_util::FutureExt;
use std::fs;
use std::panic::AssertUnwindSafe;

use beamtalk_language_service::LanguageService;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CallHierarchyOptions, CallHierarchyServerCapability, CodeActionProviderCapability,
    CompletionOptions, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, ExecuteCommandOptions,
    FoldingRangeProviderCapability, HoverProviderCapability, ImplementationProviderCapability,
    InitializeParams, InitializeResult, InitializedParams, OneOf, RenameOptions,
    ServerCapabilities, ServerInfo, SignatureHelpOptions, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextDocumentSyncOptions, TextDocumentSyncSaveOptions,
    WorkDoneProgressOptions,
};
use tracing::debug;

/// What other open files were last re-checked against for one open file
/// (BT-5188; see `Backend::interface_baselines`).
#[derive(Debug, Default)]
pub(in crate::server) struct InterfaceBaseline {
    /// The file's interface (`None`: it exported nothing).
    interface: Option<beamtalk_language_service::FileInterface>,
    /// While the file had a parse error, error recovery made the indexed
    /// interface differ from `interface`. Other files may have been checked
    /// against that recovered interface since, so clearing the error
    /// re-checks them once even when the interface is back to `interface`.
    diverged_while_broken: bool,
}

/// Startup preload panics after which the server stops retrying (BT-5188).
const MAX_STARTUP_PRELOAD_FAILURES: u32 = 3;

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
        *self.rename_caps.lock_recover() = super::rename::RenameClientCaps::from_params(&params);
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
            let mut stored_roots = self.workspace_roots.lock_recover();
            (*stored_roots).clone_from(&roots);
        }
        {
            let mut preload_config = self.preload_config.lock_recover();
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
                // `textDocument/rename` + `prepareRename` (BT-5007): static
                // rename following ADR 0114's confirmed/candidate tiers.
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                })),
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
            server_info: Some(ServerInfo {
                name: "beamtalk-lsp".to_string(),
                version: Some(beamtalk_repl_protocol::handshake::build_id().to_string()),
            }),
        })
    }

    /// Called after the client acknowledges initialization.
    pub(in crate::server) async fn handle_initialized(&self, _: InitializedParams) {
        let preload_config = {
            let mut preload_config = self.preload_config.lock_recover();
            preload_config.take()
        };
        if let Some(config) = preload_config {
            // BT-5188: a panic is contained here, so the rest of this
            // function (OTP lib dir, capability and watcher registration)
            // still runs.
            self.run_startup_preload_contained(config).await;
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
        // BT-5078: learn about on-disk changes (git pull, deletes, renames).
        self.register_file_watchers().await;

        debug!("beamtalk-lsp initialized");
        self.client
            .log_message(
                tower_lsp::lsp_types::MessageType::INFO,
                "Beamtalk language server ready",
            )
            .await;
    }

    /// Runs the startup preload, containing a panic (BT-5188).
    ///
    /// Whatever happens, the preload flag is cleared and every open file is
    /// republished: the publishes skipped while the load ran would otherwise
    /// be lost. After a panic the config is put back so the next `did_open`
    /// retries the load ([`Self::retry_startup_preload_if_pending`]), up to
    /// [`MAX_STARTUP_PRELOAD_FAILURES`] times.
    pub(in crate::server) async fn run_startup_preload_contained(&self, config: PreloadConfig) {
        let outcome = AssertUnwindSafe(self.run_startup_preload(config.clone()))
            .catch_unwind()
            .await;
        if let Err(payload) = outcome {
            let failures = self
                .startup_preload_failures
                .fetch_add(1, std::sync::atomic::Ordering::SeqCst)
                + 1;
            let msg = panic_message(payload.as_ref());
            if failures < MAX_STARTUP_PRELOAD_FAILURES {
                tracing::error!(
                    panic = %msg,
                    failures,
                    "startup preload panicked; retrying on the next opened file"
                );
                *self.preload_config.lock_recover() = Some(config);
            } else {
                tracing::error!(
                    panic = %msg,
                    failures,
                    "startup preload panicked again; giving up for this session"
                );
            }
        }
        // Re-publish diagnostics for every open file after preload ends. If a
        // file was opened via `did_open` before preload finished (or against
        // an incomplete project index), its initial diagnostics may contain
        // stale `unresolved_class` warnings against classes that have since
        // been indexed, or were never sent at all (`publish_diagnostics`
        // skips while the flag is set). Republishing self-heals those
        // without requiring the user to touch the file.
        self.republish_open_diagnostics().await;
    }

    /// Retries a startup preload that panicked (BT-5188). A no-op unless an
    /// earlier attempt failed and left its config to retry.
    async fn retry_startup_preload_if_pending(&self) {
        if self
            .startup_preload_failures
            .load(std::sync::atomic::Ordering::SeqCst)
            == 0
        {
            return;
        }
        let config = self.preload_config.lock_recover().take();
        if let Some(config) = config {
            self.run_startup_preload_contained(config).await;
        }
    }

    /// The startup preload sequence. Clears the preload flag on every exit,
    /// a panic included; the caller republishes.
    async fn run_startup_preload(&self, config: PreloadConfig) {
        // Held for the whole startup sequence so a package root that
        // `did_open` registers meanwhile (BT-4158) loads strictly after
        // it, rather than racing its whole-session setter calls.
        let root_load_guard = self.root_load_lock.lock().await;
        // The per-root loaders replace whole-session state, so feed them
        // every known root — not just `config.roots` — in case a
        // `did_open` registered one before this lock was taken.
        let all_roots = self.workspace_roots_snapshot();
        // mark preload in-flight so a didOpen/didChange racing
        // this sequence defers its own `publish_diagnostics` to the
        // self-healing republish after it instead of racing it to be the
        // last notification sent — see `is_preload_in_progress`'s doc.
        let mut preload_guard = PreloadGuard::begin(&self.service);
        // load each workspace root's real beamtalk.toml
        // [package] name before preload indexes files under that root,
        // so first-time stamping is already correct. This
        // ordering is best-effort, not a guarantee — a didOpen/didChange
        // racing this sequence still stamps eagerly with the fallback
        // marker, and set_root_packages/mark_stdlib_file re-stamp those
        // files when they run. (The [diagnostics] table below needs no
        // such care: it is applied per-request at diagnostic-computation
        // time.)
        self.load_root_packages(&all_roots).await;
        self.inject_preload_panic();
        self.preload_workspace_source_files(config).await;
        // ADR 0075: Load type cache from _build/type_cache/ for typed completions.
        self.load_type_cache(&all_roots).await;
        // ADR 0100 Rule 3: load beamtalk.toml's [diagnostics]
        // severity-override table so the LSP agrees with `beamtalk build`.
        self.load_diagnostics_table(&all_roots).await;
        // the project index is now fully populated (and won't
        // change again from this startup sequence) — clear the flag
        // *before* the caller republishes, so republish's own
        // `publish_diagnostics` calls actually send.
        preload_guard.finish();
        drop(root_load_guard);
    }

    /// [`Self::ensure_package_root_loaded`], containing a panic (BT-5188):
    /// the root's guards clear the preload flag and unregister the root (so
    /// the next `did_open` under it retries), and the open files are
    /// republished, since publishes skipped during the load are lost.
    async fn ensure_package_root_loaded_contained(&self, path: &camino::Utf8PathBuf) {
        let outcome = AssertUnwindSafe(self.ensure_package_root_loaded(path))
            .catch_unwind()
            .await;
        if let Err(payload) = outcome {
            tracing::error!(
                panic = %panic_message(payload.as_ref()),
                path = %path,
                "package root preload panicked; retrying on the next file opened under it"
            );
            self.republish_open_diagnostics().await;
        }
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
                let mut open_generation = self.open_generation.lock_recover();
                open_generation.insert(path.clone(), generation);
            }
            {
                let mut versions = self.versions.lock_recover();
                versions.insert(path.clone(), params.text_document.version);
            }
            let mut interface_changed = false;
            if uri.scheme() != "beamtalk-stdlib" {
                // BT-4967: the file's FFI registry goes in *before* its text,
                // so no diagnostics pass can see the file without it.
                self.load_stdlib_file_registry(&path).await;
                // Stdlib files are pre-loaded at startup; skip re-indexing.
                {
                    // BT-5078: the registry load above awaited, so a
                    // `did_change` (newer text, newer version) or a
                    // `did_close` may have run in the gap. Only write this
                    // open's text if it is still the latest we recorded.
                    let _sync = self.text_sync.lock_recover();
                    let still_current = self.versions.lock_recover().get(&path).copied()
                        == Some(params.text_document.version);
                    if still_current {
                        let mut svc = self.service.lock_recover();
                        // BT-5131: an open whose buffer differs from the
                        // preloaded (disk) text can change what other open
                        // files resolve, like a `did_change` would. The one
                        // clone per open is the baseline later edits compare
                        // against by reference (BT-5188).
                        let before = svc.project_index().file_interface(&path);
                        svc.update_file(path.clone(), params.text_document.text);
                        self.interface_baselines.lock_recover().insert(
                            path.clone(),
                            InterfaceBaseline {
                                interface: before,
                                diverged_while_broken: false,
                            },
                        );
                        interface_changed = self.interface_changed_since_baseline(&svc, &path);
                    }
                }
                // BT-4158: resolve this file's own package root (nearest
                // `beamtalk.toml`) — the workspace folder may be a parent
                // of it, leaving its dependencies unloaded.
                self.ensure_package_root_loaded_contained(&path).await;
            }
            self.publish_diagnostics(&uri).await;
            // After this file's own publish, so a retried whole-workspace
            // walk never holds up the file being opened (BT-5188).
            self.retry_startup_preload_if_pending().await;
            if interface_changed {
                self.request_cross_file_republish();
            }
        }
    }

    /// Whether other open files must be re-checked for `path`'s indexed
    /// interface, recording it as the new baseline if so (BT-5188). Call with
    /// the service locked, right after indexing the file's new text.
    ///
    /// While the file has a parse error the answer is "no": error recovery
    /// can drop declarations, so a pause mid-typing must not re-analyse every
    /// other open file. The baseline is kept, and if recovery made the
    /// interface differ from it, clearing the error re-checks the other files
    /// once (they may have been checked against the recovered interface in
    /// the meantime). The comparison is by reference; only an actual change
    /// clones the new interface. A file with no baseline (an edit that raced
    /// its open) gets the current interface as its baseline, and no change.
    fn interface_changed_since_baseline(
        &self,
        svc: &beamtalk_language_service::SimpleLanguageService,
        path: &camino::Utf8PathBuf,
    ) -> bool {
        let index = svc.project_index();
        let mut baselines = self.interface_baselines.lock_recover();
        let Some(base) = baselines.get_mut(path) else {
            baselines.insert(
                path.clone(),
                InterfaceBaseline {
                    interface: index.file_interface(path),
                    diverged_while_broken: false,
                },
            );
            return false;
        };
        let unchanged = index.file_interface_matches(path, base.interface.as_ref());
        if svc.has_parse_errors(path) {
            base.diverged_while_broken |= !unchanged;
            return false;
        }
        if !unchanged {
            base.interface = index.file_interface(path);
        }
        let recheck = !unchanged || base.diverged_while_broken;
        base.diverged_while_broken = false;
        recheck
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
                let _sync = self.text_sync.lock_recover();
                {
                    let mut svc = self.service.lock_recover();
                    // BT-5078/BT-5109/BT-5188: whether the interface changed
                    // is decided once the edits settle (below), against the
                    // baseline, so ordinary typing compares and clones
                    // nothing here.
                    svc.update_file(path.clone(), change.text);
                }
                let mut versions = self.versions.lock_recover();
                versions.insert(path.clone(), params.text_document.version);
            }
            // Buffer now diverges from the on-disk bytes (and the
            // runtime's compiled module) until the next `didSave`.
            self.mark_dirty(path.clone());

            let generation = {
                let mut generations = self.diagnostic_generation.lock_recover();
                let entry = generations.entry(uri.clone()).or_insert(0);
                *entry += 1;
                *entry
            };

            tokio::time::sleep(DIAGNOSTIC_DEBOUNCE_DURATION).await;

            let is_latest = {
                let generations = self.diagnostic_generation.lock_recover();
                generations.get(&uri).copied() == Some(generation)
            };

            if is_latest {
                self.publish_diagnostics(&uri).await;
                // Compared here, against the baseline, not per edit: an
                // earlier edit in this debounce window may have renamed the
                // class and been superseded by this one.
                let interface_changed = {
                    let svc = self.service.lock_recover();
                    self.interface_changed_since_baseline(&svc, &path)
                };
                if interface_changed {
                    // Already debounced (this is the latest edit) and gated
                    // on the file's interface changing, so ordinary typing
                    // never re-analyses the other open files.
                    self.request_cross_file_republish();
                }
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
                let open_generation = self.open_generation.lock_recover();
                open_generation.get(&path).copied()
            };
            let disk_content = if uri.scheme() == "beamtalk-stdlib" {
                None
            } else {
                // Startup preload indexes every file under each
                // root's `src/`/`test/` and dependency sources regardless of
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
                    let roots = self.workspace_roots.lock_recover();
                    preload_covers(path.as_std_path(), &roots)
                };
                let is_stdlib = {
                    let svc = self.service.lock_recover();
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
            let mut interface_changed = false;
            let reopened = {
                let mut open_generation = self.open_generation.lock_recover();
                let reopened = open_generation.get(&path).copied() != generation_before;
                if !reopened {
                    if uri.scheme() == "beamtalk-stdlib" {
                        let mut versions = self.versions.lock_recover();
                        versions.remove(&path);
                    } else {
                        let mut svc = self.service.lock_recover();
                        // BT-5131: reverting an edited buffer to disk (or
                        // dropping the file) can change what other open
                        // files resolve. Compared against what other files
                        // were last re-checked with (BT-5188); with no
                        // baseline (an open that lost a race), against the
                        // buffer's own interface.
                        let baseline = self
                            .interface_baselines
                            .lock_recover()
                            .remove(&path)
                            .unwrap_or_else(|| InterfaceBaseline {
                                interface: svc.project_index().file_interface(&path),
                                diverged_while_broken: false,
                            });
                        match disk_content {
                            Some(content) => svc.update_file(path.clone(), content),
                            None => svc.remove_file(&path),
                        }
                        // Parse errors on disk still count: nothing would
                        // re-check the other files later.
                        interface_changed = baseline.diverged_while_broken
                            || !svc
                                .project_index()
                                .file_interface_matches(&path, baseline.interface.as_ref());
                        let mut versions = self.versions.lock_recover();
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
                let mut generations = self.diagnostic_generation.lock_recover();
                generations.remove(&uri);
            }
            if uri.scheme() != "beamtalk-stdlib" {
                // BT-5078: ordered after any in-flight publish (and any
                // later one sees the document closed), so nothing re-adds
                // diagnostics after this clear.
                let _publishing = self.publish_lock.lock().await;
                self.client
                    .publish_diagnostics(uri.clone(), Vec::new(), None)
                    .await;
            }
            if interface_changed {
                self.request_cross_file_republish();
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
        // A startup preload that panicked is retried on the next open or
        // save, so editing only already-open files still recovers (BT-5188).
        self.retry_startup_preload_if_pending().await;
    }
}
