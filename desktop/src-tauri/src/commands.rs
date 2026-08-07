// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tauri commands wiring the picker frontend to `beamtalk-desktop-broker`
//! (process supervision) and `beamtalk-desktop-shell` (attach/empty-state
//! decisions) — ADR 0097 Implementation §3/§4, BT-2986.
//!
//! Every command here is `#[tauri::command(async)]`, even though none of
//! their bodies are themselves `async fn` (they do plain blocking I/O:
//! `spawn_front_with_port_retry`, `wait_ready`'s polling loop,
//! `Command::output()` in `cli_ops`, `Child::kill()+wait()`). This is
//! deliberate, not a mismatch: a *plain* `#[tauri::command]` (no `async`)
//! runs its body inline, synchronously, on whatever thread delivered the
//! IPC message — which on desktop is the platform webview's own UI/main
//! thread (WebView2/WKWebView/WebKitGTK's JS-bridge callbacks are
//! main-thread-bound APIs), the same thread the whole app's window/event
//! loop runs on. `attach` alone can block for up to `ATTACH_TIMEOUT` (30s);
//! running that inline on the main thread would freeze every window in the
//! app, not just the picker, for the duration. Adding `(async)` to the
//! attribute (Tauri's documented mechanism for this exact situation — see
//! `tauri::command`'s docs on "asynchronous commands") moves execution onto
//! Tauri's async-runtime thread pool instead, off the UI thread, without
//! requiring the function itself to be `async fn` or to `.await` anything.

use std::sync::{Mutex, MutexGuard, PoisonError};
use std::time::Duration;

use tauri::{AppHandle, Emitter, Manager, State, WebviewUrl, WebviewWindowBuilder, WindowEvent};

use beamtalk_desktop_broker::monitor::{self, Monitor};
use beamtalk_desktop_broker::readiness::{self, ProbeTimeouts, ReadinessState};
use beamtalk_desktop_broker::spawn::SpawnAttemptConfig;
use beamtalk_desktop_broker::{cli_ops, discovery, reap};
use beamtalk_desktop_shell::attach::{AttachDecision, AttachedFront, window_label};

use crate::dto::{
    AttachOutcome, ConnectionStateChangedEvent, ConnectionStateView, EmptyStateView, PickerView,
    WorkspaceView,
};
use crate::state::AppState;

/// Overall timeout for the `wait_ready` readiness-polling phase only —
/// generous headroom above a healthy boot, not a measured requirement. It
/// does **not** bound `spawn_front_with_port_retry` above, which runs first
/// and can itself block synchronously for up to
/// `port::DEFAULT_MAX_ATTEMPTS * spawn::DEFAULT_BIND_FAILURE_GRACE`
/// (currently up to 50s in the pathological worst case — every candidate
/// port conflicting) before `wait_ready` is ever called; `attach`'s true
/// worst-case latency is that plus this timeout, not just this timeout. In
/// practice each candidate is a fresh, distinct OS-assigned ephemeral port
/// (not the same port retried), so an all-conflict run is far less likely
/// than the single-attempt TOCTOU race `beamtalk_desktop_broker::port`'s
/// module docs describe — noted here rather than acted on, since bounding
/// the combined worst case is a product-judgment call (drop
/// `max_port_attempts` for this caller? wrap both phases in one timeout?)
/// beyond what BT-3004's calibration pass itself changed.
///
/// Originally sized against an assumption that a bad-cookie `/readiness`
/// blocks for Erlang's ~7s `net_setuptime` — measured wrong on loopback
/// (BT-3004): a real bad-cookie/dead-workspace `503` arrives in
/// milliseconds. `30s` is kept as safe headroom for a slow-but-healthy
/// front boot, not because the bad-cookie path needs it — see
/// `beamtalk_desktop_broker::readiness::ProbeTimeouts`'s doc comment for the
/// measurement.
const ATTACH_TIMEOUT: Duration = Duration::from_secs(30);
const ATTACH_POLL_INTERVAL: Duration = Duration::from_millis(300);

/// Lock `mutex`, recovering from poisoning instead of propagating it.
///
/// `state.attach`/`state.children` are each locked from several independent
/// commands (`list_workspaces`, `attach`, `detach`, `quit`, the post-attach
/// monitor thread, …). A `.map_err(|e| e.to_string())?` on `.lock()` — the
/// std default — means a single panic anywhere while holding either lock
/// poisons it permanently, and every subsequent command that touches it
/// (including `quit`/`detach_all`, the one path that could otherwise clean
/// up a still-attached front left behind by that panic) starts failing for
/// the rest of the process's life, leaking any front still running. The
/// protected data itself is usually still perfectly usable after a panic —
/// a poisoned `Mutex` is std's conservative default (the panic *might* have
/// happened mid-mutation, leaving inconsistent state), not a guarantee that
/// it did — so recovering with `into_inner()` and carrying on is the safer
/// choice for a long-lived desktop app than making the picker permanently
/// unusable until restart.
fn locked<T>(mutex: &Mutex<T>) -> MutexGuard<'_, T> {
    mutex.lock().unwrap_or_else(PoisonError::into_inner)
}

/// List discovered workspaces plus the picker's first-run empty-state
/// classification (ADR 0097 Broker §5 / User Impact: "never a silent empty
/// list").
#[tauri::command(async)]
pub fn list_workspaces(state: State<'_, AppState>) -> Result<PickerView, String> {
    let summaries = discovery::discover_workspaces().map_err(|e| e.to_string())?;
    let attach = locked(&state.attach);
    let workspaces: Vec<WorkspaceView> = summaries
        .iter()
        .map(|s| WorkspaceView::from_summary(s, attach.is_attached(&s.id)))
        .collect();
    let empty_state = beamtalk_desktop_shell::empty_state::classify_empty_state(
        workspaces.len(),
        cli_ops::resolve_cli_path,
    );
    Ok(PickerView {
        workspaces,
        empty_state: EmptyStateView::from(&empty_state),
    })
}

/// Attach to `workspace_id`: focus the existing window if already attached
/// (BT-2984 spike decision — attaching twice focuses, it does not spawn a
/// second front), else spawn a front, wait for two-stage readiness, open a
/// window, and start post-attach monitoring.
#[tauri::command(async)]
pub fn attach(
    workspace_id: String,
    app: AppHandle,
    state: State<'_, AppState>,
) -> Result<AttachOutcome, String> {
    // Atomic decide-and-claim (not a separate decide-then-record): closes
    // the exact TOCTOU race the BT-2984 spike found in its own throwaway
    // coordinator, where two near-simultaneous attach clicks for the same
    // workspace both saw "nothing tracked" and both spawned a front. See
    // `beamtalk_desktop_shell::attach`'s module docs.
    let decision = locked(&state.attach).decide_and_claim(&workspace_id);

    let generation = match decision {
        AttachDecision::FocusExisting { window_id, .. } => {
            if let Some(window) = app.get_webview_window(&window_id) {
                let _ = window.set_focus();
            }
            return Ok(AttachOutcome::Focused);
        }
        AttachDecision::AlreadyInFlight => return Ok(AttachOutcome::AlreadyAttaching),
        AttachDecision::Spawn { generation } => generation,
    };

    // From here on, the workspace is claimed: every exit path (including
    // early returns below) must release the claim on failure, or record the
    // real attachment on success, so a future attach click is never stuck
    // behind a claim nothing will resolve.
    match attach_and_open_window(&app, &state, &workspace_id, generation) {
        Ok(outcome) => Ok(outcome),
        Err(err) => {
            locked(&state.attach).release_claim(&workspace_id, generation);
            Err(err)
        }
    }
}

/// The claimed part of [`attach`]: spawn, wait for readiness, open a window,
/// and record the attachment. Split out so [`attach`] can release the claim
/// uniformly on any `Err` this returns, from any step. `generation` is the
/// claim's generation number (from [`AttachDecision::Spawn`]) — threaded
/// through to the recorded [`AttachedFront`] and [`spawn_monitor`] so the
/// post-attach monitor's stop condition survives ephemeral-port reuse (see
/// [`beamtalk_desktop_shell::attach::AttachManager::is_current_front`]).
fn attach_and_open_window(
    app: &AppHandle,
    state: &AppState,
    workspace_id: &str,
    generation: u64,
) -> Result<AttachOutcome, String> {
    emit_progress(app, workspace_id, "spawning");

    let launcher = state.launcher.clone();
    let spawn_config = SpawnAttemptConfig::new(launcher, workspace_id.to_string());
    let (child, port) = beamtalk_desktop_broker::spawn::spawn_front_with_port_retry(&spawn_config)
        .map_err(|e| e.to_string())?;
    let pid = child.id();

    // Persist the orphan-reaping record *and* start tracking the child in
    // `state.children` immediately after a successful spawn — before the
    // (up to `ATTACH_TIMEOUT`-long) readiness wait, not after. Two separate
    // reasons this matters, not one:
    //
    // - If this broker process dies uncleanly during the wait, the front is
    //   already orphaned; without a record on disk yet, the *next* broker
    //   start's `reap::sweep` (ADR 0097 Broker §4) would have no way to find
    //   and kill it at all.
    // - If the *user* quits (or detaches) while an attach is still in
    //   flight — a claim, not yet a recorded attachment — `quit`/`detach`
    //   need a way to reach and kill this child within the *current*
    //   session, not just rely on the next restart's sweep. Tracking it in
    //   `state.children` from spawn onward (looked up by `detach_internal`
    //   regardless of `AttachManager` state) closes that gap.
    //
    // From here on, every subsequent failure path kills the child via
    // `kill_and_untrack` (which also clears both of the above) rather than
    // a local `child.kill()` — the child is no longer this function's to
    // kill directly once it's in the shared map.
    persist_front_record(workspace_id, port, pid);
    locked(&state.children).insert(workspace_id.to_string(), child);

    emit_progress(app, workspace_id, "probing");

    let timeouts = ProbeTimeouts::default_local();
    let probe = readiness::http_probe("127.0.0.1", port, timeouts);
    let final_state = readiness::wait_ready(
        ReadinessState::Spawning,
        ATTACH_TIMEOUT,
        ATTACH_POLL_INTERVAL,
        probe,
    );

    match final_state {
        ReadinessState::Ready(_version) => {
            // BT-3045: the front has now actually distributed
            // (`ensure_distributed/0` runs lazily on the first `/readiness`
            // call, which just resolved above), so a real epmd-resolved
            // node_name is available on Windows for the first time — correct
            // persist_front_record's placeholder now that it's known. See
            // `update_windows_node_name_after_readiness`'s doc comment.
            //
            // Fire-and-forget on a background thread (adversarial-review
            // follow-up) rather than awaited inline here: `node_name` is
            // bookkeeping/display only — nothing in this repo reads
            // `FrontRecord.node_name` back today (`crate::reap`'s sweep keys
            // entirely off `pid`) — so there is no correctness reason for
            // window-opening below to wait on an epmd round-trip (typically
            // fast, but not zero-cost, and occasionally as slow as epmd's own
            // ~1.5s connect+read timeout budget) purely to correct a value
            // nothing consumes yet. Matches the same
            // spawn-a-detached-`std::thread`-for-best-effort-background-work
            // shape `spawn_monitor` below already uses in this file.
            // `update_windows_node_name_after_readiness` only exists (and is
            // only needed) on Windows — see its doc comment — so this gates
            // the spawn itself rather than calling into a no-op there,
            // avoiding an OS thread that would immediately return on every
            // attach off Windows.
            #[cfg(windows)]
            {
                let workspace_id_for_correction = workspace_id.to_string();
                std::thread::spawn(move || {
                    update_windows_node_name_after_readiness(
                        &workspace_id_for_correction,
                        port,
                        pid,
                    );
                });
            }
        }
        ReadinessState::Failed(reason) => {
            kill_and_untrack(state, workspace_id, port);
            return Err(format!(
                "workspace '{workspace_id}' is unreachable: {reason:?}"
            ));
        }
        ReadinessState::TimedOut(stage) => {
            kill_and_untrack(state, workspace_id, port);
            return Err(format!(
                "timed out waiting for workspace '{workspace_id}' ({stage:?})"
            ));
        }
        // `wait_ready` only ever returns one of the three arms above — but
        // matched exhaustively (not `_`) rather than treated as unreachable,
        // so a future `ReadinessState` variant added to
        // `beamtalk-desktop-broker` fails *this* build at compile time
        // (non-exhaustive match) instead of silently falling through to a
        // runtime panic here the day someone adds one without also auditing
        // every downstream match.
        ReadinessState::Spawning
        | ReadinessState::WaitingHttp
        | ReadinessState::WaitingReadiness => {
            unreachable!(
                "wait_ready only returns Ready, Failed, or TimedOut, never a waiting state"
            )
        }
    }

    let label = window_label(workspace_id);
    let url = format!("http://127.0.0.1:{port}/")
        .parse::<tauri::Url>()
        .map_err(|e| format!("invalid front URL: {e}"))?;
    let window = match WebviewWindowBuilder::new(app, label.clone(), WebviewUrl::External(url))
        .title(format!("Beamtalk — {workspace_id}"))
        .build()
    {
        Ok(window) => window,
        Err(err) => {
            // The front is up and ready but we couldn't open a window for
            // it — kill it rather than leaking a live, untracked, cookie-
            // bearing process for the rest of this session.
            kill_and_untrack(state, workspace_id, port);
            return Err(err.to_string());
        }
    };

    {
        let app_for_close = app.clone();
        let workspace_id_for_close = workspace_id.to_string();
        window.on_window_event(move |event| {
            if let WindowEvent::CloseRequested { .. } = event {
                let state = app_for_close.state::<AppState>();
                let _ = detach_internal(&app_for_close, &state, &workspace_id_for_close);
            }
        });
    }

    let recorded = locked(&state.attach).record_attached_if_claiming(AttachedFront {
        workspace_id: workspace_id.to_string(),
        port,
        pid,
        generation,
    });

    if !recorded {
        // A concurrent `detach`/`quit` cleared this workspace's claim while
        // this attach was still spawning/probing (most plausibly `quit`'s
        // `detach_all`, which — unlike a per-row Detach click, which the
        // frontend disables while a workspace is mid-attach — iterates every
        // tracked child regardless of `AttachManager` state, so it can race
        // the tail end of *this* readiness wait). Whatever killed the
        // process already removed it from `state.children`, so there is
        // nothing left there to untrack — but the window this call just
        // opened is real and would otherwise be a live, fully-untracked
        // (absent from both `AttachManager` and `state.children`) window for
        // a front nothing supervises anymore. Destroy it (not `.close()` —
        // this window already has the `on_window_event` handler below
        // registered, and `close()` re-emits `CloseRequested`, which that
        // handler would receive and react to; `destroy()` tears the window
        // down without emitting anything, so there's no double-cleanup or
        // reentrancy to reason about here) and clear the on-disk front
        // record rather than leaving that behind.
        let _ = window.destroy();
        kill_and_untrack(state, workspace_id, port);
        return Err(format!(
            "attach to '{workspace_id}' was cancelled by a concurrent detach/quit"
        ));
    }
    // The child is already tracked in `state.children` (inserted right
    // after spawn, above) — nothing more to do there on the success path.

    spawn_monitor(app.clone(), workspace_id.to_string(), port, generation);

    Ok(AttachOutcome::Opened)
}

/// Kill and untrack `workspace_id`'s child on an attach failure: removes it
/// from `state.children` (so nothing later mistakes it for still tracked),
/// kills and reaps the process, and clears its on-disk front record. Used
/// instead of a bare `child.kill()` because the child is tracked in the
/// shared map from spawn time onward (see [`attach_and_open_window`]'s doc
/// comment) — killing it locally without untracking would leave a dangling
/// map entry for an already-dead process.
fn kill_and_untrack(state: &AppState, workspace_id: &str, port: u16) {
    if let Some(mut child) = locked(&state.children).remove(workspace_id) {
        let _ = child.kill();
        let _ = child.wait();
    }
    remove_front_record(workspace_id, port);
}

/// Detach `workspace_id`: kill its front process, clear bookkeeping, close
/// its window (ADR 0097 Broker §4 — "Detach/quit terminates the front
/// process").
#[tauri::command(async)]
pub fn detach(
    workspace_id: String,
    app: AppHandle,
    state: State<'_, AppState>,
) -> Result<(), String> {
    detach_internal(&app, &state, &workspace_id)
}

/// Shared by the `detach` command, "quit" (detach-all), and a workspace
/// window's own `CloseRequested` handler, so every path that ends an
/// attachment does the same three things: kill the process, clear
/// bookkeeping, close the window.
///
/// Uses `WebviewWindow::destroy()`, not `.close()`, deliberately: `close()`'s
/// own docs say it "emits [`WindowEvent::CloseRequested`] first... so you can
/// intercept it" — since this same function is the *handler* registered for
/// that event (below, in `attach_and_open_window`), calling `.close()` here
/// would re-emit `CloseRequested` for a window already in the middle of
/// handling it, recursively re-invoking this function for as long as the
/// event keeps re-firing rather than actually tearing the window down.
/// `destroy()` "does not emit any events and force close[s] the window
/// instead" (same source), which is exactly what every caller here wants —
/// none of them need the "an outside observer can still intercept/cancel
/// this close" semantics `close()` exists for.
pub fn detach_internal(
    app: &AppHandle,
    state: &AppState,
    workspace_id: &str,
) -> Result<(), String> {
    let removed = locked(&state.attach).remove(workspace_id);

    if let Some(mut child) = locked(&state.children).remove(workspace_id) {
        let _ = child.kill();
        let _ = child.wait();
    }

    // Removing the on-disk record — which also removes the front's
    // RELEASE_TMP directory on Windows (BT-3046) — must happen *after* the
    // child is confirmed dead above: while still running, the front holds
    // files open under RELEASE_TMP, so an earlier removal would plausibly
    // fail with a sharing violation and leave the secrets directory behind.
    // Matches `kill_and_untrack`'s already-correct kill-then-untrack order.
    if let Some(front) = removed {
        if let Ok(dir) = reap::state_dir() {
            let _ = reap::remove_record(&dir, workspace_id, front.port);
        }
    }

    let label = window_label(workspace_id);
    if let Some(window) = app.get_webview_window(&label) {
        let _ = window.destroy();
    }

    Ok(())
}

/// `beamtalk workspace create <id> --background --persistent` via the
/// installed CLI (ADR 0097 Broker §5 — the first-run empty state's
/// "create a workspace" action).
///
/// Validates `workspace_id` first (`beamtalk_desktop_shell::empty_state::validate_new_workspace_id`)
/// — unlike `attach`/`detach`, which only ever receive an id the picker
/// itself discovered, this command's id comes straight from a free-text
/// field the user typed into, so it's the one place in this file user input
/// reaches a CLI subprocess invocation without having passed through
/// `list_workspaces` first.
#[tauri::command(async)]
pub fn create_workspace(workspace_id: String) -> Result<(), String> {
    beamtalk_desktop_shell::empty_state::validate_new_workspace_id(&workspace_id)?;
    let cli_path = cli_ops::resolve_cli_path().map_err(|e| e.to_string())?;
    cli_ops::create_workspace(&cli_path, &workspace_id).map_err(|e| e.to_string())
}

/// Quit: detach every tracked workspace (kills every front process, not
/// just the picker), then exit the app.
#[tauri::command(async)]
pub fn quit(app: AppHandle, state: State<'_, AppState>) -> Result<(), String> {
    detach_all(&app, &state);
    app.exit(0);
    Ok(())
}

/// Detach every tracked workspace (kills every front process). Shared by the
/// `quit` command (the in-app "Quit" button) and `main.rs`'s
/// `RunEvent::ExitRequested` handler (OS-level quit — `Cmd-Q`, taskbar
/// close, `SIGTERM`, …) so a front process gets terminated the same way
/// regardless of *how* the app is asked to exit; without the latter, an
/// OS-level quit would bypass this cleanup entirely and leave attached
/// fronts running until the next broker restart's orphan sweep found them.
///
/// Deliberately iterates `state.children` — every spawned-and-tracked child,
/// including one whose attach is still in flight (claimed but not yet
/// recorded) — rather than [`beamtalk_desktop_shell::attach::AttachManager::attached_ids`],
/// which only lists fully-recorded attachments. Quitting mid-attach must
/// still kill that front within this session rather than leaving it for the
/// next restart's orphan sweep to find.
///
/// Uses [`locked`] (recovers from a poisoned `children` mutex rather than
/// giving up) precisely because this is the one path that could otherwise
/// clean up after a panic elsewhere left a front still attached — bailing
/// out here on a poisoned lock, as a plain `.lock()?` would, is the single
/// worst place in this file to do that: it would leak every still-attached
/// front for the rest of the OS process's life, on the exact path (quit)
/// meant to guarantee they get killed.
pub fn detach_all(app: &AppHandle, state: &AppState) {
    let ids: Vec<String> = locked(&state.children).keys().cloned().collect();
    for id in ids {
        let _ = detach_internal(app, state, &id);
    }
    // BT-3059: `detach_internal`'s per-workspace `RELEASE_TMP` cleanup now
    // runs on a background thread rather than blocking the loop above, but
    // both of this function's callers (`quit` below, and `main.rs`'s
    // `RunEvent::ExitRequested` handler) terminate the OS process shortly
    // after this returns — which would otherwise abandon that cleanup
    // mid-retry on every ordinary quit, silently defeating BT-3046's reason
    // for retrying in the first place (see
    // `reap::wait_for_release_tmp_cleanup`'s doc comment).
    reap::wait_for_release_tmp_cleanup();
}

fn emit_progress(app: &AppHandle, workspace_id: &str, stage: &str) {
    let _ = app.emit(
        "attach-progress",
        crate::dto::AttachProgressEvent {
            workspace_id: workspace_id.to_string(),
            stage: stage.to_string(),
        },
    );
}

/// Best-effort initial `node_name` guess for a just-spawned front, recorded
/// before its readiness (and thus its real epmd registration) is known.
///
/// Unix: `predict_node_name`'s pid-based prediction is verified correct
/// there (`beamtalk_desktop_broker::sname`'s module doc — `Child::id()` is
/// the same pid `System.pid()` reports, an unbroken `exec` chain), so it's
/// used directly.
///
/// Windows: the same prediction is *provably wrong* there (`Child::id()` is
/// `cmd.exe`'s pid, `bin\bt_attach.bat` can only run via a console-subsystem
/// wrapper — see `sname`'s module doc) — recording it anyway would look like
/// real data while being silently incorrect. `sname::pending_node_name`
/// records an explicit placeholder instead, corrected once
/// `attach_and_open_window` confirms readiness and can resolve the real
/// name via epmd (`update_windows_node_name_after_readiness` below).
fn initial_node_name(workspace_id: &str, pid: u32) -> String {
    let suffix = beamtalk_desktop_broker::sname::attach_node_suffix(workspace_id);
    #[cfg(windows)]
    {
        let _ = pid; // only meaningful to the (wrong, on Windows) pid-based prediction
        beamtalk_desktop_broker::sname::pending_node_name(&suffix)
    }
    #[cfg(not(windows))]
    {
        beamtalk_desktop_broker::sname::predict_node_name(&suffix, pid)
    }
}

fn persist_front_record(workspace_id: &str, port: u16, pid: u32) {
    let Ok(dir) = reap::state_dir() else {
        return;
    };
    let record = reap::FrontRecord {
        workspace_id: workspace_id.to_string(),
        port,
        pid,
        node_name: initial_node_name(workspace_id, pid),
        start_time: reap::read_start_time(pid),
    };
    let _ = reap::save_record(&dir, &record);
}

/// Correct a Windows front record's placeholder `node_name` once readiness
/// confirms the front is actually up (BT-3045) — see [`initial_node_name`]'s
/// doc comment for why the initial write can't have a real answer yet.
/// Best-effort and silent on failure everywhere (no epmd match yet, the
/// record already gone via a racing detach/quit, an I/O error): `node_name`
/// remains bookkeeping/display only (`beamtalk_desktop_broker::reap`'s sweep
/// keys entirely off `pid`), so there is nothing correctness-critical riding
/// on this succeeding, and `attach_and_open_window` should not fail (or even
/// log noisily) an otherwise-successful attach over it. Windows-only: the
/// call site gates the background thread that invokes this behind
/// `#[cfg(windows)]` too, since [`initial_node_name`] already recorded the
/// verified-correct value on Unix — there is nothing for this to correct
/// there.
///
/// `resolve_registered_node_name` below is deliberately *not* also passed
/// `pid` to disambiguate a same-suffix race (BT-3062 investigated this and
/// rejected it): `pid` here is `Child::id()` — `cmd.exe`'s pid on Windows,
/// per `sname`'s module doc comment — not the `System.pid()` epmd's real
/// registration actually embeds, so a pid-based check would never match on
/// this, the only platform this function runs on. See `sname`'s module doc
/// comment ("**Suffix-only matching**") for the full reasoning and the
/// residual race this leaves accepted rather than closed.
#[cfg(windows)]
fn update_windows_node_name_after_readiness(workspace_id: &str, port: u16, pid: u32) {
    let suffix = beamtalk_desktop_broker::sname::attach_node_suffix(workspace_id);
    let Ok(Some(node_name)) = beamtalk_desktop_broker::sname::resolve_registered_node_name(&suffix)
    else {
        return;
    };
    if let Ok(dir) = reap::state_dir() {
        let _ = reap::update_record_node_name(&dir, workspace_id, port, pid, &node_name);
    }
}

/// Undo [`persist_front_record`] on an attach failure that already killed
/// the child, so a clean in-session failure doesn't leave a record for a
/// process this broker itself just terminated.
fn remove_front_record(workspace_id: &str, port: u16) {
    let Ok(dir) = reap::state_dir() else {
        return;
    };
    let _ = reap::remove_record(&dir, workspace_id, port);
}

/// Post-attach monitoring (ADR 0097 Broker §3): periodically re-poll
/// `/readiness` and reflect transitions in the window (title prefix) and to
/// the picker frontend (an event), so a dead workspace shows as a clearly
/// disconnected window instead of the front's RPCs silently hanging or the
/// LiveView page filling with socket-error noise (spike criterion (f)).
///
/// Stops once `generation` is no longer the *current* attachment's
/// generation — checked via
/// [`beamtalk_desktop_shell::attach::AttachManager::is_current_front`], not
/// merely `is_attached` and not `port`. A bare id check would let a stale
/// monitor from a detach-then-re-attach cycle survive: the new attach starts
/// its own monitor, but the *old* monitor would see `is_attached` flip back
/// to `true` for the same id and wrongly keep polling the dead old port,
/// fighting the new monitor over the same window's title/events. `port`
/// alone isn't a reliable discriminator either — the OS's ephemeral-port
/// allocator can (rarely) hand the fresh attach the *same* port the old one
/// used, which a `(workspace_id, port)` check would then treat as still
/// current. `generation` (assigned once per claim, by
/// [`beamtalk_desktop_shell::attach::AttachManager::decide_and_claim`], and
/// never reused) doesn't have that gap.
fn spawn_monitor(app: AppHandle, workspace_id: String, port: u16, generation: u64) {
    std::thread::spawn(move || {
        let mut monitor = Monitor::new();
        loop {
            let still_current = {
                let state = app.state::<AppState>();
                locked(&state.attach).is_current_front(&workspace_id, generation)
            };
            if !still_current {
                return;
            }

            let timeouts = ProbeTimeouts::default_local();
            let mut probe = readiness::http_probe("127.0.0.1", port, timeouts);
            let outcome = probe(&ReadinessState::WaitingReadiness);
            let poll_outcome = match outcome {
                readiness::ProbeOutcome::ReadinessOk(v) => monitor::PollOutcome::ReadinessOk(v),
                readiness::ProbeOutcome::ReadinessError(reason) => {
                    monitor::PollOutcome::ReadinessError(reason)
                }
                readiness::ProbeOutcome::HttpDown => monitor::PollOutcome::Unreachable,
                // http_probe only performs the HTTP-up check while
                // Spawning/WaitingHttp; passing WaitingReadiness above
                // always takes the /readiness branch, so HttpUp cannot be
                // observed here.
                readiness::ProbeOutcome::HttpUp => {
                    unreachable!("http_probe(WaitingReadiness) never returns HttpUp")
                }
            };

            if let Some(change) = monitor.observe(poll_outcome) {
                if change.to == monitor::ConnectionState::FrontUnreachable {
                    // The front's own HTTP port stopped answering — reap it
                    // if the OS process has actually exited, so a crashed
                    // front doesn't sit as a zombie (Unix) for the rest of
                    // this session. Deliberately does *not* remove it from
                    // `AttachManager`/close the window — the disconnected
                    // state stays visibly reflected (ADR 0097 Broker §3)
                    // until the user detaches; this only reaps the exit
                    // status, it doesn't end the attachment.
                    reap_if_exited(&app, &workspace_id);
                }
                reflect_connection_state(&app, &workspace_id, &change.to);
            }

            std::thread::sleep(monitor::DEFAULT_POLL_INTERVAL);
        }
    });
}

/// `try_wait()` a tracked child so an already-exited front process doesn't
/// linger as a zombie (Unix) — best-effort, never removes bookkeeping (see
/// [`spawn_monitor`]'s call site).
fn reap_if_exited(app: &AppHandle, workspace_id: &str) {
    let state = app.state::<AppState>();
    if let Some(child) = locked(&state.children).get_mut(workspace_id) {
        let _ = child.try_wait();
    }
}

/// Best-effort: prefix the workspace window's title on disconnect/
/// unreachable, clear it on reconnect. A richer in-page banner (injecting
/// DOM into the LiveView page via `WebviewWindow::eval`) was deliberately
/// not attempted here — the front's own CSP headers are untested against
/// eval-injected content from this shell, and a wrong guess there is worse
/// than the plain, always-works title-bar signal. Also emits an event so the
/// picker window's own list can show a status badge even when the workspace
/// window isn't focused.
fn reflect_connection_state(
    app: &AppHandle,
    workspace_id: &str,
    connection_state: &monitor::ConnectionState,
) {
    let label = window_label(workspace_id);
    if let Some(window) = app.get_webview_window(&label) {
        let base_title = format!("Beamtalk — {workspace_id}");
        let title = match connection_state {
            monitor::ConnectionState::Connected(_) => base_title,
            monitor::ConnectionState::Disconnected(_) => {
                format!("[Disconnected] {base_title}")
            }
            monitor::ConnectionState::FrontUnreachable => {
                format!("[Unreachable] {base_title}")
            }
        };
        let _ = window.set_title(&title);
    }

    let _ = app.emit(
        "connection-state-changed",
        ConnectionStateChangedEvent {
            workspace_id: workspace_id.to_string(),
            state: ConnectionStateView::from(connection_state),
        },
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn locked_recovers_from_a_poisoned_mutex() {
        // The exact scenario `locked` exists to survive: some thread panics
        // while holding the lock (a bug elsewhere in a command handler, not
        // anything `locked` itself does), poisoning the `Mutex`. A plain
        // `.lock()?` would make this mutex permanently unusable for the rest
        // of the process's life; `locked` must instead keep handing back a
        // usable guard.
        let mutex = Mutex::new(vec![1, 2, 3]);

        std::thread::scope(|scope| {
            let handle = scope.spawn(|| {
                let mut guard = mutex.lock().unwrap();
                guard.push(4);
                panic!("simulated panic while holding the lock");
            });
            // The panic is expected and deliberately triggered — just
            // confirm the thread actually unwound rather than propagating
            // it into this test.
            assert!(handle.join().is_err());
        });

        assert!(mutex.is_poisoned());

        // Recovers the guard *and* carries forward the in-progress mutation
        // (`push(4)`) rather than discarding it — `into_inner()` hands back
        // the data exactly as the panicking thread left it, which is the
        // whole basis for `locked`'s doc comment claim that the protected
        // data is "usually still perfectly usable after a panic".
        let guard = locked(&mutex);
        assert_eq!(*guard, vec![1, 2, 3, 4]);
    }
}
