// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Two-stage readiness probe (ADR 0097 Broker §2/§3).
//!
//! An external, non-BEAM broker cannot trigger the front's lazy
//! `connect/0` from outside the VM, so readiness is checked in two stages:
//!
//! 1. Poll the HTTP port until Phoenix answers at all (`wait_http_up`).
//! 2. `GET /readiness` (BT-2983), which forces `connect/0` plus one cheap RPC
//!    and returns `200` with a version report only when the workspace is
//!    truly reachable, or `503` with a `reason` distinguishing `epmd_absent`
//!    / `bad_cookie` / `dead_workspace` (`BtAttach.Workspace.readiness/0`'s
//!    taxonomy) — so a bad cookie or dead workspace surfaces before the
//!    window opens, not on the user's first eval.
//!
//! The state machine ([`ReadinessState`], [`advance`]) is deliberately pure —
//! it takes a [`ProbeOutcome`] the caller already obtained, rather than doing
//! I/O itself — so its transition logic is unit-testable without a real HTTP
//! server. [`wait_ready`] is the impure driver that actually polls.
//!
//! No HTTP client dependency: this is one GET request to a server this
//! broker itself just spawned on loopback, so a minimal HTTP/1.1 client over
//! `TcpStream` (in the spirit of `beamtalk_workspace::epmd`'s raw-protocol
//! approach, rather than pulling in a full client crate for one call site)
//! is enough.
//!
//! **DDD Context:** Desktop Shell

use std::io::{ErrorKind, Read, Write};
use std::net::TcpStream;
use std::time::{Duration, Instant};

use serde::Deserialize;

/// The workspace's version report (mirrors Erlang `beamtalk_version:get/0`,
/// BT-2991) — carried in a successful `/readiness` response so the front can
/// warn/refuse on a runtime/protocol mismatch (ADR 0097 Consequences,
/// "version skew").
///
/// Every field defaults to `"unknown"` (mirroring `beamtalk_version:get/0`'s
/// own `app_vsn/1` fallback for an unloaded application) rather than failing
/// to deserialize when absent. This matters for the exact scenario the
/// version handshake exists to handle: an older or future workspace whose
/// version map is missing or has renamed a field. The front already reports
/// `200` purely on `is_map/1` (`readiness_rpc/0` in `workspace.ex`), so the
/// workspace genuinely *is* reachable in that case — a hard deserialize
/// failure here would turn `ReadinessOk` into `Failed`, reporting a reachable
/// workspace as unreachable, which is the opposite of the ADR's stated
/// "warn/refuse on mismatch" (not "treat as unreachable") intent.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct VersionReport {
    #[serde(default = "unknown_version_field")]
    pub runtime_version: String,
    #[serde(default = "unknown_version_field")]
    pub protocol_version: String,
    #[serde(default = "unknown_version_field")]
    pub otp_release: String,
    #[serde(default = "unknown_version_field")]
    pub erts_version: String,
}

fn unknown_version_field() -> String {
    "unknown".to_string()
}

/// The `/readiness` failure taxonomy (`BtAttach.Workspace.readiness/0`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FailureReason {
    /// This front's own dist node couldn't publish itself to the local epmd.
    EpmdAbsent,
    /// epmd knows the target node; the dist handshake was rejected
    /// (mismatched cookie).
    BadCookie,
    /// epmd has no record of the target node, or it died between connect and
    /// the version RPC.
    DeadWorkspace,
    /// A reason string outside the three known buckets — kept rather than
    /// discarded so an older/future front's taxonomy change doesn't crash
    /// the broker, just surfaces an unrecognized string.
    Unknown(String),
}

impl From<&str> for FailureReason {
    fn from(s: &str) -> Self {
        match s {
            "epmd_absent" => Self::EpmdAbsent,
            "bad_cookie" => Self::BadCookie,
            "dead_workspace" => Self::DeadWorkspace,
            other => Self::Unknown(other.to_string()),
        }
    }
}

/// The stage a probe outcome pertains to — carried by [`ReadinessState::TimedOut`]
/// so a caller can tell "never got HTTP up" from "HTTP was up but /readiness
/// never returned 200" (very different diagnostics).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Stage {
    Http,
    Readiness,
}

/// One probe result, as observed by the (impure) caller — the input to the
/// pure [`advance`] transition function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProbeOutcome {
    /// The HTTP port refused/timed out.
    HttpDown,
    /// The HTTP port answered (any valid HTTP response — Phoenix is up).
    HttpUp,
    /// `GET /readiness` returned 200.
    ReadinessOk(VersionReport),
    /// `GET /readiness` returned 503 (or the connection otherwise failed
    /// after HTTP was confirmed up — e.g. the front crashed between stages).
    ReadinessError(FailureReason),
}

/// State of a two-stage readiness probe.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReadinessState {
    /// Front process spawned; no probe run yet.
    Spawning,
    /// Waiting for the HTTP port to come up.
    WaitingHttp,
    /// HTTP is up; waiting for `/readiness` to report 200.
    WaitingReadiness,
    /// Workspace confirmed reachable.
    Ready(VersionReport),
    /// `/readiness` reported a definite failure.
    Failed(FailureReason),
    /// Gave up waiting at `stage` (timeout, not a definite failure — the
    /// front might still come up later; distinct from [`ReadinessState::Failed`],
    /// which is a stable, informative answer from the front itself).
    TimedOut(Stage),
}

/// Pure state transition: given the current state and one probe outcome,
/// compute the next state. Never does I/O — see the module docs for why
/// that's deliberate.
///
/// `HttpDown`/`ReadinessError` observed in a state that isn't waiting for
/// them (e.g. a stray `HttpDown` after `Ready`) leave the state unchanged —
/// once ready or definitively failed, only a fresh probe cycle
/// ([`ReadinessState::WaitingHttp`]) should re-evaluate; a single stale
/// negative probe must not regress a state the caller may already have
/// acted on (e.g. opened a window). The same catch-all also covers a stray
/// `HttpUp` while already `WaitingReadiness` (a no-op — already progressed
/// past that stage).
#[must_use]
pub fn advance(state: ReadinessState, outcome: ProbeOutcome) -> ReadinessState {
    match (state, outcome) {
        (ReadinessState::Spawning | ReadinessState::WaitingHttp, ProbeOutcome::HttpUp) => {
            ReadinessState::WaitingReadiness
        }
        (ReadinessState::WaitingReadiness, ProbeOutcome::ReadinessOk(version)) => {
            ReadinessState::Ready(version)
        }
        (ReadinessState::WaitingReadiness, ProbeOutcome::ReadinessError(reason)) => {
            ReadinessState::Failed(reason)
        }
        // Front unreachable while spawning/waiting for HTTP, OR HTTP going
        // down again while waiting on readiness (front crashed between the
        // two stages) — both land back on WaitingHttp, not a silent no-op.
        (
            ReadinessState::Spawning
            | ReadinessState::WaitingHttp
            | ReadinessState::WaitingReadiness,
            ProbeOutcome::HttpDown,
        ) => ReadinessState::WaitingHttp,
        // Terminal/unrelated combinations (including a stray HttpUp while
        // already WaitingReadiness): hold steady.
        (state, _) => state,
    }
}

/// Poll `state` forward with `probe` (real I/O) until it reaches [`ReadinessState::Ready`],
/// [`ReadinessState::Failed`], or `overall_timeout` elapses (→ [`ReadinessState::TimedOut`]).
///
/// `probe` returns the next [`ProbeOutcome`] given the *current* state (so it
/// knows whether to hit `/` or `/readiness`) — kept generic so tests can
/// inject canned outcomes instead of real HTTP.
pub fn wait_ready(
    mut state: ReadinessState,
    overall_timeout: Duration,
    poll_interval: Duration,
    mut probe: impl FnMut(&ReadinessState) -> ProbeOutcome,
) -> ReadinessState {
    let deadline = Instant::now() + overall_timeout;
    loop {
        if matches!(state, ReadinessState::Ready(_) | ReadinessState::Failed(_)) {
            return state;
        }
        if Instant::now() >= deadline {
            let timed_out_stage = if matches!(state, ReadinessState::WaitingReadiness) {
                Stage::Readiness
            } else {
                Stage::Http
            };
            return ReadinessState::TimedOut(timed_out_stage);
        }
        let outcome = probe(&state);
        state = advance(state, outcome);
        if !matches!(state, ReadinessState::Ready(_) | ReadinessState::Failed(_)) {
            std::thread::sleep(poll_interval);
        }
    }
}

/// Raw HTTP/1.1 GET over a fresh `TcpStream` — connect, send a
/// `Connection: close` request, read to EOF, split status line from body.
///
/// Returns `Err` for connection failure (port not up yet) or a response that
/// doesn't parse as HTTP at all; a successful parse — even a non-2xx status —
/// is `Ok`, since a well-formed 503 with a JSON `reason` body is exactly the
/// expected shape for a failed `/readiness` probe.
fn http_get(
    host: &str,
    port: u16,
    path: &str,
    timeout: Duration,
) -> std::io::Result<(u16, String)> {
    let addr = format!("{host}:{port}");
    let socket_addr = std::net::ToSocketAddrs::to_socket_addrs(&addr)?
        .next()
        .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::InvalidInput, "no addr"))?;
    let mut stream = TcpStream::connect_timeout(&socket_addr, timeout)?;
    stream.set_write_timeout(Some(timeout))?;

    let request = format!("GET {path} HTTP/1.1\r\nHost: {host}\r\nConnection: close\r\n\r\n");
    stream.write_all(request.as_bytes())?;

    // Connection: close means the server closes its end when done writing —
    // read to EOF rather than tracking Content-Length, which is simpler and
    // correct for the small JSON bodies /readiness and / return.
    //
    // Deliberately not a plain `read_to_end`: `set_read_timeout` bounds a
    // single `read()` call, not the cumulative loop `read_to_end` makes
    // internally — a peer that dribbles bytes with gaps just under `timeout`
    // could make this call run far longer than `timeout` in total, which
    // would blow past the overall deadline `wait_ready`'s caller tracks
    // across polls. Re-deriving the per-read timeout from a fixed overall
    // deadline bounds the *whole* read to `timeout`, not just each syscall.
    let deadline = Instant::now() + timeout;
    let mut raw = Vec::new();
    let mut buf = [0u8; 8192];
    loop {
        let remaining = deadline.saturating_duration_since(Instant::now());
        if remaining.is_zero() {
            break;
        }
        stream.set_read_timeout(Some(remaining))?;
        match stream.read(&mut buf) {
            Ok(0) => break, // EOF: peer closed, response complete
            Ok(n) => raw.extend_from_slice(&buf[..n]),
            // A read timeout keeps whatever was already accumulated in `raw`
            // rather than discarding it as an error — a delayed-but-eventual
            // close (or a future proxy/keep-alive peer that doesn't honor
            // `Connection: close`) must not turn an otherwise-healthy,
            // already-fully-received response into a false HttpDown.
            Err(e) if e.kind() == ErrorKind::WouldBlock || e.kind() == ErrorKind::TimedOut => {
                break;
            }
            Err(e) => return Err(e),
        }
    }
    let text = String::from_utf8_lossy(&raw);

    let mut parts = text.splitn(2, "\r\n\r\n");
    let head = parts.next().unwrap_or_default();
    let body = parts.next().unwrap_or_default();

    let status_line = head.lines().next().unwrap_or_default();
    let status: u16 = status_line
        .split_whitespace()
        .nth(1)
        .and_then(|s| s.parse().ok())
        .ok_or_else(|| {
            std::io::Error::new(std::io::ErrorKind::InvalidData, "no HTTP status line")
        })?;

    Ok((status, body.to_string()))
}

/// Body shape of a successful `/readiness` response.
#[derive(Debug, Deserialize)]
struct OkBody {
    version: VersionReport,
}

/// Body shape of a failed `/readiness` response.
#[derive(Debug, Deserialize)]
struct ErrBody {
    reason: String,
}

/// Timeouts for the two probe stages — deliberately **separate**, not one
/// shared duration, because the stages have very different latency profiles.
///
/// The HTTP-up check is a cheap local TCP round trip to a server this broker
/// just spawned — a couple of seconds is generous. `/readiness` is not: it
/// forces the front's `connect/0`, and on a bad cookie that blocks for
/// Erlang's connection-setup timeout (`net_kernel`'s `net_setuptime`, 7s by
/// default) before the front can even answer `503`. A single timeout tuned
/// for the cheap HTTP-up check would make the `/readiness` read time out
/// *first*, which `http_probe` reports as [`ProbeOutcome::HttpDown`] — and
/// [`advance`] responds to that by regressing `WaitingReadiness` back to
/// `WaitingHttp`, so the definitive `Failed(BadCookie)` answer never
/// surfaces. That silently defeats the two-stage probe's whole purpose
/// (surfacing a bad cookie / dead workspace before the window opens).
#[derive(Debug, Clone, Copy)]
pub struct ProbeTimeouts {
    /// Budget for the HTTP-up check against `/`.
    pub http_up: Duration,
    /// Budget for `GET /readiness`. Must comfortably exceed the front's
    /// worst-case `connect/0` latency (the bad-cookie path above), not just
    /// a healthy round trip.
    pub readiness: Duration,
}

impl ProbeTimeouts {
    /// 2s for the HTTP-up check (a healthy, already-spawned local Phoenix
    /// answers near-instantly); 10s for `/readiness`, comfortably above
    /// Erlang's default 7s `net_setuptime` so a bad-cookie 503 has time to
    /// actually arrive.
    #[must_use]
    pub fn default_local() -> Self {
        Self {
            http_up: Duration::from_secs(2),
            readiness: Duration::from_secs(10),
        }
    }
}

/// Real probe: HTTP-up check against `/`, or `/readiness` depending on
/// `state`. The [`wait_ready`] driver to use outside tests.
#[must_use = "returns a probe closure — call it (typically via wait_ready) rather than discarding it"]
pub fn http_probe(
    host: &str,
    port: u16,
    timeouts: ProbeTimeouts,
) -> impl FnMut(&ReadinessState) -> ProbeOutcome + '_ {
    move |state| match state {
        ReadinessState::WaitingReadiness => {
            match http_get(host, port, "/readiness", timeouts.readiness) {
                Ok((200, body)) => match serde_json::from_str::<OkBody>(&body) {
                    Ok(ok) => ProbeOutcome::ReadinessOk(ok.version),
                    Err(_) => ProbeOutcome::ReadinessError(FailureReason::Unknown(
                        "malformed 200 response body".to_string(),
                    )),
                },
                Ok((_status, body)) => match serde_json::from_str::<ErrBody>(&body) {
                    Ok(err) => {
                        ProbeOutcome::ReadinessError(FailureReason::from(err.reason.as_str()))
                    }
                    Err(_) => ProbeOutcome::ReadinessError(FailureReason::Unknown(
                        "malformed error response body".to_string(),
                    )),
                },
                Err(_) => ProbeOutcome::HttpDown,
            }
        }
        _ => match http_get(host, port, "/", timeouts.http_up) {
            Ok(_) => ProbeOutcome::HttpUp,
            Err(_) => ProbeOutcome::HttpDown,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::net::TcpListener;
    use std::thread;

    /// Symmetric `ProbeTimeouts` for tests that don't care about the
    /// http_up-vs-readiness split — see `probe_timeouts_use_the_right_stage`
    /// below for a test that specifically exercises the split.
    fn short_timeouts(d: Duration) -> ProbeTimeouts {
        ProbeTimeouts {
            http_up: d,
            readiness: d,
        }
    }

    fn version() -> VersionReport {
        VersionReport {
            runtime_version: "0.4.0".to_string(),
            protocol_version: "2.0".to_string(),
            otp_release: "27".to_string(),
            erts_version: "15.0".to_string(),
        }
    }

    // ── pure state machine ──────────────────────────────────────────────

    #[test]
    fn spawning_to_waiting_http_on_http_down() {
        let next = advance(ReadinessState::Spawning, ProbeOutcome::HttpDown);
        assert_eq!(next, ReadinessState::WaitingHttp);
    }

    #[test]
    fn waiting_http_to_waiting_readiness_on_http_up() {
        let next = advance(ReadinessState::WaitingHttp, ProbeOutcome::HttpUp);
        assert_eq!(next, ReadinessState::WaitingReadiness);
    }

    #[test]
    fn waiting_readiness_to_ready_on_readiness_ok() {
        let next = advance(
            ReadinessState::WaitingReadiness,
            ProbeOutcome::ReadinessOk(version()),
        );
        assert_eq!(next, ReadinessState::Ready(version()));
    }

    #[test]
    fn waiting_readiness_to_failed_on_readiness_error() {
        let next = advance(
            ReadinessState::WaitingReadiness,
            ProbeOutcome::ReadinessError(FailureReason::BadCookie),
        );
        assert_eq!(next, ReadinessState::Failed(FailureReason::BadCookie));
    }

    /// Regression test found in adversarial review: a version report missing
    /// a field (older/future workspace, renamed field) must still parse —
    /// the front already only requires `is_map/1` to answer `200`, so a hard
    /// deserialize failure here would misreport a reachable workspace as
    /// unreachable.
    #[test]
    fn version_report_tolerates_missing_fields() {
        let json = r#"{"runtime_version":"0.4.0","protocol_version":"2.0"}"#;
        let report: VersionReport = serde_json::from_str(json).unwrap();
        assert_eq!(report.runtime_version, "0.4.0");
        assert_eq!(report.protocol_version, "2.0");
        assert_eq!(report.otp_release, "unknown");
        assert_eq!(report.erts_version, "unknown");
    }

    #[test]
    fn version_report_tolerates_a_completely_empty_map() {
        let report: VersionReport = serde_json::from_str("{}").unwrap();
        assert_eq!(report.runtime_version, "unknown");
        assert_eq!(report.protocol_version, "unknown");
        assert_eq!(report.otp_release, "unknown");
        assert_eq!(report.erts_version, "unknown");
    }

    #[test]
    fn waiting_readiness_regresses_to_waiting_http_if_http_drops() {
        // Front crashed between the two stages.
        let next = advance(ReadinessState::WaitingReadiness, ProbeOutcome::HttpDown);
        assert_eq!(next, ReadinessState::WaitingHttp);
    }

    #[test]
    fn ready_is_terminal_and_ignores_further_probes() {
        let ready = ReadinessState::Ready(version());
        assert_eq!(advance(ready.clone(), ProbeOutcome::HttpDown), ready);
        assert_eq!(
            advance(
                ready.clone(),
                ProbeOutcome::ReadinessError(FailureReason::DeadWorkspace)
            ),
            ready
        );
    }

    #[test]
    fn failed_is_terminal_and_ignores_further_probes() {
        let failed = ReadinessState::Failed(FailureReason::EpmdAbsent);
        assert_eq!(advance(failed.clone(), ProbeOutcome::HttpUp), failed);
        assert_eq!(
            advance(failed.clone(), ProbeOutcome::ReadinessOk(version())),
            failed
        );
    }

    #[test]
    fn failure_reason_taxonomy_from_str() {
        assert_eq!(
            FailureReason::from("epmd_absent"),
            FailureReason::EpmdAbsent
        );
        assert_eq!(FailureReason::from("bad_cookie"), FailureReason::BadCookie);
        assert_eq!(
            FailureReason::from("dead_workspace"),
            FailureReason::DeadWorkspace
        );
        assert_eq!(
            FailureReason::from("something_new"),
            FailureReason::Unknown("something_new".to_string())
        );
    }

    // ── wait_ready driver (injected outcomes, no real I/O) ──────────────

    #[test]
    fn wait_ready_reaches_ready_via_injected_outcomes() {
        let mut outcomes = vec![
            ProbeOutcome::HttpDown,
            ProbeOutcome::HttpUp,
            ProbeOutcome::ReadinessOk(version()),
        ]
        .into_iter();

        let final_state = wait_ready(
            ReadinessState::Spawning,
            Duration::from_secs(5),
            Duration::from_millis(1),
            move |_state| outcomes.next().expect("ran out of canned outcomes"),
        );
        assert_eq!(final_state, ReadinessState::Ready(version()));
    }

    #[test]
    fn wait_ready_stops_immediately_on_failed() {
        let final_state = wait_ready(
            ReadinessState::WaitingReadiness,
            Duration::from_secs(5),
            Duration::from_millis(1),
            move |_state| ProbeOutcome::ReadinessError(FailureReason::DeadWorkspace),
        );
        assert_eq!(
            final_state,
            ReadinessState::Failed(FailureReason::DeadWorkspace)
        );
    }

    #[test]
    fn wait_ready_times_out_at_http_stage() {
        let final_state = wait_ready(
            ReadinessState::Spawning,
            Duration::from_millis(20),
            Duration::from_millis(5),
            |_state| ProbeOutcome::HttpDown,
        );
        assert_eq!(final_state, ReadinessState::TimedOut(Stage::Http));
    }

    #[test]
    fn wait_ready_times_out_at_readiness_stage_when_no_outcome_resolves() {
        let final_state = wait_ready(
            ReadinessState::WaitingReadiness,
            Duration::from_millis(20),
            Duration::from_millis(5),
            |_state| ProbeOutcome::HttpUp, // no-op while WaitingReadiness
        );
        assert_eq!(final_state, ReadinessState::TimedOut(Stage::Readiness));
    }

    // ── raw HTTP client against a real loopback listener ────────────────

    fn spawn_canned_http_server(response: &'static str) -> u16 {
        let listener = TcpListener::bind(("127.0.0.1", 0)).unwrap();
        let port = listener.local_addr().unwrap().port();
        thread::spawn(move || {
            if let Ok((mut socket, _)) = listener.accept() {
                let mut buf = [0u8; 1024];
                let _ = socket.read(&mut buf); // drain the request
                let _ = socket.write_all(response.as_bytes());
            }
        });
        port
    }

    #[test]
    fn http_get_parses_a_200_response() {
        let port = spawn_canned_http_server(
            "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nConnection: close\r\n\r\n{\"status\":\"ok\"}",
        );
        let (status, body) =
            http_get("127.0.0.1", port, "/readiness", Duration::from_secs(2)).unwrap();
        assert_eq!(status, 200);
        assert_eq!(body, "{\"status\":\"ok\"}");
    }

    #[test]
    fn http_get_parses_a_503_response() {
        let port = spawn_canned_http_server(
            "HTTP/1.1 503 Service Unavailable\r\nConnection: close\r\n\r\n{\"status\":\"error\",\"reason\":\"bad_cookie\"}",
        );
        let (status, body) =
            http_get("127.0.0.1", port, "/readiness", Duration::from_secs(2)).unwrap();
        assert_eq!(status, 503);
        assert!(body.contains("bad_cookie"));
    }

    /// Regression test for a real bug found in review: `set_read_timeout`
    /// only bounds a single `read()` syscall, not the cumulative time
    /// `read_to_end` spends looping — a peer that dribbles bytes with gaps
    /// just under the per-read timeout could make one `http_get` call run
    /// far longer than its `timeout` argument. This delivers the response in
    /// two chunks with a deliberate gap, well within the overall timeout, to
    /// confirm slow-but-complete delivery still parses correctly (the fix
    /// must not break the legitimate slow-server case while bounding the
    /// pathological one below).
    #[test]
    fn http_get_assembles_a_response_delivered_in_slow_chunks() {
        let listener = TcpListener::bind(("127.0.0.1", 0)).unwrap();
        let port = listener.local_addr().unwrap().port();
        thread::spawn(move || {
            if let Ok((mut socket, _)) = listener.accept() {
                let mut buf = [0u8; 1024];
                let _ = socket.read(&mut buf);
                let _ = socket.write_all(b"HTTP/1.1 200 OK\r\nConnection: close\r\n\r\n{\"sta");
                thread::sleep(Duration::from_millis(80));
                let _ = socket.write_all(b"tus\":\"ok\"}");
                // socket drops here, closing the connection (EOF for the reader).
            }
        });

        let (status, body) =
            http_get("127.0.0.1", port, "/readiness", Duration::from_secs(2)).unwrap();
        assert_eq!(status, 200);
        assert_eq!(body, "{\"status\":\"ok\"}");
    }

    /// The pathological case the fix targets: a peer that writes a partial
    /// response and then never closes and never sends more data. Each
    /// individual `read()` will eventually time out, but the key property
    /// under test is that `http_get` as a *whole* returns at roughly
    /// `timeout`, not several multiples of it — proving the deadline is
    /// tracked across the read loop, not reset per read.
    ///
    /// The response body ends up incomplete either way (no blank line, no
    /// JSON body) — `http_get` itself has no way to know a response is
    /// "incomplete" vs. "a 200 with no body" (that distinction only exists
    /// once a caller tries to parse the body as JSON, exercised separately
    /// by `http_probe_treats_an_incomplete_response_as_a_readiness_error`
    /// below), so this test's only assertion is on timing.
    #[test]
    fn http_get_returns_within_the_overall_timeout_when_peer_never_closes() {
        let listener = TcpListener::bind(("127.0.0.1", 0)).unwrap();
        let port = listener.local_addr().unwrap().port();
        // Keep the accepted socket alive for the test's duration by parking
        // the thread instead of letting it (and the socket) drop — a
        // dropped socket would close the connection, defeating the test.
        thread::spawn(move || {
            if let Ok((mut socket, _)) = listener.accept() {
                let mut buf = [0u8; 1024];
                let _ = socket.read(&mut buf);
                let _ = socket.write_all(b"HTTP/1.1 200 OK\r\n"); // never sends the blank line
                thread::sleep(Duration::from_secs(5));
            }
        });

        let timeout = Duration::from_millis(200);
        let started = Instant::now();
        let _ = http_get("127.0.0.1", port, "/readiness", timeout);
        let elapsed = started.elapsed();

        assert!(
            elapsed < timeout * 3,
            "http_get took {elapsed:?} against a {timeout:?} timeout — the overall \
             deadline was not respected across the read loop"
        );
    }

    /// Companion to the timing test above: an incomplete response (no body)
    /// must not be mistaken for a successful, empty readiness report by the
    /// layer that actually interprets the body — `http_probe` — even though
    /// `http_get` itself just reports whatever bytes it got.
    #[test]
    fn http_probe_treats_an_incomplete_response_as_a_readiness_error() {
        let listener = TcpListener::bind(("127.0.0.1", 0)).unwrap();
        let port = listener.local_addr().unwrap().port();
        thread::spawn(move || {
            if let Ok((mut socket, _)) = listener.accept() {
                let mut buf = [0u8; 1024];
                let _ = socket.read(&mut buf);
                let _ = socket.write_all(b"HTTP/1.1 200 OK\r\n");
                thread::sleep(Duration::from_secs(5));
            }
        });

        let mut probe = http_probe(
            "127.0.0.1",
            port,
            short_timeouts(Duration::from_millis(200)),
        );
        let outcome = probe(&ReadinessState::WaitingReadiness);
        assert!(
            matches!(
                outcome,
                ProbeOutcome::ReadinessError(FailureReason::Unknown(_))
            ),
            "an incomplete 200 must not be treated as ReadinessOk: {outcome:?}"
        );
    }

    #[test]
    fn http_get_errors_when_nothing_is_listening() {
        // A closed loopback listener: bind then immediately drop, so the
        // port is (almost certainly) refused rather than accidentally reused.
        let listener = TcpListener::bind(("127.0.0.1", 0)).unwrap();
        let port = listener.local_addr().unwrap().port();
        drop(listener);

        let result = http_get("127.0.0.1", port, "/", Duration::from_millis(500));
        assert!(result.is_err());
    }

    #[test]
    fn http_probe_reports_readiness_ok_end_to_end() {
        let port = spawn_canned_http_server(
            "HTTP/1.1 200 OK\r\nConnection: close\r\n\r\n{\"status\":\"ok\",\"version\":{\"runtime_version\":\"0.4.0\",\"protocol_version\":\"2.0\",\"otp_release\":\"27\",\"erts_version\":\"15.0\"}}",
        );
        let mut probe = http_probe("127.0.0.1", port, ProbeTimeouts::default_local());
        let outcome = probe(&ReadinessState::WaitingReadiness);
        assert_eq!(outcome, ProbeOutcome::ReadinessOk(version()));
    }

    #[test]
    fn http_probe_reports_readiness_error_end_to_end() {
        let port = spawn_canned_http_server(
            "HTTP/1.1 503 Service Unavailable\r\nConnection: close\r\n\r\n{\"status\":\"error\",\"reason\":\"dead_workspace\"}",
        );
        let mut probe = http_probe("127.0.0.1", port, ProbeTimeouts::default_local());
        let outcome = probe(&ReadinessState::WaitingReadiness);
        assert_eq!(
            outcome,
            ProbeOutcome::ReadinessError(FailureReason::DeadWorkspace)
        );
    }

    #[test]
    fn http_probe_reports_http_up_for_root_path() {
        let port =
            spawn_canned_http_server("HTTP/1.1 200 OK\r\nConnection: close\r\n\r\n<html></html>");
        let mut probe = http_probe("127.0.0.1", port, ProbeTimeouts::default_local());
        let outcome = probe(&ReadinessState::WaitingHttp);
        assert_eq!(outcome, ProbeOutcome::HttpUp);
    }

    #[test]
    fn http_probe_reports_http_down_when_nothing_listening() {
        let listener = TcpListener::bind(("127.0.0.1", 0)).unwrap();
        let port = listener.local_addr().unwrap().port();
        drop(listener);

        let mut probe = http_probe(
            "127.0.0.1",
            port,
            short_timeouts(Duration::from_millis(300)),
        );
        let outcome = probe(&ReadinessState::WaitingHttp);
        assert_eq!(outcome, ProbeOutcome::HttpDown);
    }

    /// The finding this whole `ProbeTimeouts` split exists to fix: the
    /// `/readiness` stage must use its own (longer) timeout budget, not the
    /// HTTP-up one — proven here by giving them deliberately different
    /// values and confirming each stage respects its own.
    #[test]
    fn http_probe_uses_the_readiness_timeout_not_the_http_up_timeout() {
        // A peer that accepts but never writes anything: any read on it will
        // time out. http_up is generous (would happily wait past the test's
        // patience); readiness is short, so only the readiness-stage call
        // should time out quickly.
        let listener = TcpListener::bind(("127.0.0.1", 0)).unwrap();
        let port = listener.local_addr().unwrap().port();
        thread::spawn(move || {
            if let Ok((mut socket, _)) = listener.accept() {
                let mut buf = [0u8; 1024];
                let _ = socket.read(&mut buf);
                thread::sleep(Duration::from_secs(5)); // never responds
            }
        });

        let timeouts = ProbeTimeouts {
            http_up: Duration::from_secs(5),
            readiness: Duration::from_millis(150),
        };
        let mut probe = http_probe("127.0.0.1", port, timeouts);

        let started = Instant::now();
        let outcome = probe(&ReadinessState::WaitingReadiness);
        let elapsed = started.elapsed();

        assert_eq!(
            outcome,
            ProbeOutcome::HttpDown,
            "unresponsive peer times out as HttpDown"
        );
        assert!(
            elapsed < Duration::from_secs(1),
            "readiness probe took {elapsed:?} — used the 5s http_up timeout \
             instead of the 150ms readiness timeout"
        );
    }
}
