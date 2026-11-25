// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Supervision for the LSP's detached background tasks (BT-5188).
//!
//! The runtime listeners (flush events, class-changed events) and the
//! cross-file republish worker run as spawned tasks, outside the per-request
//! panic guard. Run once under [`super::guard::guard_notification`], a panic
//! was logged and the task was gone for the rest of the session. [`supervise`]
//! restarts the task instead, with exponential backoff so a task that panics
//! on every start cannot spin.
//!
//! A listener's channel receiver must outlive one run of it, so listeners
//! take a [`SharedReceiver`] and lock it for the duration of a run; a panic
//! drops the guard and the next run picks up with the next event.

use super::guard::panic_message;
use futures_util::FutureExt;
use std::future::Future;
use std::panic::AssertUnwindSafe;
use std::sync::Arc;
use std::time::Duration;

/// A channel receiver shared across restarts of the listener that drains it.
pub(in crate::server) type SharedReceiver<E> =
    Arc<tokio::sync::Mutex<tokio::sync::mpsc::UnboundedReceiver<E>>>;

/// Wraps `rx` for a supervised listener.
pub(in crate::server) fn shared<E>(
    rx: tokio::sync::mpsc::UnboundedReceiver<E>,
) -> SharedReceiver<E> {
    Arc::new(tokio::sync::Mutex::new(rx))
}

/// Delay before the first restart after a panic.
const INITIAL_BACKOFF: Duration = Duration::from_millis(100);
/// Longest delay between restarts.
const MAX_BACKOFF: Duration = Duration::from_secs(30);
/// A run that lasted this long before panicking was healthy: the next
/// restart starts again from [`INITIAL_BACKOFF`].
const HEALTHY_RUN: Duration = Duration::from_mins(1);

/// Runs the task `start` builds until it returns normally (for a listener:
/// its channel closed), restarting it after a panic. Each panic is logged
/// with the task's `name`; restarts back off exponentially from
/// [`INITIAL_BACKOFF`] to [`MAX_BACKOFF`].
pub(in crate::server) async fn supervise<F, Fut>(name: &'static str, mut start: F)
where
    F: FnMut() -> Fut,
    Fut: Future<Output = ()>,
{
    let mut backoff = INITIAL_BACKOFF;
    loop {
        let started = tokio::time::Instant::now();
        match AssertUnwindSafe(start()).catch_unwind().await {
            Ok(()) => return,
            Err(payload) => {
                if started.elapsed() >= HEALTHY_RUN {
                    backoff = INITIAL_BACKOFF;
                }
                tracing::error!(
                    task = name,
                    panic = %panic_message(payload.as_ref()),
                    restart_in_ms = u64::try_from(backoff.as_millis()).unwrap_or(u64::MAX),
                    "LSP background task panicked; restarting"
                );
                tokio::time::sleep(backoff).await;
                backoff = (backoff * 2).min(MAX_BACKOFF);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// BT-5188: a listener that panics on one event is restarted and serves
    /// the events after it.
    #[tokio::test]
    async fn panicking_listener_restarts_and_serves_later_events() {
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<&'static str>();
        let rx = shared(rx);
        let served = Arc::new(std::sync::Mutex::new(Vec::new()));
        let starts = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        tx.send("first").expect("send");
        tx.send("boom").expect("send");
        tx.send("after").expect("send");
        drop(tx);
        let (rx2, served2, starts2) = (Arc::clone(&rx), Arc::clone(&served), Arc::clone(&starts));
        tokio::time::timeout(
            Duration::from_secs(5),
            supervise("test_listener", move || {
                let (rx, served) = (Arc::clone(&rx2), Arc::clone(&served2));
                starts2.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                async move {
                    let mut rx = rx.lock().await;
                    while let Some(event) = rx.recv().await {
                        assert_ne!(event, "boom", "injected listener panic");
                        served.lock().expect("lock").push(event);
                    }
                }
            }),
        )
        .await
        .expect("supervise returns once the channel closes");
        assert_eq!(*served.lock().expect("lock"), vec!["first", "after"]);
        assert_eq!(starts.load(std::sync::atomic::Ordering::SeqCst), 2);
    }

    /// Restarts back off: a task that panics on every start is not spun.
    #[tokio::test(start_paused = true)]
    async fn restarts_back_off_exponentially() {
        let starts = Arc::new(std::sync::Mutex::new(Vec::new()));
        let starts2 = Arc::clone(&starts);
        let task = tokio::spawn(supervise("always_panics", move || {
            starts2
                .lock()
                .expect("lock")
                .push(tokio::time::Instant::now());
            async { panic!("always") }
        }));
        tokio::time::sleep(Duration::from_secs(2)).await;
        task.abort();
        let starts = starts.lock().expect("lock").clone();
        let gaps: Vec<Duration> = starts.windows(2).map(|w| w[1] - w[0]).collect();
        assert!(gaps.len() >= 3, "restarted several times: {gaps:?}");
        for pair in gaps.windows(2) {
            assert!(pair[1] >= pair[0] * 2, "each delay doubles: {gaps:?}");
        }
    }
}
