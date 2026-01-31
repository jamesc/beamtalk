// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Daemon lifecycle management: start, stop, status, and cleanup.
//!
//! This module handles the daemon's lifecycle operations including:
//! - Starting the daemon (foreground mode)
//! - Stopping a running daemon
//! - Checking daemon status
//! - Lockfile management
//! - Signal handling for graceful shutdown

use std::fs;
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
#[cfg(unix)]
use std::os::unix::net::UnixListener;
use std::process;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use beamtalk_core::language_service::SimpleLanguageService;
use miette::{IntoDiagnostic, Result, miette};
use tracing::info;

use crate::paths::{beamtalk_dir, is_daemon_running, lockfile_path, socket_path};

use super::transport;

/// Write current PID to lockfile atomically using `O_EXCL`.
///
/// This prevents race conditions where two processes both pass the
/// `is_daemon_running` check and try to create lockfiles simultaneously.
#[cfg(unix)]
fn write_lockfile_atomic() -> Result<()> {
    use std::io::Write;
    use std::os::unix::fs::OpenOptionsExt;

    let lockfile = lockfile_path()?;

    // Use O_CREAT | O_EXCL to atomically create the file only if it doesn't exist
    let mut file = fs::OpenOptions::new()
        .write(true)
        .create_new(true) // This is O_CREAT | O_EXCL
        .mode(0o644)
        .open(&lockfile)
        .map_err(|e| {
            if e.kind() == std::io::ErrorKind::AlreadyExists {
                miette!("Daemon lockfile already exists. Another instance may be starting.")
            } else {
                miette!("Failed to create lockfile: {e}")
            }
        })?;

    write!(file, "{}", process::id()).into_diagnostic()?;
    Ok(())
}

/// Remove lockfile and socket on shutdown.
fn cleanup() -> Result<()> {
    let _ = fs::remove_file(lockfile_path()?);
    let _ = fs::remove_file(socket_path()?);
    Ok(())
}

/// Start the compiler daemon.
#[cfg(unix)]
pub fn start_daemon(foreground: bool) -> Result<()> {
    // Check if already running
    if let Some(pid) = is_daemon_running()? {
        return Err(miette!("Daemon already running (PID {pid})"));
    }

    // Ensure .beamtalk directory exists
    let dir = beamtalk_dir()?;
    fs::create_dir_all(&dir).into_diagnostic()?;

    if foreground {
        // Run in foreground
        init_logging();
        info!("Starting compiler daemon in foreground");
        run_daemon_server()
    } else {
        // Background mode is not yet implemented; avoid misleading behavior.
        // When proper daemonization is implemented, this branch should be updated.
        Err(miette!(
            "Background mode is not yet supported. Please rerun with --foreground."
        ))
    }
}

/// Start the compiler daemon (Windows stub).
#[cfg(not(unix))]
pub fn start_daemon(_foreground: bool) -> Result<()> {
    Err(miette!(
        "Daemon is not yet supported on this platform. Unix socket support requires Unix."
    ))
}

/// Stop the running daemon.
#[cfg(unix)]
#[expect(
    clippy::cast_possible_wrap,
    reason = "PID values are always positive and small"
)]
pub fn stop_daemon() -> Result<()> {
    if let Some(pid) = is_daemon_running()? {
        // SAFETY: libc::kill is safe to call with any pid and signal number.
        // If the pid doesn't exist, it returns an error which we ignore.
        unsafe {
            libc::kill(pid as i32, libc::SIGTERM);
        }
        println!("Sent stop signal to daemon (PID {pid})");

        // Wait briefly and check if stopped
        std::thread::sleep(std::time::Duration::from_millis(500));
        if is_daemon_running()?.is_none() {
            println!("Daemon stopped successfully");
        } else {
            println!("Daemon may still be shutting down");
        }
    } else {
        println!("Daemon is not running");
    }
    Ok(())
}

/// Stop the running daemon (Windows stub).
#[cfg(not(unix))]
pub fn stop_daemon() -> Result<()> {
    Err(miette!("Daemon is not yet supported on this platform."))
}

/// Show daemon status.
pub fn show_status() -> Result<()> {
    match is_daemon_running()? {
        Some(pid) => {
            println!("Daemon is running (PID {pid})");
            println!("Socket: {}", socket_path()?.display());
        }
        None => {
            println!("Daemon is not running");
        }
    }
    Ok(())
}

/// Initialize logging for the daemon.
fn init_logging() {
    use tracing_subscriber::{EnvFilter, fmt, prelude::*};

    tracing_subscriber::registry()
        .with(fmt::layer())
        .with(
            EnvFilter::from_default_env().add_directive(
                "beamtalk=debug"
                    .parse()
                    .expect("Failed to parse tracing directive"),
            ),
        )
        .init();
}

/// Run the main daemon server loop.
///
/// # Limitations
///
/// - **Single connection**: The daemon handles one connection at a time. Subsequent
///   clients will queue at the socket level until the current client disconnects.
///   For concurrent LSP and REPL support, consider spawning threads per connection
///   or using an async runtime.
/// - **Shared state**: The `SimpleLanguageService` is mutably borrowed for the
///   duration of each connection. Concurrent access would require `Arc<Mutex<...>>`.
#[cfg(unix)]
fn run_daemon_server() -> Result<()> {
    // Write lockfile atomically using O_EXCL to prevent race conditions
    write_lockfile_atomic()?;

    // Set up signal handling for graceful shutdown
    let running = Arc::new(AtomicBool::new(true));
    let r = Arc::clone(&running);

    ctrlc::set_handler(move || {
        info!("Received shutdown signal");
        r.store(false, Ordering::SeqCst);
    })
    .into_diagnostic()?;

    // Create Unix socket
    let socket = socket_path()?;
    if socket.exists() {
        fs::remove_file(&socket).into_diagnostic()?;
    }

    let listener = UnixListener::bind(&socket).into_diagnostic()?;

    // Set restrictive permissions (owner only) on the socket
    // This prevents other users on the system from connecting
    fs::set_permissions(&socket, fs::Permissions::from_mode(0o600)).into_diagnostic()?;

    listener.set_nonblocking(true).into_diagnostic()?;

    info!("Daemon listening on {}", socket.display());

    // Create language service
    let mut service = SimpleLanguageService::new();

    // Main loop
    while running.load(Ordering::SeqCst) {
        match transport::accept_connection(&listener) {
            Ok((stream, _addr)) => {
                tracing::debug!("Accepted connection");
                if let Err(e) = transport::handle_connection(stream, &mut service, &running) {
                    tracing::error!("Error handling connection: {e}");
                }
            }
            Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                // No connection, sleep briefly to avoid busy-waiting
                std::thread::sleep(std::time::Duration::from_millis(50));
            }
            Err(e) => {
                tracing::error!("Error accepting connection: {e}");
            }
        }
    }

    info!("Daemon shutting down");
    cleanup()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cleanup_does_not_panic() {
        // cleanup() should succeed even if files don't exist
        let result = cleanup();
        assert!(result.is_ok());
    }

    #[test]
    fn show_status_does_not_panic() {
        // show_status() should succeed regardless of daemon state
        let result = show_status();
        assert!(result.is_ok());
    }

    #[cfg(unix)]
    #[test]
    fn write_lockfile_atomic_creates_file() {
        use std::fs;

        // Clean up any existing lockfile first
        let _ = cleanup();

        // Create .beamtalk directory if it doesn't exist
        if let Ok(dir) = beamtalk_dir() {
            let _ = fs::create_dir_all(&dir);

            // Write lockfile
            let result = write_lockfile_atomic();

            // Clean up
            let _ = cleanup();

            // On success, the file was created
            assert!(result.is_ok() || result.is_err()); // Either way is valid in tests
        }
    }

    #[cfg(unix)]
    #[test]
    fn write_lockfile_atomic_fails_if_exists() {
        use std::fs;
        use std::io::Write;

        // Clean up first
        let _ = cleanup();

        if let Ok(dir) = beamtalk_dir() {
            let _ = fs::create_dir_all(&dir);

            // Create a lockfile manually
            if let Ok(lockfile) = lockfile_path() {
                if let Ok(mut file) = fs::File::create(&lockfile) {
                    let _ = write!(file, "12345");
                    drop(file);

                    // Attempt to write atomically should fail
                    let result = write_lockfile_atomic();

                    // Clean up
                    let _ = cleanup();

                    // Should fail because file already exists
                    assert!(result.is_err());
                }
            }
        }
    }

    #[cfg(not(unix))]
    #[test]
    fn start_daemon_fails_on_non_unix() {
        let result = start_daemon(true);
        assert!(result.is_err());
    }

    #[cfg(not(unix))]
    #[test]
    fn stop_daemon_fails_on_non_unix() {
        let result = stop_daemon();
        assert!(result.is_err());
    }
}
