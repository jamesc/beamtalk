// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Network bind address resolution for the REPL WebSocket server.
//!
//! Handles parsing `--bind` CLI flag values into IP addresses,
//! including Tailscale interface auto-detection.
//!
//! **DDD Context:** CLI

use std::net::Ipv4Addr;
use std::process::Command;

use miette::{Result, miette};

/// Default bind address: localhost only (ADR 0020).
pub const DEFAULT_BIND_ADDR: Ipv4Addr = Ipv4Addr::LOCALHOST;

/// Resolve a `--bind` argument into an IPv4 address.
///
/// Supported values:
/// - `None` → `127.0.0.1` (default, localhost only)
/// - `"tailscale"` → auto-detect via `tailscale ip -4`
/// - Any valid IPv4 string → parsed directly
pub fn resolve_bind_addr(bind_arg: Option<&str>) -> Result<Ipv4Addr> {
    match bind_arg {
        None => Ok(DEFAULT_BIND_ADDR),
        Some(s) if s.eq_ignore_ascii_case("tailscale") => detect_tailscale_ip(),
        Some(s) => s
            .parse::<Ipv4Addr>()
            .map_err(|_| miette!("Invalid bind address: {s}\nExpected an IPv4 address (e.g., 192.168.1.5) or \"tailscale\".")),
    }
}

/// Returns true if the address is non-loopback (requires `--confirm-network`).
pub fn is_non_loopback(addr: Ipv4Addr) -> bool {
    !addr.is_loopback()
}

/// Validate that non-loopback binding is explicitly confirmed.
pub fn validate_network_binding(addr: Ipv4Addr, confirm_network: bool) -> Result<()> {
    if is_non_loopback(addr) && !confirm_network {
        Err(miette!(
            "⚠️  Binding to {addr} exposes the workspace to the network.\n\
             Use --confirm-network to proceed, or use --bind tailscale for secure remote access."
        ))
    } else {
        Ok(())
    }
}

/// Auto-detect the Tailscale IPv4 address via `tailscale ip -4`.
fn detect_tailscale_ip() -> Result<Ipv4Addr> {
    let output = Command::new("tailscale")
        .args(["ip", "-4"])
        .output()
        .map_err(|e| {
            miette!(
                "Failed to run `tailscale ip -4`: {e}\n\
                 Is Tailscale installed? See https://tailscale.com/download"
            )
        })?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(miette!(
            "Tailscale returned an error:\n{stderr}\n\
             Make sure Tailscale is running: `tailscale status`"
        ));
    }

    let ip_str = String::from_utf8_lossy(&output.stdout);
    // tailscale ip -4 may output multiple IPs (one per line); use the first.
    let ip_str = ip_str.lines().next().unwrap_or("").trim();

    if ip_str.is_empty() {
        return Err(miette!(
            "No output from `tailscale ip -4`.\n\
             Make sure Tailscale is connected: `tailscale status`"
        ));
    }

    ip_str.parse::<Ipv4Addr>().map_err(|_| {
        miette!(
            "Unexpected output from `tailscale ip -4`: \"{ip_str}\"\n\
             Expected an IPv4 address like 100.64.x.x"
        )
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolve_none_returns_localhost() {
        assert_eq!(resolve_bind_addr(None).unwrap(), DEFAULT_BIND_ADDR);
    }

    #[test]
    fn test_resolve_explicit_ip() {
        let addr = resolve_bind_addr(Some("192.168.1.5")).unwrap();
        assert_eq!(addr, Ipv4Addr::new(192, 168, 1, 5));
    }

    #[test]
    fn test_resolve_all_interfaces() {
        let addr = resolve_bind_addr(Some("0.0.0.0")).unwrap();
        assert_eq!(addr, Ipv4Addr::UNSPECIFIED);
    }

    #[test]
    fn test_resolve_localhost() {
        let addr = resolve_bind_addr(Some("127.0.0.1")).unwrap();
        assert_eq!(addr, Ipv4Addr::LOCALHOST);
    }

    #[test]
    fn test_resolve_invalid_ip() {
        assert!(resolve_bind_addr(Some("not-an-ip")).is_err());
    }

    #[test]
    fn test_is_non_loopback() {
        assert!(!is_non_loopback(Ipv4Addr::LOCALHOST));
        assert!(is_non_loopback(Ipv4Addr::UNSPECIFIED));
        assert!(is_non_loopback(Ipv4Addr::new(192, 168, 1, 5)));
        assert!(is_non_loopback(Ipv4Addr::new(100, 64, 0, 1)));
    }

    #[test]
    fn test_validate_loopback_no_confirmation_needed() {
        assert!(validate_network_binding(Ipv4Addr::LOCALHOST, false).is_ok());
    }

    #[test]
    fn test_validate_non_loopback_requires_confirmation() {
        assert!(validate_network_binding(Ipv4Addr::UNSPECIFIED, false).is_err());
        assert!(validate_network_binding(Ipv4Addr::UNSPECIFIED, true).is_ok());
    }

    #[test]
    fn test_validate_non_loopback_with_confirmation() {
        assert!(validate_network_binding(Ipv4Addr::new(192, 168, 1, 5), true).is_ok());
    }
}
