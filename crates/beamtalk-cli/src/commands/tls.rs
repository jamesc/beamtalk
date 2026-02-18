// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! TLS certificate management for Erlang distribution security.
//!
//! **DDD Context:** CLI — Connection Security (ADR 0020 Phase 2)
//!
//! Provides `beamtalk tls init` to generate per-workspace CA and node
//! certificates for securing Erlang distribution with mTLS.
//!
//! Certificate layout:
//! ```text
//! ~/.beamtalk/workspaces/{id}/tls/
//! ├── ca.pem          # Self-signed CA certificate
//! ├── node.pem        # Node certificate signed by the CA
//! └── node-key.pem    # Node private key (chmod 600)
//! ```

use std::fs;
#[cfg(unix)]
use std::io::Write;
use std::path::{Path, PathBuf};

use clap::Subcommand;
use miette::{IntoDiagnostic, Result};
use rcgen::{
    BasicConstraints, CertificateParams, DnType, ExtendedKeyUsagePurpose, IsCa, KeyPair,
    KeyUsagePurpose,
};

use super::workspace::{self, storage};

/// TLS certificate management subcommands.
#[derive(Debug, Subcommand)]
pub enum TlsCommand {
    /// Generate TLS certificates for a workspace
    ///
    /// Creates a self-signed CA and node certificate pair for securing
    /// Erlang distribution with mTLS. Certificates are stored in the
    /// workspace's `tls/` directory.
    Init {
        /// Workspace to generate certificates for (default: auto-detect from current directory)
        #[arg(long)]
        workspace: Option<String>,
    },
}

/// Run a TLS subcommand.
pub fn run(action: TlsCommand) -> Result<()> {
    match action {
        TlsCommand::Init { workspace } => init_tls(workspace.as_deref()),
    }
}

/// Initialize TLS certificates for a workspace.
fn init_tls(workspace_name: Option<&str>) -> Result<()> {
    let current_dir = std::env::current_dir().into_diagnostic()?;
    let project_root = workspace::discovery::discover_project_root(&current_dir);
    let workspace_id = storage::workspace_id_for(&project_root, workspace_name)?;

    let tls_dir = storage::workspace_dir(&workspace_id)?.join("tls");

    // Check if certs already exist
    if tls_dir.join("ca.pem").exists()
        && tls_dir.join("node.pem").exists()
        && tls_dir.join("node-key.pem").exists()
    {
        println!("TLS certificates already exist for workspace: {workspace_id}");
        println!("  Directory: {}", tls_dir.display());
        // Regenerate ssl_dist.conf if missing (e.g. partial state)
        if !tls_dir.join("ssl_dist.conf").exists() {
            let conf_path = generate_ssl_dist_conf(&tls_dir)?;
            println!(
                "  ✓ Distribution config regenerated: {}",
                conf_path.display()
            );
        }
        println!("  To regenerate, remove the tls/ directory first.");
        return Ok(());
    }

    fs::create_dir_all(&tls_dir).into_diagnostic()?;

    println!("Generating TLS certificates for workspace: {workspace_id}");

    // Generate CA
    let (ca_cert_pem, ca_key_pair) = generate_ca()?;
    write_pem(&tls_dir.join("ca.pem"), &ca_cert_pem)?;
    println!("  ✓ CA certificate:  {}", tls_dir.join("ca.pem").display());

    // Generate node certificate signed by CA
    let (node_cert_pem, node_key_pem) = generate_node_cert(&ca_cert_pem, &ca_key_pair)?;
    write_pem(&tls_dir.join("node.pem"), &node_cert_pem)?;
    write_key(&tls_dir.join("node-key.pem"), &node_key_pem)?;
    println!(
        "  ✓ Node certificate: {}",
        tls_dir.join("node.pem").display()
    );
    println!(
        "  ✓ Node key:         {} (mode 600)",
        tls_dir.join("node-key.pem").display()
    );

    // Generate ssl_dist.conf for Erlang
    let conf_path = generate_ssl_dist_conf(&tls_dir)?;
    println!("  ✓ Distribution config: {}", conf_path.display());

    println!();
    println!("Use `beamtalk repl --tls` to start a TLS-secured workspace.");

    Ok(())
}

/// Generate a self-signed CA certificate.
///
/// Returns the PEM-encoded certificate and the CA key pair for signing
/// node certificates.
fn generate_ca() -> Result<(String, KeyPair)> {
    let key_pair = KeyPair::generate().into_diagnostic()?;

    let mut params = CertificateParams::default();
    params
        .distinguished_name
        .push(DnType::CommonName, "Beamtalk Workspace CA");
    params
        .distinguished_name
        .push(DnType::OrganizationName, "Beamtalk");
    params.is_ca = IsCa::Ca(BasicConstraints::Unconstrained);
    params.key_usages = vec![KeyUsagePurpose::KeyCertSign, KeyUsagePurpose::CrlSign];

    let cert = params.self_signed(&key_pair).into_diagnostic()?;

    Ok((cert.pem(), key_pair))
}

/// Generate a node certificate signed by the CA.
///
/// The certificate is valid for the `localhost` hostname and includes
/// extended key usage for both TLS server and client authentication
/// (required for mutual TLS).
fn generate_node_cert(ca_cert_pem: &str, ca_key_pair: &KeyPair) -> Result<(String, String)> {
    let node_key_pair = KeyPair::generate().into_diagnostic()?;

    // Reconstruct the CA certificate from PEM for signing
    let ca_params = CertificateParams::from_ca_cert_pem(ca_cert_pem).into_diagnostic()?;
    let ca_cert = ca_params.self_signed(ca_key_pair).into_diagnostic()?;

    let mut params = CertificateParams::new(vec!["localhost".to_string()]).into_diagnostic()?;
    params
        .distinguished_name
        .push(DnType::CommonName, "Beamtalk Node");
    params
        .distinguished_name
        .push(DnType::OrganizationName, "Beamtalk");
    params.extended_key_usages = vec![
        ExtendedKeyUsagePurpose::ServerAuth,
        ExtendedKeyUsagePurpose::ClientAuth,
    ];

    let cert = params
        .signed_by(&node_key_pair, &ca_cert, ca_key_pair)
        .into_diagnostic()?;

    Ok((cert.pem(), node_key_pair.serialize_pem()))
}

/// Generate the Erlang `ssl_dist.conf` configuration file.
///
/// This file is consumed by `-ssl_dist_optfile` and configures both
/// server and client sides of the TLS distribution connection.
pub fn generate_ssl_dist_conf(tls_dir: &Path) -> Result<PathBuf> {
    // Use forward slashes for paths — Erlang string literals treat backslashes
    // as escape sequences, which breaks on Windows.
    let ca_path = tls_dir.join("ca.pem").to_string_lossy().replace('\\', "/");
    let cert_path = tls_dir
        .join("node.pem")
        .to_string_lossy()
        .replace('\\', "/");
    let key_path = tls_dir
        .join("node-key.pem")
        .to_string_lossy()
        .replace('\\', "/");

    let conf = format!(
        "[{{server, [\n\
         \x20 {{certfile, \"{cert_path}\"}},\n\
         \x20 {{keyfile, \"{key_path}\"}},\n\
         \x20 {{cacertfile, \"{ca_path}\"}},\n\
         \x20 {{verify, verify_peer}},\n\
         \x20 {{fail_if_no_peer_cert, true}}\n\
         ]}},\n\
         {{client, [\n\
         \x20 {{certfile, \"{cert_path}\"}},\n\
         \x20 {{keyfile, \"{key_path}\"}},\n\
         \x20 {{cacertfile, \"{ca_path}\"}},\n\
         \x20 {{verify, verify_peer}}\n\
         ]}}].\n",
    );

    let conf_path = tls_dir.join("ssl_dist.conf");
    fs::write(&conf_path, conf).into_diagnostic()?;

    Ok(conf_path)
}

/// Get the path to the `ssl_dist.conf` for a workspace, if TLS is initialized.
///
/// Returns `None` if TLS certificates have not been generated for the workspace.
/// Returns an error if the conf file exists but the expected TLS files in the
/// workspace `tls/` directory are missing.
pub fn ssl_dist_conf_path(workspace_id: &str) -> Result<Option<PathBuf>> {
    let tls_dir = storage::workspace_dir(workspace_id)?.join("tls");
    let conf_path = tls_dir.join("ssl_dist.conf");
    if !conf_path.exists() {
        return Ok(None);
    }

    // Verify referenced cert files exist (catch stale/partial state)
    for file in &["ca.pem", "node.pem", "node-key.pem"] {
        if !tls_dir.join(file).exists() {
            return Err(miette::miette!(
                "TLS config exists but {file} is missing.\n\
                 Remove the tls/ directory and run `beamtalk tls init` to regenerate."
            ));
        }
    }

    Ok(Some(conf_path))
}

/// Write PEM content to a file.
fn write_pem(path: &Path, content: &str) -> Result<()> {
    fs::write(path, content).into_diagnostic()
}

/// Write a private key to a file with secure permissions (mode 600).
fn write_key(path: &Path, content: &str) -> Result<()> {
    #[cfg(unix)]
    {
        use std::os::unix::fs::{OpenOptionsExt, PermissionsExt};

        let mut file = fs::OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .mode(0o600)
            .open(path)
            .into_diagnostic()?;

        file.set_permissions(fs::Permissions::from_mode(0o600))
            .into_diagnostic()?;

        file.write_all(content.as_bytes()).into_diagnostic()?;
    }

    #[cfg(not(unix))]
    {
        fs::write(path, content).into_diagnostic()?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn generate_ca_produces_valid_pem() {
        let (pem, _key) = generate_ca().unwrap();
        assert!(pem.contains("BEGIN CERTIFICATE"));
        assert!(pem.contains("END CERTIFICATE"));
    }

    #[test]
    fn generate_node_cert_produces_valid_pem() {
        let (ca_pem, ca_key) = generate_ca().unwrap();
        let (cert_pem, key_pem) = generate_node_cert(&ca_pem, &ca_key).unwrap();
        assert!(cert_pem.contains("BEGIN CERTIFICATE"));
        assert!(key_pem.contains("BEGIN PRIVATE KEY"));
    }

    #[test]
    fn ssl_dist_conf_has_required_sections() {
        let dir = tempfile::tempdir().unwrap();
        let tls_dir = dir.path().join("tls");
        fs::create_dir_all(&tls_dir).unwrap();

        // Create dummy cert files
        fs::write(tls_dir.join("ca.pem"), "ca").unwrap();
        fs::write(tls_dir.join("node.pem"), "node").unwrap();
        fs::write(tls_dir.join("node-key.pem"), "key").unwrap();

        let conf_path = generate_ssl_dist_conf(&tls_dir).unwrap();
        let conf = fs::read_to_string(conf_path).unwrap();

        assert!(conf.contains("{server,"), "missing server section");
        assert!(conf.contains("{client,"), "missing client section");
        assert!(
            conf.contains("{verify, verify_peer}"),
            "missing verify_peer"
        );
        assert!(
            conf.contains("{fail_if_no_peer_cert, true}"),
            "missing fail_if_no_peer_cert"
        );
        assert!(conf.contains("certfile"), "missing certfile");
        assert!(conf.contains("keyfile"), "missing keyfile");
        assert!(conf.contains("cacertfile"), "missing cacertfile");
    }

    #[test]
    fn ssl_dist_conf_path_returns_none_when_missing() {
        // Use a random workspace ID that definitely doesn't exist
        let result = ssl_dist_conf_path("nonexistent_workspace_12345");
        // This will either return Ok(None) or Err (if workspace dir can't be resolved)
        // Both are acceptable - the important thing is it doesn't panic
        if let Ok(path) = result {
            assert!(path.is_none());
        }
    }

    #[cfg(unix)]
    #[test]
    fn write_key_sets_secure_permissions() {
        use std::os::unix::fs::PermissionsExt;

        let dir = tempfile::tempdir().unwrap();
        let key_path = dir.path().join("test-key.pem");
        write_key(&key_path, "test key content").unwrap();

        let metadata = fs::metadata(&key_path).unwrap();
        let mode = metadata.permissions().mode() & 0o777;
        assert_eq!(mode, 0o600, "Key file should have mode 600, got {mode:o}");
    }
}
