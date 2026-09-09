// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `Lint/diagnostic_summary` coverage of dependency resolution: cross-file visibility leaks (E0402), git dependencies, transitive git dependencies, and unreadable package files/directories.

use super::*;

/// Write a fixture project declaring a git dependency `http` in
/// `beamtalk.toml`, with the dependency's checkout already present under
/// `_build/deps/http/src/` (simulating the state left by a prior
/// `beamtalk build`). The project's own
/// `src/app.bt` references the dependency's `HTTPServer` class. Returns
/// the fixture's `TempDir` (keep it alive for the duration of the test —
/// it removes the project directory on drop) and the path to
/// `src/app.bt`.
fn write_git_dependency_fixture() -> (tempfile::TempDir, std::path::PathBuf) {
    let temp = tempfile::TempDir::new().unwrap();
    let dir = temp.path();
    let src_dir = dir.join("src");
    std::fs::create_dir_all(&src_dir).unwrap();

    std::fs::write(
        dir.join("beamtalk.toml"),
        "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nhttp = { git = \"https://example.com/http.git\", tag = \"v1.0.0\" }\n",
    )
    .unwrap();

    // Simulate `beamtalk build` having already fetched the dependency.
    let dep_src_dir = dir.join("_build").join("deps").join("http").join("src");
    std::fs::create_dir_all(&dep_src_dir).unwrap();
    std::fs::write(
        dep_src_dir.join("http_server.bt"),
        "Object subclass: HTTPServer\n",
    )
    .unwrap();

    // A second project-local class so `has_cross_file_classes` is true
    // independent of dependency resolution (matching the sentinel
    // pattern `fixture_sourced_protocol_name_is_not_unresolved` uses in
    // beamtalk-core). Without this, `check_unresolved_classes` would be
    // skipped entirely for a single-file project and this test would
    // pass vacuously regardless of whether the fix is in place.
    std::fs::write(src_dir.join("other.bt"), "Object subclass: Other\n").unwrap();

    let app_file = src_dir.join("app.bt");
    std::fs::write(
        &app_file,
        "Object subclass: App\n\n  class run =>\n    HTTPServer new\n",
    )
    .unwrap();

    (temp, app_file)
}

/// MCP `lint` must resolve classes from a project's git
/// dependencies (declared in `beamtalk.toml`) the same way `beamtalk
/// build`/`beamtalk lint` do, using whatever dependency checkout is
/// already on disk under `_build/deps/<name>/` — without a false-positive
/// `Unresolved class` diagnostic.
#[test]
fn run_lint_structured_resolves_git_dependency_classes() {
    let (_temp, app_file) = write_git_dependency_fixture();
    let result = run_lint_structured(app_file.to_str().unwrap());

    let unresolved = result
        .warnings
        .iter()
        .chain(result.errors.iter())
        .any(|d| d.message.contains("Unresolved class"));
    assert!(
        !unresolved,
        "MCP lint should resolve HTTPServer from the git dependency checkout, got: {result:?}",
    );
}

/// Same as `run_lint_structured_resolves_git_dependency_classes`
/// but for the `diagnostic_summary` tool.
#[test]
fn compute_diagnostic_summary_resolves_git_dependency_classes() {
    let (_temp, app_file) = write_git_dependency_fixture();
    let result = compute_diagnostic_summary(app_file.to_str().unwrap());

    let unresolved_class_total = result["totals_by_category"]["UnresolvedClass"]["total"]
        .as_u64()
        .unwrap_or(0);
    assert_eq!(
        unresolved_class_total, 0,
        "diagnostic_summary should resolve HTTPServer from the git dependency \
             checkout with zero UnresolvedClass diagnostics, got: {result:?}",
    );
}

/// Write a fixture project declaring a *direct* git dependency `http` in
/// `beamtalk.toml`, where `http`'s own checked-out `beamtalk.toml`
/// declares a *transitive* git dependency `net` — never
/// mentioned in the project's own manifest. Both checkouts are already
/// present under `_build/deps/`, simulating a prior `beamtalk build`. The
/// project's own `src/app.bt` references `net`'s `NetClient` class
/// directly, which is only reachable by walking `http`'s manifest.
/// Returns the fixture's `TempDir` (keep it alive for the duration of the
/// test) and the path to `src/app.bt`.
fn write_transitive_git_dependency_fixture() -> (tempfile::TempDir, std::path::PathBuf) {
    let temp = tempfile::TempDir::new().unwrap();
    let dir = temp.path();
    let src_dir = dir.join("src");
    std::fs::create_dir_all(&src_dir).unwrap();

    std::fs::write(
        dir.join("beamtalk.toml"),
        "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nhttp = { git = \"https://example.com/http.git\", tag = \"v1.0.0\" }\n",
    )
    .unwrap();

    // Simulate `beamtalk build` having already fetched the direct
    // dependency, whose own manifest declares a transitive dependency.
    let http_dir = dir.join("_build").join("deps").join("http");
    std::fs::create_dir_all(http_dir.join("src")).unwrap();
    std::fs::write(
        http_dir.join("beamtalk.toml"),
        "[package]\nname = \"http\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nnet = { git = \"https://example.com/net.git\", tag = \"v1.0.0\" }\n",
    )
    .unwrap();
    std::fs::write(
        http_dir.join("src").join("http_server.bt"),
        "Object subclass: HTTPServer\n",
    )
    .unwrap();

    // The transitive dependency's checkout, never declared in app's own
    // `beamtalk.toml` — only discoverable by walking `http`'s manifest.
    let net_src_dir = dir.join("_build").join("deps").join("net").join("src");
    std::fs::create_dir_all(&net_src_dir).unwrap();
    std::fs::write(
        net_src_dir.join("net_client.bt"),
        "Object subclass: NetClient\n",
    )
    .unwrap();

    // Sentinel class so `has_cross_file_classes` isn't vacuously true/false
    // independent of dependency resolution, matching
    // `write_git_dependency_fixture`'s pattern.
    std::fs::write(src_dir.join("other.bt"), "Object subclass: Other\n").unwrap();

    let app_file = src_dir.join("app.bt");
    std::fs::write(
        &app_file,
        "Object subclass: App\n\n  class run =>\n    NetClient new\n",
    )
    .unwrap();

    (temp, app_file)
}

/// MCP `lint` must resolve classes from a *transitive*
/// dependency (declared only in a direct dependency's own
/// `beamtalk.toml`, not the project's) the same way `beamtalk
/// build`/`beamtalk lint` do, using whatever checkout is already on disk
/// under `_build/deps/<name>/` — without a false-positive `Unresolved
/// class` diagnostic.
#[test]
fn run_lint_structured_resolves_transitive_git_dependency_classes() {
    let (_temp, app_file) = write_transitive_git_dependency_fixture();
    let result = run_lint_structured(app_file.to_str().unwrap());

    let unresolved = result
        .warnings
        .iter()
        .chain(result.errors.iter())
        .any(|d| d.message.contains("Unresolved class"));
    assert!(
        !unresolved,
        "MCP lint should resolve NetClient from the transitive git dependency \
             checkout, got: {result:?}",
    );
}

/// Same as
/// `run_lint_structured_resolves_transitive_git_dependency_classes` but
/// for the `diagnostic_summary` tool.
#[test]
fn compute_diagnostic_summary_resolves_transitive_git_dependency_classes() {
    let (_temp, app_file) = write_transitive_git_dependency_fixture();
    let result = compute_diagnostic_summary(app_file.to_str().unwrap());

    let unresolved_class_total = result["totals_by_category"]["UnresolvedClass"]["total"]
        .as_u64()
        .unwrap_or(0);
    assert_eq!(
        unresolved_class_total, 0,
        "diagnostic_summary should resolve NetClient from the transitive git \
             dependency checkout with zero UnresolvedClass diagnostics, got: {result:?}",
    );
}

/// When a package-extraction file in src/ cannot be read, MCP lint
/// must surface a warning rather than silently dropping it from the
/// extraction set.
#[cfg(unix)]
#[test]
fn run_lint_structured_unreadable_package_file_warns() {
    use std::os::unix::fs::PermissionsExt;

    if running_as_root() {
        eprintln!("skipped: running as root, chmod 000 doesn't apply");
        return;
    }

    let temp = tempfile::TempDir::new().unwrap();
    let dir = temp.path();
    let src_dir = dir.join("src");
    std::fs::create_dir_all(&src_dir).unwrap();

    // Create a beamtalk.toml so find_package_root works.
    std::fs::write(
        dir.join("beamtalk.toml"),
        "[package]\nname = \"unreadable-test\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();

    // Create a readable target file.
    let target = src_dir.join("main.bt");
    std::fs::write(&target, "Object subclass: Main\n  class hello => 42\n").unwrap();

    // Create a sibling file, then make it unreadable.
    let sibling = src_dir.join("helper.bt");
    std::fs::write(&sibling, "Object subclass: Helper\n  class help => 1\n").unwrap();
    std::fs::set_permissions(&sibling, std::fs::Permissions::from_mode(0o000)).unwrap();

    let result = run_lint_structured(target.to_str().unwrap());

    // Restore permissions so the TempDir's Drop cleanup can remove the
    // sibling file.
    let _ = std::fs::set_permissions(&sibling, std::fs::Permissions::from_mode(0o644));

    let has_unreadable_warning = result.warnings.iter().any(|d| {
        d.message
            .contains("cross-file class extraction may be incomplete")
    });
    assert!(
        has_unreadable_warning,
        "MCP lint should warn about unreadable package files, got: {result:?}",
    );
}

/// `compute_diagnostic_summary` should include unreadable package
/// files in its output when a sibling file cannot be read.
#[cfg(unix)]
#[test]
fn compute_diagnostic_summary_unreadable_package_file() {
    use std::os::unix::fs::PermissionsExt;

    if running_as_root() {
        eprintln!("skipped: running as root, chmod 000 doesn't apply");
        return;
    }

    let temp = tempfile::TempDir::new().unwrap();
    let dir = temp.path();
    let src_dir = dir.join("src");
    std::fs::create_dir_all(&src_dir).unwrap();

    std::fs::write(
        dir.join("beamtalk.toml"),
        "[package]\nname = \"unreadable-test\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();

    let target = src_dir.join("main.bt");
    std::fs::write(&target, "Object subclass: Main\n  class hello => 42\n").unwrap();

    let sibling = src_dir.join("helper.bt");
    std::fs::write(&sibling, "Object subclass: Helper\n  class help => 1\n").unwrap();
    std::fs::set_permissions(&sibling, std::fs::Permissions::from_mode(0o000)).unwrap();

    let result = compute_diagnostic_summary(target.to_str().unwrap());

    // Restore permissions so the TempDir's Drop cleanup can remove the
    // sibling file.
    let _ = std::fs::set_permissions(&sibling, std::fs::Permissions::from_mode(0o644));

    assert!(
        result.get("unreadable_package_files").is_some(),
        "diagnostic summary should include unreadable_package_files, got: {result}",
    );
    let unreadable = result["unreadable_package_files"].as_array().unwrap();
    assert_eq!(unreadable.len(), 1);
    assert!(
        unreadable[0].as_str().unwrap().contains("helper.bt"),
        "unreadable file should be helper.bt, got: {unreadable:?}",
    );
}

/// `compute_diagnostic_summary` must surface an error when a
/// direct target file is unreadable, not a clean `files_checked=0` result.
#[cfg(unix)]
#[test]
fn compute_diagnostic_summary_unreadable_direct_target() {
    use std::os::unix::fs::PermissionsExt;

    if running_as_root() {
        eprintln!("skipped: running as root, chmod 000 doesn't apply");
        return;
    }

    let temp = tempfile::TempDir::new().unwrap();
    let dir = temp.path();

    let target = dir.join("locked.bt");
    std::fs::write(&target, "Object subclass: Locked\n  class hello => 1\n").unwrap();
    std::fs::set_permissions(&target, std::fs::Permissions::from_mode(0o000)).unwrap();

    let result = compute_diagnostic_summary(target.to_str().unwrap());

    // Restore permissions so the TempDir's Drop cleanup can remove the
    // target file.
    std::fs::set_permissions(&target, std::fs::Permissions::from_mode(0o644)).unwrap();

    assert_eq!(result["files_checked"], 0);
    assert!(
        result["error"].is_string(),
        "should surface an error for unreadable target, got: {result}",
    );
    let err_msg = result["error"].as_str().unwrap();
    assert!(
        err_msg.contains("locked.bt"),
        "error should name the unreadable file, got: {err_msg}",
    );
    let listed = result["unreadable_target_files"].as_array().unwrap();
    assert_eq!(listed.len(), 1);
    assert!(listed[0].as_str().unwrap().contains("locked.bt"));
}

/// Directory-based lint invocations must still surface a specific
/// unreadable target when some files resolve successfully and others do
/// not.
#[cfg(unix)]
#[test]
fn compute_diagnostic_summary_directory_with_unreadable_target() {
    use std::os::unix::fs::PermissionsExt;

    if running_as_root() {
        eprintln!("skipped: running as root, chmod 000 doesn't apply");
        return;
    }

    let temp = tempfile::TempDir::new().unwrap();
    let dir = temp.path();

    let readable = dir.join("readable.bt");
    std::fs::write(&readable, "Object subclass: Readable\n  class hello => 1\n").unwrap();

    let locked = dir.join("locked.bt");
    std::fs::write(&locked, "Object subclass: Locked\n  class hello => 2\n").unwrap();
    std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o000)).unwrap();

    let result = compute_diagnostic_summary(dir.to_str().unwrap());

    // Restore permissions so the TempDir's Drop cleanup can remove the
    // locked file.
    std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o644)).unwrap();

    assert_eq!(
        result["files_checked"], 1,
        "readable sibling should still be checked, got: {result}",
    );
    let listed = result["unreadable_target_files"]
        .as_array()
        .unwrap_or_else(|| panic!("unreadable_target_files missing, got: {result}"));
    assert_eq!(listed.len(), 1);
    assert!(
        listed[0].as_str().unwrap().contains("locked.bt"),
        "should name the locked file, got: {listed:?}",
    );
    let err_msg = result["error"].as_str().unwrap();
    assert!(err_msg.contains("locked.bt"));
}

/// `run_lint_structured` must emit a file-level error for
/// unreadable targets surfaced by a directory walk, not drop them silently.
#[cfg(unix)]
#[test]
fn run_lint_structured_directory_with_unreadable_target_errors() {
    use std::os::unix::fs::PermissionsExt;

    if running_as_root() {
        eprintln!("skipped: running as root, chmod 000 doesn't apply");
        return;
    }

    let temp = tempfile::TempDir::new().unwrap();
    let dir = temp.path();

    let readable = dir.join("readable.bt");
    std::fs::write(&readable, "Object subclass: Readable\n  class hello => 1\n").unwrap();

    let locked = dir.join("locked.bt");
    std::fs::write(&locked, "Object subclass: Locked\n  class hello => 2\n").unwrap();
    std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o000)).unwrap();

    let result = run_lint_structured(dir.to_str().unwrap());

    // Restore permissions so the TempDir's Drop cleanup can remove the
    // locked file.
    std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o644)).unwrap();

    let has_locked_error = result
        .errors
        .iter()
        .any(|d| d.file.contains("locked.bt") && d.message.contains("Failed to read"));
    assert!(
        has_locked_error,
        "expected a read-failure error naming locked.bt, got: {result:?}",
    );
}

// `find_package_root` tests live in
// `beamtalk_project::package` tests — the MCP helper is a thin
// wrapper around the shared implementation, so duplicating the
// ancestor-walk assertions here would only lock in behaviour twice.
