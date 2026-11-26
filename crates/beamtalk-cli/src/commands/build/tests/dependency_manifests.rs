// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5186: every read of a dependency's `beamtalk.toml` on the build path is
//! lenient, so a dependency written for a newer toolchain (unknown keys) still
//! builds — discovery *and* the dependency's own compile and `.app` generation.

use super::*;
use beamtalk_core::build_layout::BuildLayout;

/// Register the unknown-key `tracing::warn!` callsite exactly once, before
/// any test here can race its first registration (BT-5272).
///
/// `tracing` registers a callsite lazily, on its first hit anywhere in the
/// process. A registering thread computes the callsite's cached `Interest`
/// from the dispatchers it sees, then stores it. If a test's scoped
/// subscriber is installed in between, its interest rebuild runs first and
/// the registrant's later store overwrites it with `never`: the subscriber
/// silently receives nothing. The two tests in this module both reach the
/// callsite and run in parallel, so the second test's captured log was
/// occasionally empty. `Once::call_once` blocks every other caller until the
/// first has finished registering, so afterwards installing a subscriber
/// rebuilds interest for an already-registered callsite, which is reliable.
/// Call this first in every test that builds a dependency with unknown keys.
fn register_unknown_key_callsite() {
    static REGISTERED: std::sync::Once = std::sync::Once::new();
    REGISTERED.call_once(|| {
        let temp = TempDir::new().unwrap();
        let path = Utf8PathBuf::from_path_buf(temp.path().join("beamtalk.toml")).unwrap();
        write_test_file(
            &path,
            "[package]\nname = \"prime\"\nversion = \"0.1.0\"\nprime_only_key = 1\n",
        );
        beamtalk_package::manifest::parse_dependency_manifest_full(&path).unwrap();
    });
}

#[test]
fn build_succeeds_with_a_path_dependency_that_has_unknown_manifest_keys() {
    register_unknown_key_callsite();
    let temp = TempDir::new().unwrap();
    let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

    let helper = root.join("helper");
    fs::create_dir_all(helper.join("src")).unwrap();
    write_test_file(
        &helper.join("beamtalk.toml"),
        "future_top_level = \"x\"\n\n\
         [package]\nname = \"helper\"\nversion = \"0.1.0\"\nfuture_package_key = true\n\n\
         [future_section]\nsetting = 1\n",
    );
    write_test_file(
        &helper.join("src/helper_thing.bt"),
        "Object subclass: HelperThing\n",
    );

    let app = root.join("app");
    fs::create_dir_all(app.join("src")).unwrap();
    write_test_file(
        &app.join("beamtalk.toml"),
        "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n\
         [dependencies]\nhelper = { path = \"../helper\" }\n",
    );
    write_test_file(&app.join("src/main.bt"), "main := [42].");

    build(app.as_str(), &default_options(), false)
        .expect("a dependency's unknown manifest keys must not fail the build");

    let dep_ebin = BuildLayout::new(&app).dep_ebin_dir("helper");
    assert!(
        dep_ebin.join("helper.app").exists(),
        "the dependency was compiled and its .app generated in {dep_ebin}"
    );
}

/// The project's *own* manifest stays strict: an unknown key there is a typo.
#[test]
fn build_rejects_unknown_keys_in_the_project_manifest() {
    let temp = TempDir::new().unwrap();
    let project = create_test_project(&temp);
    write_test_file(
        &project.join("beamtalk.toml"),
        "[package]\nname = \"app\"\nversion = \"0.1.0\"\nnmae = \"typo\"\n",
    );
    write_test_file(&project.join("src/main.bt"), "main := [42].");
    let err = build(project.as_str(), &default_options(), false).unwrap_err();
    assert!(format!("{err:?}").contains("nmae"), "{err:?}");
}

/// A build reads each dependency's manifest several times; the unknown-key
/// warning must still appear once per dependency and key.
#[test]
fn unknown_key_warning_is_emitted_once_per_dependency_per_build() {
    register_unknown_key_callsite();

    #[derive(Clone, Default)]
    struct Captured(std::sync::Arc<std::sync::Mutex<Vec<u8>>>);
    impl std::io::Write for Captured {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.0.lock().unwrap().extend_from_slice(buf);
            Ok(buf.len())
        }
        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }

    let temp = TempDir::new().unwrap();
    let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let helper = root.join("helper");
    fs::create_dir_all(helper.join("src")).unwrap();
    write_test_file(
        &helper.join("beamtalk.toml"),
        "[package]\nname = \"helper\"\nversion = \"0.1.0\"\nonce_only_key = 1\n",
    );
    write_test_file(
        &helper.join("src/helper_thing.bt"),
        "Object subclass: HelperThing\n",
    );
    let app = root.join("app");
    fs::create_dir_all(app.join("src")).unwrap();
    write_test_file(
        &app.join("beamtalk.toml"),
        "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n\
         [dependencies]\nhelper = { path = \"../helper\" }\n",
    );
    write_test_file(&app.join("src/main.bt"), "main := [42].");

    let captured = Captured::default();
    let subscriber = tracing_subscriber::fmt()
        .with_max_level(tracing::Level::WARN)
        .with_ansi(false)
        .with_writer({
            let captured = captured.clone();
            move || captured.clone()
        })
        .finish();
    tracing::subscriber::with_default(subscriber, || {
        build(app.as_str(), &default_options(), false).unwrap();
        // A second build in the same process is not a new dependency read set.
        build(app.as_str(), &default_options(), false).unwrap();
    });
    let log = String::from_utf8(captured.0.lock().unwrap().clone()).unwrap();
    assert_eq!(
        log.matches("Ignoring unknown key `once_only_key`").count(),
        1,
        "warned more than once:\n{log}"
    );
}
