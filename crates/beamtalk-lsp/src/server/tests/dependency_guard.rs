// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0070 WS3 in the editor (BT-4611): the LSP preloads every declared
//! dependency's source — path dependencies included — and lifts ADR 0100's
//! pre-WS3 dependency guard only when that loading was complete, with the
//! CLI's rule. A missing selector then reports does-not-understand, and a
//! selector a dependency's extension defines does not.

use super::super::config::{DependencyLoad, collect_preload_files, root_dependencies};
use super::*;

/// The producer: a class and an extension on `Object`.
const PRODUCER_CLASS: &str = "Object subclass: Codec\n  encode: x => x\n";
const PRODUCER_EXTENSION: &str = "Object >> shout => 1\n";

/// How the consumer reaches the producer.
#[derive(Clone, Copy)]
enum DepKind {
    /// `producer = { path = "../producer" }`, outside `_build/deps`.
    Path,
    /// A git dependency already fetched into `_build/deps/producer/`.
    Git,
}

/// Writes a consumer package `app` under `base` depending on a `producer`
/// package holding `producer_files` (in its `src/`). Returns the consumer
/// root.
fn consumer_with_dependency(
    base: &Path,
    kind: DepKind,
    producer_files: &[(&str, &str)],
) -> PathBuf {
    let app = base.join("app");
    fs::create_dir_all(app.join("src")).expect("create consumer src");
    let producer = match kind {
        DepKind::Path => {
            fs::write(
                app.join("beamtalk.toml"),
                "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n\
                 [dependencies]\nproducer = { path = \"../producer\" }\n",
            )
            .expect("write consumer manifest");
            base.join("producer")
        }
        DepKind::Git => app.join("_build").join("deps").join("producer"),
    };
    fs::create_dir_all(producer.join("src")).expect("create producer src");
    for (name, source) in producer_files {
        fs::write(producer.join("src").join(name), source).expect("write producer file");
    }
    match kind {
        DepKind::Path => fs::write(
            producer.join("beamtalk.toml"),
            "[package]\nname = \"producer\"\nversion = \"0.1.0\"\n",
        )
        .expect("write producer manifest"),
        DepKind::Git => write_manifest_with_git_deps(&app, "app", &["producer"]),
    }
    app
}

/// Preloads `app` into a fresh backend, opens `app/src/main.bt` holding
/// `main_source`, and returns its diagnostic messages.
async fn diagnostics_for_main(app: &Path, main_source: &str) -> Vec<String> {
    let main_path = app.join("src").join("main.bt");
    fs::write(&main_path, main_source).expect("write main.bt");
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    backend
        .preload_workspace_source_files(PreloadConfig {
            roots: vec![app.to_path_buf()],
            stdlib_dirs: vec![],
        })
        .await;
    let utf8 = Utf8PathBuf::from_path_buf(main_path).expect("temp path is UTF-8");
    open_test_file(backend, &utf8, main_source);
    let svc = backend.service.lock().expect("service lock poisoned");
    svc.diagnostics(&utf8)
        .into_iter()
        .map(|d| d.message.to_string())
        .collect()
}

fn dnu_for<'a>(messages: &'a [String], selector: &str) -> Vec<&'a String> {
    messages
        .iter()
        .filter(|m| m.contains("does not understand") && m.contains(&format!("'{selector}'")))
        .collect()
}

fn producer_with_extension() -> Vec<(&'static str, &'static str)> {
    vec![("codec.bt", PRODUCER_CLASS), ("ext.bt", PRODUCER_EXTENSION)]
}

const USES_SHOUT: &str = "Object subclass: Main\n  run => Codec new shout\n";
const USES_WHISPER: &str = "Object subclass: Main\n  run => Codec new whisper\n";

async fn assert_guard_lifted(kind: DepKind, label: &str) {
    let temp = unique_temp_dir(&format!("beamtalk_lsp_dep_guard_{label}"));

    let app = consumer_with_dependency(&temp.join("shout"), kind, &producer_with_extension());
    let messages = diagnostics_for_main(&app, USES_SHOUT).await;
    assert!(
        dnu_for(&messages, "shout").is_empty(),
        "{label}: a selector the dependency's extension defines must not be reported, \
         got {messages:?}"
    );

    let app = consumer_with_dependency(&temp.join("whisper"), kind, &producer_with_extension());
    let messages = diagnostics_for_main(&app, USES_WHISPER).await;
    assert_eq!(
        dnu_for(&messages, "whisper").len(),
        1,
        "{label}: a missing selector must report does-not-understand once the \
         dependency's extensions are loaded, got {messages:?}"
    );

    // Without the producer's extension, `shout` is not understood either.
    let app = consumer_with_dependency(
        &temp.join("no_extension"),
        kind,
        &[("codec.bt", PRODUCER_CLASS)],
    );
    let messages = diagnostics_for_main(&app, USES_SHOUT).await;
    assert_eq!(
        dnu_for(&messages, "shout").len(),
        1,
        "{label}: removing the dependency's extension brings the DNU back, got {messages:?}"
    );

    let _ = fs::remove_dir_all(&temp);
}

#[tokio::test]
async fn path_dependency_extensions_lift_the_guard() {
    assert_guard_lifted(DepKind::Path, "path").await;
}

#[tokio::test]
async fn fetched_git_dependency_extensions_lift_the_guard() {
    assert_guard_lifted(DepKind::Git, "git").await;
}

#[tokio::test]
async fn a_dependency_with_a_parse_error_keeps_the_guard_on() {
    let temp = unique_temp_dir("beamtalk_lsp_dep_guard_parse_error");
    let app = consumer_with_dependency(
        &temp,
        DepKind::Path,
        &[
            ("codec.bt", PRODUCER_CLASS),
            ("broken.bt", "Object >> ((( =>\n"),
        ],
    );
    let messages = diagnostics_for_main(&app, USES_WHISPER).await;
    assert!(
        dnu_for(&messages, "whisper").is_empty(),
        "a dependency file that failed to parse may hide an extension, so the \
         guard stays on, got {messages:?}"
    );
    let _ = fs::remove_dir_all(&temp);
}

#[tokio::test]
async fn a_declared_dependency_missing_on_disk_keeps_the_guard_on() {
    let temp = unique_temp_dir("beamtalk_lsp_dep_guard_missing");
    let app = consumer_with_dependency(&temp, DepKind::Path, &[("codec.bt", PRODUCER_CLASS)]);
    // A second, unfetched git dependency.
    fs::write(
        app.join("beamtalk.toml"),
        "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n[dependencies]\n\
         producer = { path = \"../producer\" }\n\
         remote = { git = \"https://example.com/remote.git\", tag = \"v1\" }\n",
    )
    .expect("rewrite consumer manifest");
    let messages = diagnostics_for_main(&app, USES_WHISPER).await;
    assert!(
        dnu_for(&messages, "whisper").is_empty(),
        "an unfetched dependency's extensions are unknown, so the guard stays on, \
         got {messages:?}"
    );
    let _ = fs::remove_dir_all(&temp);
}

#[test]
fn root_dependencies_finds_path_dependencies_transitively() {
    let temp = unique_temp_dir("beamtalk_lsp_root_dependencies");
    let app = consumer_with_dependency(&temp, DepKind::Path, &[("codec.bt", PRODUCER_CLASS)]);
    // producer → leaf, another path dependency.
    fs::write(
        temp.join("producer").join("beamtalk.toml"),
        "[package]\nname = \"producer\"\nversion = \"0.1.0\"\n\n\
         [dependencies]\nleaf = { path = \"../leaf\" }\n",
    )
    .expect("rewrite producer manifest");
    fs::create_dir_all(temp.join("leaf")).expect("create leaf");
    fs::write(
        temp.join("leaf").join("beamtalk.toml"),
        "[package]\nname = \"leaf\"\nversion = \"0.1.0\"\n",
    )
    .expect("write leaf manifest");

    let deps = root_dependencies(&app);
    assert!(deps.declared);
    assert!(deps.all_found);
    let srcs: Vec<_> = deps.found.iter().map(|d| d.src.clone()).collect();
    assert_eq!(
        srcs,
        [
            temp.join("producer").join("src"),
            // No `src/`: the checkout root is the source directory.
            temp.join("leaf"),
        ]
    );
    let _ = fs::remove_dir_all(&temp);
}

#[test]
fn collect_preload_files_preloads_path_dependency_sources() {
    let temp = unique_temp_dir("beamtalk_lsp_preload_path_dep");
    let app = consumer_with_dependency(&temp, DepKind::Path, &producer_with_extension());
    let loaded = collect_preload_files(PreloadConfig {
        roots: vec![app],
        stdlib_dirs: vec![],
    });
    assert!(
        loaded.user_files.iter().any(|(p, _)| p.ends_with("ext.bt")),
        "a path dependency's source must be preloaded, got {:?}",
        loaded.user_files
    );
    assert_eq!(
        loaded.dependencies,
        DependencyLoad {
            declared: true,
            loaded: true,
            src_dirs: vec![temp.join("producer").join("src")],
        }
    );
    let _ = fs::remove_dir_all(&temp);
}

#[test]
fn a_malformed_dependency_table_keeps_the_guard_on() {
    let temp = unique_temp_dir("beamtalk_lsp_dep_guard_malformed");
    fs::create_dir_all(temp.join("src")).expect("create src");
    fs::write(
        temp.join("beamtalk.toml"),
        "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n[dependencies]\nbad = 42\n",
    )
    .expect("write manifest");
    let loaded = collect_preload_files(PreloadConfig {
        roots: vec![temp.clone()],
        stdlib_dirs: vec![],
    });
    assert!(loaded.dependencies.declared);
    assert!(!loaded.dependencies.loaded);
    assert!(loaded.dependencies.extensions_unknown(false));
    let _ = fs::remove_dir_all(&temp);
}

#[test]
fn a_package_without_dependencies_has_the_guard_off() {
    let temp = unique_temp_dir("beamtalk_lsp_dep_guard_none");
    write_manifest_with_git_deps(&temp, "app", &[]);
    let loaded = collect_preload_files(PreloadConfig {
        roots: vec![temp.clone()],
        stdlib_dirs: vec![],
    });
    assert!(!loaded.dependencies.declared);
    assert!(!loaded.dependencies.extensions_unknown(false));
    let _ = fs::remove_dir_all(&temp);
}

/// A path dependency lives outside `_build/deps/<name>/`, so its package is
/// registered from its own manifest: its files are stamped as `producer`,
/// not as the consumer's own package (so its `internal` aliases stay
/// internal).
#[tokio::test]
async fn path_dependency_files_are_stamped_with_their_own_package() {
    let temp = unique_temp_dir("beamtalk_lsp_dep_guard_stamping");
    let app = consumer_with_dependency(&temp, DepKind::Path, &[("codec.bt", PRODUCER_CLASS)]);
    let (service, _socket) = tower_lsp::LspService::new(Backend::new);
    let backend: &Backend = service.inner();
    backend.load_root_packages(std::slice::from_ref(&app)).await;
    let dep_file = Utf8PathBuf::from_path_buf(temp.join("producer").join("src").join("codec.bt"))
        .expect("temp path is UTF-8");
    let app_file =
        Utf8PathBuf::from_path_buf(app.join("src").join("main.bt")).expect("temp path is UTF-8");
    let svc = backend.service.lock().expect("service lock poisoned");
    assert_eq!(
        svc.project_index()
            .alias_package_for_file(&dep_file)
            .as_str(),
        "producer"
    );
    assert_eq!(
        svc.project_index()
            .alias_package_for_file(&app_file)
            .as_str(),
        "app"
    );
    drop(svc);
    let _ = fs::remove_dir_all(&temp);
}
