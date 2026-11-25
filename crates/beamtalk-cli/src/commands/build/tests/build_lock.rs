// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5133: `build` takes the project lock before it reads the manifest, so a
//! build that waited on another beamtalk process sees that process's edits.

use super::*;
use beamtalk_core::build_layout::BuildLayout;
use std::sync::mpsc;
use std::time::Duration;

#[test]
fn build_reads_the_manifest_only_after_the_project_lock_is_free() {
    let temp = TempDir::new().unwrap();
    let project = create_test_project(&temp);
    write_test_file(
        &project.join("beamtalk.toml"),
        "[package]\nname = \"before\"\nversion = \"0.1.0\"\n",
    );
    write_test_file(&project.join("src/main.bt"), "main := [1].");

    let held = crate::commands::dir_lock::lock_project_build(&project).unwrap();
    let (tx, rx) = mpsc::channel();
    let builder = std::thread::spawn({
        let project = project.clone();
        move || {
            let r = build(project.as_str(), &default_options(), false);
            tx.send(()).unwrap();
            r
        }
    });
    // sleep: prove the build is blocked on the lock, not merely slow to start
    assert!(rx.recv_timeout(Duration::from_millis(500)).is_err());

    // The lock holder edits the manifest (what `deps add` does) and releases.
    write_test_file(
        &project.join("beamtalk.toml"),
        "[package]\nname = \"after\"\nversion = \"0.1.0\"\n",
    );
    drop(held);
    rx.recv_timeout(Duration::from_mins(2))
        .expect("build proceeds once the lock is free");
    let _ = builder.join().unwrap();

    let built = BuildLayout::new(&project).ebin_dir();
    let app = fs::read_dir(&built)
        .unwrap()
        .filter_map(Result::ok)
        .map(|e| e.file_name().to_string_lossy().into_owned())
        .find(|n| {
            std::path::Path::new(n)
                .extension()
                .is_some_and(|e| e == "app")
        })
        .expect("an .app file is written");
    assert_eq!(
        app, "after.app",
        "build used the manifest left by the holder"
    );
}
