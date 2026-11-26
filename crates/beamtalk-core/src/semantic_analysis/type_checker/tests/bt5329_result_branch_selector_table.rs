// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5329: the `Result` branch selectors (`ifOk:`, `ifError:`,
//! `ifOk:ifError:`) live in one table, `state_threading_selectors::
//! RESULT_BRANCH_SHAPES`. The type checker's inference sites (the run-once
//! decision, the error-block narrowing, the exclusive-arms rule) must read
//! it through its helpers and never spell the selectors out again, or the
//! run-once decision and the error-variant narrowing stop agreeing.

use std::path::{Path, PathBuf};

fn rust_files(dir: &Path, out: &mut Vec<PathBuf>) {
    for entry in std::fs::read_dir(dir).expect("read dir") {
        let path = entry.expect("dir entry").path();
        if path.is_dir() {
            rust_files(&path, out);
        } else if path.extension().is_some_and(|e| e == "rs") {
            out.push(path);
        }
    }
}

/// The selector spellings of the table appear as string literals in no
/// inference file: a site that matches `"ifError:"` by hand has its own list.
#[test]
fn inference_sites_do_not_spell_result_branch_selectors() {
    let dir =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("src/semantic_analysis/type_checker/inference");
    let mut files = Vec::new();
    rust_files(&dir, &mut files);
    assert!(!files.is_empty());
    let mut offenders = Vec::new();
    for file in files {
        let src = std::fs::read_to_string(&file).expect("read source");
        for (n, line) in src.lines().enumerate() {
            let code = line.trim_start();
            if code.starts_with("//") {
                continue;
            }
            for row in crate::state_threading_selectors::RESULT_BRANCH_SHAPES {
                if line.contains(&format!("\"{}\"", row.selector)) {
                    offenders.push(format!("{}:{}: {}", file.display(), n + 1, line.trim()));
                }
            }
        }
    }
    assert!(
        offenders.is_empty(),
        "use the state_threading_selectors Result-branch helpers instead of a selector literal:\n{}",
        offenders.join("\n")
    );
}
