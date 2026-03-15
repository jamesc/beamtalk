// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

use beamtalk_core::ast::{ClassDefinition, Module};
use beamtalk_core::source_analysis::{lex_with_eof, parse};
use beamtalk_examples::{Corpus, CorpusEntry};
use pulldown_cmark::{CodeBlockKind, Event, Tag, TagEnd};

fn main() {
    let root = find_repo_root();
    let mut entries = Vec::new();

    // .bt fixture files — whole-file entries
    extract_bt_dir(&root, "stdlib/test/fixtures", None, &mut entries);
    extract_bt_dir(&root, "tests/e2e/fixtures", None, &mut entries);
    extract_bt_dir(&root, "docs/learning/fixtures", None, &mut entries);

    // .bt test files — whole-file entries (tests show how to *use* patterns)
    extract_bt_dir(&root, "stdlib/test", Some("/fixtures/"), &mut entries);

    // .bt example files
    extract_bt_dir(&root, "examples", None, &mut entries);

    // .md learning docs — extract fenced beamtalk code blocks
    extract_md_files(&root, "docs/learning", &mut entries);

    // beamtalk-language-features.md
    let lang_features = root.join("docs/beamtalk-language-features.md");
    if lang_features.exists() {
        extract_md_code_blocks(&root, &lang_features, &mut entries);
    }

    // Add synonym tags for common vocabulary mismatches
    add_synonym_tags(&mut entries);

    // Deterministic: sort entries by source-path-derived id
    entries.sort_by(|a, b| a.id.cmp(&b.id));

    // Sort tags within each entry
    for entry in &mut entries {
        entry.tags.sort();
        entry.tags.dedup();
    }

    let corpus = Corpus { entries };
    let json = serde_json::to_string_pretty(&corpus).expect("serialization must succeed");

    let output_path = root.join("crates/beamtalk-examples/corpus.json");
    fs::write(&output_path, format!("{json}\n")).expect("failed to write corpus.json");

    eprintln!(
        "Wrote {} entries to {}",
        corpus.entries.len(),
        output_path.display()
    );
}

/// Find the repository root by walking up from the current directory.
fn find_repo_root() -> PathBuf {
    let mut dir = std::env::current_dir().expect("cannot determine current directory");
    loop {
        if dir.join("Cargo.toml").exists() && dir.join("Justfile").exists() {
            return dir;
        }
        assert!(
            dir.pop(),
            "could not find repo root (no Cargo.toml + Justfile found)"
        );
    }
}

/// Parse a .bt source string and return the AST module.
fn parse_bt(source: &str) -> Module {
    let tokens = lex_with_eof(source);
    let (module, _diagnostics) = parse(tokens);
    module
}

/// Check if a .bt source parses without errors.
fn parses_cleanly(source: &str) -> bool {
    let tokens = lex_with_eof(source);
    let (_module, diagnostics) = parse(tokens);
    diagnostics
        .iter()
        .all(|d| d.severity != beamtalk_core::source_analysis::Severity::Error)
}

/// Extract tags from a parsed module: class names, superclass names, selectors, class kind.
fn extract_tags_from_module(module: &Module) -> Vec<String> {
    let mut tags = BTreeSet::new();
    for class in &module.classes {
        tags.insert(class.name.name.to_string());
        if let Some(ref superclass) = class.superclass {
            tags.insert(superclass.name.to_string());
        }
        tags.insert(format!("{:?}", class.class_kind).to_lowercase());
        extract_method_tags(class, &mut tags);
    }
    tags.into_iter().collect()
}

/// Extract method selector names from a class definition.
fn extract_method_tags(class: &ClassDefinition, tags: &mut BTreeSet<String>) {
    for method in &class.methods {
        let name = method.selector.name();
        if !name.is_empty() {
            tags.insert(name.to_string());
        }
    }
    for method in &class.class_methods {
        let name = method.selector.name();
        if !name.is_empty() {
            tags.insert(name.to_string());
        }
    }
}

/// Generate a slug-style ID from a path and optional suffix.
fn make_id(category: &str, file_stem: &str, suffix: Option<&str>) -> String {
    let base = format!("{category}-{file_stem}");
    let id = match suffix {
        Some(s) => format!("{base}-{s}"),
        None => base,
    };
    id.to_lowercase()
        .replace(|c: char| !c.is_alphanumeric() && c != '-', "-")
        .replace("--", "-")
        .trim_matches('-')
        .to_string()
}

/// Generate a human-readable title from a file stem.
fn title_from_stem(stem: &str) -> String {
    stem.replace(['_', '-'], " ")
        .split_whitespace()
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(c) => {
                    let upper: String = c.to_uppercase().collect();
                    upper + chars.as_str()
                }
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// Derive a category from a file path within the repo.
fn category_from_path(rel_path: &str) -> String {
    if rel_path.starts_with("stdlib/test/fixtures") {
        "stdlib-fixtures".to_string()
    } else if rel_path.starts_with("tests/e2e/fixtures") {
        "e2e-fixtures".to_string()
    } else if rel_path.starts_with("docs/learning/fixtures") {
        "learning-fixtures".to_string()
    } else if rel_path.starts_with("stdlib/test") {
        "stdlib-tests".to_string()
    } else if rel_path.starts_with("examples/") {
        // Extract project name: examples/bank/src/... -> examples-bank
        let parts: Vec<&str> = rel_path.split('/').collect();
        if parts.len() >= 2 {
            format!("examples-{}", parts[1])
        } else {
            "examples".to_string()
        }
    } else if rel_path.starts_with("docs/learning") {
        "learning".to_string()
    } else if rel_path.contains("language-features") {
        "language-reference".to_string()
    } else {
        "other".to_string()
    }
}

/// Generate an explanation from the file's leading comments.
fn explanation_from_comments(source: &str) -> String {
    let mut lines = Vec::new();
    let mut found_comment = false;
    for line in source.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("// Copyright") || trimmed.starts_with("// SPDX") {
            continue;
        }
        // Skip blank lines before the first comment
        if trimmed.is_empty() && !found_comment {
            continue;
        }
        if let Some(comment) = trimmed.strip_prefix("// ") {
            found_comment = true;
            lines.push(comment.to_string());
        } else if trimmed == "//" {
            found_comment = true;
            lines.push(String::new());
        } else {
            break;
        }
    }
    // Trim trailing empty lines
    while lines.last().is_some_and(String::is_empty) {
        lines.pop();
    }
    if lines.is_empty() {
        String::new()
    } else {
        lines.join(" ").trim().to_string()
    }
}

/// Walk a directory for .bt files in sorted order.
fn walk_bt_files(dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    if !dir.exists() {
        return files;
    }
    let Ok(entries) = fs::read_dir(dir) else {
        return files;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_file() && path.extension().is_some_and(|ext| ext == "bt") {
            files.push(path);
        } else if path.is_dir() {
            files.extend(walk_bt_files(&path));
        }
    }
    files.sort();
    files
}

/// Extract .bt files from a directory as whole-file corpus entries.
///
/// If `skip_containing` is set, files whose relative path contains that
/// substring are skipped (e.g. `"/fixtures/"` to avoid double-processing).
fn extract_bt_dir(
    root: &Path,
    rel_dir: &str,
    skip_containing: Option<&str>,
    entries: &mut Vec<CorpusEntry>,
) {
    let dir = root.join(rel_dir);
    for path in walk_bt_files(&dir) {
        let rel_path = path
            .strip_prefix(root)
            .unwrap_or(&path)
            .to_string_lossy()
            .to_string();

        if let Some(skip) = skip_containing {
            if rel_path.contains(skip) {
                continue;
            }
        }

        let Ok(source) = fs::read_to_string(&path) else {
            continue;
        };
        let stem = path.file_stem().unwrap_or_default().to_string_lossy();
        let category = category_from_path(&rel_path);
        let module = parse_bt(&source);
        let mut tags = extract_tags_from_module(&module);
        tags.push(stem.to_string());

        let title = if module.classes.is_empty() {
            title_from_stem(&stem)
        } else {
            module
                .classes
                .iter()
                .map(|c| c.name.name.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        };

        let explanation = explanation_from_comments(&source);

        entries.push(CorpusEntry {
            id: make_id(&category, &stem, None),
            title,
            category,
            tags,
            source: strip_license_header(&source),
            explanation,
        });
    }
}

/// Extract fenced beamtalk code blocks from markdown files in a directory.
fn extract_md_files(root: &Path, rel_dir: &str, entries: &mut Vec<CorpusEntry>) {
    let dir = root.join(rel_dir);
    let mut md_files: Vec<PathBuf> = Vec::new();
    if let Ok(read_dir) = fs::read_dir(&dir) {
        for entry in read_dir.flatten() {
            let path = entry.path();
            if path.is_file() && path.extension().is_some_and(|ext| ext == "md") {
                md_files.push(path);
            }
        }
    }
    md_files.sort();

    for path in md_files {
        extract_md_code_blocks(root, &path, entries);
    }
}

/// Extract fenced beamtalk code blocks from a single markdown file.
fn extract_md_code_blocks(root: &Path, path: &Path, entries: &mut Vec<CorpusEntry>) {
    let Ok(content) = fs::read_to_string(path) else {
        return;
    };
    let rel_path = path
        .strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .to_string();
    let stem = path.file_stem().unwrap_or_default().to_string_lossy();
    let category = category_from_path(&rel_path);

    let parser = pulldown_cmark::Parser::new(&content);

    let mut in_beamtalk_block = false;
    let mut block_content = String::new();
    let mut block_index = 0u32;
    let mut current_heading = String::new();

    for event in parser {
        match event {
            Event::Start(Tag::Heading { .. }) => {
                current_heading.clear();
            }
            Event::Text(text) => {
                if in_beamtalk_block {
                    block_content.push_str(&text);
                } else if current_heading.is_empty() && text.len() < 200 {
                    // Capture heading text for context
                    current_heading = text.to_string();
                }
            }
            Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(lang))) => {
                if lang.as_ref() == "beamtalk" {
                    in_beamtalk_block = true;
                    block_content.clear();
                }
            }
            Event::End(TagEnd::CodeBlock) => {
                if in_beamtalk_block {
                    in_beamtalk_block = false;
                    let code = block_content.trim().to_string();
                    // Skip very short blocks (< 3 lines) — trivial arithmetic examples
                    let line_count = code.lines().count();
                    if line_count >= 3 && parses_cleanly(&code) {
                        let module = parse_bt(&code);
                        let mut tags = extract_tags_from_module(&module);
                        tags.push(stem.to_string());

                        let title = if current_heading.is_empty() {
                            format!("{} block {}", title_from_stem(&stem), block_index)
                        } else {
                            format!("{} ({})", current_heading.trim(), stem)
                        };

                        entries.push(CorpusEntry {
                            id: make_id(&category, &stem, Some(&format!("block-{block_index}"))),
                            title,
                            category: category.clone(),
                            tags,
                            source: code,
                            explanation: format!("Code example from {rel_path}"),
                        });
                    }
                    block_index += 1;
                }
            }
            _ => {}
        }
    }
}

/// Strip the license header (Copyright + SPDX lines) from source.
fn strip_license_header(source: &str) -> String {
    source
        .lines()
        .skip_while(|line| {
            let trimmed = line.trim();
            trimmed.starts_with("// Copyright")
                || trimmed.starts_with("// SPDX")
                || trimmed.is_empty()
        })
        .collect::<Vec<_>>()
        .join("\n")
        .trim()
        .to_string()
}

/// Synonym map: concepts that agents commonly search for mapped to Beamtalk terminology.
const SYNONYMS: &[(&str, &[&str])] = &[
    (
        "do:",
        &[
            "loop",
            "iterate",
            "for each",
            "forEach",
            "for-each",
            "iteration",
        ],
    ),
    ("collect:", &["map", "transform", "mapping"]),
    ("select:", &["filter", "where", "filtering"]),
    ("reject:", &["filter out", "exclude"]),
    (
        "inject:into:",
        &["reduce", "fold", "accumulate", "aggregate"],
    ),
    ("detect:", &["find", "search", "first match"]),
    (
        "blocks",
        &["lambda", "closure", "anonymous function", "callback"],
    ),
    ("new", &["constructor", "instantiate", "create"]),
    (
        "Actor",
        &["async", "concurrent", "process", "gen_server", "genserver"],
    ),
    ("ifTrue:", &["if", "conditional", "branch"]),
    ("ifFalse:", &["if not", "unless", "negation"]),
    ("ifTrue:ifFalse:", &["if else", "ternary", "conditional"]),
    (
        "printString",
        &["toString", "to_string", "string conversion"],
    ),
    (
        "subclass:",
        &["inherit", "inheritance", "extends", "subclassing"],
    ),
    ("error:", &["throw", "raise", "exception"]),
    (
        "try:catch:",
        &["exception handling", "error handling", "try catch"],
    ),
    ("++", &["concatenate", "concat", "string join", "append"]),
    ("Supervisor", &["supervision", "fault tolerance", "restart"]),
    (
        "state:",
        &["instance variable", "field", "property", "member"],
    ),
    (
        "respondsTo:",
        &["duck typing", "protocol", "interface", "method check"],
    ),
    (":=", &["assignment", "assign", "set", "mutation", "mutate"]),
];

/// Add synonym tags to entries based on their existing tags and source content.
fn add_synonym_tags(entries: &mut [CorpusEntry]) {
    for entry in entries.iter_mut() {
        let mut new_tags = Vec::new();
        let source_lower = entry.source.to_lowercase();
        let existing_tags_lower: Vec<String> =
            entry.tags.iter().map(|t| t.to_lowercase()).collect();

        for &(beamtalk_term, synonyms) in SYNONYMS {
            let term_lower = beamtalk_term.to_lowercase();
            let has_term = existing_tags_lower.iter().any(|t| t.contains(&term_lower))
                || source_lower.contains(&term_lower);

            if has_term {
                for &synonym in synonyms {
                    new_tags.push(synonym.to_string());
                }
            }
        }
        entry.tags.extend(new_tags);
    }
}
