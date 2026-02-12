// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Doc test extraction: parse `// =>` assertions from `///` doc comments.
//!
//! **DDD Context:** CLI / Test System
//!
//! Scans doc comments on classes and methods for Markdown code blocks
//! (` ```beamtalk `) containing `// =>` assertions. Each code block becomes
//! a stateful test group where variable bindings persist across lines.
//!
//! Part of ADR 0014 (Beamtalk Test Framework) — doc tests.

use beamtalk_core::ast::Module;

// ──────────────────────────────────────────────────────────────────────────
// Data structures
// ──────────────────────────────────────────────────────────────────────────

/// What a doc test assertion expects: a value or an error.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expected {
    /// Match formatted result string (`_` for wildcard).
    Value(String),
    /// Match `#beamtalk_error{kind = Kind}` on error.
    Error { kind: String },
}

/// A single expression + assertion pair from a doc comment.
#[derive(Debug)]
pub(crate) struct DocTestCase {
    /// The Beamtalk expression to evaluate.
    pub expression: String,
    /// Expected outcome (value or error).
    pub expected: Expected,
    /// Absolute line number in the source file (1-based).
    pub source_line: usize,
}

/// A group of test cases from a single code block in a doc comment.
#[derive(Debug)]
pub(crate) struct DocTestGroup {
    /// Test cases within this code block (stateful: bindings persist).
    pub cases: Vec<DocTestCase>,
}

/// All doc tests extracted from a single class.
#[derive(Debug)]
pub(crate) struct ClassDocTests {
    /// The class name (e.g., `Counter`).
    pub class_name: String,
    /// Doc test groups from this class's doc comments.
    pub groups: Vec<DocTestGroup>,
}

// ──────────────────────────────────────────────────────────────────────────
// Extraction logic
// ──────────────────────────────────────────────────────────────────────────

/// Extract doc test cases from a parsed module and its source content.
///
/// Scans class-level and method-level `///` doc comments for
/// ` ```beamtalk ` code blocks containing `// =>` assertions.
///
/// Returns one `ClassDocTests` per class that has doc assertions,
/// or an empty vec if no doc tests are found.
pub(crate) fn extract_doc_tests(module: &Module, source: &str) -> Vec<ClassDocTests> {
    let source_lines: Vec<&str> = source.lines().collect();
    let mut results = Vec::new();

    for class in &module.classes {
        let mut all_groups = Vec::new();

        // Extract from class-level doc comment
        if let Some(ref doc) = class.doc_comment {
            let doc_start_line = find_doc_comment_start(&source_lines, class.span.start());
            let groups = extract_from_doc_comment(doc, doc_start_line);
            all_groups.extend(groups);
        }

        // Extract from method-level doc comments
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            if let Some(ref doc) = method.doc_comment {
                let doc_start_line = find_doc_comment_start(&source_lines, method.span.start());
                let groups = extract_from_doc_comment(doc, doc_start_line);
                all_groups.extend(groups);
            }
        }

        if !all_groups.is_empty() {
            results.push(ClassDocTests {
                class_name: class.name.name.to_string(),
                groups: all_groups,
            });
        }
    }

    results
}

/// Find the source line number where a doc comment block starts,
/// by scanning backwards from the definition's byte offset.
fn find_doc_comment_start(source_lines: &[&str], definition_byte_offset: u32) -> usize {
    // Convert byte offset to line number (1-based)
    let mut bytes_seen: u32 = 0;
    let mut def_line = 1; // 1-based
    for (i, line) in source_lines.iter().enumerate() {
        let line_bytes = u32::try_from(line.len()).unwrap_or(0) + 1; // +1 for newline
        if bytes_seen + line_bytes > definition_byte_offset {
            def_line = i + 1;
            break;
        }
        bytes_seen += line_bytes;
    }

    // Scan backwards from def_line to find the first `///` line
    let mut first_doc_line = def_line;
    for i in (0..def_line.saturating_sub(1)).rev() {
        let trimmed = source_lines[i].trim();
        if trimmed.starts_with("///") {
            first_doc_line = i + 1; // 1-based
        } else if !trimmed.is_empty() {
            break;
        }
    }

    first_doc_line
}

/// Extract doc test groups from a single doc comment string.
///
/// Finds ` ```beamtalk ` code blocks and parses `// =>` assertions
/// within them.
fn extract_from_doc_comment(doc: &str, doc_start_line: usize) -> Vec<DocTestGroup> {
    let mut groups = Vec::new();
    let doc_lines: Vec<&str> = doc.lines().collect();
    let mut i = 0;

    while i < doc_lines.len() {
        let trimmed = doc_lines[i].trim();

        // Look for opening code fence: ```beamtalk
        if is_beamtalk_code_fence(trimmed) {
            i += 1;
            let block_start = i;
            let mut block_lines = Vec::new();

            // Collect lines until closing ```
            while i < doc_lines.len() {
                let line = doc_lines[i].trim();
                if line.starts_with("```") {
                    break;
                }
                block_lines.push((i, line));
                i += 1;
            }

            // Parse assertions from the code block
            let cases = parse_code_block(&block_lines, doc_start_line + block_start);
            if !cases.is_empty() {
                groups.push(DocTestGroup { cases });
            }
        }

        i += 1;
    }

    groups
}

/// Check if a line is a beamtalk code fence opener.
fn is_beamtalk_code_fence(line: &str) -> bool {
    let line = line.trim();
    line == "```beamtalk" || line == "``` beamtalk"
}

/// Parse expression + `// =>` assertion pairs from code block lines.
///
/// Lines without `// =>` on the next line are treated as setup expressions
/// that still need to run (they get a wildcard `_` assertion).
fn parse_code_block(lines: &[(usize, &str)], absolute_line_offset: usize) -> Vec<DocTestCase> {
    let mut cases = Vec::new();
    let mut i = 0;

    while i < lines.len() {
        let (rel_line, line_text) = lines[i];
        let trimmed = line_text.trim();

        // Skip empty lines and standalone comments
        if trimmed.is_empty() || (trimmed.starts_with("//") && !trimmed.starts_with("// =>")) {
            i += 1;
            continue;
        }

        // Skip orphaned assertion markers
        if trimmed.starts_with("// =>") {
            i += 1;
            continue;
        }

        // Check for inline `// =>` on the same line
        if let Some((expr, expected)) = split_inline_assertion(trimmed) {
            cases.push(DocTestCase {
                expression: expr,
                expected: parse_expected(expected),
                source_line: absolute_line_offset + rel_line,
            });
            i += 1;
            continue;
        }

        // This is an expression — look for `// =>` on the next line
        let expression = trimmed.to_string();
        let expr_line = absolute_line_offset + rel_line;

        i += 1;
        if i < lines.len() {
            let next_trimmed = lines[i].1.trim();
            if let Some(expected_str) = next_trimmed.strip_prefix("// =>") {
                let expected_str = expected_str.trim();
                cases.push(DocTestCase {
                    expression,
                    expected: parse_expected(expected_str),
                    source_line: expr_line,
                });
                i += 1;
            } else {
                // No assertion — treat as setup (wildcard)
                cases.push(DocTestCase {
                    expression,
                    expected: Expected::Value("_".to_string()),
                    source_line: expr_line,
                });
            }
        } else {
            // Last line with no assertion — treat as setup (wildcard)
            cases.push(DocTestCase {
                expression,
                expected: Expected::Value("_".to_string()),
                source_line: expr_line,
            });
        }
    }

    cases
}

/// Try to split `expression // => expected` from a single line.
///
/// Doc examples often use inline assertions like `3 + 4  // => 7`.
fn split_inline_assertion(line: &str) -> Option<(String, &str)> {
    let marker = "// =>";
    let pos = line.find(marker)?;

    // Make sure this isn't at the start (would be an orphaned marker)
    if pos == 0 {
        return None;
    }

    let expr = line[..pos].trim();
    if expr.is_empty() {
        return None;
    }

    let expected = line[pos + marker.len()..].trim();
    Some((expr.to_string(), expected))
}

/// Parse an expected value string into an `Expected` enum.
fn parse_expected(s: &str) -> Expected {
    if let Some(kind) = s.strip_prefix("ERROR:") {
        let kind = kind.trim();
        if kind.is_empty() {
            Expected::Value("_".to_string())
        } else {
            Expected::Error {
                kind: kind.to_string(),
            }
        }
    } else {
        Expected::Value(s.to_string())
    }
}

// ──────────────────────────────────────────────────────────────────────────
// Tests
// ──────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_split_inline_assertion() {
        let (expr, expected) = split_inline_assertion("3 + 4  // => 7").unwrap();
        assert_eq!(expr, "3 + 4");
        assert_eq!(expected, "7");
    }

    #[test]
    fn test_split_inline_assertion_no_marker() {
        assert!(split_inline_assertion("3 + 4").is_none());
    }

    #[test]
    fn test_split_inline_assertion_at_start() {
        assert!(split_inline_assertion("// => 7").is_none());
    }

    #[test]
    fn test_parse_expected_value() {
        assert_eq!(parse_expected("7"), Expected::Value("7".to_string()));
        assert_eq!(parse_expected("_"), Expected::Value("_".to_string()));
    }

    #[test]
    fn test_parse_expected_error() {
        assert_eq!(
            parse_expected("ERROR:does_not_understand"),
            Expected::Error {
                kind: "does_not_understand".to_string()
            }
        );
    }

    #[test]
    fn test_parse_expected_wildcard() {
        assert_eq!(parse_expected("_"), Expected::Value("_".to_string()));
    }

    #[test]
    fn test_is_beamtalk_code_fence() {
        assert!(is_beamtalk_code_fence("```beamtalk"));
        assert!(is_beamtalk_code_fence("  ```beamtalk  "));
        assert!(!is_beamtalk_code_fence("```rust"));
        assert!(!is_beamtalk_code_fence("```"));
    }

    #[test]
    fn test_parse_code_block_inline_assertions() {
        let lines = vec![
            (0, "3 + 4         // => 7"),
            (1, "42 class      // => Integer"),
        ];
        let cases = parse_code_block(&lines, 10);
        assert_eq!(cases.len(), 2);
        assert_eq!(cases[0].expression, "3 + 4");
        assert_eq!(cases[0].expected, Expected::Value("7".to_string()));
        assert_eq!(cases[0].source_line, 10);
        assert_eq!(cases[1].expression, "42 class");
        assert_eq!(cases[1].expected, Expected::Value("Integer".to_string()));
        assert_eq!(cases[1].source_line, 11);
    }

    #[test]
    fn test_parse_code_block_next_line_assertions() {
        let lines = vec![
            (0, "counter := Counter spawn"),
            (1, "// => _"),
            (2, "counter increment await"),
            (3, "// => 1"),
        ];
        let cases = parse_code_block(&lines, 5);
        assert_eq!(cases.len(), 2);
        assert_eq!(cases[0].expression, "counter := Counter spawn");
        assert_eq!(cases[0].expected, Expected::Value("_".to_string()));
        assert_eq!(cases[0].source_line, 5);
        assert_eq!(cases[1].expression, "counter increment await");
        assert_eq!(cases[1].expected, Expected::Value("1".to_string()));
        assert_eq!(cases[1].source_line, 7);
    }

    #[test]
    fn test_parse_code_block_setup_without_assertion() {
        let lines = vec![(0, "x := 42"), (1, "x + 1"), (2, "// => 43")];
        let cases = parse_code_block(&lines, 1);
        assert_eq!(cases.len(), 2);
        // First expression gets wildcard since next line is another expression
        assert_eq!(cases[0].expression, "x := 42");
        assert_eq!(cases[0].expected, Expected::Value("_".to_string()));
        assert_eq!(cases[1].expression, "x + 1");
        assert_eq!(cases[1].expected, Expected::Value("43".to_string()));
    }

    #[test]
    fn test_parse_code_block_skips_comments() {
        let lines = vec![(0, "// setup"), (1, "3 + 4"), (2, "// => 7")];
        let cases = parse_code_block(&lines, 1);
        assert_eq!(cases.len(), 1);
        assert_eq!(cases[0].expression, "3 + 4");
    }

    #[test]
    fn test_parse_code_block_empty() {
        let cases = parse_code_block(&[], 1);
        assert!(cases.is_empty());
    }

    #[test]
    fn test_extract_from_doc_comment_with_code_block() {
        let doc = "A counter class.\n\n## Examples\n```beamtalk\n3 + 4  // => 7\n```";
        let groups = extract_from_doc_comment(doc, 1);
        assert_eq!(groups.len(), 1);
        assert_eq!(groups[0].cases.len(), 1);
        assert_eq!(groups[0].cases[0].expression, "3 + 4");
    }

    #[test]
    fn test_extract_from_doc_comment_no_code_block() {
        let doc = "A simple class without examples.";
        let groups = extract_from_doc_comment(doc, 1);
        assert!(groups.is_empty());
    }

    #[test]
    fn test_extract_from_doc_comment_non_beamtalk_block() {
        let doc = "Example:\n```erlang\nio:format(\"hello\").\n```";
        let groups = extract_from_doc_comment(doc, 1);
        assert!(groups.is_empty());
    }

    #[test]
    fn test_extract_from_doc_comment_multiple_blocks() {
        let doc = "## Block 1\n```beamtalk\n1 + 2  // => 3\n```\n\n## Block 2\n```beamtalk\n4 + 5  // => 9\n```";
        let groups = extract_from_doc_comment(doc, 1);
        assert_eq!(groups.len(), 2);
    }

    #[test]
    fn test_extract_from_doc_comment_block_without_assertions() {
        let doc = "```beamtalk\n// just a comment, no assertions\n```";
        let groups = extract_from_doc_comment(doc, 1);
        assert!(groups.is_empty());
    }

    #[test]
    fn test_extract_doc_tests_empty_module() {
        let module = Module::new(vec![], beamtalk_core::source_analysis::Span::new(0, 0));
        let results = extract_doc_tests(&module, "");
        assert!(results.is_empty());
    }

    #[test]
    fn test_error_assertion_in_doc() {
        let lines = vec![(0, "42 foo"), (1, "// => ERROR:does_not_understand")];
        let cases = parse_code_block(&lines, 1);
        assert_eq!(cases.len(), 1);
        assert_eq!(
            cases[0].expected,
            Expected::Error {
                kind: "does_not_understand".to_string()
            }
        );
    }
}
