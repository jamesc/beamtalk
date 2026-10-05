// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Test-support query: escaping closures that read a class variable.
//!
//! **DDD Context:** Compilation (test support, not a shipped surface)
//!
//! ADR 0130 §5 says a block that outlives the class-method invocation that
//! created it reads the class variables as they were at creation and cannot
//! write them. Phase 0 (BT-3703) counts how often existing code builds such a
//! block, so the Migration Path and the Phase 3 `class-state-abroad` lint
//! (BT-3712) are tuned to shapes that occur.
//!
//! An *escaping closure* here is a block literal, inside a class method, that
//! reads a class variable of its class (or of a superclass found in the same
//! corpus) and is **returned** or **stored** by that method:
//!
//! | [`EscapeShape`]        | Source shape                                       |
//! |------------------------|----------------------------------------------------|
//! | `Returned`             | last statement of the method, or `^ [...]`         |
//! | `StoredLocal`          | `name := [...]`                                    |
//! | `StoredClassVar`       | `self.name := [...]`                               |
//! | `StoredInLiteral`      | block is an element of a list/array/map literal   |
//!
//! The query is purely syntactic. A block passed as a message argument is not
//! counted here (whether it escapes depends on the callee); the runtime probe
//! (`BEAMTALK_CLASS_VAR_PROBE=1`, `beamtalk_class_var_probe`) covers those.
//! A block returned from inside a nested inlined conditional branch is not
//! seen (only the method's own last statement and explicit `^`).
//!
//! Gated like `test_helpers::test_support`: it exists for tests and for the
//! one-off census, never for the compiler or any user-facing surface.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::ast::{Block, ClassDefinition, Expression, MethodDefinition, Module};
use crate::ast_walker::walk_expression;
use crate::source_analysis::{Severity, lex_with_eof, parse};

/// How an escaping closure leaves its creating class method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum EscapeShape {
    /// Returned: the method's last statement, or the operand of `^`.
    Returned,
    /// Assigned to a local variable (`b := [...]`).
    StoredLocal,
    /// Assigned to a class variable (`self.cb := [...]`).
    StoredClassVar,
    /// An element of a list, array or map literal.
    StoredInLiteral,
}

/// One escaping closure that reads a class variable.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EscapeSite {
    /// Corpus file the block was found in.
    pub file: String,
    /// Class whose class method contains the block.
    pub class: String,
    /// Class-method selector.
    pub selector: String,
    /// 1-based source line of the block's opening bracket.
    pub line: usize,
    /// How the block escapes.
    pub shape: EscapeShape,
    /// Class variables the block reads, sorted.
    pub reads: Vec<String>,
}

/// A parsed source file of the census corpus.
#[derive(Debug)]
pub struct CorpusFile {
    /// Display path.
    pub path: String,
    /// Source text (for line numbers).
    pub source: String,
    /// Parsed module.
    pub module: Module,
}

/// Result of scanning a corpus.
#[derive(Debug, Default)]
pub struct CorpusScan {
    /// Files that parsed without errors.
    pub files: Vec<CorpusFile>,
    /// Files skipped because they did not parse.
    pub unparsed: Vec<String>,
}

/// Parses one source text; `None` when it has parse errors.
#[must_use]
pub fn parse_corpus_source(path: &str, source: &str) -> Option<CorpusFile> {
    let (module, diagnostics) = parse(lex_with_eof(source));
    if diagnostics.iter().any(|d| d.severity == Severity::Error) {
        return None;
    }
    Some(CorpusFile {
        path: path.to_string(),
        source: source.to_string(),
        module,
    })
}

/// Recursively collects and parses every `.bt` / `.btscript` file under `roots`.
#[must_use]
pub fn scan_corpus(roots: &[PathBuf]) -> CorpusScan {
    let mut paths = Vec::new();
    for root in roots {
        collect_sources(root, &mut paths);
    }
    paths.sort();
    let mut scan = CorpusScan::default();
    for path in paths {
        let display = path.display().to_string();
        let Ok(source) = std::fs::read_to_string(&path) else {
            scan.unparsed.push(display);
            continue;
        };
        match parse_corpus_source(&display, &source) {
            Some(file) => scan.files.push(file),
            None => scan.unparsed.push(display),
        }
    }
    scan
}

fn collect_sources(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        let Ok(file_type) = entry.file_type() else {
            continue;
        };
        if file_type.is_symlink() {
            continue;
        }
        if file_type.is_dir() {
            collect_sources(&path, out);
        } else if path
            .extension()
            .is_some_and(|ext| ext == "bt" || ext == "btscript")
        {
            out.push(path);
        }
    }
}

/// Finds every escaping closure that reads a class variable across `files`.
///
/// Class variables are resolved per class name across the whole corpus, so a
/// subclass in one file sees a superclass's `classState:` from another.
#[must_use]
pub fn escaping_class_var_closures(files: &[CorpusFile]) -> Vec<EscapeSite> {
    let mut own_vars: HashMap<String, HashSet<String>> = HashMap::new();
    let mut superclass: HashMap<String, String> = HashMap::new();
    for file in files {
        for class in &file.module.classes {
            own_vars
                .entry(class.name.name.to_string())
                .or_default()
                .extend(
                    class
                        .class_variables
                        .iter()
                        .map(|v| v.name.name.to_string()),
                );
            if let Some(sup) = &class.superclass {
                superclass.insert(class.name.name.to_string(), sup.name.to_string());
            }
        }
    }
    let class_vars_of = |class: &str| -> HashSet<String> {
        let mut vars = HashSet::new();
        let mut current = Some(class.to_string());
        let mut seen = HashSet::new();
        while let Some(name) = current {
            if !seen.insert(name.clone()) {
                break;
            }
            if let Some(own) = own_vars.get(&name) {
                vars.extend(own.iter().cloned());
            }
            current = superclass.get(&name).cloned();
        }
        vars
    };

    let mut sites = Vec::new();
    for file in files {
        for class in &file.module.classes {
            let vars = class_vars_of(&class.name.name);
            if vars.is_empty() {
                continue;
            }
            for method in &class.class_methods {
                scan_method(file, class, method, &vars, &mut sites);
            }
        }
    }
    sites
}

/// Counts `sites` by shape.
#[must_use]
pub fn count_by_shape(sites: &[EscapeSite]) -> std::collections::BTreeMap<EscapeShape, usize> {
    let mut counts = std::collections::BTreeMap::new();
    for site in sites {
        *counts.entry(site.shape).or_insert(0) += 1;
    }
    counts
}

fn scan_method(
    file: &CorpusFile,
    class: &ClassDefinition,
    method: &MethodDefinition,
    vars: &HashSet<String>,
    sites: &mut Vec<EscapeSite>,
) {
    let mut record = |block: &Block, shape: EscapeShape| {
        let reads = class_var_reads(block, vars);
        if reads.is_empty() {
            return;
        }
        sites.push(EscapeSite {
            file: file.path.clone(),
            class: class.name.name.to_string(),
            selector: method.selector.name().to_string(),
            line: line_of(&file.source, block.span.start()),
            shape,
            reads,
        });
    };

    if let Some(last) = method.body.last() {
        if let Some(block) = as_block(&last.expression) {
            record(block, EscapeShape::Returned);
        }
    }
    for stmt in &method.body {
        walk_expression(&stmt.expression, &mut |expr| match expr {
            Expression::Return { value, .. } => {
                if let Some(block) = as_block(value) {
                    record(block, EscapeShape::Returned);
                }
            }
            Expression::Assignment { target, value, .. } => {
                if let Some(block) = as_block(value) {
                    let shape = if matches!(**target, Expression::FieldAccess { .. }) {
                        EscapeShape::StoredClassVar
                    } else {
                        EscapeShape::StoredLocal
                    };
                    record(block, shape);
                }
            }
            Expression::ListLiteral { elements, .. }
            | Expression::ArrayLiteral { elements, .. } => {
                for block in elements.iter().filter_map(as_block) {
                    record(block, EscapeShape::StoredInLiteral);
                }
            }
            Expression::MapLiteral { pairs, .. } => {
                for block in pairs.iter().filter_map(|p| as_block(&p.value)) {
                    record(block, EscapeShape::StoredInLiteral);
                }
            }
            _ => {}
        });
    }
}

/// The block literal an expression is, looking through parentheses.
fn as_block(expr: &Expression) -> Option<&Block> {
    match expr {
        Expression::Block(block) => Some(block),
        Expression::Parenthesized { expression, .. } => as_block(expression),
        _ => None,
    }
}

/// Class variables read (as `self.name`) anywhere inside `block`, including
/// nested blocks. A `self.name := v` write target is not a read.
fn class_var_reads(block: &Block, vars: &HashSet<String>) -> Vec<String> {
    let mut written_targets = Vec::new();
    let mut reads = std::collections::BTreeSet::new();
    let root = Expression::Block(block.clone());
    walk_expression(&root, &mut |expr| {
        if let Expression::Assignment { target, .. } = expr {
            if let Expression::FieldAccess { span, .. } = &**target {
                written_targets.push(*span);
            }
        }
        if let Expression::FieldAccess {
            receiver,
            field,
            span,
        } = expr
        {
            let is_self = matches!(&**receiver, Expression::Identifier(id) if id.name == "self");
            if is_self && vars.contains(field.name.as_str()) && !written_targets.contains(span) {
                reads.insert(field.name.to_string());
            }
        }
    });
    reads.into_iter().collect()
}

fn line_of(source: &str, offset: u32) -> usize {
    let end = (offset as usize).min(source.len());
    source.as_bytes()[..end]
        .iter()
        .filter(|b| **b == b'\n')
        .count()
        + 1
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sites_for(source: &str) -> Vec<EscapeSite> {
        let file = parse_corpus_source("inline.bt", source).expect("parses");
        escaping_class_var_closures(&[file])
    }

    fn shapes(source: &str) -> Vec<(String, EscapeShape)> {
        sites_for(source)
            .into_iter()
            .map(|s| (s.selector, s.shape))
            .collect()
    }

    #[test]
    fn returned_block_reading_class_var_is_counted() {
        let src = "Object subclass: Foo\n  classState: n = 0\n  class reader => [self.n]\n";
        assert_eq!(shapes(src), vec![("reader".into(), EscapeShape::Returned)]);
    }

    #[test]
    fn explicit_return_of_block_is_counted() {
        let src = "Object subclass: Foo\n  classState: n = 0\n  class reader =>\n    ^[self.n]\n    nil\n";
        assert_eq!(shapes(src), vec![("reader".into(), EscapeShape::Returned)]);
    }

    #[test]
    fn stored_local_and_class_var_are_counted() {
        let src = "Object subclass: Foo\n  classState: n = 0\n  classState: cb = nil\n  class a =>\n    b := [self.n]\n    b value\n  class b =>\n    self.cb := [self.n]\n    nil\n";
        let mut got = shapes(src);
        got.sort();
        assert_eq!(
            got,
            vec![
                ("a".into(), EscapeShape::StoredLocal),
                ("b".into(), EscapeShape::StoredClassVar),
            ]
        );
    }

    #[test]
    fn block_in_list_literal_is_stored_in_literal() {
        let src = "Object subclass: Foo\n  classState: n = 0\n  class cbs => #([self.n], [1])\n";
        assert_eq!(
            shapes(src),
            vec![("cbs".into(), EscapeShape::StoredInLiteral)]
        );
    }

    #[test]
    fn block_not_reading_a_class_var_is_ignored() {
        let src = "Object subclass: Foo\n  classState: n = 0\n  class pure => [1 + 2]\n";
        assert!(shapes(src).is_empty());
    }

    #[test]
    fn instance_method_blocks_and_write_only_blocks_are_ignored() {
        let src = "Object subclass: Foo\n  classState: n = 0\n  class bump => [self.n := 1]\n  value => [2]\n";
        assert!(shapes(src).is_empty());
    }

    #[test]
    fn inherited_class_var_is_resolved_across_files() {
        let base = parse_corpus_source("base.bt", "Object subclass: Base\n  classState: n = 0\n")
            .expect("parses");
        let sub = parse_corpus_source("sub.bt", "Base subclass: Sub\n  class reader => [self.n]\n")
            .expect("parses");
        let sites = escaping_class_var_closures(&[base, sub]);
        assert_eq!(sites.len(), 1);
        assert_eq!(sites[0].class, "Sub");
        assert_eq!(sites[0].reads, vec!["n".to_string()]);
    }

    /// The Phase 0 census: counts escaping closures over the three corpora
    /// named by BT-3703 and prints the table the PR report quotes
    /// (`cargo test -p beamtalk-core --features test census -- --nocapture`).
    /// It asserts only that the scan found the corpus; the counts are data,
    /// not a gate (the gate is recorded in the ADR's Migration Path).
    #[test]
    fn census_over_repository_corpus() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let roots = [
            root.join("stdlib/test"),
            root.join("test-package-compiler/cases"),
            root.join("tests/repl-protocol/cases"),
            // The classes the REPL-protocol scripts `:load` live here.
            root.join("tests/repl-protocol/fixtures"),
        ];
        let scan = scan_corpus(&roots);
        assert!(!scan.files.is_empty(), "corpus not found under {root:?}");
        let sites = escaping_class_var_closures(&scan.files);
        println!(
            "class-var escaping-closure census: {} files parsed, {} unparsed, {} sites",
            scan.files.len(),
            scan.unparsed.len(),
            sites.len()
        );
        for (shape, count) in count_by_shape(&sites) {
            println!("  {shape:?}: {count}");
        }
        for site in &sites {
            println!(
                "  {:?} {}>>{} {}:{} reads {:?}",
                site.shape, site.class, site.selector, site.file, site.line, site.reads
            );
        }
        // Unparsed files are mostly REPL scripts; flag only those that
        // declare class state, since those could hide a site.
        for path in &scan.unparsed {
            if std::fs::read_to_string(path).is_ok_and(|s| s.contains("classState:")) {
                println!("  unparsed with classState: {path}");
            }
        }
    }
}
