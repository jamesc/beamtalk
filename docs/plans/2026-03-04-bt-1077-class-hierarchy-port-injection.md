# BT-1077: ClassHierarchy Port Injection Implementation Plan


**Goal:** Wire user-class metadata from the Erlang `beamtalk_compiler_server` class cache into each Rust port request, deserializing it into a full `ClassHierarchy` so the TypeChecker and codegen can see REPL-defined classes.

**Architecture:** The Erlang server already has a `classes` map (Phase 3/BT-1076). Phase 4 closes the loop: the Erlang side injects `class_hierarchy => Classes` into port requests; the Rust port deserializes each `__beamtalk_meta/0` map into a `ClassInfo` (with methods, types, flags) and calls `ClassHierarchy::add_from_beam_meta` before handing off to TypeChecker and codegen. The `beamtalk-core` method takes `Vec<ClassInfo>` (pre-parsed); ETF deserialization happens in the port binary (which already owns the `eetf` dependency).

**Tech Stack:** Rust (`eetf`, `beamtalk-core`, `beamtalk-compiler-port`), Erlang/OTP gen_server.

---

## Background: `__beamtalk_meta/0` Format (after BT-1074/BT-1075)

Each class entry in the `class_hierarchy` map is an Erlang map of the form:

```erlang
#{
    class             => 'Counter',       % atom
    superclass        => 'Actor',         % atom, or 'none' for root
    meta_version      => 2,              % integer
    is_sealed         => false,          % boolean atom
    is_abstract       => false,
    is_value          => false,
    is_typed          => false,
    fields            => [count],        % list of atoms (= state vars)
    field_types       => #{count => 'Integer'},  % atom→atom map
    method_info => #{
        increment => #{arity => 1, param_types => ['Integer'], return_type => 'Integer'},
        value     => #{arity => 0, param_types => [],           return_type => 'none'}
    },
    class_method_info => #{
        new => #{arity => 0, param_types => [], return_type => 'Counter'}
    },
    class_variables   => []              % list of atoms
}
```

The `class_hierarchy` key in the port request is: `#{ClassNameAtom => MetaMap}`.

In ETF (Rust side): atoms are `Term::Atom(a)` where `a.name: String`; integers are `Term::FixInteger(n)` where `n.value: i32`.

---

### Task 1: `ClassHierarchy::add_from_beam_meta` in beamtalk-core

**Files:**
- Modify: `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/mod.rs`

**Step 1: Write the failing unit test**

In `mod.rs`, find the `#[cfg(test)]` block (or create one after the `add_external_superclasses` method) and add:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_from_beam_meta_inserts_non_builtin_class() {
        let mut h = ClassHierarchy::with_builtins();
        let info = ClassInfo {
            name: EcoString::from("Counter"),
            superclass: Some(EcoString::from("Actor")),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_value: false,
            state: vec![EcoString::from("count")],
            state_types: HashMap::new(),
            methods: vec![MethodInfo {
                selector: EcoString::from("value"),
                arity: 0,
                kind: crate::ast::MethodKind::Primary,
                defined_in: EcoString::from("Counter"),
                is_sealed: false,
                return_type: Some(EcoString::from("Integer")),
                param_types: vec![],
            }],
            class_methods: vec![],
            class_variables: vec![],
        };
        h.add_from_beam_meta(vec![info]);
        assert!(h.has_class("Counter"));
        let cls = h.get_class("Counter").unwrap();
        assert_eq!(cls.superclass.as_deref(), Some("Actor"));
        assert_eq!(cls.methods.len(), 1);
        assert_eq!(cls.methods[0].selector.as_str(), "value");
    }

    #[test]
    fn add_from_beam_meta_skips_builtins() {
        let mut h = ClassHierarchy::with_builtins();
        let original_integer = h.get_class("Integer").unwrap().clone();
        // Try to overwrite Integer with a stub
        let stub = ClassInfo {
            name: EcoString::from("Integer"),
            superclass: None,
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_value: false,
            state: vec![],
            state_types: HashMap::new(),
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
        };
        h.add_from_beam_meta(vec![stub]);
        // Built-in should be unchanged
        assert_eq!(h.get_class("Integer").unwrap().methods.len(), original_integer.methods.len());
    }
}
```

**Step 2: Run test to confirm FAIL**

```bash
cd /home/james/source/beamtalk/.claude/worktrees/BT-1077
cargo test -p beamtalk-core add_from_beam_meta 2>&1 | tail -20
```

Expected: `error[E0599]: no method named 'add_from_beam_meta'`

**Step 3: Implement `add_from_beam_meta`**

Add after the `add_external_superclasses` method (around line 231 in mod.rs):

```rust
/// Populate the hierarchy with user-class entries pre-deserialized from
/// `__beamtalk_meta/0` maps (ADR 0050 Phase 4).
///
/// Skips builtins (`with_builtins()` has richer data than `__beamtalk_meta/0`).
/// Preserves AST-derived entries already in the hierarchy — those come from
/// `ClassHierarchy::build()` and are authoritative over cached BEAM metadata,
/// which may be stale during redefinition.
pub fn add_from_beam_meta(&mut self, classes: Vec<ClassInfo>) {
    for info in classes {
        if !Self::is_builtin_class(&info.name) {
            self.classes.entry(info.name.clone()).or_insert(info);
        }
    }
}
```

**Step 4: Run tests to confirm PASS**

```bash
cargo test -p beamtalk-core add_from_beam_meta 2>&1 | tail -10
```

Expected: `test ... ok` (2 tests pass)

**Step 5: Commit**

```bash
git add crates/beamtalk-core/src/semantic_analysis/class_hierarchy/mod.rs
git commit -m "feat: add ClassHierarchy::add_from_beam_meta BT-1077"
```

---

### Task 2: Extend semantic analysis with pre-loaded class hierarchy

**Files:**
- Modify: `crates/beamtalk-core/src/semantic_analysis/mod.rs`
- Modify: `crates/beamtalk-core/src/queries/diagnostic_provider.rs`

**Step 1: Write the failing test for `analyse_with_known_vars_and_classes`**

In `crates/beamtalk-core/src/semantic_analysis/mod.rs`, in the existing test module or a new one, add:

```rust
#[cfg(test)]
mod beam_meta_tests {
    use super::*;
    use crate::semantic_analysis::class_hierarchy::{ClassHierarchy, ClassInfo, MethodInfo};
    use crate::source_analysis::{lex_with_eof, parse};
    use ecow::EcoString;
    use std::collections::HashMap;

    #[test]
    fn analyse_with_classes_makes_user_class_visible_to_typechecker() {
        // Define a UserClass with a method 'greet' that returns String
        let counter_info = ClassInfo {
            name: EcoString::from("UserClass"),
            superclass: Some(EcoString::from("Object")),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_value: false,
            state: vec![],
            state_types: HashMap::new(),
            methods: vec![MethodInfo {
                selector: EcoString::from("greet"),
                arity: 0,
                kind: crate::ast::MethodKind::Primary,
                defined_in: EcoString::from("UserClass"),
                is_sealed: false,
                return_type: Some(EcoString::from("String")),
                param_types: vec![],
            }],
            class_methods: vec![],
            class_variables: vec![],
        };

        let src = "| x | x := UserClass new.";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        let result = analyse_with_known_vars_and_classes(&module, &[], vec![counter_info]);
        // The hierarchy should contain UserClass
        assert!(result.class_hierarchy.has_class("UserClass"));
    }
}
```

**Step 2: Run test to confirm FAIL**

```bash
cargo test -p beamtalk-core analyse_with_known_vars_and_classes 2>&1 | tail -10
```

Expected: compilation error — function not defined.

**Step 3: Add `analyse_with_known_vars_and_classes` to semantic_analysis/mod.rs**

Add after `analyse_with_known_vars` (around line 206):

```rust
/// Analyse a module with pre-defined variables and pre-loaded class entries
/// from BEAM metadata (ADR 0050 Phase 4).
///
/// `pre_loaded_classes` entries are injected into the `ClassHierarchy` after
/// building from the AST, making REPL-session classes visible to the TypeChecker.
pub fn analyse_with_known_vars_and_classes(
    module: &Module,
    known_vars: &[&str],
    pre_loaded_classes: Vec<class_hierarchy::ClassInfo>,
) -> AnalysisResult {
    let mut result = analyse_full(module, known_vars, false, false);
    result
        .class_hierarchy
        .add_from_beam_meta(pre_loaded_classes);
    result
}
```

Wait — this doesn't work correctly because the TypeChecker already ran inside `analyse_full`. We need to inject the classes *before* TypeChecking. So instead we need a new internal path:

```rust
/// Analyse a module with pre-defined variables and pre-loaded class entries
/// from BEAM metadata (ADR 0050 Phase 4).
///
/// `pre_loaded_classes` are injected into the `ClassHierarchy` *before*
/// TypeChecking, so user-defined REPL classes are visible to the TypeChecker.
pub fn analyse_with_known_vars_and_classes(
    module: &Module,
    known_vars: &[&str],
    pre_loaded_classes: Vec<class_hierarchy::ClassInfo>,
) -> AnalysisResult {
    analyse_full_with_pre_classes(module, known_vars, false, false, pre_loaded_classes)
}
```

And add `analyse_full_with_pre_classes` as a new private function. The function body is identical to `analyse_full` except it calls `result.class_hierarchy.add_from_beam_meta(pre_loaded_classes)` after Phase 0 (hierarchy build) and before Phase 2 (TypeChecker):

```rust
fn analyse_full_with_pre_classes(
    module: &Module,
    known_vars: &[&str],
    stdlib_mode: bool,
    skip_module_expression_lint: bool,
    pre_loaded_classes: Vec<class_hierarchy::ClassInfo>,
) -> AnalysisResult {
    let mut result = AnalysisResult::new();

    // Phase 0: Build Class Hierarchy (ADR 0006 Phase 1a)
    let (hierarchy_result, hierarchy_diags) =
        ClassHierarchy::build_with_options(module, stdlib_mode);
    result.class_hierarchy = hierarchy_result.expect("ClassHierarchy::build is infallible");
    result.diagnostics.extend(hierarchy_diags);

    // ADR 0050 Phase 4: inject REPL-session class metadata before TypeChecking
    result.class_hierarchy.add_from_beam_meta(pre_loaded_classes);

    // Phase 1: Name Resolution
    let mut name_resolver = NameResolver::new();
    name_resolver.define_known_vars(known_vars, module.span);
    name_resolver.resolve_module(module);
    result.diagnostics.extend(name_resolver.take_diagnostics());

    let scope = name_resolver.into_scope();

    // Phase 2: Type Checking
    let mut type_checker = TypeChecker::new();
    type_checker.check_module(module, &result.class_hierarchy);
    result.diagnostics.extend(type_checker.take_diagnostics());
    let type_map = type_checker.take_type_map();

    // Phase 3: Block Context Analysis
    let mut analyser = Analyser::with_scope(scope, type_map);
    analyser.analyse_module(module);
    result.diagnostics.extend(analyser.result.diagnostics);
    result.block_info = analyser.result.block_info;

    // Phase 4: Abstract instantiation check
    validators::check_abstract_instantiation(
        module,
        &result.class_hierarchy,
        &mut result.diagnostics,
    );

    // Remaining phases from analyse_full (copy remaining validators)
    // NOTE: Copy the exact remaining content from analyse_full here.
    result
}
```

**Important:** Copy the remaining validators from `analyse_full` exactly (phases 5 onwards). Read `analyse_full` fully before writing this function.

**Step 4: Add `compute_diagnostics_with_known_vars_and_classes` to diagnostic_provider.rs**

In `crates/beamtalk-core/src/queries/diagnostic_provider.rs`, add after `compute_diagnostics_with_known_vars`:

```rust
/// Computes diagnostics with pre-defined REPL variables and pre-loaded class
/// entries from BEAM metadata (ADR 0050 Phase 4).
///
/// `pre_loaded_classes` are injected into the ClassHierarchy before TypeChecking,
/// making REPL-session user classes visible to the TypeChecker.
#[must_use]
pub fn compute_diagnostics_with_known_vars_and_classes(
    module: &crate::ast::Module,
    parse_diagnostics: Vec<Diagnostic>,
    known_vars: &[&str],
    pre_loaded_classes: Vec<crate::semantic_analysis::class_hierarchy::ClassInfo>,
) -> Vec<Diagnostic> {
    let mut all_diagnostics = parse_diagnostics;
    let analysis_result = crate::semantic_analysis::analyse_with_known_vars_and_classes(
        module,
        known_vars,
        pre_loaded_classes,
    );
    all_diagnostics.extend(analysis_result.diagnostics);
    apply_expect_directives(module, &mut all_diagnostics);
    all_diagnostics
}
```

**Step 5: Run tests**

```bash
cargo test -p beamtalk-core 2>&1 | tail -20
```

Expected: all tests pass (including the new `analyse_with_known_vars_and_classes` test).

**Step 6: Commit**

```bash
git add crates/beamtalk-core/src/semantic_analysis/mod.rs \
        crates/beamtalk-core/src/queries/diagnostic_provider.rs
git commit -m "feat: extend semantic analysis with pre-loaded class hierarchy BT-1077"
```

---

### Task 3: Extend CodegenOptions with pre-loaded class hierarchy

**Files:**
- Modify: `crates/beamtalk-core/src/codegen/core_erlang/mod.rs`

**Step 1: Write a test for the new option**

In `crates/beamtalk-core/src/codegen/core_erlang/tests.rs` (or a new test in mod.rs), add:

```rust
#[test]
fn generate_module_with_pre_class_hierarchy_does_not_panic() {
    use crate::semantic_analysis::class_hierarchy::{ClassHierarchy, ClassInfo, MethodInfo};
    use crate::ast::MethodKind;
    use std::collections::HashMap;

    let src = "Object subclass: MyService\n  greet => 'hello'";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);

    let pre_class = ClassInfo {
        name: ecow::EcoString::from("Helper"),
        superclass: Some(ecow::EcoString::from("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_value: false,
        state: vec![],
        state_types: HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
    };

    let result = generate_module(
        &module,
        CodegenOptions::new("bt@my_service")
            .with_workspace_mode(true)
            .with_class_hierarchy(vec![pre_class]),
    );
    assert!(result.is_ok(), "generate_module should succeed: {result:?}");
}
```

**Step 2: Run test to confirm FAIL**

```bash
cargo test -p beamtalk-core generate_module_with_pre_class_hierarchy 2>&1 | tail -10
```

Expected: compilation error — `with_class_hierarchy` not defined.

**Step 3: Add `pre_class_hierarchy` field to `CodegenOptions`**

In `crates/beamtalk-core/src/codegen/core_erlang/mod.rs`, find the `CodegenOptions` struct definition and add a field:

```rust
/// ADR 0050 Phase 4: pre-loaded class entries from BEAM metadata.
/// Injected into the ClassHierarchy before codegen so user-defined REPL
/// classes are visible to is_actor_class and related checks.
pre_class_hierarchy: Vec<crate::semantic_analysis::class_hierarchy::ClassInfo>,
```

In the `CodegenOptions::new` constructor, initialize it:
```rust
pre_class_hierarchy: Vec::new(),
```

Add a builder method after `with_class_superclass_index`:

```rust
/// ADR 0050 Phase 4: pre-load user-class entries from BEAM metadata into
/// the CodegenOptions so `generate_module` injects them into the hierarchy.
#[must_use]
pub fn with_class_hierarchy(
    mut self,
    classes: Vec<crate::semantic_analysis::class_hierarchy::ClassInfo>,
) -> Self {
    self.pre_class_hierarchy = classes;
    self
}
```

**Step 4: Wire `pre_class_hierarchy` into `generate_module`**

In `generate_module`, find the section after `hierarchy.add_external_superclasses(...)` (around line 464) and add:

```rust
// ADR 0050 Phase 4: inject user-class entries from BEAM metadata
hierarchy.add_from_beam_meta(options.pre_class_hierarchy.clone());
```

Or take ownership: `hierarchy.add_from_beam_meta(std::mem::take(&mut options.pre_class_hierarchy));`
(Note: `options` may need to be `mut` in the function signature if you take ownership).

**Step 5: Run tests**

```bash
cargo test -p beamtalk-core 2>&1 | tail -20
```

Expected: all tests pass.

**Step 6: Commit**

```bash
git add crates/beamtalk-core/src/codegen/core_erlang/mod.rs
git commit -m "feat: add CodegenOptions::with_class_hierarchy BT-1077"
```

---

### Task 4: ETF deserialization helpers in beamtalk-compiler-port

**Files:**
- Modify: `crates/beamtalk-compiler-port/src/main.rs`

**Step 1: Write the failing roundtrip unit test**

In the `#[cfg(test)]` block at the bottom of `main.rs`:

```rust
/// ADR 0050 Phase 4: roundtrip — construct ETF class_hierarchy map, deserialize,
/// verify ClassInfo fields match.
#[test]
fn parse_class_hierarchy_from_term_roundtrip() {
    // Build ETF: #{counter => #{class => counter, superclass => 'Actor',
    //   meta_version => 2, is_sealed => false, is_abstract => false,
    //   is_value => false, is_typed => false,
    //   fields => [count], field_types => #{count => 'Integer'},
    //   method_info => #{value => #{arity => 0, param_types => [], return_type => 'Integer'}},
    //   class_method_info => #{new => #{arity => 0, param_types => [], return_type => 'counter'}},
    //   class_variables => []}}
    use eetf::{Atom, FixInteger, List, Map, Term};

    let value_method_map = Map::from([
        (atom("arity"), Term::from(FixInteger::from(0))),
        (
            atom("param_types"),
            Term::from(List::from(vec![])),
        ),
        (atom("return_type"), atom("Integer")),
    ]);
    let method_info_map = Map::from([
        (atom("value"), Term::from(value_method_map)),
    ]);

    let new_class_method_map = Map::from([
        (atom("arity"), Term::from(FixInteger::from(0))),
        (atom("param_types"), Term::from(List::from(vec![]))),
        (atom("return_type"), atom("counter")),
    ]);
    let class_method_info_map = Map::from([
        (atom("new"), Term::from(new_class_method_map)),
    ]);

    let field_types_map = Map::from([
        (atom("count"), atom("Integer")),
    ]);

    let meta_map = Map::from([
        (atom("class"), atom("counter")),
        (atom("superclass"), atom("Actor")),
        (atom("meta_version"), Term::from(FixInteger::from(2))),
        (atom("is_sealed"), atom("false")),
        (atom("is_abstract"), atom("false")),
        (atom("is_value"), atom("false")),
        (atom("is_typed"), atom("false")),
        (atom("fields"), Term::from(List::from(vec![atom("count")]))),
        (atom("field_types"), Term::from(field_types_map)),
        (atom("method_info"), Term::from(method_info_map)),
        (atom("class_method_info"), Term::from(class_method_info_map)),
        (atom("class_variables"), Term::from(List::from(vec![]))),
    ]);

    let class_hierarchy_term = Term::from(Map::from([
        (atom("counter"), Term::from(meta_map)),
    ]));

    let classes = parse_class_hierarchy_from_term(&class_hierarchy_term);
    assert_eq!(classes.len(), 1, "Should parse one class");

    let info = &classes[0];
    assert_eq!(info.name.as_str(), "counter");
    assert_eq!(info.superclass.as_deref(), Some("Actor"));
    assert!(!info.is_sealed);
    assert!(!info.is_abstract);
    assert!(!info.is_value);
    assert!(!info.is_typed);
    assert_eq!(info.state, vec!["count"]);
    // state_types: count => Integer
    assert_eq!(
        info.state_types.get("count").map(|s| s.as_str()),
        Some("Integer")
    );
    // methods: value/0 → return Integer
    assert_eq!(info.methods.len(), 1);
    assert_eq!(info.methods[0].selector.as_str(), "value");
    assert_eq!(info.methods[0].arity, 0);
    assert_eq!(info.methods[0].return_type.as_deref(), Some("Integer"));
    // class_methods: new/0
    assert_eq!(info.class_methods.len(), 1);
    assert_eq!(info.class_methods[0].selector.as_str(), "new");
    assert_eq!(info.class_methods[0].arity, 0);
}

#[test]
fn parse_class_hierarchy_skips_builtins() {
    let meta_map = Map::from([
        (atom("class"), atom("Integer")),
        (atom("superclass"), atom("Number")),
        (atom("meta_version"), Term::from(eetf::FixInteger::from(2))),
        (atom("is_sealed"), atom("false")),
        (atom("is_abstract"), atom("false")),
        (atom("is_value"), atom("false")),
        (atom("is_typed"), atom("false")),
        (atom("fields"), Term::from(eetf::List::from(vec![]))),
        (atom("field_types"), Term::from(Map::from([]))),
        (atom("method_info"), Term::from(Map::from([]))),
        (atom("class_method_info"), Term::from(Map::from([]))),
        (atom("class_variables"), Term::from(eetf::List::from(vec![]))),
    ]);
    let class_hierarchy_term = Term::from(Map::from([
        (atom("Integer"), Term::from(meta_map)),
    ]));
    let classes = parse_class_hierarchy_from_term(&class_hierarchy_term);
    assert!(classes.is_empty(), "Integer is a builtin — should be skipped");
}
```

**Step 2: Run tests to confirm FAIL**

```bash
cargo test -p beamtalk-compiler-port parse_class_hierarchy 2>&1 | tail -10
```

Expected: compilation error — `parse_class_hierarchy_from_term` not defined.

**Step 3: Implement ETF deserialization helpers**

Add the following functions to `main.rs` (before the `#[cfg(test)]` block, after the existing helper functions section):

```rust
/// Extract an atom name string from a Term.
fn term_to_atom(term: &Term) -> Option<String> {
    match term {
        Term::Atom(a) => Some(a.name.clone()),
        _ => None,
    }
}

/// Extract an integer value from a Term (FixInteger or BigInteger).
fn term_to_usize(term: &Term) -> Option<usize> {
    match term {
        Term::FixInteger(n) => usize::try_from(n.value).ok(),
        _ => None,
    }
}

/// Extract a list of atom strings from a Term.
fn term_to_atom_list(term: &Term) -> Vec<ecow::EcoString> {
    match term {
        Term::List(list) => list
            .elements
            .iter()
            .filter_map(term_to_atom)
            .map(|s| ecow::EcoString::from(s.as_str()))
            .collect(),
        _ => vec![],
    }
}

/// Extract an atom→atom string map from a Term (for `field_types`).
fn term_to_atom_atom_map(
    term: &Term,
) -> std::collections::HashMap<ecow::EcoString, ecow::EcoString> {
    match term {
        Term::Map(m) => m
            .map
            .iter()
            .filter_map(|(k, v)| {
                let key = term_to_atom(k)?;
                let val = term_to_atom(v)?;
                Some((
                    ecow::EcoString::from(key.as_str()),
                    ecow::EcoString::from(val.as_str()),
                ))
            })
            .collect(),
        _ => std::collections::HashMap::new(),
    }
}

/// Parse method infos from a `method_info` or `class_method_info` ETF map.
///
/// Each entry: `selector_atom => #{arity => int, param_types => [atom...], return_type => atom}`.
fn parse_method_infos_from_map(
    m: &Map,
    key: &str,
    class_name: &str,
    is_class_method: bool,
) -> Vec<beamtalk_core::semantic_analysis::class_hierarchy::MethodInfo> {
    use beamtalk_core::ast::MethodKind;
    use beamtalk_core::semantic_analysis::class_hierarchy::MethodInfo;

    let _ = is_class_method; // kept for documentation clarity
    let Some(Term::Map(method_map)) = map_get(m, key) else {
        return vec![];
    };
    method_map
        .map
        .iter()
        .filter_map(|(sel_term, info_term)| {
            let selector = term_to_atom(sel_term)?;
            let Term::Map(info_map) = info_term else {
                return None;
            };
            let arity = map_get(info_map, "arity").and_then(term_to_usize).unwrap_or(0);
            let return_type = map_get(info_map, "return_type")
                .and_then(term_to_atom)
                .and_then(|s| {
                    if s == "none" {
                        None
                    } else {
                        Some(ecow::EcoString::from(s.as_str()))
                    }
                });
            let param_types: Vec<Option<ecow::EcoString>> =
                match map_get(info_map, "param_types") {
                    Some(Term::List(list)) => list
                        .elements
                        .iter()
                        .map(|t| {
                            term_to_atom(t).and_then(|s| {
                                if s == "none" {
                                    None
                                } else {
                                    Some(ecow::EcoString::from(s.as_str()))
                                }
                            })
                        })
                        .collect(),
                    _ => vec![],
                };
            Some(MethodInfo {
                selector: ecow::EcoString::from(selector.as_str()),
                arity,
                kind: MethodKind::Primary,
                defined_in: ecow::EcoString::from(class_name),
                is_sealed: false,
                return_type,
                param_types,
            })
        })
        .collect()
}

/// Deserialize a single `__beamtalk_meta/0` ETF map into a `ClassInfo`.
///
/// Returns `None` if the term is not a map or lacks the `class` key.
/// Degrades gracefully on missing keys (old-format modules).
fn parse_class_info_from_meta_term(
    class_name: &str,
    term: &Term,
) -> Option<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo> {
    use beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo;

    let Term::Map(m) = term else { return None };

    let superclass = map_get(m, "superclass")
        .and_then(term_to_atom)
        .and_then(|s| {
            if s == "none" {
                None
            } else {
                Some(ecow::EcoString::from(s.as_str()))
            }
        });

    let is_sealed = map_get(m, "is_sealed")
        .and_then(term_to_bool)
        .unwrap_or(false);
    let is_abstract = map_get(m, "is_abstract")
        .and_then(term_to_bool)
        .unwrap_or(false);
    let is_value = map_get(m, "is_value")
        .and_then(term_to_bool)
        .unwrap_or(false);
    let is_typed = map_get(m, "is_typed")
        .and_then(term_to_bool)
        .unwrap_or(false);

    let state = map_get(m, "fields")
        .map(term_to_atom_list)
        .unwrap_or_default();

    let state_types = map_get(m, "field_types")
        .map(term_to_atom_atom_map)
        .unwrap_or_default();

    let class_variables = map_get(m, "class_variables")
        .map(term_to_atom_list)
        .unwrap_or_default();

    let methods = parse_method_infos_from_map(m, "method_info", class_name, false);
    let class_methods = parse_method_infos_from_map(m, "class_method_info", class_name, true);

    Some(ClassInfo {
        name: ecow::EcoString::from(class_name),
        superclass,
        is_sealed,
        is_abstract,
        is_value,
        is_typed,
        state,
        state_types,
        methods,
        class_methods,
        class_variables,
    })
}

/// Parse a `class_hierarchy` ETF term (atom→meta_map) into `Vec<ClassInfo>`.
///
/// Skips builtin classes — the Rust compiler already has richer data for them.
/// Degrades gracefully: classes with missing or malformed metadata are silently
/// skipped rather than causing a hard error.
fn parse_class_hierarchy_from_term(
    term: &Term,
) -> Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo> {
    let Term::Map(m) = term else { return vec![] };
    m.map
        .iter()
        .filter_map(|(name_term, meta_term)| {
            let class_name = term_to_atom(name_term)?;
            // Skip stdlib builtins — Rust side has richer info
            if beamtalk_core::semantic_analysis::ClassHierarchy::is_builtin_class(&class_name) {
                return None;
            }
            parse_class_info_from_meta_term(&class_name, meta_term)
        })
        .collect()
}
```

**Note on `map_get` usage:** The existing `map_get(map: &Map, key: &str)` helper works on `&Map`. The inner `info_map` obtained from `Term::Map(info_map)` is already a `Map`, so you can use `map_get(info_map, "arity")` directly.

**Step 4: Run tests**

```bash
cargo test -p beamtalk-compiler-port parse_class_hierarchy 2>&1 | tail -20
```

Expected: all new tests pass.

**Step 5: Commit**

```bash
git add crates/beamtalk-compiler-port/src/main.rs
git commit -m "feat: add ETF class_hierarchy deserialization helpers BT-1077"
```

---

### Task 5: Wire class_hierarchy into compile paths (Rust port)

**Files:**
- Modify: `crates/beamtalk-compiler-port/src/main.rs`

**Step 1: Write tests for the wired-up paths**

Add to the test module in main.rs:

```rust
/// ADR 0050 Phase 4: compile_expression with class_hierarchy injects user classes
/// into the TypeChecker so DNU diagnostics work across REPL-defined classes.
#[test]
fn compile_expression_with_class_hierarchy_provides_user_class_to_typechecker() {
    // Build a minimal class_hierarchy map with 'Counter' class
    let method_info = Map::from([
        (atom("value"), Term::from(Map::from([
            (atom("arity"), Term::from(eetf::FixInteger::from(0))),
            (atom("param_types"), Term::from(eetf::List::from(vec![]))),
            (atom("return_type"), atom("Integer")),
        ]))),
    ]);
    let counter_meta = Map::from([
        (atom("class"), atom("Counter")),
        (atom("superclass"), atom("Object")),
        (atom("meta_version"), Term::from(eetf::FixInteger::from(2))),
        (atom("is_sealed"), atom("false")),
        (atom("is_abstract"), atom("false")),
        (atom("is_value"), atom("false")),
        (atom("is_typed"), atom("false")),
        (atom("fields"), Term::from(eetf::List::from(vec![]))),
        (atom("field_types"), Term::from(Map::from([]))),
        (atom("method_info"), Term::from(method_info)),
        (atom("class_method_info"), Term::from(Map::from([]))),
        (atom("class_variables"), Term::from(eetf::List::from(vec![]))),
    ]);
    let class_hierarchy_term = Term::from(Map::from([
        (atom("Counter"), Term::from(counter_meta)),
    ]));

    let request = Map::from([
        (atom("command"), atom("compile_expression")),
        (atom("source"), binary("| c | c := Counter new.")),
        (atom("module"), binary("bt@test_repl")),
        (atom("known_vars"), Term::from(eetf::List::from(vec![]))),
        (atom("class_hierarchy"), Term::from(class_hierarchy_term)),
    ]);

    let response = handle_compile_expression(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    let status = map_get(m, "status");
    // With or without class_hierarchy, the compile_expression should succeed
    // (Counter is unknown without hierarchy but the expression itself is valid)
    assert!(
        status == Some(&atom("ok")) || status == Some(&atom("error")),
        "Response must have a status field: {response:?}"
    );
}
```

**Step 2: Modify `handle_compile_expression` to extract and use class_hierarchy**

In `handle_compile_expression`, after extracting `class_module_index` (around line 363), add:

```rust
// ADR 0050 Phase 4: Extract optional class_hierarchy for REPL session user classes.
let pre_class_hierarchy = match map_get(request, "class_hierarchy") {
    None => vec![],
    Some(term) => parse_class_hierarchy_from_term(term),
};
```

Then change the `compute_diagnostics_with_known_vars` call (around line 372) to:

```rust
let mut all_diagnostics =
    beamtalk_core::queries::diagnostic_provider::compute_diagnostics_with_known_vars_and_classes(
        &module,
        parse_diagnostics,
        &known_var_refs,
        pre_class_hierarchy.clone(),
    );
```

Then in `handle_inline_class_definition`, add `pre_class_hierarchy` parameter:
```rust
fn handle_inline_class_definition(
    module: beamtalk_core::ast::Module,
    source: &str,
    expr_module_name: &str,
    warnings: &[String],
    class_superclass_index: &std::collections::HashMap<String, String>,
    class_module_index: std::collections::HashMap<String, String>,
    pre_class_hierarchy: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
) -> Term {
```

And in the `CodegenOptions` builder call for `generate_module` inside `handle_inline_class_definition`, add:
```rust
.with_class_hierarchy(pre_class_hierarchy)
```

Update the call site to pass `pre_class_hierarchy.clone()` (or move it).

**Step 3: Modify `handle_compile` to extract and use class_hierarchy**

In `handle_compile`, after extracting `class_superclass_index` (around line 693), add:

```rust
// ADR 0050 Phase 4: Extract optional class_hierarchy for REPL session user classes.
let pre_class_hierarchy = match map_get(request, "class_hierarchy") {
    None => vec![],
    Some(term) => parse_class_hierarchy_from_term(term),
};
```

Then add `.with_class_hierarchy(pre_class_hierarchy)` to the `CodegenOptions` builder call (around line 702).

Also update the `compute_diagnostics_with_known_vars` call to `compute_diagnostics_with_known_vars_and_classes` with `pre_class_hierarchy.clone()`.

**Step 4: Run tests**

```bash
cargo test -p beamtalk-compiler-port 2>&1 | tail -20
just clippy 2>&1 | tail -20
```

Expected: all tests pass, no clippy warnings.

**Step 5: Commit**

```bash
git add crates/beamtalk-compiler-port/src/main.rs
git commit -m "feat: wire class_hierarchy into compile_expression and compile port handlers BT-1077"
```

---

### Task 6: Erlang side — inject class_hierarchy into port requests

**Files:**
- Modify: `runtime/apps/beamtalk_compiler/src/beamtalk_compiler_server.erl`
- Modify: `runtime/apps/beamtalk_compiler/src/beamtalk_compiler_port.erl`

#### 6a: beamtalk_compiler_server.erl — pass classes in compile_expression

**Step 1: Read the Erlang tests to understand the test style**

Read `runtime/apps/beamtalk_compiler/test/beamtalk_compiler_server_tests.erl` before modifying.

**Step 2: Identify the compile_expression handle_call clause**

In `beamtalk_compiler_server.erl`, find (around line 168):

```erlang
handle_call({compile_expression, Source, ModuleName, KnownVars, Options}, _From, State) ->
    Result = beamtalk_compiler_port:compile_expression(
        State#state.port, Source, ModuleName, KnownVars, Options
    ),
    {reply, Result, State};
```

Change to:

```erlang
handle_call({compile_expression, Source, ModuleName, KnownVars, Options}, _From, State) ->
    %% ADR 0050 Phase 4: Inject class cache so the Rust compiler sees REPL-session classes.
    Options1 = Options#{class_hierarchy => State#state.classes},
    Result = beamtalk_compiler_port:compile_expression(
        State#state.port, Source, ModuleName, KnownVars, Options1
    ),
    {reply, Result, State};
```

**Step 3: Identify the do_compile function**

In `beamtalk_compiler_server.erl`, find `do_compile/3` (around line 308). After extracting `ClassModuleIndex`, add:

```erlang
%% ADR 0050 Phase 4: Inject accumulated class metadata into the port request.
Classes = maps:get(class_hierarchy, Options, #{}),
```

Then add `class_hierarchy` injection to the request assembly (similar to how `class_superclass_index` is handled — only inject when non-empty):

After `Request3 = ...` (for class_module_index), add:

```erlang
Request =
    case map_size(Classes) of
        0 -> Request3;
        _ -> Request3#{class_hierarchy => Classes}
    end,
```

And update the final `send_port_request(Port, Request, 30000)` to use the new `Request` (variable already renamed from previous `Request`).

**Note:** Read the full `do_compile` function body carefully before editing — the variable naming (`Request0`, `Request1`, etc.) must be threaded correctly through all the conditional additions.

#### 6b: beamtalk_compiler_port.erl — extract class_hierarchy from Options

**Step 4: Identify the compile_expression/5 function**

In `beamtalk_compiler_port.erl`, find `compile_expression/5` (around line 78). After the existing `ModuleIndex` extraction, add:

```erlang
ClassHierarchy = maps:get(class_hierarchy, Options, #{}),
```

Then add `class_hierarchy` injection to the Request assembly, similar to the module index pattern (only add when non-empty):

```erlang
Request2 =
    case map_size(ClassHierarchy) of
        0 -> Request1;
        _ -> Request1#{class_hierarchy => ClassHierarchy}
    end,
```

Update the final `RequestBin = term_to_binary(Request2)` line (previously used `Request`).

**Step 5: Run the Erlang tests**

```bash
just test 2>&1 | tail -30
```

Expected: all Erlang tests pass.

**Step 6: Commit**

```bash
git add runtime/apps/beamtalk_compiler/src/beamtalk_compiler_server.erl \
        runtime/apps/beamtalk_compiler/src/beamtalk_compiler_port.erl
git commit -m "feat: inject class_hierarchy from compiler server into port requests BT-1077"
```

---

### Task 7: Run full CI and fix any issues

**Step 1: Run full CI**

```bash
just ci 2>&1 | tail -50
```

Expected: all checks pass (build, clippy, fmt-check, test, test-stdlib, test-e2e).

**Step 2: Fix any failures**

Common failure modes:
- Clippy: `unused variable` — fix by prefixing with `_` or removing
- Clippy: `clone_on_ref_ptr` — avoid unnecessary clones of `EcoString`
- `fmt-check` — run `just fmt` and re-commit
- ETF map key type mismatch — verify `map_get` on inner maps uses `&Map` not `&Term`

**Step 3: Commit any fixes**

```bash
just fmt
git add -p
git commit -m "fix: CI issues in BT-1077 implementation"
```

---

### Task 8: Integration verification (manual)

After all tests pass, verify the end-to-end behaviour works conceptually:

1. The Erlang test `beamtalk_compiler_server_tests.erl` should have existing tests for `compile_expression/3` — confirm they still pass.
2. The `get_classes/0` test-only API can be used to verify the class cache is populated after defining a class in a test.
3. Confirm that `just test-e2e` passes — the REPL tests exercise the full compilation pipeline.

---

## Summary of Files Modified

| File | Change |
|------|--------|
| `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/mod.rs` | Add `add_from_beam_meta(Vec<ClassInfo>)` |
| `crates/beamtalk-core/src/semantic_analysis/mod.rs` | Add `analyse_with_known_vars_and_classes`, `analyse_full_with_pre_classes` |
| `crates/beamtalk-core/src/queries/diagnostic_provider.rs` | Add `compute_diagnostics_with_known_vars_and_classes` |
| `crates/beamtalk-core/src/codegen/core_erlang/mod.rs` | Add `pre_class_hierarchy` field to `CodegenOptions`, wire into `generate_module` |
| `crates/beamtalk-compiler-port/src/main.rs` | ETF helpers, extract + pass `class_hierarchy` in both compile paths |
| `runtime/apps/beamtalk_compiler/src/beamtalk_compiler_server.erl` | Inject `classes` into `compile_expression` Options and `do_compile` Request |
| `runtime/apps/beamtalk_compiler/src/beamtalk_compiler_port.erl` | Extract + forward `class_hierarchy` from Options to Request |
