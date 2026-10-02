// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Compilation order of a dependency graph.
//!
//! **DDD Context:** Build System
//!
//! The single definition of the order in which dependencies are compiled
//! (leaves first). A dependency's `uses:` lines resolve against the protocols
//! of the dependencies *before* it in this order (BT-3678), so every path that
//! exports a dependency's flattened `ClassInfo`s — the graph compile, the
//! fresh-deps fast path and the offline MCP scan — must walk the graph in the
//! same order to produce the same exports (BT-3684). It lives in the library
//! crate so the offline scan, which cannot call into the binary-only
//! `commands::deps`, shares it.

use miette::Result;
use std::collections::{BTreeMap, HashMap};

/// Orders the packages of a dependency graph for compilation with Kahn's
/// algorithm: leaves first, ties broken by name (largest first), so the order
/// depends only on the graph.
///
/// `dependencies` maps each package to the names of its direct dependencies;
/// an edge to a name that is not a key, or to `root_name`, is ignored. The
/// root package itself is not part of the output.
///
/// # Errors
///
/// Returns an error if the graph has a cycle.
pub fn topological_order(
    dependencies: &BTreeMap<String, Vec<String>>,
    root_name: &str,
) -> Result<Vec<String>> {
    // Build in-degree map: count how many deps each node has within the graph
    let mut in_degree: HashMap<String, usize> = HashMap::new();
    let mut reverse_edges: HashMap<String, Vec<String>> = HashMap::new();

    // Initialize all nodes with zero in-degree
    for name in dependencies.keys() {
        in_degree.insert(name.clone(), 0);
    }

    // Count edges: for each node, increment in-degree for each of its deps that is in the graph
    for (name, deps) in dependencies {
        for dep in deps {
            if dependencies.contains_key(dep) && dep != root_name {
                *in_degree.entry(name.clone()).or_default() += 1;
                reverse_edges
                    .entry(dep.clone())
                    .or_default()
                    .push(name.clone());
            }
        }
    }

    // Kahn's algorithm: start with nodes that have no dependencies within the graph
    let mut queue: Vec<String> = in_degree
        .iter()
        .filter(|(_, deg)| **deg == 0)
        .map(|(name, _)| name.clone())
        .collect();
    queue.sort(); // deterministic order

    let mut result = Vec::new();

    while let Some(node_name) = queue.pop() {
        result.push(node_name.clone());

        if let Some(dependents) = reverse_edges.get(&node_name) {
            for dependent in dependents {
                if let Some(deg) = in_degree.get_mut(dependent) {
                    *deg -= 1;
                    if *deg == 0 {
                        queue.push(dependent.clone());
                        queue.sort(); // keep deterministic
                    }
                }
            }
        }
    }

    // Safety net: check for remaining nodes with non-zero in-degree (cycles)
    let remaining: Vec<&String> = in_degree
        .iter()
        .filter(|(_, deg)| **deg > 0)
        .map(|(name, _)| name)
        .collect();

    if !remaining.is_empty() {
        let mut cycle_names: Vec<&str> = remaining.iter().map(|s| s.as_str()).collect();
        cycle_names.sort_unstable();
        miette::bail!(
            "Circular dependency detected among: {}\n  \
             These packages form a dependency cycle and cannot be compiled.",
            cycle_names.join(", ")
        );
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn graph(edges: &[(&str, &[&str])]) -> BTreeMap<String, Vec<String>> {
        edges
            .iter()
            .map(|(name, deps)| {
                (
                    (*name).to_string(),
                    deps.iter().map(|d| (*d).to_string()).collect(),
                )
            })
            .collect()
    }

    #[test]
    fn dependencies_come_before_their_dependents() {
        let order = topological_order(
            &graph(&[("app_lib", &["util"]), ("util", &["core"]), ("core", &[])]),
            "my_app",
        )
        .unwrap();
        assert_eq!(order, ["core", "util", "app_lib"]);
    }

    #[test]
    fn ties_are_broken_by_name_largest_first() {
        // Leaves `a` and `b` order `b, a`; `d` (needing only `b`) becomes ready
        // before `a` is taken, and `c` (needing both) comes last.
        let order = topological_order(
            &graph(&[("a", &[]), ("b", &[]), ("c", &["a", "b"]), ("d", &["b"])]),
            "my_app",
        )
        .unwrap();
        assert_eq!(order, ["b", "d", "a", "c"]);
    }

    #[test]
    fn edges_to_the_root_or_to_unknown_packages_are_ignored() {
        let order = topological_order(&graph(&[("a", &["my_app", "missing"])]), "my_app").unwrap();
        assert_eq!(order, ["a"]);
    }

    #[test]
    fn a_cycle_is_an_error_naming_its_members() {
        let err = topological_order(&graph(&[("a", &["b"]), ("b", &["a"])]), "my_app")
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("Circular dependency detected among: a, b"),
            "{err}"
        );
    }
}
