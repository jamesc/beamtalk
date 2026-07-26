// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Dependency graph resolution with topological ordering (ADR 0070 Phase 1).
//!
//! **DDD Context:** Build System
//!
//! Resolves the full transitive dependency graph from a root package's
//! `beamtalk.toml`, then produces a topologically-sorted compilation order
//! (leaves first). Detects circular dependencies and enforces the single-version
//! policy (each package name may appear at most once in the resolved graph).

use beamtalk_core::compilation::{DependencySource, GitReference};
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, Result};
use std::collections::{BTreeMap, HashMap};
use tracing::{debug, info};

use crate::commands::manifest::{self, ParsedManifest};

use super::lockfile::{LockEntry, Lockfile};
use super::registry::{self, RegistryLocation};

use super::path::{ResolvedDependency, canonicalize_dep_path};

/// A node in the dependency graph, representing a discovered package.
#[derive(Debug)]
#[allow(dead_code)] // name used for Debug output and test assertions
struct DepNode {
    /// The package name.
    name: String,
    /// The package version from beamtalk.toml.
    version: String,
    /// The absolute path to the package root directory.
    root: Utf8PathBuf,
    /// Names of direct dependencies (edges in the graph).
    deps: Vec<String>,
    /// How this package was discovered (for error messages).
    source_description: String,
}

/// Resolve the full transitive dependency graph and return packages in
/// topological compilation order (leaves first).
///
/// This function:
/// 1. Walks the dependency graph starting from the root package
/// 2. Resolves all transitive dependencies (path and already-cloned git deps)
/// 3. Detects circular dependencies and reports the cycle path
/// 4. Enforces single-version policy: same package name at different versions is an error
/// 5. Returns dependencies in topological order for correct compilation
///
/// # Errors
///
/// Returns an error if:
/// - A circular dependency is detected
/// - The same package appears at different versions (single-version violation)
/// - A dependency's directory or manifest is missing/invalid
pub fn resolve_dependency_graph(
    project_root: &Utf8Path,
    options: &beamtalk_core::CompilerOptions,
) -> Result<Vec<ResolvedDependency>> {
    let manifest_path = project_root.join("beamtalk.toml");
    if !manifest_path.exists() {
        return Ok(Vec::new());
    }

    let root_manifest = manifest::parse_manifest_full(&manifest_path)?;
    if root_manifest.dependencies.is_empty() {
        return Ok(Vec::new());
    }

    let root_name = root_manifest.package.name.clone();

    // Read lockfile for git dep SHA pinning
    let lockfile = Lockfile::read(project_root)?.unwrap_or_default();

    // Phase 1: Discover the full dependency graph
    // Newly resolved git/registry deps are collected so we can update the lockfile.
    let mut ctx = DiscoveryContext {
        project_root,
        graph: BTreeMap::new(),
        discovery_stack: vec![root_name.clone()],
        lockfile: &lockfile,
        new_lock_entries: Vec::new(),
        // The registry is a property of the project being built, so the root
        // manifest's `[registry]` governs the whole graph — a transitive
        // dependency cannot redirect resolution to a registry of its own.
        registry_location: registry::resolve_registry_location(
            project_root,
            root_manifest.registry.as_ref(),
        ),
    };

    discover_deps(&mut ctx, project_root, &root_name, &root_manifest)?;

    // Extract results from context (drops the lockfile borrow)
    let graph = ctx.graph;
    let new_lock_entries = ctx.new_lock_entries;

    // Update lockfile with any newly resolved git/registry deps
    if !new_lock_entries.is_empty() {
        let mut lockfile = lockfile;
        for entry in new_lock_entries {
            lockfile.insert(entry);
        }
        lockfile.write(project_root)?;
    }

    if graph.is_empty() {
        return Ok(Vec::new());
    }

    // Phase 2: Topological sort (Kahn's algorithm)
    let compilation_order = topological_sort(&graph, &root_name)?;

    info!(
        order = ?compilation_order,
        "Resolved dependency compilation order"
    );

    // Collect direct dependency names from the root manifest
    let direct_dep_names: std::collections::HashSet<&str> = root_manifest
        .dependencies
        .keys()
        .map(String::as_str)
        .collect();

    // Phase 3: Compile each dependency in topological order
    let mut resolved = Vec::new();
    for dep_name in &compilation_order {
        let node = &graph[dep_name];
        let mut compiled = super::path::compile_dependency_at(
            project_root,
            &node.root,
            dep_name,
            options,
            &resolved,
        )?;

        // BT-1654: Set transitivity metadata
        compiled.is_direct = direct_dep_names.contains(dep_name.as_str());
        if !compiled.is_direct {
            compiled.via_chain = compute_via_chain(dep_name, &direct_dep_names, &graph);
        }

        resolved.push(compiled);
    }

    Ok(resolved)
}

/// Compute the "via" chain from the root package to a transitive dependency.
///
/// Uses BFS over the dependency graph starting from the root's direct
/// dependencies. Returns the intermediate package names that form the
/// shortest path from a direct dependency to the target.
///
/// For example, if `my_app -> json -> utils`, returns `["json"]` for target `utils`.
fn compute_via_chain(
    target: &str,
    direct_dep_names: &std::collections::HashSet<&str>,
    graph: &BTreeMap<String, DepNode>,
) -> Vec<String> {
    use std::collections::{HashSet, VecDeque};

    // BFS starting from the root's direct dependencies
    let mut queue: VecDeque<(&str, Vec<String>)> = VecDeque::new();
    let mut visited: HashSet<&str> = HashSet::new();

    for direct_name in direct_dep_names {
        if let Some(node) = graph.get(*direct_name) {
            // Check if this direct dep itself depends on the target
            if node.deps.iter().any(|d| d == target) {
                return vec![direct_name.to_string()];
            }
            queue.push_back((*direct_name, vec![direct_name.to_string()]));
            visited.insert(*direct_name);
        }
    }

    while let Some((current, path)) = queue.pop_front() {
        if let Some(node) = graph.get(current) {
            for dep in &node.deps {
                if dep == target {
                    return path;
                }
                if visited.insert(dep) {
                    let mut new_path = path.clone();
                    new_path.push(dep.clone());
                    queue.push_back((dep, new_path));
                }
            }
        }
    }

    Vec::new()
}

/// Mutable state accumulated during dependency graph discovery.
struct DiscoveryContext<'a> {
    project_root: &'a Utf8Path,
    graph: BTreeMap<String, DepNode>,
    discovery_stack: Vec<String>,
    lockfile: &'a Lockfile,
    /// Lock entries produced during this walk, written back once discovery
    /// completes.
    new_lock_entries: Vec<LockEntry>,
    /// Where registry dependencies resolve from, taken from the root manifest.
    registry_location: RegistryLocation,
}

/// Recursively discover dependencies by walking manifests.
///
/// Builds the `graph` map of all packages found in the transitive closure.
/// Detects cycles via `discovery_stack` and version conflicts via stored versions.
/// Git dependencies that haven't been cloned yet are automatically fetched.
fn discover_deps(
    ctx: &mut DiscoveryContext<'_>,
    parent_root: &Utf8Path,
    parent_name: &str,
    parent_manifest: &ParsedManifest,
) -> Result<()> {
    for (dep_name, spec) in &parent_manifest.dependencies {
        match &spec.source {
            DependencySource::Path { path } => {
                let relative_utf8 = Utf8Path::from_path(path).ok_or_else(|| {
                    miette::miette!(
                        "Dependency '{dep_name}' has a non-UTF-8 path: {}",
                        path.display()
                    )
                })?;
                let dep_root = canonicalize_dep_path(parent_root, relative_utf8);

                discover_single_dep(
                    ctx,
                    dep_name,
                    &dep_root,
                    &format!("path: {relative_utf8}"),
                    parent_name,
                )?;
            }
            DependencySource::Git { url, reference } => {
                // Always resolve via resolve_git_dep so existing checkouts are
                // verified against the lockfile SHA / requested ref. The function
                // already skips re-cloning when verify_checkout matches.
                let lock_entry = ctx.lockfile.get(dep_name);
                let resolved = super::git::resolve_git_dep(
                    dep_name,
                    url,
                    reference,
                    ctx.project_root,
                    lock_entry,
                )
                .wrap_err_with(|| {
                    format!(
                        "Failed to resolve git dependency '{dep_name}' \
                         (required by '{parent_name}')"
                    )
                })?;
                let checkout_root = resolved.checkout_path.clone();
                ctx.new_lock_entries.push(LockEntry {
                    name: resolved.name.clone(),
                    url: resolved.url.clone(),
                    reference: resolved.reference.clone(),
                    resolved_sha: resolved.resolved_sha.clone(),
                    registry_version: None,
                });

                discover_single_dep(
                    ctx,
                    dep_name,
                    &checkout_root,
                    &format!("git: {url}"),
                    parent_name,
                )?;
            }
            DependencySource::Registry { version } => {
                let checkout_root = resolve_registry_dep(ctx, dep_name, version, parent_name)?;

                discover_single_dep(
                    ctx,
                    dep_name,
                    &checkout_root,
                    &format!("registry: {version}"),
                    parent_name,
                )?;
            }
        }
    }
    Ok(())
}

/// Resolve a registry dependency to an on-disk checkout.
///
/// Takes the `(git url, tag)` straight from the lockfile when it already
/// records this exact version — so a locked build never touches the registry
/// index. Otherwise the index is consulted, then the release is fetched
/// through the ordinary git machinery and the checked-out package's own
/// declared version is validated against the request.
fn resolve_registry_dep(
    ctx: &mut DiscoveryContext<'_>,
    dep_name: &str,
    version: &str,
    parent_name: &str,
) -> Result<Utf8PathBuf> {
    let locked = ctx.lockfile.get(dep_name);

    // Fast path: the lockfile already pins this exact version, so we can go
    // straight to the git checkout without reading (or fetching) the index.
    //
    // The lock entry is forwarded to `resolve_git_dep` *only* on that hit.
    // On a miss the entry is deliberately dropped: it pins the previously
    // resolved SHA, and `resolve_git_dep` reuses an existing checkout whose
    // HEAD matches that SHA without looking at the reference we pass. Keeping
    // it across a version bump would leave the old version checked out while
    // we believe we fetched the new tag.
    let (url, reference, lock_entry) = match locked {
        Some(entry) if entry.registry_version.as_deref() == Some(version) => {
            debug!(dep = %dep_name, version, "Registry dependency satisfied from lockfile");
            (entry.url.clone(), entry.reference.clone(), locked)
        }
        _ => {
            let release = registry::resolve_release(
                ctx.project_root,
                &ctx.registry_location,
                dep_name,
                version,
            )
            .wrap_err_with(|| {
                format!(
                    "Failed to resolve registry dependency '{dep_name}' \
                             (required by '{parent_name}')"
                )
            })?;
            (release.git, GitReference::Tag(release.tag), None)
        }
    };

    let resolved =
        super::git::resolve_git_dep(dep_name, &url, &reference, ctx.project_root, lock_entry)
            .wrap_err_with(|| {
                format!(
                    "Failed to fetch registry dependency '{dep_name}' {version} \
             (required by '{parent_name}')"
                )
            })?;

    validate_checkout_version(dep_name, version, &resolved.checkout_path, &url, &reference)?;

    let checkout_root = resolved.checkout_path.clone();
    ctx.new_lock_entries.push(LockEntry {
        name: resolved.name,
        url: resolved.url,
        reference: resolved.reference,
        resolved_sha: resolved.resolved_sha,
        registry_version: Some(version.to_string()),
    });

    Ok(checkout_root)
}

/// Check that a fetched registry package declares the version we asked for.
///
/// Guards against a registry index that points a version at the wrong tag, and
/// against a package whose tag and `beamtalk.toml` have drifted apart — either
/// would otherwise silently build the wrong code.
fn validate_checkout_version(
    dep_name: &str,
    requested: &str,
    checkout_path: &Utf8Path,
    url: &str,
    reference: &GitReference,
) -> Result<()> {
    let manifest_path = checkout_path.join("beamtalk.toml");
    let manifest = manifest::parse_manifest(&manifest_path).wrap_err_with(|| {
        format!("Failed to read the manifest of registry dependency '{dep_name}'")
    })?;

    if manifest.version != requested {
        let tag = match reference {
            GitReference::Tag(tag) => tag.as_str(),
            GitReference::Branch(branch) => branch.as_str(),
            GitReference::Rev(rev) => rev.as_str(),
        };
        miette::bail!(
            "Registry dependency '{dep_name}' version mismatch.\n  \
             Requested: '{requested}' (from [dependencies] in beamtalk.toml)\n  \
             Found:     '{}' (in {manifest_path})\n\n  \
             The registry points version '{requested}' at '{tag}' in {url}, but that \
             tag's package declares a different version. This is a packaging error — \
             report it to the package maintainer.",
            manifest.version
        );
    }

    Ok(())
}

/// Discover a single dependency from its root directory.
///
/// Validates the manifest, checks for cycles and version conflicts,
/// then recurses into the dependency's own dependencies.
fn discover_single_dep(
    ctx: &mut DiscoveryContext<'_>,
    dep_name: &str,
    dep_root: &Utf8Path,
    source_description: &str,
    parent_name: &str,
) -> Result<()> {
    // Validate directory exists
    if !dep_root.exists() {
        miette::bail!(
            "Dependency '{dep_name}' (required by '{parent_name}') directory does not exist: {dep_root}"
        );
    }

    // Validate manifest exists
    let dep_manifest_path = dep_root.join("beamtalk.toml");
    if !dep_manifest_path.exists() {
        miette::bail!(
            "Dependency '{dep_name}' (required by '{parent_name}') is missing beamtalk.toml: {dep_manifest_path}"
        );
    }

    let dep_manifest = manifest::parse_manifest_full(&dep_manifest_path)
        .wrap_err_with(|| format!("Failed to parse manifest for dependency '{dep_name}'"))?;

    // Validate package name matches dependency key
    if dep_manifest.package.name != dep_name {
        miette::bail!(
            "Dependency '{dep_name}' (required by '{parent_name}') has mismatched package name.\n  \
             Expected: '{dep_name}' (from [dependencies] key)\n  \
             Found: '{}' (in {dep_manifest_path})",
            dep_manifest.package.name
        );
    }

    // Check for circular dependencies
    if ctx.discovery_stack.contains(&dep_name.to_string()) {
        let cycle_start = ctx
            .discovery_stack
            .iter()
            .position(|n| n == dep_name)
            .unwrap();
        let cycle_path: Vec<&str> = ctx.discovery_stack[cycle_start..]
            .iter()
            .map(String::as_str)
            .chain(std::iter::once(dep_name))
            .collect();
        miette::bail!(
            "Circular dependency detected: {}\n  \
             A package cannot depend on itself, directly or transitively.",
            cycle_path.join(" -> ")
        );
    }

    // Single-version policy: if we've already seen this package, check version matches
    if let Some(existing) = ctx.graph.get(dep_name) {
        if existing.version != dep_manifest.package.version {
            miette::bail!(
                "Version conflict for dependency '{dep_name}':\n  \
                 Version '{}' required (from {source_description})\n  \
                 Version '{}' already resolved (from {})\n\n  \
                 Beamtalk enforces a single-version policy: each package name may appear \
                 at most once in the resolved dependency graph. Align the versions to resolve \
                 this conflict.",
                dep_manifest.package.version,
                existing.version,
                existing.source_description,
            );
        }
        // Same version, same package — already discovered, skip
        debug!(dep = %dep_name, "Dependency already discovered with matching version, skipping");
        return Ok(());
    }

    let dep_deps: Vec<String> = dep_manifest.dependencies.keys().cloned().collect();
    let has_transitive_deps = !dep_manifest.dependencies.is_empty();

    ctx.graph.insert(
        dep_name.to_string(),
        DepNode {
            name: dep_name.to_string(),
            version: dep_manifest.package.version.clone(),
            root: dep_root.to_path_buf(),
            deps: dep_deps,
            source_description: source_description.to_string(),
        },
    );

    // Recurse into this dependency's own dependencies
    if has_transitive_deps {
        ctx.discovery_stack.push(dep_name.to_string());
        discover_deps(ctx, dep_root, dep_name, &dep_manifest)?;
        ctx.discovery_stack.pop();
    }

    Ok(())
}

/// Perform a topological sort of the dependency graph using Kahn's algorithm.
///
/// Returns package names in compilation order (leaves first, root's direct deps last).
/// The root package itself is NOT included in the output.
///
/// # Errors
///
/// Returns an error if a cycle is detected (should not happen if `discover_deps`
/// already checked, but this is a safety net).
fn topological_sort(graph: &BTreeMap<String, DepNode>, root_name: &str) -> Result<Vec<String>> {
    // Build in-degree map: count how many deps each node has within the graph
    let mut in_degree: HashMap<String, usize> = HashMap::new();
    let mut reverse_edges: HashMap<String, Vec<String>> = HashMap::new();

    // Initialize all nodes with zero in-degree
    for name in graph.keys() {
        in_degree.insert(name.clone(), 0);
    }

    // Count edges: for each node, increment in-degree for each of its deps that is in the graph
    for (name, node) in graph {
        for dep in &node.deps {
            if graph.contains_key(dep) && dep != root_name {
                *in_degree.get_mut(name).unwrap() += 1;
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
    use super::super::test_support::*;
    use super::*;
    use std::fs;
    use std::process::Command;
    use tempfile::TempDir;

    #[test]
    fn test_topological_sort_linear_chain() {
        // C <- B <- A (A depends on B, B depends on C)
        let mut graph = BTreeMap::new();
        graph.insert(
            "pkg_c".to_string(),
            DepNode {
                name: "pkg_c".to_string(),
                version: "0.1.0".to_string(),
                root: Utf8PathBuf::from("/tmp/c"),
                deps: vec![],
                source_description: "path: ../pkg_c".to_string(),
            },
        );
        graph.insert(
            "pkg_b".to_string(),
            DepNode {
                name: "pkg_b".to_string(),
                version: "0.1.0".to_string(),
                root: Utf8PathBuf::from("/tmp/b"),
                deps: vec!["pkg_c".to_string()],
                source_description: "path: ../pkg_b".to_string(),
            },
        );

        let order = topological_sort(&graph, "my_app").unwrap();
        assert_eq!(order, vec!["pkg_c", "pkg_b"]);
    }

    #[test]
    fn test_topological_sort_diamond() {
        // Diamond: A -> B, A -> C, B -> D, C -> D
        let mut graph = BTreeMap::new();
        graph.insert(
            "d".to_string(),
            DepNode {
                name: "d".to_string(),
                version: "0.1.0".to_string(),
                root: Utf8PathBuf::from("/tmp/d"),
                deps: vec![],
                source_description: "path: ../d".to_string(),
            },
        );
        graph.insert(
            "b".to_string(),
            DepNode {
                name: "b".to_string(),
                version: "0.1.0".to_string(),
                root: Utf8PathBuf::from("/tmp/b"),
                deps: vec!["d".to_string()],
                source_description: "path: ../b".to_string(),
            },
        );
        graph.insert(
            "c".to_string(),
            DepNode {
                name: "c".to_string(),
                version: "0.1.0".to_string(),
                root: Utf8PathBuf::from("/tmp/c"),
                deps: vec!["d".to_string()],
                source_description: "path: ../c".to_string(),
            },
        );

        let order = topological_sort(&graph, "a").unwrap();
        // D must come before B and C
        let d_pos = order.iter().position(|x| x == "d").unwrap();
        let b_pos = order.iter().position(|x| x == "b").unwrap();
        let c_pos = order.iter().position(|x| x == "c").unwrap();
        assert!(d_pos < b_pos, "d must be compiled before b");
        assert!(d_pos < c_pos, "d must be compiled before c");
        assert_eq!(order.len(), 3);
    }

    #[test]
    fn test_circular_dependency_detection() {
        let temp = TempDir::new().unwrap();

        // Create A and B that depend on each other
        let a_dir = temp.path().join("pkg_a");
        let b_dir = temp.path().join("pkg_b");
        fs::create_dir_all(&a_dir).unwrap();
        fs::create_dir_all(&b_dir).unwrap();

        // B depends on A
        let a_path_str = a_dir.to_str().unwrap().replace('\\', "/");
        write_manifest(
            &b_dir,
            "pkg_b",
            "0.1.0",
            &format!(
                r#"[dependencies]
pkg_a = {{ path = "{a_path_str}" }}"#
            ),
        );

        // A depends on B
        let b_path_str = b_dir.to_str().unwrap().replace('\\', "/");
        write_manifest(
            &a_dir,
            "pkg_a",
            "0.1.0",
            &format!(
                r#"[dependencies]
pkg_b = {{ path = "{b_path_str}" }}"#
            ),
        );

        // Root depends on A
        let root = temp.path().join("root");
        fs::create_dir_all(&root).unwrap();
        write_manifest(
            &root,
            "my_app",
            "0.1.0",
            &format!(
                r#"[dependencies]
pkg_a = {{ path = "{a_path_str}" }}"#
            ),
        );

        let root_path = Utf8PathBuf::from_path_buf(root).unwrap();
        let options = beamtalk_core::CompilerOptions::default();

        let result = resolve_dependency_graph(&root_path, &options);
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("Circular") || err.contains("circular"),
            "Should detect circular dependency: {err}"
        );
        // Should show the cycle path
        assert!(err.contains("->"), "Should show the cycle path: {err}");
    }

    #[test]
    fn test_version_conflict_detection() {
        let temp = TempDir::new().unwrap();

        // Create shared dep at version 0.1.0
        let shared_v1_dir = temp.path().join("shared_v1");
        fs::create_dir_all(&shared_v1_dir).unwrap();
        write_manifest(&shared_v1_dir, "shared", "0.1.0", "");

        // Create shared dep at version 0.2.0
        let shared_v2_dir = temp.path().join("shared_v2");
        fs::create_dir_all(&shared_v2_dir).unwrap();
        write_manifest(&shared_v2_dir, "shared", "0.2.0", "");

        // Create pkg_a depending on shared v1
        let a_dir = temp.path().join("pkg_a");
        fs::create_dir_all(&a_dir).unwrap();
        let shared_v1_str = shared_v1_dir.to_str().unwrap().replace('\\', "/");
        write_manifest(
            &a_dir,
            "pkg_a",
            "0.1.0",
            &format!(
                r#"[dependencies]
shared = {{ path = "{shared_v1_str}" }}"#
            ),
        );

        // Create pkg_b depending on shared v2
        let b_dir = temp.path().join("pkg_b");
        fs::create_dir_all(&b_dir).unwrap();
        let shared_v2_str = shared_v2_dir.to_str().unwrap().replace('\\', "/");
        write_manifest(
            &b_dir,
            "pkg_b",
            "0.1.0",
            &format!(
                r#"[dependencies]
shared = {{ path = "{shared_v2_str}" }}"#
            ),
        );

        // Root depends on both A and B (which need different versions of shared)
        let root = temp.path().join("root");
        fs::create_dir_all(&root).unwrap();
        let a_str = a_dir.to_str().unwrap().replace('\\', "/");
        let b_str = b_dir.to_str().unwrap().replace('\\', "/");
        write_manifest(
            &root,
            "my_app",
            "0.1.0",
            &format!(
                r#"[dependencies]
pkg_a = {{ path = "{a_str}" }}
pkg_b = {{ path = "{b_str}" }}"#
            ),
        );

        let root_path = Utf8PathBuf::from_path_buf(root).unwrap();
        let options = beamtalk_core::CompilerOptions::default();

        let result = resolve_dependency_graph(&root_path, &options);
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("Version conflict") || err.contains("version"),
            "Should detect version conflict: {err}"
        );
        assert!(
            err.contains("0.1.0") && err.contains("0.2.0"),
            "Should mention both versions: {err}"
        );
    }

    #[test]
    fn test_diamond_deps_same_version() {
        // Diamond: root -> A, root -> B, A -> shared, B -> shared (same version)
        let temp = TempDir::new().unwrap();

        // Create shared dep
        let shared_dir = temp.path().join("shared");
        fs::create_dir_all(&shared_dir).unwrap();
        write_manifest(&shared_dir, "shared", "0.1.0", "");

        // Create pkg_a depending on shared
        let a_dir = temp.path().join("pkg_a");
        fs::create_dir_all(&a_dir).unwrap();
        let shared_str = shared_dir.to_str().unwrap().replace('\\', "/");
        write_manifest(
            &a_dir,
            "pkg_a",
            "0.1.0",
            &format!(
                r#"[dependencies]
shared = {{ path = "{shared_str}" }}"#
            ),
        );

        // Create pkg_b depending on shared
        let b_dir = temp.path().join("pkg_b");
        fs::create_dir_all(&b_dir).unwrap();
        write_manifest(
            &b_dir,
            "pkg_b",
            "0.1.0",
            &format!(
                r#"[dependencies]
shared = {{ path = "{shared_str}" }}"#
            ),
        );

        // Root depends on both A and B
        let root = temp.path().join("root");
        fs::create_dir_all(&root).unwrap();
        let a_str = a_dir.to_str().unwrap().replace('\\', "/");
        let b_str = b_dir.to_str().unwrap().replace('\\', "/");
        write_manifest(
            &root,
            "my_app",
            "0.1.0",
            &format!(
                r#"[dependencies]
pkg_a = {{ path = "{a_str}" }}
pkg_b = {{ path = "{b_str}" }}"#
            ),
        );

        let root_path = Utf8PathBuf::from_path_buf(root).unwrap();
        let options = beamtalk_core::CompilerOptions::default();

        let result = resolve_dependency_graph(&root_path, &options);
        assert!(
            result.is_ok(),
            "Diamond deps with same version should work: {:?}",
            result.err()
        );

        let resolved = result.unwrap();
        // shared should be compiled before both A and B
        let names: Vec<&str> = resolved.iter().map(|r| r.name.as_str()).collect();
        assert!(
            names.contains(&"shared"),
            "shared should be resolved: {names:?}"
        );
        assert!(
            names.contains(&"pkg_a"),
            "pkg_a should be resolved: {names:?}"
        );
        assert!(
            names.contains(&"pkg_b"),
            "pkg_b should be resolved: {names:?}"
        );

        let shared_pos = names.iter().position(|n| *n == "shared").unwrap();
        let a_pos = names.iter().position(|n| *n == "pkg_a").unwrap();
        let b_pos = names.iter().position(|n| *n == "pkg_b").unwrap();
        assert!(
            shared_pos < a_pos,
            "shared must be compiled before pkg_a: {names:?}"
        );
        assert!(
            shared_pos < b_pos,
            "shared must be compiled before pkg_b: {names:?}"
        );
    }

    #[test]
    fn test_no_dependencies() {
        let temp = TempDir::new().unwrap();
        let root = temp.path();
        write_manifest(root, "my_app", "0.1.0", "");

        let root_path = Utf8PathBuf::from_path_buf(root.to_path_buf()).unwrap();
        let options = beamtalk_core::CompilerOptions::default();

        let result = resolve_dependency_graph(&root_path, &options).unwrap();
        assert!(result.is_empty());
    }

    #[test]
    fn test_no_manifest() {
        let temp = TempDir::new().unwrap();
        let root_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let options = beamtalk_core::CompilerOptions::default();

        let result = resolve_dependency_graph(&root_path, &options).unwrap();
        assert!(result.is_empty());
    }

    #[test]
    fn test_self_dependency_detected() {
        let temp = TempDir::new().unwrap();
        let root = temp.path().join("my_app");
        fs::create_dir_all(&root).unwrap();

        let root_str = root.to_str().unwrap().replace('\\', "/");
        write_manifest(
            &root,
            "my_app",
            "0.1.0",
            &format!(
                r#"[dependencies]
my_app = {{ path = "{root_str}" }}"#
            ),
        );

        let root_path = Utf8PathBuf::from_path_buf(root).unwrap();
        let options = beamtalk_core::CompilerOptions::default();

        let result = resolve_dependency_graph(&root_path, &options);
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("Circular") || err.contains("circular"),
            "Should detect self-dependency: {err}"
        );
    }

    #[test]
    fn test_transitive_chain_ordering() {
        // A -> B -> C -> D (four-level chain)
        let temp = TempDir::new().unwrap();

        let d_dir = temp.path().join("pkg_d");
        fs::create_dir_all(&d_dir).unwrap();
        write_manifest(&d_dir, "pkg_d", "0.1.0", "");

        let c_dir = temp.path().join("pkg_c");
        fs::create_dir_all(&c_dir).unwrap();
        let d_str = d_dir.to_str().unwrap().replace('\\', "/");
        write_manifest(
            &c_dir,
            "pkg_c",
            "0.1.0",
            &format!(
                r#"[dependencies]
pkg_d = {{ path = "{d_str}" }}"#
            ),
        );

        let b_dir = temp.path().join("pkg_b");
        fs::create_dir_all(&b_dir).unwrap();
        let c_str = c_dir.to_str().unwrap().replace('\\', "/");
        write_manifest(
            &b_dir,
            "pkg_b",
            "0.1.0",
            &format!(
                r#"[dependencies]
pkg_c = {{ path = "{c_str}" }}"#
            ),
        );

        let root = temp.path().join("root");
        fs::create_dir_all(&root).unwrap();
        let b_str = b_dir.to_str().unwrap().replace('\\', "/");
        write_manifest(
            &root,
            "my_app",
            "0.1.0",
            &format!(
                r#"[dependencies]
pkg_b = {{ path = "{b_str}" }}"#
            ),
        );

        let root_path = Utf8PathBuf::from_path_buf(root).unwrap();
        let options = beamtalk_core::CompilerOptions::default();

        let result = resolve_dependency_graph(&root_path, &options);
        assert!(
            result.is_ok(),
            "Transitive chain should resolve: {:?}",
            result.err()
        );

        let resolved = result.unwrap();
        let names: Vec<&str> = resolved.iter().map(|r| r.name.as_str()).collect();
        assert_eq!(names, vec!["pkg_d", "pkg_c", "pkg_b"]);
    }

    /// Create a local git repo with a beamtalk.toml, a tag, and a branch.
    /// Returns (`TempDir`, url, `commit_sha`).
    fn create_git_dep_repo(pkg_name: &str, version: &str, deps: &str) -> (TempDir, String, String) {
        let dir = TempDir::new().unwrap();
        let path = dir.path();

        run_git_cmd(path, &["init"]);
        run_git_cmd(path, &["config", "user.email", "test@test.com"]);
        run_git_cmd(path, &["config", "user.name", "Test"]);
        run_git_cmd(path, &["config", "commit.gpgsign", "false"]);

        write_manifest(path, pkg_name, version, deps);

        run_git_cmd(path, &["add", "."]);
        run_git_cmd(path, &["commit", "-m", "initial"]);
        run_git_cmd(path, &["tag", "-m", "v1.0.0", "v1.0.0"]);

        let sha = get_git_sha(path);
        let mut path_str = path.display().to_string().replace('\\', "/");
        if !path_str.starts_with('/') {
            path_str.insert(0, '/');
        }
        let url = format!("file://{path_str}");
        (dir, url, sha)
    }

    fn run_git_cmd(dir: &std::path::Path, args: &[&str]) {
        let output = Command::new("git")
            .args(args)
            .current_dir(dir)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "git {:?} failed: {}",
            args,
            String::from_utf8_lossy(&output.stderr)
        );
    }

    fn get_git_sha(dir: &std::path::Path) -> String {
        let output = Command::new("git")
            .args(["rev-parse", "HEAD"])
            .current_dir(dir)
            .output()
            .unwrap();
        String::from_utf8_lossy(&output.stdout).trim().to_string()
    }

    #[test]
    fn test_git_dep_auto_cloned_during_resolve() {
        // Create a git repo that acts as a dependency
        let (_git_dir, url, _sha) = create_git_dep_repo("my_git_dep", "0.1.0", "");

        // Create a root project that depends on the git dep
        let root_dir = TempDir::new().unwrap();
        let root = root_dir.path().join("root");
        fs::create_dir_all(&root).unwrap();
        write_manifest(
            &root,
            "my_app",
            "0.1.0",
            &format!(
                r#"[dependencies]
my_git_dep = {{ git = "{url}", tag = "v1.0.0" }}"#
            ),
        );

        let root_path = Utf8PathBuf::from_path_buf(root.clone()).unwrap();
        let options = beamtalk_core::CompilerOptions::default();

        // _build/deps should NOT exist yet
        let test_layout = crate::commands::build_layout::BuildLayout::new(&root_path);
        let deps_dir = test_layout.deps_dir();
        assert!(
            !deps_dir.exists(),
            "deps dir should not exist before resolve"
        );

        // resolve_dependency_graph should auto-clone the git dep
        let result = resolve_dependency_graph(&root_path, &options);
        assert!(
            result.is_ok(),
            "Git dep should be auto-cloned: {:?}",
            result.err()
        );

        // Verify the git dep was cloned
        let checkout = deps_dir.join("my_git_dep").join("beamtalk.toml");
        assert!(
            checkout.exists(),
            "Git dep should have been cloned to _build/deps/my_git_dep/"
        );

        // Verify a lockfile was written
        let lock_path = root.join("beamtalk.lock");
        assert!(lock_path.exists(), "Lockfile should have been written");
        let lock_content = fs::read_to_string(&lock_path).unwrap();
        assert!(
            lock_content.contains("my_git_dep"),
            "Lockfile should contain the git dep"
        );
    }

    #[test]
    fn test_transitive_git_dep_auto_cloned() {
        // Create the leaf git dep
        let (_leaf_dir, leaf_url, _leaf_sha) = create_git_dep_repo("leaf_dep", "0.1.0", "");

        // Create the middle path dep that depends on the leaf git dep
        let workspace = TempDir::new().unwrap();
        let middle_dir = workspace.path().join("middle");
        fs::create_dir_all(&middle_dir).unwrap();
        write_manifest(
            &middle_dir,
            "middle",
            "0.1.0",
            &format!(
                r#"[dependencies]
leaf_dep = {{ git = "{leaf_url}", tag = "v1.0.0" }}"#
            ),
        );

        // Create the root project that depends on the middle path dep
        let root = workspace.path().join("root");
        fs::create_dir_all(&root).unwrap();
        let middle_str = middle_dir.to_str().unwrap().replace('\\', "/");
        write_manifest(
            &root,
            "my_app",
            "0.1.0",
            &format!(
                r#"[dependencies]
middle = {{ path = "{middle_str}" }}"#
            ),
        );

        let root_path = Utf8PathBuf::from_path_buf(root.clone()).unwrap();
        let options = beamtalk_core::CompilerOptions::default();

        let result = resolve_dependency_graph(&root_path, &options);
        assert!(
            result.is_ok(),
            "Transitive git dep should be auto-cloned: {:?}",
            result.err()
        );

        let resolved = result.unwrap();
        let names: Vec<&str> = resolved.iter().map(|r| r.name.as_str()).collect();
        assert!(
            names.contains(&"leaf_dep"),
            "Transitive git dep should be resolved: {names:?}"
        );
        assert!(
            names.contains(&"middle"),
            "Direct path dep should be resolved: {names:?}"
        );
    }

    // ── Registry dependencies (BT-2978) ──────────────────────────────
    //
    // These drive the whole path: manifest → registry index → git checkout →
    // lockfile. The index is a plain local directory and the "remote" is a
    // `file://` repo, so nothing here touches the network.
    //
    // Each test points `[registry] url` at its own index. `BEAMTALK_REGISTRY`
    // deliberately outranks the manifest, so these assume it is unset — which
    // holds in CI and in a normal dev shell.

    /// Create a local registry index directory holding one package entry.
    fn create_registry_index(pkg_name: &str, versions_toml: &str) -> (TempDir, Utf8PathBuf) {
        let dir = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();
        fs::create_dir_all(root.join("packages")).unwrap();
        fs::write(
            root.join("packages").join(format!("{pkg_name}.toml")),
            format!("name = \"{pkg_name}\"\n{versions_toml}"),
        )
        .unwrap();
        (dir, root)
    }

    /// Create a root project whose only dependency is a registry package.
    fn write_registry_project(
        root: &std::path::Path,
        index_root: &Utf8Path,
        dep_name: &str,
        version: &str,
    ) {
        fs::create_dir_all(root).unwrap();
        write_manifest(
            root,
            "my_app",
            "0.1.0",
            &format!(
                "[registry]\nurl = \"{index_root}\"\n\n[dependencies]\n{dep_name} = \"{version}\"\n"
            ),
        );
    }

    #[test]
    fn test_registry_dep_resolves_and_writes_lockfile() {
        let (_git_dir, url, sha) = create_git_dep_repo("my_pkg", "1.0.0", "");
        let (_index_dir, index_root) = create_registry_index(
            "my_pkg",
            &format!("[[versions]]\nversion = \"1.0.0\"\ngit = \"{url}\"\n"),
        );

        let root_dir = TempDir::new().unwrap();
        let root = root_dir.path().join("root");
        write_registry_project(&root, &index_root, "my_pkg", "1.0.0");

        let root_path = Utf8PathBuf::from_path_buf(root.clone()).unwrap();
        let options = beamtalk_core::CompilerOptions::default();

        let result = resolve_dependency_graph(&root_path, &options);
        assert!(
            result.is_ok(),
            "Registry dep should resolve: {:?}",
            result.err()
        );

        // The package was fetched into the usual dep checkout directory.
        let layout = crate::commands::build_layout::BuildLayout::new(&root_path);
        assert!(
            layout
                .dep_checkout_dir("my_pkg")
                .join("beamtalk.toml")
                .exists(),
            "Registry dep should be checked out under _build/deps/"
        );

        // The lockfile records the registry version alongside the git pin.
        let lock_content = fs::read_to_string(root.join("beamtalk.lock")).unwrap();
        assert!(lock_content.contains("name = \"my_pkg\""), "{lock_content}");
        assert!(
            lock_content.contains("version = \"1.0.0\""),
            "lockfile should record the registry version: {lock_content}"
        );
        assert!(
            lock_content.contains("reference = \"tag:v1.0.0\""),
            "tag should default to v{{version}}: {lock_content}"
        );
        assert!(lock_content.contains(&sha), "{lock_content}");
    }

    #[test]
    fn test_registry_dep_lockfile_round_trip() {
        let (_git_dir, url, _sha) = create_git_dep_repo("my_pkg", "1.0.0", "");
        let (_index_dir, index_root) = create_registry_index(
            "my_pkg",
            &format!("[[versions]]\nversion = \"1.0.0\"\ngit = \"{url}\"\n"),
        );

        let root_dir = TempDir::new().unwrap();
        let root = root_dir.path().join("root");
        write_registry_project(&root, &index_root, "my_pkg", "1.0.0");
        let root_path = Utf8PathBuf::from_path_buf(root.clone()).unwrap();

        resolve_dependency_graph(&root_path, &beamtalk_core::CompilerOptions::default()).unwrap();

        // Reading the lockfile back preserves the registry version.
        let lock = Lockfile::read(&root_path).unwrap().unwrap();
        let entry = lock.get("my_pkg").unwrap();
        assert_eq!(entry.registry_version.as_deref(), Some("1.0.0"));
        assert_eq!(entry.reference, GitReference::Tag("v1.0.0".to_string()));

        // And re-serializing is stable.
        let reparsed = Lockfile::parse(&lock.serialize()).unwrap();
        assert_eq!(reparsed, lock);
    }

    #[test]
    fn test_registry_dep_resolves_from_lockfile_without_index() {
        let (_git_dir, url, _sha) = create_git_dep_repo("my_pkg", "1.0.0", "");
        let (index_dir, index_root) = create_registry_index(
            "my_pkg",
            &format!("[[versions]]\nversion = \"1.0.0\"\ngit = \"{url}\"\n"),
        );

        let root_dir = TempDir::new().unwrap();
        let root = root_dir.path().join("root");
        write_registry_project(&root, &index_root, "my_pkg", "1.0.0");
        let root_path = Utf8PathBuf::from_path_buf(root.clone()).unwrap();
        let options = beamtalk_core::CompilerOptions::default();

        // First resolve populates the lockfile and the checkout.
        resolve_dependency_graph(&root_path, &options).unwrap();
        assert!(root.join("beamtalk.lock").exists());

        // Destroy the index entirely — a locked build must not need it.
        index_dir.close().unwrap();
        assert!(!index_root.exists(), "index should be gone");

        let result = resolve_dependency_graph(&root_path, &options);
        assert!(
            result.is_ok(),
            "A lockfile hit should bypass the registry index: {:?}",
            result.err()
        );
    }

    /// A git repo carrying two released versions, each with its own tag and a
    /// `beamtalk.toml` declaring that version.
    fn create_two_version_repo(pkg_name: &str) -> (TempDir, String) {
        let dir = TempDir::new().unwrap();
        let path = dir.path();

        run_git_cmd(path, &["init"]);
        run_git_cmd(path, &["config", "user.email", "test@test.com"]);
        run_git_cmd(path, &["config", "user.name", "Test"]);
        run_git_cmd(path, &["config", "commit.gpgsign", "false"]);

        write_manifest(path, pkg_name, "1.0.0", "");
        run_git_cmd(path, &["add", "."]);
        run_git_cmd(path, &["commit", "-m", "v1"]);
        run_git_cmd(path, &["tag", "-m", "v1.0.0", "v1.0.0"]);

        write_manifest(path, pkg_name, "2.0.0", "");
        run_git_cmd(path, &["add", "."]);
        run_git_cmd(path, &["commit", "-m", "v2"]);
        run_git_cmd(path, &["tag", "-m", "v2.0.0", "v2.0.0"]);

        let mut path_str = path.display().to_string().replace('\\', "/");
        if !path_str.starts_with('/') {
            path_str.insert(0, '/');
        }
        (dir, format!("file://{path_str}"))
    }

    /// Bumping the version in `beamtalk.toml` must re-checkout the new tag.
    ///
    /// Regression: the stale lock entry was forwarded to `resolve_git_dep`,
    /// which reuses a checkout whose HEAD matches the locked SHA regardless of
    /// the reference asked for — leaving the old version on disk.
    #[test]
    fn test_registry_dep_version_bump_refetches_checkout() {
        let (_git_dir, url) = create_two_version_repo("my_pkg");
        let (_index_dir, index_root) = create_registry_index(
            "my_pkg",
            &format!(
                "[[versions]]\nversion = \"1.0.0\"\ngit = \"{url}\"\n\
                 [[versions]]\nversion = \"2.0.0\"\ngit = \"{url}\"\n"
            ),
        );

        let root_dir = TempDir::new().unwrap();
        let root = root_dir.path().join("root");
        write_registry_project(&root, &index_root, "my_pkg", "1.0.0");
        let root_path = Utf8PathBuf::from_path_buf(root.clone()).unwrap();
        let options = beamtalk_core::CompilerOptions::default();

        resolve_dependency_graph(&root_path, &options).unwrap();
        let checkout_manifest = crate::commands::build_layout::BuildLayout::new(&root_path)
            .dep_checkout_dir("my_pkg")
            .join("beamtalk.toml");
        assert!(
            fs::read_to_string(&checkout_manifest)
                .unwrap()
                .contains("version = \"1.0.0\""),
            "first resolve should check out 1.0.0"
        );

        // Bump the manifest to 2.0.0 and re-resolve.
        write_registry_project(&root, &index_root, "my_pkg", "2.0.0");
        resolve_dependency_graph(&root_path, &options)
            .expect("version bump should re-resolve cleanly");

        assert!(
            fs::read_to_string(&checkout_manifest)
                .unwrap()
                .contains("version = \"2.0.0\""),
            "the checkout must be updated to the newly requested version"
        );

        let lock = Lockfile::read(&root_path).unwrap().unwrap();
        assert_eq!(
            lock.get("my_pkg").unwrap().registry_version.as_deref(),
            Some("2.0.0")
        );
        assert_eq!(
            lock.get("my_pkg").unwrap().reference,
            GitReference::Tag("v2.0.0".to_string())
        );
    }

    #[test]
    fn test_registry_dep_version_mismatch_is_an_error() {
        // The repo's package declares 0.1.0, but the index advertises the same
        // tag as version 2.0.0 — a packaging error we must not build through.
        let (_git_dir, url, _sha) = create_git_dep_repo("my_pkg", "0.1.0", "");
        let (_index_dir, index_root) = create_registry_index(
            "my_pkg",
            &format!("[[versions]]\nversion = \"2.0.0\"\ngit = \"{url}\"\ntag = \"v1.0.0\"\n"),
        );

        let root_dir = TempDir::new().unwrap();
        let root = root_dir.path().join("root");
        write_registry_project(&root, &index_root, "my_pkg", "2.0.0");
        let root_path = Utf8PathBuf::from_path_buf(root).unwrap();

        let err = resolve_dependency_graph(&root_path, &beamtalk_core::CompilerOptions::default())
            .unwrap_err();
        let msg = format!("{err:?}")
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ");
        assert!(msg.contains("version mismatch"), "{msg}");
        assert!(msg.contains("Requested: '2.0.0'"), "{msg}");
        assert!(msg.contains("Found: '0.1.0'"), "{msg}");
    }

    #[test]
    fn test_registry_dep_unknown_version_lists_available() {
        let (_git_dir, url, _sha) = create_git_dep_repo("my_pkg", "1.0.0", "");
        let (_index_dir, index_root) = create_registry_index(
            "my_pkg",
            &format!("[[versions]]\nversion = \"1.0.0\"\ngit = \"{url}\"\n"),
        );

        let root_dir = TempDir::new().unwrap();
        let root = root_dir.path().join("root");
        write_registry_project(&root, &index_root, "my_pkg", "9.9.9");
        let root_path = Utf8PathBuf::from_path_buf(root).unwrap();

        let err = resolve_dependency_graph(&root_path, &beamtalk_core::CompilerOptions::default())
            .unwrap_err();
        let msg = format!("{err:?}")
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ");
        assert!(msg.contains("Available versions: 1.0.0"), "{msg}");
    }

    /// A transitive registry dependency resolves through the *root* project's
    /// registry — a dependency cannot redirect resolution to a registry of its
    /// own, which would let a package choose where its own deps come from.
    #[test]
    fn test_transitive_registry_dep_uses_root_registry() {
        let (_git_dir, url, _sha) = create_git_dep_repo("leaf_pkg", "1.0.0", "");
        let (_index_dir, index_root) = create_registry_index(
            "leaf_pkg",
            &format!("[[versions]]\nversion = \"1.0.0\"\ngit = \"{url}\"\n"),
        );

        let workspace = TempDir::new().unwrap();

        // The intermediate path dep declares the registry dep, and points at a
        // registry that does not exist — it must be ignored.
        let middle = workspace.path().join("middle");
        fs::create_dir_all(&middle).unwrap();
        write_manifest(
            &middle,
            "middle",
            "0.1.0",
            "[registry]\nurl = \"https://nonexistent.invalid/registry\"\n\n\
             [dependencies]\nleaf_pkg = \"1.0.0\"\n",
        );
        write_source(&middle, "middle.bt", "Object subclass: Middle\n");

        let root = workspace.path().join("root");
        fs::create_dir_all(&root).unwrap();
        write_manifest(
            &root,
            "my_app",
            "0.1.0",
            &format!(
                "[registry]\nurl = \"{index_root}\"\n\n\
                 [dependencies]\nmiddle = {{ path = \"../middle\" }}\n"
            ),
        );

        let root_path = Utf8PathBuf::from_path_buf(root).unwrap();
        let resolved =
            resolve_dependency_graph(&root_path, &beamtalk_core::CompilerOptions::default())
                .expect("root registry should govern the whole graph");

        let names: Vec<&str> = resolved.iter().map(|r| r.name.as_str()).collect();
        assert!(
            names.contains(&"leaf_pkg"),
            "transitive registry dep should resolve: {names:?}"
        );
        assert!(names.contains(&"middle"), "{names:?}");

        // Leaves compile before the package that depends on them.
        let leaf_at = names.iter().position(|n| *n == "leaf_pkg").unwrap();
        let middle_at = names.iter().position(|n| *n == "middle").unwrap();
        assert!(leaf_at < middle_at, "wrong compilation order: {names:?}");
    }

    #[test]
    fn test_registry_dep_unknown_package_errors() {
        let (_git_dir, url, _sha) = create_git_dep_repo("my_pkg", "1.0.0", "");
        let (_index_dir, index_root) = create_registry_index(
            "my_pkg",
            &format!("[[versions]]\nversion = \"1.0.0\"\ngit = \"{url}\"\n"),
        );

        let root_dir = TempDir::new().unwrap();
        let root = root_dir.path().join("root");
        write_registry_project(&root, &index_root, "other_pkg", "1.0.0");
        let root_path = Utf8PathBuf::from_path_buf(root).unwrap();

        let err = resolve_dependency_graph(&root_path, &beamtalk_core::CompilerOptions::default())
            .unwrap_err();
        let msg = format!("{err:?}")
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ");
        assert!(msg.contains("was not found in the registry"), "{msg}");
        assert!(
            msg.contains("Packages in the registry: my_pkg"),
            "should list what the index does have: {msg}"
        );
    }
}
