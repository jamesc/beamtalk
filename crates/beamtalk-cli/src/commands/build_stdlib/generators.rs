// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Generators for the stdlib build's checked-in and `ebin` artefacts.
//!
//! **DDD Context:** CLI / Build System
//!
//! Emits `generated_builtins.rs`, the Erlang `.hrl` headers, and the
//! `beamtalk_stdlib.app` / `beamtalk_stdlib.app.src` files from the
//! [`super::metadata::ClassMeta`] model.

use super::metadata::{ClassMeta, MethodMeta, sorted_by_class_name};
use super::orchestrate::{STDLIB_APP_FILE, STDLIB_SOURCE_DIR, module_name_from_path};
use crate::commands::app_file;
use crate::commands::util::format_sorted_atom_list;
use beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType;
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::fmt::Write;
use std::fs;
use tracing::debug;

/// Format a single stdlib class metadata entry in ADR 0070 Phase 4 map format.
fn format_stdlib_class_entry(m: &ClassMeta) -> String {
    // ADR 0103: `handle_scope` is intentionally omitted from this lightweight
    // `.app.src` registry (like `is_native`, `fields`, and `methods`). The
    // authoritative tier channel is `__beamtalk_meta/0` in the compiled `.beam`
    // (read back by the compiler-port), which does carry it. No stdlib class
    // declares `handleScope:` today, so there is no divergence to reconcile.
    app_file::format_class_entry(&app_file::ClassMetadata {
        module: m.module_name.clone(),
        class_name: m.class_name.clone(),
        parent_class: m.superclass_name.clone(),
        package: "stdlib".to_string(),
        kind: m.class_kind.as_str().to_string(),
        type_params: m.type_params.clone(),
    })
}

/// Format stdlib class metadata entries joined with the given separator.
///
/// Sorted by class name so the generated `.app`/`.app.src` content depends
/// only on *which* classes exist, never on the order the source tree happened
/// to be walked in. Without this, moving a class into a `stdlib/src/`
/// subdirectory reshuffles a checked-in generated file for no semantic reason.
fn format_stdlib_classes_list(class_metadata: &[ClassMeta], separator: &str) -> String {
    sorted_by_class_name(class_metadata)
        .into_iter()
        .map(format_stdlib_class_entry)
        .collect::<Vec<_>>()
        .join(separator)
}

/// Generate the `beamtalk_stdlib.app` file in the ebin directory.
///
/// Lists all modules — the class modules *and* the app's Erlang modules
/// (`erlang_modules`), so the file is a complete `.app` on its own: it is
/// the one `scripts/install-otp-apps.sh` installs and the one dev mode loads
/// (see [`sync_rebar3_stdlib_ebin`]) — and embeds class hierarchy metadata
/// in the `env` section.
/// The metadata is used by `beamtalk_stdlib` to load modules in dependency order.
///
/// **`type_aliases` env key (ADR 0108 Phase 8):** mirrors
/// [`super::app_file::generate_app_file`] (the real `beamtalk build`
/// pipeline), which emits `{type_aliases, [...]}` via
/// [`super::build::build_alias_metadata`] + [`app_file::format_type_aliases_entry`].
/// Without this key, `application:get_env(beamtalk_stdlib, type_aliases)`
/// returns `undefined` forever, so neither `browse-type-aliases`
/// (`beamtalk_repl_ops_browse.erl`) nor the REPL/workspace session's
/// alias-seeding path (`beamtalk_repl_state:new/3`) can ever learn
/// stdlib's own `type Name = ...` declarations.
pub(super) fn generate_app_file(
    ebin_dir: &Utf8Path,
    source_files: &[Utf8PathBuf],
    erlang_modules: &[String],
    class_metadata: &[ClassMeta],
    protocol_modules: &[String],
    alias_metadata: &[app_file::AliasMetadata],
) -> Result<()> {
    let mut module_names: Vec<String> = source_files
        .iter()
        .map(|f| module_name_from_path(f))
        .collect::<Result<_>>()?;
    module_names.extend_from_slice(erlang_modules);

    let modules_list = format_sorted_atom_list(&module_names);

    // ADR 0070 Phase 4: Generate extended class hierarchy entries for env
    let classes_list = format_stdlib_classes_list(class_metadata, ",\n                    ");

    // Protocol-only modules need to be loaded separately during stdlib init
    let protocol_modules_list = format_sorted_atom_list(protocol_modules);

    // Same `{type_aliases, [...]}` entry the ordinary `beamtalk
    // build` pipeline emits (`app_file::format_type_aliases_entry`) — empty
    // string (no key at all) when stdlib declares no aliases.
    let type_aliases_entry = app_file::format_type_aliases_entry(alias_metadata);

    let version = env!("BEAMTALK_VERSION");
    let app_content = format!(
        "{{application, beamtalk_stdlib, [\n\
         \x20   {{description, \"Beamtalk Standard Library - compiled from lib/*.bt\"}},\n\
         \x20   {{vsn, \"{version}\"}},\n\
         \x20   {{modules, [{modules_list}]}},\n\
         \x20   {{registered, []}},\n\
         \x20   {{applications, [kernel, stdlib, crypto, beamtalk_runtime]}},\n\
         \x20   {{env, [\n\
         \x20       {{classes, [{classes_list}]}},\n\
         \x20       {{protocol_modules, [{protocol_modules_list}]}}{type_aliases_entry}\n\
         \x20   ]}}\n\
         ]}}.\n"
    );

    let app_file = ebin_dir.join(STDLIB_APP_FILE);
    fs::write(&app_file, app_content)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write '{app_file}'"))?;

    debug!("Generated {}", app_file);
    Ok(())
}

/// Generate/update the `.app.src` file so rebar3 picks up the classes metadata.
///
/// The `.app.src` uses `{modules, []}` (rebar3 auto-fills modules) but embeds
/// the `{classes, [...]}` and `{type_aliases, [...]}` envs for the
/// runtime to read via `application:get_env`.
pub(super) fn generate_app_src_file(
    src_dir: &Utf8Path,
    class_metadata: &[ClassMeta],
    protocol_modules: &[String],
    alias_metadata: &[app_file::AliasMetadata],
) -> Result<()> {
    // ADR 0070 Phase 4: Generate extended class hierarchy entries for env
    let classes_list = format_stdlib_classes_list(class_metadata, ",\n            ");

    // Protocol-only modules need to be loaded separately during stdlib init
    let protocol_modules_list = format_sorted_atom_list(protocol_modules);

    // See `generate_app_file`'s doc.
    let type_aliases_entry = app_file::format_type_aliases_entry(alias_metadata);

    let app_src_content = format!(
        "{{application, beamtalk_stdlib, [\n\
         \x20   {{description, \"Beamtalk Standard Library - compiled from lib/*.bt\"}},\n\
         \x20   {{vsn, {{cmd, \"escript ../../../scripts/version.escript\"}}}},\n\
         \x20   {{modules, []}},\n\
         \x20   {{registered, []}},\n\
         \x20   {{applications, [kernel, stdlib, crypto, beamtalk_runtime]}},\n\
         \x20   {{env, [\n\
         \x20       {{classes, [\n\
         \x20           {classes_list}\n\
         \x20       ]}},\n\
         \x20       {{protocol_modules, [{protocol_modules_list}]}}{type_aliases_entry}\n\
         \x20   ]}}\n\
         ]}}.\n"
    );

    let app_src_file = src_dir.join("beamtalk_stdlib.app.src");
    fs::write(&app_src_file, &app_src_content)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write '{app_src_file}'"))?;

    debug!("Generated {}", app_src_file);
    Ok(())
}

/// Default path for generated builtins file (relative to project root).
pub(super) const GENERATED_BUILTINS_PATH: &str =
    "crates/beamtalk-core/src/semantic_analysis/class_hierarchy/generated_builtins.rs";

/// Default path for the generated Erlang builtin-class-list header.
pub(super) const GENERATED_BUILTINS_HRL_PATH: &str =
    "runtime/apps/beamtalk_runtime/include/beamtalk_generated_builtins.hrl";

/// Default path for the generated class-variable key-shape header (ADR 0130).
const GENERATED_CLASS_VARS_KEYS_HRL_PATH: &str =
    "runtime/apps/beamtalk_runtime/include/beamtalk_class_vars_keys.hrl";

/// Default path for the generated removed-supervisor-config header (ADR 0135).
const GENERATED_SUPERVISOR_CONFIG_HRL_PATH: &str =
    "runtime/apps/beamtalk_runtime/include/beamtalk_supervisor_config.hrl";

/// Write generated `content` to `dest`, but only if it differs from what's
/// already there — skips touching the file (and downstream recompilation)
/// when a stdlib build produces byte-identical output.
///
/// Shared by `generate_builtins_rs` and `generate_erlang_builtins_hrl`, which
/// otherwise each hand-rolled this exact read-compare-write dance.
/// Taking `dest` as a parameter — rather than each caller reading its own
/// hardcoded `GENERATED_*_PATH` constant internally — is also what makes this
/// unit-testable: real call sites still pass the hardcoded constants, but
/// tests can point `dest` at a temp file instead of risking a write into the
/// real checked-in generated files (see AGENTS.md's "Generated files" rule).
pub(super) fn write_generated_file_if_changed(dest: &Utf8Path, content: &str) -> Result<()> {
    let needs_write = match fs::read_to_string(dest) {
        Ok(existing) => existing != content,
        Err(_) => true,
    };

    if needs_write {
        fs::write(dest, content)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to write '{dest}'"))?;
        debug!("Generated {}", dest);
    } else {
        debug!("Generated file unchanged, skipping write: {}", dest);
    }
    Ok(())
}

/// Generate the `generated_builtins.rs` file from parsed stdlib class and
/// type-alias metadata.
///
/// This produces a Rust source file that defines `generated_builtin_classes()`,
/// `is_generated_builtin_class()`, and `generated_stdlib_alias_sources()`,
/// replacing the hand-written tables in `builtins.rs`.
///
/// **Design decision — how a stdlib `type Name = ...` alias is
/// persisted here:** `AliasInfo::annotation` is a full, recursive
/// `TypeAnnotation` AST (unions, generics, `\`/`&`, nested `Box`es, `Span`s
/// on every node) — nothing like `ClassInfo`/`MethodInfo`'s flat
/// `Option<EcoString>` return/param-type strings, which this generator
/// already emits as simple quoted-string literals (see
/// `generate_method_list`). Three representations were considered:
///
/// 1. **Add `serde` derives to `TypeAnnotation`** (and transitively
///    `Identifier`) and embed a serialized (e.g. JSON) blob. Rejected: both
///    types are deep in the shared AST used by parser, semantic analysis,
///    codegen, and the LSP — adding derives there is a real, ongoing
///    maintenance surface (every future `TypeAnnotation` variant needs a
///    serde-compatible shape) for a feature only this one generator needs.
/// 2. **Hand-write a recursive Rust-literal-construction emitter** for
///    `TypeAnnotation` (mirroring `generate_superclass_type_args`'s much
///    simpler two-variant `SuperclassTypeArg` emitter). Rejected: it would
///    duplicate the parser's own understanding of the AST's shape in a
///    second, hand-maintained place that silently falls out of sync (no
///    compile error) whenever `TypeAnnotation` gains a variant — the emitter
///    would need a matching arm added by hand, and forgetting one would
///    silently miscompile or panic on a real alias RHS instead of failing to
///    build.
/// 3. **Store each alias's verbatim declaration source text** (`"type
///    RestartStrategy = #temporary | #transient | #permanent"`, sliced
///    directly from the original `.bt` file via
///    [`extract_alias_source_snippets`]) and re-parse it back into an
///    `AliasInfo` at load time via
///    [`beamtalk_core::semantic_analysis::alias_registry::AliasRegistry::from_source_text`].
///    **Chosen.** Zero new derives, zero hand-maintained AST mirror — every
///    consumer goes through the same lexer/parser the rest of the compiler
///    already trusts, so a new `TypeAnnotation` variant just works the
///    moment the parser supports it. The cost is a handful of cheap
///    single-line re-lexes at `ClassHierarchy::generated_stdlib_aliases()`
///    call time (today: 5 stdlib aliases) — negligible next to compiling
///    every stdlib file in the same pipeline. This mirrors
///    `MethodInfo::return_type`'s existing stringly-typed precedent in this
///    very generator, just keeping the *whole* declaration instead of a bare
///    type name (a bare name isn't enough to reconstruct a union/generic
///    RHS).
pub(super) fn generate_builtins_rs(
    class_metadata: &[ClassMeta],
    alias_sources: &[String],
    protocol_file_names: &[String],
) -> Result<()> {
    let mut code = String::new();

    code.push_str(
        "// AUTO-GENERATED from lib/*.bt by `beamtalk build-stdlib` — do not edit manually.\n\
         // Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         //! Generated built-in class definitions derived from `stdlib/src/*.bt` sources.\n\
         //!\n\
         //! **Do not edit this file.** Modify the `.bt` source in `stdlib/src/` and\n\
         //! run `just build` (or `beamtalk build-stdlib`) to regenerate.\n\
         \n\
         // `ClassVarInfo` is unused whenever no stdlib class declares `classState:`.\n\
         #[allow(unused_imports)]\n\
         use super::super::{\n\
         \x20   AutoConstructors, ClassInfo, ClassVarInfo, DeclaredType, MethodInfo, SuperclassTypeArg,\n\
         };\n\
         use crate::ast::{ConstantValue, MethodKind, SlotKind};\n\
         use ecow::EcoString;\n\
         use std::collections::{BTreeSet, HashMap};\n\
         \n",
    );

    // Generate is_generated_builtin_class()
    code.push_str(
        "/// Returns true if the given class name is a stdlib built-in class.\n\
         ///\n\
         /// Auto-generated from `lib/*.bt` file names.\n\
         #[allow(clippy::too_many_lines)] // auto-generated: one match arm per stdlib class\n\
         pub(super) fn is_generated_builtin_class(name: &str) -> bool {\n\
         \x20   matches!(\n\
         \x20       name,\n",
    );

    let sorted_meta = sorted_by_class_name(class_metadata);

    for (i, meta) in sorted_meta.iter().enumerate() {
        if i == 0 {
            let _ = write!(code, "        \"{}\"", meta.class_name);
        } else {
            let _ = write!(code, "\n            | \"{}\"", meta.class_name);
        }
    }
    code.push_str("\n    )\n}\n\n");

    // One helper function per class, so that no single stack frame holds the
    // struct-literal temporaries of every stdlib method at once. A single
    // `generated_builtin_classes()` body that built all ~100 classes inline
    // had a ~970 KB debug-build frame (each `MethodInfo` literal gets its own
    // slot), which overflowed the 1 MB Windows main-thread stack once
    // `MethodInfo` grew a field (BT-3868).
    for meta in &sorted_meta {
        generate_class_entry(&mut code, meta);
    }

    // Generate generated_builtin_classes()
    code.push_str(
        "/// Returns all stdlib built-in class definitions.\n\
         ///\n\
         /// Auto-generated from parsed `lib/*.bt` ASTs.\n\
         #[allow(clippy::too_many_lines)] // auto-generated: one insert per stdlib class\n\
         #[rustfmt::skip] // preserve compact generated layout\n\
         pub(super) fn generated_builtin_classes() -> HashMap<EcoString, ClassInfo> {\n\
         \x20   let mut classes = HashMap::new();\n",
    );

    for meta in &sorted_meta {
        let _ = writeln!(
            code,
            "    classes.insert(\"{name}\".into(), {func}());",
            name = meta.class_name,
            func = class_info_fn_name(&meta.class_name),
        );
    }

    code.push_str("    classes\n}\n");

    // Generate generated_stdlib_alias_sources() — see this
    // function's own doc for the source-text-persisted-and-reparsed design.
    generate_alias_sources_section(&mut code, alias_sources);

    // Generate generated_stdlib_protocol_files() — the stdlib's
    // provision-bearing protocols for user compilation (BT-3915).
    generate_protocol_files_section(&mut code, protocol_file_names);

    let dest = Utf8PathBuf::from(GENERATED_BUILTINS_PATH);
    write_generated_file_if_changed(&dest, &code)
}

/// Generate the `beamtalk_generated_builtins.hrl` Erlang header from parsed
/// stdlib class metadata.
///
/// Defines `?BEAMTALK_GENERATED_BUILTIN_CLASSES`, the Erlang-side twin of
/// `generated_builtins.rs`'s `is_generated_builtin_class` match arms — both
/// are emitted from the exact same `class_metadata` list computed earlier in
/// `build_stdlib()`, so they cannot drift the way the old hand-typed
/// `beamtalk_class_metadata:all_builtins/0` list did. `beamtalk_class_metadata.erl`
/// includes this header and prepends the one runtime-only exception
/// (`'Future'`, no `stdlib/src/Future.bt` source — mirrors `builtins.rs`'s
/// `is_builtin_class` on the Rust side).
pub(super) fn generate_erlang_builtins_hrl(class_metadata: &[ClassMeta]) -> Result<()> {
    let sorted_meta = sorted_by_class_name(class_metadata);

    let mut code = String::new();
    code.push_str(
        "%% AUTO-GENERATED from stdlib/src/*.bt by `beamtalk build-stdlib` — do not edit manually.\n\
         %% Copyright 2026 James Casey\n\
         %% SPDX-License-Identifier: Apache-2.0\n\
         \n\
         %% Generated built-in class list derived from `stdlib/src/*.bt` sources.\n\
         %%\n\
         %% Do not edit this file. Modify the `.bt` source in `stdlib/src/` and run\n\
         %% `just build` (or `beamtalk build-stdlib`) to regenerate.\n\
         %%\n\
         %% Mirrors `is_generated_builtin_class` in\n\
         %% crates/beamtalk-core/src/semantic_analysis/class_hierarchy/generated_builtins.rs\n\
         %% — both are generated from the same stdlib class-metadata pass\n\
         %% in build_stdlib.rs, so they cannot drift out of sync with each other.\n\
         \n\
         -ifndef(BEAMTALK_GENERATED_BUILTINS_HRL).\n\
         -define(BEAMTALK_GENERATED_BUILTINS_HRL, true).\n\
         \n\
         -define(BEAMTALK_GENERATED_BUILTIN_CLASSES, [\n",
    );

    for (i, meta) in sorted_meta.iter().enumerate() {
        let sep = if i + 1 == sorted_meta.len() { "" } else { "," };
        let _ = writeln!(code, "    '{}'{sep}", meta.class_name);
    }

    code.push_str("]).\n\n-endif.\n");

    let dest = Utf8PathBuf::from(GENERATED_BUILTINS_HRL_PATH);
    write_generated_file_if_changed(&dest, &code)
}

/// Generate `beamtalk_class_vars_keys.hrl` from codegen's `class_var_keys` leaf
/// (ADR 0130 §2): the class-variable process-dictionary key shape the inlined
/// reads and writes codegen emits and `beamtalk_class_vars.erl` must agree on,
/// rendered from one Rust source instead of copied (AGENTS.md shared-fixture rule).
pub(super) fn generate_class_vars_keys_hrl() -> Result<()> {
    let dest = Utf8PathBuf::from(GENERATED_CLASS_VARS_KEYS_HRL_PATH);
    write_generated_file_if_changed(
        &dest,
        &beamtalk_codegen::core_erlang::class_var_keys::erlang_header(),
    )
}

/// Generate `beamtalk_supervisor_config.hrl` from the compiler's list of
/// removed class-side supervisor configuration selectors (ADR 0135 §2), so the
/// runtime's override warning and the compile error share one source.
pub(super) fn generate_supervisor_config_hrl() -> Result<()> {
    let dest = Utf8PathBuf::from(GENERATED_SUPERVISOR_CONFIG_HRL_PATH);
    write_generated_file_if_changed(
        &dest,
        &beamtalk_core::semantic_analysis::validators::removed_supervisor_config_erlang_header(),
    )
}

/// Name of the generated per-class helper function that builds `class_name`'s
/// `ClassInfo` (see [`generate_class_entry`]).
fn class_info_fn_name(class_name: &str) -> String {
    format!("class_info_{class_name}")
}

/// Generate the per-class helper function (`class_info_<Name>() -> ClassInfo`)
/// that `generated_builtin_classes()` calls for this class.
#[allow(clippy::too_many_lines)] // one contiguous struct-literal emission
pub(super) fn generate_class_entry(code: &mut String, meta: &ClassMeta) {
    let superclass = if meta.superclass_name == "none" {
        "None".to_string()
    } else {
        format!("Some(\"{}\".into())", meta.superclass_name)
    };

    let _ = write!(
        code,
        "#[allow(non_snake_case)] // named after the stdlib class\n\
         #[allow(clippy::too_many_lines)] // auto-generated: one literal per method\n\
         #[inline(never)] // keep each class's literal temporaries in their own frame\n\
         #[rustfmt::skip] // preserve compact generated layout\n\
         fn {func}() -> ClassInfo {{\n\
         \x20   ClassInfo {{\n\
         \x20           name: \"{name}\".into(),\n\
         \x20           superclass: {superclass},\n\
         \x20           is_sealed: {sealed},\n\
         \x20           is_abstract: {abstract_},\n\
         \x20           is_typed: {typed},\n\
         \x20           is_internal: false,\n\
         \x20           package: Some(\"stdlib\".into()),\n\
         \x20           is_value: {is_value},\n\
         \x20           is_native: {is_native},\n\
         \x20           handle_scope: {handle_scope},\n\
         \x20           surface_incomplete: false,\n\
         \x20           in_caller_new1: {in_caller_new1},\n\
         \x20           is_module_less: false,\n",
        func = class_info_fn_name(&meta.class_name),
        name = meta.class_name,
        sealed = meta.modifiers.is_sealed,
        abstract_ = meta.modifiers.is_abstract,
        typed = meta.modifiers.is_typed,
        is_value = meta.superclass_name == "Value",
        is_native = meta.modifiers.is_native,
        in_caller_new1 = meta.in_caller_new1,
        // ADR 0103: emit the declared handle scope so it survives regeneration.
        handle_scope = meta
            .handle_scope
            .as_deref()
            .map_or_else(|| "None".to_string(), |s| format!("Some(\"{s}\".into())")),
    );

    // Constructor synthesis record, the hierarchy's rule
    // (`ClassInfo::record_constructor_anchor`, BT-4132): `Some` iff the class
    // compiles as a Value, direct or indirect (`meta.is_value_subclass`, the
    // post-writeback kind; not the AST kind). The keyword decision comes from
    // `meta.auto_kw` (captured before synthesis): `meta.class_methods`
    // already holds the synthesized keyword itself.
    let auto_ctors = if meta.is_value_subclass {
        match &meta.auto_kw {
            Some(sel) => {
                format!("Some(AutoConstructors {{ keyword_constructor: Some(\"{sel}\".into()) }})")
            }
            None => "Some(AutoConstructors { keyword_constructor: None })".to_string(),
        }
    } else {
        "None".to_string()
    };
    let _ = writeln!(code, "            auto_ctors: {auto_ctors},");

    // State
    if meta.state.is_empty() {
        code.push_str("            state: vec![],\n");
    } else {
        code.push_str("            state: vec![");
        for (i, s) in meta.state.iter().enumerate() {
            if i > 0 {
                code.push_str(", ");
            }
            let _ = write!(code, "\"{s}\".into()");
        }
        code.push_str("],\n");
    }

    // State types
    if meta.state_types.is_empty() {
        code.push_str("            state_types: HashMap::new(),\n");
    } else {
        code.push_str("            state_types: HashMap::from([");
        for (i, (field, ty)) in meta.state_types.iter().enumerate() {
            if i > 0 {
                code.push_str(", ");
            }
            let _ = write!(
                code,
                "(\"{field}\".into(), {})",
                declared_type_to_rust_expr(ty)
            );
        }
        code.push_str("]),\n");
    }

    // state_has_default — used by gen_server post-initialize
    // validation to identify typed-no-default inherited fields without the AST.
    if meta.state_has_default.is_empty() {
        code.push_str("            state_has_default: HashMap::new(),\n");
    } else {
        code.push_str("            state_has_default: HashMap::from([");
        for (i, (field, has_default)) in meta.state_has_default.iter().enumerate() {
            if i > 0 {
                code.push_str(", ");
            }
            let _ = write!(code, "(\"{field}\".into(), {has_default})");
        }
        code.push_str("]),\n");
    }

    // state_kinds (ADR 0124 §1) — mirrors state_has_default: every declared
    // field gets an entry so cross-file consumers see `#eager`/`#late`
    // without the AST.
    if meta.state_kinds.is_empty() {
        code.push_str("            state_kinds: HashMap::new(),\n");
    } else {
        code.push_str("            state_kinds: HashMap::from([");
        for (i, (field, kind)) in meta.state_kinds.iter().enumerate() {
            if i > 0 {
                code.push_str(", ");
            }
            let _ = write!(
                code,
                "(\"{field}\".into(), {})",
                slot_kind_to_rust_expr(*kind)
            );
        }
        code.push_str("]),\n");
    }

    // initialize_assigns (ADR 0124 A2a): this class's own `initialize`
    // definitely-assigned slot summary — a `BTreeSet` so entries are already
    // sorted, keeping regeneration deterministic.
    if meta.initialize_assigns.is_empty() {
        code.push_str("            initialize_assigns: BTreeSet::new(),\n");
    } else {
        code.push_str("            initialize_assigns: BTreeSet::from([");
        for (i, field) in meta.initialize_assigns.iter().enumerate() {
            if i > 0 {
                code.push_str(", ");
            }
            let _ = write!(code, "\"{field}\".into()");
        }
        code.push_str("]),\n");
    }

    // has_dynamic_field_writer (ADR 0124 §6, BT-1948): whether any instance
    // method sends `fieldAt:put:`/`perform:` anywhere in its body.
    let _ = writeln!(
        code,
        "            has_dynamic_field_writer: {},",
        meta.has_dynamic_field_writer
    );

    // Instance methods
    generate_method_list(code, "methods", &meta.methods, &meta.class_name);
    // Class methods
    generate_method_list(code, "class_methods", &meta.class_methods, &meta.class_name);

    // Class variables (ADR 0124 B5a: name, declared type, default presence, kind)
    if meta.class_variables.is_empty() {
        code.push_str("            class_variables: vec![],\n");
    } else {
        code.push_str("            class_variables: vec![");
        for (i, cv) in meta.class_variables.iter().enumerate() {
            if i > 0 {
                code.push_str(", ");
            }
            let ty_expr = cv.ty.as_ref().map_or_else(
                || "None".to_string(),
                |ty| format!("Some({})", declared_type_to_rust_expr(ty)),
            );
            let _ = write!(
                code,
                "ClassVarInfo {{ name: \"{}\".into(), ty: {ty_expr}, has_default: {}, kind: {} }}",
                cv.name,
                cv.has_default,
                slot_kind_to_rust_expr(cv.kind)
            );
        }
        code.push_str("],\n");
    }

    // Type parameters
    if meta.type_params.is_empty() {
        code.push_str("            type_params: vec![],\n");
        code.push_str("            type_param_bounds: vec![],\n");
    } else {
        code.push_str("            type_params: vec![");
        for (i, tp) in meta.type_params.iter().enumerate() {
            if i > 0 {
                code.push_str(", ");
            }
            let _ = write!(code, "\"{tp}\".into()");
        }
        code.push_str("],\n");
        // Type parameter bounds (ADR 0068 Phase 2d) — currently all None
        // until stdlib classes declare bounded type params
        code.push_str("            type_param_bounds: vec![");
        for (i, _) in meta.type_params.iter().enumerate() {
            if i > 0 {
                code.push_str(", ");
            }
            code.push_str("None");
        }
        code.push_str("],\n");
    }

    generate_superclass_type_args(code, &meta.superclass_type_args, &meta.type_params);

    code.push_str("    }\n}\n\n");
}

/// Emit the `generated_stdlib_alias_sources()` function body: one
/// `&'static str` literal per stdlib type-alias declaration, in the order
/// given by `alias_sources`.
///
/// Unlike [`generate_class_entry`]'s `sorted_meta` (sorted right before
/// calling this file's class-entry generator), sorting alias entries by
/// *name* requires the caller's already-reconstructed `AliasInfo` — plain
/// declaration text alone would sort `internal type Foo = ...` under "i",
/// not alongside `type` entries — so the caller (`build_stdlib()`) is
/// responsible for passing `alias_sources` pre-sorted by name; this function
/// only formats whatever order it's given.
///
/// See `generate_builtins_rs`'s doc for why each alias is persisted as its
/// verbatim declaration source text rather than a literal-Rust
/// `TypeAnnotation` construction or a `serde` blob.
pub(super) fn generate_alias_sources_section(code: &mut String, alias_sources: &[String]) {
    code.push_str(
        "\n/// Returns the verbatim `type Name = ...` declaration source text for every\n\
         /// stdlib type alias, sorted by alias name.\n\
         ///\n\
         /// Auto-generated from `stdlib/src/*.bt` ASTs. See\n\
         /// `crates/beamtalk-cli/src/commands/build_stdlib.rs`'s `generate_builtins_rs`\n\
         /// doc for why source text (re-parsed by\n\
         /// `AliasRegistry::from_source_text`), not a literal-Rust `TypeAnnotation`\n\
         /// construction or a `serde` blob, was chosen to represent each alias here.\n\
         pub(super) fn generated_stdlib_alias_sources() -> Vec<&'static str> {\n\
         \x20   vec![\n",
    );

    for text in alias_sources {
        let _ = writeln!(code, "        {},", rust_str_lit(text));
    }
    code.push_str("    ]\n}\n");
}

/// The file name (no directory) of every stdlib source file that declares a
/// provision-bearing protocol, sorted and deduplicated — the files
/// [`generate_protocol_files_section`] embeds.
pub(super) fn provision_bearing_protocol_file_names(
    protocol_sources: &beamtalk_core::semantic_analysis::ProtocolSourceMap,
) -> Vec<String> {
    let mut names: Vec<String> = protocol_sources
        .values()
        .filter_map(|src| src.path.as_deref())
        .filter_map(|path| Utf8Path::new(path).file_name().map(str::to_string))
        .collect();
    names.sort();
    names.dedup();
    names
}

/// Emit `generated_stdlib_protocol_files()`: `(path, text)` of every stdlib
/// file declaring a provision-bearing protocol (BT-3915).
///
/// The text is `include_str!` of the stdlib source itself, not a copy, so the
/// protocols a user package flattens through `uses: Enumerable` /
/// `uses: Comparable` are exactly the ones in `stdlib/src/` — the generator
/// only decides *which* files carry them. `semantic_analysis::stdlib_protocols`
/// parses them (once) into the definitions and source map every compile path
/// falls back to after its own carried protocols.
pub(super) fn generate_protocol_files_section(code: &mut String, file_names: &[String]) {
    code.push_str(
        "\n/// Returns `(path, source text)` for every stdlib file that declares a\n\
         /// provision-bearing protocol (ADR 0127 §10a; BT-3915), sorted by path.\n\
         ///\n\
         /// Auto-generated by `crates/beamtalk-cli/src/commands/build_stdlib.rs`'s\n\
         /// `generate_protocol_files_section`. The text is the stdlib source itself\n\
         /// (`include_str!`), not a copy.\n\
         #[rustfmt::skip] // preserve compact generated layout\n\
         pub(super) fn generated_stdlib_protocol_files() -> Vec<(&'static str, &'static str)> {\n\
         \x20   vec![\n",
    );
    for name in file_names {
        let path = format!("{STDLIB_SOURCE_DIR}/{name}");
        let _ = writeln!(
            code,
            "        ({}, include_str!({})),",
            rust_str_lit(&path),
            rust_str_lit(&format!("../../../../../{path}")),
        );
    }
    code.push_str("    ]\n}\n");
}

/// Emit `superclass_type_args: vec![...]` for a subclass's parent binding.
///
/// Each entry is either a [`SuperclassTypeArg::ParamRef`] when the argument
/// names one of the subclass's own type parameters (e.g. `Collection(E)
/// subclass: Array(E)`) or a [`SuperclassTypeArg::Concrete`] otherwise (e.g.
/// `Collection(Integer) subclass: IntArray`).
pub(super) fn generate_superclass_type_args(
    code: &mut String,
    args: &[DeclaredType],
    type_params: &[String],
) {
    if args.is_empty() {
        code.push_str("            superclass_type_args: vec![],\n");
        return;
    }
    code.push_str("            superclass_type_args: vec![");
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            code.push_str(", ");
        }
        let param_ref_idx = match arg {
            DeclaredType::Simple(name) => type_params.iter().position(|p| p == name.as_str()),
            _ => None,
        };
        if let Some(idx) = param_ref_idx {
            let _ = write!(code, "SuperclassTypeArg::ParamRef {{ param_index: {idx} }}");
        } else {
            let _ = write!(
                code,
                "SuperclassTypeArg::Concrete {{ declared: {} }}",
                declared_type_to_rust_expr(arg)
            );
        }
    }
    code.push_str("],\n");
}

/// Serialise a [`beamtalk_core::ast::SlotKind`] into the Rust source
/// expression that reconstructs it (ADR 0124 §1) — the `state_kinds`/
/// `class_variables` counterpart to `declared_type_to_rust_expr`.
fn slot_kind_to_rust_expr(kind: beamtalk_core::ast::SlotKind) -> &'static str {
    match kind {
        beamtalk_core::ast::SlotKind::Eager => "SlotKind::Eager",
        beamtalk_core::ast::SlotKind::Late => "SlotKind::Late",
    }
}

/// Serialise a [`DeclaredType`] into a Rust source expression that
/// reconstructs it verbatim — the structured counterpart to stringifying via
/// `Display` and re-parsing on read. Mirrors every `DeclaredType` variant:
/// `Simple`/`Singleton`/`Generic`/`Union` go through
/// the compact constructors added for this purpose
/// ([`DeclaredType::simple`], etc.); the remaining variants (`FalseOr`,
/// `Difference`, `Intersection`, `SelfType`, `SelfClass`, `ClassOf`) are rare
/// in stdlib signatures but still emitted structurally rather than silently
/// dropped, so a stdlib author who *does* write one of them gets a correct
/// generated artifact rather than a degraded one.
pub(super) fn declared_type_to_rust_expr(dt: &DeclaredType) -> String {
    match dt {
        DeclaredType::Simple(name) => format!("DeclaredType::simple({})", rust_str_lit(name)),
        DeclaredType::Singleton(name) => {
            format!("DeclaredType::singleton({})", rust_str_lit(name))
        }
        DeclaredType::Union(members) => {
            let parts: Vec<String> = members.iter().map(declared_type_to_rust_expr).collect();
            format!("DeclaredType::union(vec![{}])", parts.join(", "))
        }
        DeclaredType::Generic { base, parameters } => {
            let parts: Vec<String> = parameters.iter().map(declared_type_to_rust_expr).collect();
            format!(
                "DeclaredType::generic({}, vec![{}])",
                rust_str_lit(base),
                parts.join(", ")
            )
        }
        DeclaredType::FalseOr(inner) => {
            format!(
                "DeclaredType::FalseOr(Box::new({}))",
                declared_type_to_rust_expr(inner)
            )
        }
        DeclaredType::Difference { base, excluded } => format!(
            "DeclaredType::Difference {{ base: Box::new({}), excluded: Box::new({}) }}",
            declared_type_to_rust_expr(base),
            declared_type_to_rust_expr(excluded)
        ),
        DeclaredType::Intersection { left, right } => format!(
            "DeclaredType::Intersection {{ left: Box::new({}), right: Box::new({}) }}",
            declared_type_to_rust_expr(left),
            declared_type_to_rust_expr(right)
        ),
        DeclaredType::Shape { fields, open } => {
            let parts: Vec<String> = fields
                .iter()
                .map(|f| {
                    format!(
                        "DeclaredType::shape_field({}, {}, {})",
                        rust_str_lit(&f.key),
                        declared_type_to_rust_expr(&f.value),
                        f.optional
                    )
                })
                .collect();
            format!("DeclaredType::shape(vec![{}], {open})", parts.join(", "))
        }
        DeclaredType::SelfType => "DeclaredType::SelfType".to_string(),
        DeclaredType::SelfClass => "DeclaredType::SelfClass".to_string(),
        DeclaredType::ClassOf(name) => {
            format!("DeclaredType::ClassOf({})", rust_str_lit_into(name))
        }
    }
}

/// Escaped Rust string literal (no `.into()` suffix) — for use as an
/// argument to a `DeclaredType` compact constructor (`impl Into<EcoString>`
/// parameters accept a bare `&str` literal directly).
fn rust_str_lit(s: &str) -> String {
    let escaped = s
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\r', "\\r")
        .replace('\n', "\\n")
        .replace('\t', "\\t");
    format!("\"{escaped}\"")
}

/// Escaped Rust string literal with a trailing `.into()` — for direct
/// enum-variant construction where the field type is a bare `EcoString`
/// (no `impl Into` coercion available).
fn rust_str_lit_into(s: &str) -> String {
    format!("{}.into()", rust_str_lit(s))
}

/// The Rust expression that rebuilds `value` in `generated_builtins.rs`
/// (BT-4504). `{:?}` of an `f64` and a `char` is a valid Rust literal of the
/// same value (parsed literals are finite).
fn constant_value_to_rust_expr(value: &beamtalk_core::ast::ConstantValue) -> String {
    use beamtalk_core::ast::ConstantValue;
    match value {
        ConstantValue::Integer(n) => format!("ConstantValue::Integer({n})"),
        ConstantValue::Float(f) => format!("ConstantValue::Float({f:?})"),
        ConstantValue::String(s) => format!("ConstantValue::String({})", rust_str_lit_into(s)),
        ConstantValue::Symbol(s) => format!("ConstantValue::Symbol({})", rust_str_lit_into(s)),
        ConstantValue::Character(c) => format!("ConstantValue::Character({c:?})"),
        ConstantValue::Nil => "ConstantValue::Nil".to_string(),
        ConstantValue::True => "ConstantValue::True".to_string(),
        ConstantValue::False => "ConstantValue::False".to_string(),
    }
}

/// Generate a method list field (`methods` or `class_methods`).
pub(super) fn generate_method_list(
    code: &mut String,
    field_name: &str,
    methods: &[MethodMeta],
    class_name: &str,
) {
    if methods.is_empty() {
        let _ = writeln!(code, "            {field_name}: vec![],");
        return;
    }

    let _ = writeln!(code, "            {field_name}: vec![");
    for m in methods {
        let kind = m.kind.to_rust_expr();
        let selector = rust_str_lit(&m.selector);
        let return_type_expr = match &m.return_type {
            Some(t) => format!("Some({})", declared_type_to_rust_expr(t)),
            None => "None".to_string(),
        };
        let param_types_expr = if m.param_types.is_empty() {
            "vec![]".to_string()
        } else {
            let parts: Vec<_> = m
                .param_types
                .iter()
                .map(|p| match p {
                    Some(t) => format!("Some({})", declared_type_to_rust_expr(t)),
                    None => "None".to_string(),
                })
                .collect();
            format!("vec![{}]", parts.join(", "))
        };
        let doc_expr = match &m.doc {
            Some(doc) => format!("Some({}.into())", rust_str_lit(doc)),
            None => "None".to_string(),
        };
        let primitive_expr = match &m.primitive {
            Some(name) => format!("Some({}.into())", rust_str_lit(name)),
            None => "None".to_string(),
        };
        let constant_value_expr = match &m.constant_value {
            Some(value) => format!("Some({})", constant_value_to_rust_expr(value)),
            None => "None".to_string(),
        };
        let _ = writeln!(
            code,
            "                MethodInfo {{ selector: {selector}.into(), arity: {arity}, \
             kind: {kind}, defined_in: \"{class}\".into(), is_sealed: {sealed}, \
             is_internal: {internal}, spawns_block: {spawns_block}, \
             return_type: {return_type_expr}, param_types: {param_types_expr}, doc: {doc_expr}, \
             origin: None, primitive: {primitive_expr}, \
             constant_value: {constant_value_expr}, answers_receiver: {answers_receiver} }},",
            arity = m.arity,
            class = class_name,
            sealed = m.is_sealed,
            internal = m.is_internal,
            spawns_block = m.spawns_block,
            answers_receiver = m.answers_receiver,
        );
    }
    code.push_str("            ],\n");
}
