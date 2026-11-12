// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Class-, alias- and protocol-metadata model and extraction for the stdlib build.
//!
//! **DDD Context:** CLI / Build System
//!
//! Parses each stdlib `.bt` file into the [`ClassMeta`] model (plus alias
//! sources and protocol infos) that [`super::generators`] turns into
//! `generated_builtins.rs`, the `.hrl` headers and the `.app`/`.app.src` files.

use beamtalk_core::semantic_analysis::alias_registry::AliasRegistry;
use beamtalk_core::semantic_analysis::class_hierarchy::{DeclaredType, MethodInfo};
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::fs;

/// Set each class's [`ClassMeta::is_value_subclass`] from the superclass
/// links of every stdlib class (BT-4132), with the hierarchy's own rule
/// (`superclass_chain_includes_value`). An indirect Value subclass's parse
/// says `Object` (e.g. `Collection(E) subclass: Bag(E)`); its chain, which
/// spans files, says `Value`, as `apply_class_kind_writeback` decides for
/// codegen.
pub(super) fn mark_value_subclasses(class_metadata: &mut [ClassMeta]) {
    let superclass_of: std::collections::HashMap<String, String> = class_metadata
        .iter()
        .filter(|m| m.superclass_name != "none")
        .map(|m| (m.class_name.clone(), m.superclass_name.clone()))
        .collect();
    for meta in class_metadata.iter_mut() {
        meta.is_value_subclass =
            beamtalk_core::semantic_analysis::class_hierarchy::superclass_chain_includes_value(
                Some(meta.superclass_name.as_str()).filter(|s| *s != "none"),
                |name| superclass_of.get(name).map(String::as_str),
            );
    }
}

/// Class modifier flags from class hierarchy analysis.
///
/// Groups the boolean modifiers that describe a class's constraints (sealed,
/// abstract, typed, native). Extracted from `ClassMeta` to keep that struct
/// focused on identity and structure.
#[derive(Default)]
#[allow(clippy::struct_excessive_bools)] // these 4 bools are orthogonal class modifier flags
pub(super) struct ClassModifiers {
    /// Whether the class is sealed (cannot be subclassed).
    pub(super) is_sealed: bool,
    /// Whether the class is abstract (cannot be instantiated directly).
    pub(super) is_abstract: bool,
    /// Whether the class has the explicit `typed` modifier.
    pub(super) is_typed: bool,
    /// Whether this class delegates to a native Erlang backing module (ADR 0056).
    pub(super) is_native: bool,
}

/// Metadata for a single stdlib class, used to generate the load-order file
/// and the generated builtins module.
#[allow(clippy::struct_field_names)] // domain names like class_name match the domain model
pub(super) struct ClassMeta {
    /// Erlang module name (e.g., `bt@stdlib@integer`).
    pub(super) module_name: String,
    /// Beamtalk class name (e.g., `Integer`).
    pub(super) class_name: String,
    /// Name of the superclass, or `"none"` for root classes.
    pub(super) superclass_name: String,
    /// Class modifier flags (sealed, abstract, typed, native).
    pub(super) modifiers: ClassModifiers,
    /// Class kind: object, value, or actor (ADR 0067/0070).
    pub(super) class_kind: beamtalk_core::ast::ClassKind,
    /// Instance state (field) names declared in the class.
    pub(super) state: Vec<String>,
    /// Declared type annotations for state fields (field name → type).
    pub(super) state_types: Vec<(String, DeclaredType)>,
    /// Which state fields have an explicit default value (field name → has default).
    /// Carries cross-file default-value presence so downstream
    /// consumers can identify typed-no-default fields without the AST.
    pub(super) state_has_default: Vec<(String, bool)>,
    /// Slot kind (`#eager` | `#late`, ADR 0124 §1) for each declared state
    /// field (field name → kind). Populated for every declared state field,
    /// mirroring `state_has_default`.
    pub(super) state_kinds: Vec<(String, beamtalk_core::ast::SlotKind)>,
    /// Slots this class's own `initialize` method definitely assigns (ADR
    /// 0124 A2a) — a must-analysis over the method body, computed by
    /// [`beamtalk_core::semantic_analysis::analyze_initialize_assigns`].
    /// Empty when the class declares no `initialize`.
    pub(super) initialize_assigns: Vec<String>,
    /// `true` when some instance method sends `fieldAt:put:` or `perform:`
    /// anywhere in its body (ADR 0124 §6, BT-1948) — computed by
    /// [`beamtalk_core::semantic_analysis::has_dynamic_field_writer`].
    pub(super) has_dynamic_field_writer: bool,
    /// Instance method signatures.
    pub(super) methods: Vec<MethodMeta>,
    /// Class-side method signatures.
    pub(super) class_methods: Vec<MethodMeta>,
    /// Class variables (`classState:` declarations) — name, declared type,
    /// default-value presence and slot kind (ADR 0124 B5a).
    pub(super) class_variables: Vec<ClassVarMeta>,
    /// Type parameters for generic classes (e.g., `["T", "E"]` for `Result(T, E)`).
    pub(super) type_params: Vec<String>,
    /// Type arguments passed to the superclass, e.g. `["E"]` for
    /// `Collection(E) subclass: List(E)`. Empty when the parent isn't parametric.
    pub(super) superclass_type_args: Vec<DeclaredType>,
    /// Declared sendability handle scope (ADR 0103, `handleScope: #symbol`).
    /// The bare symbol text (e.g. `"process"`), or `None` when undeclared.
    pub(super) handle_scope: Option<String>,
    /// Synthesized keyword constructor selector, if any — the
    /// `auto_ctors` anchor decision, made against user methods only
    /// (`synthetic_selectors::synthesized_keyword_constructor`). Captured for
    /// every class: an indirect Value subclass is only known to be one once
    /// every file's metadata is in ([`mark_value_subclasses`]).
    pub(super) auto_kw: Option<String>,
    /// Whether the class compiles as a Value: its superclass chain, over all
    /// stdlib classes, reaches `Value` (BT-4132). The post-writeback kind,
    /// not the AST kind `class_kind` (which is `Value` only for a direct
    /// `Value subclass:`). Set by [`mark_value_subclasses`] once every file's
    /// metadata is collected; it gates the `auto_ctors` anchor.
    pub(super) is_value_subclass: bool,
    /// BT-4501: the body shape of this class's explicit class-side `new:`
    /// (`ClassHierarchy::explicit_class_constructor_shape`), published as
    /// `ClassInfo::in_caller_new1`. `runs_class_method_in_caller` adds the
    /// chain half, so a module that only references the class reads the same
    /// answer codegen acted on.
    pub(super) in_caller_new1: bool,
}

/// Sort class metadata by class name — every generated-artifact writer
/// (`generate_builtins_rs`, `generate_erlang_builtins_hrl`) needs the same
/// deterministic ordering so the checked-in `.rs`/`.hrl` files are stable
/// across regenerations. Single implementation per AGENTS.md's
/// "No duplicate implementations" rule.
pub(super) fn sorted_by_class_name(class_metadata: &[ClassMeta]) -> Vec<&ClassMeta> {
    let mut sorted: Vec<&ClassMeta> = class_metadata.iter().collect();
    sorted.sort_by_key(|m| &m.class_name);
    sorted
}

/// Metadata for a single class variable (`classState:` declaration),
/// extracted from the AST — mirrors `ClassVarInfo` (ADR 0124 B5a).
pub(super) struct ClassVarMeta {
    /// The class variable name.
    pub(super) name: String,
    /// Declared type annotation, if any.
    pub(super) ty: Option<DeclaredType>,
    /// Whether this class variable carries an explicit default value.
    pub(super) has_default: bool,
    /// Slot kind (`#eager` | `#late`, ADR 0124 §1).
    pub(super) kind: beamtalk_core::ast::SlotKind,
}

/// Metadata for a single method, extracted from the AST.
#[allow(clippy::struct_excessive_bools)] // independent per-method flags, each documented
pub(super) struct MethodMeta {
    /// Message selector (e.g., `"increment"` or `"add:"`).
    pub(super) selector: String,
    /// Number of arguments the method accepts.
    pub(super) arity: usize,
    /// Method dispatch kind.
    pub(super) kind: MethodKindMeta,
    /// Whether this method is sealed (cannot be overridden).
    pub(super) is_sealed: bool,
    /// Whether this method is internal (package-scoped visibility, ADR 0071).
    pub(super) is_internal: bool,
    /// Whether this method spawns its block argument in a separate BEAM process.
    pub(super) spawns_block: bool,
    /// Return type annotation (e.g., `"Integer"`), if present.
    pub(super) return_type: Option<DeclaredType>,
    /// Parameter type annotations, one per parameter. `None` means untyped.
    pub(super) param_types: Vec<Option<DeclaredType>>,
    /// Doc comment extracted from the source (`///` lines before the method).
    pub(super) doc: Option<String>,
    /// Quoted `@primitive "<name>"` when that is the whole body (BT-4115,
    /// `MethodInfo::primitive`).
    pub(super) primitive: Option<String>,
    /// The scalar literal when it is the whole body (BT-4363, BT-4504,
    /// `MethodInfo::constant_value`).
    pub(super) constant_value: Option<beamtalk_core::ast::ConstantValue>,
    /// Whether the body answers its receiver on every path (BT-4739,
    /// `MethodInfo::answers_receiver`).
    pub(super) answers_receiver: bool,
}

/// Simplified method kind for code generation.
pub(super) enum MethodKindMeta {
    /// Standard method dispatch.
    Primary,
}

impl MethodKindMeta {
    /// Convert from the AST method kind representation.
    fn from_ast(kind: beamtalk_core::ast::MethodKind) -> Self {
        match kind {
            beamtalk_core::ast::MethodKind::Primary => Self::Primary,
        }
    }

    /// Return the Rust expression string for this kind (used in codegen output).
    pub(super) fn to_rust_expr(&self) -> &'static str {
        match self {
            Self::Primary => "MethodKind::Primary",
        }
    }
}

/// Convert an AST method definition to `MethodMeta`; `class_side` selects the
/// side's constant rule ([`MethodInfo::recorded_constant`]).
fn method_def_to_meta(m: &beamtalk_core::ast::MethodDefinition, class_side: bool) -> MethodMeta {
    MethodMeta {
        selector: m.selector.name().to_string(),
        arity: m.selector.arity(),
        kind: MethodKindMeta::from_ast(m.kind),
        is_sealed: m.is_sealed,
        is_internal: m.is_internal,
        spawns_block: false,
        return_type: m.return_type.as_ref().map(DeclaredType::from),
        param_types: m
            .parameters
            .iter()
            .map(|p| p.type_annotation.as_ref().map(DeclaredType::from))
            .collect(),
        doc: m.doc_comment.clone(),
        primitive: m.quoted_primitive_name().map(ToString::to_string),
        constant_value: MethodInfo::recorded_constant(m, class_side),
        answers_receiver: m.answers_receiver(),
    }
}

/// Synthesize auto-generated getter, functional-updater, and keyword constructor
/// methods for Value subclasses with state declarations.
///
/// This mirrors the logic in `add_value_auto_methods` in the semantic analyzer
/// (`class_hierarchy/mod.rs`) and ensures the type checker can resolve
/// state-based accessors without having to process the source `.bt` file first.
#[allow(clippy::too_many_lines)] // synthesizes getter + updater + constructor metadata inline
/// Synthesizes auto getters, `with*` setters and the keyword constructor for
/// a Value subclass, appending to `methods`/`class_methods`. Returns the
/// synthesized keyword selector, if any — the `auto_ctors` anchor
/// decision, made here against user methods only (after this runs, the
/// keyword lives in `class_methods`, so a later re-check could not tell
/// synthesis from a user collision).
pub(super) fn synthesize_value_auto_methods(
    class: &beamtalk_core::ast::ClassDefinition,
    class_name: &str,
    methods: &mut Vec<MethodMeta>,
    class_methods: &mut Vec<MethodMeta>,
) -> Option<String> {
    let user_selectors: std::collections::HashSet<String> =
        methods.iter().map(|m| m.selector.clone()).collect();
    let user_class_selectors: std::collections::HashSet<String> =
        class_methods.iter().map(|m| m.selector.clone()).collect();

    for slot in &class.state {
        let slot_name = slot.name.name.as_str();

        // Auto getter: `fieldName` → slot type (or Object if unannotated)
        if !user_selectors.contains(slot_name) {
            let default_str = slot.default_value.as_ref().map_or_else(
                || "nil".to_string(),
                beamtalk_core::semantic_analysis::class_hierarchy::format_default_value,
            );
            let getter_doc = format!(
                "Returns the `{slot_name}` field value. Default: `{default_str}`.\n\n*(compiler-generated)*"
            );
            methods.push(MethodMeta {
                selector: slot_name.to_string(),
                arity: 0,
                kind: MethodKindMeta::Primary,
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: beamtalk_core::synthetic_selectors::synthesized_getter_return_type(
                    slot,
                ),
                param_types: vec![],
                doc: Some(getter_doc),
                primitive: None,
                constant_value: None,
                answers_receiver: false,
            });
        }

        // Auto functional updater: `withFieldName:` → Self type
        let with_sel = beamtalk_core::synthetic_selectors::with_star_selector(slot_name);
        if !user_selectors.contains(&with_sel) {
            let param_type = beamtalk_core::synthetic_selectors::synthesized_copy_param_type(slot);
            let setter_doc = format!(
                "Returns a new `{class_name}` with `{slot_name}` set to the given value.\n\n*(compiler-generated)*"
            );
            methods.push(MethodMeta {
                selector: with_sel,
                arity: 1,
                kind: MethodKindMeta::Primary,
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some(
                    beamtalk_core::synthetic_selectors::synthesized_copy_return_type(),
                ),
                param_types: vec![param_type],
                doc: Some(setter_doc),
                primitive: None,
                constant_value: None,
                answers_receiver: false,
            });
        }
    }

    // Auto keyword constructor on the class side: `field1:field2:...`
    let kw_sel: String = beamtalk_core::synthetic_selectors::synthesized_keyword_constructor(
        class.state.iter().map(|s| s.name.name.as_str()),
        |sel| user_class_selectors.contains(sel),
    )?;
    let arity = class.state.len();
    let param_types = class
        .state
        .iter()
        .map(beamtalk_core::synthetic_selectors::synthesized_constructor_param_type)
        .collect();
    let args_desc: String = class
        .state
        .iter()
        .map(|s| {
            let dv = s.default_value.as_ref().map_or_else(
                || "nil".to_string(),
                beamtalk_core::semantic_analysis::class_hierarchy::format_default_value,
            );
            format!("{} (default: {})", s.name.name, dv)
        })
        .collect::<Vec<_>>()
        .join(", ");
    let ctor_doc =
        format!("Creates a new `{class_name}`. Args: {args_desc}.\n\n*(compiler-generated)*");
    class_methods.push(MethodMeta {
        selector: kw_sel.clone(),
        arity,
        kind: MethodKindMeta::Primary,
        is_sealed: false,
        is_internal: false,
        spawns_block: false,
        return_type: Some(
            beamtalk_core::synthetic_selectors::synthesized_constructor_return_type(class_name),
        ),
        param_types,
        doc: Some(ctor_doc),
        primitive: None,
        constant_value: None,
        answers_receiver: false,
    });
    Some(kw_sel)
}

/// Iterate `methods`, set `spawns_block = true` for every selector listed in
/// `selectors`, then verify every selector was matched. Bails with a diagnostic
/// naming `context` and any unmatched selectors — an unmatched selector means
/// the stdlib source no longer defines a method this metadata assumes exists.
pub(super) fn mark_spawns_for_selectors(
    methods: &mut [MethodMeta],
    selectors: &[&str],
    context: &str,
) -> Result<()> {
    for m in methods.iter_mut() {
        if selectors.iter().any(|&s| s == m.selector) {
            m.spawns_block = true;
        }
    }
    let missing: Vec<&str> = selectors
        .iter()
        .copied()
        .filter(|&s| methods.iter().all(|m| m.selector != s))
        .collect();
    if !missing.is_empty() {
        miette::bail!(
            "{context} metadata mismatch: expected spawning selectors {selectors:?} \
             but not found: {missing:?}"
        );
    }
    Ok(())
}

/// `Timer` class methods that run their block in another process.
const TIMER_SPAWN_SELECTORS: &[&str] = &["after:do:", "every:do:"];

/// `Parallel` class methods that run their blocks in other processes.
const PARALLEL_SPAWN_SELECTORS: &[&str] = &["all:", "all:timeout:", "any:"];

/// `Collection` instance methods that delegate to `Parallel all:`.
/// `Block` instance methods that run the *receiver* block in another process.
const BLOCK_SPAWN_SELECTORS: &[&str] = &["fork"];

const COLLECTION_SPAWN_SELECTORS: &[&str] =
    &["parallelCollect:", "parallelCollect:maxConcurrency:"];

/// Mark Timer class methods that spawn their block argument in a separate BEAM process.
///
/// Replaces hardcoded list in `validators.rs` so the self-capture validator
/// can skip false-positive warnings.
pub(super) fn mark_timer_spawns(class_methods: &mut [MethodMeta]) -> Result<()> {
    mark_spawns_for_selectors(class_methods, TIMER_SPAWN_SELECTORS, "Timer")
}

/// Mark `Parallel` class methods that spawn their block arguments in separate
/// BEAM processes.
///
/// Feeds the `spawns_block_selectors()` metadata a future lint uses to warn when
/// a synchronous `self` send appears inside a spawned-block argument (deadlock
/// risk: an actor blocks in `Parallel all:`, a block does a synchronous send back
/// to the same actor, and hangs).
pub(super) fn mark_parallel_spawns(class_methods: &mut [MethodMeta]) -> Result<()> {
    mark_spawns_for_selectors(class_methods, PARALLEL_SPAWN_SELECTORS, "Parallel")
}

/// Mark `Collection>>parallelCollect:` and `parallelCollect:maxConcurrency:`
/// as spawning their block argument — both
/// delegate to `Parallel all:` under the hood (directly, or via the internal
/// `runChunked:maxConcurrency:` helper), same rationale as
/// `mark_parallel_spawns`. The internal `runChunked:...` helpers are not
/// marked: their own parameter is an already-wrapped `List(Block)`, not the
/// user's raw block, so the same-actor-deadlock risk this metadata guards
/// against doesn't apply to their own call sites the way it does to the two
/// public entry points.
pub(super) fn mark_parallel_collect_spawns(methods: &mut [MethodMeta]) -> Result<()> {
    mark_spawns_for_selectors(methods, COLLECTION_SPAWN_SELECTORS, "Collection")
}

/// Mark `Block>>fork`, which runs its receiver block in a new process
/// (BT-4198). Here `spawns_block` means the receiver block is the spawned one,
/// since `fork` takes no block argument.
fn mark_block_spawns(methods: &mut [MethodMeta]) -> Result<()> {
    mark_spawns_for_selectors(methods, BLOCK_SPAWN_SELECTORS, "Block")
}

/// A single stdlib type-alias declaration collected by
/// `collect_stdlib_alias_sources`, carrying both its
/// reconstructed `AliasInfo` (for immediately seeding *this* run's own
/// `pre_loaded_aliases`) and its exact declaration source text (for
/// persisting into `generated_builtins.rs`, so a consumer with no direct
/// access to `stdlib/src/*.bt` — e.g. a REPL/workspace session —
/// can still reconstruct it later via
/// `ClassHierarchy::generated_stdlib_aliases`).
pub(super) struct AliasSource {
    pub(super) info: beamtalk_core::semantic_analysis::alias_registry::AliasInfo,
    pub(super) text: String,
}

/// Scans every stdlib source file for `type Name = ...` declarations,
/// reconstructing each into an `AliasSource` immediately.
///
/// **Why a live, same-run pre-pass — not a seed from
/// `ClassHierarchy::generated_stdlib_aliases`'s persisted snapshot** (the
/// design this replaced during review): `build-stdlib` always runs with
/// `--warnings-as-errors` (`Justfile`'s `build-stdlib` recipe), and an
/// unresolved cross-file alias reference surfaces as a
/// `DiagnosticCategory::Type` warning — a category *not* excluded from
/// warnings-as-errors promotion, unlike `UnresolvedClass` (see
/// `beam_compiler.rs`'s exclusion list). Seeding from the *previous* run's
/// persisted snapshot instead of a live scan would mean a stdlib change that
/// declares `type Foo = ...` in one file and references `Foo` cross-file in
/// another, landed in the *same* commit, bails before `generate_builtins_rs`
/// ever runs — so `Foo` would never get persisted, and every subsequent
/// `build-stdlib` run would repeat the exact same failure with no way out
/// short of splitting the change across two builds or dropping
/// `--warnings-as-errors`. A live pre-pass has direct access to every
/// stdlib source file right now, so — unlike a consumer with no source
/// access, which genuinely has no better option than the persisted
/// snapshot — it never needs to fall back to stale data at all.
///
/// Fails loudly (`Result::Err`, not a silent skip) if a freshly-sliced
/// declaration fails to re-parse via
/// [`AliasRegistry::from_source_text`] — that would indicate a bug in the
/// slicing logic itself (e.g. a span not covering a standalone-parseable
/// declaration), not a tolerable degradation. Contrast
/// `ClassHierarchy::generated_stdlib_aliases`'s defensive skip-on-failure,
/// which exists only to tolerate a corrupted or hand-edited *generated*
/// file at runtime, long after this build-time check already passed.
pub(super) fn collect_stdlib_alias_sources(
    source_files: &[Utf8PathBuf],
) -> Result<Vec<AliasSource>> {
    let mut all = Vec::new();
    for file in source_files {
        for text in extract_alias_source_snippets(file)? {
            let info = AliasRegistry::from_source_text(&text).ok_or_else(|| {
                miette::miette!(
                    "Internal error: sliced type-alias declaration in '{file}' failed to \
                     re-parse: {text:?}"
                )
            })?;
            all.push(AliasSource { info, text });
        }
    }
    Ok(all)
}

/// Scans every stdlib source file for `Protocol define: ...` declarations,
/// extracting each into a `ProtocolInfo` immediately.
///
/// **Why a live, same-run pre-pass — mirroring [`collect_stdlib_alias_sources`]
/// exactly:** `build_stdlib()`'s `CompileContext` seeds `pre_loaded_aliases`
/// from a live scan; without an equivalent live scan for protocols,
/// `pre_loaded_protocols` would sit at its `Vec::default()`, so a stdlib file
/// compiled with a `:: SomeProtocol` type annotation referencing a protocol
/// declared in *another* stdlib file (e.g. `console.bt`/`json.bt`'s
/// `:: Printable` parameters, with `Printable` declared in `printable.bt`)
/// could never see that protocol as resolved — each file is compiled
/// independently, with only this pre-pass's output as its window into the
/// rest of stdlib.
///
/// That would degrade *silently*, with no diagnostic at all — not even a
/// warning: `build_stdlib()` never sets `pre_loaded_classes` either, so
/// `has_cross_file_classes` is always `false` for every stdlib compile,
/// which gates off `check_unresolved_classes` entirely (see its call site's
/// doc in `semantic_analysis/mod.rs` — "only check ... when cross-file
/// metadata has been loaded"). So a missing `Printable` registration would
/// never surface as an `UnresolvedClass` diagnostic in this
/// pipeline in the first place. The actual damage would be one level deeper: the
/// type checker's `check_protocol_argument_conformance`
/// (`type_checker/validation.rs`) short-circuits with
/// `let Some(_protocol) = protocol_registry.get(base_protocol) else { return; }`
/// whenever the named protocol isn't registered — so with `Printable`
/// unregistered, structural protocol-conformance checking on every
/// `Printable`-typed argument anywhere in stdlib would be silently skipped
/// (never verifying, never warning): a silent degrade-to-unresolved failure
/// mode. Nothing short
/// of an explicit `pre_loaded_protocols`/`ProtocolRegistry` inspection
/// (see this function's regression test) makes the gap visible; no build
/// flag, including `--warnings-as-errors`, could ever have caught it.
///
/// Unlike [`collect_stdlib_alias_sources`], this needs no source-text
/// slicing/re-parse round trip and no `package` stamp: `ProtocolInfo` carries
/// no `package` field (protocols have no `internal` modifier at the AST
/// level — see `collect_project_protocol_and_alias_infos`'s doc in
/// `build.rs`, which this mirrors). (User compilation gets the stdlib's
/// provision-bearing protocols from `generated_builtins.rs`'s
/// `generated_stdlib_protocol_files()` table instead — see
/// [`generate_protocol_files_section`]; this live pre-pass is what the stdlib
/// build itself uses.) `ProtocolRegistry::extract_protocol_infos`
/// already returns owned, directly-usable `ProtocolInfo` values, so a single
/// per-file lex/parse/extract pass is enough.
///
/// Parse errors on an individual file are non-fatal here — consistent with
/// `build.rs`'s `collect_project_protocol_and_alias_infos`, which the
/// ordinary `beamtalk build` pipeline already uses for this exact purpose:
/// that file simply contributes no protocols to the merged set, and the same
/// parse error is already reported through the normal per-file diagnostics
/// path when that file is compiled on its own.
///
/// Also returns the full AST of every *provision-bearing* protocol found
/// (ADR 0127 §10a; BT-3591) — the trait-flattening counterpart to the
/// `ProtocolInfo` metadata above, handed to every stdlib file's
/// `ClassHierarchyContext::pre_loaded_protocol_defs` so a stdlib class's
/// `uses:` line naming a trait declared in a *different* stdlib file
/// flattens (mirrors this function's own `ProtocolInfo` pre-pass reasoning,
/// one level deeper: the same cross-file gap `ProtocolInfo` closes for
/// conformance/`extending:` resolution exists for provisions, since
/// `trait_expansion::expand_module`'s own-module-only fallback would
/// otherwise report every cross-file stdlib trait as "unknown protocol").
///
/// The third element is each such protocol's source file (path + text —
/// ADR 0127 §3, "Source locations"; BT-3625), so codegen can map a
/// flattened provision's spans to lines of the *protocol's* file.
#[allow(clippy::type_complexity)] // 3-tuple mirrors this module's other multi-output extractors
pub(super) fn collect_stdlib_protocol_infos(
    source_files: &[Utf8PathBuf],
) -> (
    Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo>,
    Vec<beamtalk_core::ast::ProtocolDefinition>,
    beamtalk_core::semantic_analysis::ProtocolSourceMap,
) {
    let mut all = Vec::new();
    let mut all_defs = Vec::new();
    let mut all_sources = beamtalk_core::semantic_analysis::ProtocolSourceMap::new();
    for file in source_files {
        let Ok(source) = fs::read_to_string(file) else {
            continue;
        };
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let (module, _diagnostics) = beamtalk_core::source_analysis::parse(tokens);
        all.extend(
            beamtalk_core::semantic_analysis::protocol_registry::ProtocolRegistry::extract_protocol_infos(
                &module,
            ),
        );
        for protocol in module
            .protocols
            .into_iter()
            .filter(beamtalk_core::semantic_analysis::trait_expansion::is_provision_bearing)
        {
            all_sources.insert(
                protocol.name.name.clone(),
                beamtalk_core::semantic_analysis::ProtocolSource {
                    path: Some(file.as_str().into()),
                    text: source.as_str().into(),
                    package: Some("stdlib".into()),
                },
            );
            all_defs.push(protocol);
        }
    }
    (all, all_defs, all_sources)
}

/// Extracts `alias_sources`' declaration texts sorted by alias *name* (not
/// raw declaration text — `internal type Foo` would otherwise sort under
/// "i", not alongside plain `type` entries), for
/// [`generate_alias_sources_section`], which trusts its caller to have
/// already sorted (see that function's doc).
pub(super) fn alias_source_texts_sorted_by_name(
    mut alias_sources: Vec<AliasSource>,
) -> Vec<String> {
    alias_sources.sort_by(|a, b| a.info.name.cmp(&b.info.name));
    alias_sources.into_iter().map(|a| a.text).collect()
}

/// Extracts every `type Name = ...` declaration in a `.bt` file as verbatim
/// source text, one string per alias.
///
/// Slices each declaration's exact span out of the original source rather
/// than reconstructing it from the parsed `TypeAnnotation` — see
/// `generate_builtins_rs`'s doc for why. A file with no type-alias
/// declarations (the common case) returns an empty `Vec`.
pub(super) fn extract_alias_source_snippets(path: &Utf8Path) -> Result<Vec<String>> {
    let source = fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read '{path}'"))?;
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, _diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    Ok(AliasRegistry::extract_alias_infos(&module)
        .iter()
        .filter_map(|info| {
            let start = usize::try_from(info.span.start()).ok()?;
            let end = usize::try_from(info.span.end()).ok()?;
            source.get(start..end).map(str::to_string)
        })
        .collect())
}

/// Check whether a `.bt` file contains only protocol definitions (no classes).
///
/// Protocol-only files (e.g. `printable.bt`) define structural protocols via
/// `Protocol define:` but contain no class definitions. These files still need
/// to be compiled so the protocol gets registered at runtime, but they have no
/// class metadata to extract.
pub(super) fn is_protocol_only_file(path: &Utf8Path) -> Result<bool> {
    let source = fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read '{path}'"))?;
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    // If parsing produced errors, the class/protocol lists may be incomplete.
    // Be conservative: treat as a normal class file so extract_class_metadata
    // reports the real error instead of silently misclassifying.
    let has_errors = diagnostics
        .iter()
        .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error);
    if has_errors {
        return Ok(false);
    }

    Ok(module.classes.is_empty() && !module.protocols.is_empty())
}

/// Extract class metadata from a `.bt` source file.
///
/// Parses the full class definition to extract the class name, superclass,
/// flags, state declarations, and method signatures. Each stdlib file
/// contains exactly one class definition.
#[allow(clippy::too_many_lines)] // field-mapping function — length is proportional to ClassMeta's fields
pub(super) fn extract_class_metadata(
    path: &Utf8Path,
    module_name: &str,
    external_protocols: &std::collections::HashMap<
        ecow::EcoString,
        beamtalk_core::ast::ProtocolDefinition,
    >,
) -> Result<ClassMeta> {
    let source = fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read '{path}'"))?;

    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (mut module, _diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    // ADR 0127 §3/§10a (BT-3594): this generator does its own from-scratch
    // parse of the file (not the real `compile_stdlib_file` pipeline a few
    // lines up in `compile_all_stdlib_files`), so a `uses:` line's provided
    // methods are never spliced into `class.methods` unless this flattens
    // them itself — otherwise every provision a stdlib class gains (e.g.
    // `DateTime uses: Comparable`'s `between:and:`/`min:`/`max:`) would be
    // invisible to `generated_builtins.rs`, and every consumer of that
    // static table (`beamtalk test`/`lint`/the LSP compiling a file that
    // merely *references* the stdlib class, never recompiling it) would
    // report a false "does not understand" for a selector the real,
    // compiled stdlib class answers just fine. Diagnostics are dropped here
    // — `compile_stdlib_file`'s own real compile of this same file already
    // reports them once; this pass exists purely to keep this metadata
    // snapshot in sync with what that compile actually produces.
    if module.classes.iter().any(|c| !c.uses.is_empty()) {
        let _ = beamtalk_core::semantic_analysis::trait_expansion::expand_module(
            &mut module,
            external_protocols,
        );
    }

    let class = module
        .classes
        .first()
        .ok_or_else(|| miette::miette!("No class definition in '{path}'"))?;

    if module.classes.len() > 1 {
        miette::bail!(
            "Expected exactly one class in '{path}', found {}",
            module.classes.len()
        );
    }

    let mut methods: Vec<MethodMeta> = class
        .methods
        .iter()
        .map(|m| method_def_to_meta(m, false))
        .collect();
    let mut class_methods: Vec<MethodMeta> = class
        .class_methods
        .iter()
        .map(|m| method_def_to_meta(m, true))
        .collect();

    let state = class
        .state
        .iter()
        .map(|s| s.name.name.to_string())
        .collect();

    let state_types = class
        .state
        .iter()
        .filter_map(|s| {
            s.type_annotation
                .as_ref()
                .map(|ty| (s.name.name.to_string(), DeclaredType::from(ty)))
        })
        .collect();

    let state_has_default = class
        .state
        .iter()
        .map(|s| (s.name.name.to_string(), s.default_value.is_some()))
        .collect();

    let state_kinds = class
        .state
        .iter()
        .map(|s| (s.name.name.to_string(), s.slot_kind))
        .collect();

    // ADR 0124 A2a: this class's own `initialize` definitely-assigned slot
    // summary, sorted (the must-analysis returns a BTreeSet, so this is
    // already deterministic) for stable regeneration.
    let initialize_assigns = class
        .methods
        .iter()
        .find(|m| {
            m.kind == beamtalk_core::ast::MethodKind::Primary && m.selector.name() == "initialize"
        })
        .map(|m| {
            beamtalk_core::semantic_analysis::analyze_initialize_assigns(&m.body)
                .into_iter()
                .map(|s| s.to_string())
                .collect()
        })
        .unwrap_or_default();

    // BT-1948 (ADR 0124 §6): whether any instance method dynamically writes
    // a slot via `fieldAt:put:`/`perform:` — see the `ClassMeta` field doc.
    let has_dynamic_field_writer =
        beamtalk_core::semantic_analysis::has_dynamic_field_writer(&class.methods);

    let class_variables = class
        .class_variables
        .iter()
        .map(|cv| ClassVarMeta {
            name: cv.name.name.to_string(),
            ty: cv.type_annotation.as_ref().map(DeclaredType::from),
            has_default: cv.default_value.is_some(),
            kind: cv.slot_kind,
        })
        .collect();

    let class_name = class.name.name.to_string();

    // Synthesize auto-generated methods for Value subclasses with state,
    // capturing the keyword-constructor decision for the anchor. Every other
    // class records the same decision without synthesis: whether it is an
    // indirect Value subclass is only known once all files are read.
    let auto_kw =
        if class.class_kind == beamtalk_core::ast::ClassKind::Value && !class.state.is_empty() {
            synthesize_value_auto_methods(class, &class_name, &mut methods, &mut class_methods)
        } else {
            beamtalk_core::synthetic_selectors::synthesized_keyword_constructor(
                class.state.iter().map(|s| s.name.name.as_str()),
                |sel| class_methods.iter().any(|m| m.selector == sel),
            )
        };

    // Mark Timer methods that spawn their block argument
    if class_name == "Timer" {
        mark_timer_spawns(&mut class_methods)?;
    }

    // Mark Parallel/Collection methods that spawn their block argument(s)
    if class_name == "Parallel" {
        mark_parallel_spawns(&mut class_methods)?;
    }
    if class_name == "Block" {
        mark_block_spawns(&mut methods)?;
    }
    if class_name == "Collection" {
        mark_parallel_collect_spawns(&mut methods)?;
    }

    let type_params = class
        .type_params
        .iter()
        .map(|tp| tp.name.name.to_string())
        .collect();

    let superclass_type_args = class
        .superclass_type_args
        .iter()
        .map(DeclaredType::from)
        .collect();

    // BT-4501: the explicit-`new:` verdict needs the AST, so it is decided
    // here and persisted (the same rule codegen publishes in
    // `process_bound_class_selectors`).
    let in_caller_new1 = {
        let (hierarchy, _) =
            beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy::build_with_options(
                &module, true,
            );
        hierarchy
            .ok()
            .and_then(|h| h.get_class(&class_name).map(|c| c.in_caller_new1))
            .unwrap_or(false)
    };

    Ok(ClassMeta {
        module_name: module_name.to_string(),
        class_name,
        superclass_name: class.superclass_name().to_string(),
        modifiers: ClassModifiers {
            is_sealed: class.is_sealed,
            is_abstract: class.is_abstract,
            is_typed: class.is_typed,
            is_native: class.backing_module.is_some(),
        },
        class_kind: class.class_kind,
        state,
        state_types,
        state_has_default,
        state_kinds,
        initialize_assigns,
        has_dynamic_field_writer,
        methods,
        class_methods,
        class_variables,
        type_params,
        superclass_type_args,
        handle_scope: class.handle_scope.as_ref().map(|s| s.name.to_string()),
        auto_kw,
        // Decided across files by `mark_value_subclasses`.
        is_value_subclass: false,
        in_caller_new1,
    })
}
