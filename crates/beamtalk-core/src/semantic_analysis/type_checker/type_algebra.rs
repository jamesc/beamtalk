// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type algebra on [`InferredType`]: `union_of`, `intersect`, `difference` and
//! negation normalisation (split from `types.rs`).
//!
//! **DDD Context:** Semantic Analysis

use crate::semantic_analysis::ClassHierarchy;
use crate::semantic_analysis::protocol_registry::ProtocolRegistry;
use crate::source_analysis::Span;
use ecow::EcoString;

use super::types::{DynamicReason, InferredType, TypeProvenance};

impl InferredType {
    /// Builds a union type, simplifying when possible.
    ///
    /// - If all resolved members are the same type, returns that type.
    /// - If any member is `Dynamic`, returns `Dynamic` (can't validate).
    /// - Otherwise returns `Union { members, .. }` with deduplication.
    ///
    /// Provenance is derived from the first input that carries a non-default
    /// provenance (Declared or Substituted win over Inferred).
    ///
    /// A member's [`TypeProvenance::Aliased`] tag never wins this
    /// selection (see [`provenance_wins_union_default`](Self::provenance_wins_union_default)) —
    /// a freshly-built union is a *different* type from any one member's
    /// alias expansion, so it must never silently claim that member's alias
    /// identity as its own display name. `best_provenance` itself is
    /// therefore never `Aliased`, which is also why the single-member
    /// collapse case below (which checks the *member's own* provenance, not
    /// `best_provenance`) needs no corresponding change: `Aliased` already
    /// fails its `matches!(.., Inferred(_))` guard today, so an aliased
    /// singleton member is already never overwritten.
    ///
    /// **Order-dependent edge case:**
    /// when two *structurally-equal* members disagree only in provenance
    /// (e.g. `union_of([Port_aliased, Integer_plain])` where `type Port =
    /// Integer`), dedup (`flat.contains`, which is provenance-blind) keeps
    /// whichever operand was pushed first and drops the other — so the
    /// surviving member's display depends on input order. This holds
    /// for `Declared` vs. `Inferred` too; `Aliased` carries
    /// the same characteristic, not a new one.
    pub(crate) fn union_of(members: &[Self]) -> Self {
        let mut flat: Vec<InferredType> = Vec::new();
        let mut best_provenance = TypeProvenance::Inferred(Span::default());
        for m in members {
            match m {
                Self::Known { provenance, .. }
                | Self::Meta { provenance, .. }
                | Self::ProtocolMeta { provenance, .. }
                | Self::Negation { provenance, .. }
                // A `Shape` is opaque to `union_of` too: shape members stay
                // separate and are never merged into a wider shape (ADR 0147
                // § Shapes in the type algebra); duplicates collapse by
                // order-independent structural equality.
                | Self::Shape { provenance, .. }
                // `Intersection` is opaque to `union_of` (ADR 0102 §1) — treated
                // like any other opaque member: kept as-is and deduplicated by
                // structural equality, with no absorption/flattening law of its
                // own (unlike `Negation`, which `union_of` actively simplifies).
                | Self::Intersection { provenance, .. } => {
                    if matches!(best_provenance, TypeProvenance::Inferred(_))
                        && Self::provenance_wins_union_default(provenance)
                    {
                        best_provenance = provenance.clone();
                    }
                    if !flat.contains(m) {
                        flat.push(m.clone());
                    }
                }
                Self::Union {
                    members: inner,
                    provenance,
                } => {
                    if matches!(best_provenance, TypeProvenance::Inferred(_))
                        && Self::provenance_wins_union_default(provenance)
                    {
                        best_provenance = provenance.clone();
                    }
                    for inner_m in inner {
                        if let Self::Known { provenance: p, .. } = inner_m
                            && matches!(best_provenance, TypeProvenance::Inferred(_))
                                && Self::provenance_wins_union_default(p)
                            {
                                best_provenance = p.clone();
                            }
                        if !flat.contains(inner_m) {
                            flat.push(inner_m.clone());
                        }
                    }
                }
                Self::Dynamic(reason) => return Self::Dynamic(*reason),
                Self::Never => { /* identity element — skip */ }
            }
        }
        // Apply the negation absorption law (`(Symbol \ #foo) | #foo ⇒ Symbol`),
        // drop singletons subsumed by a `Symbol`-based negation, and collapse
        // bare singletons under a bare `Symbol` (`#a | Symbol ⇒ Symbol` — this
        // fires with or without a negation present, keeping one normal form
        // per set so `intersect` stays commutative).
        Self::absorb_negations(&mut flat);
        match flat.len() {
            0 if members.iter().all(|m| matches!(m, Self::Never)) && !members.is_empty() => {
                Self::Never
            }
            0 => Self::Dynamic(DynamicReason::Unknown),
            1 => match flat.into_iter().next().unwrap() {
                Self::Known {
                    class_name,
                    type_args,
                    provenance,
                } if matches!(provenance, TypeProvenance::Inferred(_))
                    && !matches!(best_provenance, TypeProvenance::Inferred(_)) =>
                {
                    Self::Known {
                        class_name,
                        type_args,
                        provenance: best_provenance,
                    }
                }
                only => only,
            },
            _ => Self::Union {
                members: flat,
                provenance: best_provenance,
            },
        }
    }

    /// Whether a member's provenance is eligible to become a freshly-built
    /// union's own "best provenance" (see [`union_of`](Self::union_of)).
    ///
    /// `Inferred` never wins (it's the no-information default). `Aliased`
    /// (ADR 0108) never wins either, even though it's informative —
    /// letting it win would mean a union like `A | Integer` (where alias `A`
    /// resolves to some `InferredType` tagged `Aliased("A", _)`) could
    /// silently render as `A (...)`, even though `A | Integer` is not
    /// structurally identical to `A`'s own expansion. Only a member's alias
    /// tag on *itself* is ever display-accurate; a container built from it is
    /// a different type and must never inherit that identity.
    fn provenance_wins_union_default(p: &TypeProvenance) -> bool {
        !matches!(
            p,
            TypeProvenance::Inferred(_) | TypeProvenance::Aliased { .. }
        )
    }

    /// Returns `true` if `name` is a symbol singleton (`#foo`) — a subtype of
    /// `Symbol` in the type checker's singleton convention.
    fn is_symbol_singleton(name: &str) -> bool {
        crate::semantic_analysis::string_utils::is_symbol_singleton_name(name)
    }

    /// Returns `true` if `ty` is the base of a well-formed **singleton-excluded**
    /// [`Negation`] — i.e. `Known("Symbol")`. `Symbol` is the sole supertype of
    /// singletons the type checker models, so it is the only base for which
    /// singleton subtraction and absorption are sound.
    ///
    /// Distinct from the **nominal-excluded** `Negation` flavour (ADR 0102 §5,
    /// e.g. `Object \ Number`), whose base is any nominal class — see
    /// the nominal-class arm of [`difference`](Self::difference). The two
    /// flavours are distinguished structurally by whether `excluded` is a
    /// singleton/union-of-singletons or a nominal class name; this predicate
    /// only ever needs to ask "is this the Symbol-singleton flavour".
    ///
    /// [`Negation`]: InferredType::Negation
    fn is_symbol_base(ty: &Self) -> bool {
        matches!(ty, Self::Known { class_name, .. } if class_name == "Symbol")
    }

    /// Returns `true` if every symbol singleton (`#foo`) is a member of `ty`'s
    /// value set — i.e. `ty` is `Symbol` itself, or (with a hierarchy) a
    /// nominal *supertype* of `Symbol` such as `ProtoObject`.
    ///
    /// Used by the symbol-singleton arms of [`intersect`](Self::intersect) so
    /// `ProtoObject ∩ #foo` reduces to `#foo` instead of falling through to
    /// the nominal arm (where `h.is_subclass_of("#foo", "ProtoObject")`
    /// is `false` — singletons are never hierarchy entries) and yielding a
    /// spurious `Never`. `Object` is already absorbed by the top-identity
    /// arms, so in practice the hierarchy case only fires for supertypes
    /// *above* `Object` (`ProtoObject` in the builtin hierarchy). Without a
    /// hierarchy this is exactly [`is_symbol_base`](Self::is_symbol_base) —
    /// the previous, conservative behaviour.
    ///
    /// Note this deliberately does **not** widen the *singleton-excluded*
    /// [`Negation`] well-formedness: `is_symbol_base` (exactly `Symbol`)
    /// remains the only valid base for subtracting a *singleton*, so
    /// `difference(ProtoObject, #foo)` stays a no-op rather than fabricating a
    /// `ProtoObject \ #foo` complement — `ProtoObject` is not itself a
    /// hierarchy entry above which singleton subtraction was specified. This
    /// is unrelated to the *nominal-excluded* flavour (ADR 0102 §5),
    /// which admits any nominal class as `base` when the excluded value is
    /// itself a nominal class, not a singleton.
    ///
    /// [`Negation`]: InferredType::Negation
    fn admits_symbol_singletons(ty: &Self, hierarchy: Option<&ClassHierarchy>) -> bool {
        match ty {
            Self::Known { class_name, .. } => {
                class_name == "Symbol"
                    || hierarchy.is_some_and(|h| h.is_subclass_of("Symbol", class_name))
            }
            _ => false,
        }
    }

    /// Builds a canonical [`Negation`](Self::Negation) — the sole construction
    /// path, so every `Negation` in the system carries a **canonically ordered**
    /// `excluded` set (ADR 0102 §1: members sorted ascending by `class_name`).
    ///
    /// Order-independent `PartialEq` already treats `Symbol \ (#a | #b)` and
    /// `Symbol \ (#b | #a)` as equal; canonical ordering additionally makes the
    /// stored form *deterministic* across serialisation round-trips and
    /// independent implementations, so `Union` dedup and diagnostic output are
    /// stable.
    fn make_negation(base: Self, excluded: Self, provenance: TypeProvenance) -> Self {
        Self::Negation {
            base: Box::new(base),
            excluded: Box::new(Self::canonical_excluded(excluded)),
            provenance,
        }
    }

    /// Sorts a `Negation`'s `excluded` union members ascending by `class_name`
    /// (ADR 0102 §1). A lone singleton (or any non-`Union`) is already canonical.
    fn canonical_excluded(excluded: Self) -> Self {
        match excluded {
            Self::Union {
                mut members,
                provenance,
            } => {
                members.sort_by(|x, y| Self::excluded_sort_key(x).cmp(&Self::excluded_sort_key(y)));
                Self::Union {
                    members,
                    provenance,
                }
            }
            other => other,
        }
    }

    /// Sort key for a canonical `excluded` member — the singleton's
    /// `class_name` (e.g. `#foo`). Non-`Known` members fall back to their
    /// display name so ordering is still total.
    fn excluded_sort_key(member: &Self) -> EcoString {
        match member {
            Self::Known { class_name, .. } => class_name.clone(),
            other => other.display_name(),
        }
    }

    /// Flattens `excluded` (a singleton or a normalised union of singletons)
    /// into its constituent members.
    fn excluded_members(excluded: &Self) -> Vec<Self> {
        match excluded {
            Self::Union { members, .. } => members.clone(),
            other => vec![other.clone()],
        }
    }

    /// Applies the full negation absorption-law set (ADR 0102 §1) in place.
    ///
    /// All laws below hold because a symbol singleton is a subtype of `Symbol`
    /// (the only supertype-of-singleton relationship modelled here); other
    /// members are left untouched.
    ///
    /// - **Bare-base subsumption:** `(Symbol \ E) | Symbol = Symbol` — a bare
    ///   `Symbol` swallows every symbol-based negation and singleton. This law
    ///   applies **even with no `Negation` present** (`#a | Symbol = Symbol`):
    ///   the singleton is a subtype of `Symbol`, so the collapsed form admits
    ///   exactly the same values. Gating it on negation presence gave the same
    ///   set two normal forms depending on evaluation order, breaking
    ///   `intersect` commutativity (a Windows CI counterexample surfaced this:
    ///   `intersect(#a | Symbol, (Symbol \ #a) | Object)` vs the swapped
    ///   order).
    /// - **Full / partial singleton absorption:** a bare singleton `#s` is
    ///   redundant beside a `Symbol`-based negation, so it is dropped, and when
    ///   it appears in the negation's `excluded` set it is added back by removing
    ///   it from `excluded` (`(Symbol \ (#a | #b)) | #a = Symbol \ #b`). When
    ///   `excluded` empties, the negation collapses to bare `Symbol`.
    /// - **Same-base complement union:** `(Symbol \ E1) | (Symbol \ E2) =
    ///   Symbol \ (E1 ∩ E2)`, so logically-equal negations never escape dedup and
    ///   unions do not grow across repeated narrowing/widening passes.
    fn absorb_negations(flat: &mut Vec<Self>) {
        let has_symbol_negation = flat
            .iter()
            .any(|m| matches!(m, Self::Negation { base, .. } if Self::is_symbol_base(base)));
        // Bare-singleton-under-bare-`Symbol` subsumption fires without any
        // negation too (see doc comment) — same pass, one code path.
        let has_bare_symbol_and_singleton = flat.iter().any(Self::is_symbol_base)
            && flat.iter().any(
                |m| matches!(m, Self::Known { class_name, .. } if Self::is_symbol_singleton(class_name)),
            );
        if !has_symbol_negation && !has_bare_symbol_and_singleton {
            return;
        }

        // Partition members into the symbol-ish parts (which collapse into a
        // single result) and everything else (untouched).
        let mut bare_symbol: Option<Self> = None;
        let mut neg_excludeds: Vec<Self> = Vec::new();
        let mut neg_provenance: Option<TypeProvenance> = None;
        let mut bare_singletons: Vec<EcoString> = Vec::new();
        let mut others: Vec<Self> = Vec::new();

        for m in flat.drain(..) {
            match m {
                Self::Negation {
                    base,
                    excluded,
                    provenance,
                } if Self::is_symbol_base(&base) => {
                    // The merged negation's `excluded` set is the union of
                    // every absorbed negation's own set, so it's structurally
                    // different from whichever single input this provenance
                    // was captured from — strip a stale `Aliased` tag for the
                    // same reason `difference`'s same-base flattening arm
                    // does (see `TypeProvenance::strip_alias`'s doc).
                    neg_provenance.get_or_insert(provenance.strip_alias());
                    neg_excludeds.push(*excluded);
                }
                Self::Known { ref class_name, .. } if class_name == "Symbol" => {
                    bare_symbol.get_or_insert(m);
                }
                Self::Known { ref class_name, .. } if Self::is_symbol_singleton(class_name) => {
                    bare_singletons.push(class_name.clone());
                }
                other => others.push(other),
            }
        }

        let provenance = neg_provenance.unwrap_or(TypeProvenance::Inferred(Span::default()));

        let symbol_part = if let Some(sym) = bare_symbol {
            // Bare-base subsumption: `Symbol` swallows the negations/singletons.
            sym
        } else {
            // Same-base complement union: merge every negation's `excluded` via
            // intersection (`E1 ∩ E2 ∩ …`).
            let mut merged = neg_excludeds[0].clone();
            for e in &neg_excludeds[1..] {
                // Structural singleton merge only — no hierarchy or protocol
                // registry needed (and neither is threaded through `union_of`).
                merged = Self::intersect(&merged, e, provenance.clone(), None, None);
            }
            // Add back any excluded singleton that also appears bare (absorption).
            let remaining: Vec<Self> = if matches!(merged, Self::Never) {
                Vec::new()
            } else {
                Self::excluded_members(&merged)
                    .into_iter()
                    .filter(|e| match e {
                        Self::Known { class_name, .. } => !bare_singletons.contains(class_name),
                        _ => true,
                    })
                    .collect()
            };
            if remaining.is_empty() {
                // `Symbol \ {} = Symbol`.
                Self::known("Symbol")
            } else {
                Self::make_negation(
                    Self::known("Symbol"),
                    Self::union_of(&remaining),
                    provenance,
                )
            }
        };

        // Reassemble (symbol part first), deduplicating the untouched members.
        let mut deduped: Vec<Self> = Vec::with_capacity(others.len() + 1);
        deduped.push(symbol_part);
        for m in others {
            if !deduped.contains(&m) {
                deduped.push(m);
            }
        }
        *flat = deduped;
    }

    /// `shape ∩ Known(class_name)` (ADR 0147): a map shape is a `Dictionary`,
    /// so a test against `Dictionary` or one of its superclasses keeps the
    /// shape. A protocol keeps the shape too, as the irreducible
    /// `shape & Protocol` (like class ∩ protocol); any other class is disjoint.
    fn intersect_shape_with_class(
        shape: &Self,
        class_ty: &Self,
        class_name: &str,
        provenance: TypeProvenance,
        hierarchy: Option<&ClassHierarchy>,
        protocol_registry: Option<&ProtocolRegistry>,
    ) -> Self {
        let is_dictionary = class_name == "Dictionary"
            || hierarchy.is_some_and(|h| h.is_subclass_of("Dictionary", class_name));
        if is_dictionary {
            shape.clone()
        } else if protocol_registry.is_some_and(|r| r.has_protocol(class_name)) {
            Self::normalize_intersection(vec![shape.clone(), class_ty.clone()], provenance)
        } else {
            Self::Never
        }
    }

    /// Normalising **intersection** `A ∩ B` (ADR 0102 §1).
    ///
    /// Symbol-singleton membership IS modelled — a singleton `#foo` is a subtype
    /// of `Symbol`, so `Symbol ∩ #foo = #foo`. The general **nominal-class base
    /// case** is also modelled *when a [`ClassHierarchy`] is supplied*
    /// (`hierarchy = Some(_)`): `intersect(A, B) = B` when `B <: A`
    /// (e.g. `Number ∩ Integer = Integer`), `= A` when `A <: B`, and `= Never`
    /// when `A` and `B` are hierarchy-unrelated (single inheritance ⇒ unrelated
    /// classes are disjoint, so `Integer ∩ String = Never`). Only nominal
    /// *difference* (`Object \ Number`) stays out of scope (see
    /// [`difference`](Self::difference)).
    ///
    /// The `hierarchy` argument is **optional** so the internal structural
    /// callers (`union_of`'s complement-merge in
    /// [`absorb_negations`](Self::absorb_negations)) can pass `None` — they only
    /// merge singletons, which never need the hierarchy. When `None`, two
    /// distinct non-symbol `Known` classes fall through to `Never` (the previous
    /// behaviour). Narrowing call sites pass `Some(hierarchy)` so class narrowing
    /// reduces correctly.
    ///
    /// The `protocol_registry` argument is likewise **optional**
    /// (ADR 0102 §1/§3): supplying it lets `intersect` recognise a
    /// protocol name and route class ∩ protocol / protocol ∩ protocol to the
    /// stored [`Intersection`](Self::Intersection) instead of falling through
    /// to the disjoint default. `None` preserves the plain structural
    /// behaviour (two distinct-named `Known` types with no proven relation
    /// are `Never`).
    ///
    /// Rules:
    /// - **Dynamic first:** `intersect(Dynamic, P) = P` (Dynamic acts as the top
    ///   type for intersection — *not* union's "Dynamic absorbs" rule). Checked
    ///   before identity/Object so `intersect(Dynamic, Object) = Object`.
    /// - `intersect(T, T) = T`; `intersect(T, Never) = Never`;
    ///   `intersect(T, Object) = T`.
    /// - **Intersect through a complement (both orders):**
    ///   `intersect(Negation{B, E}, P) = difference(intersect(B, P), E)` and
    ///   symmetrically `intersect(P, Negation{B, E}) =
    ///   difference(intersect(P, B), E)` — a `Negation` never reaches the
    ///   disjoint default (needed for chained narrowing / Phase 2 `\` syntax).
    /// - **Symbol-singleton membership:** `intersect(Symbol, #foo) = #foo`.
    /// - **Class ∩ protocol / protocol ∩ protocol** (`protocol_registry =
    ///   Some`, ADR 0102 §1/§3): when the two distinct-named `Known` operands
    ///   include at least one protocol name, the result is the normalised,
    ///   irreducible [`Intersection`](Self::Intersection) — e.g.
    ///   `Collection(Object) ∩ Comparable`. Checked before the nominal-class
    ///   base case because a protocol is never an entry in the class
    ///   hierarchy's subtype lattice, and protocols/classes share one
    ///   namespace (collisions are compile errors), so `has_protocol(name)`
    ///   alone identifies a protocol without consulting the hierarchy.
    /// - **Nominal-class base case** (`hierarchy = Some`): `B <: A ⇒ B`,
    ///   `A <: B ⇒ A`, unrelated ⇒ `Never`.
    /// - **LHS-union distribution:** `intersect(T1 | … | Tn, P) =
    ///   (T1 ∩ P) | … | (Tn ∩ P)`, e.g. `intersect(Integer | #infinity,
    ///   #infinity)` normalises to the bare singleton `#infinity`.
    /// - **RHS-union fold:** `intersect(T, A | B) = (T ∩ A) | (T ∩ B)`.
    /// - Generics compare exactly, including `type_args`.
    ///
    /// The result is re-normalised through [`union_of`](Self::union_of).
    #[allow(clippy::too_many_lines)] // one arm per type-pair rule; splitting hides the ordering
    pub(crate) fn intersect(
        a: &Self,
        b: &Self,
        provenance: TypeProvenance,
        hierarchy: Option<&ClassHierarchy>,
        protocol_registry: Option<&ProtocolRegistry>,
    ) -> Self {
        match (a, b) {
            // Dynamic FIRST (before identity/Object): `intersect(Dynamic, P) = P`
            // refines the unknown, so `intersect(Dynamic, Object) = Object`.
            (Self::Dynamic(_), _) => b.clone(),
            (_, Self::Dynamic(_)) => a.clone(),
            // Never annihilates.
            (Self::Never, _) | (_, Self::Never) => Self::Never,
            // Object (top) is the identity: `T ∩ Object = T`.
            (_, other) if Self::is_object(other) => a.clone(),
            (other, _) if Self::is_object(other) => b.clone(),
            // Intersect through a complement (both orders): `(B \ E) ∩ P =
            // (B ∩ P) \ E`. A `Negation` must never fall through to the disjoint
            // default (the `(Known, Negation)` case fires in Phase 2's `\`
            // syntax; missing it silently loses narrowing).
            (Self::Negation { base, excluded, .. }, _) => Self::difference(
                &Self::intersect(base, b, provenance.clone(), hierarchy, protocol_registry),
                excluded,
                provenance,
                hierarchy,
            ),
            (_, Self::Negation { base, excluded, .. }) => Self::difference(
                &Self::intersect(a, base, provenance.clone(), hierarchy, protocol_registry),
                excluded,
                provenance,
                hierarchy,
            ),
            // LHS-union distribution.
            (Self::Union { members, .. }, _) => {
                let parts: Vec<Self> = members
                    .iter()
                    .map(|m| {
                        Self::intersect(m, b, provenance.clone(), hierarchy, protocol_registry)
                    })
                    .collect();
                Self::union_of(&parts)
            }
            // RHS-union fold.
            (_, Self::Union { members, .. }) => {
                let parts: Vec<Self> = members
                    .iter()
                    .map(|m| {
                        Self::intersect(a, m, provenance.clone(), hierarchy, protocol_registry)
                    })
                    .collect();
                Self::union_of(&parts)
            }
            // Intersect through an existing stored Intersection (both
            // orders) — flatten rather than falling through to the disjoint
            // default (ADR 0102 §1/§3). Needed so a 3+-way `&`
            // chain (`A & B & C`, parsed left-associatively as `(A & B) &
            // C`) reduces through repeated pairwise `intersect` calls to a
            // single flat, deduplicated `Intersection` instead of silently
            // losing a member. Checked after the `Union` arms above so
            // `intersect(Intersection{..}, T1 | T2)` still distributes over
            // the union first — by this point neither operand is a `Union`
            // (those arms already matched), `Dynamic`/`Never`/`Object`
            // (matched even earlier), or a `Negation` (matched above too).
            (Self::Intersection { members, .. }, _) => {
                // Re-check pairwise disjointness before appending: without
                // this, `Printable & Integer & String` would flatten to
                // `Intersection{Integer, Printable, String}` instead of
                // reducing to `Never` (`Integer ∩ String = Never`), violating
                // the stored-intersection invariant.
                for m in members {
                    let pair =
                        Self::intersect(m, b, provenance.clone(), hierarchy, protocol_registry);
                    if matches!(pair, Self::Never) {
                        return Self::Never;
                    }
                }
                let mut merged = members.clone();
                merged.push(b.clone());
                Self::normalize_intersection(merged, provenance)
            }
            (_, Self::Intersection { members, .. }) => {
                for m in members {
                    let pair =
                        Self::intersect(a, m, provenance.clone(), hierarchy, protocol_registry);
                    if matches!(pair, Self::Never) {
                        return Self::Never;
                    }
                }
                let mut merged = members.clone();
                merged.push(a.clone());
                Self::normalize_intersection(merged, provenance)
            }
            // ADR 0147: a map shape is a `Dictionary`, so an `isKindOf:` /
            // type-pattern test against `Dictionary` (or one of its
            // superclasses) keeps the shape, a protocol yields `shape & P`,
            // and any other class is disjoint.
            (Self::Shape { .. }, Self::Known { class_name, .. }) => {
                Self::intersect_shape_with_class(
                    a,
                    b,
                    class_name,
                    provenance,
                    hierarchy,
                    protocol_registry,
                )
            }
            (Self::Known { class_name, .. }, Self::Shape { .. }) => {
                Self::intersect_shape_with_class(
                    b,
                    a,
                    class_name,
                    provenance,
                    hierarchy,
                    protocol_registry,
                )
            }
            // Symbol-singleton membership: `#foo <: Symbol`, so the narrower
            // singleton is kept in both orders. Checked before the general
            // nominal case because singletons are not entries in the hierarchy
            // — the nominal arm's `h.is_subclass_of("#foo", …)` would
            // always answer `false`. With a hierarchy, the same rule extends
            // to every nominal *supertype* of `Symbol` (e.g. `ProtoObject`):
            // `ProtoObject ∩ #foo = #foo`, matching the
            // pre-ADR-0102 hierarchy walk. (`Object` never reaches here — the
            // top-identity arms above already handled it.)
            (Self::Known { .. }, Self::Known { class_name: s, .. })
                if Self::admits_symbol_singletons(a, hierarchy) && Self::is_symbol_singleton(s) =>
            {
                b.clone()
            }
            (Self::Known { class_name: s, .. }, Self::Known { .. })
                if Self::admits_symbol_singletons(b, hierarchy) && Self::is_symbol_singleton(s) =>
            {
                a.clone()
            }
            // `T ∩ T = T` (exact structural equality, generics included).
            _ if a == b => a.clone(),
            // A bare generic class name denotes every instantiation of the
            // class (a type pattern or `isKindOf:` cannot name type args), so
            // it is the identity on any instantiation of the same class:
            // `Result(String, Error) ∩ Result = Result(String, Error)`, in
            // both orders (BT-4510: the `r :: Result ->` arm of a match on a
            // `Result(String, Error)` was typed `Never`).
            _ if Self::bare_instantiation_of(b, a) => a.clone(),
            _ if Self::bare_instantiation_of(a, b) => b.clone(),
            // Class ∩ protocol / protocol ∩ protocol (ADR 0102 §1/§3):
            // the irreducible intersection. Must be checked before the nominal
            // arm below — a protocol name is never a hierarchy entry, so
            // without this arm it would silently fall through to `Never`.
            (Self::Known { class_name: an, .. }, Self::Known { class_name: bn, .. })
                if an != bn
                    && protocol_registry
                        .is_some_and(|r| r.has_protocol(an) || r.has_protocol(bn)) =>
            {
                Self::normalize_intersection(vec![a.clone(), b.clone()], provenance)
            }
            // Nominal-class base case (ADR 0102 §1): with a hierarchy, two
            // distinct-named `Known` classes relate via subtyping — the subclass
            // wins (`Number ∩ Integer = Integer`), and hierarchy-unrelated
            // classes are disjoint (`Integer ∩ String = Never`). Without a
            // hierarchy, or when the names match but generics differ (invariant
            // args ⇒ disjoint), fall through to `Never` — the previous
            // structural-only behaviour.
            (Self::Known { class_name: an, .. }, Self::Known { class_name: bn, .. }) => {
                match hierarchy {
                    Some(h) if an != bn => {
                        if h.is_subclass_of(bn, an) {
                            b.clone()
                        } else if h.is_subclass_of(an, bn) {
                            a.clone()
                        } else {
                            Self::Never
                        }
                    }
                    _ => Self::Never,
                }
            }
            // ADR 0144: `P class & Q class` (or `C class & P class`) is the
            // class object of a class conforming to both — irreducible, like
            // protocol ∩ protocol above, never disjoint.
            (Self::ProtocolMeta { .. }, Self::ProtocolMeta { .. } | Self::Meta { .. })
            | (Self::Meta { .. }, Self::ProtocolMeta { .. }) => {
                Self::normalize_intersection(vec![a.clone(), b.clone()], provenance)
            }
            // Distinct concrete types with no structural relationship (including
            // distinct singletons, `#foo ∩ #bar`) are disjoint.
            _ => Self::Never,
        }
    }

    /// Builds a normalised [`Intersection`](Self::Intersection) — the sole
    /// construction path (ADR 0102 §1), analogous to
    /// [`make_negation`](Self::make_negation).
    ///
    /// - **Flattens** nested `Intersection` members into the parent (an
    ///   `Intersection` never contains another `Intersection`).
    /// - **Deduplicates** by structural equality.
    /// - **Degenerate case:** if flattening/dedup leaves a single member, it
    ///   is returned bare rather than wrapped (mirrors `union_of`'s
    ///   single-member collapse).
    /// - Sorted by display name so independent constructions of the same
    ///   logical intersection produce the same stored order (equality is
    ///   already order-independent; this makes serialisation/output
    ///   deterministic too, mirroring `Negation`'s canonical `excluded`
    ///   ordering).
    fn normalize_intersection(raw: Vec<Self>, provenance: TypeProvenance) -> Self {
        let mut flat: Vec<Self> = Vec::with_capacity(raw.len());
        for m in raw {
            match m {
                Self::Intersection { members, .. } => {
                    for inner in members {
                        if !flat.contains(&inner) {
                            flat.push(inner);
                        }
                    }
                }
                other => {
                    if !flat.contains(&other) {
                        flat.push(other);
                    }
                }
            }
        }
        // A bare generic member is subsumed by an instantiation of the same
        // class (`Result & Result(String, Error)` is `Result(String,
        // Error)`), so it is dropped rather than kept beside it (BT-4510).
        let snapshot = flat.clone();
        flat.retain(|m| !snapshot.iter().any(|o| Self::bare_instantiation_of(m, o)));
        flat.sort_by_key(Self::display_name);
        match flat.len() {
            // Unreachable from `intersect`'s call site (which always passes two
            // distinct members), kept for a total/defensive standalone helper.
            0 => Self::Never,
            1 => flat.into_iter().next().unwrap_or(Self::Never),
            _ => Self::Intersection {
                members: flat,
                provenance,
            },
        }
    }

    /// Normalising **difference** `A \ B` (ADR 0102 §1, §5).
    ///
    /// Rules:
    /// - Boundary: `difference(Never, P) = Never`; `difference(T, Never) = T`.
    /// - **Dynamic asymmetry:** `difference(Dynamic, P) = Dynamic` and
    ///   `difference(T, Dynamic) = T` (Dynamic is opaque — *not* union's
    ///   "Dynamic absorbs" rule).
    /// - **RHS-union fold:** `difference(T, A | B) =
    ///   difference(difference(T, A), B)`.
    /// - **LHS-union distribution:** `difference(T1 | … | Tn, P) =
    ///   (T1 \ P) | … | (Tn \ P)`, which drops any member exactly equal to `P`.
    /// - `difference(Symbol, #foo) = Negation{Symbol, #foo}`; the removed set is
    ///   an [`InferredType`] (a singleton or normalised union of singletons),
    ///   so `difference(Symbol, #a | #b) = Negation{Symbol, #a | #b}`.
    /// - **Same-base flattening:** `difference(Negation{Symbol, E}, #bar) =
    ///   Negation{Symbol, union_of(E, #bar)}` — nested negation never escapes
    ///   normal form.
    /// - **Nominal-class base case** (ADR 0102 §5, `hierarchy = Some`):
    ///   for two distinct-named, non-singleton nominal classes, `difference(A,
    ///   B)` ⇒ `Negation{A, B}` when `B` is a strict subclass of `A` (the
    ///   class-hierarchy analogue of `Symbol \ #foo`); ⇒ `Never` when `A <: B`
    ///   (every instance of `A` is also a `B`); ⇒ `A` unchanged when
    ///   hierarchy-unrelated (nothing to remove). Without a hierarchy, or when
    ///   either operand is a singleton (never a hierarchy entry — that is the
    ///   dedicated `Symbol \ #foo` arm above), falls through to the structural
    ///   default. `excluded` stays a single class (never a union of classes —
    ///   out of scope per ADR 0102 §5 scope note).
    /// - `Meta` and `Dynamic` are opaque to `Negation`.
    /// - Generics compare exactly, including `type_args`.
    pub(crate) fn difference(
        a: &Self,
        b: &Self,
        provenance: TypeProvenance,
        hierarchy: Option<&ClassHierarchy>,
    ) -> Self {
        match (a, b) {
            // `T \ Dynamic = T` and `T \ Never = T` (subtracting these removes
            // nothing), plus `Dynamic \ P = Dynamic` — here `a` *is* `Dynamic`,
            // so `a.clone()` is correct. That last case is the asymmetry vs
            // union's "Dynamic absorbs" rule (ADR 0102 §1).
            (Self::Dynamic(_), _) | (_, Self::Dynamic(_) | Self::Never) => a.clone(),
            // Boundary: `Never \ P = Never`.
            (Self::Never, _) => Self::Never,
            // RHS-union fold: subtract each removed member in turn.
            (_, Self::Union { members, .. }) => members.iter().fold(a.clone(), |acc, m| {
                Self::difference(&acc, m, provenance.clone(), hierarchy)
            }),
            // LHS-union distribution: `(A | B) \ P = (A \ P) | (B \ P)`.
            (Self::Union { members, .. }, _) => {
                let parts: Vec<Self> = members
                    .iter()
                    .map(|m| Self::difference(m, b, provenance.clone(), hierarchy))
                    .collect();
                Self::union_of(&parts)
            }
            // Same-base flattening: fold the newly removed singleton into the
            // existing negation. Only well-formed `Symbol`-based negations and
            // singleton subtractions apply.
            (
                Self::Negation {
                    base,
                    excluded,
                    provenance: neg_prov,
                },
                Self::Known { class_name, .. },
            ) if Self::is_symbol_base(base) && Self::is_symbol_singleton(class_name) => {
                Self::make_negation(
                    (**base).clone(),
                    Self::union_of(&[(**excluded).clone(), b.clone()]),
                    neg_prov.clone().strip_alias(),
                )
            }
            // `Symbol \ #foo = Negation{Symbol, #foo}`.
            (
                Self::Known { class_name, .. },
                Self::Known {
                    class_name: sym, ..
                },
            ) if class_name == "Symbol" && Self::is_symbol_singleton(sym) => {
                Self::make_negation(a.clone(), b.clone(), provenance)
            }
            // `T \ T = Never` (exact structural equality, generics included).
            _ if a == b => Self::Never,
            // Subtracting a bare generic class name removes every
            // instantiation of it: `Result(String, Error) \ Result = Never`
            // (the residual after an unguarded `r :: Result ->` arm, BT-4510).
            // The converse (`Result \ Result(String, Error)`) is not
            // representable and falls through to the no-op below.
            _ if Self::bare_instantiation_of(b, a) => Self::Never,
            // Nominal-class base case (ADR 0102 §5): with a hierarchy,
            // two distinct-named, non-singleton nominal classes relate via
            // subtyping — a proper subclass subtraction is irreducible
            // (`Negation{A, B}`), a supertype/self subtraction removes
            // everything (`Never`, though `A == B` is already caught above),
            // and hierarchy-unrelated classes are disjoint (no-op). Singletons
            // are never hierarchy entries, so they are explicitly excluded —
            // they are handled by the dedicated `Symbol \ #foo` arm above.
            (Self::Known { class_name: an, .. }, Self::Known { class_name: bn, .. })
                if an != bn
                    && !Self::is_symbol_singleton(an)
                    && !Self::is_symbol_singleton(bn)
                    && hierarchy.is_some() =>
            {
                let h = hierarchy.unwrap_or_else(|| unreachable!("checked by guard above"));
                if h.is_subclass_of(bn, an) {
                    // `B <: A` (proper, since `an != bn`).
                    Self::make_negation(a.clone(), b.clone(), provenance)
                } else if h.is_subclass_of(an, bn) {
                    // `A <: B` — every instance of `A` is also a `B`.
                    Self::Never
                } else {
                    // Hierarchy-unrelated — disjoint, nothing to remove.
                    a.clone()
                }
            }
            // Nothing structurally removable — no-op (no hierarchy supplied,
            // or hierarchy-unrelated nominal classes; `Meta`/`Dynamic` are
            // opaque to `Negation`).
            _ => a.clone(),
        }
    }

    /// Whether `bare` is a generic class named without type arguments and
    /// `other` is a parameterized instantiation of the same class
    /// (`Result` and `Result(String, Error)`): the bare name admits every
    /// instantiation, so it is a supertype of `other` (BT-4510).
    fn bare_instantiation_of(bare: &Self, other: &Self) -> bool {
        matches!(
            (bare, other),
            (
                Self::Known { class_name: bn, type_args: bargs, .. },
                Self::Known { class_name: on, type_args: oargs, .. },
            ) if bn == on && bargs.is_empty() && !oargs.is_empty()
        )
    }

    /// Returns `true` if `ty` is the `Object` root class — the top type for
    /// [`intersect`](Self::intersect) (`T ∩ Object = T`).
    fn is_object(ty: &Self) -> bool {
        matches!(ty, Self::Known { class_name, type_args, .. }
            if class_name == "Object" && type_args.is_empty())
    }
}
