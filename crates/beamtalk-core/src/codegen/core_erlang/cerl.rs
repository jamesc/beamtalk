// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Minimal Core Erlang AST mirror for ADR 0088 Phase 0b (BT-2315).
//!
//! This module defines the smallest cerl node set needed to express a
//! self-identifying Core Erlang module — one that compiles, loads, and
//! responds to `module_info/0,1` so the napkin's end-to-end assertion
//! `bt_napkin:module_info(module) =:= bt_napkin` can fire. The wire bytes
//! match `term_to_binary/1` applied to the equivalent term built with the
//! OTP `cerl` module's constructors.
//!
//! # Scope
//!
//! Phase 0b is a wire-mechanism napkin (ADR 0088). It is **not** the full
//! Phase 1 mirror. The acceptance criterion calls out the minimum set as
//! "`c_module`, `c_var`, `c_literal`, annotation wrapper. NOT the full
//! Phase 1 mirror" — and that's the spine of this file. We also include
//! `c_fun` and `c_call` because they are necessary to express
//! `module_info/0` and `module_info/1` as straight delegations to
//! `erlang:get_module_info/{1,2}`. Without those two extra nodes a
//! `compile:forms/2` result is loadable but the AC's `module_info` probe
//! fails (`from_core` does not auto-inject `module_info` the way `from_erl`
//! does).
//!
//! Concretely, this file provides:
//!
//! * [`Anno`] — the annotation list attached to every cerl record. Phase 0b
//!   only emits empty annotations; the type exists so Phase 1 can carry
//!   `{file, line, column}` etc. without breaking the wire shape.
//! * [`CLiteral`] — `{c_literal, Anno, Value}`. Phase 0b carries only the
//!   atom payload kind; Phase 1 will extend [`LiteralValue`] with integer,
//!   float, list, tuple, map, and binary variants.
//! * [`CVar`] — `{c_var, Anno, Name}`. The `Name` field is either a bare
//!   atom (ordinary variable reference) or a `{Name, Arity}` tuple
//!   (function-name reference, used by export lists and function-def keys).
//! * [`CFun`] — `{c_fun, Anno, Vars, Body}`. Minimal-shape fun expression
//!   used to wrap each function body.
//! * [`CCall`] — `{c_call, Anno, Module, Name, Args}`. Remote function call,
//!   used by `module_info/0,1` to delegate to `erlang:get_module_info/{1,2}`.
//! * [`CModule`] — `{c_module, Anno, Name, Exports, Attrs, Defs}`.
//!
//! Phase 1 (ADR 0088) will extend this with `c_let`, `c_letrec`, `c_case`,
//! `c_clause`, `c_apply`, `c_primop`, `c_try`, `c_catch`, `c_receive`,
//! `c_binary`, `c_cons`, `c_tuple`, `c_map`, and the full literal payload
//! coverage. Until then, this mirror is intentionally narrow.
//!
//! # Wire shape
//!
//! Each cerl record is a tuple whose first element is the record tag atom
//! (`c_module`, `c_literal`, `c_var`, `c_fun`, `c_call`) and whose second
//! element is the annotation list. The remaining fields differ per record.
//! The byte layout this module emits matches `term_to_binary/1` applied to
//! the equivalent term built with the OTP `cerl` module's constructors —
//! see `truly_empty_module_matches_otp_term_to_binary` and
//! `minimum_module_matches_otp_term_to_binary` in the test module below.
//!
//! # References
//!
//! * ADR 0088: `docs/ADR/0088-direct-cerl-emission.md` ("Phase 0b: Napkin").
//! * Erlang cerl module: <https://www.erlang.org/doc/man/cerl.html>
//! * Phase 0a audit: `docs/ADR/0088-phase-0a-audit.md`.

use eetf::{Atom, FixInteger, List, Term, Tuple};

/// Annotation list attached to every cerl record.
///
/// Phase 0b emits only empty annotations. Phase 1 will carry source-position
/// tuples like `{file, "foo.bt"}` and `{line, 42}`. The annotation list is
/// already part of the record shape so adding entries in Phase 1 does not
/// require a wire-format break.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Anno {
    entries: Vec<Term>,
}

impl Anno {
    /// Construct an empty annotation list (`[]` in Erlang).
    #[must_use]
    pub fn empty() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    /// Render to an ETF list term.
    fn to_term(&self) -> Term {
        Term::from(List::from(self.entries.clone()))
    }
}

/// Atomic-literal payload kinds carried by [`CLiteral`].
///
/// Phase 0b only needs the atom case (module names, function-name parts of
/// calls). Phase 1 will extend this with integer, float, string/binary,
/// list, and tuple payloads — each adds a variant here and a match arm in
/// [`CLiteral::to_term`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralValue {
    /// An atom literal.
    Atom(String),
}

impl LiteralValue {
    fn to_term(&self) -> Term {
        match self {
            Self::Atom(name) => Term::from(Atom::from(name.as_str())),
        }
    }
}

/// `{c_literal, Anno, Value}` — a Core Erlang literal expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CLiteral {
    /// The annotation list (`[]` in Phase 0b).
    pub anno: Anno,
    /// The literal payload.
    pub value: LiteralValue,
}

impl CLiteral {
    /// Build an atom literal — convenience for the common case.
    #[must_use]
    pub fn atom(name: impl Into<String>) -> Self {
        Self {
            anno: Anno::empty(),
            value: LiteralValue::Atom(name.into()),
        }
    }

    /// Render to the ETF term `{c_literal, Anno, Value}`.
    #[must_use]
    pub fn to_term(&self) -> Term {
        Term::from(Tuple::from(vec![
            Term::from(Atom::from("c_literal")),
            self.anno.to_term(),
            self.value.to_term(),
        ]))
    }
}

/// The shape of a [`CVar`]'s `Name` slot.
///
/// OTP's `cerl` module uses an atom for ordinary variable references and a
/// `{Name, Arity}` tuple for function-name references. Both forms appear in
/// the Phase 0b napkin: ordinary `'X'` variables in the body of
/// `module_info/1`, and `{module_info, 0}` / `{module_info, 1}` function
/// names in the export list and the function-def keys.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VarName {
    /// Ordinary variable reference, e.g. `'X'`.
    Atom(String),
    /// Function-name reference: `{Name, Arity}`.
    FunRef {
        /// The function-name atom.
        name: String,
        /// The arity.
        arity: u32,
    },
}

impl VarName {
    fn to_term(&self) -> Term {
        match self {
            Self::Atom(name) => Term::from(Atom::from(name.as_str())),
            Self::FunRef { name, arity } => Term::from(Tuple::from(vec![
                Term::from(Atom::from(name.as_str())),
                Term::from(FixInteger::from(i32::try_from(*arity).unwrap_or(0))),
            ])),
        }
    }
}

/// `{c_var, Anno, Name}` — a Core Erlang variable or function-name reference.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CVar {
    /// The annotation list (`[]` in Phase 0b).
    pub anno: Anno,
    /// The variable name (atom) or function reference (`{Name, Arity}`).
    pub name: VarName,
}

impl CVar {
    /// Build an ordinary variable reference (`{c_var, [], 'AtomName'}`).
    #[must_use]
    pub fn var(name: impl Into<String>) -> Self {
        Self {
            anno: Anno::empty(),
            name: VarName::Atom(name.into()),
        }
    }

    /// Build a function-name reference (`{c_var, [], {name, arity}}`).
    #[must_use]
    pub fn fun_ref(name: impl Into<String>, arity: u32) -> Self {
        Self {
            anno: Anno::empty(),
            name: VarName::FunRef {
                name: name.into(),
                arity,
            },
        }
    }

    /// Render to the ETF term `{c_var, Anno, Name}`.
    #[must_use]
    pub fn to_term(&self) -> Term {
        Term::from(Tuple::from(vec![
            Term::from(Atom::from("c_var")),
            self.anno.to_term(),
            self.name.to_term(),
        ]))
    }
}

/// A Core Erlang expression — Phase 0b subset.
///
/// Only the variants needed by the napkin's `module_info/0,1` delegations
/// are populated. Phase 1 will extend this with the remaining cerl forms.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    /// A literal expression — `{c_literal, ...}`.
    Literal(CLiteral),
    /// A variable reference — `{c_var, ...}`.
    Var(CVar),
    /// A remote function call — `{c_call, ...}`.
    Call(Box<CCall>),
    /// A fun expression — `{c_fun, ...}`.
    Fun(Box<CFun>),
}

impl Expr {
    /// Render to the corresponding ETF term.
    #[must_use]
    pub fn to_term(&self) -> Term {
        match self {
            Self::Literal(lit) => lit.to_term(),
            Self::Var(v) => v.to_term(),
            Self::Call(c) => c.to_term(),
            Self::Fun(f) => f.to_term(),
        }
    }
}

/// `{c_call, Anno, Module, Name, Args}` — a remote function call.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CCall {
    /// The annotation list (`[]` in Phase 0b).
    pub anno: Anno,
    /// The module expression (typically a literal atom).
    pub module: Expr,
    /// The function-name expression (typically a literal atom).
    pub name: Expr,
    /// The argument expressions in left-to-right order.
    pub args: Vec<Expr>,
}

impl CCall {
    /// Render to the ETF term `{c_call, Anno, Module, Name, Args}`.
    #[must_use]
    pub fn to_term(&self) -> Term {
        let args = Term::from(List::from(
            self.args.iter().map(Expr::to_term).collect::<Vec<_>>(),
        ));
        Term::from(Tuple::from(vec![
            Term::from(Atom::from("c_call")),
            self.anno.to_term(),
            self.module.to_term(),
            self.name.to_term(),
            args,
        ]))
    }
}

/// `{c_fun, Anno, Vars, Body}` — a fun expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CFun {
    /// The annotation list (`[]` in Phase 0b).
    pub anno: Anno,
    /// The parameter variables.
    pub vars: Vec<CVar>,
    /// The function body.
    pub body: Expr,
}

impl CFun {
    /// Render to the ETF term `{c_fun, Anno, Vars, Body}`.
    #[must_use]
    pub fn to_term(&self) -> Term {
        let vars = Term::from(List::from(
            self.vars.iter().map(CVar::to_term).collect::<Vec<_>>(),
        ));
        Term::from(Tuple::from(vec![
            Term::from(Atom::from("c_fun")),
            self.anno.to_term(),
            vars,
            self.body.to_term(),
        ]))
    }
}

/// `{c_module, Anno, Name, Exports, Attrs, Defs}` — a Core Erlang module.
///
/// Phase 0b emits modules that contain only `module_info/0,1` — enough to
/// satisfy the napkin's end-to-end assertion but no user-defined functions.
/// The fields exist as separate Rust types so Phase 1 can extend each
/// without reshaping the top-level record.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CModule {
    /// The annotation list (`[]` in Phase 0b).
    pub anno: Anno,
    /// The module-name literal (an atom).
    pub name: CLiteral,
    /// Exported `{Name, Arity}` references.
    pub exports: Vec<CVar>,
    /// Module attribute `{Key, Value}` literal pairs.
    pub attrs: Vec<(CLiteral, CLiteral)>,
    /// Function definitions: `{Var, Fun}` pairs.
    pub defs: Vec<(CVar, CFun)>,
}

impl CModule {
    /// Build a `bt_napkin`-style minimum module: only `module_info/0,1`,
    /// delegating to `erlang:get_module_info/{1,2}`.
    ///
    /// Anything narrower than this fails the napkin's end-to-end assertion
    /// (`module_info(module) =:= Name`), because `compile:forms/2` with
    /// `from_core` does not auto-inject `module_info` the way `from_erl`
    /// does. See the ADR 0088 Phase 0b memo for details.
    #[must_use]
    pub fn minimum(name: impl Into<String>) -> Self {
        let name = name.into();
        let name_atom = CLiteral::atom(name.clone());
        let erlang_lit = Expr::Literal(CLiteral::atom("erlang"));
        let get_mod_info_lit = Expr::Literal(CLiteral::atom("get_module_info"));
        let mod_name_lit = Expr::Literal(name_atom.clone());

        // module_info/0 = fun () -> erlang:get_module_info(ModName)
        let mi0_var = CVar::fun_ref("module_info", 0);
        let mi0_fun = CFun {
            anno: Anno::empty(),
            vars: Vec::new(),
            body: Expr::Call(Box::new(CCall {
                anno: Anno::empty(),
                module: erlang_lit.clone(),
                name: get_mod_info_lit.clone(),
                args: vec![mod_name_lit.clone()],
            })),
        };

        // module_info/1 = fun (X) -> erlang:get_module_info(ModName, X)
        let x_var = CVar::var("X");
        let mi1_var = CVar::fun_ref("module_info", 1);
        let mi1_fun = CFun {
            anno: Anno::empty(),
            vars: vec![x_var.clone()],
            body: Expr::Call(Box::new(CCall {
                anno: Anno::empty(),
                module: erlang_lit,
                name: get_mod_info_lit,
                args: vec![mod_name_lit, Expr::Var(x_var)],
            })),
        };

        Self {
            anno: Anno::empty(),
            name: name_atom,
            exports: vec![mi0_var.clone(), mi1_var.clone()],
            attrs: Vec::new(),
            defs: vec![(mi0_var, mi0_fun), (mi1_var, mi1_fun)],
        }
    }

    /// Build a synthetic many-function module for the Phase 0b timing
    /// harness: `n` exported nullary functions, each returning a fresh atom
    /// literal `f0_ok`, `f1_ok`, ... .
    ///
    /// Used by `examples/cerl_napkin_timing.rs` to produce a "hand-constructed
    /// cerl tree with ~thousands of nodes" fixture per the AC. The exact node
    /// count is `2 + n * 6` (six nodes per function: export `c_var`, def
    /// `c_var` + `c_fun` + body literal + body `c_literal` value wrapper +
    /// module-name `c_literal`). For `n = 250` the tree has ~1500 nodes.
    #[must_use]
    pub fn multi_function(name: impl Into<String>, n: usize) -> Self {
        let name = name.into();
        let name_atom = CLiteral::atom(name.clone());
        let mut exports = Vec::with_capacity(n);
        let mut defs = Vec::with_capacity(n);
        for i in 0..n {
            let fn_name = format!("f{i}");
            let body_atom = format!("f{i}_ok");
            let var = CVar::fun_ref(&fn_name, 0);
            let body = Expr::Literal(CLiteral::atom(body_atom));
            let func = CFun {
                anno: Anno::empty(),
                vars: Vec::new(),
                body,
            };
            exports.push(var.clone());
            defs.push((var, func));
        }
        // module_info/0, /1 so the resulting module is loadable.
        let mut minimum = Self::minimum(name.clone());
        // Splice synthetic functions in *before* the module_info pair so
        // export and def lists stay in registration order.
        exports.append(&mut minimum.exports);
        defs.append(&mut minimum.defs);
        Self {
            anno: Anno::empty(),
            name: name_atom,
            exports,
            attrs: Vec::new(),
            defs,
        }
    }

    /// Render to the ETF term `{c_module, Anno, Name, Exports, Attrs, Defs}`.
    #[must_use]
    pub fn to_term(&self) -> Term {
        let exports = Term::from(List::from(
            self.exports.iter().map(CVar::to_term).collect::<Vec<_>>(),
        ));
        let attrs = Term::from(List::from(
            self.attrs
                .iter()
                .map(|(k, v)| Term::from(Tuple::from(vec![k.to_term(), v.to_term()])))
                .collect::<Vec<_>>(),
        ));
        let defs = Term::from(List::from(
            self.defs
                .iter()
                .map(|(var, body)| Term::from(Tuple::from(vec![var.to_term(), body.to_term()])))
                .collect::<Vec<_>>(),
        ));
        Term::from(Tuple::from(vec![
            Term::from(Atom::from("c_module")),
            self.anno.to_term(),
            self.name.to_term(),
            exports,
            attrs,
            defs,
        ]))
    }

    /// Encode the module as ETF bytes, ready to ship over the OTP Port.
    ///
    /// The output matches `term_to_binary/1` applied to the equivalent term
    /// built with the OTP `cerl` module's constructors, byte for byte.
    ///
    /// # Errors
    ///
    /// Returns the underlying `eetf` encode error if writing to the buffer
    /// fails. In practice that only occurs on a write-to-a-truly-broken
    /// destination; the in-memory `Vec<u8>` path used by all current callers
    /// is infallible.
    pub fn encode_etf(&self) -> std::io::Result<Vec<u8>> {
        let mut buf = Vec::new();
        self.to_term()
            .encode(&mut buf)
            .map_err(|err| std::io::Error::new(std::io::ErrorKind::InvalidData, err.to_string()))?;
        Ok(buf)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Reference bytes captured from
    /// `term_to_binary(cerl:c_module(cerl:c_atom(bt_napkin), [], [], []))`
    /// on the Erlang side (OTP 27). This is the truly-empty (no-defs)
    /// module shape — a *subset* of what `CModule::minimum` builds. The
    /// `to_term` machinery is exercised here to assert byte equality at
    /// the smallest possible scope (no `c_fun`/`c_call` in play); the
    /// minimum-module byte-equivalence is asserted by
    /// `minimum_module_matches_otp_term_to_binary` below against the
    /// reference `MINIMUM_BT_NAPKIN_ETF` constant.
    const EMPTY_BT_NAPKIN_ETF: &[u8] = &[
        131, // VERSION_MAGIC
        104, 6, // SMALL_TUPLE_EXT arity 6
        119, 8, // SMALL_ATOM_UTF8_EXT len 8
        b'c', b'_', b'm', b'o', b'd', b'u', b'l', b'e', 106, // NIL_EXT (Anno)
        104, 3, // SMALL_TUPLE_EXT arity 3 — c_literal
        119, 9, // SMALL_ATOM_UTF8_EXT len 9
        b'c', b'_', b'l', b'i', b't', b'e', b'r', b'a', b'l', 106, // NIL_EXT (c_literal Anno)
        119, 9, // SMALL_ATOM_UTF8_EXT len 9
        b'b', b't', b'_', b'n', b'a', b'p', b'k', b'i', b'n', 106, // NIL_EXT (Exports)
        106, // NIL_EXT (Attrs)
        106, // NIL_EXT (Defs)
    ];

    /// Build the truly-empty (no defs/exports) module shape directly,
    /// bypassing `CModule::minimum`, so we can byte-diff against the
    /// reference ETF.
    fn truly_empty(name: &str) -> CModule {
        CModule {
            anno: Anno::empty(),
            name: CLiteral::atom(name),
            exports: Vec::new(),
            attrs: Vec::new(),
            defs: Vec::new(),
        }
    }

    #[test]
    fn truly_empty_module_matches_otp_term_to_binary() {
        let module = truly_empty("bt_napkin");
        let bytes = module.encode_etf().expect("encode failed");
        assert_eq!(
            bytes, EMPTY_BT_NAPKIN_ETF,
            "Phase 0b ETF output diverges from term_to_binary(cerl:c_module(...)) \
             reference. If OTP changed the cerl record shape (unlikely — stable since OTP 18), \
             regenerate the constant by running on the Erlang side: \
             term_to_binary(cerl:c_module(cerl:c_atom(bt_napkin), [], [], []))."
        );
    }

    #[test]
    fn c_literal_atom_round_trips_through_eetf() {
        let lit = CLiteral::atom("hello");
        let term = lit.to_term();
        let mut buf = Vec::new();
        term.encode(&mut buf).unwrap();
        let decoded = Term::decode(std::io::Cursor::new(&buf)).unwrap();
        match decoded {
            Term::Tuple(t) => {
                assert_eq!(t.elements.len(), 3);
                assert_eq!(t.elements[0], Term::from(Atom::from("c_literal")));
                match &t.elements[1] {
                    Term::List(l) => assert!(l.elements.is_empty()),
                    other => panic!("expected list, got {other:?}"),
                }
                assert_eq!(t.elements[2], Term::from(Atom::from("hello")));
            }
            other => panic!("expected tuple, got {other:?}"),
        }
    }

    #[test]
    fn c_var_atom_name_round_trips() {
        let v = CVar::var("X");
        let term = v.to_term();
        match term {
            Term::Tuple(t) => {
                assert_eq!(t.elements.len(), 3);
                assert_eq!(t.elements[0], Term::from(Atom::from("c_var")));
                assert_eq!(t.elements[2], Term::from(Atom::from("X")));
            }
            other => panic!("expected tuple, got {other:?}"),
        }
    }

    #[test]
    fn c_var_fun_ref_round_trips() {
        let v = CVar::fun_ref("module_info", 0);
        let term = v.to_term();
        match term {
            Term::Tuple(t) => {
                assert_eq!(t.elements.len(), 3);
                assert_eq!(t.elements[0], Term::from(Atom::from("c_var")));
                match &t.elements[2] {
                    Term::Tuple(inner) => {
                        assert_eq!(inner.elements.len(), 2);
                        assert_eq!(inner.elements[0], Term::from(Atom::from("module_info")));
                        assert_eq!(inner.elements[1], Term::from(FixInteger::from(0)));
                    }
                    other => panic!("expected tuple for FunRef, got {other:?}"),
                }
            }
            other => panic!("expected tuple, got {other:?}"),
        }
    }

    #[test]
    fn anno_empty_round_trips_as_nil() {
        let anno = Anno::empty();
        let term = anno.to_term();
        let mut buf = Vec::new();
        term.encode(&mut buf).unwrap();
        assert_eq!(buf, vec![131, 106]);
    }

    #[test]
    fn minimum_module_has_six_top_level_fields() {
        let module = CModule::minimum("bt_napkin");
        let term = module.to_term();
        match term {
            Term::Tuple(t) => assert_eq!(t.elements.len(), 6),
            other => panic!("expected 6-tuple, got {other:?}"),
        }
    }

    #[test]
    fn minimum_module_exports_module_info_0_and_1() {
        let module = CModule::minimum("bt_napkin");
        assert_eq!(module.exports.len(), 2);
        assert_eq!(
            module.exports[0].name,
            VarName::FunRef {
                name: "module_info".to_string(),
                arity: 0,
            }
        );
        assert_eq!(
            module.exports[1].name,
            VarName::FunRef {
                name: "module_info".to_string(),
                arity: 1,
            }
        );
    }

    #[test]
    fn minimum_module_def_for_module_info_0_delegates_to_erlang() {
        let module = CModule::minimum("bt_napkin");
        let (_, mi0_fun) = &module.defs[0];
        assert!(mi0_fun.vars.is_empty(), "module_info/0 takes no args");
        match &mi0_fun.body {
            Expr::Call(call) => {
                assert_eq!(call.args.len(), 1, "single arg: ModName");
                match &call.module {
                    Expr::Literal(lit) => {
                        assert_eq!(lit.value, LiteralValue::Atom("erlang".to_string()));
                    }
                    _ => panic!("expected literal module"),
                }
                match &call.name {
                    Expr::Literal(lit) => {
                        assert_eq!(lit.value, LiteralValue::Atom("get_module_info".to_string()));
                    }
                    _ => panic!("expected literal name"),
                }
            }
            _ => panic!("expected Call body for module_info/0"),
        }
    }

    #[test]
    fn minimum_module_etf_round_trips_through_eetf() {
        let module = CModule::minimum("bt_napkin");
        let bytes = module.encode_etf().unwrap();
        let decoded = Term::decode(std::io::Cursor::new(&bytes)).unwrap();
        match decoded {
            Term::Tuple(t) => {
                assert_eq!(t.elements.len(), 6);
                assert_eq!(t.elements[0], Term::from(Atom::from("c_module")));
            }
            other => panic!("expected 6-tuple, got {other:?}"),
        }
    }

    /// Reference bytes captured from
    /// `term_to_binary(cerl:c_module(cerl:c_atom(bt_napkin), [Mi0, Mi1], [], [{Mi0, Mi0Fun}, {Mi1, Mi1Fun}]))`
    /// on the Erlang side, where `Mi0Fun` / `Mi1Fun` delegate to
    /// `erlang:get_module_info/{1,2}`. This is the byte sequence Phase 0b
    /// ships over the Port for the napkin's smoke test.
    ///
    /// If this constant ever needs to change (extremely unlikely — cerl
    /// record shapes have been stable since OTP 18), regenerate via:
    ///
    /// ```text
    /// escript scripts/cerl-napkin-dump.escript
    /// ```
    ///
    /// and update the byte array. The same bytes are also asserted by the
    /// matching Erlang `EUnit` test in
    /// `runtime/apps/beamtalk_compiler/test/beamtalk_cerl_wire_tests.erl`.
    const MINIMUM_BT_NAPKIN_ETF: &[u8] = &[
        131, 104, 6, 119, 8, 99, 95, 109, 111, 100, 117, 108, 101, 106, 104, 3, 119, 9, 99, 95,
        108, 105, 116, 101, 114, 97, 108, 106, 119, 9, 98, 116, 95, 110, 97, 112, 107, 105, 110,
        108, 0, 0, 0, 2, 104, 3, 119, 5, 99, 95, 118, 97, 114, 106, 104, 2, 119, 11, 109, 111, 100,
        117, 108, 101, 95, 105, 110, 102, 111, 97, 0, 104, 3, 119, 5, 99, 95, 118, 97, 114, 106,
        104, 2, 119, 11, 109, 111, 100, 117, 108, 101, 95, 105, 110, 102, 111, 97, 1, 106, 106,
        108, 0, 0, 0, 2, 104, 2, 104, 3, 119, 5, 99, 95, 118, 97, 114, 106, 104, 2, 119, 11, 109,
        111, 100, 117, 108, 101, 95, 105, 110, 102, 111, 97, 0, 104, 4, 119, 5, 99, 95, 102, 117,
        110, 106, 106, 104, 5, 119, 6, 99, 95, 99, 97, 108, 108, 106, 104, 3, 119, 9, 99, 95, 108,
        105, 116, 101, 114, 97, 108, 106, 119, 6, 101, 114, 108, 97, 110, 103, 104, 3, 119, 9, 99,
        95, 108, 105, 116, 101, 114, 97, 108, 106, 119, 15, 103, 101, 116, 95, 109, 111, 100, 117,
        108, 101, 95, 105, 110, 102, 111, 108, 0, 0, 0, 1, 104, 3, 119, 9, 99, 95, 108, 105, 116,
        101, 114, 97, 108, 106, 119, 9, 98, 116, 95, 110, 97, 112, 107, 105, 110, 106, 104, 2, 104,
        3, 119, 5, 99, 95, 118, 97, 114, 106, 104, 2, 119, 11, 109, 111, 100, 117, 108, 101, 95,
        105, 110, 102, 111, 97, 1, 104, 4, 119, 5, 99, 95, 102, 117, 110, 106, 108, 0, 0, 0, 1,
        104, 3, 119, 5, 99, 95, 118, 97, 114, 106, 119, 1, 88, 106, 104, 5, 119, 6, 99, 95, 99, 97,
        108, 108, 106, 104, 3, 119, 9, 99, 95, 108, 105, 116, 101, 114, 97, 108, 106, 119, 6, 101,
        114, 108, 97, 110, 103, 104, 3, 119, 9, 99, 95, 108, 105, 116, 101, 114, 97, 108, 106, 119,
        15, 103, 101, 116, 95, 109, 111, 100, 117, 108, 101, 95, 105, 110, 102, 111, 108, 0, 0, 0,
        2, 104, 3, 119, 9, 99, 95, 108, 105, 116, 101, 114, 97, 108, 106, 119, 9, 98, 116, 95, 110,
        97, 112, 107, 105, 110, 104, 3, 119, 5, 99, 95, 118, 97, 114, 106, 119, 1, 88, 106, 106,
    ];

    #[test]
    fn minimum_module_matches_otp_term_to_binary() {
        let module = CModule::minimum("bt_napkin");
        let bytes = module.encode_etf().expect("encode failed");
        assert_eq!(
            bytes.len(),
            MINIMUM_BT_NAPKIN_ETF.len(),
            "Phase 0b minimum-module ETF size diverges from \
             term_to_binary(cerl:c_module(...)). See scripts/cerl-napkin-dump.escript \
             to regenerate the reference."
        );
        assert_eq!(
            bytes, MINIMUM_BT_NAPKIN_ETF,
            "Phase 0b minimum-module ETF byte layout diverges from the OTP \
             cerl reference. See scripts/cerl-napkin-dump.escript to regenerate."
        );
    }
}
