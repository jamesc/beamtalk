// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Generated class-variable programs and the ADR 0130 reference interpreter.
//!
//! BT-3705 (ADR 0130 Phase 1). `docs/agents/expanded.md` § State-Threading
//! Codegen, point 4, requires a generated-corpus proof for any change to the
//! `ThreadedIr` lowerings. The state-threading rules for class variables are
//! "right Core Erlang, wrong value" bugs that the verifier cannot express, so
//! the proof is an execution-level agreement property:
//!
//! 1. [`gen_program`] builds a small class-method [`Program`] over two class
//!    variables (`n`, `m`) from a seed: writing and plain self-sends,
//!    `ifTrue:`/`ifTrue:ifFalse:`, `do:`/`collect:`/`inject:into:`/`to:do:`/
//!    `whileTrue:`/`timesRepeat:`, `on:do:`/`ensure:`/`Result tryDo:` with
//!    raises inside the protected block, self-sends in loop conditions,
//!    stored closures invoked later, and early `^` returns.
//! 2. [`Program::render`] spells one program three ways (a [`Spelling`]):
//!    an open class, a `sealed` class, and a base class whose helper methods
//!    are overridden in a subclass (late-bound `self foo`). All three must
//!    answer the same.
//! 3. [`Program::interpret`] is the reference interpretation of ADR 0130 §4:
//!    every write is immediate; every write made inside a protected region is
//!    undone when an error crosses that region's catch; every write of an
//!    invocation is undone when an error escapes it; a `^` undoes nothing.
//!
//! Values are non-negative integers throughout (literals, `+` of a literal,
//! class variables, helper results), which keeps every loop terminating
//! (a `whileTrue:` counter only grows) and every interpretation exact.
//!
//! This module has no dependency on the code generator: the codegen-side
//! property (`beamtalk-codegen/tests/class_var_agreement.rs`) and the BEAM
//! execution harness (`beamtalk-cli/tests/cli/cli_class_var_agreement.rs`) both
//! consume it, so the rule "what is a valid program, and what must it answer"
//! lives in exactly one place.

use std::fmt::Write as _;
use std::rc::Rc;

// ---------------------------------------------------------------------------
// Shapes (feature flags for the generator)
// ---------------------------------------------------------------------------

/// A set of language shapes the generator may use.
///
/// The full set is what ADR 0130 must make correct; [`Shapes::SUPPORTED_TODAY`]
/// is the subset every program of which passes on the current compiler, so the
/// default (non-ignored) property is green while the full set stays a measured,
/// ignored property until the Phase 3 flip removes the `#[ignore]`.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Shapes(u32);

impl std::fmt::Debug for Shapes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.names().join(","))
    }
}

impl Shapes {
    /// `do:` over a literal array.
    pub const LOOP_DO: Shapes = Shapes(1 << 0);
    /// `collect:` and `inject:into:` (the fold loops).
    pub const LOOP_FOLD: Shapes = Shapes(1 << 1);
    /// `to:do:`.
    pub const LOOP_TO_DO: Shapes = Shapes(1 << 2);
    /// `timesRepeat:`.
    pub const LOOP_TIMES: Shapes = Shapes(1 << 3);
    /// `whileTrue:` with a counter.
    pub const LOOP_WHILE: Shapes = Shapes(1 << 4);
    /// `ifTrue:` / `ifTrue:ifFalse:`.
    pub const COND: Shapes = Shapes(1 << 5);
    /// `on:do:`.
    pub const ON_DO: Shapes = Shapes(1 << 6);
    /// `ensure:`.
    pub const ENSURE: Shapes = Shapes(1 << 7);
    /// `Result tryDo:`.
    pub const TRY_DO: Shapes = Shapes(1 << 8);
    /// `Error signal:` raises.
    pub const RAISE: Shapes = Shapes(1 << 9);
    /// A closure stored in a local and invoked by a later statement.
    pub const STORED_CLOSURE: Shapes = Shapes(1 << 10);
    /// A self-send inside a loop condition or an `ifTrue:` condition.
    pub const SEND_IN_COND: Shapes = Shapes(1 << 11);
    /// `^` early returns from inside blocks.
    pub const RETURN: Shapes = Shapes(1 << 12);
    /// Self-sends of the generated helper methods (writing and plain).
    pub const HELPER_SEND: Shapes = Shapes(1 << 13);
    /// A direct `self.n := ...` write inside a block (not only at method top
    /// level). The current compiler rejects this unless the block also
    /// mutates a local (`FieldAssignmentInUnsupportedBlock`).
    pub const DIRECT_BLOCK_WRITE: Shapes = Shapes(1 << 14);
    /// A self-send used as a value: the right-hand side of a write or a
    /// `Let`, an operand of `+`, or an argument of another send.
    pub const SEND_IN_EXPR: Shapes = Shapes(1 << 15);
    /// Every block body mutates a method-level local ([`Stmt::Touch`]), the
    /// documented workaround (removed by the ADR 0130 Phase 3 flip, BT-3713)
    /// that lets the current compiler thread class
    /// variables through it. Without it the compiler rejects most
    /// class-variable writes and writing self-sends inside blocks.
    pub const LOCAL_TOUCH: Shapes = Shapes(1 << 16);
    /// A loop or fold nested inside another loop or fold body.
    pub const NESTED_LOOPS: Shapes = Shapes(1 << 17);

    /// No shapes: straight-line writes and arithmetic only.
    pub const NONE: Shapes = Shapes(0);

    /// Every shape.
    #[must_use]
    pub const fn all() -> Shapes {
        // Derived from `NAMED`, the one table of shapes, so a new shape cannot
        // be named without being in `all()`.
        let mut bits = 0;
        let mut i = 0;
        while i < Self::NAMED.len() {
            bits |= Self::NAMED[i].1.0;
            i += 1;
        }
        Shapes(bits)
    }

    /// Whether every shape in `other` is in `self`.
    #[must_use]
    pub const fn has(self, other: Shapes) -> bool {
        self.0 & other.0 == other.0
    }

    /// The union of two sets.
    #[must_use]
    pub const fn with(self, other: Shapes) -> Shapes {
        Shapes(self.0 | other.0)
    }

    /// `self` without the shapes in `other`.
    #[must_use]
    pub const fn without(self, other: Shapes) -> Shapes {
        Shapes(self.0 & !other.0)
    }

    /// Every named shape, for [`Shapes::names`] and [`Shapes::parse`].
    const NAMED: [(&'static str, Shapes); 18] = [
        ("do", Shapes::LOOP_DO),
        ("fold", Shapes::LOOP_FOLD),
        ("to_do", Shapes::LOOP_TO_DO),
        ("times", Shapes::LOOP_TIMES),
        ("while", Shapes::LOOP_WHILE),
        ("cond", Shapes::COND),
        ("on_do", Shapes::ON_DO),
        ("ensure", Shapes::ENSURE),
        ("try_do", Shapes::TRY_DO),
        ("raise", Shapes::RAISE),
        ("stored_closure", Shapes::STORED_CLOSURE),
        ("send_in_cond", Shapes::SEND_IN_COND),
        ("return", Shapes::RETURN),
        ("helper_send", Shapes::HELPER_SEND),
        ("direct_block_write", Shapes::DIRECT_BLOCK_WRITE),
        ("send_in_expr", Shapes::SEND_IN_EXPR),
        ("local_touch", Shapes::LOCAL_TOUCH),
        ("nested_loops", Shapes::NESTED_LOOPS),
    ];

    /// The names of the shapes in `self`.
    #[must_use]
    pub fn names(self) -> Vec<&'static str> {
        Self::NAMED
            .iter()
            .filter(|(_, s)| self.has(*s))
            .map(|(n, _)| *n)
            .collect()
    }

    /// Parses `all`, `none`, or a comma-separated list of shape names (the
    /// `CV_CORPUS_SHAPES` knob of the execution property). `None` on an
    /// unknown name.
    #[must_use]
    pub fn parse(text: &str) -> Option<Shapes> {
        match text.trim() {
            "all" => return Some(Shapes::all()),
            "none" | "" => return Some(Shapes::NONE),
            _ => {}
        }
        text.split(',').try_fold(Shapes::NONE, |acc, name| {
            Self::NAMED
                .iter()
                .find(|(n, _)| *n == name.trim())
                .map(|(_, s)| acc.with(*s))
        })
    }

    /// The shapes whose generated programs pass on the current compiler:
    /// writing and plain self-sends (statement position only), `ifTrue:` /
    /// `ifTrue:ifFalse:`, and early `^` returns.
    ///
    /// Everything else fails today (see the BT-3705 PR for the measured
    /// failure rate per shape): loops, `on:do:`/`ensure:`/`tryDo:`, stored
    /// closures and direct writes in blocks are rejected at compile time or
    /// fail the `ThreadedIr` verifier; a self-send used as a value (the right
    /// hand side of a write, an operand, an argument) and a raise inside a
    /// conditional arm give a wrong answer or invalid Core Erlang. Phase 3
    /// (BT-3713) replaces this with [`Shapes::all`].
    pub const SUPPORTED_TODAY: Shapes = Shapes::HELPER_SEND.with(Shapes::COND).with(Shapes::RETURN);
}

// ---------------------------------------------------------------------------
// Program IR
// ---------------------------------------------------------------------------

/// One of the two class variables every generated class declares.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CVar {
    /// `classState: n = 0`.
    N,
    /// `classState: m = 0`.
    M,
}

impl CVar {
    fn name(self) -> &'static str {
        match self {
            CVar::N => "n",
            CVar::M => "m",
        }
    }

    fn index(self) -> usize {
        match self {
            CVar::N => 0,
            CVar::M => 1,
        }
    }
}

/// A value expression. Every expression evaluates to a non-negative integer.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    /// An integer literal.
    Lit(i64),
    /// A local variable, block parameter or method argument.
    Local(String),
    /// `self.n` / `self.m`.
    Cv(CVar),
    /// `expr + literal`.
    Add(Box<Expr>, i64),
    /// `self hK: arg`.
    Send { helper: usize, arg: Box<Expr> },
    /// `[body] on: Error do: [:param | handler]`.
    OnDo {
        body: Block,
        param: String,
        handler: Block,
    },
    /// `[body] ensure: [cleanup]`.
    Ensure { body: Block, cleanup: Block },
    /// `(Result tryDo: [body]) valueOr: 0`.
    TryDo(Block),
    /// `items inject: init into: [:acc :elem | body]`.
    Inject {
        items: Vec<i64>,
        init: Box<Expr>,
        acc: String,
        elem: String,
        body: Block,
    },
    /// `(items collect: [:elem | body]) inject: 0 into: [:a :x | a + x]`.
    CollectSum {
        items: Vec<i64>,
        elem: String,
        body: Block,
    },
    /// `closure value`, where `closure` is a stored block.
    CallStored(String),
    /// `Error signal: "boom"`.
    Raise,
}

/// A statement list ending in a value expression.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Block {
    /// The statements before the value.
    pub stmts: Vec<Stmt>,
    /// The block's value (the method's value when this is a method body).
    pub tail: Box<Expr>,
}

/// A comparison against a literal: `lhs op rhs`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cond {
    /// Left operand.
    pub lhs: Expr,
    /// Operator.
    pub op: Cmp,
    /// Right operand (a literal).
    pub rhs: i64,
}

/// A comparison operator.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Cmp {
    /// `<`
    Lt,
    /// `>`
    Gt,
    /// `=:=`
    Eq,
}

/// A statement.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmt {
    /// `self.n := expr`.
    Write(CVar, Expr),
    /// `name := expr` (a new local, scoped to the enclosing block).
    Let(String, Expr),
    /// An expression evaluated for effect.
    Eval(Expr),
    /// `name := [stmts. tail]`: a closure invoked by a later `CallStored`.
    Store(String, Block),
    /// `cond ifTrue: [then] ifFalse: [els]`.
    If {
        cond: Cond,
        then: Vec<Stmt>,
        els: Option<Vec<Stmt>>,
    },
    /// `items do: [:elem | body]`.
    Do {
        items: Vec<i64>,
        elem: String,
        body: Vec<Stmt>,
    },
    /// `lo to: hi do: [:var | body]`.
    ToDo {
        lo: i64,
        hi: i64,
        var: String,
        body: Vec<Stmt>,
    },
    /// `k timesRepeat: [body]`.
    Times { k: i64, body: Vec<Stmt> },
    /// `counter := 0. [counter + extra < bound] whileTrue: [body. counter := counter + 1]`.
    While {
        counter: String,
        bound: i64,
        extra: Option<Expr>,
        body: Vec<Stmt>,
    },
    /// `^ expr`.
    Return(Expr),
    /// `Error signal: "boom"`.
    Raise,
    /// `name := name + 1` on a method-level local: the documented workaround
    /// (obsolete after the ADR 0130 Phase 3 flip, BT-3713)
    /// that makes the enclosing block a *threaded* body, in which the current
    /// compiler accepts class-variable writes and writing self-sends. No
    /// effect on the program's answer.
    Touch(String),
}

/// The method-level local [`Stmt::Touch`] bumps.
pub const TOUCH_LOCAL: &str = "t";

/// A generated program: helper class methods `hK: x` and the entry `run`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    /// Helper `K` is `class hK: x`; it may only send helpers `< K`.
    pub helpers: Vec<Block>,
    /// The entry method `class run`.
    pub run: Block,
    /// For [`Spelling::Override`]: which helpers the subclass overrides (the
    /// base class defines a stub for those, the real body for the others).
    pub overridden: Vec<bool>,
}

/// How a program is spelled as Beamtalk classes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Spelling {
    /// `Object subclass: Name` (an open class).
    Open,
    /// `sealed Object subclass: Name`.
    Sealed,
    /// A base class `run` + helpers, with a subclass overriding the helpers
    /// in [`Program::overridden`]; the entry is the subclass.
    Override,
}

impl Spelling {
    /// Every spelling.
    pub const ALL: [Spelling; 3] = [Spelling::Open, Spelling::Sealed, Spelling::Override];

    fn suffix(self) -> &'static str {
        match self {
            Spelling::Open => "Open",
            Spelling::Sealed => "Sealed",
            Spelling::Override => "Leaf",
        }
    }
}

/// What a program answers: the value of `run` (or `None` when an error escapes
/// it) and the two class variables afterwards.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Outcome {
    /// `run`'s value, or `None` when an error escaped the invocation.
    pub result: Option<i64>,
    /// `n` after the invocation.
    pub n: i64,
    /// `m` after the invocation.
    pub m: i64,
}

/// The value a test harness substitutes for `run` when an error escapes.
pub const ERROR_MARKER: i64 = -1;

// ---------------------------------------------------------------------------
// Reference interpreter (ADR 0130 §4)
// ---------------------------------------------------------------------------

/// Evaluation budget: a program that needs more steps is rejected by the
/// generator rather than interpreted.
const STEP_BUDGET: u64 = 20_000;

#[derive(Debug)]
enum Ctl {
    /// A raised error.
    Err,
    /// A `^` unwinding to the enclosing method.
    Ret(i64),
    /// The step budget ran out.
    Budget,
}

type R<T> = Result<T, Ctl>;

#[derive(Clone)]
enum Val<'p> {
    Int(i64),
    Closure(&'p Block, Rc<Vec<(String, Val<'p>)>>),
}

type Env<'p> = Vec<(String, Val<'p>)>;

struct Machine<'p> {
    prog: &'p Program,
    cv: [i64; 2],
    steps: u64,
}

impl<'p> Machine<'p> {
    fn tick(&mut self) -> R<()> {
        self.steps += 1;
        if self.steps > STEP_BUDGET {
            Err(Ctl::Budget)
        } else {
            Ok(())
        }
    }

    fn lookup(env: &Env<'p>, name: &str) -> Val<'p> {
        let Some((_, v)) = env.iter().rev().find(|(n, _)| n == name) else {
            panic!("generated program reads unbound local {name}")
        };
        v.clone()
    }

    fn lookup_int(env: &Env<'p>, name: &str) -> i64 {
        match Self::lookup(env, name) {
            Val::Int(v) => v,
            Val::Closure(..) => panic!("local {name} is a closure, not an integer"),
        }
    }

    /// Runs a method body; `^` is caught here (the method boundary).
    fn run_method(&mut self, body: &'p Block, env: &mut Env<'p>) -> R<i64> {
        match self.block(body, env) {
            Err(Ctl::Ret(v)) => Ok(v),
            other => other,
        }
    }

    /// Runs a block's statements and value in a fresh scope of `env`.
    fn block(&mut self, b: &'p Block, env: &mut Env<'p>) -> R<i64> {
        let mark = env.len();
        let r = self.block_in_scope(b, env);
        env.truncate(mark);
        r
    }

    fn block_in_scope(&mut self, b: &'p Block, env: &mut Env<'p>) -> R<i64> {
        for s in &b.stmts {
            self.stmt(s, env)?;
        }
        self.expr(&b.tail, env)
    }

    fn stmts(&mut self, ss: &'p [Stmt], env: &mut Env<'p>) -> R<()> {
        let mark = env.len();
        let mut r = Ok(());
        for s in ss {
            r = self.stmt(s, env);
            if r.is_err() {
                break;
            }
        }
        env.truncate(mark);
        r
    }

    fn cond(&mut self, c: &'p Cond, env: &mut Env<'p>) -> R<bool> {
        let l = self.expr(&c.lhs, env)?;
        Ok(match c.op {
            Cmp::Lt => l < c.rhs,
            Cmp::Gt => l > c.rhs,
            Cmp::Eq => l == c.rhs,
        })
    }

    fn stmt(&mut self, s: &'p Stmt, env: &mut Env<'p>) -> R<()> {
        self.tick()?;
        match s {
            Stmt::Write(var, e) => {
                let v = self.expr(e, env)?;
                self.cv[var.index()] = v;
            }
            Stmt::Let(name, e) => {
                let v = self.expr(e, env)?;
                env.push((name.clone(), Val::Int(v)));
            }
            Stmt::Eval(e) => {
                self.expr(e, env)?;
            }
            Stmt::Store(name, b) => {
                env.push((name.clone(), Val::Closure(b, Rc::new(env.clone()))));
            }
            Stmt::If { cond, then, els } => {
                if self.cond(cond, env)? {
                    self.stmts(then, env)?;
                } else if let Some(els) = els {
                    self.stmts(els, env)?;
                }
            }
            Stmt::Do { items, elem, body } => {
                for item in items {
                    env.push((elem.clone(), Val::Int(*item)));
                    let r = self.stmts(body, env);
                    env.pop();
                    r?;
                }
            }
            Stmt::ToDo { lo, hi, var, body } => {
                for i in *lo..=*hi {
                    env.push((var.clone(), Val::Int(i)));
                    let r = self.stmts(body, env);
                    env.pop();
                    r?;
                }
            }
            Stmt::Times { k, body } => {
                for _ in 0..*k {
                    self.stmts(body, env)?;
                }
            }
            Stmt::While {
                counter,
                bound,
                extra,
                body,
            } => {
                env.push((counter.clone(), Val::Int(0)));
                let r = self.while_loop(counter, *bound, extra.as_ref(), body, env);
                env.pop();
                r?;
            }
            Stmt::Return(e) => {
                let v = self.expr(e, env)?;
                return Err(Ctl::Ret(v));
            }
            Stmt::Raise => return Err(Ctl::Err),
            Stmt::Touch(name) => {
                let c = Self::lookup_int(env, name);
                let slot = env
                    .iter_mut()
                    .rev()
                    .find(|(n, _)| n == name)
                    .expect("touch local in scope");
                slot.1 = Val::Int(c + 1);
            }
        }
        Ok(())
    }

    fn while_loop(
        &mut self,
        counter: &str,
        bound: i64,
        extra: Option<&'p Expr>,
        body: &'p [Stmt],
        env: &mut Env<'p>,
    ) -> R<()> {
        loop {
            self.tick()?;
            let extra_v = match extra {
                Some(e) => self.expr(e, env)?,
                None => 0,
            };
            let c = Self::lookup_int(env, counter);
            if c + extra_v >= bound {
                return Ok(());
            }
            self.stmts(body, env)?;
            // The implicit trailing `counter := counter + 1`.
            let slot = env
                .iter_mut()
                .rev()
                .find(|(n, _)| n == counter)
                .expect("counter in scope");
            slot.1 = Val::Int(c + 1);
        }
    }

    fn expr(&mut self, e: &'p Expr, env: &mut Env<'p>) -> R<i64> {
        self.tick()?;
        match e {
            Expr::Lit(v) => Ok(*v),
            Expr::Local(name) => Ok(Self::lookup_int(env, name)),
            Expr::Cv(var) => Ok(self.cv[var.index()]),
            Expr::Add(a, k) => Ok(self.expr(a, env)? + k),
            Expr::Send { helper, arg } => {
                let a = self.expr(arg, env)?;
                let prog = self.prog;
                let mut henv: Env<'p> = vec![("x".to_string(), Val::Int(a))];
                self.run_method(&prog.helpers[*helper], &mut henv)
            }
            Expr::OnDo { body, handler, .. } => {
                // Catch boundary: snapshot on entry, restore when an error
                // crosses the catch, before the handler runs. A `^` is not an
                // error and restores nothing.
                let snap = self.cv;
                match self.block(body, env) {
                    Err(Ctl::Err) => {
                        self.cv = snap;
                        self.block(handler, env)
                    }
                    other => other,
                }
            }
            Expr::TryDo(body) => {
                let snap = self.cv;
                match self.block(body, env) {
                    Err(Ctl::Err) => {
                        self.cv = snap;
                        Ok(0)
                    }
                    other => other,
                }
            }
            Expr::Ensure { body, cleanup } => {
                // `ensure:` is not a boundary: it runs the cleanup on every
                // exit and lets the original outcome continue, unless the
                // cleanup itself raises.
                let r = self.block(body, env);
                if matches!(r, Err(Ctl::Budget)) {
                    return r;
                }
                self.block(cleanup, env)?;
                r
            }
            Expr::Inject {
                items,
                init,
                acc,
                elem,
                body,
            } => {
                let mut a = self.expr(init, env)?;
                for item in items {
                    env.push((acc.clone(), Val::Int(a)));
                    env.push((elem.clone(), Val::Int(*item)));
                    let r = self.block(body, env);
                    env.pop();
                    env.pop();
                    a = r?;
                }
                Ok(a)
            }
            Expr::CollectSum { items, elem, body } => {
                let mut sum = 0;
                for item in items {
                    env.push((elem.clone(), Val::Int(*item)));
                    let r = self.block(body, env);
                    env.pop();
                    sum += r?;
                }
                Ok(sum)
            }
            Expr::CallStored(name) => match Self::lookup(env, name) {
                Val::Closure(b, captured) => {
                    let mut cenv: Env<'p> = (*captured).clone();
                    self.block(b, &mut cenv)
                }
                Val::Int(_) => panic!("local {name} is not a closure"),
            },
            Expr::Raise => Err(Ctl::Err),
        }
    }
}

impl Program {
    /// Interprets the program per ADR 0130 §4. `None` when the step budget is
    /// exhausted (the generator never produces such a program).
    #[must_use]
    pub fn interpret(&self) -> Option<Outcome> {
        let mut m = Machine {
            prog: self,
            cv: [0, 0],
            steps: 0,
        };
        let mut env: Env<'_> = Vec::new();
        match m.run_method(&self.run, &mut env) {
            Ok(v) => Some(Outcome {
                result: Some(v),
                n: m.cv[0],
                m: m.cv[1],
            }),
            // Invocation boundary: an escaping error discards every write.
            Err(Ctl::Err) => Some(Outcome {
                result: None,
                n: 0,
                m: 0,
            }),
            Err(Ctl::Ret(_)) => unreachable!("run_method catches `^`"),
            Err(Ctl::Budget) => None,
        }
    }
}

// ---------------------------------------------------------------------------
// Generator
// ---------------------------------------------------------------------------

/// A small deterministic PRNG (splitmix64), so a program is a pure function of
/// its seed and a failing case is reproducible from the printed seed.
#[derive(Clone, Debug)]
struct Rng(u64);

impl Rng {
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        z ^ (z >> 31)
    }

    /// Uniform in `0..n` (`n > 0`).
    fn below(&mut self, n: u64) -> u64 {
        self.next() % n
    }

    /// Uniform index into a collection of `len > 0` items.
    // why: test-support code; collections here hold a handful of items, so the
    // `usize` <-> `u64` conversions cannot truncate on any supported target.
    #[allow(clippy::cast_possible_truncation)]
    fn index(&mut self, len: usize) -> usize {
        self.below(len as u64) as usize
    }

    /// Uniform in `lo..=hi`.
    fn range(&mut self, lo: i64, hi: i64) -> i64 {
        let span = u64::try_from(hi - lo + 1).expect("non-empty range");
        lo + i64::try_from(self.below(span)).expect("small")
    }

    fn chance(&mut self, percent: u64) -> bool {
        self.below(100) < percent
    }
}

#[derive(Clone)]
// why: independent context flags of a test-only generator; a bitset would hide
// what each one means.
#[allow(clippy::struct_excessive_bools)]
struct Ctx {
    /// Integer locals readable here.
    scope: Vec<String>,
    /// Stored closures callable here.
    closures: Vec<String>,
    /// Helpers `< helpers_below` may be sent.
    helpers_below: usize,
    /// Remaining statement nesting depth.
    depth: u32,
    /// Inside any block (loop body, arm, protected block, ...).
    in_block: bool,
    /// Inside the protected block of an `on:do:` / `tryDo:`.
    protected: bool,
    /// A `^` is allowed here (not in a stored closure or cleanup block).
    can_return: bool,
    /// Do not add [`Stmt::Touch`] (stored closures keep their own locals).
    no_touch: bool,
    /// Inside the body of a loop or fold.
    in_loop: bool,
}

struct Gen {
    rng: Rng,
    shapes: Shapes,
    fresh: u32,
}

impl Gen {
    fn name(&mut self, prefix: &str) -> String {
        self.fresh += 1;
        format!("{prefix}{}", self.fresh)
    }

    fn cvar(&mut self) -> CVar {
        if self.rng.chance(60) {
            CVar::N
        } else {
            CVar::M
        }
    }

    fn leaf(&mut self, c: &Ctx) -> Expr {
        let k = self.rng.below(5);
        if k < 2 || (c.scope.is_empty() && k < 4) {
            Expr::Lit(self.rng.range(0, 4))
        } else if k < 4 {
            let i = self.rng.index(c.scope.len());
            Expr::Local(c.scope[i].clone())
        } else {
            Expr::Cv(self.cvar())
        }
    }

    fn send(&mut self, c: &Ctx) -> Option<Expr> {
        if !self.shapes.has(Shapes::HELPER_SEND) || c.helpers_below == 0 {
            return None;
        }
        let helper = self.rng.index(c.helpers_below);
        let arg = Box::new(self.leaf(c));
        Some(Expr::Send { helper, arg })
    }

    /// An integer-valued expression without compound (block) forms.
    fn simple(&mut self, c: &Ctx) -> Expr {
        let base = match self.rng.below(10) {
            0..=3 if self.shapes.has(Shapes::SEND_IN_EXPR) => {
                self.send(c).unwrap_or_else(|| self.leaf(c))
            }
            4 if !c.closures.is_empty() => {
                let i = self.rng.index(c.closures.len());
                Expr::CallStored(c.closures[i].clone())
            }
            _ => self.leaf(c),
        };
        if self.rng.chance(30) {
            Expr::Add(Box::new(base), self.rng.range(1, 3))
        } else {
            base
        }
    }

    fn loop_ok(&self, c: &Ctx) -> bool {
        self.shapes.has(Shapes::NESTED_LOOPS) || !c.in_loop
    }

    fn compound_kinds(&self, c: &Ctx) -> Vec<u8> {
        let mut kinds = Vec::new();
        if c.depth == 0 {
            return kinds;
        }
        if self.shapes.has(Shapes::ON_DO) {
            kinds.push(0);
        }
        if self.shapes.has(Shapes::ENSURE) {
            kinds.push(1);
        }
        if self.shapes.has(Shapes::TRY_DO) {
            kinds.push(2);
        }
        if self.shapes.has(Shapes::LOOP_FOLD) && self.loop_ok(c) {
            kinds.push(3);
            kinds.push(4);
        }
        kinds
    }

    fn expr(&mut self, c: &Ctx) -> Expr {
        let kinds = self.compound_kinds(c);
        if kinds.is_empty() || !self.rng.chance(25) {
            return self.simple(c);
        }
        let kind = kinds[self.rng.index(kinds.len())];
        self.compound(c, kind)
    }

    fn inner(c: &Ctx) -> Ctx {
        let mut inner = c.clone();
        inner.depth = c.depth.saturating_sub(1);
        inner.in_block = true;
        inner
    }

    fn compound(&mut self, c: &Ctx, kind: u8) -> Expr {
        let inner = Self::inner(c);
        match kind {
            0 => {
                let mut body_ctx = inner.clone();
                body_ctx.protected = true;
                let body = self.block(&body_ctx, 1, 3);
                let handler = self.block(&inner, 0, 2);
                let param = self.name("x");
                Expr::OnDo {
                    body,
                    param,
                    handler,
                }
            }
            1 => {
                let body = self.block(&inner, 1, 3);
                let mut cleanup_ctx = inner;
                cleanup_ctx.can_return = false;
                let cleanup = self.block(&cleanup_ctx, 0, 2);
                Expr::Ensure { body, cleanup }
            }
            2 => {
                let mut body_ctx = inner;
                body_ctx.protected = true;
                Expr::TryDo(self.block(&body_ctx, 1, 3))
            }
            3 => {
                let items = self.items();
                let init = Box::new(self.leaf(c));
                let acc = self.name("a");
                let elem = self.name("e");
                let mut body_ctx = inner;
                body_ctx.scope.push(acc.clone());
                body_ctx.scope.push(elem.clone());
                body_ctx.in_loop = true;
                let body = self.block(&body_ctx, 0, 2);
                Expr::Inject {
                    items,
                    init,
                    acc,
                    elem,
                    body,
                }
            }
            _ => {
                let items = self.items();
                let elem = self.name("e");
                let mut body_ctx = inner;
                body_ctx.scope.push(elem.clone());
                body_ctx.in_loop = true;
                let body = self.block(&body_ctx, 0, 2);
                Expr::CollectSum { items, elem, body }
            }
        }
    }

    fn items(&mut self) -> Vec<i64> {
        let n = self.rng.range(2, 3);
        (0..n).map(|_| self.rng.range(1, 4)).collect()
    }

    /// A block: `min..=max` statements and a value expression.
    fn block(&mut self, c: &Ctx, min: i64, max: i64) -> Block {
        let mut c = c.clone();
        let n = self.rng.range(min, max);
        let stmts = self.stmts(&mut c, n);
        let tail = Box::new(self.expr(&c));
        Block { stmts, tail }
    }

    fn stmts(&mut self, c: &mut Ctx, n: i64) -> Vec<Stmt> {
        let mut out = Vec::new();
        if c.in_block && !c.no_touch && self.shapes.has(Shapes::LOCAL_TOUCH) {
            out.push(Stmt::Touch(TOUCH_LOCAL.to_string()));
        }
        for _ in 0..n {
            out.push(self.stmt(c));
        }
        out
    }

    fn arm(&mut self, c: &Ctx) -> Vec<Stmt> {
        let mut inner = Self::inner(c);
        let n = self.rng.range(1, 2);
        self.stmts(&mut inner, n)
    }

    fn cond(&mut self, c: &Ctx) -> Cond {
        let sent = if self.shapes.has(Shapes::SEND_IN_COND) && self.rng.chance(60) {
            self.send(c)
        } else {
            None
        };
        let base = sent.unwrap_or_else(|| self.leaf(c));
        let lhs = if self.rng.chance(30) {
            Expr::Add(Box::new(base), self.rng.range(1, 3))
        } else {
            base
        };
        let op = match self.rng.below(3) {
            0 => Cmp::Lt,
            1 => Cmp::Gt,
            _ => Cmp::Eq,
        };
        Cond {
            lhs,
            op,
            rhs: self.rng.range(0, 6),
        }
    }

    #[allow(clippy::too_many_lines)] // one flat weighted choice over statement kinds
    fn stmt(&mut self, c: &mut Ctx) -> Stmt {
        #[derive(Clone, Copy)]
        enum K {
            Write,
            Let,
            EvalSend,
            EvalCompound,
            If,
            Do,
            ToDo,
            Times,
            While,
            Store,
            EvalStored,
            Raise,
            Return,
        }
        let s = self.shapes;
        let loop_ok = self.loop_ok(c);
        let nested = c.depth > 0;
        let mut ks: Vec<(K, u64)> = Vec::new();
        if !c.in_block || s.has(Shapes::DIRECT_BLOCK_WRITE) {
            ks.push((K::Write, 5));
        }
        ks.push((K::Let, 2));
        if s.has(Shapes::HELPER_SEND) && c.helpers_below > 0 {
            ks.push((K::EvalSend, 4));
        }
        if !self.compound_kinds(c).is_empty() {
            ks.push((K::EvalCompound, 3));
        }
        if nested && s.has(Shapes::COND) {
            ks.push((K::If, 4));
        }
        if nested && loop_ok && s.has(Shapes::LOOP_DO) {
            ks.push((K::Do, 2));
        }
        if nested && loop_ok && s.has(Shapes::LOOP_TO_DO) {
            ks.push((K::ToDo, 2));
        }
        if nested && loop_ok && s.has(Shapes::LOOP_TIMES) {
            ks.push((K::Times, 2));
        }
        if nested && loop_ok && s.has(Shapes::LOOP_WHILE) {
            ks.push((K::While, 2));
        }
        if nested && s.has(Shapes::STORED_CLOSURE) {
            ks.push((K::Store, 2));
        }
        if !c.closures.is_empty() {
            ks.push((K::EvalStored, 2));
        }
        if s.has(Shapes::RAISE) {
            ks.push((K::Raise, if c.protected { 3 } else { 1 }));
        }
        if nested && s.has(Shapes::RETURN) && s.has(Shapes::COND) && c.can_return {
            ks.push((K::Return, 2));
        }
        let total: u64 = ks.iter().map(|(_, w)| w).sum();
        let mut pick = self.rng.below(total);
        let mut kind = K::Let;
        for (k, w) in &ks {
            if pick < *w {
                kind = *k;
                break;
            }
            pick -= w;
        }
        match kind {
            K::Write => Stmt::Write(self.cvar(), self.expr(c)),
            K::Let => {
                let v = self.expr(c);
                let name = self.name("v");
                c.scope.push(name.clone());
                Stmt::Let(name, v)
            }
            K::EvalSend => Stmt::Eval(self.send(c).expect("checked above")),
            K::EvalCompound => {
                let kinds = self.compound_kinds(c);
                let kind = kinds[self.rng.index(kinds.len())];
                Stmt::Eval(self.compound(c, kind))
            }
            K::If => {
                let cond = self.cond(c);
                let then = self.arm(c);
                let els = if self.rng.chance(40) {
                    Some(self.arm(c))
                } else {
                    None
                };
                Stmt::If { cond, then, els }
            }
            K::Do => {
                let items = self.items();
                let elem = self.name("e");
                let mut inner = Self::inner(c);
                inner.scope.push(elem.clone());
                inner.in_loop = true;
                let n = self.rng.range(1, 2);
                let body = self.stmts(&mut inner, n);
                Stmt::Do { items, elem, body }
            }
            K::ToDo => {
                let var = self.name("i");
                let hi = self.rng.range(1, 3);
                let mut inner = Self::inner(c);
                inner.scope.push(var.clone());
                inner.in_loop = true;
                let n = self.rng.range(1, 2);
                let body = self.stmts(&mut inner, n);
                Stmt::ToDo {
                    lo: 1,
                    hi,
                    var,
                    body,
                }
            }
            K::Times => {
                let k = self.rng.range(1, 3);
                let mut inner = Self::inner(c);
                inner.in_loop = true;
                let n = self.rng.range(1, 2);
                let body = self.stmts(&mut inner, n);
                Stmt::Times { k, body }
            }
            K::While => {
                let counter = self.name("i");
                let extra = if s.has(Shapes::SEND_IN_COND) && self.rng.chance(60) {
                    self.send(c)
                } else {
                    None
                };
                let mut inner = Self::inner(c);
                inner.scope.push(counter.clone());
                inner.in_loop = true;
                let n = self.rng.range(1, 2);
                let body = self.stmts(&mut inner, n);
                Stmt::While {
                    counter,
                    bound: self.rng.range(1, 3),
                    extra,
                    body,
                }
            }
            K::Store => {
                let mut inner = Self::inner(c);
                inner.can_return = false;
                inner.no_touch = true;
                let b = self.block(&inner, 0, 2);
                let name = self.name("b");
                c.closures.push(name.clone());
                Stmt::Store(name, b)
            }
            K::EvalStored => {
                let i = self.rng.index(c.closures.len());
                Stmt::Eval(Expr::CallStored(c.closures[i].clone()))
            }
            K::Raise => {
                if nested && s.has(Shapes::COND) && self.rng.chance(70) {
                    let cond = self.cond(c);
                    Stmt::If {
                        cond,
                        then: vec![Stmt::Raise],
                        els: None,
                    }
                } else {
                    Stmt::Raise
                }
            }
            K::Return => {
                let cond = self.cond(c);
                let value = self.expr(c);
                Stmt::If {
                    cond,
                    then: vec![Stmt::Return(value)],
                    els: None,
                }
            }
        }
    }

    /// A method body: a block that, with [`Shapes::LOCAL_TOUCH`], first
    /// declares the local its nested blocks bump.
    fn method_block(&mut self, c: &Ctx, min: i64, max: i64) -> Block {
        let mut b = self.block(c, min, max);
        if self.shapes.has(Shapes::LOCAL_TOUCH) {
            b.stmts
                .insert(0, Stmt::Let(TOUCH_LOCAL.to_string(), Expr::Lit(0)));
        }
        b
    }

    fn program(&mut self, size: u32) -> Program {
        let n_helpers = if self.shapes.has(Shapes::HELPER_SEND) {
            1 + self
                .rng
                .index(usize::try_from(size).expect("small size") + 1)
        } else {
            0
        };
        let mut helpers = Vec::new();
        for k in 0..n_helpers {
            let c = Ctx {
                scope: vec!["x".to_string()],
                closures: Vec::new(),
                helpers_below: k,
                depth: size.min(2),
                in_block: false,
                protected: false,
                can_return: true,
                no_touch: false,
                in_loop: false,
            };
            let b = self.method_block(&c, 1, 2);
            helpers.push(b);
        }
        let c = Ctx {
            scope: Vec::new(),
            closures: Vec::new(),
            helpers_below: n_helpers,
            depth: size.min(3),
            in_block: false,
            protected: false,
            can_return: true,
            no_touch: false,
            in_loop: false,
        };
        let run = self.method_block(&c, 2, 4);
        let overridden = (0..n_helpers).map(|_| self.rng.chance(60)).collect();
        Program {
            helpers,
            run,
            overridden,
        }
    }
}

/// Builds a program from `seed`. `size` (1..=3) scales helper count and
/// nesting depth. The result is always interpretable within the step budget:
/// an over-budget draw is replaced by the next derived seed.
#[must_use]
pub fn gen_program(seed: u64, size: u32, shapes: Shapes) -> Program {
    for attempt in 0..64u64 {
        let mut g = Gen {
            rng: Rng(seed ^ attempt.wrapping_mul(0xA24B_AED4_963E_E407)),
            shapes,
            fresh: 0,
        };
        let p = g.program(size);
        if p.interpret().is_some() {
            return p;
        }
    }
    // Practically unreachable; keeps `gen_program` total.
    Program {
        helpers: Vec::new(),
        run: Block {
            stmts: Vec::new(),
            tail: Box::new(Expr::Lit(0)),
        },
        overridden: Vec::new(),
    }
}

// ---------------------------------------------------------------------------
// Rendering
// ---------------------------------------------------------------------------

fn is_atom(e: &Expr) -> bool {
    matches!(e, Expr::Lit(_) | Expr::Local(_) | Expr::Cv(_))
}

/// An expression as source. `embed` wraps non-atomic forms in parentheses so
/// the text can be an operand or an argument.
fn expr_src(e: &Expr, embed: bool) -> String {
    let s = match e {
        Expr::Lit(v) => return v.to_string(),
        Expr::Local(n) => return n.clone(),
        Expr::Cv(v) => return format!("self.{}", v.name()),
        // Literal first: a line must never start with `(` (read as a continuation
        // of the previous line).
        Expr::Add(a, k) => format!("{k} + {}", expr_src(a, true)),
        Expr::Send { helper, arg } => format!("self h{helper}: {}", expr_src(arg, true)),
        Expr::OnDo {
            body,
            param,
            handler,
        } => format!(
            "{} on: Error do: [:{param} | {}]",
            block_src(body, ""),
            block_inner(handler)
        ),
        Expr::Ensure { body, cleanup } => {
            format!("{} ensure: {}", block_src(body, ""), block_src(cleanup, ""))
        }
        Expr::TryDo(body) => format!("(Result tryDo: {}) valueOr: 0", block_src(body, "")),
        Expr::Inject {
            items,
            init,
            acc,
            elem,
            body,
        } => format!(
            "{} inject: {} into: {}",
            array_src(items),
            expr_src(init, true),
            block_src(body, &format!(":{acc} :{elem} | "))
        ),
        Expr::CollectSum { items, elem, body } => format!(
            "({} collect: {}) inject: 0 into: [:sa :sx | sa + sx]",
            array_src(items),
            block_src(body, &format!(":{elem} | "))
        ),
        Expr::CallStored(n) => format!("{n} value"),
        Expr::Raise => "Error signal: \"boom\"".to_string(),
    };
    if embed { format!("({s})") } else { s }
}

fn array_src(items: &[i64]) -> String {
    let parts: Vec<String> = items.iter().map(ToString::to_string).collect();
    format!("#({})", parts.join(", "))
}

fn block_inner(b: &Block) -> String {
    let mut parts: Vec<String> = b.stmts.iter().map(stmt_src).collect();
    parts.push(expr_src(&b.tail, false));
    parts.join(". ")
}

/// `[params body]` where `params` is `":x :y | "` or empty.
fn block_src(b: &Block, params: &str) -> String {
    format!("[{params}{}]", block_inner(b))
}

fn stmts_src(ss: &[Stmt]) -> String {
    let parts: Vec<String> = ss.iter().map(stmt_src).collect();
    format!("[{}]", parts.join(". "))
}

fn stmts_src_with(param: &str, ss: &[Stmt]) -> String {
    let parts: Vec<String> = ss.iter().map(stmt_src).collect();
    format!("[:{param} | {}]", parts.join(". "))
}

fn cmp_src(c: &Cond) -> String {
    // The literal goes first so the statement never starts with `(` (a line
    // starting with a parenthesis is read as a continuation of the previous
    // statement): `3 > (self h0: 1) ifTrue: [...]`.
    let op = match c.op {
        Cmp::Lt => ">",
        Cmp::Gt => "<",
        Cmp::Eq => "=:=",
    };
    format!("{} {op} {}", c.rhs, expr_src(&c.lhs, !is_atom(&c.lhs)))
}

fn stmt_src(s: &Stmt) -> String {
    match s {
        Stmt::Write(v, e) => format!("self.{} := {}", v.name(), expr_src(e, false)),
        Stmt::Let(n, e) => format!("{n} := {}", expr_src(e, false)),
        Stmt::Eval(e) => expr_src(e, false),
        Stmt::Store(n, b) => format!("{n} := {}", block_src(b, "")),
        Stmt::If { cond, then, els } => match els {
            None => format!("{} ifTrue: {}", cmp_src(cond), stmts_src(then)),
            Some(els) => format!(
                "{} ifTrue: {} ifFalse: {}",
                cmp_src(cond),
                stmts_src(then),
                stmts_src(els)
            ),
        },
        Stmt::Do { items, elem, body } => {
            format!("{} do: {}", array_src(items), stmts_src_with(elem, body))
        }
        Stmt::ToDo { lo, hi, var, body } => {
            format!("{lo} to: {hi} do: {}", stmts_src_with(var, body))
        }
        Stmt::Times { k, body } => format!("{k} timesRepeat: {}", stmts_src(body)),
        Stmt::While {
            counter,
            bound,
            extra,
            body,
        } => {
            let lhs = match extra {
                Some(e) => format!("{counter} + {}", expr_src(e, true)),
                None => counter.clone(),
            };
            let mut parts: Vec<String> = body.iter().map(stmt_src).collect();
            parts.push(format!("{counter} := {counter} + 1"));
            format!(
                "{counter} := 0. [{lhs} < {bound}] whileTrue: [{}]",
                parts.join(". ")
            )
        }
        Stmt::Return(e) => format!("^{}", expr_src(e, false)),
        Stmt::Raise => "Error signal: \"boom\"".to_string(),
        Stmt::Touch(n) => format!("{n} := {n} + 1"),
    }
}

/// One generated class: its name and Beamtalk source.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClassSource {
    /// The class name (the file is `snake_case(name).bt`).
    pub name: String,
    /// The source text.
    pub source: String,
}

/// `Aaa`, `Aab`, ...: a letters-only word for program `index`, so class names
/// round-trip to file names through the snake-case rule (no digits).
fn word(index: usize) -> String {
    let letters: Vec<char> = ('a'..='z').collect();
    let a = letters[(index / 676) % 26];
    let b = letters[(index / 26) % 26];
    let c = letters[index % 26];
    format!("{}{b}{c}", a.to_ascii_uppercase())
}

/// `CvAbcOpen` -> `cv_abc_open`.
#[must_use]
pub fn snake_case(name: &str) -> String {
    let mut out = String::new();
    for (i, ch) in name.chars().enumerate() {
        if ch.is_ascii_uppercase() {
            if i > 0 {
                out.push('_');
            }
            out.push(ch.to_ascii_lowercase());
        } else {
            out.push(ch);
        }
    }
    out
}

const HEADER: &str = "// Copyright 2026 James Casey\n// SPDX-License-Identifier: Apache-2.0\n\n";

/// A method-body line must not start with `(`: the parser reads it as a
/// continuation of the previous line, silently changing the program. `prefix`
/// (`_ := ` for a statement, `^` for the value) makes it start with something else.
fn line_src(line: &str, prefix: &str) -> String {
    if line.starts_with('(') {
        format!("{prefix}{line}")
    } else {
        line.to_string()
    }
}

fn method_src(selector: &str, body: &Block) -> String {
    let mut s = format!("  class {selector} =>\n");
    for st in &body.stmts {
        let _ = writeln!(s, "    {}", line_src(&stmt_src(st), "_ := "));
    }
    let _ = writeln!(s, "    {}", line_src(&expr_src(&body.tail, false), "^"));
    s
}

fn class_header(superclass: &str, name: &str, sealed: bool) -> String {
    let kw = if sealed { "sealed " } else { "" };
    format!("{kw}{superclass} subclass: {name}\n  classState: n = 0\n  classState: m = 0\n\n")
}

impl Program {
    /// The entry class name for program `index` in `spelling`.
    #[must_use]
    pub fn entry_class(index: usize, spelling: Spelling) -> String {
        format!("Cv{}{}", word(index), spelling.suffix())
    }

    /// Renders the program as Beamtalk classes. The last class is the entry.
    #[must_use]
    pub fn render(&self, index: usize, spelling: Spelling) -> Vec<ClassSource> {
        let accessors = "  class n => self.n\n  class m => self.m\n\n";
        let helper_sel = |k: usize| format!("h{k}: x");
        match spelling {
            Spelling::Open | Spelling::Sealed => {
                let name = Self::entry_class(index, spelling);
                let mut src = String::from(HEADER);
                src.push_str(&class_header("Object", &name, spelling == Spelling::Sealed));
                src.push_str(accessors);
                for (k, h) in self.helpers.iter().enumerate() {
                    src.push_str(&method_src(&helper_sel(k), h));
                    src.push('\n');
                }
                src.push_str(&method_src("run", &self.run));
                vec![ClassSource { name, source: src }]
            }
            Spelling::Override => {
                let root = format!("Cv{}Root", word(index));
                let leaf = Self::entry_class(index, Spelling::Override);
                let mut base = String::from(HEADER);
                base.push_str(&class_header("Object", &root, false));
                base.push_str(accessors);
                for (k, h) in self.helpers.iter().enumerate() {
                    if self.overridden.get(k).copied().unwrap_or(false) {
                        let _ = write!(base, "  class {} => 0\n\n", helper_sel(k));
                    } else {
                        base.push_str(&method_src(&helper_sel(k), h));
                        base.push('\n');
                    }
                }
                base.push_str(&method_src("run", &self.run));
                let mut sub = String::from(HEADER);
                sub.push_str(&class_header(&root, &leaf, false));
                for (k, h) in self.helpers.iter().enumerate() {
                    if self.overridden.get(k).copied().unwrap_or(false) {
                        sub.push_str(&method_src(&helper_sel(k), h));
                        sub.push('\n');
                    }
                }
                vec![
                    ClassSource {
                        name: root,
                        source: base,
                    },
                    ClassSource {
                        name: leaf,
                        source: sub,
                    },
                ]
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Corpus-run helpers shared by the codegen and CLI properties
// ---------------------------------------------------------------------------

/// Programs per run: `CV_CORPUS_CASES`, or `default`.
#[must_use]
pub fn corpus_cases_from_env(default: usize) -> usize {
    std::env::var("CV_CORPUS_CASES")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(default)
}

/// Shapes per run: `CV_CORPUS_SHAPES` (names as in [`Shapes::names`], or `all`
/// / `none`), or `default`.
///
/// # Panics
///
/// Panics on an unknown shape name, so a typo does not silently measure the
/// wrong set.
#[must_use]
pub fn corpus_shapes_from_env(default: Shapes) -> Shapes {
    std::env::var("CV_CORPUS_SHAPES").ok().map_or(default, |s| {
        Shapes::parse(&s).unwrap_or_else(|| panic!("unknown shape in CV_CORPUS_SHAPES={s}"))
    })
}

/// Longest cause line [`normalize_cause`] keeps.
const MAX_CAUSE_CHARS: usize = 100;

/// A failure line reduced so equal causes group together in a report: digits
/// and anything inside `'...'` or `"..."` (names, offsets, versions) dropped,
/// leading punctuation trimmed, truncated.
#[must_use]
pub fn normalize_cause(line: &str) -> String {
    let line = line.trim_start_matches(|c: char| !c.is_alphanumeric());
    let mut out = String::new();
    let mut quote: Option<char> = None;
    for ch in line.chars() {
        match quote {
            Some(q) if ch == q => quote = None,
            Some(_) => {}
            None if ch == '\'' || ch == '"' => quote = Some(ch),
            None if ch.is_ascii_digit() => {}
            None => out.push(ch),
        }
    }
    out.trim().chars().take(MAX_CAUSE_CHARS).collect()
}

/// The files of one `BUnit` package that checks a batch of programs: fixture
/// classes under `test/fixtures/` and one `TestCase` under `test/`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Package {
    /// `(file name, source)` pairs for `test/fixtures/`.
    pub fixtures: Vec<(String, String)>,
    /// The program index each entry of `fixtures` belongs to (parallel).
    pub fixture_owner: Vec<usize>,
    /// `(file name, source)` of the `TestCase` file under `test/`.
    pub test: (String, String),
    /// Test method names, in `(program index, spelling)` order.
    pub test_names: Vec<(usize, Spelling, String)>,
}

/// Name of the generated `TestCase` class.
pub const TEST_CLASS: &str = "CvAgreementTest";

/// Renders `programs` (each tagged with its stable index) in every spelling
/// plus a `TestCase` asserting each spelling answers the reference
/// interpretation.
///
/// # Panics
///
/// Panics if a program is outside the interpreter step budget (the generator
/// never produces one).
#[must_use]
pub fn render_package(programs: &[(usize, Program)]) -> Package {
    let mut fixtures = Vec::new();
    let mut fixture_owner = Vec::new();
    let mut test = format!("{HEADER}TestCase subclass: {TEST_CLASS}\n\n");
    let mut test_names = Vec::new();
    for (index, p) in programs {
        let expected = p.interpret().expect("generated programs are in budget");
        for spelling in Spelling::ALL {
            for class in p.render(*index, spelling) {
                fixtures.push((format!("{}.bt", snake_case(&class.name)), class.source));
                fixture_owner.push(*index);
            }
            let entry = Program::entry_class(*index, spelling);
            let method = format!("test{entry}");
            let want = expected.result.unwrap_or(ERROR_MARKER);
            let _ = write!(
                test,
                "  {method} =>\n    r := [{entry} run] on: Error do: [:e | {ERROR_MARKER}]\n    \
                 self assert: r equals: {want}\n    \
                 self assert: {entry} n equals: {}\n    \
                 self assert: {entry} m equals: {}\n\n",
                expected.n, expected.m
            );
            test_names.push((*index, spelling, method));
        }
    }
    Package {
        fixtures,
        fixture_owner,
        test: (format!("{}.bt", snake_case(TEST_CLASS)), test),
        test_names,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lit(v: i64) -> Expr {
        Expr::Lit(v)
    }

    fn cv(v: CVar) -> Expr {
        Expr::Cv(v)
    }

    fn block(stmts: Vec<Stmt>, tail: Expr) -> Block {
        Block {
            stmts,
            tail: Box::new(tail),
        }
    }

    fn program(helpers: Vec<Block>, run: Block) -> Program {
        let overridden = vec![true; helpers.len()];
        Program {
            helpers,
            run,
            overridden,
        }
    }

    /// `n := n + 1`
    fn bump() -> Stmt {
        Stmt::Write(CVar::N, Expr::Add(Box::new(cv(CVar::N)), 1))
    }

    // why: compared directly against `Program::interpret`, which answers `Option`.
    #[allow(clippy::unnecessary_wraps)]
    fn outcome(result: Option<i64>, n: i64, m: i64) -> Option<Outcome> {
        Some(Outcome { result, n, m })
    }

    /// ADR 0130 "Error examples": `tryTake` answers 0 -- the write made inside
    /// the protected block is discarded when the error crosses the catch.
    #[test]
    fn adr_try_take_discards_write_inside_protected_block() {
        // h0: take => n := n + 1. Error signal: "boom". 0
        let take = block(vec![bump(), Stmt::Raise], lit(0));
        let run = block(
            vec![Stmt::Eval(Expr::OnDo {
                body: block(
                    vec![],
                    Expr::Send {
                        helper: 0,
                        arg: Box::new(lit(0)),
                    },
                ),
                param: "x0".to_string(),
                handler: block(vec![], lit(0)),
            })],
            cv(CVar::N),
        );
        assert_eq!(program(vec![take], run).interpret(), outcome(Some(0), 0, 0));
    }

    /// "a write made *before* entering the protected block is kept".
    #[test]
    fn adr_write_before_protected_block_is_kept() {
        let run = block(
            vec![
                bump(),
                Stmt::Eval(Expr::OnDo {
                    body: block(vec![bump(), Stmt::Raise], lit(0)),
                    param: "x0".to_string(),
                    handler: block(vec![], lit(0)),
                }),
            ],
            cv(CVar::N),
        );
        assert_eq!(program(vec![], run).interpret(), outcome(Some(1), 1, 0));
    }

    /// The handler runs after the restore, and its own writes are kept.
    #[test]
    fn handler_writes_are_kept() {
        let run = block(
            vec![Stmt::Eval(Expr::OnDo {
                body: block(vec![bump(), Stmt::Raise], lit(0)),
                param: "x0".to_string(),
                handler: block(vec![Stmt::Write(CVar::M, lit(7))], cv(CVar::N)),
            })],
            lit(3),
        );
        assert_eq!(program(vec![], run).interpret(), outcome(Some(3), 0, 7));
    }

    /// `Result tryDo:` is a catch boundary exactly like `on:do:`.
    #[test]
    fn try_do_is_a_catch_boundary() {
        let run = block(
            vec![
                Stmt::Let(
                    "v".to_string(),
                    Expr::TryDo(block(vec![bump(), Stmt::Raise], lit(9))),
                ),
                bump(),
            ],
            Expr::Add(Box::new(cv(CVar::N)), 0),
        );
        assert_eq!(program(vec![], run).interpret(), outcome(Some(1), 1, 0));
    }

    /// An error that escapes the invocation discards every write it made.
    #[test]
    fn escaping_error_discards_the_invocations_writes() {
        let run = block(vec![bump(), bump(), Stmt::Raise], lit(0));
        assert_eq!(program(vec![], run).interpret(), outcome(None, 0, 0));
    }

    /// "A `^` undoes nothing": a return from inside a protected block, a loop
    /// and an ensure body keeps every write made before it.
    #[test]
    fn return_undoes_nothing() {
        let run = block(
            vec![Stmt::Eval(Expr::OnDo {
                body: block(
                    vec![Stmt::Do {
                        items: vec![1, 2],
                        elem: "e".to_string(),
                        body: vec![
                            bump(),
                            Stmt::If {
                                cond: Cond {
                                    lhs: Expr::Local("e".to_string()),
                                    op: Cmp::Gt,
                                    rhs: 0,
                                },
                                then: vec![Stmt::Return(lit(40))],
                                els: None,
                            },
                        ],
                    }],
                    lit(0),
                ),
                param: "x0".to_string(),
                handler: block(vec![], lit(0)),
            })],
            lit(1),
        );
        assert_eq!(program(vec![], run).interpret(), outcome(Some(40), 1, 0));
    }

    /// `ensure:` is not a boundary: the cleanup's write is kept on the normal
    /// path, and goes with the error to the next boundary when one crosses.
    #[test]
    fn ensure_is_not_a_boundary() {
        let normal = block(
            vec![Stmt::Eval(Expr::Ensure {
                body: block(vec![], lit(1)),
                cleanup: block(vec![bump()], lit(0)),
            })],
            cv(CVar::N),
        );
        assert_eq!(program(vec![], normal).interpret(), outcome(Some(1), 1, 0));

        let raising = block(
            vec![Stmt::Eval(Expr::OnDo {
                body: block(
                    vec![Stmt::Eval(Expr::Ensure {
                        body: block(vec![Stmt::Raise], lit(1)),
                        cleanup: block(vec![bump()], lit(0)),
                    })],
                    lit(0),
                ),
                param: "x0".to_string(),
                handler: block(vec![], lit(0)),
            })],
            cv(CVar::N),
        );
        assert_eq!(program(vec![], raising).interpret(), outcome(Some(0), 0, 0));
    }

    /// ADR 0130 REPL session: `Counter run` answers 2 (the stored closure
    /// `[self hook]` is a no-op `hook`); with `hook` overridden to bump (the
    /// `LoudCounter` spelling) it answers 3.
    #[test]
    fn adr_repl_session_counter_and_loud_counter() {
        let make = |hook: Block| {
            // h0: hook => ...; h1: bump => n := n + 1
            let bump_helper = block(vec![bump()], cv(CVar::N));
            let run = block(
                vec![
                    Stmt::Store(
                        "b".to_string(),
                        block(
                            vec![],
                            Expr::Send {
                                helper: 0,
                                arg: Box::new(lit(0)),
                            },
                        ),
                    ),
                    Stmt::Do {
                        items: vec![1, 2, 3],
                        elem: "i".to_string(),
                        body: vec![Stmt::If {
                            cond: Cond {
                                lhs: Expr::Local("i".to_string()),
                                op: Cmp::Gt,
                                rhs: 1,
                            },
                            then: vec![Stmt::Eval(Expr::Send {
                                helper: 1,
                                arg: Box::new(lit(0)),
                            })],
                            els: None,
                        }],
                    },
                    Stmt::Eval(Expr::CallStored("b".to_string())),
                ],
                cv(CVar::N),
            );
            program(vec![hook, bump_helper], run)
        };
        let plain = make(block(vec![], lit(0)));
        assert_eq!(plain.interpret(), outcome(Some(2), 2, 0));
        // `hook` calling `bump` (helper 1) would need helper 1 < 0; the
        // override is spelled directly instead.
        let loud = make(block(vec![bump()], lit(0)));
        assert_eq!(loud.interpret(), outcome(Some(3), 3, 0));
    }

    /// A self-send in a `whileTrue:` condition runs once per test, including
    /// the final failing one.
    #[test]
    fn while_condition_send_runs_every_test() {
        let h0 = block(vec![bump()], lit(0));
        let run = block(
            vec![Stmt::While {
                counter: "i".to_string(),
                bound: 2,
                extra: Some(Expr::Send {
                    helper: 0,
                    arg: Box::new(lit(0)),
                }),
                body: vec![Stmt::Write(CVar::M, Expr::Add(Box::new(cv(CVar::M)), 1))],
            }],
            cv(CVar::N),
        );
        // i=0: cond (n=1) 0<2 -> body (m=1), i=1; cond (n=2) 1<2 -> body (m=2),
        // i=2; cond (n=3) 2<2 false.
        assert_eq!(program(vec![h0], run).interpret(), outcome(Some(3), 3, 2));
    }

    #[test]
    fn generator_is_deterministic_and_in_budget() {
        for seed in 0..200 {
            let a = gen_program(seed, 3, Shapes::all());
            let b = gen_program(seed, 3, Shapes::all());
            assert_eq!(a, b, "seed {seed}");
            assert!(a.interpret().is_some(), "seed {seed}");
        }
    }

    #[test]
    fn generator_respects_shapes() {
        for seed in 0..200 {
            let p = gen_program(seed, 3, Shapes::NONE);
            assert!(p.helpers.is_empty());
            let src = p.render(0, Spelling::Open)[0].source.clone();
            for kw in [
                "do:",
                "whileTrue:",
                "ifTrue:",
                "on:",
                "ensure:",
                "tryDo:",
                "signal:",
            ] {
                assert!(!src.contains(kw), "seed {seed}: {kw} in\n{src}");
            }
        }
    }

    #[test]
    fn generator_covers_every_shape() {
        let kinds = [
            "do:",
            "collect:",
            "inject:",
            "to:",
            "timesRepeat:",
            "whileTrue:",
            "ifTrue:",
            "ifFalse:",
            "on: Error do:",
            "ensure:",
            "tryDo:",
            "signal:",
            " value",
            "^",
            "self h0:",
        ];
        let mut seen = vec![false; kinds.len()];
        for seed in 0..400 {
            let p = gen_program(seed, 3, Shapes::all());
            let src: String = p
                .render(0, Spelling::Open)
                .into_iter()
                .map(|c| c.source)
                .collect();
            for (i, kw) in kinds.iter().enumerate() {
                seen[i] |= src.contains(kw);
            }
        }
        for (i, kw) in kinds.iter().enumerate() {
            assert!(seen[i], "no generated program contains {kw:?}");
        }
    }

    #[test]
    fn render_spellings() {
        let p = gen_program(7, 3, Shapes::all());
        let open = &p.render(3, Spelling::Open)[0];
        assert_eq!(open.name, format!("Cv{}Open", word(3)));
        assert!(open.source.contains("Object subclass: "));
        let sealed = &p.render(3, Spelling::Sealed)[0];
        assert!(sealed.source.contains("sealed Object subclass: "));
        let classes = p.render(3, Spelling::Override);
        assert_eq!(classes.len(), 2);
        assert!(
            classes[1]
                .source
                .contains(&format!("{} subclass: ", classes[0].name))
        );
    }

    /// `NAMED` is the single shape table: every entry is one distinct bit, the
    /// bits are contiguous (a shape constant defined but left out of `NAMED`
    /// would leave a gap), and `all()` is their union.
    #[test]
    fn shape_table_is_consistent() {
        let mut seen = 0u32;
        for (name, shape) in Shapes::NAMED {
            assert_eq!(shape.0.count_ones(), 1, "{name} is not a single bit");
            assert_eq!(seen & shape.0, 0, "{name} overlaps another shape");
            seen |= shape.0;
            assert_eq!(Shapes::parse(name), Some(shape), "{name} does not parse");
        }
        let n = u32::try_from(Shapes::NAMED.len()).unwrap();
        assert_eq!(seen, (1 << n) - 1, "shape bits are not contiguous");
        assert_eq!(Shapes::all().0, seen);
    }

    #[test]
    fn cause_normaliser_groups_names_and_numbers() {
        assert_eq!(
            normalize_cause("  ╰─▶ Cannot send 'h3:' to self at line 12"),
            "Cannot send  to self at line"
        );
        assert_eq!(normalize_cause("Open: rejected \"x\" 7"), "Open: rejected");
    }

    #[test]
    fn class_names_map_to_file_names() {
        assert_eq!(snake_case("CvAbcOpen"), "cv_abc_open");
        assert_eq!(snake_case(TEST_CLASS), "cv_agreement_test");
        assert_eq!(word(0), "Aaa");
        assert_eq!(word(27), "Abb");
    }

    /// Reproduction aid, not a check: prints the program a failure report
    /// names by `(seed, size)` in all three spellings, with the reference
    /// answer. `CV_SEED=<seed> CV_SIZE=<size> CV_SHAPES=<names|all> cargo test
    /// -p beamtalk-core --lib dump_program -- --ignored --nocapture`.
    #[test]
    #[ignore = "reproduction aid: prints a program for CV_SEED/CV_SIZE/CV_SHAPES"]
    fn dump_program() {
        let var = |k: &str| std::env::var(k).unwrap_or_else(|_| panic!("set {k}"));
        let seed: u64 = var("CV_SEED").parse().expect("CV_SEED is a u64");
        let size: u32 = var("CV_SIZE").parse().expect("CV_SIZE is a u32");
        let shapes = Shapes::parse(&var("CV_SHAPES")).expect("known CV_SHAPES");
        let p = gen_program(seed, size, shapes);
        for spelling in Spelling::ALL {
            for class in p.render(0, spelling) {
                println!("{}", class.source);
            }
        }
        println!("expected {:?}", p.interpret());
    }
}
