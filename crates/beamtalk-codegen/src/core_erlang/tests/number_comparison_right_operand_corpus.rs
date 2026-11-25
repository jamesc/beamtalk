// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0116 § Addendum (BT-5176), Phase 2b (BT-5210): a generated corpus of
//! inlined ordering comparisons (`< <= > >=`), receiver kind × right-operand
//! kind × position, compiled with the debug `ThreadedIr` verifier on, run on a
//! BEAM, and checked against a reference evaluation written in Rust.
//!
//! * **Receivers:** a number, float or character literal, a `String` literal;
//!   a parameter declared `Integer`, `Character`, `Integer | String` or
//!   `String`, an untyped one; `self` in `Integer`, `Float`, `Character` and
//!   `String` (BT-5213).
//! * **Right operands:** a proven numeric one (an integer, float or character
//!   literal), a `String` literal (BT-5213), an untyped parameter and a
//!   parameter declared `Integer` (tested, not trusted), each run with numbers
//!   and with non-numbers (`nil`, a symbol, a string, a list, a boolean).
//! * **Positions:** a plain expression, a loop condition, an `ifTrue:ifFalse:`
//!   receiver, a guard-return shape (`c ifTrue: [^a]. b`, BT-4452), and in an
//!   actor the cell bracket around a send and a field-writing loop condition
//!   (BT-4121).
//!
//! The runtime is replaced by stubs that record what reached it: the
//! reflected hook (`send_number_comparison/4`, answering `true`), the BT-3924
//! operator dispatch (`send/3`, answering `false`), and in an actor the cell
//! (`beamtalk_actor_state`), whose stored `n` each runtime call records, so a
//! hook that ran outside a bracket (or saw a stale cell) disagrees with the
//! reference. The reference is the addendum's rule: a number on the left and a
//! number on the right compare numerically; a number on the left and anything
//! else send the hook; a proven `String` on the left (a literal, `self` in
//! `String`) compares against a `String` and raises the frame-free
//! `beamtalk_ordering_type_error` for anything else (BT-5213); a raw binary on
//! the left (a declared or untyped receiver's `is_binary` arm) term-orders;
//! any other receiver dispatches the operator.

use super::value_type_fold_family_corpus::{core_module_name, run_erlang_probes};

/// A value at run time, as the probes pass it and the reference reads it.
#[derive(Clone, Copy, Debug)]
enum V {
    Int(i64),
    Float(f64),
    /// A string as a raw binary.
    Str(&'static str),
    /// A `String` literal: a tagged `{beamtalk_string, Binary}` tuple at run
    /// time (BT-4170), which the comparison BIF term-orders as a tuple.
    Tagged(&'static str),
    /// An atom (`nil`, `true`, a symbol).
    Atom(&'static str),
    /// A non-empty list.
    List,
}

impl V {
    fn erl(self) -> String {
        match self {
            V::Int(i) => i.to_string(),
            V::Float(f) => format!("{f:?}"),
            V::Str(s) => format!("<<\"{s}\">>"),
            V::Tagged(s) => format!("{{beamtalk_string,<<\"{s}\">>}}"),
            V::Atom(a) => format!("'{a}'"),
            V::List => "[1,2]".to_string(),
        }
    }

    fn number(self) -> Option<f64> {
        match self {
            #[allow(clippy::cast_precision_loss)]
            V::Int(i) => Some(i as f64),
            V::Float(f) => Some(f),
            _ => None,
        }
    }

    /// Erlang's term order rank, for the kinds the corpus uses.
    fn rank(self) -> u8 {
        match self {
            V::Int(_) | V::Float(_) => 0,
            V::Atom(_) => 1,
            V::Tagged(_) => 2,
            V::List => 3,
            V::Str(_) => 4,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Op {
    Lt,
    Le,
    Gt,
    Ge,
}

impl Op {
    fn src(self) -> &'static str {
        match self {
            Op::Lt => "<",
            Op::Le => "<=",
            Op::Gt => ">",
            Op::Ge => ">=",
        }
    }

    fn hook(self) -> &'static str {
        match self {
            Op::Lt => "lessThanFromNumber:",
            Op::Le => "lessOrEqualFromNumber:",
            Op::Gt => "greaterThanFromNumber:",
            Op::Ge => "greaterOrEqualFromNumber:",
        }
    }

    fn holds(self, ord: std::cmp::Ordering) -> bool {
        use std::cmp::Ordering::{Greater, Less};
        match self {
            Op::Lt => ord == Less,
            Op::Le => ord != Greater,
            Op::Gt => ord == Greater,
            Op::Ge => ord != Less,
        }
    }
}

const OPS: [Op; 4] = [Op::Lt, Op::Le, Op::Gt, Op::Ge];

/// What one comparison did: its answer and the runtime call it made, if any.
#[derive(Clone, Debug)]
enum Outcome {
    Bif(bool),
    /// The reflected hook was sent, with the left operand (as printed).
    Hook(&'static str, String),
    /// The operator was dispatched to the receiver.
    Send(&'static str),
    /// A proven `String` receiver raised the wrong-kind `type_error` term
    /// for the operator (BT-5213).
    TypeError(&'static str),
}

/// The addendum's rule (and BT-3924 / BT-4823 for the receiver).
fn reference(left: V, op: Op, right: V, receiver: Recv) -> Outcome {
    let left_number = left.number().filter(|_| receiver.number_arm_accepts(left));
    match (left_number, right.number()) {
        (Some(l), Some(r)) => Outcome::Bif(op.holds(l.partial_cmp(&r).expect("no NaN"))),
        (Some(_), None) => Outcome::Hook(op.hook(), left.erl()),
        (None, _) => match left {
            // Only a proven `String` receiver holds a tagged String.
            V::Tagged(l) => match right {
                V::Tagged(r) => Outcome::Bif(op.holds(l.cmp(r))),
                _ => Outcome::TypeError(op.src()),
            },
            V::Str(_) if receiver.string_arm_accepts() => {
                let ord = match (left, right) {
                    (V::Str(l), V::Str(r)) => l.cmp(r),
                    _ => left.rank().cmp(&right.rank()),
                };
                Outcome::Bif(op.holds(ord))
            }
            _ => Outcome::Send(op.src()),
        },
    }
}

impl Outcome {
    fn answer(&self) -> bool {
        match self {
            Outcome::Bif(b) => *b,
            Outcome::Hook(..) => true,
            Outcome::Send(_) => false,
            Outcome::TypeError(_) => unreachable!("a raise answers nothing"),
        }
    }

    /// The trace entry the stubs record, `cell` the actor's stored `n`.
    fn trace(&self, cell: &str) -> Option<String> {
        match self {
            Outcome::Bif(_) | Outcome::TypeError(_) => None,
            Outcome::Hook(sel, left) => Some(format!("{{hook,'{sel}',{left},{cell}}}")),
            Outcome::Send(op) => Some(format!("{{send,'{op}',{cell}}}")),
        }
    }
}

/// A receiver kind: its source, the parameter that carries it and the values
/// it is run with.
#[derive(Clone, Copy, Debug)]
struct Recv {
    name: &'static str,
    /// The receiver as written (`5`, `a`, `self`).
    src: &'static str,
    /// The declaration of `a` when the receiver is the parameter.
    param: Option<&'static str>,
    /// The class the method is compiled in (`self` receivers).
    host: Host,
    values: &'static [V],
    /// Which kind arms its guard has: `(number, string)`; a value outside
    /// both is dispatched.
    arms: (Arm, bool),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Arm {
    /// `is_number`
    Number,
    /// `is_integer` (a declared `Character`)
    Integer,
    None,
}

impl Recv {
    fn number_arm_accepts(self, v: V) -> bool {
        match self.arms.0 {
            Arm::Number => v.number().is_some(),
            Arm::Integer => matches!(v, V::Int(_)),
            Arm::None => false,
        }
    }

    fn string_arm_accepts(self) -> bool {
        self.arms.1
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Host {
    Value,
    Integer,
    Float,
    Character,
    String,
}

impl Host {
    fn header(self) -> &'static str {
        match self {
            Host::Value => "Value subclass: CmpProbe\n",
            Host::Integer => "Object subclass: Integer\n",
            Host::Float => "Object subclass: Float\n",
            Host::Character => "Object subclass: Character\n",
            Host::String => "Object subclass: String\n",
        }
    }

    /// The `Self` the probes pass.
    fn self_value(self) -> Option<V> {
        match self {
            Host::Value => None,
            Host::Integer => Some(V::Int(7)),
            Host::Float => Some(V::Float(2.5)),
            Host::Character => Some(V::Int(109)),
            Host::String => Some(V::Tagged("q")),
        }
    }
}

const RECEIVERS: &[Recv] = &[
    Recv {
        name: "int-literal",
        src: "5",
        param: None,
        host: Host::Value,
        values: &[V::Int(5)],
        arms: (Arm::Number, true),
    },
    Recv {
        name: "float-literal",
        src: "1.5",
        param: None,
        host: Host::Value,
        values: &[V::Float(1.5)],
        arms: (Arm::Number, true),
    },
    Recv {
        name: "char-literal",
        src: "$m",
        param: None,
        host: Host::Value,
        values: &[V::Int(109)],
        arms: (Arm::Number, true),
    },
    Recv {
        name: "string-literal",
        src: "\"m\"",
        param: None,
        host: Host::Value,
        values: &[V::Tagged("m")],
        arms: (Arm::None, true),
    },
    Recv {
        name: "declared-integer",
        src: "a",
        param: Some("a :: Integer"),
        host: Host::Value,
        values: &[V::Int(5), V::Atom("nil")],
        arms: (Arm::Number, false),
    },
    Recv {
        name: "declared-character",
        src: "a",
        param: Some("a :: Character"),
        host: Host::Value,
        values: &[V::Int(109), V::Atom("nil")],
        arms: (Arm::Integer, false),
    },
    Recv {
        name: "declared-union",
        src: "a",
        param: Some("a :: Integer | String"),
        host: Host::Value,
        values: &[V::Int(5), V::Str("m"), V::Atom("nil")],
        arms: (Arm::Number, true),
    },
    Recv {
        name: "declared-string",
        src: "a",
        param: Some("a :: String"),
        host: Host::Value,
        values: &[V::Str("m"), V::Int(5)],
        arms: (Arm::None, true),
    },
    Recv {
        name: "untyped",
        src: "a",
        param: Some("a"),
        host: Host::Value,
        values: &[V::Int(5), V::Float(1.5), V::Str("m"), V::Atom("nil")],
        arms: (Arm::Number, true),
    },
    Recv {
        name: "self-integer",
        src: "self",
        param: None,
        host: Host::Integer,
        values: &[V::Int(7)],
        arms: (Arm::Number, true),
    },
    Recv {
        name: "self-float",
        src: "self",
        param: None,
        host: Host::Float,
        values: &[V::Float(2.5)],
        arms: (Arm::Number, true),
    },
    Recv {
        name: "self-character",
        src: "self",
        param: None,
        host: Host::Character,
        values: &[V::Int(109)],
        arms: (Arm::Number, true),
    },
    Recv {
        name: "self-string",
        src: "self",
        param: None,
        host: Host::String,
        values: &[V::Tagged("q")],
        arms: (Arm::None, true),
    },
];

/// A right-operand kind: its source, the parameter that carries it and the
/// values it is run with.
#[derive(Clone, Copy, Debug)]
struct Right {
    name: &'static str,
    src: &'static str,
    param: Option<&'static str>,
    values: &'static [V],
}

const ALL_VALUES: &[V] = &[
    V::Int(4),
    V::Float(2.5),
    V::Int(109),
    V::Atom("nil"),
    V::Atom("sym"),
    V::Str("str"),
    V::List,
    V::Atom("true"),
];

const RIGHTS: &[Right] = &[
    Right {
        name: "int-literal",
        src: "3",
        param: None,
        values: &[V::Int(3)],
    },
    Right {
        name: "float-literal",
        src: "2.5",
        param: None,
        values: &[V::Float(2.5)],
    },
    Right {
        name: "char-literal",
        src: "$a",
        param: None,
        values: &[V::Int(97)],
    },
    Right {
        name: "string-literal",
        src: "\"a\"",
        param: None,
        values: &[V::Tagged("a")],
    },
    Right {
        name: "untyped",
        src: "b",
        param: Some("b"),
        values: ALL_VALUES,
    },
    Right {
        name: "declared-integer",
        src: "b",
        param: Some("b :: Integer"),
        values: &[V::Int(4), V::Atom("nil"), V::Str("str")],
    },
];

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Position {
    Plain,
    LoopCondition,
    IfTrueReceiver,
    GuardReturn,
    /// Actor: the comparison between two field writes (the cell bracket).
    ActorBracket,
    /// Actor: a field-writing loop whose condition is the comparison.
    ActorLoop,
}

impl Position {
    fn body(self, cmp: &str) -> String {
        match self {
            Position::Plain => cmp.to_string(),
            Position::LoopCondition => {
                format!("i := 0\n    [{cmp} and: [i < 2]] whileTrue: [i := i + 1]\n    i")
            }
            Position::IfTrueReceiver => format!("{cmp} ifTrue: [#yes] ifFalse: [#no]"),
            Position::GuardReturn => format!("{cmp} ifTrue: [^#yes]\n    #no"),
            Position::ActorBracket => {
                format!("self.n := self.n + 1\n    r := {cmp}\n    self.n := self.n + 10\n    r")
            }
            Position::ActorLoop => {
                format!("[{cmp} and: [self.n < 2]] whileTrue: [self.n := self.n + 1]\n    self.n")
            }
        }
    }

    fn actor(self) -> bool {
        matches!(self, Position::ActorBracket | Position::ActorLoop)
    }

    /// The probe's printed `{Result, Trace}` for one comparison `outcome`.
    fn expected(self, outcome: &Outcome) -> String {
        // A raise ends the method wherever the comparison stands.
        if let Outcome::TypeError(op) = outcome {
            return format!("{{{{type_error,'{op}'}},[]}}");
        }
        let answer = outcome.answer();
        let traces = |cells: &[&str]| -> String {
            cells
                .iter()
                .filter_map(|c| outcome.trace(c))
                .collect::<Vec<_>>()
                .join(",")
        };
        let (result, trace) = match self {
            Position::Plain => (answer.to_string(), traces(&["none"])),
            // Evaluated with `i` 0, 1 and 2 while it holds; once when not.
            Position::LoopCondition => {
                if answer {
                    ("2".to_string(), traces(&["none", "none", "none"]))
                } else {
                    ("0".to_string(), traces(&["none"]))
                }
            }
            Position::IfTrueReceiver | Position::GuardReturn => (
                if answer { "yes" } else { "no" }.to_string(),
                traces(&["none"]),
            ),
            // The cell holds `n` after the first write when the hook runs.
            Position::ActorBracket => (format!("{{{answer},11}}"), traces(&["1"])),
            Position::ActorLoop => {
                if answer {
                    ("{2,2}".to_string(), traces(&["0", "1", "2"]))
                } else {
                    ("{0,0}".to_string(), traces(&["0"]))
                }
            }
        };
        format!("{{{result},[{trace}]}}")
    }
}

const VALUE_POSITIONS: [Position; 4] = [
    Position::Plain,
    Position::LoopCondition,
    Position::IfTrueReceiver,
    Position::GuardReturn,
];

/// The actor receivers: a literal (proven), a declared and an untyped one.
fn actor_receivers() -> Vec<Recv> {
    RECEIVERS
        .iter()
        .filter(|r| matches!(r.name, "int-literal" | "declared-integer" | "untyped"))
        .copied()
        .collect()
}

/// One generated method and the probes that run it.
struct Method {
    host: &'static str,
    source: String,
    probes: Vec<(String, String, String)>,
}

/// The method's selector and Erlang call for the parameters it declares.
fn signature(index: usize, recv: Recv, right: Right) -> (String, Vec<bool>) {
    match (recv.param, right.param) {
        (Some(a), Some(b)) => (format!("m{index}: {a} with: {b}"), vec![true, true]),
        (Some(a), None) => (format!("m{index}: {a}"), vec![true, false]),
        (None, Some(b)) => (format!("m{index}: {b}"), vec![false, true]),
        (None, None) => (format!("m{index}"), vec![false, false]),
    }
}

fn erlang_selector(index: usize, params: &[bool]) -> String {
    match params.iter().filter(|p| **p).count() {
        0 => format!("m{index}"),
        1 => format!("m{index}:"),
        _ => format!("m{index}:with:"),
    }
}

fn methods() -> Vec<Method> {
    let mut out = Vec::new();
    let mut index = 0;
    let mut cases: Vec<(Recv, Position)> = RECEIVERS
        .iter()
        .flat_map(|r| VALUE_POSITIONS.iter().map(move |p| (*r, *p)))
        .collect();
    for r in actor_receivers() {
        cases.push((r, Position::ActorBracket));
        cases.push((r, Position::ActorLoop));
    }
    for (recv, position) in cases {
        for right in RIGHTS {
            for op in OPS {
                // Every operator in the plain position; two elsewhere.
                if position != Position::Plain && !matches!(op, Op::Lt | Op::Ge) {
                    continue;
                }
                index += 1;
                let cmp = format!("{} {} {}", recv.src, op.src(), right.src);
                let (sig, params) = signature(index, recv, *right);
                let source = format!("  {sig} =>\n    {}\n", position.body(&cmp));
                let selector = erlang_selector(index, &params);
                let host = if position.actor() {
                    "actor"
                } else {
                    match recv.host {
                        Host::Value => "value",
                        Host::Integer => "integer",
                        Host::Float => "float",
                        Host::Character => "character",
                        Host::String => "string",
                    }
                };
                let mut probes = Vec::new();
                for left in recv.values {
                    for value in right.values {
                        let mut args = Vec::new();
                        if params[0] {
                            args.push(left.erl());
                        }
                        if params[1] {
                            args.push(value.erl());
                        }
                        let self_term = recv
                            .host
                            .self_value()
                            .map_or_else(|| "Self".to_string(), V::erl);
                        let call = if position.actor() {
                            format!(
                                "bt_probe:observe(fun() -> 'bt@cmp_actor':'dispatch'('{selector}', [{}], 'self_ref', #{{n => 0}}) end)",
                                args.join(", ")
                            )
                        } else {
                            let module = format!("'bt@cmp_{host}'");
                            let mut all = vec![self_term];
                            all.extend(args);
                            format!(
                                "bt_probe:observe(fun() -> {module}:'{selector}'({}) end)",
                                all.join(", ")
                            )
                        };
                        let outcome = reference(*left, op, *value, recv);
                        let name = format!(
                            "{}|{}|{:?}|{}|{}|{}",
                            recv.name,
                            right.name,
                            position,
                            op.src(),
                            left.erl(),
                            value.erl()
                        )
                        .replace(['"', ' '], "");
                        probes.push((name, call, position.expected(&outcome)));
                    }
                }
                out.push(Method {
                    host,
                    source,
                    probes,
                });
            }
        }
    }
    out
}

/// The runtime stand-ins: every call into the runtime is recorded with the
/// actor cell's stored `n` (`none` outside an actor).
const STUBS: [(&str, &str); 3] = [
    (
        "beamtalk_message_dispatch",
        "-module(beamtalk_message_dispatch).\n\
         -export([send_number_comparison/4, send/3]).\n\
         cell() -> case get(bt_cell) of #{n := N} -> N; _ -> none end.\n\
         note(E) -> put(bt_trace, [E | case get(bt_trace) of undefined -> []; T -> T end]).\n\
         send_number_comparison(_R, Sel, [L], _Op) -> note({hook, Sel, L, cell()}), true.\n\
         send(_L, Op, [_R]) -> note({send, Op, cell()}), false.\n",
    ),
    (
        "beamtalk_actor_state",
        "-module(beamtalk_actor_state).\n\
         -export([store/1, take/0]).\n\
         store(S) -> put(bt_cell, S), ok.\n\
         take() -> get(bt_cell).\n",
    ),
    (
        "bt_probe",
        "-module(bt_probe).\n\
         -export([observe/1]).\n\
         observe(F) -> R = (catch F()), {norm(R), lists:reverse(case get(bt_trace) of undefined -> []; T -> T end)}.\n\
         norm({reply, R, St}) when is_map(St) -> {R, maps:get(n, St)};\n\
         norm({'EXIT', {{beamtalk_ordering_type_error, _, Op, _}, _}}) -> {type_error, Op};\n\
         norm(R) -> R.\n",
    ),
];

fn module_source(host: &str, methods: &[Method]) -> String {
    let header = match host {
        "actor" => "Actor subclass: CmpActor\n  state: n :: Integer = 0\n".to_string(),
        "value" => Host::Value.header().to_string(),
        "integer" => Host::Integer.header().to_string(),
        "float" => Host::Float.header().to_string(),
        "character" => Host::Character.header().to_string(),
        "string" => Host::String.header().to_string(),
        other => unreachable!("host {other}"),
    };
    let bodies: Vec<&str> = methods
        .iter()
        .filter(|m| m.host == host)
        .map(|m| m.source.as_str())
        .collect();
    format!("{header}\n{}", bodies.join("\n"))
}

/// The modules, named as the probes call them.
fn compile_hosts(methods: &[Method]) -> Vec<String> {
    ["value", "integer", "float", "character", "string", "actor"]
        .iter()
        .map(|host| {
            let name = format!("bt@cmp_{host}");
            let source = module_source(host, methods);
            super::assert_parses(&source);
            let core = super::codegen_named(&source, &name);
            assert_eq!(core_module_name(&core), name);
            core
        })
        .collect()
}

#[test]
fn the_corpus_covers_every_receiver_right_operand_and_position() {
    let methods = methods();
    let probes: usize = methods.iter().map(|m| m.probes.len()).sum();
    assert!(methods.len() >= 400, "only {} methods", methods.len());
    assert!(probes >= 2000, "only {probes} probes");
    for position in [
        "Plain",
        "LoopCondition",
        "IfTrueReceiver",
        "GuardReturn",
        "ActorBracket",
        "ActorLoop",
    ] {
        assert!(
            methods
                .iter()
                .flat_map(|m| &m.probes)
                .any(|(name, _, _)| name.contains(&format!("|{position}|"))),
            "no probe in {position}"
        );
    }
    // Every side of the rule is exercised: hooks, bare BIFs, dispatches and
    // wrong-kind raises.
    let expected: Vec<&String> = methods
        .iter()
        .flat_map(|m| &m.probes)
        .map(|p| &p.2)
        .collect();
    assert!(expected.iter().any(|e| e.contains("{hook,")));
    assert!(expected.iter().any(|e| e.contains("{send,")));
    assert!(expected.iter().any(|e| e.contains("{type_error,")));
    assert!(expected.iter().any(|e| e.ends_with(",[]}")));
}

/// Runs every probe of `methods` compiled from `cores` and answers the
/// disagreements with the reference, or `None` without `erlc`/`erl`.
fn disagreements(methods: &[Method], cores: &[String]) -> Option<Vec<String>> {
    let calls: Vec<(String, String)> = methods
        .iter()
        .flat_map(|m| &m.probes)
        .map(|(name, call, _)| (name.clone(), call.clone()))
        .collect();
    let core_refs: Vec<&str> = cores.iter().map(String::as_str).collect();
    let observed = run_erlang_probes(&core_refs, &STUBS, "Self = #{}", &calls)?;
    let mut mismatches = Vec::new();
    for method in methods {
        for (name, _, expected) in &method.probes {
            match observed.get(name) {
                Some(got) if got == expected => {}
                other => mismatches.push(format!(
                    "{name}: expected {expected}, ran to {other:?}\n{}",
                    method.source
                )),
            }
        }
    }
    Some(mismatches)
}

#[test]
fn generated_comparisons_agree_with_the_reference_when_run() {
    let methods = methods();
    // Debug codegen: the `ThreadedIr` verifier is on and panics on any finding.
    let cores = compile_hosts(&methods);
    let Some(mismatches) = disagreements(&methods, &cores) else {
        // `run_erlang_probes` already failed (CI) or noted the skip (local).
        return;
    };
    let probes: usize = methods.iter().map(|m| m.probes.len()).sum();
    assert!(
        mismatches.is_empty(),
        "{} of {probes} probes disagree with the reference:\n\n{}",
        mismatches.len(),
        mismatches
            .iter()
            .take(40)
            .cloned()
            .collect::<Vec<_>>()
            .join("\n")
    );
}

#[test]
fn a_bracket_classification_that_ignores_the_right_operand_disagrees_when_run() {
    // `5 < b` in an actor sends `b` its hook, user code that may reach the
    // actor's cell. Classified by its receiver alone (as before the addendum),
    // the hook runs unbracketed and sees no stored state: the actor rows with
    // a literal receiver and an unproven right operand disagree.
    let methods = methods();
    let cores = crate::core_erlang::fault::with(
        crate::core_erlang::fault::Fault::ComparisonBracketIgnoresRightOperand,
        || compile_hosts(&methods),
    );
    let Some(mismatches) = disagreements(&methods, &cores) else {
        return;
    };
    assert!(!mismatches.is_empty(), "the fault went unnoticed");
    assert!(
        mismatches
            .iter()
            .all(|m| m.starts_with("int-literal|") && m.contains("|Actor")),
        "only the literal-receiver actor rows should disagree:\n{}",
        mismatches.join("\n")
    );
}
