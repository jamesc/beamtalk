// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 statement-position probe matrix (BT-3753, BT-3841): the shapes,
//! the checked-in verdict table and the measured-answer table, shared by the
//! Phase 0 pin tests in `beamtalk-core` and the BEAM measurement harness in
//! `beamtalk-cli` (`cli_adr0131_statement_measure.rs`).
//!
//! **DDD Context:** Compilation (test support)

/// The verdict table: one line per `(context, container, target, kind)` cell,
/// `ok`, `bad`, `over` or `gap` (see `adr0131_statement_probe_pins`).
pub const PROBES_TSV: &str =
    include_str!("../semantic_analysis/tests/adr0131_statement_probes.tsv");

/// The measured-answer table (BT-3841): what each class, value-type and actor
/// cell answers when compiled with the Phase 0 position check bypassed and
/// run on a BEAM: `right` (the ADR-expected value), `wrong` (a different
/// answer or a raise) or `erlc` (the module does not compile). Regenerated
/// by `just measure-adr0131-statements`.
pub const MEASURED_TSV: &str =
    include_str!("../semantic_analysis/tests/adr0131_statement_measured.tsv");

/// One line of [`PROBES_TSV`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProbeRow {
    pub context: String,
    pub container: String,
    pub target: String,
    pub kind: String,
    pub verdict: String,
}

/// Parses a tab-separated table of `columns` columns (comments and blank
/// lines skipped).
fn rows(table: &str, columns: usize) -> impl Iterator<Item = Vec<&str>> {
    table
        .lines()
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .map(move |line| {
            let cols: Vec<&str> = line.split('\t').collect();
            assert_eq!(cols.len(), columns, "bad line {line:?}");
            cols
        })
}

/// Every line of [`PROBES_TSV`].
#[must_use]
pub fn probe_rows() -> Vec<ProbeRow> {
    rows(PROBES_TSV, 5)
        .map(|c| ProbeRow {
            context: c[0].into(),
            container: c[1].into(),
            target: c[2].into(),
            kind: c[3].into(),
            verdict: c[4].into(),
        })
        .collect()
}

/// The family of a statement-matrix cell, for the BT-3841 breakdown of the
/// shapes that do not answer right: each cell is in exactly one, by the first
/// rule that matches.
///
/// - `evaluated-block`: any statement inside a `[...] value` body (container
///   `val`);
/// - `block-value`: a `[...] value` statement itself;
/// - `enumeration`: `eachWithIndex:` / `do:separatedBy:`;
/// - `keyed-do`: `keysAndValuesDo:`;
/// - `detect-ifnone`: `detect:ifNone:` (search block or handler writes);
/// - `detect-plain`: a plain `detect:` (kind `det`);
/// - `satisfy`: `anySatisfy:`;
/// - `group-by`: `groupBy:` (BT-3897);
/// - `match-arm`: a `match:` arm (BT-3875);
/// - `other`: anything else (empty today).
#[must_use]
pub fn statement_family(container: &str, kind: &str) -> &'static str {
    if container == "val" {
        return "evaluated-block";
    }
    match kind {
        "value" => "block-value",
        "ewi" | "dsb" => "enumeration",
        "kv" => "keyed-do",
        "dinS" | "dinH" => "detect-ifnone",
        "det" => "detect-plain",
        "any" => "satisfy",
        "groupBy" => "group-by",
        "match" => "match-arm",
        _ => "other",
    }
}

/// The block-role group of a statement-matrix container: where the statement
/// sits (`method-body`, `arm`, `protected-body`, `handler`, `loop-body`,
/// `loop-condition`, `arm-loop-mix` for an arm around a loop or a loop around an arm,
/// `nested-loop` for a loop in a `do:` body or in another loop,
/// `evaluated-block`).
#[must_use]
pub fn container_group(container: &str) -> &'static str {
    match container {
        "top" => "method-body",
        "arm" | "armTF" | "armNil" | "armAnd" | "armNL" => "arm",
        "prot" | "protLast" | "ens" | "ensLast" | "protArm" => "protected-body",
        "handler" | "ensBlk" => "handler",
        "loop" | "timesC" | "toDoC" | "iter" | "coll" | "iterIter" => "loop-body",
        "cond" => "loop-condition",
        "armIter" | "iterArm" | "armLoop" | "loopArm" | "timesArm" => "arm-loop-mix",
        "loopIter" | "timesIter" | "loopLoop" | "timesLoop" | "iterLoop" | "collLoop"
        | "valLoop" | "iterTimes" | "collTimes" | "valTimes" => "nested-loop",
        "val" => "evaluated-block",
        "armProtIter" | "protIterArm" | "armIterIter" | "armProtArm" => "three-deep",
        _ => "other",
    }
}

/// How many times the probe statement runs in `container`: the number of
/// iterations of the loops around it (every looping role runs twice, so a
/// write that survives one iteration but not the next shows).
#[must_use]
pub fn container_runs(container: &str) -> i64 {
    match container {
        "loop" | "timesC" | "toDoC" | "iter" | "coll" | "armIter" | "iterArm" | "armLoop"
        | "armProtIter" | "protIterArm" | "loopArm" | "timesArm" | "valLoop" | "valTimes" => 2,
        "iterIter" | "armIterIter" | "loopIter" | "timesIter" | "loopLoop" | "timesLoop"
        | "iterLoop" | "collLoop" | "iterTimes" | "collTimes" => 4,
        _ => 1,
    }
}

/// What the probe `(container, target, kind)` answers when every construct
/// threads its writes: the kind's total in [`STATEMENT_PROBE_KINDS`], once per
/// run of the statement ([`container_runs`]). Two probes do not accumulate:
/// the `local` target ends in `t := u`, which overwrites `t` on every run, and
/// the `while` kind's guard `t < 2` makes a second run a no-op (unless the
/// write goes to the block-local `u`, `dlocal`).
///
/// # Panics
///
/// Panics on an unknown kind.
#[must_use]
pub fn expected_answer(container: &str, target: &str, kind: &str) -> i64 {
    let total = STATEMENT_PROBE_KINDS
        .iter()
        .find(|(k, _, _)| *k == kind)
        .unwrap_or_else(|| panic!("unknown kind {kind}"))
        .2;
    if target == "fresh" {
        return 0;
    }
    let accumulates = target != "local" && (kind != "while" || target == "dlocal");
    if accumulates {
        total * container_runs(container)
    } else {
        total
    }
}

/// The two probe matrices: the statement positions (`DENY_SET` side, a
/// construct as a statement of a block role) and the operand positions
/// (`ALLOW_SET` side, a construct as the value of an assignment, a receiver,
/// an argument, a cascade part, a literal element, a `match:` subject or an
/// interpolation).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Matrix {
    Statement,
    Operand,
}

impl Matrix {
    /// The probe body (one statement) of `(container, target, kind)`; for the
    /// operand matrix `target` is the position name.
    ///
    /// # Panics
    ///
    /// Panics on an unknown container, target or kind.
    #[must_use]
    pub fn body(self, kind: &str, container: &str, target: &str, cond: &str) -> String {
        match self {
            Matrix::Statement => statement_probe(kind, container, target, cond),
            Matrix::Operand => operand_probe(kind, container, target, cond),
        }
    }

    /// What the probe answers when every construct threads its writes.
    #[must_use]
    pub fn expected(self, container: &str, target: &str, kind: &str) -> i64 {
        match self {
            Matrix::Statement => expected_answer(container, target, kind),
            Matrix::Operand => expected_answer(container, "outer", kind),
        }
    }

    /// The measured-answer table of this matrix.
    #[must_use]
    pub fn measured_table(self) -> &'static str {
        match self {
            Matrix::Statement => MEASURED_TSV,
            Matrix::Operand => OPERAND_MEASURED_TSV,
        }
    }

    /// Every cell of the matrix measured on a BEAM:
    /// `(context, container, target, kind)`.
    #[must_use]
    pub fn cells(self) -> Vec<[String; 4]> {
        match self {
            Matrix::Statement => probe_rows()
                .into_iter()
                // The REPL rows (the only `gap` lines left) are not measured
                // (BT-3823).
                .filter(|r| r.context != "repl")
                .map(|r| [r.context, r.container, r.target, r.kind])
                .collect(),
            Matrix::Operand => {
                let mut cells = Vec::new();
                for context in ["class", "value", "actor"] {
                    for container in OPERAND_CONTAINERS {
                        for (position, _) in OPERAND_POSITIONS {
                            // A value-type method cannot assign a field.
                            if context == "value" && *position == "field" {
                                continue;
                            }
                            for (kind, _, _) in STATEMENT_PROBE_KINDS
                                .iter()
                                .filter(|(k, _, _)| !STATEMENT_ONLY_KINDS.contains(k))
                            {
                                cells.push([
                                    context.to_string(),
                                    (*container).to_string(),
                                    (*position).to_string(),
                                    (*kind).to_string(),
                                ]);
                            }
                        }
                    }
                }
                cells
            }
        }
    }

    /// Every line of this matrix's measured table.
    #[must_use]
    pub fn measured(self) -> Vec<[String; 5]> {
        rows(self.measured_table(), 5)
            .map(|c| {
                [
                    c[0].into(),
                    c[1].into(),
                    c[2].into(),
                    c[3].into(),
                    c[4].into(),
                ]
            })
            .collect()
    }
}

/// The measured-answer table of the operand matrix (BT-3841).
pub const OPERAND_MEASURED_TSV: &str =
    include_str!("../semantic_analysis/tests/adr0131_operand_measured.tsv");

/// The block roles of the operand matrix.
pub const OPERAND_CONTAINERS: &[&str] = &["top", "arm", "iter"];

/// The operand positions of the operand matrix: the construct `K` (a
/// statement kind of [`STATEMENT_PROBE_KINDS`] writing `t`) as the value of
/// an assignment (`assign`) or a field assignment (`field`), the receiver
/// (`recv`) or an argument (`arg`) of a send, the right operand of a binary
/// send (`operand`), a cascade part (`cascade`), a literal element
/// (`literal`), the subject of `match:` (`match`) and a string interpolation
/// (`interp`). Each is value-agnostic: it reads back only `t`.
pub const OPERAND_POSITIONS: &[(&str, &str)] = &[
    ("assign", "r := K"),
    ("field", "self.n := K"),
    ("recv", "(K) isNil"),
    ("arg", "#(0) includes: (K)"),
    ("operand", "0 == (K)"),
    ("cascade", "#(0) size; includes: (K); yourself"),
    ("literal", "#[1, (K)] size"),
    ("match", "(K) match: [_ -> 0]"),
    ("interp", "\"v {K}\" size"),
];

/// The probe of `kind` in operand position `position`, inside `container`
/// (`outer` target: the method-level `t`).
///
/// # Panics
///
/// Panics on an unknown kind, container or position.
#[must_use]
pub fn operand_probe(kind: &str, container: &str, position: &str, cond: &str) -> String {
    let (_, snippet, _) = STATEMENT_PROBE_KINDS
        .iter()
        .find(|(k, _, _)| *k == kind)
        .unwrap_or_else(|| panic!("unknown kind {kind}"));
    let (_, template) = STATEMENT_PROBE_CONTAINERS
        .iter()
        .find(|(c, _)| *c == container)
        .unwrap_or_else(|| panic!("unknown container {container}"));
    let (_, shape) = OPERAND_POSITIONS
        .iter()
        .find(|(p, _)| *p == position)
        .unwrap_or_else(|| panic!("unknown position {position}"));
    let construct = snippet.replace('X', "").replace('@', "t");
    let stmt = shape.replace('K', &construct);
    template.replace('S', &stmt).replace("%c", cond)
}

/// One method `<selector> c =>` (a class method in a `class` context) holding
/// the probe of `(container, target, kind)` of `matrix`; `c` is the condition,
/// `true` when run. `selector` is `probe:` or `probeN:`.
///
/// # Panics
///
/// Panics on an unknown container, target or kind.
#[must_use]
pub fn probe_method(
    matrix: Matrix,
    context: &str,
    selector: &str,
    container: &str,
    target: &str,
    kind: &str,
) -> String {
    let body = matrix.body(kind, container, target, "c");
    let prefix = if context == "class" { "class " } else { "" };
    format!("  {prefix}{selector} c =>\n    t := 0\n    {body}\n    t\n")
}

/// The source of a class named `name` (`context` is `class`, `value` or
/// `actor`) with `methods` (from [`probe_method`]).
///
/// # Panics
///
/// Panics on an unknown context.
#[must_use]
pub fn class_source(context: &str, name: &str, methods: &[String]) -> String {
    let header = match context {
        "class" => format!("Object subclass: {name}\n  classState: n = 0\n  class plain => 0\n"),
        "value" => format!("Value subclass: {name}\n  plain => 0\n"),
        "actor" => format!("Actor subclass: {name}\n  state: n = 0\n  plain => 0\n"),
        other => panic!("unknown context {other}"),
    };
    format!("{header}{}", methods.join("\n"))
}

/// A class named `name` with the one method `probe: c =>` holding the probe
/// of `(container, target, kind)`.
///
/// # Panics
///
/// Panics on an unknown context, container, target or kind.
#[must_use]
pub fn probe_class_source(
    context: &str,
    name: &str,
    container: &str,
    target: &str,
    kind: &str,
) -> String {
    class_source(
        context,
        name,
        &[probe_method(
            Matrix::Statement,
            context,
            "probe:",
            container,
            target,
            kind,
        )],
    )
}

/// The construct kinds of the statement-position probe matrix: each body,
/// with `@` for the local it writes and `X` for an extra statement inside its
/// block (the `stateful` variants), and how much it adds to that local.
/// `%c` is the condition: the method parameter `c`, or `4 =:= 4` at the REPL.
pub const STATEMENT_PROBE_KINDS: &[(&str, &str, i64)] = &[
    ("do", "#(1, 2) do: [:e | @ := @ + eX]", 3),
    ("while", "[@ < 2] whileTrue: [@ := @ + 1X]", 2),
    ("times", "2 timesRepeat: [@ := @ + 1X]", 2),
    ("toDo", "1 to: 2 do: [:i | @ := @ + iX]", 3),
    (
        "inject",
        "#(1, 2) inject: 0 into: [:a :x | @ := @ + xX. a + x]",
        3,
    ),
    ("collect", "#(1, 2) collect: [:x | @ := @ + xX]", 3),
    ("ifTrue", "%c ifTrue: [@ := @ + 1X]", 1),
    ("ifNil", "nil ifNil: [@ := @ + 1X]", 1),
    ("and", "%c and: [@ := @ + 1X. true]", 1),
    ("onDo", "[@ := @ + 1X] on: Error do: [:e | 0]", 1),
    ("ensure", "[@ := @ + 1X] ensure: [nil]", 1),
    ("value", "[@ := @ + 1X] value", 1),
    ("ewi", "#(1, 2) eachWithIndex: [:x :i | @ := @ + xX]", 3),
    (
        "dsb",
        "#(1, 2) do: [:x | @ := @ + xX] separatedBy: [@ := @ + 10]",
        13,
    ),
    (
        "kv",
        "#{#a => 1, #b => 2} keysAndValuesDo: [:k :w | @ := @ + wX]",
        3,
    ),
    (
        "dinH",
        "#(1, 2) detect: [:e | e > 5] ifNone: [@ := @ + 1X]",
        1,
    ),
    (
        "dinS",
        "#(1, 2) detect: [:e | @ := @ + 1X. e > 5] ifNone: [nil]",
        2,
    ),
    ("any", "#(1, 2) anySatisfy: [:e | @ := @ + 1X. e > 5]", 2),
    // Plain `detect:` that hits at the first element: the fold stops there, so
    // the block writes once (BT-3841; the miss that raises `not_found` is
    // covered by BUnit, a probe cannot answer after raising).
    ("det", "#(1, 2) detect: [:e | @ := @ + 1X. e > 0]", 1),
    // ADR 0133 Phase 3 residuals (BT-3909), statement matrix only
    // ([`STATEMENT_ONLY_KINDS`]): a `groupBy:` whose result is a receiver
    // (BT-3897, with the `fresh` target), and a `match:` arm (BT-3875).
    (
        "groupBy",
        "(#(1, 3) groupBy: [:e | @ := @ + eX. e > 2]) at: true",
        4,
    ),
    ("match", "3 match: [_ -> (@ := @ + 1X)]", 1),
];

/// The kinds of [`STATEMENT_PROBE_KINDS`] probed only in the statement
/// matrix: the operand matrix (`Matrix::Operand`) does not measure them.
pub const STATEMENT_ONLY_KINDS: &[&str] = &["groupBy", "match"];

/// The block roles of the statement-position probe matrix, with `S` for the
/// probed statement.
pub const STATEMENT_PROBE_CONTAINERS: &[(&str, &str)] = &[
    ("top", "S"),
    ("arm", "%c ifTrue: [S]"),
    ("armTF", "%c ifTrue: [S] ifFalse: [nil]"),
    ("armNil", "nil ifNil: [S]"),
    ("armAnd", "%c and: [S. true]"),
    ("armNL", "%c ifTrue: [S. 5]"),
    ("prot", "[S. 7] on: Error do: [:e | 0]"),
    ("protLast", "[S] on: Error do: [:e | 0]"),
    ("ens", "[S. 7] ensure: [nil]"),
    ("ensLast", "[S] ensure: [nil]"),
    ("handler", "[Error signal: \"x\"] on: Error do: [:e | S. 0]"),
    ("ensBlk", "[nil] ensure: [S]"),
    ("loop", "i := 0. [i < 2] whileTrue: [i := i + 1. S]"),
    ("timesC", "2 timesRepeat: [S]"),
    ("toDoC", "1 to: 2 do: [:z | S]"),
    ("iter", "#(1, 2) do: [:z | S]"),
    ("coll", "#(1, 2) collect: [:z | S. z]"),
    ("val", "[S] value"),
    ("armIter", "#(1, 2) do: [:z | %c ifTrue: [S]]"),
    ("iterArm", "%c ifTrue: [#(1, 2) do: [:z | S]]"),
    (
        "armLoop",
        "i := 0. [i < 2] whileTrue: [i := i + 1. %c ifTrue: [S]]",
    ),
    ("protArm", "%c ifTrue: [[S. 7] on: Error do: [:e | 0]]"),
    ("iterIter", "#(1, 2) do: [:y | #(1, 2) do: [:z | S]]"),
    // Three blocks deep (BT-3841 review): the roles `RIGHT_STATEMENTS` and the
    // coarse rows must agree on past depth two.
    (
        "armProtIter",
        "#(1, 2) do: [:z | [%c ifTrue: [S]] on: Error do: [:e | 0]]",
    ),
    (
        "protIterArm",
        "%c ifTrue: [#(1, 2) do: [:z | [S. 7] on: Error do: [:e | 0]]]",
    ),
    (
        "armIterIter",
        "#(1, 2) do: [:y | #(1, 2) do: [:z | %c ifTrue: [S]]]",
    ),
    (
        "armProtArm",
        "%c ifTrue: [[%c ifTrue: [S]] on: Error do: [:e | 0]]",
    ),
    // A loop as the innermost block (BT-3776 review): under an arm, a `do:`
    // body or another loop. The counters are `m` and `j`, which no kind's
    // block parameter shadows (`toDo` binds `i`, `kv` binds `k`).
    (
        "loopArm",
        "%c ifTrue: [m := 0. [m < 2] whileTrue: [m := m + 1. S]]",
    ),
    ("timesArm", "%c ifTrue: [2 timesRepeat: [S]]"),
    (
        "loopIter",
        "#(1, 2) do: [:z | m := 0. [m < 2] whileTrue: [m := m + 1. S]]",
    ),
    ("timesIter", "#(1, 2) do: [:z | 2 timesRepeat: [S]]"),
    (
        "loopLoop",
        "m := 0. [m < 2] whileTrue: [m := m + 1. j := 0. [j < 2] whileTrue: [j := j + 1. S]]",
    ),
    (
        "timesLoop",
        "m := 0. [m < 2] whileTrue: [m := m + 1. 2 timesRepeat: [S]]",
    ),
    // ... and the reverse (BT-3776 review 2): a `do:` body, a `collect:` body
    // or an evaluated block as the innermost block in a loop body.
    (
        "iterLoop",
        "m := 0. [m < 2] whileTrue: [m := m + 1. #(1, 2) do: [:z | S]]",
    ),
    (
        "collLoop",
        "m := 0. [m < 2] whileTrue: [m := m + 1. #(1, 2) collect: [:z | S. z]]",
    ),
    (
        "valLoop",
        "m := 0. [m < 2] whileTrue: [m := m + 1. [S] value]",
    ),
    ("iterTimes", "2 timesRepeat: [#(1, 2) do: [:z | S]]"),
    ("collTimes", "2 timesRepeat: [#(1, 2) collect: [:z | S. z]]"),
    ("valTimes", "2 timesRepeat: [[S] value]"),
    // A statement of a `whileTrue:` condition block (BT-3891, BT-3909).
    ("cond", "i := 0. [S. i < 2] whileTrue: [i := i + 1]"),
    // REPL-only roles.
    ("replLoop", "[t < 100] whileTrue: [S. t := t + 100]"),
];

/// The probe statement for `kind` in `container`. `target` says which local
/// the construct writes and how the block reads it back: `outer` writes the
/// method-level `t`; `local` writes a local `u` of the innermost block and
/// then sets `t := u`; `direct` first writes `t := t + 0` in the block;
/// `dlocal` writes `u` and then `t := t + u`; `fresh` binds a local `u` of
/// the construct's own block and writes no `t` (BT-3909); `cvar`, `selfsend` and
/// `cvarOuter` write `t` with a class-variable write or a self-send inside
/// the construct's block, or a class-variable write before it.
///
/// # Panics
///
/// Panics on an unknown kind, container or target.
#[must_use]
pub fn statement_probe(kind: &str, container: &str, target: &str, cond: &str) -> String {
    let (_, snippet, _) = STATEMENT_PROBE_KINDS
        .iter()
        .find(|(k, _, _)| *k == kind)
        .unwrap_or_else(|| panic!("unknown kind {kind}"));
    let (_, template) = STATEMENT_PROBE_CONTAINERS
        .iter()
        .find(|(c, _)| *c == container)
        .unwrap_or_else(|| panic!("unknown container {container}"));
    let extra = match target {
        "cvar" => ". self.n := 1",
        "selfsend" => ". self plain",
        "cvarOuter" | "outer" | "local" | "direct" | "dlocal" | "fresh" => "",
        other => panic!("unknown target {other}"),
    };
    let snippet = snippet.replace('X', extra);
    let stmt = match target {
        "local" => format!("u := 0. {}. t := u", snippet.replace('@', "u")),
        "dlocal" => format!("u := 0. {}. t := t + u", snippet.replace('@', "u")),
        "direct" => format!("t := t + 0. {}", snippet.replace('@', "t")),
        "cvarOuter" => format!("self.n := 1. {}", snippet.replace('@', "t")),
        "fresh" => snippet.replace("@ := @ + ", "u := ").replace('@', "u"),
        _ => snippet.replace('@', "t"),
    };
    template.replace('S', &stmt).replace("%c", cond)
}
