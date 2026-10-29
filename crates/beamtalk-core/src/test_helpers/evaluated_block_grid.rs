// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4274: the generated BT-3981 evaluated-block grid, hosts x positions x
//! evaluated blocks x contexts, and the probe contexts it is spliced into.
//!
//! One generator, two proofs: the ADR 0131 validator accepts every cell
//! (`beamtalk-core`'s `evaluated_block_value_corpus_is_accepted_everywhere`)
//! and every cell compiles through the debug `ThreadedIr` verifier and `erlc`
//! (`beamtalk-codegen`'s `evaluated_block_grid_corpus`). The runtime answers are
//! the hand-written `BUnit` fixtures (`stdlib/test/bt3981evalblock_test.bt`).
//!
//! The probe contexts ([`in_class`], [`in_value`], [`in_actor`]) are the ones
//! the local-threading validator tests use for every body, so the grid's cells
//! are the same programs those tests check.

/// A probe context: the class side of an `Object` subclass, a value type's
/// instance side, or an actor's instance side.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Context {
    /// `Object subclass: CvA`, a class method.
    Class,
    /// `Value subclass: Vt`, an instance method.
    Value,
    /// `Actor subclass: Ac` (one field, `n`), an instance method.
    Actor,
}

impl Context {
    /// The class that holds the probe methods. A context's source has more
    /// than one class (the class context's `CvA` precedes the others), so a
    /// consumer that compiles one class per module (codegen) keeps this one.
    #[must_use]
    pub fn probe_class(self) -> &'static str {
        match self {
            Context::Class => "CvA",
            Context::Value => "Vt",
            Context::Actor => "Ac",
        }
    }
}

/// Every [`Context`], in the order the grid enumerates them.
pub const CONTEXTS: &[Context] = &[Context::Actor, Context::Class, Context::Value];

/// The evaluated blocks of the grid. Each of the first four binds a local,
/// writes a field or sends to `self` (`peek:` answers its argument), and every
/// one answers 3.
pub const BLOCKS: &[&str] = &[
    "[n := 3. n] value",
    "[n := 0. #(1, 2) do: [:y | n := n + y]. n] value",
    "[n := 1. n := n + 2. n] value",
    "[self peek: 3] value",
    "[3] value",
    "[:k | k] value: 3",
];

/// Where the evaluated block sits within its host.
pub const POSITIONS: &[&str] = &[
    "assigned", "last", "dropped", "nested", "return", "argument",
];

/// The construct the position's statements are the body of.
pub const HOSTS: &[&str] = &[
    "method",
    "arm",
    "collect",
    "protected",
    "stored",
    "ffi",
    "user",
];

/// The number of cells of the grid: hosts x positions x blocks x contexts.
pub const CELL_COUNT: usize = 7 * 6 * 6 * 3;

/// The probe method body of one host x position x block cell.
///
/// # Panics
///
/// Panics when `host` or `position` is not one of [`HOSTS`] / [`POSITIONS`].
#[must_use]
pub fn evaluated_block_program(host: &str, position: &str, block: &str) -> String {
    let at = match position {
        "assigned" => format!("r := {block}\nr"),
        "last" => block.to_string(),
        "dropped" => format!("{block}\n7"),
        "nested" => format!("[{block}] value"),
        "return" => format!("^{block}"),
        "argument" => format!("#(3) includes: {block}"),
        other => unreachable!("{other}"),
    };
    match host {
        "method" => at,
        "arm" => format!("true ifTrue: [{at}] ifFalse: [0]"),
        "collect" => format!("#(1) collect: [:z | {at}]"),
        "protected" => format!("[{at}] on: Error do: [:e | 0]"),
        "stored" => format!("b := [:z | {at}]\nb value: 1"),
        "ffi" => format!("(Erlang lists) map: [:z | {at}] with: #(1)"),
        "user" => format!("self ap: [{at}]"),
        other => unreachable!("{other}"),
    }
}

/// One cell of the grid.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Cell {
    /// One of [`HOSTS`].
    pub host: &'static str,
    /// One of [`POSITIONS`].
    pub position: &'static str,
    /// One of [`BLOCKS`].
    pub block: &'static str,
    /// The probe context.
    pub context: Context,
}

impl Cell {
    /// The probe method body ([`evaluated_block_program`]).
    #[must_use]
    pub fn body(&self) -> String {
        evaluated_block_program(self.host, self.position, self.block)
    }

    /// The cell as a whole module with one probe method, `probe`.
    #[must_use]
    pub fn source(&self) -> String {
        grid_source(self.context, &[("probe".to_string(), self.body())])
    }

    /// `host x position x block (context)`.
    #[must_use]
    pub fn label(&self) -> String {
        format!(
            "{} x {} x {} ({:?})",
            self.host, self.position, self.block, self.context
        )
    }
}

/// The cells of the grid whose host is one of `hosts`, in host, position,
/// block, context order.
#[must_use]
pub fn cells_of(hosts: &[&'static str]) -> Vec<Cell> {
    let mut cells = Vec::new();
    for &host in hosts {
        for &position in POSITIONS {
            for &block in BLOCKS {
                for &context in CONTEXTS {
                    cells.push(Cell {
                        host,
                        position,
                        block,
                        context,
                    });
                }
            }
        }
    }
    cells
}

/// Every cell of the grid ([`CELL_COUNT`] of them).
#[must_use]
pub fn cells() -> Vec<Cell> {
    cells_of(HOSTS)
}

/// A module in `context` whose probe methods are `probes` (a name and a body
/// each). The actor also defines `peek: v => v`, the self-send of
/// [`BLOCKS`]; every context defines a user higher-order `ap:` (the `user`
/// host). Many probes in one module let a codegen corpus run `erlc` once per
/// family rather than once per cell.
#[must_use]
pub fn grid_source(context: Context, probes: &[(String, String)]) -> String {
    let mut methods: Vec<String> = Vec::new();
    if context == Context::Actor {
        methods.push("  peek: v => v\n".to_string());
    }
    methods.extend(
        probes
            .iter()
            .map(|(name, body)| probe_method(context, name, body)),
    );
    module_source(context, &methods)
}

/// A class with a user higher-order class method, followed by `body` as its
/// class method `probe`.
#[must_use]
pub fn in_class(body: &str) -> String {
    module_source(
        Context::Class,
        &[probe_method(Context::Class, "probe", body)],
    )
}

/// A value type with a user higher-order method, followed by `body` as its
/// method `probe`.
#[must_use]
pub fn in_value(body: &str) -> String {
    module_source(
        Context::Value,
        &[probe_method(Context::Value, "probe", body)],
    )
}

/// An actor (one field, `n`) with a user higher-order method, followed by
/// `body` as its method `probe`.
#[must_use]
pub fn in_actor(body: &str) -> String {
    module_source(
        Context::Actor,
        &[probe_method(Context::Actor, "probe", body)],
    )
}

/// `body` indented as the statements of a method.
#[must_use]
pub fn indent(body: &str) -> String {
    body.lines()
        .map(|l| format!("    {l}"))
        .collect::<Vec<_>>()
        .join("\n")
}

/// The probe method `name`, a fresh local `t` and then `body`.
fn probe_method(context: Context, name: &str, body: &str) -> String {
    let side = if context == Context::Class {
        "class "
    } else {
        ""
    };
    format!("  {side}{name} =>\n    t := 0\n{}\n", indent(body))
}

/// The context's classes, followed by `methods` (each ending in a newline).
fn module_source(context: Context, methods: &[String]) -> String {
    let header = match context {
        Context::Class => "Object subclass: CvA\n  class ap: b => b value\n\n",
        Context::Value => {
            "Object subclass: CvA\n  class ap: b => b value\n\nValue subclass: Vt\n  ap: b => b value\n\n"
        }
        Context::Actor => {
            "Object subclass: CvA\n  class ap: b => b value\n\nActor subclass: Ac\n  state: n = 0\n\n  sealed ap: b => b value\n\n"
        }
    };
    format!("{header}{}", methods.join("\n"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_grid_has_every_cell_once() {
        let cells = cells();
        assert_eq!(cells.len(), CELL_COUNT);
        assert_eq!(
            CELL_COUNT,
            HOSTS.len() * POSITIONS.len() * BLOCKS.len() * CONTEXTS.len()
        );
    }

    /// The single-probe sources are the local-threading tests' templates.
    #[test]
    fn a_single_probe_actor_cell_defines_peek_before_probe() {
        let src = Cell {
            host: "method",
            position: "last",
            block: "[3] value",
            context: Context::Actor,
        }
        .source();
        assert_eq!(
            src,
            in_actor("[3] value").replace("  probe =>", "  peek: v => v\n\n  probe =>")
        );
        assert_eq!(
            in_class("x"),
            "Object subclass: CvA\n  class ap: b => b value\n\n  class probe =>\n    t := 0\n    x\n"
        );
    }
}
