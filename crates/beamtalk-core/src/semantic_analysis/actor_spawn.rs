// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Statically known actor instances: `ActorClass spawn` sends and the locals
//! bound to them.
//!
//! **DDD Context:** Semantic Analysis
//!
//! Shared by the `class-state-abroad` validator
//! (`validators::class_state_abroad`) and the standalone `beamtalk-lint`
//! crate's `actor-state-abroad` lint, which both judge a block handed to an
//! actor (BT-4186).

use crate::ast::{Expression, ExpressionStatement};
use crate::ast_walker::walk_expression;
use crate::semantic_analysis::ClassHierarchy;
use std::collections::HashSet;

/// `ActorClass spawn` / `spawnWith:`: an instance of an actor. These are the
/// only `Actor` class methods that answer `Self` (all are sealed, so no
/// subclass redefines them). The named (`spawnAs:`, ...) and remote
/// (`spawnOn:`, `spawnWith:on:`, ...) variants answer `Result(Self, Error)`
/// and the monitored ones (`spawnMonitored`, ...) a `Tuple`, not an actor
/// (`actor.bt`, `generated_builtins.rs`); an actor behind `unwrap`/`value`
/// or `first` is missed.
///
/// `receiver` is an operand of the operand-normalised tree
/// (`ast::normalize`, BT-4059), so `(Worker) spawn` arrives already peeled; a
/// package-qualified class reference is not resolved and never matches.
#[must_use]
pub fn is_actor_spawn(hierarchy: &ClassHierarchy, receiver: &Expression, selector: &str) -> bool {
    matches!(selector, "spawn" | "spawnWith:")
        && matches!(
            receiver,
            Expression::ClassReference { name, package, .. }
                if package.is_none() && hierarchy.is_actor_subclass(name.name.as_str())
        )
}

/// Locals bound to an actor instance (`a := Worker spawn`) anywhere in `body`,
/// which must come from the operand-normalised tree. An assignment's
/// right-hand side is not an operand, so `a := (Worker spawn)` is peeled here.
#[must_use]
pub fn actor_locals(hierarchy: &ClassHierarchy, body: &[ExpressionStatement]) -> HashSet<String> {
    let mut actors = HashSet::new();
    for stmt in body {
        walk_expression(&stmt.expression, &mut |e| {
            let Expression::Assignment { target, value, .. } = e else {
                return;
            };
            let Expression::Identifier(local) = &**target else {
                return;
            };
            if let Expression::MessageSend {
                receiver, selector, ..
            } = value.unwrap_parens()
            {
                if is_actor_spawn(hierarchy, receiver, &selector.name()) {
                    actors.insert(local.name.to_string());
                }
            }
        });
    }
    actors
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic_analysis::test_helpers::parse_module;

    /// The actor locals of `Driver probe`, whose body is `body`.
    fn locals(body: &str) -> HashSet<String> {
        let src = format!(
            "Actor subclass: Worker\n  run => 1\n\nObject subclass: Plain\n  run => 1\n\n\
             Object subclass: Driver\n  class probe =>\n{body}"
        );
        let mut module = parse_module(&src);
        crate::ast::normalize::peel_operand_parens(&mut module);
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.expect("hierarchy builds");
        let driver = module
            .classes
            .iter()
            .find(|c| c.name.name == "Driver")
            .expect("Driver");
        actor_locals(&hierarchy, &driver.class_methods[0].body)
    }

    fn set(names: &[&str]) -> HashSet<String> {
        names.iter().map(|n| (*n).to_string()).collect()
    }

    #[test]
    fn plain_spawn_and_spawn_with_are_actors() {
        assert_eq!(
            locals("    a := Worker spawn\n    b := Worker spawnWith: #{#n => 1}\n    a\n"),
            set(&["a", "b"])
        );
    }

    #[test]
    fn parenthesised_receiver_is_an_actor() {
        assert_eq!(locals("    a := (Worker) spawn\n    a\n"), set(&["a"]));
    }

    #[test]
    fn named_and_remote_spawns_are_not_actors() {
        // BT-4187: these answer `Result(Self, Error)`.
        assert_eq!(
            locals(
                "    a := Worker spawnAs: #w\n    b := Worker spawnOn: n\n    \
                 c := Worker spawnAs: #w scope: #local\n    \
                 d := Worker spawnWith: #{#n => 1} on: n\n    \
                 e := Worker spawnWith: #{#n => 1} as: #w on: n\n    a\n"
            ),
            set(&[])
        );
    }

    #[test]
    fn non_actor_class_and_packaged_class_ref_are_not_actors() {
        assert_eq!(
            locals("    a := Plain spawn\n    b := pkg@Worker spawn\n    a\n"),
            set(&[])
        );
    }

    #[test]
    fn monitored_spawns_are_not_actors() {
        // `spawnMonitored` / `spawnMonitoredWith:` and (BT-4477) the
        // `spawnMonitoredAsync…` pair answer a `Tuple`.
        assert_eq!(
            locals(
                "    a := Worker spawnMonitored\n    \
                 b := Worker spawnMonitoredWith: #{#n => 1}\n    \
                 c := Worker spawnMonitoredAsync\n    \
                 d := Worker spawnMonitoredAsyncWith: #{#n => 1}\n    a\n"
            ),
            set(&[])
        );
    }

    #[test]
    fn only_spawn_and_spawn_with_answer_an_actor() {
        let module = parse_module("Actor subclass: Worker\n  run => 1\n\nWorker spawn\n");
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.expect("hierarchy builds");
        let Expression::MessageSend { receiver, .. } = &module.expressions[0].expression else {
            panic!("expected a send");
        };
        assert!(is_actor_spawn(&hierarchy, receiver, "spawn"));
        assert!(is_actor_spawn(&hierarchy, receiver, "spawnWith:"));
        for not_actor in [
            "spawnAs:",
            "spawnOn:",
            "spawnAs:on:",
            "spawnAs:scope:",
            "spawnWith:on:",
            "spawnWith:as:on:",
            "spawnWith:as:scope:",
            "spawnMonitored",
            "spawnMonitoredWith:",
            "spawnMonitoredAsync",
            "spawnMonitoredAsyncWith:",
            "new",
        ] {
            assert!(
                !is_actor_spawn(&hierarchy, receiver, not_actor),
                "{not_actor}"
            );
        }
    }
}
