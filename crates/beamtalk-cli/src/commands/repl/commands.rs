// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Canonical REPL meta-command vocabulary (BT-3083).
//!
//! Single source of truth for the `:cmd` names, aliases, and help text that
//! both tab-completion (`helper.rs`) and dispatch (`classify_command` in
//! `mod.rs`) consult, so a command can never be offered by tab-completion
//! without a real dispatch path — or dispatch a command tab-completion
//! doesn't know about — the way `:actors`/`:kill`/`:inspect`/`:sessions`
//! (completable, no dispatch arm) and `:interrupt`/`:recheck` (dispatched,
//! not completable) drifted apart before this module existed.
//!
//! `:actors`, `:kill`, `:inspect`, and `:sessions` are deliberately **not**
//! in this table: `docs/development/surface-parity.md` records that actor
//! listing/termination/inspection and session listing are reached via
//! message-sends (`Workspace actors`, `anActor stop`) or are surface-specific
//! to non-CLI callers (`inspect` is agent-only; `sessions` belongs to the
//! transport handshake) — not REPL meta-commands. They were speculative
//! completion entries from before the `Workspace`/`Beamtalk` object model
//! existed to carry that capability (see the parity doc's "REPL surface"
//! section) and are removed here rather than wired up.

/// One REPL meta-command: its canonical spelling, short aliases, one-line
/// help text, and whether its (optional) argument is a class name or
/// Beamtalk expression that should route through the backend's
/// receiver-aware completion engine (BT-783) rather than plain command-word
/// completion.
pub(crate) struct ReplCommandSpec {
    pub(crate) name: &'static str,
    pub(crate) aliases: &'static [&'static str],
    #[allow(dead_code, reason = "documents the vocabulary; not yet rendered")]
    pub(crate) help: &'static str,
    pub(crate) takes_class_expr_arg: bool,
}

impl ReplCommandSpec {
    /// The canonical name followed by every alias.
    fn forms(&self) -> impl Iterator<Item = &'static str> {
        std::iter::once(self.name).chain(self.aliases.iter().copied())
    }

    /// True if `word` is exactly this command's canonical name or one of its
    /// aliases (not a prefix — the caller has already split on whitespace).
    pub(crate) fn is_form(&self, word: &str) -> bool {
        self.forms().any(|f| f == word)
    }

    /// If `line` is `"<form> <rest>"` for one of this command's forms,
    /// returns `rest` trimmed (possibly empty, e.g. for `":flush "`).
    /// Returns `None` if `line` doesn't start with one of this command's
    /// forms followed by a space.
    pub(crate) fn arg<'a>(&self, line: &'a str) -> Option<&'a str> {
        self.forms()
            .find_map(|f| line.strip_prefix(f)?.strip_prefix(' '))
            .map(str::trim)
    }
}

pub(crate) const HELP: ReplCommandSpec = ReplCommandSpec {
    name: ":help",
    aliases: &[":h", ":?"],
    help: "Show help, or `:help <Class>` / `:help <Class> <selector>` for docs",
    takes_class_expr_arg: true,
};

pub(crate) const EXIT: ReplCommandSpec = ReplCommandSpec {
    name: ":exit",
    aliases: &[":quit", ":q"],
    help: "Exit the REPL",
    takes_class_expr_arg: false,
};

pub(crate) const CLEAR: ReplCommandSpec = ReplCommandSpec {
    name: ":clear",
    aliases: &[],
    help: "Clear the current session's local bindings",
    takes_class_expr_arg: false,
};

pub(crate) const BINDINGS: ReplCommandSpec = ReplCommandSpec {
    name: ":bindings",
    aliases: &[":b"],
    help: "List the current session's local binding names",
    takes_class_expr_arg: false,
};

pub(crate) const SYNC: ReplCommandSpec = ReplCommandSpec {
    name: ":sync",
    aliases: &[":s"],
    help: "Sync the workspace with the project on disk (takes no arguments)",
    takes_class_expr_arg: false,
};

pub(crate) const UNLOAD: ReplCommandSpec = ReplCommandSpec {
    name: ":unload",
    aliases: &[],
    help: "Unload a class from the workspace: `:unload <ClassName>`",
    takes_class_expr_arg: true,
};

pub(crate) const TEST: ReplCommandSpec = ReplCommandSpec {
    name: ":test",
    aliases: &[":t"],
    help: "Run tests, optionally scoped to one class: `:test [<ClassName>]`",
    takes_class_expr_arg: true,
};

pub(crate) const SHOW_CODEGEN: ReplCommandSpec = ReplCommandSpec {
    name: ":show-codegen",
    aliases: &[":sc"],
    help: "Show generated Core Erlang for an expression: `:show-codegen <expr>`",
    takes_class_expr_arg: true,
};

pub(crate) const INTERRUPT: ReplCommandSpec = ReplCommandSpec {
    name: ":interrupt",
    aliases: &[":int"],
    help: "Cancel a running evaluation",
    takes_class_expr_arg: false,
};

pub(crate) const FLUSH: ReplCommandSpec = ReplCommandSpec {
    name: ":flush",
    aliases: &[],
    help: "Write pending ChangeLog entries to disk: `:flush [<Class>|#kind|#{#file => \"path\"}]`",
    takes_class_expr_arg: true,
};

pub(crate) const CHANGES: ReplCommandSpec = ReplCommandSpec {
    name: ":changes",
    aliases: &[],
    help: "Show the workspace ChangeLog",
    takes_class_expr_arg: false,
};

pub(crate) const DIRTY: ReplCommandSpec = ReplCommandSpec {
    name: ":dirty",
    aliases: &[],
    help: "Show per-class dirty (uncommitted) selectors",
    takes_class_expr_arg: false,
};

pub(crate) const RECHECK: ReplCommandSpec = ReplCommandSpec {
    name: ":recheck",
    aliases: &[],
    help: "Re-check the whole live image: `:recheck image`",
    takes_class_expr_arg: false,
};

/// The full REPL meta-command vocabulary. This is the single source both
/// `helper.rs` (tab-completion) and `mod.rs` (dispatch, via
/// `classify_command`) consult — see the module doc for why `:actors`,
/// `:kill`, `:inspect`, and `:sessions` are intentionally absent.
pub(crate) const REPL_COMMAND_TABLE: &[&ReplCommandSpec] = &[
    &HELP,
    &EXIT,
    &CLEAR,
    &BINDINGS,
    &SYNC,
    &UNLOAD,
    &TEST,
    &SHOW_CODEGEN,
    &INTERRUPT,
    &FLUSH,
    &CHANGES,
    &DIRTY,
    &RECHECK,
];

/// All command word forms (canonical name + aliases) flattened, for
/// tab-completion of bare `:cmd` prefixes.
pub(crate) fn all_forms() -> impl Iterator<Item = &'static str> {
    REPL_COMMAND_TABLE.iter().flat_map(|c| c.forms())
}

/// `"<form> "` prefixes for every command whose argument is a class name or
/// Beamtalk expression, so `:test Cou<TAB>` / `:flush Cou<TAB>` etc. route
/// through the backend's receiver-aware completion instead of plain
/// command-word completion.
pub(crate) fn class_expr_arg_prefixes() -> impl Iterator<Item = String> {
    REPL_COMMAND_TABLE
        .iter()
        .filter(|c| c.takes_class_expr_arg)
        .flat_map(|c| c.forms().map(|f| format!("{f} ")))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_form_matches_name_and_aliases() {
        assert!(HELP.is_form(":help"));
        assert!(HELP.is_form(":h"));
        assert!(HELP.is_form(":?"));
        assert!(!HELP.is_form(":he"));
        assert!(!HELP.is_form(":help "));
    }

    #[test]
    fn arg_extracts_trimmed_argument_for_any_form() {
        assert_eq!(TEST.arg(":test Counter"), Some("Counter"));
        assert_eq!(TEST.arg(":t Counter"), Some("Counter"));
        assert_eq!(TEST.arg(":test   Counter  "), Some("Counter"));
        assert_eq!(TEST.arg(":test"), None);
        assert_eq!(TEST.arg(":testXYZ"), None);
    }

    #[test]
    fn arg_returns_empty_string_for_trailing_space_only() {
        assert_eq!(FLUSH.arg(":flush "), Some(""));
    }

    #[test]
    fn every_table_entry_form_is_unique() {
        let forms: Vec<&str> = all_forms().collect();
        let mut seen = std::collections::HashSet::new();
        for f in &forms {
            assert!(seen.insert(*f), "duplicate REPL command form: {f}");
        }
    }

    #[test]
    fn dead_actor_session_completions_are_absent() {
        // BT-3083: these were offered by tab-completion with no dispatch arm
        // — surface-parity.md documents the real capability as message-sends
        // (`Workspace actors`, `anActor stop`) or as not REPL-exposed at all
        // (`inspect` is agent-only, `sessions` is transport-handshake-only).
        let forms: Vec<&str> = all_forms().collect();
        for dead in [":actors", ":a", ":kill", ":inspect", ":sessions"] {
            assert!(
                !forms.contains(&dead),
                "{dead} should not be a REPL command form"
            );
        }
    }

    #[test]
    fn interrupt_and_recheck_are_completable() {
        // BT-3083: these dispatched but were missing from tab-completion.
        let forms: Vec<&str> = all_forms().collect();
        assert!(forms.contains(&":interrupt"));
        assert!(forms.contains(&":int"));
        assert!(forms.contains(&":recheck"));
    }

    #[test]
    fn class_expr_arg_prefixes_cover_every_alias_of_arg_taking_commands() {
        let prefixes: Vec<String> = class_expr_arg_prefixes().collect();
        for expected in [
            ":help ",
            ":h ",
            ":? ",
            ":test ",
            ":t ",
            ":show-codegen ",
            ":sc ",
            ":unload ",
            ":flush ",
        ] {
            assert!(
                prefixes.iter().any(|p| p == expected),
                "missing class-expr-arg prefix: {expected}"
            );
        }
        // Commands that don't take a class/expr argument must not appear.
        for absent in [":sync ", ":s ", ":clear ", ":exit "] {
            assert!(
                !prefixes.iter().any(|p| p == absent),
                "unexpected class-expr-arg prefix: {absent}"
            );
        }
    }
}
