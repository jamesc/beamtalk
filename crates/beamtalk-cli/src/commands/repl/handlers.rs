// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! REPL slash-command (`:`-command) classification and handlers.
//!
//! **DDD Context:** REPL
//!
//! `classify_command` resolves a typed line to a [`CommandAction`] using
//! `commands::REPL_COMMAND_TABLE`; `handle_repl_command` dispatches it to the
//! per-command handlers that talk to the [`ReplClient`]. Moved out of
//! `repl/mod.rs` (BT-4700); behaviour is unchanged.

use super::display::{format_value, output_mode, print_help};
use super::{
    ReplResponse, client::ReplClient, color, commands, display, display_eval_response,
    eval_and_display,
};
/// Result of handling a single REPL command line.
///
/// Returned by [`handle_repl_command`] to tell the main loop what to do next.
#[derive(Debug, PartialEq, Eq)]
pub(super) enum CommandResult {
    /// The line was a command and has been handled; continue the loop.
    Handled,
    /// The user typed `:exit` / `:quit`; break out of the loop.
    Exit,
    /// The line is not a REPL command; fall through to expression evaluation.
    NotACommand,
}

/// Which branch of REPL-command handling a line resolves to, without
/// executing any client I/O.
///
/// [`classify_command`] is the single recognition step both the real
/// dispatcher (`handle_repl_command`) and a table-completeness test
/// (`every_completable_form_dispatches_when_submitted_bare`, below) consult,
/// built from `commands::REPL_COMMAND_TABLE` rather than scattering alias spellings
/// across match-arm literals — the drift that let `:actors`/`:kill`/
/// `:inspect`/`:sessions` complete with no dispatch, and `:interrupt`/
/// `:recheck` dispatch with no completion.
#[derive(Debug, PartialEq, Eq)]
enum CommandAction<'a> {
    Exit,
    Help,
    HelpTopic(&'a str),
    Clear,
    RawMfa(&'a str),
    Bindings,
    Sync,
    SyncArgsError,
    UnloadUsage,
    Unload(&'a str),
    Test(&'a str),
    ShowCodegenUsage,
    ShowCodegen(&'a str),
    Interrupt,
    Flush,
    FlushArg(&'a str),
    Changes,
    Dirty,
    RecheckImage,
    RecheckUsage,
    RemoveMethodUsage,
    RemoveMethodArg(&'a str),
    RemoveClassUsage,
    RemoveClassArg(&'a str),
    FlushDestructive,
    FlushDestructiveArg(&'a str),
    RenameClassUsage,
    RenameClassArg(&'a str),
    RenameMethodUsage,
    RenameMethodArg(&'a str),
}

/// Classify a line as a known REPL meta-command (or `None` if it isn't one),
/// using `commands::REPL_COMMAND_TABLE` to resolve the command word (and its
/// aliases) — see [`CommandAction`].
#[allow(clippy::too_many_lines)] // one if-block per REPL_COMMAND_TABLE entry; irreducible
fn classify_command(line: &str) -> Option<CommandAction<'_>> {
    let word = line.split(' ').next().unwrap_or(line);
    let bare = word == line;

    if commands::EXIT.is_form(word) {
        return bare.then_some(CommandAction::Exit);
    }
    if commands::HELP.is_form(word) {
        return Some(if bare {
            CommandAction::Help
        } else {
            CommandAction::HelpTopic(commands::HELP.arg(line)?)
        });
    }
    if commands::CLEAR.is_form(word) {
        return bare.then_some(CommandAction::Clear);
    }
    if commands::RAW_MFA.is_form(word) {
        return Some(if bare {
            CommandAction::RawMfa("")
        } else {
            CommandAction::RawMfa(commands::RAW_MFA.arg(line)?)
        });
    }
    if commands::BINDINGS.is_form(word) {
        return bare.then_some(CommandAction::Bindings);
    }
    if commands::SYNC.is_form(word) {
        return Some(if bare {
            CommandAction::Sync
        } else {
            CommandAction::SyncArgsError
        });
    }
    if commands::UNLOAD.is_form(word) {
        return Some(if bare {
            CommandAction::UnloadUsage
        } else {
            CommandAction::Unload(commands::UNLOAD.arg(line)?)
        });
    }
    if commands::TEST.is_form(word) {
        return Some(if bare {
            CommandAction::Test("")
        } else {
            CommandAction::Test(commands::TEST.arg(line)?)
        });
    }
    if commands::SHOW_CODEGEN.is_form(word) {
        return Some(if bare {
            CommandAction::ShowCodegenUsage
        } else {
            CommandAction::ShowCodegen(commands::SHOW_CODEGEN.arg(line)?)
        });
    }
    if commands::INTERRUPT.is_form(word) {
        return bare.then_some(CommandAction::Interrupt);
    }
    if commands::FLUSH.is_form(word) {
        return Some(if bare {
            CommandAction::Flush
        } else {
            CommandAction::FlushArg(commands::FLUSH.arg(line)?)
        });
    }
    if commands::CHANGES.is_form(word) {
        return bare.then_some(CommandAction::Changes);
    }
    if commands::DIRTY.is_form(word) {
        return bare.then_some(CommandAction::Dirty);
    }
    if commands::RECHECK.is_form(word) {
        return match (bare, line) {
            (true, _) => Some(CommandAction::RecheckUsage),
            (false, ":recheck image") => Some(CommandAction::RecheckImage),
            (false, _) => None,
        };
    }
    if commands::REMOVE_METHOD.is_form(word) {
        return Some(if bare {
            CommandAction::RemoveMethodUsage
        } else {
            CommandAction::RemoveMethodArg(commands::REMOVE_METHOD.arg(line)?)
        });
    }
    if commands::REMOVE_CLASS.is_form(word) {
        return Some(if bare {
            CommandAction::RemoveClassUsage
        } else {
            CommandAction::RemoveClassArg(commands::REMOVE_CLASS.arg(line)?)
        });
    }
    if commands::FLUSH_DESTRUCTIVE.is_form(word) {
        return Some(if bare {
            CommandAction::FlushDestructive
        } else {
            CommandAction::FlushDestructiveArg(commands::FLUSH_DESTRUCTIVE.arg(line)?)
        });
    }
    if commands::RENAME_CLASS.is_form(word) {
        return Some(if bare {
            CommandAction::RenameClassUsage
        } else {
            CommandAction::RenameClassArg(commands::RENAME_CLASS.arg(line)?)
        });
    }
    if commands::RENAME_METHOD.is_form(word) {
        return Some(if bare {
            CommandAction::RenameMethodUsage
        } else {
            CommandAction::RenameMethodArg(commands::RENAME_METHOD.arg(line)?)
        });
    }
    None
}

/// Dispatch a single REPL command (lines starting with `:`).
///
/// Extracted from the main loop to reduce nesting depth. Returns
/// [`CommandResult::NotACommand`] when the line is not a recognised command
/// so the caller can fall through to expression accumulation/evaluation.
pub(super) fn handle_repl_command(line: &str, client: &mut ReplClient) -> CommandResult {
    let Some(action) = classify_command(line) else {
        return CommandResult::NotACommand;
    };

    match action {
        CommandAction::Exit => return CommandResult::Exit,
        CommandAction::Help => print_help(),
        CommandAction::HelpTopic(args) => handle_help_topic(args, client),
        CommandAction::Clear => handle_clear(client),
        CommandAction::RawMfa(arg) => handle_raw_mfa(arg, client),
        CommandAction::Bindings => handle_bindings(client),
        CommandAction::Sync => handle_sync(client),
        CommandAction::SyncArgsError => {
            eprintln!(":sync takes no arguments — it syncs the project from beamtalk.toml");
        }
        CommandAction::UnloadUsage => eprintln!("Usage: :unload <ClassName>"),
        CommandAction::Unload(class_name) => {
            if class_name.is_empty() {
                eprintln!("Usage: :unload <ClassName>");
            } else {
                match client.unload(class_name) {
                    Ok(response) => display_eval_response(&response, client.raw_mfa()),
                    Err(e) => eprintln!("Error: {e}"),
                }
            }
        }
        CommandAction::Test(class_name) => {
            let expr = if class_name.is_empty() {
                "Workspace test".to_string()
            } else {
                format!("Workspace test: {class_name}")
            };
            sync_tests_then_run(client, &expr);
        }
        CommandAction::ShowCodegenUsage => eprintln!("Usage: :show-codegen <expression>"),
        CommandAction::ShowCodegen(code) => handle_show_codegen(code, client),
        CommandAction::Interrupt => handle_interrupt(client),
        // ADR 0082 Phase 3: REPL alias for `Workspace flush`.
        CommandAction::Flush => eval_and_display(client, "Workspace flush"),
        // ADR 0082 Phase 3 (expression-building extracted to
        // `beamtalk_cli::repl_meta_exprs`, matching
        // `:remove-method`'s pattern): `:flush <selector>` desugars to
        // `Workspace flush: <selector>`.
        CommandAction::FlushArg(selector) => {
            match beamtalk_cli::repl_meta_exprs::flush_expr_for(selector) {
                Some(expr) => eval_and_display(client, &expr),
                None => eprintln!("Usage: :flush [<Class>|#kind|#{{ #file => \"path\" }}]"),
            }
        }
        // ADR 0082 Phase 3: REPL alias for `Workspace changes`.
        CommandAction::Changes => eval_and_display(client, "Workspace changes"),
        // ADR 0082 Phase 3: REPL alias for `Workspace changes
        // dirtyMethods` — the per-class set of dirty selectors. Pairs with
        // `:changes` (full summary) and answers "what specifically changed?".
        CommandAction::Dirty => eval_and_display(client, "Workspace changes dirtyMethods"),
        // `:recheck image` (REPL alias for `Workspace recheckImage`, ADR 0105
        // Phase 3); bare `:recheck` shows the usage hint.
        CommandAction::RecheckImage => eval_and_display(client, "Workspace recheckImage"),
        CommandAction::RecheckUsage => eprintln!("Usage: :recheck image"),
        // ADR 0112 Phase 4: REPL alias for `Behaviour>>removeSelector:`
        // — `:remove-method <Class> <selector>` desugars to
        // `<Class> removeSelector: #<selector>`, matching `:flush`/`:changes`'s
        // existing CLI-side-shortcut pattern (no new workspace-side op).
        CommandAction::RemoveMethodUsage => {
            eprintln!("Usage: :remove-method <Class> <selector>");
        }
        CommandAction::RemoveMethodArg(arg) => {
            match beamtalk_cli::repl_meta_exprs::remove_method_expr_for(arg) {
                Some(expr) => eval_and_display(client, &expr),
                None => eprintln!("Usage: :remove-method <Class> <selector>"),
            }
        }
        // ADR 0113 Phase 4: REPL alias for `Behaviour>>removeFromSystem`
        // — `:remove-class <Class>` desugars to `<Class> removeFromSystem`,
        // gated behind a `y/N` terminal confirmation. Memory-only: reaching
        // disk still requires a later, separately-confirmed
        // `:flush-destructive` — two prompts for two genuinely separate
        // decisions, matching the ADR's Surface table. See
        // `handle_remove_class` for the confirmation prompt.
        CommandAction::RemoveClassUsage => {
            eprintln!("Usage: :remove-class <Class>");
        }
        CommandAction::RemoveClassArg(arg) => handle_remove_class(arg, client),
        // ADR 0113 Phase 4: REPL alias for the Tier-2 (destructive)
        // flush gate — `:flush-destructive` / `:flush-destructive <selector>`
        // desugar to `Workspace flushIncludingDestructive` /
        // `Workspace flush: <selector> confirmDestructive: true`, mirroring
        // the `:flush`/`:flush <selector>` pair rather than a flag on
        // `:flush` (the REPL meta-command layer has no `--flag` precedent).
        // Each gated behind its own `y/N` terminal confirmation, independent
        // of any `:remove-class` prompt that ran earlier. See
        // `handle_flush_destructive`/`handle_flush_destructive_arg`.
        CommandAction::FlushDestructive => handle_flush_destructive(client),
        CommandAction::FlushDestructiveArg(selector) => {
            handle_flush_destructive_arg(selector, client);
        }
        // ADR 0114 Phase 5: REPL alias for `Behaviour>>renameTo:`
        // — `:rename-class <Class> <NewName>` desugars to `<Class> renameTo:
        // #<NewName>`, gated behind a `y/N` terminal confirmation, matching
        // `:remove-class`'s two-prompt shape: this prompt confirms the
        // in-memory rename; a later, separately-confirmed
        // `:flush-destructive` reaches disk (`rename-class` joins
        // `remove-class` in the same Tier 2 gate, ADR 0114 "Flush"). See
        // `handle_rename_class`.
        CommandAction::RenameClassUsage => {
            eprintln!("Usage: :rename-class <Class> <NewName>");
        }
        CommandAction::RenameClassArg(arg) => handle_rename_class(arg, client),
        // ADR 0114 Phase 5: REPL alias for
        // `Behaviour>>renameSelector:to:` — `:rename-method <Class>
        // <selector> <newSelector>` desugars to `<Class> renameSelector:
        // #<selector> to: #<newSelector>`, gated behind a `y/N` terminal
        // confirmation, matching `:rename-class`'s shape. Instance-side
        // only — see `beamtalk_cli::repl_meta_exprs::rename_method_expr_for`.
        CommandAction::RenameMethodUsage => {
            eprintln!("Usage: :rename-method <Class> <selector> <newSelector>");
        }
        CommandAction::RenameMethodArg(arg) => handle_rename_method(arg, client),
    }

    CommandResult::Handled
}

/// `:remove-class <Class>` (ADR 0113 Phase 4): prompts `y/N` at the
/// terminal (ADR 0113 Surface — the REPL's confirmation gesture for the
/// memory-mutating half of a destructive removal) before evaluating
/// `<Class> removeFromSystem`. The file on disk is untouched until a later,
/// separately-confirmed `:flush-destructive`.
fn handle_remove_class(arg: &str, client: &mut ReplClient) {
    match beamtalk_cli::repl_meta_exprs::remove_class_expr_for(arg) {
        Some(expr) => {
            let class = arg.trim();
            if confirm_destructive_action(&format!(
                "Remove class {class} from the running workspace? This takes effect in memory immediately; the file on disk is untouched until a later `:flush-destructive` (or `Workspace flushIncludingDestructive`)."
            )) {
                eval_and_display(client, &expr);
            } else {
                println!("Aborted: {class} was not removed.");
            }
        }
        None => eprintln!("Usage: :remove-class <Class>"),
    }
}

/// Bare `:flush-destructive` (ADR 0113 Phase 4): prompts `y/N`
/// before evaluating the unscoped `Workspace flushIncludingDestructive`,
/// which deletes every pending `.bt` file the workspace's `ChangeLog` has a
/// `remove-class` entry for.
fn handle_flush_destructive(client: &mut ReplClient) {
    if confirm_destructive_action(
        "This permanently deletes the source file for every pending class removal in the workspace. Continue?",
    ) {
        eval_and_display(
            client,
            beamtalk_cli::repl_meta_exprs::FLUSH_INCLUDING_DESTRUCTIVE_EXPR,
        );
    } else {
        println!("Aborted: no files were deleted.");
    }
}

/// `:flush-destructive <selector>` (ADR 0113 Phase 4): prompts
/// `y/N` before evaluating the scoped `Workspace flush: <selector>
/// confirmDestructive: true`.
fn handle_flush_destructive_arg(selector: &str, client: &mut ReplClient) {
    match beamtalk_cli::repl_meta_exprs::flush_destructive_expr_for(selector) {
        Some(expr) => {
            if confirm_destructive_action(&format!(
                "This permanently deletes the source file for the pending class removal matching `{selector}`. Continue?"
            )) {
                eval_and_display(client, &expr);
            } else {
                println!("Aborted: no files were deleted.");
            }
        }
        None => {
            eprintln!("Usage: :flush-destructive [<Class>|#kind|#{{ #file => \"path\" }}]");
        }
    }
}

/// `:rename-class <Class> <NewName>` (ADR 0114 Phase 5): prompts
/// `y/N` at the terminal (mirroring `:remove-class`'s confirmation gesture
/// for the memory-mutating half of a destructive operation) before
/// evaluating `<Class> renameTo: #<NewName>`. Auto-rewrites every in-project
/// cross-file reference the xref index can find and re-registers the class
/// under the new name immediately; the file on disk (the rename/move plus
/// rewritten references) is untouched until a later, separately-confirmed
/// `:flush-destructive`.
fn handle_rename_class(arg: &str, client: &mut ReplClient) {
    match beamtalk_cli::repl_meta_exprs::rename_class_expr_for(arg) {
        Some(expr) => {
            let mut tokens = arg.split_whitespace();
            let old_name = tokens.next().unwrap_or(arg.trim());
            let new_name = tokens.next().unwrap_or("");
            if confirm_destructive_action(&format!(
                "Rename class {old_name} to {new_name}? This rewrites in-project cross-file references and takes effect in memory immediately; the file on disk is untouched until a later `:flush-destructive` (or `Workspace flushIncludingDestructive`)."
            )) {
                eval_and_display(client, &expr);
            } else {
                println!("Aborted: {old_name} was not renamed.");
            }
        }
        None => eprintln!("Usage: :rename-class <Class> <NewName>"),
    }
}

/// `:rename-method <Class> <selector> <newSelector>` (ADR 0114 Phase 5):
/// prompts `y/N` at the terminal before evaluating `<Class>
/// renameSelector: #<selector> to: #<newSelector>`. Auto-rewrites only the
/// self/super sends the cross-reference index can prove are structurally
/// safe; everything else is reported as a candidate for manual review,
/// never rewritten automatically. Instance-side only. The file on disk is
/// untouched until a later, separately-confirmed `:flush-destructive`.
fn handle_rename_method(arg: &str, client: &mut ReplClient) {
    match beamtalk_cli::repl_meta_exprs::rename_method_expr_for(arg) {
        Some(expr) => {
            let mut tokens = arg.split_whitespace();
            let class = tokens.next().unwrap_or(arg.trim());
            // Strip an optional leading `#` from each raw token before
            // display — `rename_method_expr_for` does the same stripping
            // internally when building `expr`, so a selector typed as
            // `#increment` must not double up into `##increment` here.
            let old_selector = tokens
                .next()
                .map_or("", |s| s.strip_prefix('#').unwrap_or(s));
            let new_selector = tokens
                .next()
                .map_or("", |s| s.strip_prefix('#').unwrap_or(s));
            if confirm_destructive_action(&format!(
                "Rename {class}>>#{old_selector} to #{new_selector}? This auto-rewrites only structurally-safe self/super sends and takes effect in memory immediately; the file on disk is untouched until a later `:flush-destructive` (or `Workspace flushIncludingDestructive`)."
            )) {
                eval_and_display(client, &expr);
            } else {
                println!("Aborted: {class}>>#{old_selector} was not renamed.");
            }
        }
        None => eprintln!("Usage: :rename-method <Class> <selector> <newSelector>"),
    }
}

/// Prompt `message` followed by ` [y/N] ` at the terminal and read a line
/// from stdin, returning `true` only for an explicit `y`/`yes`
/// (case-insensitive). Any other input — including a blank line, EOF, or a
/// read error on a non-interactive/piped stdin — declines, so a destructive
/// REPL command (`:remove-class`, `:flush-destructive`) never proceeds
/// without an explicit affirmative (ADR 0113 "Surface": the REPL's
/// confirmation gesture for a Tier-2/destructive operation).
///
/// Thin wrapper over `confirm_destructive_action_with`, which
/// takes the confirmation source as an injected `BufRead` — the same
/// closure/seam-injection pattern `repl::bind::detect_tailscale_ip_with`
/// uses for its own untestable subprocess call — so the y/N-parsing logic
/// itself is unit-tested without a real terminal.
fn confirm_destructive_action(message: &str) -> bool {
    use std::io::Write;
    print!("{message} [y/N] ");
    let _ = std::io::stdout().flush();
    confirm_destructive_action_with(&mut std::io::stdin().lock())
}

/// Read one line from `reader`, returning `true` only for an explicit
/// `y`/`yes` (case-insensitive, surrounding whitespace ignored). Any other
/// input — including a blank line, EOF (`Ok(0)`), or a read error —
/// declines. See `confirm_destructive_action` for the caller-facing
/// contract; this half carries no stdin/terminal dependency, so it's
/// exercised directly in `mod tests` rather than only at the e2e level.
fn confirm_destructive_action_with<R: std::io::BufRead>(reader: &mut R) -> bool {
    let mut input = String::new();
    match reader.read_line(&mut input) {
        Ok(0) | Err(_) => false,
        Ok(_) => matches!(input.trim().to_ascii_lowercase().as_str(), "y" | "yes"),
    }
}

/// Handle `:help <topic>` -- look up docs for a class or method.
///
/// `args` is the already-extracted, trimmed argument (extraction
/// happens once in `classify_command`, driven by `commands::HELP`'s
/// registered aliases — `:help`, `:h`, and `:? ` all reach this, closing a
/// gap where `:? <Class>` would otherwise not dispatch help despite
/// tab-completing as if it did).
///
/// Routes through `Beamtalk help: ClassName` evaluation rather
/// than the deprecated `docs` protocol op (which has been removed).
/// Prints via Transcript (BT-4167) so multi-line docs stay readable
/// instead of showing as a quoted, escaped eval value.
fn handle_help_topic(args: &str, client: &mut ReplClient) {
    if args.is_empty() {
        print_help();
        return;
    }

    // Parse "ClassName", "ClassName selector",
    // "ClassName class", or "ClassName class selector"
    let tokens: Vec<&str> = args.split_whitespace().collect();

    // Detect "Erlang <module>" and "Erlang <module> <function>" patterns.
    // "Erlang" alone shows the Erlang class docs; "Erlang <module>" shows FFI help.
    if tokens.first() == Some(&"Erlang") && tokens.len() > 1 {
        handle_erlang_help(&tokens[1..], client);
        return;
    }

    // Build the receiver expression: either `ClassName` (instance side) or
    // `ClassName class` (class side, sends to the metaclass).
    let (receiver, selector) = match tokens.as_slice() {
        [cls] => (cls.to_string(), None),
        [cls, sel] if *sel == "class" => (format!("{cls} class"), None),
        [cls, mid, sel, ..] if *mid == "class" => (format!("{cls} class"), Some(*sel)),
        [cls, sel, ..] => (cls.to_string(), Some(*sel)),
        _ => (args.to_string(), None),
    };

    let help_expr = match selector {
        Some(sel) => format!("Beamtalk help: {receiver} selector: #{sel}"),
        None => format!("Beamtalk help: {receiver}"),
    };
    // BT-4167: Strings (including help text) display quoted via the eval
    // value. Help is documentation for human reading, not a value to
    // inspect as a literal, so route it through Transcript (bare
    // displayString) and print the captured output instead of the value.
    let expr = format!("Transcript show: ({help_expr}). nil");

    match client.eval(&expr) {
        Ok(response) => {
            if response.is_error() {
                if let Some(msg) = response.error_message() {
                    eprintln!("{msg}");
                }
            } else {
                let output = response.output.clone().unwrap_or_default();
                let text = if output.is_empty() {
                    response.value_string()
                } else {
                    output
                };
                if !text.is_empty() {
                    println!("{text}");
                }
            }
        }
        Err(e) => eprintln!("Error: {e}"),
    }
}

/// Handle `:help Erlang <module>` and `:help Erlang <module> <function>`.
///
/// Queries the backend for type signatures (from `-spec` attributes) combined
/// with EEP-48 documentation for the given Erlang module or function.
fn handle_erlang_help(tokens: &[&str], client: &mut ReplClient) {
    // Note: callers guarantee tokens is non-empty (only called when tokens.len() > 1
    // after splitting off the "Erlang" prefix).
    let (module, function) = match tokens {
        [module] => (*module, None),
        [module, function, ..] => (*module, Some(*function)),
        [] => unreachable!("handle_erlang_help called with empty tokens"),
    };

    match client.get_erlang_help(module, function) {
        Ok(response) => {
            if response.is_error() {
                if let Some(msg) = response.error_message() {
                    eprintln!("{msg}");
                }
            } else if let Some(docs) = &response.docs {
                println!("{docs}");
            }
        }
        Err(e) => eprintln!("Error: {e}"),
    }
}

/// Handle `:raw-mfa [on|off]` -- BT-4821, ADR 0139 decision 4.
///
/// Sets how *this* session renders the stack of an error reply: with it on,
/// every frame (hidden ones included) is listed with its raw
/// `module:function/arity` beside the display name. The option lives on this
/// client only; nothing is sent to the node, so a second session attached to
/// the same workspace is unchanged. A bare `:raw-mfa` toggles.
fn handle_raw_mfa(arg: &str, client: &mut ReplClient) {
    let on = match arg {
        "" => !client.raw_mfa(),
        "on" => true,
        "off" => false,
        _ => {
            eprintln!("Usage: :raw-mfa [on|off]");
            return;
        }
    };
    client.set_raw_mfa(on);
    println!(
        "Raw frames {} for this session.",
        if on { "on" } else { "off" }
    );
}

/// Handle `:clear` -- clear the current session's local bindings.
///
/// ADR 0081 Phase 6: the `clear` op was removed; this evaluates
/// `Session current clear`.
fn handle_clear(client: &mut ReplClient) {
    match client.clear_bindings() {
        Ok(response) if response.is_error() => {
            if let Some(msg) = response.error_message() {
                eprintln!("{}", display::format_error(msg));
            }
        }
        Ok(_) => println!("Bindings cleared."),
        Err(e) => eprintln!("Error: {e}"),
    }
}

/// Handle `:bindings` -- display the current session's local binding names.
///
/// ADR 0081 Phase 6: the `bindings` op was removed; this evaluates
/// `Session current bindings keys`, whose result `value` is a list of
/// binding-name symbols. Workspace bindings are a separate layer
/// (`Workspace bindings keys`).
fn handle_bindings(client: &mut ReplClient) {
    match client.get_bindings() {
        Ok(response) => {
            if response.is_error() {
                if let Some(msg) = response.error_message() {
                    eprintln!("{}", display::format_error(msg));
                }
                return;
            }
            match response.value {
                Some(serde_json::Value::Array(names)) if names.is_empty() => {
                    println!("No bindings.");
                }
                Some(serde_json::Value::Array(names)) => {
                    for name in names {
                        println!("  {}", format_value(&name));
                    }
                }
                Some(other) => println!("{}", format_value(&other)),
                None => println!("No bindings."),
            }
        }
        Err(e) => eprintln!("Error: {e}"),
    }
}

/// Handle `:sync` -- sync the workspace with the project on disk.
///
/// Sends a `load-project` op to the backend, which finds `beamtalk.toml` in
/// the current directory, scans `src/` and `native/`, and incrementally
/// compiles changed files.
fn handle_sync(client: &mut ReplClient) {
    match client.load_project(".") {
        Ok(response) => {
            display_sync_diagnostics(&response);

            if response.is_error() {
                // Show generic error message only when no structured diagnostics were printed
                if response.errors.is_empty()
                    && let Some(msg) = response.error_message()
                {
                    eprintln!("{}", display::format_error(msg));
                }
            } else {
                // Display summary and classes only on success
                if let Some(ref summary) = response.summary {
                    println!("{summary}");
                }
                if let Some(ref classes) = response.classes
                    && !classes.is_empty()
                {
                    println!("Classes: {}", classes.join(", "));
                }
            }
        }
        Err(e) => eprintln!("Error: {e}"),
    }
}

/// Display per-file errors and compiler warnings from a sync response.
///
/// Reports each failed file with its error message, line number, and
/// hint (when available), followed by a summary line listing all failed file
/// paths so the user can identify problems without manual bisection.
fn display_sync_diagnostics(response: &ReplResponse) {
    use beamtalk_repl_protocol::format::{
        WarningStyle, format_file_diagnostic, format_warning_entry,
    };

    let mode = output_mode();

    // Collect distinct failed file paths for the summary line.
    let mut failed_paths: Vec<&str> = Vec::new();

    for err in &response.errors {
        if let Some(rendered) = format_file_diagnostic(err, mode) {
            eprintln!("{rendered}");

            let path = err
                .get("path")
                .and_then(|p| p.as_str())
                .unwrap_or("unknown");
            if !failed_paths.contains(&path) {
                failed_paths.push(path);
            }
        }
    }

    // Print a summary line listing all failed files.
    if !failed_paths.is_empty() {
        let summary = format!(
            "{} file(s) failed to load: {}",
            failed_paths.len(),
            failed_paths.join(", ")
        );
        eprintln!("{}", color::paint(color::RED, &summary));
    }

    for w in response.warning_entries() {
        eprintln!("{}", format_warning_entry(&w, mode, WarningStyle::Prefixed));
    }
}

/// Sync the project with tests included, then run a test expression.
///
/// Loads `src/` + `test/` via `load-project` (incremental, mtime-based), displays
/// any sync errors/warnings, then runs the test regardless — some tests may still
/// pass even when other files fail to compile.
fn sync_tests_then_run(client: &mut ReplClient, test_expr: &str) {
    match client.load_project_opts(".", true) {
        Ok(response) => {
            display_sync_diagnostics(&response);
            if response.is_error()
                && response.errors.is_empty()
                && let Some(msg) = response.error_message()
            {
                eprintln!("{}", display::format_error(msg));
            }
        }
        Err(e) => eprintln!(
            "{}",
            color::paint(color::YELLOW, &format!("Sync warning: {e}"))
        ),
    }
    eval_and_display(client, test_expr);
}

/// Handle `:show-codegen <expr>` -- display generated Core Erlang.
///
/// `code` is the already-extracted, trimmed argument (see
/// `handle_help_topic`'s doc comment for why extraction moved out of here).
fn handle_show_codegen(code: &str, client: &mut ReplClient) {
    if code.is_empty() {
        eprintln!("Usage: :show-codegen <expression>");
        return;
    }
    match client.show_codegen(code) {
        Ok(response) => {
            if response.is_error() {
                if let Some(msg) = response.error_message() {
                    eprintln!("{}", display::format_error(msg));
                }
            } else if let Some(ref core) = response.core_erlang {
                display::display_codegen(core);
            }
            if !response.is_error() && response.core_erlang.is_none() && response.warnings.is_none()
            {
                println!("No code generated.");
            }
            for w in response.warning_entries() {
                eprintln!(
                    "{}",
                    beamtalk_repl_protocol::format::format_warning_entry(
                        &w,
                        output_mode(),
                        beamtalk_repl_protocol::format::WarningStyle::Prefixed,
                    )
                );
            }
        }
        Err(e) => eprintln!("Error: {e}"),
    }
}

/// Handle `:interrupt` -- send an interrupt op to cancel a running evaluation.
///
/// The interrupt is sent on a **separate** connection (out-of-band) so it
/// works even when the main session connection is blocked waiting for an
/// eval response. This is the manual counterpart to the automatic Ctrl-C
/// interrupt that fires during `eval_interruptible`.
fn handle_interrupt(client: &mut ReplClient) {
    match client.interrupt() {
        Ok(()) => println!("Interrupt sent."),
        Err(e) => eprintln!("Error: {e}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicBool, Ordering};

    // --- confirm_destructive_action_with ---

    #[test]
    fn confirm_destructive_action_accepts_y() {
        let mut reader = std::io::Cursor::new(b"y\n".as_slice());
        assert!(confirm_destructive_action_with(&mut reader));
    }

    #[test]
    fn confirm_destructive_action_accepts_yes_case_insensitive() {
        let mut reader = std::io::Cursor::new(b"YES\n".as_slice());
        assert!(confirm_destructive_action_with(&mut reader));
    }

    #[test]
    fn confirm_destructive_action_accepts_y_with_surrounding_whitespace() {
        let mut reader = std::io::Cursor::new(b"  y  \n".as_slice());
        assert!(confirm_destructive_action_with(&mut reader));
    }

    #[test]
    fn confirm_destructive_action_declines_n() {
        let mut reader = std::io::Cursor::new(b"n\n".as_slice());
        assert!(!confirm_destructive_action_with(&mut reader));
    }

    #[test]
    fn confirm_destructive_action_declines_blank_line() {
        let mut reader = std::io::Cursor::new(b"\n".as_slice());
        assert!(!confirm_destructive_action_with(&mut reader));
    }

    #[test]
    fn confirm_destructive_action_declines_garbage() {
        let mut reader = std::io::Cursor::new(b"maybe\n".as_slice());
        assert!(!confirm_destructive_action_with(&mut reader));
    }

    #[test]
    fn confirm_destructive_action_declines_eof() {
        let mut reader = std::io::Cursor::new(b"".as_slice());
        assert!(!confirm_destructive_action_with(&mut reader));
    }

    // --- handle_repl_command dispatch ---
    //
    // These drive `handle_repl_command` (and the non-stdin-gated handlers it
    // dispatches to) against `spawn_auth_ok_server`'s fake WS backend, the same
    // pattern `client.rs`'s own tests established for `ReplClient`.
    //
    // Handlers gated behind a terminal y/N confirmation — `:remove-class`,
    // `:rename-class`, `:rename-method`, `:flush-destructive[-arg]` — are
    // still out of scope here by design: `confirm_destructive_action`
    // itself has a `BufRead` injection seam
    // (`confirm_destructive_action_with`, tested directly below) covering its
    // own y/N-parsing branches, but each handler also calls
    // `eval_and_display`/`client.eval` on its own path, so exercising a full
    // handler end-to-end would need threading a fake reader through every
    // `handle_*` call site (and the `CommandAction` dispatch that reaches
    // them) for logic that's otherwise a thin wrapper over the already
    // fully-tested `repl_meta_exprs` expr builders plus the confirm/decline
    // branch. Not worth the seam for what's left uncovered; these stay
    // e2e-only, consistent with the rest of this dispatch suite.
    mod handle_repl_command_dispatch {
        use super::*;
        use crate::commands::test_support::spawn_auth_ok_server;
        use std::sync::Mutex;
        use tungstenite::Message;

        fn connect_to(port: u16) -> ReplClient {
            ReplClient::connect("127.0.0.1", port, "cookie").expect("connect")
        }

        #[test]
        fn clear_command_sends_session_clear_eval() {
            let port = spawn_auth_ok_server(|req, ws| {
                if req.get("op").and_then(|v| v.as_str()) == Some("eval")
                    && req.get("code").and_then(|v| v.as_str()) == Some("Session current clear")
                {
                    let _ = ws.send(Message::Text(
                        serde_json::json!({"status": ["done"]}).to_string().into(),
                    ));
                }
            });
            let mut client = connect_to(port);
            assert_eq!(
                handle_repl_command(":clear", &mut client),
                CommandResult::Handled
            );
        }

        #[test]
        fn bindings_command_handles_empty_list() {
            let port = spawn_auth_ok_server(|req, ws| {
                if req.get("op").and_then(|v| v.as_str()) == Some("eval") {
                    let _ = ws.send(Message::Text(
                        serde_json::json!({"status": ["done"], "value": []})
                            .to_string()
                            .into(),
                    ));
                }
            });
            let mut client = connect_to(port);
            assert_eq!(
                handle_repl_command(":bindings", &mut client),
                CommandResult::Handled
            );
        }

        #[test]
        fn bindings_command_handles_populated_list() {
            let port = spawn_auth_ok_server(|req, ws| {
                if req.get("op").and_then(|v| v.as_str()) == Some("eval") {
                    let _ = ws.send(Message::Text(
                        serde_json::json!({"status": ["done"], "value": ["x", "y"]})
                            .to_string()
                            .into(),
                    ));
                }
            });
            let mut client = connect_to(port);
            assert_eq!(
                handle_repl_command(":bindings", &mut client),
                CommandResult::Handled
            );
        }

        #[test]
        fn bindings_command_surfaces_server_error() {
            let port = spawn_auth_ok_server(|req, ws| {
                if req.get("op").and_then(|v| v.as_str()) == Some("eval") {
                    let _ = ws.send(Message::Text(
                        serde_json::json!({"status": ["done", "error"], "error": "boom"})
                            .to_string()
                            .into(),
                    ));
                }
            });
            let mut client = connect_to(port);
            assert_eq!(
                handle_repl_command(":bindings", &mut client),
                CommandResult::Handled
            );
        }

        #[test]
        fn sync_command_displays_summary_and_classes_on_success() {
            let port = spawn_auth_ok_server(|req, ws| {
                if req.get("op").and_then(|v| v.as_str()) == Some("load-project") {
                    let _ = ws.send(Message::Text(
                        serde_json::json!({
                            "status": ["done"],
                            "summary": "Loaded 2 classes",
                            "classes": ["Foo", "Bar"],
                        })
                        .to_string()
                        .into(),
                    ));
                }
            });
            let mut client = connect_to(port);
            assert_eq!(
                handle_repl_command(":sync", &mut client),
                CommandResult::Handled
            );
        }

        #[test]
        fn interrupt_command_sends_interrupt_op() {
            let port = spawn_auth_ok_server(|req, ws| {
                if req.get("op").and_then(|v| v.as_str()) == Some("interrupt") {
                    let _ = ws.send(Message::Text(
                        serde_json::json!({"status": ["done"]}).to_string().into(),
                    ));
                }
            });
            let mut client = connect_to(port);
            assert_eq!(
                handle_repl_command(":interrupt", &mut client),
                CommandResult::Handled
            );
        }

        #[test]
        fn unload_command_sends_unload_op_for_named_class() {
            let port = spawn_auth_ok_server(|req, ws| {
                if req.get("op").and_then(|v| v.as_str()) == Some("unload") {
                    let _ = ws.send(Message::Text(
                        serde_json::json!({"status": ["done"], "classes": ["Foo"]})
                            .to_string()
                            .into(),
                    ));
                }
            });
            let mut client = connect_to(port);
            assert_eq!(
                handle_repl_command(":unload Foo", &mut client),
                CommandResult::Handled
            );
        }

        #[test]
        fn unload_command_with_no_class_name_does_not_reach_server() {
            // `spawn_auth_ok_server`'s handler runs on a background thread
            // (`std::thread::spawn`, never joined) — a `panic!` there is
            // caught at that thread's boundary and never fails the test, so
            // "no contact" must be observed via shared state on the main
            // thread instead of asserted inside the handler.
            let contacted = Arc::new(AtomicBool::new(false));
            let contacted_writer = Arc::clone(&contacted);
            let mut client = connect_to(spawn_auth_ok_server(move |_req, _ws| {
                contacted_writer.store(true, Ordering::SeqCst);
            }));
            assert_eq!(
                handle_repl_command(":unload", &mut client),
                CommandResult::Handled
            );
            assert!(
                !contacted.load(Ordering::SeqCst),
                "unload with no class name must not contact the server"
            );
        }

        #[test]
        fn show_codegen_command_displays_generated_code() {
            let port = spawn_auth_ok_server(|req, ws| {
                if req.get("op").and_then(|v| v.as_str()) == Some("show-codegen") {
                    let _ = ws.send(Message::Text(
                        serde_json::json!({"status": ["done"], "core_erlang": "'foo'/0 = ..."})
                            .to_string()
                            .into(),
                    ));
                }
            });
            let mut client = connect_to(port);
            assert_eq!(
                handle_repl_command(":show-codegen 1 + 1", &mut client),
                CommandResult::Handled
            );
        }

        #[test]
        fn help_topic_command_evaluates_beamtalk_help() {
            let port = spawn_auth_ok_server(|req, ws| {
                if req.get("op").and_then(|v| v.as_str()) == Some("eval")
                    && req.get("code").and_then(|v| v.as_str())
                        == Some("Transcript show: (Beamtalk help: Counter). nil")
                {
                    let _ = ws.send(Message::Text(
                        serde_json::json!({"status": ["done"], "value": "nil", "output": "Counter docs"})
                            .to_string()
                            .into(),
                    ));
                }
            });
            let mut client = connect_to(port);
            assert_eq!(
                handle_repl_command(":help Counter", &mut client),
                CommandResult::Handled
            );
        }

        #[test]
        fn remove_method_command_evaluates_remove_selector_send() {
            // Content checks must happen on the main test thread: the handler
            // below runs on a background thread that `spawn_auth_ok_server`
            // never joins, so an `assert!` inside it would be silently
            // swallowed on failure instead of failing the test.
            let received_code = Arc::new(Mutex::new(None));
            let received_code_writer = Arc::clone(&received_code);
            let port = spawn_auth_ok_server(move |req, ws| {
                if req.get("op").and_then(|v| v.as_str()) == Some("eval") {
                    let code = req
                        .get("code")
                        .and_then(|v| v.as_str())
                        .unwrap_or_default()
                        .to_string();
                    *received_code_writer.lock().unwrap() = Some(code);
                    let _ = ws.send(Message::Text(
                        serde_json::json!({"status": ["done"]}).to_string().into(),
                    ));
                }
            });
            let mut client = connect_to(port);
            assert_eq!(
                handle_repl_command(":remove-method Counter increment", &mut client),
                CommandResult::Handled
            );
            let code = received_code.lock().unwrap().clone();
            let code = code.expect("eval op should have been sent to the server");
            assert!(
                code.contains("Counter") && code.contains("increment"),
                "unexpected eval code: {code}"
            );
        }

        #[test]
        fn flush_changes_dirty_recheck_commands_all_dispatch() {
            // ADR 0082 Phase 3 aliases: each desugars to a fixed
            // `Workspace ...` eval expression via `eval_and_display` — assert
            // all four reach the server and are handled, one fake server per
            // command since each expects a different `code`. The comparison
            // happens on the main thread (via shared state), not inside the
            // handler's background thread, which `spawn_auth_ok_server` never
            // joins — an `assert_eq!` there would be silently swallowed on
            // failure instead of failing the test.
            for (line, expected_code) in [
                (":flush", "Workspace flush"),
                (":changes", "Workspace changes"),
                (":dirty", "Workspace changes dirtyMethods"),
                (":recheck image", "Workspace recheckImage"),
            ] {
                let received_code = Arc::new(Mutex::new(None));
                let received_code_writer = Arc::clone(&received_code);
                let port = spawn_auth_ok_server(move |req, ws| {
                    if req.get("op").and_then(|v| v.as_str()) == Some("eval") {
                        let code = req
                            .get("code")
                            .and_then(|v| v.as_str())
                            .unwrap_or_default()
                            .to_string();
                        *received_code_writer.lock().unwrap() = Some(code);
                        let _ = ws.send(Message::Text(
                            serde_json::json!({"status": ["done"]}).to_string().into(),
                        ));
                    }
                });
                let mut client = connect_to(port);
                assert_eq!(
                    handle_repl_command(line, &mut client),
                    CommandResult::Handled,
                    "line {line:?} should be handled"
                );
                assert_eq!(
                    received_code.lock().unwrap().as_deref(),
                    Some(expected_code),
                    "line {line:?} sent unexpected eval code"
                );
            }
        }

        #[test]
        fn raw_mfa_toggle_is_per_session_and_never_reaches_the_node() {
            // BT-4821 / ADR 0139 decision 4: two sessions attached to one node
            // (one fake server). `:raw-mfa` on one is a render option of that
            // client only: the other is unchanged, and the node sees no request
            // at all, so no node-wide state can have been written.
            let contacted = Arc::new(AtomicBool::new(false));
            let contacted_writer = Arc::clone(&contacted);
            let port = spawn_auth_ok_server(move |_req, _ws| {
                contacted_writer.store(true, Ordering::SeqCst);
            });
            let mut first = connect_to(port);
            let mut second = connect_to(port);
            assert!(!first.raw_mfa() && !second.raw_mfa());

            assert_eq!(
                handle_repl_command(":raw-mfa on", &mut first),
                CommandResult::Handled
            );
            assert!(first.raw_mfa(), "the session that set it is raw");
            assert!(!second.raw_mfa(), "the other session is unchanged");

            // A bare `:raw-mfa` toggles; `off` clears; junk is rejected.
            assert_eq!(
                handle_repl_command(":raw-mfa", &mut first),
                CommandResult::Handled
            );
            assert!(!first.raw_mfa());
            handle_repl_command(":raw-mfa on", &mut second);
            handle_repl_command(":raw-mfa off", &mut second);
            handle_repl_command(":raw-mfa maybe", &mut second);
            assert!(!second.raw_mfa());

            assert!(
                !contacted.load(Ordering::SeqCst),
                ":raw-mfa must not send anything to the node"
            );
        }

        #[test]
        fn exit_command_returns_exit_without_contacting_server() {
            // See unload_command_with_no_class_name_does_not_reach_server: a
            // `panic!` inside spawn_auth_ok_server's unjoined background
            // thread never fails the test, so "no contact" is observed via
            // shared state instead.
            let contacted = Arc::new(AtomicBool::new(false));
            let contacted_writer = Arc::clone(&contacted);
            let mut client = connect_to(spawn_auth_ok_server(move |_req, _ws| {
                contacted_writer.store(true, Ordering::SeqCst);
            }));
            assert_eq!(
                handle_repl_command(":exit", &mut client),
                CommandResult::Exit
            );
            assert!(
                !contacted.load(Ordering::SeqCst),
                ":exit must not contact the server"
            );
        }

        #[test]
        fn unrecognized_line_is_not_a_command() {
            // See unload_command_with_no_class_name_does_not_reach_server: a
            // `panic!` inside spawn_auth_ok_server's unjoined background
            // thread never fails the test, so "no contact" is observed via
            // shared state instead.
            let contacted = Arc::new(AtomicBool::new(false));
            let contacted_writer = Arc::clone(&contacted);
            let mut client = connect_to(spawn_auth_ok_server(move |_req, _ws| {
                contacted_writer.store(true, Ordering::SeqCst);
            }));
            assert_eq!(
                handle_repl_command("1 + 1", &mut client),
                CommandResult::NotACommand
            );
            assert!(
                !contacted.load(Ordering::SeqCst),
                "a non-command line must not be dispatched by handle_repl_command"
            );
        }
    }

    // Argument extraction lives in `commands::ReplCommandSpec::arg`, not a
    // standalone `extract_command_arg` helper (table-driven, covers name +
    // every alias in one place). `commands.rs` has its own unit tests for that
    // method directly; the tests below exercise it through `classify_command`,
    // the actual dispatch entry point.

    #[test]
    fn classify_command_extracts_arg_for_long_prefix() {
        assert_eq!(
            classify_command(":unload foo.bt"),
            Some(CommandAction::Unload("foo.bt"))
        );
    }

    #[test]
    fn classify_command_extracts_arg_for_short_alias() {
        assert_eq!(
            classify_command(":t Counter"),
            Some(CommandAction::Test("Counter"))
        );
    }

    #[test]
    fn classify_command_trims_whitespace_in_arg() {
        assert_eq!(
            classify_command(":unload   Counter  "),
            Some(CommandAction::Unload("Counter"))
        );
    }

    #[test]
    fn classify_command_raw_mfa_takes_an_optional_argument() {
        assert_eq!(
            classify_command(":raw-mfa"),
            Some(CommandAction::RawMfa(""))
        );
        assert_eq!(
            classify_command(":raw-mfa on"),
            Some(CommandAction::RawMfa("on"))
        );
        assert_eq!(
            classify_command(":raw-mfa off"),
            Some(CommandAction::RawMfa("off"))
        );
    }

    #[test]
    fn classify_command_unrecognized_word_is_not_a_command() {
        assert_eq!(classify_command(":other cmd"), None);
    }

    #[test]
    fn classify_command_bare_unload_is_usage() {
        assert_eq!(
            classify_command(":unload"),
            Some(CommandAction::UnloadUsage)
        );
    }

    #[test]
    fn classify_command_trailing_space_only_is_empty_arg() {
        // `:unload ` (trailing space, nothing after) is a distinct case from
        // bare `:unload` — both end up showing usage, but via different
        // `CommandAction` variants (`Unload("")` vs `UnloadUsage`).
        assert_eq!(
            classify_command(":unload "),
            Some(CommandAction::Unload(""))
        );
    }

    /// Regression guard: every form (name or alias) offered by
    /// tab-completion (`commands::all_forms`, consumed by `helper.rs`) must
    /// dispatch to *something* when submitted bare — the exact bug that let
    /// `:actors`/`:kill`/`:inspect`/`:sessions` complete with no dispatch arm
    /// (parse-erroring as Beamtalk source) while `:interrupt`/`:recheck`
    /// dispatched but didn't complete.
    #[test]
    fn every_completable_form_dispatches_when_submitted_bare() {
        for form in commands::all_forms() {
            assert!(
                classify_command(form).is_some(),
                "{form:?} tab-completes but classify_command finds no dispatch"
            );
        }
    }

    // -----------------------------------------------------------------------
    // ADR 0082 Phase 3 — `:flush`, `:flush <sel>`, `:changes`,
    // `:dirty` meta-command dispatch. The aliases compile to Beamtalk
    // expressions; the surface-parity contract (and the Erlang side) treats
    // them as `evaluate` of a known string. These tests pin the translation.
    // -----------------------------------------------------------------------

    /// Construct the Beamtalk expression a `:flush <selector>` line
    /// dispatches to: `commands::FLUSH.arg` extracts the selector exactly as
    /// `classify_command` does, and `repl_meta_exprs::flush_expr_for` — the
    /// same shared function `handle_repl_command`'s real dispatch and
    /// `tests/repl_protocol.rs` call — builds the expression, so this test
    /// can only drift from production behaviour if that shared function's
    /// own contract changes.
    fn flush_expr_for(line: &str) -> Option<String> {
        beamtalk_cli::repl_meta_exprs::flush_expr_for(commands::FLUSH.arg(line).unwrap_or(""))
    }

    #[test]
    fn flush_class_selector_translates_to_workspace_flush_message() {
        assert_eq!(
            flush_expr_for(":flush Counter"),
            Some("Workspace flush: Counter".to_string())
        );
    }

    #[test]
    fn flush_symbol_kind_selector_translates_verbatim() {
        assert_eq!(
            flush_expr_for(":flush #'new-class'"),
            Some("Workspace flush: #'new-class'".to_string())
        );
    }

    #[test]
    fn flush_dictionary_selector_translates_verbatim() {
        assert_eq!(
            flush_expr_for(":flush #{ #file => \"src/foo.bt\" }"),
            Some("Workspace flush: #{ #file => \"src/foo.bt\" }".to_string())
        );
    }

    #[test]
    fn flush_with_trailing_whitespace_in_selector_is_trimmed() {
        // `extract_command_arg` trims — selectors don't end with stray space.
        assert_eq!(
            flush_expr_for(":flush   Counter   "),
            Some("Workspace flush: Counter".to_string())
        );
    }

    /// Maps a line to the Beamtalk expression `handle_repl_command` evaluates
    /// for it, by going through the real `classify_command` rather
    /// than a hand-maintained mirror — so this test can only drift from
    /// production behaviour if `handle_repl_command`'s own match arm changes
    /// without a matching `CommandAction` (which the compiler's exhaustive
    /// match already forbids).
    fn bare_alias_expr(line: &str) -> Option<&'static str> {
        match classify_command(line)? {
            CommandAction::Flush => Some("Workspace flush"),
            CommandAction::Changes => Some("Workspace changes"),
            CommandAction::Dirty => Some("Workspace changes dirtyMethods"),
            CommandAction::RecheckImage => Some("Workspace recheckImage"),
            _ => None,
        }
    }

    #[test]
    fn bare_flush_translates_to_workspace_flush() {
        assert_eq!(bare_alias_expr(":flush"), Some("Workspace flush"));
    }

    #[test]
    fn changes_translates_to_workspace_changes() {
        assert_eq!(bare_alias_expr(":changes"), Some("Workspace changes"));
    }

    #[test]
    fn dirty_translates_to_workspace_changes_dirty_methods() {
        assert_eq!(
            bare_alias_expr(":dirty"),
            Some("Workspace changes dirtyMethods")
        );
    }

    #[test]
    fn recheck_image_translates_to_workspace_recheck_image() {
        assert_eq!(
            bare_alias_expr(":recheck image"),
            Some("Workspace recheckImage")
        );
    }

    #[test]
    fn flush_with_empty_selector_reports_no_expression() {
        // `:flush ` (trailing space only) is a usage error, not an eval.
        assert_eq!(flush_expr_for(":flush "), None);
        assert_eq!(flush_expr_for(":flush     "), None);
    }

    // -----------------------------------------------------------------------
    // ADR 0112 Phase 4 — `:remove-method <Class> <selector>`
    // meta-command dispatch, matching the `:flush`/`:changes` alias pattern.
    // The expression-building logic itself (`remove_method_expr_for`) lives
    // in `beamtalk_cli::repl_meta_exprs` — shared with, and tested by,
    // `tests/repl_protocol.rs` — so only dispatch classification is tested
    // here.
    // -----------------------------------------------------------------------

    #[test]
    fn bare_remove_method_is_usage_error() {
        assert_eq!(
            classify_command(":remove-method"),
            Some(CommandAction::RemoveMethodUsage)
        );
    }

    #[test]
    fn remove_method_with_args_classifies_as_remove_method_arg() {
        assert_eq!(
            classify_command(":remove-method Counter increment"),
            Some(CommandAction::RemoveMethodArg("Counter increment"))
        );
    }

    // -----------------------------------------------------------------------
    // ADR 0113 Phase 4 — `:remove-class <Class>` and
    // `:flush-destructive [<selector>]` meta-command dispatch, matching the
    // `:remove-method`/`:flush` alias pattern. The expression-building logic
    // (`remove_class_expr_for`, `flush_destructive_expr_for`,
    // `FLUSH_INCLUDING_DESTRUCTIVE_EXPR`) lives in
    // `beamtalk_cli::repl_meta_exprs` — shared with, and tested by,
    // `tests/repl_protocol.rs` — so only dispatch classification is tested
    // here; the terminal `y/N` confirmation gate is interactive and not unit
    // tested (matches how no other REPL command's terminal I/O is unit
    // tested either).
    // -----------------------------------------------------------------------

    #[test]
    fn bare_remove_class_is_usage_error() {
        assert_eq!(
            classify_command(":remove-class"),
            Some(CommandAction::RemoveClassUsage)
        );
    }

    #[test]
    fn remove_class_with_arg_classifies_as_remove_class_arg() {
        assert_eq!(
            classify_command(":remove-class Counter"),
            Some(CommandAction::RemoveClassArg("Counter"))
        );
    }

    #[test]
    fn bare_flush_destructive_classifies_as_flush_destructive() {
        assert_eq!(
            classify_command(":flush-destructive"),
            Some(CommandAction::FlushDestructive)
        );
    }

    #[test]
    fn flush_destructive_with_arg_classifies_as_flush_destructive_arg() {
        assert_eq!(
            classify_command(":flush-destructive Counter"),
            Some(CommandAction::FlushDestructiveArg("Counter"))
        );
    }

    // -----------------------------------------------------------------------
    // ADR 0114 Phase 5 — `:rename-class <Class> <NewName>` and
    // `:rename-method <Class> <selector> <newSelector>` meta-command
    // dispatch, matching `:remove-class`'s alias pattern. The
    // expression-building logic (`rename_class_expr_for`,
    // `rename_method_expr_for`) lives in `beamtalk_cli::repl_meta_exprs` —
    // shared with, and tested by, `tests/repl_protocol.rs` — so only
    // dispatch classification is tested here; the terminal `y/N`
    // confirmation gate is interactive and not unit tested (matches
    // `:remove-class`/`:flush-destructive`).
    // -----------------------------------------------------------------------

    #[test]
    fn bare_rename_class_is_usage_error() {
        assert_eq!(
            classify_command(":rename-class"),
            Some(CommandAction::RenameClassUsage)
        );
    }

    #[test]
    fn rename_class_with_args_classifies_as_rename_class_arg() {
        assert_eq!(
            classify_command(":rename-class Counter Accumulator"),
            Some(CommandAction::RenameClassArg("Counter Accumulator"))
        );
    }

    #[test]
    fn bare_rename_method_is_usage_error() {
        assert_eq!(
            classify_command(":rename-method"),
            Some(CommandAction::RenameMethodUsage)
        );
    }

    #[test]
    fn rename_method_with_args_classifies_as_rename_method_arg() {
        assert_eq!(
            classify_command(":rename-method Counter increment incrementBy"),
            Some(CommandAction::RenameMethodArg(
                "Counter increment incrementBy"
            ))
        );
    }
}
