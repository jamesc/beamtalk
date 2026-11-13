// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4591: left-to-right operand evaluation as a checked invariant on the
//! emitted Core Erlang.
//!
//! Core Erlang leaves the evaluation order of a construct's operands
//! unspecified, and erlc evaluates a list cell's tail before its head and a map
//! literal's last pair first. So `[send(a), send(b)]` runs `send(b)` first. BT-4296
//! sequenced every operand list codegen builds (`thread_subexprs`); this module
//! is the backstop for the *next* list builder that forgets to: it scans the
//! finished module text and reports every list cell or map literal that holds
//! two or more operands that can run code (a `call`, `apply` or `primop`, not a
//! variable or a literal).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Scope: list cells and map literals only. Tuple elements and the arguments
//! of a direct `call 'm':'f'(A, B)` are not scanned.
//!
//! The scan is a recursive-descent pass over the Core Erlang the generator
//! prints (`Document::to_pretty_string`), answering for each expression whether
//! evaluating it may run code. A `fun` body does not run when the closure is
//! built, so it does not count. Patterns and guards are skipped.
//!
//! The driver runs it on every module `generate_module_with_warnings` generates
//! in debug builds (the REPL assembler drives its own generator and is not
//! scanned), so `just test-stdlib`, `just test-bunit`
//! and every codegen unit test — the `verify-threaded-ir` corpus — fail on a
//! reintroduced in-place builder.

use super::threaded_ir::{RacingConstruct, VerifyError};

/// The racing operand lists of Core Erlang `code`: one finding per list cell or
/// map literal with two or more operands that may run code. Empty for a module
/// whose operand lists are all sequenced. The scan is a heuristic over the
/// printed text: if the parser falls out of step with the Core Erlang grammar
/// it can report a finding that is not there — a loud failure in debug builds,
/// never a silent miss of the shapes it understands.
pub(super) fn racing_operand_errors(code: &str) -> Vec<VerifyError> {
    let (tokens, starts) = tokenize(code);
    let mut scan = Scan {
        code,
        starts,
        tokens,
        at: 0,
        function: String::new(),
        found: Vec::new(),
    };
    scan.module();
    scan.found
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Tok {
    /// A quoted atom, with its quotes removed.
    Atom(String),
    /// An unquoted identifier: a keyword or a variable.
    Word(String),
    /// A string, character or number literal.
    Lit,
    /// Punctuation, the multi-character forms included (`->`, `=>`, `-|`,
    /// `~{`, `}~`, `#{`, `}#`).
    Punct(&'static str),
}

#[allow(clippy::too_many_lines)] // one arm per token form
fn tokenize(code: &str) -> (Vec<Tok>, Vec<usize>) {
    let b = code.as_bytes();
    let mut out = Vec::new();
    let mut starts: Vec<usize> = Vec::new();
    let mut i = 0;
    while i < b.len() {
        let here = i;
        let c = b[i];
        let next = b.get(i + 1).copied();
        match c {
            b' ' | b'\t' | b'\r' | b'\n' => i += 1,
            b'%' => {
                while i < b.len() && b[i] != b'\n' {
                    i += 1;
                }
            }
            b'"' | b'\'' => {
                let start = i + 1;
                i += 1;
                while i < b.len() && b[i] != c {
                    i += if b[i] == b'\\' { 2 } else { 1 };
                }
                let end = i.min(b.len());
                i += 1;
                if c == b'\'' {
                    out.push(Tok::Atom(code[start..end].to_string()));
                } else {
                    out.push(Tok::Lit);
                }
            }
            b'$' => {
                // A character literal: `$a`, `$\n`.
                i += if next == Some(b'\\') { 3 } else { 2 };
                out.push(Tok::Lit);
            }
            b'-' if next == Some(b'>') => {
                out.push(Tok::Punct("->"));
                i += 2;
            }
            b'-' if next == Some(b'|') => {
                out.push(Tok::Punct("-|"));
                i += 2;
            }
            b'=' if next == Some(b'>') => {
                out.push(Tok::Punct("=>"));
                i += 2;
            }
            b':' if next == Some(b'=') => {
                out.push(Tok::Punct(":="));
                i += 2;
            }
            b'~' if next == Some(b'{') => {
                out.push(Tok::Punct("~{"));
                i += 2;
            }
            b'#' if next == Some(b'{') => {
                out.push(Tok::Punct("#{"));
                i += 2;
            }
            b'}' if next == Some(b'~') => {
                out.push(Tok::Punct("}~"));
                i += 2;
            }
            b'}' if next == Some(b'#') => {
                out.push(Tok::Punct("}#"));
                i += 2;
            }
            b'-' | b'0'..=b'9' if c != b'-' || next.is_some_and(|n| n.is_ascii_digit()) => {
                i += 1;
                while i < b.len() && (b[i].is_ascii_alphanumeric() || matches!(b[i], b'.' | b'_')) {
                    // `1.5e-3`: an exponent sign belongs to the number.
                    if matches!(b[i], b'e' | b'E')
                        && b.get(i + 1) == Some(&b'-')
                        && b.get(i + 2).is_some_and(u8::is_ascii_digit)
                    {
                        i += 1;
                    }
                    i += 1;
                }
                out.push(Tok::Lit);
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let start = i;
                while i < b.len() && (b[i].is_ascii_alphanumeric() || matches!(b[i], b'_' | b'@')) {
                    i += 1;
                }
                out.push(Tok::Word(code[start..i].to_string()));
            }
            _ => {
                let punct = match c {
                    b'(' => "(",
                    b')' => ")",
                    b'[' => "[",
                    b']' => "]",
                    b'{' => "{",
                    b'}' => "}",
                    b'<' => "<",
                    b'>' => ">",
                    b',' => ",",
                    b'|' => "|",
                    b'=' => "=",
                    b':' => ":",
                    b'/' => "/",
                    b'#' => "#",
                    _ => "?",
                };
                out.push(Tok::Punct(punct));
                i += 1;
            }
        }
        starts.resize(out.len(), here);
    }
    (out, starts)
}

fn is_keyword(word: &str) -> bool {
    matches!(
        word,
        "call"
            | "apply"
            | "primop"
            | "let"
            | "letrec"
            | "in"
            | "case"
            | "of"
            | "end"
            | "fun"
            | "try"
            | "catch"
            | "receive"
            | "after"
            | "do"
            | "when"
    )
}

/// The effect of `call module:function(args)`, `atomic` when every argument is a
/// variable or literal term. Deny by default: only the calls below are
/// anything but `RUNS`.
///
/// `QUIET`: pure projections over bound values — the codegen's
/// `self_dispatch_result_value`, immutable state-map reads and string
/// conversions.
///
/// `READS`: a read of mutable process state a sibling that runs code can
/// change — the class-variable lookup `erlang:get(Key)` and its miss fallback
/// `beamtalk_class_vars:get/2`, `get_late/2`, and a block literal's
/// creation-time `capture/2` (BT-4722), which reads the live cell or
/// class-variable map.
fn call_effect(module: Option<&str>, function: Option<&str>, atomic: bool) -> Effect {
    if !atomic {
        return RUNS;
    }
    match (module, function) {
        (Some("erlang"), Some("element" | "hd" | "tl" | "tuple_size" | "map_size"))
        | (Some("maps"), Some("get" | "find"))
        | (Some("beamtalk_primitive"), Some("string_to_binary")) => QUIET,
        // BT-4722: a block literal's creation-time capture reads the live cell /
        // class-variable map, so a sibling operand that runs code and writes
        // that state must be sequenced before it.
        (Some("erlang"), Some("get"))
        | (Some("beamtalk_class_vars"), Some("get" | "get_late" | "capture"))
        | (Some("beamtalk_actor_state"), Some("capture")) => READS,
        _ => RUNS,
    }
}

/// What evaluating an expression does: nothing observable, a read of mutable
/// process state (the class-variable lookup `case erlang:get(Key) of ..`), or
/// arbitrary code. Ordered, so a combination is the `max` of its parts.
type Effect = u8;
const QUIET: Effect = 0;
const READS: Effect = 1;
const RUNS: Effect = 2;

struct Scan<'a> {
    code: &'a str,
    /// The byte offset of each token in `code`.
    starts: Vec<usize>,
    tokens: Vec<Tok>,
    at: usize,
    /// The top-level function being scanned, for the finding.
    function: String,
    found: Vec<VerifyError>,
}

impl Scan<'_> {
    fn peek(&self) -> Option<&Tok> {
        self.tokens.get(self.at)
    }

    fn bump(&mut self) -> Option<Tok> {
        let tok = self.tokens.get(self.at).cloned();
        self.at += 1;
        tok
    }

    fn at_punct(&self, p: &str) -> bool {
        matches!(self.peek(), Some(Tok::Punct(q)) if *q == p)
    }

    fn at_word(&self, w: &str) -> bool {
        matches!(self.peek(), Some(Tok::Word(x)) if x == w)
    }

    fn eat_punct(&mut self, p: &str) -> bool {
        let hit = self.at_punct(p);
        if hit {
            self.at += 1;
        }
        hit
    }

    fn eat_word(&mut self, w: &str) -> bool {
        let hit = self.at_word(w);
        if hit {
            self.at += 1;
        }
        hit
    }

    /// Skips a balanced group whose opener is the current token.
    fn skip_group(&mut self) {
        let mut depth = 0usize;
        while let Some(tok) = self.bump() {
            if let Tok::Punct(p) = tok {
                match p {
                    "(" | "[" | "{" | "<" | "~{" | "#{" => depth += 1,
                    ")" | "]" | "}" | ">" | "}~" | "}#" => {
                        depth = depth.saturating_sub(1);
                        if depth == 0 {
                            return;
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    /// Skips a pattern (or variable list): everything up to, not including, the
    /// next top-level `when`, `->` or `=`.
    fn skip_pattern(&mut self) {
        while let Some(tok) = self.peek() {
            match tok {
                Tok::Word(w) if w == "when" => return,
                Tok::Punct("->" | "=") => return,
                Tok::Punct("(" | "[" | "{" | "<" | "~{" | "#{") => self.skip_group(),
                _ => self.at += 1,
            }
        }
    }

    /// `module 'm' [exports] attributes [attrs] defs end`.
    fn module(&mut self) {
        if !self.eat_word("module") {
            return;
        }
        self.bump(); // the module name
        self.skip_group(); // exports
        if self.eat_word("attributes") {
            self.skip_group();
        }
        while let Some(tok) = self.peek().cloned() {
            if let Tok::Atom(name) = tok {
                self.at += 1;
                self.function = name;
                if self.eat_punct("/") {
                    self.bump();
                }
                self.eat_punct("=");
                self.expr();
            } else {
                self.at += 1;
            }
        }
    }

    /// A comma-separated expression list closed by `close`; whether any may run
    /// code.
    fn expr_list(&mut self, close: &str) -> Effect {
        let mut runs = QUIET;
        while self.peek().is_some() && !self.at_punct(close) {
            runs = runs.max(self.expr());
            if !self.eat_punct(",") && !self.at_punct(close) {
                break;
            }
        }
        self.eat_punct(close);
        runs
    }

    /// Records a finding for a list or map holding `racing` operands that may
    /// run code.
    fn note(&mut self, construct: RacingConstruct, effects: &[Effect], open: usize) {
        let racing = effects.iter().filter(|e| **e >= READS).count();
        // A read races only a sibling that runs code: two reads cannot disturb
        // each other.
        if racing >= 2 && effects.contains(&RUNS) {
            let from = self.starts.get(open).copied().unwrap_or(0);
            let snippet: String = self.code[from..].chars().take(240).collect();
            self.found.push(VerifyError::RacingOperands {
                construct,
                function: self.function.clone(),
                operands: racing,
                snippet,
            });
        }
    }

    /// Parses one expression; whether evaluating it may run code.
    #[allow(clippy::too_many_lines)] // one arm per Core Erlang form
    fn expr(&mut self) -> Effect {
        let Some(tok) = self.bump() else {
            return QUIET;
        };
        match tok {
            Tok::Atom(_) => {
                // A function name: `'f'/1`.
                if self.at_punct("/") {
                    self.at += 2;
                }
                QUIET
            }
            Tok::Word(w) => match w.as_str() {
                "let" => {
                    self.skip_pattern();
                    self.eat_punct("=");
                    let bound = self.expr();
                    self.eat_word("in");
                    bound.max(self.expr())
                }
                "letrec" => {
                    while matches!(self.peek(), Some(Tok::Atom(_))) {
                        self.at += 1;
                        if self.eat_punct("/") {
                            self.at += 1;
                        }
                        self.eat_punct("=");
                        self.expr();
                    }
                    self.eat_word("in");
                    self.expr()
                }
                "case" => {
                    // A class reference (`case whereis('beamtalk_class_X') of
                    // undefined -> raise class_not_found; Pid -> Pid`) runs no
                    // user code, and `is_trivial_subexpr` treats the operand as
                    // one that races with nothing. Every other `case` is the
                    // `max` of its scrutinee and clauses; a class variable read
                    // (`case erlang:get(Key) of .. -> beamtalk_class_vars:get(..)`)
                    // is `READS` only because each call in it is a known read
                    // (`call_effect`), so a clause that runs code or `put`s
                    // makes it `RUNS`.
                    // BT-4756: a user-class literal reads its reference from the
                    // class's static call map and, on a miss, asks
                    // `beamtalk_class_shadow_flags:class_ref/1`; it is the same
                    // operand, recognised by that call among its clauses.
                    let mut class_lookup = self.at_call_to("erlang", "whereis");
                    let map_read = self.at_call_to("persistent_term", "get");
                    let scrutinee = self.expr();
                    self.eat_word("of");
                    // The miss clause (not the scrutinee, not the whole
                    // `case`) is where `class_ref/1` is called.
                    let clauses_start = self.at;
                    let clauses = self.clauses("end");
                    if map_read {
                        class_lookup = self.tokens[clauses_start..self.at].windows(4).any(|w| {
                            matches!(w, [Tok::Atom(m), Tok::Punct(":"), Tok::Atom(f), Tok::Punct("(")]
                                if m == "beamtalk_class_shadow_flags" && f == "class_ref")
                        });
                    }
                    if class_lookup {
                        QUIET
                    } else {
                        scrutinee.max(clauses)
                    }
                }
                "receive" => {
                    let clauses = self.clauses("after");
                    self.eat_word("after");
                    let timeout = self.expr();
                    self.eat_punct("->");
                    clauses.max(timeout).max(self.expr())
                }
                "try" => {
                    let body = self.expr();
                    self.eat_word("of");
                    self.skip_pattern();
                    self.eat_punct("->");
                    let ok = self.expr();
                    self.eat_word("catch");
                    self.skip_pattern();
                    self.eat_punct("->");
                    body.max(ok).max(self.expr())
                }
                "fun" => {
                    if self.at_punct("(") {
                        self.skip_group();
                    }
                    self.eat_punct("->");
                    // The body runs when the closure is called, not built.
                    self.expr();
                    QUIET
                }
                "do" => {
                    let first = self.expr();
                    first.max(self.expr())
                }
                "catch" => self.expr(),
                "call" => {
                    let module = self.peek_atom();
                    self.expr_operand();
                    self.eat_punct(":");
                    let function = self.peek_atom();
                    self.expr_operand();
                    let atomic = self.args_are_atomic();
                    self.call_args();
                    call_effect(module.as_deref(), function.as_deref(), atomic)
                }
                "apply" => {
                    self.expr_operand();
                    self.call_args();
                    RUNS
                }
                "primop" => {
                    self.bump();
                    self.call_args();
                    RUNS
                }
                // A variable (or an unknown word): reads, runs nothing.
                _ => QUIET,
            },
            Tok::Punct("(") => {
                let runs = self.expr();
                if self.eat_punct("-|") {
                    self.skip_group();
                }
                self.eat_punct(")");
                runs
            }
            Tok::Punct("<") => self.expr_list(">"),
            Tok::Punct("{") => self.expr_list("}"),
            Tok::Punct("[") => {
                let open = self.at - 1;
                let mut effects = Vec::new();
                while self.peek().is_some() && !self.at_punct("]") {
                    effects.push(self.expr());
                    if !(self.eat_punct(",") || self.eat_punct("|")) {
                        break;
                    }
                }
                self.eat_punct("]");
                self.note(RacingConstruct::ListCell, &effects, open);
                effects.into_iter().max().unwrap_or(QUIET)
            }
            Tok::Punct("~{") => {
                let open = self.at - 1;
                let mut effects = Vec::new();
                while self.peek().is_some() && !self.at_punct("}~") {
                    effects.push(self.expr());
                    if self.eat_punct("=>") || self.eat_punct(":=") {
                        effects.push(self.expr());
                    }
                    if !(self.eat_punct(",") || self.eat_punct("|")) {
                        break;
                    }
                }
                self.eat_punct("}~");
                self.note(RacingConstruct::MapLiteral, &effects, open);
                effects.into_iter().max().unwrap_or(QUIET)
            }
            Tok::Punct("#{") => {
                // Binary segments: `#<Value>(Size, Unit, Type, Flags)`.
                let mut runs = QUIET;
                while self.peek().is_some() && !self.at_punct("}#") {
                    self.eat_punct("#");
                    if self.eat_punct("<") {
                        runs = runs.max(self.expr());
                        self.eat_punct(">");
                    }
                    if self.eat_punct("(") {
                        runs = runs.max(self.expr_list(")"));
                    }
                    if !self.eat_punct(",") {
                        break;
                    }
                }
                self.eat_punct("}#");
                runs
            }
            Tok::Lit | Tok::Punct(_) => QUIET,
        }
    }

    /// The module, function or callee operand of a `call`/`apply`: an atom, a
    /// variable or a parenthesised expression — never itself a run of code in
    /// generated modules.
    fn expr_operand(&mut self) {
        self.expr();
    }

    /// Whether the cursor is at `call '<module>':'<function>'(...)`.
    fn at_call_to(&self, module: &str, function: &str) -> bool {
        matches!(
            self.tokens.get(self.at..self.at + 4),
            Some([
                Tok::Word(c),
                Tok::Atom(m),
                Tok::Punct(":"),
                Tok::Atom(f)
            ]) if c == "call" && m == module && f == function
        )
    }

    /// The quoted atom at the cursor, without consuming it.
    fn peek_atom(&self) -> Option<String> {
        match self.peek() {
            Some(Tok::Atom(a)) => Some(a.clone()),
            _ => None,
        }
    }

    /// Whether the `( arg, ... )` at the cursor holds no code to run: only
    /// variables, atoms and literal terms (a tuple or binary of them).
    fn args_are_atomic(&self) -> bool {
        if !self.at_punct("(") {
            return false;
        }
        let mut depth = 0usize;
        for tok in &self.tokens[self.at..] {
            match tok {
                Tok::Punct("(" | "[" | "{" | "<" | "~{" | "#{") => depth += 1,
                Tok::Punct(")" | "]" | "}" | ">" | "}~" | "}#") => {
                    depth = depth.saturating_sub(1);
                    if depth == 0 {
                        return true;
                    }
                }
                Tok::Word(w) if is_keyword(w) => return false,
                _ => {}
            }
        }
        false
    }

    /// `( arg, ... )`: parses the arguments, so a list or map inside one is
    /// still checked.
    fn call_args(&mut self) {
        if self.eat_punct("(") {
            self.expr_list(")");
        }
    }

    /// A clause list up to (and including) the word `end`/`after` that closes
    /// it; whether any body may run code. Patterns and guards are skipped.
    fn clauses(&mut self, end: &str) -> Effect {
        let mut runs = QUIET;
        while self.peek().is_some() && !self.at_word(end) {
            self.skip_pattern();
            if self.eat_word("when") {
                self.expr();
            }
            if !self.eat_punct("->") {
                break;
            }
            runs = runs.max(self.expr());
        }
        if end == "end" {
            self.eat_word("end");
        }
        runs
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn found(code: &str) -> Vec<(RacingConstruct, usize)> {
        racing_operand_errors(code)
            .into_iter()
            .map(|e| match e {
                VerifyError::RacingOperands {
                    construct,
                    operands,
                    ..
                } => (construct, operands),
                other => panic!("unexpected finding {other:?}"),
            })
            .collect()
    }

    fn module(body: &str) -> String {
        format!("module 'm' ['f'/1]\n  attributes []\n'f'/1 =\n  fun (X) ->\n    {body}\nend\n")
    }

    #[test]
    fn two_calls_in_a_list_cell_race() {
        let code = module("[call 'm':'a'(X), call 'm':'b'(X)]");
        assert_eq!(found(&code), vec![(RacingConstruct::ListCell, 2)]);
    }

    #[test]
    fn a_list_tail_that_runs_code_races_with_the_head() {
        let code = module("[call 'm':'a'(X)|call 'm':'b'(X)]");
        assert_eq!(found(&code), vec![(RacingConstruct::ListCell, 2)]);
    }

    #[test]
    fn two_calls_in_a_map_literal_race() {
        let code = module("~{ 'k' => call 'm':'a'(X), 'j' => apply 'g'/1(X) }~");
        assert_eq!(found(&code), vec![(RacingConstruct::MapLiteral, 2)]);
    }

    #[test]
    fn variables_literals_and_one_call_do_not_race() {
        let code = module("[X, 'a', \"s\", 1, call 'm':'a'(X), Y]");
        assert!(found(&code).is_empty());
        let code = module("~{ 'k' => X, 'j' => call 'm':'a'(X) }~");
        assert!(found(&code).is_empty());
    }

    #[test]
    fn a_pure_projection_of_variables_does_not_race() {
        let code = module(
            "~{ call 'erlang':'element'(1, A) => call 'erlang':'element'(1, B), \
             'm' => call 'erlang':'element'(1, C) }~",
        );
        assert!(found(&code).is_empty());
        let code =
            module("[call 'erlang':'element'(1, A), call 'erlang':'element'(1, call 'm':'f'(B))]");
        assert!(found(&code).is_empty(), "one real call, one projection");
    }

    #[test]
    fn a_static_call_map_class_reference_races_with_nothing() {
        // BT-4756: the class literal's static-call-map read and its
        // `class_ref/1` miss path are one quiet operand, like the old `whereis`.
        let class_ref = "case call 'persistent_term':'get'('A class static calls', ~{}~) of <~{{'class_ref', 'A class'} := R}~> when 'true' -> R <_> when 'true' -> case call 'beamtalk_class_shadow_flags':'class_ref'('A') of <'undefined'> when 'true' -> call 'erlang':'error'('x') <C> when 'true' -> C end end";
        let code = module(&format!("[call 'm':'send'(X), {class_ref}]"));
        assert!(
            found(&code).is_empty(),
            "a class reference races with nothing"
        );
        // Any other `persistent_term` read followed by a call still runs code.
        let other = "case call 'persistent_term':'get'('k', ~{}~) of <R> when 'true' -> call 'm':'f'(R) end";
        let code = module(&format!("[call 'm':'send'(X), {other}]"));
        assert_eq!(found(&code), vec![(RacingConstruct::ListCell, 2)]);
    }

    #[test]
    fn a_process_state_read_races_a_call_but_not_another_read() {
        let read =
            "case call 'erlang':'get'(K) of <M> when 'true' -> call 'maps':'find'('n', M) end";
        let code = module(&format!("[{read}, {read}]"));
        assert!(
            found(&code).is_empty(),
            "two reads cannot disturb each other"
        );
        let code = module(&format!("[call 'm':'send'(X), {read}]"));
        assert_eq!(found(&code), vec![(RacingConstruct::ListCell, 2)]);
    }

    #[test]
    fn a_process_state_write_races_a_read() {
        let read =
            "case call 'erlang':'get'(K) of <M> when 'true' -> call 'maps':'find'('n', M) end";
        let write = "let V = 6 in case call 'erlang':'get'(K) of <M> when 'true' -> let _ = call 'erlang':'put'(K, V) in V <_> when 'true' -> call 'beamtalk_class_vars':'put'(K, V) end";
        let code = module(&format!("[{read}, {write}]"));
        assert_eq!(found(&code), vec![(RacingConstruct::ListCell, 2)]);
    }

    #[test]
    fn a_read_shaped_case_whose_clause_runs_code_races() {
        let read =
            "case call 'erlang':'get'(K) of <M> when 'true' -> call 'maps':'find'('n', M) end";
        let sneaky = "case call 'erlang':'get'(K) of <M> when 'true' -> call 'm':'send'(M) end";
        let code = module(&format!("[{sneaky}, {read}]"));
        assert_eq!(found(&code), vec![(RacingConstruct::ListCell, 2)]);
        // A `put` in a closure that is built and not called does not run.
        let lazy = "case call 'erlang':'get'(K) of <M> when 'true' -> fun () -> call 'erlang':'put'(K, M) end";
        let code = module(&format!("[{lazy}, {read}]"));
        assert!(found(&code).is_empty());
    }

    #[test]
    fn a_hoisted_operand_does_not_race() {
        let code = module("let T = call 'm':'a'(X) in [T, call 'm':'b'(X)]");
        assert!(found(&code).is_empty());
    }

    #[test]
    fn a_fun_body_runs_nothing_when_the_closure_is_built() {
        let code = module("[fun () -> call 'm':'a'(X), fun () -> call 'm':'b'(X)]");
        assert!(found(&code).is_empty());
    }

    #[test]
    fn a_case_and_a_let_around_a_call_may_run_code() {
        let code = module(
            "[case X of <'a'> when 'true' -> call 'm':'a'(X) <_> when 'true' -> 'b' end, \
             let Y = X in call 'm':'b'(Y)]",
        );
        assert_eq!(found(&code), vec![(RacingConstruct::ListCell, 2)]);
    }

    #[test]
    fn a_nested_list_in_a_call_argument_is_still_checked() {
        let code = module("call 'm':'a'([call 'm':'b'(X), call 'm':'c'(X)])");
        assert_eq!(found(&code), vec![(RacingConstruct::ListCell, 2)]);
    }

    #[test]
    fn annotations_strings_and_binaries_do_not_confuse_the_scan() {
        let code = module(
            "[(call 'm':'a'(\"]),[\" -| [{'file', \"x\"}]), \
             #{#<65>(8, 1, 'integer', ['unsigned'|['big']])}#, 'a]b', $], -1, 1.5e-3]",
        );
        assert!(found(&code).is_empty());
    }

    #[test]
    fn a_finding_names_the_function() {
        let code = module("[call 'm':'a'(X), call 'm':'b'(X)]");
        match racing_operand_errors(&code).as_slice() {
            [VerifyError::RacingOperands { function, .. }] => assert_eq!(function, "f"),
            other => panic!("{other:?}"),
        }
    }
}
