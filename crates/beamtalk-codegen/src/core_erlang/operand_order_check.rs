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
//! The scan is a recursive-descent pass over the Core Erlang the generator
//! prints (`Document::to_pretty_string`), answering for each expression whether
//! evaluating it may run code. A `fun` body does not run when the closure is
//! built, so it does not count. Patterns and guards are skipped.
//!
//! The driver runs it on every generated module in debug builds
//! (`generate_module_with_warnings`), so `just test-stdlib`, `just test-bunit`
//! and every codegen unit test — the `verify-threaded-ir` corpus — fail on a
//! reintroduced in-place builder.

use super::threaded_ir::{RacingConstruct, VerifyError};

/// The racing operand lists of Core Erlang `code`: one finding per list cell or
/// map literal with two or more operands that may run code. Empty for a module
/// whose operand lists are all sequenced (or that the scanner cannot read —
/// the scan never invents a finding from text it does not understand).
pub(super) fn racing_operand_errors(code: &str) -> Vec<VerifyError> {
    let tokens = tokenize(code);
    let mut scan = Scan {
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

fn tokenize(code: &str) -> Vec<Tok> {
    let b = code.as_bytes();
    let mut out = Vec::new();
    let mut i = 0;
    while i < b.len() {
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
                while i < b.len() && (b[i].is_ascii_alphanumeric() || matches!(b[i], b'.' | b'_'))
                {
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
                while i < b.len() && (b[i].is_ascii_alphanumeric() || matches!(b[i], b'_' | b'@'))
                {
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
    }
    out
}

fn is_keyword(word: &str) -> bool {
    matches!(
        word,
        "call" | "apply" | "primop" | "let" | "letrec" | "in" | "case" | "of" | "end" | "fun"
            | "try" | "catch" | "receive" | "after" | "do" | "when"
    )
}

/// The BIFs whose call over variables and literals is a pure projection: the
/// codegen's `self_dispatch_result_value` / state-read shapes.
fn is_pure_projection(module: Option<&str>, function: Option<&str>) -> bool {
    matches!(
        (module, function),
        (
            Some("erlang"),
            Some("element" | "hd" | "tl" | "tuple_size" | "map_size")
        )
    )
}

struct Scan {
    tokens: Vec<Tok>,
    at: usize,
    /// The top-level function being scanned, for the finding.
    function: String,
    found: Vec<VerifyError>,
}

impl Scan {
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
    fn expr_list(&mut self, close: &str) -> bool {
        let mut runs = false;
        while self.peek().is_some() && !self.at_punct(close) {
            runs |= self.expr();
            if !self.eat_punct(",") && !self.at_punct(close) {
                break;
            }
        }
        self.eat_punct(close);
        runs
    }

    /// Records a finding for a list or map holding `racing` operands that may
    /// run code.
    fn note(&mut self, construct: RacingConstruct, racing: usize) {
        if racing >= 2 {
            self.found.push(VerifyError::RacingOperands {
                construct,
                function: self.function.clone(),
                operands: racing,
            });
        }
    }

    /// Parses one expression; whether evaluating it may run code.
    #[allow(clippy::too_many_lines)] // one arm per Core Erlang form
    fn expr(&mut self) -> bool {
        let Some(tok) = self.bump() else {
            return false;
        };
        match tok {
            Tok::Atom(_) => {
                // A function name: `'f'/1`.
                if self.at_punct("/") {
                    self.at += 2;
                }
                false
            }
            Tok::Lit => false,
            Tok::Word(w) => match w.as_str() {
                "let" => {
                    self.skip_pattern();
                    self.eat_punct("=");
                    let bound = self.expr();
                    self.eat_word("in");
                    let body = self.expr();
                    bound | body
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
                    let scrutinee = self.expr();
                    self.eat_word("of");
                    scrutinee | self.clauses("end")
                }
                "receive" => {
                    let clauses = self.clauses("after");
                    self.eat_word("after");
                    let timeout = self.expr();
                    self.eat_punct("->");
                    clauses | timeout | self.expr()
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
                    body | ok | self.expr()
                }
                "fun" => {
                    if self.at_punct("(") {
                        self.skip_group();
                    }
                    self.eat_punct("->");
                    // The body runs when the closure is called, not built.
                    self.expr();
                    false
                }
                "do" => {
                    let first = self.expr();
                    first | self.expr()
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
                    // A pure projection of bound variables and literals
                    // reads nothing a sibling can change and changes nothing a
                    // sibling reads, so its position among the operands is
                    // unobservable.
                    !(atomic && is_pure_projection(module.as_deref(), function.as_deref()))
                }
                "apply" => {
                    self.expr_operand();
                    self.call_args();
                    true
                }
                "primop" => {
                    self.bump();
                    self.call_args();
                    true
                }
                // A variable (or an unknown word): reads, runs nothing.
                _ => false,
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
                let mut racing = 0;
                while self.peek().is_some() && !self.at_punct("]") {
                    racing += usize::from(self.expr());
                    if !(self.eat_punct(",") || self.eat_punct("|")) {
                        break;
                    }
                }
                self.eat_punct("]");
                self.note(RacingConstruct::ListCell, racing);
                racing > 0
            }
            Tok::Punct("~{") => {
                let mut racing = 0;
                while self.peek().is_some() && !self.at_punct("}~") {
                    racing += usize::from(self.expr());
                    if self.eat_punct("=>") || self.eat_punct(":=") {
                        racing += usize::from(self.expr());
                    }
                    if !(self.eat_punct(",") || self.eat_punct("|")) {
                        break;
                    }
                }
                self.eat_punct("}~");
                self.note(RacingConstruct::MapLiteral, racing);
                racing > 0
            }
            Tok::Punct("#{") => {
                // Binary segments: `#<Value>(Size, Unit, Type, Flags)`.
                let mut runs = false;
                while self.peek().is_some() && !self.at_punct("}#") {
                    self.eat_punct("#");
                    if self.eat_punct("<") {
                        runs |= self.expr();
                        self.eat_punct(">");
                    }
                    if self.eat_punct("(") {
                        runs |= self.expr_list(")");
                    }
                    if !self.eat_punct(",") {
                        break;
                    }
                }
                self.eat_punct("}#");
                runs
            }
            Tok::Punct(_) => false,
        }
    }

    /// The module, function or callee operand of a `call`/`apply`: an atom, a
    /// variable or a parenthesised expression — never itself a run of code in
    /// generated modules.
    fn expr_operand(&mut self) {
        self.expr();
    }

    /// The quoted atom at the cursor, without consuming it.
    fn peek_atom(&self) -> Option<String> {
        match self.peek() {
            Some(Tok::Atom(a)) => Some(a.clone()),
            _ => None,
        }
    }

    /// Whether the `( arg, ... )` at the cursor holds only variables, atoms and
    /// literals.
    fn args_are_atomic(&self) -> bool {
        if !self.at_punct("(") {
            return false;
        }
        let mut i = self.at + 1;
        let mut expect_item = true;
        while let Some(tok) = self.tokens.get(i) {
            match tok {
                Tok::Punct(")") => return true,
                Tok::Punct(",") if !expect_item => expect_item = true,
                Tok::Lit | Tok::Atom(_) if expect_item => expect_item = false,
                Tok::Word(w) if expect_item && !is_keyword(w) => expect_item = false,
                _ => return false,
            }
            i += 1;
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
    fn clauses(&mut self, end: &str) -> bool {
        let mut runs = false;
        while self.peek().is_some() && !self.at_word(end) {
            self.skip_pattern();
            if self.eat_word("when") {
                self.expr();
            }
            if !self.eat_punct("->") {
                break;
            }
            runs |= self.expr();
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
        let code = module("[call 'erlang':'element'(1, A), call 'erlang':'element'(1, call 'm':'f'(B))]");
        assert!(found(&code).is_empty(), "one real call, one projection");
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
