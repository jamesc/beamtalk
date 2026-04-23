// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Builder for Erlang `-eval` expressions.
//!
//! Constructs `case` / `halt` / `io:put_chars(standard_error, ...)` snippets
//! structurally rather than via hand-escaped `format!()` strings, eliminating
//! a class of bugs around exit-code collisions, swallowed reasons, and
//! mis-escaped special characters.
//!
//! See BT-2059 for context.

/// Exit codes used by the generated Erlang expressions.
///
/// These are chosen to avoid collision with the Erlang VM's own exit codes
/// (e.g., `1` is used by `badmatch` crashes and must never be reused).
pub mod exit_codes {
    /// Operation completed successfully.
    pub const SUCCESS: u8 = 0;
    /// File needs formatting (check mode only). Distinct from VM crash (1).
    pub const NEEDS_FORMAT: u8 = 10;
    /// An error occurred (formatting, I/O, etc.). Reason printed to stderr.
    pub const ERROR: u8 = 2;
}

/// Builder for Erlang eval expressions used by the erlfmt integration.
///
/// Produces structured `case` expressions with uniform error handling:
/// every error branch prints the reason to stderr before calling `halt/1`.
pub struct ErlangEval {
    /// The file path, already escaped for embedding in an Erlang string.
    escaped_path: String,
}

impl ErlangEval {
    /// Create a new builder for the given file path.
    ///
    /// The path is escaped for safe embedding in Erlang string literals.
    pub fn new(file_path: &str) -> Self {
        Self {
            escaped_path: escape_for_erlang_string(file_path),
        }
    }

    /// Build the eval expression for **check mode**.
    ///
    /// Formats the file in memory and compares against the on-disk contents.
    /// Exit codes:
    /// - `0`: file is already formatted (or skipped)
    /// - `10`: file needs formatting
    /// - `2`: error (reason printed to stderr)
    pub fn check_expr(&self) -> String {
        let path = &self.escaped_path;
        // Build each branch separately for clarity, then compose.
        //
        // The overall structure is:
        //   case erlfmt:format_file(Path, Opts) of
        //     {ok, Formatted, _} ->
        //       case file:read_file(Path) of
        //         {ok, Original} ->
        //           Bin = unicode:characters_to_binary(Formatted),
        //           case Bin =:= Original of true -> halt(0); false -> halt(10) end;
        //         {error, ReadReason} -> <error_halt(ReadReason)>
        //       end;
        //     {skip, _} -> halt(0);
        //     {error, FmtReason} -> <error_halt(FmtReason)>
        //   end.
        let read_error = Self::error_halt("ReadReason");
        let fmt_error = Self::error_halt("FmtReason");

        format!(
            "case erlfmt:format_file(\"{path}\", [{{print_width, 100}}]) of \
                {{ok, Formatted, _}} -> \
                    case file:read_file(\"{path}\") of \
                        {{ok, Original}} -> \
                            Bin = unicode:characters_to_binary(Formatted), \
                            case Bin =:= Original of \
                                true -> halt({success}); \
                                false -> halt({needs_format}) \
                            end; \
                        {{error, ReadReason}} -> {read_error} \
                    end; \
                {{skip, _}} -> halt({success}); \
                {{error, FmtReason}} -> {fmt_error} \
            end.",
            success = exit_codes::SUCCESS,
            needs_format = exit_codes::NEEDS_FORMAT,
        )
    }

    /// Build the eval expression for **write mode**.
    ///
    /// Formats the file and writes the result back to disk.
    /// Exit codes:
    /// - `0`: file formatted successfully (or skipped)
    /// - `2`: error (reason printed to stderr)
    pub fn write_expr(&self) -> String {
        let path = &self.escaped_path;
        let write_error = Self::error_halt("WriteReason");
        let fmt_error = Self::error_halt("FmtReason");

        format!(
            "case erlfmt:format_file(\"{path}\", [{{print_width, 100}}]) of \
                {{ok, Formatted, _}} -> \
                    Bin = unicode:characters_to_binary(Formatted), \
                    case file:write_file(\"{path}\", Bin) of \
                        ok -> halt({success}); \
                        {{error, WriteReason}} -> {write_error} \
                    end; \
                {{skip, _}} -> halt({success}); \
                {{error, FmtReason}} -> {fmt_error} \
            end.",
            success = exit_codes::SUCCESS,
        )
    }

    /// Build the eval expression for the given mode.
    pub fn build(&self, check_only: bool) -> String {
        if check_only {
            self.check_expr()
        } else {
            self.write_expr()
        }
    }

    /// Generate an error-handling expression that prints `reason_var` to stderr
    /// and exits with the error exit code.
    ///
    /// This is the uniform error pattern: every error branch uses it, so
    /// reasons are never swallowed and the exit code is always consistent.
    fn error_halt(reason_var: &str) -> String {
        format!(
            "io:put_chars(standard_error, io_lib:format(\"~tp~n\", [{reason_var}])), \
             halt({error})",
            error = exit_codes::ERROR,
        )
    }
}

/// Escape a string for embedding in an Erlang string literal.
///
/// Handles backslashes, double-quotes, and control characters that the
/// Erlang parser would otherwise interpret.
pub fn escape_for_erlang_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\0' => out.push_str("\\0"),
            c => out.push(c),
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    // ---- escape_for_erlang_string tests ----

    #[test]
    fn escape_plain_string() {
        assert_eq!(escape_for_erlang_string("hello"), "hello");
    }

    #[test]
    fn escape_double_quotes() {
        assert_eq!(escape_for_erlang_string(r#"a"b"#), r#"a\"b"#);
    }

    #[test]
    fn escape_backslash() {
        assert_eq!(escape_for_erlang_string("a\\b"), "a\\\\b");
    }

    #[test]
    fn escape_newline() {
        assert_eq!(escape_for_erlang_string("a\nb"), "a\\nb");
    }

    #[test]
    fn escape_tab_and_carriage_return() {
        assert_eq!(escape_for_erlang_string("\t\r"), "\\t\\r");
    }

    #[test]
    fn escape_null() {
        assert_eq!(escape_for_erlang_string("\0"), "\\0");
    }

    #[test]
    fn escape_path_with_spaces_and_special_chars() {
        assert_eq!(
            escape_for_erlang_string("/tmp/my dir/file\"name\\.erl"),
            "/tmp/my dir/file\\\"name\\\\.erl"
        );
    }

    // ---- ErlangEval builder tests ----

    #[test]
    fn check_expr_contains_all_three_exit_codes() {
        let eval = ErlangEval::new("/tmp/test.erl");
        let expr = eval.check_expr();

        // halt(0) for success
        assert!(
            expr.contains("halt(0)"),
            "check expr must contain halt(0) for success"
        );
        // halt(10) for needs-formatting
        assert!(
            expr.contains("halt(10)"),
            "check expr must contain halt(10) for needs-formatting"
        );
        // halt(2) for errors
        assert!(
            expr.contains("halt(2)"),
            "check expr must contain halt(2) for errors"
        );
    }

    #[test]
    fn write_expr_contains_success_and_error_exit_codes() {
        let eval = ErlangEval::new("/tmp/test.erl");
        let expr = eval.write_expr();

        assert!(
            expr.contains("halt(0)"),
            "write expr must contain halt(0) for success"
        );
        assert!(
            expr.contains("halt(2)"),
            "write expr must contain halt(2) for errors"
        );
        // Write mode should NOT use halt(10)
        assert!(
            !expr.contains("halt(10)"),
            "write expr must not contain halt(10)"
        );
    }

    #[test]
    fn check_expr_propagates_read_reason_to_stderr() {
        let eval = ErlangEval::new("/tmp/test.erl");
        let expr = eval.check_expr();

        // ReadReason must appear in io:put_chars to stderr
        assert!(
            expr.contains("io:put_chars(standard_error, io_lib:format(\"~tp~n\", [ReadReason]))"),
            "check expr must print ReadReason to stderr; got: {expr}"
        );
    }

    #[test]
    fn check_expr_propagates_fmt_reason_to_stderr() {
        let eval = ErlangEval::new("/tmp/test.erl");
        let expr = eval.check_expr();

        assert!(
            expr.contains("io:put_chars(standard_error, io_lib:format(\"~tp~n\", [FmtReason]))"),
            "check expr must print FmtReason to stderr; got: {expr}"
        );
    }

    #[test]
    fn write_expr_propagates_write_reason_to_stderr() {
        let eval = ErlangEval::new("/tmp/test.erl");
        let expr = eval.write_expr();

        assert!(
            expr.contains("io:put_chars(standard_error, io_lib:format(\"~tp~n\", [WriteReason]))"),
            "write expr must print WriteReason to stderr; got: {expr}"
        );
    }

    #[test]
    fn write_expr_propagates_fmt_reason_to_stderr() {
        let eval = ErlangEval::new("/tmp/test.erl");
        let expr = eval.write_expr();

        assert!(
            expr.contains("io:put_chars(standard_error, io_lib:format(\"~tp~n\", [FmtReason]))"),
            "write expr must print FmtReason to stderr; got: {expr}"
        );
    }

    #[test]
    fn file_path_is_properly_escaped_in_expr() {
        let eval = ErlangEval::new("/tmp/my \"special\" dir/test.erl");
        let expr = eval.check_expr();

        assert!(
            expr.contains(r#"/tmp/my \"special\" dir/test.erl"#),
            "file path with quotes must be escaped in the expression; got: {expr}"
        );
        // Must not contain the raw unescaped quotes
        assert!(
            !expr.contains(r#"my "special" dir"#),
            "file path must not contain unescaped quotes"
        );
    }

    #[test]
    fn build_delegates_to_correct_mode() {
        let eval = ErlangEval::new("/tmp/test.erl");

        let check = eval.build(true);
        let write = eval.build(false);

        assert_eq!(check, eval.check_expr());
        assert_eq!(write, eval.write_expr());
    }

    #[test]
    fn check_expr_uses_erlfmt_format_file() {
        let eval = ErlangEval::new("/tmp/test.erl");
        let expr = eval.check_expr();

        assert!(
            expr.contains("erlfmt:format_file(\"/tmp/test.erl\", [{print_width, 100}])"),
            "must call erlfmt:format_file with correct path and options; got: {expr}"
        );
    }

    #[test]
    fn write_expr_uses_file_write_file() {
        let eval = ErlangEval::new("/tmp/test.erl");
        let expr = eval.write_expr();

        assert!(
            expr.contains("file:write_file(\"/tmp/test.erl\", Bin)"),
            "write mode must call file:write_file; got: {expr}"
        );
    }

    #[test]
    fn check_expr_uses_file_read_file() {
        let eval = ErlangEval::new("/tmp/test.erl");
        let expr = eval.check_expr();

        assert!(
            expr.contains("file:read_file(\"/tmp/test.erl\")"),
            "check mode must call file:read_file; got: {expr}"
        );
    }

    #[test]
    fn error_halt_format() {
        let halt = ErlangEval::error_halt("MyReason");
        assert_eq!(
            halt,
            "io:put_chars(standard_error, io_lib:format(\"~tp~n\", [MyReason])), halt(2)"
        );
    }
}
