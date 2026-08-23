// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! String utility functions for semantic analysis.
//!
//! **DDD Context:** Semantic Analysis (Shared Kernel)
//!
//! Contains general-purpose string algorithms used across analysis modules.

/// Split a type-string on `sep` while respecting parenthesis nesting.
///
/// e.g. `split_top_level("Outer(Inner(A, B), C), D", ',')` returns
/// `["Outer(Inner(A, B), C)", "D"]`, not four pieces. Empty trailing
/// segments are dropped (so `split_top_level("", sep)` is `[]`), and each
/// segment is trimmed.
///
/// This is the single nesting-aware scanner behind every "split a stored
/// type-name string on some top-level separator" need in the type checker:
/// type-parameter lists (`,`), union members (`|`), and intersection members
/// (`&`, see [`super::type_checker::validation`]'s
/// `split_intersection_type_string`, which layers "no separator found ⇒
/// `None`" semantics on top for its caller).
///
/// **Unbalanced-parens behaviour (BT-3089):** a stray extra `)` clamps depth
/// at zero via `saturating_sub` rather than going negative. Depth is only
/// ever incremented by `(`, so once it reaches zero a further unmatched `)`
/// cannot make later separators look "more nested" than they are — the
/// scanner recovers immediately instead of requiring an equal number of
/// extra `(` to compensate. Prior copies of this scanner disagreed here
/// (plain `depth -= 1` vs `saturating_sub`); `saturating_sub` is the
/// deliberately-chosen canonical behaviour since it degrades gracefully on
/// malformed input instead of compounding the damage.
pub(crate) fn split_top_level(s: &str, sep: char) -> Vec<&str> {
    let mut result = Vec::new();
    let mut depth: u32 = 0;
    let mut start = 0;
    for (i, c) in s.char_indices() {
        match c {
            '(' => depth += 1,
            ')' => depth = depth.saturating_sub(1),
            c if c == sep && depth == 0 => {
                result.push(s[start..i].trim());
                start = i + 1;
            }
            _ => {}
        }
    }
    let last = s[start..].trim();
    if !last.is_empty() {
        result.push(last);
    }
    result
}

/// Split a stored-string type name into `(base, args_slice)`.
///
/// Given `"Array(Integer)"` returns `("Array", Some("Integer"))`.
/// Given `"Result(Integer, Error)"` returns `("Result", Some("Integer, Error"))`.
/// Given `"Integer"` (no parentheses) returns `("Integer", None)`.
/// Given `"Array(Integer)extra"` (not terminated by `)`) falls back to
/// `("Array(Integer)extra", None)` — the caller should treat the string as an
/// opaque class name.
///
/// Shared by [`class_hierarchy::declared_type`](super::class_hierarchy::declared_type)
/// and [`type_checker::type_resolver`](super::type_checker::type_resolver) —
/// lives here, below both, rather than being duplicated upward into either
/// (BT-3089; `class_hierarchy` sits below `type_checker` in the dependency
/// graph and must not reach up into it).
///
/// **References:** BT-2025, BT-3089.
#[must_use]
pub(crate) fn split_generic_base(type_name: &str) -> (&str, Option<&str>) {
    match type_name.split_once('(') {
        Some((base, rest)) if rest.ends_with(')') => {
            let args = &rest[..rest.len() - 1];
            (base, Some(args))
        }
        _ => (type_name, None),
    }
}

/// Split a string of consecutive Erlang map literals at the top level.
///
/// Maps are delimited by `#{…}` with possible nesting (`{…}` also increments
/// depth). Binary literals (`<<…>>`) are skipped so that `<<",">>` content is
/// not treated as a separator. Splitting happens when depth returns to zero
/// after a closing `}`, and any trailing `,` / whitespace before the next map
/// is consumed. Returns each top-level map as a `&str` slice (inclusive of its
/// outer braces).
///
/// This is the companion scanner to [`split_top_level`] for the Erlang-map
/// content produced by `beamtalk_spec_reader.erl` — moved here from
/// `type_checker::native_types` so both map-aware and paren-aware scanners
/// live in the same shared leaf module.
///
/// **References:** ADR 0075; used by `type_checker::native_types`.
pub(crate) fn split_top_level_maps(input: &str) -> Vec<&str> {
    let mut result = Vec::new();
    let mut depth = 0i32;
    let mut start = 0;
    let bytes = input.as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        match bytes[i] {
            b'#' if i + 1 < bytes.len() && bytes[i + 1] == b'{' => {
                depth += 1;
                i += 2;
            }
            b'{' => {
                depth += 1;
                i += 1;
            }
            b'}' => {
                depth -= 1;
                if depth == 0 {
                    result.push(&input[start..=i]);
                    i += 1;
                    while i < bytes.len()
                        && (bytes[i] == b',' || bytes[i] == b' ' || bytes[i] == b'\n')
                    {
                        i += 1;
                    }
                    start = i;
                    continue;
                }
                i += 1;
            }
            b'<' if i + 1 < bytes.len() && bytes[i + 1] == b'<' => {
                i += 2;
                while i < bytes.len() {
                    if bytes[i] == b'>' && i + 1 < bytes.len() && bytes[i + 1] == b'>' {
                        i += 2;
                        break;
                    }
                    i += 1;
                }
            }
            _ => {
                i += 1;
            }
        }
    }

    if start < bytes.len() && depth == 0 {
        let remainder = input[start..].trim();
        if !remainder.is_empty() {
            result.push(remainder);
        }
    }

    result
}

/// Simple edit distance (Levenshtein) for "did you mean" suggestions.
pub(crate) fn edit_distance(a: &str, b: &str) -> usize {
    let a_chars: Vec<char> = a.chars().collect();
    let b_chars: Vec<char> = b.chars().collect();
    let m = a_chars.len();
    let n = b_chars.len();

    let mut dp = vec![vec![0usize; n + 1]; m + 1];
    for (i, row) in dp.iter_mut().enumerate().take(m + 1) {
        row[0] = i;
    }
    for (j, val) in dp[0].iter_mut().enumerate().take(n + 1) {
        *val = j;
    }

    for i in 1..=m {
        for j in 1..=n {
            let cost = usize::from(a_chars[i - 1] != b_chars[j - 1]);
            dp[i][j] = (dp[i - 1][j] + 1)
                .min(dp[i][j - 1] + 1)
                .min(dp[i - 1][j - 1] + cost);
        }
    }

    dp[m][n]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_edit_distance() {
        assert_eq!(edit_distance("abc", "abc"), 0);
        assert_eq!(edit_distance("abc", "abd"), 1);
        assert_eq!(edit_distance("abc", "abcd"), 1);
        assert_eq!(edit_distance("abc", "xyz"), 3);
        assert_eq!(edit_distance("lenght", "length"), 2);
    }

    // ---- split_top_level ----

    #[test]
    fn split_top_level_comma_simple() {
        assert_eq!(split_top_level("T, E", ','), vec!["T", "E"]);
    }

    #[test]
    fn split_top_level_comma_single() {
        assert_eq!(split_top_level("Integer", ','), vec!["Integer"]);
    }

    #[test]
    fn split_top_level_comma_nested() {
        assert_eq!(
            split_top_level("GenResult(A, B), E", ','),
            vec!["GenResult(A, B)", "E"]
        );
    }

    #[test]
    fn split_top_level_comma_empty() {
        assert_eq!(split_top_level("", ','), Vec::<&str>::new());
    }

    #[test]
    fn split_top_level_comma_deeply_nested() {
        assert_eq!(
            split_top_level("Outer(Inner(A, B), C), D", ','),
            vec!["Outer(Inner(A, B), C)", "D"]
        );
    }

    #[test]
    fn split_top_level_pipe_simple() {
        assert_eq!(split_top_level("String | nil", '|'), vec!["String", "nil"]);
    }

    #[test]
    fn split_top_level_pipe_inside_parametric_not_split() {
        assert_eq!(
            split_top_level("Result(String | Integer, Error)", '|'),
            vec!["Result(String | Integer, Error)"]
        );
    }

    #[test]
    fn split_top_level_pipe_mixed() {
        assert_eq!(
            split_top_level("List(String) | nil", '|'),
            vec!["List(String)", "nil"]
        );
    }

    #[test]
    fn split_top_level_ampersand() {
        assert_eq!(
            split_top_level("Printable & Serializable", '&'),
            vec!["Printable", "Serializable"]
        );
    }

    /// BT-3089: a stray unmatched `)` clamps depth at zero (`saturating_sub`)
    /// rather than going negative — so the very next top-level separator is
    /// still recognised as top-level, instead of requiring a matching extra
    /// `(` to "recover" first (the behaviour of the old plain `depth -= 1`
    /// copy this scanner replaces).
    #[test]
    fn split_top_level_unbalanced_closing_paren_recovers_immediately() {
        assert_eq!(split_top_level("A), B", ','), vec!["A)", "B"]);
    }

    // ---- split_top_level_maps ----

    #[test]
    fn split_top_level_maps_empty() {
        assert_eq!(split_top_level_maps(""), Vec::<&str>::new());
    }

    #[test]
    fn split_top_level_maps_single() {
        assert_eq!(split_top_level_maps("#{a => 1}"), vec!["#{a => 1}"]);
    }

    #[test]
    fn split_top_level_maps_two_maps() {
        assert_eq!(
            split_top_level_maps("#{a => 1}, #{b => 2}"),
            vec!["#{a => 1}", "#{b => 2}"]
        );
    }

    #[test]
    fn split_top_level_maps_nested_map() {
        assert_eq!(
            split_top_level_maps("#{a => #{x => 1}}, #{b => 2}"),
            vec!["#{a => #{x => 1}}", "#{b => 2}"]
        );
    }

    #[test]
    fn split_top_level_maps_binary_literal_not_split() {
        assert_eq!(
            split_top_level_maps("#{name => <<\"hello,world\">>}, #{v => 1}"),
            vec!["#{name => <<\"hello,world\">>}", "#{v => 1}"]
        );
    }

    #[test]
    fn split_top_level_maps_plain_braces_nested() {
        assert_eq!(
            split_top_level_maps("#{a => {1, 2}}, #{b => 3}"),
            vec!["#{a => {1, 2}}", "#{b => 3}"]
        );
    }

    // ---- split_generic_base ----

    #[test]
    fn split_generic_base_plain_name() {
        assert_eq!(split_generic_base("Integer"), ("Integer", None));
    }

    #[test]
    fn split_generic_base_single_arg() {
        assert_eq!(
            split_generic_base("Array(Integer)"),
            ("Array", Some("Integer"))
        );
    }

    #[test]
    fn split_generic_base_multiple_args() {
        assert_eq!(
            split_generic_base("Result(Integer, Error)"),
            ("Result", Some("Integer, Error"))
        );
    }

    #[test]
    fn split_generic_base_unterminated_treats_as_opaque() {
        assert_eq!(split_generic_base("Array(Integer"), ("Array(Integer", None));
    }
}
