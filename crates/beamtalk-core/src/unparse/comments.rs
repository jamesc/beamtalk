// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Comment and comment-attachment unparsing.
//! Split from `mod.rs`; see the module docs there.

use super::{Comment, CommentAttachment, CommentKind, Document, docvec, leaf, line};

// --- Comment unparsing ---

/// Builds a [`Document`] for a single [`Comment`].
///
/// Line comments become `// content`, block comments become `/* content */`,
/// doc-style leading comments become `/// content`.
pub(super) fn unparse_comment(comment: &Comment) -> Document<'static> {
    match comment.kind {
        CommentKind::Line => {
            if comment.content.is_empty() {
                Document::Str("//")
            } else {
                docvec!["// ", leaf::raw_text(&comment.content)]
            }
        }
        CommentKind::Block => {
            docvec!["/* ", leaf::raw_text(&comment.content), " */"]
        }
        CommentKind::Doc => {
            if comment.content.is_empty() {
                Document::Str("///")
            } else {
                docvec!["/// ", leaf::raw_text(&comment.content)]
            }
        }
    }
}

/// Produces the leading-comments documents with trailing newlines.
///
/// Each leading comment is emitted followed by a `line()` so that the next
/// element starts on a fresh line.
pub(super) fn unparse_comment_attachment_leading(ca: &CommentAttachment) -> Vec<Document<'static>> {
    let mut docs: Vec<Document<'static>> = Vec::new();
    for (i, comment) in ca.leading.iter().enumerate() {
        // Emit blank line between comment groups (but not before the first —
        // the calling context already manages spacing before the attachment).
        if i > 0 && comment.preceding_blank_line {
            docs.push(line());
        }
        docs.push(unparse_comment(comment));
        docs.push(line());
    }
    docs
}

/// Whether a blank line must separate `comments.leading` from whatever
/// follows (a doc comment, or the declaration header when there is no doc
/// comment).
///
/// Only true when the *last* leading comment is [`CommentKind::Doc`] — a
/// `///` block that `collect_comment_attachment` preserved because a blank
/// line (or `//` comment) broke it away from the declaration it visually
/// precedes. Such a block is, by construction, never adjacent to
/// what follows in the original source — a blank line always separated it —
/// so reinserting one here reconstructs that gap and stops the preserved
/// block from visually gluing onto (and, on the next parse, merging into)
/// this declaration's own doc comment.
///
/// Ordinary leading comments (license headers, section notes) are
/// deliberately excluded: several declaration kinds (`Protocol define:`,
/// `state:`) have established, already-canonical stdlib source
/// where such comments sit directly against the following doc comment or
/// header with no blank line, and this must not force one in.
///
/// Checking only the *last* entry (not `any()` over the whole slice) is
/// correct because a [`CommentKind::Doc`] entry that *needs* a blank-line
/// separator (i.e., nothing in the original source separated it from what
/// follows) will always be the last item in `comments.leading`. If a
/// [`CommentKind::Line`] (`//` comment) follows an orphaned `///` block in
/// the trivia, it is appended to `leading` *after* the `Doc` entry and
/// already provides the separation — so this function correctly returns
/// `false` and no extra blank line is inserted.
pub(super) fn leading_ends_with_orphaned_doc_comment(comments: &CommentAttachment) -> bool {
    comments
        .leading
        .last()
        .is_some_and(|c| c.kind == CommentKind::Doc)
}
