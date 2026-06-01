#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Ratchet lint: flag workaround / limitation comments that lack a tracking-issue
# reference (BT-NNNN).
#
# Rationale (BT-2347): limitation/workaround comments are unfiled bug reports.
# A comment like "Uses timesRepeat: to avoid mutable local variables across block
# boundaries" or "class vars don't thread through to:do: loops — pre-existing
# limitation" documents a real defect that nobody filed. This lint turns every
# such comment into a forcing function: either it references a tracking issue
# (BT-NNNN), or CI fails.
#
# This mirrors the existing ratchet lints (ADR 0089). It ships with an allowlist
# snapshot of the current offenders so CI is green on introduction; only *new*
# unreferenced workaround comments fail.
#
# ── How to clear a failure ──────────────────────────────────────────────────
# When this lint flags a comment you added, the fix is almost always:
#   1. File (or find) a tracking issue for the limitation/workaround, then
#   2. Add its reference (BT-NNNN) to the same comment line or an adjacent line.
#      e.g.  // Workaround for the to:do: threading gap (BT-2308).
# That is the intended outcome — make the unfiled bug a filed bug.
#
# As a last resort, if the comment genuinely is not a limitation (a false
# positive), regenerate the allowlist to snapshot it:
#   scripts/ci/lint-workaround-comments.sh --update
# Review the diff before committing — only false positives belong there.
#
# Usage:
#   scripts/ci/lint-workaround-comments.sh            # lint (CI mode)
#   scripts/ci/lint-workaround-comments.sh --update   # regenerate the allowlist

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

ALLOWLIST="scripts/ci/workaround-comments-allowlist.txt"

# Marker phrases that flag a limitation / workaround. Case-insensitive.
# These were chosen from phrasing actually found in-tree (BT-2347); routine
# rationale like "to avoid <X>" is intentionally excluded as too noisy.
#
# Word-boundary markers (must stand as whole words; excludes test data such as
# `nonexistent_type_XXXX` or `does_XXX_understand`):
WORD_MARKERS='HACK|FIXME|XXX'
# Phrase markers (matched anywhere, case-insensitive):
PHRASE_MARKERS='workaround|work around|does ?n.t thread|does not thread|pre-existing limitation|known limitation|language limitation|parser limitation'

# A combined ERE used to find candidate lines.
MARKER_RE="(\\b(${WORD_MARKERS})\\b|(${PHRASE_MARKERS}))"

# Comment-line detector: a line is a comment if it contains a `//` (Rust/BT)
# or `%` (Erlang) comment introducer.
COMMENT_RE='(//|%)'

# Lines to ignore even if they match a marker. `elp:fixme` is a structured
# Erlang LS (ELP) suppression directive that references an ELP warning code
# (resolved by ELP, not Linear), not a freeform limitation note.
IGNORE_RE='elp:fixme'

# Tracking-issue reference. BT- followed by at least one digit (excludes the
# placeholder BT-XXX).
ISSUE_RE='BT-[0-9]+'

# Compute a stable fingerprint for an offending comment: path + a hash of the
# normalised comment text, so it survives line-number drift.
fingerprint() {
    # args: <path> <text>
    local path="$1" text="$2"
    local norm
    # Collapse whitespace and lowercase so trivial reformatting is stable.
    norm="$(printf '%s' "$text" | tr '[:upper:]' '[:lower:]' | tr -s '[:space:]' ' ' | sed 's/^ //; s/ $//')"
    printf '%s\t%s\n' "$path" "$(printf '%s' "$norm" | cksum | cut -d' ' -f1)"
}

# Collect current offenders: comment lines that match a marker but have no
# BT-NNNN reference on the same or an adjacent (±1) line.
#   - fd 3 receives a human-readable "path:lineno:text" report line.
#   - stdout receives the fingerprint.
collect_offenders() {
    local files
    # Tracked files plus untracked-but-not-ignored files, so newly added
    # offenders are caught before they are committed.
    files="$( { git ls-files '*.bt' '*.rs' '*.erl' '*.hrl'; \
                git ls-files --others --exclude-standard '*.bt' '*.rs' '*.erl' '*.hrl'; } \
              | sort -u )"

    local f
    while IFS= read -r f; do
        [ -n "$f" ] || continue
        local matches
        matches="$(grep -nIiE "$MARKER_RE" "$f" 2>/dev/null || true)"
        [ -n "$matches" ] || continue

        local m lineno text
        while IFS= read -r m; do
            [ -n "$m" ] || continue
            lineno="${m%%:*}"
            text="${m#*:}"

            # Only consider comment lines.
            printf '%s' "$text" | grep -qE "$COMMENT_RE" || continue

            # Skip structured tool directives that are not limitation notes.
            printf '%s' "$text" | grep -qiE "$IGNORE_RE" && continue

            # Allowed if BT-NNNN appears on this line or an adjacent line.
            local lo=$((lineno - 1))
            local hi=$((lineno + 1))
            [ "$lo" -lt 1 ] && lo=1
            if sed -n "${lo},${hi}p" "$f" | grep -qE "$ISSUE_RE"; then
                continue
            fi

            printf '%s:%s:%s\n' "$f" "$lineno" "$text" >&3
            fingerprint "$f" "$text"
        done <<< "$matches"
    done <<< "$files"
}

# ── Update mode: regenerate the allowlist from current offenders ─────────────
if [ "${1:-}" = "--update" ]; then
    {
        echo "# Allowlist for lint-workaround-comments (BT-2347)."
        echo "# Snapshot of pre-existing workaround/limitation comments lacking a BT-NNNN ref."
        echo "# Each line: <path><TAB><text-hash>. Regenerate with --update; do NOT hand-edit"
        echo "# unless adding a genuine false positive. New offenders must add a BT-NNNN ref."
        collect_offenders 3>/dev/null | sort -u
    } > "$ALLOWLIST"
    echo "✅ Wrote allowlist: $ALLOWLIST ($(grep -cvE '^#' "$ALLOWLIST") entries)"
    exit 0
fi

# ── Lint mode ────────────────────────────────────────────────────────────────
echo "🔍 Linting for unreferenced workaround/limitation comments..."

if [ ! -f "$ALLOWLIST" ]; then
    echo "❌ Allowlist not found: $ALLOWLIST" >&2
    echo "   Run: scripts/ci/lint-workaround-comments.sh --update" >&2
    exit 1
fi

# Human-readable report goes to a temp file via fd 3; fingerprints to stdout.
REPORT="$(mktemp)"
trap 'rm -f "$REPORT"' EXIT

CURRENT="$(collect_offenders 3>"$REPORT" | sort -u)"
ALLOWED="$(grep -vE '^[[:space:]]*#' "$ALLOWLIST" | grep -vE '^[[:space:]]*$' | sort -u || true)"

# New offenders = current fingerprints not present in the allowlist.
NEW_FINGERPRINTS="$(comm -23 <(printf '%s\n' "$CURRENT") <(printf '%s\n' "$ALLOWED") || true)"

if [ -n "$NEW_FINGERPRINTS" ]; then
    echo "" >&2
    echo "❌ New workaround/limitation comment(s) without a BT-NNNN tracking reference:" >&2
    echo "" >&2
    # Map each new fingerprint back to its report line(s) for a useful message.
    while IFS= read -r fp; do
        [ -n "$fp" ] || continue
        new_path="${fp%%$'\t'*}"
        new_hash="${fp##*$'\t'}"
        while IFS= read -r rline; do
            rpath="${rline%%:*}"
            rest="${rline#*:}"
            rno="${rest%%:*}"
            rtext="${rest#*:}"
            [ "$rpath" = "$new_path" ] || continue
            if [ "$(fingerprint "$rpath" "$rtext" | cut -f2)" = "$new_hash" ]; then
                printf '  %s:%s:%s\n' "$rpath" "$rno" "$(printf '%s' "$rtext" | sed 's/^[[:space:]]*//')" >&2
            fi
        done < "$REPORT"
    done <<< "$NEW_FINGERPRINTS"
    echo "" >&2
    echo "Fix: add a tracking-issue reference (BT-NNNN) on the same or an adjacent line." >&2
    echo "     File the limitation as a Linear issue first if one does not exist." >&2
    echo "     See the header of $0 for details." >&2
    exit 1
fi

# Stale-entry check: warn if the allowlist references offenders that no longer
# exist, so the ratchet keeps tightening as comments get fixed.
STALE="$(comm -13 <(printf '%s\n' "$CURRENT") <(printf '%s\n' "$ALLOWED") || true)"
if [ -n "$STALE" ] && [ -n "$(printf '%s' "$STALE" | tr -d '[:space:]')" ]; then
    echo "ℹ️  Allowlist has $(printf '%s\n' "$STALE" | grep -c .) stale entr(y/ies) (offender fixed)." >&2
    echo "   Tighten the ratchet: scripts/ci/lint-workaround-comments.sh --update" >&2
fi

echo "✅ No unreferenced workaround/limitation comments."
