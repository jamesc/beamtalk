#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# PostToolUse hook: after editing a Rust or Erlang file, run a fast compile
# check and surface any errors/warnings directly to Claude.
#
# Stdout is injected into Claude's context as a system message.
# Exit 0 always — this hook informs, never blocks.

set -uo pipefail

# Parse file_path out of the JSON payload on stdin.
INPUT=$(cat)
FILE_PATH=$(python3 -c "
import sys, json
d = json.load(sys.stdin)
print(d.get('tool_input', {}).get('file_path', ''))
" <<< "$INPUT" 2>/dev/null || true)

[[ -z "$FILE_PATH" ]] && exit 0

case "$FILE_PATH" in
  *.rs)
    # Scope the check to the edited file's crate (nearest enclosing Cargo.toml):
    # pointing --manifest-path at a workspace member checks that package only,
    # not the whole workspace. Fall back to an unscoped check when none is found.
    MANIFEST=""
    DIR=$(dirname "$FILE_PATH")
    while [[ "$DIR" != "/" && "$DIR" != "." && -n "$DIR" ]]; do
      if [[ -f "$DIR/Cargo.toml" ]]; then
        MANIFEST="$DIR/Cargo.toml"
        break
      fi
      DIR=$(dirname "$DIR")
    done
    if [[ -n "$MANIFEST" ]]; then
      OUTPUT=$(cargo check --quiet --manifest-path "$MANIFEST" 2>&1)
    else
      OUTPUT=$(cargo check --quiet 2>&1)
    fi
    RC=$?
    if [[ $RC -ne 0 ]]; then
      echo "cargo check failed after editing $(basename "$FILE_PATH"):"
      echo "$OUTPUT" | head -30
    else
      # Surface warnings even on success (cargo check --quiet still emits them)
      WARNS=$(echo "$OUTPUT" | grep -E "^warning:" | head -10)
      if [[ -n "$WARNS" ]]; then
        echo "Rust warnings after editing $(basename "$FILE_PATH"):"
        echo "$WARNS"
      fi
    fi
    ;;

  *.erl|*.hrl)
    RUNTIME_DIR="${CLAUDE_PROJECT_DIR:-$(pwd)}/runtime"
    # Same lock as the Justfile's `build-erlang` and the SessionStart warmer
    # (rebar3 has no guard against two writers to one runtime/_build). flock is
    # Linux-only; without it (macOS) compile unguarded, as the Justfile does.
    if command -v flock >/dev/null 2>&1; then
      OUTPUT=$(cd "$RUNTIME_DIR" && flock .rebar3-compile.lock rebar3 compile 2>&1)
    else
      OUTPUT=$(cd "$RUNTIME_DIR" && rebar3 compile 2>&1)
    fi
    RC=$?

    if [[ $RC -ne 0 ]]; then
      echo "rebar3 compile failed after editing $(basename "$FILE_PATH"):"
      # Print first 30 lines of output — erlc errors include file:line context
      echo "$OUTPUT" | head -30
    else
      # Warn on any new warnings (rebar3 prefixes them with the app name)
      WARNS=$(echo "$OUTPUT" | grep -E "^(warning:|.*\.erl:[0-9]+: Warning:)" | head -10)
      if [[ -n "$WARNS" ]]; then
        echo "Erlang warnings after editing $(basename "$FILE_PATH"):"
        echo "$WARNS"
      fi
    fi
    ;;
esac

exit 0
