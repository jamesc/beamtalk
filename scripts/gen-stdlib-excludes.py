#!/usr/bin/env python3
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Auto-generate dialyzer exclude_mods and cover_excl_mods for rebar.config.

Scans runtime/apps/beamtalk_stdlib/ebin/ for bt@stdlib@*.beam files (compiled
from Beamtalk source by the Beamtalk compiler, not erlc) and updates both
exclusion lists in runtime/rebar.config.

Run after adding or removing stdlib Beamtalk classes:
    just update-stdlib-excludes

Or manually:
    python3 scripts/gen-stdlib-excludes.py
"""

import os
import re
import sys


def find_repo_root():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    return os.path.dirname(script_dir)


def collect_bt_stdlib_modules(repo_root):
    ebin_dir = os.path.join(repo_root, "runtime", "apps", "beamtalk_stdlib", "ebin")
    if not os.path.isdir(ebin_dir):
        print(f"error: ebin dir not found: {ebin_dir}", file=sys.stderr)
        print("Run 'just build-stdlib' first.", file=sys.stderr)
        sys.exit(1)

    modules = sorted(
        os.path.splitext(f)[0]
        for f in os.listdir(ebin_dir)
        if f.startswith("bt@stdlib@") and f.endswith(".beam")
    )
    if not modules:
        print(f"error: no bt@stdlib@*.beam files found in {ebin_dir}", file=sys.stderr)
        sys.exit(1)
    return modules


def format_module_list(modules, indent):
    """Format a list of module atoms as a multi-line Erlang list body (no brackets)."""
    # 4 atoms per line to match existing rebar.config style
    per_line = 4
    lines = []
    for i in range(0, len(modules), per_line):
        chunk = modules[i : i + per_line]
        lines.append(indent + ", ".join(f"'{m}'" for m in chunk))
    return lines


def update_section(content, section_name, new_lines, close_indent=""):
    """Replace the module list inside a named rebar.config tuple.

    Handles both top-level tuples ({name, [...]}.  with trailing dot) and
    nested tuples ({name, [...]})  without a trailing dot).

    close_indent: whitespace to prefix the closing ]} line (e.g. "    " for
    nested sections so the bracket aligns with the opening {name, [}).
    """
    # Match {section_name, [  ...content...  ]} with an optional trailing dot.
    # [^\]]* stops at the first ] so it works for single-depth lists.
    pattern = re.compile(
        r"(\{" + re.escape(section_name) + r",\s*\[)[^\]]*(\]\}\.?)",
        re.DOTALL,
    )
    replacement = r"\g<1>\n" + ",\n".join(new_lines) + r"\n" + close_indent + r"\g<2>"
    new_content, count = pattern.subn(replacement, content)
    if count == 0:
        print(f"warning: section '{section_name}' not found in rebar.config", file=sys.stderr)
    return new_content


def main():
    repo_root = find_repo_root()
    rebar_config = os.path.join(repo_root, "runtime", "rebar.config")

    modules = collect_bt_stdlib_modules(repo_root)
    print(f"Found {len(modules)} bt@stdlib@* modules")

    with open(rebar_config) as f:
        content = f.read()

    # cover_excl_mods is top-level: 4-space content indent, no closing indent
    content = update_section(content, "cover_excl_mods", format_module_list(modules, "    "))
    # exclude_mods is nested inside {dialyzer, [...]}: 8-space content, 4-space closing
    content = update_section(content, "exclude_mods", format_module_list(modules, "        "), close_indent="    ")

    with open(rebar_config, "w") as f:
        f.write(content)

    print(f"Updated {rebar_config}")
    print(f"  cover_excl_mods: {len(modules)} modules")
    print(f"  dialyzer exclude_mods: {len(modules)} modules")


if __name__ == "__main__":
    main()
