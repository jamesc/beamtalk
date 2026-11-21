#!/usr/bin/env python3
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Generate docs/cli-workspace.md from `beamtalk workspace --help` (BT-5089).

Usage: gen_cli_reference.py [--check] [BEAMTALK_BIN]

Writes the page, or with --check fails when the committed page differs from
what the binary prints. The page is only the clap help text, so it cannot
disagree with the CLI.
"""
import re
import subprocess
import sys

OUT = "docs/cli-workspace.md"


def help_of(binary, *args):
    return subprocess.check_output([binary, *args, "--help"], text=True, stderr=subprocess.STDOUT).rstrip()


def subcommands(text):
    m = re.search(r"^Commands:\n((?:  .*\n?)+)", text, re.M)
    names = [l.split()[0] for l in m.group(1).splitlines() if l.strip()] if m else []
    return [n for n in names if n != "help"]


def render(binary):
    top = help_of(binary, "workspace")
    out = [
        "# `beamtalk workspace`",
        "",
        "Generated from the clap help of the `beamtalk` binary by `scripts/gen_cli_reference.py`;",
        "`just check-cli-reference` fails when this page is out of date. Regenerate with",
        "`just gen-cli-reference`. Workspace state lives under `BEAMTALK_HOME` (default `~/.beamtalk`);",
        "see [environment-variables](development/environment-variables.md).",
        "",
        "## `beamtalk workspace`",
        "",
        "```text",
        top,
        "```",
    ]
    for sub in subcommands(top):
        out += ["", f"## `beamtalk workspace {sub}`", "", "```text", help_of(binary, "workspace", sub), "```"]
    return "\n".join(out) + "\n"


def main():
    args = [a for a in sys.argv[1:] if a != "--check"]
    binary = args[0] if args else "target/debug/beamtalk"
    text = render(binary)
    if "--check" in sys.argv:
        if open(OUT, encoding="utf-8").read() != text:
            print(f"{OUT} is out of date; run `just gen-cli-reference`")
            sys.exit(1)
        print("cli reference ok")
    else:
        open(OUT, "w", encoding="utf-8").write(text)


if __name__ == "__main__":
    main()
