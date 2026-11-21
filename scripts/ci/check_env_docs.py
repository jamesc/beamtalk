#!/usr/bin/env python3
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Fail when code reads an environment variable that docs/development/environment-variables.md omits (BT-5089).

Source of truth is the code: every ALL_CAPS name passed to a read of the
environment (Rust `env::var`/`var_os`/`env!`/`option_env!`, Erlang `os:getenv`,
Elixir `System.get_env`/`fetch_env!`/`System.put_env` readers, Rust consts
named `*ENV*`/`*VAR*` whose value is an ALL_CAPS literal) must appear in
backticks in the page. Names the page documents but no code reads are reported
too, so the page cannot keep a removed variable.

Cargo's own build-script variables are exempt (see CARGO_EXEMPT).
"""
import os
import re
import subprocess
import sys

PAGE = "docs/development/environment-variables.md"
CARGO_EXEMPT = {"OUT_DIR", "CARGO_MANIFEST_DIR", "CARGO_PKG_VERSION",
                "CARGO_CFG_TARGET_OS", "CARGO_CFG_TARGET_ENV"}
NAME = r'"([A-Z][A-Z0-9_]{2,})"'
READS = [
    re.compile(r'(?:env::var(?:_os)?|env!|option_env!)\(\s*' + NAME),
    re.compile(r'os:getenv\(\s*' + NAME),
    re.compile(r'System\.(?:get_env|fetch_env!?|put_env)\(\s*' + NAME),
    re.compile(r'(?:get_env|getenv)\(\s*' + NAME),
]
CONST = re.compile(r'const\s+[A-Z0-9_]*(?:ENV|VAR)[A-Z0-9_]*\s*:\s*&(?:\'static\s+)?str\s*=\s*' + NAME)
SOURCES = (".rs", ".erl", ".hrl", ".ex", ".exs", ".escript")


def used_names():
    files = subprocess.check_output(["git", "ls-files"], text=True).splitlines()
    found = {}
    for f in files:
        if not f.endswith(SOURCES) or f == PAGE:
            continue
        try:
            text = open(f, encoding="utf-8").read()
        except (OSError, UnicodeDecodeError):
            continue
        for rx in READS + [CONST]:
            for m in rx.finditer(text):
                found.setdefault(m.group(1), f)
    return {k: v for k, v in found.items() if k not in CARGO_EXEMPT}


def main():
    os.chdir(subprocess.check_output(["git", "rev-parse", "--show-toplevel"], text=True).strip())
    page = open(PAGE, encoding="utf-8").read()
    documented = set(re.findall(r"^\|\s*`([A-Z][A-Z0-9_]{2,})`", page, re.M))
    used = used_names()
    bad = [f"{n} (read in {f}) is not documented in {PAGE}" for n, f in sorted(used.items()) if n not in documented]
    bad += [f"{n} is documented in {PAGE} but no code reads it" for n in sorted(documented - set(used))]
    if bad:
        print("environment variable doc check failed:\n  " + "\n  ".join(bad))
        sys.exit(1)
    print(f"environment variables ok ({len(used)} documented)")


if __name__ == "__main__":
    main()
