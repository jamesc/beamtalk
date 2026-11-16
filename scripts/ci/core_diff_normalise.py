#!/usr/bin/env python3
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Make a corpus `.core` snapshot independent of the checkout directory (BT-4939).

`just core-diff-snapshot` writes generated Core Erlang that embeds the
absolute path of the source file it was compiled from. Two checkouts of the
same commit (the PR head and the base worktree in `target/core-diff/`) differ
in that prefix, so `core-diff` could never report "byte-identical".

Two encodings carry the path:

* a quoted string, `"/ci/work/beamtalk/stdlib/src/x.bt"` — rewritten by the
  `sed` in the `core-diff-snapshot` recipe;
* a binary literal, one segment per byte, as in `'__beamtalk_method_lines'/0`:
  `#{#<47>(8,1,'integer',['unsigned'|['big']]),#<104>(...),...}#`.

This script decodes every such binary literal; when its bytes are an absolute
path with a `/stdlib/` component it rewrites the literal to the same path
starting at `stdlib/`, byte for byte in the same encoding. Everything else —
including a different relative path or file name — is left untouched, so it
still shows up as a diff. Idempotent: an already-relative path is not an
absolute path, so a second run changes nothing (the base snapshot restored
from the CI cache may or may not have been normalised by this script).

With `--root DIR`, also fails if DIR (the checkout the snapshot was taken
from) still appears anywhere afterwards, as plain text or inside a decoded
byte literal. That is the guard for any *new* checkout-dependent output.

Usage: core_diff_normalise.py [--root DIR] SNAPSHOT_DIR
"""
import argparse
import os
import re
import sys

SEGMENT = r"#<\d+>\(8,1,'integer',\['unsigned'\|\['big'\]\]\)"
# `#{` ... `}#` with at least one segment; segments are comma-separated.
LITERAL = re.compile(r"#\{(" + SEGMENT + r"(?:," + SEGMENT + r")*)\}#")
BYTE = re.compile(r"#<(\d+)>")
# An absolute path whose first `/stdlib/` component starts the relative part.
ABSOLUTE_STDLIB = re.compile(rb"^/(?:.*?/)?stdlib/", re.DOTALL)


def decode(literal_body):
    return bytes(int(n) for n in BYTE.findall(literal_body))


def encode(data):
    seg = "#<{}>(8,1,'integer',['unsigned'|['big']])"
    return "#{" + ",".join(seg.format(b) for b in data) + "}#"


def normalise_bytes(data):
    """The bytes with a leading absolute `.../stdlib/` prefix reduced to `stdlib/`."""
    m = ABSOLUTE_STDLIB.match(data)
    if m is None:
        return data
    return b"stdlib/" + data[m.end():]


def normalise_text(text):
    def sub(m):
        old = decode(m.group(1))
        new = normalise_bytes(old)
        return m.group(0) if new == old else encode(new)

    return LITERAL.sub(sub, text)


def leaks(text, root):
    """True if `root` appears in `text`, plain or inside a decoded byte literal."""
    if root in text:
        return True
    needle = root.encode()
    return any(needle in decode(m.group(1)) for m in LITERAL.finditer(text))


def main(argv):
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--root", help="checkout root that must not remain in the output")
    ap.add_argument("snapshot_dir")
    args = ap.parse_args(argv)
    root = args.root.rstrip("/") if args.root else None

    changed = 0
    leaked = []
    for dirpath, _, names in os.walk(args.snapshot_dir):
        for name in sorted(names):
            if not name.endswith(".core"):
                continue
            path = os.path.join(dirpath, name)
            with open(path, encoding="utf-8", newline="") as f:
                text = f.read()
            new = normalise_text(text)
            if new != text:
                with open(path, "w", encoding="utf-8", newline="") as f:
                    f.write(new)
                changed += 1
            if root and leaks(new, root):
                leaked.append(path)
    print(f"core_diff_normalise: rewrote byte-literal paths in {changed} file(s)")
    if leaked:
        print(
            f"❌ core_diff_normalise: the checkout path {root!r} is still embedded in "
            f"{len(leaked)} file(s) after normalising, so snapshots would differ by "
            "checkout directory (BT-4939). Teach core_diff_normalise.py (or the "
            "sed in `core-diff-snapshot`) about the new encoding. First few:",
            file=sys.stderr,
        )
        for p in leaked[:5]:
            print(f"  {p}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
