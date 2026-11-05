#!/usr/bin/env python3
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

"""Generate the BT-4460 plain-arm nested-construct grid (ADR 0131).

A plain arm is the arm of a conditional that writes no outer local
(`generate_plain_arm`). Inside it, a nested inline construct writes a local of
that arm (`m`). The grid runs every cell on a BEAM:

    nested selector  x  statement position  x  write placement  x  writer
                     x  class kind

- nested selector: `ifTrue:ifFalse:`, `ifTrue:`, `ifFalse:`, `ifNil:ifNotNil:`,
  `ifNil:`, `ifNotNil:`, `and:`, `or:`, `on:do:`, `ensure:`, `match:`;
- statement position of the nested construct in the plain arm: a non-last
  statement, an assignment's value, the arm's last expression;
- write placement of the arm-local: written before the nested construct and
  inside it ahead of its value (`pre`), as its value (`in`), and again after
  it (`post`);
- writer: an assignment (`m := m + 10`) or a loop (`#(1, 2) do: [...]`,
  BT-4367's shape: a loop inside a construct nested in a plain arm);
- class kind: actor, value and `Object` (class-side) methods.

Each cell is two methods, the body inside the plain arm (`...Arm:`) and the
same body at the method root (`...Root`), and one test asserting that both
answer the value the small model below computes. So a cell fails when the
compiler's answer differs from what the language says, wherever it lowers
the body. Re-run after changing the grid:

    python3 scripts/gen-bt4460-plain-arm-nested-grid.py

Outputs (checked in, then formatted with `beamtalk fmt`):
    stdlib/test/fixtures/bt4460plain_arm_nested_{actor,value,object}.bt
    stdlib/test/bt4460plain_arm_nested_grid_test.bt
"""

import pathlib
import subprocess

ROOT = pathlib.Path(__file__).resolve().parent.parent
FIXTURES = ROOT / "stdlib/test/fixtures"
TEST = ROOT / "stdlib/test/bt4460plain_arm_nested_grid_test.bt"

HEADER = "// Copyright 2026 James Casey\n// SPDX-License-Identifier: Apache-2.0\n"

# The arm-local starts at this value before the nested construct.
M0 = 1

# writer name -> (statement, effect on m)
WRITERS = {
    "assign": ("m := m + 10", lambda m: m + 10),
    "loop": ("#(1, 2) do: [:e | m := m + e]", lambda m: m + 3),
}

PLACEMENTS = ["pre", "in", "post"]
POSITIONS = ["nonlast", "assign", "last"]

# selector name -> (uses c, uses z, boolean arm value)
SELECTORS = {
    "ifTrue_ifFalse": (True, False, False),
    "ifTrue": (True, False, False),
    "ifFalse": (True, False, False),
    "ifNil_ifNotNil": (False, True, False),
    "ifNil": (False, True, False),
    "ifNotNil": (True, False, False),
    "and": (True, False, True),
    "or": (True, False, True),
    "on_do": (False, False, False),
    "ensure": (False, False, False),
    "match": (False, False, False),
}


def nested(selector, arm):
    """The nested construct, whose taken arm runs the statements `arm`."""
    body = ". ".join(arm)
    return {
        "ifTrue_ifFalse": f"c ifTrue: [{body}] ifFalse: [0]",
        "ifTrue": f"c ifTrue: [{body}]",
        "ifFalse": f"c not ifFalse: [{body}]",
        "ifNil_ifNotNil": f"z ifNil: [{body}] ifNotNil: [:x | 0]",
        "ifNil": f"z ifNil: [{body}]",
        "ifNotNil": f"c ifNotNil: [:x | {body}]",
        "and": f"c and: [{body}]",
        "or": f"c not or: [{body}]",
        "on_do": f"[{body}] on: Error do: [:e | 0]",
        "ensure": f"[{body}] ensure: [nil]",
        "match": f"3 match: [3 -> [{body}] value; _ -> 0]",
    }[selector]


def cell_cases(kind):
    """Every (selector, position, placement, writer) cell of `kind` that is
    meaningful and that the validator accepts.

    In a class method a plain arm is not a frame (`is_plain_arm_conditional`),
    so a nested construct writing an arm-local keeps the position error as an
    assignment's value, and so does an evaluated block in a `match:` arm: those
    cells are left out there (the same on main).
    """
    for selector in SELECTORS:
        for position in POSITIONS:
            if kind == "object" and (position == "assign" or selector == "match"):
                continue
            for placement in PLACEMENTS:
                for writer in WRITERS:
                    # The arm's last expression has nothing after it.
                    if position == "last" and placement == "post":
                        continue
                    # A loop as the construct's value answers `nil`, which a
                    # boolean selector's arm cannot answer.
                    if placement == "in" and writer == "loop" and SELECTORS[selector][2]:
                        continue
                    yield selector, position, placement, writer


def render(value):
    if value is None:
        return "nil"
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, tuple):
        return "{" + ", ".join(render(v) for v in value) + "}"
    if isinstance(value, list):
        return "#(" + ", ".join(render(v) for v in value) + ")"
    return str(value)


def cell(selector, position, placement, writer):
    """The body statements and the value the language says they answer."""
    boolean = SELECTORS[selector][2]
    stmt, effect = WRITERS[writer]
    m = effect(M0)
    if placement == "in":
        if writer == "loop":
            arm = [stmt]
            # `do:` answers `nil` (language features, § local threading).
            value = None
        elif boolean:
            arm = [f"({stmt}) > 0"]
            value = True
        else:
            arm = [stmt]
            value = m
    else:
        tail = "m > 0" if boolean else "m + 100"
        arm = [stmt, tail]
        value = True if boolean else m + 100
    construct = nested(selector, arm)
    after = []
    if placement == "post":
        after = ["m := m * 2"]
        m_final = m * 2
    else:
        m_final = m
    if position == "nonlast":
        body = [f"m := {M0}", construct] + after + ["m"]
        answer = m_final
    elif position == "assign":
        body = [f"m := {M0}", f"v := {construct}"] + after + ["{v, m}"]
        answer = (value, m_final)
    else:
        body = [f"m := {M0}", construct]
        answer = value
    return body, answer


def indent(lines, n):
    return [" " * n + line for line in lines]


KINDS = {
    # kind -> (class name, superclass, class-side, slot line, construct)
    "actor": ("Bt4460PlainArmNestedActor", "Actor", False, "state: n = 0", "{cls} spawn"),
    "value": ("Bt4460PlainArmNestedValue", "Value", False, "field: n = 0", "{cls} new"),
    # An `Object` subclass is not instantiable: its methods are class-side.
    "object": ("Bt4460PlainArmNestedObject", "Object", True, None, "{cls}"),
}


def selector_name(base, uses_c, uses_z, arm):
    """`base` with its keywords: `Arm:` takes `c` always, `z` when needed."""
    params = []
    if arm or uses_c:
        params.append("c")
    if uses_z:
        params.append("z")
    if not params:
        return base, base
    keywords = [f"{base}{'Arm' if arm else 'Root'}:"] + [f"{p}:" for p in params[1:]]
    header = " ".join(f"{k} {p}" for k, p in zip(keywords, params))
    call = " ".join(f"{k} {v}" for k, v in zip(keywords, ["true" if p == "c" else "nil" for p in params]))
    return header, call


def fixture(kind):
    cls, superclass, class_side, slot, _ = KINDS[kind]
    prefix = "class " if class_side else ""
    out = [
        HEADER,
        "// BT-4460: differential fixture for the plain-arm nested-construct grid",
        f"// ({kind} methods). Generated by scripts/gen-bt4460-plain-arm-nested-grid.py;",
        "// do not edit by hand. Each cell is written twice: inside the arm of a",
        "// conditional that writes no outer local (`...Arm:`, a plain `BranchArm`",
        "// frame) and at the method root (`...Root`). A nested inline construct",
        "// writes `m`, a local of that arm. Asserted in",
        "// bt4460plain_arm_nested_grid_test.bt.",
        "",
        f"{superclass} subclass: {cls}",
    ]
    if slot:
        out.append(f"  {slot}")
    for selector, position, placement, writer in cell_cases(kind):
        uses_c, uses_z, _ = SELECTORS[selector]
        name = f"{selector}_{position}_{placement}_{writer}"
        body, _ = cell(selector, position, placement, writer)
        root_header, _ = selector_name(name, uses_c, uses_z, arm=False)
        if root_header == name:
            root_header = f"{name}Root"
        arm_header, _ = selector_name(name, uses_c, uses_z, arm=True)
        out.append("")
        out.append(f"  {prefix}{root_header} =>")
        out.extend(indent(body, 4))
        out.append("")
        out.append(f"  {prefix}{arm_header} =>")
        out.append("    c")
        out.append("      ifTrue: [")
        out.extend(indent(body, 8))
        out.append("      ]")
        out.append("      ifFalse: [0]")
    return "\n".join(out) + "\n"


def test_file():
    out = [
        HEADER,
        "// BT-4460 (ADR 0131): a nested inline construct that writes a local of a",
        "// plain conditional arm (the arm of a conditional that writes no outer",
        "// local) answers what the language says, and what the same body answers at",
        "// the method root, for every nested selector x statement position x write",
        "// placement x writer (an assignment, or BT-4367's loop inside the nested",
        "// construct) x class kind (actor, value, `Object` class-side).",
        "// Generated by scripts/gen-bt4460-plain-arm-nested-grid.py; do not edit by",
        "// hand. Fixtures: fixtures/bt4460plain_arm_nested_{actor,value,object}.bt.",
        "",
        "TestCase subclass: Bt4460PlainArmNestedGridTest",
    ]
    for kind, (cls, _, _, _, construct) in KINDS.items():
        for selector, position, placement, writer in cell_cases(kind):
            uses_c, uses_z, _ = SELECTORS[selector]
            name = f"{selector}_{position}_{placement}_{writer}"
            _, answer = cell(selector, position, placement, writer)
            _, root_call = selector_name(name, uses_c, uses_z, arm=False)
            if root_call == name:
                root_call = f"{name}Root"
            _, arm_call = selector_name(name, uses_c, uses_z, arm=True)
            out.append("")
            out.append(f"  test_{kind}_{name} =>")
            out.append(f"    o := {construct.format(cls=cls)}")
            out.append(f"    self assert: (o {arm_call}) equals: {render(answer)}")
            out.append(f"    self assert: (o {root_call}) equals: {render(answer)}")
    return "\n".join(out) + "\n"


def main():
    written = []
    for kind in KINDS:
        path = FIXTURES / f"bt4460plain_arm_nested_{kind}.bt"
        path.write_text(fixture(kind))
        written.append(path)
    TEST.write_text(test_file())
    written.append(TEST)
    beamtalk = ROOT / "target/debug/beamtalk"
    if beamtalk.exists():
        subprocess.run([str(beamtalk), "fmt", *map(str, written)], check=True)
    else:
        print("note: target/debug/beamtalk not built; run `just fmt-beamtalk`")
    for path in written:
        print(path.relative_to(ROOT))


if __name__ == "__main__":
    main()
