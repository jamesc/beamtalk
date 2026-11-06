#!/usr/bin/env python3
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

"""Generate the BT-4461 REPL-arm cached-binding grid (ADR 0131).

A REPL frame other than the root (a conditional arm, an `on:do:`/`ensure:`
block, an evaluated block) holds the bindings it writes in Core variables:
`m := 1` in an arm binds `m`. A construct nested in that frame that writes
`m` again is a REPL producer: it adopts the bindings map whole. The grid runs
every cell through the REPL protocol:

    outer frame  x  nested writer  x  statement position  x  write placement

- outer frame: the REPL frame the nested writer sits in (both arms of a
  conditional, `ifTrue:`, `ifFalse:`, `ifNil:`, `ifNotNil:`, `and:`, `or:`, the
  protected block, handler and cleanup of `on:do:`/`ensure:`, an evaluated
  block, an arm nested in an arm, and the bodies of `do:`, `to:do:`,
  `timesRepeat:` and `inject:into:`);
- nested writer: the construct that writes `m` (every conditional, `and:`/`or:`,
  `on:do:`/`ensure:` body, handler and cleanup, `match:`, an evaluated block, and
  the loops and folds `whileTrue:`, `to:do:`, `do:`, `inject:into:`,
  `timesRepeat:`);
- statement position of the writer in the frame: a non-last statement, an
  assignment's value, the frame's last expression;
- write placement: the frame writes `m` before the writer (`pre`, so the frame
  holds `m` in a variable) or not (`none`, `ifTrue:ifFalse:` frame only).

Each cell resets `m`, runs the input and reads `m` back as the next input, so
the binding must persist. The expected answers come from the small model
below, so a cell fails when the REPL's answer differs from what the language
says. Re-run after changing the grid:

    python3 scripts/gen-bt4461-repl-arm-cached-binding-grid.py

Output (checked in):
    tests/repl-protocol/cases/bt4461_repl_arm_cached_binding_grid.btscript
"""

import pathlib

ROOT = pathlib.Path(__file__).resolve().parent.parent
OUT = ROOT / "tests/repl-protocol/cases/bt4461_repl_arm_cached_binding_grid.btscript"

WILD = "_"


def plus(k, value="m"):
    """A writer adding `k` to `m`; its value is `m`'s new value (`value="m"`)
    or a fixed answer."""

    def model(m):
        new = m + k
        return new, (new if value == "m" else value)

    return model


# name -> (source, model: m -> (m after, construct value or WILD))
WRITERS = {
    "ifTrue_ifFalse": ("c ifTrue: [m := m + 10] ifFalse: [nil]", plus(10)),
    "ifTrue": ("c ifTrue: [m := m + 10]", plus(10)),
    "ifFalse": ("c not ifFalse: [m := m + 10]", plus(10)),
    "ifNil": ("nil ifNil: [m := m + 10]", plus(10)),
    "ifNotNil": ("3 ifNotNil: [:w | m := m + w]", plus(3)),
    "and": ("c and: [m := m + 10. true]", plus(10, "true")),
    "or": ("c not or: [m := m + 10. true]", plus(10, "true")),
    "onDo": ("[m := m + 10] on: Error do: [:e | 0]", plus(10)),
    "onDoHandler": ('[Error signal: "boom"] on: Error do: [:e | m := m + 10]', plus(10)),
    "ensure": ("[m := m + 10] ensure: [nil]", plus(10)),
    "ensureCleanup": ("[nil] ensure: [m := m + 10]", plus(10, "nil")),
    "match": ("1 match: [1 -> (m := m + 10); _ -> nil]", plus(10)),
    "value": ("[m := m + 10] value", plus(10)),
    "whileTrue": ("[m < 20] whileTrue: [m := m + 10]", lambda m: (m + 10 * max(0, -(-(20 - m) // 10)), WILD)),
    "toDo": ("1 to: 3 do: [:i | m := m + i]", plus(6, WILD)),
    "do": ("#(1, 2, 3) do: [:i | m := m + i]", plus(6, WILD)),
    "inject": ("#(1, 2, 3) inject: 0 into: [:a :i | m := m + i. a + i]", plus(6, "6")),
    "timesRepeat": ("3 timesRepeat: [m := m + 10]", plus(30, WILD)),
}

# name -> (template with {b} for the frame body, answer: frame value -> input value)
OUTERS = {
    "ifTrueArm": ("c ifTrue: [{b}] ifFalse: [0]", lambda v: v),
    "ifFalseArm": ("c not ifTrue: [0] ifFalse: [{b}]", lambda v: v),
    "ifTrue": ("c ifTrue: [{b}]", lambda v: v),
    "ifFalse": ("c not ifFalse: [{b}]", lambda v: v),
    "ifNil": ("nil ifNil: [{b}]", lambda v: v),
    "ifNotNil": ("3 ifNotNil: [:v | {b}]", lambda v: v),
    "and": ("c and: [{b}. true]", lambda v: "true"),
    "or": ("c not or: [{b}. true]", lambda v: "true"),
    "onDo": ("[{b}] on: Error do: [:e | 0]", lambda v: v),
    "onDoHandler": ('[Error signal: "boom"] on: Error do: [:e | {b}]', lambda v: v),
    "ensure": ("[{b}] ensure: [nil]", lambda v: v),
    "ensureCleanup": ("[nil] ensure: [{b}]", lambda v: "nil"),
    "value": ("[{b}] value", lambda v: v),
    "nestedArm": ("c ifTrue: [c ifTrue: [{b}] ifFalse: [0]] ifFalse: [0]", lambda v: v),
    # Loop and fold bodies: each iteration writes `m := 1` again, so `m`
    # after the loop is the writer's `m` after one iteration.
    "doBody": ("#(1, 2) do: [:i | {b}]", lambda v: WILD),
    "toDoBody": ("1 to: 2 do: [:i | {b}]", lambda v: WILD),
    "timesRepeatBody": ("2 timesRepeat: [{b}]", lambda v: WILD),
    "injectBody": ("#(1, 2) inject: 0 into: [:a :i | {b}. a + i]", lambda v: "3"),
}

POSITIONS = ("nonlast", "assigned", "last")

LOOP_BODIES = {"doBody", "toDoBody", "timesRepeatBody", "injectBody"}


def rejected(outer, writer, position):
    """The cells the REPL rejects at compile time, by rules older than this
    grid: in a block of a top-level expression (a loop or fold body), a
    construct writing a binding as an assignment's value, and a `match:` arm
    writing one (`match:` cannot return the write there)."""
    if outer not in LOOP_BODIES:
        return False
    return position == "assigned" or writer == "match"


# A block parameter the frame reassigns is bound to a variable but is never a
# key of the bindings map, so a nested writer leaves it to the frame's own
# lowering (the adversarial review of BT-4461: hiding or re-reading one raised
# `undefined_variable` or `badkey`). name -> (input, answer)
PARAMETER_WRITES = {
    # These are no-regression cells: the loop never runs, or the arm that
    # writes the parameter is not taken. A parameter write that does run is
    # lost (or its loop never ends) in the REPL before and after BT-4461:
    # BT-4499.
    "foldParamLoop": ("#(5, 6) inject: 0 into: [:a :i | [i < 3] whileTrue: [i := i + 1]. a + i]", "11"),
    "foldParamConditional": (
        "#(1, 2) inject: 0 into: [:a :i | (i > 5) ifTrue: [i := 0] ifFalse: [nil]. a + i]",
        "3",
    ),
    "foldAccumulatorLoop": ("#(1, 2) inject: 0 into: [:acc :x | 1 to: 0 do: [:k | acc := acc + k]. acc + x]", "3"),
    "ifNotNilParamLoop": ("7 ifNotNil: [:v | [v < 5] whileTrue: [v := v + 1]. v]", "7"),
}


def body_and_value(writer, position, pre):
    """The frame body for `writer` at `position`, and the frame's value and
    `m` after it, by the model."""
    source, model = WRITERS[writer]
    m = 1 if pre else 0
    m_after, value = model(m)
    prefix = "m := 1. " if pre else ""
    if position == "nonlast":
        return f"{prefix}{source}. m", m_after, m_after
    if position == "assigned":
        return f"{prefix}q := ({source}). m", m_after, m_after
    return f"{prefix}{source}", value, m_after


def render(value):
    return str(value)


def cells():
    for outer in OUTERS:
        for writer in WRITERS:
            for position in POSITIONS:
                placements = (True, False) if outer == "ifTrueArm" else (True,)
                for pre in placements:
                    if not rejected(outer, writer, position):
                        yield outer, writer, position, pre


def main():
    out = [
        "// Copyright 2026 James Casey",
        "// SPDX-License-Identifier: Apache-2.0",
        "",
        "// GENERATED by scripts/gen-bt4461-repl-arm-cached-binding-grid.py — do not edit.",
        "//",
        "// BT-4461 (ADR 0131): a construct nested in a REPL frame (an arm, a protected",
        "// block, a handler or cleanup, an evaluated block, a loop or fold body) that",
        "// writes the binding `m` the frame already wrote. The frame holds `m` in a",
        "// variable; the",
        "// construct must read `m` out of the map it threads and the frame must read",
        "// it back out of the adopted map. Each cell resets `m`, runs the input and",
        "// reads `m` as the next input, so the binding persists.",
        "",
        "c := true",
        "// => true",
        "",
    ]
    count = 0
    for outer, writer, position, pre in cells():
        template, answer = OUTERS[outer]
        body, frame_value, m_after = body_and_value(writer, position, pre)
        value = frame_value if frame_value == WILD else answer(frame_value)
        placement = "pre" if pre else "none"
        out += [
            f"// {outer} / {writer} / {position} / {placement}",
            "m := 0",
            "// => 0",
            "",
            f"r := {template.format(b=body)}",
            f"// => {render(value)}",
            "",
            "m",
            f"// => {m_after}",
            "",
        ]
        count += 1
    for name, (source, answer) in PARAMETER_WRITES.items():
        out += [
            f"// parameter / {name} / - / none",
            f"r := {source}",
            f"// => {answer}",
            "",
        ]
        count += 1
    OUT.write_text("\n".join(out))
    print(f"wrote {count} cells to {OUT.relative_to(ROOT)}")


if __name__ == "__main__":
    main()
