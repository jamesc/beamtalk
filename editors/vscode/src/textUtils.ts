// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

/**
 * Find a method declaration in Beamtalk source text.
 *
 * - Class-side: matches `class selector =>` (trimmed line prefix)
 * - Instance-side: matches `selector =>` at start of trimmed line (excludes `class selector` lines)
 * - Skips comment lines (trimmed starting with `//`)
 *
 * Returns the character offset of the selector within the text, or -1 if not found.
 */
/**
 * Build the regex for matching an instance-side method head.
 *
 * Unary selectors (`run`) match `run =>`.
 * Keyword selectors (`lookup:depth:`) match `lookup: <param> depth: <param> =>`
 * by inserting `\s+\w+` after each colon keyword part.
 *
 * Returns [regex, groupIndex] where groupIndex (1-based) captures the selector text.
 */
function instanceHeadPattern(selector: string): RegExp {
  if (!selector.includes(":")) {
    const esc = selector.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    return new RegExp(`^(${esc})\\s*=>`);
  }
  // keyword selector: split on ':', filter empty trailing part
  const parts = selector.split(":").filter((p) => p.length > 0);
  const pat = parts
    .map((p) => `${p.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}:\\s+\\w+`)
    .join("\\s+");
  return new RegExp(`^(${pat})\\s*=>`);
}

export function findMethodDeclaration(
  text: string,
  selector: string,
  side: "instance" | "class"
): number {
  const lines = text.split("\n");
  let offset = 0;

  // For class-side, the selector itself never contains parameter slots.
  const classEsc = selector.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const classRe = new RegExp(`^class\\s+(${classEsc})(?=\\s*=>|\\s|$)`);
  const instanceRe = instanceHeadPattern(selector);

  for (const line of lines) {
    const trimmed = line.trimStart();
    if (!trimmed.startsWith("//")) {
      if (side === "class") {
        const m = classRe.exec(trimmed);
        if (m) return offset + line.indexOf(m[1]);
      } else {
        if (!trimmed.startsWith("class ")) {
          const m = instanceRe.exec(trimmed);
          if (m) {
            // For keyword selectors m[1] is e.g. "lookup: name"; we want the
            // offset of just the first keyword, which is the start of m[1].
            return offset + line.indexOf(m[1]);
          }
        }
      }
    }
    offset += line.length + 1;
  }
  return -1;
}
