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
 * Build a regex matching a Beamtalk method head (selector + optional params + `=>`).
 *
 * Unary selectors (`run`) match `run =>`.
 * Keyword selectors (`lookup:depth:`) match `lookup: <ident> depth: <ident> =>`
 * by inserting `\s+\w+` after each keyword part to account for parameter names.
 *
 * The returned regex captures the full head (including param names) in group 1.
 */
function methodHeadPattern(selector: string): RegExp {
  if (!selector.includes(":")) {
    const esc = selector.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    return new RegExp(`^(${esc})\\s*=>`);
  }
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
  const headRe = methodHeadPattern(selector);
  const lines = text.split("\n");
  let offset = 0;

  for (const line of lines) {
    const trimmed = line.trimStart();
    if (!trimmed.startsWith("//")) {
      if (side === "class") {
        // Strip the leading `class ` and match the head against the remainder.
        if (trimmed.startsWith("class ")) {
          const afterClass = trimmed.slice(6).trimStart();
          const m = headRe.exec(afterClass);
          if (m) return offset + line.indexOf(m[1]);
        }
      } else {
        if (!trimmed.startsWith("class ")) {
          const m = headRe.exec(trimmed);
          if (m) return offset + line.indexOf(m[1]);
        }
      }
    }
    offset += line.length + 1;
  }
  return -1;
}
