// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

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

/**
 * Extract display info from a `state: varName = default  // comment` declaration.
 *
 * Returns `{ defaultValue, comment }` where either may be undefined if not present.
 * Returns undefined if the declaration is not found.
 */
export function extractStateVarInfo(
  text: string,
  name: string
): { defaultValue?: string; comment?: string } | undefined {
  const esc = name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  // Match `state: name = <rest>` — capture everything after `=`
  const re = new RegExp(`^state:\\s+${esc}\\s*=\\s*(.+)$`);
  for (const line of text.split("\n")) {
    const trimmed = line.trimStart();
    if (trimmed.startsWith("//")) continue;
    const m = re.exec(trimmed);
    if (!m) continue;
    const rest = m[1]; // e.g. `#()  // List of parameter name strings`
    const slashIdx = rest.indexOf(" //");
    if (slashIdx >= 0) {
      return {
        defaultValue: rest.slice(0, slashIdx).trim() || undefined,
        comment: rest.slice(slashIdx + 3).trim() || undefined,
      };
    }
    return { defaultValue: rest.trim() || undefined };
  }
  return undefined;
}

/**
 * Find a `state: varName = ...` declaration in Beamtalk source text.
 *
 * Returns the character offset of `varName`, or -1 if not found.
 */
export function findStateVarDeclaration(text: string, name: string): number {
  const esc = name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const re = new RegExp(`^(state:\\s+)${esc}\\s*=`);
  const lines = text.split("\n");
  let offset = 0;
  for (const line of lines) {
    const trimmed = line.trimStart();
    if (!trimmed.startsWith("//")) {
      const m = re.exec(trimmed);
      if (m) {
        const indent = line.length - trimmed.length;
        return offset + indent + m[1].length;
      }
    }
    offset += line.length + 1;
  }
  return -1;
}

/**
 * Extract `///` doc comment lines immediately preceding a method declaration.
 *
 * Returns the comment text with `///` prefixes stripped, or undefined if no
 * doc comment is found. Correctly distinguishes class-side from instance-side,
 * so `run` (class) and `run` (instance) get the right comment.
 */
export function extractMethodDocComment(
  text: string,
  selector: string,
  side: "instance" | "class"
): string | undefined {
  const headRe = methodHeadPattern(selector);
  const lines = text.split("\n");

  for (let i = 0; i < lines.length; i++) {
    const trimmed = lines[i].trimStart();
    if (trimmed.startsWith("//")) continue;

    let matched = false;
    if (side === "class") {
      if (trimmed.startsWith("class ")) {
        matched = headRe.test(trimmed.slice(6).trimStart());
      }
    } else {
      if (!trimmed.startsWith("class ")) {
        matched = headRe.test(trimmed);
      }
    }
    if (!matched) continue;

    // Collect /// lines immediately above, stopping at blank lines or non-/// lines
    const docLines: string[] = [];
    for (let j = i - 1; j >= 0; j--) {
      const t = lines[j].trimStart();
      if (t.startsWith("/// ")) {
        docLines.unshift(t.slice(4));
      } else if (t === "///") {
        docLines.unshift("");
      } else {
        break;
      }
    }
    return docLines.length > 0 ? docLines.join("\n") : undefined;
  }
  return undefined;
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
    const indent = line.length - trimmed.length;
    if (!trimmed.startsWith("//")) {
      if (side === "class") {
        // Strip the leading `class ` and match the head against the remainder.
        if (trimmed.startsWith("class ")) {
          const afterClass = trimmed.slice(6).trimStart();
          const m = headRe.exec(afterClass);
          if (m) {
            // Offset: line start + indent + "class " + extra whitespace
            const classPrefix = 6 + (trimmed.length - 6 - afterClass.length);
            return offset + indent + classPrefix;
          }
        }
      } else {
        if (!trimmed.startsWith("class ")) {
          const m = headRe.exec(trimmed);
          if (m) return offset + indent;
        }
      }
    }
    offset += line.length + 1;
  }
  return -1;
}
