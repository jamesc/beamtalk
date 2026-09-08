// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

/**
 * A single type "atom": a name (`Integer`), a generic instantiation with one
 * level of nested parens (`List(Foo)`, `Dictionary(String, List(E))`), a
 * metaclass reference (`Self class`, `Workflow class`), or a singleton
 * symbol type (`#all`, a subtype of `Symbol` — BT-2627).
 */
const TYPE_ATOM = `#?\\w+(?:\\((?:[^()]|\\([^()]*\\))*\\))?(?:\\s+class)?`;

/** A full type expression: one or more atoms joined by `|` (a union, e.g. `Integer | Nil`). */
const TYPE_EXPR = `${TYPE_ATOM}(?:\\s*\\|\\s*${TYPE_ATOM})*`;

/**
 * Build a regex matching a Beamtalk method head (selector + optional params + `=>`).
 *
 * Unary selectors (`run`) match `run =>` and optionally `run -> Type =>`.
 * Binary selectors (`+`) always take exactly one parameter (`+ other =>`),
 * optionally typed (`+ other :: Number =>`) — unlike unary, this parameter is
 * mandatory in real syntax, not optional in the pattern.
 * Keyword selectors (`lookup:depth:`) match `lookup: <ident> depth: <ident> =>`
 * by inserting `\s+\w+` after each keyword part to account for parameter names.
 * Typed params (`lookup: name :: String`) and typed returns (`-> Integer`) are
 * also handled, including generics (`name :: List(Foo)`), metaclass refs
 * (`-> Self class`), and unions (`-> CompiledMethod | Nil`) in either position.
 *
 * The returned regex captures the full head (including param names) in group 1.
 */
function methodHeadPattern(selector: string): RegExp {
  // A type annotation: `:: <type expr>`.
  const typeAnnotation = `::\\s*${TYPE_EXPR}`;
  // An optional return-type annotation: `-> <type expr>`.
  const returnType = `(?:\\s*->\\s*${TYPE_EXPR})?`;
  // A real keyword selector is one or more `identifier:` segments
  // (`at:put:`) — NOT merely a selector containing a colon character, since
  // a binary operator glyph can itself contain one (`=:=`, the identity
  // operator, is a single Binary selector, not a two-keyword message).
  const isKeywordSelector = /^([A-Za-z_]\w*:)+$/.test(selector);
  if (!isKeywordSelector) {
    const esc = selector.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    // A binary selector is built from operator characters (not a leading
    // letter/underscore) and always takes one parameter; a unary selector
    // never does.
    const isBinary = !/^[A-Za-z_]/.test(selector);
    const param = isBinary ? `\\s+\\w+(?:\\s*${typeAnnotation})?` : "";
    return new RegExp(`^(${esc}${param}${returnType})\\s*=>`);
  }
  const parts = selector.split(":").filter((p) => p.length > 0);
  // Each keyword part takes a parameter name with optional type: `name` or `name :: Type`
  const pat = parts
    .map((p) => `${p.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}:\\s+\\w+(?:\\s*${typeAnnotation})?`)
    .join("\\s+");
  return new RegExp(`^(${pat}${returnType})\\s*=>`);
}

/**
 * Extract display info from a `state: varName = default  // comment` declaration.
 * Also handles typed state vars: `state: varName :: Type = default  // comment`,
 * a defaultless typed declaration with no `=` at all (`state: varName :: Type`),
 * and the `field:` keyword — a synonym for `state:` in some stdlib classes.
 *
 * Returns `{ defaultValue, comment }` where either may be undefined if not present.
 * Returns undefined if the declaration is not found.
 */
export function extractStateVarInfo(
  text: string,
  name: string
): { defaultValue?: string; comment?: string } | undefined {
  const esc = name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  // Match `state: name` or `field: name`, then an optional `: Type`/`:: Type`,
  // then an optional `= <rest>` — a defaultless typed var (no `=` at all)
  // still matches, just with an undefined capture group.
  const re = new RegExp(`^(?:state|field):\\s+${esc}(?!\\w)(?:\\s*:\\s*[^=]+)?(?:\\s*=\\s*(.+))?$`);
  for (const line of text.split("\n")) {
    const trimmed = line.trimStart();
    if (trimmed.startsWith("//")) continue;
    const m = re.exec(trimmed);
    if (!m) continue;
    const rest = m[1]; // e.g. `#()  // List of parameter name strings`, undefined if no `=`
    if (rest === undefined) return {};
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
 * Also handles typed state vars: `state: varName :: Type = ...`, a
 * defaultless typed declaration with no `=` at all (`state: varName :: Type`),
 * and the `field:` keyword — a synonym for `state:` in some stdlib classes.
 *
 * Returns the character offset of `varName`, or -1 if not found.
 */
export function findStateVarDeclaration(text: string, name: string): number {
  const esc = name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  // Allow optional type annotation and default: `state: name :: Type = v`,
  // `state: name :: Type` (no default), or `state: name = v`.
  const re = new RegExp(`^((?:state|field):\\s+)${esc}(?!\\w)(?:\\s*:\\s*[^=]+)?(?:\\s*=)?`);
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
 * Find a `SuperClass subclass: ClassName` declaration in Beamtalk source
 * text, skipping any occurrence on a `//`/`///` comment line (e.g. a
 * doc-comment usage example that mentions the class).
 *
 * Returns the character offset of `ClassName`, or -1 if not found.
 */
export function findClassDeclaration(text: string, className: string): number {
  const escaped = className.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  // The lookahead also accepts `(` so a generic class declaration
  // (`subclass: Collection(E)`) matches, not just a bare `subclass: Foo`.
  const pattern = new RegExp(`\\bsubclass:\\s+(${escaped})(?=[\\s(]|$)`, "g");
  let match = pattern.exec(text);
  while (match !== null) {
    const lineStart = text.lastIndexOf("\n", match.index) + 1;
    const linePrefix = text.slice(lineStart, match.index).trimStart();
    if (!linePrefix.startsWith("//")) {
      return match.index + match[0].indexOf(className);
    }
    match = pattern.exec(text);
  }
  return -1;
}

/**
 * Find a `type AliasName = ...` declaration (ADR 0108 Phase 8, BT-2903) in
 * Beamtalk source text — optionally prefixed by the `internal` modifier
 * (`internal type AliasName = ...`) — skipping any occurrence on a
 * `//`/`///` comment line.
 *
 * Returns the character offset of `AliasName`, or -1 if not found.
 */
export function findTypeAliasDeclaration(text: string, aliasName: string): number {
  const escaped = aliasName.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const re = new RegExp(`^(?:internal\\s+)?type\\s+(${escaped})(?!\\w)`);
  const lines = text.split("\n");
  let offset = 0;
  for (const line of lines) {
    const trimmed = line.trimStart();
    if (!trimmed.startsWith("//")) {
      const m = re.exec(trimmed);
      if (m) {
        const indent = line.length - trimmed.length;
        return offset + indent + m[0].indexOf(m[1]);
      }
    }
    offset += line.length + 1;
  }
  return -1;
}

/**
 * Extract `///` doc comment lines immediately preceding a state variable
 * declaration. Mirrors `extractMethodDocComment` for methods — state vars
 * previously had no equivalent, so a `///` comment above a `state:` line
 * never surfaced in the sidebar hover tooltip.
 *
 * Returns the comment text with `///` prefixes stripped, or undefined if no
 * doc comment is found.
 */
export function extractStateVarDocComment(text: string, name: string): string | undefined {
  const esc = name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const declRe = new RegExp(`^(?:state|field):\\s+${esc}(?!\\w)`);
  const lines = text.split("\n");

  for (let i = 0; i < lines.length; i++) {
    const trimmed = lines[i].trimStart();
    if (trimmed.startsWith("//")) continue;
    if (!declRe.test(trimmed)) continue;

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

    const stripped = stripMethodModifiers(trimmed, side);
    if (!stripped || !headRe.test(stripped.rest)) continue;

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

/**
 * Resolves a 1-based declaration line (from `beamtalk_xref`'s compiled index,
 * BT-3439) to a character offset at the line's first non-whitespace column —
 * the same "start of the declaration head" position `findMethodDeclaration`/
 * `findStateVarDeclaration` locate via regex, but from a real backend line
 * number instead of a source-text guess.
 *
 * Returns -1 (callers should fall back to the regex-based finders, exactly
 * as they already do when those finders themselves return -1) when either:
 * - `oneBasedLine` falls outside the document, or
 * - the line no longer contains `expectedNeedle` as its own token (a
 *   word-boundary match, not a bare substring — see `hasWordBoundaryMatch`).
 *
 * The second check matters because the first alone can't catch every kind of
 * staleness: if the file was edited (lines inserted/deleted above the
 * declaration) since the class was last compiled/reloaded, `oneBasedLine`
 * can still be in range — just pointing at a different, unrelated line now.
 * `expectedNeedle` should be something only the real declaration line would
 * contain: `stateVar.name` for a field, or the first `:`-delimited part of
 * `method.selector` for a method (the full joined selector never appears
 * verbatim in source — see `findMethodDeclaration`'s doc — so checking for
 * it here would always miss and defeat the real-line path entirely).
 */
export function offsetForDeclarationLine(
  text: string,
  oneBasedLine: number,
  expectedNeedle: string
): number {
  const lines = text.split("\n");
  if (oneBasedLine < 1 || oneBasedLine > lines.length) return -1;
  const line = lines[oneBasedLine - 1];
  // A stale line can land on a `///` doc-comment example that happens to
  // mention the same selector (e.g. a usage snippet) — that still passes
  // the word-boundary check below, so reject comment lines outright first.
  if (line.trimStart().startsWith("//")) return -1;
  if (!hasWordBoundaryMatch(line, expectedNeedle)) return -1;
  let offset = 0;
  for (let i = 0; i < oneBasedLine - 1; i++) {
    offset += lines[i].length + 1;
  }
  const match = /\S/.exec(line);
  return offset + (match ? match.index : 0);
}

/**
 * True if `needle` occurs in `line` at a word boundary on both sides — not
 * embedded inside a longer identifier. A plain `line.includes(needle)` would
 * false-positive on e.g. `count` inside `discount`, or `at` (from `at:put:`)
 * inside `state`/`format`/`data` — exactly defeating the staleness check
 * `offsetForDeclarationLine` uses this for (BT-3439 review feedback).
 *
 * Hand-rolled rather than a `\bneedle\b` regex because `\b` is only
 * meaningful around word characters (`[A-Za-z0-9_]`) — a symbolic/binary
 * selector needle like `+` has no well-defined `\b` on either side, so a
 * regex-based check would behave inconsistently for it. Checking the
 * actual neighboring characters works uniformly for both cases.
 */
function hasWordBoundaryMatch(line: string, needle: string): boolean {
  if (needle === "") return false;
  const isWordChar = (ch: string | undefined): boolean => ch !== undefined && /\w/.test(ch);
  let from = 0;
  for (;;) {
    const idx = line.indexOf(needle, from);
    if (idx === -1) return false;
    if (!isWordChar(line[idx - 1]) && !isWordChar(line[idx + needle.length])) {
      return true;
    }
    from = idx + 1;
  }
}

/**
 * Strips a leading run of method modifiers (`class`, `sealed`, `internal` —
 * in any order/combination, mirroring the parser's modifier loop in
 * `parse_method_definition`) from a trimmed line, and reports whether the
 * requested `side` matches whether `class` was among them.
 *
 * Returns undefined if the line's modifiers don't match the requested side
 * (e.g. a `class`-flagged line when searching for an instance-side
 * declaration) — the caller should treat that as "this line doesn't match."
 *
 * Shared by `findMethodDeclaration` and `extractMethodDocComment` so a gap
 * here (like the original version only stripping a leading `class `,
 * missing `sealed`/`internal` entirely) can't drift between the two.
 */
function stripMethodModifiers(
  trimmed: string,
  side: "instance" | "class"
): { rest: string; prefixLength: number } | undefined {
  let rest = trimmed;
  let consumed = 0;
  let isClassSide = false;
  for (;;) {
    const m = /^(class|sealed|internal)\s+/.exec(rest);
    if (!m) break;
    // A modifier word immediately followed by `=>`/`-> Type =>` is actually
    // the whole selector, not a modifier — e.g. `class => @intrinsic
    // "class"` is the real, commonly-used unary method named `class`.
    const afterWord = rest.slice(m[1].length).trimStart();
    if (/^(?:->|=>)/.test(afterWord)) break;
    if (m[1] === "class") isClassSide = true;
    consumed += m[0].length;
    rest = rest.slice(m[0].length);
  }
  if (isClassSide !== (side === "class")) return undefined;
  return { rest, prefixLength: consumed };
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
      const stripped = stripMethodModifiers(trimmed, side);
      if (stripped) {
        const m = headRe.exec(stripped.rest);
        if (m) return offset + indent + stripped.prefixLength;
      }
    }
    offset += line.length + 1;
  }
  return -1;
}
