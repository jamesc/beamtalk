// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// Beamtalk syntax highlighter (Smalltalk-INSPIRED grammar), ported from
// `spikes/cockpit-ux-spike/highlight.js`. `highlightBeamtalk(src)` returns an
// HTML string with `span.tok-*` wrappers; the `.tok-*` colours are themed by
// CSS variables (`--t-*`) in `assets/css/app.css`, so the same markup re-skins
// across the paper/squeak/dusk themes.
//
// Beamtalk specifics vs classic Smalltalk:
//   - comments are // line and /* block */ (NOT double-quoted)
//   - strings are "double quoted" with {interpolation}
//   - single quotes are reserved for #'quoted symbols'
//   - method bodies use  selector => body
//   - field access via self.field   - assignment :=   - early return ^
//   - async send suffix !   - symbols #name   - maps and lists

const RESERVED = new Set([
  "self",
  "super",
  "nil",
  "true",
  "false",
  "thisContext",
])

function esc(s) {
  return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;")
}

function span(cls, text) {
  return '<span class="tok-' + cls + '">' + esc(text) + "</span>"
}

// Highlight a double-quoted string, lifting {interpolation} spans out.
function hlString(text) {
  let out = "",
    i = 0
  while (i < text.length) {
    if (text[i] === "{") {
      const end = text.indexOf("}", i)
      if (end > i) {
        out +=
          '<span class="tok-interp">' + esc(text.slice(i, end + 1)) + "</span>"
        i = end + 1
        continue
      }
    }
    out += esc(text[i])
    i += 1
  }
  return '<span class="tok-string">' + out + "</span>"
}

const RULES = [
  { cls: "comment", re: /^\/\/[^\n]*/ }, // // line comment
  { cls: "comment", re: /^\/\*[\s\S]*?\*\// }, // /* block comment */
  { cls: "_string", re: /^"(?:\\.|[^"\\])*"/ }, // "double quoted string"
  {
    cls: "symbol",
    re: /^#(?:'(?:''|[^'])*'|\{|\(|[A-Za-z_][\w]*[:]?|[-+*/~<>=&|@%,?!]+)/,
  }, // #sym #{ #(
  { cls: "field", re: /^\.[A-Za-z_]\w*/ }, // .field (after self/obj)
  { cls: "number", re: /^\d+r[0-9A-Za-z]+/ }, // radix
  { cls: "number", re: /^\d+(?:\.\d+)?(?:e-?\d+)?/ }, // int / float
  { cls: "arrow", re: /^=>/ }, // method body arrow
  { cls: "match", re: /^->/ }, // pattern / association arrow
  { cls: "assign", re: /^:=/ }, // assignment
  { cls: "return", re: /^\^/ }, // early return
  { cls: "keyword", re: /^[A-Za-z_]\w*:/ }, // keyword message part  foo:
  { cls: "ident", re: /^[A-Za-z_]\w*/ }, // identifier (resolved below)
  { cls: "send", re: /^!/ }, // async send suffix
  { cls: "cascade", re: /^;/ }, // cascade
  { cls: "binary", re: /^(?:=:=|=\/=|\/=|==|\*\*|\+\+|<=|>=|[-+*/<>=~&|@%?])+/ },
  { cls: "punct", re: /^[\[\]{}().|]/ },
  { cls: "ws", re: /^\s+/ },
  { cls: "other", re: /^./ },
]

export function highlightBeamtalk(src) {
  if (src == null) return ""
  let i = 0,
    out = ""
  const n = src.length
  while (i < n) {
    const rest = src.slice(i)
    let matched = false
    for (const rule of RULES) {
      const mm = rest.match(rule.re)
      if (!mm) continue
      const text = mm[0]
      matched = true
      if (rule.cls === "ws" || rule.cls === "other") out += esc(text)
      else if (rule.cls === "_string") out += hlString(text)
      else if (rule.cls === "field")
        out += '<span class="tok-punct">.</span>' + span("field", text.slice(1))
      else if (rule.cls === "ident") {
        if (RESERVED.has(text)) out += span("reserved", text)
        else if (/^[A-Z]/.test(text)) out += span("global", text)
        else out += span("var", text)
      } else out += span(rule.cls, text)
      i += text.length
      break
    }
    if (!matched) {
      out += esc(rest[0])
      i += 1
    }
  }
  return out
}
