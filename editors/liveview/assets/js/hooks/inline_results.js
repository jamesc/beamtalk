// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// inline_results.js — collapsible inline eval results for the Workspace editor
// (BT-2542). The classic Smalltalk Workspace "Print it inserts the result"
// idiom: evaluating with Print it drops a `→ <result>` annotation into the
// editor right after the evaluated expression.
//
// Results are rendered as CodeMirror BLOCK WIDGETS anchored to a doc position,
// NOT as document text. That is deliberate: if the `→ result` lines were real
// text, "evaluate buffer" (the default when nothing is selected) would try to
// re-evaluate them as Beamtalk source. As widgets they are visually inline in
// the buffer yet invisible to the eval path (which reads the hidden textarea the
// CmEditor hook mirrors from the doc text).
//
// A long result (more than COLLAPSE_LINES lines) renders collapsed by default to
// a one-line summary (`→ Session help · 120 lines ▸`); clicking the widget (or
// its toggle) expands it inline. Each widget carries a stable `id` so toggling
// survives the decoration rebuild that every transaction triggers.

import { StateField, StateEffect } from "@codemirror/state"
import { Decoration, EditorView, WidgetType } from "@codemirror/view"

// Past this many lines an inline result collapses by default.
const COLLAPSE_LINES = 4
// One-line summary truncation width.
const SUMMARY_WIDTH = 60

// Monotonic id so a result keeps its identity (and collapsed state) across the
// decoration rebuilds that follow every transaction.
let nextId = 1

// Effects the CmEditor hook (and the widgets) dispatch.
//   addInlineResult    — {pos, text}: anchor a new result at a line boundary.
//   toggleInlineResult — id: flip one result between collapsed and expanded.
export const addInlineResult = StateEffect.define()
export const toggleInlineResult = StateEffect.define()

function truncate(line) {
  return line.length > SUMMARY_WIDTH ? line.slice(0, SUMMARY_WIDTH - 1) + "…" : line
}

class ResultWidget extends WidgetType {
  constructor(id, text, collapsed) {
    super()
    this.id = id
    this.text = text
    this.collapsed = collapsed
  }

  eq(other) {
    return other.id === this.id && other.text === this.text && other.collapsed === this.collapsed
  }

  get lineCount() {
    return this.text.split("\n").length
  }

  toDOM(view) {
    const wrap = document.createElement("div")
    wrap.className = "cm-inline-result"
    const collapsible = this.lineCount > COLLAPSE_LINES

    if (collapsible && this.collapsed) {
      wrap.classList.add("is-collapsed")
      const first = this.text.split("\n", 1)[0]
      const summary = document.createElement("span")
      summary.className = "cm-inline-result-summary"
      summary.textContent = `→ ${truncate(first)} · ${this.lineCount} lines`
      wrap.appendChild(summary)
      wrap.appendChild(this.toggleButton(view, "▸"))
      // The whole collapsed strip is a click target — easier than hunting the ▸.
      wrap.addEventListener("mousedown", (e) => {
        e.preventDefault()
        view.dispatch({ effects: toggleInlineResult.of(this.id) })
      })
    } else {
      const body = document.createElement("span")
      body.className = "cm-inline-result-body"
      body.textContent = `→ ${this.text}`
      wrap.appendChild(body)
      if (collapsible) wrap.appendChild(this.toggleButton(view, "▾"))
    }
    return wrap
  }

  toggleButton(view, glyph) {
    const btn = document.createElement("button")
    btn.type = "button"
    btn.className = "cm-inline-result-toggle"
    btn.textContent = glyph
    btn.setAttribute("aria-label", this.collapsed ? "Expand result" : "Collapse result")
    btn.addEventListener("mousedown", (e) => {
      e.preventDefault()
      e.stopPropagation()
      view.dispatch({ effects: toggleInlineResult.of(this.id) })
    })
    return btn
  }

  // Let the widget handle its own clicks (toggling) rather than the editor
  // treating them as cursor placement.
  ignoreEvent() {
    return false
  }
}

function buildDecorations(items) {
  // Block widgets must be added in document order; items are appended in eval
  // order, which is not necessarily position order once the buffer is edited.
  const ordered = [...items].sort((a, b) => a.pos - b.pos)
  return Decoration.set(
    ordered.map((it) =>
      Decoration.widget({
        widget: new ResultWidget(it.id, it.text, it.collapsed),
        block: true,
        side: 1,
      }).range(it.pos),
    ),
    true,
  )
}

// Tracks the live set of inline results and provides their decorations. State is
// kept as a plain `items` list so positions can be mapped through edits and the
// collapsed flag toggled; decorations are rebuilt from it each transaction.
export const inlineResultsField = StateField.define({
  create() {
    return { items: [], deco: Decoration.none }
  },

  update(value, tr) {
    let items = value.items
    let changed = false

    if (tr.docChanged) {
      // Keep each result anchored as the user edits around it (assoc 1: stay put
      // when text is inserted exactly at the anchor).
      items = items.map((it) => ({ ...it, pos: tr.changes.mapPos(it.pos, 1) }))
      changed = true
    }

    for (const effect of tr.effects) {
      if (effect.is(addInlineResult)) {
        items = [
          ...items,
          { id: nextId++, pos: effect.value.pos, text: effect.value.text, collapsed: true },
        ]
        changed = true
      } else if (effect.is(toggleInlineResult)) {
        items = items.map((it) =>
          it.id === effect.value ? { ...it, collapsed: !it.collapsed } : it,
        )
        changed = true
      }
    }

    if (!changed) return value
    return { items, deco: buildDecorations(items) }
  },

  provide: (field) => EditorView.decorations.from(field, (value) => value.deco),
})
