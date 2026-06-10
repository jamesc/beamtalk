// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// TweaksPanel — the cockpit's appearance panel (BT-2487, epic BT-2482 Phase 1).
//
// Ported from the spike's Tweaks panel (`spikes/cockpit-ux-spike/app.jsx` tweak
// effects + `tweaks-panel.jsx` control set). The whole IDE is themed by CSS
// variables (`assets/css/app.css`, laid down by BT-2484), so this is a *pure
// client-side* hook: it never round-trips to the server. It flips `data-theme`
// / `data-density` attributes and `--ui-font` / `--code-font` / `--accent` /
// syntax `--t-*` variables on `<html>` (`document.documentElement`), and
// persists the chosen values to `localStorage` so they survive a reload.
//
// Controls (which CSS knob each drives), matching the spike:
//   theme    → data-theme="paper|squeak|dusk"   (whole palette swap)
//   accent   → --accent + --accent-2 (a darker shade)  (paper/squeak only;
//              dusk keeps its built-in accent)
//   syntax   → the --t-* token palette: warm (theme default), mono, vivid
//   density  → data-density="cozy|compact"      (--row-h / --pad / --gap)
//   uiFont   → --ui-font   (the shell typeface)
//   codeFont → --code-font (the editor / mono typeface)
//
// Expected DOM (rendered by the LiveView, see WorkspaceLive.render/1):
//
//   <div id="tweaks-panel" phx-hook="TweaksPanel" data-tweaks-defaults='{…}'>
//     …controls with data-tweak="<key>" each…
//   </div>
//
// Each control carries `data-tweak="<key>"`. Segmented radios are a group of
// <button data-tweak-value="<value>">; dropdowns are a <select>. The hook reads
// the active value on change, applies it, and stores it. On mount it restores
// the persisted values (falling back to the server-provided defaults) and paints
// the controls so the panel reflects the live appearance.

const STORE_KEY = "bt_cockpit_tweaks"

// The bounded value sets we manage; uiFont / codeFont are a curated dropdown so
// they have no enum here (any non-empty string is accepted).
const THEMES = ["paper", "squeak", "dusk"]
const DENSITIES = ["cozy", "compact"]
const SYNTAXES = ["warm", "mono", "vivid"]

// Syntax-palette overrides, keyed by mode. `warm` is the theme's own default (no
// overrides — we clear any previously-set --t-* vars). `mono` desaturates to ink
// + accent; `vivid` is a saturated fixed palette. Mirrors app.jsx.
const SYNTAX_KEYS = [
  "--t-comment", "--t-string", "--t-symbol", "--t-number", "--t-keyword",
  "--t-reserved", "--t-global", "--t-var", "--t-send", "--t-return",
]
const SYNTAX_PALETTES = {
  warm: null,
  mono: {
    "--t-comment": "var(--faint)", "--t-string": "var(--ink)", "--t-symbol": "var(--ink)",
    "--t-number": "var(--ink)", "--t-keyword": "var(--accent)", "--t-reserved": "var(--accent)",
    "--t-global": "var(--ink)", "--t-var": "var(--ink)", "--t-send": "var(--accent)",
    "--t-return": "var(--accent)",
  },
  vivid: {
    "--t-string": "#3a8f3a", "--t-symbol": "#0f9b8a", "--t-number": "#d9700f",
    "--t-keyword": "#1f6fb0", "--t-reserved": "#d12f59", "--t-global": "#9a6314",
    "--t-send": "#d9700f", "--t-return": "#d12f59",
  },
}

// Darken/lighten a #rgb / #rrggbb hex by a signed percentage (negative = darker).
// Mirrors the spike's `shade()` — used to derive --accent-2 from the picked
// accent so hover/border tones track the chosen colour.
function shade(hex, pct) {
  const h = String(hex).replace("#", "")
  const full = h.length === 3 ? h.split("").map((c) => c + c).join("") : h
  const n = parseInt(full, 16)
  if (Number.isNaN(n)) return hex
  let r = (n >> 16) & 255, g = (n >> 8) & 255, b = n & 255
  const f = pct / 100
  const adj = (c) => Math.max(0, Math.min(255, Math.round(c + (f < 0 ? c * f : (255 - c) * f))))
  r = adj(r); g = adj(g); b = adj(b)
  return "#" + ((1 << 24) + (r << 16) + (g << 8) + b).toString(16).slice(1)
}

export const TweaksPanel = {
  mounted() {
    this.values = this.loadValues()
    this.onChange = (ev) => this.handleChange(ev)
    this.el.addEventListener("change", this.onChange)
    this.el.addEventListener("click", this.onChange)
    this.apply()
    this.paintControls()
  },

  destroyed() {
    if (!this.onChange) return
    this.el.removeEventListener("change", this.onChange)
    this.el.removeEventListener("click", this.onChange)
  },

  // Defaults come from the server markup (`data-tweaks-defaults`), so a single
  // source declares them. Persisted localStorage values override the defaults on
  // a per-key basis (a key added to defaults later still applies until the user
  // changes it). Malformed JSON / unavailable storage falls back gracefully.
  loadValues() {
    let defaults = {}
    try {
      defaults = JSON.parse(this.el.dataset.tweaksDefaults || "{}") || {}
    } catch (_err) {
      defaults = {}
    }
    let stored = {}
    try {
      stored = JSON.parse(window.localStorage.getItem(STORE_KEY) || "{}") || {}
    } catch (_err) {
      stored = {}
    }
    return { ...defaults, ...stored }
  },

  persist() {
    try {
      window.localStorage.setItem(STORE_KEY, JSON.stringify(this.values))
    } catch (_err) {
      // Storage unavailable (private mode / disabled): the appearance still
      // applies for this page, it just won't survive a reload.
    }
  },

  // A change on a segmented button (click) or dropdown (change). The originating
  // control names the tweak via `data-tweak`; a segmented button additionally
  // carries the chosen `data-tweak-value`, a <select> reports it via `value`.
  handleChange(ev) {
    const control = ev.target.closest("[data-tweak]")
    if (!control || !this.el.contains(control)) return
    const key = control.dataset.tweak
    if (!key) return

    let value
    if (control.dataset.tweakValue !== undefined) {
      value = control.dataset.tweakValue
    } else if (typeof control.value === "string") {
      value = control.value
    } else {
      return
    }
    if (this.values[key] === value) return

    this.values = { ...this.values, [key]: value }
    this.persist()
    this.apply()
    this.paintControls()
  },

  // Push every managed value onto <html>. Bounded values fall back to a sane
  // default so a stale/garbage localStorage entry can't leave the IDE unthemed.
  apply() {
    const r = document.documentElement
    const v = this.values

    const theme = THEMES.includes(v.theme) ? v.theme : "paper"
    const density = DENSITIES.includes(v.density) ? v.density : "cozy"
    r.setAttribute("data-theme", theme)
    r.setAttribute("data-density", density)

    if (v.uiFont) r.style.setProperty("--ui-font", `"${v.uiFont}", system-ui, sans-serif`)
    if (v.codeFont) r.style.setProperty("--code-font", `"${v.codeFont}", ui-monospace, monospace`)

    // Accent is a per-theme override on paper/squeak; dusk uses its own built-in
    // accent (clear the override so the theme's value shows through).
    if ((theme === "paper" || theme === "squeak") && v.accent) {
      r.style.setProperty("--accent", v.accent)
      r.style.setProperty("--accent-2", shade(v.accent, -22))
    } else {
      r.style.removeProperty("--accent")
      r.style.removeProperty("--accent-2")
    }

    // Syntax palette: clear any previous --t-* overrides, then set the mode's.
    SYNTAX_KEYS.forEach((k) => r.style.removeProperty(k))
    const syntax = SYNTAXES.includes(v.syntax) ? v.syntax : "warm"
    const palette = SYNTAX_PALETTES[syntax]
    if (palette) {
      Object.entries(palette).forEach(([k, val]) => r.style.setProperty(k, val))
    }
  },

  // Reflect the active values back onto the controls so the panel shows what is
  // applied (selected segment highlighted, dropdown on the right option).
  paintControls() {
    this.el.querySelectorAll("[data-tweak]").forEach((control) => {
      const key = control.dataset.tweak
      const active = this.values[key]
      if (control.dataset.tweakValue !== undefined) {
        const on = control.dataset.tweakValue === active
        control.setAttribute("aria-checked", on ? "true" : "false")
        control.dataset.on = on ? "1" : "0"
      } else if (control.tagName === "SELECT" && active != null) {
        control.value = active
      }
    })
  },
}
