# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.ShellComponents do
  @moduledoc """
  Cockpit shell function components extracted from `BtAttachWeb.WorkspaceLive`
  (BT-5092): the Tweaks (appearance) panel and the eval activity indicator
  (BT-5189). Imported by `WorkspaceLive`, so the `<.tweaks_panel>` call site in
  its template is unchanged.
  """

  use BtAttachWeb, :html

  # ── Tweaks panel (BT-2487) ──────────────────────────────────────────────────

  # The default appearance the cockpit ships with, mirroring the spike's
  # `useTweaks` initial state (spikes/cockpit-ux-spike/app.jsx). The `TweaksPanel`
  # JS hook reads these from `data-tweaks-defaults`, then a per-key localStorage
  # override (the user's last choice) wins — so this is just the first-run skin.
  @tweak_defaults %{
    theme: "paper",
    accent: "#b9711b",
    syntax: "warm",
    density: "cozy",
    uiFont: "Hanken Grotesk",
    codeFont: "Cascadia Code"
  }

  # The curated accent swatches (paper/squeak only — dusk keeps its built-in
  # accent), UI-font and code-font options — exactly the sets the spike offers.
  @tweak_accents ~w(#b9711b #a8324e #2c6e8e #5d7a2e #7a4ea8)
  @tweak_ui_fonts ["Hanken Grotesk", "Inter Tight", "Public Sans", "Schibsted Grotesk"]
  @tweak_code_fonts ["Cascadia Code", "Monaspace", "JetBrains Mono"]

  # The appearance panel: a pure-client control surface. The `TweaksPanel` hook
  # owns all behaviour — each control declares the tweak it drives via
  # `data-tweak` / `data-tweak-value`, and the hook flips the matching `:root`
  # CSS variable + persists to localStorage. The server never sees a change; this
  # is presentation only, so it carries no socket state beyond the static
  # defaults the hook restores on first run.
  def tweaks_panel(assigns) do
    assigns =
      assigns
      |> assign(:defaults, @tweak_defaults)
      |> assign(:accents, @tweak_accents)
      |> assign(:ui_fonts, @tweak_ui_fonts)
      |> assign(:code_fonts, @tweak_code_fonts)

    ~H"""
    <div
      id="tweaks-panel"
      class="panel tweaks-panel"
      style="flex:none;"
      phx-hook="TweaksPanel"
      data-tweaks-defaults={Jason.encode!(@defaults)}
    >
      <div class="panel-head"><span class="panel-title">Tweaks</span></div>
      <div class="panel-body">
        <%!-- Theme → data-theme on <html> (whole palette swap) --%>
        <div class="twk-row">
          <span class="twk-cap">Theme</span>
          <div class="twk-seg" role="radiogroup" aria-label="Theme">
            <button
              :for={theme <- ~w(paper squeak dusk)}
              type="button"
              role="radio"
              data-tweak="theme"
              data-tweak-value={theme}
            >
              {theme}
            </button>
          </div>
        </div>

        <%!-- Accent → --accent / --accent-2 (paper/squeak; dusk keeps its own) --%>
        <div class="twk-row">
          <span class="twk-cap">Accent</span>
          <div class="twk-swatches" role="radiogroup" aria-label="Accent colour">
            <button
              :for={hex <- @accents}
              type="button"
              role="radio"
              class="twk-swatch"
              style={"background: #{hex};"}
              title={hex}
              aria-label={hex}
              data-tweak="accent"
              data-tweak-value={hex}
            ></button>
          </div>
          <span class="twk-accent-note">Dusk uses its built-in accent</span>
        </div>

        <%!-- Syntax → the --t-* token palette (warm/mono/vivid) --%>
        <div class="twk-row">
          <span class="twk-cap">Syntax palette</span>
          <div class="twk-seg" role="radiogroup" aria-label="Syntax palette">
            <button
              :for={mode <- ~w(warm mono vivid)}
              type="button"
              role="radio"
              data-tweak="syntax"
              data-tweak-value={mode}
            >
              {mode}
            </button>
          </div>
        </div>

        <%!-- Density → data-density (--row-h / --pad / --gap) --%>
        <div class="twk-row">
          <span class="twk-cap">Density</span>
          <div class="twk-seg" role="radiogroup" aria-label="Density">
            <button
              :for={d <- ~w(cozy compact)}
              type="button"
              role="radio"
              data-tweak="density"
              data-tweak-value={d}
            >
              {d}
            </button>
          </div>
        </div>

        <%!-- UI font → --ui-font (the shell typeface) --%>
        <div class="twk-row">
          <span class="twk-cap">UI font</span>
          <select class="twk-select" data-tweak="uiFont" aria-label="UI font">
            <option :for={font <- @ui_fonts} value={font}>{font}</option>
          </select>
        </div>

        <%!-- Code font → --code-font (the editor / mono typeface) --%>
        <div class="twk-row">
          <span class="twk-cap">Code font</span>
          <select class="twk-select mono" data-tweak="codeFont" aria-label="Code font">
            <option :for={font <- @code_fonts} value={font}>{font}</option>
          </select>
        </div>

        <p class="twk-note">Appearance only — saved to this browser.</p>
      </div>
    </div>
    """
  end

  # ── Eval activity indicator (BT-5189) ───────────────────────────────────────

  # The cockpit-wide "an evaluation is running / N are waiting" indicator with
  # its Stop control. Every eval — the dock's, the REPL's, and every internal
  # one (renames, removals, destructive flushes, pokes, class compiles) — runs
  # through the dock's single serialised queue, so this is rendered outside the
  # dock (the top bar) and inside modals, where an internal eval queued behind
  # a long user eval would otherwise have no Stop and no pending sign in view.
  # Stop interrupts the running eval and cancels the queued ones; each
  # cancelled operation reports it in its own UI.
  attr :id, :string, required: true
  attr :running, :boolean, required: true
  attr :queued, :integer, required: true

  def eval_activity(assigns) do
    ~H"""
    <span :if={@running} id={@id} class="eval-activity" role="status">
      <span class="kbdhint">
        evaluating…{if @queued > 0, do: " (#{@queued} waiting)"}
      </span>
      <button id={"#{@id}-stop"} class="btn btn-sm" type="button" phx-click="eval_interrupt">
        Stop
      </button>
    </span>
    """
  end
end
