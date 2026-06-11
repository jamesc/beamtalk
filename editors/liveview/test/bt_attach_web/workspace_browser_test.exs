# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceBrowserTest do
  @moduledoc """
  Browser end-to-end tests for the LiveView IDE: PhoenixTest driving a **real
  Chromium** via Playwright, against a live Beamtalk workspace.

  ## Why a real browser (and not LiveViewTest)

  The cockpit's client behaviour rides four LiveView JS hooks — `CodeEditor`,
  `KeyboardShortcuts`, `SelectionTracker` (BT-2485) and `TweaksPanel` (BT-2487).
  `Phoenix.LiveViewTest` renders the LiveView server-side against a floki DOM and
  **never loads `app.js`**, so none of that JavaScript runs there. These tests
  exercise it in an actual browser: the highlight overlay repaints as you type,
  ⌘/Ctrl chords submit the eval + method forms, the bindings pane and Inspector
  re-render live over distribution, and the Tweaks panel reskins the IDE via CSS
  variables + `localStorage` — none of which a server-side render can observe.

  ## What they require (double-gated)

  The interesting UI lives behind the *connected* render (`if @connected`), which
  only appears once the LiveView attaches to a workspace node. So these need BOTH:

    * a running workspace + its cookie (`BT_WORKSPACE_COOKIE`) — the `:workspace`
      gate, exactly like `WorkspaceLiveTest`; and
    * the Playwright browser install + `PHX_PLAYWRIGHT=1` — the `:playwright` gate.

  A bare `mix test` (e.g. the `liveview.yml` lane) excludes both. The dedicated
  `liveview-e2e.yml` lane provisions a workspace + Chromium and runs them. To run
  locally:

      just build
      ./target/debug/beamtalk workspace create e2e --background --persistent
      cd editors/liveview
      mix assets.build                 # app.js must exist for the LiveSocket
      npm --prefix assets install
      npx --prefix assets playwright install chromium --with-deps
      export BT_WORKSPACE_NODE=beamtalk_workspace_e2e@localhost
      export BT_WORKSPACE_COOKIE=$(cat ~/.beamtalk/workspaces/e2e/cookie)
      export PHX_PLAYWRIGHT=1
      mix test --only playwright

  Auth: OIDC is unconfigured in test, so the `/` route is open and the LiveView
  attaches as the `:owner` role — every hook + the eval form is in scope. Do
  **not** set `BT_IDE_DEV_AUTH`: it switches on the loopback dev-auth gate, which
  the headless browser does not satisfy, leaving the IDE stuck on "connecting…".
  """
  use PhoenixTest.Playwright.Case, async: false

  # Needs a workspace (connected render) AND the Playwright browser. See @moduledoc.
  @moduletag :workspace
  @moduletag :playwright

  test "the cockpit shell attaches and renders the connected IDE in a real browser", %{
    conn: conn
  } do
    conn
    |> visit("/")
    # The "attached" indicator appears only on a successful connected mount —
    # proof the real browser handshook the LiveSocket and the workspace attach
    # over distribution succeeded (not merely the disconnected HTTP render).
    |> assert_has(".att-label", text: "attached")
    # The connected-only panes that host the JS hooks are present.
    |> assert_has("#tweaks-panel")
    |> assert_has("#eval-form")
    |> assert_has("#workspace-editor-source")
  end

  test "the CodeEditor hook repaints the highlight overlay as you type (BT-2485)", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-source")
    # Type Beamtalk source into the transparent textarea. The CodeEditor hook's
    # `input` handler re-runs the highlighter and paints the result into the
    # sibling `<pre><code>` — a layer LiveViewTest's server render never touches.
    |> set_source("Transcript show: 42")
    |> evaluate(
      "document.querySelector('#workspace-editor-overlay pre.bt-editor-pre code').textContent",
      fn painted ->
        assert painted =~ "Transcript show: 42",
               "CodeEditor hook did not mirror the typed source into the highlight overlay"
      end
    )
    # And it tokenises — the highlighter emits `.tok-*` spans, so the overlay is
    # not just a plain-text copy.
    |> evaluate(
      "document.querySelectorAll('#workspace-editor-overlay pre.bt-editor-pre code [class^=\"tok-\"]').length",
      fn count -> assert count > 0, "highlighter produced no .tok-* token spans" end
    )
  end

  test "an eval round-trips through the real browser via the Print it button", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-source")
    |> set_source("3 + 4")
    # The primary action button submits the eval form (action=print_it) over the
    # live socket; the workspace evaluates and the result term renders.
    |> click("button[value='print_it']")
    |> assert_has(".ws-result .val", text: "7")
  end

  test "the KeyboardShortcuts hook submits the eval on ⌘/Ctrl+P (BT-2485)", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-source")
    |> set_source("10 + 5")
    # ⌘P / Ctrl+P is bound (data-shortcuts "mod+p" → "submit:print_it"): the hook
    # sets the hidden action field and request-submits the eval form — no button
    # click. Linux/CI uses Ctrl as the mod key.
    |> press("#workspace-editor-source", "Control+p")
    |> assert_has(".ws-result .val", text: "15")
  end

  test "the bindings pane reflects a live eval (BT-2408)", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-source")
    |> set_source("greeting := 42")
    |> click("button[value='do_it']")
    # Defining a binding fires the BT-2399 `bindings` push; the pane re-reads the
    # read-surface live and lists the new name + value — a cross-process round-trip
    # (eval → workspace → push → re-render) that only a connected socket exercises.
    |> assert_has("#bindings-panel .bname", text: "greeting")
    |> assert_has("#bindings-panel", text: "42")
  end

  test "the Inspector follows a live reference and walks back via a breadcrumb (BT-2408)", %{
    conn: conn
  } do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-source")
    # Build a live object graph: a Boxx actor holding a Leaf actor, both spawned on
    # the workspace node and bound in this session.
    |> eval_do("Actor subclass: Leaf\n  state: n = 99\n\n  n => self.n")
    |> eval_do("Actor subclass: Boxx\n  state: item = nil\n\n  setItem: x => self.item := x\n  item => self.item")
    |> eval_do("leaf := Leaf spawn")
    |> eval_do("box := Boxx spawn")
    |> eval_do("box setItem: leaf")
    # The bindings pane lists the live object handle; its "Inspect →" opens the
    # Inspector on box, whose `item` field is itself a live object reference.
    |> assert_has("#bindings-panel .bname", text: "box")
    |> click(".obj-row[phx-value-name='box'] .obj-inspect")
    |> assert_has("#inspector-panel", text: "Inspecting")
    |> assert_has("#inspector-panel .ivar-table", text: "Leaf")
    # "follow →" drills into the referenced Leaf — read live over distribution, so
    # its own field (n = 99) renders. This reference-following is the whole point
    # of carrying terms (not JSON) to the LiveView.
    |> click("#inspector-panel tr.drillable")
    |> assert_has("#inspector-panel .ivar-table", text: "99")
    # The drill breadcrumb walks back to box (re-inspecting that earlier level).
    |> click("#inspector-panel .insp-crumbs .c[phx-value-index='0']")
    |> assert_has("#inspector-panel .ivar-table", text: "item")
  end

  test "the KeyboardShortcuts hook compiles a method on ⌘/Ctrl+S (BT-2485)", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-source")
    # Define the class the starter method tab targets, then select that tab so its
    # class/selector load into the method-editor fields.
    |> eval_do("Actor subclass: Counter\n  state: value = 0\n\n  value => self.value")
    |> click(".tabstrip button[role='tab']")
    |> set_text("textarea[name='source']", "increment => self.value := self.value + 1")
    # ⌘S / Ctrl+S is bound on the method-editor form (data-scope="window",
    # data-shortcuts "mod+s" → submit): the hook request-submits the form so
    # class/selector/source ride the normal save_method — no button click.
    |> press("textarea[name='source']", "Control+s")
    |> assert_has("#method-editor", text: "Saved increment on Counter")
  end

  test "the TweaksPanel hook reskins the IDE client-side and persists it (BT-2487)", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#tweaks-panel")
    # The IDE boots on the default 'paper' theme (data-theme on <html>).
    |> evaluate("document.documentElement.getAttribute('data-theme')", fn theme ->
      assert theme == "paper"
    end)
    # Clicking the 'dusk' theme button is a PURE client-side hook action (no
    # server round-trip): it flips data-theme on <html>…
    |> click("[data-tweak='theme'][data-tweak-value='dusk']")
    |> evaluate("document.documentElement.getAttribute('data-theme')", fn theme ->
      assert theme == "dusk", "TweaksPanel did not apply the chosen theme to <html>"
    end)
    # …and persists the choice to localStorage so it survives a reload.
    |> evaluate("window.localStorage.getItem('bt_cockpit_tweaks')", fn stored ->
      assert is_binary(stored) and stored =~ "dusk",
             "TweaksPanel did not persist the theme to localStorage"
    end)
  end

  # ── helpers ─────────────────────────────────────────────────────────────────

  # Evaluate `code` against the workspace for its side effects (the "Do it"
  # action), used to stage session state (class defs, spawns, bindings).
  defp eval_do(conn, code) do
    conn
    |> set_source(code)
    |> click("button[value='do_it']")
  end

  # Set the Workspace editor's contents to exactly `source`.
  defp set_source(conn, source), do: set_text(conn, "#workspace-editor-source", source)

  # Replace a `<textarea>`'s contents with `text`. Two wrinkles the connected IDE
  # forces us to handle: the editor ships a starter expression (so it is never
  # empty — `WorkspaceLive` `bind_session/3` / `init_tabs/1`), and each eval
  # re-renders the form (LiveView patches `@expr`/`@edit_source` back in). So we
  # let any in-flight patch settle, then select-all + type, which *replaces* the
  # current value rather than prepending to it.
  defp set_text(conn, selector, text) do
    conn
    |> evaluate("new Promise((resolve) => setTimeout(resolve, 200))")
    |> press(selector, "ControlOrMeta+a")
    |> type(selector, text)
  end
end
