# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceBrowserTest do
  @moduledoc """
  Browser end-to-end tests for the LiveView IDE: PhoenixTest driving a **real
  Chromium** via Playwright, against a live Beamtalk workspace.

  ## Why a real browser (and not LiveViewTest)

  The cockpit's client behaviour rides six LiveView JS hooks — `CodeEditor`,
  `KeyboardShortcuts`, `SelectionTracker` (BT-2485), `TweaksPanel` (BT-2487),
  `FieldFlash` (BT-2492) and `OmniSearch` (BT-2495). `Phoenix.LiveViewTest`
  renders the LiveView server-side against a floki DOM and **never loads
  `app.js`**, so none of that JavaScript runs there. These tests exercise it in an
  actual browser: the highlight overlay repaints as you type, ⌘/Ctrl chords submit
  the eval + method forms, the bindings pane and Inspector re-render live over
  distribution, the Tweaks panel reskins the IDE via CSS variables + `localStorage`,
  the Inspector flashes changed fields + tracks/freezes a live actor, and the
  top-bar omni search walks its results with the arrow keys + opens the active one
  on Enter — none of which a server-side render can observe.

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
    |> eval_do(
      "Actor subclass: Boxx\n  state: item = nil\n\n  setItem: x => self.item := x\n  item => self.item"
    )
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

  # ── Phase 3 Inspector live tracking (BT-2492, backend BT-2489) ──────────────
  #
  # Field-flash, the freeze toggle, and the pid-stats chips are connected-render
  # JS-hook + live-push behaviours: the `FieldFlash` hook (`field_flash.js`)
  # pulses changed cells on a `data-flash-gen` bump, and the change push +
  # pid_stats read only happen over the live socket. `Phoenix.LiveViewTest` never
  # loads `app.js`, so these MUST run in a real browser (the e2e lane).

  # Deferred to BT-2524: the cross-node {:object_changed, …} push that drives the
  # flash is not delivered to the LiveView on the e2e lane (the server-side
  # assertion in workspace_live_test.exs is skipped for the same reason). The
  # freeze/poke/chips browser cases below do not depend on the async push.
  @tag skip: "BT-2524: cross-node object_changed push not delivered in e2e"
  test "the Inspector flashes a field when the inspected actor changes (BT-2492)", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-source")
    # A live Counter actor with a mutator, spawned + bound in this session.
    |> eval_do(
      "Actor subclass: FlashCounter\n  state: value = 0\n\n  value => self.value\n\n  increment => self.value := self.value + 1"
    )
    |> eval_do("fc := FlashCounter spawn")
    |> assert_has("#bindings-panel .bname", text: "fc")
    # Inspect it: the pane subscribes to its per-object change stream and reads its
    # fields (value = 0) + pid stats.
    |> click(".obj-row[phx-value-name='fc'] .obj-inspect")
    |> assert_has("#inspector-panel", text: "Inspecting")
    |> assert_has("#inspector-fields td[data-flash-key='value']", text: "0")
    # Send `increment`: the workspace commits a state write and pushes
    # {:object_changed, …}; the pane re-reads (value → 1) and bumps data-flash-gen,
    # which the FieldFlash hook turns into a `.vflash` pulse on the changed cell.
    # The flash class is transient (~700ms), so we poll for it RIGHT AFTER the
    # write (before any other assertion consumes the window) — a layer the server
    # render can't observe.
    |> eval_do("fc increment")
    |> assert_flashed("#inspector-fields td[data-flash-key='value']")
    # The value also re-read live to 1.
    |> assert_has("#inspector-fields td[data-flash-key='value']", text: "1")
  end

  test "the freeze toggle holds a snapshot, then resumes live tracking (BT-2492)", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-source")
    |> eval_do(
      "Actor subclass: FreezeCounter\n  state: value = 0\n\n  value => self.value\n\n  increment => self.value := self.value + 1"
    )
    |> eval_do("frz := FreezeCounter spawn")
    |> assert_has("#bindings-panel .bname", text: "frz")
    |> click(".obj-row[phx-value-name='frz'] .obj-inspect")
    |> assert_has("#inspector-panel", text: "Inspecting")
    # Tracking is live by default.
    |> assert_has(".insp-freeze.live", text: "live")
    # Freeze: the toggle flips to "frozen" and the change subscription is dropped.
    |> click(".insp-freeze")
    |> assert_has(".insp-freeze.frozen", text: "frozen")
    # A write while frozen does NOT update the pane — it holds the snapshot (0).
    |> eval_do("frz increment")
    |> assert_has("#inspector-fields td[data-flash-key='value']", text: "0")
    # Unfreeze: re-subscribe + catch up — the pane re-reads the value written while
    # frozen (now 1) and tracking is live again.
    |> click(".insp-freeze")
    |> assert_has(".insp-freeze.live", text: "live")
    |> assert_has("#inspector-fields td[data-flash-key='value']", text: "1")
  end

  test "the Inspector head shows live pid-stats chips (BT-2492)", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-source")
    |> eval_do("Actor subclass: StatCounter\n  state: value = 0\n\n  value => self.value")
    |> eval_do("sc := StatCounter spawn")
    |> assert_has("#bindings-panel .bname", text: "sc")
    |> click(".obj-row[phx-value-name='sc'] .obj-inspect")
    |> assert_has("#inspector-panel", text: "Inspecting")
    # The process-health chips read via the pid_stats op (status dot, mailbox depth,
    # reductions) — only present on the connected render over distribution.
    |> assert_has("#inspector-panel .chip.pid-stat", text: "mailbox")
    |> assert_has("#inspector-panel .chip.pid-stat", text: "reductions")
  end

  test "an owner can poke the inspected actor with a message (BT-2492)", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-source")
    |> eval_do(
      "Actor subclass: PokeCounter\n  state: value = 0\n\n  value => self.value\n\n  increment => self.value := self.value + 1"
    )
    |> eval_do("pk := PokeCounter spawn")
    |> assert_has("#bindings-panel .bname", text: "pk")
    |> click(".obj-row[phx-value-name='pk'] .obj-inspect")
    |> assert_has("#inspector-panel", text: "Inspecting")
    # The owner-only poke bar sends `pk increment` via the eval op; the field
    # re-reads to 1 (the change stream) and a confirmation renders.
    |> set_text(".poke input[name='message']", "increment")
    |> click(".poke button[type='submit']")
    |> assert_has("#inspector-fields td[data-flash-key='value']", text: "1")
  end

  # ── Phase 3 navigation aids (BT-2495) ───────────────────────────────────────
  #
  # The omni-search keyboard nav (arrow highlight + Enter open) and the
  # Senders/Implementors popovers are connected-render behaviours: the `OmniSearch`
  # hook (`omni_search.js`) moves the `.active` highlight + opens the active row on
  # real `keydown`s, and the popover content is fetched live over distribution.
  # `Phoenix.LiveViewTest` never loads `app.js`, so these MUST run in a real
  # browser (the e2e lane).

  test "omni search filters classes/selectors and arrow+enter opens a result (BT-2495)", %{
    conn: conn
  } do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-source")
    # Define a class so a known, image-present selector exists to search for.
    |> eval_do("Actor subclass: OmniCounter\n  state: value = 0\n\n  bumpValue => self.value := self.value + 1")
    # Type into the top-bar omni search; the server filters the nav-symbols index
    # and renders the results popover (a class row + the selector rows).
    |> omni_type("OmniCounter")
    |> assert_has("#omni-search ~ .omni-results .omni-row")
    # The first row is highlighted by default; ArrowDown moves the highlight to the
    # next row. `omni_key` also fires the keyup (→ phx-keyup with the unchanged
    # query), so this asserts the highlight SURVIVES the same-query re-render
    # instead of snapping back to row 0 — the regression the hybrid hook guards.
    |> omni_key("ArrowDown")
    |> evaluate(
      "document.querySelectorAll('.omni-results .omni-row').length > 0 && document.querySelectorAll('.omni-results .omni-row')[1] && document.querySelectorAll('.omni-results .omni-row')[1].classList.contains('active')",
      fn moved ->
        assert moved, "OmniSearch hook did not move the .active highlight on ArrowDown"
      end
    )
    # Search specifically for the selector so Enter opens a method tab.
    |> omni_type("bumpValue")
    |> assert_has(".omni-results .omni-row", text: "bumpValue")
    |> omni_key("Enter")
    # Enter opens the active result — a selector row opens an editable method tab,
    # so the editor breadcrumb/tab strip now shows bumpValue.
    |> assert_has("#method-editor .tabstrip", text: "bumpValue")
  end

  test "the Senders/Implementors popover lists sites and opens one (BT-2495)", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-source")
    # Define a class whose method sends a selector we can then trace. The starter
    # tab targets Counter#increment; define both so a real implementor exists.
    |> eval_do("Actor subclass: NavCounter\n  state: value = 0\n\n  step => self.value := self.value + 1")
    # Select the starter method tab so the active selector is `increment`, then
    # open the Implementors popover for it.
    |> click(".tabstrip button[role='tab']")
    |> set_text("textarea[name='source']", "increment => self.value := self.value + 1")
    |> press("textarea[name='source']", "Control+s")
    |> assert_has("#method-editor", text: "Saved increment on Counter")
    # Implementors of `increment`: the popover opens over the nav-query result. We
    # assert it renders (header + either site rows or the empty state) rather than
    # coupling to exact image contents.
    |> click("button[phx-click='implementors']")
    |> assert_has(".nav-popover .nav-pop-head", text: "Implementors")
  end

  # ── helpers ─────────────────────────────────────────────────────────────────

  # Type `query` into the omni search input and fire the `keyup` event the server
  # listens for (phx-keyup="omni_search"), so the results popover re-renders.
  defp omni_type(conn, query) do
    conn
    |> evaluate("new Promise((resolve) => setTimeout(resolve, 200))")
    |> evaluate("""
    (() => {
      const el = document.querySelector("#omni-search");
      el.focus();
      el.value = #{Jason.encode!(query)};
      el.dispatchEvent(new KeyboardEvent("keyup", {bubbles: true, key: "x"}));
    })()
    """)
  end

  # Dispatch a real `keydown` AND `keyup` for `key` on the omni input. The
  # keydown drives the OmniSearch hook (arrow highlight / Enter open / Escape
  # close); the keyup fires `phx-keyup="omni_search"` with the unchanged query,
  # forcing the server to re-render the SAME result list — the exact path that
  # must NOT snap the arrow highlight back to row 0 (the hook's `updated/0`
  # preserves it when the query text is unchanged). Firing both here keeps the
  # keyboard-nav test honest about that race.
  defp omni_key(conn, key) do
    conn
    |> evaluate("""
    (() => {
      const el = document.querySelector("#omni-search");
      el.focus();
      const opts = {bubbles: true, cancelable: true, key: #{Jason.encode!(key)}};
      el.dispatchEvent(new KeyboardEvent("keydown", opts));
      el.dispatchEvent(new KeyboardEvent("keyup", opts));
    })()
    """)
    |> evaluate("new Promise((resolve) => setTimeout(resolve, 200))")
  end

  # Evaluate `code` against the workspace for its side effects (the "Do it"
  # action), used to stage session state (class defs, spawns, bindings).
  defp eval_do(conn, code) do
    conn
    |> set_source(code)
    |> click("button[value='do_it']")
  end

  # Assert the FieldFlash hook applied (at some point) the `.vflash` pulse to the
  # cell `selector`. The class is transient (a ~700ms CSS animation), so poll the
  # browser for it rather than asserting a single instant. Returns the conn so it
  # chains. Fails if the flash never appears within the window. Relies on the
  # fresh-page invariant (each test `visit("/")`s) so a `.vflash` we observe is
  # this scenario's, not a leftover — call it at most once per test, right after
  # the triggering change.
  defp assert_flashed(conn, selector) do
    conn
    |> evaluate(
      """
      new Promise((resolve) => {
        const sel = #{Jason.encode!(selector)};
        const start = Date.now();
        const tick = () => {
          const el = document.querySelector(sel);
          if (el && el.classList.contains("vflash")) { resolve(true); return; }
          if (Date.now() - start > 3000) { resolve(false); return; }
          requestAnimationFrame(tick);
        };
        tick();
      })
      """,
      fn flashed ->
        assert flashed, "FieldFlash hook never applied .vflash to #{selector}"
      end
    )
  end

  # Set the Workspace editor's contents to exactly `source`.
  defp set_source(conn, source), do: set_text(conn, "#workspace-editor-source", source)

  # Replace a `<textarea>`'s contents with exactly `text`. Two wrinkles the
  # connected IDE forces us to handle: the editor ships a starter expression (so
  # it is never empty — `WorkspaceLive` `bind_session/3` / `init_tabs/1`), and
  # each eval re-renders the form (LiveView patches `@expr`/`@edit_source` back
  # in). So we let any in-flight patch settle, then set the value + fire the
  # `input` event the CodeEditor hook listens for in one atomic step — which
  # *replaces* the contents deterministically, unlike select-all + type, whose
  # caret/selection timing raced under CI load.
  defp set_text(conn, selector, text) do
    conn
    |> evaluate("new Promise((resolve) => setTimeout(resolve, 300))")
    |> evaluate("""
    (() => {
      const el = document.querySelector(#{Jason.encode!(selector)});
      el.focus();
      el.value = #{Jason.encode!(text)};
      el.dispatchEvent(new Event("input", {bubbles: true}));
    })()
    """)
  end
end
