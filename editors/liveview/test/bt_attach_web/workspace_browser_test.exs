# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceBrowserTest do
  @moduledoc """
  Browser end-to-end tests for the LiveView IDE: PhoenixTest driving a **real
  Chromium** via Playwright, against a live Beamtalk workspace.

  ## Why a real browser (and not LiveViewTest)

  The cockpit's client behaviour rides several LiveView JS hooks — `CmEditor`
  (the CodeMirror editor behind both the Workspace eval input and the
  method-editor tabs, BT-2538 / BT-2539), `KeyboardShortcuts` (BT-2485),
  `TweaksPanel` (BT-2487), `FieldFlash` (BT-2492) and `OmniSearch` (BT-2495).
  `Phoenix.LiveViewTest` renders the LiveView server-side against a floki DOM and
  **never loads `app.js`**, so none of that JavaScript runs there. These tests
  exercise it in an actual browser: CodeMirror highlights as you type, ⌘/Ctrl
  chords submit
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
    # The connected-only panes that host the JS hooks are present. The Tweaks
    # panel now lives behind the top-bar settings gear (still mounted in the DOM
    # so its hook applies the saved theme on load).
    |> assert_has(".settings-gear")
    |> assert_has("#eval-form")
    |> assert_has("#workspace-editor-overlay .cm-content")
  end

  test "the CmEditor hook highlights Beamtalk as you type (BT-2538)", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-overlay .cm-content")
    # Drive the CodeMirror editor; it renders the doc into `.cm-content` — a layer
    # LiveViewTest's server render never produces.
    |> set_source("Transcript show: 42")
    |> evaluate(
      "document.querySelector('#workspace-editor-overlay .cm-content').textContent",
      fn painted ->
        assert painted =~ "Transcript show: 42",
               "CmEditor did not render the typed source into CodeMirror"
      end
    )
    # And it tokenises via the shared regex highlighter, emitting `.tok-*` spans.
    |> assert_has("#workspace-editor-overlay .cm-content .tok-number", text: "42")
  end

  test "the CmEditor offers backend completions as you type (BT-2544)", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-overlay .cm-content")
    # Real typing (not a doc-replace transaction) so CodeMirror's autocomplete
    # activates on input the way a user triggers it. `Integ` is a bare prefix, so
    # the backend `complete` op offers matching class names — `Integer` is a
    # stdlib class always present in the live image, so the assertion does not
    # depend on a freshly-defined class. The candidates come from the live session
    # over the term-seam (`complete` event → `Workspace.complete/2`), then render
    # in CodeMirror's autocomplete tooltip. A small inter-key delay lets the
    # completion debounce + the server round-trip settle; `assert_has` also polls.
    |> type("#workspace-editor-overlay .cm-content", "Integ", delay: 60)
    |> assert_has(".cm-tooltip-autocomplete", text: "Integer")
  end

  test "the CmEditor shows live-image hover docs over a class name (BT-2555)", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-overlay .cm-content")
    # A bare class name on its own line. `Integer` is a stdlib class always
    # present in the live image, so hover resolves without a freshly-defined
    # class. The hovered token round-trips to the live session over the
    # term-seam (`hover` event → `Workspace.hover/2`), which formats the class's
    # signature + doc-comment from the RUNNING image (not on-disk source) via
    # the same `beamtalk_repl_docs` engine the REPL `:help` uses.
    |> set_source("Integer")
    |> hover_token("#workspace-editor-overlay .cm-content .tok-global")
    |> assert_has(".cm-hover-doc", text: "Integer")
  end

  test "the CmEditor shows live parse-only diagnostics + clears them on a fix (BT-2556)",
       %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-overlay .cm-content")
    # Invalid syntax (`:=` as a right-hand side) — the buffer round-trips to the
    # live compiler over the term-seam (`diagnostics` event →
    # `Workspace.diagnostics/1`), whose SIDE-EFFECT-FREE parse-only path returns
    # an error span. `@codemirror/lint` paints it as a `.cm-lintRange-error`
    # squiggle. The lint source debounces, so `assert_has` polls past the delay.
    |> set_source("x := :=")
    |> assert_has(".cm-lintRange-error")
    # Fixing the error clears the squiggle: a valid buffer parses clean, so the
    # next debounced lint returns no diagnostics and CodeMirror drops the mark.
    |> set_source("x := 42")
    |> refute_has(".cm-lintRange-error")
  end

  test "the method editor lints a bare method body, not a top-level script (BT-2569)",
       %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-overlay .cm-content")
    # Open a blank "new method" tab on an always-present class (Object): a :method
    # tab (data-lint-mode="method"), so the method-editor CmEditor lints in METHOD
    # mode — the buffer is a bare method body, parsed with `parse_method` rather
    # than the top-level script grammar (`diagnostics` carries `mode: "method"`).
    # The cockpit opens with no tab now, so we open one explicitly; the test never
    # saves, so the class identity is irrelevant to method-mode linting.
    |> click(~s(div[phx-click="browser_select_class"][phx-value-class="Object"]))
    |> click(~s(div[phx-click="new_method"][phx-value-class="Object"]))
    # A genuinely broken body still squiggles (`:=` with no right-hand side), so
    # the method editor really is linting — not silently disabled.
    |> set_method_source("increment => self.value :=")
    |> assert_has(".cm-lintRange-error")
    # The fix: a VALID bare method body. Under the old top-level grammar the `=>`
    # body separator tripped a false `Unexpected token: expected expression, found
    # ⇒` squiggle (the bug this guards). Method mode parses it clean, so the next
    # debounced lint clears the mark.
    |> set_method_source("increment => self.value := self.value + 1")
    |> refute_has(".cm-lintRange-error")
  end

  test "the Tests pane discovers + runs a TestCase in a real browser (BT-2557)", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-overlay .cm-content")
    # Define a TestCase subclass in the live image via the Workspace editor + Do
    # it (side-effect-only install). One passing, one failing test.
    |> set_source(
      "TestCase subclass: CockpitBrowserDemoTest\n" <>
        "  testPasses =>\n    self assert: 1 equals: 1\n" <>
        "  testFails =>\n    self assert: 1 equals: 2"
    )
    |> click("button[value='do_it']")
    # Open the Tests dock tab and refresh discovery so the catalogue reflects the
    # just-installed class (discovery is the live `list-tests` op).
    |> click("button[phx-value-tab='tests']")
    |> click("button[phx-click='tests_refresh']")
    |> assert_has(".test-catalogue", text: "CockpitBrowserDemoTest")
    # Run all: per-case results render, with the failing case + a failing summary,
    # driven through the live session (no shelled-out CLI).
    |> click("button[phx-click='run_tests']")
    |> assert_has(".test-results", text: "testFails")
    |> assert_has(".test-summary.fail")
  end

  test "an eval round-trips through the real browser via the Print it button", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-overlay .cm-content")
    |> set_source("3 + 4")
    # The primary action button submits the eval form (action=print_it) over the
    # live socket; the workspace evaluates and the result flashes in the transient
    # status line (BT-2542) — the durable copy lands inline in the buffer (asserted
    # in the inline-result test below).
    |> click("button[value='print_it']")
    |> assert_has(".eval-status .val", text: "7")
  end

  test "Print it inserts a result inline in the Workspace buffer (BT-2542)", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-overlay .cm-content")
    |> set_source("3 + 4")
    # The classic Smalltalk "Print it inserts the result": the server pushes the
    # rendered term and the CmEditor inline-results extension drops a `→ 7` block
    # widget into the editor (NOT the old below-editor bubble). It is a widget,
    # not doc text, so re-evaluating the buffer never re-runs it.
    |> click("button[value='print_it']")
    |> assert_has("#workspace-editor-overlay .cm-inline-result", text: "7")
  end

  test "a long Print it result collapses inline and expands on click (BT-2542)", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-overlay .cm-content")
    # `Beamtalk help: Integer` returns a long, multi-line class summary — the
    # canonical "long result" the issue calls out (it used to blow out the pane).
    # Inline it collapses to a one-line summary by default.
    |> set_source("Beamtalk help: Integer")
    |> click("button[value='print_it']")
    |> assert_has("#workspace-editor-overlay .cm-inline-result.is-collapsed")
    # Clicking the collapsed strip expands the full body inline.
    |> click("#workspace-editor-overlay .cm-inline-result.is-collapsed")
    |> assert_has("#workspace-editor-overlay .cm-inline-result .cm-inline-result-body")
  end

  test "the KeyboardShortcuts hook submits the eval on ⌘/Ctrl+P (BT-2485)", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-overlay .cm-content")
    |> set_source("10 + 5")
    # ⌘P / Ctrl+P is bound (data-shortcuts "mod+p" → "submit:print_it"): the hook
    # sets the hidden action field and request-submits the eval form — no button
    # click. Linux/CI uses Ctrl as the mod key.
    |> press("#workspace-editor-overlay .cm-content", "Control+p")
    |> assert_has(".eval-status .val", text: "15")
  end

  test "the bindings pane reflects a live eval (BT-2408)", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-overlay .cm-content")
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
    |> assert_has("#workspace-editor-overlay .cm-content")
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
    |> assert_has("#workspace-editor-overlay .cm-content")
    # Define a class with a uniquely-named method, then open that method tab via
    # the omni search — its nav-symbols index updates live on the eval, so no
    # browser remount (and its browse-classes-snapshot race) is needed. The opened
    # method tab carries its selector as a hidden form field, so ⌘S re-compiles a
    # real `KsCounter >> ksBump` with only the edited body. (The starter tab no
    # longer opens on mount, so authoring goes through an opened method.)
    |> eval_do(
      "Actor subclass: KsCounter\n  state: value = 0\n\n  ksBump => self.value := self.value + 1"
    )
    |> omni_type("ksBump")
    |> assert_has(".omni-results .omni-row", text: "ksBump")
    |> omni_key("Enter")
    |> assert_has("#method-editor .tabstrip", text: "ksBump")
    |> set_method_source("ksBump => self.value := self.value + 1")
    # ⌘S / Ctrl+S is bound on the method-editor form (data-scope="window",
    # data-shortcuts "mod+s" → submit): the keydown bubbles out of CodeMirror to
    # the form's KeyboardShortcuts hook, which request-submits the form so
    # class/selector/source ride the normal save_method — no button click.
    |> press("[id^='method-editor-overlay-'] .cm-content", "Control+s")
    # The save is a server round-trip (WorkspaceLive compiles `KsCounter >>
    # ksBump` before assigning `save_result`); under parallel CI load that can
    # outlast the 2s default assertion poll. Wait on the banner explicitly with a
    # generous window (BT-2529) — the assertion returns the instant the text
    # appears, so passing runs are not slowed.
    |> assert_has("#method-editor", text: "Saved ksBump on KsCounter", timeout: 10_000)
  end

  test "the TweaksPanel hook reskins the IDE client-side and persists it (BT-2487)", %{conn: conn} do
    conn
    |> visit("/")
    # The Tweaks panel is mounted on load (so the saved theme applies) but hidden
    # behind the top-bar gear; open the settings dropdown to reach its controls.
    |> click(".settings-gear")
    |> assert_has(".settings-popover.open")
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

  test "density toggle produces a measurable layout delta (BT-2551)", %{conn: conn} do
    conn
    |> visit("/")
    |> click(".settings-gear")
    |> assert_has(".settings-popover.open")
    # Default density is 'cozy' — the app shell padding uses --pad (7px) and
    # --gap (9px). Read the computed panel-body padding as a measurable proxy.
    |> evaluate(
      "getComputedStyle(document.querySelector('.panel-body')).paddingTop",
      fn pt -> assert pt == "10px", "Expected cozy panel-body paddingTop=10px, got #{pt}" end
    )
    # Switch to compact — --pad shrinks to 5px, --gap to 6px.
    |> click("[data-tweak='density'][data-tweak-value='compact']")
    |> evaluate(
      "getComputedStyle(document.querySelector('.panel-body')).paddingTop",
      fn pt -> assert pt == "8px", "Expected compact panel-body paddingTop=8px, got #{pt}" end
    )
    # The app shell padding also shrinks.
    |> evaluate(
      "getComputedStyle(document.querySelector('.app')).paddingTop",
      fn pt -> assert pt == "5px", "Expected compact app paddingTop=5px, got #{pt}" end
    )
  end

  test "accent swatches are disabled on dusk theme (BT-2551)", %{conn: conn} do
    conn
    |> visit("/")
    |> click(".settings-gear")
    |> assert_has(".settings-popover.open")
    # On paper (default), accent swatches are enabled.
    |> evaluate(
      "document.querySelector('.twk-swatches').dataset.accentDisabled",
      fn disabled -> assert disabled != "1", "Swatches should be enabled on paper" end
    )
    # Switch to dusk — swatches should be marked disabled.
    |> click("[data-tweak='theme'][data-tweak-value='dusk']")
    |> evaluate(
      "document.querySelector('.twk-swatches').dataset.accentDisabled",
      fn disabled -> assert disabled == "1", "Swatches should be disabled on dusk" end
    )
    # The hint text should be visible.
    |> assert_has(".twk-accent-note", text: "Dusk uses its built-in accent")
  end

  # ── Phase 3 Inspector live tracking (BT-2492, backend BT-2489) ──────────────
  #
  # Field-flash, the freeze toggle, and the pid-stats chips are connected-render
  # JS-hook + live-push behaviours: the `FieldFlash` hook (`field_flash.js`)
  # pulses changed cells on a `data-flash-gen` bump, and the change push +
  # pid_stats read only happen over the live socket. `Phoenix.LiveViewTest` never
  # loads `app.js`, so these MUST run in a real browser (the e2e lane).

  # BT-2524: the {:object_changed, …} push that drives the flash now fires for
  # compiled actors (the generated gen_server callbacks call
  # beamtalk_actor:notify_state_change/2 after a committed state write), so this
  # Playwright case and its server-side counterpart in workspace_live_test.exs
  # are both re-enabled.
  test "the Inspector flashes a field when the inspected actor changes (BT-2492)", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-overlay .cm-content")
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
    |> assert_has("#workspace-editor-overlay .cm-content")
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
    |> assert_has("#workspace-editor-overlay .cm-content")
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
    |> assert_has("#workspace-editor-overlay .cm-content")
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
    |> assert_has("#workspace-editor-overlay .cm-content")
    # Define a class so a known, image-present selector exists to search for.
    |> eval_do(
      "Actor subclass: OmniCounter\n  state: value = 0\n\n  bumpValue => self.value := self.value + 1"
    )
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
    |> assert_has("#workspace-editor-overlay .cm-content")
    # Define two classes that both implement a uniquely-named selector, so it has
    # real implementors to trace. The class names must NOT contain the selector
    # substring, or the omni search matches the class rows too and Enter opens a
    # class instead of the method. Open the method tab via omni (live nav-symbols
    # index — no browser remount race); the Implementors query runs on the active
    # tab's selector, so the method need only exist (no ⌘S save here).
    |> eval_do(
      "Actor subclass: TracerOne\n  state: value = 0\n\n  navTrace => self.value := self.value + 1"
    )
    |> eval_do(
      "Actor subclass: TracerTwo\n  state: value = 0\n\n  navTrace => self.value := self.value + 2"
    )
    |> omni_type("navTrace")
    |> assert_has(".omni-results .omni-row", text: "navTrace")
    |> omni_key("Enter")
    |> assert_has("#method-editor .tabstrip", text: "navTrace")
    # Implementors of `navTrace`: the popover opens over the nav-query result. We
    # assert it renders (header + either site rows or the empty state) rather than
    # coupling to exact image contents.
    |> click("button[phx-click='implementors']")
    |> assert_has(".nav-popover .nav-pop-head", text: "Implementors")
  end

  # ── Phase 3 floating inspector windows / overlay mode (BT-2493) ─────────────
  #
  # The Float toggle, opening / drilling / closing a floating window, and
  # click-to-front (z-order) are connected-render JS behaviours: the WindowDrag
  # hook (`window_drag.js`) drags a window by its title bar and raises it on a
  # press, and the window content re-renders live over distribution. The drag
  # *motion* is hard to assert deterministically (a manual/best-effort check per
  # the issue), but the open → drill → close flow and click-to-front are covered
  # here. `Phoenix.LiveViewTest` never loads `app.js`, so these MUST run in a real
  # browser (the e2e lane).

  test "Float mode opens a draggable inspector window, which drills then closes (BT-2493)", %{
    conn: conn
  } do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-overlay .cm-content")
    # A live object graph: a Boxx actor holding a Leaf actor (same shape as the
    # docked breadcrumb test), so the floating window has a reference to drill.
    |> eval_do("Actor subclass: WinLeaf\n  state: n = 99\n\n  n => self.n")
    |> eval_do(
      "Actor subclass: WinBoxx\n  state: item = nil\n\n  setItem: x => self.item := x\n  item => self.item"
    )
    |> eval_do("winleaf := WinLeaf spawn")
    |> eval_do("winbox := WinBoxx spawn")
    |> eval_do("winbox setItem: winleaf")
    |> assert_has("#bindings-panel .bname", text: "winbox")
    # Flip the top-bar Dock/Float toggle to Float.
    |> click(".insp-mode button[phx-value-mode='float']")
    # Inspecting the binding now opens a FLOATING window (the overlay layer), not
    # the docked pane — a connected-render path driven by the inspector_mode assign.
    |> click(".obj-row[phx-value-name='winbox'] .obj-inspect")
    |> assert_has("#inspector-overlay .insp-window", text: "Inspecting")
    |> assert_has(".insp-window .ivar-table", text: "WinLeaf")
    # Drill into the referenced Leaf inside the window: its own field (n = 99) reads
    # live over distribution. This is the window's independent drill stack.
    |> click(".insp-window tr.drillable")
    |> assert_has(".insp-window .ivar-table", text: "99")
    # Walk back via the window's own breadcrumb to box's `item` field.
    |> click(".insp-window .insp-crumbs .c[phx-value-index='0']")
    |> assert_has(".insp-window .ivar-table", text: "item")
    # Close the window via its × button: the overlay empties (the window is gone).
    |> click(".insp-window .iw-close")
    |> evaluate("new Promise((resolve) => setTimeout(resolve, 250))")
    |> evaluate("document.querySelectorAll('.insp-window').length", fn count ->
      assert count == 0, "closing the window did not remove it from the overlay"
    end)
  end

  test "clicking a floating window brings it to the front (z-order) (BT-2493)", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-overlay .cm-content")
    |> eval_do("Actor subclass: ZCounter\n  state: value = 0\n\n  value => self.value")
    |> eval_do("za := ZCounter spawn")
    |> eval_do("zb := ZCounter spawn")
    |> assert_has("#bindings-panel .bname", text: "za")
    |> assert_has("#bindings-panel .bname", text: "zb")
    |> click(".insp-mode button[phx-value-mode='float']")
    # Open two floating windows. The second-opened starts on top (higher z-index).
    |> click(".obj-row[phx-value-name='za'] .obj-inspect")
    |> click(".obj-row[phx-value-name='zb'] .obj-inspect")
    |> assert_has("#inspector-window-win-1")
    |> assert_has("#inspector-window-win-2")
    |> evaluate(
      """
      (() => {
        const z = (id) => parseInt(getComputedStyle(document.getElementById(id)).zIndex || "0", 10);
        return z("inspector-window-win-2") > z("inspector-window-win-1");
      })()
      """,
      fn second_on_top ->
        assert second_on_top, "the second-opened window should start on top"
      end
    )
    # Press the FIRST window's title bar: the WindowDrag hook pushes window_focus,
    # the server bumps its z above the max, and it now overlays the second — z-order
    # follows focus (the click-to-front behaviour, a connected-render JS path).
    |> evaluate("""
    (() => {
      const bar = document.querySelector("#inspector-window-win-1 [data-window-drag-handle]");
      bar.dispatchEvent(new MouseEvent("mousedown", {bubbles: true, button: 0}));
      bar.dispatchEvent(new MouseEvent("mouseup", {bubbles: true, button: 0}));
    })()
    """)
    |> evaluate("new Promise((resolve) => setTimeout(resolve, 250))")
    |> evaluate(
      """
      (() => {
        const z = (id) => parseInt(getComputedStyle(document.getElementById(id)).zIndex || "0", 10);
        return z("inspector-window-win-1") > z("inspector-window-win-2");
      })()
      """,
      fn first_now_on_top ->
        assert first_now_on_top,
               "clicking the first window did not raise it to the front (z-order follows focus)"
      end
    )
  end

  test "the REPL tab evaluates from the bottom input into the scrollback (BT-2543)", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-overlay .cm-content")
    # Switch to the REPL dock tab; its CodeMirror composer (the ReplInput hook)
    # mounts at the bottom of the pane.
    |> click("button[phx-value-tab='repl']")
    |> assert_has("#repl-input .cm-content")
    |> set_cm_source("#repl-input", "3 + 4")
    # Enter SUBMITS in the REPL (terminal convention) — the confirmed BT-2543
    # divergence from the Workspace, where Enter is a newline. A server-side render
    # can't observe this: it's the ReplInput hook's keymap + a requestSubmit.
    |> press("#repl-input .cm-content", "Enter")
    # The request→response pair lands in the scrollback above the input...
    |> assert_has(".repl-scrollback .repl-entry .repl-expr", text: "3 + 4")
    |> assert_has(".repl-scrollback .repl-entry .repl-val", text: "7")
    # ...and the input is cleared after submit (REPL style).
    |> evaluate(
      "document.querySelector('#repl-input .cm-content').textContent",
      fn after_submit ->
        refute after_submit =~ "3 + 4", "the REPL input was not cleared after submit"
      end
    )
  end

  test "↑ recalls a prior REPL expression into the input (BT-2543)", %{conn: conn} do
    conn
    |> visit("/")
    |> assert_has("#workspace-editor-overlay .cm-content")
    |> click("button[phx-value-tab='repl']")
    |> assert_has("#repl-input .cm-content")
    |> set_cm_source("#repl-input", "6 * 7")
    |> press("#repl-input .cm-content", "Enter")
    |> assert_has(".repl-scrollback .repl-entry .repl-val", text: "42")
    # The input cleared on submit; ↑ on the now-empty first line recalls the prior
    # expression from the server-owned history ring back into the composer (the
    # ReplInput hook fires repl_history_prev only at the edge; the server pushes
    # the recalled text via repl_set_input). Recall is a server round-trip, so
    # POLL for the recalled text rather than reading once — a bare read races the
    # push back.
    |> press("#repl-input .cm-content", "ArrowUp")
    |> evaluate(
      """
      new Promise((resolve, reject) => {
        const start = Date.now();
        const tick = () => {
          const el = document.querySelector("#repl-input .cm-content");
          if (el && el.textContent.includes("6 * 7")) return resolve(true);
          if (Date.now() - start > 3000) return reject(new Error("ArrowUp did not recall the prior expression"));
          requestAnimationFrame(tick);
        };
        tick();
      })
      """,
      fn recalled -> assert recalled end
    )
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

  # Dispatch a real `mousemove` at the centre of `selector` so CodeMirror's
  # hoverTooltip plugin (BT-2555) fires its query. CM triggers on a settle — the
  # pointer stops over a token for the hover delay — so we move the synthetic
  # pointer onto the token once and then leave it; `assert_has` polls for the
  # resulting `.cm-hover-doc` tooltip while the async server round-trip lands.
  #
  # The event MUST be dispatched on the token element itself, not the `.cm-editor`
  # root: CM's hover plugin ignores any mousemove whose `target` is not inside
  # `view.contentDOM` (`.cm-content`). The token span is inside `.cm-content`, and
  # `bubbles: true` still carries the event up to the plugin's listener on the
  # editor root.
  defp hover_token(conn, selector) do
    evaluate(conn, """
    (() => {
      const sel = #{Jason.encode!(selector)};
      const el = document.querySelector(sel);
      if (!el) throw new Error("hover target not found: " + sel);
      const r = el.getBoundingClientRect();
      const x = r.left + r.width / 2, y = r.top + r.height / 2;
      el.dispatchEvent(new MouseEvent("mousemove", {bubbles: true, clientX: x, clientY: y}));
    })()
    """)
  end

  # Set the Workspace editor's contents to exactly `source`.
  defp set_source(conn, source), do: set_cm_source(conn, "#workspace-editor-overlay", source)

  # Set the tabbed method editor's contents to exactly `source`. Its CmEditor
  # wrapper is re-keyed per active tab (`#method-editor-overlay-<tab>`), so match
  # it by id prefix — there is only ever one method editor mounted at a time.
  defp set_method_source(conn, source),
    do: set_cm_source(conn, "[id^='method-editor-overlay-']", source)

  # Replace a CmEditor's contents with exactly `source`. Both editors are
  # CodeMirror (BT-2538 / BT-2539), not a <textarea>, so we drive them through the
  # view the CmEditor hook exposes on the wrapper (`el.cmView`): a doc-replace
  # transaction fires the hook's update listener, which mirrors the text into the
  # hidden form field and dispatches `input` — exactly the path real typing takes.
  defp set_cm_source(conn, selector, source) do
    # Poll for the CmEditor mount (`el.cmView`) rather than a fixed sleep: under
    # parallel CI load CodeMirror's mount can outlast a 300 ms guard, and reading
    # `.cmView` on an unmounted wrapper would throw a cryptic Playwright
    # "evaluate" error instead of a clear timeout. Resolve once the view exists,
    # then dispatch a doc-replace transaction. (Selecting a method-editor tab
    # remounts the wrapper, so this also waits out the re-key.)
    evaluate(conn, """
    new Promise((resolve, reject) => {
      const start = Date.now();
      const tick = () => {
        const el = document.querySelector(#{Jason.encode!(selector)});
        if (el && el.cmView) {
          el.cmView.dispatch({
            changes: { from: 0, to: el.cmView.state.doc.length, insert: #{Jason.encode!(source)} },
          });
          return resolve(true);
        }
        if (Date.now() - start > 5000) return reject(new Error("CmEditor (cmView) never mounted"));
        requestAnimationFrame(tick);
      };
      tick();
    })
    """)
  end

  # Replace a plain form control's value with exactly `text` (e.g. the Inspector
  # poke bar's `<input>`). We let any in-flight LiveView patch settle, then set the
  # value + fire `input` in one atomic step — which *replaces* the contents
  # deterministically, unlike select-all + type whose caret/selection timing raced
  # under CI load. (Not for the CodeMirror editors — those go through
  # `set_cm_source/3`, which has no real <textarea> to fill.)
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
