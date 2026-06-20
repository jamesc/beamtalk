# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceLiveTest do
  @moduledoc """
  End-to-end proof of the Wave-1 LiveView IDE (BT-2407) through the full Phoenix
  LiveView stack against a *real* running Beamtalk workspace node, on the
  BT-2394 Attach topology. Exercises the BT-2399 term-returning op layer (eval)
  and subscription facade (Transcript), plus session-bound eval state.

  Requires a workspace and its cookie in the environment:

      beamtalk workspace create spike --background --persistent
      export BT_WORKSPACE_NODE=beamtalk_workspace_spike@localhost
      export BT_WORKSPACE_COOKIE=$(sed 's/-setcookie //;s/ //g' \\
        ~/.beamtalk/workspaces/spike/vm.args)

  Tag-gated: excluded from the default `mix test` run (see test_helper.exs) and
  run only in the workspace-gated CI job.
  """
  use BtAttachWeb.ConnCase
  import Phoenix.LiveViewTest

  # Excluded by default in test_helper.exs unless BT_WORKSPACE_COOKIE is set.
  @moduletag :workspace

  # ── RBAC: a read-only Observer at the connected LiveView (BT-2421/2424) ──────

  describe "Observer (read-only role) on a live socket" do
    setup do
      Application.put_env(:bt_attach, :oidc, %{
        issuer: "https://idp",
        client_id: "id",
        redirect_uri: "https://ide/callback",
        groups_claim: "groups",
        client_secret: "x",
        roles: %{"owner" => ["beamtalk-owners"], "observer" => ["beamtalk-observers"]}
      })

      Application.put_env(:bt_attach, :session_ttl_secs, 3600)

      on_exit(fn ->
        Application.delete_env(:bt_attach, :oidc)
        Application.delete_env(:bt_attach, :session_ttl_secs)
      end)

      :ok
    end

    defp observer_conn(conn) do
      Plug.Test.init_test_session(conn, %{
        "bt_user" => %{"sub" => "bob", "groups" => ["beamtalk-observers"]},
        "bt_logged_in_at" => System.system_time(:second)
      })
    end

    test "the execute UI (eval form) is hidden for an Observer", %{conn: conn} do
      {:ok, _view, html} = live(observer_conn(conn), "/")
      assert html =~ "read-only (Observer)"
      refute html =~ ~s(phx-submit="eval")
    end

    test "a crafted eval event from an Observer is refused, not crashed", %{conn: conn} do
      {:ok, view, _html} = live(observer_conn(conn), "/")

      # The form is gated away, but a crafted client event must still be refused
      # by the facade/RBAC (BT-2421) and rendered, not crash the LiveView on an
      # unmatched case clause (the BT-2420 facade returns a 2-tuple eval never did).
      html = render_hook(view, "eval", %{"expr" => "3 + 4"})
      assert html =~ "Not authorized"
      assert Process.alive?(view.pid)
    end

    test "an Observer can browse classes — browse is :read (BT-2491)", %{conn: conn} do
      # Browse ops are :read capability (ADR 0091 Decision 4), so an Observer who
      # cannot eval can still drive the System Browser: the pane renders and a
      # crafted browse event is authorized (NOT refused like the eval above).
      {:ok, view, html} = live(observer_conn(conn), "/")
      assert html =~ "read-only (Observer)"
      assert html =~ ~s(id="system-browser")
      # The class-tree rows (each a `browser_select_class` div) load from the
      # off-socket mount read (BT-2591) — await the async load before asserting.
      assert render_async(view) =~ ~s(phx-click="browser_select_class")

      # A class-tree click from an Observer re-renders without a "Not authorized"
      # refusal — browse is a read op, so the read-only role may browse.
      selected = render_hook(view, "browser_select_class", %{"class" => "Object"})
      refute selected =~ "Not authorized"
      assert Process.alive?(view.pid)
    end

    test "the New Class affordance is hidden for an Observer (BT-2293)", %{conn: conn} do
      # `new_class` is an :execute op, so the System Browser's "New Class"
      # affordance is owner-gated in the template (same as the eval form) — an
      # Observer must see neither the ＋ toggle nor the form it reveals.
      {:ok, _view, html} = live(observer_conn(conn), "/")
      assert html =~ "read-only (Observer)"
      refute html =~ ~s(phx-click="toggle_new_class")
      refute html =~ ~s(id="new-class-form")
      refute html =~ ~s(phx-submit="new_class")
    end

    test "the Tests pane is read-only for an Observer: catalogue listed, Run hidden, run refused (BT-2557)",
         %{conn: conn} do
      {:ok, view, _html} = live(observer_conn(conn), "/")

      # Discovery (`list_tests`) is :read — opening the Tests tab lists the
      # catalogue without an authorization refusal.
      listed = render_hook(view, "dock_tab", %{"tab" => "tests"})
      refute listed =~ "Not authorized"

      # Running (`run_tests`) is :execute — the Run controls are owner-gated away,
      # and a crafted run event is refused by RBAC (BT-2421), not crashed. BT-2597:
      # the run dispatches off-socket (`start_async`), so the RBAC refusal lands
      # via `handle_async` — await it with `render_async/2` before asserting.
      refute listed =~ ~s(phx-click="run_tests")
      render_hook(view, "run_tests", %{})
      refused = render_async(view, 30_000)
      assert refused =~ "Not authorized"
      assert Process.alive?(view.pid)
    end
  end

  describe "Test-runner pane (BT-2557)" do
    test "discovers a defined TestCase, runs it, and shows per-case pass/fail", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # Define a TestCase subclass in the live image with one passing + one
      # failing test, through the same eval path the Workspace uses.
      view
      |> form("#eval-form")
      |> render_submit(%{
        expr:
          "TestCase subclass: CockpitRunnerDemoTest\n" <>
            "  testPasses =>\n    self assert: 1 equals: 1\n" <>
            "  testFails =>\n    self assert: 1 equals: 2"
      })

      # Opening the Tests tab discovers the live catalogue — the new class appears.
      listed = render_hook(view, "dock_tab", %{"tab" => "tests"})
      assert listed =~ "CockpitRunnerDemoTest"

      # Run all: the result summary + per-case rows render, with the failing case
      # and its detail surfaced (run-all → test-all op, live session, no CLI).
      # BT-2597: the run is off-socket (`start_async(:test_op, …)`), so the hook
      # returns before results land — `render_async/2` awaits the async task.
      render_hook(view, "run_tests", %{})
      ran = render_async(view, 30_000)
      assert ran =~ "testPasses"
      assert ran =~ "testFails"
      assert ran =~ "failed"
      assert Process.alive?(view.pid)
    end

    test "run-selection runs a single class", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      view
      |> form("#eval-form")
      |> render_submit(%{
        expr:
          "TestCase subclass: CockpitRunnerSelectTest\n" <>
            "  testOk =>\n    self assert: true"
      })

      render_hook(view, "dock_tab", %{"tab" => "tests"})
      # Run just this class (the row's "run" button → run_test_class). BT-2597:
      # off-socket async, so await the task before asserting on the result.
      render_hook(view, "run_test_class", %{"class" => "CockpitRunnerSelectTest"})
      ran = render_async(view, 30_000)
      assert ran =~ "testOk"
      assert ran =~ "passed"
      assert Process.alive?(view.pid)
    end
  end

  test "eval round-trip renders the workspace result term", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    view |> form("#eval-form") |> render_submit(%{expr: "3 + 4"})
    # Scope the assertion to the transient eval-status value element (BT-2542),
    # NOT the whole page: the connected shell renders a session id
    # (`phoenix-<integer>`) whose digits would let a bare `html =~ "7"` pass even
    # when eval is completely broken and an error pane shows. The status span is
    # the real signal — it must carry the computed `7` and there must be no error
    # pane (BT-2496). The full result also lands inline in the buffer via the
    # `ws_insert_result` push (asserted separately below).
    assert view |> element(".eval-status .val") |> render() =~ "7"
    refute render(view) =~ "Empty expression"
  end

  test "Print it pushes the result for inline insertion into the buffer (BT-2542)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    # Print it (the default action) inserts its result inline in the Workspace
    # editor. The insert is a client-side CodeMirror edit driven by a server
    # push, so a server-side render can't observe the buffer — assert the push
    # instead (the browser e2e covers the actual inline DOM + collapse).
    view |> form("#eval-form") |> render_submit(%{expr: "3 + 4", action: "print_it"})
    assert_push_event(view, "ws_insert_result", %{text: text})
    assert text =~ "7"
  end

  test "Print it echoes the selection anchor for inline placement (BT-2542)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    # Track a Workspace selection, then Print it: the inline-result push echoes
    # the selection's end offset so the client anchors the widget after the
    # evaluated region even if the cursor moves during the eval round-trip
    # (wider over a remote distribution node). With no selection the anchor is
    # nil and the client falls back to the live buffer end.
    render_hook(view, "select_workspace", %{"text" => "3 + 4", "start" => 2, "end" => 7})
    view |> form("#eval-form") |> render_submit(%{expr: "unused-buffer", action: "print_it"})
    assert_push_event(view, "ws_insert_result", %{text: text, anchor: 7})
    assert text =~ "7"
  end

  test "Do it shows a terse status, no result value (BT-2542)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    # Do it evaluates for side effects only: a subtle `✓ evaluated` status, no
    # result term (ambient output streams to the Transcript instead) and no
    # inline buffer insert.
    view |> form("#eval-form") |> render_submit(%{expr: "3 + 4", action: "do_it"})
    status = view |> element(".eval-status .val") |> render()
    assert status =~ "✓ evaluated"
    refute status =~ "7"
  end

  test "eval state persists across evals within a session", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")

    # Bind a variable, then read it back in a later eval on the same session.
    view |> form("#eval-form") |> render_submit(%{expr: "x := 21 * 2"})
    view |> form("#eval-form") |> render_submit(%{expr: "x"})
    # Status-scoped (see "eval round-trip" above) so a session-id digit can't
    # spuriously satisfy the assertion when eval is broken.
    assert view |> element(".eval-status .val") |> render() =~ "42"
  end

  test "Transcript output streams live into the LiveView", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    marker = "hello-#{System.unique_integer([:positive])}"
    view |> form("#eval-form") |> render_submit(%{expr: ~s|Transcript show: "#{marker}"|})

    # The push is delivered asynchronously over distribution; poll the render.
    assert eventually(fn -> render(view) =~ marker end)
  end

  test "bindings pane reflects an eval that defines a binding (BT-2408)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    name = "bt2408_#{System.unique_integer([:positive])}"

    # Defining a binding fires the BT-2399 `bindings` push; the pane re-reads the
    # read-surface and should list the new name and its value live.
    view |> form("#eval-form") |> render_submit(%{expr: "#{name} := 123"})

    assert eventually(fn ->
             html = render(view)
             html =~ name and html =~ "123"
           end)
  end

  test "inspecting an object binding renders its live fields (BT-2408)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    suffix = System.unique_integer([:positive])
    class = "Counter#{suffix}"
    name = "counter_#{suffix}"

    # Define a real Actor subclass with an instance field, then spawn + bind it.
    # The whole class source is one eval (newlines and all). Over distribution the
    # binding is a live {:beamtalk_object, …} handle, not a flattened string, so
    # the bindings pane offers an Inspect button.
    class_src = """
    Actor subclass: #{class}
      state: count = 7

      count => self.count
    """

    view |> form("#eval-form") |> render_submit(%{expr: class_src})
    view |> form("#eval-form") |> render_submit(%{expr: "#{name} := #{class} spawn"})

    assert eventually(fn -> render(view) =~ name end)

    # Follow the reference through the read-surface `inspect` op; the Inspector
    # reads the live actor state and renders its structured instance fields
    # (the `count` field, initialised to 7).
    html = view |> element("button[phx-value-name='#{name}']") |> render_click()

    assert html =~ "Inspecting"
    assert html =~ "count"
    assert html =~ "7"
  end

  test "method editor: save a method, then an eval observes the new behaviour (BT-2409)", %{
    conn: conn
  } do
    {:ok, view, _html} = live(conn, "/")
    suffix = System.unique_integer([:positive])
    class = "EditCounter#{suffix}"

    # Define a real Actor subclass with a value field and a getter returning it.
    class_src = """
    Actor subclass: #{class}
      state: value = 1

      value => self.value
    """

    view |> form("#eval-form") |> render_submit(%{expr: class_src})

    # Sanity: the original method returns the initial field value.
    name = "ec_#{suffix}"
    view |> form("#eval-form") |> render_submit(%{expr: "#{name} := #{class} spawn"})
    html = view |> form("#eval-form") |> render_submit(%{expr: "#{name} value"})
    assert html =~ "1"

    # Save a NEW body for `value` via the write-surface method editor (ADR 0082).
    # The body is passed as a String value end-to-end — no eval-string escaping.
    save_html =
      view
      |> form("form[phx-submit='save_method']")
      |> render_submit(%{
        "class" => class,
        "selector" => "value",
        "source" => "value => self.value + 100"
      })

    assert save_html =~ "Saved value on #{class}"
    # ChangeLog coherence: the saved (class, selector) appears in the changes pane.
    assert save_html =~ class
    assert save_html =~ "value"

    # A subsequent eval on a freshly-spawned actor observes the patched behaviour
    # (compiled + flushed into the live BEAM module on the workspace node).
    name2 = "ec2_#{suffix}"
    view |> form("#eval-form") |> render_submit(%{expr: "#{name2} := #{class} spawn"})

    assert eventually(fn ->
             html = view |> form("#eval-form") |> render_submit(%{expr: "#{name2} value"})
             html =~ "101"
           end)
  end

  test "method editor: an invalid edit renders a structured error (BT-2409)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    suffix = System.unique_integer([:positive])
    class = "BadEdit#{suffix}"

    class_src = """
    Actor subclass: #{class}
      state: value = 0

      value => self.value
    """

    view |> form("#eval-form") |> render_submit(%{expr: class_src})

    # A syntactically broken body fails to compile; the write-surface returns a
    # structured #beamtalk_error{} which the LiveView renders as an actionable
    # message (NOT a flattened internal tuple/string).
    html =
      view
      |> form("form[phx-submit='save_method']")
      |> render_submit(%{
        "class" => class,
        "selector" => "value",
        "source" => "value => self.value +"
      })

    # The save-result success line must NOT appear; an error message must.
    refute html =~ "Saved value on #{class}"
    # Rendered via render_error/1 — a human-readable message, not a raw {error, …}.
    refute html =~ "{:error,"
    assert html =~ "Could not compile" or html =~ "compile" or html =~ "error"
  end

  test "method editor validates an empty class before any save (BT-2409)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")

    html =
      view
      |> form("form[phx-submit='save_method']")
      |> render_submit(%{"class" => "", "selector" => "value", "source" => "value => 1"})

    assert html =~ "Enter a class name"
  end

  # ── Phase 5 save/flush affordances: New File + ChangeLog revert (BT-2293) ────

  test "the ChangeLog viewer's revert button round-trips through the workspace (BT-2293)", %{
    conn: conn
  } do
    {:ok, view, _html} = live(conn, "/")
    suffix = System.unique_integer([:positive])
    class = "RevertMe#{suffix}"

    class_src = """
    Actor subclass: #{class}
      greeting => "ORIG"
    """

    view |> form("#eval-form") |> render_submit(%{expr: class_src})

    # Patch `greeting` via the write-surface editor — records a pending durable
    # ChangeEntry for (class, greeting), so a revert button appears for it.
    save_html =
      view
      |> form("form[phx-submit='save_method']")
      |> render_submit(%{
        "class" => class,
        "selector" => "greeting",
        "source" => ~s|greeting => "PATCHED"|
      })

    assert save_html =~ "Saved greeting on #{class}"

    # Click the per-entry revert affordance (ADR 0082 Phase 5). Scope the selector
    # by class: the workspace ChangeLog is global + persistent, so entries from
    # earlier runs share the `greeting` selector — only the per-test class name
    # disambiguates this entry's button.
    revert_html =
      view
      |> element(
        ~s(button[phx-click="revert"][phx-value-class="#{class}"][phx-value-selector="greeting"])
      )
      |> render_click()

    # The point under test is the end-to-end UI binding: the button dispatches the
    # owner-gated `:revert` op, the workspace's `revert_method/2` runs, and the
    # LiveView renders the **structured** result inline (never a raw error tuple)
    # while staying live.
    #
    # This e2e patches an *eval-defined* class, which has no prior-body snapshot in
    # the workspace's `changes/sources/` dir, so `revert_method/2` deterministically
    # returns the structured ChangeLog explanation (message prefixed `revert:`)
    # rather than restoring a body — we assert that exact outcome rather than an
    # `or` over both branches, which would silently pass on the never-taken success
    # arm. The body-restore success path is covered at the domain layer by the
    # changelog suite (`find_revert_target_returns_prev_body`); the LiveView success
    # branch of `revert_change/3` is the same `assign(save_result: …) |>
    # assign_changes()` shape as the (covered) `save_method` success path.
    assert revert_html =~ "revert:"
    refute revert_html =~ "beamtalk_error"
    refute revert_html =~ "{:error"

    # The LiveView is still live and interactive after the revert (no crash /
    # disconnect): a follow-up eval still round-trips.
    assert view |> form("#eval-form") |> render_submit(%{expr: "6 * 7"}) =~ "42"
  end

  test "the New Class toggle is present on the owner's connected render (BT-2293)", %{conn: conn} do
    {:ok, view, html} = live(conn, "/")

    # The System Browser's "New Class" affordance is collapsed by default (it must
    # not crowd the class tree): only the ＋ toggle shows on the connected render.
    assert html =~ ~s(phx-click="toggle_new_class")
    refute html =~ ~s(id="new-class-form")

    # Toggling it open reveals the source-only form (no path field — the `.bt`
    # path is derived from the declared class name server-side).
    opened = view |> element(~s(button[phx-click="toggle_new_class"])) |> render_click()
    assert opened =~ ~s(id="new-class-form")
    assert opened =~ ~s(phx-submit="new_class")
    assert opened =~ ~s(name="source")
    refute opened =~ ~s(name="path")
  end

  test "New Class validates an empty source before any create (BT-2293)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    view |> element(~s(button[phx-click="toggle_new_class"])) |> render_click()

    html =
      view
      |> form("#new-class-form")
      |> render_submit(%{"source" => ""})

    assert html =~ "Enter a class definition"
  end

  test "New Class rejects source with no class header (BT-2293)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    view |> element(~s(button[phx-click="toggle_new_class"])) |> render_click()

    # No `… subclass: Name` header → there is no class name to derive a path from,
    # so the LiveView surfaces a friendly hint rather than dispatching a doomed
    # round-trip.
    html =
      view
      |> form("#new-class-form")
      |> render_submit(%{"source" => "1 + 1"})

    # Match a non-apostrophe substring of "Couldn't find a class name …": the
    # rendered HTML escapes the apostrophe (`Couldn&#39;t`), so asserting the
    # literal contraction would spuriously fail.
    assert html =~ "find a class name"
  end

  test "New Class creates a class end-to-end via newClass:at: (BT-2293)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    view |> element(~s(button[phx-click="toggle_new_class"])) |> render_click()
    suffix = System.unique_integer([:positive])
    class = "Greeter#{suffix}"
    # The path is derived from the declared class name: `Greeter12` → its exact
    # PascalCase basename `src/Greeter12.bt` (the stdlib convention the runtime's
    # `newClass:at:` validation accepts as an exact match).
    path = "src/#{class}.bt"

    # Drive a real `newClass:at:` round-trip — the success path the validation
    # tests don't reach: form (source only) → derived path → `:new_class` facade
    # op → workspace `beamtalk_repl_eval:new_class/2`. Phase 1 installs the class
    # in memory and logs a durable new-class ChangeEntry; the `.bt` file is only
    # written on flush, so this leaves no on-disk artifact. A derivation bug
    # (wrong basename/case) would fail here, not just in validation.
    html =
      view
      |> form("#new-class-form")
      |> render_submit(%{"source" => "Object subclass: #{class}"})

    assert html =~ "Created new class — #{path}"
    refute html =~ "beamtalk_error"
    refute html =~ "{:error"
    # ChangeLog coherence: the new-class entry is now in the Changes viewer.
    assert html =~ class
    # The form collapses again after a successful create.
    refute html =~ ~s(id="new-class-form")
  end

  # ── Phase 1 JS hook foundation (BT-2485, BT-2539) ───────────────────────────

  test "the editor hooks are present on the owner's connected render (BT-2485)", %{conn: conn} do
    # The method editor's CmEditor (and its `select_source` stamp) only mount once
    # a tab is open — the cockpit now starts empty. Open a def tab, then assert the
    # editor hooks are wired.
    {_view, html, _class} = open_fresh_def_tab(conn)

    # The method editor is CodeMirror (the CmEditor hook, BT-2539): ⌘S submits via
    # KeyboardShortcuts and the selection rides select_source — the CodeEditor
    # overlay + SelectionTracker hooks are retired. The Workspace pane is also
    # CmEditor (BT-2538), covered by its own test below.
    assert html =~ ~s(phx-hook="CmEditor")
    assert html =~ ~s(phx-hook="KeyboardShortcuts")
    assert html =~ ~s(data-shortcuts)
    assert html =~ ~s(data-select-event="select_source")
    # The retired overlay machinery leaves no trace in the markup.
    refute html =~ ~s(phx-hook="CodeEditor")
    refute html =~ ~s(phx-hook="SelectionTracker")
    refute html =~ "bt-editor-pre"
  end

  test "the Workspace editor renders the CmEditor hook over a hidden expr field (BT-2538)", %{
    conn: conn
  } do
    {:ok, _view, html} = live(conn, "/")

    # The Workspace editor is CodeMirror (the CmEditor hook), mounted into an
    # ignored host over a hidden `<textarea name="expr">` that stays the posted
    # form field — so the `eval` handler and render_submit(%{expr: …}) read it
    # unchanged. Selection is reported via select_workspace.
    assert html =~ ~s(phx-hook="CmEditor")
    assert html =~ ~s(id="workspace-editor-cm")
    assert html =~ ~s(phx-update="ignore")
    assert html =~ ~s(data-select-event="select_workspace")
    assert html =~ ~s(name="expr")
  end

  test "the select_source hook event is accepted and ignored when malformed (BT-2485)", %{
    conn: conn
  } do
    # A well-formed selection payload stamped with the *active* tab id is stored
    # (BT-2549: the stamp is what the guard matches against). Open a tab first —
    # the strip starts empty, so there is no active tab to stamp until one opens.
    {view, html, _class} = open_fresh_def_tab(conn)
    tab_id = active_tab_id(html)

    assert render_hook(view, "select_source", %{
             "text" => "self.value",
             "start" => 0,
             "end" => 10,
             "tab_id" => tab_id
           })

    assert edit_selection(view) == %{text: "self.value", start: 0, end: 10}

    # A malformed payload (no text key, or non-binary) is ignored, not a crash —
    # the LiveView keeps rendering, proving the defensive clause holds.
    assert render_hook(view, "select_source", %{"garbage" => true})
    assert render_hook(view, "select_source", %{"text" => 123})
  end

  test "select_source ignores a stale stamp from a departing tab (BT-2549)", %{conn: conn} do
    {view, html, _class} = open_fresh_def_tab(conn)
    active = active_tab_id(html)

    # Seed a real selection for the active tab so we can prove the stale event
    # doesn't clobber *or* overwrite it.
    render_hook(view, "select_source", %{
      "text" => "self.value",
      "start" => 0,
      "end" => 10,
      "tab_id" => active
    })

    assert edit_selection(view) == %{text: "self.value", start: 0, end: 10}

    # The race: a `select_source` the *departing* CmEditor dispatched just before
    # its `destroyed()` ran lands carrying the previous tab's id. Its stamp no
    # longer matches `active_tab`, so the handler drops it — `:edit_selection`
    # keeps the live tab's coordinates rather than stale ones from the closed tab.
    render_hook(view, "select_source", %{
      "text" => "stale.from.closed.tab",
      "start" => 99,
      "end" => 123,
      "tab_id" => active <> ":stale"
    })

    assert edit_selection(view) == %{text: "self.value", start: 0, end: 10}

    # A payload with no tab-id stamp at all is likewise ignored (defensive: a
    # client that never stamps can't poison the assign).
    render_hook(view, "select_source", %{"text" => "no.stamp", "start" => 1, "end" => 2})
    assert edit_selection(view) == %{text: "self.value", start: 0, end: 10}
  end

  # The method editor stamps its CmEditor element with the active tab id so each
  # selection push can be matched against the live tab (BT-2549).
  test "the method editor stamps its CmEditor with the active tab id (BT-2549)", %{conn: conn} do
    # The CmEditor only mounts once a tab is open (the strip starts empty).
    {_view, html, _class} = open_fresh_def_tab(conn)

    assert html =~ ~s(data-select-event="select_source")
    assert html =~ ~s(data-tab-id="#{active_tab_id(html)}")
  end

  # ── Phase 2 tabbed method editor (BT-2494) ──────────────────────────────────

  test "the method editor renders a tab strip + breadcrumb (BT-2494)", %{conn: conn} do
    {:ok, _view, mount_html} = live(conn, "/")

    # The cockpit opens with an EMPTY strip (no starter tab): the tab strip and
    # the hidden save_method form are present, but the breadcrumb and "+ def"
    # appear only once a tab is open.
    assert mount_html =~ "tabstrip"
    assert mount_html =~ ~s(phx-submit="save_method")
    assert mount_html =~ ~s(name="tab")
    assert mount_html =~ "Nothing open"
    refute mount_html =~ "+ def"

    # Opening a class definition gives a live active tab: the breadcrumb, the
    # "+ def" affordance, and the tab-select control all render.
    {_view, html, class} = open_fresh_def_tab(conn)
    assert html =~ "editor-meta"
    assert html =~ ~s(phx-click="tab_select")
    assert html =~ ~s(phx-click="open_definition")
    assert html =~ "+ def"
    # The breadcrumb shows Class › … for the active tab.
    assert html =~ class
    assert html =~ "class definition"
  end

  test "opening a class definition adds a + def tab and switches to it (BT-2494)", %{conn: conn} do
    # Open a tab first (the strip starts empty), then "+ def" opens (or re-focuses)
    # the active class's definition tab — a tab whose compile evals the class
    # definition. The tab label carries the ▸ def marker and the breadcrumb shows
    # the class-definition form.
    {view, _html, class} = open_fresh_def_tab(conn)
    html = view |> element(~s(button[phx-click="open_definition"])) |> render_click()
    assert html =~ "#{class} ▸ def"
    assert html =~ "class definition"
  end

  test "a dirty edit marks the active tab with a dirty dot (BT-2494)", %{conn: conn} do
    # Open a def tab; it opens clean (no dirty dot), reporting "in image".
    {view, _html, class} = open_fresh_def_tab(conn)
    assert render(view) =~ "in image"
    refute render(view) =~ "modot"

    # Editing the source via the form's phx-change marks the active tab dirty —
    # the dirty dot (.modot) appears in the tab strip and the meta-note flips to
    # "edited". This is pure view state (no workspace round-trip).
    edited =
      view
      |> form("#method-editor-form")
      |> render_change(%{"source" => "Actor subclass: #{class}\n  state: x = 1"})

    assert edited =~ "modot"
    assert edited =~ "edited"
  end

  test "the tab edit-source handler ignores a malformed payload (BT-2494)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")

    # A crafted edit_source event with no/non-binary source must not crash the
    # LiveView — the defensive clause keeps it rendering.
    assert render_hook(view, "edit_source", %{"garbage" => true})
    assert render_hook(view, "edit_source", %{"source" => 123})
    assert Process.alive?(view.pid)
  end

  test "the Tweaks panel and its controls render on the connected shell (BT-2487)", %{
    conn: conn
  } do
    {:ok, _view, html} = live(conn, "/")

    # The panel is hooked client-side (TweaksPanel) and carries the first-run
    # defaults the hook restores before a localStorage choice exists.
    assert html =~ ~s(id="tweaks-panel")
    assert html =~ ~s(phx-hook="TweaksPanel")
    assert html =~ "data-tweaks-defaults"

    # Each control declares the tweak it drives via data-tweak; the hook maps it
    # to a :root CSS variable (theme/accent/syntax/density/uiFont/codeFont).
    assert html =~ ~s(data-tweak="theme")
    assert html =~ ~s(data-tweak="accent")
    assert html =~ ~s(data-tweak="syntax")
    assert html =~ ~s(data-tweak="density")
    assert html =~ ~s(data-tweak="uiFont")
    assert html =~ ~s(data-tweak="codeFont")

    # The curated option sets match the spike: three themes, the warm/mono/vivid
    # syntax modes, and the accent swatches.
    assert html =~ ~s(data-tweak-value="paper")
    assert html =~ ~s(data-tweak-value="squeak")
    assert html =~ ~s(data-tweak-value="dusk")
    assert html =~ ~s(data-tweak-value="warm")
    assert html =~ ~s(data-tweak-value="mono")
    assert html =~ ~s(data-tweak-value="vivid")
    assert html =~ ~s(data-tweak-value="#b9711b")
  end

  # ── Phase 2 System Browser pane (BT-2491) ───────────────────────────────────

  test "the System Browser renders the class tree with view + side toggles (BT-2491)", %{
    conn: conn
  } do
    {:ok, view, html} = live(conn, "/")

    # The left column is the spike's System Browser: a Hierarchy / Category view
    # toggle, a class tree, an instance/class side toggle, and a protocol/method
    # pane — driven by the BT-2488 browse ops (ADR 0096). The placeholder copy is
    # gone.
    assert html =~ ~s(id="system-browser")
    assert html =~ ~s(phx-click="browser_view")
    assert html =~ ~s(phx-value-view="hierarchy")
    assert html =~ ~s(phx-value-view="category")
    assert html =~ ~s(phx-click="browser_side")
    assert html =~ ~s(phx-value-side="instance")
    assert html =~ ~s(phx-value-side="class")
    refute html =~ "Lands in a later Phase 1 issue."

    # The class tree (each row a `browser_select_class` div) is populated from live
    # browse-classes, now read off-socket at mount (BT-2591) — await the async
    # mount load, then the rows render and a core class like Object is in the image.
    awaited = render_async(view)
    assert awaited =~ ~s(phx-click="browser_select_class")
    assert awaited =~ "Object"
  end

  test "selecting a class loads its protocols and methods (BT-2491)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    suffix = System.unique_integer([:positive])
    class = "BrowseCounter#{suffix}"

    # Define a real Actor subclass with two instance methods so the protocol +
    # method list has something to render.
    class_src = """
    Actor subclass: #{class}
      state: value = 0

      increment => self.value := self.value + 1

      value => self.value
    """

    view |> form("#eval-form") |> render_submit(%{expr: class_src})

    # browse-classes is a live snapshot; re-render the LiveView so the new class
    # appears in the tree, then click it to drive browse-protocols.
    {:ok, view, _html} = live(conn, "/")
    assert eventually(fn -> render(view) =~ class end)

    html =
      view
      |> element(~s(div[phx-value-class="#{class}"]))
      |> render_click()

    # The method list now shows the class's instance selectors and the protocol
    # filter row's "all" affordance.
    assert html =~ "increment"
    assert html =~ "value"
    assert html =~ ">all<"
    assert html =~ ~s(phx-click="browser_select_protocol")

    # The method pane leads with a "class definition" entry so the class shape is
    # browsable, not only its methods — clicking it opens the def tab (BT-2491).
    assert html =~ "class definition"
    assert html =~ ~s(phx-click="browser_open_definition")

    opened =
      view
      |> element(~s(div[phx-click="browser_open_definition"][phx-value-class="#{class}"]))
      |> render_click()

    # A class-definition tab is now open and focused for the selected class — its
    # tab label reads "<Class> ▸ def" (the def-tab marker, which only exists once
    # a def tab is open), parity with the "+ def" path.
    assert opened =~ "#{class} ▸ def"
  end

  test "the instance/class side toggle re-populates the method list (BT-2491)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    suffix = System.unique_integer([:positive])
    class = "SideCounter#{suffix}"

    # An instance method, then a class-side method added via the `>>` extension
    # form (docs/beamtalk-language-features.md "Class-side live edit"), so the two
    # sides differ.
    class_src = """
    Actor subclass: #{class}
      state: value = 0

      instanceOnly => self.value
    """

    view |> form("#eval-form") |> render_submit(%{expr: class_src})
    view |> form("#eval-form") |> render_submit(%{expr: "#{class} class >> classOnly => 42"})

    {:ok, view, _html} = live(conn, "/")
    assert eventually(fn -> render(view) =~ class end)

    # Instance side shows the instance method.
    inst = view |> element(~s(div[phx-value-class="#{class}"])) |> render_click()
    assert inst =~ "instanceOnly"

    # Flip to the class side: the method list re-populates from browse-protocols
    # for the class side (the class-only method appears).
    cls = view |> element(~s(button[phx-value-side="class"])) |> render_click()
    assert cls =~ "classOnly"
  end

  test "the protocol filter narrows the method list; 'all' shows everything (BT-2491)", %{
    conn: conn
  } do
    {:ok, view, _html} = live(conn, "/")
    suffix = System.unique_integer([:positive])
    class = "FilterCounter#{suffix}"

    class_src = """
    Actor subclass: #{class}
      state: value = 0

      increment => self.value := self.value + 1

      value => self.value
    """

    view |> form("#eval-form") |> render_submit(%{expr: class_src})

    {:ok, view, _html} = live(conn, "/")
    assert eventually(fn -> render(view) =~ class end)
    view |> element(~s(div[phx-value-class="#{class}"])) |> render_click()

    # "all" (the default filter) shows every selector.
    all = render(view)
    assert all =~ "increment"
    assert all =~ "value"

    # The handler accepts the protocol filter event and stays alive; an empty
    # value resets to "all" (the ∗ row), proving the filter round-trips.
    assert render_hook(view, "browser_select_protocol", %{"protocol" => ""})
    assert Process.alive?(view.pid)
  end

  test "selecting a method opens it in the editor with a breadcrumb (BT-2491)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    suffix = System.unique_integer([:positive])
    class = "SourceCounter#{suffix}"

    class_src = """
    Actor subclass: #{class}
      state: value = 0

      increment => self.value := self.value + 1
    """

    view |> form("#eval-form") |> render_submit(%{expr: class_src})

    {:ok, view, _html} = live(conn, "/")
    assert eventually(fn -> render(view) =~ class end)
    view |> element(~s(div[phx-value-class="#{class}"])) |> render_click()

    # Click the method row: it opens an editable tab in the method editor seeded
    # with the method source, and a `Class › side › selector` breadcrumb.
    html =
      view
      |> element(~s(div[phx-value-selector="increment"]))
      |> render_click()

    # A new editor tab is opened for the selected method — its stable tab id
    # (`method:Class:side:selector`) proves the click actually created the tab,
    # not just that the always-present `#method-editor` panel rendered.
    assert html =~ ~s(phx-value-id="method:#{class}:instance:increment")
    assert html =~ class
    assert html =~ "increment"
    # The editor breadcrumb carries the instance side and the › separator.
    assert html =~ "›"
    assert html =~ "instance"
    # The method body is loaded into the editable buffer.
    assert html =~ "self.value"
    # Regression guard: the old read-only Method Source pane is gone — browsing a
    # method must route into the editor, not resurrect the separate pane.
    refute html =~ ~s(id="browse-method-source")
  end

  test "compiling a runtime-only browsed method shows runtime, not unflushed (BT-2539 / BT-2550)",
       %{conn: conn} do
    # An in-memory `>>` compile (⌘S) live-patches the method without flushing to
    # disk. This class is eval-defined, so it is *runtime-only* (no on-disk body):
    # there is no disk counterpart for the image to "differ" from, so BT-2550
    # suppresses the misleading `unflushed` badge and the breadcrumb carries the ⚡
    # runtime badge instead. (The `unflushed`-on-compile path for a *disk-backed*
    # method is covered by `workspace_flush_badge_test.exs`.)
    {:ok, view, _html} = live(conn, "/")
    suffix = System.unique_integer([:positive])
    class = "FlushCounter#{suffix}"

    class_src = """
    Actor subclass: #{class}
      state: value = 0

      increment => self.value := self.value + 1
    """

    view |> form("#eval-form") |> render_submit(%{expr: class_src})

    {:ok, view, _html} = live(conn, "/")
    assert eventually(fn -> render(view) =~ class end)
    view |> element(~s(div[phx-value-class="#{class}"])) |> render_click()

    # Open the method as an editable tab (browse-is-edit): now the active tab.
    view |> element(~s(div[phx-value-selector="increment"])) |> render_click()

    # Compile a new body via the save form, threading the active method tab id so
    # `compile_clean/3` runs against it (the historical no-tab payload is a no-op).
    save_html =
      view
      |> form("form[phx-submit='save_method']")
      |> render_submit(%{
        "class" => class,
        "selector" => "increment",
        "source" => "increment => self.value := self.value + 2",
        "tab" => "method:#{class}:instance:increment"
      })

    assert save_html =~ "Saved increment on #{class}"
    # Runtime-only: the breadcrumb shows the ⚡ runtime badge, never `unflushed`.
    assert save_html =~ "runtime-only (no source on disk)"
    refute save_html =~ "unflushed"
  end

  # The BT-2545 *flush clears the unflushed badge* behaviour is covered by the pure
  # reconcile unit tests in `workspace_live_reconcile_test.exs` and the full
  # integration test in `workspace_flush_badge_test.exs` (BT-2554) which uses a
  # fully-stubbed workspace client. The `compile → unflushed shown` half (a
  # disk-backed method compiled to a *different* body) lives there too — the live
  # test above is runtime-only, so BT-2550 suppresses its `unflushed` badge.

  test "re-clicking an open method row re-fetches without blanking the buffer (BT-2547)", %{
    conn: conn
  } do
    # Re-activating an already-open, clean method tab routes through the
    # `%{} = existing` branch of `open_method_tab/4`, which re-fetches the live image.
    # The refreshed buffer must still hold the method source — a re-browse must never
    # blank a tab the user is looking at.
    {:ok, view, _html} = live(conn, "/")
    suffix = System.unique_integer([:positive])
    class = "RebrowseCounter#{suffix}"

    class_src = """
    Actor subclass: #{class}
      state: value = 0

      increment => self.value := self.value + 1
    """

    view |> form("#eval-form") |> render_submit(%{expr: class_src})

    {:ok, view, _html} = live(conn, "/")
    assert eventually(fn -> render(view) =~ class end)
    view |> element(~s(div[phx-value-class="#{class}"])) |> render_click()

    # First click opens the tab seeded with the method body.
    open_html =
      view
      |> element(~s(div[phx-value-selector="increment"]))
      |> render_click()

    assert open_html =~ "self.value"

    # Re-click the same row: the existing clean tab is re-activated and re-fetched. The
    # body must still be present (not clobbered with an empty buffer).
    rebrowse_html =
      view
      |> element(~s(div[phx-value-selector="increment"]))
      |> render_click()

    assert rebrowse_html =~ "self.value"
    assert rebrowse_html =~ ~s(phx-value-id="method:#{class}:instance:increment")
  end

  # ── Phase 3 Inspector live tracking (BT-2492, backend BT-2489) ──────────────

  # Spawn + bind a real Counter actor in `view`'s session and inspect it, returning
  # the binding name. The Counter exposes `value` (a field) and `increment` (a
  # mutator) so the change stream + field flash + poke have something to move.
  defp inspect_live_counter(view) do
    suffix = System.unique_integer([:positive])
    class = "TrackCounter#{suffix}"
    name = "tc_#{suffix}"

    class_src = """
    Actor subclass: #{class}
      state: value = 0

      value => self.value

      increment => self.value := self.value + 1
    """

    view |> form("#eval-form") |> render_submit(%{expr: class_src})
    view |> form("#eval-form") |> render_submit(%{expr: "#{name} := #{class} spawn"})
    assert eventually(fn -> render(view) =~ name end)

    view |> element("button[phx-value-name='#{name}']") |> render_click()
    # Assert on the full page render, not the click's element-scoped return.
    assert render(view) =~ "Inspecting"
    {name, class}
  end

  test "inspecting an actor renders pid-stats chips from the pid_stats op (BT-2492)", %{
    conn: conn
  } do
    {:ok, view, _html} = live(conn, "/")
    {_name, _class} = inspect_live_counter(view)

    # The Inspector head carries the live process-health chips read via pid_stats:
    # a scheduling status, the mailbox depth, and a reduction count. They are the
    # spike's process-health line, refreshed on every change push.
    html = render(view)
    assert html =~ "pid-stat"
    assert html =~ "mailbox"
    assert html =~ "reductions"
    # The FieldFlash hook + freeze toggle are wired onto the connected render.
    assert html =~ ~s(phx-hook="FieldFlash")
    assert html =~ ~s(phx-click="freeze_toggle")
    assert html =~ "data-flash-gen"
  end

  # BT-2524: a committed state write on a watched actor now pushes
  # {:object_changed, …} from the workspace node to the LiveView Inspector, so
  # flash-gen bumps and the changed field re-reads. The push fires because the
  # compiled actor's generated handle_call/handle_cast call
  # beamtalk_actor:notify_state_change/2 after committing new state (previously
  # only the runtime beamtalk_actor dispatch path did, via log_dispatch_complete).
  test "a committed state write flashes the changed field and bumps flash-gen (BT-2492)", %{
    conn: conn
  } do
    {:ok, view, _html} = live(conn, "/")
    {name, _class} = inspect_live_counter(view)

    gen_before = flash_gen(view)

    # Mutate the inspected actor: `increment` commits a state write, which the
    # workspace pushes as {:object_changed, …}. The pane coalesces + re-reads,
    # bumping data-flash-gen and re-rendering the new value (1). The FieldFlash JS
    # hook turns the gen bump into a visual pulse (covered by the Playwright case);
    # here we assert the server-side signal the hook keys off.
    view |> form("#eval-form") |> render_submit(%{expr: "#{name} increment"})

    assert eventually(fn ->
             html = render(view)
             flash_gen(view) > gen_before and html =~ "value" and html =~ "1"
           end)
  end

  test "the freeze toggle stops live tracking and resumes on unfreeze (BT-2492)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    {name, _class} = inspect_live_counter(view)

    # Freeze: the head button flips to "frozen" and the change subscription is
    # dropped, so a subsequent write does NOT bump flash-gen (the pane holds a
    # snapshot).
    frozen = view |> element(~s(button[phx-click="freeze_toggle"])) |> render_click()
    assert frozen =~ "frozen"

    gen_frozen = flash_gen(view)
    view |> form("#eval-form") |> render_submit(%{expr: "#{name} increment"})
    # Give any (incorrectly-still-subscribed) push time to arrive; the gen must NOT
    # move while frozen.
    Process.sleep(300)
    assert flash_gen(view) == gen_frozen

    # Unfreeze: re-subscribe + catch up — the pane re-reads, so it now reflects the
    # single increment made while frozen (value 1) and tracking is live again.
    live_again = view |> element(~s(button[phx-click="freeze_toggle"])) |> render_click()
    assert live_again =~ "live"
    assert render(view) =~ "1"
  end

  test "owner poke sends a message to the inspected actor (BT-2492)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    {_name, _class} = inspect_live_counter(view)

    # The owner-only poke bar sends a Beamtalk message to the actor by eval'ing
    # `<binding> <message>`. Sending `increment` mutates the actor; the result
    # confirmation renders and the change stream re-reads the new value.
    poked = view |> form(".poke form") |> render_submit(%{"message" => "increment"})
    refute poked =~ "Can only send"
    refute poked =~ "Not authorized"

    assert eventually(fn -> render(view) =~ "value" and render(view) =~ "1" end)
  end

  test "poke validates an empty message and a non-bound target (BT-2492)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    {_name, _class} = inspect_live_counter(view)

    # An empty message is a local validation error (no eval round-trip).
    empty = render_hook(view, "poke", %{"message" => "  "})
    assert empty =~ "Enter a message"
    assert Process.alive?(view.pid)

    # A malformed payload (no/non-binary message) is ignored, not a crash.
    assert render_hook(view, "poke", %{"garbage" => true})
    assert render_hook(view, "poke", %{"message" => 123})
    assert Process.alive?(view.pid)
  end

  test "a change push for an unwatched pid is ignored (no spurious refresh) (BT-2492)", %{
    conn: conn
  } do
    {:ok, view, _html} = live(conn, "/")
    {_name, _class} = inspect_live_counter(view)

    # Drive the {:object_changed, …} clause directly with a pid that is NOT the
    # watched actor (the test process). The pid-match guard must drop it: no
    # flash-gen bump, no crash. This isolates the BT-2492 stale-push guard from
    # the actor runtime's timing.
    gen_before = flash_gen(view)
    send(view.pid, {:object_changed, self(), []})
    # Let the message be processed (a render flushes the mailbox).
    _ = render(view)
    assert flash_gen(view) == gen_before
    assert Process.alive?(view.pid)
  end

  test "a frozen pane drops a change push for the watched pid (BT-2492)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    {name, _class} = inspect_live_counter(view)

    # Freeze, then a real write to the watched actor. The frozen guard in the
    # {:object_changed, …} clause must drop the push — flash-gen stays put even
    # though the actor genuinely changed (this asserts the guard, not just timing).
    view |> element(~s(button[phx-click="freeze_toggle"])) |> render_click()
    gen_frozen = flash_gen(view)

    view |> form("#eval-form") |> render_submit(%{expr: "#{name} increment"})
    # The write committed (the pane is frozen, so it must NOT reflect it). Poll a
    # few times to let any push land, asserting the gen never moves.
    refute eventually(fn -> flash_gen(view) != gen_frozen end, 10)
    assert Process.alive?(view.pid)
  end

  # ── Phase 3: floating inspector windows / overlay mode (BT-2493) ─────────────
  #
  # The window-list lifecycle — open / drill / close / focus (z-order) / mode
  # toggle — is server-side LiveView state, fully exercisable through
  # `Phoenix.LiveViewTest`: we drive the events the WindowDrag hook pushes and
  # assert the resulting render. (The drag *motion* + click-to-front interaction
  # need a real browser and are covered by the Playwright lane.) Window ids are
  # minted deterministically per session from 1, so a freshly-opened window is
  # always `#inspector-window-win-1`, the next `win-2`, … — which lets these
  # assert on the rendered DOM rather than poking the LiveView's internal state.

  # Spawn + bind a live Counter actor in `view`'s session WITHOUT inspecting it,
  # returning its binding name. In Float mode the inspect click opens a window
  # rather than the docked pane, so the window tests open windows themselves.
  defp spawn_counter(view) do
    suffix = System.unique_integer([:positive])
    class = "WinCounter#{suffix}"
    name = "wc_#{suffix}"

    class_src = """
    Actor subclass: #{class}
      state: value = 0

      value => self.value

      increment => self.value := self.value + 1
    """

    view |> form("#eval-form") |> render_submit(%{expr: class_src})
    view |> form("#eval-form") |> render_submit(%{expr: "#{name} := #{class} spawn"})
    assert eventually(fn -> render(view) =~ name end)
    name
  end

  # Put `view` in Float mode and open a floating window on binding `name`. Returns
  # the deterministic window id of the just-opened window.
  defp open_float_window(view, name) do
    render_hook(view, "set_inspector_mode", %{"mode" => "float"})
    view |> element("button[phx-value-name='#{name}']") |> render_click()
    # The most-recently-opened window is the last `inspector-window-win-N` in the
    # render; derive its id so the test can address it without internal state.
    ids = Regex.scan(~r/inspector-window-(win-\d+)"/, render(view))
    assert ids != [], "expected a floating window to be open"
    ids |> List.last() |> Enum.at(1)
  end

  test "the Dock/Float toggle flips inspector mode and is reflected in the render (BT-2493)", %{
    conn: conn
  } do
    {:ok, view, _html} = live(conn, "/")

    # Docked is the default — the top-bar toggle is present and no floating overlay
    # exists yet.
    html = render(view)
    assert html =~ ~s(phx-click="set_inspector_mode")
    refute html =~ ~s(id="inspector-overlay")

    # Flip to Float: the toggle marks Float selected (aria-selected on its tab). No
    # windows yet, so still no overlay until one opens.
    html = render_hook(view, "set_inspector_mode", %{"mode" => "float"})
    assert html =~ ~s(phx-value-mode="float")
    refute html =~ ~s(id="inspector-overlay")

    # A bad mode is ignored, not a crash.
    render_hook(view, "set_inspector_mode", %{"mode" => "bogus"})
    assert Process.alive?(view.pid)
  end

  test "in Float mode an inspect click opens a floating window on the binding (BT-2493)", %{
    conn: conn
  } do
    {:ok, view, _html} = live(conn, "/")
    name = spawn_counter(view)

    render_hook(view, "set_inspector_mode", %{"mode" => "float"})

    # The bindings pane's Inspect button fires "inspect"; in Float mode that opens a
    # floating window rather than driving the docked pane.
    html = view |> element("button[phx-value-name='#{name}']") |> render_click()

    assert html =~ ~s(id="inspector-overlay")
    assert html =~ ~s(id="inspector-window-win-1")
    assert html =~ "Inspecting"
    # The window's title bar carries the binding label and the window renders its
    # live field (value = 0).
    assert html =~ "iw-title"
    assert html =~ "value"
  end

  test "multiple windows target objects independently, each with its own breadcrumb (BT-2493)",
       %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    a = spawn_counter(view)
    b = spawn_counter(view)

    render_hook(view, "set_inspector_mode", %{"mode" => "float"})
    view |> element("button[phx-value-name='#{a}']") |> render_click()
    html = view |> element("button[phx-value-name='#{b}']") |> render_click()

    # Two independent windows in the overlay, with distinct deterministic ids.
    assert html =~ ~s(id="inspector-window-win-1")
    assert html =~ ~s(id="inspector-window-win-2")
    # Each window addresses its own drill/close events by its id.
    assert html =~ ~s(phx-value-id="win-1")
    assert html =~ ~s(phx-value-id="win-2")
  end

  test "drilling a floating window extends only that window's breadcrumb (BT-2493)", %{
    conn: conn
  } do
    {:ok, view, _html} = live(conn, "/")
    suffix = System.unique_integer([:positive])
    leaf = "WLeaf#{suffix}"
    boxx = "WBoxx#{suffix}"
    name = "wbox_#{suffix}"

    view
    |> form("#eval-form")
    |> render_submit(%{expr: "Actor subclass: #{leaf}\n  state: n = 99\n\n  n => self.n"})

    view
    |> form("#eval-form")
    |> render_submit(%{
      expr:
        "Actor subclass: #{boxx}\n  state: item = nil\n\n  setItem: x => self.item := x\n  item => self.item"
    })

    view |> form("#eval-form") |> render_submit(%{expr: "wleaf_#{suffix} := #{leaf} spawn"})
    view |> form("#eval-form") |> render_submit(%{expr: "#{name} := #{boxx} spawn"})
    view |> form("#eval-form") |> render_submit(%{expr: "#{name} setItem: wleaf_#{suffix}"})
    assert eventually(fn -> render(view) =~ name end)

    id = open_float_window(view, name)

    # The window shows box's `item` (a live Leaf reference). Drill into it (index 0,
    # the sole drillable row): the referenced Leaf's own field (n = 99) renders and
    # the window's breadcrumb now has two levels (a back-crumb at index 0).
    html = render_hook(view, "window_drill", %{"id" => id, "index" => "0"})
    assert html =~ "99"
    assert html =~ ~s(phx-value-index="0")

    # Walk back via the window's breadcrumb (crumb 0): back to box's `item` field.
    html = render_hook(view, "window_crumb", %{"id" => id, "index" => "0"})
    assert html =~ "item"
  end

  test "closing a floating window removes it from the overlay (BT-2493)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    name = spawn_counter(view)

    id = open_float_window(view, name)
    assert render(view) =~ ~s(id="inspector-window-#{id}")

    # Close it: the window is dropped from the list (releasing its subscription —
    # the window no longer exists to receive pushes). The overlay empties.
    html = render_hook(view, "window_close", %{"id" => id})
    refute html =~ ~s(id="inspector-window-#{id}")
    refute html =~ ~s(id="inspector-overlay")

    # Closing an unknown id is a harmless no-op.
    render_hook(view, "window_close", %{"id" => "win-does-not-exist"})
    assert Process.alive?(view.pid)
  end

  test "clicking a window brings it to the front (z-order follows focus) (BT-2493)", %{
    conn: conn
  } do
    {:ok, view, _html} = live(conn, "/")
    a = spawn_counter(view)
    b = spawn_counter(view)

    render_hook(view, "set_inspector_mode", %{"mode" => "float"})
    view |> element("button[phx-value-name='#{a}']") |> render_click()
    view |> element("button[phx-value-name='#{b}']") |> render_click()

    # The second-opened window (win-2) starts on top: a higher inline z-index.
    assert window_z(render(view), "win-2") > window_z(render(view), "win-1")

    # Focus the FIRST window: its z is bumped above the current max, so it now wins.
    render_hook(view, "window_focus", %{"id" => "win-1"})
    html = render(view)
    assert window_z(html, "win-1") > window_z(html, "win-2")
    # The DOM order is unchanged — only z moved, so a focus never reshuffles windows
    # (which would reset their drag positions): win-1 still precedes win-2.
    assert window_dom_index(html, "win-1") < window_dom_index(html, "win-2")
  end

  test "a drag-drop persists a window's position and it survives re-render (BT-2493)", %{
    conn: conn
  } do
    {:ok, view, _html} = live(conn, "/")
    name = spawn_counter(view)
    id = open_float_window(view, name)

    # The WindowDrag hook reports the final position on drop; the server persists it
    # into the window's inline left/top.
    html = render_hook(view, "window_moved", %{"id" => id, "x" => 412, "y" => 233})
    assert html =~ "left:412px"
    assert html =~ "top:233px"

    # The position survives an unrelated re-render (a bindings re-read from another
    # eval) — positions are not reset on unrelated state changes.
    view
    |> form("#eval-form")
    |> render_submit(%{expr: "unrelated_#{System.unique_integer([:positive])} := 1"})

    html = render(view)
    assert html =~ "left:412px"
    assert html =~ "top:233px"

    # A crafted non-numeric / negative payload clamps rather than crashing.
    render_hook(view, "window_moved", %{"id" => id, "x" => "bogus", "y" => -5})
    assert Process.alive?(view.pid)
  end

  test "windows persist when toggling back to Docked mode (BT-2493)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    name = spawn_counter(view)
    id = open_float_window(view, name)
    assert render(view) =~ ~s(id="inspector-window-#{id}")

    # Flipping back to Docked does NOT tear down open windows — they stay rendered
    # (and keep their subscriptions) so a misclick can't destroy a desk of
    # inspectors.
    html = render_hook(view, "set_inspector_mode", %{"mode" => "docked"})
    assert html =~ ~s(id="inspector-window-#{id}")
  end

  test "a per-window freeze toggle holds that window's snapshot (BT-2493)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    name = spawn_counter(view)
    id = open_float_window(view, name)

    # Tracking is live by default — the window's freeze button reads "live".
    assert render(view) =~ "insp-freeze live"

    # Freeze: the window drops its subscription and holds the snapshot ("frozen").
    html = render_hook(view, "window_freeze", %{"id" => id})
    assert html =~ "insp-freeze frozen"

    # Unfreeze: re-arm the watch + catch up; the button reads "live" again.
    html = render_hook(view, "window_freeze", %{"id" => id})
    assert html =~ "insp-freeze live"
  end

  test "crafted window events with bad ids/indexes are ignored, not crashes (BT-2493)", %{
    conn: conn
  } do
    {:ok, view, _html} = live(conn, "/")

    # No windows open: every window event is a safe no-op (no overlay, no crash).
    render_hook(view, "window_drill", %{"id" => "win-x", "index" => "0"})
    render_hook(view, "window_crumb", %{"id" => "win-x", "index" => "3"})
    render_hook(view, "window_focus", %{"id" => "win-x"})
    render_hook(view, "window_moved", %{"id" => "win-x", "x" => 1, "y" => 2})
    render_hook(view, "window_freeze", %{"id" => "win-x"})
    render_hook(view, "window_close", %{"id" => "win-x"})
    # Malformed payloads (missing keys) hit the catch-all clauses.
    render_hook(view, "window_drill", %{"garbage" => true})
    render_hook(view, "window_poke", %{"id" => "win-x"})
    assert Process.alive?(view.pid)
    refute render(view) =~ ~s(id="inspector-overlay")
  end

  test "two windows on the same actor are independent; closing one keeps the other (BT-2493)", %{
    conn: conn
  } do
    {:ok, view, _html} = live(conn, "/")
    name = spawn_counter(view)

    # Open TWO floating windows on the SAME binding. The workspace keys per-object
    # subscriptions by (pid, subscriber) and our subscriber is this one LiveView, so
    # both windows share the single underlying subscription — the reference-aware
    # unwatch must NOT release it while the other window still needs it.
    render_hook(view, "set_inspector_mode", %{"mode" => "float"})
    view |> element("button[phx-value-name='#{name}']") |> render_click()
    view |> element("button[phx-value-name='#{name}']") |> render_click()

    html = render(view)
    assert html =~ ~s(id="inspector-window-win-1")
    assert html =~ ~s(id="inspector-window-win-2")

    # Close the first window: the second stays open and live (its watch is intact —
    # the guard kept the shared subscription rather than silencing window 2).
    html = render_hook(view, "window_close", %{"id" => "win-1"})
    refute html =~ ~s(id="inspector-window-win-1")
    assert html =~ ~s(id="inspector-window-win-2")
    assert html =~ "insp-freeze live"
    assert Process.alive?(view.pid)

    # The "live" chip alone wouldn't catch a regression that wrongly unsubscribed
    # the SHARED pid when window 1 closed — the chip is just window 2's frozen
    # flag. So drive an actual state write on the shared actor and assert the push
    # still reaches window 2 (its flash-gen bumps): proof the subscription was
    # kept, not silently dropped (BT-2527 #5).
    gen2_before = window_flash_gen(view, "win-2")
    view |> form("#eval-form") |> render_submit(%{expr: "#{name} increment"})

    assert eventually(fn -> window_flash_gen(view, "win-2") > gen2_before end)
  end

  test "a floating-window desk survives a socket reconnect (BT-2527)", %{conn: conn} do
    # A transient socket drop must not wipe a desk full of inspector windows: the
    # session resumes, and the stashed window roots are rebuilt on the reconnect
    # (BT-2527 #3). Same token across two mounts simulates the drop + reconnect.
    conn = with_token(conn, "win-resume-#{System.unique_integer([:positive])}")

    {:ok, view1, _} = live(conn, "/")
    name = spawn_counter(view1)
    id = open_float_window(view1, name)
    assert render(view1) =~ ~s(id="inspector-window-#{id}")

    # Disconnect: terminate/2 stashes the open window and opens the grace window.
    GenServer.stop(view1.pid)

    # Reconnect inside the grace window: the desk is rebuilt from its root binding,
    # back in Float mode, re-reading the live field — not a blank cockpit.
    {:ok, view2, _} = live(conn, "/")
    assert eventually(fn -> render(view2) =~ ~s(id="inspector-overlay") end)
    html = render(view2)
    # Window ids are re-minted from 1 on the fresh mount, so the lone restored
    # window is win-1 again; assert the desk came back, on the binding, in Float.
    assert html =~ ~s(id="inspector-window-win-1")
    assert html =~ "value"
    # Float mode was restored alongside the desk (not the docked default).
    assert selected_inspector_mode(html) == "float"
  end

  test "the doc-block expand state survives a socket reconnect (BT-2570)", %{conn: conn} do
    # A transient socket drop / redeploy / laptop wake must not re-collapse a doc
    # block the user had expanded: `:doc_expanded` is a socket assign a fresh mount
    # (which every reconnect is) re-inits to its collapsed default, so terminate/2
    # stashes it in the registry and the resuming mount restores it. Same token
    # across two mounts simulates the drop + reconnect (mirrors the window-resume
    # test above). We drive the `toggle_doc` event and read the restored assign off
    # the socket — `:doc_expanded` rides the editor toggle independently of whether
    # any docced method is open, so the resume is asserted at the assign level.
    conn = with_token(conn, "doc-resume-#{System.unique_integer([:positive])}")

    {:ok, view1, _} = live(conn, "/")
    assert :sys.get_state(view1.pid).socket.assigns.doc_expanded == false

    # Expand the doc block on the first mount.
    render_click(view1, "toggle_doc", %{})
    assert :sys.get_state(view1.pid).socket.assigns.doc_expanded == true

    # Disconnect: terminate/2 stashes the expand state and opens the grace window.
    GenServer.stop(view1.pid)

    # Reconnect inside the grace window with the SAME token: the session resumes and
    # the stashed expand state is restored — the new mount comes up expanded even
    # though its fresh assigns started from the collapsed default.
    {:ok, view2, _} = live(conn, "/")
    assert eventually(fn -> :sys.get_state(view2.pid).socket.assigns.doc_expanded == true end)
  end

  test "Tidy windows re-cascades every window back on-screen (BT-2527)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    name = spawn_counter(view)
    id = open_float_window(view, name)

    # The recovery affordance renders once a desk is open.
    assert render(view) =~ ~s(phx-click="window_reset_positions")

    # Drag the window far off-screen — the server clamps only `>= 0`, so the
    # off-screen position persists (this is exactly the stranding BT-2527 #4 is
    # about; the client-side viewport max-clamp prevents it for live drags but the
    # server can't know the viewport size).
    render_hook(view, "window_moved", %{"id" => id, "x" => 4000, "y" => 4000})
    assert window_pos(render(view), id) == {4000, 4000}

    # Tidy: every window snaps back onto the default ladder (first at the origin).
    html = render_hook(view, "window_reset_positions", %{})
    assert window_pos(html, id) == {120, 96}
  end

  # The inline z-index of the floating window `id` (`win-N`), parsed out of the
  # render — the server-authoritative stacking the WindowDrag hook reads.
  defp window_z(html, id) do
    case Regex.run(~r/id="inspector-window-#{id}"[^>]*style="[^"]*z-index:(\d+)/, html) do
      [_, n] -> String.to_integer(n)
      _ -> 0
    end
  end

  # The inspector-mode segment currently marked active (`"docked"` | `"float"`),
  # read from the toggle button that carries the `on` class — every mode button
  # renders unconditionally, so only the `on` class distinguishes the live one.
  defp selected_inspector_mode(html) do
    case Regex.run(~r/class="on"[^>]*aria-selected="true"[^>]*phx-value-mode="(\w+)"/, html) do
      [_, mode] -> mode
      _ -> nil
    end
  end

  # The inline left/top of the floating window `id` (`win-N`), parsed out of the
  # render — the server-authoritative position the WindowDrag hook reads.
  defp window_pos(html, id) do
    case Regex.run(~r/id="inspector-window-#{id}"[^>]*style="left:(\d+)px;top:(\d+)px/, html) do
      [_, x, y] -> {String.to_integer(x), String.to_integer(y)}
      _ -> nil
    end
  end

  # Window `id`'s own `data-flash-gen` (the per-window live-refresh counter on its
  # ivar table), parsed out of the render. 0 when that window has no field table
  # yet. Scoped to the window's table id so two windows don't collide.
  defp window_flash_gen(view, id) do
    re = ~r/id="inspector-window-fields-#{id}"[\s\S]*?data-flash-gen="(\d+)"/

    case Regex.run(re, render(view)) do
      [_, n] -> String.to_integer(n)
      _ -> 0
    end
  end

  # The byte offset of window `id`'s `<section>` in the render — a proxy for DOM
  # order, so a test can assert focus did NOT reshuffle the windows.
  defp window_dom_index(html, id) do
    case :binary.match(html, "inspector-window-#{id}") do
      {pos, _len} -> pos
      :nomatch -> -1
    end
  end

  # ── Wave 4: multi-tab isolation / session resume / teardown (BT-2410) ────────

  test "two tabs map to two isolated workspace sessions (BT-2410)", %{conn: conn} do
    # Two distinct per-tab tokens — exactly what `sessionStorage` mints per tab —
    # must yield two isolated sessions: a binding set in one tab is invisible in
    # the other (the spike's tab1 x=100 / tab2 x=999 property, now first-class).
    tab1 = with_token(conn, "tab1-#{System.unique_integer([:positive])}")
    tab2 = with_token(conn, "tab2-#{System.unique_integer([:positive])}")

    {:ok, view1, _} = live(tab1, "/")
    {:ok, view2, _} = live(tab2, "/")

    view1 |> form("#eval-form") |> render_submit(%{expr: "x := 100"})
    view2 |> form("#eval-form") |> render_submit(%{expr: "x := 999"})

    # Each tab reads back ITS OWN x — no cross-tab leakage.
    assert view1 |> form("#eval-form") |> render_submit(%{expr: "x"}) =~ "100"
    assert view2 |> form("#eval-form") |> render_submit(%{expr: "x"}) =~ "999"
  end

  test "a reconnect resumes the same session and retains state (BT-2410)", %{conn: conn} do
    # One tab: same token across two mounts simulates a socket drop + reconnect
    # (or a page reload re-establishing the WebSocket). The reconnecting LiveView
    # must re-bind to the SAME workspace session and see its accumulated bindings.
    conn = with_token(conn, "resume-#{System.unique_integer([:positive])}")

    {:ok, view1, _} = live(conn, "/")
    view1 |> form("#eval-form") |> render_submit(%{expr: "y := 7"})
    assert view1 |> form("#eval-form") |> render_submit(%{expr: "y"}) =~ "7"

    # Disconnect: stopping the LiveView fires terminate/2 → release/1, which opens
    # the grace window (it does NOT close the session). The same-tab reconnect
    # below lands inside that window.
    GenServer.stop(view1.pid)

    {:ok, view2, _} = live(conn, "/")
    # Resumed onto the same session: the earlier binding is still there, with no
    # re-eval — state survived the reconnect.
    assert view2 |> form("#eval-form") |> render_submit(%{expr: "y"}) =~ "7"
    # And the live bindings pane was resubscribed + re-read on resume.
    assert eventually(fn -> render(view2) =~ "y" end)
  end

  test "closing a tab tears down its session after the grace window (BT-2410)", %{conn: conn} do
    # The no-orphaned-sessions guarantee: a tab that closes and never reconnects
    # has its workspace-supervised session reaped after the (test-shortened) grace
    # window — the active-session count returns to its pre-mount value.
    conn = with_token(conn, "teardown-#{System.unique_integer([:positive])}")

    before = BtAttach.Workspace.session_count()
    assert is_integer(before)

    {:ok, view, _} = live(conn, "/")
    view |> form("#eval-form") |> render_submit(%{expr: "z := 1"})
    # One more active session while the tab is open.
    assert eventually(fn -> BtAttach.Workspace.session_count() == before + 1 end)

    # Close the tab and DON'T reconnect: after the grace window the registry reaps
    # the session, so the count returns to baseline (no leak).
    GenServer.stop(view.pid)
    assert eventually(fn -> BtAttach.Workspace.session_count() == before end)
  end

  # ── REPL tab (BT-2543) ───────────────────────────────────────────────────────
  #
  # The REPL is the conversational, line-at-a-time sibling of the Workspace: a
  # request→response scrollback with a bottom-pinned input that shares the SAME
  # eval session + structured result. These exercise the server side of that
  # (the `repl_eval` / history / inspect handlers + the scrollback stream); the
  # browser-only behaviour (Enter submits, ↑/↓ at line edges, the CodeMirror
  # composer) lives in workspace_browser_test.exs.

  test "the REPL tab is present in the dock alongside the other surfaces", %{conn: conn} do
    {:ok, view, html} = live(conn, "/")
    # The new tab and its bottom-input form are in the connected render.
    assert html =~ ~s(phx-value-tab="repl")
    assert has_element?(view, ~s(button[phx-value-tab="repl"]), "REPL")
    assert has_element?(view, "#repl-form")
    # And it has NOT displaced the existing surfaces.
    assert has_element?(view, ~s(button[phx-value-tab="workspace"]))
    assert has_element?(view, ~s(button[phx-value-tab="transcript"]))
    assert has_element?(view, ~s(button[phx-value-tab="changes"]))
  end

  test "switching to the REPL tab preserves the Workspace surface state", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    # All dock panes stay in the DOM (toggled with `hidden`), so switching tabs is
    # pure view state and never tears a surface down — the eval form survives.
    render_hook(view, "dock_tab", %{"tab" => "repl"})
    assert has_element?(view, "#eval-form")
    assert has_element?(view, "#repl-form")
  end

  test "submitting appends a › request / → response pair to the scrollback", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    view |> form("#repl-form") |> render_submit(%{expr: "3 + 4"})
    # One entry, carrying both the echoed request and the computed response.
    assert view |> element(".repl-entry .repl-expr") |> render() =~ "3 + 4"
    assert view |> element(".repl-entry .repl-val") |> render() =~ "7"
    refute has_element?(view, ".repl-entry.err")
  end

  test "the scrollback accumulates one entry per submit (request→response history)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    view |> form("#repl-form") |> render_submit(%{expr: "1 + 1"})
    view |> form("#repl-form") |> render_submit(%{expr: "2 + 2"})
    html = render(view)
    assert html =~ "1 + 1"
    assert html =~ "2 + 2"
  end

  test "submitting clears the REPL input (terminal convention)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    view |> form("#repl-form") |> render_submit(%{expr: "3 + 4"})
    # The input is hook-owned (phx-update=ignore), so the clear is a server push.
    assert_push_event(view, "repl_set_input", %{text: ""})
  end

  test "a bare/blank submit is a no-op — no entry, no clear push", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    view |> form("#repl-form") |> render_submit(%{expr: "   "})
    refute has_element?(view, ".repl-entry")
  end

  test "↑/↓ walk the history ring and push the recalled text", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")

    view |> form("#repl-form") |> render_submit(%{expr: "11 * 2"})
    assert_push_event(view, "repl_set_input", %{text: ""})
    view |> form("#repl-form") |> render_submit(%{expr: "33 + 9"})
    assert_push_event(view, "repl_set_input", %{text: ""})

    # History is most-recent-first: ↑ recalls the newest, then older; ↓ walks back
    # toward the present and past the newest restores the empty live input.
    render_hook(view, "repl_history_prev", %{})
    assert_push_event(view, "repl_set_input", %{text: "33 + 9"})
    render_hook(view, "repl_history_prev", %{})
    assert_push_event(view, "repl_set_input", %{text: "11 * 2"})
    render_hook(view, "repl_history_next", %{})
    assert_push_event(view, "repl_set_input", %{text: "33 + 9"})
    render_hook(view, "repl_history_next", %{})
    assert_push_event(view, "repl_set_input", %{text: ""})
  end

  test "an eval error appends an error entry, it does not crash the REPL", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    # A malformed expression: the workspace returns an error, which lands as an
    # `err` scrollback entry rather than crashing the LiveView.
    view |> form("#repl-form") |> render_submit(%{expr: "1 +"})
    assert has_element?(view, ".repl-entry.err")
    assert Process.alive?(view.pid)
  end

  test "a REPL result stays a live object — Inspect opens it in the Inspector", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    class = "ReplBox#{System.unique_integer([:positive])}"

    class_src =
      "Actor subclass: #{class}\n  state: n = 7\n\n  n => self.n"

    view |> form("#repl-form") |> render_submit(%{expr: class_src})
    # Spawn a live actor IN the REPL; its `→ result` entry carries an Inspect
    # affordance into the cockpit Inspector (object soul preserved in the terminal).
    view |> form("#repl-form") |> render_submit(%{expr: "#{class} spawn"})

    button = view |> element(".repl-entry:last-child .repl-inspect")
    assert has_element?(button)
    html = render_click(button)
    # The Inspector now heads on the REPL result and reads its live field.
    assert html =~ "REPL result"
    assert html =~ "n"
  end

  # ── REPL meta-commands (BT-2543 follow-up) ──────────────────────────────────
  #
  # `:`-prefixed commands are an IDE concern, never sent to `eval` (which would
  # choke compiling `:h` as Beamtalk). They drive a pane (`:help X` focuses the
  # System Browser) or point at one (`:bindings` → Bindings pane); unknown colon
  # input gets a friendly note rather than a parse error.

  test ":help with no argument prints an info entry, not an eval error", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    html = view |> form("#repl-form") |> render_submit(%{expr: ":help"})
    # Routed by the IDE: a muted `:info` entry, NOT an `err` entry, and never
    # round-tripped through eval (which would reject `:help` as a parse error).
    assert has_element?(view, ".repl-entry.meta")
    refute has_element?(view, ".repl-entry.err")
    assert html =~ "IDE REPL"
  end

  test ":help <Class> focuses the System Browser on that class", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    class = "ReplHelp#{System.unique_integer([:positive])}"
    # Define the class in the image so it is in the live symbol index.
    view
    |> form("#repl-form")
    |> render_submit(%{expr: "Object subclass: #{class}\n  greet => 1"})

    html = view |> form("#repl-form") |> render_submit(%{expr: ":help #{class}"})
    # Confirmation in the scrollback …
    assert html =~ "Opened #{class} in the System Browser"
    refute has_element?(view, ".repl-entry.err")
    # … and the System Browser is now pointed at the class: its protocol pane
    # heads on the class (`selected_class` set + protocols loaded).
    assert view |> element(".panel-head", class) |> render() =~ "methods"
  end

  test ":help <Unknown> reports no such class instead of opening the browser", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    html = view |> form("#repl-form") |> render_submit(%{expr: ":help NoSuchClassHere"})
    assert html =~ "No class named NoSuchClassHere"
  end

  test ":bindings points at the Bindings pane rather than evaluating", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    html = view |> form("#repl-form") |> render_submit(%{expr: ":bindings"})
    assert has_element?(view, ".repl-entry.meta")
    refute has_element?(view, ".repl-entry.err")
    assert html =~ "Bindings pane"
  end

  test "an unrecognised :command gets a friendly note, never a parse error", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    html = view |> form("#repl-form") |> render_submit(%{expr: ":wat"})
    assert has_element?(view, ".repl-entry.meta")
    refute has_element?(view, ".repl-entry.err")
    assert html =~ "Unknown command :wat"
  end

  test "`Beamtalk help: Class` evaluates AND focuses the System Browser", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    class = "ReplHelpSend#{System.unique_integer([:positive])}"

    view
    |> form("#repl-form")
    |> render_submit(%{expr: "Object subclass: #{class}\n  greet => 1"})

    # A real `help:` send: its help text still lands as a normal `→ result`
    # entry, and the browser additionally navigates to the subject class.
    view |> form("#repl-form") |> render_submit(%{expr: "Beamtalk help: #{class}"})
    refute has_element?(view, ".repl-entry.err")
    assert view |> element(".panel-head", class) |> render() =~ "methods"
  end

  describe "REPL tab — read-only Observer (BT-2421)" do
    setup %{conn: conn} do
      Application.put_env(:bt_attach, :oidc, %{
        issuer: "https://idp",
        client_id: "id",
        redirect_uri: "https://ide/callback",
        groups_claim: "groups",
        client_secret: "x",
        roles: %{"owner" => ["beamtalk-owners"], "observer" => ["beamtalk-observers"]}
      })

      Application.put_env(:bt_attach, :session_ttl_secs, 3600)

      on_exit(fn ->
        Application.delete_env(:bt_attach, :oidc)
        Application.delete_env(:bt_attach, :session_ttl_secs)
      end)

      conn =
        Plug.Test.init_test_session(conn, %{
          "bt_user" => %{"sub" => "obs", "groups" => ["beamtalk-observers"]},
          "bt_logged_in_at" => System.system_time(:second)
        })

      {:ok, conn: conn}
    end

    test "the REPL input is hidden for an Observer", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/")
      assert html =~ "read-only (Observer)"
      refute html =~ ~s(phx-submit="repl_eval")
    end

    test "a crafted repl_eval from an Observer is refused, not crashed", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")
      # The input is gated away, but a crafted client event must still be refused by
      # the facade/RBAC and surfaced as an error entry, not crash the LiveView.
      html = render_hook(view, "repl_eval", %{"expr" => "3 + 4"})
      assert html =~ "Not authorized"
      assert Process.alive?(view.pid)
    end

    test "an Observer can run :help <Class> — meta routing performs no privileged op",
         %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")
      # `:help` is a client-side meta-command: it routes through `handle_repl_meta`
      # (driving the System Browser via the `:read` `:symbols` op), never the `:eval`
      # op. So an Observer who cannot eval can still navigate with it — no 403, no
      # crash — regardless of whether the class is in the symbol index.
      html = render_hook(view, "repl_eval", %{"expr" => ":help Object"})
      refute html =~ "Not authorized"
      # Routes to an `:info` meta entry (either "Opened Object" or, if the symbol
      # index lacks it, "No class named Object") — never an `:eval` RBAC error.
      assert has_element?(view, ".repl-entry.meta")
      refute has_element?(view, ".repl-entry.err")
      assert Process.alive?(view.pid)
    end
  end

  # Set the per-tab resume token on the conn so the connected mount reads it back
  # via `get_connect_params/1` — the test analogue of the `sessionStorage` token
  # the browser replays on the LiveSocket params.
  defp with_token(conn, token) do
    Phoenix.LiveViewTest.put_connect_params(conn, %{"workspace_token" => token})
  end

  # The Inspector's `data-flash-gen` counter (BT-2492) parsed out of the render —
  # the server-side signal the FieldFlash hook keys its pulses off. 0 when the
  # attribute is absent (no fields rendered yet).
  defp flash_gen(view) do
    case Regex.run(~r/data-flash-gen="(\d+)"/, render(view)) do
      [_, n] -> String.to_integer(n)
      _ -> 0
    end
  end

  # Open (and focus) a class-definition tab so the editor has a live active tab to
  # assert against. The cockpit now opens with an EMPTY tab strip (no starter tab),
  # so a test that needs an editor tab opens one first. Uses a freshly-defined
  # per-test Actor class (defined via eval, then picked up by a remount's
  # browse-classes snapshot) rather than a built-in — the same define → remount →
  # `browser_open_definition` path the passing "selecting a class" test exercises,
  # so it does not depend on how any particular built-in class browses. Returns
  # `{view, html, class}` (the post-open html + the synthesized class name, which
  # the breadcrumb / tab label show).
  defp open_fresh_def_tab(conn) do
    suffix = System.unique_integer([:positive])
    class = "TabHost#{suffix}"

    {:ok, view, _} = live(conn, "/")

    view
    |> form("#eval-form")
    |> render_submit(%{
      expr: "Actor subclass: #{class}\n  state: value = 0\n\n  value => self.value"
    })

    # browse-classes is a mount snapshot — remount so the tree includes the class.
    {:ok, view, _} = live(conn, "/")
    assert eventually(fn -> render(view) =~ class end)

    view |> element(~s(div[phx-value-class="#{class}"])) |> render_click()

    html =
      view
      |> element(~s(div[phx-click="browser_open_definition"][phx-value-class="#{class}"]))
      |> render_click()

    {view, html, class}
  end

  # The active method-editor tab id, read from the CmEditor element's stamp
  # (`data-tab-id`) — the same value a real selection push carries (BT-2549).
  defp active_tab_id(html) do
    case Regex.run(~r/data-tab-id="([^"]+)"/, html) do
      [_, id] -> id
      nil -> raise "data-tab-id not rendered in HTML"
    end
  end

  # The internal `:edit_selection` assign — no rendered consumer yet (BT-2549),
  # so peek at the LiveView's socket to assert the selection-guard behaviour.
  defp edit_selection(view) do
    :sys.get_state(view.pid).socket.assigns.edit_selection
  end

  # ~6s total — generous for cross-node async transcript delivery under CI load.
  defp eventually(fun, retries \\ 120) do
    cond do
      fun.() ->
        true

      retries == 0 ->
        false

      true ->
        Process.sleep(50)
        eventually(fun, retries - 1)
    end
  end
end
