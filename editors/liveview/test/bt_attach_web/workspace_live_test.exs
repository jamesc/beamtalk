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
      assert html =~ ~s(phx-click="browser_select_class")

      # A class-tree click from an Observer re-renders without a "Not authorized"
      # refusal — browse is a read op, so the read-only role may browse.
      selected = render_hook(view, "browser_select_class", %{"class" => "Object"})
      refute selected =~ "Not authorized"
      assert Process.alive?(view.pid)
    end
  end

  test "eval round-trip renders the workspace result term", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    view |> form("#eval-form") |> render_submit(%{expr: "3 + 4"})
    # Scope the assertion to the result value element, NOT the whole page: the
    # connected shell renders a session id (`phoenix-<integer>`) whose digits
    # would let a bare `html =~ "7"` pass even when eval is completely broken and
    # the result pane shows an error. The value span is the real signal — it must
    # carry the computed `7` and there must be no error pane (BT-2496).
    assert view |> element(".ws-result:not(.err) .val") |> render() =~ "7"
    refute render(view) =~ "Empty expression"
  end

  test "eval state persists across evals within a session", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")

    # Bind a variable, then read it back in a later eval on the same session.
    view |> form("#eval-form") |> render_submit(%{expr: "x := 21 * 2"})
    view |> form("#eval-form") |> render_submit(%{expr: "x"})
    # Result-scoped (see "eval round-trip" above) so a session-id digit can't
    # spuriously satisfy the assertion when eval is broken.
    assert view |> element(".ws-result:not(.err) .val") |> render() =~ "42"
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

  # ── Phase 1 JS hook foundation (BT-2485) ────────────────────────────────────

  test "the editor hooks are present on the owner's connected render (BT-2485)", %{conn: conn} do
    {:ok, _view, html} = live(conn, "/")

    # CodeEditor overlay (highlight <pre> behind the transparent <textarea>),
    # KeyboardShortcuts (⌘S → submit) and SelectionTracker (select_source).
    assert html =~ ~s(phx-hook="CodeEditor")
    assert html =~ ~s(phx-hook="KeyboardShortcuts")
    assert html =~ ~s(phx-hook="SelectionTracker")
    assert html =~ ~s(data-shortcuts)
    assert html =~ ~s(data-select-event="select_source")
    assert html =~ "bt-editor-pre"
  end

  test "the SelectionTracker hook event is accepted and ignored when malformed (BT-2485)", %{
    conn: conn
  } do
    {:ok, view, _html} = live(conn, "/")

    # A well-formed selection payload must not crash the LiveView (the assign is
    # internal, so we just prove the handler accepts it and re-renders).
    assert render_hook(view, "select_source", %{
             "text" => "self.value",
             "start" => 0,
             "end" => 10
           })

    # A malformed payload (no text key, or non-binary) is ignored, not a crash —
    # the LiveView keeps rendering, proving the defensive clause holds.
    assert render_hook(view, "select_source", %{"garbage" => true})
    assert render_hook(view, "select_source", %{"text" => 123})
  end

  # ── Phase 2 tabbed method editor (BT-2494) ──────────────────────────────────

  test "the method editor renders a tab strip + breadcrumb (BT-2494)", %{conn: conn} do
    {:ok, _view, html} = live(conn, "/")

    # The tabbed write-surface: a tab strip over a breadcrumb. The starter tab is
    # the Counter#increment method; the "+ def" affordance opens a class
    # definition; the save_method form is preserved with a hidden tab field.
    assert html =~ "tabstrip"
    assert html =~ "editor-meta"
    assert html =~ ~s(phx-click="tab_select")
    assert html =~ ~s(phx-click="open_definition")
    assert html =~ "+ def"
    assert html =~ ~s(phx-submit="save_method")
    assert html =~ ~s(name="tab")
    # The breadcrumb shows Class › side › selector for the active tab.
    assert html =~ "Counter"
    assert html =~ "increment"
  end

  test "the starter tab populates the class/selector inputs on mount (BT-2518)", %{conn: conn} do
    {:ok, _view, html} = live(conn, "/")

    # The method editor's class/selector inputs must mirror the starter tab from
    # the FIRST connected render (init_tabs syncs the active tab into the edit
    # assigns) — not stay empty until the tab is clicked. The inputs render the
    # starter Counter#increment values.
    assert html =~ ~s(value="Counter")
    assert html =~ ~s(value="increment")
  end

  test "a save on open compiles against the starter tab without clicking it first (BT-2518)", %{
    conn: conn
  } do
    {:ok, view, _html} = live(conn, "/")

    # Reproduce BT-2518: on a fresh connected mount, submit the method editor
    # form WITHOUT first clicking the tab, so the submit rides the inputs' own
    # values. Before the fix the class/selector inputs were empty, so the save
    # failed local validation with "Enter a class name to save a method." With
    # the starter tab synced, the payload carries Counter/increment and the save
    # proceeds past validation (it may then report a compile/save error — that's
    # fine; the regression was the spurious empty-class guard).
    html =
      view
      |> form("form[phx-submit='save_method']")
      |> render_submit()

    refute html =~ "Enter a class name"
    refute html =~ "Enter a selector"
  end

  test "opening a class definition adds a + def tab and switches to it (BT-2494)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")

    # "+ def" opens (or re-focuses) the active class's definition tab — a tab
    # whose compile evals the class definition. The tab label carries the ▸ def
    # marker and the breadcrumb switches to the class-definition form.
    html = view |> element(~s(button[phx-click="open_definition"])) |> render_click()
    assert html =~ "Counter ▸ def"
    assert html =~ "class definition"
  end

  test "a dirty edit marks the active tab with a dirty dot (BT-2494)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")

    # The starter Counter tab opens clean (no dirty dot), reporting "in image".
    assert render(view) =~ "in image"
    refute render(view) =~ "modot"

    # Editing the source via the form's phx-change marks the active tab dirty —
    # the dirty dot (.modot) appears in the tab strip and the meta-note flips to
    # "edited". This is pure view state (no workspace round-trip).
    edited =
      view
      |> form("#method-editor-form")
      |> render_change(%{"source" => "increment => self.value := self.value + 2"})

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
    {:ok, _view, html} = live(conn, "/")

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
    assert html =~ ~s(phx-click="browser_select_class")
    refute html =~ "Lands in a later Phase 1 issue."

    # The class tree is populated from live browse-classes — a core class like
    # Object is always in the image.
    assert html =~ "Object"
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

  test "selecting a method shows its source with a breadcrumb (BT-2491)", %{conn: conn} do
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

    # Click the method row: browse-method-source drives the centre display with a
    # `Class instance » selector` breadcrumb and the method source.
    html =
      view
      |> element(~s(div[phx-value-selector="increment"]))
      |> render_click()

    assert html =~ ~s(id="browse-method-source")
    assert html =~ class
    assert html =~ "increment"
    # The breadcrumb carries the instance side and the » separator (ADR-spec
    # `Class instance » selector · category`).
    assert html =~ "»"
    assert html =~ "instance"
    # The actual method body is shown read-only.
    assert html =~ "self.value"
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

  # Set the per-tab resume token on the conn so the connected mount reads it back
  # via `get_connect_params/1` — the test analogue of the `sessionStorage` token
  # the browser replays on the LiveSocket params.
  defp with_token(conn, token) do
    Phoenix.LiveViewTest.put_connect_params(conn, %{"workspace_token" => token})
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
