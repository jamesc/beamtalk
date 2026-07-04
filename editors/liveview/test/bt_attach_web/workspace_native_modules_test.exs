# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceNativeModulesTest do
  @moduledoc """
  Integration test for the System Browser "Native modules" section (BT-2648): a
  loaded package's hand-written native Erlang modules are discoverable in the IDE
  even when no `native:` class backs them (the reported beamtalk-http case — a
  dependency loaded without instantiating its classes). The section lists the
  modules, tags them by package/origin (DEP · <pkg> / STDLIB), and opens a
  module's `.erl` read-only in the native source-view pane. A `.beam`-only module
  (no shipped source) degrades to the existing "source not available" note.

  Drives the LiveView against the fully-stubbed workspace client
  (`BtAttachWeb.StubWorkspaceClient`), whose `browse_native_modules` /
  `browse_native_module_source` mirror the new ops. No `:workspace` tag, so this
  runs in the bare `mix test` lane.
  """
  use BtAttachWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  setup do
    Application.put_env(:bt_attach, :workspace_client, BtAttachWeb.StubWorkspaceClient)

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
      Application.delete_env(:bt_attach, :workspace_client)
      Application.delete_env(:bt_attach, :oidc)
      Application.delete_env(:bt_attach, :session_ttl_secs)
      BtAttachWeb.StubWorkspaceClient.stop_state(2_000)
    end)

    {:ok, _} = BtAttachWeb.StubWorkspaceClient.start_state()

    :ok
  end

  defp owner_conn(conn) do
    Plug.Test.init_test_session(conn, %{
      "bt_user" => %{"sub" => "alice", "groups" => ["beamtalk-owners"]},
      "bt_logged_in_at" => System.system_time(:second)
    })
  end

  defp observer_conn(conn) do
    Plug.Test.init_test_session(conn, %{
      "bt_user" => %{"sub" => "bob", "groups" => ["beamtalk-observers"]},
      "bt_logged_in_at" => System.system_time(:second)
    })
  end

  describe "Native browser (BT-2656)" do
    test "the panel defaults to Classes mode; native module rows are not in the tree", %{
      conn: conn
    } do
      {:ok, _view, html} = live(owner_conn(conn), "/")

      # BT-2656: the `Classes | Native` mode toggle replaces the retired collapsible
      # in-tree section. On open the panel is in Classes mode, so no native rows show.
      assert html =~ ~s(aria-label="Browser mode")
      assert html =~ ~s(phx-value-mode="native")
      refute html =~ ~s(phx-value-module="beamtalk_http_client")
      # The retired in-tree section / toggle is gone.
      refute html =~ "toggle_native_modules"
    end

    test "switching to Native mode lists modules with package/origin badges", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      html = render_click(view, "browser_mode", %{"mode" => "native"})

      # A dependency's native module (no class to back it) is listed and tagged
      # DEP · <pkg> — discoverable WITHOUT a native: class (the acceptance case).
      assert html =~ ~s(phx-value-module="beamtalk_http_client")
      assert html =~ "DEP · http"
      # The Native browser footer carries the count badge.
      assert html =~ ~s(aria-label="Native source filter")

      # Switching back to Classes mode restores the class tree and hides native rows.
      html = render_click(view, "browser_mode", %{"mode" => "classes"})
      refute html =~ ~s(phx-value-module="beamtalk_http_client")
      assert html =~ ~s(aria-label="Class source filter")
    end

    test "switching to Native hides the Beamtalk method browser; Classes restores it (BT-2733)",
         %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Select a Beamtalk class so the lower method browser shows its protocol/method
      # list (the `sb-classdef` "class definition" + owner-only "Add a method…" rows).
      html = render_click(view, "browser_select_class", %{"class" => "Counter"})
      assert html =~ ~s(class="tree sb-classdef")
      assert html =~ "Add a method"

      # BT-2733: switching to Native must NOT leave that class's method list showing
      # beneath the unrelated native-module list — the whole method panel is hidden.
      html = render_click(view, "browser_mode", %{"mode" => "native"})
      assert html =~ ~s(phx-value-module="beamtalk_http_client")
      refute html =~ ~s(class="tree sb-classdef")
      refute html =~ "Add a method"

      # `@selected_class` is left intact, so switching back to Classes restores the
      # method list with no re-selection.
      html = render_click(view, "browser_mode", %{"mode" => "classes"})
      assert html =~ ~s(class="tree sb-classdef")
      assert html =~ "Add a method"
    end

    test "the native origin filter defaults to All when no module is project-origin", %{
      conn: conn
    } do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # BT-2661: the Native browser defaults its origin filter to Project, falling
      # back to All when (as in the stub) no native module is project-origin. With
      # the All default, both the dependency and stdlib modules are visible.
      html = render_click(view, "browser_mode", %{"mode" => "native"})
      assert html =~ ~s(<option value="all" selected)
      assert html =~ ~s(phx-value-module="beamtalk_http_client")
      assert html =~ ~s(phx-value-module="beamtalk_subprocess")
    end

    test "the native origin filter narrows the list (deps vs stdlib)", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "browser_mode", %{"mode" => "native"})

      # Narrow to stdlib: only the stdlib module survives; the dependency module is
      # filtered out.
      html = render_click(view, "native_source", %{"src" => "stdlib"})
      assert html =~ ~s(phx-value-module="beamtalk_subprocess")
      refute html =~ ~s(phx-value-module="beamtalk_http_client")

      # Narrow to deps: only the dependency modules survive.
      html = render_click(view, "native_source", %{"src" => "deps"})
      assert html =~ ~s(phx-value-module="beamtalk_http_client")
      refute html =~ ~s(phx-value-module="beamtalk_subprocess")
    end

    test "clicking a native module opens its .erl read-only as an editor tab (BT-2667)",
         %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "browser_mode", %{"mode" => "native"})

      html =
        render_click(view, "browser_open_native_module", %{"module" => "beamtalk_http_client"})

      # BT-2667: the module opens as a first-class editor TAB (in the tab strip),
      # not the retired single-slot overlay. The tab is labelled `<module>.erl`.
      assert html =~ ~s(role="tab")
      assert html =~ "beamtalk_http_client.erl"
      # The tab body renders the source read-only.
      assert html =~ "Erlang module"
      assert html =~ ~s(class="native-pre")
      assert html =~ "handle_call({get"
      assert html =~ "read-only"
      # BT-2668: the displayed path is the clean, project-relative form — never the
      # absolute `/home/...` host path the op returned.
      assert html =~ "deps/beamtalk_http/native/beamtalk_http_client.erl"
      refute html =~ "/home/agent"

      # BT-2667: re-clicking the same module FOCUSES the existing tab rather than
      # closing it (the old overlay toggled off) or stacking a duplicate — only one
      # `.erl` tab exists.
      html =
        render_click(view, "browser_open_native_module", %{"module" => "beamtalk_http_client"})

      assert html =~ ~s(class="native-pre")
      # Exactly one tab in the strip for this module (re-open focused, didn't stack
      # a duplicate): the tab id appears twice — the tab button + its close × —
      # and a duplicated tab would double that to four.
      assert count_occurrences(html, ~s(phx-value-id="native:beamtalk_http_client")) == 2

      # Closing the tab removes it; the editor returns to its empty state.
      html = render_click(view, "tab_close", %{"id" => "native:beamtalk_http_client"})
      refute html =~ ~s(class="native-pre")
      refute html =~ "beamtalk_http_client.erl"
    end

    test "a native module tab coexists with a class tab; it does not replace the class (BT-2667)",
         %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Open a Beamtalk class definition tab first.
      html = render_click(view, "browser_open_definition", %{"class" => "Counter"})
      assert html =~ "Counter ▸ def"

      # Open a native module — it must NOT overlay/replace the class tab.
      render_click(view, "browser_mode", %{"mode" => "native"})

      html =
        render_click(view, "browser_open_native_module", %{"module" => "beamtalk_http_client"})

      # Both tabs are present in the strip; the native tab is now focused.
      assert html =~ "Counter ▸ def"
      assert html =~ "beamtalk_http_client.erl"
      assert html =~ ~s(class="native-pre")

      # Switching back to the class tab restores the class editor (the native tab
      # did not clobber it).
      html = render_click(view, "tab_select", %{"id" => "def:Counter"})
      assert html =~ "Counter ▸ def"
      refute html =~ ~s(class="native-pre")
    end

    test "a module with no shipped source shows the graceful empty state", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "browser_mode", %{"mode" => "native"})

      html =
        render_click(view, "browser_open_native_module", %{"module" => "beamtalk_http_stripped"})

      # No source content → the honest "not available" note, not an error or an
      # empty pane.
      assert html =~ "Erlang source not available"
      refute html =~ ~s(class="native-pre")
    end

    test "an observer can browse native modules (the op is :read)", %{conn: conn} do
      {:ok, view, _html} = live(observer_conn(conn), "/")

      render_click(view, "browser_mode", %{"mode" => "native"})

      html =
        render_click(view, "browser_open_native_module", %{"module" => "beamtalk_http_client"})

      assert html =~ "Erlang module"
      assert html =~ ~s(class="native-pre")
      assert html =~ "handle_call({get"
    end
  end

  # BT-2669: the native-module "Callers" affordance — the reverse of "go to
  # native source". From a native module tab, list the Beamtalk class>>method
  # sites that call into it via `(Erlang <module>) …`, reusing the
  # Senders/Implementors popover; clicking a caller opens that Beamtalk method.
  describe "native-module Callers (BT-2669)" do
    test "the Callers button lists the calling Beamtalk methods and opens one", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "browser_mode", %{"mode" => "native"})

      # Open the stub's `beamtalk_subprocess` module (the stub returns one caller,
      # Subprocess>>spawn:, for it).
      html =
        render_click(view, "browser_open_native_module", %{"module" => "beamtalk_subprocess"})

      assert html =~ "beamtalk_subprocess.erl"
      # The native tab carries the Callers affordance.
      assert html =~ ~s(phx-click="native_callers")

      # Click Callers: the shared nav popover opens with the "Callers" heading,
      # the module name, and the calling site row.
      html = render_click(view, "native_callers", %{})
      assert html =~ ~s(class="nav-popover")
      assert html =~ "Callers"
      assert html =~ "beamtalk_subprocess"
      assert html =~ "Subprocess"
      assert html =~ "spawn:"

      # Clicking the caller row opens that Beamtalk method tab (reusing nav_open).
      html =
        render_click(view, "nav_open", %{
          "class" => "Subprocess",
          "side" => "instance",
          "selector" => "spawn:"
        })

      assert html =~ "spawn:"
      # The popover closed after navigating.
      refute html =~ ~s(class="nav-popover")
    end

    test "a native module with no Beamtalk callers shows the empty state", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "browser_mode", %{"mode" => "native"})

      # `beamtalk_http_client` has no canned callers in the stub.
      render_click(view, "browser_open_native_module", %{"module" => "beamtalk_http_client"})

      html = render_click(view, "native_callers", %{})
      assert html =~ ~s(class="nav-popover")
      assert html =~ "Callers"
      assert html =~ "No Callers found."
    end

    test "an observer can use the Callers view (the op is :read)", %{conn: conn} do
      {:ok, view, _html} = live(observer_conn(conn), "/")

      render_click(view, "browser_mode", %{"mode" => "native"})
      render_click(view, "browser_open_native_module", %{"module" => "beamtalk_subprocess"})

      html = render_click(view, "native_callers", %{})
      assert html =~ "Callers"
      assert html =~ "Subprocess"
    end

    # BT-2732: the most common native module in a project is the gen_server that
    # backs an actor class via ADR 0056 (`native:` + `self delegate`). Its
    # delegating methods route into it via generated dispatch, not `(Erlang …)`
    # FFI sends, so the server-side nav op merges them in — the Callers view must
    # list the delegating class + its `self delegate` methods, each opening the
    # Beamtalk method.
    test "Callers lists an actor class's self delegate methods for its backing module",
         %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "browser_mode", %{"mode" => "native"})

      # `beamtalk_atomic_counter` backs the `AtomicCounter` actor class.
      render_click(view, "browser_open_native_module", %{"module" => "beamtalk_atomic_counter"})

      html = render_click(view, "native_callers", %{})
      assert html =~ ~s(class="nav-popover")
      assert html =~ "Callers"
      assert html =~ "beamtalk_atomic_counter"
      # The delegating class and its `self delegate` methods render as caller rows.
      assert html =~ "AtomicCounter"
      assert html =~ "increment"
      assert html =~ "incrementBy:"

      # Clicking a delegate row opens that Beamtalk method (reusing nav_open).
      html =
        render_click(view, "nav_open", %{
          "class" => "AtomicCounter",
          "side" => "instance",
          "selector" => "incrementBy:"
        })

      assert html =~ "incrementBy:"
      refute html =~ ~s(class="nav-popover")
    end
  end

  # BT-2670: a *project-owned* native (`.erl`) module is editable in the IDE —
  # edit → compile → reload → write-back via `native_save`. Deps/stdlib natives
  # stay strictly read-only, as does every non-Owner role. Compile errors surface
  # inline (structured, like Beamtalk compile errors); a clean compile shows a
  # confirmation and clears the tab's dirty state.
  describe "editable project native (BT-2670)" do
    test "a project-owned native opens editable for the owner", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "browser_mode", %{"mode" => "native"})

      html =
        render_click(view, "browser_open_native_module", %{"module" => "beamtalk_project_native"})

      assert html =~ "beamtalk_project_native.erl"
      # The editable tab renders the write-surface form (CodeMirror + Compile),
      # NOT the read-only `<pre>` viewer, and is badged "editable".
      assert html =~ ~s(id="native-editor-form")
      assert html =~ "Compile &amp; Reload"
      assert html =~ "editable"
      refute html =~ ~s(class="native-pre")
    end

    test "a deps/stdlib native stays read-only even for the owner", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "browser_mode", %{"mode" => "native"})

      # A dependency native (`editable: false` from the op).
      html =
        render_click(view, "browser_open_native_module", %{"module" => "beamtalk_http_client"})

      refute html =~ ~s(id="native-editor-form")
      assert html =~ ~s(class="native-pre")
      assert html =~ "read-only"
    end

    test "a project native is read-only for an observer (editing is Owner-gated)",
         %{conn: conn} do
      {:ok, view, _html} = live(observer_conn(conn), "/")

      render_click(view, "browser_mode", %{"mode" => "native"})

      html =
        render_click(view, "browser_open_native_module", %{"module" => "beamtalk_project_native"})

      # Even though the op reports `editable: true`, the Observer never gets the
      # write-surface form — editing is Owner-gated in the IDE.
      refute html =~ ~s(id="native-editor-form")
      assert html =~ ~s(class="native-pre")
    end

    test "saving a project native compiles + reloads and confirms", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "browser_mode", %{"mode" => "native"})
      render_click(view, "browser_open_native_module", %{"module" => "beamtalk_project_native"})

      new_source = "-module(beamtalk_project_native).\n-export([go/0]).\n\ngo() -> updated.\n"
      html = render_submit(view, "native_save", %{"source" => new_source})

      # Success banner; no error.
      assert html =~ "Saved beamtalk_project_native.erl"
      refute html =~ ~s(class="notice err)

      # The save op was dispatched with the edited source.
      assert Enum.any?(
               BtAttachWeb.StubWorkspaceClient.calls(),
               &match?({:save_native_source, "beamtalk_project_native", ^new_source}, &1)
             )
    end

    test "a compile error surfaces inline structured diagnostics, not a crash", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Force the next save to report a structured compile error (the load-path
      # error-map shape).
      BtAttachWeb.StubWorkspaceClient.set_native_save(
        {:value,
         %{
           "module" => "beamtalk_project_native",
           "source_file" => "native/beamtalk_project_native.erl",
           "errors" => [
             %{
               "path" => "native/beamtalk_project_native.erl",
               "kind" => "compile_error",
               "message" => "syntax error before: '.'",
               "line" => 4
             }
           ]
         }}
      )

      render_click(view, "browser_mode", %{"mode" => "native"})
      render_click(view, "browser_open_native_module", %{"module" => "beamtalk_project_native"})

      html = render_submit(view, "native_save", %{"source" => "go() ->\n"})

      # The structured error renders inline (line + message), not a "Saved" banner.
      assert html =~ "syntax error before"
      assert html =~ "line 4"
      refute html =~ "Saved beamtalk_project_native.erl"
      # The editable form is still present (no crash, the tab survives).
      assert html =~ ~s(id="native-editor-form")
    end

    test "a server-side read-only rejection surfaces the error, not a save", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Model the workspace rejecting the write (server-side ownership guard).
      BtAttachWeb.StubWorkspaceClient.set_native_save(
        {:error, "Native module 'beamtalk_project_native' is not an editable project source."}
      )

      render_click(view, "browser_mode", %{"mode" => "native"})
      render_click(view, "browser_open_native_module", %{"module" => "beamtalk_project_native"})

      html = render_submit(view, "native_save", %{"source" => "go() -> ok.\n"})

      assert html =~ "not an editable project source"
      refute html =~ "Saved beamtalk_project_native.erl"
    end
  end

  # BT-2668: the path-cleaning that keeps the absolute host path off-screen. Unit
  # test of the relativiser directly (it runs on whatever path the runtime op
  # returns), independent of the rendered viewer above.
  describe "clean_native_path/1 (BT-2668)" do
    alias BtAttachWeb.WorkspaceLive

    test "relativises an absolute build path to a recognisable source root" do
      assert WorkspaceLive.clean_native_path(
               "/home/james/source/proj/_build/default/deps/http/native/x.erl"
             ) == "deps/http/native/x.erl"

      assert WorkspaceLive.clean_native_path("/opt/app/apps/beamtalk_stdlib/src/y.erl") ==
               "apps/beamtalk_stdlib/src/y.erl"
    end

    test "leaves an already-relative path untouched" do
      assert WorkspaceLive.clean_native_path("deps/http/native/x.erl") ==
               "deps/http/native/x.erl"
    end

    test "falls back to the basename for an absolute path with no known source root" do
      assert WorkspaceLive.clean_native_path("/home/james/scratch/foo.erl") == "foo.erl"
    end

    test "returns nil for the absent-path sentinels (no path leak)" do
      assert WorkspaceLive.clean_native_path(:null) == nil
      assert WorkspaceLive.clean_native_path(nil) == nil
      assert WorkspaceLive.clean_native_path("") == nil
    end
  end

  # Count non-overlapping occurrences of `needle` in `haystack` — proves a tab is
  # focused, not duplicated, when its module is re-opened.
  defp count_occurrences(haystack, needle) do
    haystack |> String.split(needle) |> length() |> Kernel.-(1)
  end
end
