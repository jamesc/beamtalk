# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceFlushBadgeTest do
  @moduledoc """
  End-to-end integration test for the flush-clears-unflushed-badge flow
  (BT-2545 / BT-2554).

  Exercises the full compile → `unflushed` shown → flush → `unflushed` cleared
  flow through the LiveView against a **flushable** method using a fully-stubbed
  workspace client (`BtAttachWeb.StubWorkspaceClient`). No `:workspace` tag, so
  this runs in the bare `mix test` lane.

  The stub client's `save_method/3` adds an entry to the shared change log,
  and `flush/0` clears it — so the flush actually "writes" the method from the
  perspective of the reconcile logic (`flushed_method_keys/3` /
  `clear_disk_differs/2`).

  No reliance on the pure-unit-test shortcut for the integration path (the unit
  tests in `workspace_live_reconcile_test.exs` stay as fast complementary
  coverage).
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

  defp eventually(fun), do: eventually(fun, 20)
  defp eventually(fun, 0), do: fun.()

  defp eventually(fun, retries) do
    case fun.() do
      true ->
        true

      _ ->
        Process.sleep(50)
        eventually(fun, retries - 1)
    end
  end

  describe "flush clears the unflushed badge (BT-2545 / BT-2554)" do
    test "compile → unflushed shown → flush → unflushed cleared", %{conn: conn} do
      suffix = System.unique_integer([:positive])
      class = "FlushBadge#{suffix}"

      # 1. Define a class via eval (first mount)
      {:ok, view, _html} = live(owner_conn(conn), "/")

      class_src = """
      Actor subclass: #{class}
        state: value = 0

        increment => self.value := self.value + 1

        value => self.value
      """

      view |> form("#eval-form") |> render_submit(%{expr: class_src})

      # 2. Remount so the system browser picks up the new class
      {:ok, view, _html} = live(owner_conn(conn), "/")
      assert eventually(fn -> render(view) =~ class end)

      # 3. Browse the class and open the method as a tab
      view |> element(~s(div[phx-value-class="#{class}"])) |> render_click()

      open_html =
        view
        |> element(~s(div[phx-value-selector="increment"]))
        |> render_click()

      assert open_html =~ "in image"

      # 4. Save a new body — compile_clean flips disk_differs to true
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
      assert save_html =~ "unflushed"

      # 5. Flush — the badge must clear
      flush_html =
        view
        |> element("button[phx-click='flush']")
        |> render_click()

      assert flush_html =~ "Flushed"
      refute flush_html =~ "unflushed"
    end

    test "re-saving the identical on-disk body does not show unflushed (BT-2550)", %{conn: conn} do
      # `save_method` logs a ChangeLog entry for every `>>`, but a byte-for-byte
      # re-save of the on-disk body is not a real divergence — `compile_clean/3`
      # diffs the compiled source against the disk body captured at open and must
      # leave `disk_differs` false, so the `unflushed` badge stays away (BT-2550).
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # The stub's `increment` is a disk-backed method (origin "both",
      # disk_differs false): the body the editor seeds is exactly the on-disk body.
      disk_body = "increment => self.value := self.value + 1"

      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()
      open_html = view |> element(~s(div[phx-value-selector="increment"])) |> render_click()
      assert open_html =~ "in image"
      refute open_html =~ "unflushed"

      # Save the *same* body back — a no-op edit re-compiled.
      save_html =
        view
        |> form("form[phx-submit='save_method']")
        |> render_submit(%{
          "class" => "Counter",
          "selector" => "increment",
          "source" => disk_body,
          "tab" => "method:Counter:instance:increment"
        })

      assert save_html =~ "Saved increment on Counter"
      refute save_html =~ "unflushed"
    end

    test "re-activating a compiled tab then re-saving the disk body clears unflushed (BT-2565)",
         %{conn: conn} do
      # Regression for BT-2565: re-activating a clean tab re-derives `disk_source`.
      # `disk_body_snapshot/1` yields nil while the image diverges from disk, so the
      # on-disk body captured at open used to be lost on re-activation — a later
      # re-compile of the *exact* on-disk body then fell back to the conservative
      # `unflushed` flag instead of recognising the image was back in sync.
      {:ok, view, _html} = live(owner_conn(conn), "/")

      disk_body = "increment => self.value := self.value + 1"
      new_body = "increment => self.value := self.value + 2"

      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()
      open_html = view |> element(~s(div[phx-value-selector="increment"])) |> render_click()
      refute open_html =~ "unflushed"

      # 1. Compile a *different* body — a real divergence, so `unflushed` shows and the
      #    stub now reports the method diverges from disk on a re-browse.
      save_html =
        view
        |> form("form[phx-submit='save_method']")
        |> render_submit(%{
          "class" => "Counter",
          "selector" => "increment",
          "source" => new_body,
          "tab" => "method:Counter:instance:increment"
        })

      assert save_html =~ "unflushed"

      # 2. Re-activate the (now clean) tab: routes through the `%{} = existing` branch,
      #    which re-derives `disk_source`. The image diverges from disk, so a fresh
      #    snapshot is unavailable — the captured on-disk body must survive (BT-2565).
      reactivate_html =
        view |> element(~s(div[phx-value-selector="increment"])) |> render_click()

      assert reactivate_html =~ "unflushed"

      # 3. Re-compile the *exact on-disk body*. With the snapshot preserved this is a
      #    no-op against disk → `unflushed` clears; losing it keeps the stale badge.
      reflush_html =
        view
        |> form("form[phx-submit='save_method']")
        |> render_submit(%{
          "class" => "Counter",
          "selector" => "increment",
          "source" => disk_body,
          "tab" => "method:Counter:instance:increment"
        })

      assert reflush_html =~ "Saved increment on Counter"
      refute reflush_html =~ "unflushed"
    end

    test "re-activating a compiled tab after navigating away shows the compiled body (BT-2566)",
         %{conn: conn} do
      # BT-2566: in production `browse_method_source` returns the current *image*
      # body, so after a `>>` compile the editor re-seeds the compiled body on
      # re-activation. The stub now tracks the compiled source per method, so this
      # asserts the source-content path the BT-2565 badge test left unchecked.
      #
      # Navigating to a *different* tab and back re-keys the CodeMirror host on
      # `@active_tab`, so the re-activated `increment` editor remounts and renders
      # the freshly re-seeded buffer (a same-tab re-click is `phx-update="ignore"`).
      {:ok, view, _html} = live(owner_conn(conn), "/")

      new_body = "increment => self.value := self.value + 2"

      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()
      view |> element(~s(div[phx-value-selector="increment"])) |> render_click()

      # 1. Compile a different body — the image now diverges from the on-disk stub.
      save_html =
        view
        |> form("form[phx-submit='save_method']")
        |> render_submit(%{
          "class" => "Counter",
          "selector" => "increment",
          "source" => new_body,
          "tab" => "method:Counter:instance:increment"
        })

      assert save_html =~ "unflushed"

      # 2. Navigate away to a different method tab (`value`), re-keying the editor.
      view |> element(~s(div[phx-value-selector="value"])) |> render_click()

      # 3. Re-activate `increment`: its editor remounts and the buffer is re-seeded
      #    from the live image — the compiled body, not the original on-disk stub.
      reactivate_html =
        view |> element(~s(div[phx-value-selector="increment"])) |> render_click()

      # Match the escape-safe tail of the compiled body: the rendered buffer HTML-
      # escapes `=>`, but `:= self.value + 2` survives verbatim and is unique to the
      # compiled body. Pre-fix, the re-activated buffer reverts to the on-disk stub
      # ("+ 1") and this tail is absent.
      assert reactivate_html =~ ":= self.value + 2"
    end
  end

  describe "new-method authoring (explicit affordance)" do
    test "the System Browser opens a blank new-method tab the author can save", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Select Counter (always in the stub), then use the owner-only "new method"
      # entry — the editor opens with no tab now, so authoring a brand-new method
      # is an explicit action rather than a default startup tab.
      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()

      opened =
        view
        |> element(~s(div[phx-click="new_method"][phx-value-class="Counter"]))
        |> render_click()

      # A new-method tab is open: the breadcrumb reads "new method" (no selector
      # exists yet). There is no separate selector input anymore (BT-2606) — the
      # author writes the full method in the body.
      assert opened =~ "new method"
      refute opened =~ "new-method-selector"
      assert opened =~ "Counter ▸ new"

      # Authoring + saving drives the same write-surface `save` as any method. The
      # form posts no real selector (there is no input); the save handler parses it
      # from the body signature, so the save reports it by name (BT-2606).
      saved =
        view
        |> form("form[phx-submit='save_method']")
        |> render_submit(%{
          "class" => "Counter",
          "selector" => "",
          "source" => ~s|greet => "hi"|,
          "tab" => "new:Counter:instance"
        })

      assert saved =~ "Saved greet on Counter"

      # The new-method tab is promoted to an ordinary method tab on save: the tab
      # label / breadcrumb now name the saved selector and the new-method-only
      # selector input is gone — so the author's selector survives and a second
      # ⌘S doesn't trip the empty-selector guard (the BT-review regression).
      assert saved =~ "greet"
      refute saved =~ "Counter ▸ new"
      refute saved =~ "new-method-selector"
    end

    test "saving a new method with no selector is a local validation error", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()

      view
      |> element(~s(div[phx-click="new_method"][phx-value-class="Counter"]))
      |> render_click()

      # The body has no recognizable method signature (just `=> "hi"`), so the
      # selector can't be parsed from it (BT-2606) — saving trips the local
      # validation guard.
      html =
        view
        |> form("form[phx-submit='save_method']")
        |> render_submit(%{
          "class" => "Counter",
          "selector" => "",
          "source" => ~s|=> "hi"|,
          "tab" => "new:Counter:instance"
        })

      assert html =~ "Could not parse a method signature"
    end

    test "a new-method tab derives a keyword selector from the body on save (BT-2606)", %{
      conn: conn
    } do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()

      view
      |> element(~s(div[phx-click="new_method"][phx-value-class="Counter"]))
      |> render_click()

      # No selector input — the author writes the whole method (keyword signature +
      # body) in the source, and the save handler parses the selector from it,
      # concatenating the keyword parts into `at:put:` (BT-2606).
      saved =
        view
        |> form("form[phx-submit='save_method']")
        |> render_submit(%{
          "class" => "Counter",
          "selector" => "",
          "source" => "at: i put: v => self",
          "tab" => "new:Counter:instance"
        })

      assert saved =~ "Saved at:put: on Counter"
    end

    test "a new-method tab's body survives a tab switch so its derived selector persists (BT-2606)",
         %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()
      # Open an existing method tab (to switch to) and a new-method tab.
      view |> element(~s(div[phx-value-selector="increment"])) |> render_click()

      view
      |> element(~s(div[phx-click="new_method"][phx-value-class="Counter"]))
      |> render_click()

      # Type a full method body — tracked onto the tab struct as its source.
      view
      |> form("form[phx-submit='save_method']")
      |> render_change(%{"source" => ~s|greet => "hi"|})

      # Switch away to the increment tab, then back to the new-method tab.
      view
      |> element(
        ~s(button[phx-click="tab_select"][phx-value-id="method:Counter:instance:increment"])
      )
      |> render_click()

      html =
        view
        |> element(~s(button[phx-click="tab_select"][phx-value-id="new:Counter:instance"]))
        |> render_click()

      # `sync_active` restores the body source from the tab struct, so the
      # breadcrumb re-derives the `greet` selector instead of resetting to the
      # "new method" placeholder.
      assert html =~ "greet"
    end

    test "saving a new method whose selector is already open folds into that tab", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()
      # Open the existing `increment` method tab, then a blank new-method tab.
      view |> element(~s(div[phx-value-selector="increment"])) |> render_click()

      view
      |> element(~s(div[phx-click="new_method"][phx-value-class="Counter"]))
      |> render_click()

      # Author the new method under a selector that is *already* open. On save the
      # scratch new-method tab is dropped and the existing `increment` tab focused —
      # no duplicate, no stale "Counter ▸ new" — and the save banner still shows.
      saved =
        view
        |> form("form[phx-submit='save_method']")
        |> render_submit(%{
          "class" => "Counter",
          "selector" => "increment",
          "source" => "increment => self.value := self.value + 1",
          "tab" => "new:Counter:instance"
        })

      assert saved =~ "Saved increment on Counter"
      refute saved =~ "Counter ▸ new"
      refute saved =~ "new-method-selector"
    end
  end

  describe "new-method selector derivation: modifier + -> disambiguation (BT-2625)" do
    # The breadcrumb derives the selector live from the typed body. For a method
    # *header* that begins with a modifier (`class`/`internal`/`sealed`) followed
    # by `->`, the shape is ambiguous without type context:
    #
    #   * `sealed -> Self => …`  — a *unary* method named `sealed` with a return
    #     type, so the derived selector is the modifier word.
    #   * `sealed -> arg => …`   — a `sealed` *binary* method whose selector is
    #     `->`, so the modifier is stripped and the selector is `->`.
    #
    # Disambiguate by the token after `->`: Capitalized = return Type (keep the
    # modifier as the unary selector), lowercase = binary argument (strip the
    # modifier, `->` is the selector). The breadcrumb is the only surface for this
    # client-side parse — the authoritative parse still runs server-side on save.

    # Drive the new-method tab body and return the rendered breadcrumb selector
    # span (HTML-escaped, so `->` reads as `-&gt;`).
    defp derived_breadcrumb(view, source) do
      view
      |> form("form[phx-submit='save_method']")
      |> render_change(%{"source" => source})
    end

    setup %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()

      view
      |> element(~s(div[phx-click="new_method"][phx-value-class="Counter"]))
      |> render_click()

      {:ok, view: view}
    end

    for modifier <- ["class", "internal", "sealed"] do
      test "#{modifier} => body derives `#{modifier}` (modifier word is the unary selector)",
           %{view: view} do
        html = derived_breadcrumb(view, ~s|#{unquote(modifier)} => "hi"|)
        assert html =~ ~s(<span class="mono">#{unquote(modifier)}</span>)
      end

      test "#{modifier} -> ReturnType => body derives `#{modifier}` (unary with return type)",
           %{view: view} do
        html = derived_breadcrumb(view, ~s|#{unquote(modifier)} -> Self => self|)
        assert html =~ ~s(<span class="mono">#{unquote(modifier)}</span>)
      end

      test "#{modifier} -> arg => body derives `->` (modifier-stripped binary selector)",
           %{view: view} do
        html = derived_breadcrumb(view, ~s|#{unquote(modifier)} -> other => self|)
        assert html =~ ~s(<span class="mono">-&gt;</span>)
        refute html =~ ~s(<span class="mono">#{unquote(modifier)}</span>)
      end

      test "#{modifier} other => body derives a real unary selector (modifier stripped)",
           %{view: view} do
        html = derived_breadcrumb(view, ~s|#{unquote(modifier)} increment => self|)
        assert html =~ ~s(<span class="mono">increment</span>)
        refute html =~ ~s(<span class="mono">#{unquote(modifier)}</span>)
      end
    end

    test "stacked modifiers `class sealed spawn -> Self =>` derive the unary selector (real stdlib shape)",
         %{view: view} do
      # `class sealed spawn -> Self => …` is a real stdlib header: two stacked
      # modifiers, a unary selector `spawn`, and a `Self` return type. The
      # recursion strips both modifiers and the `-> Self` return type is kept as
      # part of the `spawn` header, so the derived selector is `spawn`.
      html = derived_breadcrumb(view, ~s|class sealed spawn -> Self => self|)
      assert html =~ ~s(<span class="mono">spawn</span>)
    end

    test "stacked modifiers `class sealed -> arg =>` derive `->` (binary selector after modifiers)",
         %{view: view} do
      html = derived_breadcrumb(view, ~s|class sealed -> other => self|)
      assert html =~ ~s(<span class="mono">-&gt;</span>)
    end
  end
end
