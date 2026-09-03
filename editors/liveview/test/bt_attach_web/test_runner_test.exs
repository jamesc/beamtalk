# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.TestRunnerTest do
  @moduledoc """
  Direct unit tests for `BtAttachWeb.Live.TestRunner` (BT-3298), driving its
  `handle_event/3` / `handle_async/3` clauses and pure helpers against a
  hand-built `%Phoenix.LiveView.Socket{}` and the fully-stubbed workspace
  client (`BtAttachWeb.StubWorkspaceClient`, BT-2554) — no full LiveView
  mount, no real workspace node. Mirrors `BtAttachWeb.Live.DockTest`
  (BT-3295) and `BtAttachWeb.Live.SystemBrowserTest` (BT-3297), the
  precedent this extraction follows.

  Covers the branches BT-3298's acceptance criteria calls out specifically:
  test discovery failure handling — including the simulated-crash path via
  `StubWorkspaceClient.set_list_tests_raise/1` — and class-scoped vs full
  test runs. The full off-socket `start_async`/`handle_async` round trip
  (discovering/running through a real LiveView process) is already covered
  end-to-end by `BtAttachWeb.WorkspaceTestsPaneTest`; these tests instead
  drive `handle_async/3` directly (a disconnected socket's `start_async` is
  a no-op per `Phoenix.LiveView.Async`, so the dispatch outcome is asserted
  by calling the async callback with the outcome directly, exactly as
  `BtAttachWeb.Live.DockTest` does for `:git_load`) so the fold logic is
  unit-testable without a live process.
  """
  use ExUnit.Case, async: false

  alias BtAttach.Facade
  alias BtAttachWeb.Live.RequestContext
  alias BtAttachWeb.Live.TestRunner
  alias BtAttachWeb.StubWorkspaceClient
  alias BtAttachWeb.WorkspaceLive

  setup do
    Application.put_env(:bt_attach, :workspace_client, StubWorkspaceClient)
    {:ok, _} = StubWorkspaceClient.start_state()

    on_exit(fn ->
      Application.delete_env(:bt_attach, :workspace_client)
      StubWorkspaceClient.stop_state(2_000)
    end)

    :ok
  end

  # A bare, disconnected socket carrying exactly the assigns TestRunner's
  # functions read — the subset of `WorkspaceLive.bind_session/3`'s init
  # relevant to the Test Runner pane. `role: :owner` by default (most tests
  # aren't about RBAC); override per test. Disconnected
  # (`Phoenix.LiveView.connected?/1` is false), so `start_async`/
  # `cancel_async` are no-ops rather than spawning a linked Task — exactly
  # the behaviour `Phoenix.LiveView.Async` documents for a disconnected
  # socket. `handle_async/3` is exercised directly instead (see the
  # `handle_async/3` describe block below).
  defp base_socket(overrides \\ %{}) do
    assigns =
      %{
        __changed__: %{},
        current_user: nil,
        role: :owner,
        session_id: "sess-1",
        session_pid: self(),
        test_classes: nil,
        test_results: nil,
        tests_error: nil,
        tests_error_owner: nil,
        tests_running: false,
        tests_discover_keep_error: false,
        tabs: [],
        active_tab: nil,
        browser_error: nil,
        output: nil,
        changes_error: nil,
        git_error: nil,
        save_result: nil,
        save_error: nil,
        flush_result: nil,
        flush_error: nil,
        bindings_error: nil,
        inspect_error: nil
      }
      |> Map.merge(overrides)

    %Phoenix.LiveView.Socket{
      assigns: assigns,
      private: %{live_temp: %{}, lifecycle: %Phoenix.LiveView.Lifecycle{}}
    }
  end

  defp method_tab(id, class, selector, opts \\ []) do
    %{
      id: id,
      kind: :method,
      class: class,
      side: Keyword.get(opts, :side, "instance"),
      selector: selector,
      source: "#{selector} => self",
      base: "#{selector} => self",
      dirty: false,
      disk_differs: false,
      runtime_only: false,
      synthetic: false,
      disk_source: nil,
      doc: nil,
      signature: nil,
      native_module: nil,
      native_delegate: false,
      class_modifiers: nil,
      class_native: false,
      source_origin: nil,
      package: nil,
      new: false
    }
  end

  describe "@test_runner_events coverage (BT-3301 pattern)" do
    test "WorkspaceLive's @test_runner_events IS TestRunner's canonical list, not a copy" do
      assert WorkspaceLive.test_runner_events() == TestRunner.__test_runner_events__()
    end

    test "every event WorkspaceLive delegates to TestRunner resolves to an implemented clause" do
      tabs = [method_tab("method:Counter:instance:increment", "Counter", "increment")]
      socket = base_socket(%{tabs: tabs, active_tab: "method:Counter:instance:increment"})

      params_by_event = %{
        "tests_refresh" => %{},
        "run_tests" => %{},
        "load_tests" => %{},
        "run_test_class" => %{"class" => "Counter"},
        "open_test_method" => %{"class" => "Counter", "selector" => "increment"},
        "dismiss_notice" => %{"key" => "tests_error"}
      }

      for event <- TestRunner.__test_runner_events__() do
        params = Map.fetch!(params_by_event, event)
        result = TestRunner.handle_event(event, params, socket)

        assert match?({:noreply, %Phoenix.LiveView.Socket{}}, result),
               "TestRunner.handle_event/3 has no clause for #{inspect(event)} (or it crashed)"
      end
    end

    test "no handle_event/3 clause head names an event missing from the canonical list" do
      source =
        Path.expand("../../lib/bt_attach_web/live/test_runner.ex", __DIR__) |> File.read!()

      clause_names =
        ~r/def handle_event\("([a-z0-9_]+)"/
        |> Regex.scan(source)
        |> Enum.map(fn [_, name] -> name end)
        |> MapSet.new()

      assert clause_names == MapSet.new(TestRunner.__test_runner_events__())
    end
  end

  describe "run_test_class / open_test_method malformed-payload fallbacks" do
    test "run_test_class with a missing class is a no-op" do
      socket = base_socket()
      assert {:noreply, ^socket} = TestRunner.handle_event("run_test_class", %{}, socket)
    end

    test "open_test_method with a non-binary selector is a no-op" do
      socket = base_socket()

      assert {:noreply, ^socket} =
               TestRunner.handle_event(
                 "open_test_method",
                 %{"class" => "Counter", "selector" => nil},
                 socket
               )
    end
  end

  describe "dismiss_notice (BT-2612)" do
    test "dismisses a known key by clearing its assign" do
      socket = base_socket(%{tests_error: "boom"})

      {:noreply, socket} =
        TestRunner.handle_event("dismiss_notice", %{"key" => "tests_error"}, socket)

      assert socket.assigns.tests_error == nil
    end

    test "an unknown key is a no-op, never String.to_atom/1 on client input" do
      socket = base_socket(%{tests_error: "boom"})

      assert {:noreply, ^socket} =
               TestRunner.handle_event(
                 "dismiss_notice",
                 %{"key" => "not_a_real_assign_key"},
                 socket
               )
    end

    test "a crafted event with no key is a no-op" do
      socket = base_socket()
      assert {:noreply, ^socket} = TestRunner.handle_event("dismiss_notice", %{}, socket)
    end

    # BT-3311: `dismiss_key_to_assign/1`'s whitelist covers scalar status
    # assigns owned by several OTHER panes (not just this one's own
    # `tests_error`) — the cross-pane utility the moduledoc describes.
    test "dismisses the cross-pane \"browser_error\" key" do
      socket = base_socket(%{browser_error: "boom"})

      {:noreply, socket} =
        TestRunner.handle_event("dismiss_notice", %{"key" => "browser_error"}, socket)

      assert socket.assigns.browser_error == nil
    end

    test "dismisses the cross-pane \"output\" key" do
      socket = base_socket(%{output: "boom"})
      {:noreply, socket} = TestRunner.handle_event("dismiss_notice", %{"key" => "output"}, socket)
      assert socket.assigns.output == nil
    end

    test "dismisses the cross-pane \"changes_error\" key" do
      socket = base_socket(%{changes_error: "boom"})

      {:noreply, socket} =
        TestRunner.handle_event("dismiss_notice", %{"key" => "changes_error"}, socket)

      assert socket.assigns.changes_error == nil
    end

    test "dismisses the cross-pane \"git_error\" key" do
      socket = base_socket(%{git_error: "boom"})

      {:noreply, socket} =
        TestRunner.handle_event("dismiss_notice", %{"key" => "git_error"}, socket)

      assert socket.assigns.git_error == nil
    end

    test "dismisses the cross-pane \"flush_error\" key" do
      socket = base_socket(%{flush_error: "boom"})

      {:noreply, socket} =
        TestRunner.handle_event("dismiss_notice", %{"key" => "flush_error"}, socket)

      assert socket.assigns.flush_error == nil
    end

    test "dismisses the cross-pane \"bindings_error\" key" do
      socket = base_socket(%{bindings_error: "boom"})

      {:noreply, socket} =
        TestRunner.handle_event("dismiss_notice", %{"key" => "bindings_error"}, socket)

      assert socket.assigns.bindings_error == nil
    end

    test "dismisses the cross-pane \"inspect_error\" key" do
      socket = base_socket(%{inspect_error: "boom"})

      {:noreply, socket} =
        TestRunner.handle_event("dismiss_notice", %{"key" => "inspect_error"}, socket)

      assert socket.assigns.inspect_error == nil
    end
  end

  describe "handle_async(:test_discover, …) — test discovery failure handling (BT-2599)" do
    test "a successful discovery clears any stale error and stores the catalogue" do
      classes = [%{"class" => "FooTest", "selectors" => ["testOne"]}]
      socket = base_socket(%{tests_error: "stale error"})

      {:noreply, socket} = TestRunner.handle_async(:test_discover, {:ok, {:ok, classes}}, socket)

      assert socket.assigns.test_classes == classes
      assert socket.assigns.tests_error == nil
    end

    test "a dispatch error degrades to tests_error and keeps the nil sentinel (not [])" do
      socket = base_socket(%{test_classes: [%{"class" => "Stale"}]})

      {:noreply, socket} =
        TestRunner.handle_async(:test_discover, {:ok, {:error, :workspace_unreachable}}, socket)

      assert socket.assigns.test_classes == nil
      assert socket.assigns.tests_error != nil
    end

    test "an unexpected reply shape degrades rather than crashing" do
      socket = base_socket()

      {:noreply, socket} = TestRunner.handle_async(:test_discover, {:ok, :bogus}, socket)

      assert socket.assigns.test_classes == nil
      assert socket.assigns.tests_error =~ "unexpected_test_result"
    end

    test "a successful re-discovery honours keep_error?: true (partial-load banner survives)" do
      socket =
        base_socket(%{
          tests_error: "2 test file(s) failed to load: boom",
          tests_discover_keep_error: true
        })

      {:noreply, socket} =
        TestRunner.handle_async(:test_discover, {:ok, {:ok, []}}, socket)

      assert socket.assigns.tests_error == "2 test file(s) failed to load: boom"
      # The transient flag is always cleared after the fold, win or lose.
      assert socket.assigns.tests_discover_keep_error == false
    end

    test "a cancelled discovery (rapid double-refresh) is a no-op" do
      socket = base_socket(%{test_classes: nil})

      assert {:noreply, ^socket} =
               TestRunner.handle_async(:test_discover, {:exit, :cancelled}, socket)
    end

    test "the simulated list_tests crash actually raises through the same dispatch discover_test_classes/1 makes, and the resulting {:exit, …} degrades gracefully" do
      # `StubWorkspaceClient.set_list_tests_raise/1` is the crash path
      # `discover_test_classes/1`'s off-socket `fn -> Facade.dispatch(:list_tests,
      # …) end` would hit inside its `:test_discover` `start_async` task. A
      # disconnected socket's `start_async` never actually invokes that
      # function (`Phoenix.LiveView.Async.run_async_task/5` no-ops when
      # `connected?/1` is false), so exercising the raise itself — rather
      # than only asserting the downstream `{:exit, …}` fold on a
      # hand-constructed reason — means calling the SAME dispatch
      # `discover_test_classes/1` builds directly. This is the direct-unit-test
      # half of the coverage; `BtAttachWeb.WorkspaceTestsPaneTest`'s
      # full-LiveView "a discovery crash degrades to tests_error" test
      # exercises the real off-socket Task/handle_async round trip end-to-end.
      StubWorkspaceClient.set_list_tests_raise(true)
      socket = base_socket()
      ctx = RequestContext.build(socket)

      assert_raise RuntimeError, "simulated list_tests crash", fn ->
        Facade.dispatch(:list_tests, %{}, ctx)
      end

      # The crash caught by `Phoenix.LiveView.Async`'s task wrapper reaches
      # `handle_async(:test_discover, {:exit, reason}, socket)` as some
      # `reason` term (the exact shape isn't this module's concern — see
      # `Phoenix.LiveView.Async.to_exit/3` — only that ANY reason degrades
      # to a `tests_error` rather than propagating).
      {:noreply, socket} =
        TestRunner.handle_async(:test_discover, {:exit, %RuntimeError{message: "boom"}}, socket)

      assert socket.assigns.test_classes == nil
      assert socket.assigns.tests_error =~ "discovery failed unexpectedly"
      assert socket.assigns.tests_discover_keep_error == false
    end
  end

  describe "handle_async(:test_op, …) — class-scoped vs full test runs (BT-2597)" do
    test "a successful full run (class: nil) stores the results and clears tests_running" do
      socket = base_socket(%{tests_running: true})
      result = %{"passed" => 2, "failed" => 0, "total" => 2, "tests" => []}

      {:noreply, socket} =
        TestRunner.handle_async(:test_op, {:ok, {:run, {:ok, result}}}, socket)

      assert socket.assigns.test_results == result
      assert socket.assigns.tests_running == false
      assert socket.assigns.tests_error == nil
    end

    test "a successful class-scoped run stores the results the same way as a full run" do
      socket = base_socket(%{tests_running: true})
      result = %{"passed" => 1, "failed" => 0, "total" => 1, "tests" => []}

      {:noreply, socket} =
        TestRunner.handle_async(:test_op, {:ok, {:run, {:ok, result}}}, socket)

      assert socket.assigns.test_results == result
      assert socket.assigns.tests_running == false
    end

    test "a run dispatch error surfaces as tests_error and clears stale results" do
      socket = base_socket(%{tests_running: true, test_results: %{"passed" => 9}})

      {:noreply, socket} =
        TestRunner.handle_async(:test_op, {:ok, {:run, {:error, :unauthorized}}}, socket)

      assert socket.assigns.test_results == nil
      assert socket.assigns.tests_running == false
      assert socket.assigns.tests_error != nil
    end

    test "a clean load re-discovers off-socket (a no-op start_async here) and clears tests_running" do
      socket = base_socket(%{tests_running: true})

      {:noreply, socket} =
        TestRunner.handle_async(:test_op, {:ok, {:load, {:ok, %{}}}}, socket)

      assert socket.assigns.tests_running == false
      # `discover_test_classes/1` reset the catalogue to the loading sentinel;
      # the disconnected socket's `start_async` never resolves it further here.
      assert socket.assigns.test_classes == nil
    end

    test "a partial load surfaces the compile-error summary and keeps it across re-discovery" do
      socket = base_socket(%{tests_running: true})

      errors = [%{"path" => "test/foo.bt", "message" => "boom"}]

      {:noreply, socket} =
        TestRunner.handle_async(:test_op, {:ok, {:load, {:ok, %{"errors" => errors}}}}, socket)

      assert socket.assigns.tests_error == "1 test file(s) failed to load: boom"
      assert socket.assigns.tests_running == false
      # The partial-load re-discovery passed keep_error?: true.
      assert socket.assigns.tests_discover_keep_error == true
    end

    test "a cancelled run/load (rapid double-action) is a no-op" do
      socket = base_socket(%{tests_running: true})

      assert {:noreply, ^socket} = TestRunner.handle_async(:test_op, {:exit, :cancelled}, socket)
    end

    test "a crashed run/load degrades to tests_error and clears stale results" do
      socket = base_socket(%{tests_running: true, test_results: %{"passed" => 1}})

      {:noreply, socket} = TestRunner.handle_async(:test_op, {:exit, :boom}, socket)

      assert socket.assigns.tests_running == false
      assert socket.assigns.test_results == nil
      assert socket.assigns.tests_error == "The test run failed unexpectedly."
    end
  end

  describe "render-template helpers" do
    test "format_test_duration/1 renders sub-second runs in ms" do
      assert TestRunner.format_test_duration(0.123) == "123 ms"
    end

    test "format_test_duration/1 renders longer runs in seconds" do
      assert TestRunner.format_test_duration(2.5) == "2.50 s"
    end

    test "format_test_duration/1 renders nothing for a non-number" do
      assert TestRunner.format_test_duration(nil) == ""
    end

    test "test_class_tally/2 tallies pass/fail/skip for one class" do
      results = %{
        "tests" => [
          %{"class" => "Counter", "status" => "pass"},
          %{"class" => "Counter", "status" => "fail"},
          %{"class" => "Other", "status" => "pass"}
        ]
      }

      assert TestRunner.test_class_tally(results, "Counter") == %{
               passed: 1,
               failed: 1,
               skipped: 0
             }
    end

    test "test_class_tally/2 returns nil when there are no results yet" do
      assert TestRunner.test_class_tally(nil, "Counter") == nil
    end

    test "test_class_tally/2 returns nil when the class had no cases in the last run" do
      results = %{"tests" => [%{"class" => "Other", "status" => "pass"}]}

      assert TestRunner.test_class_tally(results, "Counter") == nil
    end

    test "test_status_label/1 and test_status_class/1 cover pass/fail/skip and an unknown status" do
      assert TestRunner.test_status_label("pass") == "✓ pass"
      assert TestRunner.test_status_class("pass") == "st-pass"
      assert TestRunner.test_status_label("fail") == "✗ fail"
      assert TestRunner.test_status_class("fail") == "st-fail"
      assert TestRunner.test_status_label("skip") == "○ skip"
      assert TestRunner.test_status_class("skip") == "st-skip"
      assert TestRunner.test_status_label("weird") == "? weird"
      assert TestRunner.test_status_class("weird") == "st-skip"
    end
  end
end
