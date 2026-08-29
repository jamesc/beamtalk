# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.TestRunner do
  @moduledoc """
  The cockpit Test Runner pane (BT-2557, epic BT-2482 Phase 1/3) — test
  catalogue discovery, running all-or-one `TestCase` subclass, loading a
  project's `test/` files, and the cross-pane dismissable-notice utility —
  extracted out of `BtAttachWeb.WorkspaceLive` (BT-3298, epic BT-3290, the
  fifth and final sequential extraction) so its `handle_event/3` /
  `handle_async/3` clauses and the discovery/run data model they drive are
  directly unit-testable instead of only reachable through a full-LiveView
  integration test. Follows the same extraction shape
  `BtAttachWeb.Live.Inspector` (BT-3291), `BtAttachWeb.Live.Dock` (BT-3295),
  `BtAttachWeb.Live.MethodEditor` (BT-3296), and
  `BtAttachWeb.Live.SystemBrowser` (BT-3297) established.

  This module owns:

    * **Test discovery** (BT-2599) — `"tests_refresh"` re-discovers the live
      image's `TestCase` subclasses + selectors via the `list_tests` `:read`
      op. Although a `:read` reflection is usually fast, it is still a
      blocking workspace RPC, so it runs off-socket in a `:test_discover`
      `start_async` task rather than stalling the LiveView process; the
      result folds in through `handle_async/3`. `discover_test_classes/2` is
      public — `BtAttachWeb.Live.Dock`'s `dock_tab`/`:test` meta-command
      lazy first-open calls it directly (a thin call-through, not a
      reimplementation).
    * **Running tests** (BT-2597) — `"run_tests"` (every loaded class) and
      `"run_test_class"` (one class, `class` rides the click) both run the
      `run_tests` `:execute` op off-socket via a `:test_op` `start_async`
      task; `"load_tests"` loads the project's `test/` files (`:execute`)
      then re-discovers the catalogue so newly-loaded classes appear
      immediately. `"open_test_method"` opens a (failing) test method in the
      method editor, reusing `BtAttachWeb.Live.MethodEditor.open_method_tab/4`
      — test selectors are always instance-side.
    * **Dismissable status notices** (BT-2612) — `"dismiss_notice"`, the
      generic dismiss for every pane's inline `.notice` banner. The key
      arrives from the client and is never turned into an atom — it is
      mapped through a fixed whitelist (`dismiss_key_to_assign/1`) covering
      assigns owned by several different panes (`tests_error` here,
      `browser_error`/`save_error`/`git_error`/… elsewhere). This is a
      cross-pane utility with no single natural owner among the five
      sequential extractions; it lands here, the last one, rather than
      staying behind in `WorkspaceLive` — which after this extraction holds
      only mount/socket wiring and the top-level `render/1` shell.

  Every workspace read/write goes through `BtAttach.Facade.dispatch/3` (ADR
  0091 Decision 3) with `BtAttachWeb.Live.RequestContext` — never a raw
  `BtAttach.Workspace`/`:rpc` call — so this module never reimplements the
  `list_tests`/`run_tests`/`load_tests` ops or the RBAC gates they ride
  (CLAUDE.md no-duplicate-implementations).

  State (`:test_classes`, `:test_results`, `:tests_error`, `:tests_running`,
  `:tests_discover_keep_error`) stays on the LiveView's own socket —
  initialised in `WorkspaceLive.bind_session/3` and mount, same as the
  Dock/Inspector/MethodEditor/SystemBrowser assigns. `WorkspaceLive` still
  owns `handle_event/3` / `handle_async/3` (`Phoenix.LiveView` callback
  contracts) and `render/1` (the Tests pane markup is woven into the dock's
  Workspace/REPL/Transcript/Changes/Git tab strip, so it does not split
  cleanly along this extraction's event boundary — see the same call in
  `Dock`'s/`Inspector`'s/`MethodEditor`'s/`SystemBrowser`'s moduledocs), but
  delegates every Test Runner event to the functions here by name — see the
  `@test_runner_events` guard clause in `WorkspaceLive`, which reads its
  event list from `__test_runner_events__/0` below (mirroring the BT-3301
  fix that keeps `WorkspaceLive` from hand-maintaining a second copy of the
  event names).
  """

  use BtAttachWeb, :html

  import Phoenix.LiveView, only: [start_async: 3, cancel_async: 3]

  require Logger

  alias BtAttach.Facade
  alias BtAttachWeb.Live.FacadeError
  alias BtAttachWeb.Live.MethodEditor
  alias BtAttachWeb.Live.RequestContext

  # ── handle_event dispatch ────────────────────────────────────────────────
  #
  # `WorkspaceLive.handle_event/3` forwards every event whose name is in
  # `@test_runner_events` (read from `__test_runner_events__/0` below) here
  # unchanged (same event name, params, socket), so each clause below is
  # exactly the body the LiveView used to run directly.
  @test_runner_events ~w(
    tests_refresh run_tests load_tests run_test_class open_test_method
    dismiss_notice
  )

  @doc false
  def __test_runner_events__, do: @test_runner_events

  # ── Test-runner pane (BT-2557) ───────────────────────────────────────────────
  #
  # The GUI equivalent of a Smalltalk Test Runner: a dock tab that lists the live
  # image's `TestCase` subclasses, runs all or a selected class through the
  # attached session (never a shelled-out `beamtalk test`), and shows per-case
  # pass/fail with failure detail — with an affordance to open a failing method
  # in the method editor. Discovery is a `:read` op (the Observer may browse the
  # catalogue); running tests is `:execute` (Owner-only, it evaluates code), so
  # the run controls are owner-gated in the template, mirroring the eval form.

  # Re-discover the test catalogue (the "refresh" affordance). The discovery is a
  # `:read` reflection op, but it is still a blocking workspace RPC — so it runs
  # off-socket via `discover_test_classes/1` (`:test_discover` `start_async`,
  # BT-2599) rather than stalling the LiveView process against a slow node. We
  # reset `test_classes` to the nil sentinel so the pane shows its "discovering"
  # state (not the misleading "No TestCase subclasses" empty-state) until the
  # `handle_async(:test_discover, …)` fold resolves.
  def handle_event("tests_refresh", _params, socket) do
    {:noreply, socket |> assign(:test_classes, nil) |> discover_test_classes()}
  end

  # Run every loaded TestCase subclass (`test-all`).
  def handle_event("run_tests", _params, socket) do
    {:noreply, run_tests(socket, nil)}
  end

  # Load the project's test/ files into the live image, then re-discover the
  # catalogue (`load_tests`, `:execute` — Owner-only). A freshly-opened project
  # holds only src/ classes, so without this the catalogue is empty (BT-2557).
  def handle_event("load_tests", _params, socket) do
    {:noreply, load_tests(socket)}
  end

  # Run a single selected test class (`test`, `class` = the row's class).
  def handle_event("run_test_class", %{"class" => class}, socket) when is_binary(class) do
    {:noreply, run_tests(socket, class)}
  end

  # Open a (failing) test method in the method editor. Test selectors are
  # instance-side, so the side is always "instance"; reuses the System Browser's
  # method-tab opener (BT-2491) so the test runner and browser share one editor.
  def handle_event("open_test_method", %{"class" => class, "selector" => selector}, socket)
      when is_binary(class) and is_binary(selector) do
    {:noreply, MethodEditor.open_method_tab(socket, class, "instance", selector)}
  end

  # Fallback clauses for the guarded test handlers: a crafted WebSocket message
  # with a missing / non-binary `class`/`selector` must be ignored, not crash the
  # socket on a FunctionClauseError before RBAC is reached (matching `save_method`,
  # `revert`, `browser_select_class`, etc.).
  def handle_event("run_test_class", _params, socket), do: {:noreply, socket}
  def handle_event("open_test_method", _params, socket), do: {:noreply, socket}

  # ── dismissable status notices (BT-2612) ────────────────────────────────────
  #
  # Generic dismiss for top-level *scalar* status assigns. The key arrives from
  # the client and is NEVER turned into an atom (`String.to_atom/1` on user input
  # is a memory/atom-table attack vector) — instead it is mapped through a fixed
  # whitelist to the assign we clear. Unknown keys are ignored (no-op), matching
  # the existing "clear to nil" convention every backing handler uses.
  def handle_event("dismiss_notice", %{"key" => key}, socket) do
    case dismiss_key_to_assign(key) do
      nil -> {:noreply, socket}
      assign_key -> {:noreply, assign(socket, assign_key, nil)}
    end
  end

  def handle_event("dismiss_notice", _params, socket), do: {:noreply, socket}

  # The dismiss-key whitelist: every scalar status assign any pane's `.notice`
  # can render, across all five extractions — `browser_error`/`native_view`'s
  # implicit errors are handled inline elsewhere, but every OTHER pane's plain
  # "here's an error string" banner dismisses through here.
  defp dismiss_key_to_assign("browser_error"), do: :browser_error
  defp dismiss_key_to_assign("output"), do: :output
  defp dismiss_key_to_assign("changes_error"), do: :changes_error
  defp dismiss_key_to_assign("git_error"), do: :git_error
  defp dismiss_key_to_assign("tests_error"), do: :tests_error
  defp dismiss_key_to_assign("save_result"), do: :save_result
  defp dismiss_key_to_assign("save_error"), do: :save_error
  defp dismiss_key_to_assign("flush_result"), do: :flush_result
  defp dismiss_key_to_assign("flush_error"), do: :flush_error
  defp dismiss_key_to_assign("bindings_error"), do: :bindings_error
  defp dismiss_key_to_assign("inspect_error"), do: :inspect_error
  defp dismiss_key_to_assign(_unknown), do: nil

  # ── handle_async dispatch ────────────────────────────────────────────────
  #
  # `WorkspaceLive.handle_async/3` forwards `:test_discover`/`:test_op` results
  # here unchanged, mirroring `handle_event/3`'s delegation above.

  # BT-2599: the off-socket test-catalogue discovery (`discover_test_classes/1` →
  # `list_tests`, `:read`) completed. We fold the raw dispatch outcome onto the
  # socket through the pure `apply_test_classes/3` helper — the same path the
  # load-tests re-discovery uses — so the async and sync callers agree. The
  # `keep_error?` flag (set by the partial-load re-discovery) rides a transient
  # assign so a *successful* re-discovery doesn't clear a partial-load banner.
  def handle_async(:test_discover, {:ok, result}, socket) do
    keep_error? = socket.assigns[:tests_discover_keep_error] || false

    {:noreply,
     socket
     |> apply_test_classes(result, keep_error?)
     |> assign(:tests_discover_keep_error, false)}
  end

  # A newer discovery (rapid double-refresh / open-then-refresh) `cancel_async`-ed
  # this one — a no-op, mirroring the `:git_load` / `:test_op` cancellation. The
  # replacement task already reset `test_classes` to the nil sentinel, so the
  # pane stays in its "discovering" state until that newer result lands.
  def handle_async(:test_discover, {:exit, :cancelled}, socket), do: {:noreply, socket}

  # The discovery task crashed/exited. Degrade to a `tests_error` rather than
  # taking down the socket (matching the `:git_load` / `:test_op` crash handlers).
  # Leave `test_classes` at the nil sentinel so the pane shows only the error —
  # not the misleading "No TestCase subclasses" empty-state — and retries on the
  # next open/refresh.
  def handle_async(:test_discover, {:exit, reason}, socket) do
    Logger.error("test discovery crashed: #{inspect(reason)}", domain: [:beamtalk, :liveview])

    {:noreply,
     assign(socket,
       test_classes: nil,
       tests_error: "Couldn't discover tests — the discovery failed unexpectedly.",
       tests_discover_keep_error: false
     )}
  end

  # BT-2597: the off-socket test run/load (`run_tests/2` / `load_tests/1`)
  # completed. The task tags its dispatch result `{:run, _}` or `{:load, _}` so
  # the right result-application path runs; either way the op is no longer in
  # flight, so the controls re-enable.
  def handle_async(:test_op, {:ok, {:run, dispatch_result}}, socket) do
    {:noreply, socket |> apply_test_result(dispatch_result) |> assign(tests_running: false)}
  end

  def handle_async(:test_op, {:ok, {:load, dispatch_result}}, socket) do
    {:noreply, socket |> apply_test_load(dispatch_result) |> assign(tests_running: false)}
  end

  # A newer run/load `cancel_async`-ed this one. Safe as a no-op only because
  # every `cancel_async(:test_op, …)` is immediately followed by a paired
  # `start_async(:test_op, …)` (in `run_tests/2` / `load_tests/1`) that has
  # already set `tests_running: true` — so the replacement task owns the running
  # state. A future standalone `cancel_async(:test_op, …)` (e.g. a Cancel button)
  # would need to reset `tests_running` itself. Mirrors the `:git_load` no-op.
  def handle_async(:test_op, {:exit, :cancelled}, socket), do: {:noreply, socket}

  def handle_async(:test_op, {:exit, reason}, socket) do
    Logger.error("test run/load crashed: #{inspect(reason)}", domain: [:beamtalk, :liveview])

    # Clear any prior run's results so a stale pass/fail table can't sit beside
    # the crash banner (a torn read) — matching the `:git_load` crash handler and
    # the `apply_test_result/2` dispatch-error path.
    {:noreply,
     assign(socket,
       tests_running: false,
       test_results: nil,
       tests_error: "The test run failed unexpectedly."
     )}
  end

  # ── Test-runner pane data source (BT-2557) ──────────────────────────────────

  # Discover the live image's TestCase subclasses + selectors (`list_tests`,
  # `:read`). Although `:read` reflection is usually fast, it is still a blocking
  # workspace RPC: against a slow/unresponsive node the ~5s timeout would stall
  # the LiveView process (first Tests-tab open / every manual Refresh). So it
  # runs off-socket in a `:test_discover` `start_async` task, mirroring the test
  # run/load `:test_op` (BT-2597) and the git panel's `:git_load` (BT-2590). A
  # rapid double-refresh / open-then-refresh `cancel_async`-es the prior probe so
  # only the latest result wins; the result lands in
  # `handle_async(:test_discover, …)`. The `test_classes` nil sentinel is
  # preserved meanwhile so the pane shows its "discovering" state rather than the
  # misleading "No TestCase subclasses" empty-state.
  # `keep_error?` is set by the load-tests re-discovery path: a partial load has
  # already populated `tests_error` with its compile-error summary, and a
  # *successful* discovery must NOT clear it (it would swallow the partial-load
  # banner). The flag rides a transient assign that `handle_async/3` consumes.
  # Public: `BtAttachWeb.Live.Dock`'s `ensure_test_classes/1` (BT-3295, the
  # `dock_tab`/`:test` meta-command lazy-load) calls it directly.
  def discover_test_classes(socket, keep_error? \\ false) do
    ctx = RequestContext.build(socket)

    socket
    |> assign(:tests_discover_keep_error, keep_error?)
    |> cancel_async(:test_discover, :cancelled)
    |> start_async(:test_discover, fn ->
      # Off the LiveView process — capture only `ctx`, never `socket`.
      Facade.dispatch(:list_tests, %{}, ctx)
    end)
  end

  # Apply a completed `list_tests` dispatch to the socket. Pure (no dispatch);
  # shared by `handle_async(:test_discover, …)` so the async path and the
  # load-tests re-discovery agree (mirrors `apply_test_result/2` and
  # `apply_git_status/2`). A dispatch failure / RBAC denial renders a
  # `tests_error` rather than crashing the pane, mirroring `apply_changes/2`.
  #
  # On success we normally clear `tests_error` (a stale failure heals), but when
  # `keep_error?` is true (a partial load is showing its compile-error summary)
  # we leave `tests_error` intact so the banner survives the re-discovery.
  defp apply_test_classes(socket, {:ok, classes}, keep_error?) when is_list(classes) do
    socket = assign(socket, :test_classes, classes)
    if keep_error?, do: socket, else: assign(socket, :tests_error, nil)
  end

  defp apply_test_classes(socket, {:error, reason}, _keep_error?),
    # Leave the catalogue as the nil sentinel (not []) so the pane shows only the
    # error — not the misleading "No TestCase subclasses" empty-state — and so
    # re-opening the tab retries discovery (a transient failure heals).
    do: assign(socket, test_classes: nil, tests_error: FacadeError.render(reason))

  defp apply_test_classes(socket, _other, _keep_error?),
    do:
      assign(socket, test_classes: nil, tests_error: FacadeError.render(:unexpected_test_result))

  # Run all tests (`class` = nil) or a single class (`run_tests`, `:execute`).
  #
  # BT-2597: the run compiles + evaluates user code on the workspace node, which
  # can take seconds for a large suite — so it runs off-socket in a `:test_op`
  # `start_async` task (mirroring the git panel's `:git_load`, BT-2590) rather
  # than blocking the LiveView process. A rapid second action `cancel_async`-es
  # the in-flight op so only the latest result wins. The result lands in
  # `handle_async(:test_op, …)`; `tests_running` disables the controls meanwhile.
  defp run_tests(socket, class) do
    ctx = RequestContext.build(socket)

    socket
    |> assign(tests_running: true, tests_error: nil)
    |> cancel_async(:test_op, :cancelled)
    |> start_async(:test_op, fn ->
      # Off the LiveView process — capture only `ctx`, never `socket`.
      {:run, Facade.dispatch(:run_tests, %{class: class}, ctx)}
    end)
  end

  # Load the project's test/ files (`load_tests`, `:execute`), then re-discover
  # the catalogue so the newly-loaded TestCase subclasses appear immediately.
  #
  # BT-2597: like `run_tests/2`, the load compiles user code, so it runs in the
  # off-socket `:test_op` task; the result lands in `handle_async(:test_op, …)`.
  defp load_tests(socket) do
    ctx = RequestContext.build(socket)

    socket
    |> assign(tests_running: true, tests_error: nil)
    |> cancel_async(:test_op, :cancelled)
    |> start_async(:test_op, fn ->
      {:load, Facade.dispatch(:load_tests, %{}, ctx)}
    end)
  end

  # Apply a completed `run_tests` dispatch to the socket. Pure (no dispatch);
  # shared by `handle_async/3` so the async path and any future sync caller agree
  # (mirrors `apply_git_status/2`). An error (incl. a non-Owner RBAC denial)
  # surfaces as `tests_error` and clears any stale results.
  defp apply_test_result(socket, {:ok, result}) when is_map(result),
    do: assign(socket, test_results: result, tests_error: nil)

  defp apply_test_result(socket, {:error, reason}),
    do: assign(socket, test_results: nil, tests_error: FacadeError.render(reason))

  defp apply_test_result(socket, _other),
    do:
      assign(socket, test_results: nil, tests_error: FacadeError.render(:unexpected_test_result))

  # Apply a completed `load_tests` dispatch: refresh the catalogue to show
  # whatever loaded, surfacing partial compile errors as `tests_error`. The
  # re-discovery is kicked off via the off-socket `:test_discover` task
  # (`discover_test_classes/2`) so the fold never blocks the LiveView process.
  #
  # We reset `test_classes` to the nil sentinel so the catalogue shows its
  # "discovering" state until the off-socket re-discovery resolves with the
  # freshly-loaded classes, and pass `keep_error?: true` so the later
  # `handle_async(:test_discover, …)` fold doesn't clear this partial-load
  # banner on a successful re-discovery.
  defp apply_test_load(socket, {:ok, %{"errors" => [_ | _] = errors}}),
    do:
      socket
      |> assign(test_classes: nil, tests_error: load_tests_error(errors))
      |> discover_test_classes(true)

  # A clean load simply re-discovers the catalogue off-socket; the
  # `handle_async(:test_discover, …)` fold clears any stale `tests_error` on
  # success (via `apply_test_classes/3`) and sets it on failure.
  defp apply_test_load(socket, {:ok, _result}),
    do: socket |> assign(test_classes: nil) |> discover_test_classes()

  defp apply_test_load(socket, {:error, reason}),
    do: assign(socket, tests_error: FacadeError.render(reason))

  defp apply_test_load(socket, _other),
    do: assign(socket, tests_error: FacadeError.render(:unexpected_test_result))

  # Summarise compile errors from a partial test load into one line. Each error
  # is a `%{"path" => ..., "message" => ...}` map (the load-project error shape).
  defp load_tests_error(errors) do
    count = length(errors)
    first = errors |> List.first() |> Map.get("message", "")
    "#{count} test file(s) failed to load: #{first}"
  end

  # Render the aggregate run duration (seconds, from the runtime TestResult) in a
  # human unit: sub-second runs in ms, longer runs in seconds. A non-number (an
  # unexpected wire shape) renders nothing rather than crashing the summary.
  # Public: `WorkspaceLive`'s render template calls it directly.
  def format_test_duration(seconds) when is_number(seconds) and seconds < 1.0 do
    "#{round(seconds * 1000)} ms"
  end

  def format_test_duration(seconds) when is_number(seconds) do
    "#{:erlang.float_to_binary(seconds * 1.0, decimals: 2)} s"
  end

  def format_test_duration(_), do: ""

  # Per-class pass/fail tally from the last run, keyed by class name, so the
  # catalogue can show "2✓ 1✗" next to each class without re-running. Returns nil
  # when there are no results yet or the class had no cases in the last run.
  # Public: `WorkspaceLive`'s render template calls it directly.
  def test_class_tally(nil, _class), do: nil

  def test_class_tally(test_results, class) when is_map(test_results) do
    cases = for t <- test_results["tests"] || [], t["class"] == class, do: t["status"]

    case cases do
      [] ->
        nil

      _ ->
        %{
          passed: Enum.count(cases, &(&1 == "pass")),
          failed: Enum.count(cases, &(&1 == "fail")),
          skipped: Enum.count(cases, &(&1 == "skip"))
        }
    end
  end

  # Short status glyph for a per-case result row. Public: `WorkspaceLive`'s
  # render template calls it directly.
  def test_status_label("pass"), do: "✓ pass"
  def test_status_label("fail"), do: "✗ fail"
  def test_status_label("skip"), do: "○ skip"
  # An unanticipated status from the runner still gets a visible "?" label rather
  # than rendering the raw atom text unadorned.
  def test_status_label(other), do: "? " <> other

  # CSS class suffix for a per-case status. Only the three known statuses carry a
  # styled rule (`.st-pass` / `.st-fail` / `.st-skip`); an unknown status falls
  # back to the neutral skip style so a row is never left unstyled with a raw
  # `st-<atom>` class that has no matching rule. Public: `WorkspaceLive`'s render
  # template calls it directly.
  def test_status_class(status) when status in ~w(pass fail skip), do: "st-" <> status
  def test_status_class(_other), do: "st-skip"
end
