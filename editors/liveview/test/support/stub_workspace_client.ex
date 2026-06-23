# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.StubWorkspaceClient do
  @moduledoc """
  A fully-stubbed workspace client for integration-testing the LiveView mount
  and UI flow without a real workspace node (BT-2554).

  Implements the complete workspace client interface: connection management
  (connect, start_session), facade dispatch ops (eval, save, flush, changes,
  browse, etc.), and pure utility functions (render_term, render_error, etc.).

  State is held in a per-test Agent so that multiple LiveView mounts (eval on
  one, browse on another) share the same simulated workspace.
  """

  defmodule State do
    @moduledoc false
    defstruct defined_classes: MapSet.new(),
              changes: %{},
              # BT-2566: the compiled body per `{class, selector}`, recorded on
              # `save_method/3`. In production `browse_method_source` returns the
              # current *image* body, which after a `>>` compile is the compiled
              # body — so the stub serves this over the hardcoded on-disk body.
              compiled_sources: %{},
              # BT-2655: an in-image class-definition skeleton override per class,
              # recorded so a test can make the image's `browse_class_definition`
              # body diverge from the hardcoded on-disk skeleton (modelling a `>>`
              # header recompile / external edit). `reload_file/1` drops the override
              # for the reverted class so the served body falls back to disk.
              compiled_definitions: %{},
              # BT-2590: the simulated `autoflush` flag (default off) and seeded
              # git panel results, so a test can exercise the post-save refresh
              # gate and the async git load without a real workspace node.
              autoflush: false,
              git_status: nil,
              git_log: nil,
              # BT-2597: seeded test-runner pane results so the async run/load
              # path (`start_async(:test_op, …)` → `handle_async`) can be
              # exercised without a real workspace node. `nil` = use the canned
              # default; a test can override to drive error/partial-load paths.
              test_classes: nil,
              run_tests_result: nil,
              load_tests_result: nil,
              # BT-2598: seeded `reload_file` result (nil = derive class from path)
              # so a test can drive the revert reload error path.
              reload_file_result: nil,
              # BT-2598: `{class, selector}` pairs forced to report `disk_differs`
              # WITHOUT a ChangeLog entry — models an image that differs from disk
              # because disk changed under it (an external edit), distinct from an
              # unflushed in-memory edit (which `changes` carries and the revert
              # guard blocks on). A `reload_file` for the class clears them.
              forced_disk_differs: MapSet.new(),
              # BT-2634: seeded workspace bindings (`{name, term}` pairs) so a
              # LiveView test can establish a supervisor binding and drive the full
              # Inspect → children-render → drill-through path against the stub. `[]`
              # = no bindings (the default, matching a fresh session).
              bindings: [],
              # BT-2636: seeded net-vs-disk diff strings per `{class, selector}`,
              # surfaced by `change_history` so the Changes-pane disclosure caret +
              # structured diff render can be driven without a real workspace.
              change_diffs: %{},
              # BT-2619: a one-shot barrier for the async mount-load reads. When a
              # test arms the gate (`arm_mount_gate/0`), the FIRST `browse_classes`
              # call (the mount-load task's first read) blocks until the test calls
              # `release_mount_gate/0` — letting the test deterministically deliver a
              # live push BEFORE the async mount fold runs, the BT-2619 race. The gate
              # disarms itself on first use, so the subsequent push-refresh reads (and
              # the now-released mount reads) never block.
              mount_gate: nil,
              mount_gate_consumed: false,
              # BT-2661: optional full override for `browse_classes` so a test can
              # drive the origin-filter default against a custom set (e.g. a
              # stdlib-only workspace for the empty-project → "all" fallback). `nil`
              # uses the canned `base ++ defined_classes` rows.
              browse_classes_override: nil,
              calls: []
  end

  @doc "Start the state Agent for a test. Call in setup."
  def start_state do
    Agent.start(fn -> %State{} end, name: __MODULE__.State)
  end

  @doc "Stop the state Agent. Call in on_exit."
  def stop_state(timeout \\ 100) do
    case Agent.stop(__MODULE__.State, :normal, timeout) do
      :ok -> :ok
      {:error, :no_process} -> :ok
      {:error, :timeout} -> raise "StubWorkspaceClient.State timed out shutting down"
    end
  end

  defp get(key), do: Agent.get(__MODULE__.State, &Map.get(&1, key))
  defp put(key, val), do: Agent.update(__MODULE__.State, &Map.put(&1, key, val))
  defp update(key, fun), do: Agent.update(__MODULE__.State, &Map.update!(&1, key, fun))
  defp record(entry), do: update(:calls, &[entry | &1])

  # ── Connection management ────────────────────────────────────────────────

  def connect, do: :ok

  def start_session(_session_id, _meta) do
    # Returns a stub pid. Safe only when the caller uses a nil token, which
    # short-circuits SessionRegistry.register/3 before any monitor is set up.
    # A non-nil token would cause the immediately-exiting process to trigger a
    # :DOWN drop — see register/3 guard in session_registry.ex.
    spawn(fn -> :ok end)
  end

  def session_alive?(_pid), do: true
  def close_session(_pid), do: :ok

  # ── Subscription ops ─────────────────────────────────────────────────────

  def subscribe_transcript(_pid), do: :ok
  def unsubscribe_transcript(_pid), do: :ok
  def subscribe_bindings(_pid), do: :ok
  def unsubscribe_bindings(_pid), do: :ok
  # BT-2598: the class-lifecycle push stream. Records the subscribe so a test can
  # assert it is wired at mount; returns :ok (the real facade's success reply).
  def subscribe_classes(_pid), do: record({:subscribe_classes}) && :ok
  def unsubscribe_classes(_pid), do: :ok
  def subscribe_object_changes(_term, _pid), do: :ok
  def unsubscribe_object_changes(_term, _pid), do: :ok

  # ── Eval + session ops ───────────────────────────────────────────────────

  def eval(_pid, code) do
    case extract_class_name(code) do
      {:ok, class_name} ->
        update(:defined_classes, &MapSet.put(&1, class_name))
        {:ok, "class #{class_name} defined", "", []}

      :error ->
        {:ok, "stub-result", "", []}
    end
  end

  @doc """
  Test helper (BT-2619): drop all eval-defined classes so a subsequent
  `browse_classes` returns only the canned base rows. Lets a test make the mount
  load's source read STALER than a fresher live push.
  """
  def clear_defined_classes, do: put(:defined_classes, MapSet.new())

  def list_bindings(_pid) do
    if get(:fail_bindings), do: {:error, :unreachable}, else: get(:bindings) || []
  end

  @doc "Test helper (BT-2634): seed the workspace bindings the bindings pane reads."
  def put_bindings(pairs) when is_list(pairs), do: put(:bindings, pairs)

  @doc """
  Test helper (BT-2619): force `list_bindings/1` to return `{:error, :unreachable}`
  so a test can drive the bindings push-refresh ERROR path (the "errored early push
  must not lock out a successful mount fold" edge case).
  """
  def fail_bindings(flag) when is_boolean(flag), do: put(:fail_bindings, flag)

  # BT-2634: a supervisor's inspection content is its children / supervision tree
  # (drillable child handles), not actor instance vars. The stub keys variants off
  # the supervisor's class so tests can exercise each path:
  #
  #   * `EmptySup`  → a live supervisor with no running children (empty-state path)
  #   * `DeadSup`   → a dead/unreachable supervisor (structured error path)
  #   * any other   → a representative tree: a worker actor (drillable, leaf), a
  #     nested supervisor child (drillable, re-inspectable as its own supervisor),
  #     and a foreign OTP process (rendered but NOT drillable, `handle: :null`).
  def inspect_value({:beamtalk_supervisor, class, _module, pid} = _term) when is_pid(pid) do
    case to_string(class) do
      "EmptySup" ->
        {:ok, {:supervisor_children, []}}

      "DeadSup" ->
        {:error,
         {:beamtalk_error, :stale_handle, :Inspector, :children, "supervisor is not alive"}}

      _ ->
        {:ok, {:supervisor_children, stub_supervisor_children()}}
    end
  end

  def inspect_value(_term), do: {:ok, %{}}

  # BT-2634: representative supervisor children (binary-keyed rows, mirroring
  # `beamtalk_process_navigation:child_handles/1`). The nested supervisor child
  # carries a live `{:beamtalk_supervisor, …}` handle so a drill re-inspects it as
  # its own supervisor; the worker carries a `{:beamtalk_object, …}` handle; the
  # foreign process has `handle: :null` so it renders but is not drillable.
  defp stub_supervisor_children do
    [
      %{
        "label" => "Counter",
        "className" => "Counter",
        "kind" => "beamtalkActor",
        "pid" => "<0.310.0>",
        "registeredName" => :null,
        "childCount" => 0,
        "isSupervisor" => false,
        "handle" => {:beamtalk_object, :Counter, :"Elixir.Counter", stub_pid()}
      },
      %{
        "label" => "WorkerPool",
        "className" => "WorkerPool",
        "kind" => "beamtalkSupervisor",
        "pid" => "<0.311.0>",
        "registeredName" => :WorkerPool,
        "childCount" => 2,
        "isSupervisor" => true,
        "handle" => {:beamtalk_supervisor, :WorkerPool, :"Elixir.WorkerPool", stub_pid()}
      },
      %{
        "label" => "logger_std_h",
        "className" => :null,
        "kind" => "otpProcess",
        "pid" => "<0.312.0>",
        "registeredName" => :logger_std_h,
        "childCount" => 0,
        "isSupervisor" => false,
        "handle" => :null
      }
    ]
  end

  # A stable, alive pid for stub child handles (the test node's own pid — always
  # alive, so `inspectable?/1`'s `is_pid/1` guard passes and a drill is exercised).
  defp stub_pid, do: self()

  @doc """
  Stub supervisor children listing (BT-2634). Mirrors
  `BtAttach.Workspace.supervisor_children/1`: returns the representative child rows
  for a live supervisor pid.
  """
  def supervisor_children(sup_pid) when is_pid(sup_pid),
    do: {:ok, stub_supervisor_children()}

  defp extract_class_name(code) do
    case Regex.run(~r/(?:Actor|Object)\s+subclass:\s+(\w+)/, code) do
      [_, name] -> {:ok, name}
      _ -> :error
    end
  end

  # ── Write-surface: save / flush / changes ─────────────────────────────────

  def save_method(class, selector, source) do
    record({:save, class, selector})

    update(:changes, fn changes ->
      Map.put(changes, {class, selector}, "src/#{Macro.underscore(class)}.bt")
    end)

    # BT-2566: remember the compiled body so a later `browse_method_source/3`
    # returns the image body (as production does) rather than the on-disk stub.
    update(:compiled_sources, fn sources ->
      Map.put(sources, {class, selector}, source)
    end)

    {:ok, class}
  end

  def flush do
    changes = get(:changes)
    put(:changes, %{})

    summary = %{
      flushed: map_size(changes),
      files: Map.values(changes) |> Enum.uniq(),
      conflicts: [],
      skipped: []
    }

    {:ok, summary}
  end

  def change_history do
    diffs = get(:change_diffs) || %{}

    get(:changes)
    |> Enum.map(fn {{class, selector}, _file} ->
      %{
        class: class,
        selector: selector,
        kind: "method",
        intent: "durable",
        flushable: true,
        flushed: false,
        author_kind: "liveview",
        # BT-2636: seeded net-vs-disk diff so a test can drive the Changes-pane
        # disclosure caret + structured `unified_diff` render. nil by default.
        diff: Map.get(diffs, {class, selector})
      }
    end)
    |> Enum.reverse()
  end

  @doc "Test helper: seed the net-vs-disk diff string for a `{class, selector}`."
  def set_change_diff(class, selector, diff) when is_binary(diff) do
    update(:change_diffs, fn diffs -> Map.put(diffs || %{}, {class, selector}, diff) end)
  end

  # BT-2590: the workspace `autoflush` flag, read at mount. Defaults to `false`
  # (matching the production default), so a per-method save in a test patches the
  # image only and the post-save git refresh is skipped — unless a test flips it
  # via `set_autoflush/1`.
  def autoflush, do: get(:autoflush) || false

  @doc "Test helper: set the simulated `autoflush` flag."
  def set_autoflush(flag) when is_boolean(flag), do: put(:autoflush, flag)

  # ── New File + Revert ────────────────────────────────────────────────────

  def new_class(_source, path), do: {:ok, path}
  def revert(class, _selector), do: {:ok, class}

  # BT-2598: reload a reverted .bt file from disk into the image. Records the call
  # (so a test can assert the revert reloads) and returns the class name(s) the
  # path defines — derived from the path basename, mirroring the real reload's
  # `{:ok, [class_name]}` shape. A seeded override drives the error/refresh paths.
  def reload_file(path) do
    record({:reload_file, path})

    # Model the disk→image reload: the image is reset to the on-disk body, so any
    # tracked in-memory `>>` patch for this class is dropped and a subsequent
    # `browse_method_source` serves the on-disk stub again (image == disk).
    class = class_name_for_path(path)

    update(:compiled_sources, fn sources ->
      sources |> Enum.reject(fn {{c, _sel}, _src} -> c == class end) |> Map.new()
    end)

    # BT-2655: drop any in-image class-definition override for the reverted class so
    # `browse_class_definition` serves the on-disk skeleton again (image == disk).
    update(:compiled_definitions, fn defs -> Map.delete(defs, class) end)

    update(:changes, fn changes ->
      changes |> Enum.reject(fn {{c, _sel}, _file} -> c == class end) |> Map.new()
    end)

    update(:forced_disk_differs, fn forced ->
      forced |> Enum.reject(fn {c, _sel} -> c == class end) |> MapSet.new()
    end)

    get(:reload_file_result) || {:ok, [class]}
  end

  @doc "Test helper: seed the simulated `reload_file` result."
  def set_reload_file(result), do: put(:reload_file_result, result)

  @doc """
  Test helper: force `browse_method_source` to report `disk_differs: true` for
  `{class, selector}` WITHOUT a ChangeLog entry (BT-2598) — an image that differs
  from disk because disk changed under it. A `reload_file/1` for the class clears
  it. Unlike `seed_change/2`, this does NOT trip the revert pending-edit guard.
  """
  def seed_disk_differs(class, selector) do
    update(:forced_disk_differs, &MapSet.put(&1, {class, selector}))
  end

  @doc """
  Test helper: seed a pending ChangeLog entry for `{class, selector}` (BT-2598) —
  a saved-but-unflushed in-memory edit. This DOES appear in `change_history/0` and
  trips the revert pending-edit guard.
  """
  def seed_change(class, selector) do
    update(:changes, &Map.put(&1, {class, selector}, "src/#{Macro.underscore(class)}.bt"))
  end

  @doc """
  Test helper (BT-2655): override the in-image method body served by
  `browse_method_source/3` for `{class, selector}` (modelling a `>>` recompile /
  external edit reloaded into the image). `reload_file/1` drops it for the class so
  a revert's reload serves the on-disk stub body again (image == disk).
  """
  def update_compiled_source(class, selector, source) when is_binary(source) do
    update(:compiled_sources, &Map.put(&1, {class, selector}, source))
  end

  # `src/git_gate3.bt` -> "GitGate3": strip dir + extension, then CamelCase from the
  # snake_case basename the LiveView derives a path from (`Macro.underscore`).
  defp class_name_for_path(path) do
    path
    |> Path.basename(".bt")
    |> Macro.camelize()
  end

  # ── Git panel (BT-2586, BT-2590) ──────────────────────────────────────────
  # A clean working tree by default; a test can seed status/log via the helpers.

  def git_status do
    record({:git_status})
    get(:git_status) || {:ok, default_git_status()}
  end

  def git_log(count) do
    record({:git_log, count})
    get(:git_log) || {:ok, []}
  end

  def git_diff(path) do
    record({:git_diff, path})
    # BT-2636: a seeded diff drives the Git-pane structured `unified_diff` render;
    # the default empty result keeps the existing "No textual diff" empty state.
    get(:git_diff_result) || {:ok, %{worktree: "", staged: ""}}
  end

  @doc "Test helper: seed the `git_diff/1` result (e.g. a unified-diff string)."
  def set_git_diff(result), do: put(:git_diff_result, result)
  def git_stage(path), do: record({:git_stage, path}) && {:ok, nil}
  def git_unstage(path), do: record({:git_unstage, path}) && {:ok, nil}
  def git_commit(message), do: record({:git_commit, message}) && {:ok, nil}
  def git_revert_file(path), do: record({:git_revert_file, path}) && {:ok, nil}

  @doc "Test helper: seed the simulated `git_status` result."
  def set_git_status(result), do: put(:git_status, result)

  @doc "Test helper: seed the simulated `git_log` result."
  def set_git_log(result), do: put(:git_log, result)

  defp default_git_status do
    %{branch: "main", upstream: nil, ahead: 0, behind: 0, files: []}
  end

  # ── Test-runner pane (BT-2557, async in BT-2597) ──────────────────────────
  # Canned catalogue + run/load results so the Tests pane's `start_async`
  # (`:test_op`) path can be driven without a real workspace node. Each can be
  # overridden via the `set_*` helpers to exercise error / partial-load paths.

  def list_tests do
    record({:list_tests})

    # BT-2599: simulate a discovery crash so the `:test_discover` async task hits
    # its `{:exit, …}` clause (degrade-to-error rather than crash the socket).
    if get(:list_tests_raise), do: raise("simulated list_tests crash")

    get(:test_classes) || {:ok, default_test_classes()}
  end

  def run_tests(class) do
    record({:run_tests, class})
    get(:run_tests_result) || {:ok, default_run_result()}
  end

  def load_tests do
    record({:load_tests})
    get(:load_tests_result) || {:ok, default_load_result()}
  end

  @doc "Test helper: seed the simulated `list_tests` result."
  def set_test_classes(result), do: put(:test_classes, result)

  @doc "Test helper: make `list_tests` raise (drives the `:test_discover` `{:exit, …}` path)."
  def set_list_tests_raise(flag), do: put(:list_tests_raise, flag)

  @doc "Test helper: seed the simulated `run_tests` result."
  def set_run_tests(result), do: put(:run_tests_result, result)

  @doc "Test helper: seed the simulated `load_tests` result."
  def set_load_tests(result), do: put(:load_tests_result, result)

  defp default_test_classes do
    [%{"class" => "StubDemoTest", "selectors" => ["testOne", "testTwo"]}]
  end

  defp default_run_result do
    %{
      "total" => 2,
      "passed" => 1,
      "failed" => 1,
      "skipped" => 0,
      "duration" => 0.05,
      "tests" => [
        %{"name" => "testOne", "class" => "StubDemoTest", "status" => "pass", "detail" => ""},
        %{"name" => "testTwo", "class" => "StubDemoTest", "status" => "fail", "detail" => "boom"}
      ]
    }
  end

  defp default_load_result do
    %{"classes" => ["StubDemoTest"], "errors" => [], "summary" => "Reloaded 1 of 1 files"}
  end

  # ── Browse ops ───────────────────────────────────────────────────────────

  @doc """
  Test helper (BT-2619): arm the one-shot mount-load barrier. The next
  `browse_classes` call (the async mount-load task's first read) will block until
  `release_mount_gate/0` is called, so a test can deliver a live push BEFORE the
  async mount fold runs and assert the fold does not clobber it.
  """
  def arm_mount_gate do
    # `start_link` (not `start`) so the gate is linked to the test process and dies
    # with it — no orphaned Agent accumulating across the suite.
    {:ok, gate} = Agent.start_link(fn -> false end)
    put(:mount_gate, gate)
    put(:mount_gate_consumed, false)
    :ok
  end

  @doc "Test helper (BT-2619): release the armed mount-load barrier (see `arm_mount_gate/0`)."
  def release_mount_gate do
    case get(:mount_gate) do
      nil -> :ok
      gate -> if Process.alive?(gate), do: Agent.update(gate, fn _ -> true end), else: :ok
    end
  end

  # Block on the armed gate (if any). Only the FIRST read to reach the gate waits;
  # `:mount_gate_consumed` marks it spent so later reads (the push refresh, the
  # released mount reads) pass straight through. The gate ref itself is kept until
  # the test releases it, so `release_mount_gate/0` can find it.
  defp await_mount_gate do
    case get(:mount_gate) do
      nil ->
        :ok

      gate ->
        consumed = get(:mount_gate_consumed)

        if consumed do
          :ok
        else
          put(:mount_gate_consumed, true)
          wait_for_release(gate)
        end
    end
  end

  defp wait_for_release(gate) do
    if Process.alive?(gate) and Agent.get(gate, & &1) == false do
      Process.sleep(5)
      wait_for_release(gate)
    else
      :ok
    end
  end

  @doc """
  Test helper (BT-2661): override the full `browse_classes` result so a test can
  drive the origin-filter default against a custom class set — e.g. a bare /
  stdlib-only workspace (no project-origin classes) for the empty-project → "all"
  fallback. `nil` (the default) uses the canned `base ++ defined_classes` set.
  Pass a plain list of row maps; it is wrapped in the `{:value, _}` reply shape.
  """
  def set_browse_classes(rows) when is_list(rows) or is_nil(rows),
    do: put(:browse_classes_override, rows)

  def browse_classes do
    await_mount_gate()

    case get(:browse_classes_override) do
      nil -> {:value, default_browse_classes()}
      rows -> {:value, rows}
    end
  end

  defp default_browse_classes do
    base = [
      # BT-2642: project rows carry the project package name (`project_package_name/0`
      # in the backend), so the editor header's project package badge can be reached
      # by navigation. The class tree still hides project badges; the header shows them.
      %{
        "name" => "Counter",
        "source_file" => "src/counter.bt",
        "source_origin" => "project",
        "package" => "exdura"
      },
      # BT-2578: a native: class in the tree so the System Browser's "Erlang
      # backend" badge + native pane can be reached by real navigation.
      %{
        "name" => "Subprocess",
        "source_file" => "src/subprocess.bt",
        "source_origin" => "project",
        "package" => "exdura"
      },
      # BT-2605: a class whose header carries leading modifiers (`sealed typed`),
      # so the editor's class/method modifier badges can be reached by navigation.
      %{
        "name" => "Ledger",
        "source_file" => "src/ledger.bt",
        "source_origin" => "project",
        "package" => "exdura"
      },
      # BT-2641: dependency classes so the class-tree's "DEP · <pkg>" badge can be
      # reached by navigation — one with a known package, one without.
      %{
        "name" => "HttpClient",
        "source_file" => "deps/http/src/http_client.bt",
        "source_origin" => "dependency",
        "package" => "HTTP"
      },
      %{
        "name" => "Orphan",
        "source_file" => "deps/orphan.bt",
        "source_origin" => "dependency"
      },
      # BT-2642: a stdlib class so the editor header's STDLIB badge can be reached
      # by navigation.
      %{
        "name" => "Object",
        "source_file" => "stdlib/src/object.bt",
        "source_origin" => "stdlib",
        "package" => "beamtalk"
      }
    ]

    extra =
      get(:defined_classes)
      |> Enum.map(fn name ->
        %{
          "name" => name,
          "source_file" => "src/#{Macro.underscore(name)}.bt",
          "source_origin" => "project",
          "package" => "exdura"
        }
      end)

    base ++ extra
  end

  def browse_protocols(class, side) do
    selectors =
      cond do
        side == "instance" and class == "Subprocess" ->
          # BT-2578: `self delegate` facade methods on the native: class.
          [%{"selector" => "readLine"}, %{"selector" => "writeLine:"}]

        side == "instance" and
            (class == "Counter" or class == "Ledger" or
               MapSet.member?(get(:defined_classes), class)) ->
          [%{"selector" => "value"}, %{"selector" => "increment"}]

        true ->
          []
      end

    {:value, %{"protocols" => [%{"name" => "all", "selectors" => selectors}]}}
  end

  def browse_method_source(class, _side, selector) do
    {disk_source, doc, signature} =
      case selector do
        "increment" ->
          {"increment => self.value := self.value + 1",
           "Increment the counter by one.\n\n## Examples\n```beamtalk\nc increment\n```",
           "increment -> Counter"}

        "value" ->
          # A method with a signature but no `///` doc — exercises the no-doc path.
          {"value => self.value", nil, "value -> Integer"}

        _ ->
          {"stub => nil", nil, nil}
      end

    # BT-2566: in production `browse_method_source` returns the current *image*
    # body, which after a `save_method` `>>` compile is the compiled body. Serve
    # the tracked compiled source when one exists, falling back to the on-disk stub.
    source = Map.get(get(:compiled_sources), {class, selector}, disk_source)

    # A saved-but-unflushed method has an image body that diverges from disk, so a
    # re-browse reports `disk_differs: true` (BT-2565). The backend's `disk_differs`
    # is a load-time snapshot, not a live diff. A test can also force divergence
    # *without* a ChangeLog entry (BT-2598: an image that differs from disk because
    # disk changed under it, e.g. an external edit) via `seed_disk_differs/2`.
    disk_differs =
      Map.has_key?(get(:changes), {class, selector}) or
        MapSet.member?(get(:forced_disk_differs), {class, selector})

    {:value,
     %{
       "source" => source,
       "doc" => doc,
       "signature" => signature,
       "source_status" => "indexed",
       "origin" => "both",
       "disk_differs" => disk_differs,
       # BT-2578: methods on the stubbed native: class are `self delegate` facades,
       # so they carry the native_delegate flag the "→ Erlang implementation" jump
       # keys off; ordinary classes' methods do not.
       "native_delegate" => class == "Subprocess"
     }}
  end

  def browse_class_definition(class) do
    if get(:fail_class_definition) do
      # BT-2605: simulate a transient workspace failure so the editor's
      # keep-prior-badges-on-failure fallback can be exercised. Mirrors the real
      # `{:error, _}` the facade returns when the workspace is unreachable.
      {:error, :unreachable}
    else
      browse_class_definition_ok(class)
    end
  end

  @doc "BT-2605: force `browse_class_definition/1` to fail (transient-outage simulation)."
  def fail_class_definition(flag) when is_boolean(flag), do: put(:fail_class_definition, flag)

  @doc """
  Test helper (BT-2655): seed an in-image class-definition skeleton override for
  `class` so `browse_class_definition/1` serves it over the hardcoded on-disk
  skeleton (modelling a header recompile / external edit). `reload_file/1` drops
  it, so a revert's reload makes the served body fall back to the on-disk skeleton.
  """
  def seed_class_definition(class, definition) when is_binary(definition) do
    update(:compiled_definitions, &Map.put(&1, class, definition))
  end

  defp browse_class_definition_ok(class) do
    definition = Map.get(get(:compiled_definitions), class, "Object subclass: #{class}")

    {:value,
     %{
       "class" => class,
       "definition" => definition,
       "comment" => "The #{class} class.\n\n## Overview\nA stubbed class comment.",
       # BT-2578: `Subprocess` / `Headless` stand in for native: classes (ADR
       # 0056) so the System Browser's "Erlang backend" badge + native pane can be
       # exercised (`Subprocess` has shipped source, `Headless` does not); every
       # other stubbed class is ordinary (native: false).
       "native" => class in ["Subprocess", "Headless"],
       "backing_module" => native_backing(class),
       # BT-2605/BT-2629: reflected class modifiers (sealed/typed/abstract),
       # mirroring the real op-4 result keys (booleans from runtime reflection, NOT
       # parsed from the `definition` skeleton). `Ledger` is sealed, `Vector` typed,
       # `Shape` abstract; every other stubbed class carries neither.
       # (`browse_class_definition` answers for any class name, so `Shape`/`Vector`
       # need not be in the class tree to be opened.)
       "sealed" => class == "Ledger",
       "typed" => class == "Vector",
       "abstract" => class == "Shape",
       # BT-2639: `is_protocol` is a runtime-reflection boolean on op 4, gating
       # the def-tab protocol action row. `Printable` stands in for a protocol
       # class object (ADR 0068); every other stubbed class is ordinary.
       "is_protocol" => class == "Printable",
       "origin" => "both",
       "disk_differs" => false
     }}
  end

  defp native_backing("Subprocess"), do: "beamtalk_subprocess"
  defp native_backing("Headless"), do: "beamtalk_headless"
  defp native_backing(_), do: nil

  # BT-2578: the backing Erlang source of a native: class. `Subprocess` returns a
  # readable stdlib module + a `handle_call` clause map; `Headless` exercises the
  # "source not available" empty state (a `.beam`-only build, `content: nil`).
  def browse_native_source(class, selector \\ nil)

  def browse_native_source("Subprocess", selector) do
    {:value,
     %{
       "class" => "Subprocess",
       "backing_module" => "beamtalk_subprocess",
       "source_file" => "apps/beamtalk_stdlib/src/beamtalk_subprocess.erl",
       "source_origin" => "stdlib",
       "editable" => false,
       "content" => "handle_call({readLine, []}, From, State) ->\n    {noreply, State}.\n",
       "clauses" => [%{"selector" => "readLine", "line" => 1}],
       # The real op returns the Erlang atom `null` for "no matching clause", which
       # arrives over distribution as `:null` — NOT `nil`. The stub must mirror
       # that so the LiveView's normalisation is actually exercised (BT-2578).
       "selected_clause" =>
         if(selector == "readLine", do: %{"selector" => "readLine", "line" => 1}, else: :null)
     }}
  end

  def browse_native_source("Headless", _selector) do
    # `.beam`-only build: the Erlang op returns the `null` atom for the absent
    # source path / content / clause, delivered as `:null` over distribution —
    # mirror it so the empty-state path is exercised on the real wire shape.
    {:value,
     %{
       "class" => "Headless",
       "backing_module" => "beamtalk_headless",
       "source_file" => :null,
       "source_origin" => "stdlib",
       "editable" => false,
       "content" => :null,
       "clauses" => [],
       "selected_clause" => :null
     }}
  end

  def browse_native_source(class, _selector),
    do: {:error, "class `#{class}` is not native-backed"}

  # BT-2648: a loaded package's hand-written native Erlang modules. Returns a
  # dependency module with readable source (openable), a stdlib module, and a
  # `.beam`-only dependency module (not openable — the empty-state path).
  def browse_native_modules do
    {:value,
     [
       %{
         "module" => "beamtalk_http_client",
         "source_file" => "deps/beamtalk_http/native/beamtalk_http_client.erl",
         "package" => "http",
         "source_origin" => "dependency",
         "openable" => true
       },
       %{
         "module" => "beamtalk_http_stripped",
         # `.beam`-only: no readable source → not openable (the `:null` atom is
         # how the Erlang op delivers an absent path over distribution).
         "source_file" => :null,
         "package" => "http",
         "source_origin" => "dependency",
         "openable" => false
       },
       %{
         "module" => "beamtalk_subprocess",
         "source_file" => "apps/beamtalk_stdlib/src/beamtalk_subprocess.erl",
         "package" => "stdlib",
         "source_origin" => "stdlib",
         "openable" => true
       }
       # NOTE: `beamtalk_project_native` (the BT-2670 editable project native) is
       # intentionally NOT listed here — adding a project-origin module would flip
       # the Native browser's default origin filter away from its All fallback,
       # which two BT-2656/BT-2661 list tests assert. The editable native is opened
       # directly by module via `browser_open_native_module` in the BT-2670 tests,
       # which only reads `browse_native_module_source/1`, not this enumeration.
     ]}
  end

  # BT-2648: the read-only native pane keyed by a standalone native module.
  # BT-2668: the real `browse_native_module_source` op returns the ABSOLUTE on-disk
  # path (e.g. `/home/.../_build/.../*.erl`), so the stub mirrors that — the
  # LiveView relativises it to `deps/beamtalk_http/native/beamtalk_http_client.erl`
  # for display (never the `/home/...` host path) via `clean_native_path/1`.
  def browse_native_module_source("beamtalk_http_client") do
    {:value,
     %{
       "class" => :null,
       "backing_module" => "beamtalk_http_client",
       "source_file" =>
         "/home/agent/source/proj/_build/default/deps/beamtalk_http/native/beamtalk_http_client.erl",
       "source_origin" => "dependency",
       "editable" => false,
       "content" => "handle_call({get, [Url]}, _From, State) ->\n    {reply, ok, State}.\n",
       "clauses" => [%{"selector" => "get", "line" => 1}],
       "selected_clause" => :null
     }}
  end

  def browse_native_module_source("beamtalk_http_stripped") do
    {:value,
     %{
       "class" => :null,
       "backing_module" => "beamtalk_http_stripped",
       "source_file" => :null,
       "source_origin" => "dependency",
       "editable" => false,
       "content" => :null,
       "clauses" => [],
       "selected_clause" => :null
     }}
  end

  # BT-2659: the backing module of the stubbed native: class `Subprocess`. Lets the
  # "Open native source →" link on the native class-definition tab open the full
  # module `.erl` as a read-only tab (distinct from the inline BT-2578 pane).
  def browse_native_module_source("beamtalk_subprocess") do
    {:value,
     %{
       "class" => :null,
       "backing_module" => "beamtalk_subprocess",
       "source_file" => "apps/beamtalk_stdlib/src/beamtalk_subprocess.erl",
       "source_origin" => "stdlib",
       "editable" => false,
       "content" => "handle_call({spawn, [Cmd]}, _From, State) ->\n    {reply, ok, State}.\n",
       "clauses" => [%{"selector" => "spawn:", "line" => 1}],
       "selected_clause" => :null
     }}
  end

  # BT-2670: a project-owned native module — `editable: true` so the LiveView opens
  # it as an editable tab (the Owner can compile + reload + write-back via
  # `save_native_source/2`). Mirrors the real op: a project-origin module with
  # readable on-disk content advertises `editable`.
  def browse_native_module_source("beamtalk_project_native") do
    {:value,
     %{
       "class" => :null,
       "backing_module" => "beamtalk_project_native",
       "source_file" =>
         "/home/agent/source/proj/native/beamtalk_project_native.erl",
       "source_origin" => "project",
       "editable" => true,
       "content" =>
         "-module(beamtalk_project_native).\n-export([go/0]).\n\ngo() -> ok.\n",
       "clauses" => [],
       "selected_clause" => :null
     }}
  end

  def browse_native_module_source(module),
    do: {:error, "module `#{module}` not found"}

  # BT-2670: save (edit → compile → reload → write-back) a project-owned native.
  # Records the call for assertions and, by default, reports a clean compile. A
  # test can force a structured compile error via `set_native_save/1` or model a
  # server-side read-only rejection via the `:error` form. The workspace re-derives
  # project ownership server-side; the stub mirrors the success/error wire shapes.
  def save_native_source(module, source) do
    record({:save_native_source, module, source})

    case get(:native_save_result) do
      nil ->
        {:value, %{"module" => module, "source_file" => "native/#{module}.erl", "ok" => true}}

      result ->
        result
    end
  end

  @doc """
  Test helper (BT-2670): force the next `save_native_source/2` result — a compile
  error (`{:value, %{"errors" => [...]}}`) or a server-side rejection
  (`{:error, reason}`). `nil` (the default) reports a clean compile.
  """
  def set_native_save(result), do: put(:native_save_result, result)

  # ── Navigation ops ───────────────────────────────────────────────────────

  def senders_of(_selector), do: {:value, %{"sites" => []}}
  def implementors_of(_selector), do: {:value, %{"sites" => []}}

  # BT-2639: the protocol-definition action row. Return canned rows for the known
  # `Printable` protocol so the UI flow (popover render + navigable rows) can be
  # exercised, and empty otherwise. `required_methods` rows are selectors
  # (navigable → Implementors); `conforming_classes` rows are class names
  # (navigable → open class).
  def required_methods_of("Printable") do
    {:value,
     %{
       "sites" => [
         %{
           "class" => "Printable",
           "class_side" => false,
           "method" => "printOn:",
           "line" => nil,
           "source_file" => nil,
           "source_origin" => "stdlib"
         }
       ]
     }}
  end

  def required_methods_of(_protocol), do: {:value, %{"sites" => []}}

  def conforming_classes_of("Printable") do
    {:value,
     %{
       "sites" => [
         %{
           "class" => "Counter",
           "class_side" => false,
           "method" => nil,
           "line" => nil,
           "source_file" => nil,
           "source_origin" => "project"
         }
       ]
     }}
  end

  def conforming_classes_of(_protocol), do: {:value, %{"sites" => []}}

  # BT-2669: the native-module "Callers" view (reverse of "go to native
  # source"). Return canned caller rows for the known `beamtalk_subprocess`
  # module so the UI flow (Callers popover render + navigable rows that open the
  # calling Beamtalk method) can be exercised, and empty otherwise. Rows reuse
  # the senders/implementors site-row shape.
  def callers_of_native_module("beamtalk_subprocess") do
    {:value,
     %{
       "sites" => [
         %{
           "class" => "Subprocess",
           "class_side" => false,
           "method" => "spawn:",
           "line" => 12,
           "source_file" => nil,
           "source_origin" => "stdlib"
         }
       ]
     }}
  end

  def callers_of_native_module(_module), do: {:value, %{"sites" => []}}

  def symbol_index(_scope), do: {:value, %{"classes" => []}}

  def complete(_pid, _code), do: {:ok, []}

  # BT-2555: live-image hover docs for the CodeMirror editors. The stub returns
  # docs for a known class name so the UI flow can be exercised, and "" otherwise
  # (the no-tooltip path).
  def hover(_pid, code) do
    if String.contains?(code, "Counter"),
      do: {:ok, "== Counter < Actor ==\nThe Counter class."},
      else: {:ok, ""}
  end

  # ── Supervision tree ─────────────────────────────────────────────────────

  def supervision_tree(_pid, _scope), do: {:ok, []}
  def pid_stats(_term), do: {:ok, %{}}

  # ── Pure utilities (delegated to real Workspace) ─────────────────────────

  def render_term(value), do: BtAttach.Workspace.render_term(value)
  def render_error(reason), do: BtAttach.Workspace.render_error(reason)
  def format_value(json), do: BtAttach.Workspace.format_value(json)
  def format_flush_summary(summary), do: BtAttach.Workspace.format_flush_summary(summary)
  def inspectable?(term), do: BtAttach.Workspace.inspectable?(term)
  def node_name, do: :stub_node@localhost

  # ── Call recording ───────────────────────────────────────────────────────

  def calls, do: get(:calls)

  @doc "Test helper: reset the recorded call log (BT-2590)."
  def clear_calls, do: put(:calls, [])
end
