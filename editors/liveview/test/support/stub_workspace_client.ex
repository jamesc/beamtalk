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

  def list_bindings(_pid), do: []
  def inspect_value(_term), do: {:ok, %{}}

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
        diff: nil
      }
    end)
    |> Enum.reverse()
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

  def git_diff(path), do: record({:git_diff, path}) && {:ok, %{worktree: "", staged: ""}}
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

  def browse_classes do
    base = [
      %{"name" => "Counter", "source_file" => "src/counter.bt", "source_origin" => "project"},
      # BT-2578: a native: class in the tree so the System Browser's "Erlang
      # backend" badge + native pane can be reached by real navigation.
      %{
        "name" => "Subprocess",
        "source_file" => "src/subprocess.bt",
        "source_origin" => "project"
      }
    ]

    extra =
      get(:defined_classes)
      |> Enum.map(fn name ->
        %{
          "name" => name,
          "source_file" => "src/#{Macro.underscore(name)}.bt",
          "source_origin" => "project"
        }
      end)

    {:value, base ++ extra}
  end

  def browse_protocols(class, side) do
    selectors =
      cond do
        side == "instance" and class == "Subprocess" ->
          # BT-2578: `self delegate` facade methods on the native: class.
          [%{"selector" => "readLine"}, %{"selector" => "writeLine:"}]

        side == "instance" and
            (class == "Counter" or MapSet.member?(get(:defined_classes), class)) ->
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
    # is a load-time snapshot, not a live diff.
    disk_differs = Map.has_key?(get(:changes), {class, selector})

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
    {:value,
     %{
       "class" => class,
       "definition" => "Object subclass: #{class}",
       "comment" => "The #{class} class.\n\n## Overview\nA stubbed class comment.",
       # BT-2578: `Subprocess` / `Headless` stand in for native: classes (ADR
       # 0056) so the System Browser's "Erlang backend" badge + native pane can be
       # exercised (`Subprocess` has shipped source, `Headless` does not); every
       # other stubbed class is ordinary (native: false).
       "native" => class in ["Subprocess", "Headless"],
       "backing_module" => native_backing(class),
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

  # ── Navigation ops ───────────────────────────────────────────────────────

  def senders_of(_selector), do: {:value, %{"sites" => []}}
  def implementors_of(_selector), do: {:value, %{"sites" => []}}
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
