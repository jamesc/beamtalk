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
      {:error, :timeout} -> :ok
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

  def save_method(class, selector, _source) do
    record({:save, class, selector})

    update(:changes, fn changes ->
      Map.put(changes, {class, selector}, "src/#{Macro.underscore(class)}.bt")
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
        author_kind: "liveview"
      }
    end)
    |> Enum.reverse()
  end

  # ── New File + Revert ────────────────────────────────────────────────────

  def new_class(_source, path), do: {:ok, path}
  def revert(class, _selector), do: {:ok, class}

  # ── Browse ops ───────────────────────────────────────────────────────────

  def browse_classes do
    base = [%{"name" => "Counter", "source_file" => "src/counter.bt"}]

    extra =
      get(:defined_classes)
      |> Enum.map(fn name ->
        %{"name" => name, "source_file" => "src/#{Macro.underscore(name)}.bt"}
      end)

    {:value, base ++ extra}
  end

  def browse_protocols(class, side) do
    selectors =
      if side == "instance" and (class == "Counter" or MapSet.member?(get(:defined_classes), class)) do
        [%{"selector" => "value"}, %{"selector" => "increment"}]
      else
        []
      end

    {:value, %{"protocols" => [%{"name" => "all", "selectors" => selectors}]}}
  end

  def browse_method_source(_class, _side, selector) do
    source =
      case selector do
        "increment" -> "increment => self.value := self.value + 1"
        "value" -> "value => self.value"
        _ -> "stub => nil"
      end

    {:value, %{"source" => source, "source_status" => "indexed", "origin" => "both", "disk_differs" => false}}
  end

  def browse_class_definition(class) do
    {:value, %{"class" => class, "definition" => "Object subclass: #{class}", "origin" => "both", "disk_differs" => false}}
  end

  # ── Navigation ops ───────────────────────────────────────────────────────

  def senders_of(_selector), do: {:value, %{"sites" => []}}
  def implementors_of(_selector), do: {:value, %{"sites" => []}}
  def symbol_index(_scope), do: {:value, %{"classes" => []}}

  # ── Supervision tree ─────────────────────────────────────────────────────

  def supervision_tree(_pid, _scope), do: {:ok, []}
  def pid_stats(_term), do: {:ok, %{}}

  # ── Pure utilities (delegated to real Workspace) ─────────────────────────

  def render_term(value), do: BtAttach.Workspace.render_term(value)
  def render_error(reason), do: BtAttach.Workspace.render_error(reason)
  def format_value(json), do: BtAttach.Workspace.format_value(json)
  def format_flush_summary(summary), do: BtAttach.Workspace.format_flush_summary(summary)
  def inspectable?(term), do: BtAttach.Workspace.inspectable?(term)
  def node_name, do: :"stub_node@localhost"

  # ── Call recording ───────────────────────────────────────────────────────

  def calls, do: get(:calls)
end
