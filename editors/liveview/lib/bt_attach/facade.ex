# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.Facade do
  @moduledoc """
  Curated op facade — the **only** path from a LiveView event to a workspace
  `:rpc` call (ADR 0091 Decision 3).

  Phoenix exposes a **finite, named vocabulary** of operations to the browser
  and invokes only those on the workspace. It never proxies an arbitrary
  `{module, function, args}` triple from the browser into `:rpc.call/4` — a
  request for an op outside the catalog is rejected (`{:error, :forbidden_op}`)
  with **no** dist call. This is the substrate per-op RBAC hangs off (BT-2421:
  `capability/1` classifies every op as `:execute | :read | :admin`) and the
  natural audit point.

  **Honest framing (ADR 0091 Decision 3):** the facade is Phoenix-side
  discipline, not a workspace-enforced wall — once attached, the node *can*
  technically `:rpc` anything. Its value is enabling a least-privilege,
  non-`eval` role (Observer), not sandboxing `eval`.

  Calls return the BT-2399 **live terms** verbatim (`{:ok, value, output,
  warnings} | {:error, reason}` etc.) — JSON only at the WebSocket edge. The
  workspace client is injectable (`:bt_attach, :workspace_client`, default
  `BtAttach.Workspace`) so the dispatch contract is testable without a live node.
  """

  require Logger

  @typedoc "Capability class of an op (RBAC dimension, BT-2421)."
  @type capability :: :execute | :read | :admin

  # The authorized op vocabulary and its capability class (ADR 0091 Decision 3/4).
  # `eval`/`load_source`/`save`/`flush`/`reload` inject or run code (execute);
  # the read ops trigger no user code; `kill`/`rotate_cookie` are admin.
  @ops %{
    eval: :execute,
    load_source: :execute,
    save: :execute,
    flush: :execute,
    reload: :execute,
    info: :read,
    inspect: :read,
    bindings: :read,
    actors: :read,
    sessions: :read,
    complete: :read,
    changes: :read,
    subscribe_transcript: :read,
    subscribe_bindings: :read,
    subscribe_actors: :read,
    subscribe_classes: :read,
    kill: :admin,
    rotate_cookie: :admin
  }

  @doc "The full authorized op vocabulary (catalog keys)."
  @spec ops() :: [atom()]
  def ops, do: Map.keys(@ops)

  @doc "True if `op` is in the authorized vocabulary."
  @spec known?(atom()) :: boolean()
  def known?(op), do: Map.has_key?(@ops, op)

  @doc "The capability class of `op`, or `nil` if unknown."
  @spec capability(atom()) :: capability() | nil
  def capability(op), do: Map.get(@ops, op)

  @doc """
  Dispatch a curated op to the workspace, returning its live-term result.

  An op outside the vocabulary is refused with `{:error, :forbidden_op}` and
  **never** reaches the workspace client (no dist call). `ctx` carries the
  request identity (used by RBAC/audit in BT-2421); it is accepted here so the
  call sites are already shaped for it.
  """
  @spec dispatch(atom(), map(), map()) :: term()
  def dispatch(op, params, ctx \\ %{}) do
    if known?(op) do
      invoke(op, params, ctx)
    else
      Logger.warning("facade rejected off-vocabulary op: #{inspect(op)}",
        domain: [:beamtalk, :liveview]
      )

      {:error, :forbidden_op}
    end
  end

  # Route the ops the IDE drives today to the workspace client. Catalog ops with
  # no UI binding yet return `:unsupported_op` rather than silently no-op'ing.
  defp invoke(:eval, %{session_pid: pid, code: code}, _ctx), do: client().eval(pid, code)
  defp invoke(:inspect, %{term: term}, _ctx), do: client().inspect_value(term)
  defp invoke(:bindings, %{session_pid: pid}, _ctx), do: client().list_bindings(pid)
  defp invoke(:changes, _params, _ctx), do: client().change_history()
  defp invoke(:flush, _params, _ctx), do: client().flush()

  defp invoke(:save, %{class: class, selector: selector, source: source}, _ctx),
    do: client().save_method(class, selector, source)

  defp invoke(:subscribe_transcript, %{pid: pid}, _ctx), do: client().subscribe_transcript(pid)
  defp invoke(:subscribe_bindings, %{pid: pid}, _ctx), do: client().subscribe_bindings(pid)

  defp invoke(op, _params, _ctx), do: {:error, {:unsupported_op, op}}

  defp client, do: Application.get_env(:bt_attach, :workspace_client, BtAttach.Workspace)
end
