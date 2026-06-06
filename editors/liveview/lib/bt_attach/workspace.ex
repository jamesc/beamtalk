# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.Workspace do
  @moduledoc """
  Attach-topology client for a running Beamtalk workspace BEAM node.

  This is the entire surface the Phoenix side needs to talk to the workspace:
  it joins the workspace's Erlang distribution mesh with the shared cookie,
  then drives the workspace via `:rpc` and native cross-node message passing.

  Topology (BT-2394): **Attach** — Phoenix runs as its own BEAM node and
  connects to the workspace node over Erlang distribution, the same way
  Livebook attaches to a remote node. No new wire protocol: we reuse the exact
  same curated op layer the WebSocket handler (`beamtalk_ws_handler`) calls.

  ## Term contract, not JSON (BT-2399)

  Eval goes through the term-returning op-layer seam
  (`beamtalk_repl_ops:dispatch/4`), which returns a live Erlang term
  (`op_result()`), *not* the JSON the browser receives. Over distribution that
  means `Counter spawn` hands back a real, messageable remote pid wrapped in a
  `{:beamtalk_object, class, module, pid}` tuple, not a flattened display
  string. JSON encoding stays at the WebSocket transport edge only. See
  `docs/development/repl-op-term-contract.md`.

  Display (`render_term/1`) is a *separate* concern from the data path: it
  reuses the workspace's own `beamtalk_repl_json:term_to_json/1` formatter — the
  same one the browser WebSocket protocol uses — so the LiveView shows
  byte-identical values to the Phase-1 browser workspace, surface-consistent for
  free, while `eval/2` still returns the live term.

  ## Subscriptions (BT-2399 facade)

  Transcript (and the other live push streams) are subscribed through the stable
  `beamtalk_repl_subscriptions` facade rather than by casting
  `{subscribe, self()}` tuples at gen_servers (which is what the BT-2394 spike
  did as a shortcut). Because the LiveView pid is location-transparent over
  distribution, the facade's explicit-pid form registers the LiveView's own pid
  and the workspace pushes `{:transcript_output, text}` messages to it directly.

  Configuration comes from the environment so this stays decoupled from the
  workspace's own config:

    * `BT_WORKSPACE_NODE`   — e.g. `beamtalk_workspace_spike@localhost`
    * `BT_WORKSPACE_COOKIE` — contents of `~/.beamtalk/workspaces/<id>/vm.args`
                              (`-setcookie <token>`), token only
  """

  require Logger

  @doc "The configured workspace node name as an atom."
  def node_name do
    System.get_env("BT_WORKSPACE_NODE", "beamtalk_workspace_spike@localhost")
    |> String.to_atom()
  end

  @doc """
  Ensure this Phoenix node is distributed, shares the workspace cookie, and is
  connected to the workspace node. Idempotent — safe to call on every mount.
  """
  def connect do
    ensure_distributed()
    set_cookie()

    case Node.connect(node_name()) do
      true -> :ok
      reason -> {:error, {:connect_failed, node_name(), reason}}
    end
  end

  @doc """
  Create a fresh, workspace-supervised REPL session. Each LiveView mount gets
  its own session, so two browser tabs have isolated bindings (proven in the
  BT-2394 spike: tab1 `x = 100`, tab2 `x = 999`). Session state — bindings and
  loaded classes — persists across evals within the session for the lifetime of
  the LiveView process.

  Returns the remote session pid, or `{:error, reason}`.
  """
  def start_session(session_id) when is_binary(session_id) do
    case rpc(:beamtalk_session_sup, :start_session, [session_id]) do
      {:ok, pid} when is_pid(pid) -> pid
      {:badrpc, reason} -> {:error, {:unreachable, reason}}
      other -> {:error, other}
    end
  end

  @doc """
  Stop a workspace-supervised session started by `start_session/1`.

  The session is owned by the workspace's `beamtalk_session_sup`, so it does not
  go away just because the LiveView process exits; we must ask the supervisor to
  terminate it or it leaks one orphaned session per mount/reconnect. Idempotent
  and best-effort: a session that is already gone returns `{:error, :not_found}`
  and an unreachable workspace returns `{:badrpc, _}` — both are non-fatal at the
  call site (the LiveView is terminating anyway).
  """
  def close_session(session_pid) when is_pid(session_pid) do
    rpc(:beamtalk_session_sup, :stop_session, [session_pid])
  end

  @doc """
  Evaluate a Beamtalk expression in `session_pid` through the curated op layer's
  term-returning seam (`beamtalk_repl_ops:dispatch/4`, BT-2399).

  Returns the **live Erlang term** the workspace produced — no JSON. The term
  contract is the only coupling between Phoenix and the workspace:

    * success → `{:ok, term, output, warnings}`
    * error   → `{:error, reason_term, output, warnings}` where `reason_term` is
      the structured `#beamtalk_error{}` record (a tuple tagged `:beamtalk_error`,
      so no shared `.hrl` is needed)

  `output` is captured stdout (`String.t/0`) and `warnings` is a list of strings.
  """
  def eval(session_pid, expression) when is_pid(session_pid) and is_binary(expression) do
    case dispatch_eval(session_pid, expression) do
      {:ok, value, output, warnings} ->
        {:ok, value, to_string(output), Enum.map(warnings, &to_string/1)}

      # Term contract: eval may attach captured output + warnings to an error.
      {:error, reason, output, warnings} ->
        {:error, reason, to_string(output), Enum.map(warnings, &to_string/1)}

      {:error, reason} ->
        {:error, reason, "", []}

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}, "", []}

      # The op contract is a small tagged union; treat anything else as a data
      # error rather than letting a CaseClauseError crash the LiveView.
      other ->
        {:error, {:unexpected_reply, other}, "", []}
    end
  end

  # Build the protocol request as a plain map, decode it to a `protocol_msg()`
  # on the workspace node (so we don't depend on the record's `.hrl`), then
  # dispatch through the term-returning op layer. `dispatch/4` returns the
  # `op_result()` term directly — JSON encoding (`encode/2`) is never invoked.
  defp dispatch_eval(session_pid, expression) do
    request = %{"op" => "eval", "params" => %{"code" => expression}}

    case rpc(:beamtalk_repl_protocol, :decode, [encode_json(request)]) do
      {:ok, msg} ->
        params = rpc(:beamtalk_repl_protocol, :get_params, [msg])
        rpc(:beamtalk_repl_ops, :dispatch, ["eval", params, msg, session_pid])

      other ->
        other
    end
  end

  @doc """
  Render a result *term* as a string for the page, surface-consistent with the
  Phase-1 browser workspace.

  Display is a separate concern from the data path: `eval/2` returns the live
  term, and this stringifies it via the workspace's own
  `beamtalk_repl_json:term_to_json/1` — the exact formatter the browser
  WebSocket protocol uses — so values render byte-identically across surfaces. A
  spawned actor renders as the browser's `#Actor<…>` display while the live,
  messageable remote pid remains in the term `eval/2` returned.

  Falls back to a local `inspect/1` only if the workspace formatter is
  unreachable, so the pane degrades gracefully rather than crashing.
  """
  def render_term(value) do
    case rpc(:beamtalk_repl_json, :term_to_json, [value]) do
      {:badrpc, _reason} -> inspect(value)
      json -> format_value(json)
    end
  end

  @doc """
  Render a `term_to_json/1` value (the same JSON-ready value the browser
  serialises) to a display string using the canonical, surface-shared rules from
  `beamtalk_repl_protocol::format::format_value` (Rust, BT-2086 — Plain mode), so
  the LiveView pane matches the CLI / MCP / browser byte-for-byte:

    * scalars render verbatim (strings already carry `#Actor<…>`, float text, …)
    * lists render as Beamtalk list literals `#(a, b, c)`
    * maps render as `{k: v, …}`

  Pure (no RPC), so it is unit-tested directly without a live workspace.
  """
  def format_value(json) when is_binary(json), do: json
  def format_value(json) when is_integer(json), do: Integer.to_string(json)
  def format_value(true), do: "true"
  def format_value(false), do: "false"
  def format_value(nil), do: "nil"
  def format_value(json) when is_float(json), do: Float.to_string(json)

  def format_value(json) when is_list(json) do
    "#(" <> Enum.map_join(json, ", ", &format_value/1) <> ")"
  end

  def format_value(json) when is_map(json) do
    "{" <> Enum.map_join(json, ", ", fn {k, v} -> "#{k}: #{format_value(v)}" end) <> "}"
  end

  def format_value(json), do: inspect(json)

  @doc """
  Render an error *term* (`#beamtalk_error{}`) as a user-facing message,
  surface-consistent with the browser. Reuses
  `beamtalk_repl_json:format_error_message/1` so the message text matches what
  the browser shows; falls back to `inspect/1` for non-structured reasons.
  """
  def render_error(reason) do
    case rpc(:beamtalk_repl_json, :format_error_message, [reason]) do
      msg when is_binary(msg) -> msg
      _ -> inspect(reason)
    end
  end

  @doc """
  Subscribe the given `pid` (the LiveView process) to the workspace's Transcript
  push stream through the BT-2399 subscription facade.

  Over Erlang distribution the LiveView pid is location-transparent, so the
  facade's explicit-pid form registers it as the subscriber on the workspace
  node. The LiveView then receives `{:transcript_output, text}` messages
  directly — no polling, no `{subscribe, self()}` cast at the gen_server, no
  extra protocol.
  """
  def subscribe_transcript(pid) when is_pid(pid) do
    rpc(:beamtalk_repl_subscriptions, :subscribe, [:transcript, pid])
  end

  @doc "Unsubscribe `pid` from the Transcript push stream via the facade."
  def unsubscribe_transcript(pid) when is_pid(pid) do
    rpc(:beamtalk_repl_subscriptions, :unsubscribe, [:transcript, pid])
  end

  @doc """
  Subscribe the given `pid` (the LiveView process) to the workspace's
  bindings-changed push stream through the BT-2399 subscription facade.

  Like the Transcript stream, this uses the facade's explicit-pid form so the
  LiveView's own location-transparent pid is registered as the subscriber (not
  the short-lived RPC proxy). The bindings stream is a *signal* stream: it pushes
  `{:bindings_changed, session_id}` after each successful eval — a refresh
  trigger, not the data itself. On that signal the LiveView re-reads the current
  bindings via `list_bindings/1`, so the pane stays live without polling and
  without a `{subscribe, self()}` cast at the gen_server.
  """
  def subscribe_bindings(pid) when is_pid(pid) do
    rpc(:beamtalk_repl_subscriptions, :subscribe, [:bindings, pid])
  end

  @doc "Unsubscribe `pid` from the bindings-changed push stream via the facade."
  def unsubscribe_bindings(pid) when is_pid(pid) do
    rpc(:beamtalk_repl_subscriptions, :unsubscribe, [:bindings, pid])
  end

  @doc """
  List the current workspace bindings for `session_pid` as `{name, term}` pairs.

  Reads the session's live binding map through the read-surface
  (`beamtalk_repl_shell:get_bindings/1`, ADR 0085): the values are **live Erlang
  terms** — a bound `Counter spawn` is a real `{:beamtalk_object, class, module,
  pid}` handle, not a flattened display string — so the Inspector can follow them
  by reference. Display rendering is a separate concern handled by the caller via
  `render_term/1`.

  Returns a name-sorted list of `{name :: String.t(), term}` tuples, or
  `{:error, reason}` if the workspace is unreachable or the shell returns an
  unexpected shape (the call site renders the error rather than crashing).
  """
  def list_bindings(session_pid) when is_pid(session_pid) do
    case rpc(:beamtalk_repl_shell, :get_bindings, [session_pid]) do
      {:ok, bindings} when is_map(bindings) ->
        bindings
        |> Enum.map(fn {name, term} -> {to_string(name), term} end)
        |> Enum.sort_by(fn {name, _term} -> name end)

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}

      other ->
        {:error, {:unexpected_reply, other}}
    end
  end

  @doc """
  True when `term` is a live, messageable Beamtalk object handle — the
  reference-following case. Over distribution the `#beamtalk_object{}` record
  arrives as a `{:beamtalk_object, class, module, pid_or_ref}` tuple (see
  `beamtalk.hrl`). Only object terms backed by a real remote pid are
  inspectable: name-resolving proxies carry `{:registered, name}` in the identity
  slot (ADR 0079) and have no pid to `sys:get_state/2`, so they are not
  drillable here.
  """
  def inspectable?({:beamtalk_object, _class, _module, pid}) when is_pid(pid), do: true
  def inspectable?(_term), do: false

  @doc """
  Inspect a live object `term` through the read-surface `inspect` op (ADR 0085),
  returning its structured instance fields as a live-term map.

  This is the reference-following primitive: given a `{:beamtalk_object, class,
  module, pid}` handle held in the LiveView, it extracts the real remote pid and
  drives `beamtalk_repl_ops:dispatch("inspect", ...)`, which reads the actor's
  live state and returns only its user-visible fields (`{:inspect, fields_map}`).
  Because the fields are themselves live terms, an object-valued slot is again a
  `{:beamtalk_object, ...}` tuple the caller can drill into recursively — the
  whole point of carrying terms, not JSON, to the LiveView.

    * success → `{:ok, fields :: map()}` (or `{:ok, binary}` for a non-tagged
      raw state value the workspace stringified)
    * error   → `{:error, reason_term}` (`#beamtalk_error{}` or a structured
      reason); the caller renders it via `render_error/1`

  Returns `{:error, :not_inspectable}` for any term that is not a pid-backed
  object handle, so the caller never has to guess the term shape.
  """
  def inspect_value({:beamtalk_object, _class, _module, pid} = _term) when is_pid(pid) do
    pid_str = pid |> :erlang.pid_to_list() |> to_string()

    case dispatch_inspect(pid_str) do
      {:inspect, fields} when is_map(fields) -> {:ok, fields}
      {:inspect, other} -> {:ok, other}
      {:error, reason} -> {:error, reason}
      {:badrpc, reason} -> {:error, {:unreachable, reason}}
      other -> {:error, {:unexpected_reply, other}}
    end
  end

  def inspect_value(_term), do: {:error, :not_inspectable}

  # Build the `inspect` request as a plain map, decode it on the workspace node
  # (so we don't depend on the protocol record's `.hrl`), then dispatch through
  # the term-returning op layer. `dispatch/4` returns the `{:inspect, _}` term
  # directly — JSON encoding (`encode/2`) is never invoked, so the fields stay
  # live terms with their messageable pids intact.
  defp dispatch_inspect(pid_str) do
    request = %{"op" => "inspect", "params" => %{"actor" => pid_str}}

    case rpc(:beamtalk_repl_protocol, :decode, [encode_json(request)]) do
      {:ok, msg} ->
        params = rpc(:beamtalk_repl_protocol, :get_params, [msg])
        # inspect ignores SessionPid; pass self() purely to satisfy dispatch/4.
        rpc(:beamtalk_repl_ops, :dispatch, ["inspect", params, msg, self()])

      other ->
        other
    end
  end

  # ── internals ─────────────────────────────────────────────────────────────

  defp ensure_distributed do
    unless Node.alive?() do
      # A unique short name so multiple dev runs don't collide on epmd.
      name = :"bt_attach_#{System.unique_integer([:positive])}@localhost"

      # Two connected mounts can both pass Node.alive?/0 and race here; the
      # loser sees {:error, {:already_started, _}}, which is fine.
      case :net_kernel.start([name, :shortnames]) do
        {:ok, _pid} -> :ok
        {:error, {:already_started, _pid}} -> :ok
        {:error, reason} -> raise "failed to start distributed node: #{inspect(reason)}"
      end
    end
  end

  defp set_cookie do
    case System.get_env("BT_WORKSPACE_COOKIE") do
      nil -> Logger.warning("BT_WORKSPACE_COOKIE not set; relying on ~/.erlang.cookie")
      "" -> :ok
      token -> Node.set_cookie(node_name(), String.to_atom(token))
    end
  end

  # The workspace decoder accepts a JSON binary; build it with the OTP `:json`
  # module so we don't pull in a JSON dependency for one small request.
  defp encode_json(map) do
    :erlang.iolist_to_binary(:json.encode(map))
  end

  defp rpc(mod, fun, args) do
    :rpc.call(node_name(), mod, fun, args)
  end
end
