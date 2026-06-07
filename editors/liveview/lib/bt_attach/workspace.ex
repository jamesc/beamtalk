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
    start_session(session_id, %{kind: "liveview"})
  end

  @doc """
  Create a fresh session carrying origin/debug metadata.

  `meta` is a map surfaced by `Workspace sessions` / `Session info` on the
  workspace side; the LiveView passes `kind: "liveview"` plus, where known, the
  Phoenix `node` and authenticated `user`, so an operator can see which front
  (and which user) opened a session. The `kind` is normalised on the workspace
  side, so an unexpected value is harmless.
  """
  def start_session(session_id, meta) when is_binary(session_id) and is_map(meta) do
    case rpc(:beamtalk_session_sup, :start_session, [session_id, meta]) do
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
  Check whether a workspace session pid is still alive (BT-2410 Wave 4).

  Used by the resume path: before re-binding a reconnecting LiveView to a session
  the Phoenix-side registry still remembers, we confirm the remote session
  process actually exists — the workspace could have restarted between the
  disconnect and the reconnect, leaving the registry entry stale.

  `is_process_alive/1` is evaluated **on the workspace node**, where the session
  pid is local (a remote `is_process_alive/1` would `badarg`). Returns a boolean
  for a reachable workspace; an unreachable workspace yields `{:badrpc, _}`, which
  the caller treats as not-alive (resume falls back to a fresh session).
  """
  def session_alive?(session_pid) when is_pid(session_pid) do
    case rpc(:erlang, :is_process_alive, [session_pid]) do
      result when is_boolean(result) -> result
      {:badrpc, _reason} -> false
    end
  end

  @doc """
  Count the workspace-supervised sessions currently alive (BT-2410 Wave 4).

  A read-only probe of `beamtalk_session_sup`'s active children, used by the
  session-teardown tests to assert that closing a LiveView (after the resume
  grace window) leaves no orphaned session. Returns the active-child count for a
  reachable workspace, or `{:error, {:unreachable, _}}` otherwise.
  """
  def session_count do
    case rpc(:supervisor, :count_children, [:beamtalk_session_sup]) do
      counts when is_list(counts) ->
        Keyword.get(counts, :active, 0)

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}
    end
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

  @doc """
  Snapshot the live supervision tree (ADR 0092) as a list of node maps.

  `scope` is `"default"` (the workspace tree with runtime plumbing filtered — the
  safe Read view) or `"system"` (everything, including runtime internals — the
  privileged whole-node view; RBAC gates the `processes_system` op to Owners).
  Surfaced through the shared eval seam so the structured node data is identical
  to every other surface. Returns `{:ok, nodes}` (a list of node maps) or
  `{:error, reason}`.
  """
  @spec supervision_tree(pid(), String.t()) :: {:ok, term()} | {:error, term()}
  def supervision_tree(session_pid, scope)
      when is_pid(session_pid) and scope in ["default", "system"] do
    expression = "ProcessNavigation #{scope} tree asDictionaries"

    case eval(session_pid, expression) do
      {:ok, value, _output, _warnings} -> {:ok, value}
      {:error, reason, _output, _warnings} -> {:error, reason}
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
    # The `inspect` op identifies the actor by the *textual* pid form, the same
    # contract the browser uses (`beamtalk_ws_handler` formats with pid_to_list,
    # the op resolves with list_to_pid). Both sides must run on the SAME node:
    # `pid_to_list/1` of a node-LOCAL pid yields the canonical `<0.X.Y>` text
    # that `list_to_pid/1` round-trips on that node. Stringifying a *remote*
    # workspace pid here on the Phoenix node would instead emit `<N.X.Y>` (N =
    # the workspace's index in *our* dist table), and the workspace's
    # `list_to_pid/1` would then `badarg` (or worse, resolve a different pid).
    # So format the pid ON the workspace node, where it is local, via RPC.
    case rpc(:erlang, :pid_to_list, [pid]) do
      pid_chars when is_list(pid_chars) ->
        dispatch_inspect_result(dispatch_inspect(to_string(pid_chars)))

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}
    end
  end

  def inspect_value(_term), do: {:error, :not_inspectable}

  # ── write-surface: method-level edit / save / flush (ADR 0082, BT-2409) ─────

  @doc """
  Save (durably patch) a single method on a class in the attached workspace,
  through the write-surface (ADR 0082, Wave 3).

  This is the method editor's "Save" action. Per ADR 0082 the operation is the
  Beamtalk-level `aClass compile: #selector source: body` primitive — but rather
  than building a `compile:source:` *eval string* (which would force escaping the
  body's quotes, backslashes, and `{` interpolation markers back into Beamtalk
  source — exactly the fragility ADR 0082 calls out), we call the runtime install
  chokepoint `beamtalk_repl_eval:compile_method/6` directly over distribution,
  passing the body as a **String value**. `>>`, `compile:source:`, and this entry
  all converge at the same in-memory install + ChangeLog append, so the saved
  method is compiled, flushed into the live BEAM module, and recorded in the
  workspace's change history (the ChangeLog coherence criterion) in one step.

  `selector` is the method selector (e.g. `"increment"` or `"at:put:"`), `source`
  the method definition body. Author metadata is stamped as `liveview` / `human`
  for the audit trail.

  Returns:

    * success → `{:ok, class}` where `class` is the class-name binary the install
      confirmed
    * error   → `{:error, reason_term}` where `reason_term` is the structured
      `#beamtalk_error{}` for a failed compile/save (rendered via
      `render_error/1`), or `{:unreachable, _}` if the workspace is gone

  The term contract is the only coupling: no JSON crosses the Attach path.
  """
  def save_method(class, selector, source)
      when is_binary(class) and is_binary(selector) and is_binary(source) do
    # compile_method/6: (ClassNameBin, Selector, SourceBin, Intent, Author, AuthorKind).
    # Intent `durable` so the patch is logged as a keepable change (ADR 0082); the
    # selector and body are passed as values — no source-string round-trip.
    args = [class, selector, source, :durable, "liveview", :human]

    case rpc(:beamtalk_repl_eval, :compile_method, args) do
      {:ok, class_bin} ->
        {:ok, to_string(class_bin)}

      # A compile/install failure returns {error, Reason}; structure it so the
      # LiveView renders an actionable #beamtalk_error{} rather than a flattened
      # internal term.
      {:error, reason} ->
        {:error, structure_error(reason)}

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}

      other ->
        {:error, {:unexpected_reply, other}}
    end
  end

  @doc """
  Flush the workspace's pending durable changes to disk (ADR 0082 `Workspace
  flush`). This is the "Save All to Disk" action — it replays the ChangeLog
  entries written by `save_method/3` into their `.bt` source files.

  Calls `beamtalk_workspace_flush:flush/0` directly, which returns a term
  contract (never raises on a normal conflict): a `FlushResult`-shaped summary
  map on success, or `{error, #beamtalk_error{}}` on a hard runtime error. The
  summary's `conflicts` / `skipped` lists carry recoverable conditions
  (external-edit conflicts, non-flushable stdlib patches) for the caller to
  render.

  Returns `{:ok, summary_map}` or `{:error, reason_term}`.
  """
  def flush do
    case rpc(:beamtalk_workspace_flush, :flush, []) do
      {:ok, summary} when is_map(summary) -> {:ok, summary}
      {:error, reason} -> {:error, structure_error(reason)}
      {:badrpc, reason} -> {:error, {:unreachable, reason}}
      other -> {:error, {:unexpected_reply, other}}
    end
  end

  @doc """
  List the workspace's active change history (ADR 0082 `Workspace changes`) — the
  per-method ChangeLog entries that are dirty (installed in memory, not yet
  flushed to disk). This is the ChangeLog-coherence view: after `save_method/3`,
  the saved `(class, selector)` should appear here.

  Reads `beamtalk_workspace_changelog:change_entries/0` — the same
  `$beamtalk_class`-tagged value maps that back the `ChangeLog.bt` /
  `ChangeEntry.bt` value objects — in a **single RPC**. Each entry is already a
  plain atom-keyed map (not an opaque `#entry{}` record), so nothing fragile
  crosses the node boundary and there is no per-entry round-trip. We keep only the
  entries flagged `active` (current epoch, not orphaned, not yet flushed — the
  exact `Workspace changes` predicate) and render display rows newest-first.

  Returns a list of row maps, or `{:error, reason}` if the workspace is
  unreachable or returns an unexpected shape.
  """
  def change_history do
    case rpc(:beamtalk_workspace_changelog, :change_entries, []) do
      entries when is_list(entries) ->
        entries
        |> Enum.filter(&active_entry?/1)
        |> Enum.map(&entry_to_row/1)
        |> Enum.reverse()

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}

      other ->
        {:error, {:unexpected_reply, other}}
    end
  end

  # The change-entry value map carries an `active` boolean (current epoch, not
  # orphaned, not flushed) — the same predicate `Workspace changes` filters on.
  defp active_entry?(%{active: active}), do: active == true
  defp active_entry?(_), do: false

  @doc """
  Render a `FlushResult` summary map (ADR 0082 `Workspace flush`) to a one-line
  status string for the page. Pure (no RPC), so it is unit-tested directly.

  Over distribution the workspace returns an atom-keyed map; keys are read
  defensively (atom or string) so an unexpected shape degrades to `inspect/1`
  rather than crashing the pane. Conflicts and skipped (non-flushable) counts are
  appended when present, since they are the recoverable conditions the user acts
  on.
  """
  def format_flush_summary(summary) when is_map(summary) do
    flushed = summary_field(summary, :flushed, 0)
    files = summary |> summary_field(:files, []) |> List.wrap()
    conflicts = summary |> summary_field(:conflicts, []) |> List.wrap()
    skipped = summary |> summary_field(:skipped, []) |> List.wrap()

    base = "Flushed #{flushed} change(s) across #{length(files)} file(s)"

    parts =
      [base]
      |> maybe_append(conflicts != [], "#{length(conflicts)} conflict(s)")
      |> maybe_append(skipped != [], "#{length(skipped)} skipped")

    Enum.join(parts, " · ")
  end

  def format_flush_summary(other), do: inspect(other)

  defp summary_field(map, key, default) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp maybe_append(parts, false, _text), do: parts
  defp maybe_append(parts, true, text), do: parts ++ [text]

  # Render one ChangeEntry value map (atom-keyed, from `change_entries/0`) to a
  # display row. `className` arrives as an atom and `selector` as a Symbol (atom)
  # or `nil` for a new-class entry; `to_string/1` renders atoms verbatim. We read
  # every key defensively with a default so a future field addition can't crash
  # the pane.
  defp entry_to_row(entry) when is_map(entry) do
    %{
      class: to_string(Map.get(entry, :className, "")),
      selector: present_selector(Map.get(entry, :selector)),
      kind: to_string(Map.get(entry, :kind, "")),
      intent: to_string(Map.get(entry, :intent, "")),
      flushable: Map.get(entry, :flushable, false) == true,
      flushed: Map.get(entry, :flushed, false) == true,
      author_kind: to_string(Map.get(entry, :authorKind, ""))
    }
  end

  # ChangeLog selector is nil for a new-class entry; show a placeholder.
  defp present_selector(nil), do: "(class)"
  defp present_selector(value), do: to_string(value)

  # Turn a runtime `{error, Reason}` into a structured `#beamtalk_error{}` term so
  # the LiveView renders an actionable message. `ensure_structured_error/1` is the
  # workspace's own wrapper — already structured reasons pass through unchanged,
  # raw reasons get wrapped — so we never flatten an error to an opaque string.
  defp structure_error(reason) do
    case rpc(:beamtalk_repl_errors, :ensure_structured_error, [reason]) do
      {:badrpc, _} -> reason
      structured -> structured
    end
  end

  defp dispatch_inspect_result(result) do
    case result do
      {:inspect, fields} when is_map(fields) -> {:ok, fields}
      {:inspect, other} -> {:ok, other}
      {:error, reason} -> {:error, reason}
      {:badrpc, reason} -> {:error, {:unreachable, reason}}
      other -> {:error, {:unexpected_reply, other}}
    end
  end

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
