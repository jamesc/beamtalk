#!/usr/bin/env elixir
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# BT-2984 spike: a minimal prototype of the ADR 0097 Alternatives' "no-shell
# coordinator front" — the Tauri shell's 80/20 challenger. One always-on
# Phoenix-less Plug app that lists ~/.beamtalk/workspaces/*, and on
# "Attach" spawns a per-workspace `dist-liveview/bin/server <id>` (exactly
# the broker's own spawn primitive — see ../broker/broker.sh) and redirects
# the browser at it. Install this page as a PWA (browser "Install app") and
# you have a dockable, chrome-less coordinator with near-zero shell code —
# the ADR's own framing of why this alternative earns a real look before
# committing to Tauri.
#
# Run: elixir coordinator.exs   (needs network the first time, to fetch
# Plug/Bandit/Jason via Mix.install; cached after)
# Then open http://127.0.0.1:4500/
#
# What this DELIBERATELY does not attempt (out of scope for a spike, same
# boundary as the picker UI, ADR "Out of Scope" / BT-2986):
#   - create/stop workspaces via the CLI (Broker §5) — listing + attach only
#   - auth / multi-user — same single-user localhost posture as the front
#   - a service-worker / manifest.json for a literal installable PWA — the
#     spike's point is the discover+spawn+redirect flow, not packaging
Mix.install([
  {:plug, "~> 1.16"},
  {:bandit, "~> 1.5"},
  {:jason, "~> 1.4"}
])

defmodule Coordinator.State do
  @moduledoc """
  Tracks spawned per-workspace fronts so re-clicking Attach reuses a live one
  instead of leaking a second. An adversarial review of this spike flagged
  that a naive "get, then spawn if nil" (no lock between the two steps) lets
  two near-simultaneous `/attach/:id` requests both observe `nil` and both
  spawn — Plug/Bandit runs each request in its own process, so there is a
  real window. `claim_or_get/1` closes it by making the check-and-reserve a
  single atomic `Agent.get_and_update/2` call: the first caller gets
  `:claimed` and must spawn; any concurrent caller gets `{:existing,
  :pending}` and waits on `await/2` instead of racing a second spawn.
  """
  use Agent

  def start_link(_), do: Agent.start_link(fn -> %{} end, name: __MODULE__)

  @doc "Read-only peek for the listing page — not atomic, fine for display purposes only."
  def get(ws_id), do: Agent.get(__MODULE__, &Map.get(&1, ws_id))

  def claim_or_get(ws_id) do
    Agent.get_and_update(__MODULE__, fn state ->
      case Map.get(state, ws_id) do
        nil -> {:claimed, Map.put(state, ws_id, :pending)}
        entry -> {{:existing, entry}, state}
      end
    end)
  end

  def put(ws_id, port, os_pid),
    do: Agent.update(__MODULE__, &Map.put(&1, ws_id, %{port: port, os_pid: os_pid}))

  def drop(ws_id), do: Agent.update(__MODULE__, &Map.delete(&1, ws_id))

  @doc "Poll for a :pending claim (made by a concurrent request) to resolve into a real entry."
  def await(ws_id, deadline_ms) do
    case Agent.get(__MODULE__, &Map.get(&1, ws_id)) do
      %{port: _} = entry ->
        entry

      _ ->
        if System.monotonic_time(:millisecond) < deadline_ms do
          Process.sleep(100)
          await(ws_id, deadline_ms)
        else
          nil
        end
    end
  end

  def all, do: Agent.get(__MODULE__, & &1)
end

defmodule Coordinator.Discovery do
  @moduledoc "Same contract as broker/broker.sh discover — enumerate ~/.beamtalk/workspaces/*/metadata.json and check epmd liveness. A real coordinator/broker would share ONE implementation; this spike duplicates the ~15 lines rather than adding a cross-language dependency between the bash broker prototype and this Elixir one."
  @workspaces_dir Path.join(System.user_home!(), ".beamtalk/workspaces")

  def list do
    case File.ls(@workspaces_dir) do
      {:ok, entries} ->
        entries
        |> Enum.map(&describe/1)
        |> Enum.reject(&is_nil/1)
        |> Enum.sort_by(& &1.id)

      {:error, _} ->
        []
    end
  end

  defp describe(id) do
    meta_path = Path.join([@workspaces_dir, id, "metadata.json"])

    with true <- File.regular?(meta_path),
         {:ok, body} <- File.read(meta_path),
         {:ok, meta} <- Jason.decode(body),
         node when is_binary(node) and node != "" <- meta["node_name"] do
      %{id: id, node: node, alive: node_alive?(node)}
    else
      _ -> nil
    end
  end

  defp node_alive?(node) do
    short = node |> String.split("@") |> List.first() |> String.to_charlist()

    case :net_adm.names(:localhost) do
      {:ok, names} -> List.keymember?(names, short, 0)
      _ -> false
    end
  end
end

defmodule Coordinator.Spawner do
  @moduledoc "Attach primitive — mirrors broker/broker.sh's _spawn (loopback bind, RELEASE_DISTRIBUTION=none so the sname-seeding in ensure_distributed/0 actually applies, id-seeded BT_ATTACH_NODE_SUFFIX). See broker.sh's comment for why RELEASE_DISTRIBUTION=none matters."
  @repo_root Path.expand("../../..", __DIR__)
  @liveview_server Path.join(@repo_root, "dist-liveview/bin/server")

  def spawn_front(ws_id) do
    port = free_port()

    port_arg = to_string(port)
    secret = :crypto.strong_rand_bytes(32) |> Base.encode16(case: :lower)

    port_pid =
      Port.open(
        {:spawn_executable, System.find_executable("setsid")},
        [
          :binary,
          :exit_status,
          args: [@liveview_server, ws_id],
          env: [
            {~c"BT_ATTACH_BIND_IP", ~c"127.0.0.1"},
            {~c"BT_ATTACH_NODE_SUFFIX", String.to_charlist(ws_id)},
            {~c"RELEASE_DISTRIBUTION", ~c"none"},
            {~c"PORT", String.to_charlist(port_arg)},
            {~c"PHX_SERVER", ~c"true"},
            {~c"MIX_ENV", ~c"prod"},
            {~c"SECRET_KEY_BASE", String.to_charlist(secret)}
          ]
        ]
      )

    os_pid =
      case Port.info(port_pid, :os_pid) do
        {:os_pid, pid} -> pid
        _ -> nil
      end

    {port, os_pid}
  end

  defp free_port do
    {:ok, socket} = :gen_tcp.listen(0, [:binary, active: false])
    {:ok, port} = :inet.port(socket)
    :gen_tcp.close(socket)
    port
  end
end

defmodule Coordinator.Router do
  use Plug.Router

  plug(:match)
  plug(:dispatch)

  get "/" do
    rows =
      Coordinator.Discovery.list()
      |> Enum.map_join("\n", fn ws ->
        attached = Coordinator.State.get(ws.id)
        # Workspace ids come from directory names under ~/.beamtalk/workspaces/
        # — local, CLI-controlled today, but this is still an HTTP response;
        # escape before interpolating rather than assume they're HTML-safe.
        safe_id = Plug.HTML.html_escape(ws.id)
        safe_node = Plug.HTML.html_escape(ws.node)

        action =
          cond do
            not ws.alive -> "<span class=\"dead\">workspace not running</span>"
            match?(%{port: _}, attached) -> "<a href=\"http://localhost:#{attached.port}/\" target=\"_blank\">Open (already attached, port #{attached.port})</a>"
            attached == :pending -> "<span class=\"dead\">attaching…</span>"
            true -> "<a href=\"/attach/#{URI.encode(ws.id)}\">Attach</a>"
          end

        "<tr><td>#{safe_id}</td><td>#{safe_node}</td><td>#{if ws.alive, do: "alive", else: "dead"}</td><td>#{action}</td></tr>"
      end)

    body = """
    <!doctype html>
    <html><head><title>Beamtalk Desktop Coordinator (spike)</title>
    <style>body{font-family:sans-serif;margin:2rem}.dead{color:#999}table{border-collapse:collapse}td,th{padding:.4rem .8rem;border-bottom:1px solid #ddd;text-align:left}</style>
    </head><body>
    <h1>Beamtalk Workspaces</h1>
    <p>No-shell coordinator prototype (BT-2984 spike, ADR 0097 Alternatives). Install this page as an app (browser menu &rarr; Install) for a dockable window.</p>
    <table><tr><th>Workspace</th><th>Node</th><th>Status</th><th>Action</th></tr>
    #{rows}
    </table>
    </body></html>
    """

    conn
    |> Plug.Conn.put_resp_content_type("text/html")
    |> Plug.Conn.send_resp(200, body)
  end

  get "/attach/:id" do
    ws_id = conn.path_params["id"]
    deadline = System.monotonic_time(:millisecond) + 20_000

    result =
      case Coordinator.State.claim_or_get(ws_id) do
        :claimed ->
          spawn_and_wait(ws_id, deadline)

        {:existing, :pending} ->
          # A concurrent /attach/:id request already claimed this workspace
          # and is spawning it — wait on that instead of racing a second
          # spawn (the double-spawn an adversarial review of this spike
          # flagged in the original get-then-spawn version).
          case Coordinator.State.await(ws_id, deadline) do
            %{port: port} -> {:ok, port}
            nil -> :timeout
          end

        {:existing, %{port: port}} ->
          # Tracked front might have died since the last attach (crash,
          # `workspace stop`, etc.) — a short readiness re-check before
          # reusing it, same as broker.sh's own status check.
          case wait_ready(port, 3_000) do
            :ok ->
              {:ok, port}

            :timeout ->
              Coordinator.State.drop(ws_id)
              spawn_and_wait(ws_id, deadline)
          end
      end

    case result do
      {:ok, port} ->
        conn
        |> Plug.Conn.put_resp_header("location", "http://localhost:#{port}/")
        |> Plug.Conn.send_resp(302, "")

      :timeout ->
        # Redirecting anyway (the original version of this handler did) would
        # hand the browser a connection-refused tab; a real coordinator/broker
        # UI would show this inline instead of a bare 504.
        Plug.Conn.send_resp(conn, 504, "front for '#{ws_id}' did not become ready in time")
    end
  end

  match _ do
    Plug.Conn.send_resp(conn, 404, "not found")
  end

  # BT-2984 review finding: State.put/3 must not happen until wait_ready
  # confirms the front is actually serving /readiness. A concurrent
  # /attach/:id request only stops racing a second spawn (claim_or_get) — a
  # waiter in await/2 still returns as soon as it sees a %{port: _} entry, so
  # writing the entry before readiness would hand a not-yet-ready port to a
  # concurrent caller. On timeout OR an exception (e.g. setsid missing,
  # Port.open failure), drop the :pending claim so a later attach isn't
  # permanently blocked by a claim that will never resolve.
  defp spawn_and_wait(ws_id, deadline) do
    {port, os_pid} = Coordinator.Spawner.spawn_front(ws_id)
    remaining = max(deadline - System.monotonic_time(:millisecond), 0)

    case wait_ready(port, remaining) do
      :ok ->
        Coordinator.State.put(ws_id, port, os_pid)
        {:ok, port}

      :timeout ->
        Coordinator.State.drop(ws_id)
        :timeout
    end
  rescue
    e ->
      IO.warn("spawn_and_wait: exception for #{ws_id}: #{Exception.message(e)}")
      Coordinator.State.drop(ws_id)
      :timeout
  end

  # Two-stage probe (same contract as the broker's wait-ready): HTTP up, then
  # /readiness for true workspace reachability.
  defp wait_ready(port, timeout_ms) do
    deadline = System.monotonic_time(:millisecond) + timeout_ms
    do_wait_ready(port, deadline)
  end

  defp do_wait_ready(port, deadline) do
    case :httpc.request(:get, {~c"http://127.0.0.1:#{port}/readiness", []}, [], []) do
      {:ok, {{_, 200, _}, _headers, _body}} ->
        :ok

      _ ->
        if System.monotonic_time(:millisecond) < deadline do
          Process.sleep(300)
          do_wait_ready(port, deadline)
        else
          :timeout
        end
    end
  end
end

{:ok, _} = Application.ensure_all_started(:inets)
{:ok, _} = Coordinator.State.start_link([])
{:ok, _} = Bandit.start_link(plug: Coordinator.Router, ip: {127, 0, 0, 1}, port: 4500)

IO.puts("Coordinator listening on http://127.0.0.1:4500/ (Ctrl+C to stop)")
Process.sleep(:infinity)
