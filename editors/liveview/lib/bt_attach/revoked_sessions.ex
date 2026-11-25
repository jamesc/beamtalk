# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.RevokedSessions do
  @moduledoc """
  Server-side revocation list for the cookie-stored IDE session (BT-5108).

  The session lives in an encrypted client-side cookie, so clearing it on
  logout cannot invalidate a copy that was already exfiltrated. Each login
  mints a random `live_socket_id`; logout records that id here, and
  `BtAttachWeb.Auth` refuses any session carrying a revoked id, on the HTTP
  render and on every socket (re)connect and re-validation tick.

  An entry only has to outlive the session TTL, after which the cookie is
  expired anyway; a periodic sweep drops entries older than that. The list is
  in memory (the front is a single node), so a front restart forgets it — the
  residual window is the remaining TTL of cookies copied before the restart.
  """

  use GenServer

  @table __MODULE__
  @sweep_ms 10 * 60 * 1000

  def start_link(opts \\ []), do: GenServer.start_link(__MODULE__, opts, name: __MODULE__)

  @doc "Revoke a session id until `ttl_secs` from now."
  @spec revoke(String.t() | nil, pos_integer()) :: :ok
  def revoke(id, ttl_secs) when is_binary(id) do
    if :ets.whereis(@table) != :undefined do
      :ets.insert(@table, {id, now() + ttl_secs})
    end

    :ok
  end

  def revoke(_id, _ttl_secs), do: :ok

  @doc """
  True when `id` was revoked and its entry has not yet expired.

  Fails closed (BT-5135): with the table absent (the GenServer is down or not
  yet started) revocations cannot be checked, so every session id reads as
  revoked rather than letting a logged-out cookie through.
  """
  @spec revoked?(term()) :: boolean()
  def revoked?(id) when is_binary(id) do
    :ets.whereis(@table) == :undefined or lookup_live(id) != []
  end

  def revoked?(_id), do: false

  defp lookup_live(id) do
    case :ets.lookup(@table, id) do
      [{_, until}] = hit -> if until > now(), do: hit, else: []
      other -> other
    end
  end

  @doc false
  def sweep do
    :ets.select_delete(@table, [{{:_, :"$1"}, [{:"=<", :"$1", now()}], [true]}])
  end

  @impl true
  def init(_opts) do
    :ets.new(@table, [:named_table, :public, :set, read_concurrency: true])
    Process.send_after(self(), :sweep, @sweep_ms)
    {:ok, %{}}
  end

  @impl true
  def handle_info(:sweep, state) do
    sweep()
    Process.send_after(self(), :sweep, @sweep_ms)
    {:noreply, state}
  end

  defp now, do: System.system_time(:second)
end
