# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.PortReport do
  @moduledoc """
  Report the HTTP port this front actually bound back to whoever spawned it
  (ADR 0097 Broker §2, BT-4398).

  The desktop broker (`crates/beamtalk-desktop-broker`) starts a front with
  `PORT=0` — so the OS picks a free port *inside this process's own bind*,
  leaving no window between "choose a port" and "bind it" for another
  process to take it — plus `BT_ATTACH_PORT_FILE=<path>`. Once the endpoint
  is listening, `maybe_write/2` writes the bound port to that path; the broker
  polls for the file to learn where to probe. Same shape as the workspace
  node's own `port` file (`beamtalk_repl_server`, read by
  `beamtalk workspace create`'s port discovery).

  The write is atomic (write a sibling `.tmp`, then rename) so the broker
  never reads a half-written file. With `BT_ATTACH_PORT_FILE` unset or empty
  this is a no-op — every non-broker launch (`bin/server`, `just web`, remote
  deploys) is unchanged.
  """

  @env_var "BT_ATTACH_PORT_FILE"

  @doc "The env var naming the file to write the bound port to."
  @spec env_var() :: String.t()
  def env_var, do: @env_var

  @doc """
  Write `endpoint`'s bound HTTP port to `path` (default: `$BT_ATTACH_PORT_FILE`).

  Returns `:ok` without doing anything when `path` is `nil`/empty. Returns
  `{:error, reason}` when the endpoint has no running HTTP listener or the
  file can't be written — the caller fails the application start on that, so
  the broker sees the front exit instead of waiting for a file that will
  never appear.
  """
  @spec maybe_write(module(), String.t() | nil) :: :ok | {:error, term()}
  def maybe_write(endpoint, path \\ System.get_env(@env_var))

  def maybe_write(_endpoint, path) when path in [nil, ""], do: :ok

  def maybe_write(endpoint, path) do
    case endpoint.server_info(:http) do
      {:ok, {_ip, port}} when is_integer(port) and port > 0 -> write(path, port)
      {:ok, other} -> {:error, {:unexpected_server_info, other}}
      {:error, reason} -> {:error, {:no_http_listener, reason}}
    end
  end

  @doc "Atomically write `port` (plus a trailing newline) to `path`."
  @spec write(String.t(), pos_integer()) :: :ok | {:error, term()}
  def write(path, port) when is_integer(port) and port > 0 do
    tmp = path <> ".tmp"

    with :ok <- File.write(tmp, "#{port}\n"),
         :ok <- File.rename(tmp, path) do
      :ok
    else
      {:error, reason} ->
        _ = File.rm(tmp)
        {:error, {:port_file_write_failed, path, reason}}
    end
  end
end
