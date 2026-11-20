# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.BeamtalkHome do
  @moduledoc """
  Where Beamtalk keeps its per-user state: `~/.beamtalk`, overridable with
  `BEAMTALK_HOME` (BT-5081).

  The rule is the same one `beamtalk_env::beamtalk_root_dir` (Rust),
  `beamtalk_platform:beamtalk_root_dir/0` (Erlang) and the VS Code extension's
  `beamtalkRoot.ts` implement, and all four are pinned to the shared fixture
  `runtime/apps/beamtalk_runtime/test/fixtures/beamtalk_root_dir_conformance.json`
  (this module's half: `test/bt_attach/beamtalk_home_test.exs`):

    * a non-empty `BEAMTALK_HOME` wins and is used verbatim;
    * an empty `BEAMTALK_HOME` is treated as unset;
    * otherwise `<home>/.beamtalk`, with home from `HOME`, else `USERPROFILE`
      (native Windows), else the OS home directory;
    * no home at all resolves to `nil` (no invented fallback directory).

  Before this module the LiveView front hardcoded `System.user_home()` for
  `ide.toml` and `bin/server` hardcoded `${HOME}/.beamtalk`, so with
  `BEAMTALK_HOME` set the front read a different `ide.toml` than the desktop
  broker's "refuse to spawn if OIDC is configured" guard did, and
  `bin/server <id>` failed with "No workspace".

  `bin/server` resolves a workspace through `workspace_env/1` (run via
  `bin/bt_attach eval`) rather than re-implementing the rule in shell, and the
  workspace's `metadata.json` is read with a real JSON parser, as the broker's
  `discovery::read_node_name` does.
  """

  @type env :: %{optional(String.t()) => String.t()}

  @doc """
  The Beamtalk root directory, or `nil` when no home can be determined.

  `env` and `fallback_home` are injectable so the conformance test can drive
  every fixture row without touching the real environment.
  """
  @spec root_dir(env(), (-> String.t() | nil)) :: String.t() | nil
  def root_dir(env \\ System.get_env(), fallback_home \\ &System.user_home/0) do
    case non_empty(env["BEAMTALK_HOME"]) do
      nil ->
        home = non_empty(env["HOME"]) || non_empty(env["USERPROFILE"]) || fallback_home.()
        if home, do: Path.join(home, ".beamtalk"), else: nil

      override ->
        override
    end
  end

  @doc "Directory holding the per-workspace directories, or `nil` without a home."
  @spec workspaces_dir(env(), (-> String.t() | nil)) :: String.t() | nil
  def workspaces_dir(env \\ System.get_env(), fallback_home \\ &System.user_home/0) do
    case root_dir(env, fallback_home) do
      nil -> nil
      root -> Path.join(root, "workspaces")
    end
  end

  @doc """
  Resolve the attach settings for workspace `id`: `{:ok, node_name, cookie}`,
  or `{:error, message}` when the workspace is not there / unreadable.

  `node_name` comes from `metadata.json` parsed as JSON (not a regex), the
  cookie from the sibling `cookie` file.
  """
  @spec workspace_env(String.t(), env()) :: {:ok, String.t(), String.t()} | {:error, String.t()}
  def workspace_env(id, env \\ System.get_env()) when is_binary(id) do
    with {:ok, dir} <- workspace_dir(id, env),
         meta = Path.join(dir, "metadata.json"),
         cookie_file = Path.join(dir, "cookie"),
         true <-
           (File.regular?(meta) and File.regular?(cookie_file)) || {:error, no_workspace(id)},
         {:ok, node} <- read_node_name(meta),
         {:ok, cookie} <- File.read(cookie_file) do
      {:ok, node, String.trim_trailing(cookie, "\n")}
    else
      {:error, reason} when is_binary(reason) -> {:error, reason}
      {:error, reason} -> {:error, "Could not read workspace '#{id}': #{inspect(reason)}"}
    end
  end

  @doc false
  # Entry point for `bin/server <id>` (via `bin/bt_attach eval`): prints the node
  # name then the cookie on two lines, or the error on stderr and exits 1. The
  # id arrives in `BT_WORKSPACE_ID_ARG`, never interpolated into the eval string.
  def print_workspace_env do
    case workspace_env(System.get_env("BT_WORKSPACE_ID_ARG", "")) do
      {:ok, node, cookie} ->
        IO.puts(node)
        IO.puts(cookie)

      {:error, message} ->
        IO.puts(:stderr, "❌ " <> message)
        System.halt(1)
    end
  end

  defp workspace_dir(id, env) do
    cond do
      id == "" or String.contains?(id, ["/", "\\", <<0>>]) or id in [".", ".."] ->
        {:error, "Invalid workspace id '#{id}'."}

      dir = workspaces_dir(env) ->
        {:ok, Path.join(dir, id)}

      true ->
        {:error, "Cannot locate the Beamtalk home directory (set BEAMTALK_HOME)."}
    end
  end

  defp read_node_name(meta_path) do
    with {:ok, body} <- File.read(meta_path),
         {:ok, %{"node_name" => node}} when is_binary(node) and node != "" <- decode(body) do
      {:ok, node}
    else
      {:ok, _no_node_name} -> {:error, "Could not read node_name from #{meta_path}"}
      {:error, reason} -> {:error, "Could not read #{meta_path}: #{inspect(reason)}"}
    end
  end

  defp decode(body) do
    {:ok, :json.decode(body)}
  rescue
    _ -> {:error, :invalid_json}
  catch
    _, _ -> {:error, :invalid_json}
  end

  defp no_workspace(id) do
    "No workspace '#{id}' under the Beamtalk home. Start one first: " <>
      "beamtalk workspace create #{id} --background --persistent"
  end

  defp non_empty(""), do: nil
  defp non_empty(value), do: value
end
