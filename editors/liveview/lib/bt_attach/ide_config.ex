# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.IdeConfig do
  @moduledoc """
  Resolve the IDE's OIDC configuration once at boot (ADR 0091 Decision 1).

  Configuration is **read once at boot, never per start, and never on the
  workspace** — OIDC is a Phoenix-deployment concern. Non-secret config and the
  role map live in a persistent declarative file, `~/.beamtalk/ide.toml`; the
  **client secret** is resolved from an env var or a `chmod 600` file (never
  inline in TOML, never logged — ADR 0058 Principle 3). **Every key may be
  overridden by an env var** for 12-factor / k8s deployments.

  Resolution order per key: **env var → `ide.toml` → error/deny**.

  A *missing file* is fine if every required key is supplied by env (the
  12-factor path); a *present file* is the "configure once on a host" path.
  Either way config is set-once, not per-start.

  ## Enabled vs. disabled

  OIDC is **opt-in**. It is considered *requested* when any `BT_OIDC_*` env var
  is set, or when `ide.toml` contains an `[oidc]` table. When OIDC is **not**
  requested, `load/1` returns `{:ok, nil}` and the front runs unauthenticated —
  the unchanged zero-config localhost story (ADR 0020 Principle 3). When it
  **is** requested, every required key must resolve or boot fails closed
  (`{:error, _}` / `load!/1` raises) — never a partial, insecure config.

  ## Resolved shape

      %{
        issuer: "https://idp.example.com",
        client_id: "beamtalk-ide",
        redirect_uri: "https://ide.example.com/oidc/callback",
        groups_claim: "groups",
        client_secret: "•••",            # resolved; never logged
        roles: %{"owner" => [...], "observer" => [...]}  # for BT-2421, may be %{}
      }
  """

  require Logger
  import Bitwise, only: [&&&: 2]

  @default_groups_claim "groups"
  @default_secret_env "BT_OIDC_CLIENT_SECRET"

  @type t :: %{
          issuer: String.t(),
          client_id: String.t(),
          redirect_uri: String.t(),
          groups_claim: String.t(),
          client_secret: String.t(),
          roles: %{optional(String.t()) => [String.t()]}
        }

  @doc """
  Default config-file path: `$BT_IDE_CONFIG` if set, else `~/.beamtalk/ide.toml`.
  """
  @spec default_path() :: String.t()
  def default_path do
    case System.get_env("BT_IDE_CONFIG") do
      path when is_binary(path) and path != "" -> path
      _ -> Path.join([System.user_home() || ".", ".beamtalk", "ide.toml"])
    end
  end

  @doc """
  Load + resolve OIDC config from `path` (default `default_path/0`) and the
  environment. Returns `{:ok, config}`, `{:ok, nil}` when OIDC is not requested,
  or `{:error, message}` (fail-closed) on an incomplete/invalid config.
  """
  @spec load(String.t()) :: {:ok, t() | nil} | {:error, String.t()}
  def load(path \\ default_path()) do
    with {:ok, file_map} <- read_file(path),
         oidc_file = Map.get(file_map, "oidc", %{}) do
      if requested?(oidc_file) do
        resolve(oidc_file)
      else
        {:ok, nil}
      end
    end
  end

  @doc """
  Like `load/1` but raises on error — the boot-time entry point used from
  `runtime.exs`, where a misconfiguration must stop the system coming up
  (fail-closed) rather than serve an RCE-bearing tool with broken auth.
  """
  @spec load!(String.t()) :: t() | nil
  def load!(path \\ default_path()) do
    case load(path) do
      {:ok, config} -> config
      {:error, message} -> raise "Invalid IDE OIDC configuration: #{message}"
    end
  end

  # ── resolution ──────────────────────────────────────────────────────────────

  # OIDC is requested if a file `[oidc]` table exists, or any BT_OIDC_* env is set.
  defp requested?(oidc_file) do
    oidc_file != %{} or
      Enum.any?(
        ~w(BT_OIDC_ISSUER BT_OIDC_CLIENT_ID BT_OIDC_REDIRECT_URI BT_OIDC_GROUPS_CLAIM
           BT_OIDC_CLIENT_SECRET),
        &(System.get_env(&1) not in [nil, ""])
      )
  end

  defp resolve(oidc_file) do
    # Reject an inline secret outright: the secret must come from env or a
    # chmod-600 file, never from the declarative (often VCS-tracked) TOML.
    if Map.has_key?(oidc_file, "client_secret") do
      {:error,
       "client_secret must not appear inline in ide.toml — use client_secret_env " <>
         "or client_secret_file (ADR 0058 Principle 3)"}
    else
      with {:ok, issuer} <- required("issuer", "BT_OIDC_ISSUER", oidc_file),
           {:ok, client_id} <- required("client_id", "BT_OIDC_CLIENT_ID", oidc_file),
           {:ok, redirect_uri} <- required("redirect_uri", "BT_OIDC_REDIRECT_URI", oidc_file),
           {:ok, secret} <- resolve_secret(oidc_file) do
        {:ok,
         %{
           issuer: issuer,
           client_id: client_id,
           redirect_uri: redirect_uri,
           groups_claim:
             optional("groups_claim", "BT_OIDC_GROUPS_CLAIM", oidc_file, @default_groups_claim),
           client_secret: secret,
           roles: roles(oidc_file)
         }}
      end
    end
  end

  # env var → file key → error. Returns trimmed non-empty value or an error.
  defp required(file_key, env_key, oidc_file) do
    case lookup(file_key, env_key, oidc_file) do
      value when is_binary(value) and value != "" ->
        {:ok, value}

      _ ->
        {:error,
         "missing required key '#{file_key}' (set #{env_key} or ide.toml [oidc].#{file_key})"}
    end
  end

  defp optional(file_key, env_key, oidc_file, default) do
    case lookup(file_key, env_key, oidc_file) do
      value when is_binary(value) and value != "" -> value
      _ -> default
    end
  end

  # env wins over file (ADR resolution order). File values must be strings.
  defp lookup(file_key, env_key, oidc_file) do
    case System.get_env(env_key) do
      value when is_binary(value) and value != "" ->
        value

      _ ->
        case Map.get(oidc_file, file_key) do
          value when is_binary(value) -> value
          _ -> nil
        end
    end
  end

  # ── client secret (env or chmod-600 file; never logged) ──────────────────────

  defp resolve_secret(oidc_file) do
    secret_env =
      optional("client_secret_env", "BT_OIDC_CLIENT_SECRET_ENV", oidc_file, @default_secret_env)

    case System.get_env(secret_env) do
      value when is_binary(value) and value != "" ->
        {:ok, value}

      _ ->
        case Map.get(oidc_file, "client_secret_file") do
          file when is_binary(file) and file != "" ->
            read_secret_file(file)

          _ ->
            {:error, "no client secret: set #{secret_env} or ide.toml [oidc].client_secret_file"}
        end
    end
  end

  defp read_secret_file(file) do
    path = Path.expand(file)

    with {:ok, %File.Stat{mode: mode}} <- File.stat(path),
         :ok <- check_secret_perms(path, mode),
         {:ok, contents} <- File.read(path) do
      case String.trim(contents) do
        "" -> {:error, "client_secret_file is empty: #{path}"}
        secret -> {:ok, secret}
      end
    else
      {:error, :enoent} ->
        {:error, "client_secret_file not found: #{path}"}

      {:error, reason} when is_atom(reason) ->
        {:error, "cannot read client_secret_file #{path}: #{reason}"}

      {:error, message} ->
        {:error, message}
    end
  end

  # Refuse a group/other-readable secret file on Unix (the chmod-600 requirement).
  # On non-Unix (no POSIX mode bits we trust) we skip the check.
  defp check_secret_perms(path, mode) do
    if match?({:unix, _}, :os.type()) and (mode &&& 0o077) != 0 do
      {:error,
       "client_secret_file #{path} is group/other-accessible (mode #{Integer.to_string(mode &&& 0o777, 8)}); " <>
         "it must be chmod 600"}
    else
      :ok
    end
  end

  # ── roles (consumed by BT-2421; loaded here so the same loader owns it) ───────

  # The role map comes from `[oidc.roles]` in the file, with per-role env
  # overrides (BT_OIDC_ROLES_OWNER / _OBSERVER, comma/space-separated groups)
  # for the 12-factor path. An env value overrides the file's list for that role.
  defp roles(oidc_file) do
    file_roles =
      case Map.get(oidc_file, "roles") do
        %{} = table ->
          table
          |> Enum.flat_map(fn
            {role, groups} when is_binary(role) and is_list(groups) ->
              [{role, Enum.filter(groups, &is_binary/1)}]

            _ ->
              []
          end)
          |> Map.new()

        _ ->
          %{}
      end

    file_roles
    |> maybe_put_env_role("owner", "BT_OIDC_ROLES_OWNER")
    |> maybe_put_env_role("observer", "BT_OIDC_ROLES_OBSERVER")
  end

  defp maybe_put_env_role(roles, role, env_key) do
    case System.get_env(env_key) do
      value when is_binary(value) and value != "" ->
        groups = value |> String.split([",", " ", "\t"], trim: true)
        Map.put(roles, role, groups)

      _ ->
        roles
    end
  end

  # ── file IO ───────────────────────────────────────────────────────────────

  # A missing file is not an error (12-factor path); other IO errors and parse
  # failures are. Never include file *contents* in an error (may carry secrets).
  defp read_file(path) do
    case File.read(path) do
      {:ok, source} ->
        case BtAttach.Toml.parse(source) do
          {:ok, map} ->
            {:ok, map}

          {:error, {kind, lineno, _raw}} ->
            {:error, "ide.toml parse error (#{kind}) at line #{lineno}"}
        end

      {:error, :enoent} ->
        {:ok, %{}}

      {:error, reason} ->
        {:error, "cannot read #{path}: #{reason}"}
    end
  end
end
