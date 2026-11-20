# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.BindPolicy do
  @moduledoc """
  Boot-time guard for where the IDE front listens (BT-5065, ADR 0020 principle 1
  "local stays local … cannot be overridden by accident", ADR 0091).

  The front exposes eval. With no auth gate (no OIDC, no dev-auth) every request
  is trusted as Owner, so such a front must only be reachable from loopback.

  * `BT_ATTACH_BIND_IP` unset/empty → loopback (`127.0.0.1`).
  * A non-loopback bind requires an auth gate (OIDC or dev-auth), else boot
    fails — unless `BT_IDE_ALLOW_UNAUTHENTICATED=1` opts in explicitly. The
    opt-in is reported by `BtAttach.Application` as a warning on every boot.
  """

  @loopback {127, 0, 0, 1}

  @doc "The default bind when `BT_ATTACH_BIND_IP` is unset or empty."
  @spec default_ip() :: :inet.ip_address()
  def default_ip, do: @loopback

  @doc "True when the opt-in env value (`BT_IDE_ALLOW_UNAUTHENTICATED`) is truthy."
  @spec allow_unauthenticated?(String.t() | nil) :: boolean()
  def allow_unauthenticated?(value),
    do: String.downcase(value || "") in ~w(1 true yes)

  @doc """
  Validate `ip` against the auth posture.

  Returns `:ok`, `:unauthenticated_opt_in` (non-loopback, no gate, explicit
  opt-in — caller must warn), or `{:error, message}`.
  """
  @spec check(:inet.ip_address(), boolean(), boolean()) ::
          :ok | :unauthenticated_opt_in | {:error, String.t()}
  def check(ip, gate_active?, allow_unauthenticated?) do
    cond do
      BtAttachWeb.Auth.loopback?(ip) -> :ok
      gate_active? -> :ok
      allow_unauthenticated? -> :unauthenticated_opt_in
      true -> {:error, refusal(ip)}
    end
  end

  @doc "Like `check/3` but raises on refusal (for `config/runtime.exs`)."
  @spec check!(:inet.ip_address(), boolean(), boolean()) :: :ok | :unauthenticated_opt_in
  def check!(ip, gate_active?, allow_unauthenticated?) do
    case check(ip, gate_active?, allow_unauthenticated?) do
      {:error, message} -> raise message
      ok -> ok
    end
  end

  defp refusal(ip) do
    """
    refusing to start: the IDE would listen on #{:inet.ntoa(ip)} (not loopback) with no \
    authentication. The front exposes eval, so anyone who can reach the port could run \
    code as you. Either configure OIDC (BT_OIDC_ISSUER / BT_OIDC_CLIENT_ID / \
    BT_OIDC_CLIENT_SECRET, see docs/deployment/remote-liveview-ide.md), unset \
    BT_ATTACH_BIND_IP to bind loopback only, or — only on a network you fully trust — \
    set BT_IDE_ALLOW_UNAUTHENTICATED=1.\
    """
  end
end
