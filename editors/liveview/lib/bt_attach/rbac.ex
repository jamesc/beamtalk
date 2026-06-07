# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.Rbac do
  @moduledoc """
  Two-level role-based access control for the authenticated front
  (ADR 0091 Decision 4).

  v1 ships exactly the one boundary that contains anything — **execute vs.
  read**:

    * **Owner** — execute + read + admin (full RCE; the ADR 0058 authenticated
      user). Multiple Owners may share a workspace.
    * **Observer** — read only; **no** code execution. The role the facade
      (BT-2420) makes meaningful.

  Roles are mapped from the user's OIDC group claim (`groups_claim`) against the
  declarative `[oidc.roles]` table (resolved by `BtAttach.IdeConfig`). The
  capability columns are explicit so a future middle rung is additive.

  **Fail closed (ADR 0091 Decision 4):**

    * no group matches → **deny** (`{:error, :denied}`) — never a privileged
      fallback;
    * a session whose claims lack the groups claim → deny;
    * no role map supplied at all → **startup error** (`validate!/1`), not a
      silent fall-through.

  Per-op authorization (`authorize/2`) keys off the facade's capability class,
  enforced **before** any dist call, and every decision is audited.
  """

  require Logger

  @type role :: :owner | :observer
  @type capability :: BtAttach.Facade.capability()

  # Capability grants per role. Owner gets everything; Observer is read-only.
  @grants %{
    owner: [:execute, :read, :admin],
    observer: [:read]
  }

  @doc """
  Validate the role map at boot — fail closed if none was supplied.

  Called from `runtime.exs` when OIDC is enabled: the claim→role map is the
  single most security-critical configuration step, so an absent map stops boot
  rather than defaulting to deny-all-silently (which would look like a broken
  deploy) or — worse — any privileged fallback.
  """
  @spec validate!(BtAttach.IdeConfig.t()) :: :ok
  def validate!(%{roles: roles}) do
    owner = Map.get(roles, "owner", [])
    observer = Map.get(roles, "observer", [])

    if owner == [] and observer == [] do
      raise """
      Invalid IDE OIDC configuration: no role map. OIDC is enabled but
      [oidc.roles] supplies neither `owner` nor `observer` groups (and no
      BT_OIDC_ROLES_* env override). Set at least one — RBAC fails closed
      and would otherwise deny everyone.
      """
    end

    :ok
  end

  @doc """
  Resolve a user's role from their verified claims and the OIDC config.

  Reads the groups claim named by `config.groups_claim`, matches it against the
  `[oidc.roles]` table (Owner takes precedence over Observer), and returns
  `{:error, :denied}` when nothing matches or the claim is absent — fail closed.
  """
  @spec role_for(map(), BtAttach.IdeConfig.t()) :: {:ok, role()} | {:error, :denied}
  def role_for(claims, %{groups_claim: groups_claim, roles: roles}) when is_map(claims) do
    user_groups = claims |> Map.get(groups_claim) |> to_group_list()
    owner_groups = Map.get(roles, "owner", [])
    observer_groups = Map.get(roles, "observer", [])

    cond do
      any_member?(user_groups, owner_groups) -> {:ok, :owner}
      any_member?(user_groups, observer_groups) -> {:ok, :observer}
      true -> {:error, :denied}
    end
  end

  def role_for(_claims, _config), do: {:error, :denied}

  @doc """
  Authorize `role` to perform `op`, keyed off the facade capability class.

  `:ok` if granted, `{:error, :unauthorized}` otherwise. An unknown op (no
  capability) is never authorized. This is the per-op check the facade runs
  before any dist call.
  """
  @spec authorize(role(), atom()) :: :ok | {:error, :unauthorized}
  def authorize(role, op) do
    capability = BtAttach.Facade.capability(op)
    grants = Map.get(@grants, role, [])

    if capability != nil and capability in grants do
      :ok
    else
      {:error, :unauthorized}
    end
  end

  @doc "The capability classes granted to `role`."
  @spec grants(role()) :: [capability()]
  def grants(role), do: Map.get(@grants, role, [])

  @doc """
  Audit one authorization decision (ADR 0091 Decision 3/4): structured, one line
  per op, carrying the user, op, resolved role and decision. The raw claims are
  logged once at session mount (`audit_mount/2`), not per op.
  """
  @spec audit(map() | nil, atom(), role() | nil, :allow | :deny) :: :ok
  def audit(user, op, role, decision) do
    level = if decision == :allow, do: :info, else: :warning

    Logger.log(
      level,
      "authz decision=#{decision} op=#{op} role=#{inspect(role)} user=#{user_id(user)}",
      domain: [:beamtalk, :liveview]
    )
  end

  @doc "Audit the resolved role + raw claims at session mount (ADR 0091 Decision 4)."
  @spec audit_mount(map() | nil, role() | :denied) :: :ok
  def audit_mount(claims, role) do
    Logger.info(
      "authz mount role=#{inspect(role)} user=#{user_id(claims)} claims=#{inspect(claims)}",
      domain: [:beamtalk, :liveview]
    )
  end

  # A groups claim may arrive as a list, a single string, or be absent.
  defp to_group_list(groups) when is_list(groups), do: Enum.filter(groups, &is_binary/1)
  defp to_group_list(group) when is_binary(group), do: [group]
  defp to_group_list(_), do: []

  defp any_member?(a, b), do: Enum.any?(a, &(&1 in b))

  defp user_id(%{"sub" => sub}), do: inspect(sub)
  defp user_id(_), do: "unknown"
end
