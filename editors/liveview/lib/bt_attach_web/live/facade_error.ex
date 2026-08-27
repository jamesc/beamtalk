# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.FacadeError do
  @moduledoc """
  Renders a `BtAttach.Facade` short-circuit (RBAC denial / off-vocabulary op)
  as a clear, user-facing message. These are Phoenix-side decisions, so they
  don't go through `BtAttach.Workspace`'s own error formatter except as a
  fallback for reasons the facade didn't short-circuit itself.

  Extracted as a shared leaf module (BT-3291) so `BtAttachWeb.WorkspaceLive`
  and its extracted panes (e.g. `BtAttachWeb.Live.Inspector`) render the same
  facade-error copy rather than each keeping its own (CLAUDE.md's
  no-duplicate-implementations rule).
  """

  alias BtAttach.Workspace

  @spec render(term()) :: String.t()
  def render(:unauthorized),
    do: "Not authorized: your role may not perform this operation."

  def render(:forbidden_op), do: "Operation not permitted."
  def render(reason), do: Workspace.render_error(reason)
end
