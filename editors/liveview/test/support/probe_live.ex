# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.ProbeLive do
  @moduledoc """
  A minimal LiveView used only by tests to exercise `BtAttachWeb.Auth`'s
  `on_mount` gate and re-validation timer in isolation — without the workspace
  attach (Erlang distribution) that `WorkspaceLive` needs and that the test
  sandbox cannot provide. It runs the *same* `on_mount` hook, so the auth
  lifecycle behaviour under test is identical to the real IDE.
  """
  use BtAttachWeb, :live_view

  on_mount {BtAttachWeb.Auth, :require_authenticated}

  @impl true
  def mount(_params, _session, socket), do: {:ok, socket}

  @impl true
  def render(assigns) do
    ~H"""
    <div id="probe">probe ok: {inspect(@current_user)}</div>
    """
  end
end
