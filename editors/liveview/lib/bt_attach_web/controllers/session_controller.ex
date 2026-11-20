# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.SessionController do
  @moduledoc """
  `DELETE /logout` — end the authenticated session (BT-5070).

  Before this route existed `BtAttachWeb.Auth.log_out/1` had no caller, so a
  user could only leave by letting the TTL run out. The route sits in the
  `:browser` pipeline, so the CSRF check applies.
  """
  use BtAttachWeb, :controller

  @doc "Clear the session and redirect to `/` (which re-enters the login flow)."
  def delete(conn, _params), do: BtAttachWeb.Auth.log_out(conn)
end
