# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.RequestContextTest do
  use ExUnit.Case, async: true

  alias BtAttachWeb.Live.RequestContext
  alias BtAttachWeb.WorkspaceLive

  defp socket(assigns), do: %Phoenix.LiveView.Socket{assigns: Map.put(assigns, :__changed__, %{})}

  test "a socket with no :role gets the least-privileged role, never :owner (BT-5070)" do
    assert %{role: :observer} = RequestContext.build(socket(%{}))
  end

  test "an assigned role and user pass through unchanged" do
    ctx = RequestContext.build(socket(%{role: :owner, current_user: %{"sub" => "a"}}))
    assert ctx == %{user: %{"sub" => "a"}, role: :owner}
  end

  describe "resume-token scoping (BT-5070)" do
    test "the same token under different users maps to different registry keys" do
      alice = %{"sub" => "alice"}
      bob = %{"sub" => "bob"}

      assert WorkspaceLive.scope_token("tab-1", alice) ==
               WorkspaceLive.scope_token("tab-1", alice)

      refute WorkspaceLive.scope_token("tab-1", alice) == WorkspaceLive.scope_token("tab-1", bob)
    end

    test "the user's subject and the token cannot be shifted across the boundary" do
      refute WorkspaceLive.scope_token("b", %{"sub" => "a"}) ==
               WorkspaceLive.scope_token("ab", %{"sub" => ""})
    end

    test "with no authenticated subject the token is used as-is (auth disabled)" do
      assert WorkspaceLive.scope_token("t", nil) == "t"
      assert WorkspaceLive.scope_token("t", %{}) == "t"
    end
  end
end
