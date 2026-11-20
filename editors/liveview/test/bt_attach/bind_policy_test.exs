# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.BindPolicyTest do
  use ExUnit.Case, async: true

  alias BtAttach.BindPolicy

  test "default bind is loopback" do
    assert BtAttachWeb.Auth.loopback?(BindPolicy.default_ip())
  end

  test "loopback is always allowed" do
    for ip <- [{127, 0, 0, 1}, {0, 0, 0, 0, 0, 0, 0, 1}] do
      assert BindPolicy.check(ip, false, false) == :ok
    end
  end

  test "non-loopback without a gate is refused with a message naming the fixes" do
    assert {:error, msg} = BindPolicy.check({0, 0, 0, 0}, false, false)
    assert msg =~ "BT_IDE_ALLOW_UNAUTHENTICATED"
    assert msg =~ "OIDC"
  end

  test "non-loopback with a gate is allowed" do
    assert BindPolicy.check({0, 0, 0, 0}, true, false) == :ok
  end

  test "explicit opt-in is allowed but flagged for a warning" do
    assert BindPolicy.check({0, 0, 0, 0}, false, true) == :unauthenticated_opt_in
  end

  test "opt-in env parsing" do
    assert BindPolicy.allow_unauthenticated?("1")
    assert BindPolicy.allow_unauthenticated?("TRUE")
    refute BindPolicy.allow_unauthenticated?(nil)
    refute BindPolicy.allow_unauthenticated?("0")
  end
end
