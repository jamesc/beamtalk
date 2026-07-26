# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.RuntimeConfigTest do
  @moduledoc """
  Evaluates `config/runtime.exs` itself (via `Config.Reader.read!/2`, the same
  mechanism `mix release` uses to run it) with a controlled environment, so the
  `BT_ATTACH_BIND_IP` hook (ADR 0097 Implementation §1b) and `PORT` passthrough
  are covered without booting a real endpoint.

  `env: :prod` makes `config_env()` evaluate to `:prod` inside the script
  (`Config.Reader.read!/2`'s documented purpose), exercising the same branch a
  release boot takes. `BT_IDE_CONFIG` is pointed at a file that doesn't exist so
  the unrelated OIDC-load block (`runtime.exs`'s `unless config_env() ==
  :test`) resolves deterministically to "OIDC not configured" regardless of
  what the machine running this test happens to have in `~/.beamtalk/ide.toml`
  or `BT_OIDC_*`.
  """
  use ExUnit.Case, async: false

  @runtime_exs Path.expand("../../config/runtime.exs", __DIR__)

  setup do
    original = %{
      "SECRET_KEY_BASE" => System.get_env("SECRET_KEY_BASE"),
      "PORT" => System.get_env("PORT"),
      "PHX_HOST" => System.get_env("PHX_HOST"),
      "BT_ATTACH_BIND_IP" => System.get_env("BT_ATTACH_BIND_IP"),
      "BT_IDE_CONFIG" => System.get_env("BT_IDE_CONFIG")
    }

    System.put_env("SECRET_KEY_BASE", String.duplicate("a", 64))
    System.delete_env("PHX_HOST")
    # A path that cannot exist, so IdeConfig.load! deterministically sees "no
    # config file" regardless of this machine's real ~/.beamtalk/ide.toml.
    System.put_env(
      "BT_IDE_CONFIG",
      Path.join(
        System.tmp_dir!(),
        "bt-2983-nonexistent-ide-#{System.unique_integer([:positive])}.toml"
      )
    )

    on_exit(fn ->
      for {key, value} <- original do
        case value do
          nil -> System.delete_env(key)
          value -> System.put_env(key, value)
        end
      end
    end)

    :ok
  end

  defp read_prod_endpoint_http! do
    config = Config.Reader.read!(@runtime_exs, env: :prod, target: :host)

    config
    |> Keyword.fetch!(:bt_attach)
    |> Keyword.fetch!(BtAttachWeb.Endpoint)
    |> Keyword.fetch!(:http)
  end

  describe "BT_ATTACH_BIND_IP (ADR 0097 Implementation §1b)" do
    test "unset keeps today's all-interfaces default" do
      System.delete_env("BT_ATTACH_BIND_IP")

      assert Keyword.fetch!(read_prod_endpoint_http!(), :ip) == {0, 0, 0, 0, 0, 0, 0, 0}
    end

    test "empty string keeps today's all-interfaces default" do
      System.put_env("BT_ATTACH_BIND_IP", "")

      assert Keyword.fetch!(read_prod_endpoint_http!(), :ip) == {0, 0, 0, 0, 0, 0, 0, 0}
    end

    test "127.0.0.1 is accepted for the desktop-attach broker's loopback bind" do
      System.put_env("BT_ATTACH_BIND_IP", "127.0.0.1")

      assert Keyword.fetch!(read_prod_endpoint_http!(), :ip) == {127, 0, 0, 1}
    end

    test "::1 is accepted for the desktop-attach broker's loopback bind" do
      System.put_env("BT_ATTACH_BIND_IP", "::1")

      assert Keyword.fetch!(read_prod_endpoint_http!(), :ip) == {0, 0, 0, 0, 0, 0, 0, 1}
    end

    test "an invalid address fails closed at boot rather than falling back silently" do
      System.put_env("BT_ATTACH_BIND_IP", "not-an-ip")

      assert_raise RuntimeError, ~r/invalid BT_ATTACH_BIND_IP/, fn ->
        read_prod_endpoint_http!()
      end
    end
  end

  describe "PORT passthrough" do
    test "PORT is read into the endpoint's http port" do
      System.delete_env("BT_ATTACH_BIND_IP")
      System.put_env("PORT", "34567")

      assert Keyword.fetch!(read_prod_endpoint_http!(), :port) == 34_567
    end

    test "defaults to 4000 when PORT is unset" do
      System.delete_env("BT_ATTACH_BIND_IP")
      System.delete_env("PORT")

      assert Keyword.fetch!(read_prod_endpoint_http!(), :port) == 4000
    end
  end
end
