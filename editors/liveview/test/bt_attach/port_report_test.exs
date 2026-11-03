# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.PortReportTest do
  @moduledoc """
  `BtAttach.PortReport` — the front half of the desktop broker's PORT=0
  handoff (BT-4398). Stub endpoints stand in for `BtAttachWeb.Endpoint`, whose
  HTTP server isn't started under `mix test`.
  """
  use ExUnit.Case, async: true

  alias BtAttach.PortReport

  defmodule BoundEndpoint do
    def server_info(:http), do: {:ok, {{127, 0, 0, 1}, 51_234}}
  end

  defmodule NotListeningEndpoint do
    def server_info(:http), do: {:error, :not_running}
  end

  defmodule UnboundEndpoint do
    def server_info(:http), do: {:ok, {{127, 0, 0, 1}, 0}}
  end

  setup do
    dir = Path.join(System.tmp_dir!(), "bt_port_report_#{System.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf!(dir) end)
    %{dir: dir}
  end

  test "writes the endpoint's bound port, newline-terminated", %{dir: dir} do
    path = Path.join(dir, "front.port")
    assert PortReport.maybe_write(BoundEndpoint, path) == :ok
    assert File.read!(path) == "51234\n"
    refute File.exists?(path <> ".tmp"), "the temp file must be renamed, not left behind"
  end

  test "is a no-op when no port file is requested", %{dir: dir} do
    assert PortReport.maybe_write(NotListeningEndpoint, nil) == :ok
    assert PortReport.maybe_write(NotListeningEndpoint, "") == :ok
    assert File.ls!(dir) == []
  end

  test "errors (and writes nothing) when the endpoint has no HTTP listener", %{dir: dir} do
    path = Path.join(dir, "front.port")

    assert {:error, {:no_http_listener, :not_running}} =
             PortReport.maybe_write(NotListeningEndpoint, path)

    refute File.exists?(path)
  end

  test "never reports port 0 as a bound port", %{dir: dir} do
    path = Path.join(dir, "front.port")
    assert {:error, {:unexpected_server_info, _}} = PortReport.maybe_write(UnboundEndpoint, path)
    refute File.exists?(path)
  end

  test "errors when the port file can't be written", %{dir: dir} do
    path = Path.join([dir, "missing-subdir", "front.port"])

    assert {:error, {:port_file_write_failed, ^path, :enoent}} =
             PortReport.maybe_write(BoundEndpoint, path)
  end

  test "names the env var the broker sets" do
    assert PortReport.env_var() == "BT_ATTACH_PORT_FILE"
  end
end
