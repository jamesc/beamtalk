# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.BinServerTest do
  @moduledoc """
  BT-5189: the release launcher `rel/overlays/bin/server` (a POSIX `sh`
  script). These run the overlay itself, before it reaches the release, so
  they need no built release; CI's release-smoke job runs the built
  release's `bin/server <id>` end to end.
  """
  use ExUnit.Case, async: true

  @script Path.expand("../../rel/overlays/bin/server", __DIR__)

  @moduletag skip: match?({:win32, _}, :os.type()) and "bin/server is a POSIX sh script"

  test "--help prints the whole header comment, up to the BIND section's last line" do
    {out, 0} = System.cmd("sh", [@script, "--help"], stderr_to_stdout: true)

    assert out =~ "Convenience launcher for the bt_attach release"
    assert out =~ "BIND: the front listens on loopback"
    assert out =~ "BT_IDE_ALLOW_UNAUTHENTICATED=1."
    refute out =~ "Copyright"
    refute out =~ "set -eu"
    refute out =~ "#"
  end

  @tag :tmp_dir
  test "with neither openssl nor base64, an unset SECRET_KEY_BASE fails with a clear message",
       %{tmp_dir: dir} do
    # A PATH holding only the tools the script needs before key generation —
    # no openssl, no base64 — so the generated key comes out empty.
    bin = Path.join(dir, "bin")
    File.mkdir_p!(bin)

    for tool <- ~w(sed head tr dirname cat awk) do
      case System.find_executable(tool) do
        nil -> :ok
        path -> File.ln_s!(path, Path.join(bin, tool))
      end
    end

    sh = System.find_executable("sh")

    {out, status} =
      System.cmd(sh, [@script],
        env: [{"PATH", bin}, {"SECRET_KEY_BASE", nil}],
        stderr_to_stdout: true
      )

    assert status == 1
    assert out =~ "Could not generate SECRET_KEY_BASE"
    assert out =~ "set SECRET_KEY_BASE yourself"
  end
end
