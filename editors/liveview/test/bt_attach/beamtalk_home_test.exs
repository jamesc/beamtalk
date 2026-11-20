# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.BeamtalkHomeTest do
  @moduledoc """
  Elixir half of the `~/.beamtalk` root-dir conformance corpus (BT-5081); the
  Rust, Erlang and TypeScript halves are named in `BtAttach.BeamtalkHome`.
  """
  use ExUnit.Case, async: true

  alias BtAttach.BeamtalkHome

  @corpus Path.expand(
            "../../../../runtime/apps/beamtalk_runtime/test/fixtures/beamtalk_root_dir_conformance.json",
            __DIR__
          )

  @cases @corpus |> File.read!() |> :json.decode()

  for %{"name" => name, "env" => env, "root" => root, "workspaces" => workspaces} <- @cases do
    test "conformance: #{name}" do
      env = unquote(Macro.escape(env))
      # No fallback home: the fixture's env fully determines the result.
      # The corpus encodes "no result" as JSON null, which `:json` decodes to `:null`.
      assert BeamtalkHome.root_dir(env, fn -> nil end) ==
               if(unquote(root) == :null, do: nil, else: unquote(root))

      assert BeamtalkHome.workspaces_dir(env, fn -> nil end) ==
               if(unquote(workspaces) == :null, do: nil, else: unquote(workspaces))
    end
  end

  test "falls back to the OS home directory when HOME and USERPROFILE are unset" do
    assert BeamtalkHome.root_dir(%{}, fn -> "/os/home" end) == "/os/home/.beamtalk"
  end

  describe "workspace_env/2" do
    setup do
      root = Path.join(System.tmp_dir!(), "bt-home-#{System.unique_integer([:positive])}")
      on_exit(fn -> File.rm_rf!(root) end)
      {:ok, root: root, env: %{"BEAMTALK_HOME" => root}}
    end

    defp make_workspace(root, id, metadata, cookie \\ "s3cret\n") do
      dir = Path.join([root, "workspaces", id])
      File.mkdir_p!(dir)
      File.write!(Path.join(dir, "metadata.json"), metadata)
      File.write!(Path.join(dir, "cookie"), cookie)
    end

    test "resolves a workspace under BEAMTALK_HOME", %{root: root, env: env} do
      make_workspace(root, "ws1", ~s({"node_name": "beamtalk_ws1@localhost", "port": 1}))
      assert {:ok, "beamtalk_ws1@localhost", "s3cret"} = BeamtalkHome.workspace_env("ws1", env)
    end

    test "parses metadata.json as JSON, not with a regex", %{root: root, env: env} do
      # A decoy `node_name` inside a string value fools a line-oriented sed.
      make_workspace(
        root,
        "ws2",
        ~s({"note": "\\"node_name\\": \\"decoy@host\\"", "node_name": "real@localhost"})
      )

      assert {:ok, "real@localhost", _} = BeamtalkHome.workspace_env("ws2", env)
    end

    test "a missing workspace is a clear error", %{env: env} do
      assert {:error, msg} = BeamtalkHome.workspace_env("nope", env)
      assert msg =~ "No workspace 'nope'"
    end

    test "malformed metadata and a missing node_name are errors", %{root: root, env: env} do
      make_workspace(root, "bad", "{not json")
      assert {:error, _} = BeamtalkHome.workspace_env("bad", env)

      make_workspace(root, "nonode", ~s({"port": 1}))
      assert {:error, msg} = BeamtalkHome.workspace_env("nonode", env)
      assert msg =~ "node_name"
    end

    test "an id cannot escape the workspaces directory", %{env: env} do
      assert {:error, _} = BeamtalkHome.workspace_env("../etc", env)
      assert {:error, _} = BeamtalkHome.workspace_env("", env)
    end
  end

  test "bin/server defers to BtAttach.BeamtalkHome instead of re-implementing the rule" do
    script = File.read!(Path.expand("../../rel/overlays/bin/server", __DIR__))
    assert script =~ "BtAttach.BeamtalkHome.print_workspace_env"
    refute script =~ "${HOME}/.beamtalk"
    refute script =~ ~s(sed -n 's/.*"node_name")
  end
end
