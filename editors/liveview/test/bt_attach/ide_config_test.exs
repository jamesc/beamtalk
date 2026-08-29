# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.IdeConfigTest do
  # Mutates process-global env vars, so not async.
  use ExUnit.Case, async: false

  alias BtAttach.IdeConfig

  @oidc_env ~w(BT_OIDC_ISSUER BT_OIDC_CLIENT_ID BT_OIDC_REDIRECT_URI BT_OIDC_GROUPS_CLAIM
               BT_OIDC_CLIENT_SECRET BT_OIDC_CLIENT_SECRET_ENV BT_IDE_CONFIG ACME_SECRET
               BT_OIDC_ROLES_OWNER BT_OIDC_ROLES_OBSERVER)

  setup do
    saved = Map.new(@oidc_env, fn k -> {k, System.get_env(k)} end)
    Enum.each(@oidc_env, &System.delete_env/1)

    on_exit(fn ->
      Enum.each(saved, fn
        {k, nil} -> System.delete_env(k)
        {k, v} -> System.put_env(k, v)
      end)
    end)

    {:ok, tmp: tmp_path()}
  end

  defp tmp_path do
    Path.join(System.tmp_dir!(), "ide_#{System.unique_integer([:positive])}.toml")
  end

  defp write!(path, contents) do
    File.write!(path, contents)
    on_exit(fn -> File.rm_rf(path) end)
    path
  end

  @full """
  [oidc]
  issuer            = "https://idp.example.com"
  client_id         = "beamtalk-ide"
  redirect_uri      = "https://ide.example.com/oidc/callback"
  client_secret_env = "ACME_SECRET"

  [oidc.roles]
  owner    = ["beamtalk-owners"]
  observer = ["beamtalk-observers"]
  """

  describe "disabled (opt-in)" do
    test "missing file and no env → OIDC not requested → {:ok, nil}", %{tmp: tmp} do
      assert {:ok, nil} = IdeConfig.load(tmp)
    end
  end

  describe "happy path" do
    test "resolves from file + secret env", %{tmp: tmp} do
      path = write!(tmp, @full)
      System.put_env("ACME_SECRET", "s3cret")

      assert {:ok, config} = IdeConfig.load(path)
      assert config.issuer == "https://idp.example.com"
      assert config.client_id == "beamtalk-ide"
      assert config.redirect_uri == "https://ide.example.com/oidc/callback"
      assert config.groups_claim == "groups"
      assert config.client_secret == "s3cret"
      assert config.roles["owner"] == ["beamtalk-owners"]
      assert config.roles["observer"] == ["beamtalk-observers"]
    end

    test "groups_claim defaults to \"groups\"", %{tmp: tmp} do
      path = write!(tmp, @full)
      System.put_env("ACME_SECRET", "s3cret")
      assert {:ok, %{groups_claim: "groups"}} = IdeConfig.load(path)
    end
  end

  describe "env overrides file (resolution order: env → file → error)" do
    test "BT_OIDC_ISSUER overrides the file value", %{tmp: tmp} do
      path = write!(tmp, @full)
      System.put_env("ACME_SECRET", "s3cret")
      System.put_env("BT_OIDC_ISSUER", "https://override.example.com")

      assert {:ok, config} = IdeConfig.load(path)
      assert config.issuer == "https://override.example.com"
    end

    test "pure 12-factor: no file, all keys from env", %{tmp: tmp} do
      System.put_env("BT_OIDC_ISSUER", "https://env-idp")
      System.put_env("BT_OIDC_CLIENT_ID", "env-client")
      System.put_env("BT_OIDC_REDIRECT_URI", "https://env/callback")
      System.put_env("BT_OIDC_CLIENT_SECRET", "env-secret")

      assert {:ok, config} = IdeConfig.load(tmp)
      assert config.issuer == "https://env-idp"
      assert config.client_secret == "env-secret"
      assert config.roles == %{}
    end
  end

  describe "fail-closed" do
    test "requested but missing client secret → error", %{tmp: tmp} do
      path = write!(tmp, @full)
      # ACME_SECRET intentionally unset.
      assert {:error, message} = IdeConfig.load(path)
      assert message =~ "client secret"
    end

    test "missing a required key → error", %{tmp: tmp} do
      path =
        write!(tmp, """
        [oidc]
        client_id = "beamtalk-ide"
        redirect_uri = "https://ide/callback"
        client_secret_env = "ACME_SECRET"
        """)

      System.put_env("ACME_SECRET", "s3cret")
      assert {:error, message} = IdeConfig.load(path)
      assert message =~ "issuer"
    end

    test "inline client_secret in TOML is rejected", %{tmp: tmp} do
      path =
        write!(tmp, """
        [oidc]
        issuer = "https://idp"
        client_id = "id"
        redirect_uri = "https://ide/callback"
        client_secret = "do-not-do-this"
        """)

      assert {:error, message} = IdeConfig.load(path)
      assert message =~ "must not appear inline"
    end

    test "load!/1 raises on an incomplete config", %{tmp: tmp} do
      path = write!(tmp, @full)

      assert_raise RuntimeError, ~r/Invalid IDE OIDC configuration/, fn ->
        IdeConfig.load!(path)
      end
    end
  end

  describe "client_secret_file (chmod 600)" do
    @describetag :unix

    test "reads a 0600 file", %{tmp: tmp} do
      secret_path = tmp <> ".secret"
      File.write!(secret_path, "file-secret\n")
      File.chmod!(secret_path, 0o600)
      on_exit(fn -> File.rm_rf(secret_path) end)

      path =
        write!(tmp, """
        [oidc]
        issuer = "https://idp"
        client_id = "id"
        redirect_uri = "https://ide/callback"
        client_secret_file = "#{secret_path}"
        """)

      assert {:ok, config} = IdeConfig.load(path)
      assert config.client_secret == "file-secret"
    end

    test "refuses a group/other-readable secret file", %{tmp: tmp} do
      secret_path = tmp <> ".secret"
      File.write!(secret_path, "file-secret\n")
      File.chmod!(secret_path, 0o644)
      on_exit(fn -> File.rm_rf(secret_path) end)

      path =
        write!(tmp, """
        [oidc]
        issuer = "https://idp"
        client_id = "id"
        redirect_uri = "https://ide/callback"
        client_secret_file = "#{secret_path}"
        """)

      assert {:error, message} = IdeConfig.load(path)
      assert message =~ "chmod 600"
    end

    test "an empty secret file (after trim) is rejected", %{tmp: tmp} do
      secret_path = tmp <> ".secret"
      File.write!(secret_path, "   \n")
      File.chmod!(secret_path, 0o600)
      on_exit(fn -> File.rm_rf(secret_path) end)

      path =
        write!(tmp, """
        [oidc]
        issuer = "https://idp"
        client_id = "id"
        redirect_uri = "https://ide/callback"
        client_secret_file = "#{secret_path}"
        """)

      assert {:error, message} = IdeConfig.load(path)
      assert message =~ "client_secret_file is empty"
    end

    test "a client_secret_file that does not exist is reported not found", %{tmp: tmp} do
      missing_secret =
        Path.join(System.tmp_dir!(), "missing-secret-#{System.unique_integer([:positive])}")

      path =
        write!(tmp, """
        [oidc]
        issuer = "https://idp"
        client_id = "id"
        redirect_uri = "https://ide/callback"
        client_secret_file = "#{missing_secret}"
        """)

      assert {:error, message} = IdeConfig.load(path)
      assert message =~ "client_secret_file not found"
    end

    test "a client_secret_file that is a directory yields an atom-reason read error", %{tmp: tmp} do
      # `File.stat/1` succeeds on a directory (so `check_secret_perms/2` passes
      # so long as it isn't group/other-writable), but `File.read/1` then
      # fails with an atom reason (`:eisdir`) distinct from `:enoent` —
      # `read_secret_file/1`'s `{:error, reason} when is_atom(reason)` branch.
      secret_dir = tmp <> ".secretdir"
      File.mkdir_p!(secret_dir)
      File.chmod!(secret_dir, 0o700)
      on_exit(fn -> File.rm_rf(secret_dir) end)

      path =
        write!(tmp, """
        [oidc]
        issuer = "https://idp"
        client_id = "id"
        redirect_uri = "https://ide/callback"
        client_secret_file = "#{secret_dir}"
        """)

      assert {:error, message} = IdeConfig.load(path)
      assert message =~ "cannot read client_secret_file"
    end
  end

  describe "default_path/0 (BT-3311)" do
    test "falls back to ~/.beamtalk/ide.toml when BT_IDE_CONFIG is unset" do
      # setup/0 already deletes BT_IDE_CONFIG for every test in this module.
      path = IdeConfig.default_path()

      assert String.ends_with?(path, Path.join(".beamtalk", "ide.toml"))
    end

    test "load/0 (no path given) uses default_path/0 as the default argument" do
      path = tmp_path()
      System.put_env("BT_IDE_CONFIG", path)

      # The file doesn't exist, so this is deterministically "OIDC not
      # requested" regardless of what default_path/0 resolves to.
      assert {:ok, nil} = IdeConfig.load()
    end
  end

  describe "roles (BT-2421) further branches (BT-3311)" do
    test "a non-list role entry in the file is dropped rather than crashing", %{tmp: tmp} do
      path =
        write!(tmp, """
        [oidc]
        issuer = "https://idp"
        client_id = "id"
        redirect_uri = "https://ide/callback"
        client_secret_env = "ACME_SECRET"

        [oidc.roles]
        owner = "not-a-list"
        observer = ["beamtalk-observers"]
        """)

      System.put_env("ACME_SECRET", "s3cret")

      assert {:ok, config} = IdeConfig.load(path)
      refute Map.has_key?(config.roles, "owner")
      assert config.roles["observer"] == ["beamtalk-observers"]
    end

    test "BT_OIDC_ROLES_OWNER overrides the file's owner group list", %{tmp: tmp} do
      path =
        write!(tmp, """
        [oidc]
        issuer = "https://idp"
        client_id = "id"
        redirect_uri = "https://ide/callback"
        client_secret_env = "ACME_SECRET"

        [oidc.roles]
        owner = ["file-owners"]
        """)

      System.put_env("ACME_SECRET", "s3cret")
      System.put_env("BT_OIDC_ROLES_OWNER", "env-owners, platform admins")

      assert {:ok, config} = IdeConfig.load(path)
      assert config.roles["owner"] == ["env-owners", "platform", "admins"]
    end
  end

  describe "read_file/1 further error branches (BT-3311)" do
    test "an ide.toml parse error is reported with its line number", %{tmp: tmp} do
      path = write!(tmp, "[oidc\nissuer = \"https://idp\"\n")

      assert {:error, message} = IdeConfig.load(path)
      assert message =~ "ide.toml parse error"
      assert message =~ "line 1"
    end

    test "a non-enoent file read error is reported (path is a directory)", %{tmp: tmp} do
      # `File.read/1` on a directory fails with an atom reason distinct from
      # `:enoent` — `read_file/1`'s final `{:error, reason} -> {:error, "cannot
      # read ..."}` branch.
      File.mkdir_p!(tmp)
      on_exit(fn -> File.rm_rf(tmp) end)

      assert {:error, message} = IdeConfig.load(tmp)
      assert message =~ "cannot read #{tmp}"
    end
  end
end
