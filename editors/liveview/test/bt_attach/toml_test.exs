# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.TomlTest do
  use ExUnit.Case, async: true

  alias BtAttach.Toml

  describe "parse/1 — the ide.toml subset" do
    test "parses the ADR 0091 example file" do
      source = """
      # ~/.beamtalk/ide.toml
      [oidc]
      issuer            = "https://idp.example.com"
      client_id         = "beamtalk-ide"
      redirect_uri      = "https://ide.example.com/oidc/callback"
      groups_claim      = "groups"
      client_secret_env = "BT_OIDC_CLIENT_SECRET"

      [oidc.roles]
      owner    = ["beamtalk-owners", "platform-admins"]
      observer = ["beamtalk-observers"]
      """

      assert {:ok, map} = Toml.parse(source)

      assert map["oidc"]["issuer"] == "https://idp.example.com"
      assert map["oidc"]["client_id"] == "beamtalk-ide"
      assert map["oidc"]["redirect_uri"] == "https://ide.example.com/oidc/callback"
      assert map["oidc"]["groups_claim"] == "groups"
      assert map["oidc"]["client_secret_env"] == "BT_OIDC_CLIENT_SECRET"

      assert map["oidc"]["roles"]["owner"] == ["beamtalk-owners", "platform-admins"]
      assert map["oidc"]["roles"]["observer"] == ["beamtalk-observers"]
    end

    test "blank input is an empty map" do
      assert {:ok, %{}} = Toml.parse("")
      assert {:ok, %{}} = Toml.parse("\n\n   \n")
    end

    test "whole-line and trailing comments are ignored" do
      assert {:ok, map} =
               Toml.parse("""
               # a comment
               [oidc]
               issuer = "https://idp"   # trailing comment
               """)

      assert map["oidc"]["issuer"] == "https://idp"
    end

    test "a # inside a quoted string is not treated as a comment" do
      assert {:ok, map} = Toml.parse(~s([oidc]\nclient_id = "ab#cd"\n))
      assert map["oidc"]["client_id"] == "ab#cd"
    end

    test "honours \\\" and \\\\ escapes in strings" do
      assert {:ok, map} = Toml.parse(~S([oidc]
      issuer = "a\"b\\c"
      ))

      assert map["oidc"]["issuer"] == "a\"b\\c"
    end

    test "empty array yields an empty list" do
      assert {:ok, map} = Toml.parse("[oidc.roles]\nowner = []\n")
      assert map["oidc"]["roles"]["owner"] == []
    end

    test "array element commas inside strings are not split points" do
      assert {:ok, map} = Toml.parse(~s([oidc.roles]\nowner = ["a,b", "c"]\n))
      assert map["oidc"]["roles"]["owner"] == ["a,b", "c"]
    end

    test "nested tables merge rather than clobber the parent" do
      assert {:ok, map} =
               Toml.parse("""
               [oidc]
               issuer = "x"
               [oidc.roles]
               owner = ["g"]
               """)

      assert map["oidc"]["issuer"] == "x"
      assert map["oidc"]["roles"]["owner"] == ["g"]
    end

    test "rejects unsupported bare (non-string) values with a line number" do
      assert {:error, {:unsupported, 2, _}} = Toml.parse("[oidc]\nttl = 3600\n")
      assert {:error, {:unsupported, 2, _}} = Toml.parse("[oidc]\nenabled = true\n")
    end

    test "rejects a malformed table header" do
      assert {:error, {:malformed, 1, _}} = Toml.parse("[oidc\n")
      assert {:error, {:malformed, 1, _}} = Toml.parse("[]\n")
    end

    test "rejects an unterminated string" do
      assert {:error, {:malformed, 2, _}} = Toml.parse(~s([oidc]\nissuer = "oops\n))
    end

    test "rejects a key with no =" do
      assert {:error, {:malformed, 2, _}} = Toml.parse("[oidc]\nissuer\n")
    end
  end
end
