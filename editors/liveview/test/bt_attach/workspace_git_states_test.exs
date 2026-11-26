# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.WorkspaceGitStatesTest do
  @moduledoc """
  `BtAttach.Workspace` maps the git status words on the wire back to atoms from a
  closed list (BT-5205). The words come from `beamtalk_git.erl`; this test reads
  that file and fails when the Erlang side names a word the list lacks, so the two
  cannot drift apart silently.
  """
  use ExUnit.Case, async: false

  alias BtAttach.Workspace

  @git Path.expand("../../../../runtime/apps/beamtalk_workspace/src/beamtalk_git.erl", __DIR__)

  test "every status word beamtalk_git produces is in the closed list" do
    src = File.read!(@git)

    classified = Regex.scan(~r/classify_char\(\$.\) -> (\w+);/, src) |> Enum.map(&Enum.at(&1, 1))

    directly =
      Regex.scan(~r/(?:index|worktree) => (\w+)/, src)
      |> Enum.map(&Enum.at(&1, 1))
      |> Enum.filter(&(&1 in ~w(untracked ignored unmodified)))

    words = Enum.uniq(classified ++ directly)
    assert length(words) >= 8, "the scan found too few status words: #{inspect(words)}"

    missing = words -- Workspace.git_state_words()

    assert missing == [],
           "beamtalk_git names status words Workspace does not map: #{inspect(missing)}"
  end

  test "an unknown word stays text and makes no atom" do
    word = "never_seen_status_word_#{System.unique_integer([:positive])}"

    assert {:ok, %{files: [%{worktree: ^word}]}} =
             stub_status(%{"path" => "a", "index" => "modified", "worktree" => word})

    assert_raise ArgumentError, fn -> String.to_existing_atom(word) end
  end

  defp stub_status(file) do
    :meck.new(:erpc, [:unstick, :passthrough])

    :meck.expect(:erpc, :call, fn _n, :beamtalk_repl_ops, :request, _a, _t ->
      {:value, %{"files" => [file], "branch" => "main", "ahead" => 0, "behind" => 0}}
    end)

    try do
      Workspace.git_status()
    after
      :meck.unload()
    end
  end
end
