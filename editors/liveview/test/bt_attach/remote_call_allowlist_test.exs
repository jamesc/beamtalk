# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.RemoteCallAllowlistTest do
  @moduledoc """
  The guard rail of the one-operation-layer epic (BT-5200, BT-5201).

  Every LiveView capability is meant to reach the workspace through
  `beamtalk_repl_ops` (the operation layer) or `beamtalk_repl_subscriptions`
  (the push-stream facade), never by calling a runtime module directly. This
  test parses every file under `lib/` (an AST walk, so comments and docs are
  ignored) and fails when a remote call targets a `module:function` outside
  `@allowlist` below.

  `@allowlist` starts as today's set. Each entry is annotated with the issue that
  migrates it to the operation layer and removes it (`:permanent` entries are the
  operation layer and the subscription facade themselves). The list can only
  shrink: a new direct call fails here, and an entry nothing calls any more
  fails here as stale, so the migrating issue must delete it.

  `docs/development/surface-parity.md` § "LiveView direct runtime calls"
  carries the same table with what each call does; the last test pins the two
  together.

  What counts as a remote call (everything else is an ordinary local call):

    * `rpc(:mod, :fun, ...)`, the `BtAttach.Workspace` transport helper;
    * `:erpc.*` / `:rpc.*` / `Node.spawn*` outside that helper's own definition;
    * any call or capture of a `:beamtalk_*` module, and
      `:erlang.make_fun(:mod, :fun, arity)`;
    * an external fun capture (`&:mod.fun/n`) passed to `rpc/…`.

  A call whose module is not a literal atom is rejected: the allowlist cannot
  vouch for a target it cannot read. A call whose *function* is a variable
  (`git_mutate/2`) is accepted when its module has an allowlisted entry.
  """
  use ExUnit.Case, async: true

  # {module, function, class, migrating issue}
  #
  # class: :operation_layer | :subscription | :session | :capability | :encoding
  # issue: :permanent, or the issue that migrates the call and deletes the entry
  # (BT-5200 epic: BT-5202 sessions, BT-5203 editing, BT-5204 workspace state,
  # BT-5205 git and process navigation, BT-5206 close-out).
  @allowlist [
    # The operation layer and the subscription facade: where every call ends up.
    {:beamtalk_repl_ops, :request, :operation_layer, :permanent},
    {:beamtalk_repl_ops, :render, :encoding, :permanent},
    {:beamtalk_repl_subscriptions, :subscribe, :subscription, :permanent},
    {:beamtalk_repl_subscriptions, :unsubscribe, :subscription, :permanent},
    {:beamtalk_repl_subscriptions, :subscribe_object, :subscription, :permanent},
    {:beamtalk_repl_subscriptions, :unsubscribe_object, :subscription, :permanent},

    # Sessions and eval control.
    {:beamtalk_session_sup, :start_session, :session, "BT-5202"},
    {:beamtalk_repl_shell, :attach, :session, "BT-5202"},
    {:beamtalk_repl_shell, :interrupt, :session, "BT-5202"},
    {:beamtalk_repl_shell, :get_bindings, :session, "BT-5202"},
    {:supervisor, :count_children, :session, "BT-5202"},
    {:erlang, :is_process_alive, :session, "BT-5202"},

    # Code editing.
    {:beamtalk_repl_eval, :compile_method, :capability, "BT-5203"},
    {:beamtalk_repl_eval, :reload_file, :capability, "BT-5203"},
    {:beamtalk_repl_loader_newclass, :new_class, :capability, "BT-5203"},
    {:beamtalk_workspace_interface_primitives, :revert_method, :capability, "BT-5203"},

    # Workspace state.
    {:beamtalk_workspace_flush, :flush, :capability, "BT-5204"},
    {:beamtalk_workspace_changelog_views, :change_entries, :capability, "BT-5204"},
    {:beamtalk_workspace_findings_store, :all, :capability, "BT-5204"},
    {:beamtalk_workspace_meta, :get_setting, :capability, "BT-5204"},
    {:beamtalk_version, :get, :capability, "BT-5204"},

    # Git and process navigation.
    {:beamtalk_git, :git_status, :capability, "BT-5205"},
    {:beamtalk_git, :git_diff, :capability, "BT-5205"},
    {:beamtalk_git, :git_log, :capability, "BT-5205"},
    {:beamtalk_git, :git_stage, :capability, "BT-5205"},
    {:beamtalk_git, :git_unstage, :capability, "BT-5205"},
    {:beamtalk_git, :git_commit, :capability, "BT-5205"},
    {:beamtalk_git, :git_revert_file, :capability, "BT-5205"},
    {:beamtalk_process_navigation, :child_handles, :capability, "BT-5205"},
    {:erlang, :process_info, :capability, "BT-5205"},
    {:erlang, :pid_to_list, :encoding, "BT-5205"},
    {:lists, :map, :encoding, "BT-5205"}
  ]

  @lib Path.expand("../../lib", __DIR__)
  @inventory Path.expand("../../../../docs/development/surface-parity.md", __DIR__)

  # The transport helpers' own definitions are the one place `:erpc.call/5` may
  # be spelled out; they take the target as variables.
  @transport_file "workspace.ex"
  @transport_helpers [:rpc, :erpc]

  describe "remote calls from lib/" do
    test "every remote call targets an allowlisted module:function" do
      allowed = MapSet.new(@allowlist, fn {m, f, _class, _issue} -> {m, f} end)
      allowed_modules = MapSet.new(@allowlist, fn {m, _f, _class, _issue} -> m end)

      violations =
        for {file, {kind, target, line}} <- remote_calls(),
            not permitted?(kind, target, allowed, allowed_modules) do
          "#{Path.relative_to(file, @lib)}:#{line}  #{explain(kind, target)}"
        end

      assert violations == [],
             """
             LiveView made a remote call outside the operation layer:

               #{Enum.join(violations, "\n  ")}

             Route the capability through `beamtalk_repl_ops:request/3` (add an operation
             where the table lacks one) or the subscription facade. Do not extend
             @allowlist for a new call; see BT-5200 and docs/development/surface-parity.md.
             """
    end

    test "no allowlist entry is stale: each is still called" do
      used =
        for {_file, {:call, {m, f}, _line}} <- remote_calls(), reduce: MapSet.new() do
          acc -> MapSet.put(acc, {m, f})
        end

      dynamic_modules =
        for {_file, {:call, {m, :dynamic}, _line}} <- remote_calls(), into: MapSet.new(), do: m

      stale =
        for {m, f, _class, issue} <- @allowlist,
            not MapSet.member?(used, {m, f}),
            not MapSet.member?(dynamic_modules, m) do
          "#{m}:#{f} (#{inspect(issue)})"
        end

      assert stale == [],
             "allowlist entries nothing calls any more, delete them: #{inspect(stale)}"
    end

    test "every entry names its migrating issue or is a permanent operation-layer entry" do
      for {m, f, class, issue} <- @allowlist do
        assert class in [:operation_layer, :subscription, :session, :capability, :encoding]

        assert issue == :permanent or issue in ~w(BT-5202 BT-5203 BT-5204 BT-5205 BT-5206),
               "#{m}:#{f} has no valid migrating issue: #{inspect(issue)}"

        if issue == :permanent do
          assert m in [:beamtalk_repl_ops, :beamtalk_repl_subscriptions],
                 "#{m}:#{f} is marked permanent but is not the operation layer or subscription facade"
        end
      end

      keys = Enum.map(@allowlist, fn {m, f, _c, _i} -> {m, f} end)
      assert keys == Enum.uniq(keys)
    end

    test "the walker itself catches a direct call (guards the guard)" do
      src = """
      defmodule Fake do
        def a, do: rpc(:beamtalk_new_module, :thing, [])
        def b, do: :erpc.call(node(), :lists, :map, [], 1)
        def c, do: :beamtalk_other.go()
        def e, do: rpc(:erlang, :make_fun, [&:beamtalk_cap.fun/1])
        def f, do: :erlang.make_fun(:beamtalk_mf, :run, 1)
        def g, do: rpc(:beamtalk_git, fun, [])
        def h, do: :rpc.multicall([node()], :m, :f, [])
        def i, do: Node.spawn(node(), fn -> :ok end)

        # The transport helper's own body takes its target as variables.
        defp erpc(mod, fun, args), do: :erpc.call(node(), mod, fun, args, 1)
        # rpc(:beamtalk_in_comment, :x, [])
        @doc "rpc(:beamtalk_in_doc, :x, [])"
        def j, do: :ok
      end
      """

      found =
        src
        |> calls_in("workspace.ex")
        |> Enum.map(fn {kind, target, _line} -> {kind, target} end)

      assert {:call, {:beamtalk_new_module, :thing}} in found
      assert {:call, {:lists, :map}} in found
      assert {:call, {:beamtalk_other, :go}} in found
      assert {:call, {:beamtalk_cap, :fun}} in found
      assert {:call, {:beamtalk_mf, :run}} in found
      assert {:call, {:beamtalk_git, :dynamic}} in found
      assert {:forbidden, ":rpc.multicall"} in found
      assert {:forbidden, "Node.spawn"} in found
      refute Enum.any?(found, &match?({:dynamic_module, _}, &1))

      refute Enum.any?(found, fn {_k, t} ->
               t in [{:beamtalk_in_comment, :x}, {:beamtalk_in_doc, :x}]
             end)

      # ...and the same helper body anywhere else is a violation, as is a module
      # that is not a literal atom.
      assert [{:dynamic_module, :unknown, _}] =
               calls_in("def d, do: rpc(some_module(), :f, [])", "other.ex")

      assert Enum.any?(
               calls_in("defp erpc(m, f, a), do: :erpc.call(node(), m, f, a, 1)", "other.ex"),
               &match?({:dynamic_module, _, _}, &1)
             )
    end
  end

  describe "inventory table (docs/development/surface-parity.md)" do
    test "has one row per allowlist entry with the same migrating issue" do
      rows = inventory_rows()

      expected =
        for {m, f, _class, issue} <- @allowlist, into: %{} do
          {"#{m}:#{f}", issue_label(issue)}
        end

      assert Map.keys(rows) |> Enum.sort() == Map.keys(expected) |> Enum.sort(),
             "surface-parity.md inventory rows and @allowlist disagree"

      for {target, issue} <- expected do
        assert rows[target] == issue,
               "#{target}: table says #{inspect(rows[target])}, allowlist says #{issue}"
      end
    end
  end

  defp issue_label(:permanent), do: "permanent"
  defp issue_label(issue), do: issue

  # `| \`mod:fun\` | class | what | covered by | migrating issue |` rows under the
  # inventory heading.
  defp inventory_rows do
    md = File.read!(@inventory)
    [_, section] = String.split(md, "### LiveView direct runtime calls", parts: 2)
    section = section |> String.split(~r/\n##+ /, parts: 2) |> hd()

    for line <- String.split(section, "\n"),
        [_, target, rest] <- [Regex.run(~r/^\|\s*`([a-z_]+:[a-z_]+)`\s*\|(.*)$/, line)],
        into: %{} do
      cells = rest |> String.split("|", trim: true) |> Enum.map(&String.trim/1)

      issue =
        cells |> List.last() |> String.replace(~r/[^A-Za-z0-9\-]/, " ") |> String.split() |> hd()

      {target, issue}
    end
  end

  defp permitted?(:call, {m, :dynamic}, _allowed, allowed_modules),
    do: MapSet.member?(allowed_modules, m)

  defp permitted?(:call, {m, f}, allowed, _allowed_modules), do: MapSet.member?(allowed, {m, f})
  defp permitted?(_kind, _target, _allowed, _allowed_modules), do: false

  defp explain(:call, {m, f}), do: "#{m}:#{f} is not on the allowlist"
  defp explain(:dynamic_module, _), do: "remote call to a module that is not a literal atom"
  defp explain(:forbidden, what), do: "#{what} outside the transport helper"

  # ── AST walk ─────────────────────────────────────────────────────────────

  defp remote_calls do
    for file <- Path.wildcard(Path.join(@lib, "**/*.ex")),
        call <- file |> File.read!() |> calls_in(file) do
      {file, call}
    end
  end

  # Returns `[{kind, target, line}]` for every remote call in `source`.
  defp calls_in(source, file) do
    ast = Code.string_to_quoted!(source, file: file)

    ast
    |> drop_transport_helpers(Path.basename(file))
    |> Macro.prewalk([], fn node, acc -> {node, collect(node, acc)} end)
    |> elem(1)
    |> Enum.reverse()
  end

  # The helper's body spells `:erpc.call(node, mod, fun, ...)` with variables.
  defp drop_transport_helpers(ast, @transport_file) do
    Macro.prewalk(ast, fn
      {:defp, _, [{name, _, args}, _body]} when name in @transport_helpers and is_list(args) ->
        :ok

      node ->
        node
    end)
  end

  defp drop_transport_helpers(ast, _file), do: ast

  # BtAttach.Workspace's local `rpc/2..4` transport call.
  defp collect({:rpc, meta, [mod, fun | rest]}, acc) do
    captures = for arg <- rest, cap <- captures_in(arg), do: {:call, cap, meta[:line]}
    [target(mod, fun, meta) | captures] ++ acc
  end

  # `:erpc.call(node, mod, fun, ...)` / `:rpc.call(node, mod, fun, ...)`: the
  # module and function are args 2 and 3. Anything else on those modules is
  # unrecognised and rejected.
  defp collect({{:., _, [rpc_mod, call]}, meta, [_node, mod, fun | _]}, acc)
       when rpc_mod in [:erpc, :rpc] and call in [:call, :cast] do
    [target(mod, fun, meta) | acc]
  end

  defp collect({{:., _, [rpc_mod, call]}, meta, _args}, acc) when rpc_mod in [:erpc, :rpc] do
    [{:forbidden, ":#{rpc_mod}.#{call}", meta[:line]} | acc]
  end

  defp collect({{:., _, [{:__aliases__, _, [:Node]}, spawn]}, meta, _}, acc)
       when spawn in [:spawn, :spawn_link, :spawn_monitor] do
    [{:forbidden, "Node.#{spawn}", meta[:line]} | acc]
  end

  # `:erlang.make_fun(:mod, :fun, arity)`: a fun that will be called remotely.
  defp collect({{:., _, [:erlang, :make_fun]}, meta, [mod, fun, _arity]}, acc)
       when is_atom(mod) and is_atom(fun) do
    [{:call, {mod, fun}, meta[:line]} | acc]
  end

  # Any other call or capture of a `:beamtalk_*` module.
  defp collect({{:., _, [mod, fun]}, meta, _args}, acc) when is_atom(mod) and is_atom(fun) do
    if beamtalk_module?(mod), do: [{:call, {mod, fun}, meta[:line]} | acc], else: acc
  end

  defp collect(_node, acc), do: acc

  defp target(mod, fun, meta) when is_atom(mod) and is_atom(fun),
    do: {:call, {mod, fun}, meta[:line]}

  defp target(mod, _fun, meta) when is_atom(mod), do: {:call, {mod, :dynamic}, meta[:line]}
  defp target(_mod, _fun, meta), do: {:dynamic_module, :unknown, meta[:line]}

  # `&:mod.fun/n` anywhere inside an `rpc/…` argument.
  defp captures_in(arg) do
    {_, found} =
      Macro.prewalk(arg, [], fn
        {:&, _, [{:/, _, [{{:., _, [mod, fun]}, _, []}, _arity]}]} = node, acc
        when is_atom(mod) and is_atom(fun) ->
          {node, [{mod, fun} | acc]}

        node, acc ->
          {node, acc}
      end)

    found
  end

  defp beamtalk_module?(mod), do: mod |> Atom.to_string() |> String.starts_with?("beamtalk_")
end
