# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.FacadeParityTest do
  @moduledoc """
  Facade parity checks (BT-5201, closed out by BT-5206, epic BT-5200).

    * Every key in `BtAttach.Facade`'s `@ops` has an `invoke/3` clause. There is
      no reserved vocabulary: an op is implemented by dispatch or it is not in the
      catalog.
    * Every op names what it dispatches in the runtime (`Facade.wire_ops/1`), and
      its capability class (`:read | :execute | :admin`) equals the `access` class
      of each wire operation in the runtime's one op table
      (`beamtalk_repl_op_table`, read here through its pinned JSON snapshot).
      A new op cannot be added without a class, and a class cannot drift from the
      table.
    * The table gives every operation an `access` class, so a new runtime
      operation cannot be added without deciding who may call it.
  """
  use ExUnit.Case, async: true

  alias BtAttach.Facade

  @facade_source Path.expand("../../lib/bt_attach/facade.ex", __DIR__)
  @op_table Path.expand(
              "../../../../runtime/apps/beamtalk_workspace/test/fixtures/repl_op_table.json",
              __DIR__
            )

  # The ops that have a `defp invoke(:op, ...)` clause, read from the source: a
  # clause is the only way an op is implemented, and the catch-all clause names
  # a variable, not an atom.
  defp implemented_ops do
    {_ast, ops} =
      @facade_source
      |> File.read!()
      |> Code.string_to_quoted!()
      |> Macro.prewalk([], fn
        {:defp, _, [{:invoke, _, [op | _]} | _]} = node, acc when is_atom(op) ->
          {node, [op | acc]}

        {:defp, _, [{:when, _, [{:invoke, _, [op | _]} | _]} | _]} = node, acc when is_atom(op) ->
          {node, [op | acc]}

        node, acc ->
          {node, acc}
      end)

    ops |> Enum.uniq() |> Enum.sort()
  end

  # `%{"eval" => :execute, ...}`: the runtime's access class of each operation.
  defp table_access do
    @op_table
    |> File.read!()
    |> :json.decode()
    |> Map.new(fn %{"op" => op, "access" => access} -> {op, String.to_existing_atom(access)} end)
  end

  test "every op in @ops has an invoke/3 clause (no reserved vocabulary)" do
    assert Facade.ops() -- implemented_ops() == [],
           """
           Facade ops without an invoke/3 clause: #{inspect(Facade.ops() -- implemented_ops())}
           Implement the op through dispatch, or remove it from @ops. There is no reserved
           list (BT-5206).
           """
  end

  test "the facade has no reserved-op list any more" do
    refute function_exported?(Facade, :reserved_ops, 0)
  end

  test "an invoke/3 clause exists only for ops in the vocabulary" do
    # `dispatch/3` rejects off-vocabulary ops before `invoke/3`, so a clause for
    # one is dead code that looks like a capability.
    assert implemented_ops() -- Facade.ops() == []
  end

  describe "capability classes against the runtime op table" do
    test "every op says what it dispatches" do
      missing = Enum.reject(Facade.ops(), &Facade.wire_ops/1)

      assert missing == [],
             "ops with no @wire_ops entry (add the wire op, :local or :fixed_eval): #{inspect(missing)}"

      for op <- Facade.ops() do
        assert match?([_ | _], Facade.wire_ops(op)) or
                 Facade.wire_ops(op) in [:local, :fixed_eval],
               "#{op}: @wire_ops must be a non-empty list, :local or :fixed_eval"
      end
    end

    test "each op's class equals the access class of every wire operation it sends" do
      table = table_access()

      for op <- Facade.ops(), [_ | _] = wire <- [Facade.wire_ops(op)] do
        for wire_op <- wire do
          assert Map.has_key?(table, wire_op),
                 "#{op} sends `#{wire_op}`, which is not in the runtime op table"

          assert Facade.capability(op) == table[wire_op],
                 "#{op} is #{inspect(Facade.capability(op))} but `#{wire_op}` is " <>
                   "#{inspect(table[wire_op])} in the runtime op table"
        end
      end
    end

    test "the only ops that skip the check are subscriptions, session bindings and fixed-expression reads" do
      unchecked =
        for op <- Facade.ops(), not is_list(Facade.wire_ops(op)), do: op

      # `subscribe_*` and `unsubscribe_*` ride the subscription facade, not an op.
      subscriptions = for op <- unchecked, String.contains?(to_string(op), "subscribe"), do: op

      assert Enum.sort(unchecked -- subscriptions) ==
               Enum.sort(~w(bindings nodes processes processes_system)a),
             "a new op skips the table check; give it a wire operation if it has one"
    end

    test "a fixed-expression op is never more privileged than eval allows, and stays what it was" do
      # `processes_system` exposes the whole-node process map (ADR 0092): execute.
      assert Facade.capability(:processes_system) == :execute
      assert Facade.capability(:processes) == :read
      assert Facade.capability(:nodes) == :read
    end

    test "every runtime operation has an access class" do
      for {op, access} <- table_access() do
        assert access in [:read, :execute, :admin], "#{op} has no valid access class"
      end
    end

    test "the guard guards itself: a mismatch is reported" do
      # A class the table disagrees with would fail the per-op check above; prove
      # the comparison is not vacuous by checking a known-different pair.
      table = table_access()
      assert table["eval"] == :execute
      assert table["browse-classes"] == :read
      assert table["kill"] == :admin
      refute Facade.capability(:browse_classes) == table["eval"]
    end
  end
end
