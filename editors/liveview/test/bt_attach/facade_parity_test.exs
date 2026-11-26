# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.FacadeParityTest do
  @moduledoc """
  Facade parity check (BT-5201, epic BT-5200).

  Every key in `BtAttach.Facade`'s `@ops` either has an `invoke/3` clause or is
  in `BtAttach.Facade.reserved_ops/0`. The reserved list is the visible record of
  ops that are declared but not implemented, and it can only shrink: the pinned
  ceiling below is the list as of BT-5201, so adding an entry fails here, while
  implementing (or removing) an op forces its removal from the facade's list.
  """
  use ExUnit.Case, async: true

  alias BtAttach.Facade

  # The reserved set when BT-5201 landed, with the issue that retires each. Delete
  # entries as those issues land; never add one.
  @reserved_ceiling [
    # BT-5203
    :load_source,
    # BT-5204
    :info,
    # BT-5205
    :subscribe_actors,
    # BT-5206
    :rotate_cookie
  ]

  @facade_source Path.expand("../../lib/bt_attach/facade.ex", __DIR__)

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

  test "every op in @ops has an invoke/3 clause or is explicitly reserved" do
    unimplemented = Facade.ops() -- implemented_ops()

    assert Enum.sort(unimplemented) == Enum.sort(Facade.reserved_ops()),
           """
           Facade ops without an invoke/3 clause must be exactly Facade.reserved_ops/0.
             unimplemented: #{inspect(Enum.sort(unimplemented))}
             reserved:      #{inspect(Enum.sort(Facade.reserved_ops()))}
           Implement the op through dispatch (or remove it from @ops); do not add it to
           the reserved list.
           """
  end

  test "an invoke/3 clause exists only for ops in the vocabulary" do
    # `dispatch/3` rejects off-vocabulary ops before `invoke/3`, so a clause for
    # one is dead code that looks like a capability.
    assert implemented_ops() -- Facade.ops() == []
  end

  test "the reserved list can only shrink" do
    assert Facade.reserved_ops() -- @reserved_ceiling == [],
           "Facade.reserved_ops/0 grew past its BT-5201 ceiling"
  end

  test "reserved ops answer unsupported_op once authorized, with no workspace call" do
    for op <- Facade.reserved_ops() do
      assert {:error, {:unsupported_op, ^op}} = Facade.dispatch(op, %{}, %{role: :owner})
    end
  end
end
