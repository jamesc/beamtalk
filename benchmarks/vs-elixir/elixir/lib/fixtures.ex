# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Data and behaviour paired with the Beamtalk fixtures in ../beamtalk/src.

defmodule VeBench.Point do
  defstruct x: 0, y: 0

  def x(%__MODULE__{x: x}), do: x

  def sum(%__MODULE__{x: x, y: y}), do: x + y
end

defmodule VeBench.Square do
  defstruct side: 3

  def area(%__MODULE__{side: s}), do: s * s
end

defprotocol VeBench.Area do
  def area(shape)
end

defimpl VeBench.Area, for: VeBench.Square do
  def area(%VeBench.Square{side: s}), do: s * s
end

# A second implementation, so the consolidated protocol has more than one
# clause to choose from (as a real protocol would).
defimpl VeBench.Area, for: VeBench.Point do
  def area(%VeBench.Point{x: x, y: y}), do: x * y
end

defmodule VeBench.Base do
  defstruct v: 1

  def base(%{v: v}), do: v
end

# "Subclass": the inherited method is a delegate to the base module.
defmodule VeBench.Sub do
  defstruct v: 1

  defdelegate base(s), to: VeBench.Base

  def via_self(s), do: base(s)
end

defmodule VeBench.Math do
  def double(x), do: x * 2
end

defmodule VeBench.Helpers do
  def ok(x), do: {:ok, x}
end

defmodule VeBench.Counter do
  use GenServer

  def start, do: GenServer.start(__MODULE__, 0)

  @impl true
  def init(n), do: {:ok, n}

  @impl true
  def handle_call(:count, _from, n), do: {:reply, n, n}
  def handle_call(:increment, _from, n), do: {:reply, n + 1, n + 1}
  def handle_call(:probe, _from, n), do: {:reply, is_integer(n), n}

  @impl true
  def handle_cast(:increment, n), do: {:noreply, n + 1}
end
