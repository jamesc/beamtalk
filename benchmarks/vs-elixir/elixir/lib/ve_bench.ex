# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule VeBench.Loop do
  @moduledoc false

  # `defbench name, ctx, do: op` defines `name(n, ctx)`, which evaluates `op`
  # n times in the loop the Beamtalk side compiles `n timesRepeat: [last :=
  # op]` to: count up from 1 with `=<`, carry the last result as a loop
  # argument (so `op` is live and cannot be dropped as dead code).
  defmacro defbench(name, ctx, do: body) do
    loop = :"#{name}_loop"

    quote do
      def unquote(name)(n, ctx), do: unquote(loop)(1, n, ctx, 0)

      defp unquote(loop)(i, n, unquote(ctx) = ctx, _last) when i <= n,
        do: unquote(loop)(i + 1, n, ctx, unquote(body))

      defp unquote(loop)(_i, _n, _ctx, last), do: last
    end
  end
end

defmodule VeBench do
  @moduledoc """
  The Elixir half of benchmarks/vs-elixir: one function per row, each
  `row(n, ctx)` running the operation n times. Row names match the Beamtalk
  selectors in ../beamtalk/src/vb_bench.bt.
  """
  import VeBench.Loop

  alias VeBench.{Area, Counter, Helpers, Math, Point, Sub}

  # Named constants as module attributes (rows 3k-3m).
  @protocol_error 1
  @name "protocol error"
  @tag :protocol_error

  # === Fixtures ===

  def point, do: %Point{x: 1, y: 2}
  def square, do: %VeBench.Square{}
  def sub, do: %Sub{}
  def counter, do: elem(Counter.start(), 1)
  def sub_counter, do: elem(VeBench.SubCounter.start(), 1)
  def pool, do: elem(DynamicSupervisor.start_link(strategy: :one_for_one), 1)
  def capped_pool, do: elem(DynamicSupervisor.start_link(strategy: :one_for_one, max_children: 8), 1)
  def adder, do: fn x -> x + 1 end
  def list100, do: Enum.to_list(1..100)
  def dict, do: %{a: 1, b: 2, k: 3}
  def string, do: "hello world"
  def tuple, do: {1, 2}
  def math_module, do: Math
  def ok_result, do: {:ok, 7}

  # === Loop overhead ===

  defbench(:baseline, _, do: 1)

  # === 1. Getter ===

  defbench(:getter_dot, p, do: p.x)
  defbench(:getter_fun, p, do: Point.x(p))
  defbench(:getter_match, %Point{x: x}, do: x)
  defbench(:point_sum, p, do: Point.sum(p))

  # === 2. Struct update and construction ===

  defbench(:with_x, p, do: %{p | x: 3})
  defbench(:construct, a, do: %Point{x: a, y: 4})
  defbench(:construct_map, a, do: struct!(Point, %{x: a, y: 4}))
  defbench(:mapset_new, _, do: MapSet.new())

  # === 3. Remote call ===

  defbench(:remote_call, _, do: Math.double(21))
  defbench(:dyn_remote_call, mod, do: mod.double(21))

  # Rows 3k-3m: a named constant as a remote call, and as a module attribute
  # (inlined at compile time, what a folded constant send aims for).
  defbench(:remote_const_int, _, do: Math.protocol_error())
  defbench(:remote_const_string, _, do: Math.name())
  defbench(:remote_const_atom, _, do: Math.tag())
  defbench(:module_attr_int, _, do: @protocol_error)
  defbench(:module_attr_string, _, do: @name)
  defbench(:module_attr_atom, _, do: @tag)

  # === 4. Dynamic dispatch ===

  defbench(:protocol, s, do: Area.area(s))
  defbench(:apply3, {mod, s}, do: apply(mod, :area, [s]))
  defbench(:dyn_module, {mod, s}, do: mod.area(s))
  defbench(:square_area, s, do: VeBench.Square.area(s))

  # === 5. Inherited method ===

  defbench(:delegate, s, do: Sub.base(s))
  defbench(:delegate_via_self, s, do: Sub.via_self(s))
  defbench(:has_key, s, do: Map.has_key?(s, :v))

  # === 6. Funs and Enum over a 100-element list ===

  defbench(:fun_call, f, do: f.(41))
  defbench(:enum_reduce_sum, l, do: Enum.reduce(l, 0, fn e, acc -> acc + e end))
  defbench(:enum_each, l, do: Enum.each(l, fn e -> e end))
  defbench(:enum_map, l, do: Enum.map(l, fn e -> e * 2 end))
  defbench(:lists_foldl, l, do: :lists.foldl(fn e, acc -> acc + e end, 0, l))

  # === 15. DynamicSupervisor start_child + terminate_child (BT-4502) ===

  def pool_start_terminate(n, pool), do: pool_loop(1, n, pool)

  defp pool_loop(i, n, pool) when i <= n do
    spec = %{id: Counter, start: {Counter, :start_link, [0]}, restart: :temporary}
    {:ok, child} = DynamicSupervisor.start_child(pool, spec)
    :ok = DynamicSupervisor.terminate_child(pool, child)
    pool_loop(i + 1, n, pool)
  end

  defp pool_loop(_i, _n, pool), do: DynamicSupervisor.count_children(pool).active

  # === 7. GenServer ===

  defbench(:gen_call, pid, do: GenServer.call(pid, :count))
  defbench(:gen_update, pid, do: GenServer.call(pid, :increment))
  defbench(:gen_probe, pid, do: GenServer.call(pid, :probe))
  defbench(:gen_sum_of, pid, do: GenServer.call(pid, {:sum_of, %Point{x: 1, y: 2}}))
  defbench(:gen_via_self, pid, do: GenServer.call(pid, :via_self))

  # n casts, then one call so every cast has been handled.
  def gen_cast(n, pid) do
    cast_loop(1, n, pid)
    GenServer.call(pid, :count)
  end

  defp cast_loop(i, n, pid) when i <= n do
    GenServer.cast(pid, :increment)
    cast_loop(i + 1, n, pid)
  end

  defp cast_loop(_i, _n, _pid), do: :ok

  # === 8. Errors ===

  defbench :try_no_raise, n do
    try do
      n + 1
    rescue
      _ -> 0
    end
  end

  defbench :try_raise, _ do
    try do
      raise "boom"
    rescue
      _ -> 1
    end
  end

  # === 9. {:ok, x} ===

  defbench :ok_match, n do
    case Helpers.ok(n) do
      {:ok, v} -> v
    end
  end

  defbench(:ok_is_ok, r, do: match?({:ok, _}, r))

  # === 10. Collections and strings ===

  defbench(:map_put, d, do: Map.put(d, :k, 7))
  defbench(:map_get, d, do: Map.get(d, :k))
  defbench(:list_append, l, do: l ++ [7])
  defbench(:list_member, l, do: Enum.member?(l, 50))
  defbench(:binary_concat, s, do: s <> "!")

  # === 11. Integer arithmetic ===

  defbench(:sum_reduce_range, _, do: Enum.reduce(1..100, 0, fn i, acc -> acc + i end))
  defbench(:sum_recursive, _, do: sum(1, 100, 0))

  defp sum(i, n, acc) when i <= n, do: sum(i + 1, n, acc + i)
  defp sum(_i, _n, acc), do: acc

  defbench(:add, a, do: a + 1)
  defbench(:if_else, a, do: if(a > 0, do: 1, else: 2))
  defbench(:less, a, do: a < 10)
  defbench(:while_to, a, do: count_to(0, a))
  defbench(:max_of, a, do: max(a, 3))

  defp count_to(i, a) when i < a, do: count_to(i + 1, a)
  defp count_to(i, _a), do: i

  # === 12. Struct checks ===

  defbench(:struct_of, p, do: p.__struct__)
  defbench(:is_struct_check, p, do: is_struct(p, Point))
  defbench(:is_binary_check, s, do: is_binary(s))

  # === 14. Guarded static dispatch baselines (BT-4292) ===

  defbench(:tuple_elem, t, do: elem(t, 0))
  defbench(:module_ref, _, do: Point)

  # === 13. FFI calls (BT-4153) ===

  defbench(:apply_phash2, a, do: apply(:erlang, :phash2, [a]))
  defbench(:direct_phash2, a, do: :erlang.phash2(a))
  defbench(:apply_integer_to_binary, a, do: apply(:erlang, :integer_to_binary, [a]))
  defbench(:direct_integer_to_binary, a, do: :erlang.integer_to_binary(a))
  defbench(:apply_maps_find, d, do: apply(:maps, :find, [:k, d]))
  defbench(:direct_maps_find, d, do: :maps.find(:k, d))
  # No direct-call alternate for ffi_ok: vs_ffi (the Beamtalk package's
  # native/ module) is compiled after the mix project, so a static
  # :vs_ffi.ffi_ok() warns as undefined. (Verified equivalent anyway: both
  # cost 2 reds / 0 words.)
  defbench(:apply_ffi_ok, _, do: apply(:vs_ffi, :ffi_ok, []))
end
