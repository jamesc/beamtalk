# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.SessionRegistry do
  @moduledoc """
  Per-tab workspace-session registry for the LiveView IDE (BT-2410 Wave 4).

  ## Why this exists

  Each browser tab gets its own isolated workspace-supervised session (proven at
  the raw-distribution level in the BT-2394 spike: tab1 `x=100`, tab2 `x=999`).
  Wave 1 created a *fresh* session on every connected `mount/3` and tore it down
  in `terminate/2`. That is correct for isolation, but it loses two things this
  wave needs:

    * **Session resume.** A LiveView reconnect (transient socket drop, or a page
      reload that re-establishes the WebSocket) mounts a brand-new LiveView
      process. Starting a fresh session there discards the tab's accumulated eval
      state (bindings, spawned actors, loaded classes). We want the reconnecting
      tab to *re-bind* to the session it already had.
    * **No leaks across that gap.** If `terminate/2` immediately closed the
      session, the reconnecting mount would always find it gone. So teardown must
      be *deferred* through a short grace window — long enough to bridge a
      reconnect, short enough that a tab that is really gone does not leak an
      orphaned session.

  This registry owns exactly that lifecycle. It maps a stable, per-tab `token`
  (minted in the browser's `sessionStorage` and replayed on every (re)connect via
  the LiveSocket `params`, see `assets/js/app.js`) to the live workspace session.

  ## Lifecycle

    * `checkout/1` — called on the *connected* mount. If a live session exists for
      the token, its pending reap is cancelled and `{:resumed, session_id, pid}`
      is returned so the LiveView re-binds (resubscribes streams + re-reads
      bindings) instead of starting a new session. Otherwise `:miss`.
    * `register/3` — records a freshly-started session for the token and monitors
      the remote session pid, so a workspace-side death drops the stale entry.
    * `release/1` — called from `terminate/2`. Schedules a grace-period reap; a
      `checkout/1` within the window cancels it (that is the resume). If the
      window elapses with no reconnect, the registry closes the
      workspace-supervised session (`Workspace.close_session/1`) and drops the
      entry — the "no orphaned sessions" guarantee.

  Tokens are isolated per tab, so two tabs never share an entry: multi-tab
  isolation falls straight out of distinct keys.

  The registry is process-side state on the Phoenix node only; the workspace
  needs no changes (sessions are looked up here, never re-derived on the
  workspace, which has no by-id session registry).

  ## Tokens are NOT an auth boundary

  The per-tab token is a random, client-held *resume key*, not a credential. It
  scopes a tab to its own session and lets that tab reconnect to it; it does not
  authenticate the client. In today's single-user localhost Attach model the
  Erlang distribution cookie is the only auth boundary (per the BT-2394 spike's
  auth notes and the deferred auth ADR). A multi-user deployment must add real
  per-user authorization in front of this registry — token possession alone must
  never be allowed to grant access to another user's session.
  """

  use GenServer

  require Logger

  alias BtAttach.Workspace

  # Grace window between a LiveView disconnect and reaping its session. Long
  # enough to bridge a transient socket drop / quick reload, short enough that a
  # genuinely-closed tab does not hold a workspace session for long. Overridable
  # in config (tests use a tiny value to assert reaping without waiting).
  @default_reap_after_ms 30_000

  defmodule Entry do
    @moduledoc false
    @enforce_keys [:session_id, :session_pid]
    defstruct [
      :session_id,
      :session_pid,
      :monitor_ref,
      :reap_timer,
      :reap_tag,
      :window_stash,
      :owner_pid
    ]
  end

  ## Public API

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: Keyword.get(opts, :name, __MODULE__))
  end

  @doc """
  Look up the live session for `token`, cancelling any pending reap.

  Returns `{:resumed, session_id, session_pid}` when a live session is held for
  the token (the reconnect/resume path), or `:miss` when there is none (the
  first-connect path, where the caller starts a fresh session and `register/3`s
  it). A `nil` or non-binary token always misses, so a client that never sent a
  token simply gets fresh, non-resumable sessions.
  """
  @spec checkout(GenServer.server(), term()) ::
          {:resumed, String.t(), pid()} | :miss
  def checkout(server \\ __MODULE__, token)
  def checkout(_server, token) when not is_binary(token), do: :miss
  def checkout(server, token), do: GenServer.call(server, {:checkout, token})

  @doc """
  Record a freshly-started session for `token` and monitor its remote pid.

  Idempotent per token: registering a new session for a token that already holds
  one cancels the old entry's reap and closes the *previous* session, so a token
  never accumulates more than one live session (no leak if a client re-registers).
  """
  @spec register(GenServer.server(), term(), String.t(), pid()) :: :ok
  def register(server \\ __MODULE__, token, session_id, session_pid)

  def register(_server, token, _session_id, _session_pid) when not is_binary(token), do: :ok

  def register(server, token, session_id, session_pid)
      when is_binary(session_id) and is_pid(session_pid) do
    GenServer.call(server, {:register, token, session_id, session_pid})
  end

  @doc """
  Close `token`'s session immediately and forget it (no grace window).

  Used when a mount fails mid-bind, so a half-started session can't linger for the
  grace period. A `nil`/non-binary or unknown token is a harmless no-op.
  """
  @spec discard(GenServer.server(), term()) :: :ok
  def discard(server \\ __MODULE__, token)
  def discard(_server, token) when not is_binary(token), do: :ok
  def discard(server, token), do: GenServer.call(server, {:discard, token})

  @doc """
  Stash `token`'s floating-inspector window roots so a resume can rebuild them.

  Called from the LiveView's `terminate/2` alongside `release/1`. A LiveView
  reconnect mounts a brand-new process whose assigns (including the open window
  list) start empty, so a transient socket drop would otherwise tear down a desk
  full of inspector windows even though the underlying session resumes (BT-2527
  #3). The stash rides the registry entry — Phoenix-node memory that outlives the
  reconnect — and is read back by `window_stash/2` on the resuming mount. A
  `nil`/non-binary or unknown token is a harmless no-op; the stash dies with the
  entry when the session is reaped, so a genuinely-closed tab leaves nothing.
  """
  @spec stash_windows(GenServer.server(), term(), term()) :: :ok
  def stash_windows(server \\ __MODULE__, token, stash)
  def stash_windows(_server, token, _stash) when not is_binary(token), do: :ok
  def stash_windows(server, token, stash), do: GenServer.call(server, {:stash, token, stash})

  @doc """
  Read (and clear) `token`'s stashed window roots, or `nil` when there are none.

  Read once by the resuming mount after `checkout/1` resumes the session; the
  stash is cleared so a later re-render can't replay a stale desk. A
  `nil`/non-binary or unknown token returns `nil`.
  """
  @spec window_stash(GenServer.server(), term()) :: term() | nil
  def window_stash(server \\ __MODULE__, token)
  def window_stash(_server, token) when not is_binary(token), do: nil
  def window_stash(server, token), do: GenServer.call(server, {:take_stash, token})

  @doc """
  Mark `token`'s session releasable: schedule a grace-period reap.

  Called from the LiveView's `terminate/2`. If the same tab reconnects within the
  grace window, the subsequent `checkout/1` cancels the reap and the session is
  resumed. Otherwise the registry closes the workspace session and forgets the
  token. A `nil`/non-binary or unknown token is a harmless no-op.

  Synchronous, and **owner-guarded**: the reap is armed only when the *calling*
  process still owns the session (it was the last to `register`/`checkout` it).
  On a fast reconnect the new LiveView's `checkout/1` can be processed before the
  old LiveView's `terminate/2` reaches here; that `checkout` takes ownership, so
  this now-stale release from the superseded process is ignored rather than arming
  a reap that would tear the just-resumed session down after the grace window.
  """
  @spec release(GenServer.server(), term()) :: :ok
  def release(server \\ __MODULE__, token)
  def release(_server, token) when not is_binary(token), do: :ok
  def release(server, token), do: GenServer.call(server, {:release, token})

  ## GenServer callbacks

  @impl true
  def init(opts) do
    reap_after = Keyword.get(opts, :reap_after_ms, configured_reap_after())
    {:ok, %{entries: %{}, reap_after_ms: reap_after}}
  end

  @impl true
  def handle_call({:checkout, token}, {owner_pid, _ref}, state) do
    case Map.fetch(state.entries, token) do
      {:ok, %Entry{session_id: session_id, session_pid: pid} = entry} ->
        # Resume: cancel any pending reap so the session is not torn down out from
        # under the reconnecting LiveView, and record the resuming LiveView as the
        # new owner — so a stale `release` from the process being superseded can no
        # longer arm a reap against this now-active session.
        entry = cancel_reap(entry)
        entry = %Entry{entry | owner_pid: owner_pid}
        state = put_entry(state, token, entry)
        {:reply, {:resumed, session_id, pid}, state}

      :error ->
        {:reply, :miss, state}
    end
  end

  @impl true
  def handle_call({:register, token, session_id, session_pid}, {owner_pid, _ref}, state) do
    # If this token already had a session (e.g. a re-register without a clean
    # release), close the old one first so we never leak it.
    state = close_existing(state, token)

    ref = Process.monitor(session_pid)

    entry = %Entry{
      session_id: session_id,
      session_pid: session_pid,
      monitor_ref: ref,
      owner_pid: owner_pid
    }

    {:reply, :ok, put_entry(state, token, entry)}
  end

  @impl true
  def handle_call({:discard, token}, _from, state) do
    {:reply, :ok, close_existing(state, token)}
  end

  @impl true
  def handle_call({:stash, token, stash}, _from, state) do
    case Map.fetch(state.entries, token) do
      {:ok, %Entry{} = entry} ->
        {:reply, :ok, put_entry(state, token, %Entry{entry | window_stash: stash})}

      :error ->
        {:reply, :ok, state}
    end
  end

  @impl true
  def handle_call({:take_stash, token}, _from, state) do
    case Map.fetch(state.entries, token) do
      {:ok, %Entry{window_stash: stash} = entry} ->
        {:reply, stash, put_entry(state, token, %Entry{entry | window_stash: nil})}

      :error ->
        {:reply, nil, state}
    end
  end

  @impl true
  def handle_call({:release, token}, {releaser, _ref}, state) do
    case Map.fetch(state.entries, token) do
      {:ok, %Entry{owner_pid: owner} = entry} when owner == releaser ->
        # The releaser still owns the session (no newer LiveView has resumed it):
        # replace any existing reap timer (a release after a release just resets
        # the window) and arm a fresh one. Tag the timer with a unique ref so a
        # stale {:reap, token, _} still in the mailbox (cancel_timer lost the
        # race with an already-fired timer) can't reap this newer window.
        entry = cancel_reap(entry)
        reap_tag = make_ref()
        timer = Process.send_after(self(), {:reap, token, reap_tag}, state.reap_after_ms)

        {:reply, :ok,
         put_entry(state, token, %Entry{entry | reap_timer: timer, reap_tag: reap_tag})}

      {:ok, %Entry{}} ->
        # A newer LiveView has since resumed this session and owns it now; this
        # release is from the superseded process. Ignore it — arming a reap here
        # would tear down the already-resumed, live session after the grace window.
        {:reply, :ok, state}

      :error ->
        {:reply, :ok, state}
    end
  end

  @impl true
  def handle_info({:reap, token, reap_tag}, state) do
    case Map.fetch(state.entries, token) do
      {:ok, %Entry{reap_timer: timer, reap_tag: ^reap_tag} = entry} when timer != nil ->
        # The grace window elapsed without a reconnect: close the
        # workspace-supervised session and forget the token. This is the
        # no-orphaned-sessions guarantee for genuinely-closed tabs.
        demonitor(entry)
        close_session(entry)
        {:noreply, drop_entry(state, token)}

      _ ->
        # Timer was cancelled (resumed) or entry gone — nothing to do.
        {:noreply, state}
    end
  end

  # A monitored remote session pid died (e.g. the workspace itself terminated the
  # session). Drop the now-stale entry; a later checkout will miss and the
  # LiveView will start a fresh session.
  @impl true
  def handle_info({:DOWN, ref, :process, _pid, _reason}, state) do
    state =
      case Enum.find(state.entries, fn {_t, e} -> e.monitor_ref == ref end) do
        {token, entry} ->
          entry = cancel_reap(entry)
          drop_entry(state, token, entry)

        nil ->
          state
      end

    {:noreply, state}
  end

  def handle_info(_msg, state), do: {:noreply, state}

  ## Internals

  defp configured_reap_after do
    Application.get_env(:bt_attach, :session_reap_after_ms, @default_reap_after_ms)
  end

  defp put_entry(state, token, entry) do
    %{state | entries: Map.put(state.entries, token, entry)}
  end

  defp drop_entry(state, token) do
    %{state | entries: Map.delete(state.entries, token)}
  end

  # Drop an entry but cancel its monitor first so we don't leak monitors.
  defp drop_entry(state, token, %Entry{} = entry) do
    demonitor(entry)
    drop_entry(state, token)
  end

  defp close_existing(state, token) do
    case Map.fetch(state.entries, token) do
      {:ok, entry} ->
        entry = cancel_reap(entry)
        demonitor(entry)
        close_session(entry)
        drop_entry(state, token)

      :error ->
        state
    end
  end

  defp cancel_reap(%Entry{reap_timer: nil} = entry), do: entry

  defp cancel_reap(%Entry{reap_timer: timer} = entry) do
    Process.cancel_timer(timer)
    %Entry{entry | reap_timer: nil, reap_tag: nil}
  end

  defp demonitor(%Entry{monitor_ref: nil}), do: :ok

  defp demonitor(%Entry{monitor_ref: ref}) do
    Process.demonitor(ref, [:flush])
    :ok
  end

  # Best-effort close of the workspace-supervised session, performed in a DETACHED
  # process so a slow/hung workspace RPC can never block the registry GenServer.
  #
  # The registry is a shared singleton: every tab's checkout/register goes through
  # its mailbox. `Workspace.close_session/1` is a synchronous `:rpc.call` with no
  # timeout, so closing inline would let one unreachable workspace stall every
  # tab's session lookup (head-of-line blocking). The remote close is fire-and-
  # forget — the registry's own job (forget the entry) is already done — so we
  # spawn it. Errors are non-fatal: the session may already be gone, or the
  # workspace unreachable; either way the entry is dropped.
  defp close_session(%Entry{session_pid: pid}) when is_pid(pid) do
    _ =
      spawn(fn ->
        case Workspace.close_session(pid) do
          :ok -> :ok
          other -> Logger.debug("session reap close returned #{inspect(other)}")
        end
      end)

    :ok
  end

  defp close_session(_entry), do: :ok
end
