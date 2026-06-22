# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.Workspace do
  @moduledoc """
  Attach-topology client for a running Beamtalk workspace BEAM node.

  This is the entire surface the Phoenix side needs to talk to the workspace:
  it joins the workspace's Erlang distribution mesh with the shared cookie,
  then drives the workspace via `:rpc` and native cross-node message passing.

  Topology (BT-2394): **Attach** — Phoenix runs as its own BEAM node and
  connects to the workspace node over Erlang distribution, the same way
  Livebook attaches to a remote node. No new wire protocol: we reuse the exact
  same curated op layer the WebSocket handler (`beamtalk_ws_handler`) calls.

  ## Term contract, not JSON (BT-2399)

  Eval goes through the term-returning op-layer seam
  (`beamtalk_repl_ops:dispatch/4`), which returns a live Erlang term
  (`op_result()`), *not* the JSON the browser receives. Over distribution that
  means `Counter spawn` hands back a real, messageable remote pid wrapped in a
  `{:beamtalk_object, class, module, pid}` tuple, not a flattened display
  string. JSON encoding stays at the WebSocket transport edge only. See
  `docs/development/repl-op-term-contract.md`.

  Display (`render_term/1`) is a *separate* concern from the data path: it
  reuses the workspace's own `beamtalk_repl_json:term_to_json/1` formatter — the
  same one the browser WebSocket protocol uses — so the LiveView shows
  byte-identical values to the Phase-1 browser workspace, surface-consistent for
  free, while `eval/2` still returns the live term.

  ## Subscriptions (BT-2399 facade)

  Transcript (and the other live push streams) are subscribed through the stable
  `beamtalk_repl_subscriptions` facade rather than by casting
  `{subscribe, self()}` tuples at gen_servers (which is what the BT-2394 spike
  did as a shortcut). Because the LiveView pid is location-transparent over
  distribution, the facade's explicit-pid form registers the LiveView's own pid
  and the workspace pushes `{:transcript_output, text}` messages to it directly.

  Configuration comes from the environment so this stays decoupled from the
  workspace's own config:

    * `BT_WORKSPACE_NODE`   — e.g. `beamtalk_workspace_spike@localhost`
    * `BT_WORKSPACE_COOKIE` — contents of `~/.beamtalk/workspaces/<id>/vm.args`
                              (`-setcookie <token>`), token only
  """

  require Logger

  @doc "The configured workspace node name as an atom."
  def node_name do
    System.get_env("BT_WORKSPACE_NODE", "beamtalk_workspace_spike@localhost")
    |> String.to_atom()
  end

  @doc """
  Ensure this Phoenix node is distributed, shares the workspace cookie, and is
  connected to the workspace node. Idempotent — safe to call on every mount.
  """
  def connect do
    ensure_distributed()
    set_cookie()

    case Node.connect(node_name()) do
      true -> :ok
      reason -> {:error, {:connect_failed, node_name(), reason}}
    end
  end

  @doc """
  Create a fresh, workspace-supervised REPL session. Each LiveView mount gets
  its own session, so two browser tabs have isolated bindings (proven in the
  BT-2394 spike: tab1 `x = 100`, tab2 `x = 999`). Session state — bindings and
  loaded classes — persists across evals within the session for the lifetime of
  the LiveView process.

  Returns the remote session pid, or `{:error, reason}`.
  """
  def start_session(session_id) when is_binary(session_id) do
    start_session(session_id, %{kind: "liveview"})
  end

  @doc """
  Create a fresh session carrying origin/debug metadata.

  `meta` is a map surfaced by `Workspace sessions` / `Session info` on the
  workspace side; the LiveView passes `kind: "liveview"` plus, where known, the
  Phoenix `node` and authenticated `user`, so an operator can see which front
  (and which user) opened a session. The `kind` is normalised on the workspace
  side, so an unexpected value is harmless.
  """
  def start_session(session_id, meta) when is_binary(session_id) and is_map(meta) do
    case rpc(:beamtalk_session_sup, :start_session, [session_id, meta]) do
      {:ok, pid} when is_pid(pid) -> pid
      {:badrpc, reason} -> {:error, {:unreachable, reason}}
      other -> {:error, other}
    end
  end

  @doc """
  Stop a workspace-supervised session started by `start_session/1`.

  The session is owned by the workspace's `beamtalk_session_sup`, so it does not
  go away just because the LiveView process exits; we must ask the supervisor to
  terminate it or it leaks one orphaned session per mount/reconnect. Idempotent
  and best-effort: a session that is already gone returns `{:error, :not_found}`
  and an unreachable workspace returns `{:badrpc, _}` — both are non-fatal at the
  call site (the LiveView is terminating anyway).
  """
  def close_session(session_pid) when is_pid(session_pid) do
    rpc(:beamtalk_session_sup, :stop_session, [session_pid])
  end

  @doc """
  Check whether a workspace session pid is still alive (BT-2410 Wave 4).

  Used by the resume path: before re-binding a reconnecting LiveView to a session
  the Phoenix-side registry still remembers, we confirm the remote session
  process actually exists — the workspace could have restarted between the
  disconnect and the reconnect, leaving the registry entry stale.

  `is_process_alive/1` is evaluated **on the workspace node**, where the session
  pid is local (a remote `is_process_alive/1` would `badarg`). Returns a boolean
  for a reachable workspace; an unreachable workspace yields `{:badrpc, _}`, which
  the caller treats as not-alive (resume falls back to a fresh session).
  """
  def session_alive?(session_pid) when is_pid(session_pid) do
    case rpc(:erlang, :is_process_alive, [session_pid]) do
      result when is_boolean(result) -> result
      {:badrpc, _reason} -> false
    end
  end

  @doc """
  Count the workspace-supervised sessions currently alive (BT-2410 Wave 4).

  A read-only probe of `beamtalk_session_sup`'s active children, used by the
  session-teardown tests to assert that closing a LiveView (after the resume
  grace window) leaves no orphaned session. Returns the active-child count for a
  reachable workspace, or `{:error, {:unreachable, _}}` otherwise.
  """
  def session_count do
    case rpc(:supervisor, :count_children, [:beamtalk_session_sup]) do
      counts when is_list(counts) ->
        Keyword.get(counts, :active, 0)

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}
    end
  end

  @doc """
  Evaluate a Beamtalk expression in `session_pid` through the curated op layer's
  term-returning seam (`beamtalk_repl_ops:dispatch/4`, BT-2399).

  Returns the **live Erlang term** the workspace produced — no JSON. The term
  contract is the only coupling between Phoenix and the workspace:

    * success → `{:ok, term, output, warnings}`
    * error   → `{:error, reason_term, output, warnings}` where `reason_term` is
      the structured `#beamtalk_error{}` record (a tuple tagged `:beamtalk_error`,
      so no shared `.hrl` is needed)

  `output` is captured stdout (`String.t/0`) and `warnings` is a list of strings.
  """
  def eval(session_pid, expression) when is_pid(session_pid) and is_binary(expression) do
    case dispatch_eval(session_pid, expression) do
      {:ok, value, output, warnings} ->
        {:ok, value, to_string(output), Enum.map(warnings, &to_string/1)}

      # Term contract: eval may attach captured output + warnings to an error.
      {:error, reason, output, warnings} ->
        {:error, reason, to_string(output), Enum.map(warnings, &to_string/1)}

      {:error, reason} ->
        {:error, reason, "", []}

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}, "", []}

      # The op contract is a small tagged union; treat anything else as a data
      # error rather than letting a CaseClauseError crash the LiveView.
      other ->
        {:error, {:unexpected_reply, other}, "", []}
    end
  end

  @doc """
  Snapshot the live supervision tree (ADR 0092) as a list of node maps.

  `scope` is `"default"` (the workspace tree with runtime plumbing filtered — the
  safe Read view) or `"system"` (everything, including runtime internals — the
  privileged whole-node view; RBAC gates the `processes_system` op to Owners).
  Surfaced through the shared eval seam so the structured node data is identical
  to every other surface. Returns `{:ok, nodes}` (a list of node maps) or
  `{:error, reason}`.
  """
  @spec supervision_tree(pid(), String.t()) :: {:ok, term()} | {:error, term()}
  def supervision_tree(session_pid, scope)
      when is_pid(session_pid) and scope in ["default", "system"] do
    expression = "ProcessNavigation #{scope} tree asDictionaries"

    case eval(session_pid, expression) do
      {:ok, value, _output, _warnings} -> {:ok, value}
      {:error, reason, _output, _warnings} -> {:error, reason}
    end
  end

  # ── browse-surface: the System Browser data source (ADR 0096, BT-2488) ──────

  @doc """
  List every class in scope as `ClassRow`s for the System Browser class tree
  (ADR 0096 op 1, `browse-classes`). Each row carries `name`, `superclass`,
  `category`, first-line `comment`, `sealed`/`abstract`/`internal`,
  `source_file`, and `origin` (`both` | `static` | `runtime` — the image/disk
  divergence tag, never merged away).

  Returns the live `{:value, rows}` term verbatim — the rows are already a
  wire-shaped JSON value, so no JSON crosses the Attach path here either.
  Returns `{:error, reason}` on a dispatch failure.
  """
  @spec browse_classes() :: {:value, term()} | {:error, term()}
  def browse_classes, do: dispatch_browse("browse-classes", %{})

  @doc """
  Return the selectors of `class`/`side` grouped by protocol for the System
  Browser protocol + selector panes (ADR 0096 op 2, `browse-protocols`). `side`
  is `"instance"` | `"class"`. Each selector row carries `line`,
  `source_status` (xref tag — `indexed` | `synthetic` | `unindexed_runtime_fun`),
  and `origin`. An unknown class / bad side comes back as a structured
  `#beamtalk_error{}` (`{:error, reason}`).
  """
  @spec browse_protocols(String.t(), String.t()) :: {:value, term()} | {:error, term()}
  def browse_protocols(class, side) when is_binary(class) and is_binary(side),
    do: dispatch_browse("browse-protocols", %{"class" => class, "side" => side})

  @doc """
  Return one method's image-accurate source for the System Browser method-source
  pane (ADR 0096 op 3, `browse-method-source`). The result carries `source`
  (`null` for a sourceless runtime method), `source_status`, `origin`, and
  `disk_differs` (`true` when the live/patched body differs from the static/disk
  source — an unflushed `>>` patch; `null` when there is no static source to
  compare). `{:error, reason}` for an unknown class.
  """
  @spec browse_method_source(String.t(), String.t(), String.t()) ::
          {:value, term()} | {:error, term()}
  def browse_method_source(class, side, selector)
      when is_binary(class) and is_binary(side) and is_binary(selector) do
    dispatch_browse("browse-method-source", %{
      "class" => class,
      "side" => side,
      "selector" => selector
    })
  end

  @doc """
  Return `class`'s definition for the System Browser class-definition pane
  (ADR 0096 op 4, `browse-class-definition`): the class header `definition`
  (`null` for a file-less ClassBuilder class), `state` slots (name + default,
  field reflection — no user code), full `comment`, `origin`, and
  `disk_differs`. `{:error, reason}` for an unknown class.
  """
  @spec browse_class_definition(String.t()) :: {:value, term()} | {:error, term()}
  def browse_class_definition(class) when is_binary(class),
    do: dispatch_browse("browse-class-definition", %{"class" => class})

  @doc """
  Return the backing Erlang source of a `native:` class for the System Browser's
  read-only native pane (BT-2578, `browse-native-source`). The result carries
  `backing_module`, `source_file` (`null` when the `.erl` is not shipped),
  `source_origin`, `editable` (`project` ⇒ `true`), `content` (`null` =
  "source not available"), and a best-effort `handle_call` clause line-map
  (`clauses`). When `selector` is given, `selected_clause` points at the matching
  clause (or `null` — the delegate's work may live in `handle_info`). A non-native
  class comes back as a structured `#beamtalk_error{}` (`{:error, reason}`).
  """
  @spec browse_native_source(String.t(), String.t() | nil) ::
          {:value, term()} | {:error, term()}
  def browse_native_source(class, selector \\ nil)

  def browse_native_source(class, nil) when is_binary(class),
    do: dispatch_browse("browse-native-source", %{"class" => class})

  def browse_native_source(class, selector) when is_binary(class) and is_binary(selector),
    do: dispatch_browse("browse-native-source", %{"class" => class, "selector" => selector})

  # ── navigation-surface: senders/implementors + omni-search index ────────────
  #
  # BT-2495 (Cockpit Phase 3): two navigation aids ride the SAME term-returning
  # read ops the LSP/MCP navigation channel already exposes (ADR 0096 thin facade,
  # no new runtime work) — `nav-query` (senders/implementors call sites, backed by
  # the maintained `beamtalk_xref` index, BT-2201/BT-2239) and `nav-symbols` (the
  # bulk class+method outline, BT-2244). Both go through `dispatch_browse/2`, so a
  # `{:badrpc, :nodedown}` degrades to `{:error, {:unreachable, _}}` like every
  # other Attach read, and the `{:value, _}` rows arrive live (no JSON edge).

  @doc """
  Return the call sites that **send** `selector` (`nav-query` `kind=senders`,
  BT-2495) — the Senders popover's data source. Each row in the `"sites"` list
  carries `class`, `class_side`, `method`, `line`, and `source_file`
  (`null` for a sourceless class). Backed by the `beamtalk_xref` index, so an
  unknown selector resolves to an empty site list, not an error.

  Returns the live `{:value, %{"sites" => rows}}` term verbatim, or
  `{:error, reason}` on a dispatch failure.
  """
  @spec senders_of(String.t()) :: {:value, term()} | {:error, term()}
  def senders_of(selector) when is_binary(selector),
    do: dispatch_browse("nav-query", %{"kind" => "senders", "selector" => selector})

  @doc """
  Return the classes that **implement** `selector` (`nav-query`
  `kind=implementors`, BT-2495) — the Implementors popover's data source. One row
  per implementing class/metaclass in `"sites"` (`class`, `class_side`, `method`,
  `line`, `source_file`). Like `senders_of/1`, an unknown selector yields an
  empty list rather than an error.
  """
  @spec implementors_of(String.t()) :: {:value, term()} | {:error, term()}
  def implementors_of(selector) when is_binary(selector),
    do: dispatch_browse("nav-query", %{"kind" => "implementors", "selector" => selector})

  @doc """
  Return the workspace symbol outline (`nav-symbols`, BT-2495) — the omni-search
  index over every loaded class **and** its locally-defined selectors. The result
  carries `"classes"` (one row per class: `name`, `source_file`, `line`, and a
  `methods` list of `%{"selector", "class_side", "line"}`), the single read the
  top-bar omni search filters to match classes + selectors.

  `scope` is `"all"` (every loaded class, including REPL-only / source-less — the
  omni-search default so a freshly-defined class is findable) or `"user"`
  (source-backed classes only). Returns the live `{:value, %{"classes" => rows}}`
  term verbatim, or `{:error, reason}`.
  """
  @spec symbol_index(String.t()) :: {:value, term()} | {:error, term()}
  def symbol_index(scope \\ "all") when is_binary(scope),
    do: dispatch_browse("nav-symbols", %{"scope" => scope})

  # ── completion-surface: backend-driven autocomplete (BT-2544) ───────────────

  @doc """
  Return backend completion candidates for `code` — the current editor line up
  to the caret — in `session_pid`'s live context (BT-2544). This is the
  CodeMirror editors' autocomplete data source.

  Reuses the REPL `complete` op (BT-783), so the cockpit shares the CLI REPL's
  receiver-aware ranking rather than forking it: a bare prefix completes class
  names / keywords; a recognised receiver (`Integer `, a bound actor variable)
  completes its selectors. Binding-aware completion works because `session_pid`
  is passed as the op's session, so instance-method completion resolves the
  session's live bindings (BT-1045). Completion runs **no user code** (pure
  reflection / index), consistent with the facade's `:read` capability — so the
  Observer role completes too.

  `code` is the line text up to the caret; the op infers receiver + prefix from
  it. The cursor offset is the end of that text, passed so the op takes its
  context-aware path (BT-783).

  Returns the live `{completions, _}` term as `{:ok, [String.t()]}` (ranked,
  possibly empty), or `{:error, reason}` on a dispatch failure — JSON never
  crosses the Attach path here either.
  """
  @spec complete(pid(), String.t()) :: {:ok, [String.t()]} | {:error, term()}
  def complete(session_pid, code) when is_pid(session_pid) and is_binary(code) do
    case dispatch_complete(session_pid, code) do
      {:completions, list} when is_list(list) ->
        {:ok, Enum.map(list, &to_string/1)}

      {:error, reason} ->
        {:error, reason}

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}

      other ->
        {:error, {:unexpected_reply, other}}
    end
  end

  @doc """
  Return live-image hover documentation for `code` — the editor line up to the
  hovered token — in `session_pid`'s live context (BT-2555). This is the
  CodeMirror editors' hover data source.

  Reuses the `hover` op, which resolves the hovered token against the **live
  class registry** (a class name → its docs, a `Receiver selector` pair → that
  method's docs) and formats it via `beamtalk_repl_docs` — the same live doc
  engine the CLI REPL `:help` uses. Because it reads the running image rather
  than on-disk source, hover covers REPL-defined and live-patched classes the
  static LSP hover (`hover_provider.rs`) cannot see. Hover runs **no user code**
  (pure reflection), consistent with the facade's `:read` capability.

  `session_pid` is passed as the op's session so an instance receiver that is a
  bound variable classifies (BT-1045), mirroring `complete/2`.

  Returns the formatted markdown as `{:ok, String.t()}` (`""` when nothing
  resolves — the client then shows no tooltip), or `{:error, reason}` on a
  dispatch failure — JSON never crosses the Attach path here either.
  """
  @spec hover(pid(), String.t()) :: {:ok, String.t()} | {:error, term()}
  def hover(session_pid, code) when is_pid(session_pid) and is_binary(code) do
    case dispatch_hover(session_pid, code) do
      {:docs, docs} when is_binary(docs) ->
        {:ok, docs}

      {:error, reason} ->
        {:error, reason}

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}

      other ->
        {:error, {:unexpected_reply, other}}
    end
  end

  @doc """
  Return parse-only diagnostics for `code` — the full editor buffer — via the
  `diagnostics` op (BT-2556). This is the CodeMirror editors' `@codemirror/lint`
  data source.

  Reuses the compiler's **side-effect-free** `diagnostics` path (the Rust port's
  `diagnostics` command behind `beamtalk_compiler:diagnostics/2`): the buffer is
  parsed and semantically checked for DIAGNOSIS ONLY — it generates no code,
  installs no module, mutates no image state, appends nothing to the ChangeLog,
  and runs no user code. That makes it safe to fire on every keystroke and
  consistent with the facade's `:read` capability (the Observer sees diagnostics).

  Unlike `complete/2` and `hover/2`, no `session_pid` is involved: diagnostics
  analyse the buffer in isolation (no receiver / binding resolution), so the op
  is workspace-global and `self()` is passed only to satisfy `dispatch/4`'s arity
  (mirroring the browse ops).

  Each diagnostic is normalised to a binary-keyed map
  `%{"from" => byte_offset, "to" => byte_offset, "severity" => bin, "message" => bin}`
  — `start`/`end` are renamed to `from`/`to` so the client maps straight onto a
  CodeMirror `Diagnostic`. Returns `{:ok, [map()]}` (possibly empty), or
  `{:error, reason}` on a dispatch failure — JSON never crosses the Attach path.
  """
  @spec diagnostics(String.t()) :: {:ok, [map()]} | {:error, term()}
  def diagnostics(code) when is_binary(code), do: diagnostics(code, "expression")

  @doc """
  Like `diagnostics/1`, but `mode` selects the parse grammar (BT-2569):
  `"expression"` (default — a top-level script, the Workspace + REPL editors) or
  `"method"` (a bare method body — the System Browser method editor, where the
  `=>` body separator is not a valid top-level token, so the default grammar
  reports a false `expected expression, found ⇒`). Method mode is parse-only:
  a method has no class context here, so semantic checks run on Compile.
  """
  @spec diagnostics(String.t(), String.t()) :: {:ok, [map()]} | {:error, term()}
  def diagnostics(code, mode) when is_binary(code) and is_binary(mode) do
    case dispatch_diagnostics(code, mode) do
      {:diagnostics, list} when is_list(list) ->
        {:ok, Enum.map(list, &normalize_diagnostic/1)}

      {:error, reason} ->
        {:error, reason}

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}

      other ->
        {:error, {:unexpected_reply, other}}
    end
  end

  # Project one backend diagnostic term (atom-keyed `%{message, severity, start,
  # end}`, byte offsets) onto the binary-keyed wire shape the client consumes,
  # renaming `start`/`end` to `from`/`to` (CodeMirror's `Diagnostic` field names).
  defp normalize_diagnostic(diag) when is_map(diag) do
    %{
      "from" => Map.get(diag, :start, 0),
      "to" => Map.get(diag, :end, 0),
      "severity" => to_string(Map.get(diag, :severity, "error")),
      "message" => to_string(Map.get(diag, :message, ""))
    }
  end

  # Defensive fallback (symmetry with `normalize_test_entry/1`): a non-map
  # diagnostic from an unexpected compiler-port path degrades to an empty,
  # zero-span diagnostic rather than raising a FunctionClauseError that would
  # bypass the `diagnostics` handler's catch-all and crash the LiveView socket.
  defp normalize_diagnostic(_other) do
    %{"from" => 0, "to" => 0, "severity" => "error", "message" => ""}
  end

  @doc """
  Discover loaded `TestCase` subclasses + their test selectors via the
  `list-tests` op (BT-2557) — the cockpit test-runner pane's catalogue.

  Pure reflection over the live class registry (`beamtalk_test_runner:discover_tests/0`):
  runs **no** test code and mutates nothing, consistent with the facade's
  `:read` capability (the Observer may list tests). The op ignores the session
  (workspace-global discovery), so `self()` is passed only for `dispatch/4`'s
  arity, mirroring the browse ops.

  Returns `{:ok, [%{"class" => binary, "selectors" => [binary]}]}` (sorted by
  class), or `{:error, reason}` on a dispatch failure.
  """
  @spec list_tests() :: {:ok, [map()]} | {:error, term()}
  def list_tests do
    case dispatch_simple("list-tests", %{}) do
      {:value, %{"classes" => classes}} when is_list(classes) ->
        {:ok, classes}

      {:value, other} ->
        {:error, {:unexpected_reply, other}}

      {:error, reason} ->
        {:error, reason}

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}

      other ->
        {:error, {:unexpected_reply, other}}
    end
  end

  @doc """
  Run tests via the `test` / `test-all` ops (BT-2557) — the cockpit test-runner
  pane's execution path.

  `class` is `nil` to run every loaded `TestCase` subclass (`test-all`), or a
  binary class name to run a single class (`test`). Tests compile + **evaluate**
  user code (mutating the image), so the facade gates this op as `:execute`
  (Owner-only) — the same gate the eval form uses. Runs through the attached
  session/facade, never a shelled-out `beamtalk test`.

  Returns `{:ok, result}` where `result` is a normalised, string-keyed map
  `%{"total","passed","failed","skipped","duration","tests" => [entry]}` and each
  entry is `%{"name","class","status","detail"}` (`detail` is the assertion
  message for a failure, the reason for a skip, or `""`). Returns `{:error,
  reason}` on a dispatch failure or a structured test-run error.
  """
  @spec run_tests(String.t() | nil) :: {:ok, map()} | {:error, term()}
  def run_tests(class) when is_binary(class) or is_nil(class) do
    {op, params} =
      case class do
        nil -> {"test-all", %{}}
        name -> {"test", %{"class" => name}}
      end

    case dispatch_simple(op, params) do
      {:test_results, result} when is_map(result) ->
        {:ok, normalize_test_result(result)}

      {:error, reason} ->
        {:error, reason}

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}

      other ->
        {:error, {:unexpected_reply, other}}
    end
  end

  @doc """
  Load the project's `test/` files into the live image via the `load-tests` op
  (BT-2557) — the test-runner pane's "Load tests" affordance.

  Plain project loads default to `include_tests=false`, so a freshly-opened
  image holds only `src/` classes and the runner catalogue is empty. This op
  delegates to the shared `sync_project/2` with `include_tests=true`, compiling
  and loading the `test/` `.bt` files (mutating the image), so the facade gates
  it `:execute` (Owner-only) — the same gate `run_tests` uses.

  Returns `{:ok, %{"classes" => [binary], "errors" => [map], "summary" =>
  binary}}` on success, or `{:error, reason}` on a dispatch failure.
  """
  @spec load_tests() :: {:ok, map()} | {:error, term()}
  def load_tests do
    case dispatch_simple("load-tests", %{}) do
      {:value, %{"classes" => _} = result} ->
        {:ok, result}

      {:value, other} ->
        {:error, {:unexpected_reply, other}}

      {:error, reason} ->
        {:error, reason}

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}

      other ->
        {:error, {:unexpected_reply, other}}
    end
  end

  # Project a backend `TestResult` term (atom-keyed, live `tests` list) onto a
  # string-keyed wire map the LiveView can render directly.
  defp normalize_test_result(result) when is_map(result) do
    %{
      "total" => Map.get(result, :total, 0),
      "passed" => Map.get(result, :passed, 0),
      "failed" => Map.get(result, :failed, 0),
      "skipped" => Map.get(result, :skipped, 0),
      "duration" => Map.get(result, :duration, 0.0),
      "tests" => Enum.map(Map.get(result, :tests, []), &normalize_test_entry/1)
    }
  end

  # One per-case entry: `name`/`status`/`class` arrive as atoms over distribution;
  # `detail` carries the failure message (`:error`) or skip reason (`:reason`).
  defp normalize_test_entry(entry) when is_map(entry) do
    %{
      "name" => to_string(Map.get(entry, :name, "")),
      "class" => to_string(Map.get(entry, :class, "")),
      "status" => to_string(Map.get(entry, :status, "")),
      "detail" => test_entry_detail(entry)
    }
  end

  # Defensive fallback: the test runner always returns well-shaped entry maps
  # today, but a non-map entry from some unexpected failure path must degrade to
  # a rendered "fail" row rather than raising a FunctionClauseError that would
  # crash the LiveView socket with no user-visible diagnosis.
  defp normalize_test_entry(other) do
    %{
      "name" => inspect(other),
      "class" => "",
      "status" => "fail",
      "detail" => "Unexpected entry shape from test runner"
    }
  end

  defp test_entry_detail(entry) do
    cond do
      Map.has_key?(entry, :error) -> stringify_detail(Map.get(entry, :error))
      Map.has_key?(entry, :reason) -> stringify_detail(Map.get(entry, :reason))
      true -> ""
    end
  end

  defp stringify_detail(detail) when is_binary(detail), do: detail
  defp stringify_detail(detail), do: inspect(detail)

  # Decode + dispatch a session-less term op (workspace-global: discovery,
  # test runs). `self()` is passed as `dispatch/4`'s SessionPid only to satisfy
  # its arity — these ops do not read session bindings — mirroring
  # `dispatch_browse/2`. Unlike `dispatch_browse/2`, which normalises everything
  # to `{:value, _} | {:error, _}`, this returns the op-result term VERBATIM (no
  # JSON edge): **callers own the op-specific term-shape match** (`list_tests/0`
  # matches `{:value, _}`, `run_tests/1` matches `{:test_results, _}`). A new
  # caller must match its op's own result tag, not assume browse-style normalising.
  defp dispatch_simple(op, params) when is_binary(op) and is_map(params) do
    request = Map.put(params, "op", op)

    case rpc(:beamtalk_repl_protocol, :decode, [encode_json(request)]) do
      {:ok, msg} ->
        decoded_params = rpc(:beamtalk_repl_protocol, :get_params, [msg])
        rpc(:beamtalk_repl_ops, :dispatch, [op, decoded_params, msg, self()])

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}

      other ->
        {:error, {:unexpected_reply, other}}
    end
  end

  # Build the browse request as a plain map, decode it to a `protocol_msg()` on
  # the workspace node, then dispatch through the term-returning op layer. The
  # browse ops ignore the session pid (they are workspace-global reflection), so
  # `self()` is passed only to satisfy `dispatch/4`'s arity. The `{:value, _}`
  # term is returned verbatim — `encode/2` (JSON) is never invoked, so the
  # wire-shaped rows arrive live over distribution (BT-2399 / ADR 0096).
  defp dispatch_browse(op, params) when is_binary(op) and is_map(params) do
    # The protocol is FLAT: `decode_map/1` reads each op param as a TOP-LEVEL key
    # (`params = maps:without([op, id, session], Map)`), not from a nested
    # `"params"` sub-map. Merge the op's params straight in alongside `"op"`.
    request = Map.put(params, "op", op)

    case rpc(:beamtalk_repl_protocol, :decode, [encode_json(request)]) do
      {:ok, msg} ->
        decoded_params = rpc(:beamtalk_repl_protocol, :get_params, [msg])

        case rpc(:beamtalk_repl_ops, :dispatch, [op, decoded_params, msg, self()]) do
          {:value, value} -> {:value, value}
          {:error, reason} -> {:error, reason}
          {:badrpc, reason} -> {:error, {:unreachable, reason}}
          other -> {:error, {:unexpected_reply, other}}
        end

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}

      other ->
        {:error, {:unexpected_reply, other}}
    end
  end

  # Build the protocol request as a plain map, decode it to a `protocol_msg()`
  # on the workspace node (so we don't depend on the record's `.hrl`), then
  # dispatch through the term-returning op layer. `dispatch/4` returns the
  # `op_result()` term directly — JSON encoding (`encode/2`) is never invoked.
  defp dispatch_eval(session_pid, expression) do
    # Flat protocol: `code` is a top-level key, NOT nested under `"params"` (see
    # `dispatch_browse/2`). A nested map decodes to `params = %{"params" => …}`,
    # so the eval op never finds `code` and reports an empty expression.
    request = %{"op" => "eval", "code" => expression}

    case rpc(:beamtalk_repl_protocol, :decode, [encode_json(request)]) do
      {:ok, msg} ->
        params = rpc(:beamtalk_repl_protocol, :get_params, [msg])
        rpc(:beamtalk_repl_ops, :dispatch, ["eval", params, msg, session_pid])

      other ->
        other
    end
  end

  # Build the `complete` request as a flat map (`code` + `cursor` top-level,
  # mirroring `dispatch_eval`), decode it on the workspace node, and dispatch
  # through the term-returning op layer so the `{completions, _}` term arrives
  # live (no JSON edge). `session_pid` is passed as `dispatch/4`'s SessionPid so
  # the op resolves the session's bindings for instance-method completion
  # (BT-1045); `cursor` (the byte length of the line prefix) makes the op take
  # its context-aware path (BT-783) rather than bare-prefix completion.
  defp dispatch_complete(session_pid, code) do
    request = %{"op" => "complete", "code" => code, "cursor" => byte_size(code)}

    case rpc(:beamtalk_repl_protocol, :decode, [encode_json(request)]) do
      {:ok, msg} ->
        params = rpc(:beamtalk_repl_protocol, :get_params, [msg])
        rpc(:beamtalk_repl_ops, :dispatch, ["complete", params, msg, session_pid])

      other ->
        other
    end
  end

  # Build the `hover` request as a flat map (`code` top-level, mirroring
  # `dispatch_complete`), decode it on the workspace node, and dispatch through
  # the term-returning op layer so the `{docs, _}` term arrives live (no JSON
  # edge). `session_pid` is passed as `dispatch/4`'s SessionPid so the op
  # resolves the session's bindings for receiver classification (BT-1045).
  #
  # Unlike `dispatch_complete`, no `cursor` is sent: completion uses `cursor`'s
  # presence to switch between bare-prefix and context-aware modes (BT-783),
  # whereas hover always resolves the hovered token against its line context, so
  # the distinction does not apply here.
  defp dispatch_hover(session_pid, code) do
    request = %{"op" => "hover", "code" => code}

    case rpc(:beamtalk_repl_protocol, :decode, [encode_json(request)]) do
      {:ok, msg} ->
        params = rpc(:beamtalk_repl_protocol, :get_params, [msg])
        rpc(:beamtalk_repl_ops, :dispatch, ["hover", params, msg, session_pid])

      other ->
        other
    end
  end

  # Build the `diagnostics` request as a flat map (`code` + `mode` top-level),
  # decode it on the workspace node, and dispatch through the term-returning op
  # layer so the `{diagnostics, _}` term arrives live (no JSON edge). No session
  # pid is needed — diagnostics are workspace-global parse-only analysis — so
  # `self()` is passed only to satisfy `dispatch/4`'s arity, mirroring
  # `dispatch_browse/2`. `mode` (BT-2569) selects the parse grammar.
  defp dispatch_diagnostics(code, mode) do
    request = %{"op" => "diagnostics", "code" => code, "mode" => mode}

    case rpc(:beamtalk_repl_protocol, :decode, [encode_json(request)]) do
      {:ok, msg} ->
        params = rpc(:beamtalk_repl_protocol, :get_params, [msg])
        rpc(:beamtalk_repl_ops, :dispatch, ["diagnostics", params, msg, self()])

      other ->
        other
    end
  end

  @doc """
  Render a result *term* as a string for the page, surface-consistent with the
  Phase-1 browser workspace.

  Display is a separate concern from the data path: `eval/2` returns the live
  term, and this stringifies it via the workspace's own
  `beamtalk_repl_json:term_to_json/1` — the exact formatter the browser
  WebSocket protocol uses — so values render byte-identically across surfaces. A
  spawned actor renders as the browser's `#Actor<…>` display while the live,
  messageable remote pid remains in the term `eval/2` returned.

  Falls back to a local `inspect/1` only if the workspace formatter is
  unreachable, so the pane degrades gracefully rather than crashing.
  """
  def render_term(value) do
    case rpc(:beamtalk_repl_json, :term_to_json, [value]) do
      {:badrpc, _reason} -> inspect(value)
      json -> format_value(json)
    end
  end

  @doc """
  Render a `term_to_json/1` value (the same JSON-ready value the browser
  serialises) to a display string using the canonical, surface-shared rules from
  `beamtalk_repl_protocol::format::format_value` (Rust, BT-2086 — Plain mode), so
  the LiveView pane matches the CLI / MCP / browser byte-for-byte:

    * scalars render verbatim (strings already carry `#Actor<…>`, float text, …)
    * lists render as Beamtalk list literals `#(a, b, c)`
    * maps render as `{k: v, …}`

  Pure (no RPC), so it is unit-tested directly without a live workspace.
  """
  def format_value(json) when is_binary(json), do: json
  def format_value(json) when is_integer(json), do: Integer.to_string(json)
  def format_value(true), do: "true"
  def format_value(false), do: "false"
  def format_value(nil), do: "nil"
  def format_value(json) when is_float(json), do: Float.to_string(json)

  def format_value(json) when is_list(json) do
    "#(" <> Enum.map_join(json, ", ", &format_value/1) <> ")"
  end

  def format_value(json) when is_map(json) do
    "{" <> Enum.map_join(json, ", ", fn {k, v} -> "#{k}: #{format_value(v)}" end) <> "}"
  end

  def format_value(json), do: inspect(json)

  @doc """
  Render an error *term* (`#beamtalk_error{}`) as a user-facing message,
  surface-consistent with the browser. Reuses
  `beamtalk_repl_json:format_error_message/1` so the message text matches what
  the browser shows; falls back to `inspect/1` for non-structured reasons.
  """
  def render_error(reason) do
    case rpc(:beamtalk_repl_json, :format_error_message, [reason]) do
      msg when is_binary(msg) -> msg
      _ -> inspect(reason)
    end
  end

  @doc """
  Subscribe the given `pid` (the LiveView process) to the workspace's Transcript
  push stream through the BT-2399 subscription facade.

  Over Erlang distribution the LiveView pid is location-transparent, so the
  facade's explicit-pid form registers it as the subscriber on the workspace
  node. The LiveView then receives `{:transcript_output, text}` messages
  directly — no polling, no `{subscribe, self()}` cast at the gen_server, no
  extra protocol.
  """
  def subscribe_transcript(pid) when is_pid(pid) do
    rpc(:beamtalk_repl_subscriptions, :subscribe, [:transcript, pid])
  end

  @doc "Unsubscribe `pid` from the Transcript push stream via the facade."
  def unsubscribe_transcript(pid) when is_pid(pid) do
    rpc(:beamtalk_repl_subscriptions, :unsubscribe, [:transcript, pid])
  end

  @doc """
  Subscribe the given `pid` (the LiveView process) to the workspace's
  bindings-changed push stream through the BT-2399 subscription facade.

  Like the Transcript stream, this uses the facade's explicit-pid form so the
  LiveView's own location-transparent pid is registered as the subscriber (not
  the short-lived RPC proxy). The bindings stream is a *signal* stream and, since
  BT-2531, rides the SystemAnnouncer bus: it pushes the native
  `{:beamtalk_announcement, sub_ref, :BindingChanged, handler, event}` message
  after a workspace variable is assigned — a refresh trigger, not the data itself.
  On that signal the LiveView re-reads the current bindings via `list_bindings/1`,
  so the pane stays live without polling. The bus prunes the subscription on dist
  disconnect; the LiveView re-subscribes on its next (re)mount.
  """
  def subscribe_bindings(pid) when is_pid(pid) do
    rpc(:beamtalk_repl_subscriptions, :subscribe, [:bindings, pid])
  end

  @doc "Unsubscribe `pid` from the bindings-changed push stream via the facade."
  def unsubscribe_bindings(pid) when is_pid(pid) do
    rpc(:beamtalk_repl_subscriptions, :unsubscribe, [:bindings, pid])
  end

  @doc """
  Subscribe the given `pid` (the LiveView process) to the workspace's
  class-lifecycle push stream through the BT-2399 subscription facade (BT-2598).

  Like the Transcript/bindings streams, this uses the facade's explicit-pid form
  so the LiveView's own location-transparent pid is registered as the subscriber
  (not the short-lived RPC proxy). The `classes` stream rides the SystemAnnouncer
  bus and pushes the native
  `{:beamtalk_announcement, sub_ref, :ClassLoaded | :ClassRemoved, handler, event}`
  message whenever a class is (re)loaded or torn down — a *refresh trigger*, not
  the data itself. A hot redefinition (an in-memory `>>` patch, an MCP
  `save_method`, or the disk→image reload after a git revert) is itself a
  `ClassLoaded`, so any source change reaches the subscriber. On that signal the
  cockpit refreshes its open editor/browser windows and the browser class list,
  rather than each action manually re-pulling — this also covers multi-session
  and external-edit freshness. The bus prunes the subscription on dist
  disconnect; the LiveView re-subscribes on its next (re)mount.
  """
  def subscribe_classes(pid) when is_pid(pid) do
    rpc(:beamtalk_repl_subscriptions, :subscribe, [:classes, pid])
  end

  @doc "Unsubscribe `pid` from the class-lifecycle push stream via the facade."
  def unsubscribe_classes(pid) when is_pid(pid) do
    rpc(:beamtalk_repl_subscriptions, :unsubscribe, [:classes, pid])
  end

  @doc """
  Subscribe the given `subscriber` (the LiveView process) to *per-object* state
  changes on the inspected actor `term` — the live-Inspector push of Cockpit
  Phase 3 (ADR 0095 §5, BT-2489).

  Unlike the system-wide Transcript/bindings streams, this watches a single
  actor: after each committed state write the subscriber receives
  `{:object_changed, pid, changed_slots}` — a *refresh trigger* (mirroring the
  bindings stream), on which the LiveView re-issues `inspect_value/1` and
  `pid_stats/1` to read the fresh snapshot, keeping actor opacity intact.

  Opt-in by subscription: the runtime only *publishes* while at least one watcher
  is registered for the pid (see `beamtalk_object_watch`), so an unwatched actor's
  dispatch pays only a single cheap membership read. The watched actor pid is
  extracted from the object handle; only pid-backed objects are watchable (the
  `inspectable?/1` contract). Returns `{:error, :not_inspectable}` for any other
  term.

  A live consumer must re-issue `subscribe_object_changes/2` on dist reconnect
  (the watch server is supervised; a crash resets its subscription table and
  remote subscribers are not re-armed automatically — see `beamtalk_object_watch`).
  """
  def subscribe_object_changes({:beamtalk_object, _class, _module, pid} = _term, subscriber)
      when is_pid(pid) and is_pid(subscriber) do
    rpc(:beamtalk_repl_subscriptions, :subscribe_object, [pid, subscriber])
  end

  def subscribe_object_changes(_term, subscriber) when is_pid(subscriber),
    do: {:error, :not_inspectable}

  @doc "Unsubscribe `subscriber` from per-object state changes on the actor `term`."
  def unsubscribe_object_changes({:beamtalk_object, _class, _module, pid} = _term, subscriber)
      when is_pid(pid) and is_pid(subscriber) do
    rpc(:beamtalk_repl_subscriptions, :unsubscribe_object, [pid, subscriber])
  end

  def unsubscribe_object_changes(_term, subscriber) when is_pid(subscriber),
    do: {:error, :not_inspectable}

  @doc """
  Read live process metrics for the inspected actor `term` via the `pid-stats`
  read op (ADR 0095 §5, BT-2489) — message queue depth, memory, reductions,
  scheduling status, and current function. The companion read to the per-object
  change stream: the Inspector pane re-issues it on each `{:object_changed, ...}`
  push (or a refresh timer) to keep the process-health line live.

  Like `inspect_value/1`, the pid is formatted **on the workspace node** (where it
  is local) so the textual `<0.X.Y>` form round-trips through the op's
  `list_to_pid/1`. Returns `{:ok, stats_map}` (binary-keyed metrics) or
  `{:error, reason}`; `{:error, :not_inspectable}` for a non-pid-backed term.
  """
  def pid_stats({:beamtalk_object, _class, _module, pid} = _term) when is_pid(pid) do
    case rpc(:erlang, :pid_to_list, [pid]) do
      pid_chars when is_list(pid_chars) ->
        dispatch_pid_stats_result(dispatch_pid_stats(to_string(pid_chars)))

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}
    end
  end

  def pid_stats(_term), do: {:error, :not_inspectable}

  @doc """
  List the current workspace bindings for `session_pid` as `{name, term}` pairs.

  Reads the session's live binding map through the read-surface
  (`beamtalk_repl_shell:get_bindings/1`, ADR 0085): the values are **live Erlang
  terms** — a bound `Counter spawn` is a real `{:beamtalk_object, class, module,
  pid}` handle, not a flattened display string — so the Inspector can follow them
  by reference. Display rendering is a separate concern handled by the caller via
  `render_term/1`.

  Returns a name-sorted list of `{name :: String.t(), term}` tuples, or
  `{:error, reason}` if the workspace is unreachable or the shell returns an
  unexpected shape (the call site renders the error rather than crashing).
  """
  def list_bindings(session_pid) when is_pid(session_pid) do
    case rpc(:beamtalk_repl_shell, :get_bindings, [session_pid]) do
      {:ok, bindings} when is_map(bindings) ->
        bindings
        |> Enum.map(fn {name, term} -> {to_string(name), term} end)
        |> Enum.sort_by(fn {name, _term} -> name end)

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}

      other ->
        {:error, {:unexpected_reply, other}}
    end
  end

  @doc """
  True when `term` is a live, messageable Beamtalk object handle — the
  reference-following case. Over distribution the `#beamtalk_object{}` record
  arrives as a `{:beamtalk_object, class, module, pid_or_ref}` tuple (see
  `beamtalk.hrl`). Only object terms backed by a real remote pid are
  inspectable: name-resolving proxies carry `{:registered, name}` in the identity
  slot (ADR 0079) and have no pid to `sys:get_state/2`, so they are not
  drillable here.

  Supervisor handles arrive as `{:beamtalk_supervisor, class, module, pid}`
  (`beamtalk_supervisor.erl`, distinct from `#beamtalk_object{}`). A pid-backed
  supervisor is a live, drillable handle too, so it is inspectable — even though
  its inspection *content* (the supervision tree) is not yet implemented and
  currently degrades to an empty field set (see `inspect_value/1`).
  """
  def inspectable?({:beamtalk_object, _class, _module, pid}) when is_pid(pid), do: true
  def inspectable?({:beamtalk_supervisor, _class, _module, pid}) when is_pid(pid), do: true
  def inspectable?(_term), do: false

  @doc """
  Inspect a live object `term` through the read-surface `inspect` op (ADR 0085),
  returning its structured instance fields as a live-term map.

  This is the reference-following primitive: given a `{:beamtalk_object, class,
  module, pid}` handle held in the LiveView, it extracts the real remote pid and
  drives `beamtalk_repl_ops:dispatch("inspect", ...)`, which reads the actor's
  live state and returns only its user-visible fields (`{:inspect, fields_map}`).
  Because the fields are themselves live terms, an object-valued slot is again a
  `{:beamtalk_object, ...}` tuple the caller can drill into recursively — the
  whole point of carrying terms, not JSON, to the LiveView.

    * success → `{:ok, fields :: map()}` (or `{:ok, binary}` for a non-tagged
      raw state value the workspace stringified)
    * error   → `{:error, reason_term}` (`#beamtalk_error{}` or a structured
      reason); the caller renders it via `render_error/1`

  Returns `{:error, :not_inspectable}` for any term that is not a pid-backed
  object handle, so the caller never has to guess the term shape.
  """
  def inspect_value({:beamtalk_object, _class, _module, pid} = _term) when is_pid(pid) do
    # The `inspect` op identifies the actor by the *textual* pid form, the same
    # contract the browser uses (`beamtalk_ws_handler` formats with pid_to_list,
    # the op resolves with list_to_pid). Both sides must run on the SAME node:
    # `pid_to_list/1` of a node-LOCAL pid yields the canonical `<0.X.Y>` text
    # that `list_to_pid/1` round-trips on that node. Stringifying a *remote*
    # workspace pid here on the Phoenix node would instead emit `<N.X.Y>` (N =
    # the workspace's index in *our* dist table), and the workspace's
    # `list_to_pid/1` would then `badarg` (or worse, resolve a different pid).
    # So format the pid ON the workspace node, where it is local, via RPC.
    case rpc(:erlang, :pid_to_list, [pid]) do
      pid_chars when is_list(pid_chars) ->
        dispatch_inspect_result(dispatch_inspect(to_string(pid_chars)))

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}
    end
  end

  # A supervisor's meaningful inspection content is its CHILDREN / supervision
  # tree, not gen_server instance vars — a supervisor pid is NOT an actor backed
  # by `sys:get_state/2`, so routing it through the actor `inspect` op above would
  # error (BT-2634). Route it to the supervision-tree path instead: list its direct
  # children as drillable rows (each carrying a live `{:beamtalk_supervisor, ...}` /
  # `{:beamtalk_object, ...}` handle so the Inspector can drill into a child as its
  # own reference, ADR 0095). Returns `{:ok, {:supervisor_children, rows}}` — a
  # tagged result the LiveView renders as a children view, distinct from the
  # actor instance-field table. A dead/unreachable supervisor degrades to
  # `{:error, reason}` (rendered as a clear empty state, never a crash).
  def inspect_value({:beamtalk_supervisor, _class, _module, pid} = _term) when is_pid(pid) do
    case supervisor_children(pid) do
      {:ok, rows} -> {:ok, {:supervisor_children, rows}}
      {:error, reason} -> {:error, reason}
    end
  end

  def inspect_value(_term), do: {:error, :not_inspectable}

  @doc """
  List a supervisor's direct children as drillable inspection rows (BT-2634, ADR
  0095 supervisor-aware inspection).

  `sup_pid` is the live remote supervisor pid (extracted from a
  `{:beamtalk_supervisor, class, module, pid}` handle held in the LiveView).
  Like `inspect_value/1`, the pid is formatted **on the workspace node** (where it
  is local) so the textual `<0.X.Y>` form round-trips through the runtime helper's
  `list_to_pid/1`, then `beamtalk_process_navigation:child_handles/1` classifies
  each direct child and mints a **live child handle** for it — the reference the
  Inspector drills into. Static and `DynamicSupervisor` children come back through
  the same `which_children` path, so they render uniformly.

  Each row is the runtime's binary-keyed child map (`label`, `className`, `kind`,
  `pid`, `registeredName`, `childCount`, `isSupervisor`, `handle`). The `handle`
  is a live `{:beamtalk_supervisor | :beamtalk_object, class, module, pid}` term
  for a Beamtalk child, or `:null` for a foreign / restarting child (which renders
  but is not drillable).

  Returns `{:ok, rows}` (the empty list for a supervisor with no running
  children), or `{:error, reason_term}` for a dead/unreachable supervisor (a
  structured `#beamtalk_error{}`), so the caller renders a clear empty state.
  """
  @spec supervisor_children(pid()) :: {:ok, [map()]} | {:error, term()}
  def supervisor_children(sup_pid) when is_pid(sup_pid) do
    case rpc(:erlang, :pid_to_list, [sup_pid]) do
      pid_chars when is_list(pid_chars) ->
        case rpc(:beamtalk_process_navigation, :child_handles, [to_string(pid_chars)]) do
          {:ok, rows} when is_list(rows) -> {:ok, rows}
          {:error, reason} -> {:error, reason}
          {:badrpc, reason} -> {:error, {:unreachable, reason}}
          other -> {:error, {:unexpected_reply, other}}
        end

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}
    end
  end

  # ── write-surface: method-level edit / save / flush (ADR 0082, BT-2409) ─────

  @doc """
  Save (durably patch) a single method on a class in the attached workspace,
  through the write-surface (ADR 0082, Wave 3).

  This is the method editor's "Save" action. Per ADR 0082 the operation is the
  Beamtalk-level `aClass compile: #selector source: body` primitive — but rather
  than building a `compile:source:` *eval string* (which would force escaping the
  body's quotes, backslashes, and `{` interpolation markers back into Beamtalk
  source — exactly the fragility ADR 0082 calls out), we call the runtime install
  chokepoint `beamtalk_repl_eval:compile_method/6` directly over distribution,
  passing the body as a **String value**. `>>`, `compile:source:`, and this entry
  all converge at the same in-memory install + ChangeLog append, so the saved
  method is compiled, flushed into the live BEAM module, and recorded in the
  workspace's change history (the ChangeLog coherence criterion) in one step.

  `selector` is the method selector (e.g. `"increment"` or `"at:put:"`), `source`
  the method definition body. Author metadata is stamped as `liveview` / `human`
  for the audit trail.

  Returns:

    * success → `{:ok, class}` where `class` is the class-name binary the install
      confirmed
    * error   → `{:error, reason_term}` where `reason_term` is the structured
      `#beamtalk_error{}` for a failed compile/save (rendered via
      `render_error/1`), or `{:unreachable, _}` if the workspace is gone

  The term contract is the only coupling: no JSON crosses the Attach path.
  """
  def save_method(class, selector, source)
      when is_binary(class) and is_binary(selector) and is_binary(source) do
    # compile_method/6: (ClassNameBin, Selector, SourceBin, Intent, Author, AuthorKind).
    # Intent `durable` so the patch is logged as a keepable change (ADR 0082); the
    # selector and body are passed as values — no source-string round-trip.
    args = [class, selector, source, :durable, "liveview", :human]

    case rpc(:beamtalk_repl_eval, :compile_method, args) do
      {:ok, class_bin} ->
        {:ok, to_string(class_bin)}

      # A compile/install failure returns {error, Reason}; structure it so the
      # LiveView renders an actionable #beamtalk_error{} rather than a flattened
      # internal term.
      {:error, reason} ->
        {:error, structure_error(reason)}

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}

      other ->
        {:error, {:unexpected_reply, other}}
    end
  end

  @doc """
  Create a brand-new class from a source String at a target path (ADR 0082
  Phase 5 `Workspace newClass:at:`, BT-2293). This is the System Browser's
  "New Class" action — the in-memory class install + a durable
  `kind: "new-class"` ChangeLog entry; the `.bt` file itself is written later on
  `flush/0`.

  Like `save_method/3`, this calls the runtime install chokepoint directly —
  `beamtalk_repl_eval:new_class/2`, which returns the same clean term contract
  (`{ok, ClassObjects} | {error, Reason}`) rather than the raising FFI surface
  (`beamtalk_workspace_interface_primitives:newClass/2`). `source` is the class
  definition (e.g. `"Object subclass: Greeter"`), `path` the in-project target
  (e.g. `"src/Greeter.bt"`, as the LiveView derives from the class name).

  Returns:

    * success → `{:ok, path}` — the path the new class is bound to (the created
      class objects are not surfaced; the path is the actionable identity)
    * error   → `{:error, reason_term}` — a structured `#beamtalk_error{}` for a
      bad path / name clash / compile failure, or `{:unreachable, _}` if the
      workspace is gone
  """
  def new_class(source, path) when is_binary(source) and is_binary(path) do
    case rpc(:beamtalk_repl_eval, :new_class, [source, path]) do
      {:ok, _class_objects} ->
        {:ok, path}

      {:error, reason} ->
        {:error, structure_error(reason)}

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}

      other ->
        {:error, {:unexpected_reply, other}}
    end
  end

  @doc """
  Revert one pending in-memory method patch by its `(class, selector)` (ADR 0082
  Phase 5 `Workspace changes revert:`, BT-2293). This is the ChangeLog viewer's
  per-entry "revert" action: it re-installs the recorded prior body of the most
  recent active entry for that target, itself emitting a fresh durable
  ChangeEntry (the original entry stays in the audit log).

  Calls `beamtalk_workspace_interface_primitives:revert_method/2`, the
  clean-returning wrapper over the FFI `changeLogRevert/1` (which would `error`-
  raise across the dist boundary). `class` and `selector` are the binaries
  carried by a `change_history/0` row.

  Returns `{:ok, class}` on a successful revert, or `{:error, reason_term}` for
  a non-revertable entry (new-class creation, class-side patch, no recorded
  prior body, or no active entry) / an unreachable workspace.
  """
  def revert(class, selector) when is_binary(class) and is_binary(selector) do
    case rpc(:beamtalk_workspace_interface_primitives, :revert_method, [class, selector]) do
      {:ok, _class_object} ->
        {:ok, class}

      {:error, reason} ->
        {:error, structure_error(reason)}

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}

      other ->
        {:error, {:unexpected_reply, other}}
    end
  end

  @doc """
  Reload a `.bt` file from disk into the live image (BT-2598) — the disk→image
  reload the cockpit runs after a content-mutating git op (a `git revert` /
  `git restore -- <path>`), so the live image matches the reverted working tree
  (image == disk, git-first / disk-as-source-of-truth, BT-2585).

  Calls `beamtalk_repl_eval:reload_file/1`, the clean-returning reload entry: it
  re-installs the file's classes via hot redefinition (which fires a `ClassLoaded`
  announcement on the `classes` stream, so subscribed surfaces refresh their open
  windows) and repopulates the workspace_meta class-source cache. `path` is the
  project-relative `.bt` path git just restored.

  Returns `{:ok, [class_name]}` (the reloaded class-name binaries) or
  `{:error, reason_term}` (a structured `#beamtalk_error{}` for a compile failure
  / missing file, or `{:unreachable, _}` if the workspace is gone).
  """
  def reload_file(path) when is_binary(path) do
    case rpc(:beamtalk_repl_eval, :reload_file, [binary_to_list_path(path)]) do
      {:ok, class_names} when is_list(class_names) ->
        {:ok, Enum.map(class_names, &to_string/1)}

      {:error, reason} ->
        {:error, structure_error(reason)}

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}

      other ->
        {:error, {:unexpected_reply, other}}
    end
  end

  # `reload_file/1` takes an Erlang string (charlist) path — the runtime opens it
  # with `file:read_file/1` and resolves it against the workspace node's cwd (the
  # project root), the same form git restored.
  defp binary_to_list_path(path) when is_binary(path), do: String.to_charlist(path)

  @doc """
  Flush the workspace's pending durable changes to disk (ADR 0082 `Workspace
  flush`). This is the "Save All to Disk" action — it replays the ChangeLog
  entries written by `save_method/3` into their `.bt` source files.

  Calls `beamtalk_workspace_flush:flush/0` directly, which returns a term
  contract (never raises on a normal conflict): a `FlushResult`-shaped summary
  map on success, or `{error, #beamtalk_error{}}` on a hard runtime error. The
  summary's `conflicts` / `skipped` lists carry recoverable conditions
  (external-edit conflicts, non-flushable stdlib patches) for the caller to
  render.

  Returns `{:ok, summary_map}` or `{:error, reason_term}`.
  """
  def flush do
    case rpc(:beamtalk_workspace_flush, :flush, []) do
      {:ok, summary} when is_map(summary) -> {:ok, summary}
      {:error, reason} -> {:error, structure_error(reason)}
      {:badrpc, reason} -> {:error, {:unreachable, reason}}
      other -> {:error, {:unexpected_reply, other}}
    end
  end

  @doc """
  Read the workspace's `autoflush` setting (ADR 0082 Phase 4, BT-2290) — whether a
  per-method save writes through to disk immediately (`true`) or only patches the
  live image until an explicit `flush/0` (`false`, the default).

  Reads `beamtalk_workspace_meta:get_setting(autoflush, false)` in a single RPC.
  Used by the cockpit (BT-2590) to skip the post-save git refresh when autoflush
  is off: an in-memory-only save leaves the on-disk working tree untouched, so the
  git shell-out would return an identical result. A workspace that is unreachable
  or returns a non-boolean defaults to `false` (the safe, no-extra-shell-out
  choice) rather than crashing the caller.
  """
  @spec autoflush() :: boolean()
  def autoflush do
    case rpc(:beamtalk_workspace_meta, :get_setting, [:autoflush, false]) do
      flag when is_boolean(flag) -> flag
      _other -> false
    end
  end

  @doc """
  List the workspace's active change history (ADR 0082 `Workspace changes`) — the
  per-method ChangeLog entries that are dirty (installed in memory, not yet
  flushed to disk). This is the ChangeLog-coherence view: after `save_method/3`,
  the saved `(class, selector)` should appear here.

  Reads `beamtalk_workspace_changelog:change_entries/0` — the same
  `$beamtalk_class`-tagged value maps that back the `ChangeLog.bt` /
  `ChangeEntry.bt` value objects — in a **single RPC**. Each entry is already a
  plain atom-keyed map (not an opaque `#entry{}` record), so nothing fragile
  crosses the node boundary and there is no per-entry round-trip. We keep only the
  entries flagged `active` (current epoch, not orphaned, not yet flushed — the
  exact `Workspace changes` predicate) and render display rows newest-first.

  Returns a list of row maps, or `{:error, reason}` if the workspace is
  unreachable or returns an unexpected shape.
  """
  def change_history do
    case rpc(:beamtalk_workspace_changelog, :change_entries, []) do
      entries when is_list(entries) ->
        pending_rows(entries)

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}}

      other ->
        {:error, {:unexpected_reply, other}}
    end
  end

  @doc """
  Working-tree git status for the workspace project root (ADR 0082, BT-2586).

  The cockpit's post-flush, human-facing VCS surface. Delegates to the
  workspace-node `beamtalk_git` module (a shell-out to system `git` parsed into
  typed Erlang maps), so it runs where the `.bt` working tree lives. Returns
  `{:ok, status_map}` where `status_map` has `:branch`, `:upstream`, `:ahead`,
  `:behind` and `:files` (each `%{path:, index:, worktree:}`), or `{:error,
  reason}` when the workspace is unreachable, the project is not a git repo, or
  `git` is absent.
  """
  def git_status do
    case rpc(:beamtalk_git, :git_status, []) do
      {:ok, status} when is_map(status) -> {:ok, status}
      {:error, reason} -> {:error, structure_error(reason)}
      {:badrpc, reason} -> {:error, {:unreachable, reason}}
      other -> {:error, {:unexpected_reply, other}}
    end
  end

  @doc """
  Unified diff for a single path: working-tree-vs-HEAD and staged-vs-HEAD.

  Returns `{:ok, %{worktree: binary, staged: binary}}` (diff text passed through
  verbatim), or `{:error, reason}`.
  """
  def git_diff(path) when is_binary(path) do
    case rpc(:beamtalk_git, :git_diff, [path]) do
      {:ok, diff} when is_map(diff) -> {:ok, diff}
      {:error, reason} -> {:error, structure_error(reason)}
      {:badrpc, reason} -> {:error, {:unreachable, reason}}
      other -> {:error, {:unexpected_reply, other}}
    end
  end

  @doc """
  The most recent `count` commits as `{:ok, [%{sha:, short_sha:, subject:,
  author:, ts:}]}`, or `{:error, reason}`.
  """
  def git_log(count) when is_integer(count) and count > 0 do
    case rpc(:beamtalk_git, :git_log, [count]) do
      {:ok, commits} when is_list(commits) -> {:ok, commits}
      {:error, reason} -> {:error, structure_error(reason)}
      {:badrpc, reason} -> {:error, {:unreachable, reason}}
      other -> {:error, {:unexpected_reply, other}}
    end
  end

  @doc "Stage a single path (`git add`). Returns `{:ok, nil}` or `{:error, reason}`."
  def git_stage(path) when is_binary(path), do: git_mutate(:git_stage, [path])

  @doc "Unstage a single path (`git restore --staged`). Returns `{:ok, nil}` or `{:error, reason}`."
  def git_unstage(path) when is_binary(path), do: git_mutate(:git_unstage, [path])

  @doc """
  Commit the staged index with `message` (`git commit -m`). Invokes system `git`
  so hooks/signing/config apply. Returns `{:ok, nil}` or `{:error, reason}`.
  """
  def git_commit(message) when is_binary(message), do: git_mutate(:git_commit, [message])

  @doc """
  Discard a working-tree change for a single path (`git checkout --`) — the
  human counterpart to the agent ChangeLog `revert:`. Returns `{:ok, nil}` or
  `{:error, reason}`.
  """
  def git_revert_file(path) when is_binary(path), do: git_mutate(:git_revert_file, [path])

  # Shared transport for the mutating git ops, all of which return {ok, nil}.
  defp git_mutate(fun, args) do
    case rpc(:beamtalk_git, fun, args) do
      {:ok, _} -> {:ok, nil}
      {:error, reason} -> {:error, structure_error(reason)}
      {:badrpc, reason} -> {:error, {:unreachable, reason}}
      other -> {:error, {:unexpected_reply, other}}
    end
  end

  @doc false
  # Pure: reduce the raw change-entry value maps to the pending view and render
  # display rows newest-first. The pending view is active AND not shadowed, so
  # repeated patches/reverts of one method collapse to the latest entry — one
  # row per dirty method, matching `ChangeLog activeEntries` on every other
  # surface (BT-2574). Unit-tested directly (no RPC).
  def pending_rows(entries) when is_list(entries) do
    entries
    |> Enum.filter(&pending_entry?/1)
    |> Enum.map(&entry_to_row/1)
    |> Enum.reverse()
  end

  # A change entry belongs in the pending view when it is active (current epoch,
  # not orphaned, not flushed), NOT shadowed by a newer entry for the same
  # `(class, selector)`, and NOT clean (still differs from disk). `shadowed`
  # collapses repeated patches/reverts of one method to its latest entry; `clean`
  # drops a method reverted back to its on-disk body ("disappear when clean",
  # BT-2575). Both mirror `ChangeLog activeEntries`, so the pane stays consistent
  # with `Workspace changes` on every other surface. Older workspace builds may
  # not emit these flags; absence defaults to not-shadowed / not-clean.
  defp pending_entry?(%{active: true} = entry),
    do: Map.get(entry, :shadowed, false) != true and Map.get(entry, :clean, false) != true

  defp pending_entry?(_), do: false

  @doc """
  Render a `FlushResult` summary map (ADR 0082 `Workspace flush`) to a one-line
  status string for the page. Pure (no RPC), so it is unit-tested directly.

  Over distribution the workspace returns an atom-keyed map; keys are read
  defensively (atom or string) so an unexpected shape degrades to `inspect/1`
  rather than crashing the pane. Conflicts and skipped (non-flushable) counts are
  appended when present, since they are the recoverable conditions the user acts
  on.
  """
  def format_flush_summary(summary) when is_map(summary) do
    flushed = summary_field(summary, :flushed, 0)
    files = summary |> summary_field(:files, []) |> List.wrap()
    conflicts = summary |> summary_field(:conflicts, []) |> List.wrap()
    skipped = summary |> summary_field(:skipped, []) |> List.wrap()

    base = "Flushed #{flushed} change(s) across #{length(files)} file(s)"

    parts =
      [base]
      |> maybe_append(conflicts != [], "#{length(conflicts)} conflict(s)")
      |> maybe_append(skipped != [], "#{length(skipped)} skipped")

    Enum.join(parts, " · ")
  end

  def format_flush_summary(other), do: inspect(other)

  defp summary_field(map, key, default) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp maybe_append(parts, false, _text), do: parts
  defp maybe_append(parts, true, text), do: parts ++ [text]

  # Render one ChangeEntry value map (atom-keyed, from `change_entries/0`) to a
  # display row. `className` arrives as an atom and `selector` as a Symbol (atom)
  # or `nil` for a new-class entry; `to_string/1` renders atoms verbatim. We read
  # every key defensively with a default so a future field addition can't crash
  # the pane.
  defp entry_to_row(entry) when is_map(entry) do
    %{
      class: to_string(Map.get(entry, :className, "")),
      selector: present_selector(Map.get(entry, :selector)),
      kind: to_string(Map.get(entry, :kind, "")),
      intent: to_string(Map.get(entry, :intent, "")),
      flushable: Map.get(entry, :flushable, false) == true,
      flushed: Map.get(entry, :flushed, false) == true,
      author_kind: to_string(Map.get(entry, :authorKind, "")),
      diff: present_diff(Map.get(entry, :diff))
    }
  end

  # ChangeLog selector is nil for a new-class entry; show a placeholder.
  defp present_selector(nil), do: "(class)"
  defp present_selector(value), do: to_string(value)

  # The net on-disk → in-memory unified diff (BT-2575), or nil when the runtime
  # computed none (clean, or not computable). `nil` over distribution arrives as
  # the atom `nil`; anything non-binary degrades to nil so the pane never renders
  # a junk value.
  defp present_diff(diff) when is_binary(diff) and diff != "", do: diff
  defp present_diff(_), do: nil

  # Turn a runtime `{error, Reason}` into a structured `#beamtalk_error{}` term so
  # the LiveView renders an actionable message. `ensure_structured_error/1` is the
  # workspace's own wrapper — already structured reasons pass through unchanged,
  # raw reasons get wrapped — so we never flatten an error to an opaque string.
  defp structure_error(reason) do
    case rpc(:beamtalk_repl_errors, :ensure_structured_error, [reason]) do
      {:badrpc, _} -> reason
      structured -> structured
    end
  end

  defp dispatch_inspect_result(result) do
    case result do
      {:inspect, fields} when is_map(fields) -> {:ok, fields}
      {:inspect, other} -> {:ok, other}
      {:error, reason} -> {:error, reason}
      {:badrpc, reason} -> {:error, {:unreachable, reason}}
      other -> {:error, {:unexpected_reply, other}}
    end
  end

  # Build the `inspect` request as a plain map, decode it on the workspace node
  # (so we don't depend on the protocol record's `.hrl`), then dispatch through
  # the term-returning op layer. `dispatch/4` returns the `{:inspect, _}` term
  # directly — JSON encoding (`encode/2`) is never invoked, so the fields stay
  # live terms with their messageable pids intact.
  defp dispatch_inspect(pid_str) do
    # Flat protocol: `actor` is a top-level key, NOT nested under `"params"`
    # (see `dispatch_browse/2`).
    request = %{"op" => "inspect", "actor" => pid_str}

    case rpc(:beamtalk_repl_protocol, :decode, [encode_json(request)]) do
      {:ok, msg} ->
        params = rpc(:beamtalk_repl_protocol, :get_params, [msg])
        # inspect ignores SessionPid; pass self() purely to satisfy dispatch/4.
        rpc(:beamtalk_repl_ops, :dispatch, ["inspect", params, msg, self()])

      other ->
        other
    end
  end

  defp dispatch_pid_stats_result(result) do
    case result do
      # The pid-stats op returns `{:value, stats_map}` (already wire-shaped,
      # binary-keyed) — the dist path returns the term directly, no JSON encode.
      {:value, stats} when is_map(stats) -> {:ok, stats}
      {:error, reason} -> {:error, reason}
      {:badrpc, reason} -> {:error, {:unreachable, reason}}
      other -> {:error, {:unexpected_reply, other}}
    end
  end

  # Build the `pid-stats` request as a flat map (`actor` top-level, mirroring
  # `dispatch_inspect/1`), decode it on the workspace node, and dispatch through
  # the term-returning op layer so the metrics map stays a live term.
  defp dispatch_pid_stats(pid_str) do
    request = %{"op" => "pid-stats", "actor" => pid_str}

    case rpc(:beamtalk_repl_protocol, :decode, [encode_json(request)]) do
      {:ok, msg} ->
        params = rpc(:beamtalk_repl_protocol, :get_params, [msg])
        # pid-stats ignores SessionPid; pass self() to satisfy dispatch/4.
        rpc(:beamtalk_repl_ops, :dispatch, ["pid-stats", params, msg, self()])

      other ->
        other
    end
  end

  # ── internals ─────────────────────────────────────────────────────────────

  defp ensure_distributed do
    unless Node.alive?() do
      # A unique short name so multiple dev runs don't collide on epmd.
      name = :"bt_attach_#{System.unique_integer([:positive])}@localhost"

      # Two connected mounts can both pass Node.alive?/0 and race here; the
      # loser sees {:error, {:already_started, _}}, which is fine.
      case :net_kernel.start([name, :shortnames]) do
        {:ok, _pid} -> :ok
        {:error, {:already_started, _pid}} -> :ok
        {:error, reason} -> raise "failed to start distributed node: #{inspect(reason)}"
      end
    end
  end

  defp set_cookie do
    case System.get_env("BT_WORKSPACE_COOKIE") do
      nil -> Logger.warning("BT_WORKSPACE_COOKIE not set; relying on ~/.erlang.cookie")
      "" -> :ok
      token -> Node.set_cookie(node_name(), String.to_atom(token))
    end
  end

  # The workspace decoder accepts a JSON binary; build it with the OTP `:json`
  # module so we don't pull in a JSON dependency for one small request.
  defp encode_json(map) do
    :erlang.iolist_to_binary(:json.encode(map))
  end

  defp rpc(mod, fun, args) do
    :rpc.call(node_name(), mod, fun, args)
  end
end
