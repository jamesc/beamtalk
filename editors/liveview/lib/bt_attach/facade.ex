# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.Facade do
  @moduledoc """
  Curated op facade — the **only** path from a LiveView event to a workspace
  `:rpc` call (ADR 0091 Decision 3).

  Phoenix exposes a **finite, named vocabulary** of operations to the browser
  and invokes only those on the workspace. It never proxies an arbitrary
  `{module, function, args}` triple from the browser into `:rpc.call/4` — a
  request for an op outside the catalog is rejected (`{:error, :forbidden_op}`)
  with **no** dist call. This is the substrate per-op RBAC hangs off (BT-2421:
  `capability/1` classifies every op as `:execute | :read | :admin`) and the
  natural audit point.

  **Honest framing (ADR 0091 Decision 3):** the facade is Phoenix-side
  discipline, not a workspace-enforced wall — once attached, the node *can*
  technically `:rpc` anything. Its value is enabling a least-privilege,
  non-`eval` role (Observer), not sandboxing `eval`.

  Calls return the BT-2399 **live terms** verbatim (`{:ok, value, output,
  warnings} | {:error, reason}` etc.) — JSON only at the WebSocket edge. The
  workspace client is injectable (`:bt_attach, :workspace_client`, default
  `BtAttach.Workspace`) so the dispatch contract is testable without a live node.
  """

  require Logger

  @typedoc "Capability class of an op (RBAC dimension, BT-2421)."
  @type capability :: :execute | :read | :admin

  # The authorized op vocabulary and its capability class (ADR 0091 Decision 3/4).
  # `eval`/`load_source`/`save`/`flush`/`reload` inject or run code (execute);
  # the read ops trigger no user code; `kill`/`rotate_cookie` are admin.
  @ops %{
    eval: :execute,
    load_source: :execute,
    save: :execute,
    # ADR 0082 Phase 5 (BT-2293): the System Browser save/flush affordances.
    # `new_class` creates a brand-new class from source at a target path
    # (`Workspace newClass:at:`); `revert` undoes one pending in-memory method
    # patch (`Workspace changes revert:`). Both install/run code (compile +
    # ChangeLog mutation), so they are `:execute` — Owner-only, like `save`.
    new_class: :execute,
    revert: :execute,
    flush: :execute,
    # BT-2590: read the workspace `autoflush` setting (ADR 0082 Phase 4) so the
    # cockpit can skip a redundant post-save git refresh when autoflush is off
    # (an in-memory-only save leaves the working tree untouched). Pure settings
    # read — runs no user code — so it is `:read`, safe for the Observer role.
    autoflush: :read,
    # BT-2598: reload a `.bt` file from disk into the live image after a
    # content-mutating git op (revert) so image == disk (BT-2585). It re-installs
    # code via hot redefinition, so it is `:execute` — Owner-only, like `save`.
    reload: :execute,
    info: :read,
    inspect: :read,
    bindings: :read,
    actors: :read,
    # ADR 0092: the `default`-scope supervision-tree snapshot
    # (`Workspace processes`) is a Read op, scoped exactly like `actors` — it
    # adds supervision structure but no new power, safe for the Observer role.
    processes: :read,
    # ADR 0092: the `system`-scope snapshot (`ProcessNavigation system`) is NOT
    # a plain Observer read. It exposes the whole-node process map (runtime
    # internals, other sessions' supervisors, foreign processes) and is a
    # lateral-movement aid in the compromised-Phoenix scenario ADR 0091 flags,
    # so it is gated to a privileged (`:execute`-holding, i.e. Owner) role: it
    # grants no reconnaissance a code-running caller couldn't already perform.
    processes_system: :execute,
    sessions: :read,
    complete: :read,
    # BT-2555: live-image hover docs for the cockpit editors. Like `complete`,
    # it triggers no user code (pure reflection over the live class registry +
    # stored doc-comments), so it is `:read` — the Observer role may hover.
    hover: :read,
    # BT-2556: parse-only live diagnostics for the cockpit editors. The buffer is
    # parsed + semantically checked for DIAGNOSIS ONLY (no codegen, no install, no
    # eval, no ChangeLog) — pure analysis, no user code — so it is `:read` and the
    # Observer role may see diagnostics.
    diagnostics: :read,
    changes: :read,
    # ADR 0096 (BT-2488): the System Browser browse facade — four read-only
    # ops backing the four-pane navigator. They trigger no user code (pure
    # reflection + xref + stored source text, ADR 0091 Decision 4), so they are
    # safe for the Observer role: an observer can browse classes, protocols,
    # method source, and class definitions without any code-execution power.
    browse_classes: :read,
    browse_protocols: :read,
    browse_method_source: :read,
    browse_class_definition: :read,
    # BT-2578: the native backing-source view. Pure reflection over the facade
    # module's `__beamtalk_meta/0` + the backing module's on-disk `.erl` (read
    # only — no user code), so it is `:read`, safe for the Observer role.
    browse_native_source: :read,
    # BT-2648: the native-modules enumeration + a module-keyed native source
    # view. Same pure reflection as `browse_native_source` (loaded-module
    # enumeration via app env + the backing module's on-disk `.erl`, read only —
    # no user code), so both are `:read`, safe for the Observer role.
    browse_native_modules: :read,
    browse_native_module_source: :read,
    # BT-2670: edit → compile → reload → write-back for a project-owned native
    # (`.erl`) module. It compiles the edited buffer, hot-loads the module, and
    # writes the source to disk — mutating both the live image and the working
    # tree, exactly the power `save`/`new_class` carry — so it is `:execute`,
    # Owner-only. The workspace re-derives project ownership server-side and
    # rejects any deps/stdlib/outside-project target, so the gate is defence in
    # depth, not the sole guard.
    save_native_source: :execute,
    # BT-2495 (Cockpit Phase 3): the navigation aids — Senders/Implementors
    # popovers (`senders`/`implementors`) and the top-bar omni search (`symbols`).
    # Each is a thin `:read` facade over the navigation channel the LSP/MCP
    # already use (ADR 0096): `nav-query` (senders/implementors call sites, backed
    # by the `beamtalk_xref` index) and `nav-symbols` (the class+method outline).
    # They trigger no user code — pure xref + reflection — so they are safe for
    # the Observer role, scoped exactly like the browse ops.
    senders: :read,
    implementors: :read,
    # BT-2639: the protocol equivalents of senders/implementors — the System
    # Browser's protocol-definition action row. Same thin `:read` facade over the
    # `nav-query` channel (kinds `required_methods` / `conforming_classes`, backed
    # by `beamtalk_protocol_registry`). Pure reflection, no user code, so they are
    # safe for the Observer role, scoped exactly like `senders`/`implementors`.
    required_methods: :read,
    conforming_classes: :read,
    # BT-2669: the reverse of "go to native source" — the Beamtalk callers of a
    # native module, backing the "Callers" affordance on the native-module
    # viewer. Same thin `:read` facade over the `nav-query` channel (kind
    # `callers_of_native_module`, backed by the `beamtalk_xref` reverse FFI
    # index). Pure xref, no user code, safe for the Observer role.
    callers_of_native_module: :read,
    symbols: :read,
    subscribe_transcript: :read,
    subscribe_bindings: :read,
    subscribe_actors: :read,
    # BT-2598: wired — registers the LiveView pid on the `classes` push stream so
    # a `ClassLoaded`/`ClassRemoved` (any source change, including the disk→image
    # reload after a git revert) pushes a refresh. A subscription runs no user
    # code, so it is `:read`, safe for the Observer role.
    subscribe_classes: :read,
    # ADR 0105 Phase 1 (BT-2779): reload-induced findings. `reload_findings`
    # reads the current live snapshot (`beamtalk_workspace_findings_store:all/0`)
    # — pure reflection, no user code — so it is `:read`, safe for the
    # Observer role, scoped exactly like `changes`. `subscribe_reload_check`
    # registers the LiveView pid on the `reload_check` push stream so a
    # `ReloadCheckCompleted` announcement (a re-check ran, or a caller's
    # stale findings were cleared) pushes a refresh, mirroring
    # `subscribe_classes`. A subscription runs no user code, so it is
    # `:read` too.
    reload_findings: :read,
    subscribe_reload_check: :read,
    # ADR 0095 §5 / BT-2489 (Cockpit Phase 3): the live-Inspector tracking ops.
    # `subscribe_object`/`unsubscribe_object` arm a per-actor state-change push;
    # `pid_stats` reads live process metrics. All read-only — they trigger no
    # user code (a state-change *signal* plus `process_info` reflection), so they
    # are safe for the Observer role, scoped like `inspect`/`actors`.
    subscribe_object: :read,
    unsubscribe_object: :read,
    pid_stats: :read,
    # BT-2557: the cockpit test-runner pane. `list_tests` discovers loaded
    # TestCase subclasses + their selectors — pure reflection over the class
    # registry, runs no test code — so it is `:read` (the Observer may list).
    # `run_tests` compiles + EVALUATES the tests (mutating the image, just like
    # `eval`), so it is `:execute` — Owner-only, the same gate the eval form uses.
    list_tests: :read,
    run_tests: :execute,
    # BT-2557: load the project's test/ files into the live image (compiles +
    # loads user test code, mutating the image), so it is `:execute` — Owner-only,
    # the same gate `run_tests`/`eval` use. Populates the test-runner catalogue
    # and the System Browser "Tests" group from an empty (src-only) image.
    load_tests: :execute,
    # ADR 0082 Amendment 1 (BT-2586): the cockpit git panel — the post-flush,
    # human-facing VCS surface (disk↔HEAD), distinct from the ChangeLog dirty
    # indicator (memory↔disk). Read ops (`git_status`/`git_diff`/`git_log`) are
    # pure inspection of the on-disk working tree — they run no user code — so
    # they are `:read`, safe for the Observer role. The mutating ops
    # (`git_stage`/`git_unstage`/`git_commit`/`git_revert_file`) shell out to
    # system `git` to alter the index/working tree/history, so they are
    # `:execute` — Owner-only, the same gate `save`/`flush`/`revert` use.
    git_status: :read,
    git_diff: :read,
    git_log: :read,
    git_stage: :execute,
    git_unstage: :execute,
    git_commit: :execute,
    git_revert_file: :execute,
    kill: :admin,
    rotate_cookie: :admin
  }

  @doc "The full authorized op vocabulary (catalog keys)."
  @spec ops() :: [atom()]
  def ops, do: Map.keys(@ops)

  @doc "True if `op` is in the authorized vocabulary."
  @spec known?(atom()) :: boolean()
  def known?(op), do: Map.has_key?(@ops, op)

  @doc "The capability class of `op`, or `nil` if unknown."
  @spec capability(atom()) :: capability() | nil
  def capability(op), do: Map.get(@ops, op)

  @doc """
  Dispatch a curated op to the workspace, returning its live-term result.

  Order of checks (all **before** any dist call):

    1. Off-vocabulary op → `{:error, :forbidden_op}` (no dist call).
    2. RBAC per-op authorization (BT-2421), keyed to the op's capability class
       and the role carried in `ctx[:role]`. A denied op → `{:error,
       :unauthorized}`, audited, **no dist call**. When `ctx` carries no role
       (auth disabled — the trusted localhost story) the op is treated as
       Owner-authorized.
    3. Authorized → audited, then invoked on the workspace client.

  `ctx` carries `:role` (RBAC) and `:user` (audit).
  """
  @spec dispatch(atom(), map(), map()) :: term()
  def dispatch(op, params, ctx \\ %{}) do
    role = Map.get(ctx, :role, :owner)
    user = Map.get(ctx, :user)

    cond do
      not known?(op) ->
        Logger.warning("facade rejected off-vocabulary op: #{inspect(op)}",
          domain: [:beamtalk, :liveview]
        )

        {:error, :forbidden_op}

      BtAttach.Rbac.authorize(role, op) != :ok ->
        BtAttach.Rbac.audit(user, op, role, :deny)
        {:error, :unauthorized}

      true ->
        BtAttach.Rbac.audit(user, op, role, :allow)
        invoke(op, params, ctx)
    end
  end

  # Route the ops the IDE drives today to the workspace client. Catalog ops with
  # no UI binding yet return `:unsupported_op` rather than silently no-op'ing.
  defp invoke(:eval, %{session_pid: pid, code: code}, _ctx), do: client().eval(pid, code)
  defp invoke(:inspect, %{term: term}, _ctx), do: client().inspect_value(term)
  defp invoke(:bindings, %{session_pid: pid}, _ctx), do: client().list_bindings(pid)
  defp invoke(:changes, _params, _ctx), do: client().change_history()

  # ADR 0105 Phase 1 (BT-2779): current live snapshot of reload-induced
  # findings — the workspace/cockpit UI's initial-load / manual-refresh
  # counterpart to the `reload_check` push stream (`subscribe_reload_check`
  # below), same relationship `changes` has to `subscribe_classes`.
  defp invoke(:reload_findings, _params, _ctx), do: client().reload_findings()

  # ADR 0092: the supervision-tree snapshot. `processes` is the default-scope
  # Read view (runtime plumbing filtered); `processes_system` is the privileged
  # whole-node view, gated to Owners by the :execute capability above.
  defp invoke(:processes, %{session_pid: pid}, _ctx),
    do: client().supervision_tree(pid, "default")

  defp invoke(:processes_system, %{session_pid: pid}, _ctx),
    do: client().supervision_tree(pid, "system")

  # ADR 0096 (BT-2488): the four System Browser browse ops. Each returns a
  # `{:value, json_value}` live term (the wire-shaped browse rows) verbatim —
  # JSON only at the WebSocket edge. `side` defaults to `"instance"` (the
  # browser's default toggle); the selector / class params are required by the
  # respective ops and validated workspace-side (a missing class / bad side
  # comes back as a structured `#beamtalk_error{}`).
  defp invoke(:browse_classes, _params, _ctx), do: client().browse_classes()

  defp invoke(:browse_protocols, %{class: class} = params, _ctx) do
    side = Map.get(params, :side, "instance")

    if is_binary(class) and is_binary(side),
      do: client().browse_protocols(class, side),
      else: {:error, :invalid_params}
  end

  defp invoke(:browse_method_source, %{class: class, selector: selector} = params, _ctx) do
    side = Map.get(params, :side, "instance")

    if is_binary(class) and is_binary(side) and is_binary(selector),
      do: client().browse_method_source(class, side, selector),
      else: {:error, :invalid_params}
  end

  defp invoke(:browse_class_definition, %{class: class}, _ctx) do
    if is_binary(class),
      do: client().browse_class_definition(class),
      else: {:error, :invalid_params}
  end

  # BT-2578: the backing Erlang source of a native: class. `selector` is
  # optional — present → also resolve the matching `handle_call` clause; absent
  # → whole-module view. A non-native class comes back as a structured
  # `#beamtalk_error{}` workspace-side.
  defp invoke(:browse_native_source, %{class: class} = params, _ctx) do
    if is_binary(class),
      do: client().browse_native_source(class, Map.get(params, :selector)),
      else: {:error, :invalid_params}
  end

  # BT-2648: enumerate a loaded package's hand-written native Erlang modules
  # (no params) — the System Browser's native-modules section data source.
  defp invoke(:browse_native_modules, _params, _ctx), do: client().browse_native_modules()

  # BT-2648: the read-only native pane keyed by a standalone native `module`
  # (no backing class) — the module surfaced by `browse_native_modules`.
  defp invoke(:browse_native_module_source, %{module: module}, _ctx) do
    if is_binary(module),
      do: client().browse_native_module_source(module),
      else: {:error, :invalid_params}
  end

  # BT-2670: save a project-owned native (`.erl`) module — compile the edited
  # buffer, hot-reload the module, and write the source to disk. `module` is the
  # native module name, `source` the edited Erlang. The workspace re-derives
  # project ownership server-side and rejects deps/stdlib/outside-project targets;
  # bad params are `:invalid_params` with no dist call, matching the other ops.
  defp invoke(:save_native_source, %{module: module, source: source}, _ctx) do
    if is_binary(module) and is_binary(source),
      do: client().save_native_source(module, source),
      else: {:error, :invalid_params}
  end

  # BT-2495: the Senders/Implementors popovers and the omni-search index. Each
  # routes to the matching navigation read on the workspace client (`nav-query` /
  # `nav-symbols`) and returns the live `{:value, _}` term verbatim. The selector
  # param is required by `senders`/`implementors`; `symbols` takes an optional
  # `scope` (`"all"` default). Bad params come back as `:invalid_params` with no
  # dist call, matching the browse ops.
  defp invoke(:senders, %{selector: selector}, _ctx) do
    if is_binary(selector),
      do: client().senders_of(selector),
      else: {:error, :invalid_params}
  end

  defp invoke(:implementors, %{selector: selector}, _ctx) do
    if is_binary(selector),
      do: client().implementors_of(selector),
      else: {:error, :invalid_params}
  end

  # BT-2639: the protocol-definition action row. `protocol` is the protocol name;
  # both route to the matching `nav-query` kind on the workspace client and return
  # the live `{:value, _}` term verbatim. Bad params come back as `:invalid_params`
  # with no dist call, matching the senders/implementors ops.
  defp invoke(:required_methods, %{protocol: protocol}, _ctx) do
    if is_binary(protocol),
      do: client().required_methods_of(protocol),
      else: {:error, :invalid_params}
  end

  defp invoke(:conforming_classes, %{protocol: protocol}, _ctx) do
    if is_binary(protocol),
      do: client().conforming_classes_of(protocol),
      else: {:error, :invalid_params}
  end

  # BT-2669: the native-module "Callers" view. `module` is the native (Erlang)
  # module name; routes to the `callers_of_native_module` nav-query kind and
  # returns the live `{:value, _}` term verbatim. Bad params come back as
  # `:invalid_params` with no dist call, matching the senders/implementors ops.
  defp invoke(:callers_of_native_module, %{module: module}, _ctx) do
    if is_binary(module),
      do: client().callers_of_native_module(module),
      else: {:error, :invalid_params}
  end

  defp invoke(:symbols, params, _ctx) do
    scope = Map.get(params, :scope, "all")

    if is_binary(scope),
      do: client().symbol_index(scope),
      else: {:error, :invalid_params}
  end

  # BT-2544: backend-driven autocomplete for the CodeMirror editors. `code` is
  # the current line up to the caret; the client reuses the REPL `complete` op
  # (receiver-aware, binding-aware) and returns `{:ok, [String.t()]}`. Capability
  # is `:read` (completion runs no user code), so the Observer role completes
  # too. A bad shape is `:invalid_params` with no dist call, matching the nav ops.
  defp invoke(:complete, %{session_pid: pid, code: code}, _ctx) do
    if is_pid(pid) and is_binary(code),
      do: client().complete(pid, code),
      else: {:error, :invalid_params}
  end

  # BT-2555: live-image hover for the CodeMirror editors. `code` is the editor
  # line up to the hovered token; the client resolves it to class/method docs
  # against the live image via the `hover` op and returns `{:ok, String.t()}`
  # (the markdown, "" when nothing resolves). Capability `:read` (no user code),
  # so the Observer role hovers too. A bad shape is `:invalid_params` with no
  # dist call, matching `:complete`.
  defp invoke(:hover, %{session_pid: pid, code: code}, _ctx) do
    if is_pid(pid) and is_binary(code),
      do: client().hover(pid, code),
      else: {:error, :invalid_params}
  end

  # BT-2556: parse-only diagnostics for the CodeMirror editors. `code` is the
  # full editor buffer; the client runs the compiler's side-effect-free
  # `diagnostics` path and returns `{:ok, [diagnostic]}` (each a map with
  # `from`/`to`/`severity`/`message`). Unlike `:complete`/`:hover` there is no
  # session/receiver context — diagnostics analyse the buffer in isolation — so
  # only `code` is required. Capability `:read` (no user code), so the Observer
  # role sees diagnostics too. A non-binary `code` is `:invalid_params` with no
  # dist call, matching `:complete`/`:hover`.
  # `mode` (BT-2569) selects the parse grammar: `"expression"` (default — a
  # top-level script) or `"method"` (a bare method body, the System Browser
  # method editor). Absent → `"expression"`, so the Workspace/REPL editors are
  # unaffected. A non-binary `code` is `:invalid_params` with no dist call.
  defp invoke(:diagnostics, %{code: code} = params, _ctx) do
    if is_binary(code) do
      # Normalise `mode` to a known binary at the facade boundary — the shared
      # capability gate for every caller, not just the LiveView. Anything but
      # "method" degrades to the safe default, so a caller that bypasses the
      # LiveView event whitelist can't push a non-binary into
      # `Workspace.diagnostics/2`'s `is_binary(mode)` guard (BT-2569).
      mode = if Map.get(params, :mode) == "method", do: "method", else: "expression"
      client().diagnostics(code, mode)
    else
      {:error, :invalid_params}
    end
  end

  # BT-2557: the cockpit test-runner pane. `list_tests` discovers test classes
  # (no args, pure reflection). `run_tests` runs all tests (`class` is nil) or a
  # single class (`class` is a binary). A non-binary, non-nil class is a bad
  # shape → `:invalid_params` with no dist call, matching the browse ops.
  defp invoke(:list_tests, _params, _ctx), do: client().list_tests()

  defp invoke(:run_tests, %{class: class}, _ctx) do
    if is_nil(class) or is_binary(class),
      do: client().run_tests(class),
      else: {:error, :invalid_params}
  end

  defp invoke(:run_tests, _params, _ctx), do: client().run_tests(nil)

  # BT-2557: load the project's test/ files into the live image so the runner
  # catalogue + System Browser "Tests" group are populated (no args).
  defp invoke(:load_tests, _params, _ctx), do: client().load_tests()

  defp invoke(:flush, _params, _ctx), do: client().flush()

  # BT-2590: the workspace `autoflush` boolean. Returns the flag directly (never a
  # `{:ok, _}` tuple — the client defaults a degraded read to `false`), so the
  # caller can branch on it without unwrapping.
  defp invoke(:autoflush, _params, _ctx), do: client().autoflush()

  defp invoke(:save, %{class: class, selector: selector, source: source}, _ctx),
    do: client().save_method(class, selector, source)

  # ADR 0082 Phase 5 (BT-2293): create a new class from source at a path
  # (`Workspace newClass:at:`). Both args are required binaries; a bad shape is
  # `:invalid_params` with no dist call, matching the browse ops.
  defp invoke(:new_class, %{source: source, path: path}, _ctx) do
    if is_binary(source) and is_binary(path),
      do: client().new_class(source, path),
      else: {:error, :invalid_params}
  end

  # ADR 0082 Phase 5 (BT-2293): revert one pending in-memory method patch by its
  # `(class, selector)` — the binaries carried by a `Workspace changes` row.
  defp invoke(:revert, %{class: class, selector: selector}, _ctx) do
    if is_binary(class) and is_binary(selector),
      do: client().revert(class, selector),
      else: {:error, :invalid_params}
  end

  defp invoke(:subscribe_transcript, %{pid: pid}, _ctx), do: client().subscribe_transcript(pid)
  defp invoke(:subscribe_bindings, %{pid: pid}, _ctx), do: client().subscribe_bindings(pid)

  # BT-2598: the class-lifecycle push stream. Registers the LiveView pid on the
  # `classes` stream so a `ClassLoaded`/`ClassRemoved` (including any hot
  # redefinition — an in-memory patch or the disk→image reload after a git
  # revert) pushes a refresh trigger to the cockpit. Read capability (a
  # subscription registration runs no user code), like the other subscribe ops.
  defp invoke(:subscribe_classes, %{pid: pid}, _ctx), do: client().subscribe_classes(pid)

  # ADR 0105 Phase 1 (BT-2779): the reload-induced-findings push stream.
  # Registers the LiveView pid on the `reload_check` stream so a
  # `ReloadCheckCompleted` announcement pushes a refresh, mirroring
  # `subscribe_classes` exactly.
  defp invoke(:subscribe_reload_check, %{pid: pid}, _ctx),
    do: client().subscribe_reload_check(pid)

  # BT-2598: reload a `.bt` file from disk into the live image after a
  # content-mutating git op (revert), so image == disk (BT-2585). `path` is the
  # project-relative path git restored; a bad shape is `:invalid_params` with no
  # dist call, matching the browse ops. `:execute` — Owner-only, like `save`.
  defp invoke(:reload, %{path: path}, _ctx) do
    if is_binary(path), do: client().reload_file(path), else: {:error, :invalid_params}
  end

  # ADR 0095 §5 / BT-2489: per-object change tracking. `subscribe_object` watches
  # one inspected actor (`term`) for the LiveView `pid`; `pid_stats` reads its
  # live process metrics. The term is the live `{:beamtalk_object, ...}` handle
  # held in the Inspector pane; non-pid-backed terms come back
  # `{:error, :not_inspectable}` from the client.
  defp invoke(:subscribe_object, %{term: term, pid: pid}, _ctx),
    do: client().subscribe_object_changes(term, pid)

  defp invoke(:unsubscribe_object, %{term: term, pid: pid}, _ctx),
    do: client().unsubscribe_object_changes(term, pid)

  defp invoke(:pid_stats, %{term: term}, _ctx), do: client().pid_stats(term)

  # ADR 0082 Amendment 1 (BT-2586): the cockpit git panel. Read ops take no
  # session/receiver context — they inspect the on-disk working tree directly.
  # `git_status`/`git_log` need no params; `git_diff` requires a binary `path`.
  defp invoke(:git_status, _params, _ctx), do: client().git_status()

  defp invoke(:git_diff, %{path: path}, _ctx) do
    if is_binary(path), do: client().git_diff(path), else: {:error, :invalid_params}
  end

  defp invoke(:git_log, params, _ctx) do
    count = Map.get(params, :count, 20)

    if is_integer(count) and count > 0,
      do: client().git_log(count),
      else: {:error, :invalid_params}
  end

  # Mutating git ops (Owner-gated via :execute). `path`/`message` are required
  # binaries; a bad shape is `:invalid_params` with no dist call, matching the
  # browse ops.
  defp invoke(:git_stage, %{path: path}, _ctx) do
    if is_binary(path), do: client().git_stage(path), else: {:error, :invalid_params}
  end

  defp invoke(:git_unstage, %{path: path}, _ctx) do
    if is_binary(path), do: client().git_unstage(path), else: {:error, :invalid_params}
  end

  defp invoke(:git_commit, %{message: message}, _ctx) do
    if is_binary(message), do: client().git_commit(message), else: {:error, :invalid_params}
  end

  defp invoke(:git_revert_file, %{path: path}, _ctx) do
    if is_binary(path), do: client().git_revert_file(path), else: {:error, :invalid_params}
  end

  defp invoke(op, _params, _ctx), do: {:error, {:unsupported_op, op}}

  defp client, do: Application.get_env(:bt_attach, :workspace_client, BtAttach.Workspace)
end
