# ADR 0125: OTP Releases and Upgrade Compatibility

## Status
Accepted (2026-09-22)

## Context

### Problem statement

Beamtalk has no production deployment story. `beamtalk build --escript`
(ADR 0099 §4) packages a *script* — one file, one entry point, no
supervision tree, no configuration, no way to attach. Everything an
operator actually deploys is missing: a versioned artifact, a boot script,
`sys.config`/`vm.args`, a remote console, an upgrade path, and a statement
of which Erlang/OTP versions the artifact is valid on.

ADR 0061 § "Future: Release Mode" deferred `beamtalk release` to this ADR
and left one design constraint behind:

> `beamtalk_workspace_sup` must support release mode cleanly — either via a
> third config variant or by extracting `beamtalk_repl_server` +
> `beamtalk_session_sup` into a standalone OTP application releases can
> include without the full workspace.

ADR 0099's steelman recorded "Ship OTP releases, not escripts" as the
operator cohort's outstanding ask, answered "later". This is later.

Three questions have to be answered together, because each one's answer
constrains the others:

1. **What is a release and how is it built?** (Part 1)
2. **How does a running release get from version N to N+1?** (Part 2)
3. **Which OTP versions is a shipped `.beam` file valid on, and what does a
   node at release vN do when it meets a node at vN+1?** (Part 3)

### Current state

**Packaging.** `beamtalk build --escript` (`crates/beamtalk-cli/src/commands/escript.rs`)
compiles the project, locates the runtime/stdlib beams, and zips them
behind a shebang with a generated `main/1`. It starts the workspace in run
mode (`repl = false`), registers the project classes in topological order,
and maps the outcome to a POSIX exit status. It is ~650 lines and it works,
but it produces a script, not a service.

**OTP applications already exist.** `beamtalk build` generates a real `.app`
file per package (`commands/build/outputs.rs`, ADR 0026 §3), including
`{modules, …}`, `{applications, …}`, `{vsn, …}` from `[package] version`,
the `{env, [{classes, …}]}` registration list, and — when
`[application] supervisor` is set — a generated `beamtalk_<pkg>_app`
callback module with `{mod, …}`. The runtime itself is already an umbrella
of four applications (`beamtalk_runtime`, `beamtalk_stdlib`,
`beamtalk_compiler`, `beamtalk_workspace`). **The hard part of release
assembly — turning a project into OTP applications — is done.**

**But the on-disk layout is not an OTP lib tree.** Artifacts land in
`_build/dev/ebin/` (flat, `BuildLayout::ebin_dir()`) and dependencies in
`_build/deps/<name>/ebin/`; the installed runtime sits at
`PREFIX/lib/beamtalk/lib/<app>/ebin/` (`RuntimeLayout::Installed`,
`crates/beamtalk-core/src/ffi_type_specs.rs`). None of these carry the
`lib/<app>-<vsn>/ebin/` shape `systools` requires.

**Workspace modes are a boolean.** `beamtalk_workspace_sup`'s config has
`repl => boolean()`. `repl = false` (run mode) skips the file logger, the
REPL server, the session supervisor, the idle monitor, and the ADR 0105
stores. `repl = true` starts all of them. There is no third position, and
the two are coupled in a way release mode cannot use: a production node
needs the REPL server (a remote console) but must **not** have the idle
monitor (which self-terminates the node after `max_idle_seconds`) and must
not carry the live-development stores. `start_compiler => false` already
exists as an escape hatch — the escript sets it, because a packaged
artifact ships no compiler port binary.

**Hot reload has a contract, and it is relup-shaped.** ADR 0123 (BT-3524,
Implemented) decided `shapeVersion:` / `migrateFromVN:` and pinned the
runtime contract: `code_change/3`'s `Extra` is `#{module := atom()}`, and
the callback reads the *from* version **out of the state map**
(`'__shape_version__'`), deliberately ignoring `OldVsn` — "the *state* is
the source of truth, which is exactly what lets the same chain run for
persistence, where there is no `OldVsn` at all". `beamtalk_shape_chain`
(pure leaf) and `beamtalk_shape_migration` (lookups and effects) exist;
`beamtalk_hot_reload:code_change/3` already matches
`#{module := Module}`. ADR 0123 explicitly left two things here:
downgrade hooks (`migrateToVN:` — "reserved, not defined; BT-3528 decides
whether relups need them") and closing the load→suspend window ("closing it
properly is `release_handler`'s job and is deferred to BT-3528").

**There is no appup, relup, `systools`, `release_handler` or `relx` usage
anywhere in the tree.** The only hits for those words are a comment in
`outputs.rs` ("so release tooling (appup generation, etc.) can account for
it") and `beamtalk_hot_reload`'s `code_change/3`.

**OTP version policy is a README sentence.** "Erlang/OTP 27+" appears twice
in `README.md`; `beamtalk doctor` hardcodes `27` in `check_erl`'s guard
(`Some(major) if major >= 27`) and in five user-facing strings across
`check_erl` and `print_install_instructions` (including two
`asdf install erlang 27.2` lines). `parse_otp_major` itself is
version-agnostic — `version.split('.').next()?.parse()` — and needs no
change. CI has **no OTP matrix** — every job
runs the single version pinned in `.tool-versions` (currently `erlang 28.5`).
The only place a compound OTP version is computed is
`build_stamp::current_otp_version()`, which produces the
`<otp_release>-<erts_version>` string (e.g. `27-15.0.1`) that ADR 0098's
provenance stamp and ADR 0075/BT-2470's shared type-spec cache
(`<cache>/beamtalk/otp-specs/<otp_release>-<erts_version>/`) both key on.

**Security posture moved since ADR 0061 was written.** ADR 0061's release
note says the release REPL requires "TLS + auth". ADR 0058 records that
mTLS via `--tls` was **removed** (PR #1401) and that the platform's stance
is the Trusted Developer Tool model: loopback binding plus the workspace
cookie handshake is the boundary, and remote access is delivered by a
reverse proxy (Caddy/nginx) or an overlay network (Tailscale/WireGuard),
not by Beamtalk-implemented TLS. This ADR has to reconcile the two, and it
does so in ADR 0058's favour (§1.6).

### Constraints

- **No new user prerequisite.** The README's install contract is "Erlang/OTP
  27+ with `erl` and `erlc` on PATH". `rebar3` is a *contributor* tool
  (pinned in `.tool-versions` for this repo's own build); it is not
  installed on a Beamtalk user's machine.
- **ADR 0022** moved the compiler to an OTP port precisely to stop
  orchestrating external toolchains. A release path that shells into a
  third-party build tool re-introduces what that ADR removed.
- **ADR 0027** puts Windows in tier 1: no bash, no hardcoded paths, no
  POSIX-only process management.
- **ADR 0123's contract is fixed.** Whatever Part 2 decides, the
  `code_change/3` `Extra` must be ADR 0123's `#{module := atom()}` — the
  issue's own wording: *one mechanism, not two*.
- **Surface parity (ADR-adjacent, `docs/development/surface-parity.md`)**:
  any operation not labelled `surface-specific` must be equivalent
  everywhere it appears.
- **No duplication.** The OTP support window, the compound OTP version
  string, and the class dependency topological order all already exist in
  exactly one place each; this ADR must route through them, not re-derive
  them (`docs/development/architecture-principles.md` § Duplication & the
  Shared-Leaf-Module Pattern).

---

## Decision

Ship **`beamtalk release`** as a first-class CLI command producing a
standard OTP release, assembled by Beamtalk and booted by OTP's own
`systools`. Support **restart-based (blue/green) upgrades in v1** and defer
relup to a later phase whose contract this ADR pins now. Declare an **OTP
support window of "current major + previous major, minimum 27"**, enforced
from a single declared source.

### Part 1 — `beamtalk release`

#### 1.1 Command shape

```bash
beamtalk release                      # build the release for this project
beamtalk release --output dist/       # override output dir
beamtalk release --no-include-erts    # slim image; requires a host OTP
```

Output lands at `_build/release/<name>-<vsn>/` (a new
`BuildLayout::release_dir(name, vsn)`), plus a `.tar.gz` of the same tree —
the artifact you copy to a server or `ADD` into a container.

The whole story, end to end, is three commands — build, run, look inside:

```bash
$ beamtalk release
Built release orders-1.4.0 (with ERTS 16.0.2, linux/x86_64).
  → _build/release/orders-1.4.0.tar.gz  (48.2 MB)

$ _build/release/orders-1.4.0/bin/orders foreground
[orders 1.4.0] OrdersSup started; console off (see [release] console)

$ _build/release/orders-1.4.0/bin/orders rpc "Beamtalk releaseInfo"
#{#release => "orders", #release_version => "1.4.0", #otp_release => "28-16.0.2", …}
```

No Erlang on the host, no `sys.config` written by hand, nothing to `-pa`;
the third line reaches *into the running node* — `rpc`, not a second VM
(§1.7). Everything after this section is what those three lines are made of.

`[application] supervisor` is **required**: a release is a service. A
project without it gets a build error naming `beamtalk build --escript` as
the right artifact for a script.

```
Error: `beamtalk release` requires a root supervisor.

  beamtalk.toml has no [application] section, so there is nothing for the
  release to supervise. A release is a long-running service.

  For a one-shot program, build an escript instead:
      beamtalk build --escript --entry "Main main:"

  To make this project a service, declare its root supervisor:
      [application]
      supervisor = "AppSup"
```

#### 1.2 `beamtalk.toml` configuration

```toml
[package]
name = "orders"
version = "1.4.0"

[application]
supervisor = "OrdersSup"

[release]
# All keys optional; shown with their defaults.
name          = "orders"        # defaults to [package] name
apps          = []              # extra OTP apps beyond the computed closure
include-erts  = true            # bundle this machine's ERTS
console       = false           # start the REPL/remote-console listener
bind          = "127.0.0.1"     # only meaningful when console = true
# Erlang distribution is always on and loopback-bound (§1.6): it is what
# stop / ping / rpc / remote_console use. `console` governs only the
# WebSocket REPL listener. There is no distribution toggle.
sys-config    = "config/sys.config"
vm-args       = "config/vm.args"
strip-beams   = false           # drop debug_info chunks
include-compiler = false        # ship the compiler port (live-image mode)
```

**The version is derived, never declared.** `[release]` has no `version`
key: the release version is `[package] version`, exactly as the `.app`
file's `{vsn, …}` already is. This is the repo's own single-source-of-truth
rule (`VERSION` at root, `docs/development/releasing.md`) applied to user
projects. A `version` key in `[release]` is a manifest error with a hint
pointing at `[package] version`.

**`apps` is additive, not authoritative.** The included application set is
*computed*: the project's own app, the ADR 0070 dependency closure (from
`_build/deps/`, the same graph `beamtalk build` already resolves), the
runtime closure, and `kernel`/`stdlib`/`sasl` (`sasl` is already transitive
— `beamtalk_workspace.app.src` declares it — and would be required
regardless: `release_handler` lives there). `[release] apps` names *extra* OTP apps (an
Erlang dependency reached only via FFI, say) that the closure cannot see.

**The closure is an application-level set, and an application's own `.app`
decides its dependencies — not this ADR.** Two consequences follow, and
both are costs of §1.4's mode-variant decision rather than things a
`[release]` key can opt out of:

- **`beamtalk_workspace` is in every release**, because §1.4 keeps release
  mode inside `beamtalk_workspace_sup`. Its development stores are
  *unstarted modules*, not absent applications. The `.rel` lists the app;
  the supervisor does not start those children.
- **`cowboy`/`ranch` are therefore unconditional too**, even at
  `console = false`. `beamtalk_workspace.app.src` declares `cowboy` in its
  `{applications, …}` list, and `systools:make_script/2` *hard-errors* on a
  `.rel` that omits a declared dependency — verified:
  `{error, systools_make, {undefined_applications,[crypto]}}` for an
  equivalent minimal case. So the listener's *code* ships whether or not a
  listener is *started*. Making this conditional means moving `cowboy` out
  of `beamtalk_workspace`'s hard dependencies, which is part of the
  extraction §1.4 defers, not a separate knob.

`beamtalk_compiler` **is** genuinely excludable, and is excluded by
default, precisely because nothing *declares* it —
`beamtalk_workspace_sup` starts it dynamically via
`application:ensure_all_started/1`, which is exactly the seam
`start_compiler => false` already uses for the escript.

#### 1.3 Build mechanism: Rust stages, OTP's `systools` boots

**Decision: assemble the lib tree in Rust, write the `.rel` from the
computed app closure, and call `systools:make_script/2` and
`systools:make_tar/2` through `erl -noshell`.** No `rebar3`, no `relx`,
no `mix`.

```
_build/release/orders-1.4.0/
├── bin/
│   ├── orders                     # POSIX launcher (sh)
│   └── orders.cmd                 # Windows launcher (ADR 0027 tier 1)
├── erts-15.0.1/                   # when include-erts = true
├── lib/
│   ├── orders-1.4.0/ebin/         # the project's bt@orders@*.beam + orders.app
│   ├── beamtalk_runtime-0.4.0/ebin/
│   ├── beamtalk_stdlib-0.4.0/ebin/
│   └── …
└── releases/
    ├── RELEASES                   # release_handler:create_RELEASES/4, at assembly
    └── 1.4.0/
        ├── orders.rel
        ├── start.boot             # systools:make_script/2
        ├── sys.config
        ├── vm.args
        ├── shapes.json            # §3.4 shape manifest
        └── beamtalk-provenance.json  # §1.7
```

Why `systools` rather than relx-via-rebar3:

- **`systools` is OTP.** It ships in `sasl`, which every Erlang install has.
  The prerequisite list stays exactly "`erl` on PATH" — unchanged from
  today. Requiring `rebar3` would add a user-facing install step for the
  one command that operators use most.
- **relx is a convenience layer over `systools`.** What it adds beyond
  `make_script`/`make_tar` is overlays and shell scripts — and the shell
  scripts are precisely the part we must write ourselves anyway, because
  they have to carry ADR 0099's `Console`/two-tier-exit semantics and ADR
  0027's Windows `.cmd`.
- **ADR 0022's logic applies.** Orchestrating an external build tool from
  Rust is the pattern that ADR was written to retire. Calling an OTP
  library function through the `erl` we already require is not the same
  thing: there is no second toolchain to discover, version-match, or
  vendor.
- **`systools:make_relup/4` comes with it**, so Part 2's deferred phase
  needs no new dependency either.

Staging is a *copy* into a fresh versioned tree — `_build/dev/ebin/` and
the installed `lib/<app>/ebin/` layouts are untouched. The staging step is
the one genuinely new piece of Rust: read each app's **generated** `.app`
for its `{vsn, …}`, create `lib/<app>-<vsn>/ebin/`, copy the beams and the
`.app`, and (when `include-erts`) copy the ERTS tree that `erl` reports via
`code:root_dir/0` + `erlang:system_info(version)`.

**Stage from the generated `.app`, never from `.app.src`.** All five runtime
`.app.src` files — the four that can ship (`beamtalk_runtime`,
`beamtalk_stdlib`, `beamtalk_workspace`, and `beamtalk_compiler` under
`include-compiler`) plus `beamtalk_test_support`, which never does — carry
`{vsn, {cmd, "escript ../../../scripts/version.escript"}}`
— a *rebar3* `.app.src` template construct that rebar3 resolves when it
generates the real `.app`. `systools` reads plain `.app` files and has no
idea what `{cmd, …}` means. The resolved artifact
(`runtime/_build/default/lib/<app>/ebin/<app>.app`, e.g.
`{vsn,"0.4.0-dev+38a688d"}`) is the only valid staging source. This is a
one-line rule that is very easy to get wrong in the opposite direction, so
it is stated here rather than left to be discovered.

**The dev version string is safe in a lib directory name — verified, not
assumed.** `BEAMTALK_VERSION` carries a prerelease suffix in source builds
(`0.4.0-dev+38a688d`), so a staged directory is
`lib/beamtalk_runtime-0.4.0-dev+38a688d/ebin` — a name containing both `+`
and additional hyphens, which raises a fair question about how `App-Vsn`
is split. A napkin check (Phase 0 below) confirms `systools:make_script/2`
accepts exactly this shape and emits a valid `.boot`; it also emits
`{warning, missing_sasl}` when `sasl` is left out of the `.rel`, which is
`systools` independently confirming the `sasl` requirement noted in §1.2.

**But a `.boot` that builds is not a `.boot` that boots — `$ROOT`.** The
same check, taken one step further, found the trap: `make_script/2`
succeeds with `{path, ["lib/*/ebin"]}`, but the emitted `.script` records
every path as `$ROOT/lib/<app>-<vsn>/ebin`. With bundled ERTS, `$ROOT` is
the release directory and that resolves. With `--no-include-erts`, `$ROOT`
is the **host** install's `code:root_dir/0`, every project and runtime app
is unresolvable, and the node dies with `undef` on `<pkg>_app:start/2` —
reproduced. The assembly therefore passes absolute paths plus
`{variables, [{"RELEASE_DIR", Dir}]}` so the script says
`$RELEASE_DIR/lib/…`, and the launcher passes `-boot_var RELEASE_DIR <dir>`
on every boot — also reproduced working. `RELEASES` is likewise not
something the node writes for itself at first boot (booted with `sasl`,
`release_handler:which_releases/0` lists only OTP's own permanent release
and creates no file); the assembly step calls
`release_handler:create_RELEASES/4`, as relx does, so that Phase 7's
`install_release/1` has a release to upgrade *from*.

**Cross-compilation is not supported.** `include-erts = true` makes the
release OS- and architecture-specific; build it on (or in a container
matching) the target. This is stated in the command's own output, not
buried in docs:

```
Built release orders-1.4.0 (with ERTS 15.0.1, linux/x86_64).
  → _build/release/orders-1.4.0.tar.gz  (48.2 MB)

This release bundles ERTS and runs only on linux/x86_64.
Build on the target platform, or use --no-include-erts to require a
host Erlang/OTP 28, 29 or 30 (§3.2).
```

#### 1.4 Release-mode runtime — resolving the ADR 0061 constraint

**Decision: a third mode on `beamtalk_workspace_sup`, not an extracted
application.** The boolean `repl => boolean()` becomes
`mode => run | workspace | release`.

| | `run` | `workspace` | `release` |
|---|---|---|---|
| Class bootstrap (ADR 0019 singletons) | ✓ | ✓ | ✓ |
| Project-module activation | ✓ scans `_build/` | ✓ scans `_build/` | ✓ **from the `.app` module lists** — no scan, and *not* preloaded (below) |
| `beamtalk_compiler` app | ✓ | ✓ | ✗ (unless `include-compiler`) |
| `beamtalk_actor_sup` | ✓ | ✓ | ✓ |
| `telemetry_poller` (declared by `beamtalk_runtime`) | ✓ | ✓ | ✓ — mode-independent, ADR 0069 |
| Workspace file logger / on-disk artifacts | ✗ | ✓ | ✗ |
| ADR 0105 signature/shape/findings stores, recheck worker | ✗ | ✓ | ✗ |
| ADR 0082 ChangeLog | memory-only | ✓ on disk | memory-only |
| `alias_xref` | ✗ | ✓ | ✗ |
| `beamtalk_session_sup` + `beamtalk_repl_server` | ✗ | ✓ | opt-in (`console`) |
| `beamtalk_idle_monitor` | ✗ | ✓ | **✗ — never** |

Two rows carry the argument.

**The idle monitor is the vivid case, not the decisive one.**
`beamtalk_idle_monitor` does not merely stop the workspace supervisor — on
`max_idle_seconds` it calls **`init:stop/0`**, halting the entire node
(`beamtalk_idle_monitor.erl:120-122`). A production service that has served
no REPL traffic for four hours is *healthy*; `repl = true` with the
defaults would take the node down under it. It is, however, already
switchable — `auto_cleanup => false` reaches the monitor as
`enabled => false` and `check_idle` becomes a no-op
(`beamtalk_workspace_sup.erl:78,346`; `beamtalk_idle_monitor.erl:110`) — so
on its own it argues only that a release must *know* to switch it off,
which is what a mode is. The decisive rows are the ones with no switch at
all: the file logger, the ADR 0105 stores and recheck worker, `alias_xref`,
and the compiler start are gated on the single `Repl` boolean and nothing
else, so there is no configuration of today's supervisor that starts the
REPL server without also starting the live-development image around it.

**The REPL server is why `release` cannot be `repl = false`.** An operator
needs a console. Run mode has none.

**The ChangeLog row is the one place release mode inherits run mode rather
than diverging.** `beamtalk_workspace_changelog` is started
*unconditionally* today, in every mode; only its `workspace_id` is gated,
dropped to `undefined` in run mode so the log stays memory-only with no
on-disk artifacts. Release mode takes the same memory-only form — not
because a release has anything to log (§1.5 removes every mutation that
would write an entry) but because leaving it as-is costs one idle
gen_server. The one touch it does need is mechanical: `changelog_workspace_id/2`
is today a two-clause boolean function
(`beamtalk_workspace_sup.erl:365-366`); the mode triple gives it a third
clause, `release → undefined`. Listing the row as "✗" would have described
a removal nobody is making.

*Why not extract `beamtalk_repl_server` + `beamtalk_session_sup` into a
standalone application (ADR 0061's other option)?* Because the extraction
does not cut where the ADR assumed. `beamtalk_repl_server` dispatches to
`beamtalk_repl_ops`, and the op modules
(`beamtalk_repl_ops_load`, `_browse`, `_dev`, `_watch`, …) reach
`beamtalk_workspace_changelog`, `beamtalk_workspace_shape_store`,
`beamtalk_workspace_signature_store`, `beamtalk_alias_xref` and
`beamtalk_workspace_findings_store` — all in `beamtalk_workspace`. Moving
two modules out would either drag every store with them (no win) or fork
the op vocabulary between "console ops" and "workspace ops", which
`docs/development/surface-parity.md` exists to prevent. The extraction
becomes mechanical *after* §1.5's capability classification gives the op
layer a seam; until then a mode variant is the smaller, more honest change.
The extraction is recorded as the follow-up, not as this ADR's work.

**Class loading in release mode — the boot script does *not* do it, and
must not be asked to.** ADR 0061's forward-looking table marked release
mode "✗ (pre-loaded)", and an earlier draft of this ADR repeated it. It is
wrong in both of the VM's boot modes, for two different reasons that were
checked empirically during review:

- In **interactive** mode — the default when nothing passes `-mode` — the
  boot script's `{primLoad, …}` entries for application modules are
  *skipped*; modules load lazily on first reference. A `bt@*` class that
  no code names before it is needed is never loaded, never runs
  `register_class/0`, and is invisible to the registry: `Object
  allSubclasses` is short, and `Orders backfillPricing` is a DNU.
- In **embedded** mode, `primLoad` *does* load every module — **before any
  application starts**, and it runs `-on_load` as it goes. Every `bt@*`
  module that defines a class, protocol or foreign extension carries
  `-on_load(register_class/0)` — emitted under `needs_register_class`
  (`crates/beamtalk-codegen/src/core_erlang/actor_codegen.rs:227-241`),
  so not literally *every* generated module, but every one that matters
  here, since those are exactly the modules whose load registers — and
  `register_class/0` calls `beamtalk_class_builder:register/1`, which
  needs `beamtalk_runtime`'s ETS tables and re-raises on failure via
  `raw_raise` so that the *load itself* fails
  (`class_registry.rs:97-98,338`). With the runtime not yet started, that
  is a `{load_failed, …}` boot halt — reproduced with a synthetic failing
  `on_load`. The stdlib has the identical shape: today `beamtalk_stdlib` is
  only `application:load`ed and its 115 modules are `ensure_loaded` lazily
  from a `beamtalk_runtime_sup` child (`beamtalk_stdlib.erl:129-150`,
  `beamtalk_runtime_sup.erl:91`); listing them in a `.rel` under embedded
  mode primLoads them first.

So the release boots in **interactive mode** and performs **explicit
activation**, exactly as run mode and the escript already do — the
difference is only *where the module list comes from*. Run mode scans
`_build/dev/ebin`; the escript embeds a list; the release reads each
shipped app's `{modules, …}` from its `.app` (the same list `systools`
already validated) and hands the `bt@*` subset to
`beamtalk_module_activation:activate_modules/2` — which takes a **module
list**, not a directory (`beamtalk_module_activation.erl:210`), and is
the function the escript calls (`escript.rs:289-297`). `activate_modules/2`
loads each module (which fires `register_class/0`, now with the runtime
up) in `topo_sort/1` order. `sort_modules_by_dependency/2` — which an
earlier draft named here — is the *directory-reading* front end to the
same sort and is the wrong entry point for a node that has no `_build/`.
"Nothing to discover" was the right instinct for the wrong reason: there is
no *scan*, because the manifest is authoritative; there is still an
activation step, and it is the same one every other mode runs.

**Who runs it — the actual ADR 0061 constraint.** Nothing in the tree
starts `beamtalk_workspace_sup` from an application callback.
`beamtalk_workspace_app:start/2` starts `beamtalk_workspace_app_sup`, whose
`init/1` has **no children** ("workspaces are added dynamically … currently
done by CLI directly", `beamtalk_workspace_app_sup.erl:13-16`); every
existing `beamtalk_workspace_sup:start_link/1` call is a CLI `-eval`
string (`repl_startup.rs:178`, `run.rs:509`, `escript.rs:293`). A
`start.boot` that starts `beamtalk_workspace` therefore brings up no
workspace supervisor, no bootstrap, no `beamtalk_actor_sup`, and a
`mode => release` that nobody reads. The mode variant is only real once
`beamtalk_workspace_app:start/2` **reads its config from the application
environment** — `mode`, `console`, `bind`, `auto_cleanup`, and the
`bt@*` activation list — and starts `beamtalk_workspace_sup` under
`beamtalk_workspace_app_sup` when `mode` is set. `sys.config` is where a
release sets that env; the CLI keeps passing a map for the other two modes
(it may migrate to the same env path later, but that is not this ADR's
scope). This is Phase 2's core, and it is **new code**, not a config
tweak.

**Ordering, then, is by application dependency and needs no new
mechanism.** The generated project `.app` declares
`{applications, [kernel, stdlib, beamtalk_runtime, beamtalk_workspace, …]}`
(ADR 0026 §3 — `app_file.rs` must emit `beamtalk_workspace` here, which it
does not today), so OTP starts `beamtalk_workspace` — and with it the
bootstrap, activation of every shipped `bt@*` module, and
`beamtalk_actor_sup` — **before** `beamtalk_<pkg>_app:start/2` runs. The
generated `_app` keeps doing exactly what it does now
(`'<Sup>':'start_link'()` + `beamtalk_supervisor:register_root/1`,
`outputs.rs:316-341`); an earlier draft had it performing activation
itself, which would have been a second copy of the workspace's job. The
invariant to test is unchanged — every `bt@*` class registered before the
root supervisor spawns its first actor — and the boot smoke test in Phase 2
asserts it.

#### 1.5 Capability in release mode follows from what is in the release

The release ships **no compiler port** (`include-compiler = false` by
default) and **no `.bt` sources**. Everything that compiles is therefore
structurally impossible, not policy-gated — **and that includes `eval`.**
An earlier draft listed `eval` as available; it is not. `eval` compiles the
expression through the ADR 0022 port
(`beamtalk_repl_eval.erl:192` → `beamtalk_repl_compiler:compile_expression/3`
→ `beamtalk_compiler:compile_expression/3`). The only compile-free way to
run code in the tree today is the **`run-entry`** op
(`beamtalk_ws_handler.erl:471-704`): a class, a selector, and arguments,
with no source string — which is exactly how ADR 0099's `beamtalk run
--connect` dispatches. A release without a compiler is therefore a node you
can *inspect* and *dispatch into*, but not *evaluate source on*:

| Operation | Release, default | Release, `include-compiler` |
|---|---|---|
| `run-entry` (`Class selector [args]`), `inspect`, `actors`, `actor-stats`, `pid-stats`, `sessions` | ✓ | ✓ |
| `eval` (source), `complete`, `describe` (where they compile) | ✗ `release_mode_no_compiler` | ✓ |
| `Class >> sel => body`, `compile:source:`, `load-source`, `Workspace load:` | ✗ `release_mode_no_compiler` | ✓ |
| `Behaviour >> reload`, ADR 0105 live re-check | ✗ `release_mode_no_compiler` (worker not started) | ✓ reload; re-check still ✗ (no source) |
| `Workspace flush`, `flush: confirmDestructive:`, `removeFromSystem`, rename (ADR 0113/0114) | ✗ `release_mode_no_workspace` | ✗ `release_mode_no_workspace` |

(`processes`, which an earlier draft listed, is not an op; the process
views are `sessions` and `actor-stats`.)

This is a stronger ADR 0058 story than the earlier draft's, not a weaker
one: a production console that cannot compile cannot be handed arbitrary
source, so the default release's attack surface past the cookie is "call
an exported selector on a registered class" rather than "run anything".
`include-compiler = true` is what turns it back into a full REPL. Both
refusals are `#beamtalk_error{}` values naming the mode and the
alternative, never a crash and never a silent no-op:

```
release_mode_no_compiler: Counter >> increment cannot be compiled.

  This node is running an OTP release, which ships no compiler and no
  source. Live method patching is a development-mode operation.

  To change this code: edit the source, `beamtalk release`, and deploy.
  To run a live-patchable image in production instead, rebuild with
  [release] include-compiler = true (see ADR 0125 §1.5).
```

`include-compiler = true` is the Pharo-style escape hatch for teams that
deliberately want a live image in production. It bundles the compiler port
binary and `beamtalk_compiler`, re-enables the compiler-dependent ops, and
logs a warning at boot that names three things the operator has opted
into: **a compiler is now reachable on a production node** (ADR 0058's
trust boundary means anything past the cookie can compile and run
arbitrary code — that was already true of `eval`, but a compiler widens
what "arbitrary" reaches); **live patches bypass the release artifact**
and are gone on the next redeploy, so the running node can silently
diverge from what `beamtalk-provenance.json` describes; and **a class
recompiled live loses its provenance stamp** (§1.8), so `__beamtalk_meta`
no longer says which toolchain produced it. It does **not** re-enable the
ADR 0082/0113 workspace ops: a release has no working tree to flush to.

This is the concrete answer to the acceptance criterion "how `Behaviour >>
reload` / ADR 0105 relate to (or are disabled in) release mode" — they are
disabled by *absence*, which needs no flag, cannot drift, and produces a
truthful error message.

#### 1.6 Remote REPL against a release — correcting ADR 0061

**ADR 0061's "TLS + auth required" is superseded by ADR 0058.** Beamtalk
implements no TLS of its own (mTLS was removed in PR #1401). The release's
console defaults are:

- **Off.** `[release] console = false`. A release that nobody configured a
  console for opens no *WebSocket* listener. It does open one port
  regardless: **Erlang distribution is always on and loopback-bound**
  (`-sname`, `inet_dist_use_interface {127,0,0,1}`, `epmd` as OTP starts
  it), because `stop`, `ping`, `rpc` and `remote_console` all ride on it
  and an operator's `ExecStop` must work without further configuration —
  the same default `mix release` takes by naming the node. `console`
  governs the REPL listener only; it is not a distribution toggle, and
  there is none. (Hardening to `-start_epmd false` with a fixed
  `inet_dist_listen_min/max` is an operator choice recorded under
  Consequences, not a default.)
- **Loopback.** When enabled, `bind = "127.0.0.1"`. Remote access is a
  reverse proxy (Caddy/nginx terminating TLS to `ws://127.0.0.1:<port>`) or
  an overlay network — ADR 0058's Layer 2, unchanged.
- **Cookie handshake mandatory**, as in every other mode (ADR 0020). The
  cookie is read from `RELEASE_COOKIE`/`vm.args`, never generated into the
  artifact — a cookie baked into a tarball is a shared secret in a registry.
  Boot fails with a structured error if `console = true` and no cookie is
  configured.
- **A non-loopback `bind` logs a warning at boot** naming ADR 0058 and the
  reverse-proxy alternative. It is not refused — an operator inside a
  private overlay has a legitimate reason — but it is never silent.

`bin/<name> remote_console` attaches over Erlang distribution **from the
same host only** — that is not a convenience default but ADR 0091's
co-location rule for Attach, and 0091 (Implemented) is the ADR that owns
remote access, not 0058/0020 alone: "Remote Attach **must** use TLS
distribution (`inet_tls_dist`) or a tunnel" (0091 § finding 1), because
plain distribution sends the cookie in an MD5 challenge over an
unencrypted transport and `epmd` is itself a network service. That is
exactly why the always-on distribution above is **loopback-bound**, as the
console listener is, and why anything past the host is 0091's
Phoenix-authenticated front or a tunnel — never a `-name` on a routable
interface. The one launcher verb that sidesteps all of this is §1.7's
`eval`, which starts a separate VM with *no* distribution and so needs
none of it. `beamtalk repl --host … --port …` attaches over
the WebSocket protocol, identically to dev. Both land on
`beamtalk_repl_server`, so the op vocabulary is the dev vocabulary minus
§1.5's refusals — no forked surface — with the honest caveat from §1.5 that
`remote_console` is an *Erlang* shell, and a Beamtalk `eval` in it needs
`include-compiler`.

#### 1.7 Lifecycle, entry points, and `eval`

The launcher (`bin/<name>` / `bin/<name>.cmd`) supports:

| Verb | Behaviour |
|---|---|
| `foreground` (default) | Boots in the foreground. Stdout is the container's stdout — ADR 0099's `Console` writes land there, which is what 12-factor logging wants. |
| `stop` | Graceful `init:stop()` in the running node, via distribution. |
| `ping` | Liveness check, via distribution. |
| `remote_console` | Attach a shell to the running node (§1.6). |
| `eval "Class selector [args]"` | Start a **separate** VM with the **runtime closure only** — `beamtalk_runtime`, `beamtalk_stdlib`, `beamtalk_workspace` in release mode with `console` forced off — so classes activate (§1.4), but **not the project's own application** (no root supervisor) and **no distribution**; dispatch one `run-entry`; halt with its exit code. |
| `rpc "Class selector [args]"` | Dispatch one `run-entry` **into the running node** over distribution and print the result. |
| `version` | Print the release version and provenance summary. |

`eval` and `rpc` take a class, a selector and arguments — the `run-entry`
shape — not a source string, because the default release has no compiler
(§1.5). `eval`'s rule is *modelled on* `mix release`'s (*"without starting
any of the applications in the release and without starting
distribution"*) but is **weaker, and says so**: Beamtalk cannot dispatch
onto a `bt@*` class that has not been activated, and §1.4 gives activation
exactly one path — `beamtalk_workspace_sup`, started by the
`beamtalk_workspace` application. So `eval` must start the runtime closure.
What it must *not* start is the project's own application, and it starts
no distribution — and that is enough to avoid the same-host collision,
because the runtime closure in release mode with `console` forced off
opens no listener and registers only per-node names: no second root
supervisor, no second console port, no `-sname` clash. A task that needs
the live system — a backfill against running actors — is `rpc`. A task that
needs only the code — a schema check, a one-off report — is `eval`.

Daemonisation is **not** provided. The supervisor is systemd, Docker, or
Kubernetes — every one of which prefers a foreground process. This matches
where the Elixir ecosystem landed and removes the `run_erl`/`to_erl` pid
plumbing that ADR 0027 would otherwise make us write twice. (Elixir's full
verb set is `start start_iex daemon daemon_iex install eval rpc remote
restart stop pid version`; `foreground` here is its `start`, and
`restart`/`pid`/`install` are dropped as things the process supervisor
does.)

**ADR 0099's two-tier exit, applied honestly.** ADR 0099 §3 defines
exactly two behaviours for `Program exit: N`, selected by the
`node_owning` application env
(`runtime/apps/beamtalk_stdlib/src/beamtalk_program.erl`): when the node
is owned by the program, `erlang:halt(N)` from *any* process; otherwise a
`throw({beamtalk_script_exit, N})`, which 0099 documents as crashing a
supervised actor that calls it — "the supervisor simply restarts it". There
is no third behaviour, and an earlier draft's `no_program_context` error
does not exist anywhere. So the launcher sets `node_owning` per verb and
the ADR says what follows:

- `eval` sets `node_owning = true`, as the escript does (`escript.rs:290`):
  `Program exit: N` halts that throwaway VM with `N`, and the launcher
  adopts it. This is the data-migration case, and it is exactly 0099's
  script semantics.
- `foreground` sets `node_owning = false`. `Program exit:` from an actor in
  a service is then a *throw in a supervised process*: that actor
  restarts, the node does not stop. A service that wants to stop the node
  says so with **`System halt: N`** — 0099's second tier, `erlang:halt/1`,
  no `terminate/2`, no OTP shutdown — or, for the graceful path an operator
  actually wants, is stopped from outside with `bin/<name> stop`
  (`init:stop/0`, which *does* run the supervision tree down). The
  distinction is worth an operator's attention: `System halt:` is for
  "this node is wrong, end it now"; `stop` is for "we are done".
- `rpc` runs inside the foreground node and inherits its `node_owning =
  false`; a `Program exit:` in an rpc'd entry crashes the dispatching
  session, not the node, and `rpc` reports it as a non-zero exit.

```bash
$ bin/orders rpc "Orders backfillPricing"      # against the live node
Backfilled 1,284 orders.
$ echo $?
0
```

#### 1.8 Provenance (ADR 0098)

Every module **`beamtalk build` produces** already carries
`beamtalk_version` and `otp_release` in `__beamtalk_meta` (ADR 0098 §3),
so a release is self-describing at runtime with no new mechanism. The
scope is exact, and worth being exact about: those keys are written only
when the CLI supplies them via `CodegenOptions::with_provenance`, and the
two call sites are both in `beam_compiler.rs` — the path `beamtalk build`
and the stdlib build (`build_stdlib.rs`, via `BeamCompiler`) share. The
REPL/compiler-port path supplies none, by design (`options.rs:85`:
"absent for REPL/tests"; readers treat absence as a stale module). So a
release's *shipped* modules — project, dependencies, and `bt@stdlib@*`
alike — are all stamped. The one way a module in a running release loses
its stamp is a class recompiled live under `include-compiler = true`
(§1.5), which is one more thing that flag's boot warning names. The
release adds one file that aggregates what the modules cannot say
collectively:

```jsonc
// releases/1.4.0/beamtalk-provenance.json
{
  "schema": 1,
  "release": "orders",
  "release_version": "1.4.0",
  "beamtalk_version": "0.4.0",          // BEAMTALK_VERSION, verbatim
  "otp_release": "28-16.0.2",           // build_stamp::current_otp_version()
  "required_otp": { "min": 28, "max": 30 },  // §3.2: build major .. +2
  "include_erts": true,
  "erts_version": "16.0.2",
  "platform": "linux/x86_64",
  "built_at": "2026-09-21T09:14:03Z",   // informational only
  "apps": [ { "name": "orders", "vsn": "1.4.0" }, … ]
}
```

`otp_release` is the compound string from
`build_stamp::current_otp_version()` — **the same function** ADR 0098's
stamp and ADR 0075's shared cache key use. Not re-derived from
`erlang:system_info(otp_release)`, which returns only the major.

`Beamtalk releaseInfo` answers the same map as a `Dictionary` from a live
node. It is an ordinary reflective send reachable through `eval` on every
surface, so it is **parity-neutral** — not a new per-surface operation.

---

### Part 2 — Upgrades

#### 2.1 v1 is restart-based; relup is deferred with its contract pinned

**Decision: `beamtalk release` v1 supports restart-based (blue/green)
upgrades only. Relup generation is a later phase. This ADR fixes the
contract now so that phase is mechanical rather than a redesign.**

The reasoning, stated honestly because the operator cohort will push back
(and their steelman is in §Steelman):

1. **A relup must cover the whole release, not just user classes.** It needs
   correct `.appup` files for `beamtalk_runtime`, `beamtalk_stdlib` (115
   `bt@stdlib@*` modules), `cowboy`, `ranch`, `telemetry` — every
   application in the closure. OTP's own applications ship *hand-written*
   `.appup` files for exactly this reason. That is Beamtalk's own release
   process (`docs/development/releasing.md`), recurring every version, and
   it is not work a user's `beamtalk release` can generate.
2. **Restart-based is what the ecosystem actually deploys.** `mix release`
   ships no relup by default; the ecosystem's relup tooling (distillery's
   generated appups) was a durable source of subtle bugs and its successor
   dropped the feature. Containers and Kubernetes made rolling restarts the
   normal path.
3. **Deferring costs us nothing we cannot recover**, *because ADR 0123
   already chose the relup-compatible design*: `code_change/3` reads the
   version from the state, not from `OldVsn`. A relup's
   `{update, Mod, {advanced, Extra}}` passes the same
   `Extra = #{module => Mod}` the live path passes today. There is **one**
   mechanism, and it already exists.

What v1 *does* ship, and what makes restart-based upgrades safe rather than
merely available, is §2.3's compatibility preflight.

#### 2.2 Appup derivation rules (pinned now, implemented later)

When the relup phase lands, `beamtalk release --upgrade-from <prev>` derives
each Beamtalk application's `.appup` by diffing `__beamtalk_meta` between
the two releases' beams. ADR 0050 established `__beamtalk_meta/0` as the
durable class-hierarchy record, and that is the *record* this reuses — but
**not** 0050's *reader*, and not `beam_lib` either, for a reason worth
stating so nobody reaches for the wrong tool:

- `__beamtalk_meta/0` is a **compiled function**, not a BEAM chunk. Its
  value exists only when it is *executed*. `beam_lib:chunks/2` can read
  `attributes` and `exports` from a `.beam` on disk (which is what
  `beamtalk_module_activation` and `beamtalk_native_docs` use it for) but
  cannot evaluate a function, so it cannot produce the meta map.
- ADR 0050's reader calls `Module:'__beamtalk_meta'()` on modules already
  loaded in a *live* node. A build machine diffing two release directories
  has no such node, and it cannot load both releases into one node either:
  both carry the same module names (`bt@orders@cart` in 1.3.0 and 1.4.0),
  and a BEAM node holds at most one current version of a name.

So the diff is done by a small **build-time shape extractor**: an
`erl -noshell` step that loads one release's staged `bt@*` beams into a
scratch node, calls `__beamtalk_meta/0` on each, and writes the result out
as a term file — then does the same for the other release, and diffs the
two files. This extractor is not relup-specific: **it is the same step
that produces `releases/<vsn>/shapes.json` in Phase 1** (§3.4), which is
likewise a projection of `__beamtalk_meta/0` over the release's classes and
likewise cannot be computed without executing it. Phase 1 builds the
extractor because `shapes.json` needs it; Phase 6's preflight and Phase 7's
appup generator reuse it. That is the shared leaf. Per `bt@*` module:

| Change between releases | Appup instruction |
|---|---|
| Nothing (identical beam) | *(omitted)* |
| Method bodies only; `shape_version` and flattened field set unchanged | `{load_module, Mod}` |
| `shape_version` bumped, **or** flattened field set changed | `{update, Mod, {advanced, #{module => Mod}}}` |
| Class added | `{add_module, Mod}` |
| Class removed | `{delete_module, Mod}` — warned at generation, **refused at install if instances are live** (see below) |
| Superclass changed, or an ancestor's field set changed | `{update, …}` for **every concrete descendant** |

The last row mirrors ADR 0123's "subclass instances are migrated on a
superclass reload": a flattened field set is what an instance's state map
actually holds, so an ancestor's change is a descendant's migration.
Instruction ordering is the class dependency topological order from
`beamtalk_module_activation:sort_modules_by_dependency/2` — the existing
function, not a re-derivation.

**Class removal refuses here, and that is deliberately *not* what ADR 0112
does.** `removeFromSystem`'s safety checks refuse on a stdlib module and on
a class with direct subclasses, but for live instances it **stops the
actors and proceeds** — it does not refuse. That is right in a workspace:
the author asked for the class to be gone, and the actors are theirs. It is
wrong in a production upgrade, where the same instruction would terminate
live actors — with their mailboxes and state — as a *side effect* of
deploying, with nothing in the deploy naming it.

The check therefore splits across the two moments, because only one of them
can see instances: **generation time** (on a build machine) knows a class
was removed but has no idea what is running on any target node, so it emits
a *warning* naming the class and marks the relup as carrying a destructive
removal; **install time** (on the node, where `release_handler` runs) is
the only place the instance count exists, so that is where the upgrade
refuses if instances are live. Draining or stopping them stays an explicit
operator step before the deploy, never something the upgrade does quietly.

This is the same split as §3.4's: one mechanism, two policies, because a
dev workspace and a production deploy have different things to lose.

**`Extra` is ADR 0123's `Extra`, verbatim.** The issue's requirement — *one
mechanism, not two* — is enforced by a conformance test, not a comment:
a test asserts that the `Extra` term the appup generator emits is accepted
by the same `beamtalk_hot_reload:code_change/3` clause the live reload path
exercises, and that both produce identical post-migration state for the
same class and starting map.

#### 2.3 Compatibility preflight (ships in v1)

`beamtalk release --upgrade-from <prev-release-or-dir>` runs in v1 as a
**checker** even though it generates no relup. It compares the two releases'
`shapes.json` (§3.4) and provenance and reports. That comparison needs
more than a version number per class — the report below names *fields*
added and *migration methods* present or missing — so `shapes.json`
carries the full per-class generation record the §2.2 extractor reads from
`__beamtalk_meta/0`: `shape_version`, the **flattened** field map with
declared types, and the `shape_migrations` table (see §3.4 for the
shape). A previous release built before `shapes.json` existed has no file
to compare; the preflight then runs the §2.2 extractor over that release's
`lib/*/ebin` to produce one on the fly, so the check never degrades to
"unknown" merely because the old artifact predates the feature:

```
Upgrade check: orders 1.3.0 → 1.4.0

  Shape changes requiring migration
    Cart          v2 → v3   migrateFromV2: present          ok
    Session       v1 → v2   migrateFromV1: MISSING          error

  Shape changed without a version bump
    Account       v1 → v1   field `tier` added              warning

  Removed classes
    LegacyQuote                                             warning

  Toolchain
    beamtalk 0.4.0 → 0.4.0                                  ok
    OTP      28-16.0.2 → 28-16.0.2                          ok

1 error, 2 warnings.
```

This is the same check ADR 0123 §4 specified for the workspace, applied
across two *releases* rather than two *edits* — but the reuse is narrower
than "route through the existing code", and it is worth being exact about
where the seam is. `beamtalk_shape_diff` (in `beamtalk_workspace`) diffs
two `shape()` records; it has no fingerprint function, and its *input* is
produced by `beamtalk_workspace_shape_store`'s ancestor walk over the live
`beamtalk_class_metadata` ETS (`beamtalk_workspace_shape_store.erl:77-106`).
A CLI preflight over two on-disk trees has no live ETS, so the flattening
has to be reproduced from `__beamtalk_meta/0` — which is precisely what
the §2.2 extractor does. The shared leaf is therefore: the extractor builds
the `shape()` record from beams, and `beamtalk_shape_diff` diffs it. That
makes the extractor a genuinely new leaf sitting *below* both the store
(live) and the preflight (disk), and Phase 6's estimate is sized for it.

For a restart-based deploy this check is the whole safety story: it is what
tells you, before you deploy, that persisted or in-flight v2 `Session`
state will hit an unmigrated v2→v3 gap.

#### 2.4 No downgrade hooks

**Decision: `migrateToVN:` stays reserved and undefined.** ADR 0123 left
this to BT-3528; the answer is no.

A relup's `{down, Vsn}` direction runs ADR 0123's **step 3 only** — the
structural reconcile against the older declared shape — and logs a warning
naming every field it dropped. Rolling back a release is a blue/green
rollback to the previous artifact, not a state downgrade.

This keeps `migrate/3`'s shipped permissive behaviour on the local/relup
path, and it is deliberately **not** what §3.4 does on the wire: a local
downgrade is one operator-initiated, recoverable event, whereas a rolling
deploy would apply the same truncation to every message from every
upgraded peer, invisibly. §3.4 item 3 is where that difference is named
and paid for.

The reason is that a correct downgrade hook is strictly harder to write than
its forward twin (it must invent information the forward step discarded),
it doubles the authoring burden on every shape change, and in practice it
is written once, never tested against real v(N+1) state, and wrong when it
finally runs. Naming the limitation is better than shipping a hook nobody
can validate.

#### 2.5 Failure and rollback semantics

For **restart-based** upgrades the semantics are the deployment platform's:
the old release keeps running until the new one is healthy. Beamtalk's
contribution is §2.3's preflight plus the §3.4 skew contract.

For the **relup** phase, the semantics are OTP's, and they are *better* than
the live-reload path in one specific way worth recording. ADR 0123's
"actor stays suspended on `code_change` failure" rule exists because the
live path loads the new module and only then suspends each pid, leaving a
window (ADR 0123 Current state ¶3) in which an actor runs new code over old
state. `release_handler` **suspends first**, and there is no interactive
compile in the window — which is exactly the closure ADR 0123 deferred here.
Under relup, a failing `code_change/3` aborts the whole
`release_handler:install_release/1` call; the release stays `unpacked`
rather than becoming `permanent`, and the node reboots into the previous
release. The per-instance "leave it suspended" rule is the *live-patch*
rule and does not apply.

---

### Part 3 — OTP version support policy

#### 3.1 The window: current major + previous major, minimum 27

**Supported = `max(27, current_major - 1) .. current_major`.** With OTP 28
current, that is 27–28. When 29 ships, the window becomes 28–29 and dropping
27 is a minor-version note, not a breaking change.

**Enforced from one declared source.** A new `otp-support.toml` at the repo
root is the single source of truth:

```toml
# Single source of truth for the OTP support window (ADR 0125 §3.1).
min-major = 27
max-major = 28
```

Three consumers read it, none of them copying it:

1. **`beamtalk doctor`** — replaces the hardcoded `27` in `check_erl`'s
   `major >= 27` guard and in the `check_erl` /
   `print_install_instructions` strings. `parse_otp_major` is already
   version-agnostic and is left alone.
2. **`beamtalk build` / `beamtalk release`** — a build-time check; building
   on an out-of-window OTP is a warning for `build` (you may be developing
   ahead) and an **error** for `release` (you are producing an artifact
   whose validity you cannot state).
3. **The CI matrix** — a `just` recipe prints the window as a JSON array,
   consumed by the workflow's `matrix.otp`. CI today runs one pinned
   version; closing that is the teeth this policy currently lacks.

A test asserts the generated CI matrix equals the declared window, so the
"three consumers" cannot drift — the invariant is enforced, not commented
(`CLAUDE.md`: never a "keep in sync" comment without a test).

#### 3.2 BEAM files, ERTS, and what a release is valid on

OTP's documented guarantee (System Principles § Compatibility): *"Compiled
code can be loaded on at least two subsequent releases … Loading on
previous releases is not supported."* So the direction is one-way, and the
window is not "maybe the next one" — it is **guaranteed for the two
majors after the build major**. A release built on OTP 28 runs on 28, 29
and 30, and does not run on 27. An earlier draft said "may run on 29" and
recorded `required_otp` as `{min: 28, max: 28}`; both understated a
documented promise and would have refused hosts OTP itself supports. The
runtime rule is therefore `host_major ∈ [build_major, build_major + 2]`
— **and only that.** It is deliberately *not* intersected with §3.1's
support window, because the two answer different questions and are
enforced at different moments. The window is a *policy* about which OTP
majors Beamtalk builds and tests on; §3.1 item 2 enforces it at **build
time**, where the operator can act on it. The boot check is a *fact* about
what this artifact's bytecode will load on; applying the policy there would
refuse a host the artifact demonstrably runs on, for a reason nobody at
boot can do anything about. So when the window later moves to 28–29, a
release built on 28 still boots on 30: the window governs what we build
*on*, the BEAM rule governs what we boot *on*. (A first revision of this
section said "intersected", which would have made `required_otp`
`{28, 28}` again by the back door.)

**Therefore `include-erts = true` is the default.** The release bundles the
ERTS it was built against, the host needs no Erlang at all, and the
compatibility question disappears: the artifact is exact.

`--no-include-erts` produces a slim artifact for images layered on an
`erlang:NN` base. It is fully supported, and it is where the policy gets
enforced at runtime: the provenance's `required_otp` records the build
major, and the launcher **refuses to boot** on a host outside the window:

```
orders 1.4.0 cannot start on Erlang/OTP 27.

  This release's BEAM files were produced by OTP 28 and will not load on
  an older VM.

  Required: Erlang/OTP 28, 29 or 30 (built on 28)
  Found:    Erlang/OTP 27 (/usr/lib/erlang)

  Either install OTP 28 or newer, or rebuild the release on OTP 27.
```

Fail-closed at boot, with both versions named, beats a `badfile` crash
several layers deep in `code_server`.

#### 3.3 Interaction with the type-spec cache and dialyzer

**ADR 0075 / BT-2470's shared cache needs no change.** It is already keyed
by `<otp_release>-<erts_version>`, so an OTP upgrade lands under a new key
and never reuses stale entries. Both it and ADR 0098's provenance stamp
derive that string from `build_stamp::current_otp_version()`; §1.8's
`otp_release` is a third consumer of the same function.

**`strip-beams` has one named cost.** ADR 0075's auto-extraction reads
`abstract_code` from `.beam` files, and its Consequences already record
that "release-stripped packages … fall through to `Dynamic`". Stripping a
*deployed* release is harmless — nothing runs the type extractor against it
— but it makes `include-compiler = true` a much worse experience (FFI
completions and diagnostics degrade to `Dynamic` for the release's own
modules). `strip-beams = true` therefore refuses to combine with
`include-compiler = true`, with an error saying why. Appup derivation
(§2.2) is unaffected: it reads `__beamtalk_meta`, which is a generated
function, not a debug chunk.

**ADR 0068's dialyzer specs are OTP-independent; the FFI specs they
cross-check against are not.** A dialyzer run validating a release must use
the OTP major the release targets. The §3.1 CI matrix delivers this: `just
dialyzer` runs on every supported major, so a spec that is valid on 28 and
broken on 27 is caught before a release claims both.

#### 3.4 Version skew — the contract shared with BT-3527

A cluster is a set of releases, possibly at different versions. This ADR
fixes the contract; BT-3527 decides the messaging policy built on it.

1. **The wire shape is ADR 0123's envelope, verbatim:**
   `{beamtalk_shape, Class, ShapeVersion, Fields}`, and `pack/1` is reused
   unchanged. The *chain* is shared too — one `beamtalk_shape_chain`, one
   reconcile, for persistence and the wire alike.
2. **Receivers migrate forward, never backward.** `unpack/1` already runs
   `migrate/3` from the envelope's version, so a node at a *newer* shape
   accepts an older peer's term transparently. This direction needs nothing
   new.
3. **A receiver that is behind refuses — and this is new code, not a reuse.**
   Today's `unpack/1` is *permissive* in the backward direction, and
   deliberately so: `migrate/3`'s `ToVersion < FromVersion` branch logs a
   downgrade warning (`beamtalk_shape_migration:maybe_log_downgrade/3`),
   runs step 3's reconcile against the receiver's **older** declared field
   list, drops every key that list does not declare, and returns
   `{ok, Kept, ToVersion}`. That is the right policy for **persistence** —
   an operator-initiated, single-node, recoverable rollback where the old
   code genuinely cannot use the new fields — and it is the wrong policy
   for a **wire**, where a rolling deploy makes every message from every
   already-upgraded peer a silent truncation.

   So the skew contract adds a public entry point rather than changing
   `unpack/1`'s contract — though "thin" would oversell it: both `pack/1`
   and `unpack/1` are wrappers over *private* arity-2 recursions
   (`beamtalk_shape_migration.erl:507-517, 684-690`), and threading a
   policy through the nested-`Value` walk means those private functions
   grow a parameter. Small, but a refactor of existing code, not an
   addition beside it:

   ```erlang
   %% Not `unpack/2` — that arity is taken by the existing private
   %% nesting-depth recursion (?MAX_PACK_DEPTH).
   -spec unpack_strict(envelope()) ->
       {ok, Instance :: map()} | {error, #beamtalk_error{}}.
   ```

   `unpack_strict/1` refuses an envelope whose `ShapeVersion` exceeds the
   receiver's declared `shapeVersion` for that class, short-circuiting
   **before the chain runs** and returning
   `#beamtalk_error{kind = shape_version_ahead}` naming the class and both
   versions. It does not guess, and it does not drop fields. Everything
   else delegates to the same `migrate/3`. `unpack/1` is untouched, so
   every existing caller keeps today's behaviour. This is the ordering
   discipline OTP's own upgrade guidance uses: upgrade receivers before
   senders.

   **The check must be per envelope, not per message.** `pack/1` packs
   nested `Value` instances recursively so each carries its own version
   (ADR 0123 § Envelope), and `unpack_nested_value/2` calls `migrate/3`
   for each one. A strict unpack therefore threads its policy down that
   recursion: a `Cart` at the receiver's own version carrying a `Money`
   field one version ahead is still skew, and refusing only at the top
   level would truncate the nested `Value` silently — the same bug one
   level down.

   **Ownership:** this ADR *defines* `unpack_strict/1`; **BT-3527
   implements it**, because BT-3527 is what first puts an envelope on a
   wire. Nothing in this ADR's v1 sends or receives one, so it appears in
   no phase here — the same anti-rot reason §2.2 pins the appup rules in
   prose instead of shipping an unexercised generator.
4. **Skew is detectable at connect time, not per message.** Each release
   writes `releases/<vsn>/shapes.json` — `class → shapeVersion` for every
   class in the release, derived from `__beamtalk_meta` at assembly time:

   ```jsonc
   { "schema": 1, "release_version": "1.4.0",
     "shapes": {
       "Cart": { "version": 3,
                 "fields": { "items": "List", "total": "Integer" },   // flattened
                 "migrations": { "1": "migrateFromV1:", "2": "migrateFromV2:" } },
       "Session": { "version": 2, "fields": { "user": "String", "startedAt": null }, "migrations": {} },
       "Account": { "version": 1, "fields": { "balance": "Integer" }, "migrations": {} } } }
   ```

   The connect-time comparison BT-3527 makes uses only each entry's
   `version`; the rest is what §2.3's preflight needs and is the same
   record either way, so one file serves both.

   The same map is answerable from a live node (`Beamtalk shapeManifest`,
   parity-neutral like §1.8's `releaseInfo`), so two nodes — or a deploy
   tool and a running node — can compare without the file. BT-3527 uses
   this to negotiate per class at connect time rather than discovering skew
   on message N.

The deployment rule that follows, and which belongs in the operator docs:
**one shape-version bump per deploy**, and upgrade the receiving side of a
class's instances first. §2.3's preflight is what makes that rule checkable
before the deploy rather than discoverable during it.

---

### Surface parity

`beamtalk release` is **CLI-only and `surface-specific`**: it reads an
on-disk project and writes a build artifact, exactly as
`beamtalk build --escript` does. There is no live-image analogue — a
workspace cannot produce a release of itself.

`bin/<name> eval` and `bin/<name> rpc` are both `run-entry`-shaped
(`Class selector [args]`, no source) — `rpc` is literally the `run-entry`
op that `beamtalk run --connect` already dispatches, reached over
distribution instead of the WebSocket protocol — and are likewise
`surface-specific`.

`Beamtalk releaseInfo` and `Beamtalk shapeManifest` are **not**
surface-specific: they are reflective sends reached through `eval` on every
surface, answering the same `Dictionary` everywhere, like `Session info`
before them.

`docs/development/surface-parity.md` gains rows for all four.

### Amendment to ADR 0061

ADR 0061 is **Implemented**, and its § "Future: Release Mode" is the one
forward-looking part of it that this ADR closes. On acceptance, two things
in that section become stale and should be edited rather than left to
mislead a reader who finds 0061 first:

1. **The open design constraint is resolved.** 0061 asked for "either a
   third config variant or … a standalone OTP application". §1.4 chooses
   the third variant and records the extraction as a follow-up. 0061's
   constraint paragraph should point at §1.4 instead of posing the question.
2. **"TLS + auth required" is wrong and was wrong when written.** It
   predates ADR 0058's record that mTLS was removed (PR #1401). §1.6
   replaces it with 0058's actual stance: console off by default, loopback
   when on, cookie mandatory, TLS terminated by a reverse proxy or overlay.
   This is a **correction to 0061, not a new policy** — no security posture
   changes here; 0061 simply describes one Beamtalk no longer has.

0061's three-mode table (`run` / full workspace / release) is the direct
ancestor of §1.4's table, and its *structure* holds — but its release
row's "Bootstrap: ✗ (pre-loaded)" is the third stale item, and the most
consequential: §1.4 shows the boot script cannot preload `bt@*` modules in
either VM mode (interactive skips them; embedded runs `-on_load` before the
runtime exists and halts the boot). Release mode bootstraps explicitly,
like every other mode, from the `.app` module lists. §1.4's table also adds
the `beamtalk_idle_monitor`, ChangeLog and `telemetry_poller` rows 0061 did
not have to consider.

---

## Prior Art

### Erlang/OTP — `systools`, `release_handler`, `.appup`/`.relup`

The canonical mechanism, and the one this ADR builds directly on.
`systools:make_script/2` turns a `.rel` into a boot script;
`make_tar/2` packages it; `make_relup/4` computes an upgrade script
from two releases plus their `.appup` files; `release_handler` installs one
at runtime with automatic fallback to the previous release. **Adopted
wholesale.** What OTP does *not* provide is appup *generation* — every OTP
application hand-writes its own — which is the single strongest argument
for §2.1's deferral.

### Elixir — `mix release` (and Distillery before it)

`mix release` produces a self-contained tarball with `include_erts` on by
default, `sys.config`/`vm.args`, a `bin/<app>` script with
`start`/`daemon`/`remote`/`eval`/`rpc`, and **no hot upgrade support at
all**. Distillery, its predecessor, did generate appups and relups, and the
feature was a persistent source of subtle breakage; when releases moved into
Elixir core, it was dropped rather than reimplemented. **Adopted:**
`include_erts` default-on, `eval`'s no-apps/no-distribution semantics and
`rpc` for the live node, a subset of the verb set (§1.7 lists what is kept
and dropped), config at `releases/<vsn>/`. **Learned from:**
the relup decision, which is §2.1's strongest external evidence.

### rebar3 / relx

The Erlang-side equivalent, and the obvious thing to shell out to.
`relx` adds overlays, config providers, and the boot scripts on top of
`systools`. **Rejected as a dependency** (§1.3, and §Alternatives): it would
add `rebar3` to the user prerequisite list for the single most
operator-facing command, and the part it adds beyond `systools` is precisely
the part that has to be Beamtalk-specific anyway (ADR 0099 lifecycle,
ADR 0027 Windows). Its *design* is adopted; its *code* is not.

### Gleam

Gleam compiles to Erlang and delegates packaging entirely — `gleam export
erlang-shipment` produces a directory plus an `entrypoint.sh`, and anything
more is the user's job. **Deliberately not followed** — and not for the
reason an earlier draft gave. Gleam needs no `rebar3` (it builds with
`erlc` and `escript` alone); its shipment is simply a *directory with a
start script*, and Gleam's own docs say shipments are to be replaced by OTP
releases. That is the point: a directory is where Beamtalk's escript
already is, and the operator cohort (ADR 0099's steelman) asked for the
thing Gleam has not built yet — `sys.config`, a boot script, a console, an
upgrade story.

### Pharo / Squeak Smalltalk

Deployment is *the image itself* — save and ship the running world,
including the compiler. There is no separate build artifact and no upgrade
mechanism beyond "load a changeset into the live image". **Partly adopted**
as `[release] include-compiler = true` (§1.5): a live-patchable production
image is a legitimate Smalltalk deployment model, and refusing it outright
would be a gratuitous departure. Made **opt-in** because the BEAM's own
model — immutable artifact, supervised restart — is the default a BEAM
operator expects, and because a compiler on a production node is a real
attack-surface decision an operator should make consciously.

### Go / Rust — the static binary

One file, no runtime prerequisite, trivially containerised. This is what
`include-erts = true` is imitating, and it is why it is the default: an
operator should be able to `COPY` the artifact into `FROM debian:slim` and
have it run. The BEAM cannot go all the way to a single file (the ERTS tree
and `lib/` are directories), but the *property* — no host runtime — is
reachable and worth having.

### Kubernetes / container orchestration

The reason restart-based upgrades are sufficient for v1. Rolling updates,
readiness probes, and blue/green are the platform's job and are better
tested than any relup. The BEAM's advantage is not that it must avoid
restarts, but that a restart is cheap and supervised.

---

## User Impact

### Newcomer (from Python/JS/Ruby)

`beamtalk release` is the verb they already know — `npm run build`,
`docker build`, `cargo build --release`. One command, one artifact, copy it
somewhere and run `bin/<name>`. The defaults matter most here:
`include-erts = true` means they never learn what ERTS is, and
`console = false` means they cannot accidentally expose an eval endpoint.
The one concept they must meet is `[application] supervisor` — and §1.1's
error teaches it at the moment it is needed, with `--escript` offered as
the alternative for a script.

### Smalltalk developer

The honest news: a release is **not** an image. The live-patching reflex
(`Counter >> increment => …`) stops working, and §1.5's error says so in
those words rather than failing obscurely. `include-compiler = true` is
there for those who genuinely want the Pharo model, and the ADR does not
sneer at it — but it is opt-in, and the ADR is explicit that the default is
the BEAM's model, not Smalltalk's. `shapeVersion:`/`migrateFromVN:` (ADR
0123) is the concept that carries over: a class's evolution is declared in
the class, and §2.3's preflight is the tool that tells you before you deploy
what the live image would have told you at reload.

### Erlang/Elixir developer

Everything is where they expect it: `lib/<app>-<vsn>/ebin`,
`releases/<vsn>/`, `sys.config`, `vm.args`, `start.boot`, `RELEASES`,
`bin/<name> remote_console`. The release is a plain OTP release — `observer`,
`recon`, `dbg`, `etop` and `sys:get_state/1` all work on it with no Beamtalk
knowledge. They can put a Beamtalk application in an *Erlang* release too:
the `.app` files are real (ADR 0026 §3) and the beams are ordinary
`gen_server` modules. The one departure they will notice is §2.1 — no relup
in v1 — and §2.2 tells them exactly what the appup will look like when it
lands, and §2.4 tells them downgrade hooks are not coming.

### Production operator

This is the cohort the ADR is for, and the ask from ADR 0099's steelman is
answered. What they get: a versioned tarball, no host runtime, foreground
process for systemd/Docker, structured stdout, a console that is off by
default and loopback when on, a provenance file naming the exact toolchain,
a preflight that fails the deploy on an unmigrated shape change, and a
boot-time refusal on an OTP mismatch rather than a `badfile` crash.

What they do not get in v1 is zero-downtime in-place upgrade. §Steelman
takes that objection seriously; the mitigation is that restart-based
upgrades are *safe* (§2.3) rather than merely available, and that the relup
contract is pinned so the capability is a later phase, not a redesign.

### Tooling developer

`shapes.json` and `beamtalk-provenance.json` are stable, schema-versioned,
machine-readable inputs — a deploy tool can diff two releases without
booting either. `Beamtalk releaseInfo`/`shapeManifest` give the same data
from a live node. The `mode => run | workspace | release` triple is a
clearer thing to reason about than today's boolean, and §1.5's capability
table is the classification an LSP or IDE needs to grey out operations
against a release-mode node.

The cost lands here too, and it is not small. **Today every node a tool
connects to is a workspace, so capability can be assumed; after this ADR it
must be queried.** Every surface that offers a mutation — LSP code actions,
the MCP `save_method`/`try_method` tools, the LiveView IDE's save buttons —
needs a mode check and a new error path for §1.5's structured refusals,
and a tool that skips it degrades from "button is greyed out" to "button
throws". There is also a genuine two-sources-of-truth hazard: `shapes.json`
on disk describes the *artifact*, `Beamtalk shapeManifest` describes the
*running node*, and a node running last week's release disagrees with the
directory sitting next to it. Tools must say which one they are reporting;
this ADR deliberately keeps both rather than picking, because the deploy
tool needs the file (no node yet) and the operator needs the node.

---

## Steelman Analysis

### "Ship relup in v1" (the strongest objection)

- 🏭 **Operator**: "Hot upgrade is *the* reason to be on the BEAM. Telecom
  ran nine-nines on `release_handler`. If Beamtalk ships a release command
  that can only be restarted, it has shipped Go with extra steps — and the
  one thing that would have made an operator choose it over Go is exactly
  what was cut."
- ⚙️ **BEAM veteran**: "ADR 0123 *already* built the hard part. `Extra` is
  `#{module => Mod}`, `code_change/3` reads the version from the state, the
  chain is pure and tested. The appup is a table lookup over a
  `__beamtalk_meta` diff. You are deferring the easy 20% after paying for
  the hard 80%."
- 🎩 **Smalltalk purist**: "Restart-based deployment is the opposite of a
  live image. The whole tradition is that the system evolves without
  stopping. Blue/green is what languages without live update do because
  they must."
- 🎨 **Language designer**: "`shapeVersion:` is *defined* by its relationship
  to upgrade. Shipping the declaration without the upgrade that consumes it
  leaves the feature half-motivated — users declare versions for a mechanism
  they cannot run."

**Why it did not win, and what it did win.** The veteran's argument is
correct about *user classes* and incorrect about *the release*. A relup
covers every application in the closure, and `beamtalk_runtime` /
`beamtalk_stdlib` / `cowboy` need hand-written `.appup` files per Beamtalk
version — recurring work in Beamtalk's own release process that no
generator produces. Shipping `--upgrade-from` that silently emits
`load_module` for the runtime's own gen_servers would be worse than not
shipping it: it would look like hot upgrade and corrupt runtime state.
The objection did win two concrete things: §2.2 pins the derivation rules
and the conformance test *now*, so the deferral cannot become a redesign;
and §2.3's preflight ships in v1, so the declaration is not
half-motivated — `shapeVersion:` earns its keep on restart-based deploys by
catching the missing migration before the deploy.

### "Use relx via rebar3 rather than driving `systools` yourself"

- 🧑‍💻 **Newcomer**: "Battle-tested tooling beats a hand-rolled assembler.
  I would rather install one extra thing than debug your boot script."
- ⚙️ **BEAM veteran**: "relx has handled ERTS relocation, `RELEASE_*` env
  vars, remote-console node naming, config providers and Windows `.cmd` for
  a decade. You will get at least three of those subtly wrong."
- 🏭 **Operator**: "If it is a relx release, everything I know transfers —
  including relups, which you would then get for free."

**Why it did not win.** The prerequisite cost is the decisive argument: the
README promises `erl` and `erlc`, and `beamtalk release` is the command an
operator runs on a build machine they may not control. Adding `rebar3`
there is a real adoption cost for the most operator-facing command. Beyond
that, the parts relx adds over `systools` are the parts that must be
Beamtalk-specific anyway — the launcher has to carry ADR 0099's
`Console`/two-tier-exit semantics and ADR 0027's tier-1 Windows story, so
we write it either way. The veteran's list of sharp edges is real and is
the strongest reason this ADR keeps the launcher **small** (§1.7: no
daemonisation, no `run_erl`/`to_erl`) — every verb we do not ship is an
edge we do not get wrong. Vendoring relx remains available if the launcher
proves to be a recurring bug source; §Alternatives records it.

### "Extract the REPL server into its own application" (ADR 0061's other option)

- 🎨 **Language designer**: "An OTP application boundary is a *real*
  boundary, enforced by the build. A mode flag is a convention that the next
  child spec quietly violates."
- ⚙️ **BEAM veteran**: "`beamtalk_workspace` in a production release is
  wrong on its face. I can read a `.rel` file and see what is running; I
  cannot read a config map."
- 🏭 **Operator**: "The attack surface is what is *loaded*, not what is
  *started*. A flag off today is a flag on after a bad config push."

**Why it did not win — and what it changes.** The operator's point is the
sharpest and is partly conceded: §1.5's strongest guarantees are stated as
*absence* (no compiler in the release, no sources) precisely because a flag
is weaker than an omission. But the extraction does not cut where ADR 0061
assumed: the op modules reach five `beamtalk_workspace_*` stores, so moving
`beamtalk_repl_server` + `beamtalk_session_sup` alone would either drag
every store along — the exact outcome the veteran objects to — or fork the
op vocabulary, which `surface-parity.md` exists to prevent. The mode variant
is the smaller change that makes the extraction *possible*: §1.5's
capability classification is the seam the application boundary needs. The
extraction is the recorded follow-up, and this steelman is why it is
recorded rather than dismissed.

### "Default `console = true` — an operator without a console is blind"

- 🏭 **Operator**: "The first thing I do on a production incident is attach.
  Shipping it off-by-default means the one time I need it, I need a redeploy
  to get it."
- 🎩 **Smalltalk purist**: "A live system you cannot talk to is not a live
  system. The REPL is not a debug feature; it is the interface."

**Why it did not win.** The console is an authenticated eval endpoint —
ADR 0058 is explicit that anything past the cookie executes with the full
privileges of the OS process. Defaulting it on would mean every Beamtalk
release opens one unless the author knew to say otherwise, and the failure
mode (a release deployed with a cookie baked into a public image) is
unrecoverable. Off-by-default is one line of config for the operator who
wants it, and `bin/<name> remote_console` still works from the host with no
listener at all — which covers most of the incident case the steelman
raises.

### Tension points

Three places where reasonable people will still disagree after reading this:

1. **Relup in v1 (§2.1).** The BEAM-veteran and operator cohorts want it;
   the language-designer and maintainer view is that a partially-correct
   relup is worse than none. Resolved by pinning §2.2's contract and
   shipping §2.3's preflight — but the disagreement is legitimate and the
   phase boundary is where it lives.
2. **Mode variant vs. application extraction (§1.4).** BEAM veterans want a
   `.rel` file they can read; this ADR gives them a config map for one more
   release cycle. Resolved as a sequencing decision, not a permanent one.
3. **`include-compiler` (§1.5).** Smalltalkers will read the default as the
   ADR choosing BEAM orthodoxy over Smalltalk's live image; operators will
   read the flag's existence as a footgun. Both readings are fair. It is
   opt-in, loud at boot, and incompatible with `strip-beams`.

---

## Alternatives Considered

### Escript-only (do nothing new)

Keep `beamtalk build --escript` as the sole packaging story and tell
operators to wrap it. **Rejected:** an escript has no supervision tree, no
`sys.config`/`vm.args`, no console, no version-aware upgrade story, and no
way to state which OTP it is valid on. It is the right artifact for a
script and ADR 0099 was right to ship it first; it is not a service
artifact. It remains supported and is what §1.1's error points at.

### Docker-only ("the container is the release")

Ship a `Dockerfile` template and declare the image the unit of deployment.
**Rejected as the *primary* answer** — it is not an alternative to a
release, it is a consumer of one. Something still has to produce the boot
script, the config, and the app closure inside the image, and doing that
with `erl -pa` incantations in a `CMD` line reproduces every problem
`systools` already solved. A release **enables** the container story:
`FROM debian:slim` + `COPY` + `CMD ["bin/orders", "foreground"]` is three
lines because `include-erts = true` removed the runtime from the image's
concerns.

### relx via rebar3

See §Steelman. Rejected on the user-prerequisite cost and on the
observation that the value relx adds over `systools` is the
Beamtalk-specific part we must write regardless. Recorded as the fallback
if the hand-written launcher proves a recurring source of platform bugs —
in which case relx would be *vendored* alongside the compiler port binary,
not added to the user's install list.

### `mix release` parity (build on Elixir's tooling)

Generate a `mix.exs` and delegate. **Rejected:** it makes Elixir a
prerequisite for deploying a Beamtalk program, which is a strictly larger
ask than `rebar3`, and it inverts the dependency — Beamtalk would be a
guest in another language's build system. Elixir's *design* is adopted
freely (§Prior Art); its toolchain is not.

### Vendor `relx` into the Beamtalk install

Ship relx's escript beside the compiler port binary, as ADR 0022 does for
the compiler. **Deferred rather than rejected**, and the most credible
alternative to §1.3. It would give relup generation and the launcher for
free. The cost is a vendored third-party build tool whose version we must
track, whose bugs we inherit, and whose output we would still post-process
for ADR 0099/0027 semantics. Revisit if §1.7's launcher grows past ~200
lines per platform or if the relup phase proves harder than §2.2 predicts.

### Generate appups in v1 but ship no `relup` command

Emit `.appup` files into the release for a `systools:make_relup/4` the user
runs themselves. **Rejected:** unexercised generated output rots. An appup
nobody runs is an appup nobody tests, and the first person to run it would
be an operator mid-upgrade. §2.2 pins the rules in prose instead, which
costs nothing and cannot silently break.

### A `version` key in `[release]`

Let a release be versioned independently of the package. **Rejected:** two
version numbers for one artifact is exactly the derived-version-string
problem `CLAUDE.md` and `docs/development/releasing.md` already forbid for
this repo. `[package] version` is the version; the `.app` file already uses
it.

### `include-erts = false` by default

Smaller artifacts; assume a host Erlang. **Rejected as the default:** it
makes every deployment's correctness depend on a host version the artifact
cannot control, and it converts a build-time guarantee into a runtime
failure. It stays fully supported (§3.2) with a boot-time version check,
because layering on an `erlang:NN` base image is a legitimate and common
choice.

### Support window of "current major only"

Simpler policy, one CI job. **Rejected:** it forces every Beamtalk user to
upgrade OTP within weeks of an OTP release, which no production operator
will do. Two majors is the shortest window that lets a user upgrade
Beamtalk and OTP independently.

### Downgrade hooks (`migrateToVN:`)

See §2.4. Rejected: strictly harder to write than the forward step, doubles
the authoring burden on every shape change, and is almost never exercised
before the moment it must be correct.

---

## Consequences

### Positive

- Beamtalk has a production deployment story: one command, one artifact, no
  host runtime, supervised foreground process, structured stdout.
- ADR 0061's open constraint is resolved — and resolved at the level it
  actually lived: a named mode triple *plus* an application-environment
  start path for `beamtalk_workspace_sup`, which nothing in the tree has
  today. The `beamtalk_idle_monitor` halt that a naïve `repl = true`
  release would ship is switched off *by the mode* rather than by an
  operator remembering `auto_cleanup = false`.
- The prerequisite list is unchanged: `erl` and `erlc`, as the README
  already promises.
- Much of the machinery already exists — `.app` generation (ADR 0026 §3),
  the app-callback generator, the dependency closure (ADR 0070),
  `activate_modules/2` and its topological sort, the provenance stamp (ADR
  0098), the escript's staging logic. The genuinely new pieces are named,
  not hidden: tree assembly and the launcher, the app-env start path for
  `beamtalk_workspace_sup` (§1.4), and the build-time shape extractor
  (§2.2). Review moved two of those from "already exists" to "new" — a
  correction that cost estimates, not the design.
- `shapeVersion:` earns its keep on day one: §2.3's preflight turns a
  restart-based deploy from "hope the state matches" into a checked
  operation, using the same fingerprint code ADR 0123 §4 already specified.
- ADR 0123's two deferrals are answered — no downgrade hooks (§2.4), and
  the load→suspend window closes under `release_handler`, which suspends
  first (§2.5).
- The OTP support policy gets teeth for the first time: a declared window, a
  CI matrix generated from it, a test that the two agree, and a boot-time
  refusal on mismatch.
- BT-3527 gets a concrete contract to build on rather than a shape to
  invent: ADR 0123's envelope, forward-only migration, a named
  `unpack_strict/1` for the backward direction, and a connect-time shape
  manifest.

### Negative

- **No zero-downtime in-place upgrade in v1.** The headline BEAM capability
  is deferred. Mitigated by §2.2's pinned contract and §2.3's preflight, but
  it is a real gap and the operator steelman is right that it is the gap
  most likely to cost an adoption.
- **`beamtalk_workspace` is still in the release closure** when
  `console = true`, carrying modules (flush, git, changelog, the ADR 0105
  stores) that are never started there. A veteran reading the `.rel` will
  see development code in a production release. The mode flag makes it
  inert, not absent.
- **The launcher is hand-written and platform-specific.** Two scripts
  (`sh` + `.cmd`) that have to agree, plus ERTS relocation. This is the
  category of code that is tedious to get right and easy to get subtly
  wrong on the platform CI does not run on.
- **Artifact size.** `include-erts = true` plus the full `bt@stdlib@*` set
  puts a hello-world release in the tens of megabytes. `strip-beams` helps
  and costs ADR 0075 extraction (§3.3).
- **A third mode is a third thing to test.** Every workspace-sup change now
  has three configurations, and the release one is the least exercised.
  A release-mode boot smoke test is a required part of Phase 1, not a
  follow-up.
- **`otp-support.toml` is a new root-level file** — small, but one more
  place a contributor must know about. The test asserting the CI matrix
  matches it is what makes it worth having.

### Neutral

- Existing `beamtalk build --escript` behaviour is unchanged; releases are
  additive and the two artifacts serve different jobs.
- Every release runs loopback-bound Erlang distribution and, by OTP
  default, an `epmd` (§1.6). An operator who wants no `epmd` at all can
  set `-start_epmd false` with a fixed `inet_dist_listen_min`/`_max` in
  `vm.args` and point `stop`/`ping`/`rpc` at the fixed port; this ADR
  records that as a supported hardening, not a default, because the default
  has to make `bin/<name> stop` work with zero configuration.
- No language surface changes. `shapeVersion:`/`migrateFromVN:` (ADR 0123)
  are consumed as-is; `Beamtalk releaseInfo`/`shapeManifest` are reflective
  sends, parity-neutral by construction.
- `Program exit:` / `System halt:` / `Console` keep ADR 0099's semantics;
  §1.7 only states which contexts exist in a release.
- The `repl => boolean()` → `mode => …` change is internal (Erlang config
  map, a handful of call sites in the CLI, escript bootstrap, and tests);
  no user-visible configuration moves.

---

## Implementation

### Affected components

| Layer | Work |
|---|---|
| CLI (`beamtalk-cli`) | `commands/release.rs` (new); `manifest.rs` `[release]` section; `build_layout.rs` `release_dir()`; `doctor.rs` window check; `main.rs` `Release` subcommand |
| Build | Release tree staging (`lib/<app>-<vsn>/ebin`), `.rel` writer, `systools` invocation via `ErlcInvocation`-style `erl -noshell` helper with `{variables, [{"RELEASE_DIR", …}]}`, `release_handler:create_RELEASES/4`, tarball, ERTS copy |
| Launcher | `bin/<name>` (sh) + `bin/<name>.cmd`; verbs `foreground`/`stop`/`ping`/`remote_console`/`eval`/`version`; OTP-major boot check |
| Runtime (`beamtalk_workspace`) | `mode => run \| workspace \| release` on `beamtalk_workspace_sup`; **app-env-driven start of `beamtalk_workspace_sup` from `beamtalk_workspace_app:start/2`** (today `beamtalk_workspace_app_sup` has no children); third `changelog_workspace_id/2` clause; release child-spec set; capability refusals in `beamtalk_repl_ops*` |
| Build (`app_file.rs`) | Emit `beamtalk_workspace` (and `beamtalk_runtime`) into the project `.app`'s `{applications, …}` so OTP starts the workspace before the project app |
| Runtime (`beamtalk_runtime`) | `beamtalk_release:info/0`, `shape_manifest/0`; `Beamtalk releaseInfo`/`shapeManifest` intrinsics |
| Provenance | `beamtalk-provenance.json` writer, reusing `build_stamp::current_otp_version()`; the **build-time shape extractor** (§2.2) — an `erl -noshell` step that loads staged `bt@*` beams and evaluates `__beamtalk_meta/0` — which writes `shapes.json` in Phase 1 and is reused by the Phase 6 preflight and Phase 7 appup diff |
| Policy | `otp-support.toml`; `just otp-matrix`; CI matrix; matrix-vs-declaration test |
| Docs | `docs/development/surface-parity.md` (4 rows); a new `docs/development/deploying.md`; `README.md` OTP window |
| *(BT-3527, not phased here)* | `beamtalk_shape_migration:unpack_strict/1` (§3.4 item 3) — defined by this ADR, implemented by the ADR that first puts an envelope on a wire |

### Phases

**Phase 0 — Wire check (S, but it is the critical path).** Prove the
assumptions Part 1 rests on before building any of it. Review already
settled three of them empirically: `systools:make_script/2` accepts the
suffixed-version directory shape; a `.rel` omitting a declared dependency
fails hard; and a `.boot` that *builds* still does not *boot* under
`--no-include-erts` without `$RELEASE_DIR` (§1.3). What Phase 0 still owes
is the half that matters most, and its acceptance criterion is exact: **a
node boots from `start.boot`, in interactive mode, with the real
`beamtalk_runtime` + `beamtalk_stdlib` + one project app staged, and every
`bt@*` class is registered before the project's root supervisor starts** —
which requires the §1.4 app-env start path to exist in at least a
hand-written form. That criterion is where the on_load/embedded trap, the
no-children `beamtalk_workspace_app_sup`, and the `.app` dependency order
all either work or fail visibly. Anything learned here is far cheaper than
learning it in Phase 2.

**Phase 1 — Release assembly (L).** `beamtalk release` end to end: manifest
`[release]`, app closure, staged lib tree, `.rel`, `systools:make_script`,
tarball, ERTS bundling, `beamtalk-provenance.json`, and the **build-time
shape extractor** (§2.2) that produces `shapes.json` — an `erl -noshell`
step loading the staged `bt@*` beams and evaluating `__beamtalk_meta/0`,
built here because `shapes.json` cannot exist without it and reused
unchanged by Phases 6 and 7. Ships without a launcher (boot with
`erl -boot`), so the assembly is verifiable on its own. *Tests:* Rust unit
tests in `beamtalk-cli` for the closure computation and `.rel` writer; one
CLI integration test that builds a release from a fixture project and
asserts the staged tree, the `.boot`, `shapes.json` and the provenance
file exist and parse — the Phase 0 napkin promoted into CI.

**Phase 2 — Release-mode runtime (L, was M).** The `mode` triple, the
release child-spec set, the third `changelog_workspace_id/2` clause, §1.5's
capability refusals — and the two pieces §1.4 identifies as genuinely new:
`beamtalk_workspace_app:start/2` **reading its config from the application
environment and starting `beamtalk_workspace_sup`** when `mode` is set, and
`app_file.rs` emitting `beamtalk_workspace` into the project `.app`'s
`{applications, …}` so OTP orders the start. Activation runs from the
`.app` module lists through `activate_modules/2`. *Tests:* EUnit in
`beamtalk_workspace` for the env-driven start, and the boot smoke test —
start a real release from `start.boot` and assert every `bt@*` class is
registered before the root supervisor's first actor, and that the idle
monitor, compiler app and ADR 0105 stores are absent. This is the phase
that ships the first deployable artifact together with Phase 1; the
`foreground`/`stop` verbs are pulled forward from Phase 3 into it for that
reason.

**Phase 3 — Launcher (M).** `bin/<name>` + `.cmd` and the five verbs
Phase 2 did not already need (`ping`, `remote_console`, `eval`, `rpc`,
`version`; `foreground`/`stop` shipped with Phase 2). `eval` and `rpc` are
`run-entry`-shaped with `node_owning` set per verb (§1.7); every boot
passes `-boot_var RELEASE_DIR`; the OTP boot check enforces
`host_major ∈ [build_major, build_major + 2]` (§3.2). Windows CI coverage
is part of the phase, not a follow-up (ADR 0027). *Tests:* a launcher
test per verb on both platforms, including `eval`'s exit-code adoption
and `rpc` against a running fixture release.

**Phase 4 — Console in release mode (M).** `[release] console`, cookie from
`RELEASE_COOKIE`/`vm.args`, loopback default, non-loopback warning,
`remote_console`, `beamtalk repl --host`. Depends on Phase 2's capability
classification. *Tests:* `tests/repl-protocol/cases/*.btscript` against a
release-mode node, asserting that every §1.5 refusal comes back as the
named `#beamtalk_error{}` and that `eval`/`inspect` still answer — the
protocol suite is the natural home because it already exercises the same
`beamtalk_repl_server` the release reuses.

**Phase 5 — OTP support policy (S).** `otp-support.toml`, `doctor`/`build`/
`release` consumers, CI matrix, the drift test, README and docs.

**Phase 6 — Upgrade preflight (L, was M).** `beamtalk release --upgrade-from`
and §2.3's report. The size is honest about §2.3's seam: the §2.2
extractor must build a `beamtalk_shape_diff:shape()` record from beams —
the flattened field map with types plus the migrations table — with no
live ETS to walk, which is a new leaf, not a call into one; only the diff
itself is reused. *Tests:* EUnit pinning the extractor's output for a
fixture class against what `beamtalk_workspace_shape_store` produces for
the same class live — that equality is the conformance test that keeps the
disk and live flattenings from drifting — plus a CLI test over two fixture
releases asserting §2.3's report verbatim.

**Phase 7 (deferred, separate epic) — relup (L).** §2.2's appup generation,
`systools:make_relup/4`, `release_handler` integration, hand-written
`.appup` files for the runtime applications as part of
`docs/development/releasing.md`, and the §2.2 `Extra` conformance test.

### Open questions for implementation

- ~~Does `systools:make_script/2` need `{path, …}` for the staged tree?~~
  **Answered, then corrected.** `{path, ["lib/*/ebin"]}` makes
  `make_script/2` succeed, but the resulting script is `$ROOT`-relative and
  fails to boot under `--no-include-erts` (§1.3). The real answer is
  absolute paths plus `{variables, [{"RELEASE_DIR", Dir}]}` and
  `-boot_var RELEASE_DIR` at boot; `-pa` is still not required. A lesson
  for Phase 0's criterion: "the tool ran" is not the check, "the node
  booted" is.
- ERTS copy fidelity on macOS: `erts-*/bin` contains signed binaries;
  confirm the copy survives Gatekeeper, or document `--no-include-erts` as
  the macOS default.
- Whether `beamtalk_stdlib`'s 115 `bt@stdlib@*` modules should
  be pruned to the transitively-reachable set (a large artifact-size win,
  but it breaks `Object allSubclasses` reflection and DNU-based dynamic
  dispatch). Out of scope for v1; recorded as a size optimisation.

---

## Migration Path

Nothing user-facing changes. `beamtalk build --escript` keeps working
identically, and no existing project needs edits: `[release]` is entirely
optional and every key has a default. A project that already declares
`[application] supervisor` — the shape ADR 0061 `beamtalk run` services
already use — is releasable with no manifest change at all.

Internally, `repl => boolean()` becomes `mode => run | workspace | release`
in `beamtalk_workspace_sup`'s config, and `beamtalk_workspace_meta` takes
the same key (`beamtalk_workspace_meta.erl:72,96`) and changes with it. The
call sites are the CLI's workspace startup, `escript.rs`'s generated
bootstrap, the meta process, and the runtime test suites;
they are updated in the same change as the mode addition, with **no
transitional boolean clause** (the same discipline ADR 0123 applied to the
`code_change/3` `Extra` tuple).

---

## Implementation Tracking

**Epic:** [BT-3567](https://linear.app/beamtalk/issue/BT-3567)
**Status:** Planned
**Issues:**

| Phase | Issue | Title | Size | Blocked by |
|---|---|---|---|---|
| A (ADR Phase 2) | [BT-3568](https://linear.app/beamtalk/issue/BT-3568) | `mode => run \| workspace \| release` on `beamtalk_workspace_sup` + §1.5 capability refusals | M | – |
| A (ADR Phases 0 + 2) | [BT-3569](https://linear.app/beamtalk/issue/BT-3569) | App-env start of `beamtalk_workspace_sup` + `start.boot` wire-check test | M | BT-3568 |
| B (ADR Phase 1) | [BT-3570](https://linear.app/beamtalk/issue/BT-3570) | `beamtalk release` assembly: `[release]`, closure, staging, `.rel`, boot script | M | BT-3569 |
| B (ADR Phase 1) | [BT-3571](https://linear.app/beamtalk/issue/BT-3571) | ERTS, tarball, provenance, shape extractor → `shapes.json` | M | BT-3570 |
| C (ADR Phase 3) | [BT-3573](https://linear.app/beamtalk/issue/BT-3573) | Launcher `bin/<name>` + `.cmd`, all verbs, OTP boot check | M | BT-3571 |
| C (ADR Phase 4) | [BT-3575](https://linear.app/beamtalk/issue/BT-3575) | Release console + `Beamtalk releaseInfo` / `shapeManifest` | M | BT-3568, BT-3573 |
| D (ADR Phase 5) | [BT-3572](https://linear.app/beamtalk/issue/BT-3572) | `otp-support.toml`, doctor/build/release consumers, CI matrix | M | BT-3570 |
| E (ADR Phase 6) | [BT-3574](https://linear.app/beamtalk/issue/BT-3574) | `beamtalk release --upgrade-from` preflight | M | BT-3571 |
| F | [BT-3576](https://linear.app/beamtalk/issue/BT-3576) | `deploying.md`, ADR 0061 amendment, end-to-end release test | S | BT-3572, BT-3574, BT-3575 |

Phase 7 (relup) is a separate, not-yet-filed epic; `unpack_strict/1` is
owned by BT-3527.

## References

- Related issues: BT-3528 (this ADR), BT-3523 (parent), BT-3524 / BT-3533
  (ADR 0123 epic), BT-3527 (distribution), BT-2470 (shared OTP type-spec
  cache), BT-3534 (`__shape_version__` tracking)
- Related ADRs:
  - [ADR 0061](0061-program-entry-points-and-run-lifecycle.md) — entry
    points, run lifecycle, § Future: Release Mode (constraint resolved in §1.4;
    its "TLS + auth" note corrected in §1.6)
  - [ADR 0099](0099-cli-application-story.md) — `Console`, `main:`, two-tier
    exit, `build --escript`
  - [ADR 0123](0123-versioned-state-migration.md) — `shapeVersion:`,
    `migrateFromVN:`, the `code_change/3` `Extra` contract, the envelope
  - [ADR 0026](0026-package-definition-and-project-manifest.md) §3 — package
    ↔ OTP application mapping
  - [ADR 0070](0070-package-namespaces-and-dependencies.md) — dependency
    closure
  - [ADR 0022](0022-embedded-compiler-via-otp-port.md) — embedded compiler,
    external-toolchain avoidance
  - [ADR 0027](0027-cross-platform-support.md) — Windows tier 1
  - [ADR 0058](0058-platform-security-model.md) /
    [ADR 0020](0020-connection-security.md) — trust boundary, cookie
    handshake, no Beamtalk-implemented TLS
  - [ADR 0113](0113-destructive-workspace-operations.md) /
    [ADR 0082](0082-method-level-edit-save-and-changelog.md) — destructive and
    flush operations, off in release mode
  - [ADR 0098](0098-build-artifact-provenance.md) — provenance stamp,
    compound OTP version key
  - [ADR 0075](0075-erlang-ffi-type-definitions.md) — OTP-version-keyed
    type-spec cache, `debug_info` dependency
  - [ADR 0068](0068-parametric-types-and-protocols.md) § Dialyzer Spec
    Generation
  - [ADR 0105](0105-live-image-recheck-on-reload.md) — live re-check, not
    started in release mode
  - [ADR 0050](0050-incremental-compiler-class-hierarchy.md) —
    `__beamtalk_meta` as the durable hierarchy record (appup derivation input)
  - [ADR 0112](0112-method-level-removal-language-primitive.md) — removal
    semantics for `delete_module`
- Code:
  - `crates/beamtalk-cli/src/commands/escript.rs` — the packaging precedent
  - `crates/beamtalk-cli/src/commands/build/outputs.rs` — `.app` generation
  - `crates/beamtalk-cli/src/commands/build_stamp.rs` —
    `current_otp_version()`
  - `crates/beamtalk-cli/src/build_layout.rs` — build paths
  - `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_sup.erl` — the
    mode config
  - `runtime/apps/beamtalk_runtime/src/beamtalk_hot_reload.erl` —
    `code_change/3`
  - `runtime/apps/beamtalk_runtime/src/beamtalk_shape_migration.erl` — the
    chain and envelope
- Docs: `docs/development/surface-parity.md`,
  `docs/development/releasing.md` (Beamtalk's *own* release process —
  distinct from this ADR, but the home of the hand-written runtime `.appup`
  files Phase 7 requires)
- OTP: `systools`, `release_handler`, `.appup`/`.relup`, OTP Design
  Principles § Releases and Release Handling
