# Environment variables

Every environment variable the code reads, with what it does. `scripts/ci/check_env_docs.py` (run by `just lint-env-docs`, part of `just lint`) fails when code reads a name that is not a row in a table below, and when a row names a variable nothing reads.

The check scans `*.rs`, `*.erl`, `*.hrl`, `*.ex`, `*.exs` and `*.escript` for `env::var`, `env!`, `option_env!`, `os:getenv`, `System.get_env`, the helper wrappers `require_env`/`env_or*`/`env_flag*`/`read_env*`, and `*ENV*`/`*VAR*` constants. In `*.sh`/`*.bash` it finds `$NAME` / `${NAME}` only when the name starts with `BEAMTALK_`, `BT_`, `E2E_`, `VS_BENCH_` or `VD_BENCH_`. Cargo's own build-script variables (`OUT_DIR`, `CARGO_MANIFEST_DIR`, `CARGO_PKG_VERSION`, `CARGO_CFG_*`) are exempt.

**What the check cannot see:** un-prefixed shell knobs (for example `SKIP_ERLANG`, `SKIP_NODE`, `SKIP_NET`, `MISE_*` and `SKILLS_INIT_*` in `scripts/setup-cloud.sh` and `scripts/cloud-doctor.sh`, `PREFIX` in `scripts/install.sh`), because a shell variable without a project prefix cannot be told from a script-local one; names built at run time; and wrappers with other names. Read the header comment of each script for those.

Each row describes the read site, not an end-to-end run; rows for test knobs were checked against the code only.


## Runtime and CLI

Read by shipped binaries and the runtime. Unset means the default behaviour.

| Variable | Meaning |
|---|---|
| `BEAMTALK_HOME` | Root for workspace state; default `~/.beamtalk`. The CLI, the detached BEAM node, `beamtalk workspace` and the VS Code extension all honour it (`crates/beamtalk-env/src/home.rs`). An empty value counts as unset. |
| `BEAMTALK_WORKSPACE` | Name of an explicit workspace session; highest priority when the CLI picks a workspace (`crates/beamtalk-cli/src/paths.rs`). |
| `BEAMTALK_WORKSPACE_TIMEOUT` | Overrides the workspace idle timeout, in seconds (`IDLE_TIMEOUT_ENV_VAR`, `crates/beamtalk-workspace/src/lib.rs`). |
| `BEAMTALK_SESSION_IDLE_SECONDS` | How long a detached REPL session stays resumable, in seconds; default 4 hours. Read by the workspace node after the `session_idle_seconds` application env. |
| `BEAMTALK_NO_BUILD_LOCK` | Set to a non-empty value other than `0` to skip the project build lock (`_build/.build.lock`), for filesystems without advisory-lock support; concurrent beamtalk processes in one project are then not serialised (`crates/beamtalk-cli/src/commands/dir_lock.rs`). |
| `BEAMTALK_NODE_NAME` | Node name for the REPL; the `--node` flag takes priority. |
| `BEAMTALK_REPL_PORT` | Port for the REPL listener (`crates/beamtalk-cli/src/commands/repl/mod.rs`); `0` is OS-assigned. |
| `BEAMTALK_COOKIE_HOME` | Internal. The CLI and the release launcher start `erl` with a private directory holding the cookie as its home, so no cookie is in argv (BT-5185); this names that directory. The node restores the home variables and removes it on boot (`crates/beamtalk-cli/src/commands/cookie_env.rs`, `beamtalk_cookie_home:restore/0`). Not meant to be set by hand. |
| `BEAMTALK_ORIG_HOME` | Internal. The `HOME` the private cookie home replaced (`set:<value>` or `unset`), for `beamtalk_cookie_home:restore/0`. |
| `BEAMTALK_ORIG_HOMEDRIVE` | Internal. As `BEAMTALK_ORIG_HOME`, for Windows' `HOMEDRIVE` (and the same name with a `HOMEPATH` suffix for `HOMEPATH`). |
| `HOMEDRIVE` | Windows. With `HOMEPATH`, where `erlexec` takes a node's home (and so `.erlang.cookie`) from; the CLI and launcher point both at the private cookie home while a node boots. |
| `BEAMTALK_WS_ALLOWED_HOSTS` | Comma-separated hostnames the workspace WebSocket accepts in `Host`/`Origin` besides loopback and the bound IP (reverse proxy, MagicDNS). See `docs/development/remote-access.md`. |
| `BEAMTALK_COOKIE` | Erlang cookie the MCP server uses with `--port`; must be non-empty then. |
| `BEAMTALK_WORKSPACE_BOOT_TIMEOUT_SECS` | MCP server: how long to wait for `beamtalk repl` to report its port. |
| `BEAMTALK_WORKSPACE_TCP_TIMEOUT_SECS` | MCP server: how long to wait for the workspace TCP port; the code default is 60. |
| `BEAMTALK_MCP_EVAL_TIMEOUT` | MCP server: timeout for `eval`, in seconds; default 30. The `evaluate` tool's `timeout_secs` overrides it per call. |
| `BEAMTALK_MCP_TEST_TIMEOUT` | MCP server: timeout for test operations, in seconds; default 120. |
| `BEAMTALK_REPL_STARTUP_TIMEOUT_MS` | MCP client: how long to wait for a shared REPL to start, in milliseconds. |
| `BEAMTALK_COMPILER` | `escript` selects the legacy compiler backend; any other value falls back to the port backend with a warning (`crates/beamtalk-cli/src/beam_compiler.rs`). |
| `BEAMTALK_COMPILER_PORT_BIN` | Path of the compiler port binary. |
| `BEAMTALK_EXEC_PORT_BIN` | Path of the exec port binary; checked before the build directories and `PATH` (`beamtalk_exec_port.erl`). |
| `BEAMTALK_RUNTIME_DIR` | Explicit runtime directory; an invalid directory is a hard error (`crates/beamtalk-core/src/ffi_type_specs.rs`). |
| `BEAMTALK_STUBS_DIR` | Overrides the sysroot-derived directory of distribution stubs; an invalid value only logs a warning. |
| `BEAMTALK_CACHE_DIR` | Base directory of the shared OTP type-spec cache; default is the platform cache directory. |
| `BEAMTALK_ALLOW_INSECURE_GIT` | `1` lets a plaintext (`http://`, `git://`) git URL be used for a dependency, a native dependency (manifest or `beamtalk.lock`) or the registry index; otherwise it is refused on first resolve and on locked builds alike (`ALLOW_INSECURE_GIT_ENV`, `crates/beamtalk-package/src/manifest/git_url.rs`). |
| `BEAMTALK_REGISTRY` | Overrides the package registry index location (`crates/beamtalk-cli/src/commands/deps/registry.rs`). |
| `BEAMTALK_REGISTRY_CACHE_DIR` | Overrides where a git-backed registry index is cached. |
| `BEAMTALK_NO_FILE_LOG` | `1` disables the workspace file logger (`workspace.log`). |
| `BEAMTALK_NO_FOREGROUND_PRUNE` | Set by tests and harnesses so the workspace never prunes the real workspaces directory at start-up. |
| `BEAMTALK_CHECK_NLR_COMMIT` | Passed through to the workspace node when set; switches on a check in `beamtalk_actor_state:commit/2` used by the project's own test tooling. |
| `BEAMTALK_CODEGEN_DIAGNOSTICS` | `1` enables codegen diagnostics. |
| `BEAMTALK_WARN_STATEACC` | `1` enables the state-accumulator warning. |
| `BEAMTALK_CORE_SNAPSHOT_DIR` | When set, `build-stdlib` writes `.core` snapshots there and skips incremental reuse (`just core-diff`). |
| `BEAMTALK_CORE_SNAPSHOT_ONLY` | Opt-in for snapshot-only output (`crates/beamtalk-cli/src/commands/util.rs`). |
| `BEAMTALK_CLI_PATH` | Desktop broker: path of the `beamtalk` CLI. Honoured in release builds and trusted as-is, like `PATH`, which the broker also searches: both come from the user's own environment. |
| `BEAMTALK_ATTACH_LAUNCHER` | Desktop shell: path of a locally built `bt_attach` launcher. Debug builds only; a release build ignores it and runs the bundled launcher (BT-5194). |
| `BEAMTALK_BUILD_CHANNEL` | Distribution channel (`edge`, `nightly`) of a CI build; set by `edge.yml` and `nightly.yml`. |
| `BEAMTALK_BUILD_COMMIT` | Commit being built when there is no `.git` directory; otherwise `git rev-parse` is used. |
| `BEAMTALK_BUILD_ID` | Compile-time (`env!`) build identity of the compiler port; set by a build script. |
| `BEAMTALK_VERSION` | Compile-time (`env!`) version baked into `__beamtalk_meta`; set by a build script. |
| `BEAMTALK_DIRTY_TAG` | Compile-time (`env!`) tag that keeps dirty-tree compilers from sharing provenance; set by a build script. |
| `BEAMTALK_SPEC_MAPPING_STAMP` | Compile-time (`env!`) stamp for the type-spec mapping; set by a build script. |
| `BEAMTALK_NIGHTLY` | `scripts/install.sh`: `1` installs the nightly build. |
| `BEAMTALK_EDGE` | `scripts/install.sh`: `1` installs the edge build. |
| `BEAMTALK_PREFIX` | Install prefix for `scripts/install.sh` (default `~/.beamtalk`, same as `--prefix`) and `scripts/refresh-edge.sh` (default `/usr/local`). |
| `BEAMTALK_INSTALLER_URL` | `scripts/refresh-edge.sh`: installer to run when the edge build is stale; its checksum is read from `<url>.sha256`. For tests; lifts the https-only rule for the installer fetches. Ignored with a warning when `CLAUDE_CODE_REMOTE=true`. |
| `BEAMTALK_INSTALLER` | `scripts/refresh-edge.sh`: path of a local installer, run unverified; wins over the URL. Ignored with a warning when `CLAUDE_CODE_REMOTE=true`. |
| `BEAMTALK_EDGE_BASE_URL` | `scripts/refresh-edge.sh`: download base for the tarball checksum (default: the edge release). For tests; ignored with a warning when `CLAUDE_CODE_REMOTE=true`. |
| `BEAMTALK_PINS_REF` | `scripts/setup-cloud.sh`: git ref of `.tool-versions` fetched when the checkout has none; default `main`. |
| `RELEASE_NODE` | Release launcher's `-sname` override (the name part of `-name` under `RELEASE_DISTRIBUTION=tls`); the workspace node uses it as its `workspace_id`. |
| `RELEASE_HOST` | Release launcher, under `RELEASE_DISTRIBUTION=tls` (the cross-host opt-in, ADR 0126 amendment D2; see `docs/development/deploying.md` § Clustering across hosts): this host's name, the host part of `-name`. Checked by `beamtalk_release_launcher:tls_preflight/1`. |
| `RELEASE_DIST_INTERFACE` | Release launcher, under `RELEASE_DISTRIBUTION=tls`: the one IP address distribution and epmd listen on (no default; `0.0.0.0` only when named). Parsed by `beamtalk_dist_address:parse/1`, the shared loopback rule. |
| `RELEASE_SSL_DIST_OPTFILE` | Release launcher, under `RELEASE_DISTRIBUTION=tls`: the `-ssl_dist_optfile`; it must set `{verify, verify_peer}` on both sides and `{fail_if_no_peer_cert, true}` on the server side. |
| `ERL_EPMD_ADDRESS` | Erlang's epmd address (a comma-separated list). The REPL sets a loopback value unless an operator exported one. A node's distribution listener stays on loopback unless an entry is a non-loopback IP address; `localhost`, `::1`, any `127.x.x.x` and unparseable values keep it pinned (`tests/conformance/loopback_dist_conformance.json`). |
| `ERL_EPMD_PORT` | Non-default epmd port (`crates/beamtalk-workspace/src/epmd.rs`). |
| `GIT_ALLOW_PROTOCOL` | Set by the build on the rebar3 it runs for native dependencies (`https:ssh`, plus `http:git` with `BEAMTALK_ALLOW_INSECURE_GIT=1` and `file` when a declared or locked dependency uses `file://`), so git refuses any other transport before connecting, transitive dependencies included (`crates/beamtalk-cli/src/commands/build/native.rs`). An inherited value is not passed through. |
| `RUST_LOG` | Enables tracing in the CLI when set; default is no logs. |
| `NO_COLOR` | Disables REPL colour output. |
| `TERM` | `dumb` disables REPL colour output. |
| `HOME` | Forwarded to the workspace node for `~/.erlang.cookie` and epmd. |
| `PATH` | Read by `crates/beamtalk-cli/src/commands/release/launcher.rs` (launcher tests prepend a fake directory). |
| `COMSPEC` | Windows shell used by `beamtalk_os.erl`; default `cmd.exe`. |
| `TMPDIR` | First choice for `File tempDirectory`; then `TMP`, then `TEMP` (`beamtalk_file.erl`), then a platform default. |
| `TMP` | Second choice for `File tempDirectory`; see `TMPDIR`. |
| `TEMP` | Third choice for `File tempDirectory`; see `TMPDIR`. |
| `USERPROFILE` | Read only by `beamtalk_platform_tests.erl` (Windows home-directory fallback test). |

## LiveView IDE (`editors/liveview`)

Read by `config/runtime.exs` and the `bt_attach` application.

| Variable | Meaning |
|---|---|
| `PORT` | HTTP port; default 4000. |
| `PHX_SERVER` | When set, starts the endpoint (release use). |
| `PHX_HOST` | Public host name; advertises `https://<host>:443`. |
| `SECRET_KEY_BASE` | Phoenix secret key base. |
| `DNS_CLUSTER_QUERY` | DNS query for clustering. |
| `SOME_APP_SSL_KEY_PATH` | Appears only in a commented-out HTTPS example in `runtime.exs`. |
| `SOME_APP_SSL_CERT_PATH` | Appears only in a commented-out HTTPS example in `runtime.exs`. |
| `BT_ATTACH_BIND_IP` | Bind address (e.g. `::`, `0.0.0.0`); unset binds loopback only. A non-loopback bind with no OIDC and no `BT_IDE_DEV_AUTH` refuses to boot unless `BT_IDE_ALLOW_UNAUTHENTICATED=1`. An unparseable value raises. |
| `BT_ATTACH_NODE_SUFFIX` | Suffix for the front's distribution name; the desktop broker sets it to the workspace id. Without it the name is `bt_attach_<unique integer>@localhost`. |
| `BT_ATTACH_PORT_FILE` | File the front writes its chosen port to; the desktop broker reads the same name. |
| `BT_IDE_ALLOW_UNAUTHENTICATED` | `1` lets a non-loopback bind boot without an auth gate; a warning is logged on every boot. |
| `BT_IDE_DEV_AUTH` | `1`, `true` or `yes` enables development authentication. |
| `BT_IDE_CONFIG` | Path of the IDE config file (`ide.toml`) read by the desktop broker. |
| `BT_WORKSPACE_NODE` | Workspace node to attach to; default `beamtalk_workspace_spike@localhost`. |
| `BT_WORKSPACE_COOKIE` | Cookie for the workspace node. |
| `BT_WORKSPACE_ID_ARG` | Workspace id handed to the `beamtalk_home` eval, never interpolated into the eval string. |
| `BT_OIDC_ISSUER` | OIDC override of the config file's issuer. |
| `BT_OIDC_CLIENT_ID` | OIDC client id override. |
| `BT_OIDC_CLIENT_SECRET` | OIDC client secret. |
| `BT_OIDC_REDIRECT_URI` | OIDC redirect URI override. |
| `BT_OIDC_ROLES_OWNER` | Comma-separated groups mapped to the owner role. |
| `ACME_SECRET` | Test fixture only: a `client_secret_env` name in `ide_config_test.exs`. |

## Tests, coverage and diagnostics

Only affect test, coverage, bless and benchmark runs. None changes shipped behaviour.

| Variable | Meaning |
|---|---|
| `BEAMTALK_REQUIRE_ERLANG` | Makes a missing Erlang toolchain a test failure instead of a skip; `CI` has the same effect. |
| `BEAMTALK_REQUIRE_RUNTIME` | CI sets it so a missing runtime fails agreement tests instead of skipping them. |
| `BEAMTALK_TEST_HEX_REPO_URL` | Debug builds only: unsigned Hex repository used by native-build tests. |
| `BEAMTALK_TEST_PORT` | MCP client tests: port of a shared REPL. |
| `BEAMTALK_TEST_COOKIE` | MCP client tests: cookie of the shared REPL. |
| `BEAMTALK_EXEC_TEST_BIN` | `beamtalk-exec` end-to-end tests (`crates/beamtalk-exec/tests/port_protocol.rs`): binary to test instead of the one Cargo built, e.g. an older build to show what a test catches. |
| `BEAMTALK_BIN` | `desktop/e2e/attach-cycle.sh`: path of the `beamtalk` CLI; default `target/debug/beamtalk`. |
| `BT_ATTACH_LAUNCHER` | `desktop/e2e/attach-cycle.sh`: path of the LiveView release launcher; default `dist-liveview/bin/server`. (The desktop shell's own override is `BEAMTALK_ATTACH_LAUNCHER`.) |
| `BT_E2E_WORKSPACE_ID` | `desktop/e2e/attach-cycle.sh`: workspace id the e2e run creates; default `bt2989_desktop_e2e`. |
| `BT_DESKTOP_BROKER_LIVE_LAUNCHER` | Ignored live-front tests in `beamtalk-desktop-broker`: launcher path; the test panics when unset. |
| `BT_DESKTOP_BROKER_LIVE_WORKSPACE` | Ignored live-front tests: id of a running workspace. |
| `BT_DESKTOP_BROKER_LIVE_DEAD_WORKSPACE` | Ignored live-front tests: id of a workspace whose node is stopped. |
| `BEAMTALK_LIB` | `spikes/eep48-doc-chunks/run.sh`: lib directory of an installed runtime to use instead of a local build. |
| `BEAMTALK_BLESS_OP_TABLE_DOC` | Rewrites the generated op-table doc instead of failing. |
| `ADR0131_BLESS` | Rewrites the ADR 0131 phase-0 summary table. |
| `ADR0131_MEASURE_BLESS` | Rewrites the ADR 0131 statement-measure table. |
| `ADR0131_MEASURE_ONLY` | Restricts the statement-measure run to named entries. |
| `ADR0131_MEASURE_VERBOSE` | Verbose output for the statement-measure run. |
| `ROUTING_TABLE_BLESS` | Rewrites the routing-table fixture. |
| `PIN_DUMP` | Dumps the class-publish pin data in the EUnit pin test. |
| `BT4020_PROGRAMS` | Synced-call corpus: generated programs; default 1024. |
| `BT4020_ERLC_STRIDE` | Synced-call corpus: compile every Nth construct; default 8. |
| `BT4020_PARSE_DUMP` | Synced-call corpus: dump parse results. |
| `BT4020_SITES_DUMP` | Synced-call corpus: dump the site matrix. |
| `BT4020_DUMP_SRC` | Synced-call corpus: dump generated source. |
| `BT4020_DUMP_OUT` | Synced-call corpus: dump generated output. |
| `BT4059_PROGRAMS` | Paren-transparency corpus: number of programs. |
| `BT4059_PARSE_DUMP` | Paren-transparency corpus: dump parse results. |
| `BT4059_TIMING` | Paren-transparency corpus: print timings. |
| `E2E_COOKIE` | Compile-time (`option_env!`) cookie for REPL protocol tests. |
| `E2E_COVER` | Instruments the runtime with `cover` in REPL protocol tests and raises their timeout. |
| `E2E_DEBUG` | Prints REPL protocol test output. |
| `BUNIT_COVER` | When set, `beamtalk test` instruments the Beamtalk app ebins with `cover` for the BUnit run. |
| `STDLIB_COVER` | When set, the stdlib test run is `cover`-instrumented. |
| `EUNIT_ISOLATION_JOBS` | Job count of `scripts/ci/eunit-isolation.escript`; default `min(4, available logical processors)`. |
| `PHX_PLAYWRIGHT` | Selects the Playwright browser suite in LiveView tests. |
| `PLAYWRIGHT_HEADLESS` | `false` runs the browser suite headed. |
| `CARGO_TARGET_DIR` | Target directory used by the example corpus builder. |
| `VS_BENCH_BT_EBIN` | Benchmarks (vs-elixir): Beamtalk ebin directory. |
| `VS_BENCH_DEBUG` | Benchmarks (vs-elixir): debug output. |
| `VS_BENCH_NO_TRACE` | Benchmarks (vs-elixir): disable tracing. |
| `VD_BENCH_BT_EBIN` | Benchmarks (dispatch-overhead): Beamtalk ebin directory. |
| `BENCHEE_SAVE` | Benchmarks (Elixir): Benchee save tag. |
| `BENCHEE_LOAD` | Benchmarks (Elixir): Benchee load tag. |
| `BENCHEE_OVERHEAD` | Benchmarks (Elixir): `1` prints the overhead report. |
| `MEM_N` | Benchmarks (Elixir): actor count for the memory bench; default 20000. |
| `RED_N` | Benchmarks (Elixir): reductions bench size; default 3000. |
