# ADR 0154: beamtalk-uat as the External Acceptance Gate for Full Environments

## Status
Proposed (2026-11-26)

## Context

### The model: swamp-uat

The owner's reference is Paul Stack's
[*The Gate Between Our Agent Code and Our Users*](https://stack72.dev/the-gate-between-our-agent-code-and-our-users/),
which describes `systeminit/swamp-uat`. This ADR adopts that design and maps it
onto Beamtalk. In summary:

- **The shipped artifact is under test, never source.** swamp's unit, contract
  and property tests run on PRs in the main repository. UAT is a separate final
  gate that runs against the exact release binaries users download, one per
  platform target. swamp ships five targets.
- **A separate repository, triggered per release.** swamp's release workflow
  stamps a version, builds the binaries, creates a GitHub release and fires
  `repository_dispatch` (`event-type: run-uat`, payload `{"version": …}`). The
  UAT repository downloads exactly that version and runs. "Any fail blocks the
  binary": nothing reaches stable, Homebrew, Docker Hub or the CDN.
- **The user's point of view.** Each test creates a real repository in an empty
  temp directory, runs real commands against the real binary and checks what a
  user would see. Exit codes are asserted, and JSON output is validated against
  Zod schemas.
- **Coverage shape.** There is one test file per CLI command, whose path mirrors
  the command (`swamp vault list-keys` is `tests/cli/vault/list_keys_test.ts`),
  each exercised with the global flags. Every regression found adds a permanent
  test: "That's how the suite compounds."
- **Adversarial coverage** in seven categories: path traversal, secret leakage
  (including in `--verbose` and error output), injection, SIGTERM during writes,
  stale-lock reclaim after SIGKILL, concurrency, and resource exhaustion or
  corrupt and read-only state.
- **Performance gates.** Each benchmark runs 2 warmup and 5 measured iterations
  and reports p50, p75 and p95. It gates on p75 against a user-experience
  threshold, scaled by a CI slowness multiplier.
- **Governance.** The UAT repository's `CLAUDE.md` opens with "Tests are more
  accurate than the code under test." When a test fails, the code is assumed
  wrong. "Never weaken, relax, or update a test to match current behavior unless
  the user explicitly asks you to update the UAT." The reason: agents open most
  PRs, and loosening a schema is the easiest way to turn CI green while a
  contract silently changes.
- **Stack:** TypeScript on Deno, with Zod. The article's point is independence
  from the toolchain under test, not the particular stack.

All three regressions the article cites passed swamp's unit tests and sat at
integration seams. UAT went red 3 to 4 minutes after each one merged.

### What beamtalk-uat is today

[beamtalk-uat](https://github.com/jamesc/beamtalk-uat) already cites the same
article, and its `CLAUDE.md` already says the released bundle is under test,
tests beat code, and every red files a Linear Bug. What it runs falls short of
the model.

The harness is about 2,800 lines of Rust (`src/lib.rs`, `src/scenario.rs`,
`src/lsp.rs`, `src/mcp.rs`). It auto-discovers `projects/<name>/expect.toml` and
runs one *surface* per scenario. There are 16 scenarios:

| Kind | Count | What it checks |
|------|-------|----------------|
| `cli_*` | 12 | Exit code and a stdout/stderr substring for `new`, `new --app`, `fmt-check`, `lint`, `lint --format json`, `type-coverage`, `--help` and `build`. Only `cli_build` compiles anything. |
| `lsp/*` | 1 fixture, 4 capabilities | `hover`, `completion`, `definition` and `documentSymbol` against the server in AST mode. There is no workspace and no BEAM. |
| `mcp_*` | 2 | A single `evaluate` call each. |
| `smoke` | 1 | `beamtalk test` on a one-class project. |

`[[step]]` sequences work only inside one session-backed surface. A scenario
cannot start a release and then `rpc` into it, start two nodes, or edit a file
and watch the REPL pick it up. As the owner puts it, this is "more like arg
parsing". There are also gaps against the model itself:

- 7 of the CLI's 21 commands are covered.
- JSON output is checked with substrings, not schemas.
- There is no adversarial test and no performance gate.
- The `uat-gate` status arrives after the release is already public, so nothing
  is blocked.

Each leg also compiles the Rust harness first. A cold cargo cache costs the same
12 to 15 minutes on macOS and Windows that BT-5273 measured in beamtalk's
`platform-check`.

### What got through on 2026-11-26

None of these could have been caught by the current scenarios:

- **BT-5283 broke downstream builds.** The return-type check on `^` early returns
  merged at 13:45Z. Within the hour, beamtalk-mint's CI failed
  `--warnings-as-errors` with 13 rejected methods, and catalina failed the same
  way. The rejected shape was the `x isError ifTrue: [^x]` error-propagation
  idiom, which buccaneer's own `Buccaneer start:` also uses. BT-5290 fixed it the
  same day. Nothing builds the satellite packages against the toolchain before
  release. `cross-repo.yml` builds only beamtalk-http, and from source.
- **BT-5264: macOS epmd shadowing.** On macOS, a loopback client verb started a
  second epmd beside a wildcard TLS node's epmd, so local lookups reported the
  node as down. Linux refuses that bind, so Linux-only tests cannot see it.
  Someone found it by hand.
- **Release-launcher issues** around TLS distribution (BT-5233, BT-5256) were also
  found by hand. Nothing runs `beamtalk release` from an installed bundle, starts
  the result and talks to it.
- **Cold-cache timeouts (BT-5273).** `platform-check` hit its 40-minute limit
  because a cold Rust build took 12 to 15 minutes. uat's legs have the same
  exposure.

### What already exists, so this ADR doesn't duplicate it

| Existing suite or tool | Builds from | Answers | Stays in beamtalk |
|------------------------|-------------|---------|:--:|
| Rust, EUnit, stdlib and BUnit tiers (`just test`) | source | Is each unit correct? | ✓ |
| REPL protocol tests, cross-surface parity (`tests/parity`) | source | Do the protocol and surfaces agree, down to framing? | ✓ |
| `examples/cluster/scripts/e2e.sh` | source | Does ADR 0126 work across three local nodes? | ✓ |
| `liveview.yml`, `liveview-e2e.yml` | source | Does the IDE work against the same commit's workspace? | ✓ |
| `ci-platforms.yml` | source | Do the tests pass on macOS and Windows? | ✓ |
| Micro and dispatch benchmarks (`bench-dispatch` keys, vs-elixir rows, reduction and word counts) | source | Is this dispatch path fast? Did this PR fix it? | ✓ |
| Coverage (`coverage.yml`) | instrumented source | Which lines run? | ✓ |
| `cross-repo.yml` | source | Does beamtalk-http build against main? | replaced by J9 |

Every one of these builds from the same commit it tests. None answers the user's
question: *with only the published artifacts, the public registry and my OS, can
I get from install to a running service, and is it fast and safe?* The answer
depends on everything between the source tree and the user:

- `scripts/package-release.sh` and `install.sh`;
- the archive layout and ERTS/OTP skew;
- release-build settings;
- the registry index;
- the separately released IDE;
- the satellite packages' own `--warnings-as-errors` builds.

### Constraints (owner decisions)

- Everything runs in CI on Linux, macOS and Windows wherever the feature exists
  on that platform.
- The **real package registry** is in scope, not a mirror.
- **LiveView is in scope** if feasible. This ADR finds that it is (§ 13).
- uat is redesigned from a clean slate. The current harness, `expect.toml`,
  surface types and layout carry no weight.
- **Performance split.** uat owns user-level performance gates on the released
  bundle. beamtalk keeps the micro and dispatch benchmarks, which need internal
  probes and source builds and are how PRs prove a fix.
- **Coverage stays in beamtalk CI.** It needs an instrumented source build and is
  not a property of the shipped artifact.
- The gate stays a separate repository, triggered per release, as in the
  article.

## Decision

### 1. Purpose and the user-only rule

**beamtalk-uat proves that a user holding only published artifacts can use every
command, complete every supported journey, stay safe under adversarial input and
get acceptable performance, on every release target.**

**The user-only rule:** a test may use only what a user has:

- the bundle for its target, installed the documented way;
- the IDE archive;
- the public registry and package repositories;
- documented commands, flags and environment variables;
- the OS's own tools.

Tests never check out beamtalk source, never set an undocumented variable and
never call an internal function. If a test needs one of these, that is a finding:
a docs bug or a missing feature.

This rule draws the line against the in-repo suites. Those suites answer "is the
code right?" uat answers "does the shipped thing work for a user?" A behaviour
pinned by a fast in-repo test is not re-asserted in uat at finer grain.

### 2. Governance: the uat CLAUDE.md

The rewritten `CLAUDE.md` opens with these rules, adapted from swamp-uat. They
bind every agent and human working in the repository:

1. **Tests are more accurate than the code under test.** When a test fails, the
   default assumption is that Beamtalk (the toolchain, IDE or installer) must
   change, not the test.
2. **Never weaken, relax, skip or update a test, schema or threshold to match
   current behaviour unless the owner explicitly asks you to update the UAT.**
   That covers:
   - loosening a Zod schema;
   - raising a perf threshold or the CI multiplier;
   - widening a timeout;
   - adding a platform exclusion;
   - marking a test known-failing.
   A PR doing any of these must quote the owner's request.
3. **Every red files a Linear Bug** against `BT` (automated by `triage.yml`, § 11).
   A red that stays red until a fix ships links its issue from the test.
4. **Every regression gets a permanent test.** A bug found by hand, by a user or
   by a downstream package against a published artifact is not closed until uat
   has a test that fails on the broken release and passes on the fixed one.
   BT-5283 and BT-5264 are the first two.
5. **Independence.** uat never imports, vendors or builds Beamtalk source, and
   its judge never runs on the toolchain under test (§ 4).

**Enforcement.** Rule 2 is enforced mechanically, not by trust:

- `CODEOWNERS` makes the owner the required reviewer for `schemas/**`,
  `bench/thresholds.toml` and `playwright.config.ts`.
- A CI check, `governance.yml`, fails any PR whose diff removes an assertion,
  adds `test.skip` / `test.fixme` / `.only`, or raises a number in
  `thresholds.toml`, unless the PR carries the `uat:owner-approved` label, which
  only the owner can apply.

### 3. Per-target artifacts and the trigger

**Targets.** These are the bundle targets `release.yml` builds today, each tested
on the runner image that built it. `edge.yml` gains a `macos-arm64` build (owner
decision 3), so macOS-only regressions like BT-5264 surface within the hour
rather than the next night. The macOS edge leg runs the **fast set** (§ 10):

| Target | Runner | Edge | Nightly | Tag |
|--------|--------|:----:|:-------:|:---:|
| `linux-x86_64` | `ubuntu-24.04` | ✓ | ✓ | ✓ |
| `macos-arm64` | `macos-14` | ✓ (fast set) | ✓ | ✓ |
| `macos-x86_64` | `macos-15-intel` | — | ✓ | ✓ |
| `windows-x86_64` | `windows-2022` | — | ✓ | ✓ |

Every channel dispatches. `edge.yml` (every merge to `main`), `nightly.yml` and
`release.yml` each send `repository_dispatch` with
`event-type: beamtalk-artifact` and this payload:

```json
{ "channel": "edge|nightly|release", "version": "0.6.0-edge+c6b28b3",
  "tag": "edge|nightly|v0.6.0", "sha": "<commit>",
  "targets": { "linux-x86_64": "<sha256>", "...": "..." } }
```

uat downloads exactly those archives and verifies each target's sha256 before
installing. "The bundle for my target" is a checked fact, not "whatever `latest`
resolves to now". For tags, the payload also lists the IDE archives and the
VS Code `.vsix`.

### 4. Clean rewrite: TypeScript on Playwright Test, with Zod

beamtalk-uat is rewritten. The Rust harness, `expect.toml`, surface types and
`projects/<scenario>/` are deleted. What carries over is the repository, the
governance rules, the version selector (`latest | edge | nightly | vX.Y.Z |
local:<path>`), the dispatch and status wiring, and the contracts worth keeping
(§ 14).

The harness is **TypeScript on [Playwright Test](https://playwright.dev/docs/test-intro)**
used as a general runner, with **Zod** for JSON contracts. Swamp shows
TypeScript and Zod working well for this job. Playwright Test was chosen over
Deno's runner for what a multi-process, multi-surface gate needs:

| Need | What provides it |
|------|------------------|
| Ordered journeys with per-step reporting | `test.step()`. A red result reads "J4 › rpc into release › step 5/9". |
| Time budgets | Per-test and per-step timeouts, plus a global limit. |
| Diagnosable failures | `testInfo.attach()`, the HTML report, JUnit XML and traces, uploaded as one artifact. |
| Tiers and platform selection | Tags with `--grep`, and `projects`. |
| Teardown | Fixtures with guaranteed teardown, where the process-tree kill and leak checks live. |
| Parallelism | `--workers`, `--shard=i/n`. |
| LiveView | Native Chromium, Firefox and WebKit, already proven in `liveview-e2e.yml`. |
| Cross-platform with no cold build | Node behaves the same on all three OSes. `npm ci` from a cache takes seconds. |
| JSON contracts | Zod schemas in `schemas/`, one per `--format json` / MCP / protocol shape. |

**Use real clients.** MCP steps use the official MCP TypeScript SDK client. LSP
steps use `vscode-jsonrpc` / `vscode-languageclient`. These are the libraries
Claude Code, other agents and VS Code use. The current harness hand-rolls both,
on purpose, so that a library cannot hide a framing bug. For an acceptance gate
that reasoning is backwards: a framing bug that every real client tolerates is
not a user-facing bug, and one that breaks them should turn uat red.
Byte-level framing stays in the in-repo protocol and parity suites.

**Fixtures stay Beamtalk.** The apps under test are real Beamtalk projects in
`fixtures/`, with BUnit for in-app assertions. The judge is not Beamtalk, so a
broken toolchain cannot take down its own verdict.

### 5. Four layers of tests

```text
beamtalk-uat/
├── tests/
│   ├── cli/            # Layer 1: command contracts, one file per command, path mirrors it
│   │   ├── build.test.ts
│   │   ├── deps/add.test.ts   deps/list.test.ts   deps/update.test.ts
│   │   ├── workspace/list.test.ts   workspace/attach.test.ts   …
│   │   └── release.test.ts   publish.test.ts   …   (all 21 commands)
│   ├── journeys/       # Layer 2: J1 to J9, cross-surface, cross-process
│   ├── adversarial/    # Layer 3: one file per category (§ 7)
│   └── bench/          # Layer 4: user-level performance gates (§ 8)
├── schemas/            # Zod: lint/test/build JSON, MCP results, release info, …
├── fixtures/           # real Beamtalk projects (hello_web, counter_live, farm_cluster, …)
├── lib/                # env, toolchain, proc, repl (pty), mcp, lsp, release, node, ide, artifacts
├── bench/thresholds.toml   # p75 thresholds per benchmark per target (owner-gated)
└── playwright.config.ts
```

**Layer 1: command contracts.** Each of the 21 commands `beamtalk --help` lists
has a file whose path mirrors it. For each command:

- the global flags (`-v` / `-vv`, `--help`) work;
- documented exit codes hold;
- every `--format json` output parses against its Zod schema, with fields, types
  and patterns, not substrings;
- diagnostics go to the documented stream (human text to stderr, machine formats
  to stdout).

`publish` runs against a throwaway local-directory registry (a documented
`BEAMTALK_REGISTRY` form), never the real index.

**Layer 2: journeys** (§ 6). Ordered, cross-surface user stories sharing one
environment.

**Layer 3: adversarial** (§ 7). Beamtalk-specific versions of swamp's seven
categories.

**Layer 4: performance gates** (§ 8).

A journey reads as the user's story. Here is J3:

```ts
// tests/journeys/j3-packages.test.ts
import { journey, expect } from '../../lib';
import { LockfileSchema } from '../../schemas';

journey('J3 registry package serves HTTP', {
  tier: 'gate', budget: '8m', targets: 'all',
}, async ({ env, bt, step }) => {
  const app = await step('new --app', () => bt.newProject('hello_web', { app: true }));

  await step('deps add buccaneer from the real registry', async () => {
    await bt.in(app).run(['deps', 'add', 'buccaneer']);
    const lock = LockfileSchema.parse(await app.lockfile());
    expect(lock).toPin('buccaneer');
    expect(lock).toPin('catalina');              // transitive, resolved via the index
  });

  await step('overlay fixture source', () => app.overlay('fixtures/hello_web'));
  await step('build, warnings as errors', () =>
    bt.in(app).run(['build', '--warnings-as-errors']));

  const port = env.ports.allocate();
  const svc = await step('beamtalk run . (service mode)', () =>
    bt.in(app).spawn(['run', '.'], { env: { PORT: `${port}` }, readyOn: 'stderr:/Started hello_web/' }));

  await step('serve a request', async () => {
    const res = await env.http.get(`http://127.0.0.1:${port}/`);
    expect(res.status).toBe(200);
    expect(await res.text()).toBe('hello from beamtalk');
  });

  await step('stop cleanly, leave nothing behind', () => svc.interrupt({ expectExit: 0 }));
});
```

```beamtalk
// fixtures/hello_web/src/hello_web.bt
Object subclass: HelloWeb

  /// Start the server on $PORT; answers Catalina's server supervisor.
  class serve -> Result(ServerSupervisor, Error) =>
    port := (System getEnv: "PORT" default: "4000") asInteger
    Buccaneer start: #{#port => port} handler: [:ex |
      ex sendResp: 200 headers: #() body: "hello from beamtalk"
    ]
```

**Rules shared by every layer:**

- **Empty start.** Each test gets an empty temp directory, an isolated
  `HOME`/`USERPROFILE`, its own ports and unique node names and cookies.
- **Structured assertions.** Assert on structure where it exists: Zod-validated
  JSON, MCP structured content, LSP result objects, HTTP status, and `Beamtalk
  releaseInfo` through `rpc`. uat never pins REPL display formatting or prompts:
  those are intentional and owned by beamtalk's e2e tests (AGENTS.md, "REPL
  output").
- **Transcripts.** Every step records argv, cwd, the environment diff, stdout,
  stderr, the exit code and wall time.
- **Failure bundle.** On a red, the test attaches:
  - all the transcripts so far;
  - workspace logs, release `log/` directories and `erl_crash.dump`;
  - `epmd -names` and the process tree;
  - `beamtalk --version` and `beamtalk doctor`;
  - the Playwright trace, for J7;
  - a one-line repro: `BEAMTALK_UAT_VERSION=<exact> npx playwright test
    tests/journeys/j4-release.test.ts -g "rpc into release"`.
- **Checked teardown.** No process and no epmd registration may outlive the test.
  A leak is a red step, not silent cleanup.

### 6. The journey catalogue

| # | Journey | Steps (abridged) | Catches |
|---|---------|------------------|---------|
| **J1** | Install | `install.sh` (Linux and macOS) or zip plus `PATH` (Windows) into a clean `HOME` → `--version` matches the payload → `doctor` is healthy → `--uninstall`. | Packaging, checksums, archive layout, a missing ERTS or OTP app. |
| **J2** | Project lifecycle | `new` → fmt-check clean → `build` → `test` → edit → `lint` finds it → `fmt` fixes it → `run` → `new --app` → `run .` → Ctrl+C exits cleanly. | Scaffold drift (BT-2476), the build cache, output routing. |
| **J3** | Registry packages | `deps add` from the real registry → lockfile pins the closure → `build --warnings-as-errors` → buccaneer serves HTTP → `deps update` → warm rebuild. | Index resolution, git checkout, the type checker against real packages. |
| **J4** | Releases | `release` → `start` → `ping` → `rpc "Beamtalk releaseInfo"` → `stop` → leak check; TLS variant (`RELEASE_DISTRIBUTION=tls`, generated CA) plus a loopback client on the same host; `--upgrade-from` flags an unmigrated shape bump. | Launcher verbs, BT-5264-class epmd, TLS preflight, `.cmd` launchers. |
| **J5** | Two-node cluster | Two `farm_cluster` releases → connect → named send → class reference crosses the wire → hot reload on one node → skew is announced → kill a node → `NodeDown` is seen. | ADR 0126 behaviour from installed artifacts. |
| **J6** | Live development | REPL in a pty: spawn a `Counter`, send it messages, edit the file, `:sync`; the same actor keeps its state and runs the new code. MCP through the SDK: `evaluate` → `save_method` → `test` → `flush`, and the change is on disk. LSP in workspace mode: diagnostics, go to definition into a dependency. | Hot reload, flush, the LSP workspace mode that AST-only checks skip. |
| **J7** | LiveView IDE | Install the IDE archive → `bin/server <workspace-id>` → Playwright: page loads → eval round-trips → System Browser shows the class → edit a method → the workspace sees it. | Bundle/IDE skew, packaging, the Attach topology. |
| **J8** | Upgrade path | Install the previous release → project and release → install the candidate over it → same project builds, tests and releases → `--upgrade-from` is clean → the old `~/.beamtalk` cache is harmless. | Cache and lockfile compatibility, stale install state. |
| **J9** | Downstream canaries | For each package in the registry index: its default branch and its latest tag → `beamtalk test --warnings-as-errors`. | BT-5283 / BT-5290. |

J9 reads its package list from the index, so new packages are covered with no
uat edit. Today the index lists agents, announcements, buccaneer, catalina,
collections, hpack, http, linear, mint, oauth and yaml. A red names the first
`(package, file, diagnostic)`, so triage can tell a toolchain regression from a
package bug.

### 7. Adversarial coverage, Beamtalk-specific

There is one file per category in `tests/adversarial/`. Each item is a test
against the installed bundle. A test that finds today's behaviour lacking is a
finding, filed under rule 3. It is not softened to pass.

| Category | Tests |
|----------|-------|
| **Distribution security** | Workspace and foreground-REPL cookies are per-process, in a 0600 file, never `~/.erlang.cookie` or `nocookie` (CHANGELOG, BT-5066). epmd and distribution listen on loopback only by default: assert no listener on a non-loopback address. A node with the wrong cookie is refused. TLS release: a plain-TCP client cannot connect; a peer whose cert is from another CA is refused; the launcher refuses an option file lacking `verify_peer` / `fail_if_no_peer_cert`, or containing `verify_fun` or `partial_chain` (deploying.md § Clustering across hosts). The console WebSocket binds `127.0.0.1` unless `bind` is set. |
| **Secret leakage** | Set a sentinel secret in a cookie and in an env var a program reads. Then assert it never appears in: stdout or stderr at `-v` and `-vv`; REPL error output; MCP `evaluate` error text and structured content; `run --raw-mfa` stacks; workspace and release logs; crash dumps written under the project; `ps` argv of started nodes. |
| **Path traversal** | `new ../escape`, a dependency name or registry index entry containing `..` or an absolute path, `deps add --path` pointing outside, a release tarball with `../` members fed to `--upgrade-from`. `release --output` refuses `~`, `/`, the project root and `src/`, and replaces only a marked or release-shaped directory (deploying.md lists the rules). |
| **Injection** | Launcher `rpc` / `eval` arguments containing shell metacharacters run as Beamtalk text, never as shell (both `bin/<n>` and `bin/<n>.cmd`). Project names with `;`, spaces or quotes in `new`. A registry `git` URL with an `ext::` or `--upload-pack` style payload is refused. |
| **Signals and atomic writes** | SIGTERM (Windows: process kill) during `build`: the next build is correct, with no half-written `.beam` loaded. SIGKILL during `release`: the next run recovers through the `.beamtalk-swap` journal (deploying.md). SIGTERM during MCP `flush`: the source file is old or new, never partial. |
| **Locks** | SIGKILL a `build` holding the project lock: the next build proceeds within 5s. Two concurrent `build`s on one project wait, not fail. Two `release` runs on one `--output` serialise. CLI, LSP and MCP refreshing one registry cache at once never see a half-written index. |
| **Concurrency and hot reload under load** | Reload a class with `:sync` while 4 senders drive 10,000 messages per second at its actors: no lost messages, no crash, every reply is from the old or the new method. Five MCP sessions `save_method` the same class concurrently: the result is one of the five, and `flush` writes a file that compiles. |
| **Resource exhaustion and corrupt state** | An actor writes 100 MB to `Transcript`; MCP `evaluate` answers a 50 MB value; deep recursion: no OOM of the REPL or MCP server, and a bounded, explained error. Corrupt `beamtalk.lock`, a corrupt registry cache, a truncated `.beam` in `_build`, a corrupt workspace `metadata.json`, a read-only project directory: each fails with a structured error and a hint, never a crash or hang. |

### 8. Performance gates

**Method** (the article's): each benchmark runs **2 warmup** iterations, which
are discarded, then **5 measured** iterations. It reports **p50, p75 and p95** and
**gates on p75**:

```text
p75 ≤ threshold[benchmark][target] × BEAMTALK_UAT_BENCH_MULTIPLIER
```

The multiplier defaults to `1.0` on a developer machine. CI sets it per runner
class in the workflow, starting at `1.5` for GitHub-hosted runners, and an owner
can change it under governance rule 2.

**Thresholds** live in `bench/thresholds.toml`, per benchmark and target, with
the user-experience reason beside each. They are calibrated, not guessed:

1. Phase 4 runs the benchmarks report-only for 10 nightly runs.
2. Each threshold is then set at the observed p75 × 1.25 and rounded up.
3. From then on, only the owner can raise one (rule 2). Lowering one, after a
   real speedup, is open to anyone.

**Benchmarks** (owner's list), each against the installed bundle:

| Benchmark | Measures | Fixture |
|-----------|----------|---------|
| `compile.cold` / `compile.warm` | `beamtalk build` wall time with an empty / warm `_build` | a realistic project: buccaneer plus deps |
| `lifecycle.new_build_test` | `new` → `build` → `test` wall time | the scaffold |
| `repl.startup` / `repl.eval_rtt` | time to the first usable prompt; round-trip of `3 + 4` | a project REPL |
| `release.start` | `bin/<n> start` → first successful `ping` | `hello_web` release |
| `actor.throughput` / `actor.latency` | messages per second and p75 send→reply between local actors | `counter_live` |
| `cluster.send` | p75 cross-node named-send round trip | `farm_cluster`, two nodes |
| `registry.resolve` | `deps add` + resolve + fetch, cold and warm index cache | `hello_web` |
| `lsp.latency` | `hover`, `completion`, `definition` p75 in workspace mode | `hello_web` |

These catch what micro-benchmarks miss: release-build flags, packaging,
startup, install layout and registry behaviour. beamtalk keeps
`bench-dispatch`, the vs-elixir rows and the reduction and word counts. Those
need internal probes and source builds, and they remain how a PR proves a fix.
uat does not re-measure dispatch paths.

**When perf gates run.** Perf runs only on nightly and release-candidate runs,
on a dedicated job per target that runs nothing else at the same time. Edge runs
skip it, because shared-runner noise would turn a per-merge gate into a flake
generator. A nightly breach files a `performance` Bug. A release-candidate
breach blocks promotion like any other red (§ 10).

### 9. Harness capabilities

- **Cross-surface, cross-process.** All handles (`Proc`, `Repl`, `McpClient`,
  `LspClient`, `ReleaseNode`, `IdePage`) live in one environment, so a later
  step observes an earlier one.
- **Multi-node.** Unique `-sname`s and cookies per test. epmd state is captured
  and reset, and the documented `RELEASE_*` variables are supported.
- **Network and registry.** J3, J9 and `registry.resolve` use the live registry
  and GitHub. Unreachable hosts are classified `infra` (§ 11), not `toolchain`.
- **Two kinds of time budget.** A hard per-step **hang guard** ("step `rpc`
  exceeded 60s") is separate from the perf gates, which own the
  user-experience numbers.
- **Target tags.** `targets: ['linux', 'macos']` documents an intended gap and
  states its product reason (for example, ADR 0091: no IDE build for Windows).
  Adding an exclusion falls under governance rule 2.

### 10. Gating: what runs when, and what it blocks

| Trigger | Artifact | Layers | Targets | Blocks |
|---------|----------|--------|---------|--------|
| `edge` publish (every merge to `main`) | edge bundles | linux: contracts, J1 to J4, J6, J9, adversarial fast subset. macos-arm64: the fast set (contracts, J1, J2, J4 including TLS, adversarial fast subset) | linux-x86_64, macos-arm64 | Nothing. Posts a `uat-edge` status on the merged commit; a red files a Bug within the hour. |
| `nightly` publish | nightly bundles | all four layers (perf in report-and-file mode) | all 4 | Nothing. Reds file Bugs. |
| Tagged release candidate | `vX.Y.Z` pre-release bundles, IDE archives, `.vsix` | all four layers | all 4 | **Promotion to stable and every distribution channel.** |
| Weekly | `latest` | all four layers | all 4 | Nothing. Catches drift in OS images, the registry and GitHub. |
| beamtalk PR with `ci:uat` | PR-built linux bundle | contracts, J1 to J4 | linux-x86_64 | Informational (Phase 7). |
| `workflow_dispatch` | any | any | any | n/a |

**Promotion is the blocked step**, as in the article: "any fail blocks the
binary." Today `release.yml` runs on `release: published`, so users can install
the bundle before uat reports. Under this ADR:

1. A tag push builds every target and publishes the GitHub release as a
   **pre-release**, which `install.sh`'s `releases/latest` lookup does not see.
2. It dispatches uat with the payload in § 3.
3. uat runs every layer on every target. Green posts `uat-gate: success`.
4. A `promote` job in beamtalk, triggered by that status, then:
   - marks the release latest, which `install.sh` and the manual-download docs
     follow;
   - publishes the VS Code extension to the Marketplace;
   - attaches the IDE archives and pushes `ghcr.io/jamesc/beamtalk-ide:<version>`;
   - runs the desktop release.

   These steps hang off `release: published` or a `workflow_call` from
   `release.yml` today. Owner decision 2 moves the triggers of
   `release-vscode.yml`, `liveview-release.yml`, `liveview-docker.yml` and
   `desktop-release.yml` to the `promote` job. Editors and the IDE then ship
   only after uat passes. Building the `.vsix` and IDE archives still happens
   before uat, as pre-release assets, so J6 and J7 can test them. Only their
   *publication* waits for promote.
5. On red, the release stays a pre-release, Bugs are filed, and the fix is a new
   tag. An owner can promote by hand with a recorded waiver in the release notes.
   No agent may.

**What blocks promotion:**

- any red in contracts, journeys or adversarial tests on a target the test
  declares;
- any p75 over its threshold × multiplier;
- a J9 red on any registry package's latest **tag** (owner decision 1). A
  released package is code users already depend on, so a toolchain release that
  breaks it does not ship. The way out is a toolchain fix, or a package
  release that triage classifies `downstream`, followed by a re-run. The
  owner's manual waiver in step 5 remains the only override. A red only on a
  package's default branch is filed but does not block.
- a red on a target the test declares as best-effort does **not** block until
  it graduates. J5 on Windows starts best-effort and becomes blocking after
  about two weeks of consecutive green nightlies (owner decision 5). The owner
  approves the graduation as a governance-rule-2 change.

**What never blocks:** `infra` failures. These get one job re-run, then go to
the owner.

**`cross-repo.yml` is retired** once J9 covers beamtalk-http on edge. It is a
from-source subset of J9 with the same post-merge latency.

### 11. Failure classes and triage

| Class | Meaning | Files to |
|-------|---------|----------|
| `toolchain` | Beamtalk did the wrong thing (the default, per rule 1). | Linear `BT` Bug |
| `performance` | A p75 over its threshold. | Linear `BT` Bug, `performance` label |
| `security` | A Layer 3 distribution-security or leakage red. | Linear `BT` Bug, priority 2; details kept out of public logs |
| `downstream` | J9 only: the package's code is wrong. | the package's repository, linked from a `BT` comment |
| `docs` | A test needed something undocumented. | Linear `BT` Bug, area `docs` |
| `infra` | The runner, network or GitHub failed. | nothing; one re-run, then the owner |
| `harness` | uat's own bug. | a beamtalk-uat issue |

`triage.yml` is modelled on beamtalk's `ci-triage.yml`. It fingerprints each red
as `(layer, test, step, target, normalised first error line)`. It then comments
on an open `BT` issue carrying that fingerprint, or creates a Bug with:

- the labels the current uat `CLAUDE.md` requires;
- the exact version and sha256;
- the repro line;
- the artifact link.

**Secrets (owner decision 4).** beamtalk-uat holds its own Linear API key and
Claude OAuth secret, scoped to that repository and used only by `triage.yml`.
As in beamtalk's `ci-triage.yml`, they are exposed only to the triage job, and
only for runs of uat's own `main` or schedule, never for PR or fork runs. The
triage agent's tools are read-only plus Linear, and it treats logs as untrusted
data.

There are no automatic retries inside a run. A test that fails and then passes
on a rerun is filed as `flaky`: in a shipped toolchain that is a user-visible
defect until shown to be harness noise.

### 12. Platform matrix and cost

| Layer / journey | linux-x86_64 | macos-arm64 | macos-x86_64 | windows-x86_64 | Est. Linux (warm) |
|-----------------|:-:|:-:|:-:|:-:|--:|
| Command contracts (21 commands) | ✓ | ✓ | ✓ | ✓ | 6 min |
| J1 Install, J2 Lifecycle | ✓ | ✓ | ✓ | ✓ | 5 min |
| J3 Registry + HTTP | ✓ | ✓ | ✓ | ✓ | 6 min |
| J4 Releases | ✓ | ✓ | ✓ | ✓ (TLS later) | 5 min |
| J5 Cluster | ✓ | ✓ | ✓ | best effort, blocking after ~2 weeks green | 5 min |
| J6 Live dev | ✓ | ✓ | ✓ | ✓ (ConPTY) | 6 min |
| J7 LiveView | ✓ | ✓ | ✓ | — (ADR 0091) | 7 min |
| J8 Upgrade | ✓ | ✓ | ✓ | ✓ | 7 min |
| J9 Canaries (11 packages) | ✓ | weekly | weekly | weekly | 25 min (≈7 min wall over 4 shards) |
| Adversarial (8 categories) | ✓ | ✓ | ✓ | ✓ (signals via process kill) | 12 min |
| Perf gates (14 measurements × 7 runs) | ✓ | ✓ | ✓ | ✓ | 20 min (dedicated job) |

macOS and Windows runners are slower: budget 1.5× and 2×.

| Run | Runner-minutes | Wall time | Notes |
|-----|---------------:|----------:|-------|
| Per edge publish | ≈ 70 | ≈ 12 min (4 Linux shards, 1 macOS leg in parallel) | After merge, so no PR wall time. Linux ≈ 45 (≈ 30 journeys + J9, ≈ 15 contracts + fast adversarial). macos-arm64 fast set ≈ 25 (≈ 16 Linux-equivalent × 1.5). Plus the macOS edge *build* in beamtalk's `edge.yml`, ≈ 15 to 25 with a warm cache. |
| Nightly | ≈ 330 | ≈ 60 min | ≈ 200 for functional layers; ≈ 50 Linux + 75 macOS×2 + 40 Windows ≈ 130 for perf (the most expensive line, because of macOS). |
| Release candidate | ≈ 330 | ≈ 60 min to promotion | Same as nightly, blocking. |
| Weekly on `latest` | ≈ 330 | ≈ 60 min | Includes J9 on macOS and Windows. |

**BT-5273 lessons, applied:**

- **No compiled harness.** `npm ci` from a cache replaces the 12 to 15 minute
  cold cargo build.
- **Explicit, tested cache keys:**
  - the bundle by `version + sha256`;
  - npm by the lockfile;
  - Playwright browsers by the Playwright version;
  - J9 deps by the index HEAD.
- **Cold where the user is cold.** J3, `compile.cold` and `registry.resolve`
  always run cold.
- **Only `main` and scheduled runs save caches** (the `save-if` shape), so PR
  runs can't evict them.
- **Job timeouts come from measurement:** the measured cold p95 plus 50%. Step
  hang guards fire well inside that, so the failure bundle is always written.
- **Pinned runner images**, matching the ones `release.yml` builds on.

### 13. LiveView feasibility

Feasible, with one beamtalk change:

- **The artifact exists.** `liveview-release.yml` builds ERTS-embedded IDE
  archives for linux-x86_64, macos-arm64 and macos-x86_64 on tagged releases.
  `bin/server <workspace-id>` attaches to a local workspace
  (`docs/deployment/remote-liveview-ide.md`).
- **The browser side is proven.** `liveview-e2e.yml` already drives the IDE with
  PhoenixTest and Playwright, from source. J7 is the same interaction from
  published artifacts.
- **The gap:** IDE archives are not published to `nightly` or `edge`. J7 gates
  release candidates from day one. Nightly coverage needs `nightly.yml` to
  publish IDE archives, an S-sized beamtalk change.
- **The Docker image** (`ghcr.io/jamesc/beamtalk-ide`) is a second, Linux-only J7
  variant, run before the image is pushed as part of promotion.

### 14. What happens to the 16 current scenarios

Their contracts move:

- `cli_*` into Layer 1, now schema-checked;
- `lsp/*` into J6, against a real workspace;
- `mcp_*` into J6's MCP flow;
- `smoke` into J2.

The rewrite PR lists the mapping, and no contract is dropped silently.

## Prior Art

- **swamp-uat** (the model, § Context). Adopted:
  - the separate repository and per-release dispatch;
  - shipped-artifact-only testing per target;
  - one file per command with global flags;
  - schema-validated JSON;
  - the seven adversarial categories;
  - 2+5 iterations gated on p75 with a CI multiplier;
  - "tests are more accurate than the code";
  - permanent tests for regressions;
  - promotion blocking.
  Extended:
  - cross-surface journeys, because Beamtalk's user surface includes long-lived
    nodes, clusters, an IDE and an agent protocol, not only a CLI;
  - a downstream canary layer;
  - mechanical enforcement of the governance rules.
  Changed: the runner (Playwright Test, for multi-process steps and the browser),
  and edge runs are non-blocking because edge is a per-merge build, not a
  promotion candidate.
- **Rust's crater** builds and tests the crates.io ecosystem against a candidate
  compiler. J9 is a small crater, with the registry index as the corpus.
- **Phoenix's installer integration tests** generate a fresh project, fetch deps
  from Hex and run its tests. J2 and J3 share that motivation.
- **Kubernetes conformance and e2e** use tag-based tiers and collect environment
  logs on failure. Adopted, in § 5's failure bundle.
- **Erlang/OTP `common_test` with `peer`.** Considered as the runner. Adopted
  from it: unique node names and cookies per test, and no node outliving its
  test.
- **Smalltalk family.** Pharo CI loads baselines into a fresh image from a
  published VM. The journey model goes further, because Beamtalk's user surface
  includes OS processes and nodes outside any image (principles §4).

## User Impact

- **Newcomer.** J1 to J3 and the command contracts are their first hour, checked
  on their target before they can install the release.
- **Smalltalk developer.** J6 and the hot-reload-under-load test check the
  promise they came for: a running actor keeps its state across an edit, even
  under traffic.
- **Erlang/BEAM developer.** J4, J5 and the distribution-security tests check
  releases, `rpc`, TLS and cookies the way OTP teams deploy, including on macOS.
- **Production operator.** Releases arrive promoted only after the launcher,
  upgrade path, leak checks and `release.start` timing have passed on their
  target. Secret-leakage tests cover their logs and crash dumps.
- **Tooling developer and agent user.** MCP through the official SDK, LSP
  through VS Code's library, both against a real workspace; the IDE as shipped.
- **Package authors.** A toolchain change that breaks their package goes red on
  the beamtalk side within the hour.
- **Beamtalk contributors.** There is no added PR time without `ci:uat`. In
  exchange, an edge red files a Bug within the hour, and the governance rules
  mean uat can't be "fixed" by loosening it.

## Steelman Analysis

### Evolve the current Rust harness (add cross-surface `[[journey]]` to `expect.toml`)
- 🧑‍💻 **Contributor:** "It exists and is green. Extend it rather than relearn."
- ⚙️ **BEAM veteran:** "Rust matches beamtalk's tooling, and hand-rolled clients
  test framing exactly."
- 🎨 **Designer:** "Declarative TOML is data that a non-programmer can add."
- **Response:** TOML is the "arg parsing" ceiling. Journeys, adversarial timing
  and percentile benchmarks need control flow, and a TOML interpreter for that is
  a test framework written badly. Rust also brings back the BT-5273 cold-build
  cost. The framing argument is answered in § 4.

### Fold the journeys into beamtalk's own e2e suite
- 🧑‍💻 **Contributor:** "One repository, and the failing journey sits next to the
  code that broke it."
- 🏭 **Operator:** "Per-PR is earlier than per-merge."
- **Response:** in-repo tests build from source by construction, so they can't
  see packaging, the installer, published IDE archives, release-build settings
  or the registry, which is where these regressions surfaced. The article's core
  claim is independence. Per-PR stays available through `ci:uat`.

### Keep uat as a smoke test
- 🏭 **Operator:** "A small gate is a reliable gate. Flaky gates get ignored."
- **Response:** every 2026-11-26 miss was cross-process or cross-repository.
  Flakiness is handled by failure classes, fingerprinting, perf only on dedicated
  jobs, and blocking only on release candidates.

### Write the harness in Beamtalk (dogfood)
- 🎩 **Smalltalk purist:** "`Subprocess`, `ReactiveSubprocess` and BUnit exist.
  A language that can't test its own journeys isn't ready."
- **Response:** the judge must not share a failure mode with the accused, and
  the article makes independence its point. Dogfooding happens in the fixtures.

### Erlang `common_test`
- ⚙️ **BEAM veteran:** "Native `peer` nodes, hooks and logs. Multi-node is
  trivial."
- **Response:** strong for J4 and J5, but it has no browser, MCP or LSP client,
  and Windows pty and installer driving is awkward. One runner for all layers
  matters more.

### TypeScript on Deno with Zod (swamp's exact stack)
- 🎨 **Designer:** "It's proven for this exact job, with no `node_modules`, and
  permissions are sandboxed by default."
- **Response:** a close second, and Zod is kept. Playwright Test wins on built-in
  step reporting, per-step timeouts, traces, sharding and the browser that J7
  needs. Deno would need Playwright bolted on anyway.

### Python, pytest and playwright-python
- **Response:** defensible. It loses on built-in reporting and on alignment with
  the official MCP SDK and beamtalk's existing TypeScript tests (VS Code,
  LiveView).

### Tension points
- Contributors want per-PR coverage, and operators want a reliable gate. The
  ADR's answer: edge runs after merge, and per-PR runs are opt-in.
- Perf gates on shared runners against strict thresholds. The ADR's answer:
  dedicated jobs, a CI multiplier, calibration before gating, and owner-only
  threshold raises.
- Real clients against hand-rolled ones. The ADR reverses the old choice for the
  acceptance layer only.

## Alternatives Considered

Each is steelmanned above:

- **Evolve the current repo:** rejected. TOML can't express the layers, and the
  Rust harness carries the cold-build cost.
- **Fold into beamtalk e2e:** rejected. It can't test published artifacts. Kept
  as `ci:uat`.
- **Smoke only:** rejected. It misses every 2026-11-26 incident.
- **Harness in Beamtalk:** rejected for the judge, adopted for the fixtures.
- **`common_test`:** rejected. No browser, MCP or LSP clients.
- **Deno with Zod:** a close second. Zod is adopted, the runner is not.
- **pytest:** rejected on reporting and client alignment.
- **bats or shell:** can't hold a pty, an MCP client and a browser in one
  journey.

## Consequences

### Positive
- The 2026-11-26 incident classes become reds before users see them.
- Releases reach users, the Marketplace and ghcr only after all four layers pass
  on every target.
- User-level performance regressions (startup, packaging, release-build
  settings) get a gate. Until now only micro-benchmarks existed.
- Adversarial coverage of cookies, TLS, secrets, locks and atomic writes exists
  for the first time against shipped artifacts.
- The suite compounds: every escaped regression becomes a permanent test.
- The governance rules are enforced by CODEOWNERS and a CI check, not trust.
- There is no compiled harness, so no cold-cache timeouts in uat.

### Negative
- A rewrite. The current harness and its 16 scenarios are discarded, though
  their contracts survive.
- A TypeScript codebase to maintain. VS Code and LiveView tests already use
  TypeScript.
- Real-network tests depend on GitHub. `infra` classification limits the damage
  but cannot remove it.
- Ongoing cost is about 330 runner-minutes per nightly, weekly and
  release-candidate run, with macOS perf the largest line. Each edge publish
  costs about 70 runner-minutes, plus a macOS edge build in beamtalk.
- Releases are delayed by about 60 minutes, longer when red. Waivers are
  owner-only.
- Because canaries block promotion, a released satellite package that breaks
  under a correct toolchain change holds up the toolchain release. Its maintainer
  then has to ship a package release, or the owner has to waive.
- Strict governance slows legitimate contract changes: each needs the owner's
  explicit approval.
- Official client libraries can't see a framing bug every real client
  tolerates. This is intended; framing is covered in-repo.

### Neutral
- `cross-repo.yml` is retired in favour of J9.
- `release.yml` moves from "publish then notify" to "pre-release, gate, promote",
  and the VS Code, IDE, Docker and desktop publishes move behind `promote`.
- `edge.yml` gains a `macos-arm64` build. `edge.yml` and `nightly.yml` gain a
  dispatch.
- Micro and dispatch benchmarks and coverage stay in beamtalk, unchanged.

## Implementation

Phases are ordered by value per unit of effort. Each ends green on its targets
and ships independently.

| Phase | Scope | Size | Why this order |
|-------|-------|------|----------------|
| **0. Skeleton and governance** | In beamtalk-uat: Playwright Test project; `env`, `toolchain`, `proc` and `artifacts` libraries; Zod `schemas/`; teardown and leak checks; failure bundle; 4-target workflow; new `CLAUDE.md` rules; CODEOWNERS; `governance.yml`. **J1**, **J2**, and Layer 1 for the 7 commands today's scenarios cover. Delete the Rust harness. In beamtalk: `edge.yml` builds `macos-arm64` as well as linux, and `edge.yml` and `nightly.yml` dispatch with the § 3 payload. uat: repo-scoped Linear and Claude secrets. | M | Everything builds on it, and governance must exist before the suite grows. |
| **1. Canaries on edge, and triage** | **J9** sharded over the registry index; `uat-edge` status; `triage.yml` with fingerprints and classes. Retire `cross-repo.yml`. Permanent tests for BT-5283 (in J9) and BT-5264 (a macOS J4 step). | M | Highest value per effort: J9 would have caught BT-5283 within the hour. |
| **2. All command contracts; registry and releases** | Layer 1 for the remaining 14 commands. **J3** (buccaneer from the real registry) and **J4** (verbs, TLS on Linux and macOS, `--upgrade-from`, `.cmd` launchers). | M | Covers the newcomer's first-service path and the BT-5264 class. |
| **3. Adversarial layer** | Layer 3, all eight categories (§ 7), with the fast subset on edge. | M | Cheap once Phase 2's release and node handles exist. Security findings are high-severity. |
| **4. Performance gates** | `lib/bench.ts` (2 warmup + 5 measured, p50/p75/p95, multiplier); the 14 measurements of § 8 on dedicated jobs; 10 nightly report-only runs; owner sets `thresholds.toml`; gating turned on. | M | Calibration needs real nightly data, so it starts early enough to finish before Phase 7. |
| **5. Live development** | **J6**: REPL through a pty (ConPTY on Windows) with hot reload of a running actor; MCP flow through the SDK; LSP in workspace mode. | M | Interactive-first is principle #1. |
| **6. Cluster, upgrade, LiveView** | **J5**, **J8**; beamtalk publishes IDE archives on `nightly` (S); `ide.ts` and **J7** with Playwright, plus the Docker variant. | L (+S in beamtalk) | The most harness work, and J7 needs the pipeline change. |
| **7. Blocking promotion** | beamtalk: tags publish a pre-release; `promote` job on `uat-gate: success` marks it latest; the triggers of `release-vscode.yml`, `liveview-release.yml`, `liveview-docker.yml` and `desktop-release.yml` move from `release: published` to `promote`; owner waiver path. J5 on Windows graduates to blocking once two weeks green. `ci:uat` label calling uat's reusable workflow with a PR-built bundle. | M | Blocking only after phases 0 to 6 have a few weeks of measured flake rate and calibrated thresholds. |

**Affected components:**

- **beamtalk-uat:** everything.
- **beamtalk:**
  - `edge.yml` (a `macos-arm64` build; dispatch), `nightly.yml` (dispatch; IDE
    archives on nightly);
  - `release.yml` (pre-release, then promote) and `release-vscode.yml`,
    `liveview-release.yml`, `liveview-docker.yml`, `desktop-release.yml`
    (publication triggered from `promote`);
  - `cross-repo.yml` (removed);
  - `docs/development/testing-strategy.md` (new § "Acceptance gate
    (beamtalk-uat)"; Cross-Repo section replaced);
  - `docs/development/releasing.md` (promotion);
  - `docs/development/benchmarks.md` (which benchmarks live where);
  - `AGENTS.md` (`ci:uat`).
- **No compiler, runtime or stdlib changes.** Findings are filed and fixed under
  their own issues.

## Migration Path

There are no user-facing changes. For maintainers:

- The old Rust harness runs until J1, J2 and the Phase 0 contracts are green on
  all four targets against `latest`. It is then deleted in the same PR that
  switches `uat.yml`.
- Each old scenario's contract maps to a named test, listed in the rewrite PR.
- `cross-repo.yml` is deleted after J9 has covered beamtalk-http on edge for a
  week with no gaps.
- Perf gates run report-only until thresholds are calibrated (Phase 4), so
  turning them on cannot block a release on day one.

## Owner Decisions (2026-11-26)

These were raised as open questions in the first draft and settled by the owner.
They are folded into the Decision sections above:

1. **Canary blocking:** yes. A J9 red on a package's latest tag blocks
   promotion (§ 10).
2. **Pre-release, then promote:** yes. `release-vscode.yml`,
   `liveview-release.yml`, `liveview-docker.yml` and `desktop-release.yml`
   publish from the `promote` job, not on `release: published`. Editors and the
   IDE ship only after uat passes (§ 10, Phase 7).
3. **Edge on macOS:** yes. `edge.yml` also builds `macos-arm64`, and uat runs the
   fast set on it for every edge publish (§ 3, § 10, § 12).
4. **Triage secrets:** beamtalk-uat holds its own Linear and Claude secrets,
   scoped to that repository and to the triage job (§ 11).
5. **J5 on Windows:** best-effort at first. It becomes blocking after about two
   weeks of green nightlies, with owner approval (§ 10, § 12).

The user-level performance gates (§ 8) and the split that leaves micro and
dispatch benchmarks and coverage in beamtalk are also owner decisions
(§ Context, Constraints).

## References

- Related issues: BT-2253 (uat epic), BT-2449 (release → uat dispatch), BT-2453
  (`uat-gate` status), BT-2476 (scaffold fmt-clean), BT-2480 (`[[scenario]]` /
  `[[step]]`), BT-2497 (skip when a rolling pre-release is missing), BT-5066
  (foreground REPL cookie), BT-5283 and BT-5290 (return-type check broke
  downstream), BT-5264 (macOS epmd shadowing), BT-5233 and BT-5256 (TLS
  distribution), BT-5273 (platform-check cold-cache timeouts)
- Related ADRs: ADR 0014 (test framework), ADR 0073 (package distribution), ADR
  0091 (remote LiveView IDE), ADR 0125 (OTP releases and upgrade compatibility),
  ADR 0126 (location-transparent actors)
- Documentation: `docs/development/testing-strategy.md` (§ Cross-surface Parity,
  § LiveView IDE Tests, § Cross-Repo Package Tests), `docs/development/deploying.md`,
  `docs/development/releasing.md`, `docs/development/benchmarks.md`,
  `docs/beamtalk-packages.md` (§ Package Registry), `docs/beamtalk-installation.md`,
  `docs/deployment/remote-liveview-ide.md`
- External: Paul Stack, [*The Gate Between Our Agent Code and Our
  Users*](https://stack72.dev/the-gate-between-our-agent-code-and-our-users/)
  (swamp-uat); beamtalk-uat `CLAUDE.md`; [Playwright Test](https://playwright.dev/docs/test-intro);
  [Zod](https://zod.dev); Rust's crater
