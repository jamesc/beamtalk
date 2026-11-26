# ADR 0154: beamtalk-uat as the External Acceptance Gate for Full Environments

## Status
Proposed (2026-11-26)

## Context

### What beamtalk-uat is for

[beamtalk-uat](https://github.com/jamesc/beamtalk-uat) is a separate repository
that consumes Beamtalk the way a user does. It installs a *published* toolchain
bundle (`latest`, `edge`, `nightly` or `vX.Y.Z`) and drives it, following the
Swamp "gate" pattern. Its `CLAUDE.md` sets three rules, and this ADR keeps all
three:

1. The released bundle is the artifact under test, not source.
2. When a scenario fails, the default assumption is that the toolchain must
   change, not the scenario.
3. Every red result files a Linear Bug.

The rules are right. What the repo does today does not live up to them.

### What it does today

The harness is about 2,800 lines of Rust (`src/lib.rs`, `src/scenario.rs`,
`src/lsp.rs`, `src/mcp.rs`). It auto-discovers `projects/<name>/expect.toml` and
runs one *surface* per scenario. There are 16 scenarios:

| Kind | Count | What it checks |
|------|-------|----------------|
| `cli_*` | 12 | Exit code and a stdout/stderr substring for `new`, `new --app`, `fmt-check`, `lint`, `lint --format json`, `type-coverage`, `--help` and `build`. Only `cli_build` compiles anything. |
| `lsp/*` | 1 fixture, 4 capabilities | `hover`, `completion`, `definition` and `documentSymbol` against the server in AST mode. There is no workspace and no BEAM. |
| `mcp_*` | 2 | A single `evaluate` call each. |
| `smoke` | 1 | `beamtalk test` on a one-class project. |

`[[step]]` sequences exist, but only within one session-backed surface (one LSP
or one MCP process). A scenario cannot start a release and then `rpc` into it,
start two nodes, or edit a file and watch the REPL pick it up. As the owner puts
it, this is "more like arg parsing" than acceptance testing.

It runs on `repository_dispatch` from `release.yml`, on a nightly cron against
`nightly`, and per uat PR against `edge`. It covers four platforms. It posts a
non-blocking `uat-gate` commit status on the release tag. Each leg compiles the
Rust harness first, so a cold cargo cache costs the same 12 to 15 minutes on
macOS and Windows that BT-5273 measured in beamtalk's own `platform-check`.

### What got through on 2026-11-26

All of these reached users, or would have, and none could have been caught by
the current scenarios:

- **BT-5283 broke downstream builds.** The return-type check on `^` early returns
  merged at 13:45Z. Within the hour, beamtalk-mint's CI failed
  `--warnings-as-errors` with 13 rejected methods. catalina was hit the same way.
  The rejected shape was the `x isError ifTrue: [^x]` error-propagation idiom,
  which also appears in buccaneer's own `Buccaneer start:`. BT-5290 fixed it the
  same day. Nothing in either repository builds the satellite packages against
  the toolchain before release. `cross-repo.yml` builds only beamtalk-http, and
  from source.
- **BT-5264: macOS epmd shadowing.** On macOS, a loopback client verb (`ping`,
  `stop`, `rpc`) started a second epmd on `127.0.0.1` beside a wildcard TLS
  node's epmd. Every later local lookup then reported the node as down. Linux
  refuses that bind, so Linux-only tests cannot see the problem. Someone found it
  by hand while working on BT-5256.
- **Release-launcher issues** around TLS distribution (BT-5233 and its follow-ups)
  were also found by hand. In-repo tests cover the launcher script against a
  source build. Nothing runs `beamtalk release` from an installed bundle, starts
  the result and talks to it.
- **Cold-cache timeouts (BT-5273).** `platform-check` hit its 40-minute limit on
  Windows and macOS because a cold Rust build alone took 12 to 15 minutes. uat's
  own legs have the same exposure, because the harness is Rust.

### What already exists, so this ADR doesn't duplicate it

| Existing suite | Builds from | Answers |
|----------------|-------------|---------|
| Rust, EUnit, stdlib and BUnit tiers (`just test`) | source | Is each unit correct? |
| REPL protocol tests (`tests/repl-protocol`) | source | Does the workspace protocol behave? |
| Cross-surface parity (`tests/parity`, BT-2077) | source | Do REPL, MCP, CLI and LSP agree on the same input? |
| `examples/cluster/scripts/e2e.sh` (`just test-cluster`) | source | Do ADR 0126 location-transparent actors work across three local nodes? |
| `liveview.yml`, `liveview-e2e.yml` | source | Does the IDE work against a workspace built from the same commit? |
| `cross-repo.yml` | source | Does beamtalk-http still build and test against main? |
| `ci-platforms.yml` | source | Do the tests pass on macOS and Windows? |

Every one of these builds Beamtalk from the same commit it tests. None of them
answers the question a user faces: *with only the published artifacts, the
public registry and my operating system, can I get from install to a running
service?* That question includes everything between the source tree and the
user: `scripts/package-release.sh`, `install.sh`, the archive layout, ERTS and
OTP version skew, the registry index, the separately released IDE archive, and
the satellite packages' own `--warnings-as-errors` builds.

### Constraints (owner decisions)

- Everything runs in CI, on Linux, macOS and Windows wherever the feature exists
  on that platform.
- The **real package registry** (`github.com/jamesc/beamtalk-registry`, and the
  package repositories it points to) is in scope. A mirror is not.
- **LiveView is in scope** if feasible. This ADR finds that it is (see § LiveView
  feasibility).
- uat may be redesigned from scratch. The current harness, `expect.toml`,
  surface types and per-scenario layout carry no weight.

## Decision

### 1. Purpose: one sentence and one rule

**beamtalk-uat proves that a user, holding only published artifacts, can
complete each supported journey on each supported platform.**

**The user-only rule:** a journey may use only what a user has:

- the published bundle, installed the documented way (`install.sh`, or the zip on
  Windows);
- the published IDE archive;
- the public registry and public package repositories;
- documented commands, flags and environment variables;
- the OS's own tools.

Journeys never check out beamtalk source, never set an undocumented environment
variable, and never call an internal Erlang function. If a journey needs one of
those, that is itself a finding: either a docs bug or a missing feature, filed as
such.

This rule draws the line against the in-repo suites. Those suites answer "is the
code right?" uat answers "does the shipped thing work for a user?" A behaviour
already pinned by a fast in-repo test is not re-asserted in uat at finer grain.
uat asserts only that the journey completes and that the user sees the right
result.

### 2. Clean rewrite: TypeScript journeys on the Playwright Test runner

beamtalk-uat is rewritten. The Rust harness, `expect.toml`, the surface types and
`projects/<scenario>/` are deleted. What carries over is the repository, the
three rules above, the version selector (`latest | edge | nightly | vX.Y.Z |
local:<path>`), the `repository_dispatch` and commit-status wiring, and the
fixture *contracts* worth keeping. For example, BT-2476's "a fresh `new` project
is fmt-clean" becomes one step of J2. Its fixtures do not carry over.

The harness is **TypeScript on [Playwright Test](https://playwright.dev/docs/test-intro)**,
used as a general test runner rather than only as a browser tool. It was chosen
on merit, because it provides out of the box the things a journey gate needs
most:

| Need | Playwright Test gives |
|------|-----------------------|
| Ordered journeys with per-step reporting | `test.step()` nests and shows in every reporter. A red result reads "J4 › rpc into release › step 5/9". |
| Time budgets | Per-test and per-step timeouts (`test.setTimeout`, `step` with `{ timeout }`), plus a global time limit. |
| Diagnosable failures | `testInfo.attach()` for logs, files and tarballs. The HTML report, JUnit XML and traces upload as one artifact. |
| Tiers and platform selection | Tags (`@gate`, `@nightly`, `@macos-only`) with `--grep`, and `projects` for tiers. |
| Isolation and teardown | Fixtures with guaranteed teardown, which is where process-tree kill and leak checks go. |
| Parallelism and sharding | `--workers` and `--shard=i/n` across runners. |
| LiveView | Native Chromium, Firefox and WebKit, already proven in beamtalk's `liveview-e2e.yml`. |
| Cross-platform | Node runs the same on all three OSes. `npm ci` with a cache takes seconds, against 12 to 15 minutes for a cold cargo build. |

**Use real clients.** The MCP steps use the official MCP TypeScript SDK client.
The LSP steps use `vscode-jsonrpc` / `vscode-languageclient`. These are the
libraries Claude Code, other agents and VS Code actually use. The current harness
hand-rolls both clients on purpose, so that a library cannot hide a framing bug.
For an acceptance gate that reasoning is backwards: if a framing bug hides from
the client every user runs, it is not a user-facing bug. If it breaks that
client, uat should go red. Byte-level framing conformance belongs to the in-repo
`tests/parity` and protocol suites, which already have it.

**Fixtures stay Beamtalk.** The apps under test are real Beamtalk projects in
`fixtures/`, written in Beamtalk and created or overlaid by journeys. In-app
assertions use BUnit where that is natural. The *judge*, the code that decides
pass or fail, is not Beamtalk, so a broken toolchain cannot take down its own
verdict (§ Alternatives, "Write the harness in Beamtalk").

### 3. Journeys, not commands

A **journey** is an ordered sequence of steps across any surfaces and processes
that share one **environment**. An environment has:

- an isolated `HOME`/`USERPROFILE` and install prefix;
- a port allocator;
- a shared registry cache (`BEAMTALK_REGISTRY_CACHE_DIR`, a documented variable)
  where the journey allows it;
- a process supervisor that owns every process the journey starts.

A step fails fast. Later steps report `skipped (blocked by step N)`, so one red
journey yields one root cause and not nine.

Repository layout:

```text
beamtalk-uat/
├── journeys/                 # one file per journey, J1 to J9
│   ├── j1-install.journey.ts
│   ├── j2-project-lifecycle.journey.ts
│   └── …
├── fixtures/                 # real Beamtalk projects overlaid by journeys
│   ├── hello_web/            # buccaneer HTTP app (J3)
│   ├── counter_live/         # actor for hot reload (J6)
│   └── farm_cluster/         # two-node app (J5)
├── lib/                      # the harness
│   ├── env.ts                # isolated HOME, PATH, ports, tmp, teardown + leak check
│   ├── toolchain.ts          # install via install.sh / zip, version selector
│   ├── proc.ts               # spawn, readiness, transcript capture, tree-kill
│   ├── repl.ts               # `beamtalk repl` through a pty (node-pty / ConPTY)
│   ├── mcp.ts  lsp.ts        # thin wrappers over the official clients
│   ├── release.ts  node.ts   # release launcher verbs, BEAM node handles
│   ├── ide.ts                # IDE archive install and start
│   └── artifacts.ts          # per-step transcript and failure bundle
├── playwright.config.ts      # tiers, platform tags, reporters, global timeout
└── .github/workflows/        # gate.yml, nightly.yml, canaries.yml, triage.yml
```

A journey reads as the user's story. Here is J3, the registry dependency serving
HTTP:

```ts
// journeys/j3-packages.journey.ts
import { journey, expect } from '../lib';

journey('J3 registry package serves HTTP', {
  tier: 'gate', budget: '8m', platforms: ['linux', 'macos', 'windows'],
}, async ({ env, bt, step }) => {
  const app = await step('new --app', () => bt.newProject('hello_web', { app: true }));

  await step('deps add buccaneer from the real registry', async () => {
    await bt.in(app).run(['deps', 'add', 'buccaneer']);
    const lock = await app.lockfile();
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

The fixture is ordinary Beamtalk that uses buccaneer's documented API:

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

**Assert on structure where the toolchain offers it.** That means `--format
json`, MCP structured content, LSP result objects, HTTP status codes and
`Beamtalk releaseInfo` through `rpc`. Free text is asserted only where the text
is the contract, and loosely. uat never pins REPL display formatting or prompts:
those are intentional and owned by beamtalk's e2e tests (AGENTS.md, "REPL
output"). REPL steps assert the value, never the layout.

**Every step records a transcript:** the argv, cwd, the environment diff from
the base env, stdout, stderr, the exit code and the wall time. On failure the
environment's **failure bundle** is attached:

- the transcripts of every step so far;
- `~/.beamtalk/workspaces/*/` logs, release `log/` directories and `erl_crash.dump`;
- `epmd -names` and the live process tree before teardown;
- the installed bundle's `beamtalk --version` and `beamtalk doctor` output;
- the Playwright trace (J7);
- a one-line repro: `BEAMTALK_UAT_VERSION=<exact version> npx playwright test
  journeys/j4-release.journey.ts -g "rpc into release"`.

**Teardown is checked.** After every journey the environment asserts that no
process it started is still alive and that no epmd registration it caused is
left. A leak is a red step ("process `orders` survived `bin/orders stop`"), not
silent cleanup. The current harness can never see this class of bug.

### 4. The journey catalogue

| # | Journey | Steps (abridged) | Catches |
|---|---------|------------------|---------|
| **J1** | Install | `install.sh` (Linux and macOS) or zip plus `PATH` (Windows) into a clean `HOME`; `beamtalk --version` matches the version under test; `beamtalk doctor` is healthy; `--uninstall` removes it. | Packaging, checksums, archive layout, a missing ERTS or OTP app. |
| **J2** | Project lifecycle | `new` → fmt-check clean → `build` → `test` → edit → `lint` finds the edit → `fmt` fixes it → `run` prints the expected output → `new --app` → `run .` → Ctrl+C exits 0. | Scaffold drift (BT-2476), the build cache, `run` output routing and exit codes. Absorbs today's 12 `cli_*` scenarios. |
| **J3** | Registry packages | `deps add` from the real registry → lockfile pins the transitive closure → build `--warnings-as-errors` → `run .` serves HTTP through buccaneer → `deps update` → a second build reuses the cache. | Registry index resolution, git dependency checkout, the type checker against real packages. |
| **J4** | Releases | `release` → `bin/<n> start` → `ping` → `rpc "Beamtalk releaseInfo"` → `stop` → leak check; a TLS distribution variant (`RELEASE_DISTRIBUTION=tls`, with a generated CA) with a loopback client on the same host; `--upgrade-from` reports a shape bump with no migration as an error. | Launcher verbs, BT-5264-class epmd problems, TLS preflight, `.cmd` launchers on Windows. |
| **J5** | Two-node cluster | Two releases of `farm_cluster` → connect → named send across nodes → class reference crosses the wire → hot reload on one node → skew is announced → kill a node → the survivor sees `NodeDown`. | ADR 0126 behaviour from installed artifacts, not a source tree. |
| **J6** | Live development | REPL: spawn a `Counter`, send it messages, edit the source on disk, `:sync`; the same actor keeps its state and runs the new code. MCP (official SDK): `evaluate` → `save_method` → `test` → `flush`, and the change is on disk. LSP: open the project with a workspace, diagnostics appear for a type error, and go to definition on a dependency's class. | Hot reload, flush to disk, the LSP workspace mode that today's AST-only check skips. |
| **J7** | LiveView IDE | Install the IDE archive for the same version → `bin/server <workspace-id>` → Playwright: page loads → eval round-trips → System Browser shows the project's class → edit a method → the workspace sees it. | Bundle/IDE version skew, release-archive packaging, the Attach topology from published artifacts. |
| **J8** | Upgrade path | Install the previous release → build a project and a release → install the candidate over it → the same project builds, tests and releases → `--upgrade-from <old release>` is clean → the old `~/.beamtalk` cache doesn't break the new toolchain. | Cache and lockfile compatibility across versions, stale state left by installs. |
| **J9** | Downstream canaries | For each package in the registry index: check out its default branch and its latest tag → `beamtalk test --warnings-as-errors` against the candidate. | BT-5283 / BT-5290: a toolchain change that breaks real downstream code. |

**J9 is special in two ways.** It reads the package list from the registry
index, so a newly published package is covered without editing uat. And a red
result names the first `(package, file, diagnostic)` triple, so triage can tell
a toolchain regression from a package bug. Today the index lists agents,
announcements, buccaneer, catalina, collections, hpack, http, linear, mint, oauth
and yaml.

### 5. Harness capabilities, made concrete

- **Cross-surface, cross-process steps.** All handles (`Proc`, `Repl`,
  `McpClient`, `LspClient`, `ReleaseNode`, `IdePage`) live in one environment, so
  a later step can observe any earlier step's effect.
- **Multi-node.** `node.ts` allocates unique `-sname`s and cookies per journey,
  so parallel journeys on one runner never collide. It also captures and resets
  `epmd` state, and it understands the documented release variables
  (`RELEASE_DISTRIBUTION`, `RELEASE_HOST`, `RELEASE_DIST_INTERFACE`,
  `RELEASE_SSL_DIST_OPTFILE`).
- **Network and registry.** J3 and J9 use the live registry and GitHub. A
  failure to reach github.com is classified as `infra` (the step names the host
  and the error), not `toolchain`, so an outage doesn't file a toolchain bug
  (§ 7).
- **Two kinds of time budget.**
  - The **hang guard** is a hard per-step timeout, so a hang fails as "step `rpc`
    exceeded 60s", not as a dead job.
  - The **user-facing budget** is a soft per-journey target, for example "`new`
    → first `build` under 90s cold on Linux". Exceeding it by 2× or more posts a
    warning in the summary and files a `performance` bug. It does not fail the
    gate until the owner promotes that budget to hard.
- **Platform tags.** Each journey and step declares where it applies.
  `platforms: ['linux', 'macos']` documents an intended gap; it does not hide a
  bug. A step that cannot run on a platform for a *product* reason (the IDE has
  no Windows build, per ADR 0091) says so in the tag's reason string, which the
  summary reports.

### 6. Gating: when it runs and what blocks

| Trigger | Version | Tier | Platforms | Blocks? |
|---------|---------|------|-----------|---------|
| Every `edge` publish (a merge to beamtalk `main`) | `edge` | `gate` (J1 to J4, J6) + J9 | Linux (edge is Linux-only) | No. Posts a `uat-edge` commit status on the merged commit; red files a bug within the hour. |
| Nightly | `nightly` | `full` (J1 to J9) | Linux, macOS arm64, macOS x86_64, Windows | No. Red files bugs. |
| Weekly | `latest` | `full` | all | No. Catches drift in the outside world: OS images, the registry, GitHub. |
| Release candidate | `vX.Y.Z` (pre-release) | `full` | all | **Yes.** Promotion to `latest` waits for green. |
| beamtalk PR, label `ci:uat` | PR-built Linux bundle | `gate` | Linux | Informational (Phase 6). |
| `workflow_dispatch` | any | any journey or tag | any | n/a |

**Release promotion.** Today `release.yml` publishes the release, uploads assets
and only then dispatches uat. The status it gets back arrives after users can
already install the release. The change: tag pushes publish the GitHub release
as a **pre-release**. `install.sh` resolves `releases/latest`, which GitHub
excludes pre-releases from, so users can't install it yet. uat runs `full` on
all platforms against the exact tag. On green, a `promote` job flips the
release to latest. On red, the release stays a pre-release, the bugs are filed,
and the owner either fixes it and re-tags, or promotes by hand with a recorded
waiver.

**What blocks promotion:**

- any red `gate` or `full` step on a platform the journey declares;
- a J9 canary red on a package's **latest tag**, unless triage classifies it as a
  package bug and the owner waives it. A red only on a package's default branch
  is filed but does not block, because unreleased package code is not yet the
  ecosystem users depend on.

**What never blocks:**

- `infra`-classified failures (these retry the job once, and if still red, ask
  the owner);
- soft budget overruns.

**From red to Linear.** `triage.yml` in beamtalk-uat is modelled on beamtalk's
`ci-triage.yml`. It runs on a red gate or nightly run and computes a fingerprint
`(journey, step, platform, normalised first error line)`. It then searches Linear
for an open `BT` issue whose body carries that fingerprint:

- If one exists, it adds a comment with the version, run link and artifact link.
- If none does, it creates a **Bug** with the labels the uat `CLAUDE.md` already
  requires (item area mapped from the journey, size `S` by default, `agent-ready`),
  the exact version under test, the repro line and the artifact link.

Fingerprinting keeps "every red files a bug" from turning into a bug per night
per platform. There are no automatic retries inside the gate. A step that fails
and then passes on a rerun is filed as a `flaky` bug: flakiness in a shipped
toolchain is a user-visible defect until proved to be harness noise, and harness
noise is filed against beamtalk-uat.

**beamtalk's own CI does not call uat on every PR.** The cost (§ 8) and the need
for a published artifact rule it out. The edge trigger already gives about
one-merge latency. Phase 6 adds an opt-in `ci:uat` label. With it, the PR builds
the Linux bundle with `scripts/ci/package-release.sh` and calls uat's
`gate.yml` as a reusable workflow with `version: local:<artifact>`. This is for
release-machinery and launcher PRs, the class that BT-5264 came from.

**`cross-repo.yml` is retired** once J9 covers beamtalk-http on edge. It is a
from-source subset of J9, and keeping both would duplicate the rule. J9 on edge
has the same post-merge latency.

### 7. Failure classes

Every red step carries one classification. Triage uses it, and the summary
groups by it:

| Class | Meaning | Files to |
|-------|---------|----------|
| `toolchain` | The toolchain did the wrong thing (the default). | Linear `BT` Bug |
| `downstream` | J9 only: a package's code is wrong under a correct toolchain change. | That package's repository, linked from a `BT` tracking comment |
| `docs` | The journey needed something undocumented (the user-only rule). | Linear `BT` Bug, area `docs` |
| `infra` | The runner, network or GitHub failed. The step names the host and errno. | Nothing. One job retry, then the owner. |
| `harness` | uat's own bug. | beamtalk-uat issue |

The class is set by the step when it can tell. For example, a network step that
gets ECONNREFUSED from github.com is `infra`. Otherwise triage sets it, defaulting
to `toolchain`, the "tests are more accurate than the code" rule.

### 8. Platform matrix and cost

| Journey | Linux | macOS arm64 | macOS x86_64 | Windows | Est. Linux (warm) | Notes |
|---------|:-----:|:-----------:|:------------:|:-------:|------------------:|-------|
| J1 Install | ✓ | ✓ | ✓ | ✓ (zip) | 1 min | |
| J2 Lifecycle | ✓ | ✓ | ✓ | ✓ | 4 min | |
| J3 Registry + HTTP | ✓ | ✓ | ✓ | ✓ | 6 min | Clones buccaneer, catalina and hpack. |
| J4 Releases | ✓ | ✓ | ✓ | ✓ (no TLS step at first) | 5 min | The TLS variant matters most on macOS (BT-5264). |
| J5 Cluster | ✓ | ✓ | ✓ | nightly, best effort | 5 min | Windows epmd and `-sname` are expected to work but are unproven. |
| J6 Live dev | ✓ | ✓ | ✓ | ✓ | 6 min | The pty uses ConPTY on Windows. |
| J7 LiveView | ✓ | ✓ | ✓ | — | 7 min | No Windows IDE build (ADR 0091). |
| J8 Upgrade | ✓ | ✓ | ✓ | ✓ | 7 min | Installs two bundles. |
| J9 Canaries | ✓ | weekly | weekly | weekly | ~25 min (11 packages, sharded 4 ways ≈ 7 min wall) | Mostly platform-neutral code. |

macOS and Windows runners are slower; budget 1.5× and 2× respectively. Totals:

- **Per edge publish** (Linux `gate` + J9): about 30 runner-minutes, about 10
  minutes wall time with 4 shards. Only merges to beamtalk main trigger it, so
  it never touches PR wall time.
- **Nightly `full`** on 4 legs: about 45 minutes per leg plus canaries, about
  200 runner-minutes in all, of which macOS minutes are the expensive part.
- **Release candidate:** the same as nightly, about 50 minutes wall time to
  promotion.

**BT-5273 lessons, applied:**

- **No compiled harness.** `npm ci` from a cache replaces the cold cargo build
  that cost 12 to 15 minutes per macOS and Windows leg.
- **Cache keys** are explicit and tested:
  - the bundle download is keyed by `version + sha256`;
  - the npm cache by the lockfile;
  - the Playwright browsers by the Playwright version;
  - the registry/deps cache by the index HEAD, and only for J9. J3 always runs
    cold, because the cold path is the user's path.
- **Only `main` and scheduled runs save caches**, the same `save-if` shape as
  beamtalk, so PR runs can't evict the entries that `main` relies on.
- **Job timeouts come from measurement.** Each job's timeout is the measured
  cold p95 plus 50%. The per-step hang guards fire well inside it, so the
  failure bundle is always written before the runner kills the job.
- **Pinned runner images** (`ubuntu-24.04`, `macos-14`, `macos-15-intel`,
  `windows-2022`), as uat already learned from setup-beam. An image bump is a
  uat PR, not a surprise.

### 9. LiveView feasibility

Feasible, with one beamtalk-side change:

- **The artifact exists.** `liveview-release.yml` builds ERTS-embedded `mix
  release` archives (`beamtalk-ide-<version>-<platform>.tar.gz`) for
  linux-x86_64, macos-arm64 and macos-x86_64, and uploads them on a full release.
  `bin/server <workspace-id>` starts the IDE against a local workspace, resolving
  the node and cookie by id
  (`docs/deployment/remote-liveview-ide.md`).
- **The browser side is proven.** `liveview-e2e.yml` already drives the IDE with
  PhoenixTest and Playwright Chromium against a live workspace, from source. J7
  is the same interaction from published artifacts, written as Playwright Test
  steps.
- **The gap:** the IDE archive is built only on tagged releases. It is not
  published to `nightly` or `edge`. So J7 can gate release candidates from day
  one, but nightly coverage needs `nightly.yml` to call `liveview-release.yml`
  and publish the archives to the `nightly` release. That is an S-sized beamtalk
  change, in Phase 5.
- **The Docker image** `ghcr.io/jamesc/beamtalk-ide:<version>` is a second
  install path. A Linux-only J7 variant runs it with `--network host` against
  the installed workspace. It is optional and lower priority than the archive
  path.

### 10. What happens to the 16 current scenarios

Their contracts move into journey steps:

- the `cli_*` contracts into J2;
- the `lsp/*` capabilities into J6's LSP steps, now against a real workspace;
- `mcp_*` into J6's MCP flow;
- `smoke` into J2.

No contract is dropped silently. A mapping table goes in the rewrite PR's
description.

## Prior Art

- **Swamp's gate** (the pattern uat already cites). A separate repo installs the
  shipped artifact and runs user stories, and its failures go back to the
  product. Adopted as is. This ADR extends it from "commands" to "journeys" and
  adds blocking promotion.
- **Rust's crater.** It builds and tests every crates.io crate against a
  candidate compiler to find ecosystem regressions before release. J9 is a small
  crater: the same idea, sized to an 11-package registry, run on every edge and
  blocking on released tags. We adopt the "registry index is the corpus" rule.
  We don't adopt crater's scale tooling: 11 packages don't need it.
- **Phoenix's installer integration tests.** `phx.new` tests generate a fresh
  project, fetch its dependencies from Hex and run its tests, because generated
  projects are the user's first contact. J2 and J3 have the same motivation.
- **Kubernetes conformance and e2e (Ginkgo).** These are long multi-step
  journeys against real clusters, tagged by feature and platform
  (`[Serial]`, `[Feature:X]`, `[Conformance]`), with failure artifacts collected
  from the cluster. We adopt tag-based tiers and "collect the environment's logs
  on failure", not only the test's stdout.
- **Erlang/OTP's own test servers (`common_test` with `peer`).** These are
  BEAM-native multi-node tests. They were considered as the runner (§
  Alternatives). From them we adopt per-test unique node names and cookies and
  the teardown check that no node outlives its test.
- **Smalltalk family.** Pharo's CI loads baselines into a fresh image from a
  published VM and image, the image-world version of "test the shipped
  artifact". The journey model goes further than an image can, because Beamtalk's
  user surface includes OS processes, releases and nodes that live outside one
  image (principles §4, "no image mechanics").

## User Impact

- **Newcomer.** J1 to J3 are their first hour: install, scaffold, add a package,
  serve a request, on their OS. Today a broken Windows zip or a scaffold that
  fails its own fmt-check is found by a newcomer. After this change, uat finds it
  first.
- **Smalltalk developer.** J6 asserts the promise they came for: a running actor
  keeps its state across an edit and runs the new method. Today no test of the
  *installed* product checks this.
- **Erlang/BEAM developer.** J4 and J5 exercise releases, `rpc`, distribution,
  TLS and multi-node behaviour the way OTP teams deploy, on macOS too, where
  BT-5264 lived.
- **Production operator.** J4 and J8 make "upgrade from the last release" and
  "the launcher verbs work" checked preconditions of a release, not things found
  after deploying.
- **Tooling developer and agent user.** J6 runs MCP through the official SDK and
  LSP through VS Code's own JSON-RPC library, against a real workspace. J7 covers
  the IDE as shipped.
- **Package authors.** J9 means a toolchain change that breaks mint or catalina
  goes red within an hour of merge, on the beamtalk side, before their CI
  notices. When it is their bug, they get a filed issue with a repro.
- **Beamtalk contributors.** None of this adds to per-PR time unless they opt in
  with `ci:uat`. In exchange, a merge can produce a filed bug from edge within
  the hour.

## Steelman Analysis

### Evolve the current Rust harness (add cross-surface `[[journey]]` to `expect.toml`)
- 🧑‍💻 **Newcomer contributor:** "It exists, it's green, and 2,800 lines aren't
  nothing. Extend it rather than relearn."
- ⚙️ **BEAM veteran:** "Rust matches the beamtalk repo's tooling. Hand-rolled
  clients mean we can test framing exactly."
- 🏭 **Operator:** "One language across both repositories means fewer runtimes
  on CI runners."
- 🎨 **Language designer:** "Declarative TOML scenarios are data, and a
  non-programmer can add one."
- **Response:** the TOML model is the "arg parsing" ceiling. Journeys need
  control flow (retries until ready, handles passed between steps, conditional
  platform steps). Expressing that in TOML means building an interpreter, which
  is a test framework written badly. Rust also brings back the cold-cache cost
  that BT-5273 measured. The framing argument is answered in § 2: real clients
  are the right judge for acceptance.

### Fold the journeys into beamtalk's own e2e suite
- 🧑‍💻 **Contributor:** "One repository, one CI, one place to look. A failing
  journey is next to the code that broke it."
- ⚙️ **BEAM veteran:** "Multi-node e2e already exists there (`just
  test-cluster`), so build on it."
- 🏭 **Operator:** "Per-PR is earlier than per-merge, so it catches regressions
  before they land."
- 🎨 **Designer:** "No cross-repo dispatch, tokens or status plumbing."
- **Response:** in-repo tests build from source by construction, so they can't
  see packaging, the installer, published IDE archives, ERTS skew or the
  registry. That is exactly where BT-5264-class and BT-5283-class problems
  surfaced. Moving journeys into the repo also brings back the temptation to
  reach into internals, which breaks the user-only rule. The per-PR benefit is
  kept through the opt-in `ci:uat` label, without paying it on every PR.

### Keep uat as a smoke test only
- 🏭 **Operator:** "A small gate is a reliable gate. Long journeys flake, and
  flaky gates get ignored."
- 🧑‍💻 **Contributor:** "Most bugs are caught in-repo. The marginal value of
  journeys is small."
- **Response:** the four misses in § Context are all cross-process or
  cross-repository, and smoke checks cannot reach them. The flake risk is real.
  It is handled by fail-fast steps, failure classes (`infra` is not a toolchain
  bug), fingerprinted triage, and blocking only on the release candidate, not on
  edge.

### Write the harness in Beamtalk (dogfood)
- 🎩 **Smalltalk purist:** "A language that can't test its own user journeys
  isn't ready. `Subprocess`, `ReactiveSubprocess` and BUnit exist. Use them."
- 🎨 **Designer:** "Every harness bug becomes a Beamtalk usability finding.
  That's free UAT of the stdlib."
- **Response:** the judge must not share a failure mode with the accused. When
  `beamtalk test` itself regresses, or an install is broken, a Beamtalk harness
  reports "harness crashed", not "J2 step 4 failed". Beamtalk has no browser
  driver and no MCP or LSP client, so J6 and J7 would need new libraries first.
  Dogfooding happens in the *fixtures*, which are Beamtalk with BUnit tests. A
  future ADR can revisit the harness language once the stdlib has these clients.

### Erlang `common_test`
- ⚙️ **BEAM veteran:** "It is the BEAM's native acceptance framework.
  `peer` nodes, `ct_hooks`, per-suite logs and HTML reports come built in.
  Multi-node is trivial."
- **Response:** it is strong for J4 and J5, and weak elsewhere. It has no
  browser automation, no official MCP or LSP client, and driving a Windows pty
  and installer from Erlang is awkward. A single runner across all journeys
  matters more than native multi-node ergonomics, which `node.ts` covers in
  about 200 lines.

### Python, pytest and playwright-python
- 🧑‍💻 **Contributor:** "pytest fixtures are the gold standard, and Python is
  everywhere."
- **Response:** this is the closest alternative, and a defensible one. Playwright
  Test wins on built-in step reporting, per-step timeouts, traces and sharding
  without plugins. It also shares a language with the official MCP TypeScript
  SDK, `vscode-jsonrpc` and beamtalk's existing VS Code extension and LiveView
  Playwright tests.

### Tension points
- Contributors want per-PR coverage. Operators want a reliable gate. The ADR
  resolves this with edge-after-merge as the default and per-PR as opt-in.
- BEAM veterans favour `common_test` for cluster work. Tooling developers need
  browser, MCP and LSP clients. One runner wins, at the cost of reimplementing
  `peer`-style node handling in TypeScript.
- The current uat design deliberately rejected client libraries. This ADR
  reverses that for the acceptance layer only, and leaves framing conformance to
  in-repo suites.

## Alternatives Considered

Each alternative is steelmanned above. In short:

- **Evolve the current repo.** Rejected: TOML can't express journeys, and the
  Rust harness carries the cold-cache cost.
- **Fold into beamtalk e2e.** Rejected: it can't test published artifacts. Its
  benefit is kept through `ci:uat`.
- **Smoke only.** Rejected: it misses every 2026-11-26 incident.
- **Harness in Beamtalk.** Rejected for the judge, adopted for the fixtures.
- **`common_test`.** Rejected: no browser or MCP/LSP clients.
- **pytest.** A close second. Rejected on built-in step, trace and sharding
  support and on client-library alignment.
- **bats or shell.** Not steelmanned seriously. Shell can't hold a pty, an
  MCP client and a browser page in one journey, and its Windows story is Git
  Bash, not the user's shell.

## Consequences

### Positive
- The three 2026-11-26 incident classes become reds: canary breakage
  (BT-5283), macOS-only launcher behaviour (BT-5264) and release-launcher
  defects.
- Releases are promoted only after the published artifacts pass on every
  platform. Users stop being the gate.
- Failures arrive as a fingerprinted Linear bug with a repro line and a full
  environment bundle, not a log to dig through.
- No compiled harness, so no cold-cargo-cache timeouts in uat.
- The registry becomes the canary list, so ecosystem coverage grows without
  edits to uat.
- The user-only rule turns undocumented-but-needed behaviour into `docs` bugs.

### Negative
- A rewrite. The current harness and its 16 scenarios are thrown away. Their
  contracts survive as steps.
- A second language (TypeScript) to maintain alongside Rust and Erlang, though
  the VS Code extension and LiveView tests already use it.
- Real-network journeys (J3, J9) depend on GitHub availability. `infra`
  classification limits the damage but cannot remove it.
- macOS runner minutes for nightly `full` are the main ongoing cost (about 90
  of about 200 runner-minutes per night).
- A blocking release gate can delay a release by about 50 minutes, longer when
  red. Waivers are manual and recorded.
- Using official client libraries means uat will not see a framing bug that
  every real client tolerates. This is intended, and covered in-repo.

### Neutral
- `cross-repo.yml` in beamtalk is retired in favour of J9.
- `release.yml` changes from "publish then notify" to "pre-release, gate,
  promote".
- uat's `CLAUDE.md` is rewritten around journeys. Its bug-filing rules carry
  over, partly automated by `triage.yml`.

## Implementation

Phases are ordered by value per unit of effort. Each ends green on its target
platforms and is independently shippable.

| Phase | Scope | Size | Why this order |
|-------|-------|------|----------------|
| **0. Skeleton rewrite** | In beamtalk-uat: Playwright Test project; `env`, `toolchain`, `proc` and `artifacts` libraries; teardown and leak checks; version selector; failure bundle; 4-platform workflow; dispatch and status wiring carried over. **J1** and **J2**, absorbing the 12 `cli_*` and `smoke` contracts. Delete the Rust harness. | M | Everything else builds on it. J1 and J2 already exceed today's coverage. |
| **1. Canaries on edge** | **J9** sharded over the registry index; the edge trigger (beamtalk's `edge.yml` dispatches to uat); `uat-edge` status; `triage.yml` with fingerprints and failure classes. Retire `cross-repo.yml`. | M | Highest value per effort: it would have caught BT-5283 within the hour. |
| **2. Registry and releases** | **J3** (buccaneer HTTP app from the real registry) and **J4** (release verbs, the TLS distribution variant on Linux and macOS, `--upgrade-from` checker). Windows `.cmd` launcher steps. | M | Covers the newcomer's first-service path and the BT-5264 class. |
| **3. Live development** | **J6**: REPL through a pty (ConPTY on Windows) with hot reload of a running actor; the MCP agent flow through the official SDK (`evaluate`, `save_method`, `test`, `flush`); LSP in workspace mode. Fold in the old `lsp/*` and `mcp_*` contracts. | M | Interactive-first is principle #1, and today it is unchecked from installed artifacts. |
| **4. Cluster and upgrade** | **J5** (two-node `farm_cluster` from releases: named sends, class references, skew after reload, node down) and **J8** (previous release → candidate, cache and lockfile compatibility). Windows J5 as best-effort nightly. | L | Multi-node and two-bundle setups are the most harness work. |
| **5. LiveView** | beamtalk: `nightly.yml` publishes IDE archives to `nightly` (S). uat: `ide.ts` and **J7** with Playwright on Linux and macOS; optional Docker-image variant on Linux. | M (+S in beamtalk) | Feasible now (§ 9). Last because it is the only journey needing a beamtalk release-pipeline change for nightly coverage. |
| **6. Blocking promotion and opt-in PR runs** | beamtalk: `release.yml` publishes a pre-release, waits for uat `full`, and a `promote` job marks it latest; recorded waiver path. A `ci:uat` label builds a Linux bundle and calls uat `gate.yml` as a reusable workflow. Hard time budgets promoted where measurements are stable. | M | Blocking should come only after phases 0 to 5 have a few weeks of measured flake rate. |

**Affected components:**

- **beamtalk-uat:** everything.
- **beamtalk:**
  - `edge.yml` and `nightly.yml` (dispatch to uat; IDE archives on nightly);
  - `release.yml` (pre-release → promote);
  - `cross-repo.yml` (removed);
  - `docs/development/testing-strategy.md` (new § "Acceptance gate (beamtalk-uat)",
    and the Cross-Repo section replaced);
  - `docs/development/releasing.md` (the promotion step);
  - `AGENTS.md` (the `ci:uat` label).
- **No compiler, runtime or stdlib changes.** Bugs uat finds are filed and fixed
  under their own issues.

## Migration Path

There are no user-facing changes. For maintainers:

- During Phase 0, the old Rust harness keeps running until J1 and J2 are green on
  all four platforms against `latest`. It is then deleted in the same PR that
  switches `uat.yml` to the new runner.
- Each old scenario's contract maps to a named journey step, listed in the
  rewrite PR.
- `cross-repo.yml` is deleted only after J9 has covered beamtalk-http on edge
  for a week with no gaps.

## Open Questions for the Owner

1. **Canary blocking.** Should a J9 red on a package's latest *tag* block
   promotion (proposed), or only file bugs? Blocking means one stale satellite
   package can hold up a toolchain release until it is waived.
2. **Pre-release promotion.** Is changing `release.yml` to publish a pre-release
   and then promote acceptable? The CHANGELOG, the VS Code extension release and
   the IDE release currently fire on `release: published`, so they would need to
   follow the promote step instead.
3. **Edge on macOS.** edge is Linux-only, so macOS and Windows regressions reach
   uat only nightly. Is a macOS edge build worth its runner cost, given that
   BT-5264 was macOS-only?
4. **Linear automation identity.** `triage.yml` needs a Linear API key and,
   like `ci-triage.yml`, possibly a Claude OAuth secret in beamtalk-uat. Are the
   same secrets acceptable there?
5. **J5 on Windows.** Should Windows multi-node be a supported target (blocking)
   or stay best-effort?

## References

- Related issues: BT-2253 (uat epic), BT-2449 (release → uat dispatch), BT-2453
  (`uat-gate` status), BT-2476 (scaffold fmt-clean), BT-2480 (`[[scenario]]` /
  `[[step]]`), BT-2497 (skip when a rolling pre-release is missing), BT-5283
  and BT-5290 (return-type check broke downstream), BT-5264 (macOS epmd
  shadowing), BT-5233 and BT-5256 (TLS distribution), BT-5273 (platform-check
  cold-cache timeouts)
- Related ADRs: ADR 0014 (test framework), ADR 0073 (package distribution), ADR
  0091 (remote LiveView IDE), ADR 0125 (OTP releases and upgrade compatibility),
  ADR 0126 (location-transparent actors)
- Documentation: `docs/development/testing-strategy.md` (§ Cross-surface Parity,
  § LiveView IDE Tests, § Cross-Repo Package Tests), `docs/development/deploying.md`,
  `docs/development/releasing.md`, `docs/beamtalk-packages.md` (§ Package
  Registry), `docs/beamtalk-installation.md`, `docs/deployment/remote-liveview-ide.md`
- External: beamtalk-uat `CLAUDE.md`; [the Swamp gate
  pattern](https://stack72.dev/the-gate-between-our-agent-code-and-our-users/);
  [Playwright Test](https://playwright.dev/docs/test-intro); Rust's crater
