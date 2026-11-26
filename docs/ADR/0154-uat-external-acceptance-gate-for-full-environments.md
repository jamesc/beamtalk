# ADR 0154: beamtalk-uat as the External Acceptance Gate for Full Environments

## Status
Proposed (2026-11-26)

## Context

### The model: swamp-uat

The owner's reference is Paul Stack's
[*The Gate Between Our Agent Code and Our Users*](https://stack72.dev/the-gate-between-our-agent-code-and-our-users/),
which describes `systeminit/swamp-uat`. This ADR adopts that design and maps it
onto Beamtalk.

- **The shipped artifact is under test, never source.** swamp's unit, contract
  and property tests run on PRs in the main repository. UAT is a separate final
  gate that runs against the exact release binaries users download, one per
  platform target.
- **A separate repository, triggered per release.** The release workflow builds
  the binaries, creates a GitHub release and fires `repository_dispatch`
  (`event-type: run-uat`, payload `{"version": …}`). The UAT repository
  downloads exactly that version. "Any fail blocks the binary": nothing reaches
  stable, Homebrew, Docker Hub or the CDN.
- **The user's point of view.** Each test creates a real repository in an empty
  temp directory, runs real commands against the real binary and checks what a
  user would see. Exit codes are asserted, and JSON output is validated against
  Zod schemas.
- **Coverage shape.** There is one test file per CLI command, whose path mirrors
  the command, each exercised with the global flags. That comes to 88 files.
  Every regression found adds a permanent test: "That's how the suite
  compounds."
- **Adversarial coverage** in seven categories:
  - path traversal;
  - secret leakage, including in `--verbose` and error output;
  - injection;
  - SIGTERM during writes;
  - stale-lock reclaim after SIGKILL;
  - concurrency;
  - resource exhaustion and corrupt or read-only state.
- **Performance gates.** Each benchmark runs 2 warmup and 5 measured iterations,
  reports p50, p75 and p95, and gates on p75 against a user-experience threshold
  scaled by a CI slowness multiplier.
- **Governance.** The UAT repository's `CLAUDE.md` opens with "Tests are more
  accurate than the code under test." It also says: "Never weaken, relax, or
  update a test to match current behavior unless the user explicitly asks you to
  update the UAT." The reason is that agents open most PRs, and loosening a
  schema is the easiest way to turn CI green while a contract silently changes.
- **Stack:** TypeScript on Deno, with Zod. The article's point is independence
  from the toolchain under test, not the particular stack.

The three regressions the article cites all passed swamp's unit tests and all
sat at integration seams. UAT caught each within minutes of the merge.

### What beamtalk-uat is today

[beamtalk-uat](https://github.com/jamesc/beamtalk-uat) already cites the same
article. Its `CLAUDE.md` already says the released bundle is under test, that
tests beat code, and that every red files a Linear Bug. What it runs falls short
of the model.

The harness is about 2,800 lines of Rust (`src/lib.rs`, `src/scenario.rs`,
`src/lsp.rs`, `src/mcp.rs`). It auto-discovers `projects/<name>/expect.toml` and
runs one *surface* per scenario. There are 16 scenarios:

| Kind | Count | What it checks |
|------|-------|----------------|
| `cli_*` | 12 | Exit code and a stdout/stderr substring for `new`, `new --app`, `fmt-check`, `lint`, `lint --format json`, `type-coverage`, `--help` and `build`. Only `cli_build` compiles. |
| `lsp/*` | 1 fixture, 4 capabilities | `hover`, `completion`, `definition`, `documentSymbol` against the server in AST mode. There is no workspace and no BEAM. |
| `mcp_*` | 2 | One `evaluate` call each. |
| `smoke` | 1 | `beamtalk test` on a one-class project. |

`[[step]]` sequences work only inside one session-backed surface. A scenario
cannot start a release and `rpc` into it, start two nodes, or edit a file and
watch the REPL pick it up. As the owner puts it, this is "more like arg
parsing".

Against the model:

- 7 of the CLI's 30 leaf commands are covered.
- JSON output is checked with substrings.
- There are no adversarial tests and no performance gates.
- The `uat-gate` status arrives after `release: published`, so users can
  install before the gate reports.
- Each leg compiles the Rust harness first. A cold cargo cache costs the 12 to 15
  minutes on macOS and Windows that BT-5273 measured in beamtalk's
  `platform-check`.

### What got through on 2026-11-26

- **BT-5283 broke downstream builds.** The return-type check on `^` early returns
  merged at 13:45Z. Within the hour, beamtalk-mint's CI failed
  `build --warnings-as-errors` with 13 rejected methods, and catalina failed the
  same way. The rejected shape was the `x isError ifTrue: [^x]`
  error-propagation idiom, which buccaneer's own `Buccaneer start:` also uses.
  BT-5290 fixed it the same day. Nothing builds the satellite packages against
  the toolchain before release. `cross-repo.yml` builds only beamtalk-http, and
  from source.
- **BT-5264: macOS epmd shadowing.** On macOS, a loopback client verb started a
  second epmd beside a wildcard TLS node's epmd, so local lookups reported the
  node as down. Linux refuses that bind, so Linux-only tests cannot see it. It
  was found by hand.
- **Release-launcher issues** around TLS distribution (BT-5233, BT-5256) were
  also found by hand. Nothing runs `beamtalk release` from an installed bundle,
  starts the result and talks to it.
- **Cold-cache timeouts (BT-5273).** `platform-check` hit its 40-minute limit
  because a cold Rust build took 12 to 15 minutes. uat's legs have the same
  exposure.

### What already exists, so this ADR doesn't duplicate it

| Existing suite or tool | Builds from | Answers | Stays in beamtalk |
|------------------------|-------------|---------|:--:|
| Rust, EUnit, stdlib and BUnit tiers (`just test`) | source | Is each unit correct? | ✓ |
| REPL protocol and cross-surface parity tests | source | Do the protocol and surfaces agree, down to framing? | ✓ |
| `examples/cluster/scripts/e2e.sh` | source | Does ADR 0126 work across three local nodes? | ✓ |
| `liveview.yml`, `liveview-e2e.yml` | source | Does the IDE work against the same commit's workspace? | ✓ |
| `ci-platforms.yml`, the `otp-matrix` job | source | Do the tests pass on macOS, Windows and each supported OTP? | ✓ |
| Micro and dispatch benchmarks (`bench-dispatch` keys, vs-elixir rows, reduction and word counts) | source | Is this dispatch path fast? Did this PR fix it? | ✓ |
| Coverage (`coverage.yml`) | instrumented source | Which lines run? | ✓ |
| `cross-repo.yml` | source | Does beamtalk-http build against main? | replaced by J9 |

Each of these builds from the same commit it tests. None answers the user's
question: *with only the published artifacts, the public registry, my OS and my
OTP, can I get from install to a running service, and is it fast and safe?*

That question covers everything between the source tree and the user:

- `scripts/package-release.sh` and `install.sh`;
- the archive layout;
- the user's own OTP, which the core bundle does not ship (§ 1);
- release-build settings and the registry index;
- the separately released IDE and editor extension;
- the satellite packages' own `--warnings-as-errors` builds.

### Constraints (owner decisions)

- Everything runs in CI on Linux, macOS and Windows wherever the feature exists
  on that platform.
- The **real package registry** is in scope, not a mirror.
- **LiveView is in scope** if feasible. It is (§ 13).
- uat is redesigned from a clean slate. The current harness, `expect.toml`,
  surface types and layout carry no weight.
- uat owns **user-level performance gates** on the released bundle. beamtalk
  keeps its micro and dispatch benchmarks, which need internal probes and source
  builds and are how PRs prove a fix.
- **Coverage stays in beamtalk CI.** It needs an instrumented source build and
  is not a property of the shipped artifact.
- The gate is a separate repository, triggered per release, as in the article.

## Decision

### 1. Purpose and the user-only rule

**beamtalk-uat proves that a user holding only published artifacts can use every
command, complete every supported journey, stay safe under adversarial input and
get acceptable performance, on every release target and supported OTP major.**

**The user-only rule.** A test may use only what a user has:

- **The bundle for its target, installed the documented way.** That is
  `install.sh` on Linux and macOS, and the zip plus `PATH` on Windows.
- **Erlang/OTP, which the user installs themselves.** The core bundle does not
  ship ERTS. The supported window comes from `otp-support.toml` (today
  `min-major = 28`, `max-major = 29`). The documented install method for each
  target is part of what the user has:
  - macOS: `brew install erlang`;
  - Windows: the erlang.org installer;
  - Linux: asdf (`asdf install erlang <version>`).

  The documented `sudo apt install erlang` gives an OTP older than 28 on
  `ubuntu-24.04`, so J1 uses asdf there and files the apt instruction as a
  `docs` Bug. J1 performs the documented OTP install. Other tests may use a
  cached OTP of the same major installed by `erlef/setup-beam`, because the OTP
  install itself is not under test in them. This exception is stated, not
  implied.
- the IDE archive, the `.vsix`, and the IDE image at a candidate tag (§ 10);
- the public registry and package repositories;
- documented commands, flags and environment variables;
- the OS's own tools.

Tests never check out beamtalk source, never set an undocumented variable and
never call an internal function. If a test needs one of these, that is a finding:
a `docs` Bug or a missing feature.

This rule is the line against the in-repo suites. Those suites ask "is the code
right?" uat asks "does the shipped thing work for a user?" A behaviour pinned by
a fast in-repo test is not re-asserted at finer grain here.

### 2. Governance: the uat CLAUDE.md, and what enforces it

The rewritten `CLAUDE.md` opens with these rules, adapted from swamp-uat:

1. **Tests are more accurate than the code under test.** When a test fails, the
   default assumption is that Beamtalk (the toolchain, IDE, extension or
   installer) must change, not the test.
2. **Never weaken, relax, skip or update a test, schema or threshold to match
   current behaviour unless the owner explicitly asks you to update the UAT.**
   This covers:
   - loosening a Zod schema;
   - raising a perf threshold or multiplier;
   - widening a timeout;
   - adding a target or OTP exclusion;
   - demoting a blocking test to incubating (§ 10);
   - changing the perf method.
3. **Every red files a Linear Bug** against `BT`, automated by `triage.yml`
   (§ 11).
4. **Every regression gets a permanent test.** A bug found against a published
   artifact by hand, by a user or by a downstream package is not closed until
   uat has a test that fails on the broken release and passes on the fixed one.
   BT-5283 and BT-5264 are the first two.
5. **Independence.** uat never imports, vendors or builds Beamtalk source.

**What enforces rule 2.** The article relies on `CLAUDE.md` alone. This ADR adds
one real gate and one tripwire. It does not claim the tripwire is proof.

- **The real gate is a code-owner review.** All expected values that aren't
  written inline in a test step live in owner-gated paths:
  - `schemas/**`;
  - `bench/thresholds.toml` (thresholds *and* multipliers);
  - `targets.toml` (target and OTP exclusions, and incubating/blocking status);
  - `playwright.config.ts`;
  - `.github/**`;
  - `CODEOWNERS`.

  Branch protection on `main` requires a code-owner review.
- **Agents need their own identity, or the owner can't review their PRs.** An
  author cannot approve their own PR. So agents working in beamtalk-uat push and
  open PRs as the Claude GitHub App, never under the owner's user account, and
  the owner reviews. A change the owner authors and merges with admin bypass is,
  by definition, the owner's explicit request.
- **The tripwire is `governance.yml`, a required check.** It diffs `tests/**`
  and `lib/**` against the base. It fails, pending an owner review, if the PR:
  - deletes a line containing `expect(` or `.parse(`;
  - deletes a `test(` or `step(` block;
  - adds `.skip`, `.fixme` or `.only`;
  - changes the constants in `lib/bench.ts`.

  It reads the owner's approval from the reviews API: an APPROVED review from
  the owner's login on the head SHA, made by a user and not by an App. This is a
  tripwire, so a determined refactor can evade it. The owner-gated paths are
  what keep the expected values honest.
- Adding tests, steps, assertions or schemas needs no owner review, so the suite
  can compound.

### 3. Per-target artifacts, OTP, and the trigger

**Targets.** These are the bundle targets `release.yml` builds. Each is tested on
the runner image that built it. Today `release.yml`, `edge.yml`, `nightly.yml`
and `liveview-release.yml` use floating labels (`ubuntu-latest`,
`macos-latest`), so **Phase 0 pins them in beamtalk** to the images below. "The
image that built it" is then a fact, and an image bump is a reviewed change on
both sides.

| Target | Runner (pinned in Phase 0) | Edge | Nightly | Tag |
|--------|----------------------------|:----:|:-------:|:---:|
| `linux-x86_64` | `ubuntu-24.04` | ✓ every publish | ✓ | ✓ |
| `macos-arm64` | `macos-15` | ✓ hourly (fast set) | ✓ | ✓ |
| `macos-x86_64` | `macos-15-intel` | — | ✓ | ✓ |
| `windows-x86_64` | `windows-2022` | — | ✓ | ✓ |

The macOS edge leg needs two beamtalk changes, both in Phase 0. First, `edge.yml`
must publish a `macos-arm64` asset (owner decision 3). Second, `install.sh
--edge` must stop exiting on anything but linux-x86_64.

**OTP axis.** The dispatch payload carries `otp-support.toml`'s window. Artifacts
are built on min-major.

| Target | Nightly / RC OTP | Notes |
|--------|------------------|-------|
| `linux-x86_64` | min-major (all layers) **and** max-major (contracts, J1 to J4, J6) | J1 also checks that OTP 27 on `PATH` gives a clear, structured error from `doctor` and `build`. |
| `macos-arm64` | max-major | brew installs the newest. |
| `macos-x86_64` | min-major | |
| `windows-x86_64` | max-major | The erlang.org installer offers the newest. |

A window bump in `otp-support.toml` is picked up from the payload, with no uat
edit.

**Dispatch.** `edge.yml`, `nightly.yml` and `release.yml` each send
`repository_dispatch` with `event-type: beamtalk-artifact`:

```json
{ "channel": "edge|nightly|release", "version": "0.6.0-edge+c6b28b3",
  "tag": "edge|nightly|v0.6.0", "sha": "<commit>",
  "otp": { "min-major": 28, "max-major": 29 },
  "assets": { "beamtalk-…-linux-x86_64.tar.gz": "<sha256>", "…": "…",
              "beamtalk-ide-…": "<sha256>", "beamtalk-….vsix": "<sha256>" },
  "ide_image": "ghcr.io/jamesc/beamtalk-ide@sha256:<digest>" }
```

Today `release.yml` sends `beamtalk-release`. During migration, uat accepts
both event types, mapping `beamtalk-release` to `channel: release` with no
`assets` map. It verifies such a run against the published `.sha256` files
instead. The old type is dropped in the same release that switches `release.yml`.

**Download once per run.** Rolling releases (`edge`, `nightly`) are replaced in
place, so a later shard could find newer bytes than an earlier one. A `resolve`
job therefore:

1. downloads every asset in the payload once;
2. checks each against its sha256;
3. uploads them as one workflow artifact, which every shard installs from.

J1 is the one test that uses the live installer. It runs inside `resolve`
before the fan-out, and checks that what `install.sh` fetched has the payload's
sha256. If any hash no longer matches at `resolve` time, the rolling release has
moved on. The run ends as **`superseded`** (§ 11): cancelled, no Bug. uat's
workflows use concurrency groups per channel with `cancel-in-progress: true` for
`edge` and `nightly`. Release-candidate runs are never cancelled.

### 4. Clean rewrite: TypeScript on Playwright Test, with Zod

beamtalk-uat is rewritten. The Rust harness, `expect.toml`, surface types and
`projects/<scenario>/` are deleted. These carry over:

- the repository and the governance rules;
- the version selector (`latest | edge | nightly | vX.Y.Z | local:<path>`);
- the dispatch and status wiring;
- the contracts worth keeping (§ 14).

The harness is **TypeScript on [Playwright Test](https://playwright.dev/docs/test-intro)**,
used as a general runner, with **Zod** for JSON contracts. swamp shows TypeScript
and Zod working for this job. Playwright Test was chosen over Deno's runner for
what a multi-process, multi-surface gate needs:

| Need | What provides it |
|------|------------------|
| Ordered journeys with per-step reporting | `test.step()`. A red reads "J4 › rpc into release › step 5/9". |
| Time budgets | Per-test and per-step timeouts, plus a global limit. |
| Diagnosable failures | `testInfo.attach()`, the HTML report, JUnit XML and traces. |
| Tiers and selection | Tags with `--grep`, and `projects`. |
| Teardown | Fixtures with guaranteed teardown, for the process-tree kill and leak checks. |
| Parallelism | `--workers`, `--shard=i/n`. |
| LiveView, `.vsix` smoke | Native Chromium, already proven in `liveview-e2e.yml`. `@vscode/test-electron` is TypeScript too. |
| Cross-platform, no cold build | Node behaves the same on all three OSes. `npm ci` from a cache takes seconds. |

**Use real clients.** MCP steps use the official MCP TypeScript SDK client. LSP
steps use `vscode-jsonrpc` and `vscode-languageclient`. These are what Claude
Code, other agents and VS Code use. A framing bug every real client tolerates is
not user-facing. One that breaks them turns uat red. Byte-level framing stays in
beamtalk's protocol and parity suites.

**Fixtures stay Beamtalk.** The apps under test are real Beamtalk projects in
`fixtures/`, with BUnit for in-app assertions. The judge is not Beamtalk, so a
broken toolchain cannot take down its own verdict.

### 5. Four layers of tests

```text
beamtalk-uat/
├── tests/
│   ├── cli/            # Layer 1: one file per leaf command, path mirrors it (30 files)
│   │   ├── build.test.ts   release.test.ts   run.test.ts   …
│   │   ├── deps/add.test.ts   deps/list.test.ts   deps/update.test.ts
│   │   ├── workspace/{list,stop,status,attach,transcript,logs,create,prune}.test.ts
│   │   └── generate/{native,stubs}.test.ts   registry/site.test.ts
│   ├── journeys/       # Layer 2: J1 to J10
│   ├── adversarial/    # Layer 3: one file per category (§ 7)
│   ├── bench/          # Layer 4: user-level performance gates (§ 8)
│   └── editors/        # .vsix smoke (§ 6)
├── schemas/            # Zod: lint JSON, type-coverage JSON, MCP results, LSP results, releaseInfo, lockfile
├── fixtures/           # real Beamtalk projects (hello_web, counter_live, farm_cluster, …)
├── lib/                # env, toolchain, otp, proc, repl (pty), mcp, lsp, release, node, ide, bench, artifacts
├── bench/thresholds.toml   # p75 thresholds and multipliers (owner-gated)
├── targets.toml        # exclusions and incubating/blocking status (owner-gated)
└── playwright.config.ts
```

**Layer 1: command contracts.** There are 30 leaf commands:

- 16 top-level commands without subcommands, one of them `version` with its
  show, set and bump modes as cases;
- `workspace`'s 8 subcommands;
- `deps`' 3;
- `generate`'s 2;
- `registry site`.

Each file checks:

- **For every command:** the global flags (`-v` / `-vv`, `--help`); the
  documented exit codes; stream routing (human diagnostics to stderr, program
  output and machine formats to stdout); and file-system effects (what is
  created, changed or left alone).
- **Where `--format json` exists:** the output parses against its Zod schema,
  with fields, types and patterns. Today that is only `lint` and
  `type-coverage`.
- **Where a machine consumer is plausible but there is no JSON** (`test`,
  `build`, `deps list`, `workspace list`): the missing format is filed once as a
  feature Bug under rule 3. When it ships, a schema follows. The contract
  doesn't wait for it: exit codes, streams and effects are asserted now.

`publish` runs against a local bare-git package remote and a local-directory
registry (a documented `BEAMTALK_REGISTRY` form). It never touches the real
index or a real package repository.

**Layer 2: journeys** (§ 6). Ordered, cross-surface user stories sharing one
environment.

**Layer 3: adversarial tests** (§ 7). Eight categories: swamp's seven, with
"distribution security" split out because Beamtalk ships a distributed runtime.

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
    bt.in(app).spawn(['run', '.'], { env: { PORT: `${port}` },
      readyOn: 'stderr:/^Started hello_web v/m' }));   // the documented service-mode banner

  await step('serve a request', async () => {
    const res = await env.http.getUntilReady(`http://127.0.0.1:${port}/`, { within: '20s' });
    expect(res.status).toBe(200);
    expect(await res.text()).toBe('hello from beamtalk');
  });

  await step('stop cleanly, leave nothing behind', () => svc.interrupt({ expectExit: 0 }));
});
```

The fixture overlays the scaffold's `src/`. The `[application] supervisor` that
`new --app` generates starts a fixture actor, and its start-up sends
`HelloWeb serve`:

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

Readiness is the service-mode banner `beamtalk run .` documents on stderr
(`Started <app> v<version>`), followed by polling HTTP. Readiness never depends
on fixture logging alone.

**Rules shared by every layer:**

- **Clean start.** Each test gets an empty temp directory, an isolated
  `HOME`/`USERPROFILE`, its own ports, and unique node names and cookies.
- **Structured assertions.** Assert on structure where it exists: Zod-validated
  JSON, MCP structured content, LSP results, HTTP status, and `Beamtalk
  releaseInfo` via `rpc`. uat never pins REPL display formatting or prompts:
  those are intentional and owned by beamtalk's e2e tests (AGENTS.md, "REPL
  output").
- **Transcripts.** Every step records argv, cwd, the environment diff, stdout,
  stderr, the exit code and wall time.
- **Failure bundle.** On a red, the test attaches:
  - all the transcripts;
  - workspace logs, release `log/` and `erl_crash.dump`;
  - `epmd -names` and the process tree;
  - `beamtalk --version` and `doctor` output;
  - the Playwright trace;
  - a one-line repro command.

  § 7 explains where `security` bundles go instead.
- **Checked teardown.** No process and no epmd registration may outlive the
  test. A leak is a red step.

### 6. The journey catalogue

| # | Journey | Steps (abridged) | Catches |
|---|---------|------------------|---------|
| **J1** | Install | Documented OTP install (§ 1). Then `install.sh` with the documented channel flag (`--version vX.Y.Z`, `--nightly`, `--edge`) or the zip plus `PATH` into a clean `HOME`; the archive fetched has the payload's sha256; `--version` matches; `doctor` is healthy. Then: install twice; `--prefix` with a space in it; `--prefix` without write permission gives a clear error; two OTPs on `PATH` (asdf and a system `erl`), where `doctor` names the one in use; OTP 27 on `PATH` gives a structured error. Finally `--uninstall`. | Packaging, checksums, archive layout, OTP selection. |
| **J2** | Project lifecycle | `new` → fmt-check clean → `build` → `test` → edit → `lint` finds it → `fmt` fixes it → `run` → `build --escript` → run the escript with no `beamtalk` on `PATH` → `new --app` → `run .` → Ctrl+C exits cleanly. | Scaffold drift (BT-2476), the build cache, output routing, escript packaging (ADR 0099). |
| **J3** | Registry packages | `deps add` from the real registry → lockfile pins the closure → `build --warnings-as-errors` → buccaneer serves HTTP → `deps update` → warm rebuild. **Offline:** with the network blocked (`HTTPS_PROXY` set to a dead port), a locked project still builds from cache, and an unlocked `deps add` fails with a clear message. | Index resolution, git checkout, the type checker against real packages, degraded-network behaviour. |
| **J4** | Releases | `release` → `start` → `ping` → `rpc "Beamtalk releaseInfo"` → `stop` → leak check. TLS variant (`RELEASE_DISTRIBUTION=tls`, generated CA) plus a loopback client on the same host. `--upgrade-from` flags an unmigrated shape bump. Restart-based upgrade (ADR 0125 §2.1): stop 1.0, start 1.1 in place, and the node answers. Hot relup is not a v1 capability and is not tested. | Launcher verbs, BT-5264-class epmd, TLS preflight, `.cmd` launchers. |
| **J5** | Two-node cluster | Two `farm_cluster` releases → connect → named send → a class reference crosses the wire → hot reload on one node → skew is announced → kill a node → `NodeDown` is seen. | ADR 0126 behaviour from installed artifacts. |
| **J6** | Live development | REPL in a pty: spawn a `Counter`, send it messages, edit the file, `:sync`; the same actor keeps its state and runs the new code. MCP via the SDK: `evaluate` → `save_method` → `test` → `flush`, and the change is on disk. LSP in workspace mode: diagnostics, and go to definition into a dependency. | Hot reload, flush, LSP workspace mode. |
| **J7** | LiveView IDE | Install the IDE archive → `bin/server <workspace-id>` → Playwright: page loads → eval round-trips → System Browser shows the class → edit a method → the workspace sees it. Linux variant: the candidate image (§ 10). | Bundle/IDE skew, packaging, the Attach topology. |
| **J8** | Upgrade path | `install.sh --version <previous>`, verified against that release's published `.sha256` (so it works even when the previous release predates the § 3 payload) → project and release → install the candidate over it → the same project builds, tests and releases → `--upgrade-from` is clean → the old `~/.beamtalk` cache is harmless. | Cache and lockfile compatibility, stale install state. |
| **J9** | Downstream canaries | For each registry package: its default branch and its latest tag → `beamtalk test --warnings-as-errors`. Runs isolated (§ 7). | BT-5283 / BT-5290. |
| **J10** | Workspaces | `workspace create` → `repl` attaches → a second shell `workspace attach`es and sees the same bindings → `transcript` streams → `stop` → `status` shows stopped → `kill -9` a workspace node → `list` and `status` report it stale, not running → `prune` removes it → no epmd registration is left. | Workspace lifecycle across CLI invocations, stale-state recovery. |

**What J1 tests, and when the plain path runs.** J1 tests the installer, pinned
with its documented flags, so it reaches the exact candidate. The unpinned
`releases/latest` path cannot see a candidate by design. It is exercised by a
**post-promotion smoke**: J1, unflagged, on every target after `promote`. That
run can't block, because the release is already out. A red files a priority-2
Bug.

**Editors.** Phase 6 adds a `.vsix` smoke in `tests/editors/` using
`@vscode/test-electron`: install the candidate `.vsix`, open a `.bt` file in a
project, and get a diagnostic from the bundle's LSP. The Tauri desktop app is
**not** tested by uat. Consequences states that gap.

### 7. Adversarial coverage, Beamtalk-specific

There is one file per category. Each test runs against the installed bundle. A
test that finds today's behaviour lacking is a finding: it lands incubating with
its Bug linked (§ 10). It is not softened to pass.

| Category | Tests |
|----------|-------|
| **Distribution security** | Workspace and foreground-REPL cookies are per-process, in a 0600 file, never `~/.erlang.cookie` or `nocookie` (BT-5066). epmd and distribution listen only on loopback by default. A node with the wrong cookie is refused. TLS release: a plain-TCP client cannot connect, a peer whose certificate is from another CA is refused, and the launcher refuses an option file lacking `verify_peer` / `fail_if_no_peer_cert` or containing `verify_fun` / `partial_chain` (deploying.md). The console WebSocket binds `127.0.0.1` unless `bind` is set. |
| **Secret leakage** | A sentinel secret, set in a cookie and in an env var a program reads, never appears in: stdout or stderr at `-v` and `-vv`; REPL errors; MCP `evaluate` errors and structured content; `run --raw-mfa` stacks; workspace and release logs; crash dumps; `ps` argv of started nodes. |
| **Path traversal** | `new ../escape`; a dependency or index entry containing `..` or an absolute path; `deps add --path` pointing outside; a release tarball with `../` members fed to `--upgrade-from`; `release --output` refusing `~`, `/`, the project root and `src/` (deploying.md). |
| **Injection** | Launcher `rpc` / `eval` arguments with shell metacharacters stay Beamtalk text (`bin/<n>` and `bin/<n>.cmd`). Project names with `;`, spaces or quotes. A registry `git` URL with an `ext::` or `--upload-pack` payload is refused. |
| **Signals and atomic writes** | SIGTERM (on Windows, a process kill) during `build`: the next build is correct. SIGKILL during `release`: the `.beamtalk-swap` journal recovers. SIGTERM during MCP `flush`: the source is either old or new, never partial. |
| **Locks** | SIGKILL a `build` holding the project lock: the next build proceeds within 5s. Concurrent `build`s wait. Two `release` runs on one `--output` serialise. CLI, LSP and MCP refreshing one registry cache never see a half-written index. |
| **Concurrency and hot reload under load** | `:sync` a class while 4 senders drive 10,000 messages per second at its actors: no lost messages, no crash, and every reply comes from the old or the new method. Five MCP sessions `save_method` one class: the result is one of the five, and `flush` writes a file that compiles. |
| **Resource exhaustion and corrupt state** | 100 MB to `Transcript`, a 50 MB MCP `evaluate` result, and deep recursion: no OOM, and a bounded, explained error. A corrupt `beamtalk.lock`, registry cache, `.beam` or workspace `metadata.json`, and a read-only project: each gives a structured error with a hint, never a crash or hang. |

**beamtalk-uat is public, so security reds stay private.** Leakage tests use
sentinel secrets, so their output reveals nothing real. A working bypass of a
cookie, TLS or bind check is different: in a public log, it is a disclosed
vulnerability. Therefore:

- The **distribution-security** and **secret-leakage** categories run in a
  private companion repository, `beamtalk-uat-security`. It checks out the
  public harness at a pinned SHA and holds only those test files. Its runs, logs
  and artifacts are private.
- It reports to the gate only a pass/fail per test ID.
- Its `triage` files the Bug in Linear, which is private, with the bundle
  attached there.

**J9 runs third-party code, so it gets no secrets.** `beamtalk test` on a
package's branch or tag runs code its author controls, including native builds.
J9's jobs therefore run with `permissions: {}`, no secrets and no tokens. They
upload only JUnit and report artifacts. A separate `report` job, which never
executes downloaded code, reads those artifacts as data and posts results. This
isolation applies to every job that runs code not authored in beamtalk-uat or
beamtalk.

### 8. Performance gates

**Method** (the article's): each benchmark runs **2 warmup** iterations, which
are discarded, then **5 measured** iterations. It reports **p50, p75 and p95** and
**gates on p75**:

```text
p75 ≤ threshold[benchmark][target] × multiplier[runner_class]
```

- **Thresholds and multipliers live in `bench/thresholds.toml`**, which is
  owner-gated (§ 2). The workflow passes only a runner class name.
  `BEAMTALK_UAT_BENCH_MULTIPLIER` works only for local runs and is ignored when
  `CI` is set.
- **Iteration counts and the gated percentile** are constants in `lib/bench.ts`,
  watched by the tripwire.
- **Calibration.** The benchmarks first run report-only for 10 nightly runs.
  Thresholds are then set at the observed p75 × 1.25, rounded up. Only the owner
  raises one. Anyone may lower one.
- With 5 samples, p75 is the 4th-slowest run. That is the article's trade-off,
  and the owner chose it.
- **Diagnostic column.** Each report also prints a **paired ratio** against the
  previous stable release, measured interleaved in the same job. It isn't
  gated, but it separates "this release is slower" from "the runner image got
  slower" when a threshold trips. If threshold churn turns out to be high, the
  owner can promote the ratio to the gate.

**Benchmarks** (the owner's list), each against the installed bundle:

| Benchmark | Measures | Fixture |
|-----------|----------|---------|
| `compile.cold` / `compile.warm` | `beamtalk build` wall time, `_build` empty / warm | buccaneer plus deps |
| `lifecycle.new_build_test` | `new` → `build` → `test` | the scaffold |
| `repl.startup` / `repl.eval_rtt` | time to the first usable prompt; round-trip of `3 + 4` | a project REPL |
| `release.start` | `bin/<n> start` → first successful `ping` | `hello_web` release |
| `actor.throughput` / `actor.latency` | messages per second; p75 send→reply between local actors | `counter_live` |
| `cluster.send` | p75 cross-node named-send round trip | `farm_cluster` |
| `registry.resolve.cold` / `.warm` | `deps add` + resolve + fetch, cold / warm index cache | `hello_web` |
| `lsp.hover` / `lsp.completion` / `lsp.definition` | request→response p75, workspace mode | `hello_web` |

That is **14 measurements**. These catch what micro-benchmarks miss:
release-build flags, packaging, startup, install layout and registry behaviour.
beamtalk keeps `bench-dispatch`, the vs-elixir rows and the reduction and word
counts. uat does not re-measure dispatch paths.

**Where perf gates.** Perf **gates on `linux-x86_64` and `macos-arm64`**, on
dedicated jobs that run nothing else at the same time. On `macos-x86_64` and
`windows-x86_64` it runs report-only, weekly and on release candidates, until a
non-Linux perf regression actually escapes. Packaging and release-build
regressions are mostly platform-independent, so this halves the most expensive
line. Edge runs skip perf: shared-runner noise would turn a per-merge signal
into a flake generator. A nightly breach files a `performance` Bug. A
release-candidate breach blocks promotion.

### 9. Harness capabilities

- **Cross-surface, cross-process.** All handles (`Proc`, `Repl`, `McpClient`,
  `LspClient`, `ReleaseNode`, `IdePage`) live in one environment.
- **Multi-node.** Unique `-sname`s and cookies per test. epmd state is captured
  and reset, and the documented `RELEASE_*` variables are supported.
- **OTP.** `lib/otp.ts` provisions the documented install per target for J1, and
  `setup-beam` caches per major for every other test (§ 1).
- **Network and registry.** J3, J9 and `registry.resolve.*` use the live
  registry and GitHub. An unreachable host is classified `infra`, not
  `toolchain`. J3's offline step blocks the network on purpose and is a normal
  assertion.
- **Hang guards.** A hard per-step timeout ("step `rpc` exceeded 60s"),
  separate from the perf gates.

### 10. Gating: what runs when, what blocks, and how tests graduate

| Trigger | Artifact | Layers | Targets / OTP | Blocks |
|---------|----------|--------|---------------|--------|
| Every `edge` publish (merge to `main`) | edge bundle | **edge-fast**: contracts, J1, J2, J4, adversarial fast subset | linux-x86_64 / min | Nothing. Posts a `uat-edge` status on the merged commit. A red files a Bug. Superseded runs cancel. |
| **Hourly**, if edge moved | latest edge | **edge-hourly**: J3, J6, J9 on linux; the macOS fast set (contracts, J1, J2, J4 including TLS, adversarial fast subset) on macos-arm64 | linux-x86_64, macos-arm64 / min | Nothing. A red files a Bug within the hour (owner decision 3, coalesced). |
| `nightly` publish | nightly bundles | all four layers (perf gates on linux and macos-arm64, report elsewhere) | all 4 / per § 3 | Nothing. Reds file Bugs. |
| Weekly | `latest` | all four layers, plus J9 on macOS and Windows, plus perf report on macos-x86_64 and Windows | all 4 | Nothing. Catches drift in OS images, the registry and GitHub. |
| Tagged release candidate | `vX.Y.Z` pre-release assets + candidate IDE image | all four layers | all 4 / per § 3 | **Promotion**, see below. |
| After promotion | `latest` | J1 unflagged | all 4 | Nothing; the release is out. Red files a P2 Bug. |
| beamtalk PR with `ci:uat` | PR-built linux bundle | edge-fast | linux-x86_64 | Informational (Phase 8). |
| `workflow_dispatch` | any | any | any | n/a |

**Graduation: every new test starts incubating.** Rule 4 and § 7 add tests
continuously, and some land red on purpose. If every red blocked, the first
release candidate after a new layer lands would be frozen. So:

- Every test, step, target or OTP leg **lands `incubating`** in `targets.toml`.
  It runs everywhere, and a red files a Bug linked from the test, but it does
  not block.
- It **graduates to `blocking`** after 10 consecutive green nightlies on that
  target, about two weeks. A bot opens the graduation PR. Graduation makes the
  gate stricter, so it needs no owner review.
- **Demotion back to incubating is weakening** (rule 2) and needs the owner.
- J5 on Windows (owner decision 5) and J4's TLS step on Windows are ordinary
  incubating entries under this rule. Neither is a special case.

**Promotion is the blocked step**, as in the article: "any fail blocks the
binary." Today `release.yml` builds and publishes on `release: published`, so
users can install before uat reports. Under this ADR:

1. **Build and attach.** A tag push builds every target and attaches **all**
   assets to a GitHub **pre-release**: the bundles, the IDE archives and the
   `.vsix`. It also pushes the IDE image to `ghcr.io/jamesc/beamtalk-ide:rc-<sha>`.
   This requires splitting `release-vscode.yml`, `liveview-release.yml`,
   `liveview-docker.yml` and `desktop-release.yml` into **build-and-attach** and
   **publish** inputs. Today each runs both as one step under `publish: ${{
   github.event_name == 'release' }}`. Build and attach run here. Only the
   publish halves wait for promote.
2. **Dispatch.** beamtalk dispatches uat with the § 3 payload: every asset's
   sha256, and the image digest.
3. **Test and report back.** uat runs every layer on every target against those
   bytes. It posts a `uat/<channel>/<version>` commit status for people to see.
   It also sends `repository_dispatch` back to beamtalk (`event-type: uat-gate`)
   with `{tag, sha, version, assets, ide_image, result, run_id}`, using a token
   scoped to dispatch into beamtalk only.
4. **Promote.** `promote.yml` runs on that dispatch, in a **protected GitHub
   Environment**, `release-promotion`:
   - Only `promote.yml` on `main` may deploy to it.
   - It holds the only copies of the Marketplace token and the ghcr push and
     retag credentials, so no other workflow can publish.
   - The owner is a required reviewer for the first three promotions. The owner
     can later remove that.

   `promote.yml` ignores statuses. It reads the tag and sha from the payload,
   never `GITHUB_SHA`, and refuses unless **all** of these hold:
   - the tag still resolves to `sha`;
   - it **re-downloads and re-hashes every pre-release asset** and each matches
     the payload. Uploads use `--clobber`, so a re-run leg or a later
     `workflow_dispatch` could have replaced bytes after uat tested them;
   - the `rc-<sha>` image digest matches `ide_image`;
   - the API shows `run_id` is a completed, successful run of uat's `gate.yml`
     on uat's `main`, for this exact `version` and `sha`;
   - `result` is `success`.

   When all hold, it:
   - marks the release latest (`make_latest`), which `install.sh` follows;
   - publishes the tested `.vsix` to the Marketplace;
   - retags the tested image digest as `<version>` and `latest`;
   - runs the desktop publish.

   A refusal files a Bug.
5. **On red.** The release stays a pre-release, Bugs are filed, and the fix is a
   new tag. The owner can promote by hand, with a recorded waiver, through the
   same Environment. No agent can, because no agent holds the Environment's
   credentials or its reviewer role.

**A pre-release is not private, and this ADR accepts that.** `install.sh
--version vX.Y.Z` installs pre-releases by tag, and publishing one notifies repo
watchers. A red candidate therefore reaches anyone who pins its exact version,
and it uses up that version number: the fix ships as the next patch. Draft
releases would hide candidates, but uat would then need a token to download
them, which breaks the documented install path. Exposure is the smaller cost.

**What blocks promotion:**

- any red in a **blocking** contract, journey, adversarial or editor test on a
  target and OTP it declares;
- any p75 over threshold × multiplier on a gated perf target;
- a J9 red on a package's latest **tag** (owner decision 1). The exact rules,
  including how warnings are treated, are pending the owner's answer to review
  finding 4.

**What never blocks:**

- incubating reds;
- `infra` failures, which get one re-run and then go to the owner;
- `superseded` runs.

**`cross-repo.yml` is retired** once J9 covers beamtalk-http in the hourly edge
run.

### 11. Failure classes and triage

| Class | Meaning | Files to |
|-------|---------|----------|
| `toolchain` | Beamtalk did the wrong thing (the default, per rule 1). | Linear `BT` Bug |
| `performance` | A p75 over threshold. | Linear `BT` Bug, `performance` label |
| `security` | A distribution-security or leakage red (from the private repo, § 7). | Linear `BT` Bug, priority 2, with the bundle attached in Linear only |
| `downstream` | J9 only: the package's code is wrong. | The package's repository, linked from a `BT` comment |
| `docs` | A test needed something undocumented, or followed a documented step that doesn't work (for example apt OTP, § 1). | Linear `BT` Bug, area `docs` |
| `infra` | The runner, network or GitHub failed. | Nothing. One re-run, then the owner. |
| `superseded` | A rolling release moved on during the run (§ 3). | Nothing. Cancelled. |
| `harness` | uat's own bug. | A beamtalk-uat issue |

`triage.yml` is modelled on beamtalk's `ci-triage.yml`. It fingerprints each red
as `(layer, test, step, target, otp, normalised first error line)`. It then
comments on the open `BT` issue carrying that fingerprint, or creates a Bug with:

- the labels uat's `CLAUDE.md` requires;
- the exact version and sha256;
- the repro line;
- the artifact link (public classes only).

**Secrets (owner decision 4).** beamtalk-uat holds its own Linear API key and
Claude OAuth secret, scoped to that repository and to the triage job, and only
for runs of uat's `main` or schedule, never for PR or fork runs. The triage
agent's tools are read-only plus Linear, and it treats logs as untrusted data.

There are no automatic retries inside a run. A test that fails and then passes
on a rerun is filed as `flaky`.

### 12. Platform matrix, sharding and cost

**Per-run figures, from the rows.** The Linux column is warm runner-minutes.
macOS is budgeted at 1.5× and Windows at 2×. Each shard also pays about 3
minutes of setup: restore `npm` and browsers, then install the bundle and OTP
from the run's artifact.

| Layer / journey | linux | macos-arm64 | macos-x86_64 | windows | Linux min |
|-----------------|:-:|:-:|:-:|:-:|--:|
| Contracts (30 leaf commands) | ✓ | ✓ | ✓ | ✓ | 10 |
| J1 Install (with edge cases), J2 Lifecycle (with escript) | ✓ | ✓ | ✓ | ✓ | 8 |
| J3 Registry + HTTP + offline | ✓ | ✓ | ✓ | ✓ | 7 |
| J4 Releases | ✓ | ✓ | ✓ | ✓ (TLS incubating) | 6 |
| J5 Cluster | ✓ | ✓ | ✓ | incubating | 5 |
| J6 Live dev | ✓ | ✓ | ✓ | ✓ (ConPTY) | 6 |
| J7 LiveView | ✓ | ✓ | ✓ | — (ADR 0091) | 7 |
| J8 Upgrade | ✓ | ✓ | ✓ | ✓ | 7 |
| J10 Workspaces | ✓ | ✓ | ✓ | ✓ | 4 |
| J9 Canaries (11 packages) | ✓ | weekly | weekly | weekly | 25 |
| Adversarial, public categories (6) | ✓ | ✓ | ✓ | ✓ | 9 |
| Adversarial, private repo (2) | ✓ | ✓ | ✓ | ✓ | 4 |
| `.vsix` smoke | ✓ | ✓ | — | ✓ | 3 |
| **Functional total** | **101** | **114** (76 × 1.5) | **110** (73 × 1.5) | **138** (69 × 2) | |
| Linux max-major leg (contracts, J1 to J4, J6) | 37 | | | | |
| Perf (14 measurements × 7 runs, dedicated) | 20 (gate) | 30 (gate) | 30 (report, weekly and RC) | 40 (report, weekly and RC) | |

**Shard plan:**

| Target | Shards | Per-shard work | Wall |
|--------|-------:|---------------:|-----:|
| linux min-major | 6 (J9 4-way inside one) | ≈ 17 + 3 | ≈ 20 min |
| linux max-major | 2 | ≈ 19 + 3 | ≈ 22 min |
| macos-arm64 | 6 | ≈ 19 + 3 | ≈ 22 min |
| macos-x86_64 | 6 | ≈ 18 + 3 | ≈ 21 min |
| windows | 8 | ≈ 17 + 4 | ≈ 21 min |

Perf runs as one job per gated target, in parallel with the functional shards.

| Run | Runner-minutes | Wall time | How it adds up |
|-----|---------------:|----------:|----------------|
| Nightly | ≈ 635 | ≈ 30 to 60 min | Functional 101 + 37 + 114 + 110 + 138 = 500. Setup on 28 shards ≈ 85. Gated perf 20 + 30 = 50. |
| Release candidate | ≈ 705 | ≈ 35 to 75 min | Nightly plus report-only perf on macos-x86_64 and Windows (+70). |
| Weekly on `latest` | ≈ 830 | ≈ 45 to 75 min | RC plus J9 on macOS and Windows (25 × 1.5 × 2 + 25 × 2 = 125). |
| Per edge publish (edge-fast) | ≈ 34 | ≈ 15 min | Contracts 10 + J1/J2 8 + J4 6 + fast adversarial 4 = 28 over 2 shards, plus setup 6. |
| Per active hour (edge-hourly) | ≈ 105 | ≈ 20 min | Linux J3 7 + J6 6 + J9 25 = 38 over 4 shards, plus setup 12 = 50. macOS fast set 28 × 1.5 = 42 over 2 shards, plus setup 6, plus the macOS edge build in beamtalk ≈ 7 (warm). |

The wall-time ranges depend on macOS runner concurrency. GitHub caps concurrent
macOS jobs per account. A release candidate's 12 macOS shards can queue behind
each other, and behind the hourly edge macOS leg, which is why the upper ends
are higher.

**Per day, at about 50 merges a day** (`main` took 51 first-parent commits on
2026-11-25):

- Edge-fast on every publish is ≈ 50 × 34 ≈ **1,700**. `cancel-in-progress`
  coalesces bursts, so expect less.
- Edge-hourly over about 16 active hours is ≈ **1,700**, of which ≈ 900 is
  macOS.
- Nightly is ≈ **635**.

The total is **≈ 4,000 runner-minutes a day**, ≈ 1,200 of it macOS.

Running every journey plus a macOS leg on every merge, as the earlier draft
did, would have cost ≈ 50 × 125 ≈ 6,000 a day before nightly. That is why the
heavy edge layers and the macOS leg are coalesced to hourly.

**If even this is too much,** the next lever is to coalesce edge-fast too: run
it on at most one publish per 15 minutes. That keeps "a Bug within the hour"
and roughly halves the edge-fast line.

**BT-5273 lessons, applied:**

- **No compiled harness.** `npm ci` from a cache replaces the cold cargo build.
- **Explicit cache keys:** the bundle comes from the run artifact, `npm` by its
  lockfile, browsers by the Playwright version, OTP by major, and J9's deps by
  the index HEAD.
- **Cold where the user is cold.** J3, `compile.cold` and
  `registry.resolve.cold` always run cold.
- **Only `main` and scheduled runs save caches.**
- **Job timeouts** are the measured cold p95 plus 50%, and the step hang guards
  fire well inside them.
- **Pinned runner images** (§ 3, pinned on both sides in Phase 0).

### 13. LiveView feasibility

Feasible, with beamtalk changes in Phases 2 and 7:

- **The artifact exists.** `liveview-release.yml` builds ERTS-embedded IDE
  archives for linux-x86_64, macos-arm64 and macos-x86_64. `bin/server
  <workspace-id>` attaches to a local workspace
  (`docs/deployment/remote-liveview-ide.md`).
- **The browser side is proven.** `liveview-e2e.yml` already drives the IDE with
  PhoenixTest and Playwright, from source. J7 does the same from published
  artifacts.
- **Where uat gets the artifacts.**
  - For tags, the IDE archives are attached to the pre-release and the image is
    pushed as `rc-<sha>` (§ 10 step 1).
  - For nightly, `nightly.yml` must publish IDE archives to the `nightly` release
    (S-sized).
  - Edge does not carry the IDE, so J7 is not in the edge layers.

### 14. What happens to the 16 current scenarios

- `cli_*` contracts move into Layer 1, now schema-checked where JSON exists.
- `lsp/*` move into J6, against a real workspace.
- `mcp_*` move into J6's MCP flow.
- `smoke` moves into J2.

The rewrite PR lists the mapping, and no contract is dropped silently.

## Prior Art

- **swamp-uat** is the model (§ Context).
  - **Adopted:**
    - the separate repository and per-release dispatch;
    - shipped-artifact-only testing per target;
    - one file per (leaf) command with global flags;
    - schema-validated JSON;
    - its adversarial categories;
    - 2 + 5 iterations gated on p75 with a CI multiplier;
    - "tests are more accurate than the code";
    - permanent regression tests;
    - promotion blocking.
  - **Extended:**
    - cross-surface journeys, because Beamtalk's user surface includes nodes,
      clusters, an IDE and an agent protocol;
    - an OTP axis, because users bring their own runtime;
    - downstream canaries;
    - incubate-then-graduate, so promotion doesn't freeze;
    - verified promotion in a protected Environment;
    - private handling of security reds.
  - **Changed:**
    - the runner: Playwright Test, for multi-process steps and the browser;
    - edge runs don't block, because edge is per merge and not a promotion
      candidate.
- **Rust's crater** builds and tests the crates.io ecosystem against a candidate
  compiler. J9 is a small crater, with the registry index as its corpus.
- **Phoenix's installer integration tests** generate a project, fetch deps from
  Hex and run its tests. J2 and J3 share that motivation.
- **Kubernetes conformance and e2e** use tag-based tiers, collect environment
  logs on failure, and promote tests from `[Feature]` to `[Conformance]` once
  they are stable. The incubating → blocking rule follows that.
- **Erlang/OTP `common_test` with `peer`.** Adopted: unique node names and
  cookies per test, and no node outliving its test.
- **Smalltalk family.** Pharo CI loads baselines into a fresh image from a
  published VM. The journey model goes further, because Beamtalk's surface
  includes processes outside any image (principles §4).

## User Impact

- **Newcomer.** J1 to J3, J10 and the command contracts are their first hour,
  checked on their OS and OTP before they can install the release.
- **Smalltalk developer.** J6 and hot reload under load check the promise they
  came for.
- **Erlang/BEAM developer.** J4, J5 and the distribution-security tests check
  releases, `rpc`, TLS and cookies the way OTP teams deploy, on both supported
  OTP majors.
- **Production operator.** Releases are promoted only after the launcher,
  upgrade path, leak checks and `release.start` timing have passed, and a
  promotion is provably the bytes that were tested.
- **Tooling developer and agent user.** MCP through the official SDK, LSP
  through VS Code's library, the `.vsix` itself, and the IDE as shipped.
- **Package authors.** A toolchain change that breaks their package goes red on
  the beamtalk side within the hour.
- **Beamtalk contributors.** There is no added PR time without `ci:uat`. In
  return, an edge red files a Bug within the hour, and uat can't be "fixed" by
  loosening it without the owner.

## Steelman Analysis

### Evolve the current Rust harness (add cross-surface `[[journey]]` to `expect.toml`)
- 🧑‍💻 **Contributor:** "It exists and is green. Extend it rather than relearn."
- ⚙️ **BEAM veteran:** "Rust matches beamtalk's tooling, and hand-rolled
  clients test framing exactly."
- 🎨 **Designer:** "Declarative TOML is data that a non-programmer can add."
- **Response:** TOML is the "arg parsing" ceiling. Journeys, adversarial timing
  and percentile benchmarks need control flow. Rust brings back the BT-5273
  cold-build cost. Framing is covered in-repo (§ 4).

### Fold the journeys into beamtalk's own e2e suite
- 🧑‍💻 **Contributor:** "One repository, and the failure sits next to the code."
- 🏭 **Operator:** "Per-PR is earlier than per-merge."
- **Response:** in-repo tests build from source by construction, so they can't
  see packaging, the installer, the user's OTP, published IDE archives or the
  registry, which is where these regressions surfaced. Per-PR stays available
  through `ci:uat`.

### Keep uat as a smoke test
- 🏭 **Operator:** "A small gate is a reliable gate. Flaky gates get ignored."
- **Response:** every 2026-11-26 miss was cross-process or cross-repository.
  Reliability comes from:
  - incubate-then-graduate;
  - failure classes;
  - `superseded` and `infra` never blocking;
  - perf gated only where it's stable.

  The 80/20 phasing (Implementation) makes the gate real after three phases.

### Write the harness in Beamtalk (dogfood)
- 🎩 **Smalltalk purist:** "`Subprocess`, `ReactiveSubprocess` and BUnit exist."
- **Response:** the judge must not share a failure mode with the accused.
  Dogfooding happens in the fixtures.

### Erlang `common_test`
- ⚙️ **BEAM veteran:** "Native `peer` nodes, hooks and logs."
- **Response:** strong for J4 and J5, but it has no browser, MCP, LSP or VS Code
  test client.

### TypeScript on Deno with Zod (swamp's exact stack)
- 🎨 **Designer:** "Proven for this exact job, sandboxed by default."
- **Response:** a close second, and Zod is kept. Playwright Test wins on
  built-in steps, timeouts, traces, sharding, and the browser J7 needs.

### Python, pytest and playwright-python
- **Response:** defensible. It loses on built-in reporting and on alignment with
  the official MCP SDK and beamtalk's TypeScript tests.

### Tension points
- **Per-PR coverage against cost.** Edge-fast runs on every publish; the heavy
  layers and macOS are hourly; per-PR is opt-in.
- **Strict perf thresholds against shared-runner noise.** Dedicated jobs,
  calibration, gating only on two targets, and the paired-ratio diagnostic.
- **Strict blocking against a growing suite.** Incubate-then-graduate.

## Alternatives Considered

Each is steelmanned above:

- **Evolve the current repo:** rejected.
- **Fold into beamtalk e2e:** rejected; kept as `ci:uat`.
- **Smoke only:** rejected.
- **Harness in Beamtalk:** rejected for the judge, adopted for the fixtures.
- **`common_test`:** rejected.
- **Deno with Zod:** a close second. Zod is adopted.
- **pytest:** rejected.
- **bats or shell:** can't hold a pty, an MCP client and a browser in one
  journey.
- **Draft releases for candidates:** rejected (§ 10). They would need a token to
  download, which breaks the documented install path.
- **Edge-on-every-merge for every layer and macOS:** rejected on cost (§ 12),
  in favour of edge-fast plus edge-hourly.

## Consequences

### Positive
- The 2026-11-26 incident classes become reds before users see them, and by
  Phase 2 the gate blocks them.
- Releases reach users, the Marketplace and ghcr only after every blocking test
  passes on every target and OTP, and only as the exact bytes tested.
- User-level performance regressions get a gate. Until now only micro-benchmarks
  existed.
- Adversarial coverage of cookies, TLS, secrets, locks and atomic writes exists
  for the first time against shipped artifacts, without publishing security
  findings.
- The suite compounds, and new tests can't freeze promotion.
- Rule 2 has a real gate on expected values (owner-gated paths and code-owner
  review), plus an honest tripwire.

### Negative
- A rewrite. The current harness and its 16 scenarios are discarded, though
  their contracts survive.
- A TypeScript codebase to maintain, plus a private companion repository.
- **Cost:** ≈ 4,000 runner-minutes a day at today's merge rate (≈ 1,200 of them
  macOS), and ≈ 705 per release candidate.
- Releases are delayed by about 35 to 75 minutes, longer when red. A red
  candidate uses up its version number, and anyone pinning it can install it
  (§ 10).
- Agents in beamtalk-uat need their own GitHub identity (the Claude App) so the
  owner can review their PRs.
- The `governance.yml` tripwire can be evaded by a determined refactor. The
  owner-gated paths are the actual guarantee.
- Real-network tests depend on GitHub. `infra` classification limits the damage.
- Because canaries block promotion, a released package that breaks under a
  correct toolchain change holds up the release until its maintainer ships or
  the owner waives. The details are pending review finding 4.
- **The Tauri desktop app is not tested by uat**, so "ships after uat passes"
  means after the toolchain, IDE and `.vsix` pass, not the desktop shell itself.
- Official client libraries can't see a framing bug every real client
  tolerates. This is intended.

### Neutral
- `cross-repo.yml` is retired in favour of J9.
- `release.yml` and the four channel workflows split into build-and-attach and
  publish. Publishing moves behind `promote.yml` in a protected Environment.
- `edge.yml` gains a `macos-arm64` asset, `install.sh --edge` gains macOS, and
  runner labels are pinned in beamtalk.
- Micro and dispatch benchmarks and coverage stay in beamtalk, unchanged.

## Implementation

Phases follow the 80/20 point: Phases 0 to 2 cover all four 2026-11-26 incidents
**and** make promotion real. Every later layer lands incubating and graduates
(§ 10), so nothing added after Phase 2 can freeze a release.

| Phase | Scope | Size | Why this order |
|-------|-------|------|----------------|
| **0. Skeleton, governance, artifacts** | **uat:** Playwright Test project; `env`, `toolchain`, `otp`, `proc` and `artifacts` libraries; the `resolve` job with download-once fan-out and `superseded`; Zod `schemas/`; leak checks; failure bundle; 4-target × OTP workflow; `CLAUDE.md` rules; CODEOWNERS, `targets.toml`, branch protection, Claude App identity, `governance.yml` tripwire; **J1**, **J2**, and Layer 1 for the 7 already-covered commands; delete the Rust harness; accept both event types. **beamtalk:** pin runner labels; `edge.yml` publishes `macos-arm64`; `install.sh --edge` on macOS; `edge.yml` and `nightly.yml` dispatch with the § 3 payload. | M (uat) + S (beamtalk) | Everything builds on it. |
| **1. Canaries, releases, triage** | **J9**, isolated (`permissions: {}`), in edge-hourly; **J4** on linux and macOS, including the TLS step (the permanent BT-5264 test); `triage.yml` with fingerprints, classes and repo-scoped secrets; retire `cross-repo.yml`. | M | Catches BT-5283 and BT-5264 within the hour. |
| **2. Thin promotion** | **beamtalk:** split build-and-attach from publish in `release.yml`, `release-vscode.yml`, `liveview-release.yml`, `liveview-docker.yml` and `desktop-release.yml`; tags publish a pre-release with every asset attached and the `rc-<sha>` image; `promote.yml` in the `release-promotion` Environment, with re-hashing, run verification and owner review for the first three; the post-promotion J1 smoke. **uat:** the `uat-gate` dispatch back. Blocking set: contracts (7), J1, J2, J4, J9 on tags. | M | The gate becomes real. Everything after this lands incubating. |
| **3. Remaining contracts, registry, workspaces** | Layer 1 for the remaining 23 leaf commands, plus the missing-JSON feature Bugs; **J3** with offline; **J10**; J1's install edge cases and J2's escript step. All incubating. | M | The newcomer's first-service path. |
| **4. Adversarial layer** | The six public categories in uat; `beamtalk-uat-security` (private) for distribution security and leakage, with its triage. Incubating; findings filed. | M | Needs Phase 1's release and node handles. |
| **5. Performance** | `lib/bench.ts` (2 + 5, p50/p75/p95, paired-ratio column); the 14 measurements; 10 report-only nightlies; the owner sets `thresholds.toml`; gating turned on for linux and macos-arm64. | M | Calibration needs real nightly data. |
| **6. Live development and editors** | **J6** (REPL pty with ConPTY, MCP via the SDK, LSP workspace mode); the `.vsix` smoke. | M | Interactive-first is principle #1. |
| **7. Cluster, upgrade, LiveView** | **J5**, **J8**; `nightly.yml` publishes IDE archives (S, beamtalk); `ide.ts`, **J7** and the `rc-<sha>` image variant. | L (+S) | The most harness work. |
| **8. Opt-in PR runs** | A `ci:uat` label in beamtalk builds a linux bundle and calls uat's reusable `gate.yml` (edge-fast). | S | Nice to have once the rest is stable. |

**Affected components:**

- **beamtalk-uat:** everything.
- **beamtalk-uat-security** (new, private): the two security categories.
- **beamtalk:**
  - `release.yml`, `edge.yml`, `nightly.yml`, `liveview-release.yml` (pinned
    runners, dispatch, `macos-arm64` edge asset, nightly IDE archives);
  - `release-vscode.yml`, `liveview-release.yml`, `liveview-docker.yml`,
    `desktop-release.yml` (build-and-attach / publish split);
  - a new `promote.yml` and the `release-promotion` Environment;
  - `scripts/install.sh` (`--edge` on macOS) and `scripts/ci/package-release.sh`
    (the `macos-arm64` edge asset);
  - `cross-repo.yml` (removed);
  - `docs/development/testing-strategy.md`, `releasing.md`, `benchmarks.md`;
  - `docs/beamtalk-installation.md` (the apt OTP instruction, per J1's finding);
  - `AGENTS.md` (`ci:uat`).
- **No compiler, runtime or stdlib changes.** Findings are filed and fixed under
  their own issues.

## Migration Path

There are no user-facing changes. For maintainers:

- The old Rust harness runs until J1, J2 and the Phase 0 contracts are green on
  all four targets against `latest`. It is deleted in the PR that switches
  `uat.yml`.
- uat accepts both `beamtalk-release` and `beamtalk-artifact` until the release
  after Phase 0 ships.
- `cross-repo.yml` is deleted after J9 has covered beamtalk-http for a week with
  no gaps.
- Perf runs report-only until calibrated (Phase 5), and every post-Phase-2 test
  lands incubating, so no phase can block a release on day one.

## Owner Decisions (2026-11-26)

These were raised as open questions in the first draft and settled by the owner.
They are folded into the sections above:

1. **Canary blocking:** yes. A J9 red on a package's latest tag blocks
   promotion (§ 10). How warnings are treated is pending: see review finding 4.
2. **Pre-release, then promote:** yes. The VS Code, IDE, Docker and desktop
   *publishing* runs from `promote`, so editors and the IDE ship only after uat
   passes (§ 10, Phase 2).
3. **Edge on macOS:** yes. `edge.yml` builds `macos-arm64`, and uat runs the
   macOS fast set on the latest edge every hour, coalesced for cost (§ 10, § 12).
4. **Triage secrets:** beamtalk-uat holds its own Linear and Claude secrets,
   scoped to that repository and to the triage job (§ 11).
5. **J5 on Windows:** incubating at first, then blocking after about two weeks
   green, under the general graduation rule (§ 10).

The user-level performance gates (§ 8) and the split that leaves micro and
dispatch benchmarks and coverage in beamtalk are also owner decisions.

## References

- Related issues:
  - BT-2253 (uat epic), BT-2449 (release → uat dispatch), BT-2453 (`uat-gate`
    status), BT-2476 (scaffold fmt-clean), BT-2480, BT-2497;
  - BT-5066 (foreground REPL cookie);
  - BT-5283 and BT-5290 (return-type check broke downstream);
  - BT-5264 (macOS epmd shadowing);
  - BT-5233 and BT-5256 (TLS distribution);
  - BT-5273 (cold-cache timeouts).
- Related ADRs: ADR 0014 (test framework), ADR 0073 (package distribution), ADR
  0091 (remote LiveView IDE), ADR 0099 (CLI application story, `--escript`), ADR 0125 (OTP
  releases and upgrade compatibility), ADR 0126 (location-transparent actors).
- Documentation:
  - `docs/development/testing-strategy.md`;
  - `docs/development/deploying.md`, `docs/development/releasing.md`,
    `docs/development/benchmarks.md`;
  - `docs/beamtalk-packages.md` (§ Package Registry);
  - `docs/beamtalk-installation.md`, `otp-support.toml`;
  - `docs/deployment/remote-liveview-ide.md`.
- External:
  - Paul Stack, [*The Gate Between Our Agent Code and Our
    Users*](https://stack72.dev/the-gate-between-our-agent-code-and-our-users/)
    (swamp-uat);
  - beamtalk-uat `CLAUDE.md`;
  - [Playwright Test](https://playwright.dev/docs/test-intro);
  - [Zod](https://zod.dev);
  - [`@vscode/test-electron`](https://github.com/microsoft/vscode-test);
  - Rust's crater.
