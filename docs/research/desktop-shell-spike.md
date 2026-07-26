# Desktop Attach Shell Spike — Tauri vs No-Shell Coordinator

**Issue:** BT-2984
**Date:** 2026-07-27
**Status:** Complete — recommendation: **lean Tauri, but the shell decision is
not actually settled by this spike.** Six of the ADR's seven exit criteria
turned out to be shell-agnostic (satisfied identically by either alternative),
which leaves the decision resting entirely on criterion (e) — the one
criterion this spike could **not** validate hands-on (no display server, no
Tauri toolchain, no target OSes in this sandbox; see Scope below). This is
"undecided pending a real (e) test," not a confirmed yes — see Verdict below.
**Throwaway code:** `spikes/desktop-attach-spike/` (this branch, not merged to
main — see that directory's README for how to reproduce every result below)

## TL;DR

ADR 0097's spike exit criteria are (a)-(g), decided **per-duty, not vibes**: only
commit to the Tauri shell if the broker's responsibilities prove they need to
live outside the BEAM. This spike built **both** alternatives against **real**
workspaces, a **real** built `dist-liveview` release, and a **real** no-shell
Elixir/Plug coordinator — no criterion here is asserted from reading code.

The headline finding: **six of the seven duties (a, b, c, d, f, g) are shell-agnostic.**
A ~300-line throwaway Elixir/Plug "coordinator" (`coordinator/coordinator.exs`)
satisfies discovery, spawn, two-stage readiness probing, loopback/no-OIDC/
check_origin enforcement, post-attach lifecycle, and orphan reaping **exactly as
well as** a bash "broker" standing in for the eventual Rust/Tauri one — both
share the identical spawn primitive (`dist-liveview/bin/server`), so neither
gets an edge from being in a different language. The **only** duty that
structurally distinguishes the two shells is **(e), webview/keybinding parity**
— and that is exactly the criterion the ADR named as the load-bearing one.
Desk research (not hands-on — see Scope below) supports Tauri's native
menu-accelerator + window-level close interception as real and documented, at
the cost of a well-documented WebKitGTK-on-Linux stability/performance risk.

**Recommendation: keep Tauri as the shell**, but treat (e) as still open pending
a cheap, real cross-platform QA pass (see "What's not settled" below) — the
other six criteria no longer justify Tauri on their own; only (e) does, and it
is the one criterion this spike could not test hands-on.

## Scope: what this spike could and couldn't run

This spike ran on a headless Linux (WSL2) sandbox: no display server, no
`cargo-tauri` toolchain, no macOS/Windows targets. That shaped which criteria
got **live, real evidence** vs **desk research**:

| Criteria | Method |
|---|---|
| (a) (b) (c) (d) (f) (g) | **Live**: real `beamtalk workspace create`, a real built `dist-liveview` release (`just dist-liveview`), real epmd/distribution, real HTTP/websocket probes |
| (e) webview parity | **Desk research only** (Tauri docs, GitHub issues) — no display, no Tauri CLI, no target OSes available here |

## (a) Two-instance boot — PASS, after finding a real release-boot bug

Validated live: two fronts on two different workspaces, and — the sharper case
the ADR calls out explicitly — **two fronts attached to the same workspace**,
both alive simultaneously with distinct snames:

```
name bt_attach_spike-a_264798 at port 37349
name bt_attach_spike-a_264595 at port 45533
```

**Finding (real bug, not spike-only): `mix release`'s generated launcher boots
the VM already distributed under `-sname bt_attach`** (`RELEASE_NODE` defaults
to `RELEASE_NAME`) **before any Elixir code runs.** This pre-empts
`ensure_distributed/0`'s `BT_ATTACH_NODE_SUFFIX` seeding entirely — `Node.alive?/0`
is already `true` by the time the front's own code runs, so the sname-seeding
BT-2983 shipped is dead code under the actual release artifact. Reproduced
directly:

```
$ PORT=... BT_ATTACH_NODE_SUFFIX=spike-b dist-liveview/bin/server spike-b
Protocol 'inet_tcp': the name bt_attach@muckish seems to be in use by another Erlang node
```

Every spawned instance collides on the identical epmd registration — this is
precisely the two-instance-boot failure (a) is designed to catch, just one
layer lower (release boot) than the ADR's authors were reasoning about (lazy
`ensure_distributed/0`). **Fix, validated live**: the broker must launch with
`RELEASE_DISTRIBUTION=none`, which makes the release boot **non-distributed**
and hands control back to `ensure_distributed/0`'s lazy, correctly-seeded path
on the first `/readiness` call. With that env var, two-instance boot (including
two-fronts-one-workspace) passes cleanly. **This is broker-core work
(BT-2985), not shell-specific** — a Tauri broker and the no-shell coordinator
both spawn `bin/server` and both need this exact env var.

Harness caveat: `broker.sh`'s `free_port()` helper (bind port 0, read the
assigned number, close, hand that number to the front) has a check-then-use
race — nothing guarantees another process doesn't grab the same port between
the close and the front's own bind. It didn't bite across this spike's runs,
but it's a known gap in the throwaway harness, not a property of the front or
broker design being validated; BT-2985's real port allocation should retry on
bind failure rather than assume a `free_port()`-style probe is race-free.

## (b) Crash → respawn of the same workspace — PASS

Killed a live front (`kill -9`); epmd dropped its registration and the port
freed immediately. Respawned with the same `BT_ATTACH_NODE_SUFFIX` on the same
port: new registration `bt_attach_spike-a_264930` (same seeded prefix, new PID
entropy), readiness 200. Freed port reused cleanly. Shell-agnostic — this is
front + spawn-env behavior only.

## (c) `/readiness` catches bad cookie / dead workspace before the window opens — PARTIAL PASS, real taxonomy bug found

The mechanism works: `/readiness` does force `connect/0` + one RPC and returns
503 with a JSON body before any window would open, for every unreachable case
tested. **But the three-way taxonomy BT-2983 shipped has a real,
environment-triggered bug**: `classify_unreachable/2`
(`editors/liveview/lib/bt_attach/workspace.ex:2105`) calls `:net_adm.names()`
with **no host argument**, which resolves via `inet:gethostname()` (the
machine's real hostname, e.g. `muckish`) rather than `:localhost`. On this
sandbox (and plausibly other environments where the local hostname doesn't
cleanly self-resolve for an epmd query — containers, some macOS/mDNS setups),
that call fails outright:

```elixir
:net_adm.names()            #=> {:error, :address}   # WRONG default
:net_adm.names(:localhost)  #=> {:ok, [...]}          # correct, explicit
```

`classify_unreachable/2`'s catch-all (`{:error, _} -> :epmd_absent`) then
**collapses both `bad_cookie` and `dead_workspace` into `epmd_absent`** —
confirmed live for both cases (a deliberately wrong cookie, and a node name
with no backing workspace at all both report `epmd_absent`), even though epmd
is reachable and correctly enumerable with the explicit `:localhost` argument.
This directly undercuts BT-2983's own acceptance criterion ("failure taxonomy
distinguishes epmd absent vs bad cookie vs dead workspace"). **Filed as a
follow-up** (see "Follow-ups" below) rather than fixed here — it's shipped,
reviewed code outside this spike's throwaway scope, and the fix is a one-line
`:net_adm.names(:localhost)` at the call site.

Caveat on generality: this spike ran on **one** host (WSL2, hostname
`muckish`), so "PARTIAL PASS" describes what was observed here, not a claim
that the taxonomy fails on every host — on a machine where the local hostname
happens to self-resolve cleanly, `:net_adm.names()` would succeed and the
taxonomy would work as shipped. What *is* general is the code-level defect:
relying on hostname self-resolution for a query that only ever needs to reach
the **local** epmd is objectively more fragile than the explicit `:localhost`
form, independent of whether it happens to bite on any particular tester's
machine — hostnames that don't loop back cleanly are common enough in
containers/CI/some mDNS setups that this is worth fixing regardless of
whether it reproduces on a "normal" dev laptop.

Shell-agnostic: entirely front-side (BT-2983) behavior; a Tauri broker and the
coordinator would both be equally misled by it today.

## (d) Loopback bind + no-OIDC + pinned `check_origin` — PASS, all three validated live

- **Loopback bind**: `ss -ltnp` on a spawned front shows `127.0.0.1:<port>`,
  never `0.0.0.0:<port>`, with `BT_ATTACH_BIND_IP=127.0.0.1`.
- **`check_origin`**: a raw websocket-upgrade request to `/live/websocket` with
  `Origin: http://evil.example` gets `403 Forbidden`; the same request with
  `Origin: http://localhost:<port>` gets `101 Switching Protocols`. **No new
  env hook was needed** — the default `check_origin: true` plus the endpoint's
  own `url:` config (which already derives host/port from `PORT`) pins it
  correctly out of the box, exactly as ADR 0097 predicted but flagged as
  "must be validated, not assumed." Only the exact `localhost:<port>` origin
  was tried; a real webview may present a slightly different origin (e.g. a
  `127.0.0.1` navigation, or whatever Tauri's webview reports as its origin
  for a `localhost` URL) — worth a quick re-check once an actual webview is
  in the loop (see "What's not settled" below, folds into the (e) follow-up).
- **Incomplete-OIDC-config fail-closed**: spawning with an *incomplete* OIDC
  env (`BT_OIDC_ISSUER` set, required keys missing) crashes the release at
  boot with a clear `RuntimeError` — the app never serves a byte of
  unauthenticated traffic. This tests the fail-closed safety net for a
  half-configured OIDC setup, not the "no OIDC config at all" case (that path
  is just the normal unauthenticated posture, already exercised by every
  other test in this write-up). Confirms the safety net BT-2983/ADR 0091
  designed is real; the broker's job is simply to *check* for OIDC config
  **before** spawning (to give a friendly error instead of a boot crash), not
  to re-implement the enforcement.

Shell-agnostic: all three are env-vars-plus-front-config; both shells set the
identical env before invoking `bin/server`.

## (e) Webview parity — DESK RESEARCH ONLY, the one real differentiator

Could not hands-on test: no display server, no `cargo-tauri`, no macOS/Windows
target in this sandbox. What the research supports:

- **Tauri's actual mechanism for owning `Cmd/Ctrl-W` is the native window's
  menu/accelerator table plus the window-level `CloseRequested` event**
  (`onCloseRequested` → `event.preventDefault()`), not the `global-shortcut`
  plugin (that plugin is for *system-wide* hotkeys when the app isn't
  focused — a different mechanism). This is documented and real: a native
  window's close-request is an OS window message the shell's Rust code
  intercepts *before* the webview ever sees a key event — categorically
  different from a browser tab's `keydown`, which the browser chrome consumes
  first, so `preventDefault()` never fires (ADR's own framing, confirmed by
  the existence of `tauri-plugin-prevent-default`, a plugin that exists
  specifically because webviews default-handle several browser shortcuts that
  need explicit suppression).
- **This is a capability the no-shell coordinator's PWA path cannot match, by
  platform construction, not by remaining engineering effort.** An installed
  PWA window is still a browser surface — the OS/browser chrome owns
  `Ctrl/Cmd-W` and a page cannot intercept it. No amount of JS changes that.
- **Real, documented risk on the other side**: a Tauri/WebView2 GitHub issue is
  titled exactly *"Some default platform shortcuts are disabled, some are not"*
  — i.e., even within Tauri, shortcut-interception behavior is inconsistent
  across specific keys on Windows, not a clean solved problem. And WebKitGTK
  on Linux has multiple **open, upstream** issues directly relevant to a
  LiveView-heavy UI: websocket "network process crashed" errors, high
  `WebKitWebProcess` CPU usage tied to websocket traffic, and general
  DOM/rendering slowness under load — all things a live-updating IDE hits
  constantly. These are upstream WebKitGTK limitations, not something Tauri
  application code can paper over.

**Net**: the *capability* argument for Tauri is real and technically
substantiated, not "asserted from browser behaviour" as the ADR worried it
might be — but it is also not risk-free, and this spike could not verify it by
actually running a webview. That gap is the one piece of unfinished business
below.

## (f) Post-attach lifecycle — PASS, both halves validated live

- **Workspace killed while attached → window reflects it, not a wall of
  errors**: with a healthy attached front, `beamtalk workspace stop spike-a`
  was run directly. The front process **did not crash** — it kept serving HTTP
  (root page still 200) and `/readiness` cleanly returned 503 JSON (misreported
  as `epmd_absent` due to the (c) bug above, but structurally a clean error,
  not an exception wall). A polling broker/coordinator would see this
  transition and grey the window.
- **Sleep/resume → front reconnects**: rather than guessing, this was
  reproduced directly by connecting a throwaway debug node (using the
  release's own persistent cookie, `dist-liveview/releases/COOKIE`) to a live
  front and calling `:erlang.disconnect_node/1` on the workspace peer.
  `Node.list()` confirmed the link was gone; the **very next** `/readiness`
  call returned `200` again, because `connect/0` is idempotent and re-runs
  `Node.connect/1` on every call. Self-heal confirmed for a clean, mutual
  disconnect. Caveat: `disconnect_node/1` is a proxy for what OS sleep/resume
  does to a dist connection, not the literal thing — a real suspend can leave
  a half-open socket (one side thinks it's connected, the other doesn't) that
  behaves differently than a clean teardown, and `net_ticktime` timeout
  behavior wasn't exercised. The idempotent-`connect/0` mechanism this relies
  on should generalize, but "self-heal confirmed" here means confirmed for the
  clean-disconnect case specifically.

Shell-agnostic: entirely front-side behavior already shipped in BT-2983; both
shells just poll `/readiness` and react to the transition.

## (g) Orphan-reaping — PASS, PID-file sweep concretely prototyped

Chose **PID-file sweep** over process-group kill or a parent-death watch
(`PR_SET_PDEATHSIG`) because it's the only one of the three portable to Windows
without OS-specific syscalls — the ADR's packaging phase (§5) targets macOS +
Linux + Windows, and process groups / pdeathsig are Unix-only mechanisms.

Demonstrated live end-to-end: spawned a front via `setsid` (so it is not a
`wait()`-able child of its spawning shell — the same relationship a real
desktop broker has to its children), confirmed via `ps -o pid,ppid,pgid,sid`
that it re-parents to init and runs as its own session leader (a genuine
orphan, not a simulated one), then ran `broker.sh sweep` (the "next broker
start" step) which detected the still-alive, untracked-by-this-session PID,
`SIGTERM`'d it (with a `SIGKILL` fallback), and cleared its bookkeeping —
verified the OS process was actually gone afterward, not just untracked.

**Known gap in the prototyped mechanism, to harden in BT-2985**: `sweep` kills
whatever PID a pidfile names with no check that it's still the *same* process
— if the orphan already died and the OS recycled its PID for something else
before the next sweep runs, a naive PID-file sweep kills an unrelated
process. This spike's single-orphan, short-lived test window never triggered
that, so it's a latent correctness gap in the mechanism, not something ruled
out by "PASS" here. The real broker should additionally record something
distinguishing (process start time, or verify the target's executable path
matches `dist-liveview/bin/bt_attach` via `/proc/<pid>/exe` on Linux or the
platform equivalent) before signaling.

**Bonus finding, relevant to the shell decision**: the no-shell coordinator
prototype needs this too, and doesn't get it for free. Killing the running
`coordinator.exs` process (`pkill -f coordinator.exs`) left its spawned front
running as an orphan (`ps` showed it re-parented to init) — the coordinator
has **no** built-in reaping, exactly matching the ADR's own prediction ("a
coordinator front would have to spawn children with the same care anyway").
Orphan-reaping is not a Tauri-only cost; it is a **spawn-a-child-process** cost,
paid by whichever shell does the spawning.

## No-shell coordinator — built, and it works

`coordinator/coordinator.exs`: a single-file, throwaway Elixir/Plug/Bandit app
(~300 lines, `Mix.install`-bootstrapped, no separate build step) that:

1. Lists `~/.beamtalk/workspaces/*` (same discovery contract as the bash
   broker — epmd liveness check, real `metadata.json` parsing).
2. On `/attach/:id`, spawns `dist-liveview/bin/server <id>` with the identical
   env the broker uses (loopback bind, id-seeded suffix,
   `RELEASE_DISTRIBUTION=none`), runs the same two-stage readiness probe, and
   redirects the browser to `http://localhost:<port>/`.
3. Tracks already-attached workspaces in memory and **reuses the existing
   front** on a repeat `/attach/:id` rather than spawning a duplicate — proven
   live both sequentially (two calls to `/attach/spike-a` returned the same
   port both times) and concurrently: an initial version used a plain
   "get, then spawn if absent" check, which an adversarial pass on this spike
   correctly flagged as racy (two near-simultaneous `/attach/:id` requests,
   each its own Bandit-spawned process, could both observe "nothing tracked"
   and both spawn). Fixed with an atomic `Agent.get_and_update/2`
   claim-or-wait (`Coordinator.State.claim_or_get/1`) and re-verified: two
   `curl` requests fired in parallel at `/attach/spike-c` resolved to the
   *same* port, and `ps`/`epmd -names` confirmed only one front process
   actually started.

Verified live end-to-end: listing shows real `alive`/`dead` status for real
workspaces; clicking Attach spawns a real front, waits for real readiness, and
redirects to a real working page. Installed as a PWA (browser "Install app"),
this delivers the dockable-window, pick-a-workspace UX the ADR credits it
with — for near-zero shell code and zero new toolchain (no Rust GUI crate, no
per-OS build matrix, no code-signing). The place it structurally cannot
compete is (e): the coordinator's spawned fronts still open as ordinary
browser tabs/PWA windows, which never get to own `Ctrl/Cmd-W`.

## Single-instance policy & attach-twice semantics — decided

**Attaching twice to the same workspace: focus/reuse the existing front, do
not spawn a second one.** Demonstrated in the coordinator (in-memory
workspace→port map, second attach redirects to the same live front) and the
policy translates directly to a Tauri broker (check its own state before
spawning; if already attached, focus that window instead of forking a new
front). This is cheaper (no duplicate ERTS process, no second port/sname to
track) and doesn't fight the sname-seeding fix in (a) — a legitimate *second*
front on the same workspace (e.g., a deliberate "detach and reconnect fresh"
action) remains possible and collision-free because of the id+OS-pid seeding,
but is not the default behavior of clicking Attach twice.

**The broker/coordinator process itself should be single-instance** (OS-level
lock or Tauri's single-instance plugin) so a second dock-icon launch focuses
the existing picker rather than starting a second supervisor that would race
on port allocation and pidfile bookkeeping — consistent with every reference
precedent this ADR already cites (VS Code, Livebook, Docker Desktop are all
single-instance apps).

## Verdict

Per-duty, as the ADR demands:

| Criterion | Verdict | Shell-differentiating? |
|---|---|---|
| (a) two-instance boot | PASS (after a real fix: `RELEASE_DISTRIBUTION=none`) | No |
| (b) crash → respawn | PASS | No |
| (c) readiness taxonomy | PARTIAL — mechanism works, real taxonomy bug found (filed) | No |
| (d) loopback/no-OIDC/check_origin | PASS | No |
| (e) webview parity | Desk research only — real capability, real risk, not hands-on tested | **Yes — the only one** |
| (f) post-attach lifecycle | PASS | No |
| (g) orphan reaping | PASS (PID-file sweep); coordinator needs it too | No |

Six of seven duties are satisfied identically by a throwaway ~300-line Elixir
page and a bash script standing in for the eventual Rust broker — **none of
them independently justify a Rust/Tauri shell**, because none of them are
actually broker-*language* concerns; they're all "what env do you set before
running `bin/server`" concerns, answerable in any language that can spawn a
process and poll HTTP.

**The Tauri recommendation survives this spike on (e) alone** — OS-reserved
keybinding ownership is real, documented, and structurally unavailable to any
browser-tab/PWA-based alternative. That is exactly the ADR's own framing
("the spike's exit criteria... decide the shell," "this is a property the
no-shell coordinator's PWA path cannot match"), and this spike's desk research
does not contradict it.

**Recommendation: lean Tauri, but say plainly that this spike did not settle
the shell decision.** Restated without the soft-yes framing: every criterion
this spike could actually *run* turned out not to require Tauri — a
throwaway Elixir page matched it duty-for-duty, including finding real bugs
neither implementation was immune to. The entire case for Tauri rests on
(e), and (e) is the one criterion this spike validated by reading Tauri's own
documentation and GitHub issues, not by running a webview. Two of the other
six criteria surfaced real, previously-unknown bugs the moment they were
actually run (the release-boot sname collision, the readiness taxonomy
collapse) rather than when merely reasoned about — which is exactly the
failure mode a desk-research-only pass on (e) is equally exposed to. Treat
the shell decision as **open, leaning Tauri on documented capability**, not
closed, until (e) gets the same hands-on treatment before BT-2985/BT-2986
commit to a Tauri-specific broker implementation.

## What's not settled — recommended before committing further Tauri work

- **Hands-on (e) validation**: a throwaway `cargo tauri init` + a manual QA
  pass — register a custom `Cmd/Ctrl-W` action via the native menu/
  `CloseRequested` path on at least Linux (WebKitGTK, available in most CI
  runners) and ideally macOS/Windows — before BT-2985 bakes a Tauri-specific
  broker shape in. Cheap relative to the packaging lane (BT-2987) that already
  has to stand up per-OS builds; doing it earlier means an (e) surprise doesn't
  surface after BT-2986's picker UI is built on top of a Tauri assumption.
- **WebKitGTK risk budget**: if the hands-on pass reproduces the websocket
  network-process-crash or high-CPU issues found in desk research under real
  LiveView traffic, that's grounds to revisit — not necessarily flip to the
  coordinator, but possibly scope Linux support differently (e.g. document a
  minimum WebKitGTK version, or fall back to the coordinator specifically on
  Linux while keeping Tauri on macOS/Windows — a split this spike didn't
  explore because it wasn't asked to).

## Follow-ups filed

- **`classify_unreachable/2` hostname bug** (criterion (c)): `:net_adm.names()`
  (no host arg) resolves via the machine's real hostname instead of
  `:localhost`, collapsing `bad_cookie`/`dead_workspace` into `epmd_absent` on
  any host where the local hostname doesn't cleanly self-resolve for an epmd
  query (confirmed on this WSL2 sandbox; plausible on other environments too).
  One-line fix (`:net_adm.names(:localhost)`) at
  `editors/liveview/lib/bt_attach/workspace.ex:2105`. Filed as **BT-3003**
  rather than fixed in this spike branch — it's shipped, reviewed BT-2983 code,
  out of this spike's throwaway scope.
- **`RELEASE_DISTRIBUTION=none` requirement** (criterion (a)): not a bug to
  fix, but a broker-core requirement to carry into BT-2985 — without it, every
  spawned front collides on `bt_attach@<host>` at the release-boot level,
  before `ensure_distributed/0`'s sname-seeding ever runs. Documented here and
  in `spikes/desktop-attach-spike/broker/broker.sh`'s `_spawn` comment; BT-2985
  should treat it as a required env var in the real broker, not an optional
  detail.

## How to reproduce

See `spikes/desktop-attach-spike/README.md` for the full walkthrough (broker
CLI reference, coordinator run instructions, and the exact commands used for
each criterion above).
