# Deploying a Beamtalk Release

This guide is the operator-facing companion to [ADR 0125](../ADR/0125-otp-releases-and-upgrade-compatibility.md)
(OTP Releases and Upgrade Compatibility, Implemented). It covers `beamtalk
release`, the `bin/<name>` launcher, and the container/service patterns
around them. For the design rationale behind any of this, the ADR is the
source of truth — this guide only restates what an operator needs to act on.

## The three-command story

```bash
$ beamtalk release
Built release orders-1.4.0 (with ERTS 16.0.2, linux/x86_64).
  → _build/release/orders-1.4.0.tar.gz  (48.2 MB)

$ _build/release/orders-1.4.0/bin/orders foreground
[orders 1.4.0] OrdersSup started; console off (see [release] console)

$ _build/release/orders-1.4.0/bin/orders rpc "Beamtalk releaseInfo"
```

`beamtalk release` requires a root supervisor — `[application] supervisor`
in `beamtalk.toml`. A project without one gets a build error naming
`beamtalk build --escript` as the right artifact for a one-shot script; a
release is a long-running service.

The output lands at `_build/release/<name>-<vsn>/` (override with
`--output`), plus a `<name>-<vsn>.tar.gz` tarball next to it — the artifact
you `COPY` into a container or copy to a server. An existing
`--output` directory is replaced only if it carries the `.beamtalk-release`
marker the command writes (or an older release's provenance manifest); the
previous output is kept until the new release is assembled, and restored if
the build fails. An empty directory is always accepted. `--force-output`
replaces an unmarked directory only if it is release-shaped (a `releases/`
directory and nothing but `bin/`, `lib/`, `releases/`, `erts-*/` at the top
level); any other directory is refused, forced or not. A filesystem root, the
home directory, the project root, the current directory and their ancestors,
and the project's source directories (`src/`, `test/`, `native/`, `priv/`,
`deps/`, `.git/`) and anything inside them are always refused. An existing tarball
beside the output is replaced only if `beamtalk release` wrote it, or, with
`--force-output`, if it is at least a gzip'd tar archive.

A run killed part-way is cleaned up by the next one. While a previous
release is parked, a `<output>.beamtalk-swap` journal sits beside the output:
until the new release is complete the previous one is restored; once the
journal says `committed`, the new release is kept and the leftover old copy
removed.

Each run holds an exclusive lock on `<output>.beamtalk-lock` for its whole
duration, taken before that recovery, so two projects (or checkouts) sharing one
`--output` cannot interleave. Like the project build lock, a second run waits
(printing a notice) rather than failing; the OS frees the lock if the holder dies. The lock file stays beside the output
(safe to delete or git-ignore), and `BEAMTALK_NO_BUILD_LOCK=1` turns it off along
with the other beamtalk file locks.

`Beamtalk releaseInfo` is the parity-neutral reflective send that names the
release, its version and the toolchain OTP release. `rpc`/`eval` resolve `Beamtalk`, `Workspace` and `Transcript` as ordinary
class names (class registry), so `rpc "Beamtalk releaseInfo"` is a plain
class-side send.

## `[release]` manifest keys

All keys are optional; shown here with their defaults (ADR 0125 §1.2):

```toml
[release]
# name          = "<package name>"  # release/OTP-application name
# apps          = []                # extra OTP apps beyond the computed closure
# include-erts  = true              # bundle this machine's ERTS
# console       = false             # start the REPL/remote-console WebSocket listener
# bind          = "127.0.0.1"       # only meaningful when console = true
# sys-config    = "config/sys.config"
# vm-args       = "config/vm.args"
# strip-beams   = false             # drop debug_info chunks
# include-compiler = false          # ship the compiler port (live-image mode)
```

Notes that are easy to get wrong:

- **There is no `version` key.** The release version is always `[package]
  version` — the repo's own single-source-of-truth rule
  ([`docs/development/releasing.md`](releasing.md)) applied to your
  project. Setting `version` under `[release]` is a manifest error naming
  `[package] version` as the fix.
- **There is no `port` key.** Erlang distribution (which `stop`/`ping`/
  `rpc`/`remote_console` all use) is always on and loopback-bound; it needs
  no port configuration. The REPL/remote-console *WebSocket* listener
  (`console = true`) has no dedicated `[release]` key for its port either —
  see [Console + reverse proxy](#console--reverse-proxy) below for how to
  set one.
- **`apps` is additive, not authoritative.** The included application set
  is computed (your project, its dependency closure, the runtime closure,
  `kernel`/`stdlib`/`sasl`); `apps` names *extra* OTP applications the
  closure can't see (an Erlang dependency reached only via FFI, say).
- **`sys-config`/`vm-args` overlay, they don't replace.** A `[release]
  sys-config = "config/sys.config"` file's top-level entries are merged
  into the generated `sys.config` (last-value-wins per key); a `[release]
  vm-args` file's lines are appended after the generated ones (last flag
  wins, per `erl`'s own arg-file reading).

## Launcher verbs

`bin/<name>` (POSIX `sh`) and `bin/<name>.cmd` (Windows) are generated
alongside the release and carry the same verbs (ADR 0027 tier 1):

| Verb | Behaviour |
|---|---|
| `foreground` (default) | Boots in the foreground. Stdout is the container's stdout — `Console` output (ADR 0099) lands there, which is what 12-factor logging wants. |
| `stop` | Graceful `init:stop()` in the running node, via distribution. |
| `ping` | Liveness check, via distribution. |
| `remote_console` | Attach an *Erlang* shell to the running node, from the same host (ADR 0091), or over TLS with [`RELEASE_DISTRIBUTION=tls`](#clustering-across-hosts). A Beamtalk `eval` inside it needs `include-compiler`. |
| `eval "Class selector [args]"` | Start a **separate** throwaway VM — the runtime closure only, no distribution, not the project's own application — dispatch one entry, halt with its exit code. |
| `rpc "Class selector [args]"` | Dispatch one entry **into the running node** over distribution and print the result. |
| `version` | Print the release name, version, `beamtalk`/`otp` toolchain versions and platform. |

`eval` and `rpc` take a class, a selector and arguments — never a source
string, because the default release ships no compiler (see
[`include-compiler` trade-offs](#include-compiler-trade-offs)).

The **cookie** every distribution-riding verb needs (`stop`/`ping`/`rpc`/
`remote_console`) comes from the `RELEASE_COOKIE` environment variable, read
fresh on every invocation — never baked into the tarball:

```bash
RELEASE_COOKIE=$(cat /run/secrets/orders_cookie) bin/orders foreground
RELEASE_COOKIE=$(cat /run/secrets/orders_cookie) bin/orders stop
```

The launcher never puts the cookie on `erl`'s command line. It writes it to a
private directory under `$TMPDIR` (`%TEMP%` on Windows), starts `erl` with that
directory as its home, and clears `RELEASE_COOKIE` so processes the node starts
do not inherit it. The node removes the directory as it boots; if it crashes
before then, a `beamtalk-cookie-*` directory
readable only by the service user is left in the temp directory (also when a
Windows `launcher.cmd` verb is interrupted with Ctrl-C and "Terminate batch
job" is answered yes). Every verb that uses the cookie therefore needs a
writable temp directory: with a read-only root filesystem (Kubernetes
`readOnlyRootFilesystem`), mount a small writable volume (an `emptyDir`) and
point `TMPDIR` at it. Applications the boot script starts before
`beamtalk_runtime` still see the private `HOME`, so they should not resolve
paths from it while starting.

## Clustering across hosts

By default a release never leaves its host: the node is `-sname
<name>@localhost`, epmd listens on `127.0.0.1`, and distribution listens on
`127.0.0.1`. To cluster nodes on two or more hosts, opt in to TLS distribution
with peer verification (ADR 0126 §9 and its amendment, decision D2). Plain TCP
distribution off the host is not offered.

| Variable | Meaning |
|---|---|
| `RELEASE_DISTRIBUTION=tls` | The opt-in. Any other value is refused. |
| `RELEASE_HOST` | This host's name as the other nodes reach it, and the name its certificate carries: a fully qualified name (with a `.`) or an IP address, since Erlang long names refuse a bare `node1` or `localhost`. The node becomes `-name <RELEASE_NODE or release name>@<RELEASE_HOST>`. |
| `RELEASE_DIST_INTERFACE` | The IP address to listen on, for distribution and epmd. There is no default; `0.0.0.0` listens on every interface only if you name it. An IPv6 address selects `inet6_tls`. |
| `RELEASE_SSL_DIST_OPTFILE` | The `-ssl_dist_optfile` file: this node's certificate, key and CA. |

The launcher checks all of this before it starts any node, and refuses with an
explanation unless the option file sets `{verify, verify_peer}` on **both** the
server and client sides and `{fail_if_no_peer_cert, true}` on the server side.
Without them TLS only encrypts the link: any client is let in. `Node connect:`
uses the same check before it connects off-host. It also refuses `verify_fun`
and `partial_chain`, which can accept a peer the CA did not sign.

Two things the check cannot see are yours to get right:

- **Trust only your own CA.** Point `cacertfile` at a private CA that signs
  nothing but your nodes. An option file that trusts public CAs (for example
  `{cacerts, public_key:cacerts_get()}`) would let in any holder of a publicly
  issued certificate.
- **Keep host-name checking on.** By default each node checks that the peer's
  certificate names the host it dialled. `{server_name_indication, disable}` or
  a permissive `customize_hostname_check` turns that off, so any certificate
  from your CA could stand in for any node.

A two-host recipe, with `web1.example.com` (10.0.0.5) and `web2.example.com`
(10.0.0.6). Issue each host a certificate for its own name from one private CA,
then write the option file on each:

```erlang
%% /etc/orders/ssl_dist.conf on web1 (web2's names its own cert and key)
[{server, [{certfile, "/etc/orders/web1.pem"},
           {keyfile, "/etc/orders/web1.key"},
           {cacertfile, "/etc/orders/ca.pem"},
           {verify, verify_peer},
           {fail_if_no_peer_cert, true}]},
 {client, [{certfile, "/etc/orders/web1.pem"},
           {keyfile, "/etc/orders/web1.key"},
           {cacertfile, "/etc/orders/ca.pem"},
           {verify, verify_peer}]}].
```

```bash
# on web1 (on web2: RELEASE_HOST=web2.example.com RELEASE_DIST_INTERFACE=10.0.0.6)
export RELEASE_DISTRIBUTION=tls RELEASE_HOST=web1.example.com \
       RELEASE_DIST_INTERFACE=10.0.0.5 \
       RELEASE_SSL_DIST_OPTFILE=/etc/orders/ssl_dist.conf \
       RELEASE_COOKIE=$(cat /run/secrets/orders_cookie)
bin/orders foreground
```

`RELEASE_HOST` must resolve, on the node's own host too, to an address the
node listens on: the local client verbs dial it. Debian and Ubuntu map the
machine's own name to `127.0.1.1` in `/etc/hosts`; with an interface such as
`10.0.0.5`, `ping` and `stop` then report the running node as down. Fix that
line, or name a wildcard interface.

Both hosts use the same cookie. Open epmd (TCP 4369) and the distribution
ports between them, and pin the distribution ports with
`inet_dist_listen_min`/`inet_dist_listen_max` in a `[release] vm-args` overlay
to keep the firewall rule small. A node then reaches the other with `(Node
named: 'orders@web2.example.com') connect`. `stop`, `ping`, `rpc` and
`remote_console` need the same four variables (and the cookie) and talk to the
local node over TLS. Each of them first runs the TLS check in a short-lived VM,
so a `ping` takes about twice as long as on loopback: allow for that in health
check timeouts.

**Unauthenticated clients and the distribution port.** A client that does
not speak TLS (for example a plain `erl -sname` node) is turned away in a few
milliseconds and does not hold up the next peer. A client that opens TCP
connections and then sends nothing does. OTP's TLS distribution listener runs
at most one pending handshake per scheduler (`erlang:system_info(schedulers_online)`),
and each one waits up to `net_setuptime` (7 s by default) for the peer's TLS
hello. A host that holds that many idle connections open stops this node from
accepting new peers until they time out, and can keep doing so. Established
connections are not affected. So:

- Firewall epmd and the distribution ports to your cluster's own hosts. This is
  the real mitigation: only hosts you trust can reach the listener at all.
- Optionally shorten the window with `-kernel net_setuptime 2` (seconds) in a
  `[release] vm-args` overlay. Keep it above the slowest TLS handshake between
  your hosts.

**macOS and a wildcard interface.** On macOS, an epmd that listens on every
interface (`RELEASE_DIST_INTERFACE=0.0.0.0` or `::`) does not stop a second,
loopback-only epmd from starting on the same port; Linux refuses the second
one. Any non-TLS Erlang node started later on that host (a loopback release,
a `bin/<name> ping` without the TLS variables, a beamtalk REPL) starts that
loopback epmd, which then answers every loopback lookup and does not know this
node. The node keeps running and other hosts still reach it, but the local
`ping`, `stop`, `rpc` and `remote_console` report it as down until that epmd
is stopped (`epmd -kill`, with its nodes stopped). On macOS, name a specific
interface address instead of a wildcard: epmd then holds loopback on that port
itself, and a second epmd cannot start there.

## Docker

```dockerfile
FROM debian:slim
COPY orders-1.4.0/ /app/
WORKDIR /app
CMD ["bin/orders", "foreground"]
```

`include-erts = true` (the default) means the image needs no Erlang/OTP
installed — the release carries its own ERTS, built for the same OS/arch it
runs on (`beamtalk release` does not cross-compile; build on, or in a
container matching, the target platform). Pass the cookie as a build
secret or runtime environment variable, never `COPY`'d into the image:

```bash
docker run -e RELEASE_COOKIE="$(cat orders_cookie.txt)" orders:1.4.0
```

## systemd

```ini
# /etc/systemd/system/orders.service
[Unit]
Description=orders
After=network.target

[Service]
Type=simple
WorkingDirectory=/opt/orders-1.4.0
# /etc/orders/release.env contains: RELEASE_COOKIE=...
EnvironmentFile=/etc/orders/release.env
ExecStart=/opt/orders-1.4.0/bin/orders foreground
ExecStop=/opt/orders-1.4.0/bin/orders stop
Restart=on-failure

[Install]
WantedBy=multi-user.target
```

`ExecStop` runs `bin/<name> stop` — a graceful `init:stop()` over
distribution, not a signal — so `systemctl stop orders` shuts the
supervision tree down cleanly rather than killing the VM. `Restart=on-failure`
is what a `foreground` process expects to sit under: the launcher does not
daemonise itself (ADR 0125 §1.7) — that's systemd's/Docker's/Kubernetes' job,
every one of which prefers a foreground process.

## Console + reverse proxy

The console (`[release] console = true`) is an authenticated WebSocket REPL
endpoint — off by default, because it is an eval-capable endpoint even at
its most restricted (see [`include-compiler`
trade-offs](#include-compiler-trade-offs)). Turning it on:

1. **Set a port.** There is no `[release] port` key, so use the documented
   `sys-config` overlay to add one to the `beamtalk_workspace` application
   env:

   ```erlang
   % config/sys.config, referenced by [release] sys-config (the default path)
   [{beamtalk_workspace, [{tcp_port, 4001}]}].
   ```

2. **Set `console = true`** (and, if you want it, a non-default `bind`) in
   `[release]`.
3. **Configure a cookie.** A release with `console = true` and no cookie
   configured **refuses to boot** rather than opening an unauthenticated
   endpoint — set `RELEASE_COOKIE` before starting `bin/<name> foreground`
   (see [Launcher verbs](#launcher-verbs)), or add a `-setcookie <cookie>`
   line to a `[release] vm-args` overlay file.
4. **Bind loopback (the default) and put a reverse proxy in front.**
   `[release] bind` defaults to `127.0.0.1`; remote access is a reverse
   proxy (Caddy/nginx terminating TLS to `ws://127.0.0.1:<port>`) or an
   overlay network (Tailscale/WireGuard) — see [Remote
   Access](remote-access.md)'s Caddy/nginx examples, which apply here
   unchanged (same WebSocket protocol, same cookie handshake). Beamtalk
   implements no TLS of its own (ADR 0058) — a release console is never
   itself the TLS endpoint.
5. **A non-loopback `bind` is allowed, never silent.** An operator inside a
   private overlay network has a legitimate reason to bind beyond
   loopback; doing so logs a warning at boot naming ADR 0058 and the
   reverse-proxy alternative, but does not refuse to start.

Once a console is up, `beamtalk repl --host <host> --port <port>` attaches
over the same WebSocket protocol dev uses, with the identical op vocabulary
minus the release's compiler-dependent refusals (below).

### Tuning: minimum actor heap (`actor_min_heap_size`)

By default a compiled actor's process starts with the VM's default heap (233
words) and grows as it works. Starting an actor then costs about five garbage
collections (BT-4932 measured this at 1.3–1.5 µs per start). A larger minimum
heap removes them, at the price of memory for **every** actor, idle ones too:
the heap is allocated up front. The VM rounds the request up to its next heap
size, so 600, 2000 and 6000 words give heaps of 610, 2586 and 6772 words
(8 bytes each on a 64-bit VM). Measured on an idle `VbCounter`, per actor: about
2.7 KB with the setting unset, 5.7 KB at 600, 21.5 KB at 2000 and 55 KB at 6000
(OTP 28, `benchmarks/vs-elixir/elixir/bench/actor_memory.exs`). It is **off by
default**, so nothing changes until you set it.

```erlang
% config/sys.config (or: ERL_FLAGS="-beamtalk_runtime actor_min_heap_size 2000")
[{beamtalk_runtime, [{actor_min_heap_size, 2000}]}].
```

The value is a number of words from 1 to 1048576 (8 MiB per actor). It is read
when the runtime starts;
change it at run time with `application:set_env(beamtalk_runtime,
actor_min_heap_size, Words)` followed by `beamtalk_actor:refresh_spawn_options()`.
Do that once, not in a loop or at peak load: a changed value writes a
`persistent_term`, which triggers a node-wide garbage collection. Call it again
after lowering the VM's `max_heap_size` (`+hmax`, `system_flag/2`) below the
setting, which the range check cannot see once it is cached, or every start fails
until you do. Two refreshes racing around an env change can leave the older value
until the next call.
It applies to actors started afterwards through the runtime's own start paths:
`spawn`, `spawnWith:`, the monitored variants, named actors and supervised
children (`beamtalk_actor:safe_spawn*`). It does **not** apply to an actor an
Erlang/Elixir host starts itself in its own supervision tree through the
generated `Module:start_link/1,2` or `beamtalk_actor:start_link/2,3`, nor to
`native:` actors started by their own backing module. A value outside the range,
or one the VM refuses (above a `+hmax` limit, say), is ignored with a warning, so
a typo cannot change how actors start.

Measure before choosing a size: `benchmarks/vs-elixir/benchee.sh` with
`BENCH_SCRIPT=bench/actor_memory.exs` reports bytes per idle actor, and rows
16a/16c/16d (`BENCHEE_ROWS=16a,16c,16d`) report the start cost.

### Hardening: `-start_epmd false`

Every release runs loopback-bound Erlang distribution (unless it opts in to
[TLS across hosts](#clustering-across-hosts)) and, by OTP default,
an `epmd`, because that's what makes `stop`/`ping`/`rpc` work with zero
configuration. An operator who wants no `epmd` at all can set
`-start_epmd false` with a fixed `inet_dist_listen_min`/`inet_dist_listen_max`
pair in a `[release] vm-args` overlay, and point `stop`/`ping`/`rpc` at that
fixed port. This is a supported hardening, not a default — the default has
to make `bin/<name> stop` work out of the box.

## `--no-include-erts` and the OTP rule

`include-erts = true` is the default: the release bundles the ERTS it was
built against, so the host needs no Erlang at all and there is no
version-compatibility question to ask. `beamtalk release --no-include-erts`
(or `[release] include-erts = false`) produces a slim artifact for images
layered on a host OTP install instead — useful on an `erlang:NN` base
image, for instance.

Under `--no-include-erts`, the launcher enforces OTP's own documented
compatibility guarantee at boot: compiled BEAM files load on the build
major and the **two majors after it**, never on an older VM
(`required_otp` in `beamtalk-provenance.json`, derived from the build
machine's OTP major). A host outside that window is refused, loudly, before
any application starts:

```
orders 1.4.0 cannot start on Erlang/OTP 27.

  This release's BEAM files were produced by OTP 28 and will not load on
  an older VM.

  Required: Erlang/OTP 28-30 (built on 28)
  Found:    Erlang/OTP 27 (/usr/lib/erlang)

  Either install a supported Erlang/OTP version, or rebuild the release on this host.
```

This is independent of the *build-time* OTP support window
(`otp-support.toml` at the repo root, `current major + previous major,
minimum 27`) — that window governs which OTP majors `beamtalk build`/
`beamtalk release` themselves run on (a warning for `build`, a hard error
for `release`, since a release is an artifact whose validity you can't
otherwise state); the boot-time check above governs what the *artifact*
runs on, which is a fact about the bytecode, not a policy choice.

## `--upgrade-from` as a deploy gate

```bash
beamtalk release --output dist/ --upgrade-from /path/to/orders-1.3.0
```

Relup generation itself is not a v1 capability (it ships restart-based/
blue-green upgrades only — see ADR 0125 §2.1), but `--upgrade-from` still
runs as a **checker**: it compares the new release's shape manifest
(`releases/<vsn>/shapes.json`) and provenance against a previously built
release directory or `.tar.gz`/`.tgz` tarball, and prints a report:

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

Any **error** finding (an unmigrated `shapeVersion:` bump — a class whose
version changed with no corresponding `migrateFromVN:`) exits non-zero, so
wiring `--upgrade-from` into CI turns "hope the running state matches" into
a checked precondition before the deploy ships. **Warnings** — a field
added without a version bump, a removed class — do not block the build, but
are worth reading: they're exactly the changes a restart-based deploy
applies silently.

### One shape-version bump per deploy; upgrade receivers first

A cluster is a set of releases, possibly at different versions during a
rolling deploy. The contract (ADR 0125 §3.4): a node at a *newer* shape
accepts an older peer's term transparently (`unpack/1` already migrates
forward), but the reverse is not safe in general — so the deployment rule
is **one shape-version bump per class per deploy, and upgrade the receiving
side of that class's instances first.** `--upgrade-from`'s preflight is
what makes this checkable before the deploy rather than discoverable during
it: an unmigrated bump is exactly the `error` row above.

## `include-compiler` trade-offs

By default a release ships **no compiler and no `.bt` sources**
(`include-compiler = false`). Everything that compiles is therefore
structurally impossible, not policy-gated — including `eval`. What still
works with no compiler: `run-entry` (`Class selector [args]`, what
`eval`/`rpc` and the WebSocket `run-entry` op all use), `inspect`,
`actors`, `sessions`. What's refused, with a `release_mode_no_compiler`
error naming the alternative:

```
release_mode_no_compiler: Counter >> increment cannot be compiled.

  This node is running an OTP release, which ships no compiler and no
  source. Live method patching is a development-mode operation.

  To change this code: edit the source, `beamtalk release`, and deploy.
  To run a live-patchable image in production instead, rebuild with
  [release] include-compiler = true (see ADR 0125 §1.5).
```

`[release] include-compiler = true` is the opt-in, Pharo-style escape hatch
for teams that deliberately want a live, patchable image in production. It
bundles the compiler port and re-enables the compiler-dependent ops, and
logs a warning at boot naming three things you've opted into:

- **A compiler is now reachable on a production node.** ADR 0058's trust
  boundary means anything past the console cookie can already compile and
  run arbitrary code once a console is enabled — `include-compiler` widens
  what "arbitrary" reaches, since the same trust boundary now fronts a full
  compiler rather than a fixed dispatch shape.
- **Live patches bypass the release artifact** and are gone on the next
  redeploy — the running node can silently diverge from what
  `beamtalk-provenance.json` describes.
- **A class recompiled live loses its provenance stamp.**
  `__beamtalk_meta` no longer says which toolchain produced it.

It does **not** re-enable workspace operations (`Workspace flush`,
`removeFromSystem`, …) — those stay refused with `release_mode_no_workspace`
regardless: a release has no working tree to flush to.

`strip-beams = true` (drops `debug_info` chunks from staged beams, shrinking
the artifact) is refused in combination with `include-compiler = true`:
the compiler port needs `debug_info`-bearing beams for FFI completions and
diagnostics to work against the release's own modules, so the two options
are mutually exclusive, with an error naming why.

## References

- [ADR 0125](../ADR/0125-otp-releases-and-upgrade-compatibility.md) — full
  design rationale (Implemented)
- [ADR 0061](../ADR/0061-program-entry-points-and-run-lifecycle.md) §
  "Future: Release Mode" — the entry-point/lifecycle model releases build on
- [ADR 0058](../ADR/0058-platform-security-model.md) — the trust boundary
  a console (and `include-compiler`) sits behind
- [ADR 0123](../ADR/0123-versioned-state-migration.md) — `shapeVersion:`/
  `migrateFromVN:`, the mechanism `--upgrade-from`'s preflight checks
- [Remote Access](remote-access.md) — reverse-proxy (Caddy/nginx) and
  overlay-network (Tailscale) examples, shared verbatim with the release
  console
- [Releasing Beamtalk itself](releasing.md) — not this guide: Beamtalk's own
  release process, distinct from a *user project's* `beamtalk release`
- [Surface Parity](surface-parity.md) — `beamtalk release`, `bin/<name>
  eval`/`rpc`, and `releaseInfo`/`shapeManifest`'s rows
