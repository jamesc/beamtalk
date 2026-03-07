# ADR 0058: Platform Security Model

## Status
Proposed (2026-03-07)

## Context

### Background

Beamtalk is a developer tool. Its primary interface is an interactive workspace REPL backed by a live BEAM node. The node compiles and evaluates Beamtalk code, holds live actor state, hot-reloads running systems, and spawns OS subprocesses — all as first-class, intentionally designed capabilities.

Several targeted security decisions have already been made and implemented:

- **ADR 0020 (Connection Security):** The REPL WebSocket binds to `127.0.0.1:49152`. A cookie handshake (cookie file at `~/.beamtalk/workspaces/{id}/cookie`, `chmod 600`) authenticates every connection. mTLS via OTP `ssl_dist` is available for remote attach. ADR 0020 explicitly states: "The REPL is an **arbitrary code execution endpoint** — a valid cookie = full RCE as the workspace owner."
- **ADR 0051 (Subprocess Execution):** `System runCommand:` and the `Subprocess` actor expose OS process spawning. The ADR notes "no shell injection mitigation built-in (user responsibility to sanitize inputs)" as a deliberately accepted trade-off.
- **ADR 0028 (BEAM Interop):** The `Erlang` global object calls any Erlang function via `erlang:apply/3`, making every loaded Erlang module reachable from Beamtalk code with no restrictions.

Despite these decisions being individually sound, there is no document that:
1. Names and formalizes Beamtalk's overall security posture as a system.
2. Defines what the authentication boundary protects and what it does not.
3. Catalogs accepted risks with rationale, so future contributors do not re-litigate closed questions.
4. Guides future feature decisions that touch security.

This ADR fills that gap. It is a **platform-level architectural decision**, not a new feature.

### Current Attack Surface

| Surface | Implementation | Capability |
|---------|---------------|------------|
| **Compiler** | OTP Port (stdin/stdout, ADR 0022) | Processes `.bt` source → Core Erlang → BEAM bytecode |
| **FFI (ErlangProxy)** | `beamtalk_erlang_proxy.erl` + `erlang:apply/3` | Calls any loaded Erlang module:function by name |
| **Hot reload** | `beamtalk_hot_reload.erl` + `sys:change_code/4` | Replaces live actor code in running node |
| **Subprocess** | `beamtalk_subprocess.erl`, `beamtalk_exec_port.erl` | Spawns arbitrary OS processes; interactive stdin/stdout |
| **HTTP client** | `beamtalk_http.erl` (gun 2.x) | Outbound HTTP/HTTPS to arbitrary URLs |
| **REPL/workspace** | Cowboy WebSocket on `127.0.0.1:49152` | Evaluates arbitrary Beamtalk code |
| **Workspace storage** | `~/.beamtalk/workspaces/{id}/` | Cookie (`0600`), `metadata.json`, `node.info` |

### The Core Design Tension

Each of the surfaces above is **intentional**. They are what make Beamtalk useful:

- The REPL must evaluate arbitrary code. That is its purpose.
- The FFI must reach any Erlang module. That is what "seamless BEAM ecosystem integration" means.
- Hot reload must replace live code. That is a core language principle.
- Subprocess execution must spawn OS processes. That is a developer utility.

Beamtalk has no sandbox. A valid cookie grants full arbitrary code execution as the workspace owner. This is not a bug — it is the design.

The question this ADR answers is not "how do we sandbox Beamtalk?" but "what do we commit to regarding security, so that users, operators, and contributors understand the model?"

### Constraints

1. **Interactive-first** — Security controls that add friction to the REPL experience are unacceptable for the primary user.
2. **Seamless BEAM interop** — Any restriction on which Erlang modules can be called would make Beamtalk a worse citizen of the BEAM ecosystem.
3. **Hot reload is core** — The live patching capability cannot be gated behind security checks without undermining the core interactive-first value proposition.
4. **Developer tool, not application runtime** — Beamtalk is not designed to run untrusted code submitted by third parties.

## Decision

### The Trusted Developer Tool Stance

**Beamtalk adopts the "Trusted Developer Tool" security model.** The formal statement is:

> Beamtalk is a developer tool for trusted developers working on their own systems. The authentication layer — the workspace cookie validated on every WebSocket connection — is the security boundary. Code that passes authentication executes with the full privileges of the OS process that started the workspace. There is no sandbox beyond this boundary.

This decision has three parts:

#### Part 1: Define the Security Boundary

The boundary is the authentication layer defined in ADR 0020:

- **Local connections:** loopback binding (`127.0.0.1`) + cookie handshake. The OS filesystem ACL (`chmod 600` on the cookie file) is the actual authentication gate.
- **Remote connections:** mTLS (`ssl_dist`) or a network overlay (Tailscale/WireGuard) provides transport-level identity before the cookie check.
- **Browser access:** Origin validation + cookie handshake. Reverse proxy (Caddy/nginx) handles TLS termination.

**Inside the boundary** (authenticated): full execution privilege. No further access controls apply. `Erlang os cmd: "rm -rf /"` is a valid Beamtalk expression that will execute if typed by an authenticated user. This is intentional.

**Outside the boundary** (unauthenticated): connection is rejected before any code is evaluated. The workspace will not echo, evaluate, or log unauthenticated input.

The boundary is **not** about which Erlang modules can be called, which OS commands can be run, or what the HTTP client can reach. Those decisions were made in ADR 0028 and ADR 0051 respectively, and this ADR does not change them.

**Note on the workspace cookie and Erlang distribution cookie:** The workspace uses a single cookie for both the WebSocket authentication handshake and the Erlang distribution protocol (`--setcookie`). These are two distinct trust contexts sharing one secret. A process that obtains the cookie gains both REPL access and the ability to join the workspace's Erlang distribution cluster. This conflation is accepted for simplicity; a future hardening ADR could use separate secrets. For now, cookie compromise implies full workspace compromise via either vector.

#### Part 2: Specify Point Mitigations for the Highest-Risk Surfaces

The trust model does not mean "ignore all risks." Within the trust model, we adopt targeted mitigations for attacks that can affect the workspace owner themselves (not third parties):

**2a. Compiler: OTP Port boundary provides memory isolation (defense against compiler bombs)**

The compiler processes `.bt` source. A maliciously crafted or accidentally huge source file can cause OOM or infinite compile time. The OTP Port process boundary already provides memory isolation — a runaway compiler process is killed by the runtime without taking down the workspace node. No additional size limit is mandated.

```erlang
%% beamtalk_compiler_port.erl — the OTP Port boundary already applies.
%% The compiler Rust process is a separate OS process. An OOM kill there
%% does not affect the Erlang node hosting the workspace.
```

**2b. HTTP client: TLS certificate verification is mandatory**

`beamtalk_http.erl` enforces TLS peer verification for HTTPS connections (`verify_peer`, `cacerts`, SNI, hostname check). This mitigates network-level MITM attacks and is already implemented.

```erlang
%% beamtalk_http.erl (already implemented)
build_gun_opts(tls, Host) ->
    #{
        transport => tls,
        tls_opts => [
            {verify, verify_peer},
            {cacerts, public_key:cacerts_get()},
            {server_name_indication, Host},
            {customize_hostname_check, [
                {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
            ]}
        ]
    }.
```

SSRF via the HTTP client is **not mitigated** — an authenticated user can send HTTP requests to `http://169.254.169.254/` (AWS metadata endpoint) or any other URL. This is accepted: the user is trusted, and restricting URLs would break legitimate use cases (e.g., testing an API running on a local Docker network).

**2c. Workspace storage: Cookie permissions enforced on creation**

The workspace cookie is written with `chmod 600` (ADR 0020). `metadata.json` and `node.info` contain no secrets and have standard permissions. No additional filesystem controls are required.

**Acknowledged limitation:** On systems where the workspace owner is `root` (e.g., Docker containers running as root, common in CI environments), `chmod 600` provides no protection — root can read any file. In these environments, the loopback binding is the primary protection. Operators using containerized CI should treat the workspace cookie as unprotected and isolate the container's loopback interface accordingly.

**2d. Subprocess: No restrictions on spawnable commands**

`beamtalk_subprocess.erl` does not restrict which executables can be spawned. Shell injection is the user's responsibility (ADR 0051). This is accepted — restricting spawnable commands would break scripting and orchestration use cases.

**2e. FFI: No module allowlist/denylist**

`erlang:apply/3` is unrestricted. Any loaded Erlang module is callable. This is accepted — a denylist would be incomplete (users can load new modules), and an allowlist would break BEAM interop (ADR 0028 Principle 9).

#### Part 3: Establish Security Principles for Future Features

New features that touch security must be evaluated against these principles:

1. **The authentication layer is the boundary.** New capabilities added inside the auth boundary (post-cookie, post-mTLS) do not require additional access controls by default.

2. **Zero-config for local dev, explicit opt-in for network exposure.** Local loopback binding is the default. Any feature that would bind to non-loopback interfaces requires an explicit CLI flag and a warning (following ADR 0020 §Layer 2).

3. **Secrets never in REPL output, logs, or metadata.** The workspace cookie is not displayed in the REPL, not logged at INFO level, and not included in workspace metadata. Follow this pattern for any new secrets (API keys, TLS private keys, etc.).

4. **OTP error isolation is security-relevant.** An actor crash must not take down the workspace. Supervisors are mandatory for all long-running actors. This is both a fault-tolerance and a security property — a malformed message to an actor must not affect other actors.

5. **New network services bind to loopback by default.** Any future service that opens a TCP or UDP port must bind to `127.0.0.1` by default. An explicit `--bind` flag is required for any other binding.

6. **Beamtalk is single-user per workspace.** A workspace is owned by one developer. Multiple concurrent connections (e.g., multiple browser tabs, ADR 0017) from the same authenticated user are permitted — they share the same cookie and the same actor state intentionally. What is not permitted is sharing a workspace between two developers as a multi-user service, as that collapses the auth boundary to a shared secret. Each developer should have their own workspace.

### What This ADR Does Not Do

- It does not add a sandbox.
- It does not restrict which Erlang modules are callable.
- It does not restrict which OS commands are spawnable.
- It does not restrict which URLs the HTTP client can reach.
- It does not change any behavior. ADR 0020, ADR 0028, and ADR 0051 remain authoritative for their respective surfaces.

## Prior Art

### Jupyter Notebook

Jupyter's security documentation explicitly states: "If you are using the notebook in a context where untrusted users might run code, you need to configure it properly. The primary security mechanism for Jupyter is the **token** — a single random value that must be included in requests." For local use, Jupyter relies on loopback binding and considers the local user trusted. A running notebook server is documented as granting arbitrary code execution to whoever can reach it.

Beamtalk's model is nearly identical: loopback binding + cookie/token = trust. The Beamtalk cookie is a stronger secret (256-bit random, `chmod 600` per-workspace file) than a typical Jupyter token displayed in the terminal URL.

**What we adopted:** The same fundamental stance — the token/cookie is the boundary; inside the boundary, full execution privilege.

**What we rejected:** Jupyter's notebook trust model (tracking which outputs were produced by "trusted" code via notebook signatures). Beamtalk has no notebook format; the REPL evaluates expressions, not stored notebooks, so this attack vector does not apply.

### Livebook (Elixir)

Livebook is the closest prior art: a BEAM-native interactive notebook using Phoenix and Erlang distribution. Livebook uses token authentication (URL token on startup), loopback binding for local access, and password authentication for cloud deployments. The Livebook README states the security model clearly: the URL token grants code execution; do not share it.

Livebook adds one feature Beamtalk does not: session-level isolation, where each notebook session gets its own BEAM process group, limiting blast radius. Beamtalk's equivalent is OTP supervision — each actor crash is isolated by its supervisor. Livebook Teams (commercial) adds SSO and audit trails, explicitly out of scope for Beamtalk's current use case.

**What we adopted:** Same token-over-loopback model. OTP supervision for session isolation.

**What we rejected:** Session-level namespace isolation (out of scope for v1; a single workspace is a single developer's environment).

### nREPL (Clojure)

nREPL is the most direct conceptual analogue: a network REPL for developer tooling. Its documentation is explicit: "nREPL provides no security mechanisms. Running a server makes your entire Clojure environment accessible to the network. Protect it appropriately." nREPL listens on a random local port with no authentication by default (relying entirely on loopback binding). For remote access, nREPL documents SSH tunneling as the recommended approach and supports TLS via optional middleware.

Beamtalk is strictly stronger than nREPL's defaults: the cookie handshake adds authentication on top of loopback binding, and mTLS is built in for remote access rather than delegated to SSH.

**What we adopted:** Honest, explicit documentation of the "no sandbox, trusted developer" stance. Single-transport design (WebSocket, as nREPL influenced ADR 0020).

**What we rejected:** nREPL's "no auth by default" posture — Beamtalk requires the cookie handshake even on loopback (ADR 0020).

### Erlang/OTP Cookie Security

The Erlang cookie mechanism is the direct ancestor of Beamtalk's cookie. The OTP documentation is explicit about its limitations: "The security here is not that strong — the cookie is not encrypted, and the communication is by default in cleartext. The cookie prevents accidental connections, not intentional attacks." OTP recommends `ssl_dist` for strong security.

Beamtalk inherits this honest framing. The workspace cookie (used on the WebSocket protocol) is stronger than the Erlang cookie (`~/.erlang.cookie`) — it is a 256-bit random secret stored per-workspace with `chmod 600`, not a global atom in a user's home directory. For network-exposed remote access, mTLS (not cookies) provides the security property.

**What we adopted:** The cookie-per-workspace pattern. The OTP `ssl_dist` mTLS path for remote.

### Python Interactive Interpreter and IPython

Python's interactive interpreter (`python3`) and IPython have the same model: a REPL that executes arbitrary code with the launching user's full privilege. There is no sandbox. Calling `subprocess.run(["rm", "-rf", "/"])` from a Python session is valid code that will execute. The Python docs do not document a security model for the interactive interpreter — it is assumed to run trusted code in a trusted environment.

The Python/IPython model is widely understood and accepted by developers. No Python developer is surprised that their interactive session can delete files. This precedent supports Beamtalk's position: a developer REPL that executes code with user privilege is the expectation, not an anomaly.

**What we adopted:** The same implicit understanding that a developer REPL has full privilege, and the same approach of not engineering around it for the local single-user case.

**What differs:** Beamtalk adds explicit network authentication (the cookie handshake) that Python's local interpreter does not need. Python relies entirely on the OS providing a terminal; network-accessible REPL is not a built-in Python feature. Beamtalk's network-first architecture (ADR 0017) means authentication is mandatory even locally.

### Pharo Smalltalk

Pharo's image is a complete object world — there is no meaningful distinction between "the runtime" and "user code." Everything runs in the image with the same privilege. This is the most extreme form of the "trusted user" model: there is no security boundary at all within the image.

Beamtalk has a boundary that Pharo lacks: the authentication layer. A Pharo image that gets loaded grants its loader full control; a Beamtalk workspace grants its connector full control only if they know the cookie.

**What we adopted:** The single-user-owns-everything philosophical stance. No distinction between "system code" and "user code" within the workspace.

**What we did not adopt:** Pharo's image persistence model (Beamtalk code lives in files, not in a saved image).

### Newspeak

Newspeak is explicitly described as "a live object-capability language." Its module system is designed around capability security — every top-level class is a parametric namespace. Access to ambient authority (file system, network, OS) requires explicit capability injection; there is no global namespace that exposes these capabilities implicitly.

This is the most principled security model among the reference languages, but it requires the language to be designed around capabilities from the start. Beamtalk has global objects (`Erlang`, `System`, `Http`, `Subprocess`), a flat namespace (ADR 0031), and makes ambient authority available everywhere. Adopting Newspeak's object-capability model would require removing these global objects — which contradicts the interactive-first and seamless-BEAM-interop principles.

**What we acknowledged from Newspeak:** A capability-based approach requires language-level support and cannot be retrofitted cheaply. This is why Option B (capability-based restrictions) is rejected below.

**What we rejected:** The object-capability module system. It is the right answer for a language designed from scratch with security as a primary goal; it is the wrong answer for a language designed around interactive BEAM interop.

## User Impact

### Newcomer (Python/JS background)

The newcomer connects via `beamtalk repl` and evaluates code. The cookie handshake is transparent — the CLI reads the cookie file automatically. The security model is invisible in normal use, exactly like Python's interactive interpreter or Node.js REPL.

The newcomer is protected from one surprising failure mode: on a shared machine (university server, CI runner, Docker container with `--net=host`), another user's process cannot connect to their workspace and steal their session, because the cookie file is `chmod 600`.

The newcomer is **not** protected from: code they type that does harmful things (deleting files, exfiltrating environment variables). This matches their experience with Python and Node.js, where the interpreter is also a full-privilege execution environment.

**Documentation needed:** A clear "Security overview" in the getting-started guide: "Beamtalk's REPL evaluates code with your user's full permissions. Treat the workspace cookie like a password."

### Smalltalk Purist

The Smalltalk purist recognizes the model immediately: it is the Smalltalk image model, translated to BEAM. The image (workspace) is a trusted environment. Everything that runs there has full access to the system. The IDE is the tool; the developer is trusted.

The addition of explicit authentication (cookie handshake, mTLS) is a practical improvement over the classic Smalltalk image, which has no network authentication at all.

The Smalltalk purist who comes from Newspeak background may push for Option B (capability restrictions). The Steelman section addresses this.

### Erlang/BEAM Veteran

The BEAM veteran sees a familiar model. The workspace cookie is a per-workspace Erlang cookie. The loopback binding + cookie auth is exactly what most Erlang development tools do. The mTLS option via `ssl_dist` is standard OTP. Hot reload via `sys:change_code/4` is standard OTP.

One note: an authenticated user can call `Erlang erlang halt: 0` via FFI and shut down the workspace node. This is correct behavior for a developer tool — a developer should be able to restart their own workspace. It is not a security vulnerability in the trusted-developer model.

### Production Operator

The operator running Beamtalk in a server context (automated testing, scripting, CI pipeline) needs to understand the blast radius of a compromised cookie:

- **Cookie compromise on a single-user dev machine:** attacker gains the same access as the developer. Equivalent to SSH key compromise.
- **Cookie compromise on a shared server:** attacker gains workspace-owner privilege for that specific workspace. Other users' workspaces are unaffected (each has its own cookie).
- **Cookie in CI/CD:** treat it like an SSH key in CI secrets.

Operators deploying Beamtalk for remote access must follow ADR 0020 Layer 3 (mTLS or overlay network). The operator's checklist:

```bash
beamtalk tls init                    # Generate per-workspace mTLS certs
beamtalk workspace create --tls      # Workspace uses ssl_dist
beamtalk attach prod@host --tls      # mTLS on Erlang distribution
# Add Caddy/nginx reverse proxy for browser TLS termination
# Configure Tailscale ACLs for access control
```

**The operator must accept:** Beamtalk is not designed to be multi-tenant. Running one shared workspace for multiple team members and controlling access at the code level is not a supported architecture.

### Language Designer

The language designer sees this ADR as establishing that security is a platform-level concern (authentication) rather than a language-level concern (capability restrictions). This is a deliberate choice that makes Beamtalk simpler to use and implement at the cost of the guarantees that a capability language could provide.

The tension with Newspeak's heritage is real and acknowledged: Beamtalk draws inspiration from Newspeak's syntax and interactive model but explicitly rejects Newspeak's capability security model. This is correct for Beamtalk's current goals (developer tool, BEAM ecosystem citizen) and would need revisiting if Beamtalk ever targets multi-tenant execution.

## Steelman Analysis

### Steelman for Option B: Capability-Based Restrictions

**Best argument (from a Newspeak-influenced language designer):**

Beamtalk draws from Newspeak, which demonstrated that capability security and interactive development are not mutually exclusive. In Newspeak, ambient authority flows through explicitly injected capabilities. `Http`, `Subprocess`, and `Erlang` would be platform capabilities, injected by the workspace launcher rather than globally available. This means user code running in an untrusted context (e.g., a plugin in a larger system) cannot access the file system or spawn processes without being explicitly granted those capabilities.

From the production operator's perspective: "The Beamtalk scripting agent we use internally should be able to make HTTP requests but absolutely should not spawn OS processes. With the current model, if the agent is compromised via a malicious prompt, it can `Subprocess open: "exfil.sh"`. Capability flags on the workspace config would let us disable subprocess spawning entirely."

**Why we still reject it:** Capability-based restrictions require language-level support. The `Erlang` global, by design (ADR 0028), allows calling any Erlang module — including `os:cmd/1`, `file:write_file/2`, and `erlang:halt/0`. There is no language-level mechanism to prevent an authenticated user from calling `Erlang os cmd: "rm -rf ~"` regardless of capability flags. Adding true capability restrictions would require:

1. Removing the `Erlang` global or making it capability-gated.
2. Adding capability flags to the workspace protocol.
3. Auditing all stdlib methods that access ambient authority.
4. Breaking the "seamless BEAM interop" principle at the language level.

This is a language redesign, not a security feature. Beamtalk v0.1 is committed to flat namespace (ADR 0031) and ambient globals. Capability restrictions are a decision for a future major version with explicit capability architecture from the start.

The specific concern about agent/plugin contexts is valid and can be addressed at the deployment level: run untrusted agents in a separate workspace with workspace-level feature flags (Phase 3 implementation item below).

### Steelman for Option C: Defense-in-Depth (Module Denylist + Resource Limits)

**Best argument (from a security engineer at a company running shared Beamtalk infra):**

Full capabilities inside the cookie boundary is fine for a single developer. But our team runs Beamtalk on a shared server for automated scripting. The scripts are written by junior team members who are trusted but not infallible. A script that accidentally calls `Erlang os cmd: "rm -rf /tmp/shared"` can break everyone's environment. An allowlist of callable Erlang modules (e.g., only `lists`, `maps`, `string`, `math`, `crypto`) would limit the blast radius of mistakes.

OTP already has tools for resource limits. Process memory limits and reductions can bound runaway code. Adding OTP resource limits to the workspace supervisor is low-cost and high-value.

**Why we reject the module denylist/allowlist component:** An Erlang module denylist is fundamentally broken as a security mechanism. A denylist of `os` and `file` still leaves `erlang:open_port/2` (which can spawn OS processes), `code:add_patha/1` (which can load new modules), and any number of OTP application modules that access the file system indirectly. Security by denylist is a red queen's race.

An allowlist is more coherent but directly contradicts ADR 0028: "Beamtalk is a first-class BEAM citizen." An allowlist that restricts callable Erlang modules makes Beamtalk a worse BEAM citizen than Erlang itself, which has no module restrictions.

**The resource limits point is independently valid** and is accepted as a Phase 2 implementation item (OOM protection for eval processes) without adopting Option C's full model.

The shared-server concern is better addressed by deployment architecture: one workspace per developer, not one workspace shared by a team.

### Tension Points

| Tension | Cohorts | Resolution |
|---------|---------|------------|
| Newcomer wants predictability; BEAM veteran wants full BEAM access | All | Full BEAM access wins — newcomer expectation is met by "it behaves like your user account" |
| Language designer wants capability elegance; operator wants restrictions | Option B vs A | Rejected Option B — language redesign required |
| Security engineer wants denylist; BEAM veteran knows denylist is incomplete | Option C vs A | Rejected denylist — resource limits accepted separately |
| Operator wants multi-tenant; Beamtalk is single-user per workspace | Deployment model | One workspace per developer is the answer |

## Alternatives Considered

### Option B: Capability-Based Restrictions

Introduce a capability flag system where workspace code must request privileges for sensitive operations. `Http`, `Subprocess`, and `Erlang` (or its modules) would be capability-gated. The workspace launcher grants capabilities at startup; code cannot acquire them dynamically.

**Rejected** because:
1. The `Erlang` global makes capability gating impractical — any Erlang function is reachable, including functions that access ambient authority.
2. Language-level support (no static globals, no ambient namespace) is required for correct capability isolation. Beamtalk has both.
3. Implementation cost is prohibitive for v0.1 and would require reverting design decisions in ADR 0028 and ADR 0031.
4. The target user (solo developer, small team) does not need this.

A future Beamtalk version that targets multi-tenant execution (e.g., a cloud IDE where untrusted users submit code) would need to revisit this decision with capability security as a first-class design constraint from the start.

### Option C: Defense-in-Depth (Module Denylist + Resource Limits)

Keep no sandbox but add specific mitigations: a denylist/allowlist for FFI modules, subprocess spawning requires a workspace-level config opt-in, and OTP resource limits on eval processes.

**The allowlist/denylist component is rejected** — a module denylist is always incomplete (see Steelman section). Security by denylist is a red queen's race against `erlang:open_port/2`, `code:add_patha/1`, and indirect filesystem access.

**The resource limits component is accepted in principle** as a Phase 2 implementation item — applying `erlang:process_flag(max_heap_size, ...)` to eval processes for OOM protection. This is a resilience improvement, not a security boundary.

**Workspace-level feature flags** (`--no-subprocess`) are a valid future feature that can be implemented without capability-level language changes. Deferred to Phase 3 (see Implementation section).

## Consequences

### Positive

- **Clarity for contributors:** New features know the boundary. A contributor building a `SharedWorkspace` feature knows they need to consider multi-user authentication before implementing sharing.
- **Clarity for operators:** The security posture is explicitly documented. Operators can evaluate it against their threat model.
- **Honest trade-off documentation:** Security properties (connection auth, cookie file permissions, TLS cert verification, OTP process isolation) and non-properties (authenticated code running with OS user privilege) are explicit.
- **Foundation for future evolution:** If Beamtalk adds a plugin system, sandboxed evaluation, or multi-tenant mode, this ADR defines what needs to change and why.
- **BEAM interop preserved:** No restrictions on `erlang:apply/3`. ADR 0028 is unchanged.
- **Developer experience preserved:** No friction added to the REPL workflow.

### Negative

- **Not suitable for multi-tenant execution:** A Beamtalk workspace cannot safely run code from multiple users with different trust levels. This limits addressable use cases (no "code playground," no "shared team workspace" with role-based access).
- **No blast-radius reduction for FFI:** A bug in user code that calls `Erlang erlang halt: 0` terminates the workspace. Resilience against user mistakes is lower than it could be with Option C mitigations.
- **Hot-reload persistence survives cookie rotation:** An authenticated session can hot-reload code into a running actor. That code persists in the process after the session ends and after the cookie is rotated. Cookie rotation is not sufficient remediation for a compromised session that hot-reloaded malicious code — the workspace must be stopped and restarted to fully evict injected code.
- **Auditors may not be satisfied:** Organizations with strict compliance requirements ("no tool may spawn OS processes without explicit configuration") cannot use Beamtalk in scripting contexts without additional operational controls.
- **No eval audit trail:** Authenticated eval operations are not logged — there is no record of what code was evaluated in a session. Connection events and auth failures are logged (ADR 0020), but not the code itself. Production operators cannot perform forensics on "what ran" without additional tooling.
- **Security research burden is on the operator:** Evaluating Beamtalk for a given deployment requires reading ADR 0020, ADR 0028, ADR 0051, and this ADR. A consolidated `docs/security/threat-model.md` is required.
- **Plugin system requires revisiting this ADR:** Any plugin API that loads third-party code into the workspace node immediately inherits full RCE capability via the `Erlang` global (ADR 0028). A plugin system cannot be safely designed under this security model without also addressing the FFI boundary — this ADR must be revisited before a plugin system is designed.
- **Cloud playground / multi-tenant evaluation is foreclosed architecturally:** Accepting the unrestricted `erlang:apply/3` FFI (ADR 0028) means a sandboxed evaluation mode requires either removing the `Erlang` global or running untrusted code in a separate BEAM node communicating via message passing. This is a major architectural change, not an incremental addition. This decision should be understood as a strategic choice: Beamtalk is not a cloud playground, and making it one requires a different architecture.

### Neutral

- This ADR documents the current behavior; it does not change any code.
- OTP's process isolation (each actor in its own process, crash isolation via supervisor) provides a form of compartmentalization that is weaker than a sandbox but stronger than a single-threaded interpreter. This is not a security boundary, but it limits accidental damage.
- The Erlang distribution cookie (`--setcookie`) is a distinct mechanism from the Beamtalk WebSocket cookie. The distribution cookie (MD5 challenge-response) governs Erlang node-to-node communication; the WebSocket cookie governs REPL client authentication. Both are per-workspace; neither is the global `~/.erlang.cookie`.

## Implementation

This ADR is primarily a documentation and principles decision. No behavioral code changes are required.

### Phase 0: Documentation (Required)

1. **Create `docs/security/threat-model.md`** — a user-facing security guide that:
   - States the trust model: "Beamtalk evaluates code with your user's full OS permissions."
   - Lists what the cookie protects against (unauthenticated access from other local processes).
   - Lists what the cookie does NOT protect against (authenticated user doing harmful things).
   - Provides an operator checklist for remote deployment (references ADR 0020 phases 2–3).
   - Includes: "Treat the workspace cookie like an SSH private key — do not share it."

2. **Update getting-started documentation** — add a security callout after the first `beamtalk repl` example:
   ```
   Security note: The REPL evaluates code with your user account's full permissions.
   The workspace cookie at ~/.beamtalk/workspaces/{id}/cookie authenticates connections.
   Treat it like an SSH private key — do not share it.
   ```

3. **Update `README.md`** — add a "Security" section linking to `docs/security/threat-model.md`.

### Phase 1: Principle Enforcement in Code Review (Process change, no code)

When reviewing PRs that introduce new capabilities accessible from Beamtalk code (new stdlib classes with OS access, new FFI wrappers, new network services), require the author to document the security implications relative to this ADR's six principles. Specifically:
- Does the new feature bind a new network port? (Must default to loopback.)
- Does it access secrets? (Must use `chmod 600`; must not log.)
- Does it spawn processes or make network requests? (Accepted; note the trust model applies.)

### Phase 2: OTP Resource Limits for Eval Processes (Low priority)

Apply `erlang:process_flag(max_heap_size, #{size => N, kill => true})` to the gen_server process that evaluates user code in the REPL. This bounds memory consumption of a single runaway eval expression without restricting what code can do.

This is a **resilience** improvement (protecting the workspace node from accidental OOM), not a security improvement (it does not prevent authenticated users from doing harmful things). Specific limit and OOM behavior to be determined in the implementation issue.

### Phase 3: Workspace-Level Feature Flags (Future, requires new CLI/protocol work)

Add workspace configuration flags: `--no-subprocess`, with possible future `--no-ffi`. This allows operators to launch Beamtalk workspaces with specific capabilities disabled for automated/scripting contexts.

Note: this is not a sandbox — an authenticated user with `--no-subprocess` can still call `Erlang os cmd:` directly via the unrestricted FFI. These flags raise the bar for accidental capability use; they do not enforce hard security boundaries. Design and implementation deferred to a follow-up ADR.

## References

- Related issues: BT-214 (Research: End-to-End Security Review of Beamtalk Platform)
- Related ADRs: [ADR 0020 — Connection Security](0020-connection-security.md), [ADR 0028 — BEAM Interop Strategy](0028-beam-interop-strategy.md), [ADR 0051 — Subprocess Execution](0051-subprocess-execution.md), [ADR 0004 — Persistent Workspace Management](0004-persistent-workspace-management.md)
- Erlang/OTP cookie documentation: https://www.erlang.org/doc/reference_manual/distributed.html#security
- Erlang `ssl_dist` (mTLS for distribution): https://www.erlang.org/doc/apps/ssl/ssl_distribution.html
- Jupyter security model: https://jupyter-notebook.readthedocs.io/en/stable/security.html
- Livebook security (GitHub README): https://github.com/livebook-dev/livebook
- nREPL security documentation: https://nrepl.org/nrepl/usage/server.html
- Newspeak object-capability model: https://bracha.org/newspeak-modules.pdf

### Implementation Hardening Notes (follow-up issues, not in scope of this ADR)

The following implementation concerns were identified during review and should be tracked as separate issues:

- **Cookie timing-safety:** `beamtalk_ws_handler.erl` performs a byte-length check before `crypto:hash_equals/2`, breaking the constant-time guarantee. The length check should be removed or absorbed into `hash_equals`.
- **CSPRNG documentation:** `generate_cookie()` in `storage.rs` relies on `rand::rng()` being a CSPRNG. This should be made explicit (use `rand::rngs::OsRng` directly) so future changes cannot accidentally weaken it.
- **`/proc` cmdline exposure:** If the Erlang VM expands `-args_file` into its own argv, the workspace cookie may be visible in `/proc/<pid>/cmdline` on Linux despite the args file being `chmod 600`. This should be investigated; if confirmed, the cookie should be passed via a file descriptor rather than command-line arguments.
