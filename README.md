# Beamtalk

[![CI](https://github.com/jamesc/beamtalk/actions/workflows/ci.yml/badge.svg)](https://github.com/jamesc/beamtalk/actions/workflows/ci.yml)

**A live, interactive Smalltalk-like language for the BEAM VM**

Beamtalk brings Smalltalk's legendary live programming experience to Erlang's battle-tested runtime. While inspired by Smalltalk's syntax and philosophy, Beamtalk makes pragmatic choices for modern development (see [Syntax Rationale](docs/beamtalk-syntax-rationale.md)). Write code in a running system, hot-reload modules without restarts, and scale to millions of concurrent actors.

```beamtalk
// Spawn an actor with state
counter := Counter spawn

// Send messages (async by default)
counter increment
counter increment
value := counter getValue await  // => 2

// Cascades - multiple messages to same receiver
Transcript show: 'Hello'; cr; show: 'World'

// Map literals
config := #{#host => 'localhost', #port => 8080}
```

---

## Why Beamtalk?

| Feature | Benefit |
|---------|---------|
| **Interactive-first** | REPL and live workspace, not batch compilation |
| **Hot code reload** | Edit and reload modules in running systems |
| **Actors everywhere** | Every object is a BEAM process with fault isolation |
| **Async by default** | Message sends return futures; no blocking |
| **Full reflection** | Inspect any actor's state, mailbox, and methods at runtime |
| **BEAM interop** | Call Erlang/Elixir libraries; deploy to existing infrastructure |
| **Supervision built-in** | Declarative fault tolerance with restart strategies |

---

## Key Features

### Actors as Objects

Every Beamtalk object is a BEAM process with its own state and mailbox:

```beamtalk
Actor subclass: Counter
  state: value = 0

  increment => self.value += 1
  decrement => self.value -= 1
  getValue => ^self.value
```

### Async Message Passing

Messages are asynchronous by default, returning futures:

```beamtalk
// Returns immediately with a future
result := agent analyze: data

// Wait when you need the value
value := result await

// Or use continuations
agent analyze: data
  whenResolved: [:value | self process: value]
  whenRejected: [:error | self handle: error]
```

### Pattern Matching

Erlang-inspired pattern matching for clean message handling:

```beamtalk
handle: {#ok, value} => self process: value
handle: {#error, reason} => self logError: reason
handle: _ => self handleUnknown
```

### Declarative Supervision

OTP supervision trees as language-level constructs:

```beamtalk
Supervisor subclass: WebApp
  children: [
    {DatabasePool, scale: 10},
    HTTPRouter spawn,
    MetricsCollector spawn
  ]
  strategy: #oneForOne
```

### Live Patching

Hot-reload with dedicated syntax:

```beamtalk
// Update running actors instantly
patch Agent >> processMessage: msg {
  Telemetry emit: #messageReceived
  ^super processMessage: msg
}
```

---

## Designed for AI Agents

Beamtalk is purpose-built for multi-agent AI systems:

- **Every actor is a BEAM process** — millions of concurrent isolated agents
- **Live inspection** — query actor state, methods, and capabilities at runtime
- **Hot-reload** — edit agent behavior while they run, no restart needed
- **Fault tolerance** — actors crash and restart cleanly via OTP supervision
- **Distributed** — spread actors across BEAM clusters transparently

```beamtalk
// Each agent is its own BEAM process — isolated, supervised, inspectable
researcher := Researcher spawn
critic := Critic spawn

// Async messaging — returns futures
analysis := researcher analyze: codeRepo
findings := analysis await

// Live introspection while running
researcher class              // => Researcher
researcher respondsTo: #plan: // => true
Researcher methods            // => #(analyze:, plan:, query:, ...)

// Hot-reload: redefine behavior mid-run, takes effect on next message
// (proposed syntax — see agent-native development doc for design details)
Researcher >> plan: prompt =>
  Transcript show: "Planning: ", prompt.
  ^super plan: prompt
```

See [Agent-Native Development](docs/beamtalk-agent-native-development.md) for the full vision.

---

## Getting Started

### Prerequisites

- **Rust** (latest stable)
- **Erlang/OTP 26+** with `erlc` on PATH

### Build & Run

#### Using Just (Recommended)

The project includes a [Justfile](Justfile) with common tasks:

```bash
# Clone and setup
git clone https://github.com/jamesc/beamtalk.git
cd beamtalk

# Install Just (if not already installed)
cargo install just

# See all available tasks
just --list

# Run local CI checks (build, lint, unit & E2E tests)
# Note: runtime integration tests run only in GitHub Actions CI
just ci

# Start the REPL
just repl

# Run tests
beamtalk test              # Run BUnit TestCase tests
just test-stdlib           # Run compiled expression tests
just test-e2e              # Run REPL integration tests

# Clean build artifacts (works with Docker volumes)
just clean
```

#### Using Cargo Directly

```bash
# Build
cargo build

# Start the REPL
cargo run -p beamtalk-cli --bin beamtalk -- repl

# Clean (note: fails in devcontainer due to volume mount)
cargo clean  # Use `just clean` instead in devcontainer
```

### REPL Usage

**New to Beamtalk?** See the [REPL Tutorial](examples/repl-tutorial.md) for a complete beginner's guide!

```text
Beamtalk v0.1.0
Type :help for available commands, :exit to quit.

> message := 'Hello, Beamtalk!'
"Hello, Beamtalk!"

> 2 + 3 * 4
14

> :load examples/hello.bt
Loaded Hello

> Hello new
{__class__: Hello}
```

### Load Files

```text
> :load examples/counter.bt
Loaded

> :reload
Reloaded
```

---

## Development Setup

### Prerequisites

#### Required Dependencies

- **Docker Desktop** — For devcontainer support  
- **VS Code** — With the [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)  
- **Rust** (latest stable) — For building the compiler  
- **Erlang/OTP 26+** — With `erlc` on PATH

#### Required Environment Variables

| Variable | Purpose | How to Set |
|----------|---------|------------|
| `GH_TOKEN` | GitHub authentication for devcontainers | `gh auth login`, then `$env:GH_TOKEN = (gh auth token)` |
| `LINEAR_API_TOKEN` | Linear issue tracking | Get from [Linear API settings](https://linear.app/settings/api) |

#### Optional: Devcontainer CLI

For worktree-based parallel development, install the devcontainer CLI:

```bash
npm install -g @devcontainers/cli
```

**Note:** The `worktree-new` scripts will auto-install this if missing.

### Quick Start: Basic Development

**VS Code (Recommended):**

1. Clone the repository
2. Open in VS Code: **File → Open Folder**
3. When prompted, click **Reopen in Container**
4. Wait for container build (~5 minutes first time)
5. Open a terminal and run: `cargo build`

The devcontainer includes all dependencies pre-configured:
- Rust toolchain with `clippy`, `rustfmt`, `rust-analyzer`
- Erlang/OTP 26+ and `rebar3`
- Node.js LTS for build tooling
- GitHub CLI (`gh`) with authentication
- GitHub Copilot CLI for AI assistance
- VS Code extensions for Rust, TOML, Erlang, GitHub, Linear

### Advanced: Parallel Development with Worktrees

Git worktrees enable **multiple Copilot agents working in parallel** on different branches, each in its own isolated devcontainer.

#### Why Worktrees?

- **Parallel work** — Multiple agents on different issues simultaneously
- **Isolation** — Each worktree has its own build artifacts, IDE state, and port
- **No stashing** — Switch context without stashing uncommitted changes
- **Container per branch** — Each worktree runs in its own devcontainer

#### Directory Structure

```text
~/source/
├── beamtalk/               # Main repository (origin)
│   ├── .git/               # Main Git directory
│   ├── .devcontainer/
│   ├── crates/
│   └── ...
│
├── BT-99-feature/          # Worktree #1 (sibling directory)
│   ├── .git                # File pointing to ../beamtalk/.git/worktrees/BT-99-feature
│   ├── .env                # Auto-generated: REPL port, node name
│   ├── crates/
│   └── ...
│
└── BT-123-another/         # Worktree #2 (sibling directory)
    ├── .git                # File pointing to ../beamtalk/.git/worktrees/BT-123-another
    ├── .env
    ├── crates/
    └── ...
```

**Key points:**
- Worktrees are created as **sibling directories** to the main repo
- Each worktree has a `.env` file with unique REPL port and Erlang node name
- `.env` files are in `.gitignore` and cleaned up when worktree is removed

#### Environment Variables

**Required for worktree devcontainers:**

| Variable | Purpose | How to Set |
|----------|---------|------------|
| `GH_TOKEN` | GitHub authentication | `gh auth login`, then `$env:GH_TOKEN = (gh auth token)` |

**Optional for Linear integration:**

| Variable | Purpose | Where to Set |
|----------|---------|--------------|
| `LINEAR_API_TOKEN` | Linear issue tracking | Host environment |

**Setting environment variables permanently (Windows):**

```powershell
# GitHub: authenticate first, then set GH_TOKEN
gh auth login
$env:GH_TOKEN = (gh auth token)  # Set for current session
# Or add to your PowerShell profile for persistence

# Linear: store securely, retrieve at runtime
```

> **Note:** Linux/Mac instructions will be added once the scripts are tested.

#### Creating a Worktree

```powershell
# From main repo directory
.\scripts\worktree-new.ps1 BT-99-feature

# Create new branch from main
.\scripts\worktree-new.ps1 -Branch BT-99 -BaseBranch main
```

**What happens:**
1. Creates worktree as sibling directory to main repo
2. Checks out the branch (creates from base if new)
3. Generates `.env` file with unique REPL port and Erlang node name
4. Starts devcontainer with all services
5. Opens bash shell inside container

**Port assignment:**
- REPL ports are OS-assigned (ephemeral) by default — no port conflicts
- Override with `--port` flag or `BEAMTALK_REPL_PORT` env var
- Each worktree gets a unique node name and daemon socket for isolation

#### Removing a Worktree

```powershell
.\scripts\worktree-rm.ps1 BT-99-feature

# Force remove (discard uncommitted changes)
.\scripts\worktree-rm.ps1 -Branch BT-99 -Force
```

**What happens:**
1. Stops and removes the devcontainer
2. Removes the Docker volume (target cache)
3. Fixes `.git` file if modified by container
4. Removes the worktree directory
5. Optionally deletes the branch (prompts for confirmation)

#### Cleaning Up Orphaned Containers

If you deleted worktree directories manually, clean up leftover containers:

```powershell
.\scripts\worktree-cleanup.ps1        # Interactive
.\scripts\worktree-cleanup.ps1 -DryRun # Preview only
.\scripts\worktree-cleanup.ps1 -NoConfirm # Auto-confirm
```

See [scripts/README.md](scripts/README.md) for detailed documentation.

### GitHub Integration

#### Authentication

The devcontainer uses `GH_TOKEN` for GitHub CLI authentication:

```bash
# Verify authentication (inside container)
gh auth status

# Login manually if needed
gh auth login
```

**Priority order:**
1. `GH_TOKEN` environment variable (from host)
2. VS Code credential helper (auto-configured)
3. Manual `gh auth login`

#### Git Configuration

Set your identity for commits:

**Option 1: Environment variables (recommended for worktrees)**
```bash
export GIT_USER_NAME="Your Name"
export GIT_USER_EMAIL="you@example.com"
```

**Option 2: Global git config (inside container)**
```bash
git config --global user.name "Your Name"
git config --global user.email "you@example.com"
```

The `postStartCommand` in `devcontainer.json` automatically configures git from environment variables.

### Commit Signing

#### SSH Signing (Recommended)

Configure SSH signing for verified commits:

**1. Generate SSH signing key (on host):**
```bash
ssh-keygen -t ed25519 -C "you@example.com" -f ~/.ssh/id_ed25519_signing
```

**2. Add public key to GitHub:**
```bash
cat ~/.ssh/id_ed25519_signing.pub
# Copy output to GitHub Settings → SSH and GPG keys → New SSH key (select "Signing Key")
```

**3. Configure git (inside container):**
```bash
git config --global gpg.format ssh
git config --global user.signingkey ~/.ssh/id_ed25519_signing
git config --global commit.gpgsign true
```

**4. Add private key to SSH agent:**
```bash
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519_signing
```

> **Note:** For worktrees, the `worktree-new.ps1` script copies the signing key into the container and configures git if `GIT_SIGNING_KEY` environment variable is set. It does not automatically start or configure `ssh-agent`.

### Installed Tools & Extensions

#### Command Line Tools

| Tool | Version | Purpose |
|------|---------|---------|
| `rustc` | Latest stable | Rust compiler |
| `cargo` | Latest | Rust package manager |
| `clippy` | Latest | Rust linter |
| `rustfmt` | Latest | Rust code formatter |
| `just` | Latest | Task runner (alternative to Make) |
| `erlc` | OTP 26+ | Erlang compiler |
| `erl` | OTP 26+ | Erlang runtime |
| `rebar3` | Latest | Erlang build tool |
| `gh` | Latest | GitHub CLI |
| `copilot` | Latest | GitHub Copilot CLI (AI assistant) |
| `node` | LTS | Node.js runtime |
| `npm` | Latest | Node package manager |

#### Cargo Extensions

Pre-installed via `cargo-binstall`:
- `cargo-watch` — Auto-rebuild on file changes
- `cargo-nextest` — Faster test runner
- `cargo-insta` — Snapshot testing
- `cargo-llvm-cov` — Code coverage reports

#### VS Code Extensions

Automatically installed in devcontainer:
- `rust-lang.rust-analyzer` — Rust language server
- `tamasfe.even-better-toml` — TOML file support
- `vadimcn.vscode-lldb` — Debugger for Rust
- `usernamehw.errorlens` — Inline error display
- `erlang-ls.erlang-ls` — Erlang language server
- `github.copilot` — AI pair programming
- `github.vscode-github-actions` — GitHub Actions integration
- `github.vscode-pull-request-github` — PR management
- `linear.linear` — Linear issue tracking

### Troubleshooting

#### `cargo clean` fails with "Device or resource busy"

The `target/` directory is mounted as a Docker volume for performance. Use `just clean` instead:

```bash
# ❌ Fails in devcontainer
cargo clean

# ✅ Works in devcontainer
just clean

# Or manually (safe pattern that avoids .. expansion)
find target -mindepth 1 -maxdepth 1 -exec rm -rf {} +
```

#### Devcontainer won't start

```bash
# Rebuild container from scratch
# In VS Code: Ctrl+Shift+P → Dev Containers: Rebuild Container
```

#### Git authentication fails

```bash
# Inside container, check gh authentication
gh auth status

# If failed, login manually
gh auth login
```

#### Worktree `.git` file corruption

```powershell
# If `.git` file points to container paths, fix it:
.\scripts\worktree-rm.ps1 BT-99 -Force  # Will auto-fix before removal
```

#### Port conflicts

If REPL port is already in use, override in `.env`:
```bash
BEAMTALK_REPL_PORT=9999
BEAMTALK_NODE_NAME=beamtalk_custom@localhost
```

---

## Project Status

**Active development** — the compiler core is working with an interactive REPL.

### What Works Now

- ✅ **REPL** — Interactive evaluation with variable persistence
- ✅ **Lexer & Parser** — Full expression parsing with error recovery
- ✅ **Core Erlang codegen** — Compiles to BEAM bytecode via `erlc`
- ✅ **Actors** — Spawn actors with state, send async messages
- ✅ **Cascades** — Multiple messages to same receiver
- ✅ **Map literals** — `#{key => value}` syntax with Dictionary codegen
- ✅ **Class definitions** — AST support for class and method definitions
- ✅ **Standard library** — Boolean, Block, Integer, String, Collections

### In Progress

- 🔄 **Field assignments** — Actor state mutations
- 🔄 **Method dispatch** — Full message routing
- 🔄 **Supervision trees** — Declarative fault tolerance

### Planned

- 📋 **LSP** — Language server for IDE integration
- 📋 **Live browser** — Smalltalk-style class browser (Phoenix `LiveView`)
- 📋 **Hot patching** — Edit running actors in place

---

## Documentation

📚 **[Documentation Index](docs/README.md)** — Start here for a guided tour

### Core Documents

- [Design Principles](docs/beamtalk-principles.md) — 13 core principles guiding all decisions
- [Language Features](docs/beamtalk-language-features.md) — Syntax, semantics, and examples
- [Syntax Rationale](docs/beamtalk-syntax-rationale.md) — Why we keep/change Smalltalk conventions
- [Object Model](docs/beamtalk-object-model.md) — How Smalltalk objects map to BEAM

### Architecture

- [Architecture](docs/beamtalk-architecture.md) — Compiler pipeline, runtime, hot reload
- [Testing Strategy](docs/development/testing-strategy.md) — How we verify compiler correctness

### Tooling & Vision

- [Agent-Native Development](docs/beamtalk-agent-native-development.md) — AI agents as developers and live actor systems

---

## Examples & Standard Library

### Examples ([examples/](examples/))

Simple programs demonstrating language features:

```bash
cargo run -- repl
> :load examples/hello.bt
```

### Standard Library ([lib/](lib/))

Foundational classes implementing "everything is a message":

| Class | Description |
|-------|-------------|
| `Actor` | Base class for all actors |
| `Block` | First-class closures |
| `True` / `False` | Boolean control flow |
| `Integer` | Arbitrary precision arithmetic |
| `String` | UTF-8 text operations |
| `Array` / `List` | Ordered collections |
| `Set` / `Dictionary` | Unordered collections |
| `Nil` | Null object pattern |

See [lib/README.md](lib/README.md) for full documentation.

---

## Repository Structure

```text
beamtalk/
├── crates/
│   ├── beamtalk-core/       # Lexer, parser, AST, codegen
│   └── beamtalk-cli/        # Command-line interface & REPL
├── lib/                      # Standard library (.bt files)
├── runtime/                  # Erlang runtime (actors, REPL backend)
├── docs/                     # Design documents
├── examples/                 # Example programs
└── test-package-compiler/    # Snapshot tests for compiler
```

The compiler is written in **Rust** and generates **Core Erlang**, which compiles to BEAM bytecode via `erlc`.

---

## Inspiration

Beamtalk combines ideas from:

- **Smalltalk/Newspeak** — Live programming, message-based syntax, reflection (inspiration, not strict compatibility)
- **Erlang/BEAM** — Actors, fault tolerance, hot code reload, distribution
- **Elixir** — Pipe operator, protocols, comprehensions, with blocks
- **Gleam** — Result types, exhaustive pattern matching
- **Dylan** — Sealing, conditions/restarts, method combinations
- **TypeScript** — Compiler-as-language-service architecture

---

## Contributing

See [AGENTS.md](AGENTS.md) for development guidelines, coding standards, and task tracking.

We use [Linear](https://linear.app) for issue tracking (project prefix: `BT`).

---

## License

Licensed under the Apache License, Version 2.0. See [LICENSE](LICENSE) for details.
