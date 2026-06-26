# Contributing to Beamtalk

Thank you for your interest in contributing to Beamtalk! This guide covers everything you need to get started as a human contributor. For AI agent guidelines, see [AGENTS.md](AGENTS.md).

## Code of Conduct

Be kind, respectful, and constructive. We're building something fun — let's keep it that way.

## Getting Started

### Prerequisites

- **Rust** (latest stable) — compiler is written in Rust
- **Erlang/OTP** — runtime target; `erlc` must be on your PATH. The supported
  version is pinned in the repo-root `.tool-versions` (with Elixir and rebar3)
  — `mise install` (or asdf) sets up all three; CI installs from the same file
- **Docker Desktop** — for devcontainer support (recommended)
- **VS Code** — with the [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)

> **Optional — LiveView IDE (`editors/liveview`):** the web IDE is a Phoenix/Mix
> project. The pinned Elixir from `.tool-versions` covers it — **never** the
> distro `elixir` package, which pins an old `erlang`. The devcontainer
> installs it for you. See
> [`editors/liveview/README.md`](editors/liveview/README.md).

### Development Environment

The easiest way to get started is with the devcontainer:

1. Clone the repository:
   ```bash
   git clone https://github.com/jamesc/beamtalk.git
   cd beamtalk
   ```
2. Open in VS Code and click **Reopen in Container** when prompted
3. Wait for the container to build (~5 minutes the first time)
4. Verify everything works:
   ```bash
   just ci
   ```

The devcontainer includes all tools pre-configured: Rust toolchain, Erlang/OTP, rebar3, Just, GitHub CLI, and VS Code extensions.

### Without Devcontainer

If you prefer a local setup:

1. Install Rust via [rustup](https://rustup.rs/)
2. Install Erlang/OTP, Elixir, and rebar3 with [mise](https://mise.jdx.dev)
   (or asdf): `mise install` from the repo root reads the pinned versions in
   `.tool-versions`
3. Install [Just](https://github.com/casey/just): `cargo install just`
4. Build and test:
   ```bash
   just ci
   ```
5. _(Optional, for the LiveView IDE)_ `just web-setup` — the pinned Elixir
   from step 2 covers it; never use the distro `elixir` package.

### GitHub & Git Setup

The devcontainer authenticates the GitHub CLI from a `GH_TOKEN` environment
variable on the host:

| Variable | Purpose | How to set |
|----------|---------|------------|
| `GH_TOKEN` | GitHub authentication for the devcontainer | `gh auth login`, then `$env:GH_TOKEN = (gh auth token)` |

Authentication resolves in priority order: (1) the `GH_TOKEN` env var from the
host, (2) the VS Code credential helper (auto-configured), (3) a manual
`gh auth login` inside the container. Verify with `gh auth status`.

Set your commit identity either via environment variables (the
`postStartCommand` in `devcontainer.json` applies them automatically) or
global git config inside the container:

```bash
export GIT_USER_NAME="Your Name"
export GIT_USER_EMAIL="you@example.com"
# or:
git config --global user.name "Your Name"
git config --global user.email "you@example.com"
```

Linear issue tracking uses the Linear MCP server (`.vscode/mcp.json`), which
authenticates via OAuth — no API token required.

### Commit Signing (SSH)

For verified commits, configure SSH signing:

```bash
# 1. Generate a signing key (on the host)
ssh-keygen -t ed25519 -C "you@example.com" -f ~/.ssh/id_ed25519_signing

# 2. Add the public key to GitHub as a *Signing Key*
#    (Settings → SSH and GPG keys → New SSH key → key type: Signing Key)
cat ~/.ssh/id_ed25519_signing.pub

# 3. Configure git (inside the container)
git config --global gpg.format ssh
git config --global user.signingkey ~/.ssh/id_ed25519_signing
git config --global commit.gpgsign true

# 4. Add the private key to the SSH agent
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519_signing
```

## Building & Testing

We use [Just](https://github.com/casey/just) as our task runner. Run `just --list` to see all available commands.

### Common Commands

```bash
just build          # Build everything (Rust + Erlang + stdlib)
just test           # Run Rust tests + Erlang runtime + stdlib tests
just test-stdlib    # Compiled language feature tests (part of `just test`)
just test-repl-protocol  # REPL TCP-protocol tests (slow, full language validation)
just ci                  # Full CI: build, lint, test, test-stdlib, test-integration, test-mcp, test-repl-protocol

just fmt            # Format all code
just fmt-check      # Check formatting without changing files
just clippy         # Rust linter (warnings = errors)
just dialyzer       # Erlang type checking

just repl           # Start the REPL
just clean          # Clean build artifacts
```

### Test Layers

| Layer | Command | What it tests |
|-------|---------|---------------|
| **Rust unit/integration tests** | `just test-rust` | Parser, AST, codegen |
| **Erlang unit tests** | `just test-runtime` | Runtime modules in isolation |
| **Stdlib tests** | `just test-stdlib` | Language features via compiled expressions |
| **REPL protocol tests** | `just test-repl-protocol` | Full REPL TCP-protocol integration |

Run `just test` for fast feedback during development. Run `just ci` before submitting a PR.

## Making Changes

### Workflow

1. **Find or create an issue** — We use [Linear](https://linear.app) for issue tracking (project prefix: `BT`). Check for existing issues before starting new work.
2. **Create a branch** — Use the format `BT-{number}-{short-description}`:
   ```bash
   git checkout main
   git pull origin main
   git checkout -b BT-42-fix-parser-error
   ```
3. **Make small, focused changes** — One logical change per commit.
4. **Test frequently** — Run `just test` after each change.
5. **Run CI before pushing** — `just ci` catches most issues.
6. **Push and open a PR** — Reference the Linear issue in the PR title.

### Commit Messages

Use [conventional commits](https://www.conventionalcommits.org/) with the Linear issue ID:

```
feat: add string interpolation support BT-42
fix: handle empty block in parser BT-99
docs: update REPL tutorial with cascade examples BT-18
refactor: extract codegen helpers into util module BT-200
test: add E2E tests for actor supervision BT-150
```

### License Headers

All source files must include the Apache 2.0 header:

```rust
// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
```

## Project Structure

```
beamtalk/
├── crates/
│   ├── beamtalk-core/       # Lexer, parser, AST, codegen
│   └── beamtalk-cli/        # CLI, REPL client, test runners
├── stdlib/src/                # Standard library (.bt source files)
├── runtime/                  # Erlang runtime (OTP apps)
│   └── apps/
│       ├── beamtalk_runtime/ # Core runtime (primitives, actors, objects)
│       ├── beamtalk_workspace/ # REPL, workspace, session management
│       └── beamtalk_stdlib/  # Compiled stdlib (.beam files)
├── docs/                     # Design documents, ADRs
├── examples/                 # Example Beamtalk programs
├── tests/
│   ├── stdlib/               # Compiled expression tests
│   └── repl-protocol/        # REPL TCP protocol tests
└── test-package-compiler/    # Compiler snapshot tests
```

### Key Documentation

| Document | Purpose |
|----------|---------|
| [Language Features](docs/beamtalk-language-features.md) | Full syntax and semantics reference |
| [Syntax Rationale](docs/beamtalk-syntax-rationale.md) | Why we diverge from Smalltalk |
| [Design Principles](docs/beamtalk-principles.md) | Core philosophy and values |
| [Architecture](docs/beamtalk-architecture.md) | Compiler pipeline and runtime design |
| [Testing Strategy](docs/development/testing-strategy.md) | How we verify correctness |
| [ADRs](docs/ADR/README.md) | Architecture Decision Records |

## Pull Request Guidelines

### Before Submitting

- [ ] `just ci` passes locally
- [ ] Changes are focused on a single issue
- [ ] New code has tests
- [ ] Documentation is updated if user-facing behavior changed
- [ ] Commit messages follow conventional format with issue ID

### PR Description

Include:
- **What** changed and **why**
- Linear issue reference (e.g., `Closes BT-42`)
- Any notable design decisions

### Review Process

- All PRs require review before merging
- Reviewers check for correctness, test coverage, and adherence to project conventions
- Address review comments by pushing new commits (don't force-push during review)

## CI & Release Workflows

We have three GitHub Actions workflows:

| Workflow | Trigger | Purpose |
|----------|---------|---------|
| **CI** (`.github/workflows/ci.yml`) | Push to `main`, PRs | Build, lint, test, coverage |
| **Release** (`.github/workflows/release.yml`) | GitHub Release published, manual | Build distributable tarball, upload as release asset |
| **Fuzz** (`.github/workflows/fuzz.yml`) | Nightly (2 AM UTC) | Fuzz parser for crash safety |

### Creating a Release

1. Go to **Releases** on the GitHub repository
2. Click **Draft a new release**
3. Create a new tag (e.g., `v0.1.0`)
4. Publish the release

The release workflow will automatically:
- Build the full distribution (`just dist`)
- Smoke test the installed layout (`just test-install`)
- Package as `beamtalk-{version}-linux-x86_64.tar.gz`
- Attach the tarball to the GitHub release

You can also trigger the workflow manually via **Actions → Release → Run workflow** for testing.

## Where to Contribute

### Good First Issues

Look for issues labeled `agent-ready` with size `S` in Linear — these are well-specified, small tasks.

### Areas That Need Help

— **Standard library** — Implementing methods on core classes (`stdlib/src/`)
- **Documentation** — Examples, tutorials, API docs
- **Tests** — Expanding stdlib and REPL protocol test coverage
- **Examples** — Real-world Beamtalk programs in `examples/`

## Questions?

- Check the [README](README.md) for an overview
- Read the [REPL Tutorial](examples/repl-tutorial.md) to try the language
- Browse [docs/](docs/) for design documents
- Open an issue if you're stuck — we're happy to help!
