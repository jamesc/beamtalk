# Copilot Instructions for Beamtalk

## Custom Skills

This repository has custom skills in `.github/skills/` that provide detailed workflows. Use these commands:

| Command | Description |
|---------|-------------|
| `/next-issue` | Start working on next Linear issue from backlog |
| `/done` | Complete work, commit, push, and create PR |
| `/whats-next` | Get recommendations for what to work on next |
| `/pr-resolve` | Address all PR review comments systematically |
| `/merge-resolve` | Update main, merge into current branch, resolve conflicts |

See `.github/skills/` for full workflow details, or ask Copilot about any command.

---

### Creating Linear Issues

When creating Linear issues with dependencies:

1. **Create the issues** with full context, acceptance criteria, and files to modify
2. **Set agent-state label**:
   - `agent-ready` if fully specified (all acceptance criteria clear)
   - `needs-spec` if human clarification needed before work can start
3. **Set Item Area label** (which component is affected):
   - `class-system` - Class definition, parsing, codegen, and runtime
   - `stdlib` - Standard library: collections, primitives, strings
   - `repl` - REPL backend and CLI interaction
   - `cli` - Command-line interface and build tooling
   - `codegen` - Code generation to Core Erlang/BEAM
   - `runtime` - Erlang runtime: actors, futures, OTP integration
   - `parser` - Lexer, parser, AST
4. **Always set up blocking relationships** using Linear's GraphQL API:
   ```graphql
   mutation {
     issueRelationCreate(input: {
       issueId: "<blocker issue ID>"
       relatedIssueId: "<blocked issue ID>"
       type: blocks
     }) { success }
   }
   ```
5. **Add to relevant projects** if applicable
6. **Set priority** based on urgency

See [AGENTS.md](../AGENTS.md) "Creating Issue Blocking Relationships" section for details.

---

## GitHub PR and Comment Access

When working with GitHub PRs and comments, use these patterns to reliably fetch all data:

### Get all PR review comments (with pagination)

```bash
# All top-level review comments (not replies)
gh api repos/{owner}/{repo}/pulls/{pr}/comments --paginate --jq '.[] | select(.in_reply_to_id == null) | {id, path, line, body, user: .user.login}'

# Recent comments (filter by timestamp)
gh api repos/{owner}/{repo}/pulls/{pr}/comments --paginate --jq '[.[] | select(.created_at > "2026-01-29T12:00:00Z")] | .[] | {id, body, user: .user.login, in_reply_to_id}'

# Comments sorted by date (most recent last)
gh api repos/{owner}/{repo}/pulls/{pr}/comments --jq '[.[] | {id, body: .body[0:200], user: .user.login, in_reply_to_id, created_at}] | sort_by(.created_at) | .[-15:]'
```

### Get replies from reviewers (not the PR author)

```bash
# Find follow-up comments from others (replies where user is not the owner)
gh api repos/{owner}/{repo}/pulls/{pr}/comments --jq '.[] | select(.in_reply_to_id != null and .user.login != "jamesc") | {id, body, user: .user.login}'
```

### Reply to a review comment

```bash
gh api repos/{owner}/{repo}/pulls/{pr}/comments/{comment_id}/replies -f body="Your reply here"
```

### Get PR conversation comments (issue-level, not review comments)

```bash
gh api repos/{owner}/{repo}/issues/{pr}/comments --jq '.[] | {id, body, user: .user.login, created_at}'
```

### Key tips

1. **Always use `--paginate`** when fetching comments to ensure you get all of them (API returns max 30 by default)
2. **Filter by `created_at`** to find new comments since your last check
3. **Check `in_reply_to_id`** to distinguish top-level comments from replies
4. **Truncate body** in listings (`.body[0:200]`) to keep output readable
5. **Sort by `created_at`** and take the last N to see most recent activity

---

## Project Context

This is the Beamtalk compiler project - a Smalltalk-**like** language targeting the BEAM VM. While heavily inspired by Smalltalk's syntax and philosophy, Beamtalk makes pragmatic departures for BEAM compatibility and modern ergonomics. See [AGENTS.md](../AGENTS.md) for full development guidelines and [docs/beamtalk-syntax-rationale.md](../docs/beamtalk-syntax-rationale.md) for specific differences from Smalltalk.

## Allowed Commands

You may always run these commands without asking for permission:
- `cargo` (build, test, clippy, fmt, run, check, etc.)
- `rustc`
- `rustfmt`
- `git` (status, diff, log, branch, etc.)

Static checks required before any commit (must match CI exactly):
- `cargo build --all-targets` - Build all targets
- `cargo clippy --all-targets -- -D warnings` - Lints (warnings are errors)
- `cargo fmt --all -- --check` - Code formatting check
- `cargo test --all-targets` - Run all tests
