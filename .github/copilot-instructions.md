# Copilot Instructions for Beamtalk

This file is a short, agent-focused quick reference. For full development guidelines, workflows, and detailed rules (including syntax verification, issue templates, CI, and error-handling conventions), see [AGENTS.md](../AGENTS.md).

Quick reference

- **Linear issues:** Follow the full guidance in [AGENTS.md](../AGENTS.md) — see the "Creating Issue Blocking Relationships" and "Writing Agent-Ready Issues" sections for details on labels, blocking relationships, and formatting.
- **Syntax verification:** Always verify any Beamtalk syntax before using it (language spec, `examples/`, or `tests/`). See the "Syntax Verification - Preventing Hallucinations" section in [AGENTS.md](../AGENTS.md).
- **Allowed commands (agent shortcuts):** `just`, `cargo`, `rustc`, `rustfmt`, `git` — use these without asking.
- **Common CI commands:** `just ci`, `just build`, `just test`, `just test-stdlib`, `just test-e2e`.
- **PR / comments:** When scripting GitHub interactions prefer `gh api` with `--paginate`; see examples in `AGENTS.md`.
- **Docs in PRs:** When adding documentation notes to a PR, include which sections helped, what could improve, and one short actionable suggestion (see `AGENTS.md` example).

If you'd like, I can further shorten this file to a single pointer to `AGENTS.md` or move any remaining quick examples into a separate `docs/` note. Proceed with that change?

This is completely optional but helps us understand which parts of AGENTS.md are useful vs noise.
