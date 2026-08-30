# beamtalk-lint

Style/redundancy lint passes for Beamtalk source code (`beamtalk lint`).
Lint checks are distinct from compiler warnings: they're suppressed during
normal `check`/`compile` and only reported by the `lint` command, run
against a parsed `Module` from `beamtalk-core`.
