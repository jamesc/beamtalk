# beamtalk-home

Shared leaf crate: Beamtalk global config directory (`~/.beamtalk`) resolution.

Provides a single authoritative `beamtalk_root_dir()` function so both `beamtalk-core`
and `beamtalk-workspace` can resolve the `~/.beamtalk` path without either depending on
the other — the shared-leaf-module pattern from
[`docs/development/architecture-principles.md`](../../docs/development/architecture-principles.md) §6.
