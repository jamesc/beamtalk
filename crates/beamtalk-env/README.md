# beamtalk-env

Shared leaf crate: Beamtalk environment resolution.

- `beamtalk_root_dir()` — the global config directory (`~/.beamtalk`, overridable with
  `BEAMTALK_HOME`), shared by `beamtalk-project` and `beamtalk-workspace`.
- `sysroot_from_exe_path()` / `current_sysroot()` — the installation sysroot
  (`{sysroot}/bin/beamtalk` → `{sysroot}`), shared by `beamtalk-cli`, `beamtalk-lsp`,
  `beamtalk-package` and `beamtalk-examples`.

Both live below their consumers so no one depends on another — the shared-leaf-module pattern from
[`docs/development/architecture-principles.md`](../../docs/development/architecture-principles.md) §6.
