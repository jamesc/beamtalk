# beamtalk-sysroot

Shared leaf crate: Beamtalk installation sysroot resolution from the running executable path.

Provides a single authoritative `sysroot_from_exe_path()` derivation (`{sysroot}/bin/beamtalk`
→ `{sysroot}`) so `beamtalk-cli` (`--print-sysroot`, distribution FFI stub discovery) and
`beamtalk-lsp` (installed stdlib source discovery) agree on the sysroot convention without
either depending on the other — the shared-leaf-module pattern from
[`docs/development/architecture-principles.md`](../../docs/development/architecture-principles.md) §6.
