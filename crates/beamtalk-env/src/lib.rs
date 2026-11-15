// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared leaf: Beamtalk environment resolution.
//!
//! **DDD Context:** Infrastructure (shared leaf, no domain logic)
//!
//! One leaf crate for the two small, dependency-free environment lookups that
//! several crates must agree on without depending on each other — the
//! "shared-leaf-module pattern" from
//! `docs/development/architecture-principles.md` §6:
//!
//! - [`home`]: the global config directory (`~/.beamtalk`), used by
//!   `beamtalk-project` and `beamtalk-workspace`.
//! - [`sysroot`]: the installation sysroot derived from the running
//!   executable path, used by `beamtalk-cli`, `beamtalk-lsp`,
//!   `beamtalk-package` and `beamtalk-examples`.

pub mod home;
pub mod sysroot;

pub use home::beamtalk_root_dir;
pub use sysroot::{current_sysroot, sysroot_from_exe_path};
