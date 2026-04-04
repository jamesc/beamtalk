// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared builder for `erlc` command invocations.
//!
//! All native Erlang compilation goes through [`ErlcInvocation`] so that flag
//! assembly is consistent (e.g. `+debug_info`, `-I`, `-pa`, `ERL_LIBS`).

use camino::{Utf8Path, Utf8PathBuf};
use miette::{IntoDiagnostic, Result, WrapErr};
use std::process::Command;

/// Builder for an `erlc` command invocation.
///
/// Accumulates flags, include paths, code paths, and source files, then
/// produces a [`std::process::Command`] via [`Self::build`].
#[derive(Debug)]
pub struct ErlcInvocation {
    output_dir: Utf8PathBuf,
    debug_info: bool,
    docs: bool,
    erl_libs: Option<Utf8PathBuf>,
    include_dirs: Vec<Utf8PathBuf>,
    code_paths: Vec<Utf8PathBuf>,
    source_files: Vec<Utf8PathBuf>,
}

impl ErlcInvocation {
    /// Create a new invocation targeting `output_dir` for compiled `.beam` files.
    #[must_use]
    pub fn new(output_dir: impl Into<Utf8PathBuf>) -> Self {
        Self {
            output_dir: output_dir.into(),
            debug_info: false,
            docs: false,
            erl_libs: None,
            include_dirs: Vec::new(),
            code_paths: Vec::new(),
            source_files: Vec::new(),
        }
    }

    /// Enable `+debug_info` flag.
    #[must_use]
    pub fn debug_info(mut self) -> Self {
        self.debug_info = true;
        self
    }

    /// Enable EEP-48 documentation chunk generation.
    ///
    /// On OTP 27+, source files that use `-doc` / `-moduledoc` attributes
    /// automatically get a `Docs` chunk in the compiled `.beam`. This method
    /// adds `+warn_missing_doc` so that `erlc` emits warnings for exported
    /// functions and modules that lack `-doc` / `-moduledoc` attributes,
    /// encouraging developers to add documentation that will appear in
    /// `:help` output.
    #[must_use]
    pub fn docs(mut self) -> Self {
        self.docs = true;
        self
    }

    /// Set `ERL_LIBS` environment variable (only applied if the directory exists).
    #[must_use]
    pub fn erl_libs(mut self, dir: &Utf8Path) -> Self {
        if dir.exists() {
            self.erl_libs = Some(dir.to_owned());
        }
        self
    }

    /// Add an include directory (`-I`), only if it exists on disk.
    #[must_use]
    pub fn include_dir(mut self, dir: impl Into<Utf8PathBuf>) -> Self {
        let dir = dir.into();
        if dir.exists() {
            self.include_dirs.push(dir);
        }
        self
    }

    /// Add the beamtalk runtime include path (`-I`), resolving dev vs installed layout.
    ///
    /// For dev builds this is `runtime/apps/`, for installed builds `PREFIX/lib/beamtalk/lib/`.
    #[must_use]
    pub fn runtime_include(self) -> Self {
        if let Ok((runtime_dir, layout)) = crate::repl_startup::find_runtime_dir_with_layout() {
            let include_parent = match layout {
                crate::repl_startup::RuntimeLayout::Dev => runtime_dir.join("apps"),
                crate::repl_startup::RuntimeLayout::Installed => runtime_dir.join("lib"),
            };
            if let Ok(utf8) = Utf8PathBuf::try_from(include_parent) {
                self.include_dir(utf8)
            } else {
                self
            }
        } else {
            self
        }
    }

    /// Add a code path (`-pa`).
    #[must_use]
    pub fn code_path(mut self, dir: impl Into<Utf8PathBuf>) -> Self {
        self.code_paths.push(dir.into());
        self
    }

    /// Add multiple code paths (`-pa`).
    #[must_use]
    pub fn code_paths(mut self, dirs: &[Utf8PathBuf]) -> Self {
        self.code_paths.extend(dirs.iter().cloned());
        self
    }

    /// Add a source `.erl` file.
    #[must_use]
    pub fn source_file(mut self, file: impl Into<Utf8PathBuf>) -> Self {
        self.source_files.push(file.into());
        self
    }

    /// Add multiple source `.erl` files.
    #[must_use]
    pub fn source_files(mut self, files: &[Utf8PathBuf]) -> Self {
        self.source_files.extend(files.iter().cloned());
        self
    }

    /// Build the [`std::process::Command`] with all accumulated flags.
    #[must_use]
    pub fn build(&self) -> Command {
        let mut cmd = Command::new("erlc");

        if self.debug_info {
            cmd.arg("+debug_info");
        }

        if self.docs {
            cmd.arg("+warn_missing_doc");
        }

        cmd.arg("-o").arg(self.output_dir.as_str());

        if let Some(ref erl_libs) = self.erl_libs {
            cmd.env("ERL_LIBS", erl_libs.as_str());
        }

        for dir in &self.include_dirs {
            cmd.arg("-I").arg(dir.as_str());
        }

        for path in &self.code_paths {
            cmd.arg("-pa").arg(path.as_str());
        }

        for file in &self.source_files {
            cmd.arg(file.as_str());
        }

        cmd
    }

    /// Build the command, run it, and return an error if it fails.
    ///
    /// Uses `.output()` to capture stdout/stderr for error reporting.
    ///
    /// # Errors
    ///
    /// Returns an error if `erlc` cannot be spawned or exits with a non-zero
    /// status. The `context` string is included in the error message.
    pub fn run(&self, context: &str) -> Result<()> {
        let output = self
            .build()
            .output()
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to run erlc for {context}"))?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            let stdout = String::from_utf8_lossy(&output.stdout);
            miette::bail!(
                "{context} failed:\n{}",
                format!("{stdout}{stderr}").trim_end()
            );
        }

        Ok(())
    }

    /// Build the command, run it via `.status()`, and return an error if it fails.
    ///
    /// Unlike [`Self::run`], this does not capture output — erlc writes directly
    /// to the terminal. Useful for simple single-file compilations.
    ///
    /// # Errors
    ///
    /// Returns an error if `erlc` cannot be spawned or exits with a non-zero
    /// status. The `context` string is included in the error message.
    pub fn run_status(&self, context: &str) -> Result<()> {
        let status = self
            .build()
            .status()
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to run erlc for {context}"))?;

        if !status.success() {
            miette::bail!("{context} failed. Is Erlang/OTP installed?");
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_invocation() {
        let inv = ErlcInvocation::new("/tmp/out").source_file(Utf8PathBuf::from("/tmp/foo.erl"));
        let cmd = inv.build();
        let args: Vec<_> = cmd.get_args().map(|a| a.to_str().unwrap()).collect();
        assert_eq!(args, vec!["-o", "/tmp/out", "/tmp/foo.erl"]);
    }

    #[test]
    fn test_debug_info() {
        let inv = ErlcInvocation::new("/tmp/out")
            .debug_info()
            .source_file(Utf8PathBuf::from("/tmp/foo.erl"));
        let cmd = inv.build();
        let args: Vec<_> = cmd.get_args().map(|a| a.to_str().unwrap()).collect();
        assert_eq!(args, vec!["+debug_info", "-o", "/tmp/out", "/tmp/foo.erl"]);
    }

    #[test]
    fn test_docs() {
        let inv = ErlcInvocation::new("/tmp/out")
            .debug_info()
            .docs()
            .source_file(Utf8PathBuf::from("/tmp/foo.erl"));
        let cmd = inv.build();
        let args: Vec<_> = cmd.get_args().map(|a| a.to_str().unwrap()).collect();
        assert_eq!(
            args,
            vec![
                "+debug_info",
                "+warn_missing_doc",
                "-o",
                "/tmp/out",
                "/tmp/foo.erl"
            ]
        );
    }

    #[test]
    fn test_code_paths() {
        let inv = ErlcInvocation::new("/tmp/out")
            .code_path("/tmp/ebin1")
            .code_path("/tmp/ebin2")
            .source_file(Utf8PathBuf::from("/tmp/foo.erl"));
        let cmd = inv.build();
        let args: Vec<_> = cmd.get_args().map(|a| a.to_str().unwrap()).collect();
        assert_eq!(
            args,
            vec![
                "-o",
                "/tmp/out",
                "-pa",
                "/tmp/ebin1",
                "-pa",
                "/tmp/ebin2",
                "/tmp/foo.erl"
            ]
        );
    }
}
