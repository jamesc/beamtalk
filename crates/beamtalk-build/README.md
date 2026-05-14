# beamtalk-build

Shared build-script helpers for beamtalk workspace crates.

Provides `emit_beamtalk_version`, which reads the workspace `VERSION` file and git
state and exposes a `BEAMTALK_VERSION` compile-time env var. Used in the `build.rs`
of `beamtalk-cli` and `beamtalk-compiler-port`.
