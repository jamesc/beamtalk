# beamtalk-package

Leaf crate holding the offline package-analysis pipeline shared by the
`beamtalk` CLI and `beamtalk-mcp`: `manifest`, `build_layout`,
`dependency_classes`, `ffi_type_registry`, `native_type_specs`,
`erlang_lint`, `lint_pipeline` and `content_hash`. It depends on neither
`beamtalk-cli` nor any CLI-only crate (`clap`, `rustyline`, ...).
