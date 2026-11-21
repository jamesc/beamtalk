# Scripts

Helper scripts for development, packaging and CI. Run them from the repository
root. CI-only checks live in [`ci/`](ci/); most are run through `just` recipes
(`just --list`), so prefer the recipe when one exists.

## Development and environment

| Script | Description |
|--------|-------------|
| `lint-stamp.sh` | Lint stamps shared by the pre-push hook and the Justfile (`check`/`write`) |
| `cloud-doctor.sh` | Health check for cloud / MITM-proxy sandboxes |
| `setup-cloud.sh` | Setup for cloud sessions |
| `refresh-edge.sh` | Refresh the installed edge compiler when a newer `edge` release is published |
| `hex-bridge-proxy.py` | HTTP-to-HTTPS bridge proxy for hex.pm in cloud environments |
| `bench-compilation.sh` | Compilation latency benchmark (file builds and REPL expressions) |

## Install and packaging

| Script | Description |
|--------|-------------|
| `install.sh` | The installer served at `install.sh` on the project site |
| `install-otp-apps.sh` | Copy the built OTP applications into an installed layout |
| `install-share.sh` | Install the platform-independent `share/` data |
| `version.escript` | Read `VERSION` and append a dev suffix off a release tag |

## Generators, fixtures and validation

| Script | Description |
|--------|-------------|
| `gen_cli_reference.py` | Generate `docs/cli-workspace.md` from clap help (`just gen-cli-reference`) |
| `gen_op_table_fixture.escript` | Regenerate the shared REPL op-table fixture |
| `gen-otp-curated-exports.escript` | Regenerate `otp_curated_exports.txt` |
| `gen-actor-closure-matrix.py` | ADR 0137 actor-field-closure matrix |
| `gen-bt4367arm-local-grid.py`, `gen-bt4460-plain-arm-nested-grid.py`, `gen-bt4461-repl-arm-cached-binding-grid.py` | ADR 0131 differential grid fixtures |
| `compile-pipeline-corpus-lint.escript` | Lint the fuzz `compile_pipeline` corpus |
| `validate_specs.escript`, `evaluate_spec_coverage.escript` | Validate generated `-spec` attributes; measure OTP spec coverage |
| `convert_tests.py` | Convert `// =>` expression tests to BUnit format |
| `clean-covertool-xml.py` | Clean covertool XML reports |

Test fixtures for the runtime are compiled by
`runtime/apps/beamtalk_runtime/test_fixtures/compile_fixtures.escript`.
