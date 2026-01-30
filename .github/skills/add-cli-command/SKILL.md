---
name: add-cli-command
description: Add a new CLI command to the Beamtalk compiler. Use when adding new command-line functionality like new subcommands or flags.
---

# Adding a CLI Command

Follow this workflow when adding a new command to the Beamtalk CLI:

## Steps

1. **Add command enum variant** in `crates/beamtalk-cli/src/main.rs`
   - Add to the `Commands` enum with clap attributes
   - Include appropriate documentation for `--help`

2. **Implement handler function**
   - Create the command handler with appropriate error handling
   - Return `Result` for operations that can fail
   - Use `miette` for user-friendly error messages

3. **Add integration test**
   - Test in `crates/beamtalk-cli/tests/` or inline
   - Test both success and error cases
   - Test `--help` output if complex

4. **Update `--help` documentation**
   - Add clear description of what the command does
   - Document all flags and arguments
   - Include examples in the help text if useful

## Example

```rust
// In main.rs
#[derive(Parser)]
enum Commands {
    /// Existing commands...
    
    /// Frobnicate the specified file
    #[command(name = "frob")]
    Frobnicate {
        /// Path to the file to frobnicate
        #[arg(value_name = "FILE")]
        path: PathBuf,
        
        /// Enable verbose output
        #[arg(short, long)]
        verbose: bool,
    },
}

// Handler
fn handle_frobnicate(path: PathBuf, verbose: bool) -> miette::Result<()> {
    if verbose {
        println!("Frobnicating {}...", path.display());
    }
    // ... implementation ...
    Ok(())
}
```

## Testing

```rust
#[test]
fn test_frobnicate_command() {
    let result = Command::cargo_bin("beamtalk")
        .unwrap()
        .arg("frob")
        .arg("test.bt")
        .assert()
        .success();
}
```
