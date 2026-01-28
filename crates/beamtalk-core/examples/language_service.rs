// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Example demonstrating the Language Service API.
//!
//! This example shows how to use the language service to get
//! diagnostics, completions, hover info, and navigation features.

use beamtalk_core::language_service::{LanguageService, Position, SimpleLanguageService};
use camino::Utf8PathBuf;

fn main() {
    println!("Beamtalk Language Service Example\n");
    println!("==================================\n");

    // Create a language service instance
    let mut service = SimpleLanguageService::new();

    // Example 1: Parse valid code and get diagnostics
    println!("1. Valid Code - No Diagnostics");
    println!("   Source: x := 42");
    let file = Utf8PathBuf::from("example.bt");
    service.update_file(file.clone(), "x := 42".to_string());

    let diagnostics = service.diagnostics(&file);
    println!(
        "   Diagnostics: {}",
        if diagnostics.is_empty() {
            "None ✓"
        } else {
            "Some errors"
        }
    );
    println!();

    // Example 2: Parse invalid code and get diagnostics
    println!("2. Invalid Code - Syntax Error");
    println!("   Source: x := :=");
    service.update_file(file.clone(), "x := :=".to_string());

    let diagnostics = service.diagnostics(&file);
    println!("   Diagnostics: {} error(s)", diagnostics.len());
    for diag in &diagnostics {
        println!("     - {}", diag.message);
    }
    println!();

    // Example 3: Get completions
    println!("3. Code Completions");
    service.update_file(file.clone(), "count := 0.\nresult := count".to_string());

    let completions = service.completions(&file, Position::new(1, 10));
    println!("   Available completions at position (1, 10):");
    for completion in completions.iter().take(5) {
        println!("     - {} ({:?})", completion.label, completion.kind);
    }
    println!("   ... and {} more", completions.len().saturating_sub(5));
    println!();

    // Example 4: Hover information
    println!("4. Hover Information");
    println!("   Hovering over 'count' at position (1, 10)");
    service.update_file(file.clone(), "count := 0.\nresult := count".to_string());

    if let Some(hover) = service.hover(&file, Position::new(1, 10)) {
        println!("   Hover: {}", hover.contents);
    } else {
        println!("   No hover info available");
    }
    println!();

    // Example 5: Go to definition
    println!("5. Go to Definition");
    println!("   Finding definition of 'count' at position (1, 10)");

    if let Some(location) = service.goto_definition(&file, Position::new(1, 10)) {
        println!(
            "   Definition found at: {}:{}",
            location.file,
            location.span.start()
        );
    } else {
        println!("   Definition not found");
    }
    println!();

    // Example 6: Find all references
    println!("6. Find References");
    println!("   Finding all references to 'count'");

    let refs = service.find_references(&file, Position::new(0, 0));
    println!("   Found {} reference(s):", refs.len());
    for (i, loc) in refs.iter().enumerate() {
        println!("     {}. {}:{}", i + 1, loc.file, loc.span.start());
    }
    println!();

    println!("Language Service Example Complete!");
}
