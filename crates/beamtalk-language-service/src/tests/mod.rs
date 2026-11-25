// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk-language-service` tests, split by feature (mirroring
//! `beamtalk-core`'s `type_checker/tests/` layout).
//!
//! Shared fixtures live in `common`. Each feature module is kept well under
//! the ~2000-line guideline in `rust-guidelines.md` § Testing so parallel PRs
//! adding tests rarely conflict on the same region.

mod common;
mod deep_input_stack_safety;

mod adr0144_protocol_metatypes;
mod alias_and_cross_file_diagnostics;
mod file_interface;
mod find_references_click_rejection;
mod goto_definition_navigation;
mod identifier_lookup_type_annotations;
mod native_delegate_ffi_and_code_actions;
mod native_types_and_protocol_registry;
mod pipeline_stage_navigation;
mod property_tests;
mod rename;
mod shape_keys;
mod simple_service_and_call_hierarchy;
mod type_hierarchy;
