// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Static rename planning (BT-5007): class rename across files, unary / binary
//! / keyword selector rename, nested sends in keyword arguments, the confirmed
//! vs candidate split (ADR 0114), overrides, and every refusal.

use super::common::*;

/// Builds a service from `(path, source)` pairs. Exactly one source holds an
/// `@@` caret marker, removed before indexing; returns its file and position.
fn setup(files: &[(&str, &str)]) -> (SimpleLanguageService, Utf8PathBuf, Position) {
    let mut service = SimpleLanguageService::new();
    let mut caret = None;
    for (path, text) in files {
        let path = Utf8PathBuf::from(*path);
        let text = if let Some(at) = text.find("@@") {
            let before = &text[..at];
            let line = u32::try_from(before.matches('\n').count()).unwrap();
            let col =
                u32::try_from(before.len() - before.rfind('\n').map_or(0, |i| i + 1)).unwrap();
            caret = Some((path.clone(), Position::new(line, col)));
            text.replacen("@@", "", 1)
        } else {
            (*text).to_string()
        };
        service.update_file(path, text);
    }
    let (file, pos) = caret.expect("one source needs an @@ caret");
    (service, file, pos)
}

/// Applies `edits` for one file to the service's current text of it.
fn apply(service: &SimpleLanguageService, file: &str, edits: &[RenameEdit]) -> String {
    let file = Utf8PathBuf::from(file);
    let mut text = service.file_source(&file).expect("file indexed");
    let mut mine: Vec<&RenameEdit> = edits.iter().filter(|e| e.file == file).collect();
    mine.sort_by_key(|e| std::cmp::Reverse(e.span.start()));
    for e in mine {
        text.replace_range(e.span.start() as usize..e.span.end() as usize, &e.new_text);
    }
    text
}

fn candidate_edits(plan: &RenamePlan) -> Vec<RenameEdit> {
    plan.candidates.iter().flatten().cloned().collect()
}

#[test]
fn class_rename_covers_declaration_and_references_across_files() {
    let (svc, file, pos) = setup(&[
        ("/p/Counter.bt", "Object subclass: Coun@@ter\n  inc => 1\n"),
        (
            "/p/use.bt",
            "Counter subclass: Sub\n  make => Counter new\n  twin: other :: Counter => other\n",
        ),
    ]);
    let plan = svc.plan_rename(&file, pos, "Tally").expect("plan");
    assert!(plan.candidates.is_empty());
    assert_eq!(
        apply(&svc, "/p/Counter.bt", &plan.confirmed),
        "Object subclass: Tally\n  inc => 1\n"
    );
    assert_eq!(
        apply(&svc, "/p/use.bt", &plan.confirmed),
        "Tally subclass: Sub\n  make => Tally new\n  twin: other :: Tally => other\n"
    );
    let mv = plan.file_move.expect("file named after the class moves");
    assert_eq!(mv.from, "/p/Counter.bt");
    assert_eq!(mv.to, "/p/Tally.bt");
}

#[test]
fn class_rename_from_a_reference_site_and_no_move_when_file_name_differs() {
    let (svc, file, pos) = setup(&[
        ("/p/models.bt", "Object subclass: Counter\n  inc => 1\n"),
        ("/p/use.bt", "x := Coun@@ter new\n"),
    ]);
    let plan = svc.plan_rename(&file, pos, "Tally").expect("plan");
    assert!(plan.file_move.is_none());
    assert_eq!(
        apply(&svc, "/p/use.bt", &plan.confirmed),
        "x := Tally new\n"
    );
    assert_eq!(
        apply(&svc, "/p/models.bt", &plan.confirmed),
        "Object subclass: Tally\n  inc => 1\n"
    );
}

#[test]
fn class_rename_refuses_collision_invalid_name_and_target_file() {
    let (svc, file, pos) = setup(&[
        ("/p/Counter.bt", "Object subclass: Coun@@ter\n  inc => 1\n"),
        ("/p/Other.bt", "Object subclass: Other\n  go => 1\n"),
    ]);
    let err = svc.plan_rename(&file, pos, "Other").unwrap_err();
    assert!(err.message.contains("already exists"), "{}", err.message);
    let err = svc.plan_rename(&file, pos, "tally").unwrap_err();
    assert!(
        err.message.contains("not a valid class name"),
        "{}",
        err.message
    );
    let err = svc.plan_rename(&file, pos, "Counter").unwrap_err();
    assert!(err.message.contains("same"), "{}", err.message);
}

#[test]
fn class_rename_refuses_stdlib_class() {
    let (mut svc, file, pos) = setup(&[
        ("/std/Widget.bt", "Object subclass: Wid@@get\n  go => 1\n"),
        ("/p/use.bt", "Widget new\n"),
    ]);
    svc.mark_stdlib_file(Utf8PathBuf::from("/std/Widget.bt"));
    let err = svc.prepare_rename(&file, pos).unwrap_err();
    assert!(err.message.contains("standard library"), "{}", err.message);
    assert!(svc.plan_rename(&file, pos, "Gadget").is_err());
}

#[test]
fn class_rename_refuses_dependency_class() {
    let (svc, file, pos) = setup(&[
        (
            "/p/_build/deps/json/src/Parser.bt",
            "Object subclass: Par@@ser\n  go => 1\n",
        ),
        ("/p/use.bt", "Parser new\n"),
    ]);
    let err = svc.prepare_rename(&file, pos).unwrap_err();
    assert!(err.message.contains("dependency"), "{}", err.message);
}

#[test]
fn prepare_rename_refuses_literals_keywords_and_locals() {
    for src in [
        "x := 4@@2\n",
        "x := 'he@@llo'\n",
        "Object subclass: Foo\n  bar: a =>\n    t := a.\n    @@t\n",
        "@@\n",
    ] {
        let (svc, file, pos) = setup(&[("/p/a.bt", src)]);
        let err = svc.prepare_rename(&file, pos).unwrap_err();
        assert!(
            err.message.contains("Only class names"),
            "{src}: {}",
            err.message
        );
    }
}

#[test]
fn prepare_rename_returns_token_and_whole_selector_placeholder() {
    let (svc, file, pos) = setup(&[("/p/a.bt", "Object subclass: Foo\n  at: i put@@: v => v\n")]);
    let prepared = svc.prepare_rename(&file, pos).expect("prepared");
    assert_eq!(prepared.placeholder, "at:put:");
    assert_eq!(prepared.kind, RenameKind::Selector);
    let src = svc.file_source(&file).unwrap();
    assert_eq!(
        &src[prepared.span.start() as usize..prepared.span.end() as usize],
        "put:"
    );
}

#[test]
fn unary_selector_rename_splits_confirmed_and_candidates() {
    let (svc, file, pos) = setup(&[
        (
            "/p/Counter.bt",
            "Object subclass: Counter\n  inc@@rement => 1\n  twice => self increment + self increment\n  other => self class increment\n",
        ),
        (
            "/p/Sub.bt",
            "Counter subclass: Sub\n  more => super increment\n",
        ),
        ("/p/use.bt", "x := Counter new increment\n"),
    ]);
    let plan = svc.plan_rename(&file, pos, "bump").expect("plan");
    assert_eq!(
        apply(&svc, "/p/Counter.bt", &plan.confirmed),
        "Object subclass: Counter\n  bump => 1\n  twice => self bump + self bump\n  other => self class increment\n"
    );
    assert_eq!(
        apply(&svc, "/p/Sub.bt", &plan.confirmed),
        "Counter subclass: Sub\n  more => super bump\n"
    );
    // The external send and the class-side `self class increment` are candidates.
    assert_eq!(
        apply(&svc, "/p/use.bt", &plan.confirmed),
        "x := Counter new increment\n"
    );
    assert_eq!(
        apply(&svc, "/p/use.bt", &candidate_edits(&plan)),
        "x := Counter new bump\n"
    );
    assert_eq!(plan.candidates.len(), 2);
}

#[test]
fn binary_selector_rename() {
    let (svc, file, pos) = setup(&[(
        "/p/Vec.bt",
        "Object subclass: Vec\n  @@+ other => other\n  double => self + self\n  twice: v => v + v\n",
    )]);
    let plan = svc.plan_rename(&file, pos, "plus").unwrap_err();
    assert!(plan.message.contains("binary selector"), "{}", plan.message);
    let plan = svc.plan_rename(&file, pos, "<+>").expect("plan");
    assert_eq!(
        apply(&svc, "/p/Vec.bt", &plan.confirmed),
        "Object subclass: Vec\n  <+> other => other\n  double => self <+> self\n  twice: v => v + v\n"
    );
    assert_eq!(
        apply(&svc, "/p/Vec.bt", &candidate_edits(&plan)),
        "Object subclass: Vec\n  + other => other\n  double => self + self\n  twice: v => v <+> v\n"
    );
}

#[test]
fn keyword_selector_rename_edits_token_spans_with_nested_sends() {
    let (svc, file, pos) = setup(&[
        (
            "/p/Box.bt",
            "Object subclass: Box\n  at: i pu@@t: v => v\n  fill => self at: (self at: 1 put: 2) put: (self at: 3 put: 4)\n  text => 'at: put:'\n",
        ),
        ("/p/use.bt", "d at: 1 put: (d at: 2 put: 3)\n"),
    ]);
    // Arity must match.
    assert!(svc.plan_rename(&file, pos, "store:").is_err());
    let plan = svc.plan_rename(&file, pos, "set:to:").expect("plan");
    assert_eq!(
        apply(&svc, "/p/Box.bt", &plan.confirmed),
        "Object subclass: Box\n  set: i to: v => v\n  fill => self set: (self set: 1 to: 2) to: (self set: 3 to: 4)\n  text => 'at: put:'\n"
    );
    assert_eq!(
        apply(&svc, "/p/use.bt", &candidate_edits(&plan)),
        "d set: 1 to: (d set: 2 to: 3)\n"
    );
}

#[test]
fn cascade_messages_are_found() {
    let (svc, file, pos) = setup(&[(
        "/p/a.bt",
        "Object subclass: Foo\n  g@@o => 1\n  run => self go; go\n  ext => x go; go\n",
    )]);
    let plan = svc.plan_rename(&file, pos, "stop").expect("plan");
    assert_eq!(
        apply(&svc, "/p/a.bt", &plan.confirmed),
        "Object subclass: Foo\n  stop => 1\n  run => self stop; stop\n  ext => x go; go\n"
    );
    assert_eq!(plan.candidates.len(), 2);
}

#[test]
fn override_in_subclass_confirms_no_self_or_super_site() {
    let (svc, file, pos) = setup(&[
        (
            "/p/Base.bt",
            "Object subclass: Base\n  gr@@eet => 1\n  hello => self greet\n",
        ),
        (
            "/p/Sub.bt",
            "Base subclass: Sub\n  greet => super greet\n  hi => self greet\n",
        ),
    ]);
    let plan = svc.plan_rename(&file, pos, "salute").expect("plan");
    // Only the definition is confirmed.
    assert_eq!(plan.confirmed.len(), 1);
    assert_eq!(
        apply(&svc, "/p/Base.bt", &plan.confirmed),
        "Object subclass: Base\n  salute => 1\n  hello => self greet\n"
    );
    // Every send, self/super included, moves to the candidates.
    assert_eq!(plan.candidates.len(), 3);
}

#[test]
fn self_send_in_superclass_is_a_candidate_and_unrelated_class_is_untouched() {
    let (svc, file, pos) = setup(&[
        ("/p/A.bt", "Object subclass: A\n  go => 1\n"),
        ("/p/B.bt", "A subclass: B\n  g@@o => 2\n  run => self go\n"),
        ("/p/T.bt", "Object subclass: T\n  tick => self go\n"),
    ]);
    // B>>go overrides A>>go; renaming B>>go: confirmed = B's definition and
    // `self go` in B (B has no overriding subclass). A's definition is not touched.
    let plan = svc.plan_rename(&file, pos, "walk").expect("plan");
    assert_eq!(
        apply(&svc, "/p/B.bt", &plan.confirmed),
        "A subclass: B\n  walk => 2\n  run => self walk\n"
    );
    assert_eq!(
        apply(&svc, "/p/A.bt", &plan.confirmed),
        "Object subclass: A\n  go => 1\n"
    );
    assert_eq!(
        apply(&svc, "/p/T.bt", &plan.confirmed),
        "Object subclass: T\n  tick => self go\n"
    );
    assert_eq!(plan.candidates.len(), 1);
}

#[test]
fn class_side_and_instance_side_are_distinct() {
    let (svc, file, pos) = setup(&[(
        "/p/a.bt",
        "Object subclass: Foo\n  class ma@@ke => self new\n  other => self make\n  class again => self make\n",
    )]);
    let plan = svc.plan_rename(&file, pos, "build").expect("plan");
    assert_eq!(
        apply(&svc, "/p/a.bt", &plan.confirmed),
        "Object subclass: Foo\n  class build => self new\n  other => self make\n  class again => self build\n"
    );
    assert_eq!(plan.candidates.len(), 1);
}

#[test]
fn selector_rename_refuses_collision_and_ambiguity_and_stdlib() {
    let (svc, file, pos) = setup(&[(
        "/p/a.bt",
        "Object subclass: Foo\n  g@@o => 1\n  stop => 2\n",
    )]);
    let err = svc.plan_rename(&file, pos, "stop").unwrap_err();
    assert!(err.message.contains("already defines"), "{}", err.message);

    // A send whose selector several classes define is ambiguous.
    let (svc, file, pos) = setup(&[
        ("/p/a.bt", "Object subclass: A\n  go => 1\n"),
        ("/p/b.bt", "Object subclass: B\n  go => 1\n"),
        ("/p/use.bt", "x g@@o\n"),
    ]);
    let err = svc.prepare_rename(&file, pos).unwrap_err();
    assert!(err.message.contains("several classes"), "{}", err.message);

    // A selector defined only in the stdlib is refused.
    let (mut svc, file, pos) = setup(&[
        ("/std/S.bt", "Object subclass: S\n  size => 0\n"),
        ("/p/use.bt", "x si@@ze\n"),
    ]);
    svc.mark_stdlib_file(Utf8PathBuf::from("/std/S.bt"));
    let err = svc.prepare_rename(&file, pos).unwrap_err();
    assert!(err.message.contains("standard library"), "{}", err.message);
}

#[test]
fn send_cursor_resolves_the_unique_defining_class() {
    let (svc, file, pos) = setup(&[
        ("/p/a.bt", "Object subclass: A\n  go => 1\n"),
        ("/p/use.bt", "x g@@o\n"),
    ]);
    let plan = svc.plan_rename(&file, pos, "walk").expect("plan");
    assert_eq!(
        apply(&svc, "/p/a.bt", &plan.confirmed),
        "Object subclass: A\n  walk => 1\n"
    );
    assert_eq!(
        apply(&svc, "/p/use.bt", &candidate_edits(&plan)),
        "x walk\n"
    );
}
