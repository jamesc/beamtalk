// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4242: a match arm's shared fallthrough label (BT-4141) is a
//! `letrec_goto` label, so `erlc` turns each miss into a jump instead of a
//! local call.
//!
//! `beam_core_to_ssa` (OTP 27+) branches to the label from every `apply` of
//! it without checking where the `apply` sits, so the annotation is only
//! sound when every apply is in tail position of the `letrec` body (see
//! `patterns::match_lowering::SharedRest`). These tests check that rule over
//! the emitted Core with an Erlang `cerl` walk (`HARNESS`), pin the OTP
//! behaviour the rule rests on with hand-written Core, and run the
//! generated matches, so a miss that jumped to the wrong place shows up as a
//! wrong answer rather than only as Core that compiles.

use std::fmt::Write as _;
use std::path::Path;
use std::process::Command;

use super::control_flow::family_detector_differential::{collect_source_files, repo_root};
use super::{CodegenOptions, codegen, erlc_available, generate_module};

/// Erlang side of the tests: `check/1` counts a Core module's `letrec_goto`
/// labels and lists every apply of one that is not in tail position of its
/// `letrec` body; `run/0` does that for each Core file of the job in
/// `job.term`, then calls each probe and prints its result.
///
/// The tail positions are the ones `beam_core_to_ssa` compiles a goto from
/// correctly: clause bodies of a tail `case` (never its argument or a
/// guard), the body of a tail `let`/`do` (never the argument), the `of`
/// body and the handler of a tail `try` (never the protected argument), and
/// the body and goto-label definitions of a nested `letrec`. A plain
/// `letrec` definition and any `fun` are separate functions.
const HARNESS: &str = r#"-module(goto_harness).
-export([run/0, check/1]).

run() ->
    {ok, [CoreFiles, Mod, Probes]} = file:consult("job.term"),
    Labels = lists:sum([check_file(F) || F <- CoreFiles]),
    io:format("labels ~w~n", [Labels]),
    lists:foreach(fun({Fun, Args}) ->
        Parent = self(),
        Pid = spawn(fun() -> Parent ! {self(), (catch apply(Mod, Fun, Args))} end),
        R = receive {Pid, V} -> V after 5000 -> exit(Pid, kill), timeout end,
        io:format("probe ~w~n", [R])
    end, Probes),
    halt().

check_file(File) ->
    {N, Bad} = check(File),
    [io:format("bad ~s ~w~n", [File, L]) || L <- Bad],
    N.

check(File) ->
    {ok, Bin} = file:read_file(File),
    {ok, Toks, _} = core_scan:string(binary_to_list(Bin)),
    {ok, Mod} = core_parse:parse(Toks),
    cerl_trees:fold(fun(T, {N, Bad}) ->
        case is_goto(T) of
            true ->
                [{Var, Fun}] = cerl:letrec_defs(T),
                L = cerl:var_name(Var),
                {N + 1, Bad ++ tail(cerl:letrec_body(T), L) ++ tail(cerl:fun_body(Fun), L)};
            false -> {N, Bad}
        end
    end, {0, []}, Mod).

is_goto(T) ->
    cerl:type(T) =:= letrec andalso lists:member(letrec_goto, cerl:get_ann(T)).

tail(E, L) ->
    case cerl:type(E) of
        apply ->
            case is_label(cerl:apply_op(E), L) of
                true -> lists:flatmap(fun(A) -> nontail(A, L) end, cerl:apply_args(E));
                false -> nontail(E, L)
            end;
        'case' ->
            nontail(cerl:case_arg(E), L) ++
                lists:flatmap(fun(C) ->
                    nontail(cerl:clause_guard(C), L) ++ tail(cerl:clause_body(C), L)
                end, cerl:case_clauses(E));
        'let' -> nontail(cerl:let_arg(E), L) ++ tail(cerl:let_body(E), L);
        seq -> nontail(cerl:seq_arg(E), L) ++ tail(cerl:seq_body(E), L);
        'try' ->
            nontail(cerl:try_arg(E), L) ++ tail(cerl:try_body(E), L) ++
                tail(cerl:try_handler(E), L);
        letrec ->
            Goto = is_goto(E),
            lists:flatmap(fun({_, F}) when Goto -> tail(cerl:fun_body(F), L);
                             ({_, F}) -> nontail(F, L)
                          end, cerl:letrec_defs(E)) ++ tail(cerl:letrec_body(E), L);
        _ -> nontail(E, L)
    end.

nontail(E, L) ->
    cerl_trees:fold(fun(T, Acc) ->
        case cerl:type(T) =:= apply andalso is_label(cerl:apply_op(T), L) of
            true -> [L | Acc];
            false -> Acc
        end
    end, [], E).

is_label(Op, L) -> cerl:is_c_var(Op) andalso cerl:var_name(Op) =:= L.
"#;

/// What [`run_job`] observed.
struct HarnessRun {
    /// `letrec_goto` labels across the job's Core files.
    labels: usize,
    /// `<file> <label>` for each apply of a label outside tail position.
    bad: Vec<String>,
    /// Each probe's result, as `~w` prints it.
    results: Vec<String>,
}

/// A temp directory holding the compiled harness; `None` without an Erlang
/// toolchain.
fn harness_dir() -> Option<tempfile::TempDir> {
    if !erlc_available() || Command::new("erl").arg("-version").output().is_err() {
        return None;
    }
    let dir = tempfile::tempdir().expect("temp dir");
    let harness = dir.path().join("goto_harness.erl");
    std::fs::write(&harness, HARNESS).expect("write harness");
    erlc(dir.path(), &[], &harness, HARNESS);
    Some(dir)
}

/// Checks the goto labels of every Core file in `dir` named by
/// `core_files`, then calls each probe on `module`: `(function, Erlang
/// argument list)`.
fn run_job(
    dir: &Path,
    core_files: &[String],
    module: &str,
    probes: &[(&str, String)],
) -> HarnessRun {
    let quoted: Vec<String> = core_files.iter().map(|f| format!("\"{f}\"")).collect();
    let mut job = format!("[{}].\n'{module}'.\n[", quoted.join(", "));
    for (i, (fun, args)) in probes.iter().enumerate() {
        let sep = if i == 0 { "" } else { ", " };
        write!(job, "{sep}{{'{fun}', {args}}}").expect("write to String");
    }
    job.push_str("].\n");
    std::fs::write(dir.join("job.term"), job).expect("write job");

    let out = Command::new("erl")
        .current_dir(dir)
        .args(["-noshell", "-pa", ".", "-s", "goto_harness", "run"])
        .output()
        .expect("erl runs");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let lines = |key: &str| -> Vec<String> {
        stdout
            .lines()
            .filter_map(|l| l.strip_prefix(key))
            .map(str::to_string)
            .collect()
    };
    let labels = lines("labels ");
    assert_eq!(
        labels.len(),
        1,
        "harness failed:\n{stdout}\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    HarnessRun {
        labels: labels[0].parse().expect("label count"),
        bad: lines("bad "),
        results: lines("probe "),
    }
}

/// Compiles `core` (Core for module `module`), checks its goto labels and
/// calls each probe. `None` without an Erlang toolchain.
fn run_harness(module: &str, core: &str, probes: &[(&str, String)]) -> Option<HarnessRun> {
    let dir = harness_dir()?;
    // The probes need no Beamtalk runtime; the `on_load` hook would
    // register the class with it, so the module loads bare.
    let core = core.replace("'on_load' = [{'register_class', 0}],", "");
    let file = format!("{module}.core");
    let core_file = dir.path().join(&file);
    std::fs::write(&core_file, &core).expect("write core");
    // `+clint`: the runtime compiles generated Core with `core_lint` on.
    erlc(dir.path(), &["+from_core", "+clint"], &core_file, &core);
    Some(run_job(dir.path(), &[file], module, probes))
}

fn erlc(dir: &Path, opts: &[&str], file: &Path, source: &str) {
    let out = Command::new("erlc")
        .args(opts)
        .arg("-o")
        .arg(dir)
        .arg(file)
        .output()
        .expect("erlc runs");
    assert!(
        out.status.success(),
        "erlc rejected {}:\n{}{}\n{source}",
        file.display(),
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
}

/// Asserts that a generated module (`test`) has goto fallthrough labels and
/// applies each one only in tail position.
pub(super) fn assert_goto_labels_in_tail_position(core: &str) {
    let Some(run) = run_harness("test", core, &[]) else {
        return;
    };
    assert!(run.labels > 0, "expected a goto fallthrough label:\n{core}");
    assert!(
        run.bad.is_empty(),
        "goto label applied outside tail position: {:?}\n{core}",
        run.bad
    );
}

/// Checks every goto label in a generated module, then calls `run:` with
/// each argument and compares the answers.
fn assert_goto_match(src: &str, cases: &[(&str, &str)]) {
    let core = codegen(src);
    assert!(
        core.contains("-| ['letrec_goto', 'no_inline'] )"),
        "expected a goto fallthrough label:\n{core}"
    );
    let probes: Vec<(&str, String)> = ["run:", "run2:"]
        .iter()
        .flat_map(|m| {
            cases
                .iter()
                .map(move |(arg, _)| (*m, format!("[#{{}}, {arg}]")))
        })
        .collect();
    let Some(run) = run_harness("test", &core, &probes) else {
        return;
    };
    assert!(run.labels > 0, "no goto label parsed:\n{core}");
    assert!(
        run.bad.is_empty(),
        "goto label applied outside tail position: {:?}\n{core}",
        run.bad
    );
    let expected: Vec<String> = cases
        .iter()
        .map(|(_, want)| (*want).to_string())
        .chain(cases.iter().map(|(_, want)| format!("{{{want},0}}")))
        .collect();
    assert_eq!(run.results, expected, "Source:\n{src}\nCore:\n{core}");
}

/// Wraps `arms` as the tail of `run:` and, in `run2:`, as a value the
/// method goes on to use (the label is then a non-tail `letrec`, which is
/// fine: only the applies inside it must be tails).
fn methods(arms: &[&str]) -> String {
    let arms = arms.join(";\n      ");
    format!(
        "Object subclass: Foo\n  run: x =>\n    x match: [\n      {arms}\n    ]\n\n  run2: x =>\n    t := x match: [\n      {arms}\n    ]\n    {{t, 0}}\n"
    )
}

/// Hand-written Core pinning what `erlc` does with a `letrec_goto` apply in
/// each kind of position, on the OTP running the tests (the CI matrix
/// covers every supported major). The `nontail` answer is the miscompile
/// the tail-position rule exists for: the label's value becomes the whole
/// `letrec`'s, skipping the `{'wrapped', V}` around it, with no error.
#[test]
fn letrec_goto_semantics_by_apply_position() {
    let core = "module 'lgpin' ['tail'/1, 'handler'/1, 'nontail'/1, 'boom'/1]
  attributes []
'tail'/1 = fun (X) ->
  let <R> = ( letrec 'L'/0 = fun () -> 'miss' in
     case call 'erlang':'is_integer'(X) of
       <'true'> when 'true' ->
          case call 'erlang':'>'(X, 5) of
            <'true'> when 'true' -> 'big'
            <_A> when 'true' -> apply 'L'/0 ()
          end
       <_B> when 'true' -> apply 'L'/0 ()
     end -| ['letrec_goto', 'no_inline'] )
  in {'wrapped', R}
'handler'/1 = fun (X) ->
  let <R> = ( letrec 'L'/0 = fun () -> 'miss' in
     try call 'lgpin':'boom'(X) of <E> ->
        case E of
          <'a'> when 'true' -> 'hit'
          <_C> when 'true' -> apply 'L'/0 ()
        end
     catch <T, E2, S> -> apply 'L'/0 ()
     -| ['letrec_goto', 'no_inline'] )
  in {'wrapped', R}
'nontail'/1 = fun (X) ->
  ( letrec 'L'/0 = fun () -> 'miss' in
     let <V> = case X of
                 <1> when 'true' -> 'one'
                 <_D> when 'true' -> apply 'L'/0 ()
               end
     in {'wrapped', V}
   -| ['letrec_goto', 'no_inline'] )
'boom'/1 = fun (X) -> call 'erlang':'element'(1, X)
end
";
    let probes: Vec<(&str, String)> = [
        ("tail", "7"),
        ("tail", "2"),
        ("tail", "x"),
        ("handler", "{a}"),
        ("handler", "{b}"),
        ("handler", "nope"),
        ("nontail", "1"),
        ("nontail", "2"),
    ]
    .iter()
    .map(|(f, a)| (*f, format!("[{a}]")))
    .collect();
    let Some(run) = run_harness("lgpin", core, &probes) else {
        return;
    };
    assert_eq!(run.labels, 3);
    // The checker flags exactly the non-tail apply.
    assert_eq!(run.bad, ["lgpin.core {'L',0}"]);
    assert_eq!(
        run.results,
        [
            "{wrapped,big}",
            "{wrapped,miss}",
            "{wrapped,miss}",
            // A goto from a `try` handler is sound: the handler runs after
            // the protected region has ended.
            "{wrapped,hit}",
            "{wrapped,miss}",
            "{wrapped,miss}",
            "{wrapped,one}",
            // Miscompiled: a plain `letrec` answers `{wrapped,miss}`.
            "miss",
        ]
    );
}

/// The issue's measurement case: a 10-arm Type chain, every shape of
/// type test, each value hitting its own arm after missing all earlier
/// ones.
#[test]
fn type_chain_gotos_answer_every_arm() {
    let src = methods(&[
        "v :: Binary -> 1",
        "v :: Boolean -> 2",
        "v :: String -> 3",
        "v :: List -> 4",
        "v :: Float -> 5",
        "v :: Symbol -> 6",
        "v :: Tuple -> 7",
        "v :: Dictionary -> 8",
        "v :: Integer when: [v > 5] -> 9",
        "v :: Integer -> 10",
        "_ -> 11",
    ]);
    assert_goto_match(
        &src,
        &[
            ("<<1>>", "1"),
            ("true", "2"),
            ("{beamtalk_string, <<\"s\">>}", "1"),
            ("[1]", "4"),
            ("1.5", "5"),
            ("abc", "6"),
            ("{1, 2}", "7"),
            ("#{a => 1}", "8"),
            ("42", "9"),
            ("3", "10"),
            ("nil", "11"),
            ("{beamtalk_object, 'Foo', foo, 1, 2}", "7"),
        ],
    );
}

/// A segmented binary arm whose size raises: the `try` handler jumps to
/// the later arms. Array arms and a nested binary arm miss their way down
/// too (an Array is never passed: matching one needs the runtime).
#[test]
fn binary_and_array_gotos_answer_every_arm() {
    let src = methods(&[
        "<<a:(4 / q), rest/binary>> -> 1",
        "<<k:8, a:k/binary, rest/binary>> -> 2",
        "#[p, q2] -> 3",
        "{#ok, <<1, r/binary>>} -> 4",
        "{#ok, other} -> 5",
        "_ -> 6",
    ])
    .replace("    x match:", "    q := 0\n    x match:")
    .replace("    t := x match:", "    q := 0\n    t := x match:");
    assert_goto_match(
        &src,
        &[
            ("<<2, 7, 8, 9>>", "2"),
            ("<<9>>", "6"),
            ("{ok, <<1, 2>>}", "4"),
            ("{ok, <<2>>}", "5"),
            ("#{'$beamtalk_class' => 'Other'}", "6"),
            ("42", "6"),
        ],
    );
}

/// Every goto label the stdlib, its tests and the examples compile to is
/// applied only in tail position: the rule checked over every real
/// `match:`, not just the shapes above.
#[test]
fn corpus_goto_labels_are_applied_in_tail_position() {
    let Some(dir) = harness_dir() else {
        return;
    };
    let root = repo_root();
    let mut sources = Vec::new();
    for corpus_dir in [
        "stdlib/src",
        "stdlib/test",
        "stdlib/bootstrap-test",
        "examples",
    ] {
        collect_source_files(&root.join(corpus_dir), &mut sources);
    }
    let mut core_files = Vec::new();
    for (i, path) in sources.iter().enumerate() {
        let Ok(source) = std::fs::read_to_string(path) else {
            continue;
        };
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let (module, _) = beamtalk_core::source_analysis::parse(tokens);
        let stdlib = path.starts_with(root.join("stdlib/src"));
        let options = CodegenOptions::new(&format!("goto_corpus_{i}")).with_stdlib_mode(stdlib);
        // A fixture meant to fail codegen has no Core to check.
        let Ok(core) = generate_module(&module, options) else {
            continue;
        };
        if core.contains("'letrec_goto'") {
            let file = format!("goto_corpus_{i}.core");
            std::fs::write(dir.path().join(&file), core).expect("write core");
            core_files.push(file);
        }
    }
    let run = run_job(dir.path(), &core_files, "none", &[]);
    assert!(
        run.labels > 20,
        "expected the corpus to compile to well over 20 goto labels, saw {} in {} files",
        run.labels,
        core_files.len()
    );
    assert!(
        run.bad.is_empty(),
        "goto label applied outside tail position: {:?}",
        run.bad
    );
}
