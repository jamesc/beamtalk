use super::codegen;

fn show(src: &str, name: &str) {
    let code = codegen(src);
    let start = code.find(name).unwrap_or(0);
    let tail = &code[start..];
    let end = tail.find("'superclass'/0 = ").unwrap_or(tail.len());
    println!("{}", &tail[..end]);
    std::fs::write("/private/tmp/claude-501/-Users-james-source-beamtalk/ac3631a5-5cf5-4bac-bf55-bd15a5779328/scratchpad/last.core", &code).unwrap();
    let dir = tempfile::tempdir().unwrap();
    let f = dir.path().join("test.core");
    std::fs::write(&f, &code).unwrap();
    let out = std::process::Command::new("erlc")
        .arg("+from_core")
        .arg("-o")
        .arg(dir.path())
        .arg(&f)
        .output()
        .unwrap();
    println!("ERLC {}", out.status);
    println!("{}", String::from_utf8_lossy(&out.stderr));
}

#[test]
fn scratch_cascade_iter() {
    show(
        "Object subclass: Repro\n  classState: n = 0\n  class f: c =>\n    t := 0\n    #(1, 2) do: [:z | #(0) size; includes: (#(1, 2) do: [:e | t := t + e]); yourself]\n    t\n",
        "'class_f:'/2 = ",
    );
}

#[test]
fn scratch_prot_dinh_outer() {
    show(
        "Actor subclass: Repro\n  state: n = 0\n  f: c =>\n    t := 0\n    [#(1, 2) detect: [:e | e > 5] ifNone: [t := t + 1]. 7] on: Error do: [:e | 0]\n    t\n",
        "let T = 0",
    );
}

#[test]
fn scratch_prot_dins_local() {
    show(
        "Actor subclass: Repro\n  state: n = 0\n  f: c =>\n    t := 0\n    [u := 0. #(1, 2) detect: [:e | u := u + 1. e > 5] ifNone: [nil]. t := u. 7] on: Error do: [:e | 0]\n    t\n",
        "let T = 0",
    );
}
