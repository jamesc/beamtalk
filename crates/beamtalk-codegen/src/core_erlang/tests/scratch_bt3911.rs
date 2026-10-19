use super::codegen;

fn show(src: &str, name: &str) {
    let code = codegen(src);
    let start = code.find(name).unwrap_or(0);
    let tail = &code[start..];
    let end = tail.find("'register_class'/0 = ").unwrap_or(tail.len());
    println!("{}", &tail[..end]);
    // erlc with warnings
    let dir = tempfile::tempdir().unwrap();
    let f = dir.path().join("test.core");
    std::fs::write(&f, &code).unwrap();
    let out = std::process::Command::new("erlc")
        .arg("+from_core")
        .arg("+warnings_as_errors")
        .arg("-o")
        .arg(dir.path())
        .arg(&f)
        .output()
        .unwrap();
    println!(
        "ERLC {} {}",
        out.status,
        String::from_utf8_lossy(&out.stdout)
    );
    println!("{}", String::from_utf8_lossy(&out.stderr));
}

#[test]
fn scratch_do_body() {
    show(
        "Actor subclass: Repro\n  state: n = 0\n  f: m :: Integer =>\n    i := 0\n    [#(1) do: [:e | e > 0 ifTrue: [x := e. x]]. i < m] whileTrue: [i := i + 1]\n    i\n",
        "'f:'/2 = ",
    );
}

#[test]
fn scratch_closure() {
    show(
        "Actor subclass: Repro\n  state: n = 0\n  f: m :: Integer =>\n    i := 0\n    [[:z | z > 0 ifTrue: [x := z. x]] value: 1. i < m] whileTrue: [i := i + 1]\n    i\n",
        "'f:'/2 = ",
    );
}

#[test]
fn scratch_bound_before() {
    show(
        "Actor subclass: Repro\n  state: n = 0\n  f: m :: Integer =>\n    i := 0\n    [x := 0. m > 0 ifTrue: [x := 1]. i < (m + x) - 1] whileTrue: [i := i + 1]\n    i\n",
        "'f:'/2 = ",
    );
}

#[test]
fn scratch_closure_in_body() {
    show(
        "Actor subclass: Repro\n  state: n = 0\n  f: m :: Integer =>\n    i := 0\n    [i < m] whileTrue: [[:z | z > 0 ifTrue: [x := z. x]] value: 1. i := i + 1]\n    i\n",
        "'f:'/2 = ",
    );
}
