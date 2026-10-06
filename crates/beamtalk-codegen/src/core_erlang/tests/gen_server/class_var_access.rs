// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0130 class-variable codegen: reads and writes are in-place accesses
//! (inlined `erlang:get/1` + `maps:*`, `beamtalk_class_vars` on a miss), class
//! methods are `class_<sel>(ClassSelf, Args...)`, and nothing about class
//! variables is threaded, returned or rebound anywhere.

use super::*;

#[test]
fn test_bt1213_block_value_with_captured_mutation_actor() {
    // [count := count + 1] value in actor context
    // Parse from source to get a realistic AST
    // Build AST manually: Object subclass: BT1213Actor
    //   testIt => count := 0. [count := count + 1] value. count
    let s = Span::new(0, 0);
    let count_id = || Expression::Identifier(Identifier::new("count", s));

    // count := count + 1
    let add_expr = Expression::MessageSend {
        receiver: Box::new(count_id()),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![Expression::Literal(Literal::Integer(1), s)],
        is_cast: false,
        span: s,
    };
    let assign = Expression::Assignment {
        target: Box::new(count_id()),
        value: Box::new(add_expr),
        type_annotation: None,
        span: s,
    };

    // [count := count + 1] value
    let block = Block::new(vec![], vec![bare(assign)], s);
    let block_value = Expression::MessageSend {
        receiver: Box::new(Expression::Block(block)),
        selector: MessageSelector::Unary("value".into()),
        arguments: vec![],
        is_cast: false,
        span: s,
    };

    // count := 0
    let init_count = Expression::Assignment {
        target: Box::new(count_id()),
        value: Box::new(Expression::Literal(Literal::Integer(0), s)),
        type_annotation: None,
        span: s,
    };

    let method = MethodDefinition::new(
        MessageSelector::Unary("testIt".into()),
        vec![],
        vec![bare(init_count), bare(block_value), bare(count_id())],
        s,
    );

    let class = ClassDefinition {
        name: Identifier::new("BT1213Actor", s),
        superclass: Some(Identifier::new("Actor", s)),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![method],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        uses: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        shape_version: None,
        span: Span::new(0, 0),
    };

    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code = generate_module(&module, CodegenOptions::new("bt@bt1213_actor"))
        .expect("codegen should work");

    // Actor codegen should thread count through StateAcc
    assert!(
        code.contains("__local__count"),
        "Should thread count through StateAcc. Got:\n{code}"
    );
}

/// Compiles `src` and returns the Core Erlang text of the function whose
/// header starts with `header`, up to the next top-level definition.
pub(super) fn function_text<'a>(code: &'a str, header: &str) -> &'a str {
    let start = code
        .find(header)
        .unwrap_or_else(|| panic!("{header} not found in:\n{code}"));
    let rest = &code[start..];
    let end = rest[1..].find("\n'").map_or(rest.len(), |e| e + 1);
    &rest[..end]
}

/// ADR 0130 §3: a class method is `fun (ClassSelf, Args...)` returning the bare
/// result, whatever it does with class variables.
#[test]
fn class_methods_take_class_self_and_args_only() {
    let src = concat!(
        "Object subclass: Counter\n",
        "  classState: n = 0\n\n",
        "  class bump => self.n := self.n + 1\n\n",
        "  class add: x to: y => x + y\n",
    );
    let code = codegen(src);
    assert!(
        code.contains("'class_bump'/1 = fun (ClassSelf) ->"),
        "unary class method is arity 1. Got:\n{code}"
    );
    assert!(
        code.contains("'class_add:to:'/3 = fun (ClassSelf, _x"),
        "keyword class method is ClassSelf plus its parameters. Got:\n{code}"
    );
    assert!(
        !code.contains("class_var_result") && !code.contains("ClassVars"),
        "no `{{class_var_result, ..}}` and no ClassVars parameter anywhere. Got:\n{code}"
    );
}

/// ADR 0130 §2: `self.n` is read in place; the key is derived from `ClassSelf`'s
/// tag with one `element/2` (never a tag-to-name derivation per access) and the
/// one runtime owner answers every miss.
#[test]
fn class_var_read_is_inlined_with_a_helper_fallback() {
    let src = "Object subclass: Counter\n  classState: n = 0\n\n  class peek => self.n\n";
    let code = codegen(src);
    let peek = function_text(&code, "'class_peek'/1 = fun");
    assert!(
        peek.contains(
            "call 'erlang':'get'({'$bt_class_vars', call 'erlang':'element'(2, ClassSelf)})"
        ),
        "the key shape comes from the class_var_keys leaf. Got:\n{peek}"
    );
    assert!(
        peek.contains("call 'maps':'find'('n', ")
            && peek.contains("call 'beamtalk_class_vars':'get'(ClassSelf, 'n')"),
        "hit path inlined, miss path is the runtime helper. Got:\n{peek}"
    );
    assert!(
        !peek.contains("class_name_from_tag"),
        "the hot path never derives the class name. Got:\n{peek}"
    );
}

/// A class-variable write inside a loop, a conditional, an `on:do:` body and a
/// block passed to another class is just a `put`: nothing is threaded.
#[test]
fn class_var_writes_anywhere_thread_nothing() {
    let src = concat!(
        "Object subclass: Counter\n",
        "  classState: n = 0\n\n",
        "  class inLoop: k =>\n",
        "    i := 0\n",
        "    [i < k] whileTrue: [\n",
        "      self.n := self.n + 1\n",
        "      i := i + 1\n",
        "    ]\n",
        "    self.n\n\n",
        "  class inTimes: k =>\n",
        "    k timesRepeat: [self.n := self.n + 1]\n",
        "    self.n\n\n",
        "  class inDo: items =>\n",
        "    total := 0\n",
        "    items do: [:x |\n",
        "      self.n := self.n + x\n",
        "      total := total + x\n",
        "    ]\n",
        "    total\n\n",
        "  class inArm: flag =>\n",
        "    flag ifTrue: [self.n := 1] ifFalse: [self.n := 2]\n",
        "    self.n\n\n",
        "  class inHandler =>\n",
        "    [1 / 0] on: Error do: [:e | self.n := 99]\n",
        "    self.n\n\n",
        "  class inForeignBlock: aBlock =>\n",
        "    aBlock value\n\n",
        "  class passesWriter => Counter inForeignBlock: [self.n := 5]\n",
    );
    let code = codegen(src);
    assert!(
        !code.contains("ClassVars") && !code.contains("class_var_result"),
        "no loop, arm, handler or block threads class variables. Got:\n{code}"
    );
    for (method, arity) in [
        ("inLoop:", 2),
        ("inTimes:", 2),
        ("inDo:", 2),
        ("inArm:", 2),
        ("inHandler", 1),
        ("passesWriter", 1),
    ] {
        let header = format!("'class_{method}'/{arity} = fun");
        let text = function_text(&code, &header);
        assert!(
            text.contains("call 'erlang':'put'({'$bt_class_vars', "),
            "{method} writes in place. Got:\n{text}"
        );
    }
    assert_compiles_through_erlc("test", &code);
}

/// A class-side self-send of every kind passes nothing and rebinds nothing;
/// the guarded `case` of an open class keeps only the call.
#[test]
fn class_side_self_sends_pass_and_rebind_nothing() {
    let src = concat!(
        "Object subclass: Base\n",
        "  classState: n = 0\n\n",
        "  class foo => self.n\n\n",
        "  class sealed pinned => 1\n\n",
        "  class open => self foo\n\n",
        "  class direct => self pinned\n\n",
        "  class named => Base foo\n\n",
        "  class inherited => self species\n",
    );
    let code = codegen(src);
    let open = function_text(&code, "'class_open'/1 = fun");
    assert!(
        open.contains("class_self_direct_ok")
            && open.contains("<'true'> when 'true' -> call 'test':'class_foo'(ClassSelf)")
            && open
                .contains("<_> when 'true' -> call 'beamtalk_class_dispatch':'class_self_send'(")
            && open.contains("'foo', [])"),
        "an open-class self-send is a guarded direct call with the walk as the fallback. Got:\n{open}"
    );
    let direct = function_text(&code, "'class_direct'/1 = fun");
    assert!(
        direct.contains("call 'test':'class_pinned'(ClassSelf)"),
        "a `class sealed` selector keeps the direct call. Got:\n{direct}"
    );
    let named = function_text(&code, "'class_named'/1 = fun");
    assert!(
        named.contains("call 'test':'class_foo'(ClassSelf)"),
        "an own-class reference binds statically. Got:\n{named}"
    );
    let inherited = function_text(&code, "'class_inherited'/1 = fun");
    assert!(
        inherited.contains("'class_self_send'(") && inherited.contains("'species', [])"),
        "an inherited selector walks the hierarchy with no ClassVars argument. Got:\n{inherited}"
    );
    assert!(
        !code.contains("ClassVars") && !code.contains("class_var_result"),
        "no self-send passes or unwraps class variables. Got:\n{code}"
    );
}

/// ADR 0130 §3: evaluation order of effectful class-side arguments is pinned
/// left to right: each is bound in its own ordered `let` before the call.
#[test]
fn effectful_class_side_arguments_are_evaluated_in_source_order() {
    let src = concat!(
        "Object subclass: Counter\n",
        "  classState: n = 0\n\n",
        "  class bump => self.n := self.n + 1\n\n",
        "  class pair: a with: b => #(a, b)\n\n",
        "  class go => self pair: self bump with: self bump\n",
    );
    let code = codegen(src);
    let go = function_text(&code, "'class_go'/1 = fun");
    let first = go.find("let _Effect").expect("first effect bound");
    let second = go[first + 1..]
        .find("let _Effect")
        .expect("second effect bound");
    let call = go.find("'class_pair:with:'").expect("pair:with: called");
    assert!(
        first < first + 1 + second && first + 1 + second < call,
        "both effectful arguments are bound, in order, before the call. Got:\n{go}"
    );
}

/// ADR 0130 §3: a `^` unwinding out of a class method hands back only the value
/// (the catch arm yields it bare); the thrown state slot is `nil`.
#[test]
fn class_method_nlr_yields_the_bare_value() {
    let src = concat!(
        "Object subclass: Finder\n",
        "  classState: seen = 0\n\n",
        "  class first: items =>\n",
        "    items do: [:x | x > 1 ifTrue: [^x]]\n",
        "    self.seen\n",
    );
    let code = codegen(src);
    assert!(
        code.contains("call 'erlang':'throw'({'$bt_nlr', ") && code.contains(", 'nil'})"),
        "the class-method NLR throw carries no state. Got:\n{code}"
    );
    assert!(
        !code.contains("{'class_var_result', "),
        "the NLR catch arm yields the bare value. Got:\n{code}"
    );
    assert_compiles_through_erlc("test", &code);
}

/// ADR 0130 §3: `__beamtalk_meta/0` declares the class-variable ABI the module
/// was compiled for; the runtime's load gate reads it.
#[test]
fn meta_declares_class_var_abi() {
    let code = codegen("Object subclass: Counter\n  classState: n = 0\n");
    assert!(
        code.contains("'class_var_abi' => 1"),
        "__beamtalk_meta/0 must carry class_var_abi => 1. Got:\n{code}"
    );
}

/// Instance-side code is untouched: an actor field write threads `State` and
/// never touches the class-variable key.
#[test]
fn instance_field_mutation_never_touches_the_class_var_key() {
    let src = "Actor subclass: PlainCounter\n  state: count = 0\n\n  bump => self.count := self.count + 1";
    let code = codegen(src);
    assert!(
        !code.contains("$bt_class_vars"),
        "instance field mutation must not emit a class-var access. Got:\n{code}"
    );
}
