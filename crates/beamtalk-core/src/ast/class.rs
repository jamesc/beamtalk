// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Class, protocol, and state declaration AST nodes for Beamtalk.
//!
//! This module contains [`ClassDefinition`], [`ProtocolDefinition`],
//! [`StandaloneMethodDefinition`], and their supporting types.

use crate::source_analysis::Span;

use super::CommentAttachment;
use super::expression::{Expression, Identifier, MessageSelector, TypeAnnotation, TypeParamDecl};
use super::method::{MethodDefinition, ParameterDefinition};

/// The kind of supervisor based on ancestry (BT-1218, ADR 0059 Phase 1).
///
/// Set by semantic analysis when a class is found to inherit from
/// `Supervisor` or `DynamicSupervisor` (which are themselves `Object`
/// subclasses, not `Actor` subclasses). `None` for all other classes —
/// including non-supervisor `Actor` subclasses and non-supervisor classes
/// that do not descend from `Supervisor` or `DynamicSupervisor`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SupervisorKind {
    /// Inherits from `Supervisor` (static child list).
    Static,
    /// Inherits from `DynamicSupervisor` (children added at runtime).
    Dynamic,
}

/// The kind of class based on its declaration form (ADR 0042).
///
/// Determined at parse time from the superclass used in the `subclass:` declaration.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum ClassKind {
    /// An ordinary class (`Object subclass:` or any non-Actor, non-Value superclass).
    #[default]
    Object,
    /// An actor class (`Actor subclass:`). Uses `gen_server` semantics.
    Actor,
    /// A value class (`Value subclass:`). Immutable value-object semantics (ADR 0042).
    Value,
}

impl ClassKind {
    /// Derive the class kind from the direct superclass name used in a `subclass:` declaration.
    ///
    /// - `"Actor"` → [`ClassKind::Actor`]
    /// - `"Value"` → [`ClassKind::Value`]
    /// - anything else → [`ClassKind::Object`]
    #[must_use]
    pub fn from_superclass_name(name: &str) -> Self {
        match name {
            "Actor" => ClassKind::Actor,
            "Value" => ClassKind::Value,
            _ => ClassKind::Object,
        }
    }

    /// Returns the string representation used in `.app` metadata (ADR 0070 Phase 4).
    ///
    /// - `Object` → `"object"`
    /// - `Actor` → `"actor"`
    /// - `Value` → `"value"`
    #[must_use]
    pub fn as_str(&self) -> &'static str {
        match self {
            ClassKind::Object => "object",
            ClassKind::Actor => "actor",
            ClassKind::Value => "value",
        }
    }
}

/// A class definition (Aggregate Root).
///
/// Example:
/// ```text
/// Actor subclass: Counter
///   state: value = 0
///
///   increment => self.value += 1
///   getValue => ^self.value
/// ```
///
/// # DDD: Aggregate Root for Class Compilation
///
/// `ClassDefinition` is an aggregate root that encapsulates all class-related
/// entities: state declarations and method definitions. It enforces class-level
/// invariants and provides the boundary for class-specific code generation.
///
/// # Invariants
///
/// The following invariants should hold for a valid class:
///
/// 1. **Unique state names**: No two state declarations may have the same name
/// 2. **Unique method selectors**: No two methods may have the same selector
///    (within the same method kind)
/// 3. **Valid superclass**: Superclass must be a valid identifier (existence
///    checked during semantic analysis)
/// 4. **Span containment**: All child spans must be within the class span
/// 5. **Arity consistency**: Method parameter count must match selector arity
#[derive(Debug, Clone, PartialEq)]
#[allow(clippy::struct_excessive_bools)] // Class modifiers are genuinely independent flags (ADR 0071)
pub struct ClassDefinition {
    /// The class name (e.g., `Counter`).
    pub name: Identifier,
    /// The superclass name (e.g., `Actor`). `None` for root classes (`ProtoObject`).
    pub superclass: Option<Identifier>,
    /// Optional package qualifier on the superclass (ADR 0070).
    /// E.g., `json` in `json@Parser subclass: LenientParser`.
    pub superclass_package: Option<Identifier>,
    /// The class kind derived from the declaration form (Actor/Value/Object).
    pub class_kind: ClassKind,
    /// Whether this is an abstract class (cannot be instantiated).
    pub is_abstract: bool,
    /// Whether this is a sealed class (cannot be subclassed).
    pub is_sealed: bool,
    /// Whether this is a typed class (requires type annotations on methods).
    pub is_typed: bool,
    /// Whether this class is internal (package-scoped visibility, ADR 0071).
    pub is_internal: bool,
    /// Supervisor kind, set by semantic analysis (BT-1218, ADR 0059 Phase 1).
    ///
    /// `Some(Static)` if this class inherits from `Supervisor`,
    /// `Some(Dynamic)` if it inherits from `DynamicSupervisor`,
    /// `None` otherwise.
    pub supervisor_kind: Option<SupervisorKind>,
    /// Instance variable declarations.
    pub state: Vec<StateDeclaration>,
    /// Method definitions.
    pub methods: Vec<MethodDefinition>,
    /// Class-side method definitions (defined with `class` prefix).
    pub class_methods: Vec<MethodDefinition>,
    /// Class variable declarations (defined with `classState:`).
    pub class_variables: Vec<StateDeclaration>,
    /// Non-doc comments (`//` and `/* */`) appearing before this class.
    pub comments: CommentAttachment,
    /// Doc comment attached to this class (`///` lines).
    pub doc_comment: Option<String>,
    /// The Erlang module that backs this class (ADR 0056, `native: module`).
    ///
    /// Set by the parser when `native: <module>` appears on the `subclass:` declaration.
    /// `None` for all ordinary Beamtalk classes.
    pub backing_module: Option<Identifier>,
    /// Type parameters for generic classes (e.g., `T`, `E` in `Result(T, E)`).
    ///
    /// Empty for non-generic classes. Populated by the parser when parenthesized
    /// type parameters appear after the class name in the definition.
    /// Each parameter may optionally carry a protocol bound (ADR 0068 Phase 2d).
    pub type_params: Vec<TypeParamDecl>,
    /// Type arguments applied to the superclass (e.g., `(E)` in `Collection(E) subclass: Array(E)`).
    ///
    /// Empty when the superclass is not parameterized. Each entry is a type annotation
    /// that may reference this class's own type params (forwarded) or be a concrete type
    /// (e.g., `Integer` in `Collection(Integer) subclass: IntArray`).
    pub superclass_type_args: Vec<TypeAnnotation>,
    /// Source location of the entire class definition.
    pub span: Span,
}

impl ClassDefinition {
    /// Creates a new class definition.
    #[must_use]
    pub fn new(
        name: Identifier,
        superclass: Identifier,
        state: Vec<StateDeclaration>,
        methods: Vec<MethodDefinition>,
        span: Span,
    ) -> Self {
        let class_kind = ClassKind::from_superclass_name(superclass.name.as_str());
        Self {
            name,
            superclass: Some(superclass),
            superclass_package: None,
            class_kind,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            is_internal: false,
            supervisor_kind: None,
            state,
            methods,
            class_methods: Vec::new(),
            class_variables: Vec::new(),
            type_params: Vec::new(),
            superclass_type_args: Vec::new(),
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span,
        }
    }

    /// Creates a class definition with modifiers.
    #[must_use]
    pub fn with_modifiers(
        name: Identifier,
        superclass: Option<Identifier>,
        is_abstract: bool,
        is_sealed: bool,
        state: Vec<StateDeclaration>,
        methods: Vec<MethodDefinition>,
        span: Span,
    ) -> Self {
        let class_kind = superclass.as_ref().map_or(ClassKind::Object, |s| {
            ClassKind::from_superclass_name(s.name.as_str())
        });
        Self {
            name,
            superclass,
            superclass_package: None,
            class_kind,
            is_abstract,
            is_sealed,
            is_typed: false,
            is_internal: false,
            supervisor_kind: None,
            state,
            methods,
            class_methods: Vec::new(),
            class_variables: Vec::new(),
            type_params: Vec::new(),
            superclass_type_args: Vec::new(),
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span,
        }
    }

    /// Returns the superclass name, or `"none"` for root classes.
    #[must_use]
    pub fn superclass_name(&self) -> &str {
        self.superclass.as_ref().map_or("none", |s| s.name.as_str())
    }
}

/// A standalone method definition (Tonel-style).
///
/// Defines or replaces a single method on an existing class without
/// redefining the entire class. Works in `.bt` files, REPL, and via LSP.
///
/// Example: `Counter >> increment => self.value := self.value + 1`
/// Example: `Counter class >> withInitial: n => self spawnWith: #{value => n}`
/// Example: `json@Parser >> lenientParse: input => ...` (cross-package extension, ADR 0070)
#[derive(Debug, Clone, PartialEq)]
pub struct StandaloneMethodDefinition {
    /// The class this method belongs to.
    pub class_name: Identifier,
    /// Optional package qualifier for cross-package extensions (ADR 0070).
    /// E.g., `json` in `json@Parser >> lenientParse: input => ...`
    pub package: Option<Identifier>,
    /// Whether this is a class-side method (`Counter class >> ...`).
    pub is_class_method: bool,
    /// The method definition.
    pub method: MethodDefinition,
    /// Source location of the entire standalone method definition.
    pub span: Span,
}

/// A protocol definition (ADR 0068, Phase 2a).
///
/// Protocols define named message sets. A class conforms to a protocol if it
/// responds to all required messages — no explicit `implements:` declaration needed.
///
/// Example:
/// ```text
/// Protocol define: Printable
///   /// Return a human-readable string representation.
///   asString -> String
///
/// Protocol define: Collection(E)
///   size -> Integer
///   do: block :: Block(E, Object)
///   collect: block :: Block(E, Object) -> Self
///
/// Protocol define: Sortable
///   extending: Comparable
///   sortKey -> Object
/// ```
///
/// Protocol names are bare identifiers (uppercase, like class names). Protocols
/// and classes share a single namespace — having both a class and a protocol
/// with the same name is a compile error.
#[derive(Debug, Clone, PartialEq)]
pub struct ProtocolDefinition {
    /// The protocol name (e.g., `Printable`, `Collection`).
    pub name: Identifier,
    /// Type parameters for generic protocols (e.g., `E` in `Collection(E)`).
    ///
    /// Empty for non-generic protocols.
    /// Each parameter may optionally carry a protocol bound (ADR 0068 Phase 2d).
    pub type_params: Vec<TypeParamDecl>,
    /// The protocol this one extends, if any.
    ///
    /// Example: `extending: Comparable` in `Sortable`.
    pub extending: Option<Identifier>,
    /// Required instance method signatures.
    pub method_signatures: Vec<ProtocolMethodSignature>,
    /// Required class method signatures (BT-1611).
    ///
    /// Defined with the `class` prefix, e.g., `class fromString: aString :: String -> Self`.
    /// Conforming classes must respond to these selectors on the class side.
    pub class_method_signatures: Vec<ProtocolMethodSignature>,
    /// Non-doc comments (`//` and `/* */`) appearing before this protocol.
    pub comments: CommentAttachment,
    /// Doc comment attached to this protocol (`///` lines).
    pub doc_comment: Option<String>,
    /// Source location of the entire protocol definition.
    pub span: Span,
}

/// A required method signature in a protocol definition (ADR 0068, Phase 2a).
///
/// Protocol method signatures are like method definitions but without a body
/// (`=>` and implementation). They declare the shape a conforming class must have.
///
/// Examples:
/// - Unary: `asString -> String`
/// - Binary: `< other :: Self -> Boolean`
/// - Keyword: `do: block :: Block(E, Object)`
/// - With doc comment: `/// The number of elements.\n  size -> Integer`
#[derive(Debug, Clone, PartialEq)]
pub struct ProtocolMethodSignature {
    /// The method selector (name).
    pub selector: MessageSelector,
    /// Method parameters with optional type annotations.
    pub parameters: Vec<ParameterDefinition>,
    /// Optional return type annotation.
    pub return_type: Option<TypeAnnotation>,
    /// Non-doc comments (`//` and `/* */`) appearing before this signature.
    pub comments: CommentAttachment,
    /// Doc comment attached to this signature (`///` lines).
    pub doc_comment: Option<String>,
    /// Source location.
    pub span: Span,
}

/// Which keyword was used to declare an instance variable.
///
/// Both `state:` and `field:` parse to [`StateDeclaration`]; this enum tracks
/// which keyword appeared in source so the formatter can round-trip it and
/// Phase 2 warnings can fire when the wrong keyword is used for a class kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum DeclaredKeyword {
    /// The `state:` keyword (mutable actor state).
    #[default]
    State,
    /// The `field:` keyword (immutable value-object data).
    Field,
}

impl DeclaredKeyword {
    /// Returns the source-level keyword string (with trailing space).
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::State => "state: ",
            Self::Field => "field: ",
        }
    }
}

/// A state (instance variable) declaration.
///
/// Example: `state: value :: Integer = 0`
#[derive(Debug, Clone, PartialEq)]
pub struct StateDeclaration {
    /// The field name.
    pub name: Identifier,
    /// Optional type annotation.
    pub type_annotation: Option<TypeAnnotation>,
    /// Optional default value.
    pub default_value: Option<Expression>,
    /// Which keyword was used in source (`state:` or `field:`).
    pub declared_keyword: DeclaredKeyword,
    /// Non-doc comments (`//` and `/* */`) appearing before this field.
    pub comments: CommentAttachment,
    /// Doc comment attached to this field (`///` lines).
    pub doc_comment: Option<String>,
    /// Source location.
    pub span: Span,
}

impl StateDeclaration {
    /// Creates a new state declaration.
    #[must_use]
    pub fn new(name: Identifier, span: Span) -> Self {
        Self {
            name,
            type_annotation: None,
            default_value: None,
            declared_keyword: DeclaredKeyword::default(),
            comments: CommentAttachment::default(),
            doc_comment: None,
            span,
        }
    }

    /// Creates a state declaration with a type annotation.
    #[must_use]
    pub fn with_type(name: Identifier, type_annotation: TypeAnnotation, span: Span) -> Self {
        Self {
            name,
            type_annotation: Some(type_annotation),
            default_value: None,
            declared_keyword: DeclaredKeyword::default(),
            comments: CommentAttachment::default(),
            doc_comment: None,
            span,
        }
    }

    /// Creates a state declaration with a default value.
    #[must_use]
    pub fn with_default(name: Identifier, default_value: Expression, span: Span) -> Self {
        Self {
            name,
            type_annotation: None,
            default_value: Some(default_value),
            declared_keyword: DeclaredKeyword::default(),
            comments: CommentAttachment::default(),
            doc_comment: None,
            span,
        }
    }

    /// Creates a state declaration with both type and default value.
    #[must_use]
    pub fn with_type_and_default(
        name: Identifier,
        type_annotation: TypeAnnotation,
        default_value: Expression,
        span: Span,
    ) -> Self {
        Self {
            name,
            type_annotation: Some(type_annotation),
            default_value: Some(default_value),
            declared_keyword: DeclaredKeyword::default(),
            comments: CommentAttachment::default(),
            doc_comment: None,
            span,
        }
    }
}
