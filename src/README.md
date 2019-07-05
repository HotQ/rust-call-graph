```rs
#[derive(Clone, RustcEncodable, RustcDecodable, Debug)]
pub struct Crate {
    pub module: Mod,           // syntex_syntax::ast::Mod
    pub attrs: Vec<Attribute>, // alloc::vec::Vec<syntex_syntax::ast::Attribute>
    pub span: Span,            // syntex_pos::Span
}

/// Module declaration.
///
/// E.g., `mod foo;` or `mod foo { .. }`.
#[derive(Clone, RustcEncodable, RustcDecodable, Debug)]
pub struct Mod {
    /// A span from the first token past `{` to the last token until `}`.
    /// For `mod foo;`, the inner span ranges from the first token
    /// to the last token in the external file.
    pub inner: Span,
    pub items: Vec<P<Item>>,
    /// `true` for `mod foo { .. }`; `false` for `mod foo;`.
    pub inline: bool,
}


#[derive(Clone, RustcEncodable, RustcDecodable, Debug)]
pub struct Item {
    pub ident: Ident,
    pub attrs: Vec<Attribute>,
    pub id: NodeId,
    pub node: ItemKind,
    pub vis: Visibility,
    pub span: Span,

    /// Original tokens this item was parsed from. This isn't necessarily
    /// available for all items, although over time more and more items should
    /// have this be `Some`. Right now this is primarily used for procedural
    /// macros, notably custom attributes.
    ///
    /// Note that the tokens here do not include the outer attributes, but will
    /// include inner attributes.
    pub tokens: Option<TokenStream>,
}

/// An owned smart pointer.
#[derive(Hash, PartialEq, Eq)]
pub struct P<T: ?Sized> {
    ptr: Box<T>
}

```


```rs
#[derive(Clone, RustcEncodable, RustcDecodable, Debug)]
pub enum ItemKind {
    /// An `extern crate` item, with optional *original* crate name if the crate was renamed.
    ///
    /// E.g., `extern crate foo` or `extern crate foo_bar as foo`.
    ExternCrate(Option<Name>),
    /// A use declaration (`use` or `pub use`) item.
    ///
    /// E.g., `use foo;`, `use foo::bar;` or `use foo::bar as FooBar;`.
    Use(P<UseTree>),
    /// A static item (`static` or `pub static`).
    ///
    /// E.g., `static FOO: i32 = 42;` or `static FOO: &'static str = "bar";`.
    Static(P<Ty>, Mutability, P<Expr>),
    /// A constant item (`const` or `pub const`).
    ///
    /// E.g., `const FOO: i32 = 42;`.
    Const(P<Ty>, P<Expr>),
    /// A function declaration (`fn` or `pub fn`).
    ///
    /// E.g., `fn foo(bar: usize) -> usize { .. }`.
    /// 
    // &syntex_syntax::ptr::P<syntex_syntax::ast::FnDecl> 
    // &syntex_syntax::ast::Unsafety 
    // &syntex_syntax::codemap::Spanned<syntex_syntax::ast::Constness> 
    // &syntex_syntax::abi::Abi 
    // &syntex_syntax::ast::Generics 
    // &syntex_syntax::ptr::P<syntex_syntax::ast::Block> 
    Fn(P<FnDecl>, FnHeader, Generics, P<Block>),
    /// A module declaration (`mod` or `pub mod`).
    ///
    /// E.g., `mod foo;` or `mod foo { .. }`.
    Mod(Mod),
    /// An external module (`extern` or `pub extern`).
    ///
    /// E.g., `extern {}` or `extern "C" {}`.
    ForeignMod(ForeignMod),
    /// Module-level inline assembly (from `global_asm!()`).
    GlobalAsm(P<GlobalAsm>),
    /// A type alias (`type` or `pub type`).
    ///
    /// E.g., `type Foo = Bar<u8>;`.
    Ty(P<Ty>, Generics),
    /// An existential type declaration (`existential type`).
    ///
    /// E.g., `existential type Foo: Bar + Boo;`.
    Existential(GenericBounds, Generics),
    /// An enum definition (`enum` or `pub enum`).
    ///
    /// E.g., `enum Foo<A, B> { C<A>, D<B> }`.
    Enum(EnumDef, Generics),
    /// A struct definition (`struct` or `pub struct`).
    ///
    /// E.g., `struct Foo<A> { x: A }`.
    Struct(VariantData, Generics),
    /// A union definition (`union` or `pub union`).
    ///
    /// E.g., `union Foo<A, B> { x: A, y: B }`.
    Union(VariantData, Generics),
    /// A Trait declaration (`trait` or `pub trait`).
    ///
    /// E.g., `trait Foo { .. }`, `trait Foo<T> { .. }` or `auto trait Foo {}`.
    Trait(IsAuto, Unsafety, Generics, GenericBounds, Vec<TraitItem>),
    /// Trait alias
    ///
    /// E.g., `trait Foo = Bar + Quux;`.
    TraitAlias(Generics, GenericBounds),
    /// An implementation.
    ///
    /// E.g., `impl<A> Foo<A> { .. }` or `impl<A> Trait for Foo<A> { .. }`.
    Impl(
        Unsafety,
        ImplPolarity,
        Defaultness,
        Generics,
        Option<TraitRef>, // (optional) trait this impl implements
        P<Ty>,            // self
        Vec<ImplItem>,
    ),
    /// A macro invocation.
    ///
    /// E.g., `macro_rules! foo { .. }` or `foo!(..)`.
    Mac(Mac),

    /// A macro definition.
    MacroDef(MacroDef),
}

impl ItemKind {
    pub fn descriptive_variant(&self) -> &str {
        match *self {
            ItemKind::ExternCrate(..) => "extern crate",
            ItemKind::Use(..) => "use",
            ItemKind::Static(..) => "static item",
            ItemKind::Const(..) => "constant item",
            ItemKind::Fn(..) => "function",
            ItemKind::Mod(..) => "module",
            ItemKind::ForeignMod(..) => "foreign module",
            ItemKind::GlobalAsm(..) => "global asm",
            ItemKind::Ty(..) => "type alias",
            ItemKind::Existential(..) => "existential type",
            ItemKind::Enum(..) => "enum",
            ItemKind::Struct(..) => "struct",
            ItemKind::Union(..) => "union",
            ItemKind::Trait(..) => "trait",
            ItemKind::TraitAlias(..) => "trait alias",
            ItemKind::Mac(..) | ItemKind::MacroDef(..) | ItemKind::Impl(..) => "item",
        }
    }
}
```

Block & Stmt
```rs
/// A Block (`{ .. }`).
///
/// E.g., `{ .. }` as in `fn foo() { .. }`.
#[derive(Clone, RustcEncodable, RustcDecodable, Debug)]
pub struct Block {
    /// Statements in a block
    pub stmts: Vec<Stmt>,
    pub id: NodeId,
    /// Distinguishes between `unsafe { ... }` and `{ ... }`
    pub rules: BlockCheckMode,
    pub span: Span,
}

/// A statement
#[derive(Clone, RustcEncodable, RustcDecodable)]
pub struct Stmt {
    pub id: NodeId,
    pub node: StmtKind,
    pub span: Span,
}

#[derive(Clone, RustcEncodable, RustcDecodable)]
pub enum StmtKind {
    /// A local (let) binding.
    Local(P<Local>),

    /// An item definition.
    Item(P<Item>),

    /// Expr without trailing semi-colon.
    Expr(P<Expr>),
    /// Expr with a trailing semi-colon.
    Semi(P<Expr>),
    /// Macro.
    Mac(P<(Mac, MacStmtStyle, ThinVec<Attribute>)>),
}

/// Local represents a `let` statement, e.g., `let <pat>:<ty> = <expr>;`.
#[derive(Clone, RustcEncodable, RustcDecodable, Debug)]
pub struct Local {
    pub pat: P<Pat>,
    pub ty: Option<P<Ty>>,
    /// Initializer expression to set the value, if any.
    pub init: Option<P<Expr>>,
    pub id: NodeId,
    pub span: Span,
    pub attrs: ThinVec<Attribute>,
}

/// An expression
#[derive(Clone, RustcEncodable, RustcDecodable)]
pub struct Expr {
    pub id: NodeId,
    pub node: ExprKind,
    pub span: Span,
    pub attrs: ThinVec<Attribute>,
}
```


```rust
#[derive(Clone, RustcEncodable, RustcDecodable, Debug)]
pub enum ExprKind {
    /// A `box x` expression.
    Box(P<Expr>),
    /// First expr is the place; second expr is the value.
    ObsoleteInPlace(P<Expr>, P<Expr>),
    /// An array (`[a, b, c, d]`)
    Array(Vec<P<Expr>>),
    /// A function call
    ///
    /// The first field resolves to the function itself,
    /// and the second field is the list of arguments.
    /// This also represents calling the constructor of
    /// tuple-like ADTs such as tuple structs and enum variants.
    Call(P<Expr>, Vec<P<Expr>>),
    /// A method call (`x.foo::<'static, Bar, Baz>(a, b, c, d)`)
    ///
    /// The `PathSegment` represents the method name and its generic arguments
    /// (within the angle brackets).
    /// The first element of the vector of an `Expr` is the expression that evaluates
    /// to the object on which the method is being called on (the receiver),
    /// and the remaining elements are the rest of the arguments.
    /// Thus, `x.foo::<Bar, Baz>(a, b, c, d)` is represented as
    /// `ExprKind::MethodCall(PathSegment { foo, [Bar, Baz] }, [x, a, b, c, d])`.
    MethodCall(PathSegment, Vec<P<Expr>>),
    /// A tuple (e.g., `(a, b, c, d)`).
    Tup(Vec<P<Expr>>),
    /// A binary operation (e.g., `a + b`, `a * b`).
    Binary(BinOp, P<Expr>, P<Expr>),
    /// A unary operation (e.g., `!x`, `*x`).
    Unary(UnOp, P<Expr>),
    /// A literal (e.g., `1`, `"foo"`).
    Lit(Lit),
    /// A cast (e.g., `foo as f64`).
    Cast(P<Expr>, P<Ty>),
    Type(P<Expr>, P<Ty>),
    /// An `if` block, with an optional `else` block.
    ///
    /// `if expr { block } else { expr }`
    If(P<Expr>, P<Block>, Option<P<Expr>>),
    /// An `if let` expression with an optional else block
    ///
    /// `if let pat = expr { block } else { expr }`
    ///
    /// This is desugared to a `match` expression.
    IfLet(Vec<P<Pat>>, P<Expr>, P<Block>, Option<P<Expr>>),
    /// A while loop, with an optional label
    ///
    /// `'label: while expr { block }`
    While(P<Expr>, P<Block>, Option<Label>),
    /// A `while let` loop, with an optional label.
    ///
    /// `'label: while let pat = expr { block }`
    ///
    /// This is desugared to a combination of `loop` and `match` expressions.
    WhileLet(Vec<P<Pat>>, P<Expr>, P<Block>, Option<Label>),
    /// A `for` loop, with an optional label.
    ///
    /// `'label: for pat in expr { block }`
    ///
    /// This is desugared to a combination of `loop` and `match` expressions.
    ForLoop(P<Pat>, P<Expr>, P<Block>, Option<Label>),
    /// Conditionless loop (can be exited with `break`, `continue`, or `return`).
    ///
    /// `'label: loop { block }`
    Loop(P<Block>, Option<Label>),
    /// A `match` block.
    Match(P<Expr>, Vec<Arm>),
    /// A closure (e.g., `move |a, b, c| a + b + c`).
    ///
    /// The final span is the span of the argument block `|...|`.
    Closure(CaptureBy, IsAsync, Movability, P<FnDecl>, P<Expr>, Span),
    /// A block (`'label: { ... }`).
    Block(P<Block>, Option<Label>),
    /// An async block (`async move { ... }`).
    ///
    /// The `NodeId` is the `NodeId` for the closure that results from
    /// desugaring an async block, just like the NodeId field in the
    /// `IsAsync` enum. This is necessary in order to create a def for the
    /// closure which can be used as a parent of any child defs. Defs
    /// created during lowering cannot be made the parent of any other
    /// preexisting defs.
    Async(CaptureBy, NodeId, P<Block>),
    /// A try block (`try { ... }`).
    TryBlock(P<Block>),

    /// An assignment (`a = foo()`).
    Assign(P<Expr>, P<Expr>),
    /// An assignment with an operator.
    ///
    /// E.g., `a += 1`.
    AssignOp(BinOp, P<Expr>, P<Expr>),
    /// Access of a named (e.g., `obj.foo`) or unnamed (e.g., `obj.0`) struct field.
    Field(P<Expr>, Ident),
    /// An indexing operation (e.g., `foo[2]`).
    Index(P<Expr>, P<Expr>),
    /// A range (e.g., `1..2`, `1..`, `..2`, `1...2`, `1...`, `...2`).
    Range(Option<P<Expr>>, Option<P<Expr>>, RangeLimits),

    /// Variable reference, possibly containing `::` and/or type
    /// parameters (e.g., `foo::bar::<baz>`).
    ///
    /// Optionally "qualified" (e.g., `<Vec<T> as SomeTrait>::SomeType`).
    Path(Option<QSelf>, Path),

    /// A referencing operation (`&a` or `&mut a`).
    AddrOf(Mutability, P<Expr>),
    /// A `break`, with an optional label to break, and an optional expression.
    Break(Option<Label>, Option<P<Expr>>),
    /// A `continue`, with an optional label.
    Continue(Option<Label>),
    /// A `return`, with an optional value to be returned.
    Ret(Option<P<Expr>>),

    /// Output of the `asm!()` macro.
    InlineAsm(P<InlineAsm>),

    /// A macro invocation; pre-expansion.
    Mac(Mac),

    /// A struct literal expression.
    ///
    /// E.g., `Foo {x: 1, y: 2}`, or `Foo {x: 1, .. base}`,
    /// where `base` is the `Option<Expr>`.
    Struct(Path, Vec<Field>, Option<P<Expr>>),

    /// An array literal constructed from one repeated element.
    ///
    /// E.g., `[1; 5]`. The expression is the element to be
    /// repeated; the constant is the number of times to repeat it.
    Repeat(P<Expr>, AnonConst),

    /// No-op: used solely so we can pretty-print faithfully.
    Paren(P<Expr>),

    /// A try expression (`expr?`).
    Try(P<Expr>),

    /// A `yield`, with an optional value to be yielded.
    Yield(Option<P<Expr>>),

    /// Placeholder for an expression that wasn't syntactically well formed in some way.
    Err,
}
```
