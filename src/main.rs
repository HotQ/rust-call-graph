#![allow(unused_imports)]
#![allow(unused_macros)]
#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_assignments)]
#![feature(core_intrinsics)]

extern crate pretty_env_logger;
#[macro_use]
extern crate log;
extern crate syn;

#[macro_use]
pub mod macros;
pub mod signature;

use macros::*;
use signature::*;

use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::io::BufReader;
use std::io::Read;
use std::process;

use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::{env, fmt, fs};

use syn::*;

#[derive(Clone, Hash, Eq, PartialEq)]
struct Func {
    path: Path,
    decl: Option<FnDecl>,
}

// impl Clone for Func {
//     fn clone(&self) -> Self {
//         let mut f = Func {
//             boss: self.boss.clone(),
//             path: Vec::new(),
//             decl: self.decl.clone(),
//         };
//         for seg in &self.path {
//             f.path.push(seg.clone());
//         }
//         f
//     }
// }

// impl fmt::Debug for Func {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         write!(f, "{:?}", self.path)
//     }
// }
impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut sign = "fn ".to_string() + &path_to_string(&self.path);
        if let Some(decl) = self.decl.clone() {
            let tys: Vec<String> = decl.inputs.iter().map(|x| fn_arg_to_string(x)).collect();
            sign = sign + &generic_to_string(&decl.generics) + "(" + &tys.join(", ") + ")";

            if let ReturnType::Type(_, ty) = &decl.output {
                sign = sign + " -> " + &type_to_string(ty);
            }
        }
        write!(f, "{}", sign)
    }
}

fn handle_item(record: &mut Record, item: &Item) {
    use Item::*;
    match item {
        Fn(ItemFn {
            attrs,
            vis,
            constness,
            asyncness,
            unsafety,
            abi,
            ident,
            decl,
            block,
        }) => {
            debug_all!(handle_item_fn, vis, ident);
            NeverConsidered!(
                handle_item,
                Fn,
                [attrs, |x: &Vec<Attribute>| x.len() == 0],
                [constness, |x: &Option<Token![const]>| *x == None],
                [asyncness, |x: &Option<Token![async]>| *x == None],
                [unsafety, |x: &Option<Token![unsafe]>| *x == None],
                [abi, |x: &Option<Abi>| *x == None]
            );

            let caller = Func {
                path: (PathSegment::from(ident.clone())).into(),
                decl: Some((**decl).clone()),
            };
            record.caller.insert(caller.clone());
            // error!("type name: {}",type_name(&decl));
            for stmt in &block.stmts {
                handle_stmt(record, &caller, stmt);
            }
        }
        _ => {
            error!("this ItemKind is not used yet: {:?}", item);
        }
    }
}

fn handle_stmt(record: &mut Record, caller: &Func, stmt: &Stmt) {
    // info!("stmt: {:#?}", stmt);
    //     info!("StmtKind :");
    use syn::Stmt::*;
    match stmt {
        Local(local) => {
            // handle_expr(record, caller, &expr.init.as_ref().unwrap());
            if let Some((_, expr)) = local.init.clone() {
                trace!("handle_stmt Local");
                handle_expr(record, caller, &expr);
            }
        }
        Item(item) => {
            handle_item(record, item);
        }
        Expr(expr) => {
            trace!("handle_stmt expr!");
            handle_expr(record, caller, &expr);
        }
        Semi(expr, t) => {
            trace!("handle_stmt Semi!");
            handle_expr(record, caller, &expr);
        }
    };
}

macro_rules! handle_block {
    ($record:ident,$caller:ident,$block:expr) => {
        for stmt in &$block.stmts {
            handle_stmt($record, $caller, &stmt);
        }
    };
}

fn handle_expr(record: &mut Record, caller: &Func, expr: &Expr) {
    use syn::Expr::*;

    match expr {
        // /// A box expression: `box f`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Box(ExprBox #full {
        //     pub attrs: Vec<Attribute>,
        //     pub box_token: Token![box],
        //     pub expr: Box<Expr>,
        // }),

        // /// A placement expression: `place <- value`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub InPlace(ExprInPlace #full {
        //     pub attrs: Vec<Attribute>,
        //     pub place: Box<Expr>,
        //     pub arrow_token: Token![<-],
        //     pub value: Box<Expr>,
        // }),

        // /// A slice literal expression: `[a, b, c, d]`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Array(ExprArray #full {
        //     pub attrs: Vec<Attribute>,
        //     pub bracket_token: token::Bracket,
        //     pub elems: Punctuated<Expr, Token![,]>,
        // }),

        // /// A function call expression: `invoke(a, b)`.
        // ///
        // /// *This type is available if Syn is built with the `"derive"` or
        // /// `"full"` feature.*
        // pub Call(ExprCall {
        //     pub attrs: Vec<Attribute>,
        //     pub func: Box<Expr>,
        //     pub paren_token: token::Paren,
        //     pub args: Punctuated<Expr, Token![,]>,
        // }),
        Call(expr_call) => {
            // warn!("Call   {:?}", expr_call.)
            use Expr::*;
            match &*expr_call.func {
                Path(expr_path) => {
                    let callee = Func {
                        path: expr_path.path.clone(),
                        decl: None,
                    };
                    record.callee.insert(callee.clone());
                    record.called.insert((caller.clone(), callee));
                }
                _ => error!("Call!NeverConsidered! {}", type_name(&*expr_call.func)),
            }
            for arg in &expr_call.args {
                // warn!("Call.arg   {:?}", arg);
                handle_expr(record, caller, arg);
            }
        }

        MethodCall(method_call) => {
            // error!("i can't handle this right now ");
            // error!("receiver {:?}", method_call.receiver);
            handle_expr(record, caller, &method_call.receiver);

            for arg in &method_call.args {
                // warn!("MethodCall args {:?}", arg);
                handle_expr(record, caller, arg);
            }
        }

        // /// A method call expression: `x.foo::<T>(a, b)`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub MethodCall(ExprMethodCall #full {
        //     pub attrs: Vec<Attribute>,
        //     pub receiver: Box<Expr>,
        //     pub dot_token: Token![.],
        //     pub method: Ident,
        //     pub turbofish: Option<MethodTurbofish>,
        //     pub paren_token: token::Paren,
        //     pub args: Punctuated<Expr, Token![,]>,
        // }),

        // /// A tuple expression: `(a, b, c, d)`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Tuple(ExprTuple #full {
        //     pub attrs: Vec<Attribute>,
        //     pub paren_token: token::Paren,
        //     pub elems: Punctuated<Expr, Token![,]>,
        // }),

        // /// A binary operation: `a + b`, `a * b`.
        // ///
        // /// *This type is available if Syn is built with the `"derive"` or
        // /// `"full"` feature.*
        // pub Binary(ExprBinary {
        //     pub attrs: Vec<Attribute>,
        //     pub left: Box<Expr>,
        //     pub op: BinOp,
        //     pub right: Box<Expr>,
        // }),

        // /// A unary operation: `!x`, `*x`.
        // ///
        // /// *This type is available if Syn is built with the `"derive"` or
        // /// `"full"` feature.*
        // pub Unary(ExprUnary {
        //     pub attrs: Vec<Attribute>,
        //     pub op: UnOp,
        //     pub expr: Box<Expr>,
        // }),

        // /// A literal in place of an expression: `1`, `"foo"`.
        // ///
        // /// *This type is available if Syn is built with the `"derive"` or
        // /// `"full"` feature.*
        // pub Lit(ExprLit {
        //     pub attrs: Vec<Attribute>,
        //     pub lit: Lit,
        // }),

        // /// A cast expression: `foo as f64`.
        // ///
        // /// *This type is available if Syn is built with the `"derive"` or
        // /// `"full"` feature.*
        // pub Cast(ExprCast {
        //     pub attrs: Vec<Attribute>,
        //     pub expr: Box<Expr>,
        //     pub as_token: Token![as],
        //     pub ty: Box<Type>,
        // }),

        // /// A type ascription expression: `foo: f64`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Type(ExprType #full {
        //     pub attrs: Vec<Attribute>,
        //     pub expr: Box<Expr>,
        //     pub colon_token: Token![:],
        //     pub ty: Box<Type>,
        // }),

        // /// A `let` guard: `let Some(x) = opt`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Let(ExprLet #full {
        //     pub attrs: Vec<Attribute>,
        //     pub let_token: Token![let],
        //     pub pats: Punctuated<Pat, Token![|]>,
        //     pub eq_token: Token![=],
        //     pub expr: Box<Expr>,
        // }),

        // /// An `if` expression with an optional `else` block: `if expr { ... }
        // /// else { ... }`.
        // ///
        // /// The `else` branch expression may only be an `If` or `Block`
        // /// expression, not any of the other types of expression.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub If(ExprIf #full {
        //     pub attrs: Vec<Attribute>,
        //     pub if_token: Token![if],
        //     pub cond: Box<Expr>,
        //     pub then_branch: Block,
        //     pub else_branch: Option<(Token![else], Box<Expr>)>,
        // }),
        If(expr_if) => {
            handle_expr(record, caller, &expr_if.cond);
            handle_block!(record, caller, expr_if.then_branch);
            // error!("expr_if.then_branch {:#?}", expr_if.then_branch);
            if let Some((_, expr)) = &expr_if.else_branch {
                handle_expr(record, caller, &*expr);
            }
        }

        // /// A while loop: `while expr { ... }`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub While(ExprWhile #full {
        //     pub attrs: Vec<Attribute>,
        //     pub label: Option<Label>,
        //     pub while_token: Token![while],
        //     pub cond: Box<Expr>,
        //     pub body: Block,
        // }),

        // /// A for loop: `for pat in expr { ... }`.
        ForLoop(for_loop) => {
            NeverConsidered!(
                handle_expr,
                ForLoop,
                [&for_loop.attrs, move |x: &Vec<Attribute>| x.len() == 0],
                [&for_loop.label, move |x: &Option<Label>| *x == None]
            );
            // trace!("ForLoop Pat: {:?}", for_loop.pat);
            handle_expr(record, caller, &*for_loop.expr);
            handle_block!(record, caller, for_loop.body);
        }

        // /// Conditionless loop: `loop { ... }`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Loop(ExprLoop #full {
        //     pub attrs: Vec<Attribute>,
        //     pub label: Option<Label>,
        //     pub loop_token: Token![loop],
        //     pub body: Block,
        // }),
        Match(expr_match) => {
            NeverConsidered!(Match, [expr_match.attrs, |x: &Vec<Attribute>| x.len() == 0]);
            handle_expr(record, caller, &expr_match.expr);
            for arm in &expr_match.arms {
                // info!("arm: {:#?}", arm.body);
                NeverConsidered!(
                    Match,
                    guard,
                    [
                        arm.guard,
                        |x: &Option<(Token![if], std::boxed::Box<Expr>)>| *x == None
                    ]
                );
                handle_expr(record, caller, &*arm.body);
            }
        }

        // /// A closure expression: `|a, b| a + b`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Closure(ExprClosure #full {
        //     pub attrs: Vec<Attribute>,
        //     pub asyncness: Option<Token![async]>,
        //     pub movability: Option<Token![static]>,
        //     pub capture: Option<Token![move]>,
        //     pub or1_token: Token![|],
        //     pub inputs: Punctuated<FnArg, Token![,]>,
        //     pub or2_token: Token![|],
        //     pub output: ReturnType,
        //     pub body: Box<Expr>,
        // }),
        Closure(closure) => {
            handle_expr(record, caller, &*closure.body);
        }

        // /// An unsafe block: `unsafe { ... }`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Unsafe(ExprUnsafe #full {
        //     pub attrs: Vec<Attribute>,
        //     pub unsafe_token: Token![unsafe],
        //     pub block: Block,
        // }),

        // /// A blocked scope: `{ ... }`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Block(ExprBlock #full {
        //     pub attrs: Vec<Attribute>,
        //     pub label: Option<Label>,
        //     pub block: Block,
        // }),
        Block(ExprBlock {
            attrs,
            label,
            block,
        }) => {
            NeverConsidered!(
                Block,
                [attrs, |x: &Vec<Attribute>| x.len() == 0],
                [label, |x: &Option<Label>| *x == None]
            );
            handle_block!(record, caller, block);
        }

        // /// An assignment expression: `a = compute()`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Assign(ExprAssign #full {
        //     pub attrs: Vec<Attribute>,
        //     pub left: Box<Expr>,
        //     pub eq_token: Token![=],
        //     pub right: Box<Expr>,
        // }),

        // /// A compound assignment expression: `counter += 1`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub AssignOp(ExprAssignOp #full {
        //     pub attrs: Vec<Attribute>,
        //     pub left: Box<Expr>,
        //     pub op: BinOp,
        //     pub right: Box<Expr>,
        // }),

        // /// Access of a named struct field (`obj.k`) or unnamed tuple struct
        // /// field (`obj.0`).
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Field(ExprField {
        //     pub attrs: Vec<Attribute>,
        //     pub base: Box<Expr>,
        //     pub dot_token: Token![.],
        //     pub member: Member,
        // }),

        // /// A square bracketed indexing expression: `vector[2]`.
        // ///
        // /// *This type is available if Syn is built with the `"derive"` or
        // /// `"full"` feature.*
        // pub Index(ExprIndex {
        //     pub attrs: Vec<Attribute>,
        //     pub expr: Box<Expr>,
        //     pub bracket_token: token::Bracket,
        //     pub index: Box<Expr>,
        // }),

        // /// A range expression: `1..2`, `1..`, `..2`, `1..=2`, `..=2`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Range(ExprRange #full {
        //     pub attrs: Vec<Attribute>,
        //     pub from: Option<Box<Expr>>,
        //     pub limits: RangeLimits,
        //     pub to: Option<Box<Expr>>,
        // }),

        // /// A path like `std::mem::replace` possibly containing generic
        // /// parameters and a qualified self-type.
        // ///
        // /// A plain identifier like `x` is a path of length 1.
        // ///
        // /// *This type is available if Syn is built with the `"derive"` or
        // /// `"full"` feature.*
        // pub Path(ExprPath {
        //     pub attrs: Vec<Attribute>,
        //     pub qself: Option<QSelf>,
        //     pub path: Path,
        // }),
        // Path(expr_path) => handle_expr(record, caller, &expr_path.path),

        // /// A referencing operation: `&a` or `&mut a`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Reference(ExprReference #full {
        //     pub attrs: Vec<Attribute>,
        //     pub and_token: Token![&],
        //     pub mutability: Option<Token![mut]>,
        //     pub expr: Box<Expr>,
        // }),

        // /// A `break`, with an optional label to break and an optional
        // /// expression.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Break(ExprBreak #full {
        //     pub attrs: Vec<Attribute>,
        //     pub break_token: Token![break],
        //     pub label: Option<Lifetime>,
        //     pub expr: Option<Box<Expr>>,
        // }),

        // /// A `continue`, with an optional label.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Continue(ExprContinue #full {
        //     pub attrs: Vec<Attribute>,
        //     pub continue_token: Token![continue],
        //     pub label: Option<Lifetime>,
        // }),

        // /// A `return`, with an optional value to be returned.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Return(ExprReturn #full {
        //     pub attrs: Vec<Attribute>,
        //     pub return_token: Token![return],
        //     pub expr: Option<Box<Expr>>,
        // }),

        // /// A macro invocation expression: `format!("{}", q)`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Macro(ExprMacro #full {
        //     pub attrs: Vec<Attribute>,
        //     pub mac: Macro,
        // }),

        // /// A struct literal expression: `Point { x: 1, y: 1 }`.
        // ///
        // /// The `rest` provides the value of the remaining fields as in `S { a:
        // /// 1, b: 1, ..rest }`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Struct(ExprStruct #full {
        //     pub attrs: Vec<Attribute>,
        //     pub path: Path,
        //     pub brace_token: token::Brace,
        //     pub fields: Punctuated<FieldValue, Token![,]>,
        //     pub dot2_token: Option<Token![..]>,
        //     pub rest: Option<Box<Expr>>,
        // }),

        // /// An array literal constructed from one repeated element: `[0u8; N]`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Repeat(ExprRepeat #full {
        //     pub attrs: Vec<Attribute>,
        //     pub bracket_token: token::Bracket,
        //     pub expr: Box<Expr>,
        //     pub semi_token: Token![;],
        //     pub len: Box<Expr>,
        // }),

        // /// A parenthesized expression: `(a + b)`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Paren(ExprParen {
        //     pub attrs: Vec<Attribute>,
        //     pub paren_token: token::Paren,
        //     pub expr: Box<Expr>,
        // }),

        // /// An expression contained within invisible delimiters.
        // ///
        // /// This variant is important for faithfully representing the precedence
        // /// of expressions and is related to `None`-delimited spans in a
        // /// `TokenStream`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Group(ExprGroup #full {
        //     pub attrs: Vec<Attribute>,
        //     pub group_token: token::Group,
        //     pub expr: Box<Expr>,
        // }),

        // /// A try-expression: `expr?`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Try(ExprTry #full {
        //     pub attrs: Vec<Attribute>,
        //     pub expr: Box<Expr>,
        //     pub question_token: Token![?],
        // }),

        // /// An async block: `async { ... }`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Async(ExprAsync #full {
        //     pub attrs: Vec<Attribute>,
        //     pub async_token: Token![async],
        //     pub capture: Option<Token![move]>,
        //     pub block: Block,
        // }),

        // /// A try block: `try { ... }`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub TryBlock(ExprTryBlock #full {
        //     pub attrs: Vec<Attribute>,
        //     pub try_token: Token![try],
        //     pub block: Block,
        // }),

        // /// A yield expression: `yield expr`.
        // ///
        // /// *This type is available if Syn is built with the `"full"` feature.*
        // pub Yield(ExprYield #full {
        //     pub attrs: Vec<Attribute>,
        //     pub yield_token: Token![yield],
        //     pub expr: Option<Box<Expr>>,
        // }),

        // /// Tokens in expression position not interpreted by Syn.
        // ///
        // /// *This type is available if Syn is built with the `"derive"` or
        // /// `"full"` feature.*
        // pub Verbatim(ExprVerbatim #manual_extra_traits {
        //     pub tts: TokenStream,
        // }),
        _ => {} // _ => info!("handle_expr: ___ {:?}", expr), // debug
    }
}
fn demo(_: u32, _: i32) -> std::vec::Vec<i32> {
    vec![1, 23]
}
struct Record {
    impls: HashMap<Type, Vec<Func>>,
    caller: HashSet<Func>,
    called: HashSet<(Func, Func)>,
    callee: HashSet<Func>,
}

// fn mangle_type(ty: &Type) -> String {
//     let mut ret = String::from("_ZT");
//     for seg in &ty.path {
//         ret.push_str(&format!("{}{}", seg.len(), seg));
//     }
//     ret.push('E');
//     ret
// }
fn mangle_func(func: &Func) -> String {
    let mut ret = String::from("_ZN");
    for seg in &func.path.segments {
        let tmp = seg.ident.to_string();
        ret.push_str(&format!("{}{}", tmp.len(), tmp));
    }
    ret.push('E');
    ret
}

fn decl_dot(decl: &FnDecl) -> String {
    let mut ret = String::from("");
    ret = ret
        + "\n\t\t| "
        + &match &decl.output {
            ReturnType::Default => String::from(" "),
            ReturnType::Type(_, ty) => html_escape!(type_to_string(ty)),
        };
    for input in &decl.inputs {
        ret = ret + "\n\t\t| " + &html_escape!(fn_arg_to_string(input));
    }
    ret
}

fn generate_dot() {
    let record = gen_callgraph();
    let mut src_dot = String::from("digraph demo{\n\trankdir=LR\n");

    for callee in &record.callee {
        src_dot += &format!(
            "\t{}[shape = record label = <<B>{}</B>>];\n",
            mangle_func(&callee),
            path_to_string(&callee.path)
        );
    }
    for caller in &record.caller {
        src_dot += &format!(
            "\t{}[shape = record label = <<B> {} </B>{}>];\n",
            mangle_func(&caller),
            path_to_string(&caller.path),
            decl_dot(&(caller.decl.clone().unwrap())),
        );
    }

    // // cluster the `impl` block
    // for pair in &record.impls {
    //     src_dot += &format!(
    //         "\tsubgraph cluster{} {}\n\
    //          \t\tstyle = \"bold\"\n\
    //          \t\t{}[shape = none, label = \"{}\"]\n",
    //         mangle_type(&pair.0),
    //         '{',
    //         mangle_type(&pair.0),
    //         pair.0.to_str()
    //     );
    //     for func in pair.1 {
    //         src_dot += &format!("\t\t{};\n", mangle_func(&func));
    //     }
    //     src_dot += &format!("\t{}\n", '}');
    // }

    // edge: caller -> callee
    for tuple in &record.called {
        src_dot += &format!("\t{}->{};\n", mangle_func(&tuple.0), mangle_func(&tuple.1));
    }

    // end of the graph
    src_dot += "}";

    write_to_file(src_dot);
}
fn write_to_file(src_dot: String) {
    let path: &str = "output.dot";
    let mut output: File = File::create(path).unwrap();
    let res = write!(output, "{}", src_dot);
    let input: File = File::open(path).unwrap();
    let buffered: BufReader<File> = BufReader::new(input);
}

fn gen_callgraph() -> Record {
    let mut record: Record = Record {
        impls: HashMap::new(),
        caller: HashSet::new(),
        called: HashSet::new(),
        callee: HashSet::new(),
    };

    let args: Vec<String> = env::args().collect();
    let filename = args[1].as_str();
    let mut file = File::open(filename).expect("Unable to open file");

    let mut src = String::new();
    file.read_to_string(&mut src).expect("Unable to read file");

    let syntax = syn::parse_file(&src).expect("Unable to parse file");
    for item in &syntax.items {
        handle_item(&mut record, &item);
    }

    for caller in &record.caller {
        warn!("caller {}\t\t\t\t\t{}", caller, mangle_func(&caller));
        // error!(
        //     "ajdsflkajdfads : {}",
        //     match &caller.decl.clone().unwrap().output {
        //         ReturnType::Default => String::from(" "),
        //         ReturnType::Type(_, ty) => html_escape!(type_to_string(ty)),
        //     }
        // );
    }
    for callee in &record.callee {
        warn!("callee {}\t\t\t\t\t{}", callee, mangle_func(&callee));
    }
    for called in &record.called {
        warn!("called {} =====> {}", called.0, called.1);
    }
    return record;
}

fn main() {
    pretty_env_logger::init();

    generate_dot();
}
