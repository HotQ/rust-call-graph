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

use self::utils::*;
mod utils;

use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::io::BufReader;
use std::io::Read;
use std::process;

use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::{env, fmt, fs};

// extern crate syntex_syntax as syntax;

// use syntax::ast::{Expr, ExprKind, ImplItemKind, Item, ItemKind, Stmt, StmtKind, TyKind};
// use syntax::codemap::{CodeMap, FilePathMapping};
// use syntax::parse::{self, ParseSess};
// use syntax::print::pprust;
// use syntax::tokenstream::TokenStream;

// fn ident2string(id: &syntex_pos::symbol::Ident) -> String {
//     let str_id = String::from(&*id.name.as_str());
//     str_id
// }

type Path = Vec<String>;
// type PathEx = Vec<(
//     String,
//     Option<syntex_syntax::ptr::P<syntax::ast::PathParameters>>,
// )>;

#[derive(Hash, Eq, PartialEq, Clone)]
struct Type {
    path: Path,
}

#[derive(Hash, Eq, PartialEq, Clone)]
enum BossKind {
    Type(Type),
    None,
}

// #[derive(Hash, Eq, PartialEq)]
// struct Func {
//     boss: BossKind,
//     path: PathEx,
//     decl: Option<syntax::ast::FnDecl>,
// }

#[derive(Hash, Eq, PartialEq)]
struct Func {
    boss: BossKind,
    path: Path,
    decl: Option<syn::item::FnArg>,
}

// impl Type {
//     fn to_str(&self) -> String {
//         let mut ret = self.path[0].clone();
//         let mut is_1st = true;
//         for seg in &self.path {
//             if is_1st {
//                 is_1st = false;
//                 continue;
//             }
//             ret.push_str(&format!("::{}", seg));
//         }
//         ret
//     }
// }

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
// impl fmt::Display for Func {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         let mut is_1st: bool = true;
//         let mut tmp = String::from(&self.path[0].0);
//         for seg in &self.path {
//             if is_1st {
//                 is_1st = false;
//                 continue;
//             }
//             tmp.push_str(&format!("::{}", seg.0));
//             if let Some(para) = &seg.1 {
//                 error!("xcxcxcxcxcxcxc : {:?}", para);
//             }
//         }
//         write!(f, "{}()", tmp)
//     }
// }

// fn get_path(path: &syntax::ast::Path) -> Path {
//     trace!("get_path: {}", pprust::path_to_string(&path));
//     let mut ret = Vec::new();
//     for segment in &path.segments {
//         let path_name = ident2string(&segment.identifier);
//         ret.push(path_name);
//     }
//     trace!("path: {:?}", ret);
//     ret
// }

// fn get_pathex(path: &syntax::ast::Path) -> PathEx {
//     trace!("get_pathex: {}", pprust::path_to_string(&path));
//     let mut ret = Vec::new();
//     for segment in &path.segments {
//         let path_name = ident2string(&segment.identifier);
//         if let Some(para) = &segment.parameters {
//             // trace!("segment: {:?}", para);
//             // trace!("segmenty: {}", utils::type_name(&para));
//         }
//         // trace!("seg ID : {}", path_name);
//         ret.push((path_name, segment.parameters.clone()));
//     }
//     trace!("path: {:?}", ret);
//     ret
// }
//fn id2str()
fn handle_item(record: &mut Record, item: &Item) {
    match item {
        Item::Fn(ItemFn {
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
            let id = format!("{}",ident).to_owned();
            trace!("Fn {}",id);
            trace!("\tdecl {} {:#?}",type_name(&decl.inputs), decl);
        }
        _ => error!("  this ItemKind is not used yet"),
    }
    //     let id = ident2string(&item.ident);
    //     info!(
    //         "======【{} {}】======",
    //         id,
    //         String::from(item.node.descriptive_variant())
    //     );
    //     match &item.node {
    //         ItemKind::Fn(p_fn_decl, unsafety, constness, abi, rgenerics, p_block) => {
    //             let f = Func {
    //                 boss: BossKind::None,
    //                 path: vec![(id.clone(), None)],
    //                 decl: Some((**p_fn_decl).clone()),
    //             };
    //             record.caller.insert(f.clone());

    //             for s in &p_block.stmts {
    //                 handle_stmt(record, &f, &s);
    //             }
    //             // =============================================
    //         }
    //         ItemKind::Impl(
    //             unsafety,
    //             impl_polarity,
    //             defaultness,
    //             generics,
    //             o_trait_ref,
    //             p_ty,
    //             vec_implitem,
    //         ) => {
    //             info!("ItemKind::Impl {:?} {:?}", p_ty, o_trait_ref);
    //             info!("{:?}", item.node);
    //             info!("{:?}", unsafety);
    //             info!("{:?}", impl_polarity);
    //             info!("{:?}", defaultness);
    //             info!("gen  :\t{:?}", generics);
    //             info!("trait:\t{:?}", o_trait_ref);
    //             info!("type :\t{:?}", p_ty);
    //             info!("impl :\t{:?}", vec_implitem);

    //             info!("type kind {:?}", p_ty.node);
    //             if let TyKind::Path(_, path) = &p_ty.node {
    //                 trace!("TyKind::Path");
    //                 let ty: Type = Type {
    //                     path: get_path(&path),
    //                 };

    //                 if !record.impls.contains_key(&ty) {
    //                     record.impls.insert(ty.clone(), Vec::new());
    //                 }

    //                 for implitem in vec_implitem {
    //                     match &implitem.node {
    //                         ImplItemKind::Method(sig, block) => {
    //                             let id = ident2string(&implitem.ident);
    //                             let f = Func {
    //                                 boss: BossKind::Type(ty.clone()),
    //                                 path: vec![(id.clone(), None)],
    //                                 decl: Some((*sig.decl).clone()),
    //                             };
    //                             record.caller.insert(f.clone());
    //                             record.impls.get_mut(&ty).unwrap().push(f.clone());
    //                             for stmt in &block.stmts {
    //                                 handle_stmt(record, &f, &stmt);
    //                             }
    //                         }
    //                         _ => error!("implitem : {:?}", implitem.node),
    //                     }
    //                 }
    //             } else {
    //                 error!("Unmatched Type Kind");
    //             }
    //         }
    //         _ => {} // error!("  this ItemKind is not used yet"),
    //     }
}

// fn handle_stmt(record: &mut Record, caller: &Func, stmt: &Stmt) {
//     info!("StmtKind :");
//     match &stmt.node {
//         StmtKind::Local(expr) => {
//             info!("Local");
//             handle_expr(record, caller, &expr.init.as_ref().unwrap());
//         }
//         StmtKind::Item(expr) => {
//             info!("[FATAL ]:It's a trap!!! {:?}", expr);
//         }
//         StmtKind::Expr(expr) => {
//             info!("expr!");
//             handle_expr(record, caller, &expr);
//         }
//         StmtKind::Semi(expr) => {
//             info!("Semi!");
//             handle_expr(record, caller, &expr);
//         }
//         StmtKind::Mac(mac) => {
//             info!("Mac!  {:?}\n{:?}", mac.0.node.path, mac.0.node.tts);
//         }
//     };
// }

// fn handle_expr(record: &mut Record, caller: &Func, expr: &syntax::ast::Expr) {
//     let node = &expr.node;

//     match node {
//         ExprKind::Box(expr) => {
//             trace!("[Box ]");
//             handle_expr(record, caller, expr);
//         }
//         ExprKind::Array(vec_expr) => {
//             trace!("[Array ]");
//             for expr in vec_expr {
//                 handle_expr(record, caller, expr);
//             }
//         }
//         ExprKind::Call(func, args) => {
//             trace!("【Call 】{} : ", args.len());
//             match &func.node {
//                 ExprKind::Path(_, path) => {
//                     let f = Func {
//                         boss: BossKind::None,
//                         path: get_pathex(&path),
//                         decl: None,
//                     };
//                     record.callee.insert(f.clone());
//                     record.called.insert((caller.clone(), f));
//                 }
//                 _ => {
//                     warn!("Something Wrong Happened{}", utils::type_name(&**func));
//                 }
//             }
//             for expr in args {
//                 handle_expr(record, caller, expr);
//             }
//             trace!("【/Call 】");
//         }
//         ExprKind::MethodCall(span, vec_ty, vec_expr) => {
//             trace!("【MCall 】{:?} : ", ident2string(&span.node));
//             for expr in vec_expr {
//                 handle_expr(record, caller, expr);
//             }
//         }
//         ExprKind::Tup(vec_expr) => {
//             trace!("[Tup  ]");
//             for expr in vec_expr {
//                 handle_expr(record, caller, expr);
//             }
//         }
//         ExprKind::Binary(binop, lhs, rhs) => {
//             trace!("[Binary]");
//             handle_expr(record, caller, lhs);
//             handle_expr(record, caller, rhs);
//         }
//         ExprKind::Unary(un_op, expr) => {
//             trace!("[Unary ]");
//             handle_expr(record, caller, expr);
//         }
//         ExprKind::IfLet(_pat, expr, block, label) => {
//             trace!("[IfLet  ]");
//             handle_expr(record, caller, expr);
//             for stmt in &block.stmts {
//                 handle_stmt(record, caller, &stmt);
//             }
//         }
//         ExprKind::Lit(lit) => trace!("[Lit   ]: {:?}", lit.node),
//         ExprKind::If(expr, block, eelse) => {
//             trace!("[If!  ]");
//             handle_expr(record, caller, expr);
//             for stmt in &block.stmts {
//                 handle_stmt(record, caller, &stmt);
//             }
//             if let Some(expr) = eelse {
//                 trace!("[else ]");
//                 handle_expr(record, caller, expr);
//             }
//         }
//         ExprKind::Cast(expr, _ty) => {
//             trace!("[Cast  ]");
//             handle_expr(record, caller, expr);
//         }
//         // ExprKind::Type(expr, P<Ty>)=>{}
//         ExprKind::While(expr, block, o_lable) => {
//             trace!("[While ]");
//             handle_expr(record, caller, expr);
//             for stmt in &block.stmts {
//                 handle_stmt(record, caller, &stmt);
//             }
//             if let Some(lable) = o_lable {
//                 warn!("[lable!!!!! ] {:?}", lable);
//             }
//         }
//         ExprKind::ForLoop(_pat, expr, block, o_lable) => {
//             trace!(
//                 "[ForLoop] {:?}\n{:?}\n{:?}\n{:?}\n{:?}",
//                 node,
//                 _pat,
//                 expr,
//                 block,
//                 o_lable
//             );
//             handle_expr(record, caller, expr);
//             for stmt in &block.stmts {
//                 handle_stmt(record, caller, &stmt);
//             }
//             if let Some(lable) = o_lable {
//                 warn!("[lable!!!!! ] {:?}", lable);
//             }
//         }
//         ExprKind::Loop(block, o_lable) => {
//             for stmt in &block.stmts {
//                 handle_stmt(record, caller, &stmt);
//             }
//             if let Some(lable) = o_lable {
//                 warn!("[lable!!!!! ] {:?}", lable);
//             }
//         }
//         ExprKind::Match(expr, vec_arm) => {
//             trace!("[Match]:");
//             handle_expr(record, caller, expr);
//             for arm in vec_arm {
//                 trace!("arm: {:?}   ", arm.body);
//                 handle_expr(record, caller, &arm.body);
//             }
//         }
//         // ExprKind::Closure(CaptureBy, IsAsync, Movability, fn_decl, expr, Span) => {}
//         ExprKind::Block(block) => {
//             trace!("[Block]:"); // TODO: check here, i feel somthing.
//             for stat in &block.stmts {
//                 handle_stmt(record, caller, &stat);
//             }
//             trace!("[/Block]:");
//         }
//         // ExprKind::Async(CaptureBy, NodeId, block) => {}
//         // ExprKind::Await(AwaitOrigin, expr) => {}
//         ExprKind::Assign(lhs, rhs) => {
//             trace!("[Assign ]");
//             handle_expr(record, caller, rhs);
//         }
//         ExprKind::AssignOp(bin_op, lhs, rhs) => {
//             trace!("[AssignOp]:{:?}", bin_op);
//             handle_expr(record, caller, rhs);
//         }
//         // ExprKind::Field(expr, Ident) => {}
//         // ExprKind::Index(expr1, expr2) => {}
//         // ExprKind::Range(o_expr1, o_expr2, RangeLimits) => {}
//         ExprKind::Path(_, path) => {
//             trace!("[Path   ]: {:?}", pprust::path_to_string(path));
//         }
//         ExprKind::AddrOf(_, expr) => {
//             trace!("[AddrOf]: {:?}", expr);
//             handle_expr(record, caller, expr);
//         }
//         // ExprKind::Break(o_lable, o_expr) => {}
//         // ExprKind::Continue(o_lable) => {}
//         ExprKind::Ret(o_expr) => {
//             if let Some(expr) = o_expr {
//                 trace!("[Ret  ]: {:?}", expr);
//                 handle_expr(record, caller, expr);
//             }
//         }
//         // ExprKind::InlineAsm(inlineAsm) => {}
//         // ExprKind::Mac(Mac) => {}
//         ExprKind::Struct(path, vec_field, o_expr) => {
//             trace!("[Struct ]:");
//             for field in vec_field {
//                 handle_expr(record, caller, &field.expr);
//             }
//             if let Some(e) = o_expr {
//                 trace!("something magical happend! {:?}", e);
//             }
//         }
//         // ExprKind::Repeat(expr, AnonConst) => {}
//         // ExprKind::Paren(expr) => {}
//         ExprKind::Try(expr) => {
//             trace!("[Try   ]:");
//             handle_expr(record, caller, expr);
//         }
//         // ExprKind::Yield(o_expr) => {}
//         // ExprKind::Err,
//         _ => trace!("[______]: {:?}", node),
//     }
// }

struct Record {
    impls: HashMap<Type, Vec<Func>>,
    caller: HashSet<Func>,
    called: HashSet<(Func, Func)>,
    callee: HashSet<Func>,
}

// fn mangle_type(ty: &Type) -> String {
//     let mut ret = String::from("_ZT");
//     for seg in &ty.path {
//         if seg == "{{root}}" {
//             ret.push_str(&format!("4root"));
//         }
//         ret.push_str(&format!("{}{}", seg.len(), seg));
//     }
//     ret.push('E');
//     ret
// }
// fn mangle_func(func: &Func) -> String {
//     let mut ret = String::from("_ZN");
//     match &func.boss {
//         BossKind::Type(ty) => {
//             for seg in &ty.path {
//                 ret.push_str(&format!("{}{}", seg.len(), seg));
//             }
//         }
//         BossKind::None => {}
//     }
//     for seg in &func.path {
//         if seg.0 == "{{root}}" {
//             ret.push_str(&format!("4root"));
//         } else {
//             ret.push_str(&format!("{}{}", seg.0.len(), seg.0));
//         }
//     }
//     ret.push('E');
//     ret
// }
// fn decl_dot(decl: &syntax::ast::FnDecl) -> String {
//     let mut ret = String::from("");
//     ret = ret
//         + "\n\t\t| "
//         + &match &decl.output {
//             syntax::ast::FunctionRetTy::Default(_) => String::from(" "),
//             syntax::ast::FunctionRetTy::Ty(ty) => {
//                 (&pprust::ty_to_string(&ty)).replace("&", "&amp;")
//             }
//         };
//     for input in &decl.inputs {
//         ret = ret + "\n\t\t| " + &(&pprust::ty_to_string(&input.ty)).replace("&", "&amp;");
//     }
//     ret
// }
// fn generate_dot(record: &Record) {
//     let mut src_dot = String::from("digraph demo{\n\trankdir=LR\n");

//     for callee in &record.callee {
//         src_dot += &format!(
//             "\t{}[shape = record label = <<B>{}</B>>];\n",
//             mangle_func(&callee),
//             callee
//         );
//     }
//     for caller in &record.caller {
//         src_dot += &format!(
//             "\t{}[shape = record label = <<B> {} </B>{}>];\n",
//             mangle_func(&caller),
//             caller,
//             decl_dot(&(caller.decl.clone().unwrap())),
//         );
//     }

//     // cluster the `impl` block
//     for pair in &record.impls {
//         src_dot += &format!(
//             "\tsubgraph cluster{} {}\n\
//              \t\tstyle = \"bold\"\n\
//              \t\t{}[shape = none, label = \"{}\"]\n",
//             mangle_type(&pair.0),
//             '{',
//             mangle_type(&pair.0),
//             pair.0.to_str()
//         );
//         for func in pair.1 {
//             src_dot += &format!("\t\t{};\n", mangle_func(&func));
//         }
//         src_dot += &format!("\t{}\n", '}');
//     }

//     // edge: caller -> callee
//     for tuple in &record.called {
//         src_dot += &format!("\t{}->{};\n", mangle_func(&tuple.0), mangle_func(&tuple.1));
//     }

//     // end of the graph
//     src_dot += "}";

//     write_to_file(src_dot);
// }
// fn write_to_file(src_dot: String) {
//     let path: &str = "output.dot";
//     let mut output: File = File::create(path).unwrap();
//     let res = write!(output, "{}", src_dot);
//     let input: File = File::open(path).unwrap();
//     let buffered: BufReader<File> = BufReader::new(input);
// }
// fn gen_callgraph(contents: &String) -> Record {
//     let parse_session = ParseSess::new(FilePathMapping::empty());

//     let mut parser =
//         parse::new_parser_from_source_str(&parse_session, String::new(), String::from(contents));
//     let result = parser.parse_crate_mod();

//     let mut record: Record = Record {
//         impls: HashMap::new(),
//         caller: HashSet::new(),
//         called: HashSet::new(),
//         callee: HashSet::new(),
//     };

//     if let Ok(v) = result {
//         for i in &v.module.items {
//             handle_item(&mut record, &i);
//         }
//     }
//     return record;
// }
fn get_file() -> String {
    let args: Vec<String> = env::args().collect();
    let filename = args[1].as_str();
    fs::read_to_string(filename).expect("Something went wrong reading the file")
}

use syn::*;
use syn::{parse_macro_input, DeriveInput};

// pub struct File {
//     pub shebang: Option<String>,
//     pub attrs: Vec<Attribute>,
//     pub items: Vec<Item>,
// }
fn main() {
    pretty_env_logger::init();

    let mut record: Record = Record {
        impls: HashMap::new(),
        caller: HashSet::new(),
        called: HashSet::new(),
        callee: HashSet::new(),
    };
    let mut file = File::open("LEGENDtmp.rs").expect("Unable to open file");

    let mut src = String::new();
    file.read_to_string(&mut src).expect("Unable to read file");

    let syntax = syn::parse_file(&src).expect("Unable to parse file");
    for item in &syntax.items {
        handle_item(&mut record, &item);
    }

    //println!("{:#?}",syntax.items[0] );

    // println!("{:#?}", syntax);
}

// macro_rules! export_token_macro {
//     ($($dollar:tt)*) => {
//         /// A type-macro that expands to the name of the Rust type representation of a
//         /// given token.
//         ///
//         /// See the [token module] documentation for details and examples.
//         ///
//         /// [token module]: token/index.html
//         // Unfortunate duplication due to a rustdoc bug.
//         // https://github.com/rust-lang/rust/issues/45939
//         #[macro_export]
//         macro_rules! Token {
//             (abstract)    => { $crate::token::Abstract };
//             (as)          => { $crate::token::As };
//             (async)       => { $crate::token::Async };
//             (auto)        => { $crate::token::Auto };
//             (become)      => { $crate::token::Become };
//             (box)         => { $crate::token::Box };
//             (break)       => { $crate::token::Break };
//             (const)       => { $crate::token::Const };
//             (continue)    => { $crate::token::Continue };
//             (crate)       => { $crate::token::Crate };
//             (default)     => { $crate::token::Default };
//             (do)          => { $crate::token::Do };
//             (dyn)         => { $crate::token::Dyn };
//             (else)        => { $crate::token::Else };
//             (enum)        => { $crate::token::Enum };
//             (existential) => { $crate::token::Existential };
//             (extern)      => { $crate::token::Extern };
//             (final)       => { $crate::token::Final };
//             (fn)          => { $crate::token::Fn };
//             (for)         => { $crate::token::For };
//             (if)          => { $crate::token::If };
//             (impl)        => { $crate::token::Impl };
//             (in)          => { $crate::token::In };
//             (let)         => { $crate::token::Let };
//             (loop)        => { $crate::token::Loop };
//             (macro)       => { $crate::token::Macro };
//             (match)       => { $crate::token::Match };
//             (mod)         => { $crate::token::Mod };
//             (move)        => { $crate::token::Move };
//             (mut)         => { $crate::token::Mut };
//             (override)    => { $crate::token::Override };
//             (priv)        => { $crate::token::Priv };
//             (pub)         => { $crate::token::Pub };
//             (ref)         => { $crate::token::Ref };
//             (return)      => { $crate::token::Return };
//             (Self)        => { $crate::token::SelfType };
//             (self)        => { $crate::token::SelfValue };
//             (static)      => { $crate::token::Static };
//             (struct)      => { $crate::token::Struct };
//             (super)       => { $crate::token::Super };
//             (trait)       => { $crate::token::Trait };
//             (try)         => { $crate::token::Try };
//             (type)        => { $crate::token::Type };
//             (typeof)      => { $crate::token::Typeof };
//             (union)       => { $crate::token::Union };
//             (unsafe)      => { $crate::token::Unsafe };
//             (unsized)     => { $crate::token::Unsized };
//             (use)         => { $crate::token::Use };
//             (virtual)     => { $crate::token::Virtual };
//             (where)       => { $crate::token::Where };
//             (while)       => { $crate::token::While };
//             (yield)       => { $crate::token::Yield };
//             (+)           => { $crate::token::Add };
//             (+=)          => { $crate::token::AddEq };
//             (&)           => { $crate::token::And };
//             (&&)          => { $crate::token::AndAnd };
//             (&=)          => { $crate::token::AndEq };
//             (@)           => { $crate::token::At };
//             (!)           => { $crate::token::Bang };
//             (^)           => { $crate::token::Caret };
//             (^=)          => { $crate::token::CaretEq };
//             (:)           => { $crate::token::Colon };
//             (::)          => { $crate::token::Colon2 };
//             (,)           => { $crate::token::Comma };
//             (/)           => { $crate::token::Div };
//             (/=)          => { $crate::token::DivEq };
//             (.)           => { $crate::token::Dot };
//             (..)          => { $crate::token::Dot2 };
//             (...)         => { $crate::token::Dot3 };
//             (..=)         => { $crate::token::DotDotEq };
//             (=)           => { $crate::token::Eq };
//             (==)          => { $crate::token::EqEq };
//             (>=)          => { $crate::token::Ge };
//             (>)           => { $crate::token::Gt };
//             (<=)          => { $crate::token::Le };
//             (<)           => { $crate::token::Lt };
//             (*=)          => { $crate::token::MulEq };
//             (!=)          => { $crate::token::Ne };
//             (|)           => { $crate::token::Or };
//             (|=)          => { $crate::token::OrEq };
//             (||)          => { $crate::token::OrOr };
//             (#)           => { $crate::token::Pound };
//             (?)           => { $crate::token::Question };
//             (->)          => { $crate::token::RArrow };
//             (<-)          => { $crate::token::LArrow };
//             (%)           => { $crate::token::Rem };
//             (%=)          => { $crate::token::RemEq };
//             (=>)          => { $crate::token::FatArrow };
//             (;)           => { $crate::token::Semi };
//             (<<)          => { $crate::token::Shl };
//             (<<=)         => { $crate::token::ShlEq };
//             (>>)          => { $crate::token::Shr };
//             (>>=)         => { $crate::token::ShrEq };
//             (*)           => { $crate::token::Star };
//             (-)           => { $crate::token::Sub };
//             (-=)          => { $crate::token::SubEq };
//             (~)           => { $crate::token::Tilde };
//             (_)           => { $crate::token::Underscore };
//             $($dollar     => { $crate::token::Dollar };)*
//         }
//     };
// }

// fn main() {
//     pretty_env_logger::init();

//     let contents = get_file();
//     generate_dot(&gen_callgraph(&contents));
// }
