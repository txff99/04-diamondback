
use regex::Regex;
use sexp::Atom::*;
use sexp::*;


#[derive(Debug)]
pub enum Op1 { Add1, Sub1, IsNum, IsBool, IsOverflow}

#[derive(Debug)]
pub enum Op2 { Plus, Minus, Times, Equal, Greater, GreaterEqual, Less, LessEqual, }

pub enum Defn {
    Name(Vec<String>),
}

#[derive(Debug)]
pub enum Expr {
    Number(i64),
    Boolean(bool),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    Input,
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
}

fn parse_bind(bindings: &Vec<Sexp>) -> Vec<(String, Expr)> {
    let mut ret = Vec::new();
    for b in bindings {
        match b {
            Sexp::List(vec) => {
                match &vec[..] {
                    [Sexp::Atom(S(id1)), e] => ret.push((id1.to_string(), parse_expr(e))),
                    _ => panic!("Expr Invalid: id can't be bound"),
                }
            },
            _ => panic!("Expr Invalid: expr is not a list"),
        }
    }
    ret
}

fn parse_block(sexps: &Vec<Sexp>) -> Vec<Expr> {
  let mut ret = Vec::new();
  for s in sexps[1..].iter() {
    ret.push(parse_expr(s));
  }
  ret
}

fn id_preprocess(e: &str) -> String {
  let keywords: &[&str] = &["if", "loop", "break", "set!", "block", "let", "true", "false", "input"];
  if keywords.contains(&e) {
    panic!("identifier {} is keyword", e);
  }
  let re = Regex::new(r"^[a-zA-Z][a-zA-Z0-9]*$").unwrap();
  assert!(re.is_match(e), "Identifier is Invalid");
  e.to_string()
}

fn bind_typecheck(e: &Sexp, t: &str) -> Expr {
  match t {
    "numeric" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
    "boolean" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
    _ => panic!("type check only support numeric and boolean"),
  } 
}

fn check_numbound(e: Expr) -> Expr {
    Expr::UnOp(Op1::IsOverflow, Box::new(e))
}

pub fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(n) => {
            match n {
                Atom::I(e) => Expr::Number(i64::try_from(*e).unwrap()),
                Atom::S(e) => {
                    match e.as_str() {
                      "true" => Expr::Boolean(true),
                      "false" => Expr::Boolean(false),
                      "input" => Expr::Input,
                      _ => Expr::Id(id_preprocess(e.as_str()))
                    }
                },
                Atom::F(_) => panic!("Expr Invalid: does not support float type"),
            }
        },
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" => check_numbound(Expr::UnOp(Op1::Add1, Box::new(bind_typecheck(e,"numeric")))),
                [Sexp::Atom(S(op)), e] if op == "sub1" => check_numbound(Expr::UnOp(Op1::Sub1, Box::new(bind_typecheck(e, "numeric")))),
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => check_numbound(Expr::BinOp(Op2::Plus, Box::new(bind_typecheck(e1,"numeric")), Box::new(bind_typecheck(e2, "numeric")))),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => check_numbound(Expr::BinOp(Op2::Minus, Box::new(bind_typecheck(e1,"numeric")), Box::new(bind_typecheck(e2, "numeric")))),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(bind_typecheck(e1,"numeric")), Box::new(bind_typecheck(e2, "numeric"))),
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(Op2::Less, Box::new(bind_typecheck(e1,"numeric")), Box::new(bind_typecheck(e2, "numeric"))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(Op2::Greater, Box::new(bind_typecheck(e1,"numeric")), Box::new(bind_typecheck(e2, "numeric"))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(bind_typecheck(e1,"numeric")), Box::new(bind_typecheck(e2, "numeric"))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(bind_typecheck(e1,"numeric")), Box::new(bind_typecheck(e2, "numeric"))),
                [Sexp::Atom(S(op)), Sexp::List(e1), e2] if op == "let" =>  Expr::Let(parse_bind(e1), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" => Expr::If(Box::new(bind_typecheck(e1,"boolean")), Box::new(parse_expr(e2)), Box::new(parse_expr(e3))),
                [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), Sexp::Atom(S(s)), e] if op == "set!" => Expr::Set(id_preprocess(s), Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), ..] if op == "block" => Expr::Block(parse_block(vec)),
                _ => panic!("Expr Invalid: Sexp"),
            }
        },
    }
}

pub fn parse_def(s: &Sexp) -> Defn {
    todo!()
}