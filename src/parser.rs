
use regex::Regex;
use sexp::Atom::*;
use sexp::*;


#[derive(Debug)]
pub enum Op1 { Add1, Sub1, IsNum, IsBool, IsVec, IsNonNilVec, IsOverflow, Print, }

#[derive(Debug)]
pub enum Op2 { Plus, Minus, Times, Equal, Greater, GreaterEqual, Less, LessEqual, }

#[derive(Debug)]
pub enum Defn {
    Func(String, Vec<String>, Box<Expr>),
}


#[derive(Debug)]
pub enum Expr {
    Number(i64),
    Boolean(bool),
    Id(String),
    Nil,
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    Input,
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
    Call(String, Vec<Expr>),
    Pair(Box<Expr>, Box<Expr>),
    Vec(Vec<Expr>),
    MakeVec(Box<Expr>, Box<Expr>),
    VecGet(Box<Expr>, Box<Expr>),
    VecSet(Box<Expr>, Box<Expr>, Box<Expr>),
    VecLen(Box<Expr>),
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
    "vector" => Expr::UnOp(Op1::IsVec, Box::new(parse_expr(e))),
    "nonnilvector" => Expr::UnOp(Op1::IsNonNilVec, Box::new(parse_expr(e))),
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
                      "nil" => Expr::Nil,
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
                [Sexp::Atom(S(op)), e] if op == "print" => Expr::UnOp(Op1::Print, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "vec-len" => Expr::VecLen(Box::new(bind_typecheck(e, "nonnilvector"))),
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => check_numbound(Expr::BinOp(Op2::Plus, Box::new(bind_typecheck(e1,"numeric")), Box::new(bind_typecheck(e2, "numeric")))),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => check_numbound(Expr::BinOp(Op2::Minus, Box::new(bind_typecheck(e1,"numeric")), Box::new(bind_typecheck(e2, "numeric")))),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(bind_typecheck(e1,"numeric")), Box::new(bind_typecheck(e2, "numeric"))),
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(Op2::Less, Box::new(bind_typecheck(e1,"numeric")), Box::new(bind_typecheck(e2, "numeric"))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(Op2::Greater, Box::new(bind_typecheck(e1,"numeric")), Box::new(bind_typecheck(e2, "numeric"))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(bind_typecheck(e1,"numeric")), Box::new(bind_typecheck(e2, "numeric"))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(bind_typecheck(e1,"numeric")), Box::new(bind_typecheck(e2, "numeric"))),
                [Sexp::Atom(S(op)), e1, e2] if op == "pair" => Expr::Pair(Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "make-vec" => Expr::MakeVec(Box::new(bind_typecheck(e1,"numeric")), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "vec-get" => Expr::VecGet(Box::new(bind_typecheck(e1, "nonnilvector")), Box::new(bind_typecheck(e2, "numeric"))),
                [Sexp::Atom(S(op)), e1, e2, e3] if op == "vec-set!" => Expr::VecSet(Box::new(bind_typecheck(e1, "nonnilvector")), Box::new(bind_typecheck(e2, "numeric")), Box::new(parse_expr(e3))),
                [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" => Expr::If(Box::new(bind_typecheck(e1,"boolean")), Box::new(parse_expr(e2)), Box::new(parse_expr(e3))),
                [Sexp::Atom(S(op)), Sexp::List(e1), e2] if op == "let" =>  Expr::Let(parse_bind(e1), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), Sexp::Atom(S(s)), e] if op == "set!" => Expr::Set(id_preprocess(s), Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), ..] if op == "block" => Expr::Block(parse_block(vec)),
                [Sexp::Atom(S(op)), ..] if op == "vec" => Expr::Vec(parse_block(vec)),
                [Sexp::Atom(S(op)), ..] => Expr::Call(id_preprocess(op), parse_block(vec)),
                _ => panic!("Expr Invalid: Sexp"),
            }
        },
    }
}

fn parse_fun_args(s: &Sexp) -> Option<Vec<String>> {
    match s {
        Sexp::List(vec) => {
            if vec.len() > 1 && vec[1..].iter().all(|s| matches!(s, Sexp::Atom(_))) {
                Some(vec[1..].iter().map(|s| {
                    if let Sexp::Atom(ref s) = s {
                        id_preprocess(s.to_string().as_str())
                    } else {
                        unreachable!() 
                    }
                }).collect())
            } else {
                None
            }
        },
        _ => panic!("Function Invalid: expr is not a list"),
    }
}

fn parse_fun_name(s: &Sexp) -> Option<String> {
    if let Sexp::List(vec) = s {
        if let Some(Sexp::Atom(name)) = vec.first() {
            return Some(name.to_string()); // Clone the string to return
        }
    }
    None // Return None if the structure is invalid
}

pub fn parse_def(s: &Sexp) -> Defn {
    match s {
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), e1, e2] if op == "fun" => Defn::Func(parse_fun_name(e1).unwrap(), parse_fun_args(e1).unwrap(), Box::new(parse_expr(e2))),
                _ => panic!("Definition invalid")
            }
        },
        _ => panic!("Definition Invalid")
    }
}