use std::env;
use std::fs::File;
use std::io::prelude::*;

use regex::Regex;
use sexp::Atom::*;
use sexp::*;

use im::HashMap;
use rand::{thread_rng, Rng};
use rand::distributions::Alphanumeric;

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i64),
    Label(String),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RCX,
    RDX,
    R8,
    RSP,
    RBP,
    RDI,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val),
    IPush(Val),
    IPop(Val),
    IAnd(Val, Val),
    IJE(Val),
    IJNE(Val),
    IJMP(Val),
    ICMP(Val, Val),
    ISETE(()),
    ISETL(()),
    ISETLE(()),
    ISETG(()),
    ISETGE(()),
    ISAL(Val, Val),
    ISAR(Val, Val),
    ILABEL(Val),
    IJO(Val),
}

#[derive(Debug)]
enum Op1 { Add1, Sub1, IsNum, IsBool, IsOverflow}

#[derive(Debug)]
enum Op2 { Plus, Minus, Times, Equal, Greater, GreaterEqual, Less, LessEqual, }

#[derive(Debug)]
enum Expr {
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

fn generate_label(prefix: &str) -> String {
    let random_suffix: String = thread_rng()
        .sample_iter(&Alphanumeric)
        .take(6)  // Generate a 6-character random suffix
        .map(char::from)
        .collect();

    format!("{}_{}", prefix, random_suffix)
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

fn parse_expr(s: &Sexp) -> Expr {
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

fn compile_to_instrs(e: &Expr, var_map: &HashMap<String, i64>, loop_end: &str) -> Vec<Instr> {
    match e {
        Expr::Number(n) => {
            if *n < -4611686018427387904 || *n > 4611686018427387903 {
                panic!("an error ocurred: overflow");
            }
            let mut instrs = vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n))];
            instrs.push(Instr::ISAL(Val::Reg(Reg::RAX), Val::Imm(1)));
            instrs
        },
        Expr::Boolean(b) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(if *b {0b11} else {0b01}))],
        Expr::Input => {
            let instrs = vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))];
            instrs
        },
        Expr::Id(id) => {
            assert!(var_map.contains_key(id), "{}", format!("Unbound variable identifier {}", id));
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RBP, var_map[id]*8))]
        }
        Expr::UnOp(op1, subexpr) => {
            match op1 {
                Op1::Add1 => {
                  let mut instrs = compile_to_instrs(subexpr, var_map, loop_end);
                  instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(0b10)));
                  instrs
                },
                Op1::Sub1 => {
                  let mut instrs = compile_to_instrs(subexpr, var_map, loop_end);
                  instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(0b10)));
                  instrs
                },
                /* for typecheck we use the least significant bit for tag: 0 for number, 1 for bool */
                Op1::IsBool => {
                  let mut instrs = compile_to_instrs(subexpr, var_map, loop_end);
                  instrs.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
                  instrs.push(Instr::IAnd(Val::Reg(Reg::RCX), Val::Imm(0x1)));
                  instrs.push(Instr::ICMP(Val::Reg(Reg::RCX), Val::Imm(0x1)));
                  instrs.push(Instr::IJNE(Val::Label("invalid_arg_handler".to_string())));
                  instrs
                },
                Op1::IsNum => {
                  let mut instrs = compile_to_instrs(subexpr, var_map, loop_end);
                  instrs.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
                  instrs.push(Instr::IAnd(Val::Reg(Reg::RCX), Val::Imm(0x1)));
                  instrs.push(Instr::ICMP(Val::Reg(Reg::RCX), Val::Imm(0x1)));
                  instrs.push(Instr::IJE(Val::Label("invalid_arg_handler".to_string())));
                  instrs
                },
                Op1::IsOverflow => {
                    let mut instrs = compile_to_instrs(subexpr, var_map, loop_end);
                    // instrs.push(Instr::IJC(Val::Label("overflow_handler".to_string())));
                    instrs.push(Instr::IJO(Val::Label("overflow_handler".to_string())));
                    instrs
                }
            }
        },
        Expr::BinOp(op2, e1, e2) => {
            match op2 {
                Op2::Plus => {
                    let mut instrs = compile_to_instrs(e2, var_map, loop_end);
                    instrs.push(Instr::IPush(Val::Reg(Reg::RAX)));
                    instrs.extend(compile_to_instrs(e1, var_map, loop_end));
                    instrs.push(Instr::IPop(Val::Reg(Reg::RCX)));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                    instrs
                },
                Op2::Minus => {
                    let mut instrs = compile_to_instrs(e2, var_map, loop_end);
                    instrs.push(Instr::IPush(Val::Reg(Reg::RAX)));
                    instrs.extend(compile_to_instrs(e1, var_map, loop_end));
                    instrs.push(Instr::IPop(Val::Reg(Reg::RCX)));
                    instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                    instrs
                },
                Op2::Times => {
                  let mut instrs = compile_to_instrs(e2, var_map, loop_end);
                  instrs.push(Instr::IPush(Val::Reg(Reg::RAX)));
                  instrs.extend(compile_to_instrs(e1, var_map, loop_end));
                  instrs.push(Instr::IPop(Val::Reg(Reg::RCX)));
                  instrs.push(Instr::IMul(Val::Reg(Reg::RCX)));
                  /* check num bound */
                  instrs.push(Instr::IJO(Val::Label("overflow_handler".to_string())));
                    
                  /* when doing times we have to manually right shift */
                  instrs.push(Instr::ISAR(Val::Reg(Reg::RAX), Val::Imm(1)));
                  instrs
                },
                Op2::Equal => {
                    let mut instrs = compile_to_instrs(e2, var_map, loop_end);
                    instrs.push(Instr::IPush(Val::Reg(Reg::RAX)));
                    instrs.extend(compile_to_instrs(e1, var_map, loop_end));
                    instrs.push(Instr::IPop(Val::Reg(Reg::RCX)));
                    
                    /* check if type matches each other */
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::R8), Val::Reg(Reg::RCX)));
                    instrs.push(Instr::IAnd(Val::Reg(Reg::RDX), Val::Imm(0x1)));
                    instrs.push(Instr::IAnd(Val::Reg(Reg::R8), Val::Imm(0x1)));
                    instrs.push(Instr::ICMP(Val::Reg(Reg::RDX), Val::Reg(Reg::R8)));
                    instrs.push(Instr::IJNE(Val::Label("invalid_arg_handler".to_string())));
                    
                    instrs.push(Instr::ICMP(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                    instrs.push(Instr::ISETE(()));
                    instrs.push(Instr::ISAL(Val::Reg(Reg::RAX),Val::Imm(1)));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));

                    instrs
                }
                Op2::Less => {
                    let mut instrs = compile_to_instrs(e2, var_map, loop_end);
                    instrs.push(Instr::IPush(Val::Reg(Reg::RAX)));
                    instrs.extend(compile_to_instrs(e1, var_map, loop_end));
                    instrs.push(Instr::IPop(Val::Reg(Reg::RCX)));
                    
                    instrs.push(Instr::ICMP(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                    instrs.push(Instr::ISETL(()));
                    instrs.push(Instr::ISAL(Val::Reg(Reg::RAX),Val::Imm(1)));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs
                },
                Op2::LessEqual => {
                    let mut instrs = compile_to_instrs(e2, var_map, loop_end);
                    instrs.push(Instr::IPush(Val::Reg(Reg::RAX)));
                    instrs.extend(compile_to_instrs(e1, var_map, loop_end));
                    instrs.push(Instr::IPop(Val::Reg(Reg::RCX)));
                    
                    instrs.push(Instr::ICMP(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                    instrs.push(Instr::ISETLE(()));
                    instrs.push(Instr::ISAL(Val::Reg(Reg::RAX),Val::Imm(1)));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs
                },
                Op2::Greater => {
                    let mut instrs = compile_to_instrs(e2, var_map, loop_end);
                    instrs.push(Instr::IPush(Val::Reg(Reg::RAX)));
                    instrs.extend(compile_to_instrs(e1, var_map, loop_end));
                    instrs.push(Instr::IPop(Val::Reg(Reg::RCX)));
                    
                    instrs.push(Instr::ICMP(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                    instrs.push(Instr::ISETG(()));
                    instrs.push(Instr::ISAL(Val::Reg(Reg::RAX),Val::Imm(1)));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs
                },
                Op2::GreaterEqual => {
                    let mut instrs = compile_to_instrs(e2, var_map, loop_end);
                    instrs.push(Instr::IPush(Val::Reg(Reg::RAX)));
                    instrs.extend(compile_to_instrs(e1, var_map, loop_end));
                    instrs.push(Instr::IPop(Val::Reg(Reg::RCX)));
                    
                    instrs.push(Instr::ICMP(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                    instrs.push(Instr::ISETGE(()));
                    instrs.push(Instr::ISAL(Val::Reg(Reg::RAX),Val::Imm(1)));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs
                }
            }
        },
        Expr::Let(vec, subexpr) => {
            let mut var_to_regoffset: HashMap<String, i64> = HashMap::new();
            let mut instrs = Vec::new();
            instrs.push(Instr::IPush(Val::Reg(Reg::RBP)));
            instrs.push(Instr::IMov(Val::Reg(Reg::RBP), Val::Reg(Reg::RSP)));
            instrs.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm((vec.len() as i64)*8)));
            for (i, tp) in vec.iter().enumerate() {
                let index = i as i64;
                let (id, expr) = tp;
                assert!(!var_to_regoffset.contains_key(id), "Duplicate binding");
                instrs.extend(compile_to_instrs(expr, &var_to_regoffset, loop_end));
                instrs.push(Instr::IMov(Val::RegOffset(Reg::RBP, (index+1)*8), Val::Reg(Reg::RAX)));
                var_to_regoffset.insert(id.to_string(), index+1);
            }
            instrs.extend(compile_to_instrs(subexpr, &var_to_regoffset, loop_end));
            instrs.push(Instr::IMov(Val::Reg(Reg::RSP), Val::Reg(Reg::RBP)));
            instrs.push(Instr::IPop(Val::Reg(Reg::RBP)));
            instrs
        },
        Expr::If(e1, e2, e3) => {
            let mut instrs = Vec::new();
            instrs.extend(compile_to_instrs(e1, var_map, loop_end));
            instrs.push(Instr::ICMP(Val::Reg(Reg::RAX), Val::Imm(0b11)));
            let else_label = generate_label("else");
            let end_label = generate_label("end");
            instrs.push(Instr::IJNE(Val::Label(else_label.clone())));
            instrs.extend(compile_to_instrs(e2, var_map, loop_end));
            instrs.push(Instr::IJMP(Val::Label(end_label.clone())));
            instrs.push(Instr::ILABEL(Val::Label(else_label.clone())));
            instrs.extend(compile_to_instrs(e3, var_map, loop_end));
            instrs.push(Instr::IJMP(Val::Label(end_label.clone())));
            instrs.push(Instr::ILABEL(Val::Label(end_label.clone())));
            instrs
        }
        Expr::Loop(e) => {
            let loop_start = generate_label("loop_start");
            let loop_end = generate_label("loop_end");
            let mut instrs = Vec::new();
            instrs.push(Instr::ILABEL(Val::Label(loop_start.clone())));
            instrs.extend(compile_to_instrs(e, var_map, &loop_end));
            instrs.push(Instr::IJMP(Val::Label(loop_start.clone())));
            instrs.push(Instr::ILABEL(Val::Label(loop_end.clone())));
            instrs
        },
        Expr::Break(e) => {
            assert!(loop_end!="", "break outside of loop");
            let mut instrs = compile_to_instrs(e, var_map, loop_end);
            instrs.push(Instr::IJMP(Val::Label(loop_end.to_string())));
            instrs
        }
        Expr::Block(exprs) => {
            let mut instrs = Vec::new();
            for e in exprs {
                instrs.extend(compile_to_instrs(e, var_map, loop_end));
            }
            instrs
        }
        Expr::Set(id, e) => {
            /* todo combine this with Expr::ID instead of String */
            assert!(var_map.contains_key(id), "Unbound variable identifier: {}", id);
            let mut instrs = compile_to_instrs(e, var_map, loop_end);
            instrs.push(Instr::IMov(Val::RegOffset(Reg::RBP, var_map[id]*8), Val::Reg(Reg::RAX)));
            instrs
        }
    }
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(dst, src) => format!("\nmov {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IAdd(dst, src) => format!("\nadd {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ISub(dst, src) => format!("\nsub {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IPush(src) => format!("\npush {}", val_to_str(src)),
        Instr::IPop(src) => format!("\npop {}", val_to_str(src)),
        Instr::IMul(src) => format!("\nmul {}", val_to_str(src)),
        Instr::IAnd(dst, src) => format!("\nand {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ICMP(dst, src) => format!("\ncmp {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IJE(label) => format!("\nje {}", val_to_str(label)),
        Instr::IJNE(label) => format!("\njne {}", val_to_str(label)),
        Instr::IJMP(label) => format!("\njmp {}", val_to_str(label)),
        Instr::ILABEL(label) => format!("\n{}:",val_to_str(label)),
        Instr::ISAL(dst, bits) => format!("\nsal {}, {}", val_to_str(dst), val_to_str(bits)),
        Instr::ISAR(dst, bits) => format!("\nsar {}, {}", val_to_str(dst), val_to_str(bits)),
        Instr::ISETE(_) => format!("\nsete al\nmovzx rax, al"),
        Instr::ISETL(_) => format!("\nsetl al\nmovzx rax, al"),
        Instr::ISETLE(_) => format!("\nsetle al\nmovzx rax, al"),
        Instr::ISETG(_) => format!("\nsetg al\nmovzx rax, al"),
        Instr::ISETGE(_) => format!("\nsetge al\nmovzx rax, al"),
        Instr::IJO(dst) => format!("\njo {}", val_to_str(dst)),
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(Reg::RAX) => "rax".to_string(),
        Val::Reg(Reg::RSP) => "rsp".to_string(),
        Val::Reg(Reg::RCX) => "rcx".to_string(),
        Val::Reg(Reg::RDX) => "rdx".to_string(),
        Val::Reg(Reg::R8) => "r8".to_string(),
        Val::Reg(Reg::RBP) => "rbp".to_string(),
        Val::Reg(Reg::RDI) => "rdi".to_string(),
        Val::Imm(n) => n.to_string(),
        Val::RegOffset(Reg::RBP, n) => format!("[rbp - {}]", n),
        Val::Label(label) => label.clone(),
        _ => panic!("Only support RBP for regoffset"),
    }
}

fn compile(e: &Expr) -> String {
    let instrs = compile_to_instrs(e, &HashMap::new(), "");
    let mut ret = String::new();
    for instr in instrs {
        ret += &instr_to_str(&instr);
    }
    ret
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // You will make result hold the result of actually compiling
    // let result = "mov rax, 131";
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;
    let expr = parse_expr(&parse(&in_contents).unwrap());
    let result = compile(&expr);

    let asm_program = format!(
        "
section .text
extern snek_error
global our_code_starts_here
our_code_starts_here:
  {}
  ret
invalid_arg_handler:
    mov rdi, 1       
    sub rsp, 8       ; add stack alignment to avoid crashes 
    call snek_error   
    add rsp, 8       
    ret
overflow_handler:
    mov rdi, 2
    sub rsp, 8
    call snek_error
    add rsp, 8
    ret
",
        result
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
