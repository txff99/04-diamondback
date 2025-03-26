use core::num;
use std::char::REPLACEMENT_CHARACTER;

use im::HashMap;
use rand::{thread_rng, Rng};
use rand::distributions::Alphanumeric;

use crate::parser::*;

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
    R15,
    RSP,
    RBP,
    RDI,
}

#[derive(Debug)]
enum Instr {
    Mov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    Push(Val),
    Pop(Val),
    And(Val, Val),
    JZ(Val),
    JE(Val),
    JNE(Val),
    JMP(Val),
    CMP(Val, Val),
    SETE(()),
    SETL(()),
    SETLE(()),
    SETG(()),
    SETGE(()),
    SAL(Val, Val),
    SAR(Val, Val),
    LABEL(Val),
    JO(Val),
    Return,
    Call(Val),
    Test(Val, Val),
    LEA(Val, Val),
}

#[derive(Clone)]
#[derive(Debug)]
struct FuncArg {
    addr: String,
    args: Vec<String>,
}

const GC_LBL: i64 = 1;

fn generate_label(prefix: &str) -> String {
    let random_suffix: String = thread_rng()
        .sample_iter(&Alphanumeric)
        .take(6)  // Generate a 6-character random suffix
        .map(char::from)
        .collect();

    format!("{}_{}", prefix, random_suffix)
}

fn compile_defns(defns: &Vec<Defn>, defn_map: &mut HashMap<String, FuncArg>) -> Vec<Instr> {
    let mut instrs = Vec::new();
    // get func map first because there could be mutual recursion calls
    for defn in defns {
        // defn::func(vec<string>, expr)
        match defn {
            Defn::Func(name, args, _) => {
                assert!(!defn_map.contains_key(name), "Multiple functions are defined with the same name");
                defn_map.insert(name.to_string(), FuncArg{ addr: generate_label(name), args: args.clone()});
            },
        }
    }
    for defn in defns {
        match defn {
            Defn::Func(name, args, expr) => {
                instrs.push(Instr::LABEL(Val::Label(defn_map[name].addr.clone())));
                // build a var map for func args
                instrs.push(Instr::Push(Val::Reg(Reg::RBP)));
                instrs.push(Instr::Mov(Val::Reg(Reg::RBP), Val::Reg(Reg::RSP)));
                let mut var_to_regoffset: HashMap<String, i64> = HashMap::new();
                for (i, id) in args.iter().enumerate() {
                    let index = i as i64;
                    assert!(!var_to_regoffset.contains_key(id), "A function's parameter list has a duplicate name");
                    var_to_regoffset.insert(id.to_string(), -(args.len() as i64-index+1));
                }
                instrs.extend(compile_to_instrs(expr, &var_to_regoffset, "", defn_map));
                instrs.push(Instr::Mov(Val::Reg(Reg::RSP), Val::Reg(Reg::RBP)));
                instrs.push(Instr::Pop(Val::Reg(Reg::RBP)));
                instrs.push(Instr::Return);
            },
        }
    }
    instrs
}

fn compile_to_instrs(e: &Expr, var_map: &HashMap<String, i64>, loop_end: &str, defn_map: &HashMap<String, FuncArg>) -> Vec<Instr> {
    match e {
        Expr::Number(n) => {
            if *n < -4611686018427387904 || *n > 4611686018427387903 {
                panic!("an error ocurred: overflow");
            }
            let mut instrs = vec![Instr::Mov(Val::Reg(Reg::RAX), Val::Imm(*n))];
            instrs.push(Instr::SAL(Val::Reg(Reg::RAX), Val::Imm(1)));
            instrs
        },
        Expr::Boolean(b) => vec![Instr::Mov(Val::Reg(Reg::RAX), Val::Imm(if *b {0b111} else {0b011}))],
        Expr::Input => {
            let instrs = vec![Instr::Mov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))];
            instrs
        },
        Expr::Nil => vec![Instr::Mov(Val::Reg(Reg::RAX), Val::Imm(0b01))],
        Expr::Id(id) => {
            assert!(var_map.contains_key(id), "{}", format!("Unbound variable identifier {}", id));
            vec![Instr::Mov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RBP, var_map[id]*8))]
        }
        Expr::UnOp(op1, subexpr) => {
            match op1 {
                Op1::Add1 => {
                  let mut instrs = compile_to_instrs(subexpr, var_map, loop_end, defn_map);
                  instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(0b10)));
                  instrs
                },
                Op1::Sub1 => {
                  let mut instrs = compile_to_instrs(subexpr, var_map, loop_end, defn_map);
                  instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(0b10)));
                  instrs
                },
                /* for typecheck we use the least significant bit for tag: 
                xxx0 for number, xx11 for bool, xx01 for pair */
                Op1::IsBool => {
                  let mut instrs = compile_to_instrs(subexpr, var_map, loop_end, defn_map);
                  instrs.push(Instr::Mov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
                  instrs.push(Instr::And(Val::Reg(Reg::RCX), Val::Imm(0b11)));
                  instrs.push(Instr::CMP(Val::Reg(Reg::RCX), Val::Imm(0b11)));
                  instrs.push(Instr::JNE(Val::Label("invalid_arg_handler".to_string())));
                  instrs
                },
                Op1::IsNum => {
                  let mut instrs = compile_to_instrs(subexpr, var_map, loop_end, defn_map);
                  instrs.push(Instr::Mov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
                  instrs.push(Instr::And(Val::Reg(Reg::RCX), Val::Imm(0b1)));
                  instrs.push(Instr::CMP(Val::Reg(Reg::RCX), Val::Imm(0b1)));
                  instrs.push(Instr::JE(Val::Label("invalid_arg_handler".to_string())));
                  instrs
                },
                Op1::IsVec => {
                    let mut instrs = compile_to_instrs(subexpr, var_map, loop_end, defn_map);
                  instrs.push(Instr::Mov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
                  instrs.push(Instr::And(Val::Reg(Reg::RCX), Val::Imm(0b11)));
                  instrs.push(Instr::CMP(Val::Reg(Reg::RCX), Val::Imm(0b1)));
                  instrs.push(Instr::JNE(Val::Label("invalid_arg_handler".to_string())));
                  instrs
                }
                Op1::IsOverflow => {
                    let mut instrs = compile_to_instrs(subexpr, var_map, loop_end, defn_map);
                    instrs.push(Instr::JO(Val::Label("overflow_handler".to_string())));
                    instrs
                },
                Op1::Print => {
                    let mut instrs = compile_to_instrs(subexpr, var_map, loop_end, defn_map);
                    instrs.push(Instr::Push(Val::Reg(Reg::RDI)));
                    instrs.push(Instr::Mov(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX)));
                    /* stack alignment */
                    instrs.push(Instr::Push(Val::Reg(Reg::RBP)));
                    instrs.push(Instr::Mov(Val::Reg(Reg::RBP), Val::Reg(Reg::RSP)));
                    instrs.push(Instr::And(Val::Reg(Reg::RSP), Val::Imm(0xfffffff0))); 
                    instrs.push(Instr::Call(Val::Label("snek_print".to_string())));
                    instrs.push(Instr::Mov(Val::Reg(Reg::RSP), Val::Reg(Reg::RBP)));
                    instrs.push(Instr::Pop(Val::Reg(Reg::RBP)));
                    
                    instrs.push(Instr::Pop(Val::Reg(Reg::RDI)));
                    instrs
                }
            }
        },
        Expr::BinOp(op2, e1, e2) => {
            match op2 {
                Op2::Plus => {
                    let mut instrs = compile_to_instrs(e2, var_map, loop_end, defn_map);
                    instrs.push(Instr::Push(Val::Reg(Reg::RAX)));
                    instrs.extend(compile_to_instrs(e1, var_map, loop_end, defn_map));
                    instrs.push(Instr::Pop(Val::Reg(Reg::RCX)));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                    instrs
                },
                Op2::Minus => {
                    let mut instrs = compile_to_instrs(e2, var_map, loop_end, defn_map);
                    instrs.push(Instr::Push(Val::Reg(Reg::RAX)));
                    instrs.extend(compile_to_instrs(e1, var_map, loop_end, defn_map));
                    instrs.push(Instr::Pop(Val::Reg(Reg::RCX)));
                    instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                    instrs
                },
                Op2::Times => {
                  let mut instrs = compile_to_instrs(e2, var_map, loop_end, defn_map);
                  instrs.push(Instr::Push(Val::Reg(Reg::RAX)));
                  instrs.extend(compile_to_instrs(e1, var_map, loop_end, defn_map));
                  instrs.push(Instr::Pop(Val::Reg(Reg::RCX)));
                  instrs.push(Instr::IMul(Val::Reg(Reg::RAX),Val::Reg(Reg::RCX)));
                  /* check num bound */
                  instrs.push(Instr::JO(Val::Label("overflow_handler".to_string())));
                    
                  /* when doing times we have to manually right shift */
                  instrs.push(Instr::SAR(Val::Reg(Reg::RAX), Val::Imm(1)));
                  instrs
                },
                Op2::Equal => {
                    let mut instrs = compile_to_instrs(e2, var_map, loop_end, defn_map);
                    instrs.push(Instr::Push(Val::Reg(Reg::RAX)));
                    instrs.extend(compile_to_instrs(e1, var_map, loop_end, defn_map));
                    instrs.push(Instr::Pop(Val::Reg(Reg::RCX)));
                    
                    /* check if type matches each other */
                    instrs.push(Instr::Mov(Val::Reg(Reg::RDX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::Mov(Val::Reg(Reg::R8), Val::Reg(Reg::RCX)));
                    instrs.push(Instr::And(Val::Reg(Reg::RDX), Val::Imm(0x1)));
                    instrs.push(Instr::And(Val::Reg(Reg::R8), Val::Imm(0x1)));
                    instrs.push(Instr::CMP(Val::Reg(Reg::RDX), Val::Reg(Reg::R8)));
                    instrs.push(Instr::JNE(Val::Label("invalid_arg_handler".to_string())));
                    
                    instrs.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                    instrs.push(Instr::SETE(()));
                    instrs.push(Instr::SAL(Val::Reg(Reg::RAX),Val::Imm(2)));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(0b11)));

                    instrs
                }
                Op2::Less => {
                    let mut instrs = compile_to_instrs(e2, var_map, loop_end, defn_map);
                    instrs.push(Instr::Push(Val::Reg(Reg::RAX)));
                    instrs.extend(compile_to_instrs(e1, var_map, loop_end, defn_map));
                    instrs.push(Instr::Pop(Val::Reg(Reg::RCX)));
                    
                    instrs.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                    instrs.push(Instr::SETL(()));
                    instrs.push(Instr::SAL(Val::Reg(Reg::RAX),Val::Imm(2)));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(0b11)));
                    instrs
                },
                Op2::LessEqual => {
                    let mut instrs = compile_to_instrs(e2, var_map, loop_end, defn_map);
                    instrs.push(Instr::Push(Val::Reg(Reg::RAX)));
                    instrs.extend(compile_to_instrs(e1, var_map, loop_end, defn_map));
                    instrs.push(Instr::Pop(Val::Reg(Reg::RCX)));
                    
                    instrs.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                    instrs.push(Instr::SETLE(()));
                    instrs.push(Instr::SAL(Val::Reg(Reg::RAX),Val::Imm(2)));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(0b11)));
                    instrs
                },
                Op2::Greater => {
                    let mut instrs = compile_to_instrs(e2, var_map, loop_end, defn_map);
                    instrs.push(Instr::Push(Val::Reg(Reg::RAX)));
                    instrs.extend(compile_to_instrs(e1, var_map, loop_end, defn_map));
                    instrs.push(Instr::Pop(Val::Reg(Reg::RCX)));
                    
                    instrs.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                    instrs.push(Instr::SETG(()));
                    instrs.push(Instr::SAL(Val::Reg(Reg::RAX),Val::Imm(2)));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(0b11)));
                    instrs
                },
                Op2::GreaterEqual => {
                    let mut instrs = compile_to_instrs(e2, var_map, loop_end, defn_map);
                    instrs.push(Instr::Push(Val::Reg(Reg::RAX)));
                    instrs.extend(compile_to_instrs(e1, var_map, loop_end, defn_map));
                    instrs.push(Instr::Pop(Val::Reg(Reg::RCX)));
                    
                    instrs.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                    instrs.push(Instr::SETGE(()));
                    instrs.push(Instr::SAL(Val::Reg(Reg::RAX),Val::Imm(2)));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(0b11)));
                    instrs
                }
            }
        },
        Expr::Let(vec, subexpr) => {
            let mut var_to_regoffset: HashMap<String, i64> = HashMap::new();
            let mut instrs = Vec::new();
            /* clone old var map to the new one */
            instrs.push(Instr::Mov(Val::Reg(Reg::RDX), Val::Reg(Reg::RBP)));

            instrs.push(Instr::Push(Val::Reg(Reg::RBP)));
            instrs.push(Instr::Mov(Val::Reg(Reg::RBP), Val::Reg(Reg::RSP)));
            instrs.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm((vec.len() as i64)*8)));
            for (i, tp) in vec.iter().enumerate() { 
                let index = i as i64;
                let (id, expr) = tp;
                assert!(!var_to_regoffset.contains_key(id), "Duplicate binding");
                instrs.extend(compile_to_instrs(expr, &var_to_regoffset, loop_end, defn_map));
                instrs.push(Instr::Mov(Val::RegOffset(Reg::RBP, (index+1)*8), Val::Reg(Reg::RAX)));
                var_to_regoffset.insert(id.to_string(), index+1);
            }

            let mut num_var_from_parent = 0;
            for k in var_map.keys() {
                if !var_to_regoffset.contains_key(k) {
                    instrs.push(Instr::Mov(Val::Reg(Reg::R8),
                         Val::RegOffset(Reg::RDX, var_map[k]*8)));
                    instrs.push(Instr::Mov(Val::RegOffset(Reg::RBP, (vec.len() as i64 + num_var_from_parent + 1)*8),
                         Val::Reg(Reg::R8)));
                    var_to_regoffset.insert(k.to_string(), vec.len() as i64 + num_var_from_parent + 1);
                    num_var_from_parent += 1;
                }
            }

            instrs.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(num_var_from_parent*8)));
            instrs.extend(compile_to_instrs(subexpr, &var_to_regoffset, loop_end, defn_map));
            instrs.push(Instr::Mov(Val::Reg(Reg::RSP), Val::Reg(Reg::RBP)));
            instrs.push(Instr::Pop(Val::Reg(Reg::RBP)));
            instrs
        },
        Expr::If(e1, e2, e3) => {
            let mut instrs = Vec::new();
            instrs.extend(compile_to_instrs(e1, var_map, loop_end, defn_map));
            instrs.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(0b111)));
            let else_label = generate_label("else");
            let end_label = generate_label("end");
            instrs.push(Instr::JNE(Val::Label(else_label.clone())));
            instrs.extend(compile_to_instrs(e2, var_map, loop_end, defn_map));
            instrs.push(Instr::JMP(Val::Label(end_label.clone())));
            instrs.push(Instr::LABEL(Val::Label(else_label.clone())));
            instrs.extend(compile_to_instrs(e3, var_map, loop_end, defn_map));
            instrs.push(Instr::JMP(Val::Label(end_label.clone())));
            instrs.push(Instr::LABEL(Val::Label(end_label.clone())));
            instrs
        }
        Expr::Loop(e) => {
            let loop_start = generate_label("loop_start");
            let loop_end = generate_label("loop_end");
            let mut instrs = Vec::new();
            instrs.push(Instr::LABEL(Val::Label(loop_start.clone())));
            instrs.extend(compile_to_instrs(e, var_map, &loop_end, defn_map));
            instrs.push(Instr::JMP(Val::Label(loop_start.clone())));
            instrs.push(Instr::LABEL(Val::Label(loop_end.clone())));
            instrs
        },
        Expr::Break(e) => {
            assert!(loop_end!="", "break outside of loop");
            let mut instrs = compile_to_instrs(e, var_map, loop_end, defn_map);
            instrs.push(Instr::JMP(Val::Label(loop_end.to_string())));
            instrs
        },
        Expr::Block(exprs) => {
            let mut instrs = Vec::new();
            for e in exprs {
                instrs.extend(compile_to_instrs(e, var_map, loop_end, defn_map));
            }
            instrs
        },
        Expr::Set(id, e) => {
            assert!(var_map.contains_key(id), "Unbound variable identifier: {}", id);
            let mut instrs = compile_to_instrs(e, var_map, loop_end, defn_map);
            instrs.push(Instr::Mov(Val::RegOffset(Reg::RBP, var_map[id]*8), Val::Reg(Reg::RAX)));
            instrs
        },
        Expr::Call(name, args) => {
            assert!(defn_map.contains_key(name), "There is a call to a function name that doesn't exist");
            assert!(args.len()==defn_map[name].args.len(), "There is a call to a function with the wrong number of arguments");
            // assign args to stack and call 
            let mut instrs = Vec::new();
            for expr in args.iter() {
                instrs.extend(compile_to_instrs(expr, var_map, loop_end, defn_map));
                instrs.push(Instr::Push(Val::Reg(Reg::RAX)));
            }
            instrs.push(Instr::Call(Val::Label(defn_map[name].addr.clone())));
            
            // cleanup
            for _ in 0..args.len() {
                instrs.push(Instr::Pop(Val::Reg(Reg::RCX)));
            }
            
            instrs
        },
        Expr::Pair(e1, e2) => {
            let mut instrs = Vec::new();
            instrs.push(Instr::Push(Val::Reg(Reg::RBP)));
            instrs.push(Instr::Mov(Val::Reg(Reg::RBP), Val::Reg(Reg::R15)));
            instrs.push(Instr::IAdd(Val::Reg(Reg::R15), Val::Imm(8*2)));
            instrs.extend(compile_to_instrs(e1, var_map, loop_end, defn_map));
            instrs.push(Instr::Mov(Val::RegOffset(Reg::RBP, 0), Val::Reg(Reg::RAX)));
            instrs.extend(compile_to_instrs(e2, var_map, loop_end, defn_map));
            instrs.push(Instr::Mov(Val::RegOffset(Reg::RBP, -8), Val::Reg(Reg::RAX)));
            instrs.push(Instr::Mov(Val::Reg(Reg::RAX), Val::Reg(Reg::RBP)));
            instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(0b01)));
            instrs.push(Instr::Pop(Val::Reg(Reg::RBP)));
            instrs
        },
        Expr::Vec(exprs) => {
            let num_elem = exprs.len() as i64;
            let mut instrs = Vec::new();
            instrs.push(Instr::Push(Val::Reg(Reg::RBP)));
            instrs.push(Instr::Mov(Val::Reg(Reg::RBP), Val::Reg(Reg::R15)));
            instrs.push(Instr::IAdd(Val::Reg(Reg::R15), Val::Imm(8*(num_elem + 2))));
            instrs.push(Instr::Mov(Val::RegOffset(Reg::RBP, 0), Val::Imm(GC_LBL)));
            instrs.push(Instr::Mov(Val::RegOffset(Reg::RBP, -8), Val::Imm(num_elem)));
            for (i, expr) in exprs.iter().enumerate() {
                instrs.extend(compile_to_instrs(expr, var_map, loop_end, defn_map));
                instrs.push(Instr::Mov(Val::RegOffset(Reg::RBP, -8 * (i as i64 + 2)), Val::Reg(Reg::RAX)));
            }
            instrs.push(Instr::Mov(Val::Reg(Reg::RAX), Val::Reg(Reg::RBP)));
            instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(0b01)));
            instrs.push(Instr::Pop(Val::Reg(Reg::RBP)));
            instrs
        },
        Expr::MakeVec(e1, e2) => {
            let mut instrs = compile_to_instrs(e1, var_map, loop_end, defn_map);
            instrs.push(Instr::Push(Val::Reg(Reg::RBP)));
            instrs.push(Instr::Mov(Val::Reg(Reg::RBP), Val::Reg(Reg::R15)));
            
            // store vec elements in rax, compute address offset in rcx
            instrs.push(Instr::LEA(Val::Reg(Reg::RCX), Val::RegOffset(Reg::RAX, -2)));
            instrs.push(Instr::IMul(Val::Reg(Reg::RCX), Val::Imm(8)));

            instrs.push(Instr::IAdd(Val::Reg(Reg::R15), Val::Reg(Reg::RCX)));
            instrs.push(Instr::Mov(Val::RegOffset(Reg::RBP, 0), Val::Imm(GC_LBL)));
            instrs.push(Instr::Mov(Val::RegOffset(Reg::RBP, -8), Val::Reg(Reg::RAX)));
            
            instrs.extend(compile_to_instrs(e2, var_map, loop_end, defn_map));
            instrs.push(Instr::Mov(Val::Reg(Reg::RDX), Val::Reg(Reg::RBP)));
            instrs.push(Instr::IAdd(Val::Reg(Reg::RDX), Val::Imm(16)));
            
            /* mov rbp to rdx, iterate rdx until reach r15 */
            let make_vec_start = generate_label("make_vec_start");
            let make_vec_end = generate_label("make_vec_end");
            instrs.push(Instr::LABEL(Val::Label(make_vec_start.clone())));

            instrs.push(Instr::Mov(Val::RegOffset(Reg::RDX, 0), Val::Reg(Reg::RAX)));
            instrs.push(Instr::IAdd(Val::Reg(Reg::RDX), Val::Imm(8)));

            instrs.push(Instr::CMP(Val::Reg(Reg::RDX), Val::Reg(Reg::R15)));
            instrs.push(Instr::JE(Val::Label(make_vec_end.clone())));
            instrs.push(Instr::JMP(Val::Label(make_vec_start.clone())));
            instrs.push(Instr::LABEL(Val::Label(make_vec_end.clone())));
            
            instrs.push(Instr::Mov(Val::Reg(Reg::RAX), Val::Reg(Reg::RBP)));
            instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(0b01)));
            instrs.push(Instr::Pop(Val::Reg(Reg::RBP)));
            instrs
        }, 
        Expr::VecGet(e1, e2) => {
            let mut instrs = compile_to_instrs(e1, var_map, loop_end, defn_map);
            instrs.push(Instr::Push(Val::Reg(Reg::RBP)));
            instrs.push(Instr::Mov(Val::Reg(Reg::RBP), Val::Reg(Reg::RAX)));
            instrs.push(Instr::ISub(Val::Reg(Reg::RBP), Val::Imm(1)));
            
            instrs.extend(compile_to_instrs(e2, var_map, loop_end, defn_map));
            /* todo: bound check */
            instrs.push(Instr::SAR(Val::Reg(Reg::RAX), Val::Imm(1)));
            instrs.push(Instr::LEA(Val::Reg(Reg::RDX), Val::RegOffset(Reg::RAX, -2)));
            instrs.push(Instr::IMul(Val::Reg(Reg::RDX), Val::Imm(8)));
            instrs.push(Instr::IAdd(Val::Reg(Reg::RBP), Val::Reg(Reg::RDX)));
            instrs.push(Instr::Mov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RBP, 0)));
            instrs.push(Instr::Pop(Val::Reg(Reg::RBP)));
            instrs
        },
        Expr::VecSet(e1, e2, e3) => {
            let mut instrs = compile_to_instrs(e1, var_map, loop_end, defn_map);
            instrs.push(Instr::Push(Val::Reg(Reg::RBP)));
            instrs.push(Instr::Mov(Val::Reg(Reg::RBP), Val::Reg(Reg::RAX)));
            instrs.push(Instr::ISub(Val::Reg(Reg::RBP), Val::Imm(1)));
            
            instrs.extend(compile_to_instrs(e2, var_map, loop_end, defn_map));
            /* todo: bound check */
            instrs.push(Instr::SAR(Val::Reg(Reg::RAX), Val::Imm(1)));
            instrs.push(Instr::LEA(Val::Reg(Reg::RDX), Val::RegOffset(Reg::RAX, -2)));
            instrs.push(Instr::IMul(Val::Reg(Reg::RDX), Val::Imm(8)));
            instrs.push(Instr::IAdd(Val::Reg(Reg::RBP), Val::Reg(Reg::RDX)));
            
            instrs.extend(compile_to_instrs(e3, var_map, loop_end, defn_map));

            instrs.push(Instr::Mov(Val::RegOffset(Reg::RBP, 0), Val::Reg(Reg::RAX)));
            instrs.push(Instr::Pop(Val::Reg(Reg::RBP)));
            instrs
        }, 
        Expr::VecLen(e) => {
            let mut instrs = compile_to_instrs(e, var_map, loop_end, defn_map);
            instrs.push(Instr::Push(Val::Reg(Reg::RBP)));
            instrs.push(Instr::Mov(Val::Reg(Reg::RBP), Val::Reg(Reg::RAX)));
            instrs.push(Instr::ISub(Val::Reg(Reg::RBP), Val::Imm(1)));
            instrs.push(Instr::Mov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RBP, -8)));
            instrs
        }
    }
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::Mov(dst, src) => format!("\nmov qword {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IAdd(dst, src) => format!("\nadd {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ISub(dst, src) => format!("\nsub {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::Push(src) => format!("\npush {}", val_to_str(src)),
        Instr::Pop(src) => format!("\npop {}", val_to_str(src)),
        Instr::IMul(dst, src) => format!("\nimul {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::And(dst, src) => format!("\nand {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::CMP(dst, src) => format!("\ncmp {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::JZ(label) => format!("\njz {}", val_to_str(label)),
        Instr::JE(label) => format!("\nje {}", val_to_str(label)),
        Instr::JNE(label) => format!("\njne {}", val_to_str(label)),
        Instr::JMP(label) => format!("\njmp {}", val_to_str(label)),
        Instr::LABEL(label) => format!("\n{}:",val_to_str(label)),
        Instr::SAL(dst, bits) => format!("\nsal {}, {}", val_to_str(dst), val_to_str(bits)),
        Instr::SAR(dst, bits) => format!("\nsar {}, {}", val_to_str(dst), val_to_str(bits)),
        Instr::SETE(_) => format!("\nsete al\nmovzx rax, al"),
        Instr::SETL(_) => format!("\nsetl al\nmovzx rax, al"),
        Instr::SETLE(_) => format!("\nsetle al\nmovzx rax, al"),
        Instr::SETG(_) => format!("\nsetg al\nmovzx rax, al"),
        Instr::SETGE(_) => format!("\nsetge al\nmovzx rax, al"),
        Instr::JO(dst) => format!("\njo {}", val_to_str(dst)),
        Instr::Return => format!("\nret"),
        Instr::Call(dst) => format!("\ncall {}", val_to_str(dst)),
        Instr::Test(dst, src) => format!("\ntest {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::LEA(dst, src) => format!("\nlea {}, {}", val_to_str(dst), val_to_str(src)),
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(reg) => reg_to_str(reg),
        Val::Imm(n) => n.to_string(),
        Val::RegOffset(reg, n) => {
            if *n > 0 {
                format!("[{} - {}]", reg_to_str(reg), n)
            } else {
                format!("[{} + {}]", reg_to_str(reg), -n)
            }
        },
        Val::Label(label) => label.clone(),
        _ => panic!("value type not supported"),
    }
}

fn reg_to_str(v: &Reg) -> String {
    match v {
        Reg::RAX => "rax".to_string(),
        Reg::RSP => "rsp".to_string(),
        Reg::RCX => "rcx".to_string(),
        Reg::RDX => "rdx".to_string(),
        Reg::R8 => "r8".to_string(),
        Reg::R15 => "r15".to_string(),
        Reg::RBP => "rbp".to_string(),
        Reg::RDI => "rdi".to_string(),
    }
}


pub fn compile(e: &Expr, defs: &Vec<Defn>) -> String {
    // defn map -> name, args for each func & jumping address for funcs -> pass to main program
    // combine the str of defn and the main program
    let mut defn_map = HashMap::new();
    let defn_instrs = compile_defns(defs, &mut defn_map);
    let instrs = compile_to_instrs(e, &HashMap::new(), "", &defn_map);
    let mut ret = String::new();
    for instr in instrs {
        ret += &instr_to_str(&instr);
    }
    ret += "\nret";
    for instr in defn_instrs {
        ret += &instr_to_str(&instr);
    }
    ret
}
