use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::*;

mod parser;
mod lowerer;

fn sexp_separator(s: &str) -> Vec<&str> {
  let mut head = 0;
  let mut end = 0;
  let mut counter = 0;
  let mut ret = Vec::new();
  let mut contains_pair = false;
  for (i,ch) in s.chars().enumerate() {
    assert!(counter >= 0, "parse error: parentheses don't match");
    if ch == '(' {
      counter += 1;
      contains_pair = true;
    } else if ch == ')' {
      counter -= 1;
      if counter == 0 {
        end = i + 1;
        let start_byte = s.char_indices().nth(head).map(|(i, _)| i).unwrap_or(s.len());
        let end_byte = s.char_indices().nth(end).map(|(i, _)| i).unwrap_or(s.len());
        ret.push(&s[start_byte..end_byte]);
        head = end;
      }
    }
  }
  if !contains_pair {
    ret = vec![s];
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
    let exprs: Vec<&str> = sexp_separator(&in_contents);
    println!("{:?}", exprs);
    let mut defs: Vec<parser::Defn> = Vec::new();
    if exprs.len() > 1 {
      for i in 0..exprs.len()-1 {
        defs.push(parser::parse_def(&parse(exprs[i]).unwrap()));
      }
    }
    println!("{:?}", defs);
    let expr = parser::parse_expr(&parse(exprs[exprs.len()-1]).unwrap());
    println!("expr: {:?}", expr);
    let result = lowerer::compile(&expr, &defs);

    let asm_program = format!(
        "
section .text
extern snek_error
extern snek_print
global our_code_starts_here
our_code_starts_here:
mov r15, rsi
mov rsi, 0
  {}
invalid_arg_handler:
    push rdi
    mov rdi, 1    
    push rbp
    mov rbp, rsp
    and rsp, 0xfffffff0   
    call snek_error   
    mov rsp, rbp
    pop rbp
    pop rdi      
    ret
overflow_handler:
    push rdi
    mov rdi, 2
    push rbp
    mov rbp, rsp
    and rsp, 0xfffffff0   
    call snek_error
    mov rsp, rbp
    pop rbp
    pop rdi
    ret
oob_handler:
    push rdi
    mov rdi, 3
    push rbp
    mov rbp, rsp
    and rsp, 0xfffffff0   
    call snek_error
    mov rsp, rbp
    pop rbp
    pop rdi
    ret
",
        result
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
