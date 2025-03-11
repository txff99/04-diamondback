use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::*;

mod parser;
mod lowerer;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // You will make result hold the result of actually compiling
    // let result = "mov rax, 131";
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;
    let exprs: Vec<&str> = in_contents.split("\n\n").collect();
    let mut defs: Vec<parser::Defn> = Vec::new();
    for i in 0..exprs.len()-1 {
      defs.push(parser::parse_def(&parse(exprs[i]).unwrap()));
    }

    let expr = parser::parse_expr(&parse(exprs[exprs.len()-1]).unwrap());
    let result = lowerer::compile(&expr, &defs);

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
