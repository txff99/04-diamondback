use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: i64) -> i64;
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    // TODO: print error message according to writeup
    match errcode {
        1 => eprintln!("an error ocurred: invalid argument"),
        2 => eprintln!("an error ocurred: overflow"),
        _ => eprintln!("an error ocurred: errno {}", errcode),
    }
    std::process::exit(1);
}
#[export_name = "\x01snek_print"]
pub extern "C" fn snek_print(val: i64) -> i64 {
    if val == 1 {
        println!("false");
    } else if val == 3 {
        println!("true");
    } else {
        println!("{}", val>>1);
    }
    val
}

fn parse_input(input: &str) -> i64 {
    // TODO: parse the input string into internal value representation
    match input {
        "true" => 0b11,
        "false" => 0b01,
        _ => { 
            let ret: i64 = input.parse().expect("input argument can't be parsed to i64 nor is a bool");
            if ret < -4611686018427387904 || ret > 4611686018427387903 {
                panic!("an error ocurred: overflow");
            }
            ret << 1
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);


    let i: i64 = unsafe { our_code_starts_here(input) };
    if i & 1 == 1 {
        if i == 3 {
            println!("true");
        } else if i == 1 {
            println!("false");
        } else {
            println!("invalid ret {i}")
        }
    } else {
        println!("{}", i >> 1);
    }
}
