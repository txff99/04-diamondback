use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: i64, buffer: *mut i64) -> i64;
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
pub extern "C" fn snek_print(val: i64, is_recursive: bool) -> i64 {
    if val == 0b011 {
        print!("false");
    } else if val == 0b111 {
        print!("true");
    } else if val % 2 == 0 {
        print!("{}", val>>1);
    } else if val & 0b01 == 0b01 {
        /* pair values */
        if val == 1 {
            print!(" nil");
            return val;
        }
        let addr: *const i64 = (val-1) as *const i64;
        print!("(pair ");
        snek_print( unsafe {*addr} , true);
        print!(" ");
        snek_print( unsafe {*addr.offset(1)}, true);
        print!(")");
    } else {
        print!("val: {}, snek print not implemented!", val);
    }
    if !is_recursive {
        print!("\n");
    }
    val
}

fn parse_input(input: &str) -> i64 {
    // TODO: parse the input string into internal value representation
    match input {
        "true" => 0b111,
        "false" => 0b011,
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

    let mut memory:Vec<i64> = Vec::with_capacity(100000);
    let buffer: *mut i64 = memory.as_mut_ptr();

    let i: i64 = unsafe { our_code_starts_here(input, buffer) };
    snek_print(i, false);
}
