use std::io::Read;

struct Machine {
    mem: [u8; 30000],
    dp: usize,
    code: String,
    pc: usize
}

fn execute(m: &mut Machine) {
    let code = m.code.as_bytes();
    let len = code.len();

    while m.pc < len {
        match code[m.pc] {
            b'+' => {
                m.mem[m.dp] += 1;
                m.pc += 1;
            },
            b'-' => {
                m.mem[m.dp] -= 1;
                m.pc += 1;
            },
            b'>' => {
                m.dp += 1;
                m.pc += 1;
            },
            b'<' => {
                m.dp -= 1;
                m.pc += 1;
            },
            b'.' => {
                print!("{}", m.mem[m.dp]);
                m.pc += 1;
            },
            b',' => {
                let input = std::io::stdin().bytes().next().and_then(|result| result.ok());
                match input {
                    Some(x) => m.mem[m.dp] = x,
                    None => panic!("error reading input")
                };
                m.pc += 1;
            },
            _ =>
                panic!("invalid opcode {}", code[m.pc])
        }
    }
}

fn main() {
    let hello_world = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
    let mut m: Machine = Machine {
        mem: [0; 30000],
        dp: 0,
        pc: 0,
        code: String::from(hello_world)
    };
    execute(&mut m);
}
