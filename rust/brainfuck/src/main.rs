use std::io::Read;

struct Machine<'a> {
    mem: [u8; 30000],
    dp: usize,
    code: &'a [u8],
    pc: usize
}

fn find(m: &Machine, needle: u8, skip: u8, forwards: bool) -> usize {
    let mut pc = m.pc;
    let mut depth = 0;

    while !(m.code[pc] == needle && depth == 0) {
        if m.code[pc] == needle {
            depth -= 1;
        } else if m.code[pc] == skip {
            depth += 1;
        }

        if forwards {
            pc += 1;
        } else {
            pc -= 1;
        }
    }

    return pc;
}

fn execute(m: &mut Machine) {
    let len = m.code.len();
    while m.pc < len {
        match m.code[m.pc] {
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
                print!("{}", m.mem[m.dp] as char);
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
            b'[' => {
                if m.mem[m.dp] == 0 {
                    m.pc += 1;
                    m.pc = find(m, b']', b'[', true);
                } else {
                    m.pc += 1;
                }
            },
            b']' => {
                if m.mem[m.dp] == 0 {
                    m.pc += 1;
                } else {
                    m.pc -= 1;
                    m.pc = find(m, b'[', b']', false);
                }
            },
            _ =>
                panic!("invalid opcode {:?}", m.code[m.pc])
        }
    }
}

fn main() {
    let hello_world = b"++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
    let mut m: Machine = Machine {
        mem: [0; 30000],
        dp: 0,
        pc: 0,
        code: hello_world
    };
    execute(&mut m);
}
