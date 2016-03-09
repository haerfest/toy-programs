package main

import "fmt"

type machine struct {
    mem [30000]byte
    code string
    dp, pc int
}

func find(m *machine, needle byte, skip byte, depth int, next func(int) int) {
    for !(m.code[m.pc] == needle && depth == 0) {
        switch m.code[m.pc] {
        case needle:
            depth--
        case skip:
            depth++
        }
        m.pc = next(m.pc)
    }
}

func execute(m *machine) {
    for m.pc < len(m.code) {
        switch m.code[m.pc] {
        case '+':
            m.mem[m.dp]++
            m.pc++
        case '-':
            m.mem[m.dp]--
            m.pc++
        case '>':
            m.dp++
            m.pc++
        case '<':
            m.dp--
            m.pc++
        case '.':
            fmt.Printf("%c", m.mem[m.dp])
            m.pc++
        case ',':
            fmt.Scanf("%c", m.mem[m.dp])
            m.pc++
        case '[':
            if m.mem[m.dp] == 0 {
                m.pc++
                find(m, ']', '[', 0, func(pc int) int { return pc + 1 })
            } else {
                m.pc++
            }
        case ']':
            if m.mem[m.dp] == 0 {
                m.pc++
            } else {
                m.pc--
                find(m, '[', ']', 0, func(pc int) int { return pc - 1 })
            }
        }
    }
}

func main() {
    hello_world := "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---." +
                   "+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    m := machine{dp: 0, code: hello_world, pc: 0}
    execute(&m)
}
