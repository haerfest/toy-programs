package main

import "fmt"

type machine struct {
	mem    [30000]byte
	code   string
	dp, pc int
}

func find(m *machine, needle byte, skip byte, forwards bool) int {
	pc := m.pc
	depth := 0
	for !(m.code[pc] == needle && depth == 0) {
		switch m.code[pc] {
		case needle:
			depth--
		case skip:
			depth++
		}

		if forwards {
			pc++
		} else {
			pc--
		}
	}

	return pc
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
				m.pc = find(m, ']', '[', true)
			} else {
				m.pc++
			}
		case ']':
			if m.mem[m.dp] == 0 {
				m.pc++
			} else {
				m.pc--
				m.pc = find(m, '[', ']', false)
			}
		}
	}
}

func main() {
	helloWorld := "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---." +
		"+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
	m := machine{dp: 0, code: helloWorld, pc: 0}
	execute(&m)
}
