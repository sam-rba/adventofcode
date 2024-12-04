package main

import (
	"fmt"
	"io"
	"log"
	"os"
)

type Instruction interface {
	exec(*CPU)
}

type CPU struct {
	disabled bool
	acc      int // accumulator
}

type Do struct{}

func (d Do) exec(cpu *CPU) {
	fmt.Println("enable")
	cpu.disabled = false
}

type Dont struct{}

func (d Dont) exec(cpu *CPU) {
	fmt.Println("disable")
	cpu.disabled = true
}

type Mul struct {
	l, r int
}

func (m Mul) exec(cpu *CPU) {
	fmt.Printf("mul %d %d; ", m.l, m.r)
	if cpu.disabled {
		fmt.Println("disabled")
	} else {
		fmt.Println("enabled")
		cpu.acc += m.l * m.r
	}
}

func main() {
	input, err := io.ReadAll(os.Stdin)
	if err != nil {
		log.Fatal(err)
	}
	items := lex(string(input))
	instructions := parse(items)
	var cpu CPU
	for instr := range instructions {
		instr.exec(&cpu)
	}
	fmt.Println("gold:", cpu.acc)
}
