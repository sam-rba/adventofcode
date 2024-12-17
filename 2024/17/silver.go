package main

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"log"
	"os"
	"strconv"
	"strings"
)

const numReg = 3

type Computer struct {
	pc     int
	reg    [numReg]int
	output []int
}

type Program []int

type Instruction struct {
	opcode  OpCode
	operand int
}

type OpCode int

const (
	adv OpCode = iota
	bxl
	bst
	jnz
	bxc
	out
	bdv
	cdv
)

func (c *Computer) run(prog Program) {
	for c.pc < len(prog) {
		instr := c.readInstr(prog)
		c.exec(instr)
		c.pc += 2
	}
}

func (c Computer) readInstr(prog Program) Instruction {
	return Instruction{OpCode(prog[c.pc]), prog[c.pc+1]}
}

func (c *Computer) exec(instr Instruction) {
	switch instr.opcode {
	case adv:
		num := c.reg[0]
		den := 1 << c.combo(instr)
		c.reg[0] = num / den
	case bxl:
		c.reg[1] = c.reg[1] ^ instr.operand
	case bst:
		c.reg[1] = c.combo(instr) % 8
	case jnz:
		if c.reg[0] != 0 {
			c.pc = instr.operand - 2
		}
	case bxc:
		c.reg[1] = c.reg[1] ^ c.reg[2]
	case out:
		c.output = append(c.output, c.combo(instr)%8)
	case bdv:
		num := c.reg[0]
		den := 1 << c.combo(instr)
		c.reg[1] = num / den
	case cdv:
		num := c.reg[0]
		den := 1 << c.combo(instr)
		c.reg[2] = num / den
	default:
		panic(fmt.Sprint("illegal opcode:", instr.opcode))
	}
}

func (c Computer) combo(i Instruction) int {
	switch i.operand {
	case 0, 1, 2, 3:
		return i.operand
	case 4:
		return c.reg[0]
	case 5:
		return c.reg[1]
	case 6:
		return c.reg[2]
	default:
		panic(fmt.Sprint("illegal combo operand:", i.operand))
	}
}

func (c Computer) print() string {
	var s strings.Builder
	if len(c.output) > 0 {
		s.WriteString(fmt.Sprint(c.output[0]))
	}
	for _, n := range c.output[1:] {
		s.WriteString(fmt.Sprintf(",%d", n))
	}
	return s.String()
}

func main() {
	computer, prog, err := parse(os.Stdin)
	if err != nil {
		log.Fatal(err)
	}

	computer.run(prog)
	fmt.Println(computer.print())
}

func parse(in io.Reader) (Computer, Program, error) {
	r := bufio.NewReader(os.Stdin)
	computer, err := parseComputer(r)
	if err != nil {
		return Computer{}, Program{}, err
	}
	if _, err := r.ReadBytes('\n'); err != nil {
		return Computer{}, Program{}, err
	}
	prog, err := parseProgram(r)
	return computer, prog, err
}

func parseComputer(r *bufio.Reader) (Computer, error) {
	var reg [numReg]int
	for i := range reg {
		var err error
		reg[i], err = parseReg('A'+byte(i), r)
		if err != nil {
			return Computer{}, err
		}
	}
	return Computer{
		pc:  0,
		reg: reg,
	}, nil
}

func parseReg(name byte, r *bufio.Reader) (int, error) {
	line, err := r.ReadBytes('\n')
	if err != nil {
		return -1, err
	}
	prefix := fmt.Sprintf("Register %c: ", name)
	s := bytes.TrimLeft(line[:len(line)-1], prefix)
	return strconv.Atoi(string(s))
}

func parseProgram(r *bufio.Reader) (Program, error) {
	line, err := r.ReadBytes('\n')
	if err != nil && err != io.EOF {
		return nil, err
	}
	s := bytes.TrimPrefix(line, []byte("Program: "))
	s = bytes.TrimSuffix(s, []byte{'\n'})
	fields := bytes.Split(s, []byte{','})
	prog := make(Program, len(fields))
	for i := range fields {
		n, err := strconv.Atoi(string(fields[i]))
		if err != nil {
			return nil, err
		}
		prog[i] = n
	}
	return prog, nil
}
