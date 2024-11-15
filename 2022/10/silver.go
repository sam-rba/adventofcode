package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"strconv"
	"strings"
)

type Instruction struct {
	opcode Opcode
	arg    int
}

type Opcode int

const (
	_ Opcode = iota
	noop
	addx
)

type CPU struct {
	x        int
	clock    int
	pipeline chan int
	signal   chan int
}

func newCPU() CPU {
	return CPU{1, 0, make(chan int, 1), make(chan int)}
}

func (cpu *CPU) exec(instr Instruction) {
	cpu.clock++

	cpu.signal <- cpu.signalStrength()

	select {
	case v := <-cpu.pipeline:
		cpu.x += v
		cpu.exec(instr)
		return
	default:
	}

	if instr.opcode == addx {
		cpu.pipeline <- instr.arg
	}
}

func (cpu CPU) signalStrength() int {
	return cpu.clock * cpu.x
}

func (cpu CPU) Close() {
	cpu.flush()
	close(cpu.pipeline)
	close(cpu.signal)
}

func (cpu *CPU) flush() {
	cpu.exec(Instruction{noop, 0})
}

func main() {
	cpu := newCPU()

	signalSum := make(chan int)
	defer close(signalSum)
	go receiveSignal(cpu.signal, signalSum)

	instructions := make(chan Instruction)
	go parse(os.Stdin, instructions)
	for instr := range instructions {
		cpu.exec(instr)
	}

	cpu.Close()
	fmt.Println("silver:", <-signalSum)
}

func receiveSignal(signal <-chan int, signalSum chan<- int) {
	sum := 0
	clock := 0
	for sig := range signal {
		clock++
		fmt.Println(clock, sig)
		if clock >= 20 && (clock-20)%40 == 0 {
			sum += sig
		}
	}
	signalSum <- sum
}

func parse(in io.Reader, out chan<- Instruction) {
	defer close(out)
	scanner := bufio.NewScanner(in)
	for scanner.Scan() {
		instr, err := parseInstruction(scanner.Text())
		if err != nil {
			log.Fatal(err)
		}
		out <- instr
	}
}

func parseInstruction(s string) (Instruction, error) {
	if s == "noop" {
		return Instruction{noop, 0}, nil
	}

	fields := strings.Split(s, " ")
	if len(fields) != 2 || fields[0] != "addx" {
		return Instruction{}, fmt.Errorf("bad addx instruction: '%s'", s)
	}
	v, err := strconv.Atoi(fields[1])
	if err != nil {
		return Instruction{}, err
	}
	return Instruction{addx, v}, nil
}
