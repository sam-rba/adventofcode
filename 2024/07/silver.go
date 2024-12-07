package main

import (
	"bufio"
	"fmt"
	"github.com/sam-rba/adventofcode/lib"
	"github.com/sam-rba/workpool"
	"io"
	"log"
	"os"
	"slices"
	"strconv"
	"strings"
)

var operators = []Operator{add, mul}

type Operator func(int, int) int

type Equation struct {
	testVal  int
	operands []int
}

func main() {
	equations := make(chan Equation)
	go parseEquations(os.Stdin, equations)

	calibration, total := make(chan int), make(chan int)
	go lib.Sum(calibration, total)

	pool := workpool.New(workpool.DefaultSize)
	for eq := range equations {
		pool.Spawn(func() {
			if slices.Contains(eval(eq.operands, operators), eq.testVal) {
				calibration <- eq.testVal
			}
		})
	}
	pool.Wait()
	pool.Close()
	close(calibration)

	fmt.Println("silver:", <-total)
}

func parseEquations(in io.Reader, equations chan<- Equation) {
	defer close(equations)
	scanner := bufio.NewScanner(in)
	for scanner.Scan() {
		eq, err := parseEquation(scanner.Text())
		if err != nil {
			log.Fatal(err)
		}
		equations <- eq
	}
}

func parseEquation(s string) (Equation, error) {
	fields := strings.Split(s, ": ")
	if len(fields) != 2 {
		return Equation{}, fmt.Errorf("bad input: '%s'", s)
	}

	testVal, err := strconv.Atoi(fields[0])
	if err != nil {
		return Equation{}, err
	}

	operands, err := parseOperands(fields[1])
	return Equation{testVal, operands}, err
}

func parseOperands(s string) ([]int, error) {
	fields := strings.Fields(s)
	operands := make([]int, len(fields))
	for i := range fields {
		var err error
		operands[i], err = strconv.Atoi(fields[i])
		if err != nil {
			return nil, err
		}
	}
	return operands, nil
}

func eval(operands []int, operators []Operator) []int {
	if len(operands) <= 1 {
		return operands
	}

	subtrees := eval(operands[:len(operands)-1], operators)
	branches := make([]int, 0, len(subtrees)*len(operators))
	for _, subtree := range subtrees {
		for _, op := range operators {
			branch := op(operands[len(operands)-1], subtree)
			branches = append(branches, branch)
		}
	}
	return branches
}

func add(a, b int) int { return a + b }
func mul(a, b int) int { return a * b }
