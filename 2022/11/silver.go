package main

import (
	"os"
	"log"
	"io"
	"bufio"
	"fmt"
	"strings"
	"strconv"
)

const numRounds = 20

type Monkey struct {
	items []int
	inspect func(int) int
	test func(int) bool
	throwPass, throwFail int
	numInspections int
}

func main() {
	monkeys, err := parse(os.Stdin)
	if err != nil {
		log.Fatal(err)
	}
	for i := 0; i <  numRounds; i++ {
		fmt.Println("Round", i)
		for j := range monkeys {
			fmt.Printf("\tMonkey %d:\n", j)
			for _, item := range monkeys[j].items {
				fmt.Println("\t\tInspect", item)
				item = monkeys[j].inspect(item)
				fmt.Println("\t\tNew worry level:", item)

				monkeys[j].numInspections++

				item /= 3
				fmt.Println("\t\tBored", item)

				var dst int
				if monkeys[j].test(item) {
					dst = monkeys[j].throwPass
				} else {
					dst = monkeys[j].throwFail
				}
				fmt.Println("\t\tThrow to", dst)
				monkeys[dst].items = append(monkeys[dst].items, item)
			}
			monkeys[j].items = []int{}
		}
	}
	var mostActive [2]int
	for i := range monkeys {
		n := monkeys[i].numInspections
		fmt.Println(n)
		if n > mostActive[1] {
			mostActive[0] = mostActive[1]
			mostActive[1] = n
		} else if n > mostActive[0] {
			mostActive[0] = n
		}
	}
	fmt.Println("silver:", mostActive[0]*mostActive[1])
}

func parse(in io.Reader) ([]Monkey, error) {
	var monkeys []Monkey
	scanner := bufio.NewScanner(in)
	for scanner.Scan() {
		monkey, err := parseMonkey(scanner)
		if err != nil {
			return nil, err
		}
		monkeys = append(monkeys, monkey)
		scanner.Scan() // skip blank line
	}
	return monkeys, nil
}

func parseMonkey(scanner *bufio.Scanner) (Monkey, error) {
	items, err := parseItems(scanner)
	if err != nil {
		return Monkey{}, err
	}

	inspect, err := parseOperation(scanner)
	if err != nil {
		return Monkey{}, err
	}

	test, err := parseTest(scanner)
	if err != nil {
		return Monkey{}, err
	}

	throwPass, err := parseThrow(scanner)
	if err != nil {
		return Monkey{}, err
	}

	throwFail, err := parseThrow(scanner)

	return Monkey{items, inspect, test, throwPass, throwFail, 0}, err
}

func parseItems(scanner *bufio.Scanner) ([]int, error) {
	if !scanner.Scan() {
		return nil, io.EOF
	}
	s := scanner.Text()
	s = strings.TrimPrefix(s, "  Starting items: ")
	fields := strings.Split(s, ", ")
	items := make([]int, 0, len(fields))
	for _, s := range fields {
		n, err := strconv.Atoi(s)
		if err != nil {
			return nil, err
		}
		items = append(items, n)
	}
	return items, nil
}

func parseOperation(scanner *bufio.Scanner) (func(int) int, error) {
	if !scanner.Scan() {
		return nil, io.EOF
	}
	s := scanner.Text()
	s = strings.TrimPrefix(s, "  Operation: new = old ");

	var op func(int, int) int
	switch s[0] {
	case '+':
		op = func(a, b int) int { return a + b }
	case '*':
		op = func(a, b int) int { return a * b }
	default:
		return nil, fmt.Errorf("bad operation: '%s'", scanner.Text())
	}

	s = s[2:]
	if s == "old" {
		return func(item int) int { return op(item, item) }, nil
	}
	n, err := strconv.Atoi(s)
	if err != nil {
		return nil, fmt.Errorf("bad operation: '%s': %v", scanner.Text(), err)
	}
	return func(item int) int { return op(item, n) }, nil
}

func parseTest(scanner *bufio.Scanner) (func(int) bool, error) {
	if !scanner.Scan() {
		return nil, io.EOF
	}
	s := scanner.Text()
	s = strings.TrimPrefix(s, "  Test: divisible by ")
	n, err := strconv.Atoi(s)
	return func(item int) bool { return (item%n) == 0 }, err
}

func parseThrow(scanner *bufio.Scanner) (int, error) {
	if !scanner.Scan() {
		return -1, io.EOF
	}
	s := scanner.Text()
	n, err := strconv.Atoi(s[len(s)-1:])
	return n, err
}
