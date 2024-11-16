package main

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
	"strings"
)

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

	divTest, err := rParseNum(scanner)
	if err != nil {
		return Monkey{}, err
	}

	throwTrue, err := rParseNum(scanner)
	if err != nil {
		return Monkey{}, err
	}

	throwFalse, err := rParseNum(scanner)

	return Monkey{items, inspect, divTest, throwTrue, throwFalse, 0}, err
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
	s = strings.TrimPrefix(s, "  Operation: new = old ")

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

func rParseNum(scanner *bufio.Scanner) (int, error) {
	if !scanner.Scan() {
		return -1, io.EOF
	}
	fields := strings.Split(scanner.Text(), " ")
	n, err := strconv.Atoi(fields[len(fields)-1])
	return n, err
}
