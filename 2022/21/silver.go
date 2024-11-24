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

type Line struct {
	name, job string
}

type Expr interface {
	Eval() int
}

type Node struct {
	lname, rname string
	left, right  Expr
	op           Op
}

type Op func(int, int) int

func (n Node) Eval() int {
	return n.op(n.left.Eval(), n.right.Eval())
}

type Leaf int

func (l Leaf) Eval() int {
	return int(l)
}

func main() {
	lines := make(chan Line)
	go lex(os.Stdin, lines)
	tree, err := Parse(lines)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("silver:", tree.Eval())
}

func lex(in io.Reader, out chan<- Line) {
	defer close(out)
	scanner := bufio.NewScanner(in)
	for scanner.Scan() {
		fields := strings.Split(scanner.Text(), ": ")
		if len(fields) != 2 {
			log.Fatal("bad input:", scanner.Text())
		}
		out <- Line{fields[0], fields[1]}
	}
}

func Parse(in <-chan Line) (Expr, error) {
	seen := make(map[string]string)

	root, err := parse("root", in, seen)
	if err != nil {
		return nil, err
	}

	q := make([]*Node, 0)
	node := root.(*Node)
	q = append(q, node)
	for len(q) > 0 {
		dot := q[0]
		q = q[1:]

		dot.left, err = parse(dot.lname, in, seen)
		if err != nil {
			return nil, err
		}
		if node, ok := dot.left.(*Node); ok {
			q = append(q, node)
		}

		dot.right, err = parse(dot.rname, in, seen)
		if err != nil {
			return nil, err
		}
		if node, ok := dot.right.(*Node); ok {
			q = append(q, node)
		}
	}

	return root, nil
}

func parse(name string, in <-chan Line, seen map[string]string) (Expr, error) {
	if job, ok := seen[name]; ok {
		delete(seen, name)
		return parseJob(job)
	}
	for line := range in {
		if line.name == name {
			return parseJob(line.job)
		}
		seen[line.name] = line.job
	}
	return nil, fmt.Errorf("no monkey named %s", name)
}

func parseJob(job string) (Expr, error) {
	n, err := strconv.Atoi(job)

	if err != nil {
		return parseArith(job)
	}

	return Leaf(n), nil
}

func parseArith(job string) (*Node, error) {
	fields := strings.Split(job, " ")
	if len(fields) != 3 {
		return nil, fmt.Errorf("bad arithmetic job: '%s'", job)
	}
	lname, rname := fields[0], fields[2]
	op, err := parseOp(fields[1])
	if err != nil {
		return nil, err
	}
	return &Node{lname, rname, nil, nil, op}, nil
}

func parseOp(str string) (Op, error) {
	switch str {
	case "+":
		return add, nil
	case "-":
		return sub, nil
	case "*":
		return mul, nil
	case "/":
		return div, nil
	}
	return nil, fmt.Errorf("bad op: '%s'", str)
}

func add(a, b int) int { return a + b }
func sub(a, b int) int { return a - b }
func mul(a, b int) int { return a * b }
func div(a, b int) int { return a / b }
