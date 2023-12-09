package main

import (
	"bufio"
	"fmt"
	"os"
)

type Node struct {
	left  string
	right string
}

func main() {
	var (
		stdin       *bufio.Scanner
		line        string
		instrs      string
		parent      string
		left, right string
		nodes       map[string]Node
		node        Node
		ok          bool
		steps       int
	)

	// read instructions
	stdin = bufio.NewScanner(os.Stdin)
	if !stdin.Scan() {
		fmt.Println("input missing instructions")
		os.Exit(1)
	}
	instrs = stdin.Text()

	// read node definitions
	nodes = make(map[string]Node)
	stdin.Scan() // skip blank line
	for stdin.Scan() {
		line = stdin.Text()
		if len(line) < 16 {
			fmt.Println("line too short:\n", line)
			os.Exit(1)
		}
		parent = line[:3]
		left = line[7:10]
		right = line[12:15]
		if _, ok = nodes[parent]; ok {
			fmt.Println("duplicate definition of", parent)
			os.Exit(1)
		}
		nodes[parent] = Node{left, right}
	}

	// follow instructions
	parent = "AAA"
	for steps = 0; parent != "ZZZ"; steps++ {
		node, ok = nodes[parent]
		if !ok {
			fmt.Println("unknown node:", parent)
			os.Exit(1)
		}
		switch instrs[steps%len(instrs)] {
		case 'L':
			parent = node.left
		case 'R':
			parent = node.right
		default:
			fmt.Printf("unknown instruction: %c\n", instrs[steps%len(instrs)])
			os.Exit(1)
		}
	}
	fmt.Printf("found ZZZ after %d steps\n", steps)
}
