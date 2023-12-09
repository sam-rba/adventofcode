package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	var (
		stdin  *bufio.Scanner
		instrs string
		nodes  map[string]Node
		steps  int
		err    error
	)

	// read instructions
	stdin = bufio.NewScanner(os.Stdin)
	if !stdin.Scan() {
		fmt.Println("input missing instructions")
		os.Exit(1)
	}
	instrs = stdin.Text()

	stdin.Scan() // skip blank line
	nodes, err = readnodes(stdin)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	steps, err = atoz(nodes, instrs)
	if err != nil {
		fmt.Println(err)
	} else {
		fmt.Printf("found ZZZ after %d steps\n", steps)
	}
}

func atoz(nodes map[string]Node, instrs string) (int, error) {
	var (
		id    string
		steps int
		node  Node
		ok    bool
	)

	id = "AAA"
	for steps = 0; id != "ZZZ"; steps++ {
		node, ok = nodes[id]
		if !ok {
			return -1, fmt.Errorf("unknown node:", id)
		}
		switch instrs[steps%len(instrs)] {
		case 'L':
			id = node.left
		case 'R':
			id = node.right
		default:
			return -1, fmt.Errorf("unknown instruction: %c", instrs[steps%len(instrs)])
		}
	}
	return steps, nil
}
