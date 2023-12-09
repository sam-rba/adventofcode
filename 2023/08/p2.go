package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	var (
		stdin     *bufio.Scanner
		instrs    string
		nodes     map[string]Node
		curnodes  []string
		intervals []uint64
		i         int
		err       error
	)

	// read instructions
	stdin = bufio.NewScanner(os.Stdin)
	if !stdin.Scan() {
		fmt.Println("input missing instructions")
		os.Exit(1)
	}
	instrs = stdin.Text()

	// read node definitions
	stdin.Scan() // skip blank line
	nodes, err = readnodes(stdin)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	// find interval that each path lands on a 'Z' node
	curnodes = startingNodes(nodes)
	intervals = make([]uint64, len(curnodes))
	for i = range curnodes {
		intervals[i], err = interval(curnodes[i], nodes, instrs)
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}
	}

	// find the first time the intervals overlap
	fmt.Println("Part 2:", lcm(intervals))
}

// nodes that end in 'A'
func startingNodes(nodes map[string]Node) []string {
	var (
		id       string
		starting []string
	)

	for id = range nodes {
		if id[len(id)-1] == 'A' {
			starting = append(starting, id)
		}
	}
	return starting
}

// interval that a path lands on a 'Z' node
func interval(startid string, nodes map[string]Node, instrs string) (uint64, error) {
	var (
		steps uint64
		node  Node
		id    string
		ok    bool
	)

	id = startid
	for steps = 0; ; steps++ {
		if id[len(id)-1] == 'Z' {
			return steps, nil
		}
		node, ok = nodes[id]
		if !ok {
			return 0, fmt.Errorf("unknown node:", id)
		}
		switch instrs[int(steps)%len(instrs)] {
		case 'L':
			id = node.left
		case 'R':
			id = node.right
		default:
			return 0, fmt.Errorf("unknown instruction: %c\n",
				instrs[int(steps)%len(instrs)])
		}
	}
}

// least common multiple
func lcm(nums []uint64) uint64 {
	var i int

	for i = len(nums) - 1; i > 0; i-- {
		nums[i-1] = nums[i] * nums[i-1] / gcd(nums[i], nums[i-1])
	}
	return nums[0]
}

// greatest common divisor
func gcd(a, b uint64) uint64 {
	var d int

	for d = 0; a != b; {
		if a%2 == 0 {
			a /= 2
			if b%2 == 0 { // both even
				b /= 2
				d++
			}
		} else if b%2 == 0 { // only b even
			b /= 2
		} else { // both odd
			if a > b {
				a = (a - b) / 2
			} else {
				b = (b - a) / 2
			}
		}
	}
	return a * 1 << d
}
