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
		node      Node
		curnodes  []string
		id        string
		steps     uint64
		intervals []uint64
		i         int
		err       error
		ok        bool
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

	// compile starting nodes
	for id = range nodes {
		if id[len(id)-1] == 'A' {
			curnodes = append(curnodes, id)
		}
	}

	// find interval that each path lands on a 'Z' node
	intervals = make([]uint64, len(curnodes))
	for steps = 0; !nonezero(intervals); steps++ {
		for i, id = range curnodes {
			if id[len(id)-1] == 'Z' {
				intervals[i] = steps
			}
			node, ok = nodes[id]
			if !ok {
				fmt.Println("unknown node:", id)
				os.Exit(1)
			}
			switch instrs[int(steps)%len(instrs)] {
			case 'L':
				curnodes[i] = node.left
			case 'R':
				curnodes[i] = node.right
			default:
				fmt.Printf("unknown instruction: %c\n", instrs[int(steps)%len(instrs)])
			}
		}
	}
	// find the first time the intervals overlap
	fmt.Println("Part 2:", lcm(intervals))
}

func nonezero(nums []uint64) bool {
	var n uint64

	for _, n = range nums {
		if n == 0 {
			return false
		}
	}
	return true
}

func lcm(nums []uint64) uint64 {
	var i int

	for i = len(nums) - 1; i > 0; i-- {
		nums[i-1] = nums[i] * nums[i-1] / gcd(nums[i], nums[i-1])
	}
	return nums[0]
}

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
