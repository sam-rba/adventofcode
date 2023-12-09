package main

import (
	"bufio"
	"fmt"
)

type Node struct {
	left  string
	right string
}

func readnodes(sc *bufio.Scanner) (map[string]Node, error) {
	var (
		nodes       map[string]Node
		line        string
		parent      string
		left, right string
		ok          bool
	)

	nodes = make(map[string]Node)
	for sc.Scan() {
		line = sc.Text()
		if len(line) < 16 {
			return nil, fmt.Errorf("line too short: '%s'", line)
		}
		parent = line[:3]
		left = line[7:10]
		right = line[12:15]
		if _, ok = nodes[parent]; ok {
			return nil, fmt.Errorf("duplicate definition of %s", parent)
		}
		nodes[parent] = Node{left, right}
	}
	return nodes, nil
}
