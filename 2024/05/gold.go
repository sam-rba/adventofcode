package main

import (
	"bufio"
	"fmt"
	"github.com/sam-rba/workpool"
	"log"
	"os"
	"slices"
	"strconv"
	"strings"
)

func main() {
	in := bufio.NewScanner(os.Stdin)
	inEdges := parseInEdges(in)

	updates := make(chan []int)
	go parseUpdates(in, updates)

	unordered, middle, total := make(chan []int), make(chan int), make(chan int)
	go extractMiddle(unordered, middle)
	go sum(middle, total)

	pool := workpool.New(workpool.DefaultSize)
	for update := range updates {
		pool.Spawn(func() {
			if !inOrder(update, inEdges) {
				unordered <- putInOrder(update, inEdges)
			}
		})
	}
	pool.Wait()
	pool.Close()
	close(unordered)

	fmt.Println("gold:", <-total)
}

func parseInEdges(in *bufio.Scanner) map[int][]int {
	inEdges := make(map[int][]int)
	for in.Scan() && in.Text() != "" {
		fields := strings.Split(in.Text(), "|")
		if len(fields) != 2 {
			log.Fatal("bad input:", in.Text())
		}

		before, err := strconv.Atoi(fields[0])
		if err != nil {
			log.Fatal(err)
		}

		after, err := strconv.Atoi(fields[1])
		if err != nil {
			log.Fatal(err)
		}

		i, ok := slices.BinarySearch(inEdges[after], before)
		if ok {
			continue
		}
		inEdges[after] = slices.Insert(inEdges[after], i, before)
	}
	return inEdges
}

func inOrder(update []int, inEdges map[int][]int) bool {
	for i, page := range update {
		before := inEdges[page]
		for j := i + 1; j < len(update); j++ {
			if _, ok := slices.BinarySearch(before, update[j]); ok {
				return false
			}
		}
	}
	return true
}

// Kahn.
func putInOrder(update []int, inEdges map[int][]int) []int {
	nodes := make([]int, len(update))
	copy(nodes, update)

	ordered := make([]int, 0, len(update))

	for len(nodes) > 0 {
		i, ok := indexWithNoInEdges(nodes, inEdges)
		if !ok {
			log.Fatal("all nodes have incoming edges")
		}
		node := nodes[i]
		nodes = slices.Delete(nodes, i, i+1)
		ordered = append(ordered, node)
	}

	return ordered
}

func indexWithNoInEdges(nodes []int, inEdges map[int][]int) (int, bool) {
	for i, node := range nodes {
		if !isIncidentFromAny(node, nodes, inEdges[node]) {
			return i, true
		}
	}
	return -1, false
}

func isIncidentFromAny(node int, nodes []int, inEdges []int) bool {
	for i := range nodes {
		if _, ok := slices.BinarySearch(inEdges, nodes[i]); ok {
			return true
		}
	}
	return false
}
