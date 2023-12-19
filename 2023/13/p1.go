package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	var pattern [][]byte

	stdin := bufio.NewScanner(os.Stdin)
	sum := 0
	for {
		pattern = readPattern(stdin)
		if pattern == nil {
			break
		}
		// horizontal axis
		for axis := 0; axis < len(pattern)-1; axis++ {
			if reflectionHAxis(pattern, axis) {
				sum += 100 * (axis + 1)
			}
		}
		// vertical axis
		for axis := 0; axis < len(pattern[0])-1; axis++ {
			if reflectionVAxis(pattern, axis) {
				sum += axis + 1
			}
		}
	}
	fmt.Printf("part 1: %d\n", sum)
}

func readPattern(sc *bufio.Scanner) [][]byte {
	var (
		pattern [][]byte
		line    string
	)

	pattern = nil
	for sc.Scan() {
		line = sc.Text()
		if len(line) <= 0 {
			break
		}
		pattern = append(pattern, []byte(line))
	}
	return pattern
}

func reflectionHAxis(pattern [][]byte, axis int) bool {
	var i int

	for i = 0; axis-i >= 0 && axis+i+1 < len(pattern); i++ {
		if !eq(pattern[axis-i], pattern[axis+i+1]) {
			return false
		}
	}
	return true
}

func reflectionVAxis(pattern [][]byte, axis int) bool {
	var i, j int

	for i = 0; axis-i >= 0 && axis+i+1 < len(pattern[0]); i++ {
		for j = 0; j < len(pattern); j++ {
			if pattern[j][axis-i] != pattern[j][axis+i+1] {
				return false
			}
		}
	}
	return true
}

func eq(vec1, vec2 []byte) bool {
	var i int

	if len(vec1) != len(vec2) {
		return false
	}
	for i = range vec1 {
		if vec1[i] != vec2[i] {
			return false
		}
	}
	return true
}
