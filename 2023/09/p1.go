package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	var (
		stdin *bufio.Scanner
		seq   []int64
		sum   int64
		err   error
	)

	seq = make([]int64, 0, INIT_SEQ_CAP)
	stdin = bufio.NewScanner(os.Stdin)
	for sum = 0; stdin.Scan(); {
		seq, err = parseSeq(seq, stdin.Text())
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}
		sum += nextElem(seq)
	}
	fmt.Println("Part 1:", sum)
}

func nextElem(seq []int64) int64 {
	var (
		last int64
		i    int
	)

	last = seq[len(seq)-1]
	if last == 0 {
		return 0
	}
	for i = 1; i < len(seq); i++ {
		seq[i-1] = seq[i] - seq[i-1]
	}
	return nextElem(seq[:len(seq)-1]) + last
}
