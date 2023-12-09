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
		sum += prevElem(seq)
	}
	fmt.Println("Part 2:", sum)
}

func prevElem(seq []int64) int64 {
	var (
		first int64
		i     int
	)

	first = seq[0]
	if seq[len(seq)-1] == 0 {
		return 0
	}
	for i = 1; i < len(seq); i++ {
		seq[i-1] = seq[i] - seq[i-1]
	}
	return first - prevElem(seq[:len(seq)-1])
}
