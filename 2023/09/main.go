package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// initial capacity of a slice that holds a sequence
const INIT_SEQ_CAP = 24

func main() {
	var (
		stdin      *bufio.Scanner
		seq1, seq2 []int64
		sum1, sum2 int64
		err        error
	)

	seq1 = make([]int64, 0, INIT_SEQ_CAP)
	seq2 = make([]int64, 0, INIT_SEQ_CAP)
	stdin = bufio.NewScanner(os.Stdin)
	for sum1, sum2 = 0, 0; stdin.Scan(); {
		seq1, err = parseSeq(seq1, stdin.Text())
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}
		seq2, err = copySeq(seq2, seq1)
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}

		sum1 += nextElem(seq1)
		sum2 += prevElem(seq2)
	}
	fmt.Println("Part 1:", sum1)
	fmt.Println("Part 2:", sum2)
}

func parseSeq(seq []int64, str string) ([]int64, error) {
	var (
		n   int64
		err error
	)

	seq = seq[:0]
	for _, field := range strings.Fields(str) {
		n, err = strconv.ParseInt(field, 10, 64)
		if err != nil {
			return nil, err
		}
		seq = append(seq, n)
	}
	return seq, nil
}

func copySeq(dst, src []int64) ([]int64, error) {
	if cap(dst) < len(src) {
		fmt.Println("resize")
		dst = make([]int64, len(src))
	}
	dst = dst[:len(src)]
	if copy(dst, src) < len(src) {
		return nil, fmt.Errorf("sequence not copied")
	}
	return dst, nil
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
