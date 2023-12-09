package main

import (
	"strconv"
	"strings"
)

// initial capacity of a slice that holds a sequence
const INIT_SEQ_CAP = 32

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
