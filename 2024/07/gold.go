package main

import (
	"fmt"
	"log"
	"strconv"
)

const part = "gold"

var operators = []Operator{add, mul, cat}

func cat(a, b int) int {
	s := fmt.Sprintf("%d%d", a, b)
	n, err := strconv.Atoi(s)
	if err != nil {
		log.Fatal("concatentation failed:", err)
	}
	return n
}
