package main

import (
	"bufio"
	"io"
	"log"
	"slices"
	"strconv"
	"strings"
)

func parse(in io.Reader, left, right chan<- int) {
	defer close(left)
	defer close(right)

	scanner := bufio.NewScanner(in)
	for scanner.Scan() {
		fields := strings.Fields(scanner.Text())
		if len(fields) != 2 {
			log.Fatal("Bad input:", scanner.Text())
		}

		l, err := strconv.Atoi(fields[0])
		if err != nil {
			log.Fatal(err)
		}

		r, err := strconv.Atoi(fields[1])
		if err != nil {
			log.Fatal(err)
		}

		left <- l
		right <- r
	}
}

func sort(in <-chan int, out chan<- int) {
	defer close(out)

	s := make([]int, 0)
	for v := range in {
		i, _ := slices.BinarySearch(s, v)
		s = slices.Insert(s, i, v)
	}

	for _, v := range s {
		out <- v
	}
}

func sum(in <-chan int, out chan<- int) {
	defer close(out)
	sum := 0
	for v := range in {
		sum += v
	}
	out <- sum
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}
