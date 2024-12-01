package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"slices"
	"strconv"
	"strings"
)

func main() {
	left, right := make(chan int), make(chan int)
	go parse(os.Stdin, left, right)

	leftSorted, rightSorted := make(chan int), make(chan int)
	go sort(left, leftSorted)
	go sort(right, rightSorted)

	dist := make(chan int)
	go distance(leftSorted, rightSorted, dist)

	fmt.Println("silver:", sum(dist))
}

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

func distance(left, right <-chan int, out chan<- int) {
	defer close(out)

	for {
		l, ok := <-left
		if !ok {
			return
		}

		r, ok := <-right
		if !ok {
			return
		}

		out <- abs(l - r)
	}
}

func sum(in <-chan int) int {
	sum := 0
	for v := range in {
		sum += v
	}
	return sum
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}
