package main

import (
	"fmt"
	"github.com/sam-rba/adventofcode/lib"
	"os"
)

func main() {
	left, right := make(chan int), make(chan int)
	go parse(os.Stdin, left, right)

	leftSorted, rightSorted := make(chan int), make(chan int)
	go lib.Sort(left, leftSorted)
	go lib.Sort(right, rightSorted)

	dist := make(chan int)
	go distance(leftSorted, rightSorted, dist)

	total := make(chan int)
	go lib.Sum(dist, total)
	fmt.Println("silver:", <-total)
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
