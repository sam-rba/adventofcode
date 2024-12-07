package main

import (
	"fmt"
	"github.com/sam-rba/adventofcode/lib"
	"github.com/sam-rba/workpool"
	"os"
	"slices"
)

func main() {
	leftChan, rightChan := make(chan int), make(chan int)
	go parse(os.Stdin, leftChan, rightChan)

	rightSortedChan := make(chan int)
	go lib.Sort(rightChan, rightSortedChan)

	score := make(chan int)
	total := make(chan int)
	go lib.Sum(score, total)

	left, right := lib.Collect(leftChan), lib.Collect(rightSortedChan)
	pool := workpool.New(workpool.DefaultSize)
	for _, l := range left {
		pool.Spawn(func() {
			score <- similarityScore(l, right)
		})
	}
	pool.Wait()
	pool.Close()
	close(score)

	fmt.Println("gold:", <-total)
}

func similarityScore(leftVal int, right []int) int {
	return leftVal * count(leftVal, right)
}

func count(e int, s []int) int {
	i, ok := slices.BinarySearch(s, e)
	if !ok {
		return 0
	}
	n := 1
	for i = i + 1; i < len(s) && s[i] == e; i++ {
		n++
	}
	return n
}
