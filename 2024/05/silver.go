package main

import (
	"bufio"
	"fmt"
	"github.com/sam-rba/workpool"
	"log"
	"os"
	"slices"
	"strconv"
	"strings"
)

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	order := parseOrder(scanner)

	updates := make(chan []int)
	go parseUpdates(scanner, updates)

	ordered, middle, total := make(chan []int), make(chan int), make(chan int)
	go extractMiddle(ordered, middle)
	go sum(middle, total)

	pool := workpool.New(workpool.DefaultSize)
	for update := range updates {
		pool.Spawn(func() {
			if inOrder(update, order) {
				ordered <- update
			}
		})
	}
	pool.Wait()
	pool.Close()
	close(ordered)

	fmt.Println("silver:", <-total)
}

func parseOrder(in *bufio.Scanner) map[int][]int {
	order := make(map[int][]int)
	for in.Scan() && in.Text() != "" {
		fields := strings.Split(in.Text(), "|")
		if len(fields) != 2 {
			log.Fatal("bad input:", in.Text())
		}

		before, err := strconv.Atoi(fields[0])
		if err != nil {
			log.Fatal(err)
		}

		after, err := strconv.Atoi(fields[1])
		if err != nil {
			log.Fatal(err)
		}

		i, ok := slices.BinarySearch(order[before], after)
		if ok {
			continue
		}
		order[before] = slices.Insert(order[before], i, after)
	}
	return order
}

func inOrder(update []int, order map[int][]int) bool {
	for i, page := range update {
		after := order[page]
		for j := 0; j < i; j++ {
			if _, ok := slices.BinarySearch(after, update[j]); ok {
				return false
			}
		}
	}
	return true
}
