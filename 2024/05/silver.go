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

func parseUpdates(in *bufio.Scanner, updates chan<- []int) {
	defer close(updates)
	for in.Scan() {
		update, err := parseUpdate(in.Text())
		if err != nil {
			log.Fatal(err)
		}
		updates <- update
	}
}

func parseUpdate(s string) ([]int, error) {
	fields := strings.Split(s, ",")
	pages := make([]int, len(fields))
	for i := range fields {
		var err error
		pages[i], err = strconv.Atoi(fields[i])
		if err != nil {
			return nil, err
		}
	}
	return pages, nil
}

func extractMiddle(updates <-chan []int, middle chan<- int) {
	defer close(middle)
	for update := range updates {
		middle <- update[len(update)/2]
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
