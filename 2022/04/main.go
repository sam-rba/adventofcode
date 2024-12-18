package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"strconv"
	"strings"
)

type Range struct {
	lo, hi int
}

func (outer Range) contains(inner Range) bool {
	return outer.lo <= inner.lo && outer.hi >= inner.hi
}

func overlap(a, b Range) bool {
	return (a.lo <= b.lo && a.hi >= b.lo) || (a.lo <= b.hi && a.hi >= b.hi) ||
		a.contains(b) || b.contains(a)
}

func main() {
	pairs := make(chan [2]Range)
	go parsePairs(os.Stdin, pairs)
	silver := 0
	gold := 0
	for pair := range pairs {
		fmt.Println(pair)
		if pair[0].contains(pair[1]) || pair[1].contains(pair[0]) {
			silver++
		}
		if overlap(pair[0], pair[1]) {
			gold++
		}
	}
	fmt.Println("silver:", silver)
	fmt.Println("gold:", gold)
}

func parsePairs(in io.Reader, out chan<- [2]Range) {
	defer close(out)
	var pair [2]Range
	var err error
	scanner := bufio.NewScanner(in)
	for scanner.Scan() {
		fields := strings.Split(scanner.Text(), ",")
		if len(fields) != len(pair) {
			log.Fatal("malformed input:", scanner.Text())
		}
		for i := range pair {
			pair[i], err = parseRange(fields[i])
			if err != nil {
				log.Fatal(err)
			}
		}
		out <- pair
	}
}

func parseRange(str string) (Range, error) {
	fields := strings.Split(str, "-")
	if len(fields) != 2 {
		return Range{}, fmt.Errorf("malformed range:", str)
	}

	lo, err := strconv.Atoi(fields[0])
	if err != nil {
		return Range{}, err
	}
	hi, err := strconv.Atoi(fields[1])
	return Range{lo, hi}, err
}
