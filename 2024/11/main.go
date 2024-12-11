package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"strconv"
)

func main() {
	stones, err := parse(os.Stdin)
	if err != nil {
		log.Fatal(err)
	}

	n := 0
	for _, stone := range stones {
		n += numStones(stone, numBlinks, make(map[int]map[int]int))
	}

	fmt.Printf("%s: %d\n", part, n)
}

func parse(in io.Reader) ([]int, error) {
	var stones []int
	scanner := bufio.NewScanner(in)
	scanner.Split(bufio.ScanWords)
	for scanner.Scan() {
		stone, err := strconv.Atoi(scanner.Text())
		if err != nil {
			return nil, err
		}
		stones = append(stones, stone)
	}
	return stones, nil
}

// mem: stone->blinks->split(stone, blinks)
func numStones(stone int, blinks int, mem map[int]map[int]int) int {
	if m, ok := mem[stone]; ok {
		if n, ok := m[blinks]; ok {
			return n
		}
	} else {
		mem[stone] = make(map[int]int)
	}

	var n int
	if blinks <= 0 {
		n = 1
	} else if stone == 0 {
		n = numStones(1, blinks-1, mem)
	} else if nd := numDigits(stone); nd%2 == 0 {
		left, right := split(stone, nd)
		n = numStones(left, blinks-1, mem) + numStones(right, blinks-1, mem)
	} else {
		n = numStones(stone*2024, blinks-1, mem)
	}

	mem[stone][blinks] = n
	return n
}

func numDigits(x int) int {
	nd := 0
	for ; x > 0; x /= 10 {
		nd++
	}
	return nd
}

func split(stone, nDigits int) (int, int) {
	pow := 1
	for i := nDigits / 2; i > 0; i-- {
		pow *= 10
	}
	return stone / pow, stone % pow
}
