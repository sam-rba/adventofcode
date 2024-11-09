package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func main() {
	top3 := make([]int, 3)
	calories := 0
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		line := scanner.Text()

		if line == "" {
			top3 = insert(calories, top3)
			calories = 0
			continue
		}

		snack, err := strconv.Atoi(line)
		if err != nil {
			log.Fatal(err)
		}
		calories += snack
	}
	top3 = insert(calories, top3)

	fmt.Println("gold:", sum(top3))
}

func insert(e int, s []int) []int {
	if e <= s[0] {
		return s
	}
	i := 1
	for i < len(s) && e > s[i] {
		i++
	}
	return append(s[1:i], append([]int{e}, s[i:]...)...)
}

func sum(s []int) int {
	sum := 0
	for _, e := range s {
		sum += e
	}
	return sum
}
