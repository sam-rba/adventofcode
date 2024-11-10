package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"slices"
	"sync"
)

func main() {
	var pockets [2][]rune
	sum := 0
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		items := scanner.Text()

		pockets[0] = []rune(items[:len(items)/2])
		pockets[1] = []rune(items[len(items)/2:])

		sort(pockets)

		dup, ok := findDuplicate(pockets[0], pockets[1])
		if !ok {
			log.Fatal("no duplicate item in", items)
		}

		fmt.Printf("[%s] [%s] %c %d\n", string(pockets[0]), string(pockets[1]), dup, priority(dup))
		sum += priority(dup)
	}
	fmt.Println("silver:", sum)
}

func sort(pockets [2][]rune) {
	wg := new(sync.WaitGroup)
	f := func(s []rune) {
		slices.Sort(s)
		wg.Done()
	}
	wg.Add(2)
	go f(pockets[0])
	go f(pockets[1])
	wg.Wait()
}

func findDuplicate(s, t []rune) (rune, bool) {
	for _, e := range s {
		if _, ok := slices.BinarySearch(t, e); ok {
			return e, true
		}
	}
	return 0, false
}

func priority(item rune) int {
	if item >= 'a' && item <= 'z' {
		return int(item - 'a' + 1)
	} else if item >= 'A' && item <= 'Z' {
		return int(item - 'A' + 1 + ('z' - 'a' + 1))
	}
	panic(fmt.Sprint("invalid item:", item))
}
