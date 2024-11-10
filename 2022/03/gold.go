package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
)

type Bag ['z' + 1]bool

type Group [3]Bag

func main() {
	sum := 0
	groups := make(chan Group)
	go parseGroups(os.Stdin, groups)
	for group := range groups {
		badge, ok := findBadge(group)
		if !ok {
			log.Fatalf("no badge")
		}
		fmt.Printf("%c %d\n", badge, priority(badge))
		sum += priority(badge)
	}
	fmt.Println("gold:", sum)
}

func parseGroups(in io.Reader, out chan<- Group) {
	defer close(out)
	var group Group
	var i int
	scanner := bufio.NewScanner(in)
	for i = 0; scanner.Scan(); i++ {
		if i >= len(group) {
			out <- group
			i = 0
		}
		group[i] = newBag(scanner.Text())
	}
	if i != len(group) {
		log.Fatal("number of bags is not multiple of group size")
	}
	out <- group
}

func newBag(str string) Bag {
	var bag Bag
	for _, item := range str {
		bag[item] = true
	}
	return bag
}

func findBadge(group Group) (rune, bool) {
	for i := range group[0] {
		if group[0][i] && group[1][i] && group[2][i] {
			return rune(i), true
		}
	}
	return 0, false
}
