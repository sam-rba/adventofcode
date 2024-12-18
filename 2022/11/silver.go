package main

import (
	"fmt"
	"os"
	"log"
)

const (
	numRounds = 20
	worryDiv  = 3
)

func main() {
	monkeys, err := parse(os.Stdin)
	if err != nil {
		log.Fatal(err)
	}
	for i := 0; i < numRounds; i++ {
		round(monkeys, worryDiv)
	}
	fmt.Println("silver:", monkeyBusiness(mostActive(monkeys)))
}

func reduceWorry(item int, worryDiv int) int {
	item /= worryDiv
	return item
}
