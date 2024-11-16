package main

import (
	"fmt"
	"log"
	"os"
)

const numRounds = 10000

func main() {
	monkeys, err := parse(os.Stdin)
	if err != nil {
		log.Fatal(err)
	}

	lcm := 1
	for _, monkey := range monkeys {
		lcm *= monkey.divTest
	}

	for i := 0; i < numRounds; i++ {
		round(monkeys, lcm)
	}
	fmt.Println("gold:", monkeyBusiness(mostActive(monkeys)))
}

func reduceWorry(item int, worryDiv int) int {
	return item % worryDiv
}
