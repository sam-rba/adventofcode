package main

type Monkey struct {
	items                 []int
	inspect               func(int) int
	divTest               int
	throwTrue, throwFalse int
	numInspections        int
}

func round(monkeys []Monkey, worryDiv int) {
	for i := range monkeys {
		turn(&monkeys[i], monkeys, worryDiv)
	}
}

func turn(monkey *Monkey, monkeys []Monkey, worryDiv int) {
	for _, item := range monkey.items {
		item = monkey.inspect(item)

		monkey.numInspections++

		item = reduceWorry(item, worryDiv)

		var dst int
		if (item % monkey.divTest) == 0 {
			dst = monkey.throwTrue
		} else {
			dst = monkey.throwFalse
		}
		monkeys[dst].items = append(monkeys[dst].items, item)
	}
	monkey.items = []int{}
}

func mostActive(monkeys []Monkey) [2]int {
	var active [2]int
	for _, monkey := range monkeys {
		if monkey.numInspections > active[1] {
			active[0] = active[1]
			active[1] = monkey.numInspections
		} else if monkey.numInspections > active[0] {
			active[0] = monkey.numInspections
		}
	}
	return active
}

func monkeyBusiness(mostActive [2]int) int {
	return mostActive[0] * mostActive[1]
}
