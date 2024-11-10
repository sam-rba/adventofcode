package main

import (
	"fmt"
	"os"
)

type Round struct {
	opponent Hand
	outcome  Outcome
}

func (r Round) score() int {
	return int(r.outcome) + int(r.choose())
}

func (r Round) choose() Hand {
	switch r.outcome {
	case win:
		return r.opponent%3 + 1
	case loss:
		if r.opponent == 1 {
			return scissors
		}
		return r.opponent - 1
	case draw:
		return r.opponent
	}
	panic(fmt.Sprintf("illegal outcome: %d", r.outcome))
}

func main() {
	rounds := make(chan Round)
	go parseRounds(os.Stdin, rounds)
	score := 0
	for round := range rounds {
		fmt.Println(round, round.score())
		score += round.score()
	}
	fmt.Println("gold:", score)
}

func parseRound(str string) (Round, error) {
	if len(str) != 3 || str[0] < 'A' || str[0] > 'Z' || str[2] < 'X' || str[2] > 'Z' {
		return Round{}, MalformedRound{str}
	}
	return Round{
		opponent: Hand(str[0] - 'A' + 1),
		outcome:  Outcome((2 - ('Z' - str[2])) * 3),
	}, nil
}

func (r Round) String() string {
	return fmt.Sprintf("{%v, %v} %v ", r.opponent, r.choose(), r.outcome)
}
