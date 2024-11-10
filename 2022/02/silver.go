package main

import (
	"fmt"
	"os"
)

type Round struct {
	you, opponent Hand
}

func (r Round) score() int {
	return int(r.outcome()) + int(r.you)
}

func (r Round) outcome() Outcome {
	if r.you.beats(r.opponent) {
		return win
	} else if r.opponent.beats(r.you) {
		return loss
	}
	return draw
}

func (h Hand) beats(other Hand) bool {
	return other%3 == h-1
}

func main() {
	rounds := make(chan Round)
	go parseRounds(os.Stdin, rounds)
	score := 0
	for round := range rounds {
		fmt.Println(round, round.score())
		score += round.score()
	}
	fmt.Println("silver:", score)
}

func parseRound(str string) (Round, error) {
	if len(str) != 3 || str[0] < 'A' || str[0] > 'C' || str[2] < 'X' || str[2] > 'Z' {
		return Round{}, MalformedRound{str}
	}
	return Round{
		opponent: Hand(str[0] - 'A' + 1),
		you:      Hand(str[2] - 'X' + 1),
	}, nil
}

func (r Round) String() string {
	return fmt.Sprintf("{%v, %v}", r.you, r.opponent)
}
